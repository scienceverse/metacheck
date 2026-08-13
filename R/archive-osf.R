
#' Check OSF API Server Status
#'
#' Check the status of the OSF API server.
#'
#' The OSF API server is down a lot, so it's often good to check it before you run a bunch of OSF functions. When the server is down, it can take several seconds to return an error, so scripts where you are checking many URLs can take a long time before you realise they aren't working.
#'
#' You can only make 100 API requests per hour, unless you authorise your requests, when you can make 10K requests per day. The osf functions in metacheck often make several requests per URL to get all of the info. You can authorise them by creating an OSF token at https://osf.io/settings/tokens and including the following line in your .Renviron file:
#'
#' OSF_PAT="replace-with-your-token-string"
#'
#' @param osf_api the OSF API to use (e.g., "https://api.osf.io/v2")
#' @param on_error whether to stop, warn, or ignore errors
#'
#' @returns the OSF status
#' @export
#'
#' @examples
#' osf_api_check()
osf_api_check <- function(osf_api = getOption("metacheck.osf.api"),
                          on_error = c("stop", "warn", "ignore")) {
  on_error <- match.arg(on_error)
  status_code <- 0
  status <- tryCatch({
    if (!curl::has_internet()) return("no internet")
    resp <- httr2::request(osf_api) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      # httr2::req_retry(
      #   max_tries = 3,
      #   is_transient = \(resp) httr2::resp_status(resp) == 429
      # ) |>
      httr2::req_perform()
    status_code <- httr2::resp_status(resp)
    httr2::resp_status_desc(resp)
  }, error = \(e) {
    return(e$message)
  })

  if (status_code != 200) {
    logger("osf_api_check", list(error = status, code = status_code))
    msg <- sprintf(
      "The OSF API seems to be having a problem:\nError %d: %s\nCheck %s",
      status_code, status, osf_api
    )
    if (on_error == "warn") {
      warning(msg, call. = FALSE)
    } else if (on_error == "stop") {
      stop(msg, call. = FALSE)
    }
  }

  return(status)
}

#' Find OSF Links in Papers
#'
#' Get all OSF links: real hyperlinks from the paper's own `url` table, plus a
#' body-text fallback for a BARE mention like "osf.io/gms8z/" that the source
#' PDF/HTML never encoded as an actual hyperlink (common in PDF-converted
#' papers, where a plain-text URL mention loses its link formatting) — the
#' `url` table only ever contains links the source document itself made
#' clickable, so a repository this important cannot rely on that alone. Same
#' two-tier approach `github_links()` already uses for GitHub.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the OSF url in the first (href) column
#' @export
#'
#' @examples
#' osf_links(psychsci)
osf_links <- function(paper) {
  href <- text <- NULL

  urls <- paper_table(paper, "url")
  urls$href <- gsub("\\s", "", urls$href) # temp fix for urls with spaces
  osf <- grepl("osf\\.io", urls$href, ignore.case = TRUE)
  found_href <- urls[osf, ]

  # Body-text fallback: a bare "osf.io/<id>" mention with no scheme and no
  # real hyperlink. The OSF short-id is base62 (letters+digits), typically
  # 5 characters but not fixed-width by design, so kept general; an optional
  # trailing slash is common ("osf.io/gms8z/") and consumed so it is not left
  # dangling on the end of the captured URL.
  osf_bare_regex <- "(?:https?://)?osf\\.io/[A-Za-z0-9]+/?"
  other_osf <- text_search(paper, osf_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # A real hyperlink and a bare body-text mention of the SAME project commonly
  # differ only by a trailing slash (e.g. a hyperlink "osf.io/e6hps" vs. a
  # plain-text mention "osf.io/e6hps/", captured by the trailing "/?" above).
  # Left un-normalized, unique() below treats them as two different repos,
  # which then gets queried, listed, and reported as two separate rows
  # downstream (repo_check's own dedup only matches identical strings). Strip
  # trailing slashes here, before dedup, so both spellings collapse to one row.
  dplyr::bind_rows(found_href, other_osf) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()
}


# Session cache for OSF listings. The OSF directory traversal (osf_info with
# recursive = TRUE) makes many API calls and is not otherwise cached, so a
# second run in the same session (e.g. report() then convert_psychds()) would
# re-list the whole repository and risk hitting OSF's rate limit. This memoises
# the result by (osf_url, recursive) for the session. Cleared by
# osf_cache_clear(); disable with options(metacheck.osf.cache = FALSE).
.osf_listing_cache <- new.env(parent = emptyenv())

.osf_cache_key <- function(osf_url, recursive) {
  ids <- if (is.data.frame(osf_url)) unlist(osf_url, use.names = FALSE) else osf_url
  paste0(paste(sort(unique(as.character(ids))), collapse = "|"),
         "::recursive=", isTRUE(recursive))
}

#' Clear the session cache of OSF listings
#'
#' @returns the number of cached listings removed, invisibly
#' @export
osf_cache_clear <- function() {
  n <- length(ls(.osf_listing_cache))
  rm(list = ls(.osf_listing_cache), envir = .osf_listing_cache)
  invisible(n)
}

#' Retrieve info from the OSF by ID
#'
#' Repository listings are cached for the session (keyed by URL and
#' `recursive`), so repeated calls do not re-query the OSF API; clear the cache
#' with [osf_cache_clear()] or disable it with
#' `options(metacheck.osf.cache = FALSE)`.
#'
#' @param osf_url an OSF ID or URL, or a table containing them
#' @param id_col the index or name of the column that contains OSF IDs or URLs, if id is a table
#' @param recursive whether to retrieve all children
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#' # get info on one OSF node
#' osf_info("pngda")
#'
#' # also get child nodes and files
#' osf_info("https://osf.io/6nt4v", recursive = TRUE)
#' }
osf_info <- function(osf_url, id_col = 1,
                     recursive = FALSE,
                     pb = NULL) {
  # Reuse a cached listing for this session, so a second pass over the same
  # repository (e.g. report() then convert_psychds()) doesn't re-query OSF.
  use_cache <- isTRUE(getOption("metacheck.osf.cache", TRUE))
  cache_key <- if (use_cache) .osf_cache_key(osf_url, recursive) else NULL
  if (use_cache && exists(cache_key, envir = .osf_listing_cache, inherits = FALSE)) {
    if (!is.null(pb)) pb$tick(0, list(what = "OSF listing (cached)"))
    return(get(cache_key, envir = .osf_listing_cache, inherits = FALSE))
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "OSF Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(osf_url)) {
    table <- osf_url
    id_col_name <- colnames(table[id_col])
    raw_osf_urls <- table[[id_col]]
  } else {
    id_col_name <- "osf_url"
    raw_osf_urls <- unique(osf_url) |> stats::na.omit() |> as.character()
    table <- data.frame(osf_url = raw_osf_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    osf_url = raw_osf_urls
  )
  ids$osf_id <- osf_check_id(ids$osf_url)
  ids <- ids[!is.na(ids$osf_id), , drop = FALSE] |> unique()

  valid_ids <- unique(ids$osf_id)

  if (length(valid_ids) == 0) {
    paste0("No valid OSF links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(table)
  }

  # retrieve info for all valid IDs in parallel
  info <- .osf_info(valid_ids, pb = pb) |>
    dplyr::left_join(ids, by = "osf_id")
  if (!"project" %in% colnames(info)) {
    info$project <- rep(NA_character_, nrow(info))
  }

  # reduplicate and add original table info
  by <- stats::setNames("osf_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".osf")
  )

  if (isTRUE(recursive)) {
    paste0("...Main retrieval complete")|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    paste0("Starting retrieval of children...")|>
      list(what = _) |>
      pb$tick(0, tokens = _)

    # These loops walk one listing URL at a time, which looks like an obvious
    # candidate for batching them into one .batch_query() call -- and the
    # commented-out lines below are an earlier attempt at exactly that. It was
    # measured on 2026-08-12 and makes no difference: .osf_max_page_size()
    # already asks for page[size]=100, so each of these listings is a single
    # request either way, and batching only changes how the same number of
    # requests are grouped. Listing pngda (8 nodes, 57 files) took 34 requests
    # both ways, with byte-for-byte identical results; the batched version was
    # slower, because .batch_query() sleeps 0.5s between batches of 5. Left
    # sequential deliberately -- do not "optimise" this without measuring.
    # Both loops report as they go. A big repository spends minutes here with
    # nothing else to show -- ManyLabs2 has hundreds of components -- and a
    # spinner that says only "Starting retrieval" for that long is
    # indistinguishable from a hang. Saying which level is being walked, and
    # how much has been found, makes a long wait legible.
    children <- info
    child_collector <- data.frame()
    urls <- children$children[!is.na(children$children)]
    depth <- 0
    while (length(urls) > 0) {
      depth <- depth + 1
      sprintf("Listing components: level %d, %d to check (%d found so far)",
              depth, length(urls), nrow(child_collector)) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      resp <- lapply(urls, osf_get_all_pages) |> dplyr::bind_rows()
      children <- .osf_parse_response(resp)
      child_collector <- dplyr::bind_rows(child_collector, children)
      urls <- children$children[!is.na(children$children)]
    }
    if (nrow(child_collector) > 0) {
      sprintf("Found %d component%s", nrow(child_collector),
              plural(nrow(child_collector))) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
    }

    # get all new node IDs to search for files
    all_nodes <- dplyr::bind_rows(info, child_collector)
    files <- all_nodes
    urls <- files$files[!is.na(files$files)]
    file_collector <- data.frame()
    while (length(urls) > 0) {
      n_files <- sum(file_collector$kind %in% "file")
      sprintf("Listing files: %d folder%s to check (%d file%s found so far)",
              length(urls), plural(length(urls)), n_files, plural(n_files)) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      resp <- lapply(urls, osf_get_all_pages) |> dplyr::bind_rows()
      files <- .osf_parse_response(resp)
      file_collector <- dplyr::bind_rows(file_collector, files)
      urls <- files$files[!is.na(files$files)]
    }

    data <- list(data, child_collector, file_collector) |>
     dplyr::bind_rows()
  }

  paste0("...OSF retrieval complete!")|>
    list(what = _) |>
    pb$tick(0, tokens = _)

  if (use_cache) assign(cache_key, data, envir = .osf_listing_cache)
  return(data)
}



#' Check OSF IDs
#'
#' Check if strings are valid OSF IDs, URLs, or waterbutler IDs. Basically an improved wrapper for `osfr::as_id()` that returns NA for invalid IDs in a vector.
#'
#' @param osf_id a vector of OSF IDs or URLs
#'
#' @returns a vector of valid IDs, with NA in place of invalid IDs
#' @export
#'
#' @examples
#' osf_check_id("pngda")
#' osf_check_id("osf.io/pngda")
#' osf_check_id("https://osf.io/pngda")
#' osf_check_id("https://osf .io/png da") # rogue whitespace
#' osf_check_id("pnda") # invalid
osf_check_id <- function(osf_id) {
  clean_id <- osf_id |>
    gsub("\\s", "", x = _) |>
    tolower()

  sapply(clean_id, \(id) {
    # NA in (e.g. a missing link column) is NA out — not worth a warning
    if (is.na(id)) return(NA_character_)
    # A link to the OSF platform itself carries no ID by construction (papers
    # often cite osf.io generically), so skip it silently too.
    if (grepl("^(https?://)?(www\\.)?osf\\.io/?$", id)) return(NA_character_)
    tryCatch(
      {
        # for plain IDs (not URLs), check directly
        if (grepl("^[a-z0-9]{5}(_v\\d+)?$", id)) {
          return(id)
        }
        if (grepl("^[a-z0-9]{5}(_v\\d+)?\\?view_only=.+$", id)) {
          return(id)
        }
        if (nchar(id) == 24 && grepl("^[a-z0-9]+$", id)) {
          return(id)
        }

        # for URLs, parse and extract the path
        parsed <- tryCatch(httr2::url_parse(id), error = \(e) NULL)
        if (is.null(parsed)) stop()

        path <- parsed$path |>
          strsplit("/", fixed = TRUE) |> # fs::path_split() |>
          sapply(utils::tail, 1)

        # All OSF IDs are 5 or 24 characters
        if (grepl("^[a-z0-9]{5}(_v\\d+)?$", path)) {
          if (!is.null(parsed$query$view_only)) {
            path <- paste0(path, "?view_only=", parsed$query$view_only)
          }
          return(path)
        }
        if (nchar(path) == 24) {
          return(path)
        }

        stop()
      },
      error = \(e) {
        # try to extract 5-char ID
        m <- gregexpr("(?<=osf\\.io/)[a-z0-9]{5}(_v\\d+)?[?/]?",
          id,
          perl = TRUE
        )
        id5 <- regmatches(id, m) |> sub("[?/]$", "", x = _)
        if (nchar(id5) %in% c(5, 8, 9)) {
          return(id5)
        }

        # else...
        warning(id, " is not a valid OSF ID",
          call. = FALSE, immediate. = FALSE
        )
        return(NA_character_)
      }
    )
  }, USE.NAMES = FALSE)
}

#' Get OSF GUID Type
#'
#' @param guid the 5-letter GUID
#'
#' @returns the type, or `"inaccessible"` when the GUID is a validly-formed
#'   OSF ID but the resource itself could not be reached (private, embargoed,
#'   withdrawn, or deleted) -- distinct from `NA`, which means the input was
#'   not a valid OSF ID at all
#' @export
#'
#' @examples
#' # osf_type("pngda")
osf_type <- function(guid) {
  if (length(guid) > 1) {
    pb <- pb(
      total = length(guid),
      format = "Checking OSF Types [:bar] :current/:total :elapsedfull"
    )
    types <- sapply(guid, \(g) {
      pb$tick()
      osf_type(g)
    })
    return(types)
  }

  osf_api <- getOption("metacheck.osf.api")
  id <- osf_check_id(guid)

  if (is.na(id)) return(NA_character_)

  url <- sprintf(
    "%s/guids/%s/?resolve=false",
    osf_api, id
  )
  info <- osf_get_all_pages(url)

  if (!is.null(attr(info, "osf_error"))) return("inaccessible")

  otype <- info$relationships$referent$links$related$meta$type

  otype %||% NA_character_
}


# The OSF actually allows us to return 10 times more results than we thought.
# This is an important change to prevent API blocks by the OSF.
#
# Ask OSF for the largest page it allows (100 items) instead of the default 10,
# so listing a repository takes ~10x fewer sequential API calls. Only added when
# the URL doesn't already carry a page[size], and left off the `next` links OSF
# returns (those already encode the page size).
.osf_max_page_size <- function(url) {
  if (grepl("page%5Bsize%5D|page\\[size\\]", url)) return(url)
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  paste0(url, sep, "page[size]=100")
}

# Classify an HTTP status from the OSF API as an osf_error kind, or NULL for
# a status that is not an error (2xx, or a transient one already retried by
# req_retry()). Pulled out of osf_get_all_pages() as a pure function so the
# status -> kind mapping is directly testable without mocking HTTP at all.
.osf_status_error <- function(status) {
  if (status %in% c(401, 403)) "forbidden"
  else if (status == 404) "not_found"
  else if (status == 410) "gone"
  else if (status >= 400) "request_failed"
  else NULL
}

# NULL cannot carry an attribute (attr(NULL, x) <- y errors), so an
# osf_get_all_pages() failure is represented as an empty list() instead --
# still length() == 0 for every existing caller, but able to carry osf_error.
.osf_error_result <- function(kind) {
  out <- list()
  attr(out, "osf_error") <- kind
  out
}

#' Get All OSF API Query Pages
#'
#' OSF API queries only return up to 10 items per page, so this helper functions checks for extra pages and returns all of them
#'
#' @param url the OSF API URL
#' @param page_end The last page to get
#'
#' @returns a table of the returned data. When the request could not be
#'   completed (e.g. a private, embargoed, withdrawn, or deleted resource, or
#'   a network failure), this is an empty `list()` with an `osf_error`
#'   attribute set to `"forbidden"`, `"not_found"`, `"gone"`, or
#'   `"request_failed"` (a bare `NULL` cannot carry attributes in R, so it
#'   would silently discard this) -- callers that only check
#'   `length(result) == 0` keep working unchanged, and callers that need to
#'   tell "inaccessible" apart from "genuinely empty" can check
#'   `attr(result, "osf_error")`.
#' @export
#' @examples
#' # get the 20 newest preprints
#' \dontrun{
#' osf_api <- getOption("metacheck.osf.api")
#' url <- sprintf("%s/preprints/?search=date_created-desc", osf_api)
#' preprints <- osf_get_all_pages(url, 2)
#' }
osf_get_all_pages <- function(url, page_end = Inf) {
  Sys.sleep(osf_delay())

  # The first page decides whether there is a listing at all, so a refusal here
  # loses everything, not just one page of it. Retried twice with a growing
  # wait, the same as the later pages below -- but only for a failure that can
  # plausibly change on a second attempt. "not_found" and "gone" are settled
  # answers about a deleted or non-existent resource, and "forbidden" means the
  # token cannot see it; retrying those only wastes six seconds per listing
  # across the many private nodes a whole-account download encounters.
  content <- .osf_get_one_page(url)
  for (attempt in 1:2) {
    err <- attr(content, "osf_error")
    if (is.null(err) || !identical(err, "request_failed")) break
    Sys.sleep(2^attempt)
    content <- .osf_get_one_page(url)
  }
  if (!is.null(attr(content, "osf_error"))) return(content)

  # A JSON array of resources (a listing) is auto-simplified by
  # resp_body_json(simplifyVector = TRUE) into a data.frame already; a single
  # resource (e.g. /guids/{id}/) stays a flat named list (one element per JSON
  # field, of differing lengths) and must NOT be passed through bind_rows() --
  # it would try to treat those fields as columns to recycle to equal length
  # and error. Only a data.frame-shaped `data` (including a genuinely empty
  # listing, `list()`) goes through the tibble-normalizing bind_rows() below;
  # a single flat resource is returned as-is, unchanged from before.
  next_url <- content$links$`next`
  if (is.null(next_url)) {
    if (is.data.frame(content$data) || length(content$data) == 0) {
      out <- dplyr::bind_rows(content$data)
      # No `next` link means this is the whole listing, so what arrived should
      # match the total the OSF reports for it. Checked here as well as on the
      # multi-page path, because a listing that fits in one page can still come
      # back short.
      total1 <- content$links$meta$total %||% content$meta$total
      if (!is.null(total1) && is.finite(total1) && nrow(out) < total1) {
        logger("osf_get_all_pages",
               list(url = url, expected = total1, got = nrow(out)))
        warning(sprintf(
          "The OSF listed only %d of the %d items it reports for %s. Anything not listed is not retrieved, so this listing is incomplete. Run again (see ?osf_pat, which raises the request limit).",
          nrow(out), total1, sub("\\?.*$", "", url)), call. = FALSE)
        attr(out, "osf_incomplete") <- c(expected = total1, got = nrow(out))
      }
      return(out)
    }
    return(content$data)
  }

  # `url` is not necessarily page 1 -- osf_preprint_list(page_start = N) starts
  # from an arbitrary page, and page_end is the LAST page to fetch (absolute
  # page number), not a page COUNT from wherever `url` started. next_url always
  # encodes the page right after the current one, so current_page = next - 1;
  # every later page's URL is then computable up front by substituting that
  # number, no need to wait for each response before requesting the next one.
  # api.osf.io does not need proactive throttling (10,000 requests/day
  # authenticated; see the throttle decision log in .batch_query()), so the
  # remaining pages are fetched in parallel instead of one-at-a-time. Falls
  # back to the previous one-page-at-a-time recursion when total/per_page are
  # not reported, so behaviour is unchanged for any response shape this hasn't
  # been checked against.
  next_page_num <- regmatches(next_url, regexpr("(?<=page=)\\d+", next_url, perl = TRUE))
  next_page_num <- suppressWarnings(as.numeric(next_page_num))
  total <- content$links$meta$total %||% content$meta$total
  per_page <- content$links$meta$per_page %||% content$meta$per_page
  if (!length(next_page_num) || is.na(next_page_num) ||
      is.null(total) || is.null(per_page) || per_page <= 0) {
    subdata <- osf_get_all_pages(next_url, page_end)
    if (!is.null(attr(subdata, "osf_error"))) subdata <- NULL
    return(tryCatch(dplyr::bind_rows(content$data, subdata), error = \(e) {
      logger("osf_get_all_pages", list(url = url))
      dplyr::bind_rows(content$data)
    }))
  }

  current_page <- next_page_num - 1
  last_page <- min(ceiling(total / per_page), page_end)
  if (last_page < next_page_num) return(dplyr::bind_rows(content$data))

  page_urls <- vapply(next_page_num:last_page,
                      \(p) sub("page=\\d+", paste0("page=", p), next_url),
                      character(1))

  Sys.sleep(osf_delay())
  # httptest2's mock-API mode does not fake req_perform_parallel() (see the
  # note at the top of utils.R) -- same reason .batch_query() always uses
  # req_perform_sequential(). .osf_get_one_page() covers one URL at a time
  # only, so under testthat every remaining page is fetched with plain
  # req_perform() in a loop (still avoids waiting on each page's `next` link,
  # just not concurrent); for real runs the pages are fetched in parallel.
  mocking <- isTRUE(Sys.getenv("TESTTHAT") == "true")
  more_pages <- if (mocking) {
    lapply(page_urls, .osf_get_one_page)
  } else {
    .osf_get_pages_parallel(page_urls)
  }
  # A page that could not be read is retried on its own, twice, with a longer
  # wait each time, before the listing is given up as short. Under load the OSF
  # refuses the occasional request in a burst, and a dropped page means missing
  # FILES, not merely missing metadata: whatever is not listed is never
  # downloaded, and nothing downstream can tell the difference between "this
  # repository has 43 files" and "this repository has 57 files and 14 of them
  # were never mentioned". Waiting 2s then 4s keeps a retry from rejoining the
  # burst that was just refused; the same shape as the retry policy used when
  # downloading file bytes (see .storage_backoff()).
  for (attempt in 1:2) {
    failed <- which(vapply(more_pages, \(p) !is.null(attr(p, "osf_error")),
                           logical(1)))
    if (length(failed) == 0) break
    Sys.sleep(2^attempt)
    more_pages[failed] <- if (mocking) {
      lapply(page_urls[failed], .osf_get_one_page)
    } else {
      .osf_get_pages_parallel(page_urls[failed])
    }
  }

  more_data <- lapply(more_pages, \(p) if (is.null(attr(p, "osf_error"))) p$data else NULL)

  out <- tryCatch(
    do.call(dplyr::bind_rows, c(list(content$data), more_data)),
    error = \(e) {
      logger("osf_get_all_pages", list(url = url))
      dplyr::bind_rows(content$data)
    })

  # The OSF reports how many items the listing should hold. Comparing what
  # arrived against it is the only way to notice a short listing: there is no
  # other signal, and a silently incomplete listing is the worst failure this
  # code can produce, because every later step reports success on the subset.
  got <- if (is.data.frame(out)) nrow(out) else length(out)
  expected <- min(total, per_page * page_end)
  if (is.finite(expected) && got < expected) {
    logger("osf_get_all_pages",
           list(url = url, expected = expected, got = got))
    warning(sprintf(
      "The OSF listed only %d of the %d items it reports for %s. Anything not listed is not retrieved, so this listing is incomplete. This usually means the OSF refused a request under load: run again (see ?osf_pat, which raises the request limit).",
      got, expected, sub("\\?.*$", "", url)), call. = FALSE)
    attr(out, "osf_incomplete") <- c(expected = expected, got = got)
  }

  out
}

# Fetch and parse a single OSF API page. Returns the parsed content (a list
# with $data and $links), or an .osf_error_result() list on failure -- pulled
# out of osf_get_all_pages() so both the first page and the parallel-fetched
# remaining pages share identical error handling.
.osf_get_one_page <- function(url) {
  tryCatch({
    resp <- httr2::request(.osf_max_page_size(url)) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = \(resp) httr2::resp_status(resp) == 429
      ) |>
      httr2::req_perform()
    sc <- httr2::resp_status(resp)
    # A private, embargoed, withdrawn, or deleted OSF resource is a normal
    # HTTP response (403/404/410), not a request failure -- req_error() above
    # is told not to raise on it so the status can be read here and reported,
    # rather than parsing whatever body the error page happens to carry as if
    # it were real data (which used to silently look identical to "this
    # resource genuinely has no data").
    err <- .osf_status_error(sc)
    if (!is.null(err)) {
      logger("osf_get_all_pages", list(url = url, status = sc))
      return(.osf_error_result(err))
    }
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  },
  error = function(e) .osf_error_result("request_failed"))
}

# Fetch several OSF API pages in parallel. Same per-URL return contract as
# .osf_get_one_page() (a parsed content list, or an .osf_error_result() on
# failure), so callers can treat the two interchangeably.
.osf_get_pages_parallel <- function(urls) {
  reqs <- lapply(urls, \(url) {
    httr2::request(.osf_max_page_size(url)) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429)
  })
  resps <- httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE)

  lapply(seq_along(urls), \(i) {
    r <- resps[[i]]
    if (inherits(r, "error")) return(.osf_error_result("request_failed"))
    sc <- httr2::resp_status(r)
    err <- .osf_status_error(sc)
    if (!is.null(err)) {
      logger("osf_get_all_pages", list(url = urls[i], status = sc))
      return(.osf_error_result(err))
    }
    tryCatch(httr2::resp_body_json(r, simplifyVector = TRUE),
             error = \(e) .osf_error_result("request_failed"))
  })
}



#' Set the OSF delay
#'
#' Sometimes the OSF gets fussy if you make too many calls, so you can set a delay of a few seconds before each call. Use `osf_delay()` to get or set the OSF delay.
#'
#' @param delay the number of seconds to wait between OSF calls
#'
#' @return NULL
#' @export
#'
#' @examples
#' osf_delay()
osf_delay <- function(delay = NULL) {
  if (is.null(delay)) {
    return(getOption("metacheck.osf.delay"))
  } else if (is.numeric(delay)) {
    options(metacheck.osf.delay = delay)
    invisible(getOption("metacheck.osf.delay"))
  } else {
    stop("set osf_delay with a numeric value for the number of seconds to wait between OSF calls")
  }
}


#' Download all OSF Project Files
#'
#' Creates a directory for the OSF ID and downloads all of the files using a folder structure from the OSF project nodes and file storage structure. Returns (invisibly) a data frame with file info.
#'
#' Some differences may exist because the OSF allows longer file names with characters that may not be allowed on a file system, so these are cleaned up when downloading.
#'
#' Everything is downloaded by default: no size limits apply, because this
#' function exists to retrieve a repository in full and someone archiving their
#' own work wants their own files whatever size they are. Limits are available
#' for when that is not what you want -- looking at somebody else's repository,
#' say. In the default `mode = "files"`, files over `max_file_size` are omitted
#' individually, while `max_download_size` is an all-or-nothing gate for the
#' remaining repository total. Omitted files are reported as messages and
#' appear in the returned data frame with `downloaded = FALSE` and
#' `attempted = FALSE`.
#'
#' In `mode = "zip"`, OSF's Waterbutler API serves a folder as one generated zip
#' archive, which is far fewer requests than fetching each file. That endpoint
#' covers one node's `osfstorage` at a time, so a project whose files sit in
#' components, or on a linked add-on such as GitHub or Dropbox, is not one
#' archive: one archive is requested per node that holds `osfstorage` files, and
#' anything no archive can hold is downloaded individually, so the whole project
#' is still retrieved. In this mode, `max_download_size` applies to each archive
#' when the server reports a `Content-Length`, but `max_file_size` cannot filter
#' files inside an archive before download. Archives can either be kept as zips
#' or unzipped after download.
#'
#' # Downloading a whole account, or part of one
#'
#' Given a user ID, every project that user contributes to is downloaded.
#' Components are not listed separately, because downloading a project already
#' brings its components' files with it, at any depth.
#'
#' To download only some of them, list the projects first with
#' [osf_user_projects()], filter that table however you like, and pass it back:
#'
#' ```
#' projects <- osf_user_projects("4i578")
#' subset(projects, public) |> osf_file_download(download_to = "my_osf")
#' ```
#'
#' # Checking the download
#'
#' Every file that was meant to arrive is checked against the file system
#' afterwards, and against the size the OSF reported for it, so a transfer that
#' failed silently or wrote a truncated file is reported rather than passing as
#' complete. Two columns record the outcome: `downloaded` is TRUE only for a
#' file actually present at the right size, and `attempted` is FALSE for a file
#' deliberately excluded by `max_file_size` or `max_download_size`. Files that
#' were attempted but did not arrive raise a warning naming them.
#'
#' @param osf_id an OSF ID or URL. A *user* ID (the GUID in a profile URL, e.g.
#'   `"4i578"` for <https://osf.io/4i578>) downloads every project that user
#'   contributes to, so a whole account can be archived in one call. To choose
#'   a subset instead, pass the table from [osf_user_projects()] (filtered
#'   however you like) -- any data frame with an `osf_id` column works.
#' @param download_to path to download to. Each project is saved in its own
#'   folder here, named after its OSF ID. Downloading the same project again
#'   reuses that folder and fetches only what is missing, so re-running after a
#'   partial download resumes it instead of making a second copy.
#' @param max_file_size largest single file to download, in MB. `NULL` (the
#'   default) means no limit: this function exists to retrieve a repository in
#'   full, and someone archiving their own work wants their own files whatever
#'   size they are. Set a number to skip anything larger, which is useful when
#'   looking at a repository that is not yours.
#' @param max_download_size largest total per project, in MB. `NULL` (the
#'   default) means no limit, for the same reason.
#' @param max_folder_length maximum folder name length (set to make sure paths are <260 character on some Windows OS)
#' @param ignore_folder_structure if TRUE, download all files into a single folder
#' @param mode what you want from the repository:
#'
#'   * `"all"` (the default) -- the whole thing, as fast as the OSF allows. It
#'     walks the component tree and takes one archive per component, never
#'     listing individual files. Listing is what makes a large repository slow:
#'     for ManyLabs2 it ran over 15 minutes before fetching anything, while
#'     this route retrieved the whole 1.9 GB project in under 7 minutes. Files
#'     on a linked add-on (GitHub, Dropbox) are not in any OSF archive, so
#'     those are fetched individually. Size limits do not apply, because there
#'     is no file list to filter, and neither does resuming: running it again
#'     downloads every archive again. Knowing what to skip would mean reading
#'     each archive's index without fetching it, and the OSF does not allow
#'     that (a HEAD returns 501 and a Range request returns the whole file, so
#'     `zip_peek()` cannot read it). Use `"select"` if you need to resume.
#'   * `"select"` -- list every file first, apply `max_file_size` and
#'     `max_download_size`, then fetch what survives, checking each file
#'     against the size the OSF reported. Use this when you want part of a
#'     repository, or want each file verified.
#'   * `"zip"` -- like `"select"`, but transports whole nodes as archives once
#'     the listing is done. Fewer requests than `"select"`, though not faster.
#'   * `"files"` -- an old name for `"select"`, still accepted.
#' @param unzip if `TRUE` and `mode = "zip"`, unzip the downloaded archive into the output folder; if `FALSE`, keep the zip file as-is
#' @param metadata whether to also retrieve the parts of the project that are
#'   not files, into an `_osf_metadata` folder inside the project's folder: wiki
#'   pages as Markdown (current version only), the activity log as
#'   `logs.csv`, the project's title, description, tags, licence and
#'   contributors as `metadata.json`, and a readable `README.md` summarising
#'   them. Costs about four extra API requests per project.
#' @param osf_pat an OSF personal access token, needed to reach private projects
#'   and to raise the API rate limit. Defaults to whatever [osf_pat()] returns
#'   (the `OSF_PAT` environment variable unless set otherwise); passing it here
#'   sets it for the rest of the session. See [osf_pat()] for how to create one.
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info, one row per file (`osf_id` is each file's
#'   own ID). It carries `download_path`, the absolute folder the project was
#'   saved in, plus `osf_project` and `osf_url` identifying the project, so the
#'   result can be passed straight to [zenodo_upload()]. `downloaded`,
#'   `size_on_disk`, and `attempted` report the verification described above.
#'
#'   In `mode = "all"` there is no file listing, so the table has one row per
#'   **node** instead: `folder`, `osf_project`, `osf_url`, `title`, `files`
#'   (how many arrived), `bytes`, `download_path`, and `downloaded`. It can
#'   still be passed to [zenodo_upload()], which uses `download_path`.
#' @export
#'
#' @examples
#' \dontrun{
#' osf_file_download("6nt4v")
#'
#' # download everything one user has, then archive it on Zenodo
#' osf_file_download("4i578", download_to = "my_osf",
#'                   max_file_size = NULL) |>
#'   zenodo_upload()
#' }
osf_file_download <- function(osf_id,
                              download_to = ".",
                              max_file_size = NULL,
                              max_download_size = NULL,
                              max_folder_length = Inf,
                              ignore_folder_structure = FALSE,
                              mode = c("all", "select", "files", "zip"),
                              unzip = TRUE,
                              metadata = TRUE,
                              osf_pat = NULL,
                              pb = NULL) {
  ## error checking ----
  mode <- match.arg(mode)
  # "files" and "zip" named a transport rather than an intent, and the choice
  # silently decided whether the size limits applied at all. "select" is the
  # old "files" behaviour under a name that says what it is for; "zip" is kept
  # because it is still the right answer when you want whole nodes but also
  # want the file listing (to filter, or to verify).
  if (identical(mode, "select")) mode <- "select"
  # The documentation offers NULL as "no restriction", but NULL reaches the
  # size gates below as length-zero, where is.finite(NULL) is logical(0) and
  # `if` then errors with "missing value where TRUE/FALSE needed". Normalising
  # to Inf here means "no limit" is expressed once, and every later comparison
  # is a plain numeric test.
  if (is.null(max_download_size)) max_download_size <- Inf
  if (is.null(max_file_size)) max_file_size <- Inf
  # The argument `osf_pat` shadows the osf_pat() function inside this body, so
  # the function is fetched by mode; `::` would fail under load_all() before
  # NAMESPACE is regenerated.
  if (!is.null(osf_pat)) get("osf_pat", mode = "function")(osf_pat)

  # A table of projects, as osf_user_projects() returns it (filtered however
  # the caller likes), is the way to download a chosen subset of an account
  # rather than all of it.
  if (is.data.frame(osf_id)) {
    id_col <- intersect(c("osf_id", "osf_project", "id"), names(osf_id))
    if (length(id_col) == 0) {
      stop("`osf_id` is a data frame with no `osf_id` column. ",
           "Pass the table from osf_user_projects(), or a vector of IDs.",
           call. = FALSE)
    }
    osf_id <- osf_id[[id_col[[1]]]]
  }

  osf_id <- osf_check_id(osf_id) |>
    stats::na.omit() |>
    unique()
  if (length(osf_id) == 0) {
    return(NULL)
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "OSF File Download"))
    on.exit(pb$terminate())
  }

  ## expand any user IDs into the projects they contribute to ----
  osf_id <- .osf_expand_user_ids(osf_id, pb = pb)
  if (length(osf_id) == 0) {
    return(NULL)
  }

  ## iterate ----
  if (length(osf_id) > 1) {
    paste0(
      "Starting downloads for ", length(osf_id),
      " OSF projects...\n"
    )|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    dl <- lapply(osf_id, function(x) {
      tryCatch(
        {
          # Named, so adding an argument to the signature cannot silently
          # shift what these positions mean. osf_pat is not passed on: the
          # token was already stored as a session option above, and passing it
          # again would just re-set it once per project.
          osf_file_download(
            osf_id = x,
            download_to = download_to,
            max_file_size = max_file_size,
            max_download_size = max_download_size,
            max_folder_length = max_folder_length,
            ignore_folder_structure = ignore_folder_structure,
            mode = mode,
            unzip = unzip,
            metadata = metadata
          )
        },
        error = function(e) {
          warning(
            x, " resulted in an error:\n  ",
            e$message, "\n"
          )
        }
      )
    }) |>
      do.call(dplyr::bind_rows, args = _)
    paste0(
      "...Completed downloads for ", length(osf_id),
      " OSF projects"
    )|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    # names(dl) <- osf_id
    return(dl)
  }

  ## mode = "all": skip the file listing entirely ----
  # Listing every file is what makes a large repository slow: for ManyLabs2
  # (8cd4r) the recursive listing ran over 15 minutes before a single byte was
  # fetched, while walking the component tree and taking one archive per node
  # retrieved the whole 1.9 GB project in 396 seconds (measured 2026-08-13).
  #
  # The listing earns its cost when you are CHOOSING files -- it is what
  # `max_file_size` filters on, and what the per-file size verification checks
  # against. When you want the repository in full, neither applies, so the
  # listing is spent learning the names of files you were going to download
  # regardless.
  if (identical(mode, "all")) {
    return(.osf_download_all(osf_id, download_to, metadata = metadata,
                             pb = pb))
  }

  ## get files and folders ----
  paste0("Starting retrieval for ", osf_id)|>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(
    osf_info(osf_id, recursive = TRUE, pb = pb)
  )
  cols <- c("osf_id", "name", "provider", "path", "kind", "size", "download_url", "parent", "project") |>
    intersect(names(contents))
  files <- contents[contents$osf_type == "files", cols, drop = FALSE]

  # Say what was found before fetching begins, so the size of the job is known
  # up front rather than inferred from how long it takes.
  n_f <- sum(files$kind %in% "file")
  if (n_f > 0) {
    sprintf("%s: %d file%s, %s to download", osf_id, n_f, plural(n_f),
            .cap_size_str(sum(as.numeric(files$size[files$kind %in% "file"]),
                              na.rm = TRUE))) |>
      message()
  }

  if (nrow(files) == 0) {
    # "contained no files" is right for an empty project, but misleading for
    # one that could not be read at all -- the listing reports that as a type
    # rather than as an error, so it is checked here.
    unreadable <- contents$osf_type %in% c("unfound", "private", "error",
                                           "invalid")
    if (any(unreadable)) {
      why <- contents$osf_type[unreadable][[1]]
      message(sprintf(
        "%s: %s. Nothing was downloaded.", osf_id,
        switch(why,
          unfound = sprintf(
            "no such project on the OSF. Check the ID at https://osf.io/%s",
            osf_id),
          private = "this project is private and your token cannot read it. See ?osf_pat",
          invalid = "not a valid OSF ID",
          "the OSF could not be reached for this project"
        )))
    } else {
      paste0("- ", osf_id, " contained no files")|>
        list(what = _) |>
        pb$tick(0, tokens = _)
    }
    return(NULL)
  }

  mb <- 1024 * 1024

  .osf_prepare_save_paths <- function(files, contents, osf_id, max_folder_length,
                                      ignore_folder_structure) {
    parent_folders <- sapply(seq_along(files$osf_id), \(i) {
      item <- files[i, ]
      parents <- data.frame()
      last_parent <- item$project
      while (length(last_parent) > 0 && !is.na(last_parent) && last_parent != osf_id) {
        next_parent <- contents[contents$osf_id == last_parent, ]
        if (nrow(next_parent) == 0) {
          break
        } else {
          parents <- dplyr::bind_rows(parents, next_parent)
          last_parent <- parents[nrow(parents), "project"]
        }
      }

      if (nrow(parents) == 0) {
        base_parent <- contents[contents$osf_id == osf_id, ]
        if (nrow(base_parent) > 0) {
          parents <- dplyr::bind_rows(parents, base_parent)
        }
      }

      rev(parents$name) |>
        path_sanitize() |>
        paste(collapse = "/")
    })

    folder_in_path <- mapply(\(folder, file) {
      pattern <- sprintf("/%s/", folder)
      regexpr(pattern, file, fixed = TRUE)[[1]]
    }, parent_folders, files$path)
    if (length(folder_in_path) > 0 && all(folder_in_path == 1)) {
      parent_folders <- ""
    }

    files$save_path <- sprintf("%s%s%s%s", files$provider,
                               ifelse(nzchar(parent_folders), "/", ""),
                               parent_folders,
                               files$path)

    if (max_folder_length < Inf) {
      hacky_replace <- "--replace-this--"
      hacky_fp <- ifelse(substring(files$save_path, nchar(files$save_path)) == "/",
                         paste0(files$save_path, hacky_replace),
                         files$save_path)
      fp <- dirname(hacky_fp) |>
        strsplit("/") |>
        lapply(substr, start = 0, stop = max_folder_length) |>
        sapply(paste0, collapse = "/") |>
        paste0("/", basename(hacky_fp)) |>
        gsub(hacky_replace, "", x = _, fixed = TRUE)
      if (any(fp != files$save_path)) {
        warning("Some folder names were truncated to max_folder_length = ", max_folder_length, " characters")
      }
      files$save_path <- fp
    }

    files_to_copy <- which(files$kind == "file")
    if (isTRUE(ignore_folder_structure) && length(files_to_copy) > 0) {
      files$save_path[files_to_copy] <- path_sanitize(files$name[files_to_copy], keep_sep = FALSE)
      dupes <- duplicated(files$save_path[files_to_copy])
      files$save_path[files_to_copy][dupes] <-
        paste0(files$osf_id[files_to_copy][dupes], "-",
               files$name[files_to_copy][dupes])
    }

    files
  }

  .osf_copy_files <- function(files, from_dir, to_dir) {
    files_to_copy <- which(files$kind == "file")
    if (length(files_to_copy) == 0) return(integer(0))

    for (i in files_to_copy) {
      from <- file.path(from_dir, files$osf_id[[i]])
      to <- file.path(to_dir, files$save_path[[i]])
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      file.copy(from, to)
    }

    files_to_copy
  }

  .osf_relocate_unzipped <- function(files, unzip_dir, to_dir) {
    files_to_copy <- which(files$kind == "file")
    if (length(files_to_copy) == 0) return(integer(0))

    copied <- integer(0)
    for (i in files_to_copy) {
      rel_in_zip <- sub("^/+", "", files$path[[i]])
      from <- file.path(unzip_dir, rel_in_zip)
      if (!file.exists(from)) next
      to <- file.path(to_dir, files$save_path[[i]])
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      file.copy(from, to, overwrite = TRUE)
      copied <- c(copied, i)
    }

    copied
  }

  .osf_zip_url <- function(osf_id) {
    sprintf("https://files.osf.io/v1/resources/%s/providers/osfstorage/?zip=", osf_id)
  }

  .osf_zip_content_length <- function(url) {
    resp <- tryCatch({
      httr2::request(url) |>
        .osf_headers() |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    }, error = \(e) NULL)

    if (is.null(resp)) return(NA_real_)
    if (httr2::resp_status(resp) >= 400) return(NA_real_)
    val <- tryCatch(httr2::resp_header(resp, "content-length"), error = \(e) NA_character_)
    suppressWarnings(as.numeric(val))
  }

  .osf_download_zip <- function(zip_url, zip_path, zip_size = NA_real_,
                                timeout_s = 1800) {
    # OSF's Waterbutler GENERATES the zip
    # on the fly for a whole-repo request, which for a big repo (thousands of
    # files) can take minutes before a single byte streams — so a silent buffered
    # read looks hung and hits the curl timeout. We stream straight to disk with a
    # progress bar and a generous timeout, and print each stage so a slow/failed
    # download is visible instead of mysterious.
    t0 <- Sys.time()
    sz <- if (is.finite(zip_size)) sprintf("%.1f MB", zip_size / (1024^2)) else "unknown size"
    message(sprintf("[zip] requesting archive: %s (%s)", zip_url, sz))
    message("[zip] OSF builds the archive server-side first; for a large repo ",
            "this can take several minutes before download starts. Streaming to:")
    message("[zip]   ", zip_path)

    req <- httr2::request(zip_url) |>
      .osf_headers() |>
      httr2::req_timeout(timeout_s) |>
      # Same retry policy as every other file-bytes download: Waterbutler
      # serves the archive from a storage host that answers 403 when it
      # refuses a request, which is not permanent. See .storage_is_transient().
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                       is_transient = .storage_is_transient,
                       backoff = .storage_backoff) |>
      httr2::req_progress(type = "down")

    resp <- tryCatch(
      httr2::req_perform(req, path = zip_path),   # STREAM to disk, not memory
      error = function(e) e)

    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    if (inherits(resp, "error")) {
      message(sprintf("[zip] FAILED after %ss: %s", elapsed, conditionMessage(resp)))
      stop(sprintf("OSF zip download failed for %s", zip_url), call. = FALSE)
    }
    if (httr2::resp_status(resp) != 200) {
      message(sprintf("[zip] FAILED after %ss: HTTP %s", elapsed,
                      httr2::resp_status(resp)))
      stop(sprintf("OSF zip download failed for %s (HTTP %s)",
                   zip_url, httr2::resp_status(resp)), call. = FALSE)
    }
    got <- if (file.exists(zip_path)) file.info(zip_path)$size else 0
    message(sprintf("[zip] downloaded %.1f MB in %ss -> %s",
                    got / (1024^2), elapsed, basename(zip_path)))
    invisible(zip_path)
  }

  ## restrict file size ----
  if (identical(mode, "select") && is.finite(max_file_size) && max_file_size > 0) {
    too_big_files <- which(files$size > max_file_size * mb)
    if (length(too_big_files) > 0) {
      paste0(
        length(too_big_files), " file", plural(length(too_big_files)),
        " in ", osf_id, " exceeded the ", .cap_num(max_file_size),
        " MB per-file limit and ", if (length(too_big_files) == 1) "was" else "were",
        " skipped (the rest of the repository was downloaded). Largest: ",
        sprintf("%s (%s MB)",
                files$name[too_big_files][order(-files$size[too_big_files])][1],
                .cap_num(round(max(files$size[too_big_files], na.rm = TRUE) / mb))),
        ". Raise max_file_size to include them."
      ) |>
        message()

      files <- files[-too_big_files, , drop = FALSE]
    }
  }

  ## restrict total download size ----
  repo_total_mb <- sum(files$size, na.rm = TRUE) / mb
  if (identical(mode, "select") && is.finite(max_download_size) && repo_total_mb > max_download_size) {
    need_total <- ceiling(repo_total_mb)
    msg <- sprintf(
      paste0("Repository %s was not downloaded: its %d file%s total %s MB, ",
             "over the %s MB per-repository limit. ",
             "Set `max_download_size >= %s` to download it."),
      osf_id, nrow(files), plural(nrow(files)),
      .cap_num(need_total), .cap_num(max_download_size), .cap_num(need_total)
    )
    cap_report(msg)
    files <- files[0, , drop = FALSE]
  }

  ## set up download directory ----
  # On the OSF you can nest folders and give long folder names, but windows has a 260 character folder name limit.
  # download_to <- fs::path_abs(download_to)
  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, osf_id)
  }

  # A project is downloaded into the SAME folder every time, so running the
  # command again resumes rather than starting a second copy. Earlier this
  # appended a counter (6nt4v, then 6nt4v_1, ...), which meant the obvious
  # response to a partial download -- run it again -- silently re-fetched
  # everything into a new folder. For an archive of a whole account that is
  # hours of duplicated transfer, and leaves the user to work out which folder
  # is the complete one.
  #
  # Files already present at the size the OSF reports are skipped (see
  # `already_have` below), so a repeat run costs the listing plus whatever is
  # genuinely missing.
  resuming <- dir.exists(download_to)
  dir.create(download_to, showWarnings = FALSE, recursive = TRUE)
  if (resuming) {
    paste0("- Adding to existing directory ", download_to)
  } else {
    paste0("- Created directory ", download_to)
  } |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  files_to_copy <- integer(0)
  if (sum(files$kind == "file") > 0 && identical(mode, "select")) {
    ## download all to temp folder ----
    # temppath <- fs::file_temp()
    temppath <- tempfile()
    on.exit(unlink(temppath, recursive = TRUE))
    dir.create(temppath)

    files_to_download <- which(files$kind == "file")

    # Resuming: skip whatever is already on disk at the size the OSF reports
    # for it. The save paths are worked out here (rather than after the
    # download, as they used to be) precisely so this comparison can happen
    # before anything is fetched.
    #
    # Size is only trusted for osfstorage. For a linked add-on the OSF's
    # recorded size goes stale (see the note below), so presence alone decides:
    # re-fetching a file that is already there would otherwise happen on every
    # run for every add-on file.
    files <- .osf_prepare_save_paths(files, contents, osf_id,
                                     max_folder_length,
                                     ignore_folder_structure)
    if (resuming && length(files_to_download) > 0) {
      on_disk_path <- file.path(download_to, files$save_path[files_to_download])
      exp_size <- suppressWarnings(as.numeric(files$size[files_to_download]))
      trust_size <- tolower(files$provider[files_to_download]) %in%
        c("osfstorage", NA)
      have <- file.exists(on_disk_path) & !dir.exists(on_disk_path)
      right_size <- !trust_size | is.na(exp_size) |
        (file.size(on_disk_path) == exp_size) %in% TRUE
      already_have <- have & right_size

      if (any(already_have)) {
        message(sprintf(
          "%d of %d file%s from %s %s already on disk and %s not downloaded again.",
          sum(already_have), length(files_to_download),
          plural(length(files_to_download)), osf_id,
          if (sum(already_have) == 1) "is" else "are",
          if (sum(already_have) == 1) "was" else "were"))
        files_to_copy <- c(files_to_copy, files_to_download[already_have])
        files_to_download <- files_to_download[!already_have]
      }
    }

    # OSF's download_url redirects (per file) to a pre-signed cloud-storage URL
    # (Google Cloud Storage), not a shared, rate-limited endpoint -- a live burst
    # of 23 concurrent requests returned all-200 with no rate-limit headers
    # (checked 2026-08-08; see .download_many_parallel()'s comment in
    # repo-download.R for the same finding). Fetch in parallel instead of one
    # request at a time.
    "Downloading files" |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    # Everything was already on disk: nothing left to fetch.
    urls <- files$download_url[files_to_download]
    dests <- file.path(temppath, files$osf_id[files_to_download])
    # The expected size is used to detect a truncated transfer: a file that
    # arrives at the wrong size is deleted and retried. That only works when
    # the size is trustworthy, which it is for osfstorage, where the OSF holds
    # the file itself.
    #
    # For a linked add-on (GitHub, Dropbox, and so on) the OSF reports the size
    # it recorded when it last indexed the external repository, and that goes
    # stale as soon as the file changes there. Measured on 2026-08-12:
    # osf.io/download/7tevg (paper.pdf on a GitHub add-on in project 8uqfb)
    # serves 347,151 bytes while the listing still says 94,352. Passing the
    # stale figure made every such file look truncated, so it was deleted,
    # retried once, deleted again, and reported as a failed download -- 6 of
    # that project's 30 files, all of them intact on the server. No expected
    # size is passed for those, so they are kept as served.
    expected <- files$size[files_to_download]
    from_addon <- !(tolower(files$provider[files_to_download]) %in%
                      c("osfstorage", NA))
    expected[from_addon] <- NA_real_
    errs <- .download_many_parallel(urls, dests, expected)
    failed_j <- which(!is.na(errs))
    for (j in failed_j) {
      logger("osf_file_download",
             list(error = errs[j], url = urls[j]))
    }
    # Report failures instead of only logging them -- without this, a file
    # that failed after retries looks the same as a complete download (the
    # progress bar reaches N/N and it is silently absent from the result).
    if (length(failed_j) > 0) {
      message(sprintf(
        "%d of %d file%s from %s failed to download after retries (e.g. %s: %s).",
        length(failed_j), length(files_to_download),
        plural(length(files_to_download)), osf_id,
        basename(urls[failed_j[1]]), errs[failed_j[1]]))

      # A private project whose files all come back as sign-in pages means the
      # request was not authorised. Said plainly and once, because the
      # per-file messages describe the symptom rather than the cause, and a
      # whole-account archive can produce hundreds of them.
      if (any(grepl("not authorised", errs[failed_j], fixed = TRUE))) {
        warning(sprintf(
          "%s is private and its files could not be downloaded without an authorised OSF token. The listing shows the files because listings are authorised, but the file downloads were refused. Set a token with osf_pat(\"your-token\") or OSF_PAT in .Renviron, then run this again. See ?osf_pat",
          osf_id), call. = FALSE)
      }
    }

    "Setting up file structure" |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    # Save paths were already worked out above (before the resume check), so
    # they are not recomputed here. Only the newly fetched files are copied out
    # of the temp folder; anything skipped as already-present is added to
    # `files_to_copy` by the resume check and must not be dropped here.
    newly_copied <- .osf_copy_files(files[files_to_download, , drop = FALSE],
                                    temppath, download_to)
    files_to_copy <- c(files_to_copy, files_to_download[newly_copied])
  } else if (sum(files$kind == "file") > 0 && identical(mode, "zip")) {
    # Waterbutler's ?zip= endpoint archives ONE node's ONE provider. A project
    # is routinely neither: pngda, for example, holds 57 files spread over 6
    # nodes and two providers (osfstorage plus a linked GitHub add-on), of
    # which the root node's osfstorage holds only 3. Asking for the root's
    # archive and treating it as the whole project silently loses the other 54.
    #
    # download_repo_files() already solved this for the data_check/code_check
    # pipeline: group the wanted files by the node that owns them, take one
    # archive per node for the osfstorage rows, and fetch everything else
    # file-by-file. The same grouping is applied here.
    zip_nodes <- unique(stats::na.omit(
      files$project[files$kind %in% "file" &
                      tolower(files$provider %||% "") %in% "osfstorage"]))
    other_idx <- which(files$kind %in% "file" &
                         !(tolower(files$provider %||% "") %in% "osfstorage"))

    if (length(other_idx) > 0) {
      other_providers <- unique(files$provider[other_idx])
      message(sprintf(
        "[zip] %d file%s on %s cannot be in an OSF archive (the ?zip= endpoint covers osfstorage only); downloading %s individually.",
        length(other_idx), plural(length(other_idx)),
        paste(other_providers, collapse = ", "),
        if (length(other_idx) == 1) "it" else "them"))
    }
    if (length(zip_nodes) > 1) {
      message(sprintf(
        "[zip] osfstorage files belong to %d nodes; requesting one archive per node.",
        length(zip_nodes)))
    }

    files <- .osf_prepare_save_paths(files, contents, osf_id,
                                     max_folder_length,
                                     ignore_folder_structure)

    # One archive per owning node. `zip_nodes` is every node that owns at least
    # one osfstorage file, so a project whose files sit in components is
    # covered, not just the root.
    for (node in zip_nodes) {
      node_idx <- which(files$kind %in% "file" &
                          files$project %in% node &
                          tolower(files$provider %||% "") %in% "osfstorage")
      if (length(node_idx) == 0) next

      zip_url <- .osf_zip_url(node)
      zip_size <- .osf_zip_content_length(zip_url)
      message(sprintf("[zip] %s: %d file(s), archive size %s",
        node, length(node_idx),
        if (is.finite(zip_size)) sprintf("%.1f MB", zip_size / mb) else
          "not reported by server (will stream blind)"))

      if (is.finite(max_download_size) && !is.na(zip_size) &&
          zip_size > max_download_size * mb) {
        need_total <- ceiling(zip_size / mb)
        cap_report(sprintf(
          paste0("Node %s was not downloaded: its zip archive totals %s MB, ",
                 "over the %s MB per-repository limit. ",
                 "Set `max_download_size >= %s` to download it."),
          node, .cap_num(need_total), .cap_num(max_download_size),
          .cap_num(need_total)))
        next
      }

      zip_name <- paste0(path_sanitize(node, keep_sep = FALSE), ".zip")
      zip_path <- file.path(download_to, zip_name)
      sprintf("Downloading zip archive for %s", node) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      ok <- tryCatch({
        .osf_download_zip(zip_url, zip_path, zip_size = zip_size); TRUE
      }, error = \(e) {
        message(sprintf("[zip] %s: archive download failed (%s); its files are downloaded individually below.",
                        node, conditionMessage(e)))
        FALSE
      })
      if (!isTRUE(ok)) next

      if (isTRUE(unzip)) {
        unzip_dir <- tempfile(pattern = "osf-zip-")
        dir.create(unzip_dir)
        on.exit(unlink(unzip_dir, recursive = TRUE), add = TRUE)
        utils::unzip(zip_path, exdir = unzip_dir)
        # Only this node's rows are offered for relocation, so an entry is
        # matched against the archive it actually came from.
        copied <- .osf_relocate_unzipped(files[node_idx, , drop = FALSE],
                                         unzip_dir, download_to)
        files_to_copy <- c(files_to_copy, node_idx[copied])
        message(sprintf("[zip] %s: relocated %d of %d file(s)",
                        node, length(copied), length(node_idx)))
        unlink(zip_path)
        unlink(unzip_dir, recursive = TRUE)
      } else {
        # Kept as an archive: every row it covers points at the zip.
        files$save_path[node_idx] <- zip_name
        files_to_copy <- c(files_to_copy, node_idx)
      }
    }

    # Everything an OSF archive cannot hold -- other providers, and any node
    # whose archive was refused, oversized, or failed -- is fetched file by
    # file, so `mode = "zip"` still returns the whole project. This runs for
    # `unzip = FALSE` too: those files are not in any archive, so leaving them
    # out would silently drop them rather than merely leave them compressed.
    left <- setdiff(which(files$kind %in% "file"), files_to_copy)
    if (length(left) > 0) {
      sprintf("Downloading %d remaining file%s individually",
              length(left), plural(length(left))) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      temppath2 <- tempfile()
      dir.create(temppath2)
      on.exit(unlink(temppath2, recursive = TRUE), add = TRUE)
      # `left` holds row numbers in `files`; keep working in those row numbers
      # throughout, rather than in positions within a subset, so the rows
      # recorded as copied are the rows that were actually copied.
      wanted <- left[!is.na(files$download_url[left]) &
                       nzchar(files$download_url[left])]
      if (length(wanted) > 0) {
        dests <- file.path(temppath2, files$osf_id[wanted])
        # Sizes for add-on files are not trustworthy, so none is passed for
        # them; see the note in the files-mode download above.
        expected2 <- files$size[wanted]
        expected2[!(tolower(files$provider[wanted]) %in%
                      c("osfstorage", NA))] <- NA_real_
        errs <- .download_many_parallel(files$download_url[wanted], dests,
                                        expected2)
        fetched <- wanted[is.na(errs)]

        for (i in fetched) {
          from <- file.path(temppath2, files$osf_id[[i]])
          to <- file.path(download_to, files$save_path[[i]])
          dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
          if (isTRUE(file.copy(from, to, overwrite = TRUE))) {
            files_to_copy <- c(files_to_copy, i)
          }
        }

        nfail <- length(wanted) - length(fetched)
        if (nfail > 0) {
          message(sprintf("[zip] %d of %d individually-downloaded file%s failed.",
                          nfail, length(wanted), plural(length(wanted))))
        }
      }
    }
  }

  ## set up return table ----
  contents$folder <- basename(download_to)
  ret <- contents[
    contents$kind %in% "file",
    c("folder", "osf_id", "name", "filetype", "size", "downloads", "provider")
  ]

  if (identical(mode, "select") && length(files_to_copy) > 0) {
    copied <- files[files_to_copy, c("osf_id", "save_path")]
    names(copied)[[2]] <- "path"
    copied$downloaded <- TRUE
    ret <- dplyr::left_join(ret, copied, by = "osf_id")
    ret$downloaded <- ifelse(ret$downloaded %in% TRUE, TRUE, FALSE)
  } else if (identical(mode, "zip") && sum(files$kind == "file") > 0) {
    # Both zip cases join on the rows actually accounted for. With
    # `unzip = FALSE` each row's save_path is the archive that holds it (set
    # per node above), so this no longer assumes one archive named after the
    # root node -- a project whose files span several nodes gets one archive
    # per node, and each row points at its own.
    if (length(files_to_copy) > 0) {
      copied <- files[files_to_copy, c("osf_id", "save_path")]
      names(copied)[[2]] <- "path"
      copied$downloaded <- TRUE
      ret <- dplyr::left_join(ret, copied, by = "osf_id")
      ret$downloaded <- ifelse(ret$downloaded %in% TRUE, TRUE, FALSE)
    } else {
      ret$path <- NA_character_
      ret$downloaded <- FALSE
    }
  } else {
    ret$downloaded <- FALSE
  }

  # Where the project landed and which project it was, so the result can be
  # handed straight to zenodo_upload(). `osf_id` above is each FILE's own ID
  # (that is what `contents` holds), so the project is recorded under its own
  # name rather than overwriting it. `download_path` is absolute -- `folder` is
  # only the basename, which is not enough to find the files again from a
  # different working directory.
  ret$download_path <- download_to
  ret$osf_project <- osf_id
  ret$osf_url <- paste0("https://osf.io/", osf_id)

  ## the parts of the project that are not files ----
  # Wikis, the activity log, and the descriptive metadata. A project is often a
  # record as much as a store, and none of that is in the file listing.
  if (isTRUE(metadata)) {
    tryCatch(.osf_metadata_download(osf_id, download_to, pb = pb),
             error = \(e) {
               logger(".osf_metadata_download",
                      list(osf_id = osf_id, error = conditionMessage(e)))
               message(sprintf(
                 "Could not retrieve metadata for %s (%s); its files were downloaded.",
                 osf_id, conditionMessage(e)))
             })
  }

  ## verify every planned file actually reached the disk ----
  # Up to here `downloaded` records only that the copy step RAN for a file, not
  # that the file is there afterwards. A download that failed silently, a
  # file.copy() that returned FALSE, a name the file system rejected, or a
  # truncated write all leave a row marked TRUE with nothing (or nothing
  # usable) on disk. Checking the file system is the only way to know, and
  # matters most for exactly the case this is built for: archiving a whole
  # account, where no one is watching each of hundreds of files go past.
  # In zip mode with unzip = FALSE, every row's `path` is the ONE archive that
  # holds them all, so a per-file size comparison would compare each file's
  # size against the whole archive's and mark everything as failed. Only the
  # archive's presence can be checked there.
  # With `unzip = FALSE` a row whose path is a .zip is one of many sharing that
  # archive, so its own size cannot be compared against it; a row fetched
  # individually (because no archive could hold it) is a real file and is
  # checked normally. Every other mode checks every row.
  # Which rows can have their size checked against what the OSF reported.
  #
  # Not rows covered by one shared archive (`unzip = FALSE`), where many files
  # point at the same zip.
  #
  # And not files on a linked add-on such as GitHub or Dropbox. For those the
  # OSF reports the size it recorded when it last indexed the external
  # repository, which goes stale as soon as the file changes there: measured on
  # 2026-08-12, osf.io/download/7tevg (paper.pdf on a GitHub add-on) served
  # 347,151 bytes while the OSF listing still said 94,352. The file is intact
  # and correctly downloaded -- only the OSF's recorded size is out of date --
  # so comparing against it would report a perfectly good file as failed.
  # Their presence on disk is still verified.
  check_size <- rep(TRUE, nrow(ret))
  if (identical(mode, "zip") && !isTRUE(unzip) && "path" %in% names(ret)) {
    check_size[grepl("\\.zip$", ret$path) %in% TRUE] <- FALSE
  }
  if ("provider" %in% names(ret)) {
    check_size[!(tolower(ret$provider) %in% c("osfstorage", NA))] <- FALSE
  }
  ret <- .osf_verify_downloads(ret, download_to, check_size = check_size)

  # A file the caller deliberately excluded (over max_file_size, or dropped by
  # the max_download_size gate) is absent on purpose and was already reported
  # when it was skipped. Only files that were meant to arrive and did not are
  # a problem worth warning about, so the two are counted separately -- and
  # `attempted` records which is which, so the table says so too.
  ret$attempted <- ret$osf_id %in% files$osf_id
  n_ok <- sum(ret$downloaded)
  failed <- which(!ret$downloaded & ret$attempted)

  if (length(failed) > 0) {
    worst <- ret[failed, ]
    worst <- worst[order(-(worst$size %||% 0)), ]
    warning(sprintf(
      "%d of %d file%s from %s did not arrive on disk (e.g. %s). The returned table marks %s downloaded = FALSE. Rerun to try again.",
      length(failed), sum(ret$attempted), plural(sum(ret$attempted)), osf_id,
      paste(utils::head(worst$name, 3), collapse = ", "),
      if (length(failed) == 1) "it" else "them"), call. = FALSE)
  }

  n_skipped <- sum(!ret$attempted)
  sprintf("%d of %d files verified on disk%s", n_ok, sum(ret$attempted),
          if (n_skipped > 0) {
            sprintf(" (%d skipped by the size limits)", n_skipped)
          } else "") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  invisible(ret)
}


#' Get A list of preprints from the OSF
#'
#' @param provider a vector of the preprint providers, e.g. psyarxiv, socarxiv, edarxiv (see <https://osf.io/preprints/discover>)
#' @param date_created a single date or a vector of two date (min and max)
#' @param date_modified a single date or a vector of two date (min and max)
#' @param page_start the first page of 10 entries
#' @param page_end the last page of 10 entires to read
#'
#' @returns a table of preprint info
#' @export
#' @examples
#' \dontrun{
#' dc <- c("2025-09-01", "2025-10-01")
#' pp <- osf_preprint_list("psyarxiv", date_created = dc)
#' files <- pp$primary_file
#' }
osf_preprint_list <- function(provider = NULL,
                              date_created = NULL,
                              date_modified = NULL,
                              # is_published = NULL, # can only access own unpublished works
                              page_start = 1,
                              page_end = page_start) {
  filters <- paste0("page=", page_start)

  if (!is.null(provider)) {
    f <- paste0(provider, collapse = ",") |>
      paste0("filter[provider]=", x = _)
    filters <- c(filters, f)
  }

  if (!is.null(date_created)) {
    if (length(date_created) == 1) {
      f <- paste0("filter[date_created]=", date_created)
      filters <- c(filters, f)
    } else if (length(date_created) == 2) {
      gte <- paste0("filter[date_created][gte]=", min(date_created))
      lte <- paste0("filter[date_created][lte]=", max(date_created))
      filters <- c(filters, gte, lte)
    }
  }

  if (!is.null(date_modified)) {
    if (length(date_modified) == 1) {
      f <- paste0("filter[date_modified]=", date_modified)
      filters <- c(filters, f)
    } else if (length(date_modified) == 2) {
      gte <- paste0("filter[date_modified][gte]=", min(date_modified))
      lte <- paste0("filter[date_modified][lte]=", max(date_modified))
      filters <- c(filters, gte, lte)
    }
  }

  # if (!is.null(is_published)) {
  #   val <- ifelse(is_published == TRUE || is_published == "true",
  #                 "true", "false")
  #   f <- paste0("filter[is_published]=", val)
  #   filters <- c(filters, f)
  # }

  url <- paste(filters, collapse = "&") |>
    paste0(getOption("metacheck.osf.api"), "/preprints/", "?", x = _)

  pp <- osf_get_all_pages(url, page_end = page_end)

  .osf_preprint_data(pp)
}
