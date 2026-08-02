
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

  dplyr::bind_rows(found_href, other_osf) |> unique()
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

    children <- info
    child_collector <- data.frame()
    urls <- children$children[!is.na(children$children)]
    while (length(urls) > 0) {
      resp <- lapply(urls, osf_get_all_pages) |> dplyr::bind_rows()
      children <- .osf_parse_response(resp)
      # resps <- .batch_query(urls, msg = "OSF Child Info", req_func = .osf_headers)
      # children <- lapply(resps, .osf_parse_response) |> dplyr::bind_rows()
      child_collector <- dplyr::bind_rows(child_collector, children)
      urls <- children$children[!is.na(children$children)]
    }

    # get all new node IDs to search for files
    all_nodes <- dplyr::bind_rows(info, child_collector)
    files <- all_nodes
    urls <- files$files[!is.na(files$files)]
    file_collector <- data.frame()
    while (length(urls) > 0) {
      resp <- lapply(urls, osf_get_all_pages) |> dplyr::bind_rows()
      files <- .osf_parse_response(resp)
      # resps <- .batch_query(urls, msg = "OSF File Info", req_func = .osf_headers)
      #files <- lapply(resps, .osf_parse_response) |> dplyr::bind_rows()
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
#' @returns the type
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

#' Get All OSF API Query Pages
#'
#' OSF API queries only return up to 10 items per page, so this helper functions checks for extra pages and returns all of them
#'
#' @param url the OSF API URL
#' @param page_end The last page to get
#'
#' @returns a table of the returned data
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

  content <- tryCatch({
    resp <- httr2::request(.osf_max_page_size(url)) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = \(resp) httr2::resp_status(resp) == 429
      ) |>
      httr2::req_perform()
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  },
  error = function(e) {
    return(NULL)
  })

  next_url <- content$links$`next`
  last_url <- content$links$last

  subdata <- NULL
  if (!is.null(next_url)) {
    m <- gregexpr("(?<=page=)\\d+", next_url, perl = TRUE)
    page <- regmatches(next_url, m)[[1]] |> as.numeric()
    if (length(page) && page <= page_end) {
      subdata <- osf_get_all_pages(next_url, page_end)
    }
  }

  data <- tryCatch({
    dplyr::bind_rows(content$data, subdata)
  }, error = \(e) {
    logger("osf_get_all_pages", list(url = url))
    return(content$data)
  })

  return(data)
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
#' In the default `mode = "files"`, you can limit downloads to only files under a specific size (defaults to 10MB). Files over `max_file_size` are omitted individually, while `max_download_size` is an all-or-nothing gate for the remaining repository total. Omitted files will be listed as messages in verbose mode, and included in the returned data frame with the downloaded column value set to FALSE.
#'
#' In `mode = "zip"`, OSF's Waterbutler API serves the requested folder as one generated zip archive. In this mode, `max_download_size` applies to the archive as a whole when the server reports a `Content-Length`, but `max_file_size` cannot filter files inside the archive before download. The archive can either be kept as a zip or unzipped after download.
#'
#' @param osf_id an OSF ID or URL
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL for no restrictions
#' @param max_download_size maximum total size to download
#' @param max_folder_length maximum folder name length (set to make sure paths are <260 character on some Windows OS)
#' @param ignore_folder_structure if TRUE, download all files into a single folder
#' @param mode download individual files (`"files"`, the default) or request a Waterbutler zip of the whole folder/repository (`"zip"`)
#' @param unzip if `TRUE` and `mode = "zip"`, unzip the downloaded archive into the output folder; if `FALSE`, keep the zip file as-is
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info
#' @export
#'
#' @examples
#' \dontrun{
#' osf_file_download("6nt4v")
#' }
osf_file_download <- function(osf_id,
                              download_to = ".",
                              max_file_size = 10,
                              max_download_size = 100,
                              max_folder_length = Inf,
                              ignore_folder_structure = FALSE,
                              mode = c("files", "zip"),
                              unzip = TRUE,
                              pb = NULL) {
  ## error checking ----
  mode <- match.arg(mode)
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
          osf_file_download(
            x,
            download_to,
            max_file_size,
            max_download_size,
            max_folder_length,
            ignore_folder_structure,
            mode,
            unzip
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

  if (nrow(files) == 0) {
    paste0("- ", osf_id, " contained no files")|>
      list(what = _) |>
      pb$tick(0, tokens = _)
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
    # TEMP verbose diagnostics (remove later): OSF's Waterbutler GENERATES the zip
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
  if (identical(mode, "files") && !is.null(max_file_size) && max_file_size > 0) {
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
  if (identical(mode, "files") && is.finite(max_download_size) && repo_total_mb > max_download_size) {
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

  ## set up download directory (make sure it doesn't overwrite anything)
  # On the OSF you can nest folders and give long folder names, but windows has a 260 character folder name limit.
  # download_to <- fs::path_abs(download_to)
  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, osf_id)
  }
  i <- 0
  while (dir.exists(download_to)) {
    i <- i + 1
    download_to <- download_to |>
      sub("_\\d+$", "", x = _) |>
      paste0("_", i)
  }
  dir.create(download_to, showWarnings = FALSE, recursive = FALSE)
  paste0("- Created directory ", download_to)|>
    list(what = _) |>
    pb$tick(0, tokens = _)

  files_to_copy <- integer(0)
  if (sum(files$kind == "file") > 0 && identical(mode, "files")) {
    ## download all to temp folder ----
    # temppath <- fs::file_temp()
    temppath <- tempfile()
    on.exit(unlink(temppath, recursive = TRUE))
    dir.create(temppath)

    files_to_download <- which(files$kind == "file")

    # urls <- files$download_url[files_to_download]
    # resps <- .batch_query(urls, msg = "Downloading Files", req_func = .osf_headers)

    # save downloaded content to temp files
    for (j in seq_along(files_to_download)) {
      i <- files_to_download[[j]]
      sprintf("Downloading file %d/%d: %s",
               j, length(files_to_download), files$name[[i]]) |>
        list(what = _) |>
        pb$tick(0, tokens = _)

      url <- files$download_url[i]
      tryCatch({
        resp <- .batch_query(url, msg = NULL, req_func = .osf_headers)[[1]]
        #resp <- resps[[j]]
        if (!inherits(resp, "error") && httr2::resp_status(resp) == 200) {
          writeBin(httr2::resp_body_raw(resp),
                   file.path(temppath, files$osf_id[[i]]))
        }
      },
      error = \(e) {
        logger("osf_file_download", list(error = e$message, url = url))
      })
    }

    "Setting up file structure" |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    files <- .osf_prepare_save_paths(files, contents, osf_id,
                                     max_folder_length,
                                     ignore_folder_structure)
    files_to_copy <- .osf_copy_files(files, temppath, download_to)
  } else if (sum(files$kind == "file") > 0 && identical(mode, "zip")) {
    zip_url <- .osf_zip_url(osf_id)
    message("[zip] ", osf_id, ": requesting archive size (HEAD) ...")
    zip_size <- .osf_zip_content_length(zip_url)
    message(sprintf("[zip] %s: %d file(s), archive size %s",
      osf_id, sum(files$kind == "file"),
      if (is.finite(zip_size)) sprintf("%.1f MB", zip_size / mb) else
        "not reported by server (will stream blind)"))
    files <- .osf_prepare_save_paths(files, contents, osf_id,
                                     max_folder_length,
                                     ignore_folder_structure)

    if (is.finite(max_download_size) && !is.na(zip_size) && zip_size > max_download_size * mb) {
      need_total <- ceiling(zip_size / mb)
      msg <- sprintf(
        paste0("Repository %s was not downloaded: its zip archive totals %s MB, ",
               "over the %s MB per-repository limit. ",
               "Set `max_download_size >= %s` to download it."),
        osf_id, .cap_num(need_total), .cap_num(max_download_size), .cap_num(need_total)
      )
      cap_report(msg)
    } else {
      zip_name <- paste0(path_sanitize(osf_id, keep_sep = FALSE), ".zip")
      zip_path <- file.path(download_to, zip_name)
      sprintf("Downloading zip archive for %s", osf_id) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      .osf_download_zip(zip_url, zip_path, zip_size = zip_size)

      if (isTRUE(unzip)) {
        unzip_dir <- tempfile(pattern = "osf-zip-")
        dir.create(unzip_dir)
        on.exit(unlink(unzip_dir, recursive = TRUE), add = TRUE)
        message("[zip] unzipping ", basename(zip_path), " ...")
        "Unzipping archive" |>
          list(what = _) |>
          pb$tick(0, tokens = _)
        utils::unzip(zip_path, exdir = unzip_dir)
        nf <- length(list.files(unzip_dir, recursive = TRUE))
        message(sprintf("[zip] unzipped %d file(s); relocating into place ...", nf))
        files_to_copy <- .osf_relocate_unzipped(files, unzip_dir, download_to)
        message(sprintf("[zip] relocated %d file(s) to %s",
                        length(files_to_copy), basename(download_to)))
        unlink(zip_path)
      }
    }
  }

  ## set up return table ----
  contents$folder <- basename(download_to)
  ret <- contents[
    contents$kind %in% "file",
    c("folder", "osf_id", "name", "filetype", "size", "downloads", "provider")
  ]

  if (identical(mode, "files") && length(files_to_copy) > 0) {
    copied <- files[files_to_copy, c("osf_id", "save_path")]
    names(copied)[[2]] <- "path"
    copied$downloaded <- TRUE
    ret <- dplyr::left_join(ret, copied, by = "osf_id")
    ret$downloaded <- ifelse(ret$downloaded %in% TRUE, TRUE, FALSE)
  } else if (identical(mode, "zip") && sum(files$kind == "file") > 0) {
    if (isTRUE(unzip) && length(files_to_copy) > 0) {
      copied <- files[files_to_copy, c("osf_id", "save_path")]
      names(copied)[[2]] <- "path"
      copied$downloaded <- TRUE
      ret <- dplyr::left_join(ret, copied, by = "osf_id")
      ret$downloaded <- ifelse(ret$downloaded %in% TRUE, TRUE, FALSE)
    } else {
      ret$path <- paste0(path_sanitize(osf_id, keep_sep = FALSE), ".zip")
      ret$downloaded <- file.exists(file.path(download_to, paste0(path_sanitize(osf_id, keep_sep = FALSE), ".zip")))
    }
  } else {
    ret$downloaded <- FALSE
  }

  sprintf("%d files downloaded!", sum(ret$downloaded)) |>
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
