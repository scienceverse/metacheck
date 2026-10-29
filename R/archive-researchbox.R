#' Headers Needed to Access ResearchBox
#'
#' ResearchBox sits behind Cloudflare, which returns a 406 for httr2's
#' default User-Agent (the same issue archive-aspredicted.R works around).
#' Used for both the page GET in `.rbox_info()` and the file-list POST in
#' `rbox_file_download()`.
#'
#' @returns a named list of headers
#' @keywords internal
.rbox_headers <- function() {
  list(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    `User-Agent` = paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/137.0.0.0 Safari/537.36",
      "scienceverse/metacheck"
    )
  )
}

#' Find ResearchBox Links in Papers
#'
#' Get all ResearchBox links: real hyperlinks from the paper's own `url`
#' table, plus a body-text fallback for a BARE mention like "researchbox.org/
#' 801" that the source PDF/HTML never encoded as an actual hyperlink (common
#' in PDF-converted papers, where a plain-text URL mention loses its link
#' formatting) — the `url` table only ever contains links the source
#' document itself made clickable. Same two-tier approach `github_links()`
#' uses for GitHub.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the ResearchBox url in the first (href) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' rbox_links(psychsci)
#' }
rbox_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("researchbox\\.org", href, ignore.case = TRUE))

  # a peer-review link's passcode is part of the link: without it a box that
  # is not public cannot be read
  rb_bare_regex <- "(?:https?://)?researchbox\\.org/[0-9]+(?:&PEER_REVIEW_passcode=[A-Za-z0-9]+)?/?"
  other_rb <- text_search(paper, rb_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash (captured by the "/?" above), and left un-normalized that
  # turns one repo into two throughout repo_check.
  #
  # ResearchBox has since added URL forms that name a SPECIFIC FILE within a
  # box, not a separate box: "2257.8"/"2257.9" (a dotted box variant) and
  # "2257/72" (a numeric file id under the box). rb_bare_regex already stops
  # at the box id for a bare mention, but a REAL hyperlink (found_href, from
  # the paper's own url table) carries the full href unmodified -- so two
  # links to different files in the SAME box were reaching repo_check as two
  # DIFFERENT repositories, each downloaded and cached separately. Truncate
  # every href to "researchbox.org/<box id>" so all such links collapse to
  # the one box before repo_check ever sees them.
  links <- dplyr::bind_rows(found_href, other_rb) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    dplyr::mutate(href = sub(
      "(researchbox\\.org/[0-9]+)(\\.[0-9]+)?(/.*)?$", "\\1", href,
      ignore.case = TRUE))

  # A paper under review often names its box both as a peer-review link and
  # as the plain "researchbox.org/<id>" (or the hyperlink's text is found
  # again in the body). Within one paper, the plain link takes the
  # peer-review link, so the box reaches repo_check once, with its passcode:
  # it was downloaded twice, and the plain link fails for a box that is not
  # public yet.
  paper_key <- if ("paper_id" %in% names(links)) links$paper_id else ""
  key <- paste(paper_key, .rbox_id(links$href))
  has_code <- !is.na(.rbox_code(links$href))
  coded <- links$href[has_code]
  names(coded) <- key[has_code]
  plain <- !has_code & key %in% names(coded)
  links$href[plain] <- unname(coded[key[plain]])

  unique(links)
}

#' Get or Set the ResearchBox API Key
#'
#' ResearchBox's API (`https://researchbox.org/api_call.php`) needs a personal
#' key. metacheck reads it from the `RESEARCHBOX_API_KEY` environment variable,
#' which you can set in your `.Renviron` file (`usethis::edit_r_environ()`):
#'
#' `RESEARCHBOX_API_KEY="your-key"`
#'
#' Setting the key with `rbox_token()` overrides that for the current session
#' only. Without a key, [rbox_info()] and [rbox_file_download()] read the
#' box's web page instead, and [rbox_files()] and [rbox_search()] are not
#' available.
#'
#' @param token the key to set, or NULL to get the current key
#'
#' @returns the current key (character; `""` when none is set)
#' @export
#'
#' @examples
#' rbox_token() # returns "" unless a key is set
rbox_token <- function(token = NULL) {
  if (is.null(token)) {
    return(getOption("metacheck.researchbox.token") %||%
             Sys.getenv("RESEARCHBOX_API_KEY"))
  }
  if (!is.character(token) || length(token) != 1 || is.na(token)) {
    stop("Set rbox_token with a single string containing your ResearchBox API key",
         call. = FALSE)
  }
  options(metacheck.researchbox.token = token)
  invisible(token)
}

#' Get ResearchBox Box ID from URL
#'
#' @param rb_url a vector of ResearchBox URLs or box IDs
#'
#' @returns a character vector of box IDs (NA where none is found)
#' @keywords internal
.rbox_id <- function(rb_url) {
  rb_url <- trimws(as.character(rb_url))
  id <- rep(NA_character_, length(rb_url))
  bare <- grepl("^[0-9]+$", rb_url)
  id[bare] <- rb_url[bare]
  m <- regexec("researchbox\\.org/([0-9]+)", rb_url, ignore.case = TRUE)
  found <- vapply(regmatches(rb_url, m), \(x) if (length(x) == 2) x[[2]] else NA_character_,
                  character(1))
  id[!bare] <- found[!bare]
  id
}

#' Get the Peer-Review Passcode from a ResearchBox URL
#'
#' A box that is not public yet is shared for peer review with a link like
#' "researchbox.org/1150&PEER_REVIEW_passcode=MJUAAS". The API takes the
#' passcode as `code`.
#'
#' @param rb_url a vector of ResearchBox URLs
#'
#' @returns a character vector of passcodes (NA where there is none)
#' @keywords internal
.rbox_code <- function(rb_url) {
  m <- regexec("PEER_REVIEW_passcode=([A-Za-z0-9]+)", as.character(rb_url),
               ignore.case = TRUE)
  vapply(regmatches(rb_url, m), \(x) if (length(x) == 2) x[[2]] else NA_character_,
         character(1))
}

# The API is used whenever a key is set. A peer-review link sends its passcode
# along (see .rbox_code()); the web page is still read when the API refuses.
.rbox_use_api <- function(rb_url) {
  nzchar(rbox_token())
}

# Seconds ResearchBox asks us to wait before trying again, or NA when the
# response is not a refusal that names a wait. ResearchBox refuses a request
# sent too soon after the previous one with a 429 that gives the wait in a
# Retry-After header and as `retry_after_seconds` in the body: about 5 s
# between most requests and 30 s between downloads (checked 2026-10-28).
# Its hourly and daily limits also answer 429, according to its API page with
# Retry-After as well; an older reply ("list_files same-box hourly limit")
# gave no wait at all.
.rbox_retry_after <- function(resp) {
  if (httr2::resp_status(resp) != 429L) return(NA_real_)
  wait <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
  if (length(wait) == 1 && !is.na(wait)) return(wait)
  wait <- tryCatch(httr2::resp_body_json(resp)$retry_after_seconds,
                   error = \(e) NULL)
  if (is.null(wait)) NA_real_ else as.numeric(wait)
}

# The longest wait .rbox_api() sits out; a longer one (an hourly or daily
# limit) is reported instead, with the wait in the message.
.rbox_max_wait <- 60

# ResearchBox blocks a key for 10 days after 10 requests within 10 days for
# boxes that are not public, sent without the passcode. The API answers such a
# request exactly as one for a box that does not exist: HTTP 500 with an empty
# body, without the count of requests left that its API page promises (checked
# 2026-10-28 with box 10480). So every such answer to a request without a
# passcode is recorded, for 10 days, in metacheck's state folder; from
# .rbox_unfound_limit on, boxes without a passcode are not sent to the API.
# The state folder, not the working directory: on a server each job runs in
# a folder of its own, removed when the job ends, so a count kept there was
# lost after every job (see .metacheck_state_dir()).
.rbox_unfound_limit <- 8

# (the folder is made only when an answer is recorded, see .rbox_unfound_add())
.rbox_unfound_path <- function() {
  file.path(.metacheck_state_dir(), "researchbox_unfound_requests.rds")
}

# The times of the recorded answers of the last 10 days
.rbox_unfound_times <- function() {
  path <- .rbox_unfound_path()
  times <- if (file.exists(path)) tryCatch(readRDS(path), error = \(e) NULL)
  if (!inherits(times, "POSIXct")) return(as.POSIXct(character(0)))
  times[difftime(Sys.time(), times, units = "days") < 10]
}

.rbox_unfound_add <- function() {
  path <- .rbox_unfound_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(c(.rbox_unfound_times(), Sys.time()), path), error = \(e) NULL)
  invisible(NULL)
}

#' Send a Request to the ResearchBox API
#'
#' Every ResearchBox API call goes through here: it adds the key, waits as
#' long as ResearchBox asks when a request comes too soon, and turns every
#' failure into an error message instead of a condition.
#'
#' A refusal that cannot be waited out (a wait longer than a minute, which
#' ResearchBox's hourly and daily limits ask for, or a wait that kept being
#' refused) is reported with the prefix "API rate limit exhausted: ", the same
#' text `download_repo_files()` uses, so these can be told apart from other
#' failures and rerun once the limit has passed.
#'
#' ResearchBox blocks a key for 10 days after 10 requests for boxes that are
#' not public, sent without their passcode, and answers those as if the box
#' did not exist. After 8 such answers within 10 days, a request for a box
#' without a passcode is not sent; the error says why.
#'
#' @param action the API action (`"box_meta"`, `"list_files"`,
#'   `"download_files"` or `"search"`)
#' @param ... further form fields for the action (e.g. `box_id`)
#' @param code a box's peer-review passcode (see `.rbox_code()`), sent only
#'   when it is not NA
#' @param path a file to write the response to (for `"download_files"`), or
#'   NULL to keep it in memory
#'
#' @returns a list with `ok` (logical), `error` (NA or a message), and `body`
#'   (the parsed JSON reply, or NULL when `path` is given)
#' @keywords internal
.rbox_api <- function(action, ..., code = NA_character_, path = NULL) {
  token <- rbox_token()
  if (!nzchar(token)) {
    return(list(ok = FALSE, error = "no ResearchBox API key is set (see ?rbox_token)",
                body = NULL))
  }
  # A limit another job reached with the same key (an hourly or daily limit,
  # recorded below) is not asked again before it ends.
  problem <- .key_problem("researchbox")
  if (!is.null(problem)) {
    return(list(ok = FALSE, body = NULL, error = paste0(
      if (problem$problem == "limit") "API rate limit exhausted: ",
      .key_problem_message("researchbox", problem$problem, problem$until))))
  }
  fields <- list(...)
  if (!is.na(code)) fields$code <- code
  # see .rbox_unfound_limit
  about_box <- !is.null(fields$box_id) && is.na(code)
  n_unfound <- if (about_box) length(.rbox_unfound_times()) else 0
  if (n_unfound >= .rbox_unfound_limit) {
    return(list(ok = FALSE, body = NULL, error = paste0(
      "not sent: ResearchBox could not find ", n_unfound, " boxes asked for ",
      "in the last 10 days, which may be boxes that are not public; it blocks ",
      "the key for 10 days after 10 requests for those, so a box without a ",
      "peer-review passcode is not sent to the API until then")))
  }

  # httr2 hides the Authorization header when a request is printed or logged.
  # ResearchBox wants at least 5 s between requests: sending them 6 s apart
  # avoids a refusal (which may count toward its hourly limits) for each one;
  # the 30 s between downloads is still waited out after the refusal.
  resp <- tryCatch({
    .mc_request("https://researchbox.org/api_call.php", low_speed_time = 300) |>
      httr2::req_headers(Authorization = paste("Bearer", token)) |>
      httr2::req_throttle(capacity = 1, fill_time_s = 6, realm = "researchbox.org/api") |>
      httr2::req_body_form(version = "1.0", action = action, !!!fields) |>
      httr2::req_retry(max_tries = 5, retry_on_failure = TRUE,
                       is_transient = \(resp) {
                         wait <- .rbox_retry_after(resp)
                         !is.na(wait) && wait <= .rbox_max_wait
                       },
                       after = .rbox_retry_after) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(path = path)
  }, error = \(e) e)

  if (inherits(resp, "error")) {
    if (!is.null(path)) unlink(path)
    return(list(ok = FALSE, error = conditionMessage(resp), body = NULL))
  }

  status <- httr2::resp_status(resp)
  is_json <- identical(httr2::resp_content_type(resp), "application/json")
  body <- if (is_json && httr2::resp_has_body(resp)) {
    tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE), error = \(e) NULL)
  }

  if (status == 200L && (!is.null(path) || isTRUE(body$ok))) {
    return(list(ok = TRUE, error = NA_character_, body = body))
  }

  if (!is.null(path)) unlink(path)
  # A box that does not exist, or is not public and was asked for without its
  # passcode, answers 500 with an empty body (checked 2026-10-28)
  error <- if (status == 500L && !httr2::resp_has_body(resp)) {
    if (about_box) .rbox_unfound_add()
    "unfound"
  } else if (status == 429L) {
    wait <- .rbox_retry_after(resp)
    # The key's hourly or daily limit holds for every box, so it is recorded
    # for the other jobs (see api_key_status()). Not a wait under a minute (a
    # request sent too soon), nor a limit for one box or file ("list_files
    # same-box hourly limit"), which other boxes are not held to.
    key_wide <- !is.na(wait) && wait > .rbox_max_wait &&
      !grepl("same[- ](box|file)", body$error %||% "", ignore.case = TRUE)
    if (key_wide) .key_problem_record("researchbox", "limit", Sys.time() + wait)
    paste0("API rate limit exhausted: ", body$error %||% "HTTP 429",
           if (!is.na(wait)) paste0(" (ResearchBox asks to wait ", wait, " seconds)"),
           if (key_wide) paste0(". ", .key_problem_message("researchbox", "limit",
                                                         Sys.time() + wait)))
  } else if (status %in% c(401L, 403L)) {
    # a key ResearchBox does not know: 401, "token invalid" (checked 2026-10-28)
    .key_problem_record("researchbox", "rejected")
    paste0("HTTP ", status, if (!is.null(body$error)) paste0(": ", body$error),
           ". ", .key_problem_message("researchbox", "rejected"))
  } else {
    paste0("HTTP ", status, if (!is.null(body$error)) paste0(": ", body$error))
  }
  list(ok = FALSE, error = error, body = body)
}

#' Retrieve info from ResearchBox by URL
#'
#' With an API key set (see [rbox_token()]), each box costs two API requests:
#' its details and its file list. A peer-review link's passcode is sent along.
#' Without a key, or when the API refuses (ResearchBox limits how often the
#' same box can be requested per hour), the box's web page is read instead.
#'
#' `RB_license` is the licence's SPDX identifier (such as `"CC-BY-4.0"`) and
#' `RB_public` the date the box became public (`"2023-06-09"`), from either
#' route. The web page does not give the title, status, DOIs, licence link or
#' date of the last change (`RB_title`, `RB_status`, `RB_paper_doi`,
#' `RB_zenodo_doi`, `RB_paper_authors`, `RB_license_url`, `RB_last_modified`),
#' so these are `NA` when the page was read. The `files` column holds a table
#' of the box's files; from the API it also has each file's section, type,
#' description, extension, size and MD5 checksum.
#'
#' @param rb_url an ResearchBox URL, or a table containing them (e.g., as created by `rbox_links()`)
#' @param id_col the index or name of the column that contains ResearchBox URLs, if id is a table
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a previously cached result for a box already
#'   looked up (see [repo_info_cache_clear()]) instead of asking ResearchBox
#'   again. Off by default.
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#' # get info on one box
#' rbox_info("https://researchbox.org/801")
#' }
rbox_info <- function(rb_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (!online("researchbox.org")) {
    stop("ResearchBox.org seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "ResearchBox Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(rb_url)) {
    table <- rb_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "rb_url"
    raw_urls <- unique(rb_url) |> stats::na.omit()
    table <- data.frame(rb_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    rb_url = raw_urls
  )
  ids <- ids[!is.na(ids$rb_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$rb_url)

  if (length(valid_ids) == 0) {
    ("No valid ResearchBox links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(table)
  }

  # iterate over valid IDs
  paste0(
    "Starting ResearchBox retrieval for ",
    length(valid_ids), " file",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # Every box is looked up, as in zenodo_info(): this loop used to stop at the
  # first box that failed, leaving every later box without information.
  id_info <- vector("list", length(valid_ids))
  for (i in seq_along(valid_ids)) {
    u <- valid_ids[[i]]
    rb_id <- .rbox_id(u)
    cached <- if (isTRUE(cache) && !is.na(rb_id)) .repo_info_cache_get("researchbox", rb_id) else NULL
    if (!is.null(cached)) {
      # cached by box id, so a different link to the same box gets its own url
      cached$rb_url <- u
      id_info[[i]] <- cached
    } else {
      id_info[[i]] <- .rbox_info(u, pb = pb)
      if (isTRUE(cache) && !is.na(rb_id) && .repo_info_ok(id_info[[i]]))
        .repo_info_cache_put("researchbox", rb_id, id_info[[i]])
    }
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "rb_url")

  # reduplicate and add original table info
  by <- stats::setNames("rb_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".rb")
  )

  paste0("...ResearchBox retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from ResearchBox by URL
#'
#' Uses the API when a key is set (see [rbox_token()]) and reads the box's web
#' page otherwise. When the API fails (most often ResearchBox's hourly limit
#' for one box), the web page is read instead. The API answers alike for a box
#' that does not exist and one that is not public; the web page tells them
#' apart, so its answer is the one reported.
#'
#' @param rb_url a ResearchBox URL
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.rbox_info <- function(rb_url, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ", rb_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  api_error <- NULL
  if (.rbox_use_api(rb_url)) {
    info <- .rbox_info_api(rb_url)
    if (is.null(info$error)) return(info)
    api_error <- info$error
    paste0("- ResearchBox API: ", api_error, "; reading the web page instead") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
  }

  info <- .rbox_info_page(rb_url)
  # both failed: the API's reason first, so a refusal a later run will get
  # past keeps its "API rate limit exhausted: " start. "unfound" from the API
  # may be a box that is not public, which only the page can tell.
  if (!is.null(api_error) && api_error != "unfound" && "error" %in% names(info))
    info$error <- paste0(api_error, "; web page: ", info$error)
  info
}

# Columns that only one of the two routes can fill, added as NA by the other so
# both return the same columns.
.rbox_api_only <- c("RB_title", "RB_status", "RB_paper_doi", "RB_zenodo_doi",
                    "RB_paper_authors", "RB_license_url", "RB_last_modified")

# A box's details and file list from the API (two requests). Returns the same
# columns as .rbox_info_page(); on failure only `rb_url` and `error`.
.rbox_info_api <- function(rb_url) {
  obj <- data.frame(rb_url = rb_url)
  rb_id <- .rbox_id(rb_url)
  if (is.na(rb_id)) {
    obj$error <- "unfound"
    return(obj)
  }
  code <- .rbox_code(rb_url)

  meta <- .rbox_api("box_meta", box_id = rb_id, code = code)
  if (!meta$ok) {
    obj$error <- meta$error
    return(obj)
  }
  listing <- .rbox_api("list_files", box_id = rb_id, code = code)
  if (!listing$ok) {
    obj$error <- listing$error
    return(obj)
  }

  m <- meta$body
  # "" is how the API reports a field the box does not have
  field <- \(x) if (is.null(x) || length(x) != 1 || !nzchar(x)) NA_character_ else as.character(x)

  obj$files <- list(.rbox_file_table(listing$body$files))
  obj$box_id <- rb_id
  obj$reference <- NA_character_  # the web page's download token; the API needs none
  # The reference comes with HTML markup ("<i>Psychological Science</i>.
  # <BR>doi:&nbsp;<a href=...>"), which the web page shows as plain text
  obj$RB_target <- field(m$paper_reference) |> .rbox_plain_text()
  obj$RB_license <- field(m$license_id)
  obj$RB_public <- field(m$public_since)
  # author1, author2, ... each with first and last name
  authors <- m[grepl("^author[0-9]+$", names(m))]
  authors <- vapply(authors, \(a) trimws(paste(a$first, a$last)), character(1))
  obj$RB_authors <- if (length(authors)) paste(authors, collapse = ", ") else NA_character_
  obj$RB_abstract <- field(m$abstract)
  obj$RB_title <- field(m$box_title)
  obj$RB_status <- field(m$status)
  obj$RB_paper_doi <- field(m$paper_doi)
  obj$RB_zenodo_doi <- field(m$zenodo_doi)
  obj$RB_paper_authors <- field(m$paper_authors) |> trimws()
  obj$RB_license_url <- field(m$license_url)
  obj$RB_last_modified <- field(m$last_modified)

  obj
}

# A box's licence (SPDX identifier, "CC-BY-4.0"), for repo_check, which gets
# the files from the download: one box_meta request, where rbox_info() would
# also ask for the file list. Without a key, or when the API fails, the web
# page is read. NA when neither gives it. With `cache`, a licence from an
# earlier rbox_info() or .rbox_license() is reused.
.rbox_license <- function(rb_url, cache = FALSE) {
  rb_id <- .rbox_id(rb_url)
  if (is.na(rb_id)) return(NA_character_)
  if (isTRUE(cache)) {
    for (host in c("researchbox", "researchbox_license")) {
      cached <- .repo_info_cache_get(host, rb_id)
      if (is.data.frame(cached) && !is.na(cached$RB_license[1] %||% NA)) {
        return(cached$RB_license[[1]])
      }
    }
  }

  license <- NA_character_
  if (.rbox_use_api(rb_url)) {
    meta <- .rbox_api("box_meta", box_id = rb_id, code = .rbox_code(rb_url))
    if (meta$ok && is.character(meta$body$license_id) && nzchar(meta$body$license_id)) {
      license <- meta$body$license_id
    }
  }
  if (is.na(license)) {
    page <- tryCatch(suppressWarnings(.rbox_info_page(rb_url)), error = \(e) NULL)
    if (is.data.frame(page) && !"error" %in% names(page)) license <- page$RB_license
  }
  if (isTRUE(cache) && !is.na(license)) {
    .repo_info_cache_put("researchbox_license", rb_id, data.frame(RB_license = license))
  }
  license
}

# HTML markup to plain text (NA stays NA)
.rbox_plain_text <- function(x) {
  if (is.na(x)) return(x)
  tryCatch(
    xml2::read_html(paste0("<p>", x, "</p>"), encoding = "UTF-8") |>
      rvest::html_text2() |>
      gsub("\\s+", " ", x = _) |>
      trimws(),
    error = \(e) x
  )
}

# The API's file list as a table, with `name` and `file_id` first as in the
# table read from the web page. list_files gives `basename` for the name.
.rbox_file_table <- function(files) {
  if (is.null(files) || length(files) == 0 || NROW(files) == 0) {
    return(data.frame(name = character(0), file_id = character(0)))
  }
  data.frame(
    name = files$basename,
    file_id = as.character(files$file_id),
    section = files$section,
    type = files$type,
    description = files$description,
    extension = files$extension,
    size = as.numeric(files$size),
    md5 = files$md5
  )
}

# A box's details and file list read from its web page.
.rbox_info_page <- function(rb_url) {
  # set up return table
  obj <- data.frame(
    rb_url = rb_url
  )

  # get website
  resp <- .mc_request(rb_url) |>
    httr2::req_headers(!!!.rbox_headers()) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # check if redirect (suppress encodings warning)
  suppressMessages({
    # an empty body (a refusal can have none) stopped resp_body_string() before
    # the status was checked (U45)
    body_text <- if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else ""
    pattern <- "(?<=window\\.location\\.replace\\(')https://researchbox.org/\\d+(?='\\))"
    if (grepl(pattern, body_text, perl = TRUE)) {
      matches <- gregexpr(pattern, body_text, perl = TRUE)
      redirect_url <- regmatches(body_text, matches)

      resp <- .mc_request(redirect_url[[1]]) |>
        httr2::req_headers(!!!.rbox_headers()) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    }
  })

  # A box that does not exist still answers 200 (checked 2026-10-20), so any
  # other status is a refusal or a server error, not a missing box: on
  # GitHub's test machines ResearchBox did not serve box 4377, which was
  # reported as "could not be found". The status is now named.
  status <- httr2::resp_status(resp)
  if (status != 200) {
    if (status == 404) {
      warning(rb_url, " could not be found", call. = FALSE)
      obj$error <- "unfound"
    } else {
      warning("ResearchBox did not serve ", rb_url, " (HTTP ", status, ")",
              call. = FALSE)
      obj$error <- paste0("HTTP ", status)
    }
    return(obj)
  }

  # A 200 page without content is no box (U45)
  if (!httr2::resp_has_body(resp)) {
    warning(rb_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # A box that does not exist gets a page saying "Problem. There is no
  # ResearchBox #99,999,999"; one that is not public (without its passcode) a
  # page that sends the browser to a captcha (checked 2026-10-28). Both were
  # read as a box without files.
  page_text <- httr2::resp_body_string(resp)
  if (grepl("There is no ResearchBox", page_text, fixed = TRUE)) {
    warning(rb_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }
  if (grepl("captcha_wall", page_text, fixed = TRUE)) {
    warning("ResearchBox did not show ", rb_url, ": the box is probably not public yet",
            call. = FALSE)
    obj$error <- paste("not public: ResearchBox asks for a captcha instead of",
                       "showing the box; a peer-review link with its passcode is needed")
    return(obj)
  }

  # Read the content with specified encoding
  html <- xml2::read_html(page_text, encoding = "UTF-8")

  # get file list
  file_names <- xml2::xml_find_all(html, "//p [@class='file_name']") |>
    xml2::xml_text()
  # filedesc <- xml2::xml_find_all(html, "//p [@class='preview_link']") |> # blocked out, seems gone after website redesign?
  #   xml2::xml_text()
  # filedesc <- filedesc[filedesc!=""]

  # Each downloadable file has a checkbox <input type="checkbox"
  # name="fileN" value="<file_id>"> in the same document order as the
  # file_name <p> tags above, so they're paired positionally. These file_ids
  # (plus box_id and reference below) are what download_files.php needs to
  # generate the zip -- see rbox_file_download().
  file_ids <- xml2::xml_find_all(
    html, "//input[@type='checkbox'][starts-with(@name, 'file')]"
  ) |>
    xml2::xml_attr("value")

  file_list <- data.frame(
    name = file_names,
    file_id = if (length(file_ids) == length(file_names)) file_ids else NA_character_
    # description = filedesc
  )
  obj$files <- list(file_list)

  obj$box_id <- xml2::xml_find_first(html, "//input[@id='box_id']") |>
    xml2::xml_attr("value")
  obj$reference <- xml2::xml_find_first(html, "//input[@id='reference']") |>
    xml2::xml_attr("value")

  # get info from bottom table
  body <- xml2::xml_find_all(html, "//body") |>
    rvest::html_text2() # xml2::xml_text()

  # section borders
  sections <- c(
    RB_target = "SUPPLEMENTARY FILES FOR",
    RB_license = "LICENSE FOR USE",
    RB_public = "BOX PUBLIC SINCE",
    RB_authors = "BOX CREATORS",
    RB_abstract = "ABSTRACT",
    done = "$('.file_number')"
  )

  for (i in 1:5) {
    obj[[names(sections)[i]]] <- tryCatch(
      {
        after <- strsplit(body, sections[[i]], fixed = TRUE)[[1]][[2]]
        # Cut at whichever later heading comes first: a box without an
        # abstract has no "ABSTRACT", and cutting only there gave RB_authors
        # the rest of the page, footer and JavaScript included (box 10485)
        for (s in sections[-seq_len(i)]) {
          after <- strsplit(after, s, fixed = TRUE)[[1]][[1]]
        }
        trimws(after)
      },
      error = \(e) return(NA_character_)
    )
  }
  # the same form as from the API: the page names the licence in a sentence
  # ("All content posted to ResearchBox is under a CC By 4.0 License ...")
  # and writes the date as "June 09, 2023". Other text is kept as it is.
  if (grepl("CC By 4.0", obj$RB_license, ignore.case = TRUE))
    obj$RB_license <- "CC-BY-4.0"
  obj$RB_public <- .rbox_iso_date(obj$RB_public)
  obj[.rbox_api_only] <- NA_character_

  return(obj)
}

# "June 09, 2023" as "2023-06-09"; anything else is returned unchanged.
# month.name is English in every locale, which as.Date(format = "%B") is not.
.rbox_iso_date <- function(x) {
  m <- regmatches(x, regexec("^([A-Za-z]+) ([0-9]{1,2}), ([0-9]{4})$", trimws(x)))[[1]]
  month <- if (length(m) == 4) match(tolower(m[[2]]), tolower(month.name)) else NA
  if (is.na(month)) return(x)
  sprintf("%s-%02d-%02d", m[[4]], month, as.integer(m[[3]]))
}

#' List the Files in a ResearchBox
#'
#' Lists every file in one or more boxes without downloading them, using one
#' API request per box. A peer-review link's passcode is sent along. Needs an
#' API key (see [rbox_token()]).
#'
#' ResearchBox limits how often the same box's file list can be requested per
#' hour. A box it refuses has a row with an `error` starting "API rate limit
#' exhausted: ", so it can be requested again later; `cache = TRUE` avoids
#' asking again for a box already listed.
#'
#' @param rb_url a vector of ResearchBox URLs or box IDs
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a previously cached file list (from this
#'   function or [rbox_info()]) instead of asking ResearchBox again (see
#'   [repo_info_cache_clear()]). Off by default.
#'
#' @returns a data frame with one row per file: `rb_url`, `name`, `file_id`,
#'   `section`, `type`, `description`, `extension`, `size` (bytes) and `md5`,
#'   plus an `error` column when a box could not be listed
#' @export
#'
#' @examples
#' \dontrun{
#' rbox_files("https://researchbox.org/801")
#' }
rbox_files <- function(rb_url, pb = NULL, cache = FALSE) {
  if (!nzchar(rbox_token())) {
    stop("rbox_files() needs a ResearchBox API key; see ?rbox_token", call. = FALSE)
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  urls <- unique(as.character(rb_url)) |> setdiff(NA)
  tables <- lapply(urls, \(u) {
    paste0("* Listing files in ", u, "...") |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    rb_id <- .rbox_id(u)
    if (is.na(rb_id)) {
      warning(u, " is not a ResearchBox link", call. = FALSE)
      return(data.frame(rb_url = u, error = "unfound"))
    }

    files <- if (isTRUE(cache)) .rbox_cached_files(rb_id) else NULL
    if (is.null(files)) {
      listing <- .rbox_api("list_files", box_id = rb_id, code = .rbox_code(u))
      if (!listing$ok) {
        warning("ResearchBox did not list ", u, ": ", listing$error, call. = FALSE)
        return(data.frame(rb_url = u, error = listing$error))
      }
      files <- .rbox_file_table(listing$body$files)
      if (isTRUE(cache)) .repo_info_cache_put("researchbox_files", rb_id, files)
    }

    if (nrow(files) == 0) return(NULL)
    data.frame(rb_url = u, files)
  })

  dplyr::bind_rows(tables)
}

# A box's file list from the cache: rbox_files()' own, or the `files` column
# of an rbox_info() result that came from the API (the web page's file table
# has no sizes or checksums). NULL when neither is cached.
.rbox_cached_files <- function(rb_id) {
  files <- .repo_info_cache_get("researchbox_files", rb_id)
  if (!is.null(files)) return(files)
  info <- .repo_info_cache_get("researchbox", rb_id)
  files <- if (is.data.frame(info) && "files" %in% names(info)) info$files[[1]]
  if (is.data.frame(files) && "md5" %in% names(files)) files else NULL
}

#' Search ResearchBox
#'
#' Searches the data, code or papers in ResearchBox's public boxes. A data
#' search returns one row per matching variable, a code search one row per
#' matching file, and a paper search one row per matching box. Needs an API
#' key (see [rbox_token()]).
#'
#' ResearchBox returns at most 100 hits per request, so each 100 hits cost one
#' API request. A `*` matches any ending of a word (`"happi*"`).
#'
#' @param q the search text (at least two characters)
#' @param type what to search: `"data"`, `"code"` or `"paper"`
#' @param limit the most hits to return; `Inf` for all of them
#' @param offset the number of hits to skip, to continue an earlier search
#'
#' @returns a data frame with one row per hit: `rb_url` and the columns
#'   ResearchBox gives for that type (`box_id`, `formatted_reference`, and for
#'   data `file_id`, `var_id`, `var_name`, `var_description`, `original_name`
#'   and `upload_time`; for code `file_id`, `original_name` and `upload_time`;
#'   for papers `box_title`). The attribute `"total"` is the number of hits
#'   ResearchBox found, which can be more than were returned.
#' @export
#'
#' @examples
#' \dontrun{
#' rbox_search("happi*", type = "data")
#' rbox_search("ggplot", type = "code", limit = Inf)
#' }
rbox_search <- function(q, type = c("data", "code", "paper"), limit = 100,
                        offset = 0) {
  type <- match.arg(type)
  if (!is.character(q) || length(q) != 1 || is.na(q)) {
    stop("q must be a single string", call. = FALSE)
  }
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit < 1) {
    stop("limit must be a number of at least 1 (or Inf)", call. = FALSE)
  }
  if (!nzchar(rbox_token())) {
    stop("rbox_search() needs a ResearchBox API key; see ?rbox_token", call. = FALSE)
  }

  pages <- list()
  total <- Inf
  got <- 0
  while (got < limit && offset < total) {
    res <- .rbox_api("search", type = type, q = q, offset = offset,
                     limit = min(100, limit - got))
    if (!res$ok) {
      if (length(pages) == 0) stop("ResearchBox search failed: ", res$error, call. = FALSE)
      warning("ResearchBox search stopped after ", got, " hits: ", res$error,
              "; use offset = ", offset, " to continue", call. = FALSE)
      break
    }
    total <- res$body$total
    hits <- res$body$hits
    # no hits come as an empty list
    if (!is.data.frame(hits) || nrow(hits) == 0) break
    pages[[length(pages) + 1]] <- hits
    got <- got + nrow(hits)
    offset <- offset + nrow(hits)
  }

  hits <- as.data.frame(dplyr::bind_rows(pages))
  out <- data.frame(rb_url = if (nrow(hits)) paste0("https://researchbox.org/", hits$box_id) else character(0))
  out <- cbind(out, hits)
  if ("upload_time" %in% names(out)) {
    out$upload_time <- as.POSIXct(out$upload_time, origin = "1970-01-01", tz = "UTC")
  }
  attr(out, "total") <- if (is.finite(total)) total else NA_integer_
  out
}


#' Retrieve files from ResearchBox by URL
#'
#' Downloads the boxes as zips and unpacks them into the repository cache (see
#' [repo_cache_dir()]), so a later call for the same box reuses the files
#' without asking ResearchBox again.
#'
#' With an API key set (see [rbox_token()]), each box costs one API request:
#' the download. A peer-review link's passcode is sent along. The zip holds a
#' table of contents with each file's MD5 checksum; every file that arrives is
#' checked against it (`checksum_ok`), and a file that is listed but does not
#' arrive, or does not match its checksum, is recorded in the `"failed"`
#' attribute; that box is downloaded again on the next call. The table of
#' contents is not returned as one of the box's files.
#'
#' Without a key, or when the API cannot download the box, the download is
#' made through the box's web page, as before. That zip has no table of
#' contents, so with a key the box's file list is then asked for (one more
#' request, skipped with `cache = TRUE` when the list is already cached) to
#' check the files; without one, or when ResearchBox refuses the file list,
#' `checksum_ok` is `NA`. After several downloads, the web page asks for a
#' captcha, which metacheck cannot answer; that box is then recorded as
#' failed.
#'
#' ResearchBox blocks a key for two days when the same file is downloaded 10
#' times in 30 days, so a box is downloaded once and then reused from the
#' cache.
#'
#' @param rb_url a vector of ResearchBox URLs
#' @param pb a progress bar passed from another function
#' @param file_ids the ids of the files to download (the `file_id` column of
#'   [rbox_files()]), or `"all"` (the default). ResearchBox adds a data file's
#'   codebook to the download. Needs an API key.
#' @param subfolders how the files are put into folders: `"type"` (the
#'   default; "Data", "Code", "Materials", ...) or `"section"` ("Study 1",
#'   ...). Anything other than the default needs an API key.
#' @param cache if `TRUE`, reuse a previously cached file list for the
#'   checksums of a download without a table of contents (see [rbox_files()])
#'   instead of asking ResearchBox again. The downloaded files themselves are
#'   always reused.
#'
#' @returns a data frame with one row per file (`rb_url`, `name`,
#'   `file_location`, `size`, `isdir`, `checksum_ok`, plus the file type
#'   columns of [file_types]), and a `"failed"` attribute: a data frame with
#'   `rb_url`, `name` (`NA` when the whole box failed) and `error` for each box
#'   or file that could not be retrieved. An error starting "API rate limit
#'   exhausted: " is expected to succeed when run again later.
#' @export
#'
#' @examples
#' \dontrun{
#' rbox_file_download("https://researchbox.org/801")
#'
#' # only two files, in folders by section
#' rbox_file_download("https://researchbox.org/801", file_ids = c(10, 16),
#'                    subfolders = "section")
#' }
rbox_file_download <- function(rb_url, pb = NULL, file_ids = "all",
                               subfolders = c("type", "section"),
                               cache = FALSE) {
  subfolders <- match.arg(subfolders)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  # vectorise
  if (length(rb_url) > 1) {
    unique_rb <- unique(rb_url) |> setdiff(NA)

    # Several links to the same box (a peer-review link and a plain link to
    # the box) are downloaded once, through a link with the passcode when one
    # has it, and every link gets that box's files or failure.
    box <- .rbox_id(unique_rb)
    box[is.na(box)] <- unique_rb[is.na(box)]
    has_code <- !is.na(.rbox_code(unique_rb))
    boxes <- unique(box)
    pick <- vapply(boxes, \(b) {
      i <- which(box == b)
      unique_rb[[i[[which.max(has_code[i])]]]]
    }, character(1))

    box_lists <- lapply(pick, rbox_file_download, pb = pb,
                        file_ids = file_ids, subfolders = subfolders,
                        cache = cache)
    file_lists <- lapply(seq_along(unique_rb), \(k) {
      tbl <- box_lists[[match(box[[k]], boxes)]]
      failed <- attr(tbl, "failed")
      tbl$rb_url <- rep(unique_rb[[k]], nrow(tbl))
      if (nrow(failed) > 0) failed$rb_url <- unique_rb[[k]]
      attr(tbl, "failed") <- failed
      tbl
    })
    failed <- lapply(file_lists, attr, "failed") |> dplyr::bind_rows()
    info <- do.call(dplyr::bind_rows, args = file_lists)
    # every link failed: no rows; the join below stopped on a table without
    # columns ("Join columns in `y` must be present", U43)
    if (nrow(info) == 0) return(.rbox_no_files(failed))
    orig <- data.frame(rb_url = rb_url)
    df <- dplyr::left_join(orig, info, by = "rb_url")
    attr(df, "failed") <- failed

    return(df)
  }

  paste0("* Retrieving files from ", rb_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # Download the ZIP into a persistent cache keyed by the ResearchBox URL, so
  # re-runs reuse the already-downloaded/unzipped files instead of re-fetching.
  # A download of only some files, or in folders by section, is a different
  # set of files and is cached separately.
  all_files <- identical(as.character(file_ids), "all")
  # Keyed by the box, so every link to it (a peer-review link, or the box's
  # link written differently) reuses one download; a plain link keeps the
  # folder it had.
  cache_key <- if (is.na(.rbox_id(rb_url))) rb_url else paste0("https://researchbox.org/", .rbox_id(rb_url))
  if (!all_files || subfolders != "type") {
    cache_key <- paste(cache_key, subfolders, paste(sort(file_ids), collapse = "-"))
  }
  tmp_dir <- .repo_cache_subdir(cache_key)
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  zip_path <- file.path(tmp_dir, "archive.zip")
  out_dir  <- file.path(tmp_dir, "unzipped")
  # the checksum results of the download, so a reused box keeps them, and a
  # box with a missing or damaged file is downloaded again
  check_path <- file.path(tmp_dir, "checksums.rds")

  # Reuse a previous successful download/unzip when present.
  checks <- if (file.exists(check_path)) tryCatch(readRDS(check_path), error = \(e) NULL)
  already_unzipped <- dir.exists(out_dir) &&
    length(list.files(out_dir, recursive = TRUE)) > 0 &&
    !(nrow(checks$failed %||% data.frame()) > 0)

  if (!already_unzipped) {
    use_api <- .rbox_use_api(rb_url)
    if ((!all_files || subfolders != "type") && !use_api) {
      return(.rbox_no_files(data.frame(
        rb_url = rb_url, name = NA_character_,
        error = "choosing files or folders needs the ResearchBox API: a key (see ?rbox_token)")))
    }

    rb_id <- .rbox_id(rb_url)
    code <- .rbox_code(rb_url)

    paste0("Downloading to: ", zip_path) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl <- list(ok = FALSE, error = NA_character_)
    if (use_api) {
      dl <- .rbox_api("download_files", box_id = rb_id,
                      file_ids = if (all_files) "all" else paste(file_ids, collapse = ","),
                      subfolders = subfolders, code = code, path = zip_path)
    }
    # The web page can only download the whole box, in folders by type. It is
    # also read for "unfound", which may be a box that is not public: the page
    # tells which.
    page_read <- !use_api || (!dl$ok && all_files && subfolders == "type")
    if (page_read) {
      api_error <- if (use_api) dl$error
      if (use_api) {
        paste0("- ResearchBox API: ", dl$error, "; downloading through the web page instead") |>
          list(what = _) |>
          pb$tick(0, tokens = _)
      }
      dl <- .rbox_download_page(rb_url, zip_path, pb = pb)
      # both failed: the API's reason first, so a refusal a later run will get
      # past keeps its "API rate limit exhausted: " start
      if (!dl$ok && !is.null(api_error) && api_error != "unfound")
        dl$error <- paste0(api_error, "; web page: ", dl$error)
    }

    if (!dl$ok) {
      if (!identical(dl$error, "unfound")) {
        warning("Could not download ", rb_url, ": ", dl$error, call. = FALSE)
      } else if (!page_read) {
        # (the web page warns itself)
        warning(rb_url, " could not be found", call. = FALSE)
      }
      return(.rbox_no_files(data.frame(rb_url = rb_url, name = NA_character_,
                                       error = dl$error)))
    }

    # Unzipped into "unzipped.part" and renamed when done: an unzip that was
    # stopped left some files in "unzipped", which the next run reused as the
    # whole box (U203).
    part_dir <- paste0(out_dir, ".part")
    unlink(part_dir, recursive = TRUE)
    dir.create(part_dir, showWarnings = FALSE)
    paste0("Unzipping into: ", out_dir) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    # a box with one file may get that file itself, not a zip
    is_zip <- identical(readBin(zip_path, "raw", 2), charToRaw("PK"))
    if (!is_zip && !is.na(dl$single_name %||% NA)) {
      file.copy(zip_path, file.path(part_dir, basename(dl$single_name)))
    } else {
      utils::unzip(zip_path, exdir = part_dir)
    }
    # the checksums from the zip's table of contents, which is not one of the
    # box's files and is moved next to the zip
    listed <- .rbox_take_toc(part_dir, tmp_dir)
    unlink(out_dir, recursive = TRUE)
    file.rename(part_dir, out_dir)

    # No table of contents (a download through the web page): the API's file
    # list gives the checksums instead
    if (is.null(listed) && use_api) {
      listed <- if (isTRUE(cache)) .rbox_cached_files(rb_id)
      if (is.null(listed)) {
        listing <- .rbox_api("list_files", box_id = rb_id, code = code)
        if (listing$ok) {
          listed <- .rbox_file_table(listing$body$files)
          if (isTRUE(cache)) .repo_info_cache_put("researchbox_files", rb_id, listed)
        } else {
          paste0("- ResearchBox did not list the files (", listing$error,
                 "); the files are not checked") |>
            list(what = _) |>
            pb$tick(0, tokens = _)
        }
      }
    }

    checks <- .rbox_verify_downloads(rb_url, out_dir, listed, file_ids)
    saveRDS(checks, check_path)
  }

  # list files (recursively) and return; a table of contents left in a box
  # downloaded before it was moved out is not one of the box's files
  files <- list.files(out_dir, recursive = TRUE, full.names = FALSE)
  files <- files[!grepl(.rbox_toc_regex, basename(files))]
  file_locations <- file.path(out_dir, files)

  if (length(files) == 0) {
    warning("Unzip produced no files. The archive might be corrupt.")
    return(.rbox_no_files(data.frame(rb_url = rb_url, name = NA_character_,
                                     error = "the download contained no files")))
  }

  file_info <- file.info(file_locations)

  # Create dataframe
  rb_file_info <- data.frame(
    rb_url = rep(rb_url, length(files)),
    name = files,
    file_location = file_locations,
    size = file_info$size,
    isdir = file_info$isdir
  )
  # NA for a box downloaded before checksums were checked, or without them
  rb_file_info$checksum_ok <- unname(checks$checksum_ok[files]) %||% NA

  rb_file_info$ext <- strsplit(rb_file_info$name, "\\.") |>
    sapply(\(x) {
      if (length(x) < 2) {
        return("")
      }
      x[[length(x)]]
    }) |>
    tolower()
  rb_file_info <- dplyr::left_join(
    rb_file_info,
    metacheck::file_types,
    by = "ext"
  )

  failed <- checks$failed %||% data.frame(rb_url = character(0), name = character(0),
                                          error = character(0))
  if (nrow(failed) > 0) {
    warning(sprintf(
      "%d file%s from %s did not arrive intact (e.g. %s). The \"failed\" attribute lists %s; the box is downloaded again on the next run.",
      nrow(failed), plural(nrow(failed)), rb_url,
      paste(utils::head(failed$name, 3), collapse = ", "),
      if (nrow(failed) == 1) "it" else "them"), call. = FALSE)
  }
  attr(rb_file_info, "failed") <- failed

  return(rb_file_info)
}

# The table rbox_file_download() returns when no file could be retrieved: no
# rows, the usual columns, and the reason in the "failed" attribute.
.rbox_no_files <- function(failed) {
  out <- data.frame(rb_url = character(0), name = character(0),
                    file_location = character(0), size = numeric(0),
                    isdir = logical(0), checksum_ok = logical(0))
  attr(out, "failed") <- failed
  out
}

# The API's zip holds "Table of Contents Box #<box id>.json" and a .csv copy,
# next to the folders of files, listing each file's name, file_id, type,
# section, description, size and MD5 (checked 2026-10-28). A zip from the web
# page had none.
.rbox_toc_regex <- "^Table of Contents Box #[0-9]+\\.(json|csv)$"

# Move the table of contents out of the unzipped box in `dir` into `to`, and
# return its JSON as a file table like .rbox_file_table()'s: NULL when there
# is none or it cannot be read.
.rbox_take_toc <- function(dir, to) {
  paths <- list.files(dir, recursive = TRUE)
  toc <- paths[grepl(.rbox_toc_regex, basename(paths))]
  if (length(toc) == 0) return(NULL)
  moved <- file.path(to, basename(toc))
  unlink(moved)
  file.rename(file.path(dir, toc), moved)

  json <- moved[grepl("\\.json$", moved)]
  files <- if (length(json) == 1) tryCatch(jsonlite::fromJSON(json)$files, error = \(e) NULL)
  if (!is.data.frame(files) || !all(c("file_name", "file_id", "md5") %in% names(files))) {
    return(NULL)
  }
  files$basename <- files$file_name
  files$extension <- tools::file_ext(files$file_name)
  .rbox_file_table(files)
}

# Download a whole box through its web page, as ResearchBox's own "download
# all" button does. Returns list(ok, error, single_name), where single_name is
# the name of the box's only file (the page may send that file itself rather
# than a zip).
.rbox_download_page <- function(rb_url, zip_path, pb = NULL) {
  # ResearchBox no longer serves a static zip URL (the old
  # s3.wasabisys.com guess in earlier versions of this function now 404s,
  # see issue #186). The site instead generates the zip on request from a
  # POST to download_files.php, given the box_id, reference token, and the
  # numeric ids of the files to include -- all of which are scraped off
  # the box's own page by .rbox_info_page().
  info <- .rbox_info_page(rb_url)
  if ("error" %in% names(info)) {
    # .rbox_info_page() already warns for this case
    return(list(ok = FALSE, error = info$error))
  }

  file_ids <- info$files[[1]]$file_id
  file_ids <- file_ids[!is.na(file_ids)]
  if (length(file_ids) == 0 || is.na(info$box_id) || is.na(info$reference)) {
    return(list(ok = FALSE, error = "could not find downloadable files"))
  }

  # ResearchBox's file ids are tokens now ("XG08oROE0t..."), sent as text,
  # as its own download_files.js does; as.numeric() made every one NA, so
  # nothing was downloaded. Numeric ids (older pages) are still sent as
  # numbers. A JSON array even for one file (I()).
  ids <- if (all(grepl("^[0-9]+$", file_ids))) as.numeric(file_ids) else as.character(file_ids)
  dl_status <- tryCatch({
    resp <- .mc_request("https://researchbox.org/download_files.php", low_speed_time = 300) |>
      httr2::req_headers(!!!.rbox_headers()) |>
      httr2::req_body_json(list(
        files = I(ids),
        box_id = info$box_id,
        reference = info$reference
      )) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(path = zip_path)
    httr2::resp_status(resp)
  }, error = \(e) NA_integer_)

  # After several downloads the page asks for a captcha (a 429 whose JSON body
  # has action "captcha", checked 2026-10-26); waiting a few seconds does not
  # lift it, so it is reported as a limit a later run may get past
  if (identical(dl_status, 429L)) {
    refusal <- tryCatch(jsonlite::fromJSON(zip_path), error = \(e) NULL)
    unlink(zip_path)
    if (identical(refusal$action, "captcha")) {
      return(list(ok = FALSE, error = paste(
        "API rate limit exhausted: ResearchBox asks for a captcha before more",
        "downloads from its web page; an API key (see ?rbox_token) avoids this")))
    }
  }

  if (!identical(dl_status, 200L) ||
      !file.exists(zip_path) || file.size(zip_path) == 0) {
    return(list(ok = FALSE, error = paste0(
      "download failed or resulted in an empty file",
      if (!is.na(dl_status)) paste0(" (HTTP ", dl_status, ")"))))
  }

  list(ok = TRUE, error = NA_character_,
       single_name = if (length(file_ids) == 1) info$files[[1]]$name[[1]] else NA_character_)
}

#' Check downloaded ResearchBox files against their checksums
#'
#' Compares every unzipped file with the MD5 checksum ResearchBox lists for a
#' file of the same name, and records every listed file that did not arrive
#' or does not match. Files are matched by name, not by folder: the zip puts
#' them in folders by type or by section, and the file list gives both, but
#' the folder names are ResearchBox's own and could change.
#'
#' @param rb_url the box's URL, for the failed table
#' @param out_dir the folder the box was unzipped into
#' @param listed the box's file table: from the zip's table of contents, or
#'   from the API ([rbox_files()] without `rb_url`); NULL when there is none
#'   (then nothing can be checked)
#' @param file_ids the ids of the files that were asked for, or `"all"`. Only
#'   these must arrive; every file that does arrive is checked, including a
#'   codebook ResearchBox added. An id that `listed` does not have (the table
#'   of contents lists only the files in the zip) did not arrive.
#'
#' @returns a list with `checksum_ok` (a named logical vector, one per
#'   unzipped file, named by its path in `out_dir`; NA when no listed file has
#'   that name) and `failed` (a data frame with `rb_url`, `name` and `error`)
#' @keywords internal
.rbox_verify_downloads <- function(rb_url, out_dir, listed, file_ids = "all") {
  paths <- list.files(out_dir, recursive = TRUE)
  checksum_ok <- stats::setNames(rep(NA, length(paths)), paths)
  failed <- data.frame(rb_url = character(0), name = character(0),
                       error = character(0))
  if (is.null(listed) || nrow(listed) == 0 || !"md5" %in% names(listed)) {
    return(list(checksum_ok = checksum_ok, failed = failed))
  }

  md5 <- tolower(unname(tools::md5sum(file.path(out_dir, paths))))
  for (i in seq_along(paths)) {
    expected <- tolower(listed$md5[listed$name == basename(paths[[i]])])
    if (length(expected)) checksum_ok[[i]] <- md5[[i]] %in% expected
  }

  # every file asked for must have arrived with a matching checksum
  wanted <- if (identical(as.character(file_ids), "all")) seq_len(nrow(listed))
            else which(listed$file_id %in% as.character(file_ids))
  for (i in wanted) {
    same_name <- basename(paths) == listed$name[[i]]
    if (!any(same_name)) {
      error <- "not in the download"
    } else if (!tolower(listed$md5[[i]]) %in% md5[same_name]) {
      error <- "checksum does not match"
    } else {
      next
    }
    failed <- rbind(failed, data.frame(rb_url = rb_url, name = listed$name[[i]],
                                       error = error))
  }
  if (!identical(as.character(file_ids), "all")) {
    missing <- setdiff(as.character(file_ids), listed$file_id)
    if (length(missing)) {
      failed <- rbind(failed, data.frame(rb_url = rb_url,
                                         name = paste("file_id", missing),
                                         error = "not in the download"))
    }
  }

  list(checksum_ok = checksum_ok, failed = failed)
}
