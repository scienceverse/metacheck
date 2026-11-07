#' Find AsPredicted Links in Papers
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the AsPredicted url in the first (href) column
#' @export
#'
#' @examples
#' paper <- demopaper()
#' aspredicted_links(paper)
aspredicted_links <- function(paper) {
  href <- NULL

  paper_table(paper, "url") |>
    dplyr::filter(grepl("aspredicted\\.org", href, ignore.case = TRUE))
}

#' Retrieve info from AsPredicted by URL
#'
#' With a ResearchBox API key set (see [rbox_token()]), each preregistration
#' is read through ResearchBox's API, which serves AsPredicted's public
#' preregistrations, with one request each. Without a key, or when the API
#' fails, the preregistration's web page on AsPredicted is read instead;
#' AsPredicted asks for a captcha after several pages, and no page is read
#' after that.
#'
#' Both routes give the same columns. Each answer is placed by the wording of
#' its question, so both versions of AsPredicted's questions found so far
#' (1.05 and 2.00) are read: version 1.05 asks about secondary analyses
#' (`AP_secondary`) where version 2.00 asks about outliers (`AP_outliers`). An
#' answer to a question that is not recognised is added to `AP_anything_else`,
#' after its question. `AP_created` is the time of registration as AsPredicted
#' shows it ("2021/06/14 02:17 (PT)") and `AP_registered` its date
#' ("2021-06-14"). `AP_number` (AsPredicted's number), `AP_status`,
#' `AP_authors_count` and `AP_bundle` (the numbers of the preregistrations
#' registered together with this one) come from the API only, and are `NA`
#' when the web page was read. From the API, `AP_authors` gives each author's
#' name and university, without the e-mail address the web page shows.
#'
#' A link that names no preregistration, such as AsPredicted's home page, is
#' not looked up: its row has an `error`.
#'
#' @param ap_url an AsPredicted URL, or a table containing them (e.g., as created by `aspredicted_links()`)
#' @param id_col the index or name of the column that contains AsPredicted URLs, if id is a table
#' @param wait wait time in seconds after each web page read (the API's
#'   requests are spaced by ResearchBox's own limits)
#' @param cache if `TRUE`, reuse a previously cached result for a
#'   preregistration already looked up (see [repo_info_cache_clear()])
#'   instead of asking again. Off by default.
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \donttest{
#' if (online("aspredicted.org")) {
#'   aspredicted_info("https://aspredicted.org/blind.php?x=nq4xa3")
#' }
#' }
aspredicted_info <- function(ap_url, id_col = 1, wait = 1, cache = FALSE) {
  # aspredicted.org is only needed when its pages are read
  if (!nzchar(rbox_token()) && !online("aspredicted.org")) {
    stop("AsPredicted.org seems to be offline")
  }

  # handle list of links
  if (is.data.frame(ap_url)) {
    table <- ap_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "ap_url"
    raw_urls <- unique(ap_url) |> stats::na.omit()
    table <- data.frame(ap_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    ap_url = raw_urls
  )
  ids <- ids[!is.na(ids$ap_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$ap_url)

  if (length(valid_ids) == 0) {
    message("No valid AsPredicted links")
    return(table)
  }

  # iterate over valid IDs
  message(
    "Starting AsPredicted retrieval for ",
    length(valid_ids), " file",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  )
  .aspredicted_warn_if_no_key()

  # After AsPredicted's captcha no further page is read, but the API is still
  # asked for the remaining preregistrations: this loop used to stop at the
  # captcha, and every later preregistration got no information.
  id_info <- vector("list", length(valid_ids))
  captcha <- FALSE
  for (i in seq_along(valid_ids)) {
    u <- valid_ids[[i]]
    ap_id <- .ap_id(u)
    cached <- if (isTRUE(cache) && !is.na(ap_id)) .repo_info_cache_get("aspredicted", ap_id) else NULL
    if (!is.null(cached)) {
      # cached by identifier, so a different link to it gets its own url
      cached$ap_url <- u
      id_info[[i]] <- cached
      next
    }
    ap <- .aspredicted_info(u, page = !captcha)
    if (grepl("captcha", ap$error %||% "", fixed = TRUE)) captcha <- TRUE
    if (identical(attr(ap, "route"), "page")) Sys.sleep(wait)
    attr(ap, "route") <- NULL
    if (isTRUE(cache) && !is.na(ap_id) && .repo_info_ok(ap))
      .repo_info_cache_put("aspredicted", ap_id, ap)
    id_info[[i]] <- ap
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "ap_url")

  # reduplicate and add original table info
  by <- stats::setNames("ap_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".ap")
  )

  message("...AsPredicted retrieval complete!")

  return(data)
}

# A message, once per R session, that without a ResearchBox API key the
# preregistrations are read from AsPredicted's web pages, which stop after
# AsPredicted's captcha, so the user knows why only the first few were read.
# Same pattern as .dryad_warn_if_no_key(); reset with
# options(metacheck.aspredicted_key.notified = NULL) to see it again.
.aspredicted_warn_if_no_key <- function() {
  if (isTRUE(getOption("metacheck.aspredicted_key.notified")) ||
      nzchar(rbox_token())) return(invisible())
  message(
    "No ResearchBox API key is set, so AsPredicted preregistrations are read ",
    "from AsPredicted's web pages. AsPredicted asks for a captcha after ",
    "several pages, after which no more are read. Set a key with ",
    "rbox_token() (make one at ", .key_services$researchbox$url, ")."
  )
  options(metacheck.aspredicted_key.notified = TRUE)
}

#' Retrieve and process info from AsPredicted by URL
#'
#' Uses ResearchBox's API when a key is set (see [rbox_token()]), and reads
#' the preregistration's web page otherwise, or when the API fails. See
#' [aspredicted_info()] for the columns.
#'
#' @param ap_url an AsPredicted URL
#' @param page if `FALSE`, the web page is not read when the API fails (after
#'   AsPredicted asked for a captcha)
#'
#' @returns a data frame of information; attribute `route` is `"api"` or
#'   `"page"` when that was asked
#' @keywords internal
.aspredicted_info <- function(ap_url, page = TRUE) {
  message("* Retrieving info from ", ap_url, "...")

  path <- .ap_path(ap_url)
  if (is.na(path) || !nzchar(path)) {
    obj <- data.frame(ap_url = ap_url)
    obj$error <- "the link names no AsPredicted preregistration"
    warning(ap_url, " error: ", obj$error, call. = FALSE)
    return(obj)
  }

  api_error <- NULL
  if (nzchar(rbox_token())) {
    info <- .aspredicted_info_api(ap_url)
    if (is.null(info$error)) return(structure(info, route = "api"))
    api_error <- info$error
    message("- ResearchBox API: ", api_error,
            if (page) "; reading the web page instead")
  }

  if (!page) {
    obj <- data.frame(ap_url = ap_url)
    if (identical(api_error, "unfound"))
      api_error <- "ResearchBox's API does not have it, or it is not public"
    obj$error <- paste0(if (!is.null(api_error)) paste0(api_error, "; "),
                        "web page not read: AsPredicted asked for a captcha")
    return(obj)
  }

  info <- .aspredicted_info_page(ap_url)
  # Both failed: the API's reason first, so a refusal a later run will get
  # past keeps its "API rate limit exhausted: " start. "unfound" from the API
  # may be a preregistration that is not public, which the page tells.
  if (!is.null(api_error) && api_error != "unfound" && "error" %in% names(info))
    info$error <- paste0(api_error, "; web page: ", info$error)
  structure(info, route = "page")
}

# The part of an AsPredicted link after "aspredicted.org/" ("" for the home
# page), or NA for a link that is not to aspredicted.org (such as a link cut
# off at "https://aspredicted").
.ap_path <- function(ap_url) {
  x <- trimws(as.character(ap_url))
  rx <- "^(https?://)?(www\\.)?aspredicted\\.org(/|$)"
  path <- sub(rx, "", x, ignore.case = TRUE)
  path[!grepl(rx, x, ignore.case = TRUE)] <- NA_character_
  sub("/+$", "", path)
}

# The identifier ResearchBox's API takes for an AsPredicted link, or NA. The
# links found in papers are a PDF's short name ("ve2qn.pdf",
# "3pc3-6tw7.pdf"), a share code ("Y2F_6B7") or a "blind.php?x=" link; the
# API refuses the blind link itself but takes the code after "x=" (checked
# 2026-11-05). Any other link is not sent, as each identifier the API cannot
# find counts toward a block of the key (see .ap_unfound_limit).
.ap_id <- function(ap_url) {
  path <- .ap_path(ap_url)
  path <- sub("^blind\\.php\\?x=", "", path, ignore.case = TRUE)
  path[!grepl("^[A-Za-z0-9_-]+(\\.pdf)?$", path)] <- NA_character_
  path
}

# ResearchBox blocks the key for an hour after 5 different AsPredicted
# identifiers within 24 hours that it cannot find or that are not public, and
# for a day after 10. Each such identifier is recorded for 24 hours in
# metacheck's state folder (see .metacheck_state_dir()), so jobs with the same
# key share the count. An identifier recorded is not sent again, and from
# .ap_unfound_limit identifiers on, none is sent; the web page is read
# instead.
.ap_unfound_limit <- 4

.ap_unfound_path <- function() {
  file.path(.metacheck_state_dir(), "aspredicted_unfound_ids.rds")
}

# The identifiers recorded in the last 24 hours, with the time of each
.ap_unfound_ids <- function() {
  path <- .ap_unfound_path()
  x <- if (file.exists(path)) tryCatch(readRDS(path), error = \(e) NULL)
  if (!is.data.frame(x) || !all(c("id", "time") %in% names(x))) {
    return(data.frame(id = character(0), time = as.POSIXct(character(0))))
  }
  x[difftime(Sys.time(), x$time, units = "hours") < 24, , drop = FALSE]
}

.ap_unfound_add <- function(id) {
  path <- .ap_unfound_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- rbind(.ap_unfound_ids(), data.frame(id = id, time = Sys.time()))
  tryCatch(saveRDS(x, path), error = \(e) NULL)
  invisible(NULL)
}

# Words that identify each question of AsPredicted's versions 1.05 and 2.00,
# in their wording on the web page and in the API (lower case)
.ap_questions <- c(
  AP_data = "have any data been collected",
  AP_hypotheses = "main question being asked",
  AP_key_dv = "describe the key dependent variable",
  AP_conditions = "how many and which conditions",
  AP_analyses = "specify exactly which analyses",
  AP_outliers = "describe exactly how outliers",
  AP_secondary = "any secondary analyses",
  AP_sample_size = "how many observations will be collected",
  AP_anything_else = "anything else you would like to pre-register"
)

# "06/14/2021 02:17 AM" from the API as the web page writes it,
# "2021/06/14 02:17 (PT)": the same time, in AsPredicted's time zone (Pacific
# time; checked 2026-11-05 against the page of the same preregistration).
# Anything else is returned unchanged.
.ap_api_time <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) return(NA_character_)
  m <- regmatches(x, regexec(
    "^([0-9]{2})/([0-9]{2})/([0-9]{4}) ([0-9]{1,2}):([0-9]{2}) ([AP]M)$",
    trimws(x)))[[1]]
  if (length(m) != 7) return(x)
  hour <- as.integer(m[[5]]) %% 12 + if (m[[7]] == "PM") 12 else 0
  sprintf("%s/%s/%s %02d:%s (PT)", m[[4]], m[[2]], m[[3]], hour, m[[6]])
}

# "2021/06/14 02:17 (PT)" as "2021-06-14", or NA
.ap_iso_date <- function(x) {
  m <- regmatches(x, regexec("^([0-9]{4})/([0-9]{2})/([0-9]{2})", x %||% ""))[[1]]
  if (length(m) == 4) paste(m[[2]], m[[3]], m[[4]], sep = "-") else NA_character_
}

# The authors line the web page shows for an anonymous preregistration
.ap_anonymous_authors <- function(n) {
  paste0("This pre-registration is currently anonymous to enable blind peer-review.\n",
         "It has ", if (identical(as.integer(n), 1L)) "one author" else paste(n, "authors"), ".")
}

# A preregistration from ResearchBox's API (one request). Returns the same
# columns as .aspredicted_info_page(); on failure only `ap_url` and `error`.
.aspredicted_info_api <- function(ap_url) {
  obj <- data.frame(ap_url = ap_url)
  id <- .ap_id(ap_url)
  if (is.na(id)) {
    obj$error <- "unfound"
    return(obj)
  }
  # see .ap_unfound_limit
  unfound <- unique(.ap_unfound_ids()$id)
  if (id %in% unfound) {
    obj$error <- "unfound"
    return(obj)
  }
  if (length(unfound) >= .ap_unfound_limit) {
    obj$error <- paste0(
      "not sent: ResearchBox could not find ", length(unfound),
      " AsPredicted preregistrations asked for in the last 24 hours; it ",
      "blocks the key after 5, so no other preregistration is sent to the ",
      "API until then")
    return(obj)
  }

  res <- .rbox_api("get_aspredicted", id = id, format = "json")
  if (!res$ok) {
    if (identical(res$error, "unfound")) .ap_unfound_add(id)
    obj$error <- res$error
    return(obj)
  }

  b <- res$body
  # "" and null are how the API reports a field it does not have
  field <- \(x) if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) NA_character_ else as.character(x)

  obj$AP_title <- field(b$title)
  n_authors <- if (is.null(b$authors_count)) NA_integer_ else as.integer(b$authors_count)
  authors <- b$authors
  obj$AP_authors <- if (isTRUE(b$anonymous)) {
    .ap_anonymous_authors(n_authors)
  } else if (is.data.frame(authors) && nrow(authors) > 0) {
    name <- trimws(paste(authors$first %||% "", authors$last %||% ""))
    uni <- authors$uni %||% rep("", nrow(authors))
    uni[is.na(uni)] <- ""
    paste0(name, ifelse(nzchar(uni), paste0(" (", uni, ")"), "")) |>
      gsub("[[:space:]]+", " ", x = _) |>
      paste(collapse = "\n")
  } else {
    NA_character_
  }
  obj$AP_created <- .ap_api_time(b$registration_time)

  # Each answer is placed by its question's wording; the numbers differ
  # between versions (question 6 is about outliers in 2.00 and secondary
  # analyses in 1.05). A question not recognised is kept with its answer.
  questions <- b$questions
  obj[names(.ap_questions)] <- NA_character_
  other <- character(0)
  if (is.data.frame(questions) && nrow(questions) > 0) {
    for (k in seq_len(nrow(questions))) {
      q <- .rbox_plain_text(field(questions$question[[k]]))
      if (is.na(q)) q <- ""
      a <- questions$answer[[k]]
      a <- if (is.null(a) || is.na(a)) "" else trimws(gsub("\r\n?", "\n", a))
      hit <- names(.ap_questions)[vapply(.ap_questions, grepl, logical(1),
                                         x = tolower(q), fixed = TRUE)]
      if (length(hit) == 0) {
        other <- c(other, paste0(q, "\n", a))
      } else if (is.na(obj[[hit[[1]]]])) {
        obj[[hit[[1]]]] <- a
      } else {
        obj[[hit[[1]]]] <- paste(obj[[hit[[1]]]], a, sep = "\n\n")
      }
    }
  }
  if (length(other) > 0) {
    obj$AP_anything_else <- paste(c(obj$AP_anything_else[!is.na(obj$AP_anything_else)],
                                    other), collapse = "\n\n")
  }

  obj$AP_version <- field(b$question_version)
  obj$AP_registered <- .ap_iso_date(obj$AP_created)
  obj$AP_anonymous <- if (is.null(b$anonymous)) NA else isTRUE(b$anonymous)
  obj$AP_number <- field(b$a_id)
  obj$AP_status <- field(b$status)
  obj$AP_authors_count <- n_authors
  obj$AP_bundle <- if (length(b$bundle_a_ids)) paste(b$bundle_a_ids, collapse = ", ") else NA_character_

  obj
}

# A preregistration read from its web page on AsPredicted
.aspredicted_info_page <- function(ap_url) {
  # set up return table
  obj <- data.frame(
    ap_url = ap_url
  )

  resp <- .mc_request(ap_url) |>
    httr2::req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      `User-Agent` = paste(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "AppleWebKit/537.36 (KHTML, like Gecko)",
        "Chrome/137.0.0.0 Safari/537.36",
        "scienceverse/metacheck"
      )
    ) |>
    # without req_error(), an HTTP error status raised before the status check
    # below and stopped prereg_check (U54); a failed connection did too
    httr2::req_error(is_error = \(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(resp), error = \(e) NULL)
  if (is.null(resp)) {
    obj$error <- "the connection failed"
    warning(ap_url, " error: ", obj$error, call. = FALSE)
    return(obj)
  }

  # handle missing file
  if (httr2::resp_status(resp) != 200) {
    obj$error <- httr2::resp_status_desc(resp)
    warning(ap_url, " error: " , obj$error, call. = FALSE)
    return(obj)
  }

  # Read the content with specified encoding
  html <- httr2::resp_body_string(resp) |>
    xml2::read_html()

  body <- xml2::xml_find_all(html, "//body") |>
    rvest::html_text2() # xml2::xml_text()

  if (grepl("CLICK after solving captcha", body, fixed = TRUE)) {
    warning("Log in to AsPredicted to bypass the CAPTCHA", call. = FALSE)
    obj$error <- "captcha"
    return(obj)
  }

  obj$AP_title <- xml2::xml_find_first(html, "//h3 //b //i") |>
    xml2::xml_text()

  # Section headings, in the wording of version 2.00 and, where it differs,
  # of version 1.05 (question 6, and the examples under question 8)
  sections <- list(
    AP_authors = "Author(s)",
    AP_created = "Pre-registered on",
    AP_data = "1) Have any data been collected for this study already?",
    AP_hypotheses = "2) What's the main question being asked or hypothesis being tested in this study?",
    AP_key_dv = "3) Describe the key dependent variable(s) specifying how they will be measured.",
    AP_conditions = "4) How many and which conditions will participants be assigned to?",
    AP_analyses = "5) Specify exactly which analyses you will conduct to examine the main question/hypothesis.",
    AP_outliers = "6) Describe exactly how outliers will be defined and handled, and your precise rule(s) for excluding observations.",
    AP_secondary = "6) Any secondary analyses?",
    AP_sample_size = "7) How many observations will be collected or what will determine sample size?\nNo need to justify decision, but be precise about exactly how the number will be determined.",
    AP_anything_else = c(
      "8) Anything else you would like to pre-register?\n(e.g., secondary analyses, variables collected for exploratory purposes, unusual analyses planned?)",
      "8) Anything else you would like to pre-register?\n(e.g., data exclusions, variables collected for exploratory purposes, unusual analyses planned?)"
    ),
    AP_version = "Version of AsPredicted Questions: ",
    bug = "Report a bug"
  )

  # Where each heading starts and ends (NA when the page does not have it).
  # Each section is cut at the first later heading the page has: a section
  # used to be cut only at the next heading in the list, so on a version 1.05
  # page, which has no outlier question, the analyses took in questions 6 to
  # 8 and the page's scripts, and the outliers were NA.
  found <- lapply(sections, \(alts) {
    for (s in alts) {
      pos <- regexpr(s, body, fixed = TRUE)
      if (pos > 0) return(c(start = pos, end = pos + nchar(s)))
    }
    c(start = NA, end = NA)
  })
  starts <- vapply(found, `[[`, numeric(1), "start")
  for (nm in setdiff(names(sections), "bug")) {
    start <- found[[nm]][["start"]]
    if (is.na(start)) {
      obj[[nm]] <- NA_character_
      next
    }
    later <- starts[!is.na(starts) & starts > start]
    end <- if (length(later)) min(later) - 1 else nchar(body)
    obj[[nm]] <- trimws(substr(body, found[[nm]][["end"]], end))
  }

  obj$AP_registered <- .ap_iso_date(obj$AP_created)
  obj$AP_anonymous <- if (is.na(obj$AP_authors)) NA else
    grepl("currently anonymous", obj$AP_authors, fixed = TRUE)
  obj$AP_number <- NA_character_
  obj$AP_status <- NA_character_
  obj$AP_authors_count <- NA_integer_
  obj$AP_bundle <- NA_character_

  return(obj)
}
