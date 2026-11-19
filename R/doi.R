#' Clean DOIs
#'
#' Removes a surrounding doi.org or journal URL, a "doi:" label, a section
#' marker, whitespace, trailing punctuation, and an unmatched closing angle
#' bracket, parenthesis or square bracket. Decodes "%2F" as "/". Balanced
#' parentheses within the DOI are kept.
#'
#' @param doi a character vector of one or more DOIs
#'
#' @returns a character vector of cleaned DOIs (no https://doi.org or DOI:)
#' @export
#'
#' @examples
#' doi_clean("https://doi.org/10.1038/nphys1170")
#' doi_clean("doi:10.1038/nphys1170")
#' doi_clean("DOI: 10.1038/nphys1170")
#' doi_clean("<https://doi.org/10.1037/0003-066x.54.6.4 08>.")
doi_clean <- function(doi) {
  doi <- doi |>
    unlist() |>
    as.character() |>
    trimws()

  # remove prefixes
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi <- sub("^doi\\s*:\\s*", "", doi, ignore.case = TRUE)
  # handle journal specific "doi" like
  # http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0004153
  doi <- sub("^.*?(10\\.\\d{3,}.*)$", "\\1", doi, perl = TRUE)
  # remove # section markers
  doi <- sub("#.*$", "", doi)
  # remove /full off the end
  doi <- sub("/full$", "", doi)

  # PDF reading: spaces from line breaks ("10.1037/0003-066x.54.6.4 08"), a
  # URL-encoded slash, and punctuation or a closing bracket after the DOI
  # ("<https://doi.org/...>.", "(https://doi.org/...)."). A closing bracket
  # is dropped only when it has no opening one in the DOI, as in
  # "10.1016/0010-0277(93)90058-4" it is part of the DOI.
  doi <- gsub("[[:space:]]+", "", doi)
  doi <- gsub("%2F", "/", doi, ignore.case = TRUE)
  unbalanced <- \(x, open, close) {
    !is.na(x) & lengths(regmatches(x, gregexpr(close, x, fixed = TRUE))) >
      lengths(regmatches(x, gregexpr(open, x, fixed = TRUE)))
  }
  repeat {
    before <- doi
    doi <- sub("[.,;:]+$", "", doi)
    for (b in list(c("(", ")"), c("<", ">"), c("[", "]"))) {
      end <- !is.na(doi) & endsWith(doi, b[2]) & unbalanced(doi, b[1], b[2])
      doi[end] <- substr(doi[end], 1, nchar(doi[end]) - 1)
    }
    if (identical(before, doi)) break
  }

  return(doi)
}

#' Validate DOI format
#'
#' @param doi a character vector of one or more DOIs
#'
#' @returns a logical vector
#' @export
#'
#' @examples
#' doi_valid_format("10.1038/nphys1170")
#' doi_valid_format("no.no.10.1038")
doi_valid_format <- function(doi) {
  pattern <- paste0(
    "^10\\.\\d{3,9}\\/", # 10.
    "[-._;()/:<>A-Za-z0-9]*", # valid characters
    "[A-Za-z0-9]$" # must end in a number/letter
  )
  valid_format <- grepl(pattern, doi, perl = TRUE)

  return(valid_format)
}

#' Check whether a DOI resolves
#'
#' Checks the doi.org API to see if a DOI is registered and has an associated URL
#' (using `https://doi.org/api/handles`). Returns TRUE if it does, FALSE if the DOI
#' does not exist or does not have an associated URL, and NA if the test failed.
#' Clearly invalid DOIs (i.e. not starting with "10.") will return FALSE without
#' server requests.
#'
#' @param doi Character vector. One or more DOIs to check.
#' @param timeout Numeric. Request timeout in seconds. Default is `10`.
#'
#' @return Logical vector. For each input DOI, returns TRUE if the DOI resolves,
#'  FALSE if it does not resolve (or does not start with 10.), and NA if the check failed.
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (online("doi.org")) {
#'   doi_resolves("10.1038/nphys1170") # Expected: TRUE
#'   doi_resolves("10.1234/invalid.doi") # Expected: FALSE
#' }
#' }
doi_resolves <- function(doi, timeout = 10) {
  doi <- doi_clean(doi)

  # separate valid/invalid DOIs upfront
  valid_format <- doi_valid_format(doi)
  na_or_empty <- is.na(doi) | !nzchar(doi)
  needs_check <- !na_or_empty & valid_format

  res <- rep(NA, length(doi))
  res[!na_or_empty & !valid_format] <- FALSE

  if (any(needs_check)) {
    check_dois <- doi[needs_check]
    urls <- paste0(
      "https://doi.org/api/handles/",
      utils::URLencode(check_dois, reserved = TRUE),
      "?type=URL"
    )

    # only show progress bar if > batch_size
    batch_size <- 500
    msg <- if (length(urls) > batch_size) { "Checking DOIs" } else { NULL }
    resps <- .batch_query(urls, batch_size = batch_size, msg = msg, delay = 0)

    check_res <- vapply(resps, \(resp) {
      # a connection failure comes back from .batch_query() as NULL (U33)
      if (is.null(resp) || inherits(resp, "error")) return(NA)
      if (!resp$status_code %in% 200L) return(FALSE)

      body <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
      code <- body$responseCode
      if (is.null(code) || length(code) != 1L) return(NA)
      if (code == 1L) return(TRUE)
      if (code == 100L) return(FALSE)
      if (code == 2L) return(NA)
      if (code == 200L) return(FALSE)
      NA
    }, logical(1))

    res[needs_check] <- check_res
  }

  return(res)
}

#' Doi.org Info from DOI
#'
#' @param doi the DOI(s) to get info for
#'
#' @return data frame with DOIs and info
#' @export
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \donttest{
#' if (online("doi.org")) {
#'   doi_info <- doi_lookup(doi)
#' }
#' }
doi_lookup <- function(doi) {
  if (length(doi) == 0) {
    return(data.frame(doi = character(0)))
  }

  # build requests for non-NA DOIs, perform in parallel
  cleaned <- doi_clean(doi)
  is_valid <- !is.na(doi)
  valid_idx <- which(is_valid)

  urls <- paste0(
    "https://doi.org/",
    utils::URLencode(cleaned[valid_idx], reserved = TRUE)
  )
  # small batch size because most lookups use crossref and email isn't supplied
  resps <- .batch_query(urls, batch_size = 3, msg = "DOI Lookup")

  # process responses
  bibdata <- vector("list", length(doi))
  for (i in seq_along(doi)) {
    if (!is_valid[i]) {
      bibdata[[i]] <- list(doi = doi[i])
      next
    }
  }
  # A failed lookup is an empty row of its own: return() inside tryCatch()
  # returned from doi_lookup(), so one 404 emptied the whole result, and
  # bibdata[[i]] <- NULL deleted the element, shifting later rows (U12)
  for (j in seq_along(valid_idx)) {
    i <- valid_idx[j]
    resp <- resps[[j]]
    failed <- is.null(resp) || inherits(resp, "error") || httr2::resp_status(resp) >= 400
    bd <- if (failed) NULL else tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    bibdata[i] <- list(bd %||% list(doi = doi[i]))
  }

  bib_table <- lapply(bibdata, \(bd) {
    # pages
    first_page <- NA_character_
    last_page <- NA_character_
    if (!is.null(bd$page)) {
      pages <- strsplit(bd$page, "-")[[1]]
      first_page <- pages[[1]]
      if (length(pages) > 1) last_page <- pages[[2]]
    }

    # authors
    # an organisation author has a name or literal, not family and given:
    # paste() of two NULLs gave character(0) in the author string (U12)
    authors <- bd$author |>
      vapply(\(a) {
        fam <- a$family %||% a$name %||% a$literal %||% ""
        giv <- a$given %||% ""
        if (nzchar(giv)) paste(fam, giv, sep = ", ") else fam
      }, character(1)) |>
      paste(collapse = "; ")

    info <- list(
      doi        = bd[["DOI"]] %||% NA_character_,
      type       = bd[["type"]] %||% NA_character_,
      title      = bd[["title"]] %||% NA_character_,
      container  = bd[["container-title"]] %||% NA_character_,
      year       = bd[["published"]]$`date-parts`[[1]][[1]] %||% NA_real_,
      author     = authors %||% NA_character_,
      volume     = bd[["volume"]] %||% NA_character_,
      issue      = bd[["issue"]] %||% NA_character_,
      first_page = first_page,
      last_page  = last_page,
      editor     = bd[["editor"]] %||% NA_character_,
      publisher  = bd[["publisher"]] %||% NA_character_,
      url        = bd[["URL"]] %||% NA_character_
    )

    lapply(info, \(i) {
      if (length(i) == 1 & is.atomic(i)) return(i[[1]])
      unlist(i) |> paste(sep = ", ", collapse = "; ")
    })
  }) |> dplyr::bind_rows()

  bib_table
}
