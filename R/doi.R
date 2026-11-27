#' Clean DOIs
#'
#' Removes a surrounding doi.org or journal URL, a "doi:" label, a section
#' marker, whitespace, trailing punctuation, and an unmatched closing angle
#' bracket, parenthesis or square bracket. Decodes valid UTF-8 percent-encoded
#' characters (such as "%2F", "%E2%80%90", "%5B", "%5D") and normalises Unicode
#' dashes to ASCII hyphens. Balanced brackets within the DOI are kept.
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
    as.character()

  if (!length(doi)) return(doi)

  # convert native/latin1 strings to UTF-8, then guard invalid byte sequences
  doi_u <- enc2utf8(doi)
  ok <- !is.na(doi_u) & validUTF8(doi_u)
  if (!any(ok)) return(doi)
  s <- trimws(doi_u[ok])

  # remove prefixes
  s <- sub("^https?://(dx\\.)?doi\\.org/", "", s, ignore.case = TRUE)
  s <- sub("^doi\\s*:\\s*", "", s, ignore.case = TRUE)
  # handle journal specific "doi" like
  # http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0004153
  s <- sub("^.*?(10\\.\\d{3,}.*)$", "\\1", s, perl = TRUE)

  # decode HTML entities before stripping "#" fragments so numeric entities
  # like "&#160;" do not truncate the DOI suffix
  decode_doi_entities <- \(v) {
    has_ent <- grepl("&", v, fixed = TRUE)
    if (!any(has_ent)) return(v)
    t <- v[has_ent]
    t <- gsub("&lt;", "<", t, ignore.case = TRUE)
    t <- gsub("&gt;", ">", t, ignore.case = TRUE)
    t <- gsub("&(ndash|mdash|minus);", "-", t, ignore.case = TRUE)
    t <- gsub("&(nbsp|#160|#x0*a0|x0*a0);?", "", t, ignore.case = TRUE)
    t <- gsub("&amp;", "&", t, ignore.case = TRUE)
    v[has_ent] <- t
    v
  }
  s <- decode_doi_entities(s)

  # remove # section markers before percent-decoding (preserves encoded %23
  # and SICI check characters ending in ";2-#")
  s <- sub("(?<!;2-)#.*$", "", s, perl = TRUE)
  # remove /full off the end
  s <- sub("/full$", "", s)

  # PDF reading: spaces from line breaks ("10.1037/0003-066x.54.6.4 08") and
  # literal wrapping punctuation/brackets ("<https://doi.org/...>.", "(...)")
  s <- gsub("[[:space:]]+", "", s)
  unbalanced <- \(x, open, close) {
    lengths(regmatches(x, gregexpr(close, x, fixed = TRUE))) >
      lengths(regmatches(x, gregexpr(open, x, fixed = TRUE)))
  }
  strip_wrapping <- \(v) {
    repeat {
      before <- v
      v <- sub("[.,;:]+$", "", v)
      for (b in list(c("(", ")"), c("<", ">"), c("[", "]"))) {
        end <- endsWith(v, b[2]) & unbalanced(v, b[1], b[2])
        v[end] <- substr(v[end], 1, nchar(v[end]) - 1)
      }
      if (identical(before, v)) break
    }
    v
  }
  s <- strip_wrapping(s)

  # Decode valid percent-encoded UTF-8 sequences in one pass without failing
  # on a stray "%", while keeping control bytes (%00-%1F, %7F), %20 (space),
  # %23 (#), and %25 (%) encoded so doi_clean() is idempotent and never alters
  # %252F or drops %20.
  enc <- grepl("%[0-9A-Fa-f]{2}", s)
  if (any(enc)) {
    # match contiguous percent-encoded runs excluding %00-%1F, %20, %23, %25, %7F
    safe_byte <- "%(?!(?:0[0-9A-Fa-f]|1[0-9A-Fa-f]|20|23|25|7[Ff]))[0-9A-Fa-f]{2}"
    run_pat <- paste0("(?:", safe_byte, ")+")
    s[enc] <- vapply(s[enc], \(x) {
      m <- gregexpr(run_pat, x, perl = TRUE)
      ms <- regmatches(x, m)[[1]]
      if (!length(ms)) return(x)
      reps <- vapply(ms, \(tok) {
        y <- tryCatch(utils::URLdecode(tok), error = \(e) NA_character_)
        if (is.na(y)) return(tok)
        Encoding(y) <- "UTF-8"
        if (!validUTF8(y)) return(tok)
        y
      }, character(1))
      regmatches(x, m) <- list(reps)
      x
    }, character(1), USE.NAMES = FALSE)
    # decode entities that were percent-encoded (%26lt%3B -> &lt; -> <)
    s[enc] <- decode_doi_entities(s[enc])
  }

  s <- gsub("\\p{Pd}", "-", s, perl = TRUE)
  s <- strip_wrapping(s)

  doi[ok] <- s
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
    "[-._;()/:<>\\[\\]+&A-Za-z0-9]*", # valid characters (incl. SICI [], +, &)
    "(?:[A-Za-z0-9]|(?<=;2-)#)$" # must end in a letter/number (or SICI ";2-#")
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
    resps <- .batch_query(urls, batch_size = batch_size, msg = msg, delay = 0,
                          timeout_s = timeout)

    check_res <- vapply(resps, \(resp) {
      # a connection failure comes back from .batch_query() as NULL (U33)
      if (is.null(resp) || inherits(resp, "error")) return(NA)
      status <- httr2::resp_status(resp)
      if (status == 429L || status >= 500L) return(NA)
      if (status != 200L) return(FALSE)

      body <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
      code <- if (is.list(body)) body[["responseCode"]] else NULL
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
    if (!is.list(bd)) bd <- NULL
    bibdata[i] <- list(bd %||% list(doi = doi[i]))
  }

  bib_table <- lapply(bibdata, \(bd) {
    # pages
    first_page <- NA_character_
    last_page <- NA_character_
    page_val <- .json_chr(bd[["page"]])
    if (!is.na(page_val) && nzchar(page_val)) {
      pages <- strsplit(page_val, "-")[[1]]
      first_page <- pages[[1]]
      if (length(pages) > 1) last_page <- pages[[2]]
    }

    # authors
    # an organisation author has a name or literal, not family and given:
    # paste() of two NULLs gave character(0) in the author string (U12)
    raw_authors <- bd[["author"]]
    authors <- if (is.list(raw_authors) && length(raw_authors)) {
      raw_authors |>
        vapply(\(a) {
          if (!is.list(a)) return("")
          fam <- .json_chr(a[["family"]], "")
          if (!nzchar(fam)) fam <- .json_chr(a[["name"]], "")
          if (!nzchar(fam)) fam <- .json_chr(a[["literal"]], "")
          giv <- .json_chr(a[["given"]], "")
          if (nzchar(giv) && nzchar(fam)) paste(fam, giv, sep = ", ") else fam
        }, character(1)) |>
        paste(collapse = "; ")
    } else ""

    pub_dp <- if (is.list(bd[["published"]])) bd[["published"]][["date-parts"]] else NULL
    yr <- if (is.list(pub_dp) && length(pub_dp) && length(pub_dp[[1]]) &&
              !is.null(pub_dp[[1]][[1]])) {
      as.numeric(.json_int(pub_dp[[1]][[1]], NA_integer_))
    } else NA_real_

    info <- list(
      doi        = .json_chr(bd[["DOI"]]),
      type       = .json_chr(bd[["type"]]),
      title      = .json_chr(bd[["title"]]),
      container  = .json_chr(bd[["container-title"]]),
      year       = yr,
      author     = authors %||% NA_character_,
      volume     = .json_chr(bd[["volume"]]),
      issue      = .json_chr(bd[["issue"]]),
      first_page = first_page,
      last_page  = last_page,
      editor     = bd[["editor"]] %||% NA_character_,
      publisher  = .json_chr(bd[["publisher"]]),
      url        = .json_chr(bd[["URL"]])
    )

    lapply(info, \(i) {
      if (length(i) == 1 & is.atomic(i)) return(i[[1]])
      unlist(i) |> paste(sep = ", ", collapse = "; ")
    })
  }) |> dplyr::bind_rows()

  bib_table
}
