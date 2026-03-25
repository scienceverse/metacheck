# Functions for references ----

#' Clean DOIs
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

  doi <- trimws(doi)

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
#' \dontrun{
#' doi_resolves("10.1038/nphys1170") # Expected: TRUE
#' doi_resolves("10.1234/invalid.doi") # Expected: FALSE
#' }
doi_resolves <- function(doi, timeout = 10) {
  doi <- doi_clean(doi)

  if (length(doi) > 1) {
    # separate valid/invalid DOIs upfront
    valid_format <- doi_valid_format(doi)
    na_or_empty <- is.na(doi) | !nzchar(doi)
    needs_check <- !na_or_empty & valid_format

    res <- rep(NA, length(doi))
    res[!na_or_empty & !valid_format] <- FALSE

    if (any(needs_check)) {
      check_dois <- doi[needs_check]
      reqs <- lapply(check_dois, \(d) {
        url <- paste0(
          "https://doi.org/api/handles/",
          utils::URLencode(d, reserved = TRUE),
          "?type=URL"
        )
        httr2::request(url) |>
          httr2::req_timeout(timeout) |>
          httr2::req_throttle(rate = 10 / 1) |>
          httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
          httr2::req_error(is_error = \(resp) FALSE)
      })

      resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                            progress = FALSE)

      check_res <- vapply(resps, \(resp) {
        if (inherits(resp, "error")) return(NA)
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

  # single DOI
  if (is.na(doi) || !nzchar(doi)) {
    return(NA)
  }
  if (!doi_valid_format(doi)) {
    return(FALSE)
  }

  url <- paste0(
    "https://doi.org/api/handles/",
    utils::URLencode(doi, reserved = TRUE),
    "?type=URL"
  )

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    return(NA)
  }

  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  code <- body$responseCode
  if (is.null(code) || length(code) != 1L) {
    return(NA)
  }

  if (code == 1L) return(TRUE)
  if (code == 100L) return(FALSE)
  if (code == 2L) return(NA)
  if (code == 200L) return(FALSE)

  NA
}

#' Doi.org Info from DOI
#'
#' @param doi the DOI(s) to get info for
#'
#' @return data frame with DOIs and info
#' @export
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \dontrun{
#' doi_info <- doi_lookup(doi)
#' }
doi_lookup <- function(doi) {
  if (length(doi) == 0) {
    return(data.frame(doi = character(0)))
  }

  # build requests for non-NA DOIs, perform in parallel
  cleaned <- doi_clean(doi)
  is_valid <- !is.na(doi)

  valid_idx <- which(is_valid)
  valid_reqs <- lapply(valid_idx, \(i) {
    paste0("https://doi.org/", cleaned[i]) |>
      httr2::request() |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_throttle(rate = 10 / 1) |>
      httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
      httr2::req_error(is_error = \(resp) FALSE)
  })

  resps <- httr2::req_perform_parallel(valid_reqs, on_error = "continue",
                                        progress = verbose())

  # process responses
  bibdata <- vector("list", length(doi))
  for (i in seq_along(doi)) {
    if (!is_valid[i]) {
      bibdata[[i]] <- list(doi = doi[i])
      next
    }
  }
  for (j in seq_along(valid_idx)) {
    i <- valid_idx[j]
    bibdata[[i]] <- tryCatch({
      resp <- resps[[j]]
      if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
        return(NULL)
      }
      httr2::resp_body_json(resp)
    }, error = \(e) NULL)
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
    authors <- bd$author |>
      sapply(\(a) {
        paste(a$family, a$given, sep = ", ")
      }) |>
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

#' Doi.org Info from DataCite
#'
#' @param doi the DOI(s) to get info for
#'
#' @return bib_match data frame
#' @export
#' @examples
#' doi <- "10.5281/zenodo.2669586"
#' \dontrun{
#' doi_info <- datacite_doi(doi)
#' }
datacite_doi <- function(doi) {
  if (length(doi) == 0) {
    return(data.frame(
      service = character(0),
      title = character(0),
      authors = I(list()),
      doi = character(0)
    ))
  }

  # build requests for non-NA DOIs, perform in parallel
  cleaned <- doi_clean(doi)
  is_valid <- !is.na(doi)

  valid_idx <- which(is_valid)
  valid_reqs <- lapply(valid_idx, \(i) {
    paste0("https://api.datacite.org/dois/", cleaned[i]) |>
      httr2::request() |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_throttle(rate = 10 / 1) |>
      httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
      httr2::req_error(is_error = \(resp) FALSE)
  })

  resps <- httr2::req_perform_parallel(valid_reqs, on_error = "continue",
                                        progress = verbose())

  bibdata <- vector("list", length(doi))
  for (i in seq_along(doi)) {
    if (!is_valid[i]) {
      bibdata[[i]] <- list(doi = doi[i])
    }
  }
  for (j in seq_along(valid_idx)) {
    i <- valid_idx[j]
    bibdata[[i]] <- tryCatch({
      resp <- resps[[j]]
      if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
        return(NULL)
      }
      ct <- httr2::resp_content_type(resp)
      if (ct == "application/json") {
        httr2::resp_body_json(resp)
      } else {
        NULL
      }
    }, error = \(e) NULL)
  }

  bib_table <- lapply(bibdata, \(bd) {
    att <- bd$data$attributes

    # authors
    authors <- lapply(att$creators, \(a) {
      if (is.null(a) || length(a) == 0) {
        data.frame(given = character(0), family = character(0))
      } else {
        data.frame(
          given = a$givenName %||% NA_character_,
          family = a$familyName %||% NA_character_
        )
      }
    }) |> dplyr::bind_rows()

    info <- list(
      service    = "datacite",
      service_id = bd$data$id %||% NA_character_,
      score      = NA_real_,
      doi        = att$doi %||% NA_character_,
      bib_type   = att$types$bibtex %||% NA_character_,
      title      = unlist(att$titles)[[1]] %||% NA_character_,
      authors    = NA,
      container  = unlist(att$container)[[1]] %||% NA_character_,
      publisher  = att[["publisher"]] %||% NA_character_,
      year       = att$publicationYear %||% NA_real_,
      date       = unlist(att$dates)[[1]] %||% NA_character_,
      url        = att[["url"]] %||% NA_character_,
      version    = att[["version"]] %||% NA_character_
    )

    info <- lapply(info, \(i) {
      if (length(i) == 1 & is.atomic(i)) return(i[[1]])
      unlist(i) |> paste(sep = ", ", collapse = "; ")
    })

    info$authors <- list(authors)
    info
  }) |> dplyr::bind_rows()

  bib_table
}

#' Convert crossref/doi types to bibtex types
#'
#' @param type a vector of crossref types
#'
#' @returns a vector of bibtext types
#' @export
#' @keywords internal
#'
#' @examples
#' crossref_types <- c("book-part",
#'                     "journal-article",
#'                     "monograph",
#'                     NA,
#'                     "unmatched-type")
#' bibtype_convert(crossref_types)
bibtype_convert <- function(type) {
  if (is.null(type)) return(NULL)

  # required <- list(
  #   "article" = c("author", "title", "journal", "year"),
  #   "book" = c("title", "publisher", "year"),  # plus author OR editor
  #   "booklet" = c("title"),
  #   "conference" = c("author", "title", "booktitle", "year"),
  #   "inbook" = c("title", "publisher", "year"),  # plus author OR editor; and chapter OR pages
  #   "incollection" = c("author", "title", "booktitle", "publisher", "year"),
  #   "inproceedings" = c("author", "title", "booktitle", "year"),
  #   "manual" = c("title"),
  #   "mastersthesis" = c("author", "title", "school", "year"),
  #   "misc" = character(0),
  #   "phdthesis" = c("author", "title", "school", "year"),
  #   "proceedings" = c("title", "year"),
  #   "techreport" = c("author", "title", "institution", "year"),
  #   "unpublished" = c("author", "title", "note")
  # )

  dplyr::case_match(type,
    "journal-article"        ~ "article",
    "book"                   ~ "book",
    "book-chapter"           ~ "incollection",
    "book-part"              ~ "inbook",
    "book-section"           ~ "inbook",
    "book-series"            ~ "book",
    "edited-book"            ~ "book",
    "reference-book"         ~ "book",
    "monograph"              ~ "book",
    "report"                 ~ "techreport",
    "proceedings-article"    ~ "inproceedings",
    "proceedings"            ~ "proceedings",
    "conference-paper"       ~ "inproceedings",
    "conference-proceeding"  ~ "proceedings",
    "posted-content"         ~ "misc",
    "dissertation"           ~ "phdthesis",
    "thesis"                 ~ "phdthesis",
    "dataset"                ~ "misc",
    "standard"               ~ "misc",
    "reference-entry"        ~ "incollection",
    "reference-work"         ~ "book",
    "report-series"          ~ "techreport",
    "other"                  ~ "misc",
    .default = type)
}

# CrossRef Functions ----

#' CrossRef Info from DOI
#'
#' Valid selects for crossref API are:
#'
#' abstract, URL, resource, member, posted, score, created, degree, update-policy, short-title, license, ISSN, container-title, issued, update-to, issue, prefix, approved, indexed, article-number, clinical-trial-number, accepted, author, group-title, DOI, is-referenced-by-count, updated-by, event, chair, standards-body, original-title, funder, translator, published, archive, published-print, alternative-id, subject, subtitle, published-online, publisher-location, content-domain, reference, title, link, type, publisher, volume, references-count, ISBN, issn-type, assertion, deposited, page, content-created, short-container-title, relation, editor
#'
#' @param doi the DOI of the paper to get info for
#' @param select what fields to select from the crossref API
#'
#' @return data frame with DOIs and info
#' @export
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \dontrun{
#' # cr_info <- crossref_doi(doi)
#' }
crossref_doi <- function(doi, select = c(
                           "DOI",
                           "type",
                           "title",
                           "container-title",
                           "volume",
                           "issue",
                           "page",
                           "URL",
                           "abstract",
                           "year",
                           "error"
                         )) {
  if (length(doi) == 0) {
    return(data.frame())
  } else if (all(is.na(doi))) {
    return(data.frame(DOI = doi))
  }

  if (is_paper(doi) || is_paper_list(doi)) {
    papers <- doi
    doi <- paper_table(papers, "info", "doi")$doi
  }

  if (!online("api.labs.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(DOI = doi, error = "offline"))
  }

  ## vectorise with parallel requests ----
  if (length(doi) > 1) {
    cleaned <- doi_clean(doi)
    valid <- doi_valid_format(cleaned)

    # build requests only for valid DOIs
    valid_idx <- which(valid)
    invalid_idx <- which(!valid)

    if (length(valid_idx) > 0) {
      reqs <- lapply(valid_idx, \(i) {
        url <- sprintf(
          "https://api.labs.crossref.org/works/%s?mailto=%s",
          utils::URLencode(cleaned[i], reserved = TRUE),
          email()
        )
        httr2::request(url) |>
          httr2::req_headers(Accept = "application/json") |>
          httr2::req_throttle(rate = 30 / 1) |>
          httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
          httr2::req_error(is_error = \(resp) FALSE)
      })

      resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                            progress = verbose())

      valid_results <- lapply(seq_along(valid_idx), \(j) {
        tryCatch({
          resp <- resps[[j]]
          if (inherits(resp, "error")) {
            return(data.frame(DOI = doi[valid_idx[j]], error = "connection error"))
          }
          if (httr2::resp_status(resp) >= 400) {
            return(data.frame(DOI = doi[valid_idx[j]], error = paste("HTTP", httr2::resp_status(resp))))
          }
          item <- httr2::resp_body_json(resp)
          if (item$status != "ok") {
            return(data.frame(DOI = doi[valid_idx[j]], error = item$body$`message-type` %||% "unknown"))
          }
          .crossref_parse_item(item$message, select)
        }, error = \(e) {
          data.frame(DOI = doi[valid_idx[j]], error = e$message)
        })
      })
    } else {
      valid_results <- list()
    }

    invalid_results <- lapply(invalid_idx, \(i) {
      data.frame(DOI = doi[i], error = "malformed")
    })

    # combine in original order
    all_results <- vector("list", length(doi))
    for (j in seq_along(valid_idx)) all_results[[valid_idx[j]]] <- valid_results[[j]]
    for (j in seq_along(invalid_idx)) all_results[[invalid_idx[j]]] <- invalid_results[[j]]
    # handle all-NA DOIs
    na_idx <- which(is.na(doi))
    for (i in na_idx) all_results[[i]] <- data.frame(DOI = NA_character_)

    table <- do.call(dplyr::bind_rows, all_results)
    return(table)
  }

  ## single DOI checks ----
  doi <- doi_clean(doi)

  # check for well-formed DOI
  if (!doi_valid_format(doi)) {
    message(doi, " is not a well-formed DOI\\n")
    return(data.frame(DOI = doi, error = "malformed"))
  }

  url <- sprintf(
    "https://api.labs.crossref.org/works/%s?mailto=%s",
    utils::URLencode(doi, reserved = TRUE),
    email()
  )

  item <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_headers(Accept = "application/json") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
      j <- httr2::resp_body_json(resp)
      if (j$status != "ok") {
        stop(j$body$`message-type`)
      }
      j$message
    },
    error = function(e) {
      return(list(DOI = doi, error = e$message))
    },
    warning = function(w) {
      return(list(DOI = doi, error = w$message))
    }
  )

  .crossref_parse_item(item, select)
}

#' Parse a CrossRef item into a data frame row
#' @param item a list from CrossRef API response
#' @param select fields to select
#' @returns a data frame
#' @keywords internal
.crossref_parse_item <- function(item, select = c("DOI", "type", "title",
                                                    "container-title", "volume",
                                                    "issue", "page", "URL",
                                                    "abstract", "year", "error")) {
  if (length(item$title)) {
    item$title <- item$title[[1]]
  } else {
    item$title <- NULL
  }

  if (length(item$`container-title`)) {
    item$`container-title` <- item$`container-title`[[1]]
  } else {
    item$`container-title` <- NULL
  }

  if (length(item$published$`date-parts`) &
    length(item$published$`date-parts`[[1]])) {
    item$year <- item$published$`date-parts`[[1]][[1]]
  }
  item$published <- NULL

  authors <- lapply(item$author, \(a) {
    cols <- c("given", "family", "ORCID")
    suba <- a[cols]
    names(suba) <- cols
    suba
  }) |> do.call(dplyr::bind_rows, args = _)
  item$author <- NULL

  to_select <- intersect(select, names(item))

  ret <- data.frame(item[to_select], check.names = FALSE)
  if (nrow(authors)) ret$author <- list(authors)

  return(ret)
}


#' Look up Reference in CrossRef
#'
#' @details
#' The argument `ref` can take many formats.  Crossref queries only look for authors, title, and container-title (e.g., journal or book), but extra information doesn't seem to hurt.
#'
#' - a text reference or fragment
#' - a bibentry object (authors, title and container will be extracted)
#' - a vector of text or bibentry objects
#' - a paper object (the bib table will be extracted)
#'
#' Valid selects for this route are: abstract, URL, resource, member, posted, score, created, degree, update-policy, short-title, license, ISSN, container-title, issued, update-to, issue, prefix, approved, indexed, article-number, clinical-trial-number, accepted, author, group-title, DOI, is-referenced-by-count, updated-by, event, chair, standards-body, original-title, funder, translator, published, archive, published-print, alternative-id, subject, subtitle, published-online, publisher-location, content-domain, reference, title, link, type, publisher, volume, references-count, ISBN, issn-type, assertion, deposited, page, content-created, short-container-title, relation, editor
#'
#' @param ref the full text reference of the paper to get info for, see Details
#' @param min_score minimal score that is taken to be a reliable match (default 50)
#' @param rows the maximum number of rows to return per reference (default 1)
#' @param select what fields to select from the crossref API
#'
#' @return doi
#' @export
#' @examples
#' ref <- paste(
#'   "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024).",
#'   "The benefits of preregistration and Registered Reports.",
#'   "Evidence-Based Toxicology, 2(1)."
#' )
#' \donttest{
#' cr <- crossref_query(ref)
#' }
crossref_query <- function(ref, min_score = 50, rows = 1,
                           select = c(
                             "DOI",
                             "score",
                             "type",
                             "title",
                             "author",
                             "editor",
                             "publisher",
                             "container-title",
                             "published",
                             "volume",
                             "issue",
                             "page",
                             "URL"
                           )) {
  if (is_paper(ref)) {
    # pull the whole reference list
    paper <- ref
    ref <- paper$bib
  }

  if (length(ref) == 0) {
    return(data.frame())
  }

  if (inherits(ref, "bibentry") || is.data.frame(ref)) {
    # TODO: take advantage of query.title, query.author, query.container-title
    title <- ref$title
    author <- format_bib_authors(ref$authors) %||% ref$author
    container <- ref$container %||% ref$journal %||% ref$booktitle

    ref <- paste(title, author, container, sep = "; ")
  }

  if (length(ref) > 1 | is.list(ref)) {
    if (!online("api.crossref.org")) {
      message("Crossref is offline")
      return(data.frame(bib_text = ref, DOI = NA, error = "offline"))
    }

    # build requests in parallel
    select_str <- c(select, "score") |> unique() |> paste(collapse = ",")
    reqs <- lapply(ref, \(r) {
      if (inherits(r, "bibentry") || is.data.frame(r)) {
        title <- r$title
        author <- r$author
        container <- r$container %||% r$journal %||% r$booktitle
        r <- paste(author, collapse = ", ") |>
          paste(title, container, sep = "; ")
      }
      query <- utils::URLencode(r, reserved = TRUE) |>
        gsub("%28", "(", x = _) |>
        gsub("%29", ")", x = _)
      url <- sprintf(
        "https://api.crossref.org/works?mailto=%s&rows=%d&sort=score&select=%s&query.bibliographic=%s",
        email(), rows, select_str, query
      )
      httr2::request(url) |>
        httr2::req_headers(Accept = "application/json") |>
        httr2::req_throttle(rate = 30 / 1) |>
        httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
        httr2::req_error(is_error = \(resp) FALSE)
    })

    resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                          progress = verbose())

    table <- lapply(seq_along(ref), \(i) {
      r <- if (is.character(ref[[i]])) ref[[i]] else paste(ref[[i]])
      tryCatch({
        resp <- resps[[i]]
        if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
          return(data.frame(ref = r, DOI = NA_character_, error = "request failed"))
        }
        j <- httr2::resp_body_json(resp)
        if (j$status != "ok") {
          return(data.frame(ref = r, error = j$body$message %||% "unknown"))
        }
        .crossref_query_parse(j$message$items, r, min_score, select)
      }, error = \(e) {
        data.frame(ref = r, DOI = NA_character_, error = e$message)
      })
    }) |> do.call(dplyr::bind_rows, args = _)
    return(table)
  }

  if (!online("api.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(bib_text = ref, DOI = NA, error = "offline"))
  }

  query <- utils::URLencode(ref, reserved = TRUE) |>
    gsub("%28", "(", x = _) |>
    gsub("%29", ")", x = _)

  url <- sprintf(
    "https://api.crossref.org/works?mailto=%s&rows=%d&sort=score&select=%s&query.bibliographic=%s",
    email(),
    rows,
    c(select, "score") |> unique() |> paste(collapse = ","),
    query
  )

  items <- tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_headers(Accept = "application/json") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
      j <- httr2::resp_body_json(resp)
      if (j$status != "ok") {
        stop(j$body$message)
      }
      j$message$items
    },
    error = function(e) {
      return(data.frame(ref = ref, error = e$message))
    },
    warning = function(w) {
      message(w$message)
      return(data.frame(ref = ref, error = w$message))
    }
  )

  .crossref_query_parse(items, ref, min_score, select)
}

#' Parse crossref query items into a table
#' @param items list of items from CrossRef query response
#' @param ref the original reference string
#' @param min_score minimum score threshold
#' @param select fields to select
#' @returns a data frame
#' @keywords internal
.crossref_query_parse <- function(items, ref, min_score, select) {
  scores <- sapply(items, `[[`, "score")
  if (length(items) == 0 || all(scores < min_score)) {
    return(data.frame(ref = ref, DOI = NA_character_))
  }
  items <- items[scores >= min_score]

  parsed_items <- lapply(items, \(item) {
    item$title <- item$title[[1]]
    item$`container-title` <- item$`container-title`[[1]]
    item$year <- item$published$`date-parts`[[1]][[1]]
    item$published <- NULL

    authors <- lapply(item$author, \(a) {
      cols <- c("given", "family", "ORCID")
      suba <- a[cols]
      names(suba) <- cols
      suba
    }) |> do.call(dplyr::bind_rows, args = _)
    item$author <- NULL

    ret <- data.frame(item, check.names = FALSE)
    ret$author <- list(authors)
    ret
  })

  table <- do.call(dplyr::bind_rows, parsed_items)
  table$ref <- ref
  rows <- table$score >= min_score
  cols <- c("ref", select, "year") |>
    intersect(names(table))
  table[rows, cols]
}


#' Match table from bib table
#'
#' @param paper a paper or paperlist object
#' @param min_score minimal score that is taken to be a reliable match
#'
#' @returns the paper or paperlist with bib_match table added
#' @export
#'
#' @examples
#' dontrun{
#' paper <- demopaper()
#' paper$bib$match <- NULL # remove existing
#' paper2 <- add_bib_match(paper)
#' paper2$bib$match$crossref
#' }
add_bib_match <- function(paper, min_score = 50) {
  bib <- paper_table(paper, "bib")

  if (nrow(bib) == 0) {
    return(paper)
  }

  # get search string, deduplicate, and look up
  refs <- paste(
    bib$title,
    format_bib_authors(bib$authors),
    bib$container,
    sep = "; "
  )
  cr_data <- crossref_query(unique(refs), min_score = min_score)
  if ("page" %in% names(cr_data)) {
    cr_data <- tidyr::separate(
      cr_data, page,
      c("first_page", "last_page"),
      sep = "-", extra = "merge", fill = "right"
    )
  }

  # convert CrossRef author list to list of given/family data.frames
  if (is.null(cr_data$author)) {
    authors <- replicate(nrow(cr_data),
      data.frame(given = character(0), family = character(0)),
      simplify = FALSE
    )
  } else {
    authors <- lapply(cr_data$author, \(a) {
      if (is.null(a) || nrow(a) == 0) {
        data.frame(given = character(0), family = character(0))
      } else {
        data.frame(
          given = a$given %||% NA_character_,
          family = a$family %||% NA_character_
        )
      }
    })
  }

  bib_match <- data.frame(
    ref              = unique(refs),
    service          = "crossref",
    service_id       = NA_character_,
    score            = cr_data$score %||% NA_real_,
    bib_type         = bibtype_convert(cr_data$type) %||% NA_character_,
    doi              = cr_data$DOI %||% NA_character_,
    title            = cr_data$title %||% NA_character_,
    authors          = NA,
    editors          = NA,
    publisher        = cr_data$publisher %||% NA_character_,
    year             = cr_data$year %||% NA_integer_,
    date             = NA_character_,
    container        = cr_data$`container-title` %||% NA_character_,
    volume           = cr_data$volume %||% NA_character_,
    issue            = cr_data$issue %||% NA_character_,
    first_page       = cr_data$first_page %||% NA_character_,
    last_page        = cr_data$last_page %||% NA_character_,
    edition          = NA_character_,
    version          = NA_character_,
    url              = cr_data$URL %||% NA_character_
  )
  bib_match$authors <- authors
  bib_match$editors <- replicate(nrow(bib_match),
    data.frame(given = character(0), family = character(0)),
    simplify = FALSE
  )

  # re-duplicate and add IDs
  bib_match_table <- data.frame(
    paper_id = bib$paper_id,
    bib_id = bib$bib_id,
    ref = refs
  ) |>
    dplyr::left_join(bib_match, by = "ref")
  bib_match_table$ref <- NULL
  bib_match_table <- bib_match_table[!is.na(bib_match_table$score), ]

  # add bib_match table to paper object(s)
  if (is_paper(paper)) {
    bib_match_table$paper_id <- NULL
    paper$bib_match <- bib_match_table
  } else if (is_paper_list(paper)) {
    paper <- lapply(paper, \(p) {
      bib_match_i <- bib_match[bib_match$paper_id == p$paper_id, ]
      bib_match_i$paper_id <- NULL
      p$bib_match <- bib_match_i
      p
    }) |> paperlist()
  }

  paper
}


# OpenAlex functions ----

#' OpenAlex info from DOI
#'
#' See details for a list of root-level fields that can be selected.
#'
#' See <https://docs.openalex.org/api-entities/works/work-object> for explanations of the information you can retrieve about works.
#'
#' Root-level fields for the select argument:
#'
#' * id
#' * doi
#' * title
#' * display_name
#' * publication_year
#' * publication_date
#' * ids
#' * language
#' * primary_location
#' * type
#' * type_crossref
#' * indexed_in
#' * open_access
#' * authorships
#' * institution_assertions
#' * countries_distinct_count
#' * institutions_distinct_count
#' * corresponding_author_ids
#' * corresponding_institution_ids
#' * apc_list
#' * apc_paid
#' * fwci
#' * has_fulltext
#' * fulltext_origin
#' * cited_by_count
#' * citation_normalized_percentile
#' * cited_by_percentile_year
#' * biblio
#' * is_retracted
#' * is_paratext
#' * primary_topic
#' * topics
#' * keywords
#' * concepts
#' * mesh
#' * locations_count
#' * locations
#' * best_oa_location
#' * sustainable_development_goals
#' * grants
#' * datasets
#' * versions
#' * referenced_works_count
#' * referenced_works
#' * related_works
#' * abstract_inverted_index
#' * abstract_inverted_index_v3
#' * cited_by_api_url
#' * counts_by_year
#' * updated_date
#' * created_date
#'
#' @param doi the DOI of the paper to get info for
#' @param select a vector of fields to return, NULL returns all
#'
#' @return list with DOIs and info
#' @export
#'
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \donttest{
#' oa_info <- openalex_doi(doi)
#' oa_info <- openalex_doi(doi, "title")
#' }
openalex_doi <- function(doi, select = NULL) {
  # handle papers, paperlists, and vectors of multiple dois
  if (length(doi) == 0) {
    return(list())
  } else if (all(is.na(doi))) {
    return(list(DOI = doi))
  }

  if (is_paper(doi) || is_paper_list(doi)) {
    papers <- doi
    doi <- paper_table(papers, "info", "doi")$doi
  }

  if (!online("api.openalex.org")) {
    message("OpenAlex is offline")
    return(list(DOI = doi, error = "offline"))
  }

  ## vectorise with parallel requests ----
  if (length(doi) > 1) {
    cleaned <- doi_clean(doi)
    valid <- !is.na(doi) & doi_valid_format(cleaned)
    valid_idx <- which(valid)

    if (length(valid_idx) > 0) {
      reqs <- lapply(valid_idx, \(i) {
        url <- sprintf(
          "https://api.openalex.org/works/https://doi.org/%s?mailto=%s",
          cleaned[i], email()
        )
        httr2::request(url) |>
          httr2::req_headers(Accept = "application/json") |>
          httr2::req_throttle(rate = 10 / 1) |>
          httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
          httr2::req_error(is_error = \(resp) FALSE)
      })

      resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                            progress = verbose())
    }

    oa <- vector("list", length(doi))
    for (i in seq_along(doi)) {
      if (is.na(doi[i])) {
        oa[[i]] <- list(DOI = doi[i])
      } else if (!valid[i]) {
        oa[[i]] <- list(DOI = doi[i], error = "malformed")
      }
    }
    for (j in seq_along(valid_idx)) {
      i <- valid_idx[j]
      oa[[i]] <- tryCatch({
        resp <- resps[[j]]
        if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
          return(list(DOI = doi[i], error = "not found"))
        }
        info <- httr2::resp_body_json(resp)
        .openalex_add_abstract(info)
      }, error = \(e) list(DOI = doi[i], error = "not found"))
    }
    return(oa)
  }

  ## single DOI checks ----
  doi <- doi_clean(doi)

  if (!doi_valid_format(doi)) {
    message(doi, " is not a well-formed DOI\\n")
    return(list(DOI = doi, error = "malformed"))
  }

  url <- sprintf(
    "https://api.openalex.org/works/https://doi.org/%s?mailto=%s",
    doi, email()
  )

  info <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)
  },
    error = function(e) {
      if (verbose()) {
        warning(doi, " not found in OpenAlex", call. = FALSE)
      }
      return(data.frame(DOI = doi, error = "not found"))
    }
  )

  .openalex_add_abstract(info)
}

#' Add abstract from inverted index
#' @param info OpenAlex response list
#' @returns the info list with abstract field added
#' @keywords internal
.openalex_add_abstract <- function(info) {
  if (!is.null(info$abstract_inverted_index)) {
    aii <- info$abstract_inverted_index
    words <- rep(names(aii), sapply(aii, length))
    order <- unname(unlist(aii))
    info$abstract <- paste(words[order(order)], collapse = " ")
  }
  info
}


#' Look up a reference in OpenAlex
#'
#' @param title The title of the work
#' @param source The source (journal or book)
#' @param authors The authors
#' @param strict Whether to return NULL or the best match if there isn't a single match
#'
#' @returns A data frame with citation info
#' @export
#'
#' @examples
#' \dontrun{
#' openalex_query("Sample Size Justification", "Collabra Psychology")
#' }
openalex_query <- function(title, source = NA, authors = NA, strict = TRUE) {
  relevance_score <- title_match <- source_match <- NULL

  if (is.null(email())) {
    stop("You need to set an email with email('your@address.org') to use OpenAlex")
  }

  fields <- c(
    "id",
    "doi",
    "relevance_score",
    "display_name",
    "publication_year",
    "primary_location",
    "authorships",
    "type",
    "biblio"
  ) |>
    paste0(collapse = ",")

  url <- paste0(
    "https://api.openalex.org/works?filter=title.search:",
    utils::URLencode(gsub(",", "", title)),
    "&mailto=", email(),
    "&select=", fields
  )

  j <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)
  },
    error = \(e) {
      if (grepl("Couldn't resolve host name|Could not resolve host",
                e$message)) {
        return("offline")
      }
      "error"
    }
  )

  if (is.character(j)) {
    if (j == "offline") {
      message("OpenAlex is offline")
      return(NULL)
    } else if (j == "error") {
      message("Error querying OpenAlex")
      return(NULL)
    }
  }

  if (is.null(j$results) || length(j$results) == 0) {
    if (grepl(":", title)) {
      # try partial title match
      maintitle <- strsplit(title, ":", TRUE)[[1]][[1]]
      bib <- openalex_query(maintitle, source, authors, strict)
      return(bib)
    } else {
      message("No results from OpenAlex")
      return(NULL)
    }
  }

  info <- lapply(j$results, \(res) {
    res$source <- res$primary_location$source$display_name
    res$primary_location <- NULL
    res$authors <- res$authorships |>
      sapply(\(a) a$raw_author_name) |>
      paste(collapse = "; ")
    res$authorships <- NULL
    res <- unlist(res)
  }) |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::arrange(dplyr::desc(relevance_score))

  required_cols <- c("display_name", "source")
  for (rq in required_cols) {
    if (!rq %in% names(info)) info[[rq]] <- ""
  }

  info$title_match <- tolower(info$display_name) == tolower(title)
  info$source_match <- tolower(info$source) == tolower(source)
  # TODO: fuzzy match authors

  matches <- dplyr::filter(info, title_match, source_match)

  if (nrow(matches) == 1) {
    message("1 title/source match")
    return(matches)
  } else if (nrow(matches) > 1) {
    message("multiple title/source matches")
    if (strict) {
      return(NULL)
    }
    return(matches[1, ])
  }

  matches <- dplyr::filter(info, title_match)
  if (nrow(matches) == 1) {
    message("matches title, not source")
    if (strict) {
      return(NULL)
    }
    return(matches[1, ])
  } else if (nrow(matches) > 1) {
    message("multiple title matches, no source match")
    if (strict) {
      return(NULL)
    }
    return(matches[1, ])
  }

  message("no title/journal exact matches")
  if (strict) {
    return(NULL)
  }
  return(info[1, ])
}


#' Add DOIs to a bib file
#'
#' Uses OpenAlex to search for items that match the title and journal of bibtex entries that don't have a DOI and adds them in.
#'
#' @param bibfile The file path to the .bib file
#' @param save_to The file to save the results to; if NULL, saves to bibfile name with _doi appended
#' @param strict Should there be a single exact match for title and journal, if FALSE, gives the best match
#'
#' @returns a bib table in the bib2df format
#' @export
#' @keywords internal
bibtex_add_dois <- function(bibfile,
                            save_to = NULL,
                            strict = TRUE) {
  if (is.null(save_to)) {
    save_to <- sub("\\.bib$", "_doi.bib", bibfile)
  }

  df <- suppressMessages(
    suppressWarnings(
      bib2df::bib2df(bibfile)
    )
  )

  if (!"DOI" %in% names(df)) df$DOI <- NA_character_

  old_doi <- df$DOI

  msgs <- character()

  df$DOI <- sapply(seq_along(df$TITLE), \(i) {
    # message(i, ": ", substr(df$TITLE[[i]], 1, 60), "...")

    if (!is.na(df$DOI[i])) {
      msgs[[i]] <<- "DOI exists"
      return(df$DOI[[i]])
    }
    if (df$CATEGORY[i] != "ARTICLE") {
      msgs[[i]] <<- "not an article"
      return(df$DOI[[i]])
    }

    withCallingHandlers(
      expr = {
        info <- openalex_query(
          title = df$TITLE[[i]],
          source = df$JOURNAL[[i]],
          authors = df$AUTHOR[[i]],
          strict = strict
        )
      },
      message = function(m) {
        msgs[[i]] <<- conditionMessage(m) |>
          sub("^.{5}", "", x = _) |>
          sub(".{6}$", "", x = _)
        invokeRestart("muffleMessage") # prevents immediate printing
      }
    )

    if (is.null(info)) {
      return(NA_character_)
    }

    shortdoi <- sub("https://", "", info$doi, fixed = TRUE) |>
      sub("doi.org/", "", x = _, fixed = TRUE)

    return(shortdoi)
  })

  attr(df, "msgs") <- msgs

  new_doi <- df$DOI

  dois_added <- sum(!is.na(new_doi)) - sum(!is.na(old_doi))

  message(dois_added, " new DOIs added")

  bib2df::df2bib(df, save_to, )

  invisible(df)
}


#' Add DOIs to bibliography
#'
#' @param bib the bib table
#' @param strict Should there be a single exact match for title and journal, if FALSE, gives the best match
#'
#' @returns the bib table with updated DOIs
#' @export
#' @keywords internal
bib_add_dois <- function(bib, strict = TRUE) {
  old_doi <- bib$doi

  msgs <- character()

  ## set up progress bar ----
  pb <- pb(
    length(bib$doi),
    "Processing bibentry [:bar] :current/:total :elapsedfull"
  )

  bib$doi <- sapply(seq_along(bib$doi), \(i) {
    # message(i, ": ", substr(df$TITLE[[i]], 1, 60), "...")
    pb$tick()

    if (!is.na(bib$doi[i])) {
      msgs[[i]] <<- "DOI exists"
      return(bib$doi[[i]])
    }

    withCallingHandlers(
      expr = {
        info <- openalex_query(
          title = bib$title[[i]],
          source = bib$container[[i]],
          authors = bib$authors[[i]],
          strict = strict
        )
      },
      message = function(m) {
        msgs[[i]] <<- conditionMessage(m) |>
          sub("^.{5}", "", x = _) |>
          sub(".{6}$", "", x = _)
        invokeRestart("muffleMessage") # prevents immediate printing
      }
    )

    if (is.null(info) || !"doi" %in% names(info)) {
      return(NA_character_)
    }

    shortdoi <- sub("https://", "", info$doi, fixed = TRUE) |>
      sub("doi.org/", "", x = _, fixed = TRUE)

    return(shortdoi)
  })

  new_doi <- bib$doi

  dois_added <- sum(!is.na(new_doi)) - sum(!is.na(old_doi))

  message(dois_added, " new DOIs added")

  return(bib)
}
