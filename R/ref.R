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
    res <- vapply(doi, \(d) doi_resolves(d, timeout = timeout), logical(1))
    names(res) <- NULL
    return(res)
  }

  # check DOI is well-formed
  if (is.na(doi) || !nzchar(doi)) {
    return(NA)
  }
  if (!doi_valid_format(doi)) {
    return(FALSE)
  }

  # check doi.org API
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

  # https://www.doi.org/doi-handbook/HTML/rest-api-response-format.html
  if (code == 1L) {
    return(TRUE)
  } # handle found AND has URL
  if (code == 100L) {
    return(FALSE)
  } # handle not found
  if (code == 2L) {
    return(NA)
  } # internal error
  if (code == 200L) {
    return(FALSE)
  } # handle exists but no URL of requested type

  NA
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
    doi <- info_table(papers, "doi")$doi
  }

  if (!online("api.labs.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(DOI = doi, error = "offline"))
  }

  ## vectorise ----
  if (length(doi) > 1) {
    pb <- pb(length(doi),
      format = "Checking DOIs [:bar] :current/:total :elapsedfull"
    )
    table <- lapply(doi, \(d) {
      pb$tick()
      crossref_doi(d)
    }) |>
      do.call(dplyr::bind_rows, args = _)
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
      j <- jsonlite::read_json(url)
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

  # process item
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
    # handle when parts are missing
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
#' - be a text reference or fragment
#' - a bibentry object (authors, title and container will be extracted)
#' - a vector of text or bibentry objects
#' - a paper object (the ref column of the bib table will be extracted)
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
                             "container-title",
                             "published",
                             "volume",
                             "issue",
                             "page",
                             "URL",
                             "abstract"
                           )) {
  if (is_paper(ref)) {
    # pull the whole reference list
    paper <- ref
    ref <- paper$bib$ref
  }

  if (length(ref) == 0) {
    return(data.frame())
  }

  if (inherits(ref, "bibentry")) {
    # TODO: take advantage of query.title, query.author, query.container-title
    title <- ref$title
    author <- ref$author
    container <- ref$journal %||% ref$booktitle

    ref <- paste(author, collapse = ", ") |>
      paste(title, container, sep = "; ")
  } else if (length(ref) > 1 | is.list(ref)) {
    # vectorise
    pb <- pb(length(ref),
      format = "Checking References [:bar] :current/:total :elapsedfull"
    )
    table <- lapply(ref, \(r) {
      pb$tick()
      tryCatch(crossref_query(r, min_score),
        error = \(e) {
          error_tbl <- data.frame(
            ref = ref,
            DOI = NA_character_,
            error = e$message
          )
          return(error_tbl)
        }
      )
    }) |>
      do.call(dplyr::bind_rows, args = _)
    return(table)
  }

  if (!online("api.labs.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(ref = ref, DOI = NA, error = "offline"))
  }

  query <- utils::URLencode(ref, reserved = TRUE) |>
    # fix problems with crossref's Lucene / Solr-style query parser
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
      j <- jsonlite::read_json(url)
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

  scores <- sapply(items, `[[`, "score")
  if (length(items) == 0 || all(scores < min_score)) {
    table <- data.frame(ref = ref, DOI = NA_character_)
    return(table)
  } else {
    items <- items[scores >= min_score]
  }

  # parse the response into a table (works even if cols missing)
  parsed_items <- lapply(items, \(item) {
    # item <- items[[1]] # for testing
    item$title <- item$title[[1]]
    item$`container-title` <- item$`container-title`[[1]]
    item$year <- item$published$`date-parts`[[1]][[1]]
    item$published <- NULL

    authors <- lapply(item$author, \(a) {
      # handle when parts are missing
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
    doi <- info_table(papers, "doi")$doi
  }

  if (!online("api.openalex.org")) {
    message("OpenAlex is offline")
    return(list(DOI = doi, error = "offline"))
  }

  ## vectorise ----
  if (length(doi) > 1) {
    pb <- pb(length(doi),
      format = "Checking DOIs [:bar] :current/:total :elapsedfull"
    )
    oa <- lapply(doi, \(d) {
      pb$tick()
      openalex_doi(d)
    })
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

  info <- tryCatch(suppressWarnings(jsonlite::read_json(url)),
    error = function(e) {
      if (verbose()) {
        warning(doi, " not found in OpenAlex", call. = FALSE)
      }
      return(data.frame(DOI = doi, error = "not found"))
    }
  )
  # convert inverted index to abstract
  if (!is.null(info$abstract_inverted_index)) {
    aii <- info$abstract_inverted_index
    words <- rep(names(aii), sapply(aii, length))
    order <- unname(unlist(aii))
    info$abstract <- paste(words[order(order)], collapse = " ")
  }

  return(info)
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

  j <- tryCatch(jsonlite::read_json(url),
    error = \(e) {
      "error"
    },
    warning = \(w) {
      if (grepl(
        "Couldn't resolve host name",
        w$message
      )) {
        return("offline")
      }
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
          source = bib$journal[[i]],
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
