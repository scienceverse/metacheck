
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
  urls <- paste0("https://api.datacite.org/dois/", cleaned[valid_idx])

  resps <- .batch_query(urls, msg = "Querying DataCite")

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

  dplyr::recode_values(type,
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
    default = type)
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
                           "author",
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

  if (!online("api.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(DOI = doi, error = "offline"))
  }

  ## vectorise with parallel requests ----
  cleaned <- doi_clean(doi)
  valid <- doi_valid_format(cleaned)

  # build requests only for valid DOIs
  valid_idx <- which(valid)
  invalid_idx <- which(!valid)

  if (length(valid_idx) > 0) {
    urls <- sprintf(
      "https://api.labs.crossref.org/works/%s?mailto=%s",
      utils::URLencode(cleaned[valid_idx], reserved = TRUE),
      email()
    )

    resps <- .batch_query(urls, msg = "Querying CrossRef by DOI")

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


#' Parse a CrossRef item into a data frame row
#' @param item a list from CrossRef API response
#' @param select fields to select
#' @returns a data frame
#' @keywords internal
.crossref_parse_item <- function(item, select = c("DOI", "type", "title", "author",
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

  if (length(item$`journal-issue`$`published-print`$`date-parts`) &
      length(item$`journal-issue`$`published-print`$`date-parts`[[1]])) {
    item$year <- item$`journal-issue`$`published-print`$`date-parts`[[1]][[1]]
  } else if (length(item$published$`date-parts`) &
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
  if ("author" %in% select & nrow(authors)) ret$author <- list(authors)

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
                             "year",
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
    # to take advantage of query.title, query.author, query.container-title
    x <- data.frame(
      title = ref$title,
      author =(ref$authors %||% ref$author), # |> paste(collapse = ", "),
      container = ref$container %||% ref$journal %||% ref$booktitle
    )
    # split into a list of 1-row tables (revisit)
    ref <- lapply(seq_along(x$title), \(i) x[i, , drop = FALSE])
  }

  if (!online("api.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(bib_text = ref, DOI = NA, error = "offline"))
  }

  # build requests in parallel
  urls <- lapply(ref, \(r) {
    if (inherits(r, "bibentry") || is.data.frame(r)) {
      title <- utils::URLencode(r$title, reserved = TRUE) |>
        gsub("%28", "(", x = _) |>
        gsub("%29", ")", x = _)
      author <- utils::URLencode(r$author, reserved = TRUE) |>
        gsub("%28", "(", x = _) |>
        gsub("%29", ")", x = _)
      container <- (r$container %||% r$journal %||% r$booktitle) |>
        utils::URLencode(reserved = TRUE) |>
        gsub("%28", "(", x = _) |>
        gsub("%29", ")", x = _)

      url <- sprintf(
        "https://api.crossref.org/works?mailto=%s&rows=%d&sort=score",
        email(), rows
      )
      if (nzchar(title))  url <- sprintf("%s&query.title=%s", url, title)
      if (nzchar(author))  url <- sprintf("%s&query.author=%s", url, author)
      if (nzchar(container))  url <- sprintf("%s&query.container-title=%s", url, container)

      url
    } else {
      query <- utils::URLencode(r, reserved = TRUE) |>
        gsub("%28", "(", x = _) |>
        gsub("%29", ")", x = _)

      sprintf(
        "https://api.crossref.org/works?mailto=%s&rows=%d&sort=score&query.bibliographic=%s",
        email(), rows, query
      )
    }
  })

  # batch to avoid rate limiting
  resps <- .batch_query(urls, msg = "Querying CrossRef")

  table <- lapply(seq_along(ref), \(i) {
    r <- if (is.character(ref[[i]])) {
      data.frame(ref = ref[[i]])
    } else {
      ref[[i]][, ref[[i]]!=""] |>
        paste(collapse = "; \\n") |>
        data.frame(ref = _)
    }
    tryCatch({
      resp <- resps[[i]]
      if (is.null(resp) ||
          inherits(resp, "error") ||
          httr2::resp_status(resp) >= 400) {
        r$DOI <- NA_character_
        r$error  <- "request failed"
        return(r)
      }
      j <- httr2::resp_body_json(resp)
      if (j$status != "ok") {
        r$DOI <- NA_character_
        r$error = j$body$message %||% "unknown"
        return(r)
      }
      x <- .crossref_query_parse(j$message$items, min_score, select)
      x$ref <- r$ref
      x
    }, error = \(e) {
      r$DOI <- NA_character_
      r$error = e$message
      r
    })
  }) |> do.call(dplyr::bind_rows, args = _)

  return(table)
}

#' Parse crossref query items into a table
#' @param items list of items from CrossRef query response
#' @param min_score minimum score threshold
#' @param select fields to select
#' @returns a data frame
#' @keywords internal
.crossref_query_parse <- function(items, min_score, select) {
  scores <- sapply(items, `[[`, "score")
  if (length(items) == 0 || all(scores < min_score)) {
    return(data.frame(DOI = NA_character_))
  }
  items <- items[scores >= min_score]

  parsed_items <- lapply(items, \(item) {
    .crossref_parse_item(item, select)
  })

  table <- do.call(dplyr::bind_rows, parsed_items)
  cols <- intersect(select, names(table))
  table[, cols, drop = FALSE]
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
#' \dontrun{
#' paper <- demopaper()
#' paper$bib_match <- NULL # remove existing
#' paper2 <- add_bib_match(paper)
#' paper2$bib_match
#' }
add_bib_match <- function(paper, min_score = 50) {
  page <- paper_id <- bib_id <- NULL
  bib <- paper_table(paper, "bib")

  if (nrow(bib) == 0) {
    return(paper)
  }

  # look up by doi
  has_doi <- doi_valid_format(bib$doi) %in% TRUE

  # look up papers with DOI
  cr_data_doi <- data.frame()
  if (sum(has_doi) > 0) {
    cr_data_doi <- crossref_doi(bib$doi[has_doi])
    cr_data_doi$paper_id <- bib$paper_id[has_doi]
    cr_data_doi$bib_id <- bib$bib_id[has_doi]
  }
  # look up papers without
  cr_data_no <- data.frame()
  if (sum(!has_doi) > 0) {
    cr_data_no <- crossref_query(bib[!has_doi, ], min_score = min_score)
    cr_data_no$ref <- NULL
    cr_data_no$paper_id <- bib$paper_id[!has_doi]
    cr_data_no$bib_id <- bib$bib_id[!has_doi]
  }

  cr_data <- dplyr::bind_rows(cr_data_doi, cr_data_no)
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
    paper_id         = cr_data$paper_id,
    bib_id           = cr_data$bib_id,
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

  # remove unfound
  bib_match_table <- bib_match[!is.na(bib_match$doi), ] |>
    dplyr::arrange(paper_id, bib_id)

  # add bib_match table to paper object(s)
  if (is_paper(paper)) {
    bib_match_table$paper_id <- NULL
    paper$bib_match <- bib_match_table
  } else if (is_paper_list(paper)) {
    paper <- lapply(paper, \(p) {
      bib_match_i <- bib_match_table[bib_match_table$paper_id == p$paper_id, ]
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

  cleaned <- doi_clean(doi)
  valid <- !is.na(doi) & doi_valid_format(cleaned)

  # build requests only for valid DOIs
  valid_idx <- which(valid)
  invalid_idx <- which(!valid)

  if (length(valid_idx) > 0) {
    urls <- sprintf(
      "https://api.openalex.org/works/https://doi.org/%s?mailto=%s",
      utils::URLencode(cleaned[valid_idx], reserved = TRUE),
      email()
    )

    resps <- .batch_query(urls, msg = "Querying CrossRef by DOI")
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

