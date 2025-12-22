# Valid selects for crossref API are:
# abstract, URL, resource, member, posted, score, created, degree, update-policy, short-title, license, ISSN, container-title, issued, update-to, issue, prefix, approved, indexed, article-number, clinical-trial-number, accepted, author, group-title, DOI, is-referenced-by-count, updated-by, event, chair, standards-body, original-title, funder, translator, published, archive, published-print, alternative-id, subject, subtitle, published-online, publisher-location, content-domain, reference, title, link, type, publisher, volume, references-count, ISBN, issn-type, assertion, deposited, page, content-created, short-container-title, relation, editor"

#' CrossRef Info from DOI
#'
#' @param doi the DOI of the paper to get info for
#'
#' @return crossref data
#' @export
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \dontrun{
#'  # cr_info <- crossref_doi(doi)
#' }
crossref_doi <- function(doi) {
  if (length(doi) == 0) {
    return(data.frame())
  } else if (all(is.na(doi))) {
    return(data.frame(DOI = doi))
  }

  if (is_paper(doi) || is_paper_list(doi)) {
    papers <- doi
    doi <- info_table(papers, "doi")$doi
  }

  if (length(doi) > 1) {
    # vectorise
    pb <- pb(length(doi),
             format = "Checking DOIs [:bar] :current/:total :elapsedfull")
    table <- lapply(doi, \(d) {
      pb$tick()
      crossref_doi(d)
    }) |>
      do.call(dplyr::bind_rows, args = _)
    return(table)
  }

  # check for well-formed DOI
  pattern <- "^10\\.\\d{3,9}\\/[-._;()/:A-Za-z0-9]*[A-Za-z0-9]$"
  if (!grepl(pattern, doi, perl = TRUE)){
    message(doi, " is not a well-formed DOI\\n")
    return(data.frame(DOI = doi, error = "malformed"))
  }

  if (!online("api.labs.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(DOI = doi, error = "offline"))
  }

  url <- sprintf("https://api.labs.crossref.org/works/%s?mailto=%s",
                 utils::URLencode(doi, reserved = TRUE),
                 email())

  item <- tryCatch({
    j <- jsonlite::read_json(url)
    if (j$status != "ok") { stop(j$body$`message-type`) }
    j$message
  }, error = function(e) {
    return(list(DOI = doi, error = e$message))
  }, warning = function(w) {
    return(list(DOI = doi, error = w$message))
  })

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

  select <- c(
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
  ) |> intersect(names(item))

  ret <- data.frame(item[select], check.names = FALSE)
  if (nrow(authors)) ret$author <- list(authors)

  return(ret)
}



#' Look up Reference in CrossRef
#'
#' @param reference the full text reference of the paper to get info for
#' @param min_score minimal score that is taken to be a reliable match (default 50)
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
#'   cr <- crossref_query(ref)
#' }
crossref_query <- function(ref, min_score = 50, rows = 1) {
  if (length(ref) == 0) {
    return(data.frame())
  }

  if (inherits(ref, "bibentry")) {
    ref <- format(ref)
  } else if (length(ref) > 1 | is.list(ref)) {
    # vectorise
    pb <- pb(length(ref),
             format = "Checking References [:bar] :current/:total :elapsedfull")
    table <- lapply(ref, \(r) {
      pb$tick()
      crossref_query(r, min_score)
    }) |>
      do.call(dplyr::bind_rows, args = _)
    return(table)
  }

  if (!online("api.labs.crossref.org")) {
    message("Crossref is offline")
    return(data.frame(ref = ref, DOI = NA, error = "offline"))
  }


  select <- c(
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
  )

  query <- utils::URLencode(ref, reserved = TRUE)

  url <- sprintf("https://api.crossref.org/works?mailto=%s&rows=%d&sort=score&select=%s&query.bibliographic=%s",
                 email(),
                 rows,
                 paste(select, collapse = ","),
                 query)


  items <- tryCatch({
    j <- jsonlite::read_json(url)
    if (j$status != "ok") { stop(j$body$message) }
    j$message$items
  }, error = function(e) {
    return(data.frame(ref = ref, error = e$message))
  }, warning = function(w) {
    message(w$message)
    return(data.frame(ref = ref, error = w$message))
  })

  scores <- sapply(items, `[[`, "score")
  if (length(items) == 0 || all(scores < min_score)) {
    table <- data.frame(ref = ref, DOI = NA_character_)
    return(table)
  } else {
    items <- items[scores >= min_score]
  }

  # parse the response into a table
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
  cols <- intersect(select, names(table)) |> c("ref", x = _, "year")
  table[rows, cols]
}


#' Get OpenAlex info for a paper
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
#' @return a list of values
#' @export
#'
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \donttest{
#'   oa_info <- openalex(doi)
#' }
openalex <- function(doi, select = NULL) {
  # handle papers, paperlists, and vectors of multiple dois
  if (is_paper(doi)) {
    paper <- doi
    doi <- paper$info$doi
  } else if (is_paper_list(doi) || length(doi) > 1) {
    info <- lapply(doi, openalex)
    return(info)
  }

  url <- sprintf("https://api.openalex.org/works/https://doi.org/%s?mailto=%s",
                 doi, email())

  info <- tryCatch( suppressWarnings( jsonlite::read_json(url) ),
                    error = function(e) {
                      if (verbose())
                        warning(doi, " not found in OpenAlex", call. = FALSE)
                      return(list(error = doi))
                    })

  if (!is.null(info$abstract_inverted_index)) {
    # convert inverted index to abstract
    aii <- info$abstract_inverted_index
    words <- rep(names(aii), sapply(aii, length))
    order <- unname(unlist(aii))
    info$abstract <- paste(words[order(order)], collapse = " ")
  }

  # if ("error" %in% names(info) & !is.null(paper)) {
  #   # try title
  #   message("Trying to search OpenAlex by title")
  #   url <- sprintf("https://api.openalex.org/works?filter=title.search:%s&mailto=%s",
  #                  URLencode(paper$info$title), email())
  #   res <- tryCatch( suppressWarnings( jsonlite::read_json(url) ),
  #                     error = function(e) {
  #                       if (verbose())
  #                         warning(doi, " not found in OpenAlex", call. = FALSE)
  #                       return(list(error = doi))
  #                     })
  #
  #   if (res$meta$count == 1) {
  #
  #   }
  # }

  return(info)
}
