#' Crossref info
#'
#' @param doi the DOI of the paper to get info for
#'
#' @return crossref data
#' @export
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \dontrun{
#'  # cr_info <- crossref(doi)
#' }
crossref <- function(doi) {
  if (!online("api.labs.crossref.org")) {
    message("Crossref is offline")
    return(list())
  }

  if (is_paper(doi) || is_paper_list(doi)) {
    papers <- doi
    doi <- info_table(papers, "doi")$doi
  }

  if (length(doi) > 1) {
    # iterate over DOIs
    crossref_list <- lapply(doi, crossref)
    names(crossref_list) <- doi
    return(crossref_list)
  }

  # check for well-formed DOI
  pattern <- "^10\\.\\d{3,9}\\/[-._;()/:A-Za-z0-9]*[A-Za-z0-9]$"
  if (!grepl(pattern, doi, perl = TRUE)){
    message(doi, " is not a well-formed DOI\\n")
    return(list())
  }

  url <- sprintf("https://api.labs.crossref.org/works/%s?mailto=%s",
                 doi, email())
  j <- jsonlite::read_json(url)

  if (j$status == "ok") {
    return(j$message)
  } else {
    message(j$body$message)
    return(list())
  }
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

ref_info <- function(paper) {
  info <- sapply(paper$bib$doi, \(doi) {
    if (doi != "") {
      openalex(doi)
    } else {
      list()
    }
  })
}

#' Get DOI from Reference
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
#'   doi <- get_doi(ref)
#' }

# Function to get a doi from crossref by sending the full reference text.
get_doi <- function(reference, min_score = 50) {
  if (inherits(reference, "bibentry")) {
    reference <- format(reference)
  } else if (length(reference) > 1) {
    # vectorise
    pb <- pb(length(reference))
    dois <- sapply(reference, \(r) {
      pb$tick()
      get_doi(r, min_score)
    }, USE.NAMES = FALSE)
    return(dois)
  }

  options(crossref_email = email())
  tryCatch({
    res <- rcrossref::cr_works(query = reference, limit = 1)
    if (nrow(res$data) > 0 && as.numeric(res$data$score[1]) > min_score) {
      return(res$data$doi[1])
    } else {
      return(NA_character_)
    }
  }, error = function(e) {
    return(NA_character_)
  })
}
