#' Crossref info
#'
#' @param doi the DOI of the paper to get info for
#'
#' @return crossref data
#' @export
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \dontrun{
#'   cr_info <- crossref(doi)
#' }
crossref <- function(doi) {
  site_down("api.labs.crossref.org", error = FALSE)

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
                 doi, "debruine@gmail.com")
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
#' \dontrun{
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
                 doi, "debruine@gmail.com")

  info <- tryCatch( suppressWarnings( jsonlite::read_json(url) ),
                 error = function(e) {
                   if (verbose())
                     warning(doi, " not found in OpenAlex", call. = FALSE)
                   return(list(error = doi))
                 })

  # if ("error" %in% names(info) & !is.null(paper)) {
  #   # try title
  #   message("Trying to search OpenAlex by title")
  #   url <- sprintf("https://api.openalex.org/works?filter=title.search:%s&mailto=%s",
  #                  URLencode(paper$info$title), "debruine@gmail.com")
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
  info <- sapply(paper$references$doi, \(doi) {
    if (doi != "") {
      openalex(doi)
    } else {
      list()
    }
  })
}

#' Update Paper Info
#'
#' Check OpenAlex and/or CrossRef for paper info and update the paper object accordingly to fix import problems.
#'
#' @param paper a papercheck paper object or list
#'
#' @returns the updated paper object
#' @export
#'
#' @examples
#' ps1 <- paper_info(psychsci[[1]])
paper_info <- function(paper) {

}
