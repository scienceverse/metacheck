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

  if (inherits(doi, "scivrs_paper") ||
      inherits(doi[[1]], "scivrs_paper")) {
    papers <- doi
    doi <- info_table(papers, "doi")$doi
  }

  if (length(doi > 1)) {
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

#' Get OpenAlex info
#'
#' @param doi the DOI of the paper to get info for
#'
#' @return a list of values
#' @export
#'
#' @examples
#' doi <- "10.7717/peerj.4375"
#' \dontrun{
#'   oa_info <- openalex(doi)
#' }
openalex <- function(doi) {
  url <- sprintf("https://api.openalex.org/works/https://doi.org/%s?mailto=%s",
                 doi, "debruine@gmail.com")
  j <- tryCatch( suppressWarnings( jsonlite::read_json(url) ),
                 error = function(e) {
                   return(list())
                 })

  return(j)
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
