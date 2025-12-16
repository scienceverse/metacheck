

#' Check whether a DOI resolves
#'
#' Checks the doi.org API to see if a DOI is registered and has an associated URL 
#' (using`https://doi.org/api/handles`). Returns TRUE if it does, FALSE if the DOI
#' does not exist or does not have an associated URL, and NA if the test failed. 
#' Clearly invalid DOIs (i.e. not starting with "10.") will return FALSE without
#' server requests.
#'
#' @param doi Character vector. One or more DOIs to check.
#' @param timeout Numeric. Request timeout in seconds. Default is `10`.
#' @param clean_dois Logical. If `TRUE`, the function will clean the input DOIs
#'   by removing any URL prefixes (like "https://doi.org/") and whitespace.
#' @return Logical vector. For each input DOI, returns TRUE if the DOI resolves,
#'  FALSE if it does not resolve (or does not start with 10.), and NA if the check failed.
#' @examples
#' \dontrun{
#' check_doi_resolves("10.1038/nphys1170") # Expected: TRUE
#' check_doi_resolves("10.1234/invalid.doi") # Expected: FALSE
#' }
#' @export
 
check_doi_resolves <- function(doi, timeout = 10, clean_dois = FALSE) {

    doi <- as.character(doi)

    if (length(doi) > 1) {
        res <- vapply(doi, function(d) check_doi_resolves(d, timeout = timeout), logical(1))
        names(res) <- NULL
        return(res)
    }

  if (is.na(doi) || identical(doi, "") || !nzchar(doi)) return(NA)


  doi <- trimws(doi)

  if (isTRUE(clean_dois)) {
    doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
    doi <- sub("^doi\\s*:\\s*", "", doi, ignore.case = TRUE)
    doi <- trimws(doi)
    if (!nzchar(doi)) return(NA)
  }

  if (!grepl("^10\\.", doi)) return(FALSE)

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

  if (inherits(resp, "error")) return(NA)

  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  code <- body$responseCode
  if (is.null(code) || length(code) != 1L) return(NA)

  # https://www.doi.org/doi-handbook/HTML/rest-api-response-format.html
  if (code == 1L) return(TRUE)   # handle found AND has URL
  if (code == 100L) return(FALSE) # handle not found
  if (code == 2L) return(NA)     # internal error
  if (code == 200L) return(FALSE) # handle exists but no URL of requested type

  NA
}
