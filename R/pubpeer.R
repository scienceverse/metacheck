#' Get Pubpeer Comments
#'
#' Takes a DOI, and retrieves information from pubpeer related to post-publication peer review comments.
#'
#' @param doi a vector of paper DOIs
#'
#' @returns a dataframe with information from pubpeer
#' @export
#'
#' @examples
#' doi <- c(
#'   "10.1038/s41598-025-24662-9",
#'   "10.1177/0146167211398138"
#' )
#' pubpeer_comments(doi)
pubpeer_comments <- function(doi) {
  url <- "https://pubpeer.com/v3/publications?devkey=PubPeerZotero"
  doi_na <- doi[!is.na(doi)]
  body <- list(dois = tolower(doi_na)) |>
    jsonlite::toJSON(auto_unbox = TRUE)

  response <- httr::POST(
    url,
    body = body,
    encode = "raw",
    httr::add_headers(`Content-Type` = "application/json;charset=UTF-8")
  )
  if (httr::status_code(response) == 200) {
    data <- httr::content(response, as = "parsed", type = "application/json")
    pp_fb <- lapply(data$feedbacks, \(fb) {
      list(
        doi = fb$id,
        total_comments = fb$total_comments,
        url = fb$url,
        users = trimws(fb$users)
      )
    }) |>
      do.call(dplyr::bind_rows, args = _)

    if (nrow(pp_fb) == 0) {
      all <- data.frame(
        doi = tolower(doi),
        total_comments = 0,
        url = NA_character_,
        users = NA_character_
      )
    } else {
      all <- data.frame(doi = tolower(doi)) |>
        dplyr::left_join(pp_fb, by = "doi")
      all$doi <- doi
      all$total_comments[is.na(all$total_comments)] <- 0
    }

    return(all)
  } else {
    return(NULL) # Request failed
  }
}
