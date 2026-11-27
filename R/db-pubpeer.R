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
  # I(): auto_unbox sent a single DOI as a JSON string, not an array (U14)
  body <- list(dois = I(tolower(doi_na))) |>
    jsonlite::toJSON(auto_unbox = TRUE)

  # PubPeer is asked up to 3 times, 5 and 10 s apart, when it does not answer
  # or answers with a busy or server error: a single failed request made
  # ref_pubpeer fail although PubPeer answered the same DOIs normally later
  # (psyarxiv_metacheck#6). When no answer comes at all, NULL is returned, as
  # for an error answer, instead of stopping with the connection error.
  resp <- tryCatch(
    .mc_request(url) |>
      httr2::req_headers(`Content-Type` = "application/json;charset=UTF-8") |>
      httr2::req_body_raw(body, type = "application/json;charset=UTF-8") |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                       is_transient = \(resp) httr2::resp_status(resp) %in%
                         c(408, 429, 500, 502, 503, 504),
                       backoff = \(i) 5 * i) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  if (!is.null(resp) && httr2::resp_status(resp) == 200) {
    data <- httr2::resp_body_json(resp)
    # feedback without an id cannot be matched to a DOI; when none had one,
    # the table had no doi column and the join below stopped (U14)
    feedbacks <- Filter(\(fb) length(fb$id) == 1, data$feedbacks)
    pp_fb <- lapply(feedbacks, \(fb) {
      list(
        doi = fb$id,
        total_comments = fb$total_comments,
        url = fb$url,
        users = paste(trimws(fb$users), collapse = ", ")
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
      # a DOI reported twice keeps its first feedback (no duplicated rows)
      pp_fb <- pp_fb[!duplicated(pp_fb$doi), , drop = FALSE]
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
