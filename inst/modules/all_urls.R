#' List All URLs
#'
#' @description
#' List all the URLs in the main text.
#'
#' @details
#' Checks for valid URLs using a regular expression.
#'
#' @keywords general
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
all_urls <- function(paper) {
  # detailed table of results ----
  table <- extract_urls(paper)

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, name = "urls")

  # determine the traffic light ----
  tl <- if (nrow(table)) "info" else "na"

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl
  )
}
