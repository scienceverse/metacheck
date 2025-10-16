#' Module Title
#'
#' @description
#' A short description of the module
#'
#' @author Author Name (\email{name@email.com})
#'
#' @references
#' # Optional reference to include in reports
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "module_name")
module_name <- function(paper, ...) {
  # detailed table of results ----
  pattern <- "significant"
  table <- search_text(paper, pattern)

  # summary output for paperlists ----
  # must have id column as the id of each paper, one row per paper
  # further columns to be added to a master summary table
  summary_table <- dplyr::count(table, id, name = "n_significant")

  # determine the traffic light ----
  # possible values: na, info, red, yellow, green, fail
  tl <- if (nrow(table)) "info" else "na"

  # report text for each possible traffic light ----
  report <- c(
    na = "Not applicable",
    info = "This table is provided for your information",
    red = "This is a potential problem",
    yellow = "There may be a problem",
    green = "No problems found",
    fail = "The check failed, sorry"
  )

  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}
