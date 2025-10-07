#' StatCheck
#'
#' @description
#' Check consistency of p-values and test statistics
#'
#' @references
#' Nuijten M, Epskamp S (2024). _statcheck: Extract Statistics from Articles and
#' Recompute P-Values_. R package version 1.5.0,
#' <https://CRAN.R-project.org/package=statcheck>.
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci[[10]], "statcheck")
statcheck <- function(paper) {
  # detailed table of results ----
  stat_table <- papercheck::stats(paper)
  table <- stat_table[stat_table$error, ]

  # summary output for paperlists ----
  if (nrow(stat_table) > 0 && "id" %in% names(stat_table)) {
    summary_table <- dplyr::summarise(stat_table,
                                      stats_found = dplyr::n(),
                                      stats_error = sum(error),
                                      decision_error = sum(decision_error),
                                      .by = id)
  } else {
    summary_table <- NULL
  }

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(stat_table) == 0 ~ "na",
    all(stat_table$error == "FALSE") ~ "green",
    .default = "red"
  )

  # report text for each possible traffic light ----
  report <- c(
    na = "No test statistics were detected",
    red = "We detected possible errors in test statistics",
    green = "We detected no errors in test statistics",
    fail = "StatCheck failed"
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
