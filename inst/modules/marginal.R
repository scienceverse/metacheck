#' Marginal Significance
#'
#' @description
#' List all sentences that describe an effect as 'marginally significant'.
#'
#' @author Daniel Lakens
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "marginal")
marginal <- function(paper) {
  # detailed table of results ----
  pattern <- "margin\\w* (?:\\w+\\s+){0,5}significan\\w*|trend\\w* (?:\\w+\\s+){0,1}significan\\w*|almost (?:\\w+\\s+){0,2}significan\\w*|approach\\w* (?:\\w+\\s+){0,2}significan\\w*|border\\w* (?:\\w+\\s+){0,2}significan\\w*|close to (?:\\w+\\s+){0,2}significan\\w*"
  table <- search_text(paper, pattern)

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, name = "marginal")

  # determine the traffic light ----
  tl <- ifelse(nrow(table), "red", "green")

  report = c(
    red = "You described effects as marginally/borderline/close to significant. It is better to write 'did not reach the threshold alpha for significance'.",
    green = "No effects were described as marginally/borderline/close to significant."
  )

  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}
