#' Demo No Error
#'
#' @description
#' Demo description
#'
#' @details
#' Demo details...
#'
#' @keywords results
#'
#' @author Lisa DeBruine (\email{debruine@gmail.com})
#' @author Daniel Lakens
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param demo_arg an example of a passed argument
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light
#'
#' @examples
#' module_run(psychsci, "no_error")
pvals2 <- function(paper, demo_arg = "", ...) {
  # detailed table of results ----
  pattern <- "\\bp-?(value)?\\s*[<>=≤≥]{1,2}\\s*(n\\.?s\\.?|\\d?\\.\\d+)(e-\\d+)?"
  table <- search_text(paper, pattern, return = "match", "perl" = TRUE)

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, name = "p_values")

  # determine the traffic light ----
  tl <- if (nrow(table)) "info" else "na"

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = paste0("summary text", demo_arg),
    report = "report text"
  )
}

helper_func <- function() {
  # testing...
}
