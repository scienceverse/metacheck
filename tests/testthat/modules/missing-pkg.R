#' List All P-Values (Test version)
#'
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04')
#' and document location in a table.
#'
#' Here are some details...
#'
#' @author Lisa DeBruine
#' @author Daniel Lakens
#'
#' @import notarealpkg
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light
#'
#' @examples
#' module_run(psychsci, "no_error")
pvals2 <- function(paper, ...) {
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
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl
  )
}

helper_func <- function() {
  # testing...
}
