#' Chained (Test version)
#'
#' Use the list of all p-values if it exists
#'
#' @keywords general
#'
#' @author Lisa DeBruine
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light
#'
#' @examples
#' psychsci[[10]] |>
#'   module_run("all_p_values") |>
#'   module_run("chained")
chained <- function(paper, ...) {
  p <- get_prev_outputs("all_p_values", "table")
  if (!is.null(p)) {
    table <- p[1:2, 1:2]
  } else {
    #p <- module_run(paper, "all_p_values")
    table <- data.frame(a = "not from prev")
  }

  # return a list ----
  list(
    table = table,
    summary_text = "chained summary text",
    report = "chained report text"
  )
}

