#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.)
#'
#' @author Lisa DeBruine
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "exact_p")
exact_p <- function(paper, ...) {
  # detailed table of results ----
  p <- module_run(paper, "all_p_values")$table
  p$p_comp <- gsub("p-?(value)?\\s*|\\s*\\d?\\.\\d+e?-?\\d*", "",
                   p$text, ignore.case = TRUE)
  p$p_value <- gsub("^p-?(value)?\\s*[<>=≤≥]{1,2}\\s*", "",
                    p$text, ignore.case = TRUE)
  p$p_value <- suppressWarnings(as.numeric(p$p_value))
  p$imprecise <- p$p_comp == "<" & p$p_value > .001
  p$imprecise <- p$imprecise | p$p_comp == ">"
  p$imprecise <- p$imprecise | is.na(p$p_value)
  cols <- c("text", "p_comp", "p_value", "section", "header", "div", "p", "s", "id")
  table <- p[p$imprecise, cols]

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, name = "exact_p")

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(p) == 0 ~ "na",
    any(p$imprecise) ~ "red",
    !all(p$imprecise) ~ "green",
    .default = "yellow"
  )

  # report text for each possible traffic light ----
  report <- c(
    red = "You may have reported some imprecise p-values",
    yellow ="You may have reported some imprecise p-values",
    green = "All p-values were reported with standard precision",
    na = "No p-values were detected"
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
