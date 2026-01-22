#' List All P-Values
#'
#' @description
#' List all p-values in the text, returning the matched text (e.g., 'p = 0.04') and document location in a table.
#'
#' @details
#' Note that this will not catch p-values reported like "the p-value is 0.03" because that results in a ton of false positives when papers discuss p-value thresholds. If you need to detect text like that, use `search_text()` function and a custom pattern like "\\bp(-| )?values?\\s+.{1,20}\\s+[0-9\\.]+"
#'
#' This will catch most comparators like =<>~≈≠≤≥≪≫ and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact the author.
#'
#' @keywords results
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
all_p_values <- function(paper) {
  # is its own function now
  p <- extract_p_values(paper)

  # summary_table ----
  summary_table <- dplyr::count(p, id, name = "p_values")

  # traffic_light ----
  tl <- if (nrow(p)) "info" else "na"

  # summary_text ----
  summary_text <- sprintf("We found %d p-value%s",
                    nrow(p),
                    ifelse(nrow(p) == 1, "", "s"))

  # report ----
  cols <- c("text", "expanded")
  report_table <- expand_text(p, paper)[, cols]
  names(report_table) <- c("Text", "Sentence")
  report_table_code <- scroll_table(report_table, colwidths = c("5em", NA))
  explan <- "This will catch most comparators like =<>~≈≠≤≥≪≫ and most versions of scientific notation like 5.0 x 10^-2 or 5.0e-2. If you find any formats that are not correctly handled by this function, please contact debruine@gmail.com."
  report <- c(report_table_code, explan)

  # return a list ----
  list(
    table = p,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
