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
statcheck <- function(paper, ...) {
  # detailed table of results ----
  stat_table <- metacheck::stats(paper)
  # We only select t-tests and F-tests for now, as statcheck is only validated for these tests.
  stat_table <- stat_table[(stat_table$test_type == "t")|(stat_table$test_type == "F"),]

  table <- stat_table[stat_table$error, ]

  # summary output for paperlists ----
  if (nrow(stat_table) > 0 && "id" %in% names(stat_table)) {
    summary_table <- dplyr::summarise(
      stat_table,
      stats_found    = dplyr::n(),
      stats_error    = sum(error, na.rm = TRUE),
      decision_error = sum(decision_error, na.rm = TRUE),
      .by = id
    )
  } else {
    summary_table <- NULL
  }

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(stat_table) == 0 ~ "na",
    all(!stat_table$error) ~ "green",
    .default = "red"
  )

  # base report text for each possible traffic light ----
  summary_text <- c(
    na    = "No test statistics were detected.",
    red   = "We detected possible errors in test statistics. Note that as the accuracy of statcheck has only been validated for *t*-tests and *F*-tests. As Metacheck only uses validated modules, we only provide statcheck results for *t* tests and *F*-tests",
    green = "We detected no errors in test statistics.",
    fail  = "StatCheck failed."
  )

  report_text <- summary_text[[tl]]

  # If there are errors, I added scrollable HTML table to show the details
  if (tl == "red" && nrow(table) > 0) {
    # Only show these columns in the HTML view (if they exist)
    wanted <- c("text", "reported_p", "computed_p", "section")
    cols   <- intersect(wanted, names(table))

    if (length(cols) > 0) {
      report_table <- table[, cols, drop = FALSE]

      summary_text <- sprintf("%d possible errors in test statistics",
                              nrow(report_table))

      # I renamed the labels for more clarity during checking the table
      label_map <- c(
        text        = "Sentence / Text",
        reported_p  = "Reported p",
        computed_p  = "Recomputed p",
        section     = "Section"
      )
      colnames(report_table) <- label_map[cols]

      guidance <- c(
        "For metascientific research on the validity of statcheck, and it's usefulness to prevent statistical reporting errors, see:<br><br>",
        "Nuijten, M. B., van Assen, M. A. L. M., Hartgerink, C. H. J., Epskamp, S., & Wicherts, J. M. (2017). The Validity of the Tool “statcheck” in Discovering Statistical Reporting Inconsistencies. PsyArXiv. doi: [10.31234/osf.io/tcxaja](https://doi.org/10.31234/osf.io/tcxaja)",
        "Nuijten, M. B., & Wicherts, J. (2023). The effectiveness of implementing statcheck in the peer review process to avoid statistical reporting errors. PsyArXiv. doi: [10.31234/osf.io/bxau9](https://doi.org/10.31234/osf.io/bxau9)"
        )

      report_text <- c(report_text,
                       scroll_table(report_table),
                       collapse_section(guidance)) |>
        paste(collapse = "\n\n")
    }
  }

  # return a list ----
  list(
    summary_table = summary_table,
    table = table,
    na_replace = 0,
    traffic_light = tl,
    report = report_text,
    summary_text = summary_text
  )
}

