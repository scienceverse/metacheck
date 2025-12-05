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
  v <- verbose()
  verbose(FALSE)
  stat_table <- metacheck::stats(paper)
  verbose(v)

  # handle no stats
  if (nrow(stat_table) == 0) {
    ret <- list(
      summary_table = data.frame(id = paper$id),
      table = data.frame(),
      traffic_light = "na",
      report = "No detectable statistics. StatCheck currently only detects statistics written in APA format.",
      summary_text = "No detectable statistics"
    )

    return(ret)
  }

  # We only select t-tests and F-tests for now,
  # as statcheck is only validated for these tests.
  stat_filter <- (stat_table$test_type == "t") |
    (stat_table$test_type == "F")
  table <- stat_table[stat_filter, ]

  # no validated stats
  if (nrow(table) == 0) {
    ret <- list(
      summary_table = data.frame(id = paper$id),
      table = data.frame(),
      traffic_light = "na",
      report = "No t-tests or F-tests detected.\n\nThe accuracy of StatCheck has only been validated for *t*-tests and *F*-tests.",
      summary_text = "No t-tests or F-tests detected"
    )

    return(ret)
  }

  # summary output for paperlists ----
  summary_table <- dplyr::summarise(
    table,
    statcheck_found = dplyr::n(),
    statcheck_errors = sum(error, na.rm = TRUE),
    statcheck_decision_errors = sum(decision_error, na.rm = TRUE),
    .by = id
  )

  # determine the traffic light ----
  tl <- "green"
  if (any(table$error)) tl <- "red"

  if (tl == "green") {
    report <- "We detected no errors in t-tests or F-tests."
    summary_text <- report
  } else if (tl == "red") {
    n_errors <- sum(table$error, na.rm = TRUE)
    report_text <- "We detected possible errors in test statistics. Note that as the accuracy of statcheck has only been validated for *t*-tests and *F*-tests. As Metacheck only uses validated modules, we only provide statcheck results for *t* tests and *F*-tests."
    summary_text <- sprintf("%d possible error%s in t-tests or F-tests",
                            n_errors, ifelse(n_errors==1, "", "s"))

    # report_table ----
    # Only show these columns in the HTML view (if they exist)
    wanted <- c("raw", "computed_p", "section", "text")
    cols   <- intersect(wanted, names(table))
    report_table <- table[table$error, cols, drop = FALSE]
    report_table$computed_p <- round(report_table$computed_p, 5)

    # rename the labels for more clarity during checking the table
    label_map <- c(
      raw         = "Text",
      computed_p  = "Recomputed p",
      section     = "Section",
      text        = "Sentence"
    )
    colnames(report_table) <- label_map[cols]

    guidance <- c(
      "For metascientific research on the validity of statcheck, and it's usefulness to prevent statistical reporting errors, see:<br><br>",
      "Nuijten, M. B., van Assen, M. A. L. M., Hartgerink, C. H. J., Epskamp, S., & Wicherts, J. M. (2017). The Validity of the Tool “statcheck” in Discovering Statistical Reporting Inconsistencies. PsyArXiv. doi: [10.31234/osf.io/tcxaja](https://doi.org/10.31234/osf.io/tcxaja)",
      "Nuijten, M. B., & Wicherts, J. (2023). The effectiveness of implementing statcheck in the peer review process to avoid statistical reporting errors. PsyArXiv. doi: [10.31234/osf.io/bxau9](https://doi.org/10.31234/osf.io/bxau9)"
      )

    report <- c(report_text,
                     scroll_table(report_table, colwidths = c("10em", NA, NA, NA)),
                     collapse_section(guidance)) |>
      paste(collapse = "\n\n")
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

