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
  report_base <- c(
    na    = "No test statistics were detected.",
    red   = "We detected possible errors in test statistics. Note that as the accuracy of statcheck has only been validated for t-tests and F-tests, and as Metacheck only uses validated modules, we do not provide statcheck results for other tests.",
    green = "We detected no errors in test statistics.",
    fail  = "StatCheck failed."
  )
  
  report_text <- report_base[[tl]]
  
  # If there are errors, I added scrollable HTML table to show the details
  if (tl == "red" && nrow(table) > 0) {
    # Only show these columns in the HTML view (if they exist)
    wanted <- c("text", "reported_p", "computed_p", "section")
    cols   <- intersect(wanted, names(table))
    
    if (length(cols) > 0) {
      table_disp <- table[, cols, drop = FALSE]
      
      # I renamed the labels for more clarity during checking the table
      label_map <- c(
        text        = "Sentence / Text",
        reported_p  = "Reported p",
        computed_p  = "Recomputed p",
        section     = "Section"
      )
      colnames(table_disp) <- label_map[cols]
      
      # I styled the header row with a border and light background to differentiate it from the body rows
      header_html <- paste0(
        "<tr>",
        paste(
          sprintf(
            "<th style='border:1px solid #ccc; padding:6px; background-color:#f0f0f0;'>%s</th>",
            colnames(table_disp)
          ),
          collapse = ""
        ),
        "</tr>"
      )
      
      # For the body, I rendered each row with borders and pdding -> easier to scan as the rows will be separated
      body_html <- ""
      if (nrow(table_disp) > 0) {
        body_rows <- apply(table_disp, 1, function(row) {
          paste0(
            "<tr>",
            paste(
              sprintf("<td style='border:1px solid #ccc; padding:6px;'>%s</td>", row),
              collapse = ""
            ),
            "</tr>"
          )
        })
        body_html <- paste(body_rows, collapse = "\n")
      }
      
      # I wrapped headers and rows in a table + smaller font to make the info fit
      html_table <- paste0(
        "<table style='border-collapse:collapse; width:100%; font-size:90%;'>",
        "<thead>", header_html, "</thead>",
        "<tbody>", body_html, "</tbody>",
        "</table>"
      )
      
      # I put the table in a scrollable box 
      scroll_box <- paste0(
        "<br><strong>The following table shows the test statistics with potential errors:</strong>",
        "<div style='border:1px solid #444; padding:10px; ",
        "max-height:450px; overflow-y:auto; background-color:#ffffff; margin-top:8px;'>",
        html_table,
        "</div>"
      )
      
      guidance <- paste0(
        "For metascientific research on the validity of statcheck, and it's usefulness to prevent statistical reporting errors, see:<br><br>",
        "Nuijten, M. B., van Assen, M. A. L. M., Hartgerink, C. H. J., Epskamp, S., & Wicherts, J. M. (2017). The Validity of the Tool “statcheck” in Discovering Statistical Reporting Inconsistencies. PsyArXiv. ",
        "<a href='https://doi.org/10.31234/osf.io/tcxaj' target='_blank'>https://doi.org/10.31234/osf.io/tcxaj</a> <br>",
        "Nuijten, M. B., & Wicherts, J. (2023). The effectiveness of implementing statcheck in the peer review process to avoid statistical reporting errors. PsyArXiv. ",
        "<a href='https://doi.org/10.31234/osf.io/bxau9' target='_blank'>https://doi.org/10.31234/osf.io/bxau9</a> <br>"
        )
      
      report_text <- paste(report_text, scroll_box, "#### To Read ",guidance, sep = "\n\n")
    }
  }
  
  # return a list ----
  list(
    summary      = summary_table,
    na_replace   = 0,
    traffic_light = tl,
    report       = report_text
  )
}

