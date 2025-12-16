#' Preregistration Check
#'
#' @description
#' Retrieve information from preregistrations, make then easier to check.
#'
#' @author Daniel Lakens
#' @author Lisa DeBruine
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
prereg_check <- function(paper, ...) {
  # paper <- psychsci[[218]] # to test

  links_ap <- aspredicted_links(paper)
  table <- aspredicted_retrieve(links_ap, id_col = 1)

  # report
  if (nrow(table) == 0) {
    summary <- "We detected no links to preregistrations."
    summary_text <- "We detected no links to preregistrations."
    report_table <- NA_character_
  } else {
    # Text with green bold count
    summary <- sprintf(
      "We found %d preregistration(s) from AsPredicted. Meta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link(s) below. If you check one aspect of the preregistration, make it the preregistered sample size.",
      nrow(table))
    summary_text <- sprintf(
      "We found %d preregistrations.",
      nrow(table))
    report_table <- scroll_table(links_ap)
    report_table_samplesize <- scroll_table(table$AP_sample_size)

  # summary_table ----
  summary_table <- dplyr::count(table, id, name = "preregistrations", .drop = FALSE)

  # Select columns starting with "AP_"
  prereg_table <- dplyr::select(table, dplyr::starts_with("AP_"))
  # Transpose the selected data
  prereg_table <- t(prereg_table)

  # Convert to data.frame and copy rownames to first column
  prereg_table <- as.data.frame(prereg_table, stringsAsFactors = FALSE)
  prereg_table <- tibble::rownames_to_column(prereg_table, var = "rowname")

  colnames(prereg_table) <- c("Question", "Answer")
  prereg_table <- scroll_table(prereg_table) |>
    collapse_section("Full preregistration")
  }



  guidance <- collapse_section(c(
    "For metascientific work on preregistration deviations</strong>:",
    "van den Akker, O. R., Bakker, M., van Assen, M. A. L. M., Pennington, C. R., Verweij, L., Elsherif, M. M., Claesen, A., Gaillard, S. D. M., Yeung, S. K., Frankenberger, J.-L., Krautter, K., Cockcroft, J. P., Kreuer, K. S., Evans, T. R., Heppel, F. M., Schoch, S. F., Korbmacher, M., Yamada, Y., Albayrak-Aydemir, N., … Wicherts, J. M. (2024). ",
    "The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency. <em>Psychological Methods</em>. ",
    "<a href='https://doi.org/10.1037/met0000687' target='_blank'>https://doi.org/10.1037/met0000687</a>",
    "For educational material on reporting deviations from preregistrations</strong>:",
    "Lakens, D. (2024). When and How to Deviate From a Preregistration. <em>Collabra: Psychology</em>, 10(1), 117094. ",
    "<a href='https://doi.org/10.1525/collabra.117094' target='_blank'>https://doi.org/10.1525/collabra.117094</a>"
  ))


  # Combine into report
  if (nrow(table) == 0) {
    tl <- "na"
    report <- c(summary)
  } else {
    tl <- "info"
    report <- c(summary, "#### Preregistered Sample Size", report_table_samplesize, prereg_table, guidance)
  }


  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
