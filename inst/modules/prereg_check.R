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

  # table ----
  links_ap <- aspredicted_links(paper)
  table <- aspredicted_retrieve(links_ap, id_col = 1)

  # summary_table ----
  summary_table <- dplyr::count(table, id, name = "preregistrations", .drop = FALSE)

  # tl & summary_text & report ----
  if (nrow(table) == 0) {
    tl <- "na"
    summary_text <- "We detected no links to preregistrations."
    report <- summary_text
  } else {
    tl <- "info"

    summary_text <- sprintf(
      "We found %d preregistration%s.",
      nrow(table), plural(nrow(table))
    )

    ## set up report
    summary <- sprintf(
      "We found %d preregistration%s from AsPredicted. Meta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link%s below. If you check one aspect of the preregistration, make it the preregistered sample size.",
      nrow(table), plural(nrow(table)), plural(nrow(table)))

    report_table_samplesize <- scroll_table(table$AP_sample_size)

    # Select columns starting with "AP_"
    ap_cols <- grepl("^AP_", colnames(table))
    prereg_table <- data.frame(
      Question = colnames(table)[ap_cols],
      Answer = t(table[, ap_cols])
    ) |> scroll_table() |>
      collapse_section("Full preregistration")

    guidance <- c(
      "**For metascientific work on preregistration deviations**:",
      "van den Akker, O. R. et al. (2024). The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency. *Psychological Methods*. DOI: [10.1037/met0000687](https://doi.org/10.1037/met0000687)",
      "**For educational material on reporting deviations from preregistrations**:",
      "Lakens, D. (2024). When and How to Deviate From a Preregistration. *Collabra: Psychology*, 10(1), 117094. DOI: [10.1525/collabra.117094](https://doi.org/10.1525/collabra.117094)"
    )
    report <- c(summary,
                "#### Preregistered Sample Size",
                report_table_samplesize,
                prereg_table,
                collapse_section(guidance)
    )
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
