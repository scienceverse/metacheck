#' Non-Significant P Value Check
#'
#' @description
#' This module checks for imprecisely reported p values. If p > .05 is detected, it warns for misinterpretations.
#'
#' @details
#' The nonsignificant p-value check searches for regular expressions that match a predefined pattern. The module identifies all p-values in a manuscript and selects those that are not reported to be smaller than or equal to 0.05. It returns all sentences containing non-significant p-values.
#'
#' In the future, the Metacheck team aims to incorporate a machine learning classifier to only return sentences likely to contain misinterpretations. If you want to help to improve the module, reach out to the Metacheck development team.
#'
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @references
#' # Appelbaum, M., Cooper, H., Kline, R. B., Mayo-Wilson, E., Nezu, A. M., & Rao, S. M. (2018). Journal article reporting standards for quantitative research in psychology: The APA Publications and Communications Board task force report. American Psychologist, 73(1), 3–25. https://doi.org/10.1037/amp0000191
#' # Murphy, S. L., Merz, R., Reimann, L.-E., & Fernández, A. (2025). Nonsignificance misinterpreted as an effect’s absence in psychology: Prevalence and temporal analyses. Royal Society Open Science, 12(3), 242167. https://doi.org/10.1098/rsos.242167
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
stat_p_nonsig <- function(paper) {
  # detailed table of results ----
  table <- extract_p_values(paper)

  # Specify conditions for a significant result
  cond <- !is.na(table$p_value) &
    table$p_value <= 0.05 &
    !is.na(table$p_comp) &
    table$p_comp %in% c("<", "=")

  table$significance <- ifelse(cond, "significant", "nonsignificant")
  table <- subset(table, significance == "nonsignificant")

  # Expand the sentences so the full sentence can be seen
  table <- expand_text(
    table,
    paper,
    expand_to = c("sentence")
  )

  # summary_table ----
  # must have id column as the id of each paper, one row per paper
  # further columns to be added to a master summary table
  summary_table <- dplyr::summarise(table,
    n_significant = sum(significance == "significant", na.rm = TRUE),
    n_nonsignificant = sum(significance == "nonsignificant", na.rm = TRUE),
    .by = id
  )

  # traffic light ----
  # possible values: na, info, red, yellow, green, fail
  tl <- if (sum(summary_table$n_nonsignificant) > 0) "yellow" else "green"

  if (tl == "green") {
    report <- "We detected no nonsignificant p values."
    summary_text <- report
  } else {
    summary_text <- sprintf(
      "We found %d non-significant p value%s that should be checked for appropriate interpretation.",
      nrow(table), ifelse(nrow(table)==1, "", "s")
    )

    explanation <- c("Meta-scientific research has shown nonsignificant p values are commonly misinterpreted. It is incorrect to infer that there is 'no effect', 'no difference', or that groups are 'the same' after p > 0.05.",
    "It is possible that there is a true non-zero effect, but that the study did not detect it. Make sure your inference acknowledges that it is possible that there is a non-zero effect. It is correct to include the effect is 'not significantly' different, although this just restates that p > 0.05.",
    "Metacheck does not yet analyze automatically whether sentences which include non-significant p-values are correct, but we recommend manually checking the sentences below for possible misinterpreted non-significant p values.")

    guidance <- c(
      "For metascientific articles demonstrating the rate of misinterpretations of non-significant results is high, see:",
      format_ref(aczel2018),
      format_ref(murphy2025),
      "For educational material on preventing the misinterpretation of p values, see [Improving Your Statistical Inferences](https://lakens.github.io/statistical_inferences/01-pvalue.html#sec-misconception1)."
    )

    cols <- c("text", "expanded")
    report_table <- table[, cols]
    colnames(report_table) <- c("Text", "Sentence")

    # report text
    report <- c(
      explanation,
      scroll_table(report_table, colwidths = c(.1, .9)),
      collapse_section(guidance)
    ) |> paste(collapse = "\n\n")
  }

  # return a list ----
  list(
    summary_table = summary_table,
    table = table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}


aczel2018 <- bibentry(
  bibtype = "Article",
  title = "Quantifying Support for the Null Hypothesis in Psychology: An Empirical Investigation",
  author = c(
    person("B.", "Aczel"),
    person("B.", "Palfi"),
    person("A.", "Szollosi"),
    person("M.", "Kovacs"),
    person("B.", "Szaszi"),
    person("P.", "Szecsi"),
    person("M.", "Zrubka"),
    person("Q. F.", "Gronau"),
    person("D.", "van den Bergh"),
    person("E.-J.", "Wagenmakers")
  ),
  journal = "Advances in Methods and Practices in Psychological Science",
  year = 2018,
  volume = 1,
  number = 3,
  pages = "357--366",
  doi = "10.1177/2515245918773742"
)

murphy2025 <- bibentry(
  bibtype = "Article",
  title = "Nonsignificance misinterpreted as an effect’s absence in psychology: Prevalence and temporal analyses",
  author = c(
    person("S. L.", "Murphy"),
    person("R.", "Merz"),
    person("L.-E.", "Reimann"),
    person("A.", "Fernández")
  ),
  journal = "Royal Society Open Science",
  year = 2025,
  volume = 12,
  number = 3,
  pages = "242167",
  doi = "10.1098/rsos.242167"
)
