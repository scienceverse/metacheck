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
#' A p-value in a table note ("† p < .10, * p < .05") or a figure legend ("dashed lines indicate non-significant paths (p > .05)") is left out, as in stat_p_exact; the column `not_a_test` says why. A p-value next to words that state a threshold is kept, because such sentences often report a result against that level ("not significant at the conventional threshold (p = .054)").
#'
#' This module only checks p-values reported in the running text of the manuscript. It cannot (yet) process p-values reported only in tables.
#'
#' <validation>Validated on three hand-coded sets of papers (scienceverse/validation: 249 Psychological Science, 250 Economics and 250 MDPI papers). Of 1,783 non-significant p-values in the ground truth, version 0.4.4.3 found 1,719 (3.6% missed), and 94.8% of its 1,814 detections were in the ground truth. Some differences come from the ground truth: it lacks p-values with a correction name ("p Bonf = .061"), and counts partial eta squared written with its subscript after it ("η 2 p = .05"), "p = .05", and some table notes and figure legends ("† p < .10, *p < .05"), which this module leaves out, as non-significant p-values. These figures measure agreement with the hand-coding, which has known errors in both directions (scienceverse/validation#48).</validation>
#'
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @references
#' Appelbaum, M., Cooper, H., Kline, R. B., Mayo-Wilson, E., Nezu, A. M., & Rao, S. M. (2018). Journal article reporting standards for quantitative research in psychology: The APA Publications and Communications Board task force report. American Psychologist, 73(1), 3–25. https://doi.org/10.1037/amp0000191
#'
#' Murphy, S. L., Merz, R., Reimann, L.-E., & Fernández, A. (2025). Nonsignificance misinterpreted as an effect’s absence in psychology: Prevalence and temporal analyses. Royal Society Open Science, 12(3), 242167. https://doi.org/10.1098/rsos.242167
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
stat_p_nonsig <- function(paper) {
  # nothing to check in an empty paper or list: "na", not red or green
  if (.paper_is_empty(paper)) return(.empty_input_result(paper))

  # detailed table of results ----
  table <- extract_p_values(paper)

  # Specify conditions for a significant result
  cond <- !is.na(table$p_value) &
    table$p_value <= 0.05 &
    !is.na(table$p_comp) &
    # "==" is "=" (U127); "p \u2248 0" or "p ~ .01" is significant too (#642)
    table$p_comp %in% c("<", "=", "==", "\u2264", "<=", "=<", "\u2248", "~")

  table$significance <- ifelse(cond, "significant", "nonsignificant")
  table <- subset(table, significance == "nonsignificant")

  # Expand the sentences so the full sentence can be seen
  table <- text_expand(
    table,
    paper,
    expand_to = c("sentence")
  )

  # A p-value in a table note or a figure legend is not a result whose
  # interpretation can be checked ("\u2020 p < .10, * p < .05", "dashed lines
  # indicate non-significant paths (p > .05)"): left out, as stat_p_exact does
  # (.p_not_test(), R/stat_helpers.R; #604, #642). A "threshold" is kept: in
  # 40 that .p_not_test() took for one, 14 were results reported against the
  # level ("significant when the categorical measure was used (p = .003) but
  # not when the continuous measure was used (p = .08)"); markers and legends
  # were results in 3 and 2 of 40.
  table$not_a_test <- .p_not_test(table)
  table <- table[is.na(table$not_a_test) | table$not_a_test == "threshold", , drop = FALSE]

  # summary_table ----
  # must have id column as the id of each paper, one row per paper
  # further columns to be added to a master summary table
  summary_table <- dplyr::summarise(table,
    n_nonsignificant = sum(significance == "nonsignificant", na.rm = TRUE),
    .by = paper_id
  )

  # traffic light ----
  # possible values: na, info, red, yellow, green, fail
  # no nonsignificant p values: nothing to check, so "na", not green
  tl <- if (sum(summary_table$n_nonsignificant) > 0) "yellow" else "na"

  if (tl == "na") {
    report <- "We detected no nonsignificant p values."
    summary_text <- report
  } else {
    summary_text <- sprintf(
      "We found %d non-significant p value%s that should be checked for appropriate interpretation.",
      nrow(table), ifelse(nrow(table) == 1, "", "s")
    )

    explanation <- c(
      "Meta-scientific research has shown nonsignificant p values are commonly misinterpreted. It is incorrect to infer that there is 'no effect', 'no difference', or that groups are 'the same' after p > 0.05.",
      "It is possible that there is a true non-zero effect, but that the study did not detect it. Make sure your inference acknowledges that it is possible that there is a non-zero effect. It is correct to include the effect is 'not significantly' different, although this just restates that p > 0.05.",
      "Metacheck does not yet analyze automatically whether sentences which include non-significant p-values are correct, but we recommend manually checking the sentences below for possible misinterpreted non-significant p values."
    )

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
      scroll_table(report_table, colwidths = c(.1, .9), maxrows = 10),
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
