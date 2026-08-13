#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.), reported as exactly zero (e.g., p = .000), or reported as negative (e.g., p = -.22).
#'
#' @details
#' This module uses regular expressions to identify p-values. It will flag any values reported as p > ? or p < numbers greater than .001. It will also flag p-values reported as exactly zero (e.g., p = .000, p = 0.00), which are mathematically impossible — p-values are never exactly zero and should instead be reported as p < .001.
#'
#' Negative p-values (e.g., p = -.22) are searched for with a separate pattern and reported separately, because they are impossible values rather than imprecise ones. They are usually a sign that a neighbouring statistic was accidentally repeated in place of the p-value. Such values are not counted among the detected p-values, and are not seen by the other p-value checks or by StatCheck, so they are only reported when at least one is found.
#'
#' We try to exclude figure and table notes like "* p < .05", but may not succeed at excluding all false positives.
#'
#' This module only checks p-values reported in the running text of the manuscript. It cannot (yet) process p-values reported only in tables (as opposed to a table's footnote text, which the module does see and tries to exclude — see above).
#'
#' <validation>In a sample of 225 papers containing 405 instances of non-exact p-values, the module correctly detected 269 cases (true positives) and incorrectly identified 78 (false positives). It missed 136 instances of imprecisely reported p-values (false negatives) and correctly identified 4557 cases of precisely reported p-values (true negative). Additionally, 78% of positive detections were correct (positive predictive value).</validation>
#'
#'
#' @keywords results
#'
#' @author  Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk}) and Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
stat_p_exact <- function(paper) {
  # table ----
  p <- extract_p_values(paper)

  # Expand the sentences so the full sentence can be seen
  p <- expand_text(
    p,
    paper,
    expand_to = c("sentence")
  )

  # Flag imprecise p-values
  p$imprecise <- p$p_comp == "<" & p$p_value > .001
  p$imprecise <- p$imprecise | !p$p_comp %in% c("=", "<")
  p$imprecise <- p$imprecise | is.na(p$p_value)

  # remove false positive "*p < .05"
  star_pattern <- "\\*\\s*p\\s*<\\s*0?\\.0+[15]"
  stars <- grepl(star_pattern, p$expanded)
  p$imprecise <- p$imprecise & !stars

  # Flag p-values reported as exactly zero (e.g., p = .000, p = 0.00)
  p$zero <- p$p_comp == "=" & !is.na(p$p_value) & p$p_value == 0

  # Negative p-values (e.g., p = -.22) are searched for separately, because
  # extract_p_values() does not match a leading minus sign, so they never
  # appear in the table above. They are impossible values rather than
  # imprecise ones, so they are reported on their own and not added to p.
  # The character class covers the hyphen-minus, the Unicode minus and the
  # en-dash, all of which occur in text extracted from PDFs.
  # A preceding letter is excluded so that correlations whose subscript was
  # flattened during PDF extraction (e.g. "r p = -.64", a Pearson r) are not
  # mistaken for a negative p-value.
  negative_pattern <- paste0(
    "(?<![^A-Za-z][A-Za-z]\\s)(?<!^[A-Za-z]\\s)", # not "r p", but allow "The p"
    "\\bp(-?value)?\\s*", # ways to write p
    "[=<>~≈≠≤≥≪≫]{1,2}\\s*", # 1-2 operators
    "[-−–]\\s*", # a negative sign
    "\\d?\\.\\d+" # the number
  )
  negative <- text_search(paper, negative_pattern,
    return = "match",
    perl = TRUE, ignore.case = FALSE
  )
  if (nrow(negative) > 0) {
    negative <- expand_text(negative, paper, expand_to = c("sentence"))
  } else {
    # expand_text() is not run on an empty table, so add the column it would
    # have created, to keep the column selection below valid
    negative$expanded <- character(0)
  }

  # One row per reported value. These tables are deliberately not passed
  # through unique(): a single sentence can contain two separate problems
  # written identically (e.g. "indirect effect = 0.13, p < .01, direct effect
  # = -0.18, p < .01"), and removing duplicates would hide the second one and
  # disagree with the counts in the summary table.
  cols <- c("text", "expanded")
  report_table <- p[which(p$imprecise), cols, drop = FALSE]
  colnames(report_table) <- c("P-Value", "Text")

  zero_table <- p[which(p$zero), cols, drop = FALSE]
  colnames(zero_table) <- c("P-Value", "Text")

  negative_table <- negative[, cols, drop = FALSE]
  colnames(negative_table) <- c("P-Value", "Text")

  # summary_table ----
  imprecise_summary <- dplyr::count(p[which(p$imprecise), , drop = FALSE], paper_id, name = "n_imprecise")
  zero_summary <- dplyr::count(p[which(p$zero), , drop = FALSE], paper_id, name = "n_zero")
  negative_summary <- dplyr::count(negative, paper_id, name = "n_negative")
  summary_table <- dplyr::full_join(imprecise_summary, zero_summary, by = "paper_id") |>
    dplyr::full_join(negative_summary, by = "paper_id")

  # traffic light ----
  if (nrow(p) == 0 && nrow(negative_table) == 0) {
    tl <- "na"
  } else if (nrow(report_table) == 0 && nrow(zero_table) == 0 &&
    nrow(negative_table) == 0) {
    tl <- "green"
  } else {
    tl <- "red"
  }

  # report / summary_text ----
  if (tl == "na") {
    report <- "We detected no *p* values."
    summary_text <- report
  } else if (tl == "green") {
    report <- sprintf(
      "We found no imprecise, negative, or exactly zero *p*-values out of %d detected.",
      nrow(p)
    )
    summary_text <- report
  } else {
    summary_parts <- c()
    if (nrow(report_table) > 0) {
      summary_parts <- c(summary_parts, sprintf(
        "%d imprecise *p* value%s",
        nrow(report_table),
        plural(nrow(report_table))
      ))
    }
    if (nrow(zero_table) > 0) {
      summary_parts <- c(summary_parts, sprintf(
        "%d *p* value%s reported as exactly zero",
        nrow(zero_table),
        plural(nrow(zero_table))
      ))
    }
    # negative p-values are not part of the detected p-value table, so they
    # are reported separately from the "out of N detected" count
    if (length(summary_parts) > 0) {
      summary_text <- sprintf(
        "We found %s out of %d detected *p* value%s.",
        paste(summary_parts, collapse = " and "),
        nrow(p),
        plural(nrow(p))
      )
    } else {
      summary_text <- character(0)
    }
    if (nrow(negative_table) > 0) {
      summary_text <- paste(c(summary_text, sprintf(
        "%s found %d negative *p* value%s, which cannot be correct.",
        if (length(summary_text) > 0) "We also" else "We",
        nrow(negative_table),
        plural(nrow(negative_table))
      )), collapse = " ")
    }

    # Guidance text
    apa <- bibentry(
      bibtype = "Book",
      author = person("American Psychological Association"),
      year = 2020,
      title = "Publication manual of the American Psychological Association",
      edition = "7",
      subtitle = "The official guide to APA style",
      publisher = "American Psychological Association"
    )

    report <- c()

    if (nrow(report_table) > 0) {
      report_text <- "Reporting *p* values imprecisely (e.g., *p* < .05) reduces transparency, reproducibility, and re-use (e.g., in *p* value meta-analyses). Best practice is to report exact p-values with three decimal places (e.g., *p* = .032) unless *p* values are smaller than 0.001, in which case you can use *p* < .001."

      guidance <- c(
        "The APA manual states: Report exact *p* values (e.g., *p* = .031) to two or three decimal places. However, report *p* values less than .001 as *p* < .001. However, 2 decimals is too imprecise for many use-cases (e.g., a *p* value meta-analysis), so report *p* values with three digits.",
        format_ref(apa)
      )

      report <- c(
        report,
        report_text,
        scroll_table(report_table, colwidths = c(.1, .9)),
        collapse_section(guidance)
      )
    }

    if (nrow(zero_table) > 0) {
      zero_text <- "*P* values are never exactly zero. A *p* value of .000 is a rounding artifact — the actual value is simply smaller than the reported precision. Very small *p* values should be reported as *p* < .001 rather than *p* = .000."

      zero_guidance <- c(
        "The APA manual states: report *p* values less than .001 as *p* < .001.",
        format_ref(apa)
      )

      report <- c(
        report,
        zero_text,
        scroll_table(zero_table, colwidths = c(.1, .9)),
        collapse_section(zero_guidance)
      )
    }

    if (nrow(negative_table) > 0) {
      negative_text <- "*P* values cannot be negative, because they are probabilities, and so must lie between 0 and 1. A negative *p* value is a reporting mistake. It often happens when a neighbouring statistic is accidentally repeated in place of the *p* value, so that the minus sign of a test statistic or effect size is carried over (for example, reporting *t*(198) = -2.22, *p* = -.22)."

      negative_guidance <- c(
        "Check the original analysis output and correct the reported value. Because of the minus sign, this *p* value is not recognised as a *p* value by the other checks in this report, nor by StatCheck, so any inconsistency between it and its test statistic will not be detected automatically."
      )

      report <- c(
        report,
        negative_text,
        scroll_table(negative_table, colwidths = c(.1, .9)),
        collapse_section(negative_guidance)
      )
    }
  }

  # ---- Return list ----
  list(
    table = p,
    summary_table = summary_table,
    traffic_light = tl,
    na_replace = 0,
    summary_text = summary_text,
    report = report
  )
}
