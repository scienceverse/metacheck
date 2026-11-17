#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.), reported as exactly zero (e.g., p = .000), or impossible (below 0 or above 1, e.g., p = -.22).
#'
#' @details
#' This module uses regular expressions to identify p-values. It will flag any values reported as p > ? or p < numbers greater than .001. It will also flag p-values reported as exactly zero (e.g., p = .000, p = 0.00), which are mathematically impossible — p-values are never exactly zero and should instead be reported as p < .001.
#'
#' A p-value that is not the result of a test is not flagged: a table note ("* p < .05, ** p < .01"), a figure legend ("Asterisks indicate significant differences (p < .05)", "Values in bold are significant at p < .05") or a stated threshold ("p < .05 was considered significant", "thresholded at p < .001"). GROBID often puts such notes into the running text, so each p-value is judged by the words in its own clause (the text up to a ";" or a full stop), and never left out when a test statistic belongs to it ("F(1, 7) = 8.03, # p < .05"). The column `not_a_test` of the table says why a p-value was left out ("marker", "threshold", "legend" or "named").
#'
#' A p-value with the name of a correction or a test after the p ("pFWE < .05", "padj < 0.05", "p Wilcoxon = .02") is flagged only when a test statistic comes before it in its clause ("t(37) = 7.17, p FWE < .05"). Without one it is mostly the rule that decided what was significant ("genes with padj < 0.05 were selected"); such a p-value is "named" in `not_a_test`. Notes in other forms, such as a marker after the p-value ("p < .05 (*)"), are still flagged.
#'
#' A p-value below 0 or above 1 is impossible; it is listed apart and not also as imprecise. Often the statistic before it, or its minus sign, was copied into it ("t(134) = -2.32, p = -.13", "Z = 3.091, P = 3.091"). A value above 1 is listed only after a test statistic in its clause: without one, a "P" above 1 was nearly always another quantity (a proportion, pH, a group's mean, a model parameter). A number in scientific notation whose power of ten was lost when the PDF was read ("p = 2.4 x 10 -9" read as 2.4) is not listed. A "p" that is the subscript of another symbol ("r p = .64", "²p = .02" for partial eta squared, "Δp = .33"), or a quantity with a unit, a logarithm or a ratio ("P = 2.9 µm", "-log10 P = 7.5", "Ca/P = 1.67"), is not read as a p-value at all (see `extract_p_values()`). Each p-value has its own row, also when the same one is written twice in a sentence.
#'
#' This module sometimes flags false positives when sentences discuss how p-value thresholds were set. It can also miss an imprecise p-value that is reported against a threshold ("decreased at the p < .05 significance threshold").
#'
#' This module only checks p-values reported in the running text of the manuscript. It cannot (yet) process p-values reported only in tables (as opposed to a table's footnote text, which the module does see and tries to exclude — see above).
#'
#' <validation>Validated on 226 Psychological Science papers with 5,019 hand-coded p-values, 438 of them imprecise (scienceverse/validation, branch stat_p_exact). Version 0.4.4.3 reported 419 p-values as imprecise, 388 of them correctly (93%), and missed 50 (89% found). Of the misses, 13 are starred table notes ("*p < .05") that the module leaves out on purpose and 11 are p-values it did not find; of the wrong reports, 20 are p-values the ground truth does not contain (such as "ps < .05") and 6 use ≤ or ≥. These figures measure agreement with the hand-coding, which has known errors in both directions (scienceverse/validation#48).</validation>
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
  # p-values written with a minus, which extract_p_values() does not read:
  # they are listed here as impossible (#351)
  p <- dplyr::bind_rows(p, .p_extract(paper, negative = TRUE))

  # Expand the sentences so the full sentence can be seen
  p <- expand_text(
    p,
    paper,
    expand_to = c("sentence")
  )

  # "==" is "=": it counted as imprecise (U127)
  p$p_comp[p$p_comp %in% "=="] <- "="
  # the match keeps trailing spaces (U127)
  p$text <- trimws(p$text)

  # Flag imprecise p-values
  p$imprecise <- p$p_comp == "<" & p$p_value > .001
  p$imprecise <- p$imprecise | !p$p_comp %in% c("=", "<")
  p$imprecise <- p$imprecise | is.na(p$p_value)

  # Leave out a p-value that is not the result of a test: a table note
  # ("* p < .05, ** p < .01"), a figure legend ("Asterisks indicate
  # significant differences (p < .05)") or a stated threshold ("p < .05 was
  # considered significant"). Each p-value is judged in its own clause, and
  # never when that clause holds a statistic (see .p_not_test()). Only the
  # starred "*p < .05", ".01" or ".001" was left out before, and of a note
  # repeated under two tables only one copy (U127, #604).
  p$not_a_test <- .p_not_test(p)
  # A p with a correction or test name ("pFWE < .05", "padj < 0.05") counts
  # only when a test statistic comes before it in its clause ("t(37) = 7.17,
  # p FWE < .05"). Without one it is mostly the rule that decided what was
  # significant ("genes with padj < 0.05 were selected", "thresholds at the
  # cluster level, P FWE < 0.05"), in wordings the threshold words do not
  # all catch (#642)
  named <- !is.na(p$p_name) & is.na(p$not_a_test)
  if (any(named)) {
    before <- .p_clause(p[named, , drop = FALSE])$before
    has_stat <- !is.na(before) & grepl(.test_stat_pattern, before, perl = TRUE)
    p$not_a_test[named][!has_stat] <- "named"
  }
  p$imprecise <- p$imprecise & is.na(p$not_a_test)

  # Flag p-values reported as exactly zero (e.g., p = .000, p = 0.00), also
  # "p < .000", which was neither imprecise nor zero (U127)
  # A number written with an exponent is never a zero: "p < 10 -324" is
  # smaller than R can hold and reads as 0 (#642)
  p$zero <- p$p_comp %in% c("=", "<") & !is.na(p$p_value) & p$p_value == 0 &
    !grepl("[eE]\\s*[-\u2212\u00c0]|10\\s*\\^?\\s*[-\u2212\u00c0]", p$text)

  # Flag p-values that no p-value can be (#351): below 0 (a minus, often that
  # of the statistic before it, copied into the p-value), or above 1 after a
  # test statistic in its clause ("Z = 3.091, P = 3.091", "t(18) = 0.466,
  # p = 2.5872"). Not above 1 when the power of ten of a number in
  # scientific notation was lost after it ("p = 2.4 x 10 -9" read as 2.4),
  # and not below 0 when the number is part of a longer one, a formula or a
  # row of numbers ("N: P = -0.23*P concentration", "p < -0.6 -0.4 -0.2" on
  # a figure's axis). In 18 corpora (16,847 papers) this found 9 values below
  # 0 and 8 above 1, each a wrong p-value as written; without a statistic, a
  # value above 1 was nearly always no p-value (a proportion, pH, a group's
  # mean, a model parameter).
  below <- !is.na(p$p_value) & p$p_value < 0
  above <- !is.na(p$p_value) & p$p_value > 1
  p$impossible <- FALSE
  if (any(below | above)) {
    # every p-value of those sentences, so .p_clause() counts their places
    key <- paste(p$paper_id, p$text_id, sep = "\r")
    in_sent <- key %in% key[below | above]
    cl <- .p_clause(p[in_sent, , drop = FALSE])
    whole <- !grepl(.p_lost_power, cl$after, perl = TRUE)
    stat <- !is.na(cl$before) & grepl(.test_stat_pattern, cl$before, perl = TRUE)
    p$impossible[in_sent] <- whole & (below[in_sent] | (above[in_sent] & stat))
  }
  # an impossible p-value is listed once, as impossible
  p$imprecise <- p$imprecise & !p$impossible

  # one row per p-value: two identical p-values in one sentence are two
  # reports, as summary_table counts them, and the same sentence in two
  # papers is two papers' (#351)
  cols <- c("text", "expanded")
  report_table <- p[which(p$imprecise), cols, drop = FALSE]
  colnames(report_table) <- c("P-Value", "Text")

  zero_table <- p[which(p$zero), cols, drop = FALSE]
  colnames(zero_table) <- c("P-Value", "Text")

  impossible_table <- p[which(p$impossible), cols, drop = FALSE]
  colnames(impossible_table) <- c("P-Value", "Text")

  # summary_table ----
  imprecise_summary <- dplyr::count(p[which(p$imprecise), , drop = FALSE], paper_id, name = "n_imprecise")
  zero_summary <- dplyr::count(p[which(p$zero), , drop = FALSE], paper_id, name = "n_zero")
  impossible_summary <- dplyr::count(p[which(p$impossible), , drop = FALSE], paper_id, name = "n_impossible")
  summary_table <- dplyr::full_join(imprecise_summary, zero_summary, by = "paper_id") |>
    dplyr::full_join(impossible_summary, by = "paper_id")

  # traffic light ----
  if (nrow(p) == 0) {
    tl <- "na"
  } else if (nrow(report_table) == 0 && nrow(zero_table) == 0 && nrow(impossible_table) == 0) {
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
      "We found no imprecise *p* values, *p*-values of exactly zero or impossible *p* values out of %d detected.",
      nrow(p)
    )
    summary_text <- report
  } else {
    summary_parts <- c()
    if (nrow(impossible_table) > 0) {
      summary_parts <- c(summary_parts, sprintf(
        "%d impossible *p* value%s",
        nrow(impossible_table),
        plural(nrow(impossible_table))
      ))
    }
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
    n_parts <- length(summary_parts)
    if (n_parts > 2) {
      summary_parts <- c(paste(summary_parts[-n_parts], collapse = ", "), summary_parts[n_parts])
    }
    summary_text <- sprintf(
      "We found %s out of %d detected *p* value%s.",
      paste(summary_parts, collapse = " and "),
      nrow(p),
      plural(nrow(p))
    )

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

    if (nrow(impossible_table) > 0) {
      # (#351)
      impossible_text <- "*P* values lie between 0 and 1, so a negative *p* value or one above 1 cannot be right. Often the value of the test statistic before it, or its minus sign, was copied into the *p* value. Check the analysis output and report the *p* value it gives."
      report <- c(
        report,
        impossible_text,
        scroll_table(impossible_table, colwidths = c(.1, .9))
      )
    }

    if (nrow(report_table) > 0) {
      report_text <- "Reporting *p* values imprecisely (e.g., *p* < .05) reduces transparency, reproducibility, and re-use (e.g., in *p* value meta-analyses). Best practice is to report exact p-values with three decimal places (e.g., *p* = .032) unless *p* values are smaller than 0.001, in which case you can use *p* < .001."

      guidance <- c(
        "The APA manual states: Report exact *p* values (e.g., *p* = .031) to two or three decimal places. However, report *p* values less than .001 as *p* < .001. However, 2 decimals is too imprecise for many use-cases (e.g., a *p* value meta-analysis), so report *p* values with three digits.",
        format_ref(apa)
      )

      # The threshold words cannot always tell a sentence that sets a
      # threshold from one that reports a result against it (#604)
      caution <- "This module sometimes flags false positives when sentences discuss how p-value thresholds were set. It can also miss an imprecise p-value that is reported against a threshold (e.g., \"decreased at the *p* < .05 significance threshold\")."

      report <- c(
        report,
        report_text,
        caution,
        scroll_table(report_table, colwidths = c(.1, .9)),
        collapse_section(guidance)
      )
    }

    if (nrow(zero_table) > 0) {
      zero_text <- "*P* values are never exactly zero. A *p* value of .000 is a rounding artifact \u2014 the actual value is simply smaller than the reported precision. Very small *p* values should be reported as *p* < .001 rather than *p* = .000. A *p* = 0 can also be a model parameter, such as an ARIMA order; check that it is a *p* value."

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
