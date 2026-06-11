#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.) or reported as exactly zero (e.g., p = .000).
#'
#' @details
#' This module uses regular expressions to identify p-values. It will flag any values reported as p > ? or p < numbers greater than .001. It will also flag p-values reported as exactly zero (e.g., p = .000, p = 0.00), which are mathematically impossible — p-values are never exactly zero and should instead be reported as p < .001.
#'
#' We try to exclude figure and table notes like "* p < .05", but may not succeed at excluding all false positives.
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

  cols <- c("text", "expanded")
  report_table <- unique(p[p$imprecise, cols, drop = FALSE])
  colnames(report_table) <- c("P-Value", "Text")

  zero_table <- unique(p[p$zero, cols, drop = FALSE])
  colnames(zero_table) <- c("P-Value", "Text")

  # summary_table ----
  imprecise_summary <- dplyr::count(p[p$imprecise, , drop = FALSE], paper_id, name = "n_imprecise")
  zero_summary <- dplyr::count(p[p$zero, , drop = FALSE], paper_id, name = "n_zero")
  summary_table <- dplyr::full_join(imprecise_summary, zero_summary, by = "paper_id")

  # traffic light ----
  if (nrow(p) == 0) {
    tl <- "na"
  } else if (nrow(report_table) == 0 && nrow(zero_table) == 0) {
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
      "We found no imprecise *p* values or *p*-values of exactly zero out of %d detected.",
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
