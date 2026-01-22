#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.)
#'
#' @details
#' This module uses regular expressions to identify p-values. It will flag any values reported as p > ? or p < numbers greater than .001.
#'
#' We try to exclude figure and table notes like "* p < .05", but may not succeed at excluding all false positives.
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

  cols <- c("text", "expanded")
  report_table <- unique(p[p$imprecise, cols, drop = FALSE])
  colnames(report_table) <- c("P-Value", "Sentence")

  # summary_table ----
  summary_table <- p[p$imprecise, , drop = FALSE]
  summary_table <- dplyr::count(summary_table, id, name = "n_imprecise")

  # traffic light ----
  if (nrow(p) == 0) {
    tl <- "na"
  } else if(nrow(report_table) == 0) {
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
      "We found no imprecise *p* values out of %d detected.",
      nrow(p)
    )
    summary_text <- report
  } else {
    summary_text <- sprintf("We found %d imprecise *p* value%s out of %d detected.",
                            nrow(report_table),
                            plural(nrow(report_table)),
                            nrow(p))

    report_text <- "Reporting *p* values imprecisely (e.g., *p* < .05) reduces transparency, reproducibility, and re-use (e.g., in *p* value meta-analyses). Best practice is to report exact p-values with three decimal places (e.g., *p* = .032) unless *p* values are smaller than 0.001, in which case you can use *p* < .001."

    # Guidance text
    apa <- bibentry(
      bibtype = "Book",
      author  = person("American Psychological Association"),
      year    = 2020,
      title   = "Publication manual of the American Psychological Association",
      edition = "7",
      subtitle = "The official guide to APA style",
      publisher = "American Psychological Association"
    )

    guidance <- c(
      "The APA manual states: Report exact *p* values (e.g., *p* = .031) to two or three decimal places. However, report *p* values less than .001 as *p* < .001. However, 2 decimals is too imprecise for many use-cases (e.g., a *p* value meta-analysis), so report *p* values with three digits.",
      format_ref(apa)
    )

    # Combine everything into report text
    report <- c(report_text,
                scroll_table(report_table, colwidths = c(.1, .9)),
                collapse_section(guidance))
  }

  # ---- Return list ----
  list(
    table = p,
    summary_table = summary_table,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
