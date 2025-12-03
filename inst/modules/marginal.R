#' Marginal Significance
#'
#' @description
#' List all sentences that describe an effect as 'marginally significant'.
#'
#' @author Daniel Lakens
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "marginal")
marginal <- function(paper) {
  # detailed table of results ----
  pattern <- "margin\\w* (?:\\w+\\s+){0,5}significan\\w*|trend\\w* (?:\\w+\\s+){0,1}significan\\w*|almost (?:\\w+\\s+){0,2}significan\\w*|approach\\w* (?:\\w+\\s+){0,2}significan\\w*|border\\w* (?:\\w+\\s+){0,2}significan\\w*|close to (?:\\w+\\s+){0,2}significan\\w*"
  table <- search_text(paper, pattern)

  # I dropped here ID and Header columns (unwanted)
  cols <- c("section", "text")
  report_table <- table[, cols, drop = FALSE]

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, name = "marginal")

  # determine the traffic light ----
  tl <- ifelse(nrow(table), "red", "green")

  report_text = c(
    red = "You described effects with terms related to 'marginally significant'. If *p* values above 0.05 are interpreted as an effect, you inflate the alpha level, and increase the Type 1 error rate. If a *p* value is higher than the prespecified alpha level, it should be interpreted as a non-significant result.",
    green = "No effects were described with terms related to 'marginally significant'."
  )

  # If there are results → build scrollable table
  if (nrow(table) > 0) {
    guidance <- c(
      "For metascientific articles demonstrating the rate at which non-significant p-values are interpreted as marginally significant, see:",
      "Olsson-Collentine, A., van Assen, M. A. L. M., & Hartgerink, C. H. J. (2019). The Prevalence of Marginally Significant Results in Psychology Over Time. Psychological Science, 30(4), 576–586. ",
      "<https://doi.org/10.1177/0956797619830326>",
      "For the list of terms used to identifify marginally significant results, see this blog post by Matthew Hankins:",
      "<https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/>"
    )

    # Combining the main feedback text with the scrollable table -> user will be able to see the interpretation first then see the sentences
    report <- c(report_text[[tl]],
                scroll_table(report_table),
                collapse_section(guidance)) |>
              paste0(collapse = "\n\n")

  } else {
    # When nothing is detected -> return textual feedback with no tables (as minimal output when it is not needed to keep the report clear)
    report <- report_text[[tl]]
  }

  list(
    table = table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = "Marginal summary this one is a bit longer to span multiple line in the report",
    summary_table = summary_table
  )
}

