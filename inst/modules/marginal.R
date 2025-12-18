#' Marginal Significance
#'
#' @description
#' List all sentences that describe an effect as 'marginally significant'.
#'
#' @keywords results
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
  # table ----
  pattern <- "margin\\w* (?:\\w+\\s+){0,5}significan\\w*|trend\\w* (?:\\w+\\s+){0,1}significan\\w*|almost (?:\\w+\\s+){0,2}significan\\w*|approach\\w* (?:\\w+\\s+){0,2}significan\\w*|border\\w* (?:\\w+\\s+){0,2}significan\\w*|close to (?:\\w+\\s+){0,2}significan\\w*"
  table <- search_text(paper, pattern)

  # summary_table ----
  summary_table <- dplyr::count(table, id, name = "marginal")

  # traffic light ----
  tl <- ifelse(nrow(table), "red", "green")

  # summary_text ----
  summary_text <- sprintf("You described %d effect%s with terms related to 'marginally significant'.", nrow(table), ifelse(nrow(table) == 1, "", "s"))


  # report ----
  if (tl == "green") {
    report <- "No effects were described with terms related to 'marginally significant'."
  } else if (tl == "red") {
    report_text <- "You described effects with terms related to 'marginally significant'. If *p* values above 0.05 are interpreted as an effect, you inflate the alpha level, and increase the Type 1 error rate. If a *p* value is higher than the prespecified alpha level, it should be interpreted as a non-significant result."

    guidance <- c(
      "For metascientific articles demonstrating the rate at which non-significant p-values are interpreted as marginally significant, see:",
      format_ref(marg_ref),
      "For the list of terms used to identifify marginally significant results, see this [blog post by Matthew Hankins](https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/)."
    )

    cols <- c("section", "text")
    report_table <- table[, cols, drop = FALSE]

    report <- c(report_text,
                scroll_table(report_table),
                collapse_section(guidance))
  }

  # return list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

marg_ref <- bibentry(
  bibtype = "Article",
  title = "The Prevalence of Marginally Significant Results in Psychology Over Time",
  author = "Olsson-Collentine, A., van Assen, M. A. L. M., & Hartgerink, C. H. J.",
  year = "2019",
  journal = "Psychological Science",
  volume = 30,
  issue = 4,
  pages = "576--586",
  doi = "10.1177/0956797619830326"
)
