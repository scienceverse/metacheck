#' Marginal Significance
#'
#' @description
#' List all sentences that describe an effect as 'marginally significant'.
#'
#' @details
#' The marginal module searches for regular expressions that match a predefined pattern. The list of terms is a subset of those listed in a [blog post by Matthew Hankins](https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/). The module returns all sentences that match terms describing ‘marginally significant’ results.
#'
#' Some of the terms identified might not be problematic in some contexts, and there are ways to describe ‘marginal significance’ that are not detected by the module.
#'
#' <validation>In a sample of 51 papers with 87 statements, this module correctly identified 38 statements (true positives) and incorrectly flagged 22 statements (false positives). It failed to detect 27 statements. Thus, among all statements flagged by the module, 63% were genuine cases (positive predictive value). However, the module missed 42% of all true statements (false negative rate). This validation was made with an earlier, looser pattern; the pattern now requires "significant" or "significance" to follow the trigger word closely (#695).</validation>
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
marginal <- function(paper) {
  # nothing to check in an empty paper or list: "na", not red or green
  if (.paper_is_empty(paper)) return(.empty_input_result(paper))

  # table ----
  # "significan..." has to follow the trigger word closely (#695, point 9): the
  # pattern allowed up to five words between them, so "approach is a significant
  # contribution", "almost certainly significant", "trends were significant" and
  # "margins had significantly" were found. Compared on 16 corpora (15,685 papers
  # across disciplines), this pattern leaves out 243 sentences, all but about 5
  # not about a nearly significant result, and finds 71 more ("marginally
  # non-significant", "trend-level significance"), which the hyphen hid. Four more
  # corpora gave the same picture, and two forms found there were added.
  stat <- "(?:statistical(?:ly)?\\s+)?"
  pattern <- paste0(
    "(?<!sub)marginal(?:ly)?\\s+(?:(?:above\\s+the\\s+threshold\\s+for|failed\\s+to\\s+(?:be|reach)|",
    "and\\s+probably\\s+not|short\\s+of|or\\s+not|or\\s+non-?|but\\s+not|and\\s+not|above|below|over|",
    "under|outside|not)\\s*)?(?:the\\s+)?(?:levels?\\s+of\\s+)?", stat, "(?:non-?)?significan\\w*|",
    "\\btrend(?:s|ed|ing)?\\s+(?:towards?|to)\\s+", stat, "significan\\w*|",
    "\\b(?:a|the)\\s+trend\\s+(?:of|for)\\s+", stat, "significan\\w*|",
    "\\btrend(?:-|\\s)?level\\s+", stat, "significan\\w*|\\btrending\\s+(?:towards?\\s+)?", stat, "significan\\w*|",
    "\\bapproach(?:ed|ing)\\s+(?:a\\s+|the\\s+)?(?:(?:conventional|nominal|genome-?wide|traditional)\\s+)?",
    "(?:levels?\\s+of\\s+)?", stat, "significan\\w*|",
    "\\bapproach(?:es)?\\s+", stat, "significance\\b(?!\\s+test)|",
    "\\bborderline\\s+(?:of\\s+)?(?:the\\s+)?", stat, "(?:non-?)?significan\\w*|",
    "\\bborder(?:ed|ing)?\\s+(?:on|at|of|to)\\s+(?:the\\s+)?(?:level\\s+of\\s+)?", stat, "significan\\w*|",
    "\\bborder(?:ed|ing)\\s+", stat, "significance\\b|",
    "\\balmost\\s+(?:reach(?:ed|ing)?\\s+|achiev(?:ed|ing)\\s+|attain(?:ed|ing)\\s+)?", stat, "significan\\w*|",
    "\\bclose\\s+to\\s+(?:reaching\\s+|achieving\\s+|being\\s+|be\\s+)?(?:the\\s+)?", stat, "significan\\w*")
  table <- text_search(paper, pattern, perl = TRUE)

  # summary_table ----
  summary_table <- dplyr::count(table, paper_id, name = "marginal")

  # traffic light ----
  # no such terms: nothing to check, so "na" (not applicable), not green
  tl <- ifelse(nrow(table), "red", "na")

  # summary_text ----
  summary_text <- if (tl == "na") {
    "No effects were described with terms related to 'marginally significant'."
  } else {
    sprintf("You described %d effect%s with terms related to 'marginally significant'.", nrow(table), ifelse(nrow(table) == 1, "", "s"))
  }


  # report ----
  if (tl == "na") {
    report <- "No effects were described with terms related to 'marginally significant'."
  } else if (tl == "red") {
    report_text <- "You described effects with terms related to 'marginally significant'. If *p* values above 0.05 are interpreted as an effect, you inflate the alpha level, and increase the Type 1 error rate. If a *p* value is higher than the prespecified alpha level, it should be interpreted as a non-significant result."

    guidance <- c(
      "For metascientific articles demonstrating the rate at which non-significant p-values are interpreted as marginally significant, see:",
      format_ref(marg_ref),
      "For the list of terms used to identifify marginally significant results, see this [blog post by Matthew Hankins](https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/)."
    )

    cols <- c("text", "header")
    report_table <- table[, cols, drop = FALSE]
    names(report_table) <- c("Text", "Section Header")

    report <- c(
      report_text,
      scroll_table(report_table),
      collapse_section(guidance)
    )
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
  # person() objects: as one string, R's formatter re-parsed the names into
  # "van Assen, M. MAL, Hartgerink &amp;, J. CH" (U3)
  author = c(person(given = "A.", family = "Olsson-Collentine"),
             person(given = c("M.", "A.", "L.", "M."), family = "van Assen"),
             person(given = c("C.", "H.", "J."), family = "Hartgerink")),
  year = "2019",
  journal = "Psychological Science",
  volume = 30,
  issue = 4,
  pages = "576--586",
  doi = "10.1177/0956797619830326"
)
