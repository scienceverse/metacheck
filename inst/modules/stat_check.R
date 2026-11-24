#' StatCheck
#'
#' @description
#' Check consistency of p-values and test statistics
#'
#' @details
#' The Statcheck module runs Statcheck. Statcheck searches for regular expressions that match a predefined pattern, and identifies APA reported statistical tests. More information on the package can be found at <https://github.com/cran/statcheck>. The module only returns Statcheck results for t-tests and F-tests, as these are the only tests which have been validated, see <https://osf.io/preprints/psyarxiv/tcxaj_v1/>.
#'
#' Statcheck was developed by Michèle Nuijten and Sascha Epskamp.
#'
#' Statcheck considers p = 0.000 an error, as you should report p < 0.001. Furthermore, p < 0.03 is an error if the p-value was 0.031, and one should simply report exact p-values (p = 0.031). When the sentence of a test says it is one-sided ("one-tailed", "one-sided" or "directional"), a p-value that fits a one-sided test counts as consistent (statcheck's `OneTailedTxt` setting; #695). When the paper says anywhere that its tests are one-sided, a p-value that fits a one-sided test in either direction counts as consistent too: half the two-sided p, or 1 minus that half for an effect against the predicted direction. Equivalence tests (TOST) are left out, as their p-values do not test the difference. A statistic written with "<" or ">" is read as a bound, and in a sentence about equal variances an F-test's two-sided p (twice the one-tailed p) counts as consistent. For more information, see [StatCheck](https://statcheck.io/).
#'
#' This module only checks statistical results reported in the running text of the manuscript. It cannot (yet) process statistics reported only in tables.
#'
#' <validation>In a sample of 685 tests with 34 instances of inconsistent reporting, Statcheck correctly detected 34 of them, and incorrectly identified 26. Therefore 0% of true instances were missed, and 43% of detections were false positives. See <https://osf.io/preprints/psyarxiv/tcxaj_v1/> for more details of the validation.</validation>
#'
#'
#' @keywords results
#'
#' @references
#' Nuijten M, Epskamp S (2024). _statcheck: Extract Statistics from Articles and
#' Recompute P-Values_. R package version 1.5.0,
#' <https://CRAN.R-project.org/package=statcheck>.
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
stat_check <- function(paper) {
  # detailed table of results ----
  # a test whose own sentence says it is one-sided is checked as one: of 29
  # flags in 149 Meta-Psychology papers, 14 were one-sided tests, and 4 of
  # them said so in their sentence (#695). stats() reads one sentence at a
  # time, so the word has to be in the test's own sentence.
  stat_table <- metacheck::stats(paper, OneTailedTxt = TRUE)

  # handle no stats
  if (nrow(stat_table) == 0) {
    ret <- list(
      summary_table = .per_paper_table(paper),   # every paper of a list (U124)
      table = data.frame(),
      traffic_light = "na",
      report = "No detectable t- or F-tests. StatCheck currently only detects statistics written in APA format.",
      summary_text = "No detectable t- or F-tests. StatCheck currently only detects statistics written in APA format."
    )

    return(ret)
  }

  # We only select t-tests and F-tests for now,
  # as statcheck is only validated for these tests.
  stat_filter <- (stat_table$test_type == "t") |
    (stat_table$test_type == "F")
  table <- stat_table[stat_filter, ]

  # An equivalence test (TOST) reports the p of a test against the equivalence
  # bounds, not of the difference, so statcheck's p cannot be compared with it:
  # mp20202474's two equivalence tests were flagged (#695, point 5)
  table <- table[!grepl("\\bequivalence\\s+(?:tests?|testing|bounds?|margins?|intervals?|ranges?)\\b|\\bTOST\\b|\\btwo\\s+one[- ]sided\\s+tests?\\b",
                        table$text %||% rep("", nrow(table)), ignore.case = TRUE, perl = TRUE), , drop = FALSE]
  # In a paper that says its tests are one-sided, anywhere in its text, a p that
  # fits a one-sided test in either direction is consistent (#695, point 5; see
  # .paper_says_one_sided() and .stat_one_sided_fits())
  if (nrow(table)) {
    one_sided <- .paper_says_one_sided(paper)
    in_paper <- one_sided[as.character(table$paper_id)] %in% TRUE
    fits <- .stat_one_sided_fits(table)
    table$one_sided_fit <- table$error %in% TRUE & in_paper & fits %in% TRUE
    table$error[table$one_sided_fit] <- FALSE
    table$decision_error[table$one_sided_fit] <- FALSE
    # a statistic written with "<" or ">" is a bound, and an F-test for equal
    # variances reports a two-sided p (#695, point 6; see .stat_bound_or_variance_fits())
    table$bound_or_variance_fit <- table$error %in% TRUE & .stat_bound_or_variance_fits(table) %in% TRUE
    table$error[table$bound_or_variance_fit] <- FALSE
    table$decision_error[table$bound_or_variance_fit] <- FALSE
  }

  # no validated stats
  if (nrow(table) == 0) {
    ret <- list(
      summary_table = .per_paper_table(paper),   # every paper of a list (U124)
      table = data.frame(),
      traffic_light = "na",
      report = "No t-tests or F-tests detected.\n\nThe accuracy of StatCheck has only been validated for *t*-tests and *F*-tests.",
      summary_text = "No t-tests or F-tests detected"
    )

    return(ret)
  }

  # summary output for paperlists ----
  summary_table <- dplyr::summarise(
    table,
    statcheck_found = dplyr::n(),
    statcheck_errors = sum(error, na.rm = TRUE),
    statcheck_decision_errors = sum(decision_error, na.rm = TRUE),
    .by = paper_id
  )

  # determine the traffic light ----
  tl <- "green"
  if (any(table$error)) tl <- "red"

  if (tl == "green") {
    report <- "We detected no errors in t-tests or F-tests."
    summary_text <- report
  } else if (tl == "red") {
    n_errors <- sum(table$error, na.rm = TRUE)
    report_text <- "We detected possible errors in test statistics. Note that as the accuracy of statcheck has only been validated for *t*-tests and *F*-tests. As Metacheck only uses validated modules, we only provide statcheck results for *t* tests and *F*-tests."
    summary_text <- sprintf(
      "%d possible error%s in t-tests or F-tests",
      n_errors, ifelse(n_errors == 1, "", "s")
    )

    # report_table ----
    # Only show these columns in the HTML view (if they exist)
    wanted <- c("raw", "computed_p", "section", "text")
    cols <- intersect(wanted, names(table))
    report_table <- table[table$error, cols, drop = FALSE]
    report_table$computed_p <- round(report_table$computed_p, 5)

    # rename the labels for more clarity during checking the table
    label_map <- c(
      raw         = "Text",
      computed_p  = "Recomputed p",
      section     = "Section",
      text        = "Sentence"
    )
    colnames(report_table) <- label_map[cols]

    guidance <- c(
      "For metascientific research on the validity of statcheck, and it's usefulness to prevent statistical reporting errors, see:",
      format_ref(Nuijten2017),
      format_ref(Nuijten2023)
    )

    report <- c(
      report_text,
      scroll_table(report_table, colwidths = c("10em", NA, NA, NA)),
      collapse_section(guidance)
    )
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

## references ----
Nuijten2017 <- bibentry(
  bibtype = "Misc",
  title = "The validity of the tool \"statcheck\" in discovering statistical reporting inconsistencies",
  author = c(
    person("M. B.", "Nuijten"),
    person("M. A. L. M.", "van Assen"),
    person("C. H. J.", "Hartgerink"),
    person("S.", "Epskamp"),
    person("J. M.", "Wicherts")
  ),
  year = 2017,
  journal = "PsyArXiv",
  doi = "10.31234/osf.io/tcxaja",
  note = "Preprint"
)

Nuijten2023 <- bibentry(
  bibtype = "Misc",
  title = "The effectiveness of implementing statcheck in the peer review process to avoid statistical reporting errors",
  author = c(
    person("M. B.", "Nuijten"),
    person("J.", "Wicherts")
  ),
  year = 2023,
  journal = "PsyArXiv",
  doi = "10.31234/osf.io/bxau9",
  note = "Preprint"
)
