#' Randomization and Causal Claims
#'
#' @description
#' Aims to identify the presence of random assignment, and lists sentences that make causal claims in title or abstract.
#'
#' @keywords method
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' paper <- psychsci[[100]]
#' module_run(paper, "causal_claims")
causal_claims <- function(paper) {
  # First we examine if there is random assignment to conditions. In this case, causal claims should be fine.

  random_sentences <- search_text(paper, "random")

  ## Create a subset of random_sentences that matches the INCLUDE regex and does NOT match the EXCLUDE regex.

  ## 1) Define the two patterns (case-insensitive, free-spacing)
  include_re <- "(?ix)(
    \\brandom(?:ly)?\\s+assign(?:ed|ment)\\b
  | \\bassign(?:ed)?\\s+at\\s+random\\b
  | \\bassign(?:ed)?\\s+random(?:ly)?\\b
  | \\brandomiz(?:e|ed|ation)\\b
  | \\brandom(?:ly)?\\s+allocat(?:ed|ion)\\b
  | \\brandom(?:ly)?\\s+divid(?:e|ed)\\b
  | \\brandom(?:ly)?\\s+split\\b
  | \\bstratified\\s+random\\s+assign(?:ment|ed)\\b
)"

  exclude_re <- "(?ix)(
    \\brandom\\s+effects?\\b
  | \\brandom\\s+intercepts?\\b
  | \\brandom\\s+slopes?\\b
  | \\brandom\\s+factors?\\b
  | \\brandom\\s+coefficients?\\b
  | \\brandom\\s+error\\b
  | \\brandom\\s+fields?\\b
  | \\brandom\\s+generator\\b

  # order / trial / block randomized (either side of the word)
  | (?:\\border\\b[^\\n]{0,60}\\brandom(?:ly|ized)?\\b)|(?:\\brandom(?:ly|ized)?\\b[^\\n]{0,60}\\border\\b)
  | (?:\\btrial\\b[^\\n]{0,60}\\brandom(?:ly|ized)?\\b)|(?:\\brandom(?:ly|ized)?\\b[^\\n]{0,60}\\btrial\\b)
  | (?:\\bblock\\b[^\\n]{0,60}\\brandom(?:ly|ized)?\\b)|(?:\\brandom(?:ly|ized)?\\b[^\\n]{0,60}\\bblock\\b)
  | \\brandom\\s+jitter(?:ed|ing)?\\b
  | \\brandom\\s+noise\\b
  | \\brandom\\s+pixels?\\b|\\brandom\\-pixel\\b
  | \\b(?:pseudo)?random\\b

  # sampling / lotteries / non-arm selection
  | \\brandom\\s+digit\\-?dial(?:ing)?\\b
  | \\brandom\\s+samples?\\b|\\brandom\\s+sampling\\b|\\brandom\\s+sampled\\b
  | \\brandom\\s+lotter(y|ies)\\b|\\brandom\\s+offer\\b
  | \\brandom\\s+distribut(?:ion|ed)\\b
  | \\brandom\\s+locations?\\b
  | \\brandom(?:ly)?\\s+intermixed\\b

  # diagnostic mentions rather than the assignment sentence itself
  | \\bsuccessful\\s+random\\s+assignment\\b
)"

  ## 2) Compute logical masks (treat NA text as non-matches)
  inc <- !is.na(random_sentences$text) & grepl(include_re, random_sentences$text, perl = TRUE)
  exc <- !is.na(random_sentences$text) & grepl(exclude_re, random_sentences$text, perl = TRUE)

  ## 3) Subset rows that are included but not excluded
  random_assignment_subset <- random_sentences[ inc & !exc, , drop = FALSE ]
  "Procedures employed to help minimize potential bias due to nonrandomization
  (e.g., matching, propensity score matching)"
  if (nrow(random_assignment_subset) == 0) {
    summary_randomization <- "Metacheck's text matching algorithm did not identify sentences describing randomization. If random assignment was present, please clearly report this (e.g., participants were randomly assigned to...). If this was a non-randomized study, the journal article reporting standards (JARS) ask that you describe the following: \n\nProcedures employed to help minimize potential bias due to nonrandomization (e.g., matching, propensity score matching)."
    summary_text_randomization <- "No sentences describing randomization were identified."
  } else {

    summary_randomization <- sprintf(
      "%s sentences describing randomization were identified. If this was a study that contained random assignment to conditions, the journal article reporting standards (JARS) ask that you describe the following in at least one of the sentences in the table below:\n\n\
1. Random assignment method: Procedure used to generate the random assignment sequence, including details of any restriction (e.g., blocking, stratification)\n\
2. Random assignment concealment: Whether sequence was concealed until interventions were assigned\n\
3. Random assignment implementation: Who generated the assignment sequence, who enrolled participants, who assigned participants to groups",
      nrow(random_assignment_subset)
    )
    summary_text_randomization <- sprintf("%s sentences describing randomization were identified.", nrow(random_assignment_subset))
  }

  report_randomization <- c(summary_randomization,
                            scroll_table(random_assignment_subset[, c("text")]))


  # detailed table of results ----
  table <- search_text(paper, pattern = ".*", section = "abstract", return = "sentence")
  # Get the inference
  causal_classification <- causal_relations(table$text)
  # And for the title
  causal_title <- causal_relations(paper$info$title)
  # Remove duplicates based on 'sentence' as the inference returns multiple rows per sentence if there are mutiple causal aspects
  causal_classification <- causal_classification[!duplicated(causal_classification$sentence), ]
  # Bind the inference back to the id
  causal_classification <- cbind(table, causal_classification)
  # Keep only causal sentences
  causal_classification <- causal_classification[causal_classification$causal == TRUE, ]

  # summary output for paperlists ----
  if (nrow(causal_classification) == 0) {
    # Create the data frame directly
    summary_table <- data.frame(
      id = table$id[1],
      causal = 0L,
      stringsAsFactors = FALSE
    )
  } else {
    summary_table <- dplyr::count(causal_classification, id, name = "causal", .drop = FALSE)
  }

  # report
  if (isTRUE(all(causal_title$causal == FALSE))) {
    summary_causal_title <- "No causal claims were observed in the title."
    summary_text_title <- "No causal claims were observed in the title."
    report_causal_title <- ""
  } else {
    summary_causal_title <- "Causal claims detected in the title: "
    summary_text_title <- "Causal claims detected in the title."
    report_causal_title <- c(
    summary_causal_title,
    scroll_table(causal_title[, c("sentence", "cause", "effect")]))
  }

  # report
  # If all classifications are false
  if (isTRUE(all(causal_classification$causal == FALSE))) {
    summary_causal_abstract <- "No causal claims were observed in the abstract,"
    summary_text_abstract <- "No causal claims were observed in the abstract."
  } else {
    if (nrow(random_assignment_subset) == 0) {
      summary_causal_abstract <- "Causal claims detected in the abstract. As no random assignment to conditions was detected in the text, carefully check if the sentences below are warranted, given the study design. If random assignment was present, please clearly report this."
    } else {
    summary_causal_abstract <- "Causal claims detected in the abstract. Random assignment was detected, so these causal claims might be warranted, but it is always prudent to double-check."
    }
    summary_text_abstract <- "Causal claims detected in the abstract."
  }
  # Create the report. We only select some columns for the printed report
  report_causal_abstract <- c(
    summary_causal_abstract,
    scroll_table(causal_classification[, c("sentence", "cause", "effect")])
  )

  report_text = "Journal Article Reporting Standards require details about randomization procedures, or how possible bias due to non-randomization is mitigated. This information is often not reported. Furthermore, researchers sometimes make causal claims that are not warranted, for example because there was no random assignment to conditions. This module checks how (non)randomization is reported, and checks for causal claims in the title and abstract. Researchers are asked to double check whether this information is reported completely and correctly. \n"

  guidance <- c(
    "For advice on how to make causal claims, and when not to, see:",
    "Antonakis, J., Bendahan, S., Jacquart, P., & Lalive, R. (2010). On making causal claims: A review and recommendations. The Leadership Quarterly, 21(6), 1086–1120. <a href='https://doi.org/10.1016/j.leaqua.2010.10.010' target='_blank'>https://doi.org/10.1016/j.leaqua.2010.10.010</a>",
    "Grosz, M. P., Rohrer, J. M., & Thoemmes, F. (2020). The Taboo Against Explicit Causal Inference in Nonexperimental Psychology. Perspectives on Psychological Science, 15(5), 1243–1255. <a href='https://doi.org/10.1177/1745691620921521' target='_blank'>https://doi.org/10.1177/1745691620921521</a>",
    "For the APA journal articles reporting standards, see <a href='https://apastyle.apa.org/jars' target='_blank'>https://apastyle.apa.org/jars</a>"
    )

  # determine the traffic light ----
  tl <- ifelse((isTRUE(all(causal_classification$causal == FALSE)))&(isTRUE(all(causal_title$causal == FALSE))), "green", "info")

  report <- c(report_text, "#### Randomization", report_randomization, "#### Causal Claims", report_causal_title, report_causal_abstract, collapse_section(guidance))

  # return a list ----
  list(
    table = causal_classification,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = paste(summary_text_randomization, summary_text_title, summary_text_abstract)
  )
}
