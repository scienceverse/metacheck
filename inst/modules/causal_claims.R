#' Randomization and Causal Claims
#'
#' @description
#' Aims to identify the presence of random assignment, and lists sentences that make causal claims in title or abstract.
#'
#' @details
#' The Randomization and Causal Claims Check first uses regular expressions to check whether the manuscript contains a statement about randomization to conditions. Subsequently, it sends the title and abstract to a [machine learning classifier developed by Rasoul Norouzi](https://github.com/rasoulnorouzi/causal_relation_miner) that runs on [HuggingFace](https://huggingface.co/spaces/lakens/causal_sentences). Causal statements are identified. Researchers are recommended to double check if causal statements are warranted, especially if no sentences describing randomization were detected.
#'
#' The regular expressions can miss statements about randomization, or incorrectly assume there is a sentence describing randomization. The module can’t evaluate if the causal statements that are identified are warranted or not, and it only reminds users to double-check.
#'
#' If you want to improve the detection of sentences describing randomization, or otherwise improve the module, reach out to the Metacheck development team.
#'
#'
#' @keywords method
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
causal_claims <- function(paper) {
  # randomisation ----

  random_sentences <- search_text(paper, "random")

  ## define match patterns ----
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

  ## get matches ----
  inc <- grepl(include_re, random_sentences$text, perl = TRUE)
  exc <- grepl(exclude_re, random_sentences$text, perl = TRUE)
  random_assignment_subset <- random_sentences[inc & !exc, , drop = FALSE]

  ## summary_text for rand ----
  ## report for rand ----
  if (nrow(random_assignment_subset) == 0) {
    summary_text_randomization <- "We identified no sentences describing randomization."

    report_randomization <- c(
      "Metacheck's text matching algorithm did not identify sentences describing randomization. If random assignment was present, please clearly report this (e.g., participants were randomly assigned to...). If this was a non-randomized study, the journal article reporting standards (JARS) ask that you describe the following:",
      "- Procedures employed to help minimize potential bias due to nonrandomization (e.g., matching, propensity score matching)."
    )
  } else {
    summary_text_randomization <- sprintf(
      "We identified %s sentence%s describing randomization.",
      nrow(random_assignment_subset),
      nrow(random_assignment_subset) |> plural()
    )

    report_randomization <- c(
      summary_text_randomization,
      scroll_table(random_assignment_subset$text),
      "If this was a study that contained random assignment to conditions, the journal article reporting standards (JARS) ask that you describe the following:",
      "1. Random assignment method: Procedure used to generate the random assignment sequence, including details of any restriction (e.g., blocking, stratification)",
      "2. Random assignment concealment: Whether sequence was concealed until interventions were assigned",
      "3. Random assignment implementation: Who generated the assignment sequence, who enrolled participants, who assigned participants to groups"
    )
  }


  # causal claims ----
  table <- search_text(paper, pattern = ".*", section = "abstract")
  causal_title <- causal_relations(paper$info$title)
  causal_abstract <- causal_relations(table$text)

  ## summary_table ----
  summary_table <- causal_abstract |>
    dplyr::left_join(table, by = c(sentence = "text")) |>
    dplyr::summarise(causal = sum(causal), .by = "id")
  # filter after so join doesn't fail
  causal_abstract <- dplyr::filter(causal_abstract, causal)

  ## causal title ----
  if (!any(causal_title$causal)) {
    summary_text_title <- "No causal claims were observed in the title."
    report_causal_title <- summary_text_title
  } else {
    summary_text_title <- "Causal claims were detected in the title."
    report_causal_title <- c(
      summary_text_title,
      scroll_table(causal_title[, c("sentence", "cause", "effect")]), 1
    )
  }

  ## causal abstract ----
  if (!any(causal_abstract$causal)) {
    summary_text_abstract <- "No causal claims were observed in the abstract."
    report_text_causal_abstract <- ""
  } else {
    summary_text_abstract <- "Causal claims were detected in the abstract."

    if (nrow(random_assignment_subset) == 0) {
      report_text_causal_abstract <- "As no random assignment to conditions was detected in the text, carefully check if the sentences below are warranted, given the study design. If random assignment was present, please clearly report this."
    } else {
      report_text_causal_abstract <- "Random assignment was detected, so these causal claims might be warranted, but it is always prudent to double-check."
    }
  }
  # Create the report. We only select some columns for the printed report
  report_causal_abstract <- c(
    summary_text_abstract,
    scroll_table(causal_abstract[, c("sentence", "cause", "effect")], 1),
    report_text_causal_abstract
  )

  report_text <- "Journal Article Reporting Standards require details about randomization procedures, or how possible bias due to non-randomization is mitigated. This information is often not reported. Furthermore, researchers sometimes make causal claims that are not warranted, for example because there was no random assignment to conditions. This module checks how (non)randomization is reported, and checks for causal claims in the title and abstract. Researchers are asked to double check whether this information is reported completely and correctly."

  guidance <- c(
    "For advice on how to make causal claims, and when not to, see:",
    format_ref(Antonakis2010),
    format_ref(Grosz2020),
    "For the APA journal articles reporting standards, see <https://apastyle.apa.org/jars>"
  )

  # traffic light ----
  if (any(causal_title$causal) | any(causal_abstract$causal)) {
    if (nrow(random_assignment_subset) == 0) {
      tl <- "yellow" # causal language without random assignment
    } else {
      tl <- "green" # causal language with random assignment
    }
  } else {
    tl <- "green" # no causal language
  }

  # report ----
  report <- c(
    report_text,
    "#### Randomization",
    report_randomization,
    "#### Causal Claims",
    report_causal_title,
    report_causal_abstract,
    collapse_section(guidance)
  )

  # summary_text ----
  # make a list to show
  summary_text <- c(
    summary_text_randomization,
    summary_text_title,
    summary_text_abstract
  ) |>
    paste("\n- ", x = _, collapse = "")

  # return a list ----
  list(
    table = causal_abstract,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# references ----

Antonakis2010 <- bibentry(
  bibtype = "Article",
  title = "On making causal claims: A review and recommendations",
  author = c(
    person("John", "Antonakis"),
    person("Samuel", "Bendahan"),
    person("Philippe", "Jacquart"),
    person("Rafael", "Lalive")
  ),
  journal = "The Leadership Quarterly",
  year = 2010,
  volume = 21,
  number = 6,
  pages = "1086--1120",
  doi = "10.1016/j.leaqua.2010.10.010"
)

Grosz2020 <- bibentry(
  bibtype = "Article",
  title = "The Taboo Against Explicit Causal Inference in Nonexperimental Psychology",
  author = c(
    person("Martin P.", "Grosz"),
    person("Julia M.", "Rohrer"),
    person("Felix", "Thoemmes")
  ),
  journal = "Perspectives on Psychological Science",
  year = 2020,
  volume = 15,
  number = 5,
  pages = "1243--1255",
  doi = "10.1177/1745691620921521"
)
