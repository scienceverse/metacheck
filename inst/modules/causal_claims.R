#' Causal Claims
#'
#' @description
#' List all sentences that make causal claims.
#'
#' @author Daniel Lakens
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' paper <- psychsci[[100]]
#' module_run(paper, "causal_claims")
causal_claims <- function(paper) {
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
  if (causal_title$causal == FALSE) {
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
  # I all classifications are false
  if (isTRUE(all(causal_classification$causal == FALSE))) {
    summary_causal_abstract <- "No causal claims were observed in the abstract,"
    summary_text_abstract <- "No causal claims were observed in the abstract."
  } else {
    summary_causal_abstract <- "Causal claims detected in the abstract. Carefully check if the sentences based on the data you have collected are warranted, given the study design."
    summary_text_abstract <- "Causal claims detected in the abstract."
  }
  # Create the report. We only select some columns for the printed report
  report_causal_abstract <- c(
    summary_causal_abstract,
    scroll_table(causal_classification[, c("sentence", "cause", "effect")])
  )

  report_text = "Medical journals often have the following instruction in the author guidelines about the use of causal language: <br><br> *Causal language (including use of terms such as effect and efficacy) should be used only for randomized clinical trials. For all other study designs (including meta-analyses of randomized clinical trials), methods and results should be described in terms of association or correlation and should avoid cause-and-effect wording.*"

  guidance <- c(
    "For advice on how to make causal claims, and when not to, see:",
    "Antonakis, J., Bendahan, S., Jacquart, P., & Lalive, R. (2010). On making causal claims: A review and recommendations. The Leadership Quarterly, 21(6), 1086–1120. <a href='https://doi.org/10.1016/j.leaqua.2010.10.010' target='_blank'>https://doi.org/10.1016/j.leaqua.2010.10.010</a>",
    "Grosz, M. P., Rohrer, J. M., & Thoemmes, F. (2020). The Taboo Against Explicit Causal Inference in Nonexperimental Psychology. Perspectives on Psychological Science, 15(5), 1243–1255. <a href='https://doi.org/10.1177/1745691620921521' target='_blank'>https://doi.org/10.1177/1745691620921521</a>"
    )

  # determine the traffic light ----
  tl <- ifelse((isTRUE(all(causal_classification$causal == FALSE)))&(isTRUE(all(causal_title$causal == FALSE))), "green", "info")

  report <- c(report_text, report_causal_title, report_causal_abstract, collapse_section(guidance))

  # return a list ----
  list(
    table = causal_classification,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = paste(summary_text_title, summary_text_abstract)
  )
}
