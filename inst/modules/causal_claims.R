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
  table <- search_text(paper, pattern = ".*", section = "discussion", return = "sentence")
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
  summary_table <- dplyr::count(causal_classification, id, name = "causal")

  # determine the traffic light ----
  tl <- ifelse(nrow(causal_classification) > 0 | nrow(causal_title) > 0, "yellow", "green")

  report_text = c(
    yellow = "Medical journals often have the following instruction in the author guidelines about the use of causal language: <br><br> *Causal language (including use of terms such as effect and efficacy) should be used only for randomized clinical trials. For all other study designs (including meta-analyses of randomized clinical trials), methods and results should be described in terms of association or correlation and should avoid cause-and-effect wording.* <br><br> You have sentences with causal statements in the title and/or discussion. Carefully check if the sentences based on the data you have collected are warranted, given the study design.",
    green = "No sentences with causal claims were identified in the title or discussion."
  )


  # If there are results → build scrollable table
  if (nrow(causal_classification) > 0) {

    # Build title block if causal_title has results
    title_block <- ""
    if (causal_title$causal == TRUE) {
      title_rows <- apply(causal_title, 1, function(row) {
        paste0(
          "<tr>",
          paste(sprintf("<td style='border:1px solid #ccc; padding:6px;'>%s</td>", row), collapse = ""),
          "</tr>"
        )
      }) |> paste(collapse = "\n")

      title_html <- paste0(
        "<table style='border-collapse:collapse; width:100%; font-size:90%;'>",
        "<thead><tr>",
        paste(sprintf("<th style='border:1px solid #ccc; padding:6px; background-color:#f0f0f0;'>%s</th>", names(causal_title)), collapse = ""),
        "</tr></thead>",
        "<tbody>", title_rows, "</tbody>",
        "</table>"
      )

      title_block <- paste0(
        "<br><div><strong>Causal claims detected in the title:</strong></div>",
        "<div style='border:1px solid #444; padding:10px; max-height:150px; overflow-y:auto; ",
        "background-color:#ffffff; margin-top:5px; margin-bottom:15px;'>",
        title_html,
        "</div>"
      )
    }

    # Build discussion table as before
    table_rows <- apply(causal_classification, 1, function(row) {
      paste0(
        "<tr>",
        paste(sprintf("<td style='border:1px solid #ccc; padding:6px;'>%s</td>", row), collapse = ""),
        "</tr>"
      )
    }) |> paste(collapse = "\n")

    table_html <- paste0(
      "<table style='border-collapse:collapse; width:100%; font-size:90%;'>",
      "<thead><tr>",
      paste(sprintf("<th style='border:1px solid #ccc; padding:6px; background-color:#f0f0f0;'>%s</th>", names(causal_classification)), collapse = ""),
      "</tr></thead>",
      "<tbody>", table_rows, "</tbody>",
      "</table>"
    )

    scrollbox <- paste0(
      "<br><div><strong>Causal claims detected in the discussion section:</strong></div>",
      "<div style='border:1px solid #444; padding:10px; max-height:450px; overflow-y:auto; ",
      "background-color:#ffffff; margin-top:5px; margin-bottom:15px;'>",
      table_html,
      "</div>"
    )

    guidance <- paste0(
      "For advice on how to make causal claims, and when not to, see:<br><br>",
      "Antonakis, J., Bendahan, S., Jacquart, P., & Lalive, R. (2010). On making causal claims: A review and recommendations. The Leadership Quarterly, 21(6), 1086–1120. ",
      "<a href='https://doi.org/10.1016/j.leaqua.2010.10.010' target='_blank'>https://doi.org/10.1016/j.leaqua.2010.10.010</a> <br>",
      "Grosz, M. P., Rohrer, J. M., & Thoemmes, F. (2020). The Taboo Against Explicit Causal Inference in Nonexperimental Psychology. Perspectives on Psychological Science, 15(5), 1243–1255. ",
      "<a href='https://doi.org/10.1177/1745691620921521' target='_blank'>https://doi.org/10.1177/1745691620921521</a> <br>"
    )
    guidance_block <- paste0(
      "<details style='display:inline-block;'>",
      "<summary style='cursor:pointer; margin:0; padding:0;'>",
      "<strong><span style='font-size:20px; color:#006400;'>Learn More</span></strong>",
      "</summary>",
      "<div style='margin-top:10px;'>",
      guidance,
      "</div>",
      "</details>"
    )


    # Combine everything: report text + title block + discussion table + guidance
    final_report <- paste0(
      report_text[[tl]],
      "<div style='margin-top:8px;'></div>",
      title_block,
      scrollbox,
      guidance_block
    )

  } else {
    # When nothing is detected
    final_report <- paste0(
      report_text[[tl]],
      "<div style='margin-top:8px;'></div>",
      guidance_block   # <-- add here too
    )
  }

  list(
    table = causal_classification,
    summary_table = summary_table,
    traffic_light = tl,
    report = final_report
  )
}

