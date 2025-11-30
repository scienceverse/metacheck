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
marginal <- function(paper) {
  # detailed table of results ----
  table <- search_text(paper, pattern = ".*", section = "discussion", return = "sentence")
  # Get the inference
  causal_classification <- causal_relations(table$text)
  # And for the title
  causal_title <- causal_relations(paper$info$title)
  # Keep only causal sentences
  causal_classification <- causal_classification[causal_classification$causal == TRUE, ]
  # Remove duplicates based on 'sentence' as the inference returns multiple rows per sentence if there are mutiple causal aspects
  causal_classification <- causal_classification[!duplicated(causal_classification$sentence), ]
  
  # summary output for paperlists ----
  summary_table <- dplyr::count(causal_classification, causal, name = "sentences")
  
  # determine the traffic light ----
  tl <- ifelse(nrow(causal_classification) > 0 | nrow(causal_title) > 0, "yellow", "green")
  
  report_text = c(
    yellow = "Medical journals often have the following instruction in the author guidelines about the use of causal language: Causal language (including use of terms such as effect and efficacy) should be used only for randomized clinical trials. For all other study designs (including meta-analyses of randomized clinical trials), methods and results should be described in terms of association or correlation and should avoid cause-and-effect wording. You have sentences with causal statements in the title and/or discussion. Carefully check if the sentences based on the data you have collected are warranted, given the study design.",
    green = "No sentences with causal claims were identified in the title or discussion."
  )
  
  # If there are results → build scrollable table
  if (nrow(causal_classification) > 0) {
    
    # I render each result row as HTML <tr>/<td> with borders and padding so the sentences are clearly separated
    table_rows <- apply(causal_classification, 1, function(row) {
      paste0(
        "<tr>",
        paste(
          sprintf("<td style='border:1px solid #ccc; padding:6px;'>%s</td>", row),
          collapse = ""
        ),
        "</tr>"
      )
    }) |> paste(collapse="\n")
    
    # Here I built a full HTML table with styled headers + smaller font to make it fit (without feeling cramped)
    table_html <- paste0(
      "<table style='border-collapse:collapse; width:100%; font-size:90%;'>",
      "<thead><tr>",
      paste(
        sprintf("<th style='border:1px solid #ccc; padding:6px; background-color:#f0f0f0;'>%s</th>", names(causal_classification)),
        collapse=""
      ),
      "</tr></thead>",
      "<tbody>", table_rows, "</tbody>",
      "</table>"
    )
    
    # I wrapped the table here in a scrollable box. I chose the max hight of the box to keep control of how high it must be to not make the report too long (we can scroll in the box either way!)
    scrollbox <- paste0(
      "<br><div><strong>You can see these detected sentences in the following table:</strong></div>",
      "<div style='border:1px solid #444; padding:10px; max-height:450px; overflow-y:auto; ",
      "background-color:#ffffff; margin-top:5px; margin-bottom:15px;'>",
      table_html,
      "</div>"
    )
    
    guidance <- paste0(
      "For metascientific articles demonstrating the rate at which non-significant p-values are interpreted as marginally significant, see:<br><br>",
      "Olsson-Collentine, A., van Assen, M. A. L. M., & Hartgerink, C. H. J. (2019). The Prevalence of Marginally Significant Results in Psychology Over Time. Psychological Science, 30(4), 576–586. ",
      "<a href='https://doi.org/10.1177/0956797619830326' target='_blank'>https://doi.org/10.1177/0956797619830326</a> <br>",
      "For the list of terms used to identifify marginally significant results, see this blog post by Matthew Hankins: ",
      "<a href='https://mchankins.wordpress.com/2013/04/21/still-not-significant-2' target='_blank'>https://web.archive.org/web/20251001114321/https://mchankins.wordpress.com/2013/04/21/still-not-significant-2/</a><br><br>"
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
    
    # Combining the main feedback text with the scrollable table -> user will be able to see the interpretation first then see the sentences
    final_report <- paste0(
      report_text[[tl]],
      "<div style='margin-top:8px;'></div>",
      scrollbox,
      guidance_block
    )
    
  } else {
    # When nothing is detected -> return textual feedback with no tables (as minimal output when it is not needed to keep the report clear)
    final_report <- report_text[[tl]]
  }
  
  list(
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = final_report
  )
}

