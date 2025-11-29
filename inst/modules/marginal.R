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
  
  # I dopped here ID and Header columns (unwanted)
  drop_cols <- c("id", "header", "div", "p", "s")
  table <- table[, setdiff(names(table), drop_cols), drop = FALSE]
  
  # summary output for paperlists ----
  summary_table <- dplyr::count(search_text(paper, pattern), id, name = "marginal")
  
  # determine the traffic light ----
  tl <- ifelse(nrow(table), "red", "green")
  
  report_text = c(
    red = "You described effects with terms related to 'marginally significant'. If *p* values above 0.05 are interpreted as an effect, you inflate the alpha level, and increase the Type 1 error rate. If a *p* value is higher than the prespecified alpha level, it should be interpreted as a non-significant result.",
    green = "No effects were described with terms related to 'marginally significant'."
  )
  
  # If there are results → build scrollable table
  if (nrow(table) > 0) {
    
    # I render each result row as HTML <tr>/<td> with borders and padding so the sentences are clearly separated
    table_rows <- apply(table, 1, function(row) {
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
        sprintf("<th style='border:1px solid #ccc; padding:6px; background-color:#f0f0f0;'>%s</th>", names(table)),
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

