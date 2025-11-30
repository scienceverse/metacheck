#' Exact P-Values
#'
#' @description
#' List any p-values reported with insufficient precision (e.g., p < .05 or p = n.s.)
#'
#' @author Lisa DeBruine
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "exact_p")
exact_p <- function(paper, ...) {
  # ---- Detailed table of results ----
  p <- module_run(paper, "all_p_values")$table
  
  # Expand the sentences so the full sentence can be seen
  p <- expand_text(
    p,
    paper,
    expand_to = c("sentence")
  )
  
  # Flag imprecise p-values
  p$imprecise <- p$p_comp == "<" & p$p_value > .001
  p$imprecise <- p$imprecise | !p$p_comp %in% c("=", "<")
  p$imprecise <- p$imprecise | is.na(p$p_value)
  
  cols <- c("expanded")
  table <- p[p$imprecise, cols]
  
  
  # ---- Determine traffic light ----
  if(nrow(table) == 0) {
    tl <- "na"
    } else {
    tl <- "red"
    }
  
  # ---- Build report ----
  if (nrow(table) == 0) {
    report <- "We detected no imprecise p-values."
  } else {
    module_output <- sprintf(
      "We found %d imprecise p-values. Reporting p-values with inequality signs (e.g., p < .05) or without exact values reduces transparency and reproducibility. Best practice is to report exact p-values (e.g., p = .032) unless extremely small (p < .001).",
      nrow(table)
    )
    
    # Combine problematic sentences
    issues_found <- paste(sprintf("%s", table$expanded), collapse = "\n\n")
    
    # Scrollable block for sentences
    sentences_block <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      issues_found,
      "</ul>",
      "</div>"
    )
    
    # Guidance block
    guidance <- paste0(
      "For best practices on reporting p-values, see:<br><br>",
      "American Statistical Association (2016). The ASA's Statement on p-Values: Context, Process, and Purpose.<br>",
      "<a href='https://doi.org/10.1080/00031305.108https://doi.org/10.1080/00031305.2016.1154108</a><br>",
      "Lakens, D. (2016). Calculating and reporting effect sizes to facilitate cumulative science.<br>",
      "<a href='https://doi.org/10.550617697178https://doi.org/10.1177/1948550617697178</a>"
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
    
    # Combine everything into report
    report <- sprintf(
      "%s\n\n#### The Following Sentences Contain Imprecise P-Values\n\n%s\n\n%s",
      module_output, sentences_block, guidance_block
    )
  }
  
  # ---- Return list ----
  list(
    traffic_light = tl,
    report = report
  )
}
