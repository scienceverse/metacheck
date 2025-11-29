#' Non-Significant P Value Check
#'
#' @description
#' This module checks for imprecisely reported p values. If p > .05 is detected, it warns for misinterpretations. 
#'
#' @author  Lisa DeBruine (\email{lisa.debruin@glasgow.ac.uk}) and Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @references
#' # Appelbaum, M., Cooper, H., Kline, R. B., Mayo-Wilson, E., Nezu, A. M., & Rao, S. M. (2018). Journal article reporting standards for quantitative research in psychology: The APA Publications and Communications Board task force report. American Psychologist, 73(1), 3–25. https://doi.org/10.1037/amp0000191
#' # Murphy, S. L., Merz, R., Reimann, L.-E., & Fernández, A. (2025). Nonsignificance misinterpreted as an effect’s absence in psychology: Prevalence and temporal analyses. Royal Society Open Science, 12(3), 242167. https://doi.org/10.1098/rsos.242167
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list with table, summary, traffic light, and report text
#'
#' @examples
#' module_run(psychsci[[49]], "nonsignificant_pvalue")
nonsignificant_pvalue <- function(paper) {
  # detailed table of results ----
  res_p <- module_run(paper, "all_p_values")
  # Specify conditions for a significant result
  cond <- !is.na(res_p$table$p_value) &
    res_p$table$p_value <= 0.05 &
    !is.na(res_p$table$p_comp) &
    res_p$table$p_comp %in% c("<", "=")
  
  res_p$table$significance <- ifelse(cond, "significant", "nonsignificant")
  res_p$table <- subset(res_p$table, significance == "nonsignificant")
  
  # Expand the sentences so the full sentence can be seen
  res_p$table <- expand_text(
    res_p$table,
    paper,
    expand_to = c("sentence")
  )
  
  table <- res_p$table
  
  # summary output for paperlists ----
  # must have id column as the id of each paper, one row per paper
  # further columns to be added to a master summary table
  summary_table <- table %>%
    group_by(id) %>%
    summarise(
      n_significant = sum(significance == "significant", na.rm = TRUE),
      n_nonsignificant = sum(significance == "nonsignificant", na.rm = TRUE)
    )  
  # determine the traffic light ----
  # possible values: na, info, red, yellow, green, fail
  tl <- if (sum(summary_table$n_nonsignificant) > 0) "yellow" else "green"
  
  if (nrow(table) == 0) {
    report <- "We detected no nonsignificant p values."
  } else {
    module_output <- sprintf(
      "We found %d non-significant p values. \n\nMeta-scientific research has shown nonsignificant p values are commonly misinterpreted. It is incorrect to infer that there is 'no effect', 'no difference', or that groups are 'the same' after p > 0.05. \n\nIt is possible that there is a true non-zero effect, but that the study did not detect it. Make sure your inference acknowledges that it is possible that there is a non-zero effect. It is correct to include the effect is 'not significantly' different, although this just restates that p > 0.05. \n\nMetacheck does not yet analyze automatically whether sentences which include non-significant p-values are correct, but we recommend manually checking the sentences below for possible misinterpreted non-significant p values.",
      nrow(table)
    )
    issues_found <- paste(sprintf("%s", table$expanded), collapse = "\n\n")

    sentences_block <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:15px;'>",
      
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      issues_found,
      "</ul>",
      
      "</div>"
    )
    
    guidance <- paste0(
      "For metascientific articles demonstrating the rate of misinterpretations of non-significant results is high, see:<br><br>",
      "Aczel, B., Palfi, B., Szollosi, A., Kovacs, M., Szaszi, B., Szecsi, P., Zrubka, M., Gronau, Q. F., van den Bergh, D., & Wagenmakers, E.-J. (2018). ",
      "Quantifying Support for the Null Hypothesis in Psychology: An Empirical Investigation. <em>Advances in Methods and Practices in Psychological Science</em>, 1(3), 357–366. ",
      "<a href='https://doi.org/10.1177/2515245918773742' target='_blank'>https://doi.org/10.1177/2515245918773742</a> <br>",
      "Murphy, S. L., Merz, R., Reimann, L.-E., &amp; Fernández, A. (2025). ",
      "Nonsignificance misinterpreted as an effect’s absence in psychology: Prevalence and temporal analyses. ",
      "<em>Royal Society Open Science</em>, 12(3), 242167. ",
      "<a href='https://doi.org/10.1098/rsos.242167' target='_blank'>https://doi.org/10.1098/rsos.242167</a><br><br>",
      "For educational material on preventing the misinterpretation of p values, see: <a href='https://lakens.github.io/statistical_inferences/01-pvalue.html#sec-misconception1' target='_blank'>https://lakens.github.io/statistical_inferences</a>."
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
    
    
    # report text 
    
    report <- sprintf(
      "%s\n\n#### The Following Sentences Contain Non-Significant P Values\n\n%s\n\n%s",
      module_output, sentences_block, guidance_block
    )
  }
  
  
  # return a list ----
  list(
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report
  )
}
