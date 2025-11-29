#' Preregistration Check
#' 
#' @description
#' Retrieve information from preregistrations, make then easier to check. 
#'
#' @author Daniel Lakens
#' @author Lisa DeBruine
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, summary, traffic light, and report text
prereg_check <- function(paper, ...) {
  # paper <- psychsci[[218]] # to test

  links_ap <- aspredicted_links(paper)
  table <- aspredicted_retrieve(links_ap, id_col = 1)
  
  # traffic light ----
  tl <- "yellow"

  if (nrow(table) == 0) {
    report <- "We detected no links to preregistrations."
  } else {
    
    # I turned each preregistration URL into a clickable hyperlink -> easier than copying the link and pasting it in a browser if the user wants to access the link 
    links <- sprintf("<a href='%s' target='_blank'>%s</a><br>", table$text, table$text)
    
    # Here I combine the count, text, and the hyperlink into one message so the user first sees the overview and then the URLs
    module_output <- sprintf(
      "We found <strong><span style='color:#006400;'>%d</span></strong> preregistration(s) from AsPredicted.\n\nMeta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link(s) below. If you check one aspect of the preregistration, make it the preregistered sample size.\n\n%s",
      nrow(table),
      paste(links, collapse = "\n")
    )
    
    # I show each preregistered sample size as a separate bullet with light borders so the different entries are visually separated but still compact
    ss_items <- paste(
      sprintf(
        "<li style='border-bottom:1px solid #ddd; padding-bottom:4px; margin-bottom:4px;'>%s</li>",
        table$AP_sample_size
      ),
      collapse = "\n"
    )
    
    # I placed the sample size inside a scrollable box!
    ss_box <- paste0(
      "<div style='border:1px solid #ccc; padding:10px; ",
      "max-height:250px; overflow-y:auto; background-color:#f9f9f9; ",
      "margin-top:5px; margin-bottom:5px;'>",
      "<ul style='list-style-type: circle; padding-left:20px; margin:0;'>",
      ss_items,
      "</ul>",
      "</div>"
    )
    
    # Here I wrapped the preregistration sample sizes in a collapsible <details> block -> Users can expand when they need to do so (tidy report)
    ss_block <- paste0(
      "<strong><span style='font-size:20px; color:#006400;'>Preregistered Sample Size</span></strong>",
      "</summary>",
      "<div style='margin-top:8px;'>",
      ss_box,
      "</div>",
      "</details>"
    )
    
    guidance <- paste0(
      "<ul style='list-style-type: circle; padding-left: 20px;'>",
      
      "<li><strong>For metascientific work on preregistration deviations</strong>:<br>",
      "van den Akker, O. R., Bakker, M., van Assen, M. A. L. M., Pennington, C. R., Verweij, L., Elsherif, M. M., Claesen, A., Gaillard, S. D. M., Yeung, S. K., Frankenberger, J.-L., Krautter, K., Cockcroft, J. P., Kreuer, K. S., Evans, T. R., Heppel, F. M., Schoch, S. F., Korbmacher, M., Yamada, Y., Albayrak-Aydemir, N., … Wicherts, J. M. (2024). ",
      "The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency. <em>Psychological Methods</em>. ",
      "<a href='https://doi.org/10.1037/met0000687' target='_blank'>https://doi.org/10.1037/met0000687</a>",
      "</li><br>",
      
      "<li><strong>For educational material on reporting deviations from preregistrations</strong>:<br>",
      "Lakens, D. (2024). When and How to Deviate From a Preregistration. <em>Collabra: Psychology</em>, 10(1), 117094. ",
      "<a href='https://doi.org/10.1525/collabra.117094' target='_blank'>https://doi.org/10.1525/collabra.117094</a>",
      "</li>",
      
      "</ul>"
    )
    
    # I made the guidance collapsible as well! 
    guidance_block <- paste0(
      "<details style='display:block; margin-top:15px;'>",
      "<summary style='cursor:pointer; margin:0; padding:0;'>",
      "<strong><span style='font-size:20px; color:#006400;'>Learn More</span></strong>",
      "</summary>",
      "<div style='margin-top:8px;'>",
      guidance,
      "</div>",
      "</details>"
    )
    
    # Report text 
    detailed_table_block <- ""
    if (nrow(table) > 0) {
      
      # Select columns starting with "AP_"
      prereg_table <- dplyr::select(table, dplyr::starts_with("AP_"))
      # Transpose the selected data
      prereg_table <- t(prereg_table)
      colnames(prereg_table) <- c("Answer")
      # report text 
      prereg_table <- paste(knitr::kable(prereg_table, format = "html"), collapse = "\n")
      
      # I put the table in a scrollable <details> block -> Users can expand it when they want
      detailed_table_block <- paste0(
        "<details style='display:block; margin-top:15px;'>",
        "<summary style='cursor:pointer; margin:0; padding:0;'>",
        "<strong><span style='font-size:20px; color:#006400;'>Complete Preregistration Table</span></strong>",
        "</summary>",
        "<div style='margin-top:10px; max-height:350px; overflow:auto; ",
        "border:1px solid #999; padding:5px; background-color:#ffffff;'>",
        prereg_table,
        "</div>",
        "</details>"
      )
    }
  }
    
    # Bringing everything together here into one HTML report string
    report <- paste0(
      module_output,
      "\n\n",
      ss_block,
      "\n\n",
      if (nzchar(detailed_table_block)) paste0("\n\n", detailed_table_block) else "",
      "\n\n", 
      guidance_block
    )

  # return a list ----
  list(
    traffic_light = tl,
    report = report
  )
}