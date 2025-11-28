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
    
    # Create hyperlinks for each URL
    links <- sprintf("<a href='%s' target='_blank'>%s</a><br>", table$text, table$text)
    
    # Combine into one string
    module_output <- sprintf(
      "We found %d preregistration(s) from AsPredicted.\n\nMeta-scientific research has shown that deviations from preregistrations are often not reported or checked, and that the most common deviations concern the sample size. We recommend manually checking the full preregistration at the link(s) below, and have provided the preregistered sample size.\n\n%s",
      nrow(table),
      paste(links, collapse = "\n")
    )
    
    issues_found <- sprintf(
      "\n\n#### Preregistered Sample Size\n\n%s",
      paste(sprintf("**%s**", table$AP_sample_size), collapse = "\n")
    )
    
    guidance <- paste0(
      "For metascientific articles demonstrating the rate of deviationsfrom preregistrations, see:<br><br>",
      "van den Akker, O. R., Bakker, M., van Assen, M. A. L. M., Pennington, C. R., Verweij, L., Elsherif, M. M., Claesen, A., Gaillard, S. D. M., Yeung, S. K., Frankenberger, J.-L., Krautter, K., Cockcroft, J. P., Kreuer, K. S., Evans, T. R., Heppel, F. M., Schoch, S. F., Korbmacher, M., Yamada, Y., Albayrak-Aydemir, N., … Wicherts, J. M. (2024). The potential of preregistration in psychology: Assessing preregistration producibility and preregistration-study consistency. Psychological Methods. ",
      "<a href='https://doi.org/10.1037/met0000687' target='_blank'>https://doi.org/10.1037/met0000687</a> <br>",
      "For educational material on how to report deviations from preregistrations, see:<br>",
      "Lakens, D. (2024). When and How to Deviate From a Preregistration. Collabra: Psychology, 10(1), 117094. ",
      "<a href='https://doi.org/10.1525/collabra.117094' target='_blank'>https://doi.org/10.1525/collabra.117094</a> <br><br> #### Full preregistration:"
    )
    # Select columns starting with "AP_"
    prereg_table <- dplyr::select(table, dplyr::starts_with("AP_"))
    # Transpose the selected data
    prereg_table <- t(prereg_table)
    colnames(prereg_table) <- c("Answer")
    # report text 
    prereg_table <- paste(knitr::kable(prereg_table, format = "markdown"), collapse = "\n")
    
    report <- sprintf(
      "%s\n\n%s\n\n#### Guidance\n\n%s\n\n%s",
      module_output,
      issues_found,
      guidance,
      prereg_table
    )
    
  }

  # return a list ----
  list(
    table = table,
    traffic_light = tl,
    report = report
  )
}
