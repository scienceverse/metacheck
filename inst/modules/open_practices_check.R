#' Open Practices Check
#'
#' @description
#' This module incorporated ODDPub into metacheck. ODDPub is a text mining algorithm that detects which publications disseminated Open Data or Open Code together with the publication.
#'
#' @keywords general
#'
#' @author Lisa DeBruine (\email{lisa.debruin@glasgow.ac.uk}) and Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#' @import oddpub
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report text
#'
#' @examples
#' # devtools::install_github("quest-bih/oddpub")
#' module_run(psychsci[[129]], "open_data_code_check")
open_data_code_check <- function(paper) {
  # for testing: paper <- psychsci[[110]]
  paper_oddpub <- list()
  # Extract the full text
  full_text <- paper[["full_text"]][["text"]]
  #Check if full_text is NULL or empty, happens in correction notices.
  if (is.null(full_text) || length(full_text) == 0) {
    report <- "There was no text to search through"
    return(report)
  }
  # Assign to new list
  paper_oddpub[[paper$id]] <- full_text

  open_data_results <- oddpub::open_data_search(paper_oddpub, screen_das = "extra")

  # Create report sentences
  data_sentence <- ifelse(
    open_data_results$is_open_data & nzchar(open_data_results$open_data_statements),
    paste0('Data was openly shared for this article, based on the sentence "', open_data_results$open_data_statements, '".'),
    ifelse(open_data_results$is_open_data,
           "Data was openly shared for this article.",
           "Data was not openly shared for this article. which could be because there is no data related to this article. If there is data, please consider sharing it in a repository.")
  )

  code_sentence <- ifelse(
    open_data_results$is_open_code & nzchar(open_data_results$open_code_statements),
    paste0('Code was openly shared for this article, based on the sentence "', open_data_results$open_code_statements, '".'),
    ifelse(open_data_results$is_open_code,
           "Code was openly shared for this article.",
           "Code was not openly shared for this article, which could be because there is no code related to this article. If there is code, please consider sharing it in a repository, and run metacheck again to benefit from the automated code check. ")
  )
  # Guidance text
  guidance <- c(
    "Data and code sharing was determined using ODDPub, a text mining algorithm.",
    "Riedel, N., Kip, M., & Bobrov, E. (2020). ODDPub – a Text-Mining Algorithm to Detect Data Sharing in Biomedical Publications. Data Science Journal, 19(1). <a href=\"https://doi.org/10.5334/dsj-2020-042\" target=\"_blank\">https://doi.org/10.5334/dsj-2020-042</a>"
  )

  report <- c(data_sentence, code_sentence, collapse_section(guidance))
  if (open_data_results$is_open_data == TRUE & open_data_results$is_open_code == TRUE){
    summary_text <- "Data and code are shared."
    tl <- "green"
  } else if (open_data_results$is_open_data == TRUE & open_data_results$is_open_code == FALSE){
    summary_text <- "Data are shared."
    tl <- "info"
  } else if (open_data_results$is_open_data == FALSE & open_data_results$is_open_code == TRUE){
    summary_text <- "Code is shared."
    tl <- "info"
  } else {
    summary_text <- "Neiter data nor code are shared."
    tl <- "red"
  }


  summary_table <- open_data_results
  names(summary_table)[1] <- "id"

  # return a list ----
  list(
    table = open_data_results,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
