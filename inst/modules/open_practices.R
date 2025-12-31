#' Open Practices Check
#'
#' @description
#' This module incorporates ODDPub into metacheck. ODDPub is a text mining algorithm that detects which publications disseminated Open Data or Open Code together with the publication.
#'
#' @details
#' The Open Practices Check runs Open Data Detection in Publications (ODDPub). ODDPub searches for text expressions that indicate that an article shared Open Data or Open Code together with the publication. More information on the package can be found at <https://github.com/quest-bih/oddpub>. The module only returns whether open data and code is found (the original package offers more fine-grained results). The tool was validated in the biomedical literature, see <https://osf.io/yv5rx/>.
#'
#' ODDPub was developed by Nico Riedel, Vladislav Nachev, Miriam Kip, and Evgeny Bobrov at the QUEST Center for Transforming Biomedical Research, Berlin Institute of Health.
#'
#' It might miss open data and code declarations when the words used in the manuscript are not in the pattern that ODDPub searches for, or when the repositories are not in the ODDpub code (e.g., ResearchBox).
#'
#'
#' @keywords general
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#' @import oddpub
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
open_practices <- function(paper) {
  # devtools::install_github("quest-bih/oddpub")
  full_text <- search_text(paper)
  ids <- unique(full_text$id)
  paper_oddpub <- lapply(ids, \(id) {
    full_text[full_text$id == id, ]$text
  })
  names(paper_oddpub) <- ids

  #Check if full_text is NULL or empty, happens in correction notices.
  if (is.null(full_text) || nrow(full_text) == 0 || all(full_text$text == "")) {
    report <- list(
      traffic_light = "na",
      summary_text = "There was no text to search through"
    )
    return(report)
  }

  # Assign to new list
  open_data_results <- suppressMessages(
    oddpub::open_data_search(paper_oddpub, screen_das = "extra")
  )
  names(open_data_results)[1] <- "id"

  # traffic_light ----
  # summary_text ----
  if (nrow(open_data_results) > 1) {
    tl <- "info"
    # summary for a paperlist
    summary_text <- sprintf("%d papers shared both data and code, %d only data, %d only code, and %d neither.",
                            sum(open_data_results$is_open_data & open_data_results$is_open_code),
                            sum(open_data_results$is_open_data & !open_data_results$is_open_code),
                            sum(!open_data_results$is_open_data & open_data_results$is_open_code),
                            sum(!open_data_results$is_open_data & !open_data_results$is_open_code)
                            )
  } else {
    # summary for a single paper
    if (open_data_results$is_open_data == TRUE &
        open_data_results$is_open_code == TRUE) {
      summary_text <- "Data and code are shared."
      tl <- "green"
    } else if (open_data_results$is_open_data == TRUE &
               open_data_results$is_open_code == FALSE) {
      summary_text <- "Data are shared."
      tl <- "yellow"
    } else if (open_data_results$is_open_data == FALSE &
               open_data_results$is_open_code == TRUE){
      summary_text <- "Code is shared."
      tl <- "yellow"
    } else {
      summary_text <- "Neither data nor code are shared."
      tl <- "red"
    }
  }

  # summary_table ----
  summary_table <- open_data_results

  # report ----
  if (nrow(open_data_results) == 1) {
    # Oddpub can return the same info multiple times, reduce text
    open_data_statement <- strsplit(open_data_results$open_data_statements, ";")[[1]]
    open_data_statement <- trimws(open_data_statement)
    open_data_statement <- unique(open_data_statement)

    open_code_statement <- strsplit(open_data_results$open_code_statements, ";")[[1]]
    open_code_statement <- trimws(open_code_statement)
    open_code_statement <- unique(open_code_statement)

    data_sentence <- ifelse(
      open_data_results$is_open_data & nzchar(open_data_results$open_data_statements),
      paste0('Data was openly shared for this article, based on the sentence "',
             open_data_statement, '".'),
      ifelse(open_data_results$is_open_data,
             "Data was openly shared for this article.",
             "Data was not openly shared for this article. which could be because there is no data related to this article, or the repository is not reconized by ODDPub. If there is data, please consider sharing it in a repository.")
    )

    code_sentence <- ifelse(
      open_data_results$is_open_code & nzchar(open_data_results$open_code_statements),
      paste0('Code was openly shared for this article, based on the sentence "',
             open_code_statement, '".'),
      ifelse(open_data_results$is_open_code,
             "Code was openly shared for this article.",
             "Code was not openly shared for this article, which could be because there is no code related to this article. If there is code, please consider sharing it in a repository, and run metacheck again to benefit from the automated code check. ")
    )
  } else {
    data_sentence <- ""
    code_sentence <- ""
  }

  guidance <- c(
    "Data and code sharing was determined using ODDPub. ODDPub searches for text expressions that indicate that an article shared Open Data or Open Code together with the publication.  More information on the package can be found at <https://github.com/quest-bih/oddpub>. The module only returns whether open data and code is found (the original package offers more fine-grained results). The tool was validated in the biomedical literature, see <https://osf.io/yv5rx/>.",
    bibentry(
      bibtype = "Article",
      author = c(
        person("Nina", "Riedel"),
        person("Mareike", "Kip"),
        person("Evgeny", "Bobrov")
      ),
      year = 2020,
      title = "ODDPub – a Text-Mining Algorithm to Detect Data Sharing in Biomedical Publications",
      journal = "Data Science Journal",
      volume = "19",
      number = "1",
      pages = "42",
      doi = "10.5334/dsj-2020-042"
    ) |> format_ref()
  )

  report <- c(data_sentence,
              code_sentence,
              collapse_section(guidance))

  # return a list ----
  list(
    table = open_data_results,
    summary_table = open_data_results,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
