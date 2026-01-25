#' Open Practices Check
#'
#' @description
#' This module incorporates ODDPub into metacheck. ODDPub is a text mining algorithm that detects which publications disseminated Open Data or Open Code together with the publication.
#'
#' @details
#' The Open Practices Check runs Open Data Detection in Publications (ODDPub). ODDPub searches for text expressions that indicate that an article shared Open Data or Open Code together with the publication. More information on the package can be found at <https://github.com/quest-bih/oddpub>. The module only returns whether open data and code is found (the original package offers more fine-grained results). The tool was validated in the biomedical literature, see <https://osf.io/yv5rx/>.
#'
#' ODDPub was developed by Nico Riedel, Vladislav Nachev, Miriam Kip, and Evgeny Bobrov at the QUEST Center for Transforming Biomedical Research, Berlin Institute of Health. <https://doi.org/10.5334/dsj-2020-042>
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

  # format text for oddpub as vectors of sentences for each paper
  full_text <- search_text(paper)

  # Check if full_text is NULL or empty, happens in correction notices.
  if (is.null(full_text) || nrow(full_text) == 0 || all(full_text$text == "")) {
    report <- list(
      traffic_light = "na",
      summary_text = "There was no text to search through"
    )
    return(report)
  }

  ids <- unique(full_text$id)
  paper_oddpub <- lapply(ids, \(id) {
    full_text[full_text$id == id, ]$text
  })
  names(paper_oddpub) <- ids

  # categorise with oddpub
  oddpub_results <- suppressMessages(
    oddpub::open_data_search(paper_oddpub, screen_das = "extra")
  )

  # table ----
  # put in a sensible naming scheme and order
  table <- dplyr::select(oddpub_results,
    id = article,
    data_open = is_open_data,
    data_statements = open_data_statements,
    data_reuse = is_reuse,
    data_category = open_data_category,
    data_das = is_open_data_das,
    das = das,
    code_open = is_open_code,
    code_statements = open_code_statements,
    code_reuse = is_code_reuse,
    code_supplement = is_code_supplement,
    code_cas = is_open_code_cas,
    cas = cas
  )

  # Oddpub can return the same info multiple times, reduce text
  table$data_statements <- strsplit(table$data_statements, ";") |>
    sapply(\(x) {
      trimws(x) |>
        unique() |>
        paste(collapse = "\n\n")
    })

  table$code_statements <- strsplit(table$code_statements, ";") |>
    sapply(\(x) {
      trimws(x) |>
        unique() |>
        paste(collapse = "\n\n")
    })

  # traffic_light ----
  # summary_text ----
  if (nrow(table) > 1) {
    tl <- "info"
    # summary for a paperlist
    summary_text <- sprintf(
      "%d papers shared both data and code, %d only data, %d only code, and %d neither.",
      sum(table$data_open & table$code_open),
      sum(table$data_open & !table$code_open),
      sum(!table$data_open & table$code_open),
      sum(!table$data_open & !table$code_open)
    )
  } else {
    # summary for a single paper
    if (table$data_open == TRUE &
      table$code_open == TRUE) {
      summary_text <- "Shared data and code detected."
      tl <- "green"
    } else if (table$data_open == TRUE &
      table$code_open == FALSE) {
      summary_text <- "Shared data detected."
      tl <- "yellow"
    } else if (table$data_open == FALSE &
      table$code_open == TRUE) {
      summary_text <- "Shared code detected."
      tl <- "yellow"
    } else {
      summary_text <- "Neither shared data nor code detected."
      tl <- "red"
    }
  }

  # summary_table ----
  summary_table <- table

  # report ----
  data_report <- NULL
  code_report <- NULL

  if (nrow(table) == 1) {
    # data_report
    if (!table$data_open) {
      data_report <- "We did not detect open sharing of data, which could be because there is no data related to this article, or the repository is not reconized by ODDPub. If there is data, please consider sharing it in a repository."
    } else if (nzchar(table$data_statements)) {
      data_report <- sprintf(
        "Data was openly shared for this article, based on the following text:\n\n> %s",
        gsub("\n\n", "\n\n> ", table$data_statements)
      )
    } else {
      data_report <- "Data was openly shared for this article."
    }

    # code_report
    if (!table$code_open) {
      code_report <- "We did not detect open sharing of code, which could be because there is no code related to this article, or the repository is not reconized by ODDPub. If there is code, please consider sharing it in a repository."
    } else if (nzchar(table$code_statements)) {
      code_report <- sprintf(
        "Code was openly shared for this article, based on the following text:\n\n> %s",
        gsub("\n\n", "\n\n> ", table$code_statements)
      )
    } else {
      code_report <- "Code was openly shared for this article."
    }
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

  report <- c(
    data_report,
    code_report
  )
  # collapse_section(guidance))

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
