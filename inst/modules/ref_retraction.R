#' RetractionWatch
#'
#' @description
#' This module checks references and warns for citations in the RetractionWatch Database.
#'
#' @details
#' The RetractionWatch Check module compares the reference list against studies in the RetractionWatch database based on the DOI. If a study in the database is found, a reminder is provided that the study was retracted, has an expression of concern, or a correction.
#'
#' The module requires that the reference has a DOI: the DOI in the paper's reference list, or one looked up with `add_bib_match()` before the module runs (see `ref_table()`). References without a DOI are not checked.
#'
#' It is possible the authors are already aware that a study was retracted, but the module can't evaluate this.
#'
#' The database can be manually updated with the rw_update function. For more information, see https://gitlab.com/crossref/retraction-watch-data.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
ref_retraction <- function(paper) {
  # for testing: paper <- demopaper()

  # table ----
  bib <- ref_table(paper) |>
    dplyr::filter(!is.na(doi), doi != "")

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      summary_text = "We found no references with DOIs"
    )
    return(norefs)
  }

  ## join to rw table
  # DOIs compared in lower case: a fifth of the RetractionWatch DOIs have
  # capitals (the Wakefield paper, 10.1016/S0140-6736(97)11096-0), while
  # reference DOIs are lower case, so they were never flagged (U157). A DOI
  # listed under two spellings gets both notices; the reference keeps its DOI.
  rw_lower <- rw()
  rw_lower$.doi_key <- tolower(rw_lower$doi)
  rw_lower <- dplyr::summarise(
    rw_lower, .by = ".doi_key",
    retractionwatch = paste(unique(unlist(strsplit(retractionwatch, ";"))), collapse = ";"))
  bib$.doi_key <- tolower(bib$doi)
  table <- dplyr::inner_join(bib, rw_lower, by = ".doi_key")
  table$.doi_key <- NULL
  bib$.doi_key <- NULL

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # summary_table ----
  summary_table <- dplyr::summarise(
    table,
    .by = "paper_id",
    retractionwatch = sum(!is.na(retractionwatch)),
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- "No citations to articles in the RetractionWatch database were found."
    report <- sprintf(
      "We checked %d reference%s with DOIs. %s",   # "1 references" (U82)
      sum(!is.na(bib$doi)), plural(sum(!is.na(bib$doi))), summary_text
    )
  } else {
    ## sumary_text ----
    summary_text <- sprintf(
      "You cited %d article%s in the RetractionWatch database.",
      nrow(table),
      plural(nrow(table))
    )

    ## report_text ----
    n_doi <- sum(!is.na(bib$doi))
    report_text <- sprintf(
      "We checked %d reference%s with DOIs. %s\n\nCheck if you are aware of the retraction, correction, or expression of concern for each reference, and consider whether it still supports your claims.",
      n_doi, plural(n_doi), summary_text
    )

    ## report_table ----
    report_table <- table[, c("text", "retractionwatch")]
    names(report_table) <- c("Reference", "RW Type")

    ## report ----
    report <- c(report_text, scroll_table(report_table))
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
