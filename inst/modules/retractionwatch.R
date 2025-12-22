#' RetractionWatch
#'
#' @description
#' This module checks references and warns for citations in the RetractionWatch Database.
#'
#' @details
#' If you run the reference_check module in a pipeline before this, it will use the enhanced DOI list from that module, otherwise it will only run on references with existing DOIs.
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
#' @returns report text
#'
#' @examples
#' module_run(psychsci[[129]], "retractionwatch")
retractionwatch <- function(paper) {
  # for testing: paper <- read(demoxml())

  # table ----
  bib <- concat_tables(paper, "bib")[, c("id", "doi", "ref")]
  better_doi <- get_prev_outputs("reference_check", "table")
  if (!is.null(better_doi)) {
    bib$doi <- better_doi$DOI
  }

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      report = "We found no references",
      summary_text = "We found no references"
    )
    return(norefs)
  }

  ## join to rw table
  table <- dplyr::inner_join(bib, rw(), by = 'doi')

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # summary_table ----
  summary_table <- dplyr::summarise(
    table, .by = "id",
    retractionwatch = sum(!is.na(retractionwatch)),
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- "No citations to articles in the RetractionWatch database were found."
    report <- sprintf("We checked %d references with DOIs. %s",
                      sum(!is.na(bib$doi)), summary_text)
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
      "We checked %d reference%s with DOIs. %s\n\nCheck if you are aware of the replication studies, and cite them where appropriate.",
      n_doi, plural(n_doi), summary_text
    )

    ## report_table ----
    report_table <- table[, c("ref", "retractionwatch")]
    report_table$ref <- format_ref(report_table$ref)
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


