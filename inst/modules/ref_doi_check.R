#' DOI Check
#'
#' @description
#' This module checks references for missing DOIs.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param crossref_min_score The minimum score to return a DOI match from `crossref_query()`
#'
#' @returns a list
ref_doi_check <- function(paper, crossref_min_score = 50) {
  # for testing: paper <- psychsci[[109]]

  # table ----
  bib <- concat_tables(paper, "bib")

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      summary_text = "We found no references"
    )
    return(norefs)
  }

  # get DOIs
  missing_dois <- is.na(bib$doi)
  table <- crossref_query(bib$ref[missing_dois], crossref_min_score)

  table$ref <- format_ref(bib$ref[missing_dois])
  table$id <- bib$id[missing_dois]
  table$xref_id <- bib$xref_id[missing_dois]

  # missing/mismatched DOIs
  table$doi_found <- !is.na(table$DOI)

  # traffic_light ----
  tl <- "green"
  if (any(table$doi_found)) {
    tl <- "yellow"
  }

  # summary_table ----
  summary_table <- dplyr::summarise(table, .by = id,
                   refs_checked = sum(!is.na(ref)),
                   doi_found = sum(doi_found))

  # summary_text
  summary_text <- sprintf(
    "We checked %d reference%s in CrossRef and found %d missing DOI%s",
    nrow(table),
    nrow(table) |> plural(),
    sum(table$doi_found, na.rm = TRUE),
    sum(table$doi_found, na.rm = TRUE) |> plural()
  )

  guidance <- "Double check any references listed in the tables below. The match score gives an indication of how good the match was. Many books do not have a DOI or are not listed in CrossRef. Garbled references are usually a result of poor parsing of the paper by grobid; we are working on more accurate alternatives."

  if (tl == "green") guidance <- ""

  # report ----

  ## found table ----

  if (any(table$doi_found)) {
    found_table <- table[table$doi_found, c("DOI", "score", "ref"), drop = FALSE]
    found_table$score <- round(found_table$score)
    found_table$DOI <- link(found_table$DOI, type = "doi")
    names(found_table) <- c("Found DOI", "Match Score", "Original Reference")
  } else {
    found_table <- NULL
  }


  report <- c(
    summary_text,
    guidance,
    scroll_table(found_table)
  )


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


