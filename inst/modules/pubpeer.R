#' Check PubPeer Comments
#'
#' @description
#' This module checks references and warns for citations that have comments on pubpeer (excluding Statcheck comments).
#'
#' @details
#' If you run the reference_check module in a pipeline before this, it will use the enhanced DOI list from that module, otherwise it will only run on references with existing DOIs.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report text
#'
#' @examples
#' module_run(psychsci[[129]], "pubpeer")
pubpeer <- function(paper) {
  # for testing: paper <- psychsci[[109]]

  # create table ----
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

  ## join to  pubpeer ----
  pp <- pubpeer_comments(bib$doi)
  table <- dplyr::inner_join(bib, pp, by = 'doi')

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # summary_table ----
  summary_table <- dplyr::summarise(
    table, .by = "id",
    pubpeer_comments = sum(total_comments, na.rm = TRUE)
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- "No references with comments in PubPeer were found."
    report <- sprintf("We checked %d references with DOIs. %s",
                      sum(!is.na(bib$doi)), summary_text)
  } else {
    ## summary_text ----
    n <- sum(table$total_comments > 0, na.rm = TRUE)
    summary_text <- sprintf(
      "You cited %d reference%s with comments in PubPeer.",
      n, plural(n)
    )

    ## report_text ----
    n_doi <- sum(!is.na(bib$doi))
    report_text <- sprintf(
      "We checked %d reference%s with DOIs. %s\n\nPubpeer is a platform for post-publication peer review. We have filtered out Pubpeer comments by 'Statcheck'. You can check out the comments by visiting the URLs below:",
      n_doi, plural(n_doi), summary_text
    )

    ## report_table ----
    rows <- !is.na(table$url) & table$users != "Statcheck"
    cols <- c("ref", "total_comments", "url")
    report_table <- table[rows, cols]
    report_table$ref <- format_ref(report_table$ref)
    report_table$url <- link(report_table$url, "link")
    names(report_table) <- c("Reference", "Comments", "PubPeer Link")

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


