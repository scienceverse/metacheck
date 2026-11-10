#' PubPeer Comments
#'
#' @description
#' This module checks references and warns for citations that have comments on pubpeer (excluding Statcheck comments).
#'
#' @details
#' The PubPeer module uses the PubPeer API to check for each reference that has a DOI whether there are comments on the post-publication peer review platform. If comments are found, a link to the comments is provided. Comments by ‘Statcheck’ on PubPeer are ignored, see https://retractionwatch.com/2016/09/02/heres-why-more-than-50000-psychology-studies-are-about-to-have-pubpeer-entries/.
#'
#' The module requires that the reference has a DOI: the DOI in the paper's reference list, or one looked up with `add_bib_match()` before the module runs (see `ref_table()`). References without a DOI are not checked.
#'
#' For more information, see [PubPeer](https://www.pubpeer.com/static/about).
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
#' @returns a list
ref_pubpeer <- function(paper) {
  # create table ----
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

  ## join to  pubpeer ----
  pp <- pubpeer_comments(bib$doi)
  # NULL when PubPeer answered with an error: the join below stopped on it
  # ("`x` and `y` must share the same src", U23)
  if (is.null(pp)) {
    return(list(
      traffic_light = "fail",
      summary_text = "PubPeer could not be reached (the request failed), so the references were not checked for comments."
    ))
  }
  pp <- pp[pp$total_comments > 0 & pp$users != "Statcheck", ]
  # pubpeer_comments() gives one row per DOI looked up, so a DOI cited three
  # times joined to three rows each (9 rows); each commented DOI counts once
  # per paper (U118)
  pp <- pp[!duplicated(pp$doi), , drop = FALSE]
  if (!"url" %in% names(pp)) pp$url <- rep(NA_character_, nrow(pp))
  table <- dplyr::inner_join(bib, pp, by = "doi")
  table <- table[!duplicated(table[, c("paper_id", "doi")]), , drop = FALSE]

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # summary_table ----
  summary_table <- dplyr::summarise(
    table,
    .by = "paper_id",
    pubpeer_comments = sum(total_comments, na.rm = TRUE)
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- "No references with comments in PubPeer were found."
    report <- sprintf(
      "We checked %d reference%s with DOIs. %s",   # "1 references" (U82)
      sum(!is.na(bib$doi)), plural(sum(!is.na(bib$doi))), summary_text
    )
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
    # every commented reference, with a link where PubPeer gives one: rows
    # without a url were left out, and without a url column this stopped (U118)
    cols <- c("text", "total_comments", "url")
    report_table <- table[, cols]
    report_table$url <- ifelse(is.na(report_table$url), "",
                               link(report_table$url, "link"))
    names(report_table) <- c("Reference", "Comments", "PubPeer Link")

    ## report ----
    report <- c(report_text, scroll_table(report_table, maxrows = 10))
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
