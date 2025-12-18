#' Reference Check
#'
#' @description
#' This module checks references. It warns for missing DOI's, citations in the RetractionWatch database, citations that have comments on pubpeer (excluding Statcheck comments), and citations of original studies for which replication studies exist in the Replication Database.
#'
#' @keywords reference
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk}) and Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr httr jsonlite
#'
#' @param paper a paper object or paperlist object
#' @param crossref_min_score The minimum score to return a DOI match from `get_doi()`
#'
#' @returns report text
#'
#' @examples
#' module_run(psychsci[[129]], "reference_check")
reference_check <- function(paper, crossref_min_score = 50) {
  # for testing: paper <- psychsci[[109]]

  # create table ----
  table <- concat_tables(paper, "bib")

  # If there are no rows, return immediately
  if (nrow(table) == 0) {
    norefs <- list(
      table = table,
      traffic_light = "na",
      report = "We found no references",
      summary_text = "We found no references"
    )
    return(norefs)
  }

  ## missing DOIs ----
  articles_without_doi <- table |>
    subset(bibtype == "Article") |>
    subset(is.na(doi))
  n_no_doi <- nrow(articles_without_doi)

  if (n_no_doi > 0) {
    message("\nLooking up ", n_no_doi, " missing article DOIs")
    articles_without_doi$crossref_doi <- sapply(articles_without_doi$ref, get_doi, min_score = crossref_min_score)
    table <- dplyr::left_join(table, articles_without_doi,
                             by = names(table))

    updated <- is.na(table$doi) & !is.na(table$crossref_doi)
    table$doi[updated] <- table$crossref_doi[updated]
    table$doi_from_crossref <- 0
    table$doi_from_crossref[updated] <- 1
    message("Added ", sum(updated), " DOIs from CrossRef")
  } else {
    table$doi_from_crossref <- rep(0, nrow(table))
  }

  ## pubpeer ----
  pp <- pubpeer_comments(table$doi)
  if (!is.null(pp)) {
    table$pp_total_comments <- pp$total_comments
    table$pp_url <- pp$url
    table$pp_users <- pp$users
  }

  ## retraction watch ----
  table <- dplyr::left_join(table, rw(), by = 'doi')

  ## replications ----
  fred <- FReD() |>
    dplyr::select(doi = doi_original,
                  replication_ref = ref_replication,
                  replication_doi = doi_replication)
  table <- dplyr::left_join(table, fred, by = "doi")

  ## select only articles and make doi into clickable link
  articles <- table[table$bibtype == "Article", ]
  articles$doi <- link(paste0("https://doi.org/", articles$doi), articles$doi)

  # summary_text ----
  n_doi <- sum(!is.na(articles$doi))
  overall_text <- sprintf("This module only checks references classified as articles. Out of %d reference%s to articles in the reference list, %d %s a DOI.",
      nrow(articles),
      plural(nrow(articles)),
      n_doi,
      plural(n_doi, "has", "have")
  )

  ## missing doi ----
  n_cr <- sum(articles$doi_from_crossref)
  missing_summary <- sprintf("We retrieved %d of %d missing DOI%s from crossref.", n_cr, n_no_doi, plural(n_no_doi))
  missing_text <- sprintf("%s Only missing DOIs with a match score > %d are returned to have high enough accuracy. Double-check any suggested DOIs and check if the remaining missing DOIs are available.", missing_summary, crossref_min_score)
  rows <- articles$doi_from_crossref | is.na(articles$doi)
  missing_table <- articles[rows, c("doi", "ref")]
  names(missing_table) <- c("DOI", "Reference")

  ## PubPeer ----
  rows <- articles$pp_total_comments>0 & articles$pp_users != "Statcheck"
  cols <- c("doi", "ref", "pp_total_comments", "pp_url")
  pubpeer_table <- articles[rows, cols]
  pubpeer_table$pp_url <- link(pubpeer_table$pp_url, "link")
  names(pubpeer_table) <- c("DOI", "Reference", "Comments", "PubPeer Link")
  if (all(is.na(pubpeer_table))) {
    # Keep the same columns, but zero rows
    pubpeer_table <- pubpeer_table[0, , drop = FALSE]
  }

  n_pp <- nrow(pubpeer_table)
  pubpeer_summary <- sprintf(
    "We found %d reference%s with comments on Pubpeer.",
    n_pp, plural(n_pp))

  if (n_pp == 0) {
    pubpeer_text <- pubpeer_summary
  } else {
    pubpeer_text <- sprintf("%s Pubpeer is a platform for post-publication peer review. We have filtered out Pubpeer comments by 'Statcheck'. You can check out the comments by visiting the URLs below:", pubpeer_summary)
  }

  ## replications ----
  rows <- !is.na(articles$replication_doi)
  cols <- c("doi", "replication_ref", "replication_doi")
  fred_table <- articles[rows, cols]
  if(any(rows)) {
    fred_table$replication_doi <- link(paste0("https://doi.org/", fred_table$replication_doi), fred_table$replication_doi)
  }
  names(fred_table) <- c("DOI", "Replication Reference", "Replication DOI")

  if (nrow(fred_table) == 0) {
    fred_summary <- "No citations to studies in the FReD replication database were found."
    fred_text <- fred_summary
  } else {
    fred_summary <- sprintf(
      "You have cited %d article%s for which replication studies exist.",
      nrow(fred_table),
      plural(nrow(fred_table))
    )
    fred_text <- sprintf(
      "%s These replications were listed in the FORRT Replication Database (as of %s). Check if you are aware of the replication studies, and cite them where appropriate.",
      fred_summary, FReD_date()
    )
  }

  ## retractions ----
  rows <- !is.na(articles$retractionwatch)
  cols <- c("doi", "ref", "retractionwatch")
  rw_table <- articles[rows, cols]
  names(rw_table) <- c("DOI", "Reference", "RW Type")

  if (nrow(rw_table) == 0) {
    rw_summary <- "No citations to studies in the Retraction Watch database were found."
    rw_text <- rw_summary
  } else {
    rw_summary <- sprintf(
      "You have cited %d article%s for which entries in the Retraction Watch database  exist.",
      nrow(rw_table),
      plural(nrow(rw_table))
    )
    rw_text <- sprintf(
      "%s These articles were listed in the Retraction Watch database (as of %s). Check if you are aware of the issues, and cite them where appropriate.",
      rw_summary, rw_date()
    )
  }

  # traffic_light ----
  tl <- if (length(missing_table) |
            length(pubpeer_table) |
            length(fred_table) |
            length(rw_table)) "info" else "na"

  # report ----
  report <- c(
    overall_text,
    "#### Missing DOIs",
    missing_text,
    scroll_table(missing_table),
    "#### PubPeer Comments",
    pubpeer_text,
    scroll_table(pubpeer_table),
    "#### Replication Studies",
    fred_text,
    scroll_table(fred_table),
    "#### RetractionWatch",
    rw_text,
    scroll_table(rw_table)
  )

  # summary_table ----
  summary_table <- dplyr::summarise(
    table, .by = "id",
    retraction_watch = sum(!is.na(retractionwatch)),
    replications = sum(!is.na(replication_doi)),
    doi_missing = sum(doi_from_crossref | is.na(doi)),
    pubpeer_comments = sum(pp_total_comments, na.rm = TRUE)
  )

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = paste(missing_summary,
                         pubpeer_summary,
                         fred_summary,
                         rw_summary)
  )
}


