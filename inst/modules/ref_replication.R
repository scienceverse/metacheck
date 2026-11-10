#' Replication Check
#'
#' @description
#' This module checks references and warns for citations of original studies for which replication or reproduction studies exist in the FLoRA database.
#'
#' @details
#' The Replication Check module compares the reference list against studies in the FLoRA (FORRT Library of Replication Attempts) database based on the DOI. If a study in the database is found, a reminder is provided that a replication or reproduction of the original study exists, and should be cited. No reminder is given when the paper already cites that replication or reproduction (matched by its DOI).
#'
#' The module requires that the reference has a DOI: the DOI in the paper's reference list, or one looked up with `add_bib_match()` before the module runs (see `ref_table()`). References without a DOI are not checked.
#'
#' It is possible the original study was cited for other reasons than the empirical claim tested, or that the replication/reproduction in the FLoRA database is for only one of the studies in the paper, and not the study the authors discuss.
#'
#' The database can be manually updated with the `FLoRA_update()` function. For more information, see <https://forrt.org/FLoRA/>.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#' @author Lukas Wallrich (\email{lukas.wallrich@gmail.com})
#'
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param show_outcomes logical. If TRUE, include replication outcome
#'   and type in the report table. Default is FALSE.
#'
#' @returns a list
ref_replication <- function(paper, show_outcomes = FALSE) {
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

  ## join to flora table
  flora <- FLoRA() |>
    dplyr::select(
      doi = doi_o,
      replication_ref = apa_ref_r,
      replication_doi = doi_r,
      replication_url = url_r,
      replication_outcome = outcome,
      replication_type = type
    )

  # DOIs compared in lower case, as DOIs are; the reference keeps its own (U157)
  bib$.doi_key <- tolower(bib$doi)
  flora$.doi_key <- tolower(flora$doi)
  flora$doi <- NULL
  table <- dplyr::inner_join(bib, flora, by = ".doi_key")
  table$.doi_key <- NULL
  bib$.doi_key <- NULL

  ## remove rows that are already cited (by DOI), by the same paper: checked
  ## against every paper's references, paper B citing a replication hid
  ## paper A's citation of the original (U116)
  has_rep_doi <- !is.na(table$replication_doi) & table$replication_doi != ""
  already_cited <- has_rep_doi &
    paste(table$paper_id, tolower(table$replication_doi)) %in%
    paste(bib$paper_id, tolower(bib$doi))
  table <- table[!already_cited, ]

  # Remove trailing URLs from reference text to avoid duplication with link
  table$replication_ref <- trimws(gsub(
    "https?://[^[:space:]]+$", "",
    table$replication_ref, ignore.case = TRUE
  ))

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # summary_table ----
  summary_table <- dplyr::summarise(
    table,
    .by = "paper_id",
    replications = dplyr::n(),
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- "No citations to articles in the FLoRA database were found."
    report <- sprintf(
      "We checked %d reference%s with DOIs. %s",   # "1 references" (U82)
      sum(!is.na(bib$doi)), plural(sum(!is.na(bib$doi))), summary_text
    )
  } else {
    ## summary_text ----
    n_replications <- sum(table$replication_type == "replication", na.rm = TRUE)
    n_reproductions <- sum(table$replication_type == "reproduction", na.rm = TRUE)
    n_originals <- dplyr::n_distinct(table$doi)

    if (n_reproductions == 0) {
      summary_text <- sprintf(
        "We found %d replication%s for %d original%s you cited.",
        n_replications, plural(n_replications), n_originals, plural(n_originals)
      )
    } else if (n_replications == 0) {
      summary_text <- sprintf(
        "We found %d reproduction%s for %d original%s you cited.",
        n_reproductions, plural(n_reproductions), n_originals, plural(n_originals)
      )
    } else {
      summary_text <- sprintf(
        "We found %d replication%s and %d reproduction%s for %d original%s you cited.",
        n_replications, plural(n_replications),
        n_reproductions, plural(n_reproductions),
        n_originals, plural(n_originals)
      )
    }

    ## report_text ----
    n_doi <- sum(!is.na(bib$doi))
    has_both_types <- n_replications > 0 && n_reproductions > 0

    if (n_reproductions == 0) {
      study_type_text <- "replication studies"
      col_header <- "Replication"
    } else if (n_replications == 0) {
      study_type_text <- "reproduction studies"
      col_header <- "Reproduction"
    } else {
      study_type_text <- "replication/reproduction studies"
      col_header <- "Replication/Reproduction"
    }

    report_text <- sprintf(
      "We checked %d reference%s with DOIs. %s\n\nCheck if you are aware of the %s, and cite them where appropriate.",
      n_doi, plural(n_doi), summary_text, study_type_text
    )

    ## report_table ----
    report_table <- table[, c("text", "replication_ref")]

    # Create links using DOI if available, otherwise use URL
    has_doi <- !is.na(table$replication_doi) & table$replication_doi != ""
    # Format DOI links with doi: prefix to match format_ref() output
    bare_doi <- gsub("^https?://doi.org/", "", table$replication_doi)
    doi_links <- link(
      url = paste0("https://doi.org/", bare_doi),
      text = paste0("doi:", bare_doi)
    )
    replication_links <- ifelse(has_doi, doi_links, link(table$replication_url))

    # Only label entries with type if both replications and reproductions are present
    if (has_both_types) {
      type_label <- tools::toTitleCase(table$replication_type)
      report_table$replication_ref <- sprintf(
        "<b>[%s]</b> %s %s",
        type_label,
        table$replication_ref,
        replication_links
      )
    } else {
      report_table$replication_ref <- sprintf(
        "%s %s",
        table$replication_ref,
        replication_links
      )
    }
    names(report_table) <- c("Reference", col_header)

    if (show_outcomes) {
      report_table$Outcome <- table$replication_outcome
      report_table$Type <- table$replication_type
    }

    ## report ----
    colwidths <- if (show_outcomes) c(.3, .4, .15, .15) else c(.5, .5)
    report <- c(report_text, scroll_table(report_table, colwidths = colwidths))
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
