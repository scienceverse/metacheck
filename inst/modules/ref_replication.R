#' Replication Check
#'
#' @description
#' This module checks references and warns for citations of original studies for which replication or reproduction studies exist in the FLoRA database.
#'
#' @details
#' The Replication Check module compares the reference list against studies in the FLoRA (FORRT Library of Replication Attempts) database based on the DOI. If a study in the database is found, a reminder is provided that a replication or reproduction of the original study exists, and should be cited (currently, a warning is provided regardless of whether the replication/reproduction study is already cited).
#'
#' The module requires that the reference has a DOI. If you run the ref_doi_check module in a pipeline before this, it will use the enhanced DOI list from that module, otherwise it will only run on references with existing DOIs.
#'
#' It is possible the original study was cited for other reasons than the empirical claim tested, or that the replication/reproduction in the FLoRA database is for only one of the studies in the paper, and not the study the authors discuss.
#'
#' The database can be manually updated with the `FLoRA_update()` function. For more information, see <https://forrt.org/FLoRA/>.
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
#' @param show_outcomes logical. If TRUE, include replication outcome
#'   and type in the report table. Default is FALSE.
#'
#' @returns a list
ref_replication <- function(paper, show_outcomes = FALSE) {
  # for testing: paper <- psychsci[[109]]

  # table ----
  bib <- concat_tables(paper, "bib")[, c("id", "xref_id", "doi", "ref")]
  missing_doi <- get_prev_outputs("ref_doi_check", "table")

  if (!is.null(missing_doi)) {
    md <- missing_doi[, c("id", "xref_id", "DOI")]
    bib <- dplyr::left_join(bib, md, by = c("id", "xref_id"))
    is_missing <- is.na(bib$doi)
    bib$doi[is_missing] <- bib$DOI[is_missing]
    bib$DOI <- NULL
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
  table <- dplyr::inner_join(bib, flora, by = "doi")

  ## remove rows that are already cited (by DOI)
  has_rep_doi <- !is.na(table$replication_doi) & table$replication_doi != ""
  already_cited <- has_rep_doi & (table$replication_doi %in% bib$doi)
  table <- table[!already_cited, ]

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # summary_table ----
  summary_table <- dplyr::summarise(
    table,
    .by = "id",
    replications = dplyr::n(),
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- "No citations to articles in the FLoRA database were found."
    report <- sprintf(
      "We checked %d references with DOIs. %s",
      sum(!is.na(bib$doi)), summary_text
    )
  } else {
    ## sumary_text ----
    n_replications <- sum(table$replication_type == "replication", na.rm = TRUE)
    n_reproductions <- sum(table$replication_type == "reproduction", na.rm = TRUE)

    if (n_reproductions == 0) {
      summary_text <- sprintf(
        "You cited %d article%s with replication%s in the FLoRA database.",
        n_replications, plural(n_replications), plural(n_replications)
      )
    } else if (n_replications == 0) {
      summary_text <- sprintf(
        "You cited %d article%s with reproduction%s in the FLoRA database.",
        n_reproductions, plural(n_reproductions), plural(n_reproductions)
      )
    } else {
      summary_text <- sprintf(
        "You cited %d article%s with %d replication%s and %d reproduction%s in the FLoRA database.",
        nrow(table), plural(nrow(table)),
        n_replications, plural(n_replications),
        n_reproductions, plural(n_reproductions)
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
    report_table <- table[, c("ref", "replication_ref")]
    report_table$ref <- format_ref(report_table$ref)

    # Create links using DOI if available, otherwise use URL
    has_doi <- !is.na(table$replication_doi) & table$replication_doi != ""
    replication_links <- ifelse(
      has_doi,
      link(table$replication_doi, type = "doi"),
      link(table$replication_url, type = "url")
    )

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
