#' Reference Consistency
#'
#' @description
#' Check if all references are cited and all citations are referenced
#'
#' @details
#' This module is currently under development and should not be relied on until we have increased the accuracy of the reference labeling while importing papers. It has a high false-positive rate because grobid (the PDF-importing tool) tends to miss some references and falsely identify some text as citations.
#'
#' @keywords reference
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
ref_consistency <- function(paper) {
  # detailed table of results ----
  # once per reference: ref_table() repeats a reference that has several
  # bib_match rows (U114)
  bibs <- ref_table(paper) |>
    dplyr::select(paper_id, bib_id, reference = text) |>
    dplyr::distinct()
  xrefs <- paper_table(paper, "xref")
  # bibr 12.x papers cite a reference with a "bib" xref whose target_id is
  # the bib_id (their xref_id is the row's own key). Other papers use "bibr"
  # (Grobid TEI) or "bib" (older bibr exports, whose xref_id is the bib_id):
  # with "bibr" only, every reference of an older bibr export was uncited
  # and the light red (U207)
  v12 <- xrefs$paper_id %in% .bibr12_paper_ids(paper)
  if (any(v12)) xrefs$xref_id[v12] <- xrefs$target_id[v12]
  xrefs <- xrefs[ifelse(v12, xrefs$xref_type %in% "bib",
                        xrefs$xref_type %in% c("bibr", "bib")), ] |>
    dplyr::select(paper_id, bib_id = xref_id, contents, text_id)
  text <- paper_table(paper, "text") |>
    dplyr::select(paper_id, text_id, text)

  # Marked by the side each row comes from: a citation whose bib_id is not in
  # the bibliography (bib_id 99) has a bib_id, so testing is.na(bib_id) never
  # reported it (U115)
  table <- dplyr::full_join(
    dplyr::mutate(bibs, .in_bib = TRUE),
    dplyr::mutate(xrefs, .cited = TRUE),
    by = c("paper_id", "bib_id")
  ) |>
    dplyr::filter(is.na(.cited) | is.na(.in_bib)) |>
    dplyr::left_join(text, by = c("paper_id", "text_id"))
  table$text_id <- NULL
  is_missing <- is.na(table$.in_bib)
  table$.in_bib <- NULL
  table$.cited <- NULL

  # summary_table ----
  # n_missing and n_extra always: they existed only when a row was flagged (U115)
  nbibs <- dplyr::count(bibs, paper_id, name = "n_bib")
  nxrefs <- dplyr::count(xrefs, paper_id, name = "n_xrefs")
  nmiss <- dplyr::count(table[is_missing, , drop = FALSE], paper_id, name = "n_missing")
  nextra <- dplyr::count(table[!is_missing, , drop = FALSE], paper_id, name = "n_extra")
  summary_table <- data.frame(paper_id = paper_id(paper)) |>
    dplyr::left_join(nbibs, by = "paper_id") |>
    dplyr::left_join(nxrefs, by = "paper_id") |>
    dplyr::left_join(nmiss, by = "paper_id") |>
    dplyr::left_join(nextra, by = "paper_id")
  summary_table$n_missing[is.na(summary_table$n_missing)] <- 0L
  summary_table$n_extra[is.na(summary_table$n_extra)] <- 0L

  # traffic light ----
  tl <- dplyr::case_when(
    nrow(bibs) == 0 ~ "na",
    # yellow, not red: in the Meta-Psychology run it was red for 146 of 149 papers,
    # mostly for faults of the PDF conversion (#695, point 2)
    nrow(table) > 0 ~ "yellow",
    .default = "green"
  )

  # report ----
  report <- c(
    yellow = "There are cross-references that are not in the bibliography and/or bibliography entries not cross-referenced in the text",
    green = "All cross-references were in the bibliography and bibliography entries were cross-referenced in the text",
    na = "No bibliography entries were detected"
  )

  cols <- c("bib_id", "type", "contents", "reference")
  report_table <- table
  report_table$type = ifelse(is_missing, "missing", "extra")
  report_table$reference <- ifelse(is.na(report_table$reference), report_table$text, report_table$reference)
  report_table <- report_table[, cols]

  report_text <- c(
    "This module relies on Grobid correctly parsing the references. There are likely to be some false positives.",
    scroll_table(report_table)
  )

  # return
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report_text,
    summary_text = report[[tl]]
  )
}
