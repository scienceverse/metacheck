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
  bibs <- ref_table(paper) |>
    dplyr::select(paper_id, bib_id, reference = text)
  xrefs <- paper_table(paper, "xref") |>
    dplyr::filter(xref_type == "bib") |>
    dplyr::select(paper_id, bib_id = xref_id, contents, text_id)
  text <- paper_table(paper, "text") |>
    dplyr::select(paper_id, text_id, text)

  table <- dplyr::full_join(
    bibs, xrefs,
    by = c("paper_id", "bib_id")
  ) |>
    dplyr::filter(is.na(contents) | is.na(bib_id)) |>
    dplyr::left_join(text, by = c("paper_id", "text_id"))
  table$text_id <- NULL

  # summary_table ----
  nbibs <- dplyr::count(bibs, paper_id, name = "n_bib")
  nxrefs <- dplyr::count(xrefs, paper_id, name = "n_xrefs")
  nmiss <- dplyr::count(table, paper_id, missing = is.na(bib_id)) |>
    tidyr::pivot_wider(
      names_from = missing, names_prefix = "n_missing_",
      values_from = n, values_fill = 0
    )
  nextra <- dplyr::count(table, paper_id, extra = is.na(contents)) |>
    tidyr::pivot_wider(
      names_from = extra, names_prefix = "n_extra_",
      values_from = n, values_fill = 0
    )
  summary_table <- paper_id(paper) |>
    dplyr::left_join(nbibs, by = "paper_id") |>
    dplyr::left_join(nxrefs, by = "paper_id") |>
    dplyr::left_join(nmiss, by = "paper_id") |>
    dplyr::left_join(nextra, by = "paper_id") |>
    dplyr::select(-dplyr::ends_with("_FALSE")) |>
    dplyr::rename_with(\(x) gsub("_TRUE", "", x))

  # traffic light ----
  tl <- dplyr::case_when(
    nrow(bibs) == 0 ~ "na",
    nrow(table) > 0 ~ "red",
    .default = "green"
  )

  # report ----
  report <- c(
    red = "There are cross-references that are not in the bibliography and/or bibliography entries not cross-referenced in the text",
    green = "All cross-references were in the bibliography and bibliography entries were cross-referenced in the text",
    na = "No bibliography entries were detected"
  )

  cols <- c("bib_id", "type", "contents", "reference")
  report_table <- table
  report_table$type = ifelse(is.na(report_table$contents), "extra", "missing")
  report_table$reference <- ifelse(is.na(report_table$reference), report_table$text, report_table$reference)
  report_table <- report_table[, cols]

  report_text <- c(
    "This module relies on Grobid correctly parsing the references. There are likley to be some false positives.",
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
