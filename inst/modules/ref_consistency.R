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
  bibs <- concat_tables(paper, "bib")
  xrefs <- concat_tables(paper, "xrefs")
  xrefs <- xrefs[xrefs$type == "bibr", ]

  missing_refs <- dplyr::anti_join(bibs, xrefs, by = c("id", "xref_id"))
  missing_refs$missing <- rep("xrefs", nrow(missing_refs))
  missing_bib <- dplyr::anti_join(xrefs, bibs, by = c("id", "xref_id"))
  missing_bib$missing <- rep("bib", nrow(missing_bib))
  names(missing_bib) <- names(missing_bib) |> sub("text", "ref", x = _)
  missing_bib$ref <- as.list(missing_bib$ref)

  table <- dplyr::bind_rows(missing_refs, missing_bib) |>
    dplyr::arrange(id, xref_id)

  # summary_table ----
  nbibs <- dplyr::count(bibs, id, name = "n_bib")
  nxrefs <- dplyr::count(xrefs, id, name = "n_xrefs")
  nmiss <- dplyr::count(table, id, missing) |>
    tidyr::pivot_wider(
      names_from = missing, names_prefix = "missing_",
      values_from = n, values_fill = 0
    )
  summary_table <- info_table(paper, c()) |>
    dplyr::left_join(nbibs, by = "id") |>
    dplyr::left_join(nxrefs, by = "id") |>
    dplyr::left_join(nmiss, by = "id")

  # traffic light ----
  tl <- dplyr::case_when(
    nrow(bibs) == 0 ~ "na",
    nrow(missing_bib) || nrow(missing_refs) ~ "red",
    .default = "green"
  )

  # report ----
  report <- c(
    red = "There are cross-references that are not in the bibliography and/or bibliography entries not cross-referenced in the text",
    green = "All cross-references were in the bibliography and bibliography entries were cross-referenced in the text",
    na = "No bibliography entries were detected"
  )

  cols <- c("xref_id", "ref", "missing")
  report_table <- table[, cols]

  report_text <- c(
    report[[tl]],
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
