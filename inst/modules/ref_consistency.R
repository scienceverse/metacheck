#' Reference Consistency
#'
#' @description
#' Check if all references are cited and all citations are referenced
#'
#' @author Lisa DeBruine
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "ref_consistency")
ref_consistency <- function(paper) {
  # detailed table of results ----
  refs <- concat_tables(paper, "references")
  cites <- concat_tables(paper, "citations")

  missing_cites <- dplyr::anti_join(refs, cites, by = c("id", "bib_id"))
  if (nrow(missing_cites)) missing_cites$missing <- "citation"
  missing_refs <- dplyr::anti_join(cites, refs,  by = c("id", "bib_id"))
  if (nrow(missing_refs)) missing_refs$missing <- "reference"
  names(missing_refs) <- names(missing_refs) |> sub("text", "ref", x = _)

  table <- dplyr::bind_rows(missing_cites, missing_refs) |>
    dplyr::arrange(id, bib_id)

  # summary output for paperlists ----
  nrefs <- dplyr::count(refs, id, name = "n_refs")
  ncites <- dplyr::count(cites, id, name = "n_cites")
  nmiss <- dplyr::count(table, id, missing) |>
    tidyr::pivot_wider(names_from = missing, names_prefix = "missing_",
                       values_from = n, values_fill = 0)
  summary_table <- info_table(paper, c()) |>
    dplyr::left_join(nrefs, by = "id") |>
    dplyr::left_join(ncites, by = "id") |>
    dplyr::left_join(nmiss, by = "id")

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(refs) == 0 & nrow(cites) == 0 ~ "na",
    nrow(missing_cites) || nrow(missing_refs) ~ "red",
    .default = "green"
  )

  # report text for each possible traffic light ----
  report <- c(
    red = "There are references that are not cited or citations that are not referenced",
    green = "All references were cited and citations were referenced",
    na = "No citations/references were detected"
  )

  report_text <- "This module relies on Grobid correctly parsing the references. There may be some false positives."
  report_text <- paste(report_text, report[[tl]], sep = "\n\n")

  # return
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report_text
  )
}
