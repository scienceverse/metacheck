#' RetractionWatch
#'
#' @description
#' Flag any cited papers in the RetractionWatch database
#'
#' @author Lisa DeBruine
#'
#' @references
#' The Retraction Watch Database [Internet].
#' New York: The Center for Scientific Integrity. 2018.
#' ISSN: 2692-4579. [Cited 2025-05-20].
#' Available from: http://retractiondatabase.org/.
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "retractionwatch")
retractionwatch <- function(paper) {
  # detailed table of results ----
  bibs <- papercheck::concat_tables(paper, c('bib'))
  table <- dplyr::inner_join(bibs, papercheck::retractionwatch, by = 'doi')
  if (nrow(table) > 0) {
    xrefs <- papercheck::concat_tables(paper, c('xrefs'))
    table <- dplyr::left_join(table, xrefs, by = c('xref_id', "id"))
  }

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, retractionwatch) |>
    tidyr::pivot_wider(names_from = retractionwatch, names_prefix = "rw_",
                       values_from = n)

  # determine the traffic light ----
  tl <- if (nrow(table)) "yellow" else "green"

  # report text for each possible traffic light ----
  report <- c(
    yellow = "You cited some papers in the Retraction Watch database (as of 2025-05-20). These may be retracted, have corrections, or expressions of concern.",
    green = "You cited no papers in the Retraction Watch database (as of 2025-05-20)"
  )

  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}

