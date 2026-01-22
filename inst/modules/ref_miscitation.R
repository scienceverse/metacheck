#' Miscitation
#'
#' @description
#' Check for frequently miscited papers. This module is just a proof of concept -- the miscite database is not yet populated with real examples.
#'
#' @keywords reference
#'
#' @details
#' If you want to use your own database, create a data frame with the columns "doi", "reftext", and "warning", which will be used to match the DOI in papers (use short DOIs like xxx/1234, not https://doi.org/xxx/1234) and produce the report text.
#'
#' @import dplyr
#' @import tidyr
#'
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @param paper a paper object or paperlist object
#' @param db the miscitation database (data frame with doi, reftext, and warning columns)
#'
#' @returns a list
ref_miscitation <- function(paper, db = readRDS(system.file("databases/miscite.Rds", package = "metacheck"))) {
  # consolidate bib tables and filter to relevant DOI
  bibs <- concat_tables(paper, "bib") |>
    dplyr::select(xref_id, ref, doi, id) |>
    dplyr::inner_join(db, by = "doi") |>
    unique()

  # consolidate xrefs, filter, and expand
  xrefs <- concat_tables(paper, "xrefs") |>
    dplyr::right_join(bibs, by = c("xref_id", "id")) |>
    #expand_text(paper, expand_to = "paragraph")
    unique()

  # detailed table of results ----
  cols <- c("id", "xref_id", "doi", "text", "section", "div", "p", "s")
  table <- xrefs[, cols] |> unique()

  # summary output for paperlists ----
  summary_table <- table |>
    dplyr::count(id, xref_id, doi) |>
    dplyr::select(-n) |>
    tidyr::pivot_wider(names_from = doi, values_from = xref_id,
                       names_prefix = "miscite_")

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(table) > 0 ~ "yellow",
    .default = "green"
  )

  # report text for each possible traffic light ----
  if (nrow(table) == 0) {
    summary_text <- "We detected no miscited papers"
    report <- summary_text
  } else {
    to_warn <- xrefs[, c("doi", "warning", "reftext")] |>
      unique()

    summary_text <- sprintf("We found %d citation%s to papers that are commonly miscited.", nrow(table), plural(nrow(table)))

    for (i in seq_along(to_warn$doi)) {
      warn_doi <- to_warn$doi[[i]]
      all_instances <- xrefs$text[xrefs$doi == warn_doi]
      instances <- head(all_instances, 5)

      n_head <- length(instances)
      n_all <- length(all_instances)
      instance_n <- ifelse(n_head < n_all,
                        sprintf("%i of %i", n_head, n_all),
                        sprintf("%i", n_head))

      report <- sprintf("**%s**\n\n%s\n\n%s\n\n*%s Instance%s:*\n\n%s",
                        warn_doi,
                        to_warn$reftext[[i]],
                        to_warn$warning[[i]],
                        instance_n,
                        plural(length(all_instances)),
                        paste(">", instances, collapse = "\n\n"))
    }
  }

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
