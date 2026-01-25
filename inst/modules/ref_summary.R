#' Summarise References
#'
#' @description
#' Summarise information about each reference in a paper.
#'
#' @details
#' This module summarises previously-run reference section modules: ref_doi_check, ref_accuracy, ref_pubpeer, ref_replication, and ref_retractiuon.
#'
#' @keywords reference
#'
#' @author Lisa DeBruine (\email{debruine@gmail.com})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report list
ref_summary <- function(paper, ...) {
  # if all tables null, quit here
  if (nrow(paper$bib) == 0) {
    to_return <- list(
      traffic_light = "na",
      summary_text = "No references to summarise"
    )

    return(to_return)
  }

  # module code ----
  tables <- list()
  tables$doi_check <- get_prev_outputs("ref_doi_check", "table")
  tables$accuracy <- get_prev_outputs("ref_accuracy", "table")
  tables$pubpeer <- get_prev_outputs("ref_pubpeer", "table")
  tables$replication <- get_prev_outputs("ref_replication", "table")
  tables$retraction <- get_prev_outputs("ref_retraction", "table")

  # for dev only ----
  # if (TRUE) {
  #   paper <- read(demoxml())
  #   checks <- paper |>
  #     module_run("ref_doi_check") |>
  #     module_run("ref_accuracy") |>
  #     module_run("ref_pubpeer") |>
  #     module_run("ref_replication") |>
  #     module_run("ref_retraction")
  #
  #   tables$doi_check <- checks$prev_outputs$ref_doi_check$table
  #   tables$accuracy <- checks$prev_outputs$ref_accuracy$table
  #   tables$pubpeer <- checks$prev_outputs$ref_pubpeer$table
  #   tables$replication <- checks$prev_outputs$ref_replication$table
  #   tables$retraction <- checks$table
  # }

  # create return items ----

  ## table ----
  table <- paper$bib
  table$ref <- format_ref(table$ref)

  ## accuracy -----
  if (!is.null(tables$accuracy) && nrow(tables$accuracy) > 0) {
    cols <- setdiff(names(tables$accuracy), c("ref", "orig_title", "orig_authors"))
    tbl <- tables$accuracy[, cols]
    not_id <- !names(tbl) %in% c("id", "xref_id")
    names(tbl)[not_id] <- paste0("crossref_", names(tbl)[not_id])
    table <- dplyr::left_join(table, tbl, by = c("id", "xref_id"))
  }

  ## pubpeer ----
  if (!is.null(tables$pubpeer) && nrow(tables$pubpeer) > 0) {
    cols <- setdiff(names(tables$pubpeer), c("ref", "doi"))
    tbl <- tables$pubpeer[, cols]
    not_id <- !names(tbl) %in% c("id", "xref_id")
    names(tbl)[not_id] <- paste0("pubpeer_", names(tbl)[not_id])
    table <- dplyr::left_join(table, tbl, by = c("id", "xref_id"))
  }

  ## replication ----
  if (!is.null(tables$replication) && nrow(tables$replication) > 0) {
    cols <- setdiff(names(tables$replication), c("ref", "doi"))
    tbl <- tables$replication[, cols]
    table <- dplyr::left_join(table, tbl, by = c("id", "xref_id"))
  }

  ## retraction ----
  if (!is.null(tables$retraction) && nrow(tables$retraction) > 0) {
    cols <- setdiff(names(tables$retraction), c("ref", "doi"))
    tbl <- tables$retraction[, cols]
    table <- dplyr::left_join(table, tbl, by = c("id", "xref_id"))
  }

  ## traffic light ----
  tl <- "info"

  ## summary_text ----
  summary_text <- sprintf("Summary information provided for %d reference%s",
                          nrow(table), plural(nrow(table)))

  ## report ----
  cols <- grep("^(crossref_DOI|pubpeer_.*|replication_.*|retractionwatch|.*doi_found|.*ref_not_found|.*_mismatch)$", names(table), value = TRUE)
  cols <- c("ref", cols)
  report_table <- table[, cols]
  report <- scroll_table(report_table, maxrows = 10)

  # return a list ----
  list(
    table = table,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
