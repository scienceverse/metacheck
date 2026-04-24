#' Summarise References
#'
#' @description
#' Summarise information about each reference in a paper.
#'
#' @details
#' This module summarises previously-run reference section modules: ref_accuracy, ref_pubpeer, ref_replication, and ref_retraction.
#'
#' @keywords reference
#'
#' @author Lisa DeBruine (\email{debruine@gmail.com})
#'
#' @import dplyr
#' @import tidyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report list
ref_summary <- function(paper, ...) {
  # if all tables null, quit here
  table <- ref_table(paper)
  if (nrow(table) == 0) {
    to_return <- list(
      traffic_light = "na",
      summary_text = "No references to summarise"
    )

    return(to_return)
  }

  # module code ----
  tables <- list()
  tables$accuracy <- get_prev_outputs("ref_accuracy", "table")
  tables$pubpeer <- get_prev_outputs("ref_pubpeer", "table")
  tables$replication <- get_prev_outputs("ref_replication", "table")
  tables$retraction <- get_prev_outputs("ref_retraction", "table")

  # for dev only ----
  # if (TRUE) {
    # paper <- demopaper()
    # checks <- paper |>
    #   module_run("ref_accuracy") |>
    #   module_run("ref_pubpeer") |>
    #   module_run("ref_replication") |>
    #   module_run("ref_retraction")
    #
    # tables <- list()
    # tables$accuracy <- checks$prev_outputs$ref_accuracy$table
    # tables$pubpeer <- checks$prev_outputs$ref_pubpeer$table
    # tables$replication <- checks$prev_outputs$ref_replication$table
    # tables$retraction <- checks$table
  # }

  # create return items ----

  ## accuracy -----
  if (!is.null(tables$accuracy) && nrow(tables$accuracy) > 0) {
    cols <- c("paper_id", "bib_id",
              grep("no_match|_mismatch", names(tables$accuracy), value = TRUE))
    tbl <- tables$accuracy[, cols] |>
      tidyr::pivot_longer(dplyr::ends_with("_mismatch")) |>
      dplyr::mutate(name = gsub("_mismatch", "", name)) |>
      dplyr::filter(!value %in% FALSE) |>
      dplyr::summarise(accuracy_mismatch = paste(name, collapse = ", "),
                       .by = c(paper_id, bib_id, no_match))
    tbl$accuracy_mismatch[tbl$no_match %in% TRUE] <- "no match"
    tbl$no_match <- NULL
    table <- dplyr::left_join(table, tbl, by = c("paper_id", "bib_id"))
  }

  ## pubpeer ----
  if (!is.null(tables$pubpeer) && nrow(tables$pubpeer) > 0) {
    cols <- intersect(c("paper_id", "bib_id", "url"), names(tables$pubpeer))
    tbl <- tables$pubpeer[, cols]
    if ("url" %in% names(tbl)) {
      tbl$pubpeer <- link(tbl$url, "Link")
      tbl$url <- NULL
    }
    table <- dplyr::left_join(table, tbl, by = c("paper_id", "bib_id"))
  }

  ## replication ----
  if (!is.null(tables$replication) && nrow(tables$replication) > 0) {
    cols <- intersect(
      c("paper_id", "bib_id", "replication_type"),
      names(tables$replication)
    )
    tbl <- tables$replication[, cols]
    table <- dplyr::left_join(table, tbl, by = c("paper_id", "bib_id"))
  }

  ## retraction ----
  if (!is.null(tables$retraction) && nrow(tables$retraction) > 0) {
    cols <- setdiff(names(tables$retraction), c("text", "doi"))
    tbl <- tables$retraction[, cols]
    table <- dplyr::left_join(table, tbl, by = c("paper_id", "bib_id"))
  }

  ## traffic light ----
  tl <- "info"

  ## summary_text ----
  summary_text <- sprintf(
    "Summary information provided for %d reference%s",
    nrow(table), plural(nrow(table))
  )

  ## report ----
  cols <- grep("^(pubpeer.*|replication.*|retractionwatch|.*_mismatch)$",
               names(table), value = TRUE)
  cols <- c("text", cols)
  report_table <- table[, cols]
  colwidths <- c(0.75, rep(NA, length(cols) - 1))
  report <- c(
    "See the specific reports above for details.",
    scroll_table(report_table, maxrows = 10, colwidths = colwidths)
  )

  # return a list ----
  list(
    table = table,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
