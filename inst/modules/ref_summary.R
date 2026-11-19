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

  # one row per reference: several replications or PubPeer rows for a
  # reference repeated it, so the summary counted 7 references for a paper
  # with 6 (U119); their values are combined
  one_per_ref <- function(tbl) {
    keys <- c("paper_id", "bib_id")
    vals <- setdiff(names(tbl), keys)
    dplyr::summarise(tbl, dplyr::across(dplyr::all_of(vals), \(x) {
      x <- unique(x[!is.na(x) & nzchar(as.character(x))])
      if (length(x)) paste(x, collapse = "; ") else NA_character_
    }), .by = dplyr::all_of(keys))
  }

  ## accuracy -----
  if (!is.null(tables$accuracy) && nrow(tables$accuracy) > 0) {
    # "no match" decided per reference, before mismatches are filtered: a
    # reference with no_match and every *_mismatch FALSE dropped out and got
    # NA; a table without no_match stopped the module (U119)
    acc <- tables$accuracy
    if (!"no_match" %in% names(acc)) acc$no_match <- NA
    acc <- acc[!duplicated(acc[, c("paper_id", "bib_id")]), , drop = FALSE]
    mm <- grep("_mismatch$", names(acc), value = TRUE)
    acc$accuracy_mismatch <- vapply(seq_len(nrow(acc)), \(i) {
      # a title search found the work nowhere, or failed
      if (isTRUE(acc$not_found[[i]])) return("not found")
      if (isTRUE(acc$search_failed[[i]])) return("not checked")
      if (isTRUE(acc$no_match[[i]])) {
        # ref_accuracy's tier says why there is no record: a cited DOI that
        # does not exist is not the same as one that was not looked up
        return(switch(acc$tier[[i]] %||% NA_character_,
                      unresolved = "DOI not found",
                      registered = , unconfirmed = "not checked",
                      not_checked = "not looked up",
                      unreadable = "not read",
                      "no match"))
      }
      hit <- mm[vapply(mm, \(m) isTRUE(acc[[m]][[i]]), logical(1))]
      if (length(hit)) paste(sub("_mismatch$", "", hit), collapse = ", ") else NA_character_
    }, character(1))
    tbl <- acc[!is.na(acc$accuracy_mismatch), c("paper_id", "bib_id", "accuracy_mismatch")]
    table <- dplyr::left_join(table, tbl, by = c("paper_id", "bib_id"))
  }

  ## pubpeer ----
  if (!is.null(tables$pubpeer) && nrow(tables$pubpeer) > 0) {
    cols <- intersect(c("paper_id", "bib_id", "url"), names(tables$pubpeer))
    tbl <- tables$pubpeer[, cols]
    if ("url" %in% names(tbl)) {
      tbl$pubpeer <- ifelse(is.na(tbl$url), NA_character_, link(tbl$url, "Link"))
      tbl$url <- NULL
    }
    table <- dplyr::left_join(table, one_per_ref(tbl), by = c("paper_id", "bib_id"))
  }

  ## replication ----
  if (!is.null(tables$replication) && nrow(tables$replication) > 0) {
    cols <- intersect(
      c("paper_id", "bib_id", "replication_type"),
      names(tables$replication)
    )
    tbl <- tables$replication[, cols]
    table <- dplyr::left_join(table, one_per_ref(tbl), by = c("paper_id", "bib_id"))
  }

  ## retraction ----
  if (!is.null(tables$retraction) && nrow(tables$retraction) > 0) {
    cols <- setdiff(names(tables$retraction), c("text", "doi"))
    tbl <- tables$retraction[, cols]
    table <- dplyr::left_join(table, one_per_ref(tbl), by = c("paper_id", "bib_id"))
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
