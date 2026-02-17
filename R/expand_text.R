#' Expand text
#'
#' If you have a table resulting from `search_text()` or a module return object, you can expand the text column to the full sentence, paragraph, or section. You can also set `plus` and `minus` to append and prepend sentences to the result (only when `expand_to` is "sentence").
#'
#' @param results_table the table to expand
#' @param paper a metacheck paper object or a list of paper objects to look up the expanded text from
#' @param expand_to whether to expand to the sentence, paragraph, div, or section level
#' @param plus append additional sentences after the target expansion
#' @param minus prepend additional sentences before the target expansion
#'
#' @returns a results table with the expanded text
#' @export
#'
#' @examples
#' # single paper search
#' paper <- demopaper()
#' res_tbl <- search_text(paper, "p =", return = "match")
#' expanded <- expand_text(res_tbl, paper)
#'
#' # multiple paper search
#' papers <- demodir() |> read()
#' res_tbl <- search_text(papers, "replicate", return = "sentence")
#' expanded <- expand_text(res_tbl, papers, plus = 1, minus = 1)
expand_text <- function(results_table,
                        paper,
                        expand_to = c("sentence", "paragraph", "div", "section"),
                        plus = 0, minus = 0) {
  id <- section_id <- paragraph_id <- text_id <- text <- expanded <- NULL # ugh cmdcheck

  # check results_table and extract table if object
  if (!is.data.frame(results_table)) {
    if (inherits(results_table, "metacheck_module_output")) {
      results_table <- results_table$table
    } else if (is_paper(results_table)) {
      results_table <- results_table$text
    } else {
      stop("The results table was not a table or object containing a table")
    }
  }

  # set up full text table ----
  by <- c("id", "section_id", "paragraph_id", "text_id", "text")
  ft <- search_text(paper)[, by]

  # set up expand_to ----
  expand_to <- match.arg(expand_to)
  if (expand_to == "sentence") {
    by <- by[1:4]
    text <- ft |>
      dplyr::summarise(
        expanded = paste(text, collapse = " "),
        .by = dplyr::all_of(by)
      )
  } else {
    # collapse sentences within paragraphs separated by spaces
    by <- by[1:3]
    text_p <- ft |>
      dplyr::summarise(
        expanded = paste(text, collapse = " "),
        .by = dplyr::all_of(by)
      )

    if (expand_to == "section") {
      # collapse paragraphs within sections separated by line breaks
      by <- by[1:2]
    }

    text <- text_p |>
      dplyr::summarise(
        expanded = paste(expanded, collapse = "\n\n"),
        .by = dplyr::all_of(by)
      )
  }

  # expand sentences ----
  if (minus > 0 | plus > 0) {
    if (expand_to != "sentence") {
      message("Plus and minus only work when expand_to == 'sentence'")
    } else {
      # cut down to relevant sentences

      text <- lapply(-minus:plus, function(offset) {
        coords <- results_table[c("id", "section_id", "paragraph_id", "text_id")]
        coords$exp_text_id <- coords$text_id + offset
        coords
      }) |>
        do.call(rbind, args = _) |>
        unique() |>
        dplyr::left_join(text, by = c("id", "section_id", "paragraph_id", "exp_text_id" = "text_id")) |>
        dplyr::filter(!is.na(expanded)) |>
        dplyr::summarise(
          expanded = paste(expanded, collapse = " "),
          .by = dplyr::all_of(c("id", "section_id", "paragraph_id", "text_id"))
        )
    }
  }

  # join to results and process
  expanded_table <- results_table |>
    dplyr::left_join(text, by = by, suffix = c("", ".full"))

  # if expanded doesn't match anything, at least return the text
  expanded_table$expanded <- ifelse(is.na(expanded_table$expanded),
    expanded_table$text,
    expanded_table$expanded
  )

  return(expanded_table)
}
