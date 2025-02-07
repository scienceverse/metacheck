#' Expand text
#'
#' If you have a table resulting from `search_text()` or a module return object, you can expand the text column to the full sentence, paragraph, or section. You can also set `plus` and `minus` to append and prepend sentences to the result (only when `expand_to` is "sentence").
#'
#' @param results_table the table to expand
#' @param paper a papercheck paper object or a list of paper objects to look up the expanded text from
#' @param expand_to whether to expand to the sentence, paragraph, div, or section level
#' @param plus append additional sentences after the target expansion
#' @param minus prepend additional sentences before the target expansion
#'
#' @returns a results table with the expanded text
#' @export
#'
#' @examples
#' # single paper search
#' paper <- demoxml() |> read_grobid()
#' res_tbl <- search_text(paper, "p =", return = "match")
#' expanded <- expand_text(res_tbl, paper)
#'
#' # multiple paper search
#' papers <- demodir() |> read_grobid()
#' res_tbl <- search_text(papers, "replicate", return = "sentence")
#' expanded <- expand_text(res_tbl, papers, plus = 1, minus = 1)
expand_text <- function(results_table,
                        paper,
                        expand_to = c("sentence", "paragraph", "div", "section"),
                        plus = 0, minus = 0) {
  id <- div <- p <- s <- text <- NULL # ugh cmdcheck

  # check results_table and extract table if object
  if (!is.data.frame(results_table)) {
    if (inherits(results_table, "ppchk_module_output")) {
      results_table <- results_table$table
    } else if (inherits(results_table, "scivrs_paper")) {
      results_table <- results_table$full_text
    } else {
      stop("The results table was not a table or object containing a table")
    }
  }

  # set up expand_to ----
  expand_to <- match.arg(expand_to)
  if (expand_to == "sentence") {
    by <- c("id", "div", "p", "s")
  } else if (expand_to == "paragraph") {
    by <- c("id", "div", "p")
  } else if (expand_to == "div") {
    by <- c("id", "div")
  } else if (expand_to == "section") {
    by <- c("id", "section")
  }

  # set up full text table ----
  full_text <- search_text(paper)[, c("id", "section", "div", "p", "s", "text")] |>
    dplyr::summarise(expanded = paste(text, collapse = " "),
                     .by = dplyr::all_of(by))

  if (expand_to == "sentence" & (minus > 0 | plus > 0)) {
    full_text$expanded <- mapply(function(id1, d1, p1, s1) {
      srange <- seq(s1 - minus, s1 + plus, 1)
      pm <- dplyr::filter(full_text,
                          id == id1, div == d1, p == p1,
                          s %in% srange)
      paste(pm$expanded, collapse = " ")
    }, id1 = full_text$id,
        d1 = full_text$div,
        p1 = full_text$p,
        s1 = full_text$s)
  }


  # join to results and process
  expanded_table <- results_table |>
    dplyr::left_join(full_text, by = by)

  return(expanded_table)
}
