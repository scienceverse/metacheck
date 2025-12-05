#' Module Title
#'
#' @description
#' A short description of the module
#'
#' @author Author Name (\email{name@email.com})
#'
#' @references
#' # Optional reference to include in reports
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list
#'
#' @examples
#' module_run(psychsci, "module_name")
module_name <- function(paper, ...) {
  # see https://www.scienceverse.org/metacheck/articles/creating_modules.html

  # module code ----
  pattern <- "significant"

  # create return items ----

  ## table ----
  # detail your results in a format like the result of search_text()
  # this is stored to use in later modules in a report or pipeline
  table <- search_text(paper, pattern)

  ## summary_table ----
  # must have id column as the id of each paper, one row per paper
  # and further columns to be added to a master summary table
  summary_table <- dplyr::count(table, id, name = "n_significant")

  ## traffic light ----
  # displayed in reports, possible values:
  #   green: no problems detected
  #   yellow: something to check
  #   red: possible problems detected
  #   info: informational only
  #   na: not applicable
  #   fail: check failed
  tl <- if (nrow(table)) "info" else "na"

  ## summary_text ----
  # short text to be displayed at the top of reports
  # may be unique for each possible traffic light
  summary_text_options <- c(
    na = "Not applicable",
    info = "This table is provided for your information",
    red = "This is a potential problem",
    yellow = "There may be a problem",
    green = "No problems found",
    fail = "The check failed, sorry"
  )
  summary_text <- summary_text_options[[tl]]

  ## report ----
  # longer text to be displayed in the module section
  # use quarto / markdown for styling
  # https://quarto.org/docs/authoring/markdown-basics.html
  report_text <- "This table shows all of the sentences where the paper used the word *significant*. "
  report_table <- table[, c("section", "text")]
  further_info <- c(
    "For more opinions on the use of the word *significant*:",
    "* Motulsky, H. (2014). Opinion: Never use the word ‘significant’ in a scientific paper. Advances in regenerative biology, 1(1), 25155. doi: [10.3402/arb.v1.25155](https://doi.org/10.3402/arb.v1.25155)"
    )

  report <- c(
    report_text,
    scroll_table(report_table, colwidths = c(.2, .8)),
    collapse_section(further_info, title = "Further Info")
  )

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}
