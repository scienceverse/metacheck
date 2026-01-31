#' COI Check (Overinclusive)
#'
#' @description
#' Identify and extract Conflicts of Interest (COI) statements.
#'
#' @details
#' The COI Check module uses regular expressions to check sentences for words related to conflict of interest statements. It will return the sentences in which the conflict of interest statement was found.
#'
#' The function is based on code from [rtransparent](https://github.com/serghiou/rtransparent), which is no longer maintained. For their validation, see [the paper](https://doi.org/10.1371/journal.pbio.3001107).
#'
#' This version is over-inclusive, so will have more false positives, but is less likely to miss something.
#'
#' @references
#' Serghiou, S., Contopoulos-Ioannidis, D. G., Boyack, K. W., Riedel, N., Wallach, J. D., & Ioannidis, J. P. (2021). Assessment of transparency indicators across the biomedical literature: How open is open?. PLoS biology, 19(3), e3001107. doi: 10.1371/journal.pbio.3001107
#'
#' Serghiou S (2025). _rtransparent: Identifies indicators of transparency_. R package version 0.2.5, commit d0d5dfe4b6c4e519d54e341436d4263fb05d84be, <https://github.com/serghiou/rtransparent>.
#'
#' @keywords general
#'
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
coi_check <- function(paper) {
  # table ----

  ## get potential COI statements ----
  pattern_main <- c(
    "\\binterests?\\b",
    "\\bCOI\\b"
  )
  pattern_inc <- c(
    "conflict",
    "compet",
    "disclosure",
    "declaration"
  )
  # definitely exclude
  pattern_exc <- c("financial disclosure")

  table <- paper |>
    search_text(pattern_main, search_header = TRUE) |>
    search_text(pattern_inc, search_header = TRUE) |>
    search_text(pattern_exc, exclude = TRUE) |>
    # merge the text by section/id
    search_text(return = "section")


  # summary_table ----
  summary_table <- table |>
    dplyr::summarise(coi_found = TRUE, .by = id)

  # traffic light ----
  tl <- ifelse(nrow(table), "green", "red")

  # report ----
  # summary_text ----
  if (tl == "green") {
    report <- c(
      "The following conflict of interest statement was detected.",
      scroll_table(table$text)
    )
    summary_text <- "A conflict of interest statement was detected."
  } else if (tl == "red") {
    report <- "No conflict of interest statement was detected. Consider adding one."
    summary_text <- "No conflict of interest statement was detected."
  }

  # return list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = FALSE,
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

