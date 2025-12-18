#' AsPredicted
#'
#' @description
#' Get data from AsPredicted pre-registrations in a structured way
#'
#' @keywords method
#'
#' @author Daniel Lakens (\email{lakens@gmail.com})
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci, "aspredicted")
aspredicted <- function(paper) {
  # table ----
  links <- aspredicted_links(paper)
  if (nrow(links) == 0) {
    table <- data.frame(id = character(0), text = character(0))
  } else {
    table <- aspredicted_retrieve(links)
  }

  # summary_table ----
  summary_table <- dplyr::count(table, id, text) |>
    dplyr::count(id, n, name = "AP_links") |>
    dplyr::select(-n)

  # traffic light, summary_text & report ----
  if (nrow(table) == 0) {
    tl <- "na"
    summary_text <- "No AsPredicted links were found."
    report <- summary_text
  } else {
    tl <- "info"

    summary_text = sprintf(
      "%d AsPredicted link%s found and retrieved.",
      nrow(table),
      plural(nrow(table), " was", "s were")
    )
    report <- summary_text

    if (is_paper(paper) | length(paper) == 1) {
      ss_text <- paste(">", unique(table$AP_sample_size)) |>
        paste(collapse = "\n\n")

      report <- c(
        summary_text,
        "Sample size is most common deviation. This is what was stated about sample size:",
        ss_text
      )
    }
  }

  # return list ----
  list(
    table = table,
    summary_text = summary_text,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report
  )
}
