#' AsPredicted
#'
#' @description
#' Get data from AsPredicted pre-registrations in a structured way
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
  # detailed table of results ----
  links <- aspredicted_links(paper)
  if (nrow(links) == 0) {
    table <- data.frame(id = character(0), text = character(0))
  } else {
    table <- aspredicted_retrieve(links)
  }

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, text) |>
    dplyr::count(id, n, name = "AP_links") |>
    dplyr::select(-n)

  # determine the traffic light ----
  tl <- ifelse(nrow(table), "info", "na")

  n <- sum(summary_table$AP_links)
  report = c(
    na = "No AsPredicted links were found.",
    info = paste0(n," AsPredicted link",
                  ifelse(n == 1, " was", "s were"),
                  " found and retrieved.")
  )

  if (n > 0 & (is_paper(paper) | length(paper) == 1)) {
    ss_text <- paste(">", unique(table$AP_sample_size)) |>
      paste(collapse = "\n\n")

    ret_plus <- paste0("\n\nSample size is most common deviation. Ths is what was stated about sample size:",
                       "\n\n", ss_text)
  } else {
    ret_plus <- ""
  }

  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = paste0(report[[tl]], ret_plus)
  )
}
