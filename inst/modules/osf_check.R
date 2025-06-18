#' Check Status of OSF Links
#'
#' @description
#' List all OSF links and whether they are open, closed, or do not exist.
#'
#' @author Daniel Lakens
#'
#' @import httr
#' @import dplyr
#' @import progress
#'
#' @param paper a paper object or paperlist object
#' @param ... further arguments (not used)
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci[1:10], "osf_check")
osf_check <- function(paper, ...) {
  # test private: url = "https://osf.io/5tbm9"
  # test public: url = "https://osf.io/629bx"

  # detailed table of results ----
  found_osf <- osf_links(paper)

  table <- osf_retrieve(found_osf)
  table$status <- dplyr::case_when(
    is.na(table$osf_id) ~ "invalid",
    table$osf_type == "too many requests" ~ "too many requests",
    table$osf_type == "unfound" ~ "unfound",
    table$osf_type == "private" ~ "closed",
    table$public == FALSE ~ "closed",
    table$public == TRUE ~ "open",
    !is.na(table$osf_type) ~ "open",
    .default = ""
  )

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, status) |>
    tidyr::pivot_wider(names_from = status,
                       names_prefix = "osf.",
                       values_from = n, values_fill = 0)

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(table) == 0 ~ "na",
    all(table$status == "error") ~ "fail",
    all(table$status == "unknown") ~ "fail",
    any(table$status == "unfound") ~ "red",
    any(table$status == "invalid") ~ "red",
    any(table$status == "closed") ~ "red",
    all(table$status == "open") ~ "green",
    .default = "yellow"
  )

  report = c(
    na = "No OSF links were detected",
    red = "We detected closed OSF links",
    yellow = "There may be problems with some OSF links",
    green = "All OSF links are open",
    fail = "All attempts to check OSF links failed; check if you are offline."
  )

  # return a list ----
  list(
    table = table,
    summary = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report[[tl]]
  )
}
