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
  # get OSF links
  found_urls <- papercheck::module_run(paper, "all_urls")$table
  found_osf <- papercheck::search_text(found_urls, "osf\\.io")
  unique_urls <- unique(found_osf["text"])

  if (papercheck:::site_down("osf.io", error = FALSE)) {
    unique_urls$status <- "unknown"
    message("osf.io cannot be reached to assess link status")
  } else {
    # set up progress bar ----
    if (papercheck::verbose() & nrow(unique_urls)) {
      pb <- progress::progress_bar$new(
        total = nrow(unique_urls), clear = FALSE, show_after = 0,
        format = "Checking OSF availability [:bar] :current/:total :elapsedfull"
      )
      pb$tick(0)
    }

    # Check for closed OSF links
    unique_urls$status <- sapply(unique_urls$text, \(url) {
      status <- tryCatch({
        resp <- httr::GET(url)
        if (resp$status_code == 404) {
          return("missing")
        } else if (resp$status_code != 200) {
          return("error")
        }

        txt <- httr::content(resp, "text")
        if (grepl("Sign in with your OSF account to continue", txt)) {
          "closed"
        } else if (grepl("view-only link", txt)) {
          "view-only"
        } else {
          "open"
        }
      }, error = \(e) {
        return("error")
      })

      if (papercheck::verbose()) pb$tick()

      return(status)
    })
  }

  table <- dplyr::left_join(found_osf, unique_urls, by = "text")

  # summary output for paperlists ----
  summary_table <- dplyr::count(table, id, status) |>
    tidyr::pivot_wider(names_from = status,
                       names_prefix = "osf.",
                       values_from = n)

  # determine the traffic light ----
  tl <- dplyr::case_when(
    nrow(unique_urls) == 0 ~ "na",
    all(unique_urls$status == "error") ~ "fail",
    all(unique_urls$status == "unknown") ~ "fail",
    any(unique_urls$status == "closed") ~ "red",
    all(unique_urls$status == "open") ~ "green",
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
