# test private: url = "https://osf.io/5tbm9"
# test public: url = "https://osf.io/629bx"

# get OSF links
found_urls <- papercheck::module_run(paper, "all-urls")$table
found_osf <- papercheck::search_text(found_urls, "osf\\.io")
unique_urls <- unique(found_osf["text"])

if (papercheck:::site_down("osf.io", error = FALSE)) {
  unique_urls$status <- "unknown"
  message("osf.io cannot be reached to assess link status")
} else {
  # set up progress bar ----
  if (papercheck::verbose()) {
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

traffic_light <- dplyr::case_when(
  nrow(unique_urls) == 0 ~ "na",
  all(unique_urls$status == "error") ~ "fail",
  any(unique_urls$status == "closed") ~ "red",
  all(unique_urls$status == "open") ~ "green",
  .default = "yellow"
)

table <- dplyr::left_join(found_osf, unique_urls, by = "text")

# return
list(
  table = table,
  traffic_light = traffic_light
)
