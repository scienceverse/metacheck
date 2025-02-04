options("scipen" = 10,
        "digits" = 4)

# datatable constants ----
dt_options <- list(
  info = TRUE,
  lengthChange = TRUE,
  paging = TRUE,
  ordering = FALSE,
  searching = FALSE,
  pageLength = 10,
  keys = FALSE,
  dom = '<"top" ip>'
  #scrollX = TRUE,
  #columnDefs = list(list(width = "6em", targets = 7))
)

dt_search_options <- list(
  info = TRUE,
  lengthChange = TRUE,
  paging = TRUE,
  ordering = FALSE,
  searching = TRUE,
  pageLength = 10,
  keys = FALSE,
  dom = '<"top" ip>'
)
