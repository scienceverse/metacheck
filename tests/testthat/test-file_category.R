test_that("file_category", {
  expect_true(is.function(metacheck::file_category))
  expect_no_error(helplist <- help(file_category, metacheck))

  # handle zero results and/or OSF down
  summary <- file_category(data.frame())
  expect_equal(nrow(summary), 0)

  # as vector
  contents <- c("a.csv", "b.R", "codebook.xlsx", "readme.txt", "ambiguous")
  summary <- file_category(contents)
  obs <- summary$file_category
  exp <- c("data", "code", "codebook", "readme", NA)
  expect_equal(obs, exp)

  # as data frame
  contents <- data.frame(
    name = c("a.csv", "b.R", "codebook.xlsx", "readme.txt", "ambiguous"),
    category = c("code", "data", "data", "code", "code"),
    filetype = c("data", "code", "data", "text", "text")
  )
  summary <- file_category(contents)
  obs <- summary$file_category
  # not currently categorising from category
  exp <- c("data", "code", "codebook", "readme", NA)
  expect_equal(obs, exp)
})

test_that("add_filetype", {
  # edge case classification
  files <- c(
    "datarelease.pdf" = "text",    # pdf cannot be data or code
    "my_r_code.pdf" = "text",
    "data.sas" = "stats",          # sas is always code
    "codebook.sas" = "stats",
    "codebook.pdf" = "text"
  )
  ft <- filetype(names(files))
  expect_equal(ft, files)
})

test_that("edge case summarise", {
  # edge case classification
  # category is from OSF, so can be: analysis, communication, data, hypothesis, instrumentation, methods and measures, procedure, project, software, other, but mostly uncategorized (NA)
  contents <- dplyr::tribble(
    ~name,              ~category, ~classify,
    "datarelease.pdf",  NA,         NA,        # pdf cannot be data or code
    "data.pdf",         "data",     NA,        # what about qual data?
    "my_r_code.pdf",    NA,         NA,
    "readme.xls",       "project",  "readme",    # is an xls file always data?
    "data.sas",         NA,         "code",    # sas is always code
    "codebook.sas",     NA,         "codebook",
    "readme.sas",       NA,         "readme",
    "codebook.pdf",     NA,         "codebook" # not a great format but possible
  )
  contents$filetype <- filetype(contents$name)

  summary <- file_category(contents)
  expect_equal(summary$file_category, contents$classify)
})
