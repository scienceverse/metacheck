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
