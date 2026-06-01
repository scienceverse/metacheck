# local_files() ----

test_that("local_files exists", {
  expect_true(is.function(metacheck::local_files))
  expect_no_error(help(local_files, metacheck))
})

test_that("local_files errors", {
  # undefined variable triggers R error before function runs
  expect_error(local_files(bad_arg))

  # path does not exist
  path <- "/no/such/path/exists/anywhere"
  expect_warning(obs <- local_files(path),
               regexp = "does not exist",
               ignore.case = TRUE)
  expect_equal(nrow(obs), 0)

  path <- c(
    test_path("fixtures", "code_files", "analysis.R"),
    "/no/such/path/exists/anywhere"
  )
  expect_warning(obs <- local_files(path),
                 regexp = "/no/such/path/exists/anywhere",
                 ignore.case = TRUE)
  expect_equal(obs$file_name, "analysis.R")
})

test_that("local_files file path", {
  # path exists but is a file, not a directory
  path <- test_path("fixtures", "code_files", "analysis.R")
  obs <- local_files(path)
  expect_equal(obs$repo_url, path)
  expect_equal(obs$file_name, "analysis.R")
  expect_equal(obs$file_location, normalizePath(path))
})

test_that("local_files dir path", {
  # path exists but is a file, not a directory
  path <- test_path("fixtures", "demo", "code")
  obs <- local_files(path)
  expect_setequal(obs$repo_url, path)
  expect_equal(obs$file_name, sprintf("%.02d.R", 1:25))
})

test_that("local_files mixed file + dir path", {
  # path exists but is multiple files, not a directory
  path <- c(
    test_path("fixtures", "demo", "code"),
    test_path("fixtures", "demo", "README.md")
  )
  obs <- local_files(path)
  expect_true(all(obs$repo_url[1:25] %in% path[[1]]))
  exp <- c(sprintf("%.02d.R", 1:25), "README.md")
  expect_setequal(obs$file_name, exp)
})

test_that("local_files empty directory", {
  tmp <- withr::local_tempdir()
  result <- local_files(tmp)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result),
               c("repo_url", "file_name", "file_url",
                 "file_location", "file_size", "file_type"))
  expect_equal(nrow(result), 0)
})

test_that("local_files column structure", {
  path <- test_path("fixtures", "code_files")
  result <- local_files(path)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result),
               c("repo_url", "file_name", "file_url",
                 "file_location", "file_size", "file_type"))

  # repo_url is the path for every row
  expect_true(all(result$repo_url == (path)))

  # file_name is the basename of file_location
  expect_equal(result$file_name, basename(result$file_location))

  # file_url is always NA (no remote URL for local files)
  expect_true(all(is.na(result$file_url)))

  # every file_location path actually exists on disk
  expect_true(all(file.exists(result$file_location)))

  # file_size is numeric and positive for non-empty files
  expect_type(result$file_size, "double")
  expect_true(all(result$file_size > 0))
})

test_that("local_files finds files recursively", {
  path <- test_path("fixtures", "code_files")
  result <- local_files(path, recursive = TRUE)

  # top-level files
  expect_true("analysis.R" %in% result$file_name)
  expect_true("analysis_no_comments.R" %in% result$file_name)
  expect_true("data.csv" %in% result$file_name)
  expect_true("README.md" %in% result$file_name)

  # file in subdirectory
  expect_true("helper.R" %in% result$file_name)

  expect_equal(nrow(result), 7)

  # not recursive
  result <- local_files(path, recursive = FALSE)
  expect_equal(nrow(result), 6)
  expect_disjoint("helper.R", result$file_name)
})

test_that("local_files file type detection", {
  path <- test_path("fixtures", "code_files")
  result <- local_files(path)

  r_row     <- result[result$file_name == "analysis.R", ]
  csv_row   <- result[result$file_name == "data.csv", ]
  readme_row <- result[result$file_name == "README.md", ]

  expect_equal(r_row$file_type,      "code")
  expect_equal(csv_row$file_type,    "data")
  expect_equal(readme_row$file_type, "readme")
})

test_that("local_files vectorised input", {
  tmp1 <- withr::local_tempdir()
  tmp2 <- withr::local_tempdir()
  writeLines("# script\nlibrary(dplyr)", file.path(tmp1, "script1.R"))
  writeLines("x,y\n1,2",               file.path(tmp2, "data.csv"))

  result <- local_files(c(tmp1, tmp2))

  expect_equal(nrow(result), 2)
  expect_true("script1.R" %in% result$file_name)
  expect_true("data.csv"  %in% result$file_name)

  # each file carries its own repo_url
  expect_equal(result$repo_url[result$file_name == "script1.R"], (tmp1))
  expect_equal(result$repo_url[result$file_name == "data.csv"],  (tmp2))
})


test_that("local_files file without extension gets NA type", {
  tmp <- withr::local_tempdir()
  writeLines("content", file.path(tmp, "Makefile"))

  result <- local_files(tmp)
  make_row <- result[result$file_name == "Makefile", ]
  expect_true(is.na(make_row$file_type))
})

