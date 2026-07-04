# Unit tests for plumber upload validation.
# Sources validators.R directly — no running API needed.

source(testthat::test_path("..", "..", "inst", "plumber", "utils", "validators.R"))

test_that("rejects missing file", {
  v <- validate_file_upload(NULL)
  expect_false(v$valid)
  expect_equal(v$status, 400)
})

test_that("rejects multiple files", {
  v <- validate_file_upload(c("a.json", "b.json"))
  expect_false(v$valid)
  expect_equal(v$status, 400)
})

test_that("rejects nonexistent file", {
  v <- validate_file_upload(tempfile(fileext = ".json"))
  expect_false(v$valid)
  expect_equal(v$status, 400)
})

test_that("accepts an existing file under the size cap", {
  f <- tempfile(fileext = ".json")
  writeLines('{"paper_id": "x"}', f)
  v <- validate_file_upload(f)
  expect_true(v$valid)
})

test_that("rejects files over the size cap", {
  f <- tempfile(fileext = ".json")
  writeLines('{"paper_id": "x"}', f)
  v <- validate_file_upload(f, max_bytes = 5)
  expect_false(v$valid)
  expect_equal(v$status, 413)
})

test_that("default cap is 50MB", {
  expect_equal(MAX_UPLOAD_BYTES, 50 * 1024 * 1024)
})
