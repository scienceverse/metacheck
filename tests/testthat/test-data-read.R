# Tests for data_read_head() — the file-reading layer shared by data_check and
# the data modules — and its encoding/robustness handling. All offline.

test_that("data_read_head reads delimited text and detects the delimiter", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), csv,
                   row.names = FALSE)
  df <- data_read_head(csv, n_rows = Inf)
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("a", "b"))
  expect_equal(nrow(df), 3)

  # tab-separated, header present
  tsv <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("id\tval", "1\t10", "2\t20"), tsv)
  dft <- data_read_head(tsv, n_rows = Inf)
  expect_equal(names(dft), c("id", "val"))
  expect_equal(nrow(dft), 2)
})

test_that("data_read_head honours n_rows", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:100), csv, row.names = FALSE)
  expect_equal(nrow(data_read_head(csv, n_rows = 5)), 5)
  expect_equal(nrow(data_read_head(csv, n_rows = Inf)), 100)
})

test_that("data_read_head returns NULL for unsupported formats", {
  f <- withr::local_tempfile(fileext = ".xyz")
  writeLines("nothing", f)
  expect_null(data_read_head(f))
})

test_that("data_read_head sanitises invalid-UTF-8 column names when present", {
  # When a data frame comes back carrying a header with an invalid-UTF-8 byte
  # (a stray Latin-1 / BOM byte some readers tolerate), data_read_head must
  # coerce the names to valid UTF-8 so downstream grepl(..., perl = TRUE) name
  # checks do not crash with "invalid multibyte string". We drive the read via a
  # readable .rds so the name reaches the sanitisation step regardless of the
  # platform's CSV-reader tolerance for header bytes.
  bad <- paste0(rawToChar(as.raw(0xef)), "PeronalData_fullname")
  d <- data.frame(1:3, 4:6)
  names(d) <- c(bad, "ok")
  rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(d, rds)

  df <- data_read_head(rds, n_rows = Inf)
  expect_s3_class(df, "data.frame")
  expect_false(any(is.na(iconv(names(df), "UTF-8", "UTF-8"))))
  expect_no_error(grepl("id", names(df), perl = TRUE))
})

test_that("data_col_type survives an invalid-UTF-8 column name", {
  # The specific downstream call that used to crash in the sweep.
  bad <- paste0(rawToChar(as.raw(0xef)), "PeronalData_fullname")
  expect_no_error(res <- data_col_type(bad, 1:10))
  expect_type(res, "list")
})
