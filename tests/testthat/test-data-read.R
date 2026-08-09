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

test_that("data_read_head recovers a data frame from an .RData workspace", {
  skip_if_not_installed("processx")
  f <- withr::local_tempfile(fileext = ".RData")
  study_data <- data.frame(id = 1:5, score = rnorm(5))
  a_model <- lm(score ~ id, study_data)   # a non-data object alongside the data
  save(study_data, a_model, file = f)

  df <- data_read_head(f, n_rows = Inf)
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("id", "score"))
  expect_equal(nrow(df), 5)
})

test_that("data_read_head returns NULL for an .RData with no data frame", {
  skip_if_not_installed("processx")
  # A workspace of only non-data objects (a model, a vector) -> no reusable
  # tabular data -> NULL (data_check turns this into a sharing recommendation).
  f <- withr::local_tempfile(fileext = ".RData")
  a_model <- lm(mpg ~ cyl, mtcars)
  some_numbers <- 1:100
  save(a_model, some_numbers, file = f)

  expect_null(data_read_head(f, n_rows = Inf))
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

test_that("data_read_head repairs invalid-UTF-8 values, not just names", {
  # A Latin-1 byte inside a value of a nominally-UTF-8 CSV (here 0xEA, "ê",
  # from a mis-encoded apostrophe — the openmind.opmia00203 case) survives
  # fread's encoding = "UTF-8" as an invalid-UTF-8 string, and the first base
  # regex call in data_check then dies with "input string N is invalid UTF-8".
  # data_read_head must return values that are all valid UTF-8, with the
  # invalid bytes reinterpreted as Latin-1 rather than dropped.
  csv <- withr::local_tempfile(fileext = ".csv")
  con <- file(csv, open = "wb")
  writeLines(c("id,sentence", "1,ok", "2,ok"), con, useBytes = TRUE)
  writeBin(charToRaw("3,She\xead like\n"), con)
  close(con)

  df <- data_read_head(csv, n_rows = Inf)
  expect_s3_class(df, "data.frame")
  expect_true(all(validUTF8(df$sentence)))
  expect_no_error(grepl("a", df$sentence))
  # The Latin-1 byte is preserved as its UTF-8 equivalent (0xEA -> U+00EA),
  # not stripped.
  expect_identical(df$sentence[3], "She\u00ead like")
  # The repair is recorded per column, so data_check can carry it into its
  # table and data_validate can raise the "Mixed encoding" warning.
  expect_identical(attr(df, "utf8_repaired"), c(sentence = 1L))
})

test_that("data_read_head reads a CSV with Latin-1 bytes in its first lines", {
  # The collabra.102 corpus case: a Latin-1 byte in the header or the FIRST
  # data row (unlike the row-3 byte above) hits the pre-read sniffers —
  # .sniff_delimiter / .detect_header / .is_single_field_blob run trimws /
  # strsplit on raw readLines() output — which used to error with "input
  # string 1 is invalid UTF-8", so the whole file was skipped before fread's
  # tolerant read and the post-read repair ever ran.
  csv <- withr::local_tempfile(fileext = ".csv")
  con <- file(csv, open = "wb")
  writeBin(charToRaw("id,caf\xe9\n1,caf\xe9 study\n2,plain\n"), con)
  close(con)

  expect_no_warning(df <- data_read_head(csv, n_rows = Inf))
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 2L)
  # Header and values come back as valid UTF-8 with the byte reinterpreted as
  # Latin-1 (0xE9 -> U+00E9), not dropped.
  expect_identical(names(df)[2], "caf\u00e9")
  expect_identical(df[[2]][1], "caf\u00e9 study")
  expect_true(all(validUTF8(df[[2]])))
})

# ── Qualtrics multi-row header handling ───────────────────────────────────────

# Write a minimal Qualtrics "use choice text" export: machine-name header, a
# question-text row, an ImportId JSON row, then `n` data rows.
.write_qualtrics <- function(path, n = 6, status = NULL, finished = NULL,
                             durations = NULL) {
  q <- function(...) paste0('"', c(...), '"', collapse = ",")
  hdr  <- q("StartDate", "EndDate", "Status", "IPAddress", "Progress",
            "Duration (in seconds)", "Finished", "RecordedDate", "ResponseId")
  qtxt <- q("Start Date", "End Date", "Response Type", "IP Address", "Progress",
            "Duration (in seconds)", "Finished", "Recorded Date", "Response ID")
  imp  <- q('{"ImportId":"startDate"}', '{"ImportId":"endDate"}',
            '{"ImportId":"status"}', '{"ImportId":"ipAddress"}',
            '{"ImportId":"progress"}', '{"ImportId":"duration"}',
            '{"ImportId":"finished"}', '{"ImportId":"recordedDate"}',
            '{"ImportId":"_recordId"}')
  status    <- status    %||% rep(0, n)
  finished  <- finished  %||% rep(1, n)
  durations <- durations %||% rep(300, n)
  rows <- vapply(seq_len(n), function(i) q(
    sprintf("2021-05-%02d 10:00:00", i), sprintf("2021-05-%02d 10:05:00", i),
    status[i], sprintf("192.168.0.%d", i), 100, durations[i], finished[i],
    sprintf("2021-05-%02d 10:05:00", i),
    sprintf("R_%s", paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))),
    character(1))
  writeLines(c(hdr, qtxt, imp, rows), path)
}

test_that("data_read_head strips Qualtrics header rows and re-types columns", {
  csv <- withr::local_tempfile(fileext = ".csv")
  .write_qualtrics(csv, n = 6)
  df <- data_read_head(csv, n_rows = Inf)
  # The question-text and ImportId rows are gone: 6 data rows remain.
  expect_equal(nrow(df), 6)
  # Numeric metadata columns are numeric again (the junk rows had forced text).
  expect_true(is.numeric(df[["Duration (in seconds)"]]))
  expect_true(is.numeric(df[["Progress"]]))
  expect_equal(df[["Duration (in seconds)"]], rep(300, 6))
})

test_that("data_read_head leaves a non-Qualtrics CSV untouched", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(id = 1:5, StartDate = Sys.Date() + 0:4,
                              score = rnorm(5)), csv, row.names = FALSE)
  df <- data_read_head(csv, n_rows = Inf)
  expect_equal(nrow(df), 5)          # a lone StartDate is not a Qualtrics export
})

test_that("data_read_head skips a single-big-field (blob) file quickly", {
  # A .csv that is really one giant value under a single header (JSON, XML, ...)
  # is not tabular. It must be skipped (NULL) — and cheaply, not via a slow read.
  blob <- withr::local_tempfile(fileext = ".csv")
  json <- paste0("\"{", paste(sprintf("\"\"k%d\"\":%d", 1:5000, 1:5000),
                              collapse = ","), "}\"")
  writeLines(c("studyRunData", json), blob)
  t <- system.time(df <- data_read_head(blob, n_rows = Inf))
  expect_null(df)
  expect_lt(t[["elapsed"]], 2)       # cheap bail, not the multi-second read

  # A genuine one-column CSV with short rows must NOT be caught.
  ok <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("score", as.character(round(rnorm(20), 3))), ok)
  expect_equal(nrow(data_read_head(ok, n_rows = Inf)), 20)
})

test_that("data_read_head reads a table with large quoted cells fast", {
  # A real table whose cells hold big quoted values full of commas (e.g. numpy
  # array dumps) is genuinely tabular but murders base read.delim's quote scanner
  # (minutes). fread handles it in a moment; verify it reads correctly and fast.
  skip_if_not_installed("data.table")
  f <- withr::local_tempfile(fileext = ".csv")
  # column 3 is a large quoted field containing thousands of commas.
  blob_cell <- paste0("\"[", paste(rep("1.0", 5000), collapse = ","), "]\"")
  cells <- vapply(1:5, function(i)
    sprintf("%d,r%d,%s", i, i, blob_cell), character(1))
  writeLines(c("id,label,arr", cells), f)
  t <- system.time(df <- data_read_head(f, n_rows = Inf))
  expect_false(is.null(df))
  expect_equal(nrow(df), 5)
  expect_equal(names(df), c("id", "label", "arr"))
  expect_lt(t[["elapsed"]], 5)   # seconds, not minutes
})

test_that("data_read_head reads .ods like .xlsx", {
  skip_if_not_installed("readODS")
  p <- file.path(tempdir(), paste0("rh_", as.integer(runif(1, 1, 1e6)), ".ods"))
  readODS::write_ods(data.frame(id = 1:4, grp = c("a", "b", "a", "b")), p)

  df <- data_read_head(p, n_rows = 3)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_equal(names(df), c("id", "grp"))
})

test_that(".ods is treated as tabular data", {
  expect_equal(data_format("ods"), "tabular")
  expect_equal(data_format("fods"), "tabular")
})
