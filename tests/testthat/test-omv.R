# Tests for the jamovi (.omv) reader. A .omv is a zip of binary blobs, so the
# fixture is built here from the known on-disk format (metadata.json + data.bin +
# strings.bin + an analysis blob) and round-tripped through read_omv(). This
# mirrors read_jasp(), whose contract read_omv() reproduces.

# Build a minimal but structurally real .omv archive on disk; return its path.
# 3 rows, one column of each dataType: Integer (with a value label), Decimal, Text.
make_omv <- function(dir) {
  meta <- list(dataSet = list(rowCount = 3L, fields = list(
    list(name = "grp",  dataType = "Integer", measureType = "Nominal",
         columnType = "Data",
         labels = list(list(1, "Control"), list(2, "Treatment"))),
    list(name = "score", dataType = "Decimal", measureType = "Continuous",
         columnType = "Data", description = "Total score"),
    list(name = "note", dataType = "Text", measureType = "Nominal",
         columnType = "Data"))))
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE), file.path(dir, "metadata.json"))
  writeLines("{}", file.path(dir, "xdata.json"))

  # data.bin: column-major. grp int32 {1,2,1}; score double {1.5,2.5,NaN};
  # note Text int32 index {0,1,0} into strings.bin.
  con <- file(file.path(dir, "data.bin"), "wb")
  writeBin(as.integer(c(1, 2, 1)), con, size = 4, endian = "little")
  writeBin(c(1.5, 2.5, NaN),       con, size = 8, endian = "little")
  writeBin(as.integer(c(0, 1, 0)), con, size = 4, endian = "little")
  close(con)

  # strings.bin: NUL-separated pool "yes","no".
  con <- file(file.path(dir, "strings.bin"), "wb")
  writeBin(charToRaw("yes"), con); writeBin(as.raw(0), con)
  writeBin(charToRaw("no"),  con); writeBin(as.raw(0), con)
  close(con)

  # One analysis blob carrying a reproducible jmv:: call, framed as jamovi does:
  # the protobuf string-field tag (0x52) then a varint length byte (non-printable)
  # then the call. Real files always have that non-printable byte before the
  # package name (verified across the corpus), which the reader turns into a
  # separator so the leftward walk stops before the tag.
  dir.create(file.path(dir, "01 ttestIS"), showWarnings = FALSE)
  blob <- c(as.raw(0x52), as.raw(0x01),
            charToRaw("jmv::ttestIS(vars = vars(score), group = grp, students = TRUE)"))
  writeBin(blob, file.path(dir, "01 ttestIS", "analysis"))

  omv <- file.path(dir, "fixture.omv")
  wd <- setwd(dir); on.exit(setwd(wd))
  utils::zip(basename(omv),
             c("metadata.json", "xdata.json", "data.bin", "strings.bin",
               "01 ttestIS/analysis"),
             flags = "-q")
  file.path(dir, "fixture.omv")
}

test_that("read_omv decodes Integer, Decimal and Text columns", {
  skip_if_not(nzchar(Sys.which("zip")), "zip utility not available")
  d <- withr::local_tempdir()
  omv <- make_omv(d)
  r <- read_omv(omv)
  expect_equal(r$format, "jamovi")
  expect_equal(nrow(r$data), 3L)
  # ignore_attr: the grp/score columns correctly carry label/labels attributes;
  # compare values only.
  expect_equal(r$data$grp, c(1L, 2L, 1L), ignore_attr = TRUE)
  expect_equal(r$data$score, c(1.5, 2.5, NA), ignore_attr = TRUE)   # NaN -> NA
  expect_equal(r$data$note, c("yes", "no", "yes"))     # strings.bin resolved
})

test_that("read_omv attaches haven-style labels and variable label", {
  skip_if_not(nzchar(Sys.which("zip")), "zip utility not available")
  d <- withr::local_tempdir()
  r <- read_omv(make_omv(d))
  expect_equal(attr(r$data$grp, "labels"),
               stats::setNames(c(1, 2), c("Control", "Treatment")))
  expect_equal(attr(r$data$score, "label"), "Total score")
})

test_that("read_omv recovers the analysis R syntax (no framing byte)", {
  skip_if_not(nzchar(Sys.which("zip")), "zip utility not available")
  d <- withr::local_tempdir()
  r <- read_omv(make_omv(d))
  expect_length(r$analyses, 1L)
  expect_match(r$analyses[[1]], "ttestIS", fixed = TRUE)
  expect_match(r$analyses[[1]], "jmv::ttestIS(", fixed = TRUE)
  expect_false(grepl("Rjmv", r$analyses[[1]], fixed = TRUE))   # framing byte stripped
  expect_match(r$analyses[[1]], "vars(score)", fixed = TRUE)   # nested paren kept
})

test_that("read_omv errors on a non-omv file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "not.omv")
  writeLines("not a zip", f)
  expect_error(read_omv(f))
})

# ── real fixture (a small published jamovi file, fixtures/formats/sample.omv) ──

test_that("read_omv reads a real .omv file", {
  r <- read_omv(test_path("fixtures", "formats", "sample.omv"))
  expect_equal(r$format, "jamovi")
  expect_equal(nrow(r$data), 218L)
  expect_equal(ncol(r$data), 9L)
  # It carries at least one recovered analysis with reproducible jmv:: syntax.
  expect_true(length(r$analyses) >= 1L)
  expect_true(any(grepl("jmv::", r$analyses, fixed = TRUE)))
})
