# Tests for the JASP (.jasp) reader, against a small real published .jasp file
# committed under fixtures/formats/. import_jasp() returns a labelled data frame
# (haven-style label/labels attributes), a column table, and the parsed analyses,
# so downstream codebook extraction treats a .jasp like a .sav. import_omv() is
# its jamovi counterpart (see test-omv.R).

test_that("import_jasp reads a real binary-format .jasp file", {
  r <- import_jasp(test_path("fixtures", "formats", "sample.jasp"))
  expect_equal(r$format, "binary")
  expect_equal(nrow(r$data), 32L)
  expect_equal(ncol(r$data), 40L)
  expect_true("Age" %in% names(r$data))
})

test_that("import_jasp attaches haven-style value labels", {
  r <- import_jasp(test_path("fixtures", "formats", "sample.jasp"))
  # Several columns carry value labels (measure type + coding), like a .sav.
  n_labelled <- sum(vapply(r$data,
    function(c) !is.null(attr(c, "labels")), logical(1)))
  expect_gt(n_labelled, 0L)
})

test_that("import_jasp recovers the stored analyses", {
  r <- import_jasp(test_path("fixtures", "formats", "sample.jasp"))
  # JASP stores analyses.json as structured JSON; the reader returns it parsed.
  expect_false(is.null(r$analyses))
  summ <- .jasp_analyses_summary(r$analyses)
  expect_true(length(summ) >= 1L)
})

test_that("import_jasp errors on a non-jasp file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "not.jasp")
  writeLines("not a zip", f)
  expect_error(import_jasp(f))
})

# ── modern SQLite format ──────────────────────────────────────────────────────
# The only SQLite-format .jasp files in the corpus are multi-MB, too large to
# commit as fixtures. Build a minimal one here from the documented schema
# (Columns / DataSet_<n> with Column_<id>_DBL|INT / Labels) so the SQLite reader
# path (.read_jasp_sqlite) is exercised without a heavy binary fixture.
make_sqlite_jasp <- function(dir) {
  sq <- file.path(dir, "internal.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), sq)
  DBI::dbExecute(con, "CREATE TABLE Columns (id INT, name TEXT, columnType TEXT, colIdx INT, title TEXT)")
  DBI::dbExecute(con, "INSERT INTO Columns VALUES (1,'grp','nominal',0,'grp'),(2,'score','scale',1,'Total')")
  DBI::dbExecute(con, "CREATE TABLE DataSet_1 (rowNumber INT, Column_1_INT INT, Column_2_DBL REAL)")
  DBI::dbExecute(con, "INSERT INTO DataSet_1 VALUES (0,1,1.5),(1,2,2.5),(2,1,3.5)")
  DBI::dbExecute(con, "CREATE TABLE Labels (columnId INT, value INT, ordering INT, label TEXT)")
  DBI::dbExecute(con, "INSERT INTO Labels VALUES (1,1,0,'Control'),(1,2,1,'Treatment')")
  DBI::dbExecute(con, "CREATE TABLE DataSets (dataFilePath TEXT)")
  DBI::dbExecute(con, "INSERT INTO DataSets VALUES ('orig.csv')")
  DBI::dbDisconnect(con)

  jasp <- file.path(dir, "sqlite.jasp")
  wd <- setwd(dir); on.exit(setwd(wd))
  utils::zip(basename(jasp), "internal.sqlite", flags = "-q")
  jasp
}

test_that("import_jasp reads the modern SQLite format", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not(nzchar(Sys.which("zip")), "zip utility not available")
  d <- withr::local_tempdir()
  r <- import_jasp(make_sqlite_jasp(d))
  expect_equal(r$format, "sqlite")
  expect_equal(nrow(r$data), 3L)
  expect_equal(r$data$grp, c(1L, 2L, 1L), ignore_attr = TRUE)
  expect_equal(r$data$score, c(1.5, 2.5, 3.5), ignore_attr = TRUE)
  expect_equal(attr(r$data$grp, "labels"),
               stats::setNames(c(1, 2), c("Control", "Treatment")))
})
