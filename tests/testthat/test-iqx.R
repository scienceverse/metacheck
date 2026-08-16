# Tests for the Inquisit (.iqx) script reader and the two ways its content feeds
# the Behaverse paradata instrument (Feed B: title/description; Feed A: LLM task
# naming when the title is weak). All run offline; Feed A is exercised only on the
# llm_use(FALSE) fallback path (no network), per the LLM-testing convention.

# Write a minimal .iqx script; return its path.
write_iqx <- function(dir, name, lines) {
  p <- file.path(dir, name)
  writeLines(lines, p)
  p
}

test_that("read_iqx extracts title, description, items and data file", {
  d <- withr::local_tempdir()
  p <- write_iqx(d, "gen.iqx", c(
    "title: Semantic Generalization Task",
    "Trains a response then tests generalization to new stimuli.",
    "**********",
    "<item cue>",
    "/ 1 = \"boo\"",
    "/ 2 = \"kef\"",
    "</item>",
    "<item filler>",
    "/ 1 = \"a neutral sentence\"",
    "</item>",
    "<data>",
    "/ file = \"gen.iqdat\"",
    "</data>"))
  r <- read_iqx(p)
  expect_equal(r$title, "Semantic Generalization Task")
  expect_match(r$description, "generalization", ignore.case = TRUE)
  expect_true("boo" %in% r$items)
  expect_true("a neutral sentence" %in% r$items)
  expect_equal(r$data_file, "gen.iqdat")
  expect_equal(r$stem, "gen")
})

test_that("read_iqx keeps real wording but drops bare element references", {
  d <- withr::local_tempdir()
  p <- write_iqx(d, "t.iqx", c(
    "<item statements>", "/ 1 = \"Niffites are direct\"", "</item>",
    "<text ref>", "/ items = \"statements\"", "</text>"))    # bare ref, not wording
  r <- read_iqx(p)
  expect_true("Niffites are direct" %in% r$items)
  expect_false("statements" %in% r$items)
})

test_that("read_iqx errors on a missing file", {
  expect_error(read_iqx(file.path(tempdir(), "no_such.iqx")))
})

# ── Feed B: .iqx title/description enrich the paired Behaverse instrument ──────

test_that("paradata Instrument is named from a paired .iqx (Feed B)", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"))
  writeLines(c("subject\tblocknum\tblockcode\ttrialcode\tlatency\ttrialnum",
               "01\t1\tgeneralization_1\tt1\t845\t1"),
             file.path(d, "data", "generalization_1_01_2019.iqdat"))
  write_iqx(file.path(d, "data"), "generalization_1.iqx", c(
    "title: Semantic Generalization Task",
    "Trains then tests generalization.",
    "<item cue>", "/ 1 = \"boo\"", "</item>"))
  idx <- metacheck:::.osd_write_paradata(d, study_name = "test")
  doc <- jsonlite::fromJSON(
    file.path(d, "paradata", "generalization_1.json"), simplifyVector = FALSE)
  expect_equal(doc$Instrument[[1]]$name, "Semantic Generalization Task")
  expect_match(doc$Instrument[[1]]$description, "generalization", ignore.case = TRUE)
  expect_true(behaverse_validate(doc)$valid)
})

# ── Feed A: weak-title detection + llm_use(FALSE) fallback ─────────────────────

test_that(".iqx_title_is_weak flags generic/opaque titles only", {
  expect_true(metacheck:::.iqx_title_is_weak("block_1", "generalization_1"))
  expect_true(metacheck:::.iqx_title_is_weak("batch", "x"))
  expect_true(metacheck:::.iqx_title_is_weak("", "x"))
  expect_true(metacheck:::.iqx_title_is_weak("generalization_1", "generalization_1"))
  expect_false(metacheck:::.iqx_title_is_weak("Implicit Association Test", "x"))
})

test_that("Feed A is a no-op under llm_use(FALSE)", {
  withr::local_options(list())
  old <- llm_use(); on.exit(llm_use(old), add = TRUE)
  llm_use(FALSE)
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"))
  writeLines(c("subject\tblocknum\tblockcode\ttrialcode\tlatency\ttrialnum",
               "01\t1\tblock_1\tt1\t845\t1"),
             file.path(d, "data", "block_1_01.iqdat"))
  write_iqx(file.path(d, "data"), "block_1.iqx", c(
    "title: block_1", "An implicit measure.",
    "<item cue>", "/ 1 = \"flowers are pleasant\"", "</item>"))
  idx <- metacheck:::.osd_write_paradata(d, study_name = "test")
  doc <- jsonlite::fromJSON(
    file.path(d, "paradata", "block_1.json"), simplifyVector = FALSE)
  # LLM off + weak title -> falls back to the instrument code, no error.
  expect_equal(doc$Instrument[[1]]$name, "block_1")
})
