test_that("exists", {
  expect_true(is.function(metacheck::FLoRA))
  expect_no_error(helplist <- help(FLoRA, metacheck))

  expect_true(is.function(metacheck::FLoRA_date))
  expect_no_error(helplist <- help(FLoRA_date, metacheck))

  expect_true(is.function(metacheck::FLoRA_update))
  expect_no_error(helplist <- help(FLoRA_update, metacheck))
})

test_that("FLoRA", {
  f <- FLoRA()
  expect_true(nrow(f) >= 2800) # might get larger in the future
  expect_equal(ncol(f), 10)

  # has a date format
  d <- FLoRA_date()
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2}", d))
  expect_equal(attr(f, "date"), d)

  # names the Zenodo release it comes from
  expect_match(attr(f, "version"), "^\\d{4}\\.\\d{2}\\.\\d+$")
  expect_match(attr(f, "doi"), "^10\\.5281/zenodo\\.\\d+$")

  # a reproduction has an outcome (the release leaves outcome empty for it)
  expect_false(anyNA(f$outcome[f$type == "reproduction"]))
})

test_that(".FLoRA_summarise() keeps usable rows and combines reproduction outcomes", {
  raw <- data.frame(
    doi_o = c("10.1/a", "10.1/b", NA, "10.1/d", "10.1/a"),
    apa_ref_o = "Ref O",
    doi_r = c("10.2/a", NA, "10.2/c", NA, "10.2/a"),
    apa_ref_r = "Ref R",
    url_r = c(NA, "https://osf.io/abcde/", NA, NA, NA),
    title_r = "Title R",
    alt_identifier_r = c("10.31234/osf.io/abcde", NA, NA, NA, "10.31234/osf.io/abcde"),
    outcome = c("successful", NA, "failed", "failed", "successful"),
    outcome_quote = c("q", NA, "q", "q", "q"),
    type = c("replication", "reproduction", "replication", "replication", "replication"),
    outcome_computation = c(NA, "computationally reproducible", NA, NA, NA),
    outcome_computational_quote = c(NA, "same numbers", NA, NA, NA),
    outcome_robustness = c(NA, "not checked", NA, NA, NA),
    outcome_robustness_quote = NA,
    source = "FReD"
  )
  f <- .FLoRA_summarise(raw)
  # no original DOI, no replication DOI or URL, and a duplicate are left out
  expect_equal(f$doi_o, c("10.1/a", "10.1/b"))
  expect_equal(f$outcome, c("successful", "computationally reproducible, robustness not checked"))
  expect_equal(f$outcome_quote[2], "Computational reproducibility: same numbers")
})

test_that("FLoRA() uses a downloaded copy from the same or a later release date", {
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())
  builtin <- readRDS(system.file("databases/FLoRA.Rds", package = "metacheck"))
  dir <- tools::R_user_dir("metacheck", "data")
  dir.create(dir, FALSE, TRUE)
  save_copy <- function(x) saveRDS(x, file.path(dir, "FLoRA.Rds"))

  # a release from the same day or later is used
  later <- builtin[1:10, ]
  save_copy(later)
  expect_equal(nrow(FLoRA()), 10)
  attr(later, "date") <- attr(builtin, "date") + 1
  save_copy(later)
  expect_equal(nrow(FLoRA()), 10)
  expect_equal(FLoRA_date(), attr(builtin, "date") + 1)

  # but not an earlier one (downloaded before metacheck was updated)
  earlier <- later
  attr(earlier, "date") <- attr(builtin, "date") - 1
  save_copy(earlier)
  expect_equal(nrow(FLoRA()), nrow(builtin))

  # nor a former OSF download (lacks columns metacheck uses, dated by download day)
  osf <- as.data.frame(builtin[1:10, c("doi_o", "apa_ref_o", "doi_r", "apa_ref_r",
                                       "url_r", "outcome", "outcome_quote", "type")])
  attr(osf, "date") <- Sys.Date() + 1
  save_copy(osf)
  expect_equal(nrow(FLoRA()), nrow(builtin))
})

test_that("FLoRA_update() downloads the latest Zenodo release", {
  skip_if_quick()
  skip_on_cran()
  skip_if_offline("zenodo.org")
  withr::local_envvar(R_USER_DATA_DIR = withr::local_tempdir())

  path <- FLoRA_update()
  f <- readRDS(path)
  expect_true(nrow(f) >= 2800)
  expect_s3_class(attr(f, "date"), "Date")
  expect_match(attr(f, "doi"), "^10\\.5281/zenodo\\.\\d+$")
  builtin <- readRDS(system.file("databases/FLoRA.Rds", package = "metacheck"))
  expect_false(attr(f, "date") < attr(builtin, "date"))
  # FLoRA() gives the latest release, downloaded or built in
  expect_equal(attr(FLoRA(), "version"), attr(f, "version"))
})
