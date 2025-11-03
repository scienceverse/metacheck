test_that("exists", {
  expect_true(is.function(metacheck::retractionwatch))
  expect_true(is.function(metacheck::rw))
  expect_true(is.function(metacheck::rw_date))
  expect_true(is.function(metacheck::rw_update))
  expect_no_error(helplist <- help(retractionwatch, metacheck))
})

test_that("errors", {
  expect_error(retractionwatch(bad_arg))
})

test_that("defaults", {
  rw <- retractionwatch()
  expect_equal(names(rw), c("doi", "retractionwatch"))
  expect_equal(rw, rw())

  date <- rw_date()
  expect_equal(date, attr(rw, "date"))
})

test_that("update", {
  skip("long process")
  skip_on_cran()
  skip_on_covr()
  skip_if_offline("api.labs.crossref.org")

  path <- rw_update()
  expect_true(grepl("retractionwatch\\.Rds$", path))
  expect_true(file.exists(path))
})
