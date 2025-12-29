test_that("exists", {
  expect_true(is.function(metacheck::pubpeer_comments))
  expect_no_error(helplist <- help(pubpeer_comments, metacheck))
})

test_that("errors", {
  expect_error(pubpeer_comments(bad_arg))
})

# httptest::start_capturing()

httptest::use_mock_api()

test_that("defaults", {
  # both with comments
  doi <- c("10.1038/s41598-025-24662-9",
           "10.1177/0146167211398138")

  pp <- pubpeer_comments(doi)
  expect_equal(pp$doi, doi)
  expect_true(pp$total_comments[[1]] >= 16)

  # one with comments
  doi <- c("10.1038/s41598-025-24662-9",
           "10.1016/j.tics.2006.06.010")

  pp <- pubpeer_comments(doi)
  expect_equal(pp$doi, doi)
  expect_equal(pp$total_comments[[2]], 0)

  # none with comments
  doi <- c("10.1016/j.tics.2006.06.010")

  pp <- pubpeer_comments(doi)
  expect_equal(pp$doi, doi)
  expect_equal(pp$total_comments[[1]], 0)

  # invalid DOI
  doi <- c("nope/hasd")

  pp <- pubpeer_comments(doi)
  expect_equal(pp$doi, doi)
  expect_equal(pp$total_comments[[1]], 0)

  # one NA
  doi <- c("10.1038/s41598-025-24662-9",
           NA)

  pp <- pubpeer_comments(doi)
  expect_equal(pp$doi, doi)
  expect_equal(pp$url[[2]], NA_character_)
  expect_equal(pp$total_comments[[2]], 0)

  # empty doi
  pp <- pubpeer_comments(c())
  expect_null(pp)
})

httptest::stop_mocking()
# httptest::stop_capturing()
