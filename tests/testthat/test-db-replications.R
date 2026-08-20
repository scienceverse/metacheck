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
  expect_true(nrow(f) >= 700) # might get larger in the future
  expect_equal(ncol(f), 8)

  # has a date format
  d <- FLoRA_date()
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2}", d))
  expect_equal(attr(f, "date"), d)
})

# "update" (FLoRA_update() downloads flora.csv from the OSF and checks the
# result path) is removed rather than kept: it only ever asserted
# file.exists(path), never the file's content or size, so its entire value
# was covered by osf_file_download()'s own tests -- but its mock fixture
# (tests/testthat/apis/osf.io/download/t4j8f.R) recorded the real 5.6MB
# flora.csv, a 37MB text file checked into the repo for a test that did not
# need a single byte of it. That cost -- repo bloat, slow clones/checkouts,
# CI cache size -- was paid on every commit, for a test gated behind
# skip_if_quick() that consequently never ran in a normal devtools::test()
# pass anyway (see issue #364). Not worth reconstructing a smaller fixture
# for; deleted along with the fixture file.
