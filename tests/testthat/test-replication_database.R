test_that("exists", {
  expect_true(is.function(metacheck::FReD))
  expect_no_error(helplist <- help(FReD, metacheck))

  expect_true(is.function(metacheck::FReD_date))
  expect_no_error(helplist <- help(FReD_date, metacheck))

  expect_true(is.function(metacheck::FReD_update))
  expect_no_error(helplist <- help(FReD_update, metacheck))
})

test_that("FReD", {
  f <- FReD()
  expect_true(nrow(f) >= 2222) # might get larger in the future
  expect_equal(ncol(f), 4)

  # has a date format
  d <- FReD_date()
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2}", d))
  expect_equal(attr(f, "date"), d)
})

test_that("update", {
  skip_if_quick()

  path <- FReD_update()
  expect_true(grepl("FReD\\.Rds$", path))
  expect_true(file.exists(path))
})
