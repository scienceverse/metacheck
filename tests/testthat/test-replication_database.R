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

test_that("update", {
  skip_osf()

  path <- FLoRA_update()
  expect_true(grepl("FLoRA\\.Rds$", path))
  expect_true(file.exists(path))
})
