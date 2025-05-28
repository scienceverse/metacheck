#setwd("tests/testthat/")

test_that("exists", {
  expect_true(is.function(papercheck::validate))
  expect_no_error(helplist <- help(validate, papercheck))
})

test_that("errors", {
  paper <- psychsci[1:10]
  module <- "marginal"

  expect_error(validate())
  expect_error(validate(paper))
  expect_error(validate(paper, "not-a-module"))
  expect_error(validate(paper, module),
               "The results of this module did not return any objects")
  expect_error(validate(paper, module, x = 1),
               "The results of this module did not return any objects named: x")

  # expected table has columns not in the actual results
  expected <- module_run(paper, module)
  exp_sum <- expected$summary
  exp_sum$extra_col <- 1
  expect_error(validate(paper, module, summary = exp_sum),
               "The `summary` table did not have the same columns as the expected table")

  # table with no ID column
  exp_sum <- exp_sum[, 2, drop = FALSE]
  expect_error(validate(paper, module, summary = exp_sum),
               "The `summary` table must have an `id` column")
})

test_that("basic", {
  paper <- psychsci[1:10]
  module <- "marginal"
  expected <- module_run(paper, module)
  exp_summary <- expected$summary[1:9, ]
  exp_summary[1,2] <- 5 # change some expected values
  exp_summary[2,2] <- NA

  v <- validate(paper, module, summary = exp_summary)

  expect_equal(v$module, module)
  expect_equal(v$actual$summary, expected$summary)
  expect_equal(names(v$matches), "summary")
  #expect_equal(v$matches$summary$expected, c(exp_summary$marginal, NA))
  #expect_equal(v$matches$summary$marginal, rep(c(F, T, F), c(2, 7, 1)))
})
