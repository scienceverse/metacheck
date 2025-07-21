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

  # expected table has columns not in the observed results
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
  exp_summary <- expected$summary
  exp_summary[1,2] <- 5 # change some expected values
  exp_summary[2,2] <- NA

  v <- validate(paper, module, summary = exp_summary)

  expect_equal(v$module, module)
  expect_equal(v$observed$summary, expected$summary)
  expect_equal(names(v$matches), "summary")
  expect_equal(v$matches$summary$marginal.expected, c(exp_summary$marginal))
  expect_equal(v$matches$summary$marginal.observed, expected$summary$marginal)
  expect_equal(v$matches$summary$marginal, rep(c(F, T), c(2, 8)))

  # handle type mismatch
  exp_summary$marginal <- as.integer(expected$summary$marginal)
  v <- validate(paper, module, summary = exp_summary)
  expect_equal(v$stats$summary$marginal, 1)

  # print.ppchk_validate
  op <- capture_output(print(v))
  op2 <- capture_output(print.ppchk_validate(v))
  exp_op <- " Validated matches for module `marginal`:\n\n* N in validation sample: 10\n* summary: \n  * marginal: 1"
  expect_equal(op2, op)
  expect_equal(op, exp_op)
})

test_that("accuracy", {
  exp <- rep(c(T, F), 50)
  obs <- exp
  obs[1:20] <- !obs[1:20]
  a <- accuracy(exp, obs)
  expect_equal(a$hits, 40)
  expect_equal(a$misses, 10)
  expect_equal(a$false_alarms, 10)
  expect_equal(a$correct_rejections, 40)
  expect_equal(a$accuracy, 0.8)
  expect_equal(a$sensitivity, 0.8)
  expect_equal(a$specificity, 0.2)
  expect_equal(round(a$d_prime, 2), 1.68)
  expect_equal(a$beta, 1)
})
