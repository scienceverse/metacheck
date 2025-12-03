#setwd("tests/testthat/")

test_that("exists", {
  expect_true(is.function(metacheck::compare_tables))
  expect_no_error(helplist <- help(compare_tables, metacheck))
})

test_that("errors", {
  exp <- data.frame(id = 1:2)
  obs <- data.frame(id = 1:2)
  expect_error(compare_tables(exp, obs), "match_cols")
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


test_that("compare_tables", {
  # all ident, 1/2 mis-classified
  expected <- data.frame(id = 1:2, text = c("A", "B"), value = c(10, 20))
  observed <- data.frame(id = 1:2, text = c("A", "B"), value = c(10, 25))
  v <- compare_tables(expected, observed)

  exp <- c(exp = 2, obs = 2, true_pos = 2, false_pos = 0, false_neg = 0)
  expect_equal(v$identification, exp)
  expect_equal(v$classification, c(value = 0.5))

  # 1 extra observation
  expected <- data.frame(id = 1:2, text = c("A", "B"), value = c(10, 20))
  observed <- data.frame(id = 1:3, text = c("A", "B", "C"), value = c(10, 20, 30))
  v <- compare_tables(expected, observed)

  expect_equal(v$table$true_pos, c(TRUE, TRUE, FALSE))
  expect_equal(v$table$false_pos, c(FALSE, FALSE, TRUE))
  expect_equal(v$table$false_neg, c(FALSE, FALSE, FALSE))

  # specify match_cols
  exp <- data.frame(id = 1:2, val = 3:4)
  obs <- data.frame(id = 1:2, val = 3:4)
  v <- compare_tables(exp, obs, match_cols = "id")
  expect_equal(v$identification[["true_pos"]], 2)
  expect_equal(v$classification[["val"]], 1)

  # specify match_cols
  expected <- data.frame(id = 1:2, text = 3:4)
  observed <- data.frame(id = 1:2, text = 3:4)
  v <- compare_tables(expected, observed)
  expect_equal(v$identification[["true_pos"]], 2)
  expect_equal(v$classification, list())

  exp <- c("id", "text", "exp", "obs",
           "true_pos", "false_pos", "false_neg")
  expect_equal(names(v$table), exp)
})
