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

test_that("validate", {
  expect_true(is.function(metacheck::validate))
  expect_no_error(helplist <- help(validate, metacheck))

  expect_error(validate())

  # set up example
  text <- c("p < .05", "p = 0.034", "p > .10")
  imprecise <- c(T, F, T)
  p_comp <- c("<", "=", ">")

  # no ground truth
  gt <- text
  module <- "stat_p_exact"
  v <- validate(gt, module)
  expect_equal(v$imprecise, imprecise)
  expect_equal(v$p_comp, p_comp)

  # has ground truth
  gt <- data.frame(
    paper_id = as.character(1:3),
    text = text,
    p_comp = p_comp
  )
  module <- "stat_p_exact"
  v <- validate(gt, module)
  expect_equal(v$imprecise, imprecise)
  exp <- c("p_comp.gt", "p_comp.mod", "p_comp.valid")
  expect_in(exp, names(v))
  expect_equal(v$p_comp.valid, c(T, T, T))
})
