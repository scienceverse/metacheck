
test_that("stat_check counts one-sided tests as consistent when the paper says its tests are one-sided (#695, point 5)", {
  t <- data.frame(test_type = c("t", "t", "t", "F"), df1 = c(NA, NA, NA, 1), df2 = c(507.82, 40, 40, 40),
                  test_value = c(-1.93, 1.93, 1.93, 3.72), p_comp = "=", reported_p = c(.973, .03, .30, .03),
                  raw = c("t(507.82) = -1.93, p = .973", "t(40) = 1.93, p = .03", "t(40) = 1.93, p = .30",
                          "F(1, 40) = 3.72, p = .03"))
  # the other direction, half the two-sided p, not a wrong p, and F(1, df) as t squared
  expect_equal(.stat_one_sided_fits(t), c(TRUE, TRUE, FALSE, TRUE))
  one <- test_paper(c("All hypothesis tests were one-sided.",
                      "The effect went the other way, t(507.82) = -1.93, p = .973."))
  two <- test_paper("The effect went the other way, t(507.82) = -1.93, p = .973.")
  expect_true(.paper_says_one_sided(one))
  expect_equal(module_run(one, "stat_check")$traffic_light, "green")
  expect_false(module_run(two, "stat_check")$traffic_light == "green")
  # an equivalence test is left out
  tost <- test_paper("The equivalence test was significant, t(507.82) = 1.93, p = .973.")
  expect_equal(nrow(module_run(tost, "stat_check")$table), 0)
})

test_that("stat_check reads a statistic with < as a bound, and a variance test's two-sided p (#695, point 6)", {
  t <- data.frame(test_type = c("F", "F", "F", "t"), df1 = c(1, 20, 20, NA), df2 = c(104, 20, 20, 30),
                  test_comp = c("<", "=", "=", ">"), test_value = c(0.01, 1.4, 1.4, 3),
                  p_comp = c("=", "=", "=", "<"), reported_p = c(.939, .458, .458, .01),
                  raw = c("F(1, 104) < 0.01, p = .939", "F(20, 20) = 1.40, p = .458",
                          "F(20, 20) = 1.40, p = .458", "t(30) > 3, p < .01"),
                  text = c("x", "The variances were equal, F(20, 20) = 1.40, p = .458.",
                           "An effect, F(20, 20) = 1.40, p = .458.", "y"))
  # a bound fits; twice the p fits only in a sentence about equal variances
  expect_equal(.stat_bound_or_variance_fits(t), c(TRUE, TRUE, NA, TRUE))
  expect_equal(module_run(test_paper("There was no effect, F(1, 104) < 0.01, p = .939."), "stat_check")$traffic_light,
               "green")
})
