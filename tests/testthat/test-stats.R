test_that("stats", {
  expect_true(is.function(metacheck::stats))
  expect_no_error(helplist <- help(stats, metacheck))

  expect_error(stats(bad_arg))
})

test_that("defaults", {
  # search a paper object
  paper <- test_paper("Test (M=4.5, t(97.2) = -1.96, p = 0.152).")
  stat_table <- stats(paper)
  expect_true(is.data.frame(stat_table))
  expect_equal(nrow(stat_table), 1)
  expect_equal(stat_table$test_type, "t")
  expect_equal(stat_table$df2, 97.2)
  expect_equal(stat_table$test_comp, "=")
  expect_equal(stat_table$test_value, -1.96)
  expect_equal(stat_table$p_comp, "=")
  expect_equal(stat_table$reported_p, 0.152)
  expect_equal(stat_table$error, TRUE)

  # no matches
  paper <- test_paper("No stats here")
  stat_table <- stats(paper)
  expect_equal(stat_table, data.frame())
})

test_that("statcheck options", {
  test_text <- data.frame(
    text = c("t(20) = 4.23, p = .002",
             "t(20) = 4.23, p = 0.0004",
             "(z = 1.4, p < .05)",
             "z = 1.4, p < .05", # doesn't parse as Z; wierd!
             "H = 2.2, p = .000")
  )

  z_table <- stats(test_text, stat = "Z")
  expect_equal(nrow(z_table), 1)

  t_table <- stats(test_text, stat = "t")
  expect_equal(nrow(t_table), 2)
  expect_equal(t_table$error, c(T, F))

  all_table <- stats(test_text, AllPValues = TRUE)
  expect_equal(nrow(all_table), nrow(test_text))
})

test_that("error", {
  # errored out at 154
  # Error in missing value where TRUE/FALSE needed
  paper <- psychsci[100]
  expect_no_error( stats <- stats(paper) )
})

