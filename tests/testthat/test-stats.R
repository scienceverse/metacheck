grobid_dir <- system.file("grobid", package="papercheck")
filename <- file.path(grobid_dir, "incest.xml")

test_that("exists", {
  expect_true(is.function(stats))
})

test_that("defaults", {
  # search list of paper objects
  s <- read(filename)
  stat_table <- stats(s)
  expect_true(is.data.frame(stat_table))
  expect_equal(nrow(stat_table), 5)
  expect_equal(ncol(stat_table), 20)

  # search text table by sentence
  text <- search_text(s, section = "results")
  res_table <- stats(text)
  expect_true(is.data.frame(res_table))
  expect_equal(res_table$s, c(1,1,2,2,3))

  # search text table by paragraph
  text <- search_text(s, section = "results", return = "paragraph")
  p_res_table <- stats(text)
  expect_true(is.data.frame(p_res_table))
  expect_equal(p_res_table$s, rep(NA, 5))
  expect_equal(res_table$computed_p, p_res_table$computed_p)

  # no matches
  text <- search_text(s, section = "discussion")
  disc_table <- stats(text)
  expect_equal(disc_table, data.frame())
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
