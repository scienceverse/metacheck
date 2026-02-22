test_that("extract_urls", {
  expect_true(is.function(metacheck::extract_urls))
  expect_no_error(helplist <- help(extract_urls, metacheck))

  expect_error(extract_urls(bad_arg))

  # valid urls
  valid_urls <- c(
    "https://osf.io/48ncu",
    "http://researchbox.org/4377",
    "osf.io/48ncu"
  )

  paper <- test_paper(valid_urls)
  urls <- extract_urls(paper)
  expect_equal(urls$text, valid_urls)

  # invalid URLs
  invalid_urls <- c(
    "http is a way to get osf .io",
    "It is done. And now...",
    "."
    # TODO: needs work
    # "3.full text is available"
  )

  paper <- test_paper(invalid_urls)
  urls <- extract_urls(paper)
  expect_equal(urls$text, character(0))

  # paperlist
  paper <- psychsci[1:10]
  urls <- extract_urls(paper)
  expect_in(urls$paper_id, names(paper))
})


test_that("extract_p_values", {
  expect_true(is.function(metacheck::extract_p_values))
  expect_no_error(helplist <- help(extract_p_values, metacheck))

  expect_error(extract_p_values(bad_arg))

  paper <- test_paper(c(
    "t = 2.23, p = 0.005.",
    "(p = 0.152)",
    "peta = 2.3; p > .05, ppp = 2",
    "2 = p"
  ))
  p <- extract_p_values(paper)
  expect_equal(nrow(p), 3)
  expect_equal(p$text, c("p = 0.005", "p = 0.152", "p > .05"))
  expect_equal(p$p_value, c(0.005, 0.152, 0.050))
  expect_equal(p$p_comp, c("=", "=", ">"))

  # iteration: text modules need no special adaptation
  paper <- psychsci
  p <- extract_p_values(paper)


  # specific values
  expected <- c(
    "p=.05",
    "p\n=\n.05",
    "p = .05",
    "p < .05",
    "p > .05",
    "p <= .05",
    "p >= .05",
    "p == .05",
    "p << .05",
    "p >> .05",
    "p ≤ .05",
    "p ≥ .05",
    "p ≪ .05",
    "p ≫ .05",
    "p ≠ .05",
    "p-value = .05",
    "pvalue = .05",
    "p = 0.05",
    "p = 0.05",
    "p = 0.5e-1",
    "p = n.s.",
    "p = ns",
    "p = 5.0x10^-2",
    "p = 5.0 x 10^-2",
    "p = 5.0 x 10 ^ -2",
    "p = 5.0 * 10 ^ -2",
    "p = 5.0e-2",
    "p = 5.0 e-2",
    "p = 5.0 e -2"
  )
  not <- c(
    "up = 0.05",
    "p = stuff",
    "p = -0.05",
    "p less than 0.05",
    "p = 12.05"
  )

  paper <- data.frame(
    id = 1,
    text = c(expected, not),
    expected = rep(c(T, F), c(length(expected), length(not)))
  )
  p <- extract_p_values(paper)
  expect_true(!"" %in% p$p_comp)
  expect_equal(p$p_value[1:20], rep(0.05, 20))
  expect_equal(p$p_value[21:22], rep(NA_real_, 2))
  expect_equal(p$p_value[23:29], rep(0.05, 7))
})


test_that("extract_equations", {
  expect_true(is.function(metacheck::extract_equations))
  expect_no_error(helplist <- help(extract_equations, metacheck))

  expect_error(extract_equations(bad_arg))

  paper <- test_paper(c(
    "t(10) = 2.23, p = 0.005.",
    "(F(1, 23) = 9.23, p = .023)",
    "peta = 2.3; p > .05, 95% CI = [2, 4]",
    "p-value >= 0.2",
    "2 = p"
  ))
  eq <- extract_equations(paper)

  exp <- dplyr::tribble(
    ~text_id, ~grp_id, ~lhs,         ~comp, ~rhs,
    1,        1,       "t(10)",      "=",   "2.23",
    1,        1,       "p",          "=",   "0.005",
    2,        2,       "F(1, 23)",   "=",   "9.23",
    2,        2,       "p",          "=",   ".023",
    3,        3,       "peta",       "=",   "2.3",
    3,        3,       "p",          ">",   ".05",
    3,         3,       "95% CI",     "=",   "[2, 4]",
    4,        4,       "p-value",    ">=",  "0.2"
  )
  exp$eq_type <- "stat"
  exp$paper_id <- paper$paper_id

  expect_equal(eq, exp)
})


test_that("bibr vs extract_equations", {
  skip("Failures expected")

  eq_bibr <- concat_tables(psychsci, "equations")
  #eq_bibr$rhs <- as.numeric(eq_bibr$rhs)
  eq_bibr$grp_id <- NULL
  eq_bibr$bibr <- TRUE
  eq_bibr$eq_type <- NULL

  eq_mc <- extract_equations(psychsci)
  eq_mc$grp_id <- NULL
  eq_mc$eq_type <- NULL
  eq_mc$mc <- TRUE

  compare <-dplyr::full_join(
    eq_bibr, eq_mc,
    by = c("paper_id", "text_id", "lhs", "comp", "rhs"),
    suffix = c(".bibr", ".mc")
  ) |>
    dplyr::arrange(paper_id, text_id)


  dplyr::count(compare, bibr, mc)

  dplyr::filter(compare, is.na(bibr) | is.na(mc)) |> View()

})
