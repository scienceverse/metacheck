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

  paper <- paper()
  paper$full_text <- data.frame(
    id <- paper$id,
    text = valid_urls
  )
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

  paper <- paper()
  paper$full_text <- data.frame(
    id <- paper$id,
    text = invalid_urls
  )
  urls <- extract_urls(paper)
  expect_equal(urls$text, character(0))

  # paperlist
  paper <- psychsci[1:10]
  urls <- extract_urls(paper)
  expect_in(urls$id, names(paper))
})


test_that("extract_p_values", {
  expect_true(is.function(metacheck::extract_p_values))
  expect_no_error(helplist <- help(extract_p_values, metacheck))

  expect_error(extract_p_values(bad_arg))

  paper <- read(demoxml())
  p <- extract_p_values(paper)
  expect_equal(nrow(p), 3)
  expect_equal(p$text, c("p = 0.005", "p = 0.152", "p > .05"))
  expect_equal(p$p_value, c(0.005, 0.152, 0.050))
  expect_equal(p$p_comp, c("=", "=", ">"))

  # iteration: text modules need no special adaptation
  paper <- psychsci
  p <- extract_p_values(paper)
  expect_equal(nrow(p), 4832)

  # check problem with minus sign at end
  minus <- p$text[grep("-$", p$text)]
  e <- p$text[grep("e", p$text)]
  expect_equal(length(minus), 0)
  expect_equal(length(e), 9L)

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

