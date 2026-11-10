test_that("extract_urls", {
  expect_true(is.function(metacheck::extract_urls))
  expect_no_error(helplist <- help(extract_urls, metacheck))

  expect_error(extract_urls(bad_arg))

  # valid urls
  valid_urls <- c(
    "https://osf.io/48ncu",
    "http://researchbox.org/4377",
    "osf.io/48ncu",
    "http://researchbox.org/4377?PASWORD=10&hi"
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
  expect_equal(nrow(urls), 0)

  # paperlist
  skip_no_psychsci()
  paper <- psychsci[1:10]
  urls <- extract_urls(paper)
  expect_in(urls$paper_id, names(paper))
})

test_that("extract_urls skips e-mail addresses and 'et al.' glued to a word (U158, U206)", {
  # both halves of an address were listed, and "et al.Inhibitory" gave the
  # URL "al.Inhibitory"
  paper <- test_paper(c("Catarino et al.Inhibitory Control was studied.",
                        "Contact k.aristovich@ucl.ac.uk for data.",
                        "Mail john@site.com, or visit site.com or twitter.com/@user."))
  expect_equal(extract_urls(paper)$text, c("site.com", "twitter.com/@user"))
})


test_that("extract_p_values", {
  expect_true(is.function(metacheck::extract_p_values))
  expect_no_error(helplist <- help(extract_p_values, metacheck))

  expect_error(extract_p_values(bad_arg))

  paper <- test_paper(c(
    "t = 2.23, p = 0.005.",
    "(p = 0.152)",
    "peta = 2.3; p > .05, ppp = 2",
    "2 = p",
    "MAP = .5, P = .04"
  ))
  p <- extract_p_values(paper)
  expect_equal(nrow(p), 4)
  expect_equal(p$text, c("p = 0.005", "p = 0.152", "p > .05", "P = .04"))
  expect_equal(p$p_value, c(0.005, 0.152, 0.050, 0.04))
  expect_equal(p$p_comp, c("=", "=", ">", "="))

  # iteration: text modules need no special adaptation
  skip_no_psychsci()
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


test_that("extract_p_values reads ps, p's, x 10 and the Unicode minus (U204, U205)", {
  # "ps < .05" gave no row, and "p = 1.8 x 10 -6" was read as 1.8
  txt <- c("All ps < .05.", "Both p's > .10.", "Both p\u2019s > .10.",
           "The p-values < .001.", "p = 1.8 \u00d7 10 -6", "p = 6.1 \u00d7 10\u22125",
           "p=2.14e\u2212213", "p = 5.0 x 10^-2")
  pv <- extract_p_values(txt)
  expect_equal(pv$p_value, c(.05, .10, .10, .001, 1.8e-6, 6.1e-5, 2.14e-213, .05))
})

test_that("partial eta squared written with its subscript after it is not a p-value (#336)", {
  txt <- c("F(2, 63) = 590, p < .001, ƞ²p = .95",
           "an effect (η² p = .06) as",
           "t = 2, η² P = 0.029, p = .04",
           "η 2 p = .10 and p < .001",
           "η² = .05, p = .03")
  pv <- extract_p_values(txt)
  expect_equal(pv$text_id, c(1L, 3L, 4L, 5L))
  expect_equal(pv$p_value, c(.001, .04, .001, .03))

  eq <- extract_eq(txt)
  expect_equal(eq$lhs, c("F", "p", "ƞ²p", "η² p", "t", "η² P", "p",
                         "η 2 p", "p", "η²", "p"))
})

test_that("extract_eq reads the Unicode minus, and 'and z' is z (U205, U218)", {
  eq <- extract_eq("t(28) = \u22122.15, r(98) = \u2212.32, r = \u2212 0.12, and z = 3.1.")
  expect_equal(eq$lhs, c("t", "r", "r", "z"))
  expect_equal(eq$rhs, c("-2.15", "-.32", "-0.12", "3.1"))
})

test_that("extract_eq", {
  expect_true(is.function(metacheck::extract_eq))
  expect_no_error(helplist <- help(extract_eq, metacheck))

  expect_error(extract_eq(bad_arg))

  paper <- test_paper(c(
    "t(10) = 2.23, p = 0.005.",
    "(F(1, 23) = 9.23, p = .023)",
    "peta = 2.3; p > .05, 95% CI = [2, 4]",
    "p-value >= 0.2",
    "2 = p"
  ))
  eq <- extract_eq(paper)

  exp <- dplyr::tribble(
    ~text_id, ~grp_id, ~lhs,         ~df,   ~comp, ~rhs,
    1,        1,       "t",      "(10)",     "=",   "2.23",
    1,        1,       "p",          NA,     "=",   "0.005",
    2,        2,       "F",   "(1, 23)",     "=",   "9.23",
    2,        2,       "p",          NA,     "=",   ".023",
    3,        3,       "peta",       NA,     "=",   "2.3",
    3,        3,       "p",          NA,     ">",   ".05",
    3,        3,       "95% CI",     NA,     "=",   "[2, 4]",
    4,        4,       "p-value",    NA,     ">=",  "0.2"
  )
  exp$paper_id <- paper$paper_id

  expect_equal(eq, exp)
})

test_that("extract_eq complex", {
  paper <- test_paper("A Mantel-Cox log rank test indicated that the difference between conditions was significant, Mantel-Cox χ2 (1, N = 60) = 6.99, p = .008, with children waiting less in the unaware condition (M = 307.10 s, nonparametric bootstrapped 95% CI = [207.69, 406.51]) than in the unaware/additional rewards condition (M = 529.73 s, nonparametric bootstrapped 95% CI = [408.56, 650.91]).")
  eq <- extract_eq(paper)

  expect_equal(eq$lhs[[1]], "χ2")
  expect_equal(eq$df[[1]], "(1, N = 60)")

  # no stats
  paper <- test_paper("No stats.")
  eq <- extract_eq(paper)
  expect_equal(nrow(eq), 0)
  expect_setequal(names(eq), c("text_id", "grp_id", "lhs", "df", "comp", "rhs", "paper_id"))

  # no df
  paper <- test_paper("Stats are t = 2.4.")
  eq <- extract_eq(paper)
  expect_equal(eq$df, NA_character_)

  # possessive stats
  paper <- test_paper(c(
    "What if Cohen's d > 1.0?",
    "(..., Hedges's g = 0.32, ...)"
  ))
  eq <- extract_eq(paper)
  expect_equal(eq$lhs, c("Cohen's d", "Hedges's g"))

  # GROBID collapses a subscript to a plain space ("g_av" -> "g av"); when a
  # Hedges'/Cohen's prefix precedes it, the extra word is absorbed into lhs
  # only then, so an unrelated adjacent clause ("beta = ...") is not fused
  # into the preceding statistic's name (#448)
  paper <- test_paper(c(
    "t(130) = 18.04, p < 0.001, Hedges' g av = 1.76.",
    "t(23) = 2.73, p = .01, Cohen's d z = 0.56.",
    "This was significant (beta = 0.74, t(260) = 11.32)."
  ))
  eq <- extract_eq(paper)
  expect_equal(eq$lhs, c(
    "t", "p", "Hedges' g av",
    "t", "p", "Cohen's d z",
    "beta", "t"
  ))

  # the same collapsed subscript with no "Hedges'"/"Cohen's" prefix word at
  # all ("g av"/"g_av" on its own) is recognized too, scoped to exactly the
  # known letter+subscript-word pairs so it does not fuse unrelated words
  # ("sample size n = 40", "mean age years = 25") into a false statistic name
  paper <- test_paper(c(
    "t(34) = 3.4, p = .003, g av = 0.43.",
    "t(34) = 3.4, p = .003, g_av = 0.43.",
    "the sample size n = 40.",
    "mean age years = 25."
  ))
  eq <- extract_eq(paper)
  expect_equal(eq$lhs, c(
    "t", "p", "g av",
    "t", "p", "g_av",
    "n",
    "years"
  ))

  # all eq
  skip_if_quick()
  skip_no_psychsci()
  #eq <- paper_table(psychsci, "eq")
  eq <- extract_eq(psychsci)
  lhs <- dplyr::count(eq, lhs, sort = TRUE)
  num_lhs <- dplyr::filter(eq, grepl("^[0-9]$", lhs)) |>
    expand_text(psychsci) |>
    dplyr::rename(text = expanded) |>
    text_search(".{10}\\d = [0-9\\.-].{10}", return = "match")
  expect_equal(nrow(num_lhs), 0)
})


test_that("bibr vs extract_eq", {
  skip("Failures expected")

  eq_bibr <- paper_table(psychsci, "eq")
  #eq_bibr$rhs <- as.numeric(eq_bibr$rhs)
  eq_bibr$grp_id <- NULL
  eq_bibr$bibr <- TRUE
  eq_bibr$eq_type <- NULL

  eq_mc <- extract_eq(psychsci)
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

test_that("extract_p_values and extract_eq search a character vector (U150)", {
  # both stopped on a plain character vector, while extract_urls() took one
  txt <- c("We found t(28) = 2.4, p = .03.", "No effect, p > .05.")
  p <- extract_p_values(txt)
  expect_equal(p$p_value, c(0.03, 0.05))
  expect_equal(p$text_id, 1:2)
  eq <- extract_eq(txt)
  expect_equal(eq$lhs, c("t", "p", "p"))
  expect_equal(eq$grp_id, c(1, 1, 2))
  # a text table without paper_id or text_id stopped extract_eq's grouping
  expect_equal(nrow(extract_eq(data.frame(text = txt))), 3)
})

test_that("extract_tests gives each paper of a list its own tests, once (U10)", {
  # every test was repeated for every paper id
  p1 <- test_paper(c("We found t(20) = 2.1, p = .04.", "Also F(1, 30) = 4.5, p = .04.")); p1$paper_id <- "a"
  p2 <- test_paper("Here t(10) = 3.2, p = .01."); p2$paper_id <- "b"
  tests <- extract_tests(paperlist(p1, p2))
  expect_equal(as.vector(table(tests$paper_id)[c("a", "b")]), c(2L, 1L))
  eq <- extract_eq(paperlist(p1, p2))
  expect_equal(eq$grp_id[eq$paper_id == "a"], c(1, 1, 2, 2))
})
