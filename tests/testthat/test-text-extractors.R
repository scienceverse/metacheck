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

test_that("extract_p_values reads numbers without a decimal point (#642)", {
  # scientific notation with a whole first number, a capital E, 10 to a
  # power with nothing before it, and GROBID's A-accent signs for the
  # multiplication and minus signs
  txt <- c("p < 1e-10", "p < 1 \u00d7 10 -6", "P < 1x10 -29", "p<1e -4",
           "p = 2.4E-08", "p < 1E-5", "p < 10 -15", "p-value<10 -13",
           "p<1\u00c210 \u00c04", "p = 4.1\u00c210 \u00c05", "p = 1 \u00c3 10 -6")
  pv <- extract_p_values(txt)
  expect_equal(pv$text_id, seq_along(txt))
  expect_equal(pv$p_value, c(1e-10, 1e-6, 1e-29, 1e-4, 2.4e-8, 1e-5, 1e-15,
                             1e-13, 1e-4, 4.1e-5, 1e-6))

  # a whole 0 or 1 after "=", in a sentence that reports a test
  txt <- c("No effect of condition (t(20) = 0.1, p = 1).",
           "No difference between groups (Fisher's exact test, P = 1).",
           "The groups did not differ, p = 1.",
           "Both differed, chi2(1) = 40, p = 0; see Table 2.",
           "All were significant (permutation test, p \u2248 0).",
           "Interactions with time were also not significant for sCort (p = 1).")
  pv <- extract_p_values(txt)
  expect_equal(pv$text_id, seq_along(txt))
  expect_equal(pv$p_value, c(1, 1, 1, 0, 0, 1))

  # not p-values: a probability or parameter in a sentence without a test,
  # a bound, a fraction, a unit, the digits of "p < 0.05" lost after "0.",
  # a decimal comma, a value above 1, an exponent that cannot be read
  txt <- c("The option was chosen with p = 1, as before.",
           "Constraining the parameter to zero (p = 0) gave the model.",
           "for all 0 < p < 1 the map converges, t = 2",
           "with p = 1/6 in a single die roll, tested",
           "pressure P = 1 bar in the test",
           "Bolded values are significant at the p < 0.",
           "the groups differed, p = 0,05",
           "the groups differed, p = 12.05",
           "the groups differed, p = 10",
           "the groups differed (p < 1 \u00d7 10\u207b\u00b9\u2070)",
           # a statistic of 0 is not the test of a p of 0
           "the best model had ARIMA values p = 0, d = 0, q = 2.")
  expect_equal(nrow(extract_p_values(txt)), 0)
  # but it is the test of a p of 1
  expect_equal(extract_p_values("No difference, z = 0, p = 1.")$p_value, 1)

  # a paper object
  paper <- test_paper(c("We found no difference, t(10) = 0.2, p = 1.",
                        "The model used p = 1."))
  expect_equal(extract_p_values(paper)$text, "p = 1")
})

test_that("extract_p_values reads p with a correction or p-value name (#642)", {
  txt <- c("pFWE = .006", "p adj = .034", "p Bonferroni < .05", "P MCMC = 0.27",
           "p interaction = 0.003", "p_FDR < .001", "p-corrected = .01",
           "Ptrend < .001", "adjusted p = .02",
           # a test name (#21)
           "pMantel < .01", "p ANOVA < 10 -10", "p Wilcoxon = .021", "P MWU = 2.2 x 10 -9",
           "PBinomial = 0.97", "Pchi2 = 0.56")
  pv <- extract_p_values(txt)
  expect_equal(pv$text_id, seq_along(txt))
  expect_equal(pv$p_value, c(.006, .034, .05, .27, .003, .001, .01, .001, .02,
                             .01, 1e-10, .021, 2.2e-9, .97, .56))
  # the name after the p, NA for a plain p
  expect_equal(pv$p_name, c("FWE", "adj", "Bonferroni", "MCMC", "interaction", "FDR",
                            "corrected", "trend", NA, "Mantel", "ANOVA", "Wilcoxon",
                            "MWU", "Binomial", "chi2"))
  # another word after p is not a correction name
  expect_equal(nrow(extract_p_values(c("PSA (ng/ml) = 0.5", "pH = 0.5", "pre = .5"))), 0)
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

test_that("extract_eq reads p-values as extract_p_values does (#642)", {
  x <- c("t(20) = 3.1, p < 10 -15.", "t(20) = 3.1, p<2.2\u00c210 \u00c016.",
         "t(20) = 3.1, p adj = .034.", "t(20) = 3.1, N = 10 -15 years.",
         "t(20) = 3.1, M = 10-15.")
  eq <- extract_eq(x)
  eq <- eq[eq$lhs != "t", ]
  expect_equal(eq$lhs, c("p", "p", "p adj", "N", "M"))
  # 10 to a power only for a p-value; a range stays as it was
  expect_equal(eq$rhs, c("10 -15", "2.2\u00c210 \u00c016", ".034", "10", "10-15"))
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
