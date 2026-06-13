test_that("stat_effect_size", {
  module <- "stat_effect_size"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- test_paper("There are no stats.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)

  # relevant text - red
  paper <- test_paper(c(
    "A was bigger than B, t(124) = 1.23, p 0.013.",
    "We also ran an ANOVA, F(1, 13) = 2.34, p = .23."
  ))
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$summary_table$ttests_with_es, 0)
  expect_equal(mod_output$summary_table$ttests_without_es, 1)
  expect_equal(mod_output$summary_table$Ftests_with_es, 0)
  expect_equal(mod_output$summary_table$Ftests_without_es, 1)

  # relevant text - yellow
  paper <- test_paper(c(
    "A was bigger than B, t(124) = 1.23, p 0.013, d = 0.34.",
    "We also ran an ANOVA, F(1, 13) = 2.34, p = .23."
  ))
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$summary_table$ttests_with_es, 1)
  expect_equal(mod_output$summary_table$ttests_without_es, 0)
  expect_equal(mod_output$summary_table$Ftests_with_es, 0)
  expect_equal(mod_output$summary_table$Ftests_without_es, 1)

  # relevant text - green
  # ES values are coherent with the test statistics: d = 0.34 matches
  # t(124) = 1.23 under the independent unequal-n range, and ηp² = 0.15
  # matches F(1, 13) = 2.34 (implied partial eta-squared = 0.1525)
  paper <- test_paper(c(
    "A was bigger than B, t(124) = 1.23, p 0.013, d = 0.34.",
    "We also ran an ANOVA, F(1, 13) = 2.34, p = .23, ηp² = 0.15."
  ))
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(mod_output$summary_table$ttests_with_es, 1)
  expect_equal(mod_output$summary_table$ttests_without_es, 0)
  expect_equal(mod_output$summary_table$Ftests_with_es, 1)
  expect_equal(mod_output$summary_table$Ftests_without_es, 0)

  # iterate
  paper <- paperlist(
    test_paper("A was bigger than B, t(124) = 1.23, p 0.013, d = 0.34."),
    test_paper("We also ran an ANOVA, F(1, 13) = 2.34, p = .23.")
  )
  mod_output <- module_run(paper, module)
  t <- mod_output$table
  s <- mod_output$summary_table
  expect_equal(t$test_text[[1]], "t(124) = 1.23")
  expect_equal(t$test_text[[2]], "F(1, 13) = 2.34")
  expect_equal(t$test[[1]], "t-test")
  expect_equal(t$test[[2]], "F-test")
  expect_equal(s$ttests_with_es, c(1, 0))
  expect_equal(s$ttests_without_es, c(0, 0))
  expect_equal(s$Ftests_without_es, c(0, 1))
  expect_equal(s$Ftests_with_es, c(0, 0))

  # coherence checks for d in t-tests
  paper <- test_paper("A was bigger than B, t(124) = 1.23, p 0.013, d = 0.34.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$d_coherence[[1]], "match_under_assumptions")
  expect_true(mod_output$table$d_coherence_assumption[[1]] %in% c(
    "paired_dz",
    "independent_equal_n",
    "independent_unequal_n_range"
  ))

  paper <- test_paper("A was bigger than B, t(20) = 1.00, p 0.32, d = 3.00.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$d_coherence[[1]], "no_match")
  expect_equal(mod_output$table$d_coherence_assumption[[1]], "none")

  # multiple t-tests and d values in one sentence should split into paired rows
  paper <- test_paper("A was bigger than B, t(23) = 2.73; t(23) = 2.98; t(23) = 4.74, d = 0.56; d = 0.61; d = 0.97.")
  mod_output <- module_run(paper, module)
  expect_equal(nrow(mod_output$table), 3)
  expect_equal(mod_output$table$test_text, c("t(23) = 2.73", "t(23) = 2.98", "t(23) = 4.74"))
  expect_equal(mod_output$table$es, c("d = 0.56", "d = 0.61", "d = 0.97"))
  expect_equal(mod_output$table$d_coherence, rep("match_under_assumptions", 3))
  expect_equal(mod_output$table$d_coherence_assumption, rep("paired_dz", 3))

  # F-tests with multiple values and eta-squared values in one sentence should split into paired rows
  paper <- test_paper("The model was significant, F(5, 120) = 91.32; F(5, 120) = 74.45; F(5, 120) = 388.16, η² = .79; η² = .67; η² = .94.")
  mod_output <- module_run(paper, module)
  expect_equal(nrow(mod_output$table), 3)
  expect_equal(mod_output$table$test_text, c("F(5, 120) = 91.32", "F(5, 120) = 74.45", "F(5, 120) = 388.16"))
  expect_equal(mod_output$table$es, c("η² = .79", "η² = .67", "η² = .94"))
  expect_equal(mod_output$table$eta_coherence, rep("indeterminate", 3))
  expect_equal(mod_output$table$eta_coherence_assumption, rep("eta_squared", 3))

  paper <- test_paper("The model was significant, F(1, 13) = 2.34, p = .23, ηp² = 0.15.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$eta_coherence[[1]], "match_under_assumptions")
  expect_equal(mod_output$table$eta_coherence_assumption[[1]], "partial_eta_squared")

  # Cohen's f: not checked regardless of whether other ES are present
  paper <- test_paper("The model was significant, F(1, 48) = 5.23, p = .026, f = 0.37.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$eta_coherence[[1]], "indeterminate")
  expect_equal(mod_output$table$eta_coherence_note[[1]],
               "Cohen's f not checked: cannot determine whether based on eta squared or partial eta squared.")

  # Cohen's f alongside partial eta: partial eta is still checked, f is ignored
  paper <- test_paper("The model was significant, F(1, 13) = 2.34, p = .23, ηp² = 0.15, f = 0.42.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$eta_coherence[[1]], "match_under_assumptions")
  expect_equal(mod_output$table$eta_coherence_assumption[[1]], "partial_eta_squared")

  # ωp² cannot be verified from F and dfs alone — should be indeterminate, not no_match
  paper <- test_paper("The model was significant, F(2, 151) = 1.00, p = .37, ωp² = 0.00.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$eta_coherence[[1]], "indeterminate")
})

test_that("stat_effect_size emits no warnings for a paper with no stats (#308)", {
  paper <- test_paper("There are no stats.")
  expect_no_warning(mod_output <- module_run(paper, "stat_effect_size"))
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)
})

test_that("stat_effect_size reports the sample sizes behind a d coherence match", {
  module <- "stat_effect_size"

  # equal-n: t(48) = 2.00 implies d = 0.566 for n1 = n2 = 25 (N = 50)
  mod_output <- module_run(test_paper("t(48) = 2.00, d = 0.57."), module)
  expect_equal(mod_output$table$d_coherence_assumption[[1]], "independent_equal_n")
  expect_equal(mod_output$table$d_implied_n[[1]], "n1 = n2 = 25, N = 50")
  expect_match(mod_output$table$d_coherence_note[[1]], "n1 = n2 = 25, N = 50", fixed = TRUE)

  # unequal-n: closest split is reported as n1, n2 and N
  mod_output <- module_run(test_paper("t(38) = 2.10, d = 0.68."), module)
  expect_equal(mod_output$table$d_coherence_assumption[[1]], "independent_unequal_n_range")
  expect_equal(mod_output$table$d_implied_n[[1]], "n1 = 16, n2 = 24, N = 40")
  expect_match(mod_output$table$d_coherence_note[[1]], "n1 = 16, n2 = 24, N = 40", fixed = TRUE)

  # paired dz: n recorded (n = df + 1) but not added to the note
  mod_output <- module_run(test_paper("t(23) = 2.73, d = 0.56."), module)
  expect_equal(mod_output$table$d_coherence_assumption[[1]], "paired_dz")
  expect_equal(mod_output$table$d_implied_n[[1]], "n = 24")
  expect_equal(mod_output$table$d_coherence_note[[1]],
               "Match under paired-samples dz assumption.")
})

