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
  paper <- test_paper(c(
    "A was bigger than B, t(124) = 1.23, p 0.013, d = 0.34.",
    "We also ran an ANOVA, F(1, 13) = 2.34, p = .23, ηp2 = 0.01."
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

  # ξ should be detected as ES present but not coherence-checked
  paper <- test_paper("The model was significant, F(2, 151) = 1.00, p = .37, ξ = 2.30.")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$table$eta_coherence[[1]], "indeterminate")
  expect_false(is.na(mod_output$table$es[[1]]))
})

