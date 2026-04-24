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
})

