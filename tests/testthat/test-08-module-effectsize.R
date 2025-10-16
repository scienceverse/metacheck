test_that("effect_size", {
  module <- "effect_size"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)

  # relevant text - red
  paper <- psychsci[[21]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 7)
  #expect_equal(mod_output$summary$ttests_n, 4)
  expect_equal(mod_output$summary$ttests_with_es, 0)
  expect_equal(mod_output$summary$ttests_without_es, 4)
  #expect_equal(mod_output$summary$Ftests_n, 3)
  expect_equal(mod_output$summary$Ftests_with_es, 0)
  expect_equal(mod_output$summary$Ftests_without_es, 3)

  # relevant text - yellow
  paper <- psychsci[[9]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 14)
  #expect_equal(mod_output$summary$ttests_n, 10)
  expect_equal(mod_output$summary$ttests_with_es, 6)
  expect_equal(mod_output$summary$ttests_without_es, 4)
  #expect_equal(mod_output$summary$Ftests_n, 4)
  expect_equal(mod_output$summary$Ftests_with_es, 2)
  expect_equal(mod_output$summary$Ftests_without_es, 2)

  # relevant text - green
  paper <- psychsci[[5]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(nrow(mod_output$table), 2)
  #expect_equal(mod_output$summary$ttests_n, 2)
  expect_equal(mod_output$summary$ttests_with_es, 2)
  expect_equal(mod_output$summary$ttests_without_es, 0)
  #expect_equal(mod_output$summary$Ftests_n, 0)
  expect_equal(mod_output$summary$Ftests_with_es, 0)
  expect_equal(mod_output$summary$Ftests_without_es, 0)

  # iterate
  paper <- psychsci
  mod_output <- module_run(paper, module)
  t <- mod_output$table
  s <- mod_output$summary
  expect_true(all(s$ttests_with_es >= 0))
})

