test_that("ttest", {
  module <- "effect_size_ttest"

  # no relevant text
  paper <- psychsci[[1]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)

  # relevant text - red
  paper <- psychsci[[4]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(mod_output$summary$ttests_n, 1)
  expect_equal(mod_output$summary$ttests_with_es, 0)
  expect_equal(mod_output$summary$ttests_without_es, 1)

  # relevant text - yellow
  paper <- psychsci[[9]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(mod_output$summary$ttests_n, 10)
  expect_equal(mod_output$summary$ttests_with_es, 6)
  expect_equal(mod_output$summary$ttests_without_es, 4)

  # relevant text - green
  paper <- psychsci[[5]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(mod_output$summary$ttests_n, 2)
  expect_equal(mod_output$summary$ttests_with_es, 2)
  expect_equal(mod_output$summary$ttests_without_es, 0)

  # iterate
  paper <- psychsci
  mod_output <- module_run(paper, module)
  s <- mod_output$summary
  zero <- s$ttests_n == 0
  expect_true(all(is.na(s$ttests_with_es[zero])))
  expect_true(all(is.na(s$ttests_without_es[zero])))
  expect_true(all(
    s$ttests_n[!zero] ==
    s$ttests_with_es[!zero] + s$ttests_without_es[!zero]
  ))
})

test_that("ftest", {
  module <- "effect_size_ftest"

  # no relevant text
  paper <- psychsci[[3]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)

  # relevant text - red
  paper <- psychsci[[10]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(mod_output$summary$Ftests_n, 1)
  expect_equal(mod_output$summary$Ftests_with_es, 0)
  expect_equal(mod_output$summary$Ftests_without_es, 1)

  # relevant text - green
  paper <- psychsci[[2]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "green")
  expect_equal(mod_output$summary$Ftests_n, 21)
  expect_equal(mod_output$summary$Ftests_with_es, 21)
  expect_equal(mod_output$summary$Ftests_without_es, 0)

  # relevant text - yellow
  paper <- psychsci[[9]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(mod_output$summary$Ftests_n, 5)
  expect_equal(mod_output$summary$Ftests_with_es, 3)
  expect_equal(mod_output$summary$Ftests_without_es, 2)

  # iterate
  paper <- psychsci
  mod_output <- module_run(paper, module)
  s <- mod_output$summary
  zero <- s$Ftests_n == 0
  expect_true(all(is.na(s$Ftests_with_es[zero])))
  expect_true(all(is.na(s$Ftests_without_es[zero])))
  expect_true(all(
    s$Ftests_n[!zero] ==
      s$Ftests_with_es[!zero] + s$Ftests_without_es[!zero]
  ))
})

