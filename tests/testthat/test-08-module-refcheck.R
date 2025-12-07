test_that("reference_check", {
  module <- "reference_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)

  # relevant text - info
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(mod_output$summary_table$retraction_watch, 1)
  expect_equal(mod_output$summary_table$replication_exists, 1)
  expect_equal(mod_output$summary_table$doi_missing, 1)
  expect_equal(mod_output$summary_table$pubpeer_comments, 3)

})
