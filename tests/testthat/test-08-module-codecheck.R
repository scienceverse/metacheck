test_that("code_check", {
  module <- "code_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), NULL)

  # relevant text - info
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$summary_table$library_on_top, 1)
  expect_equal(mod_output$summary_table$hardcoded_folders, 1)
  expect_equal(mod_output$summary_table$loaded_files_missing, 1)
  expect_equal(mod_output$summary_table$minimum_comments, 0)

})
