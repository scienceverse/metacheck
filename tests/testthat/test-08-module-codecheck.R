test_that("code_check offline", {
  module <- "code_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  skip_if_offline("api.osf.io")
  skip_on_ci()

  # no relevant text
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)
  exp <- data.frame(id = paper$id)
  expect_equal(mod_output$summary_table, exp)
  exp <- "No links to the Open Science Framework or Github were found."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})

test_that("code_check online", {
  skip("Long Tests")

  # OSF but no R files text
  paper <- psychsci[[40]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  #expect_equal(names(mod_output$table), c("name", "download_url", "osf_id"))
  exp <- data.frame(id = paper$id)
  expect_equal(mod_output$summary_table, exp)
  exp <- "No R files were found"
  expect_true(grepl(exp, mod_output$summary_text))
  expect_true(grepl(exp, mod_output$report[[1]]))
  # expect_true(grepl("https://osf.io/download/3s8gd/",
  #                   mod_output$report[[2]], fixed = TRUE))
})

test_that("code_check online", {
  skip("Long Tests")

  # relevant text - info
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 3)
  expect_equal(mod_output$table$name, c("bad.R", "bad.Rmd", "analysis.R"))

  st <- mod_output$summary_table
  expect_equal(st$hardcoded_folders, 3)
  expect_equal(st$loaded_files_missing, 3)
  expect_equal(st$minimum_comments, 0)

  #TODO: fix errors
  # paper <- psychsci[[233]]
  # mod_output <- module_run(paper, module)
  # st <- mod_output$summary_table
  # expect_equal(st$hardcoded_folders, 4)
  # expect_equal(st$loaded_files_missing, 7)
  # expect_equal(st$minimum_comments, 0.11363, tolerance = 0.1)
})
