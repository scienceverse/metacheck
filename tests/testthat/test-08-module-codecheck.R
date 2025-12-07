test_that("code_check", {
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

  skip("Long Tests")

  # no R files text
  paper <- psychsci[[40]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(names(mod_output$table), c("name", "download_url", "osf_id"))
  exp <- data.frame(id = paper$id)
  expect_equal(mod_output$summary_table, exp)
  exp <- "No R files were found"
  expect_true(grepl(exp, mod_output$summary_text))
  expect_true(grepl(exp, mod_output$report[[1]]))
  expect_true(grepl("https://osf.io/download/3s8gd/",
                    mod_output$report[[2]], fixed = TRUE))

  # relevant text - info
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 3)
  expect_equal(mod_output$table$name, c("bad.R", "bad.Rmd", "analysis.R"))

  st <- mod_output$summary_table
  expect_equal(st$code_library_spread, 3)
  expect_equal(st$code_hardcoded_paths, 4)
  expect_equal(st$code_loaded_files_missing, 3)
  expect_equal(st$code_minimum_comments, 0)

  paper <- psychsci[[233]]
  mod_output <- module_run(paper, module)
  exp <- "Some files loaded in the R scripts were missing in the repository. Hardcoded file paths were found. Libraries were loaded in multiple places. All your code files had comments."
  expect_equal(mod_output$summary_text, exp)
  st <- mod_output$summary_table
  expect_equal(st$code_library_spread, 7)
  expect_equal(st$code_hardcoded_paths, 4)
  expect_equal(st$code_loaded_files_missing, 7)
  expect_equal(st$code_minimum_comments, 0.11363, tolerance = 0.1)
})
