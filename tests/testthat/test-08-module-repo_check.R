test_that("repo_check offline", {
  module <- "repo_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)
  exp <- data.frame(id = paper$id,
                    repo_n = 0,
                    files_n = NA,
                    files_data = NA,
                    files_code = NA,
                    files_readme = NA,
                    files_zip = NA)
  expect_equal(mod_output$summary_table, exp)
  exp <- "We found no links to repositories the Open Science Framework, Github, or ResearchBox."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})

test_that("OSF no files", {
  # OSF but no R files
  skip_if_quick()
  skip_osf()

  module <- "repo_check"
  paper <- paper()
  paper$full_text <- data.frame(text = "https://osf.io/y6a34", id = paper$id)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  exp <- data.frame(id = paper$id,
                    repo_n = 1,
                    files_n = 0,
                    files_data = 0,
                    files_code = 0,
                    files_readme = 0,
                    files_zip = 0)
  expect_equal(mod_output$summary_table, exp)
  exp <- " 0 files "
  expect_true(grepl(exp, mod_output$summary_text))
})

test_that("no code files", {
  skip_osf()

  module <- "repo_check"
  paper <- paper()
  paper$full_text <- data.frame(
    text = c("https://osf.io/m4nbv"),
    id = paper$id
  )
  mod_output <- module_run(paper, module)

  expect_true(grepl("We found 2 files ", mod_output$summary_text))
  exp <- data.frame(id = paper$id,
                    repo_n = 1,
                    files_n = 2,
                    files_data = 0,
                    files_code = 0,
                    files_readme = 1,
                    files_zip = 1)
  expect_equal(mod_output$summary_table, exp)
})

test_that("OSF", {
  skip_osf()

  module <- "repo_check"
  paper <- paper()
  paper$full_text <- data.frame(
    text = c("https://osf.io/629bx"),
    id = paper$id
  )
  mod_output <- module_run(paper, module)

  expect_true(grepl("We found 4 files ", mod_output$summary_text))
  exp <- data.frame(id = paper$id,
                    repo_n = 1,
                    files_n = 4,
                    files_data = 1,
                    files_code = 2,
                    files_readme = 0,
                    files_zip = 1)
  expect_equal(mod_output$summary_table, exp)
})

test_that("OSF, github and rb", {
  skip_if_quick()
  skip_osf()

  # relevant text - info
  module <- "repo_check"
  paper <- paper()
  paper$full_text <- data.frame(
    text = c("osf.io/629bx",
             "github.com/scienceverse/demo",
             "https://researchbox.org/4377"),
    id = paper$id
  )
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  expect_gt(nrow(mod_output$table), 14)
  exp <- c("bad.R", "bad.Rmd", "Code/Study 1.r", "good-example.R")
  expect_contains(mod_output$table$file_name, exp)

  exp <- data.frame(id = paper$id,
                    repo_n = 3,
                    files_n = 15,
                    files_data = 6,
                    files_code = 4,
                    files_readme = 1,
                    files_zip = 2)
  expect_equal(mod_output$summary_table, exp)
})

