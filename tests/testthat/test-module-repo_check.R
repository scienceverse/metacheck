test_that("repo_check offline", {
  module <- "repo_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- test_paper("No repos")
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 0,
                    files_n = NA,
                    files_data = NA,
                    files_code = NA,
                    files_readme = NA,
                    files_zip = NA)
  expect_equal(mod_output$summary_table, exp)
  exp <- "We found no links to repositories on the Open Science Framework, Github, ResearchBox, or Zenodo."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})

#httptest2::start_capturing()
httptest2::use_mock_api()

test_that("OSF no files", {
  # OSF but no R files

  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/y6a34", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  exp <- data.frame(paper_id = paper$paper_id,
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
  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/m4nbv", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_true(grepl("We found 2 files ", mod_output$summary_text))
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 1,
                    files_n = 2,
                    files_data = 0,
                    files_code = 0,
                    files_readme = 1,
                    files_zip = 1)
  expect_equal(mod_output$summary_table, exp)
})

test_that("OSF", {
  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/629bx", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_true(grepl("We found 4 files ", mod_output$summary_text))
  exp <- data.frame(paper_id = paper$paper_id,
                    repo_n = 1,
                    files_n = 4,
                    files_data = 1,
                    files_code = 2,
                    files_readme = 0,
                    files_zip = 1)
  expect_equal(mod_output$summary_table, exp)
})

test_that("OSF, github and rb", {
  # relevant text - info
  module <- "repo_check"
  text <- c("osf.io/629bx",
            "github.com/scienceverse/demo",
            "https://researchbox.org/4377")
  paper <- test_paper()
  paper$url <- data.frame(href = text, text_id = 1:3)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  expect_gt(nrow(mod_output$table), 14)
  exp <- c("bad.R", "bad.Rmd", "Code/Study 1.r", "good-example.R")
  expect_contains(mod_output$table$file_name, exp)
  expect_equal(mod_output$summary_table$paper_id, paper$paper_id)
  expect_equal(mod_output$summary_table$repo_n, 3)
  expect_equal(mod_output$summary_table$files_n, nrow(mod_output$table))
  expect_gte(mod_output$summary_table$files_data, 1)
  expect_gte(mod_output$summary_table$files_code, 1)
  expect_gte(mod_output$summary_table$files_readme, 1)
  expect_gte(mod_output$summary_table$files_zip, 1)
})

test_that("Zenodo", {
  testthat::local_mocked_bindings(
    zenodo_links = function(paper) {
      data.frame(
        paper_id = paper$paper_id,
        href = "https://doi.org/10.5281/zenodo.12345",
        stringsAsFactors = FALSE
      )
    },
    zenodo_retrieve = function(zenodo_url, id_col = 1, pb = NULL) {
      data.frame(
        zenodo_url = as.character(zenodo_url),
        files = I(list(list(
          list(key = "analysis.R", size = 100, links = list(self = "https://files.example/analysis.R")),
          list(key = "dataset.csv", size = 200, links = list(self = "https://files.example/dataset.csv")),
          list(key = "README.md", size = 50, links = list(self = "https://files.example/README.md")),
          list(key = "archive.zip", size = 300, links = list(self = "https://files.example/archive.zip")),
          list(key = "archive.7z", size = 400, links = list(self = "https://files.example/archive.7z"))
        ))),
        stringsAsFactors = FALSE
      )
    }
  )

  module <- "repo_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://doi.org/10.5281/zenodo.12345", text_id = 1)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 5)
  expect_setequal(mod_output$table$file_name, c("analysis.R", "dataset.csv", "README.md", "archive.zip", "archive.7z"))
  archive_rows <- mod_output$table[mod_output$table$file_name %in% c("archive.zip", "archive.7z"), ]
  expect_equal(nrow(archive_rows), 2)
  expect_true(all(archive_rows$file_type == "archive"))

  report_text <- paste(mod_output$report, collapse = " ")
  expect_true(grepl("archive\\.zip", report_text))
  expect_true(grepl("archive\\.7z", report_text))
  expect_false(grepl("archives:.*NA", report_text))

  exp <- data.frame(
    paper_id = paper$paper_id,
    repo_n = 1,
    files_n = 5,
    files_data = 1,
    files_code = 1,
    files_readme = 1,
    files_zip = 2
  )
  expect_equal(mod_output$summary_table, exp)
})

httptest2::stop_mocking()
#httptest2::stop_capturing()
