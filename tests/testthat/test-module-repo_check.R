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


test_that("OSF view_only link", {
  osf_url <- "https://osf.io/t9j8e/? view_only=f171281f212f4435917b16a9e581a73b"
  paper <- test_paper(url = osf_url)
  obs <- module_run(paper, "repo_check")

  expect_in(obs$table$repo_url, osf_url)
}, "mock")

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
}, "mock")

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
}, "mock")

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
}, "mock")

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
}, "mock")

test_that("Zenodo", {
  paper <- test_paper(url = "https://zenodo.org/records/17754445")
  module <- "repo_check"
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$table$file_name, "ResearchBox_4377.zip")
  expect_equal(mod_output$summary_table$files_zip, 1)

  paper <- paperlist(
    test_paper(url = "https://zenodo.org/records/17754445"),
    test_paper(url = "https://zenodo.org/records/123456789")
  )
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$summary_table$files_n, c(1, 1))
  expect_equal(mod_output$summary_table$files_zip, c(1, 0))
}, "mock")

# repo_check() + local_path ----

test_that("repo_check vector of local paths", {
  local_path <- c(
    test_path("fixtures", "code_files", "analysis.R"),
    test_path("fixtures", "code_files", "README.md")
  )
  paper <- test_paper()
  module <- "repo_check"
  mo <- module_run(paper, module, local_path = local_path)

  expect_equal(mo$summary_table$repo_n, 2)
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md"  %in% mo$table$file_name)
})

test_that("repo_check local_path only", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "repo_check", local_path = local_path)

  # one repo: the local folder
  expect_equal(mo$summary_table$repo_n, 1)
  expect_equal(mo$summary_table$files_n, 7)
  expect_equal(mo$summary_table$files_code, 5)  # .do is "stats" type, not "code"
  expect_equal(mo$summary_table$files_data, 1)
  expect_equal(mo$summary_table$files_readme, 1)
  expect_equal(mo$summary_table$files_zip, 0)

  # repo type is "local"
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md" %in% mo$table$file_name)

  # README present, no zip: green
  expect_equal(mo$traffic_light, "green")
})

test_that("repo_check paper + local_path", {
  # osf/629bx mock: 4 files (2 code, 1 data, 0 readme, 1 zip)
  # fixture_dir:    5 files (3 code, 1 data, 1 readme, 0 zip)
  paper <- test_paper(url = "https://osf.io/629bx")
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(paper, "repo_check", local_path = local_path)

  # two repos: OSF + local
  expect_equal(mo$summary_table$repo_n, 2)
  expect_equal(mo$summary_table$files_n, 11)

  # combined counts
  expect_equal(mo$summary_table$files_code, 7)  # .do is "stats" type, not "code"
  expect_equal(mo$summary_table$files_readme, 1)  # only local has README
  expect_equal(mo$summary_table$files_zip, 1)      # only OSF has zip

  # files from both sources present in table
  expect_true("README.md" %in% mo$table$file_name)   # local
  expect_true("analysis.R" %in% mo$table$file_name)  # local
  expect_true(any(grepl("bad\\.R$", mo$table$file_name, ignore.case = TRUE))) # OSF

  # OSF still has no README → repo_no_readme > 0 → yellow
  expect_equal(mo$traffic_light, "yellow")
}, "mock")


# repo_check() + local_only ----

test_that("repo_check local_only = TRUE ignores online repos, checks local only", {
  # paper has OSF link that would normally return 4 files
  paper     <- test_paper(url = "https://osf.io/629bx")
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(paper, "repo_check", local_path = local_path, local_only = TRUE)

  # only 1 repo: the local folder — OSF link silently skipped
  expect_equal(mo$summary_table$repo_n, 1)
  # all files come from the local fixture (7 files)
  expect_equal(mo$summary_table$files_n, 7)
  # no OSF URLs anywhere in the table
  expect_false(any(grepl("osf\\.io", mo$table$repo_url, ignore.case = TRUE)))
  # local files present
  expect_true("analysis.R" %in% mo$table$file_name)
  expect_true("README.md"  %in% mo$table$file_name)
})

test_that("repo_check local_only = TRUE with no local_path returns na", {
  # nothing to check: online skipped, no local path provided
  mo <- module_run(test_paper(), "repo_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$repo_n, 0)
})

test_that("repo_check local_only = TRUE with online URLs but no local_path returns na", {
  # paper has an OSF link, but local_only suppresses online lookup; no local_path given
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "repo_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$repo_n, 0)
})

test_that("repo_check local_only = FALSE is the same as the default", {
  # passing local_only = FALSE should produce identical output to omitting it
  local_path <- test_path("fixtures", "code_files")
  paper      <- test_paper()

  mo_default  <- module_run(paper, "repo_check", local_path = local_path)
  mo_explicit <- module_run(paper, "repo_check", local_path = local_path, local_only = FALSE)

  expect_equal(mo_default$summary_table, mo_explicit$summary_table)
  expect_equal(mo_default$table,         mo_explicit$table)
  expect_equal(mo_default$traffic_light, mo_explicit$traffic_light)
}, "mock")

test_that("repo_check local_only = TRUE without local_path ignores multiple online repo types", {
  # paper has OSF, GitHub, and ResearchBox links — all should be skipped
  text  <- c("osf.io/629bx", "github.com/scienceverse/demo", "https://researchbox.org/4377")
  paper <- test_paper()
  paper$url <- data.frame(href = text, text_id = 1:3)
  mo <- module_run(paper, "repo_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$repo_n, 0)
})
