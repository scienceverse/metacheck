test_that("psychds_file_plan takes a missing name from the path and excludes a file with neither (U113)", {
  # a missing name became the target "unknown/NA"; no file_name column stopped
  s <- data.frame(paper_id = "p", file_name = c("data.csv", NA, NA),
                  file_path = c("data/data.csv", "code/analysis.R", NA),
                  file_type = c("data", "code", "data"), data_type = NA)
  plan <- psychds_file_plan(s, FALSE)$table
  expect_equal(plan$file_name[2], "analysis.R")
  expect_equal(plan$status[3], "excluded")
  s$file_name <- NULL
  expect_equal(nrow(psychds_file_plan(s, FALSE)$table), 3)
})

test_that("psychds_file_plan puts a README in a subfolder in documentation/, named after the folder", {
  # every README went to the top, where they replaced each other
  s <- data.frame(file_name = c("README", "README", "README.md", "README", "README"),
                  file_path = c("README", "nest-1/README", "Archive of OSF Storage/README.md",
                                "data.zip.contents/README", "data.zip/README"),
                  data_type = "documentation", doc_role = "readme", group = NA_character_)
  tb <- psychds_file_plan(s)$table
  expect_equal(tb$target_path, c("README", "documentation/nest-1_README",
                                 "documentation/Archive-of-OSF-Storage_README.md",
                                 "README", "README"))
})

test_that("psychds_file_plan keeps a script's folders below the folder the scripts share", {
  # put flat in analysis/, two default.py collided and Python's imports broke
  s <- data.frame(file_name = c("a.R", "helpers.py", "default.py", "default.py", "b.R"),
                  file_path = c("Code/a.R", "Code/utils/helpers.py", "Code/x/default.py",
                                "Code/y/default.py", "data.zip/Code/b.R"),
                  data_type = "code", group = NA_character_)
  tb <- psychds_file_plan(s)$table
  expect_equal(tb$target_path, c("analysis/a.R", "analysis/utils/helpers.py",
                                 "analysis/x/default.py", "analysis/y/default.py",
                                 "analysis/b.R"))
  # one script in a folder of its own goes straight to analysis/
  one <- data.frame(file_name = "analysis.R", file_path = "Code/analysis.R",
                    data_type = "code", group = NA_character_)
  expect_equal(psychds_file_plan(one)$table$target_path, "analysis/analysis.R")
})

test_that("psychds_file_plan ignores a top folder that holds every file, and keeps data folders", {
  s <- data.frame(file_name = c("README.md", "a.R", "x.csv", "x.csv"),
                  file_path = c("repo-1a2b/README.md", "repo-1a2b/Code/a.R",
                                "repo-1a2b/Data/lb/x.csv", "repo-1a2b/Data/ub/x.csv"),
                  data_type = c("documentation", "code", "data", "data"),
                  doc_role = c("readme", NA, NA, NA), group = NA_character_)
  tb <- psychds_file_plan(s)$table
  expect_equal(tb$target_path, c("README.md", "analysis/a.R",
                                 "data/lb/study-x_data.csv", "data/ub/study-x_data.csv"))
})

test_that("psychds_file_plan leaves correctly placed files and valid data names alone (U112)", {
  # data/eeg.npy was told to move to itself, and study-a_data.csv was renamed
  # data/study-studyadata_data.csv
  s <- data.frame(file_name = c("data/eeg.npy", "data/study-a_data.csv", "raw.csv"),
                  file_path = c("data/eeg.npy", "data/study-a_data.csv", "raw.csv"),
                  data_type = "data", group = NA_character_)
  tb <- psychds_file_plan(s)$table
  expect_equal(tb$status, c("present", "present", "move"))
  expect_equal(tb$target_path[2], "data/study-a_data.csv")
})

test_that("psychds_check gives each paper of a list its own row and report section (U113)", {
  # checked together, the first paper got the one summary row and the
  # others 0 through na_replace
  llm_use(FALSE)
  pa <- test_paper("a"); pa$paper_id <- "A"
  pb <- test_paper("b"); pb$paper_id <- "B"
  structure_df <- data.frame(
    paper_id = c("A", "A", "B", "B", "B"),
    file_name = c("raw.csv", "README.md", "data/study-x_data.csv", "dataset_description.json", "README.md"),
    file_path = c("raw.csv", "README.md", "data/study-x_data.csv", "dataset_description.json", "README.md"),
    data_type = c("data", "documentation", "data", "documentation", "documentation"),
    doc_role = c(NA, "readme", NA, NA, "readme"),
    group = NA_character_)
  columns_df <- data.frame(paper_id = c("A", "B"), source_file = c("raw.csv", "data/study-x_data.csv"),
                           column_name = "x")
  fake <- structure(list(module = "data_check", table = columns_df, paper = paperlist(pa, pb),
                         summary_table = data.frame(paper_id = c("A", "B")),
                         structure = structure_df),
                    class = "metacheck_module_output")
  mo <- module_run(fake, "psychds_check")
  expect_equal(mo$summary_table$required_missing, c(1, 0))
  expect_equal(sum(grepl("^#### Paper ", unlist(mo$report))), 2)
})
