test_that("code_check offline", {
  module <- "code_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- test_paper("no text")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "na")
  expect_null(mo$table)
  exp <- data.frame(paper_id = paper$paper_id,
                    code_file_n = 0)
  expect_equal(mo$summary_table, exp)
  expect_match(mo$summary_text, "0")
  expect_match(mo$report, "0")
})


#httptest2::start_capturing()
httptest2::use_mock_api()


test_that("OSF no files", {
  # OSF but no R files

  module <- "code_check"
  paper <- test_paper(url = "https://osf.io/y6a34")
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "na")
  exp <- data.frame(paper_id = paper$paper_id,
                    code_file_n = 0)
  expect_equal(mo$summary_table, exp)
  expect_match(mo$summary_text, "0")
  expect_match(mo$report, "0")
})

test_that("no code files", {
  module <- "code_check"
  paper <- test_paper(url = "https://osf.io/m4nbv")
  mo <- module_run(paper, module)

  exp <- data.frame(paper_id = paper$paper_id,
                    code_file_n = 0)
  expect_equal(mo$summary_table, exp)

  exp <- "We found 0 R, 0 SAS, 0 SPSS, and 0 Stata code files."
  expect_equal(mo$summary_text, exp)
  expect_equal(mo$report, exp)
})

test_that("OSF", {
  module <- "code_check"
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "yellow")
  exp <- data.frame(paper_id = paper$paper_id,
                    code_n = 2,
                    code_checked = 2,
                    code_abs_path = 3,
                    code_missing_files = 2)
  expect_equal(mo$summary_table[, 1:5], exp[, 1:5])
})

test_that("file_limit", {
  # default limit
  paper <- test_paper()
  local_path <- test_path("fixtures", "demo", "code") # has 25 files
  n_files <- list.files(local_path) |> length()
  mo <- module_run(paper, "code_check", local_path = local_path)
  expect_equal(nrow(mo$table), n_files)
  expect_equal(mo$summary_table$code_n, n_files)
  expect_equal(mo$summary_table$code_checked, 20)

  # lower limit
  mo <- module_run(paper, module = "code_check",
                   file_limit = 2, local_path = local_path)
  expect_equal(nrow(mo$table), n_files)
  expect_equal(mo$summary_table$code_n, n_files)
  expect_equal(mo$summary_table$code_checked, 2)

  # multiple repos
  local_path <- c(
    test_path("fixtures", "demo", "code"),
    test_path("fixtures", "demo", "good-example.R")
  )
  paper <- test_paper()
  mo <- module_run(paper, module = "code_check",
                   file_limit = 2, local_path = local_path)
  expect_equal(nrow(mo$table), n_files+1)
  expect_equal(mo$summary_table$code_n, n_files+1)
  expect_equal(mo$summary_table$code_checked, 3)
  expect_setequal(mo$table$repo_url, local_path)
})


test_that("multiple paper issue", {
  # https://github.com/scienceverse/metacheck/issues/260
  # Error: Running the module 'code_check' produced errors: arguments imply differing number of rows: 0, 1

  #paper <- psychsci[6:10]

  # problem is multiple papers with no code files
  paper <- paperlist(
    test_paper(url = "https://osf.io/t9j8e/"),
    test_paper()
  )
  mo <- module_run(paper, "code_check")

  expect_setequal(mo$summary_table$paper_id, paper_id(paper))
})

# code_check() + local_path ----

test_that("code_check reads non-UTF-8 encoded files without NA", {
  # stata_latin1.do: Windows-1252 encoded (non-ASCII bytes invalid in UTF-8)
  # stata_utf16.do:  UTF-16 LE encoded (NUL bytes after every ASCII char)
  # Both previously produced code_lines=0 and percentage_comment=NA
  paper <- test_paper()
  local_path <- test_path("fixtures", "code_files", "stata_latin1.do")
  mo <- module_run(paper, "code_check", local_path = local_path)
  expect_gt(mo$table$code_lines, 0)


  local_path <- test_path("fixtures", "code_files", "stata_utf16.do")
  mo <- module_run(paper, "code_check", local_path = local_path)
  expect_gt(mo$table$code_lines, 0)
})

test_that("code_check local_path errors", {
  # non-existent path propagates as an error
  expect_warning(
    module_run(test_paper(), "code_check", local_path = "/no/such/path/exists"),
    "/no/such/path/exists"
  )
})

test_that("code_check local_path no code files", {
  tmp <- withr::local_tempdir()
  writeLines("x,y\n1,2", file.path(tmp, "data.csv"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$code_file_n, 0)
})

test_that("code_check local_path finds code files", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "code_check", local_path = local_path)

  expect_equal(mo$traffic_light, "yellow")
  # fixture has analysis.R, analysis_no_comments.R, subdir/helper.R, stata_latin1.do, stata_utf16.do
  expect_equal(mo$summary_table$code_n, 5)
  expect_true(all(c("analysis.R", "analysis_no_comments.R", "helper.R",
                    "stata_latin1.do", "stata_utf16.do") %in% mo$table$file_name))
})

test_that("code_check local_path: present files are not flagged missing", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "code_check", local_path = local_path)

  # analysis.R loads data.csv, which IS in the fixture dir
  analysis_row <- mo$table[mo$table$file_name == "analysis.R", ]
  expect_equal(analysis_row$loaded_files_missing, 0)
})

test_that("code_check local_path: absent files are flagged missing", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "code_check", local_path = local_path)

  # analysis_no_comments.R loads missing_file.csv, which is NOT in the fixture dir
  no_comments_row <- mo$table[mo$table$file_name == "analysis_no_comments.R", ]
  expect_equal(no_comments_row$loaded_files_missing, 1)
  expect_match(no_comments_row$loaded_files_missing_names, "missing_file.csv")
})

test_that("code_check local_path: files without comments are flagged", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "code_check", local_path = local_path)

  no_comments_row <- mo$table[mo$table$file_name == "analysis_no_comments.R", ]
  expect_equal(no_comments_row$percentage_comment, 0)
})

test_that("code_check paper + local_path", {
  # OSF 629bx has 2 code files; fixture_dir has 5 (3 R + 2 Stata) → total 7
  local_path <- test_path("fixtures", "code_files")
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "code_check", local_path = local_path)

  expect_equal(mo$summary_table$code_n, 7)

  # code files from both repos in the table
  expect_true("analysis.R" %in% mo$table$file_name)  # local
  expect_true("bad.R" %in% mo$table$file_name)        # OSF

  # two distinct repo_url values
  expect_setequal(
    unique(mo$table$repo_url),
    c("https://osf.io/629bx", local_path)
  )
})

httptest2::stop_mocking()
#httptest2::stop_capturing()

# parse errors ----

test_that("parse errors", {
  local_path <- test_path("fixtures", "parse-errors")
  paper <- test_paper()
  module <- "code_check"
  mo <- module_run(paper, module, local_path = local_path)

  exp <- data.frame(
    repo_name = rep("parse-errors", 8),
    repo_url = rep(local_path, 8),
    file_name = c(
      "error-ok.qmd",
      "error.R",
      "error.Rmd",
      "error.qmd",
      "knit-error.Rmd",
      "ok.R",
      "ok.Rmd",
      "ok.qmd"
    ),
    file_url = rep(NA_character_, 8),
    file_size = c(155, 57, 212, 336, 102, 127, 304, 304),
    file_type = rep("code", 8),
    language = rep("R", 8),
    checked = rep(TRUE, 8),
    parse_error = rep(c(TRUE, FALSE), c(4, 4)),
    parse_error_msg = c("line:5:1: unexpected symbol\n4: \n5: a\n   ^",
                        "line:4:1: unexpected symbol\n3: \n4: a\n   ^",
                        "line:4:1: unexpected symbol\n3: \n4: a\n   ^",
                        "line:4:1: unexpected symbol\n3: \n4: a\n   ^",
                        NA, NA, NA, NA),
    code_abs_path = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L),
    absolute_paths = c("", "", "", "\"/User/lisa/file.csv\"", "", "", "", ""),
    library_lines = c(1L, 1L, 1L, 1L, 0L, 3L, 0L, 0L),
    library_max_between = c(NA, NA, NA, NA, NA, 5L, NA, NA),
    comment_lines = c(1L, 1L, 1L, 3L, 1L, 2L, 4L, 4L),
    code_lines = c(4L, 2L, 2L, 3L, 1L, 7L, 1L, 1L),
    percentage_comment = c(0.2, 1/3, 1/3, 0.5, 0.5, 2/9, 0.8, 0.8) ,
    loaded_files_missing = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L),
    loaded_files_missing_names = c("", "", "", "file.csv", "", "", "", "")
  ) |> dplyr::arrange(file_name)
  obs <- dplyr::arrange(mo$table, file_name)

  for (nm in names(obs)) {
    expect_equal(obs[[nm]], exp[[nm]])
  }

  # summary table
  exp <- data.frame(
    paper_id = paper_id(paper),
    code_n = 8,
    code_checked = 8,
    code_abs_path = 1,
    code_missing_files = 1,
    code_min_comments = 0.2,
    code_parse_errors = 4
  )
  expect_equal(mo$summary_table, exp)
})


