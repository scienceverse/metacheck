test_that("lang_load_regex", {
  # check that this regex captures all of the intended loaders
  pattern <- c(
    "read\\.(csv2?|table|delim2?)",
    "read\\.xlsx",
    "read\\.dta",
    "read_(csv2?|tsv|delim|rds|lines)",
    "read_(xlsx?|excel)",
    "read_(dta|sav|sas)",
    "read_(feather|parquet|yaml|xml|ods)",
    "fread",
    "readRDS",
    "load",
    "readLines",
    "fromJSON",
    "readtext",
    "source"
  ) |>
    paste(collapse = "|") |>
    paste0("\\b(", x = _, ")\\s*\\(")

  x <- c(
    "x = read.csv('stuff')",
    "x=read.csv2('stuff')",
    "x <- read.table('stuff')",
    "x<-read.delim(filename)",
    "x<-read.delim2(filename)",
    "  x   <-   readRDS(filename)",
    "x <- filename |> load()",
    "readLines(file, n = 2)",
    "readr::read_csv('file') -> x",
    "read_csv2 ('file')",
    "read_tsv  ('file')",
    "read_delim('file')",
    "read_rds('file')",
    "read_lines('file')",
    "readLines(file)",
    "fread(file)",
    "read_xlsx(file)",
    "read_xls(file)",
    "read_excel(file)",
    "read_xlsx(file)",
    "read_dta(file)",
    "read_sav(file)",
    "read_sas(file)",
    "read.dta(file)",
    "read_feather(file)",
    "read_parquet(file)",
    "fromJSON(file)",
    "read_yaml(file)",
    "read_xml(file)",
    "read_ods(file)",
    "readtext(file)",
    "source(file)"

  )
  detected <- grepl(pattern, x)
  expect_true(all(detected))

  # shouldn't detect
  x <- c(
    "I can read CSV files",
    "read.CSV()",
    "read.csv is a good function",
    "get that from JSON (if you can)"
  )
  undetected <- grepl(pattern, x)
  expect_false(any(undetected))
})


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
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/y6a34", text_id = 1)
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
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/m4nbv", text_id = 1)
  mo <- module_run(paper, module)

  exp <- data.frame(paper_id = paper$paper_id,
                    code_file_n = 0)
  expect_equal(mo$summary_table, exp)

  exp <- "We found 0 R, SAS, SPSS, or Stata code files."
  expect_equal(mo$summary_text, exp)
  expect_equal(mo$report, exp)
})

test_that("OSF", {
  module <- "code_check"
  paper <- test_paper()
  paper$url <- data.frame(href = "https://osf.io/629bx", text_id = 1)
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "yellow")
  exp <- data.frame(paper_id = paper$paper_id,
                    code_n = 2,
                    code_abs_path = 3,
                    code_missing_files = 0)
  expect_equal(mo$summary_table[, 1:4], exp[, 1:4])
})

test_that("file_limit", {
  # default limit
  paper <- test_paper(url = "https://github.com/scienceverse/demo")
  mo <- module_run(paper, "code_check")
  expect_equal(nrow(mo$table), 20)
  expect_equal(mo$summary_table$code_n, 20)

  # lower limit
  mo <- module_run(paper, module = "code_check", file_limit = 2)
  expect_equal(nrow(mo$table), 2)
  expect_equal(mo$summary_table$code_n, 2)

  # multiple repos
  repos <- c("https://github.com/scienceverse/demo",
             "https://osf.io/629bx")
  paper <- test_paper(url = repos)
  mo <- module_run(paper, module = "code_check", file_limit = 2)
  expect_equal(nrow(mo$table), 4)
  expect_equal(mo$summary_table$code_n, 4)
  expect_setequal(mo$table$repo_url, repos)
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
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "code_check", local_path = local_path)

  for (fname in c("stata_latin1.do", "stata_utf16.do")) {
    row <- mo$table[mo$table$file_name == fname, ]
    expect_true(nrow(row) == 1, label = paste(fname, "found in table"))
    expect_true(row$code_lines > 0, label = paste(fname, "code_lines > 0"))
    expect_false(is.na(row$percentage_comment), label = paste(fname, "percentage_comment not NA"))
  }
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

