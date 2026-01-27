test_that("code_check offline", {
  module <- "code_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no relevant text
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)
  exp <- data.frame(id = paper$id,
                    code_file_n = 0)
  expect_equal(mod_output$summary_table, exp)
  exp <- "We found 0 R, SAS, SPSS, or Stata code files."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})


test_that("OSF no files", {
  # OSF but no R files
  skip_if_quick()
  skip_osf()

  module <- "code_check"
  paper <- paper()
  paper$full_text <- data.frame(text = "https://osf.io/y6a34", id = paper$id)
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "na")
  exp <- data.frame(id = paper$id,
                    code_file_n = 0)
  expect_equal(mod_output$summary_table, exp)
  exp <- "We found 0 R, SAS, SPSS, or Stata code files."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})

test_that("no code files", {
  skip_osf()

  module <- "code_check"
  paper <- paper()
  paper$full_text <- data.frame(
    text = c("https://osf.io/m4nbv"),
    id = paper$id
  )
  mod_output <- module_run(paper, module)

  exp <- data.frame(id = paper$id,
                    code_file_n = 0)
  expect_equal(mod_output$summary_table, exp)

  exp <- "We found 0 R, SAS, SPSS, or Stata code files."
  expect_equal(mod_output$summary_text, exp)
  expect_equal(mod_output$report, exp)
})

test_that("OSF", {
  skip_osf()

  module <- "code_check"
  paper <- paper()
  paper$full_text <- data.frame(
    text = c("https://osf.io/629bx"),
    id = paper$id
  )
  mod_output <- module_run(paper, module)

  expect_equal(mod_output$traffic_light, "yellow")
  exp <- data.frame(id = paper$id,
                    code_n = 2,
                    code_abs_path = 3,
                    code_missing_files = 0)
  expect_equal(mod_output$summary_table[, 1:4], exp[, 1:4])
})


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
