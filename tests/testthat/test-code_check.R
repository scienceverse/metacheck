test_that("code_read", {
  expect_true(is.function(metacheck::code_read))
  expect_no_error(helplist <- help(code_read, metacheck))

  expect_error(code_read(NULL))

  file_path <- test_path("fixtures", "code_files", "analysis.R")
  obs <- code_read(file_path)
  expect_equal(obs[[1]], "# Analysis script")
  expect_equal(obs[[4]], "") # check reading blank lines

  file_path <- test_path("fixtures", "code_files", "stata_latin1.do")
  obs <- code_read(file_path)
  expect_equal(obs[[1]], "* Stata do-file with Windows-1252 encoding")
  expect_equal(obs[[2]], "* Author: M\u00FCller")

  file_path <- test_path("fixtures", "code_files", "stata_utf16.do")
  obs <- code_read(file_path)
  expect_equal(obs[[1]], "* Stata do-file UTF-16 LE")
  expect_equal(obs[[2]], "* Author: Mueller")
})


test_that("code_lang", {
  expect_true(is.function(metacheck::code_lang))
  expect_no_error(helplist <- help(code_lang, metacheck))

  expect_equal(code_lang(NULL), character(0))

  file_name <- "file.R"
  obs <- code_lang(file_name)
  expect_equal(obs, "R")

  file_name <- c("file.Rmd", "file.SAS", "file.r", "file.qmd", "file.txt")
  obs <- code_lang(file_name)
  exp <- c("R", "SAS", "R", "R", NA) |> stats::setNames(file_name)
  expect_equal(obs, exp)

  file_name <- character(0)
  obs <- code_lang(file_name)
  expect_equal(obs, character(0))
})

test_that("code_extract_r", {
  expect_true(is.function(metacheck::code_extract_r))
  expect_no_error(helplist <- help(code_extract_r, metacheck))

  expect_error(code_extract_r(NULL))

  # defaults
  file_path <- demofile("qmd")
  obs <- code_extract_r(file_path)
  expect_equal(obs[[1]], "library(metacheck)")

  # NULL save path
  save_path <- NULL
  obs <- code_extract_r(file_path, save_path)
  expect_equal(obs[[1]], "library(metacheck)")

  # save_path set
  save_path <- withr::local_tempfile(fileext = ".R")
  obs <- code_extract_r(file_path, save_path)
  expect_equal(obs, save_path)
  expect_equal(readLines(obs, n = 1), "library(metacheck)")

  # documentation
  obs0 <- code_extract_r(file_path, NULL, 0)
  obs1 <- code_extract_r(file_path, NULL, 1)
  obs2 <- code_extract_r(file_path, NULL, 2)
  in1 <- setdiff(obs1, obs0)
  in2 <- setdiff(obs2, obs1)
  expect_contains(in1, "#| label: setup")
  expect_contains(in2, "#' ### Power Analysis")

  # by text
  text <- c("---",
            "title: Demo",
            "format: html",
            "---",
            "```{r}",
            "a <- 1 + 1",
            "```")

  code <- code_extract_r(text = text)
  expect_equal(code, "a <- 1 + 1")
})


test_that("code_parse_r", {
  expect_true(is.function(metacheck::code_parse_r))
  expect_no_error(helplist <- help(code_parse_r, metacheck))

  expect_error(code_parse_r())

  # R file with error
  file_path <- test_path("fixtures", "parse-errors", "error.R")
  obs <- code_parse_r(file_path)
  expect_equal(obs$file_path, file_path)
  expect_true(obs$error)
  expect_match(obs$msg, "line:4:1")

  # R file no error
  file_path <- test_path("fixtures", "parse-errors", "ok.R")
  obs <- code_parse_r(file_path)
  expect_equal(obs$file_path, file_path)
  expect_false(obs$error)
  expect_equal(obs$msg, NA_character_)

  # Rmd file with error
  file_path <- test_path("fixtures", "parse-errors", "error.Rmd")
  obs <- code_parse_r(file_path)
  expect_equal(obs$file_path, file_path)
  expect_true(obs$error)
  expect_match(obs$msg, "line:4:1")

  # Rmd file no error
  file_path <- test_path("fixtures", "parse-errors", "ok.Rmd")
  obs <- code_parse_r(file_path)
  expect_equal(obs$file_path, file_path)
  expect_false(obs$error)
  expect_equal(obs$msg, NA_character_)

  # text R
  file_path <- test_path("fixtures", "parse-errors", "ok.R")
  text <- code_read(file_path)
  obs <- code_parse_r(text = text)
  expect_equal(obs$file_path, "")
  expect_false(obs$error)
  expect_equal(obs$msg, NA_character_)

  # text Rmd file with error
  file_path <- test_path("fixtures", "parse-errors", "error.Rmd")
  text <- code_read(file_path)
  obs <- code_parse_r(text = text)
  expect_equal(obs$file_path, "")
  expect_true(obs$error)
  expect_match(obs$msg, "line:4:1")

  # vector of paths
  local_path <- test_path("fixtures", "parse-errors")
  file_path <- list.files(local_path, full.names = TRUE)
  obs <- code_parse_r(file_path)
  expect_equal(obs$file_path, file_path)
  expect_equal(obs$error, rep(c(T, F), c(4, 4)))
  expect_match(obs$msg[[1]], "unexpected symbol")
})

test_that("code_abs_path", {
  expect_true(is.function(metacheck::code_abs_path))
  expect_no_error(helplist <- help(code_abs_path, metacheck))

  expect_error(code_abs_path(NULL))

  # avoids false alarms
  code_text <- c(
    "# the abs path is C:/User/lakens/file.R",
    "func(file = 'https://lakens.com/file.R')",
    "file <- \"lakens/file.R\""
  )
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = character(0),
    line = integer(0)
  )
  expect_equal(obs, exp)

  # double quotes - windows /
  code_text <- "file <- \"C:/User/lakens/file.R\""
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = "C:/User/lakens/file.R",
    line = 1
  )
  expect_equal(obs, exp)

  # double quotes - windows \
  code_text <- "file <- \"C:\\User\\lakens\\file.R\""
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = "C:\\User\\lakens\\file.R",
    line = 1
  )
  expect_equal(obs, exp)

  # single quotes - windows
  code_text <- "file <- 'C:/User/lakens/file.R'"
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = "C:/User/lakens/file.R",
    line = 1
  )
  expect_equal(obs, exp)

  # mac/linux
  code_text <- "file <- \"/User/lakens/file.R\""
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = "/User/lakens/file.R",
    line = 1
  )
  expect_equal(obs, exp)

  # mac/linux ~ path
  code_text <- "file <- '~/file.R'"
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = "~/file.R",
    line = 1
  )
  expect_equal(obs, exp)

  # url
  code_text <- "file <- 'https://scienceverse.org/file.R'"
  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = character(0),
    line = integer(0)
  )
  expect_equal(obs, exp)

  # multiple abs
  code_text <- c(
   "file <- 'C:/User/lakens/file.R'",
   "a <- 1 + 1",
   "convert(file, '/User/lakens/file.html')"
  )

  obs <- code_abs_path(code_text)
  exp <- dplyr::tibble(
    abs_path = c("C:/User/lakens/file.R",
                 "/User/lakens/file.html"),
    line = c(1, 3)
  )
  expect_equal(obs, exp)

  # extra quotes
  code_text <- c(
    "x <- read_csv(\"/plots/x.csv\"), units=\"in\", extra = FALSE)"
  )

  obs <- code_abs_path(code_text)
  expect_equal(obs$abs_path, "/plots/x.csv")
})

test_that("code_remove_comments", {
  expect_true(is.function(metacheck::code_remove_comments))
  expect_no_error(helplist <- help(code_remove_comments, metacheck))

  expect_error(code_remove_comments(NULL))

  # R comments
  code_text <- c(
    "# this is a comment",
    "  # and a comment with whitespace",
    "",
    "x <- 'And this is code'"
  )
  lang <- "R"
  obs <- code_remove_comments(code_text, lang)
  exp <- "x <- 'And this is code'"
  expect_equal(exp, obs)

  # SPSS comments
  code_text <- c(
    "COMMENT This is an inline comment using COMMENT.",
    "",
    "* This is a single-line comment using *.",
    "",
    "COMMENT BEGIN",
    "  This is a block comment.",
    "  It can span multiple lines.",
    "COMMENT END.",
    "GET FILE='COMMENT.sav'.",
    " /* This is another block comment",
    "    using slash-star notation. */",
    "",
    "DESCRIPTIVES VARIABLES=age income."
  )
  obs <- code_remove_comments(code_text, "SPSS")
  exp <- c("GET FILE='COMMENT.sav'.",
           "DESCRIPTIVES VARIABLES=age income.")
  expect_equal(exp, obs)

  # SAS comments
  code_text <- c(
      "* This is a single-line comment using *;",
      "",
      "data example;",
      "  set mylib.mydata; * Inline comment after code;",
      "run;",
      "",
      "/* This is a block comment",
      "   that spans multiple lines. */",
      "",
      "proc means data=example;",
      "  var age income;",
      "run;"
    )
  obs <- code_remove_comments(code_text, "SAS")
  exp <- c(
    "data example;",
    "  set mylib.mydata; * Inline comment after code;",
    "run;",
    "proc means data=example;",
    "  var age income;",
    "run;"
  )
  expect_equal(exp, obs)

  # Stata comments
  code_text <- c(
    "* This is a full-line comment using *.",
    "",
    "display \"Hello world\"  // This is an inline comment using //",
    "",
    "/* This is a block comment",
    "   that spans multiple lines. */",
    "",
    "use example.dta, clear",
    "summarize age income"
  )

  obs <- code_remove_comments(code_text, "Stata")
  exp <- c(
    "display \"Hello world\"  ",
    "use example.dta, clear",
    "summarize age income"
  )
  expect_equal(exp, obs)

})

test_that("code_line_stats", {
  expect_true(is.function(metacheck::code_line_stats))
  expect_no_error(helplist <- help(code_line_stats, metacheck))

  expect_error(code_line_stats(NULL))

  # R
  code_text <- c(
    'a <- 1 # inline comment',
    '',
    '',
    '',
    '# comment',
    '   # space before comment'
  )
  obs <- code_line_stats(code_text, "R")
  exp <- list(total_lines = 3L,
              comment_lines = 2L,
              code_lines = 1L,
              percent_comments = 2/3)
  expect_equal(obs, exp)

  # SPSS
  code_text <- c(
    "COMMENT This is an inline comment using COMMENT.", #1
    "",
    "* This is a single-line comment using *.", #2
    "",
    "COMMENT BEGIN",                           #3
    "  This is a block comment.",              #4
    "  It can span multiple lines.",           #5
    "COMMENT END.",                            #6
    "GET FILE='COMMENT.sav'.",                 # code 1
    " /* This is another block comment",       #7
    "    using slash-star notation. */",       #8
    "",
    "DESCRIPTIVES VARIABLES=age income."       # code 2
  )
  obs <- code_line_stats(code_text, "SPSS")
  exp <- list(total_lines = 10L,
              comment_lines = 8L,
              code_lines = 2L,
              percent_comments = 0.8)
  expect_equal(obs, exp)
})


test_that("code_file_refs", {
  expect_true(is.function(metacheck::code_file_refs))
  expect_no_error(helplist <- help(code_file_refs, metacheck))

  expect_error(code_file_refs(NULL))

  # R
  code_text <- c(
    'source("functions.R")',
    'a <- "bread"; a1 <- "file0.csv"', # don't match possible file names not on a read line
    'b <- read.csv("file.csv")', # match
    '# b <- read.csv("old_file.csv")', # don't match commented out
    'b2 <- readr::read_csv("subdir/file.csv")', # match relative paths
    'b3 <- read_csv("file2.csv", arg = "file3")' # don't match quoted non-file
  )
  lang <- "R"
  obs <- code_file_refs(code_text, lang)
  exp <- c("functions.R",
           "file.csv",
           "subdir/file.csv",
           "file2.csv")
  expect_equal(obs, exp)

  # SPSS
  code_text <- c(
    "* --- Load native SPSS file ---",
    "GET FILE='data/example.sav'.",
    "",
    "* --- Load portable SPSS file ---",
    "IMPORT FILE='data/example.por'.",
    "",
    "* --- Load Excel (xls/xlsx) ---",
    "GET DATA",
    "  /TYPE=XLSX",
    "  /FILE='data/example.xlsx'",
    "  /SHEET=name 'Sheet1'",
    "  /CELLRANGE=full",
    "  /READNAMES=on."
  )
  lang <- "SPSS"
  obs <- code_file_refs(code_text, lang)
  exp <- c("data/example.sav",
           "data/example.por",
           "data/example.xlsx")
  expect_equal(obs, exp)


  code_text <- c(
    "* --- Load CSV / delimited text ---",
    "GET DATA",
    "  /TYPE=TXT",
    "  /FILE='data/example.csv'",
    "  /DELCASE=LINE",
    "  /DELIMITERS=\",\"",
    "  /ARRANGEMENT=DELIMITED",
    "  /FIRSTCASE=2",
    "  /VARIABLES=",
    "    id F8.0",
    "    age F8.0",
    "    income F8.2.",
    "",
    "* --- Load tab-delimited file ---",
    "GET DATA",
    "  /TYPE=TXT",
    "  /FILE='data/example.tsv'",
    "  /DELIMITERS=\"\\t\"",
    "  /ARRANGEMENT=DELIMITED",
    "  /FIRSTCASE=2.",
    "",
    "* --- Load fixed-width text file ---",
    "DATA LIST FILE='data/fixed.txt'",
    "  /id 1-4",
    "   age 5-6",
    "   income 7-12.",
    "",
    "* --- Load using ODBC (database) ---",
    "GET DATA",
    "  /TYPE=ODBC",
    "  /CONNECT='DSN=mydb;UID=user;PWD=pass;'",
    "  /SQL='SELECT * FROM mytable'.",
    "",
    "* --- Load SAS file ---",
    "GET SAS DATA='data/example.sas7bdat'.",
    "",
    "* --- Load Stata file ---",
    "GET STATA FILE='data/example.dta'.",
    "",
    "* --- Load data via FILE HANDLE ---",
    "FILE HANDLE myfile /NAME='data/example.txt'.",
    "GET DATA",
    "  /TYPE=TXT",
    "  /FILE=myfile",
    "  /DELIMITERS=\",\".",
    "",
    "* --- Inline data (not external) ---",
    "DATA LIST LIST /x y.",
    "BEGIN DATA",
    "1 2",
    "3 4",
    "END DATA."
  )
  obs <- code_file_refs(code_text, lang)
  exp <- c("data/example.csv",
           "data/example.tsv",
           "data/fixed.txt",
           "data/example.sas7bdat",
           "data/example.dta",
           "data/example.txt")
  expect_equal(obs, exp)
})

test_that("code file refs full", {
  code_text <- c(
    "x = read.csv('file1.txt')",
    "x=read.csv2('file2.txt')",
    "x <- read.table('file3.txt')",
    "x<-read.delim('file4.txt')",
    "x<-read.delim2('file5.txt')",
    "  x   <-   readRDS('file6.txt')",
    "x <- 'file7.txt' |> load()",
    "readLines('file8.txt', n = 2)",
    "readr::read_csv('file9.txt') -> x",
    "read_csv2 ('file10.txt')",
    "read_tsv  ('file11.txt')",
    "read_delim('file12.txt')",
    "read_rds('file13.txt')",
    "read_lines('file14.txt')",
    "readLines('file15.txt')",
    "fread('file16.txt')",
    "read_xlsx('file17.txt')",
    "read_xls('file18.txt')",
    "read_excel('file19.txt')",
    "read_xlsx('file20.txt')",
    "read_dta('file21.txt')",
    "read_sav('file22.txt')",
    "read_sas('file23.txt')",
    "read.dta('file24.txt')",
    "read_feather('file25.txt')",
    "read_parquet('file26.txt')",
    "fromJSON('file27.txt')",
    "read_yaml('file28.txt')",
    "read_xml('file29.txt')",
    "read_ods('file30.txt')",
    "readtext('file31.txt')",
    "source('file32.txt')"
  )

  obs <- code_file_refs(code_text, "R")
  exp <- paste0("file", 1:32, ".txt")
  expect_equal(obs, exp)

  # shouldn't detect
  code_text_no <- c(
    "I can read CSV files",
    "read.CSV()",
    "read.csv is a good function",
    "get that from JSON (if you can)"
  )
  obs <- code_file_refs(code_text_no, "R")
  expect_equal(obs, character(0))
})

test_that("code_library_lines", {
  expect_true(is.function(metacheck::code_library_lines))
  expect_no_error(helplist <- help(code_library_lines, metacheck))

  expect_error(code_library_lines(NULL))

  code_text <- c(
    "line = 1",
    "library(dplyr)",
    "",
    "# this line won't count",
    "library(\"tidyr\")",
    "line = 5",
    "renv::install('metacheck')"
  )
  obs <- code_library_lines(code_text, "R")
  exp <- dplyr::tibble(
    code = code_text[c(2, 5, 7)],
    line = c(2L, 3L, 5L)
  )
  expect_equal(exp, obs)
})


