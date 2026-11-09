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

  # A genuinely empty (0-byte) file must not crash: readr::guess_encoding()
  # returns a zero-row result for one, which previously produced NA and
  # crashed deep inside readr's own internals with the generic message
  # "missing value where TRUE/FALSE needed" -- confirmed against real 0-byte
  # files in real GitHub repositories during the code_check validation.
  empty_path <- withr::local_tempfile()
  file.create(empty_path)
  obs <- code_read(empty_path)
  expect_equal(obs, character(0))

  # A byte the guessed encoding leaves undefined (0x81 in windows-1252)
  # crashed R itself inside readr::read_lines() (vroom), tryCatch() or not;
  # it is now written as "<81>"
  odd_path <- withr::local_tempfile()
  writeBin(c(charToRaw("ab"), as.raw(0x81), charToRaw("cd\n")), odd_path)
  expect_equal(code_read(odd_path), "ab<81>cd")

  # a byte order mark is dropped; LF, CRLF and CR all end a line
  bom_path <- withr::local_tempfile()
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("x <- 1\r\ny <- 2\rz <- 3\n")), bom_path)
  expect_equal(code_read(bom_path), c("x <- 1", "y <- 2", "z <- 3"))

  # a URL that answers with an HTTP error stops, instead of its error page
  # being read as the file's code
  local_mocked_bindings(
    curl_fetch_memory = function(url, ...)
      list(status_code = 404L, content = charToRaw("404: Not Found")),
    .package = "curl")
  expect_error(code_read("https://example.org/analysis.R"),
               "HTTP status was 404", fixed = TRUE)
})

test_that("code_read reads UTF-8 without readr's guess, which needs stringi (#620)", {
  # the guess fails, as it did with a stringi that is installed but cannot load
  local_mocked_bindings(
    guess_encoding = function(...) stop("stringi package required for encoding operations"),
    .package = "readr")
  # ASCII and UTF-8 are read as before
  expect_equal(code_read(test_path("fixtures", "code_files", "analysis.R"))[[1]], "# Analysis script")
  utf8_path <- withr::local_tempfile()
  writeBin(charToRaw(enc2utf8(paste0("# M", intToUtf8(252), "ller\nx <- 1\n"))), utf8_path)
  expect_equal(code_read(utf8_path), c(paste0("# M", intToUtf8(252), "ller"), "x <- 1"))
  # bytes that are not UTF-8 fall back to Latin-1 instead of failing
  expect_equal(code_read(test_path("fixtures", "code_files", "stata_latin1.do"))[[2]],
               paste0("* Author: M", intToUtf8(252), "ller"))
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

test_that("code_lang recognizes Julia and NetLogo (issue #456)", {
  # Previously missing from .ext_registry entirely, so data_classify_files()
  # (via metacheck::file_types' coarse crosswalk) correctly typed a .jl file
  # "code", but code_lang() returned NA and code_check() never treated it as
  # a candidate for that same file -- the two subsystems disagreed.
  file_name <- c("model.jl", "model.nlogo")
  obs <- code_lang(file_name)
  exp <- c("Julia", "NetLogo") |> stats::setNames(file_name)
  expect_equal(obs, exp)
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

  # Regex escapes must NOT be mistaken for UNC paths. A backslash string in
  # code is almost always a regex ("\\d+", "\\1-\\2", "\\-A[MF12]+"), which a
  # naive "starts with \\" rule wrongly flagged as a \\server path. These write
  # doubled backslashes in R source (single on disk); build them via a temp
  # file so the reader sees the real bytes.
  # Build the on-disk bytes directly with rawToChar so there is no ambiguity
  # about R string escaping. A regex in a code file has ONE backslash before
  # the metacharacter ("\d+"); a UNC path has TWO leading backslashes then a
  # host and share ("\\host\share").
  bs <- "\\"                                   # a single backslash
  regex_code <- paste0(
    'x <- gsub("', bs, 'd+', bs, '.', bs, 'd*", "", y)\n',
    'z <- gsub("', bs, '1-', bs, '2", "", z)\n',
    'a <- strsplit(v, "', bs, '-A[MF12]+")')
  obs <- code_abs_path(regex_code)
  expect_equal(nrow(obs), 0L)                  # no regex flagged as a path

  # A GENUINE UNC path (\\host\share\...) is still flagged.
  unc_code <- paste0('read.csv("', bs, bs, 'fileserver', bs, 'share',
                     bs, 'data.csv")')
  obs <- code_abs_path(unc_code)
  expect_equal(nrow(obs), 1L)
  expect_true(startsWith(obs$abs_path, paste0(bs, bs, "fileserver")))

  # An all-digit UNC "host" is a sub()/gsub() backreference string, not a
  # network path (confirmed against real corpus code: sub('group (\\d+)_(\\d+)',
  # "\\2\\1", x) and gsub('(.{1,10})(\\s|$)', '\\1\n', x) were both wrongly
  # flagged as \\host\share paths because a bare digit satisfied the old
  # hostname character class). A real UNC hostname always has at least one
  # letter, so requiring one excludes this without excluding real hosts.
  backref_code <- paste0('sub("group (', bs, 'd+)_(', bs, 'd+)", "',
                        bs, '2', bs, '1", colnames(clupo))')
  obs <- code_abs_path(backref_code)
  expect_equal(nrow(obs), 0L)

  # A single-segment string starting with "/" is far more often an HTTP route
  # (self.fetch('/login')) or a paste0(var, "/eti")-style relative fragment
  # than a real absolute path -- every genuine absolute path has at least two
  # segments (e.g. "/Users/name"), so this is not flagged.
  route_code <- "self.fetch('/login')"
  obs <- code_abs_path(route_code)
  expect_equal(nrow(obs), 0L)
})

test_that("code_setwd", {
  expect_true(is.function(metacheck::code_setwd))
  expect_no_error(helplist <- help(code_setwd, metacheck))

  # real calls, including relative/dynamic arguments -- these still count as
  # flags, since the anti-pattern is changing the working directory at all,
  # not specifically a hardcoded absolute-path argument
  code_text <- c(
    "setwd('D:/Dropbox/project')",
    "x <- read.csv('data.csv')"
  )
  obs <- code_setwd(code_text)
  expect_equal(obs$setwd_call, "setwd('D:/Dropbox/project')")
  expect_equal(obs$line, 1)

  for (call in c("setwd(dir)", "setwd(tempdir())", "setwd(getwd())")) {
    obs <- code_setwd(call)
    expect_equal(nrow(obs), 1L)
  }

  # "setwd(...)" mentioned inside a string literal (e.g. instructional text in
  # a message() call) is not a live call -- code_setwd() has no string-literal
  # awareness on its own, so this reuses .code_pos_in_string() (the same
  # quote-tracking logic .code_strip_inline_comment() already uses) to skip it.
  fp_code <- 'message("Please run setwd(your_path) before continuing")'
  obs <- code_setwd(fp_code)
  expect_equal(nrow(obs), 0L)

  # a real call on the SAME line as an earlier, unrelated string is still
  # found -- quote-tracking must not treat the whole rest of the line as
  # "inside a string" once the earlier string has closed.
  mixed_code <- 'x <- "some/path.csv"; setwd("D:/Dropbox/project")'
  obs <- code_setwd(mixed_code)
  expect_equal(nrow(obs), 1L)
  expect_match(obs$setwd_call, "^setwd")

  # no calls at all
  expect_equal(nrow(code_setwd("x <- 1")), 0L)
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
  # https://github.com/scienceverse/metacheck/issues/261 -- a mixed
  # code+comment line (line 1) is both a code line AND a comment line; it is
  # no longer invisible to comment_lines just because it also counts as code.
  # All 3 surviving (non-blank) lines here contain a comment: the inline one,
  # plus the two whole-line ones.
  exp <- list(total_lines = 3L,
              comment_lines = 3L,
              code_lines = 1L,
              percent_comments = 1,
              has_docstring = NA)
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
              percent_comments = 0.8,
              has_docstring = NA)
  expect_equal(obs, exp)
})

test_that("code_line_stats counts mixed code+comment lines correctly (issue #261)", {
  # https://github.com/scienceverse/metacheck/issues/261 -- comment_lines was
  # derived as total_lines - blank_lines - code_lines, a pure line-count
  # subtraction. A line with BOTH code and a trailing comment counted as
  # exactly 1 code line either way, so its comment was invisible to
  # comment_lines regardless of language. Reproduces the reported OSF file
  # (osf.io/download/n85ez/, "2_2019_2020_analyses.R") pattern that made this
  # concrete: repeated model-fitting calls with a trailing comment naming
  # which model term each line represents, e.g.
  #   model1_Pos <- lme(Pos ~ scale_ini_age,# Level 2: scale_ini_age;
  #                      random=~1|id, ...)  #random intercept=id
  # -- two lines, each with real code AND a trailing comment, both of which
  # the old logic counted as pure code.
  code_text <- c(
    "model1_Pos <- lme(Pos ~ scale_ini_age,# Level 2: scale_ini_age;",
    "                   random=~1|id, data=dat)  #random intercept=id",
    "model2_Pos <- lme(Pos ~ scale_ini_age*covid,#Level 1: covid condition;",
    "                   random=~1+covid|id, data=dat)  #random slope=covid"
  )
  obs <- code_line_stats(code_text, "R")
  expect_equal(obs$total_lines, 4L)
  expect_equal(obs$code_lines, 4L)      # every line still has real code
  expect_equal(obs$comment_lines, 4L)   # every line ALSO has a trailing comment
  expect_equal(obs$percent_comments, 1)

  # the reported file's exact counts, reproduced directly:
  # code_check reported comment_lines = 19 (whole-line only); manual count
  # found "more than 30" (19 whole-line + 32 trailing = 51).
  n_whole_line <- 19L
  n_trailing <- 32L
  total <- n_whole_line + n_trailing
  synthetic <- c(
    rep("# a whole-line comment", n_whole_line),
    rep("x <- fit_model(y ~ z, data = dat) # a trailing comment", n_trailing)
  )
  obs2 <- code_line_stats(synthetic, "R")
  expect_equal(obs2$comment_lines, total)
  expect_equal(obs2$code_lines, n_trailing)
})

test_that("code_remove_comments strips trailing R comments (issue #261)", {
  # R previously had NO trailing-comment handling at all, unlike Python/Stata/
  # MATLAB which all already stripped a trailing comment via
  # .code_strip_inline_comment(). This both fed the counting bug above and let
  # comment TEXT leak into code_abs_path()/code_file_refs()/
  # code_library_lines() as false positives (a comment mentioning a path,
  # e.g. "# see C:/Users/x/data.csv", used to be read as if it were code).
  code_text <- c(
    "x <- 1 # a trailing comment",
    "y <- read.csv('a#b.csv')",       # "#" inside a string: must survive
    "z <- 'no # here either'",        # single-quoted string
    "# a whole-line comment",
    "w <- 2"
  )
  obs <- code_remove_comments(code_text, "R")
  expect_equal(obs, c(
    "x <- 1 ",
    "y <- read.csv('a#b.csv')",
    "z <- 'no # here either'",
    "w <- 2"
  ))

  # A comment naming an absolute path is no longer read as a real one.
  commented_path <- "# see C:/Users/example/data.csv for details"
  nc <- code_remove_comments(c(commented_path, "x <- 1"), "R")
  expect_equal(nrow(code_abs_path(nc)), 0L)
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

test_that("code_file_refs detects rio import()/import_list()", {
  # rio's readers are the loaders that do NOT start with "read", so they are
  # listed explicitly in the load pattern. import() dispatches on the file
  # extension, so it must be caught for every format, not just one.
  code_text <- c(
    "d1 <- import('file1.csv')",
    "d2 <- rio::import('file2.ods')",
    "d3 <- import('file3.xlsx')",
    "d4 <- import('file4.sav')",
    "d5 <- import_list('file5.xlsx')"
  )
  expect_equal(code_file_refs(code_text, "R"), paste0("file", 1:5,
    c(".csv", ".ods", ".xlsx", ".sav", ".xlsx")))

  # The Python-bridge sense of import() must NOT yield a file reference: a hit
  # only becomes a reference when the call holds a quoted string with a file
  # extension, and a module name has none. Substrings of ordinary identifiers
  # ("importance") must not match the call pattern at all.
  code_text_no <- c(
    "np <- reticulate::import('numpy')",
    "os <- import('os')",
    "x <- import(pkg)",
    "importance <- varImp(fit)",
    "imported_data <- 3"
  )
  expect_equal(code_file_refs(code_text_no, "R"), character(0))

  # rio's WRITE function is an output, not an input: including it by default
  # would report a produced file as a missing input.
  expect_equal(code_file_refs("export(d, 'out.csv')", "R"), character(0))
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


test_that("code_library_names R", {
  expect_true(is.function(metacheck::code_library_names))
  expect_no_error(helplist <- help(code_library_names, metacheck))

  code_text <- c(
    "library(dplyr)",
    "require('tidyr')",
    "requireNamespace(\"purrr\")",
    "pacman::p_load(ggplot2, readr)",
    "x <- stringr::str_trim(' a ')",
    "renv::install('metacheck')",
    "install.packages(c('a', 'b'))",
    "# library(commented)"        # comment: must be ignored
  )
  obs <- code_library_names(code_text, "R")

  # package + source pairs, order-independent
  expect_setequal(
    paste(obs$package, obs$source),
    c("dplyr library", "tidyr require", "purrr requireNamespace",
      "ggplot2 p_load", "readr p_load", "pacman namespace",
      "stringr namespace", "metacheck install", "renv namespace",
      "a install", "b install")
  )
  # the commented-out library is not captured
  expect_false("commented" %in% obs$package)
  # columns are exactly package, source, line
  expect_equal(names(obs), c("package", "source", "line"))
})

test_that("code_library_names reads the packages of groundhog.library()", {
  # c("a", "b") was read as library(c), and a variable as a package with the
  # variable's name, so packages called "c" and "pkgs" were installed.
  pkgs_of <- function(code) unique(code_library_names(code, "R")$package)

  expect_equal(pkgs_of('groundhog.library("dplyr", "2023-01-01")'), "dplyr")
  expect_equal(pkgs_of('groundhog.library(dplyr, "2023-01-01")'), "dplyr")
  expect_equal(pkgs_of('groundhog.library(pkg = "dplyr", date = "2023-01-01")'), "dplyr")
  expect_setequal(pkgs_of('groundhog.library(c("dplyr", "lme4"), "2023-01-01")'),
                  c("dplyr", "lme4"))
  expect_setequal(pkgs_of(c('groundhog.library(c("dplyr",', '  "lme4"), "2023-01-01")')),
                  c("dplyr", "lme4"))
  expect_setequal(pkgs_of(c('pkgs <- c("dplyr", "lme4")', 'groundhog.library(pkgs, day)')),
                  c("dplyr", "lme4"))
  expect_equal(pkgs_of('groundhog.library("crsh/papaja", "2023-01-01")'), "papaja")
  expect_equal(pkgs_of('groundhog.library("crsh/papaja@v1", "2023-01-01")'), "papaja")
  # github:: names the source here, not a package called github
  expect_equal(pkgs_of("groundhog.library('github::crsh/papaja', '2023-01-01')"), "papaja")
  expect_equal(pkgs_of("x <- stats::sd(y)/2"), "stats")
  # a piece of code as the argument
  expect_setequal(pkgs_of(c('groundhog.library("', '  library(dplyr)', '  library(lme4)',
                            '", "2023-01-01")')), c("dplyr", "lme4"))
  expect_setequal(pkgs_of('groundhog.library("library(dplyr); library(lme4)", "2023-01-01")'),
                  c("dplyr", "lme4"))
  expect_length(pkgs_of('# groundhog.library("dplyr", "2023-01-01")'), 0)

  # the line of a call over several lines, and how it was found
  obs <- code_library_names(c("x <- 1", 'groundhog.library(c("dplyr",', '  "lme4"), d)'), "R")
  expect_equal(obs$source, c("groundhog", "groundhog"))
  expect_equal(obs$line, c(2L, 2L))

  # another function whose name ends in library or require is not library()
  expect_equal(pkgs_of(c("my.library(x)", "my.require(y)", "library(z)")), "z")
})

test_that(".code_pin_dates finds the date in the call or in a variable", {
  dates <- function(code) {
    d <- .code_pin_dates(code)
    paste(d$mechanism, d$date)
  }
  gh <- function(date) paste("groundhog", date)
  cp <- function(date) paste("checkpoint", date)

  expect_equal(dates('groundhog.library("dplyr", "2023-01-01")'), gh("2023-01-01"))
  expect_equal(dates('groundhog::groundhog.library("dplyr", "2023-01-01")'), gh("2023-01-01"))
  expect_equal(dates('groundhog.library(c("dplyr", "lme4"), "2023-01-01")'), gh("2023-01-01"))
  expect_equal(dates(c('groundhog.library("', 'library(a)', '", "2023-01-01")')), gh("2023-01-01"))
  expect_equal(dates(c('day <- "2023-01-01"', 'groundhog.library("dplyr", day)')), gh("2023-01-01"))
  expect_equal(dates(c('pkgs <- "a"', 'day = "2023-01-01"', 'groundhog.library(pkgs, day)')),
               gh("2023-01-01"))
  expect_equal(dates(c('d <- "2023-01-01"', 'groundhog.library(date = d, pkg = "a")')),
               gh("2023-01-01"))
  expect_setequal(dates(c('groundhog.library("a", "2023-01-01")', 'groundhog.library("b", "2023-02-02")')),
                  gh(c("2023-01-01", "2023-02-02")))

  expect_equal(dates('checkpoint("2022-01-01")'), cp("2022-01-01"))
  expect_equal(dates('checkpoint::checkpoint("2022-01-01", r_version = "4.0.0")'), cp("2022-01-01"))
  expect_equal(dates('checkpoint(snapshot_date = "2022-01-01")'), cp("2022-01-01"))
  expect_equal(dates('checkpoint(snapshotDate = "2022-01-01")'), cp("2022-01-01"))
  expect_equal(dates(c('s <- "2022-01-01"', 'create_checkpoint(s)')), cp("2022-01-01"))
  expect_equal(dates('use_checkpoint("2022-01-01")'), cp("2022-01-01"))

  # no date
  expect_length(dates('# groundhog.library("dplyr", "2023-01-01")'), 0)
  expect_length(dates('groundhog.library("dplyr", "2023-13-45")'), 0)
  expect_length(dates('groundhog.library("dplyr", day)'), 0)
  expect_length(dates('groundhog.library("dplyr", as.character(Sys.Date()))'), 0)
  expect_length(dates(c("library(groundhog)", "library(checkpoint)")), 0)
  expect_length(dates(NULL), 0)

  # a line that is not valid UTF-8 does not stop the search
  expect_equal(dates(c('x <- "caf\xe9"', 'groundhog.library("a", "2023-01-01")')), gh("2023-01-01"))
})

test_that(".code_session_packages reads a sessionInfo() record", {
  obs <- .code_session_packages(c(
    "R version 3.6.1 (2019-07-05)", "", "attached base packages:",
    "[1] stats     graphics  grDevices", "", "other attached packages:",
    " [1] lme4_1.1-21     Matrix_1.2-17", " [3] ggplot2_3.2.1  ", "",
    "loaded via a namespace (and not attached):",
    " [1] Rcpp_1.0.2  compiler_3.6.1 nlme_3.1-140", " [4] data.table_1.12.8"))
  expect_equal(obs$package, c("lme4", "Matrix", "ggplot2", "Rcpp", "nlme", "data.table"))
  expect_equal(obs$version, c("1.1-21", "1.2-17", "3.2.1", "1.0.2", "3.1-140", "1.12.8"))
  expect_true(all(is.na(obs$source)))
  expect_true(all(is.na(obs$remote)))

  # as a knitted document or a Markdown quote shows it
  for (prefix in c("## ", "> ")) {
    obs <- .code_session_packages(paste0(prefix, c("other attached packages:", "[1] lme4_1.1-21")))
    expect_equal(obs$package, "lme4")
  }

  # this R session's own record, not one typed here
  obs <- .code_session_packages(utils::capture.output(print(utils::sessionInfo())))
  expect_true("metacheck" %in% obs$package)
  expect_false("stats" %in% obs$package)
})

test_that(".code_session_packages reads a session_info() table", {
  obs <- .code_session_packages(c(
    "- Packages ----", " ! package    * version  date (UTC) lib source",
    "   assertthat   0.2.1    2019-03-21 [1] CRAN (R 4.3.0)",
    "   dplyr      * 1.1.3    2023-09-03 [1] CRAN (R 4.3.1)",
    " P papaja     * 0.1.2    2023-10-11 [1] Github (crsh/papaja@c0a7bb8)",
    " V limma        3.56.2   2023-06-04 [1] Bioconductor",
    "   mypkg        0.0.1    2023-06-04 [1] local",
    "   R6           2.5.1    2021-08-19 [2] RSPM (R 4.3.0)",
    "   stats      * 4.3.1    2023-06-16 [2] local", "",
    " [1] C:/some/lib"))
  expect_equal(obs$package, c("assertthat", "dplyr", "papaja", "limma", "mypkg", "R6"))
  expect_equal(obs$version, c("0.2.1", "1.1.3", "0.1.2", "3.56.2", "0.0.1", "2.5.1"))
  expect_equal(obs$source, c("Repository", "Repository", "GitHub", "Bioconductor", "Local", "Repository"))
  expect_equal(obs$remote[3], "crsh/papaja@c0a7bb8")
  expect_true(all(is.na(obs$remote[-3])))

  # the older form, with no library column
  obs <- .code_session_packages(c(
    " package * version date       source", " MASS      7.3-51  2019-03-31 CRAN (R 3.6.0)",
    " tidyr   * 1.0.0   2019-09-11 Github (tidyverse/tidyr@abc123)"))
  expect_equal(obs$package, c("MASS", "tidyr"))
  expect_equal(obs$source, c("Repository", "GitHub"))
  expect_equal(obs$remote[2], "tidyverse/tidyr@abc123")
})

test_that(".code_session_packages finds nothing in other text", {
  expect_equal(nrow(.code_session_packages(c(
    "We used R version 4.3.1 and the package lme4_1.1 for this.", "package version date"))), 0)
  expect_equal(nrow(.code_session_packages(character(0))), 0)
  # a line that is not valid UTF-8 does not stop the search
  expect_equal(.code_session_packages(c("caf\xe9", "other attached packages:", "[1] lme4_1.1-21"))$package,
               "lme4")
})

test_that("code_library_names does not report a package-list variable as a package", {
  # Issue #421: a paper's own install boilerplate loads its packages through a
  # variable, and the variable's name ("req.lib", "list.packages", "x", ...)
  # was reported, installed, and listed as a failed dependency.
  pkgs_of <- function(code) unique(code_library_names(code, "R")$package)

  # The issue's example: the loop variable resolves to the literal list,
  # which may span several lines.
  expect_setequal(pkgs_of(c(
    'list.packages <- c("activity", "bbmle",',
    '                   "Distance")',
    'for (req.lib in list.packages) {',
    '  if (!require(req.lib, character.only = TRUE)) {',
    '    install.packages(req.lib)',
    '    library(req.lib, character.only = TRUE)',
    '  }',
    '}')), c("activity", "bbmle", "Distance"))

  # The common new.packages idiom: indexing a list variable resolves to it;
  # a derived variable (new.packages) cannot be resolved and is dropped.
  expect_setequal(pkgs_of(c(
    'list.of.packages <- c("ggplot2", "Rcpp")',
    'new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]',
    'if (length(new.packages)) install.packages(new.packages)',
    'install.packages(list.of.packages[!list.of.packages %in% rownames(installed.packages())])',
    'lapply(list.of.packages, require, character.only = TRUE)')),
    c("ggplot2", "Rcpp"))

  # Variables with no literal value in the file are dropped, never reported.
  for (v in c("x", "pkg", "packages.toinstall", "not_installed", "dependencies", "arma")) {
    expect_equal(pkgs_of(c(sprintf("install.packages(%s)", v),
                           sprintf("library(%s, character.only = TRUE)", v),
                           sprintf("requireNamespace(%s, quietly = TRUE)", v))),
                 character(0), info = v)
  }
  # A vector that is not purely literal is not resolved either.
  expect_setequal(pkgs_of(c('pk <- c("zoo", other)', 'install.packages(pk)')),
               character(0))

  # sapply(FUN = ...) and pacman's char = / character.only forms.
  expect_setequal(pkgs_of(c('pkgs = c("psych", "car")',
                         'sapply(pkgs, FUN = library, character.only = TRUE)')),
               c("car", "psych"))
  expect_setequal(pkgs_of(c('p <- c("lme4", "car")', 'pacman::p_load(char = p)')),
               c("car", "lme4", "pacman"))

  # Literal names keep working, including with further arguments.
  expect_setequal(pkgs_of(c('library(dplyr); library("tidyr")',
                         'install.packages("afex", dependencies = TRUE)',
                         'BiocManager::install("edgeR")')),
               c("afex", "BiocManager", "dplyr", "edgeR", "tidyr"))
})


test_that("code_library_names Python", {
  code_text <- c(
    "import numpy",
    "import pandas as pd",
    "import os, sys",
    "from sklearn.linear_model import LinearRegression",
    "from . import local",          # relative import: no package name
    "import matplotlib  # inline comment"
  )
  obs <- code_library_names(code_text, "Python")
  # top-level package names only; submodules and aliases stripped; the relative
  # `from . import` yields nothing.
  expect_setequal(obs$package,
                  c("numpy", "pandas", "os", "sys", "sklearn", "matplotlib"))
  expect_true(all(obs$source == "import"))
})


test_that("code_library_names other languages return empty", {
  empty <- data.frame(package = character(0), source = character(0),
                      line = integer(0))
  for (lang in c("SPSS", "SAS", "Stata")) {
    expect_equal(code_library_names("anything", lang), empty)
  }
  # no imports at all -> empty (same columns)
  expect_equal(code_library_names(c("a <- 1", "b <- 2"), "R"), empty)
})


test_that("code_packages unions comma-joined strings", {
  # sorted, de-duplicated union; blanks and NA ignored
  expect_equal(
    code_packages(c("dplyr, ggplot2", "", NA, "dplyr, tidyr")),
    c("dplyr", "ggplot2", "tidyr")
  )
  # accepts a code_check-style table (packages column)
  tbl <- data.frame(packages = c("readr, dplyr", ""))
  expect_equal(code_packages(tbl), c("dplyr", "readr"))
  # empty input -> empty character
  expect_equal(code_packages(character(0)), character(0))
  expect_equal(code_packages(c("", NA)), character(0))
})


test_that("code_lang detects Python and notebook kernels", {
  expect_equal(code_lang("analysis.py"), "Python")
  expect_equal(code_lang("ANALYSIS.PY"), "Python")

  # A .ipynb's language comes from the kernel declared INSIDE the file, not
  # from the extension: Jupyter runs Python, R, Julia and more.
  py_nb <- test_path("fixtures", "notebooks", "notebook_python.ipynb")
  r_nb  <- test_path("fixtures", "notebooks", "notebook_r.ipynb")
  expect_equal(code_lang(py_nb) |> unname(), "Python")
  expect_equal(code_lang(r_nb) |> unname(), "R")

  # A not-yet-downloaded notebook is provisionally Python (the more common
  # kernel), so code_check still downloads it, rather than erroring.
  expect_equal(code_lang("no_such_notebook.ipynb") |> unname(), "Python")
})

test_that("a notebook or Quarto document in another language is not labelled Python or R (#480)", {
  d <- withr::local_tempdir()
  nb <- function(file, metadata) {
    p <- file.path(d, file)
    jsonlite::write_json(list(cells = list(), metadata = metadata,
                              nbformat = 4, nbformat_minor = 5), p, auto_unbox = TRUE)
    p
  }
  expect_equal(unname(code_lang(nb("julia.ipynb", list(kernelspec = list(language = "julia", name = "julia-1.9"))))), "Julia")
  expect_equal(unname(code_lang(nb("scala.ipynb", list(language_info = list(name = "scala"))))), "Scala")
  expect_equal(unname(code_lang(nb("ir.ipynb", list(kernelspec = list(name = "ir"))))), "R")
  expect_equal(unname(code_lang(nb("py3.ipynb", list(kernelspec = list(name = "python3"))))), "Python")
  # a downloaded notebook that declares no language gets none
  expect_true(is.na(code_lang(nb("none.ipynb", list()))))

  qmd <- function(file, lines) { p <- file.path(d, file); writeLines(lines, p); p }
  expect_equal(unname(code_lang(qmd("j1.qmd", c("---", "jupyter: julia-1.9", "---", "x")))), "Julia")
  expect_equal(unname(code_lang(qmd("j2.qmd", c("---", "title: a", "---", "```{julia}", "x = 1", "```")))), "Julia")
  expect_equal(unname(code_lang(qmd("p.qmd", c("---", "jupyter: python3", "---")))), "Python")
  expect_equal(unname(code_lang(qmd("r.qmd", c("---", "title: a", "---", "```{r}", "x <- 1", "```")))), "R")

  expect_equal(.kernel_language("IPython"), "Python")
  expect_equal(.kernel_language("octave"), "MATLAB")
  expect_true(is.na(.kernel_language("")))
})


test_that("code_remove_comments Python", {
  code_text <- c(
    "# whole-line comment",
    "",
    "import os  # trailing comment",
    "x = 1"
  )
  expect_equal(code_remove_comments(code_text, "Python"),
               c("import os  ", "x = 1"))

  # A "#" INSIDE a string literal is not a comment. Getting this wrong loses
  # real file references (see .code_strip_inline_comment).
  expect_equal(code_remove_comments("df = pd.read_csv('a#b.csv')", "Python"),
               "df = pd.read_csv('a#b.csv')")
  expect_equal(code_remove_comments("u = 'http://x.org/#frag'  # note", "Python"),
               "u = 'http://x.org/#frag'  ")

  # Docstrings / triple-quoted strings are deliberately NOT stripped.
  expect_equal(code_remove_comments(c('"""doc"""', "y = 2"), "Python"),
               c('"""doc"""', "y = 2"))

  expect_equal(code_remove_comments(character(0), "Python"), character(0))
})


test_that("code_remove_comments keeps markers inside strings (Stata, MATLAB)", {
  # Stata: "//" inside a quoted path is part of the path, not a comment. Before
  # this was handled, these lines truncated to "https:" / "C:" and those junk
  # fragments were reported to authors as missing files.
  stata <- c(
    'import delimited using "https://example.org//files/x.csv"',
    'use "C://project//data//w1.dta"',
    'summarize x  // a real comment'
  )
  expect_equal(code_remove_comments(stata, "Stata"),
               c('import delimited using "https://example.org//files/x.csv"',
                 'use "C://project//data//w1.dta"',
                 'summarize x  '))
  expect_equal(code_file_refs(code_remove_comments(stata, "Stata"), "Stata"),
               c("https://example.org//files/x.csv", "C://project//data//w1.dta"))

  # MATLAB: "%" inside a string (a URL-encoded filename) is not a comment.
  matlab <- c("T = readtable('data/a%20b.csv');", "x = 1;  % real comment")
  expect_equal(code_remove_comments(matlab, "MATLAB"),
               c("T = readtable('data/a%20b.csv');", "x = 1;  "))
  expect_equal(code_file_refs(code_remove_comments(matlab, "MATLAB"), "MATLAB"),
               "data/a%20b.csv")
})


test_that("code_library_lines Python", {
  code_text <- c(
    "import os",
    "from scipy import stats",
    "x = 1",
    "import pandas"
  )
  obs <- code_library_lines(code_text, "Python")
  expect_equal(obs$line, c(1L, 2L, 4L))

  # "import" as a method name or inside a string is not an import statement.
  expect_equal(nrow(code_library_lines("df.import_csv('a.csv')", "Python")), 0L)

  # import x, y and import x as y are both real, single-line import shapes.
  obs2 <- code_library_lines(c("import sys, re", "import numpy as np"), "Python")
  expect_equal(obs2$line, c(1L, 2L))

  # Docstring prose starting with "from "/"import " must NOT be matched as an
  # import line: code_remove_comments()'s Python branch deliberately never
  # strips triple-quoted strings (real docstrings would be destroyed too), so
  # this text reaches code_library_lines() unchanged. Confirmed against a real
  # corpus file where a docstring sentence "from the generated model and api
  # descriptions." was matched by the old bare-keyword anchor, inflating the
  # gap to the file's real (contiguous) imports past the "scattered"
  # threshold -- see code_check validation.
  docstring_code <- c(
    '"""',
    "Some docstring text",
    "        from the generated model and api descriptions.",
    '"""',
    "import os",
    "import sys"
  )
  obs3 <- code_library_lines(docstring_code, "Python")
  expect_equal(obs3$line, c(5L, 6L))
})


test_that("code_line_stats Python", {
  # Note: code_line_stats() splits on "\n+", so a blank line between entries is
  # dropped before counting -- total_lines is 3, not 4. That is pre-existing
  # behaviour shared by every language, not specific to Python (the identical R
  # input gives the identical counts).
  #
  # https://github.com/scienceverse/metacheck/issues/261 -- the trailing-
  # commented line ("y = 2  # trailing") is both a code line AND a comment
  # line: it still contributes real code (code_lines), but is no longer
  # invisible to comment_lines just because it also counts as code.
  code_text <- c("# comment", "", "x = 1", "y = 2  # trailing")
  obs <- code_line_stats(code_text, "Python")
  expect_equal(obs$total_lines, 3)
  expect_equal(obs$code_lines, 2)
  expect_equal(obs$comment_lines, 2)

  # Python and R agree on the same shape of input.
  r_obs <- code_line_stats(c("# comment", "", "x <- 1", "y <- 2  # trailing"), "R")
  expect_equal(obs$code_lines, r_obs$code_lines)
  expect_equal(obs$comment_lines, r_obs$comment_lines)

  # has_docstring: NA for languages other than Python, and TRUE/FALSE for
  # Python depending on whether a complete triple-quoted block is present.
  # A file documented ONLY via a docstring (no "#" comments) still reports 0%
  # comments -- has_docstring is the separate, additive signal for that case
  # (see .code_has_docstring()'s own documentation).
  expect_true(is.na(r_obs$has_docstring))
  expect_false(obs$has_docstring)   # no docstring in the code_text above

  docstring_code <- c('"""Module docstring."""', "x = 1")
  ds_obs <- code_line_stats(docstring_code, "Python")
  expect_true(ds_obs$has_docstring)
  expect_equal(ds_obs$percent_comments, 0)   # a docstring is not a "#" comment
})


test_that("code_file_refs Python", {
  code_text <- c(
    "df = pd.read_csv('data/trials.csv')",
    "arr = np.loadtxt('vals.txt')",
    "m = scipy.io.loadmat('subj01.mat')",
    "with open('notes.txt') as f: pass",
    "d2 = pd.read_excel('sheets/x.xlsx')"
  )
  expect_setequal(code_file_refs(code_text, "Python"),
                  c("data/trials.csv", "vals.txt", "subj01.mat",
                    "notes.txt", "sheets/x.xlsx"))

  # An import is NEVER a file reference (the mirror of the rio import() guard).
  expect_equal(code_file_refs(c("import numpy", "from a.b import c"), "Python"),
               character(0))
})


test_that("code_file_refs leaves out files the code writes", {
  # open() in a mode that creates the file is an output, not a missing input;
  # r+ needs the file to exist, so it is still an input
  py <- c('rows = open("data.csv").read()', 'open("out.txt", "w").write("1")',
          'with open("log.txt", mode="a") as f: pass',
          'with open("in.txt") as f, open("res.txt", "w") as g: pass',
          'f = open("keep.txt", "r+")', 'g = open("new.bin", "xb")',
          'h = open(', '    "multi.txt",', '    "w")')
  expect_setequal(code_file_refs(py, "Python"), c("data.csv", "in.txt", "keep.txt"))
  # repro_file_io() asks for the writes, and sorts them itself
  expect_true(all(c("out.txt", "res.txt", "multi.txt") %in%
                    code_file_refs(py, "Python", include_writes = TRUE)))
  expect_length(.code_blank_write_opens(c(py, "", ""), "Python"), length(py) + 2)

  m <- c("d = load('in.mat');", "fid = fopen('results.txt', 'w');",
         "fid2 = fopen('raw.txt');", "fid3 = fopen(\"app.txt\", 'at');",
         "fid4 = fopen('upd.txt', 'r+');")
  expect_setequal(code_file_refs(m, "MATLAB"), c("in.mat", "raw.txt", "upd.txt"))

  # Mplus: FILE = under SAVEDATA: is the file the model writes
  mp <- c('TITLE: two files', 'DATA: FILE = "in.dat";', 'VARIABLE: NAMES = y1-y3;',
          'MODEL: f BY y1-y3;', 'SAVEDATA:', '  FILE = "fscores.dat";',
          '  SAVE = FSCORES;', 'OUTPUT: sampstat;')
  expect_equal(code_file_refs(mp, "Mplus"), "in.dat")
})


test_that("code_file_refs ignores capitals in SAS, SPSS and Mplus", {
  # these languages ignore capitals; only one way of writing was found
  expect_setequal(code_file_refs(c('DATA: FILE = "UP.dat";', 'data: file = "low.dat";'), "Mplus"),
                  c("UP.dat", "low.dat"))
  expect_setequal(code_file_refs(c("proc import datafile='low.csv' out=a;",
                                   "PROC IMPORT DATAFILE='UP.csv' OUT=b;",
                                   "INFILE bare_up.dat;"), "SAS"),
                  c("low.csv", "UP.csv", "bare_up.dat"))
  expect_setequal(code_file_refs(c("GET FILE='UP.sav'.", "get file='low.sav'.",
                                   "get data /type=txt /file=bare_low.txt"), "SPSS"),
                  c("UP.sav", "low.sav", "bare_low.txt"))
  # Stata does not: USE is not a command
  expect_equal(code_file_refs(c('use "a.dta"', 'USE "b.dta"'), "Stata"), "a.dta")
})


test_that("code_extract_py", {
  expect_true(is.function(metacheck::code_extract_py))
  expect_no_error(helplist <- help(code_extract_py, metacheck))

  expect_error(code_extract_py(NULL))

  file_path <- test_path("fixtures", "notebooks", "notebook_python.ipynb")
  obs <- code_extract_py(file_path)

  # Code cells only: the markdown cell's prose must not appear.
  expect_false(any(grepl("^# Analysis", obs)))
  # IPython magics and shell escapes are dropped (not statements in any kernel).
  expect_false(any(grepl("^%matplotlib", obs)))
  expect_false(any(grepl("^!pip", obs)))
  # A whole %%bash cell is not Python; %%time only wraps Python (#568).
  nb <- function(cells, language = "python") jsonlite::toJSON(list(
    metadata = list(kernelspec = list(language = language, name = language)),
    cells = lapply(cells, function(s) list(cell_type = "code", source = s))), auto_unbox = TRUE)
  expect_equal(code_extract_py(text = nb(list("%%bash\nls -l\nfor i in 1 2; do echo $i; done"))), character(0))
  expect_equal(code_extract_py(text = nb(list("%%time\nx = 1"))), c("x = 1", ""))
  # An indented command leaves its block valid.
  expect_equal(code_extract_py(text = nb(list("if a:\n    !pip install b"))), c("if a:", "    pass", ""))
  # Commands without a %, and what must stay.
  expect_equal(code_extract_py(text = nb(list("cd data\nnp.mean?\ncd = 3\nx = 1"))), c("cd = 3", "x = 1", ""))
  # In an R notebook these are code.
  expect_equal(code_extract_py(text = nb(list("d\n  %>% f()\n!x"), "R")), c("d", "  %>% f()", "!x", ""))
  # Real source survives, in document order.
  expect_true("import pandas as pd" %in% obs)
  expect_true("df = pd.read_csv('data/trials.csv')" %in% obs)
  # A cell that was never run still contributes its source.
  expect_true("x = 1  # never run" %in% obs)

  # save_path writes and returns the path
  save_path <- withr::local_tempfile(fileext = ".py")
  expect_equal(code_extract_py(file_path, save_path), save_path)
  expect_true(file.exists(save_path))

  # An R-kernel notebook extracts its R source the same way.
  r_nb <- test_path("fixtures", "notebooks", "notebook_r.ipynb")
  r_src <- code_extract_py(r_nb)
  expect_true("library(dplyr)" %in% r_src)
  # ...and is then checked as R, finding R packages.
  expect_setequal(code_library_names(r_src, "R")$package,
                  c("dplyr", "ggplot2"))

  # Not a notebook / no code cells -> empty, not an error.
  bad <- withr::local_tempfile(fileext = ".ipynb")
  writeLines('{"foo": 1}', bad)
  expect_equal(code_extract_py(bad), character(0))
})



test_that("code_*() functions handle an empty file without warnings or errors", {
  # Issue #425: an empty script (such as a Python package's __init__.py)
  # gave "Unknown or uninitialised column" warnings, and code_parse_r() and
  # code_line_stats() stopped with an error.
  x <- character(0)
  expect_no_warning(ap <- code_abs_path(x))
  expect_equal(nrow(ap), 0)
  expect_equal(names(ap), c("abs_path", "line"))
  expect_no_warning(sw <- code_setwd(x))
  expect_equal(nrow(sw), 0)
  expect_no_warning(ip <- code_install_packages(x))
  expect_equal(nrow(ip), 0)
  expect_no_error(ls <- code_line_stats(x, "Python"))
  expect_equal(ls$total_lines, 0)
  expect_true(is.na(ls$percent_comments))
  expect_no_error(pr <- code_parse_r(text = x))
  expect_false(pr$error)
})

test_that("code_lang falls back on odd Quarto front matter and notebooks (U68)", {
  # a front matter that is one value or a list, or a notebook that is not a
  # JSON object, stopped code_check ("$ operator is invalid")
  d <- withr::local_tempdir()
  w <- function(name, lines) { f <- file.path(d, name); writeLines(lines, f); f }
  expect_equal(code_lang(w("b.qmd", c("---", "just a title", "---", "```{python}", "x = 1", "```"))), "Python")
  expect_equal(code_lang(w("c.qmd", c("---", "- a", "- b", "---", "```{r}", "x <- 1", "```"))), "R")
  expect_true(is.na(code_lang(w("e.ipynb", "\"text\""))))
  expect_true(is.na(code_lang(w("f.ipynb", '{"metadata": "x", "cells": []}'))))
})

test_that("code_extract_r reads an .Rmd with a numeric YAML chunk label (U153)", {
  # knitr used the label 3 as a position: "subscript out of bounds", so the
  # file was skipped
  txt <- c("# Title", "", "```{r}", "#| label: 3", "x <- 1", "```")
  expect_true("x <- 1" %in% code_extract_r(text = txt))
})
