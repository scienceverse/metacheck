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
                    code_n = 0)
  expect_equal(mo$summary_table, exp)
  expect_match(mo$summary_text, "0")
  expect_match(mo$report, "0")
})


test_that("OSF no files", {
  # OSF but no R files

  module <- "code_check"
  paper <- test_paper(url = "https://osf.io/y6a34")
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "na")
  exp <- data.frame(paper_id = paper$paper_id,
                    code_n = 0)
  expect_equal(mo$summary_table, exp)
  expect_match(mo$summary_text, "0")
  expect_match(mo$report, "0")
}, "mock")

test_that("no code files", {
  module <- "code_check"
  paper <- test_paper(url = "https://osf.io/m4nbv")
  mo <- module_run(paper, module)

  exp <- data.frame(paper_id = paper$paper_id,
                    code_n = 0)
  expect_equal(mo$summary_table, exp)

  # JASP is listed alongside the analysed languages (see `listed_langs` in the
  # module): a .jasp is a binary bundle we count but do not parse. Mplus and
  # MATLAB were added to `listed_langs` alongside JASP/R/SAS/SPSS/Stata, and
  # Python (.py and .ipynb) after them.
  exp <- "We found 0 R, 0 Python, 0 SAS, 0 SPSS, 0 Stata, 0 Mplus, 0 MATLAB, and 0 JASP code files."
  expect_equal(mo$summary_text, exp)
  expect_equal(mo$report, exp)
}, "mock")

test_that("OSF", {
  skip_if_quick()

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
}, "mock")

test_that("all code files are checked (no per-repo cap)", {
  # There is no file_limit: every code file in a repo is checked.
  paper <- test_paper()
  local_path <- test_path("fixtures", "demo", "code") # has 25 files
  n_files <- list.files(local_path) |> length()
  mo <- module_run(paper, "code_check", local_path = local_path)
  expect_equal(nrow(mo$table), n_files)
  expect_equal(mo$summary_table$code_n, n_files)
  expect_equal(mo$summary_table$code_checked, n_files)   # all checked
}, "mock")


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
}, "mock")

test_that("downloaded files keep their analysis results (join regression)", {
  # Code files listed remotely (file_location = NA) are downloaded before the
  # analysis loop, which updates file_location in the checked copies. Results
  # were then joined back onto the original rows by every column — so every
  # downloaded file mismatched on file_location and got all-NA analysis
  # columns: the report claimed "N scripts loaded 0 files" and printed no
  # absolute paths (the collabra.102 case). Offline via a file:// URL.
  withr::local_options(
    metacheck.repo_cache.notified = TRUE,
    metacheck.repo_cache.dir = file.path(tempdir(), "mc-test-codecheck-cache"),
    metacheck.repo_cache.session_dir = file.path(tempdir(), "mc-test-codecheck-session"))

  src <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "# read the data",
    'dat <- read.csv("C:/Users/lisa/project/data.csv")',
    'other <- read.csv("not_in_repo.csv")'
  ), src)

  paper <- test_paper()
  # same shape as a repo_check table row (paper_id is required: the summary
  # step aggregates with .by = paper_id)
  files <- data.frame(
    paper_id = paper_id(paper),
    repo_name = "code-join-regression",
    repo_url = "https://example.org/code-join-regression",
    file_name = "analysis.R",
    file_path = "code/analysis.R",
    file_url = paste0("file:///", gsub("\\\\", "/", src)),
    file_size = file.info(src)$size,
    file_type = "code",
    file_location = NA_character_
  )
  # hand code_check a repo_check result via the module pipeline, so it takes
  # the download path instead of a local_path
  fake_repo <- list(
    module = "repo_check",
    table = files,
    summary_table = data.frame(paper_id = paper_id(paper)),
    paper = paper
  )
  class(fake_repo) <- "metacheck_module_output"

  mo <- module_run(fake_repo, "code_check", download = TRUE)

  row <- mo$table[mo$table$file_name == "analysis.R", ]
  expect_true(row$checked)
  expect_gte(row$code_abs_path, 1L)
  expect_match(row$absolute_paths, "lisa")
  expect_equal(row$loaded_files_missing, 2L)
  expect_match(row$loaded_files_missing_names, "not_in_repo.csv")
  expect_gte(mo$summary_table$code_abs_path, 1)
  expect_equal(mo$summary_table$code_missing_files, 2)
  # the report carries the actual paths and counts, not NA
  expect_true(any(grepl("lisa", mo$report, fixed = TRUE)))
  expect_false(any(grepl("loaded 0 files", mo$report, fixed = TRUE)))
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
  expect_equal(mo$summary_table$code_n, 0)
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

test_that("code_check records loaded packages per file and per paper", {
  local_path <- test_path("fixtures", "code_files")
  mo <- module_run(test_paper(), "code_check", local_path = local_path)

  # analysis.R loads dplyr + ggplot2; analysis_no_comments.R loads dplyr;
  # the Stata / helper files load nothing.
  by_name <- stats::setNames(mo$table$packages, mo$table$file_name)
  expect_equal(by_name[["analysis.R"]], "dplyr, ggplot2")
  expect_equal(by_name[["analysis_no_comments.R"]], "dplyr")
  expect_equal(by_name[["stata_latin1.do"]], "")

  n_by_name <- stats::setNames(mo$table$packages_n, mo$table$file_name)
  expect_equal(n_by_name[["analysis.R"]], 2L)
  expect_equal(n_by_name[["helper.R"]], 0L)

  # paper-level distinct union: dplyr + ggplot2 = 2
  expect_equal(mo$summary_table$code_packages_n, 2L)
  expect_match(mo$summary_text, "2 distinct packages")
  expect_match(paste(mo$report, collapse = "\n"), "ggplot2")
})

test_that("code_check flags no pinned environment as a reproducibility gap", {
  tmp <- withr::local_tempdir()
  writeLines(c("library(dplyr)", "x <- 1"), file.path(tmp, "analysis.R"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)

  expect_false(mo$summary_table$code_version_pinned)
  expect_match(mo$summary_text, "No pinned")
  expect_true(any(grepl("renv.lock", mo$report, fixed = TRUE)))
})

test_that("code_check detects a renv.lock and its R version + locked packages", {
  tmp <- withr::local_tempdir()
  writeLines(c("library(dplyr)", "x <- 1"), file.path(tmp, "analysis.R"))
  writeLines(jsonlite::toJSON(list(
    R = list(Version = "4.3.1",
            Repositories = list(list(Name = "CRAN", URL = "https://cran.rstudio.com"))),
    Packages = list(
      dplyr = list(Package = "dplyr", Version = "1.1.3",
                  Source = "Repository", Repository = "CRAN")
    )
  ), auto_unbox = TRUE, pretty = TRUE), file.path(tmp, "renv.lock"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)

  expect_true(mo$summary_table$code_version_pinned)
  expect_match(mo$summary_text, "pinned R/package environment")
  report_txt <- paste(mo$report, collapse = "\n")
  expect_match(report_txt, "renv.lock", fixed = TRUE)
  expect_match(report_txt, "4.3.1", fixed = TRUE)
  expect_match(report_txt, "dplyr", fixed = TRUE)
})

test_that("code_check detects a sessionInfo.txt dump by filename + content", {
  tmp <- withr::local_tempdir()
  writeLines(c("library(dplyr)", "x <- 1"), file.path(tmp, "analysis.R"))
  writeLines(c(
    "R version 4.4.2 (2024-10-31)",
    "Platform: aarch64-apple-darwin20",
    "Running under: macOS Sequoia 15.1"
  ), file.path(tmp, "sessionInfo.txt"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)

  expect_true(mo$summary_table$code_version_pinned)
  report_txt <- paste(mo$report, collapse = "\n")
  expect_match(report_txt, "sessionInfo", fixed = TRUE)
  expect_match(report_txt, "4.4.2", fixed = TRUE)
})

test_that("code_check requires an actual groundhog/checkpoint pin, not just library()", {
  tmp <- withr::local_tempdir()
  # library(groundhog) alone does not pin anything -- must not count.
  writeLines(c("library(groundhog)", "x <- 1"), file.path(tmp, "analysis.R"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)
  expect_false(mo$summary_table$code_version_pinned)
})

test_that("code_check detects a real groundhog.library() date-pin", {
  tmp <- withr::local_tempdir()
  writeLines(c(
    'groundhog.library("dplyr", "2022-01-01")',
    "x <- 1"
  ), file.path(tmp, "analysis.R"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)
  expect_true(mo$summary_table$code_version_pinned)
  expect_match(paste(mo$report, collapse = "\n"), "groundhog", fixed = TRUE)
})

test_that("code_check detects a real checkpoint() date-pin", {
  tmp <- withr::local_tempdir()
  writeLines(c(
    'checkpoint("2022-01-01")',
    "x <- 1"
  ), file.path(tmp, "analysis.R"))

  mo <- module_run(test_paper(), "code_check", local_path = tmp)
  expect_true(mo$summary_table$code_version_pinned)
  expect_match(paste(mo$report, collapse = "\n"), "checkpoint", fixed = TRUE)
})

test_that("code_check merges packages into a supplied manifest", {
  withr::local_options(metacheck.llm.use = FALSE)
  local_path <- test_path("fixtures", "code_files")
  mdir <- withr::local_tempdir()

  mo <- module_run(test_paper(), "code_check",
                   local_path = local_path, manifest = mdir)

  mf <- list.files(mdir, pattern = "\\.manifest\\.json$", full.names = TRUE)
  expect_length(mf, 1)
  m <- jsonlite::fromJSON(mf[[1]], simplifyVector = FALSE)
  expect_setequal(unlist(m$code$packages), c("dplyr", "ggplot2"))
})


test_that("code_check paper + local_path", {
  skip_if_quick()

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
}, "mock")


# code_check() + local_only ----

test_that("code_check local_only = TRUE ignores online repos, checks local only", {
  # paper has OSF link (629bx has 2 R files); local fixture has 5 code files
  local_path <- test_path("fixtures", "code_files")
  paper      <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "code_check", local_path = local_path, local_only = TRUE)

  # only the local fixture code files (analysis.R, analysis_no_comments.R, helper.R,
  # stata_latin1.do, stata_utf16.do) — OSF code files absent
  expect_equal(mo$summary_table$code_n, 5)
  expect_true("analysis.R"  %in% mo$table$file_name)
  expect_false("bad.R"       %in% mo$table$file_name)   # OSF file — should not appear
  expect_false("bad.Rmd"     %in% mo$table$file_name)   # OSF file — should not appear
}, "mock")

test_that("code_check local_only = TRUE with no local_path returns na", {
  # nothing to check: online skipped, no local path
  mo <- module_run(test_paper(), "code_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$code_n, 0)
})

test_that("code_check local_only = TRUE with online URLs but no local_path returns na", {
  # paper has OSF link; local_only suppresses it; no local_path
  paper <- test_paper(url = "https://osf.io/629bx")
  mo <- module_run(paper, "code_check", local_only = TRUE)

  expect_equal(mo$traffic_light, "na")
  expect_equal(mo$summary_table$code_n, 0)
}, "mock")

test_that("code_check local_only = FALSE is the same as the default", {
  local_path <- test_path("fixtures", "code_files")
  paper      <- test_paper()

  mo_default  <- module_run(paper, "code_check", local_path = local_path)
  mo_explicit <- module_run(paper, "code_check", local_path = local_path, local_only = FALSE)

  expect_equal(mo_default$summary_table, mo_explicit$summary_table)
  expect_equal(mo_default$traffic_light, mo_explicit$traffic_light)
})

test_that("code_check local_only = TRUE checks all local code files", {
  local_path <- test_path("fixtures", "demo", "code")
  n_files    <- length(list.files(local_path))
  mo <- module_run(test_paper(), "code_check",
                   local_path = local_path, local_only = TRUE)

  expect_equal(nrow(mo$table), n_files)
  expect_equal(mo$summary_table$code_checked, n_files)  # all checked, no cap
  expect_equal(mo$summary_table$code_n, n_files)
})


# parse errors ----

test_that("parse errors", {
  local_path <- test_path("fixtures", "parse-errors")
  paper <- test_paper()
  module <- "code_check"
  mo <- module_run(paper, module, local_path = local_path)

  exp <- data.frame(
    paper_id = rep(paper_id(paper), 8),
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
    # file_size and file_location are intentionally omitted: file_size's exact
    # byte count depends on the line endings of the checked-out fixtures (LF vs
    # CRLF), which differs by platform/git config, and file_location is an
    # absolute path that depends on where the repo is checked out. Both are
    # asserted separately below in an environment-independent way rather than
    # matched against hard-coded values.
    # file_path: each fixture file sits directly in local_path with no
    # subdirectory, so it is identical to file_name here.
    file_path = c(
      "error-ok.qmd", "error.R", "error.Rmd", "error.qmd",
      "knit-error.Rmd", "ok.R", "ok.Rmd", "ok.qmd"
    ),
    file_type = rep("code", 8),
    # data_type / doc_role / group come from the data_check classification
    # layer that repo_check now carries through (data_classify_files() and the
    # study-grouping pass): every fixture here is a plain code file in a single
    # unnamed study, so the type is "code", there is no documentation role, and
    # all rows share one group.
    data_type = rep("code", 8),
    doc_role = rep(NA_character_, 8),
    group = rep("ex1", 8),
    language = rep("R", 8),
    checked = rep(TRUE, 8),
    parse_error = rep(c(TRUE, FALSE), c(4, 4)),
    parse_error_msg = c("line:5:1: unexpected symbol\n4: \n5: a\n   ^",
                        "line:4:1: unexpected symbol\n3: \n4: a\n   ^",
                        "line:4:1: unexpected symbol\n3: \n4: a\n   ^",
                        "line:4:1: unexpected symbol\n3: \n4: a\n   ^",
                        NA, NA, NA, NA),
    code_abs_path = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L),
    absolute_paths = c("", "", "", "/User/lisa/file.csv", "", "", "", ""),
    # none of the fixture files call setwd()
    code_setwd = rep(0L, 8),
    setwd_calls = rep("", 8),
    library_lines = c(1L, 1L, 1L, 1L, 0L, 3L, 0L, 0L),
    library_max_between = c(NA, NA, NA, NA, NA, 5L, NA, NA),
    # packages/packages_n are alphabetical by file_name (see arrange() below):
    # error-ok.qmd, error.R, error.Rmd, error.qmd each load dplyr (the malformed
    # `library(dplyr` still yields the name); knit-error.Rmd/ok.Rmd/ok.qmd load
    # none; ok.R loads dplyr + tidyr.
    packages_n = c(1L, 1L, 1L, 1L, 0L, 2L, 0L, 0L),
    packages = c("dplyr", "dplyr", "dplyr", "dplyr", "",
                 "dplyr, tidyr", "", ""),
    comment_lines = c(1L, 1L, 1L, 3L, 1L, 2L, 4L, 4L),
    code_lines = c(4L, 2L, 2L, 3L, 1L, 7L, 1L, 1L),
    percentage_comment = c(0.2, 1/3, 1/3, 0.5, 0.5, 2/9, 0.8, 0.8) ,
    loaded_files_missing = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L),
    loaded_files_missing_names = c("", "", "", "file.csv", "", "", "", "")
  ) |> dplyr::arrange(file_name)
  obs <- dplyr::arrange(mo$table, file_name)

  # compare every column except file_size (line-ending / OS dependent) and
  # file_location (an absolute path that depends on where the repo checkout
  # lives)
  for (nm in setdiff(names(obs), c("file_size", "file_location"))) {
    expect_equal(obs[[nm]], exp[[nm]])
  }

  # file_size: assert it is present and positive for every file, without
  # depending on the exact byte count (which varies with LF vs CRLF endings)
  expect_true(all(obs$file_size > 0))
  expect_equal(length(obs$file_size), 8)

  # file_location: each fixture file was read locally, so every row should
  # resolve to an existing path on disk (the absolute value itself depends on
  # the checkout location, so it is not matched against a hard-coded string)
  expect_true(all(file.exists(obs$file_location)))

  # summary table
  exp <- data.frame(
    paper_id = paper_id(paper),
    code_n = 8,
    code_checked = 8,
    code_abs_path = 1,
    code_setwd = 0L,
    code_missing_files = 1,
    code_min_comments = 0.2,
    code_parse_errors = 4,
    # distinct packages across all 8 files: dplyr + tidyr (from ok.R)
    code_packages_n = 2L,
    # none of these fixtures pin an R/package environment (no renv.lock,
    # sessionInfo.txt, or groundhog/checkpoint date-pin call)
    code_version_pinned = FALSE
  )
  expect_equal(mo$summary_table, exp)
})


