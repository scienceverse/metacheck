# Unit tests for the STATIC helper functions in R/reproducibility_check.R —
# the functions the "reproducibility_check" module (inst/modules/
# reproducibility_check.R) is built from. Module-level (static + execute)
# tests live in test-module-reproducibility_check.R.

# repro_dependencies() ----

test_that("repro_dependencies finds library()/require() calls", {
  code <- c(
    "library(dplyr)",
    "require(ggplot2)"
  )
  deps <- repro_dependencies(code)
  expect_setequal(deps$package, c("dplyr", "ggplot2"))
  expect_true(all(deps$source == "cran"))
  expect_true(all(!deps$base))
})

test_that("repro_dependencies also picks up a pkg::fn() namespace reference", {
  # a namespace-qualified call is a real dependency even with no library()
  deps <- repro_dependencies("x <- stats::sd(1:10)")
  expect_equal(deps$package, "stats")
  expect_true(deps$base)
})

test_that("repro_dependencies tags base/recommended packages", {
  deps <- repro_dependencies(c("library(stats)", "library(dplyr)"))
  expect_equal(deps$base[deps$package == "stats"], TRUE)
  expect_equal(deps$base[deps$package == "dplyr"], FALSE)
  expect_equal(deps$source[deps$package == "stats"], "base")
})

test_that("repro_dependencies tags a GitHub source and keeps its ref", {
  # the package must ALSO be detected as a dependency in its own right (via
  # library()/a namespace call) for the github-source retag to have a row to
  # apply to — install_github()'s own string argument alone does not add one
  code <- c("library(ggplot2)",
           "remotes::install_github('tidyverse/ggplot2@v3.4.0')")
  deps <- repro_dependencies(code)
  gg <- deps[deps$package == "ggplot2", ]
  expect_equal(gg$source, "github")
  expect_equal(gg$ref, "tidyverse/ggplot2@v3.4.0")
})

test_that("repro_dependencies tags a URL source", {
  code <- c("library(pkgname)",
           'install.packages("https://example.org/src/contrib/pkgname_1.0.tar.gz")')
  deps <- repro_dependencies(code)
  pk <- deps[deps$package == "pkgname", ]
  expect_equal(pk$source, "url")
})

test_that("repro_dependencies pools a list of files, preferring github over cran for the same package", {
  # both lines in the SAME file: the github retag only applies within one
  # file's own text (each list element is pooled as a whole, then
  # de-duplicated across files) — a library() and its install_github() must
  # be in the same element for the retag to find the row to relabel.
  code_list <- list(
    c("library(ggplot2)", "remotes::install_github('tidyverse/ggplot2')")
  )
  deps <- repro_dependencies(code_list)
  gg <- deps[deps$package == "ggplot2", ]
  expect_equal(gg$source, "github")
})

test_that("repro_dependencies returns an empty frame for non-R / empty input", {
  empty_cols <- c("package", "source", "ref", "base")
  expect_equal(names(repro_dependencies(NULL)), empty_cols)
  expect_equal(nrow(repro_dependencies(NULL)), 0)
  expect_equal(nrow(repro_dependencies("library(dplyr)", lang = "Python")), 0)
  expect_equal(nrow(repro_dependencies(character(0))), 0)
})


# repro_rewrite_paths() ----

test_that("repro_rewrite_paths rewrites a single unambiguous match", {
  plan <- data.frame(
    file_name = "demographics.csv",
    target_path = "study-ex1/data/study-demographics_data.csv",
    current_path = "ex1/demographics.csv"
  )
  code <- 'd <- read.csv("data/demographics.csv")'
  out <- repro_rewrite_paths(code, "ex1/analysis.R", plan)
  expect_equal(nrow(out), 1)
  expect_true(out$matched)
  expect_false(out$ambiguous)
  expect_equal(out$target, "study-ex1/data/study-demographics_data.csv")
})

test_that("repro_rewrite_paths leaves an unmatched reference unmatched, not guessed", {
  plan <- data.frame(file_name = "scores.csv", target_path = "study-ex1/data/scores_data.csv")
  code <- 'd <- read.csv("data/not_in_plan.csv")'
  out <- repro_rewrite_paths(code, "ex1/analysis.R", plan)
  expect_equal(nrow(out), 1)
  expect_false(out$matched)
  expect_true(is.na(out$target))
})

test_that("repro_rewrite_paths disambiguates same-basename files by study group", {
  plan <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    target_path = c("study-ex1/data/study-demographics_data.csv",
                    "study-ex2/data/study-demographics_data.csv"),
    current_path = c("ex1/demographics.csv", "ex2/demographics.csv")
  )
  code <- 'd <- read.csv("demographics.csv")'
  out <- repro_rewrite_paths(code, "ex2/analysis.R", plan)
  expect_equal(nrow(out), 1)
  expect_true(out$matched)
  expect_false(out$ambiguous)
  expect_equal(out$target, "study-ex2/data/study-demographics_data.csv")
})

test_that("repro_rewrite_paths flags a still-ambiguous reference rather than guessing", {
  # two candidates, neither the script's own study nor the reference itself
  # names a group that disambiguates
  plan <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    target_path = c("study-ex1/data/study-demographics_data.csv",
                    "study-ex2/data/study-demographics_data.csv"),
    current_path = c("ex1/demographics.csv", "ex2/demographics.csv")
  )
  code <- 'd <- read.csv("demographics.csv")'
  out <- repro_rewrite_paths(code, "shared/analysis.R", plan)
  expect_equal(nrow(out), 1)
  expect_true(out$matched)
  expect_true(out$ambiguous)
  expect_true(is.na(out$target))
})

test_that("repro_rewrite_paths does not flag ambiguity when every candidate resolves to the same target", {
  # same file listed twice in the plan (e.g. referenced from two places) -
  # not real ambiguity
  plan <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    target_path = c("study-ex1/data/study-demographics_data.csv",
                    "study-ex1/data/study-demographics_data.csv")
  )
  code <- 'd <- read.csv("demographics.csv")'
  out <- repro_rewrite_paths(code, "ex1/analysis.R", plan)
  expect_equal(nrow(out), 1)
  expect_true(out$matched)
  expect_false(out$ambiguous)
})

test_that("repro_rewrite_paths uses original_target for a reference naming the pre-conversion extension", {
  # plan$file_name is the file's ORIGINAL basename (what psychds_check's
  # structure_df$file_name carries for a converted row — see
  # inst/modules/psychds_check.R's plan_table build), and basename matching
  # (a few lines up in repro_rewrite_paths) is keyed on THAT, never on
  # target_path's basename — so every reference that resolves to this plan
  # row does so via the ORIGINAL name, and ref_ext (read off that same
  # matched basename) is therefore always the original's own extension too.
  # NOTE: this means pick_target()'s documented "a reference that already
  # names the CSV form still rewrites to target_path" branch (this
  # function's own roxygen) could not be reached by basename matching in any
  # scenario tried here — flagged, not silently asserted either way.
  plan <- data.frame(
    file_name = "math.dta",
    target_path = "study-ex1/data/study-math_data.csv",
    original_target = "study-ex1/data/math.dta"
  )
  out_dta <- repro_rewrite_paths('d <- haven::read_dta("math.dta")', "ex1/analysis.R", plan)
  expect_equal(out_dta$target, "study-ex1/data/math.dta")
})

test_that("repro_rewrite_paths rewrites a sprintf()-built path as a whole call", {
  plan <- data.frame(file_name = "exp1_data.csv", target_path = "study-ex1/data/exp1_data.csv")
  code <- c('wd <- "exp1"', 'd <- read.csv(sprintf("%s_data.csv", wd))')
  out <- repro_rewrite_paths(code, "ex1/analysis.R", plan)
  expect_equal(nrow(out), 1)
  expect_true(out$is_call)
  expect_true(out$matched)
  expect_equal(out$target, "study-ex1/data/exp1_data.csv")
  expect_match(out$ref, "sprintf", fixed = TRUE)
})

test_that("repro_rewrite_paths returns an empty frame for non-R code or no plan", {
  empty_cols <- c("ref", "basename", "matched", "target", "ambiguous",
                  "n_candidates", "is_call")
  plan <- data.frame(file_name = "a.csv", target_path = "data/a.csv")
  expect_equal(names(repro_rewrite_paths('read.csv("a.csv")', "x.py", plan, lang = "Python")),
              empty_cols)
  expect_equal(nrow(repro_rewrite_paths('read.csv("a.csv")', "x.py", plan, lang = "Python")), 0)
  expect_equal(nrow(repro_rewrite_paths('read.csv("a.csv")', "x.R", NULL)), 0)
})

test_that("repro_rewrite_paths collapses byte-identical mirror duplicates before comparing groups", {
  # Two plan rows are the SAME physical file (identical content) but were
  # grouped into DIFFERENT (and so conflicting) study labels by an earlier
  # step; without collapsing this looks like real ambiguity even though there
  # is only one real file.
  tmp1 <- withr::local_tempfile(fileext = ".csv"); writeLines("a,b\n1,2", tmp1)
  tmp2 <- withr::local_tempfile(fileext = ".csv"); writeLines("a,b\n1,2", tmp1) # same content written to a second path
  writeLines(readLines(tmp1), tmp2)

  structure_df <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    file_location = c(tmp1, tmp2)
  )
  plan <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    target_path = c("study-ex1/data/study-demographics_data.csv",
                    "study-ex3/data/study-demographics_data.csv")
  )
  code <- 'd <- read.csv("demographics.csv")'
  out <- repro_rewrite_paths(code, "ex1/analysis.R", plan, structure_df = structure_df)
  expect_equal(nrow(out), 1)
  expect_true(out$matched)
  expect_false(out$ambiguous)
})


# repro_run_order() ----

test_that("repro_run_order orders by read-after-write dependency", {
  files <- data.frame(file_name = c("analysis.R", "prep.R"))
  files$reads   <- list("clean.csv", character(0))
  files$writes  <- list(character(0), "clean.csv")
  files$sources <- list(character(0), character(0))
  out <- repro_run_order(files)
  ord <- stats::setNames(out$order, out$file_name)
  expect_lt(ord[["prep.R"]], ord[["analysis.R"]])
  # order_basis reflects whether THAT file had an incoming edge (something
  # had to run before IT) — analysis.R depends on prep.R, so analysis.R (not
  # prep.R, which has no predecessor of its own) is the "dependency" row.
  expect_equal(out$order_basis[out$file_name == "analysis.R"], "dependency")
  expect_equal(out$depends_on[out$file_name == "analysis.R"], "prep.R")
})

test_that("repro_run_order orders by source() edges", {
  files <- data.frame(file_name = c("main.R", "helper.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list("helper.R", character(0))
  out <- repro_run_order(files)
  ord <- stats::setNames(out$order, out$file_name)
  expect_lt(ord[["helper.R"]], ord[["main.R"]])
})

test_that("repro_run_order falls back to numeric filename prefixes", {
  files <- data.frame(file_name = c("1_analysis.R", "0_prep.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list(character(0), character(0))
  out <- repro_run_order(files)
  ord <- stats::setNames(out$order, out$file_name)
  expect_lt(ord[["0_prep.R"]], ord[["1_analysis.R"]])
  expect_true(all(out$order_basis == "numeric"))
})

test_that("repro_run_order uses ALL digit runs in a filename, not just a leading prefix", {
  files <- data.frame(file_name = c("Exp2_1_import.R", "Exp1_02_preprocessing.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list(character(0), character(0))
  out <- repro_run_order(files)
  ord <- stats::setNames(out$order, out$file_name)
  expect_lt(ord[["Exp1_02_preprocessing.R"]], ord[["Exp2_1_import.R"]])
})

test_that("repro_run_order reports a dependency cycle and leaves it unordered", {
  files <- data.frame(file_name = c("a.R", "b.R"))
  files$reads   <- list("b_out.csv", "a_out.csv")
  files$writes  <- list("a_out.csv", "b_out.csv")
  files$sources <- list(character(0), character(0))
  out <- repro_run_order(files)
  expect_setequal(attr(out, "cycle"), c("a.R", "b.R"))
  expect_true(all(is.na(out$order)))
})

test_that("repro_run_order flags ambiguous when nothing distinguishes file order", {
  files <- data.frame(file_name = c("a.R", "b.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list(character(0), character(0))
  out <- repro_run_order(files)
  expect_true(attr(out, "ambiguous"))
})

test_that("repro_run_order is not ambiguous when only one file exists", {
  files <- data.frame(file_name = "solo.R")
  files$reads   <- list(character(0))
  files$writes  <- list(character(0))
  files$sources <- list(character(0))
  out <- repro_run_order(files)
  expect_false(attr(out, "ambiguous"))
  expect_equal(out$order, 1)
})

test_that("repro_run_order returns an empty frame for no files", {
  out <- repro_run_order(data.frame(file_name = character(0)))
  expect_equal(nrow(out), 0)
})

test_that("repro_run_order honours extra_edges (e.g. an undefined-variable correction)", {
  files <- data.frame(file_name = c("a.R", "b.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list(character(0), character(0))
  out <- repro_run_order(files, extra_edges = list(c("b.R", "a.R")))
  ord <- stats::setNames(out$order, out$file_name)
  expect_lt(ord[["b.R"]], ord[["a.R"]])
})


# repro_file_io() ----

test_that("repro_file_io classifies reads vs writes per occurrence", {
  code_list <- list(
    "prep.R" = c('dat <- read.csv("raw.csv")', 'write.csv(dat, "clean.csv")'),
    "analysis.R" = c('dat <- read.csv("clean.csv")')
  )
  io <- repro_file_io(code_list)
  prep <- io[io$file_name == "prep.R", ]
  expect_equal(prep$reads[[1]], "raw.csv")
  expect_equal(prep$writes[[1]], "clean.csv")
  analysis <- io[io$file_name == "analysis.R", ]
  expect_equal(analysis$reads[[1]], "clean.csv")
})

test_that("repro_file_io finds source() edges", {
  code_list <- list("main.R" = c('source("helper.R")'))
  io <- repro_file_io(code_list)
  expect_equal(io$sources[[1]], "helper.r")
})

test_that("repro_file_io returns an empty frame for no files", {
  io <- repro_file_io(list())
  expect_equal(nrow(io), 0)
})


# repro_defined_vars() ----

test_that("repro_defined_vars finds top-level assignments", {
  code_list <- list("a.R" = c("x <- 1", "  y <- 2", "assign('z', 3)"))
  out <- repro_defined_vars(code_list)
  # y is indented (not top-level) so it is not picked up; x and z are
  expect_setequal(out$defines[[1]], c("x", "z"))
})

test_that("repro_defined_vars returns an empty frame for no files", {
  out <- repro_defined_vars(list())
  expect_equal(nrow(out), 0)
})


# repro_missing_inputs() ----

test_that("repro_missing_inputs classifies an absent file", {
  out <- repro_missing_inputs("missing.csv", plan = NULL, structure_df = NULL)
  expect_equal(out$status, "absent")
})

test_that("repro_missing_inputs classifies a size-withheld file", {
  skipped <- data.frame(file_name = "big.csv", file_size = 200 * 1024 * 1024)
  out <- repro_missing_inputs("big.csv", plan = NULL, structure_df = NULL, skipped = skipped)
  expect_equal(out$status, "withheld_size")
  expect_match(out$detail, "200 MB")
})

test_that("repro_missing_inputs classifies a listed-but-not-downloaded file", {
  structure_df <- data.frame(file_name = "notdl.csv", file_location = NA_character_)
  out <- repro_missing_inputs("notdl.csv", plan = NULL, structure_df = structure_df)
  expect_equal(out$status, "in_repo_not_downloaded")
})

test_that("repro_missing_inputs excludes a file that is present and downloaded", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", tmp)
  structure_df <- data.frame(file_name = "present.csv", file_location = tmp)
  out <- repro_missing_inputs("present.csv", plan = NULL, structure_df = structure_df)
  expect_equal(nrow(out), 0)
})

test_that("repro_missing_inputs returns an empty frame for no refs", {
  out <- repro_missing_inputs(character(0), plan = NULL, structure_df = NULL)
  expect_equal(nrow(out), 0)
})


# repro_materialize_layout() ----

test_that("repro_materialize_layout copies planned files into their target paths", {
  src <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", src)
  structure_df <- data.frame(file_name = "demographics.csv", file_location = src)
  plan <- data.frame(file_name = "demographics.csv",
                     target_path = "study-ex1/data/study-demographics_data.csv")
  root <- withr::local_tempdir()
  repro_materialize_layout(plan, structure_df, root)

  dest <- file.path(root, "study-ex1/data/study-demographics_data.csv")
  expect_true(file.exists(dest))
  expect_true(dir.exists(file.path(root, "output")))
})

test_that("repro_materialize_layout also copies the original for a converted tabular row", {
  src <- withr::local_tempfile(fileext = ".dta")
  writeLines("fake dta bytes", src)
  structure_df <- data.frame(file_name = "math.dta", file_location = src)
  plan <- data.frame(
    file_name = "math.dta",
    target_path = "study-ex1/data/study-math_data.csv",
    original_target = "study-ex1/data/math.dta"
  )
  root <- withr::local_tempdir()
  repro_materialize_layout(plan, structure_df, root)

  # the CSV target has no real source to copy from (no CSV file on disk), so
  # ok = FALSE for it; the original .dta copy must still succeed
  expect_true(file.exists(file.path(root, "study-ex1/data/math.dta")))
})

test_that("repro_materialize_layout still creates output/ with no plan at all", {
  root <- withr::local_tempdir()
  out <- repro_materialize_layout(NULL, NULL, root)
  expect_true(dir.exists(file.path(root, "output")))
  expect_equal(nrow(attr(out, "materialised")), 0)
})

test_that("repro_materialize_layout marks a file not_ok when its source cannot be found", {
  structure_df <- data.frame(file_name = "demographics.csv", file_location = NA_character_)
  plan <- data.frame(file_name = "demographics.csv", target_path = "data/demographics.csv")
  root <- withr::local_tempdir()
  out <- repro_materialize_layout(plan, structure_df, root)
  mat <- attr(out, "materialised")
  expect_false(mat$ok[mat$target_path == "data/demographics.csv"])
})


# repro_write_scripts() ----

test_that("repro_write_scripts applies a resolved literal path rewrite", {
  code_list <- list("analysis.R" = 'd <- read.csv("data/demographics.csv")')
  rewrite_list <- list("analysis.R" = data.frame(
    ref = "data/demographics.csv", basename = "demographics.csv",
    matched = TRUE, target = "study-ex1/data/study-demographics_data.csv",
    ambiguous = FALSE, n_candidates = 1L, is_call = FALSE
  ))
  plan <- data.frame(file_name = "analysis.R", target_path = "analysis.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root)

  written <- readLines(out$script_path[1])
  expect_match(paste(written, collapse = "\n"),
              "study-ex1/data/study-demographics_data.csv", fixed = TRUE)
})

test_that("repro_write_scripts comments out setwd() and records it", {
  code_list <- list("analysis.R" = c('setwd("/Users/author/project")', 'x <- 1'))
  rewrite_list <- list("analysis.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "analysis.R", target_path = "analysis.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root)

  expect_equal(out$setwd_removed, 1)
  expect_match(out$setwd_paths, "/Users/author/project", fixed = TRUE)
  written <- readLines(out$script_path[1])
  expect_true(any(grepl("^# \\[reproducibility_check removed setwd\\]", written)))
})

test_that("repro_write_scripts redirects a literal write target into output/", {
  code_list <- list("analysis.R" = 'write.csv(x, "results/out.csv")')
  rewrite_list <- list("analysis.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "analysis.R", target_path = "analysis.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root)

  written <- paste(readLines(out$script_path[1]), collapse = "\n")
  expect_match(written, "output/out.csv", fixed = TRUE)
  expect_false(grepl("results/out.csv", written, fixed = TRUE))
})

test_that("repro_write_scripts replaces a named font family with 'sans'", {
  code_list <- list("plot.R" = 'theme_few(base_family = "Times New Roman")')
  rewrite_list <- list("plot.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "plot.R", target_path = "plot.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root)

  expect_equal(out$family_replaced, 1)
  written <- paste(readLines(out$script_path[1]), collapse = "\n")
  expect_match(written, 'base_family = "sans"', fixed = TRUE)
})

test_that("repro_write_scripts places a script with no plan target at the tree root", {
  code_list <- list("orphan.R" = "x <- 1")
  rewrite_list <- list("orphan.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan = NULL, root)
  expect_equal(out$script_path, file.path(root, "orphan.R"))
})


# repro_run_scripts() (execute phase; runs real subprocesses) ----

test_that("repro_run_scripts runs a script and records ran_ok with its output", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  file.copy(test_path("fixtures", "repro", "data.csv"), file.path(root, "data.csv"))
  writeLines(readLines(test_path("fixtures", "repro", "ok.R")),
            file.path(root, "ok.R"))
  run_tbl <- data.frame(file_name = "ok.R", script_path = file.path(root, "ok.R"),
                        run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "ok.R", timeout = 60)

  expect_equal(out$outcome, "ran_ok")
  expect_match(out$stdout, "Welch Two Sample t-test")
})

test_that("repro_run_scripts records errored with the error message", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  file.copy(test_path("fixtures", "repro", "data.csv"), file.path(root, "data.csv"))
  writeLines(readLines(test_path("fixtures", "repro", "errors.R")),
            file.path(root, "errors.R"))
  run_tbl <- data.frame(file_name = "errors.R", script_path = file.path(root, "errors.R"),
                        run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "errors.R", timeout = 60)

  expect_equal(out$outcome, "errored")
  expect_equal(out$error_type, "runtime")
  expect_match(out$error, "deliberate failure", fixed = TRUE)
})

test_that("repro_run_scripts classifies an undefined-variable error and captures the name", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  writeLines(readLines(test_path("fixtures", "repro", "undefined_var.R")),
            file.path(root, "undefined_var.R"))
  run_tbl <- data.frame(file_name = "undefined_var.R",
                        script_path = file.path(root, "undefined_var.R"), run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "undefined_var.R", timeout = 60)

  expect_equal(out$outcome, "errored")
  expect_equal(out$error_type, "undefined_variable")
  expect_equal(out$undefined_var, "some_var_no_script_defines")
})

test_that("repro_run_scripts marks a skipped file without running it", {
  root <- withr::local_tempdir()
  writeLines("x <- 1", file.path(root, "skip_me.R"))
  run_tbl <- data.frame(file_name = "skip_me.R", script_path = file.path(root, "skip_me.R"),
                        run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "skip_me.R", skip = "skip_me.R")
  expect_equal(out$outcome, "skipped_missing_inputs")
})

test_that("repro_run_scripts marks a not-parsed file without running it", {
  root <- withr::local_tempdir()
  writeLines("x <- 1", file.path(root, "bad.R"))
  run_tbl <- data.frame(file_name = "bad.R", script_path = file.path(root, "bad.R"),
                        run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "bad.R",
                           parses = stats::setNames(FALSE, "bad.R"))
  expect_equal(out$outcome, "not_parsed")
})

test_that("repro_run_scripts returns an empty frame for no scripts", {
  out <- repro_run_scripts(data.frame(), order = character(0))
  expect_equal(nrow(out), 0)
  expect_true(all(c("file_name", "outcome", "script_lines", "captures") %in% names(out)))
})
