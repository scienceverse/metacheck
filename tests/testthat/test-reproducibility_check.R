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

test_that("repro_dependencies reads a GitHub source from groundhog.library()", {
  deps <- repro_dependencies(c("groundhog.library('github::crsh/papaja', '2023-01-01')",
                               'groundhog.library("user/other@v2", "2023-01-01")',
                               'groundhog.library("dplyr", "2023-01-01")'))
  expect_setequal(deps$package, c("papaja", "other", "dplyr"))
  expect_equal(deps$source[deps$package == "papaja"], "github")
  expect_equal(deps$ref[deps$package == "papaja"], "crsh/papaja")
  expect_equal(deps$ref[deps$package == "other"], "user/other@v2")
  expect_equal(deps$source[deps$package == "dplyr"], "cran")
  # and the script then loads the package, not the source
  expect_match(.repro_neutralise_env_calls("groundhog.library('github::crsh/papaja', '2023-01-01')")$txt,
               'library("papaja")', fixed = TRUE)
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

test_that("repro_dependencies classifies a Bioconductor package as source 'bioc'", {
  deps <- repro_dependencies(c("library(edgeR)", "library(dplyr)"))
  expect_equal(deps$source[deps$package == "edgeR"], "bioc")
  expect_equal(deps$source[deps$package == "dplyr"], "cran")
  expect_false(deps$base[deps$package == "edgeR"])
})

test_that("repro_dependencies keeps a github source for a Bioconductor package explicitly named that way", {
  code <- c("library(edgeR)",
           "remotes::install_github('Bioconductor/edgeR')")
  deps <- repro_dependencies(code)
  expect_equal(deps$source[deps$package == "edgeR"], "github")
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

test_that("repro_rewrite_paths takes file names with \\P, \\6, ( [ + as written, not as patterns (#597)", {
  # a Windows path with "\P" (10.1111/2041-210X.13928) or "\6" (jskhb_v1)
  # made an invalid pattern and stopped reproducibility_check for the paper
  plan <- data.frame(
    file_name = c("we2.tsv", "fit.csv", "results(1).csv", "a[1].csv", "data+x.csv"),
    target_path = c("study-ex1/data/we2.tsv", "study-ex1/data/fit.csv", "study-ex1/data/results(1).csv",
                    "study-ex1/data/a[1].csv", "study-ex1/data/data+x.csv"))
  code <- c(r"(d <- read.table("D:\AviaNZ\Primary_dataset\ruru\we2.tsv"))",
            r"(f <- read.csv("out\6.0\fit.csv"))",
            r"(r <- read.csv(file.path(dir, "results(1).csv")))",
            r"(a <- read.csv("a[1].csv"))",
            r"(x <- read.csv(here("data+x.csv")))")
  out <- expect_no_error(repro_rewrite_paths(code, "ex1/analysis.R", plan))
  expect_setequal(basename(gsub("\\\\", "/", out$ref)), plan$file_name)
  expect_true(all(out$matched))
  expect_equal(out$in_join[out$ref == "results(1).csv"], TRUE)
  expect_equal(out$in_here[out$ref == "data+x.csv"], TRUE)
  # "a[1].csv" is not taken as the pattern "a1.csv" or as "a.csv"
  expect_false(out$in_join[out$ref == "a[1].csv"] || out$in_here[out$ref == "a[1].csv"])
  # the install error lines of a package whose name has a "." in it
  txt <- "ERROR: dependency 'x' is not available for package 'data.table'\nERROR: dependency 'y' is not available for package 'dataXtable'"
  expect_equal(.repro_install_error_lines(txt, "data.table"),
               "ERROR: dependency 'x' is not available for package 'data.table'")
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
  # plan$file_name is the file's ORIGINAL basename (what structure_df$file_name
  # carries for a converted row — see R/psychds-file-plan.R's plan_table
  # build), and basename matching
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

test_that("repro_rewrite_paths does not take different files with one name for copies", {
  # looked up by name, both rows got the first file, so they had the same
  # content hash and were collapsed to the study 1 file, also for a study 2
  # script
  tmp1 <- withr::local_tempfile(fileext = ".csv"); writeLines("a,b\n1,2", tmp1)
  tmp2 <- withr::local_tempfile(fileext = ".csv"); writeLines("a,b\n3,4", tmp2)
  structure_df <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    file_location = c(tmp1, tmp2)
  )
  plan <- data.frame(
    file_name = c("demographics.csv", "demographics.csv"),
    target_path = c("study-ex1/data/study-demographics_data.csv",
                    "study-ex2/data/study-demographics_data.csv")
  )
  code <- 'd <- read.csv("demographics.csv")'
  out <- repro_rewrite_paths(code, "ex2/analysis.R", plan, structure_df = structure_df)
  expect_true(out$matched)
  expect_equal(out$target, "study-ex2/data/study-demographics_data.csv")
})

test_that(".repro_apply_rewrites changes a path only where it stands in quotes", {
  # replaced wherever the text appeared, x.csv also changed "old_x.csv"
  rw <- data.frame(ref = "x.csv", matched = TRUE, ambiguous = FALSE,
                   target = "data/study-x_data.csv", is_call = FALSE)
  txt <- c('a <- read.csv("x.csv")', "b <- read.csv('x.csv')",
           'c <- read.csv("old_x.csv")', '"d <- read.csv(\\"x.csv\\")"')
  out <- .repro_apply_rewrites(txt, rw)
  expect_equal(out[1], 'a <- read.csv("data/study-x_data.csv")')
  expect_equal(out[2], "b <- read.csv('data/study-x_data.csv')")
  expect_equal(out[3], 'c <- read.csv("old_x.csv")')
  expect_equal(out[4], '"d <- read.csv(\\"data/study-x_data.csv\\")"')
})

test_that("repro_write_scripts writes a script in a folder with a colon in its name (#557)", {
  root <- withr::local_tempdir()
  plan <- data.frame(file_name = "code.R", target_path = "analysis/Title: a subtitle/code.R")
  out <- suppressMessages(repro_write_scripts(list(code.R = "x <- 1"), list(), plan, root))
  expect_equal(readLines(out$script_path), "x <- 1")
  expect_equal(out$script_path, file.path(root, "analysis/Title_ a subtitle/code.R"))
})

test_that("repro_materialize_layout writes to the same changed name (#557)", {
  root <- withr::local_tempdir()
  src <- withr::local_tempfile(lines = "a,b")
  plan <- data.frame(file_name = "x.csv", target_path = "data/Title: a subtitle/x.csv")
  s <- data.frame(file_name = "x.csv", file_location = src)
  repro_materialize_layout(plan, s, root)
  expect_true(file.exists(file.path(root, "data/Title_ a subtitle/x.csv")))
})

test_that("a script that cannot be written does not stop the others (#557)", {
  root <- withr::local_tempdir()
  # a file where the script's folder would have to be
  writeLines("in the way", file.path(root, "blocked"))
  plan <- data.frame(file_name = c("a.R", "b.R"), target_path = c("blocked/a.R", "analysis/b.R"))
  code <- list(a.R = "x <- 1", b.R = "y <- 2")
  expect_message(out <- repro_write_scripts(code, list(), plan, root), "could not write 'a.R'")
  expect_equal(nrow(out), 2)
  expect_false(file.exists(out$script_path[1]))
  expect_equal(readLines(out$script_path[2]), "y <- 2")
})

test_that("repro_write_scripts leaves a longer file name that ends in a changed one alone", {
  root <- withr::local_tempdir()
  plan <- data.frame(file_name = c("analysis.R", "x.csv", "old_x.csv"),
                     target_path = c("analysis/analysis.R", "data/study-x_data.csv",
                                     "data/study-oldx_data.csv"))
  code <- list(analysis.R = c('a <- read.csv("x.csv")', 'b <- read.csv("old_x.csv")'))
  rw <- list(analysis.R = repro_rewrite_paths(code$analysis.R, "analysis.R", plan))
  suppressMessages(repro_write_scripts(code, rw, plan, root))
  out <- readLines(file.path(root, "analysis/analysis.R"))
  expect_equal(out, c('a <- read.csv("data/study-x_data.csv")',
                      'b <- read.csv("data/study-oldx_data.csv")'))
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

test_that("repro_run_order fuzzy-matches a renumbered/reworded source() target", {
  # "001 - data prep.R" (sourced) vs the actual file "103 - Data Prep.R" -- an
  # exact basename match finds nothing, but both normalise to "dataprep".
  files <- data.frame(file_name = c("201 - Analysis.R", "103 - Data Prep.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list("001 - data prep.R", character(0))
  out <- repro_run_order(files)
  ord <- stats::setNames(out$order, out$file_name)
  expect_lt(ord[["103 - Data Prep.R"]], ord[["201 - Analysis.R"]])
  fuzzy <- attr(out, "fuzzy_sources")
  expect_equal(nrow(fuzzy), 1)
  expect_equal(fuzzy$from, "103 - Data Prep.R")
  expect_equal(fuzzy$to, "201 - Analysis.R")
})

test_that("repro_run_order does not fuzzy-match when an exact basename hit already exists", {
  files <- data.frame(file_name = c("main.R", "helper.R"))
  files$reads   <- list(character(0), character(0))
  files$writes  <- list(character(0), character(0))
  files$sources <- list("helper.R", character(0))
  out <- repro_run_order(files)
  expect_equal(nrow(attr(out, "fuzzy_sources")), 0)
})

test_that("repro_run_order leaves an ambiguous fuzzy source() match unresolved", {
  files <- data.frame(file_name = c("main.R", "01_prep.R", "02_prep.R"))
  files$reads   <- list(character(0), character(0), character(0))
  files$writes  <- list(character(0), character(0), character(0))
  files$sources <- list("00_prep.R", character(0), character(0))
  out <- repro_run_order(files)
  expect_equal(nrow(attr(out, "fuzzy_sources")), 0)
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

test_that("Python open() is a write only by its mode argument (U240)", {
  # any quoted word with a w, a or x in it counted, so an encoding made a
  # write, and "r+" did not
  lines <- c('open(p, encoding="ascii")', 'open("raw")', 'open("x.csv", "rb")',
             'open("x.csv", "w")', 'open("x.csv", "r+")',
             'open("x.csv", encoding="utf-8", mode="at")',
             'open(os.path.join(d, "x.csv"), "wb")')
  expect_equal(vapply(lines, .py_open_writes, logical(1), USE.NAMES = FALSE),
               c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  io <- repro_file_io(list(a.py = c('with open("out.txt", "w") as f:',
                                    'with open("in.txt", encoding="ascii") as g:')),
                      lang = "Python")
  expect_equal(io$reads[[1]], "in.txt")
  expect_equal(io$writes[[1]], "out.txt")
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


# .repro_py_defined_vars() ----

test_that(".repro_py_defined_vars finds top-level assignments and def/class", {
  code_list <- list("a.py" = c(
    "x = 1",
    "    y = 2",            # indented -- not top level
    "def some_fn():",
    "    pass",
    "class SomeClass:",
    "    pass"
  ))
  out <- .repro_py_defined_vars(code_list)
  expect_setequal(out$defines[[1]], c("x", "some_fn", "SomeClass"))
})

test_that(".repro_py_defined_vars does not confuse == with an assignment", {
  code_list <- list("a.py" = c("if x == 1:", "    pass"))
  out <- .repro_py_defined_vars(code_list)
  expect_equal(out$defines[[1]], character(0))
})

test_that(".repro_py_defined_vars returns an empty frame for no files", {
  out <- .repro_py_defined_vars(list())
  expect_equal(nrow(out), 0)
})


# .repro_ref_checkable() and .repro_inputs_to_report() ----

test_that(".repro_ref_checkable keeps only complete literal names (#568)", {
  d <- data.frame(
    ref = c("data/x.csv", "{name}.csv", "out/%s.pdf", "_field.csv", ".png", "input$a", "paste0(x, 'y.csv')",
            "{path}/weights/model.pth", "log(4runs)10k_3p.p", "fst=0.2_x.tab", "data\\\\win.csv"),
    basename = c("x.csv", "{name}.csv", "%s.pdf", "_field.csv", ".png", "input$a", "y.csv",
                 "model.pth", "log(4runs)10k_3p.p", "fst=0.2_x.tab", "win.csv"),
    is_call = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(.repro_ref_checkable(d),
               c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  # Python rows can lack is_call
  expect_true(.repro_ref_checkable(data.frame(ref = "x.csv", basename = "x.csv")))
  # a name the script builds, in any case
  expect_false(.repro_ref_checkable(data.frame(ref = "/X.csv", basename = "x.csv"), "x.csv"))
})

test_that(".repro_built_names finds names joined to a variable (#568)", {
  code <- c('a <- read.csv(paste0(x, "Field.csv"))',
            'b <- read.csv(base::paste(dir, "sub/site.csv", sep = "/"))',
            'f <- function(y) readRDS(paste0(y, "inner.rds"))',
            'g <- d[, paste0(x, "col.csv")]',
            'h <- paste0(x, )',
            'c <- read.csv(paste0("data/", "whole.csv"))',
            'e <- read.csv(file.path(x, "folder.csv"))')
  expect_setequal(.repro_built_names(code), c("field.csv", "site.csv", "inner.rds", "col.csv"))
  expect_equal(.repro_built_names('read.csv(paste0(x, "a.csv")'), character(0)) # does not parse
  expect_equal(.repro_built_names(NULL), character(0))
  py <- c('a = pd.read_csv(x + "Field.csv")', 'b = pd.read_csv("whole.csv")',
          '# c = pd.read_csv(x + "comment.csv")')
  expect_equal(.repro_built_names(py, "Python"), "field.csv")
})

test_that(".repro_inputs_to_report keeps what a run cannot show (#568)", {
  m <- data.frame(basename = c("a.csv", "b.csv", "c.csv", "d.csv"),
                  status = c("absent", "withheld_size", "in_archive", "absent"),
                  detail = "", similar_to = c(NA, NA, NA, "D_.csv"))
  expect_equal(.repro_inputs_to_report(m, FALSE), m)
  expect_equal(.repro_inputs_to_report(m, TRUE)$basename, c("b.csv", "c.csv", "d.csv"))
  expect_equal(nrow(.repro_inputs_to_report(m[0, ], TRUE)), 0)
})

test_that(".repro_inputs_after_run keeps only files of scripts that did not run through (#568)", {
  m <- data.frame(basename = c("a.csv", "b.csv", "c.csv"), status = "absent",
                  detail = "", similar_to = NA_character_)
  unresolved <- list("a.csv", "b.csv", "c.csv", character(0))
  files <- c("ok.R", "stops.R", "nopkg.R", "other.R")
  rr <- data.frame(file_name = files, outcome = c("ran_ok", "errored", "dependency_unavailable", "ran_ok"))
  expect_equal(.repro_inputs_after_run(m, unresolved, files, rr)$basename, c("b.csv", "c.csv"))
  rr$outcome <- c("ran_ok", "run_by_source", "ran_ok", "ran_ok")
  expect_equal(nrow(.repro_inputs_after_run(m, unresolved, files, rr)), 0)
  # no run, nothing changes; a script with no result counts as not run
  expect_equal(.repro_inputs_after_run(m, unresolved, files, NULL), m)
  expect_equal(.repro_inputs_after_run(m, unresolved, files, rr[rr$file_name != "ok.R", ])$basename, "a.csv")
})

test_that(".repro_static_light follows the missing inputs and the other checks (#568)", {
  none <- data.frame(basename = character(0), status = character(0))
  withheld <- data.frame(basename = "a.csv", status = "withheld_size")
  mixed <- data.frame(basename = c("a.csv", "b.csv"), status = c("withheld_size", "absent"))
  expect_equal(.repro_static_light(none, TRUE), "green")
  expect_equal(.repro_static_light(none, FALSE), "yellow")
  expect_equal(.repro_static_light(withheld, TRUE), "red")
  expect_equal(.repro_static_light(mixed, TRUE), "yellow")
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


# .repro_content_sniff() ----

test_that(".repro_content_sniff detects a JSON dump spanning the whole file", {
  code <- c('{"x":{"material":"phong","data":[1,2,3]}}')
  expect_equal(.repro_content_sniff(code), "JSON")
})

test_that(".repro_content_sniff detects an HTML doctype", {
  code <- c("<!DOCTYPE html>", "<html><body>hi</body></html>")
  expect_equal(.repro_content_sniff(code), "HTML")
})

test_that(".repro_content_sniff detects an XML declaration", {
  code <- c('<?xml version="1.0"?>', "<root></root>")
  expect_equal(.repro_content_sniff(code), "XML")
})

test_that(".repro_content_sniff leaves real R code alone, even one starting with '{'", {
  code <- c("{", "  x <- 1", "  y <- 2", "}", "print(x + y)")
  expect_true(is.na(.repro_content_sniff(code)))
})

test_that(".repro_content_sniff returns NA for empty input", {
  expect_true(is.na(.repro_content_sniff(character(0))))
  expect_true(is.na(.repro_content_sniff(NULL)))
})


# .repro_is_jags_model() ----

test_that(".repro_is_jags_model detects a model{} block as the first content", {
  code <- c("model {", "  psi ~ dlogis(0, 1)", "}")
  expect_true(.repro_is_jags_model(code))
})

test_that(".repro_is_jags_model detects a jags_script/ path component", {
  expect_true(.repro_is_jags_model("x <- 1", file_name = "study1/jags_script/conditional_model.R"))
})

test_that(".repro_is_jags_model detects a calling reference in another file's code", {
  jags_code <- c("psi ~ dlogis(0,1)")   # does NOT start with model{ itself
  other <- list(c('m <- rjags::jags.model(file = "model_def.R", data = d)'))
  expect_true(.repro_is_jags_model(jags_code, file_name = "model_def.R",
                                   other_code_text = other))
})

test_that(".repro_is_jags_model is FALSE for ordinary R code", {
  expect_false(.repro_is_jags_model(c("x <- 1", "y <- 2"), file_name = "analysis.R"))
  expect_false(.repro_is_jags_model(c("model <- lm(y ~ x)", "{", "1", "}"), file_name = "analysis.R"))
  expect_false(.repro_is_jags_model(c("data <- 1", "model <- 2"), file_name = "analysis.R"))
})

test_that(".repro_is_jags_model detects a brace on the next line and a data block first (#568)", {
  expect_true(.repro_is_jags_model(c("# a model", "model", "{", "  x ~ dnorm(0, 1)", "}")))
  expect_true(.repro_is_jags_model(c("data {", "  y <- 1", "}", "model {", "  x ~ dnorm(0, 1)", "}")))
})

test_that(".repro_checklist_section does not count a model file as a script that failed (#568)", {
  tb <- data.frame(file_name = c("m.R", "run.R"), run_order = 1:2, outcome = c("jags_model", "ran_ok"),
                   setwd_removed = 0, not_runnable_reason = c("jags_model", NA),
                   unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = c("m.R", "run.R"), outcome = c("jags_model", "ran_ok"), error = "", stderr = "")
  out <- .repro_checklist_section(tb, rr)
  expect_match(out, "**1 of 1 script ran to the end**", fixed = TRUE)
  expect_match(out, "It is a JAGS model, which another script reads.", fixed = TRUE)
})


# .repro_find_export_pkg() ----

test_that(".repro_find_export_pkg resolves a name exported by exactly one candidate", {
  pkg <- .repro_find_export_pkg("test_that", c("testthat", "dplyr"))
  expect_equal(pkg, "testthat")
})

test_that(".repro_find_export_pkg returns NA when no candidate exports the name", {
  pkg <- .repro_find_export_pkg("not_a_real_exported_name_xyz", c("testthat", "dplyr"))
  expect_true(is.na(pkg))
})

test_that(".repro_find_export_pkg returns NA for empty candidates", {
  expect_true(is.na(.repro_find_export_pkg("test_that", character(0))))
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

test_that("repro_materialize_layout copies each of several files with one name from its own place", {
  # looked up by name, every demographics.csv got the first one's content
  tmp1 <- withr::local_tempfile(fileext = ".csv"); writeLines("a,b\n1,2", tmp1)
  tmp2 <- withr::local_tempfile(fileext = ".csv"); writeLines("a,b\n3,4", tmp2)
  structure_df <- data.frame(file_name = c("demographics.csv", "demographics.csv"),
                             file_path = c("s1/demographics.csv", "s2/demographics.csv"),
                             file_location = c(tmp1, tmp2))
  plan <- psychds_file_plan(transform(structure_df, data_type = "data",
                                      group = c("ex1", "ex2")))$table
  root <- withr::local_tempdir()
  repro_materialize_layout(plan, structure_df, root)
  expect_equal(readLines(file.path(root, plan$target_path[1])), c("a,b", "1,2"))
  expect_equal(readLines(file.path(root, plan$target_path[2])), c("a,b", "3,4"))
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

test_that("repro_write_scripts replaces setwd() with a no-op and records it", {
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
  expect_true(any(grepl("^invisible\\(NULL\\)  # \\[reproducibility_check removed setwd\\] setwd",
                        written)))
})

test_that("repro_write_scripts removes base::setwd() and chdir() under any module name (tue_datamanager_check#4)", {
  # only setwd( and os.chdir( were caught, so `from os import chdir` ...
  # chdir(DIR) ran and failed on the author's Windows path
  empty_rw <- function(fn) stats::setNames(list(data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0))), fn)
  root <- withr::local_tempdir()
  py_code <- list("c.py" = c("from os import chdir", 'chdir("C:/Users/author")',
                             "import os as o", 'o.chdir("x")', "print(chdir)", "mychdir(1)"))
  out_py <- repro_write_scripts(py_code, empty_rw("c.py"),
                                data.frame(file_name = "c.py", target_path = "c.py"),
                                root, lang = "Python")
  written <- readLines(out_py$script_path)
  expect_equal(out_py$setwd_removed, 2)
  expect_true(any(grepl("^pass  # \\[reproducibility_check removed os.chdir\\] chdir", written)))
  expect_true("print(chdir)" %in% written)
  expect_true("mychdir(1)" %in% written)
  r_code <- list("d.R" = c('base::setwd("/Users/author")', "x <- 1"))
  out_r <- repro_write_scripts(r_code, empty_rw("d.R"),
                               data.frame(file_name = "d.R", target_path = "d.R"), root)
  expect_equal(out_r$setwd_removed, 1)
})

test_that("repro_write_scripts keeps a block valid when setwd()/os.chdir() is its only statement (#480)", {
  empty_rw <- function(fn) stats::setNames(list(data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0))), fn)
  root <- withr::local_tempdir()

  # R: commenting the call out would make `if` take `x <- 2` as its body
  r_code <- list("a.R" = c("x <- 1", "if (FALSE)", '  setwd("/Users/author")', "x <- 2"))
  out_r <- repro_write_scripts(r_code, empty_rw("a.R"),
                               data.frame(file_name = "a.R", target_path = "a.R"), root)
  env <- new.env()
  sys.source(out_r$script_path, envir = env)
  expect_equal(env$x, 2)

  # Python: a comment would leave the try: block empty (IndentationError)
  py_code <- list("b.py" = c("try:", '    os.chdir("/Users/author")', "except OSError:", "    pass"))
  out_py <- repro_write_scripts(py_code, empty_rw("b.py"),
                                data.frame(file_name = "b.py", target_path = "b.py"),
                                root, lang = "Python")
  written <- readLines(out_py$script_path)
  expect_equal(written[2], '    pass  # [reproducibility_check removed os.chdir] os.chdir("/Users/author")')
  expect_equal(out_py$setwd_removed, 1)
  py <- .repro_find_python()
  skip_if_not(nzchar(py), "no Python interpreter")
  res <- processx::run(py, c("-c", "import ast,sys; ast.parse(open(sys.argv[1]).read())",
                             out_py$script_path), error_on_status = FALSE)
  expect_equal(res$status, 0L)
})

test_that("the write-call scan stays fast on a large script with a non-ASCII character (#481)", {
  # substr(x, i, i) counts from the start of a string with non-ASCII
  # characters on every call, so the character walk was quadratic: a walk
  # from an unclosed call near the top to the end of a 200,000-character
  # script took 201 s (and hours at 1 MB); now about 3 s. The accented letter sits in a
  # string, not a comment, since comments are removed before the scan.
  big <- c('x <- "café"', "write.csv(df, 'out.csv'",
           rep('y <- "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"', 3200))
  expect_gt(sum(nchar(big)), 200000)
  t <- system.time(res <- .repro_redirect_writes(big, "R"))[["elapsed"]]
  expect_lt(t, 20)
  expect_equal(nrow(res), 0)   # the unclosed call is left alone, as before
  t2 <- system.time(.repro_format_call_refs(big))[["elapsed"]]
  expect_lt(t2, 20)
  # a closed call later in the same text is still found
  res2 <- .repro_redirect_writes(c(big[1], "write.csv(df, 'out.csv')", big[-(1:2)]), "R")
  expect_equal(nrow(res2), 1)
})

test_that(".repro_redirect_writes keeps a named argument's name (U134)", {
  # `save(a, b, file = "ab.RData")` became `save(a, b, "<out>/ab.RData")`, so
  # save() took the path for another object to save
  res <- .repro_redirect_writes(c('save(a, b, file = "ab.RData")',
                                  'write.csv(df, "out.csv")'), "R")
  expect_equal(res$replacement,
               c('save(a, b, file = "{{REPRO_OUTPUT}}ab.RData")',
                 'write.csv(df, "{{REPRO_OUTPUT}}out.csv")'))
})

test_that(".repro_format_call_refs reads the arguments after the file name, wherever it is (U133)", {
  # the arguments after the quoted string were counted from the start of the
  # call, so a file name that is not the first argument was misread
  res <- .repro_format_call_refs(c('a <- read.csv(paste(wd, "data.csv", sep = "/"))',
                                   'b <- source(paste0(path, ".R"))',
                                   'c <- read.csv(sprintf("%s/x.csv", wd))'))
  expect_equal(res$call_text, c('paste(wd, "data.csv", sep = "/")',
                                'sprintf("%s/x.csv", wd)'))
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

test_that("repro_write_scripts injects a library() call at the top when asked", {
  code_list <- list("uses_testthat.R" = c('test_that("x", { expect_true(TRUE) })'))
  rewrite_list <- list("uses_testthat.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "uses_testthat.R", target_path = "uses_testthat.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root,
                             inject_libs = c(uses_testthat.R = "testthat"))

  expect_equal(out$library_injected, "testthat")
  written <- readLines(out$script_path[1])
  expect_match(written[1], "^library\\(testthat\\)")
})

test_that("repro_write_scripts injects nothing for a file not named in inject_libs", {
  code_list <- list("plain.R" = "x <- 1")
  rewrite_list <- list("plain.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "plain.R", target_path = "plain.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root,
                             inject_libs = c(other.R = "testthat"))
  expect_true(is.na(out$library_injected))
  written <- readLines(out$script_path[1])
  expect_false(any(grepl("^library\\(", written)))
})

test_that("repro_write_scripts injects several library() calls in order", {
  code_list <- list("uses_pipe.R" = "x <- 1:3 %>% sum()")
  rewrite_list <- list("uses_pipe.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "uses_pipe.R", target_path = "uses_pipe.R")
  root <- withr::local_tempdir()
  out <- repro_write_scripts(code_list, rewrite_list, plan, root,
                             inject_libs = list(uses_pipe.R = c("dplyr", "ggplot2")))
  expect_equal(out$library_injected, "dplyr, ggplot2")
  written <- readLines(out$script_path[1])
  expect_match(written[1], "^library\\(dplyr\\)")
  expect_match(written[2], "^library\\(ggplot2\\)")
  expect_equal(written[3], "x <- 1:3 %>% sum()")
})

# .repro_run_by_source() ----

test_that("repro_run_order records every source() edge", {
  files <- data.frame(file_name = c("main.R", "a.R", "b.R"))
  files$sources <- list(c("a.r", "b.r"), character(0), character(0))
  out <- repro_run_order(files)
  edges <- attr(out, "source_edges")
  expect_setequal(edges$from, c("a.R", "b.R"))
  expect_true(all(edges$to == "main.R"))
})

test_that(".repro_run_by_source finds scripts run only through source(), following chains", {
  ot <- data.frame(file_name = c("main.R", "a.R", "b.R", "solo.R"))
  attr(ot, "source_edges") <- data.frame(from = c("a.R", "b.R"), to = c("main.R", "a.R"))
  out <- .repro_run_by_source(ot, c(main.R = TRUE, a.R = TRUE, b.R = TRUE, solo.R = TRUE))
  expect_setequal(names(out), c("a.R", "b.R"))
  expect_equal(out[["a.R"]], "main.R")
  expect_equal(out[["b.R"]], "a.R")
})

test_that(".repro_run_by_source runs a script on its own when its sourcer cannot run", {
  ot <- data.frame(file_name = c("main.R", "a.R"))
  attr(ot, "source_edges") <- data.frame(from = "a.R", to = "main.R")
  # main.R does not parse / lacks inputs: a.R still runs on its own.
  expect_length(.repro_run_by_source(ot, c(main.R = FALSE, a.R = TRUE)), 0)
  # a.R itself cannot run: it keeps its own outcome rather than being hidden.
  expect_length(.repro_run_by_source(ot, c(main.R = TRUE, a.R = FALSE)), 0)
})

test_that(".repro_run_by_source leaves two scripts that source each other on their own", {
  ot <- data.frame(file_name = c("a.R", "b.R"))
  attr(ot, "source_edges") <- data.frame(from = c("a.R", "b.R"), to = c("b.R", "a.R"))
  expect_length(.repro_run_by_source(ot, c(a.R = TRUE, b.R = TRUE)), 0)
})

# .repro_missing_fn_pkgs() ----

test_that(".repro_missing_fn_pkgs adds every package the other scripts attach", {
  code_list <- list(
    "1_prep.R" = c("library(tidyverse)", "library(lme4)"),
    "2_analysis.R" = c("d %>% filter(x > 1)"))
  expect_equal(.repro_missing_fn_pkgs("%>%", "2_analysis.R", code_list, character(0)),
               c("tidyverse", "lme4"))
})

test_that(".repro_missing_fn_pkgs falls back to magrittr for %>% when nothing provides it", {
  code_list <- list("only.R" = "x <- 1:3 %>% sum()",
                    "other.R" = "library(lme4)")
  expect_equal(.repro_missing_fn_pkgs("%>%", "only.R", code_list, character(0)),
               "magrittr")
})

test_that(".repro_missing_fn_pkgs returns nothing for an unknown function", {
  code_list <- list("only.R" = "x <- not_a_real_function_xyz()")
  expect_length(.repro_missing_fn_pkgs("not_a_real_function_xyz", "only.R",
                                       code_list, character(0)), 0)
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

test_that("repro_run_scripts reads a missing package in curly quotes as dependency_unavailable (U132)", {
  # in a UTF-8 locale R writes "there is no package called \u2018x\u2019";
  # only ASCII quotes were matched, so the script was "errored". The script
  # writes the message itself, so the test does not depend on the locale.
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  writeLines('stop("there is no package called \\u2018pkgxyz123\\u2019")',
             file.path(root, "needs_pkg.R"))
  run_tbl <- data.frame(file_name = "needs_pkg.R",
                        script_path = file.path(root, "needs_pkg.R"), run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "needs_pkg.R", timeout = 60,
                           failed_deps = "pkgxyz123")
  expect_equal(out$outcome, "dependency_unavailable")
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


# Recorded package versions ----

test_that(".repro_highest_version and .repro_same_version compare versions", {
  expect_equal(.repro_highest_version(c("1.9", "1.10", NA, "junk")), "1.10")
  expect_equal(.repro_highest_version(c("1.1-21", "1.1-9")), "1.1-21")
  expect_true(is.na(.repro_highest_version(c(NA, "junk"))))
  expect_true(.repro_same_version("1.1-21", "1.1.21"))
  expect_false(.repro_same_version("1.1-21", "1.1.22"))
  expect_false(.repro_same_version(NA_character_, "1.0"))
})

test_that(".repro_recorded_plan keeps only the packages the scripts load", {
  deps <- data.frame(
    package = c("dplyr", "lme4", "Matrix", "papaja", "limma", "praise", "groundhog", "stats"),
    source = c("cran", "cran", "base", "cran", "cran", "cran", "cran", "base"),
    ref = NA_character_, base = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  vp <- list(
    renv_packages = data.frame(
      file_name = c("a/renv.lock", "b/renv.lock", "a/renv.lock", "a/renv.lock", "a/renv.lock"),
      package = c("dplyr", "dplyr", "papaja", "unused", "Matrix"),
      version = c("1.0.0", "1.1.3", "0.1.2", "9.9.9", "1.5-1"),
      source = c("Repository", "Repository", "GitHub", "Repository", "Repository"),
      remote = c(NA, NA, "crsh/papaja@abc", NA, NA)),
    session_packages = data.frame(
      file_name = "si.txt", package = c("dplyr", "lme4", "limma"), version = c("0.8.0", "1.1-21", "3.50.0"),
      source = c(NA, NA, "Bioconductor"), remote = NA_character_),
    pin_dates = data.frame(mechanism = "groundhog", date = c("2020-01-01", "2021-03-01")))
  looked_up <- NULL
  local_mocked_bindings(.repro_date_versions = function(pkgs, date) {
    looked_up <<- date
    structure(stats::setNames(rep("1.0.0", length(pkgs)), pkgs), failed = FALSE)
  })
  p <- .repro_recorded_plan(deps, vp)
  d <- p$deps
  # no base package, no package manager, nothing that is only listed
  expect_setequal(d$package, c("dplyr", "lme4", "papaja", "limma", "praise"))
  # the highest version of the two lockfiles, not the session record's
  expect_equal(d$version[d$package == "dplyr"], "1.1.3")
  expect_equal(d$recorded_from[d$package == "dplyr"], "b/renv.lock")
  expect_equal(d$version[d$package == "lme4"], "1.1-21")
  # where it came from decides how it is installed
  expect_equal(d$source[d$package == "papaja"], "github")
  expect_equal(d$ref[d$package == "papaja"], "crsh/papaja@abc")
  expect_equal(d$source[d$package == "limma"], "bioc")
  # the latest date, for what no file lists
  expect_equal(looked_up, "2021-03-01")
  expect_equal(d$version[d$package == "praise"], "1.0.0")
  expect_equal(d$recorded_from[d$package == "praise"], "date 2021-03-01")
  # R's own packages keep R's version, and are named
  expect_equal(p$kept_with_r$package, "Matrix")
})

test_that(".repro_recorded_plan returns NULL when nothing records a loaded package", {
  deps <- data.frame(package = c("dplyr", "groundhog"), source = "cran", ref = NA_character_, base = FALSE)
  none <- function(pkg) list(
    renv_packages = data.frame(file_name = "renv.lock", package = pkg, version = "1.0", source = "Repository"),
    session_packages = NULL, pin_dates = NULL)
  expect_null(.repro_recorded_plan(deps, none("other")))
  expect_null(.repro_recorded_plan(deps, none("groundhog")))
  expect_null(.repro_recorded_plan(deps, NULL))
  expect_null(.repro_recorded_plan(NULL, none("dplyr")))
  # a record without a remote column, saved by an earlier version
  expect_equal(.repro_recorded_plan(deps, none("dplyr"))$deps$version, "1.0")
})

test_that(".repro_recorded_plan warns when a date's versions cannot be looked up", {
  deps <- data.frame(package = "dplyr", source = "cran", ref = NA_character_, base = FALSE)
  vp <- list(renv_packages = NULL, session_packages = NULL,
             pin_dates = data.frame(mechanism = "groundhog", date = "2010-01-01"))
  local_mocked_bindings(.repro_date_versions = function(pkgs, date)
    structure(stats::setNames(rep(NA_character_, length(pkgs)), pkgs), failed = TRUE))
  expect_warning(p <- .repro_recorded_plan(deps, vp), "2010-01-01")
  expect_null(p)
  expect_silent(p <- .repro_recorded_plan(deps, vp, use_dates = FALSE))
  expect_null(p)
})

test_that("the warning and report state that the R version is not changed", {
  recorded <- list(
    deps = data.frame(package = c("a", "b", "c", "d"), source = c("cran", "cran", "cran", "bioc"),
                      ref = NA_character_, base = FALSE, version = c("1.0", "2.0", NA, "3.0"),
                      recorded_from = "renv.lock"),
    kept_with_r = data.frame(package = "Matrix", version = "1.5-1"), dates = character(0),
    date_failed = FALSE)
  vp <- list(r_versions = "4.3.1",
             renv_packages = data.frame(file_name = "renv.lock", package = "a", version = "1.0", source = "Repository"),
             session_packages = NULL, pin_dates = NULL)
  w <- .repro_recorded_warning(recorded, vp, "docker")
  expect_match(w, "3 of the 4 packages", fixed = TRUE)
  expect_match(w, "The R version is not changed", fixed = TRUE)
  expect_match(w, "R 4.3.1", fixed = TRUE)
  expect_match(w, "long time", fixed = TRUE)
  expect_match(w, "depend on", fixed = TRUE)
  expect_match(w, "Matrix", fixed = TRUE)
  expect_match(w, "these get their current version: d", fixed = TRUE)
  expect_false(grepl("R version is not changed", .repro_recorded_warning(recorded, vp, "docker", TRUE), fixed = TRUE))

  results <- data.frame(package = c("a", "b", "c", "d"), source = "cran",
                        installed = c(TRUE, TRUE, TRUE, FALSE), message = c("", "", "", "no such package"),
                        version_recorded = c("1.0", "2.0", NA, "3.0"),
                        version_installed = c("1.0", "2.5", "9.9", NA),
                        version_note = c("", "could not build", "", ""))
  counts <- .repro_recorded_counts(recorded, results)
  expect_equal(counts$exact, c(TRUE, FALSE, FALSE))
  expect_equal(counts$replaced, c(FALSE, TRUE, FALSE))
  expect_equal(counts$failed, c(FALSE, FALSE, TRUE))
  rep_txt <- paste(.repro_recorded_report(recorded, results, vp, "process"), collapse = "\n")
  expect_match(rep_txt, "1 was installed at the recorded version, 1 got another version, and 1 could not", fixed = TRUE)
  expect_match(rep_txt, "not an exact reproduction", fixed = TRUE)
  expect_match(rep_txt, "could not build", fixed = TRUE)
  expect_match(rep_txt, "no such package", fixed = TRUE)
  expect_match(rep_txt, "also load c", fixed = TRUE)
  expect_match(rep_txt, "The R version is not changed", fixed = TRUE)
  expect_match(.repro_recorded_summary(recorded, results), "1 of the 3 packages", fixed = TRUE)
  expect_null(.repro_recorded_summary(NULL, results))
  expect_null(.repro_recorded_report(recorded, NULL, vp, "process"))
})

test_that(".repro_recorded_preview says what a run would install", {
  deps <- data.frame(package = c("a", "b"), source = "cran", ref = NA_character_, base = FALSE)
  vp <- list(renv_packages = data.frame(file_name = "renv.lock", package = "a", version = "1.0", source = "Repository"),
             session_packages = NULL, pin_dates = NULL)
  expect_match(.repro_recorded_preview(deps, vp), "would install 1 package ", fixed = TRUE)
  expect_match(.repro_recorded_preview(deps, vp), "would not change the R version", fixed = TRUE)
  vp <- list(renv_packages = NULL, session_packages = NULL,
             pin_dates = data.frame(mechanism = "checkpoint", date = "2022-01-01"))
  expect_match(.repro_recorded_preview(deps, vp), "would install 2 packages", fixed = TRUE)
  expect_null(.repro_recorded_preview(deps, list()))
})

test_that(".repro_install_limit gives the time for installing", {
  expect_equal(.repro_install_limit(NULL, 600, 3600), 600)
  expect_equal(.repro_install_limit(NULL, 600, 3600, recorded = TRUE), 3600)
  expect_equal(.repro_install_limit(NULL, 7200, 3600, recorded = TRUE), 7200)
  expect_equal(.repro_install_limit(100, 600, 3600, recorded = TRUE), 100)
  # at most half of what is left of a clock time, and never nothing
  expect_lte(.repro_install_limit(NULL, 600, Sys.time() + 1000, recorded = TRUE), 500)
  expect_equal(.repro_install_limit(NULL, 600, Sys.time() - 1000), 60)
})

test_that("the install progress says what is happening and for how long", {
  expect_equal(.repro_duration(12), "12 sec")
  expect_equal(.repro_duration(250), "4 min")
  expect_equal(.repro_duration(3900), "1 h 5 min")
  first <- .repro_install_what(1, 14, "lme4", "1.1-21", Sys.time() - 12)
  expect_match(first, "Installing lme4 1.1-21 (1 of 14, 12 sec so far)", fixed = TRUE)
  later <- .repro_install_what(3, 14, "lme4", NA, Sys.time() - 240)
  expect_match(later, "Installing lme4 (3 of 14, 4 min so far, about 24 min left)", fixed = TRUE)
})

test_that(".repro_docker_run_install follows the status file and stops at the time limit", {
  skip_if_quick()
  skip_if_not_installed("processx")
  wd <- withr::local_tempdir()
  status <- file.path(wd, "status")
  out <- file.path(wd, "out")
  prog <- file.path(wd, "p.R")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  inst <- data.frame(package = c("a", "b"), source = "cran", ref = NA_character_)
  writeLines(sprintf('for (i in 1:2) { writeLines(as.character(i), "%s"); Sys.sleep(2) }; cat("done\\n")',
                     gsub("\\\\", "/", status)), prog)
  expect_message(res <- .repro_docker_run_install(rscript, prog, 60, out, status, inst, c("1.0", NA)),
                 "Installing a 1.0 (1 of 2", fixed = TRUE)
  expect_equal(res, list(status = 0L, timeout = FALSE))
  expect_equal(readLines(out), "done")

  writeLines("Sys.sleep(30)", prog)
  res <- suppressMessages(.repro_docker_run_install(rscript, prog, 2, out, status, inst, c(NA, NA)))
  expect_true(res$timeout)
})

test_that("repro_install_deps installs a recorded version, and the current one if it cannot", {
  skip_if_not_installed("remotes")
  installed <- list()
  lib <- withr::local_tempdir()
  # Packages the tests can load anyway, since the load check after an install
  # looks in every library; the installs below only write a DESCRIPTION file.
  fake_install <- function(pkg, version) {
    dir.create(file.path(lib, pkg))
    writeLines(c(paste("Package:", pkg), paste("Version:", version), "Built: R 4.5.0; ; ; "),
               file.path(lib, pkg, "DESCRIPTION"))
  }
  local_mocked_bindings(.package = "remotes", install_version = function(package, version, lib, ...) {
    installed[[length(installed) + 1]] <<- c(package, version)
    if (package == "jsonlite") stop("version '", version, "' is invalid for package 'jsonlite'")
    fake_install(package, version)
  })
  local_mocked_bindings(.package = "utils", install.packages = function(pkgs, ...) fake_install(pkgs, "9.9"))

  deps <- data.frame(package = c("withr", "jsonlite", "dplyr"), source = "cran",
                     ref = NA_character_, version = c("0.0.1", "0.0.2", NA))
  out <- suppressMessages(repro_install_deps(deps, lib))
  expect_equal(out$installed, rep(TRUE, 3))
  expect_equal(out$version_recorded, c("0.0.1", "0.0.2", NA))
  expect_equal(out$version_installed, c("0.0.1", "9.9", "9.9"))
  expect_match(out$version_note[2], "invalid for package", fixed = TRUE)
  expect_equal(out$version_note[c(1, 3)], c("", ""))
  expect_equal(installed[[1]], c("withr", "0.0.1"))

  # already there at that version: nothing to do
  n <- length(installed)
  again <- suppressMessages(repro_install_deps(deps[1, ], lib))
  expect_equal(length(installed), n)
  expect_true(again$installed)

  # no time left: nothing is started
  late <- suppressMessages(repro_install_deps(deps[2, ], withr::local_tempdir(), deadline = Sys.time() - 1))
  expect_false(late$installed)
  expect_match(late$message, "time for installing", fixed = TRUE)
})

test_that("repro_install_deps keeps a recorded version out of your own library", {
  skip_if_not_installed("remotes")
  lib <- withr::local_tempdir()
  used <- NULL
  local_mocked_bindings(.package = "remotes", install_version = function(package, version, lib, ...) {
    used <<- lib
    dir.create(file.path(lib, package))
    writeLines(c(paste("Package:", package), paste("Version:", version), "Built: R 4.5.0; ; ; "),
               file.path(lib, package, "DESCRIPTION"))
  })
  out <- suppressMessages(repro_install_deps(
    data.frame(package = "withr", source = "cran", ref = NA_character_, version = "0.0.1"),
    lib, cran_to_main_lib = TRUE))
  expect_equal(used, lib)
  expect_equal(out$version_installed, "0.0.1")
})

test_that("repro_install_deps does not count a failed GitHub install as installed", {
  skip_if_not_installed("remotes")
  skip_if_not_installed("R6")
  deps <- data.frame(package = "R6", source = "github", ref = "r-lib/R6@abc", version = "0.0.1")
  # install_github() does not stop when the download fails. R6 is also in the
  # main library, which must not count. (Not on CRAN here, or the install
  # falls back to CRAN, #598.)
  local_mocked_bindings(.package = "remotes", install_github = function(...) NA)
  local_mocked_bindings(.repro_on_cran = function(pkg) FALSE)
  out <- suppressMessages(repro_install_deps(deps, withr::local_tempdir()))
  expect_false(out$installed)

  # installed, but the commit gave another version than the one recorded
  local_mocked_bindings(.package = "remotes", install_github = function(ref, lib, ...) {
    dir.create(file.path(lib, "R6"))
    writeLines(c("Package: R6", "Version: 0.0.2", "Built: R 4.5.0; ; ; "), file.path(lib, "R6", "DESCRIPTION"))
  })
  out <- suppressMessages(repro_install_deps(deps, withr::local_tempdir()))
  expect_true(out$installed)
  expect_equal(out$version_installed, "0.0.2")
  expect_match(out$version_note, "another version", fixed = TRUE)
})

test_that("a recorded version that fails without an error names the R version", {
  skip_if_not_installed("remotes")
  local_mocked_bindings(.package = "remotes", install_version = function(...) NA)
  local_mocked_bindings(.package = "utils", install.packages = function(pkgs, lib, ...) {
    dir.create(file.path(lib, pkgs))
    writeLines(c(paste("Package:", pkgs), "Version: 9.9", "Built: R 4.5.0; ; ; "), file.path(lib, pkgs, "DESCRIPTION"))
  })
  out <- suppressMessages(repro_install_deps(
    data.frame(package = "withr", source = "cran", ref = NA_character_, version = "0.0.1"),
    withr::local_tempdir()))
  expect_equal(out$version_note, paste0("the recorded version could not be installed on R ", getRversion()))
})

test_that("repro_install_deps installs a real old version into a temporary library", {
  skip_if_quick()
  skip_if_offline()
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))
  lib <- withr::local_tempdir()
  out <- suppressMessages(repro_install_deps(
    data.frame(package = "R6", source = "cran", ref = NA_character_, version = "2.4.0"), lib))
  expect_true(out$installed)
  expect_equal(out$version_installed, "2.4.0")
  expect_equal(as.character(utils::packageVersion("R6", lib.loc = lib)), "2.4.0")
})

test_that(".repro_date_versions reads a dated copy of CRAN", {
  skip_if_quick()
  skip_if_offline()
  v <- .repro_date_versions(c("R6", "notapackage"), "2021-03-01")
  expect_equal(v[["R6"]], "2.5.0")
  expect_true(is.na(v[["notapackage"]]))
  expect_false(attr(v, "failed"))
  # before the first copy
  expect_true(attr(.repro_date_versions("R6", "2016-01-01"), "failed"))
})

test_that(".repro_neutralise_env_calls replaces calls that would install packages", {
  rewrite <- function(...) .repro_neutralise_env_calls(c(...))
  one <- function(...) paste(rewrite(...)$txt, collapse = "\n")

  # groundhog.library() becomes library()
  expect_equal(one('groundhog.library("dplyr", "2023-01-01")'), '{ library("dplyr") }')
  expect_equal(one('groundhog::groundhog.library(c("dplyr", "lme4"), date)'),
               '{ library("dplyr"); library("lme4") }')
  expect_equal(one('groundhog.library(c("dplyr",', '   "lme4"), "2023-01-01"); x <- 1'),
               '{ library("dplyr"); library("lme4") }; x <- 1')
  expect_equal(one('pkgs <- c("a", "b")', 'groundhog.library(pkgs, d)'),
               'pkgs <- c("a", "b")\n{ library("a"); library("b") }')
  expect_equal(one('groundhog.library("', 'library(a)', 'library(b)', '", d)'),
               '{ library("a"); library("b") }')

  # a bare name is a package or a variable the script computes
  pkgs <- "stats"
  bare <- one('groundhog.library(pkgs, d)')
  expect_no_error(eval(parse(text = bare)))
  expect_match(one('groundhog.library(dplyr, d)'), 'library("dplyr")', fixed = TRUE)

  # the others do nothing, and name what was removed
  for (call in c('renv::restore()', 'renv::init(bare = TRUE)', 'renv::settings$snapshot.type("all")',
                 'source("renv/activate.R")', "source('renv/activate.R')",
                 'source(file.path("renv", "activate.R"))', 'library(renv)', 'library("groundhog")',
                 'set.groundhog.folder("x")', 'groundhog::meta.groundhog("2023-01-01")'))
    expect_match(one(call), '^invisible\\("reproducibility_check removed ')
  expect_match(one('renv::restore(', '  lockfile = "renv.lock"', ')'), '^invisible\\([^\n]*\\)$')
  expect_equal(one('if (!require(renv)) install.packages("renv")'), 'if (!TRUE) invisible(NULL)')
  expect_equal(one('if (!requireNamespace("groundhog", quietly = TRUE)) install.packages("groundhog")'),
               'if (!TRUE) invisible(NULL)')

  # checkpoint() only when the script names the package
  expect_equal(one('checkpoint("my result")'), 'checkpoint("my result")')
  expect_match(one('library(checkpoint)', 'checkpoint("2022-01-01")'),
               'removed a checkpoint call', fixed = TRUE)
  expect_match(one('checkpoint::checkpoint("2022-01-01")'), 'removed a checkpoint call', fixed = TRUE)

  # nothing else changes, and the count is right
  plain <- c('library(dplyr)', 'x <- groundhog', 'source("analysis.R")')
  expect_equal(rewrite(plain), list(txt = plain, n = 0L))
  expect_equal(rewrite('library(groundhog)', 'groundhog.library("a", d)', 'renv::restore()')$n, 3L)
  expect_equal(rewrite(character(0))$n, 0L)
  for (code in list('groundhog.library("a", d); x <- 1', 'if (TRUE) renv::restore() else 1',
                    'pkgs <- paste0("a"); groundhog.library(pkgs, d)'))
    expect_no_error(parse(text = rewrite(code)$txt))
})

test_that(".repro_neutralise_rprofile keeps the rest of a .Rprofile", {
  root <- withr::local_tempdir()
  expect_equal(.repro_neutralise_rprofile(root), 0L)
  path <- file.path(root, ".Rprofile")
  writeLines(c('options(digits = 4)', 'source("renv/activate.R")'), path)
  expect_equal(.repro_neutralise_rprofile(root), 1L)
  expect_equal(readLines(path)[1], "options(digits = 4)")
  expect_match(readLines(path)[2], "removed renv's activate.R", fixed = TRUE)
})

test_that("repro_write_scripts replaces package-manager calls only when asked", {
  code <- list("a.R" = c('groundhog.library("dplyr", "2023-01-01")', "x <- 1"))
  root <- withr::local_tempdir()
  out <- suppressMessages(repro_write_scripts(code, list(), NULL, root))
  expect_equal(out$env_calls_replaced, 0L)
  expect_equal(readLines(out$script_path), code[["a.R"]])

  out <- suppressMessages(repro_write_scripts(code, list(), NULL, root, replace_env_calls = TRUE))
  expect_equal(out$env_calls_replaced, 1L)
  expect_equal(readLines(out$script_path), c('{ library("dplyr") }', "x <- 1"))

  # not in Python code
  py <- list("b.py" = 'renv::restore()')
  out <- suppressMessages(repro_write_scripts(py, list(), NULL, root, lang = "Python",
                                              replace_env_calls = TRUE))
  expect_equal(out$env_calls_replaced, 0L)
})

test_that("repro_install_deps_docker passes the versions to its script and reports them", {
  skip_if_not_installed("processx")
  captured <- NULL
  local_mocked_bindings(.repro_docker_run_install = function(command, args, timeout, out_file, status_file,
                                                             install_deps, versions) {
    sandbox <- dirname(status_file)
    captured <<- readLines(file.path(sandbox, "install.R"))
    saveRDS(data.frame(package = "a", source = "cran", installed = TRUE, message = "",
                       version_installed = "1.0", version_note = ""),
            file.path(sandbox, ".install_results.rds"))
    writeLines("", out_file)
    list(status = 0L, timeout = FALSE)
  })
  deps <- data.frame(package = c("a", "b"), source = "cran", ref = NA_character_, version = c("1.0", NA))
  out <- repro_install_deps_docker(deps, withr::local_tempdir(), image = "x")
  # the script parses, with the version list in it
  expect_no_error(parse(text = captured))
  expect_true('.repro_docker_vers <- c("1.0", NA)' %in% captured)
  expect_true(any(grepl("remotes::install_version", captured, fixed = TRUE)))
  expect_true(any(grepl("/sandbox/.install_status", captured, fixed = TRUE)))
  # a bare image has no remotes, and before R 4.0 data.frame() makes factors
  expect_true(any(grepl('install.packages("remotes", lib = lib', captured, fixed = TRUE)))
  expect_true(any(grepl("version_note = note, via_archive = FALSE, stringsAsFactors = FALSE", captured, fixed = TRUE)))
  # the row the script reached, and one it did not
  expect_equal(out$package, c("a", "b"))
  expect_equal(out$installed, c(TRUE, FALSE))
  expect_equal(out$version_recorded, c("1.0", NA))
  expect_equal(out$version_installed, c("1.0", NA))
  expect_equal(out$version_note, c("", ""))
  # without a version column, as before
  out <- repro_install_deps_docker(deps[c("package", "source", "ref")], withr::local_tempdir(), image = "x")
  expect_true('.repro_docker_vers <- c(NA_character_, NA_character_)' %in% captured)
  expect_true(all(is.na(out$version_recorded)))
})

test_that(".repro_docker_resource_args always keeps R from writing a crash file", {
  withr::local_options(metacheck.docker_resource_limits = NULL)
  expect_equal(.repro_docker_resource_args(), c("--ulimit", "core=1"))
  withr::local_options(metacheck.docker_resource_limits = list(cpus = 2, memory_gb = 4))
  expect_equal(.repro_docker_resource_args(), c("--ulimit", "core=1", "--cpus", "2", "--memory", "4g"))
})

# A small installed package, for the load check.
local_test_package <- function(lib, name, version, imports = NULL, on_load = NULL) {
  dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  src <- file.path(withr::local_tempdir(.local_envir = parent.frame()), name)
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c(paste("Package:", name), paste("Version:", version), "Title: A Test", "Description: A test.",
               "License: GPL-3", "Author: A", "Maintainer: A <a@b.c>", if (!is.null(imports)) paste("Imports:", imports)),
             file.path(src, "DESCRIPTION"))
  writeLines(c(if (!is.null(imports)) paste0("import(", sub(" .*$", "", imports), ")"), "export(f)"), file.path(src, "NAMESPACE"))
  writeLines(c("f <- function() 1", if (!is.null(on_load)) paste0(".onLoad <- function(libname, pkgname) ", on_load)),
             file.path(src, "R", "f.R"))
  res <- processx::run(file.path(R.home("bin"), "R"),
                       c("CMD", "INSTALL", "--no-docs", "--no-test-load", "--no-multiarch", paste0("--library=", lib), src),
                       env = c("current", R_LIBS = lib), error_on_status = FALSE)
  expect_equal(res$status, 0L, info = res$stderr)
}

test_that(".repro_load_check replaces a recorded version that keeps a package from loading", {
  skip_on_cran()
  skip_if_not_installed("processx")
  root <- withr::local_tempdir()
  lib <- file.path(root, "lib"); main <- file.path(root, "main")
  local_test_package(main, "mcOld", "2.0")
  local_test_package(main, "mcNew", "1.0", imports = "mcOld (>= 2.0)")
  local_test_package(main, "mcFine", "3.0")
  local_test_package(lib, "mcOld", "1.0")
  # recorded, and fine next to what is there
  local_test_package(lib, "mcFine", "1.5")
  # recorded, breaks when it loads, and has no other copy to go back to
  local_test_package(lib, "mcBroken", "1.0", on_load = 'stop("broken on this R")')
  withr::local_envvar(R_LIBS = main)
  ir <- data.frame(package = c("mcNew", "mcOld", "mcFine", "mcBroken"), source = "cran", installed = TRUE,
                   message = "", category = NA_character_, version_recorded = c(NA, "1.0", "1.5", "1.0"),
                   version_installed = c("1.0", "1.0", "1.5", "1.0"), version_note = "")
  out <- suppressMessages(.repro_load_check(ir, lib, timeout = 300))

  # the current version of the package the error names, and the note says why
  expect_equal(out$version_installed[2], "2.0")
  expect_match(out$version_note[2], "keeps mcNew from loading", fixed = TRUE)
  expect_true(file.exists(file.path(main, "mcOld", "DESCRIPTION")))
  expect_false(file.exists(file.path(lib, "mcOld", "DESCRIPTION")))
  # a recorded version that is not at fault stays
  expect_equal(out$version_installed[3], "1.5")
  expect_equal(out$version_note[3], "")
  expect_true(file.exists(file.path(lib, "mcFine", "DESCRIPTION")))
  # a package that does not load counts as not installed
  expect_false(out$installed[4])
  expect_match(out$message[4], "the package does not load", fixed = TRUE)
  expect_match(out$message[4], "broken on this R", fixed = TRUE)
  expect_equal(out$category[4], "other")
  expect_true(all(out$installed[1:3]))
})

test_that(".repro_load_check does nothing without a recorded version in the library", {
  ir <- data.frame(package = "a", source = "cran", installed = TRUE, message = "", category = NA_character_,
                   version_recorded = "1.0", version_installed = "1.0", version_note = "")
  local_mocked_bindings(.repro_load_check_lines = function(...) stop("should not run"))
  expect_equal(.repro_load_check(ir, withr::local_tempdir()), ir)
})

test_that(".repro_install_order installs current versions before recorded ones", {
  deps <- data.frame(package = c("a", "b", "c", "d"), source = c("cran", "cran", "bioc", "github"),
                     version = c("1.0", NA, "3.1", "2.0"))
  expect_equal(.repro_install_order(deps)$package, c("b", "c", "a", "d"))
})

test_that(".repro_repos_lines replaces MRAN by the same date at Posit, and keeps other sources", {
  repos_after <- function(repos) {
    withr::local_options(repos = repos)
    eval(parse(text = .repro_repos_lines()))
    getOption("repos")
  }
  expect_equal(unname(repos_after(c(CRAN = "https://mran.microsoft.com/snapshot/2020-02-28"))),
               "https://packagemanager.posit.co/cran/2020-02-28")
  expect_equal(unname(repos_after(c(CRAN = "@CRAN@"))), "https://cloud.r-project.org")
  expect_equal(unname(repos_after(c(CRAN = "https://p3m.dev/cran/__linux__/jammy/2023-06-15"))),
               "https://p3m.dev/cran/__linux__/jammy/2023-06-15")
})

test_that("the load check script parses and runs from R 3.6", {
  lines <- .repro_load_check_lines("/rlib", c("a", "b"), "a", "/sandbox/results.rds")
  expect_no_error(parse(text = lines))
  # data.frame() makes factors before R 4.0
  expect_equal(sum(grepl("stringsAsFactors = FALSE", lines, fixed = TRUE)), 2)
  expect_false(any(grepl("deparse1|\\|>", lines)))
})

test_that(".repro_docker_uid runs the containers as the user running R on Linux", {
  withr::local_options(metacheck.docker_user = "123:456")
  expect_equal(.repro_docker_uid(), "123:456")
  withr::local_options(metacheck.docker_user = NULL)
  expected <- "1000:1000"
  if (identical(Sys.info()[["sysname"]], "Linux")) {
    ids <- c(system2("id", "-u", stdout = TRUE), system2("id", "-g", stdout = TRUE))
    if (ids[1] != "0") expected <- paste(ids, collapse = ":")
  }
  expect_equal(.repro_docker_uid(), expected)
})

test_that(".repro_docker_share opens a folder only to another user, and not through links", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"), "folder permissions are Linux's")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "sub"))
  writeLines("x", file.path(d, "sub", "a.csv"))
  outside <- withr::local_tempfile()
  writeLines("y", outside)
  Sys.chmod(outside, "600", use_umask = FALSE)
  file.symlink(outside, file.path(d, "link"))
  before <- file.mode(file.path(d, "sub"))
  # the containers run as the user running R: nothing changes
  withr::local_options(metacheck.docker_user = NULL)
  .repro_docker_share(d)
  expect_equal(file.mode(file.path(d, "sub")), before)
  # another user: the folders and files are opened, the linked file is not
  own <- paste(system2("id", "-u", stdout = TRUE), system2("id", "-g", stdout = TRUE), sep = ":")
  withr::local_options(metacheck.docker_user = if (own == "1000:1000") "1001:1001" else "1000:1000")
  expect_equal(.repro_docker_bind(d, "/sandbox"), paste0(d, ":/sandbox"))
  expect_equal(format(file.mode(c(d, file.path(d, "sub")))), c("777", "777"))
  expect_equal(format(file.mode(file.path(d, "sub", "a.csv"))), "666")
  expect_equal(format(file.mode(outside)), "600")
})

test_that("repro_install_deps_docker installs recorded versions in a real container", {
  skip_if_quick()
  skip_if_offline()
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  # callr loads R6 in the install process, so a failed install of R6 from
  # GitHub must not pass the load check.
  deps <- data.frame(package = c("R6", "callr", "R6"), source = c("cran", "cran", "github"),
                     ref = c(NA, NA, "r-lib/R6@0000000000000000000000000000000000000000"),
                     version = c("2.4.0", NA, "2.5.1"))
  out <- suppressMessages(repro_install_deps_docker(
    deps[1:2, ], withr::local_tempdir(),
    image = "ghcr.io/scienceverse/metacheck_r_large:latest", timeout = 300))
  expect_equal(out$installed, c(TRUE, TRUE))
  expect_equal(out$version_installed[1], "2.4.0")

  out <- suppressMessages(repro_install_deps_docker(
    deps[2:3, ], withr::local_tempdir(),
    image = "ghcr.io/scienceverse/metacheck_r_large:latest", timeout = 300))
  expect_equal(out$installed, c(TRUE, FALSE))
  expect_match(out$message[2], "did not put the package in the library", fixed = TRUE)
})

# repro_install_deps_docker() -- generated install.R script (issue #418) ----
# The bug this guards against was in pure string construction (deparse()
# wrapping to multiple lines for a long enough dependency vector, silently
# corrupting the c()-literal assignment), entirely before Docker is ever
# invoked -- so this reproduces that construction directly rather than
# calling repro_install_deps_docker() itself (which always shells out to a
# real `docker run`, with no seam this test suite mocks for a namespaced
# processx:: call -- see the local_mocked_bindings() docs on why mocking a
# `pkg::fun()` call from outside its own package is unreliable/discouraged).

test_that("repro_install_deps_docker's generated install.R parses for a long dependency list (#418)", {
  # 18 packages: the exact count that triggered deparse() wrapping to
  # multiple lines in the corpus paper that surfaced #418. Reproduces the
  # bug's actual mechanism directly (deparse() wrapping a long vector),
  # rather than going through the full repro_install_deps_docker() call
  # (which always shells out to a real `docker run` with no seam this test
  # suite mocks elsewhere) -- the bug and its fix are both entirely in this
  # header-construction step, before Docker is ever invoked.
  pkgs <- c("bayestestR", "brms", "broom", "broom.mixed", "dplyr", "ggdist",
           "ggh4x", "ggnewscale", "ggpubr", "ggridges", "janitor", "lubridate",
           "modelr", "moments", "PearsonDS", "scales", "tidybayes", "tidyverse")
  expect_gt(length(deparse(pkgs)), 1)  # sanity check: this vector DOES wrap

  srcs <- rep("cran", length(pkgs))

  # The exact construction repro_install_deps_docker() uses (see
  # R/reproducibility_check_docker.R) -- deparse1(), not deparse(), is the
  # fix: deparse() would silently tear the header lines below apart instead
  # of raising an error, so this asserts the OUTPUT is correct, not just
  # that deparse1() was called.
  header <- c(
    paste0(".repro_docker_pkgs <- ", deparse1(pkgs)),
    paste0(".repro_docker_srcs <- ", deparse1(srcs))
  )

  expect_length(header, 2)  # one line per assignment, never torn across lines
  expect_true(all(grepl("^\\.repro_docker_(pkgs|srcs) <- ", header)))

  env <- new.env()
  expect_no_error(eval(parse(text = header), envir = env))
  expect_equal(env$.repro_docker_pkgs, pkgs)
  expect_equal(env$.repro_docker_srcs, srcs)
})


# repro_run_scripts_docker() -- timeout stops the container, not just the
# CLI (issue #417) ----
# Issue #560: a slow answer from `docker info` is not the same as Docker not
# running. processx::run() is replaced, so these need the command but no daemon.

test_that("repro_docker_available tries again when Docker answers late (#560)", {
  skip_if_not_installed("processx")
  skip_if_not(nzchar(Sys.which("docker")), "docker command not found")

  waits <- c()
  local_mocked_bindings(.package = "processx", run = function(..., timeout) {
    waits <<- c(waits, timeout)
    late <- length(waits) < 3
    list(status = if (late) 2L else 0L, stdout = "", stderr = "", timeout = late)
  })
  expect_equal(repro_docker_available(), list(ok = TRUE, msg = ""))
  expect_equal(waits, c(15, 30, 60))
})

test_that("repro_docker_available says Docker did not answer after the last try (#560)", {
  skip_if_not_installed("processx")
  skip_if_not(nzchar(Sys.which("docker")), "docker command not found")

  waits <- c()
  local_mocked_bindings(.package = "processx", run = function(..., timeout) {
    waits <<- c(waits, timeout)
    list(status = 2L, stdout = "", stderr = "", timeout = TRUE)
  })
  res <- repro_docker_available()
  expect_false(res$ok)
  expect_match(res$msg, "did not answer")
  expect_no_match(res$msg, "not appear to be running")
  expect_equal(waits, c(15, 30, 60))
})

test_that("repro_docker_available does not try again when Docker gives an error (#560)", {
  skip_if_not_installed("processx")
  skip_if_not(nzchar(Sys.which("docker")), "docker command not found")

  waits <- c()
  local_mocked_bindings(.package = "processx", run = function(..., timeout) {
    waits <<- c(waits, timeout)
    list(status = 1L, stdout = "", stderr = "", timeout = FALSE)
  })
  res <- repro_docker_available()
  expect_false(res$ok)
  expect_match(res$msg, "not appear to be running")
  expect_equal(waits, 15)
})

# Real Docker, real timeout, gated the same way the module test file's
# execute = TRUE tests are (skip_if_quick(), Docker availability): the bug
# is specifically that processx::run(timeout = ...) killing the HOST-SIDE
# `docker run` CLI does not stop the CONTAINER it started, so this must
# actually run a container and check docker's own state afterward -- no
# in-process mock can stand in for "is a container still running".

test_that("repro_run_scripts_docker stops the container on timeout instead of orphaning it", {
  skip_if_quick()
  skip_if_not_installed("processx")
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  root <- withr::local_tempdir()
  script <- file.path(root, "sleeper.R")
  # sleeps far longer than the timeout below, so the run is GUARANTEED to
  # still be executing inside the container when the timeout fires --
  # a script that already exited on its own would never exercise the stop
  # path this test is checking.
  writeLines(c('Sys.sleep(120)', 'cat("should never reach here\\n")'), script)
  run_tbl <- data.frame(file_name = "sleeper.R", script_path = script, run_dir = root)

  out <- repro_run_scripts_docker(
    run_tbl, order = "sleeper.R", sandbox_root = root,
    image = "ghcr.io/scienceverse/metacheck_r_large:latest", timeout = 5)

  expect_equal(out$outcome, "timed_out")

  # The actual regression check: no container left `Up` after the timeout.
  # A short grace period (.repro_docker_stop()'s own "--time 5") plus
  # `docker ps` overhead means checking immediately can race the stop
  # command's own SIGTERM/SIGKILL sequence, so this polls briefly rather
  # than asserting instantaneously.
  container_gone <- FALSE
  for (i in 1:10) {
    running <- tryCatch(
      processx::run("docker", c("ps", "--filter", "name=repro_",
                                "--format", "{{.Names}}"),
                    error_on_status = FALSE, timeout = 10)$stdout,
      error = function(e) "")
    if (!nzchar(trimws(running))) { container_gone <- TRUE; break }
    Sys.sleep(1)
  }
  expect_true(container_gone,
             info = "a repro_* container was still `docker ps`-visible 10s after its own timeout fired")
})

test_that("repro_run_scripts_docker gives scripts writable cache folders and Quarto's pandoc (#523)", {
  skip_if_quick()
  skip_if_not_installed("processx")
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  root <- withr::local_tempdir()
  # renv::restore() stopped here: the home folder is read-only
  writeLines(c('d <- tools::R_user_dir("renv", "cache")',
               'dir.create(d, recursive = TRUE)',
               'stopifnot(dir.exists(d))',
               'stopifnot(rmarkdown::pandoc_available("2.8"))'),
             file.path(root, "cache.R"))
  run_tbl <- data.frame(file_name = "cache.R", script_path = file.path(root, "cache.R"),
                        run_dir = root)

  out <- repro_run_scripts_docker(
    run_tbl, order = "cache.R", sandbox_root = root,
    image = "ghcr.io/scienceverse/metacheck_r_large:latest", timeout = 120)

  expect_equal(out$outcome, "ran_ok", info = out$stderr)
})

test_that("a package install.packages() cannot find is reported as unavailable", {
  # Issue #421: install.packages() only WARNS for a name it cannot find, so
  # the load check reported "installed but package 'x' is not loadable" --
  # claiming an install that never happened, classified as uncategorised.
  # The warning is the real reason and is reported instead.
  warn <- "package 'list.packages' is not available for this version of R"
  msg <- metacheck:::.repro_not_loadable_msg("list.packages", c("other warning", warn))
  expect_equal(msg, warn)
  expect_equal(metacheck:::.repro_classify_install_message(msg), "cran_unavailable")

  # With the CRAN Archive retry's own result appended, as repro_install_deps()
  # does: a package the Archive has no folder for is not a network problem.
  msg2 <- paste0(msg, " (CRAN Archive retry also failed: package not found in the CRAN Archive)")
  expect_equal(metacheck:::.repro_classify_install_message(msg2), "cran_unavailable")

  # Without such a warning the original wording is kept.
  expect_equal(metacheck:::.repro_not_loadable_msg("pkg", character(0)),
               "installed but package 'pkg' is not loadable")
})

test_that("a package that does not load is reported with what R said", {
  skip_on_cran()
  skip_if_not_installed("processx")
  lib <- withr::local_tempdir()
  withr::defer(if ("mcLoads" %in% loadedNamespaces()) unloadNamespace("mcLoads"))
  local_test_package(lib, "mcNoLoad", "1.0", on_load = 'stop("libfoo.so.2:\n  cannot open")')
  reason <- metacheck:::.repro_load_error("mcNoLoad", lib)
  expect_match(reason, "libfoo.so.2: cannot open", fixed = TRUE)
  expect_equal(metacheck:::.repro_not_loadable_msg("mcNoLoad", character(0), reason),
               paste0("installed but package 'mcNoLoad' is not loadable: ", reason))
  # a package that loads has nothing to report
  local_test_package(lib, "mcLoads", "1.0")
  expect_equal(metacheck:::.repro_load_error("mcLoads", lib), "")
  # the install script in Docker cannot use this function, so it has its own copy
  script <- paste(deparse(body(repro_install_deps_docker)), collapse = "\n")
  expect_match(script, "is not loadable: ", fixed = TRUE)
})

test_that(".repro_classify_install_message does not call the install time limit a network issue", {
  expect_equal(metacheck:::.repro_classify_install_message("docker install timed out after 5s"), "other")
  expect_equal(metacheck:::.repro_classify_install_message("Timeout was reached: connection timed out"), "network")
})

test_that("a package found nowhere is reported as one the code does not say where to get (#389)", {
  msg <- paste0("package 'ecodeal' is not available for this version of R; found nowhere else: ",
                "not on Bioconductor, in the CRAN Archive, in our list of known places or on R-universe")
  expect_equal(.repro_classify_install_message(msg), "source_unknown")
  # a package that was looked for elsewhere and failed there is not unknown
  expect_equal(.repro_classify_install_message(
    "package 'x' is not available for this version of R; also tried elsewhere: the search of R-universe failed: timeout"),
    "network")
  ir <- data.frame(package = c("ecodeal", "dplyr", "rgdal"), installed = c(FALSE, TRUE, FALSE),
                   category = c("source_unknown", NA, "compile_failure"))
  expect_equal(.repro_source_unknown(ir), "ecodeal")
  expect_equal(.repro_source_unknown(ir[2:3, ]), character(0))
  expect_null(.repro_source_unknown_text(character(0)))
  expect_match(.repro_source_unknown_text("ecodeal"),
               "**1 package the code loads is not on CRAN or Bioconductor, and the code does not say where it comes from**: `ecodeal`", fixed = TRUE)
  expect_match(.repro_source_unknown_text(c("a", "b")), "where they come from**: `a`, `b`. ", fixed = TRUE)
  # the checklist says so for the script that needs it
  tb <- data.frame(file_name = "a.R", run_order = 1, outcome = "dependency_unavailable",
                   setwd_removed = 0, not_runnable_reason = NA, unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = "a.R", outcome = "dependency_unavailable", error = "",
                   stderr = "Error in library(ecodeal) : there is no package called 'ecodeal'")
  out <- .repro_checklist_section(tb, rr, install_missing = TRUE, source_unknown = "ecodeal")
  expect_match(out, "which is not on CRAN or Bioconductor, and the code does not say where it comes from", fixed = TRUE)
  expect_match(.repro_checklist_section(tb, rr, install_missing = TRUE), "which we could not install", fixed = TRUE)
})

test_that("repro_dependencies does not list a package-list variable as a package", {
  # Issue #421, end to end through the dependency list reproducibility_check
  # installs from.
  deps <- repro_dependencies(c(
    'list.packages <- c("activity", "bbmle")',
    'for (req.lib in list.packages) {',
    '  if (!require(req.lib, character.only = TRUE)) install.packages(req.lib)',
    '}'))
  expect_setequal(deps$package, c("activity", "bbmle"))
})


# repro_dependencies_py() ----

test_that("repro_dependencies_py finds import statements and tags base/pypi", {
  code <- c("import os", "import numpy as np", "from sklearn.linear_model import LinearRegression")
  deps <- repro_dependencies_py(code)
  expect_true(deps$base[deps$package == "os"])
  expect_false(deps$base[deps$package == "numpy"])
  expect_equal(deps$source[deps$package == "numpy"], "pypi")
})

test_that("repro_dependencies_py attaches a requirements.txt pin to the matching import", {
  code <- c("import numpy")
  manifest <- c("numpy==1.26.0", "# a comment", "")
  deps <- repro_dependencies_py(code, manifest_text = manifest)
  expect_equal(deps$ref[deps$package == "numpy"], "numpy==1.26.0")
})

test_that("repro_dependencies_py returns an empty frame for NULL/empty input", {
  empty_cols <- c("package", "source", "ref", "base")
  expect_equal(names(repro_dependencies_py(NULL)), empty_cols)
  expect_equal(nrow(repro_dependencies_py(character(0))), 0)
})

test_that("repro_dependencies_py pools a list of files, preferring a manifest pin over import-only", {
  deps <- repro_dependencies_py(list(c("import numpy"), c("import numpy")),
                                manifest_text = "numpy==1.26.0")
  expect_equal(nrow(deps[deps$package == "numpy", ]), 1)
  expect_equal(deps$ref[deps$package == "numpy"], "numpy==1.26.0")
})


# repro_install_deps_py() ----

test_that("repro_install_deps_py returns an empty frame for no dependencies", {
  empty_cols <- c("package", "source", "installed", "message", "via_archive", "category")
  out <- repro_install_deps_py(data.frame(), lib_dir = withr::local_tempdir())
  expect_equal(names(out), empty_cols)
  expect_equal(nrow(out), 0)
})

test_that("repro_install_deps_py errors with a clear message when python is not found", {
  # Neither the given name nor the `python3` fallback works.
  local_mocked_bindings(.repro_find_python = function(python_bin = "python") "")
  install_deps <- data.frame(package = "numpy", source = "pypi", ref = NA_character_)
  expect_error(
    repro_install_deps_py(install_deps, lib_dir = withr::local_tempdir(),
                          python_bin = "definitely_not_a_real_python_binary_xyz"),
    "was not found on PATH")
})

test_that(".repro_find_python falls back to python3 when the given name is not found", {
  py3 <- .repro_find_python("python3")
  skip_if_not(nzchar(py3), "no working python3 interpreter")
  expect_equal(.repro_find_python("definitely_not_a_real_python_binary_xyz"), py3)
})


# repro_run_scripts_py() error_type / dependency_unavailable classification ----
# Unit tests for the fix reconciling the process backend's error_type string
# with the docker backend's (see R/reproducibility_check_python.R's own
# comment at the change site) -- both backends must now report the SAME
# strings ("undefined_variable", "dependency_unavailable") for the same
# underlying stderr shape, not diverge (process: raw "NameError"; docker:
# "undefined_variable") the way they did before this fix.

test_that("repro_run_scripts_py reconciles a NameError to error_type 'undefined_variable'", {
  skip_if_not(nzchar(Sys.which("python")) || nzchar(Sys.which("python3")),
             "no python interpreter available")
  py_bin <- if (nzchar(Sys.which("python"))) "python" else "python3"

  root <- withr::local_tempdir()
  script <- file.path(root, "undefined_var.py")
  writeLines("print(some_var_no_script_defines)", script)
  run_tbl <- data.frame(file_name = "undefined_var.py", script_path = script, run_dir = root)

  out <- repro_run_scripts_py(run_tbl, order = "undefined_var.py",
                              python_bin = py_bin, timeout = 60)

  expect_equal(out$outcome, "errored")
  # NOT the raw "NameError" string -- reconciled to the same
  # "undefined_variable" string the docker backend already used.
  expect_equal(out$error_type, "undefined_variable")
  expect_equal(out$undefined_var, "some_var_no_script_defines")
})

test_that("repro_run_scripts_py classifies a missing import as dependency_unavailable when listed in failed_deps", {
  skip_if_not(nzchar(Sys.which("python")) || nzchar(Sys.which("python3")),
             "no python interpreter available")
  py_bin <- if (nzchar(Sys.which("python"))) "python" else "python3"

  root <- withr::local_tempdir()
  script <- file.path(root, "needs_pkg.py")
  writeLines("import this_package_does_not_exist_xyz123", script)
  run_tbl <- data.frame(file_name = "needs_pkg.py", script_path = script, run_dir = root)

  out <- repro_run_scripts_py(run_tbl, order = "needs_pkg.py", python_bin = py_bin,
                              timeout = 60,
                              failed_deps = "this_package_does_not_exist_xyz123")

  expect_equal(out$outcome, "dependency_unavailable")
  expect_equal(out$error_type, "dependency_unavailable")
})

test_that("repro_run_scripts_py does NOT classify a missing import as dependency_unavailable when it is not in failed_deps", {
  # failed_deps is empty here -- repro_install_deps_py() was never even
  # asked about this package, so the missing import must be reported as an
  # ordinary error, not (incorrectly) blamed on install failure.
  skip_if_not(nzchar(Sys.which("python")) || nzchar(Sys.which("python3")),
             "no python interpreter available")
  py_bin <- if (nzchar(Sys.which("python"))) "python" else "python3"

  root <- withr::local_tempdir()
  script <- file.path(root, "needs_pkg2.py")
  writeLines("import this_package_does_not_exist_xyz123", script)
  run_tbl <- data.frame(file_name = "needs_pkg2.py", script_path = script, run_dir = root)

  out <- repro_run_scripts_py(run_tbl, order = "needs_pkg2.py", python_bin = py_bin,
                              timeout = 60)

  expect_equal(out$outcome, "errored")
  expect_false(identical(out$error_type, "dependency_unavailable"))
})


# Python statistical-output extraction (py_stat_output mechanism) ----
# Unit-level test of the EXTRACTION step the module's py_stat_output block
# (inst/modules/reproducibility_check.R) applies to a Python script's
# captured stdout -- calling .ipynb_stat_line()/.ipynb_is_noise() directly
# against a FAKE but correctly-shaped stdout string, so this runs with no
# Python interpreter and no scipy installation required at all (a real,
# scipy-dependent end-to-end execute = TRUE test also exists, gated
# separately, in test-module-reproducibility_check.R).

test_that("a fake scipy TtestResult one-liner in stdout parses via .ipynb_stat_line()", {
  so <- "TtestResult(statistic=4.89, pvalue=6.75e-05, df=22)"
  lines <- strsplit(so, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  expect_false(.ipynb_is_noise(lines))

  tabs <- .ipynb_stat_line(lines)
  expect_false(is.null(tabs))
  expect_length(tabs, 1)
  expect_equal(tabs[[1]]$call_fn, "TtestResult")

  long <- stat_results_long(tabs, paper_id = "test_paper", source_file = "ttest.py")
  expect_gt(nrow(long), 0)
  expect_true("statistic" %in% tolower(long$statistic) ||
             any(grepl("statistic", tolower(long$statistic))))

  json <- stat_output_json(tabs, paper_id = "test_paper", source_file = "ttest.py")
  expect_true(!is.null(json))
})

test_that("match_reported_output() does not error against a stat_output list with a python_output-sourced entry", {
  so <- "TtestResult(statistic=4.89, pvalue=6.75e-05, df=22)"
  lines <- strsplit(so, "\n", fixed = TRUE)[[1]]
  tabs <- .ipynb_stat_line(lines)
  stat_output <- list(list(
    file = "ttest.py", n_tables = length(tabs), source = "python_output",
    json = stat_output_json(tabs, paper_id = "test_paper", source_file = "ttest.py"),
    long = stat_results_long(tabs, paper_id = "test_paper", source_file = "ttest.py")
  ))

  paper <- test_paper("t(22) = 4.89, p < .001.")
  matched <- tryCatch(
    match_reported_output(paper, stat_output, include_tables = TRUE),
    error = function(e) e)
  expect_false(inherits(matched, "error"))
  expect_true(is.data.frame(matched))
})

test_that("a noise-only Python stdout block does not parse (no raw-text fallback)", {
  # Consistent with read_r_output()'s own behaviour on unparseable console
  # output (R/r-output.R: it returns list(), never a raw-text fallback
  # column) -- the Python path deliberately matches that, rather than
  # inventing a raw-text fallback of its own.
  so <- "<Figure size 640x480 with 1 Axes>"
  lines <- strsplit(so, "\n", fixed = TRUE)[[1]]
  expect_true(.ipynb_is_noise(lines))
})

# A redirected write names the output folder relative to the sandbox root: the
# root is a host path, and in the Docker sandbox it is mounted at /sandbox, so
# an absolute host path failed inside the container (#502).
.write_script_502 <- function(root) {
  code_list <- list("analysis.R" = c(
    'd <- data.frame(condition = c("a", "b"), score = c(1, 2))',
    'write.csv(d, "data/means.csv", row.names = FALSE)',
    'cat("wrote the file\n")'))
  rewrite_list <- list("analysis.R" = data.frame(
    ref = character(0), basename = character(0), matched = logical(0),
    target = character(0), ambiguous = logical(0), n_candidates = integer(0),
    is_call = logical(0)))
  plan <- data.frame(file_name = "analysis.R", target_path = "code/analysis.R")
  dir.create(file.path(root, "output"), showWarnings = FALSE)
  repro_write_scripts(code_list, rewrite_list, plan, root)
}

test_that("repro_write_scripts redirects writes to output/ relative to the sandbox root (#502)", {
  root <- withr::local_tempdir()
  out <- .write_script_502(root)
  written <- paste(readLines(out$script_path[1]), collapse = "\n")
  expect_match(written, '"output/means.csv"', fixed = TRUE)
  expect_false(grepl(gsub("\\\\", "/", root), written, fixed = TRUE))
  expect_false(grepl(basename(root), written, fixed = TRUE))
})

test_that("a script that writes a file succeeds in the Docker sandbox (#502)", {
  # on GitHub's Linux runner (uid 1001) the container's user (1000) cannot
  # write in the sandbox folder; Docker Desktop does not check owners
  skip_on_ci()
  skip_if_quick()
  skip_if_not_installed("processx")
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  root <- withr::local_tempdir()
  run_tbl <- .write_script_502(root)
  out <- repro_run_scripts_docker(
    run_tbl, order = "analysis.R", sandbox_root = root,
    image = "ghcr.io/scienceverse/metacheck_r_large:latest", timeout = 120)
  # on failure, show why: on GitHub (2026-10-20) this came back "errored"
  # with no reason in the log
  expect_equal(out$outcome, "ran_ok",
               info = paste0("error: ", out$error, "\nstderr: ", out$stderr,
                             "\nstdout: ", out$stdout))
  # written inside the container, found in the host's output folder
  expect_true(file.exists(file.path(root, "output", "means.csv")))
})

# -- the package layout ---------------------------------------------------------
# The Psych-DS layout gives the code only the data files it recognised, moved
# into Psych-DS folders, so code that reads a folder of scan files, or imports
# its own helper module from a subfolder, failed for files that are in the
# package. The package layout copies every file to its own path.

test_that(".repro_package_plan gives every file its own path", {
  structure_df <- data.frame(
    file_name = c("log.csv", "log.csv", "helpers.py", "a.csv"),
    file_path = c("raw_data/scan1/log.csv", "raw_data/scan2/log.csv",
                  "code/utils/helpers.py", NA),
    file_location = c("/x/1/log.csv", "/x/2/log.csv", "/x/helpers.py", "/x/a.csv"))
  plan <- .repro_package_plan(structure_df)
  expect_equal(plan$target_path, c("raw_data/scan1/log.csv", "raw_data/scan2/log.csv",
                                   "code/utils/helpers.py", "a.csv"))
  # each row keeps its own file, also when names repeat
  expect_equal(plan$source[1:2], c("/x/1/log.csv", "/x/2/log.csv"))
  expect_equal(nrow(.repro_package_plan(NULL)), 0)
})

test_that("repro_materialize_layout copies each file of the package layout from its own source", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "1")); dir.create(file.path(d, "2"))
  writeLines("one", file.path(d, "1", "log.csv"))
  writeLines("two", file.path(d, "2", "log.csv"))
  structure_df <- data.frame(file_name = c("log.csv", "log.csv"),
                             file_path = c("scan1/log.csv", "scan2/log.csv"),
                             file_location = file.path(d, c("1", "2"), "log.csv"))
  root <- withr::local_tempdir()
  repro_materialize_layout(.repro_package_plan(structure_df), structure_df, root)
  # looked up by name, both got the first file's content
  expect_equal(readLines(file.path(root, "scan1/log.csv")), "one")
  expect_equal(readLines(file.path(root, "scan2/log.csv")), "two")
})

test_that("repro_write_scripts writes a script to its own path when given one", {
  root <- withr::local_tempdir()
  code <- list(`__init__.py` = "x = 1", `__init__.py` = "y = 2")
  rw <- list(`__init__.py` = repro_rewrite_paths("", "x", NULL))
  out <- repro_write_scripts(code, rw, NULL, root, lang = "Python",
                             script_paths = c("a/__init__.py", "b/__init__.py"))
  expect_equal(readLines(file.path(root, "a/__init__.py")), "x = 1")
  expect_equal(readLines(file.path(root, "b/__init__.py")), "y = 2")
})

test_that(".repro_keep_working_refs leaves a path that works in the package as written", {
  plan <- data.frame(file_name = c("log.csv", "log.csv", "notes.txt"),
                     target_path = c("raw_data/scan1/log.csv", "raw_data/scan2/log.csv",
                                     "raw_data/notes.txt"))
  code <- c("a = open('raw_data/scan1/log.csv')",
            "b = open('C:/Users/author/project/raw_data/notes.txt')")
  rw <- repro_rewrite_paths(code, "code/main.py", plan, "Python")
  rw <- .repro_keep_working_refs(rw, plan)
  works <- rw[rw$ref == "raw_data/scan1/log.csv", ]
  expect_true(works$as_written)
  expect_false(works$ambiguous)       # 2 files are named log.csv
  expect_equal(works$target, "raw_data/scan1/log.csv")
  abs <- rw[rw$basename == "notes.txt", ]
  expect_false(abs$as_written)        # rewritten to where the file is
  expect_equal(abs$target, "raw_data/notes.txt")
})

test_that(".repro_py_imported finds modules, not scripts", {
  code <- list(
    main.py = c("import os", "from utils import helpers", "from modules.math_class import Math"),
    helpers.py = "def f(): return 1",
    `__init__.py` = character(0),
    math_class.py = "class Math: pass",
    tool.py = c("def g(): pass", "if __name__ == '__main__':", "    g()"),
    alone.py = "print(1)")
  paths <- c("code/main.py", "code/utils/helpers.py", "code/utils/__init__.py",
             "code/modules/math_class.py", "code/tool.py", "code/alone.py")
  imp <- .repro_py_imported(code, paths)
  expect_setequal(names(imp), c("helpers.py", "__init__.py", "math_class.py"))
  expect_equal(imp[["helpers.py"]], "main.py")
  # a file with a __main__ block, or that nothing imports, is a script
  code$main.py <- c(code$main.py, "import tool")
  expect_false("tool.py" %in% names(.repro_py_imported(code, paths)))
  # a notebook cannot be imported: `import audio` names a module, not audio.ipynb (#568)
  nb <- list(demo.ipynb = c("from opensoundscape import audio", "import audio"), audio.ipynb = "x = 1")
  expect_length(.repro_py_imported(nb, c("docs/demo.ipynb", "docs/audio.ipynb")), 0)
})

test_that("the package layout runs code that reads folders and imports its own modules", {
  skip_if_quick()
  skip_if_not(nzchar(Sys.which("python")) || nzchar(Sys.which("python3")),
              "no python interpreter available")
  llm_use(FALSE)
  pkg <- withr::local_tempdir()
  w <- function(path, lines) {
    dir.create(dirname(file.path(pkg, path)), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, file.path(pkg, path))
  }
  w("raw_data/scan1/log.csv", c("a,b", "1,2"))
  w("raw_data/scan2/log.csv", c("a,b", "3,4"))
  w("raw_data/notes.txt", "some notes")
  w("code/utils/__init__.py", "")
  w("code/utils/helpers.py", c("def first_log():",
                               "    return open('raw_data/scan1/log.csv').read().splitlines()[1]"))
  w("code/main.py", c("import os", "from utils import helpers",
                      "print(sorted(os.listdir('raw_data/scan1')))", "print(helpers.first_log())"))
  w("code/other.py", "print(open('C:/Users/author/project/raw_data/notes.txt').read())")

  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = pkg,
                                    local_only = TRUE, execute = TRUE))
  oc <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(oc[["main.py"]], "ran_ok")
  expect_equal(oc[["other.py"]], "ran_ok")      # absolute path rewritten
  expect_equal(oc[["helpers.py"]], "run_by_import")
  expect_match(mo$summary_text, "2 of 2 Python scripts ran without error", fixed = TRUE)
})

test_that(".repro_keep_working_refs rewrites only what cannot work as written", {
  plan <- data.frame(file_name = c("x.csv", "helpers.R", "scores.csv"),
                     target_path = c("processed_data/fig7/x.csv", "code/helpers.R",
                                     "raw_data/scores.csv"))
  code <- c("a <- read.csv(file.path(base, 'fig7/x.csv'))",
            "source('helpers.R')",
            "d <- read.csv('../raw_data/scores.csv')")
  rw <- repro_rewrite_paths(code, "analysis.R", plan, "R")
  rw <- .repro_keep_working_refs(rw, plan, "code/analysis.R")
  # a partial path the code joins with a folder: left as written (rewriting
  # it gave processed_data/fig7/processed_data/fig7/x.csv)
  part <- rw[rw$ref == "fig7/x.csv", ]
  expect_true(part$as_written)
  expect_equal(part$target, "fig7/x.csv")
  # relative to the script's own folder: the path from the top
  expect_equal(rw$target[rw$ref == "helpers.R"], "code/helpers.R")
  expect_equal(rw$target[rw$ref == "../raw_data/scores.csv"], "raw_data/scores.csv")
  expect_false(any(rw$as_written[rw$ref %in% c("helpers.R", "../raw_data/scores.csv")]))
})

test_that(".repro_keep_working_refs points a path that works nowhere to the file of its name (#568)", {
  # the authors moved or left out a folder when they deposited
  plan <- data.frame(file_name = c("VEG.csv", "forest_age.R"),
                     target_path = c("exploratory/VEG.csv", "exploratory/forest_age.R"))
  code <- c('d <- read.csv("./data/VEG.csv")', 'e <- read.csv("data/absent.csv")')
  rw <- repro_rewrite_paths(code, "forest_age.R", plan, "R", script_path = "exploratory/forest_age.R")
  rw <- .repro_keep_working_refs(rw, plan, "exploratory/forest_age.R", "exploratory")
  veg <- rw[rw$ref == "./data/VEG.csv", ]
  expect_false(veg$as_written)
  # counted from the folder the script runs from
  expect_equal(veg$target, "VEG.csv")
  # no file of its name anywhere: as written, and a missing input
  absent <- rw[rw$ref == "data/absent.csv", ]
  expect_true(absent$as_written)
  expect_false(absent$matched)
  # the report tells the authors
  txt <- .repro_path_fix_text(list(forest_age.R = rw), execute = TRUE)
  expect_match(txt, "**1 data path in the code does not work in the repository as deposited.**", fixed = TRUE)
  expect_match(txt, "`./data/VEG.csv` to `VEG.csv` (in forest_age.R)", fixed = TRUE)
  expect_match(.repro_path_fix_text(list(a.R = rw[0, ])), "Every data path", fixed = TRUE)
})

test_that(".repro_norm_rel resolves . and ..", {
  expect_equal(.repro_norm_rel("code/../raw_data/./x.csv"), "raw_data/x.csv")
  expect_equal(.repro_norm_rel("code\\x.R"), "code/x.R")
  expect_true(is.na(.repro_norm_rel("../x.csv")))
})

test_that(".repro_relocate_relative_paths counts paths from the script's own or .Rproj folder (#523)", {
  plan <- data.frame(file_name = c("Study2.Rproj", "raw.csv", "x.csv", "top.csv"),
                     target_path = c("Study 2/Study2.Rproj", "Study 2/Data/raw.csv",
                                     "processed_data/fig7/x.csv", "Data/top.csv"))
  code <- c("write.csv(d, 'Output/made.csv')",
            "p <- read.csv(\"Output/made.csv\")",
            "write.csv(d, '../Output/up.csv')",
            "a <- read.csv(file.path(base, 'fig7/x.csv'))",
            "b <- read.csv('Data/top.csv')",
            "u <- 'https://osf.io/abc/x.csv'",
            "dir <- 'Output/'")
  # a script in a subfolder of the .Rproj folder; Output/ is in the .Rproj folder
  plan2 <- rbind(plan, data.frame(file_name = "old.csv",
                                  target_path = "Study 2/Output/old.csv"))
  out <- .repro_relocate_relative_paths(code, "Study 2/scripts/clean.R", plan2)
  expect_equal(out[1], "write.csv(d, 'Study 2/Output/made.csv')")
  expect_equal(out[2], "p <- read.csv(\"Study 2/Output/made.csv\")")
  expect_equal(out[3], "write.csv(d, 'Study 2/Output/up.csv')")
  # a folder that exists from the top, a joined piece and a web address stay
  expect_equal(out[4:6], code[4:6])
  expect_equal(out[7], "dir <- 'Study 2/Output/'")

  # without a .Rproj file, the script's own folder
  out <- .repro_relocate_relative_paths(code[1], "Study 2/clean.R",
                                        plan2[plan2$file_name != "Study2.Rproj", ])
  expect_equal(out, "write.csv(d, 'Study 2/Output/made.csv')")
  # a script at the top, or no plan: unchanged
  expect_equal(.repro_relocate_relative_paths(code, "clean.R", plan2), code)
  expect_equal(.repro_relocate_relative_paths(code, "Study 2/clean.R", NULL), code)
})

test_that(".repro_output_head keeps the first lines of long output (#523, item 2)", {
  txt <- paste(sprintf("line %d", 1:10), collapse = "\n")
  out <- .repro_output_head(txt, 4)
  expect_equal(as.character(out), "line 1\nline 2\nline 3\nline 4")
  expect_equal(attr(out, "n_lines"), 10)
  expect_identical(.repro_output_head(txt, 10), txt)
  # a vector of lines, as the Python path has
  out <- .repro_output_head(c("a", "b", "c"), 2)
  expect_equal(as.character(out), c("a", "b"))
  expect_equal(attr(out, "n_lines"), 3)
  expect_identical(.repro_output_head(NA_character_, 2), NA_character_)
  expect_identical(.repro_output_head("", 2), "")
  withr::local_options(metacheck.repro_output_max_lines = 3L)
  expect_equal(attr(.repro_output_head(txt), "n_lines"), 10)

  expect_null(.repro_output_cut_note(integer(0)))
  expect_match(.repro_output_cut_note(c(V1_Modeling.R = 2300000L), 20000L),
               "first 20,000 lines of the output of `V1_Modeling.R` (2,300,000 lines)",
               fixed = TRUE)
})

test_that("reproducibility_check searches only the first lines of long output (#523, item 2)", {
  skip_if_quick()
  llm_use(FALSE)
  withr::local_options(metacheck.repro_output_max_lines = 2000L)
  pkg <- withr::local_tempdir()
  writeLines("for (i in 1:5000) cat('row', i, 'value', i / 7, '\\n')",
             file.path(pkg, "chatty.R"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = pkg,
                                    local_only = TRUE, execute = TRUE))
  expect_equal(mo$run_results$outcome, "ran_ok")
  report <- paste(unlist(mo$report), collapse = "\n")
  # 5,000 lines and the echoed command
  expect_match(report, "first 2,000 lines of the output of `chatty.R` (5,00", fixed = TRUE)
})

test_that("the package layout runs scripts that write next to themselves (#523)", {
  skip_if_quick()
  llm_use(FALSE)
  pkg <- withr::local_tempdir()
  for (s in c("Study 2", "Study 3")) {
    dir.create(file.path(pkg, s, "Data"), recursive = TRUE)
    writeLines("Version: 1.0", file.path(pkg, s, paste0(sub(" ", "", s), ".Rproj")))
    writeLines(c("a,b", "1,2"), file.path(pkg, s, "Data", "raw.csv"))
  }
  writeLines(c("d <- read.csv('Data/raw.csv')",
               "write.csv(d, 'Data/processed.csv', row.names = FALSE)"),
             file.path(pkg, "Study 2", "1_clean.R"))
  writeLines(c("p <- read.csv('Data/processed.csv')", "print(nrow(p))"),
             file.path(pkg, "Study 2", "2_analyse.R"))

  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = pkg,
                                    local_only = TRUE, execute = TRUE))
  oc <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(oc[["1_clean.R"]], "ran_ok")
  expect_equal(oc[["2_analyse.R"]], "ran_ok")
})

test_that("repro_write_scripts leaves writes in place in the package layout", {
  root <- withr::local_tempdir()
  code <- list(a.R = c("write.csv(d, 'raw_data/made.csv')",
                       "write.csv(d, 'C:/Users/author/out.csv')"))
  rw <- list(a.R = repro_rewrite_paths(code$a.R, "a.R", NULL))
  repro_write_scripts(code, rw, NULL, root, writes_in_place = TRUE)
  txt <- readLines(file.path(root, "a.R"))
  expect_match(txt[1], "'raw_data/made.csv'", fixed = TRUE)
  expect_false(grepl("C:/Users/author", txt[2], fixed = TRUE))
  expect_match(txt[2], "output/", fixed = TRUE)
})

test_that("repro_materialize_layout makes the copy writable", {
  skip_on_os("windows")   # file modes
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "a.csv"))
  Sys.chmod(file.path(d, "a.csv"), "0444")
  structure_df <- data.frame(file_name = "a.csv", file_path = "raw_data/a.csv",
                             file_location = file.path(d, "a.csv"))
  root <- withr::local_tempdir()
  repro_materialize_layout(.repro_package_plan(structure_df), structure_df, root)
  expect_equal(as.character(file.mode(file.path(root, "raw_data/a.csv"))), "666")
})

test_that(".repro_py_manifest_pins reads only pyproject.toml's dependencies (U236)", {
  # every quoted string was a requirement (name, version, description), Poetry's
  # "^1.26" became "numpy^1.26" (refused by pip), and a pyproject.toml with a
  # string not starting like a package name stopped the function
  pep621 <- c('[project]', 'name = "analysis"', 'version = "0.1"',
              'description = "Code for the paper"', 'requires-python = ">=3.9"',
              'dependencies = [', '  "numpy>=1.24",', '  "pandas",', ']')
  expect_equal(.repro_py_manifest_pins(pep621)$requirement, c("numpy>=1.24", "pandas"))
  poetry <- c('[tool.poetry]', 'name = "analysis"', '[tool.poetry.dependencies]',
              'python = "^3.9"', 'numpy = "^1.26"', 'scipy = "~1.11"', 'pandas = "*"',
              'statsmodels = { version = "^0.14", optional = true }', 'seaborn = "0.13.2"')
  expect_equal(.repro_py_manifest_pins(poetry)$requirement,
               c("numpy>=1.26,<2.0", "scipy>=1.11,<1.12", "pandas",
                 "statsmodels>=0.14,<0.15", "seaborn==0.13.2"))
  # requirements.txt: inline comments and options are dropped
  expect_equal(.repro_py_manifest_pins(c("numpy==1.26  # pinned", "pandas>=2.0 --hash=sha256:abc"))$requirement,
               c("numpy==1.26", "pandas>=2.0"))
})

test_that("match_reported_output: missing or empty test_id, Inf values, a paper list (U142)", {
  # a missing test_id beside a jamovi residuals row made a site named NA and
  # stopped the call; "" could not be looked up; Inf compared as NA
  eq <- data.frame(text_id = 1, grp_id = 1, lhs = c("F", "p"), df = c("(2, 50)", NA),
                   comp = c("=", "="), rhs = c("4.20", ".020"), paper_id = "p")
  out <- data.frame(source_file = "a.R", analysis = "anova",
                    test_id = c("anova_group", "anova_residuals", NA, ""),
                    statistic = c("F", "df", "F", "F"), value = c("4.2", "50", "Inf", "4.2"),
                    family = c("F", "df", "F", "F"), row_label = c("group", "residuals", NA, NA))
  expect_no_error(match_reported_output(eq, out))
  expect_error(match_reported_output(paperlist(test_paper(), test_paper()), out),
               "call it for each paper")
  # a list of one paper is that paper, as module_run() passes it on
  expect_equal(match_reported_output(paperlist(test_paper()), out),
               match_reported_output(test_paper(), out))
})

test_that("repro_missing_inputs reports a withheld file without a file_size column (U135)", {
  # without a file_size column the function stopped
  plan <- data.frame(file_name = character(0), target_path = character(0))
  structure_df <- data.frame(file_name = character(0), file_location = character(0))
  out <- repro_missing_inputs("big.csv", plan, structure_df,
                              skipped = data.frame(file_name = "big.csv"))
  expect_equal(out$status, "withheld_size")
})

test_that("a path-building call kept as written is not put in quotes (tue_datamanager_check#4, item 9)", {
  # its target was the call itself, written as "sprintf(...)": unparseable
  code <- c('wd <- "/Volumes/author/project"',
            'sheets <- sprintf("%s/Data_Dictionary_Final.xlsx", wd)')
  plan <- data.frame(file_name = "Data_Dictionary_Final.xlsx",
                     target_path = "Data_Dictionary_Final.xlsx")
  rw <- .repro_keep_working_refs(repro_rewrite_paths(code, "02-scripts/a.R", plan, "R"),
                                 plan, "02-scripts/a.R")
  out <- repro_write_scripts(list("a.R" = code), list("a.R" = rw), plan, withr::local_tempdir())
  written <- readLines(out$script_path[1])
  expect_true(code[2] %in% written)
  expect_no_error(parse(text = written))
})

test_that("repro_run_scripts starts no script once the time for all scripts has run out (Lakens/tue_datamanager_check#4, item 6)", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  writeLines("Sys.sleep(60)", file.path(root, "slow.R"))
  writeLines("x <- 1", file.path(root, "next.R"))
  run_tbl <- data.frame(file_name = c("slow.R", "next.R"),
                        script_path = file.path(root, c("slow.R", "next.R")),
                        run_dir = root)
  # the first script is stopped when the time runs out, not after its own
  # 60 seconds; the second is not started
  t0 <- Sys.time()
  out <- repro_run_scripts(run_tbl, order = c("slow.R", "next.R"), timeout = 60,
                           deadline = Sys.time() + 5)
  expect_equal(out$outcome, c("timed_out", "skipped_time_limit"))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 45)
  expect_match(out$error[[2]], "time for all scripts together")
  # no limit
  expect_null(.repro_deadline(NULL))
  expect_null(.repro_deadline(Inf))
  expect_equal(.repro_time_left(NULL), Inf)
})

test_that("library() in a script does not give an S3 warning from the capture code (Lakens/tue_datamanager_check#5, item 4)", {
  expect_no_warning(.r_call_fun_arg("library(stats)"))
  # an aggregating generic still gives its FUN
  expect_equal(.r_call_fun_arg("aggregate(d$y, list(d$g), mean)"), "mean")
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  writeLines(c("library(stats)", "x <- 1"), file.path(root, "a.R"))
  run_tbl <- data.frame(file_name = "a.R", script_path = file.path(root, "a.R"), run_dir = root)
  out <- repro_run_scripts(run_tbl, order = "a.R", timeout = 60)
  expect_equal(out$outcome, "ran_ok")
  expect_false(grepl("appears not to be S3 generic", paste(out$stdout, out$stderr), fixed = TRUE))
})

test_that("the container's capture code knows the statistic names (found with Lakens/tue_datamanager_check#5, item 4)", {
  # the helpers as the container gets them: text, run in a fresh environment
  e <- new.env(parent = globalenv())
  eval(parse(text = .repro_docker_capture_preamble()), envir = e)
  expect_equal(e$stato_type_column("mean")$termSource, "STATO")
  d <- data.frame(y = c(1, 2, 3, 4), g = c("a", "a", "b", "b"))
  expect_false(is.null(e$.r_capture_value(aggregate(y ~ g, d, mean), "aggregate(y ~ g, d, mean)")))
})

test_that("a file found only inside an archive is a missing input in the package layout (#520, item 2)", {
  # OSF t69jd: the script reads "data/download 1a/x.csv", which the package
  # has only inside data.zip
  structure_df <- data.frame(
    file_name = c("Humor study 4a.csv", "main.R", "notes.csv"),
    file_path = c("data.zip/data/download 1a/Humor study 4a.csv", "main.R", "data/notes.csv"),
    file_location = c(NA, NA, NA))
  plan <- .repro_package_plan(structure_df)
  code <- c("d <- data.table::fread('data/download 1a/Humor study 4a.csv')",
            "n <- read.csv('data/notes.csv')",
            "o <- read.csv(file.path(base, 'download 1a/Humor study 4a.csv'))")
  rw <- .repro_keep_working_refs(repro_rewrite_paths(code, "main.R", plan, "R"), plan, "main.R")
  inside <- rw[rw$basename == "Humor study 4a.csv", ]
  # both the full path and the part a folder of the code's own is added to
  expect_true(all(!inside$matched))
  expect_true(all(!inside$as_written))
  # a file at its own path still works as written
  expect_true(rw$as_written[rw$basename == "notes.csv"])

  mi <- repro_missing_inputs("Humor study 4a.csv", plan, structure_df)
  expect_equal(mi$status, "in_archive")
  expect_match(mi$detail, "only inside data.zip; unpack it so the code finds it", fixed = TRUE)
  # a name that also exists outside the archive is not "in_archive"
  both <- rbind(structure_df, data.frame(file_name = "Humor study 4a.csv",
                                          file_path = "Humor study 4a.csv", file_location = NA))
  expect_equal(repro_missing_inputs("Humor study 4a.csv", plan, both)$status, "in_repo_not_downloaded")
})

test_that("a stored result is read from the right-hand side of its assignment (#549)", {
  expect_equal(.r_call_fun_arg("means <- sapply(d[, c('x', 'y')], mean)"), "mean")
  expect_equal(.r_call_fun_arg("(s = tapply(d$y, d$g, sd))"), "sd")
  expect_equal(.r_call_fun_arg("a <- b <- aggregate(y ~ g, d, median)"), "median")
  expect_true(is.na(.r_call_fun_arg("x <- 1")))
  d <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 6, 9))
  rec <- .r_capture_value(sapply(d, mean), "means <- sapply(d, mean)")
  expect_equal(rec$analysis, "mean by group")
  expect_equal(rec$rows[[2]]$stats$mean, 5.25)
})

test_that("an effectsize result is captured, typed as Cohen's d with its interval (#549)", {
  skip_if_not_installed("effectsize")
  x <- c(17, 20, 19, 25, 26, 18, 22, 21)
  y <- c(10, 12, 14, 16, 18, 11, 13, 16)
  es <- effectsize::repeated_measures_d(x, y, method = "z", adjust = FALSE)
  rec <- .r_capture_value(es, "dz_result <- effectsize::repeated_measures_d(x, y)")
  st <- rec$rows[[1]]$stats
  expect_equal(names(st), c("d", "conf.low", "conf.high"))
  expect_equal(st$d, es$d_z)
  expect_equal(stato_type_column("d")$annotationValue, "Cohen's d")
  # also from the copy of the capture code a Docker container gets
  e <- new.env(parent = globalenv())
  eval(parse(text = .repro_docker_capture_preamble()), envir = e)
  expect_equal(e$.r_capture_value(es, "dz <- f()")$rows[[1]]$stats$d, es$d_z)
  # a table of several effects names each row by its Parameter column
  g <- factor(rep(c("a", "b"), 4))
  eta <- suppressMessages(effectsize::eta_squared(stats::aov(x ~ g), partial = FALSE))
  rec <- .r_capture_value(eta, "eta <- eta_squared(m)")
  expect_equal(rec$rows[[1]]$label, "g")
  expect_true("\u03b7\u00b2" %in% names(rec$rows[[1]]$stats))
})

test_that("a value exactly halfway matches both of its correct roundings", {
  # The mean of these 8 numbers is exactly 13.625: "M = 13.63" (rounding half
  # up, as people do) was not found, because the output value was rounded
  # with round(), which gives 13.62 (half to the even digit)
  d <- data.frame(congruent = c(10, 12, 14, 16, 18, 11, 13, 15),
                  incongruent = c(17, 20, 19, 25, 26, 18, 22, 21))
  rec <- .r_capture_value(sapply(d, mean), "means <- sapply(d, mean)")
  rec$line <- 2L
  rec$call_text <- "means <- sapply(d, mean)"
  tabs <- .r_captures_to_tables(list(rec), source_label = "analysis.R")
  stat_output <- list(list(
    file = "analysis.R", n_tables = length(tabs), source = "r_output",
    json = stat_output_json(tabs, paper_id = "test_paper", source_file = "analysis.R"),
    long = stat_results_long(tabs, paper_id = "test_paper", source_file = "analysis.R")))
  found <- function(txt) match_reported_output(test_paper(txt), stat_output)$found
  expect_true(all(found("The congruent condition (M = 13.63) was faster.")))
  expect_true(all(found("The congruent condition (M = 13.62) was faster.")))
  expect_false(any(found("The congruent condition (M = 13.64) was faster.")))
  expect_false(any(found("The congruent condition (M = 13.61) was faster.")))
})

test_that(".repro_unique_script_names changes only names that repeat (#568)", {
  files <- data.frame(file_name = c("a.R", "a.R", "b.R"), file_path = c("x/a.R", ".\\y\\a.R", "z/b.R"))
  expect_equal(.repro_unique_script_names(files), c("x/a.R", "y/a.R", "b.R"))
  # no repeats, no change
  expect_equal(.repro_unique_script_names(files[2:3, ]), c("a.R", "b.R"))
  # the same path in two repositories
  files <- data.frame(file_name = c("a.R", "a.R"), file_path = c("x/a.R", "x/a.R"),
                      repo_name = c("github", "zenodo.1"))
  expect_equal(.repro_unique_script_names(files), c("github/x/a.R", "zenodo.1/x/a.R"))
  # nothing to tell them apart by: a number, and the name still ends in the file name
  files <- data.frame(file_name = c("a.R", "a.R"))
  expect_equal(.repro_unique_script_names(files), c("1/a.R", "2/a.R"))
  # a name the other language already has
  files <- data.frame(file_name = "n.ipynb", file_path = "py/n.ipynb")
  expect_equal(.repro_unique_script_names(files, taken = "n.ipynb"), "py/n.ipynb")
})

test_that(".repro_script_places gives same-named scripts places of their own (#568)", {
  files <- data.frame(file_name = c("x/a.R", "y/a.R", "b.R"), file_path = c("x/a.R", "y/a.R", "z/b.R"))
  # each its own path
  expect_equal(.repro_script_places(files), c("x/a.R", "y/a.R", "z/b.R"))
  # the same path in two repositories
  files <- data.frame(file_name = c("r1/x/a.R", "r2/x/a.R"), file_path = c("x/a.R", "x/a.R"))
  expect_equal(.repro_script_places(files), c("r1/x/a.R", "r2/x/a.R"))
  # never above the sandbox
  files <- data.frame(file_name = c("../a.R", "x/../../a.R"), file_path = c("../a.R", "x/../../a.R"))
  expect_equal(.repro_script_places(files), c("_/a.R", "x/_/_/a.R"))
})

test_that("repro_run_order follows source() to the file it names among same-named files (#568)", {
  files <- data.frame(file_name = c("a/h.R", "b/h.R", "a/m.R", "top.R", "other.R"))
  files$file_path <- files$file_name
  files$reads <- files$writes <- replicate(5, character(0), simplify = FALSE)
  files$sources <- list(character(0), character(0), "h.r", "h.r", "h.r")
  files$source_refs <- list(character(0), character(0), "h.R", "b/h.R", "h.R")
  edges <- attr(repro_run_order(files), "source_edges")
  expect_equal(edges$from[edges$to == "a/m.R"], "a/h.R")
  expect_equal(edges$from[edges$to == "top.R"], "b/h.R")
  expect_equal(.repro_sourced_file(1:2, "C:/Users/me/proj/b/h.R", c("a/h.R", "b/h.R", "m.R"), 3), 2L)
  # no file with that name beside it, and no folder in the call: no guess
  expect_length(edges$from[edges$to == "other.R"], 0)
  # one file with the name: as before, from any folder
  files <- data.frame(file_name = c("a/h.R", "m.R"))
  files$sources <- list(character(0), "h.r")
  expect_equal(attr(repro_run_order(files), "source_edges")$from, "a/h.R")
})

test_that("repro_file_io keeps each source() reference as written (#568)", {
  io <- repro_file_io(list(m.R = c('source("../Lib/H.R")', 'source("g.R")', 'source("x/g.R")')))
  expect_equal(io$sources[[1]], c("h.r", "g.r"))
  expect_equal(io$source_refs[[1]], c("../Lib/H.R", "g.R"))
})

test_that("repro_rewrite_paths takes the file in the script's own folder among same-named files (#568)", {
  plan <- data.frame(file_name = c("d.csv", "d.csv"), current_path = c("x/d.csv", "y/d.csv"),
                     target_path = c("unknown/x/d.csv", "unknown/y/d.csv"))
  code <- 'read.csv("d.csv")'
  own <- repro_rewrite_paths(code, "m.R", plan, script_path = "y/m.R")
  expect_equal(own$target, "unknown/y/d.csv")
  expect_false(own$ambiguous)
  # no file with that name in the script's folder: no guess
  expect_true(repro_rewrite_paths(code, "m.R", plan, script_path = "z/m.R")$ambiguous)
  # without the path, as before
  expect_true(repro_rewrite_paths(code, "m.R", plan)$ambiguous)
  # a reference above the top folder, with a plan that holds no paths
  plan$current_path <- NULL
  up <- repro_rewrite_paths('read.csv("../d.csv")', "m.R", plan, script_path = "m.R")
  expect_true(up$ambiguous)
})

test_that(".repro_py_imported: of same-named modules, only the one the importer can reach (#568)", {
  code <- list(`a/m.py` = "import utils", `a/utils.py` = "x = 1", `b/utils.py` = "x = 2",
               `c/n.py` = "from b.utils import x", `d/utils.py` = "x = 3")
  imp <- .repro_py_imported(code, names(code))
  expect_setequal(names(imp), c("a/utils.py", "b/utils.py"))
  expect_equal(imp[["a/utils.py"]], "a/m.py")
  expect_equal(imp[["b/utils.py"]], "c/n.py")
})

test_that(".repro_by_repository gives a line per repository when there are several (#568)", {
  tb <- data.frame(repo_name = c("a", "a", "b", "b"), outcome = c("ran_ok", "errored", "errored", "defines_functions"))
  out <- paste(.repro_by_repository(tb), collapse = "\n")
  expect_match(out, "#### Results by repository", fixed = TRUE)
  expect_null(.repro_by_repository(tb[1:2, ]))
  expect_null(.repro_by_repository(data.frame(outcome = "ran_ok")))
})

test_that(".repro_py_module_folders finds the repository's own modules (#568)", {
  files <- c("repo/maad/__init__.py", "repo/maad/sound.py", "repo/scripts/run.py", "repo/lib/helper.py",
             "repo/scripts/beside.py", "other/maad/__init__.py", "bad/json.py", "bad/own.py")
  f <- .repro_py_module_folders
  expect_equal(f("from maad import sound", "repo/scripts/run.py", files), "repo")
  expect_equal(f("import helper", "repo/scripts/run.py", files), "repo/lib")
  # nearest to the script, when a name is in two places
  expect_equal(f("import maad", "other/sub/x.py", files), "other")
  # beside the script Python finds it already; what is not in the repository is not looked for
  expect_length(f("import beside", "repo/scripts/run.py", files), 0)
  expect_length(f(c("import numpy", "import os"), "repo/scripts/run.py", files), 0)
  # a folder that would hide a standard module the script uses
  expect_length(f(c("import json", "import own"), "repo/scripts/run.py", files), 0)
  expect_equal(f("import own", "repo/scripts/run.py", files), "bad")
  # comments do not count
  expect_length(f("# import helper", "repo/scripts/run.py", files), 0)
})

test_that(".repro_py_starts_server recognises a script that waits for connections (#568)", {
  expect_true(.repro_py_starts_server(c("import http.server", "httpd.serve_forever()")))
  expect_true(.repro_py_starts_server(c("s = socket.socket()", "s.listen(5)")))
  expect_true(.repro_py_starts_server("app.run(debug = True)"))
  expect_false(.repro_py_starts_server(c("import numpy", "# s.listen(5)", "print(1)")))
  expect_false(.repro_py_starts_server("model.run(x)"))
})

test_that(".repro_drop_author_lib_loc removes a lib.loc on the authors' computer only (#594)", {
  x <- .repro_drop_author_lib_loc(c(
    'library(hydroGOF, lib.loc = "C:/Users/me/Documents/R/win-library/4.0")',
    "suppressPackageStartupMessages(require(sp, lib.loc='/home/me/R/x86_64'))",
    'requireNamespace("raster", lib.loc = "~/Rlibs", quietly = TRUE)',
    'library(mypkg, lib.loc = "lib")',
    '# library(y, lib.loc = "C:/z")',
    "library(dplyr)"))
  expect_equal(x$txt, c("library(hydroGOF)", "suppressPackageStartupMessages(require(sp))",
                        'requireNamespace("raster", quietly = TRUE)', 'library(mypkg, lib.loc = "lib")',
                        '# library(y, lib.loc = "C:/z")', "library(dplyr)"))
  expect_equal(x$paths, c("C:/Users/me/Documents/R/win-library/4.0", "/home/me/R/x86_64", "~/Rlibs"))
})

test_that("repro_dependencies takes no install address from a line made a comment (#598)", {
  # 10.1111/1365-2435.13410: the address no longer answers; embryogrowth is on CRAN
  code <- c('# install.packages("embryogrowth")',
            '# install.packages("http://www.ese.u-psud.fr/epc/conservation/CRAN/embryogrowth.tar.gz", repos=NULL, type="source")',
            "#devtools::install_github('someone/HelpersMG')",
            "library(embryogrowth); library(HelpersMG)",
            'remotes::install_github("user/mypkg")  # this one is used',
            "library(mypkg)")
  d <- repro_dependencies(code)
  expect_equal(d$source[d$package == "embryogrowth"], "cran")
  expect_true(is.na(d$ref[d$package == "embryogrowth"]))
  expect_equal(d$source[d$package == "HelpersMG"], "cran")
  expect_equal(d$source[d$package == "mypkg"], "github")
  expect_equal(d$ref[d$package == "mypkg"], "user/mypkg")
})

test_that(".repro_download_failed_msg says the download failed (#598)", {
  expect_equal(.repro_download_failed_msg("http://a.org/p.tar.gz", c("x", "cannot open URL 'http://a.org/p.tar.gz'")),
               "could not download http://a.org/p.tar.gz: cannot open URL 'http://a.org/p.tar.gz'")
  expect_match(.repro_download_failed_msg("u/r", "HTTP error 404.\n  Not Found"), "^could not download u/r: HTTP error 404")
  expect_null(.repro_download_failed_msg("u/r", c("non-zero exit status", "compilation failed")))
})

test_that("a CRAN package whose GitHub or URL install fails is installed from CRAN, with a note (#598)", {
  skip_if_quick()
  skip_on_cran()
  skip_if_offline()
  deps <- data.frame(package = "praise", source = "url",
                     ref = "http://does-not-exist.invalid/CRAN/praise.tar.gz", base = FALSE)
  r <- suppressWarnings(repro_install_deps(deps, withr::local_tempdir()))
  expect_true(r$installed)
  expect_match(r$version_note, "^installed from CRAN: the install from http://does-not-exist.invalid/CRAN/praise.tar.gz failed \\(could not download")
})

test_that(".repro_write_failed_folders finds the folder a write stopped on, not a read's (#591)", {
  f <- .repro_write_failed_folders
  q1 <- intToUtf8(8216L); q2 <- intToUtf8(8217L)
  # 10.1111/1365-2664.14583: the path built in a function of a sourced file
  expect_equal(f("Error in saveWorkbook(wb, paste0(wdoutputs, filename, \".xlsx\")) :\n  cannot create file './outputs/Butterfly_GAM_Outputs.xlsx', reason 'No such file or directory'"),
               "./outputs")
  # 10.1111/1365-2664.14735: ggsave(here("5_figs/final/methods-demo.pdf"))
  expect_equal(f("Error in `ggsave()`:\n! Cannot find directory '/sandbox/SERDP/5_figs/final'."), "/sandbox/SERDP/5_figs/final")
  expect_equal(f(paste0("Error in gzfile(file, \"wb\") : cannot open the connection\nIn addition: Warning message:\n  cannot open compressed file ",
                        q1, "results/m.rds", q2, ", probable reason ", q1, "No such file or directory", q2)), "results")
  expect_equal(f("Error in png(\"figs/a.png\") : unable to start png() device\nIn addition: Warning message:\ncannot open file 'figs/a.png': No such file or directory"), "figs")
  expect_equal(f("Traceback (most recent call last):\n  File \"a.py\", line 3, in <module>\n    with open('out/res.txt', 'w') as f:\nFileNotFoundError: [Errno 2] No such file or directory: 'out/res.txt'"), "out")
  expect_equal(f("Traceback (most recent call last):\n  File \"a.py\", line 9, in <module>\n    plt.savefig('figs/a.png')\nFileNotFoundError: [Errno 2] No such file or directory: 'figs/a.png'"), "figs")
  # a file a script reads but does not find is not a folder to make
  expect_equal(f("Error in file(file, \"rt\") : cannot open the connection\nIn addition: Warning message:\ncannot open file 'data/x.csv': No such file or directory"), character(0))
  expect_equal(f("Error in readChar(con, 5L, useBytes = TRUE) : cannot open the connection\nIn addition: Warning message:\ncannot open compressed file 'out/m.Rdata', probable reason 'No such file or directory'"), character(0))
  expect_equal(f("Traceback (most recent call last):\n  File \"a.py\", line 3, in <module>\n    df = pd.read_csv('data/x.csv')\nFileNotFoundError: [Errno 2] No such file or directory: 'data/x.csv'"), character(0))
  expect_equal(f(NA_character_), character(0))
})

test_that(".repro_make_failed_write_folders makes the folder in the sandbox only, once (#591)", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  run_tbl <- data.frame(file_name = c("a.R", "b.R"), run_dir = file.path(root, "code"))
  res <- data.frame(file_name = c("a.R", "b.R"), outcome = "errored",
                    error = c("Error in saveRDS(x, \"../out/m.rds\") : cannot open the connection\ncannot create file '../out/m.rds'",
                              "Error in pdf(\"C:/Users/me/fig.pdf\") : cannot open file 'C:/Users/me/fig.pdf'"),
                    stderr = "")
  made <- .repro_make_failed_write_folders(res, run_tbl, root)
  expect_equal(made$script, "a.R")
  expect_equal(made$folder, "out")
  expect_true(dir.exists(file.path(root, "out")))
  # not again
  unlink(file.path(root, "out"), recursive = TRUE)
  expect_equal(nrow(.repro_make_failed_write_folders(res, run_tbl, root, list(a.R = "out"))), 0)
})

test_that(".repro_error_message gives each failure's message, short (#615)", {
  m <- .repro_error_message
  # PsyArXiv 6b437_v1: the message the report shows, not only "runtime" / "other"
  out <- "Loading required package: x\nError in .Primitive(\":\")() : 0 arguments passed to ':' which requires 2\nCalls: source -> withVisible\nExecution halted"
  expect_equal(m("errored", out), "Error in .Primitive(\":\")() : 0 arguments passed to ':' which requires 2")
  # the package that is not there
  expect_match(m("dependency_unavailable", "Error in library(lme4) : there is no package called 'lme4'\nExecution halted"),
               "there is no package called 'lme4'", fixed = TRUE)
  # the address it asked for, when the error line names none
  expect_equal(m("needs_internet", "Error in curl::curl_fetch_memory(url) : Could not resolve host\nExecution halted",
                 "trying URL 'https://example.org/data.csv'"),
               "Error in curl::curl_fetch_memory(url) : Could not resolve host (https://example.org/data.csv)")
  expect_equal(m("needs_internet", "Error in file(con) : cannot open URL 'https://a.org/x.csv'"),
               "Error in file(con) : cannot open URL 'https://a.org/x.csv'")
  # the time limit
  expect_equal(m("timed_out", "timed out after 3600s", "...last output"), "timed out after 3600s")
  # Python's error line; without an error line, the last line of the output
  py <- "Traceback (most recent call last):\n  File \"a.py\", line 3, in <module>\n    print(x)\nNameError: name 'x' is not defined"
  expect_equal(m("errored", py, python = TRUE), "NameError: name 'x' is not defined")
  expect_equal(m("errored", "killed\nthe script stopped without a message"), "the script stopped without a message")
  # at most 300 characters
  long <- paste("Error in f() :", strrep("word ", 200))
  expect_lte(nchar(m("errored", long)), 300)
  # none for what ran through, or when there is no message
  expect_true(all(is.na(m(c("ran_ok", "run_by_source", "defines_functions", "errored", NA), c("x", "x", "x", "", "x")))))
  expect_equal(m(character(0), character(0)), character(0))
})

test_that(".repro_failure_reason names why a script did not run through (#568)", {
  r <- .repro_failure_reason
  expect_equal(r("errored", "cannot open file 'a.csv'"), "file")
  expect_equal(r("errored", "there is no package called 'x'"), "package")
  expect_equal(r("errored", "object 'x' not found"), "undefined")
  expect_equal(r("errored", "cannot cache function 'f'"), "environment")
  expect_equal(r("errored", " *** caught segfault ***\naddress 0x10, cause 'memory not mapped'"), "environment")
  expect_equal(r("errored", " "), "no_message")
  expect_equal(r("errored", "list index out of range"), "other")
  expect_equal(r(c("python2", "needs_a_person", "needs_internet", "timed_out"), ""), c("syntax", "interactive", "internet", "time"))
  # what is not a failure of the script has none
  expect_true(all(is.na(r(c("ran_ok", "run_by_source", "defines_functions", "not_analysis", "jags_model", "docker_failed"), "x"))))
})

test_that(".repro_run_counts gives a paper's scripts, its software and its reasons (#568, item 24)", {
  rr <- data.frame(
    outcome = c("ran_ok", "errored", "errored", "dependency_unavailable", "run_by_source",
                "not_analysis", "not_analysis", "defines_functions", "docker_failed",
                "not_downloaded"),
    error = c("", "cannot open file 'a.csv'", "list index out of range", "", "", "", "", "", "x",
              "cannot open file 'big.rds'"),
    stderr = "")
  rc <- .repro_run_counts(rr)
  expect_equal(nrow(rc), 1)
  expect_equal(rc$repro_scripts_n, 4)
  expect_equal(rc$repro_not_analysis_n, 2)
  # a script that stopped on a file we did not download is counted apart, not as a failure
  expect_equal(rc$repro_not_downloaded_n, 1)
  expect_equal(c(rc$repro_fail_file, rc$repro_fail_other, rc$repro_fail_package), c(1, 1, 1))
  # every reason has its column, also with no script that failed for it
  expect_equal(rc$repro_fail_time, 0)
  expect_setequal(names(rc), c("repro_scripts_n", "repro_not_analysis_n", "repro_not_downloaded_n",
                               paste0("repro_fail_", .repro_reason_levels)))
  # the reason column is read when the run results have one
  rr$reason <- .repro_failure_reason(rr$outcome, rr$error)
  rr$reason[2] <- "undefined"
  expect_equal(.repro_run_counts(rr)$repro_fail_undefined, 1)
  # nothing run: the same columns, all NA
  none <- .repro_run_counts(NULL)
  expect_named(none, names(rc))
  expect_true(all(is.na(unlist(none))))
})

test_that(".repro_needs_internet_error reads a network error (#568)", {
  expect_true(.repro_needs_internet_error("cannot open URL 'https://a'"))
  expect_true(.repro_needs_internet_error("requests.exceptions.ConnectionError: x"))
  expect_false(.repro_needs_internet_error("cannot open file 'a.csv'"))
})

test_that(".repro_absent_report puts scripts apart from data, and leaves out what has a near match (#568)", {
  mi <- data.frame(basename = c("a.csv", "helpers.r", "b.csv", "c.csv"), status = c("absent", "absent", "absent", "in_archive"),
                   detail = "", similar_to = c(NA, NA, "b_.csv", NA))
  out <- paste(.repro_absent_report(mi), collapse = "\n")
  expect_match(out, "1 data file that is not in the deposit", fixed = TRUE)
  expect_match(out, "`a.csv`", fixed = TRUE)
  expect_match(out, "1 script that is not in the deposit: `helpers.r`", fixed = TRUE)
  expect_no_match(out, "b.csv", fixed = TRUE)
  expect_no_match(out, "c.csv", fixed = TRUE)
  expect_null(.repro_absent_report(mi[3:4, ]))
})

test_that("the Docker install script knows where packages only elsewhere are (#568)", {
  lines <- .repro_docker_elsewhere_lines()
  expect_no_error(parse(text = lines))
  expect_true(any(grepl("rmcelreath/rethinking", lines, fixed = TRUE)))
  expect_true(any(grepl("r-universe.dev/api/search", lines, fixed = TRUE)))
  # a package found in none of the places is marked so (#389)
  expect_true(any(grepl("found nowhere else: ", lines, fixed = TRUE)))
})

test_that(".repro_run_after_lines loads a script from its own folder (#568)", {
  root <- withr::local_tempdir()
  run_tbl <- data.frame(file_name = c("use.R", "def.R", "top.R"),
                        script_path = file.path(root, c("a/use.R", "b/c/def.R", "top.R")),
                        run_dir = file.path(root, c("a", "b", ".")))
  run_tbl$run_dir[3] <- root
  out <- .repro_run_after_lines(list(use.R = "def.R", top.R = "use.R"), run_tbl, root)
  expect_match(out$use.R, 'setwd("../b")', fixed = TRUE)
  expect_match(out$use.R, 'source("c/def.R", local = globalenv())', fixed = TRUE)
  expect_match(out$top.R, 'setwd("a")', fixed = TRUE)
  expect_match(out$top.R, 'source("use.R", local = globalenv())', fixed = TRUE)
  expect_no_error(parse(text = unlist(out)))
  expect_null(.repro_run_after_lines(list(), run_tbl, root))
})

test_that("Python install names, and Python 2's modules (#568)", {
  deps <- repro_dependencies_py(c("import pywt", "import ruamel.yaml", "import pylab", "import cPickle", "import urlparse"))
  expect_setequal(deps$package[!deps$base], c("PyWavelets", "ruamel.yaml", "matplotlib"))
  expect_true(all(c("cPickle", "urlparse") %in% deps$package[deps$base]))
  expect_true(.repro_py2_error("ModuleNotFoundError: No module named 'cPickle'"))
  expect_false(.repro_py2_error("ModuleNotFoundError: No module named 'numpy'"))
})

test_that("the Docker install script parses, with its second pass (#568)", {
  expect_no_error(parse(text = .repro_docker_elsewhere_lines()))
  expect_true(any(grepl("BiocManager::install", .repro_docker_elsewhere_lines(), fixed = TRUE)))
})

test_that(".repro_neutralise_env_calls replaces calls that install packages (#568)", {
  n <- function(x) .repro_neutralise_env_calls(x)$txt
  expect_equal(n('install.packages("a")'), "invisible(NULL)")
  expect_equal(n('if (!require(a)) install.packages("a", dependencies = TRUE)'), "if (!require(a)) invisible(NULL)")
  expect_equal(n('devtools::install_github("u/r")'), "invisible(NULL)")
  expect_equal(n('remotes::install_github("u/r", ref = c("a"))'), "invisible(NULL)")
  expect_equal(n('BiocManager::install("a")'), "invisible(NULL)")
  expect_equal(n('sapply(p, install.packages)'), "sapply(p, invisible)")
  # p_load becomes library()
  expect_equal(n('pacman::p_load(a, b)'), '{ library("a"); library("b") }')
  expect_equal(n('p_load(char = pk)'), "{ for (.pkg in pk) library(.pkg, character.only = TRUE) }")
  expect_equal(n('library(pacman)'), 'invisible("reproducibility_check removed the loading of a package manager")')
  # a message about it stays valid code, and other names stay
  msg <- n('message("run install.packages(\'a\') first")')
  expect_no_error(parse(text = msg))
  expect_equal(n('my.install.packages("a")'), 'my.install.packages("a")')
  expect_equal(n('x <- 1'), 'x <- 1')
})

test_that(".repro_fill_file_choice fills in the file the code names (#568)", {
  files <- c("nest_outcomes.csv", "transects.csv")
  f <- function(x, d = files) .repro_fill_file_choice(x, d)
  same_line <- f('d <- read.csv(file.choose())   # Select "nest_outcomes" here')
  expect_equal(same_line$txt, 'd <- read.csv("nest_outcomes.csv")   # Select "nest_outcomes" here')
  expect_equal(same_line$filled, "nest_outcomes.csv")
  above <- f(c("# the transects file", "d <- read.csv(file.choose())"))
  expect_equal(above$txt[2], 'd <- read.csv("transects.csv")')
  # the only data file of the deposit
  expect_equal(f("d <- read.csv(file.choose())", "a.csv")$txt, 'd <- read.csv("a.csv")')
  # nothing says which, or two fit: it stays
  expect_equal(f("d <- read.csv(file.choose())")$txt, "d <- read.csv(file.choose())")
  expect_length(f("d <- read.csv(file.choose())")$filled, 0)
  expect_equal(f("d <- file.choose() # nest_outcomes or transects")$filled, character(0))
  # a comment is left alone, other forms too
  expect_equal(f("# file.choose()", "a.csv")$txt, "# file.choose()")
  expect_equal(f('p <- choose.files(multi = F, caption = c("a"))', "a.csv")$txt, 'p <- "a.csv"')
})

test_that(".repro_starts_app and .repro_needs_person_error (#568)", {
  expect_true(.repro_starts_app(c("library(shiny)", "shiny::runApp('a')")))
  expect_true(.repro_starts_app("shinyApp(ui, server)"))
  expect_false(.repro_starts_app(c("f <- function() runApp('a')", "x <- 1")))
  expect_false(.repro_starts_app("runApp("))
  expect_true(.repro_needs_person_error("Error in file.choose() : file choice cancelled"))
  expect_true(.repro_needs_person_error("EOFError: EOF when reading a line"))
  expect_false(.repro_needs_person_error("Error: cannot open the connection"))
})

test_that(".repro_make_write_folders makes a relative folder, never above the sandbox (#568)", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "a", "b"), recursive = TRUE)
  wr <- data.frame(call_text = c('write.csv(x, "out/r.csv")', "ggsave('../fig/p.png')", "ggsave('../../../up/q.png')",
                                 'write.csv(x, "/abs/s.csv")', 'write.csv(x, "here.csv")'))
  .repro_make_write_folders(wr, d, "a/b")
  expect_true(dir.exists(file.path(d, "a", "b", "out")))
  expect_true(dir.exists(file.path(d, "a", "fig")))
  expect_false(dir.exists(file.path(d, "up")))
  expect_false(dir.exists(file.path(dirname(d), "up")))
})

test_that(".repro_rel_from counts a path from a folder of the package (#568)", {
  expect_equal(.repro_rel_from("a/b.csv"), "a/b.csv")
  expect_equal(.repro_rel_from("a/b.csv", "a"), "b.csv")
  expect_equal(.repro_rel_from("a/b.csv", "a/c"), "../b.csv")
  expect_equal(.repro_rel_from("x/y.csv", "a/c"), "../../x/y.csv")
})

test_that(".repro_run_folders runs a script from where its paths work (#568)", {
  plan <- data.frame(target_path = c("top/R/a.R", "top/R/b.R", "top/R/h.R", "top/data/d.csv", "top/p.Rproj",
                                     "other/s.R", "other/near.csv", "tie/c.R", "tie/x/e.R", "tie/p.Rproj"))
  code <- list(`a.R` = 'read.csv("data/d.csv")',       # from the top of the project
               `b.R` = 'source("h.R")',                # from its own folder
               `h.R` = 'x <- 1',                       # sourced: runs where b.R runs
               `s.R` = 'read.csv("near.csv")',
               `c.R` = 'x <- 1',                       # names nothing: its own folder
               `e.R` = 'x <- 1')                       # a tie: the .Rproj folder
  places <- c("top/R/a.R", "top/R/b.R", "top/R/h.R", "other/s.R", "tie/c.R", "tie/x/e.R")
  order_tbl <- structure(data.frame(file_name = names(code)),
                         source_edges = data.frame(from = "h.R", to = "b.R"))
  out <- .repro_run_folders(places, code, plan, order_tbl)
  expect_equal(out, c("top", "top/R", "top/R", "other", "tie", "tie"))
  # no plan: from the top
  expect_equal(.repro_run_folders(places, code, NULL, order_tbl), rep(".", 6))
  # a script at the top stays there, and a folder keeps its spelling
  p2 <- data.frame(target_path = c("Top/a.R", "Top/d.csv", "m.R"))
  out2 <- .repro_run_folders(c("Top/a.R", "m.R"), list(`a.R` = 'read.csv("d.csv")', `m.R` = "x"), p2,
                             data.frame(file_name = c("a.R", "m.R")))
  expect_equal(out2, c("Top", "."))
})

test_that(".repro_keep_working_refs counts from where the script runs (#568)", {
  plan <- .repro_package_plan(data.frame(file_name = c("d.csv", "h.R"), file_path = c("top/data/d.csv", "top/R/h.R")))
  keep <- function(code, dir) .repro_keep_working_refs(repro_rewrite_paths(code, "m.R", plan), plan, "top/R/m.R", dir)
  # works as written from the folder it runs in
  from_top <- keep('read.csv("data/d.csv")', "top")
  expect_true(from_top$as_written)
  # a file named from elsewhere is rewritten, counted from that folder
  abs <- keep('read.csv("/home/me/data/d.csv")', "top/R")
  expect_equal(abs$target, "../data/d.csv")
  expect_false(abs$as_written)
  # beside the script, while it runs from above it
  beside <- keep('source("h.R")', "top")
  expect_equal(beside$target, "R/h.R")
})

test_that("a file at the top named as it is there counts from where the script runs (#568)", {
  # 10_1002_pan3_10310: the data is a deposit of its own, at the top; the R
  # Markdown files are three folders down and read "./<name>.csv"
  plan <- .repro_package_plan(data.frame(file_name = c("q.csv", "a.Rmd"),
                                         file_path = c("q.csv", "g/x.zip/x-1/a.Rmd")))
  rw <- .repro_keep_working_refs(repro_rewrite_paths('d <- read.csv("./q.csv")', "a.Rmd", plan),
                                 plan, "g/x.zip/x-1/a.Rmd", "g/x.zip/x-1")
  expect_false(rw$as_written)
  expect_equal(rw$target, "../../../q.csv")
  # from the top it works as written
  top <- .repro_keep_working_refs(repro_rewrite_paths('d <- read.csv("./q.csv")', "a.Rmd", plan),
                                  plan, "g/x.zip/x-1/a.Rmd", ".")
  expect_true(top$as_written)
})

test_that(".repro_docker_failed reads an error from Docker itself (#568)", {
  expect_true(.repro_docker_failed("Error response from daemon: Docker Desktop is unable to start"))
  expect_true(.repro_docker_failed("failed to connect to the docker API at npipe:////./pipe/dockerDesktopLinuxEngine"))
  expect_false(.repro_docker_failed("Error in read.csv(f): cannot open the connection"))
})

test_that(".repro_checklist_section does not count Docker's failure as the script's (#568)", {
  tb <- data.frame(file_name = "a.R", run_order = 1, outcome = "docker_failed", setwd_removed = 0,
                   not_runnable_reason = NA_character_, unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = "a.R", outcome = "docker_failed", error = "x", stderr = "")
  out <- .repro_checklist_section(tb, rr)
  expect_match(out, "says nothing about the script", fixed = TRUE)
  expect_no_match(out, "stopped with an error", fixed = TRUE)
})

test_that(".repro_function_names recognises a file that only defines functions (#568)", {
  expect_equal(.repro_function_names(c("library(a)", "k <- 1", "f <- function(x) x", "g = function() 2")), c("f", "g"))
  expect_equal(.repro_function_names(c('"doc"', 'pal <- c("a", "b")', "setClass('A')", "h <- function() 1")), "h")
  # anything else it does makes it a script
  expect_length(.repro_function_names(c("f <- function(x) x", "f(1)")), 0)
  expect_length(.repro_function_names(c("f <- function(x) x", "d <- read.csv('a.csv')")), 0)
  expect_length(.repro_function_names(c("f <- function(x) x", "print(1)")), 0)
  # no functions at all, or a file that does not parse
  expect_length(.repro_function_names(c("library(a)", "k <- 1")), 0)
  expect_length(.repro_function_names("f <- function("), 0)
})

test_that(".repro_function_users names the scripts that use a file's functions (#568)", {
  code <- list(`f.R` = "fit <- function(x) x", `a.R` = "fit(1)", `b.R` = "misfit(1)", `c.R` = "x <- fit",
               `d.R` = 'source("f.R")', `e.R` = "stats::fit(2)")
  users <- .repro_function_users("f.R", "fit", code, list(`d.R` = "f.r"))
  expect_setequal(users, c("a.R", "d.R", "e.R"))
  expect_length(.repro_function_users("f.R", "nothing", code), 0)
  # a dot in a name is a dot
  expect_length(.repro_function_users("f.R", "a.b", list(`f.R` = "", `g.R` = "axb(1)")), 0)
  expect_equal(.repro_function_users("f.R", "a.b", list(`f.R` = "", `g.R` = "a.b(1)")), "g.R")
})

test_that("a package's tests and setup.py are recognised, and analyses are not (#568)", {
  expect_true(.repro_is_r_test("pkg/tests/testthat/test-a.R"))
  expect_true(.repro_is_r_test("tests/testthat.R"))
  expect_false(.repro_is_r_test("analysis/tests.R"))
  t <- .repro_is_py_test
  expect_true(t("a/conftest.py", "x = 1"))
  expect_true(t("test_a.py", "import pytest"))
  expect_true(t("a/tests/b.py", "from unittest import TestCase"))
  expect_true(t("a/tests/b.py", c("def test_one():", "    pass")))
  expect_true(t("setup.py", c("from setuptools import setup", "setup(name = 'a')")))
  # a name alone is not enough
  expect_false(t("model_test.py", "import numpy"))
  expect_false(t("predict_test.py", "x = 1"))
  expect_false(t("setup.py", "x = 1"))
  expect_false(t("a/b.py", c("def test_one():", "    pass")))
})

test_that(".repro_checklist_section leaves function files and tests out of the count (#568)", {
  tb <- data.frame(file_name = c("f.R", "t.R", "g.R", "run.R"), run_order = 1:4,
                   outcome = c("defines_functions", "not_analysis", "defines_functions", "ran_ok"),
                   setwd_removed = 0, not_runnable_reason = c("defines_functions", "not_analysis", "defines_functions", NA),
                   unresolved_inputs = NA_character_, used_by = c("run.R", NA, NA, NA))
  rr <- data.frame(file_name = tb$file_name, outcome = tb$outcome, error = "", stderr = "")
  out <- .repro_checklist_section(tb, rr)
  expect_match(out, "**1 of 1 script ran to the end**", fixed = TRUE)
  expect_match(out, "We ran the paper's 1 R script", fixed = TRUE)
  expect_match(out, "which run.R uses", fixed = TRUE)
  expect_match(out, "no script in the deposit uses them", fixed = TRUE)
})

test_that(".repro_py_parses marks Python 2 code apart from other syntax errors (#568)", {
  skip_if(!nzchar(.repro_find_python()), "needs Python")
  out <- .repro_py_parses(list(ok = "x = 1", py2 = 'print "a"', ex = c("try:", "    x = 1", "except E, e:", "    pass"),
                               tabs = c("if a:", "        x = 1", "\ty = 2"), bad = "def ("))
  expect_equal(unname(as.vector(out)), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(attr(out, "python2"), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  # no attribute when there is none
  expect_null(attr(.repro_py_parses(list(ok = "x = 1", bad = "def (")), "python2"))
})

test_that(".repro_py2_error reads Python 2 code from an error message (#568)", {
  expect_true(.repro_py2_error("  File \"a.py\"\nSyntaxError: Missing parentheses in call to 'print'. Did you mean print(...)?"))
  expect_true(.repro_py2_error("TabError: inconsistent use of tabs and spaces in indentation"))
  expect_false(.repro_py2_error("SyntaxError: invalid syntax"))
  expect_false(.repro_py2_error("NameError: name 'x' is not defined"))
})

test_that(".repro_py_get_ipython adds a stand-in only when needed, and in a valid place (#568)", {
  expect_equal(.repro_py_get_ipython("x = 1"), "x = 1")
  txt <- c("# -*- coding: latin-1 -*-", "", 'if "colab" in str(get_ipython()):', "    pass")
  out <- .repro_py_get_ipython(txt)
  expect_equal(out[1:2], txt[1:2])
  expect_match(out[3], "^get_ipython = lambda")
  fut <- .repro_py_get_ipython(c('"""doc"""', "from __future__ import print_function", "get_ipython().system('ls')"))
  expect_match(fut[3], "^get_ipython = lambda")
  expect_equal(.repro_py_get_ipython(c("def get_ipython(): pass", "get_ipython()")), c("def get_ipython(): pass", "get_ipython()"))
  skip_if(!nzchar(.repro_find_python()), "needs Python")
  expect_true(all(.repro_py_parses(list(a = out, b = fut))))
})

test_that(".repro_path_named finds the file a path names among same-named files (#568)", {
  paths <- c("Data/test/d.csv", "Data/d.csv", "repo.zip/top/R/h.R", "other.zip/top/R/h.R")
  expect_equal(.repro_path_named(paths[1:2], "Data/test/d.csv"), 1L)
  expect_equal(.repro_path_named(paths[1:2], "Data/d.csv"), 2L)
  expect_equal(.repro_path_named(paths[1:2], "./data/TEST/d.csv"), 1L)
  # an absolute path from the author's computer
  expect_equal(.repro_path_named(paths[1:2], "/Users/me/proj/Data/test/d.csv"), 1L)
  # the name alone, or the same ending twice: no guess
  expect_length(.repro_path_named(paths[1:2], "d.csv"), 0)
  expect_length(.repro_path_named(paths[3:4], "R/h.R"), 0)
  expect_length(.repro_path_named(c("x/y/d.csv", "z/d.csv"), "q/d.csv"), 0)
  # from the script's own folder, or a folder above it, nearest first
  expect_equal(.repro_path_named(paths[3:4], "./R/h.R", "other.zip/top/s.R"), 2L)
  expect_equal(.repro_path_named(paths[3:4], "R/h.R", "other.zip/top/analysis/s.R"), 2L)
  expect_equal(.repro_path_named(paths[3:4], "../R/h.R", "repo.zip/top/scripts/s.R"), 1L)
  expect_equal(.repro_path_named(c("a/x.csv", "c/x.csv"), "x.csv", "a/b/s.R"), 1L)
  expect_equal(.repro_path_named(c("a/x.csv", "a/b/x.csv"), "x.csv", "a/b/s.R"), 2L)
  # a row without a path
  expect_equal(.repro_path_named(c(NA, "a/d.csv"), "a/d.csv"), 2L)
})

test_that(".repro_assigned_names finds text assigned to a variable (#568)", {
  expect_equal(.repro_assigned_names(c('f <- c("N.R", "S.R")', 'x = "d.csv"', 'for (i in f) source(i)',
                                       'g <- paste0("a", "b.csv")', 'read.csv(file = "z.csv")')),
               c("N.R", "S.R", "d.csv"))
  expect_equal(.repro_assigned_names('if (TRUE) { y <<- "in.csv" }'), "in.csv")
  # an empty argument, which is in nearly every script
  expect_equal(.repro_assigned_names(c('a <- x[1, ]', 'f <- "d.csv"', 'b <- y[, 2][1, , 3]', 'g <- c("e.csv", )')),
               c("d.csv", "e.csv"))
  expect_length(.repro_function_names(c("f <- function(x) x", "k <- x[1, ]")), 0)
  expect_equal(.repro_function_names(c("f <- function(x) x[1, ]", "k <- c(1, )")), "f")
  expect_length(.repro_assigned_names("x <- ("), 0)
  expect_equal(.repro_assigned_names(c('f = "d.csv"', 'g = "e.csv"  # note', 'pd.read_csv("z.csv")',
                                       'h = x + "w.csv"'), "Python"), c("d.csv", "e.csv"))
})

test_that("repro_rewrite_paths follows a name in a variable and the folders in a path (#568)", {
  plan <- data.frame(file_name = c("NetworkFunction.R", "d.csv", "d.csv"),
                     current_path = c("top/NetworkFunction.R", "Data/test/d.csv", "Data/d.csv"),
                     target_path = c("analysis/NetworkFunction.R", "d/a/d.csv", "d/b/d.csv"))
  rw <- function(code) repro_rewrite_paths(code, "m.R", plan)
  one <- rw(c('f <- c("NetworkFunction.R")', 'for (i in f) source(i)'))
  expect_equal(one$target, "analysis/NetworkFunction.R")
  expect_equal(.repro_apply_rewrites('f <- c("NetworkFunction.R")', one), 'f <- c("analysis/NetworkFunction.R")')
  # a name that is not listed, or that fits two files, adds no row
  expect_equal(nrow(rw('x <- "notlisted.csv"')), 0)
  expect_equal(nrow(rw('x <- "d.csv"')), 0)
  # in a read call the name alone stays ambiguous, as before
  expect_true(rw('read.csv("d.csv")')$ambiguous)
  expect_equal(rw('read.csv("Data/test/d.csv")')$target, "d/a/d.csv")
  expect_equal(rw('read.csv("Data/d.csv")')$target, "d/b/d.csv")
  expect_equal(rw('read.csv("/home/me/Data/d.csv")')$target, "d/b/d.csv")
  expect_equal(rw('x <- "Data/test/d.csv"')$target, "d/a/d.csv")
})

test_that(".repro_name_key ignores case, spaces and punctuation, but keeps numbers apart (#568)", {
  expect_equal(.repro_name_key("Relative cover.csv"), .repro_name_key("relative_cover.csv"))
  expect_equal(.repro_name_key("EU-Forest-species.csv"), .repro_name_key("euforestspecies.csv"))
  expect_equal(.repro_name_key("a__24_03.csv"), .repro_name_key("a_24.03.csv"))
  expect_false(.repro_name_key("s1_2.csv") == .repro_name_key("s12.csv"))
  expect_false(.repro_name_key("1_data.csv") == .repro_name_key("2_data.csv"))
})

test_that("repro_rewrite_paths finds a file spelled otherwise, and names both spellings (#568)", {
  plan <- data.frame(file_name = c("relative_cover.csv", "s12.csv", "x_1.csv", "x-1.csv", "field.csv"),
                     target_path = c("d/relative_cover.csv", "d/s12.csv", "d/a/x_1.csv", "d/b/x-1.csv", "d/field.csv"))
  rw <- function(code) repro_rewrite_paths(code, "m.R", plan)
  one <- rw('d <- read.csv("relative cover.csv")')
  expect_true(one$matched)
  expect_equal(one$target, "d/relative_cover.csv")
  expect_equal(one$name_in_code, "relative cover.csv")
  expect_equal(one$name_in_deposit, "relative_cover.csv")
  # case only: the file was found before, and now also gets a note
  case <- rw('d <- read.csv("RELATIVE_cover.csv")')
  expect_equal(case$name_in_deposit, "relative_cover.csv")
  # the same name: no note
  expect_true(is.na(rw('d <- read.csv("field.csv")')$name_in_deposit))
  # numbers stay apart; another extension; two files that fit: not matched
  expect_false(rw('d <- read.csv("s1_2.csv")')$matched)
  expect_false(rw('d <- read.csv("relative cover.xlsx")')$matched)
  expect_false(rw('d <- read.csv("x 1.csv")')$matched)
  # a piece of a name, and a name built from a variable, are not compared
  expect_false(any(rw('d <- read.csv(paste0(x, "_field.csv"))')$matched))
  expect_false(any(rw('d <- read.csv(paste(x, "fie ld.csv"))')$matched))
  # a file the script writes itself
  own <- rw(c('write.csv(d, "relative cover.csv")', 'e <- read.csv("relative cover.csv")'))
  expect_false(any(own$matched))
  # a call that holds the whole name, with only the folder unknown
  call <- rw('d <- read.csv(sprintf("%s/relative cover.csv", dir))')
  expect_equal(call$target[call$is_call], "d/relative_cover.csv")
  expect_equal(call$name_in_deposit[call$is_call], "relative_cover.csv")
})

test_that("a script whose only reference is left out joins the others' path rows (#568)", {
  # 10_1111_1365_2664_14541 stopped: "Can't combine `..1$name_in_code` <character>
  # and `..18$name_in_code` <logical>". A name in a variable that fits two files is
  # left out, which left no rows, and ifelse() of no rows gives logical(0).
  plan <- data.frame(file_name = c("relative_cover.csv", "b.csv", "b.csv"),
                     target_path = c("d/relative_cover.csv", "x/b.csv", "y/b.csv"))
  none <- repro_rewrite_paths(c('f <- "b.csv"', "d <- read.csv(f)"), "a.R", plan)
  expect_equal(nrow(none), 0)
  expect_type(none$name_in_code, "character")
  expect_type(none$name_in_deposit, "character")
  one <- repro_rewrite_paths('d <- read.csv("relative cover.csv")', "b.R", plan)
  expect_no_error(both <- dplyr::bind_rows(list(b.R = one, a.R = none)))
  expect_equal(.repro_name_corrections(list(b.R = one, a.R = none))$name_in_deposit, "relative_cover.csv")
})

test_that(".repro_keep_working_refs rewrites a name spelled otherwise in the package layout (#568)", {
  plan <- .repro_package_plan(data.frame(file_name = "relative_cover.csv", file_path = "data/relative_cover.csv"))
  rw <- repro_rewrite_paths('d <- read.csv("relative cover.csv")', "m.R", plan)
  out <- .repro_keep_working_refs(rw, plan, "m.R")
  expect_false(out$as_written)
  expect_equal(out$target, "data/relative_cover.csv")
})

test_that("repro_missing_inputs names another format only between data formats (#568)", {
  structure_df <- data.frame(file_name = c("Baja_data.csv", "ranta_1985.html"))
  out <- repro_missing_inputs(c("baja data.xlsx", "ranta_1985.csv"), NULL, structure_df)
  expect_equal(out$similar_to[out$basename == "baja data.xlsx"], "baja_data.csv")
  expect_equal(out$detail[out$basename == "baja data.xlsx"], "the deposit holds baja_data.csv, the code reads baja data.xlsx")
  expect_true(is.na(out$similar_to[out$basename == "ranta_1985.csv"]))
})

test_that(".repro_checklist_section notes a corrected name (#568)", {
  tb <- data.frame(file_name = "m.R", run_order = 1, outcome = "ran_ok", setwd_removed = 0,
                   not_runnable_reason = NA_character_, unresolved_inputs = NA_character_,
                   names_corrected = "a b.csv -> a_b.csv")
  rr <- data.frame(file_name = "m.R", outcome = "ran_ok", error = "", stderr = "")
  out <- .repro_checklist_section(tb, rr)
  expect_match(out, .repro_cell("a b.csv -> a_b.csv"), fixed = TRUE)
  expect_match(out, "should be changed in the script", fixed = TRUE)
})

# #568: one download for what a run needs, and fetching what the code names or misses

test_that(".run_download_want leaves out media from hosts that send files one by one (#568)", {
  files <- data.frame(file_name = c("a.R", "x.csv", "model.stan", "fig.png", "song.wav", "paper.pdf",
                                    "fig.png", "lib.R"),
                      file_path = c("a.R", "x.csv", "model.stan", "fig.png", "song.wav", "paper.pdf",
                                    "fig.png", "renv/library/lib.R"),
                      repo_url = c(rep("https://zenodo.org/records/1", 6), "https://github.com/a/b",
                                   "https://zenodo.org/records/1"))
  expect_equal(.run_download_want(files), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_length(.run_download_want(files[0, ]), 0)
})

test_that("code_check fetches what a run needs in its own first download (#568)", {
  paper <- test_paper()
  tbl <- data.frame(paper_id = paper_id(paper), repo_url = "https://zenodo.org/records/1",
                    file_name = c("a.R", "x.csv", "model.stan", "fig.png"),
                    file_url = paste0("https://zenodo.org/f/", 1:4),
                    file_location = NA_character_, file_size = 100)
  asked <- list()
  local_mocked_bindings(download_repo_files = function(files, ...) {
    asked[[length(asked) + 1]] <<- files$file_name
    files
  })
  fake <- structure(list(module = "repo_check", table = tbl, paper = paper,
                         summary_table = data.frame(paper_id = paper_id(paper))),
                    class = "metacheck_module_output")
  suppressWarnings(module_run(fake, "code_check"))
  expect_equal(asked[[1]], "a.R")
  asked <- list()
  withr::local_options(metacheck.download_for_run = TRUE)
  suppressWarnings(module_run(fake, "code_check"))
  expect_setequal(asked[[1]], c("a.R", "x.csv", "model.stan"))
})

test_that(".repro_named_files finds the files the code names, by name or extension (#568)", {
  s <- data.frame(file_name = c("fig.png", "call.wav", "other.wav", "notes.pdf", "here.csv"),
                  file_url = c("u1", "u2", "u3", "u4", NA), file_location = c(NA, NA, NA, NA, "x/here.csv"))
  code <- c('img <- png::readPNG("figs/fig.png")', 'w <- list.files("audio", pattern = "\\\\.wav$")')
  expect_equal(.repro_named_files(s, code), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(.repro_named_files(s, 'x <- 1'), rep(FALSE, 5))
  expect_equal(.repro_named_files(s, 'glob.glob("*.wav")'), c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(.repro_named_files(s, 're.search(r"\\.wav$", f)'), c(FALSE, TRUE, TRUE, FALSE, FALSE))
})

test_that("code and messages with bytes that are not UTF-8 do not stop the module (#568)", {
  # 10_1111_2041_210x_13721 stopped with "invalid multibyte string 1": a script
  # in Latin-1, as many written on Windows are
  latin1 <- rawToChar(as.raw(c(0x23, 0x20, 0x63, 0x61, 0x66, 0xe9)))  # "# café" in Latin-1
  expect_false(validUTF8(latin1))
  s <- data.frame(file_name = c("fig.png", "notes.pdf"), file_url = c("u1", "u2"),
                  file_location = NA_character_)
  expect_equal(.repro_named_files(s, c(latin1, 'img <- png::readPNG("fig.png")')), c(TRUE, FALSE))
  msg <- paste(latin1, "Error in file(file, \"rt\") : cannot open file 'data/late.csv': No such file or directory")
  expect_equal(.repro_unopened_files(msg), "late.csv")
})

test_that(".repro_unopened_files reads the files scripts could not open (#568)", {
  msgs <- c("Error in file(file, \"rt\") : cannot open file 'data/late.csv': No such file or directory",
            "FileNotFoundError: [Errno 2] No such file or directory: 'in/x.tif'",
            "Error: 'C:/Users/a/b.rds' does not exist in current working directory",
            "object 'x' not found", NA)
  expect_setequal(.repro_unopened_files(msgs), c("late.csv", "x.tif", "b.rds"))
  expect_setequal(.repro_unopened_files(msgs, full = TRUE), c("data/late.csv", "in/x.tif", "C:/Users/a/b.rds"))
})

test_that("a file name on the line after its call is found (#568)", {
  # 10_1002_pan3_10310: read.csv( and then "./kilimanjaro_...csv") on the next line
  code <- c('habitat <- read.csv(',
            '  "./kilimanjaro_habitat.csv")',
            'x <- list(a = read.csv("a.csv"),',
            '  b = "label.txt")',
            'write.csv(x,',
            '          "out.csv", row.names = FALSE)')
  expect_setequal(code_file_refs(code, "R"), c("./kilimanjaro_habitat.csv", "a.csv"))
  io <- repro_file_io(list(s.R = code))
  expect_setequal(io$reads[[1]], c("kilimanjaro_habitat.csv", "a.csv"))
  # a write over two lines is a write, not a read
  expect_equal(io$writes[[1]], "out.csv")
  py <- repro_file_io(list(s.py = c('df = pd.read_csv(', '    "data/in.csv")',
                                    'df.to_csv(', '    "out/res.csv")')), "Python")
  expect_equal(c(py$reads[[1]], py$writes[[1]]), c("in.csv", "res.csv"))
  # a call that is not closed within 20 lines is left out, as before
  expect_length(code_file_refs(c("read.csv(", rep("x,", 25), '"late.csv")'), "R"), 0)
})

test_that("Windows paths in quoted file names get / (#568)", {
  # 10_1111_1365_2435_13388 wrote "Data\\SamplesNoct.txt", one file on Linux
  s <- .repro_slash_paths(c('write.table(m, "Data\\\\MatrixNoct.txt", sep = "\\t")',
                            'f <- list.files(pattern = "\\\\.csv$")',
                            'd <- read.csv("C:\\\\Users\\\\a\\\\data.csv")',
                            'g <- gsub("\\\\d+", "", x)',
                            'p <- "results/plain.csv"'))
  expect_equal(s$txt, c('write.table(m, "Data/MatrixNoct.txt", sep = "\\t")',
                        'f <- list.files(pattern = "\\\\.csv$")',
                        'd <- read.csv("C:/Users/a/data.csv")',
                        'g <- gsub("\\\\d+", "", x)',
                        'p <- "results/plain.csv"'))
  expect_length(s$paths, 2)
  rows <- .repro_slash_rows(data.frame(file_name = "a.R", slashes_fixed = paste(s$paths, collapse = "; ")))
  expect_equal(rows$change_type, c("windows_path", "windows_path"))
  expect_equal(rows$detail[1], '"Data\\\\MatrixNoct.txt" -> "Data/MatrixNoct.txt"')
  expect_match(.repro_slash_report(data.frame(file_name = "a.R", slashes_fixed = s$paths[1]))[1],
               "1 script uses Windows paths")
  expect_null(.repro_slash_rows(data.frame(file_name = "a.R", slashes_fixed = "")))
})

test_that("a script that stops on a changed default of R runs again with the old one (#568)", {
  rr <- data.frame(
    file_name = c("prep.R", "map.R", "other.R", "ok.R"),
    outcome = c("errored", "errored", "errored", "ran_ok"),
    error = c("Error in relevel.default(dframe1$Treatment, \"NoFire\") : 'relevel' only for (unordered) factors",
              "Error in type.convert.default(data[[i]], as.is = as.is[i], dec = dec, : invalid multibyte string at '<b0>10'",
              "Error: object 'x' not found", ""),
    stderr = "")
  od <- .repro_old_defaults(rr)
  expect_equal(od, list(prep.R = "factors", map.R = "latin1"))
  expect_equal(.repro_old_defaults(rr[3:4, ]), stats::setNames(list(), character(0)))
  lines <- .repro_old_default_lines(c("factors", "latin1"))
  expect_length(lines, 2)
  expect_true(any(grepl('options(encoding = "latin1")', lines, fixed = TRUE)))
  # the lines give R's old behaviour, also inside packages; in a session of its
  # own, since they change R's functions for the rest of the session
  skip_if_not(nzchar(Sys.which("Rscript")))
  f <- tempfile(fileext = ".R")
  writeLines(c(.repro_old_default_lines("factors"),
               'cat(class(read.csv(text = "a\\nx")$a), class(data.frame(a = "u")$a),',
               '    class(data.frame(a = "u", stringsAsFactors = FALSE)$a))'), f)
  out <- system2("Rscript", shQuote(f), stdout = TRUE)
  expect_equal(out, "factor factor character")
  txt <- .repro_old_defaults_text(od, ran_ok = "prep.R")
  expect_match(txt, "2 scripts ran again with a default R had when the code was written")
  expect_match(txt, "It then ran to the end: prep.R.", fixed = TRUE)
  expect_null(.repro_old_defaults_text(list()))
})

test_that("a model's family is not taken for a font (#568)", {
  # 10_1111_1365_2435_14353: glmmTMB(..., family = "nbinom2") became "sans"
  expect_equal(.repro_is_font_family(c('family="nbinom2"', 'family = "binomial"', 'family = "Gamma"',
                                       'family = "zero_inflated_poisson"', 'family = "Times New Roman"',
                                       "family = 'serif'", 'base_family = "Arial"')),
               c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  root <- withr::local_tempdir()
  rt <- repro_write_scripts(list(m.R = c('m <- glmmTMB(y ~ x, family="nbinom2", data = d)',
                                         'p <- ggplot(d) + theme_bw(base_family = "Times New Roman")')),
                            list(m.R = data.frame()), NULL, root)
  out <- readLines(rt$script_path)
  expect_match(out[1], 'family="nbinom2"', fixed = TRUE)
  expect_match(out[2], 'base_family = "sans"', fixed = TRUE)
  expect_equal(rt$family_replaced, 1)
})

test_that("packages loaded from a list through a function of its own are found (#568)", {
  # 10_1002_pan3_10736: tmap was in the list and never installed
  code <- c("my_packages <-  c('tidyverse', 'plyr', 'tmap',",
            "                  'scales')",
            "lapply(my_packages, function(x) library(x, character.only = T))",
            "p <- c('a1', 'b2')",
            "invisible(lapply(p, \\(x) suppressMessages(require(x, character.only = TRUE))))")
  pk <- code_library_names(code, "R")$package
  expect_true(all(c("tidyverse", "plyr", "tmap", "scales", "a1", "b2") %in% pk))
})

test_that("a folder on the authors' computer held in a variable gets the deposit's folder (#568)", {
  # 10_1111_1365_2656_14164: root <- "~/Data_to_publish/", and the files are in
  # Data_Dryad.zip/Data_Dryad/
  plan <- .repro_package_plan(data.frame(
    file_name = c("0_FUNCTIONS.R", "a.rds", "m.R"),
    file_path = c("Data_Dryad.zip/Data_Dryad/1_scripts/0_FUNCTIONS.R",
                  "Data_Dryad.zip/Data_Dryad/2_data/a.rds", "Data_Dryad.zip/Data_Dryad/1_scripts/m.R")))
  code <- c('root <- "~/Data_to_publish/"', 'p <- paste0(root,"2_data/")',
            'source(paste0(root,"/1_scripts/0_FUNCTIONS.R"))')
  fv <- .repro_folder_vars(code, plan, "Data_Dryad.zip/Data_Dryad/1_scripts")
  expect_equal(fv$value, "~/Data_to_publish/")
  expect_equal(fv$target, "../")
  expect_setequal(fv$pieces[[1]], c("2_data/", "/1_scripts/0_FUNCTIONS.R"))
  # the pasted piece is no longer rewritten on its own; the folder is
  rw <- data.frame(ref = "/1_scripts/0_FUNCTIONS.R", basename = "0_functions.r", matched = TRUE,
                   target = "0_FUNCTIONS.R", ambiguous = FALSE, n_candidates = 1L, is_call = FALSE,
                   as_written = FALSE)
  rw2 <- .repro_apply_folder_vars(rw, fv)
  expect_equal(rw2$ref, "~/Data_to_publish/")
  expect_equal(.repro_apply_rewrites(code, rw2)[1], 'root <- "../"')
  # a piece that goes up first: paste0(root, "../2_data/a.rds"), root being the scripts' folder
  fv_up <- .repro_folder_vars(c('root <- "C:/Users/me/Data_to_publish/1_scripts/"',
                                'd <- readRDS(paste0(root, "../2_data/a.rds"))'),
                              plan, "Data_Dryad.zip/Data_Dryad/1_scripts")
  expect_equal(fv_up$target, "./")
  # file.path() and a folder without the final slash (10_1111_2041_210x_13318)
  plan2 <- .repro_package_plan(data.frame(file_name = c("k.csv", "s.R"),
                                          file_path = c("Examples/P2/Data/k.csv", "Examples/P2/s.R")))
  fv2 <- .repro_folder_vars(c('dataDir <- "~/MGDrivE/Main/P2"', 'k <- read.csv(file.path(dataDir, "Data/k.csv"))'),
                            plan2, "Examples/P2")
  expect_equal(fv2$target, ".")
  # nothing of the deposit under any folder: left alone
  expect_equal(nrow(.repro_folder_vars(c('d <- "C:/me/"', 'x <- read.csv(paste0(d, "none/x.csv"))'), plan2, ".")), 0)
  # a relative folder is left alone
  expect_equal(nrow(.repro_folder_vars(c('d <- "Data/"', 'x <- read.csv(paste0(d, "k.csv"))'), plan2, "Examples/P2")), 0)
})

test_that("a folder held in a variable that a write uses is made (#568)", {
  # 10_1111_1365_2435_14353: dirF <- "../Figures/"; ggsave(paste0(dirF, "a.png"))
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R_scripts"))
  txt <- c('dirF <- "../Figures/"', 'ggsave(paste0(dirF, "App5_hab_size.png"), width = 10)')
  .repro_make_write_folders(.repro_redirect_writes(txt), root, "R_scripts", txt)
  expect_true(dir.exists(file.path(root, "Figures")))
})

test_that("Stan code from before 2.33 gets the newer syntax (#568)", {
  # 10_1111_1365_2656_12673: DegDay.stan, ChillDev.stan, two_phase.stan
  old <- c("# Stan code for degree days", "data {", "  int<lower=0> N;",
           "  real<lower=0, upper=0.2> r_hat[3];", "  vector[ndays] Tn[N];", "  int loc[N];   // # a note",
           "}", "model {", "  y ~ normal(mu, sigma);", "  increment_log_prob(normal_log(y, mu, sigma));",
           "  real a <- fabs(b);", "  target += poisson_log(k, lambda);", "}",
           "functions { real f(real[] x, int[,] m) { return x[1]; } }", "#include other.stan")
  up <- .repro_upgrade_stan(old)
  expect_equal(up$txt[1], "// Stan code for degree days")
  expect_equal(up$txt[4], "  array[3] real<lower=0, upper=0.2> r_hat;")
  expect_equal(up$txt[5], "  array[N] vector[ndays] Tn;")
  expect_equal(up$txt[6], "  array[N] int loc;   // // a note")
  expect_equal(up$txt[3], "  int<lower=0> N;")
  expect_equal(up$txt[9], "  y ~ normal(mu, sigma);")
  expect_equal(up$txt[10], "  target += normal_lpdf(y | mu, sigma);")
  expect_equal(up$txt[11], "  real a = abs(b);")
  expect_equal(up$txt[12], "  target += poisson_lpmf(k | lambda);")
  expect_equal(up$txt[14], "functions { real f(array[] real x, array[,] int m) { return x[1]; } }")
  expect_equal(up$txt[15], "#include other.stan")
  # new syntax stays as it is
  new <- c("data {", "  array[N] int<lower=0> y;", "}", "model { target += normal_lpdf(y | 0, 1); }")
  expect_equal(.repro_upgrade_stan(new)$txt, new)
  expect_length(.repro_upgrade_stan(new)$changes, 0)
  root <- withr::local_tempdir()
  dir.create(file.path(root, "m"))
  writeLines(old, file.path(root, "m", "DegDay.stan"))
  rows <- .repro_upgrade_stan_files(root)
  expect_equal(rows$file_name, "m/DegDay.stan")
  expect_match(rows$detail, "array declarations")
  expect_null(.repro_upgrade_stan_files(root))   # already converted
})

test_that("a Python program with a window counts as needing a person (#568)", {
  # 10_1111_1365_2435_14173: a Qt application waited until its time ran out
  expect_true(.repro_py_starts_server(c("from PyQt5.QtWidgets import QApplication", "app = QApplication(sys.argv)")))
  expect_true(.repro_py_starts_server(c("root = tk.Tk()", "root.mainloop()")))
  expect_false(.repro_py_starts_server(c("import matplotlib.pyplot as plt", "plt.show()")))
})

test_that("the clipboard and a screen device are not failures of the code (#568)", {
  expect_true(.repro_needs_person_error("Error in file(file, ifelse(append, \"a\", \"w\")) : 'mode' for the clipboard must be 'r' on Unix"))
  expect_equal(.repro_failure_reason("errored", "Error in dev.copy2eps(file = \"x.eps\") : can only print from a screen device"),
               "environment")
})

test_that("the folder of a path given as file = is made (#568)", {
  # 10_1111_1365_2435_14173: tab_model(m, file = "tables/duration_LMM_revision.doc")
  root <- withr::local_tempdir()
  txt <- c('tab_model(m1, file = "tables/duration_LMM_revision.doc")', 'write.csv(x, "out/x.csv")')
  .repro_make_write_folders(.repro_redirect_writes(txt), root, ".", txt)
  expect_true(dir.exists(file.path(root, "tables")))
  expect_true(dir.exists(file.path(root, "out")))
})

test_that("a library in Docker is a volume named after its folder (#568)", {
  lib <- file.path(tempdir(), "repro_sandbox_ab12", "_lib")
  expect_equal(.repro_docker_lib_volume_name(lib), "metacheck_lib_repro_sandbox_ab12__lib")
  withr::with_options(list(metacheck.docker_lib_volume = FALSE),
    expect_equal(.repro_docker_lib_mount(lib, "/rlib", read_only = TRUE),
                 paste0(normalizePath(lib, mustWork = FALSE), ":/rlib:ro")))
})

test_that("here() gets the folder its paths point to as the project's top (#568)", {
  # 10_1111_1365_2664_14317: here("scripts", "functions.R") from scripts/
  code <- list(a.Rmd = c('source(here("scripts", "functions.R"))', 'd <- readRDS(here("data", "soil.rds"))'),
               b.R = 'x <- 1')
  plan <- .repro_package_plan(data.frame(file_name = c("functions.R", "soil.rds", "a.Rmd", "b.R"),
                                         file_path = c("scripts/functions.R", "data/soil.rds", "scripts/a.Rmd", "scripts/b.R")))
  expect_equal(unname(.repro_here_roots(code, c("scripts", "scripts"), plan)), c(".", NA))
  # the authors' .Rproj at the top: here() finds it, nothing to add
  plan2 <- .repro_package_plan(data.frame(file_name = c("functions.R", "soil.rds", "p.Rproj"),
                                          file_path = c("scripts/functions.R", "data/soil.rds", "p.Rproj")))
  expect_true(is.na(.repro_here_roots(code[1], "scripts", plan2)))
  # paths that exist from the run folder itself
  plan3 <- .repro_package_plan(data.frame(file_name = c("functions.R", "soil.rds"),
                                          file_path = c("proj/scripts/functions.R", "proj/data/soil.rds")))
  expect_equal(unname(.repro_here_roots(code[1], "proj/scripts", plan3)), "proj")
})

test_that("a bare name read directly is pointed to its file in a folder below (#568)", {
  # 10_1111_1365_2664_14633: setwd('./data') and then read.csv("x.csv"); the
  # setwd() is taken out, so the name was not found
  plan <- .repro_package_plan(data.frame(file_name = c("x.csv", "m.R"), file_path = c("data/x.csv", "scripts/m.R")))
  rw <- .repro_keep_working_refs(repro_rewrite_paths(c("setwd('./data')", 'd <- read.csv("x.csv")'), "m.R", plan,
                                                     script_path = "scripts/m.R"), plan, "scripts/m.R", ".")
  expect_false(rw$as_written)
  expect_equal(rw$target, "data/x.csv")
  # in a joining call it stays as written: the code joins it to a folder of its own
  rw2 <- .repro_keep_working_refs(repro_rewrite_paths('d <- read.csv(file.path(dir, "x.csv"))', "m.R", plan,
                                                      script_path = "scripts/m.R"), plan, "scripts/m.R", ".")
  expect_true(all(rw2$as_written[rw2$ref == "x.csv"]))
})

test_that("a Git LFS pointer is replaced by the deposit's copy of the file (#568)", {
  # 10_1111_1365_2664_14751: the GitHub archive held pointers, the Zenodo copy the files
  d <- withr::local_tempdir()
  dir.create(file.path(d, "gh")); dir.create(file.path(d, "zen"))
  writeLines(c("version https://git-lfs.github.com/spec/v1", "oid sha256:abc", "size 2000"),
             file.path(d, "gh", "covariates.RDS"))
  saveRDS(1:10, file.path(d, "zen", "other.RDS"))
  writeBin(as.raw(rep(1, 2000)), file.path(d, "zen", "covariates.RDS"))
  writeBin(as.raw(rep(1, 1999)), file.path(d, "zen", "x.RDS"))
  writeLines(c("version https://git-lfs.github.com/spec/v1", "oid sha256:def", "size 1999"),
             file.path(d, "gh", "missing.RDS"))
  src <- file.path(d, c("gh/covariates.RDS", "zen/covariates.RDS", "zen/other.RDS", "gh/missing.RDS"))
  sw <- .repro_lfs_swaps(src)
  expect_equal(sw$pointer, file.path(d, "gh/covariates.RDS"))
  expect_equal(sw$file, file.path(d, "zen/covariates.RDS"))
  expect_equal(nrow(.repro_lfs_swaps(file.path(d, "zen/other.RDS"))), 0)
})

test_that("a package's data downloaded once counts as needing the internet (#568)", {
  expect_true(.repro_needs_internet_error(
    "Error in hy_src(hydat_path) : No Hydat.sqlite3 found at /tmp/.local/share/tidyhydat. Run download_hydat() to download the database."))
})

test_that("a script that stops on a file we did not download was not tried (#568)", {
  # 10_1111_1365_2664_13614: Outputs/BF/BF.rds (653 MB) was over the size limit;
  # another BF.rds, in another folder, was downloaded
  s <- data.frame(
    file_name = c("BF.rds", "BF.rds", "occurrences.csv", "a.csv", "a.csv"),
    file_path = c("Data & code.zip/Data & code/Outputs/BF/BF.rds", "Detectability models/BF.rds",
                  "SCAN.zip/SCAN1/occurrences.csv", "x/a.csv", "y/a.csv"),
    file_location = c(NA, "/tmp/BF.rds", NA, NA, "/tmp/a.csv"),
    file_size = c(652.8 * 1024^2, 2e5, 293 * 1024^2, 10, 10))
  rr <- data.frame(
    file_name = c("fit.R", "detect.R", "occ.py", "plain.R", "ok.R"),
    outcome = c("errored", "errored", "errored", "errored", "ran_ok"),
    error = c("Error in gzfile(file, \"rb\") : cannot open the connection",
              "Error in gzfile(file, \"rb\") : cannot open compressed file 'Detectability models/BF.rds', probable reason 'No such file or directory'",
              "FileNotFoundError: [Errno 2] No such file or directory: '../SCAN.zip/SCAN1/occurrences.csv'",
              "Error in file(file, \"rt\") : cannot open file 'a.csv': No such file or directory", ""),
    stderr = c("cannot open compressed file 'Outputs/BF/BF.rds', probable reason 'No such file or directory'",
               "", "", "", ""))
  nd <- .repro_not_downloaded(rr, s)
  # fit.R and occ.py needed files that were not downloaded; detect.R's file was
  # downloaded, so it looked in the wrong place; for plain.R one a.csv was downloaded
  expect_setequal(nd$script, c("fit.R", "occ.py"))
  expect_equal(nd$file[nd$script == "fit.R"], "Data & code.zip/Data & code/Outputs/BF/BF.rds")
  expect_equal(nrow(.repro_not_downloaded(rr, s[0, ])), 0)
  expect_equal(nrow(.repro_not_downloaded(NULL, s)), 0)
  txt <- .repro_not_downloaded_text(nd, max_file_size = 100)
  expect_match(txt, "2 scripts could not be tried")
  expect_match(txt, "Outputs/BF/BF.rds (652.8 MB)", fixed = TRUE)
  expect_match(txt, "files over 100 MB are not downloaded", fixed = TRUE)
  expect_null(.repro_not_downloaded_text(nd[0, ]))
})

# #568, item 21: the internet during a run, with the limits for the repository's files

test_that(".repro_download_limits keeps the network off unless asked (#568)", {
  expect_null(.repro_download_limits(FALSE))
  expect_equal(.repro_docker_network_args(NULL), c("--network", "none"))
  expect_equal(.repro_docker_counted_cmd(c("python", "a.py"), NULL), c("python", "a.py"))
  net <- .repro_download_limits(TRUE, 100, 500)
  expect_equal(net$max_file, 100 * 1024^2)
  expect_equal(net$max_total, 500 * 1024^2)
  expect_equal(net$used$bytes, 0)
  expect_length(.repro_docker_network_args(net), 0)
  cmd <- .repro_docker_counted_cmd(c("python", "a.py"), net)
  expect_equal(cmd[c(1, 2, 4:6)], c("sh", "-c", "sh", "python", "a.py"))
  # the script's own exit status is what the container returns
  expect_match(cmd[3], "exit $s", fixed = TRUE)
  # no limit
  expect_equal(.repro_download_limits(TRUE, Inf, NULL)$max_total, Inf)
})

test_that("a download that is too large or too slow is the reason a script failed (#568)", {
  r <- .repro_failure_reason
  expect_equal(r("download_too_large", "stopped: ..."), "download")
  expect_equal(r("timed_out", .repro_timeout_text(60, list(arriving = TRUE, received = 5e6))), "download")
  expect_equal(r("timed_out", .repro_timeout_text(60, list(arriving = FALSE, received = 5e6))), "time")
  expect_equal(.repro_timeout_text(60), "timed out after 60s")
})

test_that("the report says the scripts could use the internet, and what stopped one (#568)", {
  net <- .repro_download_limits(TRUE, 100, 500)
  expect_match(.repro_internet_text(net), "up to 100 MB per file and 500 MB for all scripts together", fixed = TRUE)
  expect_match(.repro_needs_internet_text("a.R", TRUE), "could not reach what they asked for", fixed = TRUE)
  expect_match(.repro_needs_internet_text("a.R", FALSE), "which is off during the run", fixed = TRUE)
  tb <- data.frame(file_name = c("a.R", "b.R"), run_order = 1:2, outcome = c("download_too_large", "timed_out"),
                   setwd_removed = 0, not_runnable_reason = NA_character_, unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = c("a.R", "b.R"), outcome = c("download_too_large", "timed_out"),
                   error = c("stopped: it downloaded x.zip, a file of 120 MB, more than the limit of 100 MB per file (max_file_size)",
                             "timed out after 60s while a download was still arriving (12 MB received)"),
                   stderr = "")
  out <- .repro_checklist_section(tb, rr, timeout = 60, net = net)
  expect_match(out, "The scripts could use the internet", fixed = TRUE)
  expect_match(out, "a file of 120 MB", fixed = TRUE)
  expect_match(out, "while it was still downloading", fixed = TRUE)
  expect_match(out, "1 downloaded too much", fixed = TRUE)
  # with the network off, nothing about the internet
  expect_no_match(.repro_checklist_section(tb[2, ], rr[2, ], timeout = 60), "could use the internet", fixed = TRUE)
})

# Real containers and a real download, gated like the other Docker tests.
test_that("repro_run_scripts_docker uses the internet only when asked, within the limits (#568)", {
  skip_if_quick()
  skip_if_offline()
  skip_if_not_installed("processx")
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  img <- "ghcr.io/scienceverse/metacheck_r_large:latest"
  # about 30 KB, so a slow connection does not matter; the limits are below it
  url <- "https://cloud.r-project.org/src/contrib/Archive/R6/R6_2.4.0.tar.gz"
  run1 <- function(code, net, timeout = 120) {
    root <- withr::local_tempdir(.local_envir = parent.frame())
    writeLines(code, file.path(root, "d.R"))
    run_tbl <- data.frame(file_name = "d.R", script_path = file.path(root, "d.R"), run_dir = root)
    repro_run_scripts_docker(run_tbl, "d.R", root, image = img, timeout = timeout, net = net)
  }
  dl <- sprintf('download.file("%s", "x.tar.gz", mode = "wb", quiet = TRUE)', url)

  expect_equal(run1(dl, NULL)$outcome, "errored")
  net <- .repro_download_limits(TRUE, 100, 500)
  expect_equal(run1(dl, net)$outcome, "ran_ok")
  # what it received is counted for the paper's scripts together
  expect_gt(net$used$bytes, 2e4)

  # a file over the limit per file (10 KB), found while the script still runs
  out <- run1(c(dl, "Sys.sleep(30)"), .repro_download_limits(TRUE, 0.01, 500))
  expect_equal(out$outcome, "download_too_large")
  expect_match(out$error, "x.tar.gz", fixed = TRUE)
  expect_lt(out$elapsed, 25)
  # the scripts together over the total
  out <- run1(c(dl, "Sys.sleep(30)"), .repro_download_limits(TRUE, 100, 0.01))
  expect_equal(out$outcome, "download_too_large")
  expect_match(out$error, "max_download_size", fixed = TRUE)
})

test_that("repro_run_scripts_py_docker uses the internet when asked (#568)", {
  skip_if_quick()
  skip_if_offline()
  skip_if_not_installed("processx")
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  root <- withr::local_tempdir()
  writeLines(c("import urllib.request",
               'd = urllib.request.urlopen("https://cloud.r-project.org/src/contrib/Archive/R6/R6_2.4.0.tar.gz").read()',
               "print(len(d))"), file.path(root, "d.py"))
  writeLines(c("import sys", "sys.exit(3)"), file.path(root, "e.py"))
  run_tbl <- data.frame(file_name = c("d.py", "e.py"), script_path = file.path(root, c("d.py", "e.py")), run_dir = root)
  off <- repro_run_scripts_py_docker(run_tbl, c("d.py", "e.py"), root, timeout = 120)
  expect_equal(off$outcome, c("errored", "errored"))
  on <- repro_run_scripts_py_docker(run_tbl, c("d.py", "e.py"), root, timeout = 120,
                                    net = .repro_download_limits(TRUE))
  # the script's own failure is still a failure
  expect_equal(on$outcome, c("ran_ok", "errored"))
})

test_that(".repro_binary_kind tells saved data and other binary files from code (#568)", {
  d <- withr::local_tempdir()
  x <- data.frame(a = 1:3)
  save(x, file = file.path(d, "gz.R"))
  save(x, file = file.path(d, "xz.R"), compress = "xz")
  saveRDS(x, file.path(d, "plain.R"), compress = FALSE)
  writeLines(c("RDA <- 1", "BZh <- 2", "X"), file.path(d, "code.R"))
  writeBin(as.raw(c(0x23, 0x20, 0x63, 0x61, 0x66, 0xe9, 0x0a)), file.path(d, "latin1.R"))
  expect_match(.repro_binary_kind(file.path(d, "gz.R")), "gzip")
  expect_match(.repro_binary_kind(file.path(d, "xz.R")), "xz")
  expect_equal(.repro_binary_kind(file.path(d, "plain.R")), "binary")
  expect_true(is.na(.repro_binary_kind(file.path(d, "code.R"))))
  expect_true(is.na(.repro_binary_kind(file.path(d, "latin1.R"))))
  # the writer keeps such a file instead of writing a script over it
  root <- file.path(d, "root"); dir.create(root)
  file.copy(file.path(d, "gz.R"), file.path(root, "b.R"))
  before <- unname(tools::md5sum(file.path(root, "b.R")))
  suppressMessages(repro_write_scripts(list(b.R = "x <- 1"), list(b.R = data.frame()), NULL, root))
  expect_equal(unname(tools::md5sum(file.path(root, "b.R"))), before)
})

test_that(".repro_needs_arguments: programs that stopped for want of their arguments (#568)", {
  py <- list(a = c("import sys", "f = sys.argv[1]"), b = "import argparse", c = "x = [1][2]")
  err <- c('File "a.py", line 2, in <module>\n    f = sys.argv[1]\nIndexError: list index out of range',
           "a.py: error: the following arguments are required: -i",
           'File "c.py", line 1\nIndexError: list index out of range')
  expect_equal(.repro_needs_arguments(py, err, "Python"), c(TRUE, TRUE, FALSE))
  r <- list(a = "args <- commandArgs(TRUE)", b = "args <- commandArgs(TRUE)", c = "x <- 1")
  err <- c("Error in args[1] : object of type 'closure' is not subsettable",
           "Error in read.csv(\"x.csv\") : cannot open file",
           "Error in args[1] : object of type 'closure' is not subsettable")
  expect_equal(.repro_needs_arguments(r, err, "R"), c(TRUE, FALSE, FALSE))
})

test_that("a file written during the run is found, and copied to where a script looked (#568)", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "a")); dir.create(file.path(root, "b"))
  writeLines("old", file.path(root, "b", "input0.txt"))     # the deposit's own copy elsewhere
  before <- .repro_sandbox_names(root)
  writeLines("new", file.path(root, "a", "input0.txt"))     # written during the run
  writeLines("x", file.path(root, "a", "Ha_daily.Rdata"))
  after <- .repro_sandbox_names(root)
  res <- data.frame(file_name = c("s1.py", "s2.R", "s3.R"), outcome = c("errored", "errored", "errored"),
                    error = c("", "", ""),
                    stderr = c("FileNotFoundError: [Errno 2] No such file or directory: 'input0.txt'",
                               "cannot open compressed file 'Ha_daily.Rdata', probable reason 'No such file or directory'",
                               "cannot open file 'deposit.csv': No such file or directory"))
  later <- .repro_written_later(res, before, after)
  expect_equal(later, list(s1.py = "input0.txt", s2.R = "Ha_daily.Rdata"))
  run_tbl <- data.frame(file_name = c("s1.py", "s2.R"), run_dir = c(file.path(root, "a"), file.path(root, "c")))
  placed <- .repro_place_written(later, res, run_tbl, root, before, after)
  # s1 looks where the file now is; s2 gets a copy in its own folder
  expect_equal(placed$script, "s2.R")
  expect_equal(placed$to, "c/Ha_daily.Rdata")
  expect_true(file.exists(file.path(root, "c", "Ha_daily.Rdata")))
  expect_match(.repro_written_later_text(later, "s2.R", placed), "copied them there|copied it there")
})

test_that("a script that stopped on a Git LFS pointer stopped on a missing file (#568)", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  writeLines(c("version https://git-lfs.github.com/spec/v1",
               "oid sha256:625d8398ce0000000000000000000000000000000000000000000000000000",
               "size 123456"), file.path(root, "data", "results.rds"))
  writeLines("a,b", file.path(root, "data", "ok.csv"))
  res <- data.frame(file_name = c("a.R", "b.R", "c.R"), outcome = c("errored", "errored", "ran_ok"),
                    error = c('Error in readRDS(here::here("data", "results.rds")) : unknown input format',
                              "Error in f() : something else", ""),
                    stderr = "")
  st <- .repro_lfs_stopped(res, root)
  expect_equal(st, list(a.R = "results.rds"))
  expect_match(.repro_lfs_stopped_text(st), "only as a Git LFS pointer")
  expect_equal(.repro_lfs_stopped(res[2:3, ], root), stats::setNames(list(), character(0)))
})

test_that(".repro_upgrade_stan puts the vertical bar in _cdf() and _lpdf() calls (#568)", {
  stan <- c("functions {",
            "  real foo_lpdf(real y, real mu) { return normal_lpdf(y | mu, 1); }",
            "}",
            "model {",
            "  p_zero[i] = logistic_cdf(cutoff, log(growth[i]), 1);",
            "  target += foo_lpdf(y[i], f(a, b));",
            "  target += normal_lcdf(x | 0, 1) + normal_lccdf(z, 0, 1);",
            "}")
  out <- .repro_upgrade_stan(stan)
  txt <- if (is.list(out)) out$txt else out
  expect_equal(txt[2], stan[2])                                   # the definition keeps its commas
  expect_equal(trimws(txt[5]), "p_zero[i] = logistic_cdf(cutoff | log(growth[i]), 1);")
  expect_equal(trimws(txt[6]), "target += foo_lpdf(y[i] | f(a, b));")
  expect_equal(trimws(txt[7]), "target += normal_lcdf(x | 0, 1) + normal_lccdf(z | 0, 1);")
})

test_that("code_read() returns no lines for a file that is not text, but reads UTF-16 (#568)", {
  d <- withr::local_tempdir()
  x <- data.frame(a = 1:3)
  save(x, file = file.path(d, "data.R"))
  out <- code_read(file.path(d, "data.R"))
  expect_length(out, 0)
  expect_match(attr(out, "binary"), "gzip")
  # a script saved as UTF-16 (with and without a byte order mark) is text
  txt <- "x <- 1\ny <- read.csv('a.csv')\n"
  le <- unlist(lapply(utf8ToInt(txt), function(i) as.raw(c(i, 0))))
  writeBin(c(as.raw(c(0xff, 0xfe)), le), file.path(d, "bom.R"))
  writeBin(le, file.path(d, "nobom.R"))
  expect_true(is.na(.binary_kind_raw(readBin(file.path(d, "bom.R"), "raw", 100))))
  expect_true(is.na(.binary_kind_raw(readBin(file.path(d, "nobom.R"), "raw", 100))))
  expect_equal(code_read(file.path(d, "bom.R"))[1], "x <- 1")
})

test_that(".repro_md5 keeps checksums for the session and sees a changed file (#568)", {
  d <- withr::local_tempdir()
  f <- file.path(d, c("a.csv", "b.csv"))
  writeLines("x", f[1]); writeLines("x", f[2])
  h1 <- .repro_md5(f)
  expect_equal(h1[1], h1[2])
  expect_equal(.repro_md5(f), h1)                      # from the cache
  Sys.sleep(1.1); writeLines("a longer line", f[2])    # another size and time
  h2 <- .repro_md5(f)
  expect_equal(h2[1], h1[1])
  expect_false(identical(h2[2], h1[2]))
  expect_true(is.na(.repro_md5(file.path(d, "none.csv"))))
})

test_that("a script stopped by the memory limit says so, with the reason environment (#568)", {
  withr::local_options(metacheck.docker_resource_limits = list(cpus = 2, memory_gb = 3))
  note <- .repro_oom_text(137)
  expect_match(note, "memory limit of 3 GB")
  expect_null(.repro_oom_text(1))
  expect_equal(.repro_failure_reason("errored", paste("Compiling", note)), "environment")
  withr::local_options(metacheck.docker_resource_limits = NULL)
  expect_null(.repro_oom_text(137))
})

test_that("a name kept in a variable that the code joins to a folder is left as written (#568)", {
  plan <- data.frame(file_name = c("Fig_2", "x.csv"), target_path = c("Data/Fig_2", "data/x.csv"),
                     current_path = c("Data/Fig_2", "data/x.csv"))
  # R: the variable is pasted to the folder later
  code <- c('datname <- "Fig_2"', 'f <- paste("Data/", datname, sep = "")', 'd <- read.table(f)')
  rw <- repro_rewrite_paths(code, "Simulate.R", plan)
  expect_true(rw$in_join[rw$ref == "Fig_2"])
  # Python: an f-string after a slash
  rw <- repro_rewrite_paths(c('name = "x.csv"', 'df = pd.read_csv(f"data/{name}")'), "a.py", plan, lang = "Python")
  expect_true(rw$in_join[rw$ref == "x.csv"])
  # a name read directly, also when kept in a variable that is not joined, is not
  rw <- repro_rewrite_paths(c('f <- "x.csv"', 'd <- read.csv(f)'), "b.R", plan)
  expect_false(rw$in_join[rw$ref == "x.csv"])
  rw <- repro_rewrite_paths('d <- read.csv("x.csv")', "c.R", plan)
  expect_false(rw$in_join[rw$ref == "x.csv"])
})

test_that(".repro_install_error_lines finds why a package did not install (#568)", {
  txt <- paste(c("* installing *source* package 'XVector' ...",
                 "io_utils.c:16:10: fatal error: zlib.h: No such file or directory",
                 "ERROR: compilation failed for package 'XVector'",
                 "* installing *source* package 'DESeq2' ...",
                 "ERROR: dependency 'XVector' is not available for package 'DESeq2'",
                 "ERROR: dependencies ‘rgdal’, ‘rgeos’ are not available for package ‘ellipsenm’",
                 "* DONE (other)"), collapse = "\n")
  x <- .repro_install_error_lines(txt, "XVector")
  expect_match(x, "zlib.h", fixed = TRUE)
  expect_match(x, "compilation failed for package 'XVector'", fixed = TRUE)
  expect_false(grepl("DESeq2", x))
  expect_equal(.repro_install_error_lines(txt, "DESeq2"), "ERROR: dependency 'XVector' is not available for package 'DESeq2'")
  expect_match(.repro_install_error_lines(txt, "ellipsenm"), "rgdal")
  expect_equal(.repro_install_error_lines(txt, "other"), "")
})

test_that("Stan programs held in R code get the newer syntax, and other R text is left alone (#568)", {
  r <- c('library(rstan)',
         'write("data{',
         '  int<lower=0> N;',
         '  int<lower=0> y[N];  # counts',
         '}',
         'parameters{ real mu; }',
         'model{ y ~ poisson(exp(mu)); }",',
         '      "m.stan")',
         'x <- "a text with a # and model { inside, on one line"',
         'note <- "not stan:',
         '  y[N] means something else here',
         '"')
  up <- .repro_upgrade_stan_in_r(r)
  expect_true(length(up$changes) > 0)
  expect_equal(trimws(up$txt[4]), "array[N] int<lower=0> y;  // counts")
  expect_equal(up$txt[c(1, 2, 7:12)], r[c(1, 2, 7:12)])      # R code and the other texts unchanged
  # model_code in single quotes
  r2 <- c("m <- stan_model(model_code = 'data {", "  int N;", "  real x[N];", "}", "parameters { real b; }",
          "model { x ~ normal(b, 1); }", "')")
  up2 <- .repro_upgrade_stan_in_r(r2)
  expect_equal(trimws(up2$txt[3]), "array[N] real x;")
  # a distribution with a log link after ~ stays; the old density name in a call is converted
  st <- .repro_upgrade_stan(c("y ~ poisson_log(y_1);", "z ~ neg_binomial_2_log(eta, phi);",
                              "target += poisson_log(n, lambda);", "w ~ normal(0, 1);"))
  expect_equal(st$txt[1:2], c("y ~ poisson_log(y_1);", "z ~ neg_binomial_2_log(eta, phi);"))
  expect_equal(st$txt[3], "target += poisson_lpmf(n | lambda);")
  # no Stan, nothing changed
  expect_equal(.repro_upgrade_stan_in_r(c('a <- "x', 'y"'))$changes, character(0))
})

test_that("a path written into a script gets the changed name its copy has (#575, item 1)", {
  # 10.1002/2688-8319.12229: the copy went to "x_ y/", the script read "x: y/"
  rw <- data.frame(ref = c("Data/x.csv", "a: b/y.csv", "old/"), basename = c("x.csv", "y.csv", "old"),
                   matched = TRUE, target = c("Study: one/Data/x.csv", "a: b/y.csv", "Folder: two/"),
                   ambiguous = FALSE, n_candidates = 1L, is_call = FALSE, as_written = c(FALSE, TRUE, FALSE))
  out <- .repro_apply_rewrites(c('read.csv("Data/x.csv")', "read.csv('a: b/y.csv')", 'd <- "old/"'), rw)
  expect_equal(out, c('read.csv("Study_ one/Data/x.csv")', "read.csv('a_ b/y.csv')", 'd <- "Folder_ two/"'))
  # a path kept as written without such a character stays as it is
  rw2 <- data.frame(ref = "data/x.csv", basename = "x.csv", matched = TRUE, target = "data/x.csv",
                    ambiguous = FALSE, n_candidates = 1L, is_call = FALSE, as_written = TRUE)
  expect_equal(.repro_apply_rewrites('read.csv("data/x.csv")', rw2), 'read.csv("data/x.csv")')
})

test_that("of two files with one name, the one outside another project is chosen (#575, item 3)", {
  # 10.1111/1365-2656.13220: Data/ and NRSA-CA-QC/data/ both hold the file;
  # NRSA-CA-QC/ has scripts of its own
  plan <- .repro_package_plan(data.frame(
    file_name = c("n.csv", "n.csv", "a.Rmd", "b.Rmd"),
    file_path = c("Data/n.csv", "QC/data/n.csv", "R Code/1/a.Rmd", "QC/R/b.Rmd")))
  rw <- repro_rewrite_paths('d <- read.csv("n.csv")', "a.Rmd", plan, script_path = "R Code/1/a.Rmd")
  expect_false(rw$ambiguous)
  expect_equal(rw$target, "Data/n.csv")
  expect_equal(.repro_nearest_copy(c("Data/n.csv", "QC/data/n.csv"), "R Code/1/a.Rmd", plan$current_path), 1L)
  # two studies, each with scripts, and the script above both: no guess
  expect_equal(.repro_nearest_copy(c("s1/data/n.csv", "s2/data/n.csv"), "main.R",
                                   c("main.R", "s1/x.R", "s2/y.R")), integer(0))
  # the copy whose folder shares the most with the script's own
  expect_equal(.repro_nearest_copy(c("other/n.csv", "code/data/n.csv"), "code/a.R", "code/a.R"), 2L)
})

test_that("a name still ambiguous is counted, not kept as written (#575, item 3)", {
  plan <- .repro_package_plan(data.frame(file_name = c("n.csv", "n.csv", "m.R"),
                                         file_path = c("s1/n.csv", "s2/n.csv", "m.R")))
  rw <- repro_rewrite_paths('d <- read.csv("n.csv")', "m.R", plan, script_path = "m.R")
  expect_true(rw$ambiguous)
  kept <- .repro_keep_working_refs(rw, plan, "m.R", ".")
  expect_true(kept$ambiguous)
  expect_false(kept$as_written)
})

test_that("a file the deposit holds elsewhere is copied to where a script looked (#575, item 2)", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code")); dir.create(file.path(root, "b"))
  dir.create(file.path(root, "_lib", "p"), recursive = TRUE)
  writeLines("a,b", file.path(root, "HMSC_YXdata.csv"))
  writeLines("1", file.path(root, "dup.csv")); writeLines("1", file.path(root, "b", "dup.csv"))
  writeLines("1", file.path(root, "two.csv")); writeLines("2", file.path(root, "b", "two.csv"))
  writeLines("x", file.path(root, "_lib", "p", "lib.csv"))
  before <- .repro_sandbox_names(root)
  expect_false(any(grepl("^_lib/", before)))   # the R library is no file of the deposit
  res <- data.frame(file_name = c("S1.R", "S2.R", "S3.R", "S4.R", "S5.R"), outcome = "errored",
                    error = c("Error in file(file, \"rt\") : cannot open the connection",
                              "cannot open file './data/dup.csv': No such file or directory",
                              "cannot open file 'two.csv': No such file or directory",
                              "cannot open file '../../out.csv': No such file or directory",
                              "cannot open file 'lib.csv': No such file or directory"),
                    stderr = c("cannot open file './data/HMSC_YXdata.csv': No such file or directory", "", "", "", ""))
  run_tbl <- data.frame(file_name = res$file_name, run_dir = file.path(root, "code"))
  placed <- .repro_place_from_deposit(res, run_tbl, root, before)
  # one file of the name, or identical copies; not two different files, a path
  # outside the sandbox, or a file of the library
  expect_equal(placed$script, c("S1.R", "S2.R"))
  expect_equal(placed$to, c("code/data/HMSC_YXdata.csv", "code/data/dup.csv"))
  expect_true(file.exists(file.path(root, "code", "data", "HMSC_YXdata.csv")))
  expect_match(.repro_placed_text(placed), "we copied them there", fixed = TRUE)
  # placed once: the second time there is nothing left to copy
  expect_equal(nrow(.repro_place_from_deposit(res, run_tbl, root, before)), 0)
  expect_equal(.repro_placed_text(placed[0, ]), "")
})

test_that("the files sf and terra could not open are read from their errors (#575, item 2)", {
  msgs <- c("Error: Cannot open \"data/x.shp\"; The file doesn't seem to exist.",
            "Error: [rast] file does not exist: data/y.tif")
  expect_setequal(.repro_unopened_files(msgs, full = TRUE), c("data/x.shp", "data/y.tif"))
})

test_that("a folder of the deposit is made when its file could not be copied (#575, item 5)", {
  root <- withr::local_tempdir()
  plan <- data.frame(file_name = c(".dummy", "x.csv"), target_path = c("data/raw/.dummy", "data/x.csv"))
  st <- data.frame(file_name = c(".dummy", "x.csv"), file_location = NA_character_)
  ml <- repro_materialize_layout(plan, st, root)
  expect_false(any(attr(ml, "materialised")$ok))
  expect_true(dir.exists(file.path(root, "data", "raw")))
})

test_that("a path given to here() counts from where here() starts (#575, item 6)", {
  # 10.1111/1365-2656.12852: here("JohnstoneStrait/dateModPars.csv") from
  # scripts/ became here("../data/dateModPars.csv"), above the top
  plan <- .repro_package_plan(data.frame(file_name = c("dateModPars.csv", "sim.R"),
                                         file_path = c("p/data/dateModPars.csv", "p/scripts/sim.R")))
  rw <- repro_rewrite_paths('d <- read.csv(here::here("JohnstoneStrait/dateModPars.csv"))', "sim.R", plan)
  expect_true(rw$in_here)
  kept <- .repro_keep_working_refs(rw, plan, "p/scripts/sim.R", "p/scripts", here_dir = ".")
  expect_equal(kept$target, "p/data/dateModPars.csv")
  # an .Rproj above the run folder is where here() starts
  plan2 <- .repro_package_plan(data.frame(file_name = c("d.csv", "p.Rproj"), file_path = c("p/data/d.csv", "p/p.Rproj")))
  expect_equal(.repro_here_start("p/scripts", NA, plan2), "p")
  expect_equal(.repro_here_start("p/scripts", "q", plan2), "q")
  expect_equal(.repro_here_start("p/scripts", NA, plan), ".")
  # here("../data/x.csv") from scripts/ names data/x.csv
  code <- list(a.R = 'd <- read.csv(here("../data/dateModPars.csv"))')
  expect_equal(unname(.repro_here_roots(code, "p/scripts", plan)), "p/scripts")
})

test_that("the reason comes from the error that stopped the script (#575, item 10)", {
  # 10.1111/1365-2435.12953: a warning on a file, an error on an object
  se <- paste("Error in eval(ei, envir) : object 'egg_hatching_date' not found",
              "Calls: source -> withVisible -> eval -> eval",
              "In addition: Warning message:",
              "In tiff(\"C://your.file.directory/Figure1.tiff\") : TIFFOpen: C://your.file.directory/Figure1.tiff: No such file or directory",
              "Execution halted", sep = "\n")
  expect_equal(.repro_failure_reason("errored", se), "undefined")
  # Stan's "variable does not exist" is no file
  expect_equal(.repro_failure_reason("errored", "Error in eval(e) : Exception: variable does not exist; processing stage=data initialization"),
               "other")
  # "cannot open the connection" names no file: the warning that does counts
  conn <- "Error in file(file, \"rt\") : cannot open the connection\nIn addition: Warning message:\ncannot open file 'x.csv': No such file or directory"
  expect_equal(.repro_failure_reason("errored", conn), "file")
  expect_equal(.repro_failure_reason("errored", "Error: `path` does not exist: 'x.xlsx'"), "file")
  expect_equal(.repro_failure_reason("errored", "FileNotFoundError: [Errno 2] No such file or directory: 'x.csv'"), "file")
})

test_that("a program a script could not start is the reason environment (#575, item 7)", {
  mark <- paste("sh: 1: mark: not found",
                "Error in run.mark.model(model) : ",
                "Output file does not exist. Unable to find or run mark.exe",
                "Execution halted", sep = "\n")
  rsync <- paste("  File \"/usr/local/lib/python3.11/subprocess.py\", line 1955, in _execute_child",
                 "FileNotFoundError: [Errno 2] No such file or directory: 'rsync'", sep = "\n")
  expect_equal(.repro_missing_program(c(mark, rsync, "Error: x", NA)), c("mark", "rsync", NA, NA))
  expect_equal(.repro_failure_reason(c("errored", "errored"), c(mark, rsync)), c("environment", "environment"))
  # a missing data file read by Python is still a file
  expect_true(is.na(.repro_missing_program("FileNotFoundError: [Errno 2] No such file or directory: 'data'")))
  tb <- data.frame(file_name = "m.R", run_order = 1L, outcome = "errored", setwd_removed = 0,
                   not_runnable_reason = NA_character_, unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = "m.R", outcome = "errored", error = "", stderr = mark)
  expect_match(.repro_checklist_section(tb, rr), "<code>m.R</code> starts <code>mark</code>.", fixed = TRUE)
})

test_that("a file renamed before it was shared is named, not used (#575, item 11)", {
  st <- data.frame(file_name = c("Plants.txt", "2_box.data.select.csv", "FSVideoData.csv",
                                 "Data_DRYAD.xlsx", "data_clean.csv", "abcdef.csv"))
  out <- repro_missing_inputs(c("Plants2024.txt", "box.data.select.csv", "VideoData.csv", "var.csv", "x.csv", "abc.csv"),
                              NULL, st)
  expect_equal(out$similar_to[match(c("plants2024.txt", "box.data.select.csv", "videodata.csv"), out$basename)],
               c("plants.txt", "2_box.data.select.csv", "fsvideodata.csv"))
  # nothing alike, or a name too short to tell
  expect_true(all(is.na(out$similar_to[match(c("var.csv", "x.csv", "abc.csv"), out$basename)])))
  # the report names the deposit's file for the script that stopped on it
  tb <- data.frame(file_name = "p.R", run_order = 1L, outcome = "errored", setwd_removed = 0,
                   not_runnable_reason = NA_character_, unresolved_inputs = "plants2024.txt")
  rr <- data.frame(file_name = "p.R", outcome = "errored", error = "",
                   stderr = "Error in file(file, \"rt\") : cannot open the connection\nIn addition: Warning message:\ncannot open file 'Plants2024.txt': No such file or directory")
  expect_match(.repro_checklist_section(tb, rr, missing_inputs = out),
               "<code>p.R</code> reads <code>Plants2024.txt</code>; your deposit holds <code>plants.txt</code>.", fixed = TRUE)
})

test_that("how to make the code run is given per problem, in the order to fix them (#575)", {
  tbl <- data.frame(file_name = c("a.R", "b.R", "c.R", "d.R", "e.R", "f.R", "g.R", "h.R", "i.R"),
                    writes = c(NA, NA, "y.csv", NA, NA, NA, NA, NA, NA))
  rr <- data.frame(file_name = tbl$file_name, error = "",
                   stderr = c("Error in file(file, \"rt\") : cannot open the connection\nIn addition: Warning message:\ncannot open file '/Users/me/x.csv': No such file or directory",
                              "Error in file(file, \"rt\") : cannot open the connection\nIn addition: Warning message:\ncannot open file 'y.csv': No such file or directory",
                              "Error in f() : object 'z' not found",
                              "Error: `funs()` was deprecated in dplyr 0.8.0 and is now defunct.",
                              "Error in library(rgdal) : there is no package called 'rgdal'",
                              "Error in `$<-.data.frame`(x, a, value = 1) : replacement has 1 row, data has 0",
                              "", "", ""))
  outcome <- c("errored", "errored", "errored", "errored", "dependency_unavailable", "errored",
               "needs_a_person", "timed_out", "ran_ok")
  ir <- data.frame(package = "rgdal", installed = FALSE, category = "cran_unavailable")
  adv <- .repro_advice(tbl, rr, outcome, data.frame(basename = character(0)), ir, timeout = 3000)
  # a script that needs another script's file comes first: that one is fixed first
  expect_match(adv[1], "**A script needs a file that another script should have made.**", fixed = TRUE)
  expect_match(adv[1], "<code>b.R</code> needs <code>y.csv</code>, which <code>c.R</code> makes.", fixed = TRUE)
  expect_match(adv, "**A script uses a location that exists only on your computer.**\n\n- <code>a.R</code> reads <code>/Users/me/x.csv</code>.",
               fixed = TRUE, all = FALSE)
  expect_match(adv, "1. Add the file to the deposit.", fixed = TRUE, all = FALSE)
  expect_match(adv, "**Your code was written for older versions of its packages.**", fixed = TRUE, all = FALSE)
  expect_match(adv, "<code>rgdal</code> was removed from CRAN in 2023 and can no longer be installed. Replace it with <code>sf</code> or <code>terra</code>",
               fixed = TRUE, all = FALSE)
  expect_match(adv, "- <code>c.R</code>: <code>z</code>", fixed = TRUE, all = FALSE)
  expect_match(adv, "**A script runs longer than our limit of 50 minutes.**", fixed = TRUE, all = FALSE)
  expect_match(adv, "**A script waits for a person", fixed = TRUE, all = FALSE)
  # a cause it cannot tell is said to be unknown, last
  expect_match(adv[length(adv)], paste0("**Metacheck can't automatically establish what caused the error, ",
                                         "or how it should be improved.**\n\n- <code>f.R</code>"), fixed = TRUE)
  # every script that did not run through is in a block, the one that ran in none
  expect_equal(attr(adv, "scripts"), 1:8)
  # with recorded versions, a run with them is suggested instead of recording them
  rec <- .repro_advice(tbl, rr, outcome, data.frame(basename = character(0)), ir, versions_recorded = TRUE)
  expect_match(rec, "Your deposit records which package versions you used", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("renv::init()</code> in your project", rec, fixed = TRUE)))
  # nothing failed, nothing to say
  expect_no_warning(none <- .repro_advice(tbl[9, ], rr[9, ], "ran_ok", data.frame(basename = character(0))))
  expect_length(none, 0)
})

test_that("the checklist says how to make the code run on another computer (#575)", {
  tb <- data.frame(file_name = "m.R", run_order = 1L, outcome = "errored", setwd_removed = 0,
                   not_runnable_reason = NA_character_, unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = "m.R", outcome = "errored", error = "", stderr = "Error in f() : object 'z' not found")
  out <- .repro_checklist_section(tb, rr)
  expect_match(out, "### How to make your code run on another computer", fixed = TRUE)
  expect_match(out, "**A script uses something it does not create or load itself.**", fixed = TRUE)
  ok <- .repro_checklist_section(tb, transform(rr, outcome = "ran_ok", stderr = ""))
  expect_false(grepl("How to make your code run", ok, fixed = TRUE))
})

test_that("a setwd() call written over several lines is taken out whole (#578, item 1)", {
  # 10.1111/1365-2656.13801: only the first line was replaced, and R stopped on ")"
  sw <- .repro_neutralise_setwd(c("setwd(", '  "SET WORKING DIRECORY HERE"', ")", "x <- 1"))
  expect_length(sw$txt, 4)
  expect_no_error(parse(text = sw$txt))
  expect_equal(sw$n, 1)
  expect_equal(sw$paths, "SET WORKING DIRECORY HERE")
  expect_equal(sw$txt[2:4], c('  # "SET WORKING DIRECORY HERE"', "# )", "x <- 1"))
  # a call on one line, and brackets inside its text, are as before
  one <- .repro_neutralise_setwd(c('setwd("a")', "y <- 2"))
  expect_equal(one$txt[2], "y <- 2")
  expect_equal(one$paths, "a")
  br <- .repro_neutralise_setwd(c('setwd(paste0("a(", "b"))', "y <- 2"))
  expect_equal(br$txt[2], "y <- 2")
  # Python
  py <- .repro_neutralise_setwd(c("os.chdir(", "    '/home/me/x'", ")", "y = 1"), "Python")
  expect_equal(py$txt[2:4], c("    # '/home/me/x'", "# )", "y = 1"))
  expect_equal(py$paths, "/home/me/x")
})

test_that("a folder a script lists is rewritten to where the deposit holds it (#578, item 2)", {
  # 10.1111/1365-2435.14241: list.files() of a folder that is in a zip of the deposit
  plan <- .repro_package_plan(data.frame(file_name = c("a.csv", "b.csv", "m.R"),
    file_path = c("x.zip/top/ir_data/control_forage_data/a.csv", "x.zip/top/ir_data/control_forage_data/b.csv", "scripts/m.R")))
  lf <- .repro_listed_folders('d <- list.files("../data/ir_data/control_forage_data/", full.names = TRUE)', plan, "scripts")
  expect_equal(lf$target, "../x.zip/top/ir_data/control_forage_data/")
  expect_equal(.repro_apply_rewrites('d <- list.files("../data/ir_data/control_forage_data/")', lf),
               'd <- list.files("../x.zip/top/ir_data/control_forage_data/")')
  # a folder that works as written stays
  expect_equal(nrow(.repro_listed_folders('d <- list.files("../x.zip/top/ir_data/control_forage_data")', plan, "scripts")), 0)
  # two folders with the same last parts: no guess
  plan2 <- .repro_package_plan(data.frame(file_name = c("a.csv", "b.csv"),
    file_path = c("s1/ir_data/control_forage_data/a.csv", "s2/ir_data/control_forage_data/b.csv")))
  expect_equal(nrow(.repro_listed_folders('d <- list.files("../data/ir_data/control_forage_data/")', plan2, "scripts")), 0)
  # a glob pattern, and Python
  expect_equal(.repro_listed_folders('f <- Sys.glob("data/control_forage_data/*.csv")', plan, ".")$target,
               "x.zip/top/ir_data/control_forage_data/*.csv")
  py <- .repro_listed_folders(c('f = glob.glob("data/control_forage_data/*.csv")',
                                'g = Path("data/control_forage_data").glob("*.csv")', 'h = Path("data/x.csv")'),
                              plan, ".", "Python")
  expect_equal(py$ref, c("data/control_forage_data/*.csv", "data/control_forage_data"))
})

test_that("a path under the folder of a removed setwd() counts from the working folder (#578, item 3)", {
  # 10.1111/1365-2656.13449: setwd("C:\\specify_directory_here") and a model file in that folder
  code <- c('setwd("C:\\\\proj")', 'x <- read.csv("C:\\\\proj\\\\data\\\\a.csv")', 'y <- "C:/PROJ/b.txt"',
            'z <- "C:\\\\other\\\\c.csv"')
  out <- .repro_strip_setwd_folder(code, .repro_neutralise_setwd(code)$paths)
  expect_equal(out$txt[2:4], c('x <- read.csv("data/a.csv")', 'y <- "b.txt"', 'z <- "C:\\\\other\\\\c.csv"'))
  expect_length(out$paths, 2)
  rows <- .repro_slash_rows(data.frame(file_name = "a.R", slashes_fixed = "",
                                       setwd_folder_removed = paste(out$paths, collapse = "; ")))
  expect_equal(unique(rows$change_type), "path_rewrite")
  # those rows are not Windows paths in the report
  expect_null(.repro_slash_report(data.frame(file_name = "a.R", slashes_fixed = "",
                                             setwd_folder_removed = paste(out$paths, collapse = "; "))))
  # a relative setwd() folder is left alone
  expect_length(.repro_strip_setwd_folder('x <- "data/a.csv"', "data")$paths, 0)
})

test_that("a folder in a variable made with paste0() becomes the run folder when its file is found (#578, item 4)", {
  # 10.1111/1365-2435.14620: main_address = paste0("~/Library/.../"), set twice
  expect_equal(.repro_simple_string_vars('a = paste0("~/x/")'), character(0))
  expect_equal(unname(.repro_simple_string_vars('a = paste0("~/x/")', calls = TRUE)), "~/x/")
  code <- c('main_address = paste0("~/x/")',
            'd <- read_xlsx(file.path(main_address, "sub/a.xlsx"))',
            'dir.create(file.path(main_address, "Outputs"))',
            'main_address = "~/x"')
  plan <- .repro_package_plan(data.frame(file_name = c("a.xlsx", "m.R"), file_path = c("D.zip/D/a.xlsx", "m.R")))
  rw <- .repro_keep_working_refs(repro_rewrite_paths(code, "m.R", plan, script_path = "m.R"), plan, "m.R", ".")
  fv <- .repro_folder_vars(code, plan, ".", rw = rw)
  expect_true(all(fv$keep_pieces))
  out <- .repro_apply_rewrites(code, .repro_apply_folder_vars(rw, fv))
  expect_equal(out, c('main_address = paste0("./")', 'd <- read_xlsx(file.path(main_address, "D.zip/D/a.xlsx"))',
                      'dir.create(file.path(main_address, "Outputs"))', 'main_address = "./"'))
})

test_that("four more messages of text that is no longer a factor start the old default (#578, item 5)", {
  rr <- function(m) data.frame(file_name = "s.R", outcome = "errored", error = "", stderr = m)
  expect_equal(.repro_old_defaults(rr("Error in `levels<-`(x, value = l) : factor level [3] is duplicated"))$s.R, "factors")
  expect_equal(.repro_old_defaults(rr("Error in UseMethod(\"droplevels\") : \n  no applicable method for 'droplevels' applied to an object of class \"character\""))$s.R,
               "factors")
  expect_equal(.repro_old_defaults(rr("Error in standardize(m) : The object area must be either numeric or a factor."))$s.R, "factors")
  xlim <- "Error in plot.window(...) : need finite 'xlim' values"
  expect_equal(.repro_old_defaults(rr(paste0(xlim, "\nIn addition: Warning message:\nIn xy.coords(x, y, xlabel, ylabel, log) :\n  NAs introduced by coercion")))$s.R,
               "factors")
  # without the warning, it has other causes
  expect_length(.repro_old_defaults(rr(xlim)), 0)
})

test_that("a program that prints how to use it needs arguments (#578, item 6)", {
  # 10.1111/1365-2656.13545: Stacks tools that print their usage and stop
  py <- list(t = c("import sys", "try:", "    input_vcf = sys.argv[1]", "except:", "    print(__doc__)", "    sys.exit(1)"))
  expect_true(.repro_needs_arguments(py, "the script stopped without a message", "Python",
    out = "Keep only the first SNP...\n\nUsage:\n    ./extract_first_snp.py input_vcf output_vcf\n"))
  r <- list(o = c("library(optparse)", "opt <- parse_args(OptionParser(option_list = l))",
                  'if (is.null(opt$prefix)) { print_help(p); stop("Please provide the prefix") }'))
  expect_true(.repro_needs_arguments(r, "Error: Please provide the prefix", "R",
    out = "Usage: popgen.R [options]\n\t-h, --help\n\t\tShow this help message and exit"))
  # no arguments read, the word "usage" in its results
  expect_false(.repro_needs_arguments(list(a = "x <- 1"), "Error: boom", "R", out = "usage: 3 times"))
  # without the output, as before
  expect_false(.repro_needs_arguments(py, "the script stopped without a message", "Python"))
})

test_that("a script that ran out of memory says so, also when a compiler did (#578, item 8)", {
  # 10.1111/1365-2435.14319: g++ ran out of memory compiling a Stan model
  gpp <- "g++: fatal error: Killed signal terminated program cc1plus"
  expect_equal(.repro_out_of_memory(c(gpp, "Error: Killed: wrong input", "Traceback\nMemoryError",
                                      "Error: cannot allocate vector of size 2.3 Gb", NA)),
               c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(.repro_failure_reason("out_of_memory", gpp), "environment")
  withr::local_options(metacheck.docker_resource_limits = list(cpus = 2, memory_gb = 4))
  tb <- data.frame(file_name = "m.R", run_order = 1L, outcome = "out_of_memory", setwd_removed = 0,
                   not_runnable_reason = NA_character_, unresolved_inputs = NA_character_)
  rr <- data.frame(file_name = "m.R", outcome = "out_of_memory", error = "", stderr = gpp)
  out <- .repro_checklist_section(tb, rr)
  expect_match(out, "1 ran out of memory", fixed = TRUE)
  expect_match(out, "It needed more than the 4 GB of memory it could use here.", fixed = TRUE)
  expect_match(out, "**A script needs more memory than our limit of 4 GB.**", fixed = TRUE)
})
