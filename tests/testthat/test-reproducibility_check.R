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

  # the Psych-DS layout moves the helper module, so the import fails
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = pkg,
                                    local_only = TRUE, execute = TRUE, layout = "psychds"))
  oc <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(oc[["main.py"]], "errored")
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
