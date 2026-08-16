# Module-level tests for "reproducibility_check" (inst/modules/
# reproducibility_check.R). Static-analysis (execute = FALSE, the default)
# tests run offline and unconditionally; execute = TRUE tests actually spawn
# real R subprocesses via callr against tests/testthat/fixtures/repro/ scripts
# (base R only, no library() calls, so no install_missing/network dependency)
# and are gated with skip_if_quick() the same way this suite gates other
# slow/environment-dependent tests. Unit tests for the individual static/
# execution HELPER functions live in test-reproducibility_check.R.

repro_fixture <- function(...) test_path("fixtures", "repro", ...)

# Feed reproducibility_check() fake upstream module output the same way
# test-module-code_check.R's "downloaded files keep their analysis results"
# test does: wrap a hand-built table in a `metacheck_module_output`-classed
# list and chain it through module_run(), so get_prev_outputs() inside the
# module sees it exactly as it would from a real code_check()/data_check()/
# psychds_check() run.
fake_module_output <- function(module, table, paper, extra = list()) {
  out <- c(list(module = module, table = table, paper = paper,
                summary_table = data.frame(paper_id = paper_id(paper))),
          extra)
  class(out) <- "metacheck_module_output"
  out
}

code_tbl_row <- function(paper, file_name, file_location, language = "R",
                         packages = "", parse_error = FALSE) {
  data.frame(paper_id = paper_id(paper), file_name = file_name,
            file_location = file_location, file_url = NA_character_,
            language = language, packages = packages,
            parse_error = parse_error, stringsAsFactors = FALSE)
}


# static analysis (execute = FALSE, the default) ----

test_that("reproducibility_check is registered and returns na with no code files", {
  mods <- module_list()
  expect_true("reproducibility_check" %in% mods$name)

  paper <- test_paper("no code here")
  mo <- module_run(paper, "reproducibility_check")
  expect_equal(mo$traffic_light, "na")
  expect_equal(nrow(mo$table), 0)
})

test_that("static analysis: a runnable single script is green with no execution", {
  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "ok.R", repro_fixture("ok.R"))
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = c("ok.R", "data.csv"),
    file_location = c(repro_fixture("ok.R"), repro_fixture("data.csv")))

  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(
    data_check = list(structure = structure_df))

  # chain data_check's structure output in via prev_outputs directly
  # (module_run() only auto-populates prev_outputs from ONE chained
  # metacheck_module_output at a time; setting the field ourselves lets this
  # test supply both code_check's table AND data_check's structure without a
  # real data_check()/psychds_check() run).
  mo <- module_run(fake_code, "reproducibility_check")

  expect_equal(mo$table$file_name, "ok.R")
  expect_true(mo$table$runnable)
  expect_true(is.na(mo$table$parses) == FALSE && mo$table$parses)
  expect_null(mo$table$outcome[1] |> (\(x) if (!is.na(x)) x else NULL)())
  expect_match(mo$summary_text, "static analysis; no code was run", fixed = TRUE)
})

test_that("static analysis: a missing input is diagnosed and not treated as produced", {
  # repro_rewrite_paths() (called per file to build the missing-input
  # candidate list) returns an early, EMPTY result whenever the psychds_check
  # plan itself is NULL or has 0 rows (R/reproducibility_check.R's own
  # early-return guard) -- with no plan at all, nothing is ever marked
  # unmatched, so missing-input diagnosis silently finds nothing to report
  # even for a script reading a file that plainly is not in the repository.
  # A real paper always has SOME psychds_check plan by the time
  # reproducibility_check runs, so this test supplies one (with an unrelated
  # row, so it is non-empty without itself resolving the reference) to
  # exercise the diagnosis rather than trip the early-return.
  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "missing_input.R", repro_fixture("missing_input.R"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "missing_input.R",
                             file_location = repro_fixture("missing_input.R"))
  plan_df <- data.frame(file_name = "some_other_file.csv",
                        target_path = "study-ex1/data/some_other_file.csv")

  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))
  mo <- module_run(fake_code, "reproducibility_check")

  expect_false(mo$table$runnable)
  expect_equal(mo$table$not_runnable_reason, "missing_input")
  expect_match(mo$table$unresolved_inputs, "not_in_repo.csv")
})

test_that("static analysis: read-after-write orders a two-script pipeline", {
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "writes_then_reads.R", repro_fixture("writes_then_reads.R")),
    code_tbl_row(paper, "reads_written.R", repro_fixture("reads_written.R")))
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = "data.csv",
    file_location = repro_fixture("data.csv"))

  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  ord <- stats::setNames(mo$table$run_order, mo$table$file_name)
  expect_lt(ord[["writes_then_reads.R"]], ord[["reads_written.R"]])
})

test_that("static analysis: SPSS data with no .sps syntax is a red finding even with no R code", {
  paper <- test_paper()
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.sav",
                             file_location = NA_character_)
  fake_data <- fake_module_output("data_check", data.frame(), paper,
                                  extra = list(structure = structure_df))
  mo <- module_run(fake_data, "reproducibility_check")

  expect_equal(mo$traffic_light, "red")
  expect_match(paste(mo$report, collapse = "\n"), "SPSS data without syntax")
})

test_that("static analysis: SPSS data WITH syntax is yellow, not red, and not na", {
  paper <- test_paper()
  structure_df <- data.frame(paper_id = paper_id(paper),
                             file_name = c("data.sav", "syntax.sps"),
                             file_location = c(NA_character_, NA_character_))
  fake_data <- fake_module_output("data_check", data.frame(), paper,
                                  extra = list(structure = structure_df))
  mo <- module_run(fake_data, "reproducibility_check")

  expect_equal(mo$traffic_light, "yellow")
  expect_match(paste(mo$report, collapse = "\n"), "SPSS data\\b")
})

test_that("static analysis: Stata .do with no data or output names the gap explicitly", {
  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "analysis.do", NA_character_, language = "Stata")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "analysis.do",
                             file_location = NA_character_)
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  expect_equal(mo$traffic_light, "info")
  expect_match(paste(mo$report, collapse = "\n"), "Stata code without data or output")
})

test_that("static analysis: byte-identical duplicate mirrors are only run once", {
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "ok.R", repro_fixture("ok.R")),
    code_tbl_row(paper, "ok.R", repro_fixture("ok.R")))  # same file, listed twice
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  expect_equal(nrow(mo$table), 1)
  expect_match(paste(mo$report, collapse = "\n"), "Duplicate files across repo mirrors")
})


# execute = TRUE (spawns real R subprocesses; slow) ----

test_that("execute = TRUE actually runs a script and records ran_ok", {
  skip_if_quick()
  skip_if_not_installed("callr")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "ok.R", repro_fixture("ok.R"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  # the execute phase only copies data into the sandbox for files the
  # psychds_check PLAN names a target_path for (repro_materialize_layout()) —
  # with no plan at all, data.csv never reaches the sandbox and every fixture
  # script here fails on "cannot open the connection" regardless of its own
  # code. target_path = "data.csv" keeps it at the sandbox root, matching
  # where these (deliberately un-nested) fixture scripts read it from.
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check",
                   execute = TRUE, timeout = 60)

  expect_equal(mo$table$outcome, "ran_ok")
  expect_equal(mo$run_results$outcome, "ran_ok")
  expect_match(mo$run_results$stdout, "Welch Two Sample t-test")
  expect_match(mo$summary_text, "assessed AND ran", fixed = TRUE)
})

test_that("execute = TRUE: a script that errors forces the traffic light red", {
  skip_if_quick()
  skip_if_not_installed("callr")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "errors.R", repro_fixture("errors.R"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_equal(mo$traffic_light, "red")
  expect_equal(mo$table$outcome, "errored")
  expect_match(mo$run_results$error, "deliberate failure", fixed = TRUE)
})

test_that("execute = TRUE: setwd() is stripped and reported, and the script still runs", {
  skip_if_quick()
  skip_if_not_installed("callr")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "bad_setwd.R", repro_fixture("bad_setwd.R"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_equal(mo$table$outcome, "ran_ok")
  expect_match(paste(mo$report, collapse = "\n"), "setwd", fixed = TRUE)
})

test_that("execute = TRUE: console output feeds stat_output and match_reported_output", {
  skip_if_quick()
  skip_if_not_installed("callr")

  # A manuscript sentence reporting the exact t/df/p the fixture script's
  # t.test() will print, so match_reported_output() (run inside the module)
  # has something real to find in the executed console output.
  paper <- test_paper("t(4.96) = -4.04, p = .010.")
  code_tbl <- code_tbl_row(paper, "ok.R", repro_fixture("ok.R"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_gt(length(mo$stat_output), 0)
  expect_gt(nrow(mo$match_table), 0)
  expect_true(any(mo$match_table$found))
})

test_that("execute = TRUE: a model described across several statements unites into one match site", {
  # Regression test for the .r_call_object_ref() fix: model_object.R fits one
  # model (`m <- lm(...)`), then describes it two more ways — `anova(m)` (a
  # plain fn(bare_name) call, already worked before the fix) and
  # `s$coefficients[2, , drop = FALSE]` (a $/[-chain rooted at `s`, which
  # .r_root_ref_map() traces back to `m` via `s <- summary(m)`, then further
  # to `dat` via `m <- lm(x ~ g, data = dat)` — case 2's argument scan is
  # what makes m's OWN model_ref resolve to "dat" in the first place). All
  # three statements must end up tagged with the SAME model_ref, or
  # match_reported_output() can never see the model's F-test and its own
  # coefficient row as one site. See R/r-output.R's .r_call_object_ref()
  # header comment for the real corpus-paper case this generalises from (an
  # aov_car() ANOVA table printed bare, its eta-squared via a fn(model)
  # call, its CI via a fn(model$field[i]) call — three statements describing
  # one model that previously never united).
  skip_if_quick()
  skip_if_not_installed("callr")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "model_object.R", repro_fixture("model_object.R"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)
  expect_equal(mo$table$outcome, "ran_ok")

  long <- dplyr::bind_rows(lapply(mo$stat_output, `[[`, "long"))
  # both the anova(m) table (F/p) and the s$coefficients[2,] row (the
  # coefficient/SE/t/p for "gb") must share one model_ref, so
  # match_reported_output() can treat them as one candidate site.
  refs <- unique(long$model_ref[!is.na(long$model_ref)])
  expect_length(refs, 1)
  expect_true(any(long$statistic == "F value" | grepl("F", long$statistic)))
})

test_that("execute = TRUE + install_missing installs a real CRAN dependency", {
  skip_if_quick()
  skip_if_not_installed("callr")
  skip_if_offline()
  skip_on_cran()

  tmp <- withr::local_tempdir()
  script <- file.path(tmp, "needs_pkg.R")
  # a tiny, fast-installing base-adjacent package used only to prove the
  # install path works end to end
  writeLines(c('library(digest)', 'digest::digest("x")'), script)

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "needs_pkg.R", script, packages = "digest")
  structure_df <- data.frame(paper_id = character(0), file_name = character(0),
                             file_location = character(0))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))

  mo <- module_run(fake_code, "reproducibility_check",
                   execute = TRUE, install_missing = TRUE, timeout = 300)

  expect_equal(mo$table$outcome, "ran_ok")
  expect_true(any(mo$install_results$package == "digest" & mo$install_results$installed))
})
