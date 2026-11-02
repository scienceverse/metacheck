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

test_that("execute = TRUE: a script another script source()s runs only inside that script", {
  # main.R loads dplyr and then sources a.R, which uses %>% without loading
  # anything: run on its own, a.R fails, so it is only run inside main.R.
  skip_if_quick()
  skip_if_not_installed("callr")
  skip_if_not_installed("dplyr")

  tmp <- withr::local_tempdir()
  writeLines(c("library(dplyr)", 'source("a.R")'), file.path(tmp, "main.R"))
  writeLines(c("x <- data.frame(v = 1:3) %>% mutate(w = v * 2)", "print(x)"),
             file.path(tmp, "a.R"))
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "main.R", file.path(tmp, "main.R")),
    code_tbl_row(paper, "a.R", file.path(tmp, "a.R")))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = data.frame(
    paper_id = character(0), file_name = character(0), file_location = character(0))))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  oc <- stats::setNames(mo$table$outcome, mo$table$file_name)
  expect_equal(oc[["main.R"]], "ran_ok")
  expect_equal(oc[["a.R"]], "run_by_source")
  expect_equal(mo$traffic_light, "green")
  expect_match(paste(mo$report, collapse = "\n"), "Run by another script", fixed = TRUE)
})

test_that("execute = TRUE: a script relying on another script's library() calls gets them added", {
  # 1_prep.R loads dplyr; 2_analysis.R uses %>% and mutate() without loading
  # it, and nothing sources it. The re-run adds library(dplyr) to it.
  skip_if_quick()
  skip_if_not_installed("callr")
  skip_if_not_installed("dplyr")

  tmp <- withr::local_tempdir()
  writeLines(c("library(dplyr)", "y <- 1"), file.path(tmp, "1_prep.R"))
  writeLines(c("x <- data.frame(v = 1:3) %>% mutate(w = v * 2)", "print(x)"),
             file.path(tmp, "2_analysis.R"))
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "1_prep.R", file.path(tmp, "1_prep.R")),
    code_tbl_row(paper, "2_analysis.R", file.path(tmp, "2_analysis.R")))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = data.frame(
    paper_id = character(0), file_name = character(0), file_location = character(0))))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  oc <- stats::setNames(mo$table$outcome, mo$table$file_name)
  expect_equal(oc[["2_analysis.R"]], "ran_ok")
  inj <- mo$modifications[mo$modifications$change_type == "library_injected", ]
  expect_equal(inj$file_name, "2_analysis.R")
  expect_equal(inj$detail, "library(dplyr)")
  expect_match(paste(mo$report, collapse = "\n"),
               "Reproducible after adding `library()` calls: 2_analysis.R", fixed = TRUE)
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


# Recorded package versions ----

# A folder with one script and an renv.lock that lists the given packages.
recorded_folder <- function(code, lock, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  writeLines(code, file.path(d, "analysis.R"))
  pkgs <- lapply(names(lock), function(p)
    list(Package = p, Version = lock[[p]], Source = "Repository", Repository = "CRAN"))
  names(pkgs) <- names(lock)
  jsonlite::write_json(list(R = list(Version = "4.3.1"), Packages = pkgs),
                       file.path(d, "renv.lock"), auto_unbox = TRUE)
  d
}

test_that("a recorded version is installed with recorded_versions = TRUE, with a warning", {
  skip_if_quick()
  skip_if_not_installed("callr")
  skip_if_offline()
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))
  d <- recorded_folder(
    c("library(R6)", 'cat("VERSION", as.character(packageVersion("R6")), "\\n")'),
    list(R6 = "2.4.0", notloaded = "1.0"))
  expect_warning(
    mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE,
                                      recorded_versions = TRUE)),
    "The R version is not changed", fixed = TRUE)
  expect_equal(mo$run_results$outcome, "ran_ok")
  expect_match(mo$run_results$stdout, "VERSION 2.4.0", fixed = TRUE)
  # only the loaded package
  expect_equal(mo$install_results$package, "R6")
  expect_equal(mo$install_results$version_installed, "2.4.0")
  expect_equal(mo$recorded_versions$deps$recorded_from, "renv.lock")
  report_txt <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report_txt, "Recorded package versions", fixed = TRUE)
  expect_match(report_txt, "The R version is not changed", fixed = TRUE)
  expect_no_match(report_txt, "Running against current package versions", fixed = TRUE)
  expect_match(mo$summary_text, "did not change the R version", fixed = TRUE)
  expect_match(paste(mo$checklist_section, collapse = "\n"), "did not change the R version", fixed = TRUE)
})

test_that("recorded versions give way to an explicit setting", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- recorded_folder("x <- 1", list(R6 = "2.4.0"))
  writeLines(c("library(R6)", "x <- 1"), file.path(d, "analysis.R"))
  called <- 0L
  local_mocked_bindings(repro_install_deps = function(...) {
    called <<- called + 1L
    data.frame(package = character(0), installed = logical(0))
  })
  run <- function(...) suppressWarnings(suppressMessages(module_run(
    test_paper(), "reproducibility_check", local_path = d, execute = TRUE, ...)))
  expect_null(run(recorded_versions = TRUE, install_missing = FALSE)$recorded_versions)
  expect_null(run(recorded_versions = FALSE)$recorded_versions)
  # the default
  expect_null(run()$recorded_versions)
  expect_equal(called, 0L)
})

test_that("a failed run recommends recorded versions when the repository records them", {
  skip_if_quick()
  skip_if_not_installed("callr")
  advice <- "We recommend retrying the reproducibility check with `recorded_versions = TRUE`."
  server <- "this setting is not implemented on this shared server"
  run <- function(d, ...) suppressWarnings(suppressMessages(module_run(
    test_paper(), "reproducibility_check", local_path = d, execute = TRUE, ...)))
  text <- function(mo) paste(unlist(mo$report), collapse = "\n")

  d <- recorded_folder(c("library(R6)", 'stop("this script fails")'), list(R6 = "2.4.0"))
  mo <- run(d)
  expect_match(text(mo), advice, fixed = TRUE)
  expect_match(mo$summary_text, advice, fixed = TRUE)
  expect_match(paste(mo$checklist_section, collapse = "\n"), advice, fixed = TRUE)
  expect_no_match(text(mo), server, fixed = TRUE)

  # on a shared server it says the setting is not available
  withr::with_options(list(metacheck.keys.shared = TRUE), {
    mo <- run(d)
    expect_match(text(mo), server, fixed = TRUE)
    expect_match(mo$summary_text, server, fixed = TRUE)
    expect_match(paste(mo$checklist_section, collapse = "\n"), server, fixed = TRUE)
  })

  # it cannot work with nothing installed
  expect_no_match(text(run(d, install_missing = FALSE)), advice, fixed = TRUE)

  # no failure
  ok <- recorded_folder(c("library(R6)", "x <- 1"), list(R6 = "2.4.0"))
  expect_no_match(text(run(ok)), advice, fixed = TRUE)

  # no record
  none <- withr::local_tempdir()
  writeLines(c("library(R6)", 'stop("this script fails")'), file.path(none, "analysis.R"))
  expect_no_match(text(run(none)), advice, fixed = TRUE)
})

test_that("without a record of versions nothing is installed by default", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  writeLines("x <- 1", file.path(d, "analysis.R"))
  called <- 0L
  local_mocked_bindings(repro_install_deps = function(...) { called <<- called + 1L; NULL })
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE))
  expect_equal(called, 0L)
  expect_null(mo$recorded_versions)
  expect_match(paste(mo$checklist_section, collapse = "\n"), "We did not install packages", fixed = TRUE)
})

test_that("a run without execute says what a run would install, and installs nothing", {
  d <- recorded_folder("library(R6)", list(R6 = "2.4.0"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                    recorded_versions = TRUE))
  expect_match(paste(unlist(mo$report), collapse = "\n"), "would install 1 package", fixed = TRUE)
  expect_null(mo$recorded_versions)
})

test_that("a script's groundhog and renv calls are replaced when recorded versions are used", {
  skip_if_quick()
  skip_if_not_installed("callr")
  skip_if_offline()
  withr::local_options(repos = c(CRAN = "https://cloud.r-project.org"))
  d <- recorded_folder(
    c("library(groundhog)", 'groundhog.library("R6", "2020-01-01")', "renv::restore()",
      'cat("VERSION", as.character(packageVersion("R6")), "\\n")'),
    list(R6 = "2.4.0"))
  mo <- suppressWarnings(suppressMessages(
    module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE, recorded_versions = TRUE)))
  expect_equal(mo$run_results$outcome, "ran_ok")
  expect_match(mo$run_results$stdout, "VERSION 2.4.0", fixed = TRUE)
  expect_true("env_call_replaced" %in% mo$modifications$change_type)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "3 calls to `install.packages()`", fixed = TRUE)
})

# execute = TRUE, sandbox = "docker", workers > 1 (concurrent multi-paper) ----
# Real end-to-end: real Docker containers, real callr background processes,
# skipped whenever Docker/callr are unavailable (CI without Docker, quick
# local runs) the same way the process-sandbox execute = TRUE tests above are
# gated on skip_if_quick(). Each paper's own upstream module output is fed
# via tables_dir (reproducibility_check()'s own documented "prior full
# build" mechanism -- see its own roxygen), since a background process
# started by .reproducibility_check_batch() has no live module_run() chain
# to read prev_outputs from the way module_run(fake_code, ...) does above.
test_that("workers > 1 runs a paperlist's docker checks concurrently and combines results", {
  skip_if_quick()
  skip_if_not_installed("callr")
  docker_ok <- tryCatch(repro_docker_available(), error = function(e) list(ok = FALSE))
  skip_if_not(isTRUE(docker_ok$ok), "Docker not available")

  papers <- lapply(1:3, function(i) test_paper())
  names(papers) <- vapply(papers, paper_id, character(1))
  plist <- structure(papers, class = c("scivrs_paperlist", "list"))

  tables_dir <- withr::local_tempdir()
  for (p in papers) {
    pid <- paper_id(p)
    code_tbl <- code_tbl_row(p, "ok.R", repro_fixture("ok.R"))
    structure_df <- data.frame(paper_id = pid, file_name = "data.csv",
                               file_location = repro_fixture("data.csv"))
    plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
    saveRDS(list(paper_id = pid, modules = list(
      code_check = list(table = code_tbl),
      data_check = list(structure = structure_df),
      psychds_check = list(table = plan_df)
    )), file.path(tables_dir, paste0(pid, ".rds")))
  }

  results_dir <- withr::local_tempdir()
  t0 <- Sys.time()
  mo <- module_run(plist, "reproducibility_check",
                   execute = TRUE, sandbox = "docker", workers = 2,
                   timeout = 120, tables_dir = tables_dir,
                   results_dir = results_dir)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_setequal(mo$summary_table$paper_id, names(papers))
  expect_setequal(mo$table$paper_id, names(papers))
  expect_true(all(mo$table$outcome == "ran_ok"))

  # each paper wrote its own <paper_id>.rds via capture_module_tables() as it
  # finished, not just once the whole batch was done
  expect_setequal(tools::file_path_sans_ext(list.files(results_dir)), names(papers))

  # weak timing check: 3 papers at 2 workers cannot be FASTER than the
  # slowest single container run allows, but should be well short of running
  # all 3 fully sequentially (a loose bound -- this asserts overlap happened,
  # not a specific speedup factor, since container startup time varies by
  # machine).
  expect_lt(elapsed, 3 * 60)
})


# Python code path ----
# Mirrors the R sections above exactly, against the Python sibling fixtures
# (tests/testthat/fixtures/repro/*.py -- see that directory's README.md).
# Static-analysis tests run unconditionally, like the R ones; execute = TRUE
# tests are gated BOTH on skip_if_quick() (slow/environment-dependent, same
# as the R execute tests) AND on a real python/python3 interpreter actually
# being on PATH, since (unlike R, which this whole package depends on) a
# Python interpreter is not a given on every machine running this suite.

# Sys.which("python") vs Sys.which("python3"): checked once here (not
# per-test) since BOTH resolve to a real, usable interpreter on the Windows
# machine this was developed against (confirmed directly: `python --version`
# and `python3 --version` both report a real Python 3.11 install, not a
# Microsoft Store stub) -- repro_run_scripts_py()'s own default python_bin is
# "python", so that name is preferred when it resolves, with "python3" as a
# fallback for a machine where only that name is on PATH (common outside
# Windows). Neither resolving skips every execute = TRUE Python test below.
.repro_py_bin <- if (nzchar(Sys.which("python"))) "python" else
  if (nzchar(Sys.which("python3"))) "python3" else NA_character_

test_that("static analysis: a runnable Python script is green with no execution", {
  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "ok.py", repro_fixture("ok.py"), language = "Python")
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = c("ok.py", "data.csv"),
    file_location = c(repro_fixture("ok.py"), repro_fixture("data.csv")))

  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  expect_equal(mo$table$file_name, "ok.py")
  expect_true(mo$table$runnable)
  expect_match(paste(mo$report, collapse = "\n"), "Python code", fixed = TRUE)
  expect_match(mo$summary_text, "static analysis; no code was run", fixed = TRUE)
})

test_that("static analysis: a Python missing input is diagnosed and not treated as produced", {
  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "missing_input.py", repro_fixture("missing_input.py"),
                           language = "Python")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "missing_input.py",
                             file_location = repro_fixture("missing_input.py"))
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

test_that("static analysis: a two-script Python pipeline is ordered by read-after-write", {
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "writes_then_reads.py", repro_fixture("writes_then_reads.py"),
                language = "Python"),
    code_tbl_row(paper, "reads_written.py", repro_fixture("reads_written.py"),
                language = "Python"))
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = "data.csv",
    file_location = repro_fixture("data.csv"))

  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  ord <- stats::setNames(mo$table$run_order, mo$table$file_name)
  expect_lt(ord[["writes_then_reads.py"]], ord[["reads_written.py"]])
  # open("intermediate.csv", "w") names the path like a read; the script that
  # writes it was "missing_input" and not run (U235)
  expect_true(all(mo$table$runnable))
})

test_that("static analysis: an R script that reads back the file it saved is runnable (U235)", {
  paper <- test_paper()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c('d <- read.csv("data.csv")', 'saveRDS(d, "model.rds")',
               'm2 <- readRDS("model.rds")'), script)
  code_tbl <- code_tbl_row(paper, "cache.R", script)
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")
  expect_true(mo$table$runnable)
})

test_that("static analysis: a name built at run time is not predicted to be missing (#568)", {
  paper <- test_paper()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c('x <- "a"', 'd <- read.csv(paste0(x, "_field.csv"))',
               'e <- read.csv(sprintf("%s/results.csv", x))', 'f <- read.csv("absent.csv")',
               'g <- read.csv(paste0(x, "field.csv"))', 'h <- read.csv(paste0(x, "/site.csv"))',
               'i <- read.csv(paste0("data/", "whole.csv"))', 'j <- read.csv("sub\\\\windows.csv")'), script)
  code_tbl <- code_tbl_row(paper, "built.R", script)
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")
  expect_setequal(strsplit(mo$table$unresolved_inputs, ", ", fixed = TRUE)[[1]],
                  c("absent.csv", "whole.csv", "windows.csv"))
  expect_equal(mo$summary_table$repro_missing_inputs, 3)
})

test_that("static analysis: a Python name joined to a variable is not predicted to be missing (#568)", {
  paper <- test_paper()
  script <- withr::local_tempfile(fileext = ".py")
  writeLines(c('import pandas as pd', 'x = "a"', 'd = pd.read_csv(x + "field.csv")',
               'e = pd.read_csv("absent.csv")'), script)
  code_tbl <- code_tbl_row(paper, "built.py", script, language = "Python")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")
  expect_equal(mo$table$unresolved_inputs, "absent.csv")
})

test_that("execute = TRUE: a predicted missing file does not colour the light of a script that ran (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  writeLines(c('if (file.exists("absent.csv")) d <- read.csv("absent.csv")', 'cat("done\\n")'),
             file.path(d, "guarded.R"))
  file.copy(repro_fixture("data.csv"), file.path(d, "data.csv"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE,
                                    layout = "psychds"))
  expect_equal(mo$run_results$outcome, "ran_ok")
  expect_equal(mo$summary_table$repro_missing_inputs, 1)
  expect_equal(mo$traffic_light, "green")
})

test_that("execute = TRUE runs a script with a predicted missing file (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  writeLines(c('if (file.exists("absent.csv")) d <- read.csv("absent.csv")', 'cat("done\\n")'),
             file.path(d, "guarded.R"))
  writeLines('d <- read.csv("really_absent.csv")', file.path(d, "stops.R"))
  # without a data file there is no file plan, and so no prediction to test; the
  # package layout, the default for a local folder, makes no such prediction either
  file.copy(repro_fixture("data.csv"), file.path(d, "data.csv"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE,
                                    layout = "psychds"))
  outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(outcome[["guarded.R"]], "ran_ok")
  expect_equal(outcome[["stops.R"]], "errored")
  # the prediction stays in the table, and code_check reports absent files
  expect_match(mo$table$unresolved_inputs[mo$table$file_name == "stops.R"], "really_absent.csv", fixed = TRUE)
  expect_no_match(paste(unlist(mo$report), collapse = "\n"), "#### Missing inputs", fixed = TRUE)
})

test_that("execute = TRUE: scripts that ask a person, or need what a person's computer has (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  file.copy(repro_fixture("data.csv"), file.path(d, "only.csv"))
  file.copy(repro_fixture("data.csv"), file.path(d, "second.csv"))
  writeLines(c('# the file is second.csv', 'a <- read.csv(file.choose())', 'stopifnot(nrow(a) > 0)'), file.path(d, "named.R"))
  writeLines('a <- read.csv(file.choose())', file.path(d, "unnamed.R"))
  writeLines(c('library(shiny)', 'runApp("nothing")'), file.path(d, "app.R"))
  writeLines(c('View(1)', 'windowsFonts(A = windowsFont("Arial"))', 'dir.create("x", showWarnings = FALSE)',
               'write.csv(1, "newfolder/deep/r.csv")', 'stopifnot(file.exists("newfolder/deep/r.csv"))'), file.path(d, "pc.R"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE))
  outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(outcome[["named.R"]], "ran_ok")
  expect_equal(outcome[["unnamed.R"]], "needs_a_person")
  expect_equal(outcome[["app.R"]], "needs_a_person")
  expect_equal(outcome[["pc.R"]], "ran_ok")
  expect_equal(mo$modifications$detail[mo$modifications$change_type == "file_choice_filled"], "second.csv")
  expect_false(identical(mo$traffic_light, "red"))
})

test_that("execute = TRUE: the package layout runs a script from where its paths work (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  for (f in c("proj/R", "proj/data", "study", "study/out")) dir.create(file.path(d, f), recursive = TRUE)
  file.copy(repro_fixture("data.csv"), file.path(d, "proj", "data", "d.csv"))
  file.copy(repro_fixture("data.csv"), file.path(d, "study", "near.csv"))
  writeLines("x", file.path(d, "study", "out", "keep.txt"))
  # the name is only known while the script runs, so no rewriting can follow it
  writeLines(c('f <- paste0("data", "/", "d.csv")', 'stopifnot(file.exists("data/d.csv"))', "a <- read.csv(f)"),
             file.path(d, "proj", "R", "from_top.R"))
  writeLines(c('n <- paste0("near", ".csv")', 'stopifnot(file.exists("near.csv"))', "a <- read.csv(n)",
               'write.csv(a, "out/result.csv")', 'stopifnot(file.exists("out/result.csv"))'),
             file.path(d, "study", "own.R"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                    execute = TRUE, layout = "package"))
  outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(outcome[["from_top.R"]], "ran_ok")
  expect_equal(outcome[["own.R"]], "ran_ok")
})

test_that("execute = TRUE: function files and tests are not run or counted as scripts (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "tests", "testthat"), recursive = TRUE)
  writeLines("fit <- function(x) x + 1", file.path(d, "used.R"))
  writeLines("other <- function(x) x", file.path(d, "unused.R"))
  writeLines("helper <- function() 1", file.path(d, "sourced.R"))
  writeLines(c('source("sourced.R")', "stopifnot(helper() == 1)"), file.path(d, "main.R"))
  writeLines(c("fit <- function(x) x + 1", "stopifnot(fit(1) == 2)"), file.path(d, "script.R"))
  writeLines('stop("a test that fails")', file.path(d, "tests", "testthat", "test-a.R"))
  file.copy(repro_fixture("data.csv"), file.path(d, "data.csv"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE))
  outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(outcome[["unused.R"]], "defines_functions")
  expect_equal(outcome[["sourced.R"]], "run_by_source")
  expect_equal(outcome[["main.R"]], "ran_ok")
  expect_equal(outcome[["script.R"]], "ran_ok")
  expect_equal(outcome[["test-a.R"]], "not_analysis")
  expect_true(is.na(mo$table$used_by[mo$table$file_name == "unused.R"]))
  expect_equal(mo$traffic_light, "green")
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "#### Files not run on their own", fixed = TRUE)
  expect_match(report, "Of 3 scripts: 2 ran without error", fixed = TRUE)
})

test_that("execute = TRUE: Python 2 code has its own outcome, and notebook commands are noted (#568)", {
  skip_if_quick()
  skip_if(!nzchar(.repro_find_python()), "needs Python")
  d <- withr::local_tempdir()
  writeLines('print "old"', file.path(d, "old.py"))
  writeLines('print("new")', file.path(d, "new.py"))
  writeLines('import oldmod', file.path(d, "uses.py"))
  writeLines('print "module"', file.path(d, "oldmod.py"))
  jsonlite::write_json(list(
    metadata = list(kernelspec = list(language = "python", name = "python3")),
    cells = list(list(cell_type = "code", source = "%%bash\nls -l"),
                 list(cell_type = "code", source = 'if "colab" in str(get_ipython()):\n    !pip install a\nprint("nb")'))),
    file.path(d, "n.ipynb"), auto_unbox = TRUE)
  file.copy(repro_fixture("data.csv"), file.path(d, "data.csv"))
  mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d, execute = TRUE))
  outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
  expect_equal(outcome[["old.py"]], "python2")
  expect_equal(outcome[["oldmod.py"]], "python2")
  expect_equal(outcome[["new.py"]], "ran_ok")
  expect_equal(outcome[["n.ipynb"]], "ran_ok")
  expect_equal(mo$table$not_runnable_reason[mo$table$file_name == "old.py"], "python2")
  expect_setequal(mo$modifications$detail[mo$modifications$change_type == "notebook_command_removed"],
                  c("%%bash", "!pip install a"))
  expect_true("get_ipython_added" %in% mo$modifications$change_type)
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "written for Python 2", fixed = TRUE)
  expect_match(report, "#### Notebook commands", fixed = TRUE)
})

test_that("execute = TRUE: a name in a variable and a path among same-named files are followed (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  for (layout in c("package", "psychds")) {
    d <- withr::local_tempdir()
    dir.create(file.path(d, "Data", "test"), recursive = TRUE)
    dir.create(file.path(d, "scripts"))
    writeLines("h <- 1", file.path(d, "scripts", "NetworkFunction.R"))
    writeLines(c('f <- c("NetworkFunction.R")', 'for (i in f) source(i)', 'stopifnot(h == 1)'),
               file.path(d, "scripts", "viavar.R"))
    file.copy(repro_fixture("data.csv"), file.path(d, "Data", "d.csv"))
    writeLines(c("x,y", "1,2", "1,2"), file.path(d, "Data", "test", "d.csv"))
    writeLines(c('a <- read.csv("Data/test/d.csv")', 'stopifnot(ncol(a) == 2, nrow(a) == 2)'), file.path(d, "deep.R"))
    mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                      execute = TRUE, layout = layout))
    outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
    expect_equal(outcome[["deep.R"]], "ran_ok")
    expect_equal(outcome[["viavar.R"]], "ran_ok")
  }
})

test_that("execute = TRUE: a model file has its own outcome, and the script that reads it finds it (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  for (layout in c("package", "psychds")) {
    d <- withr::local_tempdir()
    dir.create(file.path(d, "jags_script"))
    writeLines(c("model {", "  x ~ dnorm(0, 1)", "}"), file.path(d, "jags_script", "m.R"))
    writeLines(c('m <- readLines("./jags_script/m.R")', 'stopifnot(any(grepl("dnorm", m)))'), file.path(d, "run.R"))
    file.copy(repro_fixture("data.csv"), file.path(d, "data.csv"))
    mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                      execute = TRUE, layout = layout))
    outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
    expect_equal(outcome[["m.R"]], "jags_model")
    expect_equal(outcome[["run.R"]], "ran_ok")
    expect_equal(mo$table$outcome[mo$table$file_name == "m.R"], "jags_model")
    expect_equal(mo$traffic_light, "green")
    expect_match(paste(unlist(mo$report), collapse = "\n"), "Of 1 script: 1 ran without error", fixed = TRUE)
  }
})

test_that("execute = TRUE: a file spelled otherwise is read, and the correction is noted (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  for (layout in c("package", "psychds")) {
    d <- withr::local_tempdir()
    writeLines(c('d <- read.csv("my data.csv")', 'stopifnot(nrow(d) > 0)'), file.path(d, "a.R"))
    file.copy(repro_fixture("data.csv"), file.path(d, "my_data.csv"))
    mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                      execute = TRUE, layout = layout))
    expect_equal(mo$run_results$outcome, "ran_ok")
    expect_equal(mo$table$names_corrected, "my data.csv -> my_data.csv")
    fixed <- mo$modifications[mo$modifications$change_type == "name_corrected", ]
    expect_equal(fixed$detail, '"my data.csv" -> "my_data.csv"')
    report <- paste(unlist(mo$report), collapse = "\n")
    expect_match(report, "should be changed", fixed = TRUE)
    expect_match(report, "my_data.csv", fixed = TRUE)
  }
})

test_that("execute = TRUE: same-named scripts each run and get their own outcome (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  for (layout in c("package", "psychds")) {
    d <- withr::local_tempdir()
    dir.create(file.path(d, "good"))
    dir.create(file.path(d, "bad"))
    writeLines(c('d <- read.csv("data.csv")', 'cat("good\\n")'), file.path(d, "good", "analysis.R"))
    writeLines('d <- read.csv("really_absent.csv")', file.path(d, "bad", "analysis.R"))
    file.copy(repro_fixture("data.csv"), file.path(d, "good", "data.csv"))
    mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                      execute = TRUE, layout = layout))
    outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
    expect_equal(outcome[["good/analysis.R"]], "ran_ok")
    expect_equal(outcome[["bad/analysis.R"]], "errored")
    expect_setequal(mo$table$file_name, c("good/analysis.R", "bad/analysis.R"))
    expect_setequal(mo$table$file_name, mo$run_results$file_name)
    expect_equal(mo$table$outcome[mo$table$file_name == "bad/analysis.R"], "errored")
    expect_equal(mo$table$file_path, mo$table$file_name)
  }
})

test_that("execute = TRUE: source() runs the same-named script of the script's own folder (#568)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  for (layout in c("package", "psychds")) {
    d <- withr::local_tempdir()
    for (f in c("a", "b")) {
      dir.create(file.path(d, f))
      writeLines(sprintf('from <- "%s"', f), file.path(d, f, "helpers.R"))
      writeLines(c('source("helpers.R")', sprintf('stopifnot(from == "%s")', f)),
                 file.path(d, f, "main.R"))
    }
    file.copy(repro_fixture("data.csv"), file.path(d, "data.csv"))
    mo <- suppressMessages(module_run(test_paper(), "reproducibility_check", local_path = d,
                                      execute = TRUE, layout = layout))
    outcome <- stats::setNames(mo$run_results$outcome, mo$run_results$file_name)
    expect_equal(outcome[["a/main.R"]], "ran_ok")
    expect_equal(outcome[["b/main.R"]], "ran_ok")
    expect_equal(outcome[["a/helpers.R"]], "run_by_source")
    expect_equal(outcome[["b/helpers.R"]], "run_by_source")
  }
})

test_that("static analysis: same-named scripts that differ each keep a row, and a name that occurs once stays as it is (#568)", {
  paper <- test_paper()
  d <- withr::local_tempdir()
  writeLines("x <- 1", file.path(d, "one.R"))
  writeLines("x <- 2", file.path(d, "two.R"))
  writeLines("x <- 3", file.path(d, "other.R"))
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "a.R", file.path(d, "one.R")),
    code_tbl_row(paper, "a.R", file.path(d, "two.R")),
    code_tbl_row(paper, "b.R", file.path(d, "other.R")))
  code_tbl$file_path <- c("x/a.R", "y/a.R", "z/b.R")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")
  expect_equal(mo$table$file_name, c("x/a.R", "y/a.R", "b.R"))
  expect_equal(mo$table$file_path, c("x/a.R", "y/a.R", "z/b.R"))
})

test_that("static analysis: a paper with BOTH R and Python code combines into one summary row", {
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "ok.R", repro_fixture("ok.R"), language = "R"),
    code_tbl_row(paper, "ok.py", repro_fixture("ok.py"), language = "Python"))
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = c("ok.R", "ok.py", "data.csv"),
    file_location = c(repro_fixture("ok.R"), repro_fixture("ok.py"), repro_fixture("data.csv")))

  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  # one row per paper, not one row per language
  expect_equal(nrow(mo$summary_table), 1)
  expect_equal(mo$summary_table$repro_code_n, 2)
  expect_setequal(mo$table$file_name, c("ok.R", "ok.py"))
  expect_match(paste(mo$report, collapse = "\n"), "ok\\.R")
  expect_match(paste(mo$report, collapse = "\n"), "### Python code", fixed = TRUE)
})

test_that("static analysis: Python-only byte-identical duplicate mirrors are only run once", {
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "ok.py", repro_fixture("ok.py"), language = "Python"),
    code_tbl_row(paper, "ok.py", repro_fixture("ok.py"), language = "Python"))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  expect_equal(nrow(mo$table), 1)
  expect_match(paste(mo$report, collapse = "\n"), "Duplicate files across repo mirrors")
})

test_that("static analysis: a Python file with no R code at all is not na (has its own section)", {
  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "ok.py", repro_fixture("ok.py"), language = "Python")
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = c("ok.py", "data.csv"),
    file_location = c(repro_fixture("ok.py"), repro_fixture("data.csv")))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")

  expect_false(identical(mo$traffic_light, "na"))
  expect_equal(nrow(mo$table), 1)
})


# execute = TRUE (Python; spawns real python/python3 subprocesses) ----

test_that("execute = TRUE actually runs a Python script and records ran_ok", {
  skip_if_quick()
  skip_if_not(!is.na(.repro_py_bin), "no python interpreter available")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "ok.py", repro_fixture("ok.py"), language = "Python")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_equal(mo$table$outcome, "ran_ok")
  expect_equal(mo$run_results$outcome, "ran_ok")
  expect_match(mo$run_results$stdout, "mean_a")
  expect_match(mo$summary_text, "assessed AND ran", fixed = TRUE)
})

test_that("execute = TRUE: a Python script that errors forces the traffic light red", {
  skip_if_quick()
  skip_if_not(!is.na(.repro_py_bin), "no python interpreter available")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "errors.py", repro_fixture("errors.py"), language = "Python")
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

test_that("execute = TRUE: os.chdir() is stripped and reported, and the Python script still runs", {
  skip_if_quick()
  skip_if_not(!is.na(.repro_py_bin), "no python interpreter available")

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "bad_chdir.py", repro_fixture("bad_chdir.py"), language = "Python")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_equal(mo$table$outcome, "ran_ok")
  expect_match(paste(mo$report, collapse = "\n"), "os.chdir", fixed = TRUE)
  # os.chdir() removal is reported via the SAME change_type label as R's
  # setwd() removal (see the module's own comment on this reuse), not a new
  # "chdir_removed" label.
  expect_true("setwd_removed" %in% mo$modifications$change_type)
})

test_that("execute = TRUE: a Python NameError is reported with the script that defines the name, without a re-run", {
  # Unlike R's repro_run_scripts(), which runs every script inside ONE shared
  # callr session, repro_run_scripts_py() runs every script as its own
  # `python` process, so a name another script defines is never available,
  # whatever the order. The module therefore does not re-run Python scripts
  # in a corrected order (that could never succeed); the report names the
  # script that defines the missing name instead.
  skip_if_quick()
  skip_if_not(!is.na(.repro_py_bin), "no python interpreter available")

  tmp <- withr::local_tempdir()
  definer <- file.path(tmp, "definer.py")
  writeLines(c("some_var_no_script_defines = 42"), definer)
  user <- file.path(tmp, "undefined_var.py")
  writeLines(readLines(repro_fixture("undefined_var.py")), user)

  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "definer.py", definer, language = "Python"),
    code_tbl_row(paper, "undefined_var.py", user, language = "Python"))
  structure_df <- data.frame(paper_id = character(0), file_name = character(0),
                             file_location = character(0))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_equal(mo$table$outcome[mo$table$file_name == "definer.py"], "ran_ok")
  expect_equal(mo$table$outcome[mo$table$file_name == "undefined_var.py"], "errored")
  expect_equal(mo$table$error_type[mo$table$file_name == "undefined_var.py"],
              "undefined_variable")
  report <- paste(mo$report, collapse = "\n")
  expect_match(report, "Undefined-name errors", fixed = TRUE)
  expect_false(grepl("re-ran once", report, fixed = TRUE))
})

test_that("execute = TRUE: a missing Python dependency is reported as dependency_unavailable, not errored", {
  skip_if_quick()
  skip_if_not(!is.na(.repro_py_bin), "no python interpreter available")

  tmp <- withr::local_tempdir()
  script <- file.path(tmp, "needs_pkg.py")
  writeLines("import this_package_does_not_exist_xyz123", script)

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "needs_pkg.py", script, language = "Python",
                           packages = "this_package_does_not_exist_xyz123")
  structure_df <- data.frame(paper_id = character(0), file_name = character(0),
                             file_location = character(0))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE,
                   install_missing = TRUE, timeout = 60)

  expect_equal(mo$table$outcome, "dependency_unavailable")
  # dependency_unavailable must NOT force the traffic light red (same
  # treatment as the R path's own dependency_unavailable case).
  expect_false(identical(mo$traffic_light, "red"))
})

test_that("execute = TRUE: Python console output feeds stat_output via a real scipy ttest", {
  skip_if_quick()
  skip_if_not(!is.na(.repro_py_bin), "no python interpreter available")
  scipy_ok <- tryCatch({
    res <- processx::run(.repro_py_bin, c("-c", "import scipy.stats"),
                         error_on_status = FALSE)
    identical(res$status, 0L)
  }, error = function(e) FALSE)
  skip_if_not(scipy_ok, "scipy not importable in this python interpreter")

  tmp <- withr::local_tempdir()
  script <- file.path(tmp, "ttest.py")
  writeLines(c(
    "import scipy.stats as stats",
    "a = [1, 2, 3, 4, 5]",
    "b = [4, 5, 6, 7, 8]",
    "print(stats.ttest_ind(a, b))"
  ), script)

  paper <- test_paper()
  code_tbl <- code_tbl_row(paper, "ttest.py", script, language = "Python")
  structure_df <- data.frame(paper_id = character(0), file_name = character(0),
                             file_location = character(0))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE, timeout = 60)

  expect_equal(mo$table$outcome, "ran_ok")
  expect_gt(length(mo$stat_output), 0)
  long <- dplyr::bind_rows(lapply(mo$stat_output, `[[`, "long"))
  expect_gt(nrow(long), 0)
})

test_that("execute = TRUE: scripts are not started once total_timeout has run out (Lakens/tue_datamanager_check#4, item 6)", {
  skip_if_quick()
  skip_if_not_installed("callr")

  paper <- test_paper()
  d <- withr::local_tempdir()
  writeLines("Sys.sleep(60)", file.path(d, "1_slow.R"))
  writeLines("y <- 2", file.path(d, "2_next.R"))
  code_tbl <- rbind(code_tbl_row(paper, "1_slow.R", file.path(d, "1_slow.R")),
                    code_tbl_row(paper, "2_next.R", file.path(d, "2_next.R")))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE,
                   timeout = 60, total_timeout = 5)
  rr <- mo$run_results
  expect_equal(rr$outcome[match(c("1_slow.R", "2_next.R"), rr$file_name)],
               c("timed_out", "skipped_time_limit"))
  expect_match(paste(mo$report, collapse = "\n"),
               "1 script was not started, because the time for all scripts together", fixed = TRUE)
})

test_that("total_timeout can be a clock time: the scripts must be done by then (#523)", {
  # the deadline itself, not counted from the first script
  at <- Sys.time() + 120
  expect_identical(metacheck:::.repro_deadline(at), at)
  expect_null(metacheck:::.repro_deadline(as.POSIXct(NA)))
  expect_null(metacheck:::.repro_deadline(Inf))
  expect_lt(abs(as.numeric(difftime(metacheck:::.repro_deadline(60), Sys.time() + 60, units = "secs"))), 2)
  expect_equal(metacheck:::.repro_deadline_text(at), "the time for all scripts together")
  expect_match(metacheck:::.repro_deadline_text(3600), "60 minutes", fixed = TRUE)

  skip_if_quick()
  skip_if_not_installed("callr")
  paper <- test_paper()
  d <- withr::local_tempdir()
  writeLines("Sys.sleep(60)", file.path(d, "1_slow.R"))
  writeLines("y <- 2", file.path(d, "2_next.R"))
  code_tbl <- rbind(code_tbl_row(paper, "1_slow.R", file.path(d, "1_slow.R")),
                    code_tbl_row(paper, "2_next.R", file.path(d, "2_next.R")))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE,
                   timeout = 60, total_timeout = Sys.time() + 5)
  rr <- mo$run_results
  expect_equal(rr$outcome[match(c("1_slow.R", "2_next.R"), rr$file_name)],
               c("timed_out", "skipped_time_limit"))
  expect_match(paste(mo$report, collapse = "\n"),
               "1 script was not started, because the time for all scripts together had run out.", fixed = TRUE)
})

test_that("execute = TRUE: a script the second run has no time for keeps its first result (Lakens/tue_datamanager_check#4, item 6)", {
  # on GitHub, covr slows R down so much that the first script does not
  # finish within the 8 s this test allows
  skip_on_ci()
  skip_if_quick()
  skip_if_not_installed("callr")

  paper <- test_paper()
  d <- withr::local_tempdir()
  # 1_use.R needs x from 2_def.R, so the first run fails on it and a second
  # run in the corrected order follows; 2_def.R uses up the time
  writeLines("print(x)", file.path(d, "1_use.R"))
  writeLines(c("x <- 1", "Sys.sleep(60)"), file.path(d, "2_def.R"))
  code_tbl <- rbind(code_tbl_row(paper, "1_use.R", file.path(d, "1_use.R")),
                    code_tbl_row(paper, "2_def.R", file.path(d, "2_def.R")))
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  plan_df <- data.frame(file_name = "data.csv", target_path = "data.csv")
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df),
                                 psychds_check = list(table = plan_df))

  mo <- module_run(fake_code, "reproducibility_check", execute = TRUE,
                   timeout = 60, total_timeout = 8)
  rr <- mo$run_results
  expect_true(attr(rr, "reran_for_order"))
  expect_equal(rr$outcome[match(c("1_use.R", "2_def.R"), rr$file_name)],
               c("errored", "timed_out"))
  expect_false(any(rr$outcome == "skipped_time_limit"))
})

test_that("the duplicate note gives each copy's path in the package, not its place on this computer (Lakens/tue_datamanager_check#5, item 2)", {
  paper <- test_paper()
  code_tbl <- dplyr::bind_rows(
    code_tbl_row(paper, "ok.R", repro_fixture("ok.R")),
    code_tbl_row(paper, "ok.R", repro_fixture("ok.R")))
  code_tbl$file_path <- c("study1/ok.R", "study2/ok.R")
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "data.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")
  txt <- paste(mo$report, collapse = "\n")
  expect_match(txt, "`ok.R` at `study2/ok.R` (same as `ok.R` at `study1/ok.R`)", fixed = TRUE)
  expect_false(grepl(dirname(repro_fixture("ok.R")), txt, fixed = TRUE))
})

test_that("a script reading a file that is only inside a .zip is not run, and the report says why (#520, item 2)", {
  paper <- test_paper()
  d <- withr::local_tempdir()
  writeLines("d <- read.csv('data/scores.csv')", file.path(d, "main.R"))
  code_tbl <- code_tbl_row(paper, "main.R", file.path(d, "main.R"))
  code_tbl$file_path <- "main.R"
  structure_df <- data.frame(paper_id = paper_id(paper), file_name = "scores.csv",
                             file_path = "data.zip/data/scores.csv",
                             file_location = repro_fixture("data.csv"))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check", layout = "package")
  txt <- paste(mo$report, collapse = "\n")
  expect_match(txt, "in_archive", fixed = TRUE)
  expect_match(txt, "only inside data.zip; unpack it so the code finds it", fixed = TRUE)
  expect_equal(mo$summary_table$repro_missing_inputs, 1)
})

test_that("renv's own files in a renv/ folder are not assessed as the paper's scripts (#548)", {
  # The exclusion matched "renv/activate.R" against file_name, which holds
  # no folder, so activate.R was assessed and its sprintf() path made it a
  # missing input and the light yellow.
  paper <- test_paper()
  d <- withr::local_tempdir()
  dir.create(file.path(d, "renv"))
  writeLines('x <- readRDS(sprintf("renv_%s.tar.gz", "1.0.0"))',
             file.path(d, "renv", "activate.R"))
  code_tbl <- rbind(code_tbl_row(paper, "ok.R", repro_fixture("ok.R")),
                    code_tbl_row(paper, "activate.R", file.path(d, "renv", "activate.R")))
  code_tbl$file_path <- c("ok.R", "renv/activate.R")
  structure_df <- data.frame(
    paper_id = paper_id(paper), file_name = c("ok.R", "data.csv"),
    file_location = c(repro_fixture("ok.R"), repro_fixture("data.csv")))
  fake_code <- fake_module_output("code_check", code_tbl, paper)
  fake_code$prev_outputs <- list(data_check = list(structure = structure_df))
  mo <- module_run(fake_code, "reproducibility_check")
  expect_equal(mo$table$file_name, "ok.R")
  expect_equal(mo$traffic_light, "green")
})

test_that("execute = TRUE: stored sapply() summaries and a stored effectsize result are found (#549)", {
  skip_if_quick()
  skip_if_not_installed("callr")
  skip_if_not_installed("effectsize")
  # The means and SDs were only found when sapply() stood on its own line,
  # and dz only when the effectsize result was printed; stored in a variable,
  # as analysis code usually does, they were "not found".
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  dir.create(file.path(root, "code"))
  write.csv(data.frame(congruent = c(10, 12, 14, 16, 18, 11, 13, 16),
                       incongruent = c(17, 20, 19, 25, 26, 18, 22, 21)),
            file.path(root, "data", "stroop_data.csv"), row.names = FALSE)
  writeLines(c(
    'stroop_data <- read.csv("../data/stroop_data.csv")',
    'means <- sapply(stroop_data[, c("congruent", "incongruent")], mean)',
    'sds <- sapply(stroop_data[, c("congruent", "incongruent")], sd)',
    'ttest_result <- t.test(stroop_data$incongruent, stroop_data$congruent, paired = TRUE)',
    'dz_result <- effectsize::repeated_measures_d(stroop_data$incongruent,',
    '  stroop_data$congruent, method = "z", adjust = FALSE)'),
    file.path(root, "code", "analysis.R"))
  paper <- test_paper(c(
    "The congruent condition (M = 13.75, SD = 2.76) was faster than the incongruent condition (M = 21, SD = 3.21).",
    "A paired t-test was significant, t(7) = 12.97, p < .001, Cohen's dz = 4.59."))

  mo <- module_run(paper, "reproducibility_check", local_path = root,
                   local_only = TRUE, execute = TRUE, timeout = 60)
  mt <- mo$match_table
  expect_true(all(mt$found))
  expect_true(any(grepl("M=13.75", mt$reported, fixed = TRUE)))
  expect_true(any(grepl("SD=2.76", mt$reported, fixed = TRUE)))
  expect_true(any(grepl("dz=4.59", mt$reported, fixed = TRUE)))
  expect_equal(mo$traffic_light, "green")
})
