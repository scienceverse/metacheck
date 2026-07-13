# Tests for the Psych-DS pipeline: data_check study grouping, psychds_check,
# convert_psychds, and the native psychds_validate. All run offline and
# deterministically by pointing test_paper() at a local fixture repo built in
# tempdir() — no network, no LLM (grouping stays NA under llm_use(FALSE)).

# Build a small, self-contained repository fixture on disk. Returns its path.
make_fixture_repo <- function() {
  d <- file.path(tempdir(), paste0("psychds_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "Code"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(id = 1:5, score = c(2.1, 3.4, 1.9, 4.0, 2.8),
               cond = c("a", "b", "a", "b", "a")),
    file.path(d, "data", "study1.csv"), row.names = FALSE)
  writeLines(c("var,description", "id,participant id", "score,outcome measure",
               "cond,experimental condition"),
             file.path(d, "codebook.csv"))
  writeLines("A readme.", file.path(d, "README.txt"))
  writeLines("x <- 1", file.path(d, "Code", "analysis.R"))
  d
}

# ── psychds_validate: hand-built directories ──────────────────────────────────

test_that("psychds_validate accepts a minimal valid dataset", {
  d <- file.path(tempdir(), "pd_valid"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(id = 1:3, x = c(1.1, 2.2, 3.3)),
                   file.path(d, "data", "source-s1_data.csv"), row.names = FALSE)
  desc <- list(
    `@context` = "https://schema.org/", `@type` = "Dataset",
    name = "Test", description = "A test dataset.",
    variableMeasured = list(
      list(`@type` = "PropertyValue", name = "id"),
      list(`@type` = "PropertyValue", name = "x")
    ), schemaVersion = "Psych-DS 1.5.1")
  writeLines(jsonlite::toJSON(desc, auto_unbox = TRUE, pretty = TRUE),
             file.path(d, "dataset_description.json"))
  writeLines("readme", file.path(d, "README.md"))
  writeLines("changes", file.path(d, "CHANGES.md"))

  res <- psychds_validate(d)
  expect_s3_class(res, "psychds_validation")
  expect_true(res$valid)
  expect_equal(res$summary$n_errors, 0)
})

test_that("psychds_validate flags a missing dataset_description.json", {
  d <- file.path(tempdir(), "pd_nodesc"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(id = 1:3),
                   file.path(d, "data", "source-s1_data.csv"), row.names = FALSE)
  res <- psychds_validate(d)
  expect_false(res$valid)
  codes <- vapply(res$issues, function(i) i$code, character(1))
  expect_true("MissingRequiredElement" %in% codes)
})

test_that("psychds_validate flags a CSV column missing from variableMeasured", {
  d <- file.path(tempdir(), "pd_colmiss"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(id = 1:3, extra = 4:6),
                   file.path(d, "data", "source-s1_data.csv"), row.names = FALSE)
  desc <- list(`@context` = "https://schema.org/", `@type` = "Dataset",
               name = "T", description = "d",
               variableMeasured = list(list(`@type` = "PropertyValue", name = "id")),
               schemaVersion = "Psych-DS 1.5.1")
  writeLines(jsonlite::toJSON(desc, auto_unbox = TRUE),
             file.path(d, "dataset_description.json"))
  res <- psychds_validate(d)
  expect_false(res$valid)
  codes <- vapply(res$issues, function(i) i$code, character(1))
  expect_true("CsvColumnMissingFromMetadata" %in% codes)
})

test_that("psychds_validate flags a non-Dataset @type", {
  d <- file.path(tempdir(), "pd_badtype"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(id = 1:3),
                   file.path(d, "data", "source-s1_data.csv"), row.names = FALSE)
  desc <- list(`@context` = "https://schema.org/", `@type` = "Thing",
               name = "T", description = "d",
               variableMeasured = list(list(name = "id")))
  writeLines(jsonlite::toJSON(desc, auto_unbox = TRUE),
             file.path(d, "dataset_description.json"))
  res <- psychds_validate(d)
  codes <- vapply(res$issues, function(i) i$code, character(1))
  expect_true("IncorrectDatasetType" %in% codes)
})

# ── data_check study grouping ─────────────────────────────────────────────────

test_that("data_check leaves study group NA without an LLM", {
  skip_if_not(getOption("metacheck.llm.use", FALSE) == FALSE || TRUE)
  llm_use(FALSE)
  d <- make_fixture_repo()
  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_true("group" %in% names(mo$structure))
  expect_true(all(is.na(mo$structure$group)))
})

# ── psychds_check ─────────────────────────────────────────────────────────────

test_that("psychds_check reports the compliance gap for a raw repo", {
  llm_use(FALSE)
  d <- make_fixture_repo()
  # Run data_check first so psychds_check consumes it.
  report_module_run(test_paper("x"),
                    c("data_check", "codebook_check", "psychds_check"),
                    args = list(data_check = list(local_path = d,
                                                  local_only = TRUE)))
  op <- module_run(test_paper("x"), "psychds_check",
                   local_path = d, local_only = TRUE)
  expect_true(op$traffic_light %in% c("yellow", "red", "green"))
  # dataset_description.json is absent in the raw repo → a required item missing
  expect_gt(op$summary_table$required_missing, 0)
  expect_true(any(grepl("Target Psych-DS", op$report)))
  # No LLM → the "subgrouping unknown" note is present
  expect_true(any(grepl("subgrouping could not be detected", op$report)))
})

# ── convert_psychds round-trips to a valid dataset ────────────────────────────

test_that("convert_psychds produces a dataset that psychds_validate accepts", {
  llm_use(FALSE)
  d <- make_fixture_repo()
  out <- file.path(tempdir(), "pd_convert_out")
  res <- convert_psychds(test_paper("x"), output_dir = out,
                         local_path = d, local_only = TRUE, overwrite = TRUE)
  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(out, "dataset_description.json")))
  expect_gt(res$n_files_copied, 0)

  # The generated data CSV must be BOM-free (the reason for .psychds_copy_no_bom)
  csvs <- list.files(file.path(out, "data"), pattern = "\\.csv$", full.names = TRUE)
  expect_gt(length(csvs), 0)
  con <- file(csvs[[1]], "rb"); on.exit(close(con))
  b <- readBin(con, "raw", 3)
  expect_false(identical(as.integer(b), c(239L, 187L, 191L)))

  val <- psychds_validate(out)
  expect_true(val$valid)
  expect_equal(val$summary$n_errors, 0)

  # Re-running without overwrite messages and skips, rather than erroring.
  expect_message(
    again <- convert_psychds(test_paper("x"), output_dir = out,
                             local_path = d, local_only = TRUE),
    "already exists")
  expect_true(isTRUE(again$existed))
  expect_equal(again$n_files_copied, 0L)
})

test_that("convert_psychds reuses a captured report result without re-running", {
  llm_use(FALSE)
  d <- make_fixture_repo()
  # Run the chain once, then convert by handing back its outputs.
  results <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check", "psychds_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))

  # Reuse by passing the captured result directly as `paper`.
  out <- file.path(tempdir(), "pd_reuse_out"); unlink(out, recursive = TRUE)
  res <- convert_psychds(results, output_dir = out)
  expect_gt(res$n_files_copied, 0)
  expect_true(psychds_validate(out)$valid)

  # A partial captured result still carries the paper (as an attribute), so
  # reusing it as `paper` recovers the paper and re-runs the full chain. Passing
  # the fixture path lets that re-run find the files and convert.
  partial <- report_module_run(
    test_paper("x"), "data_check",
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  out4 <- file.path(tempdir(), "pd_partial_paper"); unlink(out4, recursive = TRUE)
  res4 <- convert_psychds(partial, output_dir = out4,
                          local_path = d, local_only = TRUE)
  expect_gt(res4$n_files_copied, 0)
  expect_true(psychds_validate(out4)$valid)
})

test_that("gated-repo hint explains a skipped repo with recovery steps", {
  # A repo was found but not listable (e.g. a GitHub repo over the size gate):
  # the converter's empty-plan / no-columns error must name the repo, the
  # reason, and how to include it, instead of reporting "no repository".
  ops <- list(data_check = list(gated_repos = data.frame(
    repo_url = c("https://github.com/a/b/tree/master/x",
                 "https://github.com/a/b/tree/master/y"),
    repo_type = "github",
    repo_error = "GitHub repo size ~638 MB exceeds the 500 MB gate",
    stringsAsFactors = FALSE)))
  hint <- metacheck:::.converter_gated_hint(ops)
  expect_match(hint, "found but not downloaded")
  expect_match(hint, "638 MB exceeds")
  expect_match(hint, "local_path", fixed = TRUE)
  expect_match(hint, "github_gate", fixed = TRUE)
  # The two deep URLs for the same repo collapse to one root line.
  expect_equal(
    lengths(regmatches(hint, gregexpr("github.com/a/b", hint, fixed = TRUE))), 1)
  expect_false(grepl("tree/master", hint))

  # No gated repo → no hint (a genuine "no repository" case).
  expect_equal(metacheck:::.converter_gated_hint(list()), "")
  expect_equal(
    metacheck:::.converter_gated_hint(
      list(data_check = list(gated_repos = data.frame()))), "")
})

test_that("non-CSV data files are converted to CSV and the original is kept", {
  skip_if_not_installed("writexl")
  withr::local_options(metacheck.llm.use = FALSE)

  # A repo with one .xlsx data file (plus the standard fixture files).
  d <- make_fixture_repo()
  df <- data.frame(id = 1:5, score = c(2.1, 3.4, 1.9, 4.0, 2.8),
                   cond = c("a", "b", "a", "b", "a"))
  writexl::write_xlsx(df, file.path(d, "data", "survey.xlsx"))

  chain <- report_module_run(
    test_paper("x"),
    c("repo_check", "data_check", "codebook_check", "psychds_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))

  # psychds_check plans a convert + an original_target for the xlsx.
  plan <- chain$psychds_check$table
  xrow <- plan[grepl("survey", plan$file_name), ]
  expect_true(isTRUE(as.logical(xrow$convert[1])))
  expect_match(xrow$target_path[1], "_data\\.csv$")
  expect_match(xrow$original_target[1], "\\.xlsx$")

  out <- withr::local_tempdir()
  suppressMessages(convert_psychds(chain, output_dir = out, overwrite = TRUE))

  data_dir <- if (dir.exists(file.path(out, "data"))) file.path(out, "data")
              else list.files(out, pattern = "^study-", full.names = TRUE)[1] |>
                file.path("data")
  files <- list.files(data_dir)
  # Both a real converted CSV and the untouched original are present.
  csv <- grep("survey.*_data\\.csv$", files, value = TRUE)
  xls <- grep("survey.*\\.xlsx$", files, value = TRUE)
  expect_length(csv, 1)
  expect_length(xls, 1)

  # The converted CSV is genuine, readable, and preserves the data.
  back <- utils::read.csv(file.path(data_dir, csv))
  expect_equal(nrow(back), 5)
  expect_true(all(c("id", "score", "cond") %in% names(back)))

  # The bundle still validates as Psych-DS.
  expect_true(psychds_validate(out)$valid)
})

test_that("convert_psychds skips gracefully when psychds_check plan is empty", {
  llm_use(FALSE)

  # No local repository and download = "none" produce an empty data/placement
  # plan; conversion should return a no-op result rather than erroring.
  chain <- report_module_run(
    test_paper("x"),
    c("data_check", "codebook_check", "psychds_check"),
    args = list(data_check = list(download = "none", local_only = TRUE)))

  out <- file.path(tempdir(), "pd_empty_plan")
  expect_message(
    res <- convert_psychds(chain, output_dir = out, overwrite = TRUE),
    "empty plan")

  expect_true(isTRUE(res$empty_plan))
  expect_equal(res$n_files_copied, 0L)
  expect_equal(res$n_studies, 0L)
  expect_length(res$descriptions, 0)
})
