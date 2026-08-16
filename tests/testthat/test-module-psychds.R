# Tests for the Psych-DS pipeline: data_check study grouping, psychds_check,
# convert_psychds, and the native psychds_validate. All run offline and
# deterministically by pointing test_paper() at a local fixture repo built in
# tempdir() — no network, no LLM. Grouping is fully deterministic under
# llm_use(FALSE): every file except the root readme/ro-crate-metadata.json
# still resolves to a real study group (there is no "shared" placeholder).

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

test_that("data_check groups studies deterministically without an LLM", {
  llm_use(FALSE)
  d <- make_fixture_repo()
  mo <- module_run(test_paper("x"), "data_check",
                   local_path = d, local_only = TRUE)
  expect_true("group" %in% names(mo$structure))
  # Study grouping is NOT LLM-only. `.data_group_from_path()` reads the study
  # from the file path ("study1.csv" -> ex1) and runs before any LLM call,
  # deliberately: a filename states the study more reliably than a small model
  # infers it. So the group is populated even with llm_use(FALSE); only files
  # the deterministic passes cannot place need the LLM.
  grp <- mo$structure$group
  # the data file names its study in the path
  expect_equal(grp[mo$structure$file_name == "study1.csv"], "ex1")
  # The root readme is collection-level: it is EXCLUDED from grouping
  # entirely (never sent to data_group_llm()), not assigned a "shared"
  # placeholder — its group stays NA.
  expect_true(is.na(grp[mo$structure$file_name == "README.txt"]))
  # Every OTHER file resolves to a real study — there is no "shared" bucket.
  # A single-study repo's codebook and code file both fall back to the sole
  # study that exists (ex1), even though neither path names it directly.
  expect_false(any(is.na(grp[mo$structure$file_name != "README.txt"])))
  expect_equal(grp[mo$structure$file_name == "codebook.csv"], "ex1")
  expect_equal(grp[mo$structure$file_name == "analysis.R"], "ex1")
})

test_that("data_check assigns a genuinely-shipped ro-crate-metadata.json to the root, not a study", {
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("psychds_rocrate_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(id = 1:3, x = c(1, 2, 3)),
                   file.path(d, "data", "study1.csv"), row.names = FALSE)
  writeLines("{}", file.path(d, "ro-crate-metadata.json"))
  mo <- module_run(test_paper("x"), "data_check", local_path = d, local_only = TRUE)

  role <- mo$structure$doc_role[mo$structure$file_name == "ro-crate-metadata.json"]
  expect_equal(role, "readme")
  grp <- mo$structure$group[mo$structure$file_name == "ro-crate-metadata.json"]
  expect_true(is.na(grp))
})

test_that("data_check records cross-study reuse via a script's own references, not duplication", {
  llm_use(FALSE)
  d <- file.path(tempdir(), paste0("psychds_reuse_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "study1"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "study2"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(id = 1:3, x = c(1, 2, 3)),
                   file.path(d, "study1", "data.csv"), row.names = FALSE)
  utils::write.csv(data.frame(id = 1:3, x = c(4, 5, 6)),
                   file.path(d, "study2", "data.csv"), row.names = FALSE)
  # A single stimulus file physically lives under study1/, but study2's own
  # script also reads it directly — hard evidence of reuse. .data_code_refs()
  # only recognises a fixed set of read/write function calls (read_csv,
  # read.table, ...; see .CODE_READ_FNS), not arbitrary functions like
  # readLines(), so the reference must use one of those recognised names.
  writeLines("id\n1", file.path(d, "study1", "stimulus.txt"))
  writeLines('df <- read.csv("data.csv")', file.path(d, "study1", "analysis.R"))
  writeLines('stim <- read.csv("stimulus.txt"); df <- read.csv("data.csv")',
             file.path(d, "study2", "analysis.R"))
  mo <- module_run(test_paper("x"), "data_check", local_path = d, local_only = TRUE)

  st <- mo$structure
  stim_row <- st$file_name == "stimulus.txt"
  # Owned by exactly one study (whichever the deterministic passes placed it
  # in — here, its own path names study1).
  expect_equal(st$group[stim_row], "ex1")
  # study2's script referencing it by name is recorded as reuse, not as a
  # second ownership claim.
  expect_true("ex2" %in% (st$referenced_by[stim_row][[1]] %||% character(0)))
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
  # The fixture's "study1.csv" is placed in ex1 by the deterministic path pass,
  # so study groups ARE detected even with llm_use(FALSE), and the
  # "subgrouping unknown" note must NOT appear. The note is for the case where
  # no pass could place anything (see .data_group_from_path).
  expect_false(any(grepl("subgrouping could not be detected", op$report)))
})

test_that("psychds_check notes unknown subgrouping when nothing can be placed", {
  llm_use(FALSE)
  # A repo whose paths name no study: the deterministic passes place nothing,
  # so the report should say so rather than imply a single-study layout.
  d <- withr::local_tempdir()
  writeLines("id,x\n1,2", file.path(d, "measures.csv"))
  writeLines("notes", file.path(d, "README.txt"))
  report_module_run(test_paper("x"),
                    c("data_check", "codebook_check", "psychds_check"),
                    args = list(data_check = list(local_path = d,
                                                  local_only = TRUE)))
  op <- module_run(test_paper("x"), "psychds_check",
                   local_path = d, local_only = TRUE)
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

test_that("convert_psychds writes requirements.txt from the code's packages", {
  llm_use(FALSE)
  d <- make_fixture_repo()
  # Give the fixture's analysis script real library() calls so code_check finds
  # packages to list.
  writeLines(c("library(dplyr)", "library(ggplot2)", "x <- 1"),
             file.path(d, "Code", "analysis.R"))

  out <- file.path(tempdir(), "pd_requirements"); unlink(out, recursive = TRUE)
  suppressMessages(
    convert_psychds(test_paper("x"), output_dir = out,
                    local_path = d, local_only = TRUE, overwrite = TRUE))

  req <- file.path(out, "requirements.txt")
  expect_true(file.exists(req))
  body <- readLines(req)
  expect_true(all(c("dplyr", "ggplot2") %in% body))
  # header documents provenance and the names-only limitation
  expect_true(any(grepl("metacheck", body)))
  expect_true(any(grepl("not pinned versions", body)))
})

test_that(".psychds_write_requirements never clobbers an existing dep file", {
  # The guard: a real requirements.txt / renv.lock / DESCRIPTION already at the
  # archive root wins (an authors' file is not overwritten). Tested directly on
  # the helper because convert_psychds(overwrite = TRUE) rebuilds the whole dir.
  out <- withr::local_tempdir()
  writeLines("SENTINEL", file.path(out, "renv.lock"))
  cc <- list(table = data.frame(packages = "dplyr, ggplot2"))
  ops <- list(code_check = cc)

  res <- metacheck:::.psychds_write_requirements(ops, out)
  expect_null(res)                                    # nothing written
  expect_false(file.exists(file.path(out, "requirements.txt")))
  expect_equal(readLines(file.path(out, "renv.lock")), "SENTINEL")

  # With no pre-existing dep file, it writes and returns the path.
  out2 <- withr::local_tempdir()
  res2 <- metacheck:::.psychds_write_requirements(ops, out2)
  expect_equal(res2, file.path(out2, "requirements.txt"))
  expect_true(all(c("dplyr", "ggplot2") %in% readLines(res2)))

  # No packages -> nothing written.
  out3 <- withr::local_tempdir()
  empty_ops <- list(code_check = list(table = data.frame(packages = character(0))))
  expect_null(metacheck:::.psychds_write_requirements(empty_ops, out3))
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
    "No data files to convert")

  expect_true(isTRUE(res$empty_plan))
  expect_equal(res$n_files_copied, 0L)
  expect_equal(res$n_studies, 0L)
  expect_length(res$descriptions, 0)
})
