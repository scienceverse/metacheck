# Tests for convert_codebook: preparing a labelled data frame + Rmd + JSON-LD
# for the codebook package. Runs offline against a local fixture repo, no LLM,
# and does not require the `codebook` package or pandoc (HTML render is skipped
# and asserted skipped when they are absent).

# A small repository fixture with a data file and a matching codebook.
make_codebook_fixture <- function() {
  d <- file.path(tempdir(), paste0("cb_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(id = 1:5, score = c(2.1, 3.4, 1.9, 4.0, 2.8),
               cond = c("a", "b", "a", "b", "a")),
    file.path(d, "data", "study1.csv"), row.names = FALSE)
  writeLines(c("var,description", "id,participant id", "score,outcome measure",
               "cond,experimental condition"),
             file.path(d, "codebook.csv"))
  writeLines("A readme.", file.path(d, "README.txt"))
  d
}

test_that("convert_codebook writes labelled rds, Rmd and JSON-LD", {
  llm_use(FALSE)
  d <- make_codebook_fixture()
  out <- file.path(tempdir(), "cb_out"); unlink(out, recursive = TRUE)

  res <- convert_codebook(test_paper("x"), output_dir = out,
                          local_path = d, local_only = TRUE, overwrite = TRUE)

  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(out, "codebook_data.rds")))
  expect_true(file.exists(file.path(out, "codebook.Rmd")))
  expect_true(file.exists(file.path(out, "codebook_metadata.json")))
  expect_equal(res$n_studies, 1)

  # The saved data frame carries variable labels and real rows.
  df <- readRDS(file.path(out, "codebook_data.rds"))
  expect_true(all(c("id", "score", "cond") %in% names(df)))
  expect_gt(nrow(df), 0)
  # `score`/`id` came from a CSV codebook (var,description); the label matched.
  expect_equal(attr(df$score, "label"), "outcome measure")
  # We do not synthesise self-referential value labels from sample values (they
  # equal the codes and break codebook rendering on numeric columns), so no
  # column gets a fabricated `labels` attribute.
  expect_true(is.null(attr(df$cond, "labels")))
  expect_true(is.null(attr(df$score, "labels")))
  # Dataset-level metadata is attached.
  expect_false(is.null(attr(df, "metadata")))
  expect_true(nzchar(attr(df, "metadata")$name))

  # JSON-LD is a schema.org Dataset naming every variable.
  meta <- jsonlite::fromJSON(file.path(out, "codebook_metadata.json"),
                             simplifyVector = FALSE)
  expect_equal(meta[["@type"]], "Dataset")
  vm <- vapply(meta$variableMeasured, function(v) v$name, character(1))
  expect_true(all(c("id", "score", "cond") %in% vm))

  # The Rmd is runnable: loads the rds and calls codebook().
  rmd <- readLines(file.path(out, "codebook.Rmd"))
  expect_true(any(grepl("readRDS\\(\"codebook_data.rds\"\\)", rmd)))
  expect_true(any(grepl("codebook\\(codebook_data\\)", rmd)))
  # It acknowledges the codebook package with its citation.
  expect_true(any(grepl("codebook", rmd, ignore.case = TRUE)))
  expect_true(any(grepl("10.1177/2515245919838783", rmd, fixed = TRUE)))
})

test_that("convert_codebook skips the HTML render when codebook/pandoc absent", {
  skip_if(requireNamespace("codebook", quietly = TRUE) &&
            rmarkdown::pandoc_available(),
          "codebook + pandoc available; render path is exercised elsewhere")
  llm_use(FALSE)
  d <- make_codebook_fixture()
  out <- file.path(tempdir(), "cb_norender"); unlink(out, recursive = TRUE)

  res <- convert_codebook(test_paper("x"), output_dir = out,
                          local_path = d, local_only = TRUE, overwrite = TRUE)
  expect_false(res$rendered)
  expect_length(res$html_files, 0)
  # The inputs are still there for the user to render later.
  expect_true(file.exists(file.path(out, "codebook.Rmd")))
})

test_that("convert_codebook reuses a captured report result passed as `paper`", {
  llm_use(FALSE)
  d <- make_codebook_fixture()
  results <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))

  # Reuse by passing the captured result directly as the first argument.
  out1 <- file.path(tempdir(), "cb_reuse_paper"); unlink(out1, recursive = TRUE)
  res1 <- convert_codebook(results, output_dir = out1)
  expect_equal(res1$n_studies, 1)
  expect_true(file.exists(file.path(out1, "codebook_data.rds")))

  # The captured result carries the paper as an attribute, so reusing it
  # recovers the real paper metadata (title/authors) rather than a blank stub.
  p <- demopaper()
  results_meta <- report_module_run(
    p, c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  expect_true(inherits(attr(results_meta, "paper"), "scivrs_paper"))
  outm <- file.path(tempdir(), "cb_reuse_meta"); unlink(outm, recursive = TRUE)
  convert_codebook(results_meta, output_dir = outm)
  meta <- jsonlite::fromJSON(file.path(outm, "codebook_metadata.json"),
                             simplifyVector = FALSE)
  expect_true(grepl("To Err is Human", meta$name, fixed = TRUE))
  expect_false(is.null(meta$creator))
})

test_that("convert_codebook re-runs the full chain for a partial result", {
  llm_use(FALSE)
  d <- make_codebook_fixture()
  # A partial captured result (only data_check) still carries the paper as an
  # attribute, so reusing it as `paper` recovers the paper and re-runs the full
  # chain to fill the gap.
  partial <- report_module_run(
    test_paper("x"), "data_check",
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  out <- file.path(tempdir(), "cb_partial"); unlink(out, recursive = TRUE)

  res <- convert_codebook(partial, output_dir = out,
                          local_path = d, local_only = TRUE)
  expect_equal(res$n_studies, 1)
  df <- readRDS(file.path(out, "codebook_data.rds"))
  # codebook_check filled the labels, so at least one variable is labelled.
  labelled <- vapply(names(df), function(n) !is.null(attr(df[[n]], "label")),
                     logical(1))
  expect_true(any(labelled))

  # Only a results object with no recoverable paper at all errors clearly.
  bare <- list(data_check = list(table = data.frame(x = 1)))
  expect_error(
    convert_codebook(bare, output_dir = tempfile()),
    "no paper object is available")
})

test_that("convert_codebook surfaces an upstream module failure with its message", {
  # A captured result where data_check failed (as report_module_run records it:
  # traffic_light == "fail", error text in $report) should raise that real error
  # rather than the generic "no columns" one.
  mk_fail <- function(module, msg) {
    mo <- list(module = module, traffic_light = "fail", report = msg,
               table = NULL, summary_table = NULL, paper = paper("p"))
    class(mo) <- "metacheck_module_output"
    mo
  }
  res <- list(
    data_check = mk_fail("data_check",
      "This would make 47 calls to the LLM, but your maximum number of calls is set to 30."),
    codebook_check = mk_fail("codebook_check", "failed"))
  attr(res, "paper") <- paper("p")

  expect_error(
    convert_codebook(res, output_dir = tempfile()),
    "'data_check' module failed")
  expect_error(
    convert_codebook(res, output_dir = tempfile()),
    "llm_max_calls")
})

test_that("convert_codebook drops all-NA columns the codebook package can't summarise", {
  llm_use(FALSE)
  # A data file with an entirely empty column (as blank spreadsheet headers
  # produce). codebook() errors on such a column (median(table(x)) is NA), so
  # convert_codebook must drop it.
  d <- file.path(tempdir(), paste0("cb_na_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(id = 1:5, score = c(2.1, 3.4, 1.9, 4.0, 2.8),
               empty_col = rep(NA, 5)),
    file.path(d, "data", "study1.csv"), row.names = FALSE)
  out <- file.path(tempdir(), "cb_na_out"); unlink(out, recursive = TRUE)

  convert_codebook(test_paper("x"), output_dir = out,
                   local_path = d, local_only = TRUE, overwrite = TRUE)
  df <- readRDS(file.path(out, "codebook_data.rds"))
  expect_false("empty_col" %in% names(df))
  expect_true(all(c("id", "score") %in% names(df)))
  expect_false(any(vapply(df, function(x) all(is.na(x)), logical(1))))
})

test_that("convert_codebook skips gracefully when output exists without overwrite", {
  llm_use(FALSE)
  d <- make_codebook_fixture()
  out <- file.path(tempdir(), "cb_nooverwrite"); unlink(out, recursive = TRUE)
  convert_codebook(test_paper("x"), output_dir = out,
                   local_path = d, local_only = TRUE, overwrite = TRUE)

  # Re-running without overwrite messages and skips, rather than erroring.
  expect_message(
    res <- convert_codebook(test_paper("x"), output_dir = out,
                            local_path = d, local_only = TRUE),
    "already exists")
  expect_true(isTRUE(res$existed))
  expect_equal(res$n_studies, 0L)
})

test_that("convert_codebook skips gracefully when no extracted columns exist", {
  llm_use(FALSE)

  chain <- report_module_run(
    test_paper("x"),
    c("data_check", "codebook_check"),
    args = list(data_check = list(download = "none", local_only = TRUE)))

  out <- file.path(tempdir(), "cb_empty_columns")
  expect_message(
    res <- convert_codebook(chain, output_dir = out, render = FALSE, overwrite = TRUE),
    "No extracted data columns")

  expect_true(isTRUE(res$empty_columns))
  expect_equal(res$n_studies, 0L)
  expect_false(res$rendered)
  expect_length(res$rds_files, 0)
  expect_length(res$rmd_files, 0)
  expect_length(res$metadata_files, 0)
  expect_length(res$html_files, 0)
})

test_that("convert_codebook carries identified scales into its outputs", {
  labels <- data.frame(
    source_file = "s.csv", column_name = c("panas_1", "panas_2", "age"),
    label = c("Interested", "Distressed", NA_character_),
    label_status = c("labelled", "labelled", "unlabelled"),
    scale = c("PANAS", "PANAS", NA_character_),
    scale_confidence = c("high", "high", NA_character_),
    stringsAsFactors = FALSE)
  var_names <- c("panas_1", "panas_2", "age")

  # Dataset-level scales list (name + members), mirroring DDI varGrp membership.
  sc <- metacheck:::.codebook_scales(labels, var_names)
  expect_length(sc, 1)
  expect_equal(sc[[1]]$name, "PANAS")
  expect_setequal(unlist(sc[[1]]$variables), c("panas_1", "panas_2"))

  # Per-column scale attribute on the labelled data frame (preserved in the rds).
  df   <- data.frame(panas_1 = 1:3, panas_2 = 1:3, age = 20:22)
  cols <- data.frame(source_file = "s.csv", column_name = var_names,
                     col_type = "ordinal", stringsAsFactors = FALSE)
  lab_df <- metacheck:::.codebook_label_df(df, cols, labels)
  expect_equal(attr(lab_df$panas_1, "scale"), "PANAS")
  expect_null(attr(lab_df$age, "scale"))

  # measurementTechnique in the JSON-LD codebook metadata.
  tf <- withr::local_tempfile(fileext = ".json")
  scale_of <- function(v) {
    s <- labels$scale[labels$column_name == v & !is.na(labels$scale)]
    if (length(s)) s[[1]] else NA_character_
  }
  metacheck:::.codebook_write_jsonld(list(name = "x", description = "y"),
                                     var_names, tf, scale_of = scale_of)
  j <- jsonlite::fromJSON(tf, simplifyVector = FALSE)
  vm <- j[["variableMeasured"]]
  panas <- Filter(function(v) v$name == "panas_1", vm)[[1]]
  expect_equal(panas[["measurementTechnique"]], "PANAS")
  age <- Filter(function(v) v$name == "age", vm)[[1]]
  expect_null(age[["measurementTechnique"]])
})
