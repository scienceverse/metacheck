# Module tests for codebook_check and data_validate, run offline via a local
# fixture repo (test_paper + local_path). Rules-only (llm_use(FALSE)).

# Source the sourced-module internals so the paper-context helpers can be tested
# directly (they are not in the package namespace). module_find() resolves the
# module file for the loaded metacheck (source tree under load_all, or installed).
.cb_env <- new.env()
sys.source(metacheck:::module_find("codebook_check"), envir = .cb_env)

test_that("scale paper-context retrieves naming sentences and corroborates", {
  p <- test_paper(c(
    "Affect was measured with the 20-item Positive and Negative Affect Schedule (PANAS).",
    "Participants rated how interested and enthusiastic they felt.",
    "Self-esteem was assessed using the Rosenberg scale."))

  ctx <- .cb_env$.scale_paper_context(
    p, prefixes = rep("panas", 10),
    labels = c("Interested", "Distressed", "Excited", "Enthusiastic", "Proud"))
  # It finds the PANAS-naming sentence (via prefix) and the item-word sentence.
  expect_true(any(grepl("PANAS", ctx)))
  expect_true(any(grepl("enthusiastic", ctx, ignore.case = TRUE)))

  # Corroboration: PANAS is named in this context, Rosenberg is not.
  expect_true(.cb_env$.scale_name_in_text(
    "Positive and Negative Affect Schedule (PANAS)", ctx))
  expect_false(.cb_env$.scale_name_in_text("Rosenberg Self-Esteem Scale", ctx))

  # No paper text / generic prefixes -> no context, no crash.
  expect_length(.cb_env$.scale_paper_context(
    test_paper(), prefixes = c("q", "v"), labels = NA_character_), 0)
})

make_cb_fixture <- function() {
  d <- file.path(tempdir(), paste0("cb_fix_", as.integer(runif(1, 1, 1e6))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(id = 1:6, dv = c(2.1, 3.4, 1.9, 4.0, 2.8, 30.0),
               binary = c(0, 1, 0, 1, 0, 1)),
    file.path(d, "data", "study1.csv"), row.names = FALSE)
  writeLines(c("varname,description",
               "dv,dependent variable",
               "binary,condition"),
             file.path(d, "codebook.csv"))
  writeLines("A readme.", file.path(d, "README.txt"))
  d
}

test_that("codebook_check reports documentation coverage", {
  llm_use(FALSE)
  d <- make_cb_fixture()
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  cc <- ops[["codebook_check"]]

  expect_true("codebook_check" %in% module_list()$name)
  expect_true(cc$traffic_light %in% c("green", "yellow", "red"))
  # dv + binary are documented; id is not (codebook has no id entry)
  expect_gt(cc$summary_table$matched_n, 0)
  expect_true("group" %in% names(cc$table))
  expect_match(cc$summary_text, "column")
})

test_that("codebook_check is red when no codebook documentation exists", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "cb_nocodebook"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(a = 1:5, b = c(1.1, 2.2, 3.3, 4.4, 5.5)),
                   file.path(d, "data", "d.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  cc <- ops[["codebook_check"]]
  expect_equal(cc$traffic_light, "red")
  expect_equal(cc$summary_table$codebook_var_n, 0)
})

test_that("rules matcher identifies a dictionary scale without an LLM", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "cb_scale_off"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # a 10-item Likert block whose prefix IS a dictionary acronym (PANAS): the
  # rules matcher names it from the `scales` dictionary with no LLM. (Before the
  # dictionary rewrite, scale identification was LLM-only and this was all-NA.)
  set.seed(1)
  items <- as.data.frame(matrix(sample(1:5, 40 * 10, replace = TRUE), nrow = 40))
  names(items) <- paste0("panas_", 1:10)
  utils::write.csv(cbind(id = 1:40, items),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  tbl <- ops[["codebook_check"]]$table
  # Schema is stable (incl. the new scale_source), and PANAS is identified.
  expect_true(all(c("scale", "scale_confidence", "scale_source") %in% names(tbl)))
  named <- !is.na(tbl$scale) & nzchar(tbl$scale)
  expect_true(any(named))
  expect_true(all(tbl$scale[named] == "Positive and Negative Affect Schedule"))
  expect_true(all(tbl$scale_source[named] == "matched"))
})

test_that("no scale is named when neither dictionary nor manuscript identifies it", {
  # Three data files with non-dictionary prefixes and no paper naming; the LLM is
  # mocked to an unusable response. No scale is named, but the column groups are
  # still detected and reported (with a low call budget honoured throughout).
  llm_use(TRUE)
  # Mock llm() to a harmless empty structured response so data_check's own LLM
  # tiers don't hit the network; the scale tier should never even reach here.
  fake_llm <- function(text, text_col = "text", ...) {
    n <- length(unique(text[[text_col]]))
    df <- data.frame(results.index = seq_len(n), results.value = rep("other", n))
    class(df) <- c("metacheck_llm", "data.frame")
    attr(df, "llm") <- list(model = "mock")
    df
  }
  testthat::local_mocked_bindings(llm = fake_llm)

  d <- file.path(tempdir(), "cb_scale_gate"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  set.seed(3)
  mk <- function(prefix, file) {
    items <- as.data.frame(matrix(sample(1:5, 40 * 8, replace = TRUE), nrow = 40))
    names(items) <- paste0(prefix, "_", 1:8)
    utils::write.csv(cbind(id = 1:40, items),
                     file.path(d, "data", file), row.names = FALSE)
  }
  # Prefixes that are NOT dictionary acronyms and cannot be named from the text,
  # and the LLM is mocked to an unusable shape: no scale is named. (The manuscript
  # matcher is one LLM call per file, bounded by codebook_max_calls; there is no
  # separate upfront gate message any more.)
  mk("blockx", "a.csv"); mk("blocky", "b.csv"); mk("blockz", "c.csv")

  ops <- suppressWarnings(report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(
      data_check     = list(local_path = d, local_only = TRUE),
      codebook_check = list(codebook_max_calls = 1))))
  cb <- ops[["codebook_check"]]

  # No dictionary match and no manuscript naming → nothing named, but the groups
  # are still detected and reported.
  expect_true(all(is.na(cb$table$scale)))
  expect_match(paste(cb$report, collapse = "\n"), "#### Scales", fixed = TRUE)
})

test_that("scale metadata is emitted into Psych-DS variableMeasured", {
  # The PropertyValue builder puts the scale name in schema.org's native
  # measurementTechnique and the grouping in a namespaced metacheck:scale block.
  cols <- data.frame(
    source_file = "s.csv", column_name = c("panas_1", "panas_2"),
    representation = "numeric", measurement_level = "ordinal",
    concept = "likert", role = "measure", unit = NA_character_,
    quality = "ok", parse_note = NA_character_,
    min = 1, max = 5, n = 40, stringsAsFactors = FALSE)
  labels <- data.frame(
    source_file = "s.csv", column_name = c("panas_1", "panas_2"),
    label = c("Interested", "Distressed"), label_status = "labelled",
    label_source = "codebook", label_method = NA_character_,
    codebook_variable = c("panas_1", "panas_2"),
    scale = "PANAS", scale_confidence = "high", stringsAsFactors = FALSE)

  vm <- metacheck:::.psychds_variable_measured(cols, labels)
  expect_equal(vm[[1]][["measurementTechnique"]], "PANAS")
  expect_equal(vm[[1]][["metacheck:scale"]][["name"]], "PANAS")
  expect_equal(vm[[1]][["metacheck:scale"]][["confidence"]], "high")
  # The facets are emitted as separate properties.
  expect_equal(vm[[1]][["metacheck:measurementLevel"]], "ordinal")
  expect_equal(vm[[1]][["metacheck:concept"]], "likert")
  expect_equal(vm[[1]][["metacheck:representation"]], "numeric")
  # A variable with no scale carries neither field.
  vm2 <- metacheck:::.psychds_variable_measured(
    cols, labels[, setdiff(names(labels), c("scale", "scale_confidence"))])
  expect_null(vm2[[1]][["measurementTechnique"]])
})

test_that("psychds variableMeasured emits DDI code list, missing, question", {
  cols <- data.frame(
    source_file = "s.csv", column_name = "sex",
    representation = "numeric", measurement_level = "nominal",
    concept = "gender", role = "measure", unit = NA_character_,
    quality = "ok", parse_note = NA_character_, n = 40,
    stringsAsFactors = FALSE)
  labels <- data.frame(
    source_file = "s.csv", column_name = "sex", label = "Sex",
    label_status = "labelled", label_source = "codebook",
    label_method = NA_character_, codebook_variable = "sex",
    value_labels = '{"1":"Male","2":"Female","-99":"Refused"}',
    missing_values = '{"-99":"Refused"}',
    question = "What is your sex?", universe = "All respondents",
    stringsAsFactors = FALSE)
  vm <- metacheck:::.psychds_variable_measured(cols, labels)
  pv <- vm[[1]]
  # Code list: one PropertyValue child per code, plus the raw JSON.
  expect_length(pv[["metacheck:codeList"]], 3)
  expect_equal(pv[["metacheck:codeList"]][[1]][["value"]], "1")
  expect_equal(pv[["metacheck:codeList"]][[1]][["name"]], "Male")
  expect_true(grepl("Refused", pv[["metacheck:missingValues"]]))
  expect_equal(pv[["metacheck:question"]], "What is your sex?")
  expect_equal(pv[["metacheck:universe"]], "All respondents")
})

test_that("codebook_check reports identified scales and advises reaching high confidence", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "cb_scale_note"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # A 10-item PANAS block: the rules matcher identifies it from the dictionary
  # even without an LLM. With no paper text to confirm the instrument, it is
  # medium confidence, and the report advises how to reach high confidence.
  set.seed(1)
  items <- as.data.frame(matrix(sample(1:5, 40 * 10, replace = TRUE), nrow = 40))
  names(items) <- paste0("panas_", 1:10)
  utils::write.csv(cbind(id = 1:40, items),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  report <- paste(ops[["codebook_check"]]$report, collapse = "\n")
  expect_true(grepl("#### Scales", report, fixed = TRUE))
  # A scale was identified (no longer "skipped without an LLM"), and the report
  # guides the user toward high-confidence identification.
  expect_match(report, "high confidence")
})

test_that("scale-block detection groups items and splits by prefix", {
  # 8 panas items then 6 rse items, same 1-5 metric, adjacent -> two blocks.
  set.seed(2)
  df <- as.data.frame(matrix(sample(1:5, 30 * 14, replace = TRUE), nrow = 30))
  names(df) <- c(paste0("panas_", 1:8), paste0("rse_", 1:6))
  blocks <- metacheck:::.detect_scale_blocks(df)
  expect_length(blocks, 2)
  expect_equal(vapply(blocks, length, integer(1)), c(8L, 6L))
  # 4 items is the minimum, so a 4-item block IS returned ...
  df2 <- as.data.frame(matrix(sample(1:5, 30 * 4, replace = TRUE), nrow = 30))
  names(df2) <- paste0("x_", 1:4)
  expect_length(metacheck:::.detect_scale_blocks(df2), 1)
  # ... but a 3-item block is below the minimum and is not returned.
  df3 <- as.data.frame(matrix(sample(1:5, 30 * 3, replace = TRUE), nrow = 30))
  names(df3) <- paste0("y_", 1:3)
  expect_length(metacheck:::.detect_scale_blocks(df3), 0)
})

test_that("paper-text scan finds named instruments and ignores unrelated text", {
  p <- test_paper(c(
    "We administered the 20-item PANAS and the Rosenberg Self-Esteem Scale.",
    "Personality was measured with the HEXACO inventory."))
  hits <- .cb_env$.scan_paper_for_scales(p)
  # The scan now returns canonical dictionary names (from `scales`), not the old
  # hardcoded keys.
  expect_true(all(c("Positive and Negative Affect Schedule",
                    "Rosenberg Self-Esteem Scale",
                    "HEXACO Personality Inventory") %in% hits))

  # No instruments mentioned -> no hits; a bare paper -> empty, no error.
  none <- test_paper(c("We recorded reaction times.", "No surveys were used."))
  expect_length(.cb_env$.scan_paper_for_scales(none), 0)
  expect_length(.cb_env$.scan_paper_for_scales(NULL), 0)

  # Acronym-only mention is enough (word-boundary, not a substring of a word).
  acr <- test_paper("Stress was indexed by the PSS.")
  expect_true("Perceived Stress Scale" %in% .cb_env$.scan_paper_for_scales(acr))
})

test_that("data_validate flags planted data-quality issues", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_planted"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(
    id     = 1:30,
    likert = c(sample(1:5, 29, replace = TRUE), 55),   # out-of-range (1–5 + 55)
    grp    = c(rep("Ctrl", 15), rep("ctrl", 14), "X"), # case issues
    flat   = rep(1, 30)                                 # constant
  ), file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]

  expect_true("data_validate" %in% module_list()$name)
  checks <- dv$table$check
  expect_true("Values outside the scale" %in% checks)
  expect_true("Case issues" %in% checks)
  expect_true("Constant" %in% checks)
  expect_equal(dv$traffic_light, "red")   # several columns flagged
})

test_that("data_validate tiers constant columns and flags SPSS filters", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_constant"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  df <- data.frame(
    id        = 1:30,
    condition = rep("control", 30),  # constant design variable -> flagged
    version   = rep("v3", 30),       # constant text metadata -> note only
    nothing   = rep(NA, 30),         # empty column -> flagged
    flat      = rep(7, 30)           # constant numeric -> flagged
  )
  df[["filter_$"]] <- rep(1, 30)     # SPSS Select Cases filter -> flagged
  utils::write.csv(df, file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]

  expect_true("Empty column" %in% dv$table$check)
  expect_true("SPSS filter variable" %in% dv$table$check)
  const_cols <- dv$table$column[dv$table$check == "Constant"]
  expect_true(all(c("condition", "flat") %in% const_cols))
  # The constant text column is not counted as an issue...
  expect_false("version" %in% const_cols)
  # ...but is listed in the informational metadata note.
  report <- paste(dv$report, collapse = "\n")
  expect_true(grepl("file-level metadata", report))
  expect_true(grepl("version = \"v3\"", report, fixed = TRUE))
})

test_that("data_validate reports outliers as a table and one combined figure", {
  skip_if_not_installed("ggplot2")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_report"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Two bounded columns, each with a planted out-of-range value -> both listed
  # in the table; all numeric columns are drawn as facets in one figure.
  utils::write.csv(data.frame(
    id = 1:40,
    a  = c(sample(1:5, 39, replace = TRUE), 55),   # 1–5 scale + stray 55
    b  = c(sample(1:7, 39, replace = TRUE), -9)    # 1–7 scale + stray -9
  ), file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE),
                data_validate = list(plot_distributions = TRUE)))
  dv <- ops[["data_validate"]]
  report <- paste(dv$report, collapse = "\n")

  # An Out-of-Range Values section with a per-column summary sentence.
  expect_true(any(grepl("#### Out-of-Range Values", dv$report, fixed = TRUE)))
  expect_true(grepl("data-entry error", report))

  # A single combined distribution figure: exactly one embedded <img>, not one
  # per column (the old behaviour rendered a plot per numeric column).
  expect_true(any(grepl("#### Distributions", dv$report, fixed = TRUE)))
  n_imgs <- lengths(regmatches(report, gregexpr("<img ", report)))
  expect_equal(n_imgs, 1L)
  expect_true(grepl("data:image/png;base64", report))
})

test_that("data_validate distribution figure caps the number of facets", {
  skip_if_not_installed("ggplot2")
  llm_use(FALSE)
  # Many numeric columns (more than the internal facet cap of 40) -> the figure
  # is truncated and says so, rather than rendering one facet per column.
  d <- file.path(tempdir(), "dv_wide"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  n_cols <- 60L
  wide <- as.data.frame(matrix(stats::rnorm(30 * n_cols), nrow = 30))
  names(wide) <- paste0("v", seq_len(n_cols))
  utils::write.csv(wide, file.path(d, "data", "wide.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE),
                data_validate = list(plot_distributions = TRUE)))
  report <- paste(ops[["data_validate"]]$report, collapse = "\n")

  expect_true(grepl("Showing the first", report))
  expect_true(grepl(paste0("of ", n_cols, " numeric columns"), report))
  # Still a single combined figure.
  expect_equal(lengths(regmatches(report, gregexpr("<img ", report))), 1L)
})

test_that("data_validate is green on clean data", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_clean"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Evenly spread values (uniform sequence) have no Tukey outliers, so a clean
  # column genuinely produces zero findings. (Random normal data at n=40 would
  # occasionally have a value just past 1.5*IQR — that is correct, not an error.)
  utils::write.csv(data.frame(
    id = 1:40,
    x  = seq(10, 30, length.out = 40),
    y  = seq(1, 5, length.out = 40)
  ), file.path(d, "data", "clean.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]
  expect_equal(dv$traffic_light, "green")
  expect_equal(nrow(dv$table), 0)
})

# ── Careless responding ───────────────────────────────────────────────────────

# Build a survey fixture: `n_ok` honest respondents on a Likert scale plus a
# straightliner and an erratic (alternating) responder, with an id column.
make_survey <- function(prefix = "panas", n_items = 10, n_ok = 50,
                        levels = 2:4, seed = 1) {
  set.seed(seed)
  items <- as.data.frame(matrix(sample(levels, n_ok * n_items, replace = TRUE),
                                nrow = n_ok))
  names(items) <- paste0(prefix, "_", seq_len(n_items))
  straight <- as.data.frame(matrix(rep(median(levels), n_items), nrow = 1))
  erratic  <- as.data.frame(matrix(rep(c(min(levels) - 1, max(levels) + 1),
                                       length.out = n_items), nrow = 1))
  names(straight) <- names(erratic) <- names(items)
  items <- rbind(items, straight, erratic)
  cbind(participant_id = seq_len(n_ok + 2), items)
}

test_that("data_validate flags careless survey responders", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_careless"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(make_survey(), file.path(d, "data", "survey.csv"),
                   row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]

  expect_true("careless" %in% names(dv))
  expect_gt(nrow(dv$careless), 0)
  # One row per respondent now (aggregated across blocks), with these columns.
  expect_true(all(c("respondent", "n_blocks_flagged", "reasons",
                    "max_longstring", "short_scale_only") %in% names(dv$careless)))
  expect_equal(anyDuplicated(dv$careless$respondent), 0L)
  # The straightliner (last-but-one respondent) is flagged for straightlining,
  # with a longstring equal to the full 10-item block.
  straight_id <- as.character(51)
  hit <- dv$careless[dv$careless$respondent == straight_id, ]
  expect_equal(nrow(hit), 1L)
  expect_true(grepl("straightlining", hit$reasons))
  expect_equal(hit$max_longstring, 10)
  # Report carries a Careless Responding section and the summary_text mentions it.
  expect_true(any(grepl("#### Careless Responding", dv$report, fixed = TRUE)))
  expect_match(dv$summary_text, "careless responding")
})

test_that("careless scale blocks split by variable-name prefix", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_two_scales"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Two adjacent scales on the same 1-5 metric: panas_1..8 then rse_1..6. They
  # must be detected as TWO blocks, not merged, because the prefix changes.
  s1 <- make_survey("panas", n_items = 8, n_ok = 40, levels = 1:5, seed = 2)
  s2 <- make_survey("rse",   n_items = 6, n_ok = 40, levels = 1:5, seed = 3)
  wide <- cbind(s1, s2[, -1, drop = FALSE])  # drop duplicate id from s2
  utils::write.csv(wide, file.path(d, "data", "survey.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  scales <- unique(ops[["data_validate"]]$careless$scale)
  expect_true(any(grepl("^panas", scales)))
  expect_true(any(grepl("^rse", scales)))
})

test_that("data_validate does not run careless without an id or a scale block", {
  skip_if_not_installed("careless")
  llm_use(FALSE)
  # A survey block but NO identifier column: careless is not actionable, skipped.
  d <- file.path(tempdir(), "dv_noid"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  s <- make_survey()[, -1, drop = FALSE]   # drop participant_id
  utils::write.csv(s, file.path(d, "data", "survey.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  expect_equal(nrow(ops[["data_validate"]]$careless), 0)

  # Non-survey data (no Likert block): careless produces nothing either.
  d2 <- file.path(tempdir(), "dv_nonsurvey"); unlink(d2, recursive = TRUE)
  dir.create(file.path(d2, "data"), recursive = TRUE)
  utils::write.csv(data.frame(id = 1:40, rt = rnorm(40, 500, 50),
                              age = sample(18:65, 40, replace = TRUE)),
                   file.path(d2, "data", "d.csv"), row.names = FALSE)
  ops2 <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d2, local_only = TRUE)))
  expect_equal(nrow(ops2[["data_validate"]]$careless), 0)
})
