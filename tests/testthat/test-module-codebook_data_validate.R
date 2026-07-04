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

test_that("codebook_check adds empty scale columns without an LLM", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "cb_scale_off"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # a 10-item Likert block (would be a scale) but no LLM to identify it
  set.seed(1)
  items <- as.data.frame(matrix(sample(1:5, 40 * 10, replace = TRUE), nrow = 40))
  names(items) <- paste0("panas_", 1:10)
  utils::write.csv(cbind(id = 1:40, items),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  ops <- report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  tbl <- ops[["codebook_check"]]$table
  # Schema is stable: the columns exist, and are all NA when no LLM ran.
  expect_true(all(c("scale", "scale_confidence") %in% names(tbl)))
  expect_true(all(is.na(tbl$scale)))
})

test_that("scale identification is gated when it would exceed codebook_max_calls", {
  # Three data files, each a distinct 8-item scale layout → three callable
  # survey layouts. With codebook_max_calls = 1 the whole scale tier is gated
  # (nothing identified) and the refusal names the parameter and the count
  # needed. The scale tier gates BEFORE its own LLM calls, so no scale is found.
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
  mk("panas", "a.csv"); mk("rosenberg", "b.csv"); mk("bigfive", "c.csv")

  ops <- suppressWarnings(report_module_run(
    test_paper("x"), c("data_check", "codebook_check"),
    args = list(
      data_check     = list(local_path = d, local_only = TRUE),
      codebook_check = list(codebook_max_calls = 1))))
  cb <- ops[["codebook_check"]]

  expect_true(all(is.na(cb$table$scale)))        # scale tier gated → nothing found
  expect_match(paste(cb$report, collapse = "\n"),
               "codebook_max_calls", fixed = TRUE)
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

test_that("codebook_check advises enabling an LLM for scale identification", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "cb_scale_note"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # A 10-item Likert block exists, but without an LLM it cannot be identified;
  # the report should tell the user how to enable identification.
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
  expect_match(report, "skipped without an LLM")
})

test_that("scale-block detection groups items and splits by prefix", {
  # 8 panas items then 6 rse items, same 1-5 metric, adjacent -> two blocks.
  set.seed(2)
  df <- as.data.frame(matrix(sample(1:5, 30 * 14, replace = TRUE), nrow = 30))
  names(df) <- c(paste0("panas_", 1:8), paste0("rse_", 1:6))
  blocks <- metacheck:::.detect_scale_blocks(df)
  expect_length(blocks, 2)
  expect_equal(vapply(blocks, length, integer(1)), c(8L, 6L))
  # a 4-item block is below the minimum and is not returned
  df2 <- as.data.frame(matrix(sample(1:5, 30 * 4, replace = TRUE), nrow = 30))
  names(df2) <- paste0("x_", 1:4)
  expect_length(metacheck:::.detect_scale_blocks(df2), 0)
})

test_that("data_validate flags planted data-quality issues", {
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_planted"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(data.frame(
    id    = 1:30,
    score = c(rnorm(29, 50, 5), 500),                 # outlier
    grp   = c(rep("Ctrl", 15), rep("ctrl", 14), "X"), # case + sparse
    flat  = rep(1, 30)                                 # constant
  ), file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]

  expect_true("data_validate" %in% module_list()$name)
  checks <- dv$table$check
  expect_true("Outliers" %in% checks)
  expect_true("Case issues" %in% checks)
  expect_true("Constant" %in% checks)
  expect_equal(dv$traffic_light, "red")   # several columns flagged
})

test_that("data_validate reports outliers as a table and one combined figure", {
  skip_if_not_installed("ggplot2")
  llm_use(FALSE)
  d <- file.path(tempdir(), "dv_report"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Two numeric columns, each with a planted outlier -> both listed in the table
  # and drawn as facets in a single combined figure.
  utils::write.csv(data.frame(
    id = 1:40,
    a  = c(rnorm(39, 10, 1), 100),
    b  = c(rnorm(39, 0, 1), -50)
  ), file.path(d, "data", "study.csv"), row.names = FALSE)

  ops <- report_module_run(
    test_paper("x"), c("data_check", "data_validate"),
    args = list(data_check = list(local_path = d, local_only = TRUE)))
  dv <- ops[["data_validate"]]
  report <- paste(dv$report, collapse = "\n")

  # An Outliers section with a per-column summary sentence.
  expect_true(any(grepl("#### Outliers", dv$report, fixed = TRUE)))
  expect_true(grepl("IQR fences", report))

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
    args = list(data_check = list(local_path = d, local_only = TRUE)))
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
  # The straightliner (last-but-one respondent) is flagged for straightlining.
  straight_id <- as.character(51)
  hit <- dv$careless[dv$careless$respondent == straight_id, ]
  expect_true(any(grepl("straightlining", hit$reason)))
  # Its longstring equals the full block; its IRV is (near) zero.
  expect_equal(max(hit$longstring), 10)
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
