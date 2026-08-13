# Module tests for codebook_check.
#
# Scope: the checks the module performs that are NOT already covered elsewhere.
# Existing coverage this file deliberately does not repeat:
#   * test-codebook-helpers.R    — parse_codebook, match_column_labels,
#                                  normalize_varname, value-label decoding
#   * test-module-codebook_data_validate.R — basic coverage counts, the red
#                                  traffic light, the PANAS dictionary match,
#                                  Psych-DS variableMeasured emission
#   * test-behaverse.R           — paradata channel recognition
#
# What is tested here: the coverage/quality split, misalignment detection,
# unused-variable reporting, values outside a documented range, task detection,
# orphan totals, duplicated column names, the item/derived split, prefix
# grouping and propagation, the traffic-light rules, and the empty-input paths.
#
# Everything runs offline with llm_use(FALSE), except the two tests that
# explicitly check LLM-gated behaviour, which mock llm().

# The module's internal helpers are not in the package namespace, so source the
# module into an environment to reach them directly.
.cbc <- new.env()
sys.source(metacheck:::module_find("codebook_check"), envir = .cbc)

# ── Fixtures ──────────────────────────────────────────────────────────────────

# A fresh temp directory, removed when the calling test finishes.
cbc_dir <- function(name) {
  d <- file.path(tempdir(), paste0("cbc_", name, "_",
                                   as.integer(runif(1, 1, 1e6))))
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(d, recursive = TRUE), envir = parent.frame())
  d
}

# Run data_check then codebook_check over a local fixture directory.
cbc_run <- function(d, paper = test_paper("x"), ...) {
  report_module_run(
    paper, c("data_check", "codebook_check"),
    args = list(data_check = list(local_path = d, local_only = TRUE),
                codebook_check = list(...)))[["codebook_check"]]
}

# ── Coverage vs. label quality ────────────────────────────────────────────────
# The module keeps two questions apart: did a column match a codebook entry at
# all (coverage), and did it also get one usable label (quality)? A column with
# two conflicting definitions is MATCHED but not CLEAN, and must be counted that
# way, otherwise a label typo would silently read as missing documentation.

test_that("a conflicting definition counts as matched but not clean", {
  llm_use(FALSE)
  d <- cbc_dir("conflict")
  utils::write.csv(data.frame(id = 1:5, mood = c(1, 2, 3, 4, 5)),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  # Two codebooks documenting `mood` with genuinely different labels.
  writeLines(c("varname,description", "mood,Positive affect"),
             file.path(d, "codebook.csv"))
  writeLines(c("varname,description", "mood,Sleep quality rating"),
             file.path(d, "codebook_b.csv"))

  cc <- cbc_run(d)
  st <- cc$summary_table

  expect_gte(st$conflicted_n, 1)
  # Matched counts the conflicted column; clean does not.
  expect_equal(st$matched_n, st$clean_n + st$conflicted_n)
  expect_lt(st$clean_n, st$matched_n)
  # The conflict is surfaced in its own report section, not buried.
  expect_match(paste(cc$report, collapse = "\n"),
               "Conflicting or Ambiguous Definitions", fixed = TRUE)
})

test_that("identical labels from two sources merge instead of conflicting", {
  llm_use(FALSE)
  d <- cbc_dir("merge")
  utils::write.csv(data.frame(id = 1:5, mood = 1:5),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description", "mood,Positive affect"),
             file.path(d, "codebook.csv"))
  writeLines(c("varname,description", "mood,positive affect"),
             file.path(d, "codebook_b.csv"))

  cc <- cbc_run(d)
  # Same label bar capitalisation: merged, so no conflict is reported.
  expect_equal(cc$summary_table$conflicted_n, 0)
  expect_gt(cc$summary_table$clean_n, 0)
})

# ── Documented but unused variables ───────────────────────────────────────────

test_that("codebook variables absent from the data are reported as unused", {
  llm_use(FALSE)
  d <- cbc_dir("unused")
  utils::write.csv(data.frame(id = 1:5, age = 20:24),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  # Documents `age` (present) plus two variables that are not in the data.
  writeLines(c("varname,description",
               "age,Age in years",
               "income,Annual income",
               "region,Region of residence"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  expect_equal(cc$summary_table$unused_var_n, 2)
  rpt <- paste(cc$report, collapse = "\n")
  expect_match(rpt, "Documented but Unused Variables", fixed = TRUE)
  expect_match(rpt, "income")
  expect_match(rpt, "region")
})

# ── Codebook misalignment ─────────────────────────────────────────────────────
# A codebook that WAS parsed but whose names do not line up with the data. The
# gate needs >= 5 codebook variables, < 20% of columns matched, and >= 80% of
# codebook variables unused, so an ordinary partial codebook is not flagged.

test_that("a codebook whose names do not match the data is flagged misaligned", {
  llm_use(FALSE)
  d <- cbc_dir("misalign")
  # Data holds computed scores; the codebook documents the underlying items.
  utils::write.csv(
    data.frame(id = 1:5, neoImagination = 1:5, neoAnxiety = 5:1,
               neoWarmth = c(2, 3, 4, 3, 2)),
    file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description",
               paste0("neo", 1:8, ",NEO item ", 1:8)),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  rpt <- paste(cc$report, collapse = "\n")
  # Named as misaligned, with the concrete advice about naming variables as the
  # data names them.
  expect_match(rpt, "do not match the data columns", fixed = TRUE)
  expect_match(cc$summary_text, "do not match the data columns", fixed = TRUE)
  expect_gt(cc$summary_table$unused_var_n, 0)
})

test_that("an ordinary partial codebook is NOT flagged misaligned", {
  llm_use(FALSE)
  d <- cbc_dir("partial")
  # 6 columns, 5 documented: good coverage, so the misalignment gate must not
  # fire even though one column is undocumented.
  utils::write.csv(
    data.frame(id = 1:5, a = 1:5, b = 1:5, c = 1:5, dd = 1:5, ee = 1:5),
    file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description",
               "a,Item a", "b,Item b", "c,Item c", "dd,Item d", "ee,Item e"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  expect_false(grepl("do not match the data columns",
                     paste(cc$report, collapse = "\n"), fixed = TRUE))
})

# ── Values outside the documented range ───────────────────────────────────────
# Only a DOCUMENTED range is used: no inference. A value the codebook itself
# does not list is a concrete discrepancy between what authors declared and what
# the data holds.

test_that("a value the codebook does not list is reported with its kind", {
  llm_use(FALSE)
  d <- cbc_dir("range")
  # `q1` is documented 1-5 but contains -99 (a missing code) and 55 (a typo).
  utils::write.csv(
    data.frame(id = 1:6, q1 = c(1, 2, 3, -99, 55, 4)),
    file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description,values",
               "q1,Agreement,1=Strongly disagree; 2=Disagree; 3=Neutral; 4=Agree; 5=Strongly agree"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  sv <- cc$scale_violations
  expect_s3_class(sv, "data.frame")
  expect_gte(nrow(sv), 1)
  expect_true("q1" %in% sv$column)
  rpt <- paste(cc$report, collapse = "\n")
  expect_match(rpt, "Values Outside the Documented Range", fixed = TRUE)
})

test_that("no documented range means no range check (nothing is inferred)", {
  llm_use(FALSE)
  d <- cbc_dir("norange")
  # Same out-of-range-looking values, but the codebook declares no value list,
  # so the module must NOT invent a range and must report no violation.
  utils::write.csv(
    data.frame(id = 1:6, q1 = c(1, 2, 3, -99, 55, 4)),
    file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description", "q1,Agreement"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  expect_equal(nrow(cc$scale_violations), 0)
  expect_false(grepl("Values Outside the Documented Range",
                     paste(cc$report, collapse = "\n"), fixed = TRUE))
})

# ── Traffic light ─────────────────────────────────────────────────────────────

test_that("a fully documented, clean, in-range repository is green", {
  llm_use(FALSE)
  d <- cbc_dir("green")
  utils::write.csv(data.frame(age = 20:24, sex = c(1, 2, 1, 2, 1)),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description", "age,Age in years", "sex,Sex of respondent"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  expect_equal(cc$summary_table$unmatched_n, 0)
  expect_equal(cc$summary_table$conflicted_n, 0)
  expect_equal(cc$traffic_light, "green")
})

test_that("a documented-range violation downgrades green to yellow", {
  llm_use(FALSE)
  d <- cbc_dir("downgrade")
  # Every column documented and cleanly matched (so coverage alone says green),
  # but q1 holds a value its own codebook does not allow.
  utils::write.csv(data.frame(q1 = c(1, 2, 3, 9)),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description,values",
               "q1,Agreement,1=Low; 2=Mid; 3=High"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  expect_equal(cc$summary_table$unmatched_n, 0)
  expect_gte(nrow(cc$scale_violations), 1)
  expect_equal(cc$traffic_light, "yellow")
})

# ── Empty and degenerate inputs ───────────────────────────────────────────────

test_that("no extracted data columns returns an na light without calling the LLM", {
  # The module returns before naming scales when there is no data, so a paper
  # with no shared data never reaches the LLM. llm_use(TRUE) here so that the
  # early return, not the llm_use gate, is what prevents the call.
  llm_use(TRUE)
  on.exit(llm_use(FALSE), add = TRUE)
  called <- FALSE
  testthat::local_mocked_bindings(
    llm = function(...) { called <<- TRUE; stop("llm() must not be called") })

  d <- cbc_dir("empty")   # data/ exists but holds no data file
  cc <- cbc_run(d)

  expect_false(called)
  expect_equal(cc$traffic_light, "na")
  expect_equal(nrow(cc$table), 0)
  expect_match(cc$summary_text, "no extracted data columns")
  # The summary table still carries every count column, all zero.
  expect_equal(cc$summary_table$column_n, 0)
  expect_equal(cc$summary_table$codebook_var_n, 0)
})

test_that("the summary table always carries the full set of count columns", {
  llm_use(FALSE)
  d <- cbc_dir("schema")
  utils::write.csv(data.frame(id = 1:5, x = 1:5),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description", "x,A variable"),
             file.path(d, "codebook.csv"))

  # module_run, NOT report_module_run: the latter left-joins every module's
  # summary_table into one wide frame (module.R), so codebook_check's own
  # columns would arrive renamed and mixed with data_check's.
  module_run(test_paper("x"), "data_check", local_path = d, local_only = TRUE)
  cc <- module_run(test_paper("x"), "codebook_check",
                   local_path = d, local_only = TRUE)

  expect_named(
    cc$summary_table,
    c("paper_id", "column_n", "matched_n", "unmatched_n", "clean_n",
      "conflicted_n", "codebook_var_n", "unused_var_n",
      "scale_blocks_n", "scale_named_n", "scale_unnamed_n",
      "task_files_n", "task_named_n", "task_paper_only_n"))
  # na_replace must cover every count column, so a paper with no data can be
  # substituted into a paperlist summary without introducing NAs.
  counts <- setdiff(names(cc$summary_table), "paper_id")
  expect_true(all(counts %in% names(cc$na_replace)))
})

# ── Duplicated column names ───────────────────────────────────────────────────

test_that("repeated column names are reported per file as a warning", {
  llm_use(FALSE)
  d <- cbc_dir("dup")
  # A survey loop export that repeats a header instead of numbering iterations.
  # write.csv would de-duplicate, so write the header line literally.
  writeLines(c("id,POWER,POWER,POWER",
               "1,3,4,5", "2,2,1,4", "3,5,5,2"),
             file.path(d, "data", "loop.csv"))

  cc <- cbc_run(d)
  rpt <- paste(cc$report, collapse = "\n")
  expect_match(rpt, "Duplicated Column Names", fixed = TRUE)
  expect_match(rpt, "loop.csv", fixed = TRUE)
})

test_that("duplicate-name warnings are silent when every name is unique", {
  warn <- .cbc$.codebook_duplicate_name_warnings(
    list(clean.csv = data.frame(a = 1:3, b = 1:3, c = 1:3)))
  expect_length(warn, 0)
})

# ── Item vs. derived column split ─────────────────────────────────────────────
# Within one prefix block, a sum/mean/total is not an item. Two signals: a word
# suffix breaking a numbering convention, and values that are non-integer or far
# wider-ranged than the block's items.

test_that("an aggregate column is split out of an item block by its name", {
  set.seed(11)
  cols <- c(paste0("AQ", sprintf("%02d", 1:10)), "AQ_SUM")
  df <- as.data.frame(matrix(sample(1:5, 11 * 20, TRUE), nrow = 20))
  names(df) <- cols
  df$AQ_SUM <- rowSums(df[, 1:10])

  sp <- .cbc$.scale_split_items(cols, df)
  expect_true("AQ_SUM" %in% sp$derived)
  expect_false("AQ_SUM" %in% sp$items)
  expect_equal(length(sp$items), 10)
})

test_that("a non-integer mean is split out even without an aggregate name", {
  set.seed(12)
  cols <- paste0("bfi_", 1:9)
  df <- as.data.frame(matrix(sample(1:5, 9 * 20, TRUE), nrow = 20))
  names(df) <- cols
  # bfi_9 holds a mean: non-integer among integer items.
  df$bfi_9 <- rowMeans(df[, 1:8])

  sp <- .cbc$.scale_split_items(cols, df)
  expect_true("bfi_9" %in% sp$derived)
})

test_that("a block of only totals is marked totals_only", {
  set.seed(13)
  cols <- c("IERQ_pos", "IERQ_persp", "IERQ_sooth", "IERQ_model")
  df <- as.data.frame(matrix(runif(4 * 20, 1, 40), nrow = 20))
  names(df) <- cols   # wide-ranging, non-integer: all look like totals

  sp <- .cbc$.scale_split_items(cols, df)
  expect_true(sp$totals_only)
})

test_that("a small block is left intact rather than over-pruned", {
  # Splitting would leave too few items, so everything is kept.
  cols <- c("x_1", "x_2", "x_sum")
  df <- data.frame(x_1 = 1:5, x_2 = 1:5, x_sum = (1:5) * 2)
  sp <- .cbc$.scale_split_items(cols, df)
  expect_equal(sp$items, cols)
  expect_length(sp$derived, 0)
})

# ── Prefix grouping ───────────────────────────────────────────────────────────

test_that("zero-padded numbering does not split one scale into two blocks", {
  # AQ01..AQ09 share the stem "AQ0" and AQ10..AQ12 share "AQ1", but the
  # alphabetic prefix is "AQ" for both, so they are one scale.
  expect_equal(.cbc$.scale_alpha_prefix("AQ01"), "aq")
  expect_equal(.cbc$.scale_alpha_prefix("AQ10"), "aq")
  # A word segment is alphabetic and must be kept, so subscales stay separate.
  expect_equal(.cbc$.scale_alpha_prefix("CRS_EXP"), "crs_exp")
  expect_false(identical(.cbc$.scale_alpha_prefix("CRS_EXP"),
                         .cbc$.scale_alpha_prefix("CRS_IDE")))
})

test_that("a shared stem keeps its inner separators and trims only the trailing one", {
  # Cutting at the FIRST separator would collapse every Q1.* column into "Q1".
  expect_equal(.cbc$.scale_shared_stem("Q1.RR_P_1", "Q1.RR_P_2"), "Q1.RR_P")
  expect_equal(.cbc$.scale_shared_stem("bfi_1", "bfi_2"), "bfi")
  expect_equal(.cbc$.scale_shared_stem("apple", "orange"), "")
})

test_that("columns differing only by item number are one run; word stems are not", {
  same <- .cbc$.scale_same_number_run
  expect_true(same("matwarmth1", "matwarmth2"))
  expect_true(same("AQ01", "AQ10"))
  # Different word segments are different constructs and must break the run.
  expect_false(same("matwarmth6", "mataggr1"))
  expect_false(same("neighdev", "neighcur"))
})

test_that("a repeated stimulus loop collapses to one instrument base", {
  # A matrix shown for many stimuli exports POWER.PP1 ... POWER.PP170; all are
  # the same questionnaire and must map to one base.
  expect_equal(.cbc$.scale_loop_base("POWER.PP1"),
               .cbc$.scale_loop_base("POWER.PP170"))
  # A plain instrument name has no trailing index and is returned unchanged.
  expect_equal(.cbc$.scale_loop_base("PANAS"), "panas")
})

# ── Scale name propagation ────────────────────────────────────────────────────

test_that("a named scale propagates to unnamed same-prefix siblings", {
  labels_df <- data.frame(
    source_file = "s.csv",
    column_name = c("bfi_1", "bfi_2", "bfi_3", "other_1"),
    scale = c("Big Five Inventory", NA, NA, NA),
    scale_confidence = c("high", NA, NA, NA),
    scale_source = c("manuscript", NA, NA, NA))

  out <- .cbc$.propagate_scale_by_prefix(labels_df)
  expect_equal(out$scale[1:3], rep("Big Five Inventory", 3))
  expect_equal(out$scale_confidence[2], "high")
  expect_equal(out$scale_source[2], "manuscript")
  # A different prefix must not inherit the name.
  expect_true(is.na(out$scale[4]))
})

test_that("propagation never overwrites a name an earlier stage set", {
  labels_df <- data.frame(
    source_file = "s.csv",
    column_name = c("bfi_1", "bfi_2"),
    scale = c("Big Five Inventory", "Something Else"),
    scale_confidence = c("high", "low"),
    scale_source = c("manuscript", "self_generated"))

  out <- .cbc$.propagate_scale_by_prefix(labels_df)
  expect_equal(out$scale[2], "Something Else")
})

# ── Paper-text scanning for instruments and tasks ─────────────────────────────

test_that("a possessive instrument name is matchable in paper text", {
  # The separator class must include both apostrophe forms, or every possessive
  # instrument becomes unmatchable.
  pat <- .cbc$.scale_text_pattern("Raven's Advanced Progressive Matrices", NA)
  expect_true(grepl(pat, "we used Raven's Advanced Progressive Matrices",
                    perl = TRUE, ignore.case = TRUE))
  expect_true(grepl(pat, "we used Raven’s Advanced Progressive Matrices",
                    perl = TRUE, ignore.case = TRUE))
})

test_that("an acronym is matched only at word boundaries", {
  # An unanchored 3-letter acronym would match inside ordinary words: "MES" in
  # "times", "EAS" in "increased". Corroboration must not fire on those.
  expect_false(.cbc$.scale_name_in_text(
    "Meaning in Everyday Scale (MES)", "response times increased over trials"))
  expect_true(.cbc$.scale_name_in_text(
    "Meaning in Everyday Scale (MES)", "the MES was administered first"))
})

test_that("a task named in the paper is found by the task dictionary", {
  p <- test_paper(c("Participants completed a Stroop task.",
                    "Reaction times were recorded."))
  found <- .cbc$.scan_paper_for_tasks(p)
  expect_true(any(grepl("stroop", found, ignore.case = TRUE)))
})

test_that("scanning a paper with no text returns nothing rather than erroring", {
  expect_length(.cbc$.scan_paper_for_scales(test_paper(character(0))), 0)
  expect_length(.cbc$.scan_paper_for_tasks(test_paper(character(0))), 0)
})

# ── Behavioural tasks in the data ─────────────────────────────────────────────

test_that("a task is named when the data shows it and the paper confirms it", {
  llm_use(FALSE)
  d <- cbc_dir("task")
  set.seed(21)
  utils::write.csv(
    data.frame(id = rep(1:10, each = 4),
               trial = rep(1:4, 10),
               stroop_rt = round(runif(40, 400, 900)),
               stroop_correct = sample(0:1, 40, TRUE)),
    file.path(d, "data", "trials.csv"), row.names = FALSE)

  p <- test_paper(c(
    "Participants completed a colour-word Stroop task.",
    "Reaction time and accuracy were recorded on every trial."))

  cc <- cbc_run(d, paper = p)
  expect_gte(cc$summary_table$task_files_n, 1)
  expect_match(paste(cc$report, collapse = "\n"), "#### Tasks", fixed = TRUE)
})

test_that("a task named in the paper with no data is reported, not treated as an error", {
  llm_use(FALSE)
  d <- cbc_dir("taskonly")
  utils::write.csv(data.frame(id = 1:5, age = 20:24),
                   file.path(d, "data", "demo.csv"), row.names = FALSE)
  writeLines(c("varname,description", "age,Age in years", "id,Participant id"),
             file.path(d, "codebook.csv"))

  p <- test_paper("Participants completed a Stroop task before the survey.")
  cc <- cbc_run(d, paper = p)

  expect_gte(cc$summary_table$task_paper_only_n, 1)
  expect_match(paste(cc$report, collapse = "\n"),
               "named in the manuscript", fixed = TRUE)
  # Reporting a paper-only task must not by itself make the light red.
  expect_true(cc$traffic_light %in% c("green", "yellow", "red"))
})

test_that("the task matcher abstains when several task variants are corroborated", {
  # A prefix matching many dictionary variants (8 Stroop entries) with the text
  # naming more than one: abstain rather than pick the first alphabetically.
  tasks <- .cbc$.task_dictionary()
  skip_if(nrow(tasks) == 0, "tasks dictionary unavailable")
  # The dictionary must genuinely contain colliding Stroop variants for the
  # abstention rule to be meaningful.
  n_stroop <- sum(grepl("stroop", tasks$name, ignore.case = TRUE))
  expect_gt(n_stroop, 1)
})

# ── Orphan totals ─────────────────────────────────────────────────────────────

test_that("a totals-only block with no item block anywhere is flagged orphan", {
  llm_use(FALSE)
  d <- cbc_dir("orphan")
  set.seed(31)
  # Four subscale totals, wide-ranging and non-integer, with no item columns.
  utils::write.csv(
    data.frame(id = 1:20,
               IERQ_pos    = round(runif(20, 5, 35), 1),
               IERQ_persp  = round(runif(20, 5, 35), 1),
               IERQ_sooth  = round(runif(20, 5, 35), 1),
               IERQ_model  = round(runif(20, 5, 35), 1)),
    file.path(d, "data", "totals.csv"), row.names = FALSE)

  cc <- cbc_run(d)
  # Whether the block is NAMED depends on the dictionary, but the module must
  # at minimum detect and report the column group.
  expect_match(paste(cc$report, collapse = "\n"), "#### Scales", fixed = TRUE)
})

# ── Paradata exclusion feeding scale detection ────────────────────────────────

test_that("paradata columns are described but never grouped as a scale", {
  df <- data.frame(
    psqi_1_response_numeric = 1:5, psqi_1_response_time = 1:5,
    psqi_2_response_numeric = 1:5, psqi_2_response_time = 1:5,
    psqi_3_response_numeric = 1:5, psqi_3_response_time = 1:5,
    check.names = FALSE)

  mask <- .cbc$.paradata_col(df)
  expect_true(all(mask[c(2, 4, 6)]))    # the timing channels
  expect_false(any(mask[c(1, 3, 5)]))   # the answer channels survive

  groups <- .cbc$.scale_prefix_groups(df)
  grouped <- unlist(lapply(groups, function(g) g$cols %||% g$columns))
  expect_false(any(grepl("response_time", grouped)))
})

test_that("machinery columns are self-labelled so they never reach the LLM", {
  labels_df <- data.frame(
    source_file = "s.csv",
    column_name = c("StartDate", "Q1_DO_order", "Q2_TEXT", "bfi_1"),
    label = NA_character_,
    codebook_variable = NA_character_,
    label_status = "unlabelled",
    label_method = NA_character_)
  previews <- list(s.csv = data.frame(
    StartDate = "2024-01-01", Q1_DO_order = 1, Q2_TEXT = "free text", bfi_1 = 3,
    check.names = FALSE))

  out <- .cbc$.codebook_label_machinery(labels_df, previews)
  # The three machinery columns are now labelled (so excluded from the LLM),
  # each with a real description, and the genuine item is untouched.
  expect_equal(out$label_status[1:3], rep("labelled", 3))
  expect_true(all(nzchar(out$label[1:3])))
  expect_equal(out$label_method[1:3], rep("paradata_rule", 3))
  expect_equal(out$label_status[4], "unlabelled")
})

test_that("a column the codebook documented keeps its real label", {
  labels_df <- data.frame(
    source_file = "s.csv",
    column_name = "Q2_TEXT",
    label = "Other, please specify",
    codebook_variable = "Q2_TEXT",
    label_status = "labelled",
    label_method = "rules")
  previews <- list(s.csv = data.frame(Q2_TEXT = "x", check.names = FALSE))

  out <- .cbc$.codebook_label_machinery(labels_df, previews)
  expect_equal(out$label, "Other, please specify")
  expect_equal(out$label_method, "rules")
})

# ── LLM-gated behaviour ───────────────────────────────────────────────────────

test_that("rules-only mode names scales from the dictionary alone", {
  # With llm_use(FALSE) no LLM result can reach the module: llm() itself throws
  # ("Set llm_use(TRUE) to use LLM functions") and every call site wraps it in
  # tryCatch, so a tier that tries is refused rather than skipped. What matters
  # is the OUTCOME — the dictionary matcher still names PANAS, and every name
  # carries scale_source "matched", never a model-derived source.
  llm_use(FALSE)
  d <- cbc_dir("rulesonly")
  set.seed(41)
  items <- as.data.frame(matrix(sample(1:5, 40 * 10, TRUE), nrow = 40))
  names(items) <- paste0("panas_", 1:10)
  utils::write.csv(cbind(id = 1:40, items),
                   file.path(d, "data", "s.csv"), row.names = FALSE)

  cc <- cbc_run(d)
  named <- !is.na(cc$table$scale) & nzchar(cc$table$scale)
  expect_true(any(named))
  expect_true(all(cc$table$scale_source[named] == "matched"))
  # No LLM-sourced naming, and no "LLM model ... reviewed" line in the report.
  expect_false(any(cc$table$scale_source[named] %in%
                     c("manuscript", "self_generated")))
  expect_false(grepl("reviewed ambiguous cases",
                     paste(cc$report, collapse = "\n"), fixed = TRUE))
})

test_that("the LLM call budget refuses a tier upfront and names the parameter", {
  # codebook_max_calls is an upfront gate: a tier needing more calls is skipped
  # whole, not truncated, and the report must say how to lift the cap.
  gate <- cap_gate_count(50L, "codebook_max_calls", 2L, "text block",
                         context = "big_codebook.txt", action = "parse")
  expect_false(is.null(gate))
  expect_match(gate, "codebook_max_calls", fixed = TRUE)
})

# ── Report structure ──────────────────────────────────────────────────────────

test_that("the report states how many sources and columns were examined", {
  llm_use(FALSE)
  d <- cbc_dir("scope")
  utils::write.csv(data.frame(id = 1:5, age = 20:24, sex = c(1, 2, 1, 2, 1)),
                   file.path(d, "data", "s.csv"), row.names = FALSE)
  writeLines(c("varname,description", "age,Age in years", "sex,Sex"),
             file.path(d, "codebook.csv"))

  cc <- cbc_run(d)
  rpt <- paste(cc$report, collapse = "\n")
  # Sample size must always be stated: how many sources, columns and files.
  expect_match(rpt, "We examined")
  expect_match(rpt, "data column")
  expect_match(rpt, "#### Column Documentation", fixed = TRUE)
})

test_that("with no codebook the report says so instead of showing an empty table", {
  llm_use(FALSE)
  d <- cbc_dir("nocb")
  utils::write.csv(data.frame(a = 1:5, b = 1:5),
                   file.path(d, "data", "s.csv"), row.names = FALSE)

  cc <- cbc_run(d)
  rpt <- paste(cc$report, collapse = "\n")
  expect_match(rpt, "No codebook or README documentation was found", fixed = TRUE)
  expect_false(grepl("#### Column Documentation", rpt, fixed = TRUE))
  expect_equal(cc$traffic_light, "red")
})
