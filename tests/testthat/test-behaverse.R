# Tests for the Behaverse trial-level paradata framework: the native validator
# (behaverse_validate), the source-format detectors, the per-format readers /
# convert_behaverse, and the paradata column routing out of scale grouping. All
# run offline and deterministically from small hand-built fixtures; no network,
# no LLM. See R/behaverse-validate.R and R/behaverse-convert.R.

# A minimal valid TrialData document: one Instrument row + one Response row with
# all 13 required Response fields.
make_trialdata <- function() {
  list(
    Instrument = list(list(instrument_id = "psqi", timeline_id = "psqi",
                           block_id = "psqi", name = "PSQI")),
    Response = list(list(
      response_id = "1", study_name = "s", agent_id = "001", session_id = 1L,
      instrument_id = "psqi", multitask_type = "single_task", block_index = 1L,
      block_type = "questionnaire", transformation_name = "identity",
      trial_index = "1", trial_start_datetime = "1970-01-01T00:00:00Z",
      stimulus_id = 1L, stimulus_type = "question", response_numeric = 3,
      response_time = 5.2)))
}

# ── behaverse_validate: accept / reject ───────────────────────────────────────

test_that("behaverse_validate accepts a minimal valid TrialData document", {
  res <- behaverse_validate(make_trialdata())
  expect_s3_class(res, "behaverse_validation")
  expect_true(res$valid)
  expect_equal(res$summary$n_errors, 0)
})

test_that("behaverse_validate rejects a missing required field", {
  doc <- make_trialdata()
  doc$Response[[1]]$agent_id <- NULL
  res <- behaverse_validate(doc)
  expect_false(res$valid)
  expect_true(any(vapply(res$issues, function(i) i$code == "RequiredFieldMissing",
                         logical(1))))
})

test_that("behaverse_validate rejects a wrong scalar type", {
  doc <- make_trialdata()
  doc$Response[[1]]$session_id <- "one"       # required integer
  res <- behaverse_validate(doc)
  expect_false(res$valid)
  expect_true(any(vapply(res$issues, function(i) i$code == "FieldTypeMismatch",
                         logical(1))))
})

test_that("behaverse_validate rejects a table that is not an array", {
  doc <- make_trialdata()
  doc$Response <- doc$Response[[1]]           # object, not array of rows
  res <- behaverse_validate(doc)
  expect_false(res$valid)
})

test_that("behaverse_validate errors on nonsense input", {
  expect_error(behaverse_validate(bad_arg))
})

# ── source-format detectors ───────────────────────────────────────────────────

test_that("data_check_is_behaverse detects a native long table", {
  df <- data.frame(agent_id = "1", instrument_id = "psqi", trial_index = 1,
                   response_numeric = 3, response_time = 5.2)
  expect_true(data_check_is_behaverse(df))
  expect_false(data_check_is_behaverse(data.frame(a = 1, b = 2)))
})

test_that("data_check_is_behaverse detects a wide-pivot export", {
  df <- data.frame(`psqi_q_1_response_numeric_i1` = 3,
                   `psqi_q_1_response_time_i1` = 5.2, check.names = FALSE)
  expect_true(data_check_is_behaverse(df))
})

test_that("data_check_is_inquisit detects an iqdat table", {
  df <- data.frame(subject = 1, blocknum = 1, blockcode = "gen", trialcode = "t",
                   latency = 800, correct = 1)
  expect_true(data_check_is_inquisit(df))
  expect_false(data_check_is_inquisit(data.frame(x = 1, y = 2)))
})

test_that("data_check_is_jspsych detects a jsPsych table", {
  df <- data.frame(trial_type = "html", rt = 500, trial_index = 1, stimulus = "s")
  expect_true(data_check_is_jspsych(df))
  expect_false(data_check_is_jspsych(data.frame(trial_type = "x")))
})

# ── convert_behaverse: per-format readers ─────────────────────────────────────

test_that("convert_behaverse reads a native Behaverse long file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "psqi.csv")
  utils::write.csv(data.frame(
    agent_id = c("1", "1"), instrument_id = c("psqi", "psqi"),
    trial_index = c(1, 2), stimulus_type = c("question", "question"),
    response_numeric = c(3, 2), response_time = c(5.2, 4.1)),
    f, row.names = FALSE)
  docs <- convert_behaverse(f, study_name = "test")
  expect_true("psqi" %in% names(docs))
  expect_true(behaverse_validate(docs[["psqi"]])$valid)
  expect_equal(length(docs[["psqi"]]$Response), 2L)
})

test_that("convert_behaverse reads an Inquisit iqdat file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "gen.iqdat")
  writeLines(c(
    "subject\tblocknum\tblockcode\ttrialcode\tresponse\tcorrect\tlatency\tstimulusitem1",
    "49\t1\tgeneralization_1\tt1\tboo\t1\t845\tZeg boo"),
    f)
  docs <- convert_behaverse(f, study_name = "test")
  expect_true("generalization_1" %in% names(docs))
  doc <- docs[["generalization_1"]]
  expect_true(behaverse_validate(doc)$valid)
  expect_equal(doc$Response[[1]]$response_time, 845)
})

test_that("convert_behaverse reads a jsPsych file and maps rt", {
  d <- withr::local_tempdir()
  f <- file.path(d, "j1_stroop-data.csv")
  utils::write.csv(data.frame(
    trial_type = c("html", "html"), trial_index = c(0, 1),
    rt = c("", 500), response = c("", "left"), stimulus = c("+", "word")),
    f, row.names = FALSE)
  docs <- convert_behaverse(f, study_name = "test")
  expect_true(length(docs) >= 1L)
  expect_true(all(vapply(docs, function(x) behaverse_validate(x)$valid, logical(1))))
})

test_that("convert_behaverse returns empty for a non-trial-level file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "plain.csv")
  utils::write.csv(data.frame(id = 1:3, score = c(1, 2, 3)), f, row.names = FALSE)
  expect_length(convert_behaverse(f), 0L)
})

# ── E-Prime text parsing ──────────────────────────────────────────────────────

test_that("convert_behaverse reads an E-Prime text export", {
  # Real E-Prime export shape: a header block, then LogFrame-delimited trial
  # frames (`*** LogFrame Start ***` ... `*** LogFrame End ***`) whose nesting is
  # `Level: 2` in a Session/Block/Trial design. Field/timing names follow the
  # `<object>.RT` / `.ACC` / `.RESP` convention. Modelled on a verified 2008
  # export (PowerfulPowerlessRightLeft).
  d <- withr::local_tempdir()
  f <- file.path(d, "naming-1-1.txt")
  writeLines(c(
    "*** Header Start ***", "Experiment: naming", "Subject: 1", "Session: 1",
    "*** Header End ***",
    "\tLevel: 2", "\t*** LogFrame Start ***",
    "\tStimWord: CHIRURG", "\tRunning: BlockA", "\tProcedure: proc",
    "\tstim.OnsetTime: 1000", "\tstim.RTTime: 1500", "\tstim.RT: 500",
    "\tstim.ACC: 1", "\tstim.RESP: l", "\t*** LogFrame End ***"),
    f)
  docs <- convert_behaverse(f, study_name = "test")
  expect_true("naming" %in% names(docs))
  doc <- docs[["naming"]]
  expect_true(behaverse_validate(doc)$valid)
  expect_equal(doc$Response[[1]]$response_time, 500)
})

# ── text_peek + file classification ───────────────────────────────────────────

test_that("text_peek reads the first lines and tolerates a missing file", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("line one", "line two", "line three"), f)
  expect_equal(text_peek(f, n = 2), c("line one", "line two"))
  expect_length(text_peek(file.path(tempdir(), "no_such_file_xyz.txt")), 0L)
})

test_that("text_peek reads a UTF-16 file (the E-Prime encoding)", {
  f <- withr::local_tempfile(fileext = ".txt")
  con <- file(f, open = "wb", encoding = "UTF-16LE")
  writeLines(c("*** Header Start ***", "Experiment: naming"), con, useBytes = FALSE)
  close(con)
  peeked <- text_peek(f, n = 5)
  expect_true(any(grepl("Header Start", peeked)))
})

test_that("Inquisit .iqdat is downloadable data; binary E-Prime .edat/.edat2 are not", {
  # .iqdat is tab-delimited TEXT (readable), so it is downloadable research data.
  # .edat/.edat2 are proprietary BINARY (OLE compound documents) that metacheck
  # cannot parse, so they classify as "materials" and are never downloaded —
  # the analysable data comes from E-Prime's plain-.txt export instead.
  x <- c("gen_1_49.iqdat", "naming-264-1.edat2", "run.edat")
  expect_equal(unname(data_classify_files(x)), c("data", "materials", "materials"))
})

test_that("txt_classify_content recognises an E-Prime export as data", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("*** Header Start ***", "LevelName: Session", "Experiment: naming",
               "Subject: 1", "*** Header End ***"), f)
  expect_equal(txt_classify_content(f), "data")
})

test_that("txt_classify_content recognises a delimited table as data", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("id\tcond\trt", "1\ta\t500", "2\tb\t600"), f)
  expect_equal(txt_classify_content(f), "data")
})

test_that("txt_classify_content leaves prose undecided", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("These are study notes.",
               "Participants were recruited in 2019, and paid."), f)
  expect_true(is.na(txt_classify_content(f)))
})

test_that("txt_classify_content is undecided for an unreadable/missing file", {
  expect_true(is.na(txt_classify_content(file.path(tempdir(), "nope_xyz.txt"))))
})

test_that(".eprime_is_export detects an export by content, not extension", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("*** Header Start ***", "Experiment: naming", "*** Header End ***"), f)
  expect_true(metacheck:::.eprime_is_export(f))
  g <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("just some notes", "about the study"), g)
  expect_false(metacheck:::.eprime_is_export(g))
})

# ── accumulation across per-participant files ─────────────────────────────────

test_that(".osd_write_paradata accumulates per-participant files into one file", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Two participants, same instrument, one file each — as Inquisit publishes.
  for (s in c("01", "02")) {
    writeLines(c(
      "subject\tblocknum\tblockcode\ttrialcode\tresponse\tcorrect\tlatency\ttrialnum",
      paste0(s, "\t1\tgeneralization_1\tt1\tboo\t1\t845\t1"),
      paste0(s, "\t1\tgeneralization_1\tt2\tkef\t0\t900\t2")),
      file.path(d, "data", paste0("gen_", s, ".iqdat")))
  }
  idx <- metacheck:::.osd_write_paradata(d, study_name = "test")
  # ONE instrument -> ONE file, not one per participant file, and no -2 suffix.
  expect_length(idx, 1L)
  expect_equal(idx[[1]]$instrument_id, "generalization_1")
  expect_equal(idx[[1]]$n_responses, 4L)          # 2 participants x 2 trials
  expect_true(file.exists(file.path(d, "paradata", "generalization_1.json")))
  expect_length(list.files(file.path(d, "paradata")), 1L)
  # Both agents survive in the accumulated document.
  doc <- jsonlite::fromJSON(file.path(d, "paradata", "generalization_1.json"),
                            simplifyVector = FALSE)
  expect_setequal(unique(vapply(doc$Response, function(r) r$agent_id, character(1))),
                  c("01", "02"))
})

test_that(".osd_write_paradata merges per-participant jsPsych files with no task column", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"), recursive = TRUE)
  # Three participants, each their own per-participant-hash-named jsPsych export
  # (no `task` column) running the identical timeline — as many jsPsych online
  # studies publish. Without a shared key, these would become 3 instruments.
  for (s in c("aaa111", "bbb222", "ccc333")) {
    utils::write.csv(
      data.frame(trial_type = c("preload", "instructions", "html-keyboard-response"),
                rt = c(NA, NA, 452), trial_index = 0:2,
                participant_id = s),
      file.path(d, "data", paste0("source_", s, ".csv")), row.names = FALSE)
  }
  idx <- metacheck:::.osd_write_paradata(d, study_name = "test")
  # ONE instrument -> ONE file, not one per participant file.
  expect_length(idx, 1L)
  expect_equal(idx[[1]]$n_responses, 9L)          # 3 participants x 3 trials
  expect_length(list.files(file.path(d, "paradata")), 1L)
  doc <- jsonlite::fromJSON(list.files(file.path(d, "paradata"), full.names = TRUE),
                            simplifyVector = FALSE)
  expect_setequal(unique(vapply(doc$Response, function(r) r$agent_id, character(1))),
                  c("aaa111", "bbb222", "ccc333"))
  # Row-level instrument_id matches the Instrument block / filename key exactly
  # (no raw-vs-canonicalized mismatch).
  expect_true(all(vapply(doc$Response, function(r) r$instrument_id, character(1)) ==
                    doc$Instrument[[1]]$instrument_id))
})

test_that(".osd_write_paradata keeps genuinely different jsPsych timelines separate", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "data"), recursive = TRUE)
  utils::write.csv(
    data.frame(trial_type = c("preload", "html-keyboard-response"),
              rt = c(NA, 400), trial_index = 0:1, participant_id = "p1"),
    file.path(d, "data", "source_p1.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(trial_type = c("survey-text", "survey-text"),
              rt = c(3000, 2500), trial_index = 0:1, participant_id = "p2"),
    file.path(d, "data", "source_p2.csv"), row.names = FALSE)
  idx <- metacheck:::.osd_write_paradata(d, study_name = "test")
  expect_length(idx, 2L)
})

# ── trial-level file detection (held out of the tabular extractor) ────────────

test_that(".bh_is_trial_level_file recognises trial-level formats by header", {
  d <- withr::local_tempdir()
  # Inquisit
  iq <- file.path(d, "gen.iqdat")
  writeLines(c("subject\tblocknum\tblockcode\ttrialcode\tlatency",
               "1\t1\tgen\tt1\t800"), iq)
  # jsPsych
  js <- file.path(d, "stroop-data.csv")
  utils::write.csv(data.frame(trial_type = "html", rt = 500, trial_index = 1), js,
                   row.names = FALSE)
  # E-Prime
  ep <- file.path(d, "naming.txt")
  writeLines(c("*** Header Start ***", "Experiment: naming", "*** Header End ***",
               "\t\tLevel: 3", "\t\tstim.RT: 500"), ep)
  expect_true(metacheck:::.bh_is_trial_level_file(iq))
  expect_true(metacheck:::.bh_is_trial_level_file(js))
  expect_true(metacheck:::.bh_is_trial_level_file(ep))
})

test_that(".bh_is_trial_level_file leaves an ordinary table alone", {
  d <- withr::local_tempdir()
  f <- file.path(d, "scores.csv")
  utils::write.csv(data.frame(id = 1:3, panas_1 = 1:3, panas_2 = 3:1), f,
                   row.names = FALSE)
  expect_false(metacheck:::.bh_is_trial_level_file(f))
})

# ── paradata column routing out of scale grouping ─────────────────────────────
# .scale_is_paradata_col / .scale_prefix_groups live in the sourced module, not
# the package namespace, so source the module into an env to test them directly.
.cb_env_bh <- new.env()
sys.source(metacheck:::module_find("codebook_check"), envir = .cb_env_bh)

test_that(".scale_is_paradata_col flags channels but not the answer channel", {
  is_para <- .cb_env_bh$.scale_is_paradata_col
  expect_true(is_para("psqi_q_1_response_time_i1"))
  expect_true(is_para("psqi_q_1_trial_index_i1"))
  expect_true(is_para("EB T1 timing_First Click"))
  expect_false(is_para("psqi_q_1_response_numeric_i1"))
  expect_false(is_para("bfi_1"))
})

test_that(".scale_prefix_groups drops paradata channels before grouping", {
  # A wide-pivot block: three response_numeric item columns (real) interleaved
  # with response_time paradata. Only the answer channel should group.
  df <- data.frame(
    psqi_q_1_response_numeric_i1 = 1:3, psqi_q_1_response_time_i1 = 1:3,
    psqi_q_2_response_numeric_i1 = 1:3, psqi_q_2_response_time_i1 = 1:3,
    psqi_q_3_response_numeric_i1 = 1:3, psqi_q_3_response_time_i1 = 1:3,
    check.names = FALSE)
  groups <- .cb_env_bh$.scale_prefix_groups(df)
  grouped_cols <- unlist(lapply(groups, function(g) g$cols %||% g$columns))
  expect_false(any(grepl("response_time", grouped_cols)))
})
