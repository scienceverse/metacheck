# Convert trial-level behavioural data into Behaverse `trial` (TrialData) form.
#
# Many raw data formats record, alongside each questionnaire/task response, a
# family of PARADATA channels: response times, trial/stimulus indices, option
# metadata. These are not scale items — grouping them as scales produces junk
# "scales" (a "response time" block, a "trial index" block) and splits real
# instruments. Rather than discard that information, metacheck normalises it onto
# ONE target: the Behaverse Data Model `trial` schema (pinned v26.0608), which is
# a source-agnostic, tidy, trial-level standard. See inst/schema/.
#
# The design is a set of thin per-format READERS that each map a source file's
# columns onto the Behaverse Response vocabulary, feeding one shared BUILDER that
# assembles + validates a TrialData document. Supported source formats:
#
#   * behaverse — native tidy long form (columns already ARE Response fields)
#   * inquisit  — Millisecond Inquisit .iqdat (subject/blockcode/latency/...)
#   * eprime    — E-Prime text export (Subject/Level: 3 frames/<obj>.RT/.RTTime)
#   * jspsych   — jsPsych JSON/CSV (trial_type/rt/stimulus/response)
#   * qualtrics — Qualtrics page timing (First Click/Page Submit/... ; see the
#                 Qualtrics helpers in data_check_helpers.R for detection)
#
# Fidelity is not identical across formats and is recorded per document:
#   * inquisit/eprime/jspsych are natively trial-level -> near loss-free.
#   * eprime response time falls back to <obj>.RTTime - <obj>.OnsetTime when a
#     direct <obj>.RT column is absent (both are absolute-clock fields).
#   * qualtrics timing is PAGE-level, coarser than a per-response time; the
#     original metric name is preserved under additional_measures.

# ── numeric coercion helpers ──────────────────────────────────────────────────
# Parse to numeric, returning NULL (not NA) for blank / "NA" / unparseable, so a
# nullable Behaverse field is emitted as JSON null rather than a bad value.
.bh_num <- function(x) {
  # A real NA (a blank cell from read.csv, or an absent column) must be caught
  # BEFORE nzchar(), which returns NA for NA_character_ and would error in `if`.
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NULL)
  x <- trimws(as.character(x))
  if (!nzchar(x) || toupper(x) == "NA") return(NULL)
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) NULL else v
}
# As .bh_num but coerced to integer when whole; NULL otherwise.
.bh_int <- function(x) {
  v <- .bh_num(x)
  if (is.null(v)) return(NULL)
  if (v == round(v)) as.integer(v) else v
}
# Non-empty string or NULL (never NA, never "").
.bh_str <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(NULL)
  x
}
# TRUE/FALSE from a 0/1 or true/false cell, else NULL.
.bh_bool <- function(x) {
  x <- .bh_str(x)
  if (is.null(x)) return(NULL)
  switch(tolower(x), `1` = TRUE, `0` = FALSE, `true` = TRUE, `false` = FALSE,
         NULL)
}

# Normalise a raw instrument identifier to the canonical join key used to link
# the paradata file to its OSD (scales/<id>.osd <-> paradata/<id>.json). Lower-
# cased, a leading "x_" dropped (Behaverse exports the BFI as both bfi-2 and
# x_bfi2), non-alphanumerics collapsed to underscores.
.bh_instrument_key <- function(x) {
  # NA is not caught by %||% (which only guards NULL), so map an absent/NA id to
  # a stable placeholder rather than letting NA_character_ reach a file name.
  x <- as.character(x %||% "")
  if (length(x) != 1L || is.na(x)) x <- ""
  x <- tolower(trimws(x))
  if (!nzchar(x)) return("unknown")
  x <- sub("^x_", "", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  sub("^_|_$", "", x)
}

# ── TrialData builder ─────────────────────────────────────────────────────────
# Assemble ONE Behaverse Response row from a mapping the reader supplies. `map`
# is a named list of the fields the reader could resolve; required fields not
# supplied fall back to safe, honest defaults (a questionnaire administered as a
# single block/session), matching what a native Behaverse questionnaire export
# carries. `rid` is the 1-based row index within this instrument.
.bh_response_row <- function(map, rid) {
  # Value-or-default for a REQUIRED field. `%||%` alone is not enough: it catches
  # NULL but not NA, and a reader handed a missing source column supplies NA — an
  # NA in a required field fails schema validation. Treat NULL/NA/"" as absent.
  req <- function(value, default) {
    if (is.null(value) || length(value) != 1L || is.na(value)) return(default)
    if (is.character(value) && !nzchar(trimws(value))) return(default)
    value
  }
  # Required Behaverse Response fields (13), with defaults for what a source
  # format does not record. These describe a single-task questionnaire/test.
  base <- list(
    response_id          = as.character(req(map$response_id, rid)),
    study_name           = req(map$study_name, "unknown"),
    agent_id             = as.character(req(map$agent_id, "unknown")),
    session_id           = req(map$session_id, 1L),
    # Canonicalized (not the raw source value): every downstream consumer —
    # the Instrument block, the paradata filename, .osd_write_paradata's
    # cross-file grouping — keys on .bh_instrument_key(), so a row whose
    # instrument_id skipped that step would disagree with its own document
    # (e.g. raw "RedBlue_IAT1" on the row vs. canonical "redblue_iat1" on the
    # Instrument/filename). Canonicalizing once here, where every reader's
    # row passes through, replaces fixing each reader individually.
    instrument_id        = .bh_instrument_key(req(map$instrument_id, "unknown")),
    multitask_type       = req(map$multitask_type, "single_task"),
    block_index          = req(map$block_index, 1L),
    block_type           = req(map$block_type, "questionnaire"),
    transformation_name  = req(map$transformation_name, "identity"),
    trial_index          = as.character(req(map$trial_index, rid)),
    trial_start_datetime = req(map$trial_start_datetime, "1970-01-01T00:00:00Z"),
    stimulus_id          = req(map$stimulus_id, rid),
    stimulus_type        = req(map$stimulus_type, "stimulus"))
  # Optional paradata fields, included only when the reader resolved a value.
  optional <- list(
    stimulus_description     = map$stimulus_description,
    stimulus_onset           = map$stimulus_onset,
    response_option_index    = map$response_option_index,
    response_description     = map$response_description,
    response_numeric         = map$response_numeric,
    response_time            = map$response_time,
    response_initiation_time = map$response_initiation_time,
    response_validation_time = map$response_validation_time,
    input_count              = map$input_count,
    correct                  = map$correct,
    accuracy                 = map$accuracy,
    score                    = map$score,
    instrument_repetition    = map$instrument_repetition,
    additional_measures      = map$additional_measures)
  # Drop absent optionals. NA as well as NULL: the coercion helpers return NULL,
  # but a reader passing a raw cell through would otherwise emit a bare NA into a
  # nullable field. An omitted key is the correct encoding of "not recorded".
  absent <- function(v) is.null(v) || (length(v) == 1L && is.na(v))
  c(base, Filter(Negate(absent), optional))
}

# Build one Instrument row (the abstract per-instrument descriptor). `link` is a
# relative path to the paired OSD file, set on the ONLY outward-pointing field
# the Behaverse Instrument class permits (its additionalProperties is false).
.bh_instrument_row <- function(instrument_id, name = NULL, version = NULL,
                               link = NULL, description = NULL) {
  Filter(Negate(is.null), list(
    instrument_id = instrument_id,
    timeline_id   = instrument_id,
    block_id      = instrument_id,
    name          = name %||% instrument_id,
    version       = version,
    link          = link,
    description   = description))
}

# Assemble a full TrialData document from a list of Response rows for ONE
# instrument, plus its Instrument descriptor. `fidelity` is a short provenance
# note recorded under a namespaced key the schema ignores.
.bh_trialdata <- function(responses, instrument_id, name = NULL,
                          version = NULL, link = NULL, fidelity = NULL,
                          source_format = NULL, description = NULL) {
  doc <- list(
    Instrument = list(.bh_instrument_row(instrument_id, name, version, link,
                                         description)),
    Response   = responses)
  if (!is.null(source_format)) doc[["metacheck:sourceFormat"]] <- source_format
  if (!is.null(fidelity))      doc[["metacheck:fidelity"]] <- fidelity
  doc
}

# ── Reader: native Behaverse long/tidy ────────────────────────────────────────
# The columns already ARE Response fields. `df` is one instrument's long table
# (all rows share instrument_id). Column names taken from the Behaverse trial
# Response schema; anything absent is left to the builder's defaults.
.bh_read_behaverse <- function(df, study_name = "unknown") {
  g <- function(col) if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
  lapply(seq_len(nrow(df)), function(i) {
    .bh_response_row(list(
      study_name            = study_name,
      agent_id              = g("agent_id")[i],
      instrument_id         = g("instrument_id")[i],
      trial_index           = g("trial_index")[i],
      instrument_repetition = .bh_int(g("iteration")[i]),
      stimulus_type         = .bh_str(g("stimulus_type")[i]),
      stimulus_description  = .bh_str(g("stimulus_description")[i]),
      response_option_index = .bh_int(g("response_option_index")[i]),
      response_description  = .bh_str(g("response_description")[i]),
      response_numeric      = .bh_num(g("response_numeric")[i]),
      response_time         = .bh_num(g("response_time")[i]),
      response_validation_time = .bh_num(g("response_validation_time")[i])),
      rid = i)
  })
}

# ── Per-platform export-MACHINERY vocabulary ──────────────────────────────────
# The columns each trial-level platform ADDS that are export bookkeeping, not
# data a researcher analyses: browser/OS diagnostics, media-load flags, screen
# geometry, DOM node ids, per-trial pauses/timeouts, stimulus geometry. This is a
# DENYLIST, deliberately: a jsPsych/Inquisit export also carries the researcher's
# OWN custom columns (stim, resp1, correct_response, stimcolor, ...), which are
# often the most important data — an allowlist of "known platform columns" would
# wrongly drop those. So we enumerate only the KNOWN housekeeping and keep
# everything else. Kept here, in Behaverse, so the converter and the scale-
# detection filter (.scale_is_nonanalytic_col) share one definition; add a
# platform's machinery names once and both benefit. Matched case-insensitively.
.BH_MACHINERY_COLS <- list(
  # jsPsych core + common browser-check/plugin bookkeeping.
  jspsych  = c("success", "timeout", "failed_images", "failed_audio",
               "failed_video", "trial_index", "time_elapsed", "internal_node_id",
               "width", "height", "webaudio", "browser", "browser_version",
               "mobile", "os", "fullscreen", "vsync_rate", "webcam", "microphone",
               "view_history", "plugin_version"),
  # Inquisit reserved columns (the numbered stimulus geometry is added by RE).
  inquisit = c("date", "time", "build", "pretrialpause", "posttrialpause",
               "windowcenter", "trialduration", "trialtimeout", "blocktimeout",
               "inwindow"),
  # Native Behaverse paradata channels other than the substantive response.
  behaverse = c("stimulus_onset", "response_validation_time", "validation_time")
)

# Inquisit numbered stimulus channels: the ITEM is data (the shown content), the
# geometry (vertical/horizontal position, onset time, internal number) is
# machinery. Only the geometry families are matched here; stimulusitem<n> is kept.
.BH_INQUISIT_STIM_MACHINE_RE <- "^stimulus(number|vpos|hpos|onset)[0-9]+$"

# PsychoPy machinery is SUFFIX-based, not a fixed name list: component/loop names
# are study-specific (serialport_cue6_2.started, edloop.thisRepN), so the machine
# columns are recognised by their trailing role. Loop counters (.thisN/.thisIndex
# /.thisRepN/.thisTrialN/.ran) and component timing (.started/.stopped) are
# machinery; the RESPONSE channels (.rt/.keys/.corr/.response) are data and are
# NOT matched here, so they survive. Plus PsychoPy's fixed run-metadata columns.
.BH_PSYCHOPY_MACHINE_RE <- "[.](this(n|index|repn|trialn)|ran|started|stopped)$"
.BH_PSYCHOPY_META_COLS  <- c("psychopyversion", "framerate", "expname", "date",
                             "os", "session", "expstart")

# Given a trial-level file's column names and its detected `format` (one of
# jspsych/inquisit/behaverse/eprime), which columns are export MACHINERY? A
# column is machinery only when it is a KNOWN housekeeping name for that platform
# (or an Inquisit stimulus-geometry channel). Every other column — including the
# researcher's custom experimental columns — is kept. `response` and `stimulus`
# are never in the denylist. Returns a logical the length of `col_names`. Caller
# must have confirmed the platform (data_check_is_jspsych/inquisit/behaverse);
# for an unrecognised format nothing is flagged.
.bh_is_machinery_col <- function(col_names, format) {
  n <- length(col_names)
  if (n == 0 || is.null(format) || !nzchar(format)) return(rep(FALSE, n))
  deny <- .BH_MACHINERY_COLS[[format]]
  low  <- tolower(trimws(col_names))
  machinery <- if (is.null(deny)) rep(FALSE, n) else low %in% tolower(deny)
  if (identical(format, "inquisit"))
    machinery <- machinery | grepl(.BH_INQUISIT_STIM_MACHINE_RE, low, perl = TRUE)
  if (identical(format, "psychopy"))
    machinery <- machinery |
      grepl(.BH_PSYCHOPY_MACHINE_RE, low, perl = TRUE) |
      low %in% .BH_PSYCHOPY_META_COLS
  machinery
}

# ── Reader: Inquisit .iqdat ───────────────────────────────────────────────────
# Trial-level, one row per trial. subject/blocknum/blockcode/trialcode/response/
# correct/latency/stimulusitem1/stimulusonset1. blockcode identifies the
# instrument (task block). latency is the response time in ms.
.bh_read_inquisit <- function(df, study_name = "unknown") {
  g <- function(col) if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
  lapply(seq_len(nrow(df)), function(i) {
    .bh_response_row(list(
      study_name           = study_name,
      agent_id             = g("subject")[i],
      instrument_id        = g("blockcode")[i],
      block_index          = .bh_int(g("blocknum")[i]),
      block_type           = .bh_str(g("blockcode")[i]),
      transformation_name  = .bh_str(g("trialcode")[i]),
      trial_index          = g("trialnum")[i],
      stimulus_description = .bh_str(g("stimulusitem1")[i]),
      stimulus_onset       = .bh_num(g("stimulusonset1")[i]),
      response_description = .bh_str(g("response")[i]),
      response_time        = .bh_num(g("latency")[i]),
      correct              = .bh_bool(g("correct")[i])),
      rid = i)
  })
}

# ── Reader: jsPsych ───────────────────────────────────────────────────────────
# One row per trial. trial_type/trial_index/rt/response/stimulus/correct. rt is
# ms (empty on non-response screens). `instrument` names the task (from a `task`
# column when present, else supplied by the caller from the filename).
.bh_read_jspsych <- function(df, instrument = "task", study_name = "unknown") {
  g <- function(col) if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
  has_task <- "task" %in% names(df)
  lapply(seq_len(nrow(df)), function(i) {
    inst_raw <- if (has_task) (.bh_str(g("task")[i]) %||% instrument) else instrument
    inst <- .bh_instrument_key(inst_raw)
    .bh_response_row(list(
      study_name           = study_name,
      agent_id             = .bh_str(g("participant_id")[i]) %||%
                             .bh_str(g("subject")[i]) %||% "unknown",
      instrument_id        = inst,
      block_type           = .bh_str(g("trial_type")[i]),
      transformation_name  = .bh_str(g("trial_type")[i]),
      trial_index          = g("trial_index")[i],
      stimulus_description = .bh_str(g("stimulus")[i]),
      response_description = .bh_str(g("response")[i]),
      response_time        = .bh_num(g("rt")[i]),
      correct              = .bh_bool(g("correct")[i])),
      rid = i)
  })
}

# ── Reader: E-Prime text export ───────────────────────────────────────────────
# E-Prime exports are UTF-16 (or BOM) text: a header block of `Field: value`
# lines, then repeating `Level: 3` frames (one per Trial), each a run of
# `Field: value` lines. Object timing is `<obj>.RT` (ms) or, if absent,
# `<obj>.RTTime - <obj>.OnsetTime` (absolute clock). Parses one file to a list of
# per-trial named lists, plus the header (Subject, Experiment).
# Does this file's content look like an E-Prime export? E-Prime writes a fixed
# header block whose markers are unmistakable. Content-based, because E-Prime's
# .txt extension is far too ambiguous to classify on the name alone.
.eprime_is_export <- function(path) {
  head_lines <- text_peek(path, n = 30L)
  if (!length(head_lines)) return(FALSE)
  any(grepl("^\\*\\*\\*\\s*Header Start", head_lines)) ||
    (any(grepl("^(Experiment|Subject):", head_lines)) &&
       any(grepl("^LevelName:", head_lines)))
}

.bh_parse_eprime <- function(path) {
  # text_peek() handles the encodings E-Prime actually ships (UTF-16 with a BOM,
  # or 8-bit), so the whole file is read through the same tolerant path. n = Inf
  # because every trial frame is needed, not just the header.
  lines <- trimws(text_peek(path, n = Inf))
  kv <- function(l) {
    m <- regmatches(l, regexec("^([A-Za-z0-9_.]+):\\s*(.*)$", l))[[1]]
    if (length(m) == 3) stats::setNames(list(m[3]), m[2]) else NULL
  }
  header <- list(); i <- 1L
  while (i <= length(lines) && !grepl("Header End", lines[i])) {
    p <- kv(lines[i]); if (!is.null(p)) header[names(p)] <- p
    i <- i + 1L
  }
  # Trial frames are delimited by E-Prime's LogFrame markers
  # (`*** LogFrame Start ***` ... `*** LogFrame End ***`). The `Level: N` number
  # is only the frame's nesting depth and varies by experiment (a trial is
  # `Level: 2` in a Session/Block/Trial design), so the old `^Level:\s*3` rule
  # parsed real exports to ZERO trials. Each frame accumulates its `Field: value`
  # lines; a frame is kept only if it carries fields.
  trials <- list(); cur <- NULL
  for (l in lines) {
    if (grepl("\\*\\*\\*\\s*LogFrame Start", l)) {
      cur <- list()
    } else if (grepl("\\*\\*\\*\\s*LogFrame End", l)) {
      if (!is.null(cur) && length(cur)) trials[[length(trials) + 1L]] <- cur
      cur <- NULL
    } else if (!is.null(cur)) {
      p <- kv(l); if (!is.null(p)) cur[names(p)] <- p
    }
  }
  if (!is.null(cur) && length(cur)) trials[[length(trials) + 1L]] <- cur
  list(header = header, trials = trials)
}

# Find, within one parsed E-Prime trial frame, the first field ending in
# `suffix` (e.g. ".RT", ".ACC") regardless of the object prefix. NULL if none.
.bh_eprime_field <- function(trial, suffix) {
  hit <- names(trial)[endsWith(names(trial), suffix)]
  if (length(hit)) trial[[hit[1]]] else NULL
}

.bh_read_eprime <- function(parsed, study_name = "unknown") {
  header <- parsed$header; trials <- parsed$trials
  subject <- header[["Subject"]] %||% "unknown"
  experiment <- header[["Experiment"]] %||% "eprime"
  session <- .bh_int(header[["Session"]]) %||% 1L
  lapply(seq_along(trials), function(i) {
    t <- trials[[i]]
    rt <- .bh_num(.bh_eprime_field(t, ".RT"))
    if (is.null(rt)) {                                  # fallback: RTTime-OnsetTime
      rtt <- .bh_num(.bh_eprime_field(t, ".RTTime"))
      ons <- .bh_num(.bh_eprime_field(t, ".OnsetTime"))
      if (!is.null(rtt) && !is.null(ons)) rt <- rtt - ons
    }
    acc <- .bh_eprime_field(t, ".ACC")
    .bh_response_row(list(
      study_name           = study_name,
      agent_id             = subject,
      session_id           = session,
      instrument_id        = experiment,
      block_index          = .bh_int(t[["Block"]] %||% t[["BlockWB"]]),
      block_type           = .bh_str(t[["Running"]]),
      transformation_name  = .bh_str(t[["Procedure"]]),
      stimulus_type        = .bh_str(t[["stimulus_TYPE"]] %||% t[["trial_TYPE"]]),
      stimulus_description = .bh_str(t[["image"]] %||% t[["stimulus"]]),
      response_description = .bh_str(.bh_eprime_field(t, ".RESP")),
      response_time        = rt,
      correct              = .bh_bool(acc)),
      rid = i)
  })
}

# ── Reader: Qualtrics page timing ─────────────────────────────────────────────
# A Qualtrics "Timing" question emits, per page block, `<block>_First Click`,
# `_Last Click`, `_Page Submit`, `_Click Count`. Mapped onto Behaverse Response
# fields (page-level, coarser than a per-response time — recorded in fidelity):
#   First Click  -> response_initiation_time    Page Submit -> response_time
#   Last Click   -> response_validation_time     Click Count -> input_count
# The original metric names are preserved under additional_measures. `df` is the
# Qualtrics data frame; `block` is one timing block stem; returns Response rows
# (one per respondent row) for that block/instrument.
.bh_qualtrics_metric_map <- c(
  `First Click` = "response_initiation_time",
  `Page Submit` = "response_time",
  `Last Click`  = "response_validation_time",
  `Click Count` = "input_count")

.bh_read_qualtrics_timing <- function(df, block, study_name = "unknown") {
  cols <- stats::setNames(
    paste0(block, "_", names(.bh_qualtrics_metric_map)),
    unname(.bh_qualtrics_metric_map))
  cols <- cols[cols %in% names(df)]
  if (!length(cols)) return(list())
  prov <- jsonlite::toJSON(list(`qualtrics:source_metrics` = as.list(
    stats::setNames(names(.bh_qualtrics_metric_map)[
      match(names(cols), unname(.bh_qualtrics_metric_map))], names(cols)))),
    auto_unbox = TRUE)
  lapply(seq_len(nrow(df)), function(i) {
    vals <- lapply(cols, function(cn) {
      if (identical(names(cols)[match(cn, cols)], "input_count"))
        .bh_int(df[[cn]][i]) else .bh_num(df[[cn]][i])
    })
    .bh_response_row(c(list(
      study_name          = study_name,
      instrument_id       = block,
      stimulus_type       = "page",
      additional_measures = as.character(prov)),
      vals[!vapply(vals, is.null, logical(1))]),
      rid = i)
  })
}

# Is this file a trial-level format (Behaverse / Inquisit / E-Prime / jsPsych)?
# CHEAP: reads only the header (or the first lines for E-Prime), not the whole
# file, so screening hundreds of per-participant files is fast. Used by data_check
# to hold trial-level files OUT of the per-file tabular extractor and route them to
# the Behaverse accumulator instead — otherwise 200 per-participant E-Prime files
# would become 200 separate "datasets" rather than one merged instrument.
.bh_is_trial_level_file <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) return(FALSE)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("txt", "edat", "edat2") && .eprime_is_export(path)) return(TRUE)
  # Only DELIMITED-TEXT files can be a trial-level table, so only those are worth
  # a read.csv sniff. Reading a binary/document (.docx/.pdf/.sav/.xlsx/...) as CSV
  # produces "invalid input / embedded nulls" warnings and can return garbage, so
  # gate on the extension first. (E-Prime's own extensions were handled above.)
  # NOT ".log": a Stata plain-text log genuinely could be a trial-level table by
  # this extension alone, but no real corpus example has ever been found to
  # confirm a sniffer against, so .log is left out here and classed
  # "documentation" in .ext_registry (R/data_check_helpers.R) instead of
  # guessed at.
  .bh_sniff_exts <- c("csv", "tsv", "dat", "iqdat", "txt")
  if (!ext %in% .bh_sniff_exts) return(FALSE)
  # A one-row header read is enough for the data-frame detectors.
  hdr <- tryCatch(
    utils::read.csv(path, check.names = FALSE, nrows = 1L,
                    fileEncoding = "UTF-8-BOM",
                    sep = if (ext == "iqdat") "\t" else ","),
    error = function(e) NULL)
  if (is.null(hdr) || ncol(hdr) == 0) return(FALSE)
  data_check_is_behaverse(hdr) || data_check_is_inquisit(hdr) ||
    data_check_is_jspsych(hdr)
}

# ── File-level dispatch: one source file -> Response rows keyed by instrument ──
# Reads one data file, detects its trial-level format, and returns a named list
# mapping each instrument_id (canonical key) to its Response rows, plus the
# detected format and a fidelity note. Returns NULL when the file is not a
# recognised trial-level format (so a plain CSV / scale file is left alone).
.bh_read_file <- function(path, study_name = "unknown") {
  ext <- tolower(tools::file_ext(path))

  # E-Prime is text (header + Level:3 frames), not a flat table — parse directly.
  # Gated on CONTENT (.eprime_is_export), not the extension: .txt is far too
  # ambiguous to parse speculatively, and this avoids reading every .txt in full.
  if (ext %in% c("txt", "edat", "edat2") && .eprime_is_export(path)) {
    parsed <- tryCatch(.bh_parse_eprime(path), error = function(e) NULL)
    if (!is.null(parsed) && length(parsed$trials) > 0) {
      rows <- .bh_read_eprime(parsed, study_name)
      inst <- parsed$header[["Experiment"]] %||% "eprime"
      return(list(format = "eprime",
                  fidelity = "E-Prime text export; response_time from <obj>.RT or RTTime-OnsetTime.",
                  instruments = stats::setNames(list(rows), .bh_instrument_key(inst)),
                  names = stats::setNames(inst, .bh_instrument_key(inst))))
    }
  }

  # Everything else: read as a data frame and dispatch on the column vocabulary.
  # Identifier columns are read as CHARACTER so raw values are never altered: a
  # zero-padded participant id (01, 007) must not be parsed to a number (1, 7),
  # which would silently change the raw data and lose the id. read.csv would
  # otherwise auto-type them as integer. colClasses is set only for id columns
  # actually present in this file's header (naming an absent column errors), and
  # every other column still auto-types, so response times / numerics are numeric.
  sep <- if (ext == "iqdat") "\t" else ","
  hdr <- tryCatch(
    names(utils::read.csv(path, check.names = FALSE, nrows = 1L,
                          fileEncoding = "UTF-8-BOM", sep = sep)),
    error = function(e) character(0))
  id_cols <- c("subject", "agent_id", "participant_id", "instrument_id",
               "blockcode", "trialcode", "session_id")
  col_classes <- stats::setNames(
    rep("character", sum(hdr %in% id_cols)), hdr[hdr %in% id_cols])
  df <- tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8-BOM", sep = sep,
                    colClasses = if (length(col_classes)) col_classes else NA),
    error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(NULL)

  split_by <- function(rows, keyvec) {
    keys <- .bh_instrument_key_vec(keyvec)
    stats::setNames(lapply(unique(keys), function(k) rows[keys == k]),
                    unique(keys))
  }

  if (data_check_is_behaverse(df) && "instrument_id" %in% names(df)) {
    key <- df[["instrument_id"]]
    out <- lapply(split(seq_len(nrow(df)), .bh_instrument_key_vec(key)),
                  function(idx) .bh_read_behaverse(df[idx, , drop = FALSE], study_name))
    nm  <- tapply(as.character(key), .bh_instrument_key_vec(key), function(x) x[1])
    return(list(format = "behaverse",
                fidelity = "Native Behaverse tidy long form; Response channels mapped directly.",
                instruments = out, names = nm))
  }
  if (data_check_is_inquisit(df)) {
    key <- df[["blockcode"]]
    out <- lapply(split(seq_len(nrow(df)), .bh_instrument_key_vec(key)),
                  function(idx) .bh_read_inquisit(df[idx, , drop = FALSE], study_name))
    nm  <- tapply(as.character(key), .bh_instrument_key_vec(key), function(x) x[1])
    return(list(format = "inquisit",
                fidelity = "Inquisit .iqdat; latency mapped to response_time.",
                instruments = out, names = nm))
  }
  if (data_check_is_jspsych(df)) {
    has_task <- "task" %in% names(df)
    file_stem <- sub("[_.-]?data$", "", tools::file_path_sans_ext(basename(path)))
    # With no `task` column, a file's own name is the fallback instrument
    # label — but when raw filenames are per-participant (a hash, a subject
    # id), that fallback is unique per file and defeats cross-file merging
    # (see .bh_jspsych_fingerprint). Key on the timeline fingerprint instead
    # so participants who ran the same jsPsych script merge into one
    # instrument; the filename stem is kept only as the human-readable name.
    fp <- if (!has_task) .bh_jspsych_fingerprint(df) else NULL
    inst_raw <- if (has_task) df[["task"]] else file_stem
    rows <- .bh_read_jspsych(df, instrument = as.character(inst_raw)[1], study_name)
    if (!has_task && !is.null(fp)) {
      fp_key <- .bh_instrument_key(fp)
      for (r in seq_along(rows)) rows[[r]]$instrument_id <- fp_key
      out <- stats::setNames(list(rows), fp_key)
      nm  <- stats::setNames(file_stem, fp_key)
    } else {
      # jsPsych rows carry their own instrument_id from the reader; split on it.
      key <- vapply(rows, function(r) r$instrument_id, character(1))
      out <- split(rows, .bh_instrument_key_vec(key))
      nm  <- tapply(key, .bh_instrument_key_vec(key), function(x) x[1])
    }
    return(list(format = "jspsych",
                fidelity = "jsPsych; rt mapped to response_time (empty on non-response screens).",
                instruments = out, names = nm))
  }
  NULL
}

# Vectorised canonical instrument key, for splitting rows by instrument.
.bh_instrument_key_vec <- function(x)
  vapply(x, .bh_instrument_key, character(1), USE.NAMES = FALSE)

# A jsPsych export with no `task` column has no shared value to key an
# instrument on: the caller's only remaining fallback is that FILE's own name,
# which is unique per file whenever the raw filenames are per-participant (a
# hash, a subject id) rather than per-task — so 144 participant files running
# the identical timeline become 144 separate "instruments" instead of merging.
# The jsPsych timeline (its ordered sequence of trial_type plugin names) is a
# structural fingerprint of the TASK, not the participant: the same experiment
# script produces the same sequence of screens for every participant who ran
# it. Files whose fingerprint matches merge into one instrument; a file with a
# different timeline (a different task) keeps a different key. This can still
# wrongly merge two distinct tasks that happen to reuse an identical plugin
# sequence (e.g. two separate surveys both built only from
# html-keyboard-response + survey) — a real but rarer failure mode than the
# guaranteed one-per-file fragmentation it replaces.
.bh_jspsych_fingerprint <- function(df) {
  if (!"trial_type" %in% names(df)) return(NULL)
  types <- sort(unique(stats::na.omit(vapply(
    df[["trial_type"]], function(x) .bh_str(x) %||% NA_character_, character(1)))))
  if (!length(types)) return(NULL)
  paste(types, collapse = "|")
}

#' Convert a data frame or file to Behaverse `trial` (TrialData) documents
#'
#' Reads one trial-level data source (a native Behaverse table, Inquisit `.iqdat`,
#' E-Prime text export, or jsPsych file), maps its columns onto the Behaverse
#' Response vocabulary, and returns one validated `TrialData` document per
#' instrument found. Paradata (response times, trial/stimulus channels) is carried
#' faithfully; it is not discarded. This is the format-conversion core that
#' `convert_psychds()` calls to emit `paradata/<instrument_id>.json` alongside the
#' scale (OSD) files.
#'
#' @param path path to a trial-level data file.
#' @param study_name study/experiment name recorded on each Response row.
#'
#' @returns a named list of `TrialData` documents, keyed by canonical
#'   instrument_id; empty list when `path` is not a recognised trial-level format.
#' @export
#' @keywords internal
convert_behaverse <- function(path, study_name = "unknown") {
  res <- .bh_read_file(path, study_name)
  if (is.null(res)) return(list())
  lapply(stats::setNames(names(res$instruments), names(res$instruments)),
         function(key) .bh_trialdata(
           responses     = res$instruments[[key]],
           instrument_id = key,
           name          = unname(res$names[[key]]) %||% key,
           fidelity      = res$fidelity,
           source_format = res$format))
}

# Trial-level source files in a converted dataset's data/ folder. Shared by the
# pre-scan (.bh_paradata_keys) and the writer (.osd_write_paradata).
.bh_data_files <- function(output_dir) {
  data_dir <- file.path(output_dir, "data")
  if (!dir.exists(data_dir)) return(character(0))
  list.files(data_dir, full.names = TRUE, recursive = TRUE,
             pattern = "\\.(csv|tsv|txt|iqdat|edat2?)$", ignore.case = TRUE)
}

# Inquisit .iqx scripts anywhere in the converted output (definition files that
# describe the task producing the .iqdat data). Read once, keyed by the canonical
# instrument key of the script's filename stem, so an instrument's paradata can be
# enriched with the real task name/description (Feed B) — .iqdat output is named
# <script-stem>_<subject>_<datetime>.iqdat, so the .iqx stem pairs with the
# instrument key. Best-effort: when the output was renamed at runtime the stems
# do not match and no enrichment happens.
.bh_iqx_by_key <- function(output_dir) {
  iqx <- list.files(output_dir, pattern = "\\.iqx$", full.names = TRUE,
                    recursive = TRUE, ignore.case = TRUE)
  out <- list()
  for (f in iqx) {
    r <- tryCatch(read_iqx(f), error = function(e) NULL)
    if (is.null(r)) next
    key <- .bh_instrument_key(r$stem)
    if (nzchar(key) && is.null(out[[key]])) out[[key]] <- r
  }
  out
}

# Feed A: infer a task NAME for an instrument from its paired .iqx when the .iqx
# title is weak/generic but it carries item wording. One grounded LLM call over
# the .iqx description + items (the same construct-from-wording approach the
# codebook module uses for self_generated scale labels). Gated on llm_use(): with
# the LLM off, returns NULL and the caller falls back to the .iqx title or the
# instrument code. Returns a short task name, or NULL.
#
# A title is "weak" when it is empty, or an opaque/administrative code (a bare
# block name like "batch"/"block_1", or the instrument key itself) — those do not
# describe what the task measures, so the items are the better naming evidence.
.iqx_title_is_weak <- function(title, key) {
  if (is.null(title) || is.na(title) || !nzchar(trimws(title))) return(TRUE)
  t <- tolower(trimws(title))
  t == tolower(key) || grepl("^(block|batch|part|phase|trial|task)[ _]?[0-9]*$", t) ||
    nchar(t) < 4
}

.iqx_llm_name <- function(def, key, model, params) {
  if (!isTRUE(tryCatch(llm_use(), error = function(e) FALSE))) return(NULL)
  items <- def$items
  if (length(items) == 0) return(NULL)
  type_spec <- ellmer::type_object(
    task = ellmer::type_string(
      paste("Short natural name for the behavioural task or instrument these",
            "stimuli/items come from (e.g. 'Implicit Association Test',",
            "'Go/No-Go Task'), or empty if the items do not say.")))
  prompt <- paste(
    "You are given the DESCRIPTION and STIMULUS/ITEM wording of one behavioural",
    "task from an experiment script. Give a short, natural NAME for the task or",
    "instrument, grounded ONLY in the provided text. Never invent a published",
    "instrument name you cannot support; return empty if the text is uninformative.")
  text_in <- paste0(
    if (!is.na(def$description) && nzchar(def$description))
      paste0("Description:\n", def$description, "\n\n") else "",
    "Stimuli / items:\n", paste("-", utils::head(items, 30), collapse = "\n"))
  resp <- tryCatch(
    llm(text = data.frame(text = text_in), text_col = "text",
        system_prompt = prompt, type = type_spec, model = model,
        params = params, phase = "Naming tasks"),
    error = function(e) NULL)
  if (is.null(resp) || nrow(resp) == 0 || !"task" %in% names(resp)) return(NULL)
  nm <- trimws(as.character(resp$task[[1]] %||% ""))
  if (is.na(nm) || !nzchar(nm) || tolower(nm) %in% c("unknown", "unclear", "na", "none"))
    return(NULL)
  nm
}

# Canonical instrument keys that WILL have a paradata file, scanned before the
# OSD files are written so each OSD can embed the cross-reference. Light: reads
# each file and collects instrument keys, without building/validating documents.
.bh_paradata_keys <- function(output_dir, study_name = NULL) {
  files <- .bh_data_files(output_dir)
  if (!length(files)) return(character(0))
  study <- study_name %||% basename(output_dir)
  keys <- character(0)
  for (f in files) {
    res <- tryCatch(.bh_read_file(f, study), error = function(e) NULL)
    if (!is.null(res)) keys <- c(keys, names(res$instruments))
  }
  unique(keys)
}

# ── Orchestrator: write paradata/<instrument>.json for a converted dataset ────
# Parallel to .osd_write_scales(). Scans the copied data/ folder of a Psych-DS
# output for trial-level source files and writes ONE paradata/<id>.json per
# INSTRUMENT, carrying the full Response rows.
#
# Rows are ACCUMULATED ACROSS FILES before grouping. Experiment software publishes
# one file PER PARTICIPANT per block (an Inquisit study in the corpus has 439
# .iqdat files for one task), but those are not 439 instruments — they are one
# instrument whose rows are split across files, already distinguished by
# `agent_id`. Grouping per file instead of per instrument would emit hundreds of
# <id>-2.json, <id>-3.json — exactly the duplicate proliferation this feature
# exists to remove. A `-2` suffix is therefore reserved for a genuine collision
# (two DIFFERENT instrument names that slug to the same key), which is rare.
#
# Sets Instrument.link to the paired OSD when a scale with that instrument key was
# written (`osd_codes`). Returns an index (instrument_id -> rel-path + osd link)
# for OSD cross-refs and collection.json. Never deletes source data; the full
# response data is always written, however large.
.osd_write_paradata <- function(output_dir, osd_codes = character(0),
                                study_name = NULL, model = NULL, params = list()) {
  files <- .bh_data_files(output_dir)
  if (!length(files)) return(list())
  study <- study_name %||% basename(output_dir)

  # Feed B: Inquisit .iqx definitions, keyed by instrument, to enrich the paradata
  # instrument with a real task name/description (the .iqdat carries only an opaque
  # block code). Best-effort — an instrument with no matching .iqx keeps its code.
  iqx <- .bh_iqx_by_key(output_dir)

  # Pass 1: accumulate Response rows per instrument across every source file.
  rows <- list()      # instrument key -> list of Response rows
  meta <- list()      # instrument key -> list(name, format, fidelity)
  for (f in files) {
    res <- tryCatch(.bh_read_file(f, study), error = function(e) NULL)
    if (is.null(res)) next
    for (key in names(res$instruments)) {
      rows[[key]] <- c(rows[[key]], res$instruments[[key]])
      if (is.null(meta[[key]]))
        meta[[key]] <- list(name = unname(res$names[[key]]) %||% key,
                            format = res$format, fidelity = res$fidelity)
    }
  }
  if (!length(rows)) return(list())

  # Pass 2: one document, one file, per instrument.
  index <- list()
  for (key in names(rows)) {
    m <- meta[[key]]
    # Feed B: prefer the .iqx task title/description when a script pairs with this
    # instrument (by filename stem); else keep the format reader's name.
    def  <- iqx[[key]]
    nm   <- if (!is.null(def) && !is.na(def$title) && nzchar(def$title))
      def$title else m$name
    desc <- if (!is.null(def) && !is.na(def$description) && nzchar(def$description))
      def$description else NULL
    # Feed A: when the .iqx title is weak/generic but it has item wording, let the
    # LLM infer the task name from the stimuli (gated on llm_use(); no-op when off).
    if (!is.null(def) && .iqx_title_is_weak(nm, key)) {
      llm_nm <- .iqx_llm_name(def, key, model, params)
      if (!is.null(llm_nm)) nm <- llm_nm
    }
    doc <- .bh_trialdata(
      responses     = rows[[key]],
      instrument_id = key,
      name          = nm,
      link          = if (key %in% osd_codes) paste0("../scales/", key, ".osd") else NULL,
      fidelity      = m$fidelity,
      source_format = m$format,
      description   = desc)

    chk <- tryCatch(behaverse_validate(doc), error = function(e) NULL)
    if (!is.null(chk) && !isTRUE(chk$valid)) {
      message(sprintf(
        "Skipped paradata for instrument \"%s\": document did not validate against the Behaverse trial schema.",
        key))
      next
    }

    # One file per instrument: paradata/<instrument_id>.json. Guard against
    # over-long paths (OneDrive/Windows ~260-char limit) as the OSD writer does.
    p <- .safe_write_path(file.path(output_dir, "paradata", paste0(key, ".json")))
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    json <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(json, p, useBytes = TRUE)

    index[[length(index) + 1L]] <- list(
      instrument_id = key,
      path          = paste0("paradata/", key, ".json"),
      format        = m$format %||% "",
      n_responses   = length(rows[[key]]),
      osd_link      = if (key %in% osd_codes) paste0("scales/", key, ".osd") else NA_character_)
  }
  index
}
