#' Codebook Check
#'
#' @description
#' This module checks whether the data columns in a repository are documented in
#' a codebook or README. It locates codebook/readme files, extracts the variable
#' definitions they contain, matches those definitions against the data columns
#' extracted by `data_check`, and reports documentation coverage: how many data
#' columns are documented, and which documented variables are never used.
#'
#' @details
#' The Codebook Check module consumes the columns and file classification
#' produced by `data_check` (and, transitively, `repo_check`). Files classified
#' as `codebook` or `readme` are parsed with `parse_codebook()`, which reads
#' structured tables (CSV/TSV/Excel with a variable-name column and a label
#' column), embedded labels in haven files (SPSS/Stata/SAS), and plain text from
#' rich-text formats (docx/pdf/rtf/odt). Embedded haven labels from data files
#' are also harvested directly.
#'
#' Each data column is then matched against the parsed variable definitions with
#' `match_column_labels()`, using normalised-name matching with experiment-group
#' scoping, haven-label priority, and rule-based label-equivalence merging.
#'
#' Beyond the variable label, the module harvests the DDI-style per-variable
#' properties a source supplies: the **value labels / code list** (the
#' 1="Strongly disagree"…5="Strongly agree" mapping — from SPSS/Stata value
#' labels and from a codebook "values"/"coding" column), the **missing-value
#' scheme** (which codes denote missingness, from haven declared missings and
#' labels that read as "refused"/"n/a"), and **question text** and
#' columns when present. These are carried onto the matched
#' data columns and exported into the Psych-DS `variableMeasured` (as a
#' schema.org code list plus namespaced `metacheck:` fields).
#'
#' The module defaults to **rules-only** when `llm_use(FALSE)`. When
#' `llm_use(TRUE)`, three optional LLM tiers run: parsing unstructured codebook
#' text that the rules could not handle, fuzzy-matching still-unlabelled columns
#' to still-unmatched codebook variables, and merging conflicting label
#' definitions into a single canonical label.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local
#'   files only (see [test_paper()])
#' @param local_path optional path to a local directory, passed through to
#'   `data_check` / `repo_check` when their output is not already available
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param codebook_max_calls the maximum number of LLM calls a single tier will
#'   make (default 40): the number of 100-line text blocks per unstructured
#'   codebook file, and the number of distinct survey layouts sent for scale
#'   identification. This is an upfront gate: if a tier would need more calls
#'   than this, the whole tier is skipped (not truncated) with a message naming
#'   `codebook_max_calls` and the number needed.
#' @param model the LLM model name (see `llm_model_list()`) used only when
#'   `llm_use(TRUE)`
#' @param params a named list passed to `llm()`, used only when `llm_use(TRUE)`
#'
#' @returns a list
codebook_check <- function(paper, local_path = NULL, local_only = FALSE,
                           codebook_max_calls = 40L,
                           model = llm_model(),
                           params = list()) {

  .codebook_types <- c("codebook", "readme")
  # Data formats that carry embedded variable/value labels we can harvest as a
  # codebook. SPSS/Stata/SAS expose them via haven; JASP (.jasp) and jamovi
  # (.omv) carry the same haven-style label/labels attributes, exposed by
  # read_jasp()/read_omv(), so the SAME .extract_haven_labels() consumes them.
  .haven_exts     <- c("sav", "dta", "sas7bdat")
  .labelled_exts  <- c(.haven_exts, "jasp", "omv")
  max_llm_chunks  <- codebook_max_calls   # per unstructured codebook file
  gate_msgs       <- character(0)          # cap refusals to surface in the report

  # Resolve a usable paper_id from whatever tables we have.
  .pid <- function(...) {
    id <- paper_id(paper)
    for (df in list(...)) {
      if (length(id) > 0) break
      if (!is.null(df) && "paper_id" %in% names(df))
        id <- unique(df$paper_id)
    }
    if (length(id) == 0) return(NA_character_)
    id[[1]]
  }

  # ── 1. Get columns + file classification from data_check ─────────────────────
  columns_df <- get_prev_outputs("data_check", "table")
  structure_df <- get_prev_outputs("data_check", "structure")
  previews   <- get_prev_outputs("data_check", "previews")
  if (is.null(columns_df) || is.null(structure_df)) {
    mo <- if (!is.null(local_path)) {
      module_run(paper, "data_check", local_path = local_path,
                 local_only = local_only)
    } else {
      module_run(paper, "data_check", local_only = local_only)
    }
    columns_df   <- mo$table
    structure_df <- mo$structure
    previews     <- mo$previews
  }

  empty_summary <- function(text) {
    list(
      table = data.frame(),
      summary_table = data.frame(
        paper_id = .pid(columns_df, structure_df),
        column_n = 0, matched_n = 0, unmatched_n = 0, clean_n = 0,
        conflicted_n = 0, codebook_var_n = 0, unused_var_n = 0
      ),
      na_replace = c(column_n = 0, matched_n = 0, unmatched_n = 0, clean_n = 0,
                     conflicted_n = 0, codebook_var_n = 0, unused_var_n = 0),
      traffic_light = "na",
      summary_text = text,
      report = character(0)
    )
  }

  # With no shared data there is nothing to match a scale against, and we do NOT
  # archive scales inferred from prose alone. Return before naming scales from the
  # manuscript so the LLM is not called on a paper with no data file.
  if (is.null(columns_df) || nrow(columns_df) == 0)
    return(empty_summary("We found no extracted data columns to check against a codebook."))

  # ── 1b. Scales named in the MANUSCRIPT ───────────────────────────────────────
  # Runs only once we know data columns exist (above). It reads the paper text to
  # name instruments the authors describe, so the report can flag scales whose
  # item-level data was not shared alongside the data that WAS.
  text_scales <- .identify_scales_text_llm(paper, model, params)

  # ── 2. Locate codebook/readme files with a local copy ────────────────────────
  cb_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      structure_df$data_type %in% .codebook_types &
        !is.na(structure_df$file_location) &
        nzchar(structure_df$file_location) &
        file.exists(structure_df$file_location %||% ""),
      , drop = FALSE
    ]
  } else structure_df[0, , drop = FALSE]

  # Data files carrying embedded variable/value labels (SPSS/Stata/SAS via
  # haven, plus JASP/jamovi via read_jasp()/read_omv()).
  haven_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      !is.na(structure_df$data_type) & structure_df$data_type == "data" &
        tolower(tools::file_ext(structure_df$file_name)) %in% .labelled_exts &
        !is.na(structure_df$file_location) &
        file.exists(structure_df$file_location %||% ""),
      , drop = FALSE
    ]
  } else structure_df[0, , drop = FALSE]

  # ── 3. Parse codebook files (rules, then optional LLM) ───────────────────────
  llm_model_used  <- NA_character_
  llm_parse_files <- 0L
  llm_match_cols  <- 0L
  llm_merge_cols  <- 0L

  parsed_list <- list()
  if (nrow(cb_rows) > 0) {
    for (p in cb_rows$file_location) {
      pv <- parse_codebook(p)
      if (is.data.frame(pv) && nrow(pv) > 0) {
        parsed_list[[length(parsed_list) + 1L]] <- pv
      } else if (is.character(pv) && length(pv) > 0 && llm_use()) {
        # Unstructured text: send to the LLM in 100-line chunks. Upfront gate —
        # if this file needs more chunks (calls) than the cap allows, skip its
        # LLM parse entirely rather than silently truncating to the first N.
        n_chunks <- ceiling(length(pv) / 100)
        gate <- cap_gate_count(n_chunks, "codebook_max_calls", max_llm_chunks,
                               "text block", context = basename(p),
                               action = "parse")
        # Over the budget → report and skip this file's LLM parse.
        if (!is.null(gate)) {
          cap_report(gate)
          gate_msgs <- c(gate_msgs, gate)
        } else {
          llm_out <- codebook_parse_llm(pv, basename(p), model, params,
                                        max_chunks = max_llm_chunks)
          if (!is.null(llm_out) && nrow(llm_out) > 0) {
            parsed_list[[length(parsed_list) + 1L]] <- llm_out
            llm_parse_files <- llm_parse_files + 1L
            if (is.na(llm_model_used))
              llm_model_used <- attr(llm_out, "llm_model") %||% NA_character_
          }
        }
      }
    }
  }

  # Harvest embedded labels directly from data files. haven files (sav/dta/
  # sas7bdat) are read labels-only (n_max = 0L) and need the haven package;
  # .jasp/.omv carry the same haven-style attributes but are decoded by their
  # own readers (no labels-only mode, so the full data is read) and need no
  # haven. All feed the SAME .extract_haven_labels() extractor.
  if (nrow(haven_rows) > 0) {
    have_haven <- requireNamespace("haven", quietly = TRUE)
    for (i in seq_len(nrow(haven_rows))) {
      p <- haven_rows$file_location[i]
      # Scope these embedded labels to the file's OWN study group (data_check's
      # per-file assignment), not paper-wide — see .extract_haven_labels()'s
      # `group` argument for why this matters (stops one file's anomalous
      # embedded labels leaking onto same-named columns in a different study).
      file_group <- if ("group" %in% names(haven_rows)) haven_rows$group[i] else NA_character_
      ext <- tolower(tools::file_ext(p))
      df <- tryCatch(switch(ext,
        sav      = if (have_haven) as.data.frame(haven::read_sav(p, n_max = 0L)),
        dta      = if (have_haven) as.data.frame(haven::read_dta(p, n_max = 0L)),
        sas7bdat = if (have_haven) as.data.frame(haven::read_sas(p, n_max = 0L)),
        jasp     = read_jasp(p)$data,
        omv      = read_omv(p)$data
      ), error = function(e) NULL)
      if (is.null(df)) next
      res <- .extract_haven_labels(df, basename(p), group = file_group)
      if (is.null(res)) next
      # parse_method = "haven" for ALL embedded data-file labels (sav/dta/sas AND
      # jasp/omv). These labels live inside the data file, so downstream logic
      # that treats an embedded label as authoritative — e.g. the "haven_priority"
      # tie-breaker in match_column_labels() that lets an embedded label win a
      # label conflict — must apply to jasp/omv equally. Keying them all as
      # "haven" (the codebase's name for an embedded data-file label) gives them
      # that shared authority without touching every haven-keyed call site.
      res$parse_method <- "haven"
      parsed_list[[length(parsed_list) + 1L]] <- res
    }
  }

  codebook_vars_df <- if (length(parsed_list) > 0) {
    v <- dplyr::bind_rows(parsed_list)
    # Drop exact duplicate definitions (same normalised name + label + group).
    dup_key <- paste(normalize_varname(v$codebook_variable), v$label,
                     ifelse(is.na(v$group), "", v$group), sep = "\x01")
    v[!duplicated(dup_key), , drop = FALSE]
  } else {
    .empty_codebook_vars()
  }

  # ── 4. Match columns against codebook variables (rules, then optional LLM) ───
  labels_df <- match_column_labels(columns_df, codebook_vars_df)

  # Self-label export MACHINERY columns (paradata channels, survey-platform
  # metadata, display-order, trial-level task housekeeping) so they are NOT sent
  # to the codebook-matching LLM. A machinery column matches no codebook variable
  # and would otherwise stay `unlabelled` and pour into codebook_match_llm's
  # fuzzy-match tier — the wide-export fan-out this whole path guards against.
  # Marking them `labelled` with a deterministic, rule-assigned label keeps them
  # OUT of the LLM while still giving each a real variableMeasured description, so
  # Psych-DS compliance (every column described) is preserved. Only touches
  # columns still `unlabelled` — a machinery column the codebook DID document
  # keeps its real label. Uses the per-file preview so platform detection
  # (jsPsych/Inquisit/Behaverse) and Qualtrics/paradata name rules apply exactly
  # as in scale detection.
  labels_df <- .codebook_label_machinery(labels_df, previews)

  if (llm_use() && nrow(codebook_vars_df) > 0) {
    merged <- codebook_match_llm(labels_df, columns_df, codebook_vars_df,
                                 model, params)
    labels_df      <- merged$labels_df
    llm_match_cols <- merged$n_matched
    llm_merge_cols <- merged$n_merged
    if (is.na(llm_model_used)) llm_model_used <- merged$model %||% NA_character_
  }

  # ── 4b. Identify psychometric scales (manuscript-first) ──────────────────────
  # The dataset's own column names are the primary signal: columns sharing a
  # leading abbreviation (BSQ_*, MBSC_*, DEM_*) are almost always one instrument,
  # and the paper text names what the abbreviation stands for. Stages:
  #   1. Prefix-group + one-call LLM matcher (primary): group columns by
  #      abbreviation, retrieve paper sentences mentioning them, and ask the LLM
  #      (ONE call per file) to name each group FROM THE TEXT. scale_source =
  #      "manuscript". Every group is kept — unmatched ones stay unnamed.
  #   2. Dictionary rules matcher (fallback, incl. llm_use(FALSE)): for still-
  #      unnamed blocks, match a block prefix to the `scales` dictionary and
  #      confirm the FULL NAME in the text (never a bare acronym). scale_source =
  #      "matched".
  #   3. LLM self-generated fallback: coherent blocks matching no instrument get
  #      a construct label from the item/paper wording. scale_source =
  #      "self_generated".
  #   4. Prefix propagation to same-prefix sibling columns.
  # The full per-group inventory (named + unnamed) is also exported in the
  # OpenScales OSD JSON structure (`scales_osd`).
  labels_df$scale            <- NA_character_
  labels_df$scale_confidence <- NA_character_
  labels_df$scale_source     <- NA_character_
  n_scales_found     <- 0L
  n_scale_files      <- 0L
  n_scale_unnamed    <- 0L
  n_scale_selfgen    <- 0L
  n_tasks_found      <- 0L     # behavioural tasks named from the data
  n_task_files       <- 0L     # files whose columns look task-like (rt/accuracy)
  scale_groups       <- NULL   # per-group inventory (for OSD + report)
  have_previews <- !is.null(previews) && length(previews) > 0

  apply_scale <- function(sc, source = "matched") {
    # Fill scale/confidence/source from a PER-COLUMN result, without overwriting
    # a scale already set (earlier stages win; later only fill gaps).
    if (is.null(sc) || !nrow(sc)) return(invisible())
    key    <- paste(labels_df$source_file, labels_df$column_name, sep = "\x01")
    sc_key <- paste(sc$source_file, sc$column_name, sep = "\x01")
    m      <- match(key, sc_key)
    fill   <- !is.na(m) & (is.na(labels_df$scale) | !nzchar(labels_df$scale))
    src    <- if ("scale_source" %in% names(sc)) sc$scale_source[m[fill]] else source
    labels_df$scale[fill]            <<- sc$scale[m[fill]]
    labels_df$scale_confidence[fill] <<- sc$confidence[m[fill]]
    labels_df$scale_source[fill]     <<- src
  }

  # Stage 1: prefix-group + one-call LLM matcher (the primary namer).
  if (have_previews) {
    scale_groups <- .identify_scales_prefix_llm(
      previews, labels_df, paper, model, params, max_calls = codebook_max_calls)
    if (!is.null(scale_groups) && nrow(scale_groups) > 0) {
      n_scale_files <- length(unique(scale_groups$source_file))
      # Expand named groups to per-column rows for apply_scale.
      named_grp <- scale_groups[!is.na(scale_groups$scale) &
                                nzchar(scale_groups$scale), , drop = FALSE]
      if (nrow(named_grp) > 0) {
        per_col <- do.call(rbind, lapply(seq_len(nrow(named_grp)), function(i) {
          r <- named_grp[i, ]; cols <- r$columns[[1]]
          data.frame(source_file = r$source_file, column_name = cols,
                     scale = r$scale, confidence = r$confidence,
                     scale_source = "manuscript", stringsAsFactors = FALSE)
        }))
        apply_scale(per_col)
      }
      if (is.na(llm_model_used))
        llm_model_used <- attr(scale_groups, "llm_model") %||% NA_character_
    }
  }

  # Stage 2: dictionary rules matcher (fallback; also the only namer w/o an LLM).
  if (have_previews) {
    scr <- .identify_scales_rules(previews, labels_df, paper)
    if (!is.null(attr(scr, "n_detected")))
      n_scale_files <- max(n_scale_files, attr(scr, "n_detected"))
    apply_scale(scr, source = "matched")
  }

  # Stage 2b: task matcher. Runs on the same previews but detects the OTHER
  # shape of instrument — a behavioural task (Stroop, IAT, n-back), whose data
  # is trial-level rt/accuracy rather than a Likert block, so Stage 2 cannot
  # see it. Deliberately after Stage 2: apply_scale() never overwrites a name an
  # earlier stage set, so a file holding both a questionnaire and a task keeps
  # the questionnaire's naming for its Likert columns.
  if (have_previews) {
    tkr <- .identify_tasks_rules(previews, labels_df, paper)
    if (nrow(tkr) > 0) {
      apply_scale(tkr, source = "task_matched")
      n_tasks_found <- length(unique(paste(tkr$source_file, tkr$scale)))
    }
    n_task_files <- attr(tkr, "n_detected") %||% 0L
  }

  # Stage 3: LLM self-generated fallback.
  if (llm_use() && have_previews) {
    sg <- .identify_scales_selfgen(previews, labels_df, paper, model, params,
                                   columns_df = columns_df,
                                   text_scales = text_scales,
                                   max_calls = codebook_max_calls)
    if (!is.null(sg) && nrow(sg) > 0) {
      apply_scale(sg, source = "self_generated")
      n_scale_selfgen <- length(unique(paste(sg$source_file, sg$scale)))
      if (is.na(llm_model_used))
        llm_model_used <- attr(sg, "llm_model") %||% NA_character_
    }
  }

  # Stage 4: propagate to same-prefix siblings.
  labels_df <- .propagate_scale_by_prefix(labels_df)

  # Backfill scale_groups$scale from the FINAL labels_df. scale_groups was set by
  # Stage 1 (manuscript LLM) only; the dictionary (Stage 2) and self-generated
  # (Stage 3) tiers write their names into labels_df, NOT scale_groups. Without
  # this, a block named by the dictionary or self-gen tier counts as named
  # (scale_named_n) but its OSD entry keeps write = FALSE, so no .osd file is
  # written. Copy each group's column-level name (and its source) back so
  # .scales_to_osd() sees every named block.
  if (!is.null(scale_groups) && nrow(scale_groups) > 0 &&
      !is.null(labels_df) && nrow(labels_df) > 0) {
    lk <- paste(labels_df$source_file, labels_df$column_name, sep = "\x01")
    for (i in seq_len(nrow(scale_groups))) {
      cur <- scale_groups$scale[i]
      if (!is.na(cur) && nzchar(cur)) next          # Stage 1 already named it
      cols <- scale_groups$columns[[i]]
      idx  <- match(paste(scale_groups$source_file[i], cols, sep = "\x01"), lk)
      idx  <- idx[!is.na(idx)]
      nm   <- labels_df$scale[idx]
      nm   <- nm[!is.na(nm) & nzchar(nm)]
      if (length(nm)) {
        scale_groups$scale[i] <- nm[[1]]
        if ("scale_source" %in% names(labels_df)) {
          ss <- labels_df$scale_source[idx]
          ss <- ss[!is.na(ss) & nzchar(ss)]
          if (length(ss)) scale_groups$scale_source[i] <- ss[[1]]
        }
        # Confidence must ride along with the name. Without this a scale named
        # by the dictionary or self-gen tier reached the report with an EMPTY
        # confidence: it showed as blank in the inventory table, and — because
        # the guidance block counts `confidence %in% c("medium","low")` — a
        # medium-confidence match silently suppressed the "how to make this
        # high confidence" advice that the match was supposed to trigger.
        if ("scale_confidence" %in% names(labels_df) &&
            "confidence" %in% names(scale_groups)) {
          cf <- labels_df$scale_confidence[idx]
          cf <- cf[!is.na(cf) & nzchar(cf)]
          if (length(cf)) scale_groups$confidence[i] <- cf[[1]]
        }
      }
    }
  }

  # OSD export of the full per-group inventory (named + unmatched groups kept).
  # Response scale lets the codebook lead (columns_df = data_check stats fallback,
  # labels_df = codebook value labels + item wording).
  scales_osd <- if (!is.null(scale_groups) && nrow(scale_groups) > 0)
    .scales_to_osd(scale_groups, columns_df, labels_df) else list()

  named <- !is.na(labels_df$scale) & nzchar(labels_df$scale)
  n_scales_found <- length(unique(labels_df$scale[named]))
  files_named    <- unique(labels_df$source_file[named])
  n_scale_unnamed <- max(0L, n_scale_files - length(files_named))

  # Tasks named in the manuscript but absent from the data. A task is often
  # described in the methods and its trial-level data never shared, so this is
  # the "measured but not shared" signal — the task counterpart of an orphan
  # total. Reported, never treated as an error: a task may legitimately live in
  # a file we could not read, and not finding a task in the data is common.
  tasks_in_paper <- .scan_paper_for_tasks(paper)
  tasks_in_data  <- unique(labels_df$scale[named &
                     labels_df$scale_source %in% "task_matched"])
  tasks_paper_only <- setdiff(tasks_in_paper, tasks_in_data)
  n_tasks_paper_only <- length(tasks_paper_only)

  # ── 5. Coverage tallies ──────────────────────────────────────────────────────
  # Two distinct questions, kept separate:
  #   matched  — did the column match a codebook entry by name? (data coverage)
  #   clean    — did the column also get a single usable label? (label quality)
  # A conflicting/ambiguous column IS matched (it appears in the codebook) but
  # its label needs resolution, so it counts toward coverage but not clean.
  clean      <- labels_df$label_status %in% c("labelled", "llm")
  conflicted <- labels_df$label_status %in% c("conflicting_definition",
                                              "ambiguous_experiment")
  matched    <- clean | conflicted

  n_columns    <- nrow(labels_df)
  n_matched    <- sum(matched)
  n_unmatched  <- n_columns - n_matched
  n_clean      <- sum(clean)
  n_conflicted <- sum(conflicted)

  # A codebook variable is "used" if any data column matched its name — whether
  # or not the resulting label was clean. This keeps a conflicted-but-present
  # variable out of the "unused" list. Conflicting/ambiguous rows can list
  # several codebook variables (" | "-joined), so split before normalising.
  matched_norm <- unique(normalize_varname(
    unlist(strsplit(
      labels_df$codebook_variable[matched & !is.na(labels_df$codebook_variable)],
      " | ", fixed = TRUE))
  ))
  n_codebook_vars <- nrow(codebook_vars_df)
  used_var <- if (n_codebook_vars > 0)
    normalize_varname(codebook_vars_df$codebook_variable) %in% matched_norm else
    logical(0)
  n_unused <- sum(!used_var)

  pct_matched <- if (n_columns > 0)
    round(100 * n_matched / n_columns) else 0L

  # A codebook that WAS parsed but almost none of whose variables line up with
  # the data columns: the documentation exists but cannot be linked automatically
  # (e.g. the data holds computed scores like `neoImagination` while the codebook
  # documents the underlying items `neo1`…`neo120`, or the names simply differ).
  # Flag it so the author knows their codebook was found but not usable as-is, and
  # how to organise it so it can be. Require a codebook of real size and a very
  # low match rate, so an ordinary partial-coverage codebook is not flagged.
  codebook_misaligned <- n_codebook_vars >= 5L && n_columns > 0 &&
    pct_matched < 20 && n_unused >= 0.8 * n_codebook_vars
  misalign_msg <- if (isTRUE(codebook_misaligned)) {
    ex_cb  <- utils::head(codebook_vars_df$codebook_variable[!used_var], 3)
    ex_col <- utils::head(labels_df$column_name[!matched], 3)
    sprintf(paste0(
      "A codebook was found and parsed (%d variable definition%s), but its ",
      "variable names do not match the data columns, so the documentation could ",
      "not be linked automatically (only %d%% of columns matched). For example, ",
      "the codebook documents %s while the data has %s. This often means the data ",
      "holds computed scores or subscales while the codebook lists the underlying ",
      "items. To make the codebook usable automatically, document variables under ",
      "the exact names used in the data file (one row per data column), with a ",
      "label and, for rating items, the response scale."),
      n_codebook_vars, plural(n_codebook_vars), pct_matched,
      paste(sprintf("`%s`", ex_cb), collapse = ", "),
      paste(sprintf("`%s`", ex_col), collapse = ", "))
  } else NULL

  # ── 6. Traffic light ─────────────────────────────────────────────────────────
  # Coverage (unmatched columns) and label quality (conflicts) are separate
  # concerns; either can lower the light.
  tl <- if (n_codebook_vars == 0) "red"           # no codebook at all
        else if (n_unmatched == 0 && n_conflicted == 0) "green"  # all matched, all clean
        else if (pct_matched >= 80) "yellow"      # mostly matched, or only conflicts
        else "red"                                # substantial coverage gaps

  # ── 7. Report ────────────────────────────────────────────────────────────────
  if (n_codebook_vars == 0) {
    summary_text <- sprintf(
      "We found no codebook or README documentation for the %d extracted data column%s.",
      n_columns, plural(n_columns)
    )
  } else {
    summary_text <- c(
      sprintf("We parsed %d variable definition%s from codebook/README files.",
              n_codebook_vars, plural(n_codebook_vars)),
      sprintf("%d of %d data column%s (%d%%) %s documented in a codebook; %d %s not.",
              n_matched, n_columns, plural(n_columns), pct_matched,
              if (n_matched == 1) "is" else "are",
              n_unmatched, if (n_unmatched == 1) "is" else "are"),
      if (n_conflicted > 0) sprintf(
        "%d matched column%s %s a conflicting or ambiguous label that needs resolution.",
        n_conflicted, plural(n_conflicted), if (n_conflicted == 1) "has" else "have"),
      if (n_unused > 0) sprintf(
        "%d documented variable%s never appear%s in the data.",
        n_unused, plural(n_unused), if (n_unused == 1) "s" else ""),
      misalign_msg
    ) |> paste("\n- ", x = _, collapse = "")
  }

  report <- c(
    "This module checks whether each extracted data column is documented in a codebook or README, and flags documented variables that never appear in the data."
  )

  n_cb_files <- nrow(cb_rows) + nrow(haven_rows)
  report <- c(report, sprintf(
    "We examined %d codebook/README/label source%s and %d data column%s across %d file%s.",
    n_cb_files, plural(n_cb_files),
    n_columns, plural(n_columns),
    length(unique(labels_df$source_file)), plural(length(unique(labels_df$source_file)))
  ))

  if (n_codebook_vars == 0) {
    report <- c(report,
      "No codebook or README documentation was found, so no data columns could be matched to variable definitions.")
  } else {
    if (!is.null(misalign_msg))
      report <- c(report, paste0("**", misalign_msg, "**"))
    # Coverage table, one tab per data file. Documented is three-state:
    # "yes" (clean label), "conflict" (matched but unresolved), "no" (unmatched).
    doc_state <- ifelse(clean, "yes", ifelse(conflicted, "conflict", "no"))
    label_tbl <- labels_df |>
      dplyr::mutate(Documented = doc_state) |>
      dplyr::transmute(
        .data$source_file,
        Column      = .data$column_name,
        Documented  = .data$Documented,
        Label       = .data$label,
        `Codebook Variable` = .data$codebook_variable,
        Source      = .data$label_source,
        Status      = .data$label_status
      ) |>
      dplyr::arrange(.data$Documented, .data$Column)

    report <- c(
      report,
      "#### Column Documentation",
      codebook_file_tabset(label_tbl)
    )

    # Conflicting / ambiguous definitions — matched, but the label is not usable
    # as-is. Surface these explicitly so a label typo across codebooks is not
    # buried in the undocumented count. (With llm_use(TRUE) the merge tier
    # resolves most of these before this point.)
    if (n_conflicted > 0) {
      conflict_tbl <- labels_df[conflicted, , drop = FALSE] |>
        dplyr::transmute(
          File     = .data$source_file,
          Column   = .data$column_name,
          `Conflicting Labels` = .data$label,
          Sources  = .data$label_source,
          Issue    = .data$label_status
        )
      report <- c(
        report,
        sprintf("#### Conflicting or Ambiguous Definitions\n\n%d matched column%s %s more than one codebook definition, or a definition scoped to a different experiment. These are matched to a codebook but need manual resolution before the label can be trusted.",
                n_conflicted, plural(n_conflicted),
                if (n_conflicted == 1) "has" else "have"),
        scroll_table(conflict_tbl, maxrows = 15)
      )
    }

    # Unused codebook variables.
    if (n_unused > 0) {
      unused_tbl <- codebook_vars_df[!used_var, , drop = FALSE] |>
        dplyr::transmute(
          Variable = .data$codebook_variable,
          Label    = .data$label,
          Source   = .data$codebook_source
        )
      report <- c(
        report,
        sprintf("#### Documented but Unused Variables\n\n%d documented variable%s %s not matched to any data column.",
                n_unused, plural(n_unused), if (n_unused == 1) "was" else "were"),
        scroll_table(unused_tbl, maxrows = 15)
      )
    }
  }

  # ── Scales ───────────────────────────────────────────────────────────────────
  # Report the scale-group inventory: every column group detected by its shared
  # abbreviation, with the instrument it was matched to (from the manuscript /
  # dictionary) or "not matched" when the text did not name it. Unmatched groups
  # are shown too — a real column family we saw but could not name.
  if (!is.null(scale_groups) && nrow(scale_groups) > 0) {
    sg <- scale_groups
    sg$Matched <- ifelse(!is.na(sg$scale) & nzchar(sg$scale),
                         sg$scale, "— not matched to a named scale —")
    inv_tbl <- data.frame(
      File        = sg$source_file,
      Abbrev      = sg$prefix,
      Columns     = sg$n_columns,
      `Max item`  = ifelse(is.na(sg$max_item), "?", as.character(sg$max_item)),
      Scale       = sg$Matched,
      Confidence  = ifelse(is.na(sg$confidence), "", sg$confidence),
      Source      = ifelse(is.na(sg$scale_source), "", sg$scale_source),
      check.names = FALSE, stringsAsFactors = FALSE)
    n_grp   <- nrow(sg)
    n_named <- sum(!is.na(sg$scale) & nzchar(sg$scale))
    report <- c(report, "#### Scales",
      sprintf("We detected %d column group%s (by shared abbreviation) that look like scales; %d %s matched to a named instrument from the paper text or dictionary. Groups we could not name are listed too — they are real scale-like column families whose instrument the manuscript did not identify. Identified scales are exported in the OpenScales OSD structure.",
              n_grp, plural(n_grp), n_named, if (n_named == 1) "was" else "were"),
      scroll_table(inv_tbl, maxrows = 40))
    # Orphan totals: a totals-only block whose scale has no genuine item block
    # anywhere (matched by name). Warn — we found the score but not the items.
    if ("totals_only" %in% names(sg)) {
      tot <- !is.na(sg$totals_only) & sg$totals_only &
             !is.na(sg$scale) & nzchar(sg$scale)
      have_items <- unique(tolower(sg$scale[!tot & !is.na(sg$scale) &
                                            nzchar(sg$scale)]))
      orphan <- tot & !(tolower(sg$scale) %in% have_items)
      if (any(orphan)) {
        ot <- sg[orphan, , drop = FALSE]
        lines <- vapply(seq_len(nrow(ot)), function(j)
          sprintf("- **%s** (columns `%s` in %s)", ot$scale[j],
                  paste(utils::head(ot$columns[[j]], 6), collapse = "`, `"),
                  ot$source_file[j]), character(1))
        report <- c(report, paste0(
          "**Scale totals without item-level data.** For the following, we ",
          "identified what looks like the total or average score, but found ",
          "**no individual item columns**. The items may not be shared, or are ",
          "labelled differently. Consider sharing the item-level data, or ",
          "labelling items clearly, so the scale can be verified:"),
          lines)
      }
    }

    # Guidance when some groups are unnamed or only tentatively named.
    n_unnamed <- n_grp - n_named
    n_low <- sum(sg$confidence %in% c("medium","low"))
    if (n_unnamed > 0 || n_low > 0)
      report <- c(report, paste(
        "To let this tool (and anyone reusing the data) identify these scales with **high confidence**:",
        "\n- **Name the instrument in the manuscript with its abbreviation** — e.g. \"the Breakup Distress Scale (BDS)\" — matching the column prefix.",
        "\n- **Add a codebook** giving the item wording, or state the number of items (\"a 15-item scale\").",
        sep = ""))
  } else if (!llm_use() && !is.null(previews) && length(previews) > 0) {
    report <- c(report, "#### Scales",
      "Scale naming from the manuscript needs an LLM (enable with `llm_use(TRUE)`); the dictionary rules matcher still names instruments whose abbreviation matches a known scale.")
  }

  # Instruments the manuscript describes but the shared data does not carry.
  # `.scale_prefix_groups()` only sees column families with a shared leading
  # abbreviation, so scales whose columns are item-content words are invisible to
  # every stage above no matter how well the data is documented.
  report <- c(report,
              .scale_text_report(text_scales,
                                 matched = unique(labels_df$scale[named])))

  # ── Tasks ───────────────────────────────────────────────────────────────────
  # Reported separately from scales because the evidence is different: a task is
  # recognised from rt/accuracy columns rather than a Likert block, and a task
  # named in the paper with no data behind it is a routine, reportable state
  # rather than a fault.
  if (n_task_files > 0 || n_tasks_paper_only > 0) {
    task_lines <- character(0)
    if (n_task_files > 0)
      task_lines <- c(task_lines, sprintf(
        "%d data file%s contain%s columns that look like a behavioural task (reaction times, accuracy, or a block of correct/incorrect items). %s named to a known task.",
        n_task_files, plural(n_task_files),
        if (n_task_files == 1) "s" else "",
        if (n_tasks_found > 0)
          sprintf("%d distinct task%s %s", n_tasks_found, plural(n_tasks_found),
                  if (n_tasks_found == 1) "was" else "were")
        else "None could be"))
    if (n_tasks_paper_only > 0)
      task_lines <- c(task_lines, paste0(
        sprintf("**%d task%s named in the manuscript %s no matching data.** ",
                n_tasks_paper_only, plural(n_tasks_paper_only),
                if (n_tasks_paper_only == 1) "has" else "have"),
        "The trial-level data may not be shared, may live in a file we could ",
        "not read, or may use column names we did not recognise. Sharing ",
        "trial-level data (one row per trial, with condition, response time ",
        "and accuracy) would let the task be verified:"),
        paste0("- ", tasks_paper_only))
    if (length(task_lines))
      report <- c(report, "#### Tasks", task_lines)
  }

  if (llm_use()) {
    llm_text <- sprintf(
      "%sreviewed ambiguous cases (parsed %d file%s, matched %d column%s, merged %d label%s).",
      if (!is.na(llm_model_used)) sprintf("LLM model '%s' ", llm_model_used) else "LLM ",
      llm_parse_files, plural(llm_parse_files),
      llm_match_cols, plural(llm_match_cols),
      llm_merge_cols, plural(llm_merge_cols)
    )
    report <- c(report, llm_text)
  }

  # Data-quality warning: files whose column names are duplicated (a survey
  # loop/merge export that did not number its repeated blocks). Surfaced per file
  # so authors can re-export; never a hard failure.
  dup_warnings <- .codebook_duplicate_name_warnings(previews)
  if (length(dup_warnings) > 0)
    report <- c(report, "#### Duplicated Column Names",
                paste0("- ", dup_warnings))

  # Tiers refused by the LLM call budget: name the parameter and the value to
  # lift it (upfront gate — the tier was skipped, not partially processed).
  if (length(gate_msgs) > 0)
    report <- c(report, paste0("- ", gate_msgs))

  # ── 8. Summary table + return ────────────────────────────────────────────────
  pid <- .pid(labels_df, columns_df, structure_df)
  summary_table <- data.frame(
    paper_id       = pid,
    column_n       = n_columns,
    matched_n      = n_matched,
    unmatched_n    = n_unmatched,
    clean_n        = n_clean,
    conflicted_n   = n_conflicted,
    codebook_var_n = n_codebook_vars,
    unused_var_n   = n_unused,
    # Scale detection vs. naming: how many data files held a scale-like item
    # block (rule-based detection), how many distinct instruments were named
    # (LLM), and how many detected blocks stayed unnamed. The gap between the
    # first and the last is the "looks like a scale but we can't name it" set.
    scale_blocks_n  = n_scale_files,
    scale_named_n   = n_scales_found,
    scale_unnamed_n = n_scale_unnamed,
    # Behavioural tasks (Stroop, IAT, n-back). Detected from a different data
    # signature than scales — rt/accuracy columns rather than a Likert block —
    # so counted separately. `task_paper_only_n` is the task named in the
    # manuscript with no matching data: measured but seemingly not shared.
    task_files_n     = n_task_files,
    task_named_n     = n_tasks_found,
    task_paper_only_n = n_tasks_paper_only
  )

  list(
    table = labels_df,
    codebook_vars = codebook_vars_df,
    scales_osd = scales_osd,           # per-group inventory in OpenScales OSD form
    summary_table = summary_table,
    na_replace = c(column_n = 0, matched_n = 0, unmatched_n = 0, clean_n = 0,
                   conflicted_n = 0, codebook_var_n = 0, unused_var_n = 0,
                   scale_blocks_n = 0, scale_named_n = 0, scale_unnamed_n = 0,
                   task_files_n = 0, task_named_n = 0, task_paper_only_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# ── Module-local helpers ──────────────────────────────────────────────────────

# One column-documentation table per source file inside a Quarto tabset (mirrors
# data_check's file_tabset). `tbl` must include a `source_file` column.
codebook_file_tabset <- function(tbl) {
  files <- unique(tbl$source_file)
  if (length(files) == 0) return(NULL)
  # Blank-line-separate every block so Pandoc parses each `## file` as a tab
  # heading rather than swallowing it into the preceding table block.
  tabs <- vapply(files, function(f) {
    sub <- tbl[tbl$source_file == f, setdiff(names(tbl), "source_file"),
               drop = FALSE]
    paste(c(paste0("## ", f), scroll_table(sub, maxrows = 25)), collapse = "\n\n")
  }, character(1))
  paste(c("::: {.panel-tabset}", tabs, ":::"), collapse = "\n\n")
}

# LLM tier 1: parse unstructured codebook text (a character vector of lines) into
# variable definitions. Returns a data.frame (with an "llm_model" attribute) or
# NULL. Chunks lines in blocks of 100, capped at `max_chunks` calls.
codebook_parse_llm <- function(lines, src, model, params, max_chunks = 10L) {
  chunks <- split(lines, ceiling(seq_along(lines) / 100))
  chunks <- chunks[seq_len(min(length(chunks), max_chunks))]

  # Wrap the array in a single-field object: Groq's gpt-oss-20b rejects a
  # top-level bare array schema (HTTP 400 json_validate_failed). llm()'s
  # .unnest_result() unwraps the single field back into rows.
  type_spec <- ellmer::type_object(
    variables = ellmer::type_array(
      ellmer::type_object(
        variable_name = ellmer::type_string("Exact variable name/code in the data file"),
        label         = ellmer::type_string("Verbatim description text from the codebook"),
        experiment_context = ellmer::type_string(
          "Experiment/study heading if stated, else empty", required = FALSE)
      )
    )
  )
  prompt <- paste(
    "You are extracting variable definitions from a psychology research codebook or README.",
    "For each variable that has both a name and a verbatim description, return the exact",
    "variable name, the description copied verbatim (do not paraphrase), and the experiment",
    "or study heading it appears under (empty if none). Omit variables without a description."
  )

  out <- list()
  model_used <- NA_character_
  for (ch in chunks) {
    txt <- paste(ch, collapse = "\n")
    resp <- tryCatch(
      llm(text = data.frame(text = txt), text_col = "text",
          system_prompt = prompt, type = type_spec, model = model,
          params = params, phase = "Parsing codebook"),
      error = function(e) NULL
    )
    resp <- .strip_llm_wrapper(resp, "variables")
    if (is.null(resp) || nrow(resp) == 0) next
    if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_
    vn <- resp$variable_name %||% character(0)
    keep <- !is.na(vn) & nzchar(trimws(vn))
    if (!any(keep)) next
    ec <- if ("experiment_context" %in% names(resp))
      as.character(resp$experiment_context[keep]) else NA_character_
    out[[length(out) + 1L]] <- data.frame(
      codebook_variable = as.character(vn[keep]),
      label             = as.character(resp$label[keep]),
      codebook_source   = src,
      group             = .infer_group(ec),
      parse_method      = "llm"
    )
  }
  if (length(out) == 0) return(NULL)
  res <- dplyr::bind_rows(out)
  attr(res, "llm_model") <- model_used
  res
}

# Identify psychometric scales in the data with an LLM.
#
# Strategy: ONE call per file (not per block). For each readable data file we
# collect its Likert-eligible columns (via .detect_scale_blocks) plus any
# codebook item wording, and ask the model to SEGMENT them into scales and name
# each — a single structured response listing {scale, confidence, columns}. This
# replaces the previous one-call-per-block loop (which fired dozens of calls on
# a multi-file repo). Files that share an identical Likert-column signature
# (e.g. sample1/2/3-informant.csv) are identified once and the result reused, so
# the number of LLM calls is the number of DISTINCT survey layouts, not files.
#
# Returns a data.frame (source_file, column_name, scale, confidence), one row
# per item column of a confidently identified scale, with attributes
# `llm_model`, `n_detected` (files with scale-like content) and `n_skipped`.
# Retrieve sentences from the paper that mention a scale block, to give the LLM
# the manuscript's own naming as context. Searches for (a) the block's
# variable-name prefixes as whole-word tokens (authors often name variables
# after the instrument, e.g. `panas_1`), and (b) the most distinctive content
# words from the item labels (rare words like "enthusiastic" pin down PANAS).
# Returns up to `max_sent` unique sentences, or character(0).
.scale_paper_context <- function(paper, prefixes, labels, max_sent = 6L) {
  terms <- character(0)
  # Informative prefixes only (drop generic Q/V/x and very short ones).
  pfx <- unique(prefixes[nzchar(prefixes) & nchar(prefixes) >= 3 &
                         !grepl("^(col|var|item|value|resp)$", prefixes)])
  # Prefixes may contain regex metacharacters; escape them before use.
  esc <- function(x) gsub("([][{}().^$*+?|\\\\-])", "\\\\\\1", x)
  if (length(pfx) > 0)
    terms <- c(terms, sprintf("\\b%s\\b", esc(pfx)))
  # Distinctive item words: content words >= 5 chars from the labels, the rarer
  # the better; take a handful so the search stays specific.
  lab_txt <- labels[!is.na(labels) & nzchar(labels)]
  if (length(lab_txt) > 0) {
    words <- unlist(strsplit(tolower(paste(lab_txt, collapse = " ")), "[^a-z]+"))
    words <- words[nchar(words) >= 5]
    words <- names(sort(table(words)))          # rarest first
    words <- utils::head(words[!words %in% .scale_stopwords], 6L)
    if (length(words) > 0)
      terms <- c(terms, sprintf("\\b%s\\b", words))
  }
  if (length(terms) == 0) return(character(0))

  sents <- tryCatch(
    text_search(paper, pattern = terms, return = "sentence", ignore.case = TRUE),
    error = function(e) NULL)
  if (is.null(sents) || nrow(sents) == 0) return(character(0))
  s <- unique(trimws(as.character(sents$text)))
  s <- s[nzchar(s)]
  utils::head(s, max_sent)
}

# Scale dictionary: the curated `scales` dataset (see R/scales.R / data-raw/
# scales.R), one row per instrument with name / acronym / code / source. Loaded
# once and cached. Acronyms may collide (AQ, MFQ, SDS, ...); the matcher
# disambiguates a collision from the codebook and paper text.
.scale_dictionary <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    d <- tryCatch(get("scales", envir = asNamespace("metacheck")),
                  error = function(e) NULL)
    if (is.null(d)) d <- tryCatch(get("scales"), error = function(e) NULL)
    if (is.null(d)) d <- data.frame(name = character(), acronym = character(),
                                    code = character(), source = character())
    cached <<- d
    d
  }
})

# Task dictionary: the `tasks` dataset (see R/tasks.R / data-raw/tasks.R), one
# row per behavioural task with name / acronym / code / atlas_id. Same shape and
# same role as `.scale_dictionary()`, so the text-matching helpers below work on
# either without modification.
.task_dictionary <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    d <- tryCatch(get("tasks", envir = asNamespace("metacheck")),
                  error = function(e) NULL)
    if (is.null(d)) d <- tryCatch(get("tasks"), error = function(e) NULL)
    if (is.null(d)) d <- data.frame(name = character(), acronym = character(),
                                    code = character(), atlas_id = character())
    cached <<- d
    d
  }
})

# Regex matching an instrument in running text: its full name (tolerant of
# spacing/dash/punctuation) OR its acronym as a whole word. Built per dictionary
# row. NA acronym -> name only.
#
# The separator class must include BOTH apostrophes. The name is split on
# non-alphanumerics, so "Raven's" becomes the tokens Raven + s, and rejoining
# them with a class that lacks an apostrophe produces `Raven[\s._/-]*s`, which
# cannot match the literal "Raven's". That silently made every possessive
# instrument unmatchable in paper text — 19 of them across the two dictionaries
# (Raven's Advanced Progressive Matrices, Children's Depression Inventory,
# Addenbrooke's Cognitive Examination, ...). Both the ASCII (') and typographic
# (’) forms are included because manuscripts and the dictionaries use both.
.scale_text_pattern <- function(name, acronym) {
  toks <- unlist(strsplit(name, "[^A-Za-z0-9]+"))
  toks <- toks[nzchar(toks)]
  name_pat <- paste(vapply(toks, function(t)
    gsub("([][{}().^$*+?|\\\\-])", "\\\\\\1", t), character(1)),
    collapse = "[\\s._/'’-]*")
  parts <- name_pat
  if (!is.na(acronym) && nzchar(acronym)) {
    a <- gsub("([][{}().^$*+?|\\\\-])", "\\\\\\1", acronym)
    parts <- c(parts, paste0("\\b", a, "\\b"))
  }
  paste(parts, collapse = "|")
}

# Scan the paper's full text for dictionary instruments mentioned by name or
# acronym. Returns the canonical names found, to corroborate a data-proposed
# scale (and to offer the LLM confirmable candidates). Cheap regex over the text.
.scan_paper_for_scales <- function(paper) {
  .scan_paper_with_dict(paper, .scale_dictionary())
}

# Scan a paper's text for any dictionary's instruments. Factored out of
# .scan_paper_for_scales so the SAME matching applies to tasks: a task is
# named in a manuscript exactly the way a scale is ("participants completed a
# Stroop task"), and the `tasks` dataset carries the same name/acronym columns.
.scan_paper_with_dict <- function(paper, dict) {
  if (!.is_paper(paper) || is.null(paper$text) || nrow(paper$text) == 0)
    return(character(0))
  if (is.null(dict) || nrow(dict) == 0) return(character(0))
  hay <- paste(as.character(paper$text$text), collapse = " \n ")
  hit <- vapply(seq_len(nrow(dict)), function(i) {
    pat <- .scale_text_pattern(dict$name[i], dict$acronym[i])
    isTRUE(tryCatch(grepl(pat, hay, perl = TRUE, ignore.case = TRUE),
                    error = function(e) FALSE))
  }, logical(1))
  unique(dict$name[hit])
}

# Task names mentioned in the paper text. Used to corroborate a task proposed by
# the data, and reported on its own: a task named in the manuscript but absent
# from the data is exactly the "measured but not shared" signal codebook_check
# already reports for scales.
.scan_paper_for_tasks <- function(paper) {
  .scan_paper_with_dict(paper, .task_dictionary())
}

# Common words to exclude from "distinctive item word" searches.
.scale_stopwords <- c("scale","agree","disagree","strongly","never","always",
  "sometimes","often","rarely","please","following","statement","question",
  "response","really","think","feel","would","about","which","there","their",
  "other","because","being")

# Does the identified scale name (or a parenthesised acronym within it) appear
# in the retrieved paper sentences? Used to corroborate an identification (a
# hit here promotes the identification's confidence to "high").
#
# Full multi-word names are matched as a bare (fixed) substring — safe, since
# a specific multi-word phrase colliding by accident with unrelated prose is
# implausible. A bare 3-letter ACRONYM is a different risk entirely: as an
# unanchored substring it matches inside ordinary words too (verified against
# real text: "MES" inside "ti-MES", "EAS" inside "incr-EAS-ed", "RES" inside
# "the r-ES-ults" all falsely "corroborated" an unrelated identification under
# the old code) — the same class of bug fixed in .missing_label_re/
# .concept_is_rt elsewhere in this codebase. The acronym is therefore matched
# with \b...\b word-boundary anchoring, mirroring .scale_text_pattern()'s
# already-correct acronym handling (used a few lines above for the paper-wide
# regex scan), instead of joining the other needles' `fixed = TRUE` search.
.scale_name_in_text <- function(scale, sentences) {
  if (!nzchar(scale) || length(sentences) == 0) return(FALSE)
  hay <- tolower(paste(sentences, collapse = " \n "))
  # The full name (with and without a parenthesised acronym suffix).
  name_needles <- unique(tolower(c(scale, sub("\\s*\\(.*\\)\\s*$", "", scale))))
  name_needles <- name_needles[nchar(name_needles) >= 3]
  if (any(vapply(name_needles, function(n) grepl(n, hay, fixed = TRUE), logical(1))))
    return(TRUE)
  # Any ALL-CAPS acronym in parentheses (e.g. "(PANAS)"), word-boundary matched.
  acr <- regmatches(scale, gregexpr("\\(([A-Z][A-Za-z0-9-]{1,})\\)", scale))[[1]]
  acr <- unique(tolower(gsub("[()]", "", acr)))
  acr <- acr[nchar(acr) >= 2]
  if (!length(acr)) return(FALSE)
  pat <- paste0("\\b(", paste(gsub("([][{}().^$*+?|\\\\-])", "\\\\\\1", acr),
                              collapse = "|"), ")\\b")
  grepl(pat, hay, perl = TRUE)
}

# Propagate a named scale across same-file, same-prefix sibling columns. The
# LLM (or the rules namer) often names a block but echoes back only some of its
# item columns, leaving a few unnamed; and a codebook match may cover only part
# of a family. For each source_file, every unnamed column whose name-prefix
# matches a NAMED column's prefix inherits that column's scale + confidence
# (verbatim — same prefix, same instrument, same block). Only fills empty cells,
# never overwrites; when two named prefixes would both apply (they cannot, a
# prefix is unique per column) the first is used. Deterministic, no LLM.
.propagate_scale_by_prefix <- function(labels_df) {
  if (is.null(labels_df) || !nrow(labels_df) ||
      !all(c("source_file", "column_name", "scale") %in% names(labels_df)))
    return(labels_df)
  named <- !is.na(labels_df$scale) & nzchar(labels_df$scale)
  if (!any(named)) return(labels_df)
  has_source <- "scale_source" %in% names(labels_df)
  pref <- .scale_name_prefix(labels_df$column_name)
  for (f in unique(labels_df$source_file[named])) {
    in_f <- labels_df$source_file == f
    # prefix -> (scale, confidence, source) from this file's named columns
    nk <- in_f & named
    key <- pref[nk]
    # first named occurrence per prefix
    first <- !duplicated(key)
    map_scale <- stats::setNames(labels_df$scale[nk][first], key[first])
    map_conf  <- stats::setNames(labels_df$scale_confidence[nk][first], key[first])
    map_src   <- if (has_source)
      stats::setNames(labels_df$scale_source[nk][first], key[first]) else NULL
    # fill unnamed columns in this file whose prefix is a named one
    fill <- in_f & !named & pref %in% names(map_scale)
    if (any(fill)) {
      labels_df$scale[fill]            <- unname(map_scale[pref[fill]])
      labels_df$scale_confidence[fill] <- unname(map_conf[pref[fill]])
      if (has_source)
        labels_df$scale_source[fill]   <- unname(map_src[pref[fill]])
    }
  }
  labels_df
}

# Rules-only scale namer (no LLM), against the `scales` dictionary. The DATA
# proposes: a detected Likert block's shared name-prefix is matched to the
# dictionary's acronyms/names; the codebook item wording and the paper text then
# CONFIRM or DISAMBIGUATE the candidate(s). Conservative by design — a wrong
# scale label is worse than a missing one.
#
# For each block:
#   1. Propose candidates whose acronym (or a name token) equals the block prefix.
#   2. Corroborate each candidate: does the instrument's name/acronym appear in
#      (a) the codebook item wording of these columns [checked first], or
#      (b) the paper text?
#   3. Resolve:
#        * exactly one candidate corroborated  -> name it, confidence "high".
#        * one candidate, no corroboration, but the prefix is a "safe" acronym
#          (>= 4 chars) -> name it, confidence "medium" (data-only).
#        * zero candidates, or >= 2 candidates and not exactly one corroborated,
#          or a single short-acronym candidate with no corroboration -> ABSTAIN.
# Returns (source_file, column_name, scale, confidence, scale_source="matched"),
# with an "n_detected" attribute (files with scale-like blocks).
.identify_scales_rules <- function(previews, labels_df, paper) {
  empty <- structure(
    data.frame(source_file = character(), column_name = character(),
               scale = character(), confidence = character(),
               scale_source = character()),
    n_detected = 0L)
  if (is.null(previews) || !length(previews)) return(empty)
  dict <- .scale_dictionary()
  if (nrow(dict) == 0) return(empty)

  # Fast prefix -> candidate dictionary rows. Key on the acronym AND on the
  # first content token of the name, both normalized like a variable prefix.
  norm_pref <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
  dict$.akey <- norm_pref(dict$acronym)
  cand_for_prefix <- function(pfx) {
    p <- norm_pref(pfx)
    if (!nzchar(p)) return(integer(0))
    which(dict$.akey == p)          # acronym-prefix match (collisions -> several)
  }

  # Per-column codebook wording (label + question), for corroboration.
  lbl_key <- paste(labels_df$source_file, labels_df$column_name, sep = "\x01")
  wording_of <- function(file, cols) {
    i <- match(paste(file, cols, sep = "\x01"), lbl_key)
    i <- i[!is.na(i)]
    if (!length(i)) return("")
    parts <- c(labels_df$label[i],
               if ("question" %in% names(labels_df)) labels_df$question[i])
    paste(parts[!is.na(parts) & nzchar(parts)], collapse = " ")
  }

  have_paper <- .is_paper(paper) && !is.null(paper$text) && nrow(paper$text) > 0
  paper_hay  <- if (have_paper)
    paste(as.character(paper$text$text), collapse = " \n ") else ""

  # Does instrument `i`'s FULL NAME appear in `hay`? Corroboration deliberately
  # requires the full name, NOT the bare acronym: an acronym like "BES" in a
  # paper may stand for a different instrument (Breakup Emotions Scale, not the
  # dictionary's Binge Eating Scale), so a bare-acronym hit must never confirm a
  # dictionary expansion. (The manuscript-first prefix matcher handles the
  # acronym-defined-in-text case correctly; this rules fallback stays strict.)
  corroborates <- function(i, hay) {
    if (!nzchar(hay)) return(FALSE)
    pat <- .scale_text_pattern(dict$name[i], NA_character_)   # name only
    isTRUE(tryCatch(grepl(pat, hay, perl = TRUE, ignore.case = TRUE),
                    error = function(e) FALSE))
  }

  # How many of this block's items are the reference instrument's OWN items,
  # matched by wording? A signal INDEPENDENT of the name: the name test asks
  # "does the paper mention the PANAS?", this asks "are these actually the
  # PANAS's items?". Used only to break a tie the name test could not (several
  # candidates corroborated, or an acronym collision like AQ), never to make a
  # match the name test rejected. 0 when the codebook records no item wording,
  # which is the common case and correctly contributes nothing.
  n_items_matched <- function(i, file, cols) {
    ref <- .scale_reference(.scale_ref_code(dict$name[i], dict))
    if (is.null(ref)) return(0L)
    w <- stats::setNames(vapply(cols, function(c) {
      j <- match(paste(file, c, sep = "\x01"), lbl_key)
      if (is.na(j)) return(NA_character_)
      parts <- c(labels_df$label[j],
                 if ("question" %in% names(labels_df)) labels_df$question[j])
      parts <- parts[!is.na(parts) & nzchar(parts)]
      if (!length(parts)) NA_character_ else parts[1]
    }, character(1)), cols)
    m <- .scale_match_items(w, ref)
    if (is.null(m)) 0L else sum(!is.na(m$ref_item_id))
  }

  out <- list(); n_detected <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) < .scale_min_items) next
    blocks <- .detect_scale_blocks(df)
    if (length(blocks) == 0) next
    n_detected <- n_detected + 1L

    for (cols in blocks) {
      nms    <- names(df)[cols]
      prefix <- .scale_name_prefix(nms[[1]])
      cand   <- cand_for_prefix(prefix)
      if (length(cand) == 0) next               # data proposes nothing

      cb_hay <- wording_of(file, nms)           # codebook wording FIRST
      # Corroborate each candidate: codebook, then paper.
      cb_ok <- vapply(cand, corroborates, logical(1), hay = cb_hay)
      pp_ok <- vapply(cand, corroborates, logical(1), hay = paper_hay)
      corr  <- cb_ok | pp_ok

      pick <- NA_integer_; conf <- NA_character_
      if (sum(corr) == 1L) {
        pick <- cand[corr]; conf <- "high"      # unambiguous, text-confirmed
      } else if (sum(corr) > 1L) {
        # Several candidates named in the text (an acronym collision: AQ =
        # Autism Spectrum Quotient AND Aggression Questionnaire). The name test
        # cannot separate them. Item wording can: whichever candidate's OWN
        # items are in this block is the one administered. Requires a clear
        # winner on >= 2 matched items — a single shared item is the kind of
        # generic wording ("I feel calm") that two instruments both contain.
        hits <- vapply(cand[corr], n_items_matched, integer(1),
                       file = file, cols = nms)
        if (max(hits) >= 2L && sum(hits == max(hits)) == 1L) {
          pick <- cand[corr][which.max(hits)]
          conf <- "high"                        # confirmed by the items themselves
        }
      } else if (sum(corr) == 0L && length(cand) == 1L &&
                 nchar(norm_pref(dict$acronym[cand])) >= 4L) {
        pick <- cand; conf <- "medium"          # data-only, safe acronym
      }
      # else: 0 candidates left, several still tied on item evidence, or a lone
      # short-acronym with no corroboration -> ABSTAIN.

      if (!is.na(pick))
        out[[length(out) + 1L]] <- data.frame(
          source_file = file, column_name = nms,
          scale = dict$name[pick], confidence = conf,
          scale_source = "matched")
    }
  }

  res <- if (length(out)) dplyr::bind_rows(out) else empty
  attr(res, "n_detected") <- n_detected
  res
}

# ── Task identification (data side) ───────────────────────────────────────────
# The scale matcher keys on a Likert block sharing a name prefix. A behavioural
# task produces nothing of the kind: its data is either one row per TRIAL
# (subject, trial, condition, rt, correct) or one aggregated column per
# condition (stroop_rt_congruent), and reaction times fail the Likert gate by
# construction. So tasks need their own matcher over the same contract:
# per-column rows of (source_file, column_name, scale, confidence,
# scale_source) that `apply_scale()` folds into labels_df.
#
# Evidence is combined from three places, and a task is named only when the DATA
# says "a task is here" AND the NAME is corroborated:
#   * the data: an rt/accuracy column, or a block of 0/1 accuracy items
#     (.detect_task_columns / .detect_accuracy_blocks in data_check_helpers.R)
#   * the column names: a task acronym or name token as a prefix (iat_*, stroop_*)
#   * the paper text: the task's full name, never a bare acronym
#
# Abstention is cheap and deliberate. A task that goes unrecognised costs a
# report line; a task named wrongly asserts that a paper ran a paradigm it did
# not. When the evidence does not converge, no name is emitted and the columns
# are still reported as task-like but unnamed.
.identify_tasks_rules <- function(previews, labels_df, paper) {
  empty <- structure(
    data.frame(source_file = character(), column_name = character(),
               scale = character(), confidence = character(),
               scale_source = character()),
    n_detected = 0L)
  if (is.null(previews) || !length(previews)) return(empty)
  dict <- .task_dictionary()
  if (nrow(dict) == 0) return(empty)

  norm_pref <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
  dict$.akey <- norm_pref(dict$acronym)
  # Also key on the first content token of the name, so a `stroop_*` prefix
  # reaches "color-word stroop task" even though its acronym is empty.
  first_tok <- function(nm) {
    toks <- unlist(strsplit(tolower(nm), "[^a-z0-9]+"))
    toks <- toks[nzchar(toks) & !(toks %in% c("the","a","an","of","task","test"))]
    if (length(toks)) toks[1] else ""
  }
  dict$.nkey <- vapply(dict$name, first_tok, character(1))

  have_paper <- .is_paper(paper) && !is.null(paper$text) && nrow(paper$text) > 0
  paper_hay  <- if (have_paper)
    paste(as.character(paper$text$text), collapse = " \n ") else ""

  # Corroboration requires the FULL NAME in the text, never a bare acronym —
  # the same rule the scale matcher uses, and for the same reason: "IAT" and
  # "CRT" are ambiguous outside their context.
  corroborates <- function(i) {
    if (!nzchar(paper_hay)) return(FALSE)
    pat <- .scale_text_pattern(dict$name[i], NA_character_)
    isTRUE(tryCatch(grepl(pat, paper_hay, perl = TRUE, ignore.case = TRUE),
                    error = function(e) FALSE))
  }

  out <- list(); n_detected <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || !is.data.frame(df) || !ncol(df)) next
    if (!.is_task_data(df)) next          # no rt / accuracy signal in this file
    n_detected <- n_detected + 1L

    task_cols <- .detect_task_columns(df)
    acc_blocks <- .detect_accuracy_blocks(df)

    # Candidate column groups: each rt/accuracy column on its own, plus each
    # accuracy block as a unit. A group's prefix is what we try to name.
    groups <- list()
    for (k in seq_len(nrow(task_cols)))
      groups[[length(groups) + 1L]] <- task_cols$column_name[k]
    for (b in acc_blocks)
      groups[[length(groups) + 1L]] <- names(df)[b]

    for (cols in groups) {
      # Strip the rt/acc/condition marker to leave the task's own prefix:
      # "stroop_rt_congruent" -> "stroop", "raven_1" -> "raven".
      pfx <- .scale_name_prefix(cols[[1]])
      pfx <- sub(paste0("[._ -]?(", "rt|reaction[._ -]?time|response[._ -]?time|",
                        "latency|acc|accuracy|correct|error|hit|miss",
                        ")([._ -].*)?$"), "", pfx, perl = TRUE)
      pfx <- gsub("[._ -]+$", "", pfx)
      p <- norm_pref(pfx)
      if (!nzchar(p) || nchar(p) < 3L) next   # "rt" alone names no task

      cand <- which(dict$.akey == p | norm_pref(dict$.nkey) == p)
      if (!length(cand)) next

      corr <- vapply(cand, corroborates, logical(1))
      pick <- NA_integer_; conf <- NA_character_
      if (sum(corr) == 1L) {
        pick <- cand[corr]; conf <- "high"     # data + text agree
      } else if (sum(corr) > 1L) {
        # Several task names corroborated by one prefix (8 Stroop variants, 8
        # n-backs). The text cannot separate them and neither can the columns,
        # so abstain rather than pick the alphabetically-first paradigm.
        next
      } else if (length(cand) == 1L && nchar(p) >= 4L) {
        pick <- cand; conf <- "medium"         # data-only, distinctive prefix
      }
      if (is.na(pick)) next

      out[[length(out) + 1L]] <- data.frame(
        source_file = file, column_name = cols,
        scale = dict$name[pick], confidence = conf,
        scale_source = "task_matched")
    }
  }

  res <- if (length(out)) dplyr::bind_rows(out) else empty
  attr(res, "n_detected") <- n_detected
  res
}

# ── Prefix-group scale pipeline ───────────────────────────────────────────────
# A dataset's own COLUMN NAMES are the most reliable scale signal available: a
# run of columns sharing a leading abbreviation (BSQ_*, MBSC_*, BDS_*, DEM_*) is
# almost always one instrument, and the paper text names what the abbreviation
# stands for. This pipeline is manuscript-first and dictionary-independent:
#   1. group columns by leading abbreviation (any consistent PREFIX_*, not just
#      Likert blocks) -> candidate scales, with column count + max item number;
#   2. search the paper for each abbreviation (text_search) -> sentences;
#   3. ONE LLM call maps groups -> named scales from those sentences (with the
#      dictionary offered only as a hint). Unmatched groups are KEPT, unnamed.
# This fixes the failure where BES_* (Breakup Emotions Scale, named in the paper)
# was mislabelled "Binge Eating Scale" by a bare-acronym dictionary match.

# The leading abbreviation of a column name: the first token before an
# underscore/dot/dash/space, or a trailing-number split when there is no
# separator (Q1 -> Q, panas1 -> panas). Lower-cased for grouping; the display
# form keeps the original case of the first column.
.scale_prefix_of <- function(nm) {
  x <- trimws(nm)
  # token before the first separator (underscore, dot, dash or space)
  seg <- sub("[_. -].*$", "", x)
  if (identical(seg, x)) {
    # no separator: strip a trailing number (panas1 -> panas, Q1 -> Q)
    seg <- sub("[0-9]+$", "", x)
  }
  seg
}

# Group a data file's columns into scale blocks by the SHARED LEADING STRING that
# ADJACENT columns have in common — not by guessing each column's prefix in
# isolation. This recognises a block however its items are named: neoNeuroticism
# /neoAnxiety/neoAnger (camelCase word suffix), PANASPositive/PANASNegative
# (all-caps + word), bfi_1/bfi_2 (separator + number), panas1/panas2 (number),
# all collapse to their common stem (neo, PANAS, bfi, panas). The old approach
# only worked when a per-column rule (cut at separator / strip trailing digits)
# happened to extract the same stem, so it missed prefix+word conventions.
#
# Two guards keep the shared-stem idea from over-grouping:
#   * ADJACENCY — a block is a RUN of consecutive columns. Real item batteries
#     are exported in order, so two different instruments that merely share their
#     first characters (panasX at col 3, panicY at col 80) cannot merge.
#   * SEPARATOR BREAK — when the shared stem contains a separator (_ . - space),
#     the stem ends at the first separator (bfi_1/bfi_2 -> bfi, not bfi_), which
#     also prevents panas_* and panic_* running past their "panas"/"panic" stems.
# The residual case (two adjacent instruments named with no separator and sharing
# >= min_chars, e.g. panasPos1 next to panicFreq) is accepted as vanishingly rare.
.scale_group_min_chars <- 3L

# Leading stem shared by two names: the full common leading run, with only a
# TRAILING separator trimmed (the item delimiter). We must NOT cut at the FIRST
# separator: a stem like "Q1.RR_P_" (shared by Q1.RR_P_1/Q1.RR_P_2) has its first
# separator right after "Q1", and cutting there would collapse every Q1.* column
# into one "Q1" blob and break real subscales. Trailing-only trim gives
# "Q1.RR_P_" -> "Q1.RR_P" and "bfi_" -> "bfi", both correct.
.scale_shared_stem <- function(a, b) {
  n <- min(nchar(a), nchar(b))
  if (n == 0) return("")
  ca <- substr(a, 1, n); cb <- substr(b, 1, n)
  k <- 0L
  for (i in seq_len(n)) {
    if (substr(ca, i, i) != substr(cb, i, i)) break
    k <- i
  }
  if (k == 0L) return("")
  sub("[^A-Za-z0-9]+$", "", substr(a, 1, k))
}

# Alphabetic scale prefix: the leading run of a name up to (not including) the
# first digit, with a trailing separator trimmed. Used to MERGE adjacent runs
# that a zero-padded numbering split into two stems: AQ01..AQ09 share stem "AQ0"
# and AQ10..AQ20 share "AQ1", but both have alpha-prefix "AQ", so they are one
# scale. Sub-scale names that carry a word segment (CRS_EXP, CRS_IDE) keep that
# segment because it is alphabetic, so those stay correctly separate.
.scale_alpha_prefix <- function(nm) {
  p <- sub("[0-9].*$", "", nm)        # cut at the first digit
  p <- sub("[._ -]+$", "", p)         # trim a trailing separator
  tolower(p)
}

# Collapse a loop/stimulus index out of a block stem, so the SAME instrument
# repeated across many stimuli or loop iterations maps to one canonical stem.
# A wide survey that shows one matrix for N stimuli exports N stem families that
# differ only by an embedded index: POWER.PP1, POWER.PP2, ... POWER.PP170 (or
# Q3_1_, Q3_2_, ...). Each is the SAME questionnaire, so they should be one
# block, not 170. This strips a trailing `<sep><letters><digits>` or `<sep><digits>`
# index token (`.PP170`, `_2`, `-stim12`) from the stem's tail, returning the
# instrument base (`power`, `q3`). Returns the lowercased stem unchanged when no
# such trailing index is present, so a stem that is a plain instrument name
# (`PANAS`, `BFI`) is never altered. The alphabetic base must remain >= 2 letters,
# so we never collapse a stem down to nothing.
.scale_loop_base <- function(stem) {
  s <- tolower(stem)
  # peel a trailing index token: optional separator, optional letters, digits.
  base <- sub("[._ -]?[a-z]*[0-9]+$", "", s, perl = TRUE)
  base <- sub("[._ -]+$", "", base)
  # only accept the collapse when a real alphabetic base survives.
  if (sum(grepl("[a-z]", strsplit(base, "")[[1]])) >= 2) base else s
}

# Do two adjacent columns belong to the SAME instrument/subscale, i.e. do they
# differ ONLY by item number? True when, after their shared leading stem, BOTH
# remainders are just an item number (optional separator + digits, to the end).
# This keeps a real numbering run together (matwarmth1/matwarmth2, AQ01/AQ10 ->
# same) while SPLITTING two different word-stems that merely share leading letters
# (matwarmth6/mataggr1 -> different constructs; neighdev/neighcur -> two
# subscales; Q1.RR_P_1/Q1.SS_X_2 -> different subscales). A remainder that still
# contains a LETTER means a different word segment, so the columns are NOT the
# same construct and the run must break.
.scale_same_number_run <- function(a, b) {
  stem <- .scale_shared_stem(a, b)
  if (!nzchar(stem)) return(FALSE)
  ra <- substring(a, nchar(stem) + 1L)
  rb <- substring(b, nchar(stem) + 1L)
  num_only <- function(r) grepl("^[._ -]?[0-9]*$", r)   # sep? + digits, to end
  num_only(ra) && num_only(rb)
}

# Within one prefix block, split the columns into genuine scale ITEMS and DERIVED
# columns (a summed/averaged score, a mean, an attention check) that authors
# named with the same prefix but that are NOT items. Two independent signals,
# assessed relative to the block's own majority:
#
#   1. NAMING ANOMALY. When most columns end in a number (the item-numbering
#      convention, AQ01..AQ20), a column that does NOT — i.e. it ends in a WORD
#      (AQ_SUM, AQ_CHILD, CRT_Check) — breaks the pattern and is suspect. A word
#      suffix matching a known aggregate token (sum/mean/total/score/avg/index/
#      check/count) is treated as derived outright, even without the number test.
#   2. VALUE ANOMALY, from the preview values: relative to the item columns, a
#      column is derived when it is NON-INTEGER while the items are integer (a
#      MEAN like 4.3 among 1-5 ratings), or its observed range is much wider than
#      the block's typical item range (a 0-7 SUM among 0-1 items).
#
# Returns list(items = <col names>, derived = <col names>, totals_only = <lgl>).
# A block with too few columns, or where the split would leave < min_items items,
# is left intact (all columns treated as items) to avoid over-pruning a small
# genuine scale. `totals_only` is TRUE when the block has NO genuine item columns
# — every (or nearly every) column looks like an aggregated total/average — so it
# is an orphan score block, not an item battery (e.g. IERQ_pos/persp/sooth/model,
# four subscale totals with no items shared).
.AGG_TOKEN_RE <- "(sum|mean|total|score|avg|average|index|composite|check|count)$"

# Does one column, on its own values, look like a TOTAL or AVERAGE rather than a
# raw rating item? Two absolute signals (no item block needed to compare against):
#   * AVERAGE-like: it carries non-integer values (4.666, 4.25) — a raw Likert
#     item is integer, a mean is not.
#   * TOTAL-like: its observed spread (max - min) exceeds 10 — wider than any
#     plausible single rating item, i.e. a summed score.
# `st` is list(rng, int) as computed in .scale_split_items; NULL (no data) -> NA.
.col_looks_like_total <- function(st) {
  if (is.null(st)) return(NA)
  (!isTRUE(st$int)) || (is.finite(st$rng) && st$rng > 10)
}

.scale_split_items <- function(cols, df = NULL, min_items = .scale_min_items) {
  keep_all <- function(totals_only = FALSE)
    list(items = cols, derived = character(0), totals_only = totals_only)

  # Reader-invented placeholder names (…N, STEM…N, V1, X.1, col_N) are NOT scale
  # items — they are what read.csv/readxl fabricate for blank/duplicate headers
  # (e.g. a "CDA" banner row spread into CDA…4 … CDA…113). Drop them BEFORE any
  # item logic, so a block made of them loses its items and never becomes a scale.
  # Uses the same .is_placeholder_name() as the read-time header repair, so the two
  # cannot disagree about what counts as a fabricated name.
  cols <- cols[!.is_placeholder_name(cols)]
  if (length(cols) < min_items) return(keep_all())

  suffix <- sub("^.*?[._ -]", "", cols)            # segment after first separator
  ends_num  <- grepl("[0-9]$", cols)
  is_word   <- grepl("[A-Za-z]$", cols)
  agg_token <- grepl(.AGG_TOKEN_RE, tolower(suffix)) | grepl(.AGG_TOKEN_RE, tolower(cols))

  # Naming anomaly only fires when a clear numbering convention exists: the
  # MAJORITY of columns end in a number. Then word-suffix columns are anomalies.
  numbered_block <- mean(ends_num) >= 0.5
  name_flag <- agg_token | (numbered_block & is_word & !ends_num)

  # Value anomaly, when preview values are available for these columns.
  value_flag <- rep(FALSE, length(cols))
  abs_total  <- rep(NA, length(cols))              # per-column absolute total test
  if (!is.null(df) && all(cols %in% names(df))) {
    numify <- function(x) suppressWarnings(as.numeric(as.character(x)))
    stats <- lapply(cols, function(c) {
      v <- numify(df[[c]]); v <- v[is.finite(v)]
      if (!length(v)) return(NULL)
      list(rng = diff(range(v)), int = all(v == round(v)))
    })
    abs_total <- vapply(stats, .col_looks_like_total, logical(1))
    item_idx <- which(!name_flag)                  # provisional items set the norm
    if (length(item_idx) >= 2) {
      item_rngs <- vapply(item_idx, function(i)
        if (is.null(stats[[i]])) NA_real_ else stats[[i]]$rng, numeric(1))
      item_int  <- vapply(item_idx, function(i)
        if (is.null(stats[[i]])) NA else stats[[i]]$int, logical(1))
      med_rng   <- stats::median(item_rngs, na.rm = TRUE)
      items_are_integer <- isTRUE(mean(item_int, na.rm = TRUE) >= 0.5)
      for (j in seq_along(cols)) {
        st <- stats[[j]]; if (is.null(st)) next
        wider   <- is.finite(med_rng) && med_rng > 0 && st$rng > 1.5 * med_rng
        non_int <- items_are_integer && !st$int
        if (wider || non_int) value_flag[j] <- TRUE
      }
    }
  }

  # totals-only: after removing name-flagged aggregates, do the REMAINING columns
  # still all look like totals/averages? Then there is no item battery here — the
  # whole block is orphan score columns. Requires the value evidence (abs_total)
  # to be present and to cover >= 80% of the remaining columns.
  rest <- which(!name_flag)
  totals_only <- FALSE
  if (length(rest)) {
    at <- abs_total[rest]
    if (all(!is.na(at)) && mean(at) >= 0.8) totals_only <- TRUE
  }

  derived_mask <- name_flag | value_flag
  # Never strip so much that a real scale falls below the item minimum: if the
  # remaining items are too few, keep everything (better a slightly noisy block
  # than a dropped scale). The totals_only verdict still rides along, so a block
  # of all-totals is flagged even though its columns are kept.
  if (sum(!derived_mask) < min_items)
    return(keep_all(totals_only = totals_only))
  list(items = cols[!derived_mask], derived = cols[derived_mask],
       totals_only = totals_only)
}

# .qualtrics_col_stem() lives in R/data_check_helpers.R so it is in the package
# namespace: match_column_labels() (an R/ function) needs it for the Qualtrics
# stem fallback, and R/ functions are not in scope for module-only definitions.

# Build the QSF/Qualtrics arguments for .scale_prefix_groups() for one file: the
# authoritative column -> block-stem map from a parsed .qsf (labels_df$scale_group
# scoped to this file), and whether the data frame is a Qualtrics export (used
# only when no .qsf map is available). Returns list(scale_group=, qualtrics=).
.scale_group_args <- function(df, file, labels_df) {
  sg <- NULL
  if (!is.null(labels_df) && "scale_group" %in% names(labels_df) &&
      all(c("source_file", "column_name") %in% names(labels_df))) {
    fr <- labels_df[!is.na(labels_df$scale_group) & nzchar(labels_df$scale_group) &
                      labels_df$source_file == file, , drop = FALSE]
    if (nrow(fr) > 0)
      sg <- stats::setNames(fr$scale_group, fr$column_name)
  }
  list(scale_group = sg,
       qualtrics = is.null(sg) && data_check_is_qualtrics(df))
}

# `scale_group`: an optional named character vector (column name -> authoritative
# block stem, from a parsed .qsf via labels_df$scale_group). When supplied, those
# stems define blocks directly (the reliable signal), and the character-run
# heuristic is applied only to the columns no stem claims. `qualtrics`: when TRUE
# and no scale_group is given, blocks are recovered from the Qualtrics <stem>_<N>
# export-naming convention (.qualtrics_col_stem) before the heuristic runs.
# Paradata channel tokens: trial-level metadata (response times, trial/stimulus/
# option channels) that some formats (Behaverse wide pivots, Qualtrics timing)
# attach to every item column. These are NOT scale items — they are recognised
# here and EXCLUDED from scale grouping so they do not become junk "scales". The
# data is not discarded: it is routed to Behaverse `trial` paradata files (see
# R/behaverse-convert.R). Matched as whole, delimiter-bounded segments so an
# answer channel (response_numeric) and construct words are never caught.
.PARADATA_CHANNEL_RE <- paste0(
  "(^|[_. -])(",
  "trial_index|stimulus_type|stimulus_description|response_time|",
  "response_validation_time|validation_time|response_option_index|",
  "response_description|first[_ ]?click|last[_ ]?click|page[_ ]?submit|",
  "click[_ ]?count|timing|timer|reaction[_ ]?time",
  ")([_. -]|$)")

# Qualtrics PAGE-TIMER columns. When a survey has a Timing question, Qualtrics
# exports four fixed channels per timed page — First Click, Last Click, Page
# Submit, Click Count — and GLUES the "timing" stem straight onto the item name
# with no delimiter, in dot-separated CamelCase: `eraitem16timing_First.Click`,
# `Q8timing_Page.Submit`. The delimiter-bounded .PARADATA_CHANNEL_RE misses
# these because there is no boundary before "timing" and the two-word channels
# are dot-joined (First.Click), not underscore/space. This closed, unambiguous
# Qualtrics vocabulary is matched here regardless of the preceding character, so
# a real item does not drag its three/four timer columns into a "scale". The
# channels are only ever timer metadata, so relaxing the left boundary for them
# cannot swallow a substantive item.
.QUALTRICS_TIMER_RE <- paste0(
  # The four page-timer channels, dot/underscore/space joined, appearing anywhere
  # in the name. Qualtrics attaches them under a `time`/`timing` stem that is
  # itself glued to the item name (`demo1time_First.Click`,
  # `eraitem16timing_Page.Submit`), so the channel words are the reliable anchor.
  "(first[_. -]?click|last[_. -]?click|page[_. -]?submit|click[_. -]?count)([_. -]|$)|",
  # The `time`/`timing` timer stem when immediately followed by a channel word,
  # OR a delimiter-terminated `timing`. NOT a bare `time`: that is a legitimate
  # substantive variable (`sleep_time`, `time_spent`), and only the `timing`
  # spelling or a following channel word is an unambiguous Qualtrics timer.
  "tim(e|ing)[_. -]*(first|last|page|click)|timing([_. -]|$)")

# Is a column name a paradata channel (not a scale item)? The Behaverse ANSWER
# channel `response_numeric` is deliberately NOT matched, so real items survive.
.scale_is_paradata_col <- function(nm) {
  x <- tolower(nm)
  (grepl(.PARADATA_CHANNEL_RE, x, perl = TRUE) |
     grepl(.QUALTRICS_TIMER_RE, x, perl = TRUE)) &
    !grepl("response_numeric", x, fixed = TRUE)
}

# Is each column of `df` non-analytic survey/export MACHINERY rather than a
# substantive measurement — so it must be kept out of SCALE DETECTION (and hence
# the LLM), though it is still DESCRIBED elsewhere in the inventory? Covers:
#   * paradata channels (response/validation times, click/timing channels);
#   * reserved survey-platform metadata (StartDate, Duration, ResponseId, ...);
#   * Qualtrics display-order/randomisation columns (`<Q>_DO_<...>`);
#   * free-text-entry overflow columns (`*_TEXT`);
#   * trial-level task machinery (jsPsych browser/media/geometry diagnostics,
#     Inquisit stimulus geometry / pauses / timeouts, ...) via Behaverse's
#     per-platform substantive-column vocabulary (.bh_is_machinery_col), applied
#     only when the file is DETECTED as that platform so a survey column named
#     "browser" or "response" elsewhere is never touched.
# This is the "not everything in a data file is data" filter: an instrument is
# never named from these, so a wide export does not spend one LLM group per
# housekeeping column. Side-effect free; it removes columns from grouping only.
.scale_is_nonanalytic_col <- function(df) {
  nm <- names(df)
  out <- .scale_is_paradata_col(nm) |
    !is.na(.qualtrics_tag_cols(nm)) |          # reserved platform metadata
    .qualtrics_is_display_order(nm) |          # `_DO_` randomisation order
    grepl("_TEXT$", nm, perl = TRUE)           # free-text-entry overflow

  # Trial-level task machinery, gated on per-file platform detection.
  fmt <- if (data_check_is_jspsych(df)) "jspsych"
         else if (data_check_is_inquisit(df)) "inquisit"
         else if (data_check_is_psychopy(df)) "psychopy"
         else if (data_check_is_behaverse(df)) "behaverse"
         else NULL
  if (!is.null(fmt)) out <- out | .bh_is_machinery_col(nm, fmt)
  out
}

# Self-label export MACHINERY columns in labels_df so they are excluded from the
# codebook-matching LLM while still being fully described (Psych-DS compliant).
# For each source file present in `previews`, computes the machinery mask with
# the SAME rule scale detection uses (.scale_is_nonanalytic_col, which covers
# paradata, survey-platform metadata, display-order, and platform task
# housekeeping), and for every still-`unlabelled` machinery column sets a
# deterministic label. `label_status = "labelled"` removes it from the LLM tiers
# (which only see `unlabelled`/`conflicting_definition`); `label_method` records
# it was a rule, not the model. A column the codebook actually documented is left
# untouched. Files with no preview are skipped (nothing to detect from).
.codebook_label_machinery <- function(labels_df, previews) {
  if (is.null(labels_df) || nrow(labels_df) == 0 ||
      is.null(previews) || length(previews) == 0) return(labels_df)
  if (!all(c("source_file", "column_name", "label_status") %in% names(labels_df)))
    return(labels_df)

  for (f in intersect(unique(labels_df$source_file), names(previews))) {
    df <- previews[[f]]
    if (is.null(df) || ncol(df) == 0) next
    mask <- .scale_is_nonanalytic_col(df)
    machine_cols <- names(df)[mask]
    if (!length(machine_cols)) next
    rows <- labels_df$source_file == f &
      labels_df$column_name %in% machine_cols &
      labels_df$label_status == "unlabelled"
    if (!any(rows)) next
    labels_df$label[rows]        <- "Export machinery / paradata column (not a measured variable)."
    labels_df$label_status[rows] <- "labelled"
    labels_df$label_method[rows] <- "paradata_rule"
    if ("codebook_variable" %in% names(labels_df))
      labels_df$codebook_variable[rows] <- labels_df$column_name[rows]
  }
  labels_df
}

# Per-file warning for DUPLICATED column names. A survey export whose loop/merge
# iterations are not encoded into the column name repeats the same header many
# times (e.g. POWER.PP1_1 appearing 171×, once per stimulus). The reader keeps
# them, but they cannot be told apart, so a codebook can document the name only
# once and every analysis has to guess which repetition is which. This is a
# data-quality signal for the authors, not a metacheck error — so it is surfaced
# as a warning naming the worst-repeated columns, never a hard failure. Returns a
# character vector of warning lines (one per affected file), empty when none.
# `min_repeat` is the count at which a repeated name is worth flagging.
.codebook_duplicate_name_warnings <- function(previews, min_repeat = 2L) {
  if (is.null(previews) || length(previews) == 0) return(character(0))
  out <- character(0)
  for (f in names(previews)) {
    df <- previews[[f]]
    if (is.null(df) || ncol(df) == 0) next
    tab <- table(names(df))
    dup <- tab[tab >= min_repeat]
    if (!length(dup)) next
    dup <- sort(dup, decreasing = TRUE)
    ex  <- utils::head(names(dup), 5)
    out <- c(out, sprintf(
      "**Duplicated column names in `%s`.** %d column name%s appear%s more than once (worst: %s), so %d of the file's %d columns share a name with another. This usually means a survey loop/merge exported repeated blocks without numbering each iteration; the repeats cannot be told apart and can only be documented once. Consider re-exporting with per-iteration column names.",
      f, length(dup), plural(length(dup)), if (length(dup) == 1) "s" else "",
      paste(sprintf("`%s`×%d", ex, as.integer(dup[ex])), collapse = ", "),
      sum(as.integer(dup)), ncol(df)))
  }
  out
}

.scale_prefix_groups <- function(df, min_cols = .scale_min_items,
                                 min_chars = .scale_group_min_chars,
                                 scale_group = NULL, qualtrics = FALSE) {
  # Drop non-analytic MACHINERY columns before grouping — paradata channels,
  # reserved survey-platform metadata, Qualtrics display-order/randomisation, and
  # free-text overflow. None is a scale item, so none should reach scale naming
  # or the LLM; they are still described elsewhere in the inventory. This is what
  # stops a wide Qualtrics/Behaverse export's thousands of housekeeping columns
  # from becoming junk "scales" (or one LLM group each), and unsplits real
  # instruments the interleaved channels had fragmented.
  nonanalytic <- .scale_is_nonanalytic_col(df)
  if (any(nonanalytic)) df <- df[, !nonanalytic, drop = FALSE]
  all_nms <- names(df)
  if (length(all_nms) < min_cols) return(list())

  # Authoritative block claims: column -> block stem. From a parsed .qsf
  # (scale_group) when available, else the Qualtrics <stem>_<N> naming shape.
  # These columns are grouped by the claim and excluded from the character-run
  # heuristic below, which only sees the columns no claim covers.
  claim <- stats::setNames(rep(NA_character_, length(all_nms)), all_nms)
  if (!is.null(scale_group)) {
    hit <- intersect(all_nms, names(scale_group))
    claim[hit] <- as.character(scale_group[hit])
  } else if (isTRUE(qualtrics)) {
    claim[] <- vapply(all_nms, .qualtrics_col_stem, character(1))
  }
  # A claim is only kept when its stem covers at least min_cols columns here — a
  # lone claimed column is not a block, and should fall through to the heuristic.
  claimed_stems <- names(which(table(claim[!is.na(claim)]) >= min_cols))
  claim[!(claim %in% claimed_stems)] <- NA_character_

  # The heuristic walks only the UNCLAIMED columns, in original order.
  nms <- all_nms[is.na(claim[all_nms])]

  # Walk columns in order, extending a run while each next column shares a
  # >= min_chars stem with the run's stem. Emit a block when a run is long enough.
  # Pass 1: collect maximal runs of adjacent columns sharing a >= min_chars stem.
  runs <- list()
  push_run <- function(cols, stem) {
    if (length(cols)) runs[[length(runs) + 1L]] <<- list(cols = cols, stem = stem)
  }
  run_cols <- character(0); run_stem <- ""
  for (nm in nms) {
    if (is.na(nm) || !nzchar(nm)) { push_run(run_cols, run_stem); run_cols <- character(0); run_stem <- ""; next }
    if (length(run_cols) == 0) { run_cols <- nm; run_stem <- nm; next }
    stem <- .scale_shared_stem(run_stem, nm)
    # Extend only when the run shares a long-enough stem AND the new column
    # differs from the run's LAST column by item number alone (same word-stem).
    # The last-column comparison catches an alphabetic break at the boundary
    # (matwarmth6 -> mataggr1, neighdev7 -> neighcur1) that the progressively
    # shortened run stem would otherwise hide.
    same_word <- .scale_same_number_run(run_cols[[length(run_cols)]], nm)
    if (nchar(stem) >= min_chars && same_word) {
      run_cols <- c(run_cols, nm); run_stem <- stem
    } else {
      push_run(run_cols, run_stem)
      run_cols <- nm; run_stem <- nm
    }
  }
  push_run(run_cols, run_stem)

  # Pass 2: MERGE adjacent runs whose alphabetic prefix matches. Zero-padded
  # numbering splits one scale into two stems ("AQ0" for AQ01..AQ09, "AQ1" for
  # AQ10..AQ20); both share alpha-prefix "AQ", so they are one instrument. Merged
  # runs adopt the alpha-prefix as their stem so the block is named "AQ", not
  # "AQ0". Only NON-EMPTY alpha-prefixes merge (a purely numeric stem does not).
  merged <- list()
  for (r in runs) {
    ap <- .scale_alpha_prefix(r$cols[[1]])
    if (length(merged)) {
      last <- merged[[length(merged)]]
      if (nzchar(ap) && identical(ap, last$ap)) {
        last$cols <- c(last$cols, r$cols)
        merged[[length(merged)]] <- last
        next
      }
    }
    merged[[length(merged) + 1L]] <- list(cols = r$cols, stem = r$stem, ap = ap)
  }

  # Pass 2.5: COLLAPSE loop/stimulus repetitions. One instrument shown for many
  # stimuli exports many stem families differing only by an embedded index
  # (POWER.PP1, POWER.PP2, ... -> base "power"). Group the merged runs by their
  # loop-collapsed base and union each group's columns, so the repeated matrix
  # becomes ONE block instead of N. Only collapses when >= 2 runs share a base
  # AND that base is shorter than the stems (i.e. a real index was stripped), so
  # distinct instruments that merely share leading letters are never merged. The
  # kept display stem is the collapsed base (upper-cased from the first column).
  if (length(merged) > 1) {
    bases <- vapply(merged, function(m) .scale_loop_base(m$stem), character(1))
    collapsed <- list()
    for (b in unique(bases)) {
      grp <- merged[bases == b]
      # A collapse is real only if >= 2 runs share the base and the base is
      # genuinely shorter than the stems it came from (an index was removed).
      shortened <- any(nchar(b) < vapply(grp, function(m) nchar(tolower(m$stem)),
                                         integer(1)))
      if (length(grp) >= 2 && shortened) {
        cols <- unlist(lapply(grp, `[[`, "cols"), use.names = FALSE)
        disp <- toupper(substr(grp[[1]]$cols[[1]], 1, nchar(b)))
        collapsed[[length(collapsed) + 1L]] <-
          list(cols = cols, stem = disp, ap = .scale_alpha_prefix(disp))
      } else {
        for (m in grp) collapsed[[length(collapsed) + 1L]] <- m
      }
    }
    merged <- collapsed
  }

  out <- list()
  emit <- function(cols, stem, min_stem = min_chars) {
    if (length(cols) < min_cols || nchar(stem) < min_stem) return(invisible())
    # The stem must be a real variable-name prefix: it has to START WITH A LETTER
    # and contain at least two letters. This rejects placeholder / auto-named
    # columns that read.csv/readxl invent for BLANK headers ("...13", "...14" ->
    # stem "...1"), spreadsheet fillers ("X.1", "V1"), and pure-digit/punctuation
    # stems — none of which are scale items. Without this guard, a block of
    # unnamed columns is mistaken for a scale and (once an LLM names it) written
    # as a bogus scale definition.
    if (!grepl("^[A-Za-z]", stem) ||
        sum(grepl("[A-Za-z]", strsplit(stem, "")[[1]])) < 2) return(invisible())
    # Separate genuine items from derived/aggregate columns (AQ_SUM, CRS_MEAN,
    # CRT_Check) that share the prefix but are not items. Items drive naming and
    # likert_options; derived columns are recorded but not counted as items.
    split   <- .scale_split_items(cols, df, min_items = min_cols)
    items   <- split$items
    if (length(items) < min_cols) return(invisible())
    # A "totals-only" block (no genuine items, only aggregated scores) is only
    # flagged when the prefix is a plausible instrument abbreviation: >= 3 leading
    # letters. This guards against short coincidental prefixes (RT reaction times,
    # ID) whose wide/non-integer values are not scale totals.
    n_lead_letters <- nchar(sub("^([A-Za-z]+).*$", "\\1", stem))
    totals_only <- isTRUE(split$totals_only) && n_lead_letters >= 3L
    nums <- suppressWarnings(as.integer(sub(".*?([0-9]+)$", "\\1",
             grep("[0-9]+$", items, value = TRUE))))
    # Key per emitted run (not per stem): a stem that recurs in a second,
    # non-adjacent run must NOT overwrite the first block. Suffix on collision.
    key <- tolower(stem); base <- key; i <- 2L
    while (!is.null(out[[key]])) { key <- paste0(base, "#", i); i <- i + 1L }
    out[[key]] <<- list(
      prefix      = base,               # the stem (shared by same-name runs)
      display     = stem,               # original-case shared stem
      columns     = items,              # genuine items only
      derived     = split$derived,      # aggregate/score/check columns excluded
      totals_only = totals_only,        # block is orphan totals, no items found
      n_columns   = length(items),
      max_item    = if (length(nums)) max(nums, na.rm = TRUE) else NA_integer_)
  }

  for (m in merged) {
    # A merged block is displayed by its alpha-prefix (AQ); an unmerged run keeps
    # its shared stem. A genuine alpha-prefix from numbered items (AQ, RSE) may be
    # only 2 chars, so relax the stem-length floor to 2 for it; the "at least two
    # letters" guard in emit() still rejects junk stems.
    if (nzchar(m$ap) && grepl("^[A-Za-z]{2,}$", m$ap)) {
      stem <- toupper(substr(m$cols[[1]], 1, nchar(m$ap)))
      emit(m$cols, stem, min_stem = 2L)
    } else {
      emit(m$cols, m$stem)
    }
  }

  # Authoritative claimed blocks (QSF / Qualtrics), emitted through the SAME
  # emit() so item/derived splitting and the stem guards apply identically. Each
  # claimed stem's columns are kept in their data-frame order. min_stem = 2 so a
  # short-but-real export tag (e.g. "AQ") is not rejected on length alone.
  for (stem in claimed_stems) {
    cols <- all_nms[!is.na(claim[all_nms]) & claim[all_nms] == stem]
    emit(cols, stem, min_stem = 2L)
  }
  out
}

# Retrieve paper sentences mentioning an abbreviation (whole-word) or its
# spaced-out form, via metacheck's text_search. Returns unique sentences.
.scale_prefix_sentences <- function(paper, prefixes, max_sent = 40L) {
  if (!.is_paper(paper) || is.null(paper$text) || nrow(paper$text) == 0)
    return(character(0))
  pfx <- unique(prefixes[nzchar(prefixes)])
  if (length(pfx) == 0) return(character(0))
  esc <- function(x) gsub("([][{}().^$*+?|\\\\-])", "\\\\\\1", x)
  pats <- sprintf("\\b%s\\b", esc(pfx))
  sents <- tryCatch(
    text_search(paper, pattern = pats, return = "sentence",
                ignore.case = TRUE, perl = TRUE),
    error = function(e) NULL)
  if (is.null(sents) || !nrow(sents)) return(character(0))
  s <- unique(trimws(as.character(sents$text)))
  utils::head(s[nzchar(s)], max_sent)
}

# Words that signal a sentence is DESCRIBING a measurement instrument. Retrieving
# on these (as well as on the column prefix) surfaces the manuscript's own scale
# descriptions even when the abbreviation the authors used differs from the data
# column names — e.g. a 98-column block named "Response.*" whose instrument the
# text calls the "Teleological Beliefs Scale (TBS)". Without this, the sentence
# that says "the TBS contains 98 statements across six categories" is never
# retrieved (no "Response" token), so the block stays unnamed.
.SCALE_SIGNAL_WORDS <- c(
  "scale", "scales", "subscale", "subscales", "questionnaire", "questionnaires",
  "inventory", "inventories", "measure", "measures", "instrument", "instruments",
  "test", "battery", "index", "items", "item")

# Retrieve sentences that DESCRIBE a scale: those containing a scale-signalling
# word AND at least one Capitalized multi-word name or a parenthetical acronym
# (the two ways instruments are introduced: "Teleological Beliefs Scale (TBS)").
# Kept separate from the prefix search so the two sets can be pooled and capped.
.scale_description_sentences <- function(paper, max_sent = 60L) {
  if (!.is_paper(paper) || is.null(paper$text) || nrow(paper$text) == 0)
    return(character(0))
  signal <- paste0("\\b(", paste(.SCALE_SIGNAL_WORDS, collapse = "|"), ")\\b")
  sents <- tryCatch(
    text_search(paper, pattern = signal, return = "sentence",
                ignore.case = TRUE, perl = TRUE),
    error = function(e) NULL)
  if (is.null(sents) || !nrow(sents)) return(character(0))
  s <- unique(trimws(as.character(sents$text)))
  s <- s[nzchar(s)]
  # Send every scale-signal sentence to the LLM; do NOT pre-filter on surface
  # form. Whether a sentence names an instrument is the LLM's judgement, not the
  # regex's — a scale in lower case ("the grit scale") or described without a
  # proper-noun name would otherwise be removed before the model ever sees it.
  # The capitals/acronym cue now lives in the prompt as guidance instead.
  utils::head(s, max_sent)
}

# TEXT-ONLY scale detection: name the instruments the MANUSCRIPT describes, with
# no reference to the data. `.identify_scales_prefix_llm()` can only name a scale
# that surfaced as a column group sharing a leading abbreviation, so it is blind
# to instruments whose columns are named after item content (`accomplished`,
# `snobbish`, ...) — the AP-HP and PANAS-X blocks in collabra.38634 are exactly
# that, and produce no group at all. This reads the paper instead, so the report
# can tell an author which instruments have no item-level data to match.
#
# Recall is deliberately favoured over precision: an instrument that is only
# cited (not administered) is returned with administered = "unclear" rather than
# dropped, and the REPORT (not this function) applies the capitalisation gate.
# Returns a data.frame (scale_name, acronym, n_items, administered, confidence)
# or NULL.
.identify_scales_text_llm <- function(paper, model, params) {
  if (!llm_use() || !.is_paper(paper)) return(NULL)
  sents <- .scale_description_sentences(paper, max_sent = 30L)
  if (!length(sents)) return(NULL)

  type_spec <- ellmer::type_object(
    scales = ellmer::type_array(ellmer::type_object(
      scale_name = ellmer::type_string(
        "Full name of the instrument, exactly as capitalised in the text."),
      acronym    = ellmer::type_string("Its acronym as given in the text, or empty."),
      n_items    = ellmer::type_string("Number of items if stated, else empty."),
      administered = ellmer::type_string(
        "Did the authors administer this to their own participants? yes, unclear, or no."),
      confidence = ellmer::type_string("high, medium, or low."))))

  prompt <- paste(
    "You are given sentences from a research paper. List every psychometric",
    "instrument (scale, questionnaire, inventory, test) that the AUTHORS",
    "ADMINISTERED TO PARTICIPANTS in this study.",
    "GUIDANCE:",
    "(1) Instrument names are FREQUENTLY written in Capitalised Words, often",
    "defined once as 'Full Name of Scale (ABBR)', and a parenthetical ALL-CAPS",
    "acronym is a strong signal. But capitalisation is NOT required: an instrument",
    "named in lower case ('the grit scale', 'a life-orientation test') or",
    "described without a formal proper name ('a seven-item measure of perceived",
    "stress') still counts. Judge from meaning, not capitalisation. Return each",
    "distinct instrument once, using the fullest name the text gives it.",
    "(2) An author-year citation ('Gentile et al., 2013') is the REFERENCE for a",
    "scale, NOT its name. Never return an author name as the scale_name.",
    "(3) Prefer instruments the authors ADMINISTERED TO PARTICIPANTS in this",
    "study. When it is unclear whether an instrument was administered here or",
    "only cited from prior work, INCLUDE it and mark confidence 'low'. Recall",
    "matters more than precision: it is better to list a scale that turns out not",
    "to have been administered than to miss one that was.",
    "(4) Use 'administered' to record whether the text shows the authors gave this",
    "instrument to their own participants: 'yes', 'unclear', or 'no'.")

  resp <- tryCatch(
    llm(text = data.frame(text = paste("-", sents, collapse = "\n")),
        text_col = "text", system_prompt = prompt, type = type_spec,
        model = model, params = params,
        phase = "Identifying scales in the manuscript"),
    error = function(e) NULL)
  resp <- .strip_llm_wrapper(resp, "scales")
  if (is.null(resp) || !nrow(resp) || !"scale_name" %in% names(resp)) return(NULL)

  keep <- c("scale_name", "acronym", "n_items", "administered", "confidence")
  resp <- resp[, intersect(keep, names(resp)), drop = FALSE]
  resp$scale_name <- trimws(as.character(resp$scale_name))
  resp <- resp[nzchar(resp$scale_name), , drop = FALSE]
  resp <- resp[!duplicated(tolower(resp$scale_name)), , drop = FALSE]
  if (!nrow(resp)) return(NULL)
  resp
}

# Build the "#### Scales in the manuscript" report section from text-detected
# instruments. `matched` = scale names already accounted for in the data (from
# labels_df), so a scale whose items ARE shared is not warned about; pass
# character(0) when there is no data at all, and every detected scale is
# reported. Returns character(0) when the LLM named no instruments.
.scale_text_report <- function(text_scales, matched = character(0)) {
  if (is.null(text_scales) || !nrow(text_scales)) return(character(0))
  # Trust the LLM's judgement of what is an instrument: do NOT re-filter its
  # output on capitalisation. Scale names are not always capitalised ("the grit
  # scale"), and the model was asked to return lower-case instruments too, so a
  # capitalisation gate here would silently discard exactly those.
  ts <- text_scales[nzchar(trimws(text_scales$scale_name)), , drop = FALSE]
  if (!nrow(ts)) return(character(0))

  # Drop instruments already identified in the shared data: same name, or the
  # detected acronym matches a matched scale name (BFI/NPI vs the data's BIG/NARC
  # column prefixes, which .scales_to_osd() names in full).
  if (length(matched)) {
    m <- tolower(matched)
    hit <- tolower(ts$scale_name) %in% m |
      vapply(seq_len(nrow(ts)), function(i) {
        a <- tolower(trimws(ts$acronym[i] %||% ""))
        nzchar(a) && any(grepl(paste0("\\b", a, "\\b"), m))
      }, logical(1))
    ts <- ts[!hit, , drop = FALSE]
  }
  if (!nrow(ts)) return(character(0))

  lines <- vapply(seq_len(nrow(ts)), function(i) {
    bits <- c(
      if (nzchar(trimws(ts$acronym[i] %||% ""))) paste0(trimws(ts$acronym[i])),
      if (nzchar(trimws(ts$n_items[i] %||% ""))) paste0(trimws(ts$n_items[i]), " items"),
      if (identical(tolower(trimws(ts$administered[i] %||% "")), "unclear"))
        "possibly not administered here")
    sprintf("- **%s**%s", ts$scale_name[i],
            if (length(bits)) paste0(" (", paste(bits, collapse = "; "), ")") else "")
  }, character(1))

  c("#### Scales in the manuscript",
    sprintf(paste0(
      "The manuscript describes %d instrument%s whose item-level data we could ",
      "not find. De-identified item-level responses are far more valuable to ",
      "share than total scores: they let others check the scoring, the ",
      "reverse-coding, and the reliability, and let the items be reused. ",
      "Consider sharing the raw item responses for:"),
      nrow(ts), plural(nrow(ts))),
    lines,
    paste0("(Detected from the manuscript text; an instrument mentioned only in ",
           "passing may be listed here in error.)"))
}

# ONE-CALL LLM matcher: given every prefix group in a file (with column counts,
# max item numbers, and item wording) plus every paper sentence that mentions
# those abbreviations, ask the model to name each group's instrument FROM THE
# TEXT. The scale dictionary is offered only as a hint. A group the text does not
# name is returned with an empty scale (kept, unnamed). Returns a data.frame
# (source_file, prefix, columns list-col, scale, scale_full, confidence,
# scale_source, n_columns, max_item), one row per group, or NULL.
.identify_scales_prefix_llm <- function(previews, labels_df, paper, model, params,
                                        max_calls = 40L) {
  if (is.null(previews) || !length(previews)) return(NULL)
  dict <- .scale_dictionary()
  lbl_key <- if (all(c("source_file","column_name") %in% names(labels_df)))
    paste(labels_df$source_file, labels_df$column_name, sep = "\x01") else character(0)
  wording_of <- function(file, cols) {
    if (!length(lbl_key)) return(character(0))
    i <- match(paste(file, cols, sep = "\x01"), lbl_key)
    i <- i[!is.na(i)]
    if (!length(i)) return(character(0))
    w <- c(labels_df$label[i],
           if ("question" %in% names(labels_df)) labels_df$question[i])
    unique(w[!is.na(w) & nzchar(w)])
  }

  type_spec <- ellmer::type_object(
    scales = ellmer::type_array(ellmer::type_object(
      prefix     = ellmer::type_string("The column-name abbreviation, exactly as given."),
      scale_name = ellmer::type_string(
        "The instrument this abbreviation stands for, named or clearly described in the provided sentences (e.g. 'Breakup Distress Scale'). Empty if the text does not identify it."),
      confidence = ellmer::type_string("high, medium, or low."))))
  prompt <- paste(
    "A dataset's columns are grouped by a leading abbreviation; each group is",
    "likely one questionnaire/scale. You are given, for each group, its",
    "abbreviation, how many columns it has, the highest item number, and (if any)",
    "the item wording; plus sentences from the paper (some mention the group's",
    "abbreviation, others describe an instrument), and an optional list of known",
    "instruments. For EACH group, give the instrument the abbreviation stands",
    "for, taken from the paper sentences.",
    "GUIDANCE:",
    "(1) A scale/questionnaire/test is almost always referred to by a proper name",
    "in Capitalised Words, e.g. 'Anthropomorphism Questionnaire', 'Teleological",
    "Beliefs Scale', 'Centrality of Religiosity Scale' — often defined once as",
    "'Full Name of Scale (ABBR)'. Return that Capitalised name, not a lowercase",
    "paraphrase.",
    "(2) An author-year citation like 'Neave et al., 2015' or 'Huber & Huber,",
    "2012' is the REFERENCE for a scale, NOT the scale's name. Never return an",
    "author name as the scale_name.",
    "(3) Use the counts to match: a group's column count (and highest item",
    "number) should line up with a count stated in the text (e.g. 'the AQ",
    "contains 20 items', or 'the scale has 98 statements across six categories of",
    "14, 14, 25, 10, 25, and 10'). Prefer the instrument whose stated size matches",
    "the group's size, even if the abbreviation differs from the column prefix.",
    "(4) If the text does not identify the group, return an empty scale_name for",
    "it — do NOT guess and do NOT force a known instrument. Return one entry per",
    "group.")

  out <- list(); model_used <- NA_character_; n_calls <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    sga <- .scale_group_args(df, file, labels_df)
    groups <- .scale_prefix_groups(df, scale_group = sga$scale_group,
                                   qualtrics = sga$qualtrics)
    if (length(groups) == 0) next

    # Sentences to give the model: (a) those mentioning this file's abbreviations,
    # PLUS (b) sentences that describe an instrument by name (Capitalised name /
    # acronym near a scale-signalling word). (b) is essential when the column
    # prefix differs from the manuscript's abbreviation (e.g. "Response.*" columns
    # described as the "Teleological Beliefs Scale") — otherwise that description
    # is never retrieved. Pooled and de-duplicated; prefix sentences come first.
    prefixes <- vapply(groups, function(g) g$display, character(1))
    sents <- if (.is_paper(paper))
      unique(c(.scale_prefix_sentences(paper, prefixes),
               .scale_description_sentences(paper))) else character(0)

    # Dictionary hints: known instruments whose acronym equals a group prefix.
    hint_rows <- dict[toupper(gsub("[^A-Za-z0-9]","",dict$acronym)) %in%
                        toupper(prefixes) & nzchar(dict$acronym), , drop = FALSE]
    hints <- if (nrow(hint_rows))
      paste(sprintf("%s = %s", hint_rows$acronym, hint_rows$name), collapse = "; ") else ""

    # Build the group listing sent to the model.
    grp_txt <- vapply(names(groups), function(g) {
      gg <- groups[[g]]
      wd <- wording_of(file, gg$columns)
      sprintf("- %s: %d columns, highest item number %s%s",
              gg$display, gg$n_columns,
              if (is.na(gg$max_item)) "unknown" else as.character(gg$max_item),
              if (length(wd)) paste0("; item wording: ",
                paste(utils::head(wd, 6), collapse = " | ")) else "")
    }, character(1))

    sents <- utils::head(sents, 60L)   # cap the pooled evidence
    text_in <- paste0(
      "Column groups in this data file:\n", paste(grp_txt, collapse = "\n"),
      if (length(sents)) paste0("\n\nSentences from the paper (abbreviation mentions and instrument descriptions):\n",
        paste("-", sents, collapse = "\n")) else "\n\n(No relevant paper sentences found.)",
      if (nzchar(hints)) paste0("\n\nKnown instruments (hints only, confirm from the text): ", hints) else "")

    resp <- NULL
    if (llm_use() && n_calls < max_calls) {
      n_calls <- n_calls + 1L
      resp <- tryCatch(
        llm(text = data.frame(text = text_in), text_col = "text",
            system_prompt = prompt, type = type_spec, model = model,
            params = params, phase = "Matching scales to text"),
        error = function(e) NULL)
      resp <- .strip_llm_wrapper(resp, "scales")
      if (!is.null(resp) && is.na(model_used))
        model_used <- attr(resp, "llm")$model %||% NA_character_
    }

    # Map LLM answers back to groups by prefix; keep EVERY group (named or not).
    named <- stats::setNames(rep(NA_character_, length(groups)), names(groups))
    conf  <- stats::setNames(rep(NA_character_, length(groups)), names(groups))
    if (!is.null(resp) && nrow(resp) && "prefix" %in% names(resp)) {
      rk <- tolower(trimws(as.character(resp$prefix)))
      for (k in names(groups)) {
        j <- which(rk == k)
        if (!length(j)) next
        sn <- trimws(as.character(resp$scale_name[j[1]] %||% ""))
        if (!is.na(sn) && nzchar(sn) &&
            !tolower(sn) %in% c("unknown","unclear","na","none")) {
          named[k] <- sn
          cf <- tolower(trimws(as.character(resp$confidence[j[1]] %||% "medium")))
          conf[k] <- if (cf %in% c("high","medium","low")) cf else "medium"
        }
      }
    }

    for (k in names(groups)) {
      gg <- groups[[k]]
      out[[length(out) + 1L]] <- data.frame(
        source_file = file, prefix = gg$display,
        scale = named[k] %||% NA_character_,
        confidence = conf[k] %||% NA_character_,
        scale_source = if (!is.na(named[k])) "manuscript" else NA_character_,
        n_columns = gg$n_columns, max_item = gg$max_item,
        totals_only = isTRUE(gg$totals_only),
        columns = I(list(gg$columns)), stringsAsFactors = FALSE)
    }
    if (n_calls >= max_calls) break
  }

  res <- if (length(out)) do.call(rbind, out) else NULL
  if (!is.null(res)) attr(res, "llm_model") <- model_used
  res
}

# Response scale for a block of columns, letting the CODEBOOK LEAD:
#   1. If the codebook gives value labels for the block's columns, take the
#      numeric codes (excluding declared missings) as the range and the label
#      strings as the anchor labels — ground truth.
#   2. Otherwise fall back to the OBSERVED min/max/n_unique from data_check's
#      column statistics, with no anchor labels (we do not know them).
#   3. If neither yields a coherent integer-like scale, return NULL and the
#      caller omits `likert_options` (no invention).
.osd_likert_options <- function(cols, source_file, columns_df, labels_df) {
  key <- function(df) paste(df$source_file, df$column_name, sep = "\x01")
  want <- paste(source_file, cols, sep = "\x01")

  # 1. Codebook value labels (per column); use the first column that has them,
  #    since a block shares one response scale.
  if (!is.null(labels_df) && nrow(labels_df) > 0 &&
      all(c("source_file", "column_name") %in% names(labels_df)) &&
      "value_labels" %in% names(labels_df)) {
    lk <- key(labels_df)
    for (w in want) {
      i <- match(w, lk)
      if (is.na(i)) next
      vl <- .decode_value_labels(labels_df$value_labels[i])
      if (is.null(vl) || !length(vl)) next
      codes <- suppressWarnings(as.integer(names(vl)))
      miss  <- .decode_value_labels(labels_df$missing_values[i] %||% NA_character_)
      if (!is.null(miss)) {
        drop <- names(vl) %in% names(miss)
        codes <- codes[!drop]; vl <- vl[!drop]
      }
      keep <- !is.na(codes)
      codes <- codes[keep]; vl <- vl[keep]
      if (length(codes) >= 2)
        return(Filter(Negate(is.null), list(
          points = length(codes), min = min(codes), max = max(codes),
          labels = unname(as.character(vl)),
          order  = "ascending", source = "codebook")))
    }
  }

  # 2. Observed statistics from data_check. Use the MEDIAN of each column's min
  #    and max across the block, not the block-wide min/max — a scale block is
  #    often contaminated by a few aggregate columns the authors named with the
  #    same prefix (e.g. a summed neoTotal alongside the 1-5 items). Those totals
  #    have a much wider range and would dominate a min/max/n_unique aggregate
  #    (that is where a bogus "points: 85" came from). The median is robust to a
  #    minority of such columns and reflects the range the bulk of items share.
  #    `points` is the RANGE (max - min + 1), never n_unique (distinct observed
  #    values != response options for a score column).
  if (!is.null(columns_df) && nrow(columns_df) > 0 &&
      all(c("source_file", "column_name", "min", "max") %in% names(columns_df))) {
    ck <- key(columns_df)
    idx <- which(ck %in% want)
    if (length(idx)) {
      mn <- suppressWarnings(stats::median(columns_df$min[idx], na.rm = TRUE))
      mx <- suppressWarnings(stats::median(columns_df$max[idx], na.rm = TRUE))
      # Only claim a scale when the median range is a small integer-like span
      # (a plausible rating scale). A score-dominated block whose median range
      # is wide emits NOTHING rather than a wrong likert_options.
      if (is.finite(mn) && is.finite(mx) && mx > mn &&
          mn == round(mn) && mx == round(mx) && (mx - mn) <= 12) {
        return(list(points = as.integer(mx - mn + 1L),
                    min = mn, max = mx, order = "ascending", source = "observed"))
      }
    }
  }
  NULL
}

# Express identified scales in the OpenScales OSD JSON structure, as faithfully
# as the available evidence allows — one object per (file, scale-group). The
# response scale lets the codebook lead (see .osd_likert_options); item wording
# comes from the codebook labels and is placed in `translations` (with each
# item's `text_key` pointing at it, per the spec) — absent when undocumented.
# metacheck-specific facts (source file, provenance, confidence) live in a
# namespaced `definition.metacheck` block so the spec objects stay clean.
#
# Every named group yields an object; unnamed groups are returned too (for the
# in-memory inventory / report) but carry `write = FALSE` so the file writer
# skips them — an unnamed block is an unresolved detection, not a scale.
.scales_to_osd <- function(scale_groups, columns_df = NULL, labels_df = NULL,
                           dict = .scale_dictionary()) {
  if (is.null(scale_groups) || !nrow(scale_groups)) return(list())

  # Codebook label lookup for translations (id -> item wording).
  lbl_key <- if (!is.null(labels_df) && nrow(labels_df) > 0 &&
                 all(c("source_file", "column_name") %in% names(labels_df)))
    paste(labels_df$source_file, labels_df$column_name, sep = "\x01") else character(0)
  # Non-empty scalar or NA — guards a missing column (NULL) or NA cell, so
  # `if (...)` never sees a zero-length/NA condition.
  ok <- function(x) length(x) == 1L && !is.na(x) && nzchar(x)
  label_of <- function(file, col) {
    if (!length(lbl_key)) return(NA_character_)
    i <- match(paste(file, col, sep = "\x01"), lbl_key)
    if (is.na(i)) return(NA_character_)
    w <- if ("label" %in% names(labels_df)) labels_df$label[i] else NA_character_
    if (ok(w)) return(w)
    if ("question" %in% names(labels_df) && ok(labels_df$question[i]))
      return(labels_df$question[i])
    NA_character_
  }

  # Scale names (across the whole paper) that HAVE a genuine item block — a named
  # group that is NOT totals-only. A totals-only group whose scale name is in this
  # set is the redundant aggregate of items we already exported, so it is skipped;
  # one with a name NOT in the set is an orphan total (items not shared) and is
  # written with a warning. Matched by scale NAME, so it works across files.
  tot_col <- if ("totals_only" %in% names(scale_groups))
    !is.na(scale_groups$totals_only) & scale_groups$totals_only else
    rep(FALSE, nrow(scale_groups))
  has_items_for <- {
    nm <- scale_groups$scale
    real <- !is.na(nm) & nzchar(nm) & !tot_col
    unique(tolower(nm[real]))
  }

  out <- lapply(seq_len(nrow(scale_groups)), function(i) {
    r <- scale_groups[i, ]
    cols <- r$columns[[1]]
    named <- !is.na(r$scale) && nzchar(r$scale)
    totals_only <- "totals_only" %in% names(r) && isTRUE(r$totals_only)
    # A totals-only block whose scale ALSO has a real item block elsewhere is the
    # redundant aggregate of those items — drop it (report-only, not written).
    redundant_total <- totals_only && named &&
      tolower(r$scale) %in% has_items_for
    # A group with no name is still WRITTEN when it is a coherent rating-like
    # block (0-100 sliders, Likert, etc.) — recorded for its structure under an
    # "unnamed_block" provenance. Non-scale prefix groups (probability triplets,
    # unbounded model parameters) fail the gate and stay report-only.
    rating_like <- !named &&
      .scale_block_is_ratinglike(cols, r$source_file, columns_df)
    eff_source  <- if (named) r$scale_source else if (rating_like) "unnamed_block" else r$scale_source
    cp <- .osd_code_and_provenance(r$scale, r$prefix, eff_source, dict)

    # Item wording -> translations; text_key points at the id when documented.
    wording <- stats::setNames(vapply(cols, function(c) label_of(r$source_file, c),
                                      character(1)), cols)
    translations_en <- as.list(wording[!is.na(wording) & nzchar(wording)])

    lopts <- .osd_likert_options(cols, r$source_file, columns_df, labels_df)

    scale_info <- Filter(Negate(is.null), list(
      name         = if (named) r$scale else "",
      code         = cp$code,
      abbreviation = r$prefix))

    # Reference instrument (OpenScales), when this scale is a known one. Gives
    # the published item text, item count and reverse-keying to compare the
    # shared data against. NULL for manuscript-only / self-generated labels.
    reference <- .scale_reference(cp$ref_code)
    ref_match <- .scale_match_items(wording, reference)

    # The OSD spec (section 3) makes scale_info, likert_options, dimensions,
    # items and scoring ALL required; likert_options may be null when no item
    # uses the likert type, and dimensions/scoring may be empty for an unscored
    # instrument. Emit all five so the file is conformant.
    definition <- list(scale_info = scale_info)
    # `definition$likert_options <- NULL` would DELETE the key rather than set
    # it to null, and the spec requires the key to be present. Single-bracket
    # assignment of list(NULL) keeps it, and jsonlite(null = "null") then
    # writes `"likert_options": null`.
    definition["likert_options"] <- list(lopts)

    definition$items <- lapply(seq_along(cols), function(k) {
      it <- list(id = cols[k], text_key = cols[k])
      if (!is.null(lopts)) it$type <- "likert"
      # Reverse-keying is NOT written here. The spec declares it via signed
      # weights in `scoring` (see below) and marks the per-item boolean
      # deprecated, so a `reverse` field on the item would be silently ignored
      # by a conformant reader. The link back to the reference instrument is
      # metacheck's own information, so it lives under a namespaced key that
      # other readers skip (spec section 1, unknown keys are ignored).
      if (!is.null(ref_match)) {
        j <- match(cols[k], ref_match$column_name)
        if (!is.na(j) && !is.na(ref_match$ref_item_id[j]))
          it$`metacheck:reference_item` <- unname(ref_match$ref_item_id[j])
      }
      it
    })

    # ── dimensions + scoring (spec sections 7 and 9) ─────────────────────────
    # Reverse-keying belongs here, as a signed coding map: 1 = forward,
    # -1 = reverse. This is the spec's authoritative form and the only one a
    # conformant runner acts on. It also defines what reverse MEANS
    # numerically: contribution = likert_options.min + likert_options.max -
    # response, which is why an emitted -1 is only meaningful alongside the
    # likert_options we write above.
    #
    # Only items whose wording matched the reference instrument can be coded,
    # because only those have a known direction. When nothing matched there is
    # no defensible scoring block, so both dimensions and scoring are emitted
    # EMPTY (an empty array / object, which the spec explicitly permits) rather
    # than guessing that every item is forward-coded.
    coding <- NULL
    if (!is.null(ref_match)) {
      m <- ref_match[!is.na(ref_match$ref_reverse), , drop = FALSE]
      if (nrow(m))
        coding <- stats::setNames(
          as.list(ifelse(m$ref_reverse, -1L, 1L)), m$column_name)
    }

    if (!is.null(coding) && length(coding)) {
      dim_id <- .osd_safe_code(if (named) r$scale else r$prefix)
      dim_id <- tolower(gsub("-", "_", dim_id))
      definition$dimensions <- list(list(
        id   = dim_id,
        name = if (named) r$scale else r$prefix,
        description = sprintf(
          "Total score over the %d item%s matched to the reference instrument. %d reverse-keyed.",
          length(coding), plural(length(coding)),
          sum(unlist(coding) == -1L))))
      definition$scoring <- stats::setNames(list(list(
        method = "sum_coded",
        items  = coding,
        description = paste("Reverse-coding taken from the reference",
                            "instrument's published scoring, not inferred",
                            "from the data."))), dim_id)
    } else {
      # Spec: dimensions may be an empty array, scoring an empty object.
      definition$dimensions <- list()
      definition$scoring    <- stats::setNames(list(), character(0))
    }
    # A totals-only block with NO item block anywhere: an orphan aggregate score.
    # We still record it (it names a scale that was clearly measured), but flag it
    # as total-only and warn that the items were not found.
    orphan_total <- totals_only && !redundant_total
    total_note <- if (orphan_total)
      paste("Detected as the total/average score for this scale, but no",
            "item-level columns were found. The individual items may not be",
            "shared, or are labelled differently. Consider sharing item-level",
            "data or labelling items clearly so the scale can be verified.") else NULL

    # Link to the published instrument, when there is one. `items_matched` is
    # how many of THIS block's columns were identified in the reference by their
    # wording; when it is short of the reference's `n_items`, the scale looks
    # incompletely shared (or the items are worded differently) — recorded, not
    # silently reconciled. Observed data is never overwritten by the registry.
    ref_block <- if (!is.null(reference)) {
      n_matched <- if (is.null(ref_match)) 0L else sum(!is.na(ref_match$ref_item_id))
      rm_meta <- reference$meta
      # Reliabilities are per-subscale; report the range rather than a single
      # number, since most instruments here have several dimensions.
      alphas <- if (!is.null(reference$scoring)) reference$scoring$alpha else NULL
      alphas <- alphas[!is.na(alphas)]
      Filter(Negate(is.null), list(
        registry      = "OpenScales",
        code          = rm_meta$code,
        name          = rm_meta$name,
        license       = if (nzchar(rm_meta$license)) rm_meta$license else NULL,
        citation      = if (nzchar(rm_meta$citation)) rm_meta$citation else NULL,
        url           = if (nzchar(rm_meta$url)) rm_meta$url else NULL,
        n_items       = rm_meta$n_items,
        n_reverse     = rm_meta$n_reverse,
        items_matched = n_matched,
        alpha_range   = if (length(alphas))
          list(min = min(alphas), max = max(alphas)) else NULL,
        note = if (n_matched > 0L && n_matched < rm_meta$n_items)
          sprintf(paste("Matched %d of the reference instrument's %d items by",
                        "wording. The remaining items may not be shared, or may",
                        "be worded differently in this codebook."),
                  n_matched, rm_meta$n_items)
        else if (n_matched == 0L)
          paste("The scale NAME matches a known instrument, but no item wording",
                "could be matched to it — the codebook may not record item text,",
                "or this may be a different version of the instrument.")
        else NULL))
    } else NULL

    # metacheck provenance extension (kept out of the spec's scale_info).
    # source_files is always a list: one entry now, extended when the same scale
    # is found in other data files (see .osd_dedup_by_source).
    definition$metacheck <- Filter(Negate(is.null), list(
      scale_source    = cp$source,
      provenance      = cp$provenance,
      confidence      = if (!is.na(r$confidence)) r$confidence else NULL,
      kind            = if (orphan_total) "scale_total_only" else NULL,
      note            = total_note,
      reference       = ref_block,
      source_files    = as.list(r$source_file),
      n_columns       = r$n_columns,
      declared_length = if (!is.na(r$max_item)) r$max_item else NULL))

    osd <- list(osd_version = "1.0", definition = definition)
    if (length(translations_en))
      osd$translations <- list(en = translations_en)
    # Marker for the writer: named scales AND unnamed-but-rating-like blocks
    # become files; other unnamed prefix groups stay report-only. A totals-only
    # block redundant with an exported item block is NOT written; an orphan total
    # IS written (flagged), so the record + warning survive.
    #
    # Size backstop for INFERRED labels. A self-generated or unnamed block that,
    # after item/derived splitting, holds fewer than .scale_min_items genuine
    # items is not a scale — it is one survey question that some export machinery
    # (e.g. glued-on Qualtrics timer columns the paradata filter did not strip)
    # inflated to block size. We refuse to mint an .osd for it. Dictionary and
    # manuscript matches are exempt: a recognised instrument can legitimately be
    # short (a validated 2-item scale), and its name is evidence in its own right.
    n_items_block <- length(intersect(cols, names(translations_en)))
    if (n_items_block == 0L) n_items_block <- length(cols)  # no wording -> count cols
    inferred    <- eff_source %in% c("self_generated", "unnamed_block")
    too_small   <- inferred && n_items_block < .scale_min_items
    attr(osd, "write") <- (named || rating_like) && !redundant_total && !too_small
    attr(osd, "code")  <- cp$code
    attr(osd, "orphan_total") <- orphan_total
    attr(osd, "scale") <- if (named) r$scale else NA_character_
    # Identity for cross-file de-duplication: the same instrument administered in
    # several data files repeats the same columns (FightN155.csv, FightN127.csv).
    # Key on scale name (or code) + the exact item set, so "same scale, different
    # file" collapses to one definition while "same name, different items" stays
    # separate.
    attr(osd, "dedup_key") <- paste(tolower(cp$code),
                                     paste(sort(cols), collapse = "\x01"),
                                     sep = "\x02")
    osd
  })

  .osd_dedup_by_source(Filter(Negate(is.null), out))
}

# Collapse OSD objects that describe the SAME scale seen in multiple source files
# (identical dedup_key) into a single definition, recording every file it appears
# in under definition$metacheck$source_files. The first occurrence is kept; later
# duplicates only contribute their source file(s). Non-duplicate scales pass
# through unchanged (source_files stays length 1).
.osd_dedup_by_source <- function(osds) {
  if (!length(osds)) return(osds)
  keys <- vapply(osds, function(o) attr(o, "dedup_key") %||% "", character(1))
  kept <- list(); seen <- list()   # key -> index into kept
  for (i in seq_along(osds)) {
    o <- osds[[i]]; k <- keys[[i]]
    if (nzchar(k) && !is.null(seen[[k]])) {
      j <- seen[[k]]
      mc <- kept[[j]]$definition$metacheck
      mc$source_files <- as.list(unique(c(unlist(mc$source_files),
                                          unlist(o$definition$metacheck$source_files))))
      kept[[j]]$definition$metacheck <- mc
      next
    }
    kept[[length(kept) + 1L]] <- o
    if (nzchar(k)) seen[[k]] <- length(kept)
  }
  kept
}

# LLM self-generated scale labels. For scale-like blocks that matched NO known
# instrument (still unnamed in labels_df), generate a short CONSTRUCT label from
# the evidence — the codebook item wording of the block's columns and paper
# sentences that mention the block's variables. This is a distinct category from
# a dictionary match: scale_source = "self_generated". It is GROUNDED — the model
# is told to use only the provided text and to return nothing when the text does
# not describe what the items measure, so it never invents a construct from bare
# column names. Only runs with an LLM. Returns (source_file, column_name, scale,
# confidence, scale_source), or an empty frame; NULL is treated as empty.
.identify_scales_selfgen <- function(previews, labels_df, paper, model, params,
                                     columns_df = NULL, text_scales = NULL,
                                     max_calls = 40L) {
  empty <- data.frame(source_file = character(), column_name = character(),
                      scale = character(), confidence = character(),
                      scale_source = character())
  if (is.null(previews) || !length(previews)) return(empty)

  named_key <- if (all(c("source_file","column_name","scale") %in% names(labels_df)))
    paste(labels_df$source_file, labels_df$column_name,
          sep = "\x01")[!is.na(labels_df$scale) & nzchar(labels_df$scale %||% "")] else
    character(0)
  lbl_key <- paste(labels_df$source_file, labels_df$column_name, sep = "\x01")
  wording_of <- function(file, cols) {
    i <- match(paste(file, cols, sep = "\x01"), lbl_key)
    i <- i[!is.na(i)]
    if (!length(i)) return(character(0))
    w <- c(labels_df$label[i],
           if ("question" %in% names(labels_df)) labels_df$question[i])
    w[!is.na(w) & nzchar(w)]
  }
  have_paper <- .is_paper(paper) && !is.null(paper$text) && nrow(paper$text) > 0

  type_spec <- ellmer::type_object(
    construct = ellmer::type_string(
      "Short construct label for what these items measure (e.g. 'Institutional Trust', 'Environmental Concern'), or empty if the text does not say."),
    confidence = ellmer::type_string("high, medium, or low."))
  prompt <- paste(
    "You are given ONE block of survey/rating items from a dataset: their column",
    "names (which often contain meaningful words), any documented item wording,",
    "and (optionally) sentences from the paper that mention these variables. The",
    "block does NOT match a known published instrument. Give a short, natural",
    "CONSTRUCT LABEL for what the block measures (e.g. 'Positive trait ratings',",
    "'Environmental Concern'). Base it on the provided evidence — you MAY use",
    "meaningful words in the column names (including non-English words, e.g.",
    "Spanish 'buena'=good, 'mala'=bad) together with the wording and sentences.",
    "Only return an empty construct if the column names are opaque codes with no",
    "interpretable words AND there is no wording or paper text. Never invent a",
    "published scale name; describe the construct in plain words.")

  # Candidate blocks per file: the strict Likert blocks PLUS rating-like prefix
  # groups (0-100 sliders etc.) the Likert detector misses. Each entry is the
  # block's column names; deduplicated by the column set so a block found by both
  # detectors is asked about once.
  candidate_blocks <- function(df, file) {
    blocks <- lapply(.detect_scale_blocks(df), function(ci) names(df)[ci])
    sga <- .scale_group_args(df, file, labels_df)
    for (g in .scale_prefix_groups(df, scale_group = sga$scale_group,
                                   qualtrics = sga$qualtrics)) {
      if (.scale_block_is_ratinglike(g$columns, file, columns_df))
        blocks[[length(blocks) + 1L]] <- g$columns
    }
    unique(blocks)
  }

  out <- list(); model_used <- NA_character_; n_calls <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) < .scale_min_items) next
    for (nms in candidate_blocks(df, file)) {
      keys <- paste(file, nms, sep = "\x01")
      if (any(keys %in% named_key)) next          # already named — skip
      wording <- wording_of(file, nms)
      ctx <- if (have_paper)
        .scale_paper_context(paper, .scale_name_prefix(nms[[1]]), wording) else
        character(0)
      # Descriptive words in the column names themselves (letters-only tokens of
      # >= 3 chars, minus a stray item-index token). A block whose names carry
      # real words CAN be labelled from them; a block of opaque codes cannot.
      name_tokens <- unique(unlist(strsplit(tolower(nms), "[^a-z]+")))
      name_tokens <- name_tokens[nchar(name_tokens) >= 3L]
      # Need SOME evidence: wording, paper context, or interpretable name tokens.
      if (length(wording) == 0 && length(ctx) == 0 && length(name_tokens) == 0)
        next
      # A tokens-only identification is inherently weaker; cap its confidence.
      tokens_only <- length(wording) == 0 && length(ctx) == 0
      if (n_calls >= max_calls) break
      n_calls <- n_calls + 1L

      text_in <- paste0(
        "Column names:\n", paste("-", nms, collapse = "\n"),
        "\n\nItem wording:\n",
        if (length(wording)) paste("-", wording, collapse = "\n") else "(none documented)",
        if (length(ctx)) paste0(
          "\n\nRelevant paper sentences:\n", paste("-", ctx, collapse = "\n")) else "")
      resp <- tryCatch(
        llm(text = data.frame(text = text_in), text_col = "text",
            system_prompt = prompt, type = type_spec, model = model,
            params = params, phase = "Labelling scales"),
        error = function(e) NULL)
      if (is.null(resp) || nrow(resp) == 0 || !"construct" %in% names(resp)) next
      if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_
      construct <- trimws(as.character(resp$construct[[1]] %||% ""))
      if (is.na(construct) || !nzchar(construct) ||
          tolower(construct) %in% c("unknown","unclear","na","none")) next
      conf <- tolower(trimws(as.character(resp$confidence[[1]] %||% "medium")))
      if (!conf %in% c("high","medium","low")) conf <- "medium"
      if (tokens_only) conf <- "low"
      out[[length(out) + 1L]] <- data.frame(
        source_file = file, column_name = nms,
        scale = construct, confidence = conf, scale_source = "self_generated")
    }
    if (n_calls >= max_calls) break
  }
  res <- if (length(out)) dplyr::bind_rows(out) else empty
  res <- .selfgen_merge_synonyms(res)
  attr(res, "llm_model") <- model_used
  res
}

# Collapse SYNONYMOUS self-generated labels WITHIN a file. When one underlying
# construct is exported as several stem-families (or the LLM, asked once per
# block, returns near-identical wording — "Emotion Recognition",
# "Emotion Recognition Empathic Accuracy", "Emotion Identification"), the paper
# ends up with dozens of almost-duplicate .osd files for what is really one
# task. This pass merges such rows so one construct becomes one scale.
#
# The merge rule is deliberately CONSERVATIVE — it only unites two labels when
# one's word set is a SUBSET of (or equal to) the other's, after dropping
# stopwords. So "emotion recognition" folds into "emotion recognition empathic
# accuracy" (subset), but "relationship satisfaction" and "emotion recognition"
# stay separate (disjoint word sets). Fuzzy/partial-overlap matching is avoided
# on purpose: over-merging would silently fuse genuinely different scales, a
# worse error than leaving two near-duplicates. The winning label is the SHORTER
# one (the more general construct name), and every merged block's columns keep
# their rows, only their `scale` value is rewritten to the winner.
.selfgen_merge_synonyms <- function(res) {
  if (is.null(res) || !nrow(res) || !"scale" %in% names(res)) return(res)
  stop_words <- c("scale","score","scores","ratings","rating","assessment",
                  "task","level","levels","during","the","of","a","an","and",
                  "or","for","to","in","on","data","metrics","measure")
  toks <- function(s) {
    t <- unique(strsplit(tolower(gsub("[^a-z ]+", " ", s)), "\\s+")[[1]])
    t <- t[nchar(t) > 0 & !t %in% stop_words]
    t
  }
  for (f in unique(res$source_file)) {
    idx <- which(res$source_file == f)
    if (length(idx) < 2L) next
    names_f <- unique(res$scale[idx])
    if (length(names_f) < 2L) next
    tk <- lapply(names_f, toks)
    # Order candidates by token-set size so shorter (more general) names win and
    # absorb their supersets. canon maps each name -> the label it collapses to.
    ord   <- order(vapply(tk, length, integer(1)))
    canon <- stats::setNames(names_f, names_f)
    for (a in ord) {
      for (b in ord) {
        if (a == b) next
        # b is a superset of a (a's words all appear in b) -> b collapses to a's
        # canonical label. Skip empty token sets (opaque names) — never merge on
        # "no evidence".
        if (length(tk[[a]]) && all(tk[[a]] %in% tk[[b]]))
          canon[[names_f[b]]] <- canon[[names_f[a]]]
      }
    }
    res$scale[idx] <- unname(canon[res$scale[idx]])
  }
  res
}

codebook_identify_scales <- function(previews, labels_df, model, params,
                                     paper = NULL, max_calls = 40L) {
  lbl_key <- paste(labels_df$source_file, labels_df$column_name, sep = "\x01")
  label_of <- function(file, col) {
    i <- match(paste(file, col, sep = "\x01"), lbl_key)
    if (is.na(i)) return(NA_character_)
    labels_df$label[i]
  }
  have_paper_text <- .is_paper(paper) &&
    !is.null(paper$text) && nrow(paper$text) > 0

  # Scales the paper names outright. Offering these as candidates lets the model
  # CONFIRM a stated instrument — far more reliable than guessing from item
  # wording — so they lift identifications to high confidence when the columns
  # match. Two sources, unioned:
  #   text_scales — the manuscript read by an LLM (passed in; already computed
  #     once for the report warning, so this costs no extra call). Finds
  #     instruments no dictionary holds, e.g. a scale the authors built.
  #   .scan_paper_for_scales — dictionary regex. Kept as a floor: it still works
  #     with llm_use(FALSE), and catches a dictionary instrument the LLM omitted.
  llm_named <- if (!is.null(text_scales) && nrow(text_scales)) {
    text_scales$scale_name
  } else {
    character(0)
  }
  paper_scales <- if (have_paper_text) {
    union(llm_named, .scan_paper_for_scales(paper))
  } else {
    character(0)
  }

  # Per file: the Likert-eligible columns (flattened across blocks) and their
  # wording, plus a signature (ordered column names) for cross-file dedup.
  file_items <- list()
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) < .scale_min_items) next
    cols <- unlist(.detect_scale_blocks(df), use.names = FALSE)
    if (length(cols) == 0) next
    nms  <- names(df)[cols]
    labs <- vapply(nms, function(cn) label_of(file, cn), character(1))
    file_items[[file]] <- list(
      nms = nms, labs = labs,
      sig = paste(nms, collapse = "\x1f"))
  }
  n_detected <- length(file_items)
  if (n_detected == 0) {
    res <- data.frame(source_file = character(), column_name = character(),
                      scale = character(), confidence = character())
    attr(res, "llm_model") <- NA_character_
    attr(res, "n_detected") <- 0L; attr(res, "n_skipped") <- 0L
    return(res)
  }

  # The model returns an array: one entry per scale it finds in the file.
  type_spec <- ellmer::type_object(
    scales = ellmer::type_array(
      ellmer::type_object(
        scale = ellmer::type_string(
          "Common published name of the instrument these items form (e.g. 'PANAS', 'Rosenberg Self-Esteem Scale')."),
        confidence = ellmer::type_string("high, medium, or low."),
        columns = ellmer::type_array(
          ellmer::type_string("An exact item column name belonging to this scale."))
      )
    )
  )
  prompt <- paste(
    "You are given the survey-item columns of one data file, each as",
    "'<number>. <variable_name>: <item wording>' (wording omitted when unknown).",
    "Split these columns into the distinct psychometric scales they form, and",
    "name each scale. For every scale you recognise, return its common published",
    "name (e.g. PANAS, Rosenberg Self-Esteem Scale, Big Five Inventory, SVO",
    "Slider), a confidence, and the exact list of its member column names.",
    "Base identification on: the item wording; a well-known variable-name prefix",
    "(RSE, PANAS, BFI, PSS, DOS); and — most reliably — any instruments named in",
    "the paper's text or sentences provided below. When the paper names an",
    "instrument and the items plausibly match it, confirm that instrument.",
    "",
    "Only report a scale you are REASONABLY CERTAIN of: use confidence 'high'",
    "when the paper names the instrument or the items are an unmistakable match,",
    "'medium' when the name/wording strongly implies a specific published",
    "instrument, and DO NOT report the scale at all (omit it) when you would only",
    "be guessing. Never invent a scale for generic names like Q1, V3, item5, and",
    "never report 'low' confidence — omit those instead."
  )

  # Deduplicate by signature: identify once per distinct survey layout.
  sigs      <- vapply(file_items, function(x) x$sig, character(1))
  uniq_sigs <- unique(sigs)

  # Which signatures actually have a basis to identify (would cost an LLM call)?
  # A signature with no wording and only generic names is skipped for free.
  has_basis <- function(sig) {
    it  <- file_items[[names(file_items)[sigs == sig][[1]]]]
    prefixes    <- vapply(it$nms, .scale_name_prefix, character(1))
    has_wording <- sum(!is.na(it$labs) & nzchar(it$labs)) >= 2L
    all_generic <- all(!nzchar(prefixes) |
                       grepl("^(v|q|x|col|var|item|value)$", prefixes))
    has_wording || !all_generic
  }
  callable_sigs <- Filter(has_basis, uniq_sigs)
  n_skipped     <- length(uniq_sigs) - length(callable_sigs)

  # Upfront gate: if identifying every distinct survey layout would exceed the
  # call budget, skip the whole scale tier (do not identify a partial subset).
  if (length(callable_sigs) > max_calls) {
    res <- data.frame(source_file = character(), column_name = character(),
                      scale = character(), confidence = character())
    attr(res, "llm_model")  <- NA_character_
    attr(res, "n_detected") <- n_detected
    attr(res, "n_skipped")  <- n_skipped
    attr(res, "gated")      <- cap_gate_count(
      length(callable_sigs), "codebook_max_calls", max_calls,
      "survey layout", context = "scale identification", action = "identify")
    attr(res, "n_needed")   <- length(callable_sigs)
    return(res)
  }

  out <- list(); model_used <- NA_character_

  for (sig in callable_sigs) {
    files_here <- names(file_items)[sigs == sig]
    rep_file   <- files_here[[1]]
    it <- file_items[[rep_file]]
    nms <- it$nms; labs <- it$labs

    prefixes <- vapply(nms, .scale_name_prefix, character(1))

    items   <- ifelse(!is.na(labs) & nzchar(labs), paste0(nms, ": ", labs), nms)
    listing <- paste(seq_along(items), items, sep = ". ", collapse = "\n")

    # Paper context: sentences from the manuscript that mention this block's
    # variable-name prefix or its distinctive item words. The Methods section
    # usually names the instrument outright ("the 20-item PANAS ..."), which
    # lets the model confirm rather than guess. Only when a paper is available.
    ctx <- if (have_paper_text)
      .scale_paper_context(paper, prefixes, labs) else character(0)
    text_in <- listing
    if (length(paper_scales) > 0)
      text_in <- paste0(
        text_in,
        "\n\nInstruments named in the paper's text (candidates to confirm if the",
        " items match one of them): ", paste(paper_scales, collapse = ", "))
    if (length(ctx) > 0)
      text_in <- paste0(
        text_in,
        "\n\nRelevant sentences from the paper (may name the instrument):\n",
        paste("-", ctx, collapse = "\n"))

    resp <- tryCatch(
      llm(text = data.frame(text = text_in), text_col = "text",
          system_prompt = prompt, type = type_spec, model = model,
          params = params, phase = "Identifying scales"),
      error = function(e) NULL)
    resp <- .strip_llm_wrapper(resp, "scales")
    if (is.null(resp) || nrow(resp) == 0 || !"scale" %in% names(resp)) next
    if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_

    for (k in seq_len(nrow(resp))) {
      scale <- trimws(as.character(resp$scale[[k]]))
      conf  <- tolower(trimws(as.character(resp$confidence[[k]] %||% "")))
      # A model may return an array entry with a missing scale/confidence
      # (nzchar(NA) is NA, which would error the `if`); treat NA as empty/skip.
      if (is.na(scale)) scale <- ""
      if (is.na(conf))  conf  <- ""
      if (!nzchar(scale) || tolower(scale) %in% c("unknown", "unclear", "na") ||
          conf == "low")
        next
      cols_k <- resp$columns[[k]]
      cols_k <- as.character(unlist(cols_k))
      cols_k <- cols_k[cols_k %in% nms]                 # only real columns
      if (length(cols_k) < .scale_min_items) next       # ignore tiny fragments
      # Corroboration: if the identified scale name (or its acronym) appears in
      # the retrieved paper sentences OR matches an instrument the regex scan
      # found named in the paper, the manuscript itself names this instrument —
      # promote to high confidence.
      final_conf <- if (nzchar(conf)) conf else "medium"
      if ((length(ctx) > 0 && .scale_name_in_text(scale, ctx)) ||
          (length(paper_scales) > 0 && .scale_name_in_text(scale, paper_scales)))
        final_conf <- "high"
      # Apply this identification to EVERY file sharing the signature.
      for (f in files_here)
        out[[length(out) + 1L]] <- data.frame(
          source_file = f, column_name = cols_k,
          scale = scale, confidence = final_conf)
    }
  }

  res <- if (length(out) > 0) dplyr::bind_rows(out) else
    data.frame(source_file = character(), column_name = character(),
               scale = character(), confidence = character())
  attr(res, "llm_model")  <- model_used
  attr(res, "n_detected") <- n_detected
  attr(res, "n_skipped")  <- n_skipped
  res
}

# LLM tiers 2+3: fuzzy-match still-unlabelled columns to still-unmatched codebook
# variables, and merge conflicting label definitions into a canonical label.
# Returns list(labels_df, n_matched, n_merged, model).
codebook_match_llm <- function(labels_df, columns_df, codebook_vars_df,
                               model, params) {
  n_matched <- 0L
  n_merged  <- 0L
  model_used <- NA_character_
  norm_col <- normalize_varname(labels_df$column_name)

  # Tier: merge conflicting definitions.
  conflict_idx <- which(labels_df$label_status == "conflicting_definition")
  if (length(conflict_idx) > 0) {
    conflict_cols <- unique(labels_df$column_name[conflict_idx])
    type_spec <- ellmer::type_object(
      equivalent = ellmer::type_boolean("Whether all labels describe the same construct"),
      canonical  = ellmer::type_string("Best single label if equivalent, else empty",
                                        required = FALSE)
    )
    prompt <- paste(
      "You are reviewing whether multiple label definitions for the same variable in a",
      "psychology dataset describe the same construct. If they do, return equivalent=true and",
      "the most human-readable single label as canonical; otherwise equivalent=false."
    )
    for (cn in conflict_cols) {
      idx1 <- conflict_idx[labels_df$column_name[conflict_idx] == cn][1]
      labs <- strsplit(labels_df$label[idx1], " | ", fixed = TRUE)[[1]]
      txt <- sprintf("Column: %s\nCandidate labels:\n%s", cn,
                     paste0("- ", labs, collapse = "\n"))
      resp <- tryCatch(
        llm(text = data.frame(text = txt), text_col = "text",
            system_prompt = prompt, type = type_spec, model = model,
            params = params, phase = "Matching codebook columns"),
        error = function(e) NULL
      )
      if (is.null(resp) || nrow(resp) == 0) next
      if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_
      if (!isTRUE(resp$equivalent[1])) next
      canonical <- resp$canonical[1] %||% ""
      if (is.na(canonical) || !nzchar(canonical)) next
      apply_idx <- conflict_idx[labels_df$column_name[conflict_idx] == cn]
      labels_df$label[apply_idx]        <- canonical
      labels_df$label_status[apply_idx] <- "labelled"
      labels_df$label_method[apply_idx] <- "merged_llm"
      n_merged <- n_merged + length(apply_idx)
    }
  }

  # Tier: fuzzy-match unlabelled columns to unmatched codebook variables.
  documented <- labels_df$label_status %in% c("labelled", "llm")
  unlabelled_idx <- which(labels_df$label_status == "unlabelled")
  matched_norm <- unique(normalize_varname(
    labels_df$codebook_variable[documented & !is.na(labels_df$codebook_variable)]
  ))
  unmatched_vars <- codebook_vars_df[
    !normalize_varname(codebook_vars_df$codebook_variable) %in% matched_norm,
    , drop = FALSE
  ]

  if (length(unlabelled_idx) > 0 && nrow(unmatched_vars) > 0) {
    unlab_cols <- unique(labels_df$column_name[unlabelled_idx])
    # Object-wrapped array (see note above): gpt-oss-20b 400s on a bare array.
    type_spec <- ellmer::type_object(
      matches = ellmer::type_array(
        ellmer::type_object(
          column_name       = ellmer::type_string("Exact column name from the data list"),
          codebook_variable = ellmer::type_string("Exact variable name from the codebook list")
        )
      )
    )
    prompt <- paste(
      "You are matching data column names to codebook variable names for a psychology dataset.",
      "Return only confident pairings referring to the same construct (abbreviations, naming",
      "conventions, underscores vs spaces). Do not guess; both names must appear verbatim in",
      "the lists provided."
    )
    norm_unmatched <- normalize_varname(unmatched_vars$codebook_variable)

    # Batch both sides so a large repo (hundreds of unlabelled columns and/or
    # codebook variables) never sends one oversized request -> HTTP 400. Each
    # call pairs a block of columns against a block of codebook variables; the
    # response indices are validated against the verbatim names anyway, so the
    # cross-batch product still matches correctly.
    col_batches <- split(seq_along(unlab_cols),
                         ceiling(seq_along(unlab_cols) / 50))
    var_batches <- split(seq_len(nrow(unmatched_vars)),
                         ceiling(seq_len(nrow(unmatched_vars)) / 50))

    for (ci in col_batches) {
      cols_b <- unlab_cols[ci]
      for (vi in var_batches) {
        vars_b <- unmatched_vars$codebook_variable[vi]
        txt <- paste0(
          "Data columns (unlabelled):\n",
          paste(seq_along(cols_b), cols_b, sep = ". ", collapse = "\n"),
          "\n\nCodebook variables (unmatched):\n",
          paste(seq_along(vars_b), vars_b, sep = ". ", collapse = "\n")
        )
        resp <- tryCatch(
          llm(text = data.frame(text = txt), text_col = "text",
              system_prompt = prompt, type = type_spec, model = model,
              params = params, phase = "Matching codebook columns"),
          error = function(e) NULL
        )
        resp <- .strip_llm_wrapper(resp, "matches")
        if (is.null(resp) || nrow(resp) == 0 ||
            !all(c("column_name", "codebook_variable") %in% names(resp))) next
        if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_
        for (k in seq_len(nrow(resp))) {
          pnc <- normalize_varname(resp$column_name[k])
          pnv <- normalize_varname(resp$codebook_variable[k])
          if (!pnc %in% norm_col[unlabelled_idx] || !pnv %in% norm_unmatched) next
          var_row <- which(norm_unmatched == pnv)[1]
          rows <- which(norm_col == pnc & labels_df$label_status == "unlabelled")
          if (length(rows) == 0 || is.na(var_row)) next
          labels_df$label[rows]             <- unmatched_vars$label[var_row]
          labels_df$codebook_variable[rows] <- unmatched_vars$codebook_variable[var_row]
          labels_df$label_source[rows]      <- unmatched_vars$codebook_source[var_row]
          labels_df$label_status[rows]      <- "llm"
          labels_df$label_method[rows]      <- "llm"
          n_matched <- n_matched + length(rows)
        }
      }
    }
  }

  list(labels_df = labels_df, n_matched = n_matched, n_merged = n_merged,
       model = model_used)
}
