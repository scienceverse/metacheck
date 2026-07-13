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
#' **universe/filter** columns when present. These are carried onto the matched
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
  .haven_exts     <- c("sav", "dta", "sas7bdat")
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
      summary_text = text
    )
  }

  if (is.null(columns_df) || nrow(columns_df) == 0)
    return(empty_summary("We found no extracted data columns to check against a codebook."))

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

  # Data files carrying embedded haven labels (SPSS/Stata/SAS).
  haven_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      !is.na(structure_df$data_type) & structure_df$data_type == "data" &
        tolower(tools::file_ext(structure_df$file_name)) %in% .haven_exts &
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

  # Harvest embedded haven labels directly from data files.
  if (nrow(haven_rows) > 0 && requireNamespace("haven", quietly = TRUE)) {
    for (p in haven_rows$file_location) {
      ext <- tolower(tools::file_ext(p))
      df <- tryCatch(switch(ext,
        sav      = as.data.frame(haven::read_sav(p, n_max = 0L)),
        dta      = as.data.frame(haven::read_dta(p, n_max = 0L)),
        sas7bdat = as.data.frame(haven::read_sas(p, n_max = 0L))
      ), error = function(e) NULL)
      if (is.null(df)) next
      res <- .extract_haven_labels(df, basename(p))
      if (is.null(res)) next
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

  # Stage 3: LLM self-generated fallback.
  if (llm_use() && have_previews) {
    sg <- .identify_scales_selfgen(previews, labels_df, paper, model, params,
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

  # OSD export of the full per-group inventory (named + unmatched groups kept).
  # Response scale lets the codebook lead (columns_df = data_check stats fallback,
  # labels_df = codebook value labels + item wording).
  scales_osd <- if (!is.null(scale_groups) && nrow(scale_groups) > 0)
    .scales_to_osd(scale_groups, columns_df, labels_df) else list()

  named <- !is.na(labels_df$scale) & nzchar(labels_df$scale)
  n_scales_found <- length(unique(labels_df$scale[named]))
  files_named    <- unique(labels_df$source_file[named])
  n_scale_unnamed <- max(0L, n_scale_files - length(files_named))

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
        n_unused, plural(n_unused), if (n_unused == 1) "s" else "")
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
    scale_unnamed_n = n_scale_unnamed
  )

  list(
    table = labels_df,
    codebook_vars = codebook_vars_df,
    scales_osd = scales_osd,           # per-group inventory in OpenScales OSD form
    summary_table = summary_table,
    na_replace = c(column_n = 0, matched_n = 0, unmatched_n = 0, clean_n = 0,
                   conflicted_n = 0, codebook_var_n = 0, unused_var_n = 0,
                   scale_blocks_n = 0, scale_named_n = 0, scale_unnamed_n = 0),
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

# Regex matching an instrument in running text: its full name (tolerant of
# spacing/dash/punctuation) OR its acronym as a whole word. Built per dictionary
# row. NA acronym -> name only.
.scale_text_pattern <- function(name, acronym) {
  toks <- unlist(strsplit(name, "[^A-Za-z0-9]+"))
  toks <- toks[nzchar(toks)]
  name_pat <- paste(vapply(toks, function(t)
    gsub("([][{}().^$*+?|\\\\-])", "\\\\\\1", t), character(1)),
    collapse = "[\\s._/-]*")
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
  if (!.is_paper(paper) || is.null(paper$text) || nrow(paper$text) == 0)
    return(character(0))
  dict <- .scale_dictionary()
  if (nrow(dict) == 0) return(character(0))
  hay <- paste(as.character(paper$text$text), collapse = " \n ")
  hit <- vapply(seq_len(nrow(dict)), function(i) {
    pat <- .scale_text_pattern(dict$name[i], dict$acronym[i])
    isTRUE(tryCatch(grepl(pat, hay, perl = TRUE, ignore.case = TRUE),
                    error = function(e) FALSE))
  }, logical(1))
  unique(dict$name[hit])
}

# Common words to exclude from "distinctive item word" searches.
.scale_stopwords <- c("scale","agree","disagree","strongly","never","always",
  "sometimes","often","rarely","please","following","statement","question",
  "response","really","think","feel","would","about","which","there","their",
  "other","because","being")

# Does the identified scale name (or a parenthesised acronym within it) appear
# in the retrieved paper sentences? Used to corroborate an identification.
.scale_name_in_text <- function(scale, sentences) {
  if (!nzchar(scale) || length(sentences) == 0) return(FALSE)
  hay <- tolower(paste(sentences, collapse = " \n "))
  # The full name, and any ALL-CAPS acronym in parentheses (e.g. "(PANAS)").
  needles <- tolower(scale)
  acr <- regmatches(scale, gregexpr("\\(([A-Z][A-Za-z0-9-]{1,})\\)", scale))[[1]]
  acr <- gsub("[()]", "", acr)
  needles <- c(needles, tolower(acr),
               tolower(sub("\\s*\\(.*\\)\\s*$", "", scale)))  # name without acronym
  needles <- unique(needles[nchar(needles) >= 3])
  any(vapply(needles, function(n) grepl(n, hay, fixed = TRUE), logical(1)))
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
      } else if (sum(corr) == 0L && length(cand) == 1L &&
                 nchar(norm_pref(dict$acronym[cand])) >= 4L) {
        pick <- cand; conf <- "medium"          # data-only, safe acronym
      }
      # else: 0 candidates left, several corroborated (still ambiguous), or a
      # lone short-acronym with no corroboration -> ABSTAIN.

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

# Group a data file's columns by leading abbreviation. Returns a list, one entry
# per prefix with >= min_cols columns, carrying the column names, the count, and
# the maximum numeric suffix seen across the columns (tentative item count vs.
# declared length — kept separate, they can disagree when subscale/computed
# columns inflate the count). Not restricted to Likert columns (DEM_* etc. count).
.scale_prefix_groups <- function(df, min_cols = .scale_min_items) {
  nms <- names(df)
  pfx <- vapply(nms, .scale_prefix_of, character(1))
  # A prefix is a candidate abbreviation only if it is short-ish and alnum
  # (BSQ, MBSC, DEM_RACE would collapse to DEM). Drop empty / very long prefixes.
  keep <- nzchar(pfx) & nchar(pfx) <= 12
  groups <- split(nms[keep], tolower(pfx[keep]))
  out <- list()
  for (g in names(groups)) {
    cols <- groups[[g]]
    if (length(cols) < min_cols) next
    # max numeric suffix across the columns (BSQ_43 -> 43): the declared length.
    nums <- suppressWarnings(as.integer(sub(".*?([0-9]+)$", "\\1",
             grep("[0-9]+$", cols, value = TRUE))))
    max_item <- if (length(nums)) max(nums, na.rm = TRUE) else NA_integer_
    out[[g]] <- list(
      prefix     = g,
      display    = .scale_prefix_of(cols[[1]]),   # original-case abbreviation
      columns    = cols,
      n_columns  = length(cols),
      max_item   = max_item)
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
    "the item wording; plus sentences from the paper that mention these",
    "abbreviations, and an optional list of known instruments. For EACH group,",
    "give the instrument the abbreviation stands for, taken from the paper",
    "sentences (an abbreviation is often defined as 'Name of Scale (ABBR)'). Use",
    "the item counts to disambiguate (e.g. a 'X-item scale' in the text). If the",
    "text does not identify the group, return an empty scale_name for it — do NOT",
    "guess and do NOT force a known instrument. Return one entry per group.")

  out <- list(); model_used <- NA_character_; n_calls <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    groups <- .scale_prefix_groups(df)
    if (length(groups) == 0) next

    # Sentences mentioning ANY of this file's abbreviations (one search per file).
    prefixes <- vapply(groups, function(g) g$display, character(1))
    sents <- if (.is_paper(paper))
      .scale_prefix_sentences(paper, prefixes) else character(0)

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

    text_in <- paste0(
      "Column groups in this data file:\n", paste(grp_txt, collapse = "\n"),
      if (length(sents)) paste0("\n\nSentences from the paper mentioning these abbreviations:\n",
        paste("-", sents, collapse = "\n")) else "\n\n(No paper sentences mention these abbreviations.)",
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

  # 2. Observed statistics from data_check.
  if (!is.null(columns_df) && nrow(columns_df) > 0 &&
      all(c("source_file", "column_name", "min", "max", "n_unique") %in% names(columns_df))) {
    ck <- key(columns_df)
    idx <- which(ck %in% want)
    if (length(idx)) {
      mn <- suppressWarnings(min(columns_df$min[idx], na.rm = TRUE))
      mx <- suppressWarnings(max(columns_df$max[idx], na.rm = TRUE))
      nu <- suppressWarnings(max(columns_df$n_unique[idx], na.rm = TRUE))
      # Only claim a scale when the range is a small integer-like span.
      if (is.finite(mn) && is.finite(mx) && mx > mn &&
          mn == round(mn) && mx == round(mx) && (mx - mn) <= 20) {
        return(list(points = if (is.finite(nu)) nu else (mx - mn + 1L),
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
  label_of <- function(file, col) {
    if (!length(lbl_key)) return(NA_character_)
    i <- match(paste(file, col, sep = "\x01"), lbl_key)
    if (is.na(i)) return(NA_character_)
    w <- labels_df$label[i]
    if (!is.na(w) && nzchar(w)) return(w)
    if ("question" %in% names(labels_df) && !is.na(labels_df$question[i]) &&
        nzchar(labels_df$question[i])) return(labels_df$question[i])
    NA_character_
  }

  lapply(seq_len(nrow(scale_groups)), function(i) {
    r <- scale_groups[i, ]
    cols <- r$columns[[1]]
    named <- !is.na(r$scale) && nzchar(r$scale)
    cp <- .osd_code_and_provenance(r$scale, r$prefix, r$scale_source, dict)

    # Item wording -> translations; text_key points at the id when documented.
    wording <- stats::setNames(vapply(cols, function(c) label_of(r$source_file, c),
                                      character(1)), cols)
    translations_en <- as.list(wording[!is.na(wording) & nzchar(wording)])

    lopts <- .osd_likert_options(cols, r$source_file, columns_df, labels_df)

    scale_info <- Filter(Negate(is.null), list(
      name         = if (named) r$scale else "",
      code         = cp$code,
      abbreviation = r$prefix))

    definition <- list(scale_info = scale_info)
    if (!is.null(lopts)) definition$likert_options <- lopts
    definition$items <- lapply(seq_along(cols), function(k) {
      it <- list(id = cols[k], text_key = cols[k])
      if (!is.null(lopts)) it$type <- "likert"
      it
    })
    # metacheck provenance extension (kept out of the spec's scale_info).
    definition$metacheck <- Filter(Negate(is.null), list(
      scale_source    = cp$source,
      provenance      = cp$provenance,
      confidence      = if (!is.na(r$confidence)) r$confidence else NULL,
      source_file     = r$source_file,
      n_columns       = r$n_columns,
      declared_length = if (!is.na(r$max_item)) r$max_item else NULL))

    osd <- list(osd_version = "1.0", definition = definition)
    if (length(translations_en))
      osd$translations <- list(en = translations_en)
    # Marker for the writer: only named scales become files.
    attr(osd, "write") <- named
    attr(osd, "code")  <- cp$code
    osd
  })
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
    "You are given the item wording of ONE block of survey items from a dataset,",
    "and (optionally) sentences from the paper that mention these variables.",
    "These items do NOT match a known published instrument. Give a short, natural",
    "CONSTRUCT LABEL for what the block measures, based ONLY on the provided text.",
    "If the provided text does not clearly indicate what the items measure, return",
    "an empty construct — do NOT guess from variable names alone. Never invent a",
    "published scale name; describe the construct in plain words.")

  out <- list(); model_used <- NA_character_; n_calls <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) < .scale_min_items) next
    for (cols in .detect_scale_blocks(df)) {
      nms <- names(df)[cols]
      keys <- paste(file, nms, sep = "\x01")
      if (any(keys %in% named_key)) next          # already named — skip
      wording <- wording_of(file, nms)
      ctx <- if (have_paper)
        .scale_paper_context(paper, .scale_name_prefix(nms[[1]]), wording) else
        character(0)
      # Need SOME descriptive evidence — never label from bare names.
      if (length(wording) == 0 && length(ctx) == 0) next
      if (n_calls >= max_calls) break
      n_calls <- n_calls + 1L

      text_in <- paste0(
        "Item wording:\n",
        if (length(wording)) paste("-", wording, collapse = "\n") else "(none)",
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
      out[[length(out) + 1L]] <- data.frame(
        source_file = file, column_name = nms,
        scale = construct, confidence = conf, scale_source = "self_generated")
    }
    if (n_calls >= max_calls) break
  }
  res <- if (length(out)) dplyr::bind_rows(out) else empty
  attr(res, "llm_model") <- model_used
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

  # Scales the paper names outright (regex over the full text). Offering these
  # as candidates lets the model CONFIRM a stated instrument — far more reliable
  # than guessing from item wording — so they lift identifications to high
  # confidence when the columns match.
  paper_scales <- if (have_paper_text) .scan_paper_for_scales(paper) else character(0)

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
