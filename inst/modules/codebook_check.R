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

  # ── 4b. Identify psychometric scales (LLM) ───────────────────────────────────
  # Detect blocks of Likert items and ask the LLM which named instrument each is
  # (PANAS, Rosenberg Self-Esteem, ...). Adds `scale` + `scale_confidence` to
  # every item column of a confidently identified block. Only runs with an LLM
  # and readable data (previews); otherwise the columns are added empty so the
  # table schema is stable.
  labels_df$scale            <- NA_character_
  labels_df$scale_confidence <- NA_character_
  n_scales_found     <- 0L   # distinct named scales identified
  n_scale_files      <- 0L   # data files that contain scale-like item blocks
  n_scale_unnamed    <- 0L   # of those files, how many yielded no named scale
  if (llm_use() && !is.null(previews) && length(previews) > 0) {
    sc <- codebook_identify_scales(previews, labels_df, model, params,
                                   paper = paper, max_calls = codebook_max_calls)
    scale_gate <- attr(sc, "gated")
    if (!is.null(scale_gate)) {
      # Over the budget → report and skip scale identification.
      cap_report(scale_gate)
      gate_msgs <- c(gate_msgs, scale_gate)
    }
    n_scale_files <- attr(sc, "n_detected") %||% 0L
    if (!is.null(sc) && nrow(sc) > 0) {
      # Merge scale assignments back onto labels_df by (source_file, column_name).
      key      <- paste(labels_df$source_file, labels_df$column_name, sep = "\x01")
      sc_key   <- paste(sc$source_file, sc$column_name, sep = "\x01")
      m        <- match(key, sc_key)
      labels_df$scale[!is.na(m)]            <- sc$scale[m[!is.na(m)]]
      labels_df$scale_confidence[!is.na(m)] <- sc$confidence[m[!is.na(m)]]
      n_scales_found <- length(unique(sc$scale[!is.na(sc$scale) & nzchar(sc$scale)]))
    }
    # Files that had scale-like blocks but ended up with no identified scale.
    files_named <- unique(labels_df$source_file[!is.na(labels_df$scale)])
    n_scale_unnamed <- max(0L, n_scale_files - length(files_named))
    if (is.na(llm_model_used))
      llm_model_used <- attr(sc, "llm_model") %||% NA_character_
  }

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
  # Report identified psychometric scales, and — when item-blocks look like
  # scales but could not be named — tell the user what to improve so a future
  # run (or a human reader) can identify them.
  if (n_scale_files > 0 || n_scales_found > 0) {
    scale_report <- "#### Scales"
    if (n_scales_found > 0) {
      scale_tbl <- labels_df[!is.na(labels_df$scale) & nzchar(labels_df$scale), ,
                             drop = FALSE]
      scale_tbl <- scale_tbl |>
        dplyr::distinct(.data$source_file, .data$scale, .data$scale_confidence) |>
        dplyr::transmute(File = .data$source_file, Scale = .data$scale,
                         Confidence = .data$scale_confidence)
      scale_report <- c(scale_report,
        sprintf("We identified %d psychometric scale%s in the data (schema.org `measurementTechnique` in the Psych-DS output).",
                n_scales_found, plural(n_scales_found)),
        scroll_table(scale_tbl, maxrows = 20))
    }
    # Guidance for blocks that look like a scale but could not be named.
    if (n_scale_unnamed > 0) {
      scale_report <- c(scale_report,
        sprintf("%d data file%s with survey-item blocks could not be matched to a named instrument.",
                n_scale_unnamed, plural(n_scale_unnamed)),
        paste(
          "To make these scales identifiable — by this tool and by anyone reusing the data — consider:",
          "\n- **Name variables after the instrument**: use a consistent prefix per scale (e.g. `panas_1 … panas_10`, `rse_1 … rse_10`) rather than generic `Q1`, `V3`, or `item5`.",
          "\n- **Document the item wording** in a codebook (a `variable, description` table, or embedded value labels in SPSS/Stata files): the exact item text is what identifies a scale.",
          "\n- **State the scale name and reference** in the codebook or README, including the response options (e.g. 1 = *Strongly disagree* … 5 = *Strongly agree*) and any reverse-coded items.",
          sep = ""))
    }
    report <- c(report, scale_report)
  } else if (llm_use() && !is.null(previews) && length(previews) > 0) {
    # LLM ran but found no scale-like item blocks at all — nothing to advise.
  } else if (!llm_use()) {
    report <- c(report,
      "#### Scales",
      "Psychometric-scale identification is skipped without an LLM. Enable one with `llm_use(TRUE)` to detect which named instruments (PANAS, Rosenberg Self-Esteem, ...) the survey items form.")
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
    unused_var_n   = n_unused
  )

  list(
    table = labels_df,
    codebook_vars = codebook_vars_df,
    summary_table = summary_table,
    na_replace = c(column_n = 0, matched_n = 0, unmatched_n = 0, clean_n = 0,
                   conflicted_n = 0, codebook_var_n = 0, unused_var_n = 0),
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
    "Slider), a confidence (high/medium/low), and the exact list of its member",
    "column names. Use the item wording when present; you MAY also recognise a",
    "scale from a well-known variable-name prefix (RSE, PANAS, BFI, PSS, DOS).",
    "If sentences from the paper are provided, use them — they often name the",
    "instrument outright (e.g. 'the 20-item PANAS'); prefer the name stated in",
    "the paper. Omit any columns you cannot confidently assign to a named scale",
    "— do not invent scales for generic names like Q1, V3, item5."
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
    text_in <- if (length(ctx) > 0)
      paste0(listing,
             "\n\nRelevant sentences from the paper (may name the instrument):\n",
             paste("-", ctx, collapse = "\n"))
    else listing

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
      # the retrieved paper sentences, the manuscript itself names this
      # instrument — promote to high confidence.
      final_conf <- if (nzchar(conf)) conf else "medium"
      if (length(ctx) > 0 && .scale_name_in_text(scale, ctx))
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
