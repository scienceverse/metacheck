# 2_codebook_label.R
# ─────────────────────────────────────────────────────────────────────────────
# Reads codebook/README files for a paper and labels data columns accordingly.
#
# Exports: run_codebook_label(paper_id)
#
# Input:  paper_id (character)
#         outputs/<paper_id>/structure.csv  (from 0_index.R)
#         outputs/<paper_id>/columns.csv   (from 0_index.R)
# Output: outputs/<paper_id>/labels.csv
#         outputs/<paper_id>/codebook_coverage.csv
#
# Returns: list(
#   labels_df, coverage_df,
#   n_labelled, n_unlabelled,
#   n_codebook_vars, n_matched_vars,
#   label_status   # "ok" | "no_match" | "no_codebook"
# )
#
# Sourcing this file defines run_codebook_label() only — no side effects.
# ─────────────────────────────────────────────────────────────────────────────

library(metacheck)
source("./data_check/helper.R")

llm_use(TRUE)
llm_model("ollama/gpt-oss:20b-cloud")

# ── Constants ─────────────────────────────────────────────────────────────────

OUTPUT_DIR             <- "./data_check/outputs"
LLM_BATCH_SIZE         <- 20L   # shared constant — needed by llm_batch() in helper.R
MAX_CODEBOOK_LLM_CALLS <- 3L    # max LLM calls per paper for codebook text parsing
MAX_CODEBOOK_FILE_MB   <- 100   # codebook files larger than this (MB) are skipped
CODEBOOK_TYPES         <- c("codebook", "readme")

CODEBOOK_PARSE_PROMPT <- 'You are extracting variable definitions from a psychology research codebook or README.
Return a JSON array — one object per variable found.
Each object: {"variable_name": "<exact variable name>", "label": "<human-readable description>", "experiment_context": "<experiment or study name if stated, else null>"}

Rules:
- variable_name: the exact code/name used in the data file (e.g. "rt", "subj_id", "condition")
- label: a concise human-readable description of what the variable measures
- experiment_context: if the variable is described under a heading like "Experiment 1" or "Study 2a", include that heading verbatim; otherwise null
- Only include variables that have both a name and a description
- If the text contains no variable definitions, return an empty array: []
- Output ONLY the JSON array. No notes, no text outside the array.'

COLUMN_MATCH_PROMPT <- 'You are matching data column names to codebook variable names for a psychology research dataset.
You will receive two lists: unlabelled data column names and unmatched codebook variable names.
Return a JSON array of confident pairings only.
Each object: {"column_name": "<exact column name from the data list>", "codebook_variable": "<exact variable name from the codebook list>"}

Rules:
- Only include pairs you are confident refer to the same construct (e.g. abbreviations, naming conventions, underscores vs spaces)
- Do NOT guess — if unsure, omit the pair
- Both column_name and codebook_variable must appear verbatim from the lists provided
- If no confident matches exist, return an empty array: []
- Output ONLY the JSON array. No notes, no text outside the array.'

LABEL_MERGE_PROMPT <- 'You are reviewing whether multiple label definitions for the same
variable in a psychology research dataset are semantically equivalent.

You will receive a JSON array of objects, each with "column" and "labels" fields.
Return a JSON array — one object per input variable.
Each object: {"column": "<column_name>", "equivalent": true/false, "canonical": "<best label or null>"}

Rules:
- equivalent: true if all listed labels describe the same construct (synonyms, different
  phrasings, or value-coding notation for the same concept as a semantic label)
- canonical: if equivalent=true, return the most human-readable, informative single label;
  if equivalent=false, set to null
- Do NOT mark as equivalent if labels describe genuinely different constructs or scales
- Output ONLY the JSON array. No notes, no text outside the array.'

# ── Pipeline function ─────────────────────────────────────────────────────────

run_codebook_label <- function(paper_id) {

  # ── 1. Load inputs ──────────────────────────────────────────────────────────

  structure_path <- file.path(paper_output_dir(paper_id), "structure.csv")
  columns_path   <- file.path(paper_output_dir(paper_id), "columns.csv")

  if (!file.exists(structure_path))
    stop("Structure file not found: ", structure_path,
         "\nRun 0_index.R for this paper first.")
  if (!file.exists(columns_path))
    stop("Columns file not found: ", columns_path,
         "\nRun 0_index.R for this paper first.")

  structure_df <- read.csv(structure_path, stringsAsFactors = FALSE,
                            colClasses = c(paper_id = "character"))
  columns_df   <- read.csv(columns_path,   stringsAsFactors = FALSE,
                            colClasses = c(paper_id = "character"))

  # Support both "group" (0_index schema) and "experiment_group" (1_data_label schema)
  col_group <- if ("group" %in% names(columns_df)) columns_df$group else
               if ("experiment_group" %in% names(columns_df)) columns_df$experiment_group else
               rep(NA_character_, nrow(columns_df))

  message("── Codebook labelling for paper ", paper_id)
  message("   ", nrow(columns_df), " data column(s) to label")

  # ── 2. Locate codebook/readme files ─────────────────────────────────────────

  codebook_rows <- structure_df[structure_df$type %in% CODEBOOK_TYPES, , drop = FALSE]

  # ── 3. Parse codebooks or handle no_codebook case ────────────────────────────

  if (nrow(codebook_rows) == 0) {
    message("── No codebook files found — all columns marked 'no_codebook'")
    labels_df <- data.frame(
      paper_id          = columns_df$paper_id,
      source_file       = columns_df$source_file,
      column_name       = columns_df$column_name,
      group             = col_group,
      label             = NA_character_,
      codebook_variable = NA_character_,
      label_source      = NA_character_,
      label_status      = "no_codebook",
      label_method      = NA_character_,
      stringsAsFactors  = FALSE
    )
    codebook_vars_df <- data.frame(
      codebook_variable = character(0), label = character(0),
      codebook_source   = character(0), group = character(0),
      stringsAsFactors  = FALSE
    )

  } else {
    message("── Parsing ", nrow(codebook_rows), " codebook/readme file(s)")
    parsed_list <- lapply(codebook_rows$path, function(p) {
      message("  → ", basename(p))
      parse_codebook(p)
    })
    parsed_list <- Filter(Negate(is.null), parsed_list)

    if (length(parsed_list) == 0) {
      message("── No variables extracted from any codebook — all columns unlabelled")
      codebook_vars_df <- data.frame(
        codebook_variable = character(0), label = character(0),
        codebook_source   = character(0), group = character(0),
        stringsAsFactors  = FALSE
      )
    } else {
      codebook_vars_df <- do.call(rbind, parsed_list)
      # Drop exact duplicates (same normalised variable name + label + group)
      dup_key <- paste(normalize_varname(codebook_vars_df$codebook_variable),
                       codebook_vars_df$label,
                       ifelse(is.na(codebook_vars_df$group), "", codebook_vars_df$group),
                       sep = "\x01")
      codebook_vars_df <- codebook_vars_df[!duplicated(dup_key), , drop = FALSE]
      message("── Extracted ", nrow(codebook_vars_df),
              " unique codebook variable definition(s)")
    }

    # ── 4. Match columns against codebook ──────────────────────────────────────
    labels_df <- match_column_labels(columns_df, codebook_vars_df,
                                     column_match_prompt = COLUMN_MATCH_PROMPT,
                                     label_merge_prompt  = LABEL_MERGE_PROMPT)
  }

  # ── 5. Write _labels.csv ─────────────────────────────────────────────────────

  labels_out <- file.path(paper_output_dir(paper_id), "labels.csv")
  write.csv(labels_df, labels_out, row.names = FALSE)
  n_labelled <- sum(labels_df$label_status == "labelled")
  message("── Saved labels → ", labels_out,
          "  (", n_labelled, "/", nrow(labels_df), " columns labelled)")

  # ── 6. Build codebook coverage table ─────────────────────────────────────────

  if (nrow(codebook_vars_df) > 0) {
    matched_norm <- unique(normalize_varname(
      labels_df$codebook_variable[labels_df$label_status %in% c("labelled", "llm") &
                                    !is.na(labels_df$codebook_variable)]
    ))
    coverage_df <- data.frame(
      paper_id          = paper_id,
      codebook_variable = codebook_vars_df$codebook_variable,
      label             = codebook_vars_df$label,
      codebook_source   = codebook_vars_df$codebook_source,
      group             = codebook_vars_df$group,
      match_status      = ifelse(
        normalize_varname(codebook_vars_df$codebook_variable) %in% matched_norm,
        "matched", "unmatched_in_data"
      ),
      stringsAsFactors  = FALSE
    )
  } else {
    coverage_df <- data.frame(
      paper_id          = character(0),
      codebook_variable = character(0),
      label             = character(0),
      codebook_source   = character(0),
      group             = character(0),
      match_status      = character(0),
      stringsAsFactors  = FALSE
    )
  }

  # ── 7. Write _codebook_coverage.csv ──────────────────────────────────────────

  coverage_out <- file.path(paper_output_dir(paper_id), "codebook_coverage.csv")
  write.csv(coverage_df, coverage_out, row.names = FALSE)
  n_matched <- sum(coverage_df$match_status == "matched")
  message("── Saved coverage → ", coverage_out,
          "  (", n_matched, "/", nrow(coverage_df), " codebook vars matched)")

  # ── 8. Return LabellingResult ─────────────────────────────────────────────────

  n_unlabelled   <- sum(labels_df$label_status %in% c("unlabelled", "no_codebook"))
  overall_status <- if (nrow(codebook_vars_df) == 0) "no_codebook" else
                    if (n_labelled == 0) "no_match" else "ok"

  list(
    labels_df       = labels_df,
    coverage_df     = coverage_df,
    n_labelled      = n_labelled,
    n_unlabelled    = n_unlabelled,
    n_codebook_vars = nrow(coverage_df),
    n_matched_vars  = n_matched,
    label_status    = overall_status
  )
}
