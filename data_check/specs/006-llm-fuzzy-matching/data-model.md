# Data Model: LLM Fuzzy Column Matching

**Feature**: 006-llm-fuzzy-matching
**Date**: 2026-03-16

---

## Modified Output File

### `structure/<paper_id>_labels.csv` — new column added

One new column appended to the existing schema (all other columns unchanged):

| Column | Type | Description |
|---|---|---|
| `label_method` | character | How the label was determined: `"rules"` = normalized string match; `"llm"` = secondary LLM pass; `NA` = column is unlabelled |

**Full updated column order** (new column in bold):

`paper_id`, `source_file`, `column_name`, `group`, `label`, `codebook_variable`, `label_source`, `label_status`, **`label_method`**

---

## `label_status` Extension

One new value added to the existing `label_status` vocabulary:

| Value | Meaning |
|---|---|
| `labelled` | Matched by normalized string comparison (existing) |
| `unlabelled` | No match found by either rules or LLM (existing) |
| `conflicting_definition` | Multiple codebooks disagree on the label (existing) |
| `ambiguous_experiment` | Name exists only in a different experiment group (existing) |
| `no_codebook` | Paper has no codebook files (existing) |
| **`llm`** | **Matched by secondary LLM pass (new)** |

---

## New Constant (in `2_codebook_label.R`)

| Constant | Purpose |
|---|---|
| `COLUMN_MATCH_PROMPT` | System prompt for the secondary LLM column-matching call |

---

## Modified Helper (in `helper.R`)

### `match_column_labels(columns_df, codebook_vars_df, column_match_prompt = NULL)`

**New parameter**: `column_match_prompt` — when non-NULL, activates the secondary LLM pass after rule-based matching.

**Extended logic** (appended after existing rule-based loop):

1. Collect `unlabelled_cols`: rows from `columns_df` where `status_out == "unlabelled"`, extract unique `(column_name, group)` pairs.
2. Collect `unmatched_vars`: rows from `codebook_vars_df` whose `normalize_varname(codebook_variable)` does not appear in any `labelled`/`llm` row's `norm_var`. Respect group scoping.
3. If either set is empty → skip LLM, proceed to return.
4. Build prompt body listing unlabelled column names and unmatched codebook variable names.
5. Call `llm(system_prompt = column_match_prompt, text = prompt_body)`.
6. Parse response with `extract_json()` + `jsonlite::fromJSON()`.
7. Validate: keep only pairs where both sides normalise to a known candidate.
8. For each valid pair, update `label_out`, `cbk_var_out`, `src_out`, `status_out` (`"llm"`).
9. Set `label_method_out`: `"rules"` for all rows with `status_out == "labelled"`, `"llm"` for `"llm"`, `NA` otherwise.
10. Append `label_method = label_method_out` to the return data.frame.

---

## Candidate Sets (in-memory only)

| Concept | Description |
|---|---|
| `unlabelled_cols` | Vector of normalised column names with `label_status == "unlabelled"` after rule pass |
| `unmatched_vars` | Data.frame of codebook vars not yet matched, with `norm_var` key |
| LLM response | `data.frame` with columns `column_name`, `codebook_variable` (raw, not normalised) |
