# Implementation Plan: Column Type Classification and Value Normalization

**Branch**: `002-column-type-classification` | **Date**: 2026-03-16 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/002-column-type-classification/spec.md`

---

## Summary

Add a `col_type` column to every `*_columns.csv` output by classifying each data column as `continuous`, `binary`, `categorical`, `ordinal`, `date`, `id`, `text`, or a normalization variant. A rule-based fast path handles clear-cut cases; an LLM batch call handles ambiguous cases. Malformed numeric columns (comma-as-decimal-separator) are detected and normalized so statistics are computed rather than silently omitted.

---

## Technical Context

**Language/Version**: R (no version constraint beyond existing project requirements)
**Primary Dependencies**: `metacheck`, `jsonlite`, `readxl`, `haven` — all already in use; no new packages required
**Storage**: CSV files in `data_check/structure/` — additive schema change only
**Testing**: Manual regression test against paper `0956797620903716`; bulk run on representative sample
**Target Platform**: macOS/Linux (same as existing pipeline)
**Project Type**: Data pipeline script
**Performance Goals**: ≤ 10 seconds additional processing time per paper with ≤ 200 columns (SC-006)
**Constraints**: Must not break `success = TRUE` for any paper that currently succeeds; must respect existing LLM call budget architecture
**Scale/Scope**: ~200 columns per paper typical; up to several hundred for wide datasets

---

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ PASS | Classification runs within `extract_column_info()` before CSV write; failures degrade to `"unknown"` without crashing. Incremental write pattern unchanged. |
| II. Paper ID Preservation | ✅ PASS | No changes to paper ID handling; `col_type` is a column-level attribute. |
| III. Conservative Resource Limits | ✅ PASS | Column type LLM calls use a separate limit (5 calls / 100 columns per paper). File classification 10-call limit unchanged. |
| IV. Centralised Shared Helpers | ✅ PASS | Rule-based `classify_col_type_rules()` function added to `helper.R`. LLM prompt and call site stay in `0_index.R` (pipeline-specific). |
| V. Structured Error Classification | ✅ PASS | No new error codes required; classification failures degrade to `col_type = "unknown"`, not a pipeline error. |

**Post-design re-check**: All gates still pass. No violations.

---

## Project Structure

### Documentation (this feature)

```text
specs/002-column-type-classification/
├── plan.md              # This file
├── research.md          # Phase 0 decisions
├── data-model.md        # Phase 1 output schema
├── contracts/
│   └── col_type_vocab.md  # Controlled vocabulary contract
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (affected files)

```text
data_check/
├── helper.R             # ADD: classify_col_type_rules()
└── 0_index.R            # MODIFY: extract_column_info(), add COLUMN_TYPE_PROMPT,
                         #         add post-extraction LLM classification step
```

**Structure Decision**: Single-project R pipeline; no new files created. Changes are additive modifications to two existing files.

---

## Implementation Phases

### Phase A: Rule-Based Classification in `helper.R`

**Goal**: Add `classify_col_type_rules()` — the fast path that handles the majority of columns without an LLM call.

**Function signature**:
```r
classify_col_type_rules <- function(col_name, values)
# values: the raw column vector from the data frame (not the sample_values string)
# returns: list(col_type = "<label>", ambiguous = TRUE/FALSE, numeric_values = <vector or NULL>)
```

**Rule cascade** (in priority order):

1. `x_noNA <- values[!is.na(values)]`; if `length(x_noNA) == 0` → `col_type = "empty"`, done.
2. `n_unique <- length(unique(x_noNA))`; if `n_unique <= 2` → `col_type = "binary"`, done.
3. **ID detection**: `grepl("(?i)(^|\\b)(id|subj|subject|participant|pp|ppt|pid|respondent)(\\b|$|[_\\-]?\\d)", col_name, perl=TRUE)` AND all non-NA values are integers (or near-integer numerics) → `col_type = "id"`, done.
4. **Date detection**: try `as.Date()` / `as.POSIXct()` on a sample of up to 20 unique values; if ≥ 70% parse without error → `col_type = "date"`, done.
5. **Free text**: `median(nchar(as.character(x_noNA))) > 40` → `col_type = "text"`, done.
6. **Numeric column** (`is.numeric(values)`):
   - If n_unique > 20 → `col_type = "continuous"`, `ambiguous = FALSE`
   - If n_unique in 3–20 → `col_type = NA`, `ambiguous = TRUE` (LLM needed)
7. **Character column** — comma-decimal normalization:
   - `x_sub <- suppressWarnings(as.numeric(gsub(",", ".", x_noNA, fixed = TRUE)))`
   - `pct_ok <- sum(!is.na(x_sub)) / length(x_noNA)`
   - If `pct_ok >= 0.95` → `col_type = "continuous_comma_decimal"`, return substituted numeric vector
   - If `pct_ok >= 0.80` → `col_type = "continuous_outliers_excluded"`, return substituted numeric vector (NAs for non-parseable)
8. **Character column** — categorical check: `n_unique <= 10` AND `median(nchar(as.character(x_noNA))) <= 20` → `col_type = "categorical"`, done.
9. **Character column** — fallback → `col_type = "text"`, done.
10. **Numeric ambiguous** from step 6 → `ambiguous = TRUE`; LLM will decide.

**Returns**: `list(col_type, ambiguous, numeric_values)` where `numeric_values` is the (possibly normalized) numeric vector for stat computation, or NULL if non-numeric.

---

### Phase B: Integrate Classification into `extract_column_info()` in `0_index.R`

**Goal**: Thread the rule-based classification result into the column stats computation, and propagate `col_type` into the output data frame.

**Changes to `extract_column_info()`**:

1. After reading the data frame `df`, for each column compute `cls <- classify_col_type_rules(col_name, df[[col]])`.

2. Modify the stat computation block:
   - If `cls$col_type %in% c("continuous", "continuous_comma_decimal", "continuous_outliers_excluded")`:
     - Use `cls$numeric_values` (already coerced/substituted) for stat computation instead of the raw column
     - Compute full stats as before
   - Else if `cls$ambiguous == TRUE` (numeric, 3–20 unique values):
     - **Tentatively** compute stats (the column is numeric, so stats are valid regardless of final type label)
     - Mark for LLM classification
   - Else (binary, categorical, date, id, text, empty, unknown):
     - Set all numeric stat fields to NA
     - Still compute `n` and `n_missing`

3. Collect all columns where `cls$ambiguous == TRUE` into a list `ambiguous_cols` with their names, unique sample values, and tentative stats. These will be resolved by the LLM step after `column_list` is assembled.

4. Add `col_type = cls$col_type` to the column data frame row (NA for ambiguous, to be filled by LLM step).

**New column insertion in `data.frame()` constructor** (inside `extract_column_info()`):
```r
data.frame(
  paper_id      = paper_id,
  source_file   = rel_path,
  filename      = basename(path),
  group         = group,
  column_name   = names(df),
  sample_values = sample_vals,
  col_type      = col_types,      # NEW — vector of type strings or NA
  stats_mat,
  stringsAsFactors = FALSE,
  row.names     = NULL
)
```

---

### Phase C: LLM Batch for Ambiguous Columns in `0_index.R`

**Goal**: After all files are processed, send ambiguous columns to the LLM to resolve their `col_type`.

**New constant**:
```r
COLUMN_TYPE_PROMPT <- 'You are classifying columns in psychology research data.
For each column descriptor return a JSON array (same order).
Each element: {"descriptor": "<exact descriptor>", "col_type": "<type>"}

col_type — pick one:
  continuous  : numeric measurement (reaction time, score, rating, age in years, etc.)
  ordinal     : ordered scale with few levels (Likert, ranked preference, grade)
  categorical : unordered group/category code with few levels (condition, gender, language)
  binary      : exactly two possible values (yes/no, 0/1, treatment/control)
  id          : identifier — unique or nearly-unique integer per participant or row
  unknown     : cannot determine from name and sample values alone'
```

**Call site** (after `column_list` is assembled, before `columns_df` is built):

```r
# Collect ambiguous columns across all files
ambiguous_rows <- which(is.na(columns_df$col_type))
if (length(ambiguous_rows) > 0) {
  MAX_COL_TYPE_LLM_CALLS <- 5  # separate budget from file classification
  descriptors <- paste0('"', columns_df$column_name[ambiguous_rows], '"',
                        ' (samples: ', columns_df$sample_values_unique[ambiguous_rows], ')')
  # Respect budget: only send up to MAX_COL_TYPE_LLM_CALLS * LLM_BATCH_SIZE columns
  max_cols <- MAX_COL_TYPE_LLM_CALLS * LLM_BATCH_SIZE
  if (length(descriptors) > max_cols) {
    descriptors <- descriptors[seq_len(max_cols)]
    ambiguous_rows <- ambiguous_rows[seq_len(max_cols)]
  }
  llm_result <- llm_batch(
    paths         = descriptors,
    system_prompt = COLUMN_TYPE_PROMPT,
    user_prefix   = "Classify each column:",
    key_col       = "descriptor",
    extra_cols    = "col_type",
    fallback_vals = list(col_type = "unknown")
  )
  # Validate returned labels against controlled vocabulary
  VALID_COL_TYPES <- c("continuous","binary","categorical","ordinal","date","id",
                       "text","continuous_comma_decimal","continuous_outliers_excluded",
                       "empty","unknown")
  returned_types <- llm_result$col_type
  returned_types[!returned_types %in% VALID_COL_TYPES] <- "unknown"
  columns_df$col_type[ambiguous_rows] <- returned_types
}
# Final fallback: any remaining NA → "unknown"
columns_df$col_type[is.na(columns_df$col_type)] <- "unknown"
```

**Note on unique sample values**: A transient field `sample_values_unique` is computed inside `extract_column_info()` for ambiguous columns (up to 10 unique non-NA values), separate from the existing `sample_values` (first 5 head values). It is not written to the CSV; it is only used for the LLM batch input.

---

### Phase D: Stat Suppression for Non-Numeric Types

**Goal**: Ensure that `binary`, `categorical`, `ordinal`, `id`, `date`, `text`, and `empty` columns have NA for all numeric stat fields (mean, sd, etc.) in the final `columns_df`, regardless of whether the column was numeric in R.

**Implementation**: After LLM classification resolves ambiguous columns, apply a single vectorised pass:

```r
non_numeric_types <- c("binary", "categorical", "ordinal", "id", "date", "text", "empty", "unknown")
non_numeric_rows <- columns_df$col_type %in% non_numeric_types
stat_cols <- c("mean","sd","se","median","min","max","range","p25","p75","iqr","skewness","kurtosis")
columns_df[non_numeric_rows, stat_cols] <- NA
```

This ensures that even if a binary column (e.g., `0/1`) had stats computed tentatively, they are cleared from the final output.

---

## Data Model

See [data-model.md](data-model.md) for the extended column record schema.

---

## Contracts

See [contracts/col_type_vocab.md](contracts/col_type_vocab.md) for the controlled vocabulary contract.

---

## Key Design Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Rule order | Cascade with early exit | Fast, predictable, no LLM cost for clear cases |
| Comma-decimal normalization | `gsub(",",".")` with 95%/80% thresholds | Matches real data pattern; thresholds prevent false positives |
| Ambiguity threshold | Numeric column with 3–20 unique values | Too few unique values to confidently call "continuous" |
| LLM budget | Separate 5-call limit for column types | Preserves file-classification budget (Principle III) |
| Unique sample for LLM | `unique(x)[1:10]` | More informative than head-of-file for LLM classification |
| `col_type` position | After `sample_values`, before `n` | Groups qualitative fields together; no reordering of stats |
| Stat suppression | Post-LLM vectorised pass | Clean separation of classification and stat computation |
