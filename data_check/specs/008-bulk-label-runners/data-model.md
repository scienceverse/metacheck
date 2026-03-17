# Data Model: Bulk Label Runners

**Branch**: `008-bulk-label-runners` | **Date**: 2026-03-17

## Function Return Contracts

### `run_data_label(paper_id)` — NEW

```
list(
  paper_id     = character(1),   # paper ID (leading zeros preserved)
  success      = logical(1),     # TRUE if columns.csv was written
  error        = character(1),   # NA_character_ on success; error message on failure
  elapsed_sec  = numeric(1),     # wall-clock time for this paper
  n_data_files = integer(1),     # data files found in structure.csv
  n_columns    = integer(1)      # columns written to columns.csv
)
```

### `run_codebook_label(paper_id)` — EXISTING (no change to return value)

Already returns:
```
list(
  labels_df       = data.frame,  # full labels output
  coverage_df     = data.frame,  # codebook coverage output
  n_labelled      = integer(1),
  n_unlabelled    = integer(1),
  n_codebook_vars = integer(1),
  n_matched_vars  = integer(1),
  label_status    = character(1) # "ok" | "no_match" | "no_codebook"
)
```

`elapsed_sec` is NOT in the return value — bulk runner measures timing externally with `proc.time()`.

## Summary CSV Schemas

### `label_summary.csv`

One row per paper processed by `run_label_bulk.R`. Appended immediately after each paper.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `success` | logical | TRUE if `columns.csv` was written successfully |
| `error` | character | Error message if failed; NA otherwise |
| `elapsed_ms` | integer | Wall-clock time in milliseconds |
| `n_data_files` | integer | Data files found in `structure.csv` |
| `n_columns` | integer | Columns written to `columns.csv` |

### `codebook_summary.csv`

One row per paper processed by `run_codebook_bulk.R`. Appended immediately after each paper.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `success` | logical | TRUE if `labels.csv` and `codebook_coverage.csv` were written |
| `error` | character | Error message if failed; NA otherwise |
| `elapsed_ms` | integer | Wall-clock time in milliseconds (measured by bulk runner) |
| `n_labelled` | integer | Columns with `label_status = "labelled"` |
| `n_unlabelled` | integer | Columns with `label_status = "unlabelled"` or `"no_codebook"` |
| `n_codebook_vars` | integer | Total variables extracted from codebooks |
| `n_matched_vars` | integer | Codebook variables matched to at least one data column |
| `label_status` | character | Overall paper status: `"ok"`, `"no_match"`, `"no_codebook"` |

## Script Changes

### `1_data_label.R`

| Before | After |
|---|---|
| Top-level: `paper_id <- "09567976231220902"` | Removed |
| Top-level: `source("./data_check/helper.R")` | Moved inside function or kept at top (no-op on re-source) |
| Top-level: `STRUCTURE_DIR <- ...` | Removed (uses `paper_output_dir()`) |
| Top-level: `structure_path <- ...` | Inside `run_data_label()` |
| Top-level: `mapply(...)`, `write.csv(...)` | Inside `run_data_label()` |
| No return value | Returns result list |

### `2_codebook_label.R`

No changes required to function body or return value. Confirm no top-level execution code exists outside function/constant definitions — already confirmed clean.

## Bulk Runner Structure (both scripts)

```
1. Source the label script
2. Set config (N_RUNS, SUMMARY_CSV, etc.)
3. Discover eligible paper IDs by scanning outputs/
4. Load prior progress from SUMMARY_CSV (skip present IDs)
5. Loop over remaining IDs:
   a. Re-check SUMMARY_CSV (guard against concurrent writes)
   b. Time with proc.time()
   c. tryCatch(run_*label*(paper_id), error = ...)
   d. Build summary row
   e. Append row to SUMMARY_CSV immediately
6. Print final summary table
```
