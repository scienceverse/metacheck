# Implementation Plan: Bulk Label Runners

**Branch**: `008-bulk-label-runners` | **Date**: 2026-03-17 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/008-bulk-label-runners/spec.md`

## Summary

Refactor `1_data_label.R` from a top-level script into a callable `run_data_label(paper_id)` function. Confirm `2_codebook_label.R` already exposes `run_codebook_label(paper_id)` and remove any remaining top-level execution code. Add two crash-resilient bulk runner scripts — `run_label_bulk.R` and `run_codebook_bulk.R` — modelled exactly on `run_index_bulk.R`.

## Technical Context

**Language/Version**: R (base R only, no new packages)
**Primary Dependencies**: `metacheck`, `haven`, `readxl` — already present; `helper.R` (shared helpers)
**Storage**: CSV files on local filesystem; `outputs/<paper_id>/` directories (from feature 007)
**Testing**: Manual run + file-system inspection
**Target Platform**: macOS / Linux filesystem
**Project Type**: CLI pipeline / batch scripts
**Performance Goals**: No change — path/function wrapping is negligible overhead
**Constraints**: Paper IDs must remain character strings (leading-zero preservation, Constitution Principle II)
**Scale/Scope**: Same paper corpus as existing pipeline

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ PASS | Both bulk runners append to summary CSV after each paper; crash leaves prior rows intact |
| II. Paper ID Preservation | ✅ PASS | `colClasses = c(paper_id = "character")` applied in all CSV reads |
| III. Resource Limits | ✅ PASS | No new resource-consuming operations introduced |
| IV. Centralised Helpers | ✅ PASS | `paper_output_dir()` and `read_data_head()` used from `helper.R`; no duplication |
| V. Structured Error Classification | ✅ PASS | Per-paper errors caught and written to summary CSV |

No gate violations.

## Project Structure

### Documentation (this feature)

```text
specs/008-bulk-label-runners/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
data_check/
├── 1_data_label.R           # Change: wrap all logic in run_data_label(paper_id); remove top-level execution
├── 2_codebook_label.R       # Change: remove any remaining top-level execution code
├── run_label_bulk.R         # NEW: bulk runner for data-label stage
├── run_codebook_bulk.R      # NEW: bulk runner for codebook-label stage
├── label_summary.csv        # NEW (runtime): progress log for run_label_bulk.R
└── codebook_summary.csv     # NEW (runtime): progress log for run_codebook_bulk.R
```

**Structure Decision**: Single project. All new files at the `data_check/` root alongside existing bulk runners.

## Phase 0: Research

No external unknowns. All decisions fully determined by the existing codebase. See [research.md](research.md).

## Phase 1: Design

### `run_data_label(paper_id)` function contract

Wraps the existing top-level logic of `1_data_label.R`:

1. Resolve `structure_path <- file.path(paper_output_dir(paper_id), "structure.csv")`
2. Stop with a descriptive error if the file does not exist
3. Filter to data files, extract column names, write `outputs/<paper_id>/columns.csv`
4. Return structured result list:

```
list(
  paper_id     = paper_id,
  success      = TRUE/FALSE,
  error        = NA_character_ / error message,
  elapsed_sec  = numeric,
  n_data_files = integer,
  n_columns    = integer
)
```

### `run_codebook_label(paper_id)` — existing function

Already implemented in `2_codebook_label.R`. Only change: strip any top-level script execution code (e.g. hardcoded `paper_id <- "..."` and direct function calls outside a function body). The function body and return value are unchanged.

### `run_label_bulk.R` design

Mirrors `run_index_bulk.R` structure:

| Element | Value |
|---|---|
| Sources | `./data_check/1_data_label.R` |
| Eligible papers | `outputs/` subdirs containing `structure.csv` |
| Summary CSV | `./data_check/label_summary.csv` |
| Resume signal | paper_id present in `label_summary.csv` (success OR failure skipped) |
| N_RUNS cap | `Inf` by default |
| Per-paper error handling | `tryCatch` → write failure row, continue |
| Summary columns | `paper_id`, `success`, `error`, `elapsed_ms`, `n_data_files`, `n_columns` |

### `run_codebook_bulk.R` design

| Element | Value |
|---|---|
| Sources | `./data_check/2_codebook_label.R` |
| Eligible papers | `outputs/` subdirs containing `columns.csv` |
| Summary CSV | `./data_check/codebook_summary.csv` |
| Resume signal | paper_id present in `codebook_summary.csv` (success OR failure skipped) |
| N_RUNS cap | `Inf` by default |
| Per-paper error handling | `tryCatch` → write failure row, continue |
| Summary columns | `paper_id`, `success`, `error`, `elapsed_ms`, `n_labelled`, `n_unlabelled`, `n_no_codebook` |

### Eligible-paper discovery

```r
# run_label_bulk.R — papers with structure.csv
all_ids  <- basename(list.dirs("./data_check/outputs", recursive = FALSE))
eligible <- all_ids[file.exists(file.path("./data_check/outputs", all_ids, "structure.csv"))]

# run_codebook_bulk.R — papers with columns.csv
all_ids  <- basename(list.dirs("./data_check/outputs", recursive = FALSE))
eligible <- all_ids[file.exists(file.path("./data_check/outputs", all_ids, "columns.csv"))]
```

Each stage manages its own progress log, independent of `bulk_summary.csv`.

### `run_codebook_label` return value

The existing function must return a list. If it currently returns invisibly or not at all, add a return list with at minimum: `paper_id`, `success`, `error`, `elapsed_sec`, `n_labelled`, `n_unlabelled`, `n_no_codebook`. The bulk runner's `append_summary_row` will extract these fields.

### Constitution check (post-design)

All principles satisfied. Paper IDs sourced from directory names (character strings). `paper_output_dir()` from `helper.R`. Summary CSV appended after each paper. No new resource limits.
