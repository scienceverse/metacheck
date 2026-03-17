# Implementation Plan: Remove Redundant Stage-1 Column Extraction

**Branch**: `011-merge-columns-output` | **Date**: 2026-03-17 | **Spec**: [spec.md](spec.md)

## Summary

Delete `1_data_label.R` and `run_1_label_bulk.R`. Clean up three remaining references in other scripts. Recover the 47 papers whose rich stage-0 `columns.csv` was destroyed by stage 1 by re-running `run_index(paper_id, download = FALSE)` — skips re-downloading, re-processes already-present data files.

## Technical Context

**Language/Version**: R (base R only, no new packages)
**Primary Dependencies**: `metacheck` (`run_index()`) — already present
**Storage**: CSV files; `outputs/<paper_id>/columns.csv` restored in-place
**Testing**: Manual — count rich columns.csv before/after; spot-check `0956797615620784`
**Target Platform**: Local macOS / Linux
**Project Type**: Pipeline cleanup + one-time data recovery
**Performance Goals**: Recovery must not re-download (`download = FALSE`)
**Constraints**: Must not affect `0_index.R`, `2_codebook_label.R`, or `run_2_codebook_bulk.R` logic
**Scale/Scope**: 2 file deletions; 3 comment/message cleanups; 47 papers to re-process

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ Pass | Recovery uses existing `run_index()` with incremental CSV writes |
| II. Paper ID Preservation | ✅ Pass | No changes to `read.csv()` calls |
| III. Resource Limits | ✅ Pass | `download = FALSE` — no extra downloads |
| IV. Centralised Helpers | ✅ Pass | Stage-1 logic was a subset of stage-0; no helpers lost |
| V. Structured Error Classification | ✅ Pass | No error codes affected |

## Project Structure

### Documentation (this feature)

```text
specs/011-merge-columns-output/
├── plan.md              ← this file
├── spec.md
└── tasks.md             ← /speckit.tasks output
```

### Source Code

```text
data_check/
├── 1_data_label.R        ← DELETE
├── run_1_label_bulk.R    ← DELETE
├── helper.R              ← remove stale comment mentioning "1_data_label schema"
├── 2_codebook_label.R    ← remove stale comment mentioning "1_data_label schema"
├── run_2_codebook_bulk.R ← update error message referencing run_label_bulk.R
└── docs/
    └── pipeline.md       ← remove stage-1 step from flow diagram
```

**Structure Decision**: Single-project flat layout — all changes in `data_check/` root and `docs/`.

---

## Phase 0 — Research (complete)

**Finding 1**: `run_index()` accepts `download = FALSE` (line 114 of `0_index.R`). This is the recovery mechanism: re-runs classification and column extraction on already-present data without re-downloading.

**Finding 2**: 47 thin-columns papers identified by checking for absence of `col_type` in `columns.csv` header.

**Finding 3**: Three live cross-references to deleted files after deletion:
- `helper.R:607` — comment only
- `2_codebook_label.R:97` — comment only
- `run_2_codebook_bulk.R:31` — error message text

**Finding 4**: `label_summary.csv` is orphaned but harmless; left on disk as a historical artifact.

---

## Phase 1 — Design

### Updated Pipeline Flow (after deletion)

```
Stage 0: run_index() / run_index_bulk.R
  → structure.csv   (unchanged)
  → columns.csv     (rich 23-column — now the ONLY write to this file)

Stage 2: run_codebook_label() / run_2_codebook_bulk.R
  reads columns.csv → labels.csv, codebook_coverage.csv

[REMOVED] Stage 1: run_data_label() / run_1_label_bulk.R
```

### Recovery Inline Script

Run once after deletion to restore 47 thin-columns papers. No new permanent file.

```r
source("./data_check/0_index.R")
root     <- "./data_check/outputs"
dirs     <- list.dirs(root, recursive = FALSE, full.names = FALSE)
thin_ids <- dirs[sapply(dirs, function(d) {
  f <- file.path(root, d, "columns.csv")
  if (!file.exists(f)) return(FALSE)
  tryCatch(!grepl("col_type", readLines(f, n = 1, warn = FALSE)),
           error = function(e) FALSE)
})]
message("Re-processing ", length(thin_ids), " papers with thin columns.csv")
for (pid in thin_ids) {
  message("── ", pid)
  tryCatch(run_index(pid, download = FALSE),
           error = function(e) message("  ERROR: ", conditionMessage(e)))
}
```

### No contracts / data-model changes

The `columns.csv` schema is unchanged (stage-0 format was always the intended authoritative format). No new output columns introduced.
