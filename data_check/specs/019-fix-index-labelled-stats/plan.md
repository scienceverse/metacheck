# Implementation Plan: Fix Indexing Errors for Labelled Data and Empty Column Frames

**Branch**: `019-fix-index-labelled-stats` | **Date**: 2026-03-23 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/019-fix-index-labelled-stats/spec.md`

## Summary

Two targeted fixes in `pipeline/0_index.R`. Error A: coerce `x_comp` to plain `numeric` before statistics computation so `haven`-labelled type metadata does not contaminate the stats list and cause vctrs rbind failures. Error B: filter out 0-row frames from column assembly before rbind so a degenerate column does not crash an entire file's output.

## Technical Context

**Language/Version**: R (base R only, no new packages)
**Primary Dependencies**: `haven` (already installed) — source of the labelled type; vctrs (transitively via haven) — source of the precision error on rbind
**Storage**: CSV files — `outputs/<paper_id>/columns.csv`, `results/bulk_summary.csv`
**Testing**: Re-run `run_index()` on the three known affected paper IDs
**Target Platform**: macOS / Linux (local pipeline)
**Project Type**: Data pipeline (R scripts)
**Performance Goals**: No change — single coercion/filter operations
**Constraints**: Base R only; no new packages; no schema changes
**Scale/Scope**: Three lines changed in `pipeline/0_index.R`

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | PASS | Fixes prevent crashes; incremental write pattern unchanged |
| II. Paper ID Preservation | PASS | Not touched |
| III. Conservative Resource Limits | PASS | Not touched |
| IV. Centralised Shared Helpers | PASS | Both fixes are local to `extract_column_info` in `0_index.R` — not shared logic |
| V. Structured Error Classification | PASS | Fixes eliminate crashes before the error path; no new error codes needed |

## Project Structure

### Documentation (this feature)

```text
specs/019-fix-index-labelled-stats/
├── plan.md       # This file
├── research.md   # Phase 0 output
└── tasks.md      # Phase 2 output (/speckit.tasks)
```

### Source Code (files modified)

```text
pipeline/
└── 0_index.R    # Three targeted line changes
```

No new files. No new packages.

**Structure Decision**: Single-project, single-file change. No new helpers needed — all three changes are self-contained within `0_index.R`.

## Root Cause Analysis

### Error A — labelled precision (line 643)

`x_for_stats` may be a `haven::labelled<double>` vector (from `.sav`/`.dta`/`.sas7bdat` files). Subsetting with `[!is.na(...)]` preserves the labelled class and its value-label metadata. `quantile()` called on a labelled vector returns a `labelled<double>` value carrying a subset of the original label mapping. When `col_stats` is rbind'd at line 675, two columns with *different* label metadata produce incompatible `labelled<double>` columns — vctrs refuses to coerce: "Can't convert from `value` <labelled<double>> to <labelled<double>> due to loss of precision."

**Fix**: `x_comp <- as.numeric(x_for_stats[!is.na(x_for_stats)])` — strips label metadata before any computation. `as.numeric()` on a plain double/integer is a no-op, so non-haven files are unaffected.

### Error B — differing row counts (lines 675 / 702)

With Error A fixed, the most likely trigger for this is also resolved. However, applying defensive 0-row filters at both assembly sites prevents any future edge case from producing the same crash:

- **Line 675**: `do.call(rbind, lapply(col_stats, as.data.frame, ...))` — filter out any 0-row frames before rbind
- **Line 702**: `do.call(rbind, lapply(column_list, [["columns"]]))` — filter out NULL or 0-row column frames before rbind

## Implementation Design

### Change 1 — Strip labelled type before statistics (line 643)

**File**: `pipeline/0_index.R`

```r
# BEFORE
x_comp <- x_for_stats[!is.na(x_for_stats)]

# AFTER
x_comp <- as.numeric(x_for_stats[!is.na(x_for_stats)])
```

---

### Change 2 — Filter 0-row frames at stats_mat assembly (line 675)

**File**: `pipeline/0_index.R`

```r
# BEFORE
stats_mat <- do.call(rbind, lapply(col_stats, as.data.frame, stringsAsFactors = FALSE))

# AFTER
stats_frames <- lapply(col_stats, as.data.frame, stringsAsFactors = FALSE)
stats_frames <- Filter(function(f) nrow(f) > 0, stats_frames)
stats_mat    <- if (length(stats_frames) > 0) do.call(rbind, stats_frames) else NULL
```

---

### Change 3 — Filter 0-row frames at columns_df assembly (line 702)

**File**: `pipeline/0_index.R`

```r
# BEFORE
columns_df   <- do.call(rbind, lapply(column_list, `[[`, "columns"))

# AFTER
col_frames <- Filter(function(f) !is.null(f) && nrow(f) > 0,
                     lapply(column_list, `[[`, "columns"))
columns_df <- if (length(col_frames) > 0) do.call(rbind, col_frames) else NULL
```

## Complexity Tracking

No constitution violations.
