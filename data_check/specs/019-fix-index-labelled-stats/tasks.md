# Tasks: Fix Indexing Errors for Labelled Data and Empty Column Frames

**Input**: Design documents from `/specs/019-fix-index-labelled-stats/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅

**Tests**: Not requested — no test tasks generated.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2)

## Path Conventions

All changes are in `pipeline/0_index.R` — single file, three targeted edits.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Read the exact lines being changed before touching anything.

- [x] T001 Read `pipeline/0_index.R` lines 623–675 (col_stats lapply, x_comp assignment, stats_mat rbind) to confirm current code matches the plan
- [x] T002 Read `pipeline/0_index.R` lines 695–710 (column_list assembly, columns_df rbind) to confirm line 702 matches the plan

---

## Phase 2: Foundational

No shared prerequisites — both user stories modify independent lines in the same file and can be implemented sequentially.

---

## Phase 3: User Story 1 — SPSS/Stata/SAS files complete without labelled-type error (Priority: P1) 🎯 MVP

**Goal**: Strip `haven` labelled-type metadata from numeric vectors before statistics computation so vctrs rbind does not encounter mismatched label mappings.

**Independent Test**: Run `source("pipeline/helper.R"); source("pipeline/0_index.R")` then `run_index("0956797618772822")` and `run_index("09567976231158570")`. Both should return `status = "ok"` and produce a non-empty `outputs/<paper_id>/columns.csv`.

### Implementation for User Story 1

- [x] T003 [US1] In `pipeline/0_index.R` line 643, change `x_comp <- x_for_stats[!is.na(x_for_stats)]` to `x_comp <- as.numeric(x_for_stats[!is.na(x_for_stats)])` — strips haven labelled-type metadata before any statistics computation in `pipeline/0_index.R`

**Checkpoint**: Error A eliminated — SPSS/Stata/SAS files with labelled numeric columns now index successfully.

---

## Phase 4: User Story 2 — Column frame assembly tolerates 0-row edge cases (Priority: P2)

**Goal**: Filter out 0-row frames at both column assembly sites so a degenerate column cannot crash an entire file's output.

**Independent Test**: Run `run_index("0956797618773095")`. Should return `status = "ok"` with a non-empty `columns.csv`. Also re-run the two Error A papers from US1 to confirm no regression.

### Implementation for User Story 2

- [x] T004 [US2] In `pipeline/0_index.R` line 675, replace the single-line `stats_mat <- do.call(rbind, lapply(col_stats, as.data.frame, stringsAsFactors = FALSE))` with a three-line version that filters 0-row frames: assign lapply result to `stats_frames`, filter with `Filter(function(f) nrow(f) > 0, stats_frames)`, then rbind only if non-empty (else NULL) in `pipeline/0_index.R`
- [x] T005 [US2] In `pipeline/0_index.R` line 702, replace `columns_df <- do.call(rbind, lapply(column_list, `[[`, "columns"))` with a version that filters NULL and 0-row column frames before rbind: assign lapply to `col_frames`, apply `Filter(function(f) !is.null(f) && nrow(f) > 0, col_frames)`, then rbind only if non-empty (else NULL) in `pipeline/0_index.R`

**Checkpoint**: Error B eliminated — 0-row column frames no longer crash the assembly step.

---

## Phase 5: Polish & Cross-Cutting Concerns

- [x] T006 [P] Remove the three affected papers from `results/bulk_summary.csv` so they are re-processed on the next bulk run — either edit the CSV directly or run `Rscript -e 'df <- read.csv("data_check/results/bulk_summary.csv", colClasses=c(paper_id="character")); df <- df[!df$paper_id %in% c("0956797618772822","09567976231158570","0956797618773095"),]; write.csv(df,"data_check/results/bulk_summary.csv",row.names=FALSE)'` from repo root
- [x] T007 [P] Update `progress.md` — add feature 019 entry describing the two indexing fixes

---

## Dependencies & Execution Order

- T001 + T002 can run in parallel (both read-only)
- T003 depends on T001
- T004 depends on T001 (same file region)
- T005 depends on T002
- T004 and T005 can run in parallel (different line ranges in the same file — apply sequentially to be safe)
- T006 + T007 can run in parallel (independent)

---

## Parallel Opportunities

```
# Setup reads — parallel:
T001: Read 0_index.R lines 623–675
T002: Read 0_index.R lines 695–710

# Polish — parallel:
T006: Reset affected papers in bulk_summary.csv
T007: Update progress.md
```

---

## Implementation Strategy

### MVP (User Story 1 only — 1 line change)

1. T001 (read)
2. T003 (one-line fix for Error A)
3. Validate on papers `0956797618772822` and `09567976231158570`

### Full delivery

1. T001–T002 (read)
2. T003 (Error A fix)
3. T004–T005 (Error B defensive filters)
4. T006–T007 (reset papers + progress)

---

## Notes

- T003 is a one-character change (`as.numeric(...)` wrapper) — the single highest-impact edit
- T004 and T005 are defensive and follow the existing `Filter(Negate(is.null), ...)` pattern already on line 701
- All three pipeline fixes are in `pipeline/0_index.R` — no other files need changing for the fixes themselves
