# Tasks: Remove Redundant Stage-1 Column Extraction (011)

**Branch**: `011-merge-columns-output`

## Format: `[ID] [P?] [Story] Description`

---

## Phase 1: Setup

- [x] T001 Confirm baseline — count thin vs rich columns.csv across all papers (already done: 47 thin, 4 rich, 8 missing)

---

## Phase 2: Foundational — Delete Stage-1 Files

**Purpose**: Remove the two files that cause the data-loss bug. Must complete before any cleanup or recovery.

- [ ] T002 Delete `data_check/1_data_label.R`
- [ ] T003 Delete `data_check/run_1_label_bulk.R`

**Checkpoint**: Neither file exists in the repo.

---

## Phase 3: User Story 1 — Clean Up Remaining References (Priority: P1)

**Goal**: No surviving references to the deleted files anywhere in the codebase.

**Independent Test**: `grep -r "1_data_label\|run_label_bulk\|run_1_label_bulk" data_check/ --include="*.R"` returns zero matches (excluding `specs/`).

- [ ] T004 [P] [US1] In `data_check/helper.R` line ~607, update the comment `# Support both "group" (0_index schema) and "experiment_group" (1_data_label schema)` → `# Support both "group" and "experiment_group" column names` (remove the reference to 1_data_label schema)
- [ ] T005 [P] [US1] In `data_check/2_codebook_label.R` line ~97, update the same stale comment (same change as T004)
- [ ] T006 [P] [US1] In `data_check/run_2_codebook_bulk.R` line ~31, update the error message `" — run run_label_bulk.R first to produce columns.csv files."` → `" — run run_index_bulk.R first to produce columns.csv files."`
- [x] T007 [US1] In `data_check/docs/pipeline.md`, remove the stage-1 row from the pipeline scripts table and the stage-1 step from the flow diagram. Update the codebook bulk runner's eligibility description to say it reads `columns.csv` produced by stage 0.
- [x] T008 [US1] In `data_check/progress.md`, add feature 011 entry.

**Checkpoint**: `grep` returns zero matches; docs updated.

---

## Phase 4: User Story 2 — Recover 47 Thin-Columns Papers (Priority: P2)

**Goal**: All 47 papers with thin `columns.csv` restored to the rich stage-0 format.

**Independent Test**: After recovery, count papers where `columns.csv` header contains `col_type`. Should be ≥ 47 (up from 4).

- [x] T009 [US2] Run the following recovery script from the `data_check/` working directory to re-process all thin-columns papers with `run_index(download = FALSE)`:

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

- [x] T010 [US2] Verify recovery: re-count papers with rich `columns.csv` (expect ≥ 47). Spot-check paper `0956797615620784` — confirm `columns.csv` has 23 columns including `col_type`, `mean`, `sd`.

**Checkpoint**: All recoverable papers have rich `columns.csv`. Stage 2 works correctly.

---

## Phase 5: Polish

- [x] T011 [P] Run stage 2 on `0956797615620784` to confirm the richer `columns.csv` does not break codebook labelling (`run_codebook_label("0956797615620784")`).

---

## Dependencies & Execution Order

- T002 → T003 must complete before T004–T008 (can't reference-check before deletion)
- T004, T005, T006, T007, T008 can all run in parallel after T002–T003
- T009 (recovery) should run after T002–T003 (no strict dependency on T004–T008, but cleaner to do cleanup first)
- T010 depends on T009
- T011 depends on T010

## Implementation Strategy

**MVP**: T002 + T003 alone stops the bleeding immediately. Any new paper processed by stage 0 afterwards will have its rich `columns.csv` preserved. Recovery (T009–T010) restores existing papers. Cleanup (T004–T008) removes stale references.
