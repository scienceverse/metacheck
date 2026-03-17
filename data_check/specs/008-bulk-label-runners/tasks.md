# Tasks: Bulk Label Runners

**Input**: Design documents from `/specs/008-bulk-label-runners/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅

**Organization**: Tasks grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies on each other)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: No new shared infrastructure needed — `helper.R` and `paper_output_dir()` already exist. Setup is verifying the current state of `2_codebook_label.R`.

- [x] T001 Verify `data_check/2_codebook_label.R` has no top-level execution code outside function/constant definitions (confirmed via code inspection — document in a comment at top of file if any ambiguity existed). No code change expected.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Refactor `1_data_label.R` into a callable function. This is the prerequisite for US1 (bulk runner) and US3 (programmatic use).

**⚠️ CRITICAL**: T002 must complete before T003 (bulk runner) and T004 (US3 verification), since both depend on `run_data_label()` existing.

- [x] T002 Refactor `data_check/1_data_label.R`: remove hardcoded `paper_id <- "09567976231220902"`, `OUTPUT_DIR` constant, top-level `structure_path`/`data_files` assignments, top-level `mapply()`/`do.call()`/`write.csv()` calls; wrap all logic in `run_data_label(paper_id)` that (1) resolves paths via `paper_output_dir(paper_id)`, (2) loads `structure.csv`, (3) filters to data files, (4) extracts column names, (5) writes `columns.csv`, (6) returns `list(paper_id, success, error, elapsed_sec, n_data_files, n_columns)`; keep `source("./data_check/helper.R")` and library calls at top level (safe to re-source)

**Checkpoint**: Source `1_data_label.R` in a fresh R session — confirm zero output, only `run_data_label` defined. Then call `run_data_label("<known_id>")` and confirm `outputs/<id>/columns.csv` is written and the return list fields are populated.

---

## Phase 3: User Story 1 — Data-label bulk runner (Priority: P1) 🎯 MVP

**Goal**: `run_label_bulk.R` processes all papers with `structure.csv`, appends to `label_summary.csv`, auto-resumes on restart.

**Independent Test**: Run `run_label_bulk.R` with `N_RUNS <- 3`. Confirm `label_summary.csv` has 3 rows. Stop, restart — confirm those 3 papers are skipped and the next batch processes.

### Implementation for User Story 1

- [x] T003 [US1] Create `data_check/run_label_bulk.R` modelled on `run_index_bulk.R`:
  - `source("./data_check/1_data_label.R")`
  - Config: `N_RUNS <- Inf`, `SUMMARY_CSV <- "./data_check/label_summary.csv"`
  - Discover eligible papers: `basename(list.dirs("./data_check/outputs", recursive = FALSE))` filtered to those where `file.exists(file.path("./data_check/outputs", id, "structure.csv"))`
  - Load prior progress: read `SUMMARY_CSV` with `colClasses = c(paper_id = "character")`, extract `done_ids`
  - Per-paper loop: re-check CSV before each run; time with `proc.time()`; `tryCatch(run_data_label(pid), error = ...)` returning failure list on error; append one row immediately to `SUMMARY_CSV` via `write.table(..., append = TRUE)`
  - Summary row columns: `paper_id`, `success`, `error`, `elapsed_ms`, `n_data_files`, `n_columns`
  - Print final summary table after loop

**Checkpoint**: `label_summary.csv` exists with correct columns; re-run produces no duplicate rows.

---

## Phase 4: User Story 2 — Codebook-label bulk runner (Priority: P2)

**Goal**: `run_codebook_bulk.R` processes all papers with `columns.csv`, appends to `codebook_summary.csv`, auto-resumes on restart.

**Independent Test**: Run `run_codebook_bulk.R` with `N_RUNS <- 3`. Confirm `codebook_summary.csv` has 3 rows with correct columns. Stop, restart — those 3 papers are skipped.

### Implementation for User Story 2

- [x] T004 [US2] Create `data_check/run_codebook_bulk.R` modelled on `run_index_bulk.R`:
  - `source("./data_check/2_codebook_label.R")`
  - Config: `N_RUNS <- Inf`, `SUMMARY_CSV <- "./data_check/codebook_summary.csv"`
  - Discover eligible papers: `basename(list.dirs("./data_check/outputs", recursive = FALSE))` filtered to those where `file.exists(file.path("./data_check/outputs", id, "columns.csv"))`
  - Load prior progress: read `SUMMARY_CSV` with `colClasses = c(paper_id = "character")`, extract `done_ids`
  - Per-paper loop: re-check CSV before each run; time with `proc.time()` externally (function does not return `elapsed_sec`); `tryCatch(run_codebook_label(pid), error = ...)` returning failure list on error; append one row immediately to `SUMMARY_CSV`
  - Summary row columns: `paper_id`, `success`, `error`, `elapsed_ms`, `n_labelled`, `n_unlabelled`, `n_codebook_vars`, `n_matched_vars`, `label_status`
  - Extract these fields from the return list of `run_codebook_label()` (all already present)
  - Print final summary table after loop

**Checkpoint**: `codebook_summary.csv` exists with correct columns; no_codebook papers recorded correctly.

---

## Phase 5: User Story 3 — Programmatic callability (Priority: P3)

**Goal**: Sourcing either label script produces zero side effects. Functions callable from other scripts.

**Independent Test**: In a fresh R session: `source("data_check/1_data_label.R")` → no output. `source("data_check/2_codebook_label.R")` → no output. Both functions exist and are callable.

### Implementation for User Story 3

- [x] T005 [P] [US3] Add a comment block at the top of `data_check/1_data_label.R` documenting the exported function signature: `run_data_label(paper_id)` — inputs, outputs, return value fields
- [x] T006 [P] [US3] Add a comment block at the top of `data_check/2_codebook_label.R` documenting the exported function signature: `run_codebook_label(paper_id)` — inputs, outputs, return value fields

**Checkpoint**: Both scripts source cleanly. Both functions callable by name. Return lists match documented signatures.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [x] T007 [P] Update `data_check/docs/pipeline.md`: add `run_label_bulk.R` and `run_codebook_bulk.R` to the Entry Points table; add `label_summary.csv` and `codebook_summary.csv` to descriptions
- [x] T008 [P] Update `data_check/docs/output-schemas.md`: add schema tables for `label_summary.csv` and `codebook_summary.csv`
- [x] T009 Update `data_check/progress.md` with feature 008 entry
- [ ] T010 Add `data_check/label_summary.csv` and `data_check/codebook_summary.csv` to `.gitignore` (alongside existing `bulk_summary.csv` pattern)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup/Verify)**: No dependencies — start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 (T001 confirms no stripping needed in `2_codebook_label.R`)
- **Phase 3 (US1)**: Depends on T002 (needs `run_data_label()`)
- **Phase 4 (US2)**: Depends on T001 (needs confirmed clean `run_codebook_label()`); independent of Phase 3
- **Phase 5 (US3)**: Depends on T002 and T001 complete (both functions must exist)
- **Phase 6 (Polish)**: Depends on all prior phases complete

### User Story Dependencies

- **US1 (P1)**: Depends on T002 (Foundational). No dependency on US2 or US3.
- **US2 (P2)**: Depends on T001. No dependency on US1 or US3.
- **US3 (P3)**: Depends on T001 + T002. T005 and T006 are parallel with each other.

### Parallel Opportunities

- T003 (run_label_bulk.R) and T004 (run_codebook_bulk.R) can be written in parallel — different files
- T005 and T006 (comment blocks) are parallel — different files
- T007, T008, T009, T010 (polish) are all parallel — different files

---

## Parallel Example: Phase 3 + Phase 4

```text
After T001 and T002 complete:
  Task T003: Create run_label_bulk.R      ← parallel with T004
  Task T004: Create run_codebook_bulk.R   ← parallel with T003
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. T001 — verify `2_codebook_label.R` is clean
2. T002 — refactor `1_data_label.R` into function
3. T003 — create `run_label_bulk.R`
4. **STOP and VALIDATE**: run against 3 papers, confirm `label_summary.csv`, restart and confirm resume

### Incremental Delivery

1. T001–T002 → both label scripts are callable functions (US3 ✅ structurally)
2. T003 → data-label bulk running works (US1 ✅)
3. T004 → codebook-label bulk running works (US2 ✅)
4. T005–T006 → documentation comments (US3 ✅ fully)
5. T007–T010 → docs and gitignore updated

---

## Notes

- [P] tasks = different files, no dependencies on each other
- No test framework — validation is manual file-system inspection and console output
- T001 is expected to require no code change; if top-level code IS found, add a subtask to remove it before T004
- T002 is the highest-risk task — read `1_data_label.R` carefully before editing; preserve all delimiter-sniffing and column-extraction logic
- Commit after T001–T002, after T003–T004, and after T007–T010 as logical groups
