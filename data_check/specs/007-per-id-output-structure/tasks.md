# Tasks: Per-ID Output Directory Structure

**Input**: Design documents from `/specs/007-per-id-output-structure/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Add the shared helper and create the new output directory root.

- [x] T001 Add `paper_output_dir(paper_id)` helper to `data_check/helper.R` — constructs `./data_check/outputs/<paper_id>`, calls `dir.create(..., recursive = TRUE)` if missing, returns path
- [x] T002 [P] Create `data_check/outputs/` directory (add a `.gitkeep` so it is tracked by git)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: No foundational blocking prerequisites beyond Phase 1 for this feature. All user story phases may proceed after T001–T002.

**⚠️ CRITICAL**: T001 must be complete before any pipeline script changes (T003 onward) because all scripts call `paper_output_dir()`.

---

## Phase 3: User Story 1 — Find all outputs for a single paper (Priority: P1) 🎯 MVP

**Goal**: All per-paper CSV files land in `outputs/<paper_id>/` with short filenames. A user can navigate to one directory and find everything for a paper.

**Independent Test**: Run `run_index(paper_id = "<any_id>", download = FALSE)` for a paper whose data is already in `data/`. Verify that `outputs/<paper_id>/structure.csv` and `outputs/<paper_id>/columns.csv` are created. Verify `structure/<paper_id>_structure.csv` is NOT created.

### Implementation for User Story 1

- [x] T003 [US1] In `data_check/0_index.R`: rename `STRUCTURE_DIR` constant to `OUTPUT_DIR <- "./data_check/outputs"`, remove the standalone `dir.create(STRUCTURE_DIR, ...)` call (now handled by `paper_output_dir()`), update `structure_out` to `file.path(paper_output_dir(paper_id), "structure.csv")`, update `columns_out` to `file.path(paper_output_dir(paper_id), "columns.csv")`
- [x] T004 [P] [US1] In `data_check/1_data_label.R`: rename `STRUCTURE_DIR` to `OUTPUT_DIR <- "./data_check/outputs"`, update `structure_path` read to `file.path(paper_output_dir(paper_id), "structure.csv")`, update `out_path` write to `file.path(paper_output_dir(paper_id), "columns.csv")`
- [x] T005 [P] [US1] In `data_check/2_codebook_label.R`: rename `STRUCTURE_DIR` to `OUTPUT_DIR <- "./data_check/outputs"`, update `structure_path` and `columns_path` reads to use `paper_output_dir(paper_id)`, update `labels_out` to `file.path(paper_output_dir(paper_id), "labels.csv")`, update `coverage_out` to `file.path(paper_output_dir(paper_id), "codebook_coverage.csv")`

**Checkpoint**: Run `run_index()` for one paper. Confirm `outputs/<paper_id>/structure.csv` and `outputs/<paper_id>/columns.csv` exist with correct content and row counts matching the old output.

---

## Phase 4: User Story 2 — Pipeline resumes without reprocessing completed papers (Priority: P2)

**Goal**: The bulk runner's crash-resilience is unaffected. Papers already in `bulk_summary.csv` are skipped; partially processed papers (directory present but no summary row) are re-run.

**Independent Test**: Process one paper with the updated scripts, stop the runner, restart it, and confirm the paper is skipped (its ID appears in `bulk_summary.csv`). The `outputs/<paper_id>/` directory must remain intact.

### Implementation for User Story 2

- [x] T006 [US2] Verify `data_check/run_index_bulk.R` requires no changes: the resume signal is `bulk_summary.csv` (already implemented). Confirm `DATA_DIR` reference in bulk runner still resolves correctly after `STRUCTURE_DIR` → `OUTPUT_DIR` rename in `0_index.R`. If the bulk runner imports `STRUCTURE_DIR` from `0_index.R`'s environment, update any such reference to `OUTPUT_DIR`.

**Checkpoint**: Full end-to-end bulk run with `N_RUNS <- 2`, crash after first paper, restart — second paper processes, first is skipped.

---

## Phase 5: User Story 3 — Aggregate view across all papers (Priority: P3)

**Goal**: `bulk_summary.csv` continues to be produced at the repo root with the same schema, one row per paper.

**Independent Test**: After running two papers, confirm `bulk_summary.csv` has two rows, with `paper_id`, `success`, timing and count columns intact. Schema must match pre-migration output exactly.

### Implementation for User Story 3

- [x] T007 [US3] Confirm `bulk_summary.csv` path (`./data_check/bulk_summary.csv`) and schema are unchanged in `run_index_bulk.R` — no code changes expected; this is a verification task. Document the confirmation in a code comment if any ambiguity existed.

**Checkpoint**: Diff the columns of a freshly generated `bulk_summary.csv` against the existing one. Should be identical.

---

## Phase 6: Migration

**Goal**: Move all existing `structure/<paper_id>_*.csv` files to `outputs/<paper_id>/*.csv`, stripping the paper-ID prefix from filenames.

- [x] T008 Create `data_check/migrate_structure.R` — standalone one-time script:
  1. List all `*.csv` files in `./data_check/structure/`
  2. For each file: extract `paper_id` from filename (everything before the first `_` in the basename)
  3. Call `paper_output_dir(paper_id)` to create `outputs/<paper_id>/` if needed
  4. Strip the `<paper_id>_` prefix to get the short filename (e.g. `structure.csv`)
  5. `file.copy(src, dest, overwrite = FALSE)` — do NOT overwrite if dest already exists (idempotent)
  6. On successful copy, `file.remove(src)`
  7. Print per-file status (moved / skipped / failed) and a final summary count
- [x] T009 Run `migrate_structure.R` and verify: `structure/` is empty, `outputs/` has one subdirectory per paper, each containing the correct short-named CSVs, and the content of spot-checked files is byte-for-byte identical to the originals

---

## Phase 7: Polish & Cross-Cutting Concerns

- [x] T010 [P] Update `docs/pipeline.md`: change step 7 of the canonical processing order from `structure/<paper_id>_*.csv` to `outputs/<paper_id>/*.csv`; update any constants table entries referencing `STRUCTURE_DIR`
- [x] T011 [P] Update `docs/output-schemas.md`: update all file path references from `structure/<paper_id>_*.csv` to `outputs/<paper_id>/*.csv`
- [x] T012 Update `progress.md` with feature 007 completion entry
- [x] T013 Update constitution Pipeline Workflow section (step 7) to reflect new output paths — bump version to PATCH (1.0.1) and update Sync Impact Report comment in `.specify/memory/constitution.md`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies — start immediately
- **Phase 2**: N/A — absorbed into Phase 1 note
- **Phase 3 (US1)**: Depends on T001 (helper must exist before scripts are changed)
- **Phase 4 (US2)**: Depends on Phase 3 complete (bulk runner calls `run_index()`)
- **Phase 5 (US3)**: Depends on Phase 3 complete (bulk runner writes `bulk_summary.csv`)
- **Phase 6 (Migration)**: Depends on Phase 3 complete (uses `paper_output_dir()`)
- **Phase 7 (Polish)**: Depends on all phases complete

### User Story Dependencies

- **US1 (P1)**: Starts after T001 — no dependency on US2 or US3
- **US2 (P2)**: Starts after US1 complete — bulk runner calls `run_index()`
- **US3 (P3)**: Starts after US1 complete — bulk runner is the writer

### Within Each Phase

- T003, T004, T005 (Phase 3) all depend on T001 but are independent of each other [P]
- T008 (migration script creation) and T009 (run migration) are sequential

### Parallel Opportunities

- T002, T004, T005 can all run in parallel once T001 is done
- T010 and T011 (docs) can run in parallel with each other and with T012/T013

---

## Parallel Example: Phase 3 (US1)

```text
After T001 is complete:
  Task T003: Update 0_index.R
  Task T004: Update 1_data_label.R   ← parallel with T003 and T005
  Task T005: Update 2_codebook_label.R ← parallel with T003 and T004
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete T001–T002: Add helper, create `outputs/` dir
2. Complete T003–T005: Update all three pipeline scripts
3. **STOP and VALIDATE**: Run `run_index()` for one paper, confirm `outputs/<paper_id>/` layout
4. If validated: proceed to migration (T008–T009)

### Incremental Delivery

1. T001–T002 → helper ready
2. T003–T005 → new output paths live for all pipeline stages (US1 ✅)
3. T006 → confirm resume logic unaffected (US2 ✅)
4. T007 → confirm aggregate CSV unchanged (US3 ✅)
5. T008–T009 → migrate existing data
6. T010–T013 → docs and constitution updated

---

## Notes

- [P] tasks = different files, no dependencies on each other
- [Story] label maps task to specific user story for traceability
- No test framework exists in this project — validation is manual file-system inspection
- T006 and T007 are verification tasks (no code change expected); document findings in a comment if relevant
- T009 (run migration) is irreversible for individual files — ensure T008 is reviewed before executing
- Commit after T001–T002, after T003–T005, and after T008–T009 as logical groups
