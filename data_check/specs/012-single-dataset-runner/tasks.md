# Tasks: Single Dataset Runner

**Input**: Design documents from `/specs/012-single-dataset-runner/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, quickstart.md ✅

**Organization**: One user story (P1). No setup or foundational phases needed — no new project structure, no new packages, no blocking prerequisites. The entire feature is a single new file.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to

---

## Phase 1: User Story 1 — Run Full Pipeline on One Random Dataset (Priority: P1) 🎯 MVP

**Goal**: A single invocation of `Rscript data_check/run_single.R` selects a random paper ID and runs both pipeline stages end-to-end, printing status for each.

**Independent Test**: Run `Rscript data_check/run_single.R`. Verify that `data_check/outputs/<paper_id>/` is created with `structure.csv`, `columns.csv`, and `labels.csv` for a paper that had none before, or that a known-error code is printed and the script exits cleanly.

### Implementation for User Story 1

- [x] T001 [US1] Create `data_check/run_single.R`: source `./data_check/0_index.R` and `./data_check/2_codebook_label.R`; discover IDs from `XML_DIR` (defined in `0_index.R`); stop with a clear message if no IDs found; sample one ID at random (character string — no numeric coercion); print a header banner with the selected paper ID
- [x] T002 [US1] In `data_check/run_single.R`: run Stage 1 — call `run_index(paper_id = pid, download = TRUE)` inside `tryCatch`; print stage status (success/error code, n_files, n_data_files, n_columns, elapsed); on known error codes (`no_links`, `download_failed`, `empty_repo`, `too_large`) print the code and exit gracefully
- [x] T003 [US1] In `data_check/run_single.R`: run Stage 2 — check that `outputs/<pid>/columns.csv` exists before calling `run_codebook_label(paper_id = pid)`; if absent, print "Stage 2 skipped — no columns.csv" and exit; otherwise call inside `tryCatch` and print stage status (label_status, n_labelled, n_unlabelled, elapsed); print final output path on completion

**Checkpoint**: `run_single.R` is complete and independently runnable. User Story 1 is done.

---

## Phase 2: Polish & Cross-Cutting Concerns

- [x] T004 [P] Update `docs/pipeline.md` to reference `data_check/run_single.R` as the single-paper entry point alongside the bulk runners
- [x] T005 [P] Update `progress.md` with feature 012 entry

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1** (User Story 1): No blocking prerequisites — can start immediately
- **Phase 2** (Polish): Depends on Phase 1 completion

### User Story Dependencies

- **User Story 1 (P1)**: No dependencies on other stories — self-contained

### Within User Story 1

- T001 → T002 → T003 (sequential — all edit the same file)
- T004 and T005 are independent of each other [P]

### Parallel Opportunities

- T004 and T005 can run in parallel after T003

---

## Parallel Example: User Story 1

```
# T001, T002, T003 are sequential edits to data_check/run_single.R
# After T003:
Task T004: Update docs/pipeline.md
Task T005: Update progress.md
# T004 and T005 can run in parallel
```

---

## Implementation Strategy

### MVP (User Story 1 Only)

1. Complete T001 — script skeleton + ID discovery
2. Complete T002 — Stage 1 invocation and error handling
3. Complete T003 — Stage 2 invocation and output reporting
4. **STOP and VALIDATE**: run `Rscript data_check/run_single.R`, inspect outputs
5. Complete T004, T005 (docs)

---

## Notes

- Paper IDs MUST be treated as character strings throughout — no `as.numeric()`, no `as.integer()`
- `XML_DIR` is a constant already defined in `0_index.R` — reference it directly after sourcing
- Stage 2 dependency on `columns.csv` mirrors the check in `run_2_codebook_bulk.R`
- Timing pattern (using `proc.time()[["elapsed"]]`) mirrors `run_2_codebook_bulk.R`
- No `bulk_summary.csv` write needed — this is a single-paper dev tool, not a batch runner
