# Tasks: Fix Multi-Level CSV Header Skip Rule

**Input**: Design documents from `/specs/014-fix-multilevel-csv-headers/`
**Branch**: `014-fix-multilevel-csv-headers`

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to

---

## Phase 1: Setup (Foundational)

**Purpose**: Add the new constant that all subsequent logic depends on.

- [x] T001 Add `MULTILEVEL_HEADER_LOOKAHEAD <- 3L` to the constants block in `data_check/0_index.R` (alongside `MAX_FILE_MB`, `N_DATA_READ`, etc.)

**Checkpoint**: Constant defined — implementation phases can proceed.

---

## Phase 2: User Story 1 — Multi-level header recovery (Priority: P1) 🎯 MVP

**Goal**: Files with `>50% ...N` column names are recovered. Branch 1 resolves a sub-header row and populates `col_header_group` from row-1 spans. Branch 2 proceeds as-is with partial labels.

**Independent Test**: Run `run_index(paper_id = "09567976221147259", download = FALSE)` and `run_index(paper_id = "09567976231151581", download = FALSE)`. All 4 previously-skipped files must produce column records in `outputs/<paper_id>/columns.csv` with meaningful `column_name` values (not `...N`) and no "skipping (no proper header row)" messages.

- [x] T002 [US1] In `extract_column_info()` in `data_check/0_index.R`: replace the current 6-line skip block (lines ~517–522) with the full multi-level header logic from `plan.md` — including `col_header_group` forward-fill from row-1 prefixes, Branch 1 sub-header scan, Branch 2 partial-label fallback, and the `col_header_group <- rep(NA_character_, ncol(df))` default for non-multi-level files.

- [x] T003 [US1] In `data_check/0_index.R`: extend the column record construction (the section that builds each column's row for `columns.csv`) to include `col_header_group` as a field. Ensure it is passed through correctly for Branch 1 (populated), Branch 2 (NA), and all other files (NA).

- [x] T004 [US1] Smoke-test Branch 1 on paper `09567976221147259`: run `run_index("09567976221147259", download = FALSE)` and confirm:
  - `Fitting_Foglio1.csv` produces column records with `column_name` values like `PAST_SHAM`, `PAST_BETA`, `R^2` (not `...N`)
  - `MTTDATA_Raw Data.csv` produces column records with `column_name` values like `PAST_Mortgage`, `PAST_Home` and `col_header_group` values of `SHAM`, `BETA`, `ALPHA`

- [x] T005 [US1] Smoke-test Branch 1 on paper `09567976231151581`: run `run_index("09567976231151581", download = FALSE)` and confirm `Avreaged raw data_Sheet1.csv` produces column records with `column_name` values like `SUBJECT`, `LEARNING`, `TEST` and `col_header_group` values of `RB_TASK`, `II_TASK`.

- [x] T006 [US1] Smoke-test Branch 2 on paper `09567976221147259`: confirm `Experiment 3_Summary_MTT_Foglio1.csv` produces ≥1 row in `outputs/09567976221147259/columns.csv` with original partial names like `Passato...3` retained and `col_header_group = NA`.

**Checkpoint**: All 4 affected files produce column records with resolved `column_name` values. US1 complete.

---

## Phase 3: User Story 2 — Genuinely headerless files still skipped (Priority: P2)

**Goal**: Files with all-`...N` names and no usable sub-header must still be skipped.

**Independent Test**: Trace through the implemented logic: when all columns are pure `...N` AND no sub-header qualifies within 3 rows AND no non-`...N` name exists → `return(NULL)` is reached.

- [x] T007 [US2] In `data_check/0_index.R`: verify (read and trace) that the all-`...N` + no-sub-header + no-partial-label path reaches `return(NULL)`. Add a comment at that branch: `# skip: entirely placeholder header with no recoverable sub-header`.

**Checkpoint**: Regression safety confirmed.

---

## Phase 4: User Story 3 — Distinct skip message (Priority: P3)

**Goal**: The log message when multi-level resolution fails is distinct from generic skip messages.

- [x] T008 [US3] In `data_check/0_index.R`: confirm the failed-resolution skip message reads `"skipping (multi-level header, no usable sub-header found): "`. This should already be in place from T002; verify and update if not.

**Checkpoint**: All three user stories complete.

---

## Phase 5: Polish & Documentation

- [x] T009 [P] Update `data_check/docs/output-schemas.md`: add `col_header_group` column definition to the `columns.csv` schema table. Description: "Condition/group label from the first header row, forward-filled across the span it covers. `NA` for files without multi-level headers or for columns preceding the first group label."

- [x] T010 [P] Update `data_check/docs/pipeline.md`: add `MULTILEVEL_HEADER_LOOKAHEAD` to the Key Constants table.

- [x] T011 [P] Update `data_check/progress.md`: add entry for feature 014 describing the multi-level header fix and `col_header_group` addition.

- [x] T012 Clean up the stale placeholder line added by the agent-context script in `data_check/CLAUDE.md` (the `[if applicable…]` database line from the initial unfilled plan run).

---

## Dependencies & Execution Order

- **T001** — no dependencies; do first
- **T002** — depends on T001 (uses the constant)
- **T003** — depends on T002 (extends the column record builder with the new field)
- **T004, T005, T006** — all depend on T002 + T003; can run in parallel (different papers)
- **T007** — depends on T002 + T003 (code trace)
- **T008** — depends on T002 (message text lives in the same block)
- **T009, T010, T011, T012** — independent of each other [P]; depend on T002–T008 complete

### Parallel Opportunities

T004, T005, T006 — different paper IDs, independent:
```
Task: smoke-test 09567976221147259  (Branch 1 — Fitting + MTTDATA)
Task: smoke-test 09567976231151581  (Branch 1 — Avreaged raw data)
Task: smoke-test 09567976221147259  (Branch 2 — Experiment 3 Summary)
```

T009–T012 — different files, independent:
```
Task: update output-schemas.md
Task: update pipeline.md
Task: update progress.md
Task: fix CLAUDE.md
```

---

## Implementation Strategy

### MVP (User Story 1 only — 6 tasks)

1. T001: Add constant
2. T002: Implement Branch 1 + Branch 2 + `col_header_group` forward-fill
3. T003: Thread `col_header_group` into column record construction
4. T004–T006: Smoke-test all 4 affected files

**STOP and validate**: all 4 files produce records with resolved `column_name` values.

### Full delivery (all stories — 12 tasks)

5. T007: Verify headerless-skip regression
6. T008: Confirm distinguishing log message
7. T009–T012: Documentation + cleanup

---

## Notes

- All logic changes are in `extract_column_info()` in `0_index.R`
- `helper.R` is not modified — `match_column_labels()` works directly on `column_name` which is now the resolved sub-header value
- `2_codebook_label.R` is not modified — `labels.csv` schema is unchanged
- `col_header_group` is `NA` for all files that don't trigger multi-level detection — existing `columns.csv` rows for non-affected files are unaffected (new column, same NA fill)
- The pseudocode in `plan.md` is the authoritative algorithm for T002
