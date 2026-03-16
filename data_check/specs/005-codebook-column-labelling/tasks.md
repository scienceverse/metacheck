# Tasks: Codebook Column Labelling

**Input**: Design documents from `/specs/005-codebook-column-labelling/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅

**Tests**: Not requested — no test tasks generated.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- Exact file paths included in all descriptions

---

## Phase 1: Setup

**Purpose**: Create the new script file and establish the constants that all phases depend on.

- [x] T001 Create `2_codebook_label.R` with header comment, constants `MAX_CODEBOOK_LLM_CALLS = 3`, `MAX_CODEBOOK_FILE_MB = 100`, `CODEBOOK_TYPES = c("codebook", "readme")`, and a stub `run_codebook_label(paper_id)` that sources `helper.R` and stops with a message

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Shared helpers and data-loading skeleton that every user story requires before any story-specific work can begin.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [x] T002 In `2_codebook_label.R`, implement the data-loading body of `run_codebook_label(paper_id)`: read `structure/<paper_id>_structure.csv` with `colClasses = c(paper_id = "character")`, read `structure/<paper_id>_columns.csv` with same colClasses, stop with informative message if either file is missing
- [x] T003 Add `parse_codebook(path)` to `helper.R`: rule-based structured extraction — scan the first row of CSV/TSV/Excel files for a column matching `(?i)(var(iable)?|name|column)` and another matching `(?i)(label|description|desc|definition|meaning)`; return a data.frame with columns `codebook_variable`, `label`, `codebook_source`, `group = NA_character_`; return `NULL` (with warning) for files exceeding `MAX_CODEBOOK_FILE_MB` or on read error
- [x] T004 Extend `parse_codebook()` in `helper.R` with a SPSS/DTA branch: when file extension is `.sav` or `.dta`, use `haven::read_sav()` / `haven::read_dta()` and extract embedded variable labels from the `label` attribute of each column via `attr(df[[col]], "label")`; append these rows to the same return schema as T003 (depends on T003 for return schema)
- [x] T005 Extend `parse_codebook()` in `helper.R` with an LLM fallback branch: when rule-based extraction (T003/T004) yields zero rows, read the file as plain text with `readLines()` (up to `MAX_CODEBOOK_FILE_MB` limit), split into chunks of 40 lines, call `llm_batch()` with a system prompt requesting JSON `[{"variable_name":"...", "label":"...", "experiment_context":"..."}]`, map results to the return schema with `group = NA_character_`; cap at `MAX_CODEBOOK_LLM_CALLS` chunks (depends on T003 for return schema)
- [x] T006 [P] Add `normalize_varname(x)` private helper to `helper.R`: applies `tolower()`, `trimws()`, collapses interior whitespace with `gsub("\\s+", " ", x)`, and strips leading/trailing underscores and dots; usable by both `parse_codebook()` and `match_column_labels()`

**Checkpoint**: Foundation ready — `parse_codebook()` and `normalize_varname()` are callable; `run_codebook_label()` loads input data. All user stories can now proceed.

---

## Phase 3: User Story 1 — Label Columns from Codebook (Priority: P1) 🎯 MVP

**Goal**: For a paper with a codebook and data files, produce `_labels.csv` where every column that appears in the codebook carries its label and every unmatched column is marked `unlabelled`.

**Independent Test**: Run `run_codebook_label("<paper_id>")` on a paper with a known structured codebook (e.g., a CSV codebook with clear variable/label columns). Inspect `structure/<paper_id>_labels.csv` and verify that matched columns have non-NA `label` and `label_status = "labelled"`, and that columns absent from the codebook have `label_status = "unlabelled"`.

### Implementation for User Story 1

- [x] T007 [US1] Add `match_column_labels(columns_df, codebook_vars_df)` to `helper.R`: normalise both `column_name` and `codebook_variable` using `normalize_varname()`; left-join `columns_df` to `codebook_vars_df` on normalised name; set `label_status = "labelled"` for matches and `"unlabelled"` for non-matches; return a data.frame matching the `_labels.csv` schema from `data-model.md` (depends on T003 for codebook schema, T006 for normalize_varname)
- [x] T008 [US1] Wire `parse_codebook()` and `match_column_labels()` into `run_codebook_label()` in `2_codebook_label.R`: filter `_structure.csv` rows to `type %in% CODEBOOK_TYPES`, call `parse_codebook()` on each path, `rbind` results into a single `codebook_vars_df`, call `match_column_labels()`, assign `paper_id` column (depends on T002, T007)
- [x] T009 [US1] Add `no_codebook` handling to `run_codebook_label()` in `2_codebook_label.R`: when no rows in `_structure.csv` have `type %in% CODEBOOK_TYPES`, set `label_status = "no_codebook"` for all rows in `_labels.csv` and skip parsing and matching (depends on T008)
- [x] T010 [US1] Add `_labels.csv` write step to `run_codebook_label()` in `2_codebook_label.R`: write the labels data.frame to `structure/<paper_id>_labels.csv` using `write.csv(..., row.names = FALSE)`; emit a message with row count (depends on T009)
- [x] T011 [US1] Update `docs/output-schemas.md`: add `_labels.csv` schema table with all columns and the Label Status Values table as defined in `data-model.md`

**Checkpoint**: At this point, User Story 1 is fully functional. `run_codebook_label()` produces a correct `_labels.csv` for a paper with a structured codebook.

---

## Phase 4: User Story 2 — Experiment-Scoped Labelling (Priority: P2)

**Goal**: When a paper has multiple experiments, columns in Experiment 1 receive only Experiment 1's definitions, and same-name variables from different experiments are never conflated.

**Independent Test**: Run `run_codebook_label()` on a paper whose codebook has separate sections for Experiment 1 and Experiment 2 with a shared variable name defined differently. Inspect `_labels.csv` and verify that each experiment's columns carry the correct scoped label and that no cross-experiment assignment occurs.

### Implementation for User Story 2

- [x] T012 [US2] Extend `parse_codebook()` in `helper.R` with experiment context inference: after extracting variables, scan section headings or nearby text for patterns like `(?i)experiment\s*(\d+)` / `(?i)study\s*(\d+)` / `(?i)pilot\s*(\d+)` and map matches to group values (e.g., `ex1`, `ex2`, `pilot1`) using the same group vocabulary from `_structure.csv`; set `group` on matching rows; rows with no detected context keep `group = NA_character_` (treated as applicable to all groups) (depends on T003/T005 for parse output)
- [x] T013 [US2] Extend `match_column_labels()` in `helper.R` with group-scoped matching: when a codebook variable has a non-NA `group`, restrict matching to `columns_df` rows where `group == codebook_var$group`; columns that match only scoped variables from a different group remain `unlabelled`; columns that match scoped variables from multiple groups → set `label_status = "ambiguous_experiment"` and pipe-concatenate all candidate labels and sources (depends on T007)
- [x] T014 [US2] Add `conflicting_definition` detection to `match_column_labels()` in `helper.R`: when a column matches the same normalized variable name in multiple codebook sources with different label text, set `label_status = "conflicting_definition"`, pipe-concatenate all distinct labels in `label`, and pipe-concatenate all sources in `label_source` (depends on T013)

**Checkpoint**: At this point, User Stories 1 and 2 work. Experiment-scoped labelling and conflict detection are operational.

---

## Phase 5: User Story 3 — Coverage Reporting (Priority: P3)

**Goal**: After labelling, the researcher can inspect a per-codebook-variable table showing which variables were matched to data columns and which were not, along with summary counts.

**Independent Test**: Run `run_codebook_label()` on a paper and inspect `structure/<paper_id>_codebook_coverage.csv`. Verify that every extracted codebook variable appears, `match_status` is `matched` or `unmatched_in_data`, and the returned summary counts are arithmetically consistent with `_labels.csv`.

### Implementation for User Story 3

- [x] T015 [US3] Add coverage table assembly to `run_codebook_label()` in `2_codebook_label.R`: after `match_column_labels()` returns, build `_codebook_coverage.csv` by joining `codebook_vars_df` against the matched variable names in `_labels.csv`; set `match_status = "matched"` if the codebook variable appears in any labelled row, `"unmatched_in_data"` otherwise (depends on T010)
- [x] T016 [US3] Add `_codebook_coverage.csv` write step and `LabellingResult` return value to `run_codebook_label()` in `2_codebook_label.R`: write coverage table to `structure/<paper_id>_codebook_coverage.csv`; return a named list with `labels_df`, `coverage_df`, `n_labelled`, `n_unlabelled`, `n_codebook_vars`, `n_matched_vars`, `label_status` (depends on T015)
- [x] T017 [US3] Update `docs/output-schemas.md`: add `_codebook_coverage.csv` schema table with all columns and `match_status` values as defined in `data-model.md`

**Checkpoint**: All three user stories are independently functional.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Documentation sync, constitution compliance audit, and robustness hardening.

- [x] T018 Update `docs/pipeline.md`: add Stage 12 (Codebook Labelling) to the end-to-end flow diagram after Stage 11 (Append to bulk summary); add `MAX_CODEBOOK_LLM_CALLS` and `MAX_CODEBOOK_FILE_MB` to the Key Constants table
- [x] T019 [P] Audit `2_codebook_label.R` for Principle II compliance: confirm every `read.csv()` that loads paper IDs uses `colClasses = c(paper_id = "character")`; fix any missing `colClasses` arguments
- [x] T020 [P] Add the LLM system prompt constant `CODEBOOK_PARSE_PROMPT` near the top of `2_codebook_label.R` (consistent with `COLUMN_TYPE_PROMPT` pattern in `0_index.R`); update the `parse_codebook()` LLM fallback (T005) to reference this constant

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 — BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Phase 2 completion
- **User Story 2 (Phase 4)**: Depends on Phase 3 completion (extends `parse_codebook()` and `match_column_labels()` established in Phase 3)
- **User Story 3 (Phase 5)**: Depends on Phase 3 completion (US3 only adds coverage output alongside existing labels output)
- **Polish (Phase 6)**: Depends on all user story phases

### User Story Dependencies

- **US1 (P1)**: Requires Phase 2 complete — no story dependencies
- **US2 (P2)**: Requires US1 complete — extends the same helper functions
- **US3 (P3)**: Requires US1 complete — adds coverage output alongside existing labels output; can be worked in parallel with US2

### Within Each Phase

- T003, T006 in Phase 2 are independent and can be done in parallel
- T004, T005 both depend on T003 and can be done in parallel with each other
- T012, T013 in Phase 4 are sequential (T013 extends T012's scoping output)
- T015, T016 in Phase 5 are sequential

### Parallel Opportunities

- T003 and T006 (Phase 2) — different functions
- T004 and T005 (Phase 2) — different branches of same function, can be drafted in parallel
- T015 and (T012 + T013) if US2 and US3 are started together after US1
- T019 and T020 (Phase 6) — different concerns in same file

---

## Parallel Example: Phase 2

```
Can run together:
  T003: Add parse_codebook() structured branch to helper.R
  T006: Add normalize_varname() to helper.R

After T003 completes, can run together:
  T004: Extend parse_codebook() with SPSS/DTA branch
  T005: Extend parse_codebook() with LLM fallback branch
```

## Parallel Example: After US1

```
Can run in parallel once T010 is complete:
  Path A (US2): T012 → T013 → T014
  Path B (US3): T015 → T016 → T017
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001)
2. Complete Phase 2: Foundational (T002–T006)
3. Complete Phase 3: User Story 1 (T007–T011)
4. **STOP and VALIDATE**: Run `run_codebook_label()` on a known paper; inspect `_labels.csv`
5. Proceed to US2/US3 only after MVP validated

### Incremental Delivery

1. T001 → T002–T006 → T007–T011 → **MVP: basic labelling works**
2. Add T012–T014 → **Experiment scoping and conflict detection**
3. Add T015–T017 → **Coverage reporting**
4. Add T018–T020 → **Documentation and compliance polish**

---

## Notes

- [P] tasks = different files or independent concerns, no blocking dependencies
- [Story] label maps task to specific user story for traceability
- No test tasks generated (not requested in spec)
- Commit after each logical group (e.g., after each user story phase checkpoint)
- `normalize_varname()` (T006) is a private helper — does not need to be exported
- The LLM fallback in `parse_codebook()` shares the per-paper call budget (Principle III); `MAX_CODEBOOK_LLM_CALLS = 3` is the cap
- All `read.csv()` calls must use `colClasses = c(paper_id = "character")` (Principle II)
