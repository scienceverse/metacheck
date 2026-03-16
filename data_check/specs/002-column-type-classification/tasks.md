# Tasks: Column Type Classification and Value Normalization

**Input**: Design documents from `specs/002-column-type-classification/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)
- No tests section — not requested in spec

## Path Conventions

All changes are in `data_check/` at the repository root. Two files are modified:
- `data_check/helper.R` — shared helpers (rule-based classification function)
- `data_check/0_index.R` — pipeline script (integration, LLM call, schema change)

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Add the shared constant and function skeleton before any user story logic is written. Both tasks touch different files and can be done in parallel.

- [ ] T001 [P] Add `VALID_COL_TYPES` character vector constant to the constants block in `data_check/0_index.R` (after the existing `N_DATA_READ` constant). Value: `c("continuous","binary","categorical","ordinal","date","id","text","continuous_comma_decimal","continuous_outliers_excluded","empty","unknown")`
- [ ] T002 [P] Add `classify_col_type_rules()` function skeleton to `data_check/helper.R` (after `classify_by_rules()`). Signature: `classify_col_type_rules <- function(col_name, values)`. Return structure: `list(col_type = NA_character_, ambiguous = FALSE, numeric_values = NULL)`. Body: single `return()` of the fallback — no logic yet.

**Checkpoint**: Both files compile without error. `classify_col_type_rules("x", 1:5)` returns the fallback list.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: No additional foundational work required beyond Phase 1. The existing `llm_batch()` infrastructure and `extract_column_info()` scaffold are already in place. The Phase 1 skeleton is the only blocker.

**⚠️ CRITICAL**: Phase 1 must be complete before any user story work begins.

---

## Phase 3: User Story 1 — Type Labels in Output CSV (Priority: P1) 🎯 MVP

**Goal**: Every row in `*_columns.csv` has a `col_type` value. Rule-based logic handles clear-cut cases (binary, id, date, text, continuous, categorical). Columns that are ambiguous get `NA` for now (resolved in US3).

**Independent Test**: Run `run_index("0956797620903716")`. Open the output `*_columns.csv`. Verify: (1) a `col_type` column exists between `sample_values` and `n`; (2) "Gender" column has `col_type = "binary"` or `"categorical"`; (3) `n` and `n_missing` are populated for all non-empty rows.

### Implementation for User Story 1

- [ ] T003 [US1] Implement the full rule-based classification cascade (rules 1–6 and 8–9 from plan.md Phase A, **excluding** comma-decimal normalization) in `classify_col_type_rules()` in `data_check/helper.R`. The function must:
  - Rule 1: all-NA → `col_type = "empty"`
  - Rule 2: n_unique ≤ 2 → `col_type = "binary"`
  - Rule 3: col_name matches ID regex AND all values are near-integer → `col_type = "id"`
  - Rule 4: ≥ 70% of unique sample (up to 20) parse via `as.Date()` or `as.POSIXct()` → `col_type = "date"`
  - Rule 5: median `nchar()` of non-NA values > 40 → `col_type = "text"`
  - Rule 6: `is.numeric(values)` AND n_unique > 20 → `col_type = "continuous"`; AND n_unique 3–20 → `col_type = NA, ambiguous = TRUE`
  - Rule 8: character column, n_unique ≤ 10 AND median nchar ≤ 20 → `col_type = "categorical"`
  - Rule 9: character column fallback → `col_type = "text"`
  - Rule 10: numeric ambiguous (from rule 6) → `ambiguous = TRUE`, `col_type = NA`

- [ ] T004 [US1] Modify `extract_column_info()` in `data_check/0_index.R` to call `classify_col_type_rules(col, df[[col]])` for each column, collecting results into a `col_types` character vector (NA where ambiguous) and an `ambiguous_idx` logical vector. Thread these through the existing `col_stats` loop so each column's classification is computed alongside its stats.

- [ ] T005 [US1] Insert `col_type = col_types` into the `data.frame()` constructor in `extract_column_info()` in `data_check/0_index.R`, positioned between `sample_values` and `stats_mat` (i.e., column 7 in the output). Verify column order matches data-model.md.

- [ ] T006 [US1] In `extract_column_info()` in `data_check/0_index.R`, after building the column data frame, zero out numeric stat fields (mean, sd, se, median, min, max, range, p25, p75, iqr, skewness, kurtosis) for rows where `col_type` is not in `c("continuous","continuous_comma_decimal","continuous_outliers_excluded")` AND is not NA. Keep `n` and `n_missing` populated. This is the initial stat suppression — note NA rows are left intact for the LLM step (US3).

**Checkpoint**: At this point, User Story 1 is fully functional. Re-run `run_index("0956797620903716")` — the output CSV has a `col_type` column with sensible labels. The "Response time" column (comma-decimal) may still show `col_type = "text"` — that is fixed in US2.

---

## Phase 4: User Story 2 — Statistics for Recoverable Malformed Numeric Columns (Priority: P2)

**Goal**: Columns that are non-numeric only because of locale-specific decimal separators (comma instead of period) are detected, normalized, and have statistics computed. Columns with a small fraction of non-parseable values are similarly recovered.

**Independent Test**: Run `run_index("0956797620903716")`. In the output `*_columns.csv`, locate the "Response time" rows from `Final_Data_PDR1.csv`. Verify: (1) `col_type = "continuous_comma_decimal"`; (2) `mean`, `median`, `sd` are populated (not NA); (3) the statistics are numerically plausible (mean ≈ 7.7 as in the companion file).

### Implementation for User Story 2

- [ ] T007 [US2] Extend `classify_col_type_rules()` in `data_check/helper.R` to add comma-decimal normalization (plan.md Phase A, rule 7) **before** the categorical/text character-column rules (rules 8–9). Logic:
  - For character columns: apply `x_sub <- suppressWarnings(as.numeric(gsub(",", ".", x_noNA, fixed = TRUE)))`
  - Compute `pct_ok <- sum(!is.na(x_sub)) / length(x_noNA)`
  - If `pct_ok >= 0.95` → `col_type = "continuous_comma_decimal"`, return `numeric_values = as.numeric(gsub(",", ".", values, fixed = TRUE))`
  - If `pct_ok >= 0.80` → `col_type = "continuous_outliers_excluded"`, return `numeric_values = as.numeric(gsub(",", ".", values, fixed = TRUE))` (non-parseable become NA)
  - Otherwise fall through to rules 8–9 (categorical/text)

- [ ] T008 [US2] Modify the stats computation block in `extract_column_info()` in `data_check/0_index.R` so that when `cls$col_type` is `"continuous_comma_decimal"` or `"continuous_outliers_excluded"`, stats are computed using `cls$numeric_values` (the substituted/coerced vector) rather than the raw column. Ensure `n_missing` for `"continuous_outliers_excluded"` includes the coerced-to-NA outlier count.

**Checkpoint**: US1 + US2 are both functional. The "Response time" column now has statistics. All other previously-classified columns are unaffected.

---

## Phase 5: User Story 3 — LLM-Assisted Classification for Ambiguous Columns (Priority: P3)

**Goal**: Numeric columns with 3–20 unique values (which rule-based logic marks as ambiguous) are sent to the LLM in batches for classification. The returned labels are validated against `VALID_COL_TYPES` and written back to `columns_df`. A final stat suppression pass clears stats for any newly-classified non-continuous columns.

**Independent Test**: Identify a paper with an integer Likert column (e.g., `Education` in `0956797620903716` which has values 1–9). Verify after running `run_index()` that this column receives `col_type = "ordinal"` or `"categorical"` (not `"unknown"`) and has NA numeric stats.

### Implementation for User Story 3

- [ ] T009 [P] [US3] Add two constants to `data_check/0_index.R` in the constants block: `MAX_COL_TYPE_LLM_CALLS <- 5L` and `COLUMN_TYPE_PROMPT` string. The prompt content (from plan.md Phase C): classifies each column descriptor as one of `continuous`, `ordinal`, `categorical`, `binary`, `id`, `unknown`. Ask for JSON array with `{"descriptor": "...", "col_type": "..."}` per element, no explanation.

- [ ] T010 [P] [US3] Inside `extract_column_info()` in `data_check/0_index.R`, for columns where `cls$ambiguous == TRUE`, compute a `sample_values_unique` string: `paste(unique(values[!is.na(values)])[seq_len(min(10, length(unique(values[!is.na(values)]))))], collapse = ", ")`. Store this alongside `col_type = NA` in the column data frame as a transient column (it will be used in T011 and removed before CSV write).

- [ ] T011 [US3] After `column_list` is assembled and `columns_df` is built in `data_check/0_index.R`, add the LLM batch step: collect rows where `is.na(columns_df$col_type)`, build descriptor strings (`'"<col_name>" (samples: <sample_values_unique>)'`), cap at `MAX_COL_TYPE_LLM_CALLS * LLM_BATCH_SIZE` columns, call `llm_batch()` with `COLUMN_TYPE_PROMPT`, validate returned `col_type` values against `VALID_COL_TYPES` (map invalid → `"unknown"`), and write back to `columns_df$col_type`.

- [ ] T012 [US3] Add the final resolution pass in `data_check/0_index.R` after the LLM step: (1) set any remaining `NA` in `columns_df$col_type` to `"unknown"`; (2) apply full stat suppression — clear numeric stat fields for all rows where `col_type` is not in `c("continuous","continuous_comma_decimal","continuous_outliers_excluded")`; (3) drop the transient `sample_values_unique` column from `columns_df` before `write.csv()`.

**Checkpoint**: All three user stories are functional. Ambiguous columns now have LLM-assigned labels. No NA values remain in `col_type`.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Regression validation and vocabulary enforcement.

- [ ] T013 Add a vocabulary guard assertion in `data_check/0_index.R` immediately before `write.csv(columns_df, ...)`: `invalid_types <- setdiff(unique(columns_df$col_type), VALID_COL_TYPES); if (length(invalid_types) > 0) warning("Unknown col_type values: ", paste(invalid_types, collapse=", "))`. This surfaces pipeline bugs without crashing.

- [ ] T014 [P] Run `run_index("0956797620903716")` and manually verify all six success criteria from spec.md: SC-001 (col_type present in all rows), SC-002 (Response time has stats + col_type = "continuous_comma_decimal"), SC-004 (userid = "id"), SC-005 (success = TRUE). Document results as a comment in the commit message.

- [ ] T015 [P] Run `run_index("0956797619831964")` (tweet dataset paper) and verify: `userid` column gets `col_type = "id"`, `date` column gets `col_type = "date"`, numeric count columns get `col_type = "continuous"` or LLM-assigned label.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (T001, T002)**: No dependencies — start immediately, run in parallel
- **Phase 2**: No extra tasks — Phase 1 is the only blocker
- **Phase 3 (T003–T006)**: Depends on T001 + T002 complete — BLOCKS all user stories
- **Phase 4 (T007–T008)**: Depends on T003–T006 complete (US1 integration must be in place)
- **Phase 5 (T009–T012)**: T009 and T010 can start after T004 (need `ambiguous` flag); T011 depends on T009 + T010 + T008 (LLM prompt + unique samples + columns_df built); T012 depends on T011
- **Phase 6 (T013–T015)**: Depends on all prior phases complete

### User Story Dependencies

- **US1 (P1)**: Depends only on Phase 1. No dependency on US2 or US3.
- **US2 (P2)**: Depends on US1 complete (modifies the same function `classify_col_type_rules()` and the same stats block in `extract_column_info()`).
- **US3 (P3)**: Depends on US1 complete (needs `ambiguous` flag and `columns_df` structure). Independent of US2.

### Within Each User Story

- T003 before T004 (function must exist before it is called)
- T004 before T005 (col_types vector must be collected before inserting into data.frame)
- T005 before T006 (col_type column must exist before stat suppression)
- T007 before T008 (normalization logic must exist before stats use it)
- T009 and T010 in parallel (different aspects of ambiguous column prep)
- T011 before T012 (LLM call before final cleanup pass)

### Parallel Opportunities

```bash
# Phase 1 — run both in parallel:
T001  # 0_index.R: add VALID_COL_TYPES constant
T002  # helper.R: add classify_col_type_rules() skeleton

# Phase 5 prep — run in parallel after T004:
T009  # helper.R: add COLUMN_TYPE_PROMPT + MAX_COL_TYPE_LLM_CALLS
T010  # 0_index.R: compute sample_values_unique for ambiguous columns

# Phase 6 — run in parallel:
T014  # regression test paper 0956797620903716
T015  # regression test paper 0956797619831964
```

---

## Implementation Strategy

### MVP First (User Story 1 Only — T001–T006)

1. Complete Phase 1: T001, T002 (parallel)
2. Complete Phase 3: T003 → T004 → T005 → T006 (sequential)
3. **STOP and VALIDATE**: Run `run_index("0956797620903716")` — confirm `col_type` column present with sensible labels
4. Outcome: type labels in CSV for all clear-cut cases; comma-decimal and ambiguous columns get safe fallbacks

### Incremental Delivery

1. T001 + T002 (parallel) → skeleton ready
2. T003–T006 → US1 complete: type labels in output
3. T007–T008 → US2 complete: comma-decimal stats recovered
4. T009–T012 → US3 complete: LLM resolves ambiguous columns
5. T013–T015 → validation and regression guard

### Key Risk

US2 and US3 both modify `extract_column_info()` in `0_index.R`. To avoid conflicts: complete US2 (T007–T008) fully before starting T010–T012.

---

## Notes

- [P] tasks = different files or independent concerns, no shared-file conflicts
- [Story] label maps each task to its user story for traceability
- No test tasks — not requested in spec
- Commit after each user story checkpoint (T006, T008, T012) to preserve rollback points
- The `sample_values_unique` transient column is dropped from `columns_df` before CSV write (T012) — it must never appear in the output file
- `classify_col_type_rules()` must not call `llm_batch()` — it is rule-only per Constitution Principle IV (shared helper must remain pure/fast)
