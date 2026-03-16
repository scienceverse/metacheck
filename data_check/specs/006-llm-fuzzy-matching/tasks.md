# Tasks: LLM Fuzzy Column Matching

**Input**: Design documents from `/specs/006-llm-fuzzy-matching/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅

**Tests**: Not requested — no test tasks generated.

**Organization**: Tasks grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1, US2
- Exact file paths in all descriptions

---

## Phase 1: Setup

**Purpose**: Add the new prompt constant that the LLM matching pass requires.

- [ ] T001 Add `COLUMN_MATCH_PROMPT` constant to `2_codebook_label.R` (after `CODEBOOK_PARSE_PROMPT`): system prompt instructing the LLM to match unlabelled column names to unmatched codebook variable names, returning JSON array `[{"column_name":"...","codebook_variable":"..."}]`, only confident pairs, empty array if none

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Extend `match_column_labels()` signature and return value so both user stories can build on it.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [x] T002 In `match_column_labels()` in `helper.R`, add `column_match_prompt = NULL` as a new third parameter with default `NULL`; this parameter is not yet used — just update the signature and ensure existing callers are unaffected
- [x] T003 In `match_column_labels()` in `helper.R`, add `label_method_out <- rep(NA_character_, n)` alongside the existing output vectors; after the rule-based loop, set `label_method_out[i] <- "rules"` wherever `status_out[i] == "labelled"`; append `label_method = label_method_out` as the last column of the returned data.frame; update `make_empty()` to include `label_method = NA_character_` (depends on T002)
- [x] T004 In `run_codebook_label()` in `2_codebook_label.R`, pass `column_match_prompt = COLUMN_MATCH_PROMPT` to the `match_column_labels()` call (depends on T001, T002)

**Checkpoint**: `_labels.csv` now contains a `label_method` column. All existing rule-matched columns show `"rules"`, all unlabelled columns show `NA`. LLM pass not yet active.

---

## Phase 3: User Story 1 — LLM Resolves Unmatched Columns (Priority: P1) 🎯 MVP

**Goal**: After rule-based matching, unlabelled columns and unmatched codebook variables are submitted to the LLM; accepted pairings update the output with `label_status = "llm"` and `label_method = "llm"`.

**Independent Test**: Run `run_codebook_label("0956797615620784")` and inspect `structure/0956797615620784_labels.csv`. Verify that `STAI_S_Y_PRE`, `STAI_S_Y_DURING`, `STAI_S_Y_POST` now have `label_status = "llm"` with the correct labels from the codebook, and that `ID`, `Condition`, `Age`, `Sex`, `BART` still have `label_status = "labelled"` and `label_method = "rules"` (unchanged by the LLM pass).

### Implementation for User Story 1

- [x] T005 [US1] In `match_column_labels()` in `helper.R`, after the rule-based loop and before building the return data.frame, add an early-exit guard: `if (is.null(column_match_prompt)) { ... set label_method_out; return(...) }` — when prompt is NULL, skip the LLM pass entirely and return as before (depends on T003)
- [x] T006 [US1] In `match_column_labels()` in `helper.R`, collect `unlabelled_idx <- which(status_out == "unlabelled")`; extract unique normalized column names via `normalize_varname(columns_df$column_name[unlabelled_idx])`; collect `unmatched_vars_df` by filtering `codebook_vars_df` to rows whose `normalize_varname(codebook_variable)` does not appear in any row where `status_out == "labelled"`; if either set is empty skip the LLM call (depends on T005)
- [x] T007 [US1] In `match_column_labels()` in `helper.R`, build the LLM prompt body as a character string: numbered list of unlabelled column names, numbered list of unmatched codebook variables; call `llm(system_prompt = column_match_prompt, text = prompt_body)`; wrap in `tryCatch` returning `list(answer = "[]")` on error with a warning (depends on T006)
- [x] T008 [US1] In `match_column_labels()` in `helper.R`, parse the LLM response with `extract_json()` + `jsonlite::fromJSON()`; validate each proposed pair: `normalize_varname(column_name)` must be in `unlabelled_norm_cols` AND `normalize_varname(codebook_variable)` must be in `normalize_varname(unmatched_vars_df$codebook_variable)`; discard invalid pairs silently (depends on T007)
- [x] T009 [US1] In `match_column_labels()` in `helper.R`, for each valid LLM pair, find all rows in `columns_df` where `normalize_varname(column_name)` matches the pair's column; look up the matching row in `unmatched_vars_df`; update `label_out`, `cbk_var_out`, `src_out`, `status_out` (`"llm"`), and `label_method_out` (`"llm"`) for those rows (depends on T008)

**Checkpoint**: US1 complete. `run_codebook_label()` now makes a secondary LLM call for unmatched columns and raises labelling coverage. Rule-based labels are unaffected.

---

## Phase 4: User Story 2 — Label Method Provenance Column (Priority: P2)

**Goal**: Every row in `_labels.csv` has a correct `label_method` value: `"rules"`, `"llm"`, or `NA`.

**Independent Test**: After US1 is working, inspect `_labels.csv` for paper `0956797615620784`. Confirm: rows labelled by rules have `label_method = "rules"`, rows labelled by LLM have `label_method = "llm"`, unlabelled rows have `label_method = NA`. No row has an unexpected value.

### Implementation for User Story 2

- [x] T010 [US2] Update `docs/output-schemas.md`: in the `_labels.csv` schema table, add `label_method` row after `label_status`; add `"llm"` to the Label Status Values table with description "Matched by secondary LLM pass" (depends on T003)
- [x] T011 [US2] In `run_codebook_label()` in `2_codebook_label.R`, in the `no_codebook` branch, ensure the directly-constructed `labels_df` includes `label_method = NA_character_` for all rows (depends on T003)

**Checkpoint**: All user stories complete. `label_method` is correctly populated for all code paths including `no_codebook`.

---

## Phase 5: Polish & Cross-Cutting Concerns

- [x] T012 Verify the `_codebook_coverage.csv` `match_status` logic in `run_codebook_label()` in `2_codebook_label.R` still correctly identifies `"matched"` vars after the LLM pass: update the `matched_norm` derivation to include rows where `label_status %in% c("labelled", "llm")`

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 (T001) — BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Phase 2 completion
- **US2 (Phase 4)**: T010 depends only on T003 (can start after Phase 2); T011 depends on T003
- **Polish (Phase 5)**: Depends on US1 complete (T009)

### Within Each Phase

- T002 → T003 → T004 are sequential (same function, building on each other)
- T005 → T006 → T007 → T008 → T009 are sequential (single LLM pass pipeline)
- T010 and T011 are independent of each other and of US1

### Parallel Opportunities

- T010 [P] can start after T003; it is a docs edit independent of the LLM pass implementation
- T011 can start after T003 independently of T005–T009

---

## Parallel Example: After Phase 2

```
Can run in parallel once T003 is complete:
  Path A (US1): T005 → T006 → T007 → T008 → T009
  Path B (US2): T010 (docs), T011 (no_codebook branch)
```

---

## Implementation Strategy

### MVP First

1. T001 (Setup)
2. T002 → T003 → T004 (Foundational — adds label_method column, LLM pass off by default)
3. T005 → T009 (US1 — activates LLM pass)
4. **STOP and VALIDATE**: run `run_codebook_label("0956797615620784")`, check STAI columns labelled
5. T010, T011 (US2 — provenance completeness)
6. T012 (Polish — coverage fix)

---

## Notes

- T012 is critical for correctness: without it, LLM-matched vars will still show `"unmatched_in_data"` in `_codebook_coverage.csv` because the coverage logic only counted `label_status == "labelled"` rows
- `column_match_prompt = NULL` default means `match_column_labels()` is callable without a live LLM (useful for offline use or direct testing)
- The LLM call in T007 uses `llm()` directly, not `llm_batch()` — the whole candidate set is sent in one reasoning task
- Group scoping applies to the LLM candidate collection in T006: only unmatched vars applicable to the column's group (or unscoped) should be offered as candidates
