# Tasks: Reduce Over-Conservative `unknown` Column Type Classifications

**Input**: Design documents from `specs/004-reduce-unknown-coltypes/`
**Prerequisites**: plan.md ✅ spec.md ✅ research.md ✅ data-model.md ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

---

## Phase 1: Foundational (Blocking Prerequisite)

**Purpose**: Add the `is_numeric` field scaffold to all existing returns in
`classify_col_type_rules()`. Required before any story-specific changes so the function
signature stays consistent throughout.

**⚠️ CRITICAL**: US1 and US2 both touch `classify_col_type_rules()` — this phase must complete first.

- [x] T001 Add `is_numeric = FALSE` to every existing `return(list(...))` call in `classify_col_type_rules()` in `data_check/helper.R` (Rules 1, 2, 3, 4, 5, 6-high-unique, 7, 8, 9 — 9 return sites total). Do NOT add Rule 6a yet.

**Checkpoint**: All existing returns have `is_numeric = FALSE`. Function behaviour is unchanged.

---

## Phase 2: User Story 1 — Decimal numeric → `continuous` (Priority: P1) 🎯 MVP

**Goal**: Decimal-valued numeric columns are classified as `continuous` by the rule-based
classifier without any LLM call. Eliminates false `unknown` for VAS and ratio-scale columns.

**Independent Test**: Run `run_index("0956797615583071")` and confirm columns
`pre_film_VAS_Sad`, `pre_film_VAS_Hopelessness`, `pre_film_VAS_Depressed`, `pre_film_VAS_Fear`,
`pre_film_VAS_Horror`, and `IES_R_Intrusion_subscale` all have `col_type = "continuous"`.

### Implementation for User Story 1

- [x] T002 [US1] Insert Rule 6a into `classify_col_type_rules()` in `data_check/helper.R` between Rule 5 (free-text) and Rule 6 (numeric). Rule 6a: if `is.numeric(values)` AND `any(x_noNA != floor(x_noNA))`, return `list(col_type = "continuous", ambiguous = FALSE, numeric_values = values, n_coerced = NA_integer_, is_numeric = FALSE)`. See plan.md Step 1 Change A for exact code.

**Checkpoint**: Re-run paper `0956797615583071`. All decimal-valued columns should now show `col_type = "continuous"` and have numeric stats populated. The LLM should receive fewer columns to classify.

---

## Phase 3: User Story 2 — Integer scale columns no longer `unknown` (Priority: P2)

**Goal**: Integer-valued columns routed to the LLM (3–20 unique values) either get a correct
classification (`ordinal`/`continuous`) from the improved prompt, or fall back to `continuous`
if the LLM still returns `unknown`. Eliminates false `unknown` for rating scales and counts.

**Independent Test**: Run `run_index("0956797615583071")` and confirm `Attention_paid_to_film`,
`Post_film_Distress`, `Diary_Compliance`, `BDI_II`, `Tetris_Demand_Rating`, and
`Days_One_to_Seven_Image_Based_Intrusions_in_Intrusion_Diary` all have `col_type != "unknown"`.

### Implementation for User Story 2

- [x] T003 [US2] Modify Rule 6 in `classify_col_type_rules()` in `data_check/helper.R`: change the `ambiguous = TRUE` return (integer, 3–20 unique values) to include `is_numeric = TRUE` instead of `is_numeric = FALSE`. The `n_unique > 20` return keeps `is_numeric = FALSE`. See plan.md Step 1 Change B.

- [x] T004 [P] [US2] Improve `COLUMN_TYPE_PROMPT` constant in `data_check/0_index.R`: extend `continuous` definition to cover VAS, Likert means, subscale scores, bounded integers; extend `ordinal` to cover bounded integer scales; add explicit instruction "Prefer continuous or ordinal over unknown — only use unknown when name and values give no numeric signal whatsoever. When in doubt between continuous and ordinal, choose continuous." See plan.md Step 2 for exact text.

- [x] T005 [US2] In `extract_column_info()` inside `data_check/0_index.R`, extract `is_numeric_vec` from `col_classifications` (after the existing `ambiguous_idx` extraction at ~line 436): `is_numeric_vec <- vapply(col_classifications, function(cls) isTRUE(cls$is_numeric), logical(1))`. Add `is_numeric = is_numeric_vec` as a column in the `data.frame(columns = ...)` return. See plan.md Step 3. (Depends on T003)

- [x] T006 [US2] Add post-LLM numeric fallback in `data_check/0_index.R` immediately after `columns_df$col_type[ambig_rows] <- returned_types` (~line 557): for rows where `columns_df$is_numeric == TRUE` AND `col_type == "unknown"`, set `col_type <- "continuous"` and emit a message. See plan.md Step 4. (Depends on T005)

- [x] T007 [US2] Drop the transient `is_numeric` column in the final cleanup block in `data_check/0_index.R` (~line 569) by adding `columns_df$is_numeric <- NULL` alongside the existing `columns_df$sample_values_unique <- NULL`. See plan.md Step 5. (Depends on T005)

**Checkpoint**: Re-run paper `0956797615583071`. No purely numeric column with ≥ 3 unique non-NA values should have `col_type = "unknown"`. Total `unknown` count should be ≤ 10% of all columns. Verify `is_numeric` column is absent from the written CSV.

---

## Phase 4: Polish & Verification (US3 — Consistency across experiments)

**Purpose**: Confirm consistent classification for matching column names across experiment groups,
and verify no regressions on existing non-numeric classification types.

- [ ] T008 *(manual verification)* Verify consistency: after running paper `0956797615583071`, check that `BDI_II`, `Tetris_Demand_Rating`, `Diary_Compliance`, and `IES_R_Intrusion_subscale` have the same `col_type` in both `ex1` and `ex2` groups. Verify `binary` columns (`Time_of_Day`, `Condition` in ex1) are still `binary`. Verify `continuous` columns (`STAI_T`, `post_film_VAS_Sad`) retain `continuous`. Document result counts (total columns, unknown count before vs. after) in a comment at the top of this tasks.md or in a brief note.

- [ ] T009 [P] Run `run_index()` on at least one other paper from `bulk_summary.csv` that previously had a high `unknown` rate to confirm the fix generalises and does not produce regressions on non-numeric columns in other datasets.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Foundational)**: No dependencies — start immediately
- **Phase 2 (US1)**: Depends on T001 completing
- **Phase 3 (US2)**: Depends on T001 completing; T004 can run in parallel with T003; T005 depends on T003; T006 and T007 depend on T005
- **Phase 4 (Polish)**: Depends on Phases 2 and 3 completing

### Within Phase 3

```
T003 ──→ T005 ──→ T006
                 ──→ T007
T004 (parallel, independent of T003/T005/T006/T007)
```

### Parallel Opportunities

```bash
# After T001 completes, Phase 2 and Phase 3 prep can start:
Task T002: "Insert Rule 6a into classify_col_type_rules() in data_check/helper.R"
Task T003: "Modify Rule 6 to set is_numeric = TRUE in data_check/helper.R"
Task T004: "Improve COLUMN_TYPE_PROMPT in data_check/0_index.R"

# T003 and T004 can run in parallel (different locations)
# T002 and T003 can run in parallel (different rules in the same function — non-overlapping edits)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete T001: Add `is_numeric = FALSE` scaffold
2. Complete T002: Add Rule 6a
3. **STOP and VALIDATE**: Run paper `0956797615583071`, confirm VAS/decimal columns are `continuous`
4. This alone eliminates the majority of false `unknown` classifications

### Incremental Delivery

1. T001 → T002 → Validate (US1 complete — decimal columns fixed)
2. T003 → T004 (parallel) → T005 → T006 → T007 → Validate (US2 complete — integer fallback added)
3. T008 → T009 → Polish complete

### Single-developer Order

T001 → T002 → T003 → T004 → T005 → T006 → T007 → T008 → T009

---

## Notes

- [P] tasks operate on different code locations and can be done in either order
- T002 and T003 both edit `classify_col_type_rules()` but at non-overlapping locations (Rule 6a is inserted *before* Rule 6; Rule 6 modification is the *existing* block). Edit T002 first, then T003 to avoid confusion.
- No new packages required — all changes use base R
- Output CSV schema is unchanged; verify with `names(read.csv(...columns.csv))`
- Constitution Principle IV: all changes go in `helper.R` and `0_index.R` — no new files needed
