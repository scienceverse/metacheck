# Tasks: Fix Codebook Label Ambiguity (010)

**Input**: Design documents from `/specs/010-fix-label-ambiguity/`
**Branch**: `010-fix-label-ambiguity`

**Organization**: US1 = reduce false `conflicting_definition` flags; US2 = (investigated — no code change needed; BIS label matches source codebook exactly)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to

---

## Phase 1: Setup

**Purpose**: Confirm working state and baseline before changes.

- [x] T001 Verify current output for the three test papers by reading `outputs/0956797620948821/labels.csv`, `outputs/0956797620967261/labels.csv`, and `outputs/0956797617716929/labels.csv` — note the `conflicting_definition` row counts as a baseline

---

## Phase 2: Foundational (Blocking Prerequisite)

**Purpose**: Add `normalize_label()` to `helper.R` — shared by both resolution tiers. Must exist before any merge logic.

- [x] T002 Add `normalize_label(x)` function to `data_check/helper.R` immediately after `normalize_varname()` (around line 342). Implementation:
  1. `tolower(x)`
  2. Strip possessive apostrophes: `gsub("'[Ss]|'\u2019[Ss]?|\u2019[Ss]?", "", x, perl=TRUE)`
  3. Replace non-alphanumeric (except spaces) with space: `gsub("[^a-z0-9 ]", " ", x)`
  4. Strip standalone single-letter tokens that are unit-coding artifacts (optional — see research.md)
  5. Collapse whitespace: `gsub("\\s+", " ", trimws(x))`

  Add a comment block: `# Normalise a label string for semantic-equivalence comparison.`

**Checkpoint**: `normalize_label()` exists in `helper.R` and can be called.

---

## Phase 3: User Story 1 — Reduce False `conflicting_definition` Flags (Priority: P1) 🎯 MVP

**Goal**: Columns whose candidate labels are semantically equivalent get a single merged label instead of `conflicting_definition`.

**Independent Test**: Run `run_codebook_label("0956797620948821")` and `run_codebook_label("0956797620967261")`. Columns `Age`, `Gender`, `ResponseId`, `StartDate`, `EndDate`, `f_emb`, `f_bor`, `f_anx`, `f_rela`, `f_fear` must all have `label_status = "labelled"` (not `conflicting_definition`).

### Implementation for User Story 1

- [x] T003 [US1] In `data_check/helper.R`, inside `match_column_labels()` (around line 650), add the **rule-based merge tier** immediately after `distinct_labels <- unique(applicable$label)` and before the `if (length(distinct_labels) > 1)` block:

  ```r
  # Rule-based equivalence: normalise labels and re-check uniqueness
  if (length(distinct_labels) > 1) {
    norm_labels <- normalize_label(distinct_labels)
    if (length(unique(norm_labels)) == 1) {
      # All labels normalise to the same string — pick the longest original label
      canonical <- distinct_labels[which.max(nchar(distinct_labels))]
      status_out[i]        <- "labelled"
      label_out[i]         <- canonical
      cbk_var_out[i]       <- applicable$codebook_variable[1]
      src_out[i]           <- paste(unique(applicable$codebook_source), collapse = " | ")
      label_method_out[i]  <- "merged_rules"
      next  # skip the conflicting_definition block below
    }
  }
  ```

  The existing `if (length(distinct_labels) > 1) { status_out[i] <- "conflicting_definition" ... }` block follows unchanged.

- [x] T004 [US1] In `data_check/2_codebook_label.R`, add `LABEL_MERGE_PROMPT` constant after `COLUMN_MATCH_PROMPT` (around line 59):

  ```r
  LABEL_MERGE_PROMPT <- 'You are reviewing whether multiple label definitions for the same
  variable in a psychology research dataset are semantically equivalent.

  You will receive a JSON array of objects, each with "column" and "labels" fields.
  Return a JSON array — one object per input variable.
  Each object: {"column": "<column_name>", "equivalent": true/false, "canonical": "<best label or null>"}

  Rules:
  - equivalent: true if all listed labels describe the same construct (synonyms, different
    phrasings, or value-coding notation for the same concept as a semantic label)
  - canonical: if equivalent=true, return the most human-readable, informative single label;
    if equivalent=false, set to null
  - Do NOT mark as equivalent if labels describe genuinely different constructs or scales
  - Output ONLY the JSON array. No notes, no text outside the array.'
  ```

- [x] T005 [US1] In `data_check/helper.R`, update `match_column_labels()` signature to accept new optional argument `label_merge_prompt = NULL` (add after `column_match_prompt = NULL` on line 590):

  ```r
  match_column_labels <- function(columns_df, codebook_vars_df,
                                  column_match_prompt = NULL,
                                  label_merge_prompt  = NULL)
  ```

- [x] T006 [US1] In `data_check/helper.R`, add the **LLM merge tier** inside `match_column_labels()`, between the existing rule-based for-loop (ending around line 663) and the existing LLM column-matching pass (starting around line 668). The new block:

  ```r
  # ── LLM merge tier: resolve remaining conflicting_definition rows ─────────────
  if (!is.null(label_merge_prompt)) {
    conflict_idx <- which(status_out == "conflicting_definition")
    if (length(conflict_idx) > 0) {
      # Build batch input: one entry per conflicting column (unique column names)
      conflict_cols <- unique(columns_df$column_name[conflict_idx])
      batch_input <- lapply(conflict_cols, function(cn) {
        idx1 <- conflict_idx[columns_df$column_name[conflict_idx] == cn][1]
        raw_labels <- strsplit(label_out[idx1], " | ", fixed = TRUE)[[1]]
        list(column = cn, labels = raw_labels)
      })
      prompt_body <- paste0("Variables to check:\n",
                            jsonlite::toJSON(batch_input, auto_unbox = TRUE))
      merge_resp <- tryCatch(
        llm(system_prompt = label_merge_prompt, text = prompt_body),
        error = function(e) {
          warning("LLM label-merge call failed: ", conditionMessage(e))
          list(answer = "[]")
        }
      )
      merge_pairs <- tryCatch({
        parsed <- jsonlite::fromJSON(extract_json(merge_resp$answer),
                                     simplifyDataFrame = TRUE)
        if (is.data.frame(parsed) && nrow(parsed) > 0 &&
            all(c("column", "equivalent", "canonical") %in% names(parsed)))
          parsed else data.frame()
      }, error = function(e) data.frame())

      # Apply equivalences
      if (nrow(merge_pairs) > 0) {
        for (k in seq_len(nrow(merge_pairs))) {
          if (!isTRUE(merge_pairs$equivalent[k])) next
          canonical <- as.character(merge_pairs$canonical[k])
          if (is.na(canonical) || !nzchar(canonical)) next
          apply_idx <- conflict_idx[
            columns_df$column_name[conflict_idx] == merge_pairs$column[k]
          ]
          for (i in apply_idx) {
            label_out[i]        <- canonical
            status_out[i]       <- "labelled"
            label_method_out[i] <- "merged_llm"
          }
        }
      }
    }
  }
  ```

- [x] T007 [US1] In `data_check/2_codebook_label.R`, update the `match_column_labels()` call (around line 144) to pass the new prompt:

  ```r
  labels_df <- match_column_labels(columns_df, codebook_vars_df,
                                   column_match_prompt = COLUMN_MATCH_PROMPT,
                                   label_merge_prompt  = LABEL_MERGE_PROMPT)
  ```

- [x] T008 [US1] Update `data_check/docs/output-schemas.md` to document the two new `label_method` enum values (`merged_rules`, `merged_llm`) in the labels.csv schema table. Also note that `label_status = "labelled"` is used for merged rows.

- [ ] T009 [US1] Manual verification: run `run_codebook_label("0956797620948821")` and confirm `Age`, `Gender` (at least), `ResponseId`, `StartDate`, `EndDate` in Study 3 and Study 4 files no longer have `conflicting_definition`. Inspect the new `label_method` values in `outputs/0956797620948821/labels.csv`.

- [ ] T010 [US1] Manual verification: run `run_codebook_label("0956797620967261")` and confirm `f_emb`, `f_bor`, `f_anx`, `f_rela`, `f_fear` are `label_status = "labelled"` with `label_method` of `merged_rules` or `merged_llm`.

- [x] T011 [US1] Regression check: run `run_codebook_label("0956797617716929")` and confirm all BIS columns retain their existing correct labels (no labels changed or dropped).

**Checkpoint**: US1 complete — false conflict rate reduced, three test papers verified.

---

## Phase 4: User Story 2 — Correct Label Assigned (Priority: P2)

**Goal**: Confirm the BIS misplaced-label report is resolved (or already correct).

**Independent Test**: `run_codebook_label("0956797617716929")` → `BIS_2_Nonplanning_Impulsiveness` carries the label that matches the source codebook `traits_codebook.csv`.

### Implementation for User Story 2

- [x] T012 [US2] Investigate: open `data_check/data/0956797617716929/Facial_Width-to-Height_Ratio_Does_Not/codebook/traits_codebook.csv` and confirm the row `BIS_2_Nonplanning_Impulsiveness,Nonplanning Impulsiveness (N)`. Compare to the pipeline output `outputs/0956797617716929/labels.csv`. The labels match — **no code change required** per research.md Finding 2.

- [x] T013 [US2] Document finding in a comment or note: the BIS-11 label "(N)" in this codebook refers to the Non-planning Impulsiveness second-order factor, not to Neuroticism. The pipeline correctly reflects the source codebook. Update `specs/010-fix-label-ambiguity/research.md` if any new nuance is found during this review.

**Checkpoint**: US2 confirmed — either existing output is correct, or an additional code fix is applied.

---

## Phase 5: Polish & Cross-Cutting Concerns

- [x] T014 [P] Update `docs/pipeline.md` if the label-merging step warrants a note in the pipeline flow diagram (new sub-step after the column matching step).
- [x] T015 [P] Update `progress.md` with feature 010 entry: problem, approach, affected files.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies — start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 — **blocks US1 and US2**
- **Phase 3 (US1)**: Depends on Phase 2 — tasks T003–T007 can proceed in sequence; T009–T011 depend on T003–T007
- **Phase 4 (US2)**: Independent of US1 — can proceed in parallel after Phase 2
- **Phase 5 (Polish)**: Depends on Phase 3 and Phase 4

### Within User Story 1

```
T002 (normalize_label) → T003 (rule tier) → T005 (signature) → T006 (LLM tier)
                       ↘ T004 (prompt)   ↗
T006 + T007 (wire-up) → T009, T010, T011 (verification)
T008 (docs) [parallel with T003–T007]
```

### Parallel Opportunities

- T004 and T003 touch different sections of different files — run in parallel
- T008 (docs update) can run at any point during US1 implementation
- T012 and T013 (US2) can run in parallel with T009–T011 (US1 verification)
- T014 and T015 (polish) can run in parallel with each other

---

## Parallel Example: User Story 1

```
# Run these in parallel (different files):
T003 — helper.R rule-based tier
T004 — 2_codebook_label.R prompt constant
T008 — output-schemas.md docs update

# Then sequentially:
T005 — helper.R signature update
T006 — helper.R LLM tier (depends on T005)
T007 — 2_codebook_label.R wire-up (depends on T004 + T006)

# Then verification (depends on T007):
T009, T010, T011
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: baseline check
2. Complete Phase 2: `normalize_label()` — the shared building block
3. Complete T003 only (rule-based tier) — handles ~50% of false conflicts with zero LLM cost
4. **STOP and VALIDATE**: run T009–T011 after T003 alone to see rule-based coverage
5. If rule-based resolves all target cases: skip T004–T007 (LLM tier optional)
6. If LLM tier needed: complete T004–T007

### Incremental Delivery

- T003 alone delivers immediate value (rule-based merge) — deployable without LLM tier
- Adding T004–T007 handles the harder synonym cases — layered on top
- US2 (T012–T013) is a documentation/confirmation task only
