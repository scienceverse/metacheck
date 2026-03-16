# Implementation Plan: Reduce Over-Conservative `unknown` Column Type Classifications

**Branch**: `004-reduce-unknown-coltypes` | **Date**: 2026-03-16 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/004-reduce-unknown-coltypes/spec.md`

## Summary

Numeric columns with only 5 sampled rows rarely exceed 20 unique values, so virtually all
numeric columns are routed to the LLM. The LLM non-deterministically returns `unknown` for many
that are clearly `continuous`. Fix via three targeted changes: (1) a new Rule 6a that
immediately classifies decimal-valued numeric columns as `continuous` without LLM routing;
(2) an `is_numeric` flag tracked through the pipeline so post-LLM fallback can replace
`unknown` → `continuous` for integer numeric columns the LLM couldn't decide on; (3) an
improved LLM prompt that deprioritises `unknown` for numeric data.

## Technical Context

**Language/Version**: R (base R, no new packages)
**Primary Dependencies**: `helper.R` (`classify_col_type_rules()`), `0_index.R` (`COLUMN_TYPE_PROMPT`, `run_index()`)
**Storage**: `structure/<paper_id>_columns.csv` (output schema unchanged)
**Testing**: Manual re-run on paper `0956797615583071`; compare before/after `unknown` counts
**Target Platform**: macOS/Linux (wherever R is installed)
**Project Type**: Data extraction pipeline
**Performance Goals**: Reduce LLM column classification calls (fewer columns routed to LLM)
**Constraints**: Must not change output CSV schema; must not affect binary/categorical/date/text/id/empty classifications
**Scale/Scope**: 2 files changed (`helper.R`, `0_index.R`); ~30 lines added/modified

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|---|---|---|
| I. Crash Resilience | ✅ Pass | No change to incremental write pattern |
| II. Paper ID Preservation | ✅ Pass | No paper ID handling touched |
| III. Resource Limits | ✅ Pass | Fewer LLM calls (Rule 6a bypasses LLM) — conservative direction |
| IV. Centralised Helpers | ✅ Pass | `classify_col_type_rules()` change goes in `helper.R` as required |
| V. Structured Error Classification | ✅ Pass | No new paper-level error codes introduced |

**All gates pass. No violations to justify.**

## Project Structure

### Documentation (this feature)

```text
specs/004-reduce-unknown-coltypes/
├── plan.md          ← this file
├── research.md      ← Phase 0 (complete)
├── data-model.md    ← Phase 1 (complete)
└── tasks.md         ← Phase 2 (/speckit.tasks — not yet created)
```

### Source Code (files changed)

```text
data_check/
├── helper.R         ← classify_col_type_rules(): add Rule 6a + is_numeric field
└── 0_index.R        ← COLUMN_TYPE_PROMPT: improve; run_index(): extract is_numeric,
                        add post-LLM fallback, drop is_numeric in cleanup
```

## Implementation Steps

### Step 1 — `helper.R`: Add Rule 6a (decimal detection) and `is_numeric` field

**Location**: `classify_col_type_rules()`, between Rule 5 and Rule 6 (~line 195)

**Change A — Rule 6a** (insert before existing Rule 6):
```r
  # Rule 6a: decimal numeric — any fractional value is unambiguously continuous.
  # Fires before Rule 6 to avoid unnecessary LLM routing for VAS / ratio scales.
  if (is.numeric(values) && any(x_noNA != floor(x_noNA)))
    return(list(col_type = "continuous", ambiguous = FALSE, numeric_values = values,
                n_coerced = NA_integer_, is_numeric = FALSE))
```

**Change B — Rule 6** (modify existing return for integer ambiguous case):
```r
  # Rule 6: integer numeric column — 3–20 unique values, route to LLM.
  # is_numeric = TRUE flags this for post-LLM fallback if LLM returns "unknown".
  if (is.numeric(values)) {
    if (n_unique > 20)
      return(list(col_type = "continuous", ambiguous = FALSE, numeric_values = values,
                  n_coerced = NA_integer_, is_numeric = FALSE))
    return(list(col_type = NA_character_, ambiguous = TRUE, numeric_values = values,
                n_coerced = NA_integer_, is_numeric = TRUE))
  }
```

**Change C — all other returns**: Add `is_numeric = FALSE` to every existing `return(list(...))` in `classify_col_type_rules()` (Rules 1, 2, 3, 4, 5, 7, 8, 9). Rule 3's LLM-routed return also gets `is_numeric = FALSE` (ID columns must not fall back to `continuous`).

---

### Step 2 — `0_index.R`: Improve `COLUMN_TYPE_PROMPT`

**Location**: `COLUMN_TYPE_PROMPT` constant (~line 91)

**Replace with**:
```r
COLUMN_TYPE_PROMPT <- 'You are classifying columns in psychology research data.
For each column descriptor return a JSON array (same order).
Each element: {"descriptor": "<exact descriptor>", "col_type": "<type>"}

col_type — pick one:
  continuous  : numeric measurement (reaction time, score, age, VAS rating 0-10,
                Likert-scale mean, count, percentage, subscale score, any decimal value)
  ordinal     : ordered integer scale with few levels (1-5 Likert, 1-10 rating,
                bounded integer scale like attention or compliance scores)
  categorical : unordered group or category code with few levels (condition, gender,
                language, group label)
  binary      : exactly two possible values (yes/no, 0/1, treatment/control)
  id          : row or participant identifier — unique or nearly-unique integer per row
  unknown     : ONLY use when the column name AND sample values together give absolutely
                no signal — e.g., entirely redacted, all-constant meaningless codes.
                Do NOT use "unknown" for numeric-looking columns.

IMPORTANT: Prefer "continuous" or "ordinal" over "unknown". When in doubt between
"continuous" and "ordinal" for a numeric column, choose "continuous".

Output ONLY the JSON array. No notes, no text outside the array.'
```

---

### Step 3 — `0_index.R`: Extract `is_numeric` and include in `columns_df`

**Location**: inside `extract_column_info()`, after `ambiguous_idx` extraction (~line 436)

**Add** (after existing `ambiguous_idx <- ...` line):
```r
    is_numeric_vec <- vapply(col_classifications, function(cls) {
      isTRUE(cls$is_numeric)
    }, logical(1))
```

**Modify** the `data.frame(...)` in the `list(columns = ...)` return (~line 506):
Add `is_numeric = is_numeric_vec` as a column alongside `n_coerced`.

---

### Step 4 — `0_index.R`: Post-LLM numeric fallback

**Location**: after `columns_df$col_type[ambig_rows] <- returned_types` (~line 557)

**Add** (immediately after the assignment):
```r
    # Fallback: LLM "unknown" for a confirmed-numeric column → "continuous".
    # is_numeric is TRUE only for Rule 6 (integer numeric, 3-20 unique); not for ID cols.
    if ("is_numeric" %in% names(columns_df)) {
      num_unknown <- ambig_rows[
        columns_df$is_numeric[ambig_rows] & columns_df$col_type[ambig_rows] == "unknown"
      ]
      if (length(num_unknown) > 0) {
        columns_df$col_type[num_unknown] <- "continuous"
        message("── col_type fallback: ", length(num_unknown),
                " numeric column(s) reclassified from unknown → continuous")
      }
    }
```

---

### Step 5 — `0_index.R`: Drop `is_numeric` in cleanup

**Location**: Final cleanup block (~line 569), alongside `sample_values_unique <- NULL`

**Add**:
```r
    columns_df$is_numeric <- NULL
```

---

## Verification

After implementation, re-run on paper `0956797615583071` and verify:
1. `pre_film_VAS_*` columns → all `continuous` (Rule 6a fires, no LLM)
2. `IES_R_Intrusion_subscale` → `continuous` (Rule 6a fires)
3. `Attention_paid_to_film`, `Post_film_Distress`, `Diary_Compliance` → `ordinal` or `continuous` (LLM or fallback)
4. `BDI_II`, `Tetris_Demand_Rating` → `ordinal` or `continuous`
5. Total `unknown` count ≤ 10% of columns
6. `binary` columns (`Time_of_Day`, `Condition` in ex1) unchanged
7. `is_numeric` column absent from output CSV
