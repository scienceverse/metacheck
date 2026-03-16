# Feature Specification: Reduce Over-Conservative `unknown` Column Type Classifications

**Feature Branch**: `004-reduce-unknown-coltypes`
**Created**: 2026-03-16
**Status**: Draft
**Input**: User description: "001 currently, probably due to the small batch size, many columns get defined as unknown, even when they have no oddities. For example: data_check/structure/0956797615583071_columns.csv has some undefined columns where their counterparts are defined. The unknown is too conservative right now. Maybe sample some unknown rows and see what a pattern could be"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Decimal numeric columns auto-classified as continuous (Priority: P1)

A researcher runs the pipeline on a psychology dataset containing Visual Analogue Scale (VAS) columns (e.g. `pre_film_VAS_Sad` with values like `0.8 | 0.3 | 1.1`). Currently these are returned as `unknown` because only 5 rows are sampled and fewer than 20 unique values are seen. After this fix, decimal-valued numeric columns are classified as `continuous` by the rule-based classifier without LLM involvement.

**Why this priority**: This is the most common failure mode. Decimal-valued numeric columns are unambiguously continuous and should never require LLM classification.

**Independent Test**: Run the pipeline on paper `0956797615583071`. Confirm that `pre_film_VAS_Sad`, `pre_film_VAS_Hopelessness`, `pre_film_VAS_Depressed`, `pre_film_VAS_Fear`, `pre_film_VAS_Horror` are all classified as `continuous`, not `unknown`.

**Acceptance Scenarios**:

1. **Given** a numeric column with any non-integer (decimal) values and ≥ 3 unique non-NA values, **When** the rule-based classifier runs, **Then** the column is classified as `continuous` without routing to the LLM.
2. **Given** a VAS column with 5 sampled rows, **When** the pipeline completes, **Then** `col_type = "continuous"` and numeric stats are populated.

---

### User Story 2 - Integer rating scale columns no longer classified as unknown (Priority: P2)

Columns like `Attention_paid_to_film` (`10 | 6 | 9 | 10 | 10`), `Post_film_Distress` (`9 | 7 | 8 | 3 | 7`), and `Diary_Compliance` (`9 | 9 | 8 | 10 | 8`) are clearly bounded integer rating scales. Currently returned as `unknown`. After the fix, these are classified as `ordinal` or `continuous`.

**Why this priority**: Integer-valued columns with a clear bounded range are obviously meaningful scales but the LLM returns `unknown` inconsistently.

**Independent Test**: Run pipeline on paper `0956797615583071` and confirm `Attention_paid_to_film`, `Post_film_Distress`, `Diary_Compliance` are no longer `unknown`.

**Acceptance Scenarios**:

1. **Given** an integer-valued numeric column with 3–20 unique values routed to LLM, **When** the LLM responds, **Then** the response is `ordinal` or `continuous`, not `unknown`.
2. **Given** an LLM response of `unknown` for a column the rule-based classifier knows is numeric, **When** the secondary fallback runs, **Then** the column is reclassified as `continuous`.

---

### User Story 3 - Consistent classification for equivalent columns across experiments (Priority: P3)

The same measurement (e.g. `BDI_II`, `IES_R_Intrusion_subscale`) appears in multiple experiment files within the same paper. Currently one file may get `continuous` and another `unknown` for identical column types due to LLM non-determinism.

**Why this priority**: Inconsistency is a correctness issue, but resolving P1 and P2 will resolve most cases.

**Independent Test**: Run paper `0956797615583071` and confirm matching column names across `ex1` and `ex2` groups have the same `col_type`.

**Acceptance Scenarios**:

1. **Given** `BDI_II` appears in both experiment 1 and experiment 2 files, **When** the pipeline runs, **Then** both rows have the same `col_type`.
2. **Given** `pre_film_VAS_Sad` appears in both experiments, **When** the pipeline runs, **Then** both are `continuous`.

---

### Edge Cases

- A decimal column with exactly 2 unique values (e.g. `0.0 | 1.0`) — should remain `binary`, not `continuous`.
- A column where all 5 sampled values are identical — `n_unique = 1`, remains `binary` (current behavior, no change needed).
- Integer columns with negative values (e.g. `Tetris_Demand_Rating`: `4 | 0 | -2 | -2 | 0`) — clearly numeric, should not be `unknown`.
- LLM batch call fails entirely — fallback must not worsen `unknown` rate beyond current baseline.
- A genuinely unclassifiable column (mixed types, redacted values) — `unknown` remains the correct output.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The rule-based classifier MUST classify numeric columns where any value is non-integer (has a fractional part) and there are ≥ 3 unique non-NA values as `continuous`, bypassing LLM routing.
- **FR-002**: The rule-based classifier MUST classify integer-valued numeric columns with ≥ 3 unique values as `continuous` (or route to LLM for ordinal/continuous distinction), but MUST NOT produce `unknown` purely from the rules.
- **FR-003**: The LLM prompt for column classification MUST explicitly instruct the model to avoid `unknown` unless sample values are genuinely uninformative, and MUST give examples of columns that look ambiguous but are actually `continuous` or `ordinal`.
- **FR-004**: When the LLM returns `unknown` for a column that the rule-based classifier confirmed is numeric, the pipeline MUST apply a secondary fallback classifying the column as `continuous`.
- **FR-005**: Existing `binary`, `categorical`, `date`, `text`, `id`, and `empty` rule-based classifications MUST NOT be changed by this feature.

### Key Entities

- **Column descriptor**: The (column name, sample values) pair sent to the LLM for classification.
- **Rule-based classifier**: Deterministic logic in `classify_col_type_rules()` that runs before the LLM.
- **LLM column classifier**: Non-deterministic call used only for columns the rules cannot resolve.
- **`col_type`**: Final label on a column — one of `continuous`, `ordinal`, `binary`, `categorical`, `date`, `id`, `text`, `empty`, `unknown`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For paper `0956797615583071`, the fraction of columns classified as `unknown` drops from ~55% to ≤ 10%.
- **SC-002**: No purely numeric column (integer or decimal) with ≥ 3 unique non-NA values is classified as `unknown` after the full pipeline completes.
- **SC-003**: Matching column names across experiment groups within the same paper have the same `col_type` in ≥ 95% of cases.
- **SC-004**: The number of columns routed to the LLM decreases compared to the current baseline, reducing per-paper LLM processing time.
- **SC-005**: No regression: columns previously classified as `binary`, `categorical`, `date`, `text`, `id`, or `empty` retain the same classification on re-run.

## Assumptions

- Only 5 rows are sampled per file (`N_DATA_READ = 5`), so `n_unique` rarely exceeds 5. Rule improvements must not depend on large unique-value counts.
- The LLM (`ollama/gpt-oss:20b-cloud`) is non-deterministic across batches; rule-based improvements are preferred over LLM-prompt-only fixes.
- Truly unclassifiable columns should still produce `unknown` — the goal is not zero `unknown`, but eliminating false unknowns on clearly numeric data.
- Changes scope: `classify_col_type_rules()` in `helper.R` and `COLUMN_TYPE_PROMPT` in `0_index.R`.
