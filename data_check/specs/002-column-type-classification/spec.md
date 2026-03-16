# Feature Specification: Column Type Classification and Value Normalization

**Feature Branch**: `002-column-type-classification`
**Created**: 2026-03-16
**Status**: Draft
**Input**: User description: "When writing to columns.csv (line 491 in 0_index.R) it is always assumed the variable is continuous. Sometimes, this is not the case, such as id0, id2, etc. Or when they are genders (Male, Female). Sometimes it might still be good to determine some averages. Sometimes values might be continuous with some mistakes (A point instead of a comma, a single non continuous variable). Dates also have this problem. Devise a way to 1: relabel variables that have certain mistakes so summary statistics are still possible. Consider changing the columns thing to have another column that indicates whether something is binary, categorical, date, comment or anything you deem useful. You can use LLMs to classify this. Consider using the sample_values column or using a different way to sample from each data file (it might be further down where alternative things come, using unique or something might be applicable). Sample some *_columns.csv files in structure/ if you deem it needed"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Type Labels in Output CSV (Priority: P1)

A researcher inspecting `*_columns.csv` output wants to know immediately whether a column contains continuous measurements, binary flags, categorical labels, date/time values, an identifier, or free text — without needing to inspect the raw data files themselves.

**Why this priority**: Every downstream consumer of the columns CSV (analysis scripts, dashboards, manual review) benefits from a type label. It is also the prerequisite for meaningful statistics in Story 2.

**Independent Test**: Run `run_index()` on a paper that contains a mix of numeric, binary flag, gender-coded, date, and ID columns. Inspect the output `*_columns.csv` and verify the `col_type` column contains appropriate labels for each column.

**Acceptance Scenarios**:

1. **Given** a data file with a column named "Gender" containing values `1 | 2 | 1 | 2 | 1`, **When** `run_index()` completes, **Then** the matching row in `*_columns.csv` has `col_type = "binary"` or `"categorical"`.
2. **Given** a data file with a column named "userid" containing sequential integers, **When** `run_index()` completes, **Then** the matching row has `col_type = "id"`.
3. **Given** a data file with a column named "date" containing values like `2016-06-13 | 2016-03-26`, **When** `run_index()` completes, **Then** the matching row has `col_type = "date"`.
4. **Given** a data file with a column containing long free-text sentences, **When** `run_index()` completes, **Then** the matching row has `col_type = "text"`.
5. **Given** a standard continuous numeric column (e.g., reaction time in milliseconds), **When** `run_index()` completes, **Then** the matching row has `col_type = "continuous"`.
6. **Given** a Likert-scale column with values like `"Strongly agree | Neutral | Disagree"`, **When** `run_index()` completes, **Then** the matching row has `col_type = "ordinal"` or `"categorical"`.

---

### User Story 2 - Statistics for Recoverable Malformed Numeric Columns (Priority: P2)

A researcher notices that some numeric columns read as character because the source data file uses a locale-specific decimal separator (comma instead of period) or contains a small number of stray non-numeric entries. They want summary statistics computed for those columns after the malformed values are normalized or excluded.

**Why this priority**: Without normalization, an entire column's statistics are silently absent (all NA). This is a data quality problem that directly affects the usefulness of the pipeline output. The fix is well-scoped.

**Independent Test**: Use paper `0956797620903716` which contains `Final_Data_PDR1.csv` — its "Response time" column has comma-decimal values (`2,195 | 9,198 | 11,007 | 5,566`) that currently produce NA stats. Run `run_index()` and verify that this column now has numeric statistics populated and a `col_type` indicating that normalization occurred.

**Acceptance Scenarios**:

1. **Given** a column where all values match a comma-as-decimal-separator pattern (e.g., `1,234` representing `1.234`), **When** `run_index()` completes, **Then** summary statistics are computed after substitution and `col_type = "continuous_comma_decimal"`.
2. **Given** a column that is predominantly numeric but contains 1–5% non-numeric outlier strings, **When** `run_index()` completes, **Then** statistics are computed on the numeric portion, `n_missing` includes coerced-to-NA outliers, and `col_type = "continuous_outliers_excluded"`.
3. **Given** a column where more than 20% of values remain non-numeric after normalization attempts, **When** `run_index()` completes, **Then** the column is classified as `categorical` or `text` and no numeric statistics are emitted.

---

### User Story 3 - LLM-Assisted Classification for Ambiguous Columns (Priority: P3)

For columns that rule-based heuristics cannot confidently classify (e.g., numeric codes that might be IDs or Likert scores), the system sends sample values to the local LLM for a classification decision.

**Why this priority**: Rule-based classification handles clear-cut cases but will mis-classify edge cases. LLM assistance provides a higher-quality fallback without requiring manual intervention. Lower priority because rules cover the majority of cases.

**Independent Test**: Identify a paper dataset containing columns where rule-based heuristics are ambiguous (e.g., an integer column with values 1–5 that could be Likert or count). Verify that the LLM is invoked for these columns and the resulting `col_type` label is sensible.

**Acceptance Scenarios**:

1. **Given** a column with integer values in a narrow range (1–7) and an ambiguous name, **When** the LLM is queried with column name and up to 10 unique sample values, **Then** the returned `col_type` is one of the controlled vocabulary labels.
2. **Given** the LLM returns an unrecognized or empty classification, **When** `run_index()` processes the result, **Then** `col_type` falls back to `"unknown"` rather than erroring.
3. **Given** LLM classification is disabled or unavailable, **When** `run_index()` completes, **Then** ambiguous columns receive `col_type = "unknown"` and processing continues without failure.

---

### Edge Cases

- What happens when a column has all identical values (zero variance)? → Classifiable normally; stats computed; `col_type` reflects type.
- What happens when `sample_values` is empty (all-NA column)? → `col_type = "empty"`; no stats computed.
- What happens when a binary column uses text labels (`"Male"` / `"Female"`) rather than `0/1`? → Rule-based detection via unique value count and text patterns; classified as `"binary"`.
- What happens when a date column contains mixed formats? → If majority of values parse as dates, classify as `"date"`; otherwise fall back to `"text"`.
- What happens if comma-decimal substitution produces implausible values? → Normalization applied; `col_type` records it; downstream user can audit.
- What happens for very wide datasets (> 500 columns)? → Classification runs per-column without blocking; performance degradation is acceptable but must not cause timeout errors.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The `*_columns.csv` output MUST contain a new column `col_type` for every row.
- **FR-002**: `col_type` MUST be drawn from a controlled vocabulary: `continuous`, `binary`, `categorical`, `ordinal`, `date`, `id`, `text`, `continuous_comma_decimal`, `continuous_outliers_excluded`, `empty`, `unknown`.
- **FR-003**: Rule-based classification MUST run before any LLM call (fast path first).
- **FR-004**: Rule-based classification MUST cover without LLM involvement:
  - All-NA columns → `empty`
  - Exactly 2 unique non-NA values → `binary`
  - Column name matches ID patterns (`id`, `_id`, `ID`, `subj`, `participant`) and values are integers → `id`
  - Values parse as standard date/datetime formats (ISO 8601, common locale formats) → `date`
  - Median string length of values > 40 characters → `text`
  - Column is numeric in R and has > 20 unique values → `continuous` (subject to outlier check)
- **FR-005**: Malformed numeric detection MUST check whether a character column becomes fully numeric after replacing `,` with `.`. If yes, apply substitution, compute statistics, and set `col_type = "continuous_comma_decimal"`.
- **FR-006**: When a numeric column has ≤ 5% non-parseable values, those MUST be coerced to NA, statistics computed on the remainder, and `col_type = "continuous_outliers_excluded"`.
- **FR-006b**: For `continuous_comma_decimal` and `continuous_outliers_excluded` columns, the output MUST include an `n_coerced` field recording the count of values that were coerced to NA during normalization. This count MUST be NA for all other column types.
- **FR-007**: Columns that rule-based logic marks as ambiguous MUST be sent to the LLM in batches, using up to 10 unique sample values per column (prefer `unique()` over head-of-file values).
- **FR-008**: LLM calls for type classification MUST be batched consistently with the existing `llm_batch()` architecture.
- **FR-009**: The LLM prompt MUST request a single controlled-vocabulary label per column with no explanation.
- **FR-010**: Existing summary statistics columns MUST remain in the output and be populated for `continuous`, `continuous_comma_decimal`, and `continuous_outliers_excluded` columns.
- **FR-011**: For `binary`, `categorical`, `ordinal`, `id`, `date`, and `text` columns, `n` and `n_missing` MUST be reported; all numeric stat fields (mean, sd, etc.) MUST be NA.
- **FR-012**: Classification failures MUST degrade gracefully to `col_type = "unknown"`; they MUST NOT cause `run_index()` to return `success = FALSE`.
- **FR-013**: The `col_type` column MUST be inserted into the output CSV without removing or reordering any existing columns (additive schema change).

### Key Entities

- **Column Record**: A row in `*_columns.csv`, now extended with `col_type`; key attributes are `column_name`, `sample_values`, `col_type`, and the numeric statistics fields.
- **Controlled Vocabulary**: The fixed set of valid `col_type` labels; defines the contract between pipeline output and downstream consumers.
- **Normalization Event**: A transformation applied to a column's values (e.g., comma→period substitution, outlier exclusion) before statistics are computed; surfaced via `col_type` for auditability.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Every row in every `*_columns.csv` produced by `run_index()` has a non-empty `col_type` value from the controlled vocabulary.
- **SC-002**: For paper `0956797620903716`, the "Response time" column (previously all-NA stats due to comma decimals) has numeric statistics and `col_type = "continuous_comma_decimal"`.
- **SC-003**: Among a representative sample of 10 papers, fewer than 5% of column rows have `col_type = "unknown"`.
- **SC-004**: Columns that are clearly identifiers (e.g., `userid` in tweet datasets) receive `col_type = "id"` rather than misleading continuous statistics.
- **SC-005**: `run_index()` returns `success = TRUE` for all papers that previously succeeded, confirming no regressions.
- **SC-006**: The additional processing time attributable to type classification does not exceed 10 seconds for papers with ≤ 200 columns.

## Assumptions

- The existing `llm_batch()` function and local LLM are available and can be reused for type-classification prompts.
- Comma-as-decimal is the primary numeric normalization case; thousands separators and other locale variants are out of scope initially.
- Backward compatibility with existing `*_columns.csv` files is not required; only newly generated files need the `col_type` column.
- A column is "ambiguous" for LLM classification when it is numeric with 3–20 unique values and its name does not clearly indicate a scale or identifier.
- The LLM type-classification batch shares the same per-paper column limits as the existing file classification, scoped to columns rather than file paths.
