# Feature Specification: Fix Multi-Level CSV Header Skip Rule

**Feature Branch**: `014-fix-multilevel-csv-headers`
**Created**: 2026-03-18
**Status**: Draft

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Valid multi-level CSV data is processed instead of skipped (Priority: P1)

A researcher has exported data from Excel into CSV format. The resulting file has 2–3 header rows — a top row of group/condition labels (some cells blank, producing `...N` placeholder names), followed by one or more rows of actual column labels, then numeric participant data. Currently the pipeline skips this file entirely. After the fix, the pipeline uses the sub-header row as column names, captures the row-1 condition labels in a `col_header_group` field, and extracts column statistics normally.

**Why this priority**: This is the core defect. 4 of 5 known affected files are valid participant datasets being silently discarded. All data extraction value is lost for these files.

**Independent Test**: Run `run_index()` on paper `09567976221147259` (contains `Experiment 3_Summary_MTT_Foglio1.csv`, `Fitting_Foglio1.csv`, `MTTDATA_Raw Data.csv`). Before the fix all three produce "skipping" messages and contribute 0 columns. After the fix all three produce column records in `outputs/<paper_id>/columns.csv` with meaningful `column_name` values.

**Acceptance Scenarios**:

1. **Given** a CSV file whose first row is ≥50% `...N` placeholder names and whose second row contains real column labels, **When** the pipeline reads the file, **Then** it uses row 2 values as `column_name`, captures row-1 group labels in `col_header_group`, reads data from row 3 onward, and extracts column statistics without skipping.
2. **Given** a CSV file with 3 header rows where row 1 is ≥50% `...N`, **When** the pipeline reads the file, **Then** it uses the first qualifying sub-header row as `column_name` and records row-1 group labels in `col_header_group`.
3. **Given** a CSV file that truly has no header (first row is all blank or numeric), **When** the pipeline reads the file, **Then** it still skips the file, preserving existing behaviour.

---

### User Story 2 - Genuinely headerless CSVs continue to be skipped (Priority: P2)

The original intent of the rule — skipping files that have no meaningful column labels — must be preserved. A file where the first row is all-empty or all-numeric, and no subsequent row resolves to a usable header, should still be skipped.

**Why this priority**: A false negative (processing a genuinely headerless file) produces garbage column statistics. The fix must not loosen the check to the point where headerless files slip through.

**Independent Test**: Provide a CSV whose first 3 rows are all numeric values with no text labels. Confirm the pipeline still skips it with an appropriate log message.

**Acceptance Scenarios**:

1. **Given** a CSV file whose first row contains only numeric values and whose next 3 rows also contain no text column labels, **When** the pipeline reads the file, **Then** it skips the file and logs a message.
2. **Given** a CSV file whose candidate sub-header rows all consist exclusively of `NA` or empty strings, **When** the pipeline reads the file, **Then** it skips the file.

---

### User Story 3 - Log message distinguishes multi-level detection from true headerless skip (Priority: P3)

When the pipeline cannot resolve a multi-level header (detects `...N`-heavy row 1 but finds no usable sub-header row within the lookahead limit), the log message should indicate that multi-level header detection was attempted but failed — distinct from the current generic message.

**Why this priority**: Observability improvement only; does not affect data extraction. Useful for diagnosing edge cases during bulk runs.

**Independent Test**: Trigger a skip on a file where row 1 is `...N`-heavy but rows 2–3 are all `NA`. Confirm the log message differs from the standard skip message used for unreadable or empty files.

**Acceptance Scenarios**:

1. **Given** a CSV with a `...N`-heavy first row and no usable sub-header within 3 rows, **When** the pipeline skips it, **Then** the log message references multi-level header detection rather than only "no proper header row".

---

### Edge Cases

- What if the sub-header row is also partially `...N` but below the 50% threshold? It should be accepted as the column names.
- What if every candidate row within the lookahead contains at least one `...N`? Use the first row where the `...N` fraction is lower than row 1's AND at least one real non-empty, non-NA label exists; otherwise skip.
- What if the file has only one data row below the resolved header? Process it normally — single-row datasets are valid.
- What if row 2 is all `NA` but row 3 has real labels (3-level header)? The lookahead must continue to row 3 before giving up.
- What about non-CSV formats (xlsx, sav, dta)? The same `...N` pattern can appear in any format read through `read_data_head`; the fix applies uniformly.
- What about NA cells in the sub-header row? Fall back to the original `...N` name from row 1 for that column; `col_header_group` is still populated from the row-1 span for that position.
- What about Branch 2 files (partial labels, no sub-header)? `col_header_group` is `NA` for all columns — the group context is already embedded in the partial names (e.g., `Passato...3`).

## Clarifications

### Session 2026-03-18

- Q: When a sub-header is resolved, which column names are written to `columns.csv`? → A: The resolved sub-header name becomes `column_name` (enabling direct codebook matching with no changes to `match_column_labels()`). The row-1 condition/group label is captured in a new `col_header_group` field (enabling future wide→long pivot by grouping columns within the same condition span). The original `...N` placeholder is not persisted — it carries no information useful to codebook matching or future restructuring.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: When a file is read and ≥50% of column names match the `...N` placeholder pattern, the pipeline MUST attempt to find a usable sub-header row before deciding to skip.
- **FR-002**: A usable sub-header row is defined as any row within the first `MULTILEVEL_HEADER_LOOKAHEAD` rows of the data where the `...N` fraction is lower than row 1's fraction AND at least one value is a non-empty, non-NA string.
- **FR-003**: `MULTILEVEL_HEADER_LOOKAHEAD` MUST be defined as a named constant with a default value of 3.
- **FR-004**: When a usable sub-header row is found, the pipeline MUST use its values as `column_name` for each column. Cells in the sub-header that are NA or empty fall back to the original `...N` name from row 1.
- **FR-005**: When a usable sub-header row is found, the pipeline MUST populate `col_header_group` for each column by forward-filling the non-`...N` prefixes from row-1 names across their spans (e.g., `SHAM...3` sets `col_header_group = "SHAM"` for all columns until the next named group label).
- **FR-006**: When a usable sub-header row is found and used, the pipeline MUST NOT emit the existing "skipping" message; it MAY emit a diagnostic message indicating multi-level header resolution.
- **FR-007**: When no usable sub-header row is found within `MULTILEVEL_HEADER_LOOKAHEAD` rows, the pipeline MUST check if the current header has ≥1 non-`...N` column name. If yes, proceed as-is with `col_header_group = NA` for all columns. If no, skip the file.
- **FR-008**: When skipping due to failed multi-level header resolution, the log message MUST be distinct from the unreadable/empty skip message.
- **FR-009**: All existing skip conditions (unreadable, empty, timed-out, over size limit) MUST remain unaffected by this change.
- **FR-010**: `columns.csv` MUST include a `col_header_group` column. For all files where no multi-level header resolution occurred, this column is `NA`.
- **FR-011**: `column_name` values written to `columns.csv` for resolved files MUST be usable directly by `match_column_labels()` in `helper.R` without any changes to that function.

### Key Entities

- **Multi-level header file**: A tabular file where the first row contains condition/group span labels with many empty/blank cells, and a subsequent row contains the actual raw variable names as used in the codebook.
- **Sub-header row**: The first row beneath row 1 (within the lookahead limit) where the `...N` fraction is lower than row 1's and at least one real variable label exists.
- **Lookahead limit** (`MULTILEVEL_HEADER_LOOKAHEAD`): Maximum number of rows below row 1 scanned for a sub-header row; default 3.
- **`col_header_group`**: The condition/group label from row 1 that applies to a given column, derived by forward-filling non-`...N` prefixes across the span they cover. `NA` when no multi-level structure was detected. Stored for future wide→long pivot support; not used by any current pipeline stage.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 4 previously-skipped valid files across papers `09567976221147259` and `09567976231151581` produce at least 1 column record each in `outputs/<paper_id>/columns.csv` after the fix.
- **SC-002**: Zero regressions: every file that was correctly skipped before the fix continues to be skipped after it.
- **SC-003**: No new "skipping" messages appear for any file that was successfully processed before the fix.
- **SC-004**: The fix introduces no new named constants beyond `MULTILEVEL_HEADER_LOOKAHEAD` and requires no changes to `helper.R` or `2_codebook_label.R`.
- **SC-005**: `bulk_summary.csv` column counts for re-indexed affected papers increase relative to their pre-fix values.
- **SC-006**: For `MTTDATA_Raw Data.csv`, `col_header_group` values in `columns.csv` correctly reflect the condition spans: `SHAM`, `BETA`, `ALPHA` across their respective column ranges.

## Assumptions

- The `...N` placeholder pattern in CSV files is baked into the file text at export time (from Excel or a similar tool).
- Sub-header resolution is best-effort: the pipeline uses the first qualifying sub-header row as-is and does not attempt to construct compound column names.
- Codebooks describe raw variable names (e.g., `PAST_Mortgage`), not condition-qualified names (e.g., `SHAM.PAST_Mortgage`). Using the sub-header row directly as `column_name` therefore enables codebook matching without any changes to `match_column_labels()`.
- `col_header_group` is purely for future use — no current pipeline stage reads or acts on it. A future pivot step would group columns by `col_header_group` to separate condition-level data.
- A lookahead of 3 rows covers all known real-world cases (max observed: 3 header rows).
- The fix touches two locations: `0_index.R` (header resolution logic + `col_header_group` population) and `output-schemas.md` (new column documentation). `helper.R`, `2_codebook_label.R`, and `labels.csv` are unaffected.
