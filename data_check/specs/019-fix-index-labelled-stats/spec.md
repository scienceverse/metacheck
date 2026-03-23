# Feature Specification: Fix Indexing Errors for Labelled Data and Empty Column Frames

**Feature Branch**: `019-fix-index-labelled-stats`
**Created**: 2026-03-23
**Status**: Draft
**Input**: User description: "research why these errors happen in the indexing step — Can't convert from `value` <labelled<double>> to <labelled<double>> due to loss of precision. / arguments imply differing number of rows: 0, 1"

## Background

Two distinct errors cause the indexing step to fail completely for a small number of papers, recording `success = FALSE` in `bulk_summary.csv` with no column output at all.

**Error A — labelled precision**: Occurs when processing SPSS/Stata/SAS files. The `haven` package preserves "labelled" numeric types from those formats. When the pipeline computes percentile statistics (`quantile()`), the result is a plain double — not a labelled double. Downstream code that assembles per-column statistics rows then encounters a type mismatch between the labelled source and the plain computed statistic, crashing with "Can't convert from `value` <labelled<double>> to <labelled<double>> due to loss of precision." Affects papers `0956797618772822` and `09567976231158570`.

**Error B — differing row counts**: Occurs during column data frame assembly when one column's statistics result in a 0-row frame and another results in a 1-row frame. The row-bind operation fails with "arguments imply differing number of rows: 0, 1." Affects paper `0956797618773095`.

In both cases the entire paper fails — no `columns.csv` or `structure.csv` output is written — even though the underlying data is readable.

## User Scenarios & Testing

### User Story 1 — SPSS/Stata/SAS files complete indexing without error (Priority: P1)

A researcher runs the bulk indexer over a set of papers that includes SPSS or Stata data files containing value-labelled numeric columns. Previously those papers failed entirely. After this fix they complete successfully, producing `columns.csv` with column statistics.

**Why this priority**: Labelled files from statistical software are extremely common in psychology research data. Every SPSS/Stata file with numeric labels is currently at risk of this failure.

**Independent Test**: Re-run the indexing step on papers `0956797618772822` and `09567976231158570`. Verify both produce `columns.csv` with non-zero rows and `bulk_summary.csv` records `success = TRUE`.

**Acceptance Scenarios**:

1. **Given** an SPSS file with labelled numeric columns, **When** the indexer reads it and computes column statistics, **Then** statistics (including percentiles) are computed without error and written to `columns.csv`.
2. **Given** a labelled numeric column where `quantile()` returns a plain double, **When** the statistics row is assembled, **Then** the type is coerced to plain numeric before assembly so no precision error occurs.
3. **Given** a paper that previously failed with the labelled precision error, **When** re-indexed, **Then** `bulk_summary.csv` records `success = TRUE` and `error = NA`.

---

### User Story 2 — Column frame assembly tolerates 0-row edge cases (Priority: P2)

A paper whose data files produce an edge case where one column yields a 0-row statistics frame no longer crashes the entire paper's indexing run.

**Why this priority**: While less frequent than Error A, this is a silent correctness gap — a single unusual column destroys output for all other columns in the file.

**Independent Test**: Re-run the indexing step on paper `0956797618773095`. Verify it completes with `success = TRUE` and `columns.csv` contains rows.

**Acceptance Scenarios**:

1. **Given** a data file where one column produces a 0-row statistics frame, **When** column frames are assembled, **Then** the 0-row frame is dropped and remaining columns are written successfully.
2. **Given** a paper that previously failed with the differing-row-count error, **When** re-indexed, **Then** `bulk_summary.csv` records `success = TRUE`.

---

### Edge Cases

- A file where every column produces a 0-row statistics frame → `columns.csv` is written with zero data rows; paper still marked `success = TRUE`.
- A labelled column with all `NA` values → statistics produce `NA` results; no type error occurs.
- A mix of labelled and non-labelled columns in the same file → each column handled independently; no cross-column type contamination.
- An SPSS file with string value labels (not numeric) → unaffected by this fix; existing behaviour preserved.

## Requirements

### Functional Requirements

- **FR-001**: Before assembling per-column statistics into a data frame row, the pipeline MUST strip any labelled-type metadata from numeric values, converting them to plain numerics, so type mismatches do not occur during assembly.
- **FR-002**: The column data frame assembly step MUST tolerate 0-row frames in the list of per-column results, silently dropping them rather than crashing.
- **FR-003**: Both fixes MUST be applied within the existing column statistics computation path — no changes to file reading, LLM classification, or output schema are required.
- **FR-004**: Papers that previously failed with either error MUST now complete with `success = TRUE` and produce valid `columns.csv` output when re-indexed.
- **FR-005**: No currently-passing papers MUST regress — statistics values for non-labelled columns MUST be unchanged.

### Key Entities

- **Labelled numeric column**: A numeric column from SPSS/Stata/SAS that carries value-label metadata. Statistics functions return plain doubles from these, causing type mismatch on assembly.
- **Column statistics frame**: The one-row data frame assembled per column, containing computed metrics. Must be type-compatible for row-binding across all columns in a file.

## Success Criteria

### Measurable Outcomes

- **SC-001**: All three previously-failing papers (`0956797618772822`, `09567976231158570`, `0956797618773095`) complete indexing with `success = TRUE` after the fix.
- **SC-002**: Zero regression — all papers currently recorded as `success = TRUE` in `bulk_summary.csv` remain successful after re-running.
- **SC-003**: The share of SPSS/Stata/SAS files that produce a non-empty `columns.csv` increases compared to the pre-fix baseline.

## Assumptions

- Stripping the labelled type before statistics computation is safe — the numeric values themselves are unchanged, only the metadata wrapper is removed.
- 0-row column frames can be safely dropped — they represent columns where no statistics could be computed, consistent with how other failures already produce `NA` statistics.
- No schema change to `bulk_summary.csv` or `columns.csv` is needed.
- Re-indexing the three known affected papers is sufficient for validation; a full bulk re-run is not required.
