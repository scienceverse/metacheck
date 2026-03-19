# Feature Specification: Pipeline Quality Insights Report

**Feature Branch**: `016-pipeline-quality-report`
**Created**: 2026-03-19
**Status**: Draft
**Input**: User description: "Analyze the bulk_summary.csv and codebook summary CSVs produced by the pipeline to surface insightful quality metrics: error rates, coverage gaps, column-type distributions, and codebook match rates. The researcher needs a reporting layer that reads these output CSVs and produces summary tables that make it easy to see where the pipeline is performing well and where it is failing, without requiring a ground-truth dataset."

## Clarifications

### Session 2026-03-19

- Q: What format should the output report be saved in, and should it be mandatory or optional? → A: Always write a dated Markdown (.md) report automatically; no `--out` argument needed; filename format `quality_report_YYYY-MM-DD.md` saved to the working directory.
- Q: How should papers with no codebook file be distinguished from papers with a codebook that matched nothing? → A: Absent `codebook_coverage.csv` = N/A (no codebook); file present with zero matched rows = 0% coverage.

---

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Bulk Run Quality Overview (Priority: P1)

The researcher has just completed a bulk run and wants to understand how the pipeline performed across all papers. They run the quality report script against `bulk_summary.csv` and receive a clear summary: how many papers succeeded, how many failed and why, the distribution of failure types, and which papers are most likely worth investigating.

**Why this priority**: This is the most immediate need — bulk runs are the primary workflow and the researcher currently has no way to inspect overall pipeline health after a run.

**Independent Test**: Can be fully tested by pointing the script at an existing `bulk_summary.csv` and confirming it prints a coherent summary table with counts and rates.

**Acceptance Scenarios**:

1. **Given** a `bulk_summary.csv` with at least 10 rows, **When** the report script is run, **Then** it prints total paper count, success count, and a breakdown of failure types with counts and percentages.
2. **Given** a `bulk_summary.csv` where all papers succeeded, **When** the report script is run, **Then** it reports 0 failures and shows aggregate timing stats (download, LLM, column phases).
3. **Given** a `bulk_summary.csv` with missing or malformed rows, **When** the report script is run, **Then** it skips invalid rows, notes the count of skipped rows, and continues.

---

### User Story 2 - Column-Type Distribution Analysis (Priority: P2)

The researcher wants to know whether the pipeline's column-type classification is producing sensible distributions. They run the column distribution report against the collected `columns.csv` outputs and see what proportion of columns are classified as each type (`id`, `numeric`, `categorical`, `text`, `date`, `binary`, `unknown`), and whether any paper has an unusually high rate of `unknown` classifications.

**Why this priority**: `unknown` column types indicate the LLM is uncertain or the rules are insufficient. Papers with high `unknown` rates are the fastest signal of quality problems.

**Independent Test**: Can be tested independently by running against any set of `outputs/<paper_id>/columns.csv` files and verifying the distribution table sums to 100%.

**Acceptance Scenarios**:

1. **Given** a set of `columns.csv` files across multiple papers, **When** the distribution report is run, **Then** it outputs a table showing count and percentage of each `col_type` across all papers.
2. **Given** a paper with >30% `unknown` column types, **When** the report is run, **Then** that paper is flagged as a high-unknown-rate outlier.
3. **Given** a paper with zero columns classified, **When** the report is run, **Then** it is listed separately as a zero-column paper.

---

### User Story 3 - Codebook Coverage Summary (Priority: P3)

The researcher wants to understand how well the codebook labelling step is covering the dataset columns. They run the codebook coverage report against the `codebook_coverage.csv` outputs and see: what fraction of columns received a codebook label, how many were left unlabelled, and which papers had the worst coverage.

**Why this priority**: Codebook coverage is a proxy for label quality. Low coverage means most columns have no human-readable label, undermining downstream interpretation.

**Independent Test**: Can be tested independently by running against any set of `outputs/<paper_id>/codebook_coverage.csv` files and verifying coverage rates are computed correctly.

**Acceptance Scenarios**:

1. **Given** a set of `codebook_coverage.csv` files, **When** the coverage report is run, **Then** it shows per-paper coverage rate (labelled / total columns) sorted from lowest to highest.
2. **Given** a paper where no `codebook_coverage.csv` file exists, **When** the report is run, **Then** that paper is shown with coverage = N/A (not 0%), labelled "no_codebook".
3. **Given** a paper whose `codebook_coverage.csv` exists but contains zero matched rows, **When** the report is run, **Then** that paper is shown with coverage = 0% (distinct from N/A).
3. **Given** all papers having >80% coverage, **When** the report is run, **Then** it reports overall average coverage and notes no outliers.

---

### User Story 4 - Timing and Performance Summary (Priority: P4)

The researcher wants to identify slow papers — ones that consumed disproportionate time in download, LLM calls, or column extraction — to understand where bottlenecks arise.

**Why this priority**: Performance outliers often indicate unusual repo sizes or LLM failures; identifying them helps prioritise optimisation or investigation.

**Independent Test**: Can be tested by running against `bulk_summary.csv` and confirming the top-10 slowest papers by each time dimension are listed correctly.

**Acceptance Scenarios**:

1. **Given** a `bulk_summary.csv` with timing columns, **When** the performance report is run, **Then** it shows the top-10 slowest papers by total elapsed time, and by each phase (download, LLM, column).
2. **Given** papers with missing timing values (e.g. failed before column step), **When** the report is run, **Then** missing values are noted and papers are ranked on available data only.

---

### Edge Cases

- What happens when `bulk_summary.csv` is empty (header only)? → Report prints "No data to summarise" and exits cleanly.
- What happens when `outputs/` contains no `columns.csv` files? → Column distribution report prints "No column data found" and exits cleanly.
- What happens when a `paper_id` has leading zeros? → Must be treated as a character string throughout to avoid ID corruption.
- What happens when timing columns contain NA or blank? → Treat as missing; do not crash; report count of missing-timing papers.
- What happens when the same paper appears more than once in `bulk_summary.csv` (e.g. re-run after failure)? → Use the most recent row per paper (by row order).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The quality report MUST read `bulk_summary.csv` and produce a success/failure summary with counts and percentages per failure type.
- **FR-002**: The report MUST compute and display aggregate timing statistics (mean, median, max) for download, LLM, and column extraction phases across successful papers.
- **FR-003**: The report MUST scan `outputs/<paper_id>/columns.csv` files and produce a column-type distribution table (count and percentage per `col_type`).
- **FR-004**: The report MUST flag papers with an `unknown` column-type rate above a configurable threshold (default: 30%) as outliers.
- **FR-005**: The report MUST read `outputs/<paper_id>/codebook_coverage.csv` files and produce per-paper and aggregate codebook coverage rates. Papers with no `codebook_coverage.csv` MUST be shown as N/A (not 0%); papers with the file present but zero matched rows MUST be shown as 0%.
- **FR-006**: The report MUST list the top-N papers by each timing dimension (default N = 10) as a performance summary.
- **FR-007**: All report sections MUST be runnable independently — the researcher can run just the bulk summary without `columns.csv` files present.
- **FR-008**: The report MUST treat `paper_id` as a character string at all times to preserve leading zeros.
- **FR-009**: The report MUST handle missing, empty, or malformed input files gracefully, printing a descriptive message and continuing rather than crashing.
- **FR-010**: Report output MUST be written to the console in human-readable tabular format AND MUST always be saved as a Markdown (.md) file named `quality_report_YYYY-MM-DD.md` (using the current date) in the working directory. No `--out` argument is required; the file is written automatically on every run.
- **FR-011**: When a paper appears more than once in `bulk_summary.csv`, the report MUST use the most recent row (last occurrence) for that paper.

### Key Entities

- **BulkSummary**: One row per paper processed by the bulk runner; contains `paper_id`, `status`, timing columns, and error codes.
- **ColumnRecord**: One row per column in a dataset file; contains `paper_id`, `col_name`, `col_type`, and file path.
- **CodebookCoverage**: One row per column; indicates whether the column received a codebook label and the coverage fraction per paper.
- **QualityReport**: The aggregated output — a set of human-readable summary tables produced by the reporting script.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A researcher can run the quality report on a completed bulk run of up to 500 papers and receive a full printed summary within 60 seconds.
- **SC-002**: All failure types present in `bulk_summary.csv` appear in the failure-type breakdown — no failure type is silently dropped.
- **SC-003**: Column-type distribution percentages across all papers sum to 100% (±0.1% rounding tolerance).
- **SC-004**: Every paper exceeding the unknown-rate threshold appears in the outlier list; no paper below the threshold appears.
- **SC-005**: Codebook coverage rates are accurate — manual spot-checks on 5 random papers confirm the reported rate matches the actual fraction of labelled columns.
- **SC-006**: The report completes without crashing when any individual input file is missing, empty, or malformed.
- **SC-007**: Every run produces a `quality_report_YYYY-MM-DD.md` file in the working directory containing all report sections that ran successfully.

## Assumptions

- `bulk_summary.csv` follows the schema documented in `docs/output-schemas.md`.
- Per-paper `columns.csv` and `codebook_coverage.csv` files exist under `outputs/<paper_id>/` as produced by the existing pipeline.
- No new R packages are required; base R plus packages already present (`jsonlite`, `haven`, `readxl`) are sufficient.
- Output is for the researcher's own use (CLI-first); no web UI or interactive dashboard is required in this feature.
- The Markdown output file is always generated automatically; there is no `--out` flag for suppressing or redirecting it.
- The configurable outlier threshold (default 30%) and top-N count (default 10) can be passed as script arguments.
