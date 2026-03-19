# Data Model: Pipeline Quality Insights Report (016)

**Date**: 2026-03-19
**Branch**: `016-pipeline-quality-report`

---

## Input Entities

This feature is read-only. All input entities map directly to existing CSV files produced by the pipeline.

---

### BulkSummaryRow

**Source**: `bulk_summary.csv` (one row per paper per run; last occurrence per `paper_id` used)

| Field | Type | Notes |
|---|---|---|
| `paper_id` | character | MUST be read as character (leading zeros) |
| `success` | logical | TRUE = pipeline completed successfully |
| `error` | character or NA | Error message for failed runs; NA on success |
| `elapsed_ms` | integer or NA | Total wall time in milliseconds |
| `download_ms` | integer or NA | Download phase time |
| `llm_ms` | integer or NA | LLM classification phase time |
| `column_ms` | integer or NA | Column extraction phase time |
| `n_files` | integer or NA | Total files in repo |
| `n_data_files` | integer or NA | Files classified as data |
| `n_columns` | integer or NA | Total columns extracted |

**Validation rules**:
- `paper_id` must never be coerced to numeric
- On duplicate `paper_id`, retain last row only
- `elapsed_ms` and phase timing columns may be NA for failed runs — treat as missing, not zero

---

### CodebookSummaryRow

**Source**: `codebook_summary.csv` (one row per paper per codebook run)

| Field | Type | Notes |
|---|---|---|
| `paper_id` | character | MUST be read as character |
| `success` | logical | TRUE = codebook labelling completed |
| `error` | character or NA | Error message; NA on success |
| `elapsed_ms` | integer or NA | Total labelling time |
| `n_labelled` | integer | Columns that received a label |
| `n_unlabelled` | integer | Columns without a label |
| `n_codebook_vars` | integer | Variables found in codebook |
| `n_matched_vars` | integer | Codebook variables matched to dataset columns |
| `label_status` | character | `"ok"` or `"no_codebook"` |

---

### ColumnRecord

**Source**: `outputs/<paper_id>/columns.csv` (one row per column per data file)

| Field | Type | Notes |
|---|---|---|
| `paper_id` | character | MUST be read as character |
| `column_name` | character | Column name as found in data file |
| `col_type` | character | One of: `continuous`, `binary`, `categorical`, `id`, `text`, `date`, `unknown` |
| `source_file` | character | Relative path to the data file |
| `filename` | character | Filename only |
| `group` | character | File group classification |

**Validation rules**:
- `col_type` must be one of the 7 known values; unexpected values should be counted separately as "other"
- Missing `columns.csv` for a paper = zero-column paper (reported separately)

---

### CodebookCoverageRecord

**Source**: `outputs/<paper_id>/codebook_coverage.csv` (one row per codebook variable)

| Field | Type | Notes |
|---|---|---|
| `paper_id` | character | MUST be read as character |
| `codebook_variable` | character | Variable name from codebook |
| `label` | character | Human-readable label text |
| `codebook_source` | character | Filename of the codebook used |
| `match_status` | character | `"matched"` or other status |

**Validation rules**:
- Coverage rate = `n rows where match_status == "matched"` / `total rows` per paper
- Missing `codebook_coverage.csv` = N/A coverage (no codebook), NOT 0%
- Empty `codebook_coverage.csv` (header only) = 0% coverage with codebook present

---

## Output Entities

The report produces no persistent output by default (console only). With `--out` argument:

### QualityReportCSV *(optional)*

**Destination**: Path provided by `--out` argument

| Field | Type | Notes |
|---|---|---|
| `report_date` | character | ISO date of report run |
| `section` | character | Which report section (e.g., `"bulk_overview"`, `"col_type_dist"`) |
| `metric` | character | Metric name |
| `value` | character | Metric value (always character to accommodate mixed types) |

---

## Derived Metrics

These are computed from the input entities and displayed in the console report.

### Bulk Overview Metrics
- `n_total` — count of unique papers in `bulk_summary.csv`
- `n_success` — count where `success == TRUE`
- `n_failed` — count where `success == FALSE`
- `success_rate` — `n_success / n_total * 100`
- `failure_type_counts` — table of error-code prefix from `error` column (e.g., `download_failed`, `no_links`), counts and percentages
- `timing_stats` — mean/median/max of `elapsed_ms`, `download_ms`, `llm_ms`, `column_ms` for successful papers only

### Column-Type Distribution Metrics
- `col_type_counts` — count and percentage of each `col_type` across all papers
- `unknown_rate_per_paper` — per-paper percentage of `unknown` col_types
- `high_unknown_papers` — papers where `unknown_rate > threshold` (default 30%)
- `zero_column_papers` — papers with no `columns.csv` or empty `columns.csv`

### Codebook Coverage Metrics
- `coverage_rate_per_paper` — per-paper `n_matched / n_total` from `codebook_coverage.csv`
- `overall_coverage_rate` — mean coverage rate across all papers with a codebook
- `no_codebook_papers` — papers where `codebook_coverage.csv` is absent
- `low_coverage_papers` — papers below mean coverage (for attention)

### Performance Metrics
- `top_n_by_elapsed` — top-N papers by `elapsed_ms`
- `top_n_by_download` — top-N papers by `download_ms`
- `top_n_by_llm` — top-N papers by `llm_ms`
- `top_n_by_column` — top-N papers by `column_ms`

---

## State Transitions / Processing Order

```
1. Load bulk_summary.csv → deduplicate by paper_id (keep last)
2. Load codebook_summary.csv → deduplicate by paper_id (keep last)
3. Scan outputs/ → collect paths to columns.csv and codebook_coverage.csv
4. Load all columns.csv files → bind into single frame
5. Load all codebook_coverage.csv files → bind into single frame
6. Compute metrics section by section
7. Print each section to console
8. (If --out provided) write QualityReportCSV
```
