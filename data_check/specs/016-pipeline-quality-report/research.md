# Research: Pipeline Quality Insights Report (016)

**Date**: 2026-03-19
**Branch**: `016-pipeline-quality-report`

---

## Decision 1: Single script vs. modular scripts

**Decision**: Single entry-point script (`report_quality.R`) with internal functions for each report section.

**Rationale**: All existing pipeline entry points are single R scripts at the repo root (`run_single.R`, `run_index_bulk.R`, `run_codebook_bulk.R`). A single script is consistent with this pattern and avoids introducing a new script-per-section convention. Internal functions provide logical separation without filesystem fragmentation.

**Alternatives considered**:
- One script per report section — rejected; too many files for read-only reporting work
- A `reports/` subdirectory — rejected; no precedent in this codebase and adds indirection

---

## Decision 2: Output destination

**Decision**: Print to console (stdout) by default. Optionally accept a `--out` argument to write a dated summary CSV.

**Rationale**: The researcher's primary use case is inspecting results interactively after a bulk run. Console output covers this. The optional CSV output preserves results for comparison across runs without making it mandatory.

**Alternatives considered**:
- Always writing a file — rejected; adds noise for quick inspection runs
- HTML report — rejected; no browser-rendering infrastructure exists in this project; over-engineered for current need

---

## Decision 3: Where to find per-paper output files

**Decision**: Scan `outputs/` relative to the working directory (same default as `OUTPUT_DIR` constant in the pipeline). Accept an `--outputs-dir` argument to override.

**Rationale**: The pipeline writes to `./data_check/outputs/<paper_id>/` by default. The reporting script should follow the same convention so it works out-of-the-box when run from `data_check/`.

**Alternatives considered**:
- Hardcoded absolute path — rejected; breaks portability
- Reading paths from `bulk_summary.csv` — rejected; `bulk_summary.csv` does not store output paths

---

## Decision 4: Paper ID deduplication (duplicate rows in bulk_summary.csv)

**Decision**: Keep the last occurrence of each `paper_id` in `bulk_summary.csv` (most recent run wins).

**Rationale**: The bulk runner appends a new row if a paper is re-run. The last row is the authoritative result. This matches the bulk runner's own resume logic, which skips papers already in the CSV.

**Alternatives considered**:
- Keep first occurrence — rejected; first may be an older failure that was later fixed
- Aggregate all runs per paper — rejected; over-complicates the report for a rare edge case

---

## Decision 5: Unknown-rate outlier threshold

**Decision**: Default threshold = 30% of columns classified as `unknown`. Configurable via `--unknown-threshold` argument (0–100 integer, representing percentage).

**Rationale**: 30% is a reasonable starting point — more than 1-in-3 unknown columns suggests a classification problem. The researcher will tune this over time as they inspect results; making it configurable avoids a code change for each experiment.

**Alternatives considered**:
- Fixed threshold — rejected; the right threshold is unknown without ground truth; configurability is essential
- Percentile-based threshold (flag top-N%) — rejected; adds complexity and requires knowing the distribution first

---

## Decision 6: Pairwise agreement metric for column types (codebook_coverage.csv)

**Decision**: For codebook coverage, use `match_status` field from `codebook_coverage.csv`. Count rows where `match_status == "matched"` as labelled; all others (unmatched, unlabelled) as unlabelled. Coverage rate = `n_matched / (n_matched + n_unmatched)`.

**Rationale**: The `codebook_coverage.csv` already stores `match_status` per variable. This is the authoritative source for coverage — no re-computation is needed.

**Alternatives considered**:
- Using `n_labelled` / `n_unlabelled` from `codebook_summary.csv` — valid shortcut for per-paper totals but does not allow per-column inspection
- Using `labels.csv` — also valid but `codebook_coverage.csv` is more directly scoped to coverage

---

## Decision 7: Top-N performance lists

**Decision**: Default N = 10. Configurable via `--top-n` argument. Rank by `elapsed_ms` for overall, and by each phase column separately. Papers with `NA` timing (failed before that phase) are excluded from that phase's ranking with a note.

**Rationale**: The researcher wants to find outliers, not a complete ranked list. 10 is a conventional default for "top" lists. NA-exclusion is cleaner than treating NA as 0 (which would artificially rank failed papers as fastest).

**Alternatives considered**:
- Always show all papers ranked — rejected; with 252 papers, this is not useful as a console output

---

## Decision 8: Console table formatting

**Decision**: Use `format()` and `cat()` with `sprintf()` for aligned tabular console output. No external packages.

**Rationale**: Base R provides sufficient string formatting for fixed-width console tables. No new packages needed — consistent with Principle IV and the constitution's "base R only" standard.

**Alternatives considered**:
- `knitr::kable()` — rejected; requires knitr package not currently in use
- `prettyNum()` — useful for large numbers; already available in base R

---

## Resolved unknowns

All NEEDS CLARIFICATION items from spec resolved above. No open questions remain.

---

## Observed data schemas (from live CSV inspection)

### `bulk_summary.csv`
```
paper_id, success, error, elapsed_ms, download_ms, llm_ms, column_ms,
n_files, n_data_files, n_agg_dirs, n_raw, n_nonraw, n_columns, n_src_files
```
- 252 rows (as of 2026-03-19)
- `success` is logical (TRUE/FALSE)
- `error` contains free-text for failures; NA for successes
- Timing columns are NA when the phase was not reached

### `codebook_summary.csv`
```
paper_id, success, error, elapsed_ms, n_labelled, n_unlabelled,
n_codebook_vars, n_matched_vars, label_status
```
- 114 rows (as of 2026-03-19)
- `label_status` values observed: `"ok"`, `"no_codebook"`

### `outputs/<paper_id>/columns.csv`
```
paper_id, source_file, filename, group, col_header_group, column_name,
sample_values, col_type, n_coerced, n, n_missing, mean, sd, se, median,
min, max, range, p25, p75, iqr, skewness, kurtosis
```
- `col_type` observed values: `unknown`, `binary`, `continuous`, `categorical`, `id`, `text`, `date`

### `outputs/<paper_id>/codebook_coverage.csv`
```
paper_id, codebook_variable, label, codebook_source, group, match_status
```
- `match_status` observed values: `"matched"`
