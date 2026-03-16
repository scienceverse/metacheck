# Pipeline Overview

The pipeline downloads psychology research data repositories from OSF, classifies their
contents using an LLM, and extracts column-level statistics into structured CSVs.

## Entry Points

| Script | Purpose |
|---|---|
| `run_index_bulk.R` | Process all papers. Crash-resilient, auto-resumes from `bulk_summary.csv`. |
| `0_index.R` (`run_index()`) | Process a single paper by ID. Called by the bulk runner. |

---

## End-to-End Flow

```
Paper ID (character string)
      │
      ▼
┌─────────────────────┐
│  1. Resolve OSF     │  metacheck::osf_links(paper_id)
│     links           │  → list of downloadable file URLs
└──────────┬──────────┘
           │  fail → error: no_links
           ▼
┌─────────────────────┐
│  2. Download repo   │  Downloads to data_check/data/<paper_id>/
│                     │  Limit: 10 GB per paper
└──────────┬──────────┘
           │  fail → error: download_failed | too_large
           ▼
┌─────────────────────┐
│  3. Unpack          │  unpack_archive() in helper.R
│     archives        │  Handles: zip, tar, tgz, gz, bz2, xz
└──────────┬──────────┘
           │  empty after unpack → error: empty_repo (retried once)
           ▼
┌─────────────────────┐
│  4. Build file      │  Walk directory tree, collect all paths
│     tree            │  Sentinel collapse: folders with >50 files → single placeholder row
└──────────┬──────────┘
           │  >200 paths (10 LLM calls × 20 batch size) → error: too_large
           ▼
┌─────────────────────┐
│  5. LLM file        │  llm_batch() + classify_by_rules() in helper.R
│     classification  │  Assigns: type (data/codebook/code/supplemental/doc/readme/asset/other)
│                     │           group (ex1/ex2/pilot1/other/na)
└──────────┬──────────┘
           │  only files with type = "data" continue
           ▼
┌─────────────────────┐
│  6. Read data       │  read_data_head(path, n_rows = 5) in helper.R
│     heads           │  Formats: csv/tsv/txt/dat/xlsx/xls/sav/dta/sas7bdat/rds/rda/rdata
│                     │  Limit: 500 MB per file; ggplot objects → NULL (skipped)
└──────────┬──────────┘
           ▼
┌─────────────────────┐
│  7. Rule-based      │  classify_col_type_rules() in helper.R
│     column          │  Rules (in order):
│     classification  │    1. all-NA          → empty
│                     │    2. ≤2 unique        → binary
│                     │    3. ID name pattern  → LLM (is_numeric=FALSE)
│                     │    4. date-parseable   → date
│                     │    5. long strings     → text
│                     │    6a. any decimal     → continuous  ← no LLM needed
│                     │    6. integer, 3–20 u  → LLM (is_numeric=TRUE)
│                     │    6. integer, >20 u   → continuous
│                     │    7. comma-decimal    → continuous_comma_decimal / _outliers_excluded
│                     │    8. few short strs   → categorical
│                     │    9. fallback         → text
└──────────┬──────────┘
           │  ambiguous columns (NA col_type) sent to LLM
           ▼
┌─────────────────────┐
│  8. LLM column      │  llm_batch() with COLUMN_TYPE_PROMPT
│     classification  │  Classifies ambiguous columns as:
│                     │  continuous / ordinal / categorical / binary / id / unknown
│                     │  Fallback: if is_numeric=TRUE and LLM returns "unknown" → continuous
└──────────┬──────────┘
           ▼
┌─────────────────────┐
│  9. Compute stats   │  For numeric col_types: mean, sd, se, median, min, max,
│                     │  range, p25, p75, iqr, skewness, kurtosis
│                     │  Non-numeric: n and n_missing only
└──────────┬──────────┘
           ▼
┌─────────────────────┐
│  10. Write outputs  │  structure/<paper_id>_structure.csv  (one row per file)
│                     │  structure/<paper_id>_columns.csv   (one row per column)
└──────────┬──────────┘
           ▼
┌─────────────────────┐
│  11. Append to      │  bulk_summary.csv  (one row per paper, appended immediately)
│      bulk summary   │  Crash-safe: progress survives interruption
└─────────────────────┘
```

---

## Key Constants (`0_index.R`)

| Constant | Value | Purpose |
|---|---|---|
| `LLM_BATCH_SIZE` | 20 | Paths per LLM call |
| `N_DATA_READ` | 5 | Rows sampled per data file |
| `MAX_COL_TYPE_LLM_CALLS` | 5 | Max LLM calls for column classification (= 100 columns max) |
| `AGGREGATE_THRESHOLD` | 50 | Files per folder above which a sentinel row replaces individual paths |
| `MAX_DIR_WORDS` | 5 | Directory name word limit before truncation |

## Resource Limits

| Limit | Value | Error code on breach |
|---|---|---|
| Download size per paper | 10 GB | `too_large` |
| Data file size | 500 MB | file skipped silently |
| LLM calls per paper | 10 (= 200 file paths) | `too_large` |

## LLM Model

All LLM calls use `ollama/gpt-oss:20b-cloud` via `llm_batch()` in `helper.R`.
Batch size is always `LLM_BATCH_SIZE = 20`.

---

## Retry Behaviour

The bulk runner (`run_index_bulk.R`) retries once if the error is `empty_repo`
(deletes the empty downloaded folder and re-runs). All other errors are written
to `bulk_summary.csv` without retry.
