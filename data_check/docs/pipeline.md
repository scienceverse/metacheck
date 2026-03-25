# Pipeline Overview

The pipeline downloads psychology research data repositories from OSF, classifies their
contents using an LLM, and extracts column-level statistics into structured CSVs.

## Entry Points

| Script | Purpose |
|---|---|
| `run_single.R` | Run the full pipeline (index + codebook label) for one randomly selected paper. Dev/smoke-test entry point. |
| `run_index_bulk.R` | Process all papers through the index stage. Crash-resilient, auto-resumes from `bulk_summary.csv`. |
| `run_codebook_bulk.R` | Run codebook-label stage across all papers with `columns.csv`. Auto-resumes from `codebook_summary.csv`. |
| `0_index.R` (`run_index()`) | Process a single paper by ID. Called by the index bulk runner. |
| `2_codebook_label.R` (`run_codebook_label()`) | Label columns against codebooks for a single paper. Called by the codebook bulk runner. |
| `run_sweep.R` | Temperature stability sweep for a single paper. Runs full pipeline at N temperatures × R repeats; crash-resilient via per-paper `sweep_log.csv`. |
| `run_sweep_bulk.R` | Bulk temperature sweep across all papers in `XML_DIR`. Paper-level resume via `sweep_results/sweep_bulk_log.csv`; calls `run_paper_sweep()`. |
| `report_sweep.R` | Single-paper sweep report: stability (pairwise col_type + label agreement), quality proxies, weighted recommendation. Writes `sweep_report_YYYY-MM-DD.md`. |
| `report_sweep_grand.R` | Grand cross-paper report: flat CSV with one row per (paper_id × temperature × pipeline stage). No aggregation — post-processing friendly. |
| `runners/run_psychds_single.R` | Convert one paper to PsychDS format. Dev/smoke-test entry point. Accepts `paper_id` as CLI arg or pre-set variable; falls back to random paper from `results/bulk_summary.csv`. |
| `runners/run_psychds_bulk.R` | Batch-convert all successfully indexed papers to PsychDS format. Crash-resilient, auto-resumes from `psychds/conversion_summary.csv`. |
| `pipeline/3_psychds_convert.R` (`convert_psychds()`) | Convert a single paper by ID to PsychDS format. Returns list of per-study result rows. |

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
│                     │  Exception: if ALL files fall into aggregate folders (no non-aggregate
│                     │  paths remain), sentinel is cancelled and all files are processed
│                     │  individually — the 10-call limit still guards runaway repos
└──────────┬──────────┘
           │  >200 paths (10 LLM calls × 20 batch size) → error: too_large
           ▼
┌─────────────────────┐
│  5. LLM file        │  llm_batch() in helper.R
│     classification  │  Assigns: type (data/codebook/code/supplemental/doc/readme/asset/other)
│                     │           group (ex1/ex2/pilot1/other/na)
│                     │  Post-expansion override: after aggregate sentinels are expanded back
│                     │  to individual files, AGGREGATE_EXT_OVERRIDE (0_index.R) corrects
│                     │  inherited types for unambiguous extensions (.R→code, .jpg→asset, etc.)
└──────────┬──────────┘
           │  only files with type = "data" continue
           ▼
┌─────────────────────┐
│  6. Read data       │  read_data_head(path, n_rows = 5) in helper.R
│     heads           │  Formats: csv/tsv/txt/dat/xlsx/xls/sav/dta/sas7bdat/rds/rda/rdata
│                     │  Limit: 500 MB per file; ggplot objects → NULL (skipped)
│                     │  Encoding: csv/tsv/txt/dat read with default encoding; if any
│                     │  character column contains invalid UTF-8 bytes, file is re-read
│                     │  with fileEncoding="latin1" (handles Windows-1252 encoded files)
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
│  10. Write outputs  │  outputs/<paper_id>/structure.csv  (one row per file)
│                     │  outputs/<paper_id>/columns.csv   (one row per column)
└──────────┬──────────┘
           ▼
┌─────────────────────┐
│  11. Append to      │  bulk_summary.csv  (one row per paper, appended immediately)
│      bulk summary   │  Crash-safe: progress survives interruption
└──────────┬──────────┘
           │  (optional post-processing step)
           ▼
┌─────────────────────┐
│  12. Codebook       │  2_codebook_label.R  run_codebook_label(paper_id)
│      labelling      │  Reads: outputs/<paper_id>/structure.csv (codebook/readme files)
│                     │         outputs/<paper_id>/columns.csv   (data columns to label)
│                     │  Writes: outputs/<paper_id>/labels.csv        (one row per data column)
│                     │          outputs/<paper_id>/codebook_coverage.csv (one row per codebook var)
│                     │  Codebook formats: csv/tsv/xlsx/xls/sav/dta (rule-based)
│                     │                    docx (officer), pdf (pdftools), rtf (regex strip)
│                     │                    doc (textutil, macOS system binary — no install)
│                     │                    odt (unzip content.xml + XML tag strip, base R)
│                     │                    plain text (LLM chunking)
│                     │  Conflict resolution: multi-label columns resolved by rule-based
│                     │  normalisation first (normalize_label()), then LLM batch if still
│                     │  conflicting (LABEL_MERGE_PROMPT, 1 call/paper) — sets
│                     │  label_method = "merged_rules" or "merged_llm"
│                     │  Dependencies: officer (≥0.7.0), pdftools (≥3.0.0) — already installed
└──────────┬──────────┘
           │  (optional post-processing step)
           ▼
┌─────────────────────┐
│  13. PsychDS        │  pipeline/3_psychds_convert.R  convert_psychds(paper_id)
│      conversion     │  Reads: outputs/<paper_id>/structure.csv
│                     │         outputs/<paper_id>/columns.csv
│                     │         outputs/<paper_id>/labels.csv
│                     │         outputs/<paper_id>/codebook_coverage.csv
│                     │         ground_truth/<paper_id>.csv (optional)
│                     │         data_check/data/<paper_id>/grobid/*.xml (optional)
│                     │  Writes: psychds/<paper_id>/dataset_description.json
│                     │          psychds/<paper_id>/data/<name>_data.csv
│                     │          psychds/<paper_id>/data/<name>_data.json (sidecar)
│                     │          psychds/<paper_id>/data/raw/ (oversized/raw files)
│                     │          psychds/<paper_id>/materials/, documentation/, code/
│                     │          psychds/<paper_id>/documentation/txt/ (plaintext copies)
│                     │          psychds/<paper_id>/provenance.json
│                     │  Multi-study layout: psychds/<paper_id>/study-<group>/
│                     │  Oversized files (>500 MB): raw copy only, no CSV conversion
│                     │  Sentinel expansion: aggregate placeholder rows replaced with
│                     │  individual file records before conversion
│                     │  Ground truth: ground_truth/<paper_id>.csv overrides type/group/is_raw
│                     │  Paper metadata: populated from GROBID TEI XML if present (xml2)
│                     │  Plaintext extraction: doc/codebook files with .pdf/.docx/.rtf
│                     │  extension produce a .txt copy in documentation/txt/ via
│                     │  extract_plain_text() in helper.R (pdftools/officer/RTF strip);
│                     │  image-only PDFs and errors are flagged in provenance.json only
└─────────────────────┘
```

---

## Key Constants (`0_index.R`)

| Constant | Value | Script | Purpose |
|---|---|---|---|
| `OUTPUT_DIR` | `./data_check/outputs` | `0_index.R`, `2_codebook_label.R` | Root directory for per-paper output subdirectories |
| `LLM_BATCH_SIZE` | 20 | `0_index.R`, `2_codebook_label.R` | Paths per LLM call |
| `N_DATA_READ` | 5 | `0_index.R` | Rows sampled per data file |
| `MAX_COL_TYPE_LLM_CALLS` | 5 | `0_index.R` | Max LLM calls for column classification (= 100 columns max) |
| `AGGREGATE_THRESHOLD` | 50 | `0_index.R` | Files per folder above which a sentinel row replaces individual paths |
| `AGGREGATE_EXT_OVERRIDE` | named vector | `0_index.R` | Extension → type map applied after sentinel expansion to correct inherited types |
| `MAX_DIR_WORDS` | 5 | `0_index.R` | Directory name word limit before truncation |
| `MAX_CODEBOOK_LLM_CALLS` | 3 | `2_codebook_label.R` | Max LLM calls per paper for codebook text parsing |
| `MAX_CODEBOOK_FILE_MB` | 100 | `2_codebook_label.R` | Codebook files larger than this (MB) are skipped |
| `MULTILEVEL_HEADER_LOOKAHEAD` | 3L | `0_index.R` | Max rows to scan below row 1 for a usable sub-header row in multi-level CSV files |
| `PSYCHDS_OUT_DIR` | `./data_check/psychds` | `3_psychds_convert.R` | Root directory for PsychDS output directories |
| `DATA_SIZE_LIMIT_MB` | 500 | `3_psychds_convert.R` | Max data file size (MB) for CSV conversion; oversized files are raw-copied only |

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
