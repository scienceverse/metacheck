<!--
SYNC IMPACT REPORT
==================
Version change: 1.0.1 → 1.1.0 (MINOR — pipeline workflow materially expanded with column
classification, stats computation, and codebook labelling stages; new shared helper
`classify_col_type_rules()` added to Principle IV; new entry points and constants documented)

Modified principles:
  - Principle IV (Centralised Shared Helpers): added `classify_col_type_rules()`

Added sections:
  - Entry Points sub-section in Pipeline Workflow
  - Key Constants table in Technical Standards
  - Steps 4, 7–12 in Pipeline Workflow (file tree, column classification, stats, codebook labelling)

Removed sections: None

Templates requiring updates:
  - .specify/templates/plan-template.md  ✅ No change required (generic)
  - .specify/templates/spec-template.md  ✅ No change required (generic)
  - .specify/templates/tasks-template.md ✅ No change required (generic)

Follow-up TODOs:
  - TODO(RATIFICATION_DATE): Original adoption date unknown — mark when first committed to main.
-->

# Metacheck Datacheck Constitution

## Core Principles

### I. Crash Resilience

The pipeline MUST write results incrementally to durable storage (CSV files) after processing each
paper. In-memory-only accumulation is prohibited. Every bulk run MUST be resumable from the last
successfully written row without re-processing completed papers.

**Rationale**: OSF downloads and LLM calls are slow and failure-prone. A crash that discards hours
of work is unacceptable; the bulk runner's append-after-each-paper pattern is the canonical model.

### II. Data Integrity — Paper ID Preservation

Paper IDs MUST be stored and read as character strings throughout the entire pipeline. Any
`read.csv()` call that loads paper IDs MUST use `colClasses = c(paper_id = "character")`.
Numeric coercion that silently strips leading zeros is a critical correctness bug.

**Rationale**: Psychology paper IDs such as `0956797615569001` carry semantically significant
leading zeros. Silent coercion produces IDs that cannot be matched back to OSF records.

### III. Conservative Resource Limits

The following hard limits MUST be enforced and MUST NOT be bypassed without explicit user approval:

- Maximum download size per paper: **10 GB**
- Maximum data file size for column extraction: **500 MB**
- Maximum LLM calls per paper (file classification): **10** (i.e., 200 paths at batch size 20)
- Maximum LLM calls per paper (column classification): **5** (i.e., 100 columns at batch size 20)
- Maximum LLM calls per paper (codebook text parsing): **3**
- Maximum codebook file size: **100 MB** (larger files are skipped silently)

Exceeding a download or path-count limit MUST result in a structured error code written to the
output CSV (e.g., `too_large`), not a crash or silent omission.

**Rationale**: Unbounded resource consumption blocks the bulk pipeline and can exhaust disk or
API quota. Structured error codes allow downstream analysis of failure modes.

### IV. Centralised Shared Helpers

Logic used by more than one pipeline stage MUST live in `helper.R`. The following capabilities
MUST NOT be duplicated across pipeline scripts:

- File reading: `read_data_head()`
- Archive unpacking: `unpack_archive()`
- LLM classification: `llm_batch()`
- Rule-based file classification: `classify_by_rules()`
- Rule-based column classification: `classify_col_type_rules()`

New shared utilities MUST be added to `helper.R` and sourced from there.

**Rationale**: Duplication of download/unpack/LLM logic across scripts caused divergence bugs
(e.g., retry behaviour for empty folders was missing in the index script before alignment).

### V. Structured Error Classification

All pipeline failures MUST be classified into one of the following error codes and persisted:

- `no_links` — OSF repo has no downloadable files
- `download_failed` — Network or OSF API error during download
- `empty_repo` — Downloaded repo contains no usable files after unpacking
- `too_large` — Exceeded a resource limit (see Principle III)

Any new failure mode MUST be assigned a code before merging. Unclassified failures written as
free-text strings are prohibited.

**Rationale**: Consistent codes allow bulk summary analysis and targeted retry logic without
parsing free-form error messages.

## Technical Standards

- **Language**: R (no other languages in the pipeline core)
- **LLM backend**: `ollama/gpt-oss:20b-cloud` via `llm_batch()` — batch size 20 (`LLM_BATCH_SIZE`)
- **Supported file formats for column extraction**: csv, tsv, txt, dat, xlsx, xls, sav, dta,
  sas7bdat, rds, rda, rdata
- **Standalone compressed files** (`.gz`, `.bz2`, `.xz` without tar) MUST be opened with
  `gzfile()`/`bzfile()`/`xzfile()` respectively — NOT `untar()`
- **Timing**: `run_index()` returns `elapsed_sec`, `download_sec`, `llm_sec`, `column_sec`;
  bulk CSV stores these as `*_ms` (milliseconds)
- **ggplot / plot objects**: `read_data_head()` MUST return NULL for saved plot objects — this
  is correct behaviour; LLM MUST classify them as `supplemental`

### Key Constants

| Constant | Value | Script | Purpose |
|---|---|---|---|
| `OUTPUT_DIR` | `./data_check/outputs` | `0_index.R`, `2_codebook_label.R` | Root for per-paper output subdirectories |
| `LLM_BATCH_SIZE` | 20 | `0_index.R`, `2_codebook_label.R` | Paths/columns per LLM call |
| `N_DATA_READ` | 5 | `0_index.R` | Rows sampled per data file |
| `MAX_COL_TYPE_LLM_CALLS` | 5 | `0_index.R` | Max LLM calls for column classification |
| `AGGREGATE_THRESHOLD` | 50 | `0_index.R` | Files per folder above which a sentinel row replaces individual paths |
| `MAX_DIR_WORDS` | 5 | `0_index.R` | Directory name word limit before truncation |
| `MAX_CODEBOOK_LLM_CALLS` | 3 | `2_codebook_label.R` | Max LLM calls per paper for codebook text parsing |
| `MAX_CODEBOOK_FILE_MB` | 100 | `2_codebook_label.R` | Codebook files larger than this (MB) are skipped |

## Pipeline Workflow

### Entry Points

| Script | Purpose |
|---|---|
| `run_single.R` | Full pipeline (index + codebook label) for one random paper. Dev/smoke-test entry point. |
| `run_index_bulk.R` | Index stage across all papers. Crash-resilient, auto-resumes from `bulk_summary.csv`. |
| `run_codebook_bulk.R` | Codebook-label stage across all papers with `columns.csv`. Auto-resumes from `codebook_summary.csv`. |
| `0_index.R` (`run_index()`) | Process a single paper by ID. Called by the index bulk runner. |
| `2_codebook_label.R` (`run_codebook_label()`) | Label columns against codebooks for a single paper. |

### Processing Order

The canonical processing order for a single paper is:

1. Resolve OSF links → fail with `no_links` if none found
2. Download files → fail with `download_failed` or `too_large` on limit breach
3. Unpack archives via `unpack_archive()`
4. Build file tree — folders with >50 files (`AGGREGATE_THRESHOLD`) collapsed to a sentinel row
   → fail with `too_large` if >200 total paths
5. Classify file paths via `llm_batch()` + `classify_by_rules()` → assigns `type` and `group`
6. Read data heads via `read_data_head()` for files classified as `data`
7. Rule-based column classification via `classify_col_type_rules()` — assigns `col_type` where
   deterministic rules apply; ambiguous columns left for LLM
8. LLM column classification via `llm_batch()` — resolves ambiguous `col_type` values;
   numeric fallback: `unknown` → `continuous`
9. Compute column statistics (numeric: mean/sd/se/median/min/max/range/p25/p75/iqr/skewness/
   kurtosis; non-numeric: n/n_missing only)
10. Write `structure.csv` and `columns.csv` to `outputs/<paper_id>/`
11. Append result row to `bulk_summary.csv` (crash-safe)
12. *(Optional post-processing)* Codebook labelling via `run_codebook_label()` — reads structure
    and columns CSVs; writes `labels.csv` and `codebook_coverage.csv` to `outputs/<paper_id>/`

Any deviation from this order MUST be documented in the relevant feature spec with justification.

### Retry Behaviour

The index bulk runner (`run_index_bulk.R`) retries once if the error is `empty_repo` (deletes
the empty downloaded folder and re-runs). All other errors are written to `bulk_summary.csv`
without retry.

## Governance

This constitution supersedes all other development conventions for this project. Amendments
require:

1. A version bump according to semantic versioning (MAJOR / MINOR / PATCH as defined in the
   versioning policy below).
2. An updated Sync Impact Report prepended as an HTML comment.
3. Review of all affected templates and pipeline scripts for consistency.

**Versioning policy**:
- MAJOR — principle removed, redefined, or limits changed in a backward-incompatible way
- MINOR — new principle or section added, or material guidance expanded
- PATCH — wording clarification, typo fix, non-semantic refinement

All new features MUST be validated against Principles I–V before merging to `main`.

**Version**: 1.1.0 | **Ratified**: TODO(RATIFICATION_DATE): set when first committed to main | **Last Amended**: 2026-03-17
