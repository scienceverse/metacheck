<!--
SYNC IMPACT REPORT
==================
Version change: [template] → 1.0.0
Modified principles: N/A (initial ratification — all principles are new)
Added sections:
  - Core Principles (I–V)
  - Technical Standards
  - Pipeline Workflow
  - Governance
Removed sections: None (template placeholders replaced)
Templates requiring updates:
  - .specify/templates/plan-template.md  ✅ Constitution Check section present; gates align
  - .specify/templates/spec-template.md  ✅ No principle-driven mandatory sections require changes
  - .specify/templates/tasks-template.md ✅ Task phases compatible with pipeline workflow
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
- Maximum LLM calls per paper: **10** (i.e., 200 paths at batch size 20)

Exceeding any limit MUST result in a structured error code written to the output CSV
(e.g., `too_large`), not a crash or silent omission.

**Rationale**: Unbounded resource consumption blocks the bulk pipeline and can exhaust disk or
API quota. Structured error codes allow downstream analysis of failure modes.

### IV. Centralised Shared Helpers

Logic used by more than one pipeline stage MUST live in `helper.R`. The following capabilities
MUST NOT be duplicated across pipeline scripts:

- File reading: `read_data_head()`
- Archive unpacking: `unpack_archive()`
- LLM classification: `llm_batch()`
- Rule-based classification: `classify_by_rules()`

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
- **LLM backend**: `ollama/gpt-oss:20b-cloud` via `llm_batch()` — batch size 20 (constant
  `LLM_BATCH_SIZE`)
- **Supported file formats for column extraction**: csv, tsv, txt, dat, xlsx, xls, sav, dta,
  sas7bdat, rds, rda, rdata
- **Standalone compressed files** (`.gz`, `.bz2`, `.xz` without tar) MUST be opened with
  `gzfile()`/`bzfile()`/`xzfile()` respectively — NOT `untar()`
- **Timing**: `run_index()` returns `elapsed_sec`, `download_sec`, `llm_sec`, `column_sec`;
  bulk CSV stores these as `*_ms` (milliseconds)
- **ggplot / plot objects**: `read_data_head()` MUST return NULL for saved plot objects — this
  is correct behaviour; LLM MUST classify them as `supplemental`

## Pipeline Workflow

The canonical processing order for a single paper is:

1. Resolve OSF links → fail with `no_links` if none found
2. Download files → fail with `download_failed` or `too_large` on limit breach
3. Unpack archives via `unpack_archive()`
4. Classify file paths via `llm_batch()` + `classify_by_rules()` → fail with `too_large` if
   LLM call budget exceeded
5. Read data heads via `read_data_head()` for files classified as `data`
6. Extract column statistics
7. Write `<paper_id>_structure.csv` and `<paper_id>_columns.csv` to `structure/`
8. Append result row to `bulk_summary.csv`

Any deviation from this order MUST be documented in the relevant feature spec with justification.

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

**Version**: 1.0.0 | **Ratified**: TODO(RATIFICATION_DATE): set when first committed to main | **Last Amended**: 2026-03-16
