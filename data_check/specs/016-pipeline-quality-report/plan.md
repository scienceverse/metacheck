# Implementation Plan: Pipeline Quality Insights Report

**Branch**: `016-pipeline-quality-report` | **Date**: 2026-03-19 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/016-pipeline-quality-report/spec.md`

## Summary

A read-only reporting script (`report_quality.R`) that reads the existing pipeline output CSVs (`bulk_summary.csv`, `codebook_summary.csv`, `outputs/<paper_id>/columns.csv`, `outputs/<paper_id>/codebook_coverage.csv`) and prints four console-formatted report sections: bulk run overview, column-type distribution, codebook coverage, and performance timing. No new packages. No pipeline changes. No ground truth required.

## Technical Context

**Language/Version**: R (base R only — no new packages)
**Primary Dependencies**: `haven`, `readxl`, `jsonlite` — all already present; not needed for this feature (read-only CSV reporting)
**Storage**: CSV files on local filesystem — `bulk_summary.csv`, `codebook_summary.csv`, `outputs/<paper_id>/columns.csv`, `outputs/<paper_id>/codebook_coverage.csv`
**Testing**: Manual spot-check against known values in existing CSVs; no automated test framework
**Target Platform**: macOS/Linux (where R is installed); run from `data_check/` directory
**Project Type**: CLI script (single R file, `Rscript report_quality.R [options]`)
**Performance Goals**: Full report on 500 papers in under 60 seconds (reading ~500 CSVs is fast; no LLM calls)
**Constraints**: Base R only; no persistent state written; paper IDs always character strings
**Scale/Scope**: ~250–500 papers in current bulk runs; up to a few thousand columns.csv files total

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|---|---|---|
| I. Crash Resilience | ✅ N/A | Read-only script; no incremental writes needed |
| II. Paper ID Preservation | ✅ Required | All `read.csv()` calls MUST use `colClasses = c(paper_id = "character")` |
| III. Resource Limits | ✅ N/A | No downloads, no LLM calls |
| IV. Centralised Shared Helpers | ✅ Compliant | No new shared helpers needed; all logic is specific to reporting and stays in `report_quality.R` |
| V. Structured Error Classification | ✅ N/A | Script reads existing error codes; does not produce new ones |

**Gate result**: PASS. No violations.

## Project Structure

### Documentation (this feature)

```text
specs/016-pipeline-quality-report/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/
│   └── cli.md           # CLI argument contract and console output format
└── tasks.md             # Phase 2 output (/speckit.tasks — not yet created)
```

### Source Code (repository root)

```text
data_check/
├── report_quality.R          # NEW — main report script
├── bulk_summary.csv           # Existing input
├── codebook_summary.csv       # Existing input
└── outputs/
    └── <paper_id>/
        ├── columns.csv        # Existing input (per-paper)
        └── codebook_coverage.csv  # Existing input (per-paper)
```

**Structure Decision**: Single new file at repo root (`data_check/report_quality.R`), consistent with all other entry-point scripts (`run_single.R`, `run_index_bulk.R`, `run_codebook_bulk.R`). No new directories needed.

## Implementation Phases

### Phase A: Argument parsing and file loading

Implement `parse_args()` to handle CLI arguments with defaults (see `contracts/cli.md`). Implement `load_bulk()`, `load_codebook_summary()`, `load_all_columns()`, `load_all_coverage()` — each reads the relevant CSV(s), enforces `paper_id` as character, deduplicates (last-row-wins per paper), and returns a data frame. Missing files emit `[WARN]` to stderr and return `NULL`; the script continues.

**Key detail**: `load_all_columns()` and `load_all_coverage()` use `list.files(outputs_dir, pattern = "columns\\.csv$", recursive = TRUE)` to discover per-paper files. Each is read with `colClasses = c(paper_id = "character")` and bound with `do.call(rbind, ...)`.

---

### Phase B: Bulk overview section

Compute from `bulk_summary`:
- Total unique papers, success count, failure count, success rate
- Failure-type table: extract error-code prefix from `error` column using `sub("^([a-z_]+):.*", "\\1", error)`, count occurrences
- Timing stats (mean/median/max) for `elapsed_ms`, `download_ms`, `llm_ms`, `column_ms` on successful papers only (`success == TRUE` and `!is.na(timing_col)`)

Print formatted table with `cat()` and `sprintf()`.

---

### Phase C: Column-type distribution section

Compute from all `columns.csv`:
- Global count and percentage per `col_type` (7 known values + "other" catch-all)
- Per-paper unknown-rate; flag papers above `--unknown-threshold`
- Zero-column papers (papers in `bulk_summary` with `success == TRUE` but no `columns.csv` or empty file)

Print formatted table. List high-unknown papers and zero-column papers.

---

### Phase D: Codebook coverage section

Compute from all `codebook_coverage.csv` and `codebook_summary`:
- Per-paper coverage rate: `n rows where match_status == "matched" / total rows` per paper from `codebook_coverage.csv`
- Papers without `codebook_coverage.csv` = "no codebook" (mark N/A, not 0%)
- Papers with empty `codebook_coverage.csv` = 0% coverage
- Overall mean and median coverage across papers with a codebook
- List of lowest-coverage papers (bottom-N where N = `--top-n`)

---

### Phase E: Performance / timing section

From `bulk_summary` (successful papers only):
- Top-N by `elapsed_ms`
- Top-N by `download_ms`
- Top-N by `llm_ms`
- Top-N by `column_ms`

Papers with NA for a given timing column are excluded from that column's ranking; a note printed if any are excluded.

---

### Phase F: Optional CSV output

If `--out` is provided, write a long-format CSV with columns `report_date`, `section`, `metric`, `value`. Each computed metric from Phases B–E becomes one row. This allows comparison across report runs over time.

---

## Complexity Tracking

No constitution violations. No complexity justification needed.
