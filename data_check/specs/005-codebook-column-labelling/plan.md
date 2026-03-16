# Implementation Plan: Codebook Column Labelling

**Branch**: `005-codebook-column-labelling` | **Date**: 2026-03-16 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/005-codebook-column-labelling/spec.md`

## Summary

Read codebook/readme files already classified in `_structure.csv`, extract variable definitions via rule-based parsing and LLM fallback, match definitions to columns in `_columns.csv` with experiment-group scoping, and write per-paper `_labels.csv` and `_codebook_coverage.csv` outputs. A new `2_codebook_label.R` script (with a `run_codebook_label()` entry point) wraps the logic; shared parsing and matching helpers go into `helper.R`.

## Technical Context

**Language/Version**: R (base R only — no new packages; `haven`/`readxl`/`jsonlite` already present)
**Primary Dependencies**: `llm_batch()`, `extract_json()` (existing helpers in `helper.R`); `jsonlite::fromJSON`
**Storage**: `structure/<paper_id>_labels.csv`, `structure/<paper_id>_codebook_coverage.csv` (new); reads existing `_structure.csv` and `_columns.csv`
**Testing**: Manual spot-checks against known-good papers (project has no automated test harness)
**Target Platform**: Same local R environment as the rest of the pipeline
**Project Type**: CLI data-processing pipeline (R script, function-per-stage pattern)
**Performance Goals**: Codebook files are small (< a few MB); parsing latency is dominated by LLM calls for unstructured files — same order of magnitude as column classification
**Constraints**: LLM call budget inherited from constitution Principle III (10 calls/paper); codebook parsing counts against this budget. Files > 100 MB are skipped with a warning.
**Scale/Scope**: One paper at a time, bulk via `run_index_bulk.R`-style runner; same scope as existing pipeline

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ PASS | `_labels.csv` written immediately after each paper; bulk runner can resume |
| II. Paper ID Preservation | ✅ PASS | All `read.csv()` calls will use `colClasses = c(paper_id = "character")` |
| III. Conservative Resource Limits | ✅ PASS | LLM call budget shared; codebook file size capped at 10 MB; no new download |
| IV. Centralised Shared Helpers | ✅ PASS | `parse_codebook()` and `match_column_labels()` added to `helper.R`; not duplicated |
| V. Structured Error Classification | ⚠️ NEEDS NEW CODE | `no_codebook` is a per-paper label status (in `_labels.csv`), not a fatal pipeline error — does not require a `bulk_summary.csv` error code. No new top-level error codes needed. |

**Post-design re-check**: All gates pass. The `no_codebook` state is recorded in `_labels.csv` as a status field, not surfaced as a bulk error code, because labelling failure does not prevent downstream use of `_columns.csv`.

## Project Structure

### Documentation (this feature)

```text
specs/005-codebook-column-labelling/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
helper.R                        # +parse_codebook(), +match_column_labels()
2_codebook_label.R              # new — run_codebook_label(paper_id)

structure/
├── <paper_id>_structure.csv    # existing input (type, group per file)
├── <paper_id>_columns.csv      # existing input (column_name, group per column)
├── <paper_id>_labels.csv       # NEW output — one row per data column
└── <paper_id>_codebook_coverage.csv  # NEW output — one row per codebook variable

docs/
├── pipeline.md                 # UPDATE — add Stage 12 to flow diagram
└── output-schemas.md           # UPDATE — add _labels.csv and _codebook_coverage.csv schemas
```

**Structure Decision**: Flat R files at repo root, consistent with `0_index.R` / `1_data_label.R` / `2_data_to_schema.R`. New shared helpers go in `helper.R` per Principle IV. No `src/` reorganisation.

## Complexity Tracking

No constitution violations to justify.
