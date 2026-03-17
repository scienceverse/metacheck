# Implementation Plan: Single Dataset Runner

**Branch**: `012-single-dataset-runner` | **Date**: 2026-03-17 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/012-single-dataset-runner/spec.md`

## Summary

Create `data_check/run_single.R` — a top-level script that selects one random paper ID from the XML directory, runs `run_index()` (stage 1: download/classify/columns), then runs `run_codebook_label()` (stage 2: codebook labelling), and prints the result of each stage. The script mirrors how the two bulk runners work but collapses both into a single invocation for one paper.

## Technical Context

**Language/Version**: R (base R only — no new packages)
**Primary Dependencies**: `0_index.R`, `2_codebook_label.R`, `helper.R` — all already present
**Storage**: CSV files on local filesystem under `data_check/outputs/<paper_id>/`
**Testing**: Manual — run the script, inspect `outputs/<paper_id>/` for produced files
**Target Platform**: Local macOS/Linux R session (same as all other pipeline scripts)
**Project Type**: CLI script (sourced/run via `Rscript`)
**Performance Goals**: Single-paper run — no additional performance targets beyond existing pipeline
**Constraints**: Must not introduce new packages; IDs must stay as character strings
**Scale/Scope**: One paper per invocation

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I — Crash Resilience | ✅ Pass | Single-paper script; partial outputs (stage 1 succeeds, stage 2 errors) are preserved on disk by the existing pipeline scripts. No bulk accumulation needed. |
| II — Paper ID Preservation | ✅ Pass | IDs discovered from XML filenames (strings). Must NOT coerce to numeric at any point. |
| III — Resource Limits | ✅ Pass | Limits enforced inside `run_index()` — no new bypass. |
| IV — Centralised Helpers | ✅ Pass | Script calls `run_index()` and `run_codebook_label()` directly; no logic is duplicated. |
| V — Structured Error Codes | ✅ Pass | Known error codes (`no_links`, `download_failed`, `empty_repo`, `too_large`) surfaced in console output. |

No violations — Complexity Tracking table not required.

## Project Structure

### Documentation (this feature)

```text
specs/012-single-dataset-runner/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks — not created here)
```

### Source Code (repository root)

```text
data_check/
├── run_single.R         # NEW — single-paper full-pipeline runner
├── 0_index.R            # Unchanged — provides run_index()
├── 2_codebook_label.R   # Unchanged — provides run_codebook_label()
└── helper.R             # Unchanged — shared helpers
```

**Structure Decision**: Single new top-level script. No new directories, no new packages, no new helpers.
