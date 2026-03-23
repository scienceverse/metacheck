# Implementation Plan: LLM Temperature Stability Testing

**Branch**: `017-llm-temperature-testing` | **Date**: 2026-03-20 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/017-llm-temperature-testing/spec.md`

## Summary

Adds a temperature sweep toolkit on top of the existing single-paper pipeline. The single-paper sweep runner (`run_sweep.R`) is refactored to expose `run_paper_sweep()` as a callable function; a new bulk runner (`run_sweep_bulk.R`) calls it across all papers with crash-resilient per-paper logging. A grand report script (`report_sweep_grand.R`) produces a flat CSV suitable for post-processing. A no-data end state (`status = "no_data"`) is introduced so papers with no classifiable data files complete gracefully without triggering an error or aborting the sweep.

## Technical Context

**Language/Version**: R (base R only — no new packages)
**Primary Dependencies**: `metacheck` (`llm()`), `haven`, `readxl`, `jsonlite` — all already installed; `helper.R`, `0_index.R`, `2_codebook_label.R` sourced at runtime
**Storage**: CSV files on local filesystem under `sweep_results/<paper_id>/`
**Testing**: Manual smoke tests — `N_PAPERS <- 2` cap on bulk runner; `--repeats 2` on single-paper sweep
**Target Platform**: macOS local (no deployment target)
**Project Type**: CLI scripts (top-level R scripts, no package structure)
**Performance Goals**: No hard targets; 12 runs (1 paper × 4 temps × 3 repeats) must complete without manual intervention
**Constraints**: No new R packages; all resource limits from Principle III apply unchanged inside each sweep run
**Scale/Scope**: Up to ~200 papers × 4 temperatures × 5 repeats = ~4,000 pipeline runs

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-checked post-design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I — Crash Resilience | ✅ PASS | `sweep_log.csv` written after each (temp × repeat) run; `sweep_bulk_log.csv` written after each paper. Both enable resume without reprocessing. |
| II — Paper ID Preservation | ✅ PASS | `paper_id` stored as character in all log CSVs; `colClasses = c(paper_id = "character")` required on all `read.csv()` calls that load paper IDs. |
| III — Resource Limits | ✅ PASS | No limits are modified. Each sweep run calls `run_index()` and `run_codebook_label()` which enforce existing limits internally. |
| IV — Centralised Helpers | ✅ PASS | `run_paper_sweep()` extracted into `run_sweep.R`; bulk runner sources it. No sweep logic duplicated. `run_one()`, `load_or_create_sweep_log()`, etc. are not re-implemented. |
| V — Structured Error Classification | ✅ PASS | `no_data` is a **completion** status (like `"ok"`), not an error code — it is parallel to `"ok"`, not to `"failed"`. The existing error codes (`no_links`, `download_failed`, `empty_repo`, `too_large`) are unchanged and still the only recognised failure codes. No violation. |

**Post-design re-check**: ✅ All principles satisfied. No complexity violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/017-llm-temperature-testing/
├── plan.md              # This file
├── research.md          # Phase 0 — design decisions
├── data-model.md        # Phase 1 — entity schemas and state transitions
├── contracts/
│   ├── run_sweep_bulk.md       # Bulk sweep runner contract
│   └── report_sweep_grand.md  # Grand report script contract
└── tasks.md             # Phase 2 output (/speckit.tasks — not created here)
```

### Source Code

```text
data_check/
├── run_sweep.R          # MODIFIED — extract run_paper_sweep(); add no_data handling in run_one()
├── run_sweep_bulk.R     # NEW — bulk runner; sources run_sweep.R; calls run_paper_sweep()
├── report_sweep_grand.R # NEW — grand flat CSV report; sources report_sweep.R + 0_index.R
├── report_sweep.R       # UNMODIFIED — per-paper sweep report; compute_stability/quality reused
├── helper.R             # UNMODIFIED (doc fix only, no logic change)
├── 0_index.R            # UNMODIFIED
└── 2_codebook_label.R   # UNMODIFIED
```

**Structure Decision**: Flat file layout matching all existing pipeline scripts. No subdirectory for sweep scripts — consistent with the project's convention of placing all entry-point R scripts at `data_check/` root.
