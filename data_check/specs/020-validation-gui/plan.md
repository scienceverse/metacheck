# Implementation Plan: Validation GUI

**Branch**: `020-validation-gui` | **Date**: 2026-03-23 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/020-validation-gui/spec.md`

## Summary

Build a local, keyboard-optimised Shiny application that presents each file in a
paper's `structure.csv` one at a time, renders a type-appropriate preview of the file's
contents, and writes the annotator's verified `type`, `group`, and `is_raw` values to a
persistent per-paper CSV in `ground_truth/`. The app must resume seamlessly across
sessions and support all primary actions via keyboard shortcuts designed for macOS.

## Technical Context

**Language/Version**: R (base R + already-installed packages: `shiny`, `bslib`, `haven`,
`readxl`, `officer`, `pdftools`, `jsonlite`)
**Primary Dependencies**: `shiny` (UI + server), `bslib` (layout/theming), `haven`/`readxl`
(structured data preview), `officer`/`pdftools` (document text extraction) — all already present
**Storage**: Local CSV files; `ground_truth/<paper_id>.csv` per paper; no database
**Testing**: Manual smoke-test against paper `0956797616647519` (19 files, mixed types)
**Target Platform**: macOS (local Shiny app opened in default browser tab)
**Project Type**: Local desktop-web tool (single-user Shiny app, no remote server)
**Performance Goals**: File preview renders in < 3 seconds for files < 100 MB; save action
completes in < 500 ms
**Constraints**: No network access; single concurrent user; macOS keyboard (no numpad);
file previews must read only first N lines/bytes (never load full file into memory)
**Scale/Scope**: ~20–200 files per paper; tens to low hundreds of papers to validate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|---|---|---|
| I — Crash Resilience | ✅ Pass | Ground-truth CSV is written immediately on each individual save; no in-memory batching |
| II — Paper ID Preservation | ✅ Pass | All `read.csv()` calls use `colClasses = c(paper_id = "character")` |
| III — Resource Limits | ✅ Pass (scoped) | Tool previews only first N lines/bytes of each file; pipeline resource limits do not apply to a read-only validation tool |
| IV — Centralised Shared Helpers | ✅ Pass | App sources `pipeline/helper.R` and reuses `read_data_head()` for structured file previews; no duplication of reading logic |
| V — Structured Error Classification | N/A | Tool is not a pipeline stage; does not write to `bulk_summary.csv` or any error-tracked output |

No gate violations. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/020-validation-gui/
├── plan.md          ← this file
├── research.md      ← Phase 0 output
├── data-model.md    ← Phase 1 output
├── quickstart.md    ← Phase 1 output
└── tasks.md         ← Phase 2 output (/speckit.tasks — not created here)
```

### Source Code

```text
data_check/
  tools/
    validation_gui/
      app.R           ← Shiny app entry point; UI definition + server logic
      preview.R       ← File preview rendering (sources pipeline/helper.R)
      gt_store.R      ← Ground-truth CSV read/write; session state helpers
  ground_truth/       ← Version-controlled dataset directory
    .gitkeep          ← Ensures directory is tracked by git when empty
```

**Structure Decision**: A dedicated `tools/validation_gui/` subdirectory isolates the GUI
from the pipeline scripts in `pipeline/` and `runners/`. Splitting the app into three files
(`app.R`, `preview.R`, `gt_store.R`) keeps each concern under ~200 lines without creating
an over-engineered package structure. `ground_truth/` sits at the `data_check/` root as a
peer to `outputs/`, making its status as a first-class dataset immediately legible.
