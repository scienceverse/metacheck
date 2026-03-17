# Implementation Plan: Per-ID Output Directory Structure

**Branch**: `007-per-id-output-structure` | **Date**: 2026-03-17 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/007-per-id-output-structure/spec.md`

## Summary

Replace the flat `structure/` output directory — where all papers' CSVs are mixed together with paper IDs baked into filenames — with a per-paper `outputs/<paper_id>/` layout. Every pipeline stage (`0_index.R`, `2_codebook_label.R`) writes its files into the paper's own subdirectory, using short filenames without the ID prefix. A one-time migration script moves existing `structure/` files. The bulk runner's resume logic and `bulk_summary.csv` remain at the repo root.

## Technical Context

**Language/Version**: R (base R only, no new packages)
**Primary Dependencies**: `metacheck`, `haven`, `readxl`, `jsonlite` — all already present
**Storage**: CSV files on local filesystem; `outputs/<paper_id>/` directories
**Testing**: Manual run + file-system inspection; no automated test framework in this project
**Target Platform**: macOS / Linux filesystem (local development machine)
**Project Type**: CLI pipeline / batch script
**Performance Goals**: No change — output path construction is negligible overhead
**Constraints**: Paper IDs must remain character strings throughout (leading-zero preservation)
**Scale/Scope**: ~hundreds of papers; each paper produces 2–4 CSV files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ PASS | Per-paper directory is created before writing; `bulk_summary.csv` append pattern unchanged |
| II. Paper ID Preservation | ✅ PASS | ID is used as directory name (character), never parsed numerically |
| III. Resource Limits | ✅ PASS | No changes to download/LLM/extraction limits |
| IV. Centralised Helpers | ✅ PASS | Output-path helper added to `helper.R` per constitution rule |
| V. Structured Error Classification | ✅ PASS | No new failure modes introduced |

No gate violations. Complexity Tracking table not required.

## Project Structure

### Documentation (this feature)

```text
specs/007-per-id-output-structure/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
data_check/
├── 0_index.R              # Change: STRUCTURE_DIR → OUTPUT_DIR; write to outputs/<id>/
├── 1_data_label.R         # Change: STRUCTURE_DIR → OUTPUT_DIR; read/write from outputs/<id>/
├── 2_codebook_label.R     # Change: STRUCTURE_DIR → OUTPUT_DIR; read/write from outputs/<id>/
├── run_index_bulk.R       # Change: resume logic reads outputs/<id>/structure.csv
├── helper.R               # Change: add paper_output_dir() helper
├── migrate_structure.R    # NEW: one-time migration script
├── bulk_summary.csv       # UNCHANGED: stays at repo root
└── outputs/               # NEW: replaces structure/
    └── <paper_id>/
        ├── structure.csv
        ├── columns.csv
        ├── labels.csv           (if codebook stage ran)
        └── codebook_coverage.csv (if codebook stage ran)
```

**Structure Decision**: Single project layout. No frontend/backend split. The `outputs/` directory replaces `structure/` as the canonical output location; `bulk_summary.csv` remains at the repo root for cross-paper aggregation.

## Phase 0: Research

*No external unknowns requiring research. All decisions are deterministic given the existing codebase.*

See [research.md](research.md) for decision log.

## Phase 1: Design

### Output path contract

All pipeline stages resolve per-paper output paths through a single helper:

```
paper_output_dir(paper_id)  →  "./data_check/outputs/<paper_id>"
```

This helper (in `helper.R`):
1. Constructs the path.
2. Creates the directory if it does not exist (`dir.create(..., recursive = TRUE)`).
3. Returns the path as a character string.

Callers then build file paths as:
```
file.path(paper_output_dir(paper_id), "structure.csv")
file.path(paper_output_dir(paper_id), "columns.csv")
file.path(paper_output_dir(paper_id), "labels.csv")
file.path(paper_output_dir(paper_id), "codebook_coverage.csv")
```

### Resume logic change

`run_index_bulk.R` currently skips papers already present in `bulk_summary.csv`. This is unchanged. The `bulk_summary.csv` remains the primary resume signal, so no change to skip logic is required by this feature.

### Constant rename

| Old constant | New constant | File |
|---|---|---|
| `STRUCTURE_DIR <- "./data_check/structure"` | `OUTPUT_DIR <- "./data_check/outputs"` | `0_index.R`, `1_data_label.R`, `2_codebook_label.R` |

### Migration

`migrate_structure.R` (one-time script):

1. List all files in `structure/` matching `<paper_id>_*.csv`.
2. For each file: extract `paper_id` from the filename prefix, create `outputs/<paper_id>/`, copy the file with the `<paper_id>_` prefix stripped, verify the copy, then delete the original.
3. Print a summary of files moved and any failures.
4. Leaves the `structure/` directory in place but empty (safe to remove manually).

### Constitution check (post-design)

All principles remain satisfied. `paper_output_dir()` creates the directory idempotently before any write, preserving crash-resilience. Paper ID is only ever used as a directory name string, never parsed numerically.
