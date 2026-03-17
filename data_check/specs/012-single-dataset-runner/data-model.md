# Data Model: Single Dataset Runner

**Feature**: 012-single-dataset-runner
**Date**: 2026-03-17

## No New Entities

This feature introduces no new data entities or file formats. It consumes and produces the same files as the existing pipeline stages:

| File | Producer | Consumer |
|------|----------|---------|
| `outputs/<paper_id>/structure.csv` | `run_index()` | `run_codebook_label()` |
| `outputs/<paper_id>/columns.csv`   | `run_index()` | `run_codebook_label()` |
| `outputs/<paper_id>/labels.csv`    | `run_codebook_label()` | downstream analysis |
| `outputs/<paper_id>/codebook_coverage.csv` | `run_codebook_label()` | downstream analysis |

For column definitions of each file, see `docs/output-schemas.md`.

## Key Invariants

- `paper_id` is always a character string — never numeric.
- `outputs/<paper_id>/columns.csv` must exist before `run_codebook_label()` is called.
- Stage 2 is only attempted when stage 1 writes its outputs successfully.
