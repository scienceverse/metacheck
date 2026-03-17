# Research: Single Dataset Runner

**Feature**: 012-single-dataset-runner
**Date**: 2026-03-17

## How IDs Are Discovered

**Decision**: Read IDs from `XML_DIR` (defined in `0_index.R` as `"./data-raw/psychsci/grobid_0.8.2"`) by listing `.xml` files and stripping extensions — identical to `run_0_index_bulk.R`.

**Rationale**: This is the canonical ID source already used by the bulk runner. Reusing it ensures `run_single.R` operates on exactly the same population.

**Alternatives considered**: Scanning `data_check/data/` for folders — rejected because that only covers already-downloaded papers, which is a subset of all known IDs.

---

## How Stage 1 Is Invoked

**Decision**: `run_index(paper_id = pid, download = TRUE)` wrapped in `tryCatch`. On known error codes the error message is printed; partial-output papers are left in place.

**Rationale**: `run_index()` already encapsulates all pipeline logic and enforces resource limits. No re-implementation needed.

**Alternatives considered**: Inlining download/classify logic — rejected (violates Constitution Principle IV).

---

## How Stage 2 Is Invoked

**Decision**: `run_codebook_label(paper_id = pid)` wrapped in `tryCatch`, called only after stage 1 completes (success or known-error exit). Stage 2 requires `outputs/<pid>/columns.csv` to exist; if stage 1 failed, stage 2 is skipped with a message.

**Rationale**: `run_2_codebook_bulk.R` already shows the correct pattern — check for `columns.csv` before calling `run_codebook_label()`.

**Alternatives considered**: Always attempt stage 2 regardless of stage 1 outcome — rejected because `run_codebook_label()` will error anyway if `columns.csv` is absent.

---

## Timing

**Decision**: Use `proc.time()[["elapsed"]]` around each stage, consistent with `run_2_codebook_bulk.R`.

**Rationale**: Matches existing convention; no new timing infrastructure needed.

---

## No New Entities or Contracts

The script is purely internal — it calls existing functions and writes no new file formats. No contracts directory is needed.
