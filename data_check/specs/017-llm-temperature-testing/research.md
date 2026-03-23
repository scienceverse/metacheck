# Research: LLM Temperature Stability Testing — Bulk Sweep Extension

**Branch**: `017-llm-temperature-testing` | **Date**: 2026-03-20

## R1 — Refactor strategy for `run_paper_sweep()`

**Decision**: Extract the per-paper sweep loop from `run_sweep.R::main()` into a standalone function `run_paper_sweep(paper_id, temperatures, repeats, sweep_dir)`. The existing `main()` is reduced to argument parsing + validation + a single call to `run_paper_sweep()`. The bulk runner sources `run_sweep.R` and calls `run_paper_sweep()` directly.

**Rationale**: Mirrors the established pattern: `run_0_index_bulk.R` sources `0_index.R` and calls `run_index()`; `run_sweep_bulk.R` sources `run_sweep.R` and calls `run_paper_sweep()`. No logic is duplicated.

**Alternatives considered**:
- Re-implementing the temperature loop in `run_sweep_bulk.R` — rejected: duplicates `run_one()`, `load_or_create_sweep_log()`, and `append_sweep_log()`, violating Principle IV.
- Moving all sweep helpers to `helper.R` — rejected: sweep logic is not used by any other pipeline stage. Sourcing `run_sweep.R` achieves reuse without polluting the shared helper.

**Existing functions that require NO changes**: `run_one()`, `load_or_create_sweep_log()`, `append_sweep_log()`, `sweep_run_done()`.

---

## R2 — Grand report per-stage row layout

**Decision**: Source `report_sweep.R` and call `compute_stability()` + `compute_quality()` per paper. Unpivot to two rows per (paper × temperature):
- Row 1: `stage = "index"` — stability = `col_type_agreement`; quality = `known_type_rate` only (codebook metrics = NA)
- Row 2: `stage = "codebook"` — stability = `label_agreement`; quality = `codebook_coverage_rate` + `nonempty_label_rate` (index metrics = NA)

**Rationale**: `compute_stability()` already separates index-stage (`col_type_agreement`) from codebook-stage (`label_agreement`) in a single call. Splitting at the grand report level keeps existing per-paper report logic unchanged.

**Alternatives considered**:
- Re-computing pairwise agreement inside `report_sweep_grand.R` — rejected: duplicates `pairwise_agreement()`, violating Principle IV.

**NA handling**: Papers with no codebook already return `NA` for `label_agreement` and coverage metrics inside the compute functions — no additional handling needed.

---

## R3 — Bulk log resume strategy

**Decision**: Single `sweep_bulk_log.csv` at `./data_check/sweep_results/sweep_bulk_log.csv`. Schema:

| Column | Type | Notes |
|---|---|---|
| `paper_id` | character | Leading-zero safe |
| `temperatures` | character | Comma-separated, e.g. `"0,0.3,0.7,1"` |
| `repeats` | integer | Repeat count used |
| `n_ok` | integer | Number of runs that completed successfully |
| `n_failed` | integer | Number of runs that failed |
| `n_skipped` | integer | Number of runs skipped (already done) |
| `elapsed_ms` | integer | Wall time for this paper's full sweep |
| `timestamp` | character | ISO timestamp |
| `status` | character | Always `"done"` (appended after all runs attempted) |

**Resume logic**: Load `sweep_bulk_log.csv`; extract `paper_id` values as `done_ids`; `setdiff(all_ids, done_ids)` — identical to `run_0_index_bulk.R`.

**Rationale**: A paper is only logged after all its (temperature × repeat) combinations have been attempted. Within-paper resume is handled by the existing per-run `sweep_log.csv`.

**Alternatives considered**:
- Directory existence as resume signal — rejected: directory is created at the start, so a crash before any runs would still mark the paper as done.

---

## R4 — Paper discovery: XML_DIR

**Confirmed**: `XML_DIR <- "./data-raw/psychsci/grobid_0.8.2"` is defined in `0_index.R` (line 56). `run_sweep_bulk.R` sources `run_sweep.R` which sources `0_index.R`, so `XML_DIR` is available after sourcing — no redefinition needed.

```r
all_ids <- tools::file_path_sans_ext(
  list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
)
```

---

## R7 — Parallelism for bulk sweep

**Decision**: Sequential processing — one paper at a time, bulk log written after each paper.

**Rationale**: All parallel approaches were attempted and failed on macOS:
- `mclapply` (fork-based): macOS Objective-C runtime crashes in forked children; additionally, the LLM HTTP client inherits broken connection state from the parent process — all LLM calls fail in forked workers.
- `parLapply` (PSOCK cluster): `clusterEvalQ` hangs indefinitely during worker initialisation (sourcing pipeline scripts in fresh R processes blocks, likely due to package loading or network calls).
- Shell background `Rscript` processes: the LLM API rate-limits concurrent requests, eliminating the throughput benefit of parallelism.

Sequential execution with per-paper crash resilience (`sweep_bulk_log.csv` written after every paper, per-run resume via per-paper `sweep_log.csv`) is the correct approach for this workload.

---

## R5 — Script guard pattern

**Confirmed**: New scripts (`run_sweep_bulk.R`, `report_sweep_grand.R`) follow the `run_0_index_bulk.R` pattern — top-level scripts with no `if (!interactive())` guard. They run on source/Rscript execution. Only `run_sweep.R` and `report_sweep.R` use the guard because they are also callable as CLI tools with argument parsing.

---

## R6 — No-data end state

**Decision**: When `run_index()` produces a `columns.csv` with zero rows (no files classified as `data`), `run_one()` treats this as `status = "no_data"` — a successful terminal state distinct from `"ok"` and `"failed"`. The codebook stage is skipped. The result is logged in `sweep_log.csv` with `status = "no_data"` and counts towards `n_ok` in `BulkSweepRecord` (it is not a failure).

**Rationale**: A paper whose OSF repository contains only code, supplemental materials, or assets is a legitimate and common real-world case. Treating it as a failure inflates `n_failed`, pollutes bulk summary statistics, and could trigger unwanted retry logic. Separating it as `no_data` lets consumers filter or aggregate it distinctly in the grand report.

**Detection**: After `run_index()` completes successfully, check whether `columns.csv` exists and has at least 1 data row. If not → `status = "no_data"`. This check must occur inside `run_one()`, after `run_index()` returns, before attempting `run_codebook_label()`.

**Alternatives considered**:
- Map to existing `empty_repo` code — rejected: `empty_repo` means no files at all after unpacking; `no_data` means files were found and classified, but none as `data`. These are distinct outcomes.
- Map to `"ok"` — rejected: callers (bulk runner, grand report) need to distinguish "pipeline ran and produced data" from "pipeline ran but found nothing to measure" to avoid dividing by zero in quality metrics.
- Map to `"failed"` — rejected: no error occurred; the pipeline completed its work correctly.
