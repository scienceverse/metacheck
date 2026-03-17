# Feature Specification: Bulk Label Runners

**Feature Branch**: `008-bulk-label-runners`
**Created**: 2026-03-17
**Status**: Draft
**Input**: User description: "1_data_label and 2_codebook_label should be functions, not individual runnable scripts. Using this, make both a function and give them both an individual bulk run file with similar behavior as run_index_bulk.R"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Run column labelling across all papers in bulk (Priority: P1)

A pipeline operator wants to run the data-label stage across all processed papers in one command, with the same crash-resilient, auto-resume behaviour already provided by the index bulk runner. They run a single script and leave it; if it crashes, restarting it skips already-completed papers automatically.

**Why this priority**: This is the primary motivation — without a bulk runner for data-label, operators must call the function paper-by-paper manually.

**Independent Test**: Run the data-label bulk runner against a set of papers that already have `outputs/<paper_id>/structure.csv`. Confirm a `label_summary.csv` is created with one row per paper, and that restarting the runner skips already-completed papers.

**Acceptance Scenarios**:

1. **Given** N papers with `outputs/<paper_id>/structure.csv` present, **When** the data-label bulk runner is executed, **Then** `outputs/<paper_id>/columns.csv` is produced for each paper and one row per paper is appended to `label_summary.csv`.
2. **Given** `label_summary.csv` already has M rows from a prior run, **When** the bulk runner is restarted, **Then** those M papers are skipped and only the remaining papers are processed.
3. **Given** a paper whose data-label stage fails (e.g., no data files found), **When** the bulk runner encounters it, **Then** the failure is recorded in `label_summary.csv` with an error message and processing continues with the next paper.

---

### User Story 2 - Run codebook labelling across all papers in bulk (Priority: P2)

A pipeline operator wants to run the codebook-label stage across all papers that have already completed the data-label stage, again with crash-resilient auto-resume behaviour.

**Why this priority**: Mirrors US1 but for the codebook stage; depends on US1 outputs being present.

**Independent Test**: Run the codebook-label bulk runner against papers that have `outputs/<paper_id>/columns.csv`. Confirm `outputs/<paper_id>/labels.csv` and `outputs/<paper_id>/codebook_coverage.csv` are produced, and a `codebook_summary.csv` tracks progress.

**Acceptance Scenarios**:

1. **Given** N papers with `outputs/<paper_id>/columns.csv` present, **When** the codebook-label bulk runner is executed, **Then** `outputs/<paper_id>/labels.csv` and `outputs/<paper_id>/codebook_coverage.csv` are produced for each paper.
2. **Given** `codebook_summary.csv` has M rows from a prior run, **When** the bulk runner restarts, **Then** those M papers are skipped.
3. **Given** a paper with no codebook or readme files, **When** the bulk runner processes it, **Then** the result is recorded in `codebook_summary.csv` as a known outcome (no_codebook) and processing continues.

---

### User Story 3 - Call label stages programmatically from other scripts (Priority: P3)

A developer integrating the pipeline wants to call the data-label and codebook-label stages as functions from another script (e.g., a combined end-to-end runner), rather than being forced to `source()` a script that executes on load.

**Why this priority**: This is the underlying structural change enabling US1 and US2, and makes future integration straightforward. Lower priority only because the bulk runners (US1/US2) are the immediate user-facing value.

**Independent Test**: In a fresh R session, `source("1_data_label.R")` and confirm no immediate execution occurs — only a function is defined. Then call `run_data_label(paper_id = "<id>")` and confirm it produces the expected output.

**Acceptance Scenarios**:

1. **Given** `1_data_label.R` is sourced, **When** the file is loaded, **Then** no side effects occur (no files written, no errors thrown) — only `run_data_label()` is defined.
2. **Given** `2_codebook_label.R` is sourced, **When** the file is loaded, **Then** no side effects occur — only `run_codebook_label()` is defined (this function already exists; the script must stop executing top-level code on source).
3. **Given** `run_data_label(paper_id)` is called, **When** it completes successfully, **Then** it returns a structured result list analogous to `run_index()`'s return value (paper_id, success, error, timing, counts).

---

### Edge Cases

- What if a paper's `outputs/<paper_id>/structure.csv` is missing when the data-label bulk runner encounters it? The paper should be skipped with an error recorded in the summary CSV.
- What if `outputs/<paper_id>/columns.csv` is missing when the codebook-label bulk runner encounters it? Same — skip with error recorded.
- What if the bulk runner is interrupted mid-paper (file partially written)? On restart it detects the paper is not yet in the summary CSV and reprocesses it cleanly.
- What if a paper appears in the summary CSV with `success = FALSE`? It should NOT be re-run on restart (same behaviour as the index bulk runner — the summary CSV is the canonical completion record).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: `1_data_label.R` MUST wrap all column-extraction logic in a callable function (`run_data_label(paper_id)`) and MUST NOT execute any top-level code when sourced.
- **FR-002**: `2_codebook_label.R` MUST wrap all codebook-labelling logic in a callable function (`run_codebook_label(paper_id)`) and MUST NOT execute any top-level code when sourced. (The function already exists; the script's top-level execution must be removed.)
- **FR-003**: Both functions MUST return a structured result list containing at minimum: `paper_id`, `success` (logical), `error` (character or NA), elapsed timing, and relevant counts.
- **FR-004**: A bulk runner script `run_label_bulk.R` MUST process all papers for which `outputs/<paper_id>/structure.csv` exists, calling `run_data_label()` for each, appending one row per paper to `label_summary.csv` immediately after each paper completes.
- **FR-005**: A bulk runner script `run_codebook_bulk.R` MUST process all papers for which `outputs/<paper_id>/columns.csv` exists, calling `run_codebook_label()` for each, appending one row per paper to `codebook_summary.csv` immediately after each paper completes.
- **FR-006**: Both bulk runners MUST auto-resume on restart by reading their respective summary CSVs and skipping paper IDs already present (regardless of success/failure status).
- **FR-007**: Both bulk runners MUST catch per-paper errors, record them in the summary CSV, and continue processing remaining papers — a single paper failure MUST NOT halt the bulk run.
- **FR-008**: Both bulk runners MUST support an `N_RUNS` cap (integer or `Inf`) consistent with `run_index_bulk.R`.
- **FR-009**: Paper IDs MUST be read and stored as character strings throughout all new and modified scripts to preserve leading zeros.

### Key Entities

- **`run_data_label(paper_id)`**: Callable function encapsulating column extraction for one paper. Reads `outputs/<paper_id>/structure.csv`, writes `outputs/<paper_id>/columns.csv`, returns result list.
- **`run_codebook_label(paper_id)`**: Callable function (already exists in `2_codebook_label.R`) encapsulating codebook labelling for one paper. Reads `outputs/<paper_id>/structure.csv` and `columns.csv`, writes `labels.csv` and `codebook_coverage.csv`, returns result list.
- **`label_summary.csv`**: Crash-resilient progress log for the data-label bulk run. One row per paper, appended after each paper. Analogous to `bulk_summary.csv`.
- **`codebook_summary.csv`**: Crash-resilient progress log for the codebook-label bulk run. One row per paper, appended after each paper.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Sourcing either label script produces zero side effects — no files written, no errors thrown, no output printed.
- **SC-002**: The data-label bulk runner processes all eligible papers (those with `structure.csv`) in a single invocation, producing one `label_summary.csv` row per paper.
- **SC-003**: The codebook-label bulk runner processes all eligible papers (those with `columns.csv`) in a single invocation, producing one `codebook_summary.csv` row per paper.
- **SC-004**: On restart after interruption, both bulk runners skip 100% of papers already present in their respective summary CSVs.
- **SC-005**: A single paper failure causes zero disruption to subsequent papers in any bulk run — the failure is recorded and the run continues.

## Assumptions

- `run_codebook_label(paper_id)` already exists as a function in `2_codebook_label.R`; the only change needed there is removing the top-level script execution code.
- `1_data_label.R` currently runs its logic at top-level with a hardcoded paper ID; wrapping it into `run_data_label()` is the primary structural change for that file.
- The summary CSVs (`label_summary.csv`, `codebook_summary.csv`) live at the repo root alongside `bulk_summary.csv`.
- "Already completed" means the paper ID appears in the summary CSV — both successes and failures are skipped on resume (same policy as the index bulk runner).
- The data-label bulk runner determines eligible papers by scanning `outputs/` for directories containing `structure.csv`, not by reading `bulk_summary.csv`.

## Out of Scope

- Merging all three pipeline stages into a single combined bulk runner.
- Parallel/concurrent processing of multiple papers within a single bulk run.
- Changes to the output schemas of any existing CSV file.
- A GUI or interactive interface for triggering bulk runs.
