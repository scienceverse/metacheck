# Feature Specification: Per-ID Output Directory Structure

**Feature Branch**: `007-per-id-output-structure`
**Created**: 2026-03-17
**Status**: Draft
**Input**: User description: "Currently the outputs of the three main files (index, data_label and codebook label) deliver their outputs in a ridiculous way in a single structure output folder. This is not a scalable way to go further. a per id version is preferable, but the data folders already hold the folder ID. We need a rebase of all outputs into a structured output that is robust to further expansions and both machine parsable and human understandable"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Find all outputs for a single paper (Priority: P1)

A researcher or pipeline consumer wants to inspect everything the pipeline produced for a specific paper ID. They navigate to a single folder and find all related output files there — no need to grep through a flat directory of hundreds of mixed-paper files.

**Why this priority**: The core pain point. Everything else depends on outputs being co-located by paper ID.

**Independent Test**: Given any processed paper ID, a user can navigate to `outputs/<paper_id>/` and find all CSVs produced for that paper. Verifiable without running any pipeline code.

**Acceptance Scenarios**:

1. **Given** a paper `0956797615620784` has been processed, **When** the user browses `outputs/0956797615620784/`, **Then** they find `structure.csv`, `columns.csv`, `labels.csv`, and `codebook_coverage.csv` (whichever were produced).
2. **Given** two papers have been processed, **When** the user lists `outputs/`, **Then** they see exactly two subdirectories, one per paper ID, with no CSV files at the root level.

---

### User Story 2 - Pipeline resumes without reprocessing completed papers (Priority: P2)

When the bulk runner is restarted after a crash or interruption, it detects which paper IDs already have complete output directories and skips them, processing only new or incomplete papers.

**Why this priority**: Crash-resilience is a stated design goal; changing the output structure must not break it.

**Independent Test**: Process one paper, stop the runner, restart it — the already-processed paper must not be re-run and its outputs must remain intact.

**Acceptance Scenarios**:

1. **Given** paper A has a complete output directory, **When** the bulk runner starts, **Then** paper A is skipped and logged as already done.
2. **Given** paper B has an incomplete output directory (e.g., `structure.csv` present but `columns.csv` missing due to mid-run failure), **When** the bulk runner starts, **Then** paper B is reprocessed.

---

### User Story 3 - Aggregate view across all papers (Priority: P3)

A pipeline consumer wants a cross-paper summary that remains easy to produce by scanning the per-ID directories, without losing the existing aggregate reporting capability.

**Why this priority**: Downstream users depend on the aggregate CSV; the restructuring must not eliminate it.

**Independent Test**: After processing N papers, running the bulk runner produces a `bulk_summary.csv` containing one row per paper.

**Acceptance Scenarios**:

1. **Given** N processed per-ID output directories, **When** the summary is collected, **Then** `bulk_summary.csv` contains exactly N rows, one per paper.
2. **Given** a new paper is processed, **When** the summary is refreshed, **Then** `bulk_summary.csv` gains one row for the new paper.

---

### Edge Cases

- What happens when outputs from the old flat `structure/` directory exist? Migration must not silently corrupt or duplicate data.
- How does the system handle a paper ID where only some pipeline stages completed (partial outputs)? The directory exists but is incomplete — resumption logic must distinguish partial from complete.
- What if two pipeline runs attempt to write to the same paper ID directory simultaneously? File writes should be atomic at the file level to avoid partial reads.
- What if `outputs/<paper_id>/` does not yet exist when a pipeline stage tries to write? The pipeline must create it automatically.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: All per-paper output files (`structure.csv`, `columns.csv`, `labels.csv`, `codebook_coverage.csv`) MUST be written to `outputs/<paper_id>/` rather than the current flat `structure/` directory.
- **FR-002**: The `outputs/` root directory MUST contain only subdirectories named by paper ID; no per-paper CSV files at the root level.
- **FR-003**: The bulk summary file (`bulk_summary.csv`) MUST continue to exist as a cross-paper aggregate and MUST live outside the per-ID subdirectories (e.g., at the repo root).
- **FR-004**: The pipeline MUST create `outputs/<paper_id>/` automatically on first write if it does not exist.
- **FR-005**: The resume/skip logic in the bulk runner MUST detect completion by inspecting the presence of expected files within `outputs/<paper_id>/`, not by checking the old flat directory.
- **FR-006**: All three pipeline stages (index, data_label, codebook_label) MUST write their outputs to the same `outputs/<paper_id>/` directory for that paper.
- **FR-007**: A one-time migration script MUST be provided to move existing files from `structure/<paper_id>_*.csv` to `outputs/<paper_id>/*.csv` with the paper-ID prefix stripped from filenames.
- **FR-008**: Output file names within each per-ID directory MUST NOT include the paper ID prefix (e.g., `structure.csv` not `0956797615620784_structure.csv`), since the paper ID is already encoded in the directory path.

### Key Entities

- **Paper**: Identified by a unique numeric string ID (e.g., `0956797615620784`). The ID is the primary organizing key for all outputs.
- **Output Directory**: `outputs/<paper_id>/` — the canonical location for all files produced for a given paper by any pipeline stage.
- **Stage Output File**: A single CSV produced by one pipeline stage (`structure.csv`, `columns.csv`, `labels.csv`, `codebook_coverage.csv`). Lives inside the paper's output directory, without the paper ID in the filename.
- **Bulk Summary**: Cross-paper aggregate (`bulk_summary.csv`). Lives outside per-ID directories; one row per paper.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: After migration, zero per-paper CSV files exist in the old flat `structure/` directory; all are accessible under `outputs/<paper_id>/`.
- **SC-002**: A pipeline consumer can locate all outputs for any given paper by navigating to one directory path, with no file-system search required.
- **SC-003**: The bulk runner's resume behavior correctly skips 100% of fully processed papers and correctly re-queues papers with incomplete output directories when restarted after interruption.
- **SC-004**: The aggregate `bulk_summary.csv` continues to be produced with the same schema and row-per-paper structure as before the restructuring.
- **SC-005**: Adding a new pipeline stage in future requires only writing its output file into the existing `outputs/<paper_id>/` directory — no changes to directory layout logic are needed.

## Assumptions

- The `data/<paper_id>/` directories (raw downloaded data) remain unchanged; only the output side is being restructured.
- The paper ID is always available as a character string at the point each pipeline stage writes its output.
- `bulk_summary.csv` will remain at the repo root (not inside `outputs/`) to preserve existing consumer expectations.
- A "complete" paper is defined by the presence of `structure.csv` within `outputs/<paper_id>/`; this is the minimum output from the first pipeline stage and serves as the completion sentinel for resume logic.
- Migration of existing `structure/` files to `outputs/<paper_id>/` is a one-time step, not ongoing backwards-compatibility logic built into the pipeline runtime.

## Out of Scope

- Changes to the content or schema of any output CSV file.
- Changes to download logic, LLM prompts, or column extraction behavior.
- Multi-user or networked filesystem concurrency controls beyond basic atomic file writes.
- Automatic runtime migration of old-format files (migration is a one-time scripted step only).
