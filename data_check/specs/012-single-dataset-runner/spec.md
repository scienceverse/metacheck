# Feature Specification: Single Dataset Runner

**Feature Branch**: `012-single-dataset-runner`
**Created**: 2026-03-17
**Status**: Draft
**Input**: User description: "the end goal is one specific script that will do the entire process for a single dataset (id.) Make the first version of this that for now runs the process for an random dataset in the id list"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Run Full Pipeline on One Random Dataset (Priority: P1)

A researcher wants to run the entire data-check pipeline end-to-end on a single paper to inspect outputs or test behavior. In this first version, the script selects a random paper ID from the known ID list and processes it fully: downloading the repository, classifying files, extracting columns, and labelling columns from codebook — all in one invocation.

**Why this priority**: This is the core requirement. All future functionality (e.g., targeting a specific ID) builds on this single-paper, full-pipeline capability.

**Independent Test**: Run the script with no arguments. Verify that one paper's `outputs/<paper_id>/` directory is created containing `structure.csv`, `columns.csv`, and `labels.csv`.

**Acceptance Scenarios**:

1. **Given** a populated ID list exists, **When** the script is run with no arguments, **Then** it selects a random ID, runs all pipeline stages (index + codebook labelling), and writes all output files to `outputs/<paper_id>/`.
2. **Given** the selected paper has already been processed (outputs exist), **When** the script is run again, **Then** it still completes without error (re-processing is acceptable in this version).
3. **Given** the script completes successfully, **When** the outputs are inspected, **Then** `structure.csv`, `columns.csv`, and `labels.csv` are all present and non-empty.

---

### Edge Cases

- What happens when the randomly selected paper has no downloadable data (`no_links`, `empty_repo`)? The script should surface the error clearly and exit gracefully rather than crashing.
- What happens when the ID list is missing or empty? The script should stop with a clear message.
- What happens when a pipeline stage fails mid-way (e.g., `run_index` succeeds but `run_codebook_label` errors)? The partial outputs should be preserved and the error reported.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The script MUST source both the file-classification stage and the codebook-labelling stage so the full pipeline is available in a single run.
- **FR-002**: The script MUST read the available paper ID list (same source used by the bulk runner) and select one ID at random.
- **FR-003**: The script MUST run the file-classification stage for the selected paper ID and capture its result.
- **FR-004**: The script MUST run the codebook-labelling stage for the same paper ID after the file-classification stage completes.
- **FR-005**: The script MUST print the selected paper ID and the status of each stage to the console so progress is visible.
- **FR-006**: The script MUST exit gracefully with an informative message when a known pipeline error occurs (`no_links`, `download_failed`, `empty_repo`, `too_large`).
- **FR-007**: Paper IDs MUST be treated as character strings throughout — no numeric coercion, no leading-zero stripping.

### Key Entities

- **Paper ID**: A character string uniquely identifying a psychology research paper (e.g. `"0956797615569001"`). Must never be coerced to numeric.
- **Pipeline result**: The structured output from the file-classification stage, containing file counts, column counts, error status, and timing.
- **Label result**: The structured output from the codebook-labelling stage, containing label status and coverage counts.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Running the script produces output files for the selected paper in all cases where the paper's data is accessible.
- **SC-002**: The script completes (success or graceful failure) without throwing an unhandled error in all tested cases.
- **SC-003**: The selected paper ID is visible in console output so the researcher knows which dataset was processed.
- **SC-004**: A single command is sufficient to run the entire pipeline with no manual steps between stages.

## Assumptions

- The ID list is discovered the same way the bulk runner does it (scanning the local data directory for paper ID folders).
- Re-running on an already-processed paper is acceptable in this first version (no skip logic required yet).
- The script will be named `run_single.R` and placed at the top level of `data_check/`.
- No new packages are introduced; only existing pipeline scripts are sourced.
