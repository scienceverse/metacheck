# Feature Specification: Remove Redundant Stage-1 Column Extraction

**Feature Branch**: `011-merge-columns-output`
**Created**: 2026-03-17
**Status**: Draft

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Running the Pipeline Produces Rich Column Data for All Papers (Priority: P1)

A researcher processes a paper through the full pipeline and inspects `columns.csv`. Today, if stage 1 (`1_data_label.R`) runs after stage 0, the file contains only column names — losing all statistics, type classifications, and sample values that stage 0 computed. After this fix, `columns.csv` always contains the rich stage-0 output for every paper that stage 0 successfully processed. The separate stage-1 script and its bulk runner no longer exist.

**Why this priority**: 47 of 48 papers currently have their rich stage-0 statistics silently destroyed by stage 1. This is the primary data-loss bug.

**Independent Test**: Verify that `1_data_label.R` and `run_label_bulk.R` no longer exist. Run stage 0 on paper `0956797615620784`. Confirm `columns.csv` has 23 columns including `col_type`, `sample_values`, `mean`, `sd`. Run stage 2 — it should still work correctly.

**Acceptance Scenarios**:

1. **Given** a paper processed by stage 0, **When** the pipeline finishes, **Then** `columns.csv` contains `col_type`, `sample_values`, and statistics columns — not just a bare column-name list.
2. **Given** the pipeline codebase, **When** a developer looks for `1_data_label.R` or `run_label_bulk.R`, **Then** those files are gone and no other script references them.
3. **Given** a paper already in `label_summary.csv` with a thin `columns.csv`, **When** stage 0 is re-run for that paper, **Then** `columns.csv` is replaced with the rich version.

---

### User Story 2 — The 47 Papers With Thin columns.csv Are Recovered (Priority: P2)

47 papers currently have only thin `columns.csv` files (column names only) because stage 1 destroyed the stage-0 output. After stage 0 is re-run for those papers, all 47 have rich `columns.csv` files that downstream labelling can use.

**Why this priority**: The existing data is recoverable by re-running stage 0 — no re-download needed since the data files are already on disk.

**Independent Test**: After re-running stage 0 for all thin-columns papers, count the papers where `columns.csv` contains `col_type`. All 47 should flip from thin to rich.

**Acceptance Scenarios**:

1. **Given** a paper currently with a thin `columns.csv` and data already on disk, **When** stage 0 is re-run for it, **Then** `columns.csv` is overwritten with the rich 23-column version.
2. **Given** the full set of processed papers, **When** the recovery is complete, **Then** every paper with a `structure.csv` and at least one data file has a rich `columns.csv` (or no `columns.csv` at all if stage 0 genuinely found no readable data).

---

### Edge Cases

- What about the 1 paper where stage 0 reported 0 columns? → Stage 0 re-run will again find 0 columns; no `columns.csv` will be written; this is the correct outcome (nothing to label).
- What about the 8 papers with `structure.csv` but no `columns.csv` at all? → Out of scope; these need stage 0 re-run regardless and are not affected by removing stage 1.
- What if `run_label_bulk.R` is referenced by other scripts? → Check all `.R` files for references before deleting; no changes to any other pipeline stage.
- What if a user has `run_label_bulk.R` in a scheduled job? → Documented in progress notes; out of scope for automated handling.

---

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: `1_data_label.R` MUST be removed from the repository.
- **FR-002**: `run_label_bulk.R` MUST be removed from the repository.
- **FR-003**: No remaining pipeline script MUST reference or `source()` either deleted file.
- **FR-004**: Stage 0 (`0_index.R`) and stage 2 (`2_codebook_label.R`) MUST continue to work identically after the deletion.
- **FR-005**: A re-run script or inline procedure MUST exist to re-process the 47 papers that currently have only a thin `columns.csv`, restoring their rich stage-0 output.
- **FR-006**: `label_summary.csv` (written by `run_label_bulk.R`) is no longer needed as an active artifact; it should be noted as obsolete.

### Key Entities

- **Rich columns file**: 23-column output from stage 0 — `col_type`, `sample_values`, statistics. The sole authoritative source for column metadata going forward.
- **Thin columns file**: 5-column output from stage 1 — column names only. No longer produced by the pipeline after this change.
- **`label_summary.csv`**: Resume log for the now-deleted stage-1 bulk runner. Obsolete after this change.

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `1_data_label.R` and `run_label_bulk.R` no longer exist in the repository after the change.
- **SC-002**: Zero remaining references to `1_data_label.R` or `run_label_bulk.R` in any `.R` file.
- **SC-003**: After re-running stage 0 for thin-columns papers, at least 46 of 47 target papers have a `columns.csv` containing `col_type` (one paper legitimately has 0 columns from stage 0).
- **SC-004**: Stage 2 produces valid `labels.csv` output for `0956797615620784` after this change — no regressions.

---

## Assumptions

- The 47 papers with thin `columns.csv` all have their data files already present on disk (no re-download required for the recovery step).
- Re-running stage 0 for a paper overwrites the existing `columns.csv` in place — the existing thin file is replaced by the rich version.
- `label_summary.csv` is not consumed by any downstream stage and can be left in place as a historical artifact without causing harm.
- No external scripts or scheduled jobs outside this repository reference `1_data_label.R` or `run_label_bulk.R`.
