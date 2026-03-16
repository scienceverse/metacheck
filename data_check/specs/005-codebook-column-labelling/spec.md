# Feature Specification: Codebook Column Labelling

**Feature Branch**: `005-codebook-column-labelling`
**Created**: 2026-03-16
**Status**: Draft
**Input**: User description: "The next step of the pipeline is reading the detected codebooks/readme files and labelling detected data files accordingly. This implies that as many data columns need to be accurately labelled by a codebook. It is important to keep in mind that sometimes there might be an experiment level difference; two experiments might have the same variable but not the same meaning. This is an integral part of the workflow needed before rebuilding the dataset into a standard. All codebook variables should be matched, but it could be that some dataset variables are not labelled."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Label Columns from Codebook (Priority: P1)

A researcher runs the labelling step on a paper that has a structured codebook and one or more data files. The system reads the codebook, extracts variable definitions, and attaches the appropriate label/description to each matching column in the data files. The result is an enriched column list where every variable that appears in both the codebook and the data is described.

**Why this priority**: This is the core deliverable of the feature. Without it, subsequent standardisation steps cannot proceed with semantic context.

**Independent Test**: Can be tested by running the labelling step on a single paper with a known codebook and verifying that matched columns carry correct labels in the output.

**Acceptance Scenarios**:

1. **Given** a paper with a codebook defining variables `age`, `gender`, `rt` and a data file with columns `age`, `gender`, `rt`, `subj_id`, **When** the labelling step runs, **Then** `age`, `gender`, and `rt` each receive the label/description from the codebook, and `subj_id` is marked as unlabelled.
2. **Given** a paper whose codebook contains a variable not present in any data file, **When** the labelling step runs, **Then** that variable is recorded as unmatched-in-data in the coverage report but does not cause an error.
3. **Given** a paper with no codebook or readme file classified in the pipeline, **When** the labelling step runs, **Then** all columns are marked unlabelled and a `no_codebook` status is recorded.

---

### User Story 2 - Experiment-Scoped Labelling (Priority: P2)

A researcher runs the labelling step on a paper containing multiple experiments. Variable `condition` means "reward/punishment" in Experiment 1 and "low/medium/high" in Experiment 2. The system correctly associates each column's label with its experiment context so the two definitions are never mixed.

**Why this priority**: Cross-experiment label pollution would silently corrupt downstream standardisation. This is a correctness requirement, not a convenience feature.

**Independent Test**: Can be tested with a synthetic two-experiment paper where a shared variable name has different codebook definitions per experiment; verify the output attaches the correct definition to each experiment's columns.

**Acceptance Scenarios**:

1. **Given** a codebook that defines `condition` differently for Experiment 1 and Experiment 2, **When** the labelling step runs, **Then** each data file's `condition` column carries the label appropriate to its experiment, not a merged or conflated definition.
2. **Given** a data file that cannot be unambiguously assigned to a single experiment, **When** the labelling step runs, **Then** the column is labelled with all candidate definitions and flagged as `ambiguous_experiment`.

---

### User Story 3 - Coverage Reporting (Priority: P3)

After labelling, a researcher can inspect how thoroughly the codebook covered the data: how many codebook variables were matched, how many data columns remain unlabelled, and whether any codebook variables were not found in the data.

**Why this priority**: Coverage metrics drive trust in the standardisation step and reveal data quality issues in the source papers.

**Independent Test**: Can be tested by checking the summary output for a known paper against manually computed match counts.

**Acceptance Scenarios**:

1. **Given** a completed labelling run, **When** the output is inspected, **Then** it reports: total codebook variables, number matched to data columns, number of data columns labelled, and number of data columns unlabelled.
2. **Given** a paper where 0 codebook variables match any data column, **When** labelling completes, **Then** coverage is reported as 0% and a `low_coverage` flag is set, but processing continues without error.

---

### Edge Cases

- What happens when the same variable name appears in multiple codebooks within a single paper (conflicting definitions)?
- How does the system handle a codebook written as a free-text README with no formal table structure?
- What happens when a data column name differs from the codebook name only in case or whitespace (e.g., `SubjectID` vs `subject_id`)?
- How does the system behave when a codebook file is too large or corrupt to parse?
- What happens when experiment assignment cannot be inferred from file names, folder structure, or codebook headings?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST read all files classified as `codebook` or `readme` type in the paper's structure output and attempt to extract variable definitions from each.
- **FR-002**: System MUST extract, at minimum, a variable name and a human-readable label or description for each codebook entry.
- **FR-003**: System MUST match codebook variable names to column names in each data file, using case-insensitive and whitespace-normalised comparison.
- **FR-004**: System MUST attach the matched label/description to the corresponding column in the labelling output.
- **FR-005**: System MUST scope variable definitions to experiment context when the codebook distinguishes between experiments, preventing cross-experiment label assignment.
- **FR-006**: System MUST mark data columns that have no matching codebook variable as `unlabelled`; this is an expected outcome, not an error.
- **FR-007**: System MUST record every codebook variable as either `matched` (found in at least one data file) or `unmatched_in_data` (not found in any data file).
- **FR-008**: System MUST produce a per-paper coverage summary: count of codebook variables, matched variables, labelled data columns, and unlabelled data columns.
- **FR-009**: System MUST record a `no_codebook` status for papers where no codebook or readme file is available, and leave all columns unlabelled.
- **FR-010**: System MUST handle codebook parsing failures gracefully — if a codebook file cannot be parsed, it is skipped and the failure is recorded; other codebooks for the same paper are still attempted.
- **FR-011**: When a variable name appears in multiple codebooks for the same paper with conflicting definitions, the system MUST flag the column label as `conflicting_definition`, attach all candidate definitions with their source file attribution, and leave final resolution to a later step; no definition is silently discarded.

### Key Entities

- **CodebookVariable**: A variable defined in a codebook file, with a name, label/description, source file, and optional experiment scope.
- **ColumnLabel**: The association between a data column (identified by paper, file, and column name) and one or more CodebookVariables; carries match status (`labelled`, `unlabelled`, `ambiguous_experiment`).
- **LabellingResult**: Per-paper output capturing all ColumnLabels and the coverage summary; feeds into the downstream standardisation step.
- **ExperimentContext**: A named grouping (e.g., "Experiment 1", "Study 2") used to scope CodebookVariables and data files when a paper contains multiple experiments.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Every codebook variable that matches a column in a data file receives a label; no matching pair is silently omitted.
- **SC-002**: Experiment-scoped variables are never assigned to a column from a different experiment context; cross-experiment labelling rate is 0%.
- **SC-003**: The labelling step runs without manual intervention for any paper that has already completed the structure and column-extraction phases of the pipeline.
- **SC-004**: Coverage output is produced for every paper processed, including those with no codebook (reported as 0% coverage) and those with partial coverage.
- **SC-005**: For papers with a well-structured codebook, at least 80% of codebook-defined variables are matched to a data column (validates parser correctness against known-good papers).

## Assumptions

- Codebook and readme files are already identified and their paths are available from the paper's `_structure.csv` output (col_type = `codebook` or `readme`).
- The LLM available in the existing pipeline can be used to parse unstructured codebook text when rule-based extraction fails.
- Experiment context can be inferred from codebook section headings, file names, or folder structure; human disambiguation is not available during automated runs.
- Column names in data files and variable names in codebooks may differ in case or whitespace but are otherwise lexically similar (no semantic synonym matching is required at this stage).
- Output is appended to or joined with the existing `_columns.csv` schema by adding label-related columns; the schema for these columns will be defined during planning.
