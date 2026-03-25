# Feature Specification: Validation GUI

**Feature Branch**: `020-validation-gui`
**Created**: 2026-03-23
**Status**: Draft
**Input**: User description: "read validation-gui-spec.md. Make a proper spec for it"

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Label files for a paper (Priority: P1)

An annotator opens the tool, selects a paper, and works through its files one at a time. For each file they see the filename, its path in the repository folder tree, a readable preview of its contents, and the machine-generated labels as a starting point. They press a number key to assign the correct file type, optionally adjust the experiment group and rawness flag, then save and move to the next file. When they close the tool and return later, their work is exactly where they left it.

**Why this priority**: This is the entire purpose of the tool. Without the ability to label and persist labels the application has no value.

**Independent Test**: Can be fully tested by running the tool against a single paper with a known `structure.csv`, completing a full labelling pass, and verifying the ground-truth CSV contains the correct rows with correct values.

**Acceptance Scenarios**:

1. **Given** a paper whose `structure.csv` exists and contains 19 files, **When** the annotator selects that paper and labels all files, **Then** the ground-truth file contains exactly 19 rows, each with `type_gt`, `group_gt`, `is_raw_gt`, `validated_at`, and `annotator` populated.
2. **Given** a partially labelled paper (7 of 19 files saved), **When** the annotator reopens the tool and selects that paper, **Then** the 7 already-saved files are marked validated in the file list and the tool positions focus on the first unlabelled file.
3. **Given** the annotator assigns type `code` to a file, **When** they save, **Then** `is_raw_gt` is stored as `FALSE` regardless of the machine prediction.

---

### User Story 2 — Keyboard-driven labelling without mouse (Priority: P2)

An annotator labels files using only the keyboard: number keys `1`–`8` to set the file type, `R` to toggle rawness, `G` to jump to the group field, `⌘↩` to save and advance, `Tab` to skip, and `⌘[` to go back. They never need to reach for the mouse to complete a standard labelling session.

**Why this priority**: Annotation speed is the primary usability constraint. A mouse-driven workflow is significantly slower per file. Full keyboard coverage is what makes the tool practical for annotating hundreds of files.

**Independent Test**: Can be tested by completing a full labelling session touching all three label fields using only keystrokes, and verifying every action completes correctly.

**Acceptance Scenarios**:

1. **Given** focus is not inside a text field, **When** the annotator presses `3`, **Then** the type selection changes to `codebook` instantly.
2. **Given** `type` is `data`, **When** the annotator presses `R`, **Then** `is_raw` toggles between `TRUE` and `FALSE`.
3. **Given** `type` is `code`, **When** the annotator presses `R`, **Then** nothing happens (toggle is disabled for non-data types).
4. **Given** focus is anywhere in the form, **When** the annotator presses `G`, **Then** keyboard focus moves to the group text input.
5. **Given** the current labels are set, **When** the annotator presses `⌘↩`, **Then** the row is saved and the next unlabelled file is shown.
6. **Given** the annotator does not want to label the current file, **When** they press `Tab`, **Then** the tool advances to the next file without writing any row to the ground-truth file.

---

### User Story 3 — Understand file context before labelling (Priority: P2)

Before assigning labels the annotator can see a scrollable preview of the file's contents (first rows of a CSV, first lines of a script, extracted text of a PDF, inline thumbnail for images, etc.), the machine prediction for all three fields, and a folder-tree view of the entire repository with the current file highlighted. They also see the sibling files in the same folder.

**Why this priority**: Without meaningful context the annotator must open files externally, which defeats the purpose of the tool. Context quality is the second most important feature after persistence.

**Independent Test**: Can be tested by loading a paper with mixed file types (CSV, R script, PDF, image) and verifying that each type surfaces a relevant, readable preview inside the tool.

**Acceptance Scenarios**:

1. **Given** a `.csv` file is selected, **When** the context panel loads, **Then** the first 50 lines of raw text are displayed in a scrollable area.
2. **Given** a `.pdf` file is selected, **When** the context panel loads, **Then** at least 500 characters of extracted text are displayed.
3. **Given** a `.jpg` file is selected, **When** the context panel loads, **Then** an inline image thumbnail is displayed.
4. **Given** a file that cannot be previewed is selected, **When** the context panel loads, **Then** a descriptive error message is shown and the tool does not crash.
5. **Given** any file is selected, **When** the context panel loads, **Then** the machine-predicted `type`, `group`, and `is_raw` are visible, and any difference between the annotator's current selection and the prediction is visually indicated.

---

### User Story 4 — Resume a session after closing (Priority: P1)

An annotator completes part of a labelling pass, closes the tool, and returns the next day. Their saved labels are intact, the file list correctly shows which rows are validated and which are not, and the tool positions them on the first unvalidated file.

**Why this priority**: Without reliable persistence the tool cannot be used for real annotation work spread across multiple sessions.

**Independent Test**: Can be tested by saving labels for a subset of files, terminating the application entirely, reopening, selecting the same paper, and verifying that previously saved labels are present and accurate.

**Acceptance Scenarios**:

1. **Given** labels have been saved for 5 of 19 files, **When** the annotator closes and reopens the tool, **Then** those 5 rows are present in the ground-truth file and marked validated in the file list.
2. **Given** a previously validated file is selected, **When** the label controls load, **Then** they show the annotator's saved values, not the machine prediction.

---

### Edge Cases

- What happens when a file listed in `structure.csv` no longer exists on disk? The preview area must show a "file not found" message; labelling must still be possible.
- What happens when the ground-truth file contains unexpected or malformed rows? The tool must load valid rows and warn about unreadable rows rather than crashing.
- What happens when a file is very large (e.g. several hundred MB)? The preview must read only the first N lines or bytes and never load the full file into memory.
- What happens when the annotator types a novel group value not in the autocomplete list? The free-text entry must be accepted as-is.
- What happens when `is_raw` is `TRUE` for a non-data file in a pre-existing ground-truth file? On load the flag must be silently corrected to `FALSE`.
- What happens for sentinel rows (`is_sentinel = TRUE`)? The context panel must clearly explain that this row represents a collapsed folder of many files and display the estimated file count.

---

## Requirements *(mandatory)*

### Functional Requirements

**Paper selection**

- **FR-001**: The tool MUST present a list of all papers for which a `structure.csv` exists, selectable at startup.
- **FR-002**: The tool MUST load the selected paper's file list and any existing ground-truth rows without requiring the annotator to provide file paths manually.

**File list and navigation**

- **FR-003**: The file list MUST display every row from `structure.csv` for the selected paper.
- **FR-004**: Each file list entry MUST show its validation status: validated, skipped (this session, not saved), or unvisited.
- **FR-005**: The annotator MUST be able to jump to any file by clicking its entry in the file list.
- **FR-006**: After a save action, the tool MUST advance to the next unvalidated file automatically.
- **FR-007**: The annotator MUST be able to return to the previous file via `⌘[` or a back button.
- **FR-008**: The annotator MUST be able to skip the current file via `Tab` or a skip button without saving anything.

**Label controls**

- **FR-009**: The tool MUST present the eight file types as individually activatable buttons with visible key labels: `[1] data`, `[2] code`, `[3] codebook`, `[4] supplemental`, `[5] doc`, `[6] readme`, `[7] asset`, `[8] other`.
- **FR-010**: Pressing `1`–`8` when focus is not inside a text input MUST immediately activate the corresponding type button.
- **FR-011**: The tool MUST provide a text input for `group` with autocomplete suggestions from the current paper's existing groups plus `other` and `na`; free-text entry of novel values MUST be accepted.
- **FR-012**: Pressing `G` when focus is not inside a text input MUST move focus to the group text input.
- **FR-013**: The tool MUST provide a toggle for `is_raw` activatable by pressing `R` when focus is not inside a text input.
- **FR-014**: The `is_raw` toggle MUST be visually disabled and its effective value forced to `FALSE` whenever `type` is not `data`.
- **FR-015**: All three label controls MUST be pre-filled with the machine prediction when a file is visited for the first time, and with the annotator's previously saved values when returning to an already-validated file.
- **FR-016**: The tool MUST visually distinguish the annotator's current type selection from the machine prediction when they differ.

**Context panel**

- **FR-017**: The context panel MUST show the file's full relative path, file extension, and file size on disk.
- **FR-018**: The context panel MUST show a scrollable preview of the file's contents appropriate to its extension (text preview for tabular and script files; extracted text for PDFs and DOCX; inline thumbnail for images; member list for archives; class/structure summary for R data objects).
- **FR-019**: The context panel MUST show the machine-predicted `type`, `group`, and `is_raw` values.
- **FR-020**: The context panel MUST show a folder-tree view of the entire paper's repository with the current file highlighted.
- **FR-021**: The context panel MUST list sibling files in the same folder as the current file.
- **FR-022**: For sentinel rows, the context panel MUST prominently indicate that the row represents a collapsed folder and show the file count.
- **FR-023**: If a file preview fails for any reason, the context panel MUST display a descriptive error message without crashing or blocking labelling.

**Persistence**

- **FR-024**: Saving a file's labels MUST write or update exactly one row for that file in `ground_truth/<paper_id>.csv`, recording `type_gt`, `group_gt`, `is_raw_gt`, `validated_at` (ISO-8601 timestamp), and `annotator`.
- **FR-025**: The ground-truth file MUST be written immediately on each individual save action; no batching.
- **FR-026**: On startup, the tool MUST read the existing ground-truth file (if present) and restore all saved labels and their validation status.
- **FR-027**: The annotator's name or initials MUST be configurable at startup and applied to every row written in that session.

**Progress**

- **FR-028**: The tool MUST display a `validated / total` count in a prominent, always-visible location.

### Key Entities

- **Paper**: Identified by a character `paper_id` (leading zeros preserved). Has one machine-generated `structure.csv` and zero or one annotator-written ground-truth file.
- **Structure row**: Represents one file in the paper's repository. Has relative path, extension, machine-predicted `type`, `group`, `is_raw`, and an `is_sentinel` flag.
- **Ground-truth row**: The annotator's verified labels for one structure row. Joins to the structure row via `rel_path`. Carries a save timestamp and annotator identifier.
- **Session**: One continuous annotation period. Tracks skipped files in memory only; skip status is not persisted between sessions.

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: An annotator can label a straightforward file (review context, assign type, group, rawness, save) in under 20 seconds.
- **SC-002**: Zero label rows are lost or corrupted when the tool is closed mid-session and reopened.
- **SC-003**: A paper with 50 files can be labelled in a single session without the tool becoming unresponsive at any point.
- **SC-004**: A file preview loads within 3 seconds for any supported file type smaller than 100 MB.
- **SC-005**: All keyboard actions specified in the requirements work correctly without any mouse interaction.
- **SC-006**: Ground-truth files produced for multiple papers can be concatenated into a single dataset with no schema conflicts or duplicate rows per `paper_id` + `rel_path`.

---

## Assumptions

- A single annotator works on the tool at a time; no concurrent access to the same ground-truth file is expected.
- All paper data has already been downloaded to disk by the pipeline; the tool does not trigger any downloads.
- The annotator is on a macOS machine with a standard laptop keyboard (no numpad).
- File preview reads only the first N lines or bytes; the full file is never loaded into memory.
- `paper_id` is always a character string and may contain leading zeros.
- The `ground_truth/` directory is version-controlled and treated as a scientific dataset, not a pipeline output.

---

## Out of Scope

- Multi-annotator reconciliation or inter-rater reliability reporting.
- Validation of `columns.csv` fields (column types, statistics).
- Cross-paper batch review or comparison.
- Export to any format other than CSV.
- Triggering or re-running the pipeline from within the tool.
- Any network access; the tool is entirely local.
