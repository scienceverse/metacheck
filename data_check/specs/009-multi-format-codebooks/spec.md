# Feature Specification: Multi-Format Codebook Reading

**Feature Branch**: `009-multi-format-codebooks`
**Created**: 2026-03-17
**Status**: Draft
**Input**: User description: "currently the codebooks can only be machine readable from the start. However, many codebooks and readmes are idiotically in .doc, .docx and other types (pdf, etc) that cannot be machine readable. This needs to be implemented so a variety of codebooks can be read."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Extract Variables from DOCX Codebook (Priority: P1)

A researcher runs the codebook labelling step on a paper whose codebook is a `.docx` or `.doc` Word document. The system converts the document to plain text and extracts variable definitions from it, just as it would for a plain-text codebook.

**Why this priority**: Word documents are the most prevalent non-machine-readable codebook format in psychology research repositories. This delivers the most coverage improvement immediately.

**Independent Test**: Run `run_codebook_label()` on a paper whose only codebook file is a `.docx`; verify that the output `labels.csv` contains labelled columns matched to variables defined in that document.

**Acceptance Scenarios**:

1. **Given** a paper whose codebook is a `.docx` file containing a variable-definition table, **When** the labelling step runs, **Then** variable names and labels are extracted from the table and matched to data columns.
2. **Given** a `.doc` file (older Word format) used as a codebook, **When** the labelling step runs, **Then** the file is processed and variables are extracted without crashing the pipeline.
3. **Given** a corrupt or password-protected `.docx` file, **When** the labelling step attempts to read it, **Then** it is skipped with a warning and the remaining codebook files for that paper are still processed.

---

### User Story 2 - Extract Variables from PDF Codebook (Priority: P2)

A researcher runs the codebook labelling step on a paper whose codebook is a `.pdf` file. The system extracts the text from the PDF and feeds it through the existing variable-extraction logic.

**Why this priority**: PDF is the second most common non-machine-readable format. Many published papers deposit their codebooks as PDFs.

**Independent Test**: Run `run_codebook_label()` on a paper whose only codebook file is a `.pdf`; verify labelled output is produced.

**Acceptance Scenarios**:

1. **Given** a `.pdf` codebook containing a variable table or list, **When** the labelling step runs, **Then** variables are extracted and matched to data columns.
2. **Given** a PDF that is purely image-based (scanned) with no selectable text, **When** the system attempts extraction, **Then** extraction returns zero variables, a `parse_failed` record is written for that file, and processing continues without error.
3. **Given** a multi-page PDF codebook, **When** extraction runs, **Then** variables defined on any page are captured, not just the first page.

---

### User Story 3 - Graceful Handling of Other Rich-Text Formats (Priority: P3)

A researcher runs the labelling step on a paper whose codebook is in a less common format such as `.rtf` (Rich Text Format) or `.odt` (OpenDocument). The system attempts extraction and, if it cannot convert the file, skips it with a clear record rather than crashing.

**Why this priority**: Coverage of the long tail of formats improves robustness; graceful failure prevents entire paper runs from aborting on an unusual file.

**Independent Test**: Pass an `.rtf` codebook file through the system; verify it either produces extracted variables or produces a `parse_failed` record — no unhandled error occurs.

**Acceptance Scenarios**:

1. **Given** an `.rtf` codebook, **When** the labelling step runs, **Then** text is extracted and variable definitions are attempted; any result is acceptable as long as no error propagates to the caller.
2. **Given** a file type the system cannot convert at all, **When** attempted, **Then** a `parse_failed` record is written for that file and the next codebook file is attempted.

---

### Edge Cases

- What happens when a DOCX contains no text (only embedded images of tables)?
- How does the system handle a PDF where text extraction produces garbled characters (e.g., non-standard fonts)?
- What if a DOCX file is actually misnamed (e.g., it is a CSV renamed to `.docx`)?
- What happens when conversion of a large document produces more text than fits in a single LLM prompt chunk?
- How does the system handle `.doc` files on a machine where no conversion tool is available?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST attempt to read codebook and readme files with extensions `.doc`, `.docx`, or `.pdf` by extracting their plain-text content before attempting variable extraction.
- **FR-002**: Text extracted from non-machine-readable formats MUST be passed through the same LLM-based variable extraction logic already used for unstructured plain-text codebooks.
- **FR-003**: If a file's text cannot be extracted (conversion failure, image-only PDF, corrupt file), the system MUST record a `parse_failed` status for that file, skip it, and continue processing remaining codebook files for the same paper.
- **FR-004**: The system MUST respect the existing file-size limit; oversized files in any format are skipped before conversion is attempted.
- **FR-005**: The system MUST respect the existing maximum LLM calls limit per paper across all codebook files, regardless of format.
- **FR-006**: No changes to the output schema of `labels.csv` or `codebook_coverage.csv` are required; the format-reading change is internal to the codebook parsing layer.
- **FR-007**: Existing behaviour for all currently-supported formats (CSV, TSV, XLSX, SAV, DTA, plain-text) MUST be unchanged.

### Key Entities

- **CodebookFile**: A file classified as `codebook` or `readme` type in `structure.csv`; extended to include rich-text and binary formats (`.doc`, `.docx`, `.pdf`, `.rtf`).
- **ExtractedText**: Intermediate plain-text representation obtained by converting a rich-text or binary codebook file; used as input to the existing LLM variable-extraction step.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Papers whose only codebook files are `.docx` or `.pdf` produce non-empty `labels.csv` output in at least 70% of cases where the file contains selectable text (validates extraction correctness against known-good papers).
- **SC-002**: No paper run aborts due to an unhandled error caused by a `.doc`, `.docx`, or `.pdf` codebook file; all failures are recorded as `parse_failed` and processing continues.
- **SC-003**: All existing test cases for CSV, XLSX, SAV, DTA, and plain-text codebooks continue to pass without change.
- **SC-004**: The labelling step for a paper with a DOCX or PDF codebook completes in no more than twice the time of an equivalent plain-text codebook of the same length.

## Assumptions

- The pipeline environment has at least one text-extraction utility for DOCX and PDF available (either a system command or an already-installed R package); availability will be confirmed during planning.
- Image-only (scanned) PDFs are out of scope for OCR; returning zero variables from such files is acceptable.
- RTF and ODT support is best-effort; full parity with DOCX/PDF is not required for the initial implementation.
- The existing LLM prompt and chunking logic are sufficient for variable extraction once plain text is obtained; no new LLM prompt is needed.
- File-size limits are applied before any conversion attempt; very large binary documents are already excluded.
