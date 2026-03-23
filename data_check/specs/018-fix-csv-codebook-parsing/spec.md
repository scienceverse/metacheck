# Feature Specification: Fix CSV Codebook Parsing Robustness

**Feature Branch**: `018-fix-csv-codebook-parsing`
**Created**: 2026-03-23
**Status**: Draft
**Input**: User description: "the csv loading for the codebooks now always assumes that the csv output is clean it very often is not, leading to empty csv files. Look for csv codebooks already labelled as such in results and find out why the current method goes wrong. Then either implement a fix, or make it so all csv's go through the llm if that is required."

## Background

The pipeline classifies files as codebooks, then attempts to extract variable-label pairs from them. For CSV codebooks, a structured parsing path runs first: it reads the file with a delimiter sniffer, detects variable-name and description columns by header regex, and returns the pairs. If that fails, it falls back to LLM chunk parsing.

**Known failure modes identified from real data:**
1. **Multi-level / merged headers** — some CSVs use a hierarchical header spanning rows 1–2, with actual column definitions starting on row 3+. `read.delim()` reads only the first row as headers, making column detection fail silently.
2. **Non-standard header names** — variable/description columns use names outside the current regex patterns (e.g. "variable name" with a space, "var_label", "Question").
3. **Empty description cells** — rows with a valid variable name but blank description are silently dropped, producing a shorter-than-expected output.
4. **Undetected encoding issues** — unlike the main data-reading path, CSV codebook loading does not attempt a latin1 fallback, silently truncating or failing on non-UTF-8 files.
5. **Delimiter detection from blank/comment rows** — `sniff_delimiter()` probes the first non-blank line, but some CSVs open with comment or metadata rows before the actual header.

When structured parsing yields zero rows, the LLM fallback runs; if that also fails, the codebook is silently skipped and all columns remain unlabelled.

## User Scenarios & Testing

### User Story 1 - Messy CSV codebooks produce labels instead of empty output (Priority: P1)

A researcher runs the pipeline against a paper that has a CSV codebook with a multi-level header (e.g. two header rows before data rows begin). Currently, the pipeline produces an empty `labels.csv`. After this fix, the pipeline extracts variable–label pairs correctly.

**Why this priority**: The most common failure mode. Every paper with an irregular CSV codebook currently produces zero labels — the core deliverable of the pipeline stage.

**Independent Test**: Run the codebook labelling step on a paper whose CSV codebook uses multi-level headers. Verify `labels.csv` is non-empty and `codebook_coverage.csv` shows matched variables.

**Acceptance Scenarios**:

1. **Given** a CSV codebook with two header rows before column definitions begin, **When** the codebook labelling step runs, **Then** variable–label pairs are extracted from the correct rows and written to `labels.csv`.
2. **Given** a CSV codebook where description column is named "Question" instead of "label/description", **When** the step runs, **Then** the column is still detected and labels are extracted.
3. **Given** a CSV codebook with valid variable names but blank description cells, **When** the step runs, **Then** the output includes all rows with non-empty variable names, and blank-description rows are included with an empty label rather than silently dropped.

---

### User Story 2 - Structured parsing failure routes cleanly to LLM, not silent skip (Priority: P2)

When structured CSV parsing genuinely cannot produce any rows — because the layout is truly unrecognisable — the pipeline falls back to LLM parsing and records why it did so. The researcher can see in the coverage report whether labels came from structured or LLM extraction.

**Why this priority**: Visibility into failure mode is a prerequisite for any further improvement. Without it, silent failures are invisible in the output.

**Independent Test**: Run on a CSV codebook that has no recognisable header structure. Verify the LLM fallback runs (not a silent skip), and that `codebook_coverage.csv` records the parse method.

**Acceptance Scenarios**:

1. **Given** a CSV codebook with no recognisable header row, **When** structured parsing yields zero rows, **Then** LLM chunk parsing runs and its output is used.
2. **Given** structured parsing succeeds on a CSV, **When** results are written, **Then** `codebook_coverage.csv` records `parse_method = "structured"`.
3. **Given** structured parsing fails and LLM fallback runs, **When** results are written, **Then** `codebook_coverage.csv` records `parse_method = "llm"`.

---

### User Story 3 - Encoding-resilient CSV loading (Priority: P3)

A CSV codebook saved in Windows-1252 / latin1 encoding (common in older SPSS/Excel exports) is read without garbled characters or silent failure.

**Why this priority**: Encoding issues are a secondary failure mode — less common than header structure problems, but silently corrupts the few rows it does affect.

**Independent Test**: Run on a paper with a latin1-encoded CSV codebook. Verify labels contain correct characters.

**Acceptance Scenarios**:

1. **Given** a CSV codebook encoded in latin1, **When** UTF-8 reading fails or produces replacement characters, **Then** the pipeline retries with latin1 encoding and produces correct labels.

---

### Edge Cases

- CSV codebook has only one column (no description column at all) → fall through to LLM.
- CSV has headers on row 1 but all data rows are blank → produce empty output, log warning.
- CSV delimiter sniffing reads a metadata comment row as the first line → sniffer should skip comment/blank rows before probing for delimiter.
- CSV codebook is larger than `MAX_CODEBOOK_FILE_MB` → existing size-check skip applies unchanged.
- Paper has multiple CSV codebook files → each file is parsed independently; failure on one does not prevent others from being processed.

## Requirements

### Functional Requirements

- **FR-001**: The CSV codebook parser MUST attempt to detect and skip leading metadata/comment rows before looking for the header row, so that delimiter sniffing and column detection operate on the actual header.
- **FR-002**: Column header detection MUST extend its recognition patterns to include common variants currently missed (e.g. "variable name", "var_label", "question", "item").
- **FR-003**: The parser MUST search the first N rows (up to a configurable lookahead, default 5) for a row that matches the variable/description column pattern, to handle multi-level headers.
- **FR-004**: Rows with a recognised variable name but an empty description cell MUST be retained in the output with an empty label string, not silently dropped.
- **FR-005**: When UTF-8 reading produces parse errors or replacement characters, the parser MUST retry with latin1 encoding before giving up on structured parsing.
- **FR-006**: When structured parsing yields zero rows, the pipeline MUST fall through to LLM chunk parsing (existing behaviour), rather than returning an empty result without attempting LLM.
- **FR-007**: The `codebook_coverage.csv` output MUST record a `parse_method` field (`"structured"` or `"llm"`) indicating which path was used for each codebook file.
- **FR-008**: All changes MUST be confined to the CSV parsing path; PDF and DOCX codebook parsing are out of scope.

### Key Entities

- **CSV Codebook**: A `.csv` (or `.tsv`) file classified as a codebook. Contains one row per variable, with columns for variable name and human-readable label/description.
- **Parse Method**: The strategy used to extract variable–label pairs from a codebook file — either rule-based structured parsing or LLM chunk parsing.
- **Codebook Coverage Record**: The per-paper, per-codebook-file output row in `codebook_coverage.csv`, recording which variables were matched and how the codebook was parsed.

## Success Criteria

### Measurable Outcomes

- **SC-001**: Papers that previously produced empty `labels.csv` due to irregular CSV codebook structure produce non-empty labels after the fix, with no regression on currently-passing papers.
- **SC-002**: The share of CSV codebooks successfully parsed via the structured path (not requiring LLM fallback) increases compared to the baseline measured before this fix.
- **SC-003**: Every codebook-labelling run records `parse_method` in `codebook_coverage.csv`, with no rows having a missing or unknown value for that field.
- **SC-004**: No paper that currently produces correct labels regresses to empty output after the change.

## Assumptions

- The LLM fallback path for CSV codebooks already works correctly when invoked; this feature improves when and how structured parsing is attempted, not the LLM path itself.
- "Multi-level header" means at most 5 rows of preamble/header before the actual variable definitions — a larger lookahead is out of scope.
- Delimiter sniffing skipping comment rows means lines starting with `#` or blank lines — other comment formats are out of scope.
- The `parse_method` field is added to `codebook_coverage.csv` as a new column; downstream consumers that read this file should tolerate new columns gracefully.
