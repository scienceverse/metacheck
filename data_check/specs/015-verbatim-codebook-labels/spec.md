# Feature Specification: Verbatim Codebook Label Extraction

**Feature Branch**: `015-verbatim-codebook-labels`
**Created**: 2026-03-19
**Status**: Draft
**Input**: User description: "currently, the codebook label extracted is not a verbatim text by the llm, but an interpreted one. I want this to be as close to verbatim as possible, so it is as accurate as before"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - LLM Extracts Verbatim Labels (Priority: P1)

A researcher runs the codebook labeling pipeline on a paper with an unstructured codebook (PDF or DOCX). The LLM parses variable definitions and populates the `label` field in the output. Currently the LLM writes a "concise human-readable description" — a paraphrase or summary — rather than copying the exact wording from the codebook. After this fix, the `label` field must contain the exact wording as it appears in the codebook source text.

**Why this priority**: Label accuracy is the primary purpose of this pipeline stage. Any paraphrase risks changing meaning, especially for psychology research where measurement precision matters. Verbatim extraction is the most accurate possible outcome.

**Independent Test**: Provide a codebook with known variable descriptions and compare the pipeline's `label` output against the source text character-by-character.

**Acceptance Scenarios**:

1. **Given** a codebook containing `rt — reaction time in milliseconds`, **When** the LLM parses it, **Then** `label` is `"reaction time in milliseconds"` (not `"Time taken to respond"` or similar paraphrase).
2. **Given** a codebook where a variable description spans multiple words or includes punctuation, **When** extracted, **Then** the label preserves that phrasing exactly including punctuation and capitalisation.
3. **Given** a codebook where no description text appears for a variable (name only), **When** extracted, **Then** the variable is omitted from results (as per existing rules).

---

### User Story 2 - Structured Codebook Extraction Unaffected (Priority: P2)

When the codebook is a structured file (CSV, Excel, SPSS, Stata), labels are read directly from the label column without passing through the LLM parse prompt. This path already produces verbatim output and must not be altered.

**Why this priority**: Regression risk — structured extraction already works correctly and must remain unchanged.

**Independent Test**: Run the pipeline on a paper with a structured CSV codebook and verify `label` values match the raw cell contents of the label column.

**Acceptance Scenarios**:

1. **Given** a structured CSV codebook with a description column, **When** the pipeline runs, **Then** `label` values are identical to the raw cell contents (unchanged from current behaviour).

---

### Edge Cases

- What if the codebook description contains line breaks or extra whitespace? Reasonable whitespace normalisation (collapse runs of whitespace, trim leading/trailing) is acceptable; content words must not be changed.
- What if the LLM cannot find a description text but invents one? The fix should eliminate this — if no source text exists, the variable should be omitted rather than the label fabricated.
- What if the description is very long (multiple sentences)? The full text should be preserved verbatim; no truncation is required by this feature.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The LLM parse prompt MUST instruct the model to copy the variable description text verbatim from the codebook source, not paraphrase or summarise it.
- **FR-002**: The prompt MUST explicitly forbid the LLM from rephrasing, summarising, or inferring label text that does not appear in the source material.
- **FR-003**: The prompt MUST instruct the LLM to omit a variable entirely if no description text is present in the source, rather than fabricating one.
- **FR-004**: Structured codebook extraction (CSV, Excel, SPSS, Stata) MUST remain unchanged — these paths do not use the LLM parse prompt.
- **FR-005**: The label merge and column match prompts are out of scope — they operate on already-extracted labels and do not generate new label text.

### Key Entities

- **LLM parse prompt**: The instruction string that governs how labels are extracted from unstructured codebook text. Only the `label` field instruction within this prompt requires change.
- **`label` field**: The extracted description for a variable, written to `outputs/<paper_id>/labels.csv` and `outputs/<paper_id>/codebook_coverage.csv`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For a codebook with known variable descriptions, extracted labels match the source text exactly (modulo acceptable whitespace normalisation) for 100% of variables that have a description in the source.
- **SC-002**: Zero fabricated labels are produced for variables that have a name but no accompanying description in the source codebook.
- **SC-003**: Existing pipeline outputs for structured codebook files are identical before and after the change.

## Assumptions

- The LLM is capable of faithful verbatim extraction when instructed to do so — no architectural changes to the LLM call infrastructure are needed.
- "Verbatim" permits collapsing of internal whitespace (e.g., multiple spaces → one space, newlines within a description → space) but not word substitution, omission, or addition.
- Only the LLM parse prompt needs updating; the secondary matching and merge prompts are out of scope.
- No changes to output CSV schemas, column names, or downstream consumers are required.
