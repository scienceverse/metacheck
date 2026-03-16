# Feature Specification: LLM Fuzzy Column Matching

**Feature Branch**: `006-llm-fuzzy-matching`
**Created**: 2026-03-16
**Status**: Draft
**Input**: User description: "add a secondary LLM call for all non-labelled variables from the codebook. This should be implemented in the matching stage (2_codebook_label.R). Add a column to indicate whether a column was determined using hardcoded rules or llm call."

## User Scenarios & Testing *(mandatory)*

### User Story 1 — LLM Resolves Unmatched Columns (Priority: P1)

After rule-based matching runs and leaves some data columns unlabelled, a secondary LLM step receives the unmatched data column names and the unmatched codebook variable names and attempts to pair them by meaning. Columns that the LLM successfully pairs receive the corresponding codebook label, raising overall labelling coverage for the paper.

**Why this priority**: This is the entire purpose of the feature. Without it, nothing else is meaningful.

**Independent Test**: Take a paper where rule-based matching leaves known columns unlabelled due to name divergence (e.g., `STAI_S_Y_PRE` vs `STAI pre`). Run `run_codebook_label()` and verify those columns now have `label_status = "llm"` and the correct label.

**Acceptance Scenarios**:

1. **Given** a data column `STAI_S_Y_PRE` and a codebook variable `STAI pre` that rule-based matching cannot link, **When** the LLM matching step runs, **Then** the column receives the label from `STAI pre` and `label_status = "llm"`.
2. **Given** a data column with no plausible codebook counterpart, **When** the LLM matching step runs, **Then** the column remains `label_status = "unlabelled"` and `label_method = NA`.
3. **Given** a paper where all columns were already labelled by rule-based matching, **When** `run_codebook_label()` is called, **Then** no secondary LLM call is made and all columns have `label_method = "rules"`.

---

### User Story 2 — Label Method Provenance Column (Priority: P2)

Every row in the labels output carries a `label_method` field indicating how the label was determined, so a researcher can distinguish high-confidence rule-based matches from LLM-inferred matches and apply different levels of trust accordingly.

**Why this priority**: Provenance is essential for a researcher deciding how much to trust a label. Without it, rule-matched and LLM-inferred columns look identical.

**Independent Test**: Inspect the labels output for a paper that had both rule-based and LLM-matched columns. Verify `label_method` is `"rules"` for rule-matched rows, `"llm"` for LLM-matched rows, and `NA` for unlabelled rows.

**Acceptance Scenarios**:

1. **Given** a column labelled by normalized string matching, **When** the labels output is read, **Then** its `label_method` is `"rules"`.
2. **Given** a column labelled by the secondary LLM step, **When** the labels output is read, **Then** its `label_method` is `"llm"`.
3. **Given** a column that remains unlabelled after both steps, **When** the labels output is read, **Then** its `label_method` is `NA`.

---

### Edge Cases

- What if the LLM proposes a match for a column that already has a rule-based label? Rule-based labels must always take precedence; the LLM step only acts on unlabelled columns.
- What if the LLM returns a codebook variable name that was not submitted as a candidate? The pairing must be discarded.
- What if all codebook variables were already matched by rules? No unmatched codebook variables exist, so no secondary LLM call is made.
- What if the LLM call fails or returns malformed output? All submitted columns remain `unlabelled`; the failure is recorded as a warning.
- What if there are more unmatched pairs than a single LLM call can handle? The candidate set is batched; the per-paper LLM call budget cap applies.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: After rule-based matching completes, the system MUST collect all data columns with `label_status = "unlabelled"` as LLM matching candidates.
- **FR-002**: The system MUST collect all codebook variables not matched by rules as the candidate label pool for the LLM step.
- **FR-003**: If either candidate set is empty, the system MUST skip the LLM call entirely; no call is made for papers with full rule-based coverage.
- **FR-004**: The system MUST submit the unmatched column names and unmatched codebook variable names to the LLM, requesting best-guess pairings returned as structured data.
- **FR-005**: The system MUST validate each LLM-proposed pairing: both the column name and the codebook variable name must belong to the submitted candidate sets; pairings referencing unknown names are discarded.
- **FR-006**: Accepted LLM pairings MUST update the column's label, codebook variable reference, label source, and set `label_status = "llm"`.
- **FR-007**: Columns that remain unmatched after both rule-based and LLM steps MUST retain `label_status = "unlabelled"`.
- **FR-008**: Every row in the labels output MUST include a `label_method` column: `"rules"` for rule-matched, `"llm"` for LLM-matched, `NA` for unlabelled.
- **FR-009**: The LLM step MUST NOT overwrite any label produced by rule-based matching; it is strictly additive.
- **FR-010**: The LLM matching step MUST respect the existing per-paper LLM call budget.

### Key Entities

- **LLMMatchCandidate**: An unlabelled data column submitted to the LLM for pairing, identified by its column name and experiment group.
- **CandidateCodebookVar**: A codebook variable unmatched after rule-based processing, offered to the LLM as a potential label source.
- **LLMMatchingResult**: An LLM-proposed pairing between one data column name and one codebook variable name; valid only when both sides are confirmed candidates.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For papers where name divergence is the only barrier to matching, the LLM step increases labelled column count compared to rule-only output.
- **SC-002**: Rule-based labelling coverage is identical with and without the LLM step; no rule-based labels are overwritten.
- **SC-003**: Every labelled row in the output has a non-null `label_method`; every unlabelled row has `label_method = NA`.
- **SC-004**: When no unlabelled columns or no unmatched codebook variables exist, zero additional LLM calls are made.
- **SC-005**: No hallucinated variable names appear in the output; all LLM-matched `codebook_variable` values trace back to the submitted candidate pool.

## Assumptions

- Feature 005 (rule-based codebook labelling) runs first; this feature extends the matching stage of that pipeline without replacing it.
- The LLM receives only variable/column names (not values or statistics); this is sufficient for resolving abbreviation and naming convention differences common in psychology datasets.
- `label_method` is a new column added to `_labels.csv`; the output schema documentation must be updated.
- The new `label_status` value `"llm"` replaces `"unlabelled"` for LLM-matched columns; all other existing status values are unchanged.
- The per-paper LLM call budget governs the secondary matching step; no budget increase is assumed.
