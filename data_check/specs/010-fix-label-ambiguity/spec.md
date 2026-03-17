# Feature Specification: Codebook Label Matching — Reduce False Conflicts & Fix Misplaced Labels

**Feature Branch**: `010-fix-label-ambiguity`
**Created**: 2026-03-17
**Status**: Draft

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Synonymous Labels No Longer Flagged as Conflicts (Priority: P1)

A researcher runs the codebook labelling pipeline on a paper whose codebook documents the same variable in multiple files using slightly different phrasings (e.g., "Participant age" vs "Participants' age", or "Survey start time" vs "Time participants started the survey"). Today the pipeline flags every such column as `conflicting_definition`, burying useful labels under noise. After this fix, columns whose candidate labels are **semantically equivalent** get a single clean label instead of a conflict flag.

**Why this priority**: This is the most common false-positive category. It significantly inflates `conflicting_definition` counts, making the output unreliable for downstream analysis.

**Independent Test**: Run the pipeline on paper `0956797620948821`. Columns `Age`, `Gender`, `ResponseId`, `StartDate`, `EndDate` should switch from `conflicting_definition` to a single merged label. Can be tested in isolation without any changes to the misplaced-label logic.

**Acceptance Scenarios**:

1. **Given** a column whose candidate labels differ only in minor wording (possessive form, synonym word order, level of detail in parenthetical), **When** the pipeline matches it, **Then** the status is `matched` (not `conflicting_definition`) and one canonical label is written.
2. **Given** a column whose candidate labels contradict each other in meaning (e.g., "response time in ms" vs "accuracy proportion"), **When** the pipeline matches it, **Then** `conflicting_definition` is still emitted.
3. **Given** paper `0956797620967261`, columns `f_emb`, `f_bor`, `f_anx`, `f_rela`, `f_fear` whose two-source labels differ only in phrasing ("Frequency of X feelings in the past week" vs "Frequency of X emotion"), **When** the pipeline runs, **Then** all five columns get a single merged label with status `matched`.

---

### User Story 2 — Correct Label Assigned to Each Matched Column (Priority: P2)

A researcher inspects the labels CSV for paper `0956797617716929` and finds that `BIS_2_Nonplanning_Impulsiveness` has been assigned an incorrect label. This indicates the matching logic is selecting the wrong codebook entry — either picking a label from the wrong subscale level, the wrong experiment group, or a different variable entirely.

**Why this priority**: A silent wrong label is worse than a conflict flag. Less frequent but higher severity.

**Independent Test**: Run the pipeline on paper `0956797617716929`. All nine BIS columns (`BIS_1_*` and `BIS_2_*`) should carry the label that corresponds to their exact name in the codebook, verifiable by manual inspection of the known-good codebook.

**Acceptance Scenarios**:

1. **Given** column `BIS_2_Nonplanning_Impulsiveness` and a codebook entry with label "Nonplanning Impulsiveness (N)", **When** the pipeline matches it, **Then** the assigned label is the one whose codebook variable name most precisely maps to that column (not a label from a BIS_1 subscale or a different column).
2. **Given** any column where multiple codebook entries share a similar normalised name, **When** labels are assigned, **Then** the most specific / exact match is preferred over a partial or fuzzy match.

---

### Edge Cases

- What happens when all candidate labels from two sources are identical? → Should consolidate silently as `matched` (already works today; must remain unchanged).
- What happens when labels differ only by trailing punctuation or capitalisation? → Should consolidate.
- What happens when one source has a short label and another has a long descriptive label for the same variable? → Should consolidate using the longer/more informative label as canonical.
- What happens when semantic consolidation is attempted but the check is inconclusive? → Fall back to existing `conflicting_definition` behaviour; do not drop labels.
- What if a paper has no codebook conflicts at all? → No change in output.

---

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The pipeline MUST detect when multiple candidate labels for a single column are semantically equivalent and consolidate them into one canonical label rather than emitting `conflicting_definition`.
- **FR-002**: Semantic equivalence detection MUST cover at minimum: synonym phrasing, possessive vs non-possessive form ("Participants' age" / "Participant age"), minor word-order differences, and appended parenthetical clarifications that do not change meaning.
- **FR-003**: When labels are consolidated, the output `label` field MUST contain a single human-readable string (not a pipe-delimited list), and the `label_method` field MUST record how the label was resolved (a new value such as `merged_equivalent`).
- **FR-004**: `conflicting_definition` MUST still be emitted when candidate labels carry genuinely different information (different constructs, different scales, different value codings).
- **FR-005**: The column-to-codebook-variable matching MUST assign the label whose codebook variable name is the closest/most-specific match to the column name, preventing a more-distant codebook entry from overriding the correct one.
- **FR-006**: All changes MUST be backward-compatible: papers with no label conflicts must produce identical output to the current pipeline.
- **FR-007**: The pipeline MUST NOT introduce new `conflicting_definition` flags as a side-effect of this change.

### Key Entities

- **Candidate label set**: The set of distinct label strings collected for one column from all applicable codebook sources after group scoping.
- **Semantic equivalence check**: The logic (rule-based and/or LLM-assisted) that determines whether two or more label strings describe the same construct.
- **Canonical label**: The single label string chosen when a candidate set is consolidated; should prefer the most informative/specific phrasing.
- **`label_method`**: Existing output column recording how a label was obtained; needs a new enum value for the merged-equivalent case.

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For paper `0956797620948821`, columns `Age`, `Gender`, `ResponseId`, `StartDate`, `EndDate` all have status `matched` (not `conflicting_definition`) after the fix.
- **SC-002**: For paper `0956797620967261`, columns `f_emb`, `f_bor`, `f_anx`, `f_rela`, `f_fear` all have status `matched` after the fix.
- **SC-003**: For paper `0956797617716929`, `BIS_2_Nonplanning_Impulsiveness` carries the correct label from the codebook, verified by manual comparison against the source codebook file.
- **SC-004**: Re-running the full bulk pipeline on the same input set produces a `conflicting_definition` count equal to or lower than the pre-fix count; no new conflicts are introduced.
- **SC-005**: Columns that were previously correctly labelled (non-conflicting papers) retain identical output after the fix.

---

## Assumptions

- Semantic equivalence checking may delegate to the LLM already present in the pipeline when rule-based normalisation alone is insufficient, subject to the existing LLM call budget per paper.
- The `label_method` column already exists in the labels CSV; a new enum value (`merged_equivalent` or similar) is additive and backward-compatible.
- The misplaced-label bug for `BIS_2_Nonplanning_Impulsiveness` is likely caused by either (a) an inexact normalised-name match selecting the wrong codebook row, or (b) a group-scoping edge case; the exact root cause will be confirmed during planning.
- No changes to downstream consumers of `conflicting_definition` status are in scope for this feature.
