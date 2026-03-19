# Tasks: Verbatim Codebook Label Extraction

**Input**: Design documents from `/specs/015-verbatim-codebook-labels/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2)
- Include exact file paths in descriptions

---

## Phase 1: User Story 1 — LLM Extracts Verbatim Labels (Priority: P1) 🎯 MVP

**Goal**: Update `CODEBOOK_PARSE_PROMPT` so the LLM copies label text verbatim from the codebook source rather than paraphrasing or summarising it.

**Independent Test**: Run the pipeline on a paper with a DOCX or PDF codebook that has known variable descriptions; confirm extracted `label` values in `outputs/<paper_id>/labels.csv` match the source text character-for-character (modulo whitespace normalisation).

### Implementation for User Story 1

- [x] T001 [US1] Update `CODEBOOK_PARSE_PROMPT` in `data_check/2_codebook_label.R` (lines 37–47): change `"label": "<human-readable description>"` to `"label": "<verbatim description text copied from the codebook>"`, replace the `label:` rule with "copy the description text exactly as it appears — do NOT paraphrase, summarise, or infer", and add an explicit rule "Do NOT rephrase or summarise; if no description text is present for a variable, omit that variable entirely"

**Checkpoint**: After T001, run `run_codebook_label()` on one paper with a PDF or DOCX codebook. Verify `labels.csv` labels match codebook source text verbatim.

---

## Phase 2: User Story 2 — Structured Codebook Extraction Unaffected (Priority: P2)

**Goal**: Confirm that the structured extraction path (CSV, Excel, SPSS, Stata codebooks) is untouched and still produces verbatim output.

**Independent Test**: Run the pipeline on a paper with a structured CSV codebook; verify `label` values in `outputs/<paper_id>/labels.csv` are identical to the raw cell contents of the label column.

### Implementation for User Story 2

- [x] T002 [US2] Review `parse_codebook()` in `data_check/helper.R` (lines 483–560) to confirm no changes are needed to structured-format extraction paths — document finding as a comment or in the PR description

**Checkpoint**: No code change expected. Confirms regression-free scope.

---

## Phase 3: Polish & Cross-Cutting Concerns

- [x] T003 Update `docs/progress.md` to record feature 015 as complete with a one-line description of the change

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (US1)**: No prerequisites — can start immediately
- **Phase 2 (US2)**: Independent of Phase 1 — can run in parallel
- **Phase 3 (Polish)**: Depends on Phase 1 completion

### User Story Dependencies

- **User Story 1 (P1)**: No dependencies — single file edit
- **User Story 2 (P2)**: No dependencies — read-only review, no code changes expected

### Parallel Opportunities

- T001 and T002 can be done in parallel (different files)
- T003 can only start after T001 is complete and validated

---

## Implementation Strategy

### MVP (User Story 1 only)

1. Complete T001 — update prompt
2. Smoke test: run pipeline on one real paper with DOCX/PDF codebook
3. **STOP and VALIDATE**: confirm verbatim labels in output CSV
4. Then complete T002 (verification) and T003 (progress.md)

### Total Tasks: 3
- Phase 1 (US1): 1 task
- Phase 2 (US2): 1 task
- Polish: 1 task

---

## Notes

- T001 is the only code change in this entire feature
- T002 is a read-only verification — no code expected to change
- No schema changes, no new files, no new dependencies
- Commit after T001 validation; commit T002+T003 together
