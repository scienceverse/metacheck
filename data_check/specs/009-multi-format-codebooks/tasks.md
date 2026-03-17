# Tasks: Multi-Format Codebook Reading

**Input**: Design documents from `/specs/009-multi-format-codebooks/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify the environment has all required packages before any code changes.

- [x] T001Confirm `officer` (≥0.7.0) and `pdftools` (≥3.0.0) are installed by running `Rscript -e "packageVersion('officer'); packageVersion('pdftools')"` — document versions in a comment at the top of `data_check/helper.R`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Add the `extract_rich_text()` internal helper to `helper.R`. This is the shared extraction layer all three user stories depend on.

**⚠️ CRITICAL**: All user story phases depend on this helper existing.

- [x] T002 Add internal helper `.extract_rich_text(path, ext)` to `data_check/helper.R` (after the existing `parse_codebook` function). The helper must:
  - Accept `path` (character) and `ext` (lowercase extension string, no leading dot)
  - For `ext == "docx"`: call `officer::read_docx(path)`, then `officer::docx_summary()`, collect the `text` column, paste non-empty entries with `"\n"`, return the string
  - For `ext == "pdf"`: call `pdftools::pdf_text(path)`, paste all pages with `"\n"`, return the string
  - For `ext == "rtf"`: call `readLines(path, warn = FALSE)`, apply `.strip_rtf()` (see T003), return result
  - For `ext == "doc"` or `ext == "odt"`: call `readLines(path, warn = FALSE)`, paste lines with `"\n"`, return result (binary garbage is handled by the empty-string guard in the caller)
  - For any other `ext`: return `""`
  - Wrap the entire body in `tryCatch(..., error = function(e) "")` so any failure returns `""`
  - Use `requireNamespace("officer", quietly = TRUE)` and `requireNamespace("pdftools", quietly = TRUE)` guards before calling those packages; if unavailable, return `""`

- [x] T003 Add internal helper `.strip_rtf(text)` to `data_check/helper.R` (just before `.extract_rich_text`). The helper must:
  - Accept a single character string
  - Remove RTF header and control groups: `gsub("\\\\[a-z]+\\-?[0-9]*\\s?", " ", text)`
  - Remove RTF control symbols: `gsub("\\\\[^a-z\n]", " ", text)`
  - Remove braces: `gsub("[{}]", "", text)`
  - Collapse multiple whitespace: `gsub("\\s+", " ", text)`
  - Return `trimws(text)`

**Checkpoint**: `.extract_rich_text()` and `.strip_rtf()` exist in `helper.R` and can be called manually in an R session.

---

## Phase 3: User Story 1 — DOCX Codebook Support (Priority: P1) 🎯 MVP

**Goal**: `.docx` and `.doc` files classified as `codebook`/`readme` are processed by `parse_codebook()` and yield variable definitions (or graceful `parse_failed` for unreadable files).

**Independent Test**: Run `run_codebook_label(paper_id)` on any paper whose `structure.csv` lists a `.docx` codebook file. Verify `labels.csv` is written and contains at least one row with `label_status = "labelled"`.

### Implementation for User Story 1

- [x] T004 [US1] Extend the `switch(ext, ...)` block in `parse_codebook()` in `data_check/helper.R` to handle `docx` and `doc` extensions:
  - Add a `docx = , doc =` branch (before the final `NULL` fallthrough)
  - In the branch: call `text <- .extract_rich_text(path, ext)`
  - If `nchar(trimws(text)) < 10`: emit `warning("No extractable text from ", src, " (", ext, ")")` and `return(NULL)`
  - Split text into lines: `lines <- strsplit(text, "\n")[[1]]`
  - Pass `lines` directly into the existing LLM chunking loop (the `chunks <- split(lines, ...)` block that currently runs after `readLines()`)

- [x] T005 [US1] Refactor `parse_codebook()` in `data_check/helper.R` to avoid duplicating the LLM chunking loop: extract the chunk-loop body (lines from `chunks <- split(...)` to the final `do.call(rbind, ...)`) into a named block or minimal inline sub-expression, shared by both the `readLines()` plain-text path and the new rich-text path. No behaviour change — pure deduplication.

**Checkpoint**: User Story 1 complete. Papers with `.docx` codebooks produce labelled output. Papers with `.doc` files produce a `parse_failed` warning but no crash. All existing CSV/XLSX/SAV/DTA codebook behaviour is unchanged.

---

## Phase 4: User Story 2 — PDF Codebook Support (Priority: P2)

**Goal**: `.pdf` files classified as `codebook`/`readme` are processed by `parse_codebook()`. Selectable-text PDFs yield variable definitions; image-only PDFs yield graceful `parse_failed`.

**Independent Test**: Run `run_codebook_label(paper_id)` on a paper whose `structure.csv` lists a `.pdf` codebook. Verify `labels.csv` is written. For a known image-only PDF, verify a `parse_failed` warning is emitted and `labels.csv` still contains all rows (with `label_status = "no_codebook"` or `"unlabelled"` as appropriate).

### Implementation for User Story 2

- [x] T006 [US2] Extend the `switch(ext, ...)` block in `parse_codebook()` in `data_check/helper.R` to handle `pdf`:
  - Add a `pdf =` branch (before the final `NULL` fallthrough)
  - Call `text <- .extract_rich_text(path, "pdf")`
  - If `nchar(trimws(text)) < 10`: emit `warning("No extractable text from ", src, " (pdf — may be image-only)")` and `return(NULL)`
  - Split into lines and pass to the shared LLM chunking block (from T005)

**Checkpoint**: User Story 2 complete. Papers with `.pdf` codebooks produce labelled output where text is selectable. Image-only PDFs produce a warning and continue without crash.

---

## Phase 5: User Story 3 — RTF/ODT Graceful Handling (Priority: P3)

**Goal**: `.rtf` and `.odt` files classified as `codebook`/`readme` are attempted. If text can be extracted (RTF stripping), variables are extracted via LLM. If not, `parse_failed` is recorded without crashing.

**Independent Test**: Pass an `.rtf` file through `parse_codebook()` manually in an R session. Verify the call either returns a data.frame of variables OR returns `NULL` with a warning — no unhandled error.

### Implementation for User Story 3

- [x] T007 [US3] Extend the `switch(ext, ...)` block in `parse_codebook()` in `data_check/helper.R` to handle `rtf` and `odt`:
  - Add an `rtf = , odt =` branch
  - Call `text <- .extract_rich_text(path, ext)`
  - If `nchar(trimws(text)) < 10`: emit warning and `return(NULL)`
  - Split into lines and pass to the shared LLM chunking block

**Checkpoint**: User Story 3 complete. All three user stories independently functional.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Validate end-to-end correctness and update documentation.

- [x] T008 [P] Verify no regression: run `run_codebook_label()` on at least one paper with a CSV codebook and one with an XLSX codebook; confirm output is identical to pre-change behaviour by diffing `labels.csv` output

- [x] T009 [P] Update `docs/pipeline.md` to note that `parse_codebook()` now supports `.docx`, `.pdf`, `.rtf`, `.odt`, `.doc` in addition to existing formats; add `officer` and `pdftools` as pipeline dependencies

- [x] T010 [P] Update `docs/output-schemas.md` to add `parse_failed` as a recognised per-file parse status in the codebook coverage notes section (not a paper-level error code)

- [x] T011 Update `progress.md` with feature 009 entry: branch, description, status

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 — BLOCKS all user stories (T002, T003 must complete)
- **User Stories (Phases 3–5)**: All depend on Phase 2 completion
  - US1 (Phase 3) and US2 (Phase 4) are fully independent — can proceed in parallel after Phase 2
  - US3 (Phase 5) is fully independent — can proceed in parallel with US1/US2
- **Polish (Phase 6)**: Depends on desired user stories being complete; T008 requires at least Phase 3

### User Story Dependencies

- **US1 (P1)**: Depends only on T002+T003 (Foundational). No dependency on US2 or US3.
- **US2 (P2)**: Depends only on T002+T003. No dependency on US1 or US3.
- **US3 (P3)**: Depends only on T002+T003. No dependency on US1 or US2.
- T005 (refactor shared LLM block) is within US1's phase but must complete before T006 and T007 consume the shared block.

### Parallel Opportunities

- T002 and T003 are sequential (T003 must exist before T002 calls it)
- T004 and T005 are sequential within US1 (T005 refactors what T004 adds)
- T006 (US2) and T007 (US3) can both start after T005 completes — fully parallel
- T008, T009, T010 in Phase 6 are all parallel

---

## Parallel Example: After Foundational Complete

```text
# Once T002 + T003 are done, these can all run in parallel:

Task T004: Extend switch() for docx/doc  (data_check/helper.R)
Task T006: Extend switch() for pdf       (data_check/helper.R)  ← wait for T005
Task T007: Extend switch() for rtf/odt  (data_check/helper.R)  ← wait for T005

# Polish tasks (after all story phases):
Task T008: Regression verification
Task T009: Update docs/pipeline.md
Task T010: Update docs/output-schemas.md
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Phase 1: Confirm packages installed (T001)
2. Phase 2: Add `.strip_rtf()` and `.extract_rich_text()` to `helper.R` (T002, T003)
3. Phase 3: Extend `parse_codebook()` for DOCX/DOC; refactor shared LLM block (T004, T005)
4. **STOP and VALIDATE**: Run against a paper with a `.docx` codebook — verify labelled output
5. DOCX codebook support is live; PDF and RTF can follow independently

### Incremental Delivery

1. Setup + Foundational → helpers ready
2. US1 → DOCX/DOC support live (MVP)
3. US2 → PDF support live
4. US3 → RTF/ODT graceful handling live
5. Polish → docs updated, regression confirmed

---

## Notes

- All changes are confined to `data_check/helper.R`; no other files change except docs
- `parse_codebook()` already has a `tryCatch` wrapper at the top level — new branches inherit this safety net
- The LLM chunking block refactor (T005) is a pure deduplication with zero behaviour change; it enables T006 and T007 to reuse identical logic
- `[P]` tasks are in different conceptual sections but since they all touch `helper.R`, they should be executed sequentially in practice to avoid merge conflicts — mark as logically parallel but execute in order
