# Tasks: Fix CSV Codebook Parsing Robustness

**Input**: Design documents from `/specs/018-fix-csv-codebook-parsing/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅

**Tests**: Not requested — no test tasks generated.

**Organization**: Tasks grouped by user story for independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1, US2, US3)

## Path Conventions

All changes are in `pipeline/` — a single-project pipeline.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Understand the current state before making changes.

- [x] T001 Read `pipeline/helper.R` lines 1–30 (`sniff_delimiter`) and lines 375–410 (`.find_codebook_cols`, `.extract_structured_codebook`) and lines 520–650 (`parse_codebook`, `.run_llm_chunk_loop`) to confirm current behaviour matches the plan before touching any code
- [x] T002 Read `pipeline/2_codebook_label.R` lines 180–215 (coverage_df construction) to confirm the column list and schema before adding `parse_method`

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Add the `CODEBOOK_HEADER_LOOKAHEAD` constant that Phases 3–5 depend on.

**⚠️ CRITICAL**: Tasks T003 must complete before US1 and US3 implementation begins.

- [x] T003 In `pipeline/2_codebook_label.R`, add constant `CODEBOOK_HEADER_LOOKAHEAD <- 5L` in the constants block alongside `MAX_CODEBOOK_LLM_CALLS` and `MAX_CODEBOOK_FILE_MB`

**Checkpoint**: Constant defined — US1, US2, US3 implementation can now begin.

---

## Phase 3: User Story 1 — Messy CSV codebooks produce labels (Priority: P1) 🎯 MVP

**Goal**: Multi-level headers and non-standard column names no longer produce empty output.

**Independent Test**: Run `run_single.R` against a paper whose CSV codebook uses multi-level headers (e.g. `Summary_README.csv` layout). Verify `outputs/<paper_id>/labels.csv` is non-empty. Run against a currently-passing paper and confirm no regression.

### Implementation for User Story 1

- [x] T004 [US1] In `pipeline/helper.R`, extend `.find_codebook_cols()` regex patterns (lines 381–388) to include additional variable-column variants (`variable[_ ]?label`, `var[_ ]?label`, `item`) and additional label-column variants (`label[_ ]?text`, `question`, `question[_ ]?text`, `variable[_ ]?description`) in `pipeline/helper.R`
- [x] T005 [US1] In `pipeline/helper.R`, replace the `csv = , tsv = , dat =` branch inside `parse_codebook()` (lines 543–547) with a header-lookahead implementation: read the file with `header = FALSE`, scan rows 1 through `CODEBOOK_HEADER_LOOKAHEAD` using `.find_codebook_cols()`, use the matching row as the header, pass remaining rows to `.extract_structured_codebook()` in `pipeline/helper.R`
- [x] T006 [US1] In `pipeline/helper.R`, update `sniff_delimiter()` (lines 19–23) to also skip lines where `startsWith(trimws(line), "#")` — so comment/metadata rows are ignored when probing for the delimiter in `pipeline/helper.R`

**Checkpoint**: User Story 1 fully functional — multi-level CSV codebooks now produce labels.

---

## Phase 4: User Story 2 — Structured/LLM parse path visibility (Priority: P2)

**Goal**: `codebook_coverage.csv` records `parse_method` for every row; no silent skips.

**Independent Test**: Run codebook labelling on any paper with a CSV codebook. Open `outputs/<paper_id>/codebook_coverage.csv` and confirm the `parse_method` column is present with value `"structured"` or `"llm"` for every row. Run on a paper whose codebook forces LLM fallback and confirm `"llm"` is recorded.

### Implementation for User Story 2

- [x] T007 [P] [US2] In `pipeline/helper.R`, add `parse_method = "structured"` column to the data.frame returned by the structured path in `parse_codebook()` — insert before the `result$group <- .infer_group(...)` line (around line 584) in `pipeline/helper.R`
- [x] T008 [P] [US2] In `pipeline/helper.R`, add `parse_method = "llm"` column to the data.frame returned by `.run_llm_chunk_loop()` — add after the `rbind` consolidation step at the end of the function in `pipeline/helper.R`
- [x] T009 [US2] In `pipeline/2_codebook_label.R`, add `parse_method = codebook_vars_df$parse_method` to the `coverage_df` construction at lines 185–196, and add `parse_method = character(0)` to the empty fallback data.frame at lines 198–207 in `pipeline/2_codebook_label.R`

**Checkpoint**: User Stories 1 and 2 both work — labels improve AND coverage records parse method.

---

## Phase 5: User Story 3 — Encoding-resilient CSV loading (Priority: P3)

**Goal**: latin1-encoded CSV codebooks load without garbled characters or silent failure.

**Independent Test**: Run codebook labelling on a paper with a Windows-1252 / latin1 encoded CSV codebook. Verify labels contain correct non-ASCII characters rather than replacement characters.

### Implementation for User Story 3

- [x] T010 [US3] In `pipeline/helper.R`, wrap the `read.delim(..., header = FALSE)` call added in T005 with a latin1 encoding fallback: after a successful read, check any character column for invalid UTF-8 bytes using `iconv(..., from="UTF-8", to="UTF-8")`; if `any(is.na(...))`, re-read with `fileEncoding = "latin1"` — mirror the existing pattern in `read_data_head()` lines 46–55 in `pipeline/helper.R`

**Checkpoint**: All three user stories functional.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Documentation and schema sync.

- [x] T011 [P] Update `docs/output-schemas.md` — add `parse_method` column definition to the `codebook_coverage.csv` schema table (values: `"structured"`, `"llm"`)
- [x] T012 [P] Update `progress.md` — add feature 018 entry describing the CSV codebook parsing improvements

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately; read-only
- **Foundational (Phase 2)**: Depends on Setup; T003 BLOCKS T005 (uses the constant)
- **US1 (Phase 3)**: Depends on T003 (CODEBOOK_HEADER_LOOKAHEAD constant)
- **US2 (Phase 4)**: T007 and T008 depend on Setup only (read helper.R first); T009 depends on T007+T008
- **US3 (Phase 5)**: T010 depends on T005 (extends the same read block)
- **Polish (Phase 6)**: Depends on T005 (schema change) and all US phases

### User Story Dependencies

- **US1 (P1)**: Requires T003 (constant). Independent of US2 and US3.
- **US2 (P2)**: Independent of US1 and US3 (different lines in helper.R and 2_codebook_label.R).
- **US3 (P3)**: Requires T005 (extends the same CSV read block). Sequential with US1.

### Within Each User Story

- T004, T006 are parallel (different functions in helper.R)
- T005 depends on T003 and can start after T004 (extended regex needed for the scan)
- T007, T008 are parallel (different return sites in helper.R)
- T009 depends on T007+T008

---

## Parallel Opportunities

```
# Phase 1 reads can run in parallel:
Task T001: Read helper.R parse_codebook and helpers
Task T002: Read 2_codebook_label.R coverage_df construction

# Within US1 — T004 and T006 can run in parallel (different functions):
Task T004: Extend .find_codebook_cols() regex
Task T006: Update sniff_delimiter() comment-skip
# Then T005 after T004 (uses extended regex in the scan loop)

# Within US2 — T007 and T008 can run in parallel (different return sites):
Task T007: Add parse_method="structured" to structured return path
Task T008: Add parse_method="llm" to .run_llm_chunk_loop return

# Polish tasks can run in parallel:
Task T011: Update output-schemas.md
Task T012: Update progress.md
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1 (T001–T002): Read-only orientation
2. Complete Phase 2 (T003): Add constant
3. Complete Phase 3 (T004–T006): Header lookahead + extended regex + comment-skip
4. **STOP and VALIDATE**: Run against a multi-level CSV codebook paper; confirm non-empty labels; run against a passing paper to confirm no regression

### Incremental Delivery

1. Setup + Foundational (T001–T003) → code ready to modify
2. US1 (T004–T006) → multi-level CSV codebooks now produce labels
3. US2 (T007–T009) → `parse_method` visible in coverage CSV
4. US3 (T010) → encoding resilience added
5. Polish (T011–T012) → docs updated

---

## Notes

- All changes are confined to `pipeline/helper.R` and `pipeline/2_codebook_label.R` — no new files, no new packages
- `CODEBOOK_HEADER_LOOKAHEAD` is a new constant that belongs in `2_codebook_label.R` alongside `MAX_CODEBOOK_LLM_CALLS`
- The `parse_method` column is a backward-compatible addition to `codebook_coverage.csv`; existing consumers tolerate new columns
- US1 is the highest-value fix — implement and validate it before proceeding to US2/US3
