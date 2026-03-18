# Tasks: Fix R File Misclassification and CSV Read Errors

**Input**: Design documents from `/specs/013-fix-r-file-misclassification/`
**Prerequisites**: plan.md ✅ | spec.md ✅ | research.md ✅ | data-model.md ✅ | quickstart.md ✅

**Organization**: Tasks are grouped by user story. Each story phase is independently testable.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies on incomplete tasks)
- **[Story]**: Which user story this task belongs to

---

## Phase 1: Setup

**Purpose**: No new project structure needed — changes are two targeted edits in existing files.

- [x] T001 Verify paper `09567976211040491` data is already downloaded at `data_check/data/09567976211040491/` and confirm `run_index("09567976211040491", download = FALSE)` currently produces the known-bad output (`.R` files in column extraction step, "argument is of length zero" warning, and "incomplete final line" warning)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Add the `AGGREGATE_EXT_OVERRIDE` constant to `0_index.R` — required before the sentinel expansion fix (US1) can be implemented.

**⚠️ CRITICAL**: T002 must complete before T003.

- [x] T002 Add `AGGREGATE_EXT_OVERRIDE` named character vector constant to `data_check/0_index.R` in the constants block (after `ARCHIVE_EXTS` on line ~24), mapping 19 code extensions → `"code"`, 14 asset extensions → `"asset"`, and 7 data extensions → `"data"` as specified in `data-model.md`. Do NOT include `.txt`, `.dat`, `.rda`, `.rdata` in the map.

**Checkpoint**: `AGGREGATE_EXT_OVERRIDE` defined — US1 implementation can proceed.

---

## Phase 3: User Story 1 — Correct type for files inside aggregate folders (Priority: P1) 🎯 MVP

**Goal**: After aggregate sentinel expansion, files with unambiguous extensions get the correct type overriding the inherited sentinel type.

**Independent Test**: Run `run_index("09567976211040491", download = FALSE)` and confirm the file inventory table shows `code / other` and/or `asset / na` entries, and the total data file count in the column extraction step is well below 378.

- [x] T003 [US1] In `data_check/0_index.R`, immediately after the `agg_expanded_df <- do.call(rbind, agg_expanded)` line inside the `if (!is.null(aggregate_df))` block (Step 7, ~line 373), add the three-line extension override: extract `agg_ext` with `tolower(tools::file_ext(...))`, look up `AGGREGATE_EXT_OVERRIDE[agg_ext]`, and replace `agg_expanded_df$type` where the lookup is non-NA. Exact code in `quickstart.md`.

- [x] T004 [US1] Run `run_index("09567976211040491", download = FALSE)` and verify: (a) file inventory shows `code` and `asset` type entries; (b) total data files sent to column extraction is well under 378; (c) no `.R` or `.jpeg` filenames appear in the "skipping (unreadable or empty)" output. Document the actual data-file count in a comment in `run_index_bulk.R` or update `progress.md`.

---

## Phase 4: User Story 2 — Graceful handling of empty/blank-only CSV files (Priority: P2)

**Goal**: `sniff_delimiter()` handles empty files without throwing "argument is of length zero"; `read_data_head()` suppresses the cosmetic "incomplete final line" warning.

**Independent Test**: Manually create a zero-byte temp CSV and call `read_data_head()` on it; confirm it returns `NULL` with the clean "skipping (unreadable or empty)" message and no "argument is of length zero" warning. Also confirm the "incomplete final line" warning no longer appears when processing `statcheck-2.csv`.

- [x] T005 [P] [US2] In `data_check/helper.R`, fix `sniff_delimiter()` (lines ~15–27): change `line <- ""` to `line <- character(0)`; add `if (length(line) == 0) break` as the first statement inside the `for` loop body (before the `nchar` check); add `if (length(line) == 0) return(",")` immediately after the `for` loop closes. Exact code in `quickstart.md`.

- [x] T006 [P] [US2] In `data_check/helper.R`, wrap the `read.delim(path, sep = sep, nrows = n_rows, ...)` call inside `read_data_head()` (the `csv`/`txt`/`tsv`/`dat` branch of the `switch`) with `suppressWarnings(...)`. Do not alter any other part of the `read_data_head()` function. Exact code in `quickstart.md`.

- [x] T007 [US2] Verify both fixes: (a) run `run_index("09567976211040491", download = FALSE)` — confirm no "argument is of length zero" warning appears for `PickupsBehavProf.csv`; (b) confirm no "incomplete final line" warning appears for `statcheck-2.csv`; (c) `PickupsBehavProf.csv` produces the clean "skipping (unreadable or empty)" message.

---

## Phase 5: Polish & Cross-Cutting Concerns

**Goal**: Ensure docs are in sync and the fix is captured in progress tracking.

- [x] T008 [P] Update `docs/pipeline.md` Step 5 description to mention that after aggregate sentinel expansion, an extension-based type override (`AGGREGATE_EXT_OVERRIDE`) is applied to correct inherited types for files with unambiguous extensions. Update the Key Constants table to include `AGGREGATE_EXT_OVERRIDE`.

- [x] T009 [P] Add feature `013-fix-r-file-misclassification` entry to `progress.md` describing both fixes (aggregate type inheritance override and empty-CSV crash in `sniff_delimiter`).

- [x] T010 Run a full end-to-end smoke test: execute `run_index("09567976211040491", download = FALSE)` and confirm the paper completes successfully with a valid `structure.csv` and `columns.csv` written to `data_check/outputs/09567976211040491/`, no errors thrown, and no spurious warnings in output.

---

## Dependencies

```
T001 (verify baseline)
  └── T002 (add AGGREGATE_EXT_OVERRIDE constant)
        └── T003 [US1] (apply override after expansion)
              └── T004 [US1] (verify US1)

T005 [US2] (fix sniff_delimiter)   ─┐
T006 [US2] (suppress warning)      ─┤── T007 [US2] (verify US2)
                                       └── T010 (full smoke test)

T008 (docs update)   ─┐
T009 (progress.md)   ─┘  (parallel, no deps)
```

**User story independence**: US2 (T005–T006) can be worked in parallel with US1 (T003) since they touch different functions and different lines.

---

## Parallel Execution

Within Phase 4:
- T005 (`sniff_delimiter` fix in `helper.R`) and T006 (warning suppression in `helper.R`) touch different functions but the same file. They can be implemented in the same edit session but must be reviewed together before T007.

Within Phase 5:
- T008 and T009 are fully independent and can be done simultaneously.

---

## Implementation Strategy

**MVP** = Phase 1 + Phase 2 + Phase 3 (T001–T004): Fixes the primary bug (378 files sent to column extraction). Delivers SC-001 and SC-004.

**Full delivery** = all phases (T001–T010): Also fixes the empty-CSV crash and warning noise, with docs updated.

**Suggested order for a single session**: T001 → T002 → T003+T005+T006 (in one `helper.R`/`0_index.R` edit pass) → T004 → T007 → T010 → T008+T009.
