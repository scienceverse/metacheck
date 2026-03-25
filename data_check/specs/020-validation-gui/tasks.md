# Tasks: Validation GUI

**Input**: Design documents from `/specs/020-validation-gui/`
**Prerequisites**: plan.md ✅ spec.md ✅ research.md ✅ data-model.md ✅ quickstart.md ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and
testing of each story. No tests are requested in the spec — no test tasks are included.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (no dependency on any incomplete sibling task)
- **[Story]**: Which user story this task belongs to (US1–US4)
- Exact file paths are relative to `data_check/`

---

## Phase 1: Setup

**Purpose**: Create the file and directory scaffold.

- [ ] T001 Create `tools/validation_gui/` directory and empty stub files `app.R`, `preview.R`, `gt_store.R`
- [ ] T002 Create `ground_truth/` directory with a `.gitkeep` file so it is tracked by git when empty

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data-access helpers that every user story depends on.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [ ] T003 [P] In `tools/validation_gui/gt_store.R`: implement `discover_papers()` — scans `outputs/` for subdirectories containing `structure.csv` and returns a named character vector of paper IDs (using `colClasses = c(paper_id = "character")`)
- [ ] T004 [P] In `tools/validation_gui/gt_store.R`: implement `load_structure(paper_id)` — reads `outputs/<paper_id>/structure.csv` with `colClasses = c(paper_id = "character", is_raw = "logical", is_sentinel = "logical")` and returns the data.frame
- [ ] T005 In `tools/validation_gui/gt_store.R`: implement `read_gt(paper_id)` — reads `ground_truth/<paper_id>.csv` (if it exists) with `colClasses = c(paper_id = "character", is_raw_gt = "logical")`; returns an empty data.frame with correct schema if the file does not exist
- [ ] T006 In `tools/validation_gui/gt_store.R`: implement `upsert_gt(gt_df, new_row)` — replaces any existing row matching `new_row$rel_path`, appends if absent, returns the updated data.frame; and `write_gt(paper_id, gt_df)` — writes to `ground_truth/<paper_id>.csv` with `write.csv(..., row.names = FALSE)` immediately (no batching)

**Checkpoint**: Data-access layer is complete. User story phases can now begin.

---

## Phase 3: User Story 1 — Label files for a paper (Priority: P1) 🎯 MVP

**Goal**: Annotator can select a paper, see each file in a list, assign the three labels
via form controls, and save — with every save written immediately to the ground-truth CSV.

**Independent Test**: Run `shiny::runApp("tools/validation_gui")`, select paper
`0956797616647519`, label all 19 files, close the app, and verify that
`ground_truth/0956797616647519.csv` contains exactly 19 rows with correct schema.

- [ ] T007 [US1] In `tools/validation_gui/app.R`: build the top-level UI shell — `bslib::page_sidebar()` with a left sidebar (file list + progress counter placeholder) and a main panel (context panel placeholder + label controls at the bottom)
- [ ] T008 [US1] In `tools/validation_gui/app.R`: paper selector — `selectInput` populated from `discover_papers()`; server observer loads `structure.csv` and any existing GT when selection changes, resets session state
- [ ] T009 [US1] In `tools/validation_gui/app.R`: file list panel — render one row per structure row showing `filename` and a status badge (unvisited / validated ✓ / skipped); clicking a row sets `current_index`
- [ ] T010 [US1] In `tools/validation_gui/app.R`: type button row — 8 `actionButton` widgets labelled `[1] data` through `[8] other`; the active button is highlighted; pre-filled from the machine prediction when a new file is loaded; a muted label below shows the machine prediction when it differs from the current selection
- [ ] T011 [US1] In `tools/validation_gui/app.R`: group text input — `textInput` with `updateTextInput` autocomplete sourced from unique groups in the current paper's `structure.csv` plus `"other"` and `"na"`; pre-filled from machine prediction on new file load
- [ ] T012 [US1] In `tools/validation_gui/app.R`: `is_raw` toggle — `checkboxInput` or `actionButton`; disabled (greyed out via CSS) and value forced to `FALSE` when type is not `"data"`; pre-filled from machine prediction
- [ ] T013 [US1] In `tools/validation_gui/app.R`: Save button (`actionButton`, label "Save & Next →") observer — validates that `type_gt` is set, calls `upsert_gt` + `write_gt`, marks row as validated in reactive state, advances `current_index` to next unvalidated file
- [ ] T014 [US1] In `tools/validation_gui/app.R`: Skip button observer — advances `current_index` to next file, adds index to `skipped_indices` reactive (in-memory only, no write)
- [ ] T015 [US1] In `tools/validation_gui/app.R`: progress counter — `renderText` showing `"validated N / total"` displayed prominently in the sidebar header; updates after every save

**Checkpoint**: US1 fully functional. Annotator can label files and save ground truth without keyboard shortcuts or file preview.

---

## Phase 4: User Story 4 — Resume a session after closing (Priority: P1)

**Goal**: Re-opening the tool and selecting a previously worked paper restores all saved
labels and positions the annotator on the first unvalidated file.

**Independent Test**: Save labels for 5 of 19 files, kill the R process, relaunch, select
the same paper, and verify the 5 rows are marked validated and controls load saved values.

- [ ] T016 [US4] In `tools/validation_gui/app.R`: on paper selection, merge loaded GT into reactive structure state — for each `rel_path` present in the GT, set status to `"validated"` and store `type_gt`/`group_gt`/`is_raw_gt` in a named list reactive indexed by `rel_path`
- [ ] T017 [US4] In `tools/validation_gui/app.R`: when `current_index` changes, check if the row has a saved GT entry; if yes, pre-fill type buttons / group input / is_raw toggle from the saved values; if no, pre-fill from machine prediction
- [ ] T018 [US4] In `tools/validation_gui/app.R`: on paper load, set `current_index` to the first row whose status is not `"validated"` (defaulting to index 1 if all are validated)
- [ ] T019 [US4] In `tools/validation_gui/app.R`: startup annotator dialog — show a `modalDialog` on app init prompting for annotator name/initials; block main UI until a non-empty string is submitted; store in session reactive `annotator`

**Checkpoint**: US1 + US4 both work. Full P1 scope is complete and independently testable.

---

## Phase 5: User Story 2 — Keyboard-driven labelling (Priority: P2)

**Goal**: All labelling actions accessible via keyboard only; number keys 1–8 select type,
`R` toggles `is_raw`, `G` focuses the group field, `⌘↩` saves, `Tab` skips, `⌘[` goes back.

**Independent Test**: Complete a full labelling session for paper `0956797616647519` using
only the keyboard (no mouse clicks); verify all 19 rows are saved correctly.

- [ ] T020 [US2] In `tools/validation_gui/app.R`: inject a `tags$script` block in the UI that listens for `keydown` on `document` and calls `Shiny.setInputValue("key_press", {key, timestamp}, {priority: "event"})` for keys `1`–`8`, `r`/`R`, `g`/`G`, `Enter`+`metaKey`, `[`+`metaKey`, `/`+`metaKey`; also inject a companion listener that sets `Shiny.setInputValue("text_focused", true/false)` on `focus`/`blur` of all `input[type=text]` and `input[type=search]` elements
- [ ] T021 [US2] In `tools/validation_gui/app.R`: server `observeEvent(input$key_press)` — dispatch table: keys `1`–`8` → set `rv$selected_type` to the corresponding type value (only when `input$text_focused` is not `TRUE`); key `R`/`r` → toggle `is_raw` reactive (only when type is `"data"` and not text-focused); key `G`/`g` → call `session$sendCustomMessage("focus_group", list())` (only when not text-focused)
- [ ] T022 [US2] In `tools/validation_gui/app.R`: add JS handler for `"focus_group"` message that calls `.focus()` on the group `<input>` element; wire `⌘↩` key to trigger the same observer as the Save button; wire `⌘[` to the same observer as the Back button; wire `Tab` key (when not in a text input) to the Skip observer
- [ ] T023 [US2] In `tools/validation_gui/app.R`: `⌘/` key triggers `showModal(modalDialog(...))` displaying the keyboard reference table from `quickstart.md`; modal has a "Close" button and also closes on `Escape`

**Checkpoint**: Full keyboard-only workflow is operational.

---

## Phase 6: User Story 3 — File context panel (Priority: P2)

**Goal**: Context panel renders a type-appropriate file preview, machine predictions,
folder-tree view, and sibling list — all scrollable.

**Independent Test**: Load each of the 19 files in paper `0956797616647519` in sequence;
verify each file shows a non-empty preview (or a descriptive error for any that fail to load),
the folder tree is visible with the current file highlighted, and siblings are listed.

- [ ] T024 [P] [US3] In `tools/validation_gui/preview.R`: `preview_text(path, n_lines)` — `readLines(path, n = n_lines, warn = FALSE)` wrapped in `tryCatch`/`setTimeLimit(5)`; returns a character vector; used for extensions `csv`, `tsv`, `txt`, `dat` (n=50) and `r`, `rmd`, `qmd`, `py`, `do`, `sps`, `sh`, `sql` (n=80)
- [ ] T025 [P] [US3] In `tools/validation_gui/preview.R`: `preview_structured(path, n_rows = 5)` — sources `pipeline/helper.R`, calls `read_data_head(path, n_rows)`; formats column names + first 5 rows as a printable string; used for `sav`, `dta`, `sas7bdat`, `xlsx`, `xls`; handles NULL return (plot objects) gracefully
- [ ] T026 [P] [US3] In `tools/validation_gui/preview.R`: `preview_r_object(path, ext)` — for `rds`: `readRDS(path)` + `capture.output(str(obj, max.level = 2))`; for `rda`/`rdata`: `load(path, envir = e <- new.env())` + list names + `class()` of each; all wrapped in `tryCatch`
- [ ] T027 [P] [US3] In `tools/validation_gui/preview.R`: `preview_document(path, ext)` — for `pdf`: `pdftools::pdf_text(path)[1]` truncated to 500 chars; for `docx`: `officer::read_docx(path)` → `officer::docx_summary()` → paste text cells, truncated to 500 chars; wrapped in `tryCatch`
- [ ] T028 [P] [US3] In `tools/validation_gui/preview.R`: `preview_image_tag(path, ext)` — returns an `htmltools::tags$img(src = ...)` using a `base64enc::dataURI()`-encoded image for `jpg`/`jpeg`/`png`/`gif`; returns `tags$img(src = path)` for `svg`; wrapped in `tryCatch`
- [ ] T029 [P] [US3] In `tools/validation_gui/preview.R`: `preview_archive(path, ext)` — for `zip`: `unzip(path, list = TRUE)$Name[1:min(100, ...)]`; for `tar`/`tgz`: `untar(path, list = TRUE)[1:min(100, ...)]`; for `gz`/`bz2`/`xz` (standalone): note "compressed single file"; wrapped in `tryCatch`
- [ ] T030 [US3] In `tools/validation_gui/preview.R`: `render_preview(path, ext)` — master dispatcher that calls the correct `preview_*` function based on `ext`; returns `htmltools::HTML`-wrapped output; for unknown exts returns a hex-dump of the first 256 bytes via `readBin` + `paste(format(as.hexmode(...)))` and "Preview not available" notice; all failures return a formatted error string, never `stop()`
- [ ] T031 [US3] In `tools/validation_gui/app.R`: context panel header section — `renderUI` showing `rel_path`, `ext`, human-readable file size from `file.info(path)$size`, and a row of muted badge chips for machine-predicted `type`/`group`/`is_raw`; if `is_sentinel` is `TRUE` show a prominent amber banner: "Aggregate folder — this row represents N+ files"
- [ ] T032 [US3] In `tools/validation_gui/app.R`: scrollable preview area — `renderUI` calling `render_preview(path, ext)` for the current file; wrap output in a `div` with fixed-height + `overflow-y: auto` CSS
- [ ] T033 [US3] In `tools/validation_gui/app.R`: folder-tree panel — `renderText` or `renderUI` that builds a text-art tree from `structure.csv`'s `rel_path` column, grouped by directory; marks the current file with `●` and shows `[type / group]` for each sibling; also list sibling files (same parent directory) as a flat bullet list below the tree

**Checkpoint**: Full context panel is operational. All four user stories are complete.

---

## Phase 7: Polish & Cross-Cutting Concerns

- [ ] T034 In `tools/validation_gui/app.R`: amber visual indicator on the type button row — when the annotator's `rv$selected_type` differs from the current file's machine `type`, render a small muted label "LLM predicted: \<type\>" beneath the button row
- [ ] T035 In `tools/validation_gui/gt_store.R`: in `read_gt()`, after loading, silently correct any rows where `is_raw_gt = TRUE` but `type_gt != "data"` by setting `is_raw_gt = FALSE` before returning
- [ ] T036 In `tools/validation_gui/app.R`: edge-case guards — if `file.exists(path)` is `FALSE` when rendering preview, show "File not found on disk" rather than calling `render_preview`; if `read_gt()` encounters a malformed row (wrong column count), emit `warning()` and skip that row rather than crashing
- [ ] T037 In `tools/validation_gui/app.R`: register `onStop(function() { ... })` to print a session summary to the R console: total validated, total files, count of type/group/is_raw corrections vs machine prediction, and path of ground-truth file
- [ ] T038 Update `docs/progress.md` to record feature 020 (Validation GUI) as implemented

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies — start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 — **blocks all user story phases**
- **Phase 3 (US1)**: Depends on Phase 2 completion
- **Phase 4 (US4)**: Depends on Phase 3 — extends the paper-load flow built in Phase 3
- **Phase 5 (US2)**: Depends on Phase 3 — adds keyboard dispatch on top of existing button actions
- **Phase 6 (US3)**: Depends on Phase 2 only — `preview.R` tasks T024–T029 are fully independent of US1/US4/US2 and can be worked in parallel with Phase 3 once Phase 2 is complete
- **Phase 7 (Polish)**: Depends on all story phases

### User Story Dependencies

- **US1 (P1)**: Can start after Phase 2
- **US4 (P1)**: Depends on US1 (extends reactive state and paper-load observer)
- **US2 (P2)**: Depends on US1 (keyboard actions mirror existing button actions)
- **US3 (P2)**: `preview.R` tasks (T024–T030) can start after Phase 2 in parallel with US1; `app.R` context panel tasks (T031–T033) depend on T030 being done

### Parallel Opportunities

- T003 and T004 (Phase 2): independent files, run in parallel
- T024–T029 (Phase 6 preview helpers): all target different sections of `preview.R`, run in parallel
- US3 preview helper tasks (T024–T030) can be developed concurrently with US1 tasks (T007–T015)

---

## Parallel Example: US3 Preview Helpers

```text
Run simultaneously after Phase 2:
  T024 — preview_text() for plain text / script files
  T025 — preview_structured() for tabular data (sav/dta/xlsx/xls)
  T026 — preview_r_object() for rds/rda/rdata
  T027 — preview_document() for pdf/docx
  T028 — preview_image_tag() for jpg/png/gif/svg
  T029 — preview_archive() for zip/tar/gz

Then sequentially:
  T030 — render_preview() master dispatcher (depends on T024–T029)
  T031–T033 — app.R context panel wiring (depends on T030)
```

---

## Implementation Strategy

### MVP First (US1 + US4 only)

1. Complete Phase 1: Setup (T001–T002)
2. Complete Phase 2: Foundational (T003–T006)
3. Complete Phase 3: US1 (T007–T015)
4. Complete Phase 4: US4 (T016–T019)
5. **STOP and validate**: run the tool against paper `0956797616647519`, label all 19 files,
   close and reopen, confirm 19 rows persist and controls restore saved values

### Incremental Delivery

1. Setup + Foundational → data layer ready
2. US1 + US4 → labelling works end-to-end with persistence (**MVP**)
3. US2 → keyboard shortcuts added (no regression to mouse workflow)
4. US3 → preview and context panel added (no regression to label workflow)
5. Polish → hardening and docs

---

## Notes

- `[P]` tasks operate on different files or independent sections — no merge conflicts
- Constitution Principle II: every `read.csv` call must include `colClasses = c(paper_id = "character")`
- Constitution Principle I: `write_gt()` must flush to disk on every single save — no batching
- Constitution Principle IV: `preview_structured()` must source and call `read_data_head()` from `pipeline/helper.R`, not reimplement reading logic
- `is_raw` is only meaningful for `type = "data"` — enforce in both the UI (T012) and the GT loader (T035)
- Keyboard number keys and `R`/`G` must be suppressed when a text input has focus (T020–T021)
