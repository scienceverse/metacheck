# Research: Validation GUI

**Feature**: 020-validation-gui | **Date**: 2026-03-23

No external unknowns requiring agent dispatch — all dependencies are already installed in
the project. The questions below were resolved from first principles and existing project
conventions.

---

## Decision 1: UI framework — bslib vs miniUI

**Decision**: `bslib`

**Rationale**: `bslib` is actively maintained by the Shiny team, ships a stable Bootstrap 5
layout API, and handles a horizontal button-row (the type selector) cleanly via
`layout_columns()`. `miniUI` is in maintenance mode and was designed for gadgets embedded in
RStudio; it is not the right tool for a standalone full-tab app.

**Alternatives considered**:
- `miniUI` — rejected: maintenance mode; cramped layout primitives; designed for gadgets
- Raw `fluidPage` + Bootstrap CSS — viable fallback if `bslib` is absent, but bslib is already
  installed and adds nothing new to the dependency list

---

## Decision 2: Keyboard shortcut capture in Shiny

**Decision**: Inject a small JavaScript snippet via `tags$script` in the UI that listens for
`keydown` on `document` and calls `Shiny.setInputValue("key_press", ...)`. The server observes
`input$key_press` and dispatches on the key value. Focus state (whether a text input is active)
is tracked by a companion JS listener on `focus`/`blur` events that updates
`Shiny.setInputValue("text_focused", TRUE/FALSE)`.

**Rationale**: Shiny has no built-in global keyboard shortcut API. A `tags$script` injection is
the idiomatic Shiny pattern and requires zero additional packages. The focus-tracking companion
is necessary to prevent number keys from being swallowed while the annotator is typing in the
group field.

**Alternatives considered**:
- `shinyjs` package — adds a dependency for something achievable with 15 lines of vanilla JS
- `keys` package — unmaintained; no advantage over direct JS injection

---

## Decision 3: File preview strategy

**Decision**: Reuse `read_data_head()` from `pipeline/helper.R` for structured formats
(csv/tsv/txt/sav/dta/xlsx/xls/rds/rda); add a thin preview wrapper in `preview.R` for
formats not handled by `read_data_head()` (pdf, docx, images, archives). All reads are
wrapped in `tryCatch` and time-limited via `setTimeLimit()` to prevent UI freezes.

**Rationale**: `read_data_head()` already handles the full set of tabular formats the pipeline
processes. Re-implementing that logic in the GUI would violate Constitution Principle IV.
Non-tabular formats (PDF, DOCX, images) require separate handling that is not in
`read_data_head()` and is appropriate to add in `preview.R`.

**Preview limits by type**:

| Format | Limit | Mechanism |
|--------|-------|-----------|
| CSV / TSV / TXT / DAT | First 50 lines | `readLines(n = 50)` |
| SAV / DTA / SAS7BDAT | Column names + 5 rows | `read_data_head(path, n_rows = 5)` |
| XLSX / XLS | Sheet names + 5 rows of sheet 1 | `read_data_head(path, n_rows = 5)` |
| RDS | `class()` + `str(max.level = 2)` | `readRDS()` + `capture.output(str(...))` |
| RDA / RDATA | Object names + `class()` of each | `load()` into temp env |
| R / RMD / QMD / PY / DO / SPS | First 80 lines | `readLines(n = 80)` |
| DOCX | First 500 chars extracted text | `officer::read_docx()` + `officer::docx_summary()` |
| PDF | First 500 chars extracted text | `pdftools::pdf_text(pages = 1)` |
| JPG / PNG / GIF / SVG | Inline image via `renderImage` | `base64enc::dataURI()` or `img` tag |
| ZIP / GZ / TAR | Archive member list (≤ 100 entries) | `unzip(list = TRUE)` / `untar(list = TRUE)` |
| Everything else | "Preview not available" + file size | — |

**Alternatives considered**:
- Running preview in a background process (`callr`) — adds a dependency; overkill for
  a single-user local tool; `setTimeLimit()` is sufficient
- Loading full file then truncating — explicitly rejected; violates the memory constraint
  for large files

---

## Decision 4: Ground-truth write strategy

**Decision**: Read-modify-write the full `ground_truth/<paper_id>.csv` on every save.
Load the existing file at session start; keep rows as a data.frame in server memory;
on each save, upsert the relevant row (match on `rel_path`) and write the entire
data.frame back to disk with `write.csv(..., row.names = FALSE)`.

**Rationale**: Papers have at most ~200 files. Re-writing a ~200-row CSV on each save takes
< 5 ms and is far simpler than append-only strategies that require deduplication on reload.
The simplicity is worth the trivial write overhead.

**Alternatives considered**:
- Append-only writes + deduplication on read — more complex; only justified at much larger scale
- SQLite — adds a dependency; disproportionate to problem size

---

## Decision 5: `is_raw` enforcement for non-data files

**Decision**: When `type` is anything other than `data`, the `is_raw` toggle is visually
disabled (greyed out via `shinyjs::disabled()` or CSS class) and the effective stored value
is always `FALSE`. Any pre-existing ground-truth row with `is_raw_gt = TRUE` for a non-data
file is silently corrected to `FALSE` on load.

**Rationale**: The concept of "raw data" is meaningless for code, documentation, or
supplemental files. Allowing `is_raw = TRUE` for non-data files would produce nonsensical
ground-truth labels.

**Note**: `shinyjs` is already installed; if not, the same effect is achievable with a
CSS opacity class + JS `pointer-events: none`.

---

## Decision 6: Session-scoped "skipped" status

**Decision**: Skipped files are tracked in a reactive vector in server memory only. Skip
status is not written to the ground-truth CSV and is lost when the session ends.

**Rationale**: Skipping means "I chose not to decide right now" — it is a temporary
navigation state, not a label. Persisting it would pollute the ground-truth dataset with
non-decisions and complicate the schema.
