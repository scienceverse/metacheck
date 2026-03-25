# Validation GUI — Design Specification

## Purpose

The pipeline produces `structure.csv` for every paper, but the LLM-assigned fields
(`type`, `group`, `is_raw`) are unverified. Before treating any downstream statistics
as scientific ground truth a human must audit a representative sample.

This document specifies a minimal GUI that lets a single annotator work through
files one at a time, see enough context to make a confident call, and save corrections
that persist across sessions.

---

## Ground-Truth Fields

Only three columns require human judgement. Everything else is either deterministic
(path, filename, ext) or derived from these three decisions.

| Field | Source in pipeline | Values |
|---|---|---|
| `type` | LLM (`STRUCTURE_PROMPT`) | `data`, `codebook`, `code`, `supplemental`, `doc`, `readme`, `asset`, `other` |
| `group` | LLM (`STRUCTURE_PROMPT`) | `ex<N>`, `pilot<N>`, `other`, `na` |
| `is_raw` | LLM / heuristic | `TRUE`, `FALSE` |

---

## Workflow Overview

```
Select paper
     │
     ▼
File list (left panel)            Context panel (right panel, scrollable)
  ─ row 1  ← current             ┌─────────────────────────────────────────┐
  ─ row 2  (validated ✓)         │  rel_path                               │
  ─ row 3                        │  ext / file size                        │
  ─ ...                          │  Folder tree position                   │
     │                           │  ─────────────────────────────────────  │
     │                           │  File preview (first N lines / bytes)   │
     │                           │  (scrollable)                           │
     │                           └─────────────────────────────────────────┘
     │
     ▼
Label controls (bottom bar)
  type: [1 data] [2 code] [3 codebook] [4 supplemental] [5 doc] [6 readme] [7 asset] [8 other]
  group: [text input + autocomplete]   is_raw: [R — toggle]
  [⌘← Prev]  [Tab Skip]  [⌘↩ Save & Next]
```

Progress is written to `ground_truth/<paper_id>.csv` after every save action.
The file list shows validation status at a glance so sessions can be resumed at any
point.

---

## Input

The GUI reads one paper at a time, selected from a dropdown populated by scanning
`outputs/` for existing `structure.csv` files.

For each paper it loads:

- `outputs/<paper_id>/structure.csv` — machine-generated labels (pre-fills the form)
- `outputs/<paper_id>/columns.csv` — column names extracted from data files
  (used to auto-highlight variable names in the paper text panel)
- `ground_truth/<paper_id>.csv` — ground truth written by the annotator
  (if present, marks already-validated rows and restores their corrections)
- `/Volumes/Models/expanded_xml/<paper_id>.xml` — GROBID TEI XML for the paper
  (if present, provides the searchable paper text panel)

---

## Ground-Truth Dataset

Ground truth is a dataset in its own right, entirely separate from pipeline outputs.

Base path: `data_check/ground_truth/`
One file per paper: `ground_truth/<paper_id>.csv`

> **Note:** `data_check/ground_truth/` is currently listed in `.gitignore` to avoid
> committing exploratory/tryout annotations. Once annotation is done for real, remove
> the ignore rule and commit the directory as scientific output.

This directory should eventually be checked in to version control alongside the source
data (not generated, not ignored). It is the primary scientific output of the validation
effort.

Schema:

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `rel_path` | character | Join key back to `outputs/<paper_id>/structure.csv` |
| `type_gt` | character | Human-verified type |
| `group_gt` | character | Human-verified group |
| `is_raw_gt` | logical | Human-verified rawness |
| `validated_at` | datetime | ISO-8601 timestamp of the save action |
| `annotator` | character | Login name or initials (set at startup) |

Only rows the annotator has explicitly saved appear in this file. Rows absent from
the ground-truth file are treated as unvalidated.

---

## Context Panel — What to Show Per File

The context panel exists to give the annotator enough signal to make a confident
decision without opening the file separately. It should be scrollable and
information-dense.

### Always shown

| Item | Where it comes from |
|---|---|
| Full `rel_path` | `structure.csv` |
| `ext` | `structure.csv` |
| File size on disk | `file.info(path)$size` |
| Folder containing the file (parent directory) | derived from `rel_path` |
| Sibling files in the same folder | scan `rel_path` siblings in `structure.csv` |
| Machine prediction: `type` / `group` / `is_raw` | `structure.csv` |
| `is_sentinel` flag | `structure.csv` — if `TRUE` display a note explaining aggregate folder |

### File preview (scrollable, type-dependent)

| ext | What to render |
|---|---|
| `csv`, `tsv`, `txt`, `dat` | First 50 lines of raw text |
| `sav`, `dta`, `sas7bdat` | Column names + first 5 rows via `haven::read_*` |
| `xlsx`, `xls` | Sheet names + first 5 rows of sheet 1 via `readxl::read_excel` |
| `rds` | `class()` + `str()` limited to depth 2 |
| `rda`, `rdata` | Object names + `class()` of each |
| `r`, `rmd`, `qmd`, `py`, `do`, `sps` | First 80 lines of raw text |
| `docx`, `pdf` | First 500 chars of extracted text (officer / pdftools already installed) |
| `jpg`, `png`, `gif`, `svg` | Inline image thumbnail |
| `zip`, `tar`, `gz` | Archive member list (up to 100 entries) |
| everything else | Hex dump of first 256 bytes, plus "preview not available" notice |

If reading fails for any reason, display the error message in place of the preview —
never crash.

### Paper text panel (searchable, auto-highlighted)

When a GROBID TEI XML exists for the paper at `/Volumes/Models/expanded_xml/<paper_id>.xml`,
a collapsible **Paper text** panel is shown above the folder tree. It displays the
paper's title, abstract, and body text (up to 200 paragraphs).

**Column name auto-highlighting (green)**
When the panel loads, all unique `column_name` values from
`outputs/<paper_id>/columns.csv` (≥ 2 chars) are highlighted in green throughout
the paper text. This makes it easy to spot where the paper defines or references
its own variables.

**Search (yellow)**
A search box above the text filters matches in yellow on every keystroke.
Yellow highlights render on top of green column highlights.
A match counter (`N matches` / `no matches`) appears in the panel summary line.

Both search and highlighting are **fully client-side** (no Shiny round-trip per
keystroke). The panel renders once per paper load; only typing triggers JS, not a
server re-render.

---

### Folder tree context

Show a condensed folder-tree view of the entire repository (from `structure.csv`)
with the current file highlighted. This lets the annotator see the repo layout
without navigating away.

```
Power_Intuitions/
  Analysis_data__scripts_both/
    ► Analyse survey 2.R           [code / ex2]
      Analyses Study 1.R           [code / ex1]
    ● data study 1.csv             ← current file
      data study 2.csv             [data / ex2]
  Qualtrics_data/
    Questionaire_1.csv             [data / ex1]
  survey/
    codeBook study 1.docx          [codebook / ex1]
```

---

## Label Controls

### `type` — number-key buttons

Eight buttons displayed in a single row. Each button shows its key and label:

```
[1] data   [2] code   [3] codebook   [4] supplemental   [5] doc   [6] readme   [7] asset   [8] other
```

Pressing the corresponding number key (`1`–`8`) selects that type immediately —
no modifier needed. The active button is highlighted. If the annotator's selection
differs from the machine prediction, the prediction is shown in a muted label below
the buttons ("LLM predicted: supplemental") for reference.

Number keys are captured only when focus is not inside a text input field.

### `group` — text input with autocomplete

Autocomplete suggestions are derived from groups already present in the current
paper's `structure.csv` plus the fixed values `other` and `na`. Free-text entry
is allowed for novel groups (e.g. `ex3`, `pilot2`).

Pre-filled with the machine prediction. Press `G` to move keyboard focus here from
anywhere in the form.

### `is_raw` — toggle (TRUE / FALSE)

Relevant only when `type = "data"`. Greyed out and forced to `FALSE` for all other
types (non-data files cannot be raw data).

Press `R` to toggle. Pre-filled with the machine prediction.

---

## Keyboard Reference (macOS)

| Key | Action |
|---|---|
| `1` – `8` | Select type (data / code / codebook / supplemental / doc / readme / asset / other) |
| `R` | Toggle `is_raw` |
| `G` | Focus the group text input |
| `⌘↩` | Save current labels and advance to next unvalidated file |
| `Tab` | Skip (advance without saving; row stays unvalidated) |
| `⌘[` | Go to previous file |
| `⌘/` | Show this keyboard reference as an overlay |

Keys `1`–`8`, `R`, and `G` are intercepted globally within the app window
(i.e. not when focus is inside a text input).

---

## Navigation

| Action | Key | Button | Behaviour |
|---|---|---|---|
| Save & advance | `⌘↩` | [Save & Next →] | Writes row to `ground_truth/<paper_id>.csv`, moves to next unvalidated file |
| Skip | `Tab` | [Skip] | Moves to next file without saving |
| Previous | `⌘[` | [← Prev] | Returns to previous file; restores saved GT or machine prediction |
| Jump to file | — | Click row in file list | Navigates directly to that file |

The file list panel marks each row:
- ✓ green — validated (present in ground-truth file)
- – grey — skipped this session
- (blank) — not yet visited

---

## Session Persistence

On startup the GUI reads `ground_truth/<paper_id>.csv` (if it exists) and marks
already-validated rows. The annotator can close and reopen at any time; progress is
never lost.

A session summary is printed to console on exit:

```
Session complete.
  Validated:   12 / 19 files
  Corrections: 3  (type: 2, group: 1, is_raw: 0)
  Saved to: ground_truth/0956797616647519.csv
```

---

## Implementation Notes

### Technology

Shiny (`shiny` package) is the natural fit: it runs locally in a browser tab, is
already part of the R ecosystem, and gives scrollable panels, keyboard shortcut
capture via JavaScript, and image rendering with no external dependencies beyond
what is already installed.

`bslib` can be used for layout; it is lightweight and handles the button-row well.

No server, no database — all state lives in the ground-truth CSV.

### Key implementation concerns

1. **File reading must be non-blocking.** Use `tryCatch` around every preview read;
   display a degraded-but-informative fallback rather than freezing the UI.

2. **`paper_id` is always character.** Use `colClasses = c(paper_id = "character")`
   when reading any CSV.

3. **Sentinel rows** (`is_sentinel = TRUE`) represent collapsed folders with > 50
   files. The context panel must display this prominently and note that the label
   applies to the folder as a whole, not a single file.

4. **Concurrent annotation** is out of scope. The ground-truth CSV is written on
   every save and read only at startup; concurrent writers would corrupt it.

5. **`is_raw` locking.** When `type ≠ "data"`, the `R` key and toggle are disabled
   and the stored value is always `FALSE` regardless of what the machine predicted.

6. **Progress display.** Show `validated / total` prominently (e.g. in the header)
   so the annotator always knows where they stand within the paper.

7. **Keyboard capture in Shiny** requires a small JavaScript snippet injected via
   `tags$script` that listens for `keydown` and calls `Shiny.setInputValue()`.
   This is straightforward and does not require any additional packages.

8. **Paper text search must not be a reactive dependency.** The search input in
   the paper text panel must use a plain HTML `<input>` with `oninput` calling a
   JavaScript function (`xmlSetSearch`) directly — not `Shiny.setInputValue`.
   Making the search value a Shiny reactive input would re-render the full XML
   panel on every keystroke, freezing the UI for papers with large body text.

9. **Column auto-highlighting is injected via inline `<script>` in `renderUI`.**
   The column name list is serialised to JSON and passed to `window.xmlSetColumns()`
   in a `<script>` tag that is part of the `renderUI` output. This ensures the
   script runs immediately after the DOM is updated, with no timing issues.
   The `xml_text_content` div stores the raw (HTML-escaped) paper text; JS reads
   `el.textContent` (which strips any existing mark tags) on each render call to
   get the clean base text.

---

## Out of Scope (v1)

- Multi-annotator reconciliation
- Cross-paper batch review
- Validation of `columns.csv` fields (col_type, etc.)
- Agreement statistics / inter-rater reliability
- Export to formats other than CSV
