# Research: Fix R File Misclassification and CSV Read Errors

**Feature**: 013-fix-r-file-misclassification
**Date**: 2026-03-18

---

## Finding 1: Root cause of `.R`/image files appearing as `data`

**Decision**: The bug is the aggregate-folder type-inheritance mechanism in `0_index.R`, not an LLM prompt-size or batching issue.

**Evidence**:
- Lines 355–372 of `0_index.R`: when a sentinel (e.g. `[N_files.csv]`) is expanded back to individual files, the sentinel's `type` is assigned verbatim to every file in the folder.
- No extension-based correction is applied after expansion.
- The LLM classifies a sentinel whose dominant extension is `.csv` as `data`, and every `.R` script and `.jpeg` image inside that folder inherits `data`.
- This is confirmed by the observed output: `.R` scripts (`Graphs.R`, `Tests of Difference.R`) and `.jpeg` images (`DistributionsPlot.jpeg`) are all listed under "skipping (unreadable or empty)" in the column extraction step, which only processes `data`-typed files.

**Rationale for fix chosen**: Add an `AGGREGATE_EXT_OVERRIDE` lookup table (constant in `0_index.R`) that maps unambiguous file extensions to their definitive types. After expanding sentinels, any individual file whose extension appears in this map has its type overridden. Files with ambiguous extensions (`.txt`, `.dat`, unknown) retain the inherited sentinel type.

**Alternatives considered**:
1. Modify sentinel generation to produce multiple sentinels per folder (one per extension group) — rejected: significantly more complex, breaks the single-sentinel assumption in Step 7 expansion.
2. Run a post-hoc rule-based re-classification on ALL files (not just aggregate-expanded ones) — rejected: FR-003 prohibits overriding LLM classifications for individually classified files; the LLM has richer context than an extension lookup.
3. Patch the LLM prompt to make it aware it is classifying a sentinel — rejected: the LLM cannot know the individual extensions inside a collapsed folder; the sentinel path `[378_files.csv]` loses that information.

---

## Finding 2: Extension override map — which extensions are unambiguous

**Decision**: Categorise extensions into three groups for the override map:

| Override type | Extensions |
|---|---|
| `code` | `r`, `rmd`, `qmd`, `py`, `m`, `do`, `sps`, `jl`, `js`, `sh`, `bash`, `pl`, `rb`, `cpp`, `c`, `h`, `java`, `scala`, `sql` |
| `asset` | `jpg`, `jpeg`, `png`, `gif`, `bmp`, `tiff`, `tif`, `svg`, `mp4`, `avi`, `mov`, `mp3`, `wav`, `flac` |
| `data` | `csv`, `sav`, `dta`, `sas7bdat`, `xlsx`, `xls`, `rds` |

**Rationale**:
- `code` extensions are unambiguously analysis scripts — no psychology repo uses `.R` files to store tabular observation data.
- `asset` extensions (images, audio, video) are stimuli or figures — never tabular data.
- `data` extensions (`.sav`, `.dta`, etc.) are always statistical data files regardless of folder context.
- **Intentionally excluded** from the map: `.txt`, `.dat`, `.rda`, `.rdata` — these are genuinely ambiguous (`.rda` can contain data or plots; `.txt` can be anything). These retain the inherited sentinel type.

**Alternatives considered**:
- Include `.pdf` → rejected: can be manuscript, codebook, or supplemental; LLM classification is more accurate.
- Include `.rdata`/`.rda` → rejected: already handled by existing logic (`read_data_head` returns NULL for plot objects; LLM classifies plots as `supplemental`); adding them to the override would incorrectly force `data` on saved-plot RData files inside aggregate folders.

---

## Finding 3: Root cause of "argument is of length zero" for empty CSV

**Decision**: The bug is in `sniff_delimiter()` in `helper.R` (lines 15–27).

**Evidence**:
- `readLines(con, n = 1, warn = FALSE)` returns `character(0)` when called on an empty file (instead of `""`).
- The loop `line <- readLines(...)` assigns `character(0)` to `line`.
- `nchar(trimws(character(0)))` returns `integer(0)`.
- `if (integer(0) > 0)` throws "argument is of length zero" (R's `if` requires a length-1 logical).
- This error is caught by `tryCatch` in `read_data_head()` and surfaces as the observed warning.

**Fix**: Add a `length(line) == 0` guard before the `nchar` check inside the loop, and return `","` immediately after the loop if `line` is still length-0 (meaning the file was completely empty).

**Alternatives considered**:
- Initialise `line <- ""` before the loop — does not help because `readLines` overwrites it with `character(0)`.
- Wrap the `if` in `isTRUE(...)` — rejected: obscures intent; explicit length check is clearer.

---

## Finding 4: "Incomplete final line" warning suppression

**Decision**: Suppress only the `read.table` "incomplete final line" warning inside `read_data_head()` using `suppressWarnings()` scoped to the `read.delim()` call.

**Evidence**:
- `read.delim()` internally calls `read.table()`, which warns when the last line lacks a trailing newline. This is a purely cosmetic warning — the data is still read correctly.
- The warning is class `simpleWarning`, not a condition that signals a genuine error.

**Rationale**: Suppressing only at the `read.delim()` call site (not globally) preserves all other warnings in the pipeline. The `tryCatch` wrapper in `read_data_head` already handles genuine errors.

**Alternatives considered**:
- `withCallingHandlers` to filter specifically for "incomplete final line" messages — more precise but significantly more code for a harmless warning. `suppressWarnings` at the call site is proportionate.
- Global `options(warn = -1)` — rejected: would silence genuine warnings from all subsequent code.

---

## Finding 5: Constitution compliance

All five principles are satisfied:

| Principle | Status | Notes |
|---|---|---|
| I — Crash Resilience | ✅ Pass | No change to incremental write pattern |
| II — Paper ID Preservation | ✅ Pass | No change to paper ID handling |
| III — Resource Limits | ✅ Pass | No change to any limit constant |
| IV — Centralised Shared Helpers | ✅ Pass | `sniff_delimiter` fix in `helper.R`; override table constant added to `0_index.R` alongside existing constants |
| V — Structured Error Classification | ✅ Pass | No new failure modes introduced |

The constitution's pipeline step 5 ("Classify file paths via `llm_batch()` + `classify_by_rules()`") will need a minor wording update to mention the post-expansion type override, but this is a PATCH-level amendment.
