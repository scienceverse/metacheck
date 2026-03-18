# Data Model: Fix R File Misclassification and CSV Read Errors

**Feature**: 013-fix-r-file-misclassification
**Date**: 2026-03-18

---

## New Constant: AGGREGATE_EXT_OVERRIDE

Added to `0_index.R` alongside existing constants (`ARCHIVE_EXTS`, `LLM_BATCH_SIZE`, etc.).

```
AGGREGATE_EXT_OVERRIDE — named character vector: ext → type
```

| Extension (lowercase) | Overridden type |
|---|---|
| `r`, `rmd`, `qmd`, `py`, `m`, `do`, `sps`, `jl`, `js`, `sh`, `bash`, `pl`, `rb`, `cpp`, `c`, `h`, `java`, `scala`, `sql` | `"code"` |
| `jpg`, `jpeg`, `png`, `gif`, `bmp`, `tiff`, `tif`, `svg`, `mp4`, `avi`, `mov`, `mp3`, `wav`, `flac` | `"asset"` |
| `csv`, `sav`, `dta`, `sas7bdat`, `xlsx`, `xls`, `rds` | `"data"` |

**Lookup logic** (applied only to files expanded from aggregate sentinels):
- If `file_ext(path)` is a key in `AGGREGATE_EXT_OVERRIDE`, replace the inherited `type` with the mapped value.
- If the extension is not in the map (`.txt`, `.dat`, `.rda`, `.rdata`, unknown), retain the inherited `type` unchanged.

---

## Modified Data Flow: Aggregate Expansion (Step 7)

### Before (current behaviour)

```
sentinel { type = "data", group = "ex1" }
  → all files in folder inherit type = "data"   ← no correction
```

### After (fixed behaviour)

```
sentinel { type = "data", group = "ex1" }
  → all files in folder inherit type = "data"
  → for each file, look up ext in AGGREGATE_EXT_OVERRIDE
      - "Graphs.R"             ext = "r"    → override to type = "code"
      - "DistributionsPlot.jpeg" ext = "jpeg" → override to type = "asset"
      - "data.csv"             ext = "csv"  → already "data", no change needed (but override confirms it)
      - "notes.txt"            ext = "txt"  → NOT in map, retains "data"
```

---

## Modified Function: `sniff_delimiter()` in `helper.R`

### Before

```
line <- ""
for (i in seq_len(10)) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (nchar(trimws(line)) > 0) break     ← crashes if line = character(0)
}
# use line for delimiter detection
```

### After

```
line <- character(0)
for (i in seq_len(10)) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (length(line) == 0) break            ← EOF guard: empty file exits early
  if (nchar(trimws(line)) > 0) break      ← found non-blank line
}
if (length(line) == 0) return(",")        ← safe default for empty files
# use line for delimiter detection (same as before)
```

---

## Modified Function: `read_data_head()` in `helper.R`

The `read.delim()` call for `csv`/`txt`/`tsv`/`dat` extensions is wrapped in `suppressWarnings()` to prevent "incomplete final line found by readTableHeader" from surfacing in pipeline output.

This is scoped to only the `read.delim(...)` invocation; all other warnings within `read_data_head()` remain visible.

---

## No Schema Changes

No changes to output CSVs (`structure.csv`, `columns.csv`, `bulk_summary.csv`). The type values written (`"code"`, `"asset"`, `"data"`) already exist as valid enum values in the schema. No new columns, error codes, or file formats are introduced.
