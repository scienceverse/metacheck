# Implementation Plan: Fix CSV Codebook Parsing Robustness

**Branch**: `018-fix-csv-codebook-parsing` | **Date**: 2026-03-23 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/018-fix-csv-codebook-parsing/spec.md`

## Summary

CSV codebooks often have non-standard layouts (multi-level headers, extended column name variants, non-UTF-8 encoding) that defeat the current structured parsing path. The fix extends `parse_codebook()` and its helpers in `helper.R` with: a header-row lookahead scan (rows 1–5), broader column-name recognition patterns, a latin1 encoding fallback, comment-row skipping in `sniff_delimiter()`, and a `parse_method` column propagated through to `codebook_coverage.csv`.

## Technical Context

**Language/Version**: R (base R, no new packages)
**Primary Dependencies**: `haven`, `readxl`, `jsonlite` — all already installed; `helper.R` (shared helpers), `2_codebook_label.R` (coverage output)
**Storage**: CSV files on local filesystem — `outputs/<paper_id>/codebook_coverage.csv`
**Testing**: Manual smoke-test via `run_single.R`; existing bulk runner for regression check
**Target Platform**: macOS / Linux (pipeline runs locally)
**Project Type**: Data pipeline (R scripts)
**Performance Goals**: No regression on currently-passing papers; no measurable throughput change (header lookahead is CPU-only, no I/O increase)
**Constraints**: Base R only — no new packages; MAX_CODEBOOK_LLM_CALLS (3) unchanged; MAX_CODEBOOK_FILE_MB (100) unchanged
**Scale/Scope**: Affects all CSV codebooks processed by `run_codebook_label()`

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | PASS | No change to incremental write pattern |
| II. Paper ID Preservation | PASS | Not touched by this feature |
| III. Conservative Resource Limits | PASS | LLM call limits unchanged; header lookahead is CPU-only |
| IV. Centralised Shared Helpers | PASS | All changes go in `helper.R`; `parse_method` propagation in `2_codebook_label.R` |
| V. Structured Error Classification | PASS | No new error codes; `parse_method` is metadata, not an error field |

## Project Structure

### Documentation (this feature)

```text
specs/018-fix-csv-codebook-parsing/
├── plan.md              # This file
├── research.md          # Phase 0 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (files modified)

```text
pipeline/
├── helper.R             # All parsing changes (sniff_delimiter, .find_codebook_cols,
│                        #   parse_codebook — header lookahead, encoding fallback,
│                        #   parse_method column)
└── 2_codebook_label.R   # Add parse_method to coverage_df construction
```

No new files. No new packages.

**Structure Decision**: Single-project. All logic lives in `pipeline/helper.R` per Principle IV. One targeted addition in `2_codebook_label.R` to surface `parse_method` in the output schema.

## Implementation Design

### Change 1 — `sniff_delimiter()` skips comment rows

**Location**: `helper.R` lines 15–29

**Current behaviour**: Iterates up to 10 lines, stops at first non-blank line.
**New behaviour**: Also skip lines where `trimws(line)` starts with `#`.

```
for (i in seq_len(10)) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (length(line) == 0) break
  l <- trimws(line)
  if (nchar(l) > 0 && !startsWith(l, "#")) break   # ← add comment-skip
}
```

---

### Change 2 — `.find_codebook_cols()` broader patterns

**Location**: `helper.R` lines 380–391

Extend regex to include common variants observed in real data:

**Variable column**: add `variable[_ ]name`, `variable[_ ]label`, `var[_ ]label`, `item`
**Description column**: add `label[_ ]text`, `question`, `question[_ ]text`, `variable[_ ]description`

New regex:
```
variable: (?i)^(var(iable)?|name|column|field|variable_?name|varname|variable[_ ]?label|var[_ ]?label|item)$
label:    (?i)^(label|description|desc|definition|meaning|explanation|text|label[_ ]?text|question|question[_ ]?text|variable[_ ]?description)$
```

---

### Change 3 — Header-row lookahead in `parse_codebook()`

**Location**: `helper.R` inside the `csv = , tsv = , dat = {` branch of `parse_codebook()`

**Strategy**: Read the file once with `header = FALSE` so every row is a data row. Scan rows 1 through min(N, `CODEBOOK_HEADER_LOOKAHEAD`) looking for the first row where `.find_codebook_cols()` returns a non-NULL result. Use that row as the header; rows below it are the data.

Constant: `CODEBOOK_HEADER_LOOKAHEAD <- 5L` defined in `2_codebook_label.R` alongside other constants.

**Algorithm**:
```r
# Read without header; all rows are character data
raw <- read.delim(path, sep = sep, header = FALSE,
                  check.names = FALSE, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8")
# Scan rows for a matching header
header_row <- NA_integer_
for (k in seq_len(min(nrow(raw), CODEBOOK_HEADER_LOOKAHEAD))) {
  candidate <- as.character(raw[k, ])
  if (!is.null(.find_codebook_cols(candidate))) {
    header_row <- k
    break
  }
}
if (is.na(header_row)) return(NULL)   # no header found → LLM fallback
names(raw) <- as.character(raw[header_row, ])
df <- raw[(header_row + 1):nrow(raw), , drop = FALSE]
rownames(df) <- NULL
.extract_structured_codebook(df, src)
```

---

### Change 4 — Encoding fallback in `parse_codebook()`

**Location**: `helper.R` — wrap the `read.delim` (or the `header=FALSE` read from Change 3) in a latin1 retry, mirroring `read_data_head()` lines 46–55.

**Pattern**:
```r
df <- tryCatch(
  read.delim(path, sep = sep, header = FALSE, check.names = FALSE,
             stringsAsFactors = FALSE),
  error = function(e) NULL
)
if (is.null(df)) return(NULL)

# Check for invalid UTF-8 → retry with latin1
has_invalid <- any(vapply(df, function(col) {
  is.character(col) && any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
}, logical(1)))
if (has_invalid) {
  df <- tryCatch(
    read.delim(path, sep = sep, header = FALSE, check.names = FALSE,
               stringsAsFactors = FALSE, fileEncoding = "latin1"),
    error = function(e) NULL
  )
}
```

---

### Change 5 — `parse_method` column on returned data.frame

**Location**: `helper.R` — `parse_codebook()` return paths

**Structured path**: add `parse_method = "structured"` column before returning.
**LLM path** (`.run_llm_chunk_loop()`): add `parse_method = "llm"` column to returned data.frame.
**NULL returns**: unchanged (NULL means no data extracted; coverage_df row not generated).

In `2_codebook_label.R`, the `coverage_df` construction (lines 185–196) picks up `parse_method` from `codebook_vars_df`:

```r
coverage_df <- data.frame(
  paper_id          = paper_id,
  codebook_variable = codebook_vars_df$codebook_variable,
  label             = codebook_vars_df$label,
  codebook_source   = codebook_vars_df$codebook_source,
  group             = codebook_vars_df$group,
  parse_method      = codebook_vars_df$parse_method,   # ← new
  match_status      = ...,
  stringsAsFactors  = FALSE
)
```

The empty `coverage_df` fallback (lines 198–207) also gets the new column as `character(0)`.

---

### Output schema change

`codebook_coverage.csv` gains one new column:

| Column | Type | Values |
|--------|------|--------|
| `parse_method` | character | `"structured"` or `"llm"` |

This is a backward-compatible additive change. The `codebook_summary.csv` (bulk runner output) is NOT changed — `parse_method` is a per-variable detail, not a per-paper summary.

## Complexity Tracking

No constitution violations.
