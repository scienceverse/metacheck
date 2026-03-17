# Implementation Plan: Multi-Format Codebook Reading

**Branch**: `009-multi-format-codebooks` | **Date**: 2026-03-17 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/009-multi-format-codebooks/spec.md`

## Summary

Extend `parse_codebook()` in `helper.R` to extract plain text from `.docx`, `.pdf`, `.doc`, and `.rtf` codebook files before passing content to the existing LLM variable-extraction loop. Both required packages (`officer` v0.7.3, `pdftools` v3.7.0) are already installed — no new dependencies needed. The change is entirely internal to `helper.R`; no output schemas, callers, or pipeline stages change.

## Technical Context

**Language/Version**: R (base R + already-installed packages: `officer`, `pdftools`, `haven`, `readxl`)
**Primary Dependencies**: `officer` (DOCX), `pdftools` (PDF) — both already installed
**Storage**: CSV files on local filesystem; `outputs/<paper_id>/` directories
**Testing**: Manual run against papers with known DOCX/PDF codebooks
**Target Platform**: macOS (Darwin 25.3.0) — same environment as rest of pipeline
**Project Type**: R pipeline library (function-based, no CLI wrapper)
**Performance Goals**: Labelling step for DOCX/PDF codebook ≤ 2× time of equivalent plain-text codebook
**Constraints**: No new R packages; no new output columns; `MAX_CODEBOOK_FILE_MB` and `MAX_CODEBOOK_LLM_CALLS` limits apply; all errors non-fatal
**Scale/Scope**: Single-paper function; bulk runner unchanged

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Pre-design | Post-design |
|-----------|-----------|------------|
| I. Crash Resilience | ✅ All format errors wrapped in `tryCatch`; `parse_failed` returned, pipeline continues | ✅ Confirmed: `parse_codebook()` returns `NULL` on any error; caller already handles `NULL` |
| II. Paper ID Preservation | ✅ No change to ID handling | ✅ |
| III. Resource Limits | ✅ Size check applied before conversion; LLM call count unchanged | ✅ |
| IV. Centralised Helpers | ✅ New `extract_rich_text()` goes in `helper.R` | ✅ Single location |
| V. Structured Error Codes | ✅ No new paper-level error codes; `parse_failed` is a per-file console warning (same as existing) | ✅ |

**No gate violations.** Complexity Tracking table not required.

## Project Structure

### Documentation (this feature)

```text
specs/009-multi-format-codebooks/
├── plan.md              # This file
├── research.md          # Phase 0 complete
├── data-model.md        # Phase 1 complete
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
data_check/
├── helper.R             # Only file changed
│                        #   • New internal: extract_rich_text(path, ext)
│                        #   • Extended: parse_codebook() switch() block
├── 2_codebook_label.R   # Unchanged
└── outputs/<paper_id>/
    ├── labels.csv           # Schema unchanged
    └── codebook_coverage.csv # Schema unchanged
```

**Structure Decision**: Single-file change. All logic lives in `helper.R` per Constitution Principle IV.

## Implementation Design

### New internal helper: `extract_rich_text(path, ext)`

Lives in `helper.R`. Returns a character string (possibly empty) or throws (caught by caller).

```
extract_rich_text(path, ext):
  docx → requireNamespace("officer") → read_docx(path) → docx_summary() → paste text col
  pdf  → requireNamespace("pdftools") → pdf_text(path) → paste pages
  rtf  → readLines(path) → strip RTF control codes (regex) → paste
  doc  → readLines(path, warn=FALSE) → paste (binary; likely garbage → empty → parse_failed)
  odt  → readLines(path, warn=FALSE) → strip XML tags → paste
  *    → ""  (unknown; caller records parse_failed)
```

### Extended `parse_codebook()` switch block

Current `switch(ext, csv=..., xlsx=..., sav=..., dta=..., NULL)` extended to:

```
switch(ext,
  csv = , tsv = , dat = { ... existing ... },
  xlsx = , xls = { ... existing ... },
  sav = { ... existing ... },
  dta = { ... existing ... },
  docx = , pdf = , doc = , rtf = , odt = {
    text <- tryCatch(extract_rich_text(path, ext), error = function(e) "")
    if (nchar(trimws(text)) < 10) return(NULL)   # parse_failed path
    lines <- strsplit(text, "\n")[[1]]
    # hand off to existing LLM chunk loop (same code as text fallback)
  },
  NULL   # everything else falls to existing LLM readLines() path
)
```

The LLM chunk loop that follows is **not duplicated** — a refactor extracts the shared loop into a named block or sub-function so both the `readLines()` path and the new rich-text path share it.

### RTF stripping (pure R, no packages)

```r
.strip_rtf <- function(lines) {
  text <- paste(lines, collapse = "\n")
  text <- gsub("\\\\[a-z]+\\-?[0-9]*\\s?", " ", text)   # control words
  text <- gsub("\\\\[^a-z]", " ", text)                   # control symbols
  text <- gsub("[{}]", "", text)                           # braces
  text <- gsub("\\s+", " ", text)                         # collapse whitespace
  trimws(text)
}
```

## Risks & Mitigations

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| DOCX with only embedded images (no text) | Medium | `docx_summary()` returns empty `text` col → `parse_failed` |
| PDF with garbled Unicode from non-standard fonts | Low | LLM tolerates some noise; worst case extracts 0 vars → `parse_failed` |
| `.doc` binary produces garbage via `readLines()` | High | `nchar < 10` guard → `parse_failed`; expected outcome |
| `officer`/`pdftools` not available in some environments | Very low | `requireNamespace(..., quietly=TRUE)` check → `parse_failed` with warning |
| Very large DOCX/PDF hits LLM call budget | Low | Existing `MAX_CODEBOOK_LLM_CALLS` cap applies; excess chunks silently dropped |

## Testing Plan

1. Find a paper in `outputs/` whose `structure.csv` lists a `.docx` codebook → run `run_codebook_label()` → verify non-empty `labels.csv`
2. Find or synthesise a minimal test `.pdf` with 2-3 variable definitions → verify extraction
3. Confirm a corrupt/binary `.doc` produces `parse_failed` warning but does not crash
4. Confirm all existing papers (CSV/XLSX codebooks) produce identical output before and after the change
