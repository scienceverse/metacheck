# Research: Multi-Format Codebook Reading (009)

**Date**: 2026-03-17

---

## Q1: What tools are available for DOCX text extraction?

**Decision**: Use `officer::read_docx()` + `docx_summary()` (already installed, v0.7.3)

**Rationale**: `officer` is already present in the R library. `read_docx(path)` loads a `.docx` file and `docx_summary()` returns a data.frame with a `text` column containing all paragraph and table-cell text. This covers the vast majority of codebook content (tables and paragraphs). No new packages needed.

**Approach**:
```r
doc  <- officer::read_docx(path)
summ <- officer::docx_summary(doc)
text <- paste(summ$text[nzchar(trimws(as.character(summ$text)))], collapse = "\n")
```

**Alternatives considered**:
- `docxtractr` — not installed; `officer` is a superset
- `pandoc` system binary — not found on this system (`pandoc --version` fails)
- `antiword` system binary — not found on this system

---

## Q2: What tools are available for PDF text extraction?

**Decision**: Use `pdftools::pdf_text()` (already installed, v3.7.0)

**Rationale**: `pdftools` is already present. `pdf_text(path)` returns a character vector, one element per page, containing all selectable text. Pages are concatenated for LLM processing. For image-only PDFs (scanned), `pdf_text()` returns empty strings — this is handled gracefully by returning `NULL` (zero variables extracted, `parse_failed` recorded).

**Approach**:
```r
pages <- pdftools::pdf_text(path)
text  <- paste(pages, collapse = "\n")
```

**Alternatives considered**:
- `pdftotext` system binary — not found on this system
- `tika` R package — not installed; overkill for this use case
- `pdftools::pdf_ocr_text()` — available but requires Tesseract; OCR is out of scope

---

## Q3: What about `.doc` (old binary Word format)?

**Decision**: Attempt to read as raw text; if content is not extractable (binary gibberish), record `parse_failed` and skip.

**Rationale**: Old `.doc` files are binary (BIFF/OLE2 format). No suitable tool is installed (`antiword` not found, LibreOffice not found, pandoc binary not found). `officer` only handles `.docx`. A `readLines()` attempt will either produce garbage or throw an error — both are caught and result in `parse_failed`. This is acceptable per spec FR-003.

**Note**: If `antiword` or LibreOffice becomes available on this machine in the future, `.doc` support can be upgraded cheaply by calling `system2()`.

---

## Q4: What about `.rtf` (Rich Text Format)?

**Decision**: Read with `readLines()` and strip RTF control codes via regex.

**Rationale**: RTF is a text-based format (starts with `{\rtf`). While complex RTF can be hard to parse perfectly, a simple control-code stripper recovers most readable text for LLM ingestion — good enough for variable-definition extraction. If stripping yields no usable content, fall through to `parse_failed`.

**RTF stripping approach** (regex-based):
1. Remove RTF control groups: `\{[^{}]*\}` (nested groups)
2. Remove control words: `\\[a-z]+\-?[0-9]*\s?`
3. Remove remaining backslash escapes
4. Collapse whitespace

**Alternatives considered**:
- System `unrtf` binary — not found on this system
- Skip RTF entirely — viable but loses some coverage; easy text-stripping is low-cost

---

## Q5: Does adding `officer` and `pdftools` violate the "no new packages" constraint?

**Decision**: No violation — both packages are **already installed** in this R environment.

**Rationale**: The CLAUDE.md constraint "no new packages" means don't install new dependencies. `officer` (v0.7.3) and `pdftools` (v3.7.0) are present in `installed.packages()`. Using them with `library()` or `requireNamespace()` requires no new installation.

**Implementation note**: Use `requireNamespace("officer", quietly = TRUE)` rather than `library()` to keep the dependency soft — if the package is somehow absent, fall through to `parse_failed` gracefully.

---

## Q6: Constitution compliance

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ | All format errors caught with `tryCatch`; pipeline continues |
| II. Paper ID Preservation | ✅ | No change to ID handling |
| III. Resource Limits | ✅ | `MAX_CODEBOOK_FILE_MB` check applies before conversion; `MAX_CODEBOOK_LLM_CALLS` unchanged |
| IV. Centralised Helpers | ✅ | New `extract_text_from_binary()` helper goes in `helper.R` |
| V. Structured Error Codes | ⚠️ | `parse_failed` is a new per-file status (not a paper-level error code). No new paper-level error code needed since existing codes cover all paper-level outcomes. `parse_failed` is recorded in the codebook coverage table, not in `bulk_summary.csv`. |

---

## Summary of implementation approach

The change is entirely within `parse_codebook()` in `helper.R`. A new internal helper `extract_rich_text(path, ext)` handles the format-specific extraction:

| Extension | Tool | Fallback |
|-----------|------|---------|
| `.docx` | `officer::read_docx()` + `docx_summary()` | `parse_failed` on error |
| `.doc` | `readLines()` attempt | `parse_failed` (binary; likely empty) |
| `.pdf` | `pdftools::pdf_text()` | `parse_failed` on empty/error |
| `.rtf` | `readLines()` + regex strip | `parse_failed` if yield < 10 chars |
| `.odt` | `readLines()` attempt (XML inside ZIP) | `parse_failed` on error |

Once plain text is extracted, it feeds directly into the existing LLM chunking loop — no other changes to `2_codebook_label.R` or output schemas.
