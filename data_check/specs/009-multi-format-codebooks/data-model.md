# Data Model: Multi-Format Codebook Reading (009)

**Date**: 2026-03-17

---

## Entities

### CodebookFile (extended)

Represents a file classified as `codebook` or `readme` in `structure.csv`.

| Field | Type | Description |
|-------|------|-------------|
| `path` | character | Absolute path on disk |
| `ext` | character | Lowercase file extension (e.g. `"docx"`, `"pdf"`) |
| `size_mb` | numeric | File size in MB; checked against `MAX_CODEBOOK_FILE_MB` |
| `parse_status` | character | `"ok"` \| `"parse_failed"` \| `"skipped_size"` |

**New extensions supported**: `doc`, `docx`, `pdf`, `rtf`, `odt`
**Previously supported**: `csv`, `tsv`, `dat`, `xlsx`, `xls`, `sav`, `dta`, plain-text

---

### ExtractedText (new, intermediate/ephemeral)

Plain-text representation obtained from a rich-text or binary file. Not persisted to disk — used only as input to the existing LLM chunking loop.

| Field | Type | Description |
|-------|------|-------------|
| `text` | character | Full extracted plain text (may be empty string) |
| `source_path` | character | Path of the originating file |
| `extraction_method` | character | `"officer"` \| `"pdftools"` \| `"rtf_strip"` \| `"readlines"` |

**Validation**: If `nchar(trimws(text)) == 0`, the file is treated as yielding no extractable content and `parse_failed` is recorded.

---

### CodebookVariable (unchanged)

Variable definition extracted from a codebook. No schema changes.

| Field | Type | Description |
|-------|------|-------------|
| `codebook_variable` | character | Exact variable name |
| `label` | character | Human-readable description |
| `codebook_source` | character | Basename of the source file |
| `group` | character \| NA | Experiment/study context if present |

---

## State Transitions

```
CodebookFile (path, ext)
        │
        ▼
 [size check]
        │ > MAX_CODEBOOK_FILE_MB
        ├──────────────────────────────► parse_status = "skipped_size"
        │
        ▼ ≤ limit
 [format dispatch]
        │
        ├── csv/tsv/xlsx/sav/dta ──────► rule-based extraction (existing)
        │
        ├── docx ───────────────────────► officer::read_docx() → ExtractedText
        │
        ├── pdf ────────────────────────► pdftools::pdf_text() → ExtractedText
        │
        ├── rtf/odt/doc ────────────────► readLines() + strip → ExtractedText
        │
        └── other ──────────────────────► ExtractedText(text = "")
                │
                ▼
        [text empty?]
                │ yes
                ├──────────────────────► parse_status = "parse_failed", return NULL
                │
                ▼ no
        [LLM chunking loop] (existing, unchanged)
                │
                ▼
        CodebookVariable[]
                │
                ▼
        parse_status = "ok"
```

---

## Output Schema Impact

**No changes** to `labels.csv` or `codebook_coverage.csv` schemas.

The `parse_failed` status is surfaced via:
- A `warning()` message in the console (same as existing parse failures for corrupted structured files)
- `parse_codebook()` returning `NULL` (same contract as existing failures)

The caller (`run_codebook_label()`) already handles `NULL` returns from `parse_codebook()` by filtering them out via `Filter(Negate(is.null), parsed_list)`.

---

## File Layout

Only `helper.R` changes. No new files are created.

```text
data_check/
├── helper.R           # Add extract_rich_text() internal helper;
│                      # extend parse_codebook() switch() to call it
├── 2_codebook_label.R # Unchanged
└── outputs/<id>/
    ├── labels.csv          # Schema unchanged
    └── codebook_coverage.csv  # Schema unchanged
```
