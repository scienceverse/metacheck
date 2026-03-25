# Research: PsychDS Conversion

**Feature**: 021-psychds-conversion
**Phase**: 0 — Pre-design research

---

## Decision 1: XML Parsing Library for GROBID TEI

**Decision**: Use `xml2` package already installed in the R environment.

**Rationale**:
- `xml2` is confirmed installed (`/Library/Frameworks/R.framework/.../xml2`).
- GROBID outputs standard TEI-XML with a declared namespace (`http://www.tei-c.org/ns/1.0`). `xml2::xml_ns_strip()` removes the namespace prefix, allowing simple XPath like `.//title` and `.//author`.
- Verified: `xml_text(xml_find_first(doc, ".//title"))` returns the correct paper title.
- No new package installation needed — satisfies constitution "no new packages" rule.

**Alternatives considered**:
- Base R `readLines()` + regex: brittle for multi-line abstract, author list, and namespace-prefixed tags. Rejected.
- `XML` package (not installed): would require installation. Rejected.

**Key extraction XPaths** (after `xml_ns_strip()`):
| Field | XPath |
|---|---|
| Title | `.//titleStmt/title[@type='main']` or `.//titleStmt/title[1]` |
| Abstract | `.//abstract//p` (join paragraphs) |
| Authors | `.//author/persName` → `forename` + `surname` |
| DOI | `.//idno[@type='DOI']` |
| Date | `.//publicationStmt//date[@type='published']/@when` |
| Keywords | `.//keywords/term` |

---

## Decision 2: JSON-LD Generation

**Decision**: Use `jsonlite::toJSON(auto_unbox = TRUE, null = "null", pretty = TRUE)`.

**Rationale**:
- Verified: `jsonlite::toJSON(list('@type' = 'PropertyValue', name = 'x', minValue = 1), auto_unbox = TRUE)` produces correct unboxed JSON-LD.
- `auto_unbox = TRUE` is required so scalar values don't become single-element arrays.
- `null = "null"` preserves explicit `NULL` fields (omit instead by using `NA` and filtering before serialisation — preferred approach: strip `NA` values before `toJSON`).
- Named lists in R map directly to JSON objects; unnamed lists map to arrays. `variableMeasured` is built as an unnamed list of named-list PropertyValue objects.

**Preferred approach for NA omission**: build each PropertyValue as a named list, then call `Filter(Negate(is.null), pv)` before including it. Do not pass `NA` values through — convert to `NULL` and filter.

**Alternatives considered**:
- Manual string construction: fragile, error-prone for nested objects. Rejected.
- `rjson` package: not installed. Rejected.

---

## Decision 3: Full Data File Reading (vs. N_DATA_READ=5)

**Decision**: Read complete data files using format-specific functions without row limits. Re-use existing format dispatch logic from `read_data_head()` in `helper.R`, but without the `n_rows`/`n_max` argument.

**Rationale**:
- `read_data_head()` is parameterised with `n_rows` — calling the same dispatch without a limit gives the full file.
- A new internal helper `read_full_data()` in `3_psychds_convert.R` mirrors the format dispatch but reads all rows. Not added to `helper.R` because full reads are PsychDS-specific (all other pipeline stages intentionally sample).
- 500 MB limit: check `file.info(path)$size` before attempting read; skip and place in `data/raw/` if exceeded.

**Haven labelled columns** — write numeric codes to CSV, capture value labels separately:
```r
# 1. Extract value labels before stripping
value_labels <- lapply(df, function(col) attr(col, "labels"))

# 2. Strip haven labels to get base R types (numeric codes preserved)
df <- haven::zap_labels(df)          # removes label attributes, keeps values
df <- haven::zap_label(df)           # removes variable-level label attributes

# 3. Write df to CSV — numeric codes intact
write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
```

---

## Decision 4: Crash-Resilient Conversion Summary

**Decision**: Append one row to `psychds/conversion_summary.csv` after each study conversion, using the same pattern as `run_0_index_bulk.R` (check for existing rows on startup to auto-resume).

**Rationale**: Constitution Principle I mandates crash-resilient incremental output. The bulk runner MUST read `conversion_summary.csv` on startup and skip `(paper_id, study_group)` pairs already present with `success = TRUE`.

**Resume logic**:
```r
done <- if (file.exists(SUMMARY_CSV)) {
  read.csv(SUMMARY_CSV, colClasses = c(paper_id = "character"))
} else {
  data.frame(paper_id = character(), study_group = character(),
             success = logical(), stringsAsFactors = FALSE)
}

# Skip if already succeeded
if (any(done$paper_id == paper_id & done$study_group == study_group & done$success)) next
```

---

## Decision 5: Study Detection Algorithm

**Decision**: Derive studies by computing `unique(structure$group[structure$type == "data"])` after ground-truth override. Each unique value becomes one study (mapped to `study-<group>/` directory).

**Single-study simplification trigger**: `length(studies) == 1` → flat layout, no `study-*` directories.

**Shared directory trigger**: `length(studies) > 1` → any file with `group %in% c("na", "other")` that fails the co-location heuristic goes to `shared/`.

**Co-location heuristic implementation**:
```r
parent_dir <- dirname(rel_path)
siblings   <- structure$group[dirname(structure$rel_path) == parent_dir]
siblings   <- siblings[!is.na(siblings) & siblings != "na" & siblings != "other"]
if (length(unique(siblings)) == 1) assign_to_study(unique(siblings)) else assign_to_shared()
```

---

## Decision 6: New Error Codes (Constitution Principle V)

The PsychDS conversion introduces two new failure modes, scoped to the conversion summary (not the pipeline `bulk_summary.csv`):

| Code | Cause |
|---|---|
| `pipeline_failed` | Paper has `success = FALSE` in `bulk_summary.csv`; upstream pipeline error, conversion skipped |
| `no_data_files` | Paper has zero `type = "data"` files after ground-truth override |

These codes appear only in `psychds/conversion_summary.csv`, not in `bulk_summary.csv`.

---

## Decision 7: New Helpers in `helper.R`

Per constitution Principle IV, two new functions used by the conversion pipeline go in `helper.R`:

**`apply_ground_truth(structure_df, paper_id)`**: merges `ground_truth/<paper_id>.csv` overrides into the structure data frame, replacing `type`, `group`, `is_raw` for validated rows. Usable by future features that also need GT-overridden classifications.

**`sanitise_keyword_value(x, max_chars = 60)`**: sanitises a string for use as a PsychDS keyword value (removes non-alphanumeric, truncates). PsychDS-specific but simple enough to share.

**`read_full_data(path)`**: reads a complete data file (all rows). This does NOT go in `helper.R` — full reads are PsychDS-conversion-specific; `helper.R`'s `read_data_head()` already handles the partial-read case.

---

## Decision 8: Documentation Plaintext Extraction (US6)

**Decision**: Add `extract_plain_text(path)` to `helper.R` as a new exported helper. Dispatches to:
- `.pdf` → `paste(pdftools::pdf_text(path), collapse = "\n")`
- `.docx` → `officer` text extraction (same pattern as existing `.extract_rich_text()` in codebook labelling)
- `.rtf` → regex-based RTF control strip (same pattern as existing `.strip_rtf()` in codebook labelling)
- Other extensions → `NULL` (no extraction attempted)

Returns a character string on success (may be empty for image-only PDFs), `NULL` on error (wrapped in `tryCatch`).

**"Text-based" detection**: determined by file extension — `.pdf`, `.docx`, `.rtf` only. No heuristic needed; non-text formats (`.csv`, `.xlsx`, `.sav`) are excluded by extension check.

**Empty result handling**: `nzchar(trimws(text))` — if FALSE, TXT file is NOT written; `provenance.json` records `txt_extraction_skipped = TRUE`, `txt_skip_reason = "no_extractable_text"`. Extraction errors record `txt_skip_reason = "extraction_error"`.

**TXT output location**: `documentation/txt/<basename>.txt` (sibling `txt/` subdirectory, not a separate top-level directory). Original in `documentation/<basename>.<ext>` is untouched.

**Rationale**:
- All three extraction tools (`pdftools`, `officer`, RTF regex) are already installed and used by the codebook labelling stage. Zero new dependencies.
- Constitution Principle IV: `extract_plain_text()` goes in `helper.R` because it is general-purpose and could be used by other future pipeline stages.
- Constitution Principle IV: the existing private helpers `.extract_rich_text()` and `.strip_rtf()` stay private (dot-prefix); `extract_plain_text()` is a thin public wrapper that delegates to them.

**Alternatives considered**:
- Inline extraction logic in `3_psychds_convert.R`: would duplicate codebook labelling code. Rejected per Principle IV.
- Making `.extract_rich_text()` / `.strip_rtf()` public: they have codebook-specific signatures. A clean wrapper is preferable.
