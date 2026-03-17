# Progress Log

## 2026-03-17

### Completed ✅

**010** — fix-label-ambiguity (branch: `010-fix-label-ambiguity`)
- Add `normalize_label()` to `helper.R` — strips possessives, punctuation, and pluralising "s" for label comparison
- Add two-tier conflict resolution in `match_column_labels()` (rule-based tier first, LLM merge tier second)
- Rule tier: normalise all candidate labels; if they collapse to one string, pick the longest original label; `label_method = "merged_rules"`
- LLM tier: batch remaining `conflicting_definition` columns into single call with `LABEL_MERGE_PROMPT`; equivalent labels merged with `label_method = "merged_llm"`; genuinely conflicting labels preserved as `conflicting_definition`
- New optional argument `label_merge_prompt = NULL` on `match_column_labels()` — fully backward-compatible
- Add `LABEL_MERGE_PROMPT` constant to `2_codebook_label.R`; wire up in `run_codebook_label()`
- Update `docs/output-schemas.md` — new `label_method` values `merged_rules`, `merged_llm`; note `labelled` status covers merged rows
- Update `docs/pipeline.md` — document conflict resolution sub-step in codebook labelling stage
- BIS misplaced-label (0956797617716929): investigated and confirmed correct per source codebook — no code change needed

### In Progress 🔧

**009** — multi-format-codebooks (branch: `009-multi-format-codebooks`)
- Extend `parse_codebook()` in `helper.R` to support DOCX, PDF, RTF, ODT, DOC codebook files
- Add `.extract_rich_text()` using `officer` (DOCX) and `pdftools` (PDF) — both already installed
- Add `.strip_rtf()` for regex-based RTF text extraction (no new packages)
- Add `.run_llm_chunk_loop()` shared helper to deduplicate LLM chunking logic
- Graceful `parse_failed` for unreadable files (image-only PDFs, binary DOC); pipeline never aborts
- No output schema changes; existing CSV/XLSX/SAV/DTA/plain-text behaviour unchanged

**008** — bulk-label-runners (branch: `008-bulk-label-runners`)
- Refactor `1_data_label.R` into `run_data_label(paper_id)` function (no top-level execution)
- Confirm `2_codebook_label.R` already clean; update header comment with correct paths
- Add `run_label_bulk.R` — crash-resilient bulk runner for data-label stage, auto-resumes via `label_summary.csv`
- Add `run_codebook_bulk.R` — crash-resilient bulk runner for codebook-label stage, auto-resumes via `codebook_summary.csv`

**007** — per-id-output-structure (branch: `007-per-id-output-structure`)
- Replace flat `structure/` output directory with `outputs/<paper_id>/` per-paper layout
- Add `paper_output_dir()` helper to `helper.R` (centralised path + auto-create)
- Update `0_index.R`, `1_data_label.R`, `2_codebook_label.R` to write to `outputs/<paper_id>/`
- Migrate 59 existing CSVs from `structure/` → `outputs/<paper_id>/` via `migrate_structure.R`
- Short filenames inside per-ID dirs (no paper-ID prefix): `structure.csv`, `columns.csv`, `labels.csv`, `codebook_coverage.csv`
- `bulk_summary.csv` and resume logic unchanged

## 2026-03-16

### PRs Merged ✅

**#1** — 002-column-type-classification → dev (2h ago)
- Initialize speckit framework for feature specifications
- Add col_type classification with controlled vocabulary (continuous, binary, categorical, ordinal, date, id, text, continuous_comma_decimal, continuous_outliers_excluded, empty, unknown)
- Track coerced values in n_coerced field for comma-decimal normalization
- Route ID columns to LLM for proper classification
- Output: col_type and n_coerced columns in *_columns.csv
- **+1,087 lines | -13 lines**

**#2** — 003-qualtrics-header-skip → 002-column-type-classification (2h ago)
- Detect and handle Qualtrics triple-header CSV exports
- Strip metadata rows (1-2) before column extraction
- Add is_qualtrics field to *_columns.csv
- Remove obsolete helpers.R and sample_size_stuff/ modules
- Depends on #1
- **+873 lines | -1,074 lines**

**#4** — 004-reduce-unknown-coltypes → dev (1h ago)
- Add Rule 6a: decimal numeric columns → continuous (no LLM needed)
- Add is_numeric flag with post-LLM fallback (unknown → continuous for confirmed-numeric)
- Strengthen COLUMN_TYPE_PROMPT with examples
- Add docs/pipeline.md and docs/output-schemas.md (canonical pipeline documentation)
- **+1,072 lines | -21 lines**

**#5** — 005-codebook-column-labelling → dev
- Implement labelling from codebook to data columns using rule-based structures
- LLM analyses codebook, transforms to dataframe, rule-based matching to variables

**#6** — 006-llm-fuzzy-matching → dev
- Add specs and docs for LLM fuzzy matching feature
- Updated pipeline documentation

### Summary
46PRs merged, 2 PRs created and ready for review. Features 002–006 now live in dev, providing col_type classification, Qualtrics handling, unknown-type reduction, codebook labelling and fuzzy codebook matching.
