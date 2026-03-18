# Progress Log

## 2026-03-18

### Completed ✅

**013** — fix-r-file-misclassification (branch: `013-fix-r-file-misclassification`)
- Add `AGGREGATE_EXT_OVERRIDE` constant to `0_index.R`: named vector mapping 40 file extensions to their definitive type (`code`, `asset`, or `data`)
- Apply override after aggregate sentinel expansion in Step 7 of `run_index()`: files with unambiguous extensions (`.R`→code, `.jpeg`→asset, `.csv`→data, etc.) get the correct type regardless of what the LLM assigned to the sentinel
- Root cause: sentinel type was inherited verbatim by all files in a collapsed aggregate folder; for paper `09567976211040491` this caused 378 files (incl. `.R` scripts and `.jpeg` images) to reach column extraction; fixed to 340 true data files
- Fix `sniff_delimiter()` in `helper.R`: guard against `character(0)` returned by `readLines` on empty files — prevents "argument is of length zero" error on zero-byte CSVs (e.g. `PickupsBehavProf.csv`)
- Suppress "incomplete final line" cosmetic warning from `read.table` inside `read_data_head()` with `suppressWarnings()`
- Update `docs/pipeline.md` Step 5 and constants table
- Fix `sanitize_name()` in `0_index.R`: strip non-alphanumeric characters (`;`, `:`, `?`, etc.) from each word token after splitting on whitespace, so folder names like `I Hear My Voice; Therefore` → `I_Hear_My_Voice_Therefore` instead of `I_Hear_My_Voice;_Therefore`; also extend the trigger condition to fire for folders containing special characters even if they have no spaces
- Fix regex crash in sanitize loop: `sub(paste0("^", d, "/"), ...)` used the folder path as a regex pattern, causing "Missing ')'" errors when folder names contained parentheses (e.g. `Follow-up_2020-05a_(Constructs_related_to`); replaced with `startsWith` + `substr` string operations

## 2026-03-17

### Completed ✅

**012** — single-dataset-runner (branch: `012-single-dataset-runner`)
- Add `run_single.R` — single-command entry point that runs the full pipeline (index + codebook label) for one randomly selected paper
- Selects a random ID from `XML_DIR`, runs `run_index()` then `run_codebook_label()`, prints stage status and output path
- Graceful error handling for all known error codes; Stage 2 auto-skipped if `columns.csv` absent

**011** — merge-columns-output (branch: `011-merge-columns-output`)
- Delete `1_data_label.R` and `run_1_label_bulk.R` — stage 1 was overwriting stage 0's rich `columns.csv` (23 cols) with a thin 5-col version; stage 1 provided no unique value
- Recovery: re-run `run_index(paper_id, download=FALSE)` for all 47 papers whose `columns.csv` was thinned by stage 1
- Remove stale references to deleted files in `helper.R`, `2_codebook_label.R`, `run_2_codebook_bulk.R`
- Update `docs/pipeline.md` — remove stage-1 rows from scripts table and constants table

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
