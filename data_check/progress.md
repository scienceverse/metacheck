# Progress Log

## 2026-03-23

### Completed ✅

**019** — fix-index-labelled-stats (branch: `019-fix-index-labelled-stats`)
- Fix `Can't convert from 'value' <labelled<double>> to <labelled<double>> due to loss of precision` error: add `as.numeric()` coercion at `x_comp` assignment in `0_index.R` so haven-labelled type metadata is stripped before statistics computation — prevents vctrs rbind from encountering incompatible label mappings across columns
- Fix `arguments imply differing number of rows: 0, 1` error: cancel the sentinel mechanism when all files fall into aggregate folders — set `aggregate_df = NULL` and process all paths individually so `non_agg_relpaths` is never empty; the existing 10-call limit still guards runaway repos
- Remove 3 affected papers (`0956797618772822`, `09567976231158570`, `0956797618773095`) from `bulk_summary.csv` for reprocessing

**018** — fix-csv-codebook-parsing (branch: `018-fix-csv-codebook-parsing`)
- Extend `.find_codebook_cols()` in `helper.R` to recognise additional column-name variants: `variable_label`, `var_label`, `item` (variable column); `label_text`, `question`, `question_text`, `variable_description` (description column)
- Replace fixed-header CSV read in `parse_codebook()` with header-row lookahead: reads file without header, scans rows 1–`CODEBOOK_HEADER_LOOKAHEAD` (default 5) for a row matching the codebook-column patterns; handles multi-level / merged-header CSVs
- Add latin1 encoding fallback to CSV codebook reading (mirrors existing `read_data_head()` pattern); retries with `fileEncoding = "latin1"` when UTF-8 produces invalid bytes
- Update `sniff_delimiter()` to skip comment rows (lines starting with `#`) in addition to blank lines when probing for the delimiter
- Add `parse_method` column (`"structured"` or `"llm"`) to the data.frame returned by `parse_codebook()` and `.run_llm_chunk_loop()`; propagated to `codebook_coverage.csv` via `coverage_df` construction in `2_codebook_label.R`
- Add `CODEBOOK_HEADER_LOOKAHEAD <- 5L` constant to `2_codebook_label.R`
- Update `docs/output-schemas.md` with `parse_method` column definition

**refactor** — repo restructure (branch: `017-llm-temperature-testing`)
- Move all R source files from `data_check/` root into purpose-grouped subdirectories: `pipeline/` (core modules), `runners/` (entry-point scripts), `reports/` (report generators)
- Move generated summary CSVs (`bulk_summary.csv`, `codebook_summary.csv`) to `results/`
- Update all `source()` calls and path constants (`DATA_DIR`, `OUTPUT_DIR`, `SUMMARY_CSV`, `SWEEP_DIR`, `BULK_LOG`, `PROGRESS_CSV`) to resolve correctly when scripts are run from `data_check/` root
- Update `report_quality.R` to write `quality_report_*.md` to `results/` instead of root
- Update defaults in `report_quality.R` and `report_sweep_grand.R` to point at `results/` for input CSVs and output paths
- Archive `descriptive_statistics.R` to `_old/` (broken external deps, exploratory only)
- Delete scratch files: `0_result.txt`, `2_result.txt`, `quality_report_2026-03-19.md`
- All scripts still invoked from `data_check/` root (e.g. `Rscript runners/run_single.R`)

---

## 2026-03-19

### Completed ✅

**017** — llm-temperature-testing (branch: `017-llm-temperature-testing`)
- Add `run_sweep.R`: CLI tool to run a paper through the full pipeline N times at M temperatures, saving each (temperature, repeat) to an isolated output directory; crash-resilient via `sweep_log.csv` with resume support; exports `run_paper_sweep()` for use by bulk runner
- Add `run_sweep_bulk.R`: sequential bulk sweep across all papers in `XML_DIR`; paper-level crash-resilient log (`sweep_results/sweep_bulk_log.csv`); `N_PAPERS` cap; auto-resumes on restart; tracks `n_no_data` separately from `n_failed`
- Add `report_sweep.R`: reports pairwise col_type / label stability per temperature, quality proxies (known-type rate, codebook coverage, non-empty label rate), weighted recommendation, and always writes `sweep_report_YYYY-MM-DD.md`; exports `compute_stability()` / `compute_quality()` for grand report
- Add `report_sweep_grand.R`: grand flat CSV report across all swept papers; one row per (paper_id × temperature × pipeline stage); `no_data` temperatures emit NA metrics (not "failed"); no aggregation — post-processing friendly
- Modify `llm_batch()` in `helper.R` and three standalone `llm()` calls in `2_codebook_label.R`: read `getOption("llm_temperature")` and pass as `params = list(temperature = X)` when set
- Add `output_dir = NULL` param to `run_index()` in `0_index.R`: when non-NULL, writes outputs to specified path instead of `paper_output_dir(paper_id)`
- Add `output_dir = NULL` param to `run_codebook_label()` in `2_codebook_label.R`: same isolation pattern as `run_index()`
- Add `no_data` end state: `run_one()` detects when `columns.csv` is absent/empty after index; logs `status = "no_data"`, skips codebook stage, counts as completed for resume (not a failure)
- Add `data_check/sweep_results/` to `.gitignore`

**016** — pipeline-quality-report (branch: `016-pipeline-quality-report`)
- Add `report_quality.R`: single-script CLI report over `bulk_summary.csv`, `codebook_summary.csv`, per-paper `columns.csv`, and `codebook_coverage.csv`; four sections (bulk overview, col-type distribution, codebook coverage, timing); always writes `quality_report_YYYY-MM-DD.md`
- N/A for absent codebook file vs 0% for present-but-empty file
- Add `data_check/quality_report_*.md` to `.gitignore`

**015** — verbatim-codebook-labels (branch: `015-verbatim-codebook-labels`)
- Update `CODEBOOK_PARSE_PROMPT` in `2_codebook_label.R` to instruct the LLM to copy label text verbatim from the codebook source rather than paraphrasing or summarising it; add explicit no-rephrase rule and no-fabrication rule for variables without a description

## 2026-03-18

### Completed ✅

**fix** — increased LLM call count in codebooks
- Increased LLM call count in codebooks to fully parse PDFs

**fix** — latin1 encoding fallback in `read_data_head()` (branch: `dev`)
- `read_data_head()` in `helper.R`: after reading csv/tsv/txt/dat with default encoding, detect invalid UTF-8 bytes via `iconv(..., from="UTF-8", to="UTF-8")`; if any character column has invalid bytes, re-read the file with `fileEncoding="latin1"`
- Fixes crash `invalid multibyte string` when processing Windows-1252 encoded CSV files
- Updated `docs/pipeline.md` step 6 to document the encoding fallback behaviour

**014** — fix-multilevel-csv-headers (branch: `014-fix-multilevel-csv-headers`)
- Replace the blanket `>50% ...N` skip rule with a two-branch recovery strategy in `extract_column_info()` in `0_index.R`
- Branch 1 (sub-header found): scan first `MULTILEVEL_HEADER_LOOKAHEAD = 3` rows for a row with lower `...N` fraction AND non-numeric text labels; use its values as `column_name`; NA/empty sub-header cells fall back to the original `...N` placeholder; apply `make.unique()` for duplicate names
- Branch 2 (partial labels, no sub-header): proceed with original column names as-is; `col_header_group = NA`
- Skip only when header is entirely `...N` and no sub-header is found (genuinely headerless)
- Add `col_header_group` column to `columns.csv`: forward-filled condition/group label from row-1 name prefixes (e.g. `SHAM...3` → `SHAM`); `NA` for all non-multi-level files
- `column_name` is always the resolved raw variable name, enabling direct codebook matching with zero changes to `match_column_labels()`
- Add `MULTILEVEL_HEADER_LOOKAHEAD <- 3L` constant to the constants block
- Fix numeric-data-row false positive: sub-header candidate cells must contain non-numeric text (added `is.na(suppressWarnings(as.numeric(candidate)))` check)
- Recovers 4 previously-skipped files across 2 papers (`09567976221147259`, `09567976231151581`)
- Update `docs/output-schemas.md` — add `col_header_group` to `columns.csv` schema
- Update `docs/pipeline.md` — add `MULTILEVEL_HEADER_LOOKAHEAD` to Key Constants table

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
