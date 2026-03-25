# data_check Development Guidelines

Auto-generated from all feature plans. Last updated: 2026-03-25

## Active Technologies
- R (base R only — no new packages; `haven`/`readxl`/`jsonlite` already present) + `llm_batch()`, `extract_json()` (existing helpers in `helper.R`); `jsonlite::fromJSON` (005-codebook-column-labelling)
- `structure/<paper_id>_labels.csv`, `structure/<paper_id>_codebook_coverage.csv` (new); reads existing `_structure.csv` and `_columns.csv` (005-codebook-column-labelling)
- R (base R only — no new packages) + `llm()` from `metacheck`; `jsonlite::fromJSON`, `extract_json()` — all already presen (006-llm-fuzzy-matching)
- Extends `structure/<paper_id>_labels.csv` with new `label_method` column; no new files (006-llm-fuzzy-matching)
- R (base R only, no new packages) + `metacheck`, `haven`, `readxl`, `jsonlite` — all already presen (007-per-id-output-structure)
- CSV files on local filesystem; `outputs/<paper_id>/` directories (007-per-id-output-structure)
- R (base R only, no new packages) + `metacheck`, `haven`, `readxl` — already present; `helper.R` (shared helpers) (008-bulk-label-runners)
- CSV files on local filesystem; `outputs/<paper_id>/` directories (from feature 007) (008-bulk-label-runners)
- R (base R + already-installed packages: `officer`, `pdftools`, `haven`, `readxl`) + `officer` (DOCX), `pdftools` (PDF) — both already installed (009-multi-format-codebooks)
- R (base R only, no new packages) + `metacheck` (`llm()`), `jsonlite` (`fromJSON`, `extract_json`) — all already presen (010-fix-label-ambiguity)
- CSV files; `outputs/<paper_id>/labels.csv` (modified in-place by pipeline) (010-fix-label-ambiguity)
- CSV files; `outputs/<paper_id>/columns.csv` restored in-place (011-merge-columns-output)
- R (base R only — no new packages) + `0_index.R`, `2_codebook_label.R`, `helper.R` — all already presen (012-single-dataset-runner)
- CSV files on local filesystem under `data_check/outputs/<paper_id>/` (012-single-dataset-runner)
- R (base R only — no new packages) + `0_index.R`, `helper.R` — both already present; no external packages added (013-fix-r-file-misclassification)
- CSV files on local filesystem (`outputs/<paper_id>/`) (013-fix-r-file-misclassification)
- R (base R only — no new packages) + `readxl` (already present), `haven` (already present) — `read_data_head()` in `helper.R` unchanged (014-fix-multilevel-csv-headers)
- CSV files on local filesystem — `outputs/<paper_id>/columns.csv`, `outputs/<paper_id>/structure.csv` (014-fix-multilevel-csv-headers)
- R (base R, no new packages) + `metacheck` (`llm()`), `jsonlite` — already presen (015-verbatim-codebook-labels)
- CSV files on local filesystem — `outputs/<paper_id>/labels.csv`, `outputs/<paper_id>/codebook_coverage.csv` (015-verbatim-codebook-labels)
- R (base R only — no new packages) + `haven`, `readxl`, `jsonlite` — all already present; not needed for this feature (read-only CSV reporting) (016-pipeline-quality-report)
- CSV files on local filesystem — `bulk_summary.csv`, `codebook_summary.csv`, `outputs/<paper_id>/columns.csv`, `outputs/<paper_id>/codebook_coverage.csv` (016-pipeline-quality-report)
- R (base R only — no new packages) + `metacheck` (`llm()`), `haven`, `readxl`, `jsonlite` — all already installed; `helper.R`, `0_index.R`, `2_codebook_label.R` sourced at runtime (017-llm-temperature-testing)
- CSV files on local filesystem under `sweep_results/<paper_id>/`; new `sweep_bulk_log.csv` at `sweep_results/sweep_bulk_log.csv` (017-llm-temperature-testing)
- R (base R, no new packages) + `haven`, `readxl`, `jsonlite` — all already installed; `helper.R` (shared helpers), `2_codebook_label.R` (coverage output) (018-fix-csv-codebook-parsing)
- CSV files on local filesystem — `outputs/<paper_id>/codebook_coverage.csv` (018-fix-csv-codebook-parsing)
- R (base R only, no new packages) + `haven` (already installed) — source of the labelled type; vctrs (transitively via haven) — source of the precision error on rbind (019-fix-index-labelled-stats)
- CSV files — `outputs/<paper_id>/columns.csv`, `results/bulk_summary.csv` (019-fix-index-labelled-stats)
- R (base R + already-installed packages: `shiny`, `bslib`, `haven`, + `shiny` (UI + server), `bslib` (layout/theming), `haven`/`readxl` (020-validation-gui)
- Local CSV files; `ground_truth/<paper_id>.csv` per paper; no database (020-validation-gui)
- R 4.5 (base R only — no new packages) + `haven`, `readxl`, `jsonlite`, `xml2`, `pdftools`, `officer` — all already installed (021-psychds-conversion)
- Local filesystem — `data_check/psychds/<paper_id>/` output roo (021-psychds-conversion)

- R (base R, no new packages) + `helper.R` (`classify_col_type_rules()`), `0_index.R` (`COLUMN_TYPE_PROMPT`, `run_index()`) (004-reduce-unknown-coltypes)

## Project Structure

```text
src/
tests/
```

## Commands

# Add commands for R (base R, no new packages)

## Code Style

R (base R, no new packages): Follow standard conventions

## Recent Changes
- 021-psychds-conversion: Added R 4.5 (base R only — no new packages) + `haven`, `readxl`, `jsonlite`, `xml2`, `pdftools`, `officer` — all already installed
- 021-psychds-conversion: Added [if applicable, e.g., PostgreSQL, CoreData, files or N/A]
- 020-validation-gui: Added R (base R + already-installed packages: `shiny`, `bslib`, `haven`, + `shiny` (UI + server), `bslib` (layout/theming), `haven`/`readxl`


<!-- MANUAL ADDITIONS START -->

## Pipeline Documentation

The `docs/` directory contains the canonical documentation for this pipeline.
**Keep these files in sync whenever the workflow changes.**

| File | What it documents | Update when... |
|---|---|---|
| `docs/pipeline.md` | End-to-end flow from paper ID to CSV outputs, all constants, resource limits, retry behaviour | Any stage is added/removed/reordered; a constant changes value; retry logic changes; a new LLM prompt is added |
| `docs/output-schemas.md` | Column definitions for `_structure.csv`, `_columns.csv`, `bulk_summary.csv`; all type/group enum values | A column is added/removed/renamed in any output CSV; a new `col_type`, file `type`, `group`, or error code is introduced |

### Update rules

- When adding a pipeline stage → add a step to the flow diagram in `pipeline.md`
- When changing a constant (e.g. `N_DATA_READ`, `LLM_BATCH_SIZE`, resource limits) → update the constants table in `pipeline.md`
- When adding a new `col_type` → add it to the Column Types table in `output-schemas.md`
- When adding a new file classification `type` or `group` → update the File Types / Groups tables in `output-schemas.md`
- When adding a new error code → update the Error Codes table in `output-schemas.md`
- When adding a new output CSV column → add it to the relevant schema table in `output-schemas.md`
- When committing a new feature or writing a PR → add it to/update `progress.md`
- All PRs MUST target `dev`, not `main`


<!-- MANUAL ADDITIONS END -->
