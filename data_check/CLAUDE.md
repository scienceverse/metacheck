# data_check Development Guidelines

Auto-generated from all feature plans. Last updated: 2026-03-17

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
- 010-fix-label-ambiguity: Added R (base R only, no new packages) + `metacheck` (`llm()`), `jsonlite` (`fromJSON`, `extract_json`) — all already presen
- 009-multi-format-codebooks: Added R (base R + already-installed packages: `officer`, `pdftools`, `haven`, `readxl`) + `officer` (DOCX), `pdftools` (PDF) — both already installed
- 008-bulk-label-runners: Added R (base R only, no new packages) + `metacheck`, `haven`, `readxl` — already present; `helper.R` (shared helpers)


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
