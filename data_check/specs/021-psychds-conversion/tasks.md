# Tasks: PsychDS Conversion

**Input**: Design documents from `/specs/021-psychds-conversion/`
**Prerequisites**: plan.md ✅ spec.md ✅ research.md ✅ data-model.md ✅

**Tests**: Not requested — no test tasks included.

**Organization**: Tasks grouped by user story. US1 is MVP; later stories are additive on top of it. US6 (documentation TXT extraction) adds new tasks starting at T037.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1–US6)

---

## Phase 1: Setup (Shared Helpers)

**Purpose**: New shared utilities in `helper.R` — required by all user stories.

**⚠️ CRITICAL**: Foundational work that BLOCKS all user story implementation.

- [x] T001 Add `apply_ground_truth(structure_df, paper_id)` to `pipeline/helper.R` — reads `ground_truth/<paper_id>.csv` if present, overwrites `type`/`group`/`is_raw` for validated rows, adds `ground_truth_validated` column, no-op if file absent
- [x] T002 [P] Add `sanitise_keyword_value(x, max_chars = 60)` to `pipeline/helper.R` — strips extension, removes all non-alphanumeric characters, truncates to `max_chars`; returns `""` for all-special inputs
- [x] T037 Add `extract_plain_text(path)` to `pipeline/helper.R` — checks extension: `.pdf` → `paste(pdftools::pdf_text(path), collapse = "\n")`; `.docx` → officer text extraction (reuse `.extract_rich_text()` pattern from `parse_codebook()`); `.rtf` → regex RTF control strip (reuse `.strip_rtf()` pattern); other extensions → `NULL`; wraps each in `tryCatch`, returns `NULL` on error; returns character string (possibly empty) on success

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Skeleton + core infrastructure for `pipeline/3_psychds_convert.R`. No user story can be implemented without this.

**⚠️ CRITICAL**: All Phase 2 tasks must complete before Phase 3+.

- [x] T003 Create `pipeline/3_psychds_convert.R` skeleton — `source()` calls for `helper.R`, define constants (`PSYCHDS_OUT_DIR <- "./data_check/psychds"`, `DATA_SIZE_LIMIT_MB <- 500`, `NUMERIC_TYPES`, `CATEGORICAL_TYPES`, error codes `pipeline_failed`/`no_data_files`), stub `convert_psychds()` signature
- [x] T004 [P] Implement `read_full_data(path)` in `pipeline/3_psychds_convert.R` for text formats only (csv/tsv/txt/dat) — mirrors `read_data_head()` dispatch but without row limit; returns `list(df, method, haven_labels = NULL)`; returns `NULL` on read failure
- [x] T005 [P] Implement `write_json(obj, path)` in `pipeline/3_psychds_convert.R` — `jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null")` written to `path`; creates parent directories if needed
- [x] T006 [P] Implement `parse_grobid_xml(xml_path)` in `pipeline/3_psychds_convert.R` — uses `xml2::read_xml()` + `xml2::xml_ns_strip()`; extracts title (`.//titleStmt/title[@type='main']`), abstract (`.//abstract//p` joined), authors (`forename` + `surname` from `.//author/persName`), DOI (`.//idno[@type='DOI']`), date (`@when` attr on `.//publicationStmt//date[@type='published']`), keywords (`.//keywords/term`); returns named list; returns `NULL` on any error or if file absent

**Checkpoint**: Skeleton and infrastructure ready — user story implementation can begin.

---

## Phase 3: User Story 1 — Single-Study Paper Conversion (Priority: P1) 🎯 MVP

**Goal**: Convert a single-study paper with CSV data files into a complete, structurally valid PsychDS directory. Produces `dataset_description.json`, `_data.csv`, `_data.json` sidecar, and `provenance.json`.

**Independent Test**: Run `source("data_check/runners/run_psychds_single.R")` with paper `0956797615620784`. Verify `psychds/0956797615620784/dataset_description.json` exists, contains `schema:variableMeasured` with all column names from `columns.csv`, and `data/source-GambleWalkerPsychologicalScienceData_data.csv` exists and is UTF-8 CSV.

### Implementation

- [x] T007 [US1] Implement `build_property_values(cols_df, labels_df, coverage_df)` minimal version in `pipeline/3_psychds_convert.R` — left-join `cols_df` + `labels_df` on `(source_file, column_name)`, deduplicate by `!duplicated(column_name)`, return list of PropertyValue named lists with `@type`, `name`, `metacheck:col_type`, `metacheck:source_file`; `Filter(Negate(is.null), pv)` before appending
- [x] T008 [US1] Implement `write_data_csv(df, dest_path)` in `pipeline/3_psychds_convert.R` — UTF-8, no BOM, `write.csv(df, dest_path, row.names = FALSE, fileEncoding = "UTF-8")`; check for `row_id` column: if present and values not all unique, rename to `original_row_id` in df, flag rename; creates parent directory
- [x] T009 [P] [US1] Implement `build_dataset_description(study_meta, paper_meta, pv_list)` in `pipeline/3_psychds_convert.R` — assembles JSON-LD named list with `@context` (`schema`+`metacheck` namespaces), `@type = "schema:Dataset"`, required fields from `study_meta` + `paper_meta`, `schema:variableMeasured = pv_list`; fallback values when paper_meta absent
- [x] T010 [P] [US1] Implement `build_sidecar(file_record, pv_list_for_file)` in `pipeline/3_psychds_convert.R` — `schema:variableMeasured`, `metacheck:original_file` (rel_path, format, size_bytes, is_raw), `metacheck:conversion` (method, rows_written, columns_written); oversized sidecar variant with `metacheck:conversion_skipped = TRUE`
- [x] T011 [P] [US1] Implement `place_non_data_file(file_record, study_out_dir)` in `pipeline/3_psychds_convert.R` — maps `type` to subdirectory: `code` → `analysis/`, `codebook`/`doc`/`supplemental`/`other` → `documentation/`, `asset` → `materials/`, `readme` → study root as `README.<ext>`; calls `file.copy()`; retains original filename (FR-009)
- [x] T012 [US1] Implement `convert_study(paper_id, study_group, structure_df, ...)` in `pipeline/3_psychds_convert.R` — orchestrates: (1) filter structure to study files, (2) read columns.csv/labels.csv/codebook_coverage.csv, (3) for each data file: `read_full_data()` → `write_data_csv()` → `build_sidecar()`, (4) `place_non_data_file()` for non-data, (5) `build_property_values()`, (6) `build_dataset_description()`, (7) write JSON files, (8) return result row
- [x] T013 [US1] Implement `build_provenance(file_records)` and `convert_psychds(paper_id)` entry point in `pipeline/3_psychds_convert.R` — entry point: reads `bulk_summary.csv`, checks `success`, reads `structure.csv`, calls `apply_ground_truth()`, detects studies, calls `convert_study()` per study; returns list of result rows
- [x] T014 [US1] Create `runners/run_psychds_single.R` — accepts `paper_id` as CLI arg or pre-set variable; falls back to random paper from `results/bulk_summary.csv`; calls `convert_psychds(paper_id)`; prints per-study summary (study_group, status, file counts, variable counts, paper meta source, ground truth applied, output path)

**Checkpoint**: US1 functional — single-study CSV paper converts end-to-end.

---

## Phase 4: User Story 2 — Multi-Study Paper Conversion (Priority: P2)

**Goal**: Papers with multiple experiment groups produce separate `study-<group>/` directories. Cross-group files land in `shared/`.

**Independent Test**: Convert a paper with `ex1` and `ex2` groups. Verify `study-ex1/` and `study-ex2/` both exist with separate `dataset_description.json`, and cross-group files appear in `shared/`.

### Implementation

- [x] T015 [US2] Implement multi-study layout in `convert_psychds()` in `pipeline/3_psychds_convert.R` — when `length(studies) > 1`: create `study-<group>/` subdirectory per study; single-study papers use flat layout at `psychds/<paper_id>/` (no `study-*` prefix)
- [x] T016 [P] [US2] Implement `resolve_shared_files(files_df, studies)` in `pipeline/3_psychds_convert.R` — for files with `group %in% c("na","other")`: apply co-location heuristic (parent dir siblings); assign to study if siblings are all one group; else assign to `shared/<type_dir>/`; `shared/` MUST NOT contain `dataset_description.json` or `data/`
- [x] T017 [P] [US2] Update `build_dataset_description()` for multi-study papers in `pipeline/3_psychds_convert.R` — add `metacheck:shared_resources` and `metacheck:shared_files` fields when `shared/` directory exists
- [x] T018 [US2] Wire multi-study layout into `convert_psychds()` — call `resolve_shared_files()`, split `structure_df` per study, pass correct `study_out_dir` to each `convert_study()` call

**Checkpoint**: US1 + US2 functional — single and multi-study papers both work.

---

## Phase 5: User Story 3 — Rich Column Metadata Encoding (Priority: P2)

**Goal**: `variableMeasured` PropertyValues carry full statistics for continuous columns, `valuePattern` for categorical, codebook labels as `description`, SPSS/Stata value label mappings, and unmatched codebook variables appended as PropertyValues.

**Independent Test**: On a paper with labelled continuous and categorical columns, verify a continuous column in `dataset_description.json` has `metacheck:statistics.mean` and a categorical column has `valuePattern`.

### Implementation

- [x] T019 [P] [US3] Extend `build_property_values()` in `pipeline/3_psychds_convert.R` — continuous columns (`NUMERIC_TYPES`): add `minValue` (= `min`), `maxValue` (= `max`), `metacheck:statistics` block (`n`, `n_missing`, `mean`, `sd`, `se`, `median`, `p25`, `p75`, `iqr`, `skewness`, `kurtosis`)
- [x] T020 [P] [US3] Extend `build_property_values()` — labelled columns (`label_status == "labelled"`): add `description = label`, `metacheck:label_source`, `metacheck:codebook_variable`, `metacheck:label_method`
- [x] T021 [P] [US3] Extend `build_property_values()` — categorical/binary/ordinal columns (`CATEGORICAL_TYPES`): add `valuePattern` = unique pipe-joined sample values from `sample_values` column
- [x] T022 [P] [US3] Extend `build_property_values()` — unmatched codebook variables: append a PropertyValue per row in `codebook_coverage.csv` where `match_status == "unmatched"` with `metacheck:match_status = "unmatched_in_data"`
- [x] T023 [P] [US3] Extend `build_property_values()` — multi-level header columns: add `metacheck:col_header_group` to PropertyValue when `col_header_group` is not `NA`

**Checkpoint**: US1–US3 functional — rich metadata populated for all column types.

---

## Phase 6: User Story 4 — Format Conversion and Raw Preservation (Priority: P3)

**Goal**: SPSS/Stata/SAS/Excel/RDS files are read and converted to CSV; originals placed in `data/raw/`; files >500 MB are raw-copied only; `is_raw = TRUE` files get `version-raw` prefix.

**Independent Test**: Provide a paper with a `.sav` file. Verify both `data/source-<name>_data.csv` and `data/raw/<original>.sav` exist.

### Implementation

- [x] T024 [P] [US4] Extend `read_full_data()` in `pipeline/3_psychds_convert.R` for SPSS — `.sav`: `haven::read_sav(path)`; extract `attr(col, "labels")` for each column before stripping; `haven::zap_labels(df)` + `haven::zap_label(df)` to strip to numeric codes; return `list(df, method = "haven::read_sav", haven_labels)`
- [x] T025 [P] [US4] Extend `read_full_data()` for Stata and SAS — `.dta`: `haven::read_dta(path)` (same label strip pattern); `.sas7bdat`: `haven::read_sas(path)`
- [x] T026 [P] [US4] Extend `read_full_data()` for Excel — `.xlsx`/`.xls`: `readxl::excel_sheets(path)` → one df per sheet; each sheet name sanitised via `sanitise_keyword_value()`; return list with `sheets = <sheet_names>`
- [x] T027 [P] [US4] Extend `read_full_data()` in `pipeline/3_psychds_convert.R` for RDS/RData — `.rds`: `readRDS(path)`; `.rda`/`.rdata`: `load(path, envir = e <- new.env())`, collect data.frame objects from `e`; return one result per data.frame; non-data.frame objects: return `list(df = NULL, reason = "not_dataframe")`; caller places original in `data/raw/` only
- [x] T028 [US4] Implement 500 MB size check and oversized-file handling in `convert_study()` in `pipeline/3_psychds_convert.R` — before calling `read_full_data()`: check `file.info(path)$size / 1e6 > DATA_SIZE_LIMIT_MB`; if exceeded: copy original to `data/raw/`, write skip-sidecar with `metacheck:conversion_skipped = TRUE`, `metacheck:skip_reason = "file_size_exceeds_limit"`, `metacheck:file_size_mb`; skip CSV conversion
- [x] T029 [P] [US4] Implement `is_raw = TRUE` file handling in `convert_study()` in `pipeline/3_psychds_convert.R` — when `is_raw == TRUE`: prefix output CSV name with `version-raw_source-<name>_data.csv`; always copy to `data/raw/`; add `metacheck:value_labels` to sidecar when `haven_labels` is non-NULL (from T024/T025)

**Checkpoint**: US1–US4 functional — all supported file formats convert correctly with provenance preserved.

---

## Phase 7: User Story 5 — Conversion Summary + Bulk Runner (Priority: P3)

**Goal**: Every conversion run appends result rows to `psychds/conversion_summary.csv`. The bulk runner processes all completed papers crash-resiliently, auto-resuming from the last successful row.

**Independent Test**: Run `run_psychds_bulk.R` on two papers. Kill it mid-run. Restart — verify only the incomplete paper is re-processed. Verify `conversion_summary.csv` contains one accurate row per study group.

### Implementation

- [x] T030 [US5] Implement `conversion_summary.csv` append logic in `pipeline/3_psychds_convert.R` — add `append_conversion_summary(rows, summary_path)` function: creates file with header if absent, appends rows as CSV with `colClasses = c(paper_id = "character")`; called by runners (not by `convert_psychds()` itself — function returns rows, runners write them)
- [x] T031 [US5] Create `runners/run_psychds_bulk.R` — (1) load `results/bulk_summary.csv` (paper_id as character), filter to `success == TRUE`; (2) load `psychds/conversion_summary.csv` if exists, build set of `(paper_id, study_group)` pairs already with `success == TRUE`; (3) for each paper not fully done: wrap `convert_psychds(paper_id)` in `tryCatch`; (4) call `append_conversion_summary()` immediately after each paper (crash-safe); (5) print `[N/M] paper_id — <n> studies, <status>` per paper

**Checkpoint**: All 5 user stories functional — full end-to-end pipeline with crash resilience.

---

## Phase 8: Polish & Cross-Cutting Concerns (US1–US5)

**Purpose**: Edge cases, sentinel handling, and documentation updates.

- [x] T032 Implement `row_id` uniqueness check in `convert_study()` in `pipeline/3_psychds_convert.R` — after writing CSV: if output contains column named `row_id` and values are not all unique, rename column to `original_row_id` in both the CSV and sidecar `variableMeasured`; log rename in sidecar `metacheck:conversion.row_id_renamed = TRUE` (FR-015b)
- [x] T033 Implement sentinel row handling in `convert_psychds()` in `pipeline/3_psychds_convert.R` — for rows where `is_sentinel == TRUE`: replace the sentinel row with individual file records from `list.files(dirname(path), full.names = TRUE, recursive = FALSE)`; apply `AGGREGATE_EXT_OVERRIDE` classification (`.R`/`.r` → `code`, `.jpg`/`.png`/`.gif` → `asset`, etc.) before inheriting sentinel's `type`/`group`; record sentinel status in provenance.json
- [x] T034 [P] Update `docs/pipeline.md` — add PsychDS conversion as a new stage after codebook labelling; document new entry points (`run_psychds_single.R`, `run_psychds_bulk.R`) and new output directory `psychds/`
- [x] T035 [P] Update `docs/output-schemas.md` — add `psychds/conversion_summary.csv` schema; document new error codes `pipeline_failed` and `no_data_files`
- [x] T036 [P] Update `progress.md` — add feature 021 entry

---

## Phase 9: User Story 6 — Documentation Plaintext Extraction (Priority: P3)

**Goal**: Every `doc` and `codebook` file with a text-extractable format (`.pdf`, `.docx`, `.rtf`) gets a `.txt` copy written to `documentation/txt/`. Originals are untouched. Image-only PDFs and extraction errors are flagged in `provenance.json`.

**Independent Test**: Convert a paper with a `.pdf` file of `type = "doc"`. Verify `documentation/<name>.pdf` and `documentation/txt/<name>.txt` both exist. Re-convert a paper with an image-only PDF; verify no `.txt` file is created and `provenance.json` records `txt_extraction_skipped: true`.

**⚠️ Depends on**: T037 (Phase 1 helper) must complete before T038.

### Implementation

- [x] T038 [US6] Implement `write_doc_txt(src_path, doc_dest_path, ext)` in `pipeline/3_psychds_convert.R` — calls `extract_plain_text(src_path)`; if result is non-NULL and `nzchar(trimws(result))`: creates `documentation/txt/` directory if absent, writes `<basename>.txt` to `documentation/txt/`; returns named list `list(attempted = TRUE, skipped = FALSE, txt_path = <rel_path>)`; if empty/NULL: returns `list(attempted = TRUE, skipped = TRUE, skip_reason = if (is.null(result)) "extraction_error" else "no_extractable_text", txt_path = NULL)`
- [x] T039 [US6] Wire `write_doc_txt()` into `convert_study()` in `pipeline/3_psychds_convert.R` — after `place_non_data_file()` is called for each file with `type %in% c("doc", "codebook")` and `ext %in% c("pdf", "docx", "rtf")`: call `write_doc_txt()`; store the returned list on the file's provenance record
- [x] T040 [US6] Update `build_provenance()` in `pipeline/3_psychds_convert.R` — add TXT extraction fields to provenance record for each file where extraction was attempted: `txt_extraction_attempted` (logical), `txt_extraction_skipped` (logical, only when `txt_extraction_attempted == TRUE`), `txt_skip_reason` (character, only when skipped), `txt_psychds_path` (character, only when not skipped)

**Checkpoint**: US6 functional — text-based doc/codebook files produce `.txt` copies; image PDFs flagged cleanly.

---

## Phase 10: Polish — US6 Documentation Updates

**Purpose**: Update pipeline docs to reflect the documentation TXT extraction step.

- [x] T041 [P] Update `docs/pipeline.md` — add note to step 13 (PsychDS conversion) documenting that `.pdf`/`.docx`/`.rtf` files of `type = "doc"` or `"codebook"` produce a `.txt` copy in `documentation/txt/`; add `extract_plain_text()` to helper.R helpers in the Entry Points table note
- [x] T042 [P] Update `docs/output-schemas.md` — add `txt_extraction_attempted`, `txt_extraction_skipped`, `txt_skip_reason`, `txt_psychds_path` fields to the `provenance.json` schema description

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1** (Setup): No dependencies — start immediately; T037 is new and not yet done
- **Phase 2** (Foundational): Depends on Phase 1 — BLOCKS all user stories
- **Phase 3** (US1 P1): Depends on Phase 2 — first deliverable, MVP ✅ done
- **Phase 4** (US2 P2): Depends on Phase 3 ✅ done
- **Phase 5** (US3 P2): Depends on Phase 3 ✅ done
- **Phase 6** (US4 P3): Depends on Phase 3 ✅ done
- **Phase 7** (US5 P3): Depends on Phase 3 ✅ done
- **Phase 8** (Polish): Depends on all US1–US5 ✅ done
- **Phase 9** (US6 P3): Depends on T037 (Phase 1) complete; US6 is additive on top of existing conversion
- **Phase 10** (Polish US6): Depends on Phase 9 complete

### User Story Dependencies

- **US1–US5**: All complete ✅
- **US6 (P3)**: Requires T037 (`extract_plain_text()` helper). Phases 9–10 are the only remaining work.

### Within US6

- T037 (helper) → T038 (`write_doc_txt`) → T039 (wire into `convert_study`) → T040 (provenance schema)
- T038 and T040 can be done in parallel once T037 is done; T039 depends on both T038 and T040

### Parallel Opportunities

- T037 can start immediately (only new unblocked task)
- T038 and T040 are parallel after T037
- T041 and T042 (Phase 10) are fully parallel

---

## Parallel Example: User Story 6

```text
# T037 must complete first (Phase 1 helper)
Task T037: Add extract_plain_text() to pipeline/helper.R

# After T037, T038 and T040 can run in parallel:
Task T038: write_doc_txt() in pipeline/3_psychds_convert.R
Task T040: update build_provenance() schema in pipeline/3_psychds_convert.R

# T039 depends on both T038 and T040:
Task T039: wire write_doc_txt() into convert_study()
```

---

## Implementation Strategy

### Remaining Work (US6 Only)

All US1–US5 work is complete. The only remaining tasks are:

1. **T037**: `extract_plain_text()` in `helper.R` — reuses patterns already present in the file
2. **T038–T040**: Three tasks in `3_psychds_convert.R` wiring up the TXT extraction
3. **T041–T042**: Two parallel documentation updates

Total remaining: **6 tasks** across 2 phases.
