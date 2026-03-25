# Feature Specification: PsychDS Conversion

**Feature Branch**: `021-psychds-conversion`
**Created**: 2026-03-25
**Status**: Draft
**Input**: User description: "read data_check/psych_ds_docs/spec.md and make a real formal spec"

---

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Single-Study Paper Conversion (Priority: P1)

A researcher wants to convert a processed single-study psychology paper repository into a PsychDS-compliant dataset. They invoke the converter with a paper ID and receive a validator-ready directory containing cleaned CSV data files, a rich `dataset_description.json` with full column metadata, and a `provenance.json` mapping every file back to its original path.

**Why this priority**: The majority of papers are single-study. This story delivers the core end-to-end conversion value and validates the full pipeline: input reading → format conversion → metadata generation → file placement.

**Independent Test**: Run `convert_psychds("0956797615620784")`. Verify that `psychds/0956797615620784/dataset_description.json` is produced, contains `variableMeasured` entries for every column in `columns.csv`, and that `source-GambleWalkerData_data.csv` passes the PsychDS file-naming rules.

**Acceptance Scenarios**:

1. **Given** a paper with one study group and one `.csv` data file, **When** the converter is run, **Then** `psychds/<paper_id>/dataset_description.json`, `psychds/<paper_id>/data/source-<name>_data.csv`, and `psychds/<paper_id>/provenance.json` are created.
2. **Given** a paper with a GROBID XML file at `/Volumes/Models/expanded_xml/<paper_id>.xml`, **When** conversion runs, **Then** `schema:name`, `schema:description`, `schema:author`, and `schema:identifier` in `dataset_description.json` are populated from the XML.
3. **Given** a paper without a GROBID XML file, **When** conversion runs, **Then** `dataset_description.json` falls back to `"Repository <paper_id>"` for `schema:name` and a generated description string.
4. **Given** the paper has a `ground_truth/<paper_id>.csv`, **When** conversion reads file classifications, **Then** `type_gt`, `group_gt`, and `is_raw_gt` override the pipeline's LLM-assigned values for validated rows.

---

### User Story 2 — Multi-Study Paper Conversion (Priority: P2)

A researcher converts a paper with multiple experiment groups (`ex1`, `ex2`, `pilot1`). Each group produces its own independent PsychDS dataset directory under `psychds/<paper_id>/study-<group>/`, each with its own `dataset_description.json`. Files that cannot be scoped to a single study are placed in `psychds/<paper_id>/shared/`.

**Why this priority**: Multi-study papers are common in the corpus and require the more complex grouping logic. This story validates per-study splitting and the shared-directory heuristic.

**Independent Test**: Run conversion on a paper with `ex1` and `ex2` groups. Verify `study-ex1/` and `study-ex2/` directories exist, each with their own `dataset_description.json`, and that cross-group files appear in `shared/`.

**Acceptance Scenarios**:

1. **Given** a paper with groups `ex1` and `ex2`, each with at least one `type = "data"` file, **When** conversion runs, **Then** `study-ex1/` and `study-ex2/` directories are created, each containing a separate `dataset_description.json`.
2. **Given** a file with `group = "na"` whose parent directory contains only `ex1` files, **When** the directory co-location heuristic runs, **Then** the file is placed in `study-ex1/` rather than `shared/`.
3. **Given** a file with `group = "other"` whose parent directory contains files from multiple groups, **When** conversion runs, **Then** the file is placed in `shared/documentation/` (or the appropriate type subdirectory).
4. **Given** a multi-study paper and a `shared/` directory is created, **When** each study's `dataset_description.json` is written, **Then** it includes `metacheck:shared_resources` and `metacheck:shared_files` pointing to the shared directory contents.

---

### User Story 3 — Rich Column Metadata Encoding (Priority: P2)

A researcher inspects the produced `dataset_description.json` and finds every column fully annotated: continuous columns have `minValue`, `maxValue`, and a full statistics block; categorical columns have `valuePattern`; labelled columns have `description` sourced from the codebook; SPSS/Stata value labels appear in `metacheck:value_labels`.

**Why this priority**: Metadata richness is the primary scientific value of the conversion. Without it, the output is structurally valid but scientifically thin.

**Independent Test**: On a paper with labelled continuous and categorical columns, inspect `variableMeasured` in the output `dataset_description.json`. Verify a continuous column has `metacheck:statistics.mean` and a categorical column has `valuePattern`.

**Acceptance Scenarios**:

1. **Given** a column with `col_type = "continuous"` and non-null statistics in `columns.csv`, **When** `variableMeasured` is built, **Then** the PropertyValue includes `minValue`, `maxValue`, and `metacheck:statistics` with `n`, `mean`, `sd`, `median`, `p25`, `p75`, `iqr`, `skewness`, and `kurtosis`.
2. **Given** a column matched in `labels.csv` with `label_status = "labelled"`, **When** `variableMeasured` is built, **Then** the PropertyValue has a `description` field equal to the label, and `metacheck:label_source`, `metacheck:codebook_variable`, `metacheck:label_method` are populated.
3. **Given** an SPSS `.sav` file with value labels for a column, **When** the sidecar is generated, **Then** `metacheck:value_labels` contains the numeric-to-string mapping and column values in the CSV are written as numeric codes.
4. **Given** a codebook variable with `match_status = "unmatched"` in `codebook_coverage.csv`, **When** `variableMeasured` is assembled, **Then** a PropertyValue with `metacheck:match_status = "unmatched_in_data"` is appended.

---

### User Story 4 — Format Conversion and Raw Preservation (Priority: P3)

A researcher verifies that non-CSV data files (SPSS, Stata, Excel, RDS) have been converted to UTF-8 CSV with proper naming, while the originals are preserved in `data/raw/`. Files exceeding 500 MB are placed in `data/raw/` with no conversion attempted.

**Why this priority**: Format conversion is required for PsychDS compliance. Raw preservation is required by the standard's data-provenance principle.

**Independent Test**: Provide a paper with a `.sav` data file. Verify both `data/source-<name>_data.csv` and `data/raw/<original>.sav` exist in the output.

**Acceptance Scenarios**:

1. **Given** a `.sav` data file, **When** conversion runs, **Then** a `_data.csv` is written with numeric column values, AND the original `.sav` is copied to `data/raw/`.
2. **Given** a `.xlsx` file with two sheets, **When** conversion runs, **Then** two `_data.csv` files are created (one per sheet) with `sheet-<name>` in the filename.
3. **Given** a data file larger than 500 MB, **When** conversion runs, **Then** the file is copied to `data/raw/` without conversion and a sidecar JSON is written with `metacheck:conversion_skipped = true` and `metacheck:skip_reason = "file_size_exceeds_limit"`.
4. **Given** a file with `is_raw = TRUE`, **When** conversion runs, **Then** it is placed in `data/raw/` and its CSV conversion (if applicable) is prefixed `version-raw_source-<name>_data.csv`.

---

### User Story 6 — Documentation Plaintext Extraction (Priority: P3)

A researcher (or downstream machine processing pipeline) needs to read the content of documentation files (PDFs, DOCX, RTF) without format-specific tooling. After conversion, a `documentation/txt/` subfolder contains a `.txt` copy of every `doc` and `codebook` file for which text could be successfully extracted, while the originals remain untouched in `documentation/`.

**Why this priority**: Not part of the PsychDS standard — this is a practical extension for downstream machine processing of the converted repository. Original files are never modified or deleted.

**Independent Test**: Provide a paper with a `.pdf` file of `type = "doc"`. Verify that `documentation/pdf-original.pdf` and `documentation/txt/pdf-original.txt` both exist in the output, and that `provenance.json` records the TXT derivation.

**Acceptance Scenarios**:

1. **Given** a `doc` or `codebook` file with a text-based format (`.pdf`, `.docx`, `.rtf`), **When** text extraction succeeds, **Then** a `.txt` copy with the same base filename is written to `documentation/txt/`.
2. **Given** an image-only `.pdf` where text extraction yields an empty result, **When** conversion runs, **Then** no `.txt` file is created and `provenance.json` records `txt_extraction_skipped: true` and `txt_skip_reason: "no_extractable_text"` for that file.
3. **Given** a `doc` or `codebook` file in a non-text format (e.g. `.csv`, `.xlsx`, `.sav`), **When** conversion runs, **Then** no TXT extraction is attempted and no entry is added to `documentation/txt/`.
4. **Given** any original documentation file, **When** the TXT copy is created, **Then** the original file in `documentation/` is unchanged.

---

### User Story 5 — Conversion Summary (Priority: P3)

After running conversion across multiple papers, the researcher inspects `psychds/conversion_summary.csv` to see which papers succeeded, how many variables were labelled, and whether ground truth or GROBID metadata was available for each.

**Why this priority**: Observability is needed to monitor conversion quality at scale.

**Independent Test**: Run conversion on two papers. Verify `conversion_summary.csv` contains one row per study group with accurate `success`, `n_data_files`, `n_variables`, and `n_labelled` values.

**Acceptance Scenarios**:

1. **Given** a successful conversion, **When** the summary is written, **Then** a row exists with `success = TRUE`, correct file and variable counts, and the output path.
2. **Given** a paper with `success = FALSE` in `bulk_summary.csv`, **When** conversion runs, **Then** the paper is skipped and its row in the summary has `success = FALSE` with the upstream error code.
3. **Given** a paper with zero `type = "data"` files after ground-truth override, **When** conversion runs, **Then** no PsychDS directory is created and the summary records `skipped_no_data`.

---

### Edge Cases

- **Sentinel rows**: Directories with >50 files collapse to a sentinel row in `structure.csv`. Conversion must scan the actual directory on disk and classify individual files using extension-override rules before falling back to the sentinel's inherited type.
- **Duplicate column names across files**: A single deduplicated PropertyValue appears in `dataset_description.json`; per-file sidecars carry file-specific statistics. When statistics differ, the file with the most non-missing values takes precedence.
- **Multi-level CSV headers**: The resolved sub-header row becomes the CSV column header; the original multi-level structure and `col_header_group` are recorded in the sidecar.
- **`row_id` column conflict**: If a `row_id` column does not have all-unique values, it is renamed to `original_row_id` and flagged in the sidecar. (This is a hard validator error if left unaddressed — see FR-015b.)
- **Empty columns** (`col_type = "empty"`): Preserved in the output CSV but flagged with `metacheck:note = "All values NA in source data"` in the PropertyValue.
- **Comma-decimal columns**: Decimal separator is normalised to `.` in the output CSV; original separator and coercion count are recorded in the sidecar.
- **Non-data-frame RDS/RData objects**: Placed in `data/raw/` without CSV conversion.
- **Papers with pipeline errors**: Skipped entirely; the upstream error code from `bulk_summary.csv` is propagated to the conversion summary.
- **Image-only PDFs**: `pdftools::pdf_text()` returns empty strings for scanned-only PDFs. The converter detects this (all extracted pages whitespace-only) and skips TXT creation, recording the skip flag in `provenance.json`.
- **TXT extraction failures**: If text extraction throws an error rather than returning empty content, the error is caught, TXT creation is skipped, and `txt_skip_reason: "extraction_error"` is recorded in `provenance.json`.

---

## Requirements *(mandatory)*

### Functional Requirements

**Inputs and reading**

- **FR-001**: The converter MUST read `outputs/<paper_id>/structure.csv`, `columns.csv`, `labels.csv`, and `codebook_coverage.csv` as read-only inputs and MUST NOT modify them.
- **FR-002**: When `ground_truth/<paper_id>.csv` exists, the converter MUST override the pipeline's `type`, `group`, and `is_raw` values with `type_gt`, `group_gt`, and `is_raw_gt` for rows where the file has been validated; unvalidated rows MUST fall back to pipeline values.
- **FR-003**: The converter MUST skip papers where `bulk_summary.csv` records `success = FALSE`, recording the pipeline error code in the conversion summary.
- **FR-004**: The converter MUST skip papers that have zero `type = "data"` files after ground-truth override, recording `skipped_no_data` in the conversion summary.

**Study organisation**

- **FR-005**: Each unique `group` value containing at least one `type = "data"` file MUST produce a separate PsychDS dataset directory named `study-<group>/` under `psychds/<paper_id>/`.
- **FR-006**: When a paper has exactly one study group, the converter MUST produce a flat layout directly at `psychds/<paper_id>/` with no `study-*` subdirectories.
- **FR-007**: Files with `group = "na"` or `group = "other"` that cannot be unambiguously co-located with a single study via directory co-location MUST be placed in `psychds/<paper_id>/shared/`; `shared/` MUST NOT contain a `dataset_description.json` or `data/` directory.

**File placement for non-data files**

- **FR-008**: Non-data files MUST be placed by type: `code` → `analysis/`; `codebook`, `doc`, `supplemental`, `other` → `documentation/`; `asset` → `materials/`; `readme` → root of the study directory renamed to `README.<ext>`.
- **FR-009**: Non-data files MUST retain their original filenames.

**Documentation plaintext extraction**

- **FR-026**: For every file with `type = "doc"` or `type = "codebook"` in a text-extractable format (`.pdf`, `.docx`, `.rtf`), the converter MUST attempt to extract the plaintext content and write a `.txt` copy with the same base filename to `documentation/txt/` alongside the original in `documentation/`.
- **FR-027**: Text extraction MUST reuse the existing extraction capabilities already present in the pipeline: `pdftools` for `.pdf`, `officer` for `.docx`, and the regex-based RTF stripper for `.rtf`. No new packages may be added.
- **FR-028**: When text extraction yields an empty or whitespace-only result (e.g. image-only PDFs), the converter MUST NOT create a `.txt` file; instead it MUST record `txt_extraction_skipped: true` and `txt_skip_reason: "no_extractable_text"` in `provenance.json` for that source file.
- **FR-029**: Files of `type = "doc"` or `type = "codebook"` in non-text formats (`.csv`, `.xlsx`, `.sav`, `.dta`, `.rds`, etc.) MUST NOT have TXT extraction attempted.
- **FR-030**: Original files in `documentation/` MUST NOT be modified or deleted by the TXT extraction step.

**Data file conversion**

- **FR-010**: Files with `type = "data"` MUST be converted to CSV and named `source-<sanitised_name>_data.csv`; multi-sheet Excel files MUST produce one file per sheet named `source-<sanitised_name>_sheet-<sanitised_sheet>_data.csv`.
- **FR-011**: All output CSVs MUST be UTF-8 encoded without BOM, with a header row, equal cells per row, and embedded commas escaped with double quotes.
- **FR-012**: Every original data file MUST be copied to `data/raw/` in its original format regardless of whether CSV conversion succeeded.
- **FR-013**: Data files exceeding 500 MB MUST be placed in `data/raw/` without CSV conversion; a sidecar MUST record `metacheck:conversion_skipped = true` and `metacheck:skip_reason = "file_size_exceeds_limit"`.
- **FR-014**: SPSS and Stata column values MUST be written as underlying numeric codes in the CSV; the value-label mapping MUST appear as `metacheck:value_labels` in the sidecar.
- **FR-015**: Filename sanitisation applies to all keyword values (source filename, sheet name, etc.) and MUST: (1) remove the file extension, (2) replace spaces/dots/special characters with nothing, (3) collapse consecutive hyphens/underscores, (4) retain only alphanumeric characters (`[a-zA-Z0-9]`), (5) truncate to 60 characters. This ensures keyword values satisfy the PsychDS schema constraint `[a-zA-Z0-9]+`.
- **FR-015a**: Output data filenames MUST match the PsychDS schema regex `([a-z]+-[a-zA-Z0-9]+)(_[a-z]+-[a-zA-Z0-9]+)*_data\.csv` — keyword keys (e.g. `source`, `sheet`, `version`) are fixed lowercase-alpha strings; keyword values are the sanitised outputs of FR-015.
- **FR-015b**: If a data file in the output contains a column named `row_id`, the converter MUST verify that all values in that column are unique; if they are not unique, the column MUST be renamed to `original_row_id` and the rename MUST be recorded in the sidecar.

**Metadata generation**

- **FR-016**: Each study directory MUST contain a `dataset_description.json` with at minimum: `@context` (Schema.org + `metacheck` namespace), `@type`, `schema:name`, `schema:description`, `schema:variableMeasured`, and all `metacheck:` provenance fields defined in Section 6.3 of the source spec.
- **FR-017**: When a GROBID XML file exists at `/Volumes/Models/expanded_xml/<paper_id>.xml`, the converter MUST populate `schema:name`, `schema:description`, `schema:author`, `schema:identifier`, `schema:datePublished`, and `schema:keywords` from it; when absent, fallback values MUST be used.
- **FR-018**: `variableMeasured` MUST contain one PropertyValue per unique column name across all data files in the study, built by left-joining `columns.csv` and `labels.csv` on (source_file, column_name), deduplicating preferring processed over raw.
- **FR-019**: PropertyValues for continuous columns MUST include `minValue`, `maxValue`, and a `metacheck:statistics` block (n, n_missing, mean, sd, se, median, p25, p75, iqr, skewness, kurtosis) when those statistics are present in `columns.csv`.
- **FR-020**: PropertyValues for categorical or binary columns MUST include a `valuePattern` derived from unique sample values (pipe-separated).
- **FR-021**: Unmatched codebook variables (rows in `codebook_coverage.csv` with no corresponding data column) MUST be appended to `variableMeasured` with `metacheck:match_status = "unmatched_in_data"`.
- **FR-022**: Each `_data.csv` file MUST be accompanied by a sidecar `_data.json` containing file-specific `variableMeasured`, `metacheck:original_file`, and `metacheck:conversion` metadata.
- **FR-023**: A `provenance.json` MUST be written at the study root (or paper root for single-study papers) mapping every output file to its `original_rel_path`, `pipeline_type`, `pipeline_group`, and ground-truth validation status.

**Sentinel and aggregate directories**

- **FR-024**: For sentinel rows (`is_sentinel = TRUE`) in `structure.csv`, the converter MUST scan the actual directory on disk and process each file individually, applying extension-override classification before falling back to the sentinel's inherited type.

**Summary**

- **FR-025**: Every conversion run MUST write or append to `psychds/conversion_summary.csv` with columns: `paper_id`, `study_group`, `success`, `error`, `n_data_files`, `n_raw_files`, `n_variables`, `n_labelled`, `has_paper_metadata`, `has_ground_truth`, `output_path`.

---

### Key Entities

- **Paper**: Identified by a character string `paper_id` (leading zeros must be preserved). Has one or more study groups derived from `structure.csv`.
- **Study**: A unique `group` value within a paper containing at least one `type = "data"` file. Maps to one PsychDS dataset directory.
- **Data File**: A file with `type = "data"` (after ground-truth override). Converted to `_data.csv`; original copied to `data/raw/`.
- **PropertyValue**: A Schema.org PropertyValue object representing one column in `variableMeasured`. Can be global (in `dataset_description.json`) or file-scoped (in a sidecar).
- **Provenance Record**: An entry in `provenance.json` linking a PsychDS output path to the original repository path and pipeline classification.
- **Conversion Summary Row**: One row per study group in `psychds/conversion_summary.csv` recording the outcome of conversion for that study.

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Every `_data.csv` produced matches the PsychDS schema regex `([a-z]+-[a-zA-Z0-9]+)(_[a-z]+-[a-zA-Z0-9]+)*_data\.csv` without any manual correction.
- **SC-002**: For papers where `columns.csv` and `labels.csv` are both present, 100% of labelled columns appear as PropertyValues with a `description` field in `variableMeasured`.
- **SC-003**: For continuous columns with statistics available in `columns.csv`, 100% have a `metacheck:statistics` block in their PropertyValue.
- **SC-004**: Every original data file is preserved in `data/raw/`, achieving 100% raw-file coverage across all converted papers.
- **SC-005**: Papers with no `type = "data"` files are gracefully skipped without error; the conversion summary accurately records `skipped_no_data` for each.
- **SC-006**: `psychds/conversion_summary.csv` is written after every run and reflects the actual outcome (success/failure, file and variable counts) for every paper processed.
- **SC-007**: All output CSVs pass UTF-8 encoding validation (no BOM, no read-back encoding errors).
- **SC-008**: For papers with a `ground_truth/<paper_id>.csv`, the provenance records confirm that `type_gt`/`group_gt`/`is_raw_gt` values were applied to validated rows rather than the pipeline's LLM values.
- **SC-009**: For every `doc` or `codebook` file with a text-extractable format, a `.txt` copy appears in `documentation/txt/` or a skip entry exists in `provenance.json` — no such file is silently unaccounted for.

---

## Clarifications

### Session 2026-03-25

- Q: Where should TXT files be placed relative to the original? → A: Sibling `txt/` subdirectory under `documentation/` (e.g. `documentation/txt/codebook.txt`)
- Q: Which non-data file types should get TXT conversion? → A: `doc` and `codebook` types only
- Q: When text extraction yields nothing (image-only PDF), what should happen? → A: Skip TXT creation; record `txt_extraction_skipped: true` and `txt_skip_reason: "no_extractable_text"` in `provenance.json`

---

## Assumptions

- The converter operates per-paper. A separate bulk wrapper to iterate over multiple papers is not in scope for this feature.
- All pipeline output CSVs are present and well-formed for papers with `success = TRUE` in `bulk_summary.csv`.
- The PsychDS validator is a separate downstream step; this feature produces validator-ready output but does not invoke the validator itself.
- The existing R packages (`haven`, `readxl`, `jsonlite`) are sufficient; no new packages are needed.
- Output is written to `data_check/psychds/<paper_id>/`, separate from `outputs/` and `data/`.
- Downloaded data files at `data/<paper_id>/` are assumed to be present on disk; this feature does not re-download.
- The `metacheck` JSON-LD namespace (`https://metacheck.io/ns/`) is treated as a stable identifier for v1 and does not require external registration.
- The PsychDS validator will produce `UNKNOWN_NAMESPACE` **warnings** (not errors) for the `metacheck:` namespace, since only the `https://schema.org` namespace is type-checked by the validator. The output will be VALID per the validator — warnings do not prevent a VALID result. This is expected and acceptable.
