# PsychDS Conversion — Specification

## 1. Goal

Transform every paper repository produced by the metacheck-datacheck pipeline into a
PsychDS-compliant dataset at the **study level**. Each experiment/study group (`ex1`,
`ex2`, `pilot1`, etc.) within a paper becomes its own independent PsychDS dataset unit.

The conversion must:

- Produce validator-passing PsychDS output for every study
- Preserve **maximum metadata** — original file locations, pipeline classifications,
  column statistics, codebook labels, and provenance information
- Convert all tabular data to CSV with PsychDS `_data.csv` naming
- Generate rich `dataset_description.json` with full `variableMeasured` PropertyValue
  objects including descriptions, types, statistics, and value patterns
- Preserve raw/original-format files in `data/raw/`
- Organise non-data files into recommended PsychDS directories

---

## 2. Scope

### In scope

- Automated conversion from existing pipeline outputs (`structure.csv`,
  `columns.csv`, `labels.csv`, `codebook_coverage.csv`) plus downloaded data files
- Per-study PsychDS dataset generation
- `dataset_description.json` metadata generation with Schema.org + custom namespace
- File format conversion (SPSS, Stata, Excel, RDS, RData → CSV)
- Sidecar metadata for per-file provenance
- Optional integration of GROBID TEI XML paper metadata (title, authors, abstract)
- Optional integration of ground-truth corrections from `ground_truth/<paper_id>.csv`

### Out of scope (v1)

- PsychDS validator integration (validation is a separate step)
- Modification of the upstream pipeline
- Multi-paper batch orchestration (this spec covers the per-paper conversion logic)
- Re-downloading or re-processing data — conversion works from existing outputs

---

## 3. Input Sources

All inputs are read-only. The conversion never modifies pipeline outputs.

| Source | Path | Contents |
|---|---|---|
| File classifications | `outputs/<paper_id>/structure.csv` | One row per file: type, group, is_raw, path |
| Column metadata | `outputs/<paper_id>/columns.csv` | One row per column: col_type, statistics, sample values |
| Column labels | `outputs/<paper_id>/labels.csv` | One row per column: label, codebook variable, label_source |
| Codebook variables | `outputs/<paper_id>/codebook_coverage.csv` | One row per codebook variable: label, match status |
| Downloaded files | `data/<paper_id>/` | Original repository files on disk |
| Ground truth (optional) | `ground_truth/<paper_id>.csv` | Human-verified type/group/is_raw corrections |
| Paper metadata (optional) | `/Volumes/Models/expanded_xml/<paper_id>.xml` | GROBID TEI XML: title, authors, abstract, DOI |
| Bulk summary | `results/bulk_summary.csv` | Paper-level success/error status |

### Ground-truth precedence

When `ground_truth/<paper_id>.csv` exists, its `type_gt`, `group_gt`, and `is_raw_gt`
values **override** the pipeline's LLM-assigned values for any row where the file has
been validated. Unvalidated rows fall back to pipeline values.

This is critical: the PsychDS output should reflect the best available classification,
which is ground truth when available, pipeline output otherwise.

---

## 4. Output Structure

### 4.1 Root layout

```
psychds/<paper_id>/
├── study-ex1/                     # One PsychDS dataset per study
│   ├── dataset_description.json
│   ├── data/
│   │   ├── raw/                   # Original non-CSV files, raw data
│   │   ├── [keyword]_data.csv     # PsychDS-compliant data files
│   │   └── [keyword]_data.json    # Sidecar metadata (per data file)
│   ├── analysis/                  # Code files scoped to this study
│   ├── documentation/             # Codebooks, docs, supplemental
│   └── materials/                 # Assets (images, stimuli, audio)
├── study-ex2/
│   └── ...
├── study-pilot1/
│   └── ...
└── shared/                        # Files with group="other" or "na"
    ├── analysis/
    ├── documentation/
    └── materials/
```

### 4.2 Output directory

All PsychDS output is written to `psychds/<paper_id>/` relative to `data_check/`.
This directory is separate from `outputs/` (pipeline results) and `data/`
(downloaded repos).

### 4.3 Study identification

Each unique `group` value that contains at least one `type = "data"` file becomes a
separate PsychDS dataset. The mapping:

| Pipeline group | PsychDS study directory |
|---|---|
| `ex1`, `ex2`, `ex4a` | `study-ex1/`, `study-ex2/`, `study-ex4a/` |
| `pilot1`, `pilot2` | `study-pilot1/`, `study-pilot2/` |
| `other` | `study-other/` (if data files exist with group=other) |

Files with `group = "na"` (non-data files: assets, supplemental, etc.) are placed
in the `shared/` directory unless they can be unambiguously scoped to a single study
by directory co-location (see Section 8).

### 4.4 Single-study papers

When a paper has only one study group (or all data files share the same group), the
output simplifies to a single PsychDS dataset at the paper root level:

```
psychds/<paper_id>/
├── dataset_description.json
├── data/
│   ├── raw/
│   └── [keyword]_data.csv
├── analysis/
├── documentation/
└── materials/
```

No `study-*` prefix directories are created.

---

## 5. Data File Conversion

### 5.1 Which files become `_data.csv`

Only files classified as `type = "data"` (after ground-truth override) are converted
to PsychDS data files. All other file types are placed in their respective directories
(see Section 7).

### 5.2 Format conversion

| Source format | Conversion method | Notes |
|---|---|---|
| `.csv`, `.tsv`, `.txt`, `.dat` | Read via `read.csv` / `read.delim` → write as CSV | Encoding normalised to UTF-8 |
| `.sav` (SPSS) | `haven::read_sav()` → convert labelled columns → write CSV | Haven labels extracted separately for metadata |
| `.dta` (Stata) | `haven::read_dta()` → convert labelled columns → write CSV | Value labels preserved in sidecar |
| `.xlsx`, `.xls` | `readxl::read_excel()` per sheet → write one CSV per sheet | Multi-sheet files produce multiple `_data.csv` files |
| `.sas7bdat` | `haven::read_sas()` → write CSV | |
| `.rds` | `readRDS()` → write CSV if data.frame | Non-data-frame objects: skip data conversion, place original in `raw/` |
| `.rda`, `.rdata` | `load()` → write CSV for each data.frame object | Multiple objects → multiple `_data.csv` files; non-df objects → `raw/` |

**Full data read**: Unlike the pipeline's 5-row sample (`N_DATA_READ = 5`), the
PsychDS conversion reads the **complete** data file. The 500 MB file size limit from
the pipeline still applies — files exceeding this are placed in `data/raw/` without
conversion, with a note in the sidecar metadata.

### 5.3 CSV output requirements

All output CSVs must satisfy PsychDS data file rules:

- UTF-8 encoded
- Commas embedded in data values enclosed in double quotes
- Header row present (first row = column names)
- Equal number of cells per row
- No BOM (byte order mark)

### 5.4 File naming: keyword format

PsychDS requires data files to match `[key-value_]+data.csv`. The naming scheme
derives keywords from pipeline metadata:

**Single data file per study:**
```
source-{sanitised_filename}_data.csv
```

**Multiple data files per study:**
```
source-{sanitised_filename}_data.csv
```
Each file gets a unique `source` keyword based on the original filename.

**Multi-sheet Excel files:**
```
source-{sanitised_filename}_sheet-{sheet_name}_data.csv
```

**Raw vs processed variants:**
```
version-raw_source-{sanitised_filename}_data.csv
version-processed_source-{sanitised_filename}_data.csv
```

**Sanitisation rules** for keyword values:
1. Remove file extension
2. Replace spaces, dots, and special characters with empty string
3. Collapse consecutive hyphens/underscores
4. Convert to alphanumeric characters only (a-zA-Z0-9)
5. Truncate to 60 characters maximum

### 5.5 Preserving originals in `data/raw/`

The following files are always placed in `data/raw/`:

- All data files in their **original format** (the source .sav, .dta, .xlsx, etc.)
- Files with `is_raw = TRUE` (after ground-truth override)
- Data files exceeding 500 MB (not converted, original only)
- RDS/RData files containing non-data.frame objects

This ensures the earliest digital form of data is preserved, per PsychDS core
principles.

### 5.6 Haven-labelled column handling

SPSS and Stata files embed value labels (e.g., `1 = "Male"`, `2 = "Female"`).
During conversion:

1. Column values are written as their **underlying values** (numeric codes), not
   label strings — this preserves the original data structure
2. The value-label mapping is recorded in the sidecar metadata and in the
   `variableMeasured` PropertyValue for that column (via `valuePattern`)
3. If ground truth or codebook labels exist, they take precedence for the
   `description` field

---

## 6. Metadata Generation: `dataset_description.json`

Each study directory gets its own `dataset_description.json`. This is the core of
the PsychDS output and where the maximum metadata preservation happens.

### 6.1 Required fields

```json
{
    "@context": {
        "schema": "https://schema.org/",
        "metacheck": "https://metacheck.io/ns/"
    },
    "@type": "schema:Dataset",
    "schema:name": "<paper_title> — <study_label>",
    "schema:description": "<paper_abstract_or_generated_description>",
    "schema:variableMeasured": [ ... ]
}
```

### 6.2 Recommended fields (populated when source data exists)

| Field | Source | Example |
|---|---|---|
| `schema:name` | GROBID XML `<title>` + study group | "Development of Face-to-Trait Inference — Experiment 1" |
| `schema:description` | GROBID XML `<abstract>` | Full abstract text |
| `schema:author` | GROBID XML `<author>` elements | Array of Person objects with names |
| `schema:identifier` | Paper DOI from XML | `"https://doi.org/10.1177/0956797614523297"` |
| `schema:sameAs` | OSF repository URL | `"https://osf.io/..."` |
| `schema:datePublished` | GROBID XML date | `"2014-05-01"` |
| `schema:citation` | Derived from XML | Full citation string |
| `schema:keywords` | From XML keywords if present | `["face perception", "trait inference"]` |
| `schema:schemaVersion` | PsychDS version | `"Psych-DS 0.1.0"` |
| `schema:license` | From OSF metadata if available | License identifier |

### 6.3 Custom namespace: `metacheck`

To preserve pipeline provenance without polluting Schema.org properties, a custom
`metacheck` namespace encodes pipeline-specific metadata:

```json
{
    "metacheck:paper_id": "0956797614523297",
    "metacheck:study_group": "ex1",
    "metacheck:pipeline_version": "020",
    "metacheck:conversion_date": "2026-03-24",
    "metacheck:pipeline_status": {
        "index_success": true,
        "codebook_success": true,
        "n_files_total": 21,
        "n_data_files": 2,
        "n_columns": 14,
        "n_labelled_columns": 14,
        "label_status": "ok"
    },
    "metacheck:source_repository": {
        "platform": "osf",
        "download_path": "data/0956797614523297/",
        "original_structure": "Development_of_Face-to-Trait_Inference/"
    }
}
```

### 6.4 `variableMeasured` — full PropertyValue objects

This is where the bulk of the pipeline intelligence is encoded. Every column header
across all data files in the study becomes a PropertyValue object.

**Minimal (unlabelled column, no codebook):**
```json
{
    "@type": "PropertyValue",
    "name": "Sex",
    "metacheck:col_type": "categorical",
    "metacheck:source_file": "Cogsdill_FaceTrait_Experiment1Public.csv",
    "metacheck:sample_values": "M | F"
}
```

**Rich (labelled column with statistics):**
```json
{
    "@type": "PropertyValue",
    "name": "Age",
    "description": "Age in years",
    "minValue": 17,
    "maxValue": 56,
    "metacheck:col_type": "continuous",
    "metacheck:source_file": "GambleWalkerData.csv",
    "metacheck:sample_values": "25 | 24 | 26 | 24 | 24",
    "metacheck:statistics": {
        "n": 80,
        "n_missing": 0,
        "mean": 25.625,
        "sd": 6.874,
        "se": 0.769,
        "median": 25,
        "p25": 22,
        "p75": 27,
        "iqr": 5,
        "skewness": 1.913,
        "kurtosis": 5.137
    },
    "metacheck:label_source": "GambleWalkerKey.rtf",
    "metacheck:label_method": "rules",
    "metacheck:codebook_variable": "Age"
}
```

**Categorical column with value labels (from SPSS/Stata):**
```json
{
    "@type": "PropertyValue",
    "name": "Condition",
    "description": "Which condition the participant was in: 1=helmet, 2=cap",
    "valuePattern": "1|2",
    "metacheck:col_type": "binary",
    "metacheck:value_labels": {
        "1": "helmet",
        "2": "cap"
    },
    "metacheck:label_source": "GambleWalkerKey.rtf",
    "metacheck:label_method": "rules"
}
```

**Column appearing in multiple files within the same study:**
When the same column name appears in multiple data files within a study (e.g.,
`participant_id` in both a raw and processed version), the PropertyValue is
deduplicated. If statistics or labels differ across files, the sidecar metadata
carries the per-file detail and the global PropertyValue uses the values from the
first file encountered (or the processed file if both raw and processed exist).

### 6.5 Construction algorithm for `variableMeasured`

```
For each study group S in paper P:
  1. Get all rows from columns.csv where group = S
  2. Get all rows from labels.csv where group = S
  3. Left-join columns and labels on (source_file, column_name)
  4. Deduplicate by column_name (prefer processed over raw; keep first occurrence)
  5. For each unique column_name:
     a. Create PropertyValue with "name" = column_name
     b. If label exists and label_status = "labelled":
        - Set "description" from label
        - Set metacheck:label_source, metacheck:codebook_variable, metacheck:label_method
     c. If col_type is numeric (continuous, continuous_comma_decimal, etc.):
        - Set "minValue" from min, "maxValue" from max
        - Set metacheck:statistics with full stat block
     d. If col_type is categorical or binary:
        - Set "valuePattern" from unique sample_values (pipe-separated → regex alternation)
     e. Always set: metacheck:col_type, metacheck:source_file, metacheck:sample_values
  6. Append unmatched codebook variables from codebook_coverage.csv as extra
     PropertyValues with metacheck:match_status = "unmatched_in_data"
     (preserves codebook variables that have no corresponding data column)
```

---

## 7. Non-Data File Placement

Files that are not `type = "data"` are placed in conventional PsychDS directories
according to their pipeline classification:

| Pipeline `type` | PsychDS directory | Notes |
|---|---|---|
| `code` | `analysis/` | Analysis scripts, R/Python/SPSS syntax |
| `codebook` | `documentation/` | Codebooks, data dictionaries, keys |
| `doc` | `documentation/` | Manuscripts, notes, reports |
| `readme` | Root of study directory | Renamed to `README` + original extension |
| `supplemental` | `documentation/` | Survey instruments, consent forms, SPSS output |
| `asset` | `materials/` | Images, audio, video, stimuli |
| `other` | `documentation/` | Default catch-all |

### 7.1 File naming for non-data files

Non-data files retain their original filenames. PsychDS has no naming requirements
for files outside `data/`. Original names maximise recognisability for anyone
familiar with the source repository.

### 7.2 Preserving original directory structure

The original `rel_path` from `structure.csv` is recorded in a `provenance.json`
file at the study root. This maps every file in the PsychDS output back to its
original location:

```json
{
    "file_provenance": [
        {
            "psychds_path": "data/source-Study1Data_data.csv",
            "original_rel_path": "Data/Study_1_Upload_Data.csv",
            "original_format": "csv",
            "pipeline_type": "data",
            "pipeline_group": "ex1",
            "pipeline_is_raw": false,
            "ground_truth_validated": false
        },
        {
            "psychds_path": "data/raw/Study_1_Upload_Data.sav",
            "original_rel_path": "Data/Study_1_Upload_Data.sav",
            "original_format": "sav",
            "pipeline_type": "data",
            "pipeline_group": "ex1",
            "pipeline_is_raw": false,
            "ground_truth_validated": false
        },
        {
            "psychds_path": "analysis/analysis_study1.R",
            "original_rel_path": "Scripts/analysis_study1.R",
            "original_format": "r",
            "pipeline_type": "code",
            "pipeline_group": "ex1",
            "ground_truth_validated": true,
            "ground_truth": {
                "type_gt": "code",
                "group_gt": "ex1",
                "is_raw_gt": false,
                "validated_at": "2026-03-24T10:10:30",
                "annotator": "Levi Baruch"
            }
        }
    ]
}
```

---

## 8. Handling Shared and Unscoped Files

### 8.1 The problem

Many files in research repositories are not scoped to a single study:

- **`group = "other"`**: Shared code, combined datasets, general documentation
- **`group = "na"`**: Assets, supplemental materials, readmes (assigned `na` by
  the pipeline because scoping is not applicable)

These files need a home in the PsychDS output without violating the one-study-per-
dataset principle.

### 8.2 Strategy: directory co-location heuristic

Before resorting to the `shared/` directory, attempt to scope unscoped files to a
study using directory co-location:

1. For each unscoped file, find its parent directory in the original `rel_path`
2. Check if all other files in the same parent directory belong to a single study
   group
3. If yes → assign this file to that study
4. If no (mixed groups or no data files in that directory) → place in `shared/`

### 8.3 The `shared/` directory

For multi-study papers, files that cannot be scoped to a single study are placed in
`shared/` at the paper root level (outside any study directory). This directory is
**not** a PsychDS dataset — it has no `dataset_description.json` and no `data/`
directory. It is purely an organisational holding area.

Each study's `dataset_description.json` includes a reference to shared resources:

```json
{
    "metacheck:shared_resources": "../shared/",
    "metacheck:shared_files": [
        "../shared/documentation/general_codebook.pdf",
        "../shared/analysis/combined_analysis.R"
    ]
}
```

### 8.4 Single-study simplification

When a paper has only one study, there is no `shared/` directory. All files,
regardless of `group` value, are placed directly in the single study's PsychDS
structure. The `group` values `"other"` and `"na"` are treated as belonging to the
sole study.

---

## 9. Sidecar Metadata

PsychDS supports per-file sidecar JSON files that inherit from and override the
global `dataset_description.json`. These are used to encode file-specific metadata
that would be too verbose for the global file.

### 9.1 When to generate sidecars

A sidecar `_data.json` is generated for every `_data.csv` file. It contains:

```json
{
    "variableMeasured": [ ... ],
    "metacheck:original_file": {
        "rel_path": "Data/Study_1_Upload_Data.sav",
        "format": "sav",
        "size_bytes": 245760,
        "is_raw": false
    },
    "metacheck:conversion": {
        "method": "haven::read_sav",
        "encoding_normalized": true,
        "rows_written": 500,
        "columns_written": 14,
        "haven_labels_extracted": true
    }
}
```

The `variableMeasured` array in the sidecar lists only the columns present in
**this specific file**, with their file-specific statistics. This overrides the
global `variableMeasured` for this file (per PsychDS inheritance rules), providing
exact per-file stats rather than the deduplicated global values.

### 9.2 Sidecar naming

Per PsychDS convention: the sidecar shares the exact filename of the data file but
with `.csv` replaced by `.json`.

```
data/source-Study1Data_data.csv
data/source-Study1Data_data.json    ← sidecar
```

---

## 10. Paper Metadata from GROBID XML

When `/Volumes/Models/expanded_xml/<paper_id>.xml` exists, the following metadata
is extracted and used in `dataset_description.json`:

| XML element | Maps to | Extraction |
|---|---|---|
| `<title>` | `schema:name` | Combined with study label |
| `<abstract>` | `schema:description` | Full text, paragraphs joined |
| `<author>` | `schema:author` | Array of `Person` objects (`name`, optionally `affiliation`) |
| `<idno type="DOI">` | `schema:identifier` | Formatted as `https://doi.org/{doi}` |
| `<date>` | `schema:datePublished` | ISO-8601 date |
| `<keywords>` | `schema:keywords` | Array of keyword strings |

### 10.1 Fallback when XML is unavailable

If no GROBID XML exists:

- `schema:name` = `"Repository <paper_id>"` + study label
- `schema:description` = `"Data repository for paper <paper_id>, <study_label>.
  Contains <n> data files with <n> columns."`
- `schema:author` = omitted
- `schema:identifier` = omitted

---

## 11. Sentinel Rows and Aggregate Directories

Sentinel rows in `structure.csv` (where `is_sentinel = TRUE`) represent collapsed
directories with >50 files. These require special handling:

1. **Do not create a single data file** for a sentinel. Instead, process each
   individual file in the sentinel directory separately (scan the actual directory
   on disk)
2. Record the sentinel status in `provenance.json` for traceability
3. Apply the `AGGREGATE_EXT_OVERRIDE` logic: files with unambiguous extensions
   (`.R` → code, `.jpg` → asset) are classified by extension, not by the sentinel's
   inherited LLM classification
4. For files within sentinel directories that are classified as data: convert each
   individually to `_data.csv` with a keyword that includes the subdirectory path

---

## 12. Edge Cases

### 12.1 Papers with no data files

If a paper has `type = "data"` on zero files (after ground-truth override), no
PsychDS dataset is produced. The paper is logged as `skipped_no_data` in the
conversion summary.

### 12.2 Papers with pipeline errors

Papers with `success = FALSE` in `bulk_summary.csv` are skipped. Error code is
recorded in the conversion summary.

### 12.3 Duplicate column names across files

When multiple data files in the same study have columns with identical names:
- The global `variableMeasured` contains one PropertyValue per unique name
- Per-file sidecars contain file-specific statistics
- The global PropertyValue uses stats from the file with the most non-missing values

### 12.4 Files exceeding size limits

Data files >500 MB are placed in `data/raw/` without CSV conversion. A sidecar
in `data/raw/` records why the file was not converted:

```json
{
    "metacheck:conversion_skipped": true,
    "metacheck:skip_reason": "file_size_exceeds_limit",
    "metacheck:file_size_mb": 750
}
```

### 12.5 Multi-level CSV headers

Files with multi-level headers (detected by the pipeline's `MULTILEVEL_HEADER_LOOKAHEAD`)
are written with the resolved sub-header row as the CSV header. The original
multi-level structure is preserved in the sidecar:

```json
{
    "metacheck:multi_level_header": true,
    "metacheck:header_groups": ["Condition A", "Condition B"],
    "metacheck:original_header_rows": 2
}
```

The `col_header_group` from `columns.csv` is included in each affected
PropertyValue:

```json
{
    "@type": "PropertyValue",
    "name": "rt",
    "description": "Reaction time",
    "metacheck:col_header_group": "Condition A"
}
```

### 12.6 Comma-decimal data

Columns with `col_type = "continuous_comma_decimal"` or `continuous_outliers_excluded`
have had their decimal separators normalised by the pipeline. The CSV output uses
standard period-decimal notation. The sidecar records the original format:

```json
{
    "metacheck:original_decimal_separator": "comma",
    "metacheck:n_coerced": 2
}
```

### 12.7 Empty columns

Columns with `col_type = "empty"` (all NA values) are preserved in the data file
but flagged in the PropertyValue:

```json
{
    "@type": "PropertyValue",
    "name": "unused_col",
    "metacheck:col_type": "empty",
    "metacheck:note": "All values NA in source data"
}
```

### 12.8 `row_id` column

PsychDS states: if a data file includes a column named `row_id`, every value must
be unique. During conversion:
- If a column is named `row_id` in the source, verify uniqueness
- If values are not unique, rename the column to `original_row_id` and add a note
  in the sidecar metadata

---

## 13. Conversion Summary Output

Each conversion run produces a summary CSV at `psychds/conversion_summary.csv`:

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier |
| `study_group` | character | Study group converted |
| `success` | logical | Whether PsychDS output was produced |
| `error` | character | Error message if failed |
| `n_data_files` | integer | Data files converted |
| `n_raw_files` | integer | Files placed in `data/raw/` |
| `n_variables` | integer | Unique column names in `variableMeasured` |
| `n_labelled` | integer | Variables with descriptions from codebooks |
| `has_paper_metadata` | logical | Whether GROBID XML was available |
| `has_ground_truth` | logical | Whether human validation was used |
| `output_path` | character | Relative path to the study's PsychDS directory |

---

## 14. Worked Example

### Input: paper `0956797615620784` (single-study)

**structure.csv** (3 files):
- `GambleWalkerData.csv` → type=data, group=other
- `GambleWalkerAnalysis.r` → type=code, group=other
- `GambleWalkerKey.rtf` → type=codebook, group=other

**columns.csv**: 8 columns (ID, Condition, Age, Sex, STAI_S_Y_PRE, SSS_total,
BART, STAI_S_Y_DURING, STAI_S_Y_POST)

**labels.csv**: All 8 columns labelled from `GambleWalkerKey.rtf`

### Output:

```
psychds/0956797615620784/
├── dataset_description.json
├── provenance.json
├── data/
│   ├── raw/
│   │   └── (no raw files — data was already CSV)
│   ├── source-GambleWalkerData_data.csv
│   └── source-GambleWalkerData_data.json
├── analysis/
│   └── GambleWalkerPsychologicalScienceAnalysis.r
└── documentation/
    └── GambleWalkerPsychologicalScienceKey.rtf
```

**dataset_description.json** (abbreviated):
```json
{
    "@context": {
        "schema": "https://schema.org/",
        "metacheck": "https://metacheck.io/ns/"
    },
    "@type": "schema:Dataset",
    "schema:name": "Wearing a Bicycle Helmet Can Increase Risk Taking — Data",
    "schema:description": "Participants completed risk-taking measures while wearing either a bicycle helmet or a baseball cap...",
    "schema:schemaVersion": "Psych-DS 0.1.0",
    "schema:author": [
        {
            "@type": "Person",
            "name": "Tim Gamble"
        },
        {
            "@type": "Person",
            "name": "Ian Walker"
        }
    ],
    "schema:identifier": "https://doi.org/10.1177/0956797615620784",
    "metacheck:paper_id": "0956797615620784",
    "metacheck:study_group": "other",
    "schema:variableMeasured": [
        {
            "@type": "PropertyValue",
            "name": "ID",
            "description": "Participant identification number",
            "metacheck:col_type": "unknown",
            "metacheck:label_source": "GambleWalkerPsychologicalScienceKey.rtf",
            "metacheck:label_method": "rules",
            "metacheck:codebook_variable": "ID",
            "metacheck:sample_values": "1 | 2 | 3 | 4 | 5"
        },
        {
            "@type": "PropertyValue",
            "name": "Condition",
            "description": "Which condition the participant was in: 1=helmet, 2=cap",
            "valuePattern": "1|2",
            "metacheck:col_type": "binary",
            "metacheck:label_source": "GambleWalkerPsychologicalScienceKey.rtf",
            "metacheck:label_method": "rules",
            "metacheck:codebook_variable": "Condition"
        },
        {
            "@type": "PropertyValue",
            "name": "Age",
            "description": "Age in years",
            "minValue": 17,
            "maxValue": 56,
            "metacheck:col_type": "continuous",
            "metacheck:statistics": {
                "n": 80,
                "n_missing": 0,
                "mean": 25.625,
                "sd": 6.874,
                "median": 25,
                "p25": 22,
                "p75": 27,
                "iqr": 5,
                "skewness": 1.913,
                "kurtosis": 5.137
            },
            "metacheck:label_source": "GambleWalkerPsychologicalScienceKey.rtf",
            "metacheck:label_method": "rules",
            "metacheck:codebook_variable": "Age"
        },
        {
            "@type": "PropertyValue",
            "name": "SSS_total",
            "description": "Sensation Seeking Scale score",
            "minValue": 11,
            "maxValue": 36,
            "metacheck:col_type": "continuous",
            "metacheck:statistics": {
                "n": 80,
                "n_missing": 0,
                "mean": 20.95,
                "sd": 6.455,
                "median": 20,
                "p25": 16,
                "p75": 26,
                "iqr": 10,
                "skewness": 0.206,
                "kurtosis": -0.864
            },
            "metacheck:label_source": "GambleWalkerPsychologicalScienceKey.rtf",
            "metacheck:label_method": "llm",
            "metacheck:codebook_variable": "SSS total"
        }
    ]
}
```

### Input: paper `0956797614524581` (multi-study)

**structure.csv**: 12 data files across 3 studies (ex1, ex2, ex3), each study has
both `.csv` and `.sav` versions, plus abbreviated variants.

### Output:

```
psychds/0956797614524581/
├── study-ex1/
│   ├── dataset_description.json
│   ├── provenance.json
│   ├── data/
│   │   ├── raw/
│   │   │   ├── Study_1_Upload_Data.sav
│   │   │   └── Study_1_abbreviated_data.sav
│   │   ├── source-Study1UploadData_data.csv
│   │   ├── source-Study1UploadData_data.json
│   │   ├── source-Study1abbreviateddata_data.csv
│   │   └── source-Study1abbreviateddata_data.json
│   └── ...
├── study-ex2/
│   ├── dataset_description.json
│   ├── data/
│   │   ├── raw/
│   │   │   ├── Study_2_Upload_Data.sav
│   │   │   └── Study_2_abbreviated_data.sav
│   │   ├── source-Study2UploadData_data.csv
│   │   └── ...
│   └── ...
├── study-ex3/
│   └── ...
└── shared/
    └── (any files with group=na or group=other that can't be scoped)
```

Each study's `dataset_description.json` contains only the `variableMeasured` for
columns in that study's data files.

---

## 15. Implementation Strategy

### 15.1 New script: `3_psychds_convert.R`

A new pipeline stage script following the existing naming convention. Function
signature:

```r
run_psychds_convert <- function(paper_id,
                                 output_base = "./psychds",
                                 use_ground_truth = TRUE,
                                 xml_dir = "/Volumes/Models/expanded_xml")
```

### 15.2 Dependencies

- No new packages required
- `haven` (already installed) — for full data reads of SPSS/Stata
- `readxl` (already installed) — for full Excel reads
- `jsonlite` (already installed) — for JSON-LD metadata generation
- `officer` / `pdftools` (already installed) — only for text extraction if needed

### 15.3 Execution modes

| Mode | Script | Purpose |
|---|---|---|
| Single paper | `run_psychds_single.R` | Convert one paper to PsychDS (dev/test) |
| Bulk | `run_psychds_bulk.R` | Convert all papers; crash-resilient via `conversion_summary.csv` |

### 15.4 Performance considerations

Full data reads (not 5-row samples) will be significantly slower than the index
stage. Mitigations:

- Skip papers where PsychDS output already exists (resume support)
- Process files in order of increasing size
- Log per-file timing for bottleneck identification
- Respect the 500 MB file size limit — do not attempt to convert huge files

### 15.5 Testing approach

1. **Unit test**: Convert 3 known papers (single-study, multi-study, no-codebook)
   and verify PsychDS validator passes
2. **Round-trip test**: Read the `_data.csv` back and verify column count, row
   count, and column names match the pipeline's `columns.csv`
3. **Metadata test**: Parse `dataset_description.json` and verify all columns from
   `columns.csv` appear in `variableMeasured`
4. **Edge case tests**: 500MB file, sentinel directory, multi-level headers,
   comma-decimal data, haven-labelled data

---

## 16. PsychDS Compliance Checklist

| Rule | How satisfied |
|---|---|
| `dataset_description.json` in root | Generated per study |
| Valid JSON-LD | `@context`, `@type` present |
| `name` field | Paper title + study label |
| `description` field | Abstract or generated description |
| `variableMeasured` array | All column headers as PropertyValue objects |
| `@type = "Dataset"` | Always set |
| `data/` subdirectory | Always created |
| At least one `_data.csv` in `data/` | At least one converted data file per study |
| CSV files UTF-8 encoded | Ensured during conversion |
| CSV headers in first row | Ensured during conversion |
| Equal cells per row | Validated during write |
| `[key-value_]+data.csv` filename pattern | `source-{name}_data.csv` pattern |
| `row_id` uniqueness (if column exists) | Checked; renamed if violated |

### Conventions (warnings, not errors)

| Convention | How satisfied |
|---|---|
| Sidecar metadata | Generated for every `_data.csv` |
| Schema.org type checking | `author` as `Person`, `identifier` as URL |
| Additional directories | `analysis/`, `documentation/`, `materials/` |
| Canonical keywords | `source`, `version`, `sheet` from PsychDS list |

---

## 17. Open Questions

1. **Namespace URI for metacheck**: `https://metacheck.io/ns/` is a placeholder.
   Should this be a real resolvable URI with a JSON-LD context definition?

2. **SPSS/Stata value labels in CSV**: Should the CSV contain numeric codes (preserving
   original data exactly) or decoded string labels (more human-readable)? This spec
   defaults to numeric codes with labels in metadata, but this is configurable.

3. **Study naming when group is ambiguous**: Some papers have `group = "other"` for
   all files. Should this produce `study-other/` or just a flat root-level dataset?
   This spec uses `study-other/` only for multi-study papers; single-study papers
   use root level.

4. **Unmatched codebook variables**: Should codebook variables that don't match any
   data column still appear in `variableMeasured`? This spec includes them with a
   flag — they represent documented variables that may be in raw data not yet
   converted.

5. **CSV data already in pipeline `data/` directory**: For files that are already CSV,
   should they be copied verbatim or re-read and re-written to normalise encoding?
   This spec re-reads and re-writes to guarantee UTF-8 and consistent formatting,
   which is safer but slower.

6. **PsychDS version**: The spec references "Psych-DS 0.1.0". The actual version
   should be confirmed against the validator being used.
