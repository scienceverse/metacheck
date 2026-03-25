# Data Model: PsychDS Conversion

**Feature**: 021-psychds-conversion

---

## Input Data Structures

All inputs are existing pipeline outputs, read-only.

### `outputs/<paper_id>/structure.csv` (after ground-truth overlay)

| Field | Type | Used for |
|---|---|---|
| `paper_id` | character | Paper identity |
| `rel_path` | character | Original file location (→ provenance.json) |
| `filename` | character | Original basename (→ non-data file naming) |
| `ext` | character | File extension (→ conversion dispatch) |
| `type` | character | File classification (`data`, `code`, `codebook`, etc.) |
| `group` | character | Study group (`ex1`, `ex2`, `na`, `other`) |
| `is_raw` | logical | Raw flag (→ `data/raw/` placement, `version-raw` prefix) |
| `is_sentinel` | logical | Sentinel row flag (→ disk scan required) |

### `outputs/<paper_id>/columns.csv`

| Field | Type | Used for |
|---|---|---|
| `source_file` | character | Join key with labels.csv |
| `column_name` | character | PropertyValue `name` |
| `group` | character | Study assignment |
| `col_header_group` | character (nullable) | `metacheck:col_header_group` in PropertyValue |
| `col_type` | character | Drives minValue/maxValue/valuePattern selection |
| `sample_values` | character | `metacheck:sample_values` and valuePattern source |
| `n`, `n_missing` | integer | Statistics block |
| `mean`, `sd`, `se`, `median`, `min`, `max`, `p25`, `p75`, `iqr`, `skewness`, `kurtosis` | numeric | Statistics block (continuous types only) |

### `outputs/<paper_id>/labels.csv`

| Field | Type | Used for |
|---|---|---|
| `source_file` | character | Join key with columns.csv |
| `column_name` | character | Join key |
| `label` | character | PropertyValue `description` |
| `codebook_variable` | character | `metacheck:codebook_variable` |
| `label_source` | character | `metacheck:label_source` |
| `label_status` | character | `"labelled"` → include description |
| `label_method` | character | `metacheck:label_method` |

### `outputs/<paper_id>/codebook_coverage.csv`

| Field | Type | Used for |
|---|---|---|
| `codebook_variable` | character | Unmatched variable name |
| `label` | character | Unmatched variable description |
| `match_status` | character | `"unmatched"` → append to variableMeasured |

### `ground_truth/<paper_id>.csv` (optional)

| Field | Type | Used for |
|---|---|---|
| `rel_path` | character | Match key |
| `type_gt` | character | Overrides `structure.csv` `type` |
| `group_gt` | character | Overrides `structure.csv` `group` |
| `is_raw_gt` | logical | Overrides `structure.csv` `is_raw` |
| `validated_at` | character | Timestamp → provenance.json |
| `annotator` | character | Annotator name → provenance.json |

---

## Output Data Structures

### `psychds/conversion_summary.csv`

Appended one row per study after each conversion. Auto-resume key is `(paper_id, study_group)`.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `study_group` | character | Study group (`ex1`, `other`, etc.) or `"all"` for single-study |
| `success` | logical | Conversion succeeded |
| `error` | character | `pipeline_failed`, `no_data_files`, or free-text exception |
| `n_data_files` | integer | Data files converted to CSV |
| `n_raw_files` | integer | Files placed in `data/raw/` |
| `n_variables` | integer | Unique column names in `variableMeasured` |
| `n_labelled` | integer | Variables with `description` from codebook |
| `has_paper_metadata` | logical | GROBID XML was available |
| `has_ground_truth` | logical | Ground truth overrides were applied |
| `output_path` | character | Relative path to study directory |

---

### `dataset_description.json` schema

```json
{
  "@context": {
    "schema": "https://schema.org/",
    "metacheck": "https://metacheck.io/ns/"
  },
  "@type": "schema:Dataset",

  // Required (always present)
  "schema:name": "<paper_title> — <study_label>",
  "schema:description": "<abstract_or_generated>",
  "schema:variableMeasured": [ <PropertyValue>, ... ],

  // Recommended (from GROBID XML if available)
  "schema:author": [ { "@type": "schema:Person", "schema:name": "..." }, ... ],
  "schema:identifier": "https://doi.org/...",
  "schema:datePublished": "YYYY-MM-DD",
  "schema:keywords": [ "..." ],
  "schema:schemaVersion": "Psych-DS 0.1.0",

  // Provenance (always present)
  "metacheck:paper_id": "...",
  "metacheck:study_group": "ex1",
  "metacheck:pipeline_version": "021",
  "metacheck:conversion_date": "YYYY-MM-DD",
  "metacheck:pipeline_status": {
    "index_success": true,
    "codebook_success": true,
    "n_files_total": 0,
    "n_data_files": 0,
    "n_columns": 0,
    "n_labelled_columns": 0,
    "label_status": "ok"
  },
  "metacheck:source_repository": {
    "platform": "osf",
    "download_path": "data/<paper_id>/",
    "original_structure": "<root_dir_name>/"
  },

  // Multi-study papers only
  "metacheck:shared_resources": "../shared/",
  "metacheck:shared_files": [ "..." ]
}
```

---

### PropertyValue schema

**Minimal** (no label, no statistics):
```json
{
  "@type": "PropertyValue",
  "name": "<column_name>",
  "metacheck:col_type": "categorical",
  "metacheck:source_file": "<rel_path>",
  "metacheck:sample_values": "a | b | c"
}
```

**Full continuous** (label + stats):
```json
{
  "@type": "PropertyValue",
  "name": "<column_name>",
  "description": "<label>",
  "minValue": 0,
  "maxValue": 100,
  "metacheck:col_type": "continuous",
  "metacheck:source_file": "<rel_path>",
  "metacheck:sample_values": "25 | 30 | 28",
  "metacheck:statistics": {
    "n": 80, "n_missing": 0,
    "mean": 25.6, "sd": 6.9, "se": 0.77,
    "median": 25, "p25": 22, "p75": 27,
    "iqr": 5, "skewness": 1.9, "kurtosis": 5.1
  },
  "metacheck:label_source": "<filename>",
  "metacheck:label_method": "rules",
  "metacheck:codebook_variable": "<name>"
}
```

**Categorical with value labels** (SPSS/Stata):
```json
{
  "@type": "PropertyValue",
  "name": "Condition",
  "description": "<label>",
  "valuePattern": "1|2",
  "metacheck:col_type": "binary",
  "metacheck:source_file": "<rel_path>",
  "metacheck:value_labels": { "1": "helmet", "2": "cap" }
}
```

**Unmatched codebook variable**:
```json
{
  "@type": "PropertyValue",
  "name": "<codebook_variable>",
  "description": "<label>",
  "metacheck:match_status": "unmatched_in_data",
  "metacheck:source_file": "<codebook_source>"
}
```

---

### `provenance.json` schema

One entry per file placed in the PsychDS output.

```json
{
  "file_provenance": [
    {
      "psychds_path": "data/source-Study1Data_data.csv",
      "original_rel_path": "Data/Study_1_Data.csv",
      "original_format": "csv",
      "pipeline_type": "data",
      "pipeline_group": "ex1",
      "pipeline_is_raw": false,
      "ground_truth_validated": false,
      "txt_extraction_attempted": false
    },
    {
      "psychds_path": "documentation/codebook.pdf",
      "original_rel_path": "Materials/Codebook.pdf",
      "original_format": "pdf",
      "pipeline_type": "codebook",
      "pipeline_group": "na",
      "pipeline_is_raw": false,
      "ground_truth_validated": false,
      "txt_extraction_attempted": true,
      "txt_extraction_skipped": false,
      "txt_psychds_path": "documentation/txt/codebook.txt"
    },
    {
      "psychds_path": "documentation/scan.pdf",
      "original_rel_path": "Materials/scan.pdf",
      "original_format": "pdf",
      "pipeline_type": "doc",
      "pipeline_group": "na",
      "pipeline_is_raw": false,
      "ground_truth_validated": false,
      "txt_extraction_attempted": true,
      "txt_extraction_skipped": true,
      "txt_skip_reason": "no_extractable_text"
    },
    {
      "psychds_path": "data/raw/Study_1_Data.sav",
      "original_rel_path": "Data/Study_1_Data.sav",
      "original_format": "sav",
      "pipeline_type": "data",
      "pipeline_group": "ex1",
      "pipeline_is_raw": false,
      "ground_truth_validated": true,
      "ground_truth": {
        "type_gt": "data",
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

### `_data.json` sidecar schema

```json
{
  "schema:variableMeasured": [ <PropertyValue for columns in this file only> ],
  "metacheck:original_file": {
    "rel_path": "<original rel_path>",
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

**Oversized file sidecar** (in `data/raw/`):
```json
{
  "metacheck:conversion_skipped": true,
  "metacheck:skip_reason": "file_size_exceeds_limit",
  "metacheck:file_size_mb": 750
}
```

---

## Core Algorithm: `variableMeasured` Construction

```
Input: study_group S, paper_id P
Output: ordered list of PropertyValue objects

1. cols    ← rows from columns.csv where group == S
2. labels  ← rows from labels.csv where group == S
3. joined  ← left_join(cols, labels, by = c("source_file", "column_name"))
4. deduped ← deduplicate joined by column_name:
               for duplicates, prefer rows where is_raw == FALSE;
               among ties, keep first occurrence
5. pv_list ← []
6. for each row in deduped:
     pv ← { "@type": "PropertyValue", name: column_name }
     if label_status == "labelled":
         pv.description         ← label
         pv.metacheck:label_source      ← label_source
         pv.metacheck:label_method      ← label_method
         pv.metacheck:codebook_variable ← codebook_variable
     if col_type in NUMERIC_TYPES:
         pv.minValue            ← min
         pv.maxValue            ← max
         pv.metacheck:statistics ← { n, n_missing, mean, sd, se,
                                      median, p25, p75, iqr,
                                      skewness, kurtosis }
     if col_type in CATEGORICAL_TYPES:
         pv.valuePattern        ← pipe_join(unique(split(sample_values, "|")))
     if col_header_group is not NA:
         pv.metacheck:col_header_group ← col_header_group
     pv.metacheck:col_type     ← col_type
     pv.metacheck:source_file  ← source_file
     pv.metacheck:sample_values ← sample_values
     pv_list.append(Filter(Negate(is.null), pv))
7. for each row in codebook_coverage where match_status == "unmatched":
     pv ← { "@type": "PropertyValue",
              name: codebook_variable,
              description: label,
              metacheck:match_status: "unmatched_in_data",
              metacheck:source_file: codebook_source }
     pv_list.append(pv)
8. return pv_list

NUMERIC_TYPES   ← c("continuous", "continuous_comma_decimal",
                     "continuous_outliers_excluded")
CATEGORICAL_TYPES ← c("categorical", "binary", "ordinal")
```

---

## File Placement Rules

| Pipeline `type` | `group` | Destination (single-study) | Destination (multi-study) |
|---|---|---|---|
| `data` | any | `data/source-<name>_data.csv` | `study-<group>/data/source-<name>_data.csv` |
| `data` (original) | any | `data/raw/<filename>` | `study-<group>/data/raw/<filename>` |
| `code` | `ex<N>` or `pilot<N>` | `analysis/<filename>` | `study-<group>/analysis/<filename>` |
| `codebook`, `doc`, `supplemental`, `other` | specific group | `documentation/<filename>` | `study-<group>/documentation/<filename>` |
| `codebook` or `doc` (PDF/DOCX/RTF, text extracted) | specific group | `documentation/txt/<basename>.txt` | `study-<group>/documentation/txt/<basename>.txt` |
| `asset` | specific group | `materials/<filename>` | `study-<group>/materials/<filename>` |
| `readme` | any | `README.<ext>` | `study-<group>/README.<ext>` |
| any | `na` or `other` (unscoped) | n/a (single-study: placed normally) | `shared/<type_dir>/<filename>` |

---

## GROBID XML Extraction (xml2)

```r
parse_grobid_xml <- function(xml_path) {
  doc <- xml2::read_xml(xml_path)
  xml2::xml_ns_strip(doc)           # remove namespace prefixes

  list(
    title    = xml2::xml_text(xml2::xml_find_first(doc,
                 ".//titleStmt/title[@type='main']")),
    abstract = paste(xml2::xml_text(xml2::xml_find_all(doc,
                 ".//abstract//p")), collapse = " "),
    authors  = lapply(xml2::xml_find_all(doc, ".//author/persName"),
                 function(p) paste(
                   xml2::xml_text(xml2::xml_find_first(p, ".//forename[1]")),
                   xml2::xml_text(xml2::xml_find_first(p, ".//surname"))
                 )),
    doi      = xml2::xml_text(xml2::xml_find_first(doc,
                 ".//idno[@type='DOI']")),
    date     = xml2::xml_attr(xml2::xml_find_first(doc,
                 ".//publicationStmt//date[@type='published']"), "when"),
    keywords = xml2::xml_text(xml2::xml_find_all(doc, ".//keywords/term"))
  )
}
```
