# Output Schemas

Five CSV files are produced per paper run (three from `0_index.R`, two from `2_codebook_label.R`).

---

## `outputs/<paper_id>/structure.csv`

One row per file discovered in the paper's OSF repository.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `path` | character | Absolute local path to the file |
| `rel_path` | character | Path relative to the paper's download directory |
| `filename` | character | Basename of the file |
| `ext` | character | File extension (lowercase) |
| `type` | character | LLM-assigned file type — see [File Types](#file-types) |
| `group` | character | Experiment/study group — see [Groups](#groups) |
| `is_raw` | logical | `TRUE` if file appears to be raw (unprocessed) data |
| `is_sentinel` | logical | `TRUE` if row represents a collapsed folder (>50 files) |

### File Types

| Value | Meaning |
|---|---|
| `data` | Tabular data file intended for statistical analysis |
| `codebook` | Variable descriptions / data dictionary |
| `code` | Analysis or processing script |
| `supplemental` | Supporting materials — survey instruments, consent forms, saved plots, SPSS output, etc. |
| `doc` | Manuscript, report, general notes |
| `readme` | README file |
| `asset` | Image, audio, video, stimulus material |
| `other` | Does not fit any above category |

### Groups

| Value | Meaning |
|---|---|
| `ex<N>` | Main numbered experiment/study (e.g. `ex1`, `ex2`, `ex4a`) |
| `pilot<N>` | Pilot study (e.g. `pilot1`, `pilot2`) |
| `other` | Not tied to a specific experiment (shared files, previous versions) |
| `na` | Not applicable (readme, asset, supplemental, other) |

---

## `outputs/<paper_id>/columns.csv`

One row per column in each data file classified as `type = "data"`.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier |
| `source_file` | character | Relative path to the source data file |
| `filename` | character | Basename of the source file |
| `group` | character | Experiment group inherited from `structure.csv` |
| `column_name` | character | Column name as it appears in the data file |
| `sample_values` | character | First up to 5 non-NA values, pipe-separated (`\|`) |
| `col_type` | character | Classified column type — see [Column Types](#column-types) |
| `n_coerced` | integer | Values excluded during comma-decimal normalisation; `NA` when not applicable |
| `n` | integer | Count of non-missing values |
| `n_missing` | integer | Count of missing (`NA`) values |
| `mean` | numeric | Arithmetic mean (numeric types only; `NA` otherwise) |
| `sd` | numeric | Standard deviation |
| `se` | numeric | Standard error of the mean |
| `median` | numeric | Median |
| `min` | numeric | Minimum |
| `max` | numeric | Maximum |
| `range` | numeric | `max - min` |
| `p25` | numeric | 25th percentile |
| `p75` | numeric | 75th percentile |
| `iqr` | numeric | Interquartile range (`p75 - p25`) |
| `skewness` | numeric | Pearson moment skewness |
| `kurtosis` | numeric | Excess kurtosis (normal distribution = 0) |

Statistics are populated only for numeric column types (`continuous`,
`continuous_comma_decimal`, `continuous_outliers_excluded`). All other types
have `NA` for the 12 stat columns.

### Column Types

| Value | Assigned by | Meaning |
|---|---|---|
| `continuous` | Rule 6a / Rule 6 / LLM / fallback | Numeric measurement (decimal or integer with >20 unique values, or LLM-confirmed) |
| `ordinal` | LLM | Ordered integer scale with few levels (Likert, rating) |
| `binary` | Rule 2 | Exactly two unique non-NA values |
| `categorical` | Rule 8 | Unordered group code with few short levels |
| `date` | Rule 4 | Date-parseable values |
| `id` | LLM | Row or participant identifier |
| `text` | Rule 5 / Rule 9 | Free-text or long string values |
| `continuous_comma_decimal` | Rule 7 | Numeric with comma as decimal separator (≥95% convertible) |
| `continuous_outliers_excluded` | Rule 7 | Numeric with comma separator but some non-convertible values (80–95%) |
| `empty` | Rule 1 | All values are `NA` |
| `unknown` | LLM / fallback | Cannot be determined (genuinely uninformative name and values) |

---

## `bulk_summary.csv`

One row per paper, appended immediately after each paper completes. Used by the bulk
runner to resume after a crash.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier |
| `success` | logical | `TRUE` if the paper completed without error |
| `error` | character | Error code or message if `success = FALSE`; `NA` otherwise |
| `elapsed_ms` | integer | Total wall-clock time in milliseconds |
| `download_ms` | integer | Time spent downloading in milliseconds |
| `llm_ms` | integer | Time spent on all LLM calls in milliseconds |
| `column_ms` | integer | Time spent on column extraction in milliseconds |
| `n_files` | integer | Total files discovered in the repository |
| `n_data_files` | integer | Files classified as `type = "data"` |
| `n_agg_dirs` | integer | Folders collapsed to sentinel rows |
| `n_raw` | integer | Data files detected as raw (unprocessed) |
| `n_nonraw` | integer | Data files detected as processed/derived |
| `n_columns` | integer | Total columns extracted across all data files |
| `n_src_files` | integer | Source data files from which columns were extracted |

### Error Codes

| Code | Cause |
|---|---|
| `no_links` | OSF repository has no downloadable file links |
| `download_failed` | Network or OSF API error during download |
| `empty_repo` | Downloaded repository contains no usable files after unpacking (retried once) |
| `too_large` | Exceeded download size limit (10 GB) or file path count limit (200 paths) |

---

## `outputs/<paper_id>/labels.csv`

One row per column in each data file (parallel to `columns.csv`). Produced by `2_codebook_label.R`.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `source_file` | character | Relative path to the source data file (join key to `columns.csv`) |
| `column_name` | character | Column name as it appears in the data file |
| `group` | character | Experiment group inherited from `_columns.csv` |
| `label` | character | Human-readable label/description from the matched codebook variable; `NA` if unlabelled |
| `codebook_variable` | character | Variable name as written in the codebook; `NA` if unlabelled; pipe-separated if multiple candidates |
| `label_source` | character | Basename of the codebook file that provided the label; `NA` if unlabelled; pipe-separated if multiple sources |
| `label_status` | character | Labelling outcome — see [Label Status Values](#label-status-values) |
| `label_method` | character | How the label was determined: `"rules"` = normalized string match; `"llm"` = secondary LLM pass; `NA` = column is unlabelled |

### Label Status Values

| Value | Meaning |
|---|---|
| `labelled` | Column matched exactly one codebook variable with no conflicts |
| `unlabelled` | Column has no matching codebook variable |
| `conflicting_definition` | Column matched a variable present in multiple codebooks with different definitions; all candidates pipe-concatenated in `label` |
| `ambiguous_experiment` | Column name exists only in a different experiment group's codebook; candidates pipe-concatenated |
| `no_codebook` | Paper has no codebook or readme files; entire paper is unlabelled |
| `llm` | Matched by secondary LLM pass after rule-based matching found no match |

---

## `outputs/<paper_id>/codebook_coverage.csv`

One row per variable extracted from any codebook/readme file. Produced by `2_codebook_label.R`.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier |
| `codebook_variable` | character | Variable name as written in the codebook |
| `label` | character | Human-readable label/description |
| `codebook_source` | character | Basename of the codebook file |
| `group` | character | Experiment group scope inferred from codebook context; `NA` if no scope detected (applicable to all groups) |
| `match_status` | character | `matched` — variable found in at least one data column; `unmatched_in_data` — not found in any data column |
