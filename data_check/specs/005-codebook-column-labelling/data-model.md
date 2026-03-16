# Data Model: Codebook Column Labelling

**Feature**: 005-codebook-column-labelling
**Date**: 2026-03-16

---

## New Output Files

### `structure/<paper_id>_labels.csv`

One row per column in each data file (mirrors `_columns.csv` key columns, adds label fields).

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier (leading zeros preserved) |
| `source_file` | character | Relative path to the source data file (join key to `_columns.csv`) |
| `column_name` | character | Column name as it appears in the data file (join key) |
| `group` | character | Experiment group inherited from `_columns.csv` |
| `label` | character | Human-readable label/description from the matched codebook variable; `NA` if unlabelled |
| `codebook_variable` | character | Variable name as written in the codebook (may differ in case/whitespace from `column_name`); `NA` if unlabelled |
| `label_source` | character | Basename of the codebook file that provided the label; `NA` if unlabelled; pipe-separated if multiple sources conflict |
| `label_status` | character | Classification of the labelling outcome — see [Label Status Values](#label-status-values) |

#### Label Status Values

| Value | Meaning |
|---|---|
| `labelled` | Column matched exactly one codebook variable with no conflicts |
| `unlabelled` | Column has no matching codebook variable |
| `conflicting_definition` | Column matched a variable present in multiple codebooks with different definitions; all candidates attached in `label` (pipe-separated) |
| `ambiguous_experiment` | Column matched codebook variables from multiple experiment groups; candidates attached |
| `no_codebook` | Paper has no codebook or readme files; entire paper is unlabelled |

---

### `structure/<paper_id>_codebook_coverage.csv`

One row per variable extracted from any codebook/readme file.

| Column | Type | Description |
|---|---|---|
| `paper_id` | character | Paper identifier |
| `codebook_variable` | character | Variable name as written in the codebook |
| `label` | character | Human-readable label/description |
| `codebook_source` | character | Basename of the codebook file |
| `group` | character | Experiment group scope inferred from codebook context; `all` if no scope detected |
| `match_status` | character | `matched` — variable found in at least one data column; `unmatched_in_data` — not found in any data column |

---

## Key Entities (in-memory)

### `CodebookVariable` (data.frame row)

Produced by `parse_codebook(path)`. Fields: `codebook_variable`, `label`, `codebook_source`, `group`.

### `ColumnLabel` (data.frame row)

Produced by `match_column_labels()`. Fields: all `_labels.csv` columns above.

### `LabellingResult` (named list)

Return value of `run_codebook_label(paper_id)`:
```
list(
  labels_df   = <data.frame>,   # written to _labels.csv
  coverage_df = <data.frame>,   # written to _codebook_coverage.csv
  n_labelled       = integer,
  n_unlabelled     = integer,
  n_codebook_vars  = integer,
  n_matched_vars   = integer,
  label_status     = character  # overall status: "ok" | "no_codebook" | "partial"
)
```

---

## New Constants (in `2_codebook_label.R`)

| Constant | Value | Purpose |
|---|---|---|
| `MAX_CODEBOOK_LLM_CALLS` | 3 | Max LLM calls per paper for codebook parsing |
| `MAX_CODEBOOK_FILE_MB` | 100 | Files larger than this (MB) are skipped with a warning |
| `CODEBOOK_TYPES` | `c("codebook", "readme")` | `type` values from `_structure.csv` treated as codebook sources |

---

## New Helpers (added to `helper.R`)

### `parse_codebook(path)`

Reads a single codebook file and returns a data.frame of `CodebookVariable` rows.

- Attempts rule-based extraction first (structured CSV/Excel/SPSS label attributes).
- Falls back to LLM extraction for unstructured text.
- Returns `NULL` (with warning) on parse failure or oversized file.
- Does not consume LLM calls for structured files.

### `match_column_labels(columns_df, codebook_vars_df)`

Joins `_columns.csv` rows to extracted codebook variables.

- Normalises both sides: `tolower()` + `trimws()` + collapse interior whitespace.
- Respects experiment-group scoping (see research.md Decision 3).
- Detects and flags conflicts (`conflicting_definition`, `ambiguous_experiment`).
- Returns a `_labels.csv`-shaped data.frame covering all rows of `columns_df`.

---

## Joins / Relationships

```
_structure.csv
  ── (paper_id, path, type, group) ──► parse_codebook()
                                         └─► codebook_vars_df

_columns.csv
  ── (paper_id, source_file, column_name, group) ──► match_column_labels()
                                                       └─► _labels.csv

codebook_vars_df ──► _codebook_coverage.csv
```

All joins on `paper_id` + normalised column/variable name + `group`.
