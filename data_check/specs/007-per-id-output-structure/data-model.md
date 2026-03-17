# Data Model: Per-ID Output Directory Structure

**Branch**: `007-per-id-output-structure` | **Date**: 2026-03-17

## Filesystem Layout

```
data_check/
├── outputs/                          ← replaces structure/
│   └── <paper_id>/                   ← one directory per paper (character ID)
│       ├── structure.csv             ← from 0_index.R (was <paper_id>_structure.csv)
│       ├── columns.csv               ← from 0_index.R (was <paper_id>_columns.csv)
│       ├── labels.csv                ← from 2_codebook_label.R (was <paper_id>_labels.csv)
│       └── codebook_coverage.csv     ← from 2_codebook_label.R (was <paper_id>_codebook_coverage.csv)
├── bulk_summary.csv                  ← UNCHANGED: repo root, cross-paper aggregate
└── structure/                        ← EMPTY after migration; safe to delete
```

## File Schemas (unchanged)

All CSV schemas are identical to their predecessors. Only the path changes.

| File | Key columns | Produced by |
|---|---|---|
| `structure.csv` | `paper_id`, `path`, `rel_path`, `filename`, `ext`, `type`, `group`, `is_raw`, `is_sentinel` | `0_index.R` |
| `columns.csv` | `paper_id`, `source_file`, `filename`, `group`, `column_name`, `col_type`, stats columns | `0_index.R` |
| `labels.csv` | `paper_id`, `source_file`, `column_name`, `group`, `label`, `codebook_variable`, `label_status`, `label_method` | `2_codebook_label.R` |
| `codebook_coverage.csv` | `codebook_variable`, `label`, `codebook_source`, `group` | `2_codebook_label.R` |
| `bulk_summary.csv` | `paper_id`, `success`, `error`, timing columns, count columns | `run_index_bulk.R` |

## Helper Contract

```r
# In helper.R
paper_output_dir <- function(paper_id) {
  dir_path <- file.path("./data_check/outputs", paper_id)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  dir_path
}
```

**Invariants**:
- `paper_id` is always a character string (never numeric).
- The function is idempotent: calling it when the directory already exists is safe.
- Returns the directory path (not a file path); callers append the filename.

## Constant Changes

| Script | Old | New |
|---|---|---|
| `0_index.R` | `STRUCTURE_DIR <- "./data_check/structure"` | `OUTPUT_DIR <- "./data_check/outputs"` |
| `1_data_label.R` | `STRUCTURE_DIR <- "./data_check/structure"` | `OUTPUT_DIR <- "./data_check/outputs"` |
| `2_codebook_label.R` | `STRUCTURE_DIR <- "./data_check/structure"` | `OUTPUT_DIR <- "./data_check/outputs"` |

## Call-site Changes (per script)

### 0_index.R

| Old | New |
|---|---|
| `if (!dir.exists(STRUCTURE_DIR)) dir.create(STRUCTURE_DIR, recursive = TRUE)` | *(removed — `paper_output_dir()` handles this)* |
| `structure_out <- file.path(STRUCTURE_DIR, paste0(paper_id, "_structure.csv"))` | `structure_out <- file.path(paper_output_dir(paper_id), "structure.csv")` |
| `columns_out <- file.path(STRUCTURE_DIR, paste0(paper_id, "_columns.csv"))` | `columns_out <- file.path(paper_output_dir(paper_id), "columns.csv")` |

### 1_data_label.R

| Old | New |
|---|---|
| `structure_path <- file.path(STRUCTURE_DIR, paste0(paper_id, "_structure.csv"))` | `structure_path <- file.path(paper_output_dir(paper_id), "structure.csv")` |
| `out_path <- file.path(STRUCTURE_DIR, paste0(paper_id, "_columns.csv"))` | `out_path <- file.path(paper_output_dir(paper_id), "columns.csv")` |

### 2_codebook_label.R

| Old | New |
|---|---|
| `structure_path <- file.path(STRUCTURE_DIR, paste0(paper_id, "_structure.csv"))` | `structure_path <- file.path(paper_output_dir(paper_id), "structure.csv")` |
| `columns_path <- file.path(STRUCTURE_DIR, paste0(paper_id, "_columns.csv"))` | `columns_path <- file.path(paper_output_dir(paper_id), "columns.csv")` |
| `labels_out <- file.path(STRUCTURE_DIR, paste0(paper_id, "_labels.csv"))` | `labels_out <- file.path(paper_output_dir(paper_id), "labels.csv")` |
| `coverage_out <- file.path(STRUCTURE_DIR, paste0(paper_id, "_codebook_coverage.csv"))` | `coverage_out <- file.path(paper_output_dir(paper_id), "codebook_coverage.csv")` |

## Migration Script Logic

`migrate_structure.R` (one-time):

1. List `structure/*.csv` files.
2. For each: parse `<paper_id>` from filename prefix (everything before the first `_` that matches a known ID pattern).
3. Create `outputs/<paper_id>/` via `paper_output_dir()`.
4. `file.copy(src, dest)` → verify `file.exists(dest)` → `file.remove(src)`.
5. Report: N files moved, N failures (if any).
