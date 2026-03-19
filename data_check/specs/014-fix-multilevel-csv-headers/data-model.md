# Data Model: Fix Multi-Level CSV Header Skip Rule

This feature adds one new column to `columns.csv` and introduces transient in-memory structures within `extract_column_info()`. No other output schemas change. `helper.R`, `2_codebook_label.R`, and `labels.csv` are unaffected.

---

## In-Memory Structures (within `extract_column_info()`)

### `df` — raw data.frame from `read_data_head()`

| Attribute | Before fix | After fix (Branch 1) | After fix (Branch 2) |
|---|---|---|---|
| Column names | Row-1 values (`...N`-heavy) | Sub-header row values; NA/empty cells fall back to `...N` | Unchanged (partial labels as-is) |
| Rows | All file rows below header | Sliced to start at `sub_header_row + 1` | Unchanged |

### `col_header_group` — transient character vector

Computed immediately after `auto_named` check, before any renaming. Length = `ncol(df)`.

| Source | Value |
|---|---|
| Branch 1 (sub-header found) | Forward-filled row-1 group prefixes (e.g., `SHAM`, `BETA`, `ALPHA`); `NA` for columns before the first named group |
| Branch 2 (partial labels, no sub-header) | `rep(NA_character_, ncol(df))` |
| No multi-level structure triggered | `rep(NA_character_, ncol(df))` |

**Forward-fill rule**: Strip `...\d+$` suffix from each `names(df)` value. Non-empty result = group label for that column and all following pure-`...N` columns until the next non-empty result.

### `row1_names` — transient character vector

A snapshot of `names(df)` taken before any renaming. Used as fallback for NA/empty sub-header cells. Discarded after Branch 1 renaming.

---

## Output Schema Changes

### `columns.csv` — one new column added

| Column | Type | Change |
|---|---|---|
| `col_header_group` | character (nullable) | **NEW** — condition/group label from row 1; `NA` for all files without multi-level headers |

All other columns (`paper_id`, `source_file`, `filename`, `group`, `column_name`, `sample_values`, `col_type`, statistics…) are unchanged.

**`column_name` semantics** (clarified, not changed): always holds the raw variable name as it should be matched against the codebook. For resolved multi-level files this is the sub-header row value, not the original `...N` placeholder.

### `labels.csv` — unchanged

`column_name` in `labels.csv` is populated directly from `columns_df$column_name`, which is already the resolved variable name. No schema change needed.

### `structure.csv`, `bulk_summary.csv` — unchanged

---

## New Constant

| Constant | Value | Location | Purpose |
|---|---|---|---|
| `MULTILEVEL_HEADER_LOOKAHEAD` | `3L` | `0_index.R` constants block | Max rows to scan below row 1 for a usable sub-header row |
