# Research: Fix Indexing Errors for Labelled Data and Empty Column Frames

**Feature**: 019-fix-index-labelled-stats
**Date**: 2026-03-23

## Error A — `Can't convert from 'value' <labelled<double>> to <labelled<double>> due to loss of precision`

### Affected papers
`0956797618772822`, `09567976231158570`

### Code path
`pipeline/0_index.R` line 643:
```r
x_comp <- x_for_stats[!is.na(x_for_stats)]
```

`x_for_stats` originates from either `cls$numeric_values` or `df[[col]]` (line 630 for ambiguous columns). When the source file is `.sav`/`.dta`/`.sas7bdat`, `haven` reads numeric columns as `haven_labelled<double>` — a vctrs S3 class that wraps a double vector with a `labels` attribute (a named numeric vector mapping values to human-readable labels).

Subsetting a `haven_labelled` vector preserves the class. `quantile()` on a `haven_labelled` vector returns a `haven_labelled<double>` of length 1, carrying whatever subset of the labels attribute was propagated.

At line 675:
```r
stats_mat <- do.call(rbind, lapply(col_stats, as.data.frame, stringsAsFactors = FALSE))
```

`lapply(col_stats, as.data.frame)` converts each per-column list to a 1-row data.frame. Columns like `p25` and `p75` are `haven_labelled<double>` for haven-sourced columns, and `NA` (plain double) for non-numeric columns (from the early returns at lines 638–640, 647–649).

`do.call(rbind, ...)` uses vctrs internally. When combining a `haven_labelled<double>[1]` column from row N with a `double[1]` NA column from row M, vctrs applies `vec_ptype2()` to find a common type. Since `haven_labelled` carries a `labels` attribute, vctrs treats two `labelled<double>` columns with *different* label sets as incompatible — it cannot safely merge the label metadata without potentially losing label mappings — and throws: "Can't convert from `value` <labelled<double>> to <labelled<double>> due to loss of precision."

### Fix
```r
x_comp <- as.numeric(x_for_stats[!is.na(x_for_stats)])
```

`as.numeric()` on a `haven_labelled` vector strips all vctrs class information and label metadata, returning a plain `double`. The numeric values themselves are unchanged. `as.numeric()` on an already-plain `double` or `integer` is a no-op.

### Decision
- **Chosen**: `as.numeric()` coercion at line 643
- **Rationale**: One-character change; affects only the statistics computation path; preserves all numeric values; no side effects on non-haven columns
- **Alternatives considered**:
  - `haven::zap_labels()` — also strips labels but adds a haven package function call; `as.numeric()` achieves the same without naming haven explicitly
  - Wrapping the entire `col_stats` lapply in tryCatch — hides errors rather than preventing them; rejected

---

## Error B — `arguments imply differing number of rows: 0, 1`

### Affected paper
`0956797618773095`

### Code path
Two candidate rbind sites in `pipeline/0_index.R`:

**Site 1 — line 675** (stats_mat assembly):
```r
stats_mat <- do.call(rbind, lapply(col_stats, as.data.frame, stringsAsFactors = FALSE))
```
With Error A fixed this site should be safe. However, if any `col_stats` element somehow produces a 0-row frame, the `data.frame()` at line 678 that includes `stats_mat` columns alongside `names(df)` would fail: `names(df)` has length N but `stats_mat` has 0 rows.

**Site 2 — line 702** (columns_df assembly):
```r
columns_df <- do.call(rbind, lapply(column_list, `[[`, "columns"))
```
If any `extract_column_info()` result has a `columns` element that is a 0-row data.frame (possible if `df` has 0 columns after filtering, or stats_mat is NULL and the data.frame construction at line 678 yields 0 rows), rbind fails: one frame has 0 rows, another has 1+.

### Fix
Defensive `Filter()` at both sites — same pattern already used for `Filter(Negate(is.null), column_list)` at line 701.

### Decision
- **Chosen**: Filter 0-row frames before rbind at both sites
- **Rationale**: Consistent with existing Filter pattern in the same function; cheap; eliminates both the current failure and any future edge case producing 0-row frames
- **Alternatives considered**:
  - tryCatch around rbind — hides the error but doesn't produce correct output
  - Only fixing site 2 — insufficient if site 1 is the actual trigger for this paper
