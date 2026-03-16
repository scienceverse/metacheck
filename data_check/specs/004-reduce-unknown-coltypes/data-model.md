# Data Model: Reduce Over-Conservative `unknown` Column Type Classifications

**Feature**: 004-reduce-unknown-coltypes | **Phase**: 1 — Design

---

## Entities

### `col_classification` (return value of `classify_col_type_rules()`)

Internal list structure returned per column. **Adding one new field: `is_numeric`.**

| Field | Type | Description |
|---|---|---|
| `col_type` | `character` or `NA` | Classified type, or `NA` when LLM is needed |
| `ambiguous` | `logical` | `TRUE` when column should be sent to LLM |
| `numeric_values` | `numeric vector` or `NULL` | Values for stat computation; `NULL` for non-numeric |
| `n_coerced` | `integer` or `NA_integer_` | Count of values coerced to NA during normalisation |
| `is_numeric` *(new)* | `logical` | `TRUE` only when Rule 6 fired (integer numeric, 3–20 unique). Signals that an LLM `unknown` should be replaced with `continuous`. `FALSE` for all other rules. |

**`is_numeric` values by rule:**

| Rule | `is_numeric` | Reason |
|---|---|---|
| Rule 1 (all NA → empty) | `FALSE` | No numeric data |
| Rule 2 (binary) | `FALSE` | No LLM routing |
| Rule 3 (ID pattern) | `FALSE` | LLM may correctly return `id`; fallback to `continuous` would be wrong |
| Rule 4 (date) | `FALSE` | No LLM routing |
| Rule 5 (free text) | `FALSE` | No LLM routing |
| Rule 6a *(new)* (decimal → continuous) | `FALSE` | No LLM routing; directly returns `continuous` |
| Rule 6 (integer, 3–20 unique → LLM) | **`TRUE`** | LLM fallback to `continuous` is safe |
| Rule 7 (comma-decimal normalisation) | `FALSE` | No LLM routing |
| Rule 8 (categorical) | `FALSE` | No LLM routing |
| Rule 9 (text fallback) | `FALSE` | No LLM routing |

---

### `columns_df` (transient combined dataframe inside `run_index()`)

Built by `rbind`-ing per-file `extract_column_info` results. **Adding one transient column.**

| Column | Type | Persisted to CSV | Description |
|---|---|---|---|
| `paper_id` | character | ✅ | Paper identifier |
| `source_file` | character | ✅ | Relative path to source file |
| `filename` | character | ✅ | Basename of source file |
| `group` | character | ✅ | Experiment group label |
| `column_name` | character | ✅ | Column name in source data |
| `sample_values` | character | ✅ | Pipe-separated first N values |
| `col_type` | character | ✅ | Classified type (this feature improves accuracy) |
| `n_coerced` | integer | ✅ | Count of excluded values |
| `mean` … `kurtosis` | numeric | ✅ | Descriptive statistics (suppressed for non-numeric) |
| `sample_values_unique` | character | ❌ dropped | Up to 10 unique values, used for LLM descriptor; dropped before CSV write |
| `is_numeric` *(new)* | logical | ❌ dropped | Per-column `is_numeric` flag from `classify_col_type_rules()`; dropped before CSV write alongside `sample_values_unique` |

---

## State Transitions for `col_type`

```
NA (unclassified)
  │
  ├─→ classify_col_type_rules()
  │     ├─→ "empty" / "binary" / "date" / "text" / "categorical" (final, no LLM)
  │     ├─→ "continuous" (Rule 6a NEW — decimal detected, final, no LLM)
  │     ├─→ "continuous" (Rule 6 — n_unique > 20, final, no LLM)
  │     ├─→ "continuous_comma_decimal" / "continuous_outliers_excluded" (Rule 7, final)
  │     └─→ NA + ambiguous=TRUE (Rules 3, 6 integer — routed to LLM)
  │
  ├─→ LLM batch (only for ambiguous=TRUE rows)
  │     ├─→ "continuous" / "ordinal" / "categorical" / "binary" / "id" (LLM classified)
  │     └─→ "unknown" (LLM could not determine)
  │
  └─→ Post-LLM numeric fallback (NEW — for is_numeric=TRUE rows where col_type="unknown")
        └─→ "continuous"
```

## Validation Rules

- `col_type` MUST be one of `VALID_COL_TYPES`: `continuous`, `binary`, `categorical`, `ordinal`,
  `date`, `id`, `text`, `continuous_comma_decimal`, `continuous_outliers_excluded`, `empty`,
  `unknown`.
- `is_numeric` MUST only be `TRUE` when `ambiguous = TRUE` (never returned when `col_type` is
  already set by rules).
- No purely numeric column with ≥ 3 unique non-NA values should reach the final CSV with
  `col_type = "unknown"` after this feature (the fallback prevents it).
