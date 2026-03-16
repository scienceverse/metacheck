# Data Model: Column Type Classification

**Feature**: `002-column-type-classification`
**Date**: 2026-03-16

---

## Extended Column Record Schema

The `*_columns.csv` file gains one new column: `col_type`, inserted after `sample_values`.

### Full Column Order (after feature)

| # | Column | Type | Always Present | Notes |
|---|--------|------|---------------|-------|
| 1 | `paper_id` | character | yes | Leading-zero-safe; stored as string |
| 2 | `source_file` | character | yes | Relative path within downloaded repo |
| 3 | `filename` | character | yes | `basename(source_file)` |
| 4 | `group` | character | yes | `ex1`, `ex2`, `pilot1`, `other`, `na` |
| 5 | `column_name` | character | yes | Raw column header from the data file |
| 6 | `sample_values` | character | yes | First 5 non-NA values joined by ` \| ` |
| 7 | `col_type` | character | **NEW** | Controlled vocabulary label (see below) |
| 8 | `n` | integer | conditional | Count of non-NA values; NA for `empty` |
| 9 | `n_missing` | integer | conditional | Count of NA values; NA for `empty` |
| 10 | `mean` | numeric | conditional | Only for numeric `col_type` values |
| 11 | `sd` | numeric | conditional | Only for numeric `col_type` values |
| 12 | `se` | numeric | conditional | Only for numeric `col_type` values |
| 13 | `median` | numeric | conditional | Only for numeric `col_type` values |
| 14 | `min` | numeric | conditional | Only for numeric `col_type` values |
| 15 | `max` | numeric | conditional | Only for numeric `col_type` values |
| 16 | `range` | numeric | conditional | Only for numeric `col_type` values |
| 17 | `p25` | numeric | conditional | Only for numeric `col_type` values |
| 18 | `p75` | numeric | conditional | Only for numeric `col_type` values |
| 19 | `iqr` | numeric | conditional | Only for numeric `col_type` values |
| 20 | `skewness` | numeric | conditional | Only for numeric `col_type` values |
| 21 | `kurtosis` | numeric | conditional | Only for numeric `col_type` values |

### "Numeric col_type values" definition

Statistics (columns 10–21) are populated for: `continuous`, `continuous_comma_decimal`, `continuous_outliers_excluded`.

Statistics are NA for: `binary`, `categorical`, `ordinal`, `date`, `id`, `text`, `empty`, `unknown`.

Counts (columns 8–9) are populated for all types except `empty`.

---

## `col_type` Controlled Vocabulary

| Label | Meaning | Example columns |
|-------|---------|----------------|
| `continuous` | Numeric measurement; many unique values | `age`, `response_time`, `score` |
| `binary` | Exactly two unique non-NA values | `excluded` (0/1), `gender` (Male/Female) |
| `categorical` | Unordered group with few distinct text/integer codes | `condition`, `language`, `education` |
| `ordinal` | Ordered scale with few levels | Likert items, ranked preferences |
| `date` | Date or datetime values | `date`, `timestamp`, `session_date` |
| `id` | Row or participant identifier | `userid`, `participant_id`, `subj` |
| `text` | Free text; long strings | Survey open-ended responses |
| `continuous_comma_decimal` | Character column normalized via comma→period substitution | European locale decimal fields |
| `continuous_outliers_excluded` | Mostly numeric column; small fraction of non-parseable values coerced to NA | Mixed-format numeric fields |
| `empty` | All values are NA | Placeholder / unused columns |
| `unknown` | Cannot be classified by rules or LLM | Catch-all fallback |

---

## Classification Source

| Determined by | `col_type` labels |
|---------------|-----------------|
| Rule-based fast path | `empty`, `binary`, `id`, `date`, `text`, `continuous`, `continuous_comma_decimal`, `continuous_outliers_excluded`, `categorical` |
| LLM batch | `continuous`, `ordinal`, `categorical`, `binary`, `id`, `unknown` (for ambiguous numeric columns with 3–20 unique values) |
| Fallback | `unknown` |

---

## Backward Compatibility

- Columns 1–6 and 8–21 are unchanged in position, name, and semantics.
- `col_type` (column 7) is a **new additive field**.
- Existing `*_columns.csv` files in `structure/` do NOT have this column; only newly generated files will include it.
- Downstream scripts reading existing files must handle the optional presence of `col_type`.
