# Contract: `col_type` Controlled Vocabulary

**Feature**: `002-column-type-classification`
**Version**: 1.0
**Date**: 2026-03-16

---

## Purpose

This document defines the contract for the `col_type` field added to `*_columns.csv` output files. It specifies the valid label set, their semantics, and the rules governing stat field population.

---

## Valid Labels

```r
VALID_COL_TYPES <- c(
  "continuous",
  "binary",
  "categorical",
  "ordinal",
  "date",
  "id",
  "text",
  "continuous_comma_decimal",
  "continuous_outliers_excluded",
  "empty",
  "unknown"
)
```

Any value not in this set written to the CSV is a pipeline bug.

---

## Statistics Population Rules

| `col_type` | `n`, `n_missing` | `mean`, `sd`, `se`, `median`, `min`, `max`, `range`, `p25`, `p75`, `iqr`, `skewness`, `kurtosis` |
|------------|-----------------|--------------------------------------------------------------------------------------------------|
| `continuous` | populated | populated |
| `continuous_comma_decimal` | populated | populated (computed on comma→period-substituted values) |
| `continuous_outliers_excluded` | populated | populated (non-parseable values coerced to NA, counted in `n_missing`) |
| `binary` | populated | NA |
| `categorical` | populated | NA |
| `ordinal` | populated | NA |
| `date` | populated | NA |
| `id` | populated | NA |
| `text` | populated | NA |
| `empty` | NA | NA |
| `unknown` | populated | NA |

---

## Classification Method Labels

| How classified | `col_type` values possible |
|---------------|---------------------------|
| Rule: all-NA | `empty` |
| Rule: 2 unique values | `binary` |
| Rule: ID name pattern + integer values | `id` |
| Rule: majority parse as dates | `date` |
| Rule: median string length > 40 | `text` |
| Rule: numeric, n_unique > 20 | `continuous` |
| Rule: char column, comma-decimal ≥ 95% | `continuous_comma_decimal` |
| Rule: char column, comma-decimal 80–95% | `continuous_outliers_excluded` |
| Rule: char column, ≤ 10 unique short values | `categorical` |
| LLM: numeric column, 3–20 unique values | any of: `continuous`, `ordinal`, `categorical`, `binary`, `id`, `unknown` |
| Fallback | `unknown` |

---

## Stability Commitment

Labels in this vocabulary are stable across pipeline runs. Downstream scripts may safely filter or pivot on `col_type`. New labels may be added in future versions; existing labels will not be renamed or removed without a version bump.
