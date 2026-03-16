# Research: Column Type Classification and Value Normalization

**Feature**: `002-column-type-classification`
**Date**: 2026-03-16

---

## Decision 1: Rule-based classification criteria

**Decision**: Use a layered rule cascade applied per column before any LLM call.

**Rationale**: Empirical inspection of existing `*_columns.csv` files shows the vast majority of columns are unambiguously classifiable without LLM involvement:
- `userid` columns with sequential integers → `id`
- `date` columns with ISO-8601 strings → `date`
- Binary flag columns (0/1 or two unique text values) → `binary`
- Long free-text columns (survey questions as values) → `text`
- Truly numeric columns with many unique values → `continuous`

Sending all columns to the LLM would be slow and would consume the LLM call budget needed for file classification.

**Rule cascade (in priority order)**:
1. All values NA → `empty`
2. n_unique ≤ 0 after NA removal → `empty`
3. n_unique == 1 → `binary` (degenerate; only one value, but not all-NA)
4. n_unique == 2 → `binary`
5. Column name matches ID regex (`(?i)(^|\b)(id|subj|subject|participant|pp|ppt|pid|respondent)(\b|$|[_\-]?\d)`) and values are all-integer → `id`
6. Majority of values (≥ 70%) parse as a known date format → `date`
7. Median `nchar()` of non-NA string values > 40 → `text`
8. Column is numeric in R:
   - n_unique > 20 → `continuous` (proceed to stats)
   - n_unique in 3–20 → **ambiguous** (send to LLM)
   - n_unique ≤ 2 → already caught above
9. Column is character in R:
   - Comma-decimal normalization succeeds (≥ 95% numeric after `gsub(",",".",x)`) → `continuous_comma_decimal`
   - After comma-decimal normalization, 80–95% numeric → `continuous_outliers_excluded`
   - n_unique ≤ 10 and values are short (median nchar ≤ 20) → `categorical`
   - Otherwise → `text`
10. Fallback → `unknown`

**Alternatives considered**:
- Using only LLM for all columns — rejected: too slow, consumes budget needed for file classification
- Entropy-based statistics — rejected: adds complexity without clear benefit over unique-count heuristics

---

## Decision 2: Malformed numeric handling (comma-decimal)

**Decision**: Attempt comma→period substitution on any character column that fails numeric parsing. If ≥ 95% of non-NA values become numeric, compute stats on the substituted vector.

**Rationale**: The concrete test case (`Final_Data_PDR1.csv` "Response time" column with values `2,195 | 9,198 | 11,007`) confirms this pattern exists. The comma-decimal separator is a European locale convention common in Dutch/German research data, which matches the psychology paper corpus.

**Thresholds**:
- ≥ 95% numeric after substitution → `continuous_comma_decimal` (treat remaining as NA outliers)
- 80–95% numeric → `continuous_outliers_excluded` (coerce non-numeric to NA, compute on remainder)
- < 80% numeric → do not treat as numeric; fall through to categorical/text rules

**Important distinction**: `2,195` (European decimal for 2.195) vs `2,195` (English thousands separator for 2195). Context matters but is not deterministic without domain knowledge. The `col_type` label `"continuous_comma_decimal"` surfaces this for downstream auditing.

**Alternatives considered**:
- Detecting thousands separators separately — rejected: low priority; the comma pattern is more common in this corpus
- Using `readr::locale()` — rejected: adds a dependency; the pattern is simple enough for `gsub()`

---

## Decision 3: LLM integration for ambiguous columns

**Decision**: Reuse `llm_batch()` from `helper.R` for ambiguous column type classification, with a new system prompt and a descriptor-based key column.

**Rationale**: `llm_batch()` already handles chunking, fallback on parse failure, and JSON extraction. Creating a parallel mechanism would violate Constitution Principle IV (centralized helpers).

**Descriptor format for LLM**: Each column sent to the LLM is represented as a string:
```
"<column_name>" (samples: <val1>, <val2>, ..., <val10>)
```
Using unique values (via `unique()`) provides more informative signal than head-of-file values, especially for balanced datasets.

**Expected LLM response**: JSON array with objects `{"descriptor": "...", "col_type": "..."}` where `col_type` is one of the controlled vocabulary labels.

**Batching**: Ambiguous columns from a single paper are collected and sent in one or more `llm_batch()` calls. The 10-call-per-paper limit applies across both file classification and column type classification.

**Alternatives considered**:
- Separate LLM endpoint for columns — rejected: violates Principle IV
- Fine-tuning a lightweight classifier — rejected: out of scope; local LLM is sufficient
- Skipping LLM entirely, using only rules — acceptable fallback; ambiguous columns would receive `"unknown"` which is still better than wrong continuous stats

---

## Decision 4: Where new code lives

**Decision**:
- New `classify_col_type_rules()` function → `helper.R` (rule-based fast path, shareable)
- New `COLUMN_TYPE_PROMPT` constant → `0_index.R` (prompt is pipeline-specific)
- Modifications to `extract_column_info()` → `0_index.R` (this is where column processing lives)
- LLM call for ambiguous columns → `0_index.R` after `column_list` is assembled

**Rationale**: Rule-based logic belongs in `helper.R` per Constitution Principle IV. The LLM prompt and call site belong in `0_index.R` since they are specific to this pipeline stage.

**Alternatives considered**:
- Putting all classification logic in `helper.R` — rejected: the LLM prompt is tightly coupled to the column extraction context in `0_index.R`
- Creating a new `classify_columns.R` file — rejected: Principle IV mandates `helper.R` for shared utilities; a new file adds fragmentation

---

## Decision 5: Sample values strategy for LLM

**Decision**: Sample up to 10 **unique** non-NA values per column using `unique(x[!is.na(x)])[1:min(10, n_unique)]`.

**Rationale**: The existing `sample_values` field uses `head(vals, 5)` which can repeat the same value many times for skewed columns (e.g., a binary column with mostly 0s sending `0|0|0|0|0`). Unique values give the LLM maximally distinct information about the column's range.

**This is a separate field from `sample_values`** — the existing `sample_values` column (first 5 head values) is preserved unchanged. The unique sample is only used transiently for the LLM type classification call.

---

## Decision 6: `col_type` column position in output schema

**Decision**: Insert `col_type` immediately after `sample_values` (before `n`).

**Rationale**: Grouping the descriptive/qualitative fields (`column_name`, `sample_values`, `col_type`) together before the quantitative statistics improves readability and makes the type immediately visible next to the values that informed it.

**New column order**: `paper_id, source_file, filename, group, column_name, sample_values, col_type, n, n_missing, mean, sd, se, median, min, max, range, p25, p75, iqr, skewness, kurtosis`

---

## Decision 7: LLM budget accounting

**Decision**: Column type LLM calls are counted separately from file classification calls and are subject to a separate limit (default: 5 calls = 100 columns). File classification keeps its 10-call budget.

**Rationale**: Mixing the two call types into a single shared budget would make file classification unpredictable for large papers. A separate limit per phase is cleaner and more auditable.

**Constitution alignment**: Principle III specifies "max 10 LLM calls per paper" for file classification. Column classification is a new stage (step 6b in the pipeline) and gets its own limit. Both are enforced.
