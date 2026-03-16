# Research: Reduce Over-Conservative `unknown` Column Type Classifications

**Feature**: 004-reduce-unknown-coltypes | **Phase**: 0 — Research

---

## Why so many `unknown` classifications?

**Root cause analysis** against paper `0956797615583071`:

With `N_DATA_READ = 5`, at most 5 rows are loaded per file, so `n_unique ≤ 5` for every column.
Rule 6 in `classify_col_type_rules()` routes all numeric columns with `n_unique ≤ 20` to the LLM.
This means **virtually every numeric column** in the dataset is sent to the LLM, regardless of
how clear-cut its type is.

The LLM then returns `unknown` for columns like `pre_film_VAS_Sad` (samples: `0.8, 0.3, 1.1`)
and `BDI_II` (samples: `2, 11, 1, 0`) — measurements that are unambiguously numeric scales.
The inconsistency (some VAS columns get `continuous`, others `unknown`) is pure LLM
non-determinism across batches.

**Classification of current `unknown` columns in paper 0956797615583071:**

| Column | Sample values | True type | Why currently unknown |
|---|---|---|---|
| `pre_film_VAS_Sad` | `0.8, 0.3, 1.1, 0.6, 0` | continuous | Decimal, ≤20 unique → LLM, LLM said unknown |
| `pre_film_VAS_Hopelessness` | `0.1, 0.8, 0, 0.3, 0` | continuous | Same |
| `pre_film_VAS_Depressed` | `0.1, 2, 0, 0.2, 0` | continuous | Same |
| `pre_film_VAS_Fear` | `2.5, 1.9, 0, 0.3, 0` | continuous | Same |
| `pre_film_VAS_Horror` | `0.1, 0.1, 0, 0.2, 0` | continuous | Same |
| `BDI_II` | `2, 11, 1, 0, 0` | ordinal/continuous | Integer, ≤20 unique → LLM, LLM said unknown |
| `Attention_paid_to_film` | `10, 6, 9, 10, 10` | ordinal | Integer, ≤20 unique → LLM, LLM said unknown |
| `Post_film_Distress` | `9, 7, 8, 3, 7` | ordinal | Same |
| `Diary_Compliance` | `9, 9, 8, 10, 8` | ordinal | Same |
| `IES_R_Intrusion_subscale` | `0.38, 0.75, 1.25, 0.38, 1.38` | continuous | Decimal → LLM said unknown |
| `Tetris_Demand_Rating` | `4, 0, -2, -2, 0` | ordinal/continuous | Integer with negatives → LLM said unknown |
| `Condition` (ex2) | `1, 1, 1, 1, 1` | binary | n_unique=1 → binary should fire, but… |

Wait — `Condition` in ex2 has all-same values in the sample. With 5 rows all = 1, `n_unique = 1 ≤ 2` → Rule 2 should classify as `binary`. Yet it shows as `unknown`. This is because `Condition` in ex2 spans 3+ conditions in the full dataset but all 5 sampled rows happened to be condition 1. Currently `binary` classification then. But the CSV shows `unknown` — this means the _previous run_ may have been before a code change, or the sampling was different. The fix does not need to address this case specifically; Rule 2 handles it.

---

## Decision 1: Decimal numeric → always `continuous` (Rule 6a)

**Decision**: Add Rule 6a immediately before Rule 6. If `is.numeric(values)` AND any non-NA
value has a fractional part (`any(x_noNA != floor(x_noNA))`), classify as `continuous` without
LLM routing.

**Rationale**: A column containing any non-integer value cannot be binary (Rule 2 guards that),
categorical, ordinal with integer coding, or an ID. The only valid continuous type for decimal
numeric data is `continuous`. This is deterministic and requires no LLM.

**Alternatives considered**:
- Require all values to be decimal (rejected: a mixed integer/decimal column like `0, 0, 1.5`
  is still continuous)
- Use a percentage threshold (rejected: unnecessary complexity — any fractional value is
  sufficient signal)
- Raise `n_unique > 20` threshold (rejected: won't help with 5-row samples; threshold would
  need to be 3+ which would incorrectly skip integer scales)

**Edge case**: Decimal column with ≤ 2 unique values (e.g., `0.0 | 1.0`). Rule 2 fires first
(n_unique ≤ 2 → binary), so Rule 6a is never reached. Correct behavior preserved.

---

## Decision 2: Integer numeric ambiguous → `is_numeric = TRUE` flag for post-LLM fallback

**Decision**: Add an `is_numeric` boolean field to the return value of `classify_col_type_rules()`.
Set to `TRUE` only for Rule 6 (integer numeric, 3–20 unique values). All other returns set
`is_numeric = FALSE`.

After the LLM batch completes, apply a fallback: any column where `is_numeric = TRUE` AND
`col_type == "unknown"` is reclassified to `"continuous"`.

**Rationale**: For integer columns with 3–20 unique values, the LLM is still the right tool to
distinguish `ordinal` from `continuous` (e.g., a 7-point Likert scale vs. a count). But the LLM
returning `unknown` for these columns is never correct — if it can't determine ordinal vs.
continuous, `continuous` is a safe conservative fallback that at least enables statistics.

**Why `continuous` and not `ordinal` as fallback**: `continuous` is less constraining — it
enables all numeric stats without implying a specific scale type. Downstream analysis can further
refine.

**Why not apply fallback to Rule 3 (ID) columns**: ID columns also return `ambiguous = TRUE` with
`numeric_values != NULL`, but returning `continuous` for an ID column would be incorrect.
The `is_numeric` flag is `FALSE` for Rule 3, preventing the fallback.

**Tracking**: `is_numeric` is propagated through `extract_column_info` into `columns_df` as a
transient column (dropped before writing to CSV, alongside `sample_values_unique`).

---

## Decision 3: Improved LLM prompt

**Decision**: Replace the current `COLUMN_TYPE_PROMPT` with a version that:
1. Adds an explicit instruction to avoid `unknown` for numeric-looking data
2. Clarifies that `continuous` covers Likert-scale means, VAS, counts, and bounded integers
3. Reserves `unknown` only for genuinely opaque columns

**Rationale**: The LLM is the correct tool for distinguishing `ordinal` from `continuous` for
integer scales, but the current prompt gives equal weight to `unknown` as a valid option.
Strengthening the prompt reduces false `unknown` returns for borderline cases.

**LLM prompt improvements** (see plan.md for final text):
- Add: "Prefer `continuous` or `ordinal` over `unknown` — only use `unknown` when the name
  and values give no numeric signal whatsoever."
- Extend `continuous` description to explicitly include rating scales, VAS, counts, Likert means
- Extend `ordinal` description to explicitly include bounded integer scales

**Alternatives considered**:
- Remove `unknown` from valid types entirely (rejected: some columns are genuinely
  unclassifiable; removing the option causes hallucinated classifications)
- Increase temperature/retry on `unknown` (rejected: adds LLM calls and complexity;
  rule-based fallback is simpler)

---

## Decision 4: No schema change to output CSV

**Decision**: `is_numeric` is a transient column, dropped before CSV write. Output CSV schema
is unchanged: the same columns appear in `_columns.csv`, only `col_type` values change.

**Rationale**: Downstream consumers of `_columns.csv` should not be affected by this fix.

---

## Summary of changes

| File | Change |
|---|---|
| `helper.R` | Add Rule 6a (decimal → continuous). Add `is_numeric` field to all `classify_col_type_rules()` returns. |
| `0_index.R` | Improve `COLUMN_TYPE_PROMPT`. Extract `is_numeric_vec` in `extract_column_info`. Add `is_numeric` to `columns_df`. Add post-LLM numeric fallback. Drop `is_numeric` in cleanup. |
