# Research: Bulk Label Runners

**Branch**: `008-bulk-label-runners` | **Date**: 2026-03-17

No external unknowns. All decisions derived from reading the existing codebase.

---

## Decision 1: What top-level code exists in `2_codebook_label.R` to remove?

**Decision**: `2_codebook_label.R` has no top-level execution code beyond function and constant definitions — `run_codebook_label()` is already a proper function. No stripping needed beyond confirming this.

**Rationale**: The file was already refactored in a prior feature. Only `1_data_label.R` has top-level execution (hardcoded `paper_id <- "09567976231220902"` and direct `mapply`/`write.csv` calls outside any function).

**Alternatives considered**: None — direct code inspection is definitive.

---

## Decision 2: Does `run_codebook_label()` already return a result list?

**Decision**: The function does not currently return an explicit structured list. The last expression is `message(...)`. A return list must be added at the end of the function body.

**Rationale**: The bulk runner needs structured fields (`n_labelled`, `n_unlabelled`, `n_no_codebook`) to populate `codebook_summary.csv`. These counts are already computed internally by `run_codebook_label()` — they just need to be returned.

**Alternatives considered**: Parse the message output — rejected; fragile and violates clean interface design.

---

## Decision 3: How does `run_label_bulk.R` discover eligible papers?

**Decision**: Scan `./data_check/outputs/` for subdirectories, filter to those where `structure.csv` exists inside. This is independent of `bulk_summary.csv`.

**Rationale**: The data-label stage's input requirement is `structure.csv`. Scanning `outputs/` directly is authoritative and requires no cross-file coordination.

**Alternatives considered**: Read `bulk_summary.csv` for successful paper IDs — rejected; couples stages unnecessarily and would miss papers processed before `bulk_summary.csv` existed.

---

## Decision 4: Resume policy — skip failures or retry them?

**Decision**: Skip both successes AND failures on resume (same policy as `run_index_bulk.R`). A paper in the summary CSV is never re-run automatically.

**Rationale**: Consistent with the existing bulk runner. Retrying failures requires manual intervention (remove the row from the summary CSV) — this is intentional so operators can investigate before retrying.

**Alternatives considered**: Retry failures on restart — rejected; could cause infinite retry loops for papers that will always fail.

---

## Decision 5: What counts to include in `codebook_summary.csv`?

**Decision**: `paper_id`, `success`, `error`, `elapsed_ms`, `n_labelled`, `n_unlabelled`, `n_no_codebook`.

**Rationale**: These three counts — labelled, unlabelled, and no_codebook — are the primary diagnostic signals for codebook coverage quality. They are already computed inside `run_codebook_label()` as `label_status` values in the output dataframe.

**Alternatives considered**: Include full label breakdown — too granular for a summary; the per-paper `labels.csv` already has full detail.
