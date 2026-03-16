# Research: LLM Fuzzy Column Matching

**Feature**: 006-llm-fuzzy-matching
**Date**: 2026-03-16

---

## Decision 1: LLM Prompt Design — Input and Output Format

**Decision**: Send a single prompt listing all unmatched column names and all unmatched codebook variable names. Ask for a JSON array of `{"column_name": "...", "codebook_variable": "..."}` pairs. Use `llm()` directly (not `llm_batch()`), since this is a single reasoning task over a set, not item-by-item classification.

**Rationale**: The LLM needs to see both sides simultaneously to reason about semantic relationships (e.g., recognising that `STAI_S_Y_PRE` maps to `STAI pre` based on domain knowledge of the STAI instrument). Sending them one at a time would lose the context needed for cross-list matching. `llm()` is already used in `parse_codebook()`'s LLM fallback for the same reason.

**Prompt structure**:
```
Data columns (unlabelled):
1. STAI_S_Y_PRE
2. STAI_S_Y_DURING
...

Codebook variables (unmatched):
1. STAI pre
2. STAI during
...

Return JSON array: [{"column_name": "...", "codebook_variable": "..."}]
Only include pairs you are confident about. If no match exists for a column, omit it.
```

**Alternatives considered**:
- `llm_batch()` per column: loses cross-list context; inefficient for small candidate sets.
- Embedding/cosine similarity: requires a new package (`text` or Python bridge); prohibited by Principle IV / no-new-packages constraint.

---

## Decision 2: Validation of LLM Responses

**Decision**: Accept a pairing only if both `column_name` and `codebook_variable` (after `normalize_varname()`) appear in the submitted candidate sets. Discard any pairing where either side is unrecognised or where the column was already labelled by rules.

**Rationale**: LLMs hallucinate. A pairing referencing a column or codebook variable not in the candidate set would silently introduce wrong labels. The validation is O(n) and cheap.

**Implementation**: Build two lookup sets before the LLM call — `norm_unlabelled_cols` and `norm_unmatched_vars`. After parsing the response, filter to rows where both normalised keys are present. Merge accepted pairings back into `labels_df` by joining on `normalize_varname(column_name)`.

**Alternatives considered**:
- Trust LLM output unconditionally: too risky given hallucination rate; ruled out.
- Fuzzy-match LLM output back to candidates: over-engineering; strict set membership is sufficient.

---

## Decision 3: Integration Point — Inside `match_column_labels()` vs Separate Function

**Decision**: Add the LLM pass as a tail section inside `match_column_labels()`. The function signature gains a `column_match_prompt` parameter (defaulting to `NULL`; when `NULL` the LLM pass is skipped). The calling script passes `COLUMN_MATCH_PROMPT` from its constants.

**Rationale**: Keeps all column matching logic in one place (Principle IV). The prompt being optional means `match_column_labels()` remains usable without a live LLM (e.g., offline testing). The parameter approach mirrors the existing pattern where `parse_codebook()` uses globals (`CODEBOOK_PARSE_PROMPT`) from the calling script — but passing it explicitly as a parameter is cleaner for a function already in `helper.R`.

**Alternatives considered**:
- Separate `llm_match_columns()` function called from `run_codebook_label()`: splits matching logic across two functions; conflicts with Principle IV.
- Always call the LLM unconditionally: wastes budget on fully-covered papers; ruled out by FR-003.

---

## Decision 4: `label_method` Values and Placement

**Decision**: Add `label_method` as the last column of `_labels.csv`. Values: `"rules"` (matched by normalization), `"llm"` (matched by LLM), `NA` (not matched). The `make_empty()` helper inside `match_column_labels()` is updated to include this column.

**Rationale**: Placing it last avoids breaking any existing column-position assumptions in downstream scripts. `NA` for unlabelled rows is consistent with the existing pattern for `label`, `codebook_variable`, and `label_source` which are also `NA` when unlabelled.

**Alternatives considered**:
- Boolean `is_llm_matched`: less informative; doesn't distinguish unlabelled from rule-matched.
- Separate `_label_provenance.csv`: unnecessary file; provenance belongs alongside the label.
