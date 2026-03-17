# Research: 010-fix-label-ambiguity

## Finding 1 — Root cause of `conflicting_definition` false positives

**Decision**: Over-sensitivity is caused by exact string comparison of parsed labels before flagging a conflict. Two labels with the same meaning but different wording (e.g., "Participant age" vs "Participants' age") both survive the de-duplication at `2_codebook_label.R:134-138` (which de-dups only on exact label string) and then hit the check at `helper.R:650-656`:

```r
distinct_labels <- unique(applicable$label)
if (length(distinct_labels) > 1) {
  status_out[i] <- "conflicting_definition"
```

`unique()` is string-exact; any wording difference produces a second element.

**Concrete cases observed:**
- `Age`: "Participants' age | Participant age | Participants' age" — possessive vs. non-possessive
- `StartDate`: "Time participants started the survey | Survey start time" — synonym phrase
- `EndDate`: "Time participants ended the survey | Survey end time" — synonym phrase
- `ResponseId`: "Qualtrics assigned participant ID | Qualtrics participant ID" — adjective dropped
- `Gender`: "1 = male, 2 = female | Participant gender | Participant gender (male/female) | ..." — mix of value-coding notation and semantic description

**Fix location**: `helper.R`, inside `match_column_labels()`, between collecting `distinct_labels` and setting `conflicting_definition`.

**Two-tier resolution approach:**

1. **Rule-based tier** (zero LLM cost): Normalize label strings (lowercase, collapse possessives/apostrophes, strip punctuation, collapse whitespace, strip trailing common suffixes like "s"). If all normalized labels are identical → merge; use longest original label as canonical; `label_method = "merged_rules"`.

2. **LLM tier** (1 LLM call per paper for all remaining multi-label columns): Batch all unresolved multi-label columns into a single prompt. Ask LLM: "are these labels semantically equivalent? If yes, what is the canonical label?" Each column gets back `{equivalent: bool, canonical: string}`. Equivalent → `label_status = "labelled"`, `label_method = "merged_llm"`. Not-equivalent → `conflicting_definition` as before.

**Alternatives considered:**
- Word-overlap ratio threshold: fragile, too many edge cases
- Edit-distance threshold: fails for synonym rewrites (e.g., "Survey start time" vs "Time participants started the survey")
- LLM-only (no rule tier): Correct but wastes LLM calls on trivially identical-after-normalization cases

---

## Finding 2 — BIS misplaced-label investigation

**Decision**: The `BIS_2_Nonplanning_Impulsiveness → "Nonplanning Impulsiveness (N)"` mapping is **correct per the source codebook**. This is not a code bug.

**Evidence**: The raw `traits_codebook.csv` file contains the literal row:
```
BIS_2_Nonplanning_Impulsiveness,Nonplanning Impulsiveness (N),
```
The pipeline picks this up via exact rule-based name matching. The label is the standard BIS-11 second-order Non-planning factor label. No LLM fuzzy matching is involved; `label_method = "rules"`.

**Root cause of user confusion (hypothesis)**: The same codebook file contains overlapping abbreviations — the FFM domain `neu` gets "Neuroticism (N)" and `agr` gets "Agreeableness (A)" while BIS-11 second-order factors use the same parenthetical abbreviations "(A)" and "(N)" for different constructs. This visual collision may have caused the user to question whether the BIS_2 label was accidentally swapped with an FFM label.

**Action**: No code change needed for this specific case. Clarify to user that the label reflects the source codebook accurately. If the source codebook is wrong, the data owner needs to correct the codebook file.

---

## Finding 3 — LLM budget impact

The new conflict-resolution LLM call is 1 additional call per paper that has any unresolved multi-label conflicts. This is separate from:
- `MAX_CODEBOOK_LLM_CALLS = 3` (caps codebook text parsing calls)
- The column-matching LLM call (already uncapped; 1 call per paper)

Adding 1 more uncapped call is acceptable. Papers with no conflicts incur zero extra cost. No new budget constant is needed.

---

## Finding 4 — Output schema impact

New `label_method` values:
- `"merged_rules"` — labels resolved by rule-based normalization
- `"merged_llm"` — labels resolved by LLM equivalence check

`label_status` for merged rows = `"labelled"` (existing value — no schema change there).

`docs/output-schemas.md` must be updated to document the two new `label_method` values.

---

## Finding 5 — Backward compatibility

Papers with no multi-label conflicts produce byte-identical output. The new code path is only entered when `length(distinct_labels) > 1`, which is the existing conflict condition. The fallback (no resolution) preserves the current `conflicting_definition` output.

`match_column_labels()` gains a new optional argument `label_merge_prompt = NULL`. When `NULL`, the LLM tier is skipped (same as passing no column_match_prompt today). Existing callers that don't pass the argument continue to work unchanged.
