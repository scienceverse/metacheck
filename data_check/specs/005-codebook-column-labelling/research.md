# Research: Codebook Column Labelling

**Feature**: 005-codebook-column-labelling
**Date**: 2026-03-16

---

## Decision 1: Codebook Parsing Strategy

**Decision**: Two-stage parsing — rule-based for structured codebooks (tables in text/markdown/CSV/Excel), LLM fallback for unstructured free-text.

**Rationale**: Many psychology codebooks are Excel files or CSVs with a "variable" column and a "label" column — these can be parsed deterministically without an LLM call, saving budget. Free-text READMEs and Word-style prose descriptions require LLM extraction. This mirrors the existing `classify_by_rules()` + `llm_batch()` two-stage pattern already used for file and column classification.

**Rule-based triggers** (no LLM needed):
- File is `.csv` / `.tsv` / `.txt` with a detectable column structure: look for a column whose header matches `(?i)(var(iable)?|name|column)` and another matching `(?i)(label|description|desc|definition|meaning)`.
- File is `.xlsx` / `.xls`: same header scan on row 1.
- File has `.sav` / `.dta`: Haven variable labels are already embedded as attributes — no parsing needed; extract directly from `haven::read_sav()` / `haven::read_dta()` label attributes.

**LLM trigger**: Any codebook file that does not match rule-based criteria above (plain README, Word-exported .txt, custom markdown tables).

**LLM prompt design**: Send up to `LLM_BATCH_SIZE = 20` lines/chunks of codebook text per call. Request JSON array: `[{"variable_name": "...", "label": "...", "experiment_context": "..."}]`. `experiment_context` is `null` if no experiment scoping is evident in the text.

**Alternatives considered**:
- LLM-only: wastes call budget on structured files that parse trivially.
- Rule-only: fails on the large proportion of READMEs that use idiosyncratic formats.

---

## Decision 2: Column Name Matching Strategy

**Decision**: Case-insensitive, whitespace-normalised exact match as the primary strategy; no fuzzy/semantic matching in this feature.

**Rationale**: The spec notes that "column names in data files and variable names in codebooks may differ in case or whitespace but are otherwise lexically similar". Semantic synonym matching (e.g., matching `reaction_time` to `rt`) is out of scope for this feature and is reserved for the standardisation stage. Simple normalisation covers the most common mismatch source (capitalisation conventions) without risk of false matches.

**Normalisation steps**:
1. `tolower()`
2. `trimws()`
3. Collapse interior whitespace: `gsub("\\s+", " ", x)`
4. Optionally strip surrounding underscores / dots (common in SPSS exports)

**Alternatives considered**:
- Edit-distance fuzzy matching: high false-positive risk at this stage; deferred to standardisation.
- Exact match only: misses trivial case differences (e.g., `SubjectID` vs `subjectid`).

---

## Decision 3: Experiment-Group Scoping

**Decision**: Use the `group` field already present in both `_structure.csv` (per file) and `_columns.csv` (per column) as the experiment scope key. Codebook variables are scoped to a group when the parsed `experiment_context` field is non-null and maps to a known group value; otherwise they are treated as applicable to all groups.

**Rationale**: The pipeline already assigns `group` (ex1, ex2, pilot1, other, na) to every file during LLM classification. This is the authoritative experiment scope. Codebook sections that mention "Experiment 1", "Study 2", etc. are mapped to the corresponding group using a small pattern table (e.g., `(?i)experiment\s*1` → `ex1`).

**Matching logic**:
- If codebook variable has no experiment context → match against columns from all groups.
- If codebook variable has experiment context `ex1` → match only against columns where `group == "ex1"`.
- If a column matches a codebook variable from multiple groups (i.e., variable has no scope) AND the variable definitions differ → flag as `conflicting_definition`, attach all candidates.

**Alternatives considered**:
- Ignore group entirely: causes cross-experiment label pollution (explicitly ruled out by FR-005).
- Require explicit group tags in codebook: too strict; most codebooks use prose headings.

---

## Decision 4: Output Schema — New Files vs Augmenting Existing

**Decision**: Two new per-paper CSV files rather than modifying `_columns.csv` in place.

- `_labels.csv` — one row per data column; carries the label result and status.
- `_codebook_coverage.csv` — one row per codebook variable; carries match status.

**Rationale**: Adding columns to `_columns.csv` would require re-running or rewriting the column extraction stage, creating a dependency between two separate pipeline stages. A join-able `_labels.csv` keeps stages independent and crash-safe (Principle I). The existing `_columns.csv` schema is preserved without breaking downstream consumers.

**Alternatives considered**:
- Modify `_columns.csv` in-place by adding label columns: breaks idempotency of the column extraction stage; complicates crash recovery.
- Single combined output: merges two orthogonal concerns (column labels vs codebook coverage) into one file; harder to query.

---

## Decision 5: LLM Call Budget for Codebook Parsing

**Decision**: Codebook parsing LLM calls share the per-paper budget of 10 calls (Principle III). A new constant `MAX_CODEBOOK_LLM_CALLS = 3` caps the codebook parsing calls specifically, leaving headroom for other stages.

**Rationale**: Most papers have 0–2 codebook files; 3 LLM calls = 60 lines/chunks of codebook text, sufficient for typical READMEs. If a codebook exceeds this, remaining text is skipped with a warning and the variables extracted so far are used. This avoids blocking the whole paper.

**Alternatives considered**:
- Separate budget (not shared): would require constitution amendment (MAJOR version bump); deferred.
- No limit: risks exhausting the file-classification budget on unusually large codebooks.

*Note: `MAX_CODEBOOK_FILE_MB` set to 100 MB (revised from initial 10 MB estimate — codebook files can be large).*
