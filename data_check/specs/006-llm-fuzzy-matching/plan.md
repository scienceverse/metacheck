# Implementation Plan: LLM Fuzzy Column Matching

**Branch**: `006-llm-fuzzy-matching` | **Date**: 2026-03-16 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/006-llm-fuzzy-matching/spec.md`

## Summary

After rule-based matching in `match_column_labels()` (005), a secondary LLM pass receives the remaining unmatched data column names and unmatched codebook variable names and proposes pairings. Accepted pairings update the labels output with `label_status = "llm"`. A new `label_method` column (`"rules"` / `"llm"` / `NA`) is added to `_labels.csv` for every row. All changes are confined to `helper.R` (`match_column_labels()`) and `2_codebook_label.R` (prompt constant), with a schema update to `docs/output-schemas.md`.

## Technical Context

**Language/Version**: R (base R only — no new packages)
**Primary Dependencies**: `llm()` from `metacheck`; `jsonlite::fromJSON`, `extract_json()` — all already present
**Storage**: Extends `structure/<paper_id>_labels.csv` with new `label_method` column; no new files
**Testing**: Manual spot-check against paper `0956797615620784` (known STAI mismatch case)
**Target Platform**: Same local R environment as 005
**Project Type**: CLI data-processing pipeline — internal function extension
**Performance Goals**: LLM call only when unlabelled columns and unmatched codebook vars both exist; zero calls for fully-covered papers
**Constraints**: Shares the per-paper LLM call budget (Principle III); one call per batch of candidates
**Scale/Scope**: Per-paper; same scope as `run_codebook_label()`

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Crash Resilience | ✅ PASS | `_labels.csv` write is unchanged; LLM failure degrades gracefully (columns stay `unlabelled`) |
| II. Paper ID Preservation | ✅ PASS | No new `read.csv()` calls introduced |
| III. Conservative Resource Limits | ✅ PASS | LLM call only fires when candidates exist; budget cap inherited |
| IV. Centralised Shared Helpers | ✅ PASS | LLM matching logic added inside `match_column_labels()` in `helper.R` |
| V. Structured Error Classification | ✅ PASS | No new fatal error codes needed; LLM failure is a warning, not a pipeline error |

## Project Structure

### Documentation (this feature)

```text
specs/006-llm-fuzzy-matching/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
helper.R               # MODIFY — extend match_column_labels() with LLM fallback pass
2_codebook_label.R     # MODIFY — add COLUMN_MATCH_PROMPT constant

docs/
└── output-schemas.md  # UPDATE — add label_method column to _labels.csv schema
```

**Structure Decision**: All matching logic lives in `match_column_labels()` per Principle IV. The LLM prompt constant follows the `COLUMN_TYPE_PROMPT` / `CODEBOOK_PARSE_PROMPT` naming pattern and is defined in `2_codebook_label.R`.

## Complexity Tracking

No constitution violations to justify.
