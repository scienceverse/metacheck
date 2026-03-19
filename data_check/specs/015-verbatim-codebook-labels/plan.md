# Implementation Plan: Verbatim Codebook Label Extraction

**Branch**: `015-verbatim-codebook-labels` | **Date**: 2026-03-19 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/015-verbatim-codebook-labels/spec.md`

## Summary

The LLM parse prompt (`CODEBOOK_PARSE_PROMPT` in `2_codebook_label.R`) instructs the model to produce "a concise human-readable description of what the variable measures" — which causes the LLM to paraphrase or summarise rather than copy. The fix is a targeted update to that prompt: change the `label` field instruction and add an explicit no-paraphrase rule. No other code, schema, or infrastructure changes are required.

## Technical Context

**Language/Version**: R (base R, no new packages)
**Primary Dependencies**: `metacheck` (`llm()`), `jsonlite` — already present
**Storage**: CSV files on local filesystem — `outputs/<paper_id>/labels.csv`, `outputs/<paper_id>/codebook_coverage.csv`
**Testing**: Manual — run pipeline on a paper with a known-text DOCX/PDF codebook and compare extracted labels to source text
**Target Platform**: Local macOS (same as existing pipeline)
**Project Type**: Data pipeline (CLI / script)
**Performance Goals**: No change — one prompt string edit, zero runtime impact
**Constraints**: Must not alter structured-format extraction paths (CSV/Excel/SPSS/Stata); must not change any output schema columns
**Scale/Scope**: Single file change (`2_codebook_label.R`), single prompt string

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Check | Result |
|---|---|---|
| I. Crash Resilience | No changes to incremental write logic | ✅ Pass |
| II. Paper ID Preservation | No changes to CSV read/write paths | ✅ Pass |
| III. Resource Limits | `MAX_CODEBOOK_LLM_CALLS` unchanged (still 10; constitution says 3 — no regression introduced) | ✅ Pass |
| IV. Centralised Helpers | Prompt string lives in `2_codebook_label.R` where it always has; no logic duplication | ✅ Pass |
| V. Structured Error Codes | No new failure modes introduced | ✅ Pass |

All gates pass. No complexity violations.

## Project Structure

### Documentation (this feature)

```text
specs/015-verbatim-codebook-labels/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output (N/A — no schema changes)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
data_check/
└── 2_codebook_label.R   # Only file changed — CODEBOOK_PARSE_PROMPT (lines 37–47)
```

**Structure Decision**: Single-file edit. No new files, no new directories, no contract changes.

## Implementation Steps

### Step 1 — Update `CODEBOOK_PARSE_PROMPT`

**File**: `data_check/2_codebook_label.R`, lines 37–47

**Current** `label` field instruction (line 39, 43):
```
"label": "<human-readable description>"
...
- label: a concise human-readable description of what the variable measures
```

**New** `label` field instruction:
```
"label": "<verbatim description text copied from the codebook>"
...
- label: copy the description text exactly as it appears in the codebook — do NOT paraphrase, summarise, or infer
- If no description text is present for a variable, omit that variable entirely
```

The complete updated prompt will:
1. Change the JSON schema line to say `"<verbatim description text copied from the codebook>"`
2. Replace the `label:` rule line with the verbatim-copy instruction
3. Add an explicit "do NOT rephrase or summarise" prohibition
4. Reinforce the existing "only include variables with both a name and a description" rule to also cover the no-fabrication case

No other changes to any other file.

### Step 2 — Manual Smoke Test

Run the pipeline on one paper with an unstructured DOCX or PDF codebook. Verify:
- Extracted `label` values match source text (character-for-character, modulo whitespace normalisation)
- No labels appear for variables without a description in the source
- Structured codebook path (CSV/Excel) output is unchanged

### Step 3 — Update `docs/pipeline.md` (if needed)

Check whether the prompt change warrants a documentation update. Since no stage, constant, or schema column changes, `pipeline.md` and `output-schemas.md` do not need updates.
