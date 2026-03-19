# Research: Verbatim Codebook Label Extraction

**Feature**: 015-verbatim-codebook-labels
**Date**: 2026-03-19

## No open unknowns

This feature has no NEEDS CLARIFICATION items and requires no external research. All decisions fall out directly from reading the existing code.

## Key finding: Root cause

**Decision**: Change only the `label` field instruction in `CODEBOOK_PARSE_PROMPT` (`2_codebook_label.R:37–47`).

**Rationale**: The current instruction `"label": "<human-readable description>"` and the rule `label: a concise human-readable description of what the variable measures` both invite the LLM to generate a description rather than extract one. Replacing these with a verbatim-copy instruction (copy exactly, do not paraphrase) is the minimal fix.

**Alternatives considered**:
- Post-processing extracted labels with a second LLM call to "de-paraphrase" — rejected as unnecessary complexity; the correct fix is at the prompt instruction level.
- Modifying `parse_codebook()` structured extraction path — rejected; that path already produces verbatim output and must not be touched.

## Scope confirmation

| Path | Change needed? | Reason |
|---|---|---|
| `CODEBOOK_PARSE_PROMPT` | ✅ Yes | Label instruction invites paraphrase |
| `COLUMN_MATCH_PROMPT` | ✗ No | Operates on already-extracted labels, does not generate new text |
| `LABEL_MERGE_PROMPT` | ✗ No | Selects a canonical from existing labels, does not generate new text |
| `parse_codebook()` structured path | ✗ No | Reads verbatim from file columns — already correct |
| Output CSV schemas | ✗ No | No column changes |
| `docs/pipeline.md` / `docs/output-schemas.md` | ✗ No | No stage, constant, or schema changes |
