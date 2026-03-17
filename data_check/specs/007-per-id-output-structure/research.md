# Research: Per-ID Output Directory Structure

**Branch**: `007-per-id-output-structure` | **Date**: 2026-03-17

No external unknowns required investigation. All decisions below are derived from the existing codebase.

---

## Decision 1: Where to put the output-path helper

**Decision**: Add `paper_output_dir(paper_id)` to `helper.R`.

**Rationale**: Constitution Principle IV requires that logic used by more than one pipeline stage lives in `helper.R`. All three pipeline scripts (`0_index.R`, `2_codebook_label.R`, and `1_data_label.R`) write to the output directory, so the path construction must be centralised.

**Alternatives considered**:
- Duplicate the path formula in each script — rejected; violates Principle IV and is the pattern that caused prior divergence bugs.
- Create a new shared constants file — rejected; overkill for a single two-line helper; `helper.R` is already sourced everywhere.

---

## Decision 2: Output directory root name

**Decision**: `./data_check/outputs/` (not `./data_check/structure/` or `./data_check/results/`).

**Rationale**: `structure/` is semantically wrong — it now holds columns, labels, and codebook coverage too, not just structure. `outputs/` is generic enough to accommodate all current and future stage outputs without renaming again.

**Alternatives considered**:
- Keep `structure/` name with per-ID subdirectories — rejected; misleading name that will confuse future contributors.
- Use `results/` — acceptable but implies finality; `outputs/` better reflects intermediate pipeline artifacts.

---

## Decision 3: Filename convention inside per-ID directories

**Decision**: Strip the `<paper_id>_` prefix from filenames. Use `structure.csv`, `columns.csv`, `labels.csv`, `codebook_coverage.csv`.

**Rationale**: The paper ID is already encoded in the directory path. Repeating it in the filename is redundant and makes filenames longer without adding information. Short names are more readable both to humans browsing the filesystem and to scripts constructing paths.

**Alternatives considered**:
- Keep `<paper_id>_structure.csv` inside per-ID directories — rejected; redundant prefix, defeats the purpose of the directory-based organisation.

---

## Decision 4: Resume logic

**Decision**: `run_index_bulk.R` resume logic remains unchanged — it skips papers already present in `bulk_summary.csv`.

**Rationale**: The `bulk_summary.csv` is the definitive record of completed runs. The spec (FR-005) anticipated checking per-ID directories for completion, but since the bulk runner already writes to `bulk_summary.csv` atomically after each paper, that signal is more reliable than directory presence (which could reflect a partial run). No change is simpler and safer.

**Alternatives considered**:
- Switch resume check to `outputs/<paper_id>/structure.csv` presence — possible but adds complexity; `bulk_summary.csv` already does this job reliably and is the canonical progress record per the constitution.

---

## Decision 5: Migration approach

**Decision**: Standalone `migrate_structure.R` script (not integrated into the pipeline runtime).

**Rationale**: The spec (FR-007, Out of Scope) explicitly prohibits automatic runtime migration. A standalone script is run once by the developer, verified, and then abandoned. Integrating migration into the pipeline would add permanent complexity for a one-time operation.

**Alternatives considered**:
- Inline migration at pipeline startup — rejected; adds permanent overhead and conditional logic for a one-time event.
- Shell script — rejected; project is R-only (constitution Technical Standards).
