# Tasks: LLM Temperature Stability Testing

**Input**: Design documents from `/specs/017-llm-temperature-testing/`
**Prerequisites**: plan.md ✅ spec.md ✅ research.md ✅ data-model.md ✅ contracts/ ✅

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

---

## Phase 1: Foundational (Blocking Prerequisites)

**Purpose**: Changes to `run_sweep.R` that BOTH the single-paper runner (US1) and bulk runner (US5) depend on. No user story work can begin until this phase is complete.

**⚠️ CRITICAL**: US1 and US5 both source `run_sweep.R` and call `run_paper_sweep()`. This refactor must land first.

- [X] T001 Extract `run_paper_sweep(paper_id, temperatures, repeats, sweep_dir)` from the top-level execution block in `run_sweep.R` so it is a callable function; reduce the top-level block to argument parsing + a single call to `run_paper_sweep()` (research.md R1)
- [X] T002 Add `no_data` end-state detection inside `run_one()` in `run_sweep.R`: after `run_index()` returns successfully, check whether `columns.csv` exists and has ≥1 data row; if not, set `status = "no_data"`, skip `run_codebook_label()`, and log the result — do NOT treat as `"failed"` (spec.md FR-016, research.md R6)
- [X] T003 Update `append_sweep_log()` and `load_or_create_sweep_log()` in `run_sweep.R` to accept `"no_data"` as a valid status; `"no_data"` runs MUST be treated as completed for resume purposes (skipped on re-run like `"ok"`); update `run_paper_sweep()` to track `n_no_data` and include in return value

**Checkpoint**: `run_paper_sweep()` exists and is callable; the CLI entrypoint still works (`Rscript run_sweep.R --paper-id <id> --temperatures 0,0.7 --repeats 2`); `no_data` is a valid log status.

---

## Phase 2: User Story 1 — Single Paper Temperature Sweep (Priority: P1) 🎯 MVP

**Goal**: Researcher can run a full temperature sweep on one paper and get per-(temperature, repeat) outputs plus a sweep log.

**Independent Test**: `Rscript run_sweep.R --paper-id <id> --temperatures 0.0,0.7 --repeats 2 --sweep-dir ./sweep_results` produces 4 output directories under `sweep_results/<paper_id>/temp_*/rep_*/` and a `sweep_log.csv` with 4 rows; re-running skips all 4.

- [X] T004 [US1] Verify `run_one()` in `run_sweep.R` correctly sets `options(llm_temperature = temperature)` before each pipeline run and restores state after; confirm isolation between consecutive runs at different temperatures
- [X] T005 [US1] Validate temperature CLI input in `parse_sweep_args()` in `run_sweep.R`: reject values outside `[0.0, 2.0]`, non-numeric inputs, and empty lists with a descriptive error message (spec.md FR-011)
- [ ] T006 [US1] Smoke test — happy path: run `Rscript run_sweep.R --paper-id <valid_id> --temperatures 0.0,0.7 --repeats 2 --sweep-dir ./sweep_results`; confirm 4 output dirs created, `sweep_log.csv` has 4 rows, re-running skips all 4 combinations
- [ ] T007 [US1] Smoke test — no-data path: run sweep on a paper with no data files; confirm `sweep_log.csv` records `status = "no_data"`, no error is raised, codebook stage is not attempted, and re-running skips those rows (spec.md SC-009)

**Checkpoint**: Single-paper sweep is fully functional, resumable, and handles the no-data end state gracefully.

---

## Phase 3: User Story 2 — Stability Report (Priority: P2)

**Goal**: `report_sweep.R` produces per-temperature pairwise label-agreement rates for both pipeline stages (index and codebook).

**Independent Test**: `Rscript report_sweep.R --sweep-dir ./sweep_results/<paper_id>` produces `sweep_report_*.md` with a stability table; temperature 0.0 shows 100% agreement; single-repeat temperatures show a warning.

- [X] T008 [US2] In `report_sweep.R`, implement (or verify) `compute_stability(sweep_dir, log_df)`: for each temperature load per-run `columns.csv` and compute pairwise col_type agreement; load per-run `labels.csv` and compute pairwise label agreement; return data.frame with columns `temperature`, `col_type_agreement`, `label_agreement`, `repeat_count` (data-model.md StabilityScore)
- [X] T009 [US2] In `report_sweep.R`, implement `pairwise_agreement(label_vec_a, label_vec_b)`: fraction of positions with exact string match; return NA if either vector is empty or lengths differ
- [X] T010 [US2] In `report_sweep.R`, implement `section_stability(stability_df)`: render stability table as a markdown section sorted by `col_type_agreement` descending; emit a warning line when `repeat_count < 2` for any temperature (spec.md US2 acceptance scenario 4)
- [X] T011 [US2] In `report_sweep.R`, handle `no_data` runs in stability computation: exclude rows with `status = "no_data"` from pairwise agreement inputs; do not treat absent output files for `no_data` runs as errors

**Checkpoint**: Stability report section renders correctly; 100% agreement at temperature 0.0 in a deterministic 2-repeat test.

---

## Phase 4: User Story 3 — Quality Report (Priority: P3)

**Goal**: `report_sweep.R` adds per-temperature quality proxy metrics (known-type rate, codebook coverage, non-empty label rate).

**Independent Test**: Quality section appears alongside stability section; a paper with no codebook shows N/A for coverage metrics (not 0%); a temperature where all runs are `no_data` shows N/A for all quality metrics.

- [X] T012 [US3] In `report_sweep.R`, implement (or verify) `compute_quality(sweep_dir, log_df)`: for each temperature compute mean `known_type_rate` from per-run `columns.csv`; compute mean `codebook_coverage_rate` and `nonempty_label_rate` from per-run `codebook_coverage.csv`; return NA for codebook metrics when no codebook exists (data-model.md QualityScore)
- [X] T013 [US3] In `report_sweep.R`, implement `section_quality(quality_df)`: render quality metrics table; display N/A (not 0%) for papers with no codebook (spec.md US3 acceptance scenario 3)
- [X] T014 [US3] In `report_sweep.R`, exclude `no_data` runs from quality metric averages; when ALL runs for a temperature are `no_data`, render all quality metrics as N/A for that temperature

**Checkpoint**: Quality section renders correctly; N/A vs 0% distinction verified on a paper with no codebook.

---

## Phase 5: User Story 4 — Temperature Recommendation (Priority: P4)

**Goal**: `report_sweep.R` synthesises stability and quality into a single recommended temperature with a configurable weight and score breakdown.

**Independent Test**: Full report on a sweep with ≥2 temperatures produces a recommendation section naming one temperature (or documenting a tie) with a visible score table; `--stability-weight 0.8` shifts recommendation toward the more stable temperature.

- [X] T015 [US4] In `report_sweep.R`, implement (or verify) `compute_recommendation(stability_df, quality_df, stability_weight)`: combine normalised stability and quality scores with configurable weight (default `0.5`); return list with `recommended_temperature`, `score_df`, and `is_tie` flag (spec.md FR-007)
- [X] T016 [US4] In `report_sweep.R`, implement `section_recommendation(rec)`: list tied temperatures when `is_tie = TRUE`; emit warning when only 1 temperature was tested (spec.md US4 acceptance scenarios 2–3)
- [X] T017 [US4] Wire `--stability-weight` CLI argument in `parse_report_args()` in `report_sweep.R` (default `0.5`); validate it is numeric and in `[0, 1]`
- [X] T018 [US4] Wire `compute_recommendation()` and `section_recommendation()` into `write_sweep_md_report()` in `report_sweep.R`; run end-to-end on a 2-temperature sweep and confirm the recommended temperature is the one with the higher combined score (spec.md SC-006)

**Checkpoint**: Full per-paper report (`sweep_report_*.md`) includes all four sections: overview, stability, quality, recommendation.

---

## Phase 6: User Story 5 — Bulk Temperature Sweep (Priority: P2)

**Goal**: `run_sweep_bulk.R` sweeps all papers crash-resiliently with paper-level progress logging, `n_no_data` tracking, and resume support.

**Independent Test**: Set `N_PAPERS <- 4`, `N_WORKERS <- 2`, `REPEATS <- 2`; run bulk runner; `sweep_bulk_log.csv` has 4 rows written in 2 batches; re-run confirms all 4 papers are skipped without re-processing (spec.md SC-007).

- [X] T019 [P] [US5] Create/verify `run_sweep_bulk.R` with config block: `TEMPERATURES`, `REPEATS`, `N_PAPERS`, `N_WORKERS` (default `parallel::detectCores() - 1L`), `SWEEP_DIR`, `BULK_LOG`, `SEED`; source `run_sweep.R` (which sources `0_index.R` so `XML_DIR` is available) — matching contract in `contracts/run_sweep_bulk.md`
- [X] T020 [P] [US5] In `run_sweep_bulk.R`, implement `load_bulk_log(path)`: return data.frame with `BulkSweepRecord` schema (data-model.md) if file exists, or empty data.frame with correct column types if not; always use `colClasses = c(paper_id = "character")` (spec.md FR-008, constitution Principle II)
- [X] T021 [P] [US5] In `run_sweep_bulk.R`, implement `append_bulk_log(path, row)`: append one `BulkSweepRecord` row to `BULK_LOG` immediately after `run_paper_sweep()` returns; include `n_no_data` field; write after each paper for crash resilience (constitution Principle I)
- [X] T022 [US5] In `run_sweep_bulk.R`, implement the main loop: discover paper IDs from `XML_DIR`; load `BULK_LOG`; compute `setdiff(all_ids, done_ids)`; apply `SEED` shuffle and `N_PAPERS` cap; split into batches of `N_WORKERS`; for each batch call `parallel::mclapply(batch, run_paper_sweep, TEMPERATURES, REPEATS, SWEEP_DIR, mc.cores = N_WORKERS)`; append all batch result rows to `BULK_LOG` after each batch returns (research.md R3, R4, R7)
- [X] T023 [US5] In `run_sweep_bulk.R`, extract `n_no_data` from the list returned by `run_paper_sweep()` and store in `BulkSweepRecord`; ensure `n_ok` in the log counts both `"ok"` and `"no_data"` runs (contracts/run_sweep_bulk.md)
- [ ] T024 [US5] Smoke test: set `N_PAPERS <- 4`, `N_WORKERS <- 2`, `REPEATS <- 2`; run `run_sweep_bulk.R`; confirm `sweep_bulk_log.csv` has 4 rows written across 2 batches; re-run skips all 4 papers; verify `n_no_data` populated correctly

**Checkpoint**: Bulk runner is crash-resilient, resumes correctly, and separates `no_data` from `failed` in the log.

---

## Phase 7: User Story 6 — Grand Cross-Paper Report (Priority: P3)

**Goal**: `report_sweep_grand.R` produces a flat CSV with one row per (paper × temperature × stage) for post-processing; `no_data` runs show NA metrics, not 0%.

**Independent Test**: Run on `sweep_results/` with 2 swept papers × 2 temperatures → output CSV has exactly 8 rows (2 × 2 × 2); status column contains `"ok"` or `"no_data"` but not `"failed"` for a clean sweep (spec.md SC-008).

- [X] T025 [US6] Create/verify `report_sweep_grand.R` with CLI argument parsing for `--sweep-dir` and `--out-csv`; source `report_sweep.R` (gets `compute_stability`, `compute_quality`) and `0_index.R` (gets `XML_DIR`) — matching contract in `contracts/report_sweep_grand.md`
- [X] T026 [US6] In `report_sweep_grand.R`, implement `build_stage_rows(paper_id, stability_df, quality_df)`: unpivot into two rows per temperature — `stage = "index"` (col_type_agreement + known_type_rate; codebook metrics = NA) and `stage = "codebook"` (label_agreement + coverage metrics; known_type_rate = NA) — matching `GrandReportRow` schema in data-model.md
- [X] T027 [US6] In `report_sweep_grand.R`, handle `no_data` temperatures in `build_stage_rows()`: when all runs for a temperature are `no_data`, set all metric columns to NA and `status = "no_data"` for both stage rows (not `"failed"`)
- [X] T028 [US6] In `report_sweep_grand.R`, implement the main loop: for each paper ID from `XML_DIR`, skip papers with no `sweep_log.csv` (count in summary only — do not emit rows for unswept papers); call `compute_stability()` + `compute_quality()` + `build_stage_rows()` for swept papers
- [X] T029 [US6] In `report_sweep_grand.R`, write flat CSV to `--out-csv` (overwrite if exists); print summary: total papers in `XML_DIR`, papers with sweep data, papers skipped, total rows written
- [ ] T030 [US6] Verify row-count invariant: for a complete sweep of P papers × T temperatures, output has exactly P × T × 2 rows; validate with a 2-paper × 2-temperature example (spec.md SC-008)

**Checkpoint**: Grand report CSV produced; row count matches P × T × 2 invariant; `no_data` runs show NA metrics.

---

## Phase 8: Polish & Cross-Cutting Concerns

- [X] T031 [P] Update `docs/pipeline.md` entry points table to document `run_sweep_bulk.R` and `report_sweep_grand.R` as new entry points
- [X] T032 [P] Update `progress.md` with feature 017 completion summary
- [X] T033 Verify Principle II compliance across all new/modified scripts: every `read.csv()` that loads `paper_id` uses `colClasses = c(paper_id = "character")`
- [X] T034 Verify Principle I compliance: `sweep_bulk_log.csv` written after each paper, `sweep_log.csv` after each run — never accumulated in memory only

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Foundational)**: No dependencies — start immediately
- **Phase 2 (US1)**: Requires Phase 1 complete
- **Phase 3 (US2)**: Requires Phase 1; independent of Phase 2
- **Phase 4 (US3)**: Requires Phase 3 (builds on same report script)
- **Phase 5 (US4)**: Requires Phase 4 (wired into same report)
- **Phase 6 (US5)**: Requires Phase 1; independent of Phases 2–5
- **Phase 7 (US6)**: Requires Phase 3 (sources `compute_stability`/`compute_quality` from `report_sweep.R`)
- **Phase 8 (Polish)**: Requires all phases complete

### Parallel Opportunities After Phase 1

| Track | Phases | What it delivers |
|-------|--------|-----------------|
| A | 2 (US1) | Per-paper sweep runner — MVP |
| B | 3 → 4 → 5 (US2 → US3 → US4) | Per-paper report |
| C | 6 (US5) | Bulk sweep runner |

Phase 7 (US6) starts once Track B reaches Phase 3 completion.

### Within Each Phase

- [P]-marked tasks in Phase 6 (T019, T020, T021) operate on different concerns and can be written in parallel
- T031 and T032 in Phase 8 touch different files

---

## Implementation Strategy

### MVP (Phases 1–2 only)

1. Complete Phase 1: foundational `run_sweep.R` refactor
2. Complete Phase 2: single-paper smoke tests pass
3. **STOP and VALIDATE**: `Rscript run_sweep.R --paper-id <id> --temperatures 0.0,0.7 --repeats 2` works end-to-end, including no-data path

### Incremental Delivery

1. Phase 1 + 2 → single-paper sweep operational (MVP)
2. Phase 3 + 4 + 5 → per-paper report complete
3. Phase 6 → bulk runner operational
4. Phase 7 → grand cross-paper report
5. Phase 8 → polish

---

## Notes

- `[P]` tasks operate on different files with no blocking dependencies between them
- `[USn]` label maps each task to the user story it delivers
- `no_data` is a **success** status — must never appear in `n_failed` counts
- All `read.csv()` calls loading `paper_id` require `colClasses = c(paper_id = "character")` (Principle II)
- All bulk/sweep log writes must be incremental — no in-memory accumulation (Principle I)
