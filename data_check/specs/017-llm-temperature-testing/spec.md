# Feature Specification: LLM Temperature Stability Testing

**Feature Branch**: `017-llm-temperature-testing`
**Created**: 2026-03-19
**Status**: Draft
**Input**: User description: "Implement a testing toolkit that runs the same paper through the pipeline multiple times at different LLM temperatures and measures output stability (label consistency across runs) and relative quality (which temperature produces more coherent or complete outputs). This is a controlled experiment to tune the LLM temperature hyperparameter and validate that the pipeline produces reproducible results."

## Clarifications

### Session 2026-03-20

- Q: What does "per section" mean in the context of stability reporting? → A: Per pipeline stage — column-type label stability (index stage) and codebook label stability (codebook labelling stage) reported separately; this is already implicit in FR-005 and now explicit.
- Q: Should multi-paper sweep be a new bulk runner alongside the existing single-paper mode, or replace it? → A: New bulk sweep runner alongside existing single-paper mode (mirrors `run_0_index_bulk.R` / `0_index.R` pattern).
- Q: How is "all papers" defined for the bulk sweep? → A: All paper IDs discovered from XML files in `XML_DIR` (`./data-raw/psychsci/grobid_0.8.2`), same discovery logic as `run_0_index_bulk.R`.
- Q: What level should the grand report aggregate to? → A: One row per paper × temperature × pipeline stage — most granular, no aggregation; post-processing friendly.
- Q: Should the bulk sweep include a cap parameter for subsetting? → A: Yes — `N_PAPERS <- Inf` cap (same pattern as bulk runner); bulk runner MUST also include paper-level caching/logging so progress can be paused and resumed.
- Q: What should happen when a sweep run finds no data files for a paper? → A: This is a valid terminal end state (`no_data`), not a failure. The codebook stage is skipped. The run is logged as `status = no_data` and counts as a completed run (not retried, not excluded from the sweep log).
- Q: How should the bulk sweep runner parallelise across papers? → A: `parallel::mclapply` (fork-based, ships with base R, no new packages). Papers processed in batches of `N_WORKERS`; parent appends batch results to `sweep_bulk_log.csv` after each batch completes (batch-level crash resilience).

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Run Temperature Sweep on a Single Paper (Priority: P1)

The researcher selects one paper and runs the single-paper sweep tool (`run_sweep.R`), specifying a list of temperatures (e.g. 0.0, 0.3, 0.7, 1.0) and a number of repeat runs per temperature (e.g. 3 repeats). The tool runs the full pipeline for each (temperature, repeat) combination and saves the outputs to an isolated location, then reports which temperature produced the most consistent and most complete results.

**Why this priority**: This is the core capability — without the ability to run the sweep and collect outputs, nothing else in this feature is possible.

**Independent Test**: Can be fully tested on a single paper with a small temperature list (e.g. 2 temperatures × 2 repeats = 4 runs) and confirms that separate output files are produced for each run.

**Acceptance Scenarios**:

1. **Given** a valid paper ID and a list of temperatures, **When** the sweep is run, **Then** each (temperature, repeat) combination produces a separate set of output files stored under an identifiable path.
2. **Given** N temperatures × R repeats, **When** the sweep completes, **Then** exactly N × R output sets exist (or failures are recorded for any that did not complete).
3. **Given** a temperature of 0.0 (deterministic), **When** run R times, **Then** all R outputs for that temperature are identical (column labels match 100%).

---

### User Story 2 - Stability Report Across Temperatures (Priority: P2)

After running the sweep, the researcher wants to see a stability report: for each temperature, how often do repeated runs produce the same column-type classifications and the same codebook labels? The tool computes pairwise agreement across repeats and reports the mean agreement rate per temperature.

**Why this priority**: Stability (reproducibility) is the primary quality signal when there is no ground truth. A temperature with high agreement is preferable to one with high variance.

**Independent Test**: Can be tested independently by pointing the report at an existing sweep output directory and verifying agreement rates are computed correctly for each temperature.

**Acceptance Scenarios**:

1. **Given** a completed sweep directory, **When** the stability report is run, **Then** it outputs a table showing mean pairwise label-agreement rate per temperature, sorted from most to least stable.
2. **Given** a temperature where all repeats produced identical outputs, **When** the report is run, **Then** that temperature shows 100% agreement.
3. **Given** a temperature where repeats differed on every column, **When** the report is run, **Then** that temperature shows 0% agreement.
4. **Given** only 1 repeat per temperature, **When** the report is run, **Then** it warns that stability cannot be computed with a single repeat and skips stability metrics.

---

### User Story 3 - Quality Comparison Across Temperatures (Priority: P3)

The researcher wants to compare relative output quality across temperatures, without ground truth. Proxy quality metrics include: proportion of columns that received a non-`unknown` type, codebook coverage rate (fraction of columns with a label), and the number of labels that are non-empty. The report shows these metrics per temperature to help identify which temperature produces the most complete outputs.

**Why this priority**: Stability alone is insufficient — a temperature could be stably wrong. Quality proxies help distinguish a temperature that is consistently complete from one that is consistently empty.

**Independent Test**: Can be tested independently by pointing the quality report at a sweep output directory and confirming the proxy metrics are computed per temperature.

**Acceptance Scenarios**:

1. **Given** a completed sweep directory, **When** the quality report is run, **Then** it shows per-temperature mean values for: known-type rate, codebook coverage rate, and non-empty label rate.
2. **Given** a temperature that consistently produces all-`unknown` types, **When** the report is run, **Then** that temperature shows 0% known-type rate.
3. **Given** a paper with no codebook, **When** the report is run, **Then** codebook coverage is reported as N/A (not 0%) to distinguish "no codebook" from "poor matching".

---

### User Story 4 - Recommended Temperature Output (Priority: P4)

After the sweep, the researcher wants a single recommended temperature — the one that best balances stability and quality. The tool computes a combined score (weighted stability + quality proxies) and prints a recommendation with the score breakdown.

**Why this priority**: Synthesising the results into a single actionable recommendation reduces cognitive load. The researcher should not need to interpret a multi-column table to decide which temperature to use.

**Independent Test**: Can be tested by verifying the recommended temperature is the one with the highest combined score in a known sweep result.

**Acceptance Scenarios**:

1. **Given** a completed sweep with stability and quality data, **When** the recommendation is requested, **Then** it names the single temperature with the highest combined score and shows the score breakdown.
2. **Given** a tie between two temperatures, **When** the recommendation is requested, **Then** both are listed as tied and the researcher is prompted to choose based on the detailed report.
3. **Given** insufficient data (e.g. only 1 temperature tested), **When** the recommendation is requested, **Then** it states that comparison requires at least 2 temperatures.

---

### User Story 5 - Bulk Temperature Sweep Across All Papers (Priority: P2)

The researcher runs the bulk sweep tool (`run_sweep_bulk.R`), which discovers all paper IDs from XML files in `XML_DIR` (`./data-raw/psychsci/grobid_0.8.2`) and runs the temperature sweep for each paper in parallel. The bulk runner processes papers in batches of `N_WORKERS` using fork-based parallelism (`parallel::mclapply`), appends batch results to a log CSV after each batch, and resumes by skipping already-logged papers on restart.

**Why this priority**: A single-paper sweep only provides anecdotal evidence about temperature behaviour. A cross-paper sweep is required for a statistically meaningful comparison of stability and quality across temperatures. Parallelism makes this practical at scale.

**Independent Test**: Set `N_PAPERS <- 4` and `N_WORKERS <- 2`; confirm 4 papers are swept in 2 batches of 2, results logged after each batch, and re-running skips all 4.

**Acceptance Scenarios**:

1. **Given** `N_PAPERS <- Inf` and `N_WORKERS <- 4`, **When** the bulk sweep runs, **Then** up to 4 papers are processed concurrently and all papers found in `XML_DIR` are eventually swept.
2. **Given** a partial run interrupted mid-batch, **When** the bulk sweep is re-run, **Then** completed batches are skipped and the sweep resumes from the next unprocessed paper.
3. **Given** `N_PAPERS <- 3`, **When** the bulk sweep runs, **Then** at most 3 papers are processed regardless of how many are available.
4. **Given** a paper that fails at all temperatures, **When** the bulk sweep runs, **Then** the failure is logged and the remaining papers in the batch complete normally.

---

### User Story 6 - Grand Stability/Quality Report Across All Papers (Priority: P3)

After the bulk sweep, the researcher runs the grand report tool to produce a flat CSV with one row per (paper_id × temperature × pipeline stage), containing stability and quality metrics. This granular output enables flexible post-processing (aggregation, filtering, plotting) outside of R.

**Why this priority**: The most granular representation avoids premature aggregation — researchers can always aggregate, but cannot recover detail from a pre-aggregated summary.

**Independent Test**: Can be tested by pointing the report at a `sweep_results/` directory containing 2 papers and verifying the output has exactly `2 × T × 2` rows (T temperatures, 2 pipeline stages).

**Acceptance Scenarios**:

1. **Given** a `sweep_results/` directory with P papers swept at T temperatures, **When** the grand report is run, **Then** the output CSV contains P × T × 2 rows (one per paper × temperature × pipeline stage).
2. **Given** a paper with no codebook, **When** the grand report is run, **Then** codebook-stage rows for that paper show N/A for coverage metrics rather than 0%.
3. **Given** a paper that failed at a specific temperature, **When** the grand report is run, **Then** that combination is included as a row with status = `failed` and metric columns set to NA.

---

### Edge Cases

- What happens if the pipeline fails for a (temperature, repeat) combination? → Record the failure in the sweep log; do not abort the remaining combinations.
- What happens if the paper has no data files at all? → This is a valid terminal end state (`no_data`): the run completes without error, the codebook stage is skipped entirely, and `status = no_data` is recorded in the sweep log. It is not counted as a failure and is not retried.
- What happens if the paper has no codebook? → Codebook coverage metrics are marked as N/A for all temperatures; stability is computed on column-type labels only.
- What happens if a paper is too large (`too_large` error) at any temperature? → Report the failure and exclude the paper from quality/stability metrics.
- What happens if temperatures are provided out of range (e.g. negative or >2)? → Validate inputs before starting; reject invalid values with a clear error message.
- What happens if the sweep is interrupted mid-run? → Already-completed (temperature, repeat) combinations are preserved; the sweep can be resumed by re-running and skipping existing outputs.
- What happens if the bulk sweep is interrupted mid-batch? → Completed batches are safe (already written to `sweep_bulk_log.csv`); the in-progress batch is lost and will be re-processed on restart. Per-run results within a paper are protected by the per-paper `sweep_log.csv`.
- What happens if `N_WORKERS > 1` and two workers attempt to write to the same per-paper `sweep_log.csv`? → This cannot occur: each paper has a unique directory; no two workers are ever assigned the same paper.
- What happens if `XML_DIR` contains a paper ID that has no `outputs/` directory (never processed by the main pipeline)? → The sweep runs the full pipeline from scratch for that paper; it is not a precondition that the paper has been previously indexed.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The single-paper sweep tool (`run_sweep.R`) MUST accept a paper ID, a list of temperatures, and a repeat count as inputs.
- **FR-002**: For each (temperature, repeat) combination, the sweep tool MUST run the full pipeline (index + codebook labelling) and save all outputs to a distinct, identifiable location under a sweep results directory.
- **FR-003**: The sweep tool MUST record metadata for each run: temperature, repeat number, paper ID, run timestamp, success/failure status, and elapsed time.
- **FR-004**: The sweep tool MUST skip already-completed (temperature, repeat) combinations when re-run, enabling resume after interruption.
- **FR-005**: The stability report MUST compute pairwise label-agreement rate across repeats for each temperature, separately for: (a) column-type labels (index pipeline stage) and (b) codebook labels (codebook labelling pipeline stage).
- **FR-006**: The quality report MUST compute per-temperature mean values for: known-type rate (non-`unknown` columns / total columns), codebook coverage rate, and non-empty codebook label rate.
- **FR-007**: The recommendation MUST combine stability and quality proxy scores into a single ranked output; the weighting between stability and quality MUST be configurable (default: equal weight).
- **FR-008**: The sweep MUST treat `paper_id` as a character string at all times.
- **FR-009**: Individual run failures MUST be logged and not abort the remaining sweep combinations.
- **FR-010**: All sweep outputs MUST be stored in a dedicated directory (e.g. `sweep_results/<paper_id>/`) separate from the main `outputs/` directory to avoid contaminating production outputs.
- **FR-011**: The tool MUST validate temperature inputs (must be numeric, within an accepted range) before starting and reject invalid values with a descriptive error.
- **FR-012**: A bulk sweep runner (`run_sweep_bulk.R`) MUST discover paper IDs from XML files in `XML_DIR` (`./data-raw/psychsci/grobid_0.8.2`), the same source as `run_0_index_bulk.R`.
- **FR-013**: The bulk sweep runner MUST support an `N_PAPERS <- Inf` cap parameter to limit the number of papers processed (integer to cap, `Inf` for all).
- **FR-014**: The bulk sweep runner MUST write paper-level progress rows to `sweep_bulk_log.csv` after each parallel batch completes, and skip already-logged papers on restart (batch-level crash resilience).
- **FR-017**: The bulk sweep runner MUST support a configurable `N_WORKERS` integer parameter controlling the number of parallel workers (`parallel::mclapply`, fork-based, ships with base R). Papers are processed in batches of `N_WORKERS`; crash resilience is at the batch boundary (a mid-batch crash loses that batch's results but not prior batches).
- **FR-015**: The grand report MUST produce a flat CSV with one row per (paper_id × temperature × pipeline stage); columns MUST include: `paper_id`, `temperature`, `stage` (`index` or `codebook`), `repeat_count`, `mean_pairwise_agreement`, `known_type_rate`, `codebook_coverage_rate`, `nonempty_label_rate`, `status`.
- **FR-016**: When the index stage finds no data files for a paper, the pipeline MUST treat this as a valid terminal end state (`status = no_data`) rather than an error or failure. The codebook stage MUST be skipped, and the result MUST be logged as a completed run in the sweep log without triggering any retry or abort behaviour.

### Key Entities

- **SweepRun**: One record per (paper_id, temperature, repeat); contains metadata (timestamp, status, elapsed time) and a pointer to the output directory for that run.
- **LabelSet**: The set of column-type and codebook labels produced by one pipeline run; the unit of comparison for stability and quality metrics.
- **PipelineStage**: One of `index` (column-type classification) or `codebook` (codebook label matching); stability is computed separately per stage.
- **StabilityScore**: Per (paper_id, temperature, pipeline stage) mean pairwise agreement rate across repeats.
- **QualityScore**: Per (paper_id, temperature, pipeline stage) mean values for known-type rate, codebook coverage rate, and non-empty label rate.
- **SweepRecommendation**: The combined score and recommended temperature, with breakdown by stability and quality components.
- **GrandReport**: Flat CSV with one row per (paper_id × temperature × pipeline stage); the canonical output of the bulk sweep reporting tool.
- **NoDataEndState**: A valid terminal outcome when a paper's repository contains no files classified as `data`. Distinct from a failure: no error is raised, no codebook stage is attempted, and the run is recorded as `status = no_data` in sweep logs.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A sweep of 1 paper × 4 temperatures × 3 repeats (12 runs) completes without manual intervention and produces 12 distinct output sets.
- **SC-002**: At temperature = 0.0, all repeats for a given paper produce identical column-type label sets (100% pairwise agreement).
- **SC-003**: The stability report correctly ranks temperatures by reproducibility — confirmed by manually inspecting 2 temperatures and verifying the higher-agreement one ranks first.
- **SC-004**: The sweep correctly resumes after interruption — re-running a partially complete sweep adds only the missing combinations without re-running completed ones.
- **SC-005**: Quality proxy metrics (known-type rate, codebook coverage) match values computed manually from the same output files.
- **SC-006**: The recommendation output names a single temperature (or a documented tie) and provides a score breakdown that the researcher can verify by hand.
- **SC-007**: The bulk sweep runner (`run_sweep_bulk.R`) with `N_PAPERS <- 4` and `N_WORKERS <- 2` processes exactly 4 papers in 2 batches of 2 and writes a paper-level log; re-running skips all 4 papers.
- **SC-008**: The grand report CSV for a bulk sweep of P papers × T temperatures contains exactly P × T × 2 data rows (2 pipeline stages), with `status = failed` rows for any failed combinations.
- **SC-009**: When a paper with no data files is included in a sweep, the run completes without error, the sweep log records `status = no_data`, and the bulk runner continues to the next paper without any manual intervention.

## Assumptions

- The pipeline's LLM temperature can be set per-run via an existing or new parameter; if the current pipeline hardcodes temperature, a thin wrapper or parameter pass-through will be needed.
- No new R packages are required; base R plus packages already present are sufficient.
- A "repeat" means a fully independent pipeline run (not just re-reading cached outputs); the pipeline does not cache LLM responses between runs.
- Sweep outputs are stored locally on disk and are not automatically cleaned up.
- The recommended default temperature list is [0.0, 0.3, 0.7, 1.0] and default repeat count is 3, but both are overridable.
- Pairwise agreement is computed as the fraction of columns where both runs assigned the same label (exact string match).
- "Pipeline stage" refers to one of two named stages: `index` (column-type classification from `0_index.R`) and `codebook` (label matching from `2_codebook_label.R`); these are the only two stages tracked in stability and quality metrics.
- The bulk sweep runner discovers papers from `XML_DIR` (`./data-raw/psychsci/grobid_0.8.2`) — the same constant used by `run_0_index_bulk.R` and `0_index.R`.
- The grand report is a flat CSV; no aggregation is applied — consumers are expected to perform aggregation in post-processing.
- The bulk sweep paper-level log CSV (e.g. `sweep_bulk_log.csv`) records one row per completed paper (regardless of per-run failures within that paper) and is the sole source of truth for resume logic.
- A paper with no data files is a common and valid real-world scenario (e.g. repos containing only code or supplemental materials). It must not degrade sweep reliability or pollute quality/stability metrics with false zeros.
