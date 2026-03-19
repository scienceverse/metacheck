# Tasks: Pipeline Quality Insights Report (016)

**Input**: Design documents from `specs/016-pipeline-quality-report/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/cli.md ✓

**Tests**: No test tasks — not requested in the feature specification.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different logical sections, no inter-dependencies)
- **[Story]**: Which user story this task belongs to (US1–US4)

---

## Phase 1: Setup

**Purpose**: Create the script skeleton so all subsequent phases have a file to write into.

- [x] T001 Create `data_check/report_quality.R` with file header comment, `main()` entry point stub, and terminal `main()` call; no logic yet

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Argument parsing and CSV-loading infrastructure required by every report section.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [x] T002 Implement `parse_args()` in `data_check/report_quality.R` — parse CLI args `--bulk` (default `./bulk_summary.csv`), `--codebook` (default `./codebook_summary.csv`), `--outputs-dir` (default `./outputs`), `--unknown-threshold` (default `30`), `--top-n` (default `10`), `--out` (optional path), `--sections` (default `"all"`); return named list
- [x] T003 [P] Implement `load_bulk(path)` in `data_check/report_quality.R` — reads CSV with `colClasses = c(paper_id = "character")`, deduplicates by keeping last occurrence per `paper_id`, emits `[WARN]` to stderr for malformed rows and duplicates; returns data frame or NULL if file missing
- [x] T004 [P] Implement `load_codebook_summary(path)` in `data_check/report_quality.R` — same pattern as `load_bulk()`: read with `paper_id` as character, deduplicate last-row-wins, warn on missing; return data frame or NULL
- [x] T005 [P] Implement `load_all_columns(outputs_dir)` in `data_check/report_quality.R` — use `list.files(outputs_dir, pattern = "columns\\.csv$", recursive = TRUE, full.names = TRUE)` to discover files; read each with `colClasses = c(paper_id = "character")`; bind with `do.call(rbind, ...)`; emit `[WARN]` listing papers with no `columns.csv`; return combined data frame or NULL
- [x] T006 [P] Implement `load_all_coverage(outputs_dir)` in `data_check/report_quality.R` — discover `codebook_coverage.csv` files under `outputs_dir`; read each with `paper_id` as character; distinguish absent file (paper marked NA in result) vs. present-but-empty file (0 rows = 0% coverage); bind; return combined data frame or NULL
- [x] T007 Implement `print_section_header(title)` formatting helper in `data_check/report_quality.R` — prints `\n=== <title> ===\n` to stdout; used by all report sections

**Checkpoint**: Foundation ready — user story sections can now be implemented independently.

---

## Phase 3: User Story 1 — Bulk Run Quality Overview (Priority: P1) 🎯 MVP

**Goal**: Print success/failure summary and timing stats from `bulk_summary.csv`.

**Independent Test**: Run `Rscript report_quality.R --sections bulk` against existing `bulk_summary.csv` and verify it prints total paper count, per-failure-type breakdown with counts and percentages, and a timing stats table for successful papers.

- [x] T008 [US1] Implement `section_bulk_overview(bulk_df, top_n)` in `data_check/report_quality.R`:
  - Total unique papers, success count, failure count, success rate
  - Failure-type breakdown: extract error-code prefix via `sub("^([a-z_]+):.*", "\\1", error)`, count and percentage per type, sorted descending by count
  - Timing stats (mean/median/max of `elapsed_ms`, `download_ms`, `llm_ms`, `column_ms`) for rows where `success == TRUE` and timing column is not NA
  - Print formatted table using `cat()` + `sprintf()` per contracts/cli.md format
- [x] T009 [US1] Wire `section_bulk_overview` into `main()` in `data_check/report_quality.R` — call when `"bulk" %in% active_sections`; pass `bulk_df` from `load_bulk()`; if `bulk_df` is NULL emit `[WARN]` and skip section

**Checkpoint**: `--sections bulk` fully functional. MVP deliverable.

---

## Phase 4: User Story 2 — Column-Type Distribution (Priority: P2)

**Goal**: Print column-type distribution across all papers and flag high-unknown-rate outliers.

**Independent Test**: Run `Rscript report_quality.R --sections coltypes` and verify the col_type table sums to 100% (±0.1%), known outlier papers appear in the flagged list, and zero-column papers are listed.

- [x] T010 [US2] Implement `section_col_type_dist(columns_df, bulk_df, unknown_threshold)` in `data_check/report_quality.R`:
  - Global count and percentage per `col_type` across all papers; treat any `col_type` value not in the 7 known types as `"other"`
  - Per-paper unknown rate = `sum(col_type == "unknown") / nrow(paper_rows) * 100`; flag papers where rate > `unknown_threshold`
  - Zero-column papers = papers in `bulk_df` where `success == TRUE` but `paper_id` absent from `columns_df` (or `columns_df` is NULL)
  - Print formatted output per contracts/cli.md format
- [x] T011 [US2] Wire `section_col_type_dist` into `main()` in `data_check/report_quality.R` — call when `"coltypes" %in% active_sections`; if `columns_df` is NULL emit `[WARN] No column data found` and skip

**Checkpoint**: `--sections coltypes` fully functional.

---

## Phase 5: User Story 3 — Codebook Coverage Summary (Priority: P3)

**Goal**: Print per-paper codebook coverage rates sorted lowest-to-highest.

**Independent Test**: Run `Rscript report_quality.R --sections codebook` and verify: coverage rates match manual count of `match_status == "matched"` rows in a spot-checked paper; papers with no `codebook_coverage.csv` show N/A (not 0%); empty files show 0%.

- [x] T012 [US3] Implement `section_codebook_coverage(coverage_df, codebook_summary_df, top_n)` in `data_check/report_quality.R`:
  - Per-paper coverage rate: `sum(match_status == "matched") / nrow(paper_rows)` from `coverage_df`; absent file = NA (label "no_codebook"), 0-row file = 0%
  - Summary: papers attempted (from `codebook_summary_df`), succeeded, no_codebook count
  - Overall mean and median coverage across papers with a codebook (non-NA)
  - List of lowest-coverage papers (bottom `top_n` by coverage rate, ascending)
  - Print per contracts/cli.md format
- [x] T013 [US3] Wire `section_codebook_coverage` into `main()` in `data_check/report_quality.R` — call when `"codebook" %in% active_sections`; if both `coverage_df` and `codebook_summary_df` are NULL emit `[WARN] codebook section skipped` and skip

**Checkpoint**: `--sections codebook` fully functional.

---

## Phase 6: User Story 4 — Timing and Performance Summary (Priority: P4)

**Goal**: Print top-N slowest papers per timing dimension.

**Independent Test**: Run `Rscript report_quality.R --sections timing --top-n 5` and verify the top-5 papers by each timing column are correctly ranked and match manual inspection of `bulk_summary.csv`.

- [x] T014 [US4] Implement `section_timing(bulk_df, top_n)` in `data_check/report_quality.R`:
  - For each of `elapsed_ms`, `download_ms`, `llm_ms`, `column_ms`: filter to rows where `success == TRUE` and column is not NA; sort descending; take top `top_n`; print table with `paper_id` and value
  - If any papers had NA for a timing column, append note: `[N papers excluded from <phase> ranking: timing not recorded]`
  - Print per contracts/cli.md format
- [x] T015 [US4] Wire `section_timing` into `main()` in `data_check/report_quality.R` — call when `"timing" %in% active_sections`; if `bulk_df` is NULL skip with warning

**Checkpoint**: `--sections timing` fully functional.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Wire all sections together, add optional CSV output, validate end-to-end.

- [x] T016 Implement `--sections` filtering logic in `main()` in `data_check/report_quality.R` — resolve `"all"` to `c("bulk", "coltypes", "codebook", "timing")`; validate each value; unknown section names emit `[WARN]` and are ignored
- [x] T017 [P] Implement optional `--out` CSV write in `data_check/report_quality.R` — after all sections run, if `args$out` is non-empty, collect all computed metrics into long-format data frame (`report_date`, `section`, `metric`, `value` all character) and write with `write.csv(..., row.names = FALSE)`
- [x] T018 [P] Audit all `read.csv()` calls in `data_check/report_quality.R` — confirm every call that reads a CSV containing `paper_id` includes `colClasses = c(paper_id = "character")`; fix any omissions
- [x] T019 Run `Rscript report_quality.R` from `data_check/`

---

## Phase 8: Clarification — Mandatory .md Output (2026-03-19)

**Trigger**: Post-implementation clarification — researcher always wants an .md report with the date of generation.

- [x] T020 Replace optional `--out` CSV logic in `data_check/report_quality.R` with mandatory auto-write of `quality_report_YYYY-MM-DD.md` to the working directory on every run; remove `--out` CLI argument; write all active sections as formatted Markdown (headings + fenced tables); verify N/A vs 0% codebook distinction is preserved
- [x] T021 Run `Rscript report_quality.R` and confirm `quality_report_YYYY-MM-DD.md` is created in `data_check/` with correct date and all four sections against the live `bulk_summary.csv` and `codebook_summary.csv`; confirm all four sections print without error and output matches contracts/cli.md format

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — start immediately
- **Foundational (Phase 2)**: Requires Phase 1 complete — blocks all user stories
- **US1–US4 (Phases 3–6)**: All depend on Phase 2; can be implemented in sequence P1→P4
- **Polish (Phase 7)**: Requires all user story phases complete

### User Story Dependencies

- **US1 (P1)**: Depends only on Phase 2 (load_bulk)
- **US2 (P2)**: Depends on Phase 2 (load_all_columns, load_bulk for zero-column check)
- **US3 (P3)**: Depends on Phase 2 (load_all_coverage, load_codebook_summary)
- **US4 (P4)**: Depends on Phase 2 (load_bulk)
- All four stories are independent of each other; their sections do not call each other

### Within Each Phase

- `[P]`-marked foundational tasks (T003–T006) can be written in parallel — they are separate functions with no cross-dependencies

---

## Parallel Opportunities

```r
# Phase 2: These four loader functions touch independent inputs — write in any order:
load_bulk()                # reads bulk_summary.csv
load_codebook_summary()    # reads codebook_summary.csv
load_all_columns()         # scans outputs/*/columns.csv
load_all_coverage()        # scans outputs/*/codebook_coverage.csv

# Phase 7: T017 and T018 touch different concerns — can be done together:
# T017: --out CSV write logic
# T018: paper_id character audit
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001)
2. Complete Phase 2: Foundational (T002–T007)
3. Complete Phase 3: US1 Bulk Overview (T008–T009)
4. **STOP and VALIDATE**: `Rscript report_quality.R --sections bulk` prints a correct summary
5. Proceed to remaining stories

### Incremental Delivery

1. Setup + Foundational → skeleton with loaders ready
2. US1 → bulk overview works → MVP
3. US2 → column distribution added
4. US3 → codebook coverage added
5. US4 → timing section added
6. Polish → full end-to-end validated, optional CSV output wired

---

## Notes

- Single file: all code lives in `data_check/report_quality.R`
- No new packages — base R only (`list.files`, `read.csv`, `do.call(rbind, ...)`, `cat`, `sprintf`, `format`)
- `paper_id` MUST be character at every read — constitution Principle II
- Missing input files are warnings, not errors — constitution Principle I spirit (graceful degradation)
- `[P]` tasks in Phase 2 are separate functions — write in any order, no shared state
