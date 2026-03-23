# data_check Restructure Plan

## Current State: What's Messy

The `data_check/` root currently holds 13 R scripts, 2 bulk summary CSVs, scratch outputs (`0_result.txt`, `2_result.txt`), a generated report (`quality_report_2026-03-19.md`), and a stale `_old/` archive — all mixed together with source-controlled files. There's no grouping by purpose.

```
data_check/
├── 0_index.R                    # pipeline core
├── 2_codebook_label.R           # pipeline core
├── helper.R                     # pipeline core
├── run_single.R                 # runner
├── run_0_index_bulk.R           # runner
├── run_2_codebook_bulk.R        # runner
├── run_sweep.R                  # runner (sweep)
├── run_sweep_bulk.R             # runner (sweep)
├── download_all_osf.R           # runner (download)
├── report_quality.R             # report
├── report_sweep.R               # report
├── report_sweep_grand.R         # report
├── descriptive_statistics.R     # scratch/exploratory (broken deps)
├── bulk_summary.csv             # ← generated artifact at root
├── codebook_summary.csv         # ← generated artifact at root
├── 0_result.txt                 # ← scratch output
├── 2_result.txt                 # ← scratch output
├── quality_report_2026-03-19.md # ← generated report at root
├── outputs/                     # per-paper pipeline outputs
├── sweep_results/               # temperature sweep outputs
├── data/                        # downloaded OSF data
├── docs/
├── specs/
├── _old/
└── ...
```

---

## Proposed Structure

```
data_check/
├── pipeline/                    # Core pipeline modules (sourced by runners)
│   ├── helper.R
│   ├── 0_index.R
│   └── 2_codebook_label.R
│
├── runners/                     # Entry-point scripts (run from project root)
│   ├── run_single.R             # smoke test: one paper, full pipeline
│   ├── run_0_index_bulk.R       # bulk: file classification across all papers
│   ├── run_2_codebook_bulk.R    # bulk: codebook labelling across all papers
│   ├── run_sweep.R              # sweep: one paper × N temps × N repeats
│   ├── run_sweep_bulk.R         # sweep: all papers
│   └── download_all_osf.R       # standalone OSF pre-downloader
│
├── reports/                     # Report-generator scripts
│   ├── report_quality.R         # pipeline quality insights
│   ├── report_sweep.R           # per-paper sweep stability/quality
│   └── report_sweep_grand.R     # cross-paper sweep grand CSV
│
├── outputs/                     # Per-paper pipeline outputs (unchanged layout)
│   └── <paper_id>/
│       ├── structure.csv
│       ├── columns.csv
│       ├── labels.csv
│       └── codebook_coverage.csv
│
├── sweep_results/               # Temperature sweep outputs (unchanged layout)
│   ├── <paper_id>/
│   │   ├── temp_<T>/rep_<R>/   # isolated per-run outputs
│   │   └── sweep_log.csv
│   └── sweep_bulk_log.csv
│
├── data/                        # Downloaded OSF data (unchanged)
│   └── <paper_id>/
│
├── results/                     # Generated summary CSVs and reports
│   ├── bulk_summary.csv         # moved from root
│   ├── codebook_summary.csv     # moved from root
│   └── *.md                     # quality/sweep reports go here
│
├── docs/                        # Documentation (unchanged)
├── specs/                       # Feature specs (unchanged)
├── CLAUDE.md
├── progress.md
└── TODO.txt
```

---

## Files to Delete / Archive

| File | Action | Reason |
|------|--------|--------|
| `0_result.txt` | Delete | Scratch output, empty |
| `2_result.txt` | Delete | Scratch output, not checked in |
| `quality_report_2026-03-19.md` | Delete | Generated artifact; reproducible by running `report_quality.R` |
| `descriptive_statistics.R` | Move to `_old/` | Broken external dependencies (`load_csv_as_tibble()` etc. missing); exploratory only |
| `_old/` | Review then delete or keep archived | Already archived; confirm contents are truly obsolete |

---

## Path Changes Required by Script

All scripts currently use paths relative to `data_check/` (working directory when run). Moving scripts into subdirectories means two things change: `source()` calls and any path constants that were relative to the old location. Scripts should continue to be **run from the `data_check/` root**.

### `pipeline/helper.R`
- No `source()` calls; no output path constants.
- **No path changes needed.**

### `pipeline/0_index.R`
- `source("helper.R")` → `source("pipeline/helper.R")`
- Constants `DATA_DIR`, `OUTPUT_DIR`, `XML_DIR` are already absolute-ish (`"./data_check/data"` etc.) — verify these resolve correctly from `data_check/` root. *(No change if run from root.)*

### `pipeline/2_codebook_label.R`
- `source("helper.R")` → `source("pipeline/helper.R")`
- `OUTPUT_DIR = "./data_check/outputs"` — verify from root. *(No change if run from root.)*

### `runners/run_single.R`
- `source("0_index.R")` → `source("pipeline/0_index.R")`
- `source("2_codebook_label.R")` → `source("pipeline/2_codebook_label.R")`

### `runners/run_0_index_bulk.R`
- `source("0_index.R")` → `source("pipeline/0_index.R")`
- `SUMMARY_CSV = "./data_check/bulk_summary.csv"` → `"./results/bulk_summary.csv"`

### `runners/run_2_codebook_bulk.R`
- `source("2_codebook_label.R")` → `source("pipeline/2_codebook_label.R")`
- `SUMMARY_CSV = "./data_check/codebook_summary.csv"` → `"./results/codebook_summary.csv"`

### `runners/run_sweep.R`
- `source("0_index.R")` → `source("pipeline/0_index.R")`
- `source("2_codebook_label.R")` → `source("pipeline/2_codebook_label.R")`
- Default `--sweep-dir ./sweep_results` — no change (relative to run root).

### `runners/run_sweep_bulk.R`
- `source("run_sweep.R")` → `source("runners/run_sweep.R")`
  *(or extract shared functions into `pipeline/` instead of sourcing the whole runner)*
- `SWEEP_DIR = "./data_check/sweep_results"` — verify from root.
- `BULK_LOG = "./data_check/sweep_results/sweep_bulk_log.csv"` — verify from root.

### `runners/download_all_osf.R`
- No `source()` calls (uses metacheck directly).
- `PROGRESS_CSV = "./data_check/download_progress.csv"` — verify from root.

### `reports/report_quality.R`
- No `source()` calls.
- Default `--bulk ./bulk_summary.csv` → `--bulk ./results/bulk_summary.csv`
- Default `--codebook ./codebook_summary.csv` → `--codebook ./results/codebook_summary.csv`
- Default `--outputs-dir ./outputs` — no change.
- Report written to working directory → update to write to `./results/`.

### `reports/report_sweep.R`
- No `source()` calls.
- `source("run_sweep.R")` (if it sources shared compute functions) → `source("runners/run_sweep.R")` or extract helpers.
- Report written to `<sweep_dir>/` — no change needed.

### `reports/report_sweep_grand.R`
- `source("report_sweep.R")` → `source("reports/report_sweep.R")`
- `--sweep-dir ./data_check/sweep_results` → `./sweep_results` (verify).
- `--out-csv ./data_check/sweep_grand_report.csv` → `./results/sweep_grand_report.csv`.

---

## Migration Steps (in order)

1. **Create new directories**: `pipeline/`, `runners/`, `reports/`, `results/`
2. **Move files** (listed above into their new directories)
3. **Update `source()` calls** in each script (see table above)
4. **Update output path constants** in bulk runners and report scripts (bulk_summary, codebook_summary, sweep_grand_report → `./results/`)
5. **Delete scratch files**: `0_result.txt`, `2_result.txt`, `quality_report_2026-03-19.md`
6. **Archive or delete** `descriptive_statistics.R` and `_old/`
7. **Move existing CSVs** at root: `bulk_summary.csv`, `codebook_summary.csv` → `results/`
8. **Run smoke test** via `runners/run_single.R` on one paper to verify path resolution end-to-end
9. **Update `CLAUDE.md`** to reflect new structure
10. **Update `docs/pipeline.md`** to reflect new script locations

---

## Notes

- All scripts should continue to be invoked from the `data_check/` root (e.g. `Rscript runners/run_sweep.R --paper-id ...`). No `setwd()` changes needed.
- `sweep_results/` and `outputs/` stay at root — they are data directories, not source directories.
- The `results/` directory for summary CSVs is gitignored (generated artifacts).
- `run_sweep_bulk.R` sources `run_sweep.R` for shared functions. Consider refactoring: extract `run_one()` and `run_paper_sweep()` from `run_sweep.R` into `pipeline/sweep_core.R` so both the CLI runner and the bulk runner can source it cleanly.
