# Data Model: LLM Temperature Stability Testing — Bulk Sweep Extension

**Branch**: `017-llm-temperature-testing` | **Date**: 2026-03-20

## Entities

### SweepRun *(existing)*

One record per (paper_id, temperature, repeat_num). Written to `sweep_results/<paper_id>/sweep_log.csv`.

| Field | Type | Constraints |
|---|---|---|
| `paper_id` | character | Leading-zero safe; never numeric |
| `temperature` | numeric | [0.0, 2.0] inclusive |
| `repeat_num` | integer | ≥ 1 |
| `output_dir` | character | Absolute path to `temp_<T>/rep_<R>/` |
| `status` | character | `"ok"`, `"no_data"`, or `"failed"` |
| `error` | character | Empty string if `status = "ok"` or `"no_data"` |
| `elapsed_ms` | integer | Wall time in ms |
| `run_timestamp` | character | ISO 8601 (`YYYY-MM-DDTHH:MM:SS`) |

**Identity**: (`paper_id`, `temperature`, `repeat_num`) — unique within a paper's log.

---

### BulkSweepRecord *(new)*

One record per paper processed by the bulk sweep runner. Written to `sweep_results/sweep_bulk_log.csv`.

| Field | Type | Constraints |
|---|---|---|
| `paper_id` | character | Leading-zero safe |
| `temperatures` | character | Comma-separated numeric values, e.g. `"0,0.3,0.7,1"` |
| `repeats` | integer | Repeat count used for this paper |
| `n_ok` | integer | Runs completing with `status = "ok"` or `"no_data"` |
| `n_no_data` | integer | Runs where no data files were found (`status = "no_data"`) |
| `n_failed` | integer | Runs that errored (`status = "failed"`) |
| `n_skipped` | integer | Runs skipped (already in per-paper sweep_log.csv) |
| `elapsed_ms` | integer | Total wall time for this paper's sweep |
| `timestamp` | character | ISO 8601 timestamp |
| `status` | character | Always `"done"` — row is only written after all combinations attempted |

**Identity**: `paper_id` — unique in the bulk log (one row per paper).

**Resume rule**: A paper is skipped if its `paper_id` appears in `sweep_bulk_log.csv`. Per-run resume (skipping individual temperature×repeat combinations) is handled by the per-paper `sweep_log.csv`.

**Write safety**: Rows are appended by the parent process only, after each `mclapply` batch returns. Workers (child processes) never write to `sweep_bulk_log.csv`. No file locking is required.

---

### GrandReportRow *(new)*

One record per (paper_id × temperature × pipeline stage). Written to `sweep_grand_report.csv` (or user-specified path).

| Field | Type | Constraints |
|---|---|---|
| `paper_id` | character | Leading-zero safe |
| `temperature` | numeric | Temperature value from sweep log |
| `stage` | character | `"index"` or `"codebook"` |
| `repeat_count` | integer | Number of ok repeats used in metric computation |
| `mean_pairwise_agreement` | numeric / NA | [0, 1]; NA if <2 ok repeats, or no codebook (codebook stage) |
| `known_type_rate` | numeric / NA | [0, 1]; populated for `stage = "index"` only; NA for `"codebook"` |
| `codebook_coverage_rate` | numeric / NA | [0, 1]; populated for `stage = "codebook"` only; NA for `"index"` |
| `nonempty_label_rate` | numeric / NA | [0, 1]; populated for `stage = "codebook"` only; NA for `"index"` |
| `status` | character | `"ok"`, `"no_data"`, `"no_sweep"` (paper not yet swept), `"failed"` |

**Identity**: (`paper_id`, `temperature`, `stage`) — unique.

**Stage NA rules**:
- `stage = "index"`: `codebook_coverage_rate` = NA, `nonempty_label_rate` = NA
- `stage = "codebook"`: `known_type_rate` = NA
- `stage = "codebook"` when no codebook exists: `mean_pairwise_agreement` = NA, `codebook_coverage_rate` = NA, `nonempty_label_rate` = NA
- `status = "no_data"`: all metric columns = NA (no columns to measure); codebook stage is not attempted

**Row count invariant** (when all papers swept): P × T × 2 rows, where P = papers processed, T = temperatures swept, 2 = pipeline stages.

---

## File Locations

| File | Location | Created by |
|---|---|---|
| `sweep_log.csv` | `sweep_results/<paper_id>/sweep_log.csv` | `run_sweep.R` / `run_paper_sweep()` |
| `sweep_bulk_log.csv` | `sweep_results/sweep_bulk_log.csv` | `run_sweep_bulk.R` |
| `sweep_grand_report.csv` | `./data_check/sweep_grand_report.csv` (default) | `report_sweep_grand.R` |
| Per-run outputs | `sweep_results/<paper_id>/temp_<T>/rep_<R>/` | `run_one()` in `run_sweep.R` |

---

## State Transitions

### Paper lifecycle in bulk sweep

```
discovered (in XML_DIR)
  → pending (not in sweep_bulk_log.csv)
    → in-progress (being swept — no log row yet)
      → done (row appended to sweep_bulk_log.csv with status="done")
```

### Run lifecycle within a paper sweep

```
pending (not in sweep_log.csv)
  → running
    → ok       (sweep_log.csv row: status="ok"      — data found and processed)
    → no_data  (sweep_log.csv row: status="no_data" — pipeline ok, no data files)
    → failed   (sweep_log.csv row: status="failed"  — pipeline errored)
  → skipped (already in sweep_log.csv — resume path)
```

**Note**: `no_data` counts as a completed run for resume purposes (it will be skipped on re-run like `ok`). It does NOT count toward `n_failed` in `BulkSweepRecord`.
