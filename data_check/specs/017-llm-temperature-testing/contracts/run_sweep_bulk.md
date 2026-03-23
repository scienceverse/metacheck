# Contract: run_sweep_bulk.R

**Type**: Config-script (top-level, no CLI args — edit config block directly, same as `run_0_index_bulk.R`)

## Config block

```r
TEMPERATURES <- c(0.0, 0.3, 0.7, 1.0)  # temperatures to sweep
REPEATS      <- 3L                       # repeats per temperature
N_PAPERS     <- Inf                      # Inf = all; integer to cap
N_WORKERS    <- parallel::detectCores() - 1L  # parallel workers (mclapply)
SWEEP_DIR    <- "./data_check/sweep_results"
BULK_LOG     <- "./data_check/sweep_results/sweep_bulk_log.csv"
SEED         <- NULL                     # integer for deterministic paper ordering, or NULL
```

## Behaviour

1. Sources `run_sweep.R` (which sources `helper.R`, `0_index.R`, `2_codebook_label.R`)
2. Discovers all paper IDs from `XML_DIR` via `list.files(..., pattern = "\\.xml$")`
3. Loads `BULK_LOG` if it exists; skips paper IDs already present
4. Applies `SEED` shuffle if set; applies `N_PAPERS` cap
5. Splits remaining papers into batches of `N_WORKERS`
6. For each batch: calls `parallel::mclapply(batch, run_paper_sweep, ..., mc.cores = N_WORKERS)`
7. After each batch: appends all batch result rows to `BULK_LOG` (batch-level crash resilience)
8. Prints summary at end

**Write safety**: Only the parent process writes to `BULK_LOG`. Workers write only to their own `sweep_results/<paper_id>/sweep_log.csv`.

## Outputs

| Output | Location |
|---|---|
| Bulk progress log | `BULK_LOG` (default: `sweep_results/sweep_bulk_log.csv`) |
| Per-paper run log | `SWEEP_DIR/<paper_id>/sweep_log.csv` |
| Per-run pipeline outputs | `SWEEP_DIR/<paper_id>/temp_<T>/rep_<R>/` |

## Run statuses

| Status | Meaning | Counts as |
|--------|---------|-----------|
| `ok` | Pipeline ran and produced data columns | `n_ok` |
| `no_data` | Pipeline ran; no files classified as `data` | `n_ok`, `n_no_data` |
| `failed` | Pipeline errored | `n_failed` |

## Error behaviour

- Individual run failures are logged in per-paper `sweep_log.csv` with `status = "failed"` and do not abort the sweep.
- Runs where no data files exist are logged with `status = "no_data"` — treated as a successful completion; codebook stage is skipped.
- Paper-level failures (e.g. `run_paper_sweep()` itself throws) are caught, logged to `BULK_LOG` with `status = "done"` and `n_failed = REPEATS * length(TEMPERATURES)`, and the bulk runner continues to the next paper.

## Resume

Re-running with the same config resumes from the last unprocessed paper. Already-logged papers (in `BULK_LOG`) are skipped. Within a paper, already-completed (temperature × repeat) combinations are skipped via the per-paper `sweep_log.csv`.

## Exported function (consumed by this script)

```r
run_paper_sweep(paper_id, temperatures, repeats, sweep_dir)
# Returns: invisible list(n_ok, n_no_data, n_failed, n_skipped)
# n_ok includes no_data runs; n_no_data is the subset of those with status="no_data"
```
