# Contract: report_sweep_grand.R

**Type**: CLI script (Rscript / interactive)

## Usage

```bash
Rscript report_sweep_grand.R \
  [--sweep-dir ./data_check/sweep_results] \
  [--out-csv ./data_check/sweep_grand_report.csv]
```

## Arguments

| Argument | Default | Description |
|---|---|---|
| `--sweep-dir` | `./data_check/sweep_results` | Root directory containing `<paper_id>/sweep_log.csv` subdirectories |
| `--out-csv` | `./data_check/sweep_grand_report.csv` | Output path for the grand flat CSV |

## Behaviour

1. Sources `report_sweep.R` (gets `compute_stability`, `compute_quality`, data loaders)
2. Sources `0_index.R` (gets `XML_DIR` for paper discovery)
3. Discovers all paper IDs from `XML_DIR`
4. For each paper:
   - If no `sweep_log.csv` found → emits `status = "no_sweep"` rows (one per temperature placeholder is skipped; paper is noted in summary)
   - Otherwise: loads `sweep_log.csv`; calls `compute_stability()` + `compute_quality()`; builds 2 rows per temperature (one per stage)
5. Writes flat CSV to `--out-csv` (overwrites if exists)
6. Prints summary: total papers, papers with sweeps, papers with no sweep, total rows written

## Output schema

One row per (`paper_id` × `temperature` × `stage`):

| Column | Type | Notes |
|---|---|---|
| `paper_id` | character | Leading-zero safe |
| `temperature` | numeric | From sweep log |
| `stage` | character | `"index"` or `"codebook"` |
| `repeat_count` | integer | Number of ok repeats used |
| `mean_pairwise_agreement` | numeric / NA | NA if <2 ok repeats or no codebook (codebook stage) |
| `known_type_rate` | numeric / NA | index stage only |
| `codebook_coverage_rate` | numeric / NA | codebook stage only |
| `nonempty_label_rate` | numeric / NA | codebook stage only |
| `status` | character | `"ok"`, `"no_data"`, `"no_sweep"`, or `"failed"` |

## Row count

For a complete sweep of P papers × T temperatures: exactly P × T × 2 rows.
Papers not yet swept contribute 0 rows (noted in console summary only).

## Error behaviour

- Papers with no `sweep_log.csv`: skipped from output, counted in summary.
- Papers whose `sweep_log.csv` is unreadable: warning emitted; paper skipped.
- If `--sweep-dir` does not exist: error with clear message.
- If `--out-csv` parent directory does not exist: error with clear message.
