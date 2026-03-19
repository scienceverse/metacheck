# Quickstart: Pipeline Quality Insights Report (016)

## Prerequisites

- Completed bulk run: `bulk_summary.csv` exists in `data_check/`
- Optionally: `codebook_summary.csv` exists and per-paper outputs under `data_check/outputs/`
- R installed; run from the `data_check/` directory

---

## Basic Usage

Run a full report against the default output location:

```bash
cd data_check/
Rscript report_quality.R
```

This reads `./bulk_summary.csv`, `./codebook_summary.csv`, and scans `./outputs/` and prints all four report sections to the console.

---

## Run Only Specific Sections

```bash
# Just the bulk overview and timing
Rscript report_quality.R --sections bulk,timing

# Just codebook coverage
Rscript report_quality.R --sections codebook
```

---

## Custom Paths

```bash
Rscript report_quality.R \
  --bulk /path/to/bulk_summary.csv \
  --codebook /path/to/codebook_summary.csv \
  --outputs-dir /path/to/outputs
```

---

## Adjust Outlier Thresholds

```bash
# Flag papers with >50% unknown column types (stricter)
Rscript report_quality.R --unknown-threshold 50

# Show top-20 instead of top-10 in performance lists
Rscript report_quality.R --top-n 20
```

---

## Save a Summary CSV

```bash
Rscript report_quality.R --out quality_report_2026-03-19.csv
```

---

## Expected Output

The script prints to stdout. Redirect to a file if needed:

```bash
Rscript report_quality.R > report.txt
```

---

## Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `Error: bulk_summary.csv not found` | Wrong working directory | Run from `data_check/` or pass `--bulk` |
| `[WARN] codebook section skipped` | No `codebook_summary.csv` | Run codebook bulk first, or pass `--sections bulk,coltypes,timing` |
| `[WARN] N papers have no columns.csv` | Index bulk ran but outputs missing | Check `outputs/` directory; re-run index for missing papers |
| Column-type distribution shows only `unknown` | LLM classification failed broadly | Check `llm_ms` in timing report; investigate LLM connectivity |
