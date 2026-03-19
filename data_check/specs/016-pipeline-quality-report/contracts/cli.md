# CLI Contract: report_quality.R

**Script**: `data_check/report_quality.R`
**Invocation**: `Rscript report_quality.R [options]`

---

## Arguments

| Argument | Type | Default | Description |
|---|---|---|---|
| `--bulk` | path | `./bulk_summary.csv` | Path to bulk_summary.csv |
| `--codebook` | path | `./codebook_summary.csv` | Path to codebook_summary.csv |
| `--outputs-dir` | path | `./outputs` | Root directory containing per-paper output folders |
| `--unknown-threshold` | integer 0–100 | `30` | % unknown col_types above which a paper is flagged as outlier |
| `--top-n` | integer ≥ 1 | `10` | Number of papers to show in performance top-N lists |
| `--sections` | comma-separated | `all` | Which report sections to run: `bulk`, `coltypes`, `codebook`, `timing`, `all` |

**Automatic output**: Every run writes `quality_report_YYYY-MM-DD.md` to the working directory. No flag needed.

---

## Exit Codes

| Code | Meaning |
|---|---|
| 0 | Report completed successfully (with or without warnings) |
| 1 | Fatal error: required input file not found and no fallback possible |

---

## Console Output Sections

Each section is preceded by a header line (`===`) and followed by a blank line.

### Section: `bulk`
```
=== Bulk Run Overview ===
Total papers:    252
  Successful:    87  (34.5%)
  Failed:        165 (65.5%)

Failure breakdown:
  download_failed   : 120 (72.7% of failures)
  no_links          :  30 (18.2% of failures)
  empty_repo        :  10 ( 6.1% of failures)
  too_large         :   5 ( 3.0% of failures)

Timing (successful papers only):
              mean    median       max
  elapsed_ms  9191     8200     45000
  download_ms   11       10       200
  llm_ms      8903     8000     40000
  column_ms     72       60      1200
```

### Section: `coltypes`
```
=== Column-Type Distribution ===
Total columns across all papers: 1842

  col_type      count   percent
  continuous      820    44.5%
  binary          310    16.8%
  categorical     280    15.2%
  id              150     8.1%
  unknown         140     7.6%
  text             90     4.9%
  date             52     2.8%

High unknown-rate papers (>30% unknown):
  paper_id              unknown_rate  total_cols
  0956797614557697          45.0%          20
  ...

Zero-column papers: 3
  0956797613520608  (no columns.csv found)
  ...
```

### Section: `codebook`
```
=== Codebook Coverage ===
Papers with codebook labelling attempted: 114
  Labelling succeeded:     110 (96.5%)
  No codebook found:         4 ( 3.5%)

Coverage rate per paper (labelled/total columns):
  Overall mean: 72.3%
  Median:       81.0%
  Min:           0.0%
  Max:         100.0%

Lowest-coverage papers:
  paper_id              coverage  n_matched  n_total
  0956797616636631          0.0%          0       15
  ...
```

### Section: `timing`
```
=== Performance: Top-10 Slowest Papers ===

By total elapsed time:
  paper_id              elapsed_ms
  0956797616647519          45000
  ...

By download time:
  ...

By LLM time:
  ...

By column extraction time:
  ...
```

---

## Warnings (non-fatal, printed to stderr)

- `[WARN] N rows skipped in bulk_summary.csv (malformed)` — when rows cannot be parsed
- `[WARN] paper_id appears N times in bulk_summary.csv; using last occurrence` — when duplicates found
- `[WARN] codebook_summary.csv not found — codebook section skipped` — when file absent
- `[WARN] N papers in outputs/ have no columns.csv` — missing per-paper files
- `[WARN] Stability cannot be computed with <2 repeats` — (reserved for future use by 017)
