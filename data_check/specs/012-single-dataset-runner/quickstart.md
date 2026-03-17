# Quickstart: Single Dataset Runner

**Feature**: 012-single-dataset-runner

## Run

From the repo root:

```
Rscript data_check/run_single.R
```

No arguments needed. The script picks a random paper ID and runs both pipeline stages.

## Expected Output

Console output:

```
══════════════════════════════════════════════
  Paper: 0956797615569001
══════════════════════════════════════════════

── Stage 1: run_index ──────────────────────
  success=TRUE  files=14  data_files=3  columns=42  elapsed=38.2s

── Stage 2: run_codebook_label ─────────────
  label_status=ok  labelled=38  unlabelled=4  elapsed=12.1s

── Outputs: data_check/outputs/0956797615569001/
```

On failure (known error code):

```
── Stage 1: FAILED — no_links
  (stage 2 skipped)
```

## Output Files

After a successful run, `data_check/outputs/<paper_id>/` will contain:

| File | Contents |
|------|----------|
| `structure.csv` | One row per file in the repo, with type/group classification |
| `columns.csv`   | One row per column in each data file, with statistics |
| `labels.csv`    | Columns matched to codebook variable descriptions |
| `codebook_coverage.csv` | How many codebook variables were matched |
