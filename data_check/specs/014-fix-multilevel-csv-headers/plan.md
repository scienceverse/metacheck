# Implementation Plan: Fix Multi-Level CSV Header Skip Rule

**Branch**: `014-fix-multilevel-csv-headers` | **Date**: 2026-03-18 | **Spec**: [spec.md](spec.md)

## Summary

Replace the current skip-on-sight behaviour for `>50% ...N` column names with a two-branch recovery strategy:

- **Branch 1** (sub-header found): use the sub-header row values as `column_name`; extract condition/group labels from row-1 name prefixes and forward-fill them into a new `col_header_group` field. Row-1 `...N` placeholders are not persisted.
- **Branch 2** (no sub-header, partial labels): proceed with the original column names as-is; `col_header_group = NA`.
- **Skip**: only when header is entirely `...N` and no sub-header found.

`column_name` in `columns.csv` is always the resolved raw variable name — enabling direct codebook matching with zero changes to `match_column_labels()`. `col_header_group` is stored for future wide→long pivot support but not used by any current stage.

Changes touch two files: `0_index.R` (logic) and `output-schemas.md` (schema documentation). `helper.R`, `2_codebook_label.R`, and all other pipeline stages are unaffected.

## Technical Context

**Language/Version**: R (base R only — no new packages)
**Primary Dependencies**: `readxl` (already present), `haven` (already present) — `read_data_head()` in `helper.R` unchanged
**Storage**: CSV files on local filesystem — `outputs/<paper_id>/columns.csv`
**Testing**: Manual smoke-test via `run_index(paper_id, download = FALSE)` on known affected papers
**Target Platform**: macOS / local R session
**Project Type**: Data pipeline (single-paper and bulk entry points)
**Performance Goals**: No measurable change — one extra in-memory row scan + forward-fill per affected file
**Constraints**: No new packages; no changes to `helper.R`, `2_codebook_label.R`, or `labels.csv` schema
**Scale/Scope**: `extract_column_info()` in `0_index.R` (~25 lines changed); 1 new constant; 1 new output column

## Constitution Check

| Principle | Status | Notes |
|---|---|---|
| I. Crash Resilience | PASS | No change to write path or bulk runner |
| II. Paper ID Preservation | PASS | No new `read.csv()` calls; paper IDs untouched |
| III. Conservative Resource Limits | PASS | All existing limits respected; sub-header scan and forward-fill are in-memory only |
| IV. Centralised Shared Helpers | PASS | `read_data_head()` called once, unchanged. `helper.R` not modified. New logic stays in `extract_column_info()` |
| V. Structured Error Classification | PASS | No new error codes needed; skip path message updated but structure unchanged |

**Post-design re-check**: All gates pass. Scope is narrower than originally estimated — `helper.R` is untouched because `column_name` is now the resolved name and `match_column_labels()` needs no modification.

## Project Structure

### Documentation (this feature)

```text
specs/014-fix-multilevel-csv-headers/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
└── tasks.md             # Phase 2 output
```

### Source Code

```text
data_check/
├── 0_index.R            # Modified: new constant + updated extract_column_info()
├── helper.R             # Unchanged
├── 2_codebook_label.R   # Unchanged
└── docs/
    └── output-schemas.md  # Modified: add col_header_group column to columns.csv schema
```

No new files. No new directories. No contracts.

## Implementation Algorithm

```r
MULTILEVEL_HEADER_LOOKAHEAD <- 3L   # new constant — add to constants block near MAX_FILE_MB

# Inside extract_column_info(), replacing lines ~517–522:

auto_named <- grepl("^\\.\\.\\.\\d+$", names(df))

if (mean(auto_named) > 0.5) {

  # Step 1: Extract group labels from row-1 names (before any renaming).
  # "SHAM...3" → prefix "SHAM"; "...4" → prefix "" → forward-fill from last real prefix.
  row1_names   <- names(df)
  raw_prefixes <- sub("\\.\\.\\.\\d+$", "", row1_names)   # strips ...N suffix
  raw_prefixes[!nzchar(raw_prefixes)] <- NA_character_     # empty string → NA

  last_grp         <- NA_character_
  col_header_group <- vapply(raw_prefixes, function(p) {
    if (!is.na(p)) last_grp <<- p
    last_grp
  }, character(1))

  # Step 2: Branch 1 — scan for a better sub-header row.
  sub_header_row    <- NULL
  current_auto_frac <- mean(auto_named)

  for (i in seq_len(min(MULTILEVEL_HEADER_LOOKAHEAD, nrow(df)))) {
    candidate      <- as.character(df[i, ])
    cand_auto_frac <- mean(grepl("^\\.\\.\\.\\d+$", candidate))
    has_real       <- any(!is.na(candidate) & nzchar(candidate) &
                          !grepl("^\\.\\.\\.\\d+$", candidate))
    if (cand_auto_frac < current_auto_frac && has_real) {
      sub_header_row <- i
      break
    }
  }

  if (!is.null(sub_header_row)) {
    # Use sub-header values as column names.
    # NA or empty cells fall back to the original ...N name (preserves uniqueness).
    new_names          <- as.character(df[sub_header_row, ])
    fallback           <- is.na(new_names) | !nzchar(new_names)
    new_names[fallback] <- row1_names[fallback]
    new_names          <- make.unique(new_names)

    df         <- df[(sub_header_row + 1):nrow(df), , drop = FALSE]
    names(df)  <- new_names
    # col_header_group is already aligned column-wise — no adjustment needed.
    message("  multi-level header resolved (used row ", sub_header_row + 1,
            " as header): ", basename(path))

  } else {
    # Step 3: Branch 2 — no sub-header found.
    # col_header_group is not meaningful without a sub-header; set to NA.
    col_header_group <- rep(NA_character_, ncol(df))
    has_any_real     <- any(!auto_named)

    if (has_any_real) {
      message("  multi-level header detected (partial labels retained): ", basename(path))
      # proceed with df as-is
    } else {
      message("  skipping (multi-level header, no usable sub-header found): ", basename(path))
      return(NULL)
    }
  }

} else {
  # No multi-level structure — fill col_header_group with NA for schema consistency.
  col_header_group <- rep(NA_character_, ncol(df))
}

# col_header_group is now available for all subsequent code paths.
# Pass it through when constructing each column's record for columns.csv.
```

### How this covers each known affected file

| File | Auto% | Branch | `column_name` result | `col_header_group` result |
|---|---|---|---|---|
| Experiment 3_Summary_MTT_Foglio1.csv | 60% | 2 — no sub-header (row 2 is numeric); has partial labels `Passato...3` | Original names as-is | `NA` (no span structure) |
| Fitting_Foglio1.csv | 80% | 1 — row 2: `PAST_SHAM`, `PAST_BETA`, `PAST_ALPHA`, `R^2` | Sub-header values | `Peak Gaussian Curve`, `R^2` forward-filled |
| MTTDATA_Raw Data.csv | 75% | 1 — row 2: `PAST_Mortgage`, `PAST_Home`, etc. | Sub-header values | `SHAM`, `BETA`, `ALPHA` forward-filled across spans |
| Avreaged raw data_Sheet1.csv | 87% | 1 — row 2: `SUBJECT`, `LEARNING`, `TEST` | Sub-header values | `RB_TASK`, `II_TASK`, `Discrimination task` forward-filled |

### Genuinely headerless files (no regression)

All-`...N` header with no qualifying sub-header in 3 rows AND no non-`...N` column names → Branch 2 `has_any_real = FALSE` → skip. Existing behaviour preserved.

### Forward-fill detail for `col_header_group`

Row 1 of `MTTDATA_Raw Data.csv`: `...1`, `...2`, `SHAM...3`, `...4`, `...5`, `...6`, `...7`, `...8`, `BETA...9`, ...

| Col | row1_name | raw_prefix | col_header_group |
|---|---|---|---|
| 1 | `...1` | NA | NA |
| 2 | `...2` | NA | NA |
| 3 | `SHAM...3` | `SHAM` | `SHAM` |
| 4–8 | `...4`–`...8` | NA | `SHAM` (filled) |
| 9 | `BETA...9` | `BETA` | `BETA` |
| 10–14 | `...10`–`...14` | NA | `BETA` (filled) |
| 15 | `ALPHA...15` | `ALPHA` | `ALPHA` |
| … | … | … | … |

## Complexity Tracking

No constitution violations.

## Phase 1 Artifacts

- [research.md](research.md) — root cause analysis, naming strategy decision, forward-fill algorithm
- [data-model.md](data-model.md) — in-memory structures, new fields, output schema
- No contracts file — purely internal change with no external interface
