# Data Model: Validation GUI

**Feature**: 020-validation-gui | **Date**: 2026-03-23

---

## Entities

### Paper

Represents one OSF repository that has been indexed by the pipeline.

| Field | Type | Source | Notes |
|-------|------|--------|-------|
| `paper_id` | character | `structure.csv` col `paper_id` | Leading zeros preserved; read with `colClasses = c(paper_id = "character")` |

A paper is discovered by scanning `outputs/` for subdirectories containing a `structure.csv`.
One paper may have zero or one ground-truth file.

---

### StructureRow

Represents one file entry in a paper's machine-generated `structure.csv`. Read-only from
the GUI's perspective.

| Field | Type | Source | Notes |
|-------|------|--------|-------|
| `paper_id` | character | `structure.csv` | Join to Paper |
| `path` | character | `structure.csv` | Absolute local path; may not exist on current machine |
| `rel_path` | character | `structure.csv` | Primary key within a paper; join key to GroundTruthRow |
| `filename` | character | `structure.csv` | Basename |
| `ext` | character | `structure.csv` | Lowercase extension |
| `type` | character | `structure.csv` | Machine-predicted type (see valid values below) |
| `group` | character | `structure.csv` | Machine-predicted group |
| `is_raw` | logical | `structure.csv` | Machine-predicted rawness |
| `is_sentinel` | logical | `structure.csv` | `TRUE` if row represents a collapsed folder |

**Valid `type` values**: `data`, `code`, `codebook`, `supplemental`, `doc`, `readme`, `asset`, `other`

**Valid `group` values**: `ex<N>`, `pilot<N>`, `other`, `na`

---

### GroundTruthRow

The annotator's verified labels for one StructureRow. Written to `ground_truth/<paper_id>.csv`.

| Field | Type | Validation | Notes |
|-------|------|------------|-------|
| `paper_id` | character | Must match the paper being annotated | `colClasses = c(paper_id = "character")` on read |
| `rel_path` | character | Must match a `rel_path` in `structure.csv` | Primary key within a paper |
| `type_gt` | character | Must be one of the 8 valid type values | Annotator's verified type |
| `group_gt` | character | Non-empty string; `na` for non-data/non-code types | Annotator's verified group |
| `is_raw_gt` | logical | Must be `FALSE` when `type_gt ≠ "data"` | Annotator's verified rawness flag |
| `validated_at` | character | ISO-8601 datetime string | Written by app at save time; never user-editable |
| `annotator` | character | Non-empty string | Set at session startup; applied to all rows in the session |

**Uniqueness constraint**: At most one row per `(paper_id, rel_path)` pair. On save, any
existing row for the same `rel_path` is replaced (upsert semantics).

**File path**: `data_check/ground_truth/<paper_id>.csv`

**Read/write rules**:
- Written with `write.csv(..., row.names = FALSE)`
- Read with `read.csv(..., colClasses = c(paper_id = "character", is_raw_gt = "logical"))`
- Columns written in canonical order: `paper_id`, `rel_path`, `type_gt`, `group_gt`,
  `is_raw_gt`, `validated_at`, `annotator`

---

### SessionState (in-memory only)

Tracks transient UI state for the current session. Never persisted.

| Field | Type | Notes |
|-------|------|-------|
| `current_index` | integer | Index into the StructureRow list for the current paper |
| `skipped_indices` | integer vector | Indices skipped this session (not saved) |
| `annotator` | character | Set at startup; used for all GroundTruthRow writes |
| `text_focused` | logical | `TRUE` when keyboard focus is inside a text input; suppresses key shortcuts |

---

## State Transitions

### File validation status

Each StructureRow has one of three display states in the file list:

```
unvisited ──────────────────────────────────────────► validated
    │          (annotator presses ⌘↩)                 (row in GT file)
    │
    └──────────────────────────────────────────────► skipped
               (annotator presses Tab)                (in-memory only;
                                                       returns to unvisited
                                                       on session restart)
```

A validated row can be revisited and re-saved, which updates the GroundTruthRow in place
(upsert). A validated row cannot be "un-validated" except by deleting the ground-truth file
manually.

---

## Cross-file Join

The ground-truth file is designed to join back to `structure.csv` on `(paper_id, rel_path)`:

```
outputs/<paper_id>/structure.csv
  LEFT JOIN
ground_truth/<paper_id>.csv
  ON paper_id = paper_id AND rel_path = rel_path
```

Rows in `structure.csv` with no matching ground-truth row are unvalidated. The join
produces one row per file with both machine predictions and (where available) human labels
side by side.
