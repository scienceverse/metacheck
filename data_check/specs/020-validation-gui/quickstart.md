# Quickstart: Validation GUI

**Feature**: 020-validation-gui

---

## Prerequisites

All required packages are already installed in the project environment:
`shiny`, `bslib`, `haven`, `readxl`, `officer`, `pdftools`.

The pipeline must have been run for at least one paper so that
`outputs/<paper_id>/structure.csv` exists.

---

## Launch

From the `data_check/` directory (or any working directory where relative paths resolve):

```r
shiny::runApp("tools/validation_gui")
```

Or from an R console at the repo root:

```r
shiny::runApp("data_check/tools/validation_gui")
```

A browser tab opens automatically.

---

## First-time setup

On first launch a startup dialog prompts for annotator initials (e.g. `LB`). This value
is stored in the session and written to every ground-truth row saved during the session.
It is not persisted between sessions — re-enter initials on each launch.

---

## Workflow

1. **Select a paper** from the dropdown. The file list populates on the left.
2. **Review the context panel** on the right: file path, size, folder tree, file preview,
   and the machine predictions.
3. **Assign labels** using keyboard shortcuts:

   | Key | Action |
   |-----|--------|
   | `1` | Set type → `data` |
   | `2` | Set type → `code` |
   | `3` | Set type → `codebook` |
   | `4` | Set type → `supplemental` |
   | `5` | Set type → `doc` |
   | `6` | Set type → `readme` |
   | `7` | Set type → `asset` |
   | `8` | Set type → `other` |
   | `R` | Toggle `is_raw` (active only when type = `data`) |
   | `G` | Move focus to the group text input |
   | `⌘↩` | Save and advance to next unvalidated file |
   | `Tab` | Skip (no save); advance to next file |
   | `⌘[` | Go back to previous file |
   | `⌘/` | Show keyboard reference overlay |

   Number keys, `R`, and `G` are active only when focus is **not** inside a text input.

4. **Adjust group** if needed: the field auto-completes from groups already in the paper.
   Free-text values are accepted. Press `G` to focus it from anywhere.

5. **Save with `⌘↩`**. The file list marks the row green and the tool advances.

6. **Close at any time**. Ground-truth rows are written immediately on every save;
   no data is lost.

---

## Resuming a session

Re-launch the app and select the same paper. Previously saved rows are marked green.
The tool positions automatically on the first unvalidated file.

---

## Output

Each save writes one row to:

```
data_check/ground_truth/<paper_id>.csv
```

Schema: `paper_id`, `rel_path`, `type_gt`, `group_gt`, `is_raw_gt`, `validated_at`, `annotator`

To combine ground-truth files across papers into a single dataset:

```r
gt_files <- list.files("data_check/ground_truth", pattern = "\\.csv$", full.names = TRUE)
gt <- do.call(rbind, lapply(gt_files, read.csv,
                            colClasses = c(paper_id = "character",
                                           is_raw_gt = "logical")))
```

---

## Joining ground truth back to structure

```r
structure <- read.csv("data_check/outputs/<paper_id>/structure.csv",
                      colClasses = c(paper_id = "character"))
gt        <- read.csv("data_check/ground_truth/<paper_id>.csv",
                      colClasses = c(paper_id = "character",
                                     is_raw_gt = "logical"))

merged <- merge(structure, gt, by = c("paper_id", "rel_path"), all.x = TRUE)
# Rows with NA type_gt are unvalidated
```
