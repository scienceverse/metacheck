# gt_store.R
# Ground-truth CSV read/write and paper discovery helpers.
# Paths are resolved relative to the data_check/ root set by app.R.

# ── Canonical column order ────────────────────────────────────────────────────

GT_COLS <- c("paper_id", "rel_path", "type_gt", "group_gt",
             "is_raw_gt", "validated_at", "annotator")

# ── Empty GT data.frame ───────────────────────────────────────────────────────

empty_gt <- function() {
  data.frame(
    paper_id     = character(0),
    rel_path     = character(0),
    type_gt      = character(0),
    group_gt     = character(0),
    is_raw_gt    = logical(0),
    validated_at = character(0),
    annotator    = character(0),
    stringsAsFactors = FALSE
  )
}

# ── Paper discovery ───────────────────────────────────────────────────────────

# Scan outputs/ for subdirectories containing structure.csv.
# Returns a sorted character vector of paper IDs.
discover_papers <- function() {
  outputs_dir <- file.path(getOption("dc_root", "."), "outputs")
  if (!dir.exists(outputs_dir)) return(character(0))
  dirs <- list.dirs(outputs_dir, full.names = FALSE, recursive = FALSE)
  has_structure <- dirs[file.exists(file.path(outputs_dir, dirs, "structure.csv"))]
  sort(has_structure)
}

# ── Structure loading ─────────────────────────────────────────────────────────

load_structure <- function(paper_id) {
  outputs_dir <- file.path(getOption("dc_root", "."), "outputs")
  path <- file.path(outputs_dir, paper_id, "structure.csv")
  read.csv(path,
           colClasses      = c(paper_id    = "character",
                               is_raw      = "logical",
                               is_sentinel = "logical"),
           stringsAsFactors = FALSE)
}

# ── Ground-truth read ─────────────────────────────────────────────────────────

read_gt <- function(paper_id) {
  gt_dir <- file.path(getOption("dc_root", "."), "ground_truth")
  path   <- file.path(gt_dir, paste0(paper_id, ".csv"))
  if (!file.exists(path)) return(empty_gt())
  tryCatch({
    df <- read.csv(path,
                   colClasses      = c(paper_id  = "character",
                                       is_raw_gt = "logical"),
                   stringsAsFactors = FALSE)
    # Ensure all expected columns are present
    for (col in setdiff(GT_COLS, names(df))) df[[col]] <- NA_character_
    # T035: silently correct is_raw_gt = TRUE for non-data files
    non_data <- !is.na(df$type_gt) & df$type_gt != "data"
    df$is_raw_gt[non_data] <- FALSE
    df[GT_COLS]
  }, error = function(e) {
    warning("Could not read ground truth for ", paper_id, ": ", conditionMessage(e))
    empty_gt()
  })
}

# ── Ground-truth write ────────────────────────────────────────────────────────

# Upsert one row (matched on rel_path) into the in-memory GT data.frame.
# Returns the updated data.frame.
upsert_gt <- function(gt_df, new_row) {
  existing <- which(gt_df$rel_path == new_row$rel_path)
  if (length(existing) > 0) {
    gt_df[existing[1], ] <- new_row
  } else {
    gt_df <- rbind(gt_df, new_row)
  }
  gt_df
}

# Write the full GT data.frame to disk immediately (no batching).
write_gt <- function(paper_id, gt_df) {
  gt_dir <- file.path(getOption("dc_root", "."), "ground_truth")
  if (!dir.exists(gt_dir)) dir.create(gt_dir, recursive = TRUE)
  path <- file.path(gt_dir, paste0(paper_id, ".csv"))
  write.csv(gt_df[GT_COLS], path, row.names = FALSE)
  invisible(path)
}
