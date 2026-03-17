# migrate_structure.R
# ─────────────────────────────────────────────────────────────────────────────
# ONE-TIME migration script.
#
# Moves all per-paper CSVs from the old flat structure/ directory into the new
# per-paper outputs/<paper_id>/ layout, stripping the paper-ID prefix from
# filenames:
#
#   structure/<paper_id>_structure.csv        → outputs/<paper_id>/structure.csv
#   structure/<paper_id>_columns.csv          → outputs/<paper_id>/columns.csv
#   structure/<paper_id>_labels.csv           → outputs/<paper_id>/labels.csv
#   structure/<paper_id>_codebook_coverage.csv → outputs/<paper_id>/codebook_coverage.csv
#
# Safe to run multiple times: existing destination files are NOT overwritten.
# Review the output before deleting the structure/ directory.
# ─────────────────────────────────────────────────────────────────────────────

source("./data_check/helper.R")

SRC_DIR <- "./data_check/structure"

csv_files <- list.files(SRC_DIR, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  message("── No CSV files found in ", SRC_DIR, " — nothing to migrate.")
  stop("Exiting: structure/ is already empty.")
}

message("── Found ", length(csv_files), " CSV file(s) in ", SRC_DIR)

n_moved   <- 0L
n_skipped <- 0L
n_failed  <- 0L

for (src in csv_files) {
  base <- basename(src)

  # Extract paper_id: everything before the first underscore.
  # Paper IDs are pure-digit strings; the short filename follows the first '_'.
  underscore_pos <- regexpr("_", base, fixed = TRUE)
  if (underscore_pos < 2) {
    message("  SKIP (unexpected filename format): ", base)
    n_skipped <- n_skipped + 1L
    next
  }

  paper_id   <- substr(base, 1, underscore_pos - 1)
  short_name <- substr(base, underscore_pos + 1, nchar(base))  # e.g. "structure.csv"

  dest_dir <- paper_output_dir(paper_id)   # creates outputs/<paper_id>/ if needed
  dest     <- file.path(dest_dir, short_name)

  if (file.exists(dest)) {
    message("  SKIP (dest exists): ", dest)
    n_skipped <- n_skipped + 1L
    next
  }

  ok <- file.copy(src, dest, overwrite = FALSE)
  if (!ok || !file.exists(dest)) {
    message("  FAIL (copy failed): ", base, " → ", dest)
    n_failed <- n_failed + 1L
    next
  }

  file.remove(src)
  message("  moved: ", base, " → outputs/", paper_id, "/", short_name)
  n_moved <- n_moved + 1L
}

cat("\n── Migration complete ───────────────────────────────────────────────────\n")
cat(sprintf("   Moved:   %d\n", n_moved))
cat(sprintf("   Skipped: %d\n", n_skipped))
cat(sprintf("   Failed:  %d\n", n_failed))

remaining <- list.files(SRC_DIR, pattern = "\\.csv$", full.names = FALSE)
if (length(remaining) == 0) {
  cat("\n   structure/ is now empty — safe to remove manually.\n")
} else {
  cat(sprintf("\n   WARNING: %d file(s) remain in structure/:\n", length(remaining)))
  for (f in remaining) cat("     ", f, "\n")
}
