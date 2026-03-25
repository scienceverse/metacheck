# run_psychds_single.R
# ─────────────────────────────────────────────────────────────────────────────
# Convert one paper to PsychDS format.  Smoke-test / dev entry point.
#
# Usage:
#   paper_id <- "0956797615620784"   # set before sourcing (optional)
#   source("data_check/runners/run_psychds_single.R")
#
#   Or run from repo root:
#   Rscript data_check/runners/run_psychds_single.R [paper_id]
#
# If paper_id is not set, picks a random successfully indexed paper from
# results/bulk_summary.csv.
#
# Output: data_check/psychds/<paper_id>/
# ─────────────────────────────────────────────────────────────────────────────

source("data_check/pipeline/helper.R")
source("data_check/pipeline/3_psychds_convert.R")

# Allow paper_id to be passed as a command-line argument
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) {
  paper_id <- as.character(args[1])
}

# If paper_id was auto-selected by a previous source() run, clear it so
# re-sourcing without an explicit paper_id picks a fresh random paper.
if (exists(".psychds_paper_id_was_random") && .psychds_paper_id_was_random) {
  rm(paper_id, .psychds_paper_id_was_random)
}

# If paper_id is not defined (interactive use without setting it), pick randomly
if (!exists("paper_id") || is.null(paper_id) || !nzchar(paper_id)) {
  bulk_path <- "./data_check/results/bulk_summary.csv"
  if (!file.exists(bulk_path)) bulk_path <- "./data_check/bulk_summary.csv"
  if (!file.exists(bulk_path))
    stop("No paper_id set and no bulk_summary.csv found. ",
         "Set `paper_id <- '...'` before sourcing this script.")
  bulk <- read.csv(bulk_path, stringsAsFactors = FALSE,
                   colClasses = c(paper_id = "character"))
  ok   <- bulk[!is.na(bulk$success) & as.logical(bulk$success) == TRUE, "paper_id"]
  if (length(ok) == 0)
    stop("No successfully indexed papers found in bulk_summary.csv.")
  paper_id <- sample(ok, 1)
  .psychds_paper_id_was_random <- TRUE
  message("No paper_id set — randomly selected: ", paper_id)
}

message("─────────────────────────────────────────────────────")
message("Converting paper: ", paper_id)
message("─────────────────────────────────────────────────────")

t_start <- proc.time()
results  <- convert_psychds(paper_id)
elapsed  <- round((proc.time() - t_start)[["elapsed"]], 1)

message("")
message("Done in ", elapsed, "s — ", length(results), " study result(s):")
message("")
for (r in results) {
  status <- if (isTRUE(r$success)) "✓ OK" else paste0("✗ ", r$error)
  message(sprintf("  study_group  : %s", r$study_group))
  message(sprintf("  status       : %s", status))
  message(sprintf("  data files   : %d converted, %d raw",
                  r$n_data_files, r$n_raw_files))
  message(sprintf("  variables    : %d (%d labelled)",
                  r$n_variables, r$n_labelled))
  message(sprintf("  paper meta   : %s",
                  if (isTRUE(r$has_paper_metadata)) "GROBID XML" else "fallback"))
  message(sprintf("  ground truth : %s",
                  if (isTRUE(r$has_ground_truth)) "applied" else "none"))
  message(sprintf("  output       : %s", r$output_path))
  message("")
}
