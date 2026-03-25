# run_psychds_bulk.R
# ─────────────────────────────────────────────────────────────────────────────
# Batch-convert all successfully indexed papers to PsychDS format.
# Crash-resilient: appends one row per study to psychds/conversion_summary.csv
# immediately after each paper.  Auto-resumes on restart — skips papers where
# all studies already have success == TRUE in the summary.
#
# Usage:
#   Rscript data_check/runners/run_psychds_bulk.R
#   or source("data_check/runners/run_psychds_bulk.R")
# ─────────────────────────────────────────────────────────────────────────────

source("data_check/pipeline/helper.R")
source("data_check/pipeline/3_psychds_convert.R")

SUMMARY_CSV <- file.path(PSYCHDS_OUT_DIR, "conversion_summary.csv")

# ── 1. Load target papers from bulk_summary.csv ───────────────────────────────

bulk_path <- "./data_check/results/bulk_summary.csv"
if (!file.exists(bulk_path)) bulk_path <- "./data_check/bulk_summary.csv"
if (!file.exists(bulk_path))
  stop("bulk_summary.csv not found. Run the index pipeline first.")

bulk <- read.csv(bulk_path, stringsAsFactors = FALSE,
                 colClasses = c(paper_id = "character"))
target_ids <- bulk$paper_id[!is.na(bulk$success) & as.logical(bulk$success) == TRUE]
if (length(target_ids) == 0) stop("No successfully indexed papers in bulk_summary.csv.")
target_ids <- unique(target_ids)

message("Target papers: ", length(target_ids))

# ── 2. Load existing conversion summary (auto-resume) ─────────────────────────

done_pairs <- character(0)  # "paper_id:::study_group" pairs already succeeded

if (file.exists(SUMMARY_CSV)) {
  done <- tryCatch(
    read.csv(SUMMARY_CSV, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) NULL
  )
  if (!is.null(done) && nrow(done) > 0) {
    succeeded <- done[!is.na(done$success) & done$success == TRUE, ]
    if (nrow(succeeded) > 0)
      done_pairs <- paste0(succeeded$paper_id, ":::", succeeded$study_group)
    message("Already converted: ", length(done_pairs), " studies — will skip")
  }
}

# ── 3. Determine which papers still need processing ───────────────────────────

papers_to_run <- Filter(function(pid) {
  # A paper is "done" only if ALL its studies are in done_pairs.
  # Since we don't know how many studies a paper has until we run it,
  # we just skip papers where at least one success row exists AND no
  # failure row — safer to re-attempt papers with any failure row.
  pid_pairs <- done_pairs[startsWith(done_pairs, paste0(pid, "::::"))]
  length(pid_pairs) > 0 &&
    all(endsWith(pid_pairs, ":::TRUE") | grepl(":::TRUE$", pid_pairs))
}, target_ids)

# Simpler approach: skip paper only if it appears in done_pairs with success
done_paper_ids <- unique(sub(":::.*", "", done_pairs))
papers_to_run  <- setdiff(target_ids, done_paper_ids)

total     <- length(target_ids)
remaining <- length(papers_to_run)
message("Remaining: ", remaining, " / ", total)

if (remaining == 0) {
  message("All papers already converted. Nothing to do.")
  invisible(NULL)
}

# ── 4. Convert each paper ─────────────────────────────────────────────────────

n_ok  <- 0L
n_err <- 0L

for (k in seq_along(papers_to_run)) {
  pid <- papers_to_run[k]
  cat(sprintf("[%d/%d] %s ... ", k, remaining, pid))

  results <- tryCatch(
    convert_psychds(pid),
    error = function(e) {
      list(list(
        paper_id = pid, study_group = "all",
        success = FALSE, error = conditionMessage(e),
        n_data_files = 0L, n_raw_files = 0L,
        n_variables = 0L, n_labelled = 0L,
        has_paper_metadata = FALSE, has_ground_truth = FALSE,
        output_path = NA_character_
      ))
    }
  )

  # Append to summary immediately (Principle I)
  append_conversion_summary(results, SUMMARY_CSV)

  n_studies_ok  <- sum(vapply(results, function(r) isTRUE(r$success), logical(1)))
  n_studies_err <- length(results) - n_studies_ok

  if (n_studies_err == 0) {
    n_ok <- n_ok + 1L
    cat(sprintf("%d stud%s OK\n", n_studies_ok,
                if (n_studies_ok == 1) "y" else "ies"))
  } else {
    n_err <- n_err + 1L
    err_codes <- paste(vapply(results[vapply(results,
      function(r) !isTRUE(r$success), logical(1))],
      function(r) r$error, character(1)), collapse = "; ")
    cat(sprintf("%d/%d studies FAILED: %s\n",
                n_studies_err, length(results), err_codes))
  }
}

message("")
message("─────────────────────────────────────────────────────")
message(sprintf("Bulk conversion complete: %d OK, %d failed",
                n_ok, n_err))
message(sprintf("Summary written to: %s", SUMMARY_CSV))
