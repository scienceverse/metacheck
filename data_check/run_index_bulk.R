# run_index_bulk.R
# ─────────────────────────────────────────────────────────────────────────────
# Run run_index() across all (or N) papers, writing results incrementally
# to a CSV so that progress survives crashes. On restart, already-completed
# papers are skipped automatically.
# ─────────────────────────────────────────────────────────────────────────────

source("./data_check/0_index.R")

# ── Config ────────────────────────────────────────────────────────────────────

N_RUNS      <- Inf          # Inf = all papers; set an integer to cap
SEED        <- NULL         # set an integer for reproducibility, or NULL
SUMMARY_CSV <- "./data_check/bulk_summary.csv"

if (!is.null(SEED)) set.seed(SEED)

# ── Discover all papers ──────────────────────────────────────────────────────

all_ids <- tools::file_path_sans_ext(
  list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
)
if (length(all_ids) == 0) stop("No XML files found in ", XML_DIR)

# ── Load prior progress ─────────────────────────────────────────────────────

done_ids <- character(0)
if (file.exists(SUMMARY_CSV)) {
  prior <- read.csv(SUMMARY_CSV, stringsAsFactors = FALSE)
  done_ids <- prior$paper_id
  message("── Resuming: ", length(done_ids), " paper(s) already completed, skipping")
}

# ── Determine papers to run ─────────────────────────────────────────────────

remaining_ids <- setdiff(all_ids, done_ids)
if (!is.null(SEED)) {
  remaining_ids <- sample(remaining_ids)
}
if (is.finite(N_RUNS) && N_RUNS < length(remaining_ids)) {
  remaining_ids <- remaining_ids[seq_len(N_RUNS)]
}

n_total    <- length(remaining_ids)
n_prior    <- length(done_ids)

if (n_total == 0) {
  message("── Nothing to do — all papers already processed.")
  q(save = "no")
}

message("── Will process ", n_total, " paper(s)")

# ── Helper: append one row to the summary CSV ───────────────────────────────

na_fallback <- function(x, na = NA) if (is.null(x) || length(x) == 0) na else x

append_summary_row <- function(r) {
  row <- data.frame(
    paper_id     = na_fallback(r$paper_id, NA_character_),
    success      = r$success,
    error        = na_fallback(r$error, NA_character_),
    elapsed_sec  = round(na_fallback(r$elapsed_sec, NA_real_), 1),
    n_files      = na_fallback(r$n_files, NA_integer_),
    n_data_files = na_fallback(r$n_data_files, NA_integer_),
    n_agg_dirs   = na_fallback(r$n_agg_dirs, NA_integer_),
    n_raw        = na_fallback(r$n_raw, NA_integer_),
    n_nonraw     = na_fallback(r$n_nonraw, NA_integer_),
    n_columns    = na_fallback(r$n_columns, NA_integer_),
    n_src_files  = na_fallback(r$n_source_files, NA_integer_),
    stringsAsFactors = FALSE
  )
  write_header <- !file.exists(SUMMARY_CSV)
  write.table(row, SUMMARY_CSV, append = TRUE, sep = ",",
              row.names = FALSE, col.names = write_header)
}

# ── Run ──────────────────────────────────────────────────────────────────────

for (i in seq_along(remaining_ids)) {
  pid <- remaining_ids[i]

  cat("\n══════════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Run %d / %d  (overall %d / %d)  —  %s\n",
              i, n_total, n_prior + i, n_prior + n_total, pid))
  cat("══════════════════════════════════════════════════════════════════════\n")

  result <- tryCatch(
    run_index(paper_id = pid),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      list(
        paper_id       = pid,
        success        = FALSE,
        error          = conditionMessage(e),
        elapsed_sec    = NA_real_,
        n_files        = NA_integer_,
        n_data_files   = NA_integer_,
        n_agg_dirs     = NA_integer_,
        n_raw          = NA_integer_,
        n_nonraw       = NA_integer_,
        n_columns      = NA_integer_,
        n_source_files = NA_integer_
      )
    }
  )

  append_summary_row(result)
}

# ── Print summary ────────────────────────────────────────────────────────────

summary_df <- read.csv(SUMMARY_CSV, stringsAsFactors = FALSE)

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                       BULK RUN SUMMARY                              ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

cat("── Per-run results ────────────────────────────────────────────────────\n")
print(summary_df[, setdiff(names(summary_df), "error")], row.names = FALSE)

n_ok   <- sum(summary_df$success)
n_fail <- nrow(summary_df) - n_ok

cat(sprintf("\n── Success rate: %d / %d  (%.0f%%)\n",
            n_ok, nrow(summary_df), 100 * n_ok / nrow(summary_df)))

if (n_fail > 0) {
  cat("\n── Failures:\n")
  fails <- summary_df[!summary_df$success, ]
  for (j in seq_len(nrow(fails))) {
    cat(sprintf("   %s — %s\n", fails$paper_id[j], fails$error[j]))
  }
}

if (n_ok > 0) {
  ok_times <- summary_df$elapsed_sec[summary_df$success]
  cat(sprintf(
    "\n── Elapsed time (successful runs):\n   mean=%.1fs  median=%.1fs  min=%.1fs  max=%.1fs\n",
    mean(ok_times), median(ok_times), min(ok_times), max(ok_times)
  ))

  ok_rows <- summary_df[summary_df$success, ]
  cat("\n── Coverage (successful runs):\n")
  cat(sprintf("   Files per paper      — mean=%.1f  median=%.1f  range=[%d,%d]\n",
              mean(ok_rows$n_files),      median(ok_rows$n_files),
              min(ok_rows$n_files),       max(ok_rows$n_files)))
  cat(sprintf("   Data files per paper — mean=%.1f  median=%.1f  range=[%d,%d]\n",
              mean(ok_rows$n_data_files), median(ok_rows$n_data_files),
              min(ok_rows$n_data_files),  max(ok_rows$n_data_files)))
  cat(sprintf("   Columns extracted    — mean=%.1f  median=%.1f  range=[%d,%d]\n",
              mean(ok_rows$n_columns),    median(ok_rows$n_columns),
              min(ok_rows$n_columns),     max(ok_rows$n_columns)))

  n_no_data <- sum(ok_rows$n_data_files == 0)
  if (n_no_data > 0) {
    cat(sprintf("\n   ⚠  %d paper(s) produced zero data files:\n", n_no_data))
    for (pid in ok_rows$paper_id[ok_rows$n_data_files == 0]) cat("      ", pid, "\n")
  }

  n_no_cols <- sum(ok_rows$n_data_files > 0 & ok_rows$n_columns == 0)
  if (n_no_cols > 0) {
    cat(sprintf("   ⚠  %d paper(s) had data files but zero columns extracted:\n", n_no_cols))
    for (pid in ok_rows$paper_id[ok_rows$n_data_files > 0 & ok_rows$n_columns == 0]) {
      cat("      ", pid, "\n")
    }
  }
}

cat("\n── Results saved to: ", SUMMARY_CSV, "\n")
