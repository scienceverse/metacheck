# run_label_bulk.R
# ─────────────────────────────────────────────────────────────────────────────
# Run run_data_label() across all papers that have outputs/<paper_id>/structure.csv,
# writing results incrementally to label_summary.csv so that progress survives
# crashes. On restart, already-completed (or failed) papers are skipped.
# ─────────────────────────────────────────────────────────────────────────────

source("./data_check/1_data_label.R")

# ── Config ────────────────────────────────────────────────────────────────────

N_RUNS      <- Inf          # Inf = all eligible papers; set an integer to cap
SEED        <- NULL         # set an integer for reproducibility, or NULL
SUMMARY_CSV <- "./data_check/label_summary.csv"

if (!is.null(SEED)) set.seed(SEED)

# ── Discover eligible papers ──────────────────────────────────────────────────
# A paper is eligible if outputs/<paper_id>/structure.csv exists.

outputs_root <- "./data_check/outputs"
all_ids <- basename(list.dirs(outputs_root, recursive = FALSE))
all_ids <- all_ids[all_ids != ""]

eligible_ids <- all_ids[
  file.exists(file.path(outputs_root, all_ids, "structure.csv"))
]

if (length(eligible_ids) == 0) {
  stop("No eligible papers found in ", outputs_root,
       " — run run_index_bulk.R first.")
}

# ── Load prior progress ───────────────────────────────────────────────────────

done_ids <- character(0)
if (file.exists(SUMMARY_CSV)) {
  prior <- tryCatch(
    read.csv(SUMMARY_CSV, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) NULL
  )
  if (!is.null(prior) && "paper_id" %in% names(prior)) {
    done_ids <- unique(as.character(prior$paper_id))
    message("── Resuming: ", length(done_ids),
            " paper(s) already processed, skipping")
  }
}

# ── Determine papers to run ───────────────────────────────────────────────────

remaining_ids <- setdiff(eligible_ids, done_ids)
if (!is.null(SEED)) remaining_ids <- sample(remaining_ids)
if (is.finite(N_RUNS) && N_RUNS < length(remaining_ids)) {
  remaining_ids <- remaining_ids[seq_len(N_RUNS)]
}

n_total <- length(remaining_ids)
n_prior <- length(done_ids)

if (n_total == 0) {
  message("── Nothing to do — all eligible papers already processed.")
  q(save = "no")
}

message("── Will process ", n_total, " paper(s) (",
        length(eligible_ids), " eligible, ", n_prior, " already done)")

# ── Helper: append one row to the summary CSV ─────────────────────────────────

na_fallback <- function(x, na = NA) if (is.null(x) || length(x) == 0) na else x

append_summary_row <- function(r) {
  row <- data.frame(
    paper_id     = na_fallback(r$paper_id, NA_character_),
    success      = r$success,
    error        = na_fallback(r$error, NA_character_),
    elapsed_ms   = round(na_fallback(r$elapsed_sec, NA_real_) * 1000),
    n_data_files = na_fallback(r$n_data_files, NA_integer_),
    n_columns    = na_fallback(r$n_columns, NA_integer_),
    stringsAsFactors = FALSE
  )
  write_header <- !file.exists(SUMMARY_CSV)
  write.table(row, SUMMARY_CSV, append = TRUE, sep = ",",
              row.names = FALSE, col.names = write_header)
}

# ── Run ───────────────────────────────────────────────────────────────────────

for (i in seq_along(remaining_ids)) {
  pid <- remaining_ids[i]

  # Guard: re-read CSV in case a prior iteration already covered this ID
  if (file.exists(SUMMARY_CSV)) {
    already <- tryCatch(
      read.csv(SUMMARY_CSV, stringsAsFactors = FALSE,
               colClasses = c(paper_id = "character")),
      error = function(e) NULL
    )
    if (!is.null(already) && pid %in% already$paper_id) {
      message("  skipping (already in CSV): ", pid)
      next
    }
  }

  cat("\n══════════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Run %d / %d  (overall %d / %d)  —  %s\n",
              i, n_total, n_prior + i, n_prior + n_total, pid))
  cat("══════════════════════════════════════════════════════════════════════\n")

  t_start <- proc.time()[["elapsed"]]

  result <- tryCatch(
    run_data_label(paper_id = pid),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      list(
        paper_id     = pid,
        success      = FALSE,
        error        = conditionMessage(e),
        elapsed_sec  = proc.time()[["elapsed"]] - t_start,
        n_data_files = NA_integer_,
        n_columns    = NA_integer_
      )
    }
  )

  append_summary_row(result)
}

# ── Print summary ─────────────────────────────────────────────────────────────

summary_df <- read.csv(SUMMARY_CSV, stringsAsFactors = FALSE,
                       colClasses = c(paper_id = "character"))

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    DATA LABEL BULK RUN SUMMARY                      ║\n")
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
  ok_rows <- summary_df[summary_df$success, ]
  cat(sprintf(
    "\n── Columns extracted (successful runs):\n   mean=%.1f  median=%.1f  min=%d  max=%d\n",
    mean(ok_rows$n_columns), median(ok_rows$n_columns),
    min(ok_rows$n_columns), max(ok_rows$n_columns)
  ))
}

cat("\n── Results saved to: ", SUMMARY_CSV, "\n")
