# run_codebook_bulk.R
# ─────────────────────────────────────────────────────────────────────────────
# Run run_codebook_label() across all papers that have outputs/<paper_id>/columns.csv,
# writing results incrementally to codebook_summary.csv so that progress survives
# crashes. On restart, already-completed (or failed) papers are skipped.
# ─────────────────────────────────────────────────────────────────────────────

source("./data_check/2_codebook_label.R")

# ── Config ────────────────────────────────────────────────────────────────────

N_RUNS      <- Inf          # Inf = all eligible papers; set an integer to cap
SEED        <- NULL         # set an integer for reproducibility, or NULL
SUMMARY_CSV <- "./data_check/codebook_summary.csv"

if (!is.null(SEED)) set.seed(SEED)

# ── Discover eligible papers ──────────────────────────────────────────────────
# A paper is eligible if outputs/<paper_id>/columns.csv exists.

outputs_root <- "./data_check/outputs"
all_ids <- basename(list.dirs(outputs_root, recursive = FALSE))
all_ids <- all_ids[all_ids != ""]

eligible_ids <- all_ids[
  file.exists(file.path(outputs_root, all_ids, "columns.csv"))
]

if (length(eligible_ids) == 0) {
  stop("No eligible papers found in ", outputs_root,
       " — run run_index_bulk.R first to produce columns.csv files.")
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

append_summary_row <- function(r, elapsed_sec) {
  row <- data.frame(
    paper_id        = na_fallback(r$paper_id, NA_character_),
    success         = isTRUE(r$success),
    error           = na_fallback(r$error, NA_character_),
    elapsed_ms      = round(na_fallback(elapsed_sec, NA_real_) * 1000),
    n_labelled      = na_fallback(r$n_labelled, NA_integer_),
    n_unlabelled    = na_fallback(r$n_unlabelled, NA_integer_),
    n_codebook_vars = na_fallback(r$n_codebook_vars, NA_integer_),
    n_matched_vars  = na_fallback(r$n_matched_vars, NA_integer_),
    label_status    = na_fallback(r$label_status, NA_character_),
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
    run_codebook_label(paper_id = pid),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      list(
        paper_id        = pid,
        success         = FALSE,
        error           = conditionMessage(e),
        n_labelled      = NA_integer_,
        n_unlabelled    = NA_integer_,
        n_codebook_vars = NA_integer_,
        n_matched_vars  = NA_integer_,
        label_status    = NA_character_
      )
    }
  )

  elapsed <- proc.time()[["elapsed"]] - t_start

  # run_codebook_label() does not set $success or $error — add them for the
  # success path (the error handler above sets them for the failure path).
  if (is.null(result$success)) result$success <- TRUE
  if (is.null(result$error))   result$error   <- NA_character_
  if (is.null(result$paper_id)) result$paper_id <- pid

  append_summary_row(result, elapsed)
}

# ── Print summary ─────────────────────────────────────────────────────────────

summary_df <- read.csv(SUMMARY_CSV, stringsAsFactors = FALSE,
                       colClasses = c(paper_id = "character"))

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                  CODEBOOK LABEL BULK RUN SUMMARY                    ║\n")
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
  statuses <- table(ok_rows$label_status)
  cat("\n── Label status breakdown (successful runs):\n")
  for (s in names(statuses)) {
    cat(sprintf("   %-15s %d\n", s, statuses[[s]]))
  }
  cat(sprintf(
    "\n── Coverage (successful runs):\n   Labelled cols — mean=%.1f  Codebook vars matched — mean=%.1f\n",
    mean(ok_rows$n_labelled, na.rm = TRUE),
    mean(ok_rows$n_matched_vars, na.rm = TRUE)
  ))
}

cat("\n── Results saved to: ", SUMMARY_CSV, "\n")
