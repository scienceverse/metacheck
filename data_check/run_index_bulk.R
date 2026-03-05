# run_index_bulk.R
# ─────────────────────────────────────────────────────────────────────────────
# Stress-test run_index() across N randomly selected papers.
# Each run picks a random paper_id (same randomisation logic as 0_index.R).
# Results are collected and printed as a diagnostic summary.
# ─────────────────────────────────────────────────────────────────────────────

source("./data_check/0_index.R")

# ── Config ────────────────────────────────────────────────────────────────────

N_RUNS <- 10          # how many papers to run
SEED   <- NULL        # set an integer for reproducibility, or NULL for random

if (!is.null(SEED)) set.seed(SEED)

# ── Run ───────────────────────────────────────────────────────────────────────

results <- vector("list", N_RUNS)

for (i in seq_len(N_RUNS)) {
  cat("\n══════════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Run %d / %d\n", i, N_RUNS))
  cat("══════════════════════════════════════════════════════════════════════\n")

  results[[i]] <- tryCatch(
    run_index(paper_id = NA),
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      list(
        paper_id     = NA_character_,
        success      = FALSE,
        error        = conditionMessage(e),
        elapsed_sec  = NA_real_,
        n_files      = NA_integer_,
        n_data_files = NA_integer_,
        n_agg_dirs   = NA_integer_,
        n_raw        = NA_integer_,
        n_nonraw     = NA_integer_,
        n_columns    = NA_integer_,
        n_source_files = NA_integer_,
        type_counts  = NULL,
        group_counts = NULL,
        file_df      = NULL,
        columns_df   = NULL
      )
    }
  )
}

# ── Build summary table ───────────────────────────────────────────────────────

summary_df <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    paper_id     = if (is.null(r$paper_id) || is.na(r$paper_id)) NA_character_ else r$paper_id,
    success      = r$success,
    elapsed_sec  = round(r$elapsed_sec, 1),
    n_files      = r$n_files,
    n_data_files = r$n_data_files,
    n_agg_dirs   = r$n_agg_dirs,
    n_raw        = r$n_raw,
    n_nonraw     = r$n_nonraw,
    n_columns    = r$n_columns,
    n_src_files  = r$n_source_files,
    stringsAsFactors = FALSE
  )
}))

# ── Print diagnostics ─────────────────────────────────────────────────────────

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                       BULK RUN SUMMARY                              ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

# ── 1. Per-run table ──────────────────────────────────────────────────────────

cat("── Per-run results ────────────────────────────────────────────────────\n")
print(summary_df, row.names = FALSE)

# ── 2. High-level fit stats ───────────────────────────────────────────────────

n_ok   <- sum(summary_df$success)
n_fail <- N_RUNS - n_ok

cat(sprintf("\n── Success rate: %d / %d  (%.0f%%)\n", n_ok, N_RUNS, 100 * n_ok / N_RUNS))

if (n_fail > 0) {
  cat("\n── Failures:\n")
  failed_idx <- which(!summary_df$success)
  for (fi in failed_idx) {
    pid_str <- results[[fi]]$paper_id
    pid_str <- if (is.null(pid_str) || is.na(pid_str)) "unknown" else pid_str
    cat(sprintf("   Run %d — %s\n   Error: %s\n",
                fi, pid_str, results[[fi]]$error))
  }
}

# ── 3. Timing ─────────────────────────────────────────────────────────────────

if (n_ok > 0) {
  ok_times <- summary_df$elapsed_sec[summary_df$success]
  cat(sprintf(
    "\n── Elapsed time (successful runs):\n   mean=%.1fs  median=%.1fs  min=%.1fs  max=%.1fs\n",
    mean(ok_times), median(ok_times), min(ok_times), max(ok_times)
  ))
}

# ── 4. Coverage stats ─────────────────────────────────────────────────────────

ok_rows <- summary_df[summary_df$success, ]

if (nrow(ok_rows) > 0) {
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

  # Papers with zero data files — a potential pipeline miss
  n_no_data <- sum(ok_rows$n_data_files == 0)
  if (n_no_data > 0) {
    cat(sprintf("\n   ⚠  %d paper(s) produced zero data files:\n", n_no_data))
    for (pid in ok_rows$paper_id[ok_rows$n_data_files == 0]) cat("      ", pid, "\n")
  }

  # Papers with zero columns — data files found but nothing read
  n_no_cols <- sum(ok_rows$n_data_files > 0 & ok_rows$n_columns == 0)
  if (n_no_cols > 0) {
    cat(sprintf("   ⚠  %d paper(s) had data files but zero columns extracted:\n", n_no_cols))
    for (pid in ok_rows$paper_id[ok_rows$n_data_files > 0 & ok_rows$n_columns == 0]) {
      cat("      ", pid, "\n")
    }
  }
}

# ── 5. Aggregate type distribution ────────────────────────────────────────────

all_file_dfs <- Filter(Negate(is.null), lapply(results, `[[`, "file_df"))
if (length(all_file_dfs) > 0) {
  all_files <- do.call(rbind, all_file_dfs)
  cat("\n── File type distribution (all successful runs combined):\n")
  type_tbl <- sort(table(all_files$type), decreasing = TRUE)
  for (nm in names(type_tbl)) {
    pct <- 100 * type_tbl[[nm]] / nrow(all_files)
    cat(sprintf("   %-14s %4d  (%.1f%%)\n", nm, type_tbl[[nm]], pct))
  }

  cat("\n── Group distribution (data files only):\n")
  data_only <- all_files[all_files$type == "data", ]
  if (nrow(data_only) > 0) {
    grp_tbl <- sort(table(data_only$group), decreasing = TRUE)
    for (nm in names(grp_tbl)) {
      pct <- 100 * grp_tbl[[nm]] / nrow(data_only)
      cat(sprintf("   %-14s %4d  (%.1f%%)\n", nm, grp_tbl[[nm]], pct))
    }
  }

  # Raw detection rate
  data_nonsentinel <- all_files[all_files$type == "data" & !all_files$is_sentinel, ]
  if (nrow(data_nonsentinel) > 0) {
    pct_raw <- 100 * mean(data_nonsentinel$is_raw, na.rm = TRUE)
    cat(sprintf("\n── Raw file detection rate: %.1f%% of data files flagged as raw\n", pct_raw))
  }
}

# ── 6. Return results invisibly for further inspection ────────────────────────

invisible(results)
