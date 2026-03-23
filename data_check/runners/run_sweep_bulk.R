# run_sweep_bulk.R
# ─────────────────────────────────────────────────────────────────────────────
# Bulk temperature sweep runner: runs the full temperature sweep for every
# paper discovered from XML_DIR, writing paper-level progress to a log CSV so
# the run can be paused and resumed.
#
# On restart, already-logged papers are skipped automatically.
# Within a paper, already-completed (temperature, repeat) combinations are
# skipped by the per-paper sweep_log.csv (handled inside run_paper_sweep()).
# ─────────────────────────────────────────────────────────────────────────────

# Set flag so run_sweep.R's !interactive() guard does not trigger main()
RUN_SWEEP_SOURCED_AS_LIB <- TRUE
source("data_check/runners/run_sweep.R")   # provides run_paper_sweep() + helpers
# run_sweep.R's main() sources helper.R, 0_index.R, 2_codebook_label.R at
# runtime; source them explicitly here so XML_DIR is available at top level.
source("data_check/pipeline/helper.R")
source("data_check/pipeline/0_index.R")
source("data_check/pipeline/2_codebook_label.R")

# ── Config ────────────────────────────────────────────────────────────────────

TEMPERATURES <- c(0.0, 0.3, 0.7, 1.0)  # temperatures to sweep
REPEATS      <- 3L                      # repeats per temperature per paper
N_PAPERS     <- Inf                     # Inf = all; set an integer to cap
SWEEP_DIR    <- "./data_check/sweep_results"
BULK_LOG     <- "./data_check/sweep_results/sweep_bulk_log.csv"
SEED         <- NULL                    # integer for reproducible paper order, or NULL

if (!is.null(SEED)) set.seed(SEED)

# ── Discover all papers ───────────────────────────────────────────────────────

all_ids <- tools::file_path_sans_ext(
  list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
)
if (length(all_ids) == 0) stop("No XML files found in ", XML_DIR)

# ── Bulk log helpers ──────────────────────────────────────────────────────────

load_bulk_log <- function(path) {
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character"))
  } else {
    data.frame(
      paper_id     = character(0),
      temperatures = character(0),
      repeats      = integer(0),
      n_ok         = integer(0),
      n_no_data    = integer(0),
      n_failed     = integer(0),
      n_skipped    = integer(0),
      elapsed_ms   = integer(0),
      timestamp    = character(0),
      status       = character(0),
      stringsAsFactors = FALSE
    )
  }
}

append_bulk_log <- function(path, row) {
  write.table(
    row,
    file      = path,
    sep       = ",",
    append    = TRUE,
    col.names = !file.exists(path),
    row.names = FALSE,
    quote     = TRUE
  )
}

# ── Load prior progress ───────────────────────────────────────────────────────

dir.create(SWEEP_DIR, recursive = TRUE, showWarnings = FALSE)
bulk_log <- load_bulk_log(BULK_LOG)

done_ids <- unique(as.character(bulk_log$paper_id))
if (length(done_ids) > 0)
  message("── Resuming: ", length(done_ids), " paper(s) already swept, skipping")

# ── Determine papers to run ───────────────────────────────────────────────────

remaining_ids <- setdiff(all_ids, done_ids)
if (!is.null(SEED)) remaining_ids <- sample(remaining_ids)
if (is.finite(N_PAPERS) && N_PAPERS < length(remaining_ids))
  remaining_ids <- remaining_ids[seq_len(N_PAPERS)]

n_total <- length(remaining_ids)
n_prior <- length(done_ids)

if (n_total == 0) {
  message("── Nothing to do — all papers already logged.")
  q(save = "no")
}

message("── Will process ", n_total, " paper(s)  (", n_prior, " already done)")

# ── Run ───────────────────────────────────────────────────────────────────────

bulk_start <- proc.time()[["elapsed"]]

for (i in seq_along(remaining_ids)) {
  pid <- remaining_ids[[i]]

  cat("\n══════════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Paper %d / %d  (overall %d / %d)  —  %s\n",
              i, n_total, n_prior + i, n_prior + n_total, pid))
  cat("══════════════════════════════════════════════════════════════════════\n")

  t_start   <- proc.time()[["elapsed"]]
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  sweep_result <- tryCatch(
    run_paper_sweep(pid, TEMPERATURES, REPEATS, SWEEP_DIR),
    error = function(e) {
      message("  PAPER FAILED: ", conditionMessage(e))
      list(n_ok = 0L, n_no_data = 0L,
           n_failed  = length(TEMPERATURES) * REPEATS,
           n_skipped = 0L)
    }
  )

  elapsed_ms <- as.integer(round((proc.time()[["elapsed"]] - t_start) * 1000))

  log_row <- data.frame(
    paper_id     = pid,
    temperatures = paste(TEMPERATURES, collapse = ","),
    repeats      = REPEATS,
    n_ok         = sweep_result$n_ok,
    n_no_data    = if (is.null(sweep_result$n_no_data)) 0L else sweep_result$n_no_data,
    n_failed     = sweep_result$n_failed,
    n_skipped    = sweep_result$n_skipped,
    elapsed_ms   = elapsed_ms,
    timestamp    = timestamp,
    status       = "done",
    stringsAsFactors = FALSE
  )
  append_bulk_log(BULK_LOG, log_row)

  cat(sprintf("\n  [%d/%d] %s — ok=%d  no_data=%d  failed=%d  skipped=%d  (%ds)\n",
              i, n_total, pid,
              log_row$n_ok, log_row$n_no_data,
              log_row$n_failed, log_row$n_skipped,
              round(elapsed_ms / 1000)))
}

# ── Summary ───────────────────────────────────────────────────────────────────

total_elapsed <- round((proc.time()[["elapsed"]] - bulk_start) / 60, 1)

final_log <- tryCatch(
  read.csv(BULK_LOG, stringsAsFactors = FALSE,
           colClasses = c(paper_id = "character")),
  error = function(e) NULL
)

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    BULK SWEEP SUMMARY                               ║\n")
cat("╚══════════════════════════════════════════════════════════════════════╝\n\n")

if (!is.null(final_log) && nrow(final_log) > 0) {
  total_ok      <- sum(final_log$n_ok,      na.rm = TRUE)
  total_no_data <- if ("n_no_data" %in% names(final_log))
                     sum(final_log$n_no_data, na.rm = TRUE) else 0L
  total_failed  <- sum(final_log$n_failed,  na.rm = TRUE)
  total_skipped <- sum(final_log$n_skipped, na.rm = TRUE)
  cat(sprintf("── Papers logged : %d\n", nrow(final_log)))
  cat(sprintf("── Runs ok       : %d\n", total_ok))
  cat(sprintf("── Runs no_data  : %d\n", total_no_data))
  cat(sprintf("── Runs failed   : %d\n", total_failed))
  cat(sprintf("── Runs skipped  : %d\n", total_skipped))
}
cat(sprintf("── Total elapsed : %.1f min\n", total_elapsed))
cat("── Bulk log      :", BULK_LOG, "\n")
