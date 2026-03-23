# report_sweep_grand.R
# ─────────────────────────────────────────────────────────────────────────────
# Grand stability and quality report across all swept papers.
#
# Produces a flat CSV with one row per (paper_id × temperature × pipeline stage)
# for flexible post-processing. No aggregation is applied — consumers aggregate.
#
# Usage:
#   Rscript reports/report_sweep_grand.R \
#     [--sweep-dir ./sweep_results] \
#     [--out-csv   ./results/sweep_grand_report.csv]
#
# Output columns (FR-015):
#   paper_id, temperature, stage, repeat_count,
#   mean_pairwise_agreement, known_type_rate,
#   codebook_coverage_rate, nonempty_label_rate, status
# ─────────────────────────────────────────────────────────────────────────────

# ── Sources ───────────────────────────────────────────────────────────────────
# report_sweep.R provides: compute_stability(), compute_quality(), loaders
# 0_index.R provides: XML_DIR constant

# Set flag so report_sweep.R's !interactive() guard does not trigger main()
REPORT_SWEEP_SOURCED_AS_LIB <- TRUE
source("data_check/reports/report_sweep.R")
source("data_check/pipeline/helper.R")
source("data_check/pipeline/0_index.R")

# ── Argument parsing ──────────────────────────────────────────────────────────

parse_grand_args <- function() {
  args      <- commandArgs(trailingOnly = TRUE)
  sweep_dir <- "./data_check/sweep_results"
  out_csv   <- "./data_check/results/sweep_grand_report.csv"

  i <- 1L
  while (i <= length(args)) {
    switch(args[i],
      "--sweep-dir" = { i <- i + 1L; sweep_dir <- args[i] },
      "--out-csv"   = { i <- i + 1L; out_csv   <- args[i] }
    )
    i <- i + 1L
  }

  if (!dir.exists(sweep_dir))
    stop("--sweep-dir does not exist: ", sweep_dir)

  out_parent <- dirname(out_csv)
  if (!dir.exists(out_parent))
    stop("Parent directory of --out-csv does not exist: ", out_parent)

  list(sweep_dir = sweep_dir, out_csv = out_csv)
}

# ── Row builder ───────────────────────────────────────────────────────────────

# Build two rows (index stage + codebook stage) for one (paper × temperature).
# temp_log_rows: rows from sweep_log.csv for this (paper × temperature).
build_stage_rows <- function(paper_id, temp, stab_row, qual_row, temp_log_rows) {
  repeat_count <- if (!is.null(qual_row)) qual_row$n_repeats_used else NA_integer_

  # Detect no_data: all runs for this temperature ended with status="no_data"
  all_no_data <- !is.null(temp_log_rows) &&
    nrow(temp_log_rows) > 0 &&
    all(temp_log_rows$status == "no_data")

  if (all_no_data) {
    # Both stage rows: all metrics NA, status="no_data"
    base <- data.frame(
      paper_id                = paper_id,
      temperature             = temp,
      stage                   = NA_character_,
      repeat_count            = 0L,
      mean_pairwise_agreement = NA_real_,
      known_type_rate         = NA_real_,
      codebook_coverage_rate  = NA_real_,
      nonempty_label_rate     = NA_real_,
      status                  = "no_data",
      stringsAsFactors        = FALSE
    )
    index_row    <- base; index_row$stage    <- "index"
    codebook_row <- base; codebook_row$stage <- "codebook"
    return(rbind(index_row, codebook_row))
  }

  # index stage
  ct_agree <- if (!is.null(stab_row)) stab_row$col_type_agreement else NA_real_
  index_row <- data.frame(
    paper_id                = paper_id,
    temperature             = temp,
    stage                   = "index",
    repeat_count            = repeat_count,
    mean_pairwise_agreement = ct_agree,
    known_type_rate         = if (!is.null(qual_row)) qual_row$known_type_rate else NA_real_,
    codebook_coverage_rate  = NA_real_,
    nonempty_label_rate     = NA_real_,
    status                  = if (!is.na(ct_agree)) "ok" else "failed",
    stringsAsFactors        = FALSE
  )

  # codebook stage
  lb_agree <- if (!is.null(stab_row)) stab_row$label_agreement else NA_real_
  cov_rate <- if (!is.null(qual_row)) qual_row$codebook_coverage_rate else NA_real_
  lb_rate  <- if (!is.null(qual_row)) qual_row$nonempty_label_rate    else NA_real_
  codebook_row <- data.frame(
    paper_id                = paper_id,
    temperature             = temp,
    stage                   = "codebook",
    repeat_count            = repeat_count,
    mean_pairwise_agreement = lb_agree,
    known_type_rate         = NA_real_,
    codebook_coverage_rate  = cov_rate,
    nonempty_label_rate     = lb_rate,
    status                  = if (is.na(lb_agree) && is.na(cov_rate)) "no_codebook" else "ok",
    stringsAsFactors        = FALSE
  )

  rbind(index_row, codebook_row)
}

# ── Main ──────────────────────────────────────────────────────────────────────

main <- function() {
  args <- parse_grand_args()

  cat("── Grand sweep report ───────────────────────────────────────────────────\n")
  cat("   sweep_dir :", args$sweep_dir, "\n")
  cat("   out_csv   :", args$out_csv,   "\n\n")

  # Discover all paper IDs from XML_DIR
  all_ids <- tools::file_path_sans_ext(
    list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
  )
  if (length(all_ids) == 0) stop("No XML files found in ", XML_DIR)

  cat(sprintf("── %d paper(s) in XML_DIR\n", length(all_ids)))

  all_rows      <- list()
  n_swept       <- 0L
  n_no_sweep    <- 0L
  row_idx       <- 0L

  for (pid in all_ids) {
    log_path <- file.path(args$sweep_dir, pid, "sweep_log.csv")

    if (!file.exists(log_path)) {
      n_no_sweep <- n_no_sweep + 1L
      next
    }

    sweep_log <- tryCatch(
      read.csv(log_path, stringsAsFactors = FALSE,
               colClasses = c(paper_id = "character")),
      error = function(e) {
        warning("Could not read sweep_log.csv for paper ", pid, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(sweep_log) || nrow(sweep_log) == 0) {
      n_no_sweep <- n_no_sweep + 1L
      next
    }

    n_swept <- n_swept + 1L

    stab_df <- tryCatch(
      compute_stability(sweep_log, file.path(args$sweep_dir, pid)),
      error = function(e) { warning("stability failed for ", pid, ": ", conditionMessage(e)); NULL }
    )
    qual_df <- tryCatch(
      compute_quality(sweep_log, file.path(args$sweep_dir, pid)),
      error = function(e) { warning("quality failed for ", pid, ": ", conditionMessage(e)); NULL }
    )

    # Collect all temperatures from either data frame
    temps <- sort(unique(c(
      if (!is.null(stab_df)) stab_df$temperature else numeric(0),
      if (!is.null(qual_df)) qual_df$temperature else numeric(0)
    )))

    for (temp in temps) {
      stab_row <- if (!is.null(stab_df)) {
        row <- stab_df[stab_df$temperature == temp, , drop = FALSE]
        if (nrow(row) > 0) as.list(row[1, ]) else NULL
      } else NULL

      qual_row <- if (!is.null(qual_df)) {
        row <- qual_df[qual_df$temperature == temp, , drop = FALSE]
        if (nrow(row) > 0) as.list(row[1, ]) else NULL
      } else NULL

      temp_log_rows <- sweep_log[sweep_log$temperature == temp, , drop = FALSE]

      rows <- build_stage_rows(pid, temp, stab_row, qual_row, temp_log_rows)
      row_idx <- row_idx + 1L
      all_rows[[row_idx]] <- rows
    }
  }

  if (length(all_rows) == 0) {
    cat("\n── No swept papers found. Run run_sweep_bulk.R first.\n")
    return(invisible(NULL))
  }

  grand_df <- do.call(rbind, all_rows)
  # Ensure paper_id remains character after rbind
  grand_df$paper_id <- as.character(grand_df$paper_id)

  write.csv(grand_df, args$out_csv, row.names = FALSE)

  cat(sprintf("\n── Summary ──────────────────────────────────────────────────────────────\n"))
  cat(sprintf("   Papers in XML_DIR : %d\n", length(all_ids)))
  cat(sprintf("   Papers swept      : %d\n", n_swept))
  cat(sprintf("   Papers not swept  : %d\n", n_no_sweep))
  cat(sprintf("   Total rows written: %d\n", nrow(grand_df)))
  cat(sprintf("   Output CSV        : %s\n", args$out_csv))
}

if (!interactive()) main()
