# report_quality.R — Pipeline Quality Insights Report (feature 016)
#
# Usage: Rscript report_quality.R [options]
# See specs/016-pipeline-quality-report/contracts/cli.md for full contract.
#
# Always writes quality_report_YYYY-MM-DD.md to the working directory.
#
# Options:
#   --bulk              PATH   bulk_summary.csv path          [./bulk_summary.csv]
#   --codebook          PATH   codebook_summary.csv path      [./codebook_summary.csv]
#   --outputs-dir       PATH   per-paper outputs root         [./outputs]
#   --unknown-threshold INT    % unknown to flag as outlier   [30]
#   --top-n             INT    rows in performance lists      [10]
#   --sections          CSV    bulk,coltypes,codebook,timing  [all]

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

parse_args <- function() {
  argv <- commandArgs(trailingOnly = TRUE)
  result <- list(
    bulk              = "./bulk_summary.csv",
    codebook          = "./codebook_summary.csv",
    outputs_dir       = "./outputs",
    unknown_threshold = 30L,
    top_n             = 10L,
    sections          = "all"
  )
  i <- 1L
  while (i <= length(argv)) {
    switch(argv[i],
      "--bulk"              = { result$bulk              <- argv[i + 1L]; i <- i + 2L },
      "--codebook"          = { result$codebook          <- argv[i + 1L]; i <- i + 2L },
      "--outputs-dir"       = { result$outputs_dir       <- argv[i + 1L]; i <- i + 2L },
      "--unknown-threshold" = { result$unknown_threshold <- as.integer(argv[i + 1L]); i <- i + 2L },
      "--top-n"             = { result$top_n             <- as.integer(argv[i + 1L]); i <- i + 2L },
      "--sections"          = { result$sections          <- argv[i + 1L]; i <- i + 2L },
      { i <- i + 1L }
    )
  }
  result
}

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

print_section_header <- function(title) {
  cat(sprintf("\n=== %s ===\n\n", title))
}

warn <- function(...) message("[WARN] ", ...)

# ---------------------------------------------------------------------------
# CSV loaders (all enforce paper_id as character — constitution Principle II)
# ---------------------------------------------------------------------------

load_bulk <- function(path) {
  if (!file.exists(path)) {
    warn("bulk_summary.csv not found at: ", path)
    return(NULL)
  }
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) { warn("Failed to read ", path, ": ", conditionMessage(e)); NULL }
  )
  if (is.null(df)) return(NULL)
  dupes <- unique(df$paper_id[duplicated(df$paper_id)])
  if (length(dupes) > 0) {
    warn(length(dupes), " paper_id(s) appear more than once in bulk_summary.csv; using last occurrence")
    df <- df[!duplicated(df$paper_id, fromLast = TRUE), ]
  }
  df
}

load_codebook_summary <- function(path) {
  if (!file.exists(path)) {
    warn("codebook_summary.csv not found at: ", path)
    return(NULL)
  }
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")),
    error = function(e) { warn("Failed to read ", path, ": ", conditionMessage(e)); NULL }
  )
  if (is.null(df)) return(NULL)
  dupes <- unique(df$paper_id[duplicated(df$paper_id)])
  if (length(dupes) > 0) {
    warn(length(dupes), " paper_id(s) appear more than once in codebook_summary.csv; using last occurrence")
    df <- df[!duplicated(df$paper_id, fromLast = TRUE), ]
  }
  df
}

load_all_columns <- function(outputs_dir) {
  if (!dir.exists(outputs_dir)) {
    warn("outputs directory not found: ", outputs_dir)
    return(NULL)
  }
  paths <- list.files(outputs_dir, pattern = "columns\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  if (length(paths) == 0) {
    warn("No columns.csv files found under ", outputs_dir)
    return(NULL)
  }
  frames <- lapply(paths, function(p) {
    tryCatch(
      read.csv(p, stringsAsFactors = FALSE,
               colClasses = c(paper_id = "character")),
      error = function(e) { warn("Failed to read ", p, ": ", conditionMessage(e)); NULL }
    )
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0) return(NULL)
  do.call(rbind, frames)
}

load_all_coverage <- function(outputs_dir) {
  if (!dir.exists(outputs_dir)) {
    warn("outputs directory not found: ", outputs_dir)
    return(NULL)
  }
  paths <- list.files(outputs_dir, pattern = "codebook_coverage\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  if (length(paths) == 0) {
    warn("No codebook_coverage.csv files found under ", outputs_dir)
    return(NULL)
  }
  frames <- lapply(paths, function(p) {
    tryCatch(
      read.csv(p, stringsAsFactors = FALSE,
               colClasses = c(paper_id = "character")),
      error = function(e) { warn("Failed to read ", p, ": ", conditionMessage(e)); NULL }
    )
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0) return(NULL)
  do.call(rbind, frames)
}

# ---------------------------------------------------------------------------
# Report sections
# ---------------------------------------------------------------------------

section_bulk_overview <- function(bulk_df) {
  print_section_header("Bulk Run Overview")

  n_total   <- nrow(bulk_df)
  n_success <- sum(bulk_df$success == TRUE, na.rm = TRUE)
  n_failed  <- n_total - n_success
  pct_ok    <- if (n_total > 0) n_success / n_total * 100 else 0

  cat(sprintf("Total papers:    %d\n", n_total))
  cat(sprintf("  Successful:    %d  (%.1f%%)\n", n_success, pct_ok))
  cat(sprintf("  Failed:        %d  (%.1f%%)\n\n", n_failed, 100 - pct_ok))

  if (n_failed > 0) {
    failed <- bulk_df[!(bulk_df$success == TRUE & !is.na(bulk_df$success)), ]
    failed <- failed[!is.na(failed$error), ]
    if (nrow(failed) > 0) {
      failed$error_code <- sub("^([a-z_]+):.*", "\\1", failed$error)
      tbl <- sort(table(failed$error_code), decreasing = TRUE)
      cat("Failure breakdown:\n")
      for (nm in names(tbl)) {
        cnt <- as.integer(tbl[[nm]])
        pct <- cnt / n_failed * 100
        cat(sprintf("  %-20s: %4d (%5.1f%% of failures)\n", nm, cnt, pct))
      }
      cat("\n")
    }
  }

  succ <- bulk_df[bulk_df$success == TRUE & !is.na(bulk_df$success), ]
  if (nrow(succ) > 0) {
    tcols <- c("elapsed_ms", "download_ms", "llm_ms", "column_ms")
    tcols <- tcols[tcols %in% names(succ)]
    cat("Timing (successful papers only):\n")
    cat(sprintf("  %-15s %10s %10s %10s\n", "", "mean", "median", "max"))
    for (col in tcols) {
      vals <- succ[[col]][!is.na(succ[[col]])]
      if (length(vals) == 0) {
        cat(sprintf("  %-15s %10s %10s %10s\n", col, "NA", "NA", "NA"))
      } else {
        cat(sprintf("  %-15s %10.0f %10.0f %10.0f\n",
                    col, mean(vals), median(vals), max(vals)))
      }
    }
    cat("\n")
  }
}

KNOWN_COL_TYPES <- c("continuous", "binary", "categorical", "id",
                     "text", "date", "unknown")

section_col_type_dist <- function(columns_df, bulk_df, unknown_threshold) {
  print_section_header("Column-Type Distribution")

  n_cols <- nrow(columns_df)
  cat(sprintf("Total columns across all papers: %d\n\n", n_cols))

  ct  <- ifelse(columns_df$col_type %in% KNOWN_COL_TYPES, columns_df$col_type, "other")
  tbl <- sort(table(ct), decreasing = TRUE)
  cat(sprintf("  %-15s %8s  %8s\n", "col_type", "count", "percent"))
  for (nm in names(tbl)) {
    cnt <- as.integer(tbl[[nm]])
    cat(sprintf("  %-15s %8d  %7.1f%%\n", nm, cnt, cnt / n_cols * 100))
  }
  cat("\n")

  paper_ids <- unique(columns_df$paper_id)
  rates_df  <- data.frame(
    paper_id     = paper_ids,
    unknown_rate = vapply(paper_ids, function(pid) {
      rows <- columns_df[columns_df$paper_id == pid, ]
      if (nrow(rows) == 0) return(0)
      sum(rows$col_type == "unknown", na.rm = TRUE) / nrow(rows) * 100
    }, numeric(1)),
    n_cols = vapply(paper_ids, function(pid)
      sum(columns_df$paper_id == pid), integer(1)),
    stringsAsFactors = FALSE
  )

  high <- rates_df[rates_df$unknown_rate > unknown_threshold, ]
  high <- high[order(high$unknown_rate, decreasing = TRUE), ]

  cat(sprintf("High unknown-rate papers (>%d%% unknown):\n", unknown_threshold))
  if (nrow(high) == 0) {
    cat("  None\n")
  } else {
    cat(sprintf("  %-25s %14s %12s\n", "paper_id", "unknown_rate", "total_cols"))
    for (i in seq_len(nrow(high))) {
      cat(sprintf("  %-25s %13.1f%% %12d\n",
                  high$paper_id[i], high$unknown_rate[i], high$n_cols[i]))
    }
  }
  cat("\n")

  if (!is.null(bulk_df)) {
    succ_pids <- bulk_df$paper_id[bulk_df$success == TRUE & !is.na(bulk_df$success)]
    zero_pids <- succ_pids[!succ_pids %in% unique(columns_df$paper_id)]
    cat(sprintf("Zero-column papers: %d\n", length(zero_pids)))
    for (pid in zero_pids) cat(sprintf("  %s  (no columns.csv found)\n", pid))
    if (length(zero_pids) > 0) cat("\n")
  }
}

section_codebook_coverage <- function(coverage_df, codebook_summary_df, top_n) {
  print_section_header("Codebook Coverage")

  if (!is.null(codebook_summary_df) && nrow(codebook_summary_df) > 0) {
    n_att  <- nrow(codebook_summary_df)
    n_ok   <- sum(codebook_summary_df$label_status == "ok",          na.rm = TRUE)
    n_nocb <- sum(codebook_summary_df$label_status == "no_codebook", na.rm = TRUE)
    cat(sprintf("Papers with codebook labelling attempted: %d\n", n_att))
    cat(sprintf("  Labelling succeeded:     %d (%5.1f%%)\n", n_ok,   n_ok  / n_att * 100))
    cat(sprintf("  No codebook found:       %d (%5.1f%%)\n\n", n_nocb, n_nocb / n_att * 100))
  }

  if (is.null(coverage_df) || nrow(coverage_df) == 0) {
    cat("No codebook_coverage.csv data available.\n\n")
    return()
  }

  # Per-paper coverage: absent file = N/A (not 0%); present-but-empty = 0%
  paper_ids <- unique(coverage_df$paper_id)
  cov_df <- data.frame(
    paper_id  = paper_ids,
    n_matched = vapply(paper_ids, function(pid) {
      sum(coverage_df$match_status[coverage_df$paper_id == pid] == "matched", na.rm = TRUE)
    }, integer(1)),
    n_total   = vapply(paper_ids, function(pid)
      sum(coverage_df$paper_id == pid), integer(1)),
    stringsAsFactors = FALSE
  )
  cov_df$coverage_pct <- cov_df$n_matched / cov_df$n_total * 100

  rates <- cov_df$coverage_pct
  cat("Coverage rate per paper (matched/total columns):\n")
  cat(sprintf("  Overall mean: %.1f%%\n", mean(rates)))
  cat(sprintf("  Median:       %.1f%%\n", median(rates)))
  cat(sprintf("  Min:          %.1f%%\n", min(rates)))
  cat(sprintf("  Max:          %.1f%%\n\n", max(rates)))

  bottom <- cov_df[order(cov_df$coverage_pct), ]
  n_show <- min(top_n, nrow(bottom))
  cat(sprintf("Lowest-coverage papers (bottom %d):\n", n_show))
  cat(sprintf("  %-25s %10s %10s %10s\n", "paper_id", "coverage", "n_matched", "n_total"))
  for (i in seq_len(n_show)) {
    cat(sprintf("  %-25s %9.1f%% %10d %10d\n",
                bottom$paper_id[i], bottom$coverage_pct[i],
                bottom$n_matched[i], bottom$n_total[i]))
  }
  cat("\n")
}

section_timing <- function(bulk_df, top_n) {
  print_section_header(sprintf("Performance: Top-%d Slowest Papers", top_n))

  succ   <- bulk_df[bulk_df$success == TRUE & !is.na(bulk_df$success), ]
  tcols  <- c("elapsed_ms",  "download_ms", "llm_ms",    "column_ms")
  labels <- c("total elapsed time", "download time", "LLM time", "column extraction time")

  for (k in seq_along(tcols)) {
    col <- tcols[k]
    if (!col %in% names(succ)) next
    cat(sprintf("By %s:\n", labels[k]))
    na_mask    <- is.na(succ[[col]])
    n_excluded <- sum(na_mask)
    ranked     <- succ[!na_mask, ]
    ranked     <- ranked[order(ranked[[col]], decreasing = TRUE), ]
    n_show     <- min(top_n, nrow(ranked))
    if (n_show == 0) {
      cat("  No data available\n")
    } else {
      cat(sprintf("  %-25s %12s\n", "paper_id", col))
      for (i in seq_len(n_show)) {
        cat(sprintf("  %-25s %12.0f\n", ranked$paper_id[i], ranked[[col]][i]))
      }
    }
    if (n_excluded > 0) {
      cat(sprintf("  [%d paper(s) excluded: timing not recorded]\n", n_excluded))
    }
    cat("\n")
  }
}

# ---------------------------------------------------------------------------
# T020 — mandatory .md report writer
# ---------------------------------------------------------------------------

write_md_report <- function(md_sections) {
  out_path <- sprintf("quality_report_%s.md", format(Sys.Date(), "%Y-%m-%d"))
  date_str <- format(Sys.Date(), "%Y-%m-%d")

  header <- c(
    "# Pipeline Quality Report",
    sprintf("**Generated**: %s", date_str),
    ""
  )

  lines <- header
  for (sec in md_sections) {
    lines <- c(lines, sprintf("## %s", sec$title), "", "```", sec$content, "```", "")
  }

  writeLines(lines, out_path)
  cat(sprintf("\nReport saved to: %s\n", out_path))
}

# ---------------------------------------------------------------------------
# main()
# ---------------------------------------------------------------------------

main <- function() {
  args <- parse_args()

  # Resolve active sections
  all_sections <- c("bulk", "coltypes", "codebook", "timing")
  if (identical(args$sections, "all")) {
    active <- all_sections
  } else {
    requested <- trimws(strsplit(args$sections, ",")[[1]])
    bad       <- requested[!requested %in% all_sections]
    if (length(bad) > 0) warn("Unknown section(s) ignored: ", paste(bad, collapse = ", "))
    active <- requested[requested %in% all_sections]
  }

  # Load only what is needed
  needs_bulk <- any(c("bulk", "coltypes", "timing") %in% active)

  bulk_df      <- if (needs_bulk)             load_bulk(args$bulk)                 else NULL
  codebook_sum <- if ("codebook" %in% active) load_codebook_summary(args$codebook) else NULL
  columns_df   <- if ("coltypes" %in% active) load_all_columns(args$outputs_dir)   else NULL
  coverage_df  <- if ("codebook" %in% active) load_all_coverage(args$outputs_dir)  else NULL

  md_sections <- list()

  run_section <- function(title, expr) {
    output <- capture.output(expr)
    cat(output, sep = "\n")
    cat("\n")
    md_sections[[length(md_sections) + 1L]] <<- list(title = title, content = output)
  }

  if ("bulk" %in% active) {
    if (is.null(bulk_df)) {
      warn("bulk section skipped: could not load bulk_summary.csv")
    } else {
      run_section("Bulk Run Overview", section_bulk_overview(bulk_df))
    }
  }

  if ("coltypes" %in% active) {
    if (is.null(columns_df)) {
      warn("No column data found; coltypes section skipped")
    } else {
      run_section("Column-Type Distribution",
                  section_col_type_dist(columns_df, bulk_df, args$unknown_threshold))
    }
  }

  if ("codebook" %in% active) {
    if (is.null(coverage_df) && is.null(codebook_sum)) {
      warn("codebook section skipped: no codebook data available")
    } else {
      run_section("Codebook Coverage",
                  section_codebook_coverage(coverage_df, codebook_sum, args$top_n))
    }
  }

  if ("timing" %in% active) {
    if (is.null(bulk_df)) {
      warn("timing section skipped: could not load bulk_summary.csv")
    } else {
      run_section(sprintf("Performance: Top-%d Slowest Papers", args$top_n),
                  section_timing(bulk_df, args$top_n))
    }
  }

  write_md_report(md_sections)
}

main()
