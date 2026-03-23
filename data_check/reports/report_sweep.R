# report_sweep.R
# ─────────────────────────────────────────────────────────────────────────────
# Stability and quality report for a completed temperature sweep.
#
# Usage:
#   Rscript report_sweep.R --sweep-dir ./sweep_results/<paper_id> \
#     [--stability-weight 0.5] [--sections all]
#
# Sections: overview, stability, quality, recommendation, all
# Always writes sweep_report_YYYY-MM-DD.md to --sweep-dir.
# ─────────────────────────────────────────────────────────────────────────────

# ── Argument parsing ──────────────────────────────────────────────────────────

parse_report_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  sweep_dir        <- NULL
  stability_weight <- 0.5
  sections_raw     <- "all"

  i <- 1L
  while (i <= length(args)) {
    switch(args[i],
      "--sweep-dir" = {
        i <- i + 1L
        sweep_dir <- args[i]
      },
      "--stability-weight" = {
        i <- i + 1L
        stability_weight <- as.numeric(args[i])
      },
      "--sections" = {
        i <- i + 1L
        sections_raw <- args[i]
      }
    )
    i <- i + 1L
  }

  if (is.null(sweep_dir) || !nzchar(sweep_dir))
    stop("--sweep-dir is required")
  if (is.na(stability_weight) || stability_weight < 0 || stability_weight > 1)
    stop("--stability-weight must be a numeric value in [0, 1]")

  valid_sections <- c("overview", "stability", "quality", "recommendation", "all")
  sections <- strsplit(sections_raw, ",")[[1]]
  bad <- setdiff(sections, valid_sections)
  if (length(bad) > 0)
    stop("Unknown section(s): ", paste(bad, collapse = ", "),
         ". Valid: ", paste(valid_sections, collapse = ", "))

  active <- if ("all" %in% sections) c("overview", "stability", "quality", "recommendation") else sections

  list(
    sweep_dir        = sweep_dir,
    stability_weight = stability_weight,
    active_sections  = active
  )
}

# ── Data loaders ──────────────────────────────────────────────────────────────

load_run_columns <- function(run_dir) {
  path <- file.path(run_dir, "columns.csv")
  if (!file.exists(path)) return(NULL)
  df <- read.csv(path, stringsAsFactors = FALSE,
                 colClasses = c(paper_id = "character"))
  if (!all(c("column_name", "source_file", "col_type") %in% names(df))) return(NULL)
  df[, c("column_name", "source_file", "col_type")]
}

load_run_labels <- function(run_dir) {
  path <- file.path(run_dir, "labels.csv")
  if (!file.exists(path)) return(NULL)
  df <- read.csv(path, stringsAsFactors = FALSE,
                 colClasses = c(paper_id = "character"))
  if (!all(c("column_name", "label") %in% names(df))) return(NULL)
  df[, c("column_name", "source_file", "label")]
}

load_run_coverage <- function(run_dir) {
  path <- file.path(run_dir, "codebook_coverage.csv")
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE,
           colClasses = c(paper_id = "character"))
}

# ── Pairwise agreement ────────────────────────────────────────────────────────

pairwise_agreement <- function(df_a, df_b, label_col) {
  if (is.null(df_a) || is.null(df_b)) return(NA_real_)

  key_a <- paste(df_a$column_name, df_a$source_file, sep = "\x01")
  key_b <- paste(df_b$column_name, df_b$source_file, sep = "\x01")

  all_keys <- union(key_a, key_b)
  if (length(all_keys) == 0) return(NA_real_)

  val_a <- df_a[[label_col]][match(all_keys, key_a)]
  val_b <- df_b[[label_col]][match(all_keys, key_b)]

  # Columns present in only one run count as disagreement (NA partner = mismatch)
  mean(!is.na(val_a) & !is.na(val_b) & val_a == val_b)
}

# ── Stability computation ─────────────────────────────────────────────────────

compute_stability <- function(sweep_log_df, sweep_dir) {
  temps <- sort(unique(sweep_log_df$temperature))
  rows  <- vector("list", length(temps))

  for (ti in seq_along(temps)) {
    temp    <- temps[ti]
    ok_rows <- sweep_log_df[sweep_log_df$temperature == temp &
                              sweep_log_df$status == "ok", ]

    if (nrow(ok_rows) < 2) {
      warning(sprintf("Temperature %.1f has fewer than 2 ok repeats — stability is NA", temp))
      rows[[ti]] <- data.frame(
        temperature          = temp,
        col_type_agreement   = NA_real_,
        label_agreement      = NA_real_,
        n_pairs              = 0L,
        n_columns_compared   = NA_integer_,
        stringsAsFactors     = FALSE
      )
      next
    }

    cols_list   <- lapply(ok_rows$output_dir, load_run_columns)
    labels_list <- lapply(ok_rows$output_dir, load_run_labels)

    pairs_idx <- combn(nrow(ok_rows), 2)
    n_pairs   <- ncol(pairs_idx)

    ct_agreements <- numeric(n_pairs)
    lb_agreements <- numeric(n_pairs)
    n_cols_total  <- 0L
    has_labels    <- FALSE

    for (pi in seq_len(n_pairs)) {
      ia <- pairs_idx[1, pi]
      ib <- pairs_idx[2, pi]

      ct_agreements[pi] <- pairwise_agreement(cols_list[[ia]], cols_list[[ib]], "col_type")

      la <- labels_list[[ia]]
      lb <- labels_list[[ib]]
      if (!is.null(la) && !is.null(lb)) {
        lb_agreements[pi] <- pairwise_agreement(la, lb, "label")
        has_labels <- TRUE
      } else {
        lb_agreements[pi] <- NA_real_
      }

      if (!is.null(cols_list[[ia]]) && !is.null(cols_list[[ib]])) {
        n_cols_total <- n_cols_total +
          length(union(paste(cols_list[[ia]]$column_name, cols_list[[ia]]$source_file),
                       paste(cols_list[[ib]]$column_name, cols_list[[ib]]$source_file)))
      }
    }

    rows[[ti]] <- data.frame(
      temperature        = temp,
      col_type_agreement = mean(ct_agreements, na.rm = TRUE),
      label_agreement    = if (has_labels) mean(lb_agreements, na.rm = TRUE) else NA_real_,
      n_pairs            = n_pairs,
      n_columns_compared = n_cols_total,
      stringsAsFactors   = FALSE
    )
  }

  do.call(rbind, rows)
}

# ── Quality computation ───────────────────────────────────────────────────────

compute_quality <- function(sweep_log_df, sweep_dir) {
  temps <- sort(unique(sweep_log_df$temperature))
  rows  <- vector("list", length(temps))

  for (ti in seq_along(temps)) {
    temp    <- temps[ti]
    ok_rows <- sweep_log_df[sweep_log_df$temperature == temp &
                              sweep_log_df$status == "ok", ]
    n_rep   <- nrow(ok_rows)

    if (n_rep == 0) {
      rows[[ti]] <- data.frame(
        temperature            = temp,
        known_type_rate        = NA_real_,
        codebook_coverage_rate = NA_real_,
        nonempty_label_rate    = NA_real_,
        n_repeats_used         = 0L,
        stringsAsFactors       = FALSE
      )
      next
    }

    kt_rates  <- numeric(n_rep)
    cov_rates <- rep(NA_real_, n_rep)
    lb_rates  <- rep(NA_real_, n_rep)

    for (ri in seq_len(n_rep)) {
      run_dir <- ok_rows$output_dir[ri]

      cols_df <- load_run_columns(run_dir)
      if (!is.null(cols_df) && nrow(cols_df) > 0) {
        kt_rates[ri] <- mean(cols_df$col_type != "unknown", na.rm = TRUE)
      }

      cov_df <- load_run_coverage(run_dir)
      if (!is.null(cov_df) && nrow(cov_df) > 0) {
        cov_rates[ri] <- mean(cov_df$match_status == "matched", na.rm = TRUE)
      }

      lab_df <- load_run_labels(run_dir)
      if (!is.null(lab_df) && nrow(lab_df) > 0) {
        lb_rates[ri] <- mean(!is.na(lab_df$label) & nzchar(lab_df$label), na.rm = TRUE)
      }
    }

    rows[[ti]] <- data.frame(
      temperature            = temp,
      known_type_rate        = mean(kt_rates, na.rm = TRUE),
      codebook_coverage_rate = if (all(is.na(cov_rates))) NA_real_ else mean(cov_rates, na.rm = TRUE),
      nonempty_label_rate    = if (all(is.na(lb_rates))) NA_real_ else mean(lb_rates, na.rm = TRUE),
      n_repeats_used         = n_rep,
      stringsAsFactors       = FALSE
    )
  }

  do.call(rbind, rows)
}

# ── Recommendation ────────────────────────────────────────────────────────────

compute_recommendation <- function(stability_df, quality_df, w_stab) {
  df <- merge(stability_df[, c("temperature", "col_type_agreement")],
              quality_df[, c("temperature", "known_type_rate",
                              "codebook_coverage_rate", "nonempty_label_rate")],
              by = "temperature", all = TRUE)

  w_qual <- 1 - w_stab

  df$quality_score <- apply(
    df[, c("known_type_rate", "codebook_coverage_rate", "nonempty_label_rate")],
    1,
    function(row) {
      vals <- row[!is.na(row)]
      if (length(vals) == 0) NA_real_ else mean(vals)
    }
  )

  df$stability_score <- df$col_type_agreement

  df$combined_score <- mapply(function(stab, qual) {
    if (is.na(stab) && is.na(qual)) return(NA_real_)
    if (is.na(qual)) {
      warning("Quality is NA for temperature ", stab, " — using stability only")
      return(stab)
    }
    if (is.na(stab)) return(qual)
    w_stab * stab + w_qual * qual
  }, df$stability_score, df$quality_score)

  df <- df[order(df$combined_score, decreasing = TRUE), ]

  top_score <- max(df$combined_score, na.rm = TRUE)
  df$is_tied <- !is.na(df$combined_score) & (top_score - df$combined_score < 0.001)

  df
}

# ── Section printers ──────────────────────────────────────────────────────────

section_sweep_overview <- function(sweep_log_df) {
  cat("Sweep Overview\n")
  cat(strrep("─", 60), "\n")

  temps <- sort(unique(sweep_log_df$temperature))
  for (temp in temps) {
    rows     <- sweep_log_df[sweep_log_df$temperature == temp, ]
    n_att    <- nrow(rows)
    n_ok     <- sum(rows$status == "ok")
    n_fail   <- sum(rows$status == "failed")
    elapsed  <- if (sum(!is.na(rows$elapsed_ms)) > 0)
                  paste0(" | total ", round(sum(rows$elapsed_ms, na.rm = TRUE) / 1000), "s")
                else ""
    resumed  <- if (n_ok + n_fail < n_att) " [partial]" else ""
    cat(sprintf("  T=%.1f  attempted=%d  ok=%d  failed=%d%s%s\n",
                temp, n_att, n_ok, n_fail, elapsed, resumed))
  }

  total_elapsed <- sum(sweep_log_df$elapsed_ms, na.rm = TRUE)
  cat(sprintf("\nTotal elapsed: %ds across %d runs\n",
              round(total_elapsed / 1000), nrow(sweep_log_df)))
}

section_stability <- function(stability_df) {
  cat("Stability Report (pairwise col_type agreement)\n")
  cat(strrep("─", 60), "\n")

  has_labels <- any(!is.na(stability_df$label_agreement))

  stability_df <- stability_df[order(stability_df$col_type_agreement, decreasing = TRUE,
                                     na.last = TRUE), ]

  if (has_labels) {
    cat(sprintf("  %-8s  %-20s  %-18s  %-8s\n",
                "Temp", "col_type_agreement", "label_agreement", "n_pairs"))
    cat(strrep("─", 60), "\n")
    for (i in seq_len(nrow(stability_df))) {
      r <- stability_df[i, ]
      ct  <- if (is.na(r$col_type_agreement)) " N/A  " else sprintf("%.1f%%", r$col_type_agreement * 100)
      lb  <- if (is.na(r$label_agreement))    " N/A  " else sprintf("%.1f%%", r$label_agreement * 100)
      cat(sprintf("  T=%-5.1f  %-20s  %-18s  %d\n", r$temperature, ct, lb, r$n_pairs))
    }
  } else {
    cat(sprintf("  %-8s  %-20s  %-8s\n", "Temp", "col_type_agreement", "n_pairs"))
    cat(strrep("─", 60), "\n")
    for (i in seq_len(nrow(stability_df))) {
      r  <- stability_df[i, ]
      ct <- if (is.na(r$col_type_agreement)) " N/A  " else sprintf("%.1f%%", r$col_type_agreement * 100)
      cat(sprintf("  T=%-5.1f  %-20s  %d\n", r$temperature, ct, r$n_pairs))
    }
  }

  low_rep <- stability_df[!is.na(stability_df$n_pairs) & stability_df$n_pairs == 0, ]
  if (nrow(low_rep) > 0)
    warning(sprintf("%d temperature(s) had <2 ok repeats — stability is NA for T=%s",
                    nrow(low_rep),
                    paste(low_rep$temperature, collapse = ", ")))
}

section_quality <- function(quality_df) {
  cat("Quality Proxy Metrics\n")
  cat(strrep("─", 60), "\n")
  cat(sprintf("  %-8s  %-16s  %-20s  %-20s  %-8s\n",
              "Temp", "known_type_rate", "codebook_coverage", "nonempty_label", "n_rep"))
  cat(strrep("─", 60), "\n")

  fmt <- function(x) if (is.na(x)) "N/A             " else sprintf("%.1f%%", x * 100)

  for (i in seq_len(nrow(quality_df))) {
    r <- quality_df[i, ]
    cat(sprintf("  T=%-5.1f  %-16s  %-20s  %-20s  %d\n",
                r$temperature,
                fmt(r$known_type_rate),
                fmt(r$codebook_coverage_rate),
                fmt(r$nonempty_label_rate),
                r$n_repeats_used))
  }
}

section_recommendation <- function(rec_df, w_stab) {
  cat("Recommended Temperature\n")
  cat(strrep("─", 60), "\n")

  valid <- rec_df[!is.na(rec_df$combined_score), ]
  if (nrow(valid) < 2) {
    cat("  NOTE: Comparison requires at least 2 temperatures with valid scores.\n")
    if (nrow(valid) == 1)
      cat(sprintf("  Only T=%.1f has a score — no comparison possible.\n", valid$temperature[1]))
    return(invisible(NULL))
  }

  winners <- valid[valid$is_tied, ]
  if (nrow(winners) == 1) {
    cat(sprintf("  Recommended: T=%.1f  (combined score: %.3f)\n",
                winners$temperature[1], winners$combined_score[1]))
  } else {
    cat(sprintf("  Tied temperatures: %s  (score: %.3f each)\n",
                paste(sprintf("T=%.1f", winners$temperature), collapse = ", "),
                winners$combined_score[1]))
    cat("  Please review the detailed report to choose between tied candidates.\n")
  }

  cat(sprintf("\n  Weights: stability=%.2f  quality=%.2f\n", w_stab, 1 - w_stab))
  cat("\n  Score breakdown:\n")
  cat(sprintf("  %-8s  %-10s  %-13s  %-10s  %s\n",
              "Temp", "combined", "stability", "quality", "tied?"))
  cat(strrep("─", 60), "\n")
  for (i in seq_len(nrow(rec_df))) {
    r <- rec_df[i, ]
    fmt <- function(x) if (is.na(x)) "N/A       " else sprintf("%.3f     ", x)
    cat(sprintf("  T=%-5.1f  %s  %s  %s  %s\n",
                r$temperature,
                fmt(r$combined_score),
                fmt(r$stability_score),
                fmt(r$quality_score),
                if (isTRUE(r$is_tied)) "*" else ""))
  }
}

# ── Markdown report writer ────────────────────────────────────────────────────

write_sweep_md_report <- function(md_sections, sweep_dir) {
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  out_path <- file.path(sweep_dir, sprintf("sweep_report_%s.md", date_str))

  header <- c(
    "# Temperature Sweep Report",
    sprintf("**Generated**: %s", date_str),
    sprintf("**Sweep directory**: %s", sweep_dir),
    ""
  )

  lines <- header
  for (sec in md_sections) {
    lines <- c(lines, sprintf("## %s", sec$title), "", "```", sec$content, "```", "")
  }

  writeLines(lines, out_path)
  cat(sprintf("\nReport saved to: %s\n", out_path))
}

# ── Main ──────────────────────────────────────────────────────────────────────

main <- function() {
  args <- parse_report_args()

  log_path <- file.path(args$sweep_dir, "sweep_log.csv")
  if (!file.exists(log_path))
    stop("sweep_log.csv not found in: ", args$sweep_dir,
         "\nRun run_sweep.R first.")

  sweep_log_df <- read.csv(log_path, stringsAsFactors = FALSE,
                            colClasses = c(paper_id = "character"))

  md_sections <- list()

  run_section <- function(title, expr) {
    output <- capture.output(expr)
    cat(output, sep = "\n")
    cat("\n")
    md_sections[[length(md_sections) + 1L]] <<- list(title = title, content = output)
  }

  stability_df     <- NULL
  quality_df       <- NULL
  recommendation_df <- NULL

  if ("overview" %in% args$active_sections) {
    run_section("Sweep Overview",
                section_sweep_overview(sweep_log_df))
  }

  if ("stability" %in% args$active_sections) {
    stability_df <- compute_stability(sweep_log_df, args$sweep_dir)
    run_section("Stability Report",
                section_stability(stability_df))
  }

  if ("quality" %in% args$active_sections) {
    quality_df <- compute_quality(sweep_log_df, args$sweep_dir)
    run_section("Quality Proxy Metrics",
                section_quality(quality_df))
  }

  if ("recommendation" %in% args$active_sections) {
    if (is.null(stability_df))
      stability_df <- compute_stability(sweep_log_df, args$sweep_dir)
    if (is.null(quality_df))
      quality_df <- compute_quality(sweep_log_df, args$sweep_dir)
    recommendation_df <- compute_recommendation(stability_df, quality_df,
                                                args$stability_weight)
    run_section("Recommended Temperature",
                section_recommendation(recommendation_df, args$stability_weight))
  }

  write_sweep_md_report(md_sections, args$sweep_dir)
}

if (!exists("REPORT_SWEEP_SOURCED_AS_LIB") && !interactive()) main()
