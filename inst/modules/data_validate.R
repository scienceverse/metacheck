#' Data Validate
#'
#' @description
#' This module runs automated data-quality checks on the tabular data files
#' extracted by `data_check`, flagging likely problems (miscoded missing values,
#' outliers, constant or near-constant columns, inconsistent category casing,
#' sparse categories) and drawing a per-column outlier visualization so reviewers
#' can spot suspicious values at a glance. It also screens columns for personal
#' information that should not be shared openly (emails, IP addresses, national
#' IDs, credit-card numbers, identifying column names, geographic coordinates,
#' and open typed free-text fields), reported as "review before sharing" prompts
#' without echoing the matching values.
#'
#' @details
#' The Data Validate module consumes the columns and the full data frames read by
#' `data_check`. For each numeric column it applies a Tukey (1.5×IQR) outlier
#' rule and a miscoded-missing-value check; for each categorical column it checks
#' for constant columns, case-only duplicate categories, sparsely populated
#' levels, leading/trailing whitespace, and mostly-numeric columns contaminated
#' by a few non-numeric values. Findings are reported per file, alongside a
#' boxplot + histogram of each numeric column with outliers highlighted.
#'
#' The checks are intentionally generic and run on every extracted column.
#' Columns that `codebook_check` matched to a documented variable are shown with
#' their label for context; variable-specific rules (e.g. participant IDs must be
#' unique, ages must be positive) are a planned extension.
#'
#' All checks are native R (no external data-screening dependency). Plots require
#' `ggplot2`.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local
#'   files only (see [test_paper()])
#' @param local_path optional path passed to `data_check` when its output is not
#'   already available
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param outlier_k the IQR multiplier for the Tukey outlier rule (default 1.5)
#' @param model,params passed to `data_check` when `llm_use(TRUE)`
#'
#' @returns a list
data_validate <- function(paper, local_path = NULL, local_only = FALSE,
                          outlier_k = 1.5,
                          model = llm_model(), params = list()) {

  .pid <- function(...) {
    id <- paper_id(paper)
    for (df in list(...)) {
      if (length(id) > 0) break
      if (!is.null(df) && "paper_id" %in% names(df)) id <- unique(df$paper_id)
    }
    if (length(id) == 0) NA_character_ else id[[1]]
  }

  # ── 1. Inputs from data_check ───────────────────────────────────────────────
  columns_df <- get_prev_outputs("data_check", "table")
  previews   <- get_prev_outputs("data_check", "previews")
  if (is.null(previews)) {
    mo <- if (!is.null(local_path)) {
      module_run(paper, "data_check", local_path = local_path,
                 local_only = local_only, model = model, params = params)
    } else {
      module_run(paper, "data_check", local_only = local_only,
                 model = model, params = params)
    }
    columns_df <- mo$table
    previews   <- mo$previews
  }
  labels_df <- get_prev_outputs("codebook_check", "table")

  empty <- function(text) {
    list(
      table = data.frame(),
      summary_table = data.frame(
        paper_id = .pid(columns_df), column_n = 0, flagged_n = 0),
      na_replace = c(column_n = 0, flagged_n = 0),
      traffic_light = "na",
      summary_text = text
    )
  }

  if (is.null(previews) || length(previews) == 0)
    return(empty("We found no readable tabular data files to validate."))

  # Label lookup (source_file + column_name → documented label).
  label_of <- function(file, col) {
    if (is.null(labels_df) || !all(c("source_file", "column_name", "label")
                                   %in% names(labels_df))) return(NA_character_)
    hit <- labels_df$source_file == file & labels_df$column_name == col &
      labels_df$label_status %in% c("labelled", "llm")
    if (any(hit)) labels_df$label[which(hit)[1]] else NA_character_
  }

  # ── 2. Run checks per column ─────────────────────────────────────────────────
  findings <- list()
  plot_specs    <- list()   # per numeric column: values + bounds for the distribution facet
  outlier_specs <- list()   # per numeric column with outliers: row for the outlier table
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    for (col in names(df)) {
      x <- df[[col]]
      lbl <- label_of(file, col)
      col_finds <- list()

      if (is.numeric(x)) {
        o <- data_check_outliers(x, k = outlier_k)
        if (o$problem) col_finds[["Outliers"]] <- o$message
        m <- data_check_miscoded_missing(x)
        if (m$problem) col_finds[["Miscoded missing"]] <- m$message
        # Keep numeric vectors (capped) for the combined distribution figure.
        v <- x[!is.na(x) & !is.nan(x)]
        if (length(v) >= 4 && length(unique(v)) > 1) {
          plot_specs[[length(plot_specs) + 1L]] <- list(
            file = file, col = col, values = utils::head(v, 5000),
            lower = o$lower, upper = o$upper)
          # One row per column that actually has outliers, for the outlier table.
          if (o$problem) {
            ex <- utils::head(sort(o$values), 8)
            outlier_specs[[length(outlier_specs) + 1L]] <- data.frame(
              source_file = file, column = col, label = lbl,
              n_outliers = length(o$values),
              lower = o$lower, upper = o$upper,
              examples = paste(signif(ex, 4), collapse = ", "))
          }
        }
      } else {
        # A mostly-numeric column stored as text is really a contaminated
        # numeric column, not a genuine categorical — so if that fires, skip the
        # categorical-quality checks (sparse levels / case), which would treat
        # every distinct number as a spurious "level".
        nt <- data_check_numeric_in_text(x)
        if (nt$problem) {
          col_finds[["Numeric as text"]] <- nt$message
        } else {
          cc <- data_check_case_issues(x)
          if (cc$problem) col_finds[["Case issues"]] <- cc$message
          sp <- data_check_sparse_levels(x)
          if (sp$problem) col_finds[["Sparse levels"]] <- sp$message
        }
        ws <- data_check_whitespace(x)
        if (ws$problem) col_finds[["Whitespace"]] <- ws$message
      }
      cst <- data_check_constant(x)
      if (cst$problem) col_finds[["Constant"]] <- cst$message

      # Personal / disclosure information (PII): flag columns that may hold
      # data that should not be shared openly. Reported as "review before
      # sharing" prompts, never echoing the matching value.
      pv <- data_check_pii_values(x)
      if (pv$problem) col_finds[["Personal info (values)"]] <- pv$message
      pn <- data_check_pii_name(col)
      if (pn$problem) col_finds[["Personal info (column name)"]] <- pn$message
      pg <- data_check_pii_geo(col, x)
      if (pg$problem) col_finds[["Geographic coordinates"]] <- pg$message
      if (!is.numeric(x)) {
        pf <- data_check_pii_freetext(x)
        if (pf$problem) col_finds[["Free-text (may hold PII)"]] <- pf$message
      }

      for (chk in names(col_finds)) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = file, column = col,
          label = lbl, check = chk, detail = col_finds[[chk]]
        )
      }
    }
  }
  findings_df <- if (length(findings) > 0) dplyr::bind_rows(findings) else
    data.frame(source_file = character(0), column = character(0),
               label = character(0), check = character(0), detail = character(0))

  outlier_df <- if (length(outlier_specs) > 0) dplyr::bind_rows(outlier_specs) else
    data.frame(source_file = character(0), column = character(0),
               label = character(0), n_outliers = integer(0),
               lower = numeric(0), upper = numeric(0), examples = character(0))

  n_columns <- if (!is.null(columns_df)) nrow(columns_df) else
    sum(vapply(previews, ncol, integer(1)))
  n_flagged <- length(unique(paste(findings_df$source_file, findings_df$column)))

  # Per-check tally: how many distinct columns each check flagged (a column can
  # be flagged by several checks, so these overlap and need not sum to n_flagged).
  check_counts <- if (nrow(findings_df) > 0) {
    findings_df |>
      dplyr::distinct(.data$source_file, .data$column, .data$check) |>
      dplyr::count(.data$check, name = "columns", sort = TRUE)
  } else {
    data.frame(check = character(0), columns = integer(0))
  }
  # Human-readable phrasing for each check. Noun phrases ("... with potential
  # outliers") so they read correctly after any count, singular or plural.
  check_phrase <- c(
    Outliers            = "with potential outliers",
    "Miscoded missing"  = "with miscoded missing values",
    Constant            = "with a single constant value",
    "Case issues"       = "with inconsistent category casing",
    "Sparse levels"     = "with sparsely populated categories",
    Whitespace          = "with leading/trailing whitespace",
    "Numeric as text"   = "with numeric values stored as text",
    "Personal info (values)"      = "whose values look like personal information",
    "Personal info (column name)" = "whose name suggests personal information",
    "Geographic coordinates"      = "that look like geographic coordinates",
    "Free-text (may hold PII)"    = "of free text that may contain personal detail"
  )

  # ── 3. Traffic light ─────────────────────────────────────────────────────────
  frac_flagged <- if (n_columns > 0) n_flagged / n_columns else 0
  tl <- if (n_flagged == 0) "green"
        else if (frac_flagged < 0.25) "yellow"
        else "red"

  # ── 4. Report ────────────────────────────────────────────────────────────────
  summary_text <- if (n_flagged == 0) {
    sprintf("We ran automated data-quality checks on %d column%s and found no issues.",
            n_columns, plural(n_columns))
  } else {
    # Append a per-check breakdown so reviewers see what dominates without
    # scrolling every column, e.g. "42 contain potential outliers; 18 have ...".
    parts <- vapply(seq_len(nrow(check_counts)), function(i) {
      chk <- check_counts$check[i]
      phrase <- if (chk %in% names(check_phrase)) check_phrase[[chk]] else
        sprintf("flagged by %s", tolower(chk))
      n <- check_counts$columns[i]
      sprintf("%d column%s %s", n, plural(n), phrase)
    }, character(1))
    sprintf("We ran automated data-quality checks on %d column%s; %d column%s %s at least one potential issue (%s).",
            n_columns, plural(n_columns), n_flagged, plural(n_flagged),
            if (n_flagged == 1) "has" else "have",
            paste(parts, collapse = "; "))
  }

  report <- c(
    "This module runs automated data-quality checks (outliers, miscoded missing values, constant columns, category casing, sparse levels) on the extracted data files.",
    sprintf("We examined %d column%s across %d data file%s.",
            n_columns, plural(n_columns),
            length(previews), plural(length(previews)))
  )

  if (nrow(check_counts) > 0) {
    # Issue-type breakdown first: for large files, this is the headline — how
    # many columns each kind of problem affects — before the full per-column list.
    breakdown_tbl <- check_counts |>
      dplyr::transmute(
        Issue = .data$check,
        `Columns affected` = .data$columns)
    report <- c(report, "#### Issues by Type",
                sprintf("Number of columns affected by each type of issue (a column can be flagged by more than one check, so these need not sum to %d).",
                        n_flagged),
                scroll_table(breakdown_tbl, maxrows = 10))
  }

  if (nrow(findings_df) > 0) {
    finds_tbl <- findings_df |>
      dplyr::transmute(
        File = .data$source_file, Column = .data$column,
        Label = .data$label, Check = .data$check, Detail = .data$detail)
    report <- c(report, "#### Potential Issues (per column)",
                scroll_table(finds_tbl, maxrows = 20))
  }

  # Outliers: one table row per numeric column that has values outside the
  # Tukey fences, rather than one plot per column. Bounds + a few example values
  # let a reviewer judge each column without scanning hundreds of figures.
  if (nrow(outlier_df) > 0) {
    out_tbl <- outlier_df |>
      dplyr::arrange(dplyr::desc(.data$n_outliers)) |>
      dplyr::transmute(
        File = .data$source_file, Column = .data$column, Label = .data$label,
        `N outliers` = .data$n_outliers,
        `Range [low, high]` = sprintf("[%.3g, %.3g]", .data$lower, .data$upper),
        `Example values` = .data$examples)
    n_out_cols <- nrow(outlier_df)
    n_out_vals <- sum(outlier_df$n_outliers)
    report <- c(report,
      "#### Outliers",
      sprintf("%d numeric column%s %s %d value%s outside the %.1f×IQR fences.",
              n_out_cols, plural(n_out_cols),
              if (n_out_cols == 1) "has" else "have",
              n_out_vals, plural(n_out_vals), outlier_k),
      scroll_table(out_tbl, maxrows = 20))
  }

  # Distributions: a single faceted figure with one small histogram per numeric
  # column (outlier fences drawn as dashed lines), instead of a separate plot
  # per column. One render call keeps wide files fast; the facet count is capped
  # so the figure stays legible.
  if (length(plot_specs) > 0 && requireNamespace("ggplot2", quietly = TRUE)) {
    report <- c(report, "#### Distributions",
                data_validate_dist_facets(plot_specs))
  } else if (length(plot_specs) > 0) {
    report <- c(report,
      "*Install the `ggplot2` package to see the distribution histograms.*")
  }

  # ── 5. Summary table + return ────────────────────────────────────────────────
  summary_table <- data.frame(
    paper_id  = .pid(columns_df),
    column_n  = n_columns,
    flagged_n = n_flagged
  )

  list(
    table = findings_df,
    summary_table = summary_table,
    na_replace = c(column_n = 0, flagged_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# ── Module-local helper ───────────────────────────────────────────────────────

# Maximum number of numeric columns drawn in the combined distribution figure.
# Beyond this the facet grid becomes unreadable and slow, so we show the first
# `.dv_max_facets` and note how many were omitted.
.dv_max_facets <- 40L

# Build ONE faceted figure (a small histogram per numeric column, with the
# Tukey outlier fences as dashed lines) and return it as a self-contained
# base64 <img>. A single ggsave replaces the previous one-render-per-column
# tabset, which rendered hundreds of PNGs on wide files. The image is embedded
# as an inline data URI so a moved report keeps its figure (Quarto's own figure
# output links external `_files/` PNGs that the embed step does not inline).
data_validate_dist_facets <- function(plot_specs, max_facets = .dv_max_facets) {
  n_total <- length(plot_specs)
  if (n_total == 0) return(NULL)
  specs <- utils::head(plot_specs, max_facets)

  # Long data frame: one row per value, tagged by a unique per-column facet key
  # ("file: column"), plus the fences carried alongside for the rule layers.
  facet_label <- function(s) {
    lab <- if (nzchar(s$file)) paste0(s$col, "  (", s$file, ")") else s$col
    substr(lab, 1, 60)
  }
  parts <- lapply(seq_along(specs), function(i) {
    s <- specs[[i]]
    data.frame(facet = facet_label(s), v = as.numeric(s$values))
  })
  fences <- do.call(rbind, lapply(seq_along(specs), function(i) {
    s <- specs[[i]]
    if (is.na(s$lower)) NULL else
      data.frame(facet = facet_label(specs[[i]]),
                 xint = c(s$lower, s$upper))
  }))
  d <- do.call(rbind, parts)
  # Stable facet order matching plot_specs order.
  d$facet <- factor(d$facet, levels = unique(vapply(specs, facet_label, character(1))))

  uri <- tryCatch({
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$v)) +
      ggplot2::geom_histogram(bins = 30, fill = "grey70") +
      ggplot2::facet_wrap(~ facet, scales = "free", ncol = 4) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 8) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
    if (!is.null(fences)) {
      fences$facet <- factor(fences$facet, levels = levels(d$facet))
      p <- p + ggplot2::geom_vline(
        data = fences, ggplot2::aes(xintercept = .data$xint),
        linetype = "dashed", colour = "red", linewidth = 0.3)
    }
    n_facets <- nlevels(d$facet)
    nrow_facets <- ceiling(n_facets / 4)
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    ggplot2::ggsave(tmp, p, width = 9,
                    height = max(2, nrow_facets * 1.3), dpi = 96,
                    device = "png", bg = "white", limitsize = FALSE)
    paste0("data:image/png;base64,", base64enc::base64encode(tmp))
  }, error = function(e) NA_character_)

  if (is.na(uri)) return("*Distribution figure could not be rendered.*")
  img <- sprintf("<img src=\"%s\" alt=\"Column distributions\" style=\"max-width:100%%\"/>", uri)
  note <- if (n_total > length(specs))
    sprintf("\n\n*Showing the first %d of %d numeric columns.*",
            length(specs), n_total) else ""
  paste0(img, note)
}
