#' Data Validate
#'
#' @description
#' This module runs automated data-quality checks on the tabular data files
#' extracted by `data_check`, flagging likely problems (miscoded missing values,
#' outliers, empty and constant columns, SPSS filter variables, inconsistent
#' category casing, column names that cannot be reused as file or variable
#' names, text values stored in a legacy non-UTF-8 encoding) and drawing a
#' per-column outlier visualization so reviewers
#' can spot suspicious values at a glance. It also screens columns for personal
#' information that should not be shared openly (emails, IP addresses, national
#' IDs, credit-card numbers, identifying column names, geographic coordinates,
#' and open typed free-text fields), reported as "review before sharing" prompts
#' without echoing the matching values.
#'
#' It also reports an inventory of the **demographic columns** (age, gender/sex,
#' race/ethnicity) detected in the data — the variables studies typically report
#' about their sample — using `data_check`'s name+value tag (or recomputing it
#' from the previews). This is informational, not a problem flag.
#'
#' For **Qualtrics survey exports** it summarises the response metadata that is
#' reliably extractable from any Qualtrics file: how many rows are previews /
#' unfinished responses that likely need dropping, the completion-time
#' distribution (with a count of implausibly fast responses), the
#' data-collection window, and which Qualtrics fields carry personal information
#' to review before sharing. The substantive question columns are not interpreted
#' here (that is the scale-block detection's job).
#'
#' For survey data it additionally screens for **careless responding**. When a
#' file contains a block of Likert-type items (a run of adjacent columns sharing
#' a response scale and a variable-name prefix) together with an identifier
#' column, the `careless` package's longstring and IRV indices are computed per
#' scale block, and respondents that straightline or answer unusually flatly /
#' erratically are flagged by their id. This check is skipped (with a note) when
#' the optional `careless` package is not installed.
#'
#' @details
#' The Data Validate module consumes the columns and the full data frames read by
#' `data_check`. For each numeric column it checks for out-of-range values (on
#' bounded, few-level columns: a value far outside the column's apparent range,
#' the signature of a data-entry error or unrecoded missing code — continuous
#' columns like reaction times are not range-checked, since a long tail is
#' normal) and miscoded missing values; for each categorical column it checks
#' for case-only duplicate categories, leading/trailing whitespace, and
#' mostly-numeric columns contaminated by a few non-numeric values. Empty
#' columns, constant columns (tiered: numeric or design-named constants are
#' flagged, constant text is listed as likely file-level metadata), and SPSS
#' "Select Cases" filter variables are flagged for every column type.
#' Column *names* are checked too: names carrying
#' characters that are illegal in file names (`< > : " / \ | ? *`), control
#' characters, or more than 64 characters break when reused — as file names,
#' in analysis scripts, or on import into other statistical packages (64 bytes
#' is the most SPSS accepts for a variable name; SAS and Stata stop at 32).
#' Very long names additionally prevent generating the codebook's per-variable
#' figures (the file path exceeds Windows' 260-character limit), and a
#' garbled name usually means the file's header row was not exported or
#' parsed as intended. Sibling columns whose names differ *only* in special
#' characters (e.g. `t'` next to a t-with-diacritic) are flagged as colliding:
#' tools that sanitize names on import cannot tell them apart. Text values
#' whose bytes are not valid UTF-8 (a legacy Latin-1/Windows-1252 export, or a
#' file that mixes encodings) are flagged as **mixed encoding**, with
#' instructions to re-save the file as UTF-8 — such characters corrupt
#' silently on other systems; metacheck itself reads on by re-interpreting the
#' affected values as Latin-1. Findings are
#' reported per file, alongside a
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
#' @param plot_distributions create plots of distributions of columns (default `FALSE`)
#' @param max_facets the maximum number of numeric columns drawn in the combined
#'   distribution figure (default 40); beyond this the facet grid becomes
#'   unreadable. This is a display limit (all columns are still checked): when
#'   more columns exist, the figure shows the first `max_facets` and a note names
#'   the parameter and how many were omitted.
#' @param model,params passed to `data_check` when `llm_use(TRUE)`
#'
#' @returns a list
data_validate <- function(paper, local_path = NULL, local_only = FALSE,
                          outlier_k = 1.5, plot_distributions = FALSE, max_facets = .dv_max_facets,
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

  # Codebook ground truth for a column: the enumerated valid codes (value
  # labels) and the declared missing codes, decoded from the codebook_check
  # table's JSON columns. These override inference in the numeric checks: a
  # documented 1-5 value set makes a 6 out-of-range; a declared -99 is flagged
  # as missing directly. Returns numeric vectors (or NULL when undocumented /
  # codebook_check has not run).
  codebook_of <- function(file, col) {
    empty <- list(valid_values = NULL, missing_values = NULL)
    if (is.null(labels_df) ||
        !all(c("source_file", "column_name") %in% names(labels_df)))
      return(empty)
    hit <- which(labels_df$source_file == file & labels_df$column_name == col)
    if (!length(hit)) return(empty)
    i <- hit[[1]]
    num_codes <- function(json) {
      if (!length(json) || is.na(json) || !nzchar(json)) return(NULL)
      kv <- .decode_value_labels(json)
      codes <- suppressWarnings(as.numeric(names(kv) %||% kv))
      codes <- codes[!is.na(codes)]
      if (length(codes)) codes else NULL
    }
    vl <- if ("value_labels"   %in% names(labels_df)) labels_df$value_labels[i]   else NA
    mv <- if ("missing_values" %in% names(labels_df)) labels_df$missing_values[i] else NA
    list(valid_values = num_codes(vl), missing_values = num_codes(mv))
  }

  # Values whose bytes were not valid UTF-8 and had to be re-interpreted as
  # Latin-1 at read time (recorded by data_check per column; the repaired
  # preview values no longer show it). 0 when absent or from an older
  # data_check run without the column.
  utf8_repaired_of <- function(file, col) {
    if (is.null(columns_df) || !all(c("source_file", "column_name",
                                      "utf8_repaired") %in% names(columns_df)))
      return(0L)
    hit <- columns_df$source_file == file & columns_df$column_name == col
    if (any(hit)) columns_df$utf8_repaired[which(hit)[1]] %||% 0L else 0L
  }

  # ── 2. Run checks per column ─────────────────────────────────────────────────
  findings <- list()
  plot_specs    <- list()   # per numeric column: values + bounds for the distribution facet
  outlier_specs <- list()   # per numeric column with outliers: row for the outlier table
  meta_const_specs <- list()  # constant text columns that look like file-level metadata
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    # Names that become identical once special characters are sanitized away
    # (computed per file: collisions are between sibling columns).
    coll <- data_check_colname_collisions(names(df))
    for (col in names(df)) {
      x <- df[[col]]
      lbl <- label_of(file, col)
      col_finds <- list()

      if (is.numeric(x)) {
        # Normalize numeric vectors to base doubles so integer64 columns from
        # fread/readxl do not cause type conflicts in downstream bind_rows().
        x_num <- suppressWarnings(as.numeric(x))
        # Values outside a rating scale's valid range — the only reliable
        # column-level "impossible value" signal (unbounded columns like RT/age
        # have no principled range; their extreme values are not flagged here).
        # One detector run classifies each out-of-scale value as a missing code,
        # a keying typo, or unexplained. Ground truth (codebook value labels /
        # declared missing codes) overrides inference.
        cbk <- codebook_of(file, col)
        sv <- data_check_scale_values(x_num, declared = cbk$missing_values,
                                      valid_values = cbk$valid_values)
        if (sv$problem) col_finds[["Values outside the scale"]] <- sv$message
        # The distribution figure still uses the Tukey fences as visual context
        # (dashed lines); they help a reader eyeball a distribution but are no
        # longer emitted as a per-column finding.
        o <- data_check_outliers(x_num, k = outlier_k)
        v <- x_num[!is.na(x_num) & !is.nan(x_num)]
        if (length(v) >= 4 && length(unique(v)) > 1) {
          plot_specs[[length(plot_specs) + 1L]] <- list(
            file = file, col = col, values = utils::head(v, 5000),
            lower = as.numeric(o$lower), upper = as.numeric(o$upper))
          # One row per column with out-of-scale values, for the summary table.
          if (sv$problem) {
            ex <- utils::head(sort(suppressWarnings(as.numeric(sv$values))), 8)
            outlier_specs[[length(outlier_specs) + 1L]] <- data.frame(
              source_file = file, column = col, label = lbl,
              n_outliers = length(sv$values),
              lower = as.numeric(sv$lower), upper = as.numeric(sv$upper),
              examples = paste(signif(ex, 4), collapse = ", "))
          }
        }
      } else {
        # A mostly-numeric column stored as text is really a contaminated
        # numeric column, not a genuine categorical — so if that fires, skip
        # the case check, which would treat every distinct number as a
        # spurious "level".
        nt <- data_check_numeric_in_text(x)
        if (nt$problem) {
          col_finds[["Numeric as text"]] <- nt$message
        } else {
          cc <- data_check_case_issues(x)
          if (cc$problem) col_finds[["Case issues"]] <- cc$message
        }
        ws <- data_check_whitespace(x)
        if (ws$problem) col_finds[["Whitespace"]] <- ws$message
      }
      # Empty and constant columns, tiered by how likely they are to signal a
      # real problem. An all-missing column (a variable that never recorded)
      # is always flagged. A constant column is flagged when it is numeric or
      # when its name looks like a design/condition variable — one value there
      # suggests the file was filtered to a single condition before export. A
      # constant *text* column is usually intentional file-level metadata
      # ("version 3", a language code) and is only listed in an informational
      # note, not counted as an issue. Near-constant columns are only
      # suspicious for design-named columns: rare-event outcomes and exclusion
      # flags are legitimately 99% one value.
      emp <- data_check_empty(x)
      if (emp$problem) {
        col_finds[["Empty column"]] <- emp$message
      } else {
        cst <- data_check_constant(x)
        if (cst$problem) {
          if (data_check_design_name(col)) {
            col_finds[["Constant"]] <- paste(cst$message,
              "The name suggests a design/condition variable; if the study had more than one condition, the file may have been filtered before export.")
          } else if (!cst$near && is.numeric(x)) {
            col_finds[["Constant"]] <- cst$message
          } else if (!cst$near) {
            meta_const_specs[[length(meta_const_specs) + 1L]] <- data.frame(
              source_file = file, column = col, value = cst$values[[1]])
          }
        }
      }
      # An SPSS "Select Cases" filter variable matters whether or not it is
      # constant: constant-1 means the file is a pre-filtered subset; varying
      # means analyses likely used only the selected rows.
      fl <- data_check_spss_filter(col, x)
      if (fl$problem) col_finds[["SPSS filter variable"]] <- fl$message

      # Column-name quality: names with file-illegal characters, control
      # characters, padding, or excessive length break downstream reuse (figure
      # file names, scripts, metadata keys) and often signal a misparsed header.
      cn <- data_check_colname(col)
      if (cn$problem) col_finds[["Problematic column name"]] <- cn$message
      if (!is.null(coll[[col]]))
        col_finds[["Colliding column names"]] <- coll[[col]]

      # Mixed / legacy encoding: values whose bytes were not valid UTF-8 at
      # read time. metacheck re-interpreted them as Latin-1 to continue, but
      # on other systems these characters silently corrupt (é becomes "Ã©" or
      # "�"), so the researcher should re-save the file as UTF-8.
      n_enc <- utf8_repaired_of(file, col)
      if (!is.na(n_enc) && n_enc > 0) {
        col_finds[["Mixed encoding"]] <- sprintf(
          "%d value%s %s not valid UTF-8 (bytes from a legacy encoding such as Latin-1/Windows-1252, e.g. a mis-encoded accent or apostrophe); metacheck re-interpreted %s as Latin-1 to continue. Such characters corrupt silently when the file is opened on another system. Re-save the file with UTF-8 encoding: in Excel use 'Save As > CSV UTF-8 (Comma delimited)'; in R, write.csv(..., fileEncoding = \"UTF-8\"); in SPSS, 'Save as type: CSV' with Encoding 'UTF-8'.",
          n_enc, plural(n_enc), if (n_enc == 1) "is" else "are",
          if (n_enc == 1) "it" else "them")
      }

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
  # ── 2a. Demographic columns (age / gender / race) ───────────────────────────
  # Report which files contain the demographic variables almost every study
  # collects. Prefer data_check's precomputed `concept` facet (name+value
  # agreement); fall back to computing it here from the previews so this works
  # even against an older data_check run whose table predates the facet.
  demo_specs <- list()
  demo_tagged <- !is.null(columns_df) &&
    all(c("source_file", "column_name", "concept") %in% names(columns_df))
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    for (col in names(df)) {
      kind <- if (demo_tagged) {
        hit <- columns_df$source_file == file & columns_df$column_name == col
        v <- if (any(hit)) columns_df$concept[which(hit)[1]] else NA_character_
        if (is.na(v) || !nzchar(v)) NA_character_ else v
      } else {
        data_check_demographic(col, df[[col]])
      }
      # The `concept` facet also carries non-demographic concepts (reaction_time,
      # likert, Qualtrics tags, ...); keep only the demographic ones here.
      if (!is.na(kind) && !kind %in% c("age", "gender", "race"))
        kind <- NA_character_
      if (!is.na(kind))
        demo_specs[[length(demo_specs) + 1L]] <- data.frame(
          source_file = file, column = col, demographic = kind)
    }
  }
  demo_df <- if (length(demo_specs) > 0) dplyr::bind_rows(demo_specs) else
    data.frame(source_file = character(0), column = character(0),
               demographic = character(0))

  # ── 2a2. Qualtrics survey metadata ──────────────────────────────────────────
  # For each file that is a Qualtrics export, summarise the things reliably
  # extractable from its response-metadata columns: how many rows are previews /
  # unfinished (and should likely be dropped), the completion-time distribution
  # (with implausibly fast responses), the data-collection window, and which
  # Qualtrics PII fields are present. Nothing here interprets the substantive
  # question columns — that is out of scope.
  qualtrics_specs <- list()
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) == 0) next
    if (!data_check_is_qualtrics(df)) next
    s <- .dv_qualtrics_summary(df)
    if (!is.null(s)) {
      s$source_file <- file
      qualtrics_specs[[length(qualtrics_specs) + 1L]] <- s
    }
  }

  # ── 2b. Careless responding (survey data only) ──────────────────────────────
  # For files that contain a block of Likert-type items AND an identifier column,
  # run careless-response indices (longstring + IRV) per scale block and flag the
  # respondents that look careless. Needs the `careless` package; skipped (with a
  # note) when it is not installed. Findings feed the same per-column tally as a
  # "Careless responding" check, one row per (file, flagged respondent).
  careless_specs <- list()   # per flagged respondent, for the report table
  careless_note  <- NULL
  careless_avail <- requireNamespace("careless", quietly = TRUE)
  n_careless_files <- 0L
  for (file in names(previews)) {
    df <- previews[[file]]
    if (is.null(df) || ncol(df) < .dv_careless_min_items ||
        nrow(df) < .dv_careless_min_rows) next
    blocks <- .detect_scale_blocks(df)
    if (length(blocks) == 0) next

    # An identifier column: prefer data_check's `identifier` role for this
    # file; fall back to a name-pattern match; else use row numbers.
    id_col <- NULL
    if (!is.null(columns_df) &&
        all(c("source_file", "column_name", "role") %in% names(columns_df))) {
      idc <- columns_df$column_name[columns_df$source_file == file &
                                      columns_df$role %in% "identifier"]
      idc <- idc[idc %in% names(df)]
      if (length(idc) > 0) id_col <- idc[[1]]
    }
    if (is.null(id_col)) {
      # Word-boundary anchored: an unanchored "subject"/"participant" substring
      # match hit real response columns whose names happen to start with that
      # stem — "SubjectiveSES", "subjective_aware", "subject_cond",
      # "subject_parity" are Likert/condition items about a participant's
      # subjective state, not identifier columns — verified against the
      # cached corpus, same bug class as .concept_is_rt/.missing_label_re.
      hit <- grep("(?i)(^id$|\\bparticipant|\\bsubject\\b|\\brespond|_id$|\\bprolific|\\bmturk|\\bworker)",
                  names(df), perl = TRUE)
      if (length(hit) > 0) id_col <- names(df)[hit[[1]]]
    }
    has_id <- !is.null(id_col)
    if (!has_id) next            # careless findings are only actionable with an ID
    n_careless_files <- n_careless_files + 1L
    if (!careless_avail) next    # count the opportunity, but cannot compute

    ids <- as.character(df[[id_col]])
    for (cols in blocks) {
      scale  <- .scale_block_range(df[, cols, drop = FALSE])
      # Prefer a named scale identified by codebook_check (e.g. "PANAS") over the
      # bare variable-name prefix, when available for these columns.
      prefix <- .scale_name_prefix(names(df)[cols[[1]]])
      if (!is.null(labels_df) && all(c("source_file", "column_name", "scale")
                                     %in% names(labels_df))) {
        hit <- labels_df$source_file == file &
          labels_df$column_name %in% names(df)[cols] &
          !is.na(labels_df$scale) & nzchar(labels_df$scale %||% "")
        if (any(hit)) prefix <- labels_df$scale[which(hit)[1]]
      }
      res <- tryCatch(.dv_careless_block(df[, cols, drop = FALSE], ids, scale, prefix),
                      error = function(e) NULL)
      if (!is.null(res) && nrow(res) > 0) {
        res$source_file <- file
        careless_specs[[length(careless_specs) + 1L]] <- res
      }
    }
  }
  if (n_careless_files > 0 && !careless_avail)
    careless_note <- sprintf(
      "%d file%s contain survey data with an identifier, but careless-response checks were skipped because the `careless` package is not installed. Install it with `install.packages(\"careless\")` to screen for straightlining and other careless responding.",
      n_careless_files, plural(n_careless_files))

  careless_block_df <- if (length(careless_specs) > 0) dplyr::bind_rows(careless_specs) else
    data.frame(source_file = character(0), scale = character(0),
               respondent = character(0), longstring = integer(0),
               irv = numeric(0), reason = character(0))

  # Aggregate to ONE ROW PER RESPONDENT (the per-block table double-counts a
  # person flagged in several scale blocks). For each respondent we record how
  # many blocks flagged them out of how many they appear in, the flag reasons,
  # and whether every flag is short-scale straightlining — which on a short,
  # unidirectional scale is often normal, coherent answering rather than
  # carelessness, so those respondents should be read with caution.
  careless_df <- .dv_careless_by_respondent(careless_block_df, previews,
                                            columns_df)

  findings_df <- if (length(findings) > 0) dplyr::bind_rows(findings) else
    data.frame(source_file = character(0), column = character(0),
               label = character(0), check = character(0), detail = character(0))

  outlier_df <- if (length(outlier_specs) > 0) dplyr::bind_rows(outlier_specs) else
    data.frame(source_file = character(0), column = character(0),
               label = character(0), n_outliers = integer(0),
               lower = numeric(0), upper = numeric(0), examples = character(0))

  meta_const_df <- if (length(meta_const_specs) > 0)
    dplyr::bind_rows(meta_const_specs) else
    data.frame(source_file = character(0), column = character(0),
               value = character(0))

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
    "Out-of-range values" = "with values outside the column's apparent range (likely data-entry errors)",
    "Miscoded missing"  = "with miscoded missing values",
    Constant            = "with a single constant value",
    "Empty column"      = "with no observed values (entirely empty)",
    "SPSS filter variable" = "holding an SPSS \"Select Cases\" filter (analyses may have used only a subset of rows)",
    "Case issues"       = "with inconsistent category casing",
    Whitespace          = "with leading/trailing whitespace",
    "Numeric as text"   = "with numeric values stored as text",
    "Problematic column name" = "whose name contains file-illegal characters or is excessively long",
    "Colliding column names" = "whose names become identical when special characters are removed",
    "Mixed encoding"    = "with values in a legacy (non-UTF-8) encoding",
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
  # Careless-responding findings are respondent-level (not counted in the column
  # tally); if any were found, the result is at least yellow.
  if (nrow(careless_df) > 0 && tl == "green") tl <- "yellow"

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
  if (nrow(careless_df) > 0) {
    n_car   <- nrow(careless_df)                 # distinct respondents
    n_short <- sum(careless_df$short_scale_only %in% TRUE)
    summary_text <- paste0(summary_text,
      sprintf(" %d survey respondent%s %s flagged for possible careless responding%s.",
              n_car, plural(n_car), if (n_car == 1) "was" else "were",
              if (n_short > 0) sprintf(
                " (%d of them only via short-scale straightlining, which can be normal answering)",
                n_short) else ""))
  }
  if (nrow(demo_df) > 0) {
    kinds <- sort(unique(demo_df$demographic))
    summary_text <- paste0(summary_text,
      sprintf(" We detected demographic column%s for %s.",
              plural(length(kinds)),
              paste(tools::toTitleCase(kinds), collapse = ", ")))
  }
  if (length(qualtrics_specs) > 0) {
    n_drop <- sum(vapply(qualtrics_specs, function(s) s$n_drop %||% 0L, integer(1)))
    summary_text <- paste0(summary_text,
      sprintf(" %d file%s %s a Qualtrics survey export%s.",
              length(qualtrics_specs), plural(length(qualtrics_specs)),
              if (length(qualtrics_specs) == 1) "is" else "are",
              if (n_drop > 0)
                sprintf(" (%d row%s look like previews/unfinished responses to review)",
                        n_drop, plural(n_drop)) else ""))
  }

  report <- c(
    "This module runs automated data-quality checks (outliers, miscoded missing values, empty and constant columns, SPSS filter variables, category casing, problematic column names, mixed text encodings) on the extracted data files.",
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

  # Constant text columns that look like intentional file-level metadata
  # (version numbers, language codes, study labels). Listed so a reviewer can
  # scan them for anything that should vary, but not counted as issues.
  if (nrow(meta_const_df) > 0) {
    items <- sprintf("%s: %s = \"%s\"", meta_const_df$source_file,
                     meta_const_df$column, meta_const_df$value)
    n_meta <- length(items)
    shown <- utils::head(items, 15)
    report <- c(report, sprintf(
      "%d text column%s hold%s a single constant value and look%s like file-level metadata rather than a data problem (not counted as an issue): %s%s.",
      n_meta, plural(n_meta),
      if (n_meta == 1) "s" else "", if (n_meta == 1) "s" else "",
      paste(shown, collapse = "; "),
      if (n_meta > length(shown))
        sprintf(" and %d more", n_meta - length(shown)) else ""))
  }

  # Out-of-range values: one row per bounded (integer, few-level) column that
  # has values outside its apparent range — the signature of a data-entry error
  # or unrecoded missing code, not a statistical outlier. The apparent range and
  # a few example values let a reviewer judge each column at a glance.
  if (nrow(outlier_df) > 0) {
    out_tbl <- outlier_df |>
      dplyr::arrange(dplyr::desc(.data$n_outliers)) |>
      dplyr::transmute(
        File = .data$source_file, Column = .data$column, Label = .data$label,
        `N out-of-range` = .data$n_outliers,
        `Apparent range` = sprintf("[%g, %g]", .data$lower, .data$upper),
        `Out-of-range values` = .data$examples)
    n_out_cols <- nrow(outlier_df)
    n_out_vals <- sum(outlier_df$n_outliers)
    report <- c(report,
      "#### Out-of-Range Values",
      sprintf("%d bounded numeric column%s %s %d value%s outside %s apparent range — likely a data-entry error or an unrecoded missing code (e.g. a stray 99 in a 1–7 scale). Continuous columns (reaction times, scores, physiological measures) are not checked here, since a long tail is normal there.",
              n_out_cols, plural(n_out_cols),
              if (n_out_cols == 1) "has" else "have",
              n_out_vals, plural(n_out_vals),
              if (n_out_cols == 1) "its" else "their"),
      scroll_table(out_tbl, maxrows = 20))
  }

  # Distributions: a single faceted figure with one small histogram per numeric
  # column (outlier fences drawn as dashed lines), instead of a separate plot
  # per column. One render call keeps wide files fast; the facet count is capped
  # so the figure stays legible.
  if (length(plot_specs) > 0 && requireNamespace("ggplot2", quietly = TRUE) && plot_distributions == TRUE) {
    report <- c(report, "#### Distributions",
                data_validate_dist_facets(plot_specs, max_facets = max_facets))
  } else if (length(plot_specs) > 0 && plot_distributions == TRUE) {
    report <- c(report,
      "*Install the `ggplot2` package to see the distribution histograms.*")
  }

  # Qualtrics survey metadata: for each detected Qualtrics export, a summary of
  # the reliably-extractable response metadata — preview/unfinished rows to drop,
  # completion-time distribution, data-collection window, and PII fields present.
  if (length(qualtrics_specs) > 0) {
    report <- c(report, .dv_qualtrics_report(qualtrics_specs, length(previews)))
  }

  # Demographic columns: an informational inventory of the age / gender / race
  # variables detected in the data (not a problem flag). Helps a reviewer see at
  # a glance whether the shared data documents its sample's demographics.
  if (nrow(demo_df) > 0) {
    demo_tbl <- demo_df |>
      dplyr::transmute(
        File = .data$source_file, Column = .data$column,
        Demographic = tools::toTitleCase(.data$demographic))
    kinds <- sort(unique(demo_df$demographic))
    report <- c(report,
      "#### Demographic Variables",
      sprintf("We detected %d demographic column%s (%s) across the data file%s. These are the age/gender/race variables studies typically report; this is an inventory, not a problem flag.",
              nrow(demo_df), plural(nrow(demo_df)),
              paste(tools::toTitleCase(kinds), collapse = ", "),
              plural(length(previews))),
      scroll_table(demo_tbl, maxrows = 20))
  }

  # Careless responding: respondents flagged by longstring / IRV on a survey
  # scale block. Reported per respondent (with the scale and reason), so a
  # reviewer can inspect those rows in the raw data.
  if (nrow(careless_df) > 0) {
    car_tbl <- careless_df |>
      dplyr::transmute(
        Respondent = .data$respondent,
        `Blocks flagged` = .data$n_blocks_flagged,
        Scales = .data$scales,
        Reasons = .data$reasons,
        `Max longstring` = .data$max_longstring,
        IRV = .data$irv,
        `Short-scale only` = ifelse(.data$short_scale_only %in% TRUE,
                                    "yes", "no"))
    n_car   <- nrow(careless_df)
    n_short <- sum(careless_df$short_scale_only %in% TRUE)
    report <- c(report,
      "#### Careless Responding",
      sprintf("%d distinct respondent%s show signs of careless responding — straightlining (long runs of the same answer) or unusually flat/erratic answering — in at least one survey scale. One respondent can be flagged in several scales; the table below is **one row per person**, with how many scale blocks flagged them.",
              n_car, plural(n_car)),
      if (n_short > 0) sprintf(
        "Of these, **%d were flagged *only* by short-scale straightlining** (a run of identical answers on a scale of %d items or fewer). On a short, one-directional scale, answering consistently is often normal, coherent responding rather than carelessness — treat these as weak signals and inspect the actual responses before excluding anyone.",
        n_short, .dv_short_scale_max) else NULL,
      "These are prompts to inspect those rows, not definitive judgements.",
      scroll_table(car_tbl, maxrows = 20))
  } else if (!is.null(careless_note)) {
    report <- c(report, "#### Careless Responding", careless_note)
  }

  # ── 5. Summary table + return ────────────────────────────────────────────────
  summary_table <- data.frame(
    paper_id  = .pid(columns_df),
    column_n  = n_columns,
    flagged_n = n_flagged
  )

  qualtrics_df <- if (length(qualtrics_specs) > 0)
    dplyr::bind_rows(lapply(qualtrics_specs, function(s) data.frame(
      source_file = s$source_file, n_rows = s$n_rows,
      n_drop = s$n_drop, median_seconds = s$median_seconds,
      n_fast = s$n_fast, date_min = s$date_min, date_max = s$date_max,
      pii_fields = paste(s$pii_fields, collapse = ", ")))) else
    data.frame()

  list(
    table = findings_df,
    careless = careless_df,
    demographics = demo_df,
    qualtrics = qualtrics_df,
    summary_table = summary_table,
    na_replace = c(column_n = 0, flagged_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# ── Careless-responding helpers ───────────────────────────────────────────────

# Minimum respondents before careless indices are computed. (The minimum items
# per scale block is `.scale_min_items`, shared with codebook_check via the
# scale-block detection helpers in data_check_helpers.R.)
.dv_careless_min_items <- .scale_min_items
.dv_careless_min_rows  <- 30L

# Scale-block detection is shared with codebook_check: see .detect_scale_blocks,
# .scale_name_prefix and .scale_block_range in R/data_check_helpers.R.

# Run careless indices (longstring + IRV) on one scale block and return the
# respondents that look careless. `block` is a numeric data frame of items;
# `ids` is the identifier column aligned to its rows (or row numbers).
#
# A respondent is flagged when EITHER:
#   * their longest string of identical consecutive answers covers a large
#     fraction of the block (straightlining), OR
#   * their IRV (SD of responses) is an extreme low/high outlier for the block.
# Thresholds are relative to the block so a short scale is not over-flagged.
.dv_careless_block <- function(block, ids, scale, prefix) {
  block <- as.data.frame(lapply(block, function(x)
    suppressWarnings(as.numeric(as.character(x)))))
  n_items <- ncol(block)
  ls <- careless::longstring(block)
  iv <- careless::irv(block, na.rm = TRUE)

  # Straightlining: same answer for >= 80% of items (and at least 5 in a row).
  straight_cut <- max(5L, ceiling(0.8 * n_items))
  is_straight  <- !is.na(ls) & ls >= straight_cut
  # IRV outliers: Tukey fence on the block's IRV distribution (both tails —
  # near-zero = flat responding, very high = erratic/alternating).
  o <- data_check_outliers(iv, k = 1.5)
  is_irv_out <- !is.na(iv) &
    ((!is.na(o$lower) & iv < o$lower) | (!is.na(o$upper) & iv > o$upper))

  flagged <- which(is_straight | is_irv_out)
  if (length(flagged) == 0) return(NULL)
  reason <- ifelse(is_straight[flagged] & is_irv_out[flagged], "straightlining + IRV outlier",
             ifelse(is_straight[flagged], "straightlining", "IRV outlier"))
  data.frame(
    scale      = paste0(prefix, " (", scale, ", ", n_items, " items)"),
    respondent = as.character(ids[flagged]),
    longstring = ls[flagged],
    irv        = round(iv[flagged], 2),
    reason     = reason
  )
}

# Item count of a "prefix (min-max, N items)" scale label, or NA.
.dv_scale_n_items <- function(scale_label) {
  m <- regmatches(scale_label, regexpr("([0-9]+) items", scale_label))
  suppressWarnings(as.integer(sub(" items$", "", m)))
}

# A block is "short" (straightlining is weak evidence there) when it has few
# items — a run of identical answers is common and often coherent on a short
# unidirectional scale.
.dv_short_scale_max <- 7L

# Collapse the per-(respondent x block) careless table to one row per
# respondent. Columns: paper-level respondent id, source_file(s), how many
# blocks flagged them and on which scales, the distinct reasons, worst
# longstring / most extreme IRV, and `short_scale_only` — TRUE when every flag
# is short-scale straightlining (likely-benign consistent answering, not clear
# carelessness). Returns a zero-row frame with the same columns when empty.
.dv_careless_by_respondent <- function(block_df, previews = NULL,
                                       columns_df = NULL) {
  cols0 <- c("respondent", "source_file", "n_blocks_flagged", "scales",
             "reasons", "max_longstring", "irv", "short_scale_only")
  if (is.null(block_df) || nrow(block_df) == 0)
    return(stats::setNames(
      data.frame(matrix(nrow = 0, ncol = length(cols0))), cols0))

  block_df$.n_items   <- .dv_scale_n_items(block_df$scale)
  block_df$.is_short_straight <-
    grepl("straightlin", block_df$reason) &
    !grepl("IRV", block_df$reason) &
    !is.na(block_df$.n_items) & block_df$.n_items <= .dv_short_scale_max

  parts <- lapply(split(block_df, block_df$respondent), function(g) {
    data.frame(
      respondent       = g$respondent[1],
      source_file      = paste(sort(unique(g$source_file)), collapse = "; "),
      n_blocks_flagged = nrow(g),
      scales           = paste(sort(unique(g$scale)), collapse = "; "),
      reasons          = paste(sort(unique(g$reason)), collapse = "; "),
      max_longstring   = max(g$longstring, na.rm = TRUE),
      # The IRV furthest from a typical value (most diagnostic single number).
      irv              = g$irv[which.max(abs(g$irv - stats::median(g$irv, na.rm = TRUE)))][1],
      # TRUE only if EVERY block that flagged this person is short-scale
      # straightlining — i.e. no long-scale or IRV-based flag corroborates it.
      short_scale_only = all(g$.is_short_straight),
      stringsAsFactors = FALSE)
  })
  out <- dplyr::bind_rows(parts)
  # Most-flagged first.
  out[order(-out$n_blocks_flagged, -out$max_longstring), , drop = FALSE]
}

# ── Qualtrics metadata helpers ────────────────────────────────────────────────

# Locate a Qualtrics metadata column by its semantic tag (see
# .qualtrics_meta_cols in data_check_helpers.R); returns the column vector or
# NULL. Matching is by tag, so a renamed-but-recognised column is still found.
.dv_q_col <- function(df, tag) {
  tags <- .qualtrics_tag_cols(names(df))
  hit <- which(tags == tag)
  if (length(hit) == 0) NULL else df[[hit[1]]]
}

# Parse a Qualtrics datetime column (ISO "YYYY-MM-DD HH:MM:SS") to POSIXct.
# Returns all-NA (never errors) when the values are not datetimes: as.POSIXct
# with tryFormats *errors* rather than warns when no format matches, so we parse
# per-format and coalesce, guarded by tryCatch.
.dv_q_datetime <- function(x) {
  if (is.null(x)) return(NULL)
  xc <- as.character(x)
  out <- as.POSIXct(rep(NA_real_, length(xc)), tz = "UTC", origin = "1970-01-01")
  for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")) {
    miss <- is.na(out)
    if (!any(miss)) break
    parsed <- tryCatch(
      as.POSIXct(xc[miss], tz = "UTC", format = fmt),
      error = function(e) as.POSIXct(rep(NA_real_, sum(miss)), tz = "UTC",
                                     origin = "1970-01-01"))
    out[miss] <- parsed
  }
  out
}

# Summarise one Qualtrics export's response metadata. Returns a list of the
# reliably-extractable facts (or NULL if the file carries none of them):
#   n_rows, n_drop (preview/spam/unfinished rows), median_seconds, n_fast
#   (implausibly fast completions), date_min/date_max (collection window),
#   pii_fields (which Qualtrics PII columns are present).
.dv_qualtrics_summary <- function(df) {
  n_rows <- nrow(df)

  # Rows to review/drop: Status marks previews (1) and spam (2/8); Finished == 0
  # / FALSE and Progress < 100 mark incomplete responses. Any one qualifies.
  status <- .dv_q_col(df, "qualtrics_status")
  finished <- .dv_q_col(df, "qualtrics_finished")
  progress <- .dv_q_col(df, "qualtrics_progress")
  drop <- rep(FALSE, n_rows)
  if (!is.null(status)) {
    s <- suppressWarnings(as.numeric(as.character(status)))
    st <- tolower(trimws(as.character(status)))
    # Numeric coding (0 = real IP response) or text labels ("Survey Preview").
    drop <- drop | (!is.na(s) & s %in% c(1, 2, 8)) |
      grepl("preview|spam", st)
  }
  if (!is.null(finished)) {
    f <- tolower(trimws(as.character(finished)))
    drop <- drop | f %in% c("0", "false", "no")
  }
  if (!is.null(progress)) {
    p <- suppressWarnings(as.numeric(as.character(progress)))
    drop <- drop | (!is.na(p) & p < 100)
  }
  n_drop <- sum(drop)

  # Completion time: Duration (in seconds); fall back to EndDate - StartDate.
  dur <- suppressWarnings(as.numeric(as.character(.dv_q_col(df, "qualtrics_duration"))))
  if (all(is.na(dur))) {
    sd <- .dv_q_datetime(.dv_q_col(df, "qualtrics_start"))
    ed <- .dv_q_datetime(.dv_q_col(df, "qualtrics_end"))
    if (!is.null(sd) && !is.null(ed))
      dur <- as.numeric(difftime(ed, sd, units = "secs"))
  }
  dur <- dur[!is.na(dur) & dur >= 0]
  median_seconds <- if (length(dur) > 0) stats::median(dur) else NA_real_
  # Implausibly fast: under half the median AND under 2 minutes (a heuristic
  # speeding flag; only meaningful with enough completed responses).
  n_fast <- if (length(dur) >= 10 && !is.na(median_seconds))
    sum(dur < pmin(0.5 * median_seconds, 120)) else NA_integer_

  # Collection window: prefer RecordedDate, else StartDate.
  dt <- .dv_q_datetime(.dv_q_col(df, "qualtrics_recorded"))
  if (is.null(dt) || all(is.na(dt))) dt <- .dv_q_datetime(.dv_q_col(df, "qualtrics_start"))
  date_min <- date_max <- NA_character_
  if (!is.null(dt) && any(!is.na(dt))) {
    date_min <- format(min(dt, na.rm = TRUE), "%Y-%m-%d")
    date_max <- format(max(dt, na.rm = TRUE), "%Y-%m-%d")
  }

  # Which Qualtrics PII fields are present (drives a before-sharing prompt).
  pii_tags <- c(qualtrics_ip = "IP address", qualtrics_email = "email",
                qualtrics_lat = "location", qualtrics_lon = "location",
                qualtrics_externalref = "external reference (e.g. panel ID)",
                qualtrics_recipient = "recipient name")
  present <- unique(unname(pii_tags[intersect(
    .qualtrics_tag_cols(names(df)), names(pii_tags))]))

  list(n_rows = n_rows, n_drop = n_drop, median_seconds = median_seconds,
       n_fast = n_fast, date_min = date_min, date_max = date_max,
       pii_fields = present)
}

# Build the "Qualtrics Survey Metadata" report section from the per-file
# summaries produced by .dv_qualtrics_summary().
.dv_qualtrics_report <- function(specs, n_previews) {
  fmt_dur <- function(sec) {
    if (is.na(sec)) return("—")
    if (sec < 90) sprintf("%.0f s", sec)
    else if (sec < 5400) sprintf("%.1f min", sec / 60)
    else sprintf("%.1f h", sec / 3600)
  }
  rows <- lapply(specs, function(s) data.frame(
    File = s$source_file,
    Responses = s$n_rows,
    `Preview/unfinished` = s$n_drop,
    `Median time` = fmt_dur(s$median_seconds),
    `Very fast` = if (is.na(s$n_fast)) "—" else as.character(s$n_fast),
    `Collected` = if (is.na(s$date_min)) "—" else
      if (identical(s$date_min, s$date_max)) s$date_min else
        paste(s$date_min, "to", s$date_max),
    `PII fields` = if (length(s$pii_fields) == 0) "none" else
      paste(s$pii_fields, collapse = ", "),
    check.names = FALSE))
  tbl <- dplyr::bind_rows(rows)

  n_files <- length(specs)
  total_drop <- sum(vapply(specs, function(s) s$n_drop, integer(1)))
  intro <- sprintf(
    "%d of the %d data file%s %s a Qualtrics survey export. The table below summarises the response metadata Qualtrics records for every survey (not the substantive question columns): how many rows look like previews or unfinished responses that usually need dropping before analysis, the typical completion time (with a count of implausibly fast responses), the data-collection window, and which Qualtrics fields carry personal information to review before sharing.",
    n_files, n_previews, plural(n_previews),
    if (n_files == 1) "is" else "are")
  note <- if (total_drop > 0)
    sprintf(" Across these files, %d row%s %s flagged as a preview, spam, or unfinished response — check whether they should be excluded.",
            total_drop, plural(total_drop),
            if (total_drop == 1) "is" else "are") else ""

  c("#### Qualtrics Survey Metadata",
    paste0(intro, note),
    scroll_table(tbl, maxrows = 20))
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
    sprintf(paste0("\n\n*Showing the first %d of %d numeric columns (all were ",
                   "checked; only the figure is limited). Set `max_facets = %d` ",
                   "to plot them all.*"),
            length(specs), n_total, n_total) else ""
  paste0(img, note)
}
