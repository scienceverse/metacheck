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
#' For every **spreadsheet file** (`.xlsx`, `.xls`, OpenDocument `.ods`/`.fods`)
#' it also inspects the raw workbook for formatting practices that hurt
#' machine-readability and do not survive a plain CSV export: cells that use
#' fill colour to encode information, merged cells, fully empty rows, empty or
#' unnamed (blank-header) columns, and a header row that is not the first row of
#' the sheet. This runs on the workbook file itself (via `data_check`'s file
#' classification), independently of whether the file could be extracted into a
#' clean tabular preview — a merged banner cell or an offset header is often
#' exactly why extraction failed. Legacy `.xls` is a binary format with no XML to
#' inspect, so it is checked only for an offset header and reported as
#' un-inspectable for the rest, with conversion to `.xlsx`/`.ods` recommended.
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
  columns_df   <- get_prev_outputs("data_check", "table")
  previews     <- get_prev_outputs("data_check", "previews")
  structure_df <- get_prev_outputs("data_check", "structure")
  if (is.null(previews)) {
    mo <- if (!is.null(local_path)) {
      module_run(paper, "data_check", local_path = local_path,
                 local_only = local_only, model = model, params = params)
    } else {
      module_run(paper, "data_check", local_only = local_only,
                 model = model, params = params)
    }
    columns_df   <- mo$table
    previews     <- mo$previews
    structure_df <- mo$structure
  }
  labels_df <- get_prev_outputs("codebook_check", "table")

  # ── 1b. Spreadsheet formatting checks (.xlsx/.xls/.ods/.fods) ──────────────
  # Runs on the RAW workbook file via structure_df$file_location, independently
  # of whether data_check could extract a clean `preview` from it: a merged
  # banner cell or an offset header is often exactly why extraction failed, so
  # this must not depend on previews existing. See .dv_spreadsheet_findings()
  # below (ported from the former spreadsheet_check module) for what each check
  # covers (colour coding, merged cells, empty rows/columns, offset header,
  # non-rectangular data, un-inspectable legacy .xls).
  spreadsheet_result <- .dv_spreadsheet_findings(structure_df)
  spreadsheet_findings_df <- spreadsheet_result$findings
  n_spreadsheet_files <- spreadsheet_result$n_files

  empty <- function(text) {
    n_flagged_files_spreadsheet <- length(unique(spreadsheet_findings_df$source_file))
    list(
      table = spreadsheet_findings_df,
      summary_table = data.frame(
        paper_id = .pid(columns_df, structure_df), column_n = 0, flagged_n = 0,
        spreadsheet_file_n = n_spreadsheet_files,
        spreadsheet_flagged_file_n = n_flagged_files_spreadsheet),
      na_replace = c(column_n = 0, flagged_n = 0,
                     spreadsheet_file_n = 0, spreadsheet_flagged_file_n = 0),
      traffic_light = if (nrow(spreadsheet_findings_df) > 0) "yellow" else "na",
      summary_text = if (nrow(spreadsheet_findings_df) > 0)
        paste(text, .dv_spreadsheet_summary_text(spreadsheet_findings_df, n_spreadsheet_files))
      else text,
      report = if (nrow(spreadsheet_findings_df) > 0)
        .dv_spreadsheet_report(spreadsheet_findings_df, n_spreadsheet_files)
      else NULL
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
  # Spreadsheet-formatting findings (file/sheet-level: colour, merges, empty
  # rows/columns, offset header, ...) join the same table. They carry
  # column = NA (the issue is not about one column), so they show up in the
  # "Potential Issues" table but are excluded from n_flagged/check_counts
  # below, which are column-level tallies.
  findings_df <- dplyr::bind_rows(findings_df, spreadsheet_findings_df)

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
  # Column-level findings only (spreadsheet findings carry column = NA and are
  # a file/sheet-level concern, not a column one — tallied separately below).
  column_findings_df <- findings_df[!is.na(findings_df$column), , drop = FALSE]
  n_flagged <- length(unique(paste(column_findings_df$source_file,
                                   column_findings_df$column)))
  n_flagged_files_spreadsheet <- length(unique(spreadsheet_findings_df$source_file))

  # Per-check tally: how many distinct columns each check flagged (a column can
  # be flagged by several checks, so these overlap and need not sum to n_flagged).
  check_counts <- if (nrow(column_findings_df) > 0) {
    column_findings_df |>
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
  # Spreadsheet-formatting findings are file-level (not counted in the column
  # tally either); if any were found, the result is at least yellow (matches
  # the former spreadsheet_check module, which never went red on its own).
  if (nrow(spreadsheet_findings_df) > 0 && tl == "green") tl <- "yellow"

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
  if (nrow(spreadsheet_findings_df) > 0) {
    summary_text <- paste(summary_text,
      .dv_spreadsheet_summary_text(spreadsheet_findings_df, n_spreadsheet_files))
  }

  report <- c(
    "This module runs automated data-quality checks (outliers, miscoded missing values, empty and constant columns, SPSS filter variables, category casing, problematic column names, mixed text encodings, spreadsheet formatting) on the extracted data files.",
    sprintf("We examined %d column%s across %d data file%s.",
            n_columns, plural(n_columns),
            length(previews), plural(length(previews)))
  )

  # Single "Issues identified" table: one row per (file, column) that has at
  # least one issue, one cell listing every issue found for it (icon + short
  # label, hover for the full detail sentence). PII findings — "Personal info
  # (values)" and "Personal info (column name)" are two independent detectors
  # (one scans cell contents, one scans the column name) but are merged to a
  # single displayed "Personally Identifying Information" label here, since the
  # distinction is not one readers need — see .dv_issue_cell().
  # Spreadsheet-formatting findings (column = NA) join the same table: their
  # "column" cell shows the sheet name instead.
  all_issue_findings <- dplyr::bind_rows(column_findings_df, spreadsheet_findings_df)
  if (nrow(all_issue_findings) > 0) {
    issues_tbl <- all_issue_findings |>
      dplyr::mutate(.col_display = ifelse(is.na(.data$column), .data$label, .data$column)) |>
      dplyr::arrange(.data$source_file, .data$.col_display) |>
      dplyr::summarise(
        Issues = .dv_issue_cell(.data$check, .data$detail),
        .by = c(source_file, .col_display)
      ) |>
      dplyr::transmute(File = .data$source_file, Column = .data$.col_display,
                       Issues = .data$Issues)
    report <- c(report, "#### Issues Identified",
                sprintf("%d file/column combination%s %s at least one issue.",
                        nrow(issues_tbl), plural(nrow(issues_tbl)),
                        if (nrow(issues_tbl) == 1) "has" else "have"),
                scroll_table(issues_tbl, maxrows = 20, escape = FALSE))
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

  # Demographic columns (age/gender/race) are still computed into `demo_df`
  # and returned via the `demographics` field for programmatic consumers, but
  # are no longer rendered as their own report section — they are informational
  # rather than an issue, and do not belong in the "Issues Identified" table.

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
    flagged_n = n_flagged,
    spreadsheet_file_n = n_spreadsheet_files,
    spreadsheet_flagged_file_n = n_flagged_files_spreadsheet
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
    na_replace = c(column_n = 0, flagged_n = 0,
                   spreadsheet_file_n = 0, spreadsheet_flagged_file_n = 0),
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

# ── Issues Identified table (single-column display) ───────────────────────────

# Icon shown before each issue's short label in the "Issues Identified" table.
# "Personal info (values)" and "Personal info (column name)" are two
# independent detectors (data_check_pii_values() scans cell contents,
# data_check_pii_name() scans only the column name — a column can trip either
# without the other) but share one icon/label here: the distinction between
# "the data itself looks sensitive" and "the name suggests it might be" is not
# one a reader of this table needs, both mean the same thing — review before
# sharing. The two checks themselves are untouched; only this DISPLAY mapping
# merges them.
.dv_issue_icon <- c(
  "Values outside the scale"       = "\U0001F4C8",  # 📈 out-of-range
  "Miscoded missing"                = "\U00002753",  # ❓
  "Constant"                        = "\U0001F7F0",  # 🟰
  "Empty column"                    = "\U00002B1C",  # ⬜
  "SPSS filter variable"            = "\U0001F9EE",  # 🧮
  "Case issues"                     = "\U0001F524",  # 🔤
  "Whitespace"                      = "\U00002194\U0000FE0F",  # ↔️
  "Numeric as text"                 = "#\U0000FE0F\U000020E3",  # #️⃣
  "Problematic column name"         = "\U0001F3F7\U0000FE0F",  # 🏷️
  "Colliding column names"          = "\U0001F465",  # 👥
  "Mixed encoding"                  = "\U0001F523",  # 🔣
  "Personal info (values)"          = "\U0001F512",  # 🔒
  "Personal info (column name)"     = "\U0001F512",  # 🔒
  "Geographic coordinates"          = "\U0001F30D",  # 🌍
  "Free-text (may hold PII)"        = "\U0001F512",  # 🔒
  "Not a rectangular dataset"       = "\U0001F4D0",  # 📐
  "Header not on first row"         = "\U0001F4CB",  # 📋
  "Un-inspectable (.xls)"           = "\U000026A0\U0000FE0F",  # ⚠️
  "Unreadable"                      = "\U000026A0\U0000FE0F",  # ⚠️
  "Colour coding"                   = "\U0001F3A8",  # 🎨
  "Merged cells"                    = "\U0001F517",  # 🔗
  "Empty rows"                      = "\U00002B1C",  # ⬜
  "Empty or unnamed columns"        = "\U00002B1C"   # ⬜
)

# Checks whose displayed label/icon should collapse together (PII, values vs.
# name) — every key in this vector maps to the same shown label.
.dv_issue_merge_label <- c(
  "Personal info (values)"      = "Personally Identifying Information",
  "Personal info (column name)" = "Personally Identifying Information",
  "Free-text (may hold PII)"    = "Personally Identifying Information"
)

# Build the "Issues" cell HTML for one (file, column) group: one line per
# issue (icon + short label), each wrapped in a span whose `title` attribute
# carries the full original `detail` sentence(s) for hover. Several issues on
# the same column stack as several lines in the same cell (via <br>, since
# scroll_table() replaces "\n" with "<br>" — see report-helpers.R). Checks that
# share a merged display label (the PII trio) collapse to ONE line.
.dv_issue_cell <- function(check, detail) {
  # dplyr::summarise() calls this once per (source_file, column) GROUP, with
  # `check`/`detail` as the group's vectors — build one cell string per group.
  #
  # Checks that share a merged display label (currently just the PII trio —
  # see .dv_issue_merge_label) are grouped FIRST, by that label, before
  # building any HTML: two PII detectors firing on the same column must become
  # ONE line with both details in its tooltip, not two lines that only look
  # identical by accident (their `detail` text differs even though the label
  # is shared, so de-duplicating the finished HTML strings would not catch
  # this — grouping by label upfront is what makes the merge actually happen).
  display_label <- ifelse(!is.na(.dv_issue_merge_label[check]),
                          unname(.dv_issue_merge_label[check]), check)
  groups <- split(seq_along(check), display_label)

  lines <- vapply(groups, function(idx) {
    lbl <- display_label[idx[[1]]]
    # One icon per group: the first check's icon (all PII checks share one
    # anyway; a future merge group with genuinely different icons would still
    # need a single glyph here, so "first wins" is the deliberate rule).
    icon <- .dv_issue_icon[check[idx[[1]]]]
    icon <- if (is.na(icon)) "\U00002139\U0000FE0F" else unname(icon)
    # First clause of each detail (up to the first period/semicolon), capped
    # in length, as the scannable summary; the full sentence(s) go in the
    # hover title below instead.
    short <- vapply(idx, function(i)
      sub("[.;].*$", "", detail[[i]]), character(1))
    short <- short[!is.na(short) & nzchar(trimws(short))]
    short <- unique(short)
    short <- ifelse(nchar(short) > 70, paste0(substr(short, 1, 67), "..."), short)
    short_txt <- if (length(short) > 0)
      paste0(" — ", paste(short, collapse = "; ")) else ""
    # Full sentences from every finding in the group, for the hover tooltip.
    full_detail <- paste(unique(detail[idx]), collapse = " ")
    # detail/label can echo values read from the paper's own data (a category
    # string, a file name, ...), so both are HTML-escaped before going into
    # the report: .spv_html_escape() (R/spv.R) handles &/</>; the title
    # attribute is single-quoted, so a single quote needs its own escape too.
    detail_esc <- gsub("'", "&#39;", .spv_html_escape(full_detail), fixed = TRUE)
    text_esc   <- .spv_html_escape(paste0(lbl, short_txt))
    sprintf("<span title='%s'>%s %s</span>", detail_esc, icon, text_esc)
  }, character(1))

  paste(lines, collapse = "\n")
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

# ── Spreadsheet formatting checks ─────────────────────────────────────────────
#
# Formerly the standalone `spreadsheet_check` module; merged into data_validate
# because both check "additional categories of things that can be wrong" with a
# data file — this half just inspects the raw workbook CONTAINER (colour, merges,
# empty rows/columns, offset headers) rather than the CONTENT data_validate's
# other checks read from data_check's already-parsed `previews`. Those file-level
# facts (e.g. a cell's fill colour) are gone by the time a file becomes a data
# frame, so this reads the raw .xlsx/.ods zip/XML directly via structure_df's
# file_location, same as before.
#
# Findings are returned in data_validate's shared findings shape (source_file,
# column, label, check, detail) — column = NA for these, since the issue is
# file/sheet-level, not about one column — so they merge into the same
# `findings_df` / "Potential Issues" reporting path as every other check here.

.dv_spreadsheet_exts <- c("xlsx", "xls", "ods", "fods")

# Run the spreadsheet-formatting checks over every spreadsheet file in
# structure_df (data_check's file-classification table) that has a readable
# local copy. Returns list(findings, n_files): `findings` in the shared
# (source_file, column, label, check, detail) shape (0 rows when none found or
# no spreadsheet files exist), `n_files` the count of spreadsheet files examined
# (0 when none).
.dv_spreadsheet_findings <- function(structure_df) {
  empty_findings <- data.frame(source_file = character(0), column = character(0),
                               label = character(0), check = character(0),
                               detail = character(0))

  xl_rows <- if (!is.null(structure_df) && nrow(structure_df) > 0) {
    structure_df[
      tolower(tools::file_ext(structure_df$file_name)) %in% .dv_spreadsheet_exts &
        !is.na(structure_df$file_location) &
        nzchar(structure_df$file_location) &
        file.exists(structure_df$file_location %||% ""),
      , drop = FALSE
    ]
  } else structure_df[0, , drop = FALSE]

  if (is.null(xl_rows) || nrow(xl_rows) == 0)
    return(list(findings = empty_findings, n_files = 0L))

  n_files <- nrow(xl_rows)
  findings <- list()

  for (i in seq_len(n_files)) {
    fname <- xl_rows$file_name[i]
    path  <- xl_rows$file_location[i]
    ext   <- tolower(tools::file_ext(fname))

    # data_check flagged this file as not a usable rectangular dataset (a coding
    # worksheet: mostly free text and/or almost all empty). It is still inspected
    # for formatting below; here we add the structural note so the author knows
    # the file needs restructuring, not just reformatting.
    if (isFALSE(xl_rows$tabular_usable[i])) {
      reason <- xl_rows$non_tabular_reason[i] %||% NA_character_
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Not a rectangular dataset",
        detail = sprintf(
          "This file reads as a table but is not a usable dataset%s. Store the data as a plain rectangular table (one header row, one column per variable) with a codebook.",
          if (!is.na(reason)) sprintf(" (%s)", reason) else ""))
    }

    # Offset header: a banner / blank / units row above the real column header, so
    # the file does not read as a clean table. Reported so the AUTHOR removes the
    # junk row(s) at source; metacheck also repairs it in-memory for its own checks.
    #
    # This runs for EVERY format, including .xls: it reads the first rows through
    # readxl/readODS rather than the XML, so it does not need the zipped-XML
    # structure the style-level checks below require.
    oh <- tryCatch(.dv_spreadsheet_offset_header(path, ext), error = function(e) NULL)
    if (!is.null(oh)) {
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Header not on first row",
        detail = sprintf(
          "The column header is on row %d; above it is %s. Remove the row%s above the header so the first row of the sheet is the column header — otherwise the file reads with invented column names (…1, …4) and the data mis-types.",
          oh$header_row, oh$detail, plural(oh$n_above)))
    }

    # Style-level checks (colour, merges, empty rows/columns) need the document
    # XML. .xlsx and .ods/.fods both provide it; binary .xls does not.
    if (ext == "xls") {
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Un-inspectable (.xls)",
        detail = "Legacy .xls format: colour, merged cells and empty rows/columns cannot be inspected. Convert to .xlsx or .ods for a full check.")
      next
    }

    insp <- tryCatch(
      if (ext %in% c("ods", "fods")) .dv_ods_inspect(path) else .dv_excel_inspect(path),
      error = function(e) NULL)
    if (is.null(insp)) {
      findings[[length(findings) + 1L]] <- data.frame(
        source_file = fname, column = NA_character_, label = NA_character_,
        check = "Unreadable",
        detail = sprintf("The file could not be parsed as a %s workbook.",
                         if (ext %in% c("ods", "fods")) "OpenDocument" else ".xlsx"))
      next
    }

    for (s in insp$sheets) {
      if (s$color_cells > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Colour coding",
          detail = sprintf("%d cell%s use fill colour to encode information; colour is lost on CSV export.",
                           s$color_cells, plural(s$color_cells)))
      }
      if (length(s$merges) > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Merged cells",
          detail = sprintf("%d merged range%s (%s) break the rectangular grid.",
                           length(s$merges), plural(length(s$merges)),
                           paste(utils::head(s$merges, 5), collapse = ", ")))
      }
      if (s$empty_rows > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Empty rows",
          detail = sprintf("%d fully empty row%s inside the data range.",
                           s$empty_rows, plural(s$empty_rows)))
      }
      if (s$empty_cols > 0) {
        findings[[length(findings) + 1L]] <- data.frame(
          source_file = fname, column = NA_character_, label = s$name,
          check = "Empty or unnamed columns",
          detail = sprintf("%d column%s %s empty or have a blank header.",
                           s$empty_cols, plural(s$empty_cols),
                           if (s$empty_cols == 1) "is" else "are"))
      }
    }
  }

  findings_df <- if (length(findings) > 0) dplyr::bind_rows(findings) else empty_findings
  list(findings = findings_df, n_files = n_files)
}

# Human-readable rollup of the spreadsheet findings, appended to data_validate's
# summary_text. Mirrors the former spreadsheet_check module's summary wording.
.dv_spreadsheet_summary_text <- function(findings_df, n_files) {
  n_flagged_files <- length(unique(findings_df$source_file))
  sprintf("%d of %d spreadsheet file%s %s at least one formatting issue (colour coding, merged cells, empty rows/columns, or an offset header).",
          n_flagged_files, n_files, plural(n_files),
          if (n_flagged_files == 1) "has" else "have")
}

# Report section for spreadsheet-formatting findings, appended after the
# per-column "Potential Issues" table.
.dv_spreadsheet_report <- function(findings_df, n_files) {
  n_flagged_files <- length(unique(findings_df$source_file))
  tbl <- findings_df |>
    dplyr::transmute(File = .data$source_file, Sheet = .data$label,
                     Issue = .data$check, Detail = .data$detail)
  c("#### Spreadsheet Formatting",
    sprintf("We examined %d spreadsheet file%s in the repository; %d %s at least one formatting issue.",
            n_files, plural(n_files), n_flagged_files,
            if (n_flagged_files == 1) "has" else "have"),
    scroll_table(tbl, maxrows = 20),
    "Spreadsheet formatting such as colour, merged cells, and blank rows/columns is not preserved when data are read programmatically or exported to CSV. Store data as a plain rectangular table (one header row, one column per variable, no colour-encoded meaning) so it is machine-readable.")
}

# Detect an OFFSET HEADER: a banner / blank / units / repeated-label row sitting
# ABOVE the real column header, so the file does not read as a clean rectangular
# table (the reader takes the junk row as the header and invents …N names, or
# spreads one label — CDA merged across 110 columns — into CDA…1 … CDA…110).
#
# Reuses the same detector as the read-time repair (data_promote_header_row /
# .detect_header_row) so the flag and the repair cannot disagree about where the
# header is. Returns NULL when the header is already row 1, else a one-line human
# description of what sits above it, for the researcher to remove at source.
.dv_spreadsheet_offset_header <- function(path, ext = tolower(tools::file_ext(path))) {
  raw <- if (ext %in% c("ods", "fods")) {
    # readODS is in Suggests: without it an .ods simply skips this check (the
    # xml2-based style checks still run), rather than erroring.
    if (!requireNamespace("readODS", quietly = TRUE)) return(NULL)
    # No `range=`: a fixed range like "A1:Z6" silently TRUNCATES the sheet to 26
    # columns, which would hide the header on a wide export (a 30-column sheet
    # reads back as 26). Read the sheet and cap the rows in R instead.
    tryCatch({
      d <- as.data.frame(suppressWarnings(readODS::read_ods(
        path, col_names = FALSE, .name_repair = "minimal")))
      utils::head(d, 6L)
    }, error = function(e) NULL)
  } else {
    tryCatch(as.data.frame(suppressWarnings(readxl::read_excel(
      path, col_names = FALSE, n_max = 6L, col_types = "text",
      .name_repair = "minimal"))), error = function(e) NULL)
  }
  if (is.null(raw) || nrow(raw) < 2 || ncol(raw) < 2) return(NULL)
  rows <- lapply(seq_len(nrow(raw)), function(i) as.character(raw[i, , drop = TRUE]))
  det  <- .detect_header_row(rows)
  if (det$header_row <= 1L || length(det$stripped) == 0) return(NULL)

  # Describe each stripped row: a repeated banner ("CDA" × 110), a near-empty
  # spacer, or a stale placeholder header. Keep it short and concrete.
  describe <- function(v) {
    vals <- trimws(as.character(v)); nz <- vals[nzchar(vals) & !is.na(vals)]
    if (length(nz) == 0) return("an empty row")
    dup <- .row_duplication(v)
    if (dup >= 0.6 && length(unique(nz)) <= 3)
      return(sprintf("\"%s\" repeated across %d column%s",
                     paste(unique(nz), collapse = "\"/\""),
                     length(nz), plural(length(nz))))
    if (mean(.is_placeholder_name(v)) >= 0.5)
      return("a row of placeholder names from an earlier mis-read")
    sprintf("a partial label row (%s%s)",
            paste(utils::head(nz, 3), collapse = ", "),
            if (length(nz) > 3) ", …" else "")
  }
  descr <- vapply(det$stripped, describe, character(1))
  list(header_row = det$header_row, n_above = length(det$stripped),
       detail = paste(descr, collapse = "; then "))
}

# Inspect one .xlsx file by reading it as a zip of XML parts. Returns a list with
# `sheets`, each a list(name, color_cells, merges, empty_rows, empty_cols), or
# NULL on failure. Uses only xml2 (no readxl/openxlsx dependency for the
# style-level checks); the empty-row/column checks read cell values from the
# sheet XML directly.
.dv_excel_inspect <- function(path) {
  if (!file.exists(path)) return(NULL)
  tmp <- tempfile("xlsx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)

  xl <- file.path(tmp, "xl")
  if (!dir.exists(xl)) return(NULL)

  # Which cell style indices (s=) reference a non-default fill colour?
  colored_styles <- .dv_excel_colored_styles(file.path(xl, "styles.xml"))

  # Sheet name + file mapping (workbook order matches sheetN.xml order well
  # enough for reporting; fall back to the file stem).
  wb_path <- file.path(xl, "workbook.xml")
  sheet_names <- if (file.exists(wb_path)) {
    wb <- tryCatch(xml2::read_xml(wb_path), error = function(e) NULL)
    if (is.null(wb)) character(0) else {
      xml2::xml_attr(xml2::xml_find_all(wb, ".//*[local-name()='sheet']"), "name")
    }
  } else character(0)

  sheet_files <- sort(list.files(file.path(xl, "worksheets"),
                                 pattern = "^sheet[0-9]+\\.xml$",
                                 full.names = TRUE))
  sheets <- list()
  for (j in seq_along(sheet_files)) {
    nm <- if (j <= length(sheet_names)) sheet_names[[j]] else
      tools::file_path_sans_ext(basename(sheet_files[[j]]))
    sheets[[length(sheets) + 1L]] <-
      .dv_excel_inspect_sheet(sheet_files[[j]], nm, colored_styles)
  }

  list(sheets = sheets)
}

# Return the 0-based cell-style indices (positions in cellXfs) whose fill is a
# non-default colour. Default fills are patternType none/gray125 or black/white
# fgColor; anything else counts as colour coding.
.dv_excel_colored_styles <- function(styles_path) {
  if (!file.exists(styles_path)) return(integer(0))
  st <- tryCatch(xml2::read_xml(styles_path), error = function(e) NULL)
  if (is.null(st)) return(integer(0))

  fills <- xml2::xml_find_all(st, ".//*[local-name()='fills']/*[local-name()='fill']")
  fill_is_color <- vapply(fills, function(fl) {
    fg <- xml2::xml_find_first(fl, ".//*[local-name()='fgColor']")
    if (inherits(fg, "xml_missing")) return(FALSE)
    rgb <- xml2::xml_attr(fg, "rgb")
    # A themed colour (no rgb) or a plain black/white fill is not "colour coding".
    !is.na(rgb) && nzchar(rgb) &&
      !toupper(rgb) %in% c("FF000000", "FFFFFFFF", "00000000")
  }, logical(1))
  colored_fill_ids <- which(fill_is_color) - 1L    # 0-based fillId

  if (length(colored_fill_ids) == 0) return(integer(0))

  xfs <- xml2::xml_find_all(st, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
  xf_fill <- suppressWarnings(as.integer(xml2::xml_attr(xfs, "fillId")))
  which(xf_fill %in% colored_fill_ids) - 1L         # 0-based style index (s=)
}

# Inspect one worksheet XML: count colour-coded cells, merged ranges, fully
# empty rows, and empty/unnamed columns. `colored_styles` is the set of 0-based
# style indices that use a colour fill.
#
# All cell-level facts are extracted with a handful of whole-document xml2 calls
# and then reduced with base-R vector ops. A previous version ran an XPath query
# per cell to test whether it held a value, which is O(rows x cols) XPath
# evaluations — minutes on a wide (e.g. 300 x 2000) Qualtrics export. Here the
# populated cells come from a single `.//c[v|is]` query, and row/column identity
# is parsed from the vector of cell references.
.dv_excel_inspect_sheet <- function(sheet_path, name, colored_styles) {
  blank <- list(name = name, color_cells = 0L, merges = character(0),
                empty_rows = 0L, empty_cols = 0L)
  sh <- tryCatch(xml2::read_xml(sheet_path), error = function(e) NULL)
  if (is.null(sh)) return(blank)

  # Every cell, and its reference (e.g. "B12"). One query for all cells.
  cells    <- xml2::xml_find_all(sh, ".//*[local-name()='c']")
  cell_ref <- xml2::xml_attr(cells, "r")

  # Populated cells: those with a <v> (value) or <is> (inline string) child.
  # One query returns exactly the non-empty cells, avoiding per-cell XPath.
  val_cells <- xml2::xml_find_all(
    sh, ".//*[local-name()='c'][*[local-name()='v'] or *[local-name()='is']]")
  val_ref <- xml2::xml_attr(val_cells, "r")

  # Colour-coded cells: cells whose style index is in colored_styles.
  color_cells <- 0L
  if (length(colored_styles) > 0 && length(cells) > 0) {
    cell_s <- suppressWarnings(as.integer(xml2::xml_attr(cells, "s")))
    color_cells <- sum(cell_s %in% colored_styles, na.rm = TRUE)
  }

  # Merged ranges.
  merges <- xml2::xml_attr(
    xml2::xml_find_all(sh, ".//*[local-name()='mergeCell']"), "ref")
  merges <- merges[!is.na(merges)]

  # Split cell references into column letters and row numbers (vectorised).
  ref_col <- function(ref) sub("[0-9]+$", "", ref)
  ref_row <- function(ref) suppressWarnings(as.integer(sub("^[A-Za-z]+", "", ref)))
  val_col <- ref_col(val_ref)
  val_rownum <- ref_row(val_ref)

  # Empty rows: rows inside the populated range that carry no value. Only blanks
  # that fall between the first and last populated row are counted (trailing
  # blank <row> elements in the XML are rare and not meaningful).
  empty_rows <- 0L
  if (length(val_rownum) > 0) {
    populated <- sort(unique(val_rownum))
    if (length(populated) > 1)
      empty_rows <- (max(populated) - min(populated) + 1L) - length(populated)
  }

  # Empty / unnamed columns. The header is the first populated row; a column is
  # problematic if its header cell is blank, or it has a header but no value in
  # any row below. Column identity is the letter part of the cell reference.
  empty_cols <- 0L
  if (length(val_ref) > 0) {
    hdr_row  <- min(val_rownum)
    hdr_cols <- val_col[val_rownum == hdr_row]                 # cols with a header value
    all_cols <- unique(ref_col(cell_ref))                      # every column that appears
    body_cols <- unique(val_col[val_rownum > hdr_row])         # cols with a body value
    blank_header  <- setdiff(all_cols, hdr_cols)               # column present but no header
    header_no_body <- setdiff(hdr_cols, body_cols)             # header but empty below
    empty_cols <- length(unique(c(blank_header, header_no_body)))
  }

  list(name = name, color_cells = color_cells, merges = merges,
       empty_rows = empty_rows, empty_cols = empty_cols)
}

# ── OpenDocument (.ods / .fods) ───────────────────────────────────────────────
#
# ODF stores the same facts as OOXML but in a structurally different way, so the
# .dv_excel_* parsers above cannot simply be pointed at it:
#
#   * ALL sheets live in one content.xml (not one sheetN.xml per sheet);
#   * cells carry NO reference attribute (no r="B12") — position is IMPLICIT in
#     document order, so row/column indices must be reconstructed by counting;
#   * blank runs are COMPRESSED: `table:number-columns-repeated="3"` stands for
#     three cells and `table:number-rows-repeated="5"` for five rows. Counting
#     elements naively would report one empty row where there are five;
#   * colour is `fo:background-color` on a named cell style, not a fillId;
#   * merges are `table:number-columns-spanned`/`-rows-spanned` on the anchor
#     cell (followed by <table:covered-table-cell> placeholders), not a
#     ready-made "A1:B1" range string — so the range label is synthesised here
#     to match the xlsx report wording.
#
# Returns the SAME shape as .dv_excel_inspect(): list(sheets = list(list(name,
# color_cells, merges, empty_rows, empty_cols))), so the module body treats the
# two formats identically.
.dv_ods_inspect <- function(path) {
  if (!file.exists(path)) return(NULL)

  # .fods is a single flat XML file; .ods is a zip whose content.xml holds it.
  ext <- tolower(tools::file_ext(path))
  doc <- if (identical(ext, "fods")) {
    tryCatch(xml2::read_xml(path), error = function(e) NULL)
  } else {
    tmp <- tempfile("ods_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                   error = function(e) FALSE)
    if (!ok) return(NULL)
    cx <- file.path(tmp, "content.xml")
    if (!file.exists(cx)) return(NULL)
    tryCatch(xml2::read_xml(cx), error = function(e) NULL)
  }
  if (is.null(doc)) return(NULL)

  colored <- .dv_ods_colored_styles(doc)

  # One <table:table> per sheet, in workbook order.
  tables <- xml2::xml_find_all(doc, ".//*[local-name()='table']")
  # Guard: nested tables (a table inside a cell) would double-count. Keep only
  # tables whose parent is the spreadsheet body.
  keep <- vapply(tables, function(tb) {
    p <- xml2::xml_parent(tb)
    identical(xml2::xml_name(p), "spreadsheet")
  }, logical(1))
  tables <- tables[keep]
  if (length(tables) == 0) return(list(sheets = list()))

  sheets <- lapply(seq_along(tables), function(j) {
    nm <- xml2::xml_attr(tables[[j]], "name")
    if (is.na(nm) || !nzchar(nm)) nm <- paste0("Sheet", j)
    .dv_ods_inspect_sheet(tables[[j]], nm, colored)
  })
  list(sheets = sheets)
}

# Names of cell styles whose background is a real colour. Mirrors
# .dv_excel_colored_styles(): a style counts only if it sets an explicit
# background that is not the default/neutral (transparent, white, black). Both
# the automatic styles (used by cells) and any common styles are scanned.
.dv_ods_colored_styles <- function(doc) {
  sty <- xml2::xml_find_all(
    doc, ".//*[local-name()='style'][@*[local-name()='family']='table-cell']")
  if (length(sty) == 0) return(character(0))
  nm <- xml2::xml_attr(sty, "name")
  bg <- vapply(sty, function(s) {
    p <- xml2::xml_find_first(s, ".//*[local-name()='table-cell-properties']")
    if (inherits(p, "xml_missing")) return(NA_character_)
    xml2::xml_attr(p, "background-color")
  }, character(1))
  is_col <- !is.na(bg) & nzchar(bg) &
    !tolower(bg) %in% c("transparent", "none", "#ffffff", "#fff",
                        "#000000", "#000")
  nm[is_col & !is.na(nm)]
}

# Convert a 1-based column index to spreadsheet letters (1 -> A, 27 -> AA), so
# merged ranges can be reported as "A1:B1" exactly like the xlsx path.
.dv_ods_col_letter <- function(i) {
  out <- character(length(i))
  for (k in seq_along(i)) {
    n <- i[[k]]; s <- ""
    while (n > 0) {
      r <- (n - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      n <- (n - 1L) %/% 26L
    }
    out[[k]] <- s
  }
  out
}

# Inspect one <table:table>. Walks rows/cells in document order, expanding the
# repeat counters, and records for each populated cell its (row, col) position —
# reconstructing the coordinates OOXML gives for free. Downstream logic then
# matches .dv_excel_inspect_sheet() exactly.
.dv_ods_inspect_sheet <- function(tbl, name, colored) {
  blank <- list(name = name, color_cells = 0L, merges = character(0),
                empty_rows = 0L, empty_cols = 0L)

  rows <- xml2::xml_find_all(tbl, "./*[local-name()='table-row']")
  if (length(rows) == 0) return(blank)

  int_attr <- function(node, a, default = 1L) {
    v <- suppressWarnings(as.integer(xml2::xml_attr(node, a)))
    if (is.na(v) || v < 1L) default else v
  }

  val_row <- integer(0); val_col <- integer(0)   # populated cell coordinates
  seen_col <- integer(0)                          # every column that appears
  color_cells <- 0L
  merges <- character(0)
  r <- 0L   # 1-based row cursor

  # A trailing run of empty rows is padding (LibreOffice writes rows out to the
  # sheet limit, e.g. number-rows-repeated="1048570"); it is not data structure,
  # and the empty-row count below only looks BETWEEN populated rows anyway.
  for (ri in seq_along(rows)) {
    row <- rows[[ri]]
    rep_r <- int_attr(row, "number-rows-repeated")
    cells <- xml2::xml_find_all(
      row, "./*[local-name()='table-cell' or local-name()='covered-table-cell']")

    cc <- 0L   # 1-based column cursor within this row
    for (ci in seq_along(cells)) {
      cell  <- cells[[ci]]
      rep_c <- int_attr(cell, "number-columns-repeated")
      # Populated = has a value type or any text content (matches the xlsx rule
      # of "<v> or <is> present").
      vt <- xml2::xml_attr(cell, "value-type")
      txt <- trimws(xml2::xml_text(cell))
      populated <- (!is.na(vt) && nzchar(vt)) || nzchar(txt)

      # A huge repeat count on an EMPTY cell is right-padding to the sheet limit;
      # it does not mean thousands of real columns. Only count padding runs when
      # the cell actually holds something.
      span_c <- if (populated) rep_c else min(rep_c, 1024L)

      idx <- cc + seq_len(span_c)
      seen_col <- c(seen_col, idx)

      if (populated) {
        # The same value repeated across `rep_c` columns occupies each of them.
        for (rr in seq_len(rep_r)) {
          val_row <- c(val_row, rep(r + rr, span_c))
          val_col <- c(val_col, idx)
        }
        sn <- xml2::xml_attr(cell, "style-name")
        if (!is.na(sn) && sn %in% colored)
          color_cells <- color_cells + (span_c * rep_r)
      } else {
        sn <- xml2::xml_attr(cell, "style-name")
        # A colour-filled but EMPTY cell still encodes information visually
        # (a shaded block marking a group), so it counts — but only when the
        # run is a plausible real range, not sheet-limit padding.
        if (!is.na(sn) && sn %in% colored)
          color_cells <- color_cells + (span_c * rep_r)
      }

      # Merge anchor: spans recorded on the cell that starts the range.
      sc <- suppressWarnings(as.integer(
        xml2::xml_attr(cell, "number-columns-spanned")))
      sr <- suppressWarnings(as.integer(
        xml2::xml_attr(cell, "number-rows-spanned")))
      sc <- if (is.na(sc)) 1L else sc
      sr <- if (is.na(sr)) 1L else sr
      if (sc > 1L || sr > 1L) {
        merges <- c(merges, sprintf(
          "%s%d:%s%d",
          .dv_ods_col_letter(cc + 1L), r + 1L,
          .dv_ods_col_letter(cc + sc), r + sr))
      }

      cc <- cc + span_c
    }
    r <- r + rep_r
  }

  # Empty rows: blank rows BETWEEN the first and last populated row (identical
  # rule to the xlsx path — trailing padding is not counted).
  empty_rows <- 0L
  if (length(val_row) > 0) {
    populated <- sort(unique(val_row))
    if (length(populated) > 1)
      empty_rows <- (max(populated) - min(populated) + 1L) - length(populated)
  }

  # Empty / unnamed columns: header is the first populated row; a column is
  # problematic if it has no header value, or a header but nothing below it.
  empty_cols <- 0L
  if (length(val_col) > 0) {
    hdr_row  <- min(val_row)
    hdr_cols <- unique(val_col[val_row == hdr_row])
    body_cols <- unique(val_col[val_row > hdr_row])
    # Only consider columns within the used range; padding beyond the last
    # populated column is not a missing column.
    all_cols <- unique(seen_col[seen_col <= max(val_col)])
    blank_header   <- setdiff(all_cols, hdr_cols)
    header_no_body <- setdiff(hdr_cols, body_cols)
    empty_cols <- length(unique(c(blank_header, header_no_body)))
  }

  list(name = name, color_cells = color_cells, merges = merges,
       empty_rows = empty_rows, empty_cols = empty_cols)
}
