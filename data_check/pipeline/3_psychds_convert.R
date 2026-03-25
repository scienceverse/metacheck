# 3_psychds_convert.R
# ─────────────────────────────────────────────────────────────────────────────
# Convert metacheck-datacheck pipeline outputs into PsychDS-compliant dataset
# directories under psychds/<paper_id>/.
#
# Entry point:  convert_psychds(paper_id)  →  list of per-study result rows
#
# The caller (runner) is responsible for writing result rows to
# psychds/conversion_summary.csv (Principle I — callee never writes summary).
# ─────────────────────────────────────────────────────────────────────────────

source("data_check/pipeline/helper.R")

# ── Constants ─────────────────────────────────────────────────────────────────

PSYCHDS_OUT_DIR    <- "./data_check/psychds"
DATA_SIZE_LIMIT_MB <- 500
PIPELINE_VERSION   <- "021"

# Rows to scan below row 1 for a sub-header in multi-level CSVs.
# Must stay in sync with the same constant in 0_index.R.
MULTILEVEL_HEADER_LOOKAHEAD <- 3L

# Column types that receive a statistics block in variableMeasured
NUMERIC_TYPES <- c("continuous", "continuous_comma_decimal",
                   "continuous_outliers_excluded")

# Column types that receive a valuePattern in variableMeasured
CATEGORICAL_TYPES <- c("categorical", "binary", "ordinal")

# New conversion-scoped error codes (Principle V)
ERR_PIPELINE_FAILED <- "pipeline_failed"
ERR_NO_DATA_FILES   <- "no_data_files"

# File-type → PsychDS subdirectory mapping
TYPE_TO_SUBDIR <- list(
  code        = "analysis",
  codebook    = "documentation",
  doc         = "documentation",
  supplemental = "documentation",
  other       = "documentation",
  asset       = "materials"
  # "data" and "readme" handled separately
)

# Extension override for sentinel directories (aggregate repos)
AGGREGATE_EXT_OVERRIDE <- list(
  r    = "code", R = "code",
  py   = "code", jl = "code", m = "code",
  sps  = "code", do = "code", sas = "code",
  jpg  = "asset", jpeg = "asset", png = "asset",
  gif  = "asset", bmp = "asset", tif = "asset", tiff = "asset",
  mp4  = "asset", avi = "asset", mov = "asset", wav = "asset", mp3 = "asset",
  svg  = "asset",
  pdf  = "doc",
  docx = "doc", doc = "doc", txt = "doc", rtf = "doc",
  md   = "doc",
  xlsx = "data", xls = "data",
  csv  = "data", tsv = "data", dat = "data",
  sav  = "data", dta = "data", sas7bdat = "data",
  rds  = "data", rda = "data", rdata = "data"
)

# ── Internal: read_full_data ──────────────────────────────────────────────────

# Read a complete data file (all rows, no sampling).
# Returns a list:  list(df, method, haven_labels, sheets)
#   df           – data.frame (or NULL on failure / non-data-frame object)
#   method       – character string describing the read function used
#   haven_labels – named list of value-label mappings per column (or NULL)
#   sheets       – list of list(df, sheet_name, method) for multi-sheet Excel;
#                  NULL for all other formats
# Files > DATA_SIZE_LIMIT_MB are NOT read here; the caller handles size check.
read_full_data <- function(path) {
  ext <- tolower(tools::file_ext(path))

  # ── Text formats ────────────────────────────────────────────────────────────
  if (ext %in% c("csv", "tsv", "txt", "dat")) {
    sep <- if (ext == "tsv") "\t" else sniff_delimiter(path)
    df  <- tryCatch(
      suppressWarnings(
        read.delim(path, sep = sep, header = TRUE, check.names = FALSE,
                   stringsAsFactors = FALSE, fileEncoding = "")
      ),
      error = function(e) NULL
    )
    if (!is.null(df)) {
      # Retry with latin1 if UTF-8 produces invalid bytes
      has_invalid <- any(vapply(df, function(col) {
        is.character(col) &&
          any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
      }, logical(1)))
      if (has_invalid) {
        df <- tryCatch(
          suppressWarnings(
            read.delim(path, sep = sep, header = TRUE, check.names = FALSE,
                       stringsAsFactors = FALSE, fileEncoding = "latin1")
          ),
          error = function(e) NULL
        )
      }
    }
    # ── Multi-level sub-header row drop ──────────────────────────────────────
    # When R assigns ...N placeholder names (duplicate/empty header cells), a
    # sub-header row sits inside the data.  Drop it here so the output CSV
    # does not contain it as a data row.  Column renaming is deferred to
    # convert_study(), which uses columns.csv (already resolved by 0_index.R)
    # as the authority — avoiding the NA-string fallback bug.
    if (!is.null(df) && nrow(df) > 0) {
      auto_named <- grepl("^\\.\\.\\.\\d+$", names(df))
      if (mean(auto_named) > 0.5) {
        sub_header_row    <- NULL
        current_auto_frac <- mean(auto_named)
        for (i in seq_len(min(MULTILEVEL_HEADER_LOOKAHEAD, nrow(df)))) {
          candidate      <- as.character(df[i, ])
          cand_auto_frac <- mean(grepl("^\\.\\.\\.\\d+$", candidate))
          has_real <- any(!is.na(candidate) & nzchar(candidate) &
                          candidate != "NA" &
                          !grepl("^\\.\\.\\.\\d+$", candidate) &
                          is.na(suppressWarnings(as.numeric(candidate))))
          if (cand_auto_frac < current_auto_frac && has_real) {
            sub_header_row <- i
            break
          }
        }
        if (!is.null(sub_header_row))
          df <- df[(sub_header_row + 1):nrow(df), , drop = FALSE]
      }
    }
    return(list(df = df, method = "read.delim",
                haven_labels = NULL, sheets = NULL))
  }

  # ── SPSS ────────────────────────────────────────────────────────────────────
  if (ext == "sav") {
    df_raw <- tryCatch(haven::read_sav(path), error = function(e) NULL)
    if (is.null(df_raw)) return(list(df = NULL, method = "haven::read_sav",
                                     haven_labels = NULL, sheets = NULL))
    value_labels <- lapply(df_raw, function(col) {
      lbl <- attr(col, "labels")
      if (is.null(lbl) || length(lbl) == 0) NULL else as.list(setNames(names(lbl), lbl))
    })
    value_labels <- Filter(Negate(is.null), value_labels)
    df <- as.data.frame(haven::zap_label(haven::zap_labels(df_raw)),
                        stringsAsFactors = FALSE)
    return(list(df = df, method = "haven::read_sav",
                haven_labels = if (length(value_labels) > 0) value_labels else NULL,
                sheets = NULL))
  }

  # ── Stata ────────────────────────────────────────────────────────────────────
  if (ext == "dta") {
    df_raw <- tryCatch(haven::read_dta(path), error = function(e) NULL)
    if (is.null(df_raw)) return(list(df = NULL, method = "haven::read_dta",
                                     haven_labels = NULL, sheets = NULL))
    value_labels <- lapply(df_raw, function(col) {
      lbl <- attr(col, "labels")
      if (is.null(lbl) || length(lbl) == 0) NULL else as.list(setNames(names(lbl), lbl))
    })
    value_labels <- Filter(Negate(is.null), value_labels)
    df <- as.data.frame(haven::zap_label(haven::zap_labels(df_raw)),
                        stringsAsFactors = FALSE)
    return(list(df = df, method = "haven::read_dta",
                haven_labels = if (length(value_labels) > 0) value_labels else NULL,
                sheets = NULL))
  }

  # ── SAS ─────────────────────────────────────────────────────────────────────
  if (ext == "sas7bdat") {
    df_raw <- tryCatch(haven::read_sas(path), error = function(e) NULL)
    if (is.null(df_raw)) return(list(df = NULL, method = "haven::read_sas",
                                     haven_labels = NULL, sheets = NULL))
    df <- as.data.frame(haven::zap_label(haven::zap_labels(df_raw)),
                        stringsAsFactors = FALSE)
    return(list(df = df, method = "haven::read_sas",
                haven_labels = NULL, sheets = NULL))
  }

  # ── Excel (multi-sheet) ──────────────────────────────────────────────────────
  if (ext %in% c("xlsx", "xls")) {
    sheet_names <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
    if (is.null(sheet_names)) return(list(df = NULL, method = "readxl::read_excel",
                                          haven_labels = NULL, sheets = NULL))
    sheets <- lapply(sheet_names, function(s) {
      df <- tryCatch(
        as.data.frame(readxl::read_excel(path, sheet = s),
                      stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      list(df = df, sheet_name = s, method = "readxl::read_excel")
    })
    # For single-sheet, also expose df at top level for convenience
    top_df <- if (length(sheets) == 1) sheets[[1]]$df else NULL
    return(list(df = top_df, method = "readxl::read_excel",
                haven_labels = NULL, sheets = sheets))
  }

  # ── RDS ──────────────────────────────────────────────────────────────────────
  if (ext == "rds") {
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(obj) || !is.data.frame(obj))
      return(list(df = NULL, method = "readRDS",
                  haven_labels = NULL, sheets = NULL,
                  skip_reason = "not_dataframe"))
    return(list(df = as.data.frame(obj, stringsAsFactors = FALSE),
                method = "readRDS", haven_labels = NULL, sheets = NULL))
  }

  # ── RData / Rda ───────────────────────────────────────────────────────────────
  if (ext %in% c("rda", "rdata")) {
    e <- new.env(parent = emptyenv())
    tryCatch(load(path, envir = e), error = function(err) NULL)
    dfs <- Filter(is.data.frame, as.list(e))
    if (length(dfs) == 0)
      return(list(df = NULL, method = "load",
                  haven_labels = NULL, sheets = NULL,
                  skip_reason = "not_dataframe"))
    sheets <- lapply(names(dfs), function(nm)
      list(df = as.data.frame(dfs[[nm]], stringsAsFactors = FALSE),
           sheet_name = nm, method = "load"))
    top_df <- if (length(sheets) == 1) sheets[[1]]$df else NULL
    return(list(df = top_df, method = "load",
                haven_labels = NULL, sheets = sheets))
  }

  # ── Unsupported ───────────────────────────────────────────────────────────────
  list(df = NULL, method = paste0("unsupported:", ext),
       haven_labels = NULL, sheets = NULL)
}

# ── Internal: write_json ──────────────────────────────────────────────────────

write_json <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json_str <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE,
                               null = "null", na = "null")
  writeLines(json_str, path, useBytes = FALSE)
  invisible(path)
}

# ── Internal: parse_grobid_xml ────────────────────────────────────────────────

parse_grobid_xml <- function(xml_path) {
  if (is.null(xml_path) || !file.exists(xml_path)) return(NULL)
  tryCatch({
    doc <- xml2::read_xml(xml_path)
    xml2::xml_ns_strip(doc)

    .txt <- function(xpath) {
      n <- xml2::xml_find_first(doc, xpath)
      if (is.na(n)) NA_character_ else trimws(xml2::xml_text(n))
    }
    .txts <- function(xpath) {
      trimws(xml2::xml_text(xml2::xml_find_all(doc, xpath)))
    }

    title <- .txt(".//titleStmt/title[@type='main']")
    if (is.na(title) || !nzchar(title))
      title <- .txt(".//titleStmt/title[1]")

    abstract_paras <- .txts(".//abstract//p")
    abstract <- if (length(abstract_paras) == 0) NA_character_
                else paste(abstract_paras, collapse = " ")

    authors <- vapply(
      xml2::xml_find_all(doc, ".//author/persName"),
      function(p) {
        fn <- trimws(xml2::xml_text(xml2::xml_find_first(p, ".//forename[1]")))
        sn <- trimws(xml2::xml_text(xml2::xml_find_first(p, ".//surname")))
        fn <- if (is.na(fn)) "" else fn
        sn <- if (is.na(sn)) "" else sn
        trimws(paste(fn, sn))
      },
      character(1)
    )
    authors <- authors[nzchar(authors)]

    doi      <- .txt(".//idno[@type='DOI']")
    date_raw <- xml2::xml_attr(
      xml2::xml_find_first(doc, ".//publicationStmt//date[@type='published']"),
      "when"
    )
    keywords <- .txts(".//keywords/term")

    list(
      title    = if (is.na(title))    NULL else title,
      abstract = if (is.na(abstract)) NULL else abstract,
      authors  = if (length(authors) == 0) NULL else as.list(authors),
      doi      = if (is.na(doi))      NULL else doi,
      date     = if (is.na(date_raw)) NULL else date_raw,
      keywords = if (length(keywords) == 0) NULL else as.list(keywords)
    )
  }, error = function(e) {
    warning("parse_grobid_xml: failed for ", xml_path, ": ", conditionMessage(e))
    NULL
  })
}

# ── Internal: build_property_values ──────────────────────────────────────────

# Build the variableMeasured list for one study group.
# Minimal in Phase 3 (US1): name + col_type + source_file + sample_values.
# Extended by US3 tasks (T019-T023): stats, labels, valuePattern, unmatched vars.
build_property_values <- function(cols_df, labels_df, coverage_df) {
  if (is.null(cols_df) || nrow(cols_df) == 0) return(list())

  # Join labels (left join on source_file + column_name)
  if (!is.null(labels_df) && nrow(labels_df) > 0) {
    joined <- merge(cols_df, labels_df,
                    by = c("source_file", "column_name"),
                    all.x = TRUE, suffixes = c("", ".lbl"))
  } else {
    joined <- cols_df
    joined$label         <- NA_character_
    joined$label_status  <- NA_character_
    joined$label_source  <- NA_character_
    joined$label_method  <- NA_character_
    joined$codebook_variable <- NA_character_
  }

  # Deduplicate by column_name — keep first occurrence per column_name
  # (cols_df does not carry is_raw; callers pre-filter to the relevant study)
  deduped <- joined[!duplicated(joined$column_name), ]

  pv_list <- lapply(seq_len(nrow(deduped)), function(i) {
    row <- deduped[i, ]
    pv  <- list(
      `@type`                    = "PropertyValue",
      name                       = row$column_name
    )

    # ── US3: label fields (T020) ──────────────────────────────────────────
    if (!is.na(row$label_status) && identical(row$label_status, "labelled") &&
        !is.na(row$label) && nzchar(row$label)) {
      pv[["description"]]                    <- row$label
      pv[["metacheck:label_source"]]         <- row$label_source
      pv[["metacheck:label_method"]]         <- row$label_method
      pv[["metacheck:codebook_variable"]]    <- row$codebook_variable
    }

    # ── US3: statistics block (T019) ─────────────────────────────────────
    col_type <- if ("col_type" %in% names(row)) row$col_type else NA_character_
    if (!is.na(col_type) && col_type %in% NUMERIC_TYPES) {
      if (!is.na(row$min)) pv[["minValue"]] <- row$min
      if (!is.na(row$max)) pv[["maxValue"]] <- row$max
      stat_fields <- c("n", "n_missing", "mean", "sd", "se", "median",
                       "p25", "p75", "iqr", "skewness", "kurtosis")
      stat_block <- lapply(stat_fields, function(f) {
        if (f %in% names(row) && !is.na(row[[f]])) row[[f]] else NULL
      })
      names(stat_block) <- stat_fields
      stat_block <- Filter(Negate(is.null), stat_block)
      if (length(stat_block) > 0)
        pv[["metacheck:statistics"]] <- stat_block
    }

    # ── US3: valuePattern (T021) ─────────────────────────────────────────
    if (!is.na(col_type) && col_type %in% CATEGORICAL_TYPES &&
        !is.na(row$sample_values) && nzchar(row$sample_values)) {
      vals <- unique(trimws(strsplit(row$sample_values, "\\|")[[1]]))
      vals <- vals[nzchar(vals)]
      if (length(vals) > 0)
        pv[["valuePattern"]] <- paste(vals, collapse = "|")
    }

    # ── US3: col_header_group (T023) ─────────────────────────────────────
    if ("col_header_group" %in% names(row) &&
        !is.na(row$col_header_group) && nzchar(row$col_header_group))
      pv[["metacheck:col_header_group"]] <- row$col_header_group

    # ── Always-present fields ─────────────────────────────────────────────
    if (!is.na(col_type)) pv[["metacheck:col_type"]] <- col_type
    if ("source_file" %in% names(row) && !is.na(row$source_file))
      pv[["metacheck:source_file"]] <- row$source_file
    if ("sample_values" %in% names(row) && !is.na(row$sample_values))
      pv[["metacheck:sample_values"]] <- row$sample_values

    Filter(Negate(is.null), pv)
  })

  # ── US3: append unmatched codebook variables (T022) ───────────────────────
  if (!is.null(coverage_df) && nrow(coverage_df) > 0) {
    unmatched <- coverage_df[
      !is.na(coverage_df$match_status) &
        coverage_df$match_status == "unmatched", ]
    if (nrow(unmatched) > 0) {
      extra <- lapply(seq_len(nrow(unmatched)), function(i) {
        r <- unmatched[i, ]
        pv <- list(
          `@type`                  = "PropertyValue",
          name                     = r$codebook_variable,
          `metacheck:match_status` = "unmatched_in_data"
        )
        if (!is.na(r$label) && nzchar(r$label))
          pv[["description"]] <- r$label
        if ("codebook_source" %in% names(r) && !is.na(r$codebook_source))
          pv[["metacheck:source_file"]] <- r$codebook_source
        Filter(Negate(is.null), pv)
      })
      pv_list <- c(pv_list, extra)
    }
  }

  pv_list
}

# ── Internal: build_dataset_description ──────────────────────────────────────

build_dataset_description <- function(paper_id, study_group, property_values,
                                      xml_meta, bulk_row,
                                      shared_files = NULL) {
  # Required schema:name — from XML or fallback
  study_label <- if (is.null(study_group) || study_group == "all") "Data"
                 else paste("Study", toupper(study_group))
  if (!is.null(xml_meta) && !is.null(xml_meta$title) && nzchar(xml_meta$title)) {
    schema_name <- paste0(xml_meta$title, " \u2014 ", study_label)
  } else {
    schema_name <- paste0("Repository ", paper_id, " \u2014 ", study_label)
  }

  # Required schema:description
  if (!is.null(xml_meta) && !is.null(xml_meta$abstract) &&
      nzchar(xml_meta$abstract)) {
    schema_desc <- xml_meta$abstract
  } else {
    n_data <- if (!is.null(bulk_row) && "n_data_files" %in% names(bulk_row))
                bulk_row$n_data_files else "unknown"
    n_cols <- if (!is.null(bulk_row) && "n_columns" %in% names(bulk_row))
                bulk_row$n_columns else "unknown"
    schema_desc <- paste0(
      "Data repository for paper ", paper_id, ", ", study_label, ". ",
      "Contains ", n_data, " data files with ", n_cols, " columns."
    )
  }

  # Pipeline status block
  pipeline_status <- list(
    index_success    = TRUE,
    codebook_success = !is.null(property_values) && any(vapply(property_values,
      function(pv) !is.null(pv[["description"]]), logical(1))),
    n_files_total    = if (!is.null(bulk_row) && "n_files" %in% names(bulk_row))
                         as.integer(bulk_row$n_files) else NA_integer_,
    n_data_files     = if (!is.null(bulk_row) && "n_data_files" %in% names(bulk_row))
                         as.integer(bulk_row$n_data_files) else NA_integer_,
    n_columns        = if (!is.null(bulk_row) && "n_columns" %in% names(bulk_row))
                         as.integer(bulk_row$n_columns) else NA_integer_,
    n_labelled_columns = sum(vapply(property_values,
      function(pv) !is.null(pv[["description"]]), integer(1))),
    label_status     = "ok"
  )

  desc <- list(
    `@context` = list(
      schema    = "https://schema.org/",
      metacheck = "https://metacheck.io/ns/"
    ),
    `@type`                     = "schema:Dataset",
    `schema:name`               = schema_name,
    `schema:description`        = schema_desc,
    `schema:variableMeasured`   = property_values,
    `schema:schemaVersion`      = "Psych-DS 0.1.0"
  )

  # Recommended fields from GROBID XML
  if (!is.null(xml_meta)) {
    if (!is.null(xml_meta$authors) && length(xml_meta$authors) > 0) {
      desc[["schema:author"]] <- lapply(xml_meta$authors, function(nm) {
        list(`@type` = "schema:Person", `schema:name` = nm)
      })
    }
    if (!is.null(xml_meta$doi) && nzchar(xml_meta$doi))
      desc[["schema:identifier"]] <- paste0("https://doi.org/", xml_meta$doi)
    if (!is.null(xml_meta$date) && nzchar(xml_meta$date))
      desc[["schema:datePublished"]] <- xml_meta$date
    if (!is.null(xml_meta$keywords) && length(xml_meta$keywords) > 0)
      desc[["schema:keywords"]] <- xml_meta$keywords
  }

  # Provenance fields
  desc[["metacheck:paper_id"]]          <- paper_id
  desc[["metacheck:study_group"]]       <- study_group
  desc[["metacheck:pipeline_version"]]  <- PIPELINE_VERSION
  desc[["metacheck:conversion_date"]]   <- format(Sys.Date(), "%Y-%m-%d")
  desc[["metacheck:pipeline_status"]]   <- Filter(Negate(is.null), pipeline_status)
  desc[["metacheck:source_repository"]] <- list(
    platform       = "osf",
    download_path  = paste0("data/", paper_id, "/")
  )

  # Multi-study shared resources
  if (!is.null(shared_files) && length(shared_files) > 0) {
    desc[["metacheck:shared_resources"]] <- "../shared/"
    desc[["metacheck:shared_files"]]     <- as.list(shared_files)
  }

  Filter(Negate(is.null), desc)
}

# ── Internal: build_sidecar ───────────────────────────────────────────────────

build_sidecar <- function(rel_path, format, size_bytes, is_raw, method,
                          file_cols_df, labels_df) {
  file_pvs <- build_property_values(file_cols_df, labels_df, NULL)
  list(
    `schema:variableMeasured`  = file_pvs,
    `metacheck:original_file`  = Filter(Negate(is.null), list(
      rel_path  = rel_path,
      format    = format,
      size_bytes = as.integer(size_bytes),
      is_raw    = as.logical(is_raw)
    )),
    `metacheck:conversion` = Filter(Negate(is.null), list(
      method                 = method,
      encoding_normalized    = TRUE,
      rows_written           = NULL,   # filled in by caller
      columns_written        = NULL,   # filled in by caller
      haven_labels_extracted = grepl("haven", method)
    ))
  )
}

# ── Internal: build_provenance ────────────────────────────────────────────────

build_provenance <- function(file_records) {
  list(file_provenance = lapply(file_records, function(r) {
    rec <- list(
      psychds_path          = r$psychds_path,
      original_rel_path     = r$original_rel_path,
      original_format       = r$original_format,
      pipeline_type         = r$pipeline_type,
      pipeline_group        = r$pipeline_group,
      pipeline_is_raw       = as.logical(r$pipeline_is_raw),
      ground_truth_validated = as.logical(r$ground_truth_validated)
    )
    if (isTRUE(r$ground_truth_validated)) {
      rec[["ground_truth"]] <- Filter(Negate(is.null), list(
        type_gt      = r$gt_type_gt,
        group_gt     = r$gt_group_gt,
        is_raw_gt    = r$gt_is_raw_gt,
        validated_at = r$gt_validated_at,
        annotator    = r$gt_annotator
      ))
    }
    # TXT extraction provenance (doc/codebook files only)
    if (isTRUE(r$txt_extraction_attempted)) {
      rec[["txt_extraction_attempted"]] <- TRUE
      rec[["txt_extraction_skipped"]]   <- isTRUE(r$txt_extraction_skipped)
      if (!is.null(r$txt_skip_reason))
        rec[["txt_skip_reason"]]   <- r$txt_skip_reason
      if (!is.null(r$txt_psychds_path))
        rec[["txt_psychds_path"]]  <- r$txt_psychds_path
    }
    Filter(Negate(is.null), rec)
  }))
}

# ── Internal: write_doc_txt ───────────────────────────────────────────────────

# Attempt to extract plain text from a doc/codebook file and write a .txt copy
# to documentation/txt/ under the study root.
#
# Returns a named list with:
#   attempted       TRUE always
#   skipped         TRUE if no .txt was written (empty result or error)
#   skip_reason     "no_extractable_text" | "extraction_error" | NULL
#   txt_psychds_path relative path of written .txt, or NULL if skipped
#
# Calls extract_plain_text() from helper.R. Only supported extensions
# (.pdf / .docx / .rtf) proceed — all others receive attempted = FALSE.
write_doc_txt <- function(src_path, study_root) {
  ext <- tolower(tools::file_ext(src_path))
  if (!ext %in% c("pdf", "docx", "rtf"))
    return(list(attempted = FALSE, skipped = TRUE,
                skip_reason = NULL, txt_psychds_path = NULL))

  text <- extract_plain_text(src_path)

  if (is.null(text)) {
    return(list(attempted = TRUE, skipped = TRUE,
                skip_reason = "extraction_error", txt_psychds_path = NULL))
  }
  if (!nzchar(trimws(text))) {
    return(list(attempted = TRUE, skipped = TRUE,
                skip_reason = "no_extractable_text", txt_psychds_path = NULL))
  }

  txt_dir  <- file.path(study_root, "documentation", "txt")
  base_nm  <- tools::file_path_sans_ext(basename(src_path))
  txt_file <- file.path(txt_dir, paste0(base_nm, ".txt"))
  dir.create(txt_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(text, txt_file, useBytes = FALSE)

  rel_path <- file.path("documentation", "txt", paste0(base_nm, ".txt"))
  list(attempted = TRUE, skipped = FALSE,
       skip_reason = NULL, txt_psychds_path = rel_path)
}

# ── Internal: place_non_data_file ─────────────────────────────────────────────

# Copy a non-data file to its PsychDS destination directory.
# Returns the relative PsychDS path (relative to study root) or NULL on skip.
place_non_data_file <- function(src_path, file_type, filename, study_root) {
  if (file_type == "readme") {
    ext     <- tools::file_ext(filename)
    dest_nm <- if (nzchar(ext)) paste0("README.", ext) else "README"
    dest    <- file.path(study_root, dest_nm)
    psychds_path <- dest_nm
  } else {
    subdir  <- TYPE_TO_SUBDIR[[file_type]]
    if (is.null(subdir)) subdir <- "documentation"
    dest    <- file.path(study_root, subdir, filename)
    psychds_path <- file.path(subdir, filename)
  }
  if (!file.exists(src_path)) return(NULL)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(src_path, dest, overwrite = TRUE)
  psychds_path
}

# ── Internal: write_data_csv ──────────────────────────────────────────────────

# Write a data.frame to a UTF-8 CSV with no BOM, proper quoting.
# Applies row_id uniqueness check per FR-015b.
# Returns list(path, rows_written, columns_written, row_id_renamed).
write_data_csv <- function(df, dest_path) {
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  row_id_renamed <- FALSE

  # FR-015b: row_id uniqueness check
  if ("row_id" %in% names(df)) {
    if (anyDuplicated(df[["row_id"]]) != 0) {
      names(df)[names(df) == "row_id"] <- "original_row_id"
      row_id_renamed <- TRUE
    }
  }

  write.csv(df, dest_path, row.names = FALSE, fileEncoding = "UTF-8",
            quote = TRUE)
  list(
    path            = dest_path,
    rows_written    = nrow(df),
    columns_written = ncol(df),
    row_id_renamed  = row_id_renamed
  )
}

# ── Internal: convert_study ───────────────────────────────────────────────────

# Convert one study group end-to-end.
# Returns a named list (the per-study result row for conversion_summary.csv).
convert_study <- function(paper_id, study_group, files_df, cols_df, labels_df,
                          coverage_df, xml_meta, out_dir,
                          shared_files = NULL) {

  dir.create(file.path(out_dir, "data", "raw"), recursive = TRUE,
             showWarnings = FALSE)

  file_records   <- list()
  n_data_files   <- 0L
  n_raw_files    <- 0L
  n_labelled     <- 0L

  # ── Process data files ─────────────────────────────────────────────────────
  data_files <- files_df[!is.na(files_df$type) & files_df$type == "data", ]

  for (i in seq_len(nrow(data_files))) {
    row      <- data_files[i, ]
    src_path <- row$path
    rel_path <- row$rel_path
    filename <- row$filename
    ext      <- tolower(tools::file_ext(filename))
    is_raw   <- isTRUE(row$is_raw)
    gt_val   <- isTRUE(row$ground_truth_validated)

    # Size check (FR-013, US4/T028)
    size_bytes <- tryCatch(file.info(src_path)$size, error = function(e) NA_real_)
    size_mb    <- if (is.na(size_bytes)) 0 else size_bytes / 1e6

    # Always copy original to data/raw/
    raw_dest <- file.path(out_dir, "data", "raw", filename)
    if (file.exists(src_path)) file.copy(src_path, raw_dest, overwrite = TRUE)
    n_raw_files <- n_raw_files + 1L

    prov_base <- list(
      original_rel_path      = rel_path,
      original_format        = ext,
      pipeline_type          = row$type,
      pipeline_group         = row$group,
      pipeline_is_raw        = row$is_raw,
      ground_truth_validated = gt_val,
      gt_type_gt             = if (gt_val && "gt_type_gt" %in% names(row)) row$gt_type_gt else NULL,
      gt_group_gt            = if (gt_val && "gt_group_gt" %in% names(row)) row$gt_group_gt else NULL,
      gt_is_raw_gt           = if (gt_val && "gt_is_raw_gt" %in% names(row)) row$gt_is_raw_gt else NULL,
      gt_validated_at        = if (gt_val && "gt_validated_at" %in% names(row)) row$gt_validated_at else NULL,
      gt_annotator           = if (gt_val && "gt_annotator" %in% names(row)) row$gt_annotator else NULL
    )

    # Add raw/ provenance entry
    file_records <- c(file_records, list(c(
      list(psychds_path = file.path("data", "raw", filename)),
      prov_base
    )))

    if (!is.na(size_mb) && size_mb > DATA_SIZE_LIMIT_MB) {
      # Oversized: skip conversion, write skip-sidecar in raw/
      skip_sidecar <- list(
        `metacheck:conversion_skipped` = TRUE,
        `metacheck:skip_reason`        = "file_size_exceeds_limit",
        `metacheck:file_size_mb`       = round(size_mb, 1)
      )
      sidecar_nm   <- sub("\\.csv$", ".json",
                          paste0(tools::file_path_sans_ext(filename), ".json"))
      write_json(skip_sidecar,
                 file.path(out_dir, "data", "raw", sidecar_nm))
      next
    }

    # Read full data
    read_result <- read_full_data(src_path)
    if (is.null(read_result)) next

    # Determine all sheets to write
    sheets <- read_result$sheets
    if (is.null(sheets)) {
      if (!is.null(read_result$df))
        sheets <- list(list(df = read_result$df, sheet_name = NULL,
                            method = read_result$method))
    }
    if (is.null(sheets) || length(sheets) == 0) next

    for (sh in sheets) {
      if (is.null(sh$df) || !is.data.frame(sh$df)) next

      # Build output filename (FR-010, FR-015, FR-015a)
      base_kw  <- sanitise_keyword_value(filename)
      if (!nzchar(base_kw)) base_kw <- paste0("file", i)

      version_prefix <- if (is_raw) "version-raw_" else ""
      sheet_suffix   <- if (!is.null(sh$sheet_name) && nzchar(sh$sheet_name)) {
        paste0("_sheet-", sanitise_keyword_value(sh$sheet_name))
      } else ""

      csv_name   <- paste0(version_prefix, "source-", base_kw,
                           sheet_suffix, "_data.csv")
      csv_dest   <- file.path(out_dir, "data", csv_name)
      sidecar_nm <- sub("\\.csv$", ".json", csv_name)
      sidecar_dest <- file.path(out_dir, "data", sidecar_nm)

      # Per-file columns subset (needed for rename + sidecar)
      file_cols <- if (!is.null(cols_df) && nrow(cols_df) > 0)
        cols_df[cols_df$source_file == rel_path, ] else NULL
      file_lbls <- if (!is.null(labels_df) && nrow(labels_df) > 0)
        labels_df[labels_df$source_file == rel_path, ] else NULL

      # Apply pipeline-resolved column names from columns.csv.
      # columns.csv is the authority: 0_index.R already resolved multi-level
      # CSV headers (sub-header row detection, ...N fallback) and stored the
      # result in column_name.  A positional rename here avoids re-implementing
      # that logic and the NA-string edge case it carries.
      if (!is.null(file_cols) && nrow(file_cols) == ncol(sh$df))
        names(sh$df) <- file_cols$column_name

      write_res <- write_data_csv(sh$df, csv_dest)
      n_data_files <- n_data_files + 1L

      sidecar <- build_sidecar(rel_path, ext, size_bytes, is_raw,
                               sh$method, file_cols, file_lbls)
      sidecar[["metacheck:conversion"]][["rows_written"]]    <- write_res$rows_written
      sidecar[["metacheck:conversion"]][["columns_written"]] <- write_res$columns_written
      if (isTRUE(write_res$row_id_renamed))
        sidecar[["metacheck:conversion"]][["row_id_renamed"]] <- TRUE
      if (!is.null(read_result$haven_labels) && length(read_result$haven_labels) > 0)
        sidecar[["metacheck:value_labels"]] <- read_result$haven_labels
      write_json(sidecar, sidecar_dest)

      file_records <- c(file_records, list(c(
        list(psychds_path = file.path("data", csv_name)),
        prov_base
      )))
    }
  }

  # ── Process non-data files ─────────────────────────────────────────────────
  non_data <- files_df[is.na(files_df$type) | files_df$type != "data", ]
  for (i in seq_len(nrow(non_data))) {
    row      <- non_data[i, ]
    file_type <- if (is.na(row$type)) "other" else row$type
    psychds_path <- place_non_data_file(
      src_path   = row$path,
      file_type  = file_type,
      filename   = row$filename,
      study_root = out_dir
    )
    if (is.null(psychds_path)) next

    # Attempt plaintext extraction for doc/codebook files (US6)
    txt_info <- NULL
    if (file_type %in% c("doc", "codebook") &&
        tolower(tools::file_ext(row$filename)) %in% c("pdf", "docx", "rtf")) {
      txt_info <- write_doc_txt(row$path, out_dir)
    }

    file_records <- c(file_records, list(Filter(Negate(is.null), list(
      psychds_path              = psychds_path,
      original_rel_path         = row$rel_path,
      original_format           = tolower(tools::file_ext(row$filename)),
      pipeline_type             = row$type,
      pipeline_group            = row$group,
      pipeline_is_raw           = row$is_raw,
      ground_truth_validated    = isTRUE(row$ground_truth_validated),
      # TXT extraction fields (NULL when no extraction attempted)
      txt_extraction_attempted  = if (!is.null(txt_info)) txt_info$attempted else NULL,
      txt_extraction_skipped    = if (!is.null(txt_info)) txt_info$skipped   else NULL,
      txt_skip_reason           = if (!is.null(txt_info)) txt_info$skip_reason else NULL,
      txt_psychds_path          = if (!is.null(txt_info)) txt_info$txt_psychds_path else NULL
    ))))
  }

  # ── Build variableMeasured and dataset_description.json ───────────────────
  study_cols <- if (!is.null(cols_df)) cols_df else NULL
  study_lbls <- if (!is.null(labels_df)) labels_df else NULL
  study_cov  <- if (!is.null(coverage_df)) coverage_df else NULL

  property_values <- build_property_values(study_cols, study_lbls, study_cov)
  n_labelled <- sum(vapply(property_values,
                           function(pv) !is.null(pv[["description"]]), integer(1)))

  bulk_row <- NULL  # passed in if available; callers may augment
  dataset_desc <- build_dataset_description(
    paper_id        = paper_id,
    study_group     = study_group,
    property_values = property_values,
    xml_meta        = xml_meta,
    bulk_row        = bulk_row,
    shared_files    = shared_files
  )

  write_json(dataset_desc, file.path(out_dir, "dataset_description.json"))
  write_json(build_provenance(file_records),
             file.path(out_dir, "provenance.json"))

  list(
    paper_id          = paper_id,
    study_group       = study_group,
    success           = TRUE,
    error             = NA_character_,
    n_data_files      = n_data_files,
    n_raw_files       = n_raw_files,
    n_variables       = length(property_values),
    n_labelled        = n_labelled,
    has_paper_metadata = !is.null(xml_meta),
    has_ground_truth  = any(data_files$ground_truth_validated, na.rm = TRUE),
    output_path       = out_dir
  )
}

# ── Internal: expand_sentinel_rows ───────────────────────────────────────────

# For sentinel rows (is_sentinel == TRUE), replace with individual files on disk.
expand_sentinel_rows <- function(structure_df) {
  non_sentinel <- structure_df[!isTRUE(structure_df$is_sentinel) &
                                 !is.na(structure_df$is_sentinel) &
                                 structure_df$is_sentinel == FALSE, ]
  sentinel     <- structure_df[!is.na(structure_df$is_sentinel) &
                                 structure_df$is_sentinel == TRUE, ]
  if (nrow(sentinel) == 0) return(structure_df)

  expanded <- lapply(seq_len(nrow(sentinel)), function(i) {
    s    <- sentinel[i, ]
    dir  <- dirname(s$path)
    if (!dir.exists(dir)) return(NULL)
    files <- list.files(dir, full.names = TRUE, recursive = FALSE)
    if (length(files) == 0) return(NULL)
    rows <- lapply(files, function(f) {
      ext      <- tolower(tools::file_ext(f))
      override <- AGGREGATE_EXT_OVERRIDE[[ext]]
      row      <- s
      row$path     <- f
      row$filename <- basename(f)
      row$ext      <- ext
      row$rel_path <- sub(paste0(".*", s$paper_id, "/"), "", f)
      row$is_sentinel <- FALSE
      if (!is.null(override)) row$type <- override
      row
    })
    do.call(rbind, rows)
  })
  expanded <- Filter(Negate(is.null), expanded)
  if (length(expanded) > 0)
    rbind(non_sentinel, do.call(rbind, expanded))
  else
    non_sentinel
}

# ── Internal: co-location heuristic ──────────────────────────────────────────

# For files with group %in% c("na","other") in a multi-study paper,
# try to assign them to a specific study via directory co-location.
# Returns files_df with updated group values.
resolve_shared_files <- function(files_df, studies) {
  unscoped_mask <- !is.na(files_df$group) &
    files_df$group %in% c("na", "other")
  if (!any(unscoped_mask)) return(files_df)

  study_vals <- studies  # e.g. c("ex1","ex2")

  for (i in which(unscoped_mask)) {
    parent <- dirname(files_df$rel_path[i])
    # Find siblings: other files in the same parent directory
    same_parent <- dirname(files_df$rel_path) == parent
    same_parent[i] <- FALSE
    sibling_groups <- files_df$group[same_parent]
    sibling_groups <- sibling_groups[
      !is.na(sibling_groups) &
        sibling_groups %in% study_vals
    ]
    if (length(unique(sibling_groups)) == 1) {
      files_df$group[i] <- unique(sibling_groups)
    }
    # else: leave as "other"/"na" → will go to shared/
  }
  files_df
}

# ── append_conversion_summary ─────────────────────────────────────────────────

# Append one or more result rows to psychds/conversion_summary.csv.
# Creates file with header if absent. Caller is responsible for calling this.
append_conversion_summary <- function(rows, summary_path = NULL) {
  if (is.null(summary_path))
    summary_path <- file.path(PSYCHDS_OUT_DIR, "conversion_summary.csv")
  dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)

  df <- if (is.data.frame(rows)) rows else do.call(rbind, lapply(rows, as.data.frame))

  # Ensure paper_id is character
  if ("paper_id" %in% names(df))
    df$paper_id <- as.character(df$paper_id)

  write.table(df, summary_path,
              append    = file.exists(summary_path),
              sep       = ",",
              row.names = FALSE,
              col.names = !file.exists(summary_path),
              quote     = TRUE,
              fileEncoding = "UTF-8")
  invisible(summary_path)
}

# ── convert_psychds (main entry point) ───────────────────────────────────────

# Convert all studies for a single paper.
# Returns a list of per-study result rows (caller writes to summary CSV).
convert_psychds <- function(paper_id) {
  paper_id <- as.character(paper_id)

  # Principle I helper: build a failure row
  .fail_row <- function(study_group, code) {
    list(list(
      paper_id = paper_id, study_group = study_group,
      success = FALSE, error = code,
      n_data_files = 0L, n_raw_files = 0L,
      n_variables = 0L, n_labelled = 0L,
      has_paper_metadata = FALSE, has_ground_truth = FALSE,
      output_path = NA_character_
    ))
  }

  # 1. Check bulk_summary.csv
  bulk_path <- "./data_check/results/bulk_summary.csv"
  if (!file.exists(bulk_path))
    bulk_path <- "./data_check/bulk_summary.csv"
  if (file.exists(bulk_path)) {
    bulk <- tryCatch(
      read.csv(bulk_path, stringsAsFactors = FALSE,
               colClasses = c(paper_id = "character")),
      error = function(e) NULL
    )
    if (!is.null(bulk)) {
      paper_row <- bulk[bulk$paper_id == paper_id, ]
      if (nrow(paper_row) > 0 && !isTRUE(as.logical(paper_row$success[1])))
        return(.fail_row("all", ERR_PIPELINE_FAILED))
    }
  }

  # 2. Read pipeline output CSVs
  out_base <- file.path("./data_check/outputs", paper_id)
  structure_path  <- file.path(out_base, "structure.csv")
  columns_path    <- file.path(out_base, "columns.csv")
  labels_path     <- file.path(out_base, "labels.csv")
  coverage_path   <- file.path(out_base, "codebook_coverage.csv")

  if (!file.exists(structure_path))
    return(.fail_row("all", "missing_structure_csv"))

  structure_df <- read.csv(structure_path, stringsAsFactors = FALSE,
                            colClasses = c(paper_id = "character"))
  cols_df <- if (file.exists(columns_path))
    read.csv(columns_path, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")) else NULL
  labels_df <- if (file.exists(labels_path))
    read.csv(labels_path, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")) else NULL
  coverage_df <- if (file.exists(coverage_path))
    read.csv(coverage_path, stringsAsFactors = FALSE,
             colClasses = c(paper_id = "character")) else NULL

  # 3. Apply ground-truth overrides
  structure_df <- apply_ground_truth(structure_df, paper_id)

  # 4. Expand sentinel rows
  structure_df <- expand_sentinel_rows(structure_df)

  # 5. Detect studies
  data_mask <- !is.na(structure_df$type) & structure_df$type == "data"
  studies   <- unique(structure_df$group[data_mask])
  studies   <- studies[!is.na(studies)]
  if (length(studies) == 0) return(.fail_row("all", ERR_NO_DATA_FILES))

  # 6. Parse GROBID XML
  xml_path <- file.path("/Volumes/Models/expanded_xml",
                        paste0(paper_id, ".xml"))
  xml_meta <- parse_grobid_xml(xml_path)

  # 7. Determine layout
  single_study <- (length(studies) == 1)
  results      <- list()

  if (single_study) {
    # ── Single-study: flat layout ──────────────────────────────────────────
    out_dir <- file.path(PSYCHDS_OUT_DIR, paper_id)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    result <- tryCatch(
      convert_study(paper_id, studies[1], structure_df,
                    cols_df, labels_df, coverage_df,
                    xml_meta, out_dir, shared_files = NULL),
      error = function(e) list(
        paper_id = paper_id, study_group = studies[1],
        success = FALSE, error = conditionMessage(e),
        n_data_files = 0L, n_raw_files = 0L,
        n_variables = 0L, n_labelled = 0L,
        has_paper_metadata = !is.null(xml_meta),
        has_ground_truth = any(structure_df$ground_truth_validated, na.rm = TRUE),
        output_path = out_dir
      )
    )
    results <- list(result)

  } else {
    # ── Multi-study: study-<group>/ layout + shared/ ───────────────────────
    paper_root <- file.path(PSYCHDS_OUT_DIR, paper_id)

    # Resolve unscoped files via co-location heuristic
    structure_df <- resolve_shared_files(structure_df, studies)

    # Collect shared files (still group %in% c("na","other") after heuristic)
    shared_mask  <- !is.na(structure_df$group) &
      structure_df$group %in% c("na", "other")
    shared_files_df <- structure_df[shared_mask, ]
    shared_rel_paths <- character(0)

    # Place shared files into shared/
    if (nrow(shared_files_df) > 0) {
      shared_root <- file.path(paper_root, "shared")
      for (i in seq_len(nrow(shared_files_df))) {
        r       <- shared_files_df[i, ]
        subdir  <- TYPE_TO_SUBDIR[[if (is.na(r$type)) "other" else r$type]]
        if (is.null(subdir)) subdir <- "documentation"
        dest    <- file.path(shared_root, subdir, r$filename)
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        if (file.exists(r$path)) file.copy(r$path, dest, overwrite = TRUE)
        shared_rel_paths <- c(shared_rel_paths,
                              file.path("../shared", subdir, r$filename))
      }
    }

    # Convert each study
    for (sg in studies) {
      study_mask  <- !is.na(structure_df$group) & structure_df$group == sg
      study_files <- structure_df[study_mask, ]
      out_dir     <- file.path(paper_root, paste0("study-", sg))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      study_cols <- if (!is.null(cols_df))
        cols_df[!is.na(cols_df$group) & cols_df$group == sg, ] else NULL
      study_lbls <- if (!is.null(labels_df))
        labels_df[!is.na(labels_df$group) & labels_df$group == sg, ] else NULL
      study_cov  <- coverage_df  # full coverage_df shared across studies

      result <- tryCatch(
        convert_study(paper_id, sg, study_files,
                      study_cols, study_lbls, study_cov,
                      xml_meta, out_dir,
                      shared_files = if (length(shared_rel_paths) > 0)
                        shared_rel_paths else NULL),
        error = function(e) list(
          paper_id = paper_id, study_group = sg,
          success = FALSE, error = conditionMessage(e),
          n_data_files = 0L, n_raw_files = 0L,
          n_variables = 0L, n_labelled = 0L,
          has_paper_metadata = !is.null(xml_meta),
          has_ground_truth = any(study_files$ground_truth_validated, na.rm = TRUE),
          output_path = out_dir
        )
      )
      results <- c(results, list(result))
    }
  }

  results
}
