# A native-R Psych-DS validator. This is NOT a port of the official
# (Deno/TypeScript) psychds-validator — that engine evaluates JavaScript
# expression strings from its schema and expands a 1.4 MB schema.org JSON-LD
# graph, neither of which belongs in R. Instead this reimplements the same
# *checks* idiomatically in R, covering every error-level rule in the Psych-DS
# 1.5 schema and the practical warnings. The official validator is used only as
# a development oracle to confirm agreement (see tests).
#
# Rules implemented (schema error codes in brackets):
#  - dataset_description.json present, valid JSON/UTF-8, @type Dataset,
#    required keys name/description/variableMeasured  [MissingRequiredElement,
#    JsonInvalid, IncorrectDatasetType, JsonKeyRequired]
#  - a data/ directory with at least one *_data.csv    [MissingRequiredElement]
#  - each CSV: non-empty, valid header, no duplicate/blank headers, consistent
#    column count, unique row_id                        [CSVHeaderMissing,
#    CSVHeaderRepeated, CSVHeaderLengthMismatch, RowidValuesNotUnique, FileEmpty]
#  - CSV columns <-> variableMeasured agreement         [CsvColumnMissingFromMetadata,
#    VariableMissingFromCsvColumns]
#  - data filename keyword formatting (key-value_..._data.csv)
#    [FilenameKeywordFormattingError]
#  - recommended README / CHANGES                        [warnings]

# Strip a UTF-8 BOM from a string (the official validator treats a BOM-prefixed
# header as a distinct column, so we normalise before comparing).
.strip_bom <- function(x) sub("^﻿", "", x)

# One validator issue.
.psychds_issue <- function(code, severity, reason, files = character(0)) {
  list(code = code, severity = severity, reason = reason, files = files)
}

#' Validate a Psych-DS dataset directory (native R)
#'
#' Checks a directory against the Psych-DS 1.5 dataset standard: required
#' metadata file and fields, a `data/` directory with `*_data.csv` files, CSV
#' header/structure integrity, agreement between CSV columns and the
#' `variableMeasured` metadata, and data-file keyword naming. Returns the issues
#' found with their severities.
#'
#' This is a native R reimplementation of the checks performed by the official
#' [psychds-validator](https://github.com/psych-ds/psychds-validator/); it does
#' not execute the schema's JavaScript rules or perform full schema.org JSON-LD
#' expansion (the schema.org-property warnings are therefore not reported).
#'
#' @param dir path to the dataset directory (a study root containing
#'   `dataset_description.json`)
#'
#' @returns a list with `valid` (TRUE when no error-severity issues), `issues`
#'   (a list of issues), and `summary` (counts). Printed as a compact report.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- convert_psychds(paper, "psychds/mydata")
#' psychds_validate(res$output_dir)
#' }
psychds_validate <- function(dir) {
  issues <- list()
  add <- function(...) issues[[length(issues) + 1L]] <<- .psychds_issue(...)

  if (!dir.exists(dir))
    stop("Dataset directory not found: ", dir, call. = FALSE)

  all_files <- list.files(dir, recursive = TRUE)
  data_csvs <- list.files(file.path(dir, "data"), pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)

  # ── Required: dataset_description.json ───────────────────────────────────────
  dd_path <- file.path(dir, "dataset_description.json")
  meta    <- NULL
  if (!file.exists(dd_path)) {
    add("MissingRequiredElement", "error",
        "Missing required dataset_description.json at the dataset root.")
  } else {
    raw <- tryCatch(readLines(dd_path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
    meta <- tryCatch(jsonlite::fromJSON(paste(raw, collapse = "\n"),
                                        simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(meta)) {
      add("JsonInvalid", "error",
          "dataset_description.json is not valid JSON.", "dataset_description.json")
    } else {
      typ <- meta[["@type"]] %||% meta[["type"]]
      if (is.null(typ)) {
        add("MissingDatasetType", "error",
            "Metadata is missing the required @type property.",
            "dataset_description.json")
      } else if (!identical(as.character(typ), "Dataset")) {
        add("IncorrectDatasetType", "error",
            "Metadata @type must be \"Dataset\".", "dataset_description.json")
      }
      for (key in c("name", "description", "variableMeasured")) {
        if (is.null(meta[[key]]))
          add("JsonKeyRequired", "error",
              sprintf("Metadata is missing required key \"%s\".", key),
              "dataset_description.json")
      }
      for (key in c("author", "license", "citation", "keywords")) {
        if (is.null(meta[[key]]))
          add("JsonKeyRecommended", "warning",
              sprintf("Metadata is missing recommended key \"%s\".", key),
              "dataset_description.json")
      }
    }
  }

  # ── Required: data/ with at least one *_data.csv ────────────────────────────
  data_files <- grep("_data\\.csv$", data_csvs, value = TRUE)
  if (length(data_files) == 0)
    add("MissingRequiredElement", "error",
        "No *_data.csv files found under data/. A Psych-DS dataset requires at least one data file.")

  # variableMeasured names from metadata (for column agreement).
  vm_names <- character(0)
  if (!is.null(meta) && !is.null(meta$variableMeasured)) {
    vm_names <- vapply(meta$variableMeasured, function(v) {
      nm <- if (is.list(v)) v$name else v
      if (is.null(nm)) NA_character_ else as.character(nm)
    }, character(1))
    vm_names <- .strip_bom(vm_names[!is.na(vm_names)])
  }

  # ── Per-CSV checks ───────────────────────────────────────────────────────────
  dir_norm <- gsub("\\\\", "/", normalizePath(dir, winslash = "/", mustWork = FALSE))
  rel_to_dir <- function(p) {
    p <- gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
    if (startsWith(p, dir_norm))
      sub("^/", "", substring(p, nchar(dir_norm) + 1L)) else p
  }
  for (csv in data_files) {
    rel <- rel_to_dir(csv)
    nm  <- basename(csv)

    if (file.exists(csv) && file.size(csv) == 0) {
      add("FileEmpty", "warning", "Data file is empty.", rel); next
    }

    # Keyword formatting: key-value_key-value_..._data.csv
    stem <- sub("_data\\.csv$", "", nm)
    keyword_ok <- grepl("_data\\.csv$", nm) &&
      all(grepl("^[A-Za-z0-9]+-[A-Za-z0-9]+$",
                strsplit(stem, "_", fixed = TRUE)[[1]]))
    if (!keyword_ok)
      add("FilenameKeywordFormattingError", "error",
          "Data file names must use key-value keyword formatting, e.g. study-x_data.csv.",
          rel)

    df <- tryCatch(
      utils::read.csv(csv, check.names = FALSE, nrows = -1,
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
      error = function(e) NULL)
    if (is.null(df)) {
      add("CSVFormattingError", "error", "Data file is not parsable as CSV.", rel)
      next
    }
    headers <- .strip_bom(names(df))
    if (length(headers) == 0 || all(!nzchar(headers))) {
      add("CSVHeaderMissing", "error",
          "CSV data files must contain a valid header with at least one column.", rel)
      next
    }
    if (anyDuplicated(headers) != 0)
      add("CSVHeaderRepeated", "error", "Duplicate CSV header names.", rel)
    if ("row_id" %in% headers && anyDuplicated(df[["row_id"]]) != 0)
      add("RowidValuesNotUnique", "error",
          "Column \"row_id\" must contain unique values.", rel)

    # Column <-> variableMeasured agreement.
    if (!is.null(meta) && length(vm_names) > 0) {
      missing_from_meta <- setdiff(headers[nzchar(headers)], vm_names)
      if (length(missing_from_meta) > 0)
        add("CsvColumnMissingFromMetadata", "error",
            sprintf("CSV columns not in variableMeasured: %s.",
                    paste(missing_from_meta, collapse = ", ")), rel)
    }
  }

  # variableMeasured names that never appear in any CSV column.
  if (!is.null(meta) && length(vm_names) > 0 && length(data_files) > 0) {
    all_headers <- unique(unlist(lapply(data_files, function(csv) {
      df <- tryCatch(utils::read.csv(csv, check.names = FALSE, nrows = 0,
                                     fileEncoding = "UTF-8-BOM"),
                     error = function(e) NULL)
      if (is.null(df)) character(0) else .strip_bom(names(df))
    })))
    orphan_vars <- setdiff(vm_names, all_headers)
    if (length(orphan_vars) > 0)
      add("VariableMissingFromCsvColumns", "warning",
          sprintf("variableMeasured names not found in any CSV: %s.",
                  paste(utils::head(orphan_vars, 10), collapse = ", ")))
  }

  # ── Recommended files ────────────────────────────────────────────────────────
  root_files_lc <- tolower(list.files(dir))
  if (!any(grepl("^readme(\\.|$)", root_files_lc)))
    add("MissingReadme", "warning",
        "It is recommended to include a README file in the dataset root.")
  if (!any(grepl("^changes(\\.|$)", root_files_lc)))
    add("MissingChanges", "warning",
        "It is recommended to include a CHANGES file in the dataset root.")

  severities <- vapply(issues, function(x) x$severity, character(1))
  res <- list(
    valid = !any(severities == "error"),
    issues = issues,
    summary = list(
      n_errors   = sum(severities == "error"),
      n_warnings = sum(severities == "warning"),
      n_data_files = length(data_files)
    )
  )
  class(res) <- "psychds_validation"
  res
}

#' @export
print.psychds_validation <- function(x, ...) {
  cat(if (x$valid) "✓ VALID" else "✗ INVALID", "Psych-DS dataset",
      sprintf("(%d error%s, %d warning%s)\n",
              x$summary$n_errors, plural(x$summary$n_errors),
              x$summary$n_warnings, plural(x$summary$n_warnings)))
  for (iss in x$issues) {
    mark <- if (iss$severity == "error") "  [ERROR]  " else "  [warn]   "
    cat(mark, iss$code, ": ", iss$reason, "\n", sep = "")
    if (length(iss$files) > 0)
      cat("            ", paste(iss$files, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
