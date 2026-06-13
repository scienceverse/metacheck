# helpers.R
# Helper functions for plumber API

#' Normalize zero-length values to NULL
#'
#' @param x Value to normalize
#' @return NULL if x is NULL or has length 0, otherwise x
nz <- function(x) {
  if (is.null(x) || length(x) == 0) NULL else x
}

# Classes the JSON serializer (jsonlite, via plumber's `@serializer json`)
# knows how to encode. Anything else must be stripped before it reaches the
# response or serialization aborts the whole request.
.JSON_SAFE_CLASSES <- c(
  "logical", "integer", "numeric", "double", "character",
  "factor", "Date", "POSIXct", "POSIXt", "data.frame", "list"
)

#' Make a value safe for JSON serialization
#'
#' Module outputs can embed values carrying S3 classes jsonlite has no `asJSON`
#' method for — notably `ellmer_output`, attached by ellmer's structured LLM
#' extraction to columns inside a module's `table`/`summary_table`. metacheck
#' keeps these classes for R-side display (DT/`report_table`), but serializing
#' them aborts the request with "No method asJSON S3 class: ellmer_output".
#'
#' This walks the structure and drops any class outside `.JSON_SAFE_CLASSES`
#' down to its underlying type (e.g. an `ellmer_output` character vector becomes
#' a plain character vector), recursing into data frames, list columns, and
#' nested lists. Base types, factors, dates, and data frames pass through
#' unchanged.
#'
#' @param x Any value destined for a JSON response
#' @return The same value with unserializable S3 classes stripped
json_safe <- function(x) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, json_safe)
    return(x)
  }
  if (length(setdiff(class(x), .JSON_SAFE_CLASSES))) {
    x <- unclass(x)
    if (!is.null(attr(x, "class"))) x <- as.character(x)
  }
  if (is.list(x)) x <- lapply(x, json_safe)
  x
}

#' Extract uploaded file path from multipart form data
#'
#' @param mp Parsed multipart form data
#' @return Character vector of file path(s), or NULL if no file uploaded
extract_uploaded_file <- function(mp) {
  if (is.null(mp$file)) {
    return(NULL)
  }

  # Handle single file upload
  if (is.list(mp$file) && !is.null(mp$file$datapath)) {
    return(mp$file$datapath)
  }

  # Handle multiple files with same field name (should be rejected)
  if (is.list(mp$file) && length(mp$file) > 0) {
    return(sapply(mp$file, function(f) f$datapath))
  }

  NULL
}

#' Create an error response
#'
#' @param res Plumber response object
#' @param status HTTP status code
#' @param message Error message
#' @return List with error message
error_response <- function(res, status, message) {
  res$status <- status
  res$serializer <- plumber::serializer_unboxed_json()
  list(error = message)
}


#' Extract named info fields from a paper object
#'
#' Replacement for the removed package-level `info_table()`. Uses
#' `paper_table(paper, "info")` to get all info fields, then subsets to the
#' requested `fields`, tolerating any that are absent in the data.
#'
#' @param paper a paper object (scivrs_paper)
#' @param fields character vector of field names to return
#' @return a one-row tibble with paper_id plus the requested fields (NA for
#'   missing fields)
info_fields <- function(paper, fields) {
  tbl <- paper_table(paper, "info")

  # Add any requested columns that are absent in the table (e.g. "submission",
  # "received", "accepted" are not present in every bibr output)
  missing_cols <- setdiff(fields, names(tbl))
  for (col in missing_cols) {
    tbl[[col]] <- NA_character_
  }

  # Always prepend paper_id; then keep only requested fields
  keep <- intersect(c("paper_id", fields), names(tbl))
  tbl[, keep, drop = FALSE]
}


#' Read a paper from a bibr JSON file
#'
#' @param file_path Path to bibr JSON file
#' @param request_id Request ID for logging
#' @return List with success status and either paper object or error message
read_paper <- function(file_path, request_id) {
  logger::log_info("Reading paper: {request_id}")

  # .read_bibr directly, NOT read(): read() swallows per-file errors into
  # NULL, after which paperlist() throws a misleading error and print()s the
  # parsed structure into the logs. The internal reader surfaces the real
  # parse error so error_response() can return it to the client.
  result <- tryCatch(
    {
      logger::log_info("Reading bibr JSON file")
      metacheck:::.read_bibr(file_path)
    },
    error = function(e) {
      logger::log_error("Error reading paper: {conditionMessage(e)}")
      e
    }
  )

  if (inherits(result, "error")) {
    return(list(success = FALSE, error = conditionMessage(result)))
  }

  logger::log_info("Paper read successfully: {request_id}")
  list(success = TRUE, paper = result)
}


#' Render metacheck's native HTML report from already-run module outputs
#'
#' Reuses the module outputs `/check` just computed (NO module re-run, no extra
#' LLM calls): builds the report qmd with `report_qmd()` and renders it to a
#' single self-contained HTML file with Quarto. The report template sets
#' `embed-resources: true`, so the output is one standalone file.
#'
#' Best-effort by contract: any failure (Quarto missing, render error, an
#' unexpected paper/module shape) returns "" so the JSON `/check` response is
#' never affected. The caller includes whatever this returns as `report_html`.
#'
#' @param module_output named list of `metacheck_module_output` objects
#' @param paper the paper object (`scivrs_paper`)
#' @param request_id request id, for logging
#' @return the rendered HTML as a single string, or "" if rendering failed
render_report_html <- function(module_output, paper, request_id = "") {
  tryCatch(
    {
      report_text <- metacheck::report_qmd(module_output, paper)

      qmd <- tempfile(fileext = ".qmd")
      html <- sub("\\.qmd$", ".html", qmd)
      on.exit(unlink(c(qmd, html)), add = TRUE)
      writeLines(report_text, qmd)

      # quarto writes the .html next to the input .qmd (both in tempdir)
      quarto::quarto_render(input = qmd, output_format = "html", quiet = TRUE)

      if (!file.exists(html)) {
        logger::log_warn("report html not produced ({request_id})")
        return("")
      }
      paste(readLines(html, warn = FALSE), collapse = "\n")
    },
    error = function(e) {
      logger::log_warn("report render failed ({request_id}): {conditionMessage(e)}")
      ""
    }
  )
}
