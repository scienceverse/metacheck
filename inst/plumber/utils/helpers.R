# helpers.R
# Helper functions for plumber API

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


#' Parse a multipart string parameter into a logical
#'
#' Multipart form fields arrive as strings; this accepts the usual truthy/falsy
#' spellings ("true"/"false"/"1"/"0"/"yes"/"no", any case) and returns
#' `default` when the value is absent or unrecognised. (Base `as.logical()`
#' returns NA for "0"/"1"/"yes"/"no", which is why we don't lean on it here.)
#'
#' @param x the raw parameter value (character or NULL)
#' @param default logical to return when `x` is missing or unparseable
#' @return a single logical
parse_bool <- function(x, default = TRUE) {
  if (is.null(x) || length(x) == 0 || !nzchar(x[1])) return(default)
  v <- tolower(trimws(as.character(x)[1]))
  if (v %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (v %in% c("false", "f", "0", "no", "n")) return(FALSE)
  default
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


#' Run an endpoint handler against an uploaded, parsed paper
#'
#' Centralises the pipeline every paper endpoint used to repeat by hand: a
#' request id (+ start log), multipart parse, file extraction, upload
#' validation, and the bibr parse — returning the right `error_response()` at
#' each failure point.
#'
#' The upload is read straight from the tempfile `mime::parse_multipart()`
#' already wrote — no second copy (that doubled I/O on payloads up to the 50MB
#' cap) — and is `unlink()`ed when the request returns, so parsed uploads don't
#' accumulate in the session tempdir.
#'
#' `prevalidate` (optional) runs after the multipart parse but BEFORE the
#' (expensive) bibr parse, so an endpoint can reject bad parameters without
#' paying the parse cost. It receives `mp` and returns NULL to proceed, or a
#' `list(status=, message=)` to abort the request with that error.
#'
#' @param req,res plumber request/response objects
#' @param endpoint short endpoint name, for logging
#' @param handler function(paper, mp, request_id) producing the response body
#' @param prevalidate optional function(mp) -> NULL | list(status, message)
#' @return the handler's value, or an `error_response()` list
with_uploaded_paper <- function(req, res, endpoint, handler, prevalidate = NULL) {
  request_id <- uuid::UUIDgenerate()
  logger::log_info("Request started ({endpoint}): {request_id}")

  mp <- mime::parse_multipart(req)
  uploaded_file <- extract_uploaded_file(mp)
  # mime leaves its multipart tempfile(s) in the session tempdir; clean them up
  # when the request returns (the response body is already in memory by then).
  on.exit(if (!is.null(uploaded_file)) unlink(uploaded_file), add = TRUE)

  validation <- validate_file_upload(uploaded_file)
  if (!validation$valid) {
    return(error_response(res, validation$status, validation$message))
  }

  if (!is.null(prevalidate)) {
    pv <- prevalidate(mp)
    if (!is.null(pv)) return(error_response(res, pv$status, pv$message))
  }

  paper_obj <- read_paper(uploaded_file, request_id)
  if (!paper_obj$success) {
    return(error_response(res, 400, paper_obj$error))
  }

  handler(paper_obj$paper, mp, request_id)
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
