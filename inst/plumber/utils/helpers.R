# helpers.R
# Helper functions for plumber API

#' Normalize zero-length values to NULL
#'
#' @param x Value to normalize
#' @return NULL if x is NULL or has length 0, otherwise x
nz <- function(x) {
  if (is.null(x) || length(x) == 0) NULL else x
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
