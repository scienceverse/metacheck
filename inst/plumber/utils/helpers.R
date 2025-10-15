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


#' Read a paper from GROBID XML file
#'
#' @param file_path Path to GROBID XML file
#' @param request_id Request ID for logging
#' @return List with success status and either paper object or error message
read_paper <- function(file_path, request_id) {
  logger::log_info("Reading paper: {request_id}")

  # Read paper with error handling
  result <- tryCatch(
    {
      logger::log_info("Reading GROBID XML file")
      papercheck::read_grobid(file_path)
    },
    error = function(e) {
      logger::log_error("Error reading paper: {e$message}")
      e
    }
  )

  if (inherits(result, "error")) {
    return(list(success = FALSE, error = result$message))
  }

  logger::log_info("Paper read successfully: {request_id}")
  list(success = TRUE, paper = result)
}
