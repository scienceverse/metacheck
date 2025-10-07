# validators.R
# Validation functions for plumber API

#' Check if a file is a valid XML
#'
#' @param file_path Path to the file to check
#' @return Logical indicating if file is a valid XML
is_xml_file <- function(file_path) {
  tryCatch(
    {
      xml2::read_xml(file_path)
      TRUE
    },
    error = function(e) FALSE
  )
}

#' Validate file upload (XML only)
#'
#' @param file_path Path to the uploaded file
#' @return List with valid (logical), and optionally status and message
validate_file_upload <- function(file_path) {
  if (is.null(file_path)) {
    logger::log_warn("Request rejected: No file uploaded")
    return(list(
      valid = FALSE,
      status = 400,
      message = "No file uploaded. Please use the 'file' field."
    ))
  }

  # Check if multiple files were uploaded
  if (length(file_path) > 1) {
    logger::log_warn("Request rejected: Multiple files uploaded")
    return(list(
      valid = FALSE,
      status = 400,
      message = "Please upload only one file at a time."
    ))
  }

  if (!file.exists(file_path)) {
    logger::log_warn("Validation error: Uploaded file does not exist")
    return(list(
      valid = FALSE,
      status = 400,
      message = "Uploaded file does not exist."
    ))
  }

  if (file.size(file_path) > 3 * 1024 * 1024) { # 3MB limit
    logger::log_warn("Request rejected: File too large ({file.size(file_path)} bytes)")
    return(list(
      valid = FALSE,
      status = 413,
      message = "File too large. Maximum size is 50MB"
    ))
  }

  is_xml <- is_xml_file(file_path)

  if (!is_xml) {
    logger::log_warn("Request rejected: Invalid file type (not GROBID XML)")
    return(list(
      valid = FALSE,
      status = 400,
      message = "Uploaded file is not a valid GROBID XML file."
    ))
  }

  logger::log_info("File uploaded: size={file.size(file_path)} bytes, type=XML")
  list(valid = TRUE)
}

#' Validate GROBID parameters
#'
#' @param params List of parsed parameters
#' @return List with valid (logical), and optionally message
validate_grobid_params <- function(params) {
  # Validate grobid_url
  if (!is.null(params$grobid_url) && !grepl("^https?://", params$grobid_url)) {
    logger::log_warn("Validation error: Invalid grobid_url")
    return(list(
      valid = FALSE,
      message = "Invalid grobid_url. Must start with http:// or https://"
    ))
  }
}
