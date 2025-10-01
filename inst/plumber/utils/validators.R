# validators.R
# Validation functions for plumber API

#' Validate PDF file upload
#'
#' @param pdf_file Path to the uploaded PDF file
#' @return List with valid (logical), and optionally status and message
validate_pdf_upload <- function(pdf_file) {
  if (is.null(pdf_file)) {
    logger::log_warn("Request rejected: No file uploaded")
    return(list(
      valid = FALSE,
      status = 400,
      message = "No file uploaded. Please use the 'file' field."
    ))
  }
  
  if (!file.exists(pdf_file)) {
    logger::log_warn("Validation error: Uploaded file does not exist")
    return(list(
      valid = FALSE,
      status = 400,
      message = "Uploaded file does not exist."
    ))
  }
  
  if (file.size(pdf_file) > 50 * 1024 * 1024) { # 50MB limit
    logger::log_warn("Request rejected: File too large ({file.size(pdf_file)} bytes)")
    return(list(
      valid = FALSE,
      status = 413,
      message = "File too large. Maximum size is 50MB"
    ))
  }
  
  if (!is_pdf_file(pdf_file)) {
    logger::log_warn("Request rejected: Invalid PDF file")
    return(list(
      valid = FALSE,
      status = 400,
      message = "Uploaded file is not a valid PDF."
    ))
  }
  
  logger::log_info("File uploaded: size={file.size(pdf_file)} bytes")
  list(valid = TRUE)
}

#' Check if a file is a valid PDF
#'
#' @param file_path Path to the file to check
#' @return Logical indicating if file is a valid PDF
is_pdf_file <- function(file_path) {
  tryCatch({
    con <- file(file_path, "rb")
    on.exit(close(con), add = TRUE)
    header <- readBin(con, "raw", n = 4)
    identical(header, as.raw(c(0x25, 0x50, 0x44, 0x46))) # %PDF
  }, error = function(e) FALSE)
}

#' Check if a file is a valid XML
#'
#' @param file_path Path to the file to check
#' @return Logical indicating if file is a valid XML
is_xml_file <- function(file_path) {
  tryCatch({
    xml2::read_xml(file_path)
    TRUE
  }, error = function(e) FALSE)
}

#' Validate file upload (PDF or XML)
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
  
  if (file.size(file_path) > 50 * 1024 * 1024) { # 50MB limit
    logger::log_warn("Request rejected: File too large ({file.size(file_path)} bytes)")
    return(list(
      valid = FALSE,
      status = 413,
      message = "File too large. Maximum size is 50MB"
    ))
  }
  
  is_pdf <- is_pdf_file(file_path)
  is_xml <- is_xml_file(file_path)
  
  if (!is_pdf && !is_xml) {
    logger::log_warn("Request rejected: Invalid file type (not PDF or XML)")
    return(list(
      valid = FALSE,
      status = 400,
      message = "Uploaded file is not a valid PDF or XML file."
    ))
  }
  
  logger::log_info("File uploaded: size={file.size(file_path)} bytes, type={ifelse(is_pdf, 'PDF', 'XML')}")
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
  
  # Validate consolidate_header
  if (!is.null(params$consolidate_header) && !params$consolidate_header %in% c(0, 1)) {
    return(list(
      valid = FALSE,
      message = "Invalid consolidateHeader. Must be 0 or 1"
    ))
  }
  
  # Validate consolidate_citations
  if (!is.null(params$consolidate_citations) && !params$consolidate_citations %in% c(0, 1)) {
    return(list(
      valid = FALSE,
      message = "Invalid consolidateCitations. Must be 0 or 1"
    ))
  }
  
  # Validate consolidate_funders
  if (!is.null(params$consolidate_funders) && !params$consolidate_funders %in% c(0, 1)) {
    return(list(
      valid = FALSE,
      message = "Invalid consolidateFunders. Must be 0 or 1"
    ))
  }
  
  # Validate start
  if (!is.null(params$start) && (!is.numeric(params$start) || params$start < -1)) {
    return(list(
      valid = FALSE,
      message = "Invalid start. Must be -1 or a non-negative integer"
    ))
  }
  
  # Validate end
  if (!is.null(params$end) && (!is.numeric(params$end) || params$end < -1)) {
    return(list(
      valid = FALSE,
      message = "Invalid end. Must be -1 or a non-negative integer"
    ))
  }
  
  # Validate start/end range
  if (!is.null(params$start) && !is.null(params$end) && 
      params$start > params$end && params$end != -1) {
    return(list(
      valid = FALSE,
      message = "Invalid range. Start page cannot be greater than end page"
    ))
  }
  
  list(valid = TRUE)
}
