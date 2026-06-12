# validators.R
# Validation functions for plumber API (bibr JSON input)

# Sized against bibr JSON *output* (text-heavy JSON from a 30MB PDF), not the
# platform's PDF upload cap. The platform strips base64 figures before
# POSTing, so real payloads stay in single-digit MB.
MAX_UPLOAD_BYTES <- 50 * 1024 * 1024

#' Validate file upload (bibr JSON)
#'
#' Cheap structural checks only — JSON validity is established by the single
#' parse inside read_paper() (parsing a 50MB body twice in a single-threaded
#' process is a real cost), and parse errors surface through error_response().
#'
#' @param file_path Path to the uploaded file
#' @param max_bytes Maximum allowed file size in bytes
#' @return List with valid (logical), and optionally status and message
validate_file_upload <- function(file_path, max_bytes = MAX_UPLOAD_BYTES) {
  if (is.null(file_path)) {
    logger::log_warn("Request rejected: No file uploaded")
    return(list(
      valid = FALSE,
      status = 400,
      message = "No file uploaded. Please use the 'file' field."
    ))
  }

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

  if (file.size(file_path) > max_bytes) {
    logger::log_warn("Request rejected: File too large ({file.size(file_path)} bytes)")
    return(list(
      valid = FALSE,
      status = 413,
      message = sprintf("File too large. Maximum size is %dMB.", max_bytes %/% (1024 * 1024))
    ))
  }

  logger::log_info("File uploaded: size={file.size(file_path)} bytes, type=JSON")
  list(valid = TRUE)
}
