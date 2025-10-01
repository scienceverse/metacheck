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

#' Parse GROBID parameters from multipart form data
#'
#' @param mp Parsed multipart form data
#' @return List of normalized parameters
parse_grobid_params <- function(mp) {
  params <- list(
    grobid_url = nz(mp$grobidUrl),
    consolidate_header = nz(as.integer(mp$consolidateHeader)),
    consolidate_citations = nz(as.integer(mp$consolidateCitations)),
    consolidate_funders = nz(as.integer(mp$consolidateFunders)),
    start = nz(as.integer(mp$start)),
    end = nz(as.integer(mp$end))
  )

  params
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

#' Process PDF with GROBID
#'
#' @param pdf_file Path to PDF file
#' @param params List of GROBID parameters
#' @param request_id Request ID for logging
#' @return List with success status and either xml or error message
process_pdf_with_grobid <- function(pdf_file, params, request_id) {
  # Create temp file for output
  tmp_xml <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp_xml), add = TRUE)

  # Build arguments for pdf2grobid
  args <- list(
    filename = pdf_file,
    save_path = tmp_xml
  )

  # Add optional parameters
  if (!is.null(params$grobid_url)) {
    args$grobid_url <- params$grobid_url
  }
  if (!is.null(params$consolidate_header)) {
    args$consolidate_header <- as.integer(params$consolidate_header)
  }
  if (!is.null(params$consolidate_citations)) {
    args$consolidate_citations <- as.integer(params$consolidate_citations)
  }
  if (!is.null(params$consolidate_funders)) {
    args$consolidate_funders <- as.integer(params$consolidate_funders)
  }
  if (!is.null(params$start)) {
    args$start <- as.integer(params$start)
  }
  if (!is.null(params$end)) {
    args$end <- as.integer(params$end)
  }

  logger::log_info("Processing PDF with args: {paste(names(args), collapse=', ')}")

  # Call pdf2grobid with error handling
  result <- tryCatch(
    {
      do.call(pdf2grobid, args)
      TRUE
    },
    error = function(e) {
      logger::log_error("pdf2grobid error: {e$message}")
      e$message
    }
  )

  # Check if processing was successful
  if (isTRUE(result) && file.exists(tmp_xml)) {
    xml_txt <- paste(readLines(tmp_xml, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    return(list(success = TRUE, xml = xml_txt))
  }

  error_msg <- if (is.character(result)) result else "Failed to process the PDF file."
  list(success = FALSE, error = error_msg)
}

#' Read a paper from PDF or XML file
#'
#' @param file_path Path to PDF or XML file
#' @param request_id Request ID for logging
#' @param params List of GROBID parameters (optional)
#' @return List with success status and either paper object or error message
read_paper <- function(file_path, request_id, params = list()) {
  logger::log_info("Reading paper: {request_id}")

  # Determine file type
  is_pdf <- is_pdf_file(file_path)
  tmp_xml <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp_xml), add = TRUE)
  # Read paper with appropriate function
  result <- tryCatch(
    {
      if (is_pdf) {
        logger::log_info("Processing PDF file via GROBID")
        # First convert PDF to GROBID XML


        # Build arguments for pdf2grobid
        args <- list(
          filename = file_path,
          save_path = tmp_xml
        )

        # Add optional GROBID parameters
        if (!is.null(params$grobid_url)) {
          args$grobid_url <- params$grobid_url
        }
        if (!is.null(params$consolidate_header)) {
          args$consolidate_header <- as.integer(params$consolidate_header)
        }
        if (!is.null(params$consolidate_citations)) {
          args$consolidate_citations <- as.integer(params$consolidate_citations)
        }
        if (!is.null(params$consolidate_funders)) {
          args$consolidate_funders <- as.integer(params$consolidate_funders)
        }
        if (!is.null(params$start)) {
          args$start <- as.integer(params$start)
        }
        if (!is.null(params$end)) {
          args$end <- as.integer(params$end)
        }

        do.call(pdf2grobid, args)

        if (!file.exists(tmp_xml)) {
          stop("GROBID processing failed")
        }

        # Read the GROBID XML
        papercheck::read_grobid(tmp_xml)
      } else { # if not PDF, assume XML
        logger::log_info("Reading XML file directly")
        # Try reading as GROBID XML first
        # save the XML in a temp file to be able to use read_grobid
        xml_content <- paste(readLines(file_path,
          warn = FALSE,
          encoding = "UTF-8"
        ), collapse = "\n")
        writeLines(xml_content, con = tmp_xml, useBytes = TRUE)
        papercheck::read_grobid(file_path)
      }
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
