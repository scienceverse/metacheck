# endpoints/grobid.R
# GROBID processing endpoint

library(xml2)
library(logger)

# Source utility functions
# When using pr_mount, the working directory is the parent of endpoints/
source("../utils/validators.R", local = TRUE)
source("../utils/helpers.R", local = TRUE)

#* Convert PDF to GROBID XML
#* @post /pdf2grobid
#* @param file:file PDF file to process
#* @param grobidUrl:[string] GROBID server URL (optional)
#* @param consolidateHeader:[int] Consolidate header (0 or 1, optional)
#* @param consolidateCitations:[int] Consolidate citations (0 or 1, optional)
#* @param consolidateFunders:[int] Consolidate funders (0 or 1, optional)
#* @param start:[int] Start page number, -1 for first page (optional)
#* @param end:[int] End page number, -1 for last page (optional)
#* @serializer contentType list(type = "application/xml")
function(req, res) {
  request_id <- uuid::UUIDgenerate()
  logger::log_info("Request started: {request_id}")
  
  # Parse multipart form data
  mp <- mime::parse_multipart(req)
  pdf_file <- mp$file$datapath
  
  # Validate file upload
  validation <- validate_pdf_upload(pdf_file)
  if (!validation$valid) {
    return(error_response(res, validation$status, validation$message))
  }
  
  # Parse and normalize parameters
  params <- parse_grobid_params(mp)
  
  # Validate parameters
  param_validation <- validate_grobid_params(params)
  if (!param_validation$valid) {
    return(error_response(res, 400, param_validation$message))
  }
  
  # Process PDF with GROBID
  result <- process_pdf_with_grobid(pdf_file, params, request_id)
  
  if (result$success) {
    logger::log_info("Request completed successfully: {request_id}")
    return(result$xml)
  }
  
  # Handle error
  logger::log_error("Request failed: {request_id}")
  return(error_response(res, 500, result$error))
}
