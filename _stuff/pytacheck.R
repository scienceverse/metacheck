#' Process a paper using the Pytacheck API
#'
#' @param file_path Path to the PDF file
#' @param api_url Base URL of the API
#' @return A Tibble containing the processed data (id, filename, grobid_xml, ocr_markdown, etc.)
process_paper <- function(file_path, api_url = "http://api.metacheck.app") {

  req <- httr2::request(api_url) |>
    httr2::req_url_path("/process/") |>
    httr2::req_body_multipart(
      file = curl::form_file(file_path),
      use_grobid = "true",
      use_ocr = "true"
    )

  resp <- httr2::req_perform(req)

  content_type <- httr2::resp_header(resp, "content-type")

  if (grepl("application/vnd.apache.arrow.stream", content_type)) {
    # It's an Arrow stream
    buffer <- httr2::resp_body_raw(resp)
    reader <- arrow::RecordBatchStreamReader$create(buffer)
    table <- reader$read_table()
    return(dplyr::as_tibble(table))
  } else {
    # It's JSON (likely an error or fallback)
    return(httr2::resp_body_json(resp))
  }
}

file_path <- metacheck::demopdf()
result <- process_paper(file_path)
tempfile <- tempfile(fileext = ".xml")
write(result$grobid_xml, tempfile)
paper <- metacheck::read(tempfile)

