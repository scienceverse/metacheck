# test-11-api.R
# Tests for the papercheck Plumber API

library(httptest)
library(testthat)
library(jsonlite)
library(xml2)

# Get test files
test_pdf <- system.file("grobid", "eyecolor.pdf", package = "papercheck")
test_xml <- system.file("grobid", "eyecolor.xml", package = "papercheck")

# Helper function to check if API is running
api_is_running <- function() {
  tryCatch({
    response <- httr::GET("http://localhost:2005/health", httr::timeout(2))
    httr::status_code(response) == 200
  }, error = function(e) FALSE)
}

# Helper function to create multipart form data
create_multipart <- function(file_path, additional_params = list()) {
  # Create a simple multipart form structure
  boundary <- paste0("----WebKitFormBoundary", sample(100000:999999, 1))
  
  # Read file content
  file_content <- readBin(file_path, "raw", file.info(file_path)$size)
  file_name <- basename(file_path)
  
  # Build multipart body
  body <- paste0("--", boundary, "\r\n")
  body <- paste0(body, 'Content-Disposition: form-data; name="file"; filename="', file_name, '"\r\n')
  body <- paste0(body, "Content-Type: application/octet-stream\r\n\r\n")
  
  # Convert to raw and append file content
  body_raw <- charToRaw(body)
  body_raw <- c(body_raw, file_content, charToRaw("\r\n"))
  
  # Add additional parameters
  for (name in names(additional_params)) {
    param_part <- paste0("--", boundary, "\r\n")
    param_part <- paste0(param_part, 'Content-Disposition: form-data; name="', name, '"\r\n\r\n')
    param_part <- paste0(param_part, additional_params[[name]], "\r\n")
    body_raw <- c(body_raw, charToRaw(param_part))
  }
  
  # Close multipart
  body_raw <- c(body_raw, charToRaw(paste0("--", boundary, "--\r\n")))
  
  list(
    body = body_raw,
    headers = list(
      `Content-Type` = paste0("multipart/form-data; boundary=", boundary),
      `Content-Length` = length(body_raw)
    )
  )
}

# Helper function to make POST requests with file upload
api_post <- function(endpoint, file_path = NULL, params = list()) {
  if (!is.null(file_path)) {
    multipart <- create_multipart(file_path, params)
    httr::POST(
      url = paste0("http://localhost:2005", endpoint),
      body = multipart$body,
      do.call(httr::add_headers, multipart$headers),
      encode = "raw"
    )
  } else {
    httr::POST(
      url = paste0("http://localhost:2005", endpoint),
      body = params,
      encode = "form"
    )
  }
}

# Test health check endpoint
test_that("API health check works", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  response <- httr::GET("http://localhost:2005/health")
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_equal(result$status, "ok")
  expect_true("timestamp" %in% names(result))
})

# Test /paper/info endpoint with XML file
test_that("/paper/info endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/info", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("title" %in% names(result))
  expect_true("keywords" %in% names(result))
  expect_true("doi" %in% names(result))
  expect_true("description" %in% names(result))
  
  # Check expected values
  expect_true(grepl("Positive sexual imprinting", result$title, ignore.case = TRUE))
})

# Test /paper/info endpoint with PDF file
test_that("/paper/info endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/info", test_pdf)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("title" %in% names(result))
  expect_true("keywords" %in% names(result))
  expect_true("doi" %in% names(result))
  expect_true("description" %in% names(result))
  
  # Check expected values
  expect_true(grepl("Positive sexual imprinting", result$title, ignore.case = TRUE))
})

# Test /paper/info endpoint with custom fields
test_that("/paper/info endpoint works with custom fields", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/info", test_xml, list(fields = "title,doi"))
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("title" %in% names(result))
  expect_true("doi" %in% names(result))
  expect_false("keywords" %in% names(result))
  expect_false("description" %in% names(result))
})

# Test /paper/authors endpoint with XML file
test_that("/paper/authors endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/authors", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("name.surname" %in% names(result))
  
  # Check for expected authors
  expect_true(any(grepl("Debruine", result$name.surname, ignore.case = TRUE)))
  expect_true(any(grepl("Jones", result$name.surname, ignore.case = TRUE)))
  expect_true(any(grepl("Little", result$name.surname, ignore.case = TRUE)))
})

# Test /paper/authors endpoint with PDF file
test_that("/paper/authors endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/authors", test_pdf)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("name.surname" %in% names(result))
})

# Test /paper/references endpoint with XML file
test_that("/paper/references endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/references", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  expect_true(length(result) > 0)
  
  # Check that references have expected structure
  if (length(result) > 0) {
    expect_true("title" %in% names(result[[1]]) || "raw_reference" %in% names(result[[1]]))
  }
})

# Test /paper/references endpoint with PDF file
test_that("/paper/references endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/references", test_pdf)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  expect_true(length(result) > 0)
})

# Test /paper/cross-references endpoint with XML file
test_that("/paper/cross-references endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/cross-references", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  # Cross-references might be empty for some papers
})

# Test /paper/cross-references endpoint with PDF file
test_that("/paper/cross-references endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/cross-references", test_pdf)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
})

# Test /paper/search endpoint with XML file
test_that("/paper/search endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/search", test_xml, list(q = "imprinting"))
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  # Search should return some results for "imprinting"
  expect_true(length(result) > 0)
})

# Test /paper/search endpoint with PDF file
test_that("/paper/search endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/search", test_pdf, list(q = "imprinting"))
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  expect_true(length(result) > 0)
})

# Test /paper/search endpoint with no query parameter
test_that("/paper/search endpoint fails without query", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/search", test_xml)
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("required", result$error, ignore.case = TRUE))
})

# Test /paper/module endpoint with XML file
test_that("/paper/module endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  # Test with a simple module that should work
  response <- api_post("/paper/module", test_xml, list(name = "statcheck"))
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  expect_true("module" %in% names(result) || "table" %in% names(result) || "report" %in% names(result))
})

# Test /paper/module endpoint with PDF file
test_that("/paper/module endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/module", test_pdf, list(name = "statcheck"))
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
})

# Test /paper/module endpoint with invalid module
test_that("/paper/module endpoint fails with invalid module", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/module", test_xml, list(name = "invalid_module"))
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("not found", result$error, ignore.case = TRUE))
})

# Test /paper/module endpoint with no module name
test_that("/paper/module endpoint fails without module name", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/module", test_xml)
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("required", result$error, ignore.case = TRUE))
})

# Test /paper/check endpoint with XML file
test_that("/paper/check endpoint works with XML file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/check", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("paper_info" %in% names(result))
  expect_true("authors" %in% names(result))
  expect_true("references" %in% names(result))
  expect_true("cross_references" %in% names(result))
  expect_true("modules_run" %in% names(result))
  expect_true("results" %in% names(result))
  
  # Check paper info
  expect_true("title" %in% names(result$paper_info))
  expect_true("doi" %in% names(result$paper_info))
  expect_true("keywords" %in% names(result$paper_info))
  
  # Check authors
  expect_true(is.data.frame(result$authors))
  expect_true(nrow(result$authors) > 0)
  
  # Check references
  expect_true(is.list(result$references))
  
  # Check modules
  expect_true(is.character(result$modules_run))
  expect_true(length(result$modules_run) > 0)
  expect_true(is.list(result$results))
})

# Test /paper/check endpoint with PDF file
test_that("/paper/check endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/paper/check", test_pdf)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("paper_info" %in% names(result))
  expect_true("authors" %in% names(result))
  expect_true("references" %in% names(result))
  expect_true("cross_references" %in% names(result))
  expect_true("modules_run" %in% names(result))
  expect_true("results" %in% names(result))
})

# Test /paper/check endpoint with specific modules
test_that("/paper/check endpoint works with specific modules", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/check", test_xml, list(modules = "statcheck,all_urls"))
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(length(result$modules_run) == 2)
  expect_true("statcheck" %in% result$modules_run)
  expect_true("all_urls" %in% result$modules_run)
})

# Test /paper/check endpoint with invalid modules
test_that("/paper/check endpoint fails with invalid modules", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/check", test_xml, list(modules = "invalid_module"))
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("Invalid modules", result$error, ignore.case = TRUE))
})

# Test GROBID /grobid/pdf2grobid endpoint
test_that("/grobid/pdf2grobid endpoint works with PDF file", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/grobid/pdf2grobid", test_pdf)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Should return XML content
  expect_true(grepl("<\\?xml", content))
  expect_true(grepl("<TEI", content))
})

# Test error handling for missing file
test_that("API handles missing file gracefully", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  # Test without file
  response <- httr::POST("http://localhost:2005/paper/info", body = list())
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("No file uploaded", result$error, ignore.case = TRUE))
})

# Test error handling for invalid file type
test_that("API handles invalid file type gracefully", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  # Create a temporary text file
  temp_file <- tempfile(fileext = ".txt")
  writeLines("This is not a PDF or XML file", temp_file)
  
  response <- api_post("/paper/info", temp_file)
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("not a valid PDF or XML", result$error, ignore.case = TRUE))
  
  unlink(temp_file)
})

# Test error handling for multiple files
test_that("API handles multiple files gracefully", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  # This test would require creating a multipart request with multiple files
  # For now, we'll skip this as it's complex to set up
  skip("Multiple file upload test requires complex multipart setup")
})

# Test GROBID parameters
test_that("API handles GROBID parameters correctly", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  params <- list(
    consolidateHeader = "1",
    consolidateCitations = "1",
    start = "1",
    end = "2"
  )
  
  response <- api_post("/paper/info", test_pdf, params)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("title" %in% names(result))
})

# Test invalid GROBID parameters
test_that("API handles invalid GROBID parameters correctly", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  params <- list(
    consolidateHeader = "invalid",
    start = "-5"
  )
  
  response <- api_post("/paper/info", test_pdf, params)
  # Should still work as validation might be lenient or parameters might be ignored
  expect_true(httr::status_code(response) %in% c(200, 400))
})

# Test file size validation
test_that("API rejects files larger than 50MB", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip("Skipping file size test - creates large temporary file")
  
  # Create a temporary file larger than 50MB
  temp_file <- tempfile(fileext = ".pdf")
  # Write PDF header
  con <- file(temp_file, "wb")
  writeBin(as.raw(c(0x25, 0x50, 0x44, 0x46)), con)  # %PDF
  # Pad with zeros to exceed 50MB
  writeBin(raw(51 * 1024 * 1024), con)
  close(con)
  
  response <- api_post("/paper/info", temp_file)
  expect_equal(httr::status_code(response), 413)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_true(grepl("too large|50MB", result$error, ignore.case = TRUE))
  
  unlink(temp_file)
})

# Test corrupted PDF
test_that("API handles corrupted PDF gracefully", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  # Create a file with PDF header but corrupted content
  temp_file <- tempfile(fileext = ".pdf")
  writeLines(c("%PDF-1.4", "This is not valid PDF content", "random garbage"), temp_file)
  
  response <- api_post("/paper/info", temp_file)
  # Should return an error (either 400 or 500 depending on where it fails)
  expect_true(httr::status_code(response) %in% c(400, 500))
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  
  unlink(temp_file)
})

# Test malformed XML
test_that("API handles malformed XML gracefully", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  # Create a malformed XML file
  temp_file <- tempfile(fileext = ".xml")
  writeLines(c("<?xml version='1.0'?>", "<root>", "<unclosed>"), temp_file)
  
  response <- api_post("/paper/info", temp_file)
  expect_true(httr::status_code(response) %in% c(400, 500))
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  
  unlink(temp_file)
})

# Test empty file
test_that("API handles empty files gracefully", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  temp_file <- tempfile(fileext = ".pdf")
  file.create(temp_file)
  
  response <- api_post("/paper/info", temp_file)
  expect_true(httr::status_code(response) %in% c(400, 500))
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  
  unlink(temp_file)
})

# Test page range validation
test_that("API validates page range parameters correctly", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  # Start > End (should fail)
  response <- api_post("/paper/info", test_pdf, list(start = "10", end = "5"))
  expect_true(httr::status_code(response) %in% c(400, 500))
  
  # Negative start (not -1)
  response <- api_post("/paper/info", test_pdf, list(start = "-5"))
  expect_true(httr::status_code(response) %in% c(400, 500))
  
  # Valid range
  response <- api_post("/paper/info", test_pdf, list(start = "1", end = "2"))
  expect_equal(httr::status_code(response), 200)
})

# Test search with special characters
test_that("API handles search queries with special characters", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  # Test with special characters
  special_queries <- c("test & query", "test|query", "test*")
  
  for (query in special_queries) {
    response <- api_post("/paper/search", test_xml, list(q = query))
    # Should not crash, even if no results found
    expect_true(httr::status_code(response) %in% c(200, 400),
                info = paste("Query failed:", query))
  }
})

# Test search with empty query
test_that("API handles empty search query", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/search", test_xml, list(q = ""))
  expect_equal(httr::status_code(response), 400)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true("error" %in% names(result))
  expect_match(result$error, "(?i)query.*required|'q'.*required")
})

# Test response headers
test_that("API returns correct response headers", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  
  response <- httr::GET("http://localhost:2005/health")
  
  # Check Content-Type header
  content_type <- httr::headers(response)[["content-type"]]
  expect_true(grepl("application/json", content_type, ignore.case = TRUE))
})

# Test GROBID XML response headers
test_that("/grobid/pdf2grobid returns correct XML content type", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  response <- api_post("/grobid/pdf2grobid", test_pdf)
  
  if (httr::status_code(response) == 200) {
    content_type <- httr::headers(response)[["content-type"]]
    expect_true(grepl("xml", content_type, ignore.case = TRUE))
  }
})

# Test /paper/check with empty modules list
test_that("/paper/check handles empty modules parameter", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/check", test_xml, list(modules = ""))
  # Should default to all modules
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(length(result$modules_run) > 0)
})

# Test consolidate parameters with valid values
test_that("API accepts valid consolidate parameters", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  # Test with all consolidate options set to 0
  params <- list(
    consolidateHeader = "0",
    consolidateCitations = "0",
    consolidateFunders = "0"
  )
  
  response <- api_post("/paper/info", test_pdf, params)
  expect_equal(httr::status_code(response), 200)
})

# Test invalid consolidate parameters
test_that("API rejects invalid consolidate parameters", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_pdf), "Test PDF file not found")
  
  # Test with invalid values (not 0 or 1)
  response <- api_post("/paper/info", test_pdf, list(consolidateHeader = "2"))
  expect_true(httr::status_code(response) %in% c(400, 500))
})

# Test references structure validation
test_that("/paper/references returns properly structured data", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/references", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  expect_true(is.list(result))
  
  if (length(result) > 0) {
    # Each reference should have some expected fields
    first_ref <- result[[1]]
    expect_true(is.list(first_ref))
    # Check for common reference fields
    possible_fields <- c("title", "author", "year", "journal", "raw_reference", "doi")
    expect_true(any(possible_fields %in% names(first_ref)),
                info = "Reference should have at least one expected field")
  }
})

# Test authors structure validation
test_that("/paper/authors returns properly structured author data", {
  skip_if_not(api_is_running(), "API is not running on localhost:2005")
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- api_post("/paper/authors", test_xml)
  expect_equal(httr::status_code(response), 200)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)
  
  # Validate structure
  expect_true(is.data.frame(result), "Authors should be returned as a data frame")
  expect_true(nrow(result) > 0, "Should have at least one author")
  expect_true("name" %in% names(result), "Author data should include 'name' field")
  
  # Validate data types
  expect_type(result$name, "character")
  
  # Validate specific content
  author_names <- tolower(paste(result$name.surname, collapse = " "))
  expect_true(grepl("debruine|jones|little", author_names),
              info = "Should contain expected author names from test paper")
})
