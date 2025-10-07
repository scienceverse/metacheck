# test-11-api.R
# Tests for the papercheck Plumber API

library(testthat)
library(httr)
library(jsonlite)

# Get test files
test_xml <- system.file("grobid", "prereg.xml", package = "papercheck")

# API base URL
api_url <- "http://localhost:2005"

# Helper function to check if API is running
api_is_running <- function() {
  tryCatch({
    response <- GET(paste0(api_url, "/health"), timeout(2))
    status_code(response) == 200
  }, error = function(e) FALSE)
}

# Skip all tests if API is not running
skip_if_no_api <- function() {
  if (!api_is_running()) {
    skip("API is not running. Start it with: Rscript inst/plumber/run_api.R")
  }
}

# Test health endpoint
test_that("Health endpoint returns 200 and proper response", {
  skip_on_ci()
  skip_if_no_api()
  
  response <- GET(paste0(api_url, "/health"))
  
  expect_equal(status_code(response), 200)
  
  content <- content(response, as = "parsed")
  expect_true("status" %in% names(content))
  # content$status may be a list with one element or a character vector
  expect_equal(as.character(content$status), "ok")
  expect_true("timestamp" %in% names(content))
})

# Test /paper/info endpoint
test_that("/paper/info returns paper info", {
  skip_on_ci()
  skip_if_no_api()
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- POST(
    paste0(api_url, "/paper/info"),
    body = list(file = upload_file(test_xml)),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 200)
  
  content <- content(response, as = "parsed")
  expect_type(content, "list")
  # Should have some paper metadata
  expect_true(length(content) > 0)
})

# Test /paper/authors endpoint
test_that("/paper/authors returns author table", {
  skip_on_ci()
  skip_if_no_api()
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- POST(
    paste0(api_url, "/paper/authors"),
    body = list(file = upload_file(test_xml)),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 200)
  
  content <- content(response, as = "parsed")
  expect_type(content, "list")
})

# Test /paper/references endpoint
test_that("/paper/references returns references", {
  skip_on_ci()
  skip_if_no_api()
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- POST(
    paste0(api_url, "/paper/references"),
    body = list(file = upload_file(test_xml)),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 200)
  
  content <- content(response, as = "parsed")
  expect_type(content, "list")
})

# Test /paper/cross-references endpoint
test_that("/paper/cross-references returns cross-references", {
  skip_on_ci()
  skip_if_no_api()
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- POST(
    paste0(api_url, "/paper/cross-references"),
    body = list(file = upload_file(test_xml)),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 200)
  
  content <- content(response, as = "parsed")
  expect_type(content, "list")
})

# Test /paper/search endpoint
test_that("/paper/search finds text in paper", {
  skip_on_ci()
  skip_if_no_api()
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- POST(
    paste0(api_url, "/paper/search"),
    body = list(
      file = upload_file(test_xml),
      pattern = "pre-register"
    ),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 200)
  
  content <- content(response, as = "parsed")
  expect_type(content, "list")
})

# Test /paper/search without query parameter
test_that("/paper/search requires query parameter", {
  skip_on_ci()
  skip_if_no_api()
  skip_if_not(file.exists(test_xml), "Test XML file not found")
  
  response <- POST(
    paste0(api_url, "/paper/search"),
    body = list(file = upload_file(test_xml)),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 400)
  
  content <- content(response, as = "parsed")
  expect_true("error" %in% names(content))
})

# Test error handling: no file upload
test_that("Endpoints return 400 when no file is uploaded", {
  skip_on_ci()
  skip_if_no_api()
  
  response <- POST(
    paste0(api_url, "/paper/info"),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 400)
  
  content <- content(response, as = "parsed")
  expect_true("error" %in% names(content))
})

# Test error handling: invalid XML file
test_that("Endpoints return 400 for invalid XML", {
  skip_on_ci()
  skip_if_no_api()
  
  # Create a temporary non-XML file
  tmp_file <- tempfile(fileext = ".txt")
  writeLines("This is not XML", tmp_file)
  on.exit(unlink(tmp_file))
  
  response <- POST(
    paste0(api_url, "/paper/info"),
    body = list(file = upload_file(tmp_file)),
    encode = "multipart"
  )
  
  expect_equal(status_code(response), 400)
  
  content <- content(response, as = "parsed")
  expect_true("error" %in% names(content))
})