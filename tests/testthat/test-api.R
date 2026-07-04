# test-11-api.R
# Tests for the metacheck Plumber API

# GET test files (bibr JSON — the API no longer accepts GROBID XML)
test_json <- system.file("demos", "to_err_is_human.json", package = "metacheck")
golden_json <- system.file("demos", "golden_bibr_10_2.json", package = "metacheck")

# API base URL (override with METACHECK_API_URL to test a non-default instance)
api_url <- Sys.getenv("METACHECK_API_URL", "http://localhost:2005")

# Helper function to check if API is running
api_is_running <- function() {
  tryCatch({
    resp <- httr2::request(paste0(api_url, "/health")) |>
      httr2::req_timeout(2) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    httr2::resp_status(resp) == 200
  }, error = function(e) FALSE)
}

# Skip all tests if API is not running
skip_if_no_api <- function() {
  skip_on_ci()

  if (!api_is_running()) {
    skip("API is not running. Start it with: Rscript inst/plumber/run_api.R")
  }
}

# Test health endpoint
test_that("Health endpoint returns 200 and proper response", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/health")) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_true("status" %in% names(content))
  # content$status may be a list with one element or a character vector
  expect_equal(as.character(content$status), "ok")
  expect_true("timestamp" %in% names(content))
})

# Test /paper/info endpoint
test_that("/paper/info returns paper info", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/info")) |>
    httr2::req_body_multipart(file = curl::form_file(test_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_type(content, "list")
  # Should have some paper metadata
  expect_true(length(content) > 0)
  expect_equal(content[[1]]$title, "To Err is Human: An Empirical Investigation")
})

# Test /paper/authors endpoint
test_that("/paper/authors returns author table", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/authors")) |>
    httr2::req_body_multipart(file = curl::form_file(test_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_type(content, "list")
  expect_equal(content[[1]]$family, "Lakens")
})

# Test /paper/references endpoint
test_that("/paper/references returns references", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/references")) |>
    httr2::req_body_multipart(file = curl::form_file(test_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_type(content, "list")
  expect_equal(content[[1]]$title, "Faux: Simulation for Factorial Designs")
})

# Test /paper/cross-references endpoint
test_that("/paper/cross-references returns cross-references", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/cross-references")) |>
    httr2::req_body_multipart(file = curl::form_file(test_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_type(content, "list")
})

# Test /paper/search endpoint
test_that("/paper/search finds text in paper", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/search")) |>
    httr2::req_body_multipart(
      file = curl::form_file(test_json),
      pattern = "pre-register"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_type(content, "list")
})

# Test /paper/search without query parameter
test_that("/paper/search requires query parameter", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/search")) |>
    httr2::req_body_multipart(file = curl::form_file(test_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)

  content <- httr2::resp_body_json(resp)
  expect_true("error" %in% names(content))
})

# Test error handling: no file upload
test_that("Endpoints return 400 when no file is uploaded", {
  skip_if_no_api()

  # Force POST (no body): an empty GET would hit a non-existent route, not the
  # upload handler's "no file" path.
  resp <- httr2::request(paste0(api_url, "/paper/info")) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)

  content <- httr2::resp_body_json(resp)
  expect_true("error" %in% names(content))
})

# Test error handling: invalid JSON file
test_that("Endpoints return 400 for non-JSON file", {
  skip_if_no_api()

  # Create a temporary non-JSON file
  tmp_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("This is not JSON", tmp_file)

  resp <- httr2::request(paste0(api_url, "/paper/info")) |>
    httr2::req_body_multipart(file = curl::form_file(tmp_file)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)

  content <- httr2::resp_body_json(resp)
  expect_true("error" %in% names(content))
})

# Current bibr (10.2) golden file parses and checks
test_that("/paper/info handles a 10.2-shaped bibr JSON", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/info")) |>
    httr2::req_body_multipart(file = curl::form_file(golden_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)
})

# Malformed JSON is a client error with the real parse message
test_that("/paper/info rejects malformed JSON with 400", {
  skip_if_no_api()

  bad <- withr::local_tempfile(fileext = ".json")
  writeLines("this is not json {", bad)

  resp <- httr2::request(paste0(api_url, "/paper/info")) |>
    httr2::req_body_multipart(file = curl::form_file(bad)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)
  content <- httr2::resp_body_json(resp)
  expect_true("error" %in% names(content))
})

# A cheap, non-LLM, network-free module used by the /module and /check tests.
cheap_module <- "all_urls"

# Test /paper/modules discovery endpoint
test_that("/paper/modules lists the available modules", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/modules")) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)

  content <- httr2::resp_body_json(resp)
  expect_true("modules" %in% names(content))
  expect_true(length(content$modules) > 0)
  expect_true(cheap_module %in% unlist(content$modules))
})

# Test /paper/module runs a module
test_that("/paper/module runs a module and returns results", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/module")) |>
    httr2::req_body_multipart(
      file = curl::form_file(test_json),
      name = cheap_module
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)
  content <- httr2::resp_body_json(resp)
  expect_type(content, "list")
})

# Test /paper/module rejects an unknown module name
test_that("/paper/module rejects an unknown module with 400", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/module")) |>
    httr2::req_body_multipart(
      file = curl::form_file(test_json),
      name = "no_such_module"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)
  content <- httr2::resp_body_json(resp)
  expect_true("error" %in% names(content))
})

# Test /paper/module requires the name parameter
test_that("/paper/module requires a module name", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/module")) |>
    httr2::req_body_multipart(file = curl::form_file(test_json)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)
})

# Test /paper/check aggregates metadata + module results; report=false skips render
test_that("/paper/check returns metadata and module results (report=false)", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/check")) |>
    httr2::req_body_multipart(
      file = curl::form_file(test_json),
      modules = cheap_module,
      report = "false"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)
  content <- httr2::resp_body_json(resp)
  expect_true(all(c(
    "metacheck_version", "paper_info", "authors", "references",
    "cross_references", "modules_run", "results", "report_html"
  ) %in% names(content)))
  # report=false -> empty report_html, but JSON results are still present
  expect_equal(as.character(content$report_html), "")
  expect_true(length(content$results) > 0)
})

# Test /paper/check renders the HTML report by default
test_that("/paper/check renders an HTML report by default", {
  skip_if_no_api()
  skip_if_not(nzchar(Sys.which("quarto")), "quarto CLI not available")

  resp <- httr2::request(paste0(api_url, "/paper/check")) |>
    httr2::req_body_multipart(
      file = curl::form_file(test_json),
      modules = cheap_module
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 200)
  content <- httr2::resp_body_json(resp)
  expect_true(nchar(as.character(content$report_html)) > 0)
})

# Test /paper/check rejects invalid module names before parsing
test_that("/paper/check rejects invalid module names with 400", {
  skip_if_no_api()

  resp <- httr2::request(paste0(api_url, "/paper/check")) |>
    httr2::req_body_multipart(
      file = curl::form_file(test_json),
      modules = "no_such_module"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  expect_equal(httr2::resp_status(resp), 400)
  content <- httr2::resp_body_json(resp)
  expect_true("error" %in% names(content))
})
