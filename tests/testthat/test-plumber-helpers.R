# Unit tests for plumber response helpers.
# Sources helpers.R directly — no running API needed.

source(testthat::test_path("..", "..", "inst", "plumber", "utils", "helpers.R"))

# Regression: LLM modules embed `ellmer_output`-classed values (from ellmer's
# structured extraction) in their table columns. jsonlite has no asJSON method
# for that class, so serializing a /check or /module response aborted the whole
# request with "No method asJSON S3 class: ellmer_output". json_safe() must
# strip such classes to plain, serializable values.

ellmer_table <- function() {
  t <- data.frame(claim = c("c1", "c2"), stringsAsFactors = FALSE)
  t$apriori <- structure(c('{"apriori": false}', '{"apriori": true}'),
                         class = "ellmer_output")
  t
}

test_that("raw ellmer_output column is unserializable (the bug)", {
  expect_error(
    jsonlite::toJSON(ellmer_table(), auto_unbox = TRUE),
    "asJSON"
  )
})

test_that("json_safe strips ellmer_output so the table serializes", {
  safe <- json_safe(ellmer_table())
  expect_silent(out <- jsonlite::toJSON(safe, auto_unbox = TRUE))
  # underlying JSON-string values are preserved, just declassed
  expect_false(inherits(safe$apriori, "ellmer_output"))
  expect_equal(safe$apriori, c('{"apriori": false}', '{"apriori": true}'))
})

test_that("json_safe handles a list-column of per-row ellmer_output", {
  t <- data.frame(x = 1:2)
  t$llm <- I(list(structure("a", class = "ellmer_output"),
                  structure("b", class = "ellmer_output")))
  safe <- json_safe(list(module = "m", summary_table = t))
  expect_silent(jsonlite::toJSON(safe, auto_unbox = TRUE))
})

test_that("json_safe sanitizes nested module-output lists", {
  obj <- structure(
    list(module = "causal_claims", table = ellmer_table(),
         section = "general", traffic_light = "info"),
    class = "metacheck_module_output"
  )
  expect_silent(jsonlite::toJSON(json_safe(obj), auto_unbox = TRUE))
})

test_that("json_safe leaves ordinary columns untouched", {
  t <- data.frame(
    n = 1:2,
    f = factor(c("a", "b")),
    d = as.Date(c("2026-01-01", "2026-01-02"))
  )
  safe <- json_safe(t)
  expect_equal(safe$n, 1:2)
  expect_s3_class(safe$d, "Date")
  expect_s3_class(safe$f, "factor")
  expect_silent(jsonlite::toJSON(safe, auto_unbox = TRUE))
})
