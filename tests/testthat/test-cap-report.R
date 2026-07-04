# Tests for the shared cap-gate message builders (report-helpers.R). These are
# pure string builders: NULL when the work fits, otherwise a message naming the
# parameter(s), current value(s), and the value needed to proceed.

test_that("cap_gate_size returns NULL when the repo fits", {
  expect_null(cap_gate_size(
    repo = "https://osf.io/abc", n_files = 3, total_mb = 40,
    oversized = data.frame(name = character(0), size_mb = numeric(0)),
    max_file_size = 100, max_download_size = 500))
})

test_that("cap_gate_size flags an oversized file and names both params", {
  msg <- cap_gate_size(
    repo = "https://osf.io/abc", n_files = 5, total_mb = 34,
    oversized = data.frame(name = "big.RData", size_mb = 5125),
    max_file_size = 100, max_download_size = 500)
  expect_type(msg, "character")
  expect_match(msg, "was not downloaded")
  expect_match(msg, "per-file limit")
  # lift-to value is ceil(largest) = 5125
  expect_match(msg, "max_file_size >= 5125")
  expect_match(msg, "max_download_size >=")
})

test_that("cap_gate_size flags an over-total repo even with no single big file", {
  msg <- cap_gate_size(
    repo = "https://osf.io/abc", n_files = 20, total_mb = 800,
    oversized = data.frame(name = character(0), size_mb = numeric(0)),
    max_file_size = 100, max_download_size = 500)
  expect_match(msg, "per-repository limit")
  expect_match(msg, "800 MB")
  expect_match(msg, "max_download_size >= 800")
})

test_that("cap_gate_size does not cap total when max_download_size is Inf", {
  expect_null(cap_gate_size(
    repo = "r", n_files = 2, total_mb = 9000,
    oversized = data.frame(name = character(0), size_mb = numeric(0)),
    max_file_size = Inf, max_download_size = Inf))
})

test_that("cap_gate_count returns NULL within the cap and a message over it", {
  expect_null(cap_gate_count(30, "file_limit", 30, "tabular data file",
                             context = "repo-x", action = "extract"))
  msg <- cap_gate_count(71, "file_limit", 30, "tabular data file",
                        context = "https://osf.io/abc", action = "extract")
  expect_match(msg, "file_limit` cap of 30")
  expect_match(msg, "file_limit >= 71")
  expect_match(msg, "extract them")
  expect_match(msg, "https://osf.io/abc")
})

test_that("cap_gate_unknown names the file and the Inf instruction", {
  msg <- cap_gate_unknown("https://osf.io/abc", "bundle.zip")
  expect_match(msg, "could not be")
  expect_match(msg, "bundle.zip")
  expect_match(msg, "max_file_size = Inf")
  expect_match(msg, "max_download_size = Inf")
})
