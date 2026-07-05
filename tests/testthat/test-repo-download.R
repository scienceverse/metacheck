# Tests for download_repo_files() and its cache. Run offline by pointing
# file_url at local file:// URLs (httr2 fetches these without network).

# Build a files data.frame (repo_check shape) pointing at local source files.
make_dl_files <- function(sizes = c(100, 100)) {
  srcs <- vapply(seq_along(sizes), function(i) {
    p <- tempfile(fileext = ".csv")
    writeLines(paste(rep("x", sizes[i]), collapse = ""), p)
    p
  }, character(1))
  data.frame(
    repo_url  = "https://example.org/repo-test-XYZ",
    file_name = paste0("f", seq_along(srcs), ".csv"),
    file_path = paste0("data/f", seq_along(srcs), ".csv"),
    file_url  = paste0("file:///", gsub("\\\\", "/", srcs)),
    file_size = file.info(srcs)$size,
    file_location = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("download_repo_files downloads and populates file_location", {
  # isolate the cache so the test is hermetic
  files <- make_dl_files()
  # clear any prior cache for this repo
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)

  dl <- download_repo_files(files, max_file_size = 10, max_download_size = 100)
  expect_equal(sum(!is.na(dl$file_location)), 2)
  expect_true(all(file.exists(dl$file_location)))
  expect_equal(nrow(attr(dl, "gated")), 0)
})

test_that("download_repo_files reuses the cache without re-downloading", {
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)

  dl1 <- download_repo_files(files)
  # Break the source URLs so a re-download would fail; reuse must still work.
  files$file_url <- "file:///nonexistent/path.csv"
  dl2 <- download_repo_files(files)
  expect_equal(dl1$file_location, dl2$file_location)
  expect_true(all(file.exists(dl2$file_location)))
})

test_that("a file over the per-file cap is skipped, the rest downloads", {
  # max_file_size is a per-file filter: the oversized file is left out but the
  # rest of the repository still downloads (the repo is NOT gated).
  files <- make_dl_files(sizes = c(50, 5000))   # 2nd file much larger on disk
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  cap_mb <- files$file_size[2] / (1024 * 1024) * 0.5   # between the two sizes
  dl <- download_repo_files(files, max_file_size = cap_mb,
                            max_download_size = 100)
  expect_equal(sum(!is.na(dl$file_location)), 1)        # the small one only
  expect_equal(nrow(attr(dl, "gated")), 0)              # repo not gated
  os <- attr(dl, "oversize_skipped")
  expect_equal(nrow(os), 1)
  expect_equal(os$file_name, "f2.csv")
})

test_that("a repo over the total cap is gated (nothing downloaded)", {
  files <- make_dl_files(sizes = c(2000, 2000))  # ~2 KB each on disk
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  # per-file cap generous, total cap tiny → total gate fires
  total_mb <- sum(files$file_size) / (1024 * 1024)
  dl <- download_repo_files(files, max_file_size = 100,
                            max_download_size = total_mb / 2)
  expect_equal(sum(!is.na(dl$file_location)), 0)
  expect_match(attr(dl, "gated")$message, "per-repository limit")
})

test_that("NA manifest size falls back to a HEAD Content-Length probe", {
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  files$file_size <- NA_real_   # force the probe path

  # Stub .remote_size so both files resolve to 6 GB (> the 100 MB per-file cap):
  # both are filtered out individually, so nothing downloads but the repo is not
  # "gated" — it's the per-file filter, recorded in oversize_skipped.
  local_mocked_bindings(.remote_size = function(url) 6e9)  # 6 GB
  dl <- download_repo_files(files, max_file_size = 100, max_download_size = 500)
  expect_equal(sum(!is.na(dl$file_location)), 0)
  expect_equal(nrow(attr(dl, "oversize_skipped")), 2)
})

test_that("an undeterminable size gates the repo with the Inf instruction", {
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  files$file_size <- NA_real_

  local_mocked_bindings(.remote_size = function(url) NA_real_)  # no header
  dl <- download_repo_files(files, max_file_size = 100, max_download_size = 500)
  expect_equal(sum(!is.na(dl$file_location)), 0)
  expect_match(attr(dl, "gated")$message, "could not be determined")
  expect_match(attr(dl, "gated")$message, "max_file_size = Inf")
})

test_that("cache paths are stable and per-repo", {
  a <- metacheck:::.repo_cache_subdir("https://osf.io/abc")
  b <- metacheck:::.repo_cache_subdir("https://osf.io/abc")
  cc <- metacheck:::.repo_cache_subdir("https://osf.io/xyz")
  expect_equal(a, b)          # stable
  expect_false(a == cc)       # distinct repos differ
})
