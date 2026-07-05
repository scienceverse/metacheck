# Tests for download_repo_files() and its cache. Run offline by pointing
# file_url at local file:// URLs (httr2 fetches these without network).

# Mark the one-per-session cache-location notice as already shown, so it does
# not fire inside unrelated download tests. The dedicated notice test opts back
# in with withr::local_options(metacheck.repo_cache.notified = NULL).
# Also isolate BOTH caches from the real user cache: the persistent cache
# (repo_cache.dir) and the per-session temp cache (repo_cache.session_dir), so
# no test writes into the user's actual downloads or sees another test's files.
options(
  metacheck.repo_cache.notified = TRUE,
  metacheck.repo_cache.dir = file.path(tempdir(), "mc-test-persist-cache"),
  metacheck.repo_cache.session_dir = file.path(tempdir(), "mc-test-session-cache"))

# Build a files data.frame (repo_check shape) pointing at local source files.
# Each call uses a fresh unique repo URL so tests never collide in either cache
# (cache = FALSE writes to the session dir, which persists across tests in a run).
make_dl_files <- function(sizes = c(100, 100)) {
  repo <- paste0("https://example.org/repo-test-",
                 paste(sample(c(letters, 0:9), 12, TRUE), collapse = ""))
  srcs <- vapply(seq_along(sizes), function(i) {
    p <- tempfile(fileext = ".csv")
    writeLines(paste(rep("x", sizes[i]), collapse = ""), p)
    p
  }, character(1))
  data.frame(
    repo_url  = repo,
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

test_that("failed downloads are reported and recorded, not swallowed", {
  # One good local file, one URL that cannot resolve: the good file downloads,
  # the bad one lands in the "failed" attribute and a message names it (the
  # old behaviour was a silent FALSE — the batch looked complete while files
  # were missing).
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  files$file_url[2] <- "file:///nonexistent/never/there.csv"

  expect_message(
    dl <- download_repo_files(files, max_file_size = 10, max_download_size = 100),
    "failed after retries"
  )
  expect_false(is.na(dl$file_location[1]))
  expect_true(is.na(dl$file_location[2]))
  fa <- attr(dl, "failed")
  expect_equal(nrow(fa), 1)
  expect_equal(fa$file_name, "f2.csv")
  expect_true(nzchar(fa$error))
})

test_that("cache paths are stable and per-repo", {
  a <- metacheck:::.repo_cache_subdir("https://osf.io/abc")
  b <- metacheck:::.repo_cache_subdir("https://osf.io/abc")
  cc <- metacheck:::.repo_cache_subdir("https://osf.io/xyz")
  expect_equal(a, b)          # stable
  expect_false(a == cc)       # distinct repos differ
})

test_that("repo_cache_clear removes cached files and reports freed space", {
  # Isolate from the real user cache so size checks are hermetic.
  withr::local_options(metacheck.repo_cache.dir = withr::local_tempdir())
  # Download two small files (cache = TRUE → persistent cache), then clear one
  # repo, then everything.
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  dl <- download_repo_files(files, max_file_size = 10, max_download_size = 100,
                            cache = TRUE)
  expect_true(all(file.exists(dl$file_location)))
  before <- repo_cache_size()
  expect_gt(before, 0)

  # Targeted clear: only this repo's subdir is removed.
  expect_message(
    freed <- repo_cache_clear(files$repo_url[1]),
    "Cleared 1 cached repository")
  expect_gt(freed, 0)
  expect_false(dir.exists(metacheck:::.repo_cache_subdir(files$repo_url[1])))
  expect_false(any(file.exists(dl$file_location)))

  # Clearing a now-absent repo frees nothing and does not error.
  expect_equal(suppressMessages(repo_cache_clear(files$repo_url[1])), 0)
})

test_that("repo_cache_clear() with no args empties the whole cache", {
  withr::local_options(metacheck.repo_cache.dir = withr::local_tempdir())
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  download_repo_files(files, max_file_size = 10, max_download_size = 100,
                      cache = TRUE)
  expect_gt(repo_cache_size(), 0)

  freed <- suppressMessages(repo_cache_clear())
  expect_gt(freed, 0)
  # Root still exists (recreated empty) so later downloads can write.
  expect_true(dir.exists(repo_cache_dir()))
  expect_equal(repo_cache_size(), 0)
})

test_that("cache = TRUE notice is printed once per session; cache = FALSE stays silent", {
  withr::local_options(metacheck.repo_cache.notified = NULL,
                       metacheck.repo_cache.dir = withr::local_tempdir())
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)

  # First persistent-cache download announces the cache + how to clear it.
  expect_message(
    download_repo_files(files, max_file_size = 10, max_download_size = 100,
                        cache = TRUE),
    "repo_cache_clear\\(\\)")

  # A second download does not repeat it (flag is set).
  files2 <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files2$repo_url[1]), recursive = TRUE)
  msgs <- character(0)
  withCallingHandlers(
    download_repo_files(files2, max_file_size = 10, max_download_size = 100,
                        cache = TRUE),
    message = function(m) msgs <<- c(msgs, conditionMessage(m)))
  expect_false(any(grepl("repo_cache_clear", msgs)))
})

test_that("cache = FALSE downloads to a temp dir and never touches the persistent cache", {
  withr::local_options(metacheck.repo_cache.notified = NULL,
                       metacheck.repo_cache.dir = withr::local_tempdir(),
                       metacheck.repo_cache.session_dir = NULL)
  files <- make_dl_files()

  # No cache-location notice for a non-persistent download.
  msgs <- character(0)
  dl <- withCallingHandlers(
    download_repo_files(files, max_file_size = 10, max_download_size = 100),
    message = function(m) msgs <<- c(msgs, conditionMessage(m)))
  expect_false(any(grepl("repo_cache_clear", msgs)))

  # Files were fetched, but into the session temp dir — not the persistent cache.
  expect_true(all(file.exists(dl$file_location)))
  expect_equal(repo_cache_size(), 0)
  session_dir <- getOption("metacheck.repo_cache.session_dir")
  expect_true(all(startsWith(normalizePath(dl$file_location),
                             normalizePath(session_dir))))
})

test_that("zip timeout is passed to zip transport", {
  files <- data.frame(
    repo_url = "https://osf.io/abcde",
    file_name = "a.csv",
    file_path = "a.csv",
    file_url = "https://files.osf.io/v1/resources/abcde/providers/osfstorage/a.csv",
    file_size = 1024,
    file_location = NA_character_,
    stringsAsFactors = FALSE
  )
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)

  local_mocked_bindings(
    osf_check_id = function(x) "abcde",
    .remote_content_length = function(url) NA_real_,
    .download_zip_to_cache = function(files, row_idx, zip_url, strip_dir,
                                      req_func, timeout_s) {
      expect_equal(timeout_s, 7)
      files$file_location[row_idx] <- files$.cache_path[row_idx]
      files
    },
    .package = "metacheck"
  )

  dl <- download_repo_files(files, max_file_size = 10, max_download_size = 100,
                            zip_timeout_s = 7)
  expect_false(is.na(dl$file_location[1]))
})

test_that("warns when archive transport is larger than selected files", {
  files <- data.frame(
    repo_url = "https://osf.io/abcde",
    file_name = "a.csv",
    file_path = "a.csv",
    file_url = "https://files.osf.io/v1/resources/abcde/providers/osfstorage/a.csv",
    file_size = 1024,
    file_location = NA_character_,
    stringsAsFactors = FALSE
  )
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)

  local_mocked_bindings(
    osf_check_id = function(x) "abcde",
    .remote_content_length = function(url) 50 * 1024 * 1024,
    .download_zip_to_cache = function(files, row_idx, zip_url, strip_dir,
                                      req_func, timeout_s) {
      files$file_location[row_idx] <- files$.cache_path[row_idx]
      files
    },
    .package = "metacheck"
  )

  expect_warning(
    dl <- download_repo_files(files, max_file_size = 10, max_download_size = 500),
    "larger archive transport"
  )
  expect_false(is.na(dl$file_location[1]))
})

test_that("OSF non-osfstorage rows fall back to file-by-file", {
  files <- data.frame(
    repo_url = c("https://osf.io/abcde", "https://osf.io/abcde"),
    file_name = c("a.csv", "b.csv"),
    file_path = c("a.csv", "b.csv"),
    file_url = c(
      "https://files.osf.io/v1/resources/abcde/providers/osfstorage/a.csv",
      "https://files.osf.io/v1/resources/abcde/providers/dropbox/b.csv"
    ),
    file_size = c(1024, 1024),
    file_location = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)

  fallback_n <- 0L
  local_mocked_bindings(
    osf_check_id = function(x) "abcde",
    .remote_content_length = function(url) NA_real_,
    .download_zip_to_cache = function(files, row_idx, zip_url, strip_dir,
                                      req_func, timeout_s) {
      # zip transport should only cover the osfstorage row
      expect_equal(length(row_idx), 1)
      files$file_location[row_idx] <- files$.cache_path[row_idx]
      files
    },
    .download_one = function(url, dest) {
      fallback_n <<- fallback_n + 1L
      NA_character_
    },
    .package = "metacheck"
  )

  dl <- download_repo_files(files, max_file_size = 10, max_download_size = 100)
  expect_equal(fallback_n, 1L)
  expect_true(all(!is.na(dl$file_location)))
})
