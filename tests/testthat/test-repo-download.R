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
  expect_equal(nrow(attr(dl, "omitted")), 0)
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

test_that("download_repo_files omits files over the per-file size cap", {
  files <- make_dl_files()
  unlink(metacheck:::.repo_cache_subdir(files$repo_url[1]), recursive = TRUE)
  # cap far below the file size → both omitted
  dl <- download_repo_files(files, max_file_size = 0.00001,
                            max_download_size = 100)
  expect_equal(sum(!is.na(dl$file_location)), 0)
  expect_equal(nrow(attr(dl, "omitted")), 2)
})

test_that("cache paths are stable and per-repo", {
  a <- metacheck:::.repo_cache_subdir("https://osf.io/abc")
  b <- metacheck:::.repo_cache_subdir("https://osf.io/abc")
  cc <- metacheck:::.repo_cache_subdir("https://osf.io/xyz")
  expect_equal(a, b)          # stable
  expect_false(a == cc)       # distinct repos differ
})
