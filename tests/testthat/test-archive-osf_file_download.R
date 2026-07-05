test_that("osf_file_download", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  expect_true(is.function(metacheck::osf_file_download))
  expect_no_error(helplist <- help(osf_file_download, metacheck))

  expect_warning(x <- osf_file_download("notanid"))
  expect_null(x)

  osf_id <- "6nt4v" # processed data - 1 file
  node_name <- "Processed_Data"

  download_to <- withr::local_tempdir()
  op <- capture_messages(
    dl <- osf_file_download(osf_id, download_to)
  )
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, "osfstorage", node_name, "processed-data.csv") |> file.exists())
  expect_equal(dl$folder, osf_id)
  expect_equal(dl$downloaded, TRUE)
  expect_true(nchar(dl$osf_id) %in% c(5, 24))

  ## second download with existing file (same download_to so it sees existing dir)
  op <- capture_messages(
    dl2 <- osf_file_download(osf_id, download_to)
  )
  folder <- paste0(osf_id, "_1")
  expect_equal(dl2$folder, folder)
  f2 <- file.path(download_to, folder)
  expect_true(dir.exists(f2))

  unlink(f, recursive = TRUE)
  unlink(f2, recursive = TRUE)

  # error in one ID
  osf_id <- c("yuck", "6nt4v")
  expect_warning(dl3 <- osf_file_download(osf_id, download_to), "yuck")
  expect_equal(dl$name, dl3$name)
  f3 <- file.path(download_to, "6nt4v")
  unlink(f3, recursive = TRUE)
}, "mock")

test_that("too small max_file_size", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  osf_id <- "6nt4v"
  tmpdir2 <- withr::local_tempdir()
  op <- capture_messages(
    dl <- osf_file_download(osf_id, tmpdir2,
                            max_file_size = .0001)
  )
  expect_equal(nrow(dl), 1)
  expect_equal(dl$folder, osf_id)
  expect_true(nchar(dl$osf_id) %in% c(5, 24))
  expect_equal(dl$downloaded, FALSE)
  expect_true(any(grepl("per-file limit", op, fixed = TRUE)))
  f <- file.path(tmpdir2, osf_id)
  expect_true(dir.exists(f))
  expect_equal(list.files(f), character(0))
}, "mock")

test_that("too small max_download_size", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  osf_id <- "6nt4v"
  tmpdir <- withr::local_tempdir()
  expect_warning(op <- capture_messages(
    dl <- osf_file_download(osf_id, tmpdir,
                            max_download_size = .0001)
  ), "per-repository limit")
  expect_equal(nrow(dl), 1)
  expect_equal(dl$folder, osf_id)
  expect_true(nchar(dl$osf_id) %in% c(5, 24))
  expect_equal(dl$downloaded, FALSE)
  expect_true(any(grepl("per-repository limit", op, fixed = TRUE)))
  f <- file.path(tmpdir, osf_id)
  expect_true(dir.exists(f))
  expect_equal(list.files(f), character(0))
}, "mock")

test_that("osf_file_download zip keep archive", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  mock_id <- "abcde"
  contents <- data.frame(
    osf_type = c("files", "nodes"),
    osf_id = c("file1", "child1"),
    name = c("processed-data.csv", "Processed_Data"),
    provider = c("osfstorage", NA),
    path = c("/processed-data.csv", NA),
    kind = c("file", "folder"),
    size = c(8, NA),
    download_url = c("https://example.test/file.csv", NA),
    parent = c("child1", mock_id),
    project = c("child1", mock_id),
    filetype = c("csv", NA),
    downloads = c(1, NA),
    stringsAsFactors = FALSE
  )

  zip_src <- withr::local_tempdir()
  writeLines("x,y\n1,2", file.path(zip_src, "processed-data.csv"))
  withr::with_dir(zip_src,
                  utils::zip("mock.zip", "processed-data.csv", flags = "-q"))
  zip_raw <- readBin(file.path(zip_src, "mock.zip"), "raw",
                     n = file.info(file.path(zip_src, "mock.zip"))$size)

  download_to <- withr::local_tempdir()
  dl <- with_mocked_bindings(
    with_mocked_bindings(
      osf_file_download(mock_id, download_to, mode = "zip", unzip = FALSE),
      request = function(url) structure(list(url = url, method = "GET"), class = "httr2_request"),
      req_method = function(req, method) {
        req$method <- method
        req
      },
      req_error = function(req, is_error) req,
      req_perform = function(req) {
        structure(list(status = 200, headers = list(), body = raw()), class = "httr2_response")
      },
      resp_status = function(resp) resp$status,
      resp_header = function(resp, name) resp$headers[[tolower(name)]] %||% NA_character_,
      resp_body_raw = function(resp) resp$body,
      .package = "httr2"
    ),
    osf_info = function(...) contents,
    .batch_query = function(...) {
      list(structure(list(status = 200, body = zip_raw), class = "httr2_response"))
    },
    .package = "metacheck"
  )

  expect_equal(nrow(dl), 1)
  expect_true(dl$downloaded[[1]])
  expect_equal(dl$path[[1]], "abcde.zip")
  expect_true(file.exists(file.path(download_to, mock_id, "abcde.zip")))
}, "none")

test_that("osf_file_download zip unzip preserves structure", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  mock_id <- "abcde"
  contents <- data.frame(
    osf_type = c("files", "nodes"),
    osf_id = c("file1", "child1"),
    name = c("processed-data.csv", "Processed_Data"),
    provider = c("osfstorage", NA),
    path = c("/processed-data.csv", NA),
    kind = c("file", "folder"),
    size = c(8, NA),
    download_url = c("https://example.test/file.csv", NA),
    parent = c("child1", mock_id),
    project = c("child1", mock_id),
    filetype = c("csv", NA),
    downloads = c(1, NA),
    stringsAsFactors = FALSE
  )

  zip_src <- withr::local_tempdir()
  writeLines("x,y\n1,2", file.path(zip_src, "processed-data.csv"))
  withr::with_dir(zip_src,
                  utils::zip("mock.zip", "processed-data.csv", flags = "-q"))
  zip_raw <- readBin(file.path(zip_src, "mock.zip"), "raw",
                     n = file.info(file.path(zip_src, "mock.zip"))$size)

  download_to <- withr::local_tempdir()
  dl <- with_mocked_bindings(
    with_mocked_bindings(
      osf_file_download(mock_id, download_to, mode = "zip", unzip = TRUE),
      request = function(url) structure(list(url = url, method = "GET"), class = "httr2_request"),
      req_method = function(req, method) {
        req$method <- method
        req
      },
      req_error = function(req, is_error) req,
      req_perform = function(req) {
        structure(list(status = 200, headers = list(), body = raw()), class = "httr2_response")
      },
      resp_status = function(resp) resp$status,
      resp_header = function(resp, name) resp$headers[[tolower(name)]] %||% NA_character_,
      resp_body_raw = function(resp) resp$body,
      .package = "httr2"
    ),
    osf_info = function(...) contents,
    .batch_query = function(...) {
      list(structure(list(status = 200, body = zip_raw), class = "httr2_response"))
    },
    .package = "metacheck"
  )

  expect_equal(dl$path[[1]], "osfstorage/Processed_Data/processed-data.csv")
  expect_true(dl$downloaded[[1]])
  expect_false(file.exists(file.path(download_to, mock_id, "abcde.zip")))
  expect_true(file.exists(file.path(download_to, mock_id,
                                    "osfstorage", "Processed_Data", "processed-data.csv")))
}, "none")

test_that("osf_file_download zip unzip can flatten structure", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  mock_id <- "abcde"
  contents <- data.frame(
    osf_type = c("files", "files", "nodes"),
    osf_id = c("file1", "file2", mock_id),
    name = c("README", "README", "Project Root"),
    provider = c("osfstorage", "osfstorage", NA),
    path = c("/README", "/nested/README", NA),
    kind = c("file", "file", "folder"),
    size = c(8, 9, NA),
    download_url = c("https://example.test/README", "https://example.test/nested/README", NA),
    parent = c(mock_id, mock_id, NA),
    project = c(mock_id, mock_id, NA),
    filetype = c("txt", "txt", NA),
    downloads = c(1, 1, NA),
    stringsAsFactors = FALSE
  )

  zip_src <- withr::local_tempdir()
  dir.create(file.path(zip_src, "nested"))
  writeLines("root", file.path(zip_src, "README"))
  writeLines("nested", file.path(zip_src, "nested", "README"))
  withr::with_dir(zip_src,
                  utils::zip("mock.zip", c("README", "nested/README"), flags = "-q"))
  zip_raw <- readBin(file.path(zip_src, "mock.zip"), "raw",
                     n = file.info(file.path(zip_src, "mock.zip"))$size)

  download_to <- withr::local_tempdir()
  dl <- with_mocked_bindings(
    with_mocked_bindings(
      osf_file_download(mock_id, download_to, mode = "zip", unzip = TRUE,
                        ignore_folder_structure = TRUE),
      request = function(url) structure(list(url = url, method = "GET"), class = "httr2_request"),
      req_method = function(req, method) {
        req$method <- method
        req
      },
      req_error = function(req, is_error) req,
      req_perform = function(req) {
        structure(list(status = 200, headers = list(), body = raw()), class = "httr2_response")
      },
      resp_status = function(resp) resp$status,
      resp_header = function(resp, name) resp$headers[[tolower(name)]] %||% NA_character_,
      resp_body_raw = function(resp) resp$body,
      .package = "httr2"
    ),
    osf_info = function(...) contents,
    .batch_query = function(...) {
      list(structure(list(status = 200, body = zip_raw), class = "httr2_response"))
    },
    .package = "metacheck"
  )

  expect_setequal(dl$path, c("README", "file2-README"))
  expect_true(all(dl$downloaded))
  expect_true(file.exists(file.path(download_to, mock_id, "README")))
  expect_true(file.exists(file.path(download_to, mock_id, "file2-README")))
}, "none")

test_that("nested", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  skip_if_quick()

  osf_id <- "j3gcx"
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  # list.files(download_to, recursive = T)

  storage <- "osfstorage"
  node <- "Raw_Data"
  f <- file.path(download_to, osf_id, storage, node, "nest-1")
  expect_true(dir.exists(f))
  f <- file.path(download_to, osf_id, storage, node, "data.xlsx")
  expect_true(file.exists(f))
  exp_paths <- c("README",
                 "data.xlsx",
                 "nest-1/README",
                 "nest-1/test-1.txt",
                 "nest-1/nest-2/test-2.txt",
                 "nest-1/nest-2/nest-3/test-3.txt",
                 "nest-1/nest-2/nest-3/nest-4/test-4.txt") |>
    file.path(storage, node, x = _)
  expect_setequal(dl$path, exp_paths)

  for (path in exp_paths) {
    expect_true(file.path(download_to, osf_id, path) |> file.exists())
  }

}, "mock")

test_that("truncate", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  skip_if_quick()

  osf_id <- "j3gcx"
  download_to <- withr::local_tempdir()
  expect_warning(op <- capture_messages(
    dl <- osf_file_download(osf_id, download_to,
                            max_folder_length = 3)
  ), "truncated")
  # list.files(download_to, recursive = T)

  storage <- "osf"
  node <- "Raw"
  f <- file.path(download_to, osf_id, storage, node, "nes")
  expect_true(dir.exists(f))
  f <- file.path(download_to, osf_id, storage, node, "data.xlsx")
  expect_true(file.exists(f))
  exp_paths <- c("README",
                 "data.xlsx",
                 "nes/README",
                 "nes/test-1.txt",
                 "nes/nes/test-2.txt",
                 "nes/nes/nes/test-3.txt",
                 "nes/nes/nes/nes/test-4.txt") |>
    file.path(storage, node, x = _)
  expect_setequal(dl$path, exp_paths)

  for (path in exp_paths) {
    expect_true(file.path(download_to, osf_id, path) |> file.exists())
  }
}, "mock")

test_that("multiple osf_ids", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  skip_if_quick()

  osf_id <- c("6nt4v", "j3gcx")
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  # list.files(download_to, recursive = T)

  storage <- "osfstorage"
  expect_equal(dl$folder, rep(osf_id, c(1, 7)))
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f) |> all())
  expect_true(file.path(f[[1]], storage, "Processed_Data", "processed-data.csv") |> file.exists())
  expect_true(file.path(f[[2]], storage, "Raw_Data", "nest-1/README") |> file.exists())
}, "mock")

test_that("Waterbutler ID for folder", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  osf_id <- "https://files.de-1.osf.io/v1/resources/j3gcx/providers/osfstorage/685a46eb8c103f8ab307047f/?zip="
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  # list.files(download_to, recursive = T)

  expect_true(all(dl$folder == "685a46eb8c103f8ab307047f"))
  f <- file.path(download_to, "685a46eb8c103f8ab307047f", "osfstorage", "nest-1")
  expect_true(dir.exists(f))
  expect_true(file.path(f, "nest-2") |> dir.exists())
  expect_true(file.path(f, "README") |> file.exists())
}, "mock")

test_that("osf_file_download github", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  skip_if_quick() # won't be mocked

  osf_id <- "mc45x"
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, "osfstorage", "DESCRIPTION") |>
                file.exists())
  expect_true(file.path(f, "github", "README.md") |>
                file.exists())
})

test_that("osf_file_download long unnested", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  skip_if_quick()

  osf_id <- "j3gcx"

  # unnested with duplicate file names
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to,
                          ignore_folder_structure = TRUE)
  expect_true("test-4.txt" %in% dl$path)
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f))
  # duplicate READMEs get parent ID appended — check structurally
  readme_files <- grep("README$", list.files(f), value = TRUE)
  expect_equal(length(readme_files), 2)
  expect_true("test-4.txt" %in% list.files(f))
  expect_false(file.path(f, "nest-1") |> dir.exists())
}, "mock")

test_that("osf_file_download ignore_folder_structure", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  skip_if_quick() # won't be mocked

  # https://github.com/scienceverse/metacheck/issues/100
  osf_id <- c("mjrpy")

  download_to <- withr::local_tempdir()
  x <- osf_file_download(osf_id = osf_id,
                         download_to = download_to,
                         ignore_folder_structure = TRUE
                         )

  destdir <- file.path(download_to, osf_id)

  f <- list.files(destdir)
  expect_true("S1.pdf" %in% f)
  expect_true("5f86ba100847120103fddec6-S1.pdf" %in% f)
})

test_that("osf_file_download issue 99", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  # https://github.com/scienceverse/metacheck/issues/99
  osf_id <- c("msfcn")
  download_to <- withr::local_tempdir()
  x <- osf_file_download(osf_id, download_to)
  destdir <- file.path(download_to, osf_id)

  f <- list.files(destdir, recursive = TRUE)
  expect_equal(length(f), 3)
}, "mock")

test_that("osf_file_download registrations", {
  osf_cache_clear()
  withr::defer(osf_cache_clear())

  # https://github.com/scienceverse/metacheck/issues/249

  #skip_if_quick()

  osf_id <- "jqkg7"
  contents <- osf_info(osf_id, recursive = TRUE)
  expect_contains(contents$kind, c("folder", "file"))
}, "mock")
