# options(metacheck.osf.api = "https://api.osf.io/v2/")
# osf_delay(0)

#httptest2::start_capturing()
httptest2::use_mock_api()

# testthat::local_mocked_bindings(
#   online = \(...) TRUE
# )

test_that("osf_file_download", {
  expect_warning(x <- osf_file_download("notanid"))
  expect_null(x)

  osf_id <- "6nt4v" # processed data - 1 file
  node_name <- "Processed Data"

  tmpdir <- withr::local_tempdir()
  op <- capture_messages(
    dl <- osf_file_download(osf_id, tmpdir)
  )
  f <- file.path(tmpdir, osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, node_name, "processed-data.csv") |> file.exists())
  expect_equal(dl$folder, osf_id)
  expect_equal(dl$downloaded, TRUE)
  expect_true(nchar(dl$osf_id) %in% c(5, 24))

  ## second download with existing file (same tmpdir so it sees existing dir)
  op <- capture_messages(
    dl2 <- osf_file_download(osf_id, tmpdir)
  )
  folder <- paste0(osf_id, "_1")
  expect_equal(dl2$folder, folder)
  f2 <- file.path(tmpdir, folder)
  expect_true(dir.exists(f2))

  unlink(f, recursive = TRUE)
  unlink(f2, recursive = TRUE)

  # error in one ID
  osf_id <- c("yuck", "6nt4v")
  expect_warning(dl3 <- osf_file_download(osf_id, tmpdir), "yuck")
  expect_equal(dl$name, dl3$name)
  f3 <- file.path(tmpdir, "6nt4v")
  unlink(f3, recursive = TRUE)
})

test_that("too small max_file_size", {
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
  f <- file.path(tmpdir2, osf_id)
  expect_true(dir.exists(f))
  expect_equal(list.files(f), character(0))
})

test_that("too small max_download_size", {
  osf_id <- "6nt4v"
  tmpdir <- withr::local_tempdir()
  op <- capture_messages(
    dl <- osf_file_download(osf_id, tmpdir,
                            max_download_size = .0001)
  )
  expect_equal(nrow(dl), 1)
  expect_equal(dl$folder, osf_id)
  expect_true(nchar(dl$osf_id) %in% c(5, 24))
  expect_equal(dl$downloaded, FALSE)
  f <- file.path(tmpdir, osf_id)
  expect_true(dir.exists(f))
  expect_equal(list.files(f), character(0))
})

test_that("truncate", {
  osf_id <- "j3gcx"
  node_name <- "Raw Data"
  tmpdir <- withr::local_tempdir()
  expect_warning(op <- capture_messages(
    dl <- osf_file_download(osf_id, tmpdir,
                            max_folder_length = 3)
  ), "truncated")
  f <- file.path(tmpdir, osf_id, node_name, "nes")
  expect_true(dir.exists(f))
  f <- file.path(tmpdir, osf_id, node_name, "data.xlsx")
  expect_true(file.exists(f))
  exp_paths <- c("README",
                 "data.xlsx",
                 "nes/README",
                 "nes/test-1.txt",
                 "nes/nes/test-2.txt",
                 "nes/nes/nes/test-3.txt",
                 "nes/nes/nes/nes/test-4.txt") |>
    paste0(node_name, "/", x = _)
  expect_setequal(dl$path, exp_paths)
})

test_that("multiple osf_ids", {
  osf_id <- c("6nt4v", "j3gcx")
  tmpdir <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, tmpdir)
  node_name <- c("Processed Data", "Raw Data")
  expect_equal(dl$folder, rep(osf_id, c(1, 7)))
  f <- file.path(tmpdir, osf_id, node_name)
  expect_true(dir.exists(f) |> all())
  expect_true(file.path(f[[1]], "processed-data.csv") |> file.exists())
  expect_true(file.path(f[[2]], "nest-1/README") |> file.exists())
})

test_that("Waterbutler ID for folder", {
  osf_id <- "https://files.de-1.osf.io/v1/resources/j3gcx/providers/osfstorage/685a46eb8c103f8ab307047f/?zip="
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  expect_true(all(dl$folder == "685a46eb8c103f8ab307047f"))
  f <- file.path(download_to, "685a46eb8c103f8ab307047f", "nest-1")
  expect_true(dir.exists(f))
  expect_true(file.path(f, "nest-2") |> dir.exists())
  expect_true(file.path(f, "README") |> file.exists())
})

test_that("osf_file_download github", {
  osf_id <- "mc45x"
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  node_name <- "Testing"
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, node_name, "DESCRIPTION") |>
                file.exists())
  expect_true(file.path(f, node_name, "README.md") |>
                file.exists())

  # osf_id <- "https://osf.io/mc45x/files/163c0afa-8ea9-4fb1-a621-951278d27d20?view_only="
})

test_that("osf_file_download long nested", {
  osf_id <- "j3gcx" # raw data - nesting and duplicates

  # nested folders
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to)
  node_name <- "Raw Data"
  expect_true("Raw Data/nest-1/nest-2/nest-3/nest-4/test-4.txt" %in% dl$path)
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, node_name, "README") |> file.exists())
  expect_true(file.path(f, node_name, "nest-1") |> dir.exists())
})

test_that("osf_file_download long unnested", {
  osf_id <- "j3gcx"

  # unnested with duplicate file names
  download_to <- withr::local_tempdir()
  dl <- osf_file_download(osf_id, download_to,
                          ignore_folder_structure = TRUE)
  expect_true("test-4.txt" %in% dl$path)
  f <- file.path(download_to, osf_id)
  expect_true(dir.exists(f))
  # duplicate READMEs get parent ID appended — check structurally
  readme_files <- grep("^README", list.files(f), value = TRUE)
  expect_equal(length(readme_files), 2)
  expect_true("test-4.txt" %in% list.files(f))
  expect_false(file.path(f, "nest-1") |> dir.exists())
})

test_that("osf_file_download ignore_folder_structure", {
  # https://github.com/scienceverse/metacheck/issues/100
  osf_id <- c("mjrpy")

  download_to <- withr::local_tempdir()
  x <- osf_file_download(osf_id = osf_id,
                         download_to = download_to,
                         ignore_folder_structure = TRUE
                         )

  destdir <- file.path(download_to, osf_id)

  f <- list.files(destdir)
  expect_true("S1_mjrpy.pdf" %in% f)
  expect_true("S1_twm2a.pdf" %in% f)
})

test_that("osf_file_download issue 99", {
  # https://github.com/scienceverse/metacheck/issues/99
  osf_id <- c("msfcn")
  download_to <- withr::local_tempdir()
  x <- osf_file_download(osf_id, download_to)
  destdir <- file.path(download_to, osf_id)

  f <- list.files(destdir, recursive = TRUE)
  expect_equal(length(f), 3)
})

test_that("osf_file_download registrations", {
  # https://github.com/scienceverse/metacheck/issues/249

  #skip_if_quick()

  osf_id <- "jqkg7"
  contents <- osf_retrieve(osf_id, recursive = TRUE)
  expect_contains(contents$kind, c("folder", "file"))
})

httptest2::stop_mocking()
#httptest2::stop_capturing()
