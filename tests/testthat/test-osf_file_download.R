verbose(FALSE)
# options(metacheck.osf.api = "https://api.osf.io/v2/")
# osf_delay(0)

# skip if requires OSF API
osf_skip <- function() {
  skip("Skip OSF") # skips all tests that require API

  # skips tests if contraindicated
  skip_if_offline()
  skip_on_cran()
  skip_on_covr()
  skip_if_not(osf_api_check() == "ok")
}

test_that("osf_file_download", {
  expect_warning(x <- osf_file_download("notanid"))
  expect_null(x)

  osf_skip()

  osf_id <- "6nt4v" # processed data - 1 file
  node_name <- "Processed Data"

  op <- capture_messages(
    dl <- osf_file_download(osf_id, tempdir())
  )
  f <- file.path(tempdir(), osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, node_name, "processed-data.csv") |> file.exists())
  expect_equal(dl$folder, osf_id)
  expect_equal(dl$downloaded, TRUE)
  expect_equal(dl$osf_id, "6846ed6a29684b023953943e")

  ## second download with existing file
  op <- capture_messages(
    dl2 <- osf_file_download(osf_id, tempdir())
  )
  folder <- paste0(osf_id, "_1")
  expect_equal(dl2$folder, folder)
  f2 <- file.path(tempdir(), folder)
  expect_true(dir.exists(f2))

  unlink(f, recursive = TRUE)
  unlink(f2, recursive = TRUE)

  # error in one ID
  osf_id <- c("yuck", "6nt4v")
  expect_warning(dl3 <- osf_file_download(osf_id, tempdir()), "yuck")
  expect_equal(dl$name, dl3$name)
  f3 <- file.path(tempdir(), "6nt4v")
  unlink(f3, recursive = TRUE)

  # too small max_file_size
  osf_id <- "6nt4v"
  op <- capture_messages(
    dl <- osf_file_download(osf_id, tempdir(),
                            max_file_size = .0001)
  )
  expect_equal(nrow(dl), 1)
  expect_equal(dl$folder, osf_id)
  expect_equal(dl$osf_id, "6846ed6a29684b023953943e")
  expect_equal(dl$downloaded, FALSE)
  f <- file.path(tempdir(), osf_id)
  expect_true(dir.exists(f))
  expect_equal(list.files(f), character(0))
  unlink(f, recursive = TRUE)

  # too small max_download_size
  op <- capture_messages(
    dl <- osf_file_download(osf_id, tempdir(),
                            max_download_size = .0001)
  )
  expect_equal(nrow(dl), 1)
  expect_equal(dl$folder, osf_id)
  expect_equal(dl$osf_id, "6846ed6a29684b023953943e")
  expect_equal(dl$downloaded, FALSE)
  f <- file.path(tempdir(), osf_id)
  expect_true(dir.exists(f))
  expect_equal(list.files(f), character(0))
  unlink(f, recursive = TRUE)

  ## truncate
  osf_id <- "j3gcx"
  node_name <- "Raw Data"
  expect_warning(op <- capture_messages(
    dl <- osf_file_download(osf_id, tempdir(),
                            max_folder_length = 3)
  ), "truncated")
  f <- file.path(tempdir(), osf_id, node_name, "nes")
  expect_true(dir.exists(f))
  f <- file.path(tempdir(), osf_id, node_name, "data.xlsx")
  expect_true(file.exists(f))
  exp_paths <- c("README", "data.xlsx",
                 "nes/README",
                 "nes/test-1.txt",
                 "nes/nes/test-2.txt",
                 "nes/nes/nes/test-3.txt",
                 "nes/nes/nes/nes/test-4.txt") |>
    paste0(node_name, "/", x = _)
  expect_equal(dl$path, exp_paths)
  f <- file.path(tempdir(), osf_id)
  unlink(f, recursive = TRUE)

  ## multiple osf_ids
  osf_id <- c("6nt4v", "j3gcx")
  dl <- osf_file_download(osf_id, tempdir())
  node_name <- c("Processed Data", "Raw Data")
  expect_equal(dl$folder, rep(osf_id, c(1, 7)))
  f <- file.path(tempdir(), osf_id, node_name)
  expect_true(dir.exists(f) |> all())
  expect_true(file.path(f[[1]], "processed-data.csv") |> file.exists())
  expect_true(file.path(f[[2]], "nest-1/README") |> file.exists())
  f <- file.path(tempdir(), osf_id)
  unlink(f, recursive = TRUE)

  ## Waterbutler ID for folder
  osf_id <- "https://files.de-1.osf.io/v1/resources/j3gcx/providers/osfstorage/685a46eb8c103f8ab307047f/?zip="
  dl <- osf_file_download(osf_id, tempdir())
  expect_true(all(dl$folder == "685a46eb8c103f8ab307047f"))
  f <- file.path(tempdir(), "685a46eb8c103f8ab307047f", "nest-1")
  expect_true(dir.exists(f))
  expect_true(file.path(f, "nest-2") |> dir.exists())
  expect_true(file.path(f, "README") |> file.exists())
  f <- file.path(tempdir(), "685a46eb8c103f8ab307047f")
  unlink(f, recursive = TRUE)
})

test_that("osf_file_download github", {
  osf_skip()

  osf_id <- "mc45x"
  dl <- osf_file_download(osf_id, tempdir())
  node_name <- "Testing"
  f <- file.path(tempdir(), osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, node_name, "DESCRIPTION") |>
                file.exists())
  expect_true(file.path(f, node_name, "README.md") |>
                file.exists())
  unlink(f, recursive = TRUE)

  # osf_id <- "https://osf.io/mc45x/files/163c0afa-8ea9-4fb1-a621-951278d27d20?view_only="
})

test_that("osf_file_download long", {
  osf_skip()

  osf_id <- "j3gcx" # raw data - nesting and duplicates

  # nested folders
  dl <- osf_file_download(osf_id, tempdir())
  node_name <- "Raw Data"
  expect_true("Raw Data/nest-1/nest-2/nest-3/nest-4/test-4.txt" %in% dl$path)
  f <- file.path(tempdir(), osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, node_name, "README") |> file.exists())
  expect_true(file.path(f, node_name, "nest-1") |> dir.exists())
  unlink(f, recursive = TRUE)

  # unnested with duplicate file names
  dl <- osf_file_download(osf_id, tempdir(),
                          ignore_folder_structure = TRUE)
  expect_true("test-4.txt" %in% dl$path)
  f <- file.path(tempdir(), osf_id)
  expect_true(dir.exists(f))
  expect_true(file.path(f, "README_685a46eb8c103f8ab307047f") |> file.exists())
  expect_true(file.path(f, "README_j3gcx") |> file.exists())
  expect_true(file.path(f, "test-4.txt") |> file.exists())
  expect_false(file.path(f, "nest-1") |> dir.exists())
  unlink(f, recursive = TRUE)
})

test_that("osf_file_download ignore_folder_structure", {
  osf_skip()

  # https://github.com/scienceverse/metacheck/issues/100
  osf_id <- c("mjrpy")

  x <- osf_file_download(osf_id = osf_id,
                         download_to = tempdir(),
                         ignore_folder_structure = TRUE
                         )

  destdir <- tempdir() |> file.path(osf_id)

  f <- list.files(destdir)
  expect_true("S1_mjrpy.pdf" %in% f)
  expect_true("S1_twm2a.pdf" %in% f)

  unlink(destdir, recursive = TRUE)
})

test_that("osf_file_download issue 99", {
  osf_skip()

  # https://github.com/scienceverse/metacheck/issues/99
  osf_id <- c("msfcn")
  x <- osf_file_download(osf_id, tempdir())
  destdir <- tempdir() |> file.path(osf_id)

  f <- list.files(destdir, recursive = TRUE)
  expect_equal(length(f), 3)

  unlink(destdir, recursive = TRUE)
})

verbose(TRUE)
