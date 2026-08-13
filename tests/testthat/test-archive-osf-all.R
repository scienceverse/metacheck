# mode = "all" downloads a whole repository without listing its files: it walks
# the component tree and takes one archive per node. These tests use 6nt4v (a
# single node, one file, no components) and pngda (9 nodes, two providers,
# nested folders) -- both small enough to fetch live, unlike the projects the
# mode is built for.

test_that("osf_file_download accepts the mode names", {
  # "all" is the default: this function exists to retrieve a repository in
  # full, and that is the fast route.
  expect_equal(eval(formals(osf_file_download)$mode)[[1]], "all")
  expect_setequal(eval(formals(osf_file_download)$mode),
                  c("all", "select", "files", "zip"))
})


test_that("mode = 'files' is still accepted as a name for 'select'", {
  skip_if_quick() # downloads from OSF for real
  skip_if_not(online("api.osf.io"))
  skip_on_cran()

  # "files" named a transport rather than an intent, but callers used it, so
  # it keeps working.
  download_to <- withr::local_tempdir()
  dl <- suppressWarnings(
    osf_file_download("6nt4v", download_to, mode = "files", metadata = FALSE))

  # the select-mode table has one row per FILE, with the verification columns
  expect_true(all(c("size_on_disk", "attempted") %in% names(dl)))
  expect_equal(nrow(dl), 1)
})


test_that(".osf_walk_nodes finds every component and its title", {
  skip_if_quick() # walks the real OSF node tree
  skip_if_not(online("api.osf.io"))
  skip_on_cran()

  nodes <- .osf_walk_nodes("pngda")

  expect_s3_class(nodes, "data.frame")
  expect_true(all(c("osf_id", "title") %in% names(nodes)))

  # the root is included, first
  expect_equal(nodes$osf_id[[1]], "pngda")
  expect_equal(nodes$title[[1]], "Papercheck Test")

  # and its components, at any depth: 6nt4v sits two levels down, under ckjef
  expect_true("ckjef" %in% nodes$osf_id)
  expect_true("6nt4v" %in% nodes$osf_id)
  expect_false(any(duplicated(nodes$osf_id)))

  # titles are what make readable folder names; IDs alone would not be
  expect_true(any(nodes$title == "Raw Data", na.rm = TRUE))
})


test_that("mode = 'all' downloads a whole single-node project", {
  skip_if_not(online("api.osf.io"))
  skip_on_cran()
  skip_if_quick()

  # Not mocked: the archive is streamed to a file path, and httptest2 does not
  # replay req_perform(path = ) (the same limitation the zip tests work around
  # with hand-built stubs). Recording the fixture is not enough, so this test
  # fetches for real and is skipped in quick mode.
  download_to <- withr::local_tempdir()
  dl <- suppressWarnings(
    osf_file_download("6nt4v", download_to, mode = "all", metadata = FALSE))

  # one row per NODE, not per file
  expect_s3_class(dl, "data.frame")
  expect_true(all(c("folder", "osf_project", "title", "files", "bytes",
                    "download_path", "downloaded") %in% names(dl)))
  expect_equal(nrow(dl), 1)
  expect_true(dl$downloaded[[1]])
  expect_equal(dl$files[[1]], 1)

  # the folder is named after the component, with its ID appended so two
  # components sharing a title cannot collide
  expect_match(dl$folder[[1]], "6nt4v$")
  expect_true(dir.exists(dl$download_path[[1]]))
  expect_equal(length(list.files(dl$download_path[[1]], recursive = TRUE)), 1)
})


test_that("mode = 'all' retrieves as much as mode = 'select'", {
  skip_if_not(online("api.osf.io"))
  skip_on_cran()
  skip_if_quick()

  # pngda is the hard case: 9 nodes, 57 files, and 29 of those on a linked
  # GitHub add-on that no OSF archive can contain. "all" has to fetch those
  # individually or it would silently return half the project.
  osf_cache_clear()
  a <- suppressWarnings(osf_file_download(
    "pngda", withr::local_tempdir(), mode = "all", metadata = FALSE))

  osf_cache_clear()
  s <- suppressWarnings(osf_file_download(
    "pngda", withr::local_tempdir(), mode = "select", metadata = FALSE))

  expect_equal(sum(a$files), sum(s$downloaded))

  # every node reports the number of files actually in its folder, counted
  # once after both the archive and the add-on files have been written
  for (i in which(a$files > 0)) {
    on_disk <- list.files(a$download_path[[i]], recursive = TRUE)
    on_disk <- on_disk[!dir.exists(file.path(a$download_path[[i]], on_disk))]
    expect_equal(length(on_disk), a$files[[i]])
  }
})


test_that("mode = 'all' names folders after component titles", {
  skip_if_not(online("api.osf.io"))
  skip_on_cran()
  skip_if_quick()

  download_to <- withr::local_tempdir()
  osf_cache_clear()
  dl <- suppressWarnings(
    osf_file_download("pngda", download_to, mode = "all", metadata = FALSE))

  # "Raw Data" becomes "Raw_Data_j3gcx": readable, sanitised for the file
  # system, and traceable back to the node it came from
  expect_true("Raw_Data_j3gcx" %in% dl$folder)
  expect_true("Papercheck_Test_pngda" %in% dl$folder)
  expect_false(any(duplicated(dl$folder)))
})
