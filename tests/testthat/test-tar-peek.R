# Listing plain .tar archives (#487). The archives are built here: with
# utils::tar(), and by hand for the pax and GNU long-name forms that R's own
# tar does not write.

# One 512-byte tar header.
tar_header <- function(name, size, type = "0", ustar = TRUE, prefix = "") {
  h <- raw(512)
  put <- function(at, s) {
    b <- charToRaw(s)
    h[at:(at + length(b) - 1L)] <<- b
  }
  put(1, name)
  put(101, "0000644"); put(109, "0000000"); put(117, "0000000")
  put(125, sprintf("%011o", as.integer(size))); put(137, sprintf("%011o", 0L))
  put(157, type)
  if (ustar) { put(258, "ustar"); put(264, "00") } else put(258, "ustar  ")
  if (nzchar(prefix)) put(346, prefix)
  h[149:156] <- charToRaw("        ")
  put(149, sprintf("%06o", sum(as.integer(h))))
  h[155] <- as.raw(0)
  h
}
tar_pad <- function(b) c(b, raw((512 - length(b) %% 512) %% 512))
tar_entry <- function(name, content, type = "0", ...) {
  content <- if (is.character(content)) charToRaw(content) else content
  c(tar_header(name, length(content), type, ...), tar_pad(content))
}
# A pax record states its own length, digits included.
pax_record <- function(key, value) {
  body <- paste0(" ", key, "=", value, "\n")
  n <- nchar(body, "bytes")
  len <- n + nchar(n)
  if (nchar(len) > nchar(n)) len <- n + nchar(len)
  paste0(len, body)
}
write_tar <- function(path, ...) {
  writeBin(c(..., raw(1024)), path)
  path
}

# A tar made by utils::tar(): nested folders, an empty file, and a path too
# long for the 100-byte name field.
make_r_tar <- function() {
  src <- withr::local_tempdir(.local_envir = parent.frame())
  long <- file.path("study", strrep("a", 60), paste0(strrep("b", 60), ".csv"))
  dir.create(file.path(src, "study", "data"), recursive = TRUE)
  dir.create(file.path(src, dirname(long)), recursive = TRUE)
  writeLines("# Study\nRead me first.", file.path(src, "study", "README.md"))
  writeLines(c("id,score", "1,3", "2,5"), file.path(src, "study", "data", "scores.csv"))
  file.create(file.path(src, "study", "empty.txt"))
  writeLines("x", file.path(src, long))
  path <- withr::local_tempfile(fileext = ".tar", .local_envir = parent.frame())
  # utils::tar() warns that the long path is "not portable"; it is there on
  # purpose.
  withr::with_dir(src, suppressWarnings(utils::tar(path, files = "study", tar = "internal")))
  list(path = path, long = long, src = src)
}

test_that(".tar_list_file lists a tar made by utils::tar()", {
  fx <- make_r_tar()
  l <- .tar_list_file(fx$path)
  files <- list.files(fx$src, recursive = TRUE)
  expect_setequal(l$name, files)
  expect_equal(unname(l$size[match(files, l$name)]),
               unname(file.size(file.path(fx$src, files))))
  expect_true(fx$long %in% l$name)
  expect_equal(l$size[l$name == "study/empty.txt"], 0)
})

test_that(".tar_list_file reads pax, GNU long-name and ustar-prefix names", {
  long <- paste0("deposit/", strrep("x", 120), "/README.md")
  pax <- pax_record("path", long)
  gnu_long <- paste0("deposit/", strrep("y", 110), "/data.csv")
  p <- write_tar(
    withr::local_tempfile(fileext = ".tar"),
    tar_entry("pax_global_header", pax_record("comment", "made by git archive"), type = "g"),
    tar_entry("deposit/", raw(0), type = "5"),
    tar_entry("PaxHeaders/README.md", pax, type = "x"),
    tar_entry("README.md", "# Deposit\n"),
    tar_entry("././@LongLink", c(charToRaw(gnu_long), as.raw(0)), type = "L", ustar = FALSE),
    tar_entry(substr(gnu_long, 1, 100), "a,b\n1,2\n", ustar = FALSE),
    tar_entry("scores.csv", "x\n1\n", prefix = "deposit/sub"),
    tar_entry("./deposit/notes.txt", "notes")
  )
  l <- .tar_list_file(p)
  expect_equal(l$name, c(long, gnu_long, "deposit/sub/scores.csv", "deposit/notes.txt"))
  expect_equal(l$size, c(10, 8, 4, 5))
  # the stored name is kept for extraction
  expect_equal(l$member[[4]], "./deposit/notes.txt")
})

test_that(".tar_list_file skips a binary pax record, such as a Mac's extended attributes", {
  # tar on a Mac stores a file's extended attributes as pax records whose
  # value is binary, with null bytes: rawToChar() stopped on them ("embedded
  # nul in string"), and with it data_check (PsyArXiv preprint 4z75t_v1)
  value <- as.raw(c(0x01, 0x00, 0x00, 0xb1, 0xa4, 0x7f, 0x3d))
  body <- c(charToRaw(" SCHILY.xattr.com.apple.provenance="), value, charToRaw("\n"))
  n <- length(body) + 2L                      # two digits for the length itself
  xattr <- c(charToRaw(as.character(n)), body)
  p <- write_tar(
    withr::local_tempfile(fileext = ".tar"),
    tar_entry("PaxHeaders/data.csv", c(xattr, charToRaw(pax_record("path", "deposit/data.csv"))),
              type = "x"),
    tar_entry("data.csv", "a,b\n1,2\n")
  )
  l <- .tar_list_file(p)
  expect_equal(l$name, "deposit/data.csv")
  expect_equal(l$size, 8)
})

test_that(".tar_list_file returns NULL for a file that is not a readable tar", {
  p <- withr::local_tempfile(fileext = ".tar")
  writeLines(rep("not a tar archive", 100), p)
  expect_null(.tar_list_file(p))

  # a damaged second header
  good <- tar_entry("a.txt", "hello")
  bad <- tar_entry("b.txt", "world")
  bad[10] <- as.raw(0x7A)
  write_tar(p, good, bad)
  expect_null(.tar_list_file(p))
})

test_that(".tar_peek lists a remote tar with one download and keeps the copy", local({
  fx <- make_r_tar()
  url <- "https://example.org/files/archive-test-1.tar"
  withr::defer({
    suppressWarnings(rm(list = url, envir = metacheck:::.tar_peek_cache))
    suppressWarnings(rm(list = url, envir = metacheck:::.tar_peek_copies))
  })
  calls <- character(0)
  testthat::local_mocked_bindings(.download_one = function(url, dest, ...) {
    calls <<- c(calls, url)
    file.copy(fx$path, dest, overwrite = TRUE)
    NA_character_
  })
  size <- file.size(fx$path)

  # size unknown, or over the limit: nothing is downloaded
  expect_null(.tar_peek(url))
  expect_null(.tar_peek(url, size = size, max_bytes = size - 1))
  expect_length(calls, 0)

  l <- .tar_peek(url, size = size)
  expect_true("study/README.md" %in% l$name)
  expect_equal(calls, url)
  copy <- get(url, envir = metacheck:::.tar_peek_copies)
  expect_true(file.exists(copy))

  # a second listing this session needs no request
  expect_identical(.tar_peek(url, size = size), l)
  expect_length(calls, 1)

  # a file on disk is read without a request
  expect_true("study/README.md" %in% .tar_peek(fx$path)$name)
  expect_length(calls, 1)
}))

test_that(".tar_peek does not list a download shorter than the listed size", local({
  fx <- make_r_tar()
  url <- "https://example.org/files/archive-test-2.tar"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.tar_peek_cache)))
  testthat::local_mocked_bindings(.download_one = function(url, dest, ...) {
    writeBin(readBin(fx$path, "raw", 1024), dest)
    NA_character_
  })
  expect_null(.tar_peek(url, size = file.size(fx$path)))
  expect_false(exists(url, envir = metacheck:::.tar_peek_copies, inherits = FALSE))
}))

test_that(".tar_fetch_members takes members from the session copy, a local tar or one download", local({
  fx <- make_r_tar()
  url <- "https://example.org/files/archive-test-3.tar"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.tar_peek_copies)))
  calls <- 0
  testthat::local_mocked_bindings(.download_one = function(url, dest, ...) {
    calls <<- calls + 1
    file.copy(fx$path, dest, overwrite = TRUE)
    NA_character_
  })
  want <- c("study/README.md", "study/data/scores.csv")

  # from a tar on disk
  dest <- withr::local_tempdir()
  got <- .tar_fetch_members(fx$path, want, dest)
  expect_true(all(got$ok))
  expect_equal(readLines(got$path[got$name == "study/README.md"])[[2]], "Read me first.")

  # from the copy .tar_peek() kept: no request
  assign(url, fx$path, envir = metacheck:::.tar_peek_copies)
  got <- .tar_fetch_members(url, want, withr::local_tempdir())
  expect_true(all(got$ok))
  expect_equal(calls, 0)

  # in a new session: one download, kept at archive_path for a restarted run
  rm(list = url, envir = metacheck:::.tar_peek_copies)
  arc <- file.path(withr::local_tempdir(), "archive.tar")
  got <- .tar_fetch_members(url, want, withr::local_tempdir(), archive_path = arc,
                            size = file.size(fx$path))
  expect_true(all(got$ok))
  expect_equal(calls, 1)
  got <- .tar_fetch_members(url, want, withr::local_tempdir(), archive_path = arc)
  expect_true(all(got$ok))
  expect_equal(calls, 1)

  # an archive over the download budget is not downloaded
  got <- .tar_fetch_members(url, want, withr::local_tempdir(),
                            size = file.size(fx$path), max_whole_bytes = 10)
  expect_false(any(got$ok))
  expect_equal(unique(got$error), "the archive is larger than the download budget")
  expect_equal(calls, 1)
}))

test_that(".tar_fetch_members refuses a member whose path leaves the folder", {
  p <- write_tar(withr::local_tempfile(fileext = ".tar"),
                 tar_entry("../evil.txt", "x"), tar_entry("ok.txt", "fine"))
  got <- .tar_fetch_members(p, c("../evil.txt", "ok.txt"), withr::local_tempdir())
  expect_equal(got$ok, c(FALSE, TRUE))
  expect_match(got$error[[1]], "path traversal")
})

test_that("download_repo_files fetches a tar member through .tar_fetch_members", local({
  fx <- make_r_tar()
  repo <- paste0("https://example.org/repo-test-",
                 paste(sample(c(letters, 0:9), 12, TRUE), collapse = ""))
  url <- paste0(repo, "/files/123/download")
  withr::defer(unlink(metacheck:::.repo_cache_subdir(repo), recursive = TRUE))
  calls <- 0
  testthat::local_mocked_bindings(.download_one = function(url, dest, ...) {
    calls <<- calls + 1
    file.copy(fx$path, dest, overwrite = TRUE)
    NA_character_
  })
  files <- data.frame(
    repo_url = repo, file_name = "scores.csv",
    file_path = "archive.tar/study/data/scores.csv",
    file_url = NA_character_, file_size = 15, file_location = NA_character_,
    archive_url = url, archive_member = "study/data/scores.csv",
    archive_format = "tar", archive_size = file.size(fx$path), paper_id = "p.1"
  )
  dl <- download_repo_files(files)
  expect_false(is.na(dl$file_location[[1]]))
  expect_equal(readLines(dl$file_location[[1]])[[1]], "id,score")
  expect_equal(calls, 1)
}))

test_that("repo_check lists the files inside a plain .tar (#487)", local({
  fx <- make_r_tar()
  size <- file.size(fx$path)
  url <- "https://osf.io/download/tartest1/"
  withr::defer({
    suppressWarnings(rm(list = url, envir = metacheck:::.tar_peek_cache))
    suppressWarnings(rm(list = url, envir = metacheck:::.tar_peek_copies))
  })
  calls <- 0
  testthat::local_mocked_bindings(
    online = \(...) TRUE,
    osf_info = function(osf_url, ...) data.frame(
      osf_url = osf_url, osf_type = c("nodes", "files"), parent = NA_character_,
      kind = c(NA, "file"), public = TRUE, name = c(NA, "archive.tar"),
      path = c(NA, "/archive.tar"), download_url = c(NA, url),
      size = c(NA, size), filetype = c(NA, "archive"), provider = c(NA, "osfstorage")),
    .download_one = function(url, dest, ...) {
      calls <<- calls + 1
      file.copy(fx$path, dest, overwrite = TRUE)
      NA_character_
    }
  )
  paper <- test_paper(text = "Data: https://osf.io/tartest")
  mo <- module_run(paper, "repo_check")
  tb <- mo$table
  expect_true("archive.tar/study/README.md" %in% tb$file_path)
  expect_false("archive.tar" %in% tb$file_name)
  expect_true(all(tb$archive_format == "tar"))
  expect_equal(calls, 1)
  # the archive advice still names the tar
  expect_true(any(grepl("not ZIP archives: archive.tar", mo$report, fixed = TRUE)))
}))

test_that("repo_check lists the README inside the Zenodo tar of #487", {
  skip_if_quick()
  skip_if_offline("zenodo.org")
  paper <- test_paper(text = "Data: https://doi.org/10.5281/zenodo.4037178")
  mo <- suppressWarnings(module_run(paper, "repo_check"))
  expect_true("C_fire_invasives-1.0.0.tar/C_fire_invasives-1.0.0/README.md" %in%
                mo$table$file_path)
})
