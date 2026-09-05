# Tests for the ZIP central-directory parser and the download decision. The
# range-fetch (zip_peek) needs a live HTTP server, so it is covered by the OSF
# acceptance run; here we test the pure parser on a real local zip's bytes and
# the classification decision, which need no network.

test_that(".parse_zip_central_dir reads names and sizes from a real zip tail", {
  # Build a small zip with known contents.
  d <- withr::local_tempdir()
  writeLines(rep("x", 100), file.path(d, "data.csv"))
  writeLines("img", file.path(d, "stim.png"))
  zip <- file.path(d, "test.zip")
  withr::with_dir(d, utils::zip("test.zip", c("data.csv", "stim.png"),
                                flags = "-q"))
  skip_if_not(file.exists(zip), "zip utility unavailable")

  raw <- readBin(zip, "raw", file.size(zip))
  cd <- metacheck:::.parse_zip_central_dir(raw)
  expect_false(is.null(cd))
  expect_true(all(c("data.csv", "stim.png") %in% basename(cd$name)))
  # uncompressed size of data.csv is ~200 bytes (100 lines of "x\n")
  expect_true(cd$size[basename(cd$name) == "data.csv"] > 0)
})

test_that(".parse_zip_central_dir also reports method, csize, offset and crc", {
  d <- withr::local_tempdir()
  writeLines(rep("id,x", 200), file.path(d, "data.csv"))   # compressible
  writeLines("hello", file.path(d, "notes.txt"))
  zip <- file.path(d, "test.zip")
  withr::with_dir(d, utils::zip("test.zip", c("data.csv", "notes.txt"),
                                flags = "-q"))
  skip_if_not(file.exists(zip), "zip utility unavailable")

  raw <- readBin(zip, "raw", file.size(zip))
  cd <- metacheck:::.parse_zip_central_dir(raw)

  # name and size stay the first two columns: zip_decision() and repo_check()
  # use only those, so the fetch fields must be purely additive.
  expect_equal(names(cd)[1:2], c("name", "size"))
  expect_true(all(c("method", "csize", "offset", "crc") %in% names(cd)))

  # The first member's local header starts at the very beginning of the archive.
  expect_equal(min(cd$offset), 0)
  expect_true(all(cd$method %in% c(0, 8)))   # stored or deflate

  # The stored CRC must match the file on disk, which proves the field is being
  # read from the right offset rather than merely being present.
  for (i in seq_len(nrow(cd))) {
    f <- file.path(d, cd$name[i])
    expect_equal(metacheck:::.crc32(readBin(f, "raw", file.size(f))), cd$crc[i])
  }
})

test_that(".crc32 matches the standard check value", {
  # The conventional CRC32 test vector: CRC32("123456789") == 0xcbf43926.
  expect_equal(metacheck:::.crc32(charToRaw("123456789")), 3421780262)
  expect_equal(metacheck:::.crc32(raw(0)), 0)
})

test_that(".zip_crc_ok distinguishes a match, a mismatch, and no check", {
  b <- charToRaw("123456789")
  expect_true(metacheck:::.zip_crc_ok(b, 3421780262))
  expect_false(metacheck:::.zip_crc_ok(b, 12345))
  # An absent CRC is "not checked", not "failed" -- .zip_member_fetch() rejects
  # only on FALSE, so returning NA here must not discard a good download.
  expect_true(is.na(metacheck:::.zip_crc_ok(b, NA_real_)))
})

test_that(".zip_crc_ok accepts CRCs above the R integer maximum", {
  # CRC32 is unsigned 32-bit, so roughly half of all values exceed
  # .Machine$integer.max. Parsing one into an R integer yields NA and makes a
  # correct file look corrupt, which would silently reject half of all
  # downloads, so this case is tested explicitly.
  big <- charToRaw("123456789")            # CRC 3421780262 > 2147483647
  expect_gt(metacheck:::.crc32(big), .Machine$integer.max)
  expect_true(metacheck:::.zip_crc_ok(big, metacheck:::.crc32(big)))
})

test_that(".zip_inflate_member passes stored members through and rejects others", {
  # Method 0 is stored: the bytes are the file, so no decompression and no
  # dependency on the zip package.
  expect_identical(metacheck:::.zip_inflate_member(as.raw(1:5), 0), as.raw(1:5))
  # Only deflate (8) is supported; anything else is refused rather than guessed.
  expect_null(metacheck:::.zip_inflate_member(as.raw(1:5), 12))
})

test_that(".zip_inflate_member recovers a member larger than 32768 bytes", {
  # Regression test for issue #384: zip::inflate()'s documented "resize the
  # output buffer multiple times" behaviour when size = NULL does not happen
  # in practice, so any member over its default 32768-byte buffer came back
  # silently truncated at exactly 32768 bytes -- no error, no warning. This
  # built a real local zip whose one member compresses to ~95KB uncompressed,
  # well past that threshold, and confirms the size = NULL path truncates
  # while size = entry$size (this function's second argument) recovers the
  # complete, correct member.
  d <- withr::local_tempdir()
  content <- paste0("line ", 1:3000, " ", strrep("x", 20))
  writeLines(content, file.path(d, "big.R"))
  zipfile <- file.path(d, "test.zip")
  withr::with_dir(d, utils::zip("test.zip", "big.R", flags = "-q"))
  skip_if_not(file.exists(zipfile), "zip utility unavailable")

  raw <- readBin(zipfile, "raw", file.size(zipfile))
  cd <- metacheck:::.parse_zip_central_dir(raw)
  entry <- cd[cd$name == "big.R", , drop = FALSE]
  expect_gt(entry$size, 32768)   # the test is only meaningful past the buffer

  lh <- raw[(entry$offset + 1):(entry$offset + 30)]
  name_len <- metacheck:::.le_int(lh, 27, 2)
  extra_len <- metacheck:::.le_int(lh, 29, 2)
  data_start <- entry$offset + 30 + name_len + extra_len
  comp <- raw[(data_start + 1):(data_start + entry$csize)]

  truncated <- metacheck:::.zip_inflate_member(comp, entry$method)
  expect_equal(length(truncated), 32768)   # documents the underlying bug

  fixed <- metacheck:::.zip_inflate_member(comp, entry$method, size = entry$size)
  expect_equal(length(fixed), entry$size)
  # writeLines() uses the platform line ending (\r\n on Windows), so compare
  # after normalising rather than assuming \n -- the point of this assertion
  # is that every line survived intact and in order, not the exact byte width.
  recovered_lines <- strsplit(rawToChar(fixed), "\r?\n")[[1]]
  expect_identical(recovered_lines, content)
})

test_that(".zip_member_fetch refuses a Zip64 entry instead of using the sentinel", {
  # A >4GB archive stores 0xFFFFFFFF in the 4-byte fields, which the parser turns
  # into NA. Fetching from that offset would request a meaningless byte range.
  entry <- data.frame(name = "big.dat", size = NA_real_, method = 8,
                      csize = NA_real_, offset = NA_real_, crc = 1)
  expect_null(metacheck:::.zip_member_fetch("http://example.invalid/x.zip", entry))
})

test_that("zip_decision keeps a data zip and links a pure-asset zip", {
  # Stub zip_peek so no network: two synthetic listings.
  local_mocked_bindings(
    zip_peek = function(url, ...) {
      if (grepl("data", url))
        data.frame(name = c("study.csv", "notes.png"), size = c(100, 200))
      else
        data.frame(name = c("a.png", "b.jpg", "readme.txt"), size = c(1, 2, 3))
    }
  )
  d1 <- zip_decision("http://x/data.zip", skip_types = "materials")
  expect_true(d1$worth)                       # has study.csv (data)

  d2 <- zip_decision("http://x/stimuli.zip", skip_types = "materials")
  expect_false(d2$worth)                       # only images + a readme, no data
  expect_match(d2$reason, "link")
})

test_that("zip_decision returns NA when the peek fails", {
  local_mocked_bindings(zip_peek = function(url, ...) NULL)
  d <- zip_decision("http://x/opaque.zip")
  expect_true(is.na(d$worth))
  expect_match(d$reason, "could not peek")
})

test_that(".expand_zip keeps inner data files and drops inner materials", {
  d <- withr::local_tempdir()
  writeLines("id,x\n1,2", file.path(d, "study.csv"))   # data
  writeLines("img", file.path(d, "stim.png"))          # materials
  writeLines("notes", file.path(d, "README.txt"))      # documentation (readme)
  z <- file.path(d, "mixed.zip")
  withr::with_dir(d, utils::zip("mixed.zip",
                                c("study.csv", "stim.png", "README.txt"),
                                flags = "-q"))
  skip_if_not(file.exists(z), "zip utility unavailable")

  zip_row <- data.frame(
    repo_url = "r", file_name = "mixed.zip", file_path = "mixed.zip",
    file_url = "u", file_location = z, file_size = file.size(z),
    file_type = "archive", repo_name = "r", paper_id = "p.1",
    data_type = "unknown", doc_role = NA_character_,
    data_format = "tabular", group = NA_character_,
    stringsAsFactors = FALSE)

  rows <- metacheck:::.expand_zip(z, zip_row, skip_types = "materials")
  expect_setequal(rows$file_name, c("study.csv", "README.txt"))  # png dropped
  expect_false("stim.png" %in% rows$file_name)
  expect_equal(rows$data_type[rows$file_name == "study.csv"], "data")
  expect_equal(rows$data_type[rows$file_name == "README.txt"], "documentation")
  expect_equal(rows$doc_role[rows$file_name == "README.txt"], "readme")
  expect_true(all(file.exists(rows$file_location)))              # extracted
  # inner rows inherit the zip's repo/paper, lose their own URL
  expect_true(all(rows$paper_id == "p.1"))
  expect_true(all(is.na(rows$file_url)))
})
