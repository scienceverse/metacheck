# Tests for the ZIP central-directory parser and the download decision, on a
# real local zip's bytes, which need no network. The range requests zip_peek()
# sends are tested at the end of this file against httr2's mocked responses.

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

# Captured as plain top-level code before the two tests below replace
# zip_peek(): local_mocked_bindings()'s own restore does not reliably take
# effect in this setup (see the same note in test-repo-download.R), and the
# tests at the end of this file need the real function. It is put back
# explicitly in the namespace after them, which is where package code such as
# zip_decision() looks it up. A bare zip_peek() call inside a later test is
# still found in a different environment that keeps the stand-in, so those
# tests call metacheck:::zip_peek() instead.
.real_zip_peek <- get("zip_peek", envir = asNamespace("metacheck"), inherits = FALSE)

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
assignInNamespace("zip_peek", .real_zip_peek, ns = "metacheck")

test_that("zip_peek() reuses a same-session result instead of re-fetching", {
  # Regression test for issue #384: a single pipeline run calls zip_peek() on
  # the SAME archive URL twice (once in repo_check to list members, again in
  # download_repo_files()'s .zip_fetch_members() to fetch one) with no
  # throttle between them -- the second call is what got exposed to a burst
  # rate limit. Caching the first success removes the second network call
  # entirely rather than only making it retry-resilient.
  #
  # This exercises .zip_peek_cache directly (get/assign, the same operations
  # zip_peek() itself performs) rather than calling zip_peek() end-to-end:
  # zip_peek() needs a live HTTP server for anything beyond the cache check
  # itself (see this file's header comment), so a real network round-trip
  # would be needed to reach the "second call" behaviour this test is about.
  withr::defer(rm(list = ls(envir = metacheck:::.zip_peek_cache),
                 envir = metacheck:::.zip_peek_cache))

  url <- "http://example.invalid/same.zip"
  cached <- mget(url, envir = metacheck:::.zip_peek_cache, ifnotfound = list(NULL))[[1]]
  expect_null(cached)   # nothing cached yet for this URL

  cd <- data.frame(name = "study.csv", size = 100, method = 0, csize = 100,
                   offset = 0, crc = 12345, stringsAsFactors = FALSE)
  assign(url, cd, envir = metacheck:::.zip_peek_cache)

  reused <- mget(url, envir = metacheck:::.zip_peek_cache, ifnotfound = list(NULL))[[1]]
  expect_equal(reused, cd)   # the exact object zip_peek() would now return

  # A different URL is unaffected by the cached entry above.
  other <- mget("http://example.invalid/other.zip", envir = metacheck:::.zip_peek_cache,
               ifnotfound = list(NULL))[[1]]
  expect_null(other)
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

# ── Hosts that refuse HEAD (issue #424) ─────────────────────────────────────
# Dryad, Figshare and Harvard Dataverse redirect downloads to Amazon S3, which
# answers HEAD with 403 but a ranged GET with 206. These tests stand in for
# such a host with httr2's mocked responses, serving the bytes of a real zip.

# A small real zip's bytes, or NULL when no zip utility is available.
.test_zip_bytes <- function(d) {
  writeLines(rep("id,x", 50), file.path(d, "data.csv"))
  writeLines("hello", file.path(d, "notes.txt"))
  withr::with_dir(d, utils::zip("t.zip", c("data.csv", "notes.txt"), flags = "-q"))
  zip <- file.path(d, "t.zip")
  if (!file.exists(zip)) return(NULL)
  readBin(zip, "raw", file.size(zip))
}

# A mocked host serving `bytes`. HEAD gets `head_status`. A Range header is
# honoured with 206 and a Content-Range header, except that a suffix range
# gets `suffix_status` instead when that is not 206 (200 = range ignored and
# the whole file sent; 416 = over-long suffix refused, as GitHub does).
.s3_like_host <- function(bytes, head_status = 403L, suffix_status = 206L,
                          log = NULL) {
  total <- length(bytes)
  function(req) {
    method <- req$method %||% "GET"
    range <- req$headers$Range %||% NA_character_
    if (!is.null(log)) log$calls <- c(log$calls, paste(method, range))
    if (identical(method, "HEAD")) return(httr2::response(head_status))
    if (is.na(range)) return(httr2::response(200L, body = bytes))
    if (grepl("^bytes=-", range)) {
      if (suffix_status == 200L) return(httr2::response(200L, body = bytes))
      n <- as.numeric(sub("^bytes=-", "", range))
      if (suffix_status == 416L && n > total)
        return(httr2::response(416L, headers = list(
          `Content-Range` = sprintf("bytes */%d", total))))
      from <- max(0, total - n); to <- total - 1
    } else {
      lims <- as.numeric(strsplit(sub("^bytes=", "", range), "-")[[1]])
      from <- lims[1]; to <- min(lims[2], total - 1)
    }
    httr2::response(206L,
      headers = list(`Content-Range` = sprintf("bytes %.0f-%.0f/%d", from, to, total)),
      body = bytes[(from + 1):(to + 1)])
  }
}

test_that(".content_range_total reads the size from a Content-Range header", {
  resp <- function(cr) httr2::response(206L, headers = list(`Content-Range` = cr))
  expect_equal(metacheck:::.content_range_total(resp("bytes 0-0/2545568")), 2545568)
  expect_equal(metacheck:::.content_range_total(resp("bytes */2246")), 2246)
  expect_true(is.na(metacheck:::.content_range_total(resp("bytes 0-99/*"))))
  expect_true(is.na(metacheck:::.content_range_total(httr2::response(206L))))
})

test_that("zip_peek() lists a zip on a host that refuses HEAD", {
  d <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://s3-like.example/refuses-head.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.s3_like_host(bytes, log = log))
  cd <- metacheck:::zip_peek(url)

  expect_setequal(cd$name, c("data.csv", "notes.txt"))
  # The 403 was not retried: exactly one HEAD was sent.
  expect_equal(sum(startsWith(log$calls, "HEAD")), 1)
  expect_true(any(grepl("^GET bytes=-", log$calls)))
})

test_that("zip_peek() rejects a whole-file answer when the size is unknown", {
  # Without a size, a host that ignores the range would send the whole file,
  # which for a real archive can be gigabytes: only 206 is accepted.
  d <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://s3-like.example/ignores-range.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  httr2::local_mocked_responses(.s3_like_host(bytes, suffix_status = 200L))
  expect_null(metacheck:::zip_peek(url))
})

test_that("zip_peek() handles a 416 answer to an over-long suffix range", {
  # GitHub answers 416 when the requested tail is longer than the file, but
  # still reports the size ("bytes */2246"); the file is then fetched whole by
  # position.
  d <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://s3-like.example/short.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  httr2::local_mocked_responses(.s3_like_host(bytes, suffix_status = 416L))
  cd <- metacheck:::zip_peek(url)
  expect_setequal(cd$name, c("data.csv", "notes.txt"))
})

test_that(".zip_member_fetch() works on a host that refuses HEAD", {
  d <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://s3-like.example/member.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  httr2::local_mocked_responses(.s3_like_host(bytes))
  cd <- metacheck:::zip_peek(url)
  got <- metacheck:::.zip_member_fetch(url, cd[cd$name == "notes.txt", , drop = FALSE])
  notes <- file.path(d, "notes.txt")
  expect_equal(got, readBin(notes, "raw", file.size(notes)))
})

# ── On-disk zip-peek cache (issue #427) ─────────────────────────────────────
# Regression tests for the in-memory-only cache: a restarted R process re-peeks
# every zip it already peeked, even though a zip's contents cannot change.

test_that("zip_peek(cache = TRUE) persists a result to disk and a later call reuses it", {
  d <- withr::local_tempdir()
  withr::local_options(metacheck.zip_peek_cache.dir = d)
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://s3-like.example/persisted.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.s3_like_host(bytes, log = log))
  cd <- metacheck:::zip_peek(url, cache = TRUE)
  expect_setequal(cd$name, c("data.csv", "notes.txt"))
  expect_true(length(log$calls) > 0)   # a real network round-trip happened

  # A cache file now exists on disk for this URL.
  expect_true(metacheck:::.zip_peek_cache_has(url))

  # Clear the in-memory cache only (simulating a fresh R session), then call
  # again: the disk cache is hit, so NO further HTTP request is made.
  rm(list = url, envir = metacheck:::.zip_peek_cache)
  log$calls <- character(0)
  cd2 <- metacheck:::zip_peek(url, cache = TRUE)
  expect_equal(cd2, cd)
  expect_equal(length(log$calls), 0)
})

test_that("zip_peek(cache = FALSE) never reads or writes the on-disk cache", {
  d <- withr::local_tempdir()
  withr::local_options(metacheck.zip_peek_cache.dir = d)
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://s3-like.example/uncached.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  httr2::local_mocked_responses(.s3_like_host(bytes))
  metacheck:::zip_peek(url)   # cache defaults to FALSE
  expect_false(metacheck:::.zip_peek_cache_has(url))
})

test_that("zip_peek_cache_clear() removes every cached entry", {
  d <- withr::local_tempdir()
  withr::local_options(metacheck.zip_peek_cache.dir = d)
  metacheck:::.zip_peek_cache_put("http://x/a.zip", data.frame(name = "a", size = 1))
  metacheck:::.zip_peek_cache_put("http://x/b.zip", NULL)
  expect_true(metacheck:::.zip_peek_cache_has("http://x/a.zip"))

  n <- zip_peek_cache_clear()
  expect_equal(n, 2)
  expect_false(metacheck:::.zip_peek_cache_has("http://x/a.zip"))
})

# ── skip_on_api_limit reaching zip_peek()'s own HTTP helpers (issue #427) ───
# .wait_out_known_rate_limit() (R/repo-download.R) only skips a confirmed,
# already-known rate limit when told to -- previously zip_peek()'s helpers
# called it with no argument at all, so a caller's own skip_on_api_limit could
# never reach it except via the global option.

test_that("zip_peek(skip_on_api_limit = TRUE) gives up instead of waiting on a known rate limit", {
  # A dedicated, never-reused host: withr::defer() inside test_that() has
  # already been found (see this file's own note above .real_zip_peek) not to
  # reliably run before the NEXT test_that() starts in this setup, so a record
  # against a host any other test in this file also uses (s3-like.example)
  # could otherwise leak into one of them and force a real multi-second sleep
  # there instead of failing loudly here. Cleaned up immediately below rather
  # than deferred, for the same reason.
  host <- "s3-rate-limited-only.example"
  on.exit(suppressWarnings(rm(list = host, envir = metacheck:::.host_rate_limit_cache)),
         add = TRUE)
  # Record a long remaining wait for this host, as a real 429 response would.
  metacheck:::.host_rate_limit_record(host, 999)

  url <- paste0("https://", host, "/rate-limited.zip")
  on.exit(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)), add = TRUE)

  # No mocked response is registered: if the wait were not skipped, this would
  # either hang (Sys.sleep(999)) or error on an unmocked request. A quick NULL
  # return proves the known-limit check gave up up front.
  cd <- metacheck:::zip_peek(url, skip_on_api_limit = TRUE)
  expect_null(cd)
})

test_that(".remote_size falls back to a ranged request when HEAD is refused", {
  httr2::local_mocked_responses(.s3_like_host(as.raw(1:200)))
  expect_equal(metacheck:::.remote_size("https://s3-like.example/file.csv"), 200)

  # A host answering HEAD with a 403 error page must not have that page's
  # Content-Length read as the file size.
  httr2::local_mocked_responses(function(req) {
    if (identical(req$method, "HEAD"))
      return(httr2::response(403L, headers = list(`Content-Length` = "243")))
    httr2::response(403L)
  })
  expect_true(is.na(metacheck:::.remote_size("https://s3-like.example/private.csv")))
})
