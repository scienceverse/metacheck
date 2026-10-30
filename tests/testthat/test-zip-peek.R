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
  # a near miss is a mismatch: all.equal() accepted it (U70)
  expect_false(metacheck:::.zip_crc_ok(b, 3421780262 + 20))
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

test_that(".zip_member_fetch refuses an entry whose offset is unknown instead of using the sentinel", {
  # A >4GB archive stores 0xFFFFFFFF in the 4-byte fields; the parser reads the
  # real value from the entry's Zip64 extra field, and turns the placeholder
  # into NA only when that field is missing. Fetching from that offset would
  # request a meaningless byte range.
  entry <- data.frame(name = "big.dat", size = NA_real_, method = 8,
                      csize = NA_real_, offset = NA_real_, crc = 1)
  expect_null(metacheck:::.zip_member_fetch("http://example.invalid/x.zip", entry))
})

test_that(".zip_member_fetch records why a fetch failed instead of a bare NULL (#429)", {
  # Without a `reason` environment nothing changes -- still a bare NULL.
  entry <- data.frame(name = "big.dat", size = NA_real_, method = 8,
                      csize = NA_real_, offset = NA_real_, crc = 1)
  reason <- new.env(parent = emptyenv())
  expect_null(metacheck:::.zip_member_fetch("http://example.invalid/x.zip", entry,
                                            reason = reason))
  expect_match(reason$msg, "Zip64")
})

# Captured as plain top-level code before the tests below replace zip_peek():
# local_mocked_bindings()'s own restore does not reliably take effect in this
# setup (see the same note in test-repo-download.R), and the tests at the end
# of this file need the real function. It is put back explicitly in the
# namespace after them, which is where package code such as zip_decision()
# looks it up. A bare zip_peek() call inside a later test is still found in a
# different environment that keeps the stand-in, so those tests call
# metacheck:::zip_peek() instead.
.real_zip_peek <- get("zip_peek", envir = asNamespace("metacheck"), inherits = FALSE)

test_that(".zip_fetch_members reports a per-member error instead of a silent ok=FALSE (#429)", {
  # A listing succeeds (mocked), but the member itself can never be fetched
  # (offset/csize NA, i.e. Zip64) -- this must show up as a specific message in
  # the `error` column, not just ok == FALSE with nothing to say why.
  local_mocked_bindings(
    zip_peek = function(url, ...) {
      data.frame(name = "big.dat", size = NA_real_, method = 8,
                csize = NA_real_, offset = NA_real_, crc = 1)
    }
  )
  d <- withr::local_tempdir()
  got <- metacheck:::.zip_fetch_members("http://example.invalid/x.zip", dest = d)
  expect_false(got$ok)
  expect_match(got$error, "Zip64")
})

test_that(".zip_fetch_members downloads the archive once in whole-archive mode (#471)", {
  # A real local zip, served through a file:// URL so .download_one() can fetch
  # it without a network; zip_peek() is stubbed with the zip's own central
  # directory, as repo_check would have read it.
  d <- withr::local_tempdir()
  writeLines(rep("id,x", 200), file.path(d, "a.csv"))
  writeLines(rep("y", 50), file.path(d, "b.csv"))
  dir.create(file.path(d, "sub"))
  writeLines("z <- 1", file.path(d, "sub", "c.R"))
  writeLines("stimulus", file.path(d, "img.txt"))
  withr::with_dir(d, utils::zip("arc.zip", c("a.csv", "b.csv", "sub/c.R", "img.txt"),
                                flags = "-q"))
  zip <- file.path(d, "arc.zip")
  skip_if_not(file.exists(zip), "zip utility unavailable")
  cd <- metacheck:::.parse_zip_central_dir(readBin(zip, "raw", file.size(zip)))
  local_mocked_bindings(zip_peek = function(url, ...) cd)

  url <- paste0("file:///", normalizePath(zip, winslash = "/"))
  dest <- file.path(d, "out")
  kept <- file.path(d, "kept.zip")
  # request_bytes = Inf (requests counted only, as for Dryad) takes the
  # archive whenever it fits the budget. With the default 1 MB per request
  # this tiny archive's three adjacent members would be one range request,
  # cheaper than the archive (see the .zip_whole_is_cheaper() test below).
  got <- metacheck:::.zip_fetch_members(url, names = c("a.csv", "b.csv", "sub/c.R"),
                                        dest = dest, max_whole_bytes = Inf,
                                        archive_path = kept, request_bytes = Inf)
  expect_true(all(got$ok))
  expect_equal(normalizePath(got$path), normalizePath(file.path(dest, got$name)))
  expect_equal(readLines(file.path(dest, "sub", "c.R")), "z <- 1")
  expect_false(file.exists(file.path(dest, "img.txt")))   # not wanted, not written
  expect_true(file.exists(kept))   # archive kept for a restarted run

  # Too large for the budget: the archive is not downloaded (NULL tells
  # .zip_fetch_members() to fetch member by member instead).
  kept2 <- file.path(d, "kept2.zip")
  want <- cd[cd$name %in% c("a.csv", "b.csv"), , drop = FALSE]
  expect_null(metacheck:::.zip_fetch_whole(url, cd, want, file.path(d, "out2", want$name),
                                           max_bytes = 10, archive_path = kept2))
  expect_false(file.exists(kept2))
})

test_that(".zip_whole_size estimates a zip's size from its central directory (#471)", {
  d <- withr::local_tempdir()
  writeLines(rep("id,x", 200), file.path(d, "a.csv"))
  writeLines("hello", file.path(d, "b.txt"))
  withr::with_dir(d, utils::zip("arc.zip", c("a.csv", "b.txt"), flags = "-q"))
  zip <- file.path(d, "arc.zip")
  skip_if_not(file.exists(zip), "zip utility unavailable")
  cd <- metacheck:::.parse_zip_central_dir(readBin(zip, "raw", file.size(zip)))
  est <- metacheck:::.zip_whole_size(cd)
  # Local extra fields are not in the central directory, so the estimate may be
  # short by a few dozen bytes per member, never more.
  expect_true(abs(est - file.size(zip)) < 100 * nrow(cd))
  cd$offset[1] <- NA   # Zip64: unknown
  expect_true(is.na(metacheck:::.zip_whole_size(cd)))
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
  # each file takes its own type, not the zip's "archive" (U213)
  expect_equal(rows$file_type[rows$file_name == "study.csv"], "data")
  expect_equal(rows$file_type[rows$file_name == "README.txt"], "text")
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

test_that(".range_status_is_transient retries any status except 200/206 (#429)", {
  # Widened after #429: an intermittent 401 (Dryad's real, confirmed status
  # for an unauthenticated OR momentarily-flaky request, see .auth_for_url()'s
  # comment) was accepted as final after exactly one try before this, since it
  # is not on .storage_is_transient()'s fixed 403/429/5xx list. 200 and 206
  # are the two statuses that mean "the request completed, one way or
  # another" -- nothing to retry.
  resp <- function(status) httr2::response(status)
  expect_true(metacheck:::.range_status_is_transient(resp(401L)))
  expect_true(metacheck:::.range_status_is_transient(resp(400L)))
  expect_true(metacheck:::.range_status_is_transient(resp(403L)))
  expect_true(metacheck:::.range_status_is_transient(resp(429L)))
  expect_true(metacheck:::.range_status_is_transient(resp(500L)))
  expect_false(metacheck:::.range_status_is_transient(resp(200L)))
  expect_false(metacheck:::.range_status_is_transient(resp(206L)))
})

test_that(".http_range_bytes's request is configured to retry on a 401 (#429)", {
  # httr2::local_mocked_responses()/with_mocked_responses() do not exercise
  # req_retry()'s actual retry loop at all -- req_perform()'s own source
  # returns the mocked response immediately via handle_resp(), before the
  # `while (tries < max_tries ...)` loop that consults is_transient/backoff
  # is ever reached. This is a documented httr2 mocking limitation, not a
  # metacheck bug -- confirmed independently elsewhere in this codebase (see
  # test-repo-download.R's skipped "skip_on_api_limit = TRUE gives up on a
  # confirmed rate limit fast" test and its comment). So this checks the
  # request's policy directly instead of simulating multiple attempts.
  req <- httr2::request("https://flaky.example/x.zip") |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                     is_transient = metacheck:::.range_status_is_transient,
                     backoff = metacheck:::.storage_backoff)
  policy <- req$policies$retry_max_tries
  expect_equal(policy, 3)
  is_transient <- req$policies$retry_is_transient
  expect_true(is_transient(httr2::response(401L)))
  expect_true(is_transient(httr2::response(400L)))
  expect_false(is_transient(httr2::response(200L)))
  expect_false(is_transient(httr2::response(206L)))
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

test_that("a failed zip_peek() download is kept for the session only, not on disk (U210)", {
  # a connection failure, rate limit or server error passes, but the disk
  # entry outlived the session, so the zip was not listed again
  d <- withr::local_tempdir()
  withr::local_options(metacheck.zip_peek_cache.dir = d)
  url <- "https://down.example/failed.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))
  local_mocked_bindings(.http_range_tail = function(...) NULL, .head_size = function(...) 100)
  expect_null(metacheck:::zip_peek(url, cache = TRUE))
  expect_true(exists(url, envir = metacheck:::.zip_peek_cache, inherits = FALSE))
  expect_false(metacheck:::.zip_peek_cache_has(url))
})

test_that("an unreadable zip-peek cache entry is a miss, and a new entry replaces it (U210)", {
  # an interrupted write left a file that was returned as a cached failure
  d <- withr::local_tempdir()
  withr::local_options(metacheck.zip_peek_cache.dir = d)
  url <- "https://x.example/broken.zip"
  writeBin(as.raw(1:10), metacheck:::.zip_peek_cache_path(url))
  expect_false(metacheck:::.zip_peek_cache_has(url))
  metacheck:::.zip_peek_cache_put(url, data.frame(name = "a", size = 1))
  expect_true(metacheck:::.zip_peek_cache_has(url))
  expect_equal(metacheck:::.zip_peek_cache_get(url)$name, "a")
  expect_equal(length(list.files(d)), 1)   # no temporary file left behind
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

# ── Zip64 archives ──────────────────────────────────────────────────────────
# A real Zip64 archive is over 4 GB or holds over 65,535 entries, too large to
# build in a test, but the format does not depend on size: any archive may be
# written with Zip64 records, and the placeholders (0xFFFFFFFF) mean the same
# thing whatever the real values are. .test_zip64_bytes() writes one by hand,
# with stored (uncompressed) members.

# Little-endian bytes of a non-negative number.
.le_bytes <- function(v, n) as.raw((v %/% 256^(0:(n - 1))) %% 256)

# A Zip64 archive holding `members` (a named list of raw vectors). `zip64`
# names the central-directory fields written as placeholders, with their real
# values in each entry's Zip64 extra field: any of "usize", "csize",
# "offset". `local_extra` is the length of a padding extra field added to each
# member's LOCAL header only (the central directory does not record it), to
# make a local header longer than its central copy, as real writers do.
.test_zip64_bytes <- function(members, zip64 = c("usize", "csize", "offset"),
                              local_extra = 0) {
  ff4 <- .le_bytes(4294967295, 4)
  fields <- c("usize", "csize", "offset")
  locals <- list(); centrals <- list(); offset <- 0
  for (nm in names(members)) {
    data <- members[[nm]]
    name <- charToRaw(nm)
    crc <- .le_bytes(metacheck:::.crc32(data), 4)
    size <- .le_bytes(length(data), 4)
    pad <- if (local_extra > 0)
      c(.le_bytes(0xCAFE, 2), .le_bytes(local_extra - 4, 2), raw(local_extra - 4))
    else raw(0)
    local <- c(as.raw(c(0x50, 0x4b, 0x03, 0x04)), .le_bytes(45, 2), raw(2),
               raw(2),                     # method 0: stored
               raw(4), crc, size, size,
               .le_bytes(length(name), 2), .le_bytes(length(pad), 2), name, pad, data)
    vals <- c(usize = length(data), csize = length(data), offset = offset)
    in_extra <- fields[fields %in% zip64]
    extra <- if (length(in_extra) > 0)
      c(.le_bytes(1, 2), .le_bytes(8 * length(in_extra), 2),
        unlist(lapply(in_extra, \(f) .le_bytes(vals[[f]], 8))))
    else raw(0)
    field <- function(f) if (f %in% zip64) ff4 else .le_bytes(vals[[f]], 4)
    central <- c(as.raw(c(0x50, 0x4b, 0x01, 0x02)), .le_bytes(45, 2), .le_bytes(45, 2),
                 raw(2), raw(2), raw(4), crc, field("csize"), field("usize"),
                 .le_bytes(length(name), 2), .le_bytes(length(extra), 2),
                 raw(2), raw(2), raw(2), raw(4), field("offset"), name, extra)
    locals[[nm]] <- local; centrals[[nm]] <- central
    offset <- offset + length(local)
  }
  body <- unlist(locals, use.names = FALSE)
  cd <- unlist(centrals, use.names = FALSE)
  n <- length(members)
  rec_at <- length(body) + length(cd)
  rec64 <- c(as.raw(c(0x50, 0x4b, 0x06, 0x06)), .le_bytes(44, 8), .le_bytes(45, 2),
             .le_bytes(45, 2), raw(4), raw(4), .le_bytes(n, 8), .le_bytes(n, 8),
             .le_bytes(length(cd), 8), .le_bytes(length(body), 8))
  loc64 <- c(as.raw(c(0x50, 0x4b, 0x06, 0x07)), raw(4), .le_bytes(rec_at, 8),
             .le_bytes(1, 4))
  eocd <- c(as.raw(c(0x50, 0x4b, 0x05, 0x06)), raw(2), raw(2),
            .le_bytes(65535, 2), .le_bytes(65535, 2), ff4, ff4, raw(2))
  c(body, cd, rec64, loc64, eocd)
}

# A mocked host serving `bytes` that honours ranges and reports the final URL
# as the requested one (so .range_ignored_record() sees no redirect); every
# request is logged. `ignore_range = TRUE` answers every GET with 200 and the
# whole body, as PsychArchives does.
.range_host <- function(bytes, log, ignore_range = FALSE) {
  total <- length(bytes)
  function(req) {
    method <- req$method %||% "GET"
    range <- req$headers$Range %||% NA_character_
    log$calls <- c(log$calls, paste(method, range))
    if (identical(method, "HEAD"))
      return(httr2::response(200L, url = req$url,
                             headers = list(`Content-Length` = as.character(total))))
    if (ignore_range || is.na(range))
      return(httr2::response(200L, url = req$url,
                             headers = list(`Content-Length` = as.character(total)),
                             body = bytes))
    if (grepl("^bytes=-", range)) {
      from <- max(0, total - as.numeric(sub("^bytes=-", "", range))); to <- total - 1
    } else {
      lims <- as.numeric(strsplit(sub("^bytes=", "", range), "-")[[1]])
      from <- lims[1]; to <- min(lims[2], total - 1)
    }
    httr2::response(206L, url = req$url,
      headers = list(`Content-Range` = sprintf("bytes %.0f-%.0f/%d", from, to, total)),
      body = bytes[(from + 1):(to + 1)])
  }
}

.test_members <- function() {
  list("data/a.csv" = charToRaw(strrep("id,x\n1,2\n", 40)),
       "b.R"        = charToRaw("x <- 1\n"),
       "c.txt"      = charToRaw("notes\n"))
}

test_that(".parse_zip_central_dir reads a Zip64 archive's sizes and offsets", {
  members <- .test_members()
  bytes <- .test_zip64_bytes(members)
  cd <- metacheck:::.parse_zip_central_dir(bytes)
  expect_equal(cd$name, names(members))
  expect_equal(cd$size, unname(as.numeric(lengths(members))))
  expect_equal(cd$csize, unname(as.numeric(lengths(members))))
  expect_false(anyNA(cd$offset))
  # Each offset points at that member's local header signature.
  for (o in cd$offset)
    expect_identical(bytes[(o + 1):(o + 4)], as.raw(c(0x50, 0x4b, 0x03, 0x04)))
  # The central directory starts where the last member's bytes end.
  expect_equal(attr(cd, "cd_offset"), max(cd$offset) + 30 + nchar("c.txt") + 6)

  # The same when the tail is only part of the archive and its total size is
  # known (the Zip64 end record is then found by its absolute offset).
  tail <- utils::tail(bytes, length(bytes) - 10)
  attr(tail, "total") <- length(bytes)
  expect_equal(metacheck:::.parse_zip_central_dir(tail)$offset, cd$offset)
})

test_that(".parse_zip_central_dir reads a Zip64 extra field holding only the offset", {
  # The Figshare case (10.6084/m9.figshare.28052888.v1): sizes fit in 32 bits,
  # offsets past 4 GB do not, so the extra field holds the offset alone.
  members <- .test_members()
  cd <- metacheck:::.parse_zip_central_dir(.test_zip64_bytes(members, zip64 = "offset"))
  expect_equal(cd$size, unname(as.numeric(lengths(members))))
  expect_false(anyNA(cd$offset))
  expect_equal(cd$offset[1], 0)
})

test_that(".parse_zip_central_dir marks a placeholder with no Zip64 field as NA", {
  members <- .test_members()
  bytes <- .test_zip64_bytes(members, zip64 = "offset")
  cd_ok <- metacheck:::.parse_zip_central_dir(bytes)
  # Turn the first entry's extra-field ID into an unrelated one: the offset
  # placeholder then has no real value to replace it.
  cd_start <- attr(cd_ok, "cd_offset") + 1
  extra_at <- cd_start + 46 + nchar(names(members)[1])
  bytes[extra_at:(extra_at + 1)] <- .le_bytes(0x7075, 2)
  cd <- metacheck:::.parse_zip_central_dir(bytes)
  expect_equal(nrow(cd), 3)
  expect_true(is.na(cd$offset[1]))
  expect_false(anyNA(cd$offset[2:3]))
})

test_that("zip_peek() lists a Zip64 archive and its members can be fetched", {
  members <- .test_members()
  bytes <- .test_zip64_bytes(members)
  url <- "https://range-host.example/zip64.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(bytes, log))

  cd <- metacheck:::zip_peek(url, size = length(bytes))
  expect_setequal(cd$name, names(members))
  d <- withr::local_tempdir()
  got <- metacheck:::.zip_fetch_members(url, dest = d)
  expect_true(all(got$ok))
  for (nm in names(members))
    expect_identical(readBin(file.path(d, nm), "raw", 1e4), members[[nm]])
})

test_that("a member is fetched with one range request, and nearby members share one", {
  d0 <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d0)
  skip_if(is.null(bytes), "zip utility unavailable")
  url <- "https://range-host.example/one-request.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(bytes, log))
  cd <- metacheck:::zip_peek(url, size = length(bytes))

  # One member: one request (it used to be two, header then data).
  log$calls <- character(0)
  got <- metacheck:::.zip_member_fetch(url, cd[cd$name == "data.csv", , drop = FALSE])
  expect_identical(got, readBin(file.path(d0, "data.csv"), "raw", 1e5))
  expect_length(log$calls, 1)

  # Both members sit next to each other: one request for the two.
  log$calls <- character(0)
  d <- withr::local_tempdir()
  fetched <- metacheck:::.zip_fetch_members(url, dest = d)
  expect_true(all(fetched$ok))
  expect_length(log$calls, 1)
  expect_identical(readBin(file.path(d, "notes.txt"), "raw", 100),
                   readBin(file.path(d0, "notes.txt"), "raw", 100))
})

test_that(".zip_range_plan merges members within the gap and splits the rest", {
  want <- data.frame(name = c("a", "b", "c"), size = 10, method = 0,
                     csize = c(100, 100, 100), offset = c(0, 200, 1e6), crc = NA)
  plan <- metacheck:::.zip_range_plan(want, max_gap = 65536)
  expect_length(plan, 2)
  expect_equal(plan[[1]]$rows, 1:2)
  expect_equal(plan[[2]]$rows, 3)
  # A range stops at the next local header instead of running the 1 KB margin
  # into it ...
  expect_equal(plan[[1]]$to, 200 + 30 + 1 + 100 + 1024 - 1)
  expect_equal(metacheck:::.zip_range_plan(want[1, ], want)[[1]]$to, 199)
  # ... and at the start of the central directory for the last member.
  cd <- want; attr(cd, "cd_offset") <- 1e6 + 131
  expect_equal(metacheck:::.zip_range_plan(want[3, ], cd)[[1]]$to, 1e6 + 130)
  # Without a gap allowance every member is requested on its own.
  expect_length(metacheck:::.zip_range_plan(want, max_gap = -1), 3)
})

test_that("a local extra field longer than the margin costs a second request, not a failure", {
  members <- .test_members()
  bytes <- .test_zip64_bytes(members, local_extra = 3000)
  url <- "https://range-host.example/long-extra.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(bytes, log))
  cd <- metacheck:::zip_peek(url, size = length(bytes))
  # No later bound (a listing of this member alone), so the 1 KB margin is all
  # the first request covers.
  entry <- cd[cd$name == "b.R", , drop = FALSE]
  log$calls <- character(0)
  got <- metacheck:::.zip_member_fetch(url, entry)
  expect_identical(got, members[["b.R"]])
  expect_length(log$calls, 2)
})

test_that(".http_range_bytes accepts a range cut short at the end of the file only when asked", {
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(as.raw(1:100), log))
  url <- "https://range-host.example/short.bin"
  expect_identical(metacheck:::.http_range_bytes(url, 90, 200, allow_short = TRUE),
                   as.raw(91:100))
  reason <- new.env(parent = emptyenv())
  expect_null(metacheck:::.http_range_bytes(url, 90, 200, reason = reason))
  expect_match(reason$msg, "short read")
})

test_that(".zip_whole_is_cheaper weighs requests against bytes", {
  cd <- data.frame(name = sprintf("f%02d.csv", 1:20), size = 1000, method = 0,
                   csize = 1000, offset = seq(0, by = 1e6, length.out = 20), crc = NA)
  plan <- metacheck:::.zip_range_plan(cd, cd)   # 20 members 1 MB apart: 20 requests
  expect_length(plan, 20)
  # A ~19 MB archive against 20 requests of ~2 KB each: the archive is cheaper.
  expect_true(metacheck:::.zip_whole_is_cheaper(cd, cd, plan, Inf))
  # ... but not when it does not fit the budget, or the caller turned it off.
  expect_false(metacheck:::.zip_whole_is_cheaper(cd, cd, plan, 1e6))
  expect_false(metacheck:::.zip_whole_is_cheaper(cd, cd, plan, 0))
  # Three members from the same archive: three requests cost less than 19 MB.
  few <- cd[1:3, ]
  expect_false(metacheck:::.zip_whole_is_cheaper(cd, few,
    metacheck:::.zip_range_plan(few, cd), Inf))
  # Counting requests only (Dryad's quota), three members are enough.
  expect_true(metacheck:::.zip_whole_is_cheaper(cd, few,
    metacheck:::.zip_range_plan(few, cd), Inf, request_bytes = Inf))
  # Fewer members than min_whole_members never take the archive.
  expect_false(metacheck:::.zip_whole_is_cheaper(cd, cd[1:2, ],
    metacheck:::.zip_range_plan(cd[1:2, ], cd), Inf, request_bytes = Inf))
})

# ── Hosts that ignore Range ─────────────────────────────────────────────────

test_that(".range_ignored_key keeps the path's shape and wildcards ids", {
  expect_equal(
    metacheck:::.range_ignored_key("https://www.psycharchives.org/rest/bitstreams/112719a3-3346/retrieve"),
    "www.psycharchives.org/rest/bitstreams/*/retrieve")
  # Zenodo's per-file URLs and its files-archive get different keys.
  expect_false(identical(
    metacheck:::.range_ignored_key("https://zenodo.org/api/records/123/files-archive"),
    metacheck:::.range_ignored_key("https://zenodo.org/api/records/123/files/a.zip/content")))
})

test_that("a host that ignores Range is not read whole and is skipped afterwards", {
  withr::defer(rm(list = ls(envir = metacheck:::.range_ignored_hosts),
                  envir = metacheck:::.range_ignored_hosts))
  big <- as.raw(rep(1:255, length.out = 3 * 1048576))   # 3 MB, over the 2 MB limit
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(big, log, ignore_range = TRUE))
  url1 <- "https://ignores.example/bitstreams/111/retrieve"
  url2 <- "https://ignores.example/bitstreams/222/retrieve"
  withr::defer(suppressWarnings(rm(list = c(url1, url2), envir = metacheck:::.zip_peek_cache)))

  expect_null(metacheck:::.http_range_tail(url1, 131072, total = length(big)))
  expect_true(metacheck:::.range_ignored_known(url2))   # same host and path shape

  # A later peek on that host sends nothing at all.
  log$calls <- character(0)
  expect_null(metacheck:::zip_peek(url2, size = length(big)))
  expect_length(log$calls, 0)
  reason <- new.env(parent = emptyenv())
  expect_null(metacheck:::.http_range_bytes(url2, 0, 10, reason = reason))
  expect_match(reason$msg, "range not honoured")
  expect_length(log$calls, 0)
})

test_that("a small file sent whole by a host that ignores Range is still read", {
  withr::defer(rm(list = ls(envir = metacheck:::.range_ignored_hosts),
                  envir = metacheck:::.range_ignored_hosts))
  d <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(bytes, log, ignore_range = TRUE))
  url <- "https://ignores-small.example/files/1/retrieve"
  # A tail shorter than the file: the 200 answer holds the tail, and the file
  # is far under 2 MB, so it is read.
  raw <- metacheck:::.http_range_tail(url, 64, total = length(bytes))
  expect_identical(as.vector(raw), utils::tail(bytes, 64))
  expect_true(metacheck:::.range_ignored_known(url))
})

test_that("a 200 that came from another host after a redirect is not recorded", {
  withr::defer(rm(list = ls(envir = metacheck:::.range_ignored_hosts),
                  envir = metacheck:::.range_ignored_hosts))
  url <- "https://osf.io/download/6891f2f7157709eae14783e1/"
  resp <- httr2::response(200L, url = "https://files.osf.io/v1/resources/x/providers/github/a.zip")
  expect_false(metacheck:::.range_ignored_record(url, resp))
  expect_false(metacheck:::.range_ignored_known(url))
  resp_same <- httr2::response(200L, url = url)
  expect_true(metacheck:::.range_ignored_record(url, resp_same))
  expect_true(metacheck:::.range_ignored_known(url))
})

# ── Zips already on disk ────────────────────────────────────────────────────

test_that("zip_peek() lists a zip on disk without a request, and its members extract locally", {
  d <- withr::local_tempdir()
  bytes <- .test_zip_bytes(d)
  skip_if(is.null(bytes), "zip utility unavailable")
  path <- file.path(d, "t.zip")
  httr2::local_mocked_responses(function(req) stop("no request expected"))

  cd <- metacheck:::zip_peek(path)
  expect_setequal(cd$name, c("data.csv", "notes.txt"))
  # The same columns a remote peek gives, with the same values.
  expect_equal(cd, metacheck:::.parse_zip_central_dir(bytes), ignore_attr = TRUE)
  # Not cached: a local file can change.
  expect_false(exists(path, envir = metacheck:::.zip_peek_cache, inherits = FALSE))

  out <- file.path(d, "out")
  got <- metacheck:::.zip_fetch_members(path, names = "notes.txt", dest = out)
  expect_true(got$ok)
  expect_identical(readBin(got$path, "raw", 100),
                   readBin(file.path(d, "notes.txt"), "raw", 100))

  expect_equal(metacheck:::.zip_local_source(data.frame(
    file_location = c(path, NA, file.path(d, "missing.zip")))),
    c(path, NA, NA))
})

test_that("zip_decision() passes a known size on to zip_peek()", {
  seen <- NULL
  local_mocked_bindings(zip_peek = function(url, ..., size = NULL) {
    seen <<- size
    data.frame(name = "study.csv", size = 1)
  })
  zip_decision("http://x/data.zip", size = 1234)
  expect_equal(seen, 1234)
})
assignInNamespace("zip_peek", .real_zip_peek, ns = "metacheck")

test_that("a whole small file sent as 200 for a range from byte 0 is accepted, not recorded", {
  withr::defer(rm(list = ls(envir = metacheck:::.range_ignored_hosts),
                  envir = metacheck:::.range_ignored_hosts))
  # The first member of a tiny archive, requested with its 1 KB margin, covers
  # the whole file; a host may answer that with a plain 200.
  log <- new.env(); log$calls <- character(0)
  httr2::local_mocked_responses(.range_host(as.raw(1:100), log, ignore_range = TRUE))
  url <- "https://whole-ok.example/files/1/tiny.zip"
  expect_identical(metacheck:::.http_range_bytes(url, 0, 500, allow_short = TRUE),
                   as.raw(1:100))
  expect_false(metacheck:::.range_ignored_known(url))
  # Without allow_short the exact range is required, as before.
  expect_null(metacheck:::.http_range_bytes(url, 0, 500))
  expect_false(metacheck:::.range_ignored_known(url))
})

# Archives in data_check unpack only the inner files that will be kept, and
# none larger than the per-file limit (#472): a 74 MB go.txt.gz became a
# 1.2 GB copy in the cache, and picrust2's 6.4 GB of copies were all dropped
# right after, as plain documentation.
.archive_row_472 <- function() data.frame(
  repo_url = "r", file_name = "x", file_path = "x", file_url = "u",
  file_location = NA_character_, file_size = NA_real_, paper_id = "p.1",
  data_type = "unknown", doc_role = NA_character_, data_format = NA_character_)
.big_csv_472 <- function() c("id,value", paste0(1:20000, ",", 1:20000 / 7))

test_that(".expand_compressed decompresses only a kept file, and only up to the limit (#472)", {
  d <- withr::local_tempdir()
  w_gz <- function(name, lines) {
    p <- file.path(d, name); con <- gzfile(p, "w"); writeLines(lines, con); close(con); p
  }
  row <- .archive_row_472()

  # documentation by name: dropped, so never decompressed
  p <- w_gz("go.txt.gz", rep("GO:0000001\tdescription", 5000))
  expect_equal(nrow(metacheck:::.expand_compressed(p, row)), 0)
  expect_false(file.exists(file.path(paste0(p, ".contents"), "go.txt")))

  # data within the limit: decompressed as before
  p <- w_gz("results.csv.gz", .big_csv_472())
  rows <- metacheck:::.expand_compressed(p, row)
  expect_equal(rows$file_name, "results.csv")
  expect_true(file.exists(rows$file_location))

  # over the limit: decompression stops, no copy is kept (also not the one
  # the call above left), and the file is listed without a copy and reported
  rows <- metacheck:::.expand_compressed(p, row, max_bytes = 100 * 1024)
  expect_false(file.exists(file.path(paste0(p, ".contents"), "results.csv")))
  expect_equal(rows$file_name, "results.csv")
  expect_true(is.na(rows$file_location))
  expect_equal(rows$data_type, "data")
  expect_equal(attr(rows, "oversize")$file_name, "results.csv")
})

test_that("a stopped extraction is not reused as complete (U203)", {
  # extraction went straight into "<archive>.contents", so a half-written file
  # there was reused by the next run; a stopped run now leaves it in ".part"
  d <- withr::local_tempdir()
  writeLines(c("id,x", "1,2", "2,3"), file.path(d, "study.csv"))
  z <- file.path(d, "a.zip")
  withr::with_dir(d, utils::zip("a.zip", "study.csv", flags = "-q"))
  skip_if_not(file.exists(z), "zip utility unavailable")
  row <- .archive_row_472()
  dest <- metacheck:::.archive_dest(z)
  dir.create(paste0(dest, ".part"))
  writeLines("id,x", file.path(paste0(dest, ".part"), "study.csv"))   # cut short
  rows <- metacheck:::.expand_zip(z, row)
  expect_equal(readLines(rows$file_location), c("id,x", "1,2", "2,3"))
  expect_false(dir.exists(paste0(dest, ".part")))

  gz <- file.path(d, "notes.csv.gz")
  con <- gzfile(gz, "w"); writeLines(c("a", "b"), con); close(con)
  gdest <- metacheck:::.archive_dest(gz)
  rows <- metacheck:::.expand_compressed(gz, row)
  expect_equal(readLines(rows$file_location), c("a", "b"))
  expect_false(dir.exists(paste0(gdest, ".part")))
})

test_that("an archive of the user's own is extracted into the temporary folder (U214)", {
  # extracting beside it wrote into the user's folder, and the next run
  # counted every extracted file twice
  own <- withr::local_tempfile(tmpdir = test_path(), fileext = ".zip")
  writeBin(as.raw(1:10), own)
  dest <- metacheck:::.archive_dest(own)
  expect_false(startsWith(dest, normalizePath(test_path(), winslash = "/")))
  expect_true(startsWith(normalizePath(dest, winslash = "/", mustWork = FALSE),
                         normalizePath(tempdir(), winslash = "/")))
  # a downloaded archive (here: in this session's download folder) is
  # extracted beside itself
  dl <- withr::local_tempfile(tmpdir = metacheck:::.repo_session_dir(), fileext = ".zip")
  writeBin(as.raw(1:10), dl)
  expect_equal(metacheck:::.archive_dest(dl), paste0(dl, ".contents"))
  # a folder of the user's in the temporary folder (an unpacked upload) is
  # the user's too
  up <- withr::local_tempfile(fileext = ".zip")
  writeBin(as.raw(1:10), up)
  expect_false(identical(metacheck:::.archive_dest(up), paste0(up, ".contents")))
  expect_match(metacheck:::.archive_dest(up), "metacheck-archives")
})

test_that(".expand_zip and .expand_tar unpack only kept members within the limit (#472)", {
  d <- withr::local_tempdir()
  src <- file.path(d, "src"); dir.create(src)
  writeLines(.big_csv_472(), file.path(src, "big.csv"))
  writeLines(c("a,b", "1,2"), file.path(src, "small.csv"))
  writeLines("notes", file.path(src, "notes.txt"))
  writeLines("# Read me", file.path(src, "README.md"))
  members <- c("big.csv", "small.csv", "notes.txt", "README.md")
  row <- .archive_row_472()
  row$file_type <- "archive"

  z <- file.path(d, "data.zip")
  withr::with_dir(src, utils::zip(z, members, flags = "-q"))
  skip_if_not(file.exists(z), "zip utility unavailable")
  archives <- c(zip = z,
                tar = file.path(d, "data.tar"),
                tgz = file.path(d, "data.tar.gz"))
  withr::with_dir(src, {
    utils::tar(archives[["tar"]], members, compression = "none", tar = "internal")
    utils::tar(archives[["tgz"]], members, compression = "gzip", tar = "internal")
  })

  for (a in archives) {
    rows <- if (grepl("zip$", a)) metacheck:::.expand_zip(a, row, max_bytes = 100 * 1024)
            else metacheck:::.expand_tar(a, row, max_bytes = 100 * 1024)
    # notes.txt (plain documentation) and big.csv (over the limit) are not unpacked
    expect_setequal(list.files(metacheck:::.archive_dest(a)), c("small.csv", "README.md"))
    expect_setequal(rows$file_name, c("small.csv", "README.md", "big.csv"))
    expect_true(is.na(rows$file_location[rows$file_name == "big.csv"]))
    expect_equal(attr(rows, "oversize")$file_name, "big.csv")
    # a file over the limit takes its own type too (U213)
    expect_equal(rows$file_type[rows$file_name == "big.csv"], "data")
    if (!grepl("tar.gz$", a))   # a compressed tar gives no size before unpacking
      expect_equal(attr(rows, "oversize")$file_size, file.size(file.path(src, "big.csv")))
  }
})

test_that("zip_peek asks for exactly the central directory a short first tail lacks (#473)", local({
  # A first tail too short for the central directory used to be followed by a
  # fixed 1 MB tail: too much on a slow host (Zenodo range requests at
  # ~7 KB/s), too little for a directory over 1 MB. The end record in the
  # first tail gives the size the directory needs.
  d <- withr::local_tempdir()
  files <- sprintf("file_with_a_fairly_long_name_%03d.csv", 1:60)
  for (f in files) writeLines("x", file.path(d, f))
  z <- file.path(d, "many.zip")
  withr::with_dir(d, utils::zip(z, files, flags = "-q"))
  skip_if_not(file.exists(z), "zip utility unavailable")
  bytes <- readBin(z, "raw", file.size(z))
  url <- "https://example.org/many-473.zip"
  withr::defer(suppressWarnings(rm(list = url, envir = metacheck:::.zip_peek_cache)))

  asked <- numeric(0)
  testthat::local_mocked_bindings(
    .http_range_tail = function(url, n, total = NULL, ...) {
      asked <<- c(asked, n)
      out <- utils::tail(bytes, n)
      attr(out, "total") <- length(bytes)
      out
    })
  cd <- metacheck:::zip_peek(url, tail_bytes = 1024, size = length(bytes))
  expect_setequal(cd$name, files)
  expect_length(asked, 2)
  expect_equal(asked[[1]], 1024)
  # far less than the old 1 MB retry, and enough
  expect_lt(asked[[2]], 20000)
  need <- metacheck:::.parse_zip_central_dir(utils::tail(bytes, 1024), need_only = TRUE)
  expect_equal(asked[[2]], need + 1024)
}))

test_that("repository requests give up on a stalled connection (#473)", {
  req <- metacheck:::.mc_request("https://example.org/x")
  expect_equal(req$options$low_speed_limit, 1024L)
  expect_equal(req$options$low_speed_time, 60L)
  expect_equal(req$options$connecttimeout, 30L)
  expect_equal(metacheck:::.mc_request("https://example.org/x", low_speed_time = 300)$options$low_speed_time,
               300L)

  # live: a server that sends 10 bytes over 60 s is given up on after the
  # window (5 s here), instead of being waited for
  skip_if_quick()
  skip_if_offline("httpbin.org")
  t0 <- Sys.time()
  err <- tryCatch(
    httr2::req_perform(metacheck:::.mc_request(
      "https://httpbin.org/drip?duration=60&numbytes=10&delay=1", low_speed_time = 5)),
    error = function(e) e)
  expect_s3_class(err, "error")
  expect_lt(as.numeric(Sys.time() - t0, units = "secs"), 30)
})

test_that("zip member names without the UTF-8 flag are read as CP437 (U76)", {
  # old Windows zip tools store "café.csv" as CP437 bytes (0x82 = é) without
  # the UTF-8 flag; the name was kept as invalid UTF-8 and every later
  # sub()/gsub() stopped data_check with "invalid multibyte string"
  members <- list(charToRaw("a,b\n1,2\n"))
  names(members) <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0x82, 0x2e, 0x63, 0x73, 0x76)))
  cd <- metacheck:::.parse_zip_central_dir(.test_zip64_bytes(members, zip64 = character(0)))
  expect_equal(cd$name, "caf\u00e9.csv")
  expect_true(validUTF8(cd$name))
  # the stored length, which the UTF-8 name (9 bytes) no longer has
  expect_equal(cd$name_bytes, 8)
})
