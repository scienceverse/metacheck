# Peek inside a remote ZIP without downloading it. A ZIP stores its file listing
# (the "central directory") at the END of the archive, so an HTTP range request
# for the tail is enough to read every entry's name and uncompressed size. This
# lets the downloader decide whether a zip is worth fetching (does it hold data,
# or only stimuli/assets?) before spending the bandwidth. Falls back to NULL when
# the host doesn't honour ranges or the central directory isn't in the tail, so
# the caller can download-and-inspect instead.

# In-memory, same-session cache of zip_peek() results, keyed by URL. A single
# pipeline run peeks the SAME archive twice: once in repo_check (discovering
# member names/sizes to list, inst/modules/repo_check.R) and again inside
# download_repo_files()'s .zip_fetch_members() (fetching one member's bytes,
# R/repo-download.R) -- the second call needs the very same central-directory
# fields (method/csize/offset/crc) the first call already read but did not
# carry forward. Caching here removes the duplicate network round-trip
# entirely (rather than only making it retry-resilient, see req_retry() below)
# and, since the two calls happen close together within one session, halves
# the chance of a live archive being peeked twice into the same host's burst
# rate limit -- part of what made issue #384's fetch fail non-deterministically.
# Reset per R session (an environment, not the on-disk repo cache in
# R/cache.R): a zip_peek() result is only ever reused within the SAME run that
# produced it, never across runs, so it does not need to persist or be
# user-visible/clearable the way the on-disk file cache does.
.zip_peek_cache <- new.env(parent = emptyenv())

# Little-endian integer from a raw vector slice (1-based, `n` bytes).
.le_int <- function(raw, at, n) {
  bytes <- as.integer(raw[at:(at + n - 1)])
  sum(bytes * 256^(seq_len(n) - 1))
}

# ── Learning a remote file's size (issue #424) ──────────────────────────────
# A HEAD request is the usual way to learn a file's size, but it is not
# reliable: Dryad, Figshare and Harvard Dataverse all redirect their download
# URL to Amazon S3, which answers HEAD with 403 while it answers a ranged GET
# with 206 (verified live 2026-09-24). A 206 or 416 answer to a ranged GET
# carries the total size in its Content-Range header ("bytes 0-0/2545568", or
# "bytes */2246" when the requested range is not satisfiable), so a ranged GET
# is the fallback whenever HEAD gives no usable size.
#
# Whole-archive endpoints that build their zip on request (OSF ?zip=, Zenodo
# files-archive, Dataverse dataset download, GitHub zipball, GitLab
# archive.zip, Dryad dataset download) report no size by EITHER method: they
# ignore Range and stream the body with no Content-Length (verified live
# 2026-09-24). That is why the ranged GET here is sent through
# req_perform_connection(): the status is read before any body, and every
# answer other than 206 is closed unread, so a host that ignores the range
# cannot make this request transfer a whole archive.

# Retry rule for a SIZE request: the same transient/rate-limit rule
# download_repo_files() uses for file bytes, minus 403. The storage rule
# retries 403 because an OSF pre-signed download URL can answer 403 for a
# moment (see .storage_is_transient(), R/repo-download.R), but S3 answers a
# HEAD with 403 every single time, so retrying it only added ~6 s of backoff
# per Dryad/Figshare/Dataverse file before the size request failed anyway.
.size_probe_is_transient_factory <- function(skip_on_api_limit = FALSE) {
  is_transient <- .storage_is_transient_factory(skip_on_api_limit)
  function(resp) httr2::resp_status(resp) != 403L && is_transient(resp)
}

# A host behind an Amazon web firewall (AWS WAF) can answer a request with a
# browser check instead of the file: HTTP 202, an empty body and the header
# "x-amzn-waf-action: challenge", which only a browser running JavaScript can
# pass. LDbase's firewall did this to R's requests for about an hour after a
# burst of requests (verified 2026-10-15; command-line curl from the same
# machine still got the file). Such an answer gives no size, and a file of
# unknown size is left out of download_repo_files()'s downloads without a
# word, so this warns once per host and session, saying why.
.waf_challenge_warned <- new.env(parent = emptyenv())
.waf_challenge_check <- function(resp, url) {
  action <- httr2::resp_header(resp, "x-amzn-waf-action")
  if (is.null(action) || !identical(tolower(action), "challenge")) return(invisible(FALSE))
  host <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) NA_character_)
  host <- tolower(host %||% NA_character_)
  if (!is.na(host) && !exists(host, envir = .waf_challenge_warned, inherits = FALSE)) {
    assign(host, TRUE, envir = .waf_challenge_warned)
    warning(host, "'s firewall answered with a browser check (AWS WAF challenge) ",
            "instead of the file, usually after many requests in a short time. ",
            "Its files cannot be sized or downloaded until it lifts; try again later.",
            call. = FALSE)
  }
  invisible(TRUE)
}

# Total size from a response's Content-Range header, or NA when the header is
# absent or its total is "*" (unknown).
.content_range_total <- function(resp) {
  cr <- httr2::resp_header(resp, "content-range")
  if (is.null(cr) || is.na(cr)) return(NA_real_)
  m <- regmatches(cr, regexec("/([0-9]+)[[:space:]]*$", cr))[[1]]
  if (length(m) < 2) return(NA_real_)
  as.numeric(m[2])
}

# Size in bytes from a HEAD request, or NA. Only a successful (2xx) answer is
# read: an error answer's Content-Length is the length of the error page, not
# of the file.
.head_size <- function(url, skip_on_api_limit = FALSE) {
  tryCatch({
    # See .wait_out_known_rate_limit()'s own comment (R/repo-download.R): a
    # host already known to be rate-limited waits out the remaining time
    # before this request instead of rediscovering the same 429.
    if (!.wait_out_known_rate_limit(url, skip_on_api_limit)) return(NA_real_)
    h <- .mc_request(url) |> httr2::req_method("HEAD") |>
      .auth_for_url() |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                       is_transient = .size_probe_is_transient_factory(skip_on_api_limit),
                       backoff = .storage_backoff,
                       after = .storage_retry_after_factory(skip_on_api_limit)) |>
      httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
    if (.waf_challenge_check(h, url)) return(NA_real_)
    status <- httr2::resp_status(h)
    if (status < 200 || status >= 300) return(NA_real_)
    cl <- suppressWarnings(as.numeric(httr2::resp_header(h, "content-length")))
    if (length(cl) != 1 || is.na(cl) || cl <= 0) NA_real_ else cl
  }, error = function(e) NA_real_)
}

# Send one ranged GET (`range` is the Range header value) and return
# list(status, total, body). `body` is read only for a 206 answer; any other
# answer is closed unread (see the section comment above).
.http_range_get <- function(url, range, skip_on_api_limit = FALSE) {
  if (!.wait_out_known_rate_limit(url, skip_on_api_limit))
    return(list(status = NA_integer_, total = NA_real_, body = NULL))
  resp <- .mc_request(url) |>
    httr2::req_headers(Range = range) |>
    .auth_for_url() |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                     is_transient = .size_probe_is_transient_factory(skip_on_api_limit),
                     backoff = .storage_backoff,
                     after = .storage_retry_after_factory(skip_on_api_limit)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform_connection()
  on.exit(close(resp), add = TRUE)
  if (.waf_challenge_check(resp, url))
    return(list(status = httr2::resp_status(resp), total = NA_real_, body = NULL))
  status <- httr2::resp_status(resp)
  # A suffix range ("bytes=-n") never covers the whole file on purpose, so a
  # 200 answer here means the host ignores Range (see .range_ignored_record()).
  if (status == 200L && grepl("^bytes=-", range)) .range_ignored_record(url, resp)
  list(status = status,
       total = .content_range_total(resp),
       body = if (status == 206L) httr2::resp_body_raw(resp) else NULL)
}

# ── Hosts that ignore Range ──────────────────────────────────────────────────
# Some hosts answer a ranged GET with 200 and the complete file: PsychArchives'
# /rest/bitstreams/<id>/retrieve (verified live 2026-10-13: "Range: bytes=0-9"
# came back 200 with the full 4644-byte file), the OSF's proxy for files in a
# GitHub add-on, and Zenodo's whole-record files-archive. A peek of such a file
# cannot work, so once a host has shown it, later peeks this session give up
# before sending anything (zip_peek() and .http_range_bytes() check
# .range_ignored_known() first) instead of paying a HEAD and a GET each time.
#
# The record is keyed on the host plus the SHAPE of the path (every path
# segment holding a digit replaced by "*", so ".../bitstreams/<uuid>/retrieve"
# covers every PsychArchives file), not on the host alone: zenodo.org serves
# its per-file /content URLs with ranges honoured and its files-archive
# without, so a host-wide record would stop every later Zenodo peek. For the
# same reason nothing is recorded when the answer came from a DIFFERENT host
# after a redirect: osf.io/download/<id>/ redirects both OSF Storage files
# (ranges honoured) and GitHub add-on files (ranges ignored) to other hosts,
# and the two request URLs look alike, so whether ranges work there is a
# property of the file, not of osf.io. Reset per R session, like
# .zip_peek_cache: a host can change how it behaves.
.range_ignored_hosts <- new.env(parent = emptyenv())

.range_ignored_key <- function(url) {
  u <- tryCatch(httr2::url_parse(url), error = function(e) NULL)
  if (is.null(u) || is.null(u$hostname) || !nzchar(u$hostname)) return(NA_character_)
  segs <- strsplit(u$path %||% "", "/", fixed = TRUE)[[1]]
  segs <- segs[nzchar(segs)]
  segs[grepl("[0-9]", segs)] <- "*"
  paste0(tolower(u$hostname), "/", paste(segs, collapse = "/"))
}

.range_ignored_known <- function(url) {
  key <- .range_ignored_key(url)
  !is.na(key) && exists(key, envir = .range_ignored_hosts, inherits = FALSE)
}

# Record that `url`'s host ignored a range request, unless `resp` came from a
# different host after a redirect (see the section comment above).
.range_ignored_record <- function(url, resp = NULL) {
  key <- .range_ignored_key(url)
  if (is.na(key)) return(invisible(FALSE))
  final <- if (is.null(resp)) url else resp$url %||% url
  host_of <- function(x) tolower(tryCatch(httr2::url_parse(x)$hostname,
                                          error = function(e) NA_character_) %||% NA_character_)
  if (!identical(host_of(url), host_of(final))) return(invisible(FALSE))
  assign(key, TRUE, envir = .range_ignored_hosts)
  invisible(TRUE)
}

# Size of a 200 answer's body as the host reported it (Content-Length), or NA.
.resp_content_length <- function(resp) {
  cl <- suppressWarnings(as.numeric(httr2::resp_header(resp, "content-length")))
  if (length(cl) != 1 || !is.finite(cl) || cl < 0) NA_real_ else cl
}

# Size in bytes from a one-byte ranged GET, or NA. The fallback for when a
# HEAD request gives no usable size (see the section comment above).
.range_size <- function(url, skip_on_api_limit = FALSE) {
  tryCatch({
    r <- .http_range_get(url, "bytes=0-0", skip_on_api_limit = skip_on_api_limit)
    if (r$status %in% c(206L, 416L) && is.finite(r$total) && r$total > 0)
      r$total else NA_real_
  }, error = function(e) NA_real_)
}

# Fetch the last `n` bytes of a URL via an HTTP Range request. Returns the raw
# bytes (possibly fewer than n if the file is smaller), or NULL on failure. The
# file's total size is attached as attr(, "total") (NA when the host did not
# report it), so zip_peek() can size its larger retry even when HEAD gave
# nothing.
#
# A host that ignores the range answers 200 with the whole file. That answer
# is only read when the file is small -- at most max(n, 2 MB), by the known
# size or the answer's own Content-Length -- because the tail is then inside
# it at little cost. Anything larger is closed unread and NULL returned (the
# caller falls back to downloading the archive, as before). This used to go
# through req_perform(), which reads the whole body before the status can be
# looked at: a peek of a multi-gigabyte PsychArchives zip, whose /retrieve
# endpoint ignores Range, transferred the entire file into memory. Sent
# through req_perform_connection() now, like .http_range_get(), so the status
# and headers are read before any of the body.
#
# `total`: the file size if already known; NULL to ask for it with HEAD first;
# NA when HEAD already failed, in which case the tail is requested by its
# distance from the end instead (.http_range_suffix(), issue #424).
#
# Routed through .auth_for_url() (see R/repo-download.R): Dryad answers 401 to
# an unauthenticated request on ANY byte-touching endpoint, including a HEAD
# or Range GET, so a Dryad zip's peek always failed before this (confirmed
# live 2026-09-02, see issue #378) -- every other host .auth_for_url() knows
# about is unaffected, since it only adds headers where the URL matches.
#
# Also retried via req_retry() with the SAME transient/backoff/rate-limit
# helpers download_repo_files() uses for ordinary file downloads
# (.storage_is_transient_factory()/.storage_backoff()/
# .storage_retry_after_factory(), R/repo-download.R) -- without this, a 429
# here was indistinguishable from "host does not support ranges" (both
# silently returned NULL), which is what made issue #384's fetch fail
# non-deterministically: repo_check's own zip_peek() call (discovering member
# names, inst/modules/repo_check.R) and download_repo_files()'s later
# .zip_fetch_members()->zip_peek() call (fetching a member's bytes) hit the
# SAME archive URL twice in one pipeline run with no throttle between them,
# so the second call was the one exposed to a burst-rate-limit refusal.
.http_range_tail <- function(url, n, total = NULL, skip_on_api_limit = FALSE) {
  tryCatch({
    if (is.null(total)) total <- .head_size(url, skip_on_api_limit = skip_on_api_limit)
    if (is.na(total))
      return(.http_range_suffix(url, n, skip_on_api_limit = skip_on_api_limit))
    if (total <= 0) return(NULL)
    start <- max(0, total - n)
    if (!.wait_out_known_rate_limit(url, skip_on_api_limit)) return(NULL)
    r <- .mc_request(url) |>
      httr2::req_headers(Range = sprintf("bytes=%.0f-%.0f", start, total - 1)) |>
      .auth_for_url() |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                       is_transient = .storage_is_transient_factory(skip_on_api_limit),
                       backoff = .storage_backoff,
                       after = .storage_retry_after_factory(skip_on_api_limit)) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform_connection()
    on.exit(close(r), add = TRUE)
    status <- httr2::resp_status(r)
    # 206 = partial content (range honoured). 200 = whole file: only read
    # when it is small (see the comment above).
    body <- NULL
    if (status == 206L) body <- httr2::resp_body_raw(r)
    if (status == 200L) {
      # A range from byte 0 asks for the whole file, which a host may answer
      # with a plain 200 while still honouring other ranges, so only a range
      # starting later shows that the host ignores Range.
      if (start > 0) .range_ignored_record(url, r)
      body_size <- .resp_content_length(r)
      if (is.na(body_size)) body_size <- total
      if (!is.finite(body_size) || body_size > max(n, 2 * 1048576)) return(NULL)
      body <- utils::tail(httr2::resp_body_raw(r), n)
    }
    if (!is.null(body)) attr(body, "total") <- total
    body
  }, error = function(e) NULL)
}

# The tail request for a file of unknown size: "bytes=-n" asks for the last n
# bytes without knowing where they start. Only a 206 answer is accepted (see
# the size section comment above: a host ignoring the range would otherwise
# send the whole file). A 416 means the file is shorter than n on a host that
# refuses an over-long suffix (GitHub raw files do, verified live
# 2026-09-24); its Content-Range still gives the size, so the whole, small
# file is then requested by exact position instead.
.http_range_suffix <- function(url, n, skip_on_api_limit = FALSE) {
  r <- .http_range_get(url, sprintf("bytes=-%.0f", n),
                       skip_on_api_limit = skip_on_api_limit)
  if (r$status == 206L && !is.null(r$body)) {
    body <- r$body
    attr(body, "total") <- r$total
    return(body)
  }
  if (r$status == 416L && is.finite(r$total) && r$total > 0)
    return(.http_range_tail(url, n, total = r$total,
                            skip_on_api_limit = skip_on_api_limit))
  NULL
}

# Fetch an arbitrary byte range [from, to] (0-based, inclusive) from a URL.
#
# Unlike .http_range_tail(), a 200 response is a FAILURE here rather than a
# fallback: the tail helper can still use a whole-file body (the tail is inside
# it), but a caller asking for one member's bytes wants exactly those bytes, and
# accepting the whole archive would silently transfer the gigabytes this is
# meant to avoid. Returns raw bytes, or NULL when the host ignored the range.
#
# Retried via req_retry() for the same reason as .http_range_tail() -- see its
# comment (issue #384). This is the call a member fetch makes for every
# member, or every run of nearby members (.zip_range_plan()), so a fetch
# inside a loop over many members is exactly the back-to-back-request pattern
# a host's burst limit is most likely to catch.
#
# `reason`, if given, is an environment the caller reads afterwards: on any
# failure this writes a one-line explanation into `reason$msg` (a caught error's
# message, or "HTTP <status>", or "short read"), so a caller can tell a
# transient network/HTTP failure apart from a host that plain doesn't support
# range requests, instead of both looking like an identical silent NULL (#429).
#
# Retries any non-206, non-200 status (not just .storage_is_transient()'s
# fixed 403/429/5xx list), up to 3 tries with the same exponential backoff --
# widened after #429: a single-member fetch that came back with some OTHER
# status (a plain 401, a 400, anything not on that list) was accepted as
# final after exactly one try, even though the identical request succeeded
# moments later when reproduced by hand, and nothing distinguishes "this host
# will never honour ranges" from "that one request happened to fail" for an
# unlisted status. 200 is excluded on purpose: it means the host is ignoring
# Range entirely and sending the whole body, a fixed property of the host/URL
# that a retry cannot change, so retrying it would only waste three requests'
# worth of time before giving the same answer. 206 (success) is also excluded,
# since there is nothing to retry.
.range_status_is_transient <- function(resp) {
  !(httr2::resp_status(resp) %in% c(200L, 206L))
}
#
# Sent through req_perform_connection() (see .http_range_tail()'s comment): a
# host that ignores the range answers 200 with the whole archive, which is now
# closed unread instead of being transferred into memory first and then
# discarded. A host seen doing that is remembered (.range_ignored_record()),
# and later requests to it this session fail at once.
#
# `allow_short = TRUE` accepts an answer shorter than asked for when its
# Content-Range shows it stops at the end of the file. A member fetch asks for
# a margin past the member's own bytes (see .zip_range_plan()), and for the
# last member of a small archive that margin can reach past the end of the
# file; a host then sends the bytes up to the end, as RFC 9110 says it should.
.http_range_bytes <- function(url, from, to, skip_on_api_limit = FALSE, reason = NULL,
                              allow_short = FALSE) {
  tryCatch({
    if (!is.finite(from) || !is.finite(to) || from < 0 || to < from) {
      if (!is.null(reason)) reason$msg <- "invalid byte range"
      return(NULL)
    }
    if (.range_ignored_known(url)) {
      if (!is.null(reason)) reason$msg <- "HTTP 200 (range not honoured; host seen ignoring ranges earlier this session)"
      return(NULL)
    }
    # See .http_range_tail()'s own comment on this same call: check the
    # session's rate-limit record before sending, so a host already known
    # rate-limited (recorded by any earlier request this session, including
    # a different archive/member) waits out the remaining time up front.
    if (!.wait_out_known_rate_limit(url, skip_on_api_limit)) {
      if (!is.null(reason)) reason$msg <- "host is rate-limited (waiting skipped by skip_on_api_limit)"
      return(NULL)
    }
    r <- .mc_request(url) |>
      httr2::req_headers(Range = sprintf("bytes=%.0f-%.0f", from, to)) |>
      .auth_for_url() |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                       is_transient = .range_status_is_transient,
                       backoff = .storage_backoff,
                       after = .storage_retry_after_factory(skip_on_api_limit)) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform_connection()
    on.exit(close(r), add = TRUE)
    status <- httr2::resp_status(r)
    # A range from byte 0 that reaches past the end of a small file covers the
    # whole file, which a host may answer with a plain 200 while honouring
    # other ranges. With `allow_short`, that whole (small) file is exactly
    # what a 206 cut at the end of the file would have held.
    if (status == 200L && from == 0 && isTRUE(allow_short)) {
      cl <- .resp_content_length(r)
      if (is.finite(cl) && cl <= to + 1) {
        body <- httr2::resp_body_raw(r)
        if (length(body) == cl) return(body)
      }
    }
    if (status != 206) {
      # Only a range starting past byte 0 shows that the host ignores Range
      # (see .http_range_tail()).
      if (status == 200L && from > 0) .range_ignored_record(url, r)
      if (!is.null(reason)) reason$msg <- sprintf("HTTP %d (range not honoured)", status)
      return(NULL)
    }
    body <- httr2::resp_body_raw(r)
    ends_at_eof <- function() {
      total <- .content_range_total(r)
      is.finite(total) && from + length(body) == total
    }
    if (length(body) > (to - from + 1) ||
        (length(body) < (to - from + 1) && !(isTRUE(allow_short) && ends_at_eof()))) {
      if (!is.null(reason)) reason$msg <- "short read (range request returned the wrong length)"
      return(NULL)
    }
    body
  }, error = function(e) {
    if (!is.null(reason)) reason$msg <- conditionMessage(e)
    NULL
  })
}

# Parse ZIP central-directory entries from a raw tail that ends at the true end
# of the file. Returns a data.frame(name, size) of uncompressed sizes, or NULL if
# the End-Of-Central-Directory record isn't present in the tail.
#
# Also returns the four fields a SINGLE-MEMBER fetch needs, which a plain listing
# does not use: `method` (0 stored / 8 deflate), `csize` (compressed bytes),
# `offset` (where this member's local header starts in the archive), and `crc`
# (the integrity check). They are read from bytes the walk already had to step
# over, so collecting them costs nothing. See .zip_member_fetch().
#
# Zip64. An archive over 4 GB, or with more than 65,535 entries, cannot state
# its central directory's size, offset or entry count in the 2- and 4-byte
# fields of the ordinary end record, so it writes 0xFFFF/0xFFFFFFFF there and
# puts the real values in a Zip64 end record (56 bytes, signature PK\06\06)
# followed by a Zip64 locator (20 bytes, PK\06\07) that sits immediately
# before the ordinary end record. The central directory then ends where the
# Zip64 end record begins, not where the ordinary end record begins, so
# before this was handled the walk started 76 bytes too early, found no entry
# signature, and the archive could not be listed at all. Each entry likewise
# writes 0xFFFFFFFF in any of its uncompressed size, compressed size or local
# header offset that does not fit, and puts the real value in its "Zip64
# extended information" extra field (header ID 0x0001), which holds 8-byte
# values for ONLY the fields that carry the placeholder, always in the order
# uncompressed size, compressed size, offset (then a 4-byte disk number when
# that field is 0xFFFF). Verified 2026-10-13 against the 7.6 GB zip of
# Figshare article 10.6084/m9.figshare.28052888.v1: 867 entries, 591 of whose
# offsets are stored only in that extra field; with both handled, the same
# 128 KB tail lists every entry with a usable offset.
#
# The tail's absolute position in the archive is needed only to find the Zip64
# end record by the offset the locator gives; `raw` carries the archive's total
# size as attr(, "total") when the caller knows it (.http_range_tail() sets
# it). Without it, the record is looked for directly before the locator, where
# every writer puts it.
#
# The result carries attr(, "cd_offset"): where the central directory starts
# in the archive, i.e. where the last member's bytes must end. A member fetch
# uses it to bound its request (see .zip_range_plan()).
#
# With `need_only = TRUE` it returns instead how many bytes from the end of
# the archive hold the whole central directory (as found from the end
# record), or NA when the tail does not show it. zip_peek() uses that to ask
# for exactly the bytes it lacks after a first tail that was too short
# (#473), rather than a fixed 1 MB that may be too much or too little.
.parse_zip_central_dir <- function(raw, need_only = FALSE) {
  n <- length(raw)
  if (n < 22) return(if (need_only) NA_real_ else NULL)
  eocd_sig  <- as.raw(c(0x50, 0x4b, 0x05, 0x06))   # "PK\05\06"
  cd_sig    <- as.raw(c(0x50, 0x4b, 0x01, 0x02))   # "PK\01\02" central-dir header
  loc64_sig <- as.raw(c(0x50, 0x4b, 0x06, 0x07))   # "PK\06\07" Zip64 locator
  rec64_sig <- as.raw(c(0x50, 0x4b, 0x06, 0x06))   # "PK\06\06" Zip64 end record
  sig_at <- function(at, sig) {
    at >= 1 && at + 3 <= n && identical(raw[at:(at + 3)], sig)
  }

  # Find the EOCD signature, scanning from the end (comment may follow it).
  eocd <- NA_integer_
  for (i in (n - 21):1) {
    if (identical(raw[i:(i + 3)], eocd_sig)) { eocd <- i; break }
    if (i < n - 21 - 65536) break   # comment can't exceed 64KB; stop early
  }
  if (is.na(eocd)) return(if (need_only) NA_real_ else NULL)

  # EOCD fields (offsets relative to the signature start):
  #   +10 (2) total entries, +12 (4) central-dir size, +16 (4) central-dir offset
  n_entries <- .le_int(raw, eocd + 10, 2)
  cd_size   <- .le_int(raw, eocd + 12, 4)
  cd_offset <- .le_int(raw, eocd + 16, 4)
  cd_end    <- eocd

  # Zip64 end record, found through the locator right before the EOCD. Its
  # fields (offsets relative to its signature): +32 (8) total entries,
  # +40 (8) central-dir size, +48 (8) central-dir offset.
  loc <- eocd - 20
  if (sig_at(loc, loc64_sig)) {
    rec <- NA_real_
    total <- suppressWarnings(as.numeric(attr(raw, "total") %||% NA_real_))
    if (length(total) == 1 && is.finite(total) && total >= n) {
      at <- .le_int(raw, loc + 8, 8) - (total - n) + 1
      if (sig_at(at, rec64_sig)) rec <- at
    }
    if (is.na(rec) && loc - 56 >= 1) {
      # Directly before the locator (no extensible data), else scan back.
      for (at in (loc - 56):max(1, loc - 56 - 65536)) {
        if (sig_at(at, rec64_sig)) { rec <- at; break }
      }
    }
    if (!is.na(rec)) {
      n_entries <- .le_int(raw, rec + 32, 8)
      cd_size   <- .le_int(raw, rec + 40, 8)
      cd_offset <- .le_int(raw, rec + 48, 8)
      cd_end    <- rec
    } else if (n_entries == 65535 || cd_size == 4294967295) {
      # Zip64 end record not in the tail, and it is needed
      return(if (need_only) NA_real_ else NULL)
    }
    # (Otherwise the four bytes that looked like a locator were part of the
    # last entry, and the ordinary end record's values stand.)
  }

  # The central directory ends right before the (Zip64) end record and is
  # cd_size bytes long, so within our tail it starts at (cd_end - cd_size) when
  # fully present.
  cd_start <- cd_end - cd_size
  # bytes from the central directory's start to the end of the archive
  if (need_only) return(n - cd_start + 1)
  if (cd_start < 1) return(NULL)   # central directory not fully in the tail

  # Preallocated: growing six vectors with c() costs time proportional to the
  # square of the entry count, which a Zip64 archive can put past 65,535. An
  # entry is at least 46 bytes, which bounds the count a corrupt end record
  # could claim.
  n_max <- min(n_entries, floor((cd_end - cd_start) / 46))
  names <- character(n_max); name_lens <- numeric(n_max); sizes <- numeric(n_max)
  methods <- numeric(n_max); csizes <- numeric(n_max)
  offsets <- numeric(n_max); crcs <- numeric(n_max)
  ff4 <- 4294967295
  n_read <- 0
  p <- cd_start
  for (e in seq_len(n_max)) {
    if (p + 46 > n) break
    if (!identical(raw[p:(p + 3)], cd_sig)) break
    method  <- .le_int(raw, p + 10, 2)   # 0 = stored, 8 = deflate
    crc     <- .le_int(raw, p + 16, 4)   # CRC32 of the uncompressed bytes
    csize   <- .le_int(raw, p + 20, 4)   # compressed size
    usize   <- .le_int(raw, p + 24, 4)   # uncompressed size
    name_len <- .le_int(raw, p + 28, 2)
    extra_len <- .le_int(raw, p + 30, 2)
    comm_len  <- .le_int(raw, p + 32, 2)
    disk    <- .le_int(raw, p + 34, 2)
    offset  <- .le_int(raw, p + 42, 4)   # local header offset within the archive
    if (p + 46 + name_len + extra_len - 1 > n) break   # entry runs past the tail
    nm <- if (name_len > 0) rawToChar(raw[(p + 46):(p + 46 + name_len - 1)]) else ""
    # A name is UTF-8 when general-purpose flag bit 11 says so; without it the
    # zip standard's default is CP437 (old Windows zip tools). Such a name is
    # not valid UTF-8 as soon as it has an accent, and every later sub()/gsub()
    # on it stopped data_check with "invalid multibyte string" (U76). Every
    # byte is a CP437 character, so the conversion cannot fail; a name marked
    # UTF-8 that is not gets its bad bytes read as Latin-1 (.utf8_lines()).
    utf8_flag <- bitwAnd(.le_int(raw, p + 8, 2), 2048L) != 0
    if (!validUTF8(nm)) {
      nm <- if (utf8_flag) .utf8_lines(nm) else iconv(nm, "CP437", "UTF-8")
    }
    Encoding(nm) <- "UTF-8"
    if (extra_len > 0 &&
        (usize == ff4 || csize == ff4 || offset == ff4 || disk == 65535)) {
      z <- .zip64_extra(raw, p + 46 + name_len, extra_len,
                        need = c(usize == ff4, csize == ff4, offset == ff4))
      if (!is.null(z)) {
        if (usize == ff4 && !is.na(z[1]))  usize  <- z[1]
        if (csize == ff4 && !is.na(z[2]))  csize  <- z[2]
        if (offset == ff4 && !is.na(z[3])) offset <- z[3]
      }
    }
    n_read <- n_read + 1
    names[n_read] <- nm; name_lens[n_read] <- name_len; sizes[n_read] <- usize
    methods[n_read] <- method; csizes[n_read] <- csize
    offsets[n_read] <- offset; crcs[n_read] <- crc
    p <- p + 46 + name_len + extra_len + comm_len
  }
  if (n_read == 0) return(NULL)
  keep <- seq_len(n_read)
  names <- names[keep]; name_lens <- name_lens[keep]
  sizes <- sizes[keep]; methods <- methods[keep]
  csizes <- csizes[keep]; offsets <- offsets[keep]; crcs <- crcs[keep]
  # A placeholder with no Zip64 extra field to resolve it (a malformed entry)
  # must not be read as a real size or byte offset, so it is marked NA and
  # .zip_member_fetch() refuses it. Listing still works: the names are correct
  # and that is what the peek is for.
  sizes[sizes == ff4] <- NA_real_
  csizes[csizes == ff4] <- NA_real_
  offsets[offsets == ff4] <- NA_real_
  # name_bytes: the name's length as stored, which a converted (CP437) name no
  # longer has; .zip_range_plan() and .zip_whole_size() need the stored length
  out <- data.frame(name = names, size = sizes, method = methods, csize = csizes,
                    offset = offsets, crc = crcs, name_bytes = name_lens,
                    stringsAsFactors = FALSE)
  attr(out, "cd_offset") <- if (cd_offset == ff4) NA_real_ else cd_offset
  out
}

# Read the Zip64 extended-information extra field (header ID 0x0001) from an
# entry's extra block of `len` bytes starting at `at`. `need` says which of
# uncompressed size, compressed size and local header offset hold the
# placeholder in the fixed fields; only those are present, in that order (see
# .parse_zip_central_dir()). Returns a numeric(3) with NA for a field not
# needed or not present, or NULL when the block holds no Zip64 field.
.zip64_extra <- function(raw, at, len, need) {
  end <- at + len - 1
  q <- at
  while (q + 3 <= end) {
    id <- .le_int(raw, q, 2)
    sz <- .le_int(raw, q + 2, 2)
    if (q + 3 + sz > end) break   # field claims more bytes than the block has
    if (id == 1) {
      vals <- rep(NA_real_, 3)
      pos <- q + 4
      for (k in which(need)) {
        if (pos + 7 > q + 3 + sz) break
        vals[k] <- .le_int(raw, pos, 8)
        pos <- pos + 8
      }
      return(vals)
    }
    q <- q + 4 + sz
  }
  NULL
}


#' Peek at the contents of a remote ZIP file without downloading it
#'
#' Uses an HTTP range request to fetch only the tail of a ZIP archive and read
#' its central directory, returning the name and uncompressed size of every entry
#' inside. This lets a downloader decide whether a zip is worth fetching (does it
#' contain data, or only stimuli/assets?) before spending the bandwidth.
#'
#' @param url the download URL of a ZIP file, or the path of a ZIP file
#'   already on disk (e.g. one inside a downloaded ResearchBox), which is then
#'   read directly without any request
#' @param tail_bytes how many bytes of the tail to fetch (default 128 KB; raised
#'   automatically on retry when the central directory is larger)
#' @param cache if `TRUE`, persist this URL's result to disk (see
#'   [zip_peek_cache_clear()]) so a later R process reuses it instead of
#'   re-peeking the archive over HTTP. Off by default, like the
#'   repository-listing cache (see [repo_info_cache_clear()]); a caller that never asks for
#'   caching sees no behavior change. `repo_check()`/`data_check()` forward
#'   their own `cache` argument here.
#' @param skip_on_api_limit if `TRUE`, a confirmed exhausted rate-limit bucket
#'   during a peek is treated as a failure (returns `NULL`) instead of waiting
#'   out the host's reset -- see [download_repo_files()]'s parameter of the
#'   same name. Falls back to `getOption("metacheck.skip_on_api_limit")` when
#'   not given directly.
#' @param size the archive's size in bytes, when the caller already knows it
#'   (e.g. from a repository's file listing). Skips the HEAD request otherwise
#'   made to learn it -- on the OSF's file server every request costs several
#'   seconds, however small the file. `NULL` or `NA` (the default) asks the
#'   host as before.
#'
#' @returns a data.frame with columns `name` (entry path inside the zip) and
#'   `size` (uncompressed bytes), excluding directory entries; or `NULL` when the
#'   host does not support range requests or the listing cannot be read (the
#'   caller should then download-and-inspect). Also returns `method`, `csize`,
#'   `offset`, `crc` and `name_bytes`, which describe where each member's bytes
#'   sit in the archive so a single member can be fetched without the rest.
#'   Names without the zip's UTF-8 flag are read as CP437 (old Windows zip
#'   tools) and returned in UTF-8; `name_bytes` is the name's stored length.
#'   Zip64 archives
#'   (over 4 GB or over 65,535 entries) are read in full, including the sizes
#'   and offsets stored only in their Zip64 extra fields; `csize` and `offset`
#'   are `NA` only for an entry whose listing is malformed.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' zip_peek("https://osf.io/download/abcde/")
#' }
zip_peek <- function(url, tail_bytes = 131072, cache = FALSE,
                     skip_on_api_limit = FALSE, size = NULL) {
  # Reuse an earlier peek of this exact URL from this same session -- SUCCESS
  # OR FAILURE. Originally only a success was cached, on the assumption a
  # refused/unsupported NULL "would still return NULL again quickly" -- true
  # for a host that plainly does not honour ranges (a fast, clear refusal),
  # but confirmed live 2026-09-09 to be false when the host instead answers
  # every call with a retryable-looking status (403/429/5xx) persistently:
  # a single zip_peek() call already pays req_retry()'s full backoff
  # sequence (up to 3 tries each, exponential up to a 30s cap, across up to
  # 3 separate HTTP calls this function and its helpers make) before giving
  # up, and this file's own header comment documents that the SAME url is
  # peeked more than once per pipeline run (once in repo_check, again in
  # download_repo_files()'s .zip_fetch_members()) -- each call independently
  # paying that cost again compounds into a multi-hour stall for one
  # persistently-403ing archive. exists(...) (not mget's ifnotfound), so a
  # cached NULL is distinguishable from "never attempted" -- assign(url,
  # NULL, envir=...) really does bind NULL in an environment (unlike
  # env$x <- NULL on a list, which removes the element); confirmed live.
  if (exists(url, envir = .zip_peek_cache, inherits = FALSE)) {
    return(get(url, envir = .zip_peek_cache, inherits = FALSE))
  }

  # A zip already on disk is listed from the file itself. ResearchBox rows
  # carry the box's web page as file_url and the downloaded copy as
  # file_location, so a zip inside a box used to be peeked at the web page --
  # which answers a Range request with 406 (verified 2026-10-13), costing about
  # 0.8 s per inner zip for nothing. Not cached: a local file can change.
  if (.is_local_file(url)) return(.zip_list_local(url))

  # A host already seen ignoring ranges this session (see
  # .range_ignored_record()) would answer this peek with the whole file too,
  # so give up before sending the HEAD and GET.
  if (.range_ignored_known(url)) {
    assign(url, NULL, envir = .zip_peek_cache)
    return(NULL)
  }

  # On-disk cache, one level further out than the in-memory one above: reused
  # across R processes, not just within one (issue #427). Gated on `cache`
  # (default FALSE) so an uncached caller's behavior is unchanged. A hit here
  # is also written into the in-memory cache, so a second call THIS session
  # (e.g. repo_check's peek followed by download_repo_files()'s) skips even
  # the disk read.
  if (isTRUE(cache) && .zip_peek_cache_has(url)) {
    cd <- .zip_peek_cache_get(url)
    assign(url, cd, envir = .zip_peek_cache)
    return(cd)
  }

  # HEAD once for the total size (also lets us grab a bigger tail if needed).
  # When HEAD gives none (S3-backed hosts refuse it, issue #424), total stays
  # NA: the first tail is then requested by distance from the end, and its
  # Content-Range supplies the size for the 1 MB retry.
  #
  # A size the caller already has (from the repository's own file listing)
  # replaces the HEAD request. That matters most on the OSF: its file server
  # took 2 to 20 seconds to answer each request for a few-KB zip (measured
  # 2026-10-13), so the HEAD alone could cost as much as the peek itself. With
  # the size known, a zip no larger than the tail is fetched whole by one
  # ranged GET (bytes=0-<size-1>), which .http_range_tail() accepts as a 206
  # or a plain 200 alike.
  known <- suppressWarnings(as.numeric(size %||% NA_real_))
  total <- if (length(known) == 1 && is.finite(known) && known > 0) known else
    .head_size(url, skip_on_api_limit = skip_on_api_limit)

  # Two tries at most. When the first tail is too short for the central
  # directory, its end record says how many bytes the directory needs, and
  # the second request asks for exactly those (#473): for Zenodo's
  # foxnet-v2.0.1.zip (a 292 KB directory, range requests at ~7 KB/s) that is
  # 293 KB instead of 1 MB, and a directory over 1 MB, which a fixed 1 MB
  # retry could never list, is read too (up to 64 MB, against a damaged end
  # record). Without an end record in the tail, the retry is 1 MB as before.
  nb <- tail_bytes
  for (attempt in 1:2) {
    raw <- .http_range_tail(url, nb, total = total,
                            skip_on_api_limit = skip_on_api_limit)
    if (is.null(raw)) {
      # kept for this session only: a failed request (a connection failure, a
      # rate limit, a server error) can pass, and the disk cache outlives the
      # session, so the archive was not listed again until the cache was
      # cleared (U210)
      assign(url, NULL, envir = .zip_peek_cache)
      return(NULL)
    }
    if (is.na(total)) total <- attr(raw, "total") %||% NA_real_
    cd <- .parse_zip_central_dir(raw)
    if (!is.null(cd)) {
      cd <- .zip_drop_dirs(cd)
      assign(url, cd, envir = .zip_peek_cache)
      if (isTRUE(cache)) .zip_peek_cache_put(url, cd)
      return(cd)
    }
    if (!is.null(total) && !is.na(total) && nb >= total) break  # whole file seen
    need <- .parse_zip_central_dir(raw, need_only = TRUE)
    nb_next <- if (isTRUE(need > length(raw))) min(need + 1024, 64 * 1048576) else 1048576
    if (nb_next <= nb) break   # asking again for the same bytes cannot help
    nb <- nb_next
  }
  assign(url, NULL, envir = .zip_peek_cache)
  if (isTRUE(cache)) .zip_peek_cache_put(url, NULL)
  NULL
}

# Drop directory entries from a parsed listing, keeping its "cd_offset"
# attribute (which subsetting a data.frame would discard).
.zip_drop_dirs <- function(cd) {
  out <- cd[!grepl("/$", cd$name), , drop = FALSE]
  attr(out, "cd_offset") <- attr(cd, "cd_offset")
  out
}

# TRUE when `x` names an existing local file rather than a URL. A URL scheme
# ("https://", "file://") is never treated as a path, so a file:// URL still
# goes through the HTTP code like any other URL.
.is_local_file <- function(x) {
  length(x) == 1 && !is.na(x) && nzchar(x) &&
    !grepl("^[A-Za-z][A-Za-z0-9+.-]*://", x) &&
    file.exists(x) && !dir.exists(x)
}

# For each row of a file table, the zip's local copy when there is one
# (file_location names an existing file), else NA. A row with a local copy is
# listed and read from disk (zip_peek() and .zip_fetch_members() accept a
# path) rather than through its file_url. Needed for ResearchBox, whose rows
# carry the box's web page as file_url and the copy unpacked from the box's
# zip as file_location: a peek at the web page fails (406, ~0.8 s each,
# verified 2026-10-13).
.zip_local_source <- function(files) {
  loc <- files$file_location %||% rep(NA_character_, nrow(files))
  vapply(loc, function(p) if (.is_local_file(p)) p else NA_character_,
         character(1), USE.NAMES = FALSE)
}

# List a zip that is already on disk, with the same columns zip_peek() returns
# for a remote one: the central directory is read from the file's last 1 MB by
# the same parser. A central directory larger than that falls back to
# utils::unzip(list = TRUE), which gives names and sizes only (method, csize,
# offset and crc NA: they are not needed, since a local member is extracted
# with unzip rather than fetched by byte range). NULL when the file is not a
# readable zip.
.zip_list_local <- function(path, tail_bytes = 1048576) {
  total <- file.size(path)
  if (is.na(total) || total < 22) return(NULL)
  n <- min(total, tail_bytes)
  raw <- tryCatch({
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    seek(con, total - n)
    readBin(con, "raw", n)
  }, error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  attr(raw, "total") <- total
  cd <- .parse_zip_central_dir(raw)
  if (is.null(cd)) {
    lst <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
    if (is.null(lst) || nrow(lst) == 0) return(NULL)
    cd <- data.frame(name = lst$Name, size = as.numeric(lst$Length),
                     method = NA_real_, csize = NA_real_, offset = NA_real_,
                     crc = NA_real_, stringsAsFactors = FALSE)
  }
  .zip_drop_dirs(cd)
}

# ── Single-member extraction ─────────────────────────────────────────────────
# Having listed an archive with zip_peek(), fetch ONE member's bytes without
# transferring the rest. Each member of a standard zip is compressed on its own,
# so its bytes are a self-contained stream that can be read in isolation -- which
# is what makes this possible for .zip and impossible for .7z or .tar.gz, where
# one stream spans every file. That is the practical reason repo_check tells
# authors to archive as .zip.
#
# Verified 2026-08-13 against Zenodo record 13384475: one 2.06 MB member was
# recovered from a 129.8 MB archive in 1.2 seconds, transferring 2.06 MB, and its
# CRC32 matched the value stored in the archive's own central directory.
#
# Hosts behave differently here. A Zenodo per-file URL
# (/api/records/<id>/files/<name>/content) answers 206 with the requested bytes,
# and so does an OSF file in OSF storage (osf.io/download/<id>/, which redirects
# to a signed storage URL; verified 2026-10-13 -- an earlier note here said the
# OSF refused ranges, which is no longer true). A file in an add-on linked to
# an OSF project (e.g. GitHub, proxied by the OSF) and Zenodo's whole-record
# `files-archive` endpoint ignore the Range header and return the complete
# body, so neither can be read this way -- .http_range_bytes() treats that 200
# as a failure and the caller falls back to downloading the archive.

# Decompress one member's raw bytes.
#
# `zip::inflate()` expects a ZLIB stream, but a zip member stores BARE deflate
# with no zlib wrapper, so passing the member's bytes directly fails with "Input
# data is invalid". Prepending the two-byte zlib header (0x78 0x01) makes it
# readable.
#
# `size` (the member's exact uncompressed size, from the archive's own central
# directory) must NOT be passed as-is: `zip::inflate(..., size = uncompressed_size)`
# fails outright with "Failed to inflate data" for reasons internal to the zip
# package. Leaving `size` unset does not fail, but is worse: its documented
# "resize the output buffer multiple times" behaviour does not actually happen
# in practice, so any member over the default 32768-byte buffer is silently
# TRUNCATED at exactly 32768 bytes with no error, no warning, and a
# `bytes_written` field callers never inspect (confirmed live 2026-09-05,
# issue #384 -- e.g. a real 44669-byte code file came back as exactly 32768
# bytes). `.zip_member_fetch()`'s own length check below does catch the
# resulting mismatch and discard the corrupt bytes, but the practical effect
# was that every code/data file over 32KB inside a zip silently failed to
# fetch, indistinguishable from a genuine network or host-compatibility
# failure -- accounting for the large majority of issue #384's disagreements.
# Passing `size + 1` (one byte over the true size) avoids both failure modes:
# it is no longer the exact size that triggers the "Failed to inflate data"
# error, and it is large enough that the real output always fits in the
# up-front buffer instead of depending on the broken resize path. Verified
# against both a >32KB and a <32KB real Zenodo zip member.
#
# Returns raw bytes, or NULL when the member cannot be decompressed.
.zip_inflate_member <- function(comp, method, size = NA_real_) {
  if (method == 0) return(comp)          # stored: no compression, no dependency
  if (method != 8) return(NULL)          # only deflate is supported
  if (!requireNamespace("zip", quietly = TRUE)) return(NULL)
  size_hint <- if (!is.na(size) && size >= 0) size + 1 else NULL
  tryCatch({
    res <- zip::inflate(c(as.raw(c(0x78, 0x01)), comp), size = size_hint)
    as.raw(res$output %||% res)
  }, error = function(e) NULL)
}

# Fetch a single member of a remote zip, given its row from zip_peek().
#
# The member's compressed bytes do NOT start at `offset`: that points at the
# local file header, whose length varies with the name and extra fields it
# carries. The extra field there is commonly a DIFFERENT length from the central
# directory's copy (28 vs 0 bytes in the archive this was verified against), so
# the local header must be read to locate the data rather than computing the
# position from the central directory.
#
# One range request per member, not two. This used to ask for the 30-byte
# local header first and then for the data, so every member cost two
# round trips. It now asks for the header, the name, the compressed data and
# a margin of `margin` bytes (1 KB) at once (.zip_range_plan()), and reads the
# header from the start of that answer; the margin covers a local extra field
# longer than the central one (28 bytes in the case above). Only when the
# local extra field is longer than the margin is a second request made, for
# the bytes still missing (.zip_member_from_buffer()).
#
# Returns raw bytes of the decompressed member, or NULL on any failure.
#
# `reason`, if given, is an environment the caller reads afterwards: on any
# failure this writes a one-line explanation into `reason$msg`, so a caller can
# record WHY a member was never downloaded instead of a bare, indistinguishable
# NULL (#429) -- e.g. a transient range-request failure (worth a retry) vs. a
# corrupt local header or a CRC mismatch (retrying won't help).
#
# `cd`, if given, is the archive's whole listing (from zip_peek()), used only
# to bound the request at the next member's local header (see
# .zip_range_plan()).
.zip_member_fetch <- function(url, entry, verify = TRUE, skip_on_api_limit = FALSE,
                              reason = NULL, cd = NULL) {
  if (is.null(entry) || nrow(entry) != 1) {
    if (!is.null(reason)) reason$msg <- "invalid archive entry"
    return(NULL)
  }
  if (is.na(entry$offset) || is.na(entry$csize)) {
    if (!is.null(reason))
      reason$msg <- "Zip64 entry whose size/offset could not be read from the archive's listing"
    return(NULL)
  }
  if (entry$csize == 0) return(raw(0))                         # empty member

  g <- .zip_range_plan(entry, cd %||% entry)[[1]]
  buf <- .http_range_bytes(url, g$from, g$to, skip_on_api_limit = skip_on_api_limit,
                           reason = reason, allow_short = TRUE)
  if (is.null(buf)) return(NULL)
  .zip_member_from_buffer(url, entry, buf, g$from, verify = verify,
                          skip_on_api_limit = skip_on_api_limit, reason = reason)
}

# Byte ranges to request for the rows of `want` (from zip_peek()), as a list of
# list(rows, from, to): `rows` the positions in `want` whose bytes lie in
# [from, to] (0-based, inclusive).
#
# Each member needs its 30-byte local header, its name, its compressed data,
# and the local extra field, whose length the central directory does not give
# (`margin` bytes are allowed for it, see .zip_member_fetch()). The range is cut
# off at the next local header or, for the last member, at the start of the
# central directory (attr(cd, "cd_offset")): a member's bytes cannot run past
# either, so the margin never asks for bytes that belong to something else.
#
# Members close together share one request. Harvard Dataverse
# doi:10.7910/DVN/SVVULN holds the 7 code files code_check wants inside one
# 23 KB stretch of its zip, which cost 14 requests with two per member
# (measured 2026-10-13). Members are merged when the gap between them is at
# most `max_gap` (64 KB: transferring that is cheaper than a round trip) and
# the merged range stays within `max_span` (16 MB, which bounds the memory one
# request's answer takes); a single member larger than `max_span` still gets
# a request of its own, exactly as before.
.zip_range_plan <- function(want, cd = want, margin = 1024, max_gap = 65536,
                            max_span = 16 * 1048576) {
  if (is.null(want) || nrow(want) == 0) return(list())
  name_bytes <- want$name_bytes %||% nchar(want$name, type = "bytes")
  from <- want$offset
  need_to <- from + 30 + name_bytes + want$csize - 1   # without the extra field
  to <- need_to + margin
  bounds <- sort(unique(stats::na.omit(c(cd$offset, attr(cd, "cd_offset")))))
  if (length(bounds) > 0) {
    nxt <- bounds[findInterval(from, bounds) + 1]   # first bound after `from`
    cut <- !is.na(nxt) & nxt - 1 < to
    to[cut] <- pmax(need_to[cut], nxt[cut] - 1)
  }

  ord <- order(from)
  plan <- list()
  cur <- NULL
  for (i in ord) {
    if (!is.null(cur) && from[i] - cur$to - 1 <= max_gap &&
        max(cur$to, to[i]) - cur$from + 1 <= max_span) {
      cur$rows <- c(cur$rows, i)
      cur$to <- max(cur$to, to[i])
    } else {
      if (!is.null(cur)) plan[[length(plan) + 1L]] <- cur
      cur <- list(rows = i, from = from[i], to = to[i])
    }
  }
  plan[[length(plan) + 1L]] <- cur
  plan
}

# Read one member out of `buf`, the bytes of the archive from position
# `buf_from` (0-based) on, as fetched for its range (.zip_range_plan()). Parses
# the local header found at the member's offset, fetches whatever part of the
# data `buf` does not hold (a local extra field longer than the margin, or a
# range cut short), then decompresses and checks the result against the size
# and CRC32 in the archive's own listing. Returns raw bytes or NULL, writing
# the reason into `reason$msg` like .zip_member_fetch().
.zip_member_from_buffer <- function(url, entry, buf, buf_from, verify = TRUE,
                                    skip_on_api_limit = FALSE, reason = NULL) {
  pos <- entry$offset - buf_from + 1          # 1-based index of the local header
  if (pos < 1 || pos + 29 > length(buf)) {
    if (!is.null(reason)) reason$msg <- "local file header not in the fetched range"
    return(NULL)
  }
  if (!identical(buf[pos:(pos + 3)], as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
    if (!is.null(reason)) reason$msg <- "local file header signature mismatch"
    return(NULL)
  }
  data_from <- pos + 30 + .le_int(buf, pos + 26, 2) + .le_int(buf, pos + 28, 2)
  data_to <- data_from + entry$csize - 1
  if (data_to <= length(buf)) {
    comp <- buf[data_from:data_to]
  } else {
    have <- if (data_from <= length(buf)) buf[data_from:length(buf)] else raw(0)
    miss_from <- max(data_from, length(buf) + 1)
    rest <- .http_range_bytes(url, buf_from + miss_from - 1, buf_from + data_to - 1,
                              skip_on_api_limit = skip_on_api_limit, reason = reason)
    if (is.null(rest)) return(NULL)
    comp <- c(have, rest)
  }

  out <- .zip_inflate_member(comp, entry$method, size = entry$size)
  if (is.null(out)) {
    if (!is.null(reason))
      reason$msg <- if (entry$method != 8 && entry$method != 0)
        sprintf("unsupported compression method (%d)", entry$method)
      else "decompression failed"
    return(NULL)
  }

  # The uncompressed size and CRC32 come from the archive itself, so they check
  # the bytes against what the author stored rather than against our own guess.
  # A CRC that was not computed returns NA, which must not be read as failure:
  # only an actual mismatch (FALSE) rejects the bytes.
  if (!is.na(entry$size) && length(out) != entry$size) {
    if (!is.null(reason)) reason$msg <- "decompressed size did not match the archive's record"
    return(NULL)
  }
  if (isTRUE(verify) && isFALSE(.zip_crc_ok(out, entry$crc))) {
    if (!is.null(reason)) reason$msg <- "CRC32 mismatch (corrupt download)"
    return(NULL)
  }
  out
}

# CRC32 of a raw vector, in base R (no dependency).
#
# Standard CRC32 with the reversed polynomial 0xEDB88320, which as a signed
# 32-bit integer is -306674912. Validated against digest::digest(algo="crc32")
# on random inputs and against the conventional check value: CRC32("123456789")
# is cbf43926.
#
# This is a per-byte loop in R and costs roughly a second per megabyte, so
# .zip_crc_ok() only reaches for it on small members; the C implementation in
# `digest` is preferred whenever that package happens to be installed.
.crc32 <- function(bytes) {
  tab <- vapply(0:255, function(n) {
    cc <- n
    for (k in 1:8)
      cc <- if (bitwAnd(cc, 1L)) bitwXor(-306674912L, bitwShiftR(cc, 1))
            else bitwShiftR(cc, 1)
    cc
  }, integer(1))
  crc <- -1L
  for (b in as.integer(bytes))
    crc <- bitwXor(tab[bitwAnd(bitwXor(crc, b), 255L) + 1L], bitwShiftR(crc, 8))
  crc <- bitwXor(crc, -1L)
  if (crc < 0) crc + 2^32 else as.numeric(crc)   # CRC32 is unsigned
}

# Compare raw bytes against a CRC32 taken from a zip's central directory.
#
# Returns TRUE/FALSE, or NA when the check was not performed -- so a caller can
# tell "not checked" from "failed". A member is only hashed in base R below
# `max_slow_bytes` (1 MB, about a second), because hashing a large member in an
# R loop would cost far more than the transfer this whole approach exists to
# avoid. `digest` has no such limit and is used whenever it is available.
.zip_crc_ok <- function(bytes, crc, max_slow_bytes = 1048576) {
  if (is.na(crc)) return(NA)
  got <- if (requireNamespace("digest", quietly = TRUE)) {
    h <- tryCatch(digest::digest(bytes, algo = "crc32", serialize = FALSE),
                  error = function(e) NA_character_)
    if (is.na(h)) return(NA)
    # NOT strtoi(): it yields an R integer, so any CRC above 2147483647 -- about
    # half of all values -- comes back NA and a correct file looks corrupt.
    # Parse the 8 hex digits into a double instead.
    sum(strtoi(substring(h, seq(1, 7, 2), seq(2, 8, 2)), 16L) * 256^(3:0))
  } else if (length(bytes) <= max_slow_bytes) {
    .crc32(bytes)
  } else {
    return(NA)
  }
  # an exact comparison: both are whole numbers, and all.equal()'s tolerance
  # accepted a CRC up to about 60 off for large values (U70)
  isTRUE(as.numeric(got) == as.numeric(crc))
}

# Download selected members of a remote zip into `dest`, skipping the rest.
#
# `names` are entry paths as zip_peek() reports them; when NULL every member is
# fetched. Files are written under `dest` following their path inside the
# archive. Returns a data.frame of what was fetched (name, path on disk, bytes,
# ok, error), or NULL when the archive could not be listed at all -- which is
# the signal for the caller to fall back to downloading the whole thing.
#
# `error` is NA for a member fetched successfully (ok == TRUE) and otherwise a
# one-line reason it wasn't -- a range request that failed or was refused, a
# corrupt local header, a CRC mismatch, a path-traversal rejection, or a write
# failure. Without this a row simply had ok == FALSE with nothing to say why,
# so a transient failure worth retrying and a host that will never support
# this looked identical after the fact (#429).
#
# Members are fetched by byte range, one request per member or per run of
# nearby members (.zip_range_plan()).
#
# Whole-archive mode (issue #471): downloading the archive once and extracting
# the wanted members locally costs one request, against one per planned range.
# It is used when at least `min_whole_members` members are wanted, the
# archive's own size (estimated from its central directory, see
# .zip_whole_size()) is at most `max_whole_bytes`, and it is the cheaper of
# the two, counting each request as `request_bytes` bytes of transfer:
#   request_bytes + archive size  <=  ranges * request_bytes + bytes in ranges.
# `request_bytes` defaults to 1 MB: a range request cost about 0.6 s on
# Figshare (38 requests in 23 s for data_check on article 33065816), about as
# long as transferring 1 MB from these hosts (that article's whole 1.3 MB zip
# came in one request in about 1 s; measured 2026-10-13). So a small archive
# with many wanted members is downloaded whole, and a large archive from which
# a few small members are wanted is not. `request_bytes = Inf` counts requests
# only, so the archive is taken whenever it fits the budget: Dryad's callers
# pass that, because every request there counts against a strict daily quota
# (see download_repo_files()). Off by default (`max_whole_bytes = 0`): the
# caller decides, because the archive-*.R platform callers chose member
# fetching precisely to avoid transferring an archive that is mostly material.
# `archive_path`, when given, is where the downloaded archive is kept, so a
# restarted run reuses it instead of requesting it again; when NULL it is a
# temporary file removed on exit. Any member the whole-archive step could not
# deliver (download failed, rate limit, extraction failed) falls through to
# the byte-range fetch below, so this never does worse than before.
#
# `url` may also be the path of a zip already on disk (a ResearchBox box is
# downloaded and unpacked whole by repo_check, so a zip inside it is local):
# the wanted members are then extracted from it directly, with no request.
#
# `size`, the archive's size when the caller knows it, is passed to
# zip_peek() to save its HEAD request.
.zip_fetch_members <- function(url, names = NULL, dest, verify = TRUE,
                               cache = FALSE, skip_on_api_limit = FALSE,
                               max_whole_bytes = 0, archive_path = NULL,
                               min_whole_members = 3, request_bytes = 1048576,
                               size = NULL) {
  cd <- tryCatch(zip_peek(url, cache = cache, skip_on_api_limit = skip_on_api_limit,
                          size = size),
                 error = function(e) NULL)
  if (is.null(cd) || nrow(cd) == 0) return(NULL)

  want <- if (is.null(names)) cd else cd[cd$name %in% names, , drop = FALSE]
  if (nrow(want) == 0) return(want[, c("name", "size"), drop = FALSE])

  out_path <- rep(NA_character_, nrow(want))
  ok <- rep(FALSE, nrow(want))
  error <- rep(NA_character_, nrow(want))
  targets <- vapply(want$name, .zip_member_target, character(1), dest = dest,
                    USE.NAMES = FALSE)
  error[is.na(targets)] <- "entry path escapes the archive (path traversal)"

  if (.is_local_file(url)) {
    got <- tryCatch(.zip_extract_members(url, want, targets),
                    error = function(e) rep(NA_character_, nrow(want)))
    ok <- !is.na(got)
    out_path[ok] <- got[ok]
    error[!ok & !is.na(targets)] <- "could not extract the member from the local archive"
    return(data.frame(name = want$name, path = out_path, size = want$size, ok = ok,
                      error = error, stringsAsFactors = FALSE))
  }

  # Members with nothing to fetch (empty), or whose position is unknown, are
  # handled one by one below; the rest are planned as byte ranges.
  rangeable <- function(idx) idx[!is.na(targets[idx]) & !is.na(want$offset[idx]) &
                                   !is.na(want$csize[idx]) & want$csize[idx] > 0]
  todo <- rangeable(seq_len(nrow(want)))
  plan <- .zip_range_plan(want[todo, , drop = FALSE], cd)

  if (.zip_whole_is_cheaper(cd, want, plan, max_whole_bytes, min_whole_members,
                            request_bytes)) {
    whole <- tryCatch(
      .zip_fetch_whole(url, cd, want, targets, max_bytes = max_whole_bytes,
                       archive_path = archive_path,
                       skip_on_api_limit = skip_on_api_limit),
      error = function(e) NULL)
    if (!is.null(whole)) {
      ok <- !is.na(whole)
      out_path[ok] <- whole[ok]
    }
  }

  write_member <- function(i, bytes) {
    target <- targets[i]
    dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
    written <- tryCatch({ writeBin(bytes, target); TRUE },
                        error = function(e) FALSE)
    if (!written) {
      error[i] <<- "could not write the extracted member to disk"
      return(invisible())
    }
    out_path[i] <<- target
    ok[i] <<- TRUE
    error[i] <<- NA_character_
  }

  # Byte ranges for whatever the whole archive did not deliver.
  todo <- rangeable(which(!ok))
  for (g in .zip_range_plan(want[todo, , drop = FALSE], cd)) {
    reason <- new.env(parent = emptyenv())
    buf <- .http_range_bytes(url, g$from, g$to, skip_on_api_limit = skip_on_api_limit,
                             reason = reason, allow_short = TRUE)
    for (i in todo[g$rows]) {
      if (is.null(buf)) {
        error[i] <- reason$msg %||% "unknown failure"
        next
      }
      r_i <- new.env(parent = emptyenv())
      bytes <- .zip_member_from_buffer(url, want[i, , drop = FALSE], buf, g$from,
                                       verify = verify,
                                       skip_on_api_limit = skip_on_api_limit,
                                       reason = r_i)
      if (is.null(bytes)) {
        error[i] <- r_i$msg %||% "unknown failure"
        next
      }
      write_member(i, bytes)
    }
  }

  # The rest: empty members (no request) and members whose position the
  # listing does not give (.zip_member_fetch() records why).
  for (i in which(!ok & !is.na(targets))) {
    if (i %in% todo) next   # already tried by range above
    reason <- new.env(parent = emptyenv())
    bytes <- .zip_member_fetch(url, want[i, , drop = FALSE], verify = verify,
                               skip_on_api_limit = skip_on_api_limit,
                               reason = reason)
    if (is.null(bytes)) {
      error[i] <- reason$msg %||% "unknown failure"
      next
    }
    write_member(i, bytes)
  }
  data.frame(name = want$name, path = out_path, size = want$size, ok = ok,
             error = error, stringsAsFactors = FALSE)
}

# Whether downloading the whole archive is cheaper than fetching `plan`'s
# byte ranges; see .zip_fetch_members() for the rule and its measurements.
.zip_whole_is_cheaper <- function(cd, want, plan, max_whole_bytes,
                                  min_whole_members = 3, request_bytes = 1048576) {
  if (nrow(want) < min_whole_members || !isTRUE(max_whole_bytes > 0)) return(FALSE)
  size <- .zip_whole_size(cd)
  if (is.na(size) || size > max_whole_bytes) return(FALSE)
  if (is.infinite(request_bytes)) return(TRUE)
  ranged_bytes <- sum(vapply(plan, \(g) g$to - g$from + 1, numeric(1)))
  request_bytes + size <= length(plan) * request_bytes + ranged_bytes
}

# Where a member named `name` is written under `dest`, or NA when its path would
# escape `dest`. Entry paths come from the archive, so they are constrained to
# sit under dest: a member named "../secret" or "/etc/x" would otherwise write
# outside the target directory when the archive is hostile or simply malformed.
.zip_member_target <- function(name, dest) {
  rel <- gsub("\\\\", "/", name)
  rel <- sub("^([A-Za-z]:)?/+", "", rel)
  if (any(strsplit(rel, "/", fixed = TRUE)[[1]] == "..")) return(NA_character_)
  file.path(dest, .portable_path(rel))
}

# Size in bytes of a remote zip, estimated from its central directory (`cd`,
# from zip_peek()) so no extra request is needed: the end of the last member's
# data, plus the central directory and its end record that follow it. Extra
# fields are not in `cd`, so this can be short by a few dozen bytes per member
# (31 per member for an Info-ZIP archive) -- close enough for a size cap and a
# timeout. A Zip64 archive's own end records (76 bytes) are added when the
# archive is large enough to need them. NA when any entry's offset or
# compressed size is unknown.
.zip_whole_size <- function(cd) {
  if (is.null(cd) || nrow(cd) == 0 || anyNA(cd$offset) || anyNA(cd$csize))
    return(NA_real_)
  name_bytes <- cd$name_bytes %||% nchar(cd$name, type = "bytes")
  data_end <- max(cd$offset + 30 + name_bytes + cd$csize)
  zip64 <- if (data_end >= 4294967295 || nrow(cd) >= 65535) 76 else 0
  data_end + sum(46 + name_bytes) + 22 + zip64
}

# Download a whole zip once and extract the rows of `want` (from zip_peek())
# into `targets` (from .zip_member_target(), same order). Returns a character
# vector the length of `targets`: the written path, or NA for a member that was
# not delivered (the caller then fetches it by byte range). Returns NULL when
# the archive was not used at all: too large or of unknown size, or the
# download failed. See .zip_fetch_members() for when and why this is used.
.zip_fetch_whole <- function(url, cd, want, targets, max_bytes,
                             archive_path = NULL, skip_on_api_limit = FALSE) {
  size <- .zip_whole_size(cd)
  if (is.na(size) || size > max_bytes) return(NULL)

  if (is.null(archive_path)) {
    archive_path <- tempfile(fileext = ".zip")
    on.exit(unlink(archive_path), add = TRUE)
  }
  zip_list <- function(p) {
    if (!file.exists(p)) return(NULL)
    tryCatch(utils::unzip(p, list = TRUE), error = function(e) NULL)
  }
  # Reuse an archive kept by an earlier run; one that cannot be listed is a
  # partial or corrupt download and is fetched again.
  if (is.null(zip_list(archive_path))) {
    unlink(archive_path)
    err <- .download_one(url, archive_path, skip_on_api_limit = skip_on_api_limit,
                         expected_bytes = size)
    if (!is.na(err) || is.null(zip_list(archive_path))) {
      unlink(archive_path)
      return(NULL)
    }
  }
  .zip_extract_members(archive_path, want, targets)
}

# Extract the rows of `want` from the zip at `archive_path` (on disk) into
# `targets` (same order; NA = refused by .zip_member_target()). Returns the
# written path per member, or NA for one not delivered.
#
# Extracts into a scratch directory first, then copies each member to its
# target, so the paths on disk are the same as the byte-range fetch writes
# (which strips leading "/" and drive letters) and a traversal name is never
# handed to unzip at all.
.zip_extract_members <- function(archive_path, want, targets) {
  exdir <- tempfile("zipwhole")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  safe <- !is.na(targets)
  if (any(safe))
    tryCatch(suppressWarnings(utils::unzip(archive_path, files = want$name[safe],
                                           exdir = exdir)),
             error = function(e) NULL)

  out <- rep(NA_character_, length(targets))
  for (i in which(safe)) {
    src <- file.path(exdir, want$name[i])
    # The archive's own record of the size checks the extracted bytes; a
    # mismatch or a missing file leaves the member to the per-member fetch.
    if (!file.exists(src) ||
        (!is.na(want$size[i]) && file.size(src) != want$size[i])) next
    dir.create(dirname(targets[i]), showWarnings = FALSE, recursive = TRUE)
    if (isTRUE(file.copy(src, targets[i], overwrite = TRUE))) out[i] <- targets[i]
  }
  out
}

# ── Archive-format classification ────────────────────────────────────────────
# Which archive/compression formats metacheck can OPEN with base R alone:
#   * zip            → utils::unzip                (also peekable without download)
#   * tar family     → utils::untar                (.tar[.gz|.bz2|.xz], .tgz, ...)
#   * single-file gz → gz/bz2/xz connection        (one compressed file, no index)
# Everything else archive-shaped (.7z, .rar, .cab, .arj, .lzma, ...) needs an
# external binary or the libarchive-backed `archive` package, which metacheck
# does NOT depend on — those are left as `other` (never fetched) and repo_check
# warns the author to re-upload as .zip. `untar` FAILS on a bare .gz that is not
# a tarball ("Missing type keyword"), so single-file compressions are a distinct
# class handled by connection, not by untar.
.is_zip <- function(name)
  grepl("[.]zip$", name, ignore.case = TRUE)
.is_tar_archive <- function(name)
  grepl("[.](tar|tar[.]gz|tgz|tar[.]bz2|tbz2?|tar[.]xz|txz)$", name,
        ignore.case = TRUE)
# Bare single-file compressions (NOT .tar.*): one file inside, classified by the
# name with the compression suffix stripped (results.csv.gz → results.csv).
.is_single_compress <- function(name)
  grepl("[.](gz|bz2|xz)$", name, ignore.case = TRUE) & !.is_tar_archive(name)
# Archives base R can read (and therefore worth downloading despite `unknown`).
.is_readable_archive <- function(name)
  .is_zip(name) | .is_tar_archive(name) | .is_single_compress(name)

# Shared back end for the three .expand_* functions. Called AFTER extraction: it
# walks what is actually on disk under `dest` (rather than trusting the archive's
# listed member names, which tar may rewrite — stripping drive letters or leading
# "/"/"./" — so listed paths need not match the extracted ones), classifies each
# file, keeps only data or documentation (readme/codebook, by doc_role) content
# (never skip_types), and builds rows inheriting `archive_row`'s repo/paper/group
# fields. `label` prefixes file_path for provenance (the container name).
# Returns a 0-row frame when nothing worth keeping is inside — which is how an
# archive of only materials ends up contributing nothing, exactly as if it had
# not been downloaded.
# Which inner files of an archive are kept: data and code, and documentation
# that is a codebook or README, never a type in `skip_types`, never a file
# that does not belong in a repository (see .useless_file_kind()). `name` is
# the inner paths; they are classified by base name, all together (so a
# GeoTIFF sees the GIS files beside it).
#
# "code" is kept alongside data/documentation (gated by skip_types like
# everything else) so a zip's bundled code files survive expansion instead
# of being silently discarded -- data_check itself never surfaces them (it
# only column-extracts data_type == "data"), but code_check reads this same
# table via get_prev_outputs("data_check", "structure") when data_check ran
# first in the pipeline, and needs the file present to classify at all. See
# issue #383 (Gap 3).
#
# The .expand_* functions apply this to the archive's listing BEFORE
# unpacking (#472), so a file that would be dropped right after is never
# written: picrust2's reference tables (go.txt.gz and others, plain
# documentation by name) were decompressed into 6.4 GB of cache only to be
# discarded.
.archive_keep <- function(name, skip_types) {
  if (length(name) == 0) return(logical(0))
  name <- gsub("\\\\", "/", name)
  base <- basename(name)
  types <- data_classify_files(base)
  roles <- .data_doc_role(base)
  (types %in% c("data", "code") |
     (types == "documentation" & !is.na(roles) & roles %in% c("codebook", "readme"))) &
    !(types %in% skip_types) &
    !grepl("(^|/)__MACOSX/", name) & !.is_useless_file(name)
}

# `rows` plus one row for each inner file that was NOT unpacked because it is
# larger than the per-file limit (#472), with no local copy (file_location
# NA), the way a file too large to download is listed; the same files are
# attached as attribute "oversize" (file_name, file_size in bytes, NA when
# unknown), which data_check adds to its record of files over the limit, so
# the manifest gives them that reason. `name` is the inner paths.
.with_oversize <- function(rows, archive_row, label, name, file_size) {
  if (length(name) == 0) return(rows)
  extra <- archive_row[rep(1, length(name)), , drop = FALSE]
  extra$file_name     <- basename(name)
  extra$file_path     <- file.path(label, name)
  extra$file_location <- NA_character_
  extra$file_size     <- as.numeric(file_size)
  if ("data_type" %in% names(extra)) extra$data_type <- data_classify_files(basename(name))
  if ("doc_role" %in% names(extra))  extra$doc_role  <- .data_doc_role(basename(name))
  if ("data_format" %in% names(extra))
    extra$data_format <- data_format(tolower(tools::file_ext(name)))
  if ("file_url" %in% names(extra))  extra$file_url  <- NA_character_
  if ("file_type" %in% names(extra)) extra$file_type <- .inner_file_type(name)
  out <- dplyr::bind_rows(rows, extra)
  attr(out, "oversize") <- data.frame(file_name = basename(name),
                                      file_size = as.numeric(file_size))
  out
}

.archive_rows <- function(dest, archive_row, label, skip_types) {
  empty <- archive_row[0, , drop = FALSE]
  if (!dir.exists(dest)) return(empty)

  # Paths relative to the extraction root (no full.names → no prefix to strip,
  # so no fragile path-regex). loc is rebuilt with file.path for the real files.
  rel <- list.files(dest, recursive = TRUE, all.files = TRUE)
  rel <- rel[!grepl("(^|/)__MACOSX/", rel)]           # drop mac resource forks
  if (length(rel) == 0) return(empty)
  loc <- file.path(dest, rel)

  types <- data_classify_files(basename(loc))
  roles <- .data_doc_role(basename(loc))
  fmt   <- data_format(tolower(tools::file_ext(loc)))
  keep  <- .archive_keep(rel, skip_types)
  if (!any(keep)) return(empty)

  loc <- loc[keep]; rel <- rel[keep]; types <- types[keep]
  roles <- roles[keep]; fmt <- fmt[keep]

  rows <- archive_row[rep(1, length(loc)), , drop = FALSE]
  rows$file_name     <- basename(loc)
  rows$file_path     <- file.path(label, rel)   # inner path, prefixed for context
  rows$file_location <- loc
  rows$file_size     <- suppressWarnings(file.size(loc))
  if ("data_type" %in% names(rows))   rows$data_type   <- types
  if ("doc_role" %in% names(rows))    rows$doc_role    <- roles
  if ("data_format" %in% names(rows)) rows$data_format <- fmt
  # Inner files came from a local extraction, not a remote URL of their own.
  if ("file_url" %in% names(rows))    rows$file_url    <- NA_character_
  if ("file_type" %in% names(rows))   rows$file_type   <- .inner_file_type(rel)
  rows
}

# Each file's own type from its extension (`file_types`, as repo_check types a
# repository's files), NA without a known one. The rows for an archive's files
# are copies of the archive's row, so every file kept the type "archive" (U213).
.inner_file_type <- function(name) {
  ext <- tolower(tools::file_ext(name))
  metacheck::file_types$type[match(ext, metacheck::file_types$ext)]
}

# The folder an archive is extracted into. A downloaded archive (in the
# repository cache, or this session's download folder when the cache is off;
# see .repo_session_dir()) is extracted beside itself, to "<archive>.contents",
# which a later run reuses. Any other archive is the user's own (local_path):
# extracting beside it wrote into the user's folder, and the next run listed
# the extracted files as files of their own and extracted the archive again,
# so every file counted twice (U214). It is extracted into the temporary
# folder instead, under the archive's checksum. A folder of the user's that is
# itself in the temporary folder (an upload unpacked there, a test) is the
# user's too: the whole temporary folder counted as downloads, so FAIR_repo()
# wrote "<archive>.contents" into the folder it was converting.
.archive_dest <- function(path) {
  if (.in_download_folder(path)) return(paste0(path, ".contents"))
  file.path(tempdir(), "metacheck-archives", unname(tools::md5sum(path)))
}

# Whether `path` is a file metacheck downloaded: in the repository cache, or
# this session's download folder when the cache is off. metacheck writes next
# to such a file (an unpacked archive, code recovered from an output file);
# any other file is the user's own, and nothing is written into its folder.
.in_download_folder <- function(path) {
  repo_cache <- getOption("metacheck.repo_cache.dir")
  if (is.null(repo_cache) || !nzchar(repo_cache))
    repo_cache <- file.path(.metacheck_cache_root(), ".metacheck_repo_cache")
  session <- getOption("metacheck.repo_cache.session_dir",
                       file.path(tempdir(), "metacheck-repo-files"))
  roots <- normalizePath(c(session, repo_cache), winslash = "/", mustWork = FALSE)
  norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") { roots <- tolower(roots); norm <- tolower(norm) }
  any(startsWith(norm, paste0(roots, "/")))
}

# The folder code recovered from an output file is written to: a `code_dir_name`
# folder next to a downloaded file, which a later run reuses; for a file of the
# user's own, a folder of that name in the temporary folder (removed when the R
# session ends), under the file's checksum, so the user's folder is not changed.
.recovered_code_dir <- function(path, code_dir_name = "code") {
  if (.in_download_folder(path)) return(file.path(dirname(path), code_dir_name))
  file.path(tempdir(), "metacheck-recovered-code", unname(tools::md5sum(path)),
            code_dir_name)
}

# Move the members extracted into `part` (a folder beside `dest`) into `dest`,
# then remove `part`. Extracting straight into `dest` left a half-written file
# there when the run was stopped, which the next run reused as complete
# (U203); a member that was not extracted is simply not moved.
.move_extracted <- function(part, dest, members) {
  for (m in members) {
    from <- file.path(part, m)
    if (!file.exists(from)) next
    to <- file.path(dest, m)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.rename(from, to)
  }
  unlink(part, recursive = TRUE)
}

# Expand a downloaded zip and return rows for its DATA-type inner files, ready to
# bind into data_check's `all_files`. The zip is extracted once to a cache dir
# beside the zip; each inner file is classified, and only files whose type is
# data or documentation with doc_role readme/codebook (and not in skip_types)
# are returned — materials and other inner content are left in the extraction
# dir but not added to the archive (the original zip remains the link for
# them). `zip_row` is the zip's own row in all_files, whose repo/paper/group
# fields the inner rows inherit.
#
# Returns a data.frame with the same columns as `zip_row` (one per kept inner
# file, file_location pointing at the extracted copy), or a 0-row frame.
#
# Only the members that will be kept (.archive_keep()) and are no larger than
# `max_bytes` are unpacked (#472); the listing gives each member's size before
# anything is written. A member over the limit is reported in the result's
# "oversize" attribute, like a file too large to download, and a copy left by
# an earlier run is removed, so the cache never holds more than the limits.
.expand_zip <- function(zip_path, zip_row, skip_types = "materials", max_bytes = Inf) {
  empty <- zip_row[0, , drop = FALSE]
  if (is.null(zip_path) || is.na(zip_path) || !file.exists(zip_path)) return(empty)

  # Extract into "<zip>.contents/" beside the cached zip (idempotent: reuse);
  # a zip of the user's own is extracted elsewhere (.archive_dest()).
  dest <- .archive_dest(zip_path)
  entries <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(entries) || nrow(entries) == 0) return(empty)
  entries <- entries[!grepl("/$", entries$Name), , drop = FALSE]
  keep <- .archive_keep(entries$Name, skip_types)
  over <- keep & entries$Length > max_bytes
  unlink(file.path(dest, entries$Name[over]))
  want <- entries$Name[keep & !over]
  missing <- want[!file.exists(file.path(dest, want))]
  if (length(missing) > 0) {
    part <- paste0(dest, ".part")
    tryCatch(utils::unzip(zip_path, files = missing, exdir = part),
             error = function(e) NULL, warning = function(w) NULL)
    .move_extracted(part, dest, missing)
  }

  rows <- if (length(want) > 0) .archive_rows(dest, zip_row, basename(zip_path), skip_types)
          else empty
  .with_oversize(rows, zip_row, basename(zip_path), entries$Name[over], entries$Length[over])
}

# Expand a downloaded tar-family archive (.tar / .tar.gz / .tgz / .tar.bz2 /
# .tar.xz / ...). Mirror of .expand_zip using base R's utils::untar (no extra
# dependency). Peeking is impossible for tar (no tail index), so the caller must
# have downloaded it first; the size caps still bound what gets fetched. A tar we
# cannot read (corrupt, or an unusual variant) degrades to a 0-row frame rather
# than erroring the run.
#
# As in .expand_zip(), only members that will be kept and are no larger than
# `max_bytes` are unpacked (#472). A plain .tar gives each member's size from
# its headers (.tar_list_file()) before anything is written. A compressed tar
# gives only names without reading it all, so its kept members are unpacked
# and any over the limit removed again straight away.
.expand_tar <- function(tar_path, tar_row, skip_types = "materials", max_bytes = Inf) {
  empty <- tar_row[0, , drop = FALSE]
  if (is.null(tar_path) || is.na(tar_path) || !file.exists(tar_path)) return(empty)

  dest <- .archive_dest(tar_path)
  entries <- tryCatch(utils::untar(tar_path, list = TRUE), error = function(e) NULL,
                      warning = function(w) NULL)
  if (is.null(entries) || length(entries) == 0) return(empty)
  entries <- entries[!grepl("/$", entries)]
  plain <- grepl("[.]tar$", tar_path, ignore.case = TRUE)
  listing <- if (plain) .tar_list_file(tar_path) else NULL
  size <- if (!is.null(listing)) listing$size[match(entries, listing$member)] else
    rep(NA_real_, length(entries))
  keep <- .archive_keep(entries, skip_types)
  over <- keep & !is.na(size) & size > max_bytes
  unlink(file.path(dest, entries[over]))
  want <- entries[keep & !over]
  missing <- want[!file.exists(file.path(dest, want))]
  if (length(missing) > 0) {
    part <- paste0(dest, ".part")
    tryCatch(utils::untar(tar_path, files = missing, exdir = part),
             error = function(e) NULL, warning = function(w) NULL)
    .move_extracted(part, dest, missing)
  }
  # sizes only known once unpacked (compressed tar)
  got <- file.path(dest, want)
  big <- file.exists(got) & file.size(got) > max_bytes
  if (any(big)) {
    size[match(want[big], entries)] <- file.size(got[big])
    over[match(want[big], entries)] <- TRUE
    unlink(got[big])
  }

  rows <- if (any(keep & !over)) .archive_rows(dest, tar_row, basename(tar_path), skip_types)
          else empty
  .with_oversize(rows, tar_row, basename(tar_path), entries[over], size[over])
}

# Expand a downloaded single-file compression (.gz / .bz2 / .xz that is NOT a
# .tar.*). These wrap exactly ONE file; there is no listing, so we decompress the
# stream to the name with the compression suffix stripped (results.csv.gz →
# results.csv) and classify that one file. Uses base R connections only.
#
# Decompressed only when the inner file will be kept (.archive_keep(), by its
# name), and only up to `max_bytes` (#472): the download limits are checked
# on the compressed size, and a 74 MB go.txt.gz became a 1.2 GB copy. The
# uncompressed size is not stored in the file, so decompression stops once
# the limit is passed, the partial copy is deleted, and the file is reported
# in the "oversize" attribute (size NA, since only "more than the limit" is
# known). A copy over the limit left by an earlier run is removed.
.expand_compressed <- function(gz_path, gz_row, skip_types = "materials", max_bytes = Inf) {
  empty <- gz_row[0, , drop = FALSE]
  if (is.null(gz_path) || is.na(gz_path) || !file.exists(gz_path)) return(empty)

  ext <- tolower(tools::file_ext(gz_path))
  open_con <- switch(ext,
    gz  = gzfile, bz2 = bzfile, xz = xzfile, NULL)
  if (is.null(open_con)) return(empty)

  dest <- .archive_dest(gz_path)
  inner_name <- sub("[.](gz|bz2|xz)$", "", basename(gz_path), ignore.case = TRUE)
  if (!.archive_keep(inner_name, skip_types)) return(empty)
  out <- file.path(dest, inner_name)
  if (file.exists(out) && isTRUE(file.size(out) > max_bytes)) unlink(out)
  if (!file.exists(out)) {
    # In a function of its own, so both connections are closed before a
    # partial copy is deleted (an open file cannot be deleted on Windows).
    # written beside `dest` and moved in when complete: a decompression
    # that was stopped left a partial file, reused as complete (U203)
    part <- file.path(paste0(dest, ".part"), inner_name)
    decompress <- function() {
      dir.create(dirname(part), recursive = TRUE, showWarnings = FALSE)
      con <- open_con(gz_path, "rb"); on.exit(close(con), add = TRUE)
      oc  <- file(part, "wb"); on.exit(close(oc), add = TRUE)
      written <- 0
      repeat {
        chunk <- readBin(con, "raw", n = 1048576L)
        if (length(chunk) == 0) return("ok")
        written <- written + length(chunk)
        if (written > max_bytes) return("over")
        writeBin(chunk, oc)
      }
    }
    status <- tryCatch(decompress(), error = function(e) "failed",
                       warning = function(w) "failed")
    if (!identical(status, "ok")) {
      unlink(dirname(part), recursive = TRUE)
      return(if (identical(status, "over"))
        .with_oversize(empty, gz_row, basename(gz_path), inner_name, NA) else empty)
    }
    .move_extracted(dirname(part), dest, inner_name)
  }

  .archive_rows(dest, gz_row, basename(gz_path), skip_types)
}

#' Decide whether a remote ZIP is worth downloading for a data archive
#'
#' Peeks inside the zip (see [zip_peek()]) and classifies its entries. A zip is
#' worth downloading if it contains actual data or a codebook; a zip of only
#' stimuli/software is better linked than mirrored.
#'
#' @param url the zip's download URL, or the path of a zip already on disk
#' @param skip_types data_type(s) that don't count as worth-downloading content
#'   (e.g. `"materials"`)
#' @param cache passed to [zip_peek()]: if `TRUE`, reuse an on-disk cached
#'   peek across R sessions instead of re-peeking the archive.
#' @param skip_on_api_limit passed to [zip_peek()]: if `TRUE`, a confirmed
#'   exhausted rate-limit bucket is treated as a failed peek instead of
#'   waiting out the host's reset.
#' @param size passed to [zip_peek()]: the zip's size in bytes when already
#'   known (e.g. from the repository's file listing), which saves a HEAD
#'   request. `NULL` (the default) asks the host.
#'
#' @returns a list with `worth` (`TRUE`/`FALSE`, or `NA` when the peek failed so
#'   the caller can fall back to downloading), `reason`, `n_entries`, `types`
#'   (table of inner types), and `contents` (the peeked data.frame or `NULL`).
#' @export
#' @keywords internal
zip_decision <- function(url, skip_types = "materials", cache = FALSE,
                         skip_on_api_limit = FALSE, size = NULL) {
  peek <- zip_peek(url, cache = cache, skip_on_api_limit = skip_on_api_limit,
                   size = size)
  if (is.null(peek))
    return(list(worth = NA, reason = "could not peek (download to inspect)",
                n_entries = NA_integer_, types = NULL, contents = NULL))
  types <- data_classify_files(basename(peek$name))
  roles <- .data_doc_role(basename(peek$name))
  # "worth it" = contains actual research data or a codebook (not just stimuli,
  # instructions, or media). A zip of only materials/readmes is linked, not
  # mirrored, matching the archive intent "we host data, link to materials".
  worth <- any(types == "data" |
                (types == "documentation" & !is.na(roles) & roles == "codebook")) &&
    !all(types %in% skip_types)
  list(
    worth     = worth,
    reason    = if (worth) "contains archivable content"
                else "only skipped types inside (link, do not mirror)",
    n_entries = nrow(peek),
    types     = table(types),
    contents  = peek)
}
