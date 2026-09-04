# Selective, cached download of repository files for the modules that read file
# contents (data_check, code_check). repo_check lists files without fetching
# them (file_location = NA for OSF/GitHub/Zenodo); these helpers fetch only the
# files a module needs, into a shared persistent cache, so downloads are reused
# across modules and R sessions.

# Root cache directory for downloaded repository files. Defaults to
# ".metacheck_repo_cache" in the working directory (see .metacheck_cache_subdir);
# the `metacheck.repo_cache.dir` option overrides just this cache, and
# `metacheck.cache.dir` relocates both caches together.
.repo_cache_dir <- function() {
  .metacheck_cache_subdir(".metacheck_repo_cache",
                          override = getOption("metacheck.repo_cache.dir"))
}

# Stable per-repo cache subdirectory. Keyed by a filesystem-safe encoding of the
# repo URL so different repos never collide and the same repo always resolves to
# the same folder (enabling cross-session reuse).
.repo_cache_subdir <- function(repo_url) {
  key <- as.character(repo_url %||% "unknown")
  key <- gsub("^https?://", "", key)          # scheme is noise
  key <- gsub("[^A-Za-z0-9._-]+", "_", key)   # filesystem-safe
  key <- gsub("^_+|_+$", "", key)
  if (!nzchar(key)) key <- "unknown"
  file.path(.repo_cache_dir(), key)
}

# Repo-relative cache location (per-repo key + repo-relative path), shared by the
# persistent and the session cache so both use an identical on-disk layout.
.repo_cache_rel <- function(repo_url, file_path) {
  rel <- gsub("\\\\", "/", file_path)
  rel <- gsub("^/+", "", rel)
  key <- as.character(repo_url %||% "unknown")
  key <- gsub("^https?://", "", key)
  key <- gsub("[^A-Za-z0-9._-]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  if (!nzchar(key)) key <- "unknown"
  file.path(key, rel)
}

# Local cache path for one file, preserving its repo-relative path so files with
# the same basename in different folders don't collide.
.repo_cache_path <- function(repo_url, file_path) {
  file.path(.repo_cache_dir(), .repo_cache_rel(repo_url, file_path))
}

# Per-session scratch cache used when cache = FALSE: a single temp directory
# (created once per session, removed by R on exit) that holds this run's
# downloads without persisting them. Mirrors the persistent cache's layout.
.repo_session_dir <- function() {
  dir <- getOption("metacheck.repo_cache.session_dir",
                   file.path(tempdir(), "metacheck-repo-files"))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  options(metacheck.repo_cache.session_dir = dir)
  dir
}

#' Locate the downloaded-repository file cache
#'
#' Files fetched from OSF/GitHub/Zenodo/ResearchBox by `data_check` and
#' `code_check` are stored in a persistent on-disk cache so they are reused
#' across modules and R sessions (never re-downloaded). This returns that
#' cache's root directory.
#'
#' The cache is never cleared automatically — it has no size cap or expiry, so
#' it is safe to reuse but grows without bound. Use [repo_cache_clear()] to
#' delete it (e.g. to reclaim disk space after a large corpus run).
#'
#' The location defaults to `rappdirs::user_cache_dir("metacheck/repo_files")`;
#' override it with `options(metacheck.repo_cache.dir = "/some/path")` (e.g. for
#' a project-local cache).
#'
#' @returns the cache root directory path (character), invisibly
#' @seealso [repo_cache_clear()], [repo_cache_size()]
#' @export
#' @examples
#' \dontrun{
#' repo_cache_dir()
#' }
repo_cache_dir <- function() {
  invisible(.repo_cache_dir())
}

#' Size of the downloaded-repository file cache
#'
#' @returns total size of the cache in bytes (numeric). 0 when the cache is
#'   empty or absent.
#' @seealso [repo_cache_dir()], [repo_cache_clear()]
#' @export
#' @examples
#' \dontrun{
#' # human-readable
#' format(structure(repo_cache_size(), class = "object_size"), units = "auto")
#' }
repo_cache_size <- function() {
  .metacheck_dir_size(.repo_cache_dir())
}

#' Delete the downloaded-repository file cache
#'
#' Removes files fetched from data repositories by `data_check` / `code_check`
#' from the on-disk cache (see [repo_cache_dir()]). The cache only speeds up
#' re-runs — anything deleted is simply re-downloaded when next needed — so
#' clearing it is always safe; the usual reason is to reclaim disk space after a
#' large corpus build.
#'
#' By default the whole cache is removed. Pass `repo_url` to remove only the
#' cached files of specific repositories.
#'
#' @param repo_url optional character vector of repository URLs. When supplied,
#'   only those repositories' cached files are deleted; otherwise the entire
#'   cache is cleared.
#' @param quiet if `FALSE` (default), report how much was freed.
#'
#' @returns the number of bytes freed (numeric), invisibly.
#' @seealso [repo_cache_dir()], [repo_cache_size()]
#' @export
#' @examples
#' \dontrun{
#' # clear everything
#' repo_cache_clear()
#'
#' # clear one repository only
#' repo_cache_clear("https://osf.io/abcde")
#' }
repo_cache_clear <- function(repo_url = NULL, quiet = FALSE) {
  # Safety guard: refuse to delete a real cache while tests are running unless
  # the test has redirected the cache location (via metacheck.repo_cache.dir or
  # the shared metacheck.cache.dir) to a temp dir. Without this, a test that
  # forgets to redirect would wipe whatever cache is in the working directory.
  # Guards the whole-cache clear only — a targeted repo_url clear is scoped.
  if (is.null(repo_url) &&
      identical(Sys.getenv("TESTTHAT"), "true") &&
      is.null(getOption("metacheck.repo_cache.dir")) &&
      is.null(getOption("metacheck.cache.dir"))) {
    stop("repo_cache_clear() refused to empty the cache during tests without a ",
         "redirected location. Set one first, e.g. ",
         "withr::local_options(metacheck.cache.dir = withr::local_tempdir()).",
         call. = FALSE)
  }

  targets <- if (is.null(repo_url)) .repo_cache_dir()
             else vapply(repo_url, .repo_cache_subdir, character(1))
  targets <- unique(targets[dir.exists(targets)])

  freed <- sum(vapply(targets, function(d) {
    files <- list.files(d, recursive = TRUE, full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
    if (length(files) == 0) 0 else sum(file.size(files), na.rm = TRUE)
  }, numeric(1)))

  for (d in targets) unlink(d, recursive = TRUE)
  # Recreate the (empty) root so later downloads have somewhere to write.
  if (is.null(repo_url)) dir.create(.repo_cache_dir(), showWarnings = FALSE,
                                    recursive = TRUE)

  if (!isTRUE(quiet)) {
    human <- format(structure(freed, class = "object_size"), units = "auto")
    scope <- if (is.null(repo_url)) "the repository file cache"
             else sprintf("%d cached repositor%s", length(targets),
                          if (length(targets) == 1) "y" else "ies")
    message(sprintf("Cleared %s (%s freed).", scope, human))
  }
  invisible(freed)
}

# Resolve a remote file's size (bytes) from its Content-Length header via a
# lightweight HEAD request. Used to turn a missing manifest size into a real
# size *before* downloading, so an unsized 5 GB file is caught by the gate. NA
# on any error or when the header is absent (chunked/dynamic responses).
#
# Routed through .auth_for_url(): Dryad answers 401 to an unauthenticated HEAD
# on ANY byte-touching endpoint, including this one (verified live 2026-08-16,
# see .dryad_headers()'s own comment) -- without this a Dryad file whose size
# is not already known from the repo's own listing is silently dropped from
# the download candidate set (see .remote_content_length()'s req_func, which
# existed for the same reason but had to be opted into per call site; only 1
# of 7 call sites did, so the other 6 still 401'd on Dryad -- .auth_for_url()
# dispatches on the URL itself, so every caller is covered with no opt-in).
.remote_size <- function(url) {
  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      .auth_for_url() |>
      httr2::req_error(is_error = function(r) FALSE)
    resp <- httr2::req_perform(req)
    cl <- httr2::resp_header(resp, "content-length")
    if (is.null(cl) || is.na(cl) || !nzchar(cl)) return(NA_real_)
    as.numeric(cl)
  }, error = function(e) NA_real_)
}

# Resolve a remote resource's Content-Length (bytes) via HEAD. Used to compare
# one-shot archive transport size against the selected-file estimate.
#
# req_func: request configurator applied before sending, on top of the
# host-based auth .auth_for_url() already adds from the URL alone. Was the
# ONLY auth this function applied, opt-in per call site via req_func =
# .dryad_headers -- but only 1 of 7 call sites actually opted in, so the other
# 6 still 401'd if they ever hit a Dryad URL (Dryad requires a token for ANY
# call that touches file bytes, including a bare HEAD -- verified live
# 2026-08-16, see .dryad_headers()'s own comment). .auth_for_url() dispatches
# on the URL itself, so every call site is covered with no opt-in; req_func
# stays for a caller that needs something .auth_for_url() does not cover.
.remote_content_length <- function(url, req_func = identity) {
  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      .auth_for_url() |>
      req_func() |>
      httr2::req_error(is_error = function(r) FALSE)
    resp <- httr2::req_perform(req)
    cl <- httr2::resp_header(resp, "content-length")
    if (is.null(cl) || is.na(cl) || !nzchar(cl)) return(NA_real_)
    as.numeric(cl)
  }, error = function(e) NA_real_)
}

# Add the right authorisation to a file-download request, based on its host.
#
# Listing calls all go through .osf_headers(), so an OSF token reveals every
# private project and its file names and sizes. The file BYTES were fetched
# anonymously, which fails in a way that is easy to miss: the OSF does not
# answer 403 for a private file, it returns HTTP 200 and an HTML sign-in page
# (verified 2026-08-13 on osf.io/download/854te/, which returned
# "<title>OSF | Sign in</title>" with status 200). That page was then written
# to disk as if it were the file, and only the size check caught it -- as
# "truncated", which read like a network problem rather than a missing token.
#
# Zenodo behaves the same way for a restricted record, so its token is added
# too where one is set.
.auth_for_url <- function(req) {
  url <- req$url %||% ""
  if (grepl("osf\\.io", url, ignore.case = TRUE)) {
    pat <- tryCatch(osf_pat(), error = \(e) "")
    if (nzchar(pat %||% "")) {
      # unrestricted_auth is what makes the token survive the redirect, and
      # without it the token is useless for a private file.
      #
      # osf.io/download/<id>/ answers 302 to a REGIONAL storage host --
      # files.de-1.osf.io for a European project, files.osf.io for others.
      # curl drops the Authorization header when a redirect crosses to a
      # different host (a sensible default: it stops a token leaking to
      # wherever a redirect points). The OSF relies on that header being kept,
      # so the storage host then answers 403 and the file never arrives.
      #
      # Verified 2026-08-13 on a real private file (osf.io/download/854te/,
      # 30,461 bytes): 403 without this option, HTTP 200 and the full file
      # with it. Public files are unaffected either way.
      #
      # The token is only sent to hosts matched above, all of them OSF's own,
      # so allowing it to follow their internal redirect does not expose it to
      # a third party.
      req <- req |>
        httr2::req_headers(Authorization = sprintf("Bearer %s", pat)) |>
        httr2::req_options(unrestricted_auth = TRUE)
    }
  } else if (grepl("zenodo\\.org", url, ignore.case = TRUE)) {
    sandbox <- grepl("sandbox\\.zenodo\\.org", url, ignore.case = TRUE)
    pat <- tryCatch(zenodo_pat(sandbox = sandbox), error = \(e) "")
    if (nzchar(pat %||% "")) {
      # Zenodo also redirects a file request to its own storage host, so the
      # token needs to survive that hop for the same reason as the OSF above.
      req <- req |>
        httr2::req_headers(Authorization = sprintf("Bearer %s", pat)) |>
        httr2::req_options(unrestricted_auth = TRUE)
    }
  } else if (grepl(.dataverse_host_regex(), url, ignore.case = TRUE)) {
    # Dataverse's token is per-installation (see .dataverse_pat()), unlike
    # OSF/Zenodo's single shared token, and is sent as X-Dataverse-key rather
    # than a Bearer Authorization header -- .dataverse_headers() applies it.
    req <- .dataverse_headers(req)
  } else if (grepl("figshare\\.com", url, ignore.case = TRUE)) {
    # Figshare's token (see .figshare_pat()) is sent as "Authorization: token
    # X" rather than a Bearer scheme -- .figshare_headers() applies it. Public
    # files download without one; a token here only raises rate limits or
    # unlocks a private article.
    req <- .figshare_headers(req)
  } else if (grepl("datadryad\\.org", url, ignore.case = TRUE)) {
    # Dryad's token (see .dryad_pat()) is a Bearer token like OSF/Zenodo, but
    # unlike every other host handled here it is REQUIRED to download file
    # bytes even from a fully public dataset (verified live 2026-08-16: both
    # the per-file and whole-dataset download endpoints answer 401 with no
    # token) -- .dryad_headers() applies it when set, but with none set every
    # Dryad download in this function will fail.
    req <- .dryad_headers(req)
  } else if (grepl("reshare\\.ukdataservice\\.ac\\.uk", url, ignore.case = TRUE)) {
    # ReShare has no documented personal-access-token scheme; public deposits
    # download without any auth header at all (verified live 2026-08-16).
    # .reshare_headers() only sets User-Agent.
    req <- .reshare_headers(req)
  } else if (grepl("data\\.4tu\\.nl", url, ignore.case = TRUE)) {
    # 4TU.ResearchData's Djehuty platform reuses Figshare's auth scheme
    # ("Authorization: token X") but issues its own tokens, meaningless to
    # real Figshare -- see .researchdata4tu_pat(). Applied directly here
    # (rather than via .figshare_headers(), which reads figshare_pat()) so a
    # file URL reached outside researchdata4tu_file_download()'s own
    # temporary option override still gets the right token.
    pat <- tryCatch(.researchdata4tu_pat(), error = \(e) "")
    req <- req |> httr2::req_headers(`User-Agent` = "metacheck")
    if (nzchar(pat %||% "")) {
      req <- req |> httr2::req_headers(Authorization = sprintf("token %s", pat))
    }
  }
  req
}

# Did this response serve a login page instead of the file?
#
# An unauthenticated request for a private OSF file returns HTTP 200 with an
# HTML sign-in page. Treated as a normal download it produces a file of the
# wrong size, reported as "truncated", which sends the reader looking for a
# network fault. Recognising it lets the real cause be named.
.is_login_page <- function(path, expected_size = NA_real_) {
  if (!file.exists(path)) return(FALSE)
  size <- file.size(path)
  # A sign-in page is small and is HTML; a data file of the same size would
  # have to begin with an HTML doctype to be confused with one.
  if (is.na(size) || size == 0 || size > 200000) return(FALSE)

  # Read RAW BYTES, not characters. Most downloads are binary (PDFs, xlsx,
  # gzip), and readChar() on those emits "truncating string with embedded
  # nuls" and "unable to translate ... to a wide string" for every file --
  # 15 warnings on a 31-file project, all of them noise about files that were
  # downloaded perfectly. Bytes have no encoding to get wrong.
  raw_head <- tryCatch(readBin(path, "raw", n = 400), error = \(e) raw(0))
  if (length(raw_head) == 0) return(FALSE)

  # A NUL byte in the first bytes means binary, and rules out an HTML page
  # before any text comparison is attempted.
  if (any(raw_head == as.raw(0))) return(FALSE)

  head_txt <- rawToChar(raw_head)
  # The bytes may still not be valid in the native encoding, so compare them
  # as bytes rather than letting R re-encode them.
  grepl("<!DOCTYPE html|<html", head_txt, ignore.case = TRUE, useBytes = TRUE) &&
    grepl("sign in|log in|osf \\| sign", head_txt,
          ignore.case = TRUE, useBytes = TRUE)
}

# Which HTTP statuses are worth retrying when fetching a FILE from repository
# storage. httr2's default covers 429 and 503. 403 is added because OSF does
# not serve files from api.osf.io directly: it redirects to a pre-signed
# cloud-storage URL, and that URL answers 403 both when the signature has
# expired and when the storage host refuses a request from a burst. Neither is
# permanent, but the default treats 403 as "not allowed, do not retry", so a
# file is silently lost from an otherwise complete download. Observed live on
# 2026-08-12: one file of 56 from pngda failed this way while the test suite
# was downloading concurrently, and succeeded on every run made on its own.
#
# This applies only to downloading file BYTES from storage hosts. A 403 from
# the OSF API itself means a private resource and is handled separately, by
# .osf_status_error(), which is untouched by this.
.storage_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(403, 429, 500, 502, 503, 504)
}

# Wait longer between each try, so a retry does not simply rejoin the burst
# that was just refused: 2s after the first failure, 4s after the second,
# doubling to a 30s ceiling. httr2 honours a server's Retry-After header in
# preference to this, and .storage_retry_after_factory() (below) takes
# priority over both when a response carries a RateLimit-Reset-style header.
.storage_backoff <- function(attempt) {
  min(2^attempt, 30)
}

# Read a host-reported rate-limit reset off a 429 response (seconds from
# now, or NA if the response carries no rate-limit headers at all). Shared
# by .storage_retry_after_factory()/.storage_is_transient_factory() below and
# by the zip-download path, so the header-family lookup lives in exactly one
# place.
#
# Confirmed live 2026-08-31 against real 429 responses: Dryad and GitLab both
# send the IETF-draft "RateLimit-*" header family (RateLimit-Limit,
# RateLimit-Remaining, RateLimit-Reset -- a Unix timestamp), while Zenodo and
# GitHub send the older, unstandardised "X-RateLimit-*" convention (same
# three fields, same Unix-timestamp Reset). Both are checked -- header lookup
# is case-insensitive, so "ratelimit-remaining" and "x-ratelimit-remaining"
# are matched by name only, not by which prefix a given host happens to use.
# OSF, Figshare, and Dataverse do not expose rate-limit headers on the
# endpoints checked (OSF's own docs state a plain 100/hour-unauthenticated,
# 10,000/day-authenticated limit instead, with no header to read it from
# live) -- for those this returns NA and callers fall back to their existing
# behaviour (.storage_backoff()'s exponential doubling), unaffected.
# Pure (no side effects, e.g. no message()) -- used both where a wait is
# about to be acted on (and should be announced -- see
# .storage_retry_after_factory() below) and where a wait is only being
# INSPECTED after the fact (e.g. .download_one()'s error handler, checking
# whether a failure it already gave up on was specifically a confirmed
# rate-limit exhaustion, where announcing a wait that is not going to happen
# would be actively misleading).
.rate_limit_wait <- function(resp) {
  remaining <- httr2::resp_header(resp, "ratelimit-remaining") %||%
    httr2::resp_header(resp, "x-ratelimit-remaining")
  reset <- httr2::resp_header(resp, "ratelimit-reset") %||%
    httr2::resp_header(resp, "x-ratelimit-reset")

  if (is.null(remaining) || is.null(reset)) return(NA_real_)
  if (!identical(suppressWarnings(as.numeric(remaining)), 0)) return(NA_real_)

  reset_time <- suppressWarnings(as.numeric(reset))
  if (is.na(reset_time)) return(NA_real_)

  wait <- reset_time - as.numeric(Sys.time())
  # A reset time already in the past (clock skew, or the header describes a
  # window that just rolled over) means "safe to retry now" -- 0, not a
  # negative wait, which req_retry() would otherwise treat as "wait forever
  # already elapsed" ambiguously depending on how it clamps.
  if (is.na(wait) || wait < 0) 0 else wait
}

# "47s" / "12.3 min" / "1.4 hours" -- whichever unit keeps the number easy to
# read at a glance, for the rate-limit wait message below.
.format_wait_duration <- function(seconds) {
  if (seconds < 60) return(sprintf("%ds", ceiling(seconds)))
  if (seconds < 3600) return(sprintf("%.1f min", seconds / 60))
  sprintf("%.1f hours", seconds / 3600)
}

# req_retry()'s own wait is SILENT by default (confirmed against httr2's
# docs: no mention of printing anything for `after`/`backoff`, and no
# verbosity is configured on these requests) -- a multi-minute-to-hour-long
# wait with no visible sign anything is happening looks identical to a
# genuinely hung process. This announces it: states the exact duration so a
# user monitoring a long corpus run knows this is expected and roughly how
# long to wait rather than assuming something is stuck, and names Ctrl+C as
# the way out (skip_on_api_limit is the scripted equivalent -- see
# download_repo_files()'s own parameter of that name -- for not wanting to
# wait at all, decided ahead of time rather than interrupted after the
# fact). Called only from the spot that is ABOUT to act on the wait
# (.storage_retry_after_factory()'s non-skip branch, and the zip path's own
# equivalent spot), never from a place merely inspecting what would have
# happened, so it never announces a wait that will not occur.
.announce_rate_limit_wait <- function(wait) {
  if (wait > 5) {
    message(sprintf(
      "Rate limit reached; waiting %s for the host's own reset before retrying (Ctrl+C to stop, or use skip_on_api_limit = TRUE to skip this file instead of waiting).",
      .format_wait_duration(wait)))
  }
}

# This is a genuine, confirmed-live root cause for a real production failure
# (not speculative hardening): .storage_backoff()'s blind exponential
# doubling was observed retrying a Dryad download for several minutes against
# a bucket that was provably empty until the top of the hour (RateLimit-
# Remaining: 0), burning the same wall-clock time as it would take to just
# wait for RateLimit-Reset, and contributing to a real corpus-run crash by
# keeping heavy 429 conditions alive far longer than the host's own quota
# recovery would have required.
#
# `after` (the httr2 req_retry() argument this powers) must return either a
# wait in seconds or NA to fall back to `backoff`/Retry-After. It has no way
# to say "give up entirely" -- that is what returning FALSE from
# is_transient is for. A factory here too (mirroring
# .storage_is_transient_factory(), not just relying on it) because it was
# confirmed LIVE (2026-08-31) that httr2 still calls `after` even when
# `is_transient` has already returned FALSE for the same response -- the
# request correctly does not retry either way, but without this the rate-
# limit message below would print "waiting 1 hour..." immediately before
# skip_on_api_limit gives up in the same instant, which is actively
# misleading. skip_on_api_limit here suppresses both the wait-worthy return
# value and the message, matching what actually happens (nothing waits).
.storage_retry_after_factory <- function(skip_on_api_limit = FALSE) {
  function(resp) {
    if (isTRUE(skip_on_api_limit)) return(NA_real_)
    wait <- .rate_limit_wait(resp)
    if (!is.na(wait)) .announce_rate_limit_wait(wait)
    wait
  }
}

# Wraps .storage_is_transient() with one extra rule: `skip_on_api_limit`
# (default FALSE, i.e. always wait for a confirmed rate-limit reset -- see
# download_repo_files()'s own parameter of the same name) changes 429
# specifically. When TRUE, a 429 that carries a confirmed
# RateLimit-Remaining: 0 is treated as NOT transient (so req_retry() gives up
# immediately instead of waiting for the reset), while a
# 429 WITHOUT a confirmed exhausted-bucket signal (no rate-limit headers at
# all, or Remaining > 0 -- a plain burst refusal, not a quota exhaustion)
# still retries with ordinary backoff either way, since skip_on_api_limit's
# whole point is skipping a wait that is already known to be long and
# certain, not giving up on an ordinary transient refusal.
.storage_is_transient_factory <- function(skip_on_api_limit = FALSE) {
  function(resp) {
    if (!.storage_is_transient(resp)) return(FALSE)
    if (isTRUE(skip_on_api_limit) && httr2::resp_status(resp) == 429L) {
      wait <- .rate_limit_wait(resp)
      if (!is.na(wait)) return(FALSE)  # confirmed exhausted bucket: give up now
    }
    TRUE
  }
}

# Download one file to `dest`. Returns NA_character_ on success, or a short
# error description on failure (for the caller's failure report). Files are
# size-gated upfront by the caller, so this just fetches. Transient server
# refusals — OSF rate-limits bursts with 429, answers 403 from its pre-signed
# storage URLs, and 503s happen — are retried with backoff (Retry-After is
# honoured), because a batch of many small requests otherwise loses files at
# random.
#
# skip_on_api_limit: see download_repo_files()'s own parameter of the same
# name. When a 429 is given up on because of it (rather than because
# max_tries ran out on an ordinary transient refusal), the returned message
# is prefixed "API rate limit exhausted: " -- a fixed, greppable string so a
# caller (or a future retry script reading download_repo_files()'s `failed`
# attribute) can filter these out from every other kind of download failure
# and know a plain re-run, once the host's own quota has reset, is expected
# to succeed rather than needing any other fix.
.download_one <- function(url, dest, skip_on_api_limit = FALSE) {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  tryCatch({
    # Throttle realm = the host (explicit: the default derivation errors on
    # host-less URLs such as file://, which the tests use).
    host <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) NULL)
    if (is.null(host) || !length(host) || is.na(host) || !nzchar(host))
      host <- "local"
    req <- httr2::request(url) |>
      # A private file needs the token, or the OSF serves a sign-in page with
      # status 200 instead. See .auth_for_url().
      .auth_for_url() |>
      # Pace the requests instead of firing the whole batch back-to-back: a
      # leaky bucket per host that allows a short burst of 10, then sustains
      # ~1 request/second. Downloads are sequential either way; this only
      # inserts waits when the burst budget is spent, which is what keeps OSF
      # from answering 429.
      httr2::req_throttle(capacity = 10, fill_time_s = 10, realm = host) |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                       is_transient = .storage_is_transient_factory(skip_on_api_limit),
                       backoff = .storage_backoff,
                       after = .storage_retry_after_factory(skip_on_api_limit)) |>
      httr2::req_progress()
    httr2::req_perform(req, path = dest)
    if (.is_login_page(dest)) {
      unlink(dest)
      return("not authorised (the OSF returned a sign-in page; see ?osf_pat)")
    }
    if (file.exists(dest) && file.size(dest) > 0) NA_character_
    else "empty response"
  }, error = function(e) {
    if (file.exists(dest)) unlink(dest)
    # A 429 caught here (rather than retried away) means .storage_is_transient
    # -- via the skip_on_api_limit factory -- declared it non-transient, i.e.
    # gave up specifically because the bucket is confirmed exhausted. e$resp
    # is httr2's own attached response for an httr2_http_* condition.
    if (isTRUE(skip_on_api_limit) && inherits(e, "httr2_http_429") &&
        !is.null(e$resp) && !is.na(.rate_limit_wait(e$resp))) {
      return(paste0("API rate limit exhausted: ", conditionMessage(e)))
    }
    conditionMessage(e)
  })
}

# Download several files in parallel, with no proactive throttle. Only used for
# hosts verified not to need one: OSF's `download_url` and Zenodo's file `self`
# links both resolve (after a redirect through the host's own API) to
# pre-signed URLs on a cloud storage backend (OSF: Google Cloud Storage; the
# redirect is per-file, so there is no shared per-host bottleneck the way
# api.osf.io or Waterbutler's own listing endpoints have). A live burst of 23
# concurrent OSF download_url requests returned all-200 with no rate-limit
# headers surfacing at all (checked 2026-08-08), consistent with a CDN-backed
# signed-URL design rather than a per-client-limited API. Unlike
# `.download_one()`, this does NOT throttle -- do not route an unverified host
# through this path; `.download_one()`'s per-host throttle remains the default
# for everything else (e.g. ResearchBox), for exactly the reason its own
# comment gives.
#
# `expected_size` (optional, bytes, NA for "unknown"): a concurrent-download
# burst was observed once (2026-08-08) to leave a small number of files short
# of their reported size while still returning HTTP 200 (a partial write that
# passes a bare `size > 0` check) -- likely a dropped connection mid-transfer
# under parallel load, not seen when the same files were fetched again
# immediately after. Checking against the size OSF/Zenodo already reported in
# their file listing catches this as a retriable failure instead of silently
# keeping a truncated file.
#
# Returns a character vector the same length as `urls`, NA_character_ for a
# success and an error description for a failure -- same contract as
# `.download_one()`, so callers can loop over the result identically.
#
# skip_on_api_limit: see download_repo_files()'s own parameter of the same
# name. A 429 given up on because of it is reported with the same
# "API rate limit exhausted: " prefix .download_one() uses (see there for
# why: a fixed, greppable string a caller can filter on to know a plain
# re-run after the host's quota resets is expected to succeed).
.download_many_parallel <- function(urls, dests, expected_size = NA_real_,
                                    .retried = FALSE, skip_on_api_limit = FALSE) {
  if (length(urls) == 0) return(character(0))

  expected_size <- rep_len(as.numeric(expected_size), length(urls))
  for (d in dests) dir.create(dirname(d), showWarnings = FALSE, recursive = TRUE)

  reqs <- lapply(urls, \(url) {
    tryCatch({
      httr2::request(url) |>
        .auth_for_url() |>
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                         is_transient = .storage_is_transient_factory(skip_on_api_limit),
                         backoff = .storage_backoff,
                         after = .storage_retry_after_factory(skip_on_api_limit)) |>
        httr2::req_error(is_error = \(resp) FALSE)
    }, error = \(e) NULL)
  })

  valid <- !vapply(reqs, is.null, logical(1))
  resps <- vector("list", length(urls))
  # req_perform_parallel() is not faked by httptest2's mock-API mode (see the
  # note at the top of this file) -- same reason .batch_query() uses
  # req_perform_sequential() even in production. Mirror that here rather than
  # branching on a mocking flag, so tests exercise the real code path.
  mocking <- isTRUE(Sys.getenv("TESTTHAT") == "true")
  resps[valid] <- if (mocking) {
    httr2::req_perform_sequential(reqs[valid], paths = dests[valid],
                                  on_error = "continue", progress = FALSE)
  } else {
    httr2::req_perform_parallel(reqs[valid], paths = dests[valid],
                                on_error = "continue", progress = FALSE)
  }

  errs <- vapply(seq_along(urls), \(i) {
    if (!valid[i]) return("bad URL")
    r <- resps[[i]]
    if (inherits(r, "error")) {
      # req_perform*(path = ...) can create an empty/partial file at the
      # destination before the request itself fails (observed for a broken
      # file:// URL); .download_one() cleans this up on its own error branch,
      # do the same here so a failed row isn't mistaken for a successful one
      # by any caller that only checks file.exists().
      if (file.exists(dests[i])) unlink(dests[i])
      return(conditionMessage(r))
    }
    sc <- httr2::resp_status(r)
    # A file:// URL has no real HTTP status line -- httr2 reports 0 for it,
    # not an error, matching .download_one()'s tolerance (it never inspects
    # status for a local read, only whether the destination ended up
    # non-empty). Real HTTP failures (4xx/5xx) still get caught below.
    if (!sc %in% c(200, 0)) {
      if (file.exists(dests[i])) unlink(dests[i])
      if (isTRUE(skip_on_api_limit) && sc == 429L && !is.na(.rate_limit_wait(r)))
        return(paste0("API rate limit exhausted: HTTP ", sc))
      return(sprintf("HTTP %d", sc))
    }
    if (!file.exists(dests[i]) || file.size(dests[i]) == 0) {
      # httptest2's mock-API mode returns the response object in memory but
      # does not honour req_perform*(path = ...) streaming, so a real HTTP 200
      # with a real body still leaves no file on disk under mocking. Recover
      # by writing the already-fetched body directly; a genuine empty response
      # has no body either, so this still reports "empty response" for that
      # case (checked below).
      body <- tryCatch(httr2::resp_body_raw(r), error = \(e) raw(0))
      if (length(body) == 0) return("empty response")
      writeBin(body, dests[i])
    }
    # A sign-in page served in place of the file: the file is private and the
    # request was not authorised. Named as such, because "truncated" sent
    # readers looking for a network fault instead of a missing token.
    if (.is_login_page(dests[i], expected_size[i])) {
      unlink(dests[i])
      return("not authorised (the OSF returned a sign-in page; see ?osf_pat)")
    }
    exp <- expected_size[i]
    if (!is.na(exp) && exp > 0) {
      got <- file.size(dests[i])
      # `got` is NA when the file is not there at all, which is a failed
      # download rather than a truncated one; saying "truncated (NA of ...)"
      # described the symptom in a way that hid the cause.
      if (is.na(got)) {
        return("download failed (nothing was written)")
      }
      if (got != exp) {
        unlink(dests[i])
        return(sprintf("truncated (%.0f of %.0f bytes)", got, exp))
      }
    }
    NA_character_
  }, character(1))

  # One retry pass for size-mismatch/truncation only -- transient under a
  # parallel burst, not worth retrying an outright HTTP error or bad URL.
  # .retried stops this at one pass: an `expected_size` that is simply wrong
  # (as opposed to a genuinely truncated transfer) would otherwise recurse
  # forever, since every attempt reports the same "mismatch".
  retry <- if (.retried) integer(0) else which(!is.na(errs) & startsWith(errs, "truncated ("))
  if (length(retry) > 0) {
    retry_errs <- .download_many_parallel(urls[retry], dests[retry], expected_size[retry],
                                          .retried = TRUE, skip_on_api_limit = skip_on_api_limit)
    errs[retry] <- retry_errs
  }

  errs
}

# Download a repository as a single zip archive and extract only the requested
# files into the shared cache. Used instead of N individual file downloads for
# OSF (Waterbutler zip) and GitHub (API zipball).
#
# strip_dir: strip the first path component of each zip entry, because GitHub
#   zipball prepends "owner-repo-sha/" to every path; OSF waterbutler does not.
# req_func:  request configurator (.osf_headers or .github_config).
# max_bytes: hard cap enforced DURING the download itself (bytes; Inf = no
#   cap), not just a pre-flight Content-Length check. Needed because a
#   pre-flight size check can be wrong or entirely unavailable -- e.g. Dryad's
#   bulk-download endpoint never reports Content-Length (verified live
#   2026-08-30: HEAD 403s, GET is chunked with no length header), so its
#   gate upstream can only ever pass an ESTIMATE, not a confirmed size. Issue
#   #366 documented exactly this failure mode on OSF: a single failed text
#   file triggered a whole-repo zip fallback that pulled 2.47GB before timing
#   out, because the only protection was req_timeout(), which bounds wall-clock
#   time, not bytes -- a fast connection can pull many GB inside any
#   reasonable timeout. Streaming with req_perform_stream() and aborting once
#   the cap is crossed bounds the actual resource that matters (bytes), and
#   works even when no upstream size estimate exists at all.
#
# Returns `files` with file_location filled for successfully extracted rows.
# A row this fails to fill is left for the caller's file-by-file fallback to
# retry -- see skip_on_api_limit below for what that fallback then reports.
#
# skip_on_api_limit: see download_repo_files()'s own parameter of the same
# name. When FALSE (default), a confirmed-exhausted 429 here gets exactly
# ONE wait-and-retry (see the loop below); when TRUE, it gives up on the zip
# immediately without waiting at all, same as the per-file paths -- either
# way, whatever rows are still missing after this function returns fall
# through to download_repo_files()'s own file-by-file fallback, which is
# where skip_on_api_limit's "API rate limit exhausted: " message actually
# gets recorded (this function does not track per-file failures itself; it
# only fills what it can and leaves the rest for that fallback to report).
.download_zip_to_cache <- function(files, row_idx, zip_url,
                                   strip_dir = FALSE,
                                   req_func = identity,
                                   timeout_s = 120,
                                   max_bytes = Inf,
                                   skip_on_api_limit = FALSE) {
  zip_tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_tmp), add = TRUE)

  # One connection attempt, then (if it was rate-limited AND
  # skip_on_api_limit is FALSE) ONE retry after waiting for the host's own
  # stated reset -- not a general retry loop (see the streaming-corruption
  # note below, still accurate for repeated attempts), but a single
  # rate-limit-aware retry is safe: nothing has been written to `con` yet at
  # this point, so there is no partial-write risk the way a mid-stream retry
  # would have.
  resp <- NULL
  conn_err <- NULL
  for (attempt in 1:2) {
    req <- httr2::request(zip_url) |>
      req_func() |>
      httr2::req_error(is_error = \(r) FALSE)
    attempt_resp <- tryCatch(httr2::req_perform_connection(req), error = \(e) e)
    if (inherits(attempt_resp, "error")) {
      conn_err <- attempt_resp
      break
    }

    # A 429 never reaches the streaming loop below: its body is a short
    # error page/JSON, not archive bytes, and treating that as "downloaded
    # 37 bytes successfully" would go on to fail unzip() with a confusing
    # "not a zip file" error instead of a clear rate-limit one. Confirmed
    # live 2026-08-31 that Dryad's bulk-download endpoint DOES send the same
    # RateLimit-* headers on this path as its per-file endpoint.
    if (httr2::resp_status(attempt_resp) == 429L) {
      wait <- .rate_limit_wait(attempt_resp)
      close(attempt_resp)
      if (!isTRUE(skip_on_api_limit) && attempt == 1 && !is.na(wait) && wait > 0) {
        .announce_rate_limit_wait(wait)
        Sys.sleep(wait)
        next
      }
      return(files)  # rate-limited with no usable wait, skip_on_api_limit, or already retried once
    }
    resp <- attempt_resp
    break
  }
  if (is.null(resp)) {
    if (!is.null(conn_err))
      message(sprintf("Zip download failed (%s): %s", zip_url, conditionMessage(conn_err)))
    return(files)
  }

  dl_err <- tryCatch({
    on.exit(close(resp), add = TRUE)

    con <- file(zip_tmp, open = "wb")
    written <- 0
    over_cap <- FALSE
    timed_out <- FALSE
    deadline <- Sys.time() + timeout_s
    repeat {
      if (Sys.time() > deadline) {
        timed_out <- TRUE
        break
      }
      chunk <- httr2::resp_stream_raw(resp, kb = 512)
      if (length(chunk) == 0) {
        if (httr2::resp_stream_is_complete(resp)) break
        next
      }
      writeBin(chunk, con)
      written <- written + length(chunk)
      if (is.finite(max_bytes) && written > max_bytes) {
        over_cap <- TRUE
        break
      }
    }
    close(con)

    if (isTRUE(over_cap)) {
      sprintf("archive exceeded the %s MB cap during download and was aborted",
              .cap_num(round(max_bytes / (1024 * 1024))))
    } else if (isTRUE(timed_out)) {
      sprintf("archive download exceeded the %ds timeout", timeout_s)
    } else if (!file.exists(zip_tmp) || file.size(zip_tmp) == 0) {
      "empty response"
    } else {
      NA_character_
    }
  }, error = \(e) conditionMessage(e))

  if (!is.na(dl_err)) {
    message(sprintf("Zip download failed (%s): %s", zip_url, dl_err))
    return(files)
  }

  entries <- tryCatch(utils::unzip(zip_tmp, list = TRUE), error = \(e) NULL)
  if (is.null(entries) || nrow(entries) == 0) return(files)

  zip_entries  <- entries$Name
  zip_entries  <- zip_entries[!grepl("/$", zip_entries)]   # drop dir entries
  lookup_paths <- if (strip_dir) sub("^[^/]*/", "", zip_entries)
                  else zip_entries
  lookup_paths <- gsub("\\\\", "/", lookup_paths)
  lookup_paths <- gsub("^/+",  "",  lookup_paths)

  rel <- files$file_path[row_idx]
  empty_rel <- is.na(rel) | !nzchar(rel %||% "")
  rel[empty_rel] <- files$file_name[row_idx][empty_rel]
  rel <- gsub("\\\\", "/", rel)
  rel <- gsub("^/+",  "",  rel)

  matched_entry <- rep(NA_character_, length(row_idx))
  for (j in seq_along(row_idx)) {
    m <- match(rel[j], lookup_paths)
    if (!is.na(m)) matched_entry[j] <- zip_entries[m]
  }

  to_extract <- matched_entry[!is.na(matched_entry)]
  if (length(to_extract) == 0) return(files)

  extract_dir <- tempfile(pattern = "repo-zip-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  tryCatch(
    utils::unzip(zip_tmp, files = to_extract, exdir = extract_dir),
    error = \(e) NULL)

  for (j in seq_along(row_idx)) {
    if (is.na(matched_entry[j])) next
    src  <- file.path(extract_dir, matched_entry[j])
    dest <- files$.cache_path[row_idx[j]]
    if (file.exists(src) && file.size(src) > 0) {
      dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
      if (file.copy(src, dest, overwrite = TRUE))
        files$file_location[row_idx[j]] <- dest
    }
  }

  files
}


#' Download the files listed by `repo_check()`
#'
#' Fetches the bytes for a table of repository files (as produced by
#' `repo_check()`), writing each into a per-session temp directory or a
#' persistent on-disk cache, and fills in `file_location` for every file it
#' successfully retrieves. Where a whole-repo archive download is available
#' (OSF's Waterbutler `?zip=` endpoint, Zenodo's `files-archive` endpoint,
#' Dataverse's `/api/access/dataset` endpoint, Dryad's `stash:download`
#' endpoint, a GitHub zipball), it is used instead of one HTTP request per
#' file; a repo whose archive download fails, or is rejected by the
#' size/worth-it gate, falls back to file-by-file fetching automatically.
#' Figshare, 4TU.ResearchData (Figshare-compatible), and ReShare have no
#' documented whole-record bulk endpoint, so their files are always fetched
#' one by one.
#'
#' Two independent size gates apply: `max_file_size` skips oversize files
#' individually (the rest of the repository still downloads); `max_download_size`
#' is a budget on the repository's TOTAL cached footprint (already-cached plus
#' newly-downloaded), not on a single run's increment — already-cached files are
#' kept for free, then the smallest still-missing files are added until the next
#' one would exceed the budget, so re-running never grows a repository's cache
#' past the budget and the canonical "files we ever download" set is fixed.
#'
#' @param files a data.frame of files (as returned by `repo_check()`'s
#'   `table`): needs `repo_url`, `file_url`, and `file_path` or `file_name`
#' @param max_file_size largest single file to download, in MB
#' @param max_download_size largest total cached footprint per repository, in MB
#' @param zip_timeout_s timeout (seconds) for a whole-repo zip download attempt
#'   before falling back to file-by-file fetching
#' @param cache if `TRUE`, write into the persistent rappdirs cache (survives
#'   across sessions, never cleared automatically — see `repo_cache_clear()`);
#'   if `FALSE` (default), write into a per-session temp directory that R
#'   removes on exit
#' @param skip_on_api_limit if `FALSE` (the default), a download that hits a
#'   host-confirmed exhausted rate limit (a 429 whose response carries
#'   `RateLimit-Remaining: 0`/`X-RateLimit-Remaining: 0` and a parseable
#'   reset time — currently known to apply to Dryad, Zenodo, GitHub, and
#'   GitLab; other hosts do not expose this and are unaffected either way)
#'   waits for the host's own stated reset before retrying, however long that
#'   is — appropriate for an unattended corpus build, where completeness
#'   matters more than wall-clock time. Set `TRUE` to give up on that file
#'   immediately instead: it is recorded in the `"failed"` attribute with an
#'   `"API rate limit exhausted: "`-prefixed message, filterable from every
#'   other kind of failure, so a caller can re-run later (once the quota has
#'   reset) targeting only those specific files rather than waiting inline.
#'   An ordinary transient refusal (a 429 without a confirmed exhausted-quota
#'   signal, a 403/5xx) still retries with ordinary backoff regardless of
#'   this setting — it only changes what happens once a real quota
#'   exhaustion is confirmed.
#' @param pb an optional progress bar object (see `pb()`), ticked as files
#'   download
#'
#' @returns `files` with `file_location` filled in for every file retrieved
#'   (unchanged, i.e. `NA`, for files that were skipped or failed), and three
#'   attributes: `"gated"` (data.frame: `repo_url`, `message` — repositories
#'   refused outright by the size caps), `"oversize_skipped"` (data.frame:
#'   `repo_url`, `file_name`, `file_size` — individual files skipped under
#'   `max_file_size`), and `"failed"` (data.frame: `repo_url`, `file_name`,
#'   `error` — files whose download was attempted but errored, e.g. a
#'   transient network failure, or — when `skip_on_api_limit = TRUE` and the
#'   host confirmed it — an exhausted API rate limit; see `skip_on_api_limit`).
#' @export
#' @keywords internal
download_repo_files <- function(files,
                                max_file_size = 100,
                                max_download_size = 500,
                                zip_timeout_s = 120,
                                cache = FALSE,
                                skip_on_api_limit = FALSE,
                                pb = NULL) {
  if (is.null(files) || nrow(files) == 0) return(files)
  if (!"file_location" %in% names(files))
    files$file_location <- NA_character_

  # Where downloaded files are written.
  #   cache = TRUE  → the persistent rappdirs cache: files survive the session
  #     and are reused on later runs (but the cache grows without bound and is
  #     never cleared automatically; see repo_cache_clear()).
  #   cache = FALSE (default) → a per-session temp directory that R removes on
  #     exit: files are available for this run only, nothing accumulates on disk.
  # The layout (per-repo subdir, repo-relative path) is identical either way, so
  # the rest of the function does not care which root is in use.
  cache_root <- if (isTRUE(cache)) NULL else .repo_session_dir()
  cache_path <- function(repo_url, file_path) {
    if (is.null(cache_root)) .repo_cache_path(repo_url, file_path)
    else file.path(cache_root, .repo_cache_rel(repo_url, file_path))
  }

  # Cache path per file; reuse anything already cached.
  rel_path <- files$file_path %||% files$file_name
  rel_path <- ifelse(is.na(rel_path), files$file_name, rel_path)
  files$.cache_path <- vapply(seq_len(nrow(files)), function(i) {
    # Guard the cache path against the OS path-length limit (a deeply nested repo
    # file under a long OneDrive root can exceed ~260 chars). .safe_write_path
    # shortens + warns; using its result for BOTH the download target and the
    # recorded file_location keeps the on-disk name and the record in agreement.
    .safe_write_path(cache_path(files$repo_url[i], rel_path[i]))
  }, character(1))

  already <- file.exists(files$.cache_path)
  files$file_location[already] <- files$.cache_path[already]

  # ── Archive members (repo_check's zip-peek expansion) ───────────────────────
  # A row from repo_check's zip-peek expansion (inst/modules/repo_check.R,
  # "look inside .zip archives") has file_url = NA -- an entry inside an
  # archive has no address a plain GET/HEAD can request -- but carries
  # archive_url (the parent zip's own URL) and archive_member (its exact name
  # within the archive, from zip_peek()'s own listing) instead. Fetch those
  # here, via the same per-member byte-range mechanism
  # (.zip_fetch_members()/.zip_member_fetch(), R/zip-peek.R) every
  # archive-*.R platform file already uses for its own zip-vs-file-by-file
  # download decision -- this just reaches it from the OTHER direction, a row
  # that already knows it wants ONE named member rather than "the whole zip,
  # filtered by type/size". Still gated by cache reuse (`already`, above) and
  # skipped when it fails (host cannot range-request, member cannot be
  # located): file_location stays NA and the row is simply never downloaded,
  # same as any other unresolvable file. Does not touch a row with a real
  # file_url, so this is purely additive for archive-member rows.
  if ("archive_url" %in% names(files)) {
    is_member <- !already & !is.na(files$archive_url) & nzchar(files$archive_url %||% "") &
      !is.na(files$archive_member %||% NA_character_)
    if (any(is_member)) {
      for (arc in unique(files$archive_url[is_member])) {
        idx <- which(is_member & files$archive_url == arc)
        # A member's cache destination must NOT reuse .cache_path (built from
        # the display file_path, which embeds the archive's own file name as
        # a path component, e.g. ".../SharpRT/CMV_Spread-v1.0.zip/Program/
        # main.m"): if that same archive was ALSO downloaded whole by the
        # normal file_url path (a real FILE at ".../CMV_Spread-v1.0.zip"),
        # creating a directory of the identical name and path fails on
        # Windows -- confirmed live, "cannot create dir ... reason 'No such
        # file or directory'" for exactly this collision. Instead, key the
        # extraction directory off the ARCHIVE's own URL (repo-relative, via
        # the same cache_path() helper every other file in this function
        # uses, just with a synthetic relative path that cannot collide with
        # a real file_path), suffixed ".contents" to match data_check's own
        # zip-expansion convention (.expand_zip(), R/zip-peek.R: dest <-
        # paste0(zip_path, ".contents")).
        arc_key <- gsub("^https?://", "", arc)
        arc_key <- gsub("[^A-Za-z0-9._-]+", "_", arc_key)
        member_dest <- paste0(cache_path(files$repo_url[idx[1]],
                                         file.path(".archive_members", arc_key)),
                              ".contents")
        member_dest <- .safe_write_path(member_dest)
        dir.create(member_dest, showWarnings = FALSE, recursive = TRUE)
        fetched <- tryCatch(
          .zip_fetch_members(arc, names = files$archive_member[idx], dest = member_dest),
          error = function(e) NULL)
        if (is.null(fetched)) next
        for (k in idx) {
          row <- fetched[fetched$name == files$archive_member[k] & fetched$ok %in% TRUE, , drop = FALSE]
          if (nrow(row) == 0 || is.na(row$path[[1]])) next
          files$file_location[k] <- row$path[[1]]
        }
      }
    }
  }

  has_url <- !is.na(files$file_url) & nzchar(files$file_url)
  mb <- 1024 * 1024

  gated <- data.frame(repo_url = character(0), message = character(0),
                      stringsAsFactors = FALSE)
  # Individual files skipped because they exceed max_file_size (the repo is still
  # downloaded — only these files are left out). Recorded for reporting.
  oversize_skipped <- data.frame(repo_url = character(0), file_name = character(0),
                                 file_size = numeric(0), stringsAsFactors = FALSE)
  to_get <- integer(0)

  # ── Per-repository budget + per-file size filter ────────────────────────────
  # max_file_size is a PER-FILE filter: files over it are skipped individually,
  # the rest of the repo still downloads. max_download_size is a REPO BUDGET on
  # the TOTAL cached footprint of the repo (already-cached + newly-downloaded),
  # NOT on a single run's increment. So re-running never grows a repo's cache
  # beyond the budget: the canonical set of files "we ever download" is fixed —
  # already-cached files are kept for free, then the smallest still-missing files
  # are added until the next one would exceed the budget. A repo whose kept data
  # is under the budget downloads fully; a larger one downloads its smallest
  # files up to the cap (partial fill), the same set every run.
  #
  # The budget is computed over ALL kept files (cached + missing), so the loop
  # runs for every repo with URLs, not only those with missing files.
  for (repo in unique(files$repo_url[has_url])) {
    idx <- which(files$repo_url == repo & has_url)
    if (length(idx) == 0) next
    is_cached <- already[idx]

    # Resolve sizes (bytes): manifest size; for a cached file the on-disk size;
    # else a HEAD Content-Length probe for a still-missing file.
    sizes <- as.numeric(files$file_size[idx])
    for (k in which(is.na(sizes))) {
      if (is_cached[k]) {
        sz <- tryCatch(file.size(files$.cache_path[idx[k]]), error = function(e) NA_real_)
        sizes[k] <- if (is.finite(sz)) sz else NA_real_
      } else {
        sizes[k] <- .remote_size(files$file_url[idx[k]])
      }
    }

    repo_file_cap  <- max_file_size
    repo_total_cap <- max_download_size

    # Per-file filter: drop files over the per-file cap (keep the rest). Reported
    # only for missing files — a cached oversize file is already on disk.
    over <- if (is.finite(repo_file_cap)) !is.na(sizes) & sizes > repo_file_cap * mb
            else rep(FALSE, length(idx))
    if (any(over & !is_cached)) {
      o <- over & !is_cached
      oversize_skipped <- rbind(oversize_skipped, data.frame(
        repo_url  = repo,
        file_name = files$file_name[idx[o]],
        file_size = sizes[o],
        stringsAsFactors = FALSE))
    }

    # Unknown-size files are excluded from the candidate set (we cannot budget
    # them), rather than gating the whole repo. Candidates = kept, known-size.
    cand <- !over & !is.na(sizes)
    if (!any(cand)) next
    c_idx    <- idx[cand]
    c_size   <- sizes[cand]
    c_cached <- is_cached[cand]

    # Canonical "ever-download" set under the budget. Cached files are kept for
    # free (never re-downloaded, never re-counted against a run), then the
    # smallest still-missing files are added until the budget is spent. This
    # makes the set deterministic and idempotent: the same repo yields the same
    # set every run, so re-running only tops up toward the cap, never past it.
    cap_bytes <- if (is.finite(repo_total_cap)) repo_total_cap * mb else Inf
    used <- sum(c_size[c_cached])                       # cached footprint (kept)
    missing_order <- which(!c_cached)
    missing_order <- missing_order[order(c_size[missing_order],
                                         files$.cache_path[c_idx[missing_order]])]
    take_missing <- integer(0)
    for (k in missing_order) {
      if (used + c_size[k] <= cap_bytes) {
        take_missing <- c(take_missing, k)
        used <- used + c_size[k]
      }
      # keep scanning: a later, smaller file may still fit under the budget
    }

    # Download only the newly-selected (still-missing) members of the set.
    to_get <- c(to_get, c_idx[take_missing])

    # Report a partial fill: some kept files were left out because the budget was
    # reached (distinct from the per-file oversize skips reported separately).
    omitted <- setdiff(missing_order, take_missing)
    if (length(omitted) > 0 && is.finite(cap_bytes)) {
      msg <- sprintf(
        paste0("Repository %s exceeds the %s MB per-repository budget: ",
               "downloaded the smallest files up to the cap, %d file%s omitted. ",
               "Raise `max_download_size` to include more."),
        repo, .cap_num(repo_total_cap),
        length(omitted), plural(length(omitted)))
      cap_report(msg)
      gated <- rbind(gated, data.frame(repo_url = repo, message = msg,
                                       stringsAsFactors = FALSE))
    }
  }

  # Report the individually-skipped oversized files (one message per repo).
  if (nrow(oversize_skipped) > 0) {
    for (repo in unique(oversize_skipped$repo_url)) {
      sub <- oversize_skipped[oversize_skipped$repo_url == repo, ]
      message(sprintf(
        paste0("%d file%s in %s exceeded the %s MB per-file limit and %s ",
               "skipped (the rest of the repository was downloaded). ",
               "Largest: %s. Raise max_file_size to include them."),
        nrow(sub), plural(nrow(sub)), repo, .cap_num(max_file_size),
        if (nrow(sub) == 1) "was" else "were",
        paste(sprintf("%s (%s MB)", sub$file_name[order(-sub$file_size)][1],
                      .cap_num(round(max(sub$file_size) / mb))))))
    }
  }

  # ── Download the files of repositories that passed the gate ─────────────────
  # For OSF repos, use the Waterbutler zip endpoint (one request for all of
  # osfstorage). For Zenodo repos, use the files-archive endpoint (one request
  # for the whole record). For Dataverse repos, use the /api/access/dataset
  # endpoint (one request for the whole dataset). For Dryad repos, use the
  # dataset's own stash:download endpoint (one request for the whole dataset).
  # For GitHub repos, use the API zipball (one request, follows the redirect to
  # the signed download URL). All five are cheaper and less rate-limit-sensitive
  # than N individual file requests. Figshare has no such endpoint (see
  # figshare_file_download()'s documentation) so it always falls through to the
  # file-by-file path below. Any repo whose zip download fails, or that the
  # zip-vs-file-by-file gate rejects, falls through the same way.
  failed <- data.frame(repo_url = character(0), file_name = character(0),
                       error = character(0), stringsAsFactors = FALSE)
  if (length(to_get) > 0) {
    remaining <- to_get

    # ── OSF: Waterbutler zip ────────────────────────────────────────────────────
    osf_repos <- unique(files$repo_url[
      remaining[grepl("osf\\.io", files$repo_url[remaining], ignore.case = TRUE)]])
    # Is a file on OSF's osfstorage (the only provider the ?zip= endpoint
    # covers)? Authoritative source is the `provider` column repo_check now
    # carries; fall back to the file_url form for older rows that predate it.
    is_osfstorage <- function(i) {
      if ("provider" %in% names(files) && !is.na(files$provider[i]))
        return(identical(tolower(files$provider[i]), "osfstorage"))
      url <- files$file_url[i]
      if (is.na(url) || !nzchar(url)) return(FALSE)
      grepl("/providers/osfstorage/", url, ignore.case = TRUE)
    }
    for (repo in osf_repos) {
      ridx <- intersect(remaining, which(files$repo_url == repo))
      if (length(ridx) == 0) next
      # Waterbutler zip only covers osfstorage. Non-osfstorage rows fall through
      # to file-by-file download below as a complement path.
      ridx_zip <- ridx[vapply(ridx, is_osfstorage, logical(1))]
      if (length(ridx_zip) == 0) next
      osf_id <- tryCatch(osf_check_id(repo), error = \(e) NA_character_)
      # Only use zip for 5-char node GUIDs (not waterbutler folder IDs)
      if (is.na(osf_id) || !nzchar(osf_id %||% "") || nchar(osf_id) != 5) next
      zip_url <- sprintf(
        "https://files.osf.io/v1/resources/%s/providers/osfstorage/?zip=", osf_id)
      zip_bytes <- .remote_content_length(zip_url)

      # ── Zip-vs-file-by-file decision ─────────────────────────────────────────
      # The ?zip= endpoint is all-or-nothing: it always zips the WHOLE node's
      # osfstorage, not just the files we want. So it is only worth taking when
      # the one-shot transport is not wildly bigger than what we'd fetch
      # individually. Take the zip when BOTH:
      #   (1) size: the zip is <= 2x the per-repo budget (a one-request transport
      #       earns a 2x allowance over the per-run download budget); AND
      #   (2) worth it: EITHER we want more than 50 files from this node (at that
      #       many individual requests the per-request latency + OSF rate-limiting
      #       make the zip faster regardless of a little wasted data), OR the node
      #       holds no more than 2x the files we actually want (so the zip drags
      #       in at most one unwanted file per wanted one).
      # `node_osf_n` counts EVERY osfstorage file in the node (the zip's real
      # payload), not just the wanted subset, so the waste ratio is honest for
      # mixed / partially-filtered nodes. When zip_bytes is unknown the size gate
      # cannot be checked, so we do not risk an unbounded transport: fall back.
      n_wanted   <- length(ridx_zip)
      node_osf_n <- sum(files$repo_url == repo &
                        vapply(seq_len(nrow(files)), is_osfstorage, logical(1)))
      size_ok  <- !is.na(zip_bytes) &&
        (!is.finite(max_download_size) || zip_bytes <= 2 * max_download_size * mb)
      worth_it <- n_wanted > 50L || node_osf_n <= 2L * n_wanted
      if (!isTRUE(size_ok && worth_it)) {
        why <- if (!size_ok)
          sprintf("zip transport %s MB exceeds 2x the %s MB budget",
                  if (is.na(zip_bytes)) "unknown" else
                    .cap_num(round(zip_bytes / mb)), .cap_num(max_download_size))
        else
          sprintf("node holds %d osfstorage files for %d wanted (>2x) and wanted <= 50",
                  node_osf_n, n_wanted)
        message(sprintf(
          "Skipping zip for %s (%s); downloading its files individually.",
          repo, why))
        next   # leaves ridx in `remaining` for the file-by-file loop below
      }

      expected_bytes <- sum(as.numeric(files$file_size[ridx_zip]), na.rm = TRUE)
      if (!is.na(zip_bytes) && expected_bytes > 0 && zip_bytes > expected_bytes) {
        message(sprintf(
          paste0("Repository %s downloads as one archive of %s MB to extract ",
                 "%s MB of selected files (whole-node osfstorage zip)."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(round(expected_bytes / mb))))
      }
      message(sprintf("Downloading %s as zip (%d file%s)...",
                      repo, length(ridx_zip), plural(length(ridx_zip))))
      files <- .download_zip_to_cache(files, ridx_zip, zip_url,
                                      strip_dir = FALSE,
                                      req_func = .osf_headers,
                                      timeout_s = zip_timeout_s,
                                      max_bytes = max_download_size * mb,
                                      skip_on_api_limit = skip_on_api_limit)
      remaining <- setdiff(remaining, ridx_zip[!is.na(files$file_location[ridx_zip])])
    }

    # ── Zenodo: files-archive ────────────────────────────────────────────────────
    # Undocumented in Zenodo's public API reference, but a stable first-party
    # endpoint (the same URL Zenodo's own record pages link to as "Download
    # all"); verified live to return a proper zip. Like OSF's ?zip=, it is
    # all-or-nothing for the record, so the same size/worth-it gate applies.
    zen_repos <- unique(files$repo_url[
      remaining[grepl("zenodo", files$repo_url[remaining], ignore.case = TRUE)]])
    for (repo in zen_repos) {
      ridx <- intersect(remaining, which(files$repo_url == repo))
      if (length(ridx) == 0) next
      zenodo_id <- tryCatch(.zenodo_id(repo), error = \(e) NA_character_)
      if (is.na(zenodo_id) || !nzchar(zenodo_id %||% "")) next
      zip_url <- sprintf("https://zenodo.org/api/records/%s/files-archive", zenodo_id)
      zip_bytes <- .remote_content_length(zip_url)

      # Same zip-vs-file-by-file decision as OSF (see comment above): take the
      # archive when its transport size is not wildly bigger than the per-repo
      # budget, and either most of the record's files are wanted or the
      # archive would not drag in more than 2x unwanted files.
      n_wanted   <- length(ridx)
      record_n   <- sum(files$repo_url == repo)
      size_ok  <- !is.na(zip_bytes) &&
        (!is.finite(max_download_size) || zip_bytes <= 2 * max_download_size * mb)
      worth_it <- n_wanted > 50L || record_n <= 2L * n_wanted
      if (!isTRUE(size_ok && worth_it)) {
        why <- if (!size_ok)
          sprintf("zip transport %s MB exceeds 2x the %s MB budget",
                  if (is.na(zip_bytes)) "unknown" else
                    .cap_num(round(zip_bytes / mb)), .cap_num(max_download_size))
        else
          sprintf("record holds %d files for %d wanted (>2x) and wanted <= 50",
                  record_n, n_wanted)
        message(sprintf(
          "Skipping zip for %s (%s); downloading its files individually.",
          repo, why))
        next   # leaves ridx in `remaining` for the file-by-file loop below
      }

      expected_bytes <- sum(as.numeric(files$file_size[ridx]), na.rm = TRUE)
      if (!is.na(zip_bytes) && expected_bytes > 0 && zip_bytes > expected_bytes) {
        message(sprintf(
          paste0("Repository %s downloads as one archive of %s MB to extract ",
                 "%s MB of selected files (whole-record Zenodo archive)."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(round(expected_bytes / mb))))
      }
      message(sprintf("Downloading %s as zip (%d file%s)...",
                      repo, length(ridx), plural(length(ridx))))
      files <- .download_zip_to_cache(files, ridx, zip_url,
                                      strip_dir = FALSE,
                                      timeout_s = zip_timeout_s,
                                      max_bytes = max_download_size * mb,
                                      skip_on_api_limit = skip_on_api_limit)
      remaining <- setdiff(remaining, ridx[!is.na(files$file_location[ridx])])
    }

    # ── Dataverse: whole-dataset archive ────────────────────────────────────────
    # /api/access/dataset/:persistentId is Dataverse's documented bulk-download
    # endpoint (the same URL a dataset page's own "Download All" button uses).
    # Like Zenodo's files-archive and OSF's ?zip=, it is all-or-nothing for the
    # dataset, so the same size/worth-it gate applies. Unlike those two hosts,
    # Dataverse is many independent installations, so the request needs
    # .dataverse_headers() for that installation's own API token (see
    # archive-dataverse.R) rather than a single shared auth scheme.
    dv_repos <- unique(files$repo_url[
      remaining[grepl(.dataverse_host_regex(), files$repo_url[remaining], ignore.case = TRUE)]])
    for (repo in dv_repos) {
      ridx <- intersect(remaining, which(files$repo_url == repo))
      if (length(ridx) == 0) next
      parsed <- .dataverse_parse(repo)
      host <- parsed$host[[1]]
      doi <- parsed$doi[[1]]
      if (is.na(host) || is.na(doi)) next
      zip_url <- sprintf(
        "https://%s/api/access/dataset/:persistentId/?persistentId=doi:%s",
        host, doi)
      zip_bytes <- .remote_content_length(zip_url)

      # Same zip-vs-file-by-file decision as Zenodo/OSF (see comments above).
      n_wanted   <- length(ridx)
      record_n   <- sum(files$repo_url == repo)
      size_ok  <- !is.na(zip_bytes) &&
        (!is.finite(max_download_size) || zip_bytes <= 2 * max_download_size * mb)
      worth_it <- n_wanted > 50L || record_n <= 2L * n_wanted
      if (!isTRUE(size_ok && worth_it)) {
        why <- if (!size_ok)
          sprintf("zip transport %s MB exceeds 2x the %s MB budget",
                  if (is.na(zip_bytes)) "unknown" else
                    .cap_num(round(zip_bytes / mb)), .cap_num(max_download_size))
        else
          sprintf("dataset holds %d files for %d wanted (>2x) and wanted <= 50",
                  record_n, n_wanted)
        message(sprintf(
          "Skipping zip for %s (%s); downloading its files individually.",
          repo, why))
        next   # leaves ridx in `remaining` for the file-by-file loop below
      }

      expected_bytes <- sum(as.numeric(files$file_size[ridx]), na.rm = TRUE)
      if (!is.na(zip_bytes) && expected_bytes > 0 && zip_bytes > expected_bytes) {
        message(sprintf(
          paste0("Repository %s downloads as one archive of %s MB to extract ",
                 "%s MB of selected files (whole-dataset Dataverse archive)."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(round(expected_bytes / mb))))
      }
      message(sprintf("Downloading %s as zip (%d file%s)...",
                      repo, length(ridx), plural(length(ridx))))
      files <- .download_zip_to_cache(files, ridx, zip_url,
                                      strip_dir = FALSE,
                                      req_func = .dataverse_headers,
                                      timeout_s = zip_timeout_s,
                                      max_bytes = max_download_size * mb,
                                      skip_on_api_limit = skip_on_api_limit)
      remaining <- setdiff(remaining, ridx[!is.na(files$file_location[ridx])])
    }

    # ── Dryad: whole-dataset archive ────────────────────────────────────────────
    # /api/v2/datasets/<encoded-doi>/download is Dryad's documented bulk-
    # download endpoint (verified live: it is the "stash:download" link
    # carried on every dataset's own API response, the same link a dataset
    # page's own "Download Dataset" button uses). Like Dataverse's
    # /api/access/dataset and Zenodo's files-archive, it is all-or-nothing for
    # the dataset, so the same size/worth-it gate applies.
    #
    # repo_url is whatever a paper cited -- confirmed against a real 1861-paper
    # corpus run that every single Dryad repo_url is the doi.org citation form
    # (e.g. "https://doi.org/10.5061/dryad.8sf7m0cjc"), never a bare
    # datadryad.org URL, even though file_url (the per-file download link) is
    # always datadryad.org. Matching only "datadryad\\.org" here missed every
    # one of them, so this whole branch was silently dead code for the entire
    # corpus: every Dryad dataset fell through to file-by-file downloads (2 + N
    # requests each, one per file), which is what was driving the 429s seen at
    # corpus scale -- not a missing token (file_url downloads were already
    # authorised correctly by .auth_for_url()).
    dryad_repos <- unique(files$repo_url[
      remaining[grepl("datadryad\\.org|doi\\.org/10\\.5061/dryad",
                      files$repo_url[remaining], ignore.case = TRUE)]])
    for (repo in dryad_repos) {
      ridx <- intersect(remaining, which(files$repo_url == repo))
      if (length(ridx) == 0) next
      doi <- tryCatch(.dryad_doi(repo), error = \(e) NA_character_)
      if (is.na(doi) || !nzchar(doi %||% "")) next
      encoded <- utils::URLencode(paste0("doi:", doi), reserved = TRUE)
      zip_url <- sprintf("https://datadryad.org/api/v2/datasets/%s/download", encoded)
      zip_bytes <- .remote_content_length(zip_url, req_func = .dryad_headers)

      # Dryad's bulk-download endpoint cannot report its size ahead of time by
      # ANY method (verified live 2026-08-30): a HEAD gets 403
      # InvalidSignatureException (the endpoint proxies to a pre-signed S3 URL
      # whose signature is verb-specific and does not validate for HEAD), and
      # a GET answers 200 with Transfer-Encoding: chunked and no
      # Content-Length header at all. So zip_bytes is always NA here, unlike
      # every other backend in this function where the live probe usually
      # works. Falling back to "unknown size means skip the zip" (as issue
      # #366 recommends when size truly cannot be confirmed) would make this
      # whole branch permanently dead for Dryad regardless of the URL-matching
      # fix above -- so estimate instead, from sizes repo_check() already
      # listed for this dataset. A zip is compressed, so the true download is
      # normally smaller than this sum: a conservative (not optimistic)
      # stand-in for the live probe.
      expected_bytes <- sum(as.numeric(files$file_size[ridx]), na.rm = TRUE)
      if (is.na(zip_bytes) && expected_bytes > 0) zip_bytes <- expected_bytes

      # Same zip-vs-file-by-file request-COUNT decision as Dataverse/Zenodo/OSF
      # (see comments above) -- but Dryad additionally gates on a QUOTA-aware
      # threshold the other hosts don't get, because Dryad uniquely (confirmed
      # 2026-09-01 against its own open-source rate-limit config,
      # datadryad/dryad-app config/initializers/rack_attack.rb +
      # config/app_config.yml) runs the zip and per-file download paths
      # through two INDEPENDENT per-IP quota buckets with very different
      # sizes: zip_downloads_per_day = 100, file_downloads_per_day = 500 (5x
      # more headroom), each tracked under its own Rack::Attack cache key
      # regardless of how many bytes/files a request involves. The plain
      # request-count logic above (worth_it) always prefers zip once a
      # dataset's wanted files are >=half of it (true for nearly every
      # real dataset, since callers almost always want "all of it") -- which
      # means EVERY Dryad dataset routes through the strict 100/day zip
      # bucket while the far larger 500/day file bucket sits unused.
      #
      # Threshold raised from 4 to 12 files on 2026-09-01, mid-corpus-run:
      # even with the original threshold of 4, the ZIP BUCKET'S HOURLY CAP
      # (20/hour, separate from and much tighter than the 100/day ceiling)
      # was still being tripped repeatedly -- confirmed live against the
      # real Cooper-validation corpus run, batch 29, hitting the same
      # 5-file dataset on three consecutive restarts because the hourly
      # bucket had no time to recover between them (restarting the SCRIPT
      # does not reset the HOST's quota clock; only real elapsed time does).
      # Recomputed against the corpus data on disk at the time (873 Dryad
      # datasets): threshold 4 -> 190 zip requests corpus-wide (9.5/hour
      # average if spent over the minimum 2 zip-days, i.e. routinely
      # exceeding the 20/hour ceiling on its own even before accounting for
      # a real run's uneven, bursty pacing); threshold 12 -> only 33 zip
      # requests corpus-wide (1.6/hour average), a much larger safety
      # margin against the hourly wall specifically. This trades away some
      # of the daily-quota-optimality the threshold-sweep analysis found
      # (12 is not the sweep's minimum-day threshold, 3-6 was) for
      # practical hourly headroom that matters more once a real run is
      # actually hitting the hourly cap repeatedly -- a deliberate,
      # observed-behaviour-driven adjustment, not a re-run of that sweep.
      n_wanted   <- length(ridx)
      record_n   <- sum(files$repo_url == repo)
      size_ok  <- !is.na(zip_bytes) &&
        (!is.finite(max_download_size) || zip_bytes <= 2 * max_download_size * mb)
      quota_worth_it <- n_wanted > 12L
      worth_it <- (n_wanted > 50L || record_n <= 2L * n_wanted) && quota_worth_it
      if (!isTRUE(size_ok && worth_it)) {
        why <- if (!size_ok)
          sprintf("zip transport %s MB exceeds 2x the %s MB budget",
                  if (is.na(zip_bytes)) "unknown" else
                    .cap_num(round(zip_bytes / mb)), .cap_num(max_download_size))
        else if (!quota_worth_it)
          sprintf("only %d file%s wanted -- Dryad's zip quota (100/day) is 5x stricter than its per-file quota (500/day), not worth spending on a small dataset",
                  n_wanted, plural(n_wanted))
        else
          sprintf("dataset holds %d files for %d wanted (>2x) and wanted <= 50",
                  record_n, n_wanted)
        message(sprintf(
          "Skipping zip for %s (%s); downloading its files individually.",
          repo, why))
        next   # leaves ridx in `remaining` for the file-by-file loop below
      }

      if (expected_bytes > 0 && zip_bytes > expected_bytes) {
        message(sprintf(
          paste0("Repository %s downloads as one archive of %s MB to extract ",
                 "%s MB of selected files (whole-dataset Dryad archive)."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(round(expected_bytes / mb))))
      }
      message(sprintf("Downloading %s as zip (%d file%s)...",
                      repo, length(ridx), plural(length(ridx))))
      files <- .download_zip_to_cache(files, ridx, zip_url,
                                      strip_dir = FALSE,
                                      req_func = .dryad_headers,
                                      timeout_s = zip_timeout_s,
                                      max_bytes = max_download_size * mb,
                                      skip_on_api_limit = skip_on_api_limit)
      remaining <- setdiff(remaining, ridx[!is.na(files$file_location[ridx])])
    }

    # ── GitHub: API zipball ─────────────────────────────────────────────────────
    gh_repos <- unique(files$repo_url[
      remaining[grepl("github\\.com", files$repo_url[remaining], ignore.case = TRUE)]])
    for (repo in gh_repos) {
      ridx <- intersect(remaining, which(files$repo_url == repo))
      if (length(ridx) == 0) next
      clean_repo <- tryCatch(github_repo(repo), error = \(e) NULL)
      if (is.null(clean_repo)) next
      # Omitting the ref makes GitHub use the default branch.
      zip_url <- sprintf("https://api.github.com/repos/%s/zipball", clean_repo)
      zip_bytes <- .remote_content_length(zip_url)
      expected_bytes <- sum(as.numeric(files$file_size[ridx]), na.rm = TRUE)
      if (!is.na(zip_bytes) && expected_bytes > 0 && zip_bytes > expected_bytes) {
        warning(sprintf(
          paste0("Repository %s will be downloaded as a larger archive transport ",
                 "(%s MB) than the selected file estimate (%s MB)."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(round(expected_bytes / mb))
        ), call. = FALSE)
      }
      if (!is.na(zip_bytes) && is.finite(max_download_size) && zip_bytes > max_download_size * mb) {
        warning(sprintf(
          paste0("Repository %s archive transport is %s MB, above max_download_size ",
                 "(%s MB). Continuing by design because transport is one-shot zip."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(max_download_size)
        ), call. = FALSE)
      }
      message(sprintf("Downloading %s as zip (%d file%s)...",
                      repo, length(ridx), plural(length(ridx))))
      files <- .download_zip_to_cache(files, ridx, zip_url,
                                      strip_dir = TRUE,
                                      req_func = .github_config,
                                      timeout_s = zip_timeout_s,
                                      skip_on_api_limit = skip_on_api_limit)
      remaining <- setdiff(remaining, ridx[!is.na(files$file_location[ridx])])
    }

    # ── GitLab: repository archive.zip ──────────────────────────────────────────
    # GitLab's archive.zip does not support HTTP Range requests (confirmed
    # live: a Range header request comes back 200, not 206, accept-ranges:
    # none), so it also has no content-length up front (it is a streamed
    # response) -- .remote_content_length() below will simply return NA for
    # it, same as it already does for any endpoint without that header; the
    # size-comparison warnings just do not fire, downloading still proceeds.
    gl_repos <- unique(files$repo_url[
      remaining[grepl("gitlab\\.com", files$repo_url[remaining], ignore.case = TRUE)]])
    for (repo in gl_repos) {
      ridx <- intersect(remaining, which(files$repo_url == repo))
      if (length(ridx) == 0) next
      clean_repo <- tryCatch(gitlab_repo(repo), error = \(e) NULL)
      if (is.null(clean_repo)) next
      proj_id <- .gitlab_project_id(clean_repo)
      # Omitting sha makes GitLab use the default branch.
      zip_url <- sprintf("https://gitlab.com/api/v4/projects/%s/repository/archive.zip",
                         proj_id)
      zip_bytes <- .remote_content_length(zip_url)
      expected_bytes <- sum(as.numeric(files$file_size[ridx]), na.rm = TRUE)
      if (!is.na(zip_bytes) && expected_bytes > 0 && zip_bytes > expected_bytes) {
        warning(sprintf(
          paste0("Repository %s will be downloaded as a larger archive transport ",
                 "(%s MB) than the selected file estimate (%s MB)."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(round(expected_bytes / mb))
        ), call. = FALSE)
      }
      if (!is.na(zip_bytes) && is.finite(max_download_size) && zip_bytes > max_download_size * mb) {
        warning(sprintf(
          paste0("Repository %s archive transport is %s MB, above max_download_size ",
                 "(%s MB). Continuing by design because transport is one-shot zip."),
          repo, .cap_num(round(zip_bytes / mb)), .cap_num(max_download_size)
        ), call. = FALSE)
      }
      message(sprintf("Downloading %s as zip (%d file%s)...",
                      repo, length(ridx), plural(length(ridx))))
      # strip_dir = TRUE: like GitHub's zipball, GitLab's archive.zip wraps
      # every entry in a single top-level folder
      # ("namespace-project-<ref>-<sha>/..."), which must be stripped so the
      # extracted paths match the plain repo-relative paths already recorded
      # in `files` from gitlab_tree_files().
      files <- .download_zip_to_cache(files, ridx, zip_url,
                                      strip_dir = TRUE,
                                      req_func = .gitlab_config,
                                      timeout_s = zip_timeout_s,
                                      skip_on_api_limit = skip_on_api_limit)
      remaining <- setdiff(remaining, ridx[!is.na(files$file_location[ridx])])
    }

    # ── File-by-file for ResearchBox / zip fallbacks ────────────────────────────
    if (length(remaining) > 0) {
      if (is.null(pb)) {
        pb <- pb(length(remaining), "Downloading files [:bar] :current/:total")
        on.exit(pb$terminate())
      }

      # OSF osfstorage and Zenodo file URLs redirect to pre-signed cloud-storage
      # links (verified: see .download_many_parallel()'s comment) and can be
      # fetched in parallel with no proactive throttle. This is a property of
      # the FILE's own URL, not its parent repo -- an OSF node can hold
      # non-osfstorage rows (e.g. a linked Dropbox add-on) that reach a
      # completely different host and have not been checked the same way, so
      # checking repo_url alone would wrongly route those through the
      # unthrottled path too. is_osfstorage() (defined above) already makes
      # this per-file distinction for the zip gate; reuse it here.
      is_zenodo_file <- grepl("zenodo\\.org", files$file_url[remaining], ignore.case = TRUE)
      is_parallel_safe <- vapply(remaining, is_osfstorage, logical(1)) | is_zenodo_file
      remaining_parallel <- remaining[is_parallel_safe]
      remaining_seq <- remaining[!is_parallel_safe]

      if (length(remaining_parallel) > 0) {
        errs <- .download_many_parallel(
          files$file_url[remaining_parallel], files$.cache_path[remaining_parallel],
          as.numeric(files$file_size[remaining_parallel]),
          skip_on_api_limit = skip_on_api_limit)
        for (k in seq_along(remaining_parallel)) {
          i <- remaining_parallel[k]
          if (is.na(errs[k])) {
            files$file_location[i] <- files$.cache_path[i]
          } else {
            failed <- rbind(failed, data.frame(
              repo_url = files$repo_url[i], file_name = files$file_name[i],
              error = errs[k], stringsAsFactors = FALSE))
          }
          if (!is.null(pb)) pb$tick()
        }
      }

      for (i in remaining_seq) {
        err <- .download_one(files$file_url[i], files$.cache_path[i],
                             skip_on_api_limit = skip_on_api_limit)
        if (is.na(err)) {
          files$file_location[i] <- files$.cache_path[i]
        } else {
          failed <- rbind(failed, data.frame(
            repo_url = files$repo_url[i], file_name = files$file_name[i],
            error = err, stringsAsFactors = FALSE))
        }
        if (!is.null(pb)) pb$tick()
      }
    }
  }

  # Report the failures instead of swallowing them: without this, a transient
  # refusal (e.g. OSF rate-limiting a burst) looks like a complete download —
  # the progress bar reaches N/N and the files are just silently absent.
  if (nrow(failed) > 0) {
    for (repo in unique(failed$repo_url)) {
      frows <- failed[failed$repo_url == repo, ]
      message(sprintf(
        paste0("%d download%s from %s failed after retries (e.g. %s: %s). ",
               "Re-run to retry: cached files are reused, only the missing ",
               "files are fetched."),
        nrow(frows), plural(nrow(frows)), repo,
        frows$file_name[1], sub("\n.*", "", frows$error[1])))
    }
  }

  # When persistent caching is on, point the user at the cache and how to clear
  # it, once files have actually been fetched. Only meaningful for cache = TRUE
  # (cache = FALSE writes to a temp dir R discards on exit, so there is nothing
  # to clear). Shown at most once per session (this runs once per paper in a
  # corpus build, so repeating it would be noise); reset with
  # options(metacheck.repo_cache.notified = NULL) to see it again.
  if (isTRUE(cache) && length(to_get) > 0 &&
      !isTRUE(getOption("metacheck.repo_cache.notified"))) {
    message(sprintf(
      paste0("Downloaded files are cached in %s and reused on re-runs (never ",
             "cleared automatically). Free the space with repo_cache_clear()."),
      .repo_cache_dir()))
    options(metacheck.repo_cache.notified = TRUE)
  }

  files$.cache_path <- NULL
  attr(files, "gated") <- gated
  attr(files, "oversize_skipped") <- oversize_skipped
  attr(files, "failed") <- failed
  files
}
