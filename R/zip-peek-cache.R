# On-disk cache for zip_peek()'s result (a zip's central-directory listing,
# read via one or two HTTP range requests -- see R/zip-peek.R). This is the
# same fix, one level further in, that repo_info_cache() (R/repo-info-cache.R)
# already applies to a repository's file LISTING: zip_peek()'s own cache
# (.zip_peek_cache, R/zip-peek.R) is in-memory only, so it is reset every time
# the R process restarts, and a restarted pipeline run re-peeks every zip it
# already peeked in an earlier run of the SAME session, even though a zip's
# contents cannot have changed (scienceverse/metacheck#427).
#
# Off by default (a zip_peek() caller that does not pass cache = TRUE is
# unaffected), same shape as repo_info_cache()'s own cache = TRUE/FALSE
# parameter, gated on the same `cache` argument repo_check()/data_check()
# already take.

#' Clear the on-disk zip-peek cache
#'
#' Deletes all cached zip listings (see [zip_peek()]'s `cache` argument).
#' Safe at any time -- a cleared entry is simply re-peeked over HTTP the next
#' time it is needed.
#'
#' @returns the number of cache entries removed, invisibly
#' @export
zip_peek_cache_clear <- function() {
  dir <- .zip_peek_cache_dir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files)) unlink(files)
  invisible(length(files))
}

# Root directory for cached zip listings. Defaults to
# ".metacheck_zip_peek_cache" in the working directory (see
# .metacheck_cache_subdir); `metacheck.zip_peek_cache.dir` overrides just this
# cache, `metacheck.cache.dir` relocates all caches together.
.zip_peek_cache_dir <- function() {
  .metacheck_cache_subdir(".metacheck_zip_peek_cache",
                          override = getOption("metacheck.zip_peek_cache.dir"))
}

# Stable, filesystem-safe cache key for one archive URL. Unlike
# .repo_info_cache_key() (host + short id), a zip's download URL can itself be
# long and carry query strings/tokens (e.g. an OSF or Dryad pre-signed S3
# URL), so the URL is hashed rather than sanitised into a filename verbatim.
.zip_peek_cache_key <- function(url) {
  digest_ok <- requireNamespace("digest", quietly = TRUE)
  if (digest_ok) return(digest::digest(url, algo = "sha1"))
  # No `digest` dependency required elsewhere in this file's caller
  # (zip_peek() itself has no such dependency either) -- fall back to a
  # deterministic, collision-resistant-enough key built from base R alone: the
  # URL's own length plus a checksum of its bytes.
  bytes <- utils::URLencode(url, reserved = TRUE)
  paste0(nchar(url), "_", sum(utf8ToInt(bytes)) %% 1e8)
}

# Path of the cache file for one archive URL.
.zip_peek_cache_path <- function(url) {
  file.path(.zip_peek_cache_dir(), paste0(.zip_peek_cache_key(url), ".rds"))
}

# Read a cached zip listing, or NULL on a miss / unreadable file. Distinct
# from "cached as a known failure" (a data.frame vs. NULL result, both of
# which are legitimate zip_peek() return values) -- see .zip_peek_cache_put()
# for why a failure is cached too.
.zip_peek_cache_get <- function(url) {
  path <- .zip_peek_cache_path(url)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

# Write a zip_peek() result (a data.frame listing, or NULL) for one archive
# URL. A NULL result (host does not support ranges, or the central directory
# could not be read) is deliberately cached too, matching the in-memory cache
# in R/zip-peek.R -- see that file's own comment: a zip's support for range
# requests, like its contents, cannot change, so a cached refusal is not a
# transient failure worth re-trying on the next restart. Write failures are
# swallowed (a caching miss is never worse than the uncached behaviour).
.zip_peek_cache_put <- function(url, value) {
  tryCatch(saveRDS(value, .zip_peek_cache_path(url)), error = function(e) NULL)
  invisible(value)
}

# Whether a cache entry exists on disk for this URL -- distinguishes "cached
# NULL" (a real result: the peek failed) from "never attempted", the same
# distinction zip_peek()'s in-memory check makes with exists()/get() rather
# than a NULL-returning lookup.
.zip_peek_cache_has <- function(url) file.exists(.zip_peek_cache_path(url))
