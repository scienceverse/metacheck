# On-disk cache for repository LISTING/metadata results (repo_check's own API
# calls: dryad_info(), zenodo_info() -- "what files does this repository
# have," not the file BYTES themselves, which download_repo_files() already
# caches separately, see repo-download.R).
#
# Unlike a file download, a listing has no natural on-disk footprint to check
# for reuse, so every repo_check() call re-queries every repository's host API
# from scratch -- even for a repository whose files are already fully
# downloaded and cached. For a host with a strict quota (Dryad: confirmed live
# 2026-09-08 against the Cooper corpus rerun, a ~100-request limit before a
# 429; the real corpus workload -- listing each repository once -- needed
# nowhere near that many requests, but RESTARTING an interrupted run re-issues
# every listing call again from scratch, and did so 11 times in one morning
# for a single 50-paper batch alone (20+ Dryad repositories x 2 requests each
# x 11 restarts = 440+ requests from re-listing, against a workload that
# genuinely needed a handful) -- this repeated re-querying, not the actual
# download volume, was what was exhausting the quota. Caching the LISTING
# result makes a restart free for anything already listed once.
#
# Off by default (a repo_check() caller that does not pass cache = TRUE is
# unaffected) -- opt in explicitly, same shape as download caching's own
# cache = TRUE/FALSE parameter.

#' Enable, disable, or query the repository-listing cache
#'
#' When enabled, [dryad_info()] and [zenodo_info()] (via `repo_check()`'s own
#' `cache` parameter) store each repository's listing result (dataset
#' metadata + file list) on disk and reuse it on a later call for the same
#' repository, instead of re-querying the host's API. This is a SEPARATE
#' cache from the downloaded-file-bytes cache (see [repo_cache_dir()]) --
#' listing a repository and downloading its files are different costs, and a
#' restart that has nothing new to download would otherwise still re-pay the
#' full listing cost every time.
#'
#' A cached listing is not refreshed automatically; a published dataset's own
#' metadata/file list rarely changes once public, but if a paper's repository
#' has genuinely been updated, clear the relevant entry with
#' [repo_info_cache_clear()].
#'
#' @param enabled if logical, sets whether the cache is used; if `NULL`
#'   (default) returns the current setting
#'
#' @returns the current setting (logical), invisibly when setting
#' @export
#'
#' @examples
#' repo_info_cache()          # is the cache on?
#' \dontrun{
#' repo_info_cache(TRUE)      # reuse listings across restarts
#' }
repo_info_cache <- function(enabled = NULL) {
  if (is.null(enabled)) {
    return(isTRUE(getOption("metacheck.repo_info.cache", FALSE)))
  }
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    stop("Set repo_info_cache with TRUE or FALSE", call. = FALSE)
  }
  options(metacheck.repo_info.cache = enabled)
  invisible(enabled)
}

#' Clear the on-disk repository-listing cache
#'
#' Deletes all cached repository listings (see [repo_info_cache()]). Safe at
#' any time -- a cleared entry is simply re-fetched from the host's API the
#' next time it is needed.
#'
#' @returns the number of cache entries removed, invisibly
#' @export
repo_info_cache_clear <- function() {
  dir <- .repo_info_cache_dir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files)) unlink(files)
  invisible(length(files))
}

# Root directory for cached repository listings. Defaults to
# ".metacheck_repo_info_cache" in the working directory (see
# .metacheck_cache_subdir); `metacheck.repo_info_cache.dir` overrides just
# this cache, `metacheck.cache.dir` relocates all caches together.
.repo_info_cache_dir <- function() {
  .metacheck_cache_subdir(".metacheck_repo_info_cache",
                          override = getOption("metacheck.repo_info_cache.dir"))
}

# Stable, filesystem-safe cache key for one host + identifier pair (a DOI, a
# record ID, ...).
.repo_info_cache_key <- function(host, id) {
  key <- paste0(host, "_", as.character(id %||% "unknown"))
  key <- gsub("[^A-Za-z0-9._-]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  if (!nzchar(key)) key <- paste0(host, "_unknown")
  key
}

# Path of the cache file for one host + identifier pair.
.repo_info_cache_path <- function(host, id) {
  file.path(.repo_info_cache_dir(), paste0(.repo_info_cache_key(host, id), ".rds"))
}

# Read a cached listing, or NULL on a miss / unreadable file.
.repo_info_cache_get <- function(host, id) {
  path <- .repo_info_cache_path(host, id)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

# Write a listing result for one host + identifier pair. Failures are
# swallowed (a caching miss is never worse than the uncached behaviour).
.repo_info_cache_put <- function(host, id, value) {
  tryCatch(saveRDS(value, .repo_info_cache_path(host, id)), error = function(e) NULL)
  invisible(value)
}

# Every *_info() worker here (.dryad_info(), .zenodo_info(), etc.) reports a
# failed lookup (host down, 404, unparseable response) via its own `error`
# column rather than raising a condition -- caching that result verbatim
# would turn a TRANSIENT failure into a PERMANENT one, since a later restart
# would keep replaying the same cached failure forever instead of trying
# again. Only a result with no (or all-NA) `error` column is worth caching.
.repo_info_ok <- function(value) {
  if (is.null(value) || !is.data.frame(value)) return(FALSE)
  if (!"error" %in% names(value)) return(TRUE)
  all(is.na(value$error))
}

# github_tree_files() and gitlab_tree_files() report a failed listing
# (invalid/inaccessible repo, or a host error) as a list with gated = TRUE,
# rather than a data.frame `error` column -- same transient-failure concern
# as .repo_info_ok() above, just a different result shape. Only a
# successfully-listed repository (gated = FALSE) is worth caching.
.repo_info_list_ok <- function(value) {
  is.list(value) && !isTRUE(value$gated)
}
