# Selective, cached download of repository files for the modules that read file
# contents (data_check, code_check). repo_check lists files without fetching
# them (file_location = NA for OSF/GitHub/Zenodo); these helpers fetch only the
# files a module needs, into a shared persistent cache, so downloads are reused
# across modules and R sessions.

# Root cache directory for downloaded repository files.
.repo_cache_dir <- function() {
  dir <- rappdirs::user_cache_dir("metacheck/repo_files", "scienceverse")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
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

# Local cache path for one file, preserving its repo-relative path so files with
# the same basename in different folders don't collide.
.repo_cache_path <- function(repo_url, file_path) {
  rel <- gsub("\\\\", "/", file_path)
  rel <- gsub("^/+", "", rel)
  file.path(.repo_cache_subdir(repo_url), rel)
}

# Resolve a remote file's size (bytes) from its Content-Length header via a
# lightweight HEAD request. Used to turn a missing manifest size into a real
# size *before* downloading, so an unsized 5 GB file is caught by the gate. NA
# on any error or when the header is absent (chunked/dynamic responses).
.remote_size <- function(url) {
  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = function(r) FALSE)
    resp <- httr2::req_perform(req)
    cl <- httr2::resp_header(resp, "content-length")
    if (is.null(cl) || is.na(cl) || !nzchar(cl)) return(NA_real_)
    as.numeric(cl)
  }, error = function(e) NA_real_)
}

# Download one file to `dest`. Returns NA_character_ on success, or a short
# error description on failure (for the caller's failure report). Files are
# size-gated upfront by the caller, so this just fetches. Transient server
# refusals — OSF rate-limits bursts with 429, and 503s happen — are retried
# with backoff (Retry-After is honoured), because a batch of many small
# requests otherwise loses files at random.
.download_one <- function(url, dest) {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  tryCatch({
    # Throttle realm = the host (explicit: the default derivation errors on
    # host-less URLs such as file://, which the tests use).
    host <- tryCatch(httr2::url_parse(url)$hostname, error = function(e) NULL)
    if (is.null(host) || !length(host) || is.na(host) || !nzchar(host))
      host <- "local"
    req <- httr2::request(url) |>
      # Pace the requests instead of firing the whole batch back-to-back: a
      # leaky bucket per host that allows a short burst of 10, then sustains
      # ~1 request/second. Downloads are sequential either way; this only
      # inserts waits when the burst budget is spent, which is what keeps OSF
      # from answering 429.
      httr2::req_throttle(capacity = 10, fill_time_s = 10, realm = host) |>
      httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
      httr2::req_progress()
    httr2::req_perform(req, path = dest)
    if (file.exists(dest) && file.size(dest) > 0) NA_character_
    else "empty response"
  }, error = function(e) {
    if (file.exists(dest)) unlink(dest)
    conditionMessage(e)
  })
}

#' Download selected repository files into the shared cache
#'
#' Given file rows from `repo_check` (with `repo_url`, `file_url`, `file_path`,
#' `file_size`), download the files of a repository into a persistent cache keyed
#' by repo URL. Files already present in the cache are reused (never
#' re-downloaded).
#'
#' The two size caps work differently. `max_file_size` is a **per-file filter**:
#' any single file larger than it is skipped individually, while the rest of the
#' repository still downloads. `max_download_size` is a **per-repository gate**:
#' if the total of the files that would download (i.e. after the per-file filter)
#' exceeds it, the **whole** repository is skipped. Sizes come from the manifest
#' `file_size`, or a `HEAD` `Content-Length` probe when that is missing; a file
#' whose size cannot be determined at all gates its repository (we refuse to
#' stream a file of unknown size).
#'
#' @param files a data.frame of file rows to download
#' @param max_file_size largest single file to download, in MB (`Inf` = no cap);
#'   larger files are skipped individually
#' @param max_download_size largest total download per repository, in MB
#'   (`Inf` = no cap); measured after the per-file filter, and if exceeded the
#'   whole repository is skipped
#' @param pb an optional progress bar
#'
#' Downloads are paced (a short burst, then ~1 request/second per host) and
#' transient refusals (HTTP 429/503, dropped connections) are retried with
#' backoff, honouring `Retry-After`. A file that still fails after the retries
#' is reported with a message (one per repository) and recorded in the
#' `"failed"` attribute; because the cache is reused, re-running fetches only
#' the files that are still missing.
#'
#' @returns `files` with `file_location` set to the cache path for each
#'   downloaded (or already-cached) file, and `NA` otherwise. Attribute
#'   `"gated"` is a data.frame (`repo_url`, `message`) of repositories skipped by
#'   the total cap; attribute `"oversize_skipped"` is a data.frame (`repo_url`,
#'   `file_name`, `file_size`) of individual files skipped by the per-file cap;
#'   attribute `"failed"` is a data.frame (`repo_url`, `file_name`, `error`) of
#'   files whose download failed after retries.
#' @export
#' @keywords internal
download_repo_files <- function(files,
                                max_file_size = 100,
                                max_download_size = 500,
                                pb = NULL) {
  if (is.null(files) || nrow(files) == 0) return(files)
  if (!"file_location" %in% names(files))
    files$file_location <- NA_character_

  # Cache path per file; reuse anything already cached.
  rel_path <- files$file_path %||% files$file_name
  rel_path <- ifelse(is.na(rel_path), files$file_name, rel_path)
  files$.cache_path <- vapply(seq_len(nrow(files)), function(i) {
    .repo_cache_path(files$repo_url[i], rel_path[i])
  }, character(1))

  already <- file.exists(files$.cache_path)
  files$file_location[already] <- files$.cache_path[already]

  has_url <- !is.na(files$file_url) & nzchar(files$file_url)
  mb <- 1024 * 1024

  gated <- data.frame(repo_url = character(0), message = character(0),
                      stringsAsFactors = FALSE)
  # Individual files skipped because they exceed max_file_size (the repo is still
  # downloaded — only these files are left out). Recorded for reporting.
  oversize_skipped <- data.frame(repo_url = character(0), file_name = character(0),
                                 file_size = numeric(0), stringsAsFactors = FALSE)
  to_get <- integer(0)

  # ── Per-repository gate + per-file size filter ──────────────────────────────
  # max_file_size is a PER-FILE filter: files over it are skipped individually,
  # the rest of the repo still downloads. max_download_size is a REPO gate: if
  # the total of the files that WOULD download (after removing oversized ones)
  # exceeds it, the whole repository is skipped.
  for (repo in unique(files$repo_url[has_url & !already])) {
    idx <- which(files$repo_url == repo & has_url & !already)
    if (length(idx) == 0) next

    # Resolve sizes (bytes): manifest size, else a HEAD Content-Length probe.
    sizes <- as.numeric(files$file_size[idx])
    need_probe <- which(is.na(sizes))
    for (k in need_probe) sizes[k] <- .remote_size(files$file_url[idx[k]])

    # A file whose size we still cannot determine gates the whole repo (we can't
    # know whether it fits, and refuse to stream blindly).
    unknown <- which(is.na(sizes))
    if (length(unknown) > 0) {
      msg <- cap_gate_unknown(repo, files$file_name[idx[unknown[1]]])
      # Report it like the total-size gate does — this refusal used to be
      # recorded only in the manifest, so a repo could silently come up empty.
      cap_report(msg)
      gated <- rbind(gated, data.frame(
        repo_url = repo, message = msg, stringsAsFactors = FALSE))
      next
    }

    repo_file_cap  <- max_file_size
    repo_total_cap <- max_download_size

    # Per-file filter: drop files over the per-file cap (keep the rest).
    over <- if (is.finite(repo_file_cap)) sizes > repo_file_cap * mb
            else rep(FALSE, length(idx))
    if (any(over)) {
      oversize_skipped <- rbind(oversize_skipped, data.frame(
        repo_url  = repo,
        file_name = files$file_name[idx[over]],
        file_size = sizes[over],
        stringsAsFactors = FALSE))
    }
    keep_idx  <- idx[!over]
    keep_size <- sizes[!over]
    if (length(keep_idx) == 0) next   # nothing left after the filter

    # Repo gate on the SURVIVORS' total.
    total_mb <- sum(keep_size) / mb
    if (is.finite(repo_total_cap) && total_mb > repo_total_cap) {
      need_total <- ceiling(total_mb)
      msg <- sprintf(
        paste0("Repository %s was not downloaded: its %d file%s total %s MB, ",
               "over the %s MB per-repository limit. ",
               "Set `max_download_size >= %s` to download it."),
        repo, length(keep_idx), plural(length(keep_idx)),
        .cap_num(need_total), .cap_num(repo_total_cap), .cap_num(need_total))
      cap_report(msg)
      gated <- rbind(gated, data.frame(repo_url = repo, message = msg,
                                       stringsAsFactors = FALSE))
      next
    }

    to_get <- c(to_get, keep_idx)   # repo fits: download the surviving files
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
  failed <- data.frame(repo_url = character(0), file_name = character(0),
                       error = character(0), stringsAsFactors = FALSE)
  if (length(to_get) > 0) {
    if (is.null(pb)) {
      pb <- pb(length(to_get), "Downloading files [:bar] :current/:total")
      on.exit(pb$terminate())
    }
    for (i in to_get) {
      err <- .download_one(files$file_url[i], files$.cache_path[i])
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

  files$.cache_path <- NULL
  attr(files, "gated") <- gated
  attr(files, "oversize_skipped") <- oversize_skipped
  attr(files, "failed") <- failed
  files
}
