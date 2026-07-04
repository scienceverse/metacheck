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

# Download one file to `dest`. Returns TRUE on success. Files are size-gated
# upfront by the caller, so this just fetches.
.download_one <- function(url, dest) {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  tryCatch({
    req <- httr2::request(url) |> httr2::req_progress()
    httr2::req_perform(req, path = dest)
    file.exists(dest) && file.size(dest) > 0
  }, error = function(e) {
    if (file.exists(dest)) unlink(dest)
    FALSE
  })
}

#' Download selected repository files into the shared cache
#'
#' Given file rows from `repo_check` (with `repo_url`, `file_url`, `file_path`,
#' `file_size`), download the files of a repository into a persistent cache keyed
#' by repo URL. Files already present in the cache are reused (never
#' re-downloaded).
#'
#' Size caps are an **upfront, all-or-nothing gate per repository**: before
#' downloading, the full set of not-yet-cached files for a repository is measured
#' (using the manifest `file_size`, or a `HEAD` `Content-Length` probe when that
#' is missing) and checked against the caps. If any file exceeds `max_file_size`,
#' or the repository's total exceeds `max_download_size`, **none** of that
#' repository's files are downloaded and a message explaining how to raise the
#' caps is attached (rather than silently downloading a partial subset). A file
#' whose size cannot be determined at all also gates its repository.
#'
#' @param files a data.frame of file rows to download
#' @param max_file_size largest single file to download, in MB (`Inf` = no cap)
#' @param max_download_size largest total download per repository, in MB
#'   (`Inf` = no cap); counts only files not already cached
#' @param pb an optional progress bar
#'
#' @returns `files` with `file_location` set to the cache path for each
#'   downloaded (or already-cached) file, and `NA` for files in gated
#'   repositories or without a URL. An attribute `"gated"` holds a data.frame
#'   (`repo_url`, `message`) of repositories refused by the caps.
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
  to_get <- integer(0)

  # ── Per-repository upfront gate ─────────────────────────────────────────────
  for (repo in unique(files$repo_url[has_url & !already])) {
    idx <- which(files$repo_url == repo & has_url & !already)
    if (length(idx) == 0) next

    # Resolve sizes (bytes): manifest size, else a HEAD Content-Length probe.
    sizes <- as.numeric(files$file_size[idx])
    need_probe <- which(is.na(sizes))
    for (k in need_probe) sizes[k] <- .remote_size(files$file_url[idx[k]])

    # A file whose size we still cannot determine gates the whole repo.
    unknown <- which(is.na(sizes))
    if (length(unknown) > 0) {
      gated <- rbind(gated, data.frame(
        repo_url = repo,
        message  = cap_gate_unknown(repo, files$file_name[idx[unknown[1]]]),
        stringsAsFactors = FALSE))
      next
    }

    # Effective caps for THIS repo (a prompt may raise them for this repo only).
    repo_file_cap  <- max_file_size
    repo_total_cap <- max_download_size

    total_mb <- sum(sizes) / mb
    over_idx <- which(is.finite(repo_file_cap) & sizes > repo_file_cap * mb)
    oversized <- data.frame(
      name    = files$file_name[idx[over_idx]],
      size_mb = sizes[over_idx] / mb,
      stringsAsFactors = FALSE)

    msg <- cap_gate_size(repo, length(idx), total_mb, oversized,
                         repo_file_cap, repo_total_cap)
    if (!is.null(msg)) {
      # Ask the user (interactive, not auto) whether to skip or raise the caps
      # for this repo, showing the offending files + sizes. Under auto() or a
      # non-interactive session this reports inline and skips without blocking.
      items <- data.frame(
        name = files$file_name[idx],
        size = sizes,                      # bytes
        stringsAsFactors = FALSE)
      # Raise both caps to what this repo needs, so a single "raise" proceeds.
      need_file  <- if (length(over_idx)) ceiling(max(sizes[over_idx]) / mb) else repo_file_cap
      need_total <- ceiling(total_mb)
      ans <- cap_prompt(msg, param = "max_file_size / max_download_size",
                        needed = paste0(need_file, " / ", need_total),
                        current = paste0(.cap_num(repo_file_cap), " / ",
                                         .cap_num(repo_total_cap)),
                        items = items, custom = FALSE)
      if (identical(ans$action, "raise")) {
        to_get <- c(to_get, idx)           # user opted in: download the repo
        next
      }
      gated <- rbind(gated, data.frame(repo_url = repo, message = msg,
                                       stringsAsFactors = FALSE))
      next
    }

    to_get <- c(to_get, idx)   # repo fits: download all its files
  }

  # ── Download the files of repositories that passed the gate ─────────────────
  if (length(to_get) > 0) {
    if (is.null(pb)) {
      pb <- pb(length(to_get), "Downloading files [:bar] :current/:total")
      on.exit(pb$terminate())
    }
    for (i in to_get) {
      if (.download_one(files$file_url[i], files$.cache_path[i]))
        files$file_location[i] <- files$.cache_path[i]
      if (!is.null(pb)) pb$tick()
    }
  }

  files$.cache_path <- NULL
  attr(files, "gated") <- gated
  files
}
