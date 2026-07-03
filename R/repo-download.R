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

#' Download selected repository files into the shared cache
#'
#' Given file rows from `repo_check` (with `repo_url`, `file_url`, `file_path`,
#' `file_size`), download only those files, honouring per-file and total-size
#' caps, into a persistent cache keyed by repo URL. Files already present in the
#' cache are reused (never re-downloaded); files omitted by the caps or missing
#' a download URL are left for a later run to retry.
#'
#' @param files a data.frame of file rows to download
#' @param max_file_size largest single file to download, in MB (`NULL` = no cap)
#' @param max_download_size largest total download per repo, in MB (`NULL` = no
#'   cap); the cap counts only files not already cached
#' @param pb an optional progress bar
#'
#' @returns `files` with `file_location` set to the cache path for each
#'   downloaded (or already-cached) file, and `NA` for omitted/failed files. An
#'   attribute `"omitted"` holds a data.frame of files skipped by the caps.
#' @export
#' @keywords internal
download_repo_files <- function(files,
                                max_file_size = 10,
                                max_download_size = 100,
                                pb = NULL) {
  if (is.null(files) || nrow(files) == 0) return(files)
  if (!"file_location" %in% names(files))
    files$file_location <- NA_character_

  # Which rows have a usable download URL and are not already cached?
  rel_path <- files$file_path %||% files$file_name
  rel_path <- ifelse(is.na(rel_path), files$file_name, rel_path)
  files$.cache_path <- vapply(seq_len(nrow(files)), function(i) {
    .repo_cache_path(files$repo_url[i], rel_path[i])
  }, character(1))

  already <- file.exists(files$.cache_path)
  files$file_location[already] <- files$.cache_path[already]

  has_url <- !is.na(files$file_url) & nzchar(files$file_url)
  to_get  <- which(has_url & !already)

  omitted <- files[0, , drop = FALSE]

  if (length(to_get) > 0) {
    sizes <- files$file_size[to_get]
    sizes[is.na(sizes)] <- 0

    # Per-file cap.
    if (!is.null(max_file_size) && max_file_size > 0) {
      too_big <- sizes > max_file_size * 1024 * 1024
      if (any(too_big)) {
        omitted <- rbind(omitted, files[to_get[too_big], , drop = FALSE])
        to_get  <- to_get[!too_big]
        sizes   <- sizes[!too_big]
      }
    }

    # Total-size cap: drop the largest remaining files until under budget.
    if (!is.null(max_download_size) && max_download_size > 0) {
      budget <- max_download_size * 1024 * 1024
      while (length(to_get) > 0 && sum(sizes) > budget) {
        biggest <- which.max(sizes)
        omitted <- rbind(omitted, files[to_get[biggest], , drop = FALSE])
        to_get  <- to_get[-biggest]
        sizes   <- sizes[-biggest]
      }
    }
  }

  # Download the survivors.
  if (length(to_get) > 0) {
    if (is.null(pb)) {
      pb <- pb(length(to_get), "Downloading files [:bar] :current/:total")
      on.exit(pb$terminate())
    }
    for (i in to_get) {
      dest <- files$.cache_path[i]
      dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
      ok <- tryCatch({
        req <- httr2::request(files$file_url[i]) |>
          httr2::req_progress()
        httr2::req_perform(req, path = dest)
        file.exists(dest) && file.size(dest) > 0
      }, error = function(e) {
        if (file.exists(dest)) unlink(dest)
        FALSE
      })
      if (ok) files$file_location[i] <- dest
      if (!is.null(pb)) pb$tick()
    }
  }

  files$.cache_path <- NULL
  attr(files, "omitted") <- omitted
  files
}
