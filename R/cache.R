# Shared cache-location logic for metacheck's two on-disk caches:
#   * the downloaded-repository-file cache (see repo-download.R)
#   * the LLM-response cache (see llm-cache.R)
#
# Both default to a folder in the CURRENT WORKING DIRECTORY, so a project's
# cache lives with the project — easy to find, easy to inspect, easy to delete,
# and gone when the project folder is. This is a deliberate choice over a hidden
# per-user OS cache: metacheck caches are per-analysis, not shared across
# projects, and a visible in-project folder is far more transparent (a hidden
# cache is exactly what can be silently lost).
#
# The location is overridable. `options(metacheck.cache.dir = "/some/path")`
# relocates BOTH caches under that root; each cache also keeps its own override
# (metacheck.repo_cache.dir option / METACHECK_LLM_CACHE_DIR env var) for finer
# control and backward compatibility.

# Default cache root: the working directory. One shared parent so both caches
# sit together and a single override moves them as a unit.
.metacheck_cache_root <- function() {
  getOption("metacheck.cache.dir", getwd())
}

# Resolve one named cache directory (creating it if needed). `subdir` is the
# folder name under the root (e.g. ".metacheck_repo_cache"). `override` is an
# already-resolved path from a cache-specific option/env var, or NULL/"" to use
# the shared root. The cache folders are named ".metacheck_*"; a user who wants
# them out of git can add ".metacheck_*" to their own .gitignore (metacheck does
# not write into the project itself — see metacheck_cache_info()).
.metacheck_cache_subdir <- function(subdir, override = NULL) {
  dir <- if (!is.null(override) && nzchar(override)) override
         else file.path(.metacheck_cache_root(), subdir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

# Total size (bytes) of a cache dir's contents.
.metacheck_dir_size <- function(dir) {
  if (!dir.exists(dir)) return(0)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
  if (length(files) == 0) return(0)
  sum(file.size(files), na.rm = TRUE)
}

#' Show where metacheck's caches live and how big they are
#'
#' Prints the location and size of both on-disk caches — the downloaded
#' repository-file cache and the LLM-response cache — so they are never hidden.
#' By default each sits in a folder in the current working directory
#' (`.metacheck_repo_cache` and `.metacheck_llm_cache`); relocate both with
#' `options(metacheck.cache.dir = "/some/path")`.
#'
#' @returns a data.frame (`cache`, `path`, `size_mb`), invisibly.
#' @seealso [repo_cache_dir()], [repo_cache_clear()], [llm_cache_clear()]
#' @export
#' @examples
#' \dontrun{
#' metacheck_cache_info()
#' }
metacheck_cache_info <- function() {
  repo_dir <- .repo_cache_dir()
  llm_dir  <- .llm_cache_dir()
  info <- data.frame(
    cache   = c("repo_files", "llm"),
    path    = c(repo_dir, llm_dir),
    size_mb = round(c(.metacheck_dir_size(repo_dir),
                      .metacheck_dir_size(llm_dir)) / 1024^2, 1),
    stringsAsFactors = FALSE)
  message("metacheck caches:")
  for (i in seq_len(nrow(info)))
    message(sprintf("  %-11s %s  (%s MB)", info$cache[i], info$path[i],
                    format(info$size_mb[i], big.mark = ",")))
  # If the caches sit inside a git repo, remind the user to ignore them — a
  # repository-file cache can reach many GB and should not be committed.
  in_wd <- is.null(getOption("metacheck.cache.dir")) ||
    identical(normalizePath(getOption("metacheck.cache.dir"), mustWork = FALSE),
              normalizePath(getwd(), mustWork = FALSE))
  if (in_wd && dir.exists(".git"))
    message("  tip: add '.metacheck_*' to your .gitignore so these are not committed.")
  invisible(info)
}
