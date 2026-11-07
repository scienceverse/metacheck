# On-disk cache for LLM responses.
#
# LLM calls are the slow, billed part of a report() run and are re-issued in
# full on every run (see R/llm.R). Because llm() forces temperature = 0, a call
# with the same (model, system_prompt, text, type, params) is deterministic, so
# its result can be stored and replayed without hitting the provider again.
#
# Each cache entry is one .rds file in the cache folder (see .llm_cache_dir()),
# named by a hash of the call inputs. An entry stores the unnested data frame that the
# per-call loop in llm() would have produced, plus the raw ellmer result (which
# carries any provider-returned reasoning/thinking content) and an optional
# thinking trace, so traces are retained wherever a provider exposes them.

#' Turn the lasting LLM response cache on or off, or query it
#'
#' [llm()] stores each structured or free-text response and replays it on
#' identical later calls, so asking the same question again does not re-issue
#' (or re-bill) the LLM request. Because [llm()] runs at temperature 0 the
#' replayed answer matches a fresh call. Cached entries are keyed by model,
#' system prompt, input text, type spec, and params; changing any of these
#' produces a fresh call.
#'
#' By default the responses are kept only in the R session's temporary folder,
#' which R removes when the session ends. With `llm_cache(TRUE)` they are kept
#' between sessions, in the folder `.metacheck_llm_cache` in the working
#' directory, or under the folder set with `options(metacheck.cache.dir = )` or
#' the `METACHECK_LLM_CACHE_DIR` environment variable. With `llm_cache(FALSE)`
#' no response is stored or replayed, so every call asks the model again;
#' `options(metacheck.llm.cache = NULL)` returns to the default. Use
#' [llm_cache_clear()] to remove the stored responses.
#'
#' @param enabled `TRUE` to keep responses between sessions, `FALSE` to store
#'   none; if `NULL` (default) returns whether responses are kept between
#'   sessions
#'
#' @returns the current setting (logical), invisibly when setting
#' @export
#'
#' @examples
#' llm_cache()          # are responses kept between sessions? (FALSE by default)
#' llm_cache(TRUE)      # keep responses between sessions
#' llm_cache(FALSE)     # no cache: every call asks the model again
#' options(metacheck.llm.cache = NULL)  # back to the default: this session only
llm_cache <- function(enabled = NULL) {
  if (is.null(enabled)) {
    return(isTRUE(getOption("metacheck.llm.cache", FALSE)))
  }
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    stop("Set llm_cache with TRUE or FALSE", call. = FALSE)
  }
  options(metacheck.llm.cache = enabled)
  invisible(enabled)
}

#' Clear the on-disk LLM response cache
#'
#' Deletes all cached LLM responses in the cache that is in use: the lasting
#' cache when [llm_cache()] is `TRUE`, otherwise this session's temporary one.
#'
#' @returns the number of cache entries removed, invisibly
#' @export
#' @examples
#' llm_cache_clear()
llm_cache_clear <- function() {
  dir <- .llm_cache_dir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files)) unlink(files)
  invisible(length(files))
}

# Root directory for cached LLM responses. By default (llm_cache() FALSE) a
# folder in the session's temporary directory, which R removes when the
# session ends: CRAN does not allow a package to keep files elsewhere unless
# the user asks for it. With llm_cache(TRUE) the lasting cache: the folder
# ".metacheck_llm_cache" in the working directory (see .metacheck_cache_subdir),
# or the METACHECK_LLM_CACHE_DIR env var (just this cache) or
# `metacheck.cache.dir` (all caches) when set.
.llm_cache_dir <- function() {
  if (!llm_cache()) {
    return(normalizePath(file.path(tempdir(), "metacheck-llm-cache"),
                         winslash = "/", mustWork = FALSE))
  }
  override <- Sys.getenv("METACHECK_LLM_CACHE_DIR", "")
  .metacheck_cache_subdir(".metacheck_llm_cache",
                          override = if (nzchar(override)) override else NULL)
}

# Hash the inputs that fully determine an LLM response into a stable key.
# Params are sorted by name so list order does not matter; the ellmer type
# object is reduced to its printed form so a schema change misses the cache.
# The payload is serialised to a canonical byte stream (xdr = version-stable,
# ascii = no embedded object addresses) and md5-summed via a temp file, so no
# external hashing dependency is needed.
.llm_cache_key <- function(text, system_prompt, type, model, params) {
  p <- if (length(params)) params[order(names(params))] else params
  payload <- list(
    text = text, system_prompt = system_prompt, model = model,
    type = if (is.null(type)) NULL else utils::capture.output(print(type)),
    params = p
  )
  raw <- serialize(payload, connection = NULL, ascii = TRUE, xdr = TRUE)
  # without the header fields that record the R version that wrote it
  # (lines 3 and 4 of the ascii form): with them, an R upgrade changed every
  # key and emptied the cache (U20)
  lines <- strsplit(rawToChar(raw), "\n", fixed = TRUE)[[1]]
  raw <- charToRaw(paste(lines[-(3:4)], collapse = "\n"))
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(raw, tmp)
  unname(tools::md5sum(tmp))
}

# Path of the cache file for a given key.
.llm_cache_path <- function(key) {
  file.path(.llm_cache_dir(), paste0(key, ".rds"))
}

# Read a cached entry, or NULL on a miss / unreadable file.
.llm_cache_get <- function(key) {
  path <- .llm_cache_path(key)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

# Write an entry for a key. `df` is the unnested per-call data frame; `raw` is
# the untouched ellmer result; `thinking` is an optional reasoning trace.
.llm_cache_put <- function(key, df, raw = NULL, thinking = NULL) {
  entry <- list(
    df = df, raw = raw, thinking = thinking,
    created = Sys.time(), version = 1L
  )
  path <- .llm_cache_path(key)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(entry, path), error = function(e) NULL)
  invisible(entry)
}
