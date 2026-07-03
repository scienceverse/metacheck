# On-disk cache for LLM responses.
#
# LLM calls are the slow, billed part of a report() run and are re-issued in
# full on every run (see R/llm.R). Because llm() forces temperature = 0, a call
# with the same (model, system_prompt, text, type, params) is deterministic, so
# its result can be stored and replayed without hitting the provider again.
#
# Each cache entry is one .rds file under a persistent rappdirs cache dir, named
# by a hash of the call inputs. An entry stores the unnested data frame that the
# per-call loop in llm() would have produced, plus the raw ellmer result (which
# carries any provider-returned reasoning/thinking content) and an optional
# thinking trace, so traces are retained wherever a provider exposes them.

#' Enable, disable, or query the LLM response cache
#'
#' When enabled (the default), [llm()] stores each structured or free-text
#' response on disk and replays it on identical later calls, so re-running a
#' report on the same paper does not re-issue (or re-bill) the LLM requests.
#' Because [llm()] runs at temperature 0 the replayed answer matches a fresh
#' call. Cached entries are keyed by model, system prompt, input text, type
#' spec, and params; changing any of these produces a fresh call.
#'
#' @param enabled if logical, sets whether the cache is used; if `NULL`
#'   (default) returns the current setting
#'
#' @returns the current setting (logical), invisibly when setting
#' @export
#'
#' @examples
#' llm_cache()          # is the cache on?
#' \dontrun{
#' llm_cache(FALSE)     # force fresh calls
#' }
llm_cache <- function(enabled = NULL) {
  if (is.null(enabled)) {
    return(isTRUE(getOption("metacheck.llm.cache", TRUE)))
  }
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    stop("Set llm_cache with TRUE or FALSE", call. = FALSE)
  }
  options(metacheck.llm.cache = enabled)
  invisible(enabled)
}

#' Clear the on-disk LLM response cache
#'
#' Deletes all cached LLM responses (see [llm_cache()]).
#'
#' @returns the number of cache entries removed, invisibly
#' @export
llm_cache_clear <- function() {
  dir <- .llm_cache_dir()
  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files)) unlink(files)
  invisible(length(files))
}

# Root directory for cached LLM responses. The METACHECK_LLM_CACHE_DIR env var
# overrides the default location (used by tests to stay out of the user cache).
.llm_cache_dir <- function() {
  dir <- Sys.getenv("METACHECK_LLM_CACHE_DIR", "")
  if (!nzchar(dir)) dir <- rappdirs::user_cache_dir("metacheck/llm", "scienceverse")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
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
  tryCatch(saveRDS(entry, .llm_cache_path(key)), error = function(e) NULL)
  invisible(entry)
}
