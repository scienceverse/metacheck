#' List available paper corpora
#'
#' Queries the GitHub Releases API for the scienceverse/papers repository and
#' returns a table of available paper corpora that can be loaded with
#' [papers_load()].
#'
#' @param repo GitHub repository in "owner/repo" format; a private
#'   repository needs a GitHub token with access to it (see [github_pat()])
#'
#' @returns a data frame with columns `name`, `tag`, `size_mb`, and `cached`
#' @export
#'
#' @examples
#' \donttest{
#' if (online("api.github.com")) {
#'   # GitHub can refuse when its request limit is reached; then this says why
#'   tryCatch(papers_available(), error = function(e) message(conditionMessage(e)))
#' }
#' }
papers_available <- function(repo = "scienceverse/papers") {
  assets <- .papers_release_assets(repo)
  cache_dir <- .papers_cache_dir()

  corpus_name <- tools::file_path_sans_ext(assets$name)
  data.frame(
    name    = corpus_name,
    tag     = assets$tag,
    size_mb = round(assets$size / 1e6, 1),
    # paste0(character(0), ".rds") is ".rds": one value for no corpora (U18)
    cached  = if (length(corpus_name)) file.exists(file.path(cache_dir, paste0(corpus_name, ".rds"))) else logical(0)
  )
}


#' Load a paper corpus
#'
#' Downloads a paper corpus RDS file from the scienceverse/papers GitHub
#' repository and loads it into R as a paperlist object. By default the file
#' is downloaded to a temporary location and discarded after loading, which
#' is the right choice for one-off use. Set `cache = TRUE` to save it
#' permanently in the user data directory instead, so subsequent calls reuse
#' the cached copy rather than re-downloading. Use [papers_remove()] to
#' delete a cached corpus.
#'
#' A corpus in a private repository (such as `scienceverse/papers_private`)
#' is read with the GitHub token [github_pat()] returns, which needs access
#' to that repository.
#'
#' @param name name of the corpus, without the `.rds` extension
#'   (see [papers_available()])
#' @param repo GitHub repository in "owner/repo" format; a private
#'   repository needs a GitHub token with access to it (see [github_pat()])
#' @param cache whether to save the corpus to the user data directory for
#'   reuse across sessions, instead of downloading it fresh each time
#' @param overwrite whether to re-download if already cached (only relevant
#'   when `cache = TRUE`)
#'
#' @returns a paperlist object
#' @export
#'
#' @examples
#' \donttest{
#' if (online("api.github.com")) {
#'   # GitHub can refuse when its request limit is reached; then this says why
#'   collabra <- tryCatch(papers_load("collabra"),
#'                        error = function(e) message(conditionMessage(e)))
#' }
#' }
papers_load <- function(name,
                         repo = "scienceverse/papers",
                         cache = FALSE,
                         overwrite = FALSE) {
  cache_dir <- .papers_cache_dir()
  cache_path <- file.path(cache_dir, paste0(name, ".rds"))

  if (cache && file.exists(cache_path) && !overwrite) {
    message("Loading cached ", name, " ...")
    # a corpus saved before the repair of split links (see .repair_links())
    return(.paper_repair_links(readRDS(cache_path)))
  }

  assets <- .papers_release_assets(repo)
  corpus_names <- tools::file_path_sans_ext(assets$name)
  entry  <- assets[corpus_names == name, ]

  if (nrow(entry) == 0) {
    available <- paste(corpus_names, collapse = ", ")
    stop("'", name, "' not found in releases of ", repo, ". ",
         "Available: ", available, call. = FALSE)
  }

  size_mb <- round(entry$size[[1]] / 1e6, 1)
  message("Downloading ", name, " (", size_mb, " MB) ...")

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  # a private repository's file can only be downloaded through the API, with
  # the token (#369); GitHub then sends the request on to where the file is
  # stored, and the token is not sent there
  token <- .key_use("github", check = .github_key_check)
  if (nzchar(token) && !is.na(entry$api_url[[1]])) {
    req <- .mc_request(entry$api_url[[1]]) |>
      .github_config() |>
      httr2::req_headers(Accept = "application/octet-stream")
  } else {
    req <- .mc_request(entry$download_url[[1]])
  }
  req <- httr2::req_error(req, is_error = \(r) FALSE)
  if (verbose()) req <- httr2::req_progress(req, type = "down")

  resp <- tryCatch(
    httr2::req_perform(req, path = tmp),
    error = \(e) stop("Download failed: ", e$message, call. = FALSE)
  )

  if (httr2::resp_status(resp) != 200) {
    stop("Download failed (status ", httr2::resp_status(resp), "): ",
         entry$download_url[[1]], call. = FALSE)
  }

  if (cache) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(tmp, cache_path, overwrite = TRUE)
    message("Cached to ", cache_path)
  }

  message("Loading ", name, " ...")
  .paper_repair_links(readRDS(tmp))
}


#' Remove a cached paper corpus
#'
#' Deletes the locally cached RDS file for a corpus to free disk space.
#'
#' @param name name of the corpus (see [papers_available()])
#'
#' @returns `TRUE` invisibly if deleted, `FALSE` if not cached
#' @export
#'
#' @examples
#' \dontrun{
#' papers_remove("collabra")
#' }
papers_remove <- function(name) {
  cache_dir <- .papers_cache_dir()
  path <- file.path(cache_dir, paste0(name, ".rds"))

  if (!file.exists(path)) {
    message("'", name, "' is not cached; nothing to remove.")
    return(invisible(FALSE))
  }

  unlink(path)
  message("Removed cached corpus: ", name)
  invisible(TRUE)
}


#' Get a paper corpus's Dublin Core metadata
#'
#' Fetches and parses `metadata.json` for a corpus from the scienceverse/papers
#' GitHub repository. This file documents provenance that is not stored in the
#' corpus `.rds` itself: how the corpus was built, what years/license it
#' covers, and so on. See each corpus's `README.md` on the
#' [papers repository](https://github.com/scienceverse/papers) for a fuller
#' narrative description, including known gaps and data-quality caveats.
#'
#' @param name name of the corpus (see [papers_available()])
#' @param repo GitHub repository in "owner/repo" format; a private
#'   repository needs a GitHub token with access to it (see [github_pat()])
#'
#' @returns a named list of the corpus's Dublin Core metadata fields (colons
#'   in the original `dc:field` names are replaced with underscores, e.g.
#'   `dc:title` becomes `dc_title`)
#' @export
#'
#' @examples
#' \donttest{
#' if (online("api.github.com")) {
#'   # GitHub can refuse when its request limit is reached; then this says why
#'   meta <- tryCatch(papers_metadata("jdm"),
#'                    error = function(e) message(conditionMessage(e)))
#'   if (!is.null(meta)) meta$dc_coverage
#' }
#' }
papers_metadata <- function(name, repo = "scienceverse/papers") {
  url <- sprintf(
    "https://raw.githubusercontent.com/%s/main/%s/metadata.json", repo, name
  )

  resp <- tryCatch(
    .mc_request(url) |>
      .github_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) stop("Could not reach GitHub: ", e$message, call. = FALSE)
  )

  if (httr2::resp_status(resp) == 404) {
    avail <- paste(papers_available(repo)$name, collapse = ", ")
    stop("No metadata.json found for '", name, "' in ", repo, ". ",
         "Available: ", avail, call. = FALSE)
  }
  if (httr2::resp_status(resp) != 200) {
    stop("GitHub error (status ", httr2::resp_status(resp), "): ", url,
         call. = FALSE)
  }

  # raw.githubusercontent.com serves .json files as text/plain, so the
  # content-type check in resp_body_json() must be disabled
  meta <- httr2::resp_body_json(resp, check_type = FALSE, simplifyVector = TRUE)
  names(meta) <- gsub(":", "_", names(meta), fixed = TRUE)
  meta
}


# ----- internal helpers ------------------------------------------------------

#' @keywords internal
.papers_cache_dir <- function() {
  tools::R_user_dir("metacheck", "data") |>
    file.path("papers")
}

#' Fetch all RDS release assets from a GitHub repo
#'
#' Queries all releases (not just the latest) so corpora can be spread across
#' multiple releases. Release tags follow a `{corpus}-{date}` convention
#' (e.g. `collabra-2026-06-18`); if a corpus has been re-released under a new
#' tag (e.g. to fix a mistake in an older, now-immutable release), only the
#' most recently published release for that corpus family is kept. Returns
#' one row per .rds asset.
#'
#' @returns a data frame with columns `name`, `tag`, `size`, `download_url`
#'   and `api_url` (the address a private repository's file is downloaded
#'   from, with a token), one row per .rds asset (no rows when there is none)
#' @keywords internal
.papers_release_assets <- function(repo = "scienceverse/papers") {
  url <- sprintf("https://api.github.com/repos/%s/releases", repo)

  resp <- tryCatch(
    .mc_request(url) |>
      .github_config() |>
      httr2::req_headers(Accept = "application/vnd.github+json") |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) stop("Could not reach GitHub API: ", e$message, call. = FALSE)
  )

  if (httr2::resp_status(resp) != 200) {
    # GitHub answers 404 for a private repository it is not shown
    hint <- if (httr2::resp_status(resp) == 404) {
      paste0(" If the repository is private, set a GitHub token with access ",
             "to it (see ?github_pat).")
    } else ""
    stop("GitHub API error (status ", httr2::resp_status(resp), "): ", url,
         hint, call. = FALSE)
  }

  releases <- httr2::resp_body_json(resp)

  if (length(releases) == 0) {
    return(data.frame(name = character(0), tag = character(0),
                      size = integer(0), download_url = character(0),
                      api_url = character(0)))
  }

  # keep only the most recently published release per corpus family
  # (tag minus its trailing -YYYY-MM-DD date)
  tags <- sapply(releases, `[[`, "tag_name")
  published <- sapply(releases, `[[`, "published_at")
  family <- sub("-\\d{4}-\\d{2}-\\d{2}$", "", tags)
  newest <- order(published, decreasing = TRUE)
  releases <- releases[newest][!duplicated(family[newest])]

  rows <- lapply(releases, \(rel) {
    tag <- rel$tag_name
    assets <- rel$assets
    if (length(assets) == 0) return(NULL)
    rds <- Filter(\(a) grepl("\\.rds$", a$name, ignore.case = TRUE), assets)
    if (length(rds) == 0) return(NULL)
    data.frame(
      name         = sapply(rds, `[[`, "name"),
      tag          = tag,
      size         = sapply(rds, `[[`, "size"),
      download_url = sapply(rds, `[[`, "browser_download_url"),
      api_url      = vapply(rds, \(a) a$url %||% NA_character_, character(1))
    )
  })

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  # no release with an .rds file: an empty table, as for no releases; NULL here
  # stopped papers_available() and then papers_load() (U18)
  if (is.null(out)) out <- data.frame(name = character(0), tag = character(0),
                                      size = numeric(0), download_url = character(0),
                                      api_url = character(0))
  out
}
