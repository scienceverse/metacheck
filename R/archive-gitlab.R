# GitLab.com is a single hosted service, matched by hostname the same way
# github_links() matches github.com. Self-hosted GitLab instances (a
# different institution's own gitlab.example.org) are not detected: unlike
# GitHub, which is one host, "GitLab" as Cooper et al. code it could be any
# of thousands of independently-run instances, and there is no way to tell
# a self-hosted GitLab URL apart from an arbitrary institutional website by
# pattern alone.
#
# API notes verified live 2026-08-30 against real public projects
# (gitlab-org/gitlab, gitlab-org/gitlab-runner):
#   - GET /api/v4/projects/:id returns default_branch directly, and a
#     `license` object (key/name/html_url) ONLY when the request includes
#     `?license=true` -- omitted otherwise. This differs from GitHub, whose
#     equivalent call includes license by default.
#   - GET /api/v4/projects/:id/repository/tree?recursive=true is PAGINATED
#     (up to 100 entries per page by default, more with per_page up to 100;
#     x-total-pages / x-next-page response headers), unlike GitHub's Git
#     Trees API, which returns an entire recursive tree in one request. A
#     full listing needs a page-following loop.
#   - The tree endpoint does NOT return file size (only id/name/type/path/
#     mode). File size requires a SEPARATE call; the per-file REST endpoint
#     (/repository/files/:file_path) returns one file's size but is
#     impractical to call once per file for a large repository. The
#     GraphQL API's repository.blobs(paths: [...]) query returns size for
#     a batch of paths in a single request -- used here instead of N
#     separate REST calls. GraphQL enforces its own query-complexity budget
#     (default max 200) that a large paths: array can exceed; see
#     .gitlab_blob_sizes()'s own comment for the confirmed-live cutoff and
#     the batch size used here.
#   - GET /api/v4/projects/:id/repository/archive.zip?sha=<ref> returns the
#     whole repository as a zip. It does NOT support HTTP Range requests
#     (confirmed live: a Range header request came back 200, not 206, with
#     accept-ranges: none), so the zip_peek()-style trick used for GitHub's
#     zipball (reading only the central directory via a Range request) does
#     not work here -- the archive must be downloaded in full, the same way
#     Dryad's whole-dataset endpoint is already handled.
#   - :id in every endpoint above accepts either the numeric project id or
#     the URL-encoded "namespace/project" path (e.g. "gitlab-org%2Fgitlab").
#     This module uses the path form throughout, so no separate id lookup
#     is needed -- but every query string in this file is built directly
#     into the URL rather than via httr2::req_url_query(): confirmed live
#     that req_url_query() on a request whose PATH already contains a
#     percent-encoded segment (the %2F in the project path) breaks the
#     request (404, where the identical call with the query hand-built into
#     the URL string 200s).

#' Find GitLab Links in Papers
#'
#' GitLab.com links can appear in papers the same two ways GitHub links do: a
#' real hyperlink, or a bare "owner/repo" mention near the word "gitlab".
#' Only gitlab.com is matched; a self-hosted GitLab instance's own domain is
#' indistinguishable from an arbitrary institutional website (see the file
#' header comment).
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the GitLab url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' gitlab_links(psychsci)
#' }
gitlab_links <- function(paper) {
  href <- text <- text_id <- NULL

  gitlab_regex <- "(?:https?://)?gitlab\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"

  found_gl <- paper_table(paper, "url") |>
    text_search(gitlab_regex, perl = TRUE) |>
    dplyr::select(href, text_id, paper_id)

  # find gitlab repos referenced only by "owner/repo" near "gitlab" (+-10
  # words), the same fallback pattern github_links() uses for GitHub.
  strip_text <- text_search(paper, ".*[^\\.$]", return = "match", perl = TRUE)
  plusminus <- "(?:\\b\\w+\\b\\W+){0,10}\\bgitlab(\\.com)?\\b(?:\\W+\\b\\w+\\b){0,10}"
  no_gitlab_regex <- "[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:\\.git)?"
  other_gl <- text_search(strip_text, "gitlab") |>
    text_search(gitlab_regex, exclude = TRUE, perl = TRUE) |>
    text_search("gitlab.io", exclude = TRUE) |>
    text_search(plusminus, return = "match") |>
    text_search(no_gitlab_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, text_id, paper_id)

  all_gl <- dplyr::bind_rows(found_gl, other_gl)

  return(all_gl)
}

#' Get Short GitLab Project Path
#'
#' @param repo The URL of the project (in the format "namespace/project" or
#'   "https://gitlab.com/namespace/project")
#'
#' @returns character string of the "namespace/project" path, or NULL if the
#'   project does not exist / is not reachable
#' @export
#'
#' @examples
#' gitlab_repo("gitlab-org/gitlab")
#' gitlab_repo("https://gitlab.com/gitlab-org/gitlab/")
#' gitlab_repo("https://gitlab.com/gitlab-org/gitlab.git")
gitlab_repo <- function(repo) {
  if (length(repo) == 0) {
    return(NULL)
  }

  if (length(repo) > 1) {
    res <- sapply(repo, gitlab_repo)
    return(res)
  }

  # A GitLab path can itself contain slashes (subgroups, e.g.
  # "gitlab-org/quality/triage-ops"), unlike GitHub's flat "owner/repo" --
  # so, unlike github_repo()'s regex, this only strips a leading
  # gitlab.com/ (if present) and a trailing .git, keeping every remaining
  # path segment rather than matching exactly two.
  path <- sub("^(?:https?://)?(?:www\\.)?gitlab\\.com/", "", repo,
             ignore.case = TRUE, perl = TRUE)
  path <- sub("\\.git/?$", "", path)
  path <- sub("/+$", "", path)
  path <- trimws(path)

  if (!nzchar(path) || !grepl("^[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)+$", path)) {
    return(NULL)
  }

  url <- paste0("https://gitlab.com/", path)
  resp <- httr2::request(url) |>
    httr2::req_method("HEAD") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    return(NULL)
  }

  return(path)
}

# GitLab's :id path segment: the URL-encoded "namespace/project" form,
# accepted everywhere a numeric project id would also work (see file header).
.gitlab_project_id <- function(path) {
  utils::URLencode(path, reserved = TRUE)
}

#' GitLab Configuration
#'
#' Adds GitLab auth headers to an httr2 request. A token is optional for
#' public projects (used here only to raise the low unauthenticated rate
#' limit); see [gitlab_pat()].
#'
#' @param req an httr2 request object
#'
#' @returns the modified request
#' @export
#' @keywords internal
.gitlab_config <- function(req) {
  req <- req |> httr2::req_headers(`User-Agent` = "scienceverse/metacheck")
  pat <- tryCatch(gitlab_pat(), error = \(e) "")
  if (nzchar(pat %||% "")) {
    req <- req |> httr2::req_headers(`PRIVATE-TOKEN` = pat)
  }
  req
}

#' Set or get a GitLab API token
#'
#' A personal access token from gitlab.com account settings. Optional for
#' listing/downloading PUBLIC projects (used here only to raise GitLab's
#' unauthenticated rate limit); required for private projects.
#'
#' Store it as the `GITLAB_PAT` environment variable so it is read every
#' time R starts.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' gitlab_pat() # returns "" unless a token is set
gitlab_pat <- function(pat = NULL) {
  .gitlab_pat(pat)
}

.gitlab_pat <- function(pat = NULL) {
  opt <- "metacheck.gitlab.pat"
  env <- "GITLAB_PAT"

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set gitlab_pat with a single string containing your GitLab token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Get GitLab project files, license, and default branch
#'
#' Fetches a GitLab project's file tree (following pagination, since
#' GitLab's tree endpoint caps at 100 entries per page, unlike GitHub's
#' single-request Git Trees API), its detected licence, and default branch.
#' File sizes are backfilled with a single batched GraphQL
#' `repository.blobs(paths: [...])` query, since the REST tree listing does
#' not include size.
#'
#' Returns a list with:
#' \describe{
#'   \item{\code{gated}}{logical; \code{TRUE} only when the project could
#'     not be listed at all (invalid/inaccessible)}
#'   \item{\code{reason}}{character reason, or \code{NA}}
#'   \item{\code{files}}{data.frame with repo/clean_repo/name/path/
#'     download_url/size/type, or \code{NULL} when the tree could not be
#'     fetched}
#'   \item{\code{default_branch}}{character}
#'   \item{\code{license}}{character SPDX-style key (e.g. "mit"), or NA}
#' }
#'
#' @param repo GitLab project URL or "namespace/project" string
#'
#' @export
#' @keywords internal
gitlab_tree_files <- function(repo) {
  clean_repo <- gitlab_repo(repo)
  if (is.null(clean_repo))
    return(list(gated = TRUE,
                reason = "invalid or inaccessible GitLab repository",
                files  = NULL, default_branch = NA_character_,
                license = NA_character_))

  proj_id <- .gitlab_project_id(clean_repo)

  # ── 1. Project metadata (default branch + licence) ──────────────────────────
  # license=true is required to get the license object at all -- omitted, the
  # field is simply absent from the response (confirmed live).
  meta_resp <- tryCatch(
    httr2::request(sprintf("https://gitlab.com/api/v4/projects/%s?license=true", proj_id)) |>
      .gitlab_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  if (is.null(meta_resp) || httr2::resp_status(meta_resp) != 200) {
    return(list(gated = TRUE,
                reason = "invalid or inaccessible GitLab repository",
                files  = NULL, default_branch = NA_character_,
                license = NA_character_))
  }

  meta           <- httr2::resp_body_json(meta_resp)
  default_branch <- meta$default_branch %||% "main"
  license        <- meta$license$key %||% NA_character_

  # ── 2. File tree (paginated, 100 entries/page) ───────────────────────────────
  all_entries <- list()
  page <- 1L
  repeat {
    # Query string built directly into the URL, NOT via httr2::req_url_query()
    # -- confirmed live that req_url_query() on a URL whose path already
    # contains a percent-encoded segment (proj_id's %2F) breaks the request
    # (came back 404 on a project that 200s with the query hand-built into
    # the URL string instead). The license request below has the same
    # percent-encoded proj_id and already avoids req_url_query() for this
    # reason.
    tree_resp <- tryCatch(
      httr2::request(sprintf(
        "https://gitlab.com/api/v4/projects/%s/repository/tree?recursive=true&per_page=100&page=%d",
        proj_id, page)) |>
        .gitlab_config() |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(),
      error = \(e) NULL)
    if (is.null(tree_resp) || httr2::resp_status(tree_resp) != 200) {
      if (page == 1L) {
        return(list(gated = FALSE, reason = NA_character_,
                    files = NULL, default_branch = default_branch,
                    license = license))
      }
      break
    }
    page_entries <- httr2::resp_body_json(tree_resp)
    if (length(page_entries) == 0) break
    all_entries <- c(all_entries, page_entries)

    headers <- httr2::resp_headers(tree_resp)
    next_page <- headers$`x-next-page`
    if (is.null(next_page) || !nzchar(next_page)) break
    page <- as.integer(next_page)
  }

  blobs <- Filter(\(x) x$type == "blob", all_entries)
  n_files <- length(blobs)

  # ── 3. Build file data.frame ─────────────────────────────────────────────────
  if (n_files == 0) {
    files_df <- data.frame(
      repo = character(0), clean_repo = character(0), name = character(0),
      path = character(0), download_url = character(0), size = numeric(0),
      type = character(0), stringsAsFactors = FALSE)
  } else {
    paths <- vapply(blobs, \(x) x$path %||% "", character(1))
    raw_base <- sprintf("https://gitlab.com/%s/-/raw/%s/",
                        clean_repo, utils::URLencode(default_branch))
    files_df <- data.frame(
      repo         = repo,
      clean_repo   = clean_repo,
      name         = basename(paths),
      path         = paths,
      download_url = paste0(raw_base, paths),
      size         = NA_real_,
      ft           = "file",
      stringsAsFactors = FALSE)

    # Backfill file size via one batched GraphQL query (confirmed live: the
    # REST tree listing has no size field, but
    # repository.blobs(paths: [...]) returns it for a batch of paths in a
    # single request). Batched in groups, since a very large repository's
    # full path list could otherwise make one oversized request.
    sizes <- .gitlab_blob_sizes(clean_repo, paths)
    if (length(sizes) > 0) {
      files_df$size <- unname(sizes[files_df$path])
    }

    files_df$ext  <- tolower(tools::file_ext(files_df$name))
    files_df      <- dplyr::left_join(files_df, metacheck::file_types, by = "ext")
    files_df$type[is.na(files_df$type)] <- files_df$ft[is.na(files_df$type)]
    files_df$ft   <- NULL
    files_df$ext  <- NULL
  }

  list(gated = FALSE, reason = NA_character_,
       files = files_df, default_branch = default_branch, license = license)
}

# File sizes for a batch of paths in a GitLab project, via the GraphQL
# repository.blobs(paths: [...]) query -- confirmed live to return size for
# multiple paths in one request, unlike the REST tree listing (no size) and
# the REST per-file endpoint (one file per request).
#
# Batched at 90 paths per request, NOT 100: GitLab's GraphQL API enforces a
# query complexity budget (default max 200), and a blobs(paths:) query's
# complexity scales with the number of paths requested. Confirmed live
# against gitlab-org/gitlab-runner: 94 paths succeeds, 95 paths already
# fails ("Query has complexity of 202, which exceeds max complexity of
# 200"), and 100 paths fails at complexity 212. A failed batch returns
# these errors in the response body with HTTP 200 (not a 4xx/5xx status),
# so this is silent unless checked -- the very first version of this
# function batched at 100 and silently returned zero sizes for most files
# in a >100-file repository, exactly this failure. 90 keeps a small margin
# below the empirically-confirmed 94-path cutoff, since complexity may vary
# slightly by path content/length in a different repository, not just count.
.gitlab_blob_sizes <- function(clean_repo, paths) {
  if (length(paths) == 0) return(numeric(0))

  batches <- split(paths, ceiling(seq_along(paths) / 90))
  parts <- lapply(batches, function(batch) {
    body <- list(query = sprintf(
      'query { project(fullPath: "%s") { repository { blobs(paths: [%s]) { nodes { path size } } } } }',
      clean_repo,
      paste(sprintf('"%s"', gsub('"', '\\\\"', batch)), collapse = ",")
    ))
    resp <- tryCatch(
      httr2::request("https://gitlab.com/api/graphql") |>
        httr2::req_method("POST") |>
        httr2::req_body_json(body) |>
        .gitlab_config() |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(),
      error = \(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
    res <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    nodes <- res$data$project$repository$blobs$nodes
    if (length(nodes) == 0) return(NULL)
    sizes <- vapply(nodes, \(n) as.numeric(n$size %||% NA_real_), numeric(1))
    names(sizes) <- vapply(nodes, \(n) n$path %||% NA_character_, character(1))
    sizes
  })

  # unname() the outer list before c()-ing: parts is a named list (named by
  # batch index), and c() on a named list of named vectors prefixes each
  # inner name with its outer name ("1..dockerignore" instead of
  # ".dockerignore") -- confirmed live this silently broke every lookup in
  # gitlab_tree_files() (sizes[files_df$path] found nothing, leaving every
  # file's size NA even though this function returned real values).
  parts <- unname(parts[!vapply(parts, is.null, logical(1))])
  if (length(parts) == 0) return(numeric(0))
  do.call(c, parts)
}
