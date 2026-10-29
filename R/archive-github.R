# A paper's url table with its href as the text to search. Without a text
# column, text_search() searches the table's first column, which in a bibr
# 12.x paper is the integer url_id, so no GitHub or GitLab link was ever
# found (U196).
.url_hrefs <- function(paper) {
  urls <- paper_table(paper, "url")
  if ("href" %in% names(urls)) urls$text <- urls$href
  urls
}

#' Find GitHub Links in Papers
#'
#' GitHub links can be in PDFs in several ways.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the GitHub url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' github_links(psychsci)
#' }
github_links <- function(paper) {
  href <- text <- text_id <- NULL

  # # strip punctuation off the end of sentences to avoid weird matches
  strip_text <- text_search(paper, ".*[^\\.$]", return = "match", perl = TRUE)
  #
  # # search for github URLS
  github_regex <- "(?:https?://)?github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"
  # found_gh <- text_search(strip_text, github_regex, return = "match", perl = TRUE)

  found_gh <- .url_hrefs(paper) |>
    text_search(github_regex, perl = TRUE) |>
    dplyr::select(href, text_id, paper_id)

  # A link to an account ("https://github.com/OneZoom/") names no repository,
  # but is kept so repo_check can report it as an account rather than drop it
  # without a word (see .github_account_name(), #485).
  account_regex <- "^(?:https?://)?(?:www\\.)?github\\.com/(?:orgs/)?[A-Za-z0-9-]+/?[.,;:)]*$"
  found_account <- .url_hrefs(paper) |>
    text_search(account_regex, perl = TRUE) |>
    dplyr::select(href, text_id, paper_id)
  found_account <- found_account[
    !vapply(found_account$href, \(u) is.null(.github_account_name(u)), logical(1)), ,
    drop = FALSE]
  found_gh <- dplyr::bind_rows(found_gh, found_account)


  # find github repos referenced only by org/repo near github (+-10 words)
  # like "See our github repo at scienceverse/metacheck"
  plusminus <- "(?:\\b\\w+\\b\\W+){0,10}\\bgithub(\\.com)?\\b(?:\\W+\\b\\w+\\b){0,10}"
  no_github_regex <- "[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:\\.git)?"
  other_gh <- text_search(strip_text, "github") |>
    text_search(github_regex, exclude = TRUE, perl = TRUE) |>
    text_search("github.io", exclude = TRUE) |>
    text_search(plusminus, return = "match") |>
    text_search(no_github_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, text_id, paper_id)

  all_gh <- dplyr::bind_rows(found_gh, other_gh)

  return(all_gh)
}

#' Get GitHub Repo Info
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#' @param recursive whether to search the files recursively
#'
#' @returns a list of information about the repo
#' @export
#'
#' @examples
#' \dontrun{
#' github_info("scienceverse/metacheck")
#' }
github_info <- function(repo, recursive = FALSE) {
  repo <- github_repo(repo)
  if (is.null(repo)) {
    return(NULL)
  }

  readme <- github_readme(repo)
  languages <- github_languages(repo)
  files <- github_files(repo, recursive = recursive)

  list(
    repo = repo,
    readme = readme,
    files = files,
    languages = languages
  )
}

#' Get Short GitHub Repo Name
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#'
#' @returns character string of short repo name
#' @export
#'
#' @examples
#' github_repo("scienceverse/metacheck")
#' github_repo("https://github.com/scienceverse/metacheck/")
#' github_repo("https://github.com/scienceverse/metacheck.git")
github_repo <- function(repo) {
  if (length(repo) == 0) {
    return(NULL)
  }

  if (length(repo) > 1) {
    res <- sapply(repo, github_repo)
    return(res)
  }

  simple_repo <- .github_repo_name(repo)
  if (is.null(simple_repo)) {
    return(NULL)
  }

  url <- paste0("https://github.com/", simple_repo)
  resp <- .mc_request(url) |>
    httr2::req_method("HEAD") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    return(NULL)
  }

  return(simple_repo)
}

# The "owner/repo" text of a GitHub URL or bare mention, WITHOUT checking that
# the repository exists (github_repo() adds that check with a HEAD request to
# the github.com page). For a caller whose next step is a GitHub API request
# about the same repository -- github_tree_files()'s /repos/{owner}/{repo} --
# that HEAD is redundant: the API's own 404 gives the same answer, so the
# check cost one extra request per repository (survey 2026-10-13). The
# owner/repo pattern is still enforced here, which is what keeps an arbitrary
# "word/word" text match near the word "github" (github_links()'s bare-mention
# fallback) from reaching the API at all. Returns NULL when no owner/repo
# pattern is found. Single value only.
.github_repo_name <- function(repo) {
  if (length(repo) != 1 || is.na(repo)) return(NULL)

  match <- regexec("(?<=^|/)([a-z0-9-])+/([a-z0-9\\._-])+(?=\\.git|/|$)",
    repo,
    perl = TRUE, ignore.case = TRUE
  )
  thematch <- regmatches(repo, match)

  if (length(thematch) == 0 || length(thematch[[1]]) == 0) {
    return(NULL)
  }

  out <- sub("\\.git$", "", x = thematch[[1]][[1]])
  # GitHub does not allow a repository named "." or "..". Such a match is an
  # account address followed by a sentence's full stop
  # ("https://github.com/OneZoom/."), see .github_account_name() (#485).
  if (grepl("/\\.+$", out)) return(NULL)
  out
}

# The account (user or organisation) name of a GitHub address that names an
# account but no repository: "https://github.com/OneZoom", with or without a
# trailing slash or sentence punctuation, or "github.com/orgs/OneZoom". Such
# a page lists an account's repositories, but does not say which one holds a
# paper's files (#485). NULL when the address is not an account page. Single
# value only.
.github_account_name <- function(url) {
  if (length(url) != 1 || is.na(url)) return(NULL)
  m <- regmatches(url, regexec(
    "^(?:https?://)?(?:www\\.)?github\\.com/(?:orgs/)?([A-Za-z0-9-]+)/?[.,;:)]*$",
    trimws(url), perl = TRUE, ignore.case = TRUE))[[1]]
  if (length(m) < 2) return(NULL)
  # GitHub's own pages at the same depth are not accounts.
  site_pages <- c("about", "features", "topics", "explore", "marketplace",
                  "pricing", "sponsors", "settings", "login", "join",
                  "trending", "collections", "search", "enterprise", "orgs")
  if (tolower(m[[2]]) %in% site_pages) return(NULL)
  m[[2]]
}

#' Get README from GitHub
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#'
#' @returns a character string of the README contents
#' @export
#'
#' @examples
#' \dontrun{
#' github_readme("scienceverse/metacheck")
#' }
github_readme <- function(repo) {
  if (length(repo) > 1) {
    res <- sapply(repo, github_readme)
    return(res)
  }

  repo <- github_repo(repo)
  if (is.null(repo)) {
    return("")
  }

  readme_url <- sprintf(
    "https://api.github.com/repos/%s/readme",
    repo
  )

  resp <- .mc_request(readme_url) |>
    .github_config() |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) == 200) {
    content <- httr2::resp_body_json(resp)
    readme <- base64enc::base64decode(content$content) |> rawToChar()
  } else {
    readme <- ""
  }

  return(readme)
}

#' Get File List from GitHub
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#' @param dir an optional directory name to search
#' @param recursive whether to search the files recursively
#'
#' @returns a data frame of files
#' @export
#'
#' @examples
#' \dontrun{
#' github_files("scienceverse/metacheck")
#' }
github_files <- function(repo, dir = "",
                         recursive = FALSE) {
  # vectorise
  if (length(repo) > 1) {
    unique_repos <- unique(repo) |> setdiff(NA)

    # dir passed on: each repository was listed from its root (U48)
    file_lists <- lapply(unique_repos, github_files, dir = dir, recursive = recursive)
    info <- do.call(dplyr::bind_rows, args = file_lists)
    orig <- data.frame(repo = repo)
    df <- dplyr::left_join(orig, info, by = "repo")

    return(df)
  }

  clean_repo <- github_repo(repo)
  if (is.null(clean_repo)) {
    return(NULL)
  }

  url <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    clean_repo,
    dir
  ) |> utils::URLencode()

  resp <- .mc_request(url) |>
    .github_config() |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  headers <- httr2::resp_headers(resp)
  contents <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())

  if (httr2::resp_status(resp) != 200) {
    limit <- .github_limit(resp)
    if (!is.null(limit)) {
      message(limit)
    } else {
      message(dir, ": ", contents$message)
    }
    # return NULL instead of error to handle rate limit exceeding at end of file list, will still return files up to that point
    return(NULL)
  }

  # a single file comes back as one object, not a list of them, and an
  # empty folder as an empty list; both stopped the listing (U48)
  if (!is.null(contents$name)) contents <- list(contents)
  if (length(contents) == 0) return(NULL)

  files <- lapply(contents, \(file) {
    data.frame(
      repo = repo,
      clean_repo = clean_repo,
      name = file$name,
      path = file$path,
      download_url = ifelse(is.null(file$download_url), NA, file$download_url),
      ft = file$type,
      size = file$size
    )
  }) |> do.call(rbind, args = _)

  # fix double slashes
  files$name <- files$name |>
    gsub("/+", "/", x = _) |>
    gsub("^/|/$", "", x = _)

  files <- sort_by(files, files$path)
  files$ext <- strsplit(files$name, "\\.") |>
    sapply(\(x) {
      if (length(x) < 2) {
        return("")
      }
      x[[length(x)]]
    }) |>
    tolower()
  files <- dplyr::left_join(files, metacheck::file_types, by = "ext")
  files$type[is.na(files$type)] <- files$ft[is.na(files$type)]
  files$ft <- NULL

  # get dir contents if recursive ----
  if (isTRUE(recursive)) {
    subdirs <- files$path[files$type == "dir"]
    if (length(subdirs)) {
      dir_contents <- lapply(subdirs, \(subdir) {
        # message(subdir)
        github_files(repo, subdir, recursive = TRUE)
      }) |> do.call(rbind, args = _)

      files <- rbind(files, dir_contents)
    }
  }

  return(files)
}


#' Get GitHub repository files via the Git Trees API
#'
#' Fetches the complete file tree of a GitHub repository in two API calls
#' (repo metadata + recursive tree), rather than the N-request recursive
#' \code{/contents/} crawl used by \code{github_files()}.
#'
#' A repository is listed in full whatever its size, matching how every other
#' archive source (OSF, Zenodo, ResearchBox, PsychArchives, local) is treated.
#' Because the Git Trees API returns the whole tree in ONE request, listing a
#' large repository costs no more than listing a small one, so there is nothing
#' to protect against at this stage. What a repository's size does limit is how
#' much of it is DOWNLOADED, and that is handled downstream by
#' \code{download_repo_files()}'s per-file and per-repository budgets, which
#' fill smallest-file-first up to the cap and report what was omitted.
#'
#' The one case that still refuses is GitHub's own hard API limit: a tree with
#' more than 100,000 entries comes back flagged \code{truncated}, meaning the
#' response is incomplete. There is no way to list such a repository through
#' this endpoint, so it is reported rather than silently returning a partial
#' file list.
#'
#' Returns a list with:
#' \describe{
#'   \item{\code{gated}}{logical; \code{TRUE} only when the repository could
#'     not be listed at all (invalid/inaccessible, or a truncated tree)}
#'   \item{\code{reason}}{character reason, or \code{NA}}
#'   \item{\code{files}}{data.frame shaped like \code{github_files()} output,
#'     or \code{NULL} when the tree could not be fetched}
#'   \item{\code{default_branch}}{character}
#' }
#'
#' @param repo GitHub repo URL or \code{"owner/repo"} string
#'
#' @export
#' @keywords internal
github_tree_files <- function(repo) {
  invalid <- list(gated = TRUE,
                  reason = "invalid or inaccessible GitHub repository",
                  files  = NULL, default_branch = NA_character_,
                  license = NA_character_)

  # Text pattern only -- no HEAD request to the github.com page first (see
  # .github_repo_name()): the metadata request below answers "does this
  # repository exist?" itself, with a 404.
  # An account page is reachable; it is just not a repository (#485). Checked
  # first, because "github.com/orgs/OneZoom" also has the owner/repo shape.
  if (!is.null(.github_account_name(repo))) {
    invalid$reason <- "links to a GitHub account, not to a specific repository"
    return(invalid)
  }
  clean_repo <- .github_repo_name(repo)
  if (is.null(clean_repo)) return(invalid)

  # ── 1. Repo metadata (size + default branch, 1 request) ─────────────────────
  meta_resp <- tryCatch(
    .mc_request(sprintf("https://api.github.com/repos/%s", clean_repo)) |>
      .github_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  # 404 = no such repository, or a private one this token cannot see: the
  # same outcome the github.com HEAD check used to report before this request
  # was ever made. Any OTHER failure (403/429 rate limit, 5xx, no connection)
  # says nothing about the repository, so it still falls through to the
  # github_files() crawl below, as before.
  if (!is.null(meta_resp) && httr2::resp_status(meta_resp) == 404) return(invalid)
  # GitHub's limit (60 requests per hour without a key, 5,000 with one) makes
  # the repository "not listed", not empty: the crawl below would be refused
  # too, and its empty result read as a repository without files.
  limit <- if (!is.null(meta_resp)) .github_limit(meta_resp)
  if (!is.null(limit)) {
    return(list(gated = TRUE, reason = limit, files = NULL,
                default_branch = NA_character_, license = NA_character_))
  }
  if (is.null(meta_resp) || httr2::resp_status(meta_resp) != 200) {
    return(.github_crawl(repo, "main", NA_character_))
  }

  meta           <- httr2::resp_body_json(meta_resp)
  default_branch <- meta$default_branch %||% "main"
  # GitHub's own repo-metadata response already includes a detected licence
  # (SPDX id, e.g. "MIT", "AGPL-3.0"), or NULL when GitHub could not detect
  # one from a LICENSE-style file -- confirmed live against a real public
  # repo. No separate API call needed: this is the same request that already
  # supplies default_branch above.
  # %empty_or% (not %||%) because spdx_id can come back as a length-zero
  # value rather than NULL, which would otherwise propagate as a
  # length-zero "license" into the caller's vapply(..., character(1)).
  license <- .field_or_value(meta$license, "spdx_id") %empty_or% NA_character_

  # ── 2. Git tree (recursive, 1 request) ──────────────────────────────────────
  tree_resp <- tryCatch(
    .mc_request(
      sprintf("https://api.github.com/repos/%s/git/trees/%s?recursive=1",
              clean_repo, default_branch)) |>
      .github_config() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  limit <- if (!is.null(tree_resp)) .github_limit(tree_resp)
  if (!is.null(limit)) {
    return(list(gated = TRUE, reason = limit, files = NULL,
                default_branch = default_branch, license = license))
  }
  if (is.null(tree_resp) || httr2::resp_status(tree_resp) != 200) {
    return(.github_crawl(repo, default_branch, license))
  }

  tree <- httr2::resp_body_json(tree_resp)

  if (isTRUE(tree$truncated))
    return(list(
      gated  = TRUE,
      reason = "GitHub repo tree truncated (>100 000 items); too large to list",
      files  = NULL, default_branch = default_branch, license = license))

  blobs   <- Filter(\(x) x$type == "blob", tree$tree %||% list())
  n_files <- length(blobs)

  # ── 3. Build file data.frame ─────────────────────────────────────────────────
  # %empty_or% (not %||%) because a field can come back as a length-zero
  # value rather than NULL; vapply(..., character(1)/numeric(1)) requires
  # exactly length 1 from every call.
  paths <- vapply(blobs, \(x) x$path %empty_or% "", character(1))
  if (n_files == 0) {
    files_df <- data.frame(
      repo = character(0), clean_repo = character(0), name = character(0),
      path = character(0), download_url = character(0), size = numeric(0),
      type = character(0), stringsAsFactors = FALSE)
  } else {
    raw_base <- sprintf("https://raw.githubusercontent.com/%s/%s/",
                        clean_repo, default_branch)
    files_df <- data.frame(
      repo         = repo,
      clean_repo   = clean_repo,
      name         = basename(paths),
      path         = paths,
      download_url = paste0(raw_base, paths),
      size         = vapply(blobs, \(x) x$size %empty_or% NA_real_, numeric(1)),
      ft           = "file",
      stringsAsFactors = FALSE)
    files_df$ext  <- tolower(tools::file_ext(files_df$name))
    files_df      <- dplyr::left_join(files_df, metacheck::file_types, by = "ext")
    files_df$type[is.na(files_df$type)] <- files_df$ft[is.na(files_df$type)]
    files_df$ft   <- NULL
    files_df$ext  <- NULL
  }

  list(gated = FALSE, reason = NA_character_,
       files = files_df, default_branch = default_branch, license = license)
}


# The /contents/ crawl of github_files(), for when the tree could not be
# fetched. A crawl that GitHub's limit stopped is "not listed", like the tree
# requests above, rather than a repository without files.
.github_crawl <- function(repo, default_branch, license) {
  files_df <- tryCatch(github_files(repo, recursive = TRUE), error = \(e) NULL)
  problem <- .key_problem("github")
  if (is.null(files_df) && identical(problem$problem, "limit")) {
    return(list(gated = TRUE, files = NULL,
                reason = paste0("API rate limit exhausted: ",
                                .key_problem_message("github", "limit", problem$until)),
                default_branch = default_branch, license = license))
  }
  list(gated = FALSE, reason = NA_character_, files = files_df,
       default_branch = default_branch, license = license)
}

# The message for a response refused because GitHub's limit was reached
# (recorded, see .key_limit_reached()), or NULL for any other response.
# GitHub answers 403 or 429 with x-ratelimit-remaining 0 and the end of the
# hour in x-ratelimit-reset; its secondary limit (too many requests at once)
# answers the same statuses with retry-after instead.
.github_limit <- function(resp) {
  if (!httr2::resp_status(resp) %in% c(403L, 429L)) return(NULL)
  remaining <- httr2::resp_header(resp, "x-ratelimit-remaining")
  reset <- suppressWarnings(as.numeric(httr2::resp_header(resp, "x-ratelimit-reset")))
  wait <- suppressWarnings(as.numeric(httr2::resp_header(resp, "retry-after")))
  if (!identical(remaining, "0") && length(wait) == 0) return(NULL)
  until <- if (length(reset) == 1 && !is.na(reset)) {
    as.POSIXct(reset, origin = "1970-01-01")
  } else if (length(wait) == 1 && !is.na(wait)) {
    Sys.time() + wait
  } else NA
  .key_limit_reached("github", until)
}

#' GitHub Configuration
#'
#' Adds GitHub auth and accept headers to an httr2 request. The key is the
#' one [github_pat()] returns, left out when GitHub refuses it (see
#' [api_key_status()]).
#'
#' @param req an httr2 request object
#'
#' @returns the modified request
#' @export
#'
#' @keywords internal
.github_config <- function(req) {
  token <- .key_use("github", check = .github_key_check)

  req <- req |>
    httr2::req_headers(
      Accept = "application/vnd.github.v3+json",
      `User-Agent` = "scienceverse/metacheck"
    )

  if (nzchar(token)) {
    req <- req |>
      httr2::req_headers(Authorization = paste("token", token))
  }

  return(req)
}

# GitHub's /rate_limit does not count toward the limit.
.github_key_check <- function(key) {
  .key_check_request("https://api.github.com/rate_limit",
                     list(Authorization = paste("token", key)))
}

#' Set or get a GitHub token
#'
#' A token raises GitHub's limit from 60 to 5,000 requests per hour. A
#' fine-grained token with access to public repositories only reads nothing
#' that is not public: make one at
#' <https://github.com/settings/personal-access-tokens/new>, choose "Public
#' repositories" under "Repository access", and add no permissions.
#'
#' Without a token set here, the token is found the way git finds one: the
#' `GITHUB_PAT` environment variable, else git's credential store (see
#' [gitcreds::gitcreds_get()]). Store it in your `.Renviron` file as
#' `GITHUB_PAT="your-token"` so it is read every time R starts.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' \dontrun{
#' github_pat()
#' }
github_pat <- function(pat = NULL) {
  opt <- "metacheck.github.pat"
  if (is.null(pat)) {
    set <- getOption(opt) %||% ""
    if (nzchar(set)) return(set)
    token <- tryCatch(gitcreds::gitcreds_get(), error = \(e) NULL)
    return(token$password %||% "")
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set github_pat with a single string containing your GitHub token",
         call. = FALSE)
  }
  options(metacheck.github.pat = pat)
  invisible(pat)
}

#' Get Languages from GitHub Repo
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#'
#' @returns vector of languages
#' @export
#'
#' @examples
#' \dontrun{
#' github_languages("scienceverse/metacheck")
#' }
github_languages <- function(repo) {
  if (length(repo) > 1) {
    res <- lapply(repo, github_languages)
    tbl <- do.call(dplyr::bind_rows, res)
    return(tbl)
  }

  repo <- github_repo(repo)
  if (is.null(repo)) {
    return(NULL)
  }

  url <- sprintf(
    "https://api.github.com/repos/%s/languages",
    repo
  )

  resp <- .mc_request(url) |>
    .github_config() |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  languages <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
  # an error answer's fields ("message", "documentation_url") are no languages (U48)
  if (httr2::resp_status(resp) != 200) languages <- list()
  if (length(languages)) {
    lang_df <- data.frame(
      repo = repo,
      language = names(languages),
      bytes = unlist(languages),
      row.names = NULL
    )
  } else {
    lang_df <- data.frame(
      repo = repo,
      language = NA_character_,
      bytes = NA_real_,
      row.names = NULL
    )
  }

  return(lang_df)
}
