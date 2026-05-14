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
#' github_links(psychsci)
github_links <- function(paper) {
  href <- text <- text_id <- NULL

  # # strip punctuation off the end of sentences to avoid weird matches
  strip_text <- search_text(paper, ".*[^\\.$]", return = "match", perl = TRUE)
  #
  # # search for github URLS
  github_regex <- "(?:https?://)?github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"
  # found_gh <- search_text(strip_text, github_regex, return = "match", perl = TRUE)

  found_gh <- paper_table(paper, "url") |>
    search_text(github_regex, perl = TRUE) |>
    dplyr::select(href, text_id, paper_id)


  # find github repos referenced only by org/repo near github (+-10 words)
  # like "See our github repo at scienceverse/metacheck"
  plusminus <- "(?:\\b\\w+\\b\\W+){0,10}\\bgithub(\\.com)?\\b(?:\\W+\\b\\w+\\b){0,10}"
  no_github_regex <- "[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:\\.git)?"
  other_gh <- search_text(strip_text, "github") |>
    search_text(github_regex, exclude = TRUE, perl = TRUE) |>
    search_text("github.io", exclude = TRUE) |>
    search_text(plusminus, return = "match") |>
    search_text(no_github_regex, return = "match", perl = TRUE) |>
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

  # get repo name ----
  match <- regexec("(?<=^|/)([a-z0-9-])+/([a-z0-9\\._-])+(?=\\.git|/|$)",
    repo,
    perl = TRUE, ignore.case = TRUE
  )
  thematch <- regmatches(repo, match)

  if (length(thematch) == 0 || length(thematch[[1]]) == 0) {
    return(NULL)
  }

  simple_repo <- sub("\\.git$", "", x = thematch[[1]][[1]])

  url <- paste0("https://github.com/", simple_repo)
  resp <- httr2::request(url) |>
    httr2::req_method("HEAD") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    return(NULL)
  }

  return(simple_repo)
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

  resp <- httr2::request(readme_url) |>
    github_config() |>
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

    file_lists <- lapply(unique_repos, github_files, recursive = recursive)
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

  resp <- httr2::request(url) |>
    github_config() |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  headers <- httr2::resp_headers(resp)
  contents <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())

  if (httr2::resp_status(resp) != 200) {
    rl <- headers$`x-ratelimit-remaining`
    if (!is.null(rl) && as.integer(rl) == 0) {
      reset <- headers$`x-ratelimit-reset` |>
        as.integer() |>
        as.POSIXct() |>
        format("%Y-%m-%d %T")
      message("Rate limit exceeded, resetting at ", reset)
    } else {
      message(dir, ": ", contents$message)
    }
    # return NULL instead of error to handle rate limit exceeding at end of file list, will still return files up to that point
    return(NULL)
  }

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


#' GitHub Configuration
#'
#' Adds GitHub auth and accept headers to an httr2 request.
#'
#' @param req an httr2 request object
#'
#' @returns the modified request
#' @export
#'
#' @keywords internal
github_config <- function(req) {
  token <- tryCatch(
    gitcreds::gitcreds_get(),
    error = function(e) NULL
  )

  req <- req |>
    httr2::req_headers(
      Accept = "application/vnd.github.v3+json",
      `User-Agent` = "scienceverse/metacheck"
    )

  if (!is.null(token)) {
    req <- req |>
      httr2::req_headers(Authorization = paste("token", token$password))
  }

  return(req)
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

  resp <- httr2::request(url) |>
    github_config() |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  languages <- tryCatch(httr2::resp_body_json(resp), error = \(e) list())
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
