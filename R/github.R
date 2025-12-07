#' Get GitHub Repo Info
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#' @param recursive whether to search the files recursively
#'
#' @returns a list of information about the repo
#' @export
#'
#' @examples
#' \donttest{
#'   github_info("scienceverse/metacheck")
#' }
github_info <- function(repo, recursive = FALSE) {
  repo <- github_repo(repo)
  if (is.null(repo)) return(NULL)

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
  if (length(repo) == 0) return(NULL)

  if (length(repo) > 1) {
    res <- sapply(repo, github_repo)
    return(res)
  }

  # get repo name ----
  match <- regexec("(?<=^|/)([a-z0-9-])+/([a-z0-9\\._-])+(?=\\.git|/|$)",
                   repo, perl = TRUE, ignore.case = TRUE)

  simple_repo <- regmatches(repo, match)[[1]][[1]] |>
    sub("\\.git$", "", x = _)

  url <- paste0("https://github.com/", simple_repo)
  head <- httr::HEAD(url)

  if (head$status_code != 200) {
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
#' \donttest{
#'   github_readme("scienceverse/metacheck")
#' }
github_readme <- function(repo) {
  if (length(repo) > 1) {
    res <- sapply(repo, github_readme)
    return(res)
  }

  repo <- github_repo(repo)
  if (is.null(repo)) return("")

  readme_url <- sprintf(
    "https://api.github.com/repos/%s/readme",
    repo
  )

  results <- httr::GET(readme_url, github_config())
  if (results$status_code == 200) {
    content <-  httr::content(results, "parsed")
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
#' \donttest{
#'   github_files("scienceverse/metacheck")
#' }
github_files <- function(repo, dir = "",
                         recursive = FALSE) {
  repo <- github_repo(repo)
  if (is.null(repo)) return(NULL)

  url <- sprintf(
    "https://api.github.com/repos/%s/contents/%s",
    repo,
    dir
  ) |> utils::URLencode()

  response <- httr::GET(url, github_config())
  headers <- httr::headers(response)
  contents <-  httr::content(response, "parsed")

  if (response$status_code != 200) {
    if (as.integer(headers$`x-ratelimit-remaining`) == 0) {
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
      if (length(x) < 2) return("")
      x[[length(x)]]
    }) |> tolower()
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
#' @returns a list of config items to use in httr::GET()
#' @export
#'
#' @keywords internal
github_config <- function() {
  token <- tryCatch(
    gitcreds::gitcreds_get(),
    error = function(e) NULL
  )

  if (!is.null(token)) {
    config <- httr::add_headers(
      Authorization = paste("token", token$password),
      Accept = "application/vnd.github.v3+json",
      `User-Agent` = "scienceverse/metacheck"
    )
  } else {
    config <- httr::add_headers(
      Accept = "application/vnd.github.v3+json",
      `User-Agent` = "scienceverse/metacheck"
    )
  }

  return(config)
}

#' Get Languages from GitHub Repo
#'
#' @param repo The URL of the repository (in the format "username/repo" or "https://github.com/username/repo")
#'
#' @returns vector of languages
#' @export
#'
#' @examples
#' \donttest{
#'   github_languages("scienceverse/metacheck")
#' }
github_languages <- function(repo) {
  if (length(repo) > 1) {
    res <- lapply(repo, github_languages)
    tbl <- do.call(dplyr::bind_rows, res)
    return(tbl)
  }

  repo <- github_repo(repo)
  if (is.null(repo)) return(NULL)

  url <- sprintf(
    "https://api.github.com/repos/%s/languages",
    repo
  )

  results <- httr::GET(url, github_config())
  languages <- httr::content(results, "parsed")
  lang_df <- data.frame(
    repo = repo,
    language = names(languages),
    bytes = unlist(languages),
    row.names = NULL
  )

  return(lang_df)
}
