#' Repository Check
#'
#' @description
#' This module retrieves information from repositories.
#'
#' @details
#' The Repository Check module lists files on the OSF, GitHub, and ResearchBox based on links in the manuscript.
#'
#' If you want to extend the package to be able to download files from additional data repositories reach out to the Metacheck development team.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
repo_check <- function(paper) {
  # get repository links ----
  # paper <- read(demoxml())

  ## get links ----
  osf_links_found <- osf_links(paper)
  github_links_found <- github_links(paper)
  rb_links_found <- rbox_links(paper)

  ## organise repos in a table
  osf_links_found$repo_type <- "osf"
  github_links_found$repo_type <- "github"
  rb_links_found$repo_type <- "researchbox"
  cols <- c("id", "text", "repo_type")
  repos <- dplyr::bind_rows(
    osf_links_found[, cols],
    github_links_found[, cols],
    rb_links_found[, cols]
  ) |> dplyr::distinct()
  names(repos)[2] <- "repo_url"
  repos$repo_error <- NA_character_

  # get files ----

  ## OSF ----
  osf_urls <- repos |>
    dplyr::filter(repo_type == "osf") |>
    _$repo_url |>
    unique()
  osf_files_df <- data.frame(repo_name = character(0))
  if (length(osf_urls) > 0) {
    suppressWarnings({
      osf_info <- lapply(osf_urls, \(x) {
        osf_files <- osf_retrieve(x, recursive = TRUE)
        osf_files$repo_name <- x
        osf_files
      }) |> dplyr::bind_rows()
    })

    # "kind" only in table if there are files
    if ("kind" %in% names(osf_info)) {
      osf_file_list <- osf_info |>
        dplyr::filter(kind == "file", !isFALSE(public))

      osf_files_df <- data.frame(
        repo_url = osf_file_list$repo_name,
        file_name = osf_file_list$name,
        file_url = osf_file_list$download_url,
        file_location = rep(NA_character_, nrow(osf_file_list)),
        file_size = osf_file_list$size,
        file_type = osf_file_list$filetype
      )
    }

    # remove e.g., registrations from repos list
    osf_to_remove <- osf_info |>
      dplyr::filter(!osf_type %in% c("nodes", "files", "private")) |>
      _$osf_url
    repos <- repos[!repos$repo_url %in% osf_to_remove, ]

    # note private repos
    private_repos <- osf_info |>
      dplyr::filter(osf_type %in% "private") |>
      _$osf_url
    if (length(private_repos)) {
      repos$repo_error[repos$repo_url %in% private_repos] <- "private"
    }
  }

  ## GitHub ----
  github_urls <- repos |>
    dplyr::filter(repo_type == "github") |>
    _$repo_url |>
    unique()
  github_files_df <- data.frame(repo_name = character(0))
  if (length(github_urls) > 0) {
    github_file_list <- github_files(github_urls, recursive = TRUE) |>
      dplyr::filter(type != "dir")

    github_files_df <- data.frame(
      repo_url = github_file_list$repo,
      file_name = github_file_list$name,
      file_url = github_file_list$download_url,
      file_location = rep(NA_character_, nrow(github_file_list)),
      file_size = github_file_list$size,
      file_type = github_file_list$type
    )
  }

  ## ResearchBox ----
  rb_urls <- repos |>
    dplyr::filter(repo_type == "researchbox") |>
    _$repo_url |>
    unique()
  rb_files_df <- data.frame(repo_name = character(0))
  if (length(rb_urls) > 0) {
    rb_file_list <- rbox_file_download(rb_urls) |>
      dplyr::filter(!isdir)
    rb_files_df <- data.frame(
      repo_url = rb_file_list$rb_url,
      file_name = rb_file_list$name,
      file_url = rb_file_list$rb_url,
      file_location = rb_file_list$file_location,
      file_size = rb_file_list$size,
      file_type = rb_file_list$type
    )
  }

  ## no repos found ----
  if (nrow(repos) == 0) {
    info <- list(
      traffic_light = "na",
      summary_text = "We found no links to repositories the Open Science Framework, Github, or ResearchBox.",
      summary_table = data.frame(
        id = paper$id,
        repo_n = 0,
        files_n = NA,
        files_data = NA,
        files_code = NA,
        files_readme = NA,
        files_zip = NA
      )
    )

    return(info)
  }

  ## file numbers and types ----
  all_files <- dplyr::bind_rows(osf_files_df, github_files_df, rb_files_df)

  # remove duplicate links
  # (can happen when same repo is referenced different ways)
  dupes <- duplicated(all_files$file_url)
  all_files <- all_files[!dupes, ]
  # remove repos that were only duplicates
  in_files <- repos$repo_url %in% all_files$repo_url
  repos <- repos[in_files, ]

  if (nrow(all_files) == 0) {
    all_files$repo_url <- character(0)
    all_files$file_name <- character(0)
    all_files$file_url <- character(0)
    all_files$file_location <- character(0)
    all_files$file_size <- numeric(0)
    all_files$file_type <- character(0)
    is_readme <- logical(0)
  } else {
    is_readme <- grepl(
      "readme|read[_ ]me",
      all_files$file_name,
      ignore.case = TRUE
    )
    all_files$file_type[is_readme] <- "readme"
  }

  repos <- dplyr::full_join(repos, all_files, by = "repo_url") |>
    dplyr::summarise(
      files_n = sum(!is.na(file_name)),
      files_data = sum(file_type %in% "data"),
      files_code = sum(file_type %in% "code"),
      files_readme = sum(file_type %in% "readme"),
      files_zip = sum(file_type %in% "archive"),
      .by = c(id, repo_url, repo_type, repo_error)
    )


  summary_files <- sprintf(
    "We found %d file%s in %d %s.",
    sum(repos$files_n),
    plural(sum(repos$files_n)),
    nrow(repos),
    plural(nrow(repos), "repository", "repositories")
  )

  ## empty repos ----
  repo_no_files <- sum(repos$files_n == 0)
  summary_repo <- NULL
  report_repo <- NULL

  if (repo_no_files > 0) {
    summary_repo <- sprintf(
      "We found %d empty %s.",
      repo_no_files,
      plural(repo_no_files, "repository", "repositories")
    )

    report_repo <- c(
      "Double-check permissions on repositories with no detectable files."
    )
  }

  ## missing READMEs ----
  readme_n <- sum(is_readme)
  repo_no_readme <- sum(repos$files_readme == 0)
  summary_readme <- sprintf(
    "We found %d README file%s and %d %s without READMEs.",
    readme_n,
    plural(readme_n),
    repo_no_readme,
    plural(repo_no_readme, "repository", "repositories")
  )

  if (repo_no_readme > 0) {
    report_readme <- "README files are a way to document the contents and structure of a folder, helping users locate the information they need. You can use a README to document changes to a repository, and explain how files are named. Please consider adding a README to each repository."
  } else {
    report_readme <- "README files were found in all repositories."
  }

  ## zip files ----
  zip_n <- sum(repos$files_zip)
  if (zip_n > 0) {
    zip_files <- all_files$file_name[all_files$file_type == "archive"]
    report_zip <- sprintf(
      "The following files are archives: %s. We did not examine their content. Consider uploading these individually to improve discoverability and re-use.",
      paste(zip_files, collapse = ", ")
    )
    summary_zip <- sprintf(
      "We found %d archive file%s.",
      zip_n,
      plural(zip_n)
    )
  } else {
    summary_zip <- NULL
    report_zip <- NULL
  }

  report_tbl <- all_files |>
    dplyr::mutate(file = link(file_url, file_name)) |>
    dplyr::select(Repository = repo_url,
                  File = file,
                  Size = file_size,
                  Type = file_type)

  # human-readable sizes
  report_tbl$Size <- sapply(
    report_tbl$Size,
    utils:::format.object_size,
    units = "auto",
    standard = "SI",
    digits = 1
  )

  # set up summary table of repositories
  repo_tbl <- repos
  repo_tbl$id <- NULL
  repo_tbl$repo_url <- link(repo_tbl$repo_url)
  names(repo_tbl) <- c("Repository", "Platform", "Error",
                       "All Files", "Data Files",
                       "Code Files", "READMEs",
                       "Archives")
  if (all(is.na(repo_tbl$Error))) {
    repo_tbl$Error <- NULL
  }

  report <- c(
    report_repo,
    scroll_table(repo_tbl, maxrows = 10),
    scroll_table(report_tbl, maxrows = 10),
    report_readme,
    report_zip
  )

  # traffic_light ----
  if (zip_n == 0 && repo_no_readme == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # summary_table ----
  summary_table <- repos |>
    dplyr::summarise(
      repo_n = dplyr::n(),
      dplyr::across(files_n:files_zip, sum),
      .by = c(id)
    )

  # summary_text ----
  summary_text <- c(
    summary_repo,
    summary_files,
    summary_readme,
    summary_zip
  ) |>
    paste("\n- ", x = _, collapse = "")

  # return a list ----
  list(
    table = all_files,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
