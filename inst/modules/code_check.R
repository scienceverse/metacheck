#' Code Check
#'
#' @description
#' This module retrieves information from repositories (OSF and GitHub) about code files (R, SAS, SPSS, Stata), zip files, and readme.
#'
#' @details
#' The Code Check module lists files on the OSF and GitHub based on links in the manuscript, and retrieves R, Rmd, Qmd, SAS, SPSS, and Stata files. The module then uses regular expressions to check the code. The regular expression search will detect the number of comments, the lines at which libraries/imports are loaded, attempts to detect absolute paths to files, and lists files that are loaded, and checks if these files are in the repository. It will also check for a readme file in the repository, and will warn it can’t examine the contents of zip files. The module will return suggestions to improve the code if there are no comments, if libraries/imports are loaded in lines further than 4 lines apart, if files that are loaded are not in the repository, and if hardcoded file paths are found.
#'
#' The regular expressions can miss information in code files, or falsely detect parts of the code as a fixed file path. Libraries/imports might be loaded in one block, even if there are more than 3 intermittent lines. The package was validated internally on papers published in Psychological Science. There might be valid reasons why some loaded files can’t be shared, but the module can’t evaluate these reasons, and always gives a warning.
#'
#' If you want to extend the package to be able to download files from additional data repositories, or perform additional checks on code files, or make the checks work on other types of code files, reach out to the Metacheck development team.
#'
#' @keywords results
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @import dplyr
#' @import httr
#' @import jsonlite
#' @param paper a paper object or paperlist object
#' @returns a list
code_check <- function(paper) {
  # example with osf Rmd files and github files: paper <- psychsci[[203]]
  # example with missing data files: paper <- psychsci[[221]]
  # Many R files, some with library in different places. paper <- psychsci[[225]]
  # Best example, with many issues, for paper: paper <- psychsci[[233]]
  # ResearchBox and GitHub example (in full xml): paper <- xml[["09567976251333666"]]

  # get repository links ----

  ## OSF ----
  osf_links_found <- osf_links(paper)
  osf_urls <- unique(osf_links_found$text)

  ## GitHub ----
  # Regex pattern for GitHub URLs (including subpaths)
  github_links_found <- github_links(paper)
  github_urls <- unique(github_links_found$text)

  ## ResearchBox ----
  rb_links_found <- rbox_links(paper)
  rb_urls <- unique(rb_links_found$text)

  ## no links found ----
  if (length(osf_urls) == 0 &&
      length(github_urls) == 0 &&
      length(rb_urls) == 0) {

    summary_text <- "We found no links to the Open Science Framework, Github, or ResearchBox."

    info <- list(
      traffic_light = "na",
      summary_text = summary_text,
      summary_table = data.frame(
        id = paper$id,
        hardcoded_folders = NA,
        loaded_files_missing = NA,
        minimum_comments = NA,
        readme_missing = NA,
        zip_files_present = NA
      )
    )

    return(info)
  }

  # get files ----

  ## OSF ----
  osf_files_df <- data.frame(name = character(0))
  if (length(osf_urls) > 0) {
    osf_info <- suppressWarnings(osf_retrieve(osf_urls, recursive = TRUE))

    # "kind" only in table if there are files
    if ("kind" %in% names(osf_info)) {
      osf_file_list <- osf_info |>
        dplyr::filter(kind == "file", !isFALSE(public)) |>
        dplyr::mutate(source = paste0("osf.io/", project))

      osf_files_df <- data.frame(
        source = osf_file_list$source,
        name = osf_file_list$name,
        download_url = osf_file_list$download_url,
        file_location = rep(NA_character_, nrow(osf_file_list))
      )
    }
  }

  ## GitHub ----
  github_files_df <- data.frame(name = character(0))
  if (length(github_urls) > 0) {
    github_file_list <- github_files(github_urls, recursive = TRUE) |>
      dplyr::filter(type != "dir")

    github_files_df <- data.frame(
      source = github_file_list$repo,
      name = github_file_list$name,
      download_url = github_file_list$download_url,
      file_location = rep(NA_character_, nrow(github_file_list))
    )
  }

  ## ResearchBox ----
  rb_files_df <- data.frame(name = character(0))
  if (length(rb_urls) > 0) {
    rb_file_list <- rbox_file_download(rb_urls)
    rb_files_df <- data.frame(
      source = rb_file_list$rb_url,
      name = rb_file_list$name,
      download_url = rb_file_list$rb_url,
      file_location = rb_file_list$file_location
    )
  }

  all_repos <- c(osf_urls, github_urls, rb_urls)
  all_files <- dplyr::bind_rows(osf_files_df, github_files_df, rb_files_df)

  ## find code files ----
  code_ext <- grepl("\\.(r|rmd|qmd|sas|sps|do|ado)$", all_files$name, ignore.case = TRUE)
  code_files <- all_files[code_ext, , drop = FALSE]
  zip_ext <- grepl("\\.zip$", all_files$name, ignore.case = TRUE)
  zip_files <- all_files[zip_ext, , drop = FALSE]
  readme_files <- grepl("readme|read[_ ]me", all_files$name, ignore.case = TRUE)
  readme_files <- all_files[readme_files, ]
  summary_code <-  sprintf(
    "We found %d R, SAS, SPSS, or Stata code file%s in the %d searched %s.",
    length(code_files),
    plural(length(code_files)),
    length(all_repos),
    plural(length(all_repos), "repository", "repositories")
  )

  ## missing READMEs ----
  readme_issue <- unique(all_files$source) |>
    setdiff(readme_files$source)

  summary_readme <- sprintf("We found %d README file%s and %d source%s without READMEs%s%s.",
                            nrow(readme_files),
                            plural(nrow(readme_files)),
                            length(readme_issue),
                            plural(length(readme_issue)),
                            ifelse(length(readme_issue), ": ", ""),
                            paste(readme_issue, collapse = ", "))

  if (is.null(readme_issue)) {
    report_readme <- NULL
  } else if (length(readme_issue) > 0) {
    report_readme <- paste(summary_readme, "\n\nREADME files are a way to document the contents and structure of a folder, helping users locate the information they need. You can use a README to document changes to a repository, and explain how files are named. Please consider adding a README to each repository.")
    readme_missing <- 1
  } else {
    report_readme <- "README files were found in all repositories."
    readme_missing <- 0
  }

  ## zip files ----
  zip_issue <- zip_files$name
  if (length(zip_issue) > 0) {
    report_zip <- sprintf(
      "### ZIP Files \n\nZIP files were present in the repository: %s. We can't examine their content. If the zip file contains data and code files, consider uploading these individually to improve discoverability and re-use.",
      paste(zip_files$name, collapse = ", ")
    )
    summary_zip <- sprintf("We found %d ZIP file%s.",
                           length(zip_issue),
                           plural(length(zip_issue))
    )
  } else {
    summary_zip <- NULL
    report_zip <- NULL
  }


  # no relevant code files found ----
  if (nrow(code_files) == 0) {
    if (nrow(all_files)) {
      # no files at all
      tl <- "yellow"
      summary_text <- sprintf(
        "We found no files in the following repositor%s: %s",
        plural(length(all_repos), "y", "ies"),
        paste(all_repos, collapse = ", ")
      )
      readme_missing <- length(missing_readmes)
      zip_files_present <- nrow(zip_files)
    } else {
      tl <- "na"
      summary_text <- c(summary_code, summary_readme, summary_zip) |>
        paste("    - ", x = _, collapse = "\n") |>
        paste0("\n", x = _)
      readme_missing <- NA
      zip_files_present <- NA
    }


    info <- list(
      traffic_light = tl,
      summary_text = summary_text,
      summary_table = data.frame(
        id = paper$id,
        hardcoded_folders = NA,
        loaded_files_missing = NA,
        minimum_comments = NA,
        readme_missing = readme_missing,
        zip_files_present = zip_files_present
      ),
      report = c(report_readme, report_zip)
    )

    return(info)
  }

  # Check code ----

  # Create list of all file names in repository
  files_in_repository <- basename(all_files$name)

  # Shared absolute path pattern and quoted filename pattern
  absolute_path_pattern <- '(?<![A-Za-z0-9_])(["\'])(?:(?!https?://)(?:[A-Za-z]:[\\\\/]|(?:\\\\\\\\|//)[^\\\\/]+[\\\\/]|~[/\\\\]|/(?:Users|home|var|etc|opt|srv|mnt|Volumes|Library|Applications|gpfs|data|tmp|media|root)\\b)[^"\']*)\\1'

  # --- Process each code file ---
  collected <- lapply(seq_len(nrow(code_files)), \(i) {
    tryCatch({
      collector <- list()
      # access via URL if not local
      if (!is.na(code_files$file_location[i])) {
        con <- file(code_files$file_location[i], "r")
      } else {
        con <- url(code_files$download_url[i])
      }

      # read in files
      file_lines <- readLines(con, warn = FALSE)
      close(con)


      # Convert to UTF-8, replacing invalid characters
      file_lines <- iconv(file_lines, from = "UTF-8", to = "UTF-8", sub = "byte")
      # Remove any NA entries resulting from failed conversions
      file_lines <- file_lines[!is.na(file_lines)]

      # detect language (function below)
      lang <- detect_lang(code_files$name[i])
      collector$language <- lang

      # Create a comment-less version, per language
      file_nc <- remove_comments(file_lines, lang)

      # get absolute paths based on grepl (on non-comment lines)
      absolute_paths <- grep(absolute_path_pattern, file_nc, value = TRUE, perl = TRUE)
      if (length(absolute_paths) > 0) {
        collector$hardcoded_folders <- 1
        collector$absolute_paths <- paste(absolute_paths, collapse = ", ")
      } else {
        collector$hardcoded_folders <- 0
        collector$absolute_paths <- NA
      }

      # Find lines where libraries/imports/includes are loaded  (function below)
      library_lines <- get_library_lines(file_nc, lang)

      # If the import statements are at most 3 lines apart, we consider it OK
      if (length(library_lines) > 1 && !all(diff(library_lines) < 4)) {
        collector$library_on_top <- 1
        collector$library_lines <- paste(library_lines, collapse = ", ")
      } else if (length(library_lines) > 0) {
        collector$library_on_top <- 0
        collector$library_lines <- NA
      } else {
        collector$library_on_top <- NA
      }

      # Get statistics about lines of code and comments  (function below)
      line_stats <- get_line_stats(file_lines, lang)
      collector$comment_lines <- line_stats$comment_lines
      collector$code_lines <- line_stats$code_lines
      collector$percentage_comment <- line_stats$percent_comments

      # missing loaded files  (function below)
      missing_files <- get_missing_files(file_nc, lang, files_in_repository)
      collector$loaded_files_missing <- length(missing_files) > 0
      collector$loaded_files_missing_names <- paste(missing_files, collapse = ", ")

      return(collector)
    }, error = \(e) {
      collector <- list(error = e$message)
      return(collector)
    })
  }) # end of loop over code files

  code_check <- do.call(dplyr::bind_rows, collected)
  code_files <- dplyr::bind_cols(code_files, code_check)

  # Reporting ----

  ## Libraries/imports grouping issues ----
  library_issue <- code_files$name[code_files$library_on_top %in% 1]
  if (length(library_issue) == 0) {
    report_library <- "Best programming practice is to load all required libraries/imports in one block near the top of the code. In all code files, libraries/imports were loaded in one block."
    summary_library <- "All libraries/imports were loaded in one block."
    report_table_library <- NULL
  } else {
    report_library <- sprintf(
      "Best programming practice is to load all required libraries/imports in one block near the top of the code. In %d code files, libraries/imports were at multiple places (i.e., with more than 3 lines in between). This was true in the following files, where libraries/imports were loaded on the following lines:\n\n",
      length(library_issue)
    )
    summary_library <- "Libraries/imports were loaded in multiple places."
    issues_library <- paste(sprintf("**%s**", library_issue), collapse = "\n\n")
    lines_library <- paste(sprintf("**%s**", code_files$library_lines[which(code_files$library_on_top == 1)]), collapse = "\n\n")
    report_table_library <- code_files_subset <- code_files[!is.na(code_files$library_lines), c("name", "language", "library_lines"), drop = FALSE]
    colnames(report_table_library) <- c("Code File name", "Language", "Lines at which libraries/imports are loaded")
  }

  ## Hardcoded paths ----
  hardcoded_issues <- code_files$name[code_files$hardcoded_folders %in% 1]
  if (length(hardcoded_issues) == 0) {
    report_hardcoded <- "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names do not exist on other computers. No hardcoded file paths were found in any of the code files."
    summary_hardcoded <- "No hardcoded file paths were found."
    report_table_hardcoded <- NULL
  } else {
    report_hardcoded <- sprintf(
      "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names do not exist on other computers. The following hardcoded file paths were found in %d code file(s).",
      length(hardcoded_issues)
    )
    summary_hardcoded <- "Hardcoded file paths were found."
    report_table_hardcoded <- code_files_subset <- code_files[!is.na(code_files$absolute_paths), c("name", "language", "absolute_paths"), drop = FALSE]
    colnames(report_table_hardcoded) <- c("Code File name", "Language", "Absolute paths found")
  }

  ## Comments ----
  comment_issue <- min(code_files$percentage_comment, na.rm = TRUE)
  if (is.finite(comment_issue) && comment_issue > 0) {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code). All your code files had comments."
    summary_comments <- "All your code files had comments."
    report_table_comments <- NULL
  } else {
    report_comments <- sprintf("Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code). The following %d files had no comments:",
                               sum(code_files$percentage_comment == 0, na.rm = TRUE))
    summary_comments <- "Some code files had no comments."
    report_table_comments <- code_files_subset <- code_files[!is.na(code_files$percentage_comment), c("name", "language", "percentage_comment"), drop = FALSE]
    colnames(report_table_comments) <- c("Code File name", "Language", "Percentage of lines that are comments")
  }

  ## Missing files ----
  missingfiles_issue <- code_files$name[code_files$loaded_files_missing %in% TRUE]
  if (length(missingfiles_issue) == 0) {
    summary_missingfiles <- "All files loaded in the code were present in the repository."
    report_missingfiles <- summary_missingfiles
    report_table_files_missing <- NULL
  } else {
    summary_missingfiles <- "Some files loaded in the code were missing in the repository."

    report_missingfiles <- sprintf(
      "The scripts load files, but %d scripts loaded files that could not be automatically identified in the repository. Check if the following files are made available, so that others can reproduce your code, or that the files are missing:",
      length(missingfiles_issue)
    )

    rows <- code_files$loaded_files_missing_names != ""
    cols <- c("name", "language", "loaded_files_missing_names")
    report_table_files_missing <- code_files[rows, cols]
    colnames(report_table_files_missing) <- c("Code File name", "Language", "Files loaded in code but missing in repository")
  }


  ## set up table of code file links ----
  cols <- c("name", "download_url") |> intersect(names(all_files))
  rows <- all_files$download_url %in% code_files$download_url
  report_table <- unique(all_files[rows, cols])
  report_table$download_url <- sprintf("<a href='%s'>download</a>", report_table$download_url)
  names(report_table) <- c("Code File Name", "Download")

  report <- c(
    "Below, we describe some best coding practices and give the results of automatic evaluation of these practices in the code files below. This check may miss things or produce false positives if your scripts are less typical.",
    scroll_table(report_table, maxrows = 5),
    "#### Missing Files",
    report_missingfiles,
    scroll_table(report_table_files_missing, maxrows = 5),
    "#### Hardcoded Paths",
    report_hardcoded,
    scroll_table(report_table_hardcoded, maxrows = 5),
    "#### Libraries / Imports",
    report_library,
    scroll_table(report_table_library, maxrows = 5),
    "#### Code Comments",
    report_comments,
    scroll_table(report_table_comments, maxrows = 5),
    "#### README",
    report_readme,
    report_zip
  )

  # traffic_light ----
  # green only if no issues across all code files
  if (length(missingfiles_issue) == 0 &&
      is.finite(comment_issue) && comment_issue > 0 &&
      length(hardcoded_issues) == 0 &&
      length(library_issue) == 0 &&
      length(zip_issue) == 0 &&
      length(readme_issue) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # Aggregate by project and count number of 1s
  summary_table <- data.frame(
    id = paper$id,
    hardcoded_folders = length(hardcoded_issues),
    loaded_files_missing = sum(code_files$loaded_files_missing, na.rm = TRUE),
    minimum_comments = min(code_files$percentage_comment, na.rm = TRUE),
    readme_missing = readme_missing,
    zip_files_present = length(zip_issue)
  )

  # summary_text ----
  summary_text <- c(
    summary_code,
    summary_missingfiles,
    summary_hardcoded,
    summary_library,
    summary_comments,
    summary_readme,
    summary_zip
  ) |>
    paste("\n    - ", x = _, collapse = "")

  # table ----
  table <- code_files
  table$file_location <- NULL

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

# HELPER FUNCTIONS ----

# Helper: detect language by extension
detect_lang <- function(fname) {
  lname <- tolower(fname)
  if (grepl("\\.(r|rmd|qmd)$", lname)) return("R")
  if (grepl("\\.sas$", lname)) return("SAS")
  if (grepl("\\.sps$", lname)) return("SPSS")
  if (grepl("\\.(do|ado)$", lname)) return("Stata")
  return(NA_character_)
}

remove_comments <- function(file_lines, lang) {
  if (lang == "R") {
    file_nc <- grep('^(\\s*$|\\s*#|```\\s*\\{r)', file_lines, invert = TRUE, value = TRUE)
    file_nc <- grep('knitr::', file_nc, invert = TRUE, value = TRUE)
  } else if (lang == "SAS") {
    in_block <- FALSE
    tmp <- character(0)
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block   <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*.*;\\s*$", L)
      if (!in_block && !line_comment) tmp <- c(tmp, L)
      if (in_block && ends_block) in_block <- FALSE
    }
    file_nc <- tmp
  } else if (lang == "SPSS") {
    in_block <- FALSE
    tmp <- character(0)
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block   <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      if (!in_block && !line_comment) tmp <- c(tmp, L)
      if (in_block && ends_block) in_block <- FALSE
    }
    file_nc <- tmp
  } else if (lang == "Stata") {
    in_block <- FALSE
    tmp <- character(0)
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block   <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      if (!in_block && !line_comment) {
        if (grepl("//", L)) L <- sub("//.*$", "", L) # strip end-of-line comments
        tmp <- c(tmp, L)
      }
      if (in_block && ends_block) in_block <- FALSE
    }
    file_nc <- tmp
  } else {
    file_nc <- file_lines
  }

  return(file_nc)
}

get_line_stats <- function(file_lines, lang) {
  total_lines <- length(file_lines)

  if (lang == "R") {
    comment_lines <- sum(grepl("^\\s*#", file_lines))
    code_lines <- sum(file_lines != "" & !grepl("^\\s*#", file_lines))
  } else if (lang == "SAS") {
    # crude but effective: mark SAS line comments and block comments
    in_block <- FALSE
    is_comment_line <- logical(length(file_lines))
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block   <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*.*;\\s*$", L)
      is_comment_line[ln] <- in_block || line_comment
      if (in_block && ends_block) in_block <- FALSE
    }
    comment_lines <- sum(is_comment_line)
    code_lines <- sum(file_lines != "" & !is_comment_line)
  } else if (lang == "SPSS") {
    in_block <- FALSE
    is_comment_line <- logical(length(file_lines))
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block   <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      is_comment_line[ln] <- in_block || line_comment
      if (in_block && ends_block) in_block <- FALSE
    }
    comment_lines <- sum(is_comment_line)
    code_lines <- sum(file_lines != "" & !is_comment_line)
  } else if (lang == "Stata") {
    in_block <- FALSE
    is_comment_line <- logical(length(file_lines))
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block   <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      # treat lines containing only comments as comments (// may be EOL)
      eol_only <- grepl("^\\s*//", L)
      is_comment_line[ln] <- in_block || line_comment || eol_only
      if (in_block && ends_block) in_block <- FALSE
    }
    comment_lines <- sum(is_comment_line)
    code_lines <- sum(file_lines != "" & !is_comment_line)
  } else {
    comment_lines <- sum(file_lines == "") # fallback; shouldn't happen
    code_lines <- sum(file_lines != "")
  }

  percent_comments <- if (total_lines > 0) (comment_lines / total_lines) else NA

  return(list(
    total_lines = total_lines,
    comment_lines = comment_lines,
    code_lines = code_lines,
    percent_comments = percent_comments
  ))
}

get_library_lines <- function(file_nc, lang) {
  # Language-specific regexes for imports and data loads
  lang_import_regex <- list(
    R     = "^[^#]*\\b(library|require)\\s*\\(",
    SAS   = "\\b(%include|libname|filename|options)\\b",
    SPSS  = "\\b(INSERT|BEGIN\\s+PROGRAM|SET)\\b",
    Stata = "\\b(do|run|cd|adopath|net\\s+install|ssc\\s+install)\\b"
  )

  import_regex <- lang_import_regex[[lang]]
  if (is.null(import_regex)) return(integer(0))
  lines <- grep(import_regex, file_nc, perl = TRUE)

  return(lines)
}

get_missing_files <- function(file_nc, lang, files_in_repository) {
  # Examine files loaded, but missing in repo
  lang_load_regex <- list(
    R     = c(
      "read\\.(csv2?|table|delim2?)",
      "read\\.xlsx",
      "read\\.dta",
      "read_(csv2?|tsv|delim|rds|lines)",
      "read_(xlsx?|excel)",
      "read_(dta|sav|sas)",
      "read_(feather|parquet|yaml|xml|ods)",
      "fread",
      "readRDS",
      "load",
      "readLines",
      "fromJSON",
      "readtext",
      "source"
    ) |>
      paste(collapse = "|") |>
      paste0("\\b(", x = _, ")\\s*\\("),
    SAS   = "\\b(proc\\s+import|infile|datafile\\s*=|libname)\\b",
    SPSS  = "\\b(GET\\s+FILE|GET\\s+DATA|DATA\\s+LIST|SAVE)\\b",
    Stata = "\\b(use|import\\s+delimited|insheet|merge|append)\\b"
  )
  grepl_load <- lang_load_regex[[lang]]
  load_lines <- if (!is.null(grepl_load)) {
    grep(grepl_load, file_nc, value = TRUE, perl = TRUE)
  } else {
    character(0)
  }

  # Quoted filenames
  quoted_filename_pattern <- "(['\"])(?!\\.\\1)[^'\"/\\\\]+\\.[A-Za-z0-9]{1,8}(?:\\.[A-Za-z0-9]{1,8})*\\1"

  loaded_file <- unlist(regmatches(
    load_lines,
    gregexpr(quoted_filename_pattern, load_lines, perl = TRUE)
  ))
  loaded_file <- gsub("^['\"]|['\"]$", "", loaded_file)

  # Unquoted captures (language-specific)
  lang_unquoted_captures <- list(
    R = list(), # quoted captures suffice for your R grepl_load
    SAS = list(
      list(regex = "infile\\s+([^\\s;]+)", group = 1),
      list(regex = "datafile\\s*=\\s*([^\\s;]+)", group = 1)
    ),
    SPSS = list(
      list(regex = "GET\\s+DATA.*?/FILE\\s*=\\s*([^\\s]+)", group = 1)
    ),
    Stata = list(
      list(regex = "^\\s*use\\s+([^,\\s]+)", group = 1),
      list(regex = "import\\s+delimited\\s+using\\s+([^,\\s]+)", group = 1),
      list(regex = "insheet\\s+using\\s+([^,\\s]+)", group = 1),
      list(regex = "merge\\b.*?using\\s+([^,\\s]+)", group = 1),
      list(regex = "append\\b.*?using\\s+([^,\\s]+)", group = 1)
    )
  )

  extra <- character(0)
  caps <- lang_unquoted_captures[[lang]]
  if (length(caps) > 0) {
    for (cap in caps) {
      m <- regexec(cap$regex, load_lines, perl = TRUE)
      reg <- regmatches(load_lines, m)
      if (length(reg) > 0) {
        vals <- vapply(reg, function(x) if (length(x) >= cap$group + 1) x[cap$group + 1] else NA_character_, character(1))
        extra <- c(extra, vals)
      }
    }
  }
  extra <- extra[!is.na(extra)]
  loaded_file <- basename(unique(c(loaded_file, extra)))

  missing_files <- loaded_file[!tolower(loaded_file) %in% tolower(files_in_repository)]

  return(missing_files)
}
