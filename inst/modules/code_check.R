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
  # osf_links

  osf_links_found <- osf_links(paper)

  if (nrow(osf_links_found) > 0) {
    osf_info_retrieved <- suppressWarnings(osf_retrieve(osf_links_found, recursive = TRUE, find_project = TRUE))
  }

  # Regex pattern for GitHub URLs (including subpaths)
  github_regex <- "https://github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"
  github_links_found <- search_text(paper, github_regex, return = "match")
  # remove duplicates
  github_links_found <- github_links_found[!duplicated(github_links_found$text), ]
  # Below needs to be made to work for multiple URLS! And I need to remove duplicates
  if (nrow(github_links_found) > 0) {
    github_file_list <- github_links_found$text %>%
      map(~ github_files(.x, recursive = TRUE)) %>%
      bind_rows()
  }

  # ResearchBox
  rb_url <- rbox_links(paper)
  # remove duplicates
  rb_url <- rb_url[!duplicated(rb_url$text), ]

  # no (public) links found
  if ((nrow(osf_links_found) == 0 ||
       isTRUE(all(osf_info_retrieved$public == FALSE, na.rm = FALSE))) &&
      nrow(github_links_found) == 0 &&
      nrow(rb_url) == 0) {
    return(list(
      traffic_light = "na",
      summary_text = "No links to the Open Science Framework or Github were found.",
      summary_table = data.frame(
        id = paper$id,
        hardcoded_folders = NA,
        loaded_files_missing = NA,
        minimum_comments = NA,
        readme_missing = NA,
        zip_files_present = NA
      ),
      report = "No links to the Open Science Framework or Github were found."
    ))
  }

  ## get code files ----
  #TODO: check if find_project should be set -- it takes time and doesn't add anything we're using
  osf_files_df <- osf_info_retrieved

  # Need to set text to character is emtpy to enable merge
  if (is.logical(osf_files_df$text)) {
    osf_files_df$text <- as.character(osf_files_df$text)
  }
  if (!"name" %in% colnames(osf_files_df)) osf_files_df$name <- NA_character_
  if (!"download_url" %in% colnames(osf_files_df)) osf_files_df$download_url <- NA_character_

  if (nrow(github_links_found) > 0) {
    github_files_df <- github_links_found$text %>%
      map(~ github_files(.x, recursive = TRUE)) %>%
      bind_rows()
  } else {
    # empty tibble with correct column types
    github_files_df <- dplyr::tibble(name = character(), download_url = character())
  }

  if (!"name" %in% colnames(github_files_df)) github_files_df$name <- NA_character_
  if (!"download_url" %in% colnames(github_files_df)) github_files_df$download_url <- NA_character_

  if (nrow(rb_url) > 0) {
    rb_file_list <- rb_url$text %>%
      map(~ rbox_retrieve(.x)) %>%
      bind_rows()
    # Get a list of all files on researchbox, for example to check if we want to download them
    rb_files_df <- unlist(rb_file_list$files)
    # download the files, works for multiple research box links
    rb_download <- purrr::map_dfr(rb_url$text, ~ rbox_file_download(.x))
  } else {
    # empty tibble with correct column types
    rb_download <- dplyr::tibble(name = character(), file_location = character())
  }

  all_files <- dplyr::bind_rows(osf_files_df, github_files_df, rb_download)

  code_ext <- grepl("\\.(r|rmd|qmd|sas|sps|do|ado)$", all_files$name, ignore.case = TRUE)
  code_files <- all_files[code_ext, c("name", "download_url", "file_location")]
  zip_ext <- grepl("\\.zip$", all_files$name, ignore.case = TRUE)
  zip_files <- all_files[zip_ext, c("name", "download_url")]
  readme_files <- grepl("readme|read[_ ]me", all_files$name, ignore.case = TRUE)
  readme_files <- all_files[readme_files, c("name", "download_url")]

  if (nrow(code_files) == 0) {
    return(list(
      traffic_light = "na",
      summary_text  = "No R, SAS, SPSS, or Stata code files were found in the repository.",
      summary_table = data.frame(
        id = paper$id,
        hardcoded_folders = NA,
        loaded_files_missing = NA,
        minimum_comments = NA,
        readme_missing = NA,
        zip_files_present = NA
      ),
      report = "No links to the Open Science Framework or Github were found."
    ))
  }

  # Add columns for aspects we will check (mimic your existing structure)
  code_files$comment_lines <- NA
  code_files$code_lines <- NA
  code_files$percentage_comment <- NA
  code_files$library_on_top <- NA
  code_files$hardcoded_folders <- NA
  code_files$loaded_files_missing <- NA
  code_files$library_lines <- NA
  code_files$absolute_paths <- NA
  code_files$loaded_files_missing_names <- NA
  code_files$language <- NA_character_

  # Create list of all file names in repository
  files_in_repository <- basename(all_files$name)

  # Shared absolute path pattern and quoted filename pattern
  absolute_path_pattern <- '(?<![A-Za-z0-9_])(["\'])(?:(?!https?://)(?:[A-Za-z]:[\\\\/]|(?:\\\\\\\\|//)[^\\\\/]+[\\\\/]|~[/\\\\]|/(?:Users|home|var|etc|opt|srv|mnt|Volumes|Library|Applications|gpfs|data|tmp|media|root)\\b)[^"\']*)\\1'
  quoted_filename_pattern <- "(['\"])(?!\\.\\1)[^'\"/\\\\]+\\.[A-Za-z0-9]{1,8}(?:\\.[A-Za-z0-9]{1,8})*\\1"

  # Helper: detect language by extension
  detect_lang <- function(fname) {
    lname <- tolower(fname)
    if (grepl("\\.(r|rmd|qmd)$", lname)) return("R")
    if (grepl("\\.sas$", lname)) return("SAS")
    if (grepl("\\.sps$", lname)) return("SPSS")
    if (grepl("\\.(do|ado)$", lname)) return("Stata")
    return(NA_character_)
  }

  # Language-specific regexes for imports and data loads
  lang_import_regex <- list(
    R     = "^[^#]*\\b(library|require)\\s*\\(",
    SAS   = "\\b(%include|libname|filename|options)\\b",
    SPSS  = "\\b(INSERT|BEGIN\\s+PROGRAM|SET)\\b",
    Stata = "\\b(do\\b|run\\b|cd\\b|adopath\\b|net\\s+install\\b|ssc\\s+install\\b)\\b"
  )
  lang_load_regex <- list(
    R     = "\\b((?:[[:alnum:]_.]+::)?read\\.(?:csv2?|table|delim2?)|(?:[[:alnum:]_.]+::)?readRDS|(?:[[:alnum:]_.]+::)?load|(?:[[:alnum:]_.]+::)?read_(?:csv2?|tsv|delim|rds|lines)|(?:[[:alnum:]_.]+::)?fread|(?:[[:alnum:]_.]+::)?read_(?:xlsx?|excel)|(?:[[:alnum:]_.]+::)?read\\.xlsx|(?:[[:alnum:]_.]+::)?read_(?:dta|sav|sas)|(?:[[:alnum:]_.]+::)?read\\.dta|(?:[[:alnum:]_.]+::)?read_feather|(?:[[:alnum:]_.]+::)?read_parquet|(?:[[:alnum:]_.]+::)?fromJSON|(?:[[:alnum:]_.]+::)?read_yaml|(?:[[:alnum:]_.]+::)?read_xml|(?:[[:alnum:]_.]+::)?read_ods|(?:[[:alnum:]_.]+::)?readtext)\\s*\\(|\\bsource\\s*\\(",
    SAS   = "\\b(proc\\s+import|infile\\b|datafile\\s*=|libname\\b)\\b",
    SPSS  = "\\b(GET\\s+FILE|GET\\s+DATA|DATA\\s+LIST|SAVE)\\b",
    Stata = "\\b(use\\b|import\\s+delimited\\b|insheet\\b|merge\\b|append\\b)\\b"
  )

  # Additional language-specific unquoted filename captures
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

  # --- Process each code file ---
  for (i in 1:nrow(code_files)) {
    if(!is.na(code_files$download_url[i])) {
      download_link <- code_files$download_url[i]
      con <- url(download_link)
      file_lines <- suppressWarnings(readLines(con)) # some files might have incomplete final line, but we ignore this
      close(con)
    } else {
      local_path <- code_files$file_location[i]
      con <- file(local_path, "r")
      file_lines <- suppressWarnings(readLines(con))
      close(con)
    }

    # Convert to UTF-8, replacing invalid characters
    file_lines <- iconv(file_lines, from = "UTF-8", to = "UTF-8", sub = "byte")
    # Remove any NA entries resulting from failed conversions
    file_lines <- file_lines[!is.na(file_lines)]

    # Language
    lang <- detect_lang(code_files$name[i])
    code_files$language[i] <- lang

    # Create a comment-less version, per language
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

    # get absolute paths based on grepl (on non-comment lines)
    absolute_paths <- grep(absolute_path_pattern, file_nc, value = TRUE, perl = TRUE)
    if (length(absolute_paths) > 0) {
      code_files$hardcoded_folders[i] <- 1
      code_files$absolute_paths[i] <- paste(absolute_paths, collapse = ", ")
    } else {
      code_files$hardcoded_folders[i] <- 0
      code_files$absolute_paths[i] <- NA
    }

    # Find lines where libraries/imports/includes are loaded
    import_regex <- lang_import_regex[[lang]]
    if (is.null(import_regex)) import_regex <- ""
    library_lines <- if (nzchar(import_regex)) grep(import_regex, file_nc, perl = TRUE) else integer(0)

    # If the import statements are at most 3 lines apart, we consider it OK
    if (length(library_lines) > 1 && !all(diff(library_lines) < 4)) {
      code_files$library_on_top[i] <- 1
      code_files$library_lines[i] <- paste(library_lines, collapse = ", ")
    } else if (length(library_lines) > 0) {
      code_files$library_on_top[i] <- 0
      code_files$library_lines[i] <- NA
    } else {
      code_files$library_on_top[i] <- NA
    }

    # Get statistics about lines of code and comments.
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

    code_files$comment_lines[i] <- comment_lines
    code_files$code_lines[i] <- code_lines
    percent_comments <- if (total_lines > 0) (comment_lines / total_lines) else NA
    code_files$percentage_comment[i] <- percent_comments

    # Examine files loaded, but missing in repo
    grepl_load <- lang_load_regex[[lang]]
    if (is.null(grepl_load)) grepl_load <- ""
    load_lines <- if (nzchar(grepl_load)) grep(grepl_load, file_nc, value = TRUE, perl = TRUE) else character(0)

    # Quoted filenames
    loaded_file <- unlist(regmatches(
      load_lines,
      gregexpr(quoted_filename_pattern, load_lines, perl = TRUE)
    ))
    loaded_file <- gsub("^['\"]|['\"]$", "", loaded_file)

    # Unquoted captures (language-specific)
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

    code_files$loaded_files_missing[i] <- if (length(missing_files) == 0) NA else 1
    code_files$loaded_files_missing_names[i] <- if (length(missing_files) == 0) NA else paste(missing_files, collapse = ", ")
  } # end of loop over code files

  # --- Reporting ---

  # Libraries/imports grouping issues
  library_issue <- code_files$name[which(code_files$library_on_top == 1)]
  if (length(library_issue) == 0) {
    report_library <- "Best programming practice is to load all required libraries/imports in one block near the top of the code. In all code files, libraries/imports were loaded in one block."
    summary_library <- "All libraries/imports were loaded in one block."
    report_table_library <- NA
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

  # Hardcoded paths
  hardcoded_issues <- code_files$name[code_files$hardcoded_folders == 1]
  if (length(hardcoded_issues) == 0) {
    report_hardcoded <- "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names do not exist on other computers. No hardcoded file paths were found in any of the code files."
    summary_hardcoded <- "No hardcoded file paths were found."
    report_table_hardcoded <- NA
  } else {
    report_hardcoded <- sprintf(
      "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names do not exist on other computers. The following hardcoded file paths were found in %d code file(s).",
      length(hardcoded_issues)
    )
    summary_hardcoded <- "Hardcoded file paths were found."
    report_table_hardcoded <- code_files_subset <- code_files[!is.na(code_files$absolute_paths), c("name", "language", "absolute_paths"), drop = FALSE]
    colnames(report_table_hardcoded) <- c("Code File name", "Language", "Absolute paths found")
  }

  # Comments
  comment_issue <- min(code_files$percentage_comment, na.rm = TRUE)
  if (is.finite(comment_issue) && comment_issue > 0) {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code). All your code files had comments."
    summary_comments <- "All your code files had comments."
    report_table_comments <- NA
  } else {
    report_comments <- sprintf("Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code). The following %d files had no comments:",
                               sum(code_files$percentage_comment == 0, na.rm = TRUE))
    summary_comments <- "Some code files had no comments."
    report_table_comments <- code_files_subset <- code_files[!is.na(code_files$percentage_comment), c("name", "language", "percentage_comment"), drop = FALSE]
    colnames(report_table_comments) <- c("Code File name", "Language", "Percentage of lines that are comments")
  }

  # Missing files
  missingfiles_issue <- code_files$loaded_files_missing[!is.na(code_files$loaded_files_missing)]
  if (length(missingfiles_issue) == 0) {
    report_missingfiles <- "All files loaded in the code were present in the repository."
    summary_missingfiles <- "All files loaded in the code were present in the repository."
    report_table_files_missing <- NA
  } else {
    report_missingfiles <- sprintf(
      "The scripts load files, but %d scripts loaded files that could not be automatically identified in the repository. Check if the following files are made available, so that others can reproduce your code, or that the files are missing:",
      length(missingfiles_issue)
    )
    summary_missingfiles <- "Some files loaded in the code were missing in the repository."
    report_table_files_missing <- code_files_subset <- code_files[!is.na(code_files$loaded_files_missing_names), c("name", "language", "loaded_files_missing_names"), drop = FALSE]
    colnames(report_table_files_missing) <- c("Code File name", "Language", "Files loaded in code but missing in repository")
  }

  ## README
  readme_issue <- nrow(readme_files > 0)
  if (readme_issue > 0) {
    report_readme <- sprintf(
      "README files were present in the repository: %s",
      paste(readme_files$name, collapse = ", ")
    )
    summary_readme <- "A README file was present in the repository."
    readme_missing <- 0
  } else {
    report_readme <- "No README file was found. README files are a way to document the contents and structure of a folder, helping users locate the information they need. You can use a README to document changes to a repository, and explain how files are named. Please consider adding a README."
    summary_readme <- "No README file was found."
    readme_missing <- 1
  }

  ## ZIP files
  zip_issue <- nrow(zip_files > 0)
  if (zip_issue > 0) {
    report_zip <- sprintf(
      "### ZIP Files \n\nZIP files were present in the repository: %s. We can't examine their content. If the zip file contains data and code files, consider uploading these individually to improve discoverability and re-use.",
      paste(zip_files$name, collapse = ", ")
    )
    zip_files_present <- 1
    summary_zip <- "A zip file was present in the repository."
  } else {
    report_zip <- NULL
    summary_zip <- NULL
    zip_files_present <- 0
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

  # Traffic light: green only if no issues across all code files
  if (length(missingfiles_issue) == 0 &&
      is.finite(comment_issue) && comment_issue > 0 &&
      length(hardcoded_issues) == 0 &&
      length(library_issue) == 0) {
    tl <- "green"
  } else {
    tl <- "info"
  }

  # Aggregate by project and count number of 1s (keep your fields; now across all code files)
  summary_table <- data.frame(
    id = paper$id,
    hardcoded_folders = length(hardcoded_issues),
    loaded_files_missing = sum(code_files$loaded_files_missing == 1, na.rm = TRUE),
    minimum_comments = min(code_files$percentage_comment, na.rm = TRUE),
    readme_missing <- readme_missing,
    zip_files_present <- zip_files_present
  )

  # summary_text ----
  summary_text <- c(
    summary_missingfiles,
    summary_hardcoded,
    summary_library,
    summary_comments,
    summary_readme,
    summary_zip
  ) |>
    paste("\n    - ", x = _, collapse = "")

  # return a list ----
  list(
    table = code_files,                # now includes all languages + same columns
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}

