#' Code Check
#'
#' @description
#' Retrieve information r files.
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#'
#' @import dplyr
#' @import httr
#' @import jsonlite
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report text
#'
#' @examples
#' module_run(psychsci[[233]], "code_check")
code_check <- function(paper) {
  # example with osf Rmd files and github files: paper <- psychsci[[203]]
  # example with missing data files: paper <- psychsci[[221]]
  # Many R files, some with library in different places. paper <- psychsci[[225]]
  # Best example, with many issues, for paper: paper <- psychsci[[233]]
  osf_links_found <- osf_links(paper)
  if (nrow(osf_links_found) > 0) {
    osf_info_retrieved <- suppressWarnings(osf_retrieve(osf_links_found, recursive = TRUE, find_project = TRUE))
  }
  # Regex pattern for GitHub URLs (including subpaths)
  github_regex <- "https://github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"
  github_found <- search_text(paper, github_regex, return = "match")
  if (nrow(github_found) > 0) {
    github_file_list <- github_files(github_found$text, recursive = TRUE)
  }
  if (nrow(osf_links_found) == 0 && nrow(github_found) == 0) {
    report <- "No links to the Open Science Framework or Github were found."
    return(list(
      traffic_light = "na",
      report = report
    ))
  }

  # Ensure both data frames have the same columns
  if (nrow(osf_links_found) > 0 && nrow(github_found) > 0) {
    common_cols <- union(names(osf_info_retrieved), names(github_file_list))
    # Add missing columns as NA
    for (col in setdiff(common_cols, names(osf_info_retrieved))) {
      osf_info_retrieved[[col]] <- NA
    }
    for (col in setdiff(common_cols, names(github_file_list))) {
      github_file_list[[col]] <- NA
    }
    # Bind rows
    all_files <- rbind(osf_info_retrieved, github_file_list)
  } else if (nrow(osf_links_found) > 0) {
    all_files <- osf_info_retrieved
  } else if (nrow(github_found) > 0) {
    all_files <- github_file_list
  }
  # Create dataframe with only r files
  r_files <- all_files[grepl("\\.(r|rmd|qmd)$", all_files$name, ignore.case = TRUE), ]

  if (nrow(r_files) == 0) {
    report <- "No R files were found in the repository."
    return(list(
      traffic_light = "na",
      report = report
    ))
  }

  # Add comments for aspects we will check
  r_files$comment_lines <- NA
  r_files$code_lines <- NA
  r_files$percentage_comment <- NA
  r_files$library_on_top <- NA
  r_files$hardcoded_folders <- NA
  r_files$loaded_files_missing <- NA
  r_files$library_lines <- NA
  r_files$absolute_paths <- NA
  r_files$loaded_files_missing_names <- NA

  # Create list of all file names in repository. Will compare loaded files against this list to see if files are loaded, but not shared
  files_in_repository <- basename(all_files$name)
  # Create variable to store missing files

  # Lines with data-loading calls
  grepl_load <- "\\b((?:[[:alnum:]_.]+::)?read\\.(?:csv2?|table|delim2?)|(?:[[:alnum:]_.]+::)?readRDS|(?:[[:alnum:]_.]+::)?load|(?:[[:alnum:]_.]+::)?read_(?:csv2?|tsv|delim|rds|lines)|(?:[[:alnum:]_.]+::)?fread|(?:[[:alnum:]_.]+::)?read_(?:xlsx?|excel)|(?:[[:alnum:]_.]+::)?read\\.xlsx|(?:[[:alnum:]_.]+::)?read_(?:dta|sav|sas)|(?:[[:alnum:]_.]+::)?read\\.dta|(?:[[:alnum:]_.]+::)?read_feather|(?:[[:alnum:]_.]+::)?read_parquet|(?:[[:alnum:]_.]+::)?fromJSON|(?:[[:alnum:]_.]+::)?read_yaml|(?:[[:alnum:]_.]+::)?read_xml|(?:[[:alnum:]_.]+::)?read_ods|(?:[[:alnum:]_.]+::)?readtext)\\s*\\(|\\bsource\\s*\\("

  for (i in 1:nrow(r_files)) {
    # Read in the R code
    download_link <- r_files$download_url[i]
    con <- url(download_link)
    rfile <- suppressWarnings(readLines(con)) # some files might have incomplete final line, but we ignore this
    close(con)

    # Convert to UTF-8, replacing invalid characters
    rfile <- iconv(rfile, from = "UTF-8", to = "UTF-8", sub = "byte")
    # Remove any NA entries resulting from failed conversions
    rfile <- rfile[!is.na(rfile)]
    # Filter out comments, empty lines, and Rmd chunk headers. We create a comment-less version
    rfile_nc <- grep('^(\\s*$|\\s*#|```\\s*\\{r)', rfile, invert = TRUE, value = TRUE)
    # Exclude lines containing knitr::
    rfile_nc <- grep('knitr::', rfile, invert = TRUE, value = TRUE)

    # get absolute paths based on grepl
    absolute_paths <- grep(
      '(?<![A-Za-z0-9_])(["\'])(?:(?!https?://)(?:[A-Za-z]:[\\\\/]|(?:\\\\\\\\|//)[^\\\\/]+[\\\\/]|~[/\\\\]|/(?:Users|home|var|etc|opt|srv|mnt|Volumes|Library|Applications|gpfs|data|tmp|media|root)\\b)[^"\']*)\\1',
      rfile_nc,
      value = TRUE,
      perl = TRUE
    )

    if (length(absolute_paths) > 0) {
      r_files$hardcoded_folders[i] <- 1
      r_files$absolute_paths[i] <- paste(absolute_paths, collapse = ", ")
    } else {
      r_files$hardcoded_folders[i] <- 0
      r_files$absolute_paths[i] <- NA
    }

    # Find lines where libraries are loaded
    library_lines <- grep("^[^#]*\\b(library|require)\\s*\\(", rfile_nc)
    # If the libraries are at most 3 lines apart, we consider it OK
    if (length(library_lines) > 1 && !all(diff(library_lines) < 4)) {
      r_files$library_on_top[i] <- 1
      r_files$library_lines[i] <- paste(library_lines, collapse = ", ")
    } else if (length(library_lines) > 0) {
      cat("Libraries are loaded in a single block, well done!\n")
      r_files$library_on_top[i] <- 0
      r_files$library_lines[i] <- NA
    } else {
      cat("No libraries are specified in this script.\n")
      r_files$library_on_top[i] <- NA
    }

    # Get statistics about lines of code and comments.
    total_lines <- length(rfile)
    # Count comment lines
    comment_lines <- sum(grepl("^\\s*#", rfile))
    r_files$comment_lines[i] <- comment_lines
    # Count code lines
    code_lines <- sum(rfile != "" & !grepl("^\\s*#", rfile))
    r_files$code_lines[i] <- code_lines
    # Percentage of comment lines
    percent_comments <- if (total_lines > 0) (comment_lines / total_lines) else NA
    r_files$percentage_comment[i] <- percent_comments

    # Examine files loaded, but missing in repo
    load_lines <- grep(grepl_load, rfile_nc, value = TRUE, perl = TRUE)
    loaded_file <- unlist(regmatches(load_lines,
                                     gregexpr("(['\"])[^'\"]*\\.[^'\"]*\\1", load_lines, perl = TRUE)))
    loaded_file <- gsub("^['\"]|['\"]$", "", loaded_file)
    loaded_file <- basename(loaded_file)

    missing_files <- loaded_file[!tolower(loaded_file) %in% tolower(files_in_repository)]

    r_files$loaded_files_missing[i] <- if (length(missing_files) == 0) {
      NA
    } else {
      1
    }
    r_files$loaded_files_missing_names[i] <- if (length(missing_files) == 0) {
      NA
    } else {
      paste(missing_files, collapse = ", ")
    }
  } # end of loop over r files

  # Create the report string
  library_issue <- r_files$name[which(r_files$library_on_top == 1)]
  if (length(library_issue) == 0) {
    report_library <- "Best programming practice is to load all required libraries at one place in the code. In all R files, all libraries were loaded in one block."
    summary_library <- "All libraries were loaded in one block."
    report_table_library <- NA
  } else {
    report_library <- sprintf(
      "Best programming practice is to load all required libraries at one place in the code. In %d R files, libraries were at multiple places in the R files (i.e., with more than 3 lines in between). This was true in the following R files, where libraries were loaded on the following lines:\n\n",
      length(library_issue)  )
    summary_library <- "Libraries were loaded in multiple places."
    issues_library <- paste(sprintf("**%s**", library_issue), collapse = "\n\n")
    lines_library <- paste(sprintf("**%s**", r_files$library_lines[which(r_files$library_on_top == 1)]), collapse = "\n\n")
    report_table_library <- r_files_subset <- r_files[!is.na(r_files$library_lines), c("name", "library_lines"), drop = FALSE]
    colnames(report_table_library) <- c("R File names", "Lines at which libraries are loaded")
  }

  # Create the report string hardcoded folders
  hardcoded_issues <- r_files$name[r_files$hardcoded_folders == 1]
  if (length(hardcoded_issues) == 0) {
    report_hardcoded <- "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names are do not exist on other computers. No hardcoded file paths were found in any of the R files."
    summary_hardcoded <- "No hardcoded file paths were found."
    report_table_hardcoded <- NA
  } else {
    report_hardcoded <- sprintf(
      "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names are do not exist on other computers. The following hardcoded file paths were found in %d R file(s).",
      length(hardcoded_issues))
    summary_hardcoded <- "Hardcoded file paths were found."
    report_table_hardcoded <- r_files_subset <- r_files[!is.na(r_files$absolute_paths), c("name", "absolute_paths"), drop = FALSE]
    colnames(report_table_hardcoded) <- c("R File names", "Absolute paths found")
  }

  # Create the report string for lack of comments
  comment_issue <- min(r_files$percentage_comment)
  if (comment_issue > 0) {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code. All your code files had comments."
    summary_comments <- "All your code files had comments."
    report_table_comments <- NA
  } else {
    report_comments <- sprintf("Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code. The following %d files had no comments:",
      sum(r_files$percentage_comment == 0)  )
    summary_comments <- "Some code files had no comments."
    report_table_comments <- r_files_subset <- r_files[!is.na(r_files$percentage_comment), c("name", "percentage_comment"), drop = FALSE]
    colnames(report_table_comments) <- c("R File names", "Percentage of lines that are comments")
  }

  # Create the report string for files loaded but not in repository
  missingfiles_issue <- r_files$loaded_files_missing[!is.na(r_files$loaded_files_missing)]
  if (length(missingfiles_issue) == 0) {
    report_missingfiles <- "All files loaded in the R scripts were present in the repository."
    summary_missingfiles <- "All files loaded in the R scripts were present in the repository."
    report_table_files_missing <- NA
  } else {
    report_missingfiles <- sprintf(
      "The scripts load files, but %d scripts loaded files that could not be automatically identified in the repository. Check if the following files are made available, so that others can reproduce your code, or that the files are missing:",
      length(missingfiles_issue))
    summary_missingfiles <- "Some files loaded in the R scripts were missing in the repository."
    report_table_files_missing <- r_files_subset <- r_files[!is.na(r_files$loaded_files_missing_names), c("name", "loaded_files_missing_names"), drop = FALSE]
    colnames(report_table_files_missing) <- c("R File names", "Files loaded in R file but missing in repository")
  }

  report <- paste(report_missingfiles, scroll_table(report_table_files_missing), report_hardcoded, scroll_table(report_table_hardcoded), report_library, scroll_table(report_table_library), report_comments, scroll_table(report_table_comments))

  if (length(missingfiles_issue) == 0 && comment_issue > 0 && length(hardcoded_issues) == 0 && length(library_issue) == 0) {
    tl <- "green"
  } else {
    tl <- "info"
  }

  # Aggregate by project and count number of 1s

  summary_table <- data.frame(
    id = paper$id,
    total_comments = sum(articles$total_comments, na.rm = TRUE),
    hardcoded_folders = sum(articles$has_doi, na.rm = TRUE),
    loaded_files_missing = sum(r_files$loaded_files_missing == 1, na.rm = TRUE),
    minimum_comments = min(r_files$percentage_comment, na.rm = TRUE)
  )

  # return a list ----
  list(
    table = r_files,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = paste(summary_missingfiles, summary_hardcoded, summary_library, summary_comments)
  )
}
