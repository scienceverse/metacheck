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
  # If there are no rows, return immediately
  print("retrieving file info from the OSF")
  if (nrow(osf_links_found) > 0) {
    osf_info_retrieved <- suppressWarnings(osf_retrieve(osf_links_found, recursive = TRUE, find_project = TRUE))
  }
  # Regex pattern for GitHub URLs (including subpaths)
  github_regex <- "https://github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"
  github_found <- search_text(paper, github_regex, return = "match")
  if (nrow(github_found) > 0) {
    github_file_list <- github_files(github_found$text[2], recursive = TRUE)
  }
  if (nrow(osf_links_found) == 0 && nrow(github_found) == 0) {
    report <- "No links to the Open Science Framework or Github were found."
    return(list(
      traffic_light = "na",
      report = report
    ))
  }
  
  # Ensure both data frames have the same columns
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
      cat("We found the following absolute paths:\n")
      cat(absolute_paths, sep = "\n")
      r_files$hardcoded_folders[i] <- 1
      r_files$absolute_paths[i] <- paste(absolute_paths, collapse = ", ")
    } else {
      cat("There are no absolute paths, well done!\n")
      r_files$hardcoded_folders[i] <- 0
      r_files$absolute_paths[i] <- NA
    }
    
    # Find lines where libraries are loaded
    library_lines <- grep("^[^#]*\\b(library|require)\\s*\\(", rfile_nc)
    # If the libraries are at most 3 lines apart, we consider it OK
    if (length(library_lines) > 1 && !all(diff(library_lines) < 4)) {
      cat("Libraries are loaded in multiple places in the script:\n")
      cat(sprintf("Line %d: %s\n", library_lines, rfile_nc[library_lines]), sep = "")
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
    print(loaded_file)
    
    missing_files <- loaded_file[!tolower(loaded_file) %in% tolower(files_in_repository)]
    
    r_files$loaded_files_missing[i] <- if (length(missing_files) == 0) {
      NA
    } else {
      paste(missing_files, collapse = ", ")
    }
  } # end of loop over r files
  # Create the report string
  library_issue <- r_files$name[which(r_files$library_on_top == 1)]
  if (length(library_issue) == 0) {
    report_library <- "\n\n#### Libraries Loaded\n\n Best programming practice is to load all required libraries at one place in the code. In all R files, all libraries were loaded in one block.\n\n"
  } else {
    library_report <- sprintf(
      "\n\n#### \n\n#### Libraries Loaded\n\n Best programming practice is to load all required libraries at one place in the code. In %d R files, libraries were at multiple places in the R files (i.e., with more than 3 lines in between). This was true in the following R files, where libraries were loaded on the following lines:\n\n",
      length(library_issue)  )
    issues_library <- paste(sprintf("**%s**", library_issue), collapse = "\n\n")
    lines_library <- paste(sprintf("**%s**", r_files$library_lines[which(r_files$library_on_top == 1)]), collapse = "\n\n")
    report_library <- sprintf(
      "%s\n\n%s\n\n%s\n\n",
      library_report, issues_library, lines_library)
  }

  # Create the report string hardcoded folders
  hardcoded_issues <- r_files$name[r_files$hardcoded_folders == 1]
  if (length(hardcoded_issues) == 0) {
    report_hardcoded <- "\n\n#### Hardcoded Paths\n\n Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files as these folder names are do not exist on other computers. No hardcoded file paths were found in any of the R files.\n\n"
  } else {
    hardcoded_report <- sprintf(
      "\n\n#### Hardcoded Paths\n\n Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files as these folder names are do not exist on other computers. The following hardcoded file paths were found in %d R file(s).\n\n",
      length(hardcoded_issues)  )
    issues_hardcoded <- paste(sprintf("**%s**", hardcoded_issues), collapse = "\n\n")
    paths_hardcoded <- paste(sprintf("**%s**", r_files$absolute_paths[r_files$hardcoded_folders == 1]), collapse = "\n\n")
    report_hardcoded <- sprintf(
      "%s\n\n%s\n\n%s\n\n",
      hardcoded_report, issues_hardcoded, paths_hardcoded)
  }
  
  # Create the report string for lack of comments
  comment_issue <- min(r_files$percentage_comment)
  if (comment_issue > 0) {
    report_comments <- "\n\n#### Commenting Code\n\n Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code. All your code files had comments.\n\n"
  } else {
    comments_report <- sprintf(
      "\n\n#### Commenting Code\n\n Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code. The following %d files had no comments:\n\n",
      sum(r_files$percentage_comment == 0)  )
    issues_comments <- paste(sprintf("**%s**", r_files$name[r_files$percentage_comment == 0]), collapse = "\n\n")
    report_comments <- sprintf(
      "%s\n\n%s\n\n",
      comments_report, issues_comments)
  }
  
  # Create the report string for files loaded but not in repository
  missingfiles_issue <- r_files$loaded_files_missing[!is.na(r_files$loaded_files_missing)]
  if (length(missingfiles_issue) == 0) {
    report_missingfiles <- "\n\n#### Missing Files\n\n All files loaded in the R scripts were present in the repository.\n\n"
  } else {
    missingfiles_report <- sprintf(
      "\n\n#### Missing Files\n\n The scripts load files, but %d scripts loaded files that could not be automatically identified in the repository. Check if the following files are made available, so that others can reproduce your code, or that the files are missing:\n\n",
      length(missingfiles_issue))
    issues_missingfiles <- paste(sprintf("**%s**", missingfiles_issue), collapse = "\n\n")
    report_missingfiles <- sprintf(
      "%s\n\n%s\n\n",
      missingfiles_report, issues_missingfiles)
  }

  report <- paste(report_missingfiles, report_hardcoded, report_library, report_comments)
  
  if (length(missingfiles_issue) == 0 && comment_issue > 0 && length(hardcoded_issues) == 0 && length(library_issue) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }
  # return a list ----
  list(
    traffic_light = tl,
    report = report
  )
}
