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
  # example with osf Rmd files and github files:
  # paper <- psychsci[[203]]
  # example with missing data files:
  # paper <- psychsci[[221]]
  # Many R files, some with library in different places.
  # paper <- psychsci[[225]]
  # Best example, with many issues, for paper:
  # paper <- psychsci[[233]]

  # find R files ----

  ## search text for URLs ----
  osf_links_found <- osf_links(paper)
  github_regex <- "https://github\\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*"
  github_links_found <- search_text(paper, github_regex, return = "match")

  ## no relevant URLs found ----
  if (nrow(osf_links_found) == 0 && nrow(github_links_found) == 0) {
    report <- "No links to the Open Science Framework or Github were found."
    return(list(
      traffic_light = "na",
      report = report,
      summary_text = report
    ))
  }

  ## get r files ----
  #TODO: check if find_project should be set -- it takes time and doesn't add anything we're using
  osf_files <- osf_retrieve(osf_links_found, recursive = TRUE,
                                     find_project = FALSE)
  if (!"name" %in% colnames(osf_files)) osf_files$name <- NA
  if (!"download_url" %in% colnames(osf_files)) osf_files$download_url <- NA
  github_files <- github_files(github_links_found$text, recursive = FALSE)
  if (!"name" %in% colnames(github_files)) github_files$name <- NA
  if (!"download_url" %in% colnames(github_files)) github_files$download_url <- NA
  all_files <- dplyr::bind_rows(osf_files, github_files)
  r_ext <- grepl("\\.(r|rmd|qmd)$", all_files$name, ignore.case = TRUE)
  r_files <- all_files[r_ext, c("name", "download_url")]

  if (nrow(r_files) == 0) {
    cols <- c("name", "download_url", "osf_id") |>
      intersect(names(all_files))
    table <- unique(all_files[!is.na(all_files$download_url), cols])

    n_osf <- nrow(osf_links_found)
    n_github <- nrow(github_links_found)
    summary_text <- sprintf("We found %d OSF link%s and %d GitHub link%s, but no R files were found.",
                            n_osf, plural(n_osf),
                            n_github, plural(n_github))

    return(list(
      table = table,
      traffic_light = "na",
      report = c(summary_text, scroll_table(table)),
      summary_text = summary_text
    ))
  }

  # Add comments for aspects we will check
  r_files$hardcoded <- NA
  r_files$hardcoded_paths <- NA_character_
  r_files$library_spread <- NA
  r_files$library_lines <- NA_character_
  r_files$total_lines <- NA_real_
  r_files$comment_lines <- NA_real_
  r_files$code_lines <- NA_real_
  r_files$percentage_comment <- NA_real_
  r_files$loaded_files_missing <- NA_real_
  r_files$loaded_files_missing_names <- NA_real_

  # Create list of all file names in repository. Will compare loaded files against this list to see if files are loaded, but not shared
  # TODO: this should be specific to the repo when there are multiple github or OSF repos linked
  files_in_repository <- basename(all_files$name)

  # Lines with data-loading calls
  grepl_load <- "\\b((?:[[:alnum:]_.]+::)?read\\.(?:csv2?|table|delim2?)|(?:[[:alnum:]_.]+::)?readRDS|(?:[[:alnum:]_.]+::)?load|(?:[[:alnum:]_.]+::)?read_(?:csv2?|tsv|delim|rds|lines)|(?:[[:alnum:]_.]+::)?fread|(?:[[:alnum:]_.]+::)?read_(?:xlsx?|excel)|(?:[[:alnum:]_.]+::)?read\\.xlsx|(?:[[:alnum:]_.]+::)?read_(?:dta|sav|sas)|(?:[[:alnum:]_.]+::)?read\\.dta|(?:[[:alnum:]_.]+::)?read_feather|(?:[[:alnum:]_.]+::)?read_parquet|(?:[[:alnum:]_.]+::)?fromJSON|(?:[[:alnum:]_.]+::)?read_yaml|(?:[[:alnum:]_.]+::)?read_xml|(?:[[:alnum:]_.]+::)?read_ods|(?:[[:alnum:]_.]+::)?readtext)\\s*\\(|\\bsource\\s*\\("


  # process files ----
  for (i in seq_along(r_files$name)) {
    ## Read in the R code ----
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

    ## hardcoded ----
    hardcoded_paths <- grep(
      '(?<![A-Za-z0-9_])(["\'])(?:(?!https?://)(?:[A-Za-z]:[\\\\/]|(?:\\\\\\\\|//)[^\\\\/]+[\\\\/]|~[/\\\\]|/(?:Users|home|var|etc|opt|srv|mnt|Volumes|Library|Applications|gpfs|data|tmp|media|root)\\b)[^"\']*)\\1',
      rfile_nc,
      value = TRUE,
      perl = TRUE
    )

    r_files$hardcoded[i] <- length(hardcoded_paths)
    r_files$hardcoded_paths[i] <- paste(hardcoded_paths, collapse = ", ")

    ## libraries ----
    library_lines <- grep("^[^#]*\\b(library|require)\\s*\\(", rfile_nc)
    r_files$library_lines[i] <- paste(library_lines, collapse = ", ")

    # If the libraries are at most 3 lines apart, we consider it OK
    if (length(library_lines) > 1 && !all(diff(library_lines) < 4)) {
      r_files$library_spread[i] <- TRUE
    } else if (length(library_lines) > 0) {
      r_files$library_spread[i] <- FALSE
    }

    # TODO: check this logic for qmd/rmd and blank lines
    ## code and comments ----
    total_lines <- length(rfile)
    r_files$total_lines[i] <- total_lines
    # Count comment lines
    comment_lines <- sum(grepl("^\\s*#", rfile))
    r_files$comment_lines[i] <- comment_lines
    # Count code lines
    code_lines <- sum(rfile != "" & !grepl("^\\s*#", rfile))
    r_files$code_lines[i] <- code_lines
    # Percentage of comment lines
    percent_comments <- if (total_lines > 0) (comment_lines / total_lines) else NA
    r_files$percentage_comment[i] <- percent_comments

    ## missing files ----
    # Examine files loaded, but missing in repo
    load_lines <- grep(grepl_load, rfile_nc, value = TRUE, perl = TRUE)
    loaded_file <- unlist(regmatches(load_lines,
                                     gregexpr("(['\"])[^'\"]*\\.[^'\"]*\\1", load_lines, perl = TRUE)))
    loaded_file <- gsub("^['\"]|['\"]$", "", loaded_file)
    loaded_file <- basename(loaded_file)

    # TODO: why originally tolower? file names are case sensitive
    #missing_files <- loaded_file[!tolower(loaded_file) %in% tolower(files_in_repository)]
    missing_files <- loaded_file[!loaded_file %in% files_in_repository]
    r_files$loaded_files_missing[i] <- length(missing_files)
    r_files$loaded_files_missing_names[i] <- paste(missing_files, collapse = ", ")
  } # end of process files loop over r files


  # report ----
  ## library ----
  library_issue <- r_files$name[which(r_files$library_spread)]
  if (length(library_issue) == 0) {
    report_library <- "Best programming practice is to load all required libraries at one place in the code. In all R files, all libraries were loaded in one block."
    summary_library <- "All libraries were loaded in one block."
    report_table_library <- NULL
  } else {
    report_library <- sprintf(
      "Best programming practice is to load all required libraries at one place in the code. In %d R files, libraries were at multiple places in the R files (i.e., with more than 3 lines in between). This was true in the following R files, where libraries were loaded on the following lines:\n\n",
      length(library_issue)  )
    summary_library <- "Libraries were loaded in multiple places."
    issues_library <- paste(sprintf("**%s**", library_issue), collapse = "\n\n")
    lines_library <- paste(sprintf("**%s**", r_files$library_lines[which(r_files$library_spread == 1)]), collapse = "\n\n")
    rows <- r_files$library_spread
    report_table_library <- r_files[rows, c("name", "library_lines"), drop = FALSE]
    colnames(report_table_library) <- c("R File names", "Lines at which libraries are loaded")
  }

  ## hardcoded ----
  hardcoded_issues <- r_files$name[r_files$hardcoded > 0]
  if (length(hardcoded_issues) == 0) {
    report_hardcoded <- "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names are do not exist on other computers. No hardcoded file paths were found in any of the R files."
    summary_hardcoded <- "No hardcoded file paths were found."
    report_table_hardcoded <- NULL
  } else {
    report_hardcoded <- sprintf(
      "Best programming practice is to use relative file paths instead of hardcoded file paths (e.g., C://Lakens/files) as these folder names are do not exist on other computers. The following %d hardcoded file paths were found in %d R file(s).",
      sum(r_files$hardcoded),
      length(hardcoded_issues))
    summary_hardcoded <- "Hardcoded file paths were found."
    report_table_hardcoded <- r_files[!is.na(r_files$hardcoded_paths),
                                      c("name", "hardcoded_paths"),
                                      drop = FALSE]
    report_table_hardcoded <- tidyr::separate_longer_delim(report_table_hardcoded, hardcoded_paths, ", ")
    colnames(report_table_hardcoded) <- c("R File names", "Absolute paths found")

  }

  ## comments ----
  comment_issue <- min(r_files$percentage_comment)
  if (comment_issue > 0) {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code. All your code files had comments."
    summary_comments <- "All your code files had comments."
    report_table_comments <- NULL
  } else {
    n_no_comments <- sum(r_files$percentage_comment == 0)
    report_comments <- sprintf("Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code. The following %d file%s had no comments:",
                               n_no_comments, plural(n_no_comments)  )
    summary_comments <- sprintf("%d file%s had no comments.",
                               n_no_comments, plural(n_no_comments)  )
    cols <- c("name", "total_lines", "code_lines", "comment_lines", "percentage_comment")
    report_table_comments <- r_files[!is.na(r_files$percentage_comment), cols, drop = FALSE]
    report_table_comments$percentage_comment <- sprintf("%d%%", round(100*report_table_comments$percentage_comment))
    colnames(report_table_comments) <- c("R File names", "Total Lines", "Code Lines", "Comments", "Percent comments")
  }

  ## missingfiles ----
  # Create the report string for files loaded but not in repository
  missingfiles_issue <- sum(r_files$loaded_files_missing > 0)
  if (missingfiles_issue == 0) {
    report_missingfiles <- "All files loaded in the R scripts were present in the repository."
    summary_missingfiles <- "All files loaded in the R scripts were present in the repository."
    report_table_files_missing <- NULL
  } else {
    report_missingfiles <- sprintf(
      "The scripts load files, but %d scripts loaded %d files that could not be automatically identified in the repository. Check if the following files are made available, so that others can reproduce your code, or that the files are missing:",
      missingfiles_issue,
      sum(r_files$loaded_files_missing)
    )
    summary_missingfiles <- "Some files loaded in the R scripts were missing in the repository."
    rows <- r_files$loaded_files_missing_names != ""
    report_table_files_missing <- r_files[rows, c("name", "loaded_files_missing_names"), drop = FALSE]
    colnames(report_table_files_missing) <- c("R File names", "Files loaded in R file but missing in repository")
  }

  ## set up table of R file links ----
  cols <- c("name", "download_url") |>
    intersect(names(all_files))
  rows <- all_files$download_url %in% r_files$download_url
  report_table <- unique(all_files[rows, cols])
  report_table$download_url <- sprintf("<a href='%s'>download</a>",
                                       report_table$download_url)
  names(report_table) <- c("R File Name", "Download")

  report <- c(
    "Below, we describe some best coding practices and give the results of automatic evaluation of these practices in the R files below. This check may miss things or produce false positives if your R scripts are less typical.",
    scroll_table(report_table, scroll_above = 5),
    "### Missing Files",
    report_missingfiles,
    scroll_table(report_table_files_missing, scroll_above = 5),
    "### Hardcoded Paths",
    report_hardcoded,
    scroll_table(report_table_hardcoded, scroll_above = 5),
    "### Libraries",
    report_library,
    scroll_table(report_table_library, scroll_above = 5),
    "### Code Comments",
    report_comments,
    scroll_table(report_table_comments, scroll_above = 5)
  )

  if (missingfiles_issue == 0 &&
      comment_issue > 0 &&
      length(hardcoded_issues) == 0 &&
      length(library_issue) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # summary_table ----
  # Aggregate by project and count number of 1s
  summary_table <- data.frame(
    id = paper$id,
    code_library_spread = sum(r_files$library_spread, na.rm = TRUE),
    code_hardcoded_paths = sum(r_files$hardcoded, na.rm = TRUE),
    code_loaded_files_missing = sum(r_files$loaded_files_missing == 1, na.rm = TRUE),
    code_minimum_comments = min(r_files$percentage_comment, na.rm = TRUE)
  )

  # return a list ----
  list(
    table = r_files,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = paste(summary_missingfiles,
                         summary_hardcoded,
                         summary_library,
                         summary_comments)
  )
}
