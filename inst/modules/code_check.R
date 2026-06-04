#' Code Check
#'
#' @description
#' This module retrieves information from repositories checked by repo_check about code files (R, SAS, SPSS, Stata).
#'
#' @details
#' The Code Check module checks R, Rmd, Qmd, SAS, SPSS, and Stata files, using regular expressions to check the code. The regular expression search will detect the number of comments, the lines at which libraries/imports are loaded, attempts to detect absolute paths to files, and lists files that are loaded, and checks if these files are in the repository. The module will return suggestions to improve the code if there are no comments, if libraries/imports are loaded in lines further than 4 lines apart, if files that are loaded are not in the repository, and if absolute file paths are found.
#'
#' The regular expressions can miss information in code files, or falsely detect parts of the code as a fixed file path. Libraries/imports might be loaded in one block, even if there are more than 3 intermittent lines. The package was validated internally on papers published in Psychological Science. There might be valid reasons why some loaded files can’t be shared, but the module can’t evaluate these reasons, and always gives a warning.
#'
#' If you want to extend the package to perform additional checks on code files, or make the checks work on other types of code files, reach out to the Metacheck development team.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Raphael Merz (\email{r.t.p.merz@tue.nl})
#'
#' @import dplyr
#' @import httr
#' @import jsonlite
#'
#' @param paper a paper object or paperlist object, or NULL to check local files only (see [test_paper()])
#' @param file_limit the maximum number of files per repository to assess. This prevents downloading and processing hundreds of .R files from, e.g., an R package repo.
#' @param local_path optional path to a local directory. When provided, all files in that directory (recursively) are added to the file list alongside any files found via `repo_check`.
#'
#' @returns a list
code_check <- function(paper, file_limit = 20, local_path = NULL) {
  # example with osf Rmd files and github files: paper <- psychsci[[203]]
  # example with missing data files: paper <- psychsci[[221]]
  # Many R files, some with library in different places. paper <- psychsci[[225]]
  # Best example, with many issues, for paper: paper <- psychsci[[233]]
  # ResearchBox and GitHub example (in full xml): paper <- xml[["09567976251333666"]]

  all_files <- get_prev_outputs("repo_check", "table")
  if (is.null(all_files)) {
    if (!is.null(local_path)) {
      mo <- module_run(paper, "repo_check", local_path = local_path)
    } else {
      mo <- module_run(paper, "repo_check")
    }
    all_files <- mo$table %||% data.frame(file_name = character(0), repo_url = character(0))
  }
  all_files$language <- code_lang(all_files$file_name)

  ## find relevant code files ----
  relevant <- all_files$lang %in% c("R", "SAS", "SPSS", "Stata")
  code_files <- all_files[relevant, , drop = FALSE]

  summary_code <- sprintf(
    "We found %d R, %d SAS, %d SPSS, and %d Stata code file%s.",
    sum(code_files$language == "R"),
    sum(code_files$language == "SAS"),
    sum(code_files$language == "SPSS"),
    sum(code_files$language == "Stata"),
    plural(nrow(code_files))
  )

  # only look at first file_limit files in each repo
  n_files <- dplyr::count(code_files, repo_url)$n

  if (any(n_files > file_limit)) {
    summary_code <- paste(summary_code, "Only the first", file_limit, "files per repository were analysed.")

    checked_files <- dplyr::slice_head(code_files,
                                       n = file_limit,
                                       by = repo_url)
  } else {
    checked_files <- code_files
  }

  # no relevant code files found ----
  if (nrow(code_files) == 0) {
    info <- list(
      traffic_light = "na",
      summary_text = summary_code,
      summary_table = data.frame(
        paper_id = paper_id(paper),
        code_file_n = 0
      )
    )

    return(info)
  }

  # Check code ----

  # Create list of all file names in repository
  # TODO: iterate this by repo so file names don't bleed over

  # --- Process each code file (up to file_limit) ---
  pb_code <- pb(nrow(checked_files), ":what [:bar] :current/:total")
  pb_code$tick(0, list(what = ""))
  on.exit(pb_code$terminate())

  collected <- lapply(seq_along(checked_files$file_location), \(i) {
    the_file <- checked_files[i, ]
    the_file$checked <- TRUE
    pb_code$tick(1, list(what = the_file$file_name))

    tryCatch({
      # access via URL if not local
      if (!is.na(the_file$file_location)) {
        file_path <- the_file$file_location
      } else {
        file_path <- the_file$file_url
      }

      # read in files
      is_rmd <- grepl("\\.(rmd|qmd)",
                      the_file$file_name,
                      ignore.case = TRUE)
      if (is_rmd) {
        file_lines <- code_extract_r(file_path)
      } else {
        file_lines <- code_read(file_path)
      }

      # try to parse R-type code
      if (the_file$language == "R") {
        parse_check <- code_parse_r(text = file_lines)
        the_file$parse_error <- parse_check$error
        the_file$parse_error_msg <- parse_check$msg
      }

      # Create a comment-less version, per language
      file_nc <- code_remove_comments(file_lines, the_file$language)

      # get absolute paths based on grepl (on non-comment lines)
      absolute_paths <- code_abs_path(file_nc)
      the_file$code_abs_path <- nrow(absolute_paths)
      the_file$absolute_paths <- paste(absolute_paths$abs_path,
                                       collapse = "\n")

      # Find lines where libraries/imports/includes are loaded
      library_lines <- code_library_lines(file_nc, the_file$language)

      # If the import statements are at most 3 lines apart, we consider it OK
      the_file$library_lines <- nrow(library_lines)
      if (nrow(library_lines) > 1) {
        the_file$library_max_between <- diff(library_lines$line) |> max()
      } else {
        the_file$library_max_between <- NA_integer_
      }

      # Get statistics about lines of code and comments
      line_stats <- code_line_stats(file_lines, the_file$language)
      the_file$comment_lines <- line_stats$comment_lines
      the_file$code_lines <- line_stats$code_lines
      the_file$percentage_comment <- line_stats$percent_comments

      # missing loaded files
      file_refs <- code_file_refs(file_nc, the_file$language)
      files_in_repo <- all_files[all_files$repo_url == the_file$repo_url, ]$file_name
      # fix possible winslashes
      base_ref <- gsub("\\\\", "/", file_refs) |> basename()
      base_repo <- gsub("\\\\", "/", files_in_repo) |> basename()
      missing_files <- setdiff(base_ref, base_repo)
      the_file$loaded_files_missing <- length(missing_files)
      the_file$loaded_files_missing_names <- paste(missing_files, collapse = ", ")

      return(the_file)
    },
    error = \(e) {
      the_file$error = e$message
      return(the_file)
    })
  }) # end of loop over code files

  code_check <- dplyr::bind_rows(collected)
  code_files <- dplyr::left_join(code_files, code_check, by = names(code_files))
  code_files$checked[is.na(code_files$checked)] <- FALSE

  # Reporting ----

  ## library ----
  library_sep <- sapply(code_files$library_max_between > 3, isTRUE)
  library_issue <- code_files$file_name[library_sep]
  if (length(library_issue) == 0) {
    report_library <- "Best programming practice is to load all required libraries/imports in one block near the top of the code. In all code files, libraries/imports were loaded in one block."
    summary_library <- "All libraries/imports were loaded in one block."
    report_table_library <- NULL
  } else {
    report_library <- sprintf(
      "Best programming practice is to load all required libraries/imports in one block near the top of the code. In %d code files, libraries/imports were at multiple places (i.e., with more than 3 non-comment lines in between).",
      length(library_issue)
    )
    summary_library <- "Libraries/imports were loaded in multiple places."
  }

  ## absolute paths ----
  absolute_issues <- code_files$file_name[code_files$code_abs_path > 0]
  if (length(absolute_issues) == 0) {
    report_absolute <- "Best programming practice is to use relative file paths (e.g., './files') instead of absolute file paths (e.g., 'C://Lakens/project_dir/files') as these folder names do not exist on other computers. No absolute file paths were found in any of the code files."
    summary_absolute <- "No absolute file paths were found."
    report_table_absolute <- NULL
  } else {
    report_absolute <- sprintf(
      "Best programming practice is to use relative file paths (e.g., './files') instead of absolute file paths (e.g., 'C://Lakens/project_dir/files') as these folder names do not exist on other computers. The following absolute file paths were found in %d code file%s. However, these may be false positives in code like `paste0(dir, '/file.csv')`. ",
      length(absolute_issues),
      plural(length(absolute_issues))
    )
    summary_absolute <- "Absolute file paths were found."
    cols <- c("file_name", "absolute_paths")
    report_table_absolute <- code_files[code_files$code_abs_path > 0, cols]
    colnames(report_table_absolute) <- c("File name", "Absolute paths found")
  }

  ## Comments ----
  comment_issue <- code_files$file_name[code_files$percentage_comment == 0]
  if (length(comment_issue) == 0) {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code). All your code files had comments."
    summary_comments <- "All your code files had comments."
  } else {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code)."
    summary_comments <- sprintf(
      "%d code file%s had no comments.",
      length(comment_issue),
      plural(length(comment_issue))
    )
  }
  cols <- c("file_name", "language", "percentage_comment")
  rows <- !is.na(code_files$percentage_comment)
  report_table_comments <- code_files[rows, cols]
  report_table_comments$percentage_comment <- sprintf("%.0f%%", report_table_comments$percentage_comment * 100)
  colnames(report_table_comments) <- c(
    "File name", "Language", "Percent comments"
  )

  ## Missing files ----
  missingfiles_issue <- code_files$file_name[code_files$loaded_files_missing > 0]
  if (length(missingfiles_issue) == 0) {
    summary_missingfiles <- "All files loaded in the code were present in the repository."
    report_missingfiles <- summary_missingfiles
    report_table_files_missing <- NULL
  } else {
    n_missing <- sum(code_files$loaded_files_missing, na.rm = TRUE)
    summary_missingfiles <- sprintf(
      "%d file%s loaded in the code %s missing in the repository.",
      n_missing, plural(n_missing), plural(n_missing, "was", "were")
    )

    report_missingfiles <- sprintf(
      "The scripts load files, but %d script%s loaded %d file%s that could not be automatically identified in the repository. Check if the following files are made available, so that others can reproduce your code, or that the files are missing:",
      length(missingfiles_issue),
      plural(length(missingfiles_issue)),
      n_missing,
      plural(n_missing)
    )

    rows <- code_files$loaded_files_missing > 0
    cols <- c("file_name", "loaded_files_missing_names")
    report_table_files_missing <- code_files[rows, cols]
    colnames(report_table_files_missing) <- c("File name", "Missing Files")
  }

  ## set up table of code file links ----
  cols <- c("file_name", "file_url",
            "percentage_comment",
            "loaded_files_missing",
            "code_abs_path",
            "library_max_between") |>
    intersect(names(code_files))
  report_table <- unique(code_files[, cols])
  report_table$file_name <- link(report_table$file_url, report_table$file_name)
  report_table$file_url <- NULL
  report_table$percentage_comment <- sprintf("%.0f%%", report_table$percentage_comment * 100)
  names(report_table) <- c("File Name", "% Comments", "Missing Files", "Absolute Paths", "Code Between Libraries")

  ## Parsable Code ----
  parse_issues <- sum(code_files$parse_error, na.rm = TRUE)
  if (parse_issues == 0) {
    report_parse <- "All R-type code files (.R, .Rmd, .qmd) could be read in. There were no parsing issues."
    summary_parse <- "No parsing issues of R-type files were found."
    report_table_parse <- NULL
  } else {
    report_parse <- sprintf(
      "We encountered parsing issues when trying to read in R-type code files. The following errors were found in %d code file%s:",
      parse_issues,
      plural(parse_issues)
    )
    summary_parse <- "Parsing issues of R-type files were found."
    cols <- c("file_name", "parse_error_msg")
    report_table_parse <- code_files[isTRUE(code_files$parse_error), cols]
    colnames(report_table_parse) <- c("File name", "Error Message")
  }

  report <- c(
    "Below, we describe some best coding practices and give the results of automatic evaluation of these practices in the code files below. This check may miss things or produce false positives if your scripts are less typical.",
    scroll_table(report_table, maxrows = 5),
    "#### Code Comments",
    report_comments,
    "#### Missing Files",
    report_missingfiles,
    scroll_table(report_table_files_missing, maxrows = 5),
    "#### Absolute Paths",
    report_absolute,
    scroll_table(report_table_absolute, maxrows = 5),
    "#### Libraries / Imports",
    report_library,
    "#### Parsable code",
    report_parse,
    scroll_table(report_table_parse)
  )

  # traffic_light ----
  # green only if no issues across all code files
  if (length(missingfiles_issue) == 0 &&
      length(comment_issue) == 0 &&
      length(absolute_issues) == 0 &&
      length(library_issue) == 0 &&
      length(parse_issues) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # Aggregate by project
  summary_table <- data.frame(
    paper_id = paper$paper_id,
    code_n = nrow(code_files),
    code_checked = sum(code_files$checked, na.rm = TRUE),
    code_abs_path = sum(code_files$code_abs_path, na.rm = TRUE),
    code_missing_files = sum(code_files$loaded_files_missing, na.rm = TRUE),
    code_min_comments = min(code_files$percentage_comment, na.rm = TRUE),
    code_parse_errors = sum(code_files$parse_error, na.rm = TRUE)
  )

  # summary_text ----
  summary_text <- c(
    summary_code,
    summary_comments,
    summary_missingfiles,
    summary_absolute,
    summary_library,
    summary_parse
  ) |>
    paste("\n- ", x = _, collapse = "")

  # table ----
  table <- code_files
  table$file_location <- NULL

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = c(code_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}
