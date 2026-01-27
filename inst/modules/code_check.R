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
#'
#' @import dplyr
#' @import httr
#' @import jsonlite
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a list
code_check <- function(paper) {
  # example with osf Rmd files and github files: paper <- psychsci[[203]]
  # example with missing data files: paper <- psychsci[[221]]
  # Many R files, some with library in different places. paper <- psychsci[[225]]
  # Best example, with many issues, for paper: paper <- psychsci[[233]]
  # ResearchBox and GitHub example (in full xml): paper <- xml[["09567976251333666"]]

  all_files <- get_prev_outputs("repo_check", "table")
  if (is.null(all_files)) {
    mo <- module_run(paper, "repo_check")
    all_files <- mo$table %||% data.frame(file_name = character(0))
  }

  ## find code files ----
  code_ext <- grepl("\\.(r|rmd|qmd|sas|sps|do|ado)$",
                    all_files$file_name,
                    ignore.case = TRUE)
  code_files <- all_files[code_ext, , drop = FALSE]

  summary_code <- sprintf(
    "We found %d R, SAS, SPSS, or Stata code file%s.",
    nrow(code_files),
    plural(nrow(code_files))
  )

  # only look at first 20
  if (nrow(code_files) > 20) {
    summary_code <- paste(summary_code, "Only the first 20 were analysed.")
  }

  # no relevant code files found ----
  if (nrow(code_files) == 0) {
    info <- list(
      traffic_light = "na",
      summary_text = summary_code,
      summary_table = data.frame(
        id = paper$id,
        code_file_n = 0
      )
    )

    return(info)
  }

  # Check code ----

  # Create list of all file names in repository
  files_in_repository <- basename(all_files$file_name)

  # Shared absolute path pattern and quoted filename pattern
  absolute_path_pattern <- '(?<![A-Za-z0-9_])(["\'])(?:(?!https?://)(?:[A-Za-z]:[\\\\/]|(?:\\\\\\\\|//)[^\\\\/]+[\\\\/]|~[/\\\\]|/(?:Users|home|var|etc|opt|srv|mnt|Volumes|Library|Applications|gpfs|data|tmp|media|root)\\b)[^"\']*)\\1'

  # --- Process each code file (up to 20) ---
  maxfile <- min(nrow(code_files), 20)
  collected <- lapply(1:maxfile, \(i) {
    tryCatch({
      collector <- list()
      # access via URL if not local
      if (!is.na(code_files$file_location[i])) {
        con <- file(code_files$file_location[i], "r")
      } else {
        con <- url(code_files$file_url[i])
      }

      # read in files
      file_lines <- readLines(con, warn = FALSE)
      close(con)

      is_rmd <- grepl("\\.(rmd|qmd)",
                      code_files$file_name[[i]],
                      ignore.case = TRUE)
      if (is_rmd) {
        tmp_r <- tempfile(fileext = ".R")
        # prevent error on duplicate chunk labels
        old_knitr_opt <- getOption("knitr.duplicate.label")
        on.exit(options(knitr.duplicate.label = old_knitr_opt))
        options(knitr.duplicate.label = 'allow')

        knitr::purl(
          text = file_lines,
          output = tmp_r,
          quiet = TRUE,
          documentation = 1 # 0 = only code,
                            # 1 = chunk headers as comments
                            # 2 = all text as roxygen comments
        )
        file_lines <- readLines(tmp_r)
      }


      # Convert to UTF-8, replacing invalid characters
      file_lines <- iconv(file_lines, from = "UTF-8", to = "UTF-8", sub = "byte")
      # Remove any NA entries resulting from failed conversions
      file_lines <- file_lines[!is.na(file_lines)]

      # detect language (function below)
      lang <- detect_lang(code_files$file_name[i])
      collector$language <- lang

      # Create a comment-less version, per language
      file_nc <- remove_comments(file_lines, lang)

      # get absolute paths based on grepl (on non-comment lines)
      absolute_paths <- grep(
        absolute_path_pattern,
        file_nc,
        value = TRUE,
        perl = TRUE
      )
      collector$code_abs_path <- length(absolute_paths)
      collector$absolute_paths <- paste(absolute_paths, collapse = ", ")

      # Find lines where libraries/imports/includes are loaded  (function below)
      library_lines <- get_library_lines(file_nc, lang)

      # If the import statements are at most 3 lines apart, we consider it OK
      collector$library_lines <- length(library_lines)
      if (length(library_lines) > 1) {
        collector$library_max_between <- diff(library_lines) |> max()
      } else {
        collector$library_max_between <- NA
      }

      # Get statistics about lines of code and comments  (function below)
      line_stats <- get_line_stats(file_lines, lang)
      collector$comment_lines <- line_stats$comment_lines
      collector$code_lines <- line_stats$code_lines
      collector$percentage_comment <- line_stats$percent_comments

      # missing loaded files  (function below)
      missing_files <- get_missing_files(file_nc, lang, files_in_repository)
      collector$loaded_files_missing <- length(missing_files)
      collector$loaded_files_missing_names <- paste(missing_files, collapse = ", ")

      return(collector)
    },
    error = \(e) {
      collector <- list(error = e$message)
      return(collector)
    })
  }) # end of loop over code files

  code_check <- dplyr::bind_rows(collected)
  code_files <- dplyr::bind_cols(code_files[1:maxfile, ], code_check)

  # Reporting ----

  ## Libraries/imports grouping issues ----
  library_sep <- code_files$library_max_between > 3 &
    !is.na(code_files$library_max_between )
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
      "Best programming practice is to use relative file paths instead of absolute file paths (e.g., C://Lakens/files) as these folder names do not exist on other computers. The following absolute file paths were found in %d code file%s:",
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
    report_library
  )

  # traffic_light ----
  # green only if no issues across all code files
  if (length(missingfiles_issue) == 0 &&
      length(comment_issue) == 0 &&
      length(absolute_issues) == 0 &&
      length(library_issue) == 0) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # Aggregate by project
  summary_table <- data.frame(
    id = paper$id,
    code_n = nrow(code_files),
    code_abs_path = sum(code_files$code_abs_path, na.rm = TRUE),
    code_missing_files = sum(code_files$loaded_files_missing, na.rm = TRUE),
    code_min_comments = min(code_files$percentage_comment, na.rm = TRUE)
  )

  # summary_text ----
  summary_text <- c(
    summary_code,
    summary_comments,
    summary_missingfiles,
    summary_absolute,
    summary_library
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

# HELPER FUNCTIONS ----

# Helper: detect language by extension
detect_lang <- function(fname) {
  lname <- tolower(fname)
  # TODO: actually detect language used in qmd files
  if (grepl("\\.(r|rmd|qmd)$", lname)) {
    return("R")
  }
  if (grepl("\\.sas$", lname)) {
    return("SAS")
  }
  if (grepl("\\.sps$", lname)) {
    return("SPSS")
  }
  if (grepl("\\.(do|ado)$", lname)) {
    return("Stata")
  }
  return(NA_character_)
}

remove_comments <- function(file_lines, lang) {
  if (lang == "R") {
    file_nc <- grep("^(\\s*$|\\s*#|```\\s*\\{r)", file_lines, invert = TRUE, value = TRUE)
    # file_nc <- grep("knitr::", file_nc, invert = TRUE, value = TRUE)
  } else if (lang == "SAS") {
    in_block <- FALSE
    tmp <- character(0)
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
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
      ends_block <- grepl("\\*/", L)
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
      ends_block <- grepl("\\*/", L)
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
  # remove blank lines
  file_lines <- file_lines[trimws(file_lines) != ""]
  total_lines <- length(file_lines)

  if (lang == "R") {
    comment_lines <- sum(grepl("^\\s*#", file_lines))
    code_lines <- sum(!grepl("^\\s*#", file_lines))
  } else if (lang == "SAS") {
    # crude but effective: mark SAS line comments and block comments
    in_block <- FALSE
    is_comment_line <- logical(length(file_lines))
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*.*;\\s*$", L)
      is_comment_line[ln] <- in_block || line_comment
      if (in_block && ends_block) in_block <- FALSE
    }
    comment_lines <- sum(is_comment_line)
    code_lines <- sum(!is_comment_line)
  } else if (lang == "SPSS") {
    in_block <- FALSE
    is_comment_line <- logical(length(file_lines))
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      is_comment_line[ln] <- in_block || line_comment
      if (in_block && ends_block) in_block <- FALSE
    }
    comment_lines <- sum(is_comment_line)
    code_lines <- sum(!is_comment_line)
  } else if (lang == "Stata") {
    in_block <- FALSE
    is_comment_line <- logical(length(file_lines))
    for (ln in seq_along(file_lines)) {
      L <- file_lines[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      # treat lines containing only comments as comments (// may be EOL)
      eol_only <- grepl("^\\s*//", L)
      is_comment_line[ln] <- in_block || line_comment || eol_only
      if (in_block && ends_block) in_block <- FALSE
    }
    comment_lines <- sum(is_comment_line)
    code_lines <- sum(!is_comment_line)
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
  if (is.null(import_regex)) {
    return(integer(0))
  }
  lines <- grep(import_regex, file_nc, perl = TRUE)

  return(lines)
}

get_missing_files <- function(file_nc, lang, files_in_repository) {
  # Examine files loaded, but missing in repo
  lang_load_regex <- list(
    R = c(
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
    SAS = "\\b(proc\\s+import|infile|datafile\\s*=|libname)\\b",
    SPSS = "\\b(GET\\s+FILE|GET\\s+DATA|DATA\\s+LIST|SAVE)\\b",
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

