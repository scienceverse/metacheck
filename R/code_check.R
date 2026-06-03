#' Read code from files
#'
#' @param file_path a file path or url to read in
#'
#' @returns a character vector of the file contents
#' @export
#'
#' @examples
#' file_path <- demofile("json")
#' text <- code_read(file_path)
code_read <- function(file_path) {
  # first try readr, handles most encodings well
  enc <- readr::guess_encoding(file_path)$encoding[[1]]

  file_lines <- tryCatch(
    readr::read_lines(file_path, locale = readr::locale(encoding = enc)),
    error = \(e) { NULL },
    warning = \(w) { NULL }
  )

  # Fallback: base R handles NULs
  if (is.null(file_lines)) {
    file_lines <- readLines(file_path, warn = FALSE, skipNul = TRUE)
  }

  # Convert to UTF-8, replacing invalid characters
  file_lines <- iconv(file_lines, to = "UTF-8", sub = "byte")
  # Remove any NA entries resulting from failed conversions
  file_lines <- file_lines[!is.na(file_lines)]

  return(file_lines)
}


#' Detect Code Language
#'
#' Detects code language used in files, only for languages metacheck currently processes (R, SAS, SPSS, Stata).
#'
#' @param file_name a vector of file names
#'
#' @returns a vector of languages
#' @export
#'
#' @examples
#' file_name <- "file.R"
#' code_lang(file_name)
#'
#' file_name <- c("file.Rmd", "file.SAS", "file.r", "file.qmd", "file.txt")
#' code_lang(file_name)
code_lang <- function(file_name) {
  if (length(file_name) > 1) {
    return(sapply(file_name, code_lang))
  } else if (length(file_name) == 0) {
    return(character(0))
  }

  lname <- tolower(file_name)
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

#' Convert Rmd/qmd files to R code only
#'
#' @param file_path a vector of file paths to check
#' @param save_path if NULL, returns a text vector, else a path to save to
#' @param documentation 0:2 value to pass to knitr::purl
#' @param text alternative to file_path, pass text directly
#'
#' @returns a character vector
#' @export
#'
#' @examples
#' file_path <- demofile("qmd")
#' code_text <- code_extract_r(file_path)
code_extract_r <- function(file_path = NULL, save_path = NULL, documentation = 0, text = NULL) {
  if (is.null(file_path) & is.null(text)) {
    stop("You must specify one of file_path or text")
  } else if (is.null(text)) {
    text <- code_read(file_path)
  }

  if (is.null(save_path)) {
    output <- tempfile(fileext = ".R")
    on.exit(unlink(output))
  } else {
    output <- save_path
  }

  # prevent error on duplicate chunk labels
  old_knitr_opt <- getOption("knitr.duplicate.label")
  on.exit(options(knitr.duplicate.label = old_knitr_opt))
  options(knitr.duplicate.label = 'allow')

  # purl errors are very unlikely
  knitr::purl(
    text = text,
    output = output,
    documentation = documentation,
    quiet = TRUE
  )

  if (is.null(save_path)) {
    code_read(output)
  } else {
    save_path
  }
}

#' Parse code to check for errors
#'
#' @param file_path a vector of file paths to check
#' @param text alternative to file_path, pass text directly
#'
#' @returns a data frame with columns `file_path` and `line`
#' @export
#'
#' @examples
#' file_path <- demofile("qmd")
#' code_parse_r(file_path)
code_parse_r <- function(file_path = "", text = NULL) {
  if (all(file_path == "") & is.null(text)) {
    stop("You must specify one of file_path or text")
  }

  errors <- lapply(file_path, \(fp) {
    if (fp != "") text <- code_read(fp)

    # check for rmd/qmd file
    if (grepl("^---\\s*$", text[[1]])) {
      text <- code_extract_r(text = text)
    }

    # attempt to parse and catch errors
    parse_check <- tryCatch({
      parse(text = text, keep.source = TRUE)
      list(file_path = fp, error = FALSE, msg = NA_character_)
    }, error = \(e) {
      msg <- sub("<text>", "line", e$message, fixed = TRUE)
      return(list(file_path = fp, error = TRUE, msg = msg))
    })

    return(parse_check)
  }) |> dplyr::bind_rows()

  return(errors)
}

#' Return Absolute Paths
#'
#' Check code for the presence of absolute paths
#'
#' @param code_text the text of the code, excluding comments
#'
#' @returns a vector of absolute paths
#' @export
#'
#' @examples
#' code_text <- c(
#'   "file <- 'C:/User/lakens/file.R'",
#'   "tmp <- '/User/lakens/file.html'",
#'   "convert(file, tmp)"
#' )
#' code_abs_path(code_text)
code_abs_path <- function(code_text) {
  text <- text_id <- NULL # fix cmd check note
  # Shared absolute path pattern and quoted filename pattern
  # absolute_path_pattern <- '(?<![A-Za-z0-9_])(["\'])(?:(?!https?://)(?:[A-Za-z]:[\\\\/]|(?:\\\\\\\\|//)[^\\\\/]+[\\\\/]|~[/\\\\]|/(?:Users|home|var|etc|opt|srv|mnt|Volumes|Library|Applications|gpfs|data|tmp|media|root)\\b)[^"\']*)\\1'

  absolute_path_pattern <- paste0(
    "([\"'])", # start quote
    "(?:~/(?:[^\\n'\"]+)|", # e.g., ~/Desktop/...
    "/(?!/)[^\\n'\"]+|",    # e.g., /User/...
    "[A-Za-z]:[\\\\/][^\\n'\"]+|", # e.g., C:/... or D:\...
    "\\\\\\\\[^\\n'\"]+)", # e.g., \\server
    "\\1" # end matching quote
  )

  code_lines <- dplyr::tibble(
    text = strsplit(code_text, "\n+") |> unlist()
  )
  code_lines$text_id <- seq_along(code_lines$text)

  abs_lines <- search_text(
    code_lines,
    absolute_path_pattern,
    perl = TRUE
  )
  abs_paths <- search_text(
    abs_lines,
    absolute_path_pattern,
    perl = TRUE,
    return = "match"
  ) |>
    dplyr::select(abs_path = text, line = text_id)

  abs_paths$abs_path <- abs_paths$abs_path  |>
    gsub("^[\"']", "", x = _) |>
    gsub("[\"']$", "", x = _)

  return(abs_paths)
}


#' Remove comments from code text
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata)
#'
#' @returns the code_text minus comment lines
#' @export
#'
#' @examples
#' code_text <- c(
#'   "# this is a comment",
#'   "",
#'   "x <- 'And this is code'"
#' )
#' code_text_nc <- code_remove_comments(code_text, "R")
code_remove_comments <- function(code_text, lang = c("R", "SPSS", "SAS", "Stata")) {
  lang <- match.arg(lang)
  in_block <- FALSE
  code_text <- strsplit(code_text, "\n+") |> unlist()
  code_text_nc <- character(0)

  if (lang == "R") {
    code_text_nc <- grep("^(\\s*$|\\s*#|```\\s*\\{r)",
                         code_text, invert = TRUE, value = TRUE)
    # code_text_nc <- grep("knitr::", file_nc, invert = TRUE, value = TRUE)
  } else if (lang == "SAS") {
    for (ln in seq_along(code_text)) {
      L <- code_text[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*.*;\\s*$", L)
      if (!in_block && !line_comment) code_text_nc <- c(code_text_nc, L)
      if (in_block && ends_block) in_block <- FALSE
    }
  } else if (lang == "SPSS") {
    for (ln in seq_along(code_text)) {
      L <- code_text[ln]
      starts_block <- grepl("/\\*|COMMENT BEGIN", L)
      ends_block <- grepl("\\*/|COMMENT END\\.", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*(\\*|COMMENT)", L)
      if (!in_block && !line_comment) code_text_nc <- c(code_text_nc, L)
      if (in_block && ends_block) in_block <- FALSE
    }
  } else if (lang == "Stata") {
    for (ln in seq_along(code_text)) {
      L <- code_text[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      if (!in_block && !line_comment) {
        if (grepl("//", L)) L <- sub("//.*$", "", L) # strip end-of-line comments
          code_text_nc <- c(code_text_nc, L)
      }
      if (in_block && ends_block) in_block <- FALSE
    }
  } else {
    code_text_nc <- code_text
  }

  return(code_text_nc)
}

#' Get Code Composition Stats
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata)
#'
#' @returns list with items `total_lines`, `comment_lines`, `code_lines`, and `percent_comment`
#' @export
#'
#' @examples
#' code_text <- c(
#'   "library(dplyr)",
#'   "",
#'   "# this line is a comment",
#'   "a <- 1"
#' )
#' code_line_stats(code_text, "R")
code_line_stats <- function(code_text, lang = c("R", "SPSS", "SAS", "Stata")) {
  lang <- match.arg(lang)
  code_text <- strsplit(code_text, "\n+") |> unlist()

  total_lines <- length(code_text)
  blank_lines <- sum(trimws(code_text) == "")
  code_lines <- code_remove_comments(code_text, lang) |> length()
  comment_lines <- total_lines - blank_lines - code_lines

  percent_comments <- if (total_lines > 0) (comment_lines / total_lines) else NA_real_

  return(list(
    total_lines = total_lines,
    comment_lines = comment_lines,
    code_lines = code_lines,
    percent_comments = percent_comments
  ))
}

#' Get Code Library Lines
#'
#' Returns the lines on which library/require calls exist. This is a helper function for the code_check module.
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata)
#'
#' @returns a data frame with columns `code` and `line` (the line numbers on which library calls exist, after removing blank lines and comments)
#' @export
#'
#' @examples
#' code_text <- c(
#'   "library(dplyr)",
#'   "",
#'   "# this line won't count",
#'   "library(tidyr)",
#'   "renv::install('metacheck')"
#' )
#' code_library_lines(code_text, "R")
code_library_lines <- function(code_text, lang = c("R", "SPSS", "SAS", "Stata")) {
  lang <- match.arg(lang)

  # set up data frame
  code_text <- code_remove_comments(code_text, lang)
  df <- dplyr::tibble(
    code = code_text,
    line = seq_along(code_text)
  )

  # Language-specific regexes for imports and data loads
  lang_import_regex <- list(
    R     = "^[^#]*\\b(library|require|renv::install|p_load)\\s*\\(",
    SAS   = "\\b(%include|libname|filename|options)\\b",
    SPSS  = "\\b(INSERT|BEGIN\\s+PROGRAM|SET)\\b",
    Stata = "\\b(do|run|cd|adopath|net\\s+install|ssc\\s+install)\\b"
  )

  lines <- search_text(df, lang_import_regex[[lang]], perl = TRUE)[, c("code", "line")]

  return(lines)
}

#' Get files referenced in code
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata)
#'
#' @returns a vector of files that are referenced in the code
#' @export
#'
#' @examples
#' code_text <- c(
#'   'source("functions.R")',
#'   'a <- "bread"',
#'   'b <- read.csv("file.csv")'
#' )
#' code_file_refs(code_text, "R")
#'
code_file_refs <- function(code_text,
                           lang = c("R", "SPSS", "SAS", "Stata")) {
  lang <- match.arg(lang)
  code_text <- code_remove_comments(code_text, lang)

  # Examine files loaded, but missing in repo
  lang_load_regex <- list(
    R = c(
      "read[\\._][A-Za-z\\._0-9]+", # generic read.* or read_*
      # "read\\.(csv2?|table|delim2?)",
      # "read\\.xlsx",
      # "read\\.dta",
      # "read_(csv2?|tsv|delim|rds|lines)",
      # "read_(xlsx?|excel)",
      # "read_(dta|sav|sas)",
      # "read_(feather|parquet|yaml|xml|ods)",
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
    SPSS = c("\\/?FILE",
             "FILE\\s+HANDLE\\s+.+\\s+\\/NAME",
             "GET\\s+SAS\\s+DATA") |>
      paste(collapse = "|") |>
      paste0("\\b(", x = _, ")\\s*="),
    Stata = "\\b(use|import\\s+delimited|insheet|merge|append)\\b"
  )
  grepl_load <- lang_load_regex[[lang]]
  load_lines <- grep(grepl_load, code_text, value = TRUE, perl = TRUE)

  # Quoted filenames
  quoted_filename_pattern <- "(['\"])(?!\\.\\1)[^'\"]+\\.[A-Za-z0-9]{1,8}(?:\\.[A-Za-z0-9]{1,8})*\\1"

  loaded_file <- regmatches(
    load_lines,
    gregexpr(quoted_filename_pattern, load_lines, perl = TRUE)
  ) |>
    unlist() |>
    gsub("^['\"]|['\"]$", "", x = _)

  # Unquoted captures (language-specific)
  lang_unquoted_captures <- list(
    R = list(), # quoted captures suffice
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
  for (cap in caps) {
    m <- regexec(cap$regex, load_lines, perl = TRUE)
    reg <- regmatches(load_lines, m)
    if (length(reg) > 0) {
      vals <- vapply(reg, function(x) if (length(x) >= cap$group + 1) x[cap$group + 1] else NA_character_, character(1))
      extra <- c(extra, vals)
    }
  }
  extra <- extra[!is.na(extra)] |>
    gsub("^[\"']", "", x = _) |>
    gsub("[\"']$", "", x = _)
  loaded_file <- c(loaded_file, extra) |> unique()

  return(loaded_file)
}
