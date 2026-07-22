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
  # A .jasp / .omv bundles a dataset with its analyses. It is a binary (zip)
  # archive, so none of the text-based checks below apply; it is listed, not
  # analysed. (read_jasp()/read_omv() recover the analysis syntax separately.)
  if (grepl("\\.jasp$", lname)) {
    return("JASP")
  }
  if (grepl("\\.omv$", lname)) {
    return("jamovi")
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

  # A quoted string that is an absolute filesystem path. The UNC branch is
  # deliberately strict: a bare backslash string is far more often a regex
  # escape ("\d+", "\1-\2", "\-A[MF12]+") than a network path, so we require the
  # real UNC shape \\host\share — two leading backslashes, a host name
  # (letters/digits/dots/hyphens), then a single backslash and a share — which
  # regex escapes do not have (a regex has a metacharacter/digit right after the
  # backslashes, not host\share). In a PCRE pattern each literal backslash is
  # written "\\", so the on-disk bytes \\host\ become "\\\\[host]\\" below.
  absolute_path_pattern <- paste0(
    "([\"'])", # start quote
    "(?:~/(?:[^\\n'\"]+)|", # e.g., ~/Desktop/...
    "/(?!/)[^\\n'\"]+|",    # e.g., /User/...
    "[A-Za-z]:[\\\\/][^\\n'\"]+|", # e.g., C:/... or D:\...
    "\\\\\\\\[A-Za-z0-9._-]+\\\\[^\\n'\"]+)", # UNC \\host\share\...
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


#' Find setwd() calls in code
#'
#' A `setwd()` call in analysis code is a portability problem: it hardcodes an
#' assumption about the working directory the code runs in (frequently an
#' absolute path on the author's own machine), mutates global state, and makes a
#' script depend on being run from a particular place. Best practice is to keep
#' the working directory as the caller sets it and use relative paths. This scans
#' the (comment-free) code for `setwd(...)` calls and returns one row per call,
#' with the argument as written. R-only (the construct is R's; other languages
#' have their own, e.g. Stata `cd`, which this does not scan).
#'
#' @param code_text the code text for a single file (character vector), ideally
#'   comment-free (as produced by [code_remove_comments()])
#'
#' @returns a data frame with columns `setwd_call` (the `setwd(...)` text as
#'   written) and `line` (its line number). Empty frame when none are found.
#' @export
#'
#' @examples
#' code_text <- c(
#'   "setwd('D:/Dropbox/project')",
#'   "x <- read.csv('data.csv')"
#' )
#' code_setwd(code_text)
code_setwd <- function(code_text) {
  text_id <- NULL # fix cmd check note
  # A setwd( call: the token at a call position, capturing to the LAST closing
  # paren on the line (greedy `.*`), so a nested argument like setwd(getwd()) or
  # setwd(dirname(path)) is shown in full. A setwd argument spanning lines is
  # rare and left to the simple line scan. Matched on the whole line so the
  # report can show the call as written.
  setwd_pattern <- "setwd\\s*\\(.*\\)"

  code_lines <- dplyr::tibble(
    text = strsplit(code_text, "\n+") |> unlist()
  )
  code_lines$text_id <- seq_along(code_lines$text)

  setwd_matches <- search_text(
    code_lines,
    setwd_pattern,
    perl = TRUE,
    return = "match"
  )
  if (nrow(setwd_matches) == 0)
    return(data.frame(setwd_call = character(0), line = integer(0)))

  dplyr::select(setwd_matches, setwd_call = text, line = text_id)
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

#' Get package names loaded in code
#'
#' Extracts the names of the packages/libraries a code file loads. Where
#' [code_library_lines()] only reports the lines on which imports occur (to check
#' they are grouped), this returns the actual package identifiers so they can be
#' catalogued and searched, or written to a `requirements.txt`.
#'
#' For R this captures `library()` / `require()` / `requireNamespace()` (bare or
#' quoted argument), pacman's `p_load()` (which may list several packages at
#' once), namespace-qualified calls (`pkg::fun` / `pkg:::fun`), and
#' `install.packages()` / `renv::install()`. For Python it captures `import a`,
#' `import a as b`, `import a, b`, and `from a.b import c` (recording the
#' top-level package `a`). SAS / SPSS / Stata have no package concept, so an
#' empty frame is returned.
#'
#' The extraction is regex-based (like the rest of the module) so it degrades
#' gracefully on files that will not parse. It can therefore miss dynamically
#' constructed package names, and does not know which *version* was used — only
#' the names appear in the source.
#'
#' @param code_text the code text for a single file
#' @param lang the language (R, Python, SPSS, SAS, Stata)
#'
#' @returns a data frame with columns `package`, `source` (how the package was
#'   referenced: `library`, `require`, `requireNamespace`, `p_load`, `namespace`,
#'   `install`, or `import`), and `line` (the line number). Rows are unique on
#'   `package` + `source` + `line`. An empty frame (same columns) when none found.
#' @export
#'
#' @examples
#' code_text <- c(
#'   "library(dplyr)",
#'   "require('tidyr')",
#'   "pacman::p_load(ggplot2, readr)",
#'   "x <- stringr::str_trim(' a ')"
#' )
#' code_library_names(code_text, "R")
code_library_names <- function(code_text,
                               lang = c("R", "Python", "SPSS", "SAS", "Stata")) {
  lang <- match.arg(lang)
  empty <- data.frame(package = character(0), source = character(0),
                      line = integer(0))

  # SAS / SPSS / Stata: no package/library concept to extract.
  if (lang %in% c("SPSS", "SAS", "Stata")) return(empty)

  # Strip comments. code_remove_comments() does not know Python, so handle its
  # `#` line comments directly; R (and R-in-Rmd/qmd) goes through the shared
  # helper for consistency with the rest of the module.
  if (lang == "Python") {
    code_text <- unlist(strsplit(code_text, "\n"))
    code_text <- sub("#.*$", "", code_text)
  } else {
    code_text <- code_remove_comments(code_text, lang)
  }
  if (length(code_text) == 0) return(empty)

  # A single record of (package, source, line); collected then row-bound.
  hits <- list()
  add <- function(pkgs, source, line) {
    pkgs <- trimws(gsub("^['\"]|['\"]$", "", pkgs))   # strip quotes
    pkgs <- pkgs[nzchar(pkgs)]
    if (length(pkgs) == 0) return(invisible())
    hits[[length(hits) + 1L]] <<- data.frame(
      package = pkgs, source = source, line = line)
  }

  # Pull the first capture group of `regex` out of one line, applied per line so
  # the originating line number is preserved.
  cap <- function(regex, line, i) {
    m <- regmatches(line, regexec(regex, line, perl = TRUE))[[1]]
    if (length(m) >= i + 1L && nzchar(m[i + 1L])) m[i + 1L] else NA_character_
  }

  for (ln in seq_along(code_text)) {
    L <- code_text[ln]

    if (lang == "R") {
      # library(x) / library("x") / require(x) / requireNamespace("x")
      for (fn in c("library", "require", "requireNamespace")) {
        g <- cap(sprintf("\\b%s\\s*\\(\\s*([A-Za-z0-9._'\"]+)", fn), L, 1)
        if (!is.na(g)) add(g, fn, ln)
      }
      # pacman::p_load(a, b, c) — one or more comma-separated packages
      pl <- cap("\\bp_load\\s*\\(([^)]*)\\)", L, 1)
      if (!is.na(pl)) add(strsplit(pl, "\\s*,\\s*")[[1]], "p_load", ln)
      # install.packages("x") / renv::install("x")
      for (fn in c("install\\.packages", "install")) {
        g <- cap(sprintf("\\b%s\\s*\\(\\s*(c\\()?\\s*([A-Za-z0-9._'\", ]+?)\\s*\\)",
                         fn), L, 2)
        if (!is.na(g)) add(strsplit(g, "\\s*,\\s*")[[1]], "install", ln)
      }
      # pkg::fun / pkg:::fun namespace-qualified calls (all on the line)
      ns <- regmatches(L, gregexpr("\\b([A-Za-z][A-Za-z0-9._]*):{2,3}", L,
                                   perl = TRUE))[[1]]
      if (length(ns) > 0) add(sub(":{2,3}$", "", ns), "namespace", ln)

    } else if (lang == "Python") {
      # from a.b import c  ->  top-level package a
      fr <- cap("^\\s*from\\s+([A-Za-z0-9_.]+)\\s+import\\b", L, 1)
      if (!is.na(fr)) add(sub("\\..*$", "", fr), "import", ln)
      # import a, b.c as d  ->  top-level a, b (drop "as alias" and submodules)
      im <- cap("^\\s*import\\s+(.+)$", L, 1)
      if (!is.na(im)) {
        parts <- strsplit(im, "\\s*,\\s*")[[1]]
        top <- sub("\\..*$", "", sub("\\s+as\\s+.*$", "", trimws(parts)))
        add(top, "import", ln)
      }
    }
  }

  if (length(hits) == 0) return(empty)
  out <- dplyr::bind_rows(hits)
  # Drop non-identifier captures (e.g. a stray operator) and de-duplicate.
  out <- out[grepl("^[A-Za-z][A-Za-z0-9._]*$", out$package), , drop = FALSE]
  unique(out)
}

#' Distinct packages from a code_check table
#'
#' The `code_check` module stores the packages each code file loads as a
#' comma-joined string in its `table$packages` column. This returns the sorted,
#' de-duplicated union across a set of those rows — the paper-level dependency
#' list used for the module summary, the manifest `code` section, and the
#' `requirements.txt` written into a Psych-DS archive by [convert_psychds()].
#'
#' @param packages a character vector of comma-joined package strings (e.g.
#'   `code_check(...)$table$packages`), or a `code_check` table (data frame with
#'   a `packages` column).
#'
#' @returns a sorted character vector of distinct package names (possibly empty)
#' @export
#'
#' @examples
#' code_packages(c("dplyr, ggplot2", "", "dplyr, tidyr"))
code_packages <- function(packages) {
  if (is.data.frame(packages)) packages <- packages$packages
  packages <- packages[!is.na(packages) & nzchar(packages)]
  if (length(packages) == 0) return(character(0))
  strsplit(paste(packages, collapse = ", "), "\\s*,\\s*")[[1]] |>
    unique() |> sort()
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
