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
#' Detects code language used in files, only for languages metacheck currently processes (R, SAS, SPSS, Stata, Mplus).
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
  if (grepl("\\.inp$", lname)) {
    return("Mplus")
  }
  # A .jasp / .omv bundles a dataset with its analyses. It is a binary (zip)
  # archive, so none of the text-based checks below apply; it is listed, not
  # analysed. (import_jasp()/import_omv() recover the analysis syntax separately.)
  if (grepl("\\.jasp$", lname)) {
    return("JASP")
  }
  if (grepl("\\.omv$", lname)) {
    return("jamovi")
  }
  # ".m" is MATLAB source (Octave runs the same syntax). It also names
  # Objective-C source elsewhere in the world, but a psychology-paper code
  # repository overwhelmingly means MATLAB, and metacheck has no separate
  # Objective-C handling to confuse it with.
  if (grepl("\\.m$", lname)) {
    return("MATLAB")
  }
  return(NA_character_)
}

# For every ".spv" file in `all_files`, download it (reusing the same
# download_repo_files() path/cache/size-cap options code_check() already
# uses for .sps/.R/etc.), recover its embedded SPSS syntax as a sibling
# ".sps" file (.spv_export_syntax(), R/spv.R), and append ONE synthetic
# row per recovered ".sps" to the returned data.frame. That row then flows
# through the REST of code_check() completely unmodified: code_lang()
# already maps ".sps" -> "SPSS", so the recovered syntax is read, checked for
# comments/absolute paths/library lines, etc. exactly like any author-saved
# .sps file would be -- a .spv itself is still never checked as code (it
# stays classed data_type = "output"; see .data_check_types()), only the
# syntax RECOVERED from it becomes a checked code file.
#
# Files that fail to download, don't decode, or have no recoverable syntax
# are silently skipped (no row added) -- an .spv with no usable syntax is not
# an error, since most of its content is legitimately just rendered tables.
.code_expand_spv <- function(all_files, max_file_size, max_download_size, cache) {
  is_spv <- grepl("\\.spv$", all_files$file_name, ignore.case = TRUE)
  if (!any(is_spv)) return(all_files)

  spv_files <- all_files[is_spv, , drop = FALSE]
  need_dl <- is.na(spv_files$file_location) | !nzchar(spv_files$file_location %||% "")
  if (any(need_dl) && "file_url" %in% names(spv_files)) {
    dl <- tryCatch(
      download_repo_files(spv_files[need_dl, , drop = FALSE],
                          max_file_size = max_file_size,
                          max_download_size = max_download_size, cache = cache),
      error = function(e) NULL)
    if (!is.null(dl)) spv_files$file_location[need_dl] <- dl$file_location
  }

  new_rows <- list()
  for (i in seq_len(nrow(spv_files))) {
    loc <- spv_files$file_location[i]
    if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) next
    sps_path <- tryCatch(.spv_export_syntax(loc), error = function(e) NA_character_)
    if (is.na(sps_path)) next

    row <- spv_files[i, , drop = FALSE]
    row$file_name <- basename(sps_path)
    row$file_path <- file.path(dirname(spv_files$file_path[i] %||% spv_files$file_name[i]),
                               "code", basename(sps_path))
    row$file_location <- sps_path
    row$file_url <- NA_character_
    row$file_size <- file.size(sps_path)
    new_rows[[length(new_rows) + 1L]] <- row
  }
  if (!length(new_rows)) return(all_files)
  dplyr::bind_rows(all_files, new_rows)
}

# For every ".smcl" file in `all_files`, download it (reusing the same
# download_repo_files() path/cache/size-cap options code_check() already
# uses for .sps/.R/etc.), recover its echoed Stata syntax as a sibling
# ".do" file (.smcl_export_syntax(), R/stata.R), and append ONE synthetic
# row per recovered ".do" to the returned data.frame. That row then flows
# through the REST of code_check() completely unmodified: code_lang()
# already maps ".do" -> "Stata", so the recovered syntax is read, checked
# for comments/absolute paths/library lines, etc. exactly like any
# author-saved .do file would be -- a .smcl itself is still never checked
# as code (it stays classed data_type = "output"; see
# .data_check_types()), only the syntax RECOVERED from it becomes a
# checked code file. Mirrors .code_expand_spv() exactly; the one real
# difference is that a .smcl's command echo IS its own verbatim syntax (no
# separate structure element to recover it from, unlike .spv).
#
# Files that fail to download, don't parse, or have no recoverable syntax
# are silently skipped (no row added) -- a .smcl with no usable syntax is
# not an error, since most of its content is legitimately just rendered
# tables and log bookkeeping.
.code_expand_smcl <- function(all_files, max_file_size, max_download_size, cache) {
  is_smcl <- grepl("\\.smcl$", all_files$file_name, ignore.case = TRUE)
  if (!any(is_smcl)) return(all_files)

  smcl_files <- all_files[is_smcl, , drop = FALSE]
  need_dl <- is.na(smcl_files$file_location) | !nzchar(smcl_files$file_location %||% "")
  if (any(need_dl) && "file_url" %in% names(smcl_files)) {
    dl <- tryCatch(
      download_repo_files(smcl_files[need_dl, , drop = FALSE],
                          max_file_size = max_file_size,
                          max_download_size = max_download_size, cache = cache),
      error = function(e) NULL)
    if (!is.null(dl)) smcl_files$file_location[need_dl] <- dl$file_location
  }

  new_rows <- list()
  for (i in seq_len(nrow(smcl_files))) {
    loc <- smcl_files$file_location[i]
    if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) next
    do_path <- tryCatch(.smcl_export_syntax(loc), error = function(e) NA_character_)
    if (is.na(do_path)) next

    row <- smcl_files[i, , drop = FALSE]
    row$file_name <- basename(do_path)
    row$file_path <- file.path(dirname(smcl_files$file_path[i] %||% smcl_files$file_name[i]),
                               "code", basename(do_path))
    row$file_location <- do_path
    row$file_url <- NA_character_
    row$file_size <- file.size(do_path)
    new_rows[[length(new_rows) + 1L]] <- row
  }
  if (!length(new_rows)) return(all_files)
  dplyr::bind_rows(all_files, new_rows)
}

# For every ".out" file in `all_files`, download it (reusing the same
# download_repo_files() path/cache/size-cap options code_check() already
# uses for .sps/.R/etc.), recover its own verbatim "INPUT INSTRUCTIONS"
# block as a sibling ".inp" file (.mplus_export_syntax(), R/mplus.R), and
# append ONE synthetic row per recovered ".inp" to the returned data.frame.
# Mirrors .code_expand_spv()/.code_expand_smcl() exactly; like .smcl (and
# unlike .spv), a .out's syntax is already verbatim text sitting right in
# the file, not something decoded from a separate binary structure. A .out
# itself is still never checked as code (it stays classed data_type =
# "output"; see .fixed_ext_type in R/data_check_helpers.R), only the syntax
# RECOVERED from it becomes a checked code file.
#
# Files that fail to download, don't parse, or have no recoverable syntax
# are silently skipped (no row added) -- should not happen for a genuine
# Mplus .out (INPUT INSTRUCTIONS is always present), but a malformed or
# truncated download is not an error worth surfacing here.
.code_expand_mplus <- function(all_files, max_file_size, max_download_size, cache) {
  is_out <- grepl("\\.out$", all_files$file_name, ignore.case = TRUE)
  if (!any(is_out)) return(all_files)

  out_files <- all_files[is_out, , drop = FALSE]
  need_dl <- is.na(out_files$file_location) | !nzchar(out_files$file_location %||% "")
  if (any(need_dl) && "file_url" %in% names(out_files)) {
    dl <- tryCatch(
      download_repo_files(out_files[need_dl, , drop = FALSE],
                          max_file_size = max_file_size,
                          max_download_size = max_download_size, cache = cache),
      error = function(e) NULL)
    if (!is.null(dl)) out_files$file_location[need_dl] <- dl$file_location
  }

  new_rows <- list()
  for (i in seq_len(nrow(out_files))) {
    loc <- out_files$file_location[i]
    if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) next
    inp_path <- tryCatch(.mplus_export_syntax(loc), error = function(e) NA_character_)
    if (is.na(inp_path)) next

    row <- out_files[i, , drop = FALSE]
    row$file_name <- basename(inp_path)
    row$file_path <- file.path(dirname(out_files$file_path[i] %||% out_files$file_name[i]),
                               "code", basename(inp_path))
    row$file_location <- inp_path
    row$file_url <- NA_character_
    row$file_size <- file.size(inp_path)
    new_rows[[length(new_rows) + 1L]] <- row
  }
  if (!length(new_rows)) return(all_files)
  dplyr::bind_rows(all_files, new_rows)
}

# For every ".html" file in `all_files`, download it (reusing the same
# download_repo_files() path/cache/size-cap options code_check() already uses
# for .sps/.R/etc.) and content-sniff it (.html_sniff_kind(), R/html-output.R)
# to tell rendered R Markdown / Quarto (pandoc/knitr) analysis output apart
# from a Stata log translated to HTML, a jsPsych/psiTurk task-runner page, or
# a project documentation site — unlike .spv/.smcl/.out, ".html" carries NO
# format-locked signal from its extension alone (see R/html-output.R's file
# header), so every candidate is downloaded and checked rather than only ones
# already classified some other way.
#
# For the "rmd" kind, the ORIGINAL R source sits verbatim inside the rendered
# document and is recovered as a sibling ".R" file (.html_export_r_source(),
# R/html-output.R) — the same "recover a checkable code file" idea as
# .code_expand_spv()/.code_expand_smcl()/.code_expand_mplus(). For "stata",
# NO recovery is attempted (the real markup has not been confirmed against a
# genuine example; see R/html-output.R) — only the .html itself is
# reclassified data_type = "output" downstream (R/data_check_helpers.R), same
# as any other detected output. An .html that sniffs as neither kind is left
# completely alone: no row is added, and its classification is whatever
# data_classify_files() already gave it (see the name-based "output" rule
# there for the OTHER trigger — a filename literally containing "output" —
# which does not require downloading/sniffing at all).
.code_expand_html <- function(all_files, max_file_size, max_download_size, cache) {
  is_html <- grepl("\\.html?$", all_files$file_name, ignore.case = TRUE)
  if (!any(is_html)) return(all_files)

  html_files <- all_files[is_html, , drop = FALSE]
  need_dl <- is.na(html_files$file_location) | !nzchar(html_files$file_location %||% "")
  if (any(need_dl) && "file_url" %in% names(html_files)) {
    dl <- tryCatch(
      download_repo_files(html_files[need_dl, , drop = FALSE],
                          max_file_size = max_file_size,
                          max_download_size = max_download_size, cache = cache),
      error = function(e) NULL)
    if (!is.null(dl)) html_files$file_location[need_dl] <- dl$file_location
  }

  new_rows <- list()
  html_kind <- rep(NA_character_, nrow(html_files))
  for (i in seq_len(nrow(html_files))) {
    loc <- html_files$file_location[i]
    if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) next
    html_kind[i] <- tryCatch(.html_sniff_kind(loc), error = function(e) NA_character_)
    if (is.na(html_kind[i]) || html_kind[i] != "rmd") next

    r_path <- tryCatch(.html_export_r_source(loc), error = function(e) NA_character_)
    if (is.na(r_path)) next

    row <- html_files[i, , drop = FALSE]
    row$file_name <- basename(r_path)
    row$file_path <- file.path(dirname(html_files$file_path[i] %||% html_files$file_name[i]),
                               "code", basename(r_path))
    row$file_location <- r_path
    row$file_url <- NA_character_
    row$file_size <- file.size(r_path)
    new_rows[[length(new_rows) + 1L]] <- row
  }

  # Reclassify each ORIGINAL .html row's data_type from its sniffed kind, when
  # the table already carries that column (repo_check's table does not; a
  # caller that ran data_check first, and so has data_type, gets it updated
  # here too rather than only on the synthetic .R row's own — absent —
  # classification). Both "rmd" and "stata" mean "this IS rendered output".
  if ("data_type" %in% names(all_files)) {
    sniffed <- !is.na(html_kind)
    if (any(sniffed))
      all_files$data_type[which(is_html)[sniffed]] <- "output"
  }

  if (!length(new_rows)) return(all_files)
  dplyr::bind_rows(all_files, new_rows)
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
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata,
#'   Mplus, MATLAB)
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
code_remove_comments <- function(code_text, lang = c("R", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
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
  } else if (lang == "Mplus") {
    # Mplus syntax comment is "!" to end of line; no block-comment syntax.
    code_text_nc <- code_text[!grepl("^\\s*!", code_text)]
    code_text_nc <- sub("!.*$", "", code_text_nc)
  } else if (lang == "MATLAB") {
    # MATLAB line comments are "%" to end of line; block comments are "%{"/"%}",
    # each valid ONLY when alone on its own line (whitespace aside) -- a "%{"
    # elsewhere on a line (e.g. inside a string, or as part of an expression)
    # is not a block-comment start, unlike SAS/Stata's "/*" which is recognised
    # anywhere on the line.
    for (ln in seq_along(code_text)) {
      L <- code_text[ln]
      starts_block <- grepl("^\\s*%\\{\\s*$", L)
      ends_block   <- grepl("^\\s*%\\}\\s*$", L)
      if (!in_block && starts_block) { in_block <- TRUE; next }
      if (in_block) { if (ends_block) in_block <- FALSE; next }
      if (grepl("^\\s*%", L)) next   # whole-line comment
      # Strip a trailing "%..." comment. Like the Stata "//" branch above, this
      # does not know about string literals, so a literal "%" inside a quoted
      # string (e.g. disp('100%')) is also truncated -- a pre-existing class of
      # limitation in this function, not new here.
      code_text_nc <- c(code_text_nc, sub("%.*$", "", L))
    }
  } else {
    code_text_nc <- code_text
  }

  return(code_text_nc)
}

#' Get Code Composition Stats
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata,
#'   Mplus, MATLAB)
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
code_line_stats <- function(code_text, lang = c("R", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
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
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata,
#'   Mplus, MATLAB)
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
code_library_lines <- function(code_text, lang = c("R", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
  lang <- match.arg(lang)

  # set up data frame
  code_text <- code_remove_comments(code_text, lang)
  df <- dplyr::tibble(
    code = code_text,
    line = seq_along(code_text)
  )

  # Language-specific regexes for imports and data loads. Mplus has no
  # library/import concept at all (like SAS/SPSS/Stata) -- its closest
  # analogue is the DATA: command's own file reference, which
  # code_file_refs() already extracts separately; matched against nothing
  # here (this returns 0 rows for every Mplus file, by design, the same way
  # code_library_names() below returns an empty frame for it).
  #
  # MATLAB has no package-manager concept either, but addpath()/toolboxdir()
  # calls play the same role library()/require() do in R: bringing in code
  # (local function files, or a licensed toolbox) not defined in the script
  # itself -- the closest MATLAB analogue this check has, so it is what is
  # matched here (mirroring code_library_names() below, which reports these
  # same calls' argument as the "package" name).
  lang_import_regex <- list(
    R      = "^[^#]*\\b(library|require|renv::install|p_load)\\s*\\(",
    SAS    = "\\b(%include|libname|filename|options)\\b",
    SPSS   = "\\b(INSERT|BEGIN\\s+PROGRAM|SET)\\b",
    Stata  = "\\b(do|run|cd|adopath|net\\s+install|ssc\\s+install)\\b",
    Mplus  = "(?!)",
    MATLAB = "\\b(addpath|import)\\s*\\("
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
#' @param lang the language (R, Python, SPSS, SAS, Stata, Mplus, MATLAB)
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
                               lang = c("R", "Python", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
  lang <- match.arg(lang)
  empty <- data.frame(package = character(0), source = character(0),
                      line = integer(0))

  # SAS / SPSS / Stata / Mplus / MATLAB: no INSTALLABLE package concept to
  # extract (no CRAN/PyPI-style registry a requirements.txt could name).
  # MATLAB's addpath()/import() name a local FOLDER or fully-qualified
  # function, not an installable identifier -- already reported separately as
  # a "library-line" by code_library_lines() above, so it is not duplicated
  # here as a "package".
  if (lang %in% c("SPSS", "SAS", "Stata", "Mplus", "MATLAB")) return(empty)

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
#' @param lang the language (we only currently handle R, SPSS, SAS, Stata,
#'   Mplus, MATLAB)
#' @param include_writes also return files the code *writes* (R only). Off by
#'   default: callers that ask "which referenced inputs are missing from the
#'   repository?" (e.g. `code_check`) must not see a written file as a missing
#'   input. [repro_file_io()] turns this on so a file produced by one script can
#'   be recognised as the input another script consumes.
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
                           lang = c("R", "SPSS", "SAS", "Stata", "Mplus", "MATLAB"),
                           include_writes = FALSE) {
  lang <- match.arg(lang)
  code_text <- code_remove_comments(code_text, lang)

  # Examine files loaded, but missing in repo
  lang_load_regex <- list(
    R = c(
      # Generic read.* / read_*, which covers read.csv, read.spss, read_excel,
      # read_ods, read_sav, read_parquet, ... — i.e. nearly every reader in the
      # R ecosystem, whatever the file format. (An earlier version enumerated
      # each reader by name; those lines were redundant once this pattern went
      # in, so they are gone rather than left commented out.)
      "read[\\._][A-Za-z\\._0-9]+",
      # rio's readers are the notable loaders that do NOT begin with "read":
      # import() dispatches on the file extension, so a script can load .csv,
      # .xlsx, .ods or .sav through it. Without this, files loaded via rio were
      # invisible to the missing-file check below and a repository that had
      # simply failed to upload its data still reported "all files present".
      # Matching is safe because a hit only yields a reference when the call
      # also contains a QUOTED string with a file extension: the Python-bridge
      # sense, reticulate::import("numpy"), captures nothing.
      "import(_list)?",
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
    Stata = "\\b(use|import\\s+delimited|insheet|merge|append)\\b",
    # Mplus's DATA: command names its input file with "FILE = ...;" (seen
    # verbatim in every real corpus file's INPUT INSTRUCTIONS, e.g.
    # `FILE = "model_admir_child_....dat";`).
    Mplus = "\\bFILE\\s*=",
    # MATLAB's common data-reading functions, plus low-level fopen() and the
    # script/function-calling `run` -- MATLAB has no source()-equivalent for
    # code (a .m on the path is just called by name, no explicit "load this
    # file" statement), so `run` here is the closest analogue: it DOES take an
    # explicit script path, unlike an ordinary function call.
    MATLAB = c("load", "csvread", "dlmread", "readtable", "readmatrix",
              "readcell", "xlsread", "importdata", "fopen", "run") |>
      paste(collapse = "|") |>
      paste0("\\b(", x = _, ")\\s*\\(")
  )
  grepl_load <- lang_load_regex[[lang]]

  # Write calls are *not* part of the default reference set: a written file is an
  # output, and the main consumer (code_check) reports referenced files that are
  # absent from the repository — an output would be a false "missing input"
  # there. Reproducibility ordering does need them, so they are opt-in.
  if (isTRUE(include_writes) && identical(lang, "R")) {
    write_call_regex <- c(
      "write[\\._][A-Za-z\\._0-9]*", # write.csv, write_csv, write.table, ...
      "saveRDS",
      "save",                        # save(), and save.image() via the \\.? below
      "save\\.image",
      "ggsave",
      "export",
      "fwrite"
    ) |>
      paste(collapse = "|") |>
      paste0("\\b(", x = _, ")\\s*\\(")
    grepl_load <- paste0(grepl_load, "|", write_call_regex)
  }

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
    ),
    Mplus = list() # FILE = "..." is always quoted; quoted-filename pattern suffices
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
