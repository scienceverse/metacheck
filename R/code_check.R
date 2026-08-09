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


# The language a Jupyter notebook's code cells are written in.
#
# Jupyter is kernel-agnostic: the same ".ipynb" extension is used for Python,
# R, Julia, and others, and a psychology repository genuinely contains both
# (in a real 144-notebook corpus: 126 Python, 7 R). The kernel is declared
# INSIDE the file, in metadata$kernelspec$language (or, for older notebooks,
# metadata$language_info$name), so it is read rather than guessed. Every one
# of those 144 notebooks carried one of those two fields.
#
# Only the metadata is parsed, not the whole document: notebooks reach several
# MB once outputs are stored, and reading a 2.4MB JSON just to learn one word
# is wasteful when code_lang() is called on every file in a repository.
#
# Returns "Python" when the file cannot be read (a URL not yet downloaded, an
# unreadable path) or declares a language metacheck has no checks for --
# Python is both the most common kernel and the safe default, since a notebook
# that reached this function is at minimum a notebook.
#
# @param file_name path to a .ipynb file
# @returns "R" or "Python"
.ipynb_lang <- function(file_name) {
  if (length(file_name) != 1 || is.na(file_name) ||
      !file.exists(file_name)) return("Python")

  txt <- tryCatch(code_read(file_name), error = function(e) NULL)
  if (is.null(txt) || !length(txt)) return("Python")

  nb <- tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n"),
                                    simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(nb)) return("Python")

  lang <- nb$metadata$kernelspec$language %||%
          nb$metadata$language_info$name %||% ""
  lang <- tolower(as.character(lang)[[1]] %||% "")

  # "ir" is the IRkernel's NAME rather than its language, seen in the wild on
  # older notebooks whose kernelspec$language is absent.
  if (lang %in% c("r", "ir")) return("R")
  "Python"
}

# The language a Quarto document's code chunks are written in.
#
# Unlike .Rmd (an R Markdown document, R/knitr only by construction), .qmd is
# Quarto's own extension and is explicitly polyglot: the same rendering
# pipeline runs knitr (R) or Jupyter (Python, Julia, and other kernels)
# depending on what the document declares, mirroring .ipynb's own
# kernel-agnosticism (see .ipynb_lang() above) -- but unlike .ipynb, .qmd's
# language was previously assumed to be "R" unconditionally via
# .ext_registry's format lock (issue #180), so a Python-engine .qmd was
# purled with code_extract_r()/knitr::purl(), which only recovers ```{r}
# chunks and silently produced an empty or garbage R file for a paper that
# never ran any R at all.
#
# No real corpus example of a non-R .qmd was found in the local repo cache
# (every cached .qmd is R/knitr) -- this follows Quarto's own documented
# language-selection rules rather than a corpus-confirmed case the way
# .ipynb_lang() is:
#   1. YAML front matter `engine: jupyter` + a `jupyter:` kernel/kernelspec
#      field naming the language (e.g. `jupyter: python3`,
#      `jupyter: {kernelspec: {language: python}}`) -- checked first, since
#      it is the explicit, authoritative declaration when present.
#   2. Absent that, the language of the FIRST executable chunk fence
#      (```{python}, ```{r}, ```{julia}, ...) -- what Quarto itself infers
#      the primary engine from when no YAML engine is declared.
#   3. Absent both, "R": .qmd is RMarkdown's direct successor and the
#       overwhelming majority of real documents (and every cached example)
#      are knitr/R, so this stays the safe default -- also preserves the
#      existing code_lang() unit test, which checks "file.qmd" (a file that
#      does not exist on disk, so cannot be content-sniffed) and expects "R".
#
# Only R and Python are returned: those are the two languages metacheck's
# code_check pipeline has extractors for (code_extract_r() / code_extract_py()).
# A Julia/Observable-JS chunk fence is recognised as "not R" but reported as
# "R" (the fallback), since there is no metacheck check for those languages
# either way and misreporting the extractor to run is worse than running the
# (wrong) default one silently -- same trade-off .ipynb_lang() makes by
# collapsing every non-R kernel to "Python".
#
# @param file_name path to a .qmd file
# @returns "R" or "Python"
.qmd_lang <- function(file_name) {
  if (length(file_name) != 1 || is.na(file_name) ||
      !file.exists(file_name)) return("R")

  txt <- tryCatch(code_read(file_name), error = function(e) NULL)
  if (is.null(txt) || !length(txt)) return("R")

  # YAML front matter is the block between the first "---" line and the next
  # "---" line. Quarto (like R Markdown) requires it to open the file, so it
  # is always the first fence if present at all.
  yaml_lang <- NA_character_
  if (grepl("^---\\s*$", txt[[1]])) {
    end <- which(grepl("^---\\s*$", txt[-1]))[1]
    if (!is.na(end)) {
      front_matter <- txt[2:end]
      yaml <- tryCatch(yaml::yaml.load(paste(front_matter, collapse = "\n")),
                       error = function(e) NULL)
      jupyter <- yaml$jupyter
      # `jupyter:` may be a bare kernel name ("python3") or a list with its
      # own kernelspec$language (the same shape .ipynb stores under
      # metadata$kernelspec$language).
      if (is.list(jupyter)) {
        yaml_lang <- jupyter$kernelspec$language %||% jupyter$language %||% NA_character_
      } else if (is.character(jupyter)) {
        yaml_lang <- jupyter
      }
      yaml_lang <- tolower(as.character(yaml_lang)[[1]] %||% NA_character_)
    }
  }
  if (!is.na(yaml_lang)) {
    # Bare Jupyter kernel names ("python3", "ir") as well as language names
    # ("python", "r") are both seen in the wild -- normalise both forms.
    if (grepl("^py", yaml_lang)) return("Python")
    if (yaml_lang %in% c("r", "ir")) return("R")
  }

  # No (or unrecognised) YAML engine: fall back to the first executable
  # chunk fence, e.g. "```{python}" or "```{python 3}" (chunk options after
  # the language are allowed, hence no closing brace in the pattern).
  chunk <- grep("^```+\\s*\\{[a-zA-Z]+", txt, value = TRUE, perl = TRUE)[1]
  if (!is.na(chunk)) {
    chunk_lang <- regmatches(chunk, regexpr("(?<=\\{)[a-zA-Z]+", chunk, perl = TRUE))
    chunk_lang <- tolower(chunk_lang)
    if (length(chunk_lang) && grepl("^py", chunk_lang)) return("Python")
  }

  "R"
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

  ext <- tolower(tools::file_ext(file_name))
  # ".ipynb" is a Jupyter notebook: JSON holding source in its code cells,
  # recovered by code_extract_py()/code_extract_r() the way an .Rmd/.qmd is
  # purled. Jupyter runs MANY kernels, so the extension alone does NOT fix the
  # language -- an .ipynb is as often R (the IRkernel) as Python in a
  # psychology repository. Confirmed against a real corpus: of 144 cached
  # notebooks, 126 declared Python and 7 declared R, every one of them via
  # metadata INSIDE the file. Two of those R notebooks (OSF s83qn) opened with
  # `library(rethinking)`, and calling them Python found zero packages in a
  # file that plainly loads five.
  #
  # Extension alone therefore only gets us "a notebook" (.ext_registry's own
  # code_lang column is NA for "ipynb" for exactly this reason);
  # .ipynb_lang() reads the declared kernel from the file when it is
  # available locally. This mirrors how a ".out" is classified by extension
  # and then CORRECTED by content once downloaded (see .ext_registry,
  # R/data_check_helpers.R) -- name first, content decides. Python is the
  # fallback when the file cannot be read (a URL, not yet downloaded), since
  # it is by far the more common kernel.
  if (ext == "ipynb") return(.ipynb_lang(file_name))

  # ".qmd" is Quarto, and (unlike ".Rmd") explicitly polyglot -- the extension
  # alone only gets us "a Quarto document", the same way ".ipynb" only gets us
  # "a notebook" above. .ext_registry's own code_lang column is NA for "qmd"
  # for exactly this reason; .qmd_lang() reads the declared/inferred engine
  # from the file when it is available locally (issue #180).
  if (ext == "qmd") return(.qmd_lang(file_name))

  lang <- .ext_registry$code_lang[match(ext, .ext_registry$ext)]
  if (is.na(lang)) NA_character_ else lang
}

# code_check() previously called download_repo_files() up to five separate
# times: once each inside .code_expand_spv()/.code_expand_smcl()/
# .code_expand_mplus()/.code_expand_html() (each downloading only its own
# extension's candidate rows), then again for the main `checked_files` set.
# Every one of those calls hits the same on-disk cache keyed by
# repo_url/file_path, so back-to-back calls do not re-download shared files --
# but each is still a separate function call, a separate cap/gating pass, and
# (for a repo close to its max_download_size budget) an ordering-dependent
# partial fill, since download_repo_files() spends a repo's remaining budget
# on whichever candidate set it sees first.
#
# .code_predownload() removes that ordering dependence and the redundant call
# overhead: it unions the candidate rows for all five downstream steps (spv,
# smcl, mplus, html, and every "checked" language) BEFORE any of them run,
# and downloads that union in ONE download_repo_files() call. Each of the
# four .code_expand_*() functions, and the main download step in
# inst/modules/code_check.R, keep their own `need_dl` check afterwards -- with
# file_location already populated here, that check simply finds nothing left
# to do and is a no-op, so their standalone behaviour (including being
# callable/testable on their own, outside code_check()) is unchanged.
.code_predownload <- function(all_files, max_file_size, max_download_size, cache) {
  # spv/smcl/out are output-typed formats whose embedded syntax the
  # .code_expand_*() steps below recover as a sibling code file (see their own
  # comments); html/htm is not in .ext_registry at all (an .html's data_check
  # type is decided by keyword/content, never format-locked), so it stays an
  # explicit extension check here rather than a registry lookup. The language
  # set is every code_lang code_lang() can return, read from the registry
  # (plus "Python", which .ipynb_lang()'s content-sniff can also produce, and
  # "JASP"/"jamovi", which are archives never downloaded as checked CODE by
  # code_check() -- excluded here on purpose, same as before this refactor).
  code_langs <- setdiff(unique(stats::na.omit(.ext_registry$code_lang)),
                        c("JASP", "jamovi"))
  is_candidate <- grepl("\\.(spv|smcl|out|html?)$", all_files$file_name, ignore.case = TRUE) |
    all_files$language %in% union(code_langs, "Python")
  need_dl <- is_candidate &
    (is.na(all_files$file_location) | !nzchar(all_files$file_location %||% "")) &
    !is.na(all_files$file_url) & nzchar(all_files$file_url %||% "")
  if (!any(need_dl)) return(all_files)

  dl <- tryCatch(
    download_repo_files(all_files[need_dl, , drop = FALSE],
                        max_file_size = max_file_size,
                        max_download_size = max_download_size, cache = cache),
    error = function(e) NULL)
  if (!is.null(dl)) all_files$file_location[need_dl] <- dl$file_location
  attr(all_files, "gated") <- attr(dl, "gated")
  attr(all_files, "oversize_skipped") <- attr(dl, "oversize_skipped")
  attr(all_files, "failed") <- attr(dl, "failed")
  all_files
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

#' Convert a Jupyter notebook to code only
#'
#' A `.ipynb` is a JSON document, not a text script: its source lives in the
#' `source` array of each `"code"` cell. This concatenates those cells in
#' document order so the result can be checked by the same text-based helpers
#' every other language goes through ([code_remove_comments()],
#' [code_library_names()], [code_file_refs()], ...) — the notebook analogue of
#' [code_extract_r()] purling an `.Rmd`/`.qmd`.
#'
#' Jupyter is kernel-agnostic, so a notebook's cells may be Python **or** R
#' (or another language); this extracts the source whatever the kernel, and
#' [code_lang()] is what reports which language it is. The function is named
#' for the file type it reads, not for a language it assumes.
#'
#' Markdown and raw cells are dropped (they are prose, not code). Cell
#' boundaries are preserved as blank lines so line counts stay meaningful, and
#' IPython magics (`%matplotlib inline`) and shell escapes (`!pip install x`)
#' are dropped: they are not statements in any kernel language and would only
#' ever be noise to the checks downstream.
#'
#' The OUTPUTS a notebook stores alongside its code are not touched here — see
#' [read_stat_tables()], which reads those as result tables the same way it
#' reads a `.jasp`/`.omv` archive.
#'
#' @param file_path path to a `.ipynb` file
#' @param save_path if NULL, returns a text vector, else a path to save to
#' @param text alternative to file_path, pass the notebook JSON directly
#'
#' @returns a character vector of source lines (empty when the file has
#'   no code cells or is not parseable JSON)
#' @export
code_extract_py <- function(file_path = NULL, save_path = NULL, text = NULL) {
  if (is.null(file_path) & is.null(text)) {
    stop("You must specify one of file_path or text")
  } else if (is.null(text)) {
    text <- code_read(file_path)
  }

  nb <- tryCatch(jsonlite::fromJSON(paste(text, collapse = "\n"),
                                    simplifyVector = FALSE),
                 error = function(e) NULL)
  cells <- nb$cells
  if (is.null(cells) || !length(cells)) {
    out <- character(0)
    if (is.null(save_path)) return(out)
    writeLines(out, save_path)
    return(save_path)
  }

  src <- lapply(cells, function(cl) {
    if (!identical(cl$cell_type, "code")) return(NULL)
    # `source` is normally an array of lines (each keeping its trailing "\n"),
    # but the schema also permits a single string -- handle both.
    s <- cl$source
    if (is.null(s) || !length(s)) return(NULL)
    lines <- unlist(strsplit(paste(unlist(s), collapse = ""), "\n"))
    # Drop IPython magics ("%cd ..", "%%timeit") and shell escapes ("!pip
    # install x"): neither is Python, and both would otherwise be scanned for
    # imports and file references as though they were.
    lines <- lines[!grepl("^\\s*[%!]", lines)]
    if (!length(lines)) return(NULL)
    c(lines, "")   # blank line marks the cell boundary
  })

  out <- unlist(Filter(Negate(is.null), src)) %||% character(0)

  if (is.null(save_path)) {
    out
  } else {
    writeLines(out, save_path)
    save_path
  }
}

#' Extract Python chunks from a Quarto document
#'
#' A `.qmd` whose declared/inferred engine is Python (see [code_lang()]'s
#' `.qmd_lang()`) cannot go through [code_extract_r()]: `knitr::purl()` only
#' recovers ```` ```{r} ```` chunks, so a Python-engine document would purl to
#' nothing. This is the `.qmd` analogue of [code_extract_r()] for that case —
#' concatenating the body of every ```` ```{python} ```` fence in document
#' order, the same "recover a checkable code file" idea [code_extract_py()]
#' applies to a `.ipynb`'s code cells (chunk boundaries are preserved as a
#' blank line so line counts stay meaningful).
#'
#' Unlike [code_extract_r()], this is a plain text/regex scan, not a real
#' Pandoc/Quarto parse: a fence marker appearing inside a string literal or a
#' displayed (non-executable) code block written with four backticks around a
#' literal ```` ```{python} ```` would be misread as a real chunk. This
#' mirrors the fence-based approach already used elsewhere in the module
#' (e.g. `code_parse_r()`'s own `^---\\s*$` front-matter check) rather than
#' introducing a new parsing dependency for a single edge case.
#'
#' @param file_path a path to a `.qmd` file
#' @param save_path if NULL, returns a text vector, else a path to save to
#' @param text alternative to file_path, pass text directly
#'
#' @returns a character vector of Python source lines (empty when the
#'   document has no Python chunks)
#' @export
code_extract_qmd_py <- function(file_path = NULL, save_path = NULL, text = NULL) {
  if (is.null(file_path) & is.null(text)) {
    stop("You must specify one of file_path or text")
  } else if (is.null(text)) {
    text <- code_read(file_path)
  }

  # A fenced chunk opens with one-or-more backticks + "{python...}" (chunk
  # options after the language, e.g. "{python, echo=FALSE}", are permitted so
  # not matched into the language itself) and closes with a line of backticks
  # matching the SAME fence length -- Pandoc allows longer fences to nest
  # shorter ones verbatim, so the close must match the open, not just be "any
  # backtick line", or a nested example fence would end extraction early.
  out <- character(0)
  i <- 1L
  n <- length(text)
  while (i <= n) {
    m <- regexec("^(```+)\\s*\\{python\\b", text[[i]], ignore.case = TRUE)
    fence <- regmatches(text[[i]], m)[[1]]
    if (length(fence) == 0) { i <- i + 1L; next }

    open_fence <- fence[[2]]
    close_pattern <- paste0("^", open_fence, "\\s*$")
    j <- i + 1L
    body <- character(0)
    while (j <= n && !grepl(close_pattern, text[[j]])) {
      body <- c(body, text[[j]])
      j <- j + 1L
    }
    # Drop "#| option: value" chunk-option lines: renderer directives, not
    # code, mirroring knitr::purl() dropping the equivalent `#|` lines from an
    # {r} chunk (confirmed: purl() emits only "x <- 1" from a chunk opening
    # with "#| echo: false" / "#| include: false"). Left in, these would read
    # as real (if syntactically harmless, since "#" is also Python's comment
    # marker) code lines to code_line_stats()/code_abs_path()/etc.
    body <- body[!grepl("^\\s*#\\|", body)]
    if (length(body)) out <- c(out, body, "")   # blank line marks chunk boundary
    i <- j + 1L
  }

  if (is.null(save_path)) {
    out
  } else {
    writeLines(out, save_path)
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


# Strip a trailing end-of-line comment, ignoring any comment marker that falls
# INSIDE a string literal.
#
# A naive sub("<marker>.*$", "", L) truncates at the first marker wherever it
# appears, including inside a quoted path -- which loses real DATA FILE
# references and, worse, leaves a junk fragment behind that is then reported to
# the author as a referenced file missing from their repository. Confirmed
# against all three languages that have an end-of-line comment marker:
#   Stata  "//"  `use "C://project//data//w1.dta"`     -> ref "C:"
#                `import delimited using "https://..."` -> ref "https:"
#                `merge 1:1 id using "sub//b.dta"`      -> ref "sub"
#   MATLAB "%"   `readtable('data/a%20b.csv')`          -> ref lost entirely
#   Python "#"   `pd.read_csv('a#b.csv')`               -> ref lost entirely
# (URL-encoded spaces, URL fragments, and doubled path separators are all
# ordinary things to find in a real analysis script.)
#
# Scans character by character, tracking whether we are inside a single- or
# double-quoted string, and only treats the marker as a comment when it occurs
# outside one. Backslash escapes the next character. Returns the line
# truncated at the first genuine comment marker, or unchanged when there is
# none.
#
# @param L a single line of code
# @param marker the comment marker ("//", "%", "#")
# @returns the line, minus any trailing comment
.code_strip_inline_comment <- function(L, marker) {
  chars <- strsplit(L, "")[[1]]
  if (!length(chars)) return(L)
  mchars <- strsplit(marker, "")[[1]]
  mlen <- length(mchars)
  quote_ch <- NA_character_   # which quote type we are inside, NA if none
  escaped <- FALSE
  i <- 1L
  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (escaped) { escaped <- FALSE; i <- i + 1L; next }
    if (ch == "\\") { escaped <- TRUE; i <- i + 1L; next }
    if (is.na(quote_ch)) {
      if (ch == "'" || ch == '"') {
        quote_ch <- ch
      } else if (i + mlen - 1L <= length(chars) &&
                 identical(chars[i:(i + mlen - 1L)], mchars)) {
        return(if (i == 1L) "" else paste(chars[seq_len(i - 1L)], collapse = ""))
      }
    } else if (ch == quote_ch) {
      quote_ch <- NA_character_
    }
    i <- i + 1L
  }
  L
}

#' Remove comments from code text
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, Python, SPSS, SAS,
#'   Stata, Mplus, MATLAB)
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
code_remove_comments <- function(code_text, lang = c("R", "Python", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
  lang <- match.arg(lang)
  in_block <- FALSE
  code_text <- strsplit(code_text, "\n+") |> unlist()
  code_text_nc <- character(0)

  if (lang == "R") {
    code_text_nc <- grep("^(\\s*$|\\s*#|```\\s*\\{r)",
                         code_text, invert = TRUE, value = TRUE)
    # code_text_nc <- grep("knitr::", file_nc, invert = TRUE, value = TRUE)
    # Strip trailing "# ..." comments, skipping any "#" inside a string
    # literal (a URL fragment, a column name df['#hits'], a filename
    # 'a#b.csv') -- same syntax and same risk as Python's "#", handled the
    # same way. Previously R kept a mixed code+comment line completely
    # unstripped, which both let comment text leak into code_abs_path()/
    # code_file_refs()/code_library_lines() false positives and (via
    # code_line_stats(), see below) made every such line invisible to the
    # comment count -- see issue #261.
    code_text_nc <- vapply(code_text_nc, .code_strip_inline_comment, character(1),
                           marker = "#", USE.NAMES = FALSE)
  } else if (lang == "Python") {
    # Python's only comment syntax is "#" to end of line -- there is no block
    # comment (a triple-quoted string used as one is a STRING expression, and
    # stripping it would also remove real docstrings AND any triple-quoted
    # literal, so it is deliberately left alone).
    code_text_nc <- code_text[!grepl("^\\s*#", code_text)]
    # Strip trailing "# ..." comments, skipping any "#" inside a string
    # literal (a URL fragment, a column name df['#hits'], a filename
    # 'a#b.csv') -- see .code_strip_inline_comment() for why a naive sub()
    # is not safe here.
    code_text_nc <- vapply(code_text_nc, .code_strip_inline_comment, character(1),
                           marker = "#", USE.NAMES = FALSE)
    # Blank lines are dropped for the same reason R's branch drops them: they
    # are neither code nor comment, and code_line_stats() counts them
    # separately (it subtracts blanks AND this function's result from the
    # total to get comment_lines -- leaving blanks in here would make every
    # blank line count as a line of CODE, understating percent_comments).
    code_text_nc <- code_text_nc[trimws(code_text_nc) != ""]
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
        # Strip end-of-line "//" comments, skipping any "//" inside a string
        # literal. Stata paths routinely contain one -- a URL passed to
        # `import delimited using "https://..."`, or a doubled Windows
        # separator "C://project//data//w1.dta" -- and truncating there both
        # loses the real reference AND leaves a junk fragment ("https:",
        # "C:") that code_file_refs() then reports to the author as a
        # missing file. See .code_strip_inline_comment().
        if (grepl("//", L)) L <- .code_strip_inline_comment(L, "//")
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
      # Strip a trailing "%..." comment, skipping any "%" inside a string
      # literal -- disp('100% done'), and (the case that actually loses data)
      # a URL-encoded filename such as readtable('data/a%20b.csv'), where
      # truncating drops the file reference entirely. See
      # .code_strip_inline_comment().
      code_text_nc <- c(code_text_nc, .code_strip_inline_comment(L, "%"))
    }
  } else {
    code_text_nc <- code_text
  }

  return(code_text_nc)
}

# Per-language, per-ORIGINAL-line detection of whether a line contains a
# comment (whole-line OR trailing), mirroring code_remove_comments()'s own
# block/line rules exactly so the two never disagree. Returns a logical
# vector the same length as code_text.
#
# This exists separately from code_remove_comments() because that function's
# return value only says which lines SURVIVE as code -- a mixed code+comment
# line survives there too (just with its comment portion stripped), so its
# length alone cannot distinguish "pure code" from "code with a comment
# attached". Deriving comment_lines by subtracting length(code_remove_comments(...))
# from total_lines (the previous approach) therefore made every mixed line
# invisible to the comment count, regardless of language -- see issue #261.
.code_comment_flags <- function(code_text, lang) {
  n <- length(code_text)
  has_comment <- rep(FALSE, n)
  in_block <- FALSE

  if (lang %in% c("R", "Python")) {
    # Both languages use "#" to end of line, no block-comment syntax (a
    # triple-quoted Python string used as a comment is a real string
    # expression, not stripped -- see code_remove_comments()'s Python branch).
    whole_line <- grepl("^\\s*#", code_text)
    trailing <- vapply(code_text, function(L) {
      !identical(.code_strip_inline_comment(L, "#"), L)
    }, logical(1), USE.NAMES = FALSE)
    has_comment <- whole_line | trailing
  } else if (lang == "SAS") {
    for (ln in seq_len(n)) {
      L <- code_text[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      was_in_block <- in_block
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*.*;\\s*$", L)
      has_comment[ln] <- was_in_block || line_comment || (!was_in_block && starts_block)
      if (in_block && ends_block) in_block <- FALSE
    }
  } else if (lang == "SPSS") {
    for (ln in seq_len(n)) {
      L <- code_text[ln]
      starts_block <- grepl("/\\*|COMMENT BEGIN", L)
      ends_block <- grepl("\\*/|COMMENT END\\.", L)
      was_in_block <- in_block
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*(\\*|COMMENT)", L)
      has_comment[ln] <- was_in_block || line_comment || (!was_in_block && starts_block)
      if (in_block && ends_block) in_block <- FALSE
    }
  } else if (lang == "Stata") {
    for (ln in seq_len(n)) {
      L <- code_text[ln]
      starts_block <- grepl("/\\*", L)
      ends_block <- grepl("\\*/", L)
      was_in_block <- in_block
      if (!in_block && starts_block) in_block <- TRUE
      line_comment <- grepl("^\\s*\\*", L)
      trailing <- !was_in_block && !line_comment && grepl("//", L) &&
        !identical(.code_strip_inline_comment(L, "//"), L)
      has_comment[ln] <- was_in_block || line_comment || (!was_in_block && starts_block) || trailing
      if (in_block && ends_block) in_block <- FALSE
    }
  } else if (lang == "Mplus") {
    # Mplus syntax comment is "!" to end of line; no block-comment syntax.
    has_comment <- grepl("!", code_text)
  } else if (lang == "MATLAB") {
    for (ln in seq_len(n)) {
      L <- code_text[ln]
      starts_block <- grepl("^\\s*%\\{\\s*$", L)
      ends_block   <- grepl("^\\s*%\\}\\s*$", L)
      was_in_block <- in_block
      if (!in_block && starts_block) { in_block <- TRUE; has_comment[ln] <- TRUE; next }
      if (was_in_block) { has_comment[ln] <- TRUE; if (ends_block) in_block <- FALSE; next }
      whole_line <- grepl("^\\s*%", L)
      trailing <- !whole_line && !identical(.code_strip_inline_comment(L, "%"), L)
      has_comment[ln] <- whole_line || trailing
    }
  }
  # SAS/SPSS/Stata's `line_comment` regexes only match a comment STARTING at
  # the line, so a line already counted via was_in_block/line_comment/starts_block
  # is correct as-is; no separate trailing check needed there (none of those
  # three have trailing single-line comment syntax, only their block syntax and
  # Stata's "//" which is handled above).
  has_comment
}

#' Get Code Composition Stats
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, Python, SPSS, SAS,
#'   Stata, Mplus, MATLAB)
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
code_line_stats <- function(code_text, lang = c("R", "Python", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
  lang <- match.arg(lang)
  code_text <- strsplit(code_text, "\n+") |> unlist()

  total_lines <- length(code_text)
  blank_lines <- sum(trimws(code_text) == "")
  code_lines <- code_remove_comments(code_text, lang) |> length()
  # Lines actually CONTAINING a comment marker (whole-line or trailing),
  # counted directly rather than derived by subtracting code_lines from
  # total_lines -- a mixed code+comment line is real code (so it belongs in
  # code_lines too) AND a real comment (so it must count here), and the two
  # are not mutually exclusive. See .code_comment_flags().
  comment_lines <- sum(.code_comment_flags(code_text, lang) & trimws(code_text) != "")

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
#' @param lang the language (we only currently handle R, Python, SPSS, SAS,
#'   Stata, Mplus, MATLAB)
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
code_library_lines <- function(code_text, lang = c("R", "Python", "SPSS", "SAS", "Stata", "Mplus", "MATLAB")) {
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
    # Python's "import x" / "from x import y" are the direct analogue of R's
    # library() calls -- the same "are the imports grouped at the top?" check
    # applies unchanged. Anchored at the start of the line (imports are
    # statements, so a legal top-level import always begins its line, modulo
    # indentation) to avoid matching the word "import" inside a string or a
    # method name such as `df.import_csv`.
    Python = "^\\s*(import|from)\\s+[A-Za-z_.]",
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

  code_text <- code_remove_comments(code_text, lang)
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
      # library(x) / library("x") / require(x) / requireNamespace("x") — ALL
      # occurrences on the line, not just the first: authors commonly
      # semicolon-chain a whole block of library() calls on one line
      # ("library(a); library(b); library(c)"), and cap()'s regexec() only
      # ever returns a single match per call. Confirmed as a real bug against
      # a real corpus paper's script, which chained 14 library() calls on one
      # line — only the first (its own package) was ever detected, so the
      # other 13 (including one genuinely needed at runtime) were silently
      # never installed, and the script errored on the first missing one.
      for (fn in c("library", "require", "requireNamespace")) {
        m <- gregexpr(sprintf("\\b%s\\s*\\(\\s*([A-Za-z0-9._'\"]+)", fn), L,
                      perl = TRUE)
        g <- regmatches(L, m)[[1]]
        if (length(g) > 0) {
          pkgs <- sub(sprintf("^%s\\s*\\(\\s*", fn), "", g, perl = TRUE)
          add(pkgs, fn, ln)
        }
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

#' Detect whether a paper pinned its R/package versions
#'
#' Checks a repository's files for the three ways authors commonly pin the
#' environment their analysis depended on, so a later reproduction attempt
#' knows which R and package versions were actually used rather than
#' whatever happens to be current: an `renv.lock` file (parsed for the R
#' version and every locked package + its version/source), a `sessionInfo()`
#' / `sessioninfo::session_info()` text dump (matched by filename, since
#' authors deposit these as a dedicated file rather than burying them in
#' unrelated text — see `@details`), or a `groundhog::groundhog.library()` /
#' `checkpoint::checkpoint()` call in the R code, which pin by date instead of
#' a lockfile. Static analysis only: this reports what was DECLARED, not
#' whether the declared versions still install today (that risk is what
#' `repro_install_deps()`'s CRAN Archive fallback exists for).
#'
#' @details
#' The `sessionInfo()` scan is filename-gated, not a blanket content scan of
#' every text file in the repository: it looks at files whose name suggests
#' they ARE such a dump (`sessioninfo`/`session_info`/`session-info`,
#' case-insensitive, any extension — the two forms actually seen in a real
#' corpus are `sessionInfo.txt` and `session_info.txt`) plus any `readme*`
#' file, since authors sometimes paste the dump into a README instead of a
#' dedicated file. A blanket scan of every `.txt`/`.md`/`.log` file was
#' considered and rejected: it would also match a paper's own methods prose
#' mentioning an R version in a sentence, which is not a pinned-environment
#' record.
#'
#' `renv.lock` is matched by its fixed, case-sensitive filename (that is the
#' one name `renv` itself ever writes) and searched for anywhere in the
#' repository tree, not just the top level — a paper can deposit more than
#' one (e.g. one per study folder), and all are reported.
#'
#' `groundhog`/`checkpoint` are detected by the PINNING CALL itself
#' (`groundhog.library(pkg, "YYYY-MM-DD")`, `checkpoint("YYYY-MM-DD")`), not
#' merely `library(groundhog)`/`library(checkpoint)`: loading the package is
#' not evidence it was used to pin anything, so a bare `library()` call does
#' not count.
#'
#' @param all_files the full repo file listing (`repo_check`'s table: needs
#'   `file_name`, `file_url`, `repo_url`, `file_location` where available)
#' @param code_text_list a named list of already-read R file text (names are
#'   file_names) — reused for the groundhog/checkpoint scan so no file is
#'   downloaded or read twice
#' @param max_file_size passed to [download_repo_files()] for any candidate
#'   file not yet local
#' @param max_download_size passed to [download_repo_files()]
#' @param cache passed to [download_repo_files()]
#'
#' @returns a list: `pinned` (logical, TRUE if any mechanism was found),
#'   `mechanisms` (character vector, any of `renv.lock`, `sessionInfo`,
#'   `groundhog`, `checkpoint`), `r_versions` (character vector of R version
#'   strings found, one per source), `renv_files` (character vector of
#'   `renv.lock` file_names found), `renv_packages` (data frame `file_name`,
#'   `package`, `version`, `source` — one row per locked package, across all
#'   `renv.lock` files found), `sessioninfo_files` (character vector of
#'   matched file_names), `file_location` (named character vector, file_name
#'   -> resolved local path, for every candidate file this call downloaded —
#'   a caller re-checking a SUBSET of `all_files` per paper can splice these
#'   back into its own copy of `all_files` first, so the same file is never
#'   downloaded twice across repeat calls)
#' @keywords internal
.code_version_pin_check <- function(all_files, code_text_list = list(),
                                    max_file_size = 100, max_download_size = 500,
                                    cache = FALSE) {
  out <- list(pinned = FALSE, mechanisms = character(0),
             r_versions = character(0), renv_files = character(0),
             renv_packages = data.frame(file_name = character(0),
                                        package = character(0),
                                        version = character(0),
                                        source = character(0)),
             sessioninfo_files = character(0),
             file_location = character(0))
  if (is.null(all_files) || nrow(all_files) == 0) return(out)

  base_nm <- basename(gsub("\\\\", "/", all_files$file_name))

  # ── renv.lock ────────────────────────────────────────────────────────────
  is_renv <- base_nm == "renv.lock"
  if (any(is_renv)) {
    renv_rows <- all_files[is_renv, , drop = FALSE]
    need_dl <- (is.na(renv_rows$file_location) | !nzchar(renv_rows$file_location %||% "")) &
      !is.na(renv_rows$file_url) & nzchar(renv_rows$file_url %||% "")
    if (any(need_dl)) {
      dl <- tryCatch(
        download_repo_files(renv_rows[need_dl, , drop = FALSE],
                            max_file_size = max_file_size,
                            max_download_size = max_download_size, cache = cache),
        error = function(e) NULL)
      if (!is.null(dl)) renv_rows$file_location[need_dl] <- dl$file_location
    }
    out$file_location <- c(out$file_location, stats::setNames(
      renv_rows$file_location, renv_rows$file_name))
    for (i in seq_len(nrow(renv_rows))) {
      loc <- renv_rows$file_location[i]
      if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) next
      lock <- tryCatch(jsonlite::fromJSON(loc, simplifyVector = FALSE),
                       error = function(e) NULL)
      if (is.null(lock)) next
      out$renv_files <- c(out$renv_files, renv_rows$file_name[i])
      rv <- lock$R$Version
      if (!is.null(rv)) out$r_versions <- c(out$r_versions, as.character(rv))
      pkgs <- lock$Packages
      if (length(pkgs) > 0) {
        pkg_rows <- lapply(pkgs, function(p) data.frame(
          file_name = renv_rows$file_name[i],
          package = p$Package %||% NA_character_,
          version = p$Version %||% NA_character_,
          source = p$Source %||% NA_character_))
        out$renv_packages <- dplyr::bind_rows(out$renv_packages,
                                              dplyr::bind_rows(pkg_rows))
      }
    }
    if (length(out$renv_files) > 0) out$mechanisms <- c(out$mechanisms, "renv.lock")
  }

  # ── sessionInfo() / session_info() dump ─────────────────────────────────
  # Filename-gated (see roxygen @details): a dedicated file, or a README that
  # might embed the dump, downloaded only if not already local.
  is_si_name <- grepl("session[_-]?info", base_nm, ignore.case = TRUE)
  is_readme  <- grepl("^readme", base_nm, ignore.case = TRUE)
  si_candidates <- is_si_name | is_readme
  if (any(si_candidates)) {
    si_rows <- all_files[si_candidates, , drop = FALSE]
    need_dl <- (is.na(si_rows$file_location) | !nzchar(si_rows$file_location %||% "")) &
      !is.na(si_rows$file_url) & nzchar(si_rows$file_url %||% "")
    if (any(need_dl)) {
      dl <- tryCatch(
        download_repo_files(si_rows[need_dl, , drop = FALSE],
                            max_file_size = max_file_size,
                            max_download_size = max_download_size, cache = cache),
        error = function(e) NULL)
      if (!is.null(dl)) si_rows$file_location[need_dl] <- dl$file_location
    }
    out$file_location <- c(out$file_location, stats::setNames(
      si_rows$file_location, si_rows$file_name))
    # sessionInfo()'s own first line: "R version 4.4.2 (2024-10-31)". Matched
    # against R's real print format (confirmed against real corpus files),
    # not guessed.
    rv_pat <- "R version ([0-9]+\\.[0-9]+\\.[0-9]+)"
    for (i in seq_len(nrow(si_rows))) {
      loc <- si_rows$file_location[i]
      if (is.na(loc) || !nzchar(loc) || !file.exists(loc)) next
      txt <- tryCatch(code_read(loc), error = function(e) NULL)
      if (is.null(txt) || !length(txt)) next
      # code_read() is documented to return one element per line, but is not
      # trusted blindly here: for at least one real corpus file it returned
      # the WHOLE file as a single element with embedded newline characters
      # (an encoding/line-ending edge case neither readr::read_lines() nor
      # its readLines() fallback split correctly) -- re-splitting on "\n"
      # defensively is a no-op when txt is already one-line-per-element, and
      # fixes the case when it is not. This is what surfaced the real bug:
      # sub(".*pattern.*", "\\1", hit) on a MULTI-LINE hit (paste0(".*", ...)
      # does not match "\n" by default) spliced together the LEADING text
      # before "R version" with the version digits themselves, e.g.
      # extracting "\n4.5.0" instead of "4.5.0" -- which then produced a
      # Docker image tag with a literal embedded newline
      # ("rocker/r-ver:\n4.5.0"), silently breaking every `docker run` call
      # with "invalid reference format". Confirmed directly against a real
      # failing run.
      txt <- unlist(strsplit(txt, "\n", fixed = TRUE))
      hit <- grep(rv_pat, txt, value = TRUE, perl = TRUE)[1]
      if (!is.na(hit)) {
        # regmatches() on the capture group directly, not a sub(".*x.*", "\\1")
        # splice: immune to what surrounds the match on the SAME line, and
        # cannot pull in any other line's text the way the splice did above.
        m <- regexpr(rv_pat, hit, perl = TRUE)
        version <- regmatches(hit, m)
        version <- sub("^R version ", "", version)
        out$sessioninfo_files <- c(out$sessioninfo_files, si_rows$file_name[i])
        out$r_versions <- c(out$r_versions, version)
      }
    }
    if (length(out$sessioninfo_files) > 0) out$mechanisms <- c(out$mechanisms, "sessionInfo")
  }

  # ── groundhog / checkpoint date-pinning calls ───────────────────────────
  # The pinning CALL itself, with a date-shaped argument -- library(groundhog)
  # / library(checkpoint) alone is not evidence anything was pinned (see
  # roxygen @details).
  # groundhog.library()'s first argument is often c("pkg1", "pkg2", ...) --
  # one level of nested parens between the call's own "(" and the date
  # argument, so a flat [^)]* (stops at the FIRST ")") missed every
  # multi-package call. (?:[^()]|\([^()]*\))* tolerates exactly one level of
  # nesting, which covers a plain c(...) argument list; confirmed against
  # groundhog's own documented multi-package call shape.
  gh_pat <- "groundhog(?:::)?\\.?library\\s*\\((?:[^()]|\\([^()]*\\))*[\"'](\\d{4}-\\d{2}-\\d{2})[\"']"
  cp_pat <- "checkpoint(?:::checkpoint)?\\s*\\(\\s*[\"'](\\d{4}-\\d{2}-\\d{2})[\"']"
  has_gh <- FALSE; has_cp <- FALSE
  for (txt in code_text_list) {
    if (!length(txt)) next
    joined <- paste(txt, collapse = "\n")
    if (!has_gh && grepl(gh_pat, joined, perl = TRUE)) has_gh <- TRUE
    if (!has_cp && grepl(cp_pat, joined, perl = TRUE)) has_cp <- TRUE
    if (has_gh && has_cp) break
  }
  if (has_gh) out$mechanisms <- c(out$mechanisms, "groundhog")
  if (has_cp) out$mechanisms <- c(out$mechanisms, "checkpoint")

  out$mechanisms <- unique(out$mechanisms)
  out$r_versions <- unique(out$r_versions)
  out$pinned <- length(out$mechanisms) > 0
  out
}

#' Get files referenced in code
#'
#' @param code_text the code text for a single file
#' @param lang the language (we only currently handle R, Python, SPSS, SAS,
#'   Stata, Mplus, MATLAB)
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
                           lang = c("R", "Python", "SPSS", "SAS", "Stata", "Mplus", "MATLAB"),
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
    # Python's readers, by library. pandas/polars supply the bulk as
    # `read_<format>` (read_csv, read_excel, read_stata, read_spss, read_sas,
    # read_parquet, read_pickle, read_json, ...), which the generic
    # "read_[A-Za-z_0-9]+" covers the same way R's "read[\\._][A-Za-z...]" does
    # -- NOT anchored to a `pd.` prefix, since `from pandas import read_csv`
    # then calls it bare. The rest are named individually because they do not
    # begin with "read": numpy (load/loadtxt/genfromtxt), pickle (load),
    # json (load), scipy.io (loadmat), pyreadstat (read_* covered above),
    # and Python's own open(). Like the R branch, a hit only YIELDS a
    # reference when the call also contains a quoted string with a file
    # extension, so `import numpy` can never be mistaken for a file
    # reference here (the mirror image of the reticulate::import("numpy")
    # false positive guarded against in the R branch above) -- but "import"
    # is deliberately NOT in this list at all, so the question never arises.
    Python = c(
      "read_[A-Za-z_0-9]+",
      "loadtxt",
      "genfromtxt",
      "loadmat",
      "load",
      "open",
      "read_table"
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

  # A sprintf()/format()-style format specifier ("%.2e", "%.3f", "%5.2f", "%d")
  # matches quoted_filename_pattern by coincidence: its own "%" + digits + "."
  # + conversion letter looks exactly like "<name>.<ext>" to a pattern that
  # only checks for A dot followed by 1-8 alphanumeric characters. Confirmed
  # as a real false positive against a real corpus paper's script (which
  # printed values with sprintf("%.2e", x)) — "%.2e" and "%.3f" were reported
  # as referenced files "not present in the repository", entirely spurious
  # since neither is a filename at all. Excluded here rather than tightening
  # quoted_filename_pattern itself, since a real filename can legitimately
  # contain "%" (URL-encoded characters, a literal percent in a name) and the
  # format-specifier SHAPE (leading "%", then only digits/./conversion
  # letters, nothing else) is what actually distinguishes the false positive.
  is_format_spec <- grepl("^%[-+ 0#]*[0-9]*(\\.[0-9]+)?[diouxXeEfgGaAscp]$",
                          loaded_file, perl = TRUE)
  loaded_file <- loaded_file[!is_format_spec]

  # Unquoted captures (language-specific)
  lang_unquoted_captures <- list(
    R = list(), # quoted captures suffice
    # Python has no bareword file paths -- every filename is a string literal
    # (or an f-string / Path() composition, which the quoted-filename pattern
    # picks up whenever a literal extension survives in the source). Same
    # situation as Mplus below: quoted captures suffice.
    Python = list(),
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
