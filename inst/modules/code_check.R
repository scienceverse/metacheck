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
#' The module also checks whether the repository pins the R/package versions the analysis actually depended on: an `renv.lock` file (parsed for the R version and every locked package + its version/source), a `sessionInfo()`/`sessioninfo::session_info()` text dump (matched by filename — `sessionInfo.txt`/`session_info.txt` and similar, or embedded in a README), or a `groundhog::groundhog.library()`/`checkpoint::checkpoint()` date-pin call in the code (a bare `library(groundhog)`/`library(checkpoint)` does not count — the pinning call itself must be present). When none of these is found, the report flags it as a reproducibility gap: package versions may drift between when the analysis was run and any later reproduction attempt.
#'
#' If you want to extend the package to perform additional checks on code files, or make the checks work on other types of code files, reach out to the Metacheck development team.
#'
#' @keywords results
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Raphael Merz (\email{r.t.p.merz@tue.nl})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object, or NULL to check local files only (see [test_paper()])
#' @param local_path optional path to a local directory. When provided, all files in that directory (recursively) are added to the file list alongside any files found via `repo_check`.
#' @param local_only if TRUE, skip online repository lookups (see `repo_check`)
#' @param download if TRUE (default), download the code files to be checked from online repositories so they are read locally. Set FALSE to stream each file from its URL instead.
#' @param max_file_size largest single file to download, in MB (default 100). Size caps are an upfront, all-or-nothing gate per repository; set `Inf` for no cap.
#' @param max_download_size largest total download per repository, in MB (default 500). Set `Inf` for no cap.
#' @param cache if TRUE, keep downloaded files in a persistent on-disk cache (see [repo_cache_dir()]) so they are reused on later runs. If FALSE (the default), download to a temporary directory discarded when the session ends. Clear the cache with [repo_cache_clear()].
#' @param manifest optional path to a metacheck manifest directory or `*.manifest.json` file. When given, the distinct packages loaded across the paper's code are merged into the manifest's `code` section (see [manifest_merge()]), preserving any `files`/`provenance` written by `data_check`. A directory resolves to `<paper_id>.manifest.json` inside it; the manifest is created if it does not yet exist.
#'
#' @returns a list
code_check <- function(paper, local_path = NULL,
                        local_only = FALSE, download = TRUE,
                        max_file_size = 100, max_download_size = 500,
                        cache = FALSE, manifest = NULL) {
  # example with osf Rmd files and github files: paper <- psychsci[[203]]
  # example with missing data files: paper <- psychsci[[221]]
  # Many R files, some with library in different places. paper <- psychsci[[225]]
  # Best example, with many issues, for paper: paper <- psychsci[[233]]
  # ResearchBox and GitHub example (in full xml): paper <- xml[["09567976251333666"]]

  # A cached repo_check output (from an earlier module_run() in the same
  # pipeline) never encodes which local_path, if any, it was called with. If
  # local_path is given here, a cached result from a run without it (or with
  # a different one) would silently omit those local files, so local_path
  # forces a fresh repo_check call rather than trusting the cache.
  all_files <- if (is.null(local_path)) get_prev_outputs("repo_check", "table") else NULL
  if (is.null(all_files)) {
    if (!is.null(local_path)) {
      mo <- module_run(paper, "repo_check", local_path = local_path, local_only = local_only)
    } else {
      mo <- module_run(paper, "repo_check", local_only = local_only)
    }
    all_files <- mo$table %||% data.frame(file_name = character(0), repo_url = character(0))
  }
  all_files$language <- code_lang(all_files$file_name)

  # Download every file the steps below might need (spv/smcl/mplus/html
  # candidates, plus every checked-language file) in ONE call, instead of
  # each step downloading its own subset separately -- see .code_predownload()
  # for why. Each step's own need_dl check afterwards then finds file_location
  # already populated and is a no-op.
  predl_gated <- NULL
  if (isTRUE(download)) {
    all_files <- .code_predownload(all_files, max_file_size, max_download_size, cache)
    predl_gated <- attr(all_files, "gated")
  }

  # An .spv is SPSS's rendered OUTPUT (never itself checked as code), but its
  # structure XML embeds the exact SPSS syntax that produced each result.
  # Recover that syntax as a real .sps sibling file (written into a `code`
  # subfolder next to the .spv) and add it to the file list so it is checked
  # like any other .sps file below -- see .code_expand_spv() and
  # .spv_export_syntax() (R/spv.R). Downloads .spv files that aren't
  # already local using the same options this function already passes to
  # download_repo_files() further down.
  if (any(grepl("\\.spv$", all_files$file_name, ignore.case = TRUE))) {
    all_files <- .code_expand_spv(all_files, max_file_size, max_download_size, cache)
    all_files$language <- code_lang(all_files$file_name)
  }

  # A .smcl is Stata's rendered OUTPUT log (never itself checked as code),
  # but every command it ran is echoed verbatim -- unlike .spv, this IS
  # already the exact syntax with no separate structure to recover it from.
  # Recover it as a real .do sibling file the same way .spv's syntax becomes
  # a .sps file -- see .code_expand_smcl() and .smcl_export_syntax()
  # (R/stata.R).
  if (any(grepl("\\.smcl$", all_files$file_name, ignore.case = TRUE))) {
    all_files <- .code_expand_smcl(all_files, max_file_size, max_download_size, cache)
    all_files$language <- code_lang(all_files$file_name)
  }

  # A .out is Mplus's rendered OUTPUT (never itself checked as code), but
  # (like .smcl, and unlike .spv) its own "INPUT INSTRUCTIONS" section
  # already holds the exact verbatim syntax that produced it, with no
  # separate structure to decode it from. Recover it as a real .inp sibling
  # file the same way .smcl's echoed commands become a .do file -- see
  # .code_expand_mplus() and .mplus_export_syntax() (R/mplus.R).
  if (any(grepl("\\.out$", all_files$file_name, ignore.case = TRUE))) {
    all_files <- .code_expand_mplus(all_files, max_file_size, max_download_size, cache)
    all_files$language <- code_lang(all_files$file_name)
  }

  # A ".html" carries no format-locked signal from its extension alone (unlike
  # .spv/.smcl/.out) -- it needs downloading and content-sniffing to tell
  # rendered R Markdown/Quarto analysis output apart from a task-runner page
  # or documentation site. See .code_expand_html() and .html_sniff_kind()/
  # .html_export_r_source() (R/html-output.R) for the full rationale.
  if (any(grepl("\\.html?$", all_files$file_name, ignore.case = TRUE))) {
    all_files <- .code_expand_html(all_files, max_file_size, max_download_size, cache)
    all_files$language <- code_lang(all_files$file_name)
  }

  ## find relevant code files ----
  # JASP files are counted and listed, but not analysed: a .jasp is a binary
  # (zip) archive, so the text-based checks below (comments, absolute paths,
  # library lines) cannot read it. MATLAB (.m) is plain text, so unlike JASP it
  # IS checked here (comments, absolute paths, addpath()/import() "library"
  # lines, referenced files) -- only ACTUALLY RUNNING the code stays out of
  # scope, in reproducibility_check.
  #
  # Python (.py and .ipynb) is checked on the same terms as MATLAB: it is text
  # (a .ipynb is JSON, whose code cells are recovered as text by
  # code_extract_py() below, exactly as .Rmd/.qmd are purled by
  # code_extract_r()), so every text-based check applies. RUNNING it stays out
  # of scope in reproducibility_check, which is R-only.
  listed_langs <- c("R", "Python", "SAS", "SPSS", "Stata", "Mplus", "MATLAB", "JASP")
  checked_langs <- setdiff(listed_langs, "JASP")

  relevant <- all_files$language %in% listed_langs
  code_files <- all_files[relevant, , drop = FALSE]

  lang_counts <- sapply(listed_langs, \(l) sum(code_files$language == l))
  lang_parts <- sprintf("%d %s", lang_counts, names(lang_counts))
  summary_code <- sprintf(
    "We found %s code file%s.",
    paste0(paste(lang_parts[-length(lang_parts)], collapse = ", "),
           ", and ", lang_parts[length(lang_parts)]),
    plural(nrow(code_files))
  )

  checked_files <- code_files[code_files$language %in% checked_langs, , drop = FALSE]

  # no relevant code files found ----
  if (nrow(code_files) == 0) {
    info <- list(
      traffic_light = "na",
      summary_text = summary_code,
      # code_n (not code_file_n): must match the normal-path summary_table's
      # column name below, or a corpus batch mixing papers with and without
      # code files ends up with two disjoint, sparsely-populated columns
      # after being combined across papers (e.g. by capture_check_results()/
      # collect_checks_csv()-style aggregation) instead of one code_n column.
      # na_replace backfills the other normal-path count columns to 0 the
      # same way it already does for a paper with code_n = 0 sitting inside
      # a mixed paperlist that takes the normal path.
      summary_table = data.frame(
        paper_id = paper_id(paper),
        code_n = 0
      ),
      na_replace = c(code_n = 0)
    )

    return(info)
  }

  # Download any remaining code files into the shared cache so their contents
  # are read locally (and reused on later runs) rather than streamed from the
  # repository URL each time. .code_predownload() above already fetched every
  # checked-language file it could; this only catches rows that weren't yet
  # classified (and so weren't in its candidate set) when it ran -- e.g. new
  # rows appended by the expand steps between here and there. Files without a
  # local copy fall back to streaming from file_url below.
  if (isTRUE(download) && "file_url" %in% names(checked_files)) {
    need_dl <- (is.na(checked_files$file_location) |
                  !nzchar(checked_files$file_location %||% "")) &
      !is.na(checked_files$file_url) & nzchar(checked_files$file_url %||% "")
    if (any(need_dl)) {
      dl <- download_repo_files(checked_files[need_dl, , drop = FALSE],
                                max_file_size = max_file_size,
                                max_download_size = max_download_size,
                                cache = cache)
      checked_files$file_location[need_dl] <- dl$file_location
      predl_gated <- dplyr::bind_rows(predl_gated, attr(dl, "gated"))
    }
  }
  # Repositories refused by the size caps (from the pre-pass and/or the
  # catch-up download above): surface each refusal once.
  if (!is.null(predl_gated) && nrow(predl_gated) > 0) {
    for (m in unique(predl_gated$message)) {
      summary_code <- paste(summary_code, m)
      warning(m, call. = FALSE)
    }
  }

  # Check code ----

  # Create list of all file names in repository
  # TODO: iterate this by repo so file names don't bleed over

  # --- Process each code file ---
  if (nrow(checked_files) > 0) {
    pb_code <- pb(nrow(checked_files), ":what [:bar] :current/:total")
    pb_code$tick(0, list(what = ""))
    on.exit(pb_code$terminate())
  }

  # R file text, keyed by paper_id then file_name -- collected as a side
  # effect of the loop below so .code_version_pin_check()'s groundhog/
  # checkpoint scan can reuse it afterwards without re-reading every file.
  # Nested by paper_id (not a single flat list) because file_name can repeat
  # ACROSS papers in a paperlist (two papers can each have "analysis.R"), and
  # the per-paper summary_table split below must not let one paper's
  # groundhog/checkpoint call count toward another's.
  r_text_by_paper <- list()

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
      is_rmd <- grepl("\\.rmd$", the_file$file_name, ignore.case = TRUE)
      # .qmd is Quarto, and (unlike .Rmd) explicitly polyglot: code_lang()'s
      # .qmd_lang() has already read its declared/inferred engine (issue
      # #180), so which extractor runs here follows THAT, not the extension --
      # a Python-engine .qmd purled with code_extract_r()/knitr::purl() would
      # silently yield nothing, since purl() only recovers ```{r} chunks.
      is_qmd <- grepl("\\.qmd$", the_file$file_name, ignore.case = TRUE)
      is_qmd_py <- is_qmd && identical(the_file$language, "Python")
      # A .ipynb is JSON, not a text script: its source lives in the `source`
      # array of each code cell, so it needs extracting before any text-based
      # check can read it -- the same treatment .Rmd/.qmd get above. This
      # holds whatever kernel the notebook declares (code_lang() has already
      # read that from the file), so an R notebook is extracted here and then
      # checked as R. (The OUTPUTS a notebook also stores are read separately,
      # by read_stat_tables() in reproducibility_check.)
      is_ipynb <- grepl("\\.ipynb$", the_file$file_name, ignore.case = TRUE)
      if (is_rmd || (is_qmd && !is_qmd_py)) {
        file_lines <- code_extract_r(file_path)
      } else if (is_qmd_py) {
        file_lines <- code_extract_qmd_py(file_path)
      } else if (is_ipynb) {
        file_lines <- code_extract_py(file_path)
      } else {
        file_lines <- code_read(file_path)
      }

      # Side-collect R text for the version-pinning scan after this loop
      # (.code_version_pin_check()'s groundhog/checkpoint detection), keyed
      # by paper_id first so file_name collisions across papers in a
      # paperlist cannot leak one paper's pin into another's. <<- since this
      # is inside lapply()'s own function scope.
      if (the_file$language == "R") {
        pid_here <- if ("paper_id" %in% names(the_file))
          as.character(the_file$paper_id) else "_all"
        lst <- r_text_by_paper[[pid_here]] %||% list()
        lst[[the_file$file_name]] <- file_lines
        r_text_by_paper[[pid_here]] <<- lst
      }

      # try to parse R-type code; NA (not assessed) for other languages
      the_file$parse_error <- NA
      the_file$parse_error_msg <- NA_character_
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
      # Join with " | " (not "\n"): a path can contain spaces, and a newline
      # separator is collapsed to a space downstream (in reports / CSVs), which
      # would fragment a space-containing path into unreadable pieces. " | " is
      # an unambiguous, single-line-safe delimiter.
      the_file$absolute_paths <- paste(absolute_paths$abs_path,
                                       collapse = " | ")

      # get setwd() calls (R only; the construct is R's). Same " | " delimiter
      # as absolute_paths above, for the same single-line-safe reason.
      setwd_calls <- if (the_file$language == "R")
        code_setwd(file_nc) else
        data.frame(setwd_call = character(0), line = integer(0))
      the_file$code_setwd <- nrow(setwd_calls)
      the_file$setwd_calls <- paste(setwd_calls$setwd_call, collapse = " | ")

      # Find lines where libraries/imports/includes are loaded
      library_lines <- code_library_lines(file_nc, the_file$language)

      # If the import statements are at most 3 lines apart, we consider it OK
      the_file$library_lines <- nrow(library_lines)
      if (nrow(library_lines) > 1) {
        the_file$library_max_between <- diff(library_lines$line) |> max()
      } else {
        the_file$library_max_between <- NA_integer_
      }

      # Names of the packages/libraries loaded, for cataloguing and for a
      # requirements.txt. Stored comma-joined (like absolute_paths above);
      # non-R languages return none. Sorted + de-duplicated per file.
      pkgs <- code_library_names(file_nc, the_file$language)$package |>
        unique() |> sort()
      the_file$packages_n <- length(pkgs)
      the_file$packages <- paste(pkgs, collapse = ", ")

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

  collected_df <- dplyr::bind_rows(collected)
  # When every repository was gated (nothing analysed), `collected` is empty and
  # `collected_df` has no columns to join on. Seed the per-file analysis columns as
  # NA so the reporting below still finds them; all files are simply unchecked.
  if (ncol(collected_df) == 0) {
    analysis_cols <- c("checked", "parse_error", "parse_error_msg",
                       "code_abs_path", "absolute_paths",
                       "code_setwd", "setwd_calls", "library_lines",
                       "library_max_between", "packages_n", "packages",
                       "comment_lines", "code_lines",
                       "percentage_comment", "loaded_files_missing",
                       "loaded_files_missing_names", "error")
    for (col in analysis_cols)
      if (!col %in% names(code_files)) code_files[[col]] <- NA
  } else {
    # `collected` holds one row per checked file (the error path returns its
    # row too), so `collected_df` already is checked_files plus the analysis
    # columns. Do NOT left_join it back onto code_files by the original
    # columns: the download step above updates file_location in
    # checked_files, so every downloaded file would mismatch on that key and
    # get all-NA analysis columns (absolute paths and missing files then
    # vanish from the report).
    #
    # Listed-but-unanalysed files (JASP) are not in `collected`, so append them
    # back: bind_rows fills their analysis columns with NA, and `checked` is
    # set FALSE below. Without this they would vanish from the file listing.
    unchecked <- code_files[!code_files$language %in% checked_langs, , drop = FALSE]
    code_files <- dplyr::bind_rows(collected_df, unchecked)
  }
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
  # which() throughout: an unchecked/errored file has NA analysis values, and
  # indexing by `NA > 0` would inject phantom NA entries into the issue lists
  # (inflating their length and filling the report tables with NA rows).
  absolute_issues <- code_files$file_name[which(code_files$code_abs_path > 0)]
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
    report_table_absolute <- code_files[which(code_files$code_abs_path > 0), cols]
    colnames(report_table_absolute) <- c("File name", "Absolute paths found")
  }

  ## setwd() ----
  # which() (not logical indexing) for the same NA-safety reason as above.
  setwd_issues <- if ("code_setwd" %in% names(code_files))
    code_files$file_name[which(code_files$code_setwd > 0)] else character(0)
  if (length(setwd_issues) == 0) {
    report_setwd <- "Best programming practice is to avoid `setwd()` in analysis code: it hardcodes an assumption about the working directory (often an absolute path on the author's own machine), so the code breaks when run anywhere else. Keep the working directory as the caller sets it and use relative paths. No `setwd()` calls were found in any of the code files."
    summary_setwd <- "No setwd() calls were found."
    report_table_setwd <- NULL
  } else {
    report_setwd <- sprintf(
      "Best programming practice is to avoid `setwd()` in analysis code: it hardcodes an assumption about the working directory (often an absolute path on the author's own machine), so the code breaks when run anywhere else. Keep the working directory as the caller sets it and use relative paths. `setwd()` calls were found in %d code file%s.",
      length(setwd_issues),
      plural(length(setwd_issues))
    )
    summary_setwd <- "setwd() calls were found."
    cols <- c("file_name", "setwd_calls")
    report_table_setwd <- code_files[which(code_files$code_setwd > 0), cols]
    colnames(report_table_setwd) <- c("File name", "setwd() calls found")
  }

  ## Comments ----
  # Only files we actually read can support a claim about comments. A paper with
  # only listed-but-unanalysed files (JASP) would otherwise be told "all your
  # code files had comments" about files that were never opened.
  n_analysed <- sum(!is.na(code_files$percentage_comment))
  comment_issue <- code_files$file_name[which(code_files$percentage_comment == 0)]
  if (n_analysed == 0) {
    report_comments <- "Best programming practice is to add comments to code, to explain what the code does (to yourself in the future, or peers who want to re-use your code). None of the files found could be checked for comments."
    summary_comments <- "No code files could be checked for comments."
  } else if (length(comment_issue) == 0) {
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
  missingfiles_issue <- code_files$file_name[which(code_files$loaded_files_missing > 0)]
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

    rows <- which(code_files$loaded_files_missing > 0)
    cols <- c("file_name", "loaded_files_missing_names")
    report_table_files_missing <- code_files[rows, cols]
    colnames(report_table_files_missing) <- c("File name", "Missing Files")
  }

  ## set up table of code file links ----
  # Named vector, not a positional c(...) assignment: if a checked language
  # errors on every one of its files (e.g. all unreadable/undecodable), the
  # analysis columns below can be genuinely absent from code_files rather than
  # just NA-filled, and a positional names() assignment would then mismatch
  # length and error. Labelling only the columns actually present keeps this
  # safe regardless of which analysis columns survived.
  col_labels <- c(
    file_name             = "File Name",
    percentage_comment    = "% Comments",
    loaded_files_missing  = "Missing Files",
    code_abs_path         = "Absolute Paths",
    library_max_between   = "Code Between Libraries"
  )
  cols <- c(names(col_labels), "file_url") |> intersect(names(code_files))
  report_table <- unique(code_files[, cols])
  report_table$file_name <- link(report_table$file_url, report_table$file_name)
  report_table$file_url <- NULL
  # Unanalysed files (JASP) have no comment percentage; sprintf() would print a
  # literal "NA%" for them, so show an empty cell instead.
  if ("percentage_comment" %in% names(report_table)) {
    report_table$percentage_comment <- ifelse(
      is.na(report_table$percentage_comment), "",
      sprintf("%.0f%%", report_table$percentage_comment * 100)
    )
  }
  names(report_table) <- col_labels[names(report_table)]

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
    # which(), not isTRUE(): isTRUE() on the whole column is FALSE for any
    # multi-file paper, which emptied this table even when errors were found.
    report_table_parse <- code_files[which(code_files$parse_error), cols]
    colnames(report_table_parse) <- c("File name", "Error Message")
  }

  ## Packages / dependencies ----
  # The sorted, de-duplicated union of the packages loaded by a set of files,
  # via the shared code_packages() helper (also used by convert_psychds()).
  pkg_union <- function(rows) code_packages(code_files$packages[rows])

  all_packages <- pkg_union(seq_len(nrow(code_files)))
  if (length(all_packages) == 0) {
    report_packages <- "No packages/libraries were detected as loaded in the code files. (This check reads R and Python imports; other languages are not scanned for packages.) Even when no imports are detected, it is good practice to record the exact package versions the analysis depended on — see \"Reproducible Environment\" below."
    summary_packages <- "No packages/libraries were detected in the code."
  } else {
    report_packages <- sprintf(
      "The code files load %d distinct package%s/librar%s: %s. These are the names found in the source (no version information is available from static analysis).",
      length(all_packages), plural(length(all_packages)),
      if (length(all_packages) == 1) "y" else "ies",
      paste(all_packages, collapse = ", ")
    )
    summary_packages <- sprintf(
      "The code loaded %d distinct package%s.",
      length(all_packages), plural(length(all_packages))
    )
  }

  ## Reproducible environment (version pinning) ----
  # Was the R version / package set actually PINNED anywhere (renv.lock,
  # a sessionInfo() dump, or a groundhog/checkpoint date-pin call)? Static
  # analysis only -- see .code_version_pin_check()'s own roxygen for what
  # each mechanism does and does not establish.
  version_pin <- .code_version_pin_check(
    all_files, code_text_list = unlist(r_text_by_paper, recursive = FALSE),
    max_file_size = max_file_size, max_download_size = max_download_size,
    cache = cache)
  # Splice resolved locations back into all_files so the per-paper re-check
  # below (summary_table$code_version_pinned) finds every candidate file
  # already local and downloads nothing a second time.
  if (length(version_pin$file_location) > 0) {
    m <- match(names(version_pin$file_location), all_files$file_name)
    hit <- !is.na(m) & nzchar(version_pin$file_location %||% "")
    all_files$file_location[m[hit]] <- version_pin$file_location[hit]
  }

  if (!version_pin$pinned) {
    report_version_pin <- "No `renv.lock` file, `sessionInfo()`/`session_info()` record, or `groundhog`/`checkpoint` date-pin call was found anywhere in the repository. Without one of these, the exact R and package versions used for the analysis are not recoverable, and package versions may drift between when this analysis was run and any later reproduction attempt. Consider depositing an `renv.lock` (via the `renv` package), a `sessionInfo()` text dump, or (for R) a `groundhog`/`checkpoint` date-pin alongside the code — or, for Python code, a `requirements.txt` (not currently checked for by this module, but still good practice to include)."
    summary_version_pin <- "No pinned R/package environment (renv.lock, sessionInfo(), or groundhog/checkpoint) was found."
    report_table_version_pin <- NULL
  } else {
    mech_labels <- c(renv.lock = "an `renv.lock` file", sessionInfo = "a `sessionInfo()` record",
                     groundhog = "a `groundhog` date-pin", checkpoint = "a `checkpoint` date-pin")
    found_txt <- paste(mech_labels[version_pin$mechanisms], collapse = ", ")
    rv_txt <- if (length(version_pin$r_versions) > 0)
      sprintf(" Declared R version%s: %s.", plural(length(version_pin$r_versions)),
             paste(version_pin$r_versions, collapse = ", ")) else ""
    report_version_pin <- paste0(
      "The repository pins its analysis environment via ", found_txt, ".", rv_txt,
      if (nrow(version_pin$renv_packages) > 0) sprintf(
        " The `renv.lock` file%s lock%s %d package version%s.",
        plural(length(version_pin$renv_files)),
        if (length(version_pin$renv_files) == 1) "s" else "",
        nrow(version_pin$renv_packages), plural(nrow(version_pin$renv_packages))) else "")
    summary_version_pin <- sprintf(
      "A pinned R/package environment was found (%s).",
      paste(version_pin$mechanisms, collapse = ", "))
    report_table_version_pin <- if (nrow(version_pin$renv_packages) > 0) {
      tb <- version_pin$renv_packages
      colnames(tb) <- c("Lockfile", "Package", "Version", "Source")
      tb
    } else NULL
  }

  ## merge packages into the manifest ----
  if (!is.null(manifest) && length(all_packages) > 0) {
    path <- manifest
    if (!grepl("\\.json$", path, ignore.case = TRUE)) {
      pid <- paper_id(paper)
      pid <- if (length(pid) && !is.na(pid[[1]])) pid[[1]] else "manifest"
      path <- file.path(path, paste0(pid, ".manifest.json"))
    }
    tryCatch(
      manifest_merge(path, list(code = list(
        packages = as.list(all_packages),
        ddi_mapping = list(
          "code.packages" = "otherMat/software (loaded packages)"
        )
      ))),
      error = function(e) NULL
    )
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
    "#### Working Directory (setwd)",
    report_setwd,
    scroll_table(report_table_setwd, maxrows = 5),
    "#### Libraries / Imports",
    report_library,
    "#### Packages / Dependencies",
    report_packages,
    "#### Reproducible Environment",
    report_version_pin,
    scroll_table(report_table_version_pin, maxrows = 10),
    "#### Parsable code",
    report_parse,
    scroll_table(report_table_parse)
  )

  # traffic_light ----
  # green only if no issues across all code files
  # parse_issues is a count, not a vector: length() on it is always 1, which
  # made green unreachable however clean the code was.
  # A green light means "we checked and found no issues", so it needs at least
  # one analysed file: a paper with only listed-but-unanalysed files (JASP) has
  # nothing to be green about.
  if (n_analysed == 0) {
    tl <- "na"
  } else if (length(missingfiles_issue) == 0 &&
      length(comment_issue) == 0 &&
      length(absolute_issues) == 0 &&
      length(setwd_issues) == 0 &&
      length(library_issue) == 0 &&
      parse_issues == 0 &&
      version_pin$pinned) {
    tl <- "green"
  } else {
    tl <- "yellow"
  }

  # Aggregate by paper
  summary_table <- code_files |>
    dplyr::summarise(
      code_n = dplyr::n(),
      code_checked = sum(checked, na.rm = TRUE),
      code_abs_path = sum(code_abs_path, na.rm = TRUE),
      code_setwd = if ("code_setwd" %in% names(code_files))
        sum(code_setwd, na.rm = TRUE) else 0L,
      code_missing_files = sum(loaded_files_missing, na.rm = TRUE),
      # Guard the all-NA group (e.g. a file with no parseable code lines):
      # min(na.rm = TRUE) would warn and return Inf, so fall back to NA.
      code_min_comments = if (any(!is.na(percentage_comment)))
        min(percentage_comment, na.rm = TRUE) else NA_real_,
      code_parse_errors = sum(parse_error, na.rm = TRUE),
      .by = paper_id
    )
  # Distinct packages per paper (union over that paper's files). Done as a
  # separate step (not inside summarise) because the union needs to split and
  # de-duplicate the comma-joined strings, which is awkward in a grouped
  # summarise; a small join keeps it correct for multi-paper paperlists.
  pkg_counts <- vapply(
    split(seq_len(nrow(code_files)), code_files$paper_id),
    function(rows) length(pkg_union(rows)), integer(1)
  )
  summary_table$code_packages_n <-
    pkg_counts[as.character(summary_table$paper_id)] |> unname()
  summary_table$code_packages_n[is.na(summary_table$code_packages_n)] <- 0L

  # Version-pinning is genuinely per-paper (renv.lock/sessionInfo.txt belong to
  # ONE paper's repository, unlike the whole-run `version_pin` used for the
  # report text above), so it needs its own per-paper split -- same shape as
  # pkg_counts just above, but keyed on all_files (renv.lock/sessionInfo.txt
  # are not in code_files at all, since code_lang() never classifies them).
  if ("paper_id" %in% names(all_files)) {
    pin_by_paper <- vapply(
      split(seq_len(nrow(all_files)), all_files$paper_id),
      function(rows) {
        pid_here <- as.character(all_files$paper_id[rows[1]])
        isTRUE(.code_version_pin_check(
          all_files[rows, , drop = FALSE],
          code_text_list = r_text_by_paper[[pid_here]] %||% list())$pinned)
      },
      logical(1))
    summary_table$code_version_pinned <-
      pin_by_paper[as.character(summary_table$paper_id)] |> unname()
  } else {
    summary_table$code_version_pinned <- version_pin$pinned
  }
  summary_table$code_version_pinned[is.na(summary_table$code_version_pinned)] <- FALSE

  # summary_text ----
  summary_text <- c(
    summary_code,
    summary_comments,
    summary_missingfiles,
    summary_absolute,
    summary_setwd,
    summary_library,
    summary_packages,
    summary_version_pin,
    summary_parse
  ) |>
    paste("\n- ", x = _, collapse = "")

  # table ----
  # file_location/file_path are kept (unlike earlier versions of this module):
  # reproducibility_check and other downstream consumers need a code file's
  # local path directly from this table, rather than re-deriving it from
  # data_check's structure table (which by default never downloads code
  # files at all -- see resolve_path() in reproducibility_check.R).
  table <- code_files

  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = c(code_n = 0),
    traffic_light = tl,
    report = report,
    summary_text = summary_text,
    # Exposed for reproducibility_check()'s Docker backend: picks the base
    # image's R version from version_pin$r_versions when this check found a
    # declared one (renv.lock/sessionInfo), falling back to "latest"
    # otherwise -- see .repro_docker_image_for() (R/reproducibility_check_docker.R).
    version_pin = version_pin
  )
}
