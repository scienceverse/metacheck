#' Convert a research folder into a clean, FAIR copy
#'
#' Writes a reorganised copy of a folder of research files (data, code,
#' materials, documentation) on your own computer. Nothing is downloaded and
#' no link is followed: only the folder you name is read, and it is not
#' changed. The folder is first copied to a temporary folder, and the checks
#' run on that copy, so nothing they write (such as code recovered from an
#' output file) can reach your folder; the copy is removed at the end. The
#' checks run the way [report_repository()] runs them: `repo_check` takes an
#' inventory of the folder, and `code_check`, `data_check`, `codebook_check`
#' and `reproducibility_check` (with `execute = FALSE`, so no code is run,
#' unless you set `execute`) build on it. Whether a language model is used is
#' decided by [llm_use()].
#'
#' Every file is placed where the Psych-DS layout puts it (data under `data/`,
#' code under `analysis/`, materials under `materials/`, documentation under
#' `documentation/`, one `study-<group>/` folder per study when there are
#' several; see [psychds_file_plan()]), keeping its own subfolders. A file a
#' script writes and no script reads, and an R data file holding a fitted
#' model, is a result and goes to `outputs/`. A data file in a format that
#' needs particular software (`.xlsx`, `.sav`, `.dta`, ...) is also written
#' as a real CSV file, with the original kept next to it; an Excel file with
#' several sheets gets one CSV per sheet, named after the sheet; an R data
#' file only when it holds a table and no CSV of the same name is next to
#' it. A table in a text file (`.txt`, `.tsv`) is kept as it is. A data CSV
#' file is copied without a byte order mark. The files of an archive package
#' (BagIt), which list the files as they were, go to
#' `logs/original-package/`. A file from an archive in the folder
#' that is identical to a file next to it, and a README identical to
#' another, are left out.
#'
#' R and Python scripts (`.R`, `.Rmd`, `.qmd`, `.py`, `.ipynb`) are changed so
#' they still find their files in the new layout, run from the top folder:
#' each file path that [repro_rewrite_paths()] can match to one file of the
#' folder is replaced by that file's new path, as is the path a script writes
#' a file to that is in the folder or goes to `outputs/`; and each
#' `setwd()`/`os.chdir()` line that sets a folder written out in the script,
#' no folder, or the script's own folder is replaced by a statement that does
#' nothing, with the original line kept in a comment (one that sets a folder
#' worked out when the code runs, such as `setwd(tempdir())`, is left). Every
#' change is reported, and listed in `CHANGES.md`. Scripts in other
#' languages (Stata, SPSS, MATLAB, ...) are copied unchanged, so their file
#' paths may no longer work.
#'
#' Code that `code_check` recovers from an output file (a rendered R Markdown
#' or Quarto page, an SPSS `.spv`, a Stata `.smcl` or an Mplus `.out` file)
#' is kept as a script in `analysis/`, unless the folder has the script it was
#' made from. In a README that the folder has, a link to a file of the folder
#' (such as an image) is changed to that file's new place.
#'
#' Metadata is written next to the files:
#' * `dataset_description.json` in each study folder with data (Psych-DS), describing
#'   every data column, with its label from the codebook when
#'   `codebook_check` found one;
#' * `ro-crate-metadata.json` at the top (RO-Crate 1.3): every file, the
#'   paper, the authors with their CRediT roles when the paper states them,
#'   the contact, funding (the paper's funding statement, or what
#'   `funding_check` finds in its text), the licence, the languages and
#'   packages the code uses, the software proprietary formats need, which
#'   files each script reads and writes, the missing-value codes, and how each
#'   study was run (DDI-Lifecycle terms). "unknown" is written only where the
#'   authors are asked for something: the licence, an author's CRediT roles,
#'   and when and how the data were collected and processed;
#' * `README.md`, a readable version of the RO-Crate, in the top folder and in
#'   each study folder, unless the folder has its own README there. Without a
#'   paper, the title is taken from the folder's own README;
#' * `requirements.txt`, the Python packages the code imports, for
#'   `pip install -r requirements.txt`, unless the folder has its own
#'   `requirements.txt`, `environment.yml` or `pyproject.toml`. R packages are
#'   listed in the README and the RO-Crate;
#' * `logs/`, the list of files and the results of the checks, without the
#'   places on this computer the files are in;
#' * `CHANGES.md`, and a `CHANGES.md` in each study folder that has none.
#'
#' @param path the folder to convert
#' @param output_dir the folder to write into; created if needed. Defaults to
#'   the folder's own name with `_FAIR` appended, in the working directory.
#' @param paper optionally, the paper that uses these files (see [read()]),
#'   for its title, authors, DOI, CRediT roles and funding. Its links are not
#'   followed. Without it, these are listed in the README as still to be
#'   filled in.
#' @param overwrite whether to replace an existing `output_dir`. When `FALSE`
#'   (the default) and `output_dir` exists, nothing is written and the
#'   returned list has `existed = TRUE`.
#' @param execute whether to run the R and Python scripts, as
#'   `reproducibility_check(execute = TRUE)` does: in a copy, with each script
#'   in its own R or Python process (see the `reproducibility_check` module for what
#'   that does and does not protect). Only then is a script found that uses a
#'   function of a package it does not load; the `library()` calls
#'   `reproducibility_check` adds for it are added at the top of the script
#'   in the copy. Default `FALSE`: no code is run.
#'
#' @returns (invisibly) a list with `output_dir`, `n_files_copied`,
#'   `n_studies`, `plan` (the file plan, with a `written` column: the path
#'   each file was written to, NA when it was not), `scripts` (one row per
#'   changed script: `target_path`, `paths_changed`, `setwd_removed`,
#'   `setwd_paths`, `libraries_added`), `skipped` (files that could not be
#'   read), `copy_failed`
#'   and `renamed` (a file whose target another file already took),
#'   `empty_plan` (TRUE when the folder had no files), and the paths of the
#'   metadata written: `descriptions`, `rocrate`, `requirements`, `logs` and
#'   `readme_changes`. When an existing `output_dir` was kept, the list has
#'   only `output_dir`, the counts, and `existed = TRUE`.
#' @export
#' @examples
#' \dontrun{
#' # convert a folder; the copy is written to "my_study_FAIR"
#' FAIR_repo("my_study")
#'
#' # with the paper, for its title, authors and funding
#' FAIR_repo("my_study", paper = read("my_study_paper.xml"))
#' }
FAIR_repo <- function(path, output_dir = NULL, paper = NULL,
                      overwrite = FALSE, execute = FALSE) {
  if (!is.character(path) || length(path) != 1 || is.na(path))
    stop("`path` must be a single path to a folder", call. = FALSE)
  if (!dir.exists(path)) stop("No folder found at ", path, call. = FALSE)
  if (!is.null(paper) && !.is_paper(paper))
    stop("`paper` must be a single paper object (see read())", call. = FALSE)
  full_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (is.null(output_dir)) output_dir <- paste0(basename(full_path), "_FAIR")
  out_full <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (identical(out_full, full_path) || startsWith(out_full, paste0(full_path, "/")))
    stop("`output_dir` must be outside the folder being converted", call. = FALSE)
  if (dir.exists(output_dir) && !overwrite) {
    message("The folder already exists, so nothing was written: ", output_dir,
            ". Set overwrite = TRUE to replace it.")
    return(invisible(list(output_dir = output_dir, n_files_copied = 0L,
                          n_studies = 0L, existed = TRUE)))
  }

  # The checks run on a copy of the folder in the temporary folder, so
  # nothing they write (code recovered from an output file, an unpacked
  # archive) can reach the folder being converted: it is only read, to make
  # the copy. The copy has the folder's name, so the file paths and the paper
  # id are the same. Only the files listed before the checks run are
  # converted.
  work <- .FAIR_repo_work_copy(full_path)
  on.exit(unlink(dirname(work), recursive = TRUE), add = TRUE)
  listed <- normalizePath(list.files(work, recursive = TRUE, all.files = TRUE,
                                     full.names = TRUE, no.. = TRUE),
                          winslash = "/", mustWork = FALSE)

  ops <- .FAIR_repo_run_checks(work, paper, execute)
  # Without a paper there is no title and there are no authors; the title is
  # then taken from the folder's own README, else its name (see
  # .FAIR_repo_collect()).
  if (is.null(paper)) {
    paper <- test_paper()
    paper$info$title <- NA_character_
    paper$author <- NULL
  }
  pid <- basename(full_path)

  dc <- ops[["data_check"]]
  structure_df <- .FAIR_repo_only_listed(dc$structure, work, listed)
  # psychds_file_plan() gives one row per file of data_check's file table, in
  # the same order, so row i of the plan is file i of structure_df.
  plan <- psychds_file_plan(structure_df, dc$group_no_evidence)$table

  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # With no files, the logs and the metadata are still written.
  empty_plan <- !nrow(plan)
  if (empty_plan) {
    message("No files were found to convert, so only the logs and the ",
            "metadata were written.")
    plan <- data.frame(file_name = character(0), data_type = character(0),
                       group = character(0), target_path = character(0),
                       convert = logical(0), original_target = character(0))
  }

  plan <- .FAIR_repo_adjust_plan(plan, structure_df, ops)
  plan <- .FAIR_repo_identical(plan, structure_df)
  recovered <- .FAIR_repo_add_recovered(plan, structure_df, ops)
  plan <- recovered$plan
  structure_df <- recovered$structure_df
  copied <- .FAIR_repo_copy(plan, structure_df, output_dir,
                            .FAIR_repo_added_libraries(ops))
  plan$written <- copied$written
  plan$same_as <- copied$same_as
  # what was actually written: a file that was no table is not converted, and
  # an original may have a numbered name
  plan$convert <- copied$converted
  plan$original_target <- copied$original_written
  plan$sheet_files <- copied$sheet_files
  plan$sheet_map <- copied$sheet_map
  # where the output file a recovered script came from was written
  plan$recovered_source <- plan$written[plan$recovered_row]
  attr(copied, "readme_links") <- .FAIR_repo_fix_readme_links(plan, structure_df,
                                                               output_dir)
  meta <- .FAIR_repo_write_metadata(paper, pid, ops, plan, structure_df,
                                    copied, output_dir, folder = work)
  if (!empty_plan) .FAIR_repo_messages(plan, copied)

  study_dirs <- unique(sub("^(study-[^/]+)/.*$", "\\1",
                           grep("^study-", stats::na.omit(plan$written),
                                value = TRUE)))
  invisible(c(list(
    output_dir     = output_dir,
    n_files_copied = sum(!is.na(plan$written)),
    n_studies      = if (all(is.na(plan$written))) 0L else max(1L, length(study_dirs)),
    plan           = plan,
    scripts        = copied$scripts,
    skipped        = plan$file_name[copied$skipped],
    copy_failed    = plan$file_name[copied$failed],
    renamed        = copied$renamed,
    identical      = .FAIR_repo_identical_table(plan),
    empty_plan     = empty_plan
  ), meta))
}

# Run the checks FAIR_repo() uses on a local folder, as report_repository()
# does: repo_check reads the folder, and the others build on its result.
# local_only is given to every check that has it, so none of them looks for a
# repository online, also when the paper links one. reproducibility_check
# reads the code without running it. funding_check runs only on a paper.
# A failed check stops the conversion with its own error.
.FAIR_repo_run_checks <- function(path, paper = NULL, execute = FALSE) {
  modules <- c("repo_check", "code_check", "data_check", "codebook_check",
               "reproducibility_check", if (!is.null(paper)) "funding_check")
  args <- list(
    repo_check = list(local_path = path, local_only = TRUE),
    code_check = list(local_only = TRUE),
    data_check = list(local_only = TRUE),
    codebook_check = list(local_only = TRUE),
    reproducibility_check = list(local_only = TRUE, execute = isTRUE(execute)))
  ops <- report_module_run(paper %||% test_paper(), modules, args = args)
  for (m in modules) {
    mo <- ops[[m]]
    if (!is.null(mo) && identical(mo$traffic_light, "fail"))
      stop("The '", m, "' check failed, so the folder cannot be converted:\n  ",
           paste(mo$report %||% "unknown error", collapse = "\n  "),
           call. = FALSE)
  }
  ops
}

# Copy the folder at `path` (every file, hidden ones too, with their dates)
# into a new folder in the temporary folder, and return the copy's path. The
# checks run on the copy, so the folder itself is only read. Stops when not
# every file was copied: converting part of a folder would leave files out
# without saying so.
.FAIR_repo_work_copy <- function(path) {
  parent <- tempfile("metacheck-fair-")
  dir.create(parent)
  ok <- file.copy(path, parent, recursive = TRUE, copy.date = TRUE)
  work <- file.path(parent, basename(path))
  count <- function(d) length(list.files(d, recursive = TRUE, all.files = TRUE,
                                         no.. = TRUE))
  n_in <- count(path)
  n_out <- if (dir.exists(work)) count(work) else 0L
  if (!isTRUE(ok) || n_out < n_in) {
    unlink(parent, recursive = TRUE)
    stop("The folder could not be copied to the temporary folder (", n_out,
         " of ", n_in, " files were copied), so it was not converted: ", path,
         call. = FALSE)
  }
  normalizePath(work, winslash = "/")
}

# data_check's file table without the files in the copy that were not there
# before the checks ran (`listed`): a check made them, not the authors. Files
# outside the copy, such as the files of an archive unpacked in the
# temporary folder, are kept.
.FAIR_repo_only_listed <- function(structure_df, work, listed) {
  loc <- structure_df$file_location
  if (!is.data.frame(structure_df) || !nrow(structure_df) || is.null(loc))
    return(structure_df)
  norm <- tolower(normalizePath(loc, winslash = "/", mustWork = FALSE))
  inside <- !is.na(loc) & startsWith(norm, tolower(paste0(work, "/")))
  structure_df[!(inside & !norm %in% tolower(listed)), , drop = FALSE]
}

# Code that code_check recovered from an output file (a rendered R Markdown
# page, an SPSS .spv, a Stata .smcl or an Mplus .out file) is kept as a
# script: a row is added to the plan and to the file table for each, placed in
# analysis/ with the subfolders of its output file ("documentation/sub/x.html"
# gives "analysis/sub/x.R"), so it is copied, and its paths changed, like the
# authors' own scripts. `recovered_row` is the plan row of the output file.
# Not when the folder has a script of that name (the .Rmd the page was made
# from): the recovered code would be a second copy of it.
.FAIR_repo_add_recovered <- function(plan, structure_df, ops) {
  plan$recovered_row <- rep(NA_integer_, nrow(plan))
  none <- list(plan = plan, structure_df = structure_df)
  cc <- ops[["code_check"]]$table
  if (!is.data.frame(cc) || !nrow(cc) || !"recovered_from" %in% names(cc) ||
      !nrow(plan)) return(none)
  loc <- cc$file_location %||% rep(NA_character_, nrow(cc))
  rec <- cc[!is.na(cc$recovered_from) & !is.na(loc) &
              file.exists(ifelse(is.na(loc), "", loc)), , drop = FALSE]
  rec <- rec[!duplicated(rec$file_location), , drop = FALSE]
  if (!nrow(rec)) return(none)

  paths <- structure_df$file_path %||% structure_df$file_name
  stem <- function(p) tolower(tools::file_path_sans_ext(basename(gsub("\\\\", "/", p))))
  own_scripts <- stem(paths[plan$data_type %in% "code"])
  for (k in seq_len(nrow(rec))) {
    if (stem(rec$file_name[k]) %in% own_scripts) next
    src <- match(rec$recovered_from[k], paths)
    src_target <- if (is.na(src)) NA_character_ else plan$target_path[src]
    dir <- if (is.na(src_target) || !nzchar(src_target)) "analysis" else
      sub("^((study-[^/]+/)?)[^/]+(/|$)", "\\1analysis\\3", dirname(src_target))
    srow <- structure_df[if (is.na(src)) NA_integer_ else src, , drop = FALSE]
    srow$file_name <- rec$file_name[k]
    if ("file_path" %in% names(srow)) srow$file_path <- rec$file_path[k]
    # its own name, not its output file's, in what identifies a file
    if ("source_file" %in% names(srow)) srow$source_file <- rec$file_path[k]
    srow$file_location <- rec$file_location[k]
    srow$data_type <- "code"
    prow <- plan[if (is.na(src)) NA_integer_ else src, , drop = FALSE]
    prow$file_name <- rec$file_name[k]
    prow$data_type <- "code"
    prow$target_path <- paste0(dir, "/", rec$file_name[k])
    prow$convert <- FALSE
    prow$original_target <- NA_character_
    if ("same_as" %in% names(prow)) prow$same_as <- NA_character_
    if ("script_writes" %in% names(prow)) prow$script_writes <- FALSE
    prow$recovered_row <- src
    structure_df <- rbind(structure_df, srow)
    plan <- rbind(plan, prow)
    own_scripts <- c(own_scripts, stem(rec$file_name[k]))
  }
  rownames(plan) <- NULL
  rownames(structure_df) <- NULL
  list(plan = plan, structure_df = structure_df)
}

# Write the metadata files (see R/fair-repo-metadata.R) and return their
# paths: `descriptions` (dataset_description.json per study), `rocrate`,
# `requirements`, `logs`, and `readme_changes` (the README and CHANGES files
# metacheck wrote).
.FAIR_repo_write_metadata <- function(paper, pid, ops, plan, structure_df,
                                      copied, output_dir, folder = NULL) {
  columns_df <- ops[["data_check"]]$table
  labels_df  <- ops[["codebook_check"]]$table
  roots <- .FAIR_repo_roots(plan)
  reused <- .FAIR_repo_reused_by_group(plan)
  written <- as.character(plan$written)
  written[is.na(written)] <- ""
  # what the README, the RO-Crate and the dataset descriptions are written from
  info <- .FAIR_repo_collect(paper, pid, ops, plan, structure_df, output_dir)
  # each data file's path in the copy, by data_check's name for it (an
  # identical file left out: the copy that was kept)
  sf <- structure_df$source_file %||% structure_df$file_name
  new_path <- ifelse(is.na(plan$written), plan$same_as %||% NA_character_, plan$written)
  new_path <- stats::setNames(new_path, sf)[!is.na(new_path) & !is.na(sf) & !duplicated(sf)]
  # the columns of a sheet are in the CSV of that sheet
  sheet_map <- plan$sheet_map %||% rep(list(character(0)), nrow(plan))
  for (i in which(lengths(sheet_map) > 0 & !is.na(sf))) {
    for (s in names(sheet_map[[i]])) {
      key <- .FAIR_repo_sheet_key(sf[i], s, columns_df)
      if (!is.na(key)) new_path[key] <- sheet_map[[i]][[s]]
    }
  }

  # dataset_description.json for each study with a data/ folder (a Psych-DS
  # dataset has data; a folder of code only is not one); a repository's own
  # dataset_description.json is kept.
  descriptions <- character(0)
  for (root in roots) {
    prefix <- if (nzchar(root$dir)) paste0(root$dir, "/") else ""
    # with every sheet's CSV: the first sheet can be a README in documentation/
    all_written <- c(written, unlist(plan$sheet_files))
    if (!any(startsWith(all_written, paste0(prefix, "data/")))) next
    dest <- file.path(output_dir, root$dir, "dataset_description.json")
    if (file.exists(dest)) next
    cols <- if (is.null(root$grp) || !is.data.frame(columns_df)) columns_df else
      columns_df[columns_df$group %in% root$grp, , drop = FALSE]
    variables <- .FAIR_repo_variable_measured(cols, labels_df, new_path)
    # every column of every CSV in data/ is described, also those data_check
    # did not read (a sheet of notes it left out): Psych-DS requires it
    csvs <- all_written[startsWith(all_written, paste0(prefix, "data/")) &
                          grepl("\\.csv$", all_written)]
    variables <- c(variables, .FAIR_repo_missing_columns(
      file.path(output_dir, csvs), csvs, vapply(variables, `[[`, "", "name")))
    desc <- .FAIR_repo_dataset_description(
      paper, info$title, .FAIR_repo_label(root$grp), variables,
      if (is.null(root$grp)) character(0) else reused[[root$grp]] %||% character(0))
    .FAIR_repo_write_json(desc, dest)
    descriptions <- c(descriptions, dest)
  }

  # The paper's full text is not copied into the folder for now. To write it
  # as documentation/fulltext.txt again, remove the # from the lines marked
  # "full text" here and below, and from .FAIR_repo_write_paper_text() in
  # R/fair-repo-metadata.R, and delete `fulltext <- NULL`.
  # fulltext <- .FAIR_repo_write_paper_text(paper, output_dir, pid)  # full text
  fulltext <- NULL
  logs <- .FAIR_repo_write_logs(ops, structure_df, pid, output_dir, folder)
  requirements <- .FAIR_repo_write_requirements(info$software, plan, output_dir)
  readme_changes <- .FAIR_repo_write_readme_changes(plan, copied, info, output_dir,
                                                    columns_df, labels_df)

  rel <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    top <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    substring(p, nchar(top) + 2L)
  }
  rocrate <- .FAIR_repo_rocrate_json(
    info, output_dir, plan, labels_df,
    extra_files = rel(c(descriptions, fulltext, names(logs), requirements,
                        readme_changes)))

  list(descriptions = descriptions, rocrate = rocrate,
       # fulltext = fulltext %||% character(0),  # full text
       requirements = requirements %||% character(0),
       logs = names(logs), readme_changes = readme_changes)
}

# A PropertyValue for each column of the CSV files (`paths`, written as
# `rel` in the copy) whose name is not in `known`, taken from the header row.
.FAIR_repo_missing_columns <- function(paths, rel, known) {
  out <- list()
  for (k in seq_along(paths)) {
    cols <- tryCatch(names(utils::read.csv(paths[k], nrows = 1, check.names = FALSE)),
                     error = function(e) character(0))
    for (cl in setdiff(cols, c(known, vapply(out, `[[`, "", "name")))) {
      out[[length(out) + 1]] <- list(`@type` = "PropertyValue", name = cl,
                                     `metacheck:sourceFile` = rel[k])
    }
  }
  out
}

# Where results, R data files, text tables and an archive package's own files
# go, decided before anything is copied, so the scripts' paths are changed to
# their final places.
# * A file a script writes and no script reads is a result: it goes to
#   outputs/, under its own name. (A file one script writes and another
#   reads, such as cleaned data, stays data.) reproducibility_check gives
#   what each script reads and writes, found by reading the code.
# * An R data file (.rds, .RData, .rda) holding a fitted model is a result
#   too. One holding no table, or with a CSV of the same name next to it, is
#   kept as it is instead of written again as CSV: an .RData next to its own
#   .csv became a second table under a numbered name.
# * A table in a text file (.txt, .tsv, .dat, .tab) is already open and
#   readable, so it is kept as it is, without a CSV copy next to it.
# * The files of a BagIt package (bagit.txt, bag-info.txt, manifest-md5.txt,
#   ...; data archives such as Dryad add them) list the files as they were:
#   after the reorganisation their paths and checksums no longer match, so
#   they go to logs/original-package/ instead of documentation/.
.FAIR_repo_adjust_plan <- function(plan, structure_df, ops) {
  n <- nrow(plan)
  if (!n) return(plan)
  loc <- structure_df$file_location %||% rep(NA_character_, n)
  name <- basename(gsub("\\\\", "/", plan$file_name))
  to_outputs <- function(i) {
    dir <- dirname(plan$target_path[i])
    # "study-ex1/data/scen_a" becomes "study-ex1/outputs/scen_a"
    dir <- sub("^((study-[^/]+/)?)[^/]+", "\\1outputs", dir)
    plan$target_path[i] <<- paste0(dir, "/", name[i])
    plan$convert[i] <<- FALSE
    plan$original_target[i] <<- NA_character_
  }
  keep_as_is <- function(i) {
    if (!is.na(plan$original_target[i])) plan$target_path[i] <<- plan$original_target[i]
    plan$convert[i] <<- FALSE
    plan$original_target[i] <<- NA_character_
  }

  repro <- ops[["reproducibility_check"]]$table
  plan$script_writes <- FALSE
  if (is.data.frame(repro) && all(c("reads", "writes") %in% names(repro))) {
    reads <- tolower(basename(unlist(repro$reads)))
    writes <- tolower(basename(unlist(repro$writes)))
    result <- setdiff(writes, reads)
    # a file a script writes: its new path goes into that script too (see
    # .FAIR_repo_point_to_new_paths())
    plan$script_writes <- tolower(name) %in% writes & !plan$data_type %in% "code"
    for (i in which(tolower(name) %in% result & !is.na(plan$target_path) &
                    !plan$data_type %in% "code"))
      to_outputs(i)
  }

  ext <- tolower(tools::file_ext(name))
  for (i in which(ext %in% c("txt", "tsv", "dat", "tab") & plan$convert %in% TRUE))
    keep_as_is(i)
  bagit <- grepl("^(bagit\\.txt|bag-info\\.txt|(tag)?manifest-[a-z0-9]+\\.txt)$",
                 tolower(name)) & !is.na(plan$target_path)
  for (i in which(bagit)) {
    plan$target_path[i] <- paste0("logs/original-package/", name[i])
    plan$convert[i] <- FALSE
    plan$original_target[i] <- NA_character_
  }
  r_data <- which(ext %in% c("rds", "rdata", "rda") & !is.na(plan$target_path) &
                    !startsWith(sub("^study-[^/]+/", "", plan$target_path), "outputs/"))
  csv_here <- tolower(paste0(dirname(plan$current_path %||% name), "/",
                             tools::file_path_sans_ext(name)))[ext == "csv"]
  for (i in r_data) {
    if (is.na(loc[i]) || !file.exists(loc[i])) next
    kind <- .FAIR_repo_r_data_kind(loc[i])
    stem <- tolower(paste0(dirname(plan$current_path[i] %||% name[i]), "/",
                           tools::file_path_sans_ext(name[i])))
    if (kind == "model") to_outputs(i)
    else if (kind != "table" || stem %in% csv_here) keep_as_is(i)
  }
  plan
}

# What an R data file holds: "table" (what data_read_head() reads as a data
# frame), "model" (a fitted model: an object with a model call, or of a
# common model class), or "other".
.FAIR_repo_r_data_kind <- function(path) {
  # an object of a package that is not installed loads with a warning
  # ("namespace 'rjags' is not available"), which says nothing here
  objs <- tryCatch(suppressWarnings({
    if (grepl("\\.rds$", path, ignore.case = TRUE)) list(readRDS(path)) else {
      e <- new.env()
      nms <- load(path, envir = e)
      mget(nms, envir = e)
    }
  }), error = function(e) list())
  if (!length(objs)) return("other")
  is_model <- function(o) {
    inherits(o, c("lm", "glm", "merMod", "lmerMod", "glmerMod", "brmsfit",
                  "stanfit", "stanreg", "gam", "lavaan", "rfsrc",
                  "randomForest", "train", "nls", "coxph", "survreg",
                  "MCMCglmm", "glmmTMB", "betareg", "polr", "clm", "htest")) ||
      (is.list(o) && !is.data.frame(o) && is.call(o$call)) ||
      (isS4(o) && "call" %in% methods::slotNames(o))
  }
  if (any(vapply(objs, is_model, logical(1)))) return("model")
  if (any(vapply(objs, function(o) is.data.frame(o) || is.matrix(o), logical(1))))
    return("table")
  "other"
}

# Mark the files the copy does not need twice, in a column `same_as`: the
# place of the identical file that is kept, else NA.
# * A file from an archive in the folder that is identical to a file outside
#   it: data_check unpacks a .zip, so a folder holding both data.zip and its
#   unpacked files had every file twice.
# * A README identical to another README; the one highest in the folder is
#   kept.
# A left-out file's target_path becomes the kept file's, so a script that
# reads it is pointed at the kept copy.
.FAIR_repo_identical <- function(plan, structure_df) {
  n <- nrow(plan)
  plan$same_as <- rep(NA_character_, n)
  if (!n) return(plan)
  loc <- structure_df$file_location %||% rep(NA_character_, n)
  path <- gsub("\\\\", "/", structure_df$file_path %||% plan$file_name)
  path[is.na(path)] <- plan$file_name[is.na(path)]
  on_disk <- !is.na(loc) & file.exists(loc) & !is.na(plan$target_path)
  # a file listed from inside an archive ("data.zip/x.csv") or from where it
  # was unpacked ("data.zip.contents/x.csv")
  in_zip <- grepl("(^|/)[^/]*\\.(zip|tar|tgz|gz|7z|rar)(\\.contents)?/", path,
                  ignore.case = TRUE)
  readme <- (structure_df$doc_role %||% rep(NA_character_, n)) %in% "readme"

  # only files that could have a twin are read for their checksum
  base <- tolower(basename(path))
  maybe <- on_disk & (in_zip | readme |
                        base %in% base[in_zip] | base %in% base[readme])
  h <- rep(NA_character_, n)
  h[maybe] <- unname(tools::md5sum(loc[maybe]))

  for (i in which(in_zip & !is.na(h))) {
    twin <- which(!in_zip & h %in% h[i])[1]
    if (!is.na(twin)) plan$same_as[i] <- plan$target_path[twin]
  }
  rows <- which(readme & !is.na(h) & is.na(plan$same_as))
  depth <- lengths(strsplit(path[rows], "/", fixed = TRUE))
  seen <- character(0)
  for (i in rows[order(depth)]) {
    if (h[i] %in% names(seen)) plan$same_as[i] <- seen[[h[i]]]
    else seen[h[i]] <- plan$target_path[i]
  }
  left_out <- !is.na(plan$same_as)
  plan$target_path[left_out] <- plan$same_as[left_out]
  plan
}

# The files left out because an identical one is in the copy: their path in
# the original folder, and the place of the kept file.
.FAIR_repo_identical_table <- function(plan) {
  rows <- which(!is.na(plan$same_as %||% rep(NA, nrow(plan))))
  path <- plan$current_path[rows] %||% plan$file_name[rows]
  data.frame(path = ifelse(is.na(path), plan$file_name[rows], path),
             same_as = plan$same_as[rows])
}

# Copy every planned file to its place in output_dir. Each plan row's file is
# structure_df's row of the same number (see FAIR_repo()), never looked up by
# name: names repeat across a paper's repositories.
#
# A data file planned for conversion is written as a real CSV, and its
# original is copied next to it. A data CSV is copied without a byte order
# mark. An R or Python script gets its file paths and setwd() lines changed
# (.FAIR_repo_write_script()). When a second file is planned for a path
# already written, an identical copy is left out and a different file is
# written under a numbered name ("x_2.csv").
#
# Rows marked by .FAIR_repo_identical() are not copied.
#
# Returns list(written = the path written per plan row, or NA; scripts =
# changes per script; skipped / failed = plan rows not on disk / not
# copied; renamed = data frame of from/to paths; same_as = per plan row, the
# place of the identical file that was kept instead, or NA).
.FAIR_repo_copy <- function(plan, structure_df, output_dir, libraries = list()) {
  n <- nrow(plan)
  written <- rep(NA_character_, n)
  skipped <- integer(0)
  failed  <- integer(0)
  renamed <- data.frame(file_name = character(0), planned = character(0),
                        written = character(0))
  scripts <- list()
  src_all <- structure_df$file_location %||% rep(NA_character_, n)
  convert <- plan$convert %in% TRUE
  converted <- convert          # FALSE where the file was no table after all
  original_written <- rep(NA_character_, n)
  sheet_files <- rep(list(character(0)), n)   # the CSVs of a file's other sheets
  sheet_map <- rep(list(character(0)), n)     # sheet name -> its CSV
  hashes  <- list()   # target path -> md5 of the file written there
  same_as <- plan$same_as %||% rep(NA_character_, n)

  # The path to write to: the planned one, unless another file took it. NULL
  # when an identical file is already there.
  place <- function(target, src, h = unname(tools::md5sum(src))) {
    if (is.null(hashes[[target]])) {
      hashes[[target]] <<- h
      return(target)
    }
    if (identical(hashes[[target]], h)) return(NULL)
    stem <- tools::file_path_sans_ext(target)
    ext <- tools::file_ext(target)
    k <- 2L
    repeat {
      alt <- paste0(stem, "_", k, if (nzchar(ext)) paste0(".", ext))
      if (is.null(hashes[[alt]])) { hashes[[alt]] <<- h; return(alt) }
      if (identical(hashes[[alt]], h)) return(NULL)
      k <- k + 1L
    }
  }

  for (i in seq_len(n)) {
    target <- plan$target_path[i]
    if (is.na(target) || !nzchar(target) || !is.na(same_as[i])) next
    src <- src_all[i]
    if (is.na(src) || !nzchar(src) || !file.exists(src)) {
      skipped <- c(skipped, i)
      next
    }
    to <- place(target, src)
    if (is.null(to)) {   # an identical copy is already written
      same_as[i] <- target
      next
    }
    if (to != target)
      renamed <- rbind(renamed, data.frame(file_name = plan$file_name[i],
                                           planned = target, written = to))
    dest <- file.path(output_dir, to)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

    is_data_csv <- grepl("\\.csv$", to, ignore.case = TRUE) &&
      grepl("^(study-[^/]+/)?data/", to)
    script <- if (identical(plan$data_type[i], "code"))
      .FAIR_repo_write_script(src, plan$file_name[i], dest, to, plan,
                              structure_df,
                              .FAIR_repo_libraries_for(libraries, plan, i)) else NULL
    if (convert[i]) {
      # The original is kept next to the CSV, placed like any other file, so
      # two different originals with one name do not overwrite each other.
      orig <- plan$original_target[i]
      orig_to <- if (!is.na(orig) && nzchar(orig)) place(orig, src) else NULL
      orig_ok <- !is.null(orig_to) && {
        odest <- file.path(output_dir, orig_to)
        dir.create(dirname(odest), recursive = TRUE, showWarnings = FALSE)
        isTRUE(file.copy(src, odest, overwrite = TRUE))
      }
      if (orig_ok) original_written[i] <- orig_to
      # A spreadsheet with several sheets gets one CSV per sheet holding a
      # table, named after the sheet: only the first sheet was written, so
      # the CSV lacked the other sheets, or held a README sheet instead of
      # the data.
      sheets <- .data_file_sheets(src)
      if (length(sheets) > 1) {
        made <- character(0)
        for (k in seq_along(sheets)) {
          # each sheet is its own content: the file's checksum and the sheet
          sheet_to <- place(.FAIR_repo_sheet_target(to, sheets[k], k), src,
                            h = paste(unname(tools::md5sum(src)), k))
          if (is.null(sheet_to)) next
          sdest <- file.path(output_dir, sheet_to)
          if (.FAIR_repo_write_data_csv(src, sdest, sheet = k))
            made <- c(made, stats::setNames(sheet_to, sheets[k]))
        }
        if (length(made)) {
          written[i] <- made[[1]]
          sheet_files[[i]] <- unname(made[-1])
          sheet_map[[i]] <- made
          next
        }
      }
      if (.FAIR_repo_write_data_csv(src, dest)) {
        written[i] <- to
      } else {
        # Not a table (an .rds holding a fitted model or a list): the
        # original is the file itself.
        unlink(dest)
        converted[i] <- FALSE
        original_written[i] <- NA_character_
        if (orig_ok) written[i] <- orig_to else failed <- c(failed, i)
      }
      next
    }
    ok <- if (!is.null(script)) {
      scripts[[to]] <- script
      TRUE
    } else if (is_data_csv) {
      .FAIR_repo_copy_no_bom(src, dest)
    } else {
      isTRUE(file.copy(src, dest, overwrite = TRUE))
    }
    if (ok) written[i] <- to else failed <- c(failed, i)
  }

  scripts <- if (length(scripts)) dplyr::bind_rows(scripts) else
    data.frame(target_path = character(0), paths_changed = integer(0),
               setwd_removed = integer(0), setwd_paths = character(0),
               libraries_added = character(0))
  list(written = written, scripts = scripts, skipped = skipped,
       failed = unique(failed), renamed = renamed, same_as = same_as,
       converted = converted, original_written = original_written,
       sheet_files = sheet_files, sheet_map = sheet_map,
       not_tables = which(convert & !converted & !is.na(written)))
}

# Write an R or Python script with its file paths changed to the new layout
# and its setwd()/os.chdir() lines replaced. The paths are found in the code
# (for an .Rmd/.qmd its R chunks, for a notebook its code cells) and changed
# in the whole file, each only where it stands in quotes. Returns NULL for a
# file that is not an R or Python script (the caller copies it unchanged),
# else a one-row data frame of what was changed.
.FAIR_repo_write_script <- function(src, file_name, dest, target, plan,
                                    structure_df, libraries = character(0)) {
  ext <- tolower(tools::file_ext(file_name))
  if (!ext %in% c("r", "rmd", "qmd", "py", "ipynb")) return(NULL)
  # a notebook or Quarto document declares its language inside the file
  lang <- unname(code_lang(if (ext %in% c("ipynb", "qmd")) src else file_name))
  if (is.na(lang) || !lang %in% c("R", "Python")) return(NULL)
  # Python chunks of a Quarto document are not extracted
  if (ext == "qmd" && lang != "R") return(NULL)

  raw <- tryCatch(code_read(src), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  code <- tryCatch(
    if (ext %in% c("rmd", "qmd")) code_extract_r(src)
    else if (ext == "ipynb") code_extract_py(src)
    else raw,
    error = function(e) character(0))

  rw <- repro_rewrite_paths(code, target, plan, lang,
                            structure_df = structure_df)
  txt <- .repro_apply_rewrites(raw, rw)
  pointed <- .FAIR_repo_point_to_new_paths(txt, plan)
  txt <- pointed$txt
  # A notebook's lines are JSON strings, which the setwd() pattern does not
  # match, so a notebook keeps its setwd()/os.chdir() calls.
  sw <- .FAIR_repo_neutralise_setwd(txt, lang, parse = ext == "r")
  txt <- sw$txt
  # the library() calls reproducibility_check added when it ran the script
  # (execute = TRUE), for a function of a package the script did not load
  libraries <- if (ext == "r") libraries[!is.na(libraries) & nzchar(libraries)] else character(0)
  if (length(libraries))
    txt <- c(sprintf("library(%s)  # [metacheck FAIR_repo added: the script uses a function of this package without loading it]",
                     libraries), txt)

  # A script with nothing to change is copied as it is, so its line endings
  # and encoding stay the authors'.
  ok <- if (identical(txt, raw)) {
    isTRUE(file.copy(src, dest, overwrite = TRUE))
  } else {
    tryCatch({
      con <- file(dest, open = "wb", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      writeLines(txt, con, useBytes = TRUE)
      TRUE
    }, error = function(e) FALSE)
  }
  if (!ok) return(NULL)

  changed <- if (nrow(rw)) rw$matched & !rw$ambiguous & !is.na(rw$target) &
    nzchar(rw$target) & !(rw$as_written %||% FALSE) & rw$target != rw$ref else
    logical(0)
  data.frame(target_path = target, paths_changed = sum(changed) + pointed$n,
             setwd_removed = sw$n,
             setwd_paths = paste(sw$paths, collapse = ", "),
             libraries_added = paste(libraries, collapse = ", "))
}

# The packages reproducibility_check added library() calls for to the script
# of plan row i (see .FAIR_repo_added_libraries()), found by its file name;
# none when two scripts share that name, since which one was run is unknown.
.FAIR_repo_libraries_for <- function(libraries, plan, i) {
  fn <- basename(gsub("\\\\", "/", plan$file_name[i]))
  code_names <- basename(gsub("\\\\", "/", plan$file_name[plan$data_type %in% "code"]))
  if (!length(libraries) || sum(code_names == fn) != 1) return(character(0))
  libraries[[fn]] %||% character(0)
}

# The library() calls reproducibility_check added to a script it ran (with
# execute = TRUE) because the script used a function of a package it did not
# load: a list, script file name -> packages. Empty when no code was run.
.FAIR_repo_added_libraries <- function(ops) {
  m <- ops[["reproducibility_check"]]$modifications
  if (!is.data.frame(m) || !nrow(m) || !"change_type" %in% names(m)) return(list())
  m <- m[m$change_type %in% "library_injected", , drop = FALSE]
  if (!nrow(m)) return(list())
  split(sub("^library\\((.*)\\)$", "\\1", m$detail), basename(m$file_name))
}

# Replace a script's setwd()/os.chdir() calls that set a folder written out in
# the script (an absolute path on the authors' computer, or a relative folder,
# which no longer holds what it did), set no folder, or set the script's own
# folder (rstudioapi's document path, sys.frame(1)$ofile, __file__), with a
# statement that does nothing (see .repro_neutralise_setwd()). The scripts'
# file paths now start at the top folder, so any of these sends them to the
# wrong place. A folder computed when the code runs (setwd(tempdir()), or
# setwd(old) to go back to where it was) is left: replacing those in a
# package's tests (100 lines in one folder) changed where the tests write.
# With `parse`, an R script is parsed so only real calls count, not the text
# "setwd(" inside a string. Returns list(txt, n, paths), as
# .repro_neutralise_setwd() does.
.FAIR_repo_neutralise_setwd <- function(txt, lang, parse = FALSE) {
  call_pat <- if (identical(lang, "Python"))
    "^\\s*(?:[A-Za-z_][A-Za-z0-9_]*\\.)?chdir\\s*\\((.*)$" else
    "^\\s*(?:base::)?setwd\\s*\\((.*)$"
  hit <- grepl(call_pat, txt, perl = TRUE)
  if (parse && any(hit)) {
    pd <- tryCatch(utils::getParseData(parse(text = txt, keep.source = TRUE)),
                   error = function(e) NULL)
    if (!is.null(pd))
      hit <- hit & seq_along(txt) %in%
        pd$line1[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "setwd"]
  }
  arg <- sub(call_pat, "\\1", txt, perl = TRUE)
  replace <- hit & (
    grepl("^\\s*\\)", arg) |                                # no folder
      grepl("^\\s*(['\"])[^'\"]*\\1\\s*\\)", arg) |         # a written-out folder
      grepl("getActiveDocumentContext|getSourceEditorContext|sys\\.frame|this\\.path|thisfile|__file__",
            arg))                                           # the script's own folder
  if (!any(replace)) return(list(txt = txt, n = 0L, paths = character(0)))
  sw <- .repro_neutralise_setwd(txt[replace], lang, by = "metacheck FAIR_repo")
  txt[replace] <- sw$txt
  list(txt = txt, n = sw$n, paths = sw$paths)
}

# Point a script's quoted paths to the new place of the files scripts write:
# a script that writes write.csv(x, "results/table.csv") writes
# "outputs/table.csv" in the copy, where the shared table is, and one that
# writes data another script reads writes it under its new name, which the
# reading script now uses. repro_rewrite_paths() changes the files a script
# reads, not the ones it writes: a script that wrote
# "data/phylogeny/phy_spp_names.csv" kept writing it, while 22 scripts read
# "data/phylogeny/study-physppnames_data.csv". A quoted string counts when
# its last part is the original name of such a file. Returns list(txt, n =
# the number of strings changed).
.FAIR_repo_point_to_new_paths <- function(txt, plan) {
  written_by_script <- plan$script_writes %||% rep(FALSE, nrow(plan))
  # a file written as CSV with its original kept: a script writes the original
  place <- ifelse(plan$convert %in% TRUE & !is.na(plan$original_target %||% NA),
                  plan$original_target %||% NA_character_, plan$target_path)
  out <- which(!is.na(place) &
                 (grepl("^(study-[^/]+/)?outputs/", place) | written_by_script %in% TRUE))
  if (!length(out)) return(list(txt = txt, n = 0L))
  name <- tolower(basename(gsub("\\\\", "/", plan$file_name)))
  target_of <- stats::setNames(place[out], name[out])
  # one name for two different files: which one a script means is unknown (an
  # identical copy that is left out does not count)
  kept <- name[!is.na(plan$target_path) & is.na(plan$same_as %||% NA)]
  target_of <- target_of[!names(target_of) %in% kept[duplicated(kept)]]
  n <- 0L
  m <- gregexpr("([\"'])([^\"'\n]+)\\1", txt, perl = TRUE)
  found <- regmatches(txt, m)
  for (k in seq_along(found)) {
    for (s in unique(found[[k]])) {
      inner <- substring(s, 2, nchar(s) - 1)
      key <- tolower(basename(gsub("\\\\", "/", inner)))
      if (!key %in% names(target_of) || inner == target_of[[key]]) next
      q <- substring(s, 1, 1)
      txt[k] <- gsub(s, paste0(q, target_of[[key]], q), txt[k], fixed = TRUE)
      n <- n + 1L
    }
  }
  list(txt = txt, n = n)
}

# Change the links in the copied READMEs (Markdown, R Markdown, Quarto and
# HTML) that point to a file of the folder, such as an image in
# man/figures/, to that file's new place: the README kept its place but the
# file moved, so the images no longer showed. Only the link changes, in the
# copy; a link to a web page, to a part of the page, or to a file that is not
# in the folder is left as it is. A link is read from the README's own
# folder, as in the folder as it was. Returns the READMEs changed.
.FAIR_repo_fix_readme_links <- function(plan, structure_df, output_dir) {
  if (!nrow(plan) || is.null(plan$written)) return(character(0))
  orig <- gsub("\\\\", "/", structure_df$file_path %||% structure_df$file_name)
  orig <- sub("^\\./", "", orig)
  readme <- which(!is.na(plan$written) &
                    grepl("^readme.*\\.(md|markdown|rmd|qmd|html?)$",
                          basename(orig), ignore.case = TRUE))
  # the written place of each file, by its path in the folder as it was
  place <- ifelse(is.na(plan$written), plan$same_as %||% NA_character_, plan$written)
  place_of <- stats::setNames(place, tolower(orig))[!is.na(place)]
  changed <- character(0)
  for (i in readme) {
    dest <- file.path(output_dir, plan$written[i])
    txt <- tryCatch(readLines(dest, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
    if (is.null(txt) || !all(validUTF8(txt))) next
    from_dir <- dirname(orig[i])
    new_dir <- dirname(plan$written[i])
    up <- if (new_dir == ".") "" else
      strrep("../", length(strsplit(new_dir, "/", fixed = TRUE)[[1]]))
    new_txt <- txt
    # ](link), ](link "title"), src="link", href="link"
    pat <- "(\\]\\(\\s*<?|(?:src|href)\\s*=\\s*[\"'])([^)\"'<>\\s#?]+)"
    m <- gregexpr(pat, txt, perl = TRUE)
    found <- regmatches(txt, m)
    for (k in which(lengths(found) > 0)) {
      for (s in unique(found[[k]])) {
        lead <- sub(pat, "\\1", s, perl = TRUE)
        link <- sub(pat, "\\2", s, perl = TRUE)
        if (grepl("^[A-Za-z][A-Za-z0-9+.-]*:|^/", link)) next   # web, mail, absolute
        plain <- tryCatch(utils::URLdecode(link), error = function(e) link)
        rel <- .FAIR_repo_join_path(from_dir, plain)
        to <- if (is.na(rel)) NA_character_ else place_of[tolower(rel)]
        if (is.na(to)) next
        new_link <- paste0(up, to)
        if (grepl("%20", link, fixed = TRUE)) new_link <- gsub(" ", "%20", new_link, fixed = TRUE)
        if (new_link == link) next
        new_txt[k] <- gsub(s, paste0(lead, new_link), new_txt[k], fixed = TRUE)
      }
    }
    if (identical(new_txt, txt)) next
    ok <- tryCatch({
      con <- file(dest, open = "wb", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      writeLines(new_txt, con, useBytes = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok) changed <- c(changed, plan$written[i])
  }
  changed
}

# `link` read from the folder `dir` ("." for the top), as a path from the top
# folder: "./a/../b.png" from "docs" is "docs/b.png". NA when it leaves the
# top folder.
.FAIR_repo_join_path <- function(dir, link) {
  parts <- c(if (dir != ".") strsplit(dir, "/", fixed = TRUE)[[1]],
             strsplit(link, "/", fixed = TRUE)[[1]])
  out <- character(0)
  for (p in parts) {
    if (p %in% c("", ".")) next
    if (p == "..") {
      if (!length(out)) return(NA_character_)
      out <- out[-length(out)]
    } else out <- c(out, p)
  }
  if (!length(out)) NA_character_ else paste(out, collapse = "/")
}

# Copy a file, dropping a leading UTF-8 byte order mark (EF BB BF) if
# present: with one, the first column name of a CSV is read as "﻿id".
# Byte-level, so it works for any encoding. Returns TRUE on success.
.FAIR_repo_copy_no_bom <- function(src, dest) {
  tryCatch({
    n <- file.info(src)$size
    if (is.na(n) || n == 0) return(isTRUE(file.copy(src, dest, overwrite = TRUE)))
    con <- file(src, "rb"); on.exit(close(con), add = TRUE)
    bytes <- readBin(con, "raw", n)
    if (length(bytes) >= 3 && bytes[1] == as.raw(0xEF) &&
        bytes[2] == as.raw(0xBB) && bytes[3] == as.raw(0xBF))
      bytes <- bytes[-(1:3)]
    out <- file(dest, "wb"); on.exit(close(out), add = TRUE)
    writeBin(bytes, out)
    TRUE
  }, error = function(e) FALSE)
}

# data_check's name for the table on sheet `sheet` of the file it calls `file`:
# "file [sheet]" when it read several tables from the file, the file's own name
# when this was the only one, NA when it read no table from this sheet.
.FAIR_repo_sheet_key <- function(file, sheet, columns_df) {
  if (!is.data.frame(columns_df) || !"sheet" %in% names(columns_df)) return(NA_character_)
  key <- paste0(file, " [", sheet, "]")
  if (key %in% columns_df$source_file) return(key)
  if (any(columns_df$source_file %in% file & columns_df$sheet %in% sheet)) return(file)
  NA_character_
}

# The CSV of one sheet: the file's CSV name with the sheet as its Psych-DS
# "description" ("data/study-harvest_data.csv" and sheet "data sheet" give
# "data/study-harvest_description-datasheet_data.csv"). A sheet of notes
# or a codebook rather than a table (see .sheet_is_notes()) goes to
# documentation/: in data/ its cells were listed as variables
# without names ("...2", "...3").
.FAIR_repo_sheet_target <- function(target, sheet, k) {
  value <- gsub("[^A-Za-z0-9]+", "", sheet)
  if (!nzchar(value)) value <- paste0("sheet", k)
  out <- sub("_data\\.csv$", paste0("_description-", value, "_data.csv"), target)
  if (.sheet_is_notes(sheet))
    out <- sub("^((study-[^/]+/)?)data/", "\\1documentation/", out)
  out
}

# Write a real CSV file from a data file that is not one (xlsx, sav, dta,
# ...), read in full with data_read_head(), the reader data_check uses, so the
# columns match the ones data_check found. UTF-8, no byte order mark, no row
# names. Returns FALSE when the file cannot be read as a table.
.FAIR_repo_write_data_csv <- function(src, dest, sheet = NULL) {
  tryCatch({
    df <- data_read_head(src, n_rows = Inf, sheet = sheet)
    if (is.null(df) || !is.data.frame(df) || !ncol(df) || !nrow(df)) return(FALSE)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    con <- file(dest, open = "wb", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    utils::write.csv(df, con, row.names = FALSE, na = "")
    TRUE
  }, error = function(e) FALSE)
}

# Tell the user what was not copied, renamed, or changed in a script.
.FAIR_repo_messages <- function(plan, copied) {
  n_written <- sum(!is.na(plan$written))
  msg <- sprintf("Wrote %d file%s to the new layout.", n_written,
                 plural(n_written))

  if (length(copied$skipped)) {
    k <- length(copied$skipped)
    types <- plan$data_type[copied$skipped]
    types[is.na(types)] <- "unknown"
    by_type <- sort(table(types), decreasing = TRUE)
    msg <- c(msg, sprintf(
      "%d file%s %s not copied, because %s could not be read (by type: %s).",
      k, plural(k), plural(k, "was", "were"), plural(k, "it", "they"),
      paste(sprintf("%d %s", by_type, names(by_type)), collapse = ", ")))
  }
  if (length(copied$failed)) {
    k <- length(copied$failed)
    msg <- c(msg, sprintf("%d file%s could not be written: %s.", k, plural(k),
                          paste(plan$file_name[copied$failed], collapse = ", ")))
  }
  if (length(copied$not_tables)) {
    k <- length(copied$not_tables)
    msg <- c(msg, sprintf(paste0(
      "%d data file%s %s no table (for example an R file holding a fitted ",
      "model), so %s kept as %s, without a CSV copy."),
      k, plural(k), plural(k, "is", "are"), plural(k, "it was", "they were"),
      plural(k, "it is", "they are")))
  }
  n_same <- sum(!is.na(plan$same_as %||% rep(NA, nrow(plan))))
  if (n_same)
    msg <- c(msg, sprintf(paste0(
      "%d file%s %s left out, because an identical file is in the copy ",
      "(for example a file both in a .zip and next to it); CHANGES.md lists them."),
      n_same, plural(n_same), plural(n_same, "was", "were")))
  if (nrow(copied$renamed)) {
    msg <- c(msg, paste0(
      "Two different files had the same place in the new layout, so the ",
      "second was given a numbered name: ",
      paste(sprintf("%s -> %s", copied$renamed$file_name,
                    copied$renamed$written), collapse = "; "),
      ". A script that reads one of them may need its path checked."))
  }
  sc <- copied$scripts
  if (nrow(sc) && any(sc$paths_changed > 0)) {
    k <- sum(sc$paths_changed > 0)
    msg <- c(msg, sprintf(
      "File paths were changed in %d script%s, so %s the files in the new layout.",
      k, plural(k), plural(k, "it finds", "they find")))
  }
  if (nrow(sc) && any(sc$setwd_removed > 0)) {
    hit <- sc[sc$setwd_removed > 0, , drop = FALSE]
    msg <- c(msg, sprintf(paste0(
      "setwd() was removed from %d script%s (%s), because it set a folder ",
      "written out in the script or the script's own folder, and the file ",
      "paths now start at the top folder; each line is kept as a comment. Run ",
      "the scripts from the top folder of the copy."),
      nrow(hit), plural(nrow(hit)), paste(hit$target_path, collapse = ", ")))
  }
  libs <- sc$libraries_added %||% rep("", nrow(sc))
  if (nrow(sc) && any(nzchar(libs))) {
    k <- sum(nzchar(libs))
    msg <- c(msg, sprintf(paste0(
      "library() calls were added at the top of %d script%s, because running ",
      "%s showed %s a function of a package %s not load."),
      k, plural(k), plural(k, "it", "them"), plural(k, "it uses", "they use"),
      plural(k, "it does", "they do")))
  }
  n_rec <- sum(!is.na(plan$written) & !is.na(plan$recovered_row %||% NA))
  if (n_rec)
    msg <- c(msg, sprintf(paste0(
      "%d script%s %s recovered from output files that hold the code that ",
      "made them; CHANGES.md lists them."),
      n_rec, plural(n_rec), plural(n_rec, "was", "were")))
  message(paste(msg, collapse = "\n"))
}
