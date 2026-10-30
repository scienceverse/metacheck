#' Convert a research folder into a clean, FAIR copy
#'
#' Writes a reorganised copy of a folder of research files (data, code,
#' materials, documentation) on your own computer. Nothing is downloaded and
#' no link is followed: only the folder you name is read, and it is not
#' changed. The checks run the way [report_repository()] runs them:
#' `repo_check` takes an inventory of the folder, and `code_check`,
#' `data_check`, `codebook_check` and `reproducibility_check` (with
#' `execute = FALSE`, so no code is run) build on it. Whether a language model
#' is used is decided by [llm_use()].
#'
#' Every file is placed where the Psych-DS layout puts it (data under `data/`,
#' code under `analysis/`, materials under `materials/`, documentation under
#' `documentation/`, one `study-<group>/` folder per study when there are
#' several; see [psychds_file_plan()]). A data file that is not a CSV file
#' (`.xlsx`, `.sav`, `.dta`, ...) is also written as a real CSV file, with the
#' original kept next to it. A data CSV file is copied without a byte order
#' mark.
#'
#' R and Python scripts (`.R`, `.Rmd`, `.qmd`, `.py`, `.ipynb`) are changed so
#' they still find their files in the new layout: each file path that
#' [repro_rewrite_paths()] can match to one file of the folder is replaced by
#' that file's new path, and each `setwd()`/`os.chdir()` line is replaced by a
#' statement that does nothing, with the original line kept in a comment.
#' Every change is reported, and listed in `CHANGES.md`. Scripts in other
#' languages (Stata, SPSS, MATLAB, ...) are copied unchanged, so their file
#' paths may no longer work.
#'
#' Metadata is written next to the files:
#' * `dataset_description.json` in each study folder (Psych-DS), describing
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
#'   each study folder, unless the folder has its own README there;
#' * `requirements.txt`, the packages the code loads, unless the folder has
#'   its own `requirements.txt`, `renv.lock` or `DESCRIPTION`;
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
#'
#' @returns (invisibly) a list with `output_dir`, `n_files_copied`,
#'   `n_studies`, `plan` (the file plan, with a `written` column: the path
#'   each file was written to, NA when it was not), `scripts` (one row per
#'   changed script: `target_path`, `paths_changed`, `setwd_removed`,
#'   `setwd_paths`), `skipped` (files that could not be read), `copy_failed`
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
                      overwrite = FALSE) {
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

  ops <- .FAIR_repo_run_checks(full_path, paper)
  # Without a paper, the copy is named after the folder.
  if (is.null(paper)) {
    paper <- test_paper()
    paper$info$title <- basename(full_path)
    paper$author <- NULL
  }
  pid <- basename(full_path)

  dc <- ops[["data_check"]]
  structure_df <- dc$structure
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

  plan <- .FAIR_repo_identical(plan, structure_df)
  copied <- .FAIR_repo_copy(plan, structure_df, output_dir)
  plan$written <- copied$written
  plan$same_as <- copied$same_as
  # what was actually written: a file that was no table is not converted, and
  # an original may have a numbered name
  plan$convert <- copied$converted
  plan$original_target <- copied$original_written
  meta <- .FAIR_repo_write_metadata(paper, pid, ops, plan, structure_df,
                                    copied, output_dir, folder = full_path)
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
.FAIR_repo_run_checks <- function(path, paper = NULL) {
  modules <- c("repo_check", "code_check", "data_check", "codebook_check",
               "reproducibility_check", if (!is.null(paper)) "funding_check")
  args <- list(
    repo_check = list(local_path = path, local_only = TRUE),
    code_check = list(local_only = TRUE),
    data_check = list(local_only = TRUE),
    codebook_check = list(local_only = TRUE),
    reproducibility_check = list(local_only = TRUE, execute = FALSE))
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

  # dataset_description.json for each study holding files; a repository's
  # own dataset_description.json is kept.
  descriptions <- character(0)
  for (root in roots) {
    prefix <- if (nzchar(root$dir)) paste0(root$dir, "/") else ""
    if (!any(nzchar(written) & startsWith(written, prefix))) next
    dest <- file.path(output_dir, root$dir, "dataset_description.json")
    if (file.exists(dest)) next
    cols <- if (is.null(root$grp) || !is.data.frame(columns_df)) columns_df else
      columns_df[columns_df$group %in% root$grp, , drop = FALSE]
    desc <- .FAIR_repo_dataset_description(
      paper, .FAIR_repo_label(root$grp),
      .FAIR_repo_variable_measured(cols, labels_df),
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
  requirements <- .FAIR_repo_write_requirements(ops, plan, output_dir)
  # what the README and the RO-Crate are both written from
  info <- .FAIR_repo_collect(paper, pid, ops, plan, structure_df, output_dir)
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
.FAIR_repo_copy <- function(plan, structure_df, output_dir) {
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
  hashes  <- list()   # target path -> md5 of the file written there
  same_as <- plan$same_as %||% rep(NA_character_, n)

  # The path to write to: the planned one, unless another file took it. NULL
  # when an identical file is already there.
  place <- function(target, src) {
    h <- unname(tools::md5sum(src))
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
                              structure_df) else NULL
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
               setwd_removed = integer(0), setwd_paths = character(0))
  list(written = written, scripts = scripts, skipped = skipped,
       failed = unique(failed), renamed = renamed, same_as = same_as,
       converted = converted, original_written = original_written,
       not_tables = which(convert & !converted & !is.na(written)))
}

# Write an R or Python script with its file paths changed to the new layout
# and its setwd()/os.chdir() lines replaced. The paths are found in the code
# (for an .Rmd/.qmd its R chunks, for a notebook its code cells) and changed
# in the whole file, each only where it stands in quotes. Returns NULL for a
# file that is not an R or Python script (the caller copies it unchanged),
# else a one-row data frame of what was changed.
.FAIR_repo_write_script <- function(src, file_name, dest, target, plan,
                                    structure_df) {
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
  # A notebook's lines are JSON strings, which the setwd() pattern does not
  # match, so a notebook keeps its setwd()/os.chdir() calls.
  sw <- .repro_neutralise_setwd(txt, lang, by = "metacheck FAIR_repo")
  txt <- sw$txt

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
  data.frame(target_path = target, paths_changed = sum(changed),
             setwd_removed = sw$n,
             setwd_paths = paste(sw$paths, collapse = ", "))
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

# Write a real CSV file from a data file that is not one (xlsx, sav, dta,
# ...), read in full with data_read_head(), the reader data_check uses, so the
# columns match the ones data_check found. UTF-8, no byte order mark, no row
# names. Returns FALSE when the file cannot be read as a table.
.FAIR_repo_write_data_csv <- function(src, dest) {
  tryCatch({
    df <- data_read_head(src, n_rows = Inf)
    if (is.null(df) || !is.data.frame(df) || !ncol(df)) return(FALSE)
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
      "setwd() was removed from %d script%s (%s), because the folder it set ",
      "does not exist in the new layout; each line is kept as a comment. Run ",
      "the scripts from the top folder of the copy."),
      nrow(hit), plural(nrow(hit)), paste(hit$target_path, collapse = ", ")))
  }
  message(paste(msg, collapse = "\n"))
}
