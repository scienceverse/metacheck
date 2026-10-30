#' Convert a paper's repository into a clean, FAIR copy
#'
#' Writes a reorganised copy of the repository (or repositories) a paper links
#' to: every file is placed where the Psych-DS layout puts it (data under
#' `data/`, code under `analysis/`, materials under `materials/`,
#' documentation under `documentation/`, one `study-<group>/` folder per study
#' when there are several; see [psychds_file_plan()]). A data file that is
#' not a CSV file (`.xlsx`, `.sav`, `.dta`, ...) is also written as a real CSV
#' file, with the original kept next to it. A data CSV file is copied without
#' a byte order mark.
#'
#' R and Python scripts (`.R`, `.Rmd`, `.qmd`, `.py`, `.ipynb`) are changed so
#' they still find their files in the new layout: each file path that
#' [repro_rewrite_paths()] can match to one file of the repository is replaced
#' by that file's new path, and each `setwd()`/`os.chdir()` line is replaced
#' by a statement that does nothing, with the original line kept in a
#' comment. Every change is reported, and listed in `CHANGES.md`. Scripts in
#' other languages (Stata, SPSS, MATLAB, ...) are copied unchanged, so their
#' file paths may no longer work.
#'
#' Metadata is written next to the files:
#' * `dataset_description.json` in each study folder (Psych-DS), describing
#'   every data column, with its label from the codebook when
#'   `codebook_check` found one;
#' * `ro-crate-metadata.json` at the top (RO-Crate 1.3): every file, the
#'   paper, the authors with their CRediT roles when the paper states them,
#'   the contact, funding (`funding_check`, or the paper's funding statement),
#'   the licence, the repositories the files come from, the languages and
#'   packages the code uses, the software proprietary formats need, which
#'   files each script reads and writes, the missing-value codes, how each
#'   study was run (DDI-Lifecycle terms; from the preregistration when the
#'   result passed holds `prereg_check` and the paper links exactly one), and
#'   the repository files that are not in the copy. "unknown" is written only
#'   where the authors are asked for something: the licence, an author's
#'   CRediT roles, and when and how the data were collected and processed;
#' * `README.md`, a readable version of the RO-Crate, in the top folder and in
#'   each study folder, unless the repository has its own README there;
#' * `requirements.txt`, the packages the code loads, unless the repository
#'   has its own `requirements.txt`, `renv.lock` or `DESCRIPTION`;
#' * `logs/`, the list of repository files and the results of the checks,
#'   without the places on this computer they were downloaded to;
#' * `CHANGES.md`, and a `CHANGES.md` in each study folder that has none.
#'
#' @param paper a paper object (see [read()]), **or** a captured result of
#'   `report_module_run(paper, ...)` (or a module output chain). The results
#'   of `data_check`, `codebook_check`, `code_check`, `reproducibility_check`
#'   and `funding_check` in it are reused; only the ones it lacks are run,
#'   and `reproducibility_check` is then run with `execute = FALSE`, so no
#'   code of the paper is run.
#' @param output_dir the folder to write into; created if needed. Defaults to
#'   `"FAIR_repo/<paper_id>"` under the working directory.
#' @param overwrite whether to replace an existing `output_dir`. When `FALSE`
#'   (the default) and `output_dir` exists, nothing is written and the
#'   returned list has `existed = TRUE`.
#' @param refresh_osf whether to fetch a new OSF file listing even if one was
#'   already retrieved this session. The default (`FALSE`) reuses it, so a
#'   conversion right after the checks does not query OSF again.
#' @param local_path,local_only passed to the checks that are run (see
#'   `data_check()`)
#' @param model,params passed to the checks that are run; used only when
#'   `llm_use(TRUE)`
#' @param download,skip_types,peek_zips,max_file_size,max_download_size,cache
#'   passed to `data_check` when it is run. `download` defaults to `"all"`,
#'   because the copy should hold every file, not only the data.
#'
#' @returns (invisibly) a list with `output_dir`, `n_files_copied`,
#'   `n_studies`, `plan` (the file plan, with a `written` column: the path
#'   each file was written to, NA when it was not), `scripts` (one row per
#'   changed script: `target_path`, `paths_changed`, `setwd_removed`,
#'   `setwd_paths`), `skipped` (files not on disk, so not copied),
#'   `copy_failed` and `renamed` (a file whose target another file already
#'   took), `empty_plan` (TRUE when the repository had no files), and the
#'   paths of the metadata written: `descriptions`, `rocrate`,
#'   `requirements`, `logs` and `readme_changes`. When an existing
#'   `output_dir` was kept, the list has only `output_dir`, the counts, and
#'   `existed = TRUE`.
#' @export
#' @examples
#' \dontrun{
#' # Reuse the checks you ran, so nothing is downloaded again:
#' res <- report_module_run(paper, c("data_check", "codebook_check",
#'                                   "code_check", "reproducibility_check"))
#' FAIR_repo(res)
#'
#' # Or run the checks it needs:
#' FAIR_repo(paper)
#' }
FAIR_repo <- function(paper, output_dir = NULL, overwrite = FALSE,
                      refresh_osf = FALSE,
                      local_path = NULL, local_only = FALSE,
                      model = llm_model(), params = list(),
                      download = "all", skip_types = NULL, peek_zips = TRUE,
                      max_file_size = 100, max_download_size = 500,
                      cache = FALSE) {
  # Reuse the session's OSF listing unless a fresh one is requested, so
  # building right after the checks does not query (and risk throttling) OSF.
  old_osf_cache <- getOption("metacheck.osf.cache", TRUE)
  options(metacheck.osf.cache = !isTRUE(refresh_osf))
  on.exit(options(metacheck.osf.cache = old_osf_cache), add = TRUE)

  run_args <- list(
    data_check = list(local_path = local_path, local_only = local_only,
                      model = model, params = params, download = download,
                      skip_types = skip_types, peek_zips = peek_zips,
                      max_file_size = max_file_size,
                      max_download_size = max_download_size, cache = cache),
    codebook_check = list(local_path = local_path, local_only = local_only,
                          model = model, params = params),
    code_check = list(local_path = local_path, local_only = local_only,
                      max_file_size = max_file_size,
                      max_download_size = max_download_size, cache = cache),
    reproducibility_check = list(local_path = local_path,
                                 local_only = local_only,
                                 model = model, params = params,
                                 execute = FALSE, cache = cache)
  )
  # An existing folder is left alone before any check is run, when its name
  # is known; the default name needs the paper id, so it is checked after.
  kept <- function(output_dir) {
    message("The folder already exists, so nothing was written: ", output_dir,
            ". Set overwrite = TRUE to replace it.")
    invisible(list(output_dir = output_dir, n_files_copied = 0L,
                   n_studies = 0L, existed = TRUE))
  }
  if (!is.null(output_dir) && dir.exists(output_dir) && !overwrite)
    return(kept(output_dir))

  resolved <- .FAIR_repo_resolve(paper, .FAIR_repo_needed, run_args)
  ops <- resolved$ops
  pid <- resolved$pid
  if (is.null(output_dir)) {
    output_dir <- file.path("FAIR_repo", pid)
    if (dir.exists(output_dir) && !overwrite) return(kept(output_dir))
  }

  dc <- ops[["data_check"]]
  structure_df <- dc$structure
  # The plan is made here from data_check's file table, not taken from a
  # psychds_check result: psychds_file_plan() gives one row per file, in the
  # same order, so row i of the plan is file i of structure_df.
  plan <- psychds_file_plan(structure_df, dc$group_no_evidence)$table

  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # With no repository files, the logs and the metadata are still written.
  empty_plan <- !nrow(plan)
  if (empty_plan) {
    message("No repository files were found to convert, so only the logs ",
            "and the metadata were written.",
            .FAIR_repo_gated_hint(ops))
    plan <- data.frame(file_name = character(0), data_type = character(0),
                       group = character(0), target_path = character(0),
                       convert = logical(0), original_target = character(0))
  }

  copied <- .FAIR_repo_copy(plan, structure_df, output_dir)
  plan$written <- copied$written
  meta <- .FAIR_repo_write_metadata(resolved$paper, pid, ops, plan,
                                    structure_df, copied, output_dir)
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
    empty_plan     = empty_plan
  ), meta))
}

# Write the metadata files (see R/fair-repo-metadata.R) and return their
# paths: `descriptions` (dataset_description.json per study), `rocrate`,
# `requirements`, `logs`, and `readme_changes` (the README and CHANGES files
# metacheck wrote).
.FAIR_repo_write_metadata <- function(paper, pid, ops, plan, structure_df,
                                      copied, output_dir) {
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
    data_rows <- which(startsWith(written, paste0(prefix, "data/")))
    desc <- .FAIR_repo_dataset_description(
      paper, .FAIR_repo_label(root$grp),
      .FAIR_repo_variable_measured(cols, labels_df),
      .FAIR_repo_distribution(data_rows, plan, structure_df),
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
  logs <- .FAIR_repo_write_logs(ops, structure_df, pid, output_dir)
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
    info, output_dir, plan, structure_df, labels_df,
    extra_files = rel(c(descriptions, fulltext, names(logs), requirements,
                        readme_changes)))

  list(descriptions = descriptions, rocrate = rocrate,
       # fulltext = fulltext %||% character(0),  # full text
       requirements = requirements %||% character(0),
       logs = names(logs), readme_changes = readme_changes)
}

# The checks FAIR_repo() uses, in the order they run. funding_check only
# reads the paper's text.
.FAIR_repo_needed <- c("data_check", "codebook_check", "code_check",
                       "reproducibility_check", "funding_check")

# Resolve the module outputs FAIR_repo() needs, reusing anything already
# computed and running only what is missing.
#
# `paper` may be a paper object OR a captured result: the named list
# report_module_run() returns (with the paper as its "paper" attribute), or a
# single metacheck_module_output with the earlier modules in $prev_outputs.
# When data_check is reused, the missing modules are run on a chain built
# from the reused results, so they use its downloaded files instead of
# downloading again. Without a reused data_check, every needed module is run
# from the paper.
#
# Returns list(ops = <named module outputs>, paper = <paper>, pid = <id>).
.FAIR_repo_resolve <- function(paper, needed, args = list()) {
  if (.is_paper_list(paper))
    stop("FAIR_repo() converts one paper at a time; pass a single paper.",
         call. = FALSE)

  chain <- paper
  if (inherits(chain, "metacheck_module_output")) {
    reuse <- chain$prev_outputs %||% list()
    this <- chain
    this$prev_outputs <- NULL
    reuse[[chain$module %||% "unknown"]] <- this
  } else if (is.list(chain) && !.is_paper(chain) &&
             any(names(chain) %in% needed)) {
    reuse <- chain
  } else {
    reuse <- list()
  }

  # The paper: given directly, else the "paper" attribute report_module_run()
  # attaches to its result, else a module output that still carries $paper.
  real_paper <- if (.is_paper(paper)) paper else NULL
  if (is.null(real_paper) && .is_paper(attr(chain, "paper")))
    real_paper <- attr(chain, "paper")
  if (is.null(real_paper)) {
    for (mo in reuse) {
      if (.is_paper(mo$paper)) { real_paper <- mo$paper; break }
    }
  }

  missing <- setdiff(needed, names(reuse))
  ops <- reuse
  if (length(missing)) {
    if (is.null(real_paper))
      stop("Cannot run ", paste(missing, collapse = ", "),
           " because the result passed has no paper. Pass the paper object ",
           "itself.", call. = FALSE)
    if (!"data_check" %in% names(reuse)) {
      if (length(reuse))
        warning("FAIR_repo() ran every check again, because the result passed ",
                "has no data_check result to build on.", call. = FALSE)
      ops <- report_module_run(real_paper, needed, args = args)
    } else {
      ch <- .FAIR_repo_chain(reuse, real_paper)
      ops <- report_module_run(ch, needed[needed %in% missing], args = args)
    }
  }

  # A failed check is reported with its own error, rather than letting the
  # conversion fail later with a less clear message. A failed module carries
  # traffic_light "fail" and its error text in $report (see
  # report_module_run()).
  for (m in needed) {
    mo <- ops[[m]]
    if (!is.null(mo) && identical(mo$traffic_light, "fail"))
      stop("The '", m, "' check failed, so the repository cannot be ",
           "converted:\n  ",
           paste(mo$report %||% "unknown error", collapse = "\n  "),
           call. = FALSE)
  }

  # paper_id: from the paper, else from any module table.
  pid <- if (!is.null(real_paper)) paper_id(real_paper) else character(0)
  if (!length(pid) || all(is.na(pid))) {
    pid <- character(0)
    for (mo in ops) {
      for (tbl in list(mo$table, mo$summary_table, mo$structure)) {
        if (is.data.frame(tbl) && "paper_id" %in% names(tbl)) {
          got <- unique(tbl$paper_id[!is.na(tbl$paper_id)])
          if (length(got)) { pid <- got; break }
        }
      }
      if (length(pid)) break
    }
  }
  pid <- if (!length(pid)) "dataset" else as.character(pid[[1]])
  if (is.null(real_paper)) real_paper <- paper(id = pid)

  list(ops = ops, paper = real_paper, pid = pid)
}

# Build a module-output chain from reused results, the same shape
# report_module_run() passes from one module to the next (see
# .load_module_tables()), so a module run on it finds the reused results
# with get_prev_outputs().
.FAIR_repo_chain <- function(reuse, paper) {
  reuse <- lapply(names(reuse), function(m) {
    mo <- reuse[[m]]
    mo$module <- mo$module %||% m
    mo$paper <- NULL
    mo$prev_outputs <- NULL
    class(mo) <- "metacheck_module_output"
    mo
  }) |> stats::setNames(names(reuse))
  last <- reuse[[length(reuse)]]
  last$prev_outputs <- reuse
  last$paper <- paper
  last$summary_table <- last$summary_table %||%
    data.frame(paper_id = paper_id(paper))
  last
}

# Explain an empty conversion when a linked repository was found but could
# not be listed (a GitHub repository over the size limit, a private OSF
# component): which repository, why, and how to include it. Returns "" when
# no repository was skipped.
.FAIR_repo_gated_hint <- function(ops) {
  g <- ops[["data_check"]]$gated_repos %||% ops[["repo_check"]]$gated_repos
  if (is.null(g) || !nrow(g)) return("")
  # One paper often links the same repository by several deep URLs
  # (.../tree/master/x, .../tree/master/y); each is reported once.
  g$root <- sub("/(tree|blob)/.*$", "", g$repo_url)
  g <- g[!duplicated(paste(g$root, g$repo_error)), , drop = FALSE]
  lines <- sprintf("  - %s: %s", g$root,
                   ifelse(is.na(g$repo_error), "could not be listed", g$repo_error))
  paste0(
    "\n\nA linked repository was found but not downloaded:\n",
    paste(lines, collapse = "\n"),
    "\n\nTo include it, raise the size limits (`max_download_size`, ",
    "`max_file_size`) and run again, or download the repository yourself and ",
    "pass its folder as `local_path`.")
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
# Returns list(written = the path written per plan row, or NA; scripts =
# changes per script; skipped / failed = plan rows not on disk / not
# copied; renamed = data frame of from/to paths).
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
  hashes  <- list()   # target path -> md5 of the file written there

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
    if (is.na(target) || !nzchar(target)) next
    src <- src_all[i]
    if (is.na(src) || !nzchar(src) || !file.exists(src)) {
      skipped <- c(skipped, i)
      next
    }
    to <- place(target, src)
    if (is.null(to)) next   # an identical copy is already written
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
    ok <- if (convert[i]) {
      .FAIR_repo_write_data_csv(src, dest)
    } else if (!is.null(script)) {
      scripts[[to]] <- script
      TRUE
    } else if (is_data_csv) {
      .FAIR_repo_copy_no_bom(src, dest)
    } else {
      isTRUE(file.copy(src, dest, overwrite = TRUE))
    }
    if (ok) written[i] <- to else failed <- c(failed, i)

    # A converted file keeps its original next to the CSV.
    orig <- plan$original_target[i]
    if (convert[i] && !is.na(orig) && nzchar(orig)) {
      odest <- file.path(output_dir, orig)
      dir.create(dirname(odest), recursive = TRUE, showWarnings = FALSE)
      if (!isTRUE(file.copy(src, odest, overwrite = TRUE)))
        failed <- c(failed, i)
    }
  }

  scripts <- if (length(scripts)) dplyr::bind_rows(scripts) else
    data.frame(target_path = character(0), paths_changed = integer(0),
               setwd_removed = integer(0), setwd_paths = character(0))
  list(written = written, scripts = scripts, skipped = skipped,
       failed = unique(failed), renamed = renamed)
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
    msg <- c(msg, sprintf(paste0(
      "%d file%s %s not copied, because %s not downloaded (by type: %s). ",
      "Run with download = \"all\" and without skip_types to include more; ",
      "data_check's manifest gives the reason per file."),
      k, plural(k), plural(k, "was", "were"), plural(k, "it was", "they were"),
      paste(sprintf("%d %s", by_type, names(by_type)), collapse = ", ")))
  }
  if (length(copied$failed)) {
    k <- length(copied$failed)
    msg <- c(msg, sprintf("%d file%s could not be written: %s.", k, plural(k),
                          paste(plan$file_name[copied$failed], collapse = ", ")))
  }
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
