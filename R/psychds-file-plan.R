#' Build a file-to-target-path plan for a Psych-DS-style layout
#'
#' @description
#' Maps every file `data_check` found to the location it would occupy in a
#' Psych-DS-shaped layout: data files under `data/`, code under `analysis/`,
#' materials under `materials/`, documentation under `documentation/` (or
#' `documentation/codebooks/` for codebooks), the root readme/licence at the
#' collection root, and anything unclassified under `unknown/`. This is pure
#' file/folder organisation -- it does not check Psych-DS compliance (that is
#' `psychds_check()`) and it does not write anything to disk.
#'
#' @details
#' `data_check` assigns every file except the collection-level root README/
#' `ro-crate-metadata.json` to exactly one study group -- deterministically
#' where path/repository/code-reference evidence allows, and via an LLM only
#' for the residual cases (see [data_group_llm()]). A multi-study repository
#' is modelled with a `study-<group>/` directory per study (each a complete
#' Psych-DS dataset); only the root readme and licence sit at the collection
#' root beside them.
#'
#' A tabular data file whose source is not already `.csv` (`.xlsx`/`.sav`/
#' `.dta`/...) is planned to be converted to `.csv` at its target path, with
#' the untouched original kept alongside it (`convert`/`original_target`
#' columns) -- no conversion is actually performed here, only planned. A raw
#' (non-tabular) data file (`.npy`/`.h5`/...) keeps its own real name instead
#' of a `_data.csv` target, since it cannot be read as a table.
#'
#' This is a pure function of `structure_df`: it takes no `paper` and makes no
#' `data_check`/`module_run()` call of its own, so callers resolve their own
#' `structure_df` (chained output, a saved build, or a fresh `data_check` run)
#' and pass it in directly -- the same frame-dependent `get_prev_outputs()`
#' chain lookup every module relies on only works correctly one call deep from
#' `module_run()`'s own `eval()`, not from a second, ordinary function call
#' nested inside it.
#'
#' @param structure_df the `structure` table from `data_check` (one row per
#'   repository file, with at least `file_name`/`file_path`/`data_type`; the
#'   `group`/`doc_role`/`referenced_by` columns are used when present).
#' @param group_no_evidence TRUE when no file path, repository split, or LLM
#'   answer named a study anywhere in this repository, so every file fell back
#'   to a single default study (`data_check`'s own `group_no_evidence` output).
#'   Carried through to the return value only; does not affect the plan.
#'
#' @returns a list with `table` (one row per file: `file_name`, `data_type`,
#'   `group`, `current_path`, `target_path`, `status` -- `"present"`/`"move"`/
#'   `"excluded"` --, `convert`, `original_target`, `referenced_by`) and
#'   `group_no_evidence` (passed through unchanged). `table` is an empty
#'   data.frame when `structure_df` is NULL or has no rows.
#' @export
#' @keywords internal
psychds_file_plan <- function(structure_df, group_no_evidence = FALSE) {

  group_no_evidence <- isTRUE(group_no_evidence)

  if (is.null(structure_df) || nrow(structure_df) == 0)
    return(list(table = data.frame(), group_no_evidence = group_no_evidence))

  n_files <- nrow(structure_df)

  # A missing file name falls back to the path's file name; a file with
  # neither is excluded below. A missing name became the target "unknown/NA",
  # and a structure without a file_name column stopped (U113).
  if (is.null(structure_df$file_name))
    structure_df$file_name <- rep(NA_character_, n_files)
  name_missing <- is.na(structure_df$file_name) | !nzchar(structure_df$file_name)
  path_given <- structure_df$file_path %||% rep(NA_character_, n_files)
  structure_df$file_name[name_missing] <- basename(path_given[name_missing])
  no_name <- is.na(structure_df$file_name)

  # Psych-DS's OWN validator rule for a datafile name is the regex
  # '([a-z]+-[a-zA-Z0-9]+)(_[a-z]+-[a-zA-Z0-9]+)*_data\.(csv|tsv)' (schema_model/
  # versions/*/rules/files/tabular_data/data.yaml, "Datafile", verified directly
  # against the psych-ds/psych-ds GitHub repo -- not assumed from the prose docs
  # alone): a KEY is lowercase-alpha-only, but a VALUE is "upper- and lowercase
  # alphanumeric" -- case is explicitly allowed in the value, so keep it. Only
  # strip what the value pattern actually disallows (anything that is not a
  # letter or digit); this used to also lowercase and was therefore stripping
  # more than the spec requires -- confirmed by reading the validator rule
  # directly, not by assumption, after the user questioned whether the
  # aggressive slugification was really a Psych-DS requirement.
  keyword_slug <- function(x) {
    gsub("[^a-zA-Z0-9]+", "", x)
  }

  # File-type -> Psych-DS subdirectory (data and readme/license handled
  # separately, by doc_role, below).
  #
  # "unknown" gets its OWN subdirectory rather than being folded into
  # documentation/: data_classify_files() could not place these files by
  # format, folder, or filename keyword at all (see .ext_registry,
  # R/data_check_helpers.R) -- silently filing them under documentation/ hid
  # that gap. A visible unknown/ folder is an actionable signal: a
  # researcher (or metacheck's own maintainer) can see exactly what wasn't
  # recognized and rename the file to include a data/code/materials/output/
  # documentation keyword, which data_classify_files()'s Tier 2 keyword rules
  # will then pick up correctly on a re-run.
  type_to_subdir <- c(
    code          = "analysis",
    materials     = "materials",
    output        = "outputs",
    documentation = "documentation",
    unknown       = "unknown"
  )

  # Every file except the collection-level root README/ro-crate-metadata.json
  # resolves to exactly one study -- there is no "shared" bucket to filter out;
  # those root files simply carry group = NA (see data_check.R). Every column
  # below is guarded the same way: a structure_df missing it entirely (not
  # just holding NAs -- e.g. a hand-built frame in a test, or a caller that
  # never ran the full data_check pipeline) must not make a later
  # data.frame(..., col = structure_df$col, ...) throw "differing number of
  # rows" from a NULL (length-0) column.
  data_type <- if ("data_type" %in% names(structure_df))
    structure_df$data_type else rep(NA_character_, n_files)
  groups <- if ("group" %in% names(structure_df))
    structure_df$group else rep(NA_character_, n_files)
  doc_role <- if ("doc_role" %in% names(structure_df))
    structure_df$doc_role else rep(NA_character_, n_files)
  study_groups <- unique(groups[!is.na(groups)])
  multi_study  <- length(study_groups) > 1

  # Map each file to its Psych-DS target path: data files -> data/<...>_data.csv;
  # readme/license -> root; everything else -> its type subdirectory. Study
  # prefix is added when groups exist.
  is_data <- !is.na(data_type) & data_type == "data"

  # Each file's path in the repository, without the archive it is in and
  # without a top folder that holds every file (a repository downloaded from
  # GitHub sits in "<name>-<commit>/"): with that folder, the repository's
  # own README counted as one in a subfolder.
  rel <- .plan_relative_paths(path_given)

  # A file keeps its folders below the folder its study's files of the same
  # kind share ("Code/utils/helpers.py" next to "Code/a.R" becomes
  # "analysis/utils/helpers.py"; "Data/scenario_lb/x.csv" next to
  # "Data/y.csv" becomes "data/scenario_lb/study-x_data.csv"). Put flat in
  # one folder, two files with one name collided, code that finds its own
  # modules by folder (Python's `from utils import helpers`) stopped working,
  # and data in folders that each hold one condition lost which condition
  # they were.
  # Which READMEs at the top of a study are its main README: one named just
  # "README" (any extension), or the only README at that top.
  is_readme <- !is.na(data_type) & data_type == "documentation" &
    doc_role %in% "readme"
  at_top <- is_readme & vapply(rel, function(p) is.na(.plan_readme_folder(p)),
                               logical(1))
  plain <- tolower(tools::file_path_sans_ext(basename(
    gsub("\\\\", "/", structure_df$file_name)))) %in% "readme"
  top_key <- ifelse(is.na(groups), "", groups)
  n_top <- stats::ave(as.integer(at_top), top_key, FUN = sum)
  top_readme_main <- at_top & (plain | n_top == 1)

  kind <- ifelse(is.na(data_type), "unknown", data_type)
  kind[kind == "documentation" & doc_role %in% c("readme", "license")] <- NA
  sub_dir <- rep("", n_files)
  dirs <- strsplit(ifelse(is.na(rel), "", dirname(rel)), "/", fixed = TRUE)
  dirs <- lapply(dirs, function(d) d[!d %in% c(".", "")])
  grp_key <- ifelse(is.na(groups), "", groups)
  for (key in unique(paste(grp_key, kind)[!is.na(kind)])) {
    rows <- which(paste(grp_key, kind) == key & !is.na(kind) & !is.na(rel))
    if (!length(rows)) next
    common <- Reduce(function(a, b) {
      k <- 0L
      while (k < min(length(a), length(b)) && a[k + 1L] == b[k + 1L]) k <- k + 1L
      a[seq_len(k)]
    }, dirs[rows])
    for (i in rows) {
      # (x[-seq_along(character(0))] is empty, not x)
      below <- if (length(common)) dirs[[i]][-seq_along(common)] else dirs[[i]]
      # A first folder that only names the kind of file ("data/", "Code/")
      # is what the copy's own folder already says: with some data files at
      # the top and others in "data/", "data/eeg.npy" became
      # "data/data/eeg.npy".
      if (length(below) && tolower(below[1]) %in% .plan_kind_folders)
        below <- below[-1]
      if (length(below)) sub_dir[i] <- paste0(paste(below, collapse = "/"), "/")
    }
  }

  # Only a repository with >=2 detected study groups uses the study-<group>/
  # layout; a single study (or unknown grouping) is a flat single dataset.
  #
  # Files that belong to a specific study go under study-<group>/ (a complete,
  # valid Psych-DS dataset). Only the root README/ro-crate-metadata.json (group
  # is NA by construction -- see data_check.R) get NO study prefix: they live at
  # the dataset root, beside the study-*/ folders. This follows BIDS
  # (collection-level content sits at the root, never in a pseudo-subject like
  # sub-shared/) and keeps every study-*/ a real dataset.
  target_of <- function(i) {
    dt   <- data_type[i]
    if (is.null(dt) || is.na(dt)) dt <- "unknown"
    role <- doc_role[i]
    name <- basename(gsub("\\\\", "/", structure_df$file_name[i]))
    grp  <- groups[i]
    prefix <- if (multi_study && !is.na(grp))
      paste0("study-", grp, "/") else ""

    if (dt == "data") {
      # a name that is already a valid Psych-DS data file name is kept:
      # study-a_data.csv became data/study-studyadata_data.csv, so data files
      # were never in place, even in a compliant dataset (U112)
      if (grepl("^([a-z]+-[a-zA-Z0-9]+_)+data\\.(csv|tsv)$", name))
        return(paste0(prefix, "data/", sub_dir[i], name))
      stem <- keyword_slug(tools::file_path_sans_ext(name))
      if (!nzchar(stem)) stem <- paste0("file", i)
      # Every data file gets a Psych-DS *_data.csv target: the fileRegex
      # requires AT LEAST ONE key-value pair before "_data.csv" (there is no
      # valid zero-pairs form), so some wrapper key is unavoidable. "study" is
      # used here -- a REAL, official Psych-DS keyword (schema_model/versions/
      # */meta/context.yaml's own controlled keyword list: study, site,
      # subject, session, task, condition, trial, stimulus, description).
      paste0(prefix, "data/", sub_dir[i], "study-", stem, "_data.csv")
    } else if (dt == "documentation" && !is.na(role) && role == "readme") {
      # The root readme/ro-crate-metadata.json never carries a study prefix
      # (grp is NA for these rows by construction); a PER-STUDY readme (rare,
      # but possible if a study's own folder has its own README) still gets one.
      # A README in a subfolder describes that folder, so it goes to
      # documentation/ named after the folder: every README went to the top,
      # where they replaced each other and lost what they described.
      folder <- .plan_readme_folder(rel[i])
      if (!is.na(folder)) return(paste0(prefix, "documentation/", folder, "_", name))
      # At the top, the README is a file named just "README", or the only
      # README there is; one about a single file ("README_for_snowmelt.txt"
      # next to others) keeps its name in documentation/: renamed README.txt,
      # README_2.txt, ..., they lost which file each described.
      if (!top_readme_main[i]) return(paste0(prefix, "documentation/", name))
      ext <- tools::file_ext(name)
      paste0(prefix, if (nzchar(ext)) paste0("README.", ext) else "README")
    } else if (dt == "documentation" && !is.na(role) && role == "license") {
      # A LICENSE is collection-level, same as the readme: one licence covers
      # the whole deposit, so it goes at the archive root with no study prefix.
      ext <- tools::file_ext(name)
      paste0(prefix, if (nzchar(ext)) paste0("LICENSE.", ext) else "LICENSE")
    } else {
      # Single-bracket lookup: an unmapped data_type returns NA rather than
      # throwing "subscript out of bounds" as `[[` would.
      sub <- unname(type_to_subdir[dt])
      if (is.na(sub)) sub <- "documentation"
      paste0(prefix, sub, "/", sub_dir[i], name)
    }
  }
  target_path  <- vapply(seq_len(n_files), target_of, character(1))
  target_path[no_name] <- NA_character_   # neither name nor path: excluded (U113)
  current_path <- gsub("\\\\", "/", structure_df$file_path %||% structure_df$file_name)
  current_path <- ifelse(is.na(current_path), structure_df$file_name, current_path)
  # target_of() always returns a real path now (consumed archive containers
  # never reach this table at all -- data_check.R drops those rows once their
  # contents are extracted, rather than keeping a placeholder row for them).
  # This guard is kept defensively in case a future data_type slips through
  # unmapped; it should never trigger in practice.

  # A TABULAR data file whose source is not already a CSV (xlsx/xls/ods/tsv/dat/
  # sav/dta/sas7bdat/jasp/omv/rds/rdata) is planned to be CONVERTED to CSV for
  # its _data.csv target (rather than having its bytes renamed, which would be
  # an invalid CSV), with its ORIGINAL kept alongside so a release built from
  # this plan retains the authored artifact (an .xlsx carries formatting/
  # sheets, a .sav/.dta carries value labels). `convert` marks those rows;
  # `original_target` is where the untouched original goes (same data/ dir,
  # original extension).
  #
  # A RAW (non-tabular) data file -- .npy/.h5/.pickle/.fif/... -- cannot be read
  # as a table, so it is neither converted nor renamed to .csv: it is planned
  # to be copied with its true extension to a raw_target and does NOT claim a
  # _data.csv path.
  src_ext        <- tolower(tools::file_ext(structure_df$file_name))
  # "Convertible" is asked of data_format(), the package's single source of
  # truth for what data_read_head() can parse -- the same reader any future
  # converter would use to do the conversion. A hardcoded list here would drift
  # from the reader (it did: .ods/.fods were readable but copied raw). A .csv
  # needs no conversion, so it is excluded even though it is tabular.
  needs_convert  <- is_data & src_ext != "csv" &
                    data_format(src_ext) == "tabular"
  is_raw_data    <- is_data & nzchar(src_ext) & src_ext != "csv" & !needs_convert

  # Psych-DS's Datafile naming rule (the fileRegex checked above target_of())
  # applies ONLY to the ".csv"/".tsv" files the "Datafile" validator rule looks
  # for -- it says nothing about any OTHER file sitting in data/. The untouched
  # original (kept purely so a release built from this plan retains what the
  # author actually deposited) and a raw/non-tabular data file (which never
  # claims a _data.csv path at all -- it isn't the thing that rule is checking
  # for) are both exactly that "other file" case, so neither needs the
  # study-<slug> keyword wrapper target_of() built for the _data.csv target:
  # each gets its OWN real basename, verified directly against the actual
  # Psych-DS validator rule (schema_model/versions/*/rules/files/tabular_data/
  # data.yaml) rather than assumed -- that rule's `extensions: [".csv", ".tsv"]`
  # scopes it to the datafile itself, and none of the other schema_model rules
  # constrain filenames elsewhere under data/.
  same_dir_real_name <- function(tp, i) {
    # Same directory as the (possibly study-prefixed) _data.csv target, but the
    # file's OWN real basename -- not derived from the slugified target_path.
    real_name <- basename(gsub("\\\\", "/", structure_df$file_name[i]))
    file.path(dirname(tp), real_name)
  }

  # Convertible: keep the _data.csv target, add original alongside (real name).
  original_target <- vapply(seq_len(n_files), function(i)
    if (isTRUE(needs_convert[i])) same_dir_real_name(target_path[i], i) else NA_character_,
    character(1))
  # Raw: replace the (wrong) _data.csv target with the file's own real name,
  # and do not treat it as a CSV to write.
  raw_target <- vapply(seq_len(n_files), function(i)
    if (isTRUE(is_raw_data[i])) same_dir_real_name(target_path[i], i) else NA_character_,
    character(1))
  target_path <- ifelse(is_raw_data, raw_target, target_path)
  # judged against the final target: computed before raw data got its own
  # name, data/eeg.npy was told "Move data/eeg.npy -> data/eeg.npy" (U112)
  is_excluded  <- is.na(target_path)
  misplaced    <- !is_excluded & current_path != target_path

  # referenced_by: which OTHER studies reuse this file (cross-study reuse via
  # a script's own read/write references -- see data_group_llm()). A plain
  # list column so a future converter can write a reference into each of
  # those studies' own metadata instead of copying the file.
  plan_table <- data.frame(
    file_name       = structure_df$file_name,
    data_type       = data_type,
    group           = groups,
    current_path    = current_path,
    target_path     = target_path,
    # `excluded` = a data_type target_of() could not map (should not occur in
    # practice; see the comment above is_excluded).
    status          = ifelse(is_excluded, "excluded",
                             ifelse(misplaced, "move", "present")),
    # convert = TRUE -> a future converter would write a real CSV at
    # target_path from the read data; original_target = where to also copy
    # the untouched original.
    convert         = needs_convert,
    original_target = original_target
  )
  plan_table$referenced_by <- if ("referenced_by" %in% names(structure_df))
    structure_df$referenced_by else vector("list", n_files)

  list(table = plan_table, group_no_evidence = group_no_evidence)
}

# The folder a README is in, as a name for a file ("Archive of OSF Storage"
# becomes "Archive-of-OSF-Storage"), or NA for a README at the top. The top
# of an archive ("data.zip/README", or unpacked "data.zip.contents/README")
# counts as the top.
.plan_readme_folder <- function(path) {
  if (length(path) != 1 || is.na(path) || !nzchar(path)) return(NA_character_)
  d <- dirname(.plan_strip_archive(path))
  if (d %in% c(".", "", "/")) return(NA_character_)
  slug <- gsub("(^-+|-+$)", "", gsub("[^A-Za-z0-9]+", "-", basename(d)))
  if (nzchar(slug)) slug else NA_character_
}

# Folder names that only say what kind of file is in them, as the copy's own
# data/, analysis/, materials/, documentation/ and outputs/ folders do.
.plan_kind_folders <- c(
  "data", "dataset", "datasets", "rawdata", "raw_data", "raw-data",
  "code", "codes", "script", "scripts", "analysis", "analyses", "src", "r",
  "python", "syntax", "materials", "material", "stimuli",
  "documentation", "docs", "doc", "output", "outputs", "results")

# Paths with "/", without the archive each is in, and without the top folders
# that hold every file ("repo-1a2b3c/Code/a.R" and "repo-1a2b3c/README.md"
# become "Code/a.R" and "README.md"). NA stays NA.
.plan_relative_paths <- function(paths) {
  p <- .plan_strip_archive(paths)
  ok <- !is.na(p) & nzchar(p)
  repeat {
    parts <- strsplit(p[ok], "/", fixed = TRUE)
    if (!length(parts) || any(lengths(parts) < 2)) break
    first <- unique(vapply(parts, `[`, "", 1))
    if (length(first) != 1) break
    p[ok] <- substring(p[ok], nchar(first) + 2L)
  }
  p
}

# A path with "/" and without the archive it is in ("data.zip/x.csv" or
# "data.zip.contents/x.csv" becomes "x.csv").
.plan_strip_archive <- function(path) {
  p <- gsub("\\\\", "/", path)
  sub("^([^/]*/)*[^/]*\\.(zip|tar|tgz|gz|7z|rar)(\\.contents)?/", "", p,
      ignore.case = TRUE)
}
