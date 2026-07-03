# Helpers for the data_check module.
#
# Ported from the `datacheck` pipeline (0_index.R / helper.R), rewritten to
# build on metacheck's existing file-handling (`file_category()`,
# `file_types`) rather than datacheck's standalone rule tables. The LLM path
# from datacheck is deliberately NOT ported here: these helpers are rules-only
# and run with `llm_use(FALSE)`. The module upgrades to an LLM classifier only
# when `llm_use(TRUE)` (see data_check.R).

# ── File-type crosswalk ──────────────────────────────────────────────────────

# datacheck's semantic file types. Superset of metacheck's coarse
# `file_types$type` vocabulary; `data_check` reports at this granularity.
.data_check_types <- c(
  "data", "codebook", "code", "software", "output",
  "supplemental", "readme", "asset", "other"
)

# Map metacheck's coarse `file_types$type` values onto data_check's semantic
# types. Used as the fallback layer after the name-based rules in
# `file_category()` (readme / codebook) have had first refusal.
.file_type_crosswalk <- c(
  data    = "data",
  code    = "code",
  stats   = "code",       # SPSS/SAS/Stata syntax → code
  exec    = "software",   # exe/dll/app/... → software
  config  = "software",   # yaml/ini/toml/... → software
  audio   = "asset",
  video   = "asset",
  image   = "asset",
  `3D`    = "asset",
  font    = "asset",
  book    = "supplemental",
  slide   = "supplemental",
  text    = "supplemental",
  web     = "supplemental",
  archive = "other"
)

# Extensions whose data_check type is fixed by format, applied last to correct
# any coarser guess (ported from 0_index.R FIXED_EXT_TYPE). Lowercase, no dot.
.fixed_ext_type <- c(
  r = "code", rmd = "code", qmd = "code", ipynb = "code",
  do = "code", sps = "code", sas = "code",
  exe = "software", dmg = "software", app = "software", jar = "software",
  msi = "software", deb = "software", rpm = "software",
  sh = "software", bash = "software", zsh = "software",
  bat = "software", cmd = "software", ps1 = "software",
  dll = "software", so = "software", dylib = "software",
  lua = "software", psyexp = "software", osexp = "software",
  spv = "output", fig = "output"
)

#' Classify repository files into data_check semantic types
#'
#' Rules-only classifier used by the `data_check` module when the LLM is off.
#' Layers metacheck's `file_category()` (name-based readme/codebook/data/code
#' rules) over an extension crosswalk built on `metacheck::file_types`, then
#' applies format-locked extension overrides.
#'
#' @param file_name a character vector of file names (basenames)
#'
#' @returns a character vector of data_check types (see [.data_check_types]);
#'   `"other"` when no rule fires.
#' @export
#' @keywords internal
#'
#' @examples
#' data_classify_files(c("data.csv", "analysis.R", "README.md", "codebook.xlsx"))
data_classify_files <- function(file_name) {
  n <- length(file_name)
  if (n == 0) return(character(0))

  # Layer 1: metacheck name-based rules (readme / codebook / data / code)
  cat <- file_category(file_name)$file_category

  # Layer 2: extension crosswalk from metacheck::file_types
  ext <- tolower(tools::file_ext(file_name))
  coarse <- filetype(file_name)                 # named vector, may be "a;b"
  coarse_first <- sub(";.*$", "", unname(coarse))
  crosswalked <- unname(.file_type_crosswalk[coarse_first])

  type <- ifelse(!is.na(cat), cat, crosswalked)

  # Layer 3: format-locked extension overrides (highest priority)
  fixed <- unname(.fixed_ext_type[ext])
  type <- ifelse(!is.na(fixed), fixed, type)

  # README filename → readme (belt-and-braces; file_category usually catches it)
  type[grepl("^readme($|\\.)", tolower(file_name))] <- "readme"

  type[is.na(type)] <- "other"
  type
}

# ── Data format (tabular vs raw) ─────────────────────────────────────────────

.tabular_extensions <- c("csv", "tsv", "txt", "dat", "xlsx", "xls",
                         "sav", "dta", "sas7bdat")
.raw_extensions <- c(
  # EEG / physiological
  "edf", "bdf", "acq", "gdf", "rec", "cnt", "vhdr", "vmrk", "eeg", "mff",
  "set", "fdt", "fif",
  # Neuroimaging
  "nii", "img", "hdr", "mgh", "mgz", "mnc", "dcm",
  # Motion capture
  "c3d", "trc", "mot", "sto",
  # Array / scientific formats
  "mat", "h5", "hdf5", "hdf", "nc", "cdf", "npy", "npz", "pkl", "pickle",
  # Eye-tracking
  "asc",
  # Audio / video
  "wav", "mp3", "flac", "ogg", "m4a", "aiff", "aif", "au", "wma",
  "mp4", "avi", "mov", "mkv", "wmv", "m4v", "flv", "webm", "3gp",
  # Generic binary + documents (guard against a PDF classed as data)
  "bin", "raw", "pdf", "docx", "doc", "odt", "rtf"
)

#' Classify a data file as tabular or raw
#'
#' @param ext a character vector of lowercase file extensions (no leading dot)
#'
#' @returns `"tabular"` or `"raw"` for each element (never `NA`; unknown
#'   extensions fall back to `"tabular"`).
#' @export
#' @keywords internal
#'
#' @examples
#' data_format(c("csv", "edf", "mp4", "sav"))
data_format <- function(ext) {
  ifelse(tolower(ext) %in% .raw_extensions, "raw", "tabular")
}

#' Detect a file manifest / table-of-contents masquerading as tabular data
#'
#' A manifest (e.g. a "table of contents" CSV) is structurally a valid tabular
#' file, so extension-based classification treats it as data. It is
#' distinguished from real research data by content, using the repository's own
#' file list as ground truth: a manifest has a column in which most values name
#' other files in the repository. This is name- and header-agnostic — it does
#' not rely on the file or its columns being *called* anything in particular.
#'
#' To avoid demoting genuine data that merely references assets (e.g. a
#' `stimulus` column of image filenames), a candidate column must both reach the
#' `threshold` of file references and reference at least `min_exts` distinct file
#' extensions (a manifest points across code/data/docs; an asset column is
#' usually one extension).
#'
#' @param df a data.frame (the read tabular file)
#' @param repo_files a character vector of the other file names/paths in the
#'   same repository (basenames are compared)
#' @param threshold minimum fraction of a column's non-empty values that must
#'   resolve to repository files
#' @param min_exts minimum number of distinct referenced file extensions
#'
#' @returns `TRUE` when `df` looks like a file manifest, else `FALSE`.
#' @export
#' @keywords internal
data_is_manifest <- function(df, repo_files, threshold = 0.8, min_exts = 2L) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(FALSE)
  repo_files <- repo_files[!is.na(repo_files) & nzchar(repo_files)]
  if (length(repo_files) == 0) return(FALSE)
  repo_base <- tolower(basename(gsub("\\\\", "/", repo_files)))

  for (col in df) {
    vals <- tolower(trimws(as.character(col)))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) next
    vbase <- basename(gsub("\\\\", "/", vals))
    is_ref <- vbase %in% repo_base
    if (mean(is_ref) < threshold) next
    exts <- tools::file_ext(vals[is_ref])
    exts <- exts[nzchar(exts)]
    if (length(unique(exts)) >= min_exts) return(TRUE)
  }
  FALSE
}

# Default number of items per LLM classification call. Sized for reliable
# structured-array responses on small models (e.g. Groq's gpt-oss-20b): input
# tokens are not the binding constraint here — the limit is how many array items
# the model returns complete and correctly indexed. ~50 keeps responses reliable
# while cutting call count ~50x versus one-call-per-item. Used by every batched
# classifier in data_check so batch size is tuned in one place.
.data_check_llm_batch <- 50L

# Classify a vector of items with an LLM in index-mapped batches. Each batch
# sends a numbered listing of `item_texts` and expects an object-wrapped array
# of {index, value} objects back; results are mapped to positions by index, so a
# dropped or reordered entry never misaligns the others. Returns a character
# vector the same length as `item_texts` (NA where the LLM gave no valid value).
#
# `system_prompt` should instruct the model to return one {index, value} per
# input line; `value_desc` documents the `value` field; `valid` optionally
# restricts accepted values (others become NA). Runs only when llm_use(TRUE).
.llm_classify_batched <- function(item_texts, system_prompt, value_desc,
                                  valid = NULL, batch_size = .data_check_llm_batch,
                                  model = llm_model(), params = list()) {
  n <- length(item_texts)
  out <- rep(NA_character_, n)
  if (n == 0) return(out)

  # Object-wrapped array: some providers (Groq's gpt-oss-20b) 400 on a bare
  # top-level array; nesting under a field is accepted and llm() unwraps it.
  type_spec <- ellmer::type_object(
    results = ellmer::type_array(
      ellmer::type_object(
        index = ellmer::type_integer("The item's number in the list"),
        value = ellmer::type_string(value_desc)
      )
    )
  )

  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  model_used <- NA_character_
  for (rows in batches) {
    listing <- paste(seq_along(rows), item_texts[rows], sep = ". ",
                     collapse = "\n")
    resp <- tryCatch(
      llm(text = data.frame(text = listing), text_col = "text",
          system_prompt = system_prompt, type = type_spec, model = model,
          params = params),
      error = function(e) NULL
    )
    resp <- .strip_llm_wrapper(resp, "results")
    if (is.null(resp) || nrow(resp) == 0 ||
        !all(c("index", "value") %in% names(resp))) next
    if (is.na(model_used)) model_used <- attr(resp, "llm")$model %||% NA_character_
    idx <- suppressWarnings(as.integer(resp$index))
    val <- tolower(trimws(as.character(resp$value)))
    good <- !is.na(idx) & idx >= 1 & idx <= length(rows) & nzchar(val)
    if (!is.null(valid)) good <- good & val %in% valid
    if (any(good)) out[rows[idx[good]]] <- val[good]
  }
  attr(out, "llm_model") <- model_used
  out
}

#' Assign a study group to each file with an LLM
#'
#' Classifies every file in a repository into a study group from its path
#' (folder + name) context, so a multi-study repository can be split into
#' `study-<group>/` directories (used by `psychds_check`). Group codes follow
#' datacheck's scheme: `ex1`, `ex2a`, `pilot1`, ..., or `shared` for files that
#' belong to no single study (READMEs, shared materials). Only meaningful with
#' an LLM; callers keep `group = NA` when `llm_use(FALSE)`.
#'
#' Only files that will actually be analysed or placed (data, codebooks, code,
#' readmes, supplemental/output/other) are sent to the model; assets (images,
#' PDFs, and other non-analysable files) are never grouped and default to
#' `shared`. The sent files are batched (see `.data_check_llm_batch`) so large
#' repositories do not exceed the model's request/output limits.
#'
#' @param files a data.frame of files (needs `file_path` or `file_name`; an
#'   optional `data_type` column is used to skip assets)
#' @param model the LLM model name
#' @param params a named list passed to `llm()`
#' @param batch_size number of files per LLM call
#'
#' @returns a data.frame with a `group` column (one row per input file, same
#'   order) and an `"model"` attribute, or `NULL` on failure.
#' @export
#' @keywords internal
data_group_llm <- function(files, model = llm_model(), params = list(),
                           batch_size = .data_check_llm_batch) {
  return(.data_group_llm_impl(files, model, params, batch_size))
}

# When a structured schema wraps its array in a single object field (needed
# because some providers 400 on a bare top-level array), ellmer returns the
# inner fields prefixed with "<wrapper>." (e.g. assignments.index). Strip that
# prefix so consumers can read the un-prefixed column names either way.
.strip_llm_wrapper <- function(df, wrapper) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  pref <- paste0(wrapper, ".")
  hit <- startsWith(names(df), pref)
  if (any(hit)) names(df)[hit] <- sub(pref, "", names(df)[hit], fixed = TRUE)
  df
}

.data_group_llm_impl <- function(files, model = llm_model(), params = list(),
                                 batch_size = 30) {
  if (is.null(files) || nrow(files) == 0) return(NULL)
  paths <- if ("file_path" %in% names(files)) files$file_path else files$file_name
  paths <- ifelse(is.na(paths) | !nzchar(paths), files$file_name, paths)
  paths <- gsub("\\\\", "/", paths)

  # Only group files that will actually be analysed or placed into a study
  # directory: data files, codebooks, code, and readmes. Assets, generic
  # "other", and bulk output/supplemental files don't drive study structure, so
  # we don't spend LLM calls on them; they default to 'shared'. When no
  # data_type column is present we fall back to grouping everything.
  placeable <- c("data", "codebook", "software", "code", "readme")
  dtype <- if ("data_type" %in% names(files))
    tolower(as.character(files$data_type)) else rep(NA_character_, length(paths))
  send <- if (all(is.na(dtype))) rep(TRUE, length(paths)) else dtype %in% placeable
  if (!any(send)) return(data.frame(group = rep("shared", length(paths))))

  prompt <- paste(
    "You are grouping the files of a psychology research repository by study.",
    "Many repositories contain multiple studies (Experiment 1, Study 2a, a",
    "pilot, ...). Assign each numbered file to a study group using these codes:",
    "'ex1','ex2','ex2a',... for experiments/studies, 'pilot1','pilot2',... for",
    "pilots, and 'shared' for files that belong to no single study (top-level",
    "READMEs, shared materials, whole-repo codebooks). Infer groups from folder",
    "names and filenames. If the repository is a single study, use 'shared' for",
    "everything. Return one entry per input file, in the same order."
  )
  # Wrap the array in a single-field object. Some providers (notably Groq's
  # gpt-oss-20b) reject a top-level bare JSON array schema with HTTP 400
  # json_validate_failed; nesting it under an object field is accepted, and
  # llm()'s .unnest_result() unwraps the single-field object back into rows.
  type_spec <- ellmer::type_object(
    assignments = ellmer::type_array(
      ellmer::type_object(
        index = ellmer::type_integer("The file's number in the list"),
        group = ellmer::type_string("Study group code: ex1/ex2a/pilot1/shared")
      )
    )
  )

  # Batch the files to keep each request (and its structured array response)
  # within the model's limits. Each batch is numbered 1..n within itself so the
  # model returns small indices; we map them back via the batch's global rows.
  send_rows <- which(send)
  batches <- split(send_rows, ceiling(seq_along(send_rows) / batch_size))

  group <- rep("shared", length(paths))
  any_ok <- FALSE
  used_model <- NA_character_
  for (rows in batches) {
    listing <- paste(seq_along(rows), paths[rows], sep = ". ", collapse = "\n")
    resp <- tryCatch(
      llm(text = data.frame(text = listing), text_col = "text",
          system_prompt = prompt, type = type_spec, model = model,
          params = params),
      error = function(e) NULL
    )
    resp <- .strip_llm_wrapper(resp, "assignments")
    if (is.null(resp) || nrow(resp) == 0 ||
        !all(c("index", "group") %in% names(resp))) next
    idx <- suppressWarnings(as.integer(resp$index))
    grp <- tolower(trimws(as.character(resp$group)))
    ok  <- !is.na(idx) & idx >= 1 & idx <= length(rows) & nzchar(grp)
    if (any(ok)) {
      group[rows[idx[ok]]] <- grp[ok]
      any_ok <- TRUE
    }
    if (is.na(used_model)) used_model <- attr(resp, "llm")$model %||% NA_character_
  }
  if (!any_ok) return(NULL)

  out <- data.frame(group = group)
  attr(out, "model") <- used_model
  out
}

# ── Tabular reading ──────────────────────────────────────────────────────────

# Sniff the field delimiter of a delimited text file from its first
# non-blank, non-comment line.
.sniff_delimiter <- function(path) {
  line <- character(0)
  con  <- file(path, "r")
  on.exit(close(con))
  for (i in seq_len(10)) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) break
    l <- trimws(line)
    if (nchar(l) > 0 && !startsWith(l, "#")) break
  }
  if (length(line) == 0) return(",")
  candidates <- c(",", ";", "\t", "|")
  counts <- vapply(candidates, function(d)
    nchar(line) - nchar(gsub(d, "", line, fixed = TRUE)), integer(1))
  if (max(counts) == 0) "," else candidates[which.max(counts)]
}

# Decide whether a delimited text file has a header row. A file is treated as
# headerless when its first two non-comment rows both look all-numeric (real
# headers carry at least one textual label). With <2 readable rows we assume a
# header (safer default).
.detect_header <- function(path, sep) {
  con   <- file(path, "r")
  on.exit(close(con))
  lines <- character(0)
  while (length(lines) < 2) {
    l <- readLines(con, n = 1, warn = FALSE)
    if (length(l) == 0) break
    if (nzchar(trimws(l)) && !startsWith(trimws(l), "#")) lines <- c(lines, l)
  }
  if (length(lines) < 2) return(TRUE)
  split_row <- function(l)
    trimws(gsub('^"|"$', '', strsplit(l, sep, fixed = TRUE)[[1]]))
  is_num <- function(x) {
    if (!nzchar(x)) return(TRUE)
    if (toupper(x) %in% c("NA", "NAN", "NULL", "INF", "-INF", "+INF")) return(TRUE)
    suppressWarnings(!is.na(as.numeric(x)))
  }
  all_num <- function(toks) length(toks) > 0 && all(vapply(toks, is_num, logical(1)))
  !(all_num(split_row(lines[1])) && all_num(split_row(lines[2])))
}

#' Read the head of a data file regardless of format
#'
#' Reads the first `n_rows` of a tabular data file (csv/tsv/txt/dat/xlsx/xls/
#' sav/dta/sas7bdat/rds/rda/rdata). Delimiter and header presence are
#' auto-detected for delimited text; invalid UTF-8 triggers a latin1 retry.
#'
#' @param path path to a data file
#' @param n_rows number of rows to read (`Inf` for all)
#'
#' @returns a data.frame, or `NULL` on failure / unsupported format.
#' @export
#' @keywords internal
data_read_head <- function(path, n_rows = 5) {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    df <- switch(ext,
      csv = , txt = , tsv = , dat = {
        sep <- if (ext == "tsv") "\t" else .sniff_delimiter(path)
        hdr <- .detect_header(path, sep)
        df <- suppressWarnings(
          utils::read.delim(path, sep = sep, header = hdr, nrows = n_rows,
                            check.names = FALSE)
        )
        has_invalid <- any(vapply(df, function(col) {
          is.character(col) && any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
        }, logical(1)))
        if (has_invalid) {
          df <- suppressWarnings(
            utils::read.delim(path, sep = sep, header = hdr, nrows = n_rows,
                              check.names = FALSE, fileEncoding = "latin1")
          )
        }
        if (!hdr && !is.null(df) && ncol(df) > 0)
          names(df) <- paste0("col_", seq_len(ncol(df)))
        df
      },
      xlsx = , xls = {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("The 'readxl' package is required to read Excel files.")
        nmax <- if (is.finite(n_rows)) n_rows else Inf
        # Suppress readxl's per-cell type-guess warnings ("Expecting numeric ...
        # got a date"): a mixed column can emit one per row (hundreds on a wide
        # sheet). We re-classify column types ourselves via data_col_type(), so
        # readxl's guess is not relied upon.
        suppressWarnings(as.data.frame(readxl::read_excel(path, n_max = nmax)))
      },
      sav = , dta = , sas7bdat = {
        if (!requireNamespace("haven", quietly = TRUE))
          stop("The 'haven' package is required to read SPSS/Stata/SAS files.")
        nmax <- if (is.finite(n_rows)) n_rows else Inf
        as.data.frame(switch(ext,
          sav      = haven::read_sav(path, n_max = nmax),
          dta      = haven::read_dta(path, n_max = nmax),
          sas7bdat = haven::read_sas(path, n_max = nmax)))
      },
      rds = {
        obj <- readRDS(path)
        if (is.data.frame(obj)) utils::head(obj, n_rows) else NULL
      },
      rda = , rdata = {
        env <- new.env()
        load(path, envir = env)
        dfs <- Filter(is.data.frame, as.list(env))
        if (length(dfs) > 0) utils::head(dfs[[1]], n_rows) else NULL
      },
      NULL
    )
    # Coerce column names to valid UTF-8. A stray non-UTF-8 byte in a header
    # (e.g. a Latin-1 or BOM byte the file's own read tolerated) otherwise
    # crashes downstream `grepl(..., perl = TRUE)` name checks with "invalid
    # multibyte string". Sub out invalid bytes rather than dropping the column.
    if (!is.null(df) && !is.null(names(df))) {
      nm <- names(df)
      bad <- is.na(iconv(nm, from = "UTF-8", to = "UTF-8"))
      if (any(bad)) {
        fixed <- iconv(nm[bad], from = "latin1", to = "UTF-8", sub = "")
        fixed[is.na(fixed) | !nzchar(fixed)] <- paste0("col_", which(bad))[is.na(fixed) | !nzchar(fixed)]
        nm[bad] <- fixed
        names(df) <- nm
      }
    }
    df
  }, error = function(e) {
    if (grepl("time limit", conditionMessage(e), ignore.case = TRUE)) stop(e)
    warning("Could not read ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

# ── Column-type classification (rules only) ──────────────────────────────────

# data_check column types. The LLM-only refinements (ordinal/categorical for
# ambiguous integer columns) are not produced by the rules path; ambiguous
# columns fall back to continuous (numeric) or text (character).
.data_check_col_types <- c(
  "continuous", "binary", "categorical", "ordinal", "date", "id",
  "text", "continuous_comma_decimal", "continuous_outliers_excluded",
  "empty", "constant", "unknown"
)

#' Classify a single data column by rule
#'
#' Rule order (ported from datacheck `classify_col_type_rules()`): all-NA →
#' empty; ID name pattern → id; 1 unique → constant; 2 unique → binary;
#' date-parseable → date; long strings → text; numeric → continuous (or
#' ambiguous integer, flagged for LLM); comma-decimal → continuous variants.
#'
#' @param col_name the column's name (drives the ID-pattern rule)
#' @param values the column's values (a vector)
#'
#' @returns a list with `col_type` (a value from [.data_check_col_types], or
#'   `NA` when only the LLM could decide), `ambiguous` (whether the LLM should
#'   be consulted), `numeric_values` (numeric vector for stats, or `NULL`),
#'   `n_coerced`, and `is_numeric`.
#' @export
#' @keywords internal
#'
#' @examples
#' data_col_type("age", c(23, 45, 31, 29))
#' data_col_type("subject_id", c("s01", "s02", "s03"))
data_col_type <- function(col_name, values) {
  # Guard against a non-UTF-8 column name reaching the perl grepl below, which
  # errors (not just warns) on some code paths. data_read_head() sanitises names
  # at read time; this is belt-and-braces for direct callers.
  if (length(col_name) && !is.na(col_name) &&
      is.na(iconv(col_name, "UTF-8", "UTF-8")))
    col_name <- iconv(col_name, "latin1", "UTF-8", sub = "")

  x_noNA <- values[!is.na(values)]
  n_noNA <- length(x_noNA)

  if (n_noNA == 0)
    return(list(col_type = "empty", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  n_unique <- length(unique(x_noNA))

  id_pat <- paste0(
    "(?i)(",
    "^(participant|subject|subj|respondent|pp|ppt|pid|sub)$",
    "|^id$",
    "|[_\\-\\.](id|number|num|nr|no|code)$",
    "|^(subjectid|subjectnumber|responseid|recordid|participantid|",
    "subjectno|subjectnum|subjectcode|participantno|participantnum)$",
    "|^sub[_\\-]\\d",
    "|^(participant|subject|subj|pp|sub)[_\\-]?\\d+$",
    ")"
  )
  if (grepl(id_pat, col_name, perl = TRUE))
    return(list(col_type = "id", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (n_unique == 1)
    return(list(col_type = "constant", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (n_unique == 2)
    return(list(col_type = "binary", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  char_sample <- as.character(unique(x_noNA))[seq_len(min(20, n_unique))]
  n_date_ok <- sum(vapply(char_sample, function(v) {
    tryCatch(!is.na(as.Date(v)), warning = function(w) FALSE, error = function(e) FALSE)
  }, logical(1)))
  if (n_date_ok / length(char_sample) >= 0.70)
    return(list(col_type = "date", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (stats::median(nchar(as.character(x_noNA))) > 40)
    return(list(col_type = "text", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  if (is.numeric(values)) {
    if (any(x_noNA != floor(x_noNA)) || n_unique > 20)
      return(list(col_type = "continuous", ambiguous = FALSE,
                  numeric_values = values, n_coerced = NA_integer_,
                  is_numeric = FALSE))
    # ambiguous integer 3–20 unique: rules can't tell ordinal/categorical/
    # continuous apart. LLM-off → treat as continuous.
    return(list(col_type = NA_character_, ambiguous = TRUE,
                numeric_values = values, n_coerced = NA_integer_,
                is_numeric = TRUE))
  }

  x_sub  <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA), fixed = TRUE)))
  pct_ok <- sum(!is.na(x_sub)) / n_noNA
  if (pct_ok >= 0.95) {
    num_vec <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    return(list(col_type = "continuous_comma_decimal", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = sum(is.na(x_sub)),
                is_numeric = FALSE))
  }
  if (pct_ok >= 0.80) {
    num_vec <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    return(list(col_type = "continuous_outliers_excluded", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = sum(is.na(x_sub)),
                is_numeric = FALSE))
  }

  # remaining character columns: LLM would decide categorical/text/... — off → text
  list(col_type = NA_character_, ambiguous = TRUE, numeric_values = NULL,
       n_coerced = NA_integer_, is_numeric = FALSE)
}

# ── Column statistics ────────────────────────────────────────────────────────

#' Summary statistics for a numeric column
#'
#' @param x_for_stats a numeric vector (may contain NA)
#' @param x_raw the raw source column (for n / n_missing / n_unique)
#'
#' @returns a one-row data.frame of statistics.
#' @export
#' @keywords internal
data_col_stats <- function(x_for_stats, x_raw) {
  n_unique_val <- length(unique(x_raw[!is.na(x_raw)]))
  empty_stats <- function(n, n_miss) data.frame(
    n = n, n_missing = n_miss, n_unique = n_unique_val,
    mean = NA_real_, sd = NA_real_, se = NA_real_, median = NA_real_,
    min = NA_real_, max = NA_real_, range = NA_real_, p25 = NA_real_,
    p75 = NA_real_, iqr = NA_real_, skewness = NA_real_, kurtosis = NA_real_
  )

  if (is.null(x_for_stats)) {
    n_miss <- sum(is.na(x_raw)); n_val <- length(x_raw) - n_miss
    return(empty_stats(n_val, n_miss))
  }

  x <- as.numeric(x_for_stats)
  x <- x[!is.na(x) & !is.nan(x)]
  n <- length(x)
  n_miss <- sum(is.na(x_for_stats))
  if (n == 0) return(empty_stats(0L, n_miss))

  mn <- mean(x)
  s  <- if (n > 1) stats::sd(x) else NA_real_
  p25 <- stats::quantile(x, 0.25, names = FALSE)
  p75 <- stats::quantile(x, 0.75, names = FALSE)
  data.frame(
    n = n, n_missing = n_miss, n_unique = n_unique_val,
    mean = mn,
    sd = s,
    se = if (!is.na(s)) s / sqrt(n) else NA_real_,
    median = stats::median(x),
    min = min(x), max = max(x), range = max(x) - min(x),
    p25 = p25, p75 = p75, iqr = p75 - p25,
    skewness = if (n > 2 && !is.na(s) && s > 0) mean((x - mn)^3) / s^3 else NA_real_,
    kurtosis = if (n > 3 && !is.na(s) && s > 0) mean((x - mn)^4) / s^4 - 3 else NA_real_
  )
}

# ── Codebook parsing + column matching (used by codebook_check) ───────────────
#
# Ported from datacheck's 2_codebook_label.R / helper.R. The rules-only path
# (structured CSV/Excel, haven embedded labels, rich-text extraction, exact and
# normalised name matching) runs with `llm_use(FALSE)`; the LLM tiers (parsing
# unstructured codebooks, fuzzy column matching, semantic label merging) are
# gated behind `llm_use(TRUE)` in codebook_check.R.

# Normalise a variable/column name for matching: lowercase, underscores → space,
# collapse whitespace, strip leading/trailing dots.
normalize_varname <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("[_]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^[.]+|[.]+$", "", x)
  trimws(x)
}

# Reduce each word of an already-lowercased, punctuation-stripped label to its
# Porter stem (SnowballC), falling back to a crude trailing-"s" stripper when
# SnowballC is unavailable so matching still runs in minimal environments.
.stem_words <- local({
  have_snowball <- NULL
  function(s) {
    if (is.null(have_snowball))
      have_snowball <<- requireNamespace("SnowballC", quietly = TRUE)
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    words <- words[nzchar(words)]
    if (length(words) == 0) return("")
    if (isTRUE(have_snowball)) {
      stemmed <- tryCatch(SnowballC::wordStem(words, language = "porter"),
                          error = function(e) NULL)
      if (!is.null(stemmed)) return(paste(stemmed, collapse = " "))
    }
    paste(sub("^([a-z]{7,})s$", "\\1", words, perl = TRUE), collapse = " ")
  }
})

# Normalise a label for semantic-equivalence comparison: strip possessives and
# punctuation, Porter-stem each word, collapse whitespace. So "Participants'
# responses" and "Participant response" normalise to the same string.
normalize_label <- function(x) {
  x <- tolower(x)
  x <- gsub("'s|’s|‘s", "", x, perl = TRUE)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  vapply(x, .stem_words, character(1), USE.NAMES = FALSE)
}

# Scan a data.frame's headers for a "variable name" column and a "label" column.
# Returns list(var_col, lab_col) or NULL.
.find_codebook_cols <- function(col_names) {
  var_col <- grep(
    "(?i)^(var(iable)?|name|column|field|variable[_ ]?name|varname|item)$",
    col_names, perl = TRUE, value = TRUE
  )[1]
  lab_col <- grep(
    paste0("(?i)^(label|description|desc|definition|meaning|explanation|text|",
           "label[_ ]?text|question|question[_ ]?text|variable[_ ]?description|",
           "variable[_ ]?label|var[_ ]?label)$"),
    col_names, perl = TRUE, value = TRUE
  )[1]
  if (is.na(var_col) || is.na(lab_col)) return(NULL)
  list(var_col = var_col, lab_col = lab_col)
}

# Empty codebook-variable table (the canonical column set).
.empty_codebook_vars <- function() {
  data.frame(
    codebook_variable = character(0), label = character(0),
    codebook_source = character(0), group = character(0),
    parse_method = character(0)
  )
}

# Extract variable-label pairs from a structured data.frame (CSV/Excel rows).
# Returns NULL when no matching header columns are found.
.extract_structured_codebook <- function(df, src) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) return(NULL)
  cols <- .find_codebook_cols(names(df))
  if (is.null(cols)) return(NULL)
  rows <- df[nzchar(trimws(as.character(df[[cols$var_col]]))), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  data.frame(
    codebook_variable = as.character(rows[[cols$var_col]]),
    label             = as.character(rows[[cols$lab_col]]),
    codebook_source   = src,
    group             = NA_character_
  )
}

# Extract embedded variable labels from a haven-read data.frame (SPSS/Stata/SAS).
# Returns NULL if no labelled columns found. Caller adds parse_method = "haven".
.extract_haven_labels <- function(df, src) {
  labels <- vapply(names(df), function(col) {
    lbl <- attr(df[[col]], "label")
    if (is.null(lbl)) NA_character_ else trimws(as.character(lbl[1]))
  }, character(1))
  has_label <- !is.na(labels) & nzchar(labels)
  if (!any(has_label)) return(NULL)
  data.frame(
    codebook_variable = names(df)[has_label],
    label             = labels[has_label],
    codebook_source   = src,
    group             = NA_character_
  )
}

# Strip RTF control codes from a string, returning plain text.
.strip_rtf <- function(text) {
  text <- gsub("\\\\[a-z]+\\-?[0-9]*\\s?", " ", text)
  text <- gsub("\\\\[^a-z\n]", " ", text)
  text <- gsub("[{}]", "", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

# Extract plain text from a rich-text or binary codebook file (docx/pdf/rtf/
# odt). Returns "" on any failure or missing optional dependency.
.extract_rich_text <- function(path, ext) {
  tryCatch({
    switch(ext,
      docx = {
        if (!requireNamespace("officer", quietly = TRUE)) return("")
        doc  <- officer::read_docx(path)
        summ <- officer::docx_summary(doc)
        txt  <- as.character(summ$text)
        paste(txt[nzchar(trimws(txt))], collapse = "\n")
      },
      pdf = {
        if (!requireNamespace("pdftools", quietly = TRUE)) return("")
        paste(pdftools::pdf_text(path), collapse = "\n")
      },
      rtf = {
        .strip_rtf(paste(readLines(path, warn = FALSE), collapse = "\n"))
      },
      odt = {
        tmp <- tempfile()
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        dir.create(tmp)
        tryCatch({
          utils::unzip(path, files = "content.xml", exdir = tmp)
          xml_path <- file.path(tmp, "content.xml")
          if (!file.exists(xml_path)) return("")
          raw <- paste(readLines(xml_path, warn = FALSE), collapse = "\n")
          txt <- gsub("<[^>]+>", " ", raw)
          txt <- gsub("&amp;", "&", txt, fixed = TRUE)
          txt <- gsub("&lt;", "<", txt, fixed = TRUE)
          txt <- gsub("&gt;", ">", txt, fixed = TRUE)
          txt <- gsub("&apos;", "'", txt, fixed = TRUE)
          txt <- gsub("&quot;", '"', txt, fixed = TRUE)
          trimws(gsub("\\s+", " ", txt))
        }, error = function(e) "")
      },
      ""
    )
  }, error = function(e) "")
}

#' Parse a codebook file into variable definitions
#'
#' Rule-based codebook reader. Handles structured tables (CSV/TSV/Excel with a
#' variable-name column and a label column, including wide-format transposition
#' and multi-row header scanning), and embedded haven labels (SPSS/Stata). For
#' rich-text formats (docx/pdf/rtf/odt) it extracts plain text. Files that yield
#' no structured definitions return their raw text lines (character vector) so
#' the caller can route them to an LLM when `llm_use(TRUE)`.
#'
#' @param path path to a codebook/readme file
#' @param header_lookahead rows to scan for a header in multi-level CSVs
#'
#' @returns a data.frame of variable definitions (`codebook_variable`, `label`,
#'   `codebook_source`, `group`, `parse_method`); a character vector of text
#'   lines when only unstructured text is available; or `NULL` on failure.
#' @export
#' @keywords internal
parse_codebook <- function(path, header_lookahead = 5L) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  src <- basename(path)

  result <- tryCatch(
    switch(ext,
      csv = , tsv = , dat = {
        sep <- if (ext == "tsv") "\t" else .sniff_delimiter(path)
        raw <- tryCatch(
          utils::read.delim(path, sep = sep, header = FALSE,
                            check.names = FALSE),
          error = function(e) NULL
        )
        if (is.null(raw) || nrow(raw) == 0) {
          NULL
        } else {
          has_invalid <- any(vapply(raw, function(col) {
            is.character(col) &&
              any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
          }, logical(1)))
          if (has_invalid) {
            raw <- tryCatch(
              utils::read.delim(path, sep = sep, header = FALSE,
                                check.names = FALSE, fileEncoding = "latin1"),
              error = function(e) NULL
            )
          }
          if (is.null(raw) || nrow(raw) == 0) {
            NULL
          } else {
            # Wide-format detection: variables as columns, stats as rows. If
            # >=50% of first-column values are known statistic names, transpose.
            wide_stats <- c("mean", "sd", "se", "min", "max", "median", "n")
            col1 <- trimws(tolower(as.character(raw[, 1])))
            col1 <- col1[nzchar(col1)]
            if (length(col1) > 0 && mean(col1 %in% wide_stats) >= 0.5) {
              var_names  <- as.character(raw[1, ])
              stat_names <- as.character(raw[, 1])
              traw <- as.data.frame(t(raw[, -1, drop = FALSE]))
              names(traw) <- stat_names[-1]
              raw <- cbind(data.frame(variable = var_names[-1]), traw)
              rownames(raw) <- NULL
            }
            header_row <- NA_integer_
            for (k in seq_len(min(nrow(raw), header_lookahead))) {
              if (!is.null(.find_codebook_cols(trimws(as.character(raw[k, ]))))) {
                header_row <- k
                break
              }
            }
            if (is.na(header_row)) {
              NULL
            } else {
              names(raw) <- trimws(as.character(raw[header_row, ]))
              df <- raw[seq(header_row + 1L, nrow(raw)), , drop = FALSE]
              rownames(df) <- NULL
              .extract_structured_codebook(df, src)
            }
          }
        }
      },
      xlsx = , xls = {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          NULL
        } else {
          df <- tryCatch(as.data.frame(readxl::read_excel(path)),
                         error = function(e) NULL)
          .extract_structured_codebook(df, src)
        }
      },
      sav = , dta = , sas7bdat = {
        if (!requireNamespace("haven", quietly = TRUE)) {
          NULL
        } else {
          df <- switch(ext,
            sav      = haven::read_sav(path),
            dta      = haven::read_dta(path),
            sas7bdat = haven::read_sas(path))
          res <- .extract_haven_labels(as.data.frame(df), src)
          if (!is.null(res)) attr(res, ".is_haven") <- TRUE
          res
        }
      },
      docx = , pdf = , rtf = , odt = {
        text <- .extract_rich_text(path, ext)
        if (nchar(trimws(text)) < 10) NULL else strsplit(text, "\n")[[1]]
      },
      NULL
    ),
    error = function(e) NULL
  )

  # Rich-text formats hand back a character vector of lines for the LLM tier.
  if (is.character(result) && !is.data.frame(result)) return(result)

  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    result$parse_method <- if (isTRUE(attr(result, ".is_haven"))) "haven"
                           else "structured"
    attr(result, ".is_haven") <- NULL
    return(result)
  }

  # No structured definitions: return raw lines so the caller can try the LLM.
  tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
}

# Map free-text experiment context strings to canonical group codes, e.g.
# "Experiment 1" -> "ex1", "Study 2a" -> "ex2a", "Pilot 1" -> "pilot1".
.infer_group <- function(context_str) {
  vapply(context_str, function(s) {
    if (is.null(s) || is.na(s) || !nzchar(trimws(as.character(s))))
      return(NA_character_)
    s <- trimws(as.character(s))
    m <- regmatches(s, regexpr("(?i)pilot\\s*(\\d+[a-z]?)", s, perl = TRUE))
    if (length(m) > 0 && nzchar(m)) {
      num <- sub("(?i)pilot\\s*", "", m, perl = TRUE)
      return(paste0("pilot", tolower(num)))
    }
    m <- regmatches(s, regexpr("(?i)(experiment|study)\\s*(\\d+[a-z]?)", s, perl = TRUE))
    if (length(m) > 0 && nzchar(m)) {
      num <- sub("(?i)(experiment|study)\\s*", "", m, perl = TRUE)
      return(paste0("ex", tolower(num)))
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Match data columns against codebook variable definitions (rules only)
#'
#' For each column in `columns_df`, find codebook variables whose normalised
#' name matches, respecting experiment-group scoping. Resolves multiple
#' definitions by haven priority, then rule-based label-equivalence
#' (`normalize_label`); genuinely differing labels are flagged
#' `conflicting_definition`. The LLM tiers (fuzzy matching, semantic merge) are
#' applied separately by codebook_check when `llm_use(TRUE)`.
#'
#' @param columns_df data columns to label (needs `paper_id`, `source_file`,
#'   `column_name`; optional `group`/`experiment_group`)
#' @param codebook_vars_df parsed codebook variables (from [parse_codebook()])
#'
#' @returns a data.frame with one row per input column: `paper_id`,
#'   `source_file`, `column_name`, `group`, `label`, `codebook_variable`,
#'   `label_source`, `label_status`, `label_method`.
#' @export
#' @keywords internal
match_column_labels <- function(columns_df, codebook_vars_df) {
  col_group <- if ("group" %in% names(columns_df)) columns_df$group else
               if ("experiment_group" %in% names(columns_df)) columns_df$experiment_group else
               rep(NA_character_, nrow(columns_df))

  make_empty <- function(status = "unlabelled") {
    data.frame(
      paper_id          = columns_df$paper_id,
      source_file       = columns_df$source_file,
      column_name       = columns_df$column_name,
      group             = col_group,
      label             = NA_character_,
      codebook_variable = NA_character_,
      label_source      = NA_character_,
      label_status      = status,
      label_method      = NA_character_
    )
  }

  if (is.null(columns_df) || nrow(columns_df) == 0) return(make_empty())
  if (is.null(codebook_vars_df) || nrow(codebook_vars_df) == 0)
    return(make_empty())

  norm_col <- normalize_varname(columns_df$column_name)

  # Expand range notation (e.g. "V1-V10") into individual variable rows.
  range_pat  <- "^([A-Za-z]*)\\s*(\\d+)\\s*[-–]\\s*(\\d+)$"
  range_rows <- grep(range_pat, codebook_vars_df$codebook_variable, perl = TRUE)
  if (length(range_rows) > 0) {
    expanded <- Filter(Negate(is.null), lapply(range_rows, function(i) {
      parts <- regmatches(
        codebook_vars_df$codebook_variable[i],
        regexec(range_pat, codebook_vars_df$codebook_variable[i], perl = TRUE)
      )[[1]]
      prefix <- parts[2]; start <- as.integer(parts[3]); end <- as.integer(parts[4])
      if (is.na(start) || is.na(end) || start > end) return(NULL)
      row <- codebook_vars_df[i, , drop = FALSE]
      do.call(rbind, lapply(seq(start, end), function(nn) {
        row$codebook_variable <- paste0(prefix, nn)
        row
      }))
    }))
    if (length(expanded) > 0)
      codebook_vars_df <- rbind(
        codebook_vars_df[-range_rows, , drop = FALSE],
        do.call(rbind, expanded)
      )
  }

  norm_var <- normalize_varname(codebook_vars_df$codebook_variable)
  n <- nrow(columns_df)
  label_out <- cbk_var_out <- src_out <- label_method_out <- rep(NA_character_, n)
  status_out <- rep("unlabelled", n)

  for (i in seq_len(n)) {
    name_idx <- which(norm_var == norm_col[i])
    if (length(name_idx) == 0) next
    cg <- col_group[i]

    matches  <- codebook_vars_df[name_idx, , drop = FALSE]
    scoped   <- matches[!is.na(matches$group), , drop = FALSE]
    unscoped <- matches[ is.na(matches$group), , drop = FALSE]
    same_group   <- scoped[!is.na(scoped$group) & scoped$group == cg, , drop = FALSE]
    applicable   <- rbind(unscoped, same_group)
    other_scoped <- scoped[!is.na(scoped$group) & scoped$group != cg, , drop = FALSE]

    if (nrow(applicable) == 0) {
      if (nrow(other_scoped) > 0) {
        status_out[i]  <- "ambiguous_experiment"
        label_out[i]   <- paste(unique(other_scoped$label), collapse = " | ")
        cbk_var_out[i] <- paste(unique(other_scoped$codebook_variable), collapse = " | ")
        src_out[i]     <- paste(unique(other_scoped$codebook_source), collapse = " | ")
      }
      next
    }

    distinct_labels <- unique(applicable$label)
    if (length(distinct_labels) > 1) {
      haven_rows <- if ("parse_method" %in% names(applicable))
        applicable[!is.na(applicable$parse_method) &
                     applicable$parse_method == "haven", , drop = FALSE] else
        applicable[0, , drop = FALSE]
      norm_labels <- normalize_label(distinct_labels)
      if (nrow(haven_rows) > 0) {
        status_out[i]       <- "labelled"
        label_out[i]        <- haven_rows$label[which.max(nchar(haven_rows$label))]
        cbk_var_out[i]      <- haven_rows$codebook_variable[1]
        src_out[i]          <- paste(unique(haven_rows$codebook_source), collapse = " | ")
        label_method_out[i] <- "haven_priority"
      } else if (length(unique(norm_labels)) == 1) {
        status_out[i]       <- "labelled"
        label_out[i]        <- distinct_labels[which.max(nchar(distinct_labels))]
        cbk_var_out[i]      <- applicable$codebook_variable[1]
        src_out[i]          <- paste(unique(applicable$codebook_source), collapse = " | ")
        label_method_out[i] <- "merged_rules"
      } else {
        status_out[i]  <- "conflicting_definition"
        label_out[i]   <- paste(distinct_labels, collapse = " | ")
        cbk_var_out[i] <- paste(unique(applicable$codebook_variable), collapse = " | ")
        src_out[i]     <- paste(unique(applicable$codebook_source), collapse = " | ")
      }
    } else {
      status_out[i]  <- "labelled"
      label_out[i]   <- distinct_labels[1]
      cbk_var_out[i] <- applicable$codebook_variable[1]
      src_out[i]     <- paste(unique(applicable$codebook_source), collapse = " | ")
    }
  }

  label_method_out[status_out == "labelled" & is.na(label_method_out)] <- "rules"

  data.frame(
    paper_id          = columns_df$paper_id,
    source_file       = columns_df$source_file,
    column_name       = columns_df$column_name,
    group             = col_group,
    label             = label_out,
    codebook_variable = cbk_var_out,
    label_source      = src_out,
    label_status      = status_out,
    label_method      = label_method_out
  )
}

# ── Data-quality checks (native, used by data_validate) ───────────────────────
#
# Clean-room reimplementations of common data-screening checks. Each returns a
# list(problem = <logical>, message = <chr>, values = <flagged values or NULL>),
# so callers can treat them uniformly. Deliberately dependency-free base R (the
# equivalent checks in the dataReporter package are GPL-2 and pull in
# robustbase + an S3 framework; the logic itself is small, so we own it here).

# Common sentinel values that disguise missingness in shared data.
.data_missing_sentinels <- c(-99, -999, -9999, 99, 999, 9999, -1)

#' Flag values that look like miscoded missing data
#'
#' @param x a numeric vector
#' @param sentinels candidate sentinel values
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_miscoded_missing <- function(x, sentinels = .data_missing_sentinels) {
  if (!is.numeric(x)) return(list(problem = FALSE, message = "", values = NULL))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(problem = FALSE, message = "", values = NULL))
  # A sentinel is suspicious only if it sits far from the bulk of the data: an
  # isolated extreme repeated value. Require it to be an outlier and to recur.
  found <- sentinels[vapply(sentinels, function(s) {
    n_s <- sum(x == s)
    n_s >= 2 && (s < stats::quantile(x, 0.05) || s > stats::quantile(x, 0.95))
  }, logical(1))]
  if (length(found) == 0) return(list(problem = FALSE, message = "", values = NULL))
  # Report each flagged code with how often it occurs, framed as a "this is
  # probably a missing-data placeholder that was left as a number, and should
  # likely be NA" warning — not a claim that the value itself is wrong.
  parts <- vapply(found, function(s)
    sprintf("%s (appears %d times)", s, sum(x == s)), character(1))
  lead <- if (length(found) == 1)
    "A value far outside the data range looks like" else
    "Values far outside the data range look like"
  list(problem = TRUE,
       message = sprintf(
         "%s a missing-data code left as a number and may need to be recoded to NA: %s.",
         lead, paste(parts, collapse = ", ")),
       values = found)
}

#' Flag Tukey (IQR) outliers in a numeric vector
#'
#' Values below Q1 - k*IQR or above Q3 + k*IQR. This is the symmetric boxplot
#' rule; a skew-aware (medcouple) variant can be added later.
#'
#' @param x a numeric vector
#' @param k IQR multiplier (default 1.5)
#' @param n_max max number of flagged values to list in the message
#' @returns list(problem, message, values, lower, upper)
#' @export
#' @keywords internal
data_check_outliers <- function(x, k = 1.5, n_max = 10) {
  none <- list(problem = FALSE, message = "", values = NULL,
               lower = NA_real_, upper = NA_real_)
  if (!is.numeric(x)) return(none)
  x <- x[!is.na(x) & !is.nan(x)]
  if (length(x) < 4) return(none)
  qs <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
  iqr <- qs[2] - qs[1]
  if (iqr == 0) return(none)
  lower <- qs[1] - k * iqr
  upper <- qs[2] + k * iqr
  out <- unique(x[x < lower | x > upper])
  if (length(out) == 0)
    return(list(problem = FALSE, message = "", values = NULL,
                lower = lower, upper = upper))
  shown <- utils::head(sort(out), n_max)
  list(problem = TRUE,
       message = sprintf("%d outlier value%s outside [%.3g, %.3g]: %s%s",
                         length(out), plural(length(out)), lower, upper,
                         paste(signif(shown, 4), collapse = ", "),
                         if (length(out) > n_max) ", ..." else ""),
       values = out, lower = lower, upper = upper)
}

#' Flag a constant or near-constant column
#'
#' @param x a vector
#' @param threshold if the most common non-NA value covers at least this
#'   fraction, the column is near-constant
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_constant <- function(x, threshold = 0.99) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(problem = FALSE, message = "", values = NULL))
  tab <- sort(table(x), decreasing = TRUE)
  top_frac <- tab[[1]] / length(x)
  if (length(tab) == 1)
    return(list(problem = TRUE, message = "Column is constant (one unique value).",
                values = names(tab)[1]))
  if (top_frac >= threshold)
    return(list(problem = TRUE,
                message = sprintf("Near-constant: %.0f%% of values are \"%s\".",
                                 100 * top_frac, names(tab)[1]),
                values = names(tab)[1]))
  list(problem = FALSE, message = "", values = NULL)
}

#' Flag categorical levels that differ only by letter case
#'
#' e.g. "Male" and "male" — likely the same category entered inconsistently.
#'
#' @param x a character or factor vector
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_case_issues <- function(x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- as.character(x)
  x <- unique(x[!is.na(x) & nzchar(trimws(x))])
  if (length(x) == 0) return(none)
  lower <- tolower(x)
  dup <- lower[duplicated(lower)]
  if (length(dup) == 0) return(none)
  groups <- vapply(unique(dup), function(l)
    paste(x[lower == l], collapse = "/"), character(1))
  list(problem = TRUE,
       message = sprintf("Categories differing only by case: %s",
                         paste(groups, collapse = "; ")),
       values = groups)
}

#' Flag sparsely represented categorical levels
#'
#' @param x a character or factor vector
#' @param min_n levels with fewer than this many observations are flagged
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_sparse_levels <- function(x, min_n = 2L) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(none)
  tab <- table(x)
  sparse <- names(tab)[tab < min_n]
  if (length(sparse) == 0) return(none)
  list(problem = TRUE,
       message = sprintf("%d level%s with fewer than %d observation%s: %s",
                         length(sparse), plural(length(sparse)), min_n,
                         plural(min_n),
                         paste(utils::head(sparse, 10), collapse = ", ")),
       values = sparse)
}

#' Flag values with leading or trailing whitespace
#'
#' Padded values (e.g. "Male " vs "Male") silently split a category. Flags the
#' affected values in a character/factor column.
#'
#' @param x a character or factor vector
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_whitespace <- function(x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- as.character(x)
  x <- x[!is.na(x)]
  padded <- unique(x[x != trimws(x) & nzchar(trimws(x))])
  if (length(padded) == 0) return(none)
  list(problem = TRUE,
       message = sprintf("%d value%s with leading/trailing whitespace: %s",
                         length(padded), plural(length(padded)),
                         paste(utils::head(sprintf('"%s"', padded), 10),
                               collapse = ", ")),
       values = padded)
}

#' Flag a mostly-numeric column stored as text
#'
#' When a column read as character is mostly numbers but has a few values that
#' do not parse (e.g. "n/a", ">100", "50 approx"), those dirty cells forced the
#' whole column to text — a data-quality problem in the source, not a read
#' error. A *fully* numeric text column is not flagged here: the file readers
#' auto-type clean numeric columns, so an all-numeric character column would
#' indicate a reader problem rather than a data problem.
#'
#' @param x a character or factor vector
#' @param threshold minimum fraction of non-empty values that must parse as
#'   numeric for the column to be considered "mostly numeric"
#' @param n_max max number of non-numeric values to list
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_numeric_in_text <- function(x, threshold = 0.8, n_max = 10) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 5) return(none)
  # Treat comma-decimals as numeric too (European formatting).
  num <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  frac_num <- mean(!is.na(num))
  # Mostly-but-not-fully numeric: contamination worth reporting.
  if (frac_num < threshold || frac_num >= 1) return(none)
  bad <- unique(x[is.na(num)])
  list(problem = TRUE,
       message = sprintf("Column is %.0f%% numeric but %d value%s cannot be parsed: %s",
                         100 * frac_num, length(bad), plural(length(bad)),
                         paste(utils::head(sprintf('"%s"', bad), n_max),
                               collapse = ", ")),
       values = bad)
}

# ── Personal / disclosure information ─────────────────────────────────────────
# These checks flag columns that may hold information that should not be shared
# openly (personally identifiable information, PII). They are intentionally
# conservative — a hit is a "review this before sharing" prompt, not proof of a
# violation. The value regexes are standard patterns (vendored so metacheck
# takes no dependency); a matched pattern is reported, never the matching value
# itself, so the report does not itself leak the PII.

# Standard value patterns. Following the approach used by mature detectors
# (e.g. Microsoft Presidio), each pattern is classed by how specific it is:
#
#   "specific" patterns (email, IP, SSN, credit card) are distinctive enough
#   that a SINGLE valid match warrants a "review before sharing" flag — for
#   disclosure, one real email leaking is already a problem, so requiring a
#   fraction of the column would be a dangerous false negative;
#
#   "broad" patterns would collide with ordinary data (dates, codes, long
#   integers), so they would require a FRACTION of the column plus a validation
#   step. No broad pattern is currently enabled (the former phone pattern was
#   removed for false-positives on timestamps).
#
# A raw regex match is necessary but not sufficient: patterns with a validator
# (credit card -> Luhn) must also pass it, which keeps ordinary numbers from
# tripping the flag.
.pii_value_patterns <- list(
  email = list(
    regex = "(?i)\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b",
    kind  = "specific"),
  # IPv4 with each octet 0-255.
  ip_address = list(
    regex = "\\b(?:(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)\\.){3}(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)\\b",
    kind  = "specific"),
  # US SSN: 3-2-4 with separators, excluding obvious non-SSN (000/666/9xx area,
  # 00 group, 0000 serial).
  ssn = list(
    regex = "\\b(?!000|666|9\\d\\d)\\d{3}-(?!00)\\d{2}-(?!0000)\\d{4}\\b",
    kind  = "specific"),
  # Credit-card-like: 13-16 digits in even 4-digit groups (space/dash) or one
  # unbroken run, not part of a longer digit/decimal string. Must also pass a
  # Luhn checksum, which rejects the vast majority of coincidental digit runs.
  credit_card = list(
    regex = "(?<![\\d.])(?:\\d{13,16}|\\d{4}[ -]\\d{4}[ -]\\d{4}[ -]\\d{1,4})(?![\\d.])",
    kind  = "specific", validate = ".pii_luhn_ok")
  # NOTE: a phone pattern was removed. It was "broad" and collided heavily with
  # date/time strings (e.g. Qualtrics StartDate/EndDate timestamps), producing
  # false positives on essentially every survey export, and modern studies
  # rarely collect phone numbers. The remaining value patterns are all
  # "specific" with validators, so a hit is almost always a real identifier.
)

# Luhn checksum: the check most card issuers use. Rejects most random 13-16
# digit runs, so a plausible credit card must both match the shape and validate.
.pii_luhn_ok <- function(s) {
  d <- as.integer(strsplit(gsub("[^0-9]", "", s), "")[[1]])
  n <- length(d)
  if (n < 13 || n > 16) return(FALSE)
  d <- rev(d)
  d[seq(2, n, by = 2)] <- d[seq(2, n, by = 2)] * 2
  d[d > 9] <- d[d > 9] - 9
  sum(d) %% 10 == 0
}


# Column-name tokens that suggest the column identifies a person, even when the
# values look innocuous. Matched case-insensitively against normalised names.
# NOTE: the bare "name" token was removed — it is a sub-string of many
# non-personal column names (experimentName, trial_name, fileName, videoName,
# conditionName, variable name, ...) and produced mostly false positives. The
# specific person-name compounds below (firstname/lastname/surname/fullname)
# are retained because they reliably indicate a real person's name.
.pii_name_tokens <- c(
  "firstname", "lastname", "surname", "fullname",
  "email", "e-mail", "phone", "mobile", "telephone", "fax",
  "address", "street", "zipcode", "zip", "postcode", "postalcode",
  "ssn", "socialsecurity", "passport", "nationalid", "taxid",
  "dob", "dateofbirth", "birthdate", "birthday",
  "ipaddress", "ip", "mac", "creditcard", "iban", "bankaccount",
  "latitude", "longitude", "lat", "lon", "lng", "geolocation", "gps",
  "username", "userid", "handle", "initials"
)

#' Flag values that match a personal-information pattern
#'
#' Scans a column's values for standard PII patterns (email, IP address, SSN,
#' credit-card-like). Reports which pattern matched and how many values, never
#' the matching values themselves (so the report does not leak the PII).
#'
#' All current patterns are *specific*: they flag on a single validated match,
#' because for disclosure one real identifier is already a problem. A raw regex
#' match is necessary but not sufficient — the credit-card pattern must also pass
#' a Luhn checksum, which keeps ordinary numbers from tripping the flag. (A
#' broad-pattern path with a per-column fraction threshold, `broad_min_frac`, is
#' retained for future patterns but is currently unused.)
#'
#' @param x a vector (coerced to character)
#' @param broad_min_frac for broad patterns, the minimum fraction of non-empty
#'   values that must match for the column to be flagged (currently unused)
#' @returns list(problem, message, values) — `values` is the matched pattern
#'   name(s), not the data
#' @export
#' @keywords internal
data_check_pii_values <- function(x, broad_min_frac = 0.30) {
  none <- list(problem = FALSE, message = "", values = NULL)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 3) return(none)

  hits <- character(0)
  for (nm in names(.pii_value_patterns)) {
    spec <- .pii_value_patterns[[nm]]
    m <- grepl(spec$regex, x, perl = TRUE)
    matched <- x[m]
    if (length(matched) == 0) next

    # Validate the matched values, when the pattern has a validator.
    if (!is.null(spec$validate)) {
      vfun <- get(spec$validate, mode = "function")
      matched <- matched[vapply(matched, vfun, logical(1))]
      if (length(matched) == 0) next
    }
    n_valid <- length(matched)
    frac <- n_valid / length(x)

    # Specific patterns: a single validated match is enough (a leaked email is
    # already a disclosure). Broad patterns: require a fraction of the column.
    flag <- if (identical(spec$kind, "specific")) n_valid >= 1
            else frac >= broad_min_frac
    if (flag)
      hits <- c(hits, sprintf("%s (%d value%s, %.0f%%)", nm, n_valid,
                              plural(n_valid), 100 * frac))
  }
  if (length(hits) == 0) return(none)
  list(problem = TRUE,
       message = sprintf("Values look like personal information: %s. Review before sharing.",
                         paste(hits, collapse = "; ")),
       values = sub(" .*$", "", hits))
}

#' Flag a column whose name suggests personal information
#'
#' Matches the (normalised) column name against tokens that typically identify a
#' person (name, email, address, dob, ssn, ip, coordinates, ...). Complements
#' [data_check_pii_values()]: catches identifying columns whose values look
#' ordinary (e.g. a `participant_name` free-text field).
#'
#' @param col_name the column name
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_pii_name <- function(col_name) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.null(col_name) || is.na(col_name) || !nzchar(col_name)) return(none)
  norm <- gsub("[^a-z0-9]", "", tolower(col_name))
  if (!nzchar(norm)) return(none)
  # Match a token as a whole normalised name or a clear sub-token; short tokens
  # (ip, zip, lat, lon, dob) must be the whole name to avoid matching inside
  # ordinary words (e.g. "description" contains "ip").
  short <- nchar(.pii_name_tokens) <= 3
  exact <- .pii_name_tokens[short]
  sub   <- .pii_name_tokens[!short]
  hit <- norm %in% exact | any(vapply(sub, function(t) grepl(t, norm, fixed = TRUE),
                                      logical(1)))
  if (!isTRUE(hit)) return(none)
  matched <- c(exact[exact == norm],
               sub[vapply(sub, function(t) grepl(t, norm, fixed = TRUE), logical(1))])
  list(problem = TRUE,
       message = sprintf("Column name suggests personal information (matched: %s). Review before sharing.",
                         paste(unique(matched), collapse = ", ")),
       values = unique(matched))
}

#' Flag a numeric column that looks like a geographic coordinate
#'
#' Latitude/longitude columns can re-identify participants. Flags a numeric
#' column whose name looks like a coordinate, or whose values all fall in the
#' latitude (-90..90) or longitude (-180..180) range with decimal precision.
#'
#' @param col_name the column name
#' @param x the column values
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_pii_geo <- function(col_name, x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  norm <- gsub("[^a-z0-9]", "", tolower(col_name %||% ""))
  name_geo <- norm %in% c("lat", "latitude", "lon", "lng", "longitude",
                          "geolocation", "gps", "coord", "coordinate")
  # Value range alone cannot distinguish a coordinate from any other decimal
  # measurement (temperature, reaction time, ...), so a bare value match would
  # flag ordinary data. We therefore require the column NAME to look
  # geographic; the value range then only *confirms* it. A geographic name with
  # too few values to check still flags (the name is enough of a prompt).
  if (!name_geo) return(none)
  num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  num <- num[!is.na(num)]
  if (length(num) >= 3) {
    in_lat <- all(num >= -90  & num <= 90)
    in_lon <- all(num >= -180 & num <= 180)
    if (!in_lat && !in_lon)
      return(none)   # geographic name but values are out of coordinate range
  }
  list(problem = TRUE,
       message = "Column name suggests geographic coordinates. Review before sharing.",
       values = "geo")
}

#' Flag a free-text column that may contain incidental personal information
#'
#' Open-ended typed responses (comments, explanations, descriptions) can contain
#' names, places, or other identifying detail, so they warrant a "review before
#' sharing" prompt. The aim is to flag genuine typed prose only — not any long,
#' varied string. Long values that are *not* prose (numeric matrices with blank
#' headers, IDs, hashes, URLs, file paths, base64) are common in research data
#' and previously produced false positives, so a column is flagged only when its
#' typical value actually reads like written language:
#'
#' * long enough (`min_median_chars`),
#' * varied enough to be responses rather than a repeated category
#'   (`min_unique_frac`),
#' * **multi-word** — most values contain whitespace between words, and
#' * **predominantly alphabetic** — letters, not mostly digits/punctuation.
#'
#' @param x a character or factor vector
#' @param min_median_chars typical (median) length above which a column may be
#'   free text
#' @param min_unique_frac minimum fraction of distinct values (prose is rarely
#'   repeated; a coded category is)
#' @param min_multiword_frac minimum fraction of values that contain more than
#'   one word (a space between word characters)
#' @param min_alpha_frac minimum share of alphabetic characters in the typical
#'   value (screens out numeric/ID/hash/URL columns)
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_pii_freetext <- function(x, min_median_chars = 40,
                                    min_unique_frac = 0.8,
                                    min_multiword_frac = 0.6,
                                    min_alpha_frac = 0.5) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (is.numeric(x)) return(none)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 5) return(none)

  med <- stats::median(nchar(x))
  uniq_frac <- length(unique(x)) / length(x)
  if (med < min_median_chars || uniq_frac < min_unique_frac) return(none)

  # Real prose is multi-word: most values have a space between word characters.
  multiword_frac <- mean(grepl("\\w\\s+\\w", x))
  if (multiword_frac < min_multiword_frac) return(none)

  # And it is mostly letters, not digits/punctuation (rejects numeric matrices,
  # IDs, hashes, URLs, base64). Measured on the typical (median-length) value.
  typical <- x[order(abs(nchar(x) - med))][1]
  n_alpha <- nchar(gsub("[^A-Za-z]", "", typical))
  alpha_frac <- if (nchar(typical) > 0) n_alpha / nchar(typical) else 0
  if (alpha_frac < min_alpha_frac) return(none)

  list(problem = TRUE,
       message = sprintf("Free-text column (median %.0f characters, %.0f%% distinct) may contain names or other personal detail. Review before sharing.",
                         med, 100 * uniq_frac),
       values = NULL)
}
