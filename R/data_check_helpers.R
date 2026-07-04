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

# Write a per-paper file manifest (JSON) recording every repository file and
# whether it was downloaded — the provenance needed to audit a corpus or rebuild
# a data archive without re-querying every repo. `files` is data_check's finalised
# `all_files`; `want` is the logical vector of files this run tried to download;
# `gated` is the download gate table (repos refused by the size caps).
#
# Sizes are completed here: a downloaded file's real size comes from disk, and a
# wanted file the listing left unsized (OSF returns NA for some files, often the
# large ones) is resolved with a cheap HEAD probe — so the manifest carries a
# real size for choosing the archive's size ceiling. Only NA-sized wanted files
# are probed, and only when a manifest is requested, so normal runs pay nothing.
.data_check_write_manifest <- function(manifest, files, want, gated,
                                       paper_id, download,
                                       max_file_size, max_download_size) {
  # Resolve the output path: a directory → "<paper_id>.manifest.json" inside it;
  # a ".json" path is used verbatim.
  path <- manifest
  if (!grepl("\\.json$", path, ignore.case = TRUE)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    pid <- if (length(paper_id) && !is.na(paper_id[[1]])) paper_id[[1]] else "manifest"
    path <- file.path(path, paste0(pid, ".manifest.json"))
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  n <- nrow(files)
  loc <- files$file_location %||% rep(NA_character_, n)
  downloaded <- !is.na(loc) & nzchar(loc) & file.exists(loc %||% "")
  gated_urls <- if (!is.null(gated) && nrow(gated) > 0) gated$repo_url else character(0)

  # Complete the sizes. A downloaded file's real size is on disk. For a wanted
  # file the listing left unsized (OSF returns NA for some files — exactly the
  # large ones), resolve it with a cheap HEAD probe so the manifest carries a
  # real size for ceiling planning. This runs only when a manifest is requested
  # (opt-in) and only for the NA-sized wanted files, so normal runs pay nothing.
  file_size <- as.numeric(files$file_size)
  on_disk_size <- ifelse(downloaded, suppressWarnings(file.size(loc)), NA_real_)
  file_size <- ifelse(!is.na(on_disk_size), on_disk_size, file_size)

  url <- files$file_url %||% rep(NA_character_, n)
  probe <- which(is.na(file_size) & (want %in% TRUE) &
                   !is.na(url) & nzchar(url) & !downloaded)
  if (length(probe) > 0) {
    pb_probe <- pb(length(probe),
                   "Sizing files (HEAD) [:bar] :current/:total")
    on.exit(pb_probe$terminate(), add = TRUE)
    for (i in probe) {
      file_size[i] <- .remote_size(url[i])
      pb_probe$tick()
    }
  }

  # Why was a file not downloaded? Ordered from most to least specific.
  reason <- vapply(seq_len(n), function(i) {
    if (downloaded[i]) return(NA_character_)
    url <- files$file_url[i] %||% NA_character_
    if (identical(download, "none")) return("download = \"none\"")
    if (!isTRUE(want[i]))
      return("not a data/codebook/README file (use download = \"all\")")
    if (is.na(url) || !nzchar(url)) return("no download URL in the listing")
    if (files$repo_url[i] %in% gated_urls)
      return("repository refused by the size caps")
    "download failed"
  }, character(1))

  entries <- lapply(seq_len(n), function(i) {
    Filter(Negate(is.null), list(
      file_name    = files$file_name[i],
      file_path    = files$file_path[i] %||% files$file_name[i],
      repo_url     = files$repo_url[i],
      file_url     = files$file_url[i] %||% NA_character_,
      file_size    = if (!is.na(file_size[i])) file_size[i] else NULL,
      data_type    = files$data_type[i] %||% NA_character_,
      data_format  = files$data_format[i] %||% NA_character_,
      downloaded   = downloaded[i],
      skip_reason  = if (downloaded[i]) NULL else reason[i]
    ))
  })

  doc <- list(
    paper_id  = if (length(paper_id)) paper_id[[1]] else NA_character_,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    download  = download,
    caps      = list(max_file_size_mb = max_file_size,
                     max_download_size_mb = max_download_size),
    n_files      = n,
    n_downloaded = sum(downloaded),
    files        = entries
  )

  json <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

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
                                  model = llm_model(), params = list(),
                                  phase = NULL) {
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
          params = params, phase = phase),
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
          params = params, phase = "Assigning study groups"),
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
        # Qualtrics "use choice text" exports carry extra header rows (question
        # text, ImportId JSON) as the first data rows, which force every column
        # to character. Strip them and re-type so the rest of data_check works.
        if (!is.null(df) && data_check_is_qualtrics(df))
          df <- data_strip_qualtrics_header(df)
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
        df <- suppressWarnings(as.data.frame(readxl::read_excel(path, n_max = nmax)))
        if (!is.null(df) && data_check_is_qualtrics(df))
          df <- data_strip_qualtrics_header(df)
        df
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
        # An .RData/.rda workspace can hold arbitrary objects — fitted models,
        # session state — not just data frames. Restoring a model that
        # references an uninstalled package (e.g. robustlmm, effects) makes
        # load() print namespace/restore diagnostics at the C level (not
        # suppressible from R) and can crash. We read it in an isolated
        # subprocess (.read_rdata_isolated), which returns the first data frame
        # or NULL, plus a "reusability" verdict for data_check's reporting.
        .read_rdata_isolated(path, n_rows)
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

# Read an .RData/.rda workspace in an ISOLATED subprocess and return its first
# data frame (head of `n_rows`), or NULL. Isolation is essential: restoring
# model/session objects that reference uninstalled packages prints C-level
# diagnostics and can crash the process — none of which must reach the caller.
# A NULL return means the workspace holds no reusable tabular data (only models
# / session objects, or it could not be restored at all); data_check turns that
# into a sharing recommendation.
.read_rdata_isolated <- function(path, n_rows = 5) {
  out_rds <- tempfile(fileext = ".rds")
  on.exit(unlink(out_rds), add = TRUE)
  nmax <- if (is.finite(n_rows)) n_rows else Inf

  # The child loads the workspace with its message stream sunk to null, then
  # writes the first data frame (or NULL) to out_rds. It never errors to the
  # parent, so a model-heavy or broken workspace cannot make noise or crash.
  script <- sprintf(paste(
    "con <- file(nullfile(), open='wt'); sink(con, type='message')",
    "e <- new.env()",
    "ok <- tryCatch({ load(%s, envir = e); TRUE }, error = function(x) FALSE)",
    "sink(type='message'); close(con)",
    "df <- NULL",
    "if (ok) { dfs <- Filter(is.data.frame, as.list(e))",
    "  if (length(dfs) > 0) { d <- as.data.frame(dfs[[1]]); n <- %s",
    "    df <- if (is.finite(n)) utils::head(d, n) else d } }",
    "saveRDS(df, %s)",
    sep = "\n"),
    deparse(path), if (is.finite(nmax)) nmax else "Inf", deparse(out_rds))

  tryCatch(
    processx::run(rscript_path(), args = c("-e", script),
                  error_on_status = FALSE, timeout = 60),
    error = function(e) NULL)

  if (!file.exists(out_rds)) return(NULL)
  tryCatch(readRDS(out_rds), error = function(e) NULL)
}

# Path to the Rscript executable of the current R installation.
rscript_path <- function() {
  file.path(R.home("bin"),
            if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
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

# Empty codebook-variable table (the canonical column set). The DDI-derived
# per-variable properties (value_labels, missing_values, question, universe) are
# carried as extra columns; they default to NA and only populate when a source
# supplies them.
.empty_codebook_vars <- function() {
  data.frame(
    codebook_variable = character(0), label = character(0),
    codebook_source = character(0), group = character(0),
    value_labels = character(0), missing_values = character(0),
    question = character(0), universe = character(0),
    parse_method = character(0)
  )
}

# ── Value labels / code lists + missing-value scheme (DDI ValueDomain) ─────────
# A categorical variable's meaning lives in its code list — the mapping
# 1="Strongly disagree" ... 5="Strongly agree" — and in which codes denote
# missingness (-99="refused"). DDI models these as CodeList / ValueDomain and
# MissingValues. We serialise a code list as a compact JSON object keyed by code
# ("{\"1\":\"Male\",\"2\":\"Female\"}") so it survives as a single data.frame
# column and round-trips through the label-matching machinery unchanged.

# Encode a named code->label mapping as a JSON string. `codes` are the values,
# `labels` the human labels (same length). Returns NA when empty.
.encode_value_labels <- function(codes, labels) {
  keep <- !is.na(codes) & !is.na(labels) & nzchar(trimws(as.character(labels)))
  if (!any(keep)) return(NA_character_)
  obj <- as.list(as.character(labels[keep]))
  names(obj) <- as.character(codes[keep])
  tryCatch(as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)),
           error = function(e) NA_character_)
}

# Decode a value-labels JSON string back to a named character vector
# (names = codes, values = labels). Returns NULL on failure / NA.
.decode_value_labels <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
  if (is.null(out) || length(out) == 0) return(NULL)
  v <- unlist(out); v[!is.na(v)]
}

# Encode a set of missing-value codes (optionally with reasons) as JSON. `codes`
# is a vector of the sentinel codes; `reasons` an optional same-length vector of
# labels ("refused", "not applicable"). Returns NA when empty.
.encode_missing_values <- function(codes, reasons = NULL) {
  codes <- codes[!is.na(codes)]
  if (length(codes) == 0) return(NA_character_)
  if (is.null(reasons)) {
    tryCatch(as.character(jsonlite::toJSON(as.character(codes))),
             error = function(e) NA_character_)
  } else {
    .encode_value_labels(codes, reasons)
  }
}

# Extract value labels + declared missing values from one haven column. Returns
# list(value_labels = <json|NA>, missing_values = <json|NA>). haven puts the
# code list in attr(,"labels") and SPSS-declared missings in attr(,"na_values")
# / attr(,"na_range"); a labelled code whose label names it missing (e.g.
# "Refused", "N/A") is also treated as a missing code.
.haven_value_labels <- function(col) {
  labs <- attr(col, "labels")
  na_values <- attr(col, "na_values")
  na_range  <- attr(col, "na_range")
  vl <- NA_character_
  miss_codes <- numeric(0); miss_reasons <- character(0)

  if (!is.null(labs) && length(labs) > 0) {
    codes  <- unname(labs)
    reasons <- names(labs)
    vl <- .encode_value_labels(codes, reasons)
    # Labels that read as missingness → sentinel missing codes.
    is_miss <- grepl("(?i)(missing|refus|declined|no answer|not applicable|n/?a|prefer not|don'?t know|unknown|skipped)",
                     reasons, perl = TRUE)
    if (any(is_miss)) {
      miss_codes  <- c(miss_codes, codes[is_miss])
      miss_reasons <- c(miss_reasons, reasons[is_miss])
    }
  }
  if (!is.null(na_values)) {
    miss_codes  <- c(miss_codes, na_values)
    miss_reasons <- c(miss_reasons, rep(NA_character_, length(na_values)))
  }
  if (!is.null(na_range) && length(na_range) == 2 && all(is.finite(na_range))) {
    # A declared missing RANGE: record its endpoints as a compact note.
    miss_codes  <- c(miss_codes, na_range)
    miss_reasons <- c(miss_reasons, rep("range", 2))
  }
  mv <- if (length(miss_codes) > 0) {
    keep <- !duplicated(miss_codes)
    r <- miss_reasons[keep]
    if (all(is.na(r))) .encode_missing_values(miss_codes[keep])
    else .encode_value_labels(miss_codes[keep], r)
  } else NA_character_

  list(value_labels = vl %||% NA_character_, missing_values = mv %||% NA_character_)
}

# Parse a codebook "values" cell into value labels. Handles the common textual
# encodings authors use: "1 = Male; 2 = Female", "1=Male, 2=Female",
# "0: no | 1: yes", newline-separated. Returns a value-labels JSON string or NA.
.parse_value_label_text <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(trimws(s))) return(NA_character_)
  s <- as.character(s)
  # Split into entries on ; | newline (comma too, but only when not inside a
  # decimal — handled by requiring a code=label shape per entry).
  parts <- unlist(strsplit(s, "\\s*[;|\\n]\\s*|\\s*,\\s*(?=\\s*-?\\d+(\\.\\d+)?\\s*[:=])", perl = TRUE))
  parts <- trimws(parts[nzchar(trimws(parts))])
  if (length(parts) == 0) return(NA_character_)
  codes <- character(0); labels <- character(0)
  for (p in parts) {
    m <- regmatches(p, regexec("^\\s*(-?\\d+(?:\\.\\d+)?)\\s*[:=]\\s*(.+?)\\s*$", p, perl = TRUE))[[1]]
    if (length(m) == 3) { codes <- c(codes, m[2]); labels <- c(labels, m[3]) }
  }
  if (length(codes) < 2) return(NA_character_)   # need a real mapping, not one pair
  .encode_value_labels(codes, labels)
}

# From a value-labels JSON string, derive the missing-value scheme: the codes
# whose label reads as missingness ("refused", "n/a", "prefer not to answer").
# Returns a missing-values JSON string or NA. Used so a code list from a text
# codebook contributes to the missing scheme, matching the haven path.
.missing_from_value_labels <- function(vl_json) {
  vl <- .decode_value_labels(vl_json)
  if (is.null(vl) || length(vl) == 0) return(NA_character_)
  is_miss <- grepl("(?i)(missing|refus|declined|no answer|not applicable|n/?a|prefer not|don'?t know|unknown|skipped)",
                   unname(vl), perl = TRUE)
  if (!any(is_miss)) return(NA_character_)
  .encode_value_labels(names(vl)[is_miss], unname(vl)[is_miss])
}

# Find a "value labels" / "coding" column in a structured codebook's headers.
.find_value_label_col <- function(col_names) {
  grep(paste0("(?i)^(value[_ ]?labels?|values?|codes?|coding|categor(y|ies)|",
              "response[_ ]?options?|levels?|value[_ ]?meanings?)$"),
       col_names, perl = TRUE, value = TRUE)[1]
}

# Find a "question text" column and a "universe"/"filter" column in a codebook.
.find_question_col <- function(col_names) {
  grep("(?i)^(question|question[_ ]?text|item[_ ]?text|prompt|wording|item[_ ]?wording|survey[_ ]?question)$",
       col_names, perl = TRUE, value = TRUE)[1]
}
.find_universe_col <- function(col_names) {
  grep("(?i)^(universe|population|applies[_ ]?to|filter|skip[_ ]?logic|asked[_ ]?of|base|subset|condition[_ ]?asked)$",
       col_names, perl = TRUE, value = TRUE)[1]
}

# Extract variable-label pairs from a structured data.frame (CSV/Excel rows).
# Returns NULL when no matching header columns are found.
.extract_structured_codebook <- function(df, src) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) return(NULL)
  cols <- .find_codebook_cols(names(df))
  if (is.null(cols)) return(NULL)
  rows <- df[nzchar(trimws(as.character(df[[cols$var_col]]))), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)

  # Optional DDI-derived columns: value labels / coding, question text,
  # universe/filter. Each is parsed per row when its column is present.
  val_col <- .find_value_label_col(names(df))
  q_col   <- .find_question_col(names(df))
  u_col   <- .find_universe_col(names(df))
  na_str  <- function(x) { x <- trimws(as.character(x)); ifelse(nzchar(x), x, NA_character_) }

  value_labels <- if (!is.na(val_col))
    vapply(as.character(rows[[val_col]]), .parse_value_label_text, character(1),
           USE.NAMES = FALSE) else rep(NA_character_, nrow(rows))
  # Missing scheme from any code whose label reads as missingness.
  missing_values <- vapply(value_labels, .missing_from_value_labels,
                           character(1), USE.NAMES = FALSE)

  data.frame(
    codebook_variable = as.character(rows[[cols$var_col]]),
    label             = as.character(rows[[cols$lab_col]]),
    codebook_source   = src,
    group             = NA_character_,
    value_labels      = value_labels,
    missing_values    = missing_values,
    question          = if (!is.na(q_col)) na_str(rows[[q_col]]) else NA_character_,
    universe          = if (!is.na(u_col)) na_str(rows[[u_col]]) else NA_character_
  )
}

# Extract embedded variable labels from a haven-read data.frame (SPSS/Stata/SAS).
# Returns NULL if no labelled columns found. Caller adds parse_method = "haven".
.extract_haven_labels <- function(df, src) {
  labels <- vapply(names(df), function(col) {
    lbl <- attr(df[[col]], "label")
    if (is.null(lbl)) NA_character_ else trimws(as.character(lbl[1]))
  }, character(1))
  # Value labels + declared missing values are useful even for columns without a
  # variable label, so harvest them for every column and keep any column that has
  # EITHER a label or a code list.
  vlmv <- lapply(names(df), function(col) .haven_value_labels(df[[col]]))
  value_labels   <- vapply(vlmv, function(x) x$value_labels %||% NA_character_, character(1))
  missing_values <- vapply(vlmv, function(x) x$missing_values %||% NA_character_, character(1))

  keep <- (!is.na(labels) & nzchar(labels)) | !is.na(value_labels)
  if (!any(keep)) return(NULL)
  data.frame(
    codebook_variable = names(df)[keep],
    label             = labels[keep],
    codebook_source   = src,
    group             = NA_character_,
    value_labels      = value_labels[keep],
    missing_values    = missing_values[keep],
    question          = NA_character_,
    universe          = NA_character_
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
      label_method      = NA_character_,
      value_labels      = NA_character_,
      missing_values    = NA_character_,
      question          = NA_character_,
      universe          = NA_character_
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
  # DDI-derived per-variable properties carried from the matched codebook rows.
  vl_out <- mv_out <- q_out <- univ_out <- rep(NA_character_, n)
  # First non-NA value of a codebook column across the applicable matches (used
  # to carry value_labels/missing_values/question/universe onto the data column).
  first_present <- function(rows, col)
    if (col %in% names(rows)) {
      v <- rows[[col]][!is.na(rows[[col]]) & nzchar(as.character(rows[[col]]))]
      if (length(v) > 0) as.character(v[1]) else NA_character_
    } else NA_character_

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

    # Carry the DDI-derived properties from the matched codebook rows onto the
    # data column (independent of which label won: a variable's code list /
    # question / universe are the same whichever source described it).
    vl_out[i]   <- first_present(applicable, "value_labels")
    mv_out[i]   <- first_present(applicable, "missing_values")
    q_out[i]    <- first_present(applicable, "question")
    univ_out[i] <- first_present(applicable, "universe")
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
    label_method      = label_method_out,
    value_labels      = vl_out,
    missing_values    = mv_out,
    question          = q_out,
    universe          = univ_out
  )
}

# ── Analysis unit (DDI analysisUnit) ──────────────────────────────────────────
# DDI records the unit of observation a data file describes: what one row IS — a
# person, a trial, a dyad, a session/time-point. It matters because it tells a
# reviewer whether rows are independent (one per participant) or nested (many
# trials per participant), which changes how the data should be analysed, and a
# repository that mixes person-level and trial-level files without saying so is a
# common source of confusion. We infer it from structure: which columns are
# identifiers (from the `role` facet or a name pattern), whether they are unique
# per row, and whether a trial/stimulus/session column is present.

# Column-name patterns for the units, used alongside the identifier role.
.analysis_unit_patterns <- list(
  trial   = "(?i)(^|[_.])(trial|item|stimulus|stim|trialnum|itemnum|trial_?id|item_?id|rt|response)([_.]|$)",
  session = "(?i)(^|[_.])(session|wave|timepoint|time_?point|visit|day|block|run|occasion|measurement)([_.]|$)",
  dyad    = "(?i)(^|[_.])(dyad|couple|partner|actor|target|pair|group_?id|team)([_.]|$)"
)

#' Infer the analysis unit (unit of observation) of a data file
#'
#' Rule-based inference of what one row of a data frame represents — `"person"`,
#' `"trial"`, `"session"`, `"dyad"`, or `NA` when unclear (DDI `analysisUnit`).
#' Uses the identifier column(s) and their uniqueness: a unique-per-row id with
#' no repeat structure is person-level (wide); a repeating id together with a
#' trial/stimulus column is trial-level (long); a repeating id with a
#' session/wave column is a repeated-measures session unit; two distinct id-like
#' columns suggest a dyad. Needs enough rows to judge repetition.
#'
#' @param df a data.frame (the read data file)
#' @param id_cols optional character vector of identifier column names (e.g. from
#'   data_check's `role == "identifier"`); when NULL, inferred by name pattern
#'
#' @returns a list with `unit` (one of the above or NA) and `reason` (a short
#'   human-readable explanation).
#' @export
#' @keywords internal
data_analysis_unit <- function(df, id_cols = NULL) {
  none <- list(unit = NA_character_, reason = "could not determine the unit of observation")
  if (is.null(df) || nrow(df) < 3 || ncol(df) == 0) return(none)
  nm <- names(df)

  # Identifier columns: caller-supplied (from the role facet) or by name.
  if (is.null(id_cols) || length(id_cols) == 0) {
    id_pat <- "(?i)(^id$|_id$|^id_|participant|subject|subj|respondent|^pp$|^ppt$|^pid$|prolific|mturk|worker)"
    id_cols <- nm[grepl(id_pat, nm, perl = TRUE)]
  }
  id_cols <- id_cols[id_cols %in% nm]

  has_col <- function(kind) any(grepl(.analysis_unit_patterns[[kind]], nm, perl = TRUE))
  trial_col   <- has_col("trial")
  session_col <- has_col("session")
  dyad_col    <- has_col("dyad")

  # Two or more distinct identifier columns → likely a dyad/relational unit.
  if (length(id_cols) >= 2 || dyad_col)
    return(list(unit = "dyad",
                reason = "two identifier columns (or a dyad/partner column) suggest a relational unit"))

  if (length(id_cols) == 1) {
    ids <- df[[id_cols[1]]]
    # An id column that is entirely NA gives 0/0 = NaN; treat it as non-unique
    # so the `>= 0.98` test below doesn't error on a missing value.
    n_ids <- sum(!is.na(ids))
    frac_unique <- if (n_ids == 0) 0 else length(unique(ids[!is.na(ids)])) / n_ids
    if (frac_unique >= 0.98)
      return(list(unit = "person",
                  reason = sprintf("the identifier '%s' is unique per row (one row per participant)", id_cols[1])))
    # Repeating id → nested/long. Distinguish trial vs session by the co-column.
    if (trial_col)
      return(list(unit = "trial",
                  reason = sprintf("the identifier '%s' repeats and a trial/item column is present (long format)", id_cols[1])))
    if (session_col)
      return(list(unit = "session",
                  reason = sprintf("the identifier '%s' repeats and a session/wave column is present (repeated measures)", id_cols[1])))
    return(list(unit = "trial",
                reason = sprintf("the identifier '%s' repeats across rows (multiple rows per participant)", id_cols[1])))
  }

  # No id column: fall back to the presence of a trial/session structure.
  if (trial_col)   return(list(unit = "trial",   reason = "a trial/item column is present but no participant identifier"))
  if (session_col) return(list(unit = "session", reason = "a session/wave column is present but no participant identifier"))
  none
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

# ── Demographic-column detection ──────────────────────────────────────────────
# Detect the three demographic variables that almost every human-subjects study
# collects: age, gender/sex, and race/ethnicity. Used by data_check (to tag the
# column) and data_validate (to report which demographics a file contains).
#
# Detection requires NAME and VALUES to agree: the column NAME must look like the
# demographic, AND the VALUES must be consistent with it. Name alone is too weak
# (a column literally called "age" that holds free text is not usable age data)
# and values alone are ambiguous (a 1/2 column is as likely a condition code as
# a sex code). Requiring both keeps false positives low — the aim is a column a
# reviewer can trust is really participant age / gender / race.

# Column-name tokens per demographic, matched against the normalised name
# (lowercase, punctuation stripped). Whole-name match OR a word-boundary token
# match, so `participant_age` and `age_years` hit but `page` / `agent` do not
# (handled by the boundary regex below, not these bare tokens).
.demographic_name_tokens <- list(
  age    = c("age", "agejaren", "ageyears", "ageyrs", "leeftijd", "alter"),
  gender = c("gender", "sex", "geslacht", "genderidentity", "sexgender",
             "gendersex"),
  race   = c("race", "ethnicity", "ethnic", "raceethnicity", "ethnicgroup",
             "raceeth", "hispanic", "raza", "etnia")
)

# Anchored name regexes: the token must be the whole name or a standalone word
# within it (separated by _ . - space or a case boundary), so it does not fire
# inside unrelated words. Built once from the token lists above.
.demographic_name_regex <- list(
  # age: exclude common false friends where "age" is a substring (percentage,
  # image, page, average, agent, storage, damage, usage, coverage, language).
  age    = "(?i)(^|[^a-z])(age|leeftijd|alter)([^a-z]|$)|(?i)age[_.-]?(years|yrs|jaren)|(?i)(years|yrs)[_.-]?age",
  gender = "(?i)(^|[^a-z])(gender|sex|geslacht)([^a-z]|$)",
  race   = "(?i)(^|[^a-z])(race|ethnicity|ethnic|hispanic|raza|etnia)([^a-z]|$)"
)

# Do a column's VALUES look like this demographic? Conservative value checks
# that CONFIRM a name match; they are not used to detect on their own.
.demographic_values_ok <- function(kind, x) {
  x_chr <- trimws(as.character(x))
  x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
  if (length(x_chr) < 3) return(TRUE)   # too few values to contradict the name

  if (kind == "age") {
    # Numeric (allow comma decimals) and almost all within a human-age range.
    num <- suppressWarnings(as.numeric(gsub(",", ".", x_chr, fixed = TRUE)))
    frac_num <- mean(!is.na(num))
    if (frac_num < 0.8) return(FALSE)
    v <- num[!is.na(num)]
    # Drop common missing-data sentinels before the range test so a genuine age
    # column carrying a -99 / 999 code is not rejected by that single value.
    v <- v[!v %in% .data_missing_sentinels]
    if (length(v) == 0) return(FALSE)
    # Ages are 0-120; allow a small tail of remaining miscodes.
    mean(v >= 0 & v <= 120) >= 0.9
  } else if (kind == "gender") {
    # Either a small set of textual categories that read as sex/gender, or a
    # low-cardinality numeric coding (1/2, 0/1/2, ...).
    u <- unique(tolower(x_chr))
    gender_words <- c("m", "f", "male", "female", "man", "woman", "men",
                      "women", "boy", "girl", "nonbinary", "non-binary",
                      "nb", "other", "trans", "transgender", "genderqueer",
                      "prefer not to say", "prefernottosay", "pnts", "n/a",
                      "unknown", "d", "diverse", "man/vrouw", "vrouw", "man",
                      "intersex", "agender", "fluid", "questioning")
    hit_frac <- mean(u %in% gender_words)
    is_lowcard_numeric <- {
      num <- suppressWarnings(as.numeric(x_chr))
      all(!is.na(num)) && length(unique(num)) <= 4 &&
        all(num == round(num)) && all(num >= 0 & num <= 9)
    }
    hit_frac >= 0.6 || is_lowcard_numeric
  } else if (kind == "race") {
    # Race/ethnicity is categorical with a modest number of levels; if numeric,
    # a low-cardinality coding. Reject long free text and high-cardinality.
    x2 <- x_chr
    if (length(unique(x2)) > 30) return(FALSE)
    med_chars <- stats::median(nchar(x2))
    if (med_chars > 60) return(FALSE)   # long prose is not a race category
    num <- suppressWarnings(as.numeric(x2))
    if (all(!is.na(num)))
      return(length(unique(num)) <= 25 && all(num == round(num)))
    TRUE
  } else {
    FALSE
  }
}

#' Detect whether a column holds participant age, gender/sex, or race/ethnicity
#'
#' A content-based classifier for the three demographic variables collected by
#' almost every human-subjects study. A column is tagged only when its NAME
#' looks like the demographic AND its VALUES are consistent with it (see
#' [.demographic_values_ok]), which keeps false positives low: a `condition`
#' column coded 1/2 is not flagged as gender, and an `age` column of free text
#' is not treated as usable age data.
#'
#' Complements [data_col_type()] (which gives a structural type such as
#' continuous/categorical): this adds a *semantic* label used by `data_check`
#' (reported in the column table) and `data_validate` (which reports the
#' demographics a file contains). Detection is name-driven, so a demographic
#' under a cryptic name (e.g. `q3`) is intentionally not caught here — that is
#' the LLM classifier's job.
#'
#' @param col_name the column's name
#' @param x the column's values
#'
#' @returns `"age"`, `"gender"`, or `"race"` when the column matches one of
#'   them, else `NA_character_`.
#' @export
#' @keywords internal
#'
#' @examples
#' data_check_demographic("age", c(23, 45, 31, 29))
#' data_check_demographic("gender", c("Male", "Female", "Female", "Male"))
#' data_check_demographic("condition", c(1, 2, 1, 2))   # NA (name does not match)
data_check_demographic <- function(col_name, x) {
  if (is.null(col_name) || length(col_name) != 1 || is.na(col_name) ||
      !nzchar(col_name)) return(NA_character_)
  # Guard against a non-UTF-8 name reaching the perl regexes below.
  if (is.na(iconv(col_name, "UTF-8", "UTF-8")))
    col_name <- iconv(col_name, "latin1", "UTF-8", sub = "")

  for (kind in names(.demographic_name_regex)) {
    if (grepl(.demographic_name_regex[[kind]], col_name, perl = TRUE) &&
        .demographic_values_ok(kind, x))
      return(kind)
  }
  NA_character_
}

# ── Column facets (orthogonal properties, DDI-style) ──────────────────────────
# A data column has several INDEPENDENT properties, and collapsing them into one
# `col_type` enum (the old model) conflated things that are not alternatives:
# how the value is stored, what measurement level it is on, and what it actually
# measures. Following DDI (which separates RepresentedVariable representation,
# @classificationLevel, the Variable→Concept link, VariableRole and UnitType) we
# describe each column with orthogonal facets instead:
#
#   representation     numeric | text | datetime | code | empty
#                      (how the value is stored/represented)
#   measurement_level  nominal | ordinal | interval | ratio | NA
#                      (Stevens level; DDI @classificationLevel)
#   concept            reaction_time | accuracy | age | gender | race | likert |
#                      condition | id | date | timestamp | NA
#                      (what the column measures; DDI Variable→Concept)
#   role               identifier | measure | condition | timestamp | measure
#                      (how it functions in the dataset; DDI VariableRole)
#   unit               seconds | milliseconds | years | NA (DDI UnitType)
#   quality            ok | empty | constant | near_constant (data state)
#   parse_note         NA | comma_decimal | mostly_numeric
#                      (a representation quirk, NOT a type — was a fake col_type)
#
# `data_col_facets()` derives these from the existing rule primitive
# `data_col_type()` (kept internal so its battle-tested edge cases — UTF-8
# guard, date threshold, comma-decimal, text-length — are preserved) plus the
# concept detectors below. Rules run always; the LLM (in data_check) only fills
# facets the rules left NA.

# Concept detector: name+value agreement, same discipline as the demographic
# detector. Returns a concept code or NA. Order matters — the first match wins,
# so specific concepts (reaction_time) are tried before generic ones.

# Reaction/response time: a numeric column named rt/latency/response time whose
# values are plausible durations. We do not fix the unit here (ms vs s); that is
# the `unit` facet, inferred separately.
.concept_is_rt <- function(col_name, x) {
  nm <- .qualtrics_key(col_name)   # lowercase, alnum-only
  # Require an explicit RT-ish name token so we do not match every "time" column
  # (a clock timestamp is a different concept). `.qualtrics_key` has stripped
  # separators, so match tokens rather than word boundaries.
  name_ok <- grepl("(^rt$|^rts$|reactiontime|responsetime|responselatency|latency|rtms|rtsec|^rt|rt$)", nm)
  if (!name_ok) return(FALSE)
  num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
  num <- num[!is.na(num)]
  if (length(num) < 3) return(TRUE)          # name is strong enough on its own
  # Durations are non-negative; a column with many negatives is not an RT.
  mean(num >= 0) >= 0.95
}

# Accuracy/correctness: a 0/1 (or boolean, or correct/incorrect) column named
# acc/correct/hit/error.
.concept_is_accuracy <- function(col_name, x) {
  nm <- .qualtrics_key(col_name)
  if (!grepl("(^acc$|accuracy|iscorrect|correct|incorrect|^hit$|iserror|^error$|errorrate)", nm))
    return(FALSE)
  v <- tolower(trimws(as.character(x)))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) < 3) return(TRUE)
  u <- unique(v)
  num <- suppressWarnings(as.numeric(u))
  is01 <- all(!is.na(num)) && all(num %in% c(0, 1))
  is_bool <- all(u %in% c("true", "false", "correct", "incorrect", "hit",
                          "miss", "yes", "no", "right", "wrong"))
  is01 || is_bool
}

# Condition/group assignment: a low-cardinality column named condition/group/
# treatment/arm/cond. Kept deliberately name-driven (values look like any other
# categorical), so it never steals a genuine gender/accuracy column.
.concept_is_condition <- function(col_name, x) {
  nm <- .qualtrics_key(col_name)
  grepl("(^cond$|condition|^group$|treatment|^arm$|manipulation|between|within)", nm)
}

# Timestamp (a clock time / datetime the event happened) vs a plain date. Both
# have representation `datetime`; the concept distinguishes a full timestamp
# (has a time component) from a calendar date.
.concept_is_timestamp <- function(col_name, x) {
  nm <- .qualtrics_key(col_name)
  name_ok <- grepl("(time|timestamp|datetime|onset|startdate|enddate|recordeddate)", nm)
  if (!name_ok) return(FALSE)
  v <- as.character(x)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0) return(FALSE)
  # A time component (HH:MM) present in most values → timestamp, not bare date.
  mean(grepl("\\d{1,2}:\\d{2}", v)) >= 0.5
}

#' Detect the substantive concept a column measures
#'
#' A content classifier for the *concept* facet (what the column measures),
#' independent of how it is stored or its measurement level. Uses name+value
#' agreement like [data_check_demographic()], which it wraps for the demographic
#' concepts. Rules-only and deterministic; concepts under cryptic names are left
#' `NA` for the LLM tier in `data_check` to fill.
#'
#' @param col_name the column's name
#' @param x the column's values
#'
#' @returns one of `"reaction_time"`, `"accuracy"`, `"condition"`, `"age"`,
#'   `"gender"`, `"race"`, `"timestamp"`, or `NA_character_`. (`id`, `date` and
#'   `likert` concepts are assigned by [data_col_facets()] from the role /
#'   representation / measurement level, not here.)
#' @export
#' @keywords internal
data_col_concept <- function(col_name, x) {
  if (is.null(col_name) || length(col_name) != 1 || is.na(col_name) ||
      !nzchar(col_name)) return(NA_character_)
  if (is.na(iconv(col_name, "UTF-8", "UTF-8")))
    col_name <- iconv(col_name, "latin1", "UTF-8", sub = "")

  if (.concept_is_rt(col_name, x))        return("reaction_time")
  if (.concept_is_accuracy(col_name, x))  return("accuracy")
  demo <- data_check_demographic(col_name, x)
  if (!is.na(demo))                       return(demo)
  if (.concept_is_timestamp(col_name, x)) return("timestamp")
  if (.concept_is_condition(col_name, x)) return("condition")
  NA_character_
}

# Map the old rule primitive's col_type onto a (representation, level) pair.
# This is where the conflated enum is untangled into two orthogonal facets.
.coltype_to_facets <- function(ct, is_numeric_hint) {
  switch(ct %||% "unknown",
    empty       = c(rep = "empty",    lvl = NA_character_),
    constant    = c(rep = NA_character_, lvl = NA_character_),  # rep unknown w/o values
    binary      = c(rep = NA_character_, lvl = "nominal"),
    date        = c(rep = "datetime", lvl = NA_character_),
    text        = c(rep = "text",     lvl = NA_character_),
    id          = c(rep = "text",     lvl = "nominal"),
    continuous                    = c(rep = "numeric", lvl = "ratio"),
    continuous_comma_decimal      = c(rep = "numeric", lvl = "ratio"),
    continuous_outliers_excluded  = c(rep = "numeric", lvl = "ratio"),
    c(rep = NA_character_, lvl = NA_character_)
  )
}

#' Describe a data column as orthogonal facets (DDI-style)
#'
#' Replaces the single `col_type` enum with independent properties, so the
#' numeric character of a column (how it is stored, its measurement level) is
#' kept separate from what it measures (its concept) and how it functions (its
#' role). See the facet vocabulary in the "Column facets" section of this file.
#'
#' Derives representation, measurement level, role, quality and a parse note from
#' the rule primitive [data_col_type()] (preserving its edge cases), and the
#' concept from [data_col_concept()]. The `likert` concept is inferred here from
#' an ordinal integer measurement level; `id`/`date` concepts from the role /
#' representation. `unit` is left `NA` for concepts whose unit is not implied
#' (an LLM/codebook can fill it); `reaction_time` seeds `seconds`/`milliseconds`
#' from the value magnitude.
#'
#' @param col_name the column's name
#' @param values the column's values
#'
#' @returns a list with `representation`, `measurement_level`, `concept`,
#'   `role`, `unit`, `quality`, `parse_note`, plus the numeric helpers carried
#'   over from [data_col_type()] (`numeric_values`, `n_coerced`, `is_numeric`,
#'   `ambiguous`) so `data_check` can compute statistics and target the LLM.
#' @export
#' @keywords internal
#'
#' @examples
#' data_col_facets("RT", c(543, 612, 498, 701))
#' data_col_facets("subject_id", c("s01", "s02", "s03"))
data_col_facets <- function(col_name, values) {
  prim <- data_col_type(col_name, values)      # the rule primitive
  ct   <- prim$col_type
  x_noNA <- values[!is.na(values)]
  n_noNA <- length(x_noNA)
  n_unique <- length(unique(x_noNA))

  # representation + measurement_level from the (untangled) col_type.
  f <- .coltype_to_facets(ct, prim$is_numeric)
  representation <- unname(f["rep"])
  measurement_level <- unname(f["lvl"])

  # A constant/binary column's representation is decided by its actual storage.
  if (is.na(representation) && n_noNA > 0) {
    num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA),
                                            fixed = TRUE)))
    representation <- if (mean(!is.na(num)) >= 0.8) "numeric" else "text"
  }

  # quality: constant/empty are a data STATE, not a type. (near_constant is a
  # data_validate finding; here we surface the exact-constant/empty case.)
  quality <- if (identical(ct, "empty") || n_noNA == 0) "empty"
             else if (identical(ct, "constant") || n_unique == 1) "constant"
             else "ok"

  # concept (rules); fall through to structural concepts.
  concept <- data_col_concept(col_name, values)

  # role: an id column is an identifier; a timestamp/date column is temporal;
  # everything else defaults to a measure. condition concept → condition role.
  role <- if (identical(ct, "id")) "identifier"
          else if (identical(concept, "timestamp") || identical(ct, "date")) "timestamp"
          else if (identical(concept, "condition")) "condition"
          else "measure"

  # Structural concepts that follow from other facets rather than name+value:
  if (is.na(concept)) {
    if (identical(role, "identifier")) concept <- "id"
    else if (identical(ct, "date"))    concept <- "date"
    else if (identical(representation, "datetime")) concept <- "timestamp"
  }

  # Likert: an ordinal-looking integer column (the rules mark these "ambiguous"
  # integers with 3–20 unique values). Only claim it when nothing more specific
  # was found, and set the ordinal level.
  if (is.na(concept) && isTRUE(prim$ambiguous) && isTRUE(prim$is_numeric)) {
    if (.is_likert_item(values)) {
      concept <- "likert"
      measurement_level <- "ordinal"
    }
  }

  # Concept-implied measurement level: a categorical concept is nominal even
  # when the rules could not decide the level from values alone (e.g. a gender
  # column with >2 spellings did not hit the binary rule).
  if (is.na(measurement_level) &&
      concept %in% c("gender", "race", "accuracy", "condition"))
    measurement_level <- "nominal"

  # unit: implied by a few concepts; NA otherwise.
  unit <- NA_character_
  if (identical(concept, "reaction_time")) {
    num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA),
                                            fixed = TRUE)))
    num <- num[!is.na(num) & num > 0]
    # Median RT above ~100 is almost certainly milliseconds; below, seconds.
    unit <- if (length(num) > 0 && stats::median(num) >= 100) "milliseconds"
            else if (length(num) > 0) "seconds" else NA_character_
    if (is.na(measurement_level)) measurement_level <- "ratio"
  } else if (identical(concept, "age")) {
    unit <- "years"
    if (is.na(measurement_level)) measurement_level <- "ratio"
  }

  # parse_note: the representation quirk the old fake col_types encoded.
  parse_note <- if (identical(ct, "continuous_comma_decimal")) "comma_decimal"
                else if (identical(ct, "continuous_outliers_excluded")) "mostly_numeric"
                else NA_character_

  list(
    representation    = representation,
    measurement_level = measurement_level,
    concept           = concept,
    role              = role,
    unit              = unit,
    quality           = quality,
    parse_note        = parse_note,
    # carried over for data_check's statistics + LLM targeting:
    numeric_values = prim$numeric_values,
    n_coerced      = prim$n_coerced,
    is_numeric     = prim$is_numeric,
    ambiguous      = prim$ambiguous
  )
}

# ── Qualtrics survey-export detection ─────────────────────────────────────────
# Qualtrics CSV/TSV exports have a fixed, distinctive shape: a set of reserved
# response-metadata columns (StartDate, Duration (in seconds), Finished, ...)
# that are the same across every survey, and — for the "use choice text" export
# — a multi-row header (machine names, then human question text, then an
# `ImportId` JSON row). We detect the file as Qualtrics from those metadata
# names and/or the ImportId row, strip the junk header rows so the data types
# correctly, and tag the metadata columns so data_validate can report the things
# that ARE reliably extractable from any Qualtrics file (completion time,
# preview/unfinished rows, recording window, which PII fields are present).
#
# We deliberately do NOT try to interpret the substantive question/scale columns
# here — that is the scale-block detector's job (a different unit).

# Reserved Qualtrics metadata column names, mapped to a semantic tag. Names are
# matched case-insensitively after stripping non-alphanumerics, so "Duration (in
# seconds)" and "Duration..in.seconds." (R-mangled) both hit. The tag drives both
# reporting and the multi-row-header fix.
.qualtrics_meta_cols <- c(
  startdate            = "qualtrics_start",
  enddate              = "qualtrics_end",
  status               = "qualtrics_status",
  ipaddress            = "qualtrics_ip",
  progress             = "qualtrics_progress",
  durationinseconds    = "qualtrics_duration",
  finished             = "qualtrics_finished",
  recordeddate         = "qualtrics_recorded",
  responseid           = "qualtrics_responseid",
  recipientlastname    = "qualtrics_recipient",
  recipientfirstname   = "qualtrics_recipient",
  recipientemail       = "qualtrics_email",
  externaldatareference = "qualtrics_externalref",
  externalreference    = "qualtrics_externalref",
  locationlatitude     = "qualtrics_lat",
  locationlongitude    = "qualtrics_lon",
  distributionchannel  = "qualtrics_channel",
  userlanguage         = "qualtrics_language"
)

# Normalise a column name to its Qualtrics lookup key: lowercase, drop anything
# non-alphanumeric. "Duration (in seconds)" -> "durationinseconds".
.qualtrics_key <- function(nm) gsub("[^a-z0-9]", "", tolower(nm))

# Map each column name of a data frame to its Qualtrics metadata tag (or NA).
.qualtrics_tag_cols <- function(col_names) {
  keys <- vapply(col_names, .qualtrics_key, character(1), USE.NAMES = FALSE)
  unname(.qualtrics_meta_cols[keys])
}

#' Detect whether a data frame is a Qualtrics survey export
#'
#' Fires when the columns include enough of Qualtrics' reserved response-metadata
#' names (StartDate, EndDate, Progress, Duration (in seconds), Finished,
#' RecordedDate, ResponseId, DistributionChannel, ...) that the file is
#' unambiguously a Qualtrics export — these exact names essentially never
#' co-occur outside Qualtrics. The `ResponseId` column (values like `R_xxxxx`)
#' or a leftover `ImportId` JSON header cell is treated as corroborating.
#'
#' @param df a data.frame (a read tabular file)
#' @param min_meta minimum number of distinct metadata columns required
#'
#' @returns `TRUE` when `df` looks like a Qualtrics export, else `FALSE`.
#' @export
#' @keywords internal
data_check_is_qualtrics <- function(df, min_meta = 4L) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  tags <- .qualtrics_tag_cols(names(df))
  n_meta <- length(unique(stats::na.omit(tags)))
  if (n_meta >= min_meta) return(TRUE)
  # Corroboration for borderline files (a heavily-renamed export): a ResponseId
  # column whose values are Qualtrics response ids (R_ + base62), or an ImportId
  # JSON cell surviving in the first rows.
  if (n_meta >= 2L) {
    rid <- names(df)[.qualtrics_key(names(df)) == "responseid"]
    if (length(rid) > 0) {
      v <- as.character(df[[rid[1]]])
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) > 0 && mean(grepl("^R_[A-Za-z0-9]{6,}$", v)) >= 0.5)
        return(TRUE)
    }
    # An ImportId cell survives in the first rows. read.delim strips the
    # surrounding quotes, so match the bare token, not a quoted one.
    if (any(vapply(df, function(col)
      any(grepl("ImportId", as.character(utils::head(col, 3)), fixed = TRUE)),
      logical(1)))) return(TRUE)
  }
  FALSE
}

# Is a row a Qualtrics secondary-header row (not real data)? The "use choice
# text" export writes, below the machine-name header: (row 1) the human question
# text, and (row 2) an `{"ImportId":...}` JSON blob. Read as data, these force
# every column to character. We detect such a row so data_read_head can drop it.
#
# A row is a secondary header when it carries the ImportId JSON, OR when it
# repeats the reserved metadata *labels* (e.g. a cell literally reading
# "Duration (in seconds)" or "Start Date") that Qualtrics puts in the question-
# text row for its own metadata columns.
.qualtrics_is_header_row <- function(row_vals) {
  vals <- trimws(as.character(row_vals))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) return(FALSE)
  # `ImportId` marks the JSON row. read.delim strips the JSON's quotes, so match
  # the bare token rather than the quoted `"ImportId"`.
  if (any(grepl("ImportId", vals, fixed = TRUE))) return(TRUE)
  # Question-text row: Qualtrics labels its own metadata columns with prose
  # versions of their names. If several cells match those labels, it's a header.
  label_keys <- .qualtrics_key(vals)
  mean(label_keys %in% names(.qualtrics_meta_cols)) >= 0.3
}

#' Strip Qualtrics secondary-header rows and re-type the columns
#'
#' A Qualtrics "use choice text" export has extra header rows (human question
#' text, then an `ImportId` JSON row) directly under the machine-name header.
#' `read.delim` reads the machine names as the header but keeps those two rows as
#' the first data rows, which forces every column to character. This drops any
#' leading rows that look like Qualtrics header rows (see
#' [.qualtrics_is_header_row]) and coerces columns that are now fully numeric
#' back to numeric, so the rest of `data_check` types the file correctly.
#'
#' @param df a data.frame read from a Qualtrics export (machine names as header)
#' @param max_strip maximum number of leading rows to consider stripping
#'
#' @returns the cleaned data.frame (unchanged if no header rows are found).
#' @export
#' @keywords internal
data_strip_qualtrics_header <- function(df, max_strip = 2L) {
  if (is.null(df) || nrow(df) == 0) return(df)
  drop <- 0L
  for (i in seq_len(min(max_strip, nrow(df)))) {
    if (.qualtrics_is_header_row(df[i, , drop = TRUE])) drop <- i else break
  }
  if (drop == 0L) return(df)
  df <- df[-seq_len(drop), , drop = FALSE]
  rownames(df) <- NULL
  # Columns that are now fully numeric (the junk text row was what made them
  # character) get coerced back, so data_col_type / stats treat them as numeric.
  for (j in seq_along(df)) {
    if (!is.character(df[[j]])) next
    v <- trimws(df[[j]])
    nonempty <- v[!is.na(v) & nzchar(v)]
    if (length(nonempty) == 0) next
    num <- suppressWarnings(as.numeric(nonempty))
    if (all(!is.na(num))) df[[j]] <- suppressWarnings(as.numeric(v))
  }
  df
}

# ── Likert scale-block detection ──────────────────────────────────────────────
# Shared by data_validate (careless responding) and codebook_check (LLM scale
# identification). A "scale block" is a run of adjacent Likert-type columns that
# share a variable-name prefix, i.e. one psychometric scale (PANAS_1..10).

# Minimum items for a block to count as a scale. Chosen empirically from the OSF
# corpus: real short psychological scales run ~5-7 items, so 5 keeps genuine
# short scales while dropping 3-4 item fragments that are too noisy to interpret.
.scale_min_items <- 5L

# Is a column a plausible Likert item? Integer-valued, 3-11 distinct levels (2
# is binary, not Likert), spanning a narrow range within a plausible bound.
#
# Deliberately does NOT key on the exact observed min-max: within one scale,
# different items reach different extremes, so per-column range varies (item A
# 1-4, item B 2-5) even on a shared metric. Keying on exact range would split a
# single scale into fragments; membership is decided by name prefix instead.
.is_likert_item <- function(x) {
  if (!is.numeric(x)) {
    xn <- suppressWarnings(as.numeric(as.character(x)))
    if (mean(is.na(xn)) > 0.2) return(FALSE)
    x <- xn
  }
  x <- x[!is.na(x)]
  if (length(x) < 10) return(FALSE)
  if (any(x != round(x))) return(FALSE)
  u <- unique(x)
  length(u) >= 3 && length(u) <= 11 &&
    diff(range(u)) <= 12 && min(u) >= -5 && max(u) <= 100
}

# Variable-name prefix: strip a trailing item number (bfi_1 -> bfi, RSE10 -> rse)
# so PANAS_1..10 and RSE_1..5 are recognised as two scales even when adjacent
# and on the same response range.
.scale_name_prefix <- function(nm) {
  p <- sub("[._-]?[0-9]+$", "", nm)
  p <- sub("[._-]+$", "", p)
  tolower(p)
}

# Pooled response range of a set of item columns, as a "min-max" label (e.g.
# "1-7").
.scale_block_range <- function(block) {
  v <- unlist(lapply(block, function(x)
    suppressWarnings(as.numeric(as.character(x)))), use.names = FALSE)
  v <- v[!is.na(v)]
  if (length(v) == 0) return("?")
  paste0(min(v), "-", max(v))
}

# Detect Likert scale blocks in a data frame: maximal runs of adjacent Likert
# columns sharing a name prefix. Returns a list of integer column-index vectors,
# one per block of at least `min_items` items. Scales are assumed contiguous
# (holds for typical survey exports, Q1_1, Q1_2, ...); a prefix change or a
# non-Likert column breaks a run.
.detect_scale_blocks <- function(df, min_items = .scale_min_items) {
  ok <- vapply(df, .is_likert_item, logical(1))
  nm <- names(df)
  blocks <- list(); start <- NA_integer_; cur_pre <- NA_character_
  flush <- function(endi) {
    if (is.na(start)) return(invisible())
    if (endi - start + 1L >= min_items)
      blocks[[length(blocks) + 1L]] <<- seq.int(start, endi)
  }
  for (j in seq_along(ok)) {
    p <- if (isTRUE(ok[[j]])) .scale_name_prefix(nm[[j]]) else NA_character_
    same <- !is.na(p) && identical(p, cur_pre)
    if (!same) {
      flush(j - 1L)
      cur_pre <- p
      start <- if (!is.na(p)) j else NA_integer_
    }
  }
  flush(length(ok))
  blocks
}
