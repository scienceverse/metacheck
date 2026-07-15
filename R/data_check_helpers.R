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
  spv = "output", fig = "output",
  # Binary scientific-data containers that name-based rules miss (they would
  # otherwise fall through to "other"). These hold research data, not assets.
  npy = "data", npz = "data", h5 = "data", hdf5 = "data", hdf = "data",
  fif = "data", pkl = "data", pickle = "data", pk = "data",
  ft = "data", feather = "data", parquet = "data", textgrid = "data"
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
#' @returns a character vector of data_check types (see `.data_check_types`);
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

  # A preregistration document belongs in documentation/, not data/. A file named
  # "preregistration" / "pre-registration" (or the abbreviation "prereg") is
  # treated as `supplemental` (which targets documentation/) so it is never
  # converted to a data CSV or filed as code — a prereg .csv/.tsv/.html would
  # otherwise be misrouted. Genuine analysis SCRIPTS named after the prereg keep
  # their `code` type (a script is still a script); everything else named prereg
  # is reclassified. The pattern matches "prereg", "pre-reg", "preregistration",
  # and "pre-registration" as a word (a leading boundary stops false hits inside
  # unrelated words).
  # Match "prereg" / "pre-reg" / "preregistration" / "pre-registration" only as a
  # whole token: bounded on the left (start or a non-letter) and on the right by a
  # non-letter or end-of-token — so "preregional" / "preregnancy" do NOT match.
  is_prereg <- grepl("(^|[^a-z])pre[ _-]?reg(istration)?([^a-z]|$)",
                     tolower(basename(file_name)))
  type[is_prereg & !type %in% c("code", "readme")] <- "supplemental"

  type[is.na(type)] <- "other"
  type
}

# ── Data format (tabular vs raw) ─────────────────────────────────────────────

.tabular_extensions <- c("csv", "tsv", "txt", "dat", "xlsx", "xls",
                         "sav", "dta", "sas7bdat", "jasp")
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

# Default sampler seed for the study-group pass. Fixed (not random) so repeated
# runs of the same paper ask the provider for the same sampling path; callers can
# override via params$seed. Best-effort — see .data_group_llm_impl().
.data_group_seed <- 8675309L

# Write a per-paper file manifest (JSON) recording every repository file and
# whether it was downloaded — the provenance needed to audit a corpus or rebuild
# a data archive without re-querying every repo. `files` is data_check's finalised
# `all_files`; `want` is the logical vector of files this run tried to download;
# `gated` is the download gate table (repos refused by the size caps);
# `oversize` / `failed` are download_repo_files()'s "oversize_skipped" and
# "failed" attributes; `zip_peek` the per-row zip-peek skip reasons; `model` the
# LLM model string the run used.
#
# Every file not downloaded is classified as **intentional** (a policy decision:
# download mode, skip_types, zip peek, the size caps — re-running changes
# nothing unless the settings change) or **unintentional** (the run wanted the
# file and could not fetch it: transient download failure, missing URL — a
# re-run with the same settings retries exactly these, since cached files are
# reused). The top-level `not_downloaded` block separates the two and sets
# `rerun_recommended`, so a corpus audit can find incomplete papers mechanically.
#
# The `provenance` block records what is needed to reproduce the archive: the
# metacheck version, R version and platform, the production timestamp, and the
# LLM model (when LLM assistance was on). Field names map onto DDI-Codebook 2.5
# elements and the mapping ships inside the manifest (provenance$ddi_mapping) so
# the JSON is self-describing.
#
# Sizes are completed here: a downloaded file's real size comes from disk, and a
# wanted file the listing left unsized (OSF returns NA for some files, often the
# large ones) is resolved with a cheap HEAD probe — so the manifest carries a
# real size for choosing the archive's size ceiling. Only NA-sized wanted files
# are probed, and only when a manifest is requested, so normal runs pay nothing.
.data_check_write_manifest <- function(manifest, files, want, gated,
                                       paper_id, download,
                                       max_file_size, max_download_size,
                                       skip_types = NULL,
                                       oversize = NULL, failed = NULL,
                                       zip_peek = NULL, model = NULL) {
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
  # `files` can grow after zip expansion; normalize `want` so all logical
  # operations below are length-stable and NA-free.
  if (length(want) == 0) {
    want <- rep(FALSE, n)
  } else if (length(want) != n) {
    want <- rep_len(want, n)
  }
  want <- as.logical(want)
  want[is.na(want)] <- FALSE

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
  probe <- which(is.na(file_size) & want &
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

  # Why was a file not downloaded? Ordered from most to least specific, and
  # classified: intentional = a policy decision (re-running changes nothing
  # unless settings change); unintentional = wanted but not fetched (a re-run
  # with the same settings retries exactly these).
  dtype <- files$data_type %||% rep(NA_character_, n)
  if (is.null(zip_peek) || length(zip_peek) != n)
    zip_peek <- c(zip_peek, rep(NA_character_, n))[seq_len(n)]
  over_key <- if (!is.null(oversize) && nrow(oversize) > 0)
    paste(oversize$repo_url, oversize$file_name) else character(0)
  fail_err <- if (!is.null(failed) && nrow(failed) > 0)
    stats::setNames(sub("\n.*", "", failed$error),
                    paste(failed$repo_url, failed$file_name)) else character(0)

  reason      <- rep(NA_character_, n)
  intentional <- rep(NA, n)
  for (i in which(!downloaded)) {
    key <- paste(files$repo_url[i], files$file_name[i])
    url <- files$file_url[i] %||% NA_character_
    if (identical(download, "none")) {
      reason[i] <- "download = \"none\""; intentional[i] <- TRUE
    } else if (!is.null(skip_types) && dtype[i] %in% skip_types) {
      reason[i] <- sprintf("excluded type '%s' (linked, not mirrored)", dtype[i])
      intentional[i] <- TRUE
    } else if (!is.na(zip_peek[i]) && nzchar(zip_peek[i])) {
      reason[i] <- zip_peek[i]; intentional[i] <- TRUE
    } else if (!isTRUE(want[i])) {
      reason[i] <- "not a data/codebook/README file (use download = \"all\")"
      intentional[i] <- TRUE
    } else if (is.na(url) || !nzchar(url)) {
      reason[i] <- "no download URL in the listing"; intentional[i] <- FALSE
    } else if (key %in% over_key) {
      reason[i] <- sprintf("exceeds max_file_size (%s MB): skipped by the per-file cap",
                           .cap_num(max_file_size))
      intentional[i] <- TRUE
    } else if (files$repo_url[i] %in% gated_urls) {
      reason[i] <- "repository refused by the size caps"; intentional[i] <- TRUE
    } else if (key %in% names(fail_err)) {
      reason[i] <- paste0("download failed after retries: ", fail_err[[key]])
      intentional[i] <- FALSE
    } else {
      reason[i] <- "download failed"; intentional[i] <- FALSE
    }
  }
  status <- ifelse(downloaded, "downloaded",
                   ifelse(intentional %in% TRUE, "skipped", "failed"))

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
      status       = status[i],
      skip_reason  = if (downloaded[i]) NULL else reason[i],
      skip_intentional = if (downloaded[i]) NULL else intentional[i]
    ))
  })

  generated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  unint <- which(!downloaded & intentional %in% FALSE)
  intent <- which(!downloaded & intentional %in% TRUE)

  # Reproducibility metadata. Field names map onto DDI-Codebook 2.5 elements;
  # ddi_mapping documents the correspondence inside the manifest itself.
  provenance <- list(
    software  = list(name = "metacheck", version = tryCatch(
      as.character(utils::packageVersion("metacheck")),
      error = function(e) NA_character_)),
    r_version = R.version.string,
    platform  = R.version$platform,
    prod_date = generated,
    llm       = if (isTRUE(llm_use()))
      list(used = TRUE, model = model %||% llm_model())
    else list(used = FALSE),
    ddi_mapping = list(
      "provenance.software"  = "docDscr/citation/prodStmt/software (@version)",
      "provenance.prod_date" = "docDscr/citation/prodStmt/prodDate",
      "files[].file_name"    = "fileDscr/fileTxt/fileName",
      "files[].file_url"     = "fileDscr/@URI",
      "files[].data_type"    = "fileDscr/fileTxt/fileCont",
      "files[].status"       = "fileDscr/fileTxt/ProcStat",
      "files[].skip_reason"  = "fileDscr/notes"
    )
  )

  doc <- list(
    paper_id  = if (length(paper_id)) paper_id[[1]] else NA_character_,
    generated = generated,
    download  = download,
    skip_types = if (length(skip_types)) as.list(skip_types) else NULL,
    caps      = list(max_file_size_mb = max_file_size,
                     max_download_size_mb = max_download_size),
    provenance   = provenance,
    n_files      = n,
    n_downloaded = sum(downloaded),
    not_downloaded = list(
      intentional_n   = length(intent),
      unintentional_n = length(unint),
      # The unintentional list is the re-run signal: these are the files a
      # re-run with the same settings will retry (cache reuse skips the rest).
      unintentional_files = lapply(unint, function(i) list(
        file_name = files$file_name[i],
        repo_url  = files$repo_url[i],
        reason    = reason[i])),
      rerun_recommended = length(unint) > 0
    ),
    files        = entries
  )
  doc <- Filter(Negate(is.null), doc)

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
#' `shared`. Paths that name their study outright ("Experiment 1/",
#' "study2a_data.csv") are grouped by a deterministic regex first and skip the
#' model entirely; LLM-returned codes are normalized and validated against the
#' scheme, so a malformed code can never become a study directory name. The
#' sent files are batched (see `.data_check_llm_batch`) so large repositories
#' do not exceed the model's request/output limits.
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
                           batch_size = .data_check_llm_batch, paper = NULL) {
  return(.data_group_llm_impl(files, model, params, batch_size, paper))
}

#' Study roster named in a paper's text
#'
#' Reads the manuscript for the studies it names — "Experiment 1", "Study 2a",
#' "Pilot 2" — and returns them as normalised group codes (`ex1`, `ex2a`,
#' `pilot2`). This is the AUTHORITATIVE list of a paper's studies: the authors
#' say how many there are and what they are called, so it both names the groups
#' and gives a count to validate any file grouping against (see
#' `.data_group_check_roster`). Deterministic and free — a regex over text we
#' already extracted — so it runs BEFORE any LLM.
#'
#' Only mentions of the form <word><optional space/punct><number><optional
#' single letter> count; a bare "the experiment" names no specific study and is
#' ignored. Sorted by number then letter, so the order is stable.
#'
#' @param paper a paper object
#'
#' @returns a character vector of group codes, or `character(0)` when the text
#'   names no numbered study.
#' @export
#' @keywords internal
data_study_roster <- function(paper) {
  if (!.is_paper(paper)) return(character(0))
  hits <- tryCatch(
    text_search(paper, "\\b(?:study|experiment|pilot)[ ._-]?[0-9]{1,2}[a-z]?\\b",
                return = "match", ignore.case = TRUE, perl = TRUE),
    error = function(e) NULL)
  if (is.null(hits) || !nrow(hits)) return(character(0))
  m <- tolower(gsub("[ ._-]", "", as.character(hits$text)))
  # A trailing letter is a sub-study suffix ("2a"); anything else is dropped by
  # the normalizer, which also maps experiment/study -> ex and keeps pilot.
  code <- .data_group_normalize(m)
  code <- unique(code[!is.na(code) & code != "shared"])
  if (!length(code)) return(character(0))
  # Stable order: by number, then by sub-study letter.
  num <- suppressWarnings(as.integer(sub("^(ex|pilot)([0-9]{1,2})[a-z]?$", "\\2", code)))
  suf <- sub("^(ex|pilot)[0-9]{1,2}([a-z]?)$", "\\2", code)
  code[order(num, suf)]
}

# Compare a file grouping to the manuscript's study roster and report the
# agreement. The roster is what the AUTHORS say exists; the grouping is what we
# inferred from the files. A mismatch means the structure we are about to write
# contradicts the paper — worth surfacing rather than silently emitting. Returns
# list(roster, found, missing, extra, agrees).
.data_group_check_roster <- function(groups, roster) {
  found <- unique(groups[!is.na(groups) & groups != "shared"])
  list(roster  = roster,
       found   = found,
       missing = setdiff(roster, found),   # named in the paper, not in the files
       extra   = setdiff(found, roster),   # in the files, not named in the paper
       agrees  = length(roster) > 0 && setequal(roster, found))
}

# Data files referenced by a code file. A script names the data it reads and
# writes — read_csv("raw/x.csv"), readRDS("../data/processed/y.rds"),
# write_csv(df, "processed/z.csv") — which is HARD evidence that the script and
# those files belong to the same study: no guessing, no LLM. Returns the
# referenced paths' basenames (lowercased), or character(0).
#
# Matching on basenames deliberately ignores the relative prefix: a script's
# "../data/processed/trial_level.csv" and the repository's
# "processed/trial_level.csv" are the same file seen from different working
# directories, and reconciling those prefixes reliably is not worth it when the
# basename already identifies the file within its repository.
.CODE_READ_FNS <- paste(
  "read_csv2?", "read\\.csv2?", "read_tsv", "read_delim", "read\\.delim",
  "read_table2?", "read\\.table", "readRDS", "read_rds", "read_excel",
  "read_xlsx", "read_xls", "read_sav", "read_dta", "read_sas", "read_spss",
  "fread", "read_json", "fromJSON", "read_feather", "read_parquet",
  "write_csv2?", "write\\.csv2?", "write_tsv", "write_delim", "saveRDS",
  "write_rds", "write_xlsx", "write_sav", "write_dta", "write_feather",
  "write_parquet", "load", "save",
  sep = "|")

.data_code_refs <- function(path, max_bytes = 2e6) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(character(0))
  if (file.size(path) > max_bytes) return(character(0))   # not a script
  txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                  error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(character(0))
  # <fn>( ... "<path>"  — the first quoted string of a read/write call. Allows
  # arguments before the path (write_csv(df, "out.csv")).
  pat <- paste0("(?:", .CODE_READ_FNS, ")\\s*\\([^)\"']*[\"']([^\"']+)[\"']")
  m <- regmatches(txt, gregexpr(pat, txt, perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(m)) return(character(0))
  refs <- sub(paste0("^.*?[\"']([^\"']+)[\"'].*$"), "\\1", m)
  refs <- refs[grepl("\\.[A-Za-z0-9]{1,6}$", refs)]   # must look like a file
  unique(tolower(basename(gsub("\\\\", "/", refs))))
}

# Deterministically derive a study group from a file path, or NA when the path
# names no study. Filenames and folder names very often carry the study label
# verbatim — "Experiment 1/", "study2a_data.csv", even smashed together without
# separators ("...dataexperiment1creplication...") — and a regex reads those
# more reliably than a small LLM, which has misread exactly such names. The
# filename is searched first, then the enclosing folders from innermost to
# outermost. The short prefixes ("ex", "exp") must not be preceded by a letter
# (so "index1"/"flex2" don't match), while the full words match even embedded
# in smashed-together names. A trailing letter counts as a sub-study suffix
# only when it ends its token ("study2a_data" -> ex2a), not when the next word
# merely starts with a letter ("experiment3explicit" -> ex3).
.data_group_from_path <- function(paths) {
  ex_pat <- paste0(
    "(?:experiment|study|(?<![a-z])expt?|(?<![a-z])ex)",
    "[ ._-]?([0-9]{1,2})([a-z](?![a-z]))?"
  )
  pilot_pat <- "(?<![a-z])pilot[ ._-]?([0-9]{1,2})?"
  vapply(paths, function(path) {
    if (is.na(path) || !nzchar(path)) return(NA_character_)
    parts <- rev(strsplit(tolower(path), "/", fixed = TRUE)[[1]])
    for (part in parts) {
      m <- regmatches(part, regexec(ex_pat, part, perl = TRUE))[[1]]
      if (length(m) > 0) return(paste0("ex", m[2], m[3]))
      m <- regmatches(part, regexec(pilot_pat, part, perl = TRUE))[[1]]
      if (length(m) > 0)
        return(paste0("pilot", if (nzchar(m[2])) m[2] else "1"))
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# Normalize an LLM-returned study-group code to the documented scheme and
# reject anything outside it (NA). The model occasionally answers in prose
# variants ("Experiment 1", "study 2a") or with a bare "pilot"; anything that
# still doesn't fit ex<N><letter?>/pilot<N>/shared after normalization is a
# hallucination and must not leak into study directory names.
.data_group_normalize <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[ ._-]", "", x)
  x <- sub("^(experiment|study|expt|exp)(?=[0-9])", "ex", x, perl = TRUE)
  x[x == "pilot"] <- "pilot1"
  ifelse(grepl("^(ex|pilot)[0-9]{1,2}[a-z]?$|^shared$", x), x, NA_character_)
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

# Separate a paper's files into studies BY SOURCE REPOSITORY. A paper often links
# several independent repositories (multiple OSF components, a Zenodo archive, a
# GitHub repo); when those repos hold DIFFERENT files, each is a distinct study —
# a far more reliable signal than the file paths, which frequently name only a
# processing stage ("raw/", "processed/") and no study at all. Returns a per-file
# base study code (`ex1`, `ex2`, ... by first appearance of each qualifying repo),
# or all-NA when there is only one repository or the repos are mirrors of each
# other (near-identical file sets — not separate studies).
#
# The "files differ" guard compares each pair of repos' basename sets by Jaccard
# overlap; repos that overlap >= `mirror_overlap` are treated as one study (the
# earlier slot), so a duplicated/mirrored component does not spawn a bogus study.
.data_group_from_repo <- function(repo, paths, mirror_overlap = 0.9) {
  n <- length(paths)
  if (n == 0) return(character(0))
  repo <- as.character(repo)
  repo[is.na(repo) | !nzchar(repo)] <- NA_character_
  distinct <- unique(repo[!is.na(repo)])
  if (length(distinct) < 2) return(rep(NA_character_, n))   # single (or no) repo

  base <- tolower(basename(gsub("\\\\", "/", paths)))
  files_of <- lapply(distinct, function(r) unique(base[!is.na(repo) & repo == r]))
  names(files_of) <- distinct

  # Assign each distinct repo a study slot, merging repos that mirror an earlier
  # one (near-identical file sets) into that earlier slot.
  slot_of <- stats::setNames(rep(NA_integer_, length(distinct)), distinct)
  next_slot <- 0L
  for (r in distinct) {
    merged_into <- NA_integer_
    for (prev in distinct) {
      if (identical(prev, r) || is.na(slot_of[prev])) next
      a <- files_of[[r]]; b <- files_of[[prev]]
      inter <- length(intersect(a, b)); uni <- length(union(a, b))
      if (uni > 0 && inter / uni >= mirror_overlap) { merged_into <- slot_of[prev]; break }
    }
    if (!is.na(merged_into)) slot_of[r] <- merged_into
    else { next_slot <- next_slot + 1L; slot_of[r] <- next_slot }
  }
  if (max(slot_of, na.rm = TRUE) < 2) return(rep(NA_character_, n))  # all one study

  out <- rep(NA_character_, n)
  have <- !is.na(repo)
  out[have] <- paste0("ex", slot_of[repo[have]])
  out
}

.data_group_llm_impl <- function(files, model = llm_model(), params = list(),
                                 batch_size = 30, paper = NULL) {
  if (is.null(files) || nrow(files) == 0) return(NULL)
  # Pin the sampler unless the caller chose otherwise. llm() already defaults to
  # temperature 0, but on a SERVED model that alone does not guarantee a
  # reproducible answer (request batching, KV-cache state and GPU floating-point
  # non-associativity all perturb the logits), and study groups decide the
  # dataset's directory structure — a run-to-run flip silently reshapes the
  # output. Providers document `seed` as best-effort rather than a promise, so
  # this narrows the variance, it does not eliminate it; the deterministic passes
  # above are what actually make the common cases reproducible.
  if (is.null(params$seed)) params$seed <- .data_group_seed
  paths <- if ("file_path" %in% names(files)) files$file_path else files$file_name
  paths <- ifelse(is.na(paths) | !nzchar(paths), files$file_name, paths)
  paths <- gsub("\\\\", "/", paths)
  repo  <- if ("repo_url" %in% names(files)) files$repo_url else
           if ("repo_name" %in% names(files)) files$repo_name else
           rep(NA_character_, length(paths))

  # Only group files that will actually be analysed or placed into a study
  # directory: data files, codebooks, code, and readmes. Assets, generic
  # "other", and bulk output/supplemental files don't drive study structure, so
  # we don't spend LLM calls on them; they default to 'shared'. When no
  # data_type column is present we fall back to grouping everything.
  placeable <- c("data", "codebook", "software", "code", "readme")
  dtype <- if ("data_type" %in% names(files))
    tolower(as.character(files$data_type)) else rep(NA_character_, length(paths))
  send <- if (all(is.na(dtype))) rep(TRUE, length(paths)) else dtype %in% placeable

  # Base group by SOURCE REPOSITORY: a paper that links several repos with
  # different files is multi-study, one study per repo (see .data_group_from_repo).
  # This seeds the default so unrecognised files fall to their repo's study, not
  # to 'shared'. NA (single repo / mirrors) keeps the old 'shared' default.
  repo_grp <- .data_group_from_repo(repo, paths)
  multi_repo <- any(!is.na(repo_grp))
  group <- ifelse(is.na(repo_grp), "shared", repo_grp)

  # Deterministic pre-pass: a path that names its study outright ("Experiment
  # 1/", "study2a_data.csv", "...experiment1creplication...") overrides the repo
  # base — an explicit study name in the path is more specific than "which repo".
  # The regex is exact where a small LLM has misread such names. Files still
  # ambiguous AFTER both repo and path passes go to the LLM.
  pre <- .data_group_from_path(paths)
  fixed <- send & !is.na(pre)
  group[fixed] <- pre[fixed]

  # CODE-REFERENCE pass: a script names the data it reads and writes, so every
  # file it references belongs to the script's study. This is hard evidence (no
  # guessing) and rescues data files whose own path names no study — the common
  # case, where paths describe a processing stage ("raw/", "processed/") rather
  # than a study. Only fills files still unplaced by the repo/path passes, and
  # only from scripts that ARE placed, so it propagates a known group outward
  # rather than inventing one.
  loc <- if ("file_location" %in% names(files)) files$file_location else
    rep(NA_character_, length(paths))
  is_code <- dtype %in% c("code", "software")
  placed  <- !is.na(group) & group != "shared"
  script_i <- which(is_code & placed & !is.na(loc))
  if (length(script_i)) {
    base_of <- tolower(basename(paths))
    for (si in script_i) {
      refs <- .data_code_refs(loc[si])
      if (!length(refs)) next
      # Files this script references that are still unplaced -> the script's group.
      hit <- base_of %in% refs & (is.na(group) | group == "shared")
      if (any(hit)) group[hit] <- group[si]
    }
  }

  # When the repository already separates studies, trust it: only send files the
  # repo pass could NOT place (single-repo case) to the LLM. This avoids the LLM
  # re-scattering repo-separated files to 'shared'.
  send <- send & is.na(pre) & (!multi_repo | is.na(repo_grp)) &
    (is.na(group) | group == "shared")

  # The LLM is the LAST resort: it only sees files the deterministic passes
  # (repository, path regex, code references) could not place. When they placed
  # everything — the common case for a multi-repo paper — no call is made at all.
  # NB: this must not return early; the roster relabelling and the "data is never
  # shared" guard below still have to run.
  prompt <- paste(
    "You are grouping the files of a psychology research repository by study.",
    "Many repositories contain multiple studies (Experiment 1, Study 2a, a",
    "pilot, ...). Assign each numbered file to a study group using these codes:",
    "'ex1','ex2','ex2a',... for experiments/studies, 'pilot1','pilot2',... for",
    "pilots. Infer groups from folder names and filenames. A DATA file always",
    "belongs to a study — never leave a data file ungrouped. Use 'shared' ONLY",
    "for repository-wide NON-DATA files that genuinely serve every study (a",
    "top-level README, a whole-repo codebook, shared materials). If the whole",
    "repository is a single study, put every file in 'ex1' (not 'shared').",
    "Return one entry per input file, in the same order."
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

  any_ok <- FALSE
  used_model <- NA_character_
  unresolved <- integer(0)   # rows no batch (or retry) ever answered for

  # Ask the model about one batch of rows; returns the rows it could NOT place.
  # A batch can fail outright (network error, HTTP 400 json_validate_failed —
  # providers reject a structured response they cannot validate, which happens
  # more often on LONG arrays) or come back partial. Either way the rows left
  # over are reported back so the caller can retry them in smaller pieces rather
  # than silently leaving them at their default — the old behaviour, which made
  # an intermittent provider error look exactly like "the model said 'shared'"
  # and was the main source of run-to-run instability.
  ask_batch <- function(rows) {
    listing <- paste(seq_along(rows), paths[rows], sep = ". ", collapse = "\n")
    resp <- tryCatch(
      llm(text = data.frame(text = listing), text_col = "text",
          system_prompt = prompt, type = type_spec, model = model,
          params = params, phase = "Assigning study groups"),
      error = function(e) NULL
    )
    resp <- .strip_llm_wrapper(resp, "assignments")
    if (is.null(resp) || nrow(resp) == 0 ||
        !all(c("index", "group") %in% names(resp))) return(rows)
    idx <- suppressWarnings(as.integer(resp$index))
    # Normalize the model's codes to the documented scheme and drop anything
    # that still doesn't fit (a hallucinated code like "pilot" for a file whose
    # name says "experiment3" must not become a study directory name).
    grp <- .data_group_normalize(resp$group)
    ok  <- !is.na(idx) & idx >= 1 & idx <= length(rows) & !is.na(grp)
    if (any(ok)) {
      group[rows[idx[ok]]] <<- grp[ok]
      any_ok <<- TRUE
    }
    if (is.na(used_model))
      used_model <<- attr(resp, "llm")$model %||% NA_character_
    rows[setdiff(seq_along(rows), idx[ok])]   # rows still unanswered
  }

  for (rows in batches) {
    left <- ask_batch(rows)
    # ONE retry, at half the batch size. A provider that rejects a batch usually
    # does so because the structured response was too long, and halving fixes
    # that; if the halves still fail the request itself is the problem and
    # splitting further will not help. Deliberately bounded: retrying down to
    # single files would cost ~2n calls per failed batch (~100 for a 50-file
    # batch) to place files the fallback rules below place for free.
    if (length(left) > 1L) {
      chunks <- split(left, ceiling(seq_along(left) / max(1L, length(left) %/% 2L)))
      left <- unlist(lapply(chunks, ask_batch), use.names = FALSE)
      if (is.null(left)) left <- integer(0)
    }
    if (length(left) > 0) unresolved <- c(unresolved, left)
  }
  # Regex- or repo-derived groups are worth returning even when every LLM batch
  # failed; only give up when NO pass produced anything (no repo split, no path
  # match, no LLM answer).
  if (!any_ok && !any(fixed) && !multi_repo) return(NULL)

  # A DATA file must NEVER be 'shared' — data always belongs to a study. Fall any
  # still-'shared' data file back to a study: its own repo's study when the repo
  # pass placed it, else the sole study when exactly one exists (a single-study
  # repo whose data the LLM wrongly called 'shared'). Only genuinely repo-wide
  # NON-DATA (README, codebook, materials) may remain 'shared'. This runs BEFORE
  # the roster relabelling below so it works with the raw slot labels.
  is_data <- dtype == "data"
  study_codes <- unique(group[grepl("^(ex|pilot)[0-9]", group)])
  stray <- is_data & group == "shared"
  if (any(stray)) {
    group[stray & !is.na(repo_grp)] <- repo_grp[stray & !is.na(repo_grp)]
    still <- is_data & group == "shared"
    if (any(still) && length(study_codes) == 1L) group[still] <- study_codes[[1]]
    # No study exists at all (single-study repo, LLM gave only 'shared'): every
    # data file becomes ex1 so nothing lands at the collection root as data.
    still <- is_data & group == "shared"
    if (any(still) && length(study_codes) == 0L) group[still] <- "ex1"
  }

  # RELABEL from the manuscript's study roster. The authors say what their
  # studies are called ("Experiment 1, 2a, 2b, 3"); our partition may be
  # structurally right but named by slot (ex1..ex4 from four repositories). When
  # the partition has exactly as many groups as the paper names studies, adopt
  # the authors' labels — the paper is authoritative for naming.
  #
  # Groups already carrying a roster label (a path that literally said
  # "Experiment 2a") are left alone and their label is taken out of the pool, so
  # only the slot-named groups are renamed. Mapping is by first appearance, which
  # matches how both lists are ordered (repo order / study order) but is a
  # heuristic: when the counts differ we do NOT rename at all, and the roster
  # check below reports the disagreement instead of guessing.
  roster <- if (!is.null(paper)) data_study_roster(paper) else character(0)
  if (length(roster)) {
    found <- unique(group[!is.na(group) & group != "shared"])
    already <- intersect(found, roster)              # correctly named already
    to_name <- setdiff(found, roster)                # slot-named (ex1, ex2, ...)
    avail   <- setdiff(roster, already)
    if (length(to_name) > 0 && length(to_name) == length(avail)) {
      # Order both by first appearance so the mapping is stable.
      ord <- to_name[order(match(to_name, group[!is.na(group)]))]
      map <- stats::setNames(avail, ord)
      hit <- !is.na(group) & group %in% ord
      group[hit] <- unname(map[group[hit]])
    }
  }
  out <- data.frame(group = group)
  attr(out, "model") <- used_model
  attr(out, "roster") <- roster
  attr(out, "roster_check") <- .data_group_check_roster(group, roster)
  # Files the model never answered for, even after retries. They keep whatever
  # default they had (the safety net below still guarantees no data file stays
  # 'shared'), but the caller is told so an intermittent provider failure is
  # visible instead of masquerading as a real 'shared' verdict.
  attr(out, "unresolved") <- if (length(unresolved))
    paths[sort(unique(unresolved))] else character(0)
  out
}

# ── Tabular reading ──────────────────────────────────────────────────────────

# Sniff the field delimiter of a delimited text file from its first
# non-blank, non-comment line.
# Reinterpret invalid-UTF-8 bytes in freshly read lines as Latin-1 (a
# conversion that cannot fail, since every byte is a valid Latin-1 character).
# The pre-read sniffers below run string ops (trimws, strsplit, gsub) on raw
# readLines() output, and any of those errors with "input string 1 is invalid
# UTF-8" when a Latin-1-encoded file has a non-ASCII byte in its first lines —
# which used to make the whole file unreadable before the readers' own
# encoding tolerance ever got a chance.
.utf8_lines <- function(x) {
  if (length(x) == 0) return(x)
  bad <- !validUTF8(x)
  if (any(bad)) x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
  x
}

.sniff_delimiter <- function(path) {
  line <- character(0)
  con  <- file(path, "r")
  on.exit(close(con))
  for (i in seq_len(10)) {
    line <- .utf8_lines(readLines(con, n = 1, warn = FALSE))
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
    l <- .utf8_lines(readLines(con, n = 1, warn = FALSE))
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
# Cheaply detect a "single big field" file — a .csv/.txt/.dat whose content is
# really one large value stuffed into a single column, not a table. This covers
# any such file, whatever the value is (a JSON blob, an XML document, a base64
# string, a serialised log, ...): the giveaway is format-independent — the data
# is a *single column* whose rows are *huge*. A real one-column dataset has short
# rows (one value each); a blob-in-a-cell has an enormous row. Such files are
# pathologically slow to parse with read.delim and carry no tabular data, so
# data_read_head() skips them. Reads only the first two lines, so the check is
# effectively free versus the multi-second (sometimes minute-long) read.
#
# We count fields quote-aware (a delimiter inside a quoted value does not split a
# field), so a fully-quoted blob containing thousands of commas is still one
# column. The row-size threshold keeps genuinely narrow one-column CSVs safe.
.blob_row_min_bytes <- 4096L

.count_fields <- function(line, sep) {
  # Number of top-level fields: split on `sep` only when outside double quotes.
  chars <- strsplit(line, "", fixed = TRUE)[[1]]
  if (length(chars) == 0) return(0L)
  in_quote <- FALSE
  fields <- 1L
  for (ch in chars) {
    if (ch == "\"") in_quote <- !in_quote
    else if (!in_quote && ch == sep) fields <- fields + 1L
  }
  fields
}

.is_single_field_blob <- function(path, sep) {
  con <- file(path, "r")
  on.exit(close(con))
  first2 <- tryCatch(.utf8_lines(readLines(con, n = 2, warn = FALSE)),
                     error = function(e) character(0))
  if (length(first2) < 2) return(FALSE)
  header <- first2[[1]]
  row1   <- first2[[2]]
  # A single-column header AND an oversized first data row = one big field, not
  # a table. Field counts are quote-aware so a quoted value's inner delimiters
  # don't inflate the count.
  .count_fields(header, sep) <= 1L &&
    .count_fields(row1, sep) <= 1L &&
    nchar(row1, type = "bytes") >= .blob_row_min_bytes
}

# Read a delimited file into a data.frame. Uses data.table::fread when available
# — orders of magnitude faster than utils::read.delim on files with large or
# awkward quoted fields (e.g. cells holding multi-line numpy-array dumps), which
# make base R's quote-scanning pathologically slow (minutes per file). Falls back
# to read.delim (with a latin1 retry for invalid UTF-8) when data.table is not
# installed. `n_rows = Inf` reads the whole file.
.read_delim_fast <- function(path, sep, header, n_rows = Inf) {
  nmax <- if (is.finite(n_rows)) n_rows else Inf
  if (requireNamespace("data.table", quietly = TRUE)) {
    # fread self-corrects quoting/field-count quirks but warns while doing so
    # (as read.delim does); suppress those, matching the read.delim path.
    df <- tryCatch(
      suppressWarnings(as.data.frame(
        data.table::fread(
          path, sep = sep, header = header,
          nrows = if (is.finite(nmax)) nmax else -1L,
          showProgress = FALSE, data.table = FALSE,
          check.names = FALSE, encoding = "UTF-8"),
        check.names = FALSE)),
      error = function(e) NULL)
    if (!is.null(df)) return(df)
    # fall through to read.delim on any fread failure
  }
  df <- suppressWarnings(
    utils::read.delim(path, sep = sep, header = header, nrows = n_rows,
                      check.names = FALSE))
  has_invalid <- any(vapply(df, function(col) {
    is.character(col) && any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
  }, logical(1)))
  if (has_invalid) {
    df <- suppressWarnings(
      utils::read.delim(path, sep = sep, header = header, nrows = n_rows,
                        check.names = FALSE, fileEncoding = "latin1"))
  }
  df
}

# Coerce a just-read data frame to valid UTF-8, names first, then values.
# A stray non-UTF-8 byte in a header (e.g. a Latin-1 or BOM byte the file's
# own read tolerated) otherwise crashes downstream `grepl(..., perl = TRUE)`
# name checks with "invalid multibyte string"; sub out invalid bytes rather
# than dropping the column. For character VALUES: fread reads with
# encoding = "UTF-8", which marks strings as UTF-8 without validating, so a
# Latin-1 byte in a nominally-UTF-8 file (a mis-encoded apostrophe, °, µ,
# é ...) yields strings that crash the base regex calls data_check runs on
# every column ("input string N is invalid UTF-8"). Reinterpret only the
# invalid entries as Latin-1 — a conversion that cannot fail, since every
# byte is a valid Latin-1 character — and leave valid values untouched.
# The per-column repair counts are recorded in the "utf8_repaired" attribute
# so data_check can carry them into its columns table and data_validate can
# warn the researcher about the file's mixed encoding (the repaired values
# themselves no longer show it). Idempotent: a second pass finds nothing
# invalid and leaves both the data and the attribute untouched.
.utf8_repair_df <- function(df) {
  if (is.null(df)) return(df)
  if (!is.null(names(df))) {
    nm <- names(df)
    bad <- is.na(iconv(nm, from = "UTF-8", to = "UTF-8"))
    if (any(bad)) {
      fixed <- iconv(nm[bad], from = "latin1", to = "UTF-8", sub = "")
      fixed[is.na(fixed) | !nzchar(fixed)] <- paste0("col_", which(bad))[is.na(fixed) | !nzchar(fixed)]
      nm[bad] <- fixed
      names(df) <- nm
    }
  }
  if (ncol(df) > 0) {
    repaired <- integer(0)
    for (j in seq_along(df)) {
      x <- df[[j]]
      if (is.character(x)) {
        bad <- !is.na(x) & !validUTF8(x)
        if (any(bad)) {
          x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8")
          df[[j]] <- x
          repaired[names(df)[j]] <- sum(bad)
        }
      } else if (is.factor(x)) {
        lv <- levels(x)
        bad <- !is.na(lv) & !validUTF8(lv)
        if (any(bad)) {
          lv[bad] <- iconv(lv[bad], from = "latin1", to = "UTF-8")
          levels(df[[j]]) <- lv
          repaired[names(df)[j]] <- sum(bad)
        }
      }
    }
    if (length(repaired) > 0) attr(df, "utf8_repaired") <- repaired
  }
  df
}

data_read_head <- function(path, n_rows = 5) {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    df <- switch(ext,
      csv = , txt = , tsv = , dat = {
        sep <- if (ext == "tsv") "\t" else .sniff_delimiter(path)
        hdr <- .detect_header(path, sep)
        # Cheap bail-out for a non-tabular file disguised as .csv: one big field
        # (e.g. a JSON blob dumped under a single header). It yields a useless
        # 1-column "table" and there is nothing to extract. Detect it from the
        # first two lines only. See .is_single_field_blob().
        if (.is_single_field_blob(path, sep)) return(NULL)
        df <- .read_delim_fast(path, sep = sep, header = hdr, n_rows = n_rows)
        if (!hdr && !is.null(df) && ncol(df) > 0)
          names(df) <- paste0("col_", seq_len(ncol(df)))
        # Repair invalid UTF-8 before the Qualtrics detection below, whose
        # regex calls on names and head values would otherwise error/warn on a
        # Latin-1 byte in the first rows. (The repair after the switch is then
        # a no-op for this branch.)
        df <- .utf8_repair_df(df)
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
      jasp = {
        # A .jasp bundles a labelled data frame (like SPSS): read_jasp() returns
        # the columns with haven-style label/labels attributes, so the rest of
        # data_check (and the CSV conversion in psychds-convert) treats it exactly
        # like a .sav. Both the old binary and modern SQLite formats are handled.
        df <- read_jasp(path)$data
        if (is.data.frame(df) && is.finite(n_rows)) utils::head(df, n_rows) else df
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
    .utf8_repair_df(df)
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

# Detect a Likert / rating scale in a numeric column and infer its valid range.
#
# A scale is a small set of CONSECUTIVE integers spanning a plausible range
# (0-based, 1-based, or symmetric bipolar). The column is expected to be
# CONTAMINATED — the whole reason to detect the scale is to surface the weird
# values (a stray 99, a mistyped 33) as being outside the valid range. So the
# range must be inferred robustly, from the DENSE core of common consecutive
# levels, not from min()/max() (which one outlier destroys).
#
# Method (hybrid "E"): find the dense consecutive core (mode-anchored, bridging
# small interior gaps, stopping at a rare+gapped level), then anchor the FLOOR
# to the natural scale start (1, or 0 if a 0 is observed) — reporting that
# inference — and take the CEILING as the top core level. Everything outside the
# accepted [lo, hi] is returned as `suspects` for the out-of-range / miscoded
# checks to interpret.
#
# Returns NULL when the column is not a scale (too many levels, non-integer,
# core too short, or the core does not explain enough of the data). Otherwise a
# list: lo, hi, levels_present, coverage, suspects, floor_inferred (the
# levels we inferred below the lowest observed), note (human-readable).
#
# `min_core` = minimum consecutive core levels to count as a scale (default 3).
# `min_coverage` = the accepted range must explain at least this fraction of the
# non-missing values (default 0.90); the leftover are the suspects.
# `common_frac` = a level counts as a real (bridgeable) response level at this
# fraction of the data, absolute floor 2 (default 0.01); rarer gapped levels are
# treated as detached contaminants and left as suspects.
.detect_likert_scale <- function(x, max_levels = 23L, min_core = 3L,
                                  min_coverage = 0.90, common_frac = 0.01) {
  x <- x[!is.na(x) & !is.nan(x) & is.finite(x)]
  if (length(x) < 20) return(NULL)          # need enough data to bound a scale
  if (any(x != round(x)))  return(NULL)     # non-integer -> continuous
  x <- as.integer(round(x))
  u <- sort(unique(x))
  # A scale lives within [-11, 11]; a spread of distinct levels beyond ~23 is a
  # count/continuous column, not a rating scale.
  if (length(u) < 2L || length(u) > max_levels) return(NULL)

  tab <- table(x)
  lv  <- as.integer(names(tab))
  cnt <- as.integer(tab)
  n   <- length(x)
  mode_i <- which.max(cnt)

  # A level is "common" if it holds a non-trivial share of the data; "rare"
  # otherwise. Only used to decide whether a GAPPED level is an interior scale
  # level to bridge, or a detached contaminant (99, 33) to leave as a suspect.
  # A level counts as common at `common_frac` of the data, with an absolute
  # floor of 2 so a single stray value can never read as a real response level.
  common_floor <- max(common_frac * n, 2)
  is_common <- cnt >= common_floor

  # Grow a consecutive-integer core outward from the modal level. Bridge a single
  # missing interior level (a 1-7 scale where nobody picked 4 is still 1-7), but
  # stop when the next OCCURRING level is both rare and separated by a gap >= 2
  # (a detached contaminant), or when there is a gap of >= 3 (clearly not part of
  # the run).
  # The scale is the run of CONSECUTIVE occupied integer levels around the mode.
  # An ADJACENT occupied level (step 1) is always part of the scale, however
  # rare: a lone 6 next to a 1-5 core means the scale really goes to 6 and that
  # level was just rarely used — it is NOT a typo. A value beyond a GAP (a 99, a
  # mistyped 33, an 8 after 1-6) cannot be a quiet extension (there would be a
  # hole), so the core stops and that value becomes a suspect.
  #
  # Small interior gaps are bridged when both sides are common levels (a 1-7
  # scale showing {1,2,5,6,7} has an empty 3-4 but is still 1-7); a gap into a
  # rare far side is treated as the boundary.
  present <- lv                       # occupied levels, sorted
  hi <- lo <- lv[mode_i]
  extend <- function(dir) {
    repeat {
      cand <- if (dir > 0) present[present > hi] else present[present < lo]
      if (length(cand) == 0) break
      nextlv <- if (dir > 0) min(cand) else max(cand)
      gap    <- abs(nextlv - if (dir > 0) hi else lo)
      ni     <- which(lv == nextlv)
      if (gap == 1L) {                       # adjacent -> always extend
        if (dir > 0) hi <<- nextlv else lo <<- nextlv
      } else if (is_common[ni] && nextlv >= -11L && nextlv <= 11L) {
        # an interior gap (empty middle levels) to a COMMON far side that is
        # still inside the scale envelope: bridge it. A 1-7 scale showing
        # {1,2,5,6,7} bridges the empty 3-4. A detached rare contaminant (99,
        # 33) is NOT common, so it is never bridged.
        if (dir > 0) hi <<- nextlv else lo <<- nextlv
      } else break                            # gap into rare/detached -> stop
    }
  }
  extend(+1); extend(-1)

  core_levels <- lo:hi
  if (length(core_levels) < min_core) return(NULL)

  # Floor anchoring: scales start at 0 or 1. Infer as little as possible, and
  # record what we infer. If the observed floor is 2 or 3, snap down to the
  # natural start — 0 when a 0 is present anywhere, else 1 — but never below the
  # data's actual minimum-minus-a-little (we only fill the small gap to 0/1).
  floor_inferred <- integer(0)
  natural_floor <- if (0L %in% u) 0L else 1L
  if (lo > natural_floor && lo <= natural_floor + 2L &&
      natural_floor >= -11L) {
    floor_inferred <- setdiff(natural_floor:(lo - 1L), u)
    lo <- natural_floor
  }
  # Bipolar symmetry: if the core is symmetric-ish around 0 (a -k..k scale) keep
  # it as observed; no floor anchoring applies (natural_floor logic above only
  # fires for non-negative cores because natural_floor is 0/1).

  if (lo < -11L || hi > 11L) return(NULL)    # outside the scale envelope

  accepted <- lo:hi
  in_range <- x %in% accepted
  coverage <- mean(in_range)
  if (coverage < min_coverage) return(NULL)  # core doesn't explain the column

  suspects <- sort(unique(x[!in_range]))

  note <- {
    obs_lo <- min(u); obs_hi <- max(u)
    inf <- if (length(floor_inferred))
      sprintf("; inferred the unobserved floor value%s %s to make it a %d-based scale",
              plural(length(floor_inferred)),
              paste(floor_inferred, collapse = ", "), natural_floor) else ""
    sprintf("Detected a %d–%d rating scale (levels observed: %s%s).",
            lo, hi, paste(intersect(accepted, u), collapse = ", "), inf)
  }

  list(lo = lo, hi = hi,
       levels_present = intersect(accepted, u),
       coverage = coverage, suspects = suspects,
       floor_inferred = floor_inferred, note = note)
}

# data_check column types. The LLM-only refinements (ordinal/categorical for
# ambiguous integer columns) are not produced by the rules path; ambiguous
# columns fall back to continuous (numeric) or text (character).
.data_check_col_types <- c(
  "continuous", "binary", "categorical", "ordinal", "likert", "date", "id",
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
#' @returns a list with `col_type` (a value from `.data_check_col_types`, or
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

  # x_for_stats may hold non-numeric text (e.g. a coding sheet's long free-text
  # column pushed through here): as.numeric() would emit "NAs introduced by
  # coercion" — surfaced as `In FUN(X[[i]], ...)` because this runs inside
  # data_check's per-column lapply. The NAs are expected and discarded on the
  # next line, so the warning is pure noise; suppress it, matching every other
  # coercion of raw column values in this file.
  x <- suppressWarnings(as.numeric(x_for_stats))
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

# A short, human-readable slug used as BOTH the OSD `code` and the on-disk file
# name (scales/<slug>.osd). Lowercase words joined by underscores, from the scale
# NAME when it has one (PANAS -> "positive_and_negative_affect_schedule"), else
# from the column prefix/abbreviation (unnamed block -> "response"). Capped at a
# word boundary so the file name stays reasonable; the full name is kept in
# scale_info$name. Provenance is NOT encoded in the slug (it lives in
# metacheck$scale_source) — the slug is just a stable, readable identifier.
.osd_slug <- function(name = NULL, prefix = NULL, max_chars = 60L) {
  x <- if (!is.null(name) && !is.na(name) && nzchar(name)) name else prefix %||% ""
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) return("scale")
  if (nchar(x) > max_chars) {
    trunc <- substr(x, 1, max_chars)
    at <- regexpr("_[^_]*$", trunc)          # trim back to last full word
    if (at > 1) trunc <- substr(trunc, 1, at - 1L)
    x <- gsub("_+$", "", trunc)
  }
  if (!nzchar(x)) "scale" else x
}

# A code valid under the OpenScales OSD spec: uppercase letters, digits, and
# hyphens only. Capped at 40 characters (at a hyphen boundary where possible):
# an over-long "code" comes from a self-generated LLM label that is really a
# sentence, not an instrument name — the full text is kept in scale_info$name.
.osd_safe_code <- function(x, max_chars = 40L) {
  x <- toupper(gsub("[^A-Za-z0-9]+", "-", x %||% ""))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) return("SCALE")
  if (nchar(x) > max_chars) {
    trunc <- substr(x, 1, max_chars)
    at <- regexpr("-[^-]*$", trunc)          # trim back to last full token
    if (at > 1) trunc <- substr(trunc, 1, at - 1L)
    x <- gsub("-+$", "", trunc)
  }
  if (!nzchar(x)) "SCALE" else x
}

# Mint the OSD `code` and provenance for one identified scale. The `code` is a
# short, readable slug used as BOTH scale_info$code and the on-disk file name
# (scales/<code>.osd): the scale NAME when it has one (PANAS ->
# "positive_and_negative_affect_schedule"), else the column prefix (unnamed block
# -> "response"). Provenance is NOT encoded in the slug — it is carried in the
# returned `source` (dictionary / manuscript / self_generated / unnamed_block),
# which the .osd's metacheck block and the README record. Three levels of trust:
#   * dictionary     — matched a known instrument (OpenScales / curated).
#   * manuscript     — a real instrument named in the paper.
#   * self_generated — an LLM-inferred construct label, NOT a named instrument.
#   * unnamed_block  — a coherent same-prefix rating block, unnamed.
# `prefix` is the column abbreviation, used when the scale has no name. Shared by
# codebook_check (writing .osd files) and psychds-convert (cross-referencing
# variables to a scale code), so it lives here rather than in the module. Returns
# list(code, source, provenance).
.osd_code_and_provenance <- function(scale, prefix, scale_source, dict) {
  src <- scale_source %||% ""
  in_dict <- FALSE
  if (!is.na(scale) && nzchar(scale)) {
    i <- which(tolower(dict$name) == tolower(scale))
    if (length(i)) in_dict <- TRUE
  }
  # Slug from the name when present, else the column prefix. Same value regardless
  # of provenance — the slug is a readable identifier, not a trust marker.
  code <- .osd_slug(name = scale, prefix = prefix)

  if (in_dict) {
    list(code = code, source = "dictionary",
         provenance = "Matched a known instrument in metacheck's scale dictionary (OpenScales-derived or curated).")
  } else if (identical(src, "self_generated")) {
    list(code = code, source = "self_generated",
         provenance = "This label was GENERATED BY metacheck from the item wording. It is NOT a recognised named instrument, only metacheck's inference of what the items measure.")
  } else if (identical(src, "unnamed_block")) {
    list(code = code, source = "unnamed_block",
         provenance = "A coherent block of same-prefix rating columns detected in the data, but NOT named: neither a known instrument nor a construct metacheck could infer from the available text. Recorded for its structure (items + response scale) only.")
  } else {
    list(code = code, source = "manuscript",
         provenance = "A named instrument identified from the manuscript text but not present in the OpenScales registry.")
  }
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
# Does a character vector look like variable NAMES (short, no spaces, mostly
# alnum/underscore — neo1, BFI_3, q07)? Used by the positional layout detector.
.looks_like_varnames <- function(x) {
  x <- trimws(as.character(x)); x <- x[nzchar(x) & !is.na(x)]
  if (length(x) < 3) return(FALSE)
  ok <- grepl("^[A-Za-z][A-Za-z0-9_.]{0,30}$", x) & !grepl("\\s", x)
  mean(ok) >= 0.8 && length(unique(x)) >= 0.8 * length(x)   # mostly ids, mostly unique
}

# Does a character vector look like item WORDING (sentence-like: has spaces, some
# length, not all identical)?
.looks_like_wording <- function(x) {
  x <- trimws(as.character(x)); x <- x[nzchar(x) & !is.na(x)]
  if (length(x) < 3) return(FALSE)
  mean(grepl("\\s", x)) >= 0.6 && stats::median(nchar(x)) >= 8
}

# Positional codebook extractor for sheets/files with NO usable header row (a
# prose title instead of column names, e.g. an IPIP-NEO sheet whose columns are
# item-id | wording | anchor1 | anchor2 | ...). Scans for a column that looks
# like variable names with an adjacent column that looks like item wording; any
# further columns whose values look like "1 - Label" anchors are gathered into a
# value-labels code list. Returns a codebook-vars data.frame or NULL.
.extract_codebook_positional <- function(df, src) {
  if (is.null(df) || nrow(df) < 3 || ncol(df) < 2) return(NULL)
  raw <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  p <- ncol(raw)

  var_j <- NA_integer_; lab_j <- NA_integer_
  for (j in seq_len(p - 1L)) {
    if (.looks_like_varnames(raw[[j]]) && .looks_like_wording(raw[[j + 1L]])) {
      var_j <- j; lab_j <- j + 1L; break
    }
  }
  if (is.na(var_j)) return(NULL)

  keep <- nzchar(trimws(raw[[var_j]])) & !is.na(raw[[var_j]])
  rows <- raw[keep, , drop = FALSE]
  if (nrow(rows) < 3) return(NULL)

  # Anchor columns: columns after the label whose cells look like "N - Label".
  anchor_js <- integer(0)
  for (j in seq.int(lab_j + 1L, p)) {
    if (j > p) break
    vals <- trimws(rows[[j]]); vals <- vals[nzchar(vals) & !is.na(vals)]
    if (length(vals) && mean(grepl("^[0-9]+\\s*[-=:.)]", vals)) >= 0.6)
      anchor_js <- c(anchor_js, j)
  }
  value_labels <- rep(NA_character_, nrow(rows))
  if (length(anchor_js)) {
    # A scale block shares one anchor set; build it once from the first data row
    # that has anchors, as JSON {code: label}.
    for (i in seq_len(nrow(rows))) {
      cells <- trimws(as.character(rows[i, anchor_js]))
      cells <- cells[nzchar(cells) & !is.na(cells)]
      m <- regmatches(cells, regexec("^([0-9]+)\\s*[-=:.)]\\s*(.+)$", cells))
      codes <- vapply(m, function(z) if (length(z) == 3) z[2] else NA_character_, character(1))
      labs  <- vapply(m, function(z) if (length(z) == 3) trimws(z[3]) else NA_character_, character(1))
      ok <- !is.na(codes)
      if (any(ok))
        value_labels[i] <- .encode_value_labels(codes[ok], labs[ok])
    }
  }

  data.frame(
    codebook_variable = trimws(rows[[var_j]]),
    label             = trimws(rows[[lab_j]]),
    codebook_source   = src,
    group             = NA_character_,
    value_labels      = value_labels,
    missing_values    = vapply(value_labels, .missing_from_value_labels,
                               character(1), USE.NAMES = FALSE),
    question          = NA_character_,
    universe          = NA_character_
  )
}

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
          # Explore EVERY sheet, not just the first: a codebook often keeps its
          # scale item lists on separate tabs (e.g. an IPIP-NEO sheet beside a
          # general "Codebook" sheet). For each sheet try the named-header parser
          # first, then a positional fallback for sheets whose header is a prose
          # title rather than column names. Combine everything that parses; the
          # source records the sheet so a variable can be traced back.
          sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
          if (length(sheets) == 0) sheets <- NA_character_   # single default read
          parsed <- list()
          for (sh in sheets) {
            df <- tryCatch(
              if (is.na(sh)) as.data.frame(readxl::read_excel(path))
              else as.data.frame(readxl::read_excel(path, sheet = sh)),
              error = function(e) NULL)
            if (is.null(df)) next
            ssrc <- if (is.na(sh)) src else paste0(src, " [", sh, "]")
            one <- .extract_structured_codebook(df, ssrc)
            if (is.null(one)) one <- .extract_codebook_positional(df, ssrc)
            if (!is.null(one) && nrow(one) > 0) parsed[[length(parsed) + 1L]] <- one
          }
          if (length(parsed) > 0) dplyr::bind_rows(parsed) else NULL
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
      jasp = {
        # A .jasp carries its own variable/value labels (measurement level +
        # value coding), so it serves as its own codebook. read_jasp() attaches
        # haven-style attributes, so the SAME extractor used for .sav applies.
        df <- tryCatch(read_jasp(path)$data, error = function(e) NULL)
        if (is.null(df)) NULL else {
          res <- .extract_haven_labels(df, src)
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

# Conventional numeric codes that disguise missingness in shared data. These are
# only ever flagged when they sit OUTSIDE the column's real data (a scale's valid
# range, or far from the bulk) — the list nominates candidates; the "detached
# from the data" test in data_check_scale_values decides. So codes that are
# plausible real values (97 in a 0-100 score, 99 in an age) do not fire unless
# they are genuinely out of place.
#
# Three real-world families, scaled by field width:
#   - 9x-block: consecutive high codes for don't-know / refused / not-applicable
#     (memisc 97/98/99; SPSS defaults; many social-science surveys)
#   - repeated-digit 8- and 7-families (Statistics Canada, WVS: 8=DK, 7=skip)
#   - extreme repeated placeholders at wide widths
# Deliberately EXCLUDED: single digits 7/8/9 (valid Likert points — the scale
# detector catches an out-of-range 9 in context), -1 (very often a legitimate
# score, e.g. a difference score or a bipolar scale point), and single-digit
# negatives -7/-8/-9 (legitimate values on bipolar -k..k rating scales).
# Only -99 and -999 are kept from the negative family: they are the two forms
# actually attested as common user codings (SPSS/Stata guidance); the wider and
# 9x/8x negative variants had no source and are omitted.
.data_missing_sentinels <- c(
  # 9x-block (don't know / refused / not applicable)
  97, 98, 99,
  997, 998, 999,
  9997, 9998, 9999,
  99997, 99998, 99999,
  # repeated-digit 8- and 7-families
  88, 888, 8888, 88888,
  77, 777, 7777, 77777,
  # extreme repeated placeholders at wide field widths
  999999, 888888, 777777, 99999999,
  # the two attested negative codings
  -99, -999
)

# Could an out-of-scale value `v` be a keying TYPO of an in-scale value? Returns
# the most plausible intended value (inside [lo, hi]) or NA. Covers the common
# fat-finger patterns: a repeated digit (33 -> 3, 55 -> 5), a doubled/trailing
# digit (25 -> 2 or 5), a dropped/added minus, or an extra leading digit.
.scale_typo_of <- function(v, lo, hi) {
  if (is.na(v) || v %in% lo:hi) return(NA_integer_)
  cand <- integer(0)
  av <- abs(v)
  ds <- strsplit(as.character(av), "")[[1]]
  if (length(ds) >= 2) {
    # each single digit (33->3, 25->2 or 5, 105->1/0/5)
    cand <- c(cand, as.integer(ds))
    # drop the leading digit (25 -> 5, 105 -> 5), drop the trailing (25 -> 2)
    cand <- c(cand, as.integer(substring(as.character(av), 2)),
              as.integer(substring(as.character(av), 1, nchar(as.character(av)) - 1)))
  }
  # sign flip (a -3 typed on a 1..7 scale, or a 3 that should be -3 on a bipolar)
  cand <- c(cand, -v)
  cand <- unique(cand[!is.na(cand)])
  inside <- cand[cand >= lo & cand <= hi]
  if (!length(inside)) return(NA_integer_)
  # prefer the candidate closest to v's magnitude order (single digit of v)
  inside[which.min(abs(inside - (av %% 10)))]
}

#' Flag values that fall outside a rating scale's valid range
#'
#' A rating scale (Likert / rating item) has a small set of consecutive valid
#' integer levels. Any value outside that set is a data problem, and this check
#' both flags it and, for each value, offers the most likely explanation:
#' \itemize{
#'   \item a **missing-data code** left as a number (a `-99` / `999` in the
#'     sentinel list, or a codebook-**declared** missing code) — recode to `NA`;
#'   \item a **keying typo** of an in-scale value (a `33` for `3`, a `55` for
#'     `5`) — the probable intended value is named;
#'   \item otherwise an **unexplained** out-of-range value to review.
#' }
#' The valid range is ground truth when `valid_values` / `valid_range` are
#' supplied (e.g. from a codebook), otherwise inferred by `.detect_likert_scale`.
#' A column that is not a rating scale (continuous, many-level, non-integer, too
#' few rows) has no fixed range and is not flagged here — unbounded variables
#' (age, reaction time) have no principled "valid range" to violate.
#'
#' This unifies the former `data_check_out_of_range` and
#' `data_check_miscoded_missing`: one detector run, one finding per column.
#'
#' @param x a numeric vector
#' @param sentinels candidate missing-data sentinel codes
#' @param declared optional codebook-declared missing codes (ground truth)
#' @param valid_values optional enumerated valid codes (ground truth)
#' @param valid_range optional `c(lo, hi)` valid range (ground truth)
#' @param n_max max number of values to list in the message
#' @returns list(problem, message, values, lower, upper, classes) where `classes`
#'   labels each flagged value "missing", "typo:<intended>", or "unexplained"
#' @export
#' @keywords internal
data_check_scale_values <- function(x, sentinels = .data_missing_sentinels,
                                    declared = NULL, valid_values = NULL,
                                    valid_range = NULL, n_max = 10) {
  none <- list(problem = FALSE, message = "", values = NULL,
               lower = NA_real_, upper = NA_real_, classes = character(0))
  if (!is.numeric(x)) return(none)
  xv <- x[!is.na(x) & !is.nan(x) & is.finite(x)]
  if (length(xv) == 0) return(none)

  # ── Establish the valid range: ground truth, else inferred scale ────────────
  if (!is.null(valid_values) && length(valid_values)) {
    vv <- sort(unique(as.numeric(valid_values)))
    vv <- vv[is.finite(vv)]
    if (length(vv) == 0) return(none)

    # Some codebooks carry only endpoints (e.g., 1 and 9) for a bounded
    # rating scale. If interior integer values are actually present, interpret
    # that as a contiguous scale range rather than two literal valid codes.
    vv_int <- all(vv == round(vv))
    if (vv_int && length(vv) == 2L && diff(vv) >= 2L) {
      x_int <- sort(unique(xv[xv == round(xv)]))
      has_interior <- any(x_int > vv[1] & x_int < vv[2])
      valid_set <- if (has_interior) seq.int(vv[1], vv[2]) else vv
    } else {
      valid_set <- vv
    }

    lo <- min(valid_set); hi <- max(valid_set)

    # For contiguous non-negative rating scales, anchor a sparsely observed
    # floor to the natural start (0 when 0 is observed, otherwise 1).
    is_contig_int <- all(valid_set == round(valid_set)) &&
      length(valid_set) >= 2L && all(diff(valid_set) == 1)
    if (is_contig_int && lo >= 0) {
      x_int <- sort(unique(xv[xv == round(xv)]))
      natural_floor <- if (0 %in% x_int) 0 else 1
      if (lo > natural_floor && lo <= natural_floor + 2L) {
        lo <- natural_floor
        valid_set <- seq.int(lo, hi)
      }
    }
  } else if (!is.null(valid_range) && length(valid_range) == 2 &&
             all(is.finite(valid_range))) {
    lo <- min(valid_range); hi <- max(valid_range)
    valid_set <- lo:hi
  } else {
    sc <- .detect_likert_scale(xv)
    if (is.null(sc)) return(none)          # not a scale -> no range to violate
    lo <- sc$lo; hi <- sc$hi
    valid_set <- lo:hi
  }

  out <- sort(unique(xv[!(xv %in% valid_set)]))
  if (length(out) == 0)
    return(list(problem = FALSE, message = "", values = NULL,
                lower = lo, upper = hi, classes = character(0)))

  # ── Classify each out-of-scale value ────────────────────────────────────────
  declared_num <- if (!is.null(declared)) as.numeric(declared) else numeric(0)
  classify <- function(v) {
    if (v %in% declared_num) return("missing")
    if (v %in% sentinels)    return("missing")
    typo <- .scale_typo_of(v, lo, hi)
    if (!is.na(typo))        return(paste0("typo:", typo))
    "unexplained"
  }
  classes <- vapply(out, classify, character(1))

  describe <- function(v, cls) {
    if (cls == "missing") sprintf("%s (looks like a missing-data code → recode to NA)", v)
    else if (startsWith(cls, "typo:"))
      sprintf("%s (looks like a typo of %s)", v, sub("^typo:", "", cls))
    else sprintf("%s (outside the scale, cause unclear)", v)
  }
  shown_i <- seq_len(min(length(out), n_max))
  parts <- vapply(shown_i, function(i) describe(out[i], classes[i]), character(1))
  msg <- sprintf(
    "%d value%s outside the %d–%d scale: %s%s",
    length(out), plural(length(out)), lo, hi,
    paste(parts, collapse = ", "),
    if (length(out) > n_max) ", ..." else "")
  list(problem = TRUE, message = msg, values = out,
       lower = lo, upper = hi, classes = classes)
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
  if (length(x) == 0)
    return(list(problem = FALSE, message = "", values = NULL, near = FALSE))
  tab <- sort(table(x), decreasing = TRUE)
  top_frac <- tab[[1]] / length(x)
  if (length(tab) == 1)
    return(list(problem = TRUE,
                message = sprintf("Column is constant: every value is \"%s\".",
                                  names(tab)[1]),
                values = names(tab)[1], near = FALSE))
  if (top_frac >= threshold)
    return(list(problem = TRUE,
                message = sprintf("Near-constant: %.0f%% of values are \"%s\".",
                                 100 * top_frac, names(tab)[1]),
                values = names(tab)[1], near = TRUE))
  list(problem = FALSE, message = "", values = NULL, near = FALSE)
}

#' Flag a column with no observed values
#'
#' All values are NA (or, for text, blank/whitespace-only). Such a column
#' usually means a variable that never recorded anything or an export
#' artifact, and it is invisible to [data_check_constant()] which strips NAs.
#'
#' @param x a vector
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_empty <- function(x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  n <- length(x)
  if (n == 0) return(none)
  filled <- if (is.numeric(x)) !is.na(x) else
    !is.na(x) & nzchar(trimws(as.character(x)))
  if (any(filled)) return(none)
  list(problem = TRUE,
       message = sprintf("Column is empty: all %d value%s %s missing.",
                         n, plural(n), if (n == 1) "is" else "are"),
       values = NULL)
}

#' Does a column name look like an experimental design variable?
#'
#' Matches names built from design/condition tokens (condition, group,
#' treatment, arm, dose, manipulation, intervention), requiring a word
#' boundary so e.g. "charm" does not match "arm". Used to decide whether a
#' constant column is suspicious: a design variable with one value suggests
#' the file was filtered to a single condition before export.
#'
#' @param col a column name
#' @returns logical
#' @export
#' @keywords internal
data_check_design_name <- function(col) {
  grepl("(?i)(^|[._ -])(cond(ition)?|grp|group|treat(ment)?|arm|dose|manip(ulation)?|intervention)([._ -]|[0-9]|$)",
        col, perl = TRUE)
}

#' Flag an SPSS "Select Cases" filter variable
#'
#' SPSS's Select Cases dialog creates a 0/1 variable named `filter_$`
#' (mangled to `filter_.` or `filter_` by some importers). Its presence
#' matters to a re-user either way: if it is constant at 1 the file was
#' saved after deleting unselected cases, so the shared data are a
#' pre-filtered subset; if it varies, the reported analyses likely used only
#' the selected rows and the filter must be re-applied to reproduce them.
#'
#' @param col the column name
#' @param x the column's values
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_spss_filter <- function(col, x) {
  none <- list(problem = FALSE, message = "", values = NULL)
  if (!grepl("(?i)^filter_[$._]?$", col, perl = TRUE)) return(none)
  v <- suppressWarnings(as.numeric(x))
  v <- v[!is.na(v)]
  if (length(v) == 0) return(none)
  n_sel <- sum(v == 1)
  msg <- if (n_sel == length(v)) {
    "SPSS \"Select Cases\" filter variable: every row is selected (value 1), so the file appears to have been saved after deleting unselected cases — the shared data are a pre-filtered subset of what was collected."
  } else {
    sprintf("SPSS \"Select Cases\" filter variable: %d of %d rows are selected (value 1). The reported analyses likely used only the selected rows; re-apply this filter to reproduce them.",
            n_sel, length(v))
  }
  list(problem = TRUE, message = msg,
       values = c(selected = n_sel, total = length(v)))
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

#' Flag a problematic column name
#'
#' Column names travel: they become variable names in analysis scripts, chunk
#' labels and figure file names in generated codebooks, and keys in metadata
#' files. A name that contains characters that are illegal in file names
#' (`< > : " / \ | ? *`), control characters (tabs, newlines), leading/trailing
#' whitespace, or that runs to hundreds of characters cannot be used in those
#' places without modification — tools either fail (e.g. a figure file cannot
#' be created on Windows) or silently rename the variable so it no longer
#' matches the shared data. Good practice is short names built from letters,
#' digits and underscores.
#'
#' Such names typically signal an upstream problem: a file whose header was not
#' parsed as intended (e.g. a whole header line captured as a single "name"),
#' or export settings that leaked formatting into the header.
#'
#' The length threshold is not arbitrary: 64 bytes is the maximum variable-name
#' length SPSS supports
#' (<https://www.ibm.com/docs/en/spss-statistics/32.0.0?topic=view-variable-names>),
#' and SAS and Stata cap names at 32 characters
#' (<https://www.stata.com/manuals/rlimits.pdf>), so a name over 64 characters
#' cannot be imported into any of the three major statistical packages without
#' being renamed — after which it no longer matches the shared data or its
#' documentation. (DDI-Codebook's `var@name` documentation still notes names
#' are "usually up to eight characters, following the rules of SAS and SPSS" —
#' a legacy of those systems' old limits, not a modern recommendation, so DDI
#' imposes no constraint of its own.)
#'
#' This check only warns; nothing is renamed or dropped. ([convert_codebook()]
#' separately excludes columns whose name would push a generated figure's file
#' path past Windows' 260-character limit — the one case where a name makes
#' rendering impossible; that budget depends on the output path, so it is
#' computed there, not here.)
#'
#' @param col_name the column name
#' @param max_chars names longer than this are flagged as excessively long;
#'   the default is SPSS's 64-byte maximum variable-name length (see Details)
#' @returns list(problem, message, values)
#' @export
#' @keywords internal
data_check_colname <- function(col_name, max_chars = 64L) {
  none <- list(problem = FALSE, message = "", values = NULL)
  nm <- as.character(col_name)
  if (length(nm) != 1 || is.na(nm)) return(none)

  issues <- character(0)
  # Characters that are illegal in Windows file names (and unsafe everywhere).
  illegal <- regmatches(nm, gregexpr('[<>:"/\\\\|?*]', nm))[[1]]
  # Control characters (tab, newline, carriage return, ...).
  ctrl <- regmatches(nm, gregexpr("[[:cntrl:]]", nm))[[1]]
  if (length(illegal) > 0)
    issues <- c(issues, sprintf("characters not allowed in file names (%s)",
                                paste(unique(sprintf('"%s"', illegal)),
                                      collapse = ", ")))
  if (length(ctrl) > 0)
    issues <- c(issues, sprintf("%d control character%s (tab/newline)",
                                length(ctrl), plural(length(ctrl))))
  if (nm != trimws(nm))
    issues <- c(issues, "leading/trailing whitespace")
  if (nchar(nm) > max_chars)
    issues <- c(issues, sprintf(
      "a length of %d characters (SPSS allows at most 64, SAS and Stata 32, so this name cannot be imported there without renaming)",
      nchar(nm)))
  if (length(issues) == 0) return(none)

  bad_chars <- unique(c(illegal, ctrl))
  list(problem = TRUE,
       message = sprintf(
         "Column name has %s. Such names break when reused as file names or in code; prefer short names of letters, digits and underscores.%s",
         paste(issues, collapse = "; "),
         if (length(bad_chars) > 0)
           " A name like this can also mean the file's header was not parsed as intended." else ""),
       values = if (length(bad_chars) > 0) bad_chars else NULL)
}

#' Flag column names that collide after sanitization
#'
#' Many tools replace the special characters in a variable name with `_` or
#' drop them: R's `make.names()`, SPSS/SAS/Stata on import, and generated
#' codebooks (section ids, figure file names). Two columns whose names differ
#' *only* in special characters — e.g. the phoneme symbols `t'` and a
#' t-with-diacritic, which both sanitize to `t_` — therefore become
#' indistinguishable the moment the data leave the original file, and links or
#' merged results silently point at the wrong variable. Identical duplicate
#' names collide trivially and are flagged too.
#'
#' The sanitization mirrored here (every character that is not a Unicode
#' letter or digit becomes `_`) is the one the codebook package uses for its
#' section ids, where collisions surface as pandoc "Duplicate identifier"
#' warnings.
#'
#' @param col_names character vector of a file's column names
#' @returns a named list mapping each colliding column name to a message
#'   (empty list when all names stay distinct)
#' @export
#' @keywords internal
data_check_colname_collisions <- function(col_names) {
  nms <- as.character(col_names)
  key <- gsub("[^\\p{L}\\p{N}]", "_", nms, perl = TRUE)
  out <- list()
  for (k in unique(key[duplicated(key)])) {
    members <- nms[key == k]
    for (i in which(key == k)) {
      others <- unique(members[members != nms[i]])
      out[[nms[i]]] <- sprintf(
        "Column name becomes \"%s\" when special characters are replaced, the same as %s: tools that sanitize names (R's make.names(), SPSS/SAS/Stata import, codebook section links) cannot tell these columns apart.",
        k,
        if (length(others) == 0)
          sprintf("%d other identically named column%s",
                  sum(members == nms[i]) - 1L,
                  plural(sum(members == nms[i]) - 1L))
        else
          paste(sprintf('"%s"', utils::head(others, 5)), collapse = ", "))
    }
  }
  out
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
#' `.demographic_values_ok`), which keeps false positives low: a `condition`
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

# Thresholds for the "is this a usable rectangular dataset?" test below. Named
# so they can be tuned as the check is run on more repositories. Derived from a
# set of human-coded qualitative worksheets (prose columns ~45-50%, missingness
# ~90%+) versus their study's real dataset (~3% prose, ~2% missing).
.tabular_prose_high  <- 0.70   # overwhelmingly free-text → not a dataset on its own
.tabular_prose_mid   <- 0.40   # the coding-sheet middle ground: both must hold
.tabular_miss_mid    <- 0.40   # (missingness only corroborates prose; never alone)

#' Is a read-in data frame a usable rectangular dataset?
#'
#' `readxl`/`read.delim` will happily read a human-formatted coding worksheet
#' (interleaved "Code N: description" free-text columns + sparse 0/1 indicators,
#' summary/legend rows at the bottom) into a data frame — but the result is not a
#' rectangular *dataset*: most "columns" are prose annotations and most cells are
#' structurally empty. Extracting columns from such a file yields junk (the
#' "Code"-prefixed columns) and sending them to the LLM wastes calls on non-data.
#'
#' This detects that case from facets `data_check` has already computed, using
#' two signals combined with tiered rules (not a single AND-gate, so an extreme
#' value on one axis can exclude a file on its own):
#'
#' * **prose fraction** — share of columns that are free text: `representation`
#'   is `"text"`, the `concept` is not a recognised text kind (id/date/timestamp),
#'   and the column is high-cardinality (distinct/non-missing > 0.5), i.e. a
#'   genuine free-text field, not a small set of category labels.
#' * **missingness fraction** — share of columns that are >50% missing.
#'
#' Exclude when the file is almost entirely empty, or overwhelmingly free text,
#' or moderately both. A file with an ordinary structure (a few open-response
#' columns, or a legitimately sparse but numeric design) trips none of these.
#'
#' @param facets a list of per-column facet lists, as produced by
#'   [data_col_facets()] (one per column, in column order).
#' @param df the read-in data frame the facets describe.
#'
#' @returns a list with `usable` (logical) and, when `FALSE`, a human-readable
#'   `reason` naming the signals that fired.
#' @export
#' @keywords internal
.tabular_usable <- function(facets, df) {
  p <- length(facets)
  if (p == 0 || is.null(df) || nrow(df) == 0)
    return(list(usable = FALSE, reason = "the file has no data rows or columns"))

  is_prose <- vapply(seq_len(p), function(j) {
    f <- facets[[j]]
    if (!identical(f$representation, "text")) return(FALSE)
    if (isTRUE(f$concept %in% c("id", "date", "timestamp"))) return(FALSE)
    x <- df[[j]]
    nonNA <- x[!is.na(x)]
    length(nonNA) > 0 && (length(unique(nonNA)) / length(nonNA)) > 0.5
  }, logical(1))

  miss_hi <- vapply(seq_len(p), function(j) mean(is.na(df[[j]])) > 0.5, logical(1))

  prose_frac <- mean(is_prose)
  miss_frac  <- mean(miss_hi)
  pct <- function(x) sprintf("%.0f%%", 100 * x)

  # Tiered rules. NOTE: high missingness ALONE does NOT exclude. Legitimate
  # branched / planned-missing surveys (e.g. a Qualtrics export where each
  # respondent sees only their condition's questions) are 90%+ missing but are
  # real NUMERIC data — excluding them drops the actual dataset (and its scales).
  # We exclude only when the file is overwhelmingly FREE TEXT, or moderately free
  # text AND mostly empty (the human coding-worksheet signature). Missingness is
  # corroborating, never sufficient.
  if (prose_frac >= .tabular_prose_high)
    return(list(usable = FALSE, reason = sprintf(
      "%s of columns are free text, not variables", pct(prose_frac))))
  if (prose_frac >= .tabular_prose_mid && miss_frac >= .tabular_miss_mid)
    return(list(usable = FALSE, reason = sprintf(
      "%s of columns are free text and %s are mostly empty — this looks like a coding worksheet, not a rectangular dataset",
      pct(prose_frac), pct(miss_frac))))

  list(usable = TRUE, reason = NA_character_)
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
#' `.qualtrics_is_header_row`) and coerces columns that are now fully numeric
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

# Minimum items for a block to count as a scale. Set to 3 to catch genuine short
# scales (e.g. 3-item subscales) at the cost of a few more noisy small fragments;
# the dictionary/LLM naming stage filters those out (an un-nameable group stays
# unnamed rather than becoming a false scale). Shared by scale grouping and the
# careless-responding detector (.dv_careless_min_items).
.scale_min_items <- 3L

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
    if (length(xn) == 0) return(FALSE)
    na_frac <- mean(is.na(xn))
    if (!is.finite(na_frac) || na_frac > 0.2) return(FALSE)
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

# Is a prefix group a RATING-LIKE block, judged from data_check's per-column
# statistics (no file re-read)? This is broader than `.detect_scale_blocks`,
# which only accepts small-integer Likert items and so misses 0-100 slider /
# percentage rating scales (values like 11, 95, 71). It exists to gate what the
# OSD exporter is allowed to WRITE — named or unnamed — so that a coherent rating
# block is kept while genuine non-scales (probabilities, model parameters) are
# rejected.
#
# A block qualifies when, pooled across its columns:
#   * at least 60% of its columns are numeric (a scale block is numeric ratings,
#     not free text / ids);
#   * the pooled minimum is >= -1 — rejects unbounded model parameters that go
#     negative (e.g. alpha/beta weights spanning -52 .. +10);
#   * the pooled maximum is > 1 — rejects [0,1] quantities (probabilities,
#     posterior means) that are NOT ratings, and
#   * the pooled maximum is <= 100 — the upper bound of a plausible rating
#     envelope (0-100 sliders, 1-7 Likert, 0-10 scales all pass; a summed total
#     or a count that runs into the hundreds does not).
# `cols` are the block's column names; `source_file` scopes the lookup so a
# same-named column in another file is not mixed in.
.scale_block_is_ratinglike <- function(cols, source_file, columns_df) {
  if (is.null(columns_df) || !nrow(columns_df) ||
      !all(c("source_file", "column_name", "min", "max") %in% names(columns_df)))
    return(FALSE)
  key  <- paste(columns_df$source_file, columns_df$column_name, sep = "\x01")
  want <- paste(source_file, cols, sep = "\x01")
  idx  <- which(key %in% want)
  if (length(idx) < .scale_min_items) return(FALSE)

  mn <- suppressWarnings(as.numeric(columns_df$min[idx]))
  mx <- suppressWarnings(as.numeric(columns_df$max[idx]))
  numeric_frac <- mean(is.finite(mn) & is.finite(mx))
  if (!is.finite(numeric_frac) || numeric_frac < 0.6) return(FALSE)

  lo <- suppressWarnings(min(mn, na.rm = TRUE))
  hi <- suppressWarnings(max(mx, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi)) return(FALSE)
  lo >= -1 && hi > 1 && hi <= 100
}
