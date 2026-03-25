# 0_index.R
# ─────────────────────────────────────────────────────────────────────────────
# Full pipeline: download → unpack → understand structure → classify files
#                → extract columns + sample values → save
#
# Exports: run_index(paper_id = NA)
#
# Input:  paper_id (character, or NA to pick randomly)
# Output: list with pipeline results (see return value at bottom of function)
#         data_check/structure/<paper_id>_structure.csv   (one row per file)
#         data_check/structure/<paper_id>_columns.csv     (one row per column)
# ─────────────────────────────────────────────────────────────────────────────

library(metacheck)
source("data_check/pipeline/helper.R")

llm_use(TRUE)
llm_model("ollama/gpt-oss:20b-cloud")

# ── Constants ─────────────────────────────────────────────────────────────────

DATA_DIR        <- "./data_check/data"
OUTPUT_DIR      <- "./data_check/outputs"
ARCHIVE_EXTS    <- c("zip", "gz", "tar", "tgz", "bz2", "xz")
# Extension-based type overrides applied after aggregate sentinel expansion.
# Maps lowercase file extension → definitive type for unambiguous file kinds.
# Extensions absent from this map (e.g. txt, dat, rda) retain the sentinel's
# inherited type unchanged.
AGGREGATE_EXT_OVERRIDE <- c(
  r = "code", rmd = "code", qmd = "code", py = "code", m = "code",
  do = "code", sps = "code", jl = "code", js = "code", sh = "code",
  bash = "code", pl = "code", rb = "code", cpp = "code", c = "code",
  h = "code", java = "code", scala = "code", sql = "code",
  jpg = "asset", jpeg = "asset", png = "asset", gif = "asset",
  bmp = "asset", tiff = "asset", tif = "asset", svg = "asset",
  mp4 = "asset", avi = "asset", mov = "asset", mp3 = "asset",
  wav = "asset", flac = "asset",
  csv = "data", sav = "data", dta = "data", sas7bdat = "data",
  xlsx = "data", xls = "data", rds = "data"
)
LLM_BATCH_SIZE  <- 20
N_DATA_READ     <- 5
MAX_TOTAL_DATA_MB <- 10 * 1024  # 10 GB total data read cap per paper across all data files
MAX_FILE_READ_SEC <- 5 * 60    # per-file read timeout (seconds); file is skipped if exceeded
VALID_COL_TYPES <- c("continuous", "binary", "categorical", "ordinal", "date", "id",
                     "text", "continuous_comma_decimal", "continuous_outliers_excluded",
                     "empty", "unknown")
MAX_COL_TYPE_LLM_CALLS <- 5L
# Folders with more than this many files are treated as aggregate datasets
AGGREGATE_THRESHOLD <- 50
# Max rows to scan below row 1 for a usable sub-header in multi-level CSV files
MULTILEVEL_HEADER_LOOKAHEAD <- 3L
# Directory names longer than this many words are truncated; spaces → underscores
MAX_DIR_WORDS   <- 5

# Local repository of more xmls. Remove to fallback to psychsci.
XML_DIR <- "/Volumes/Models/expanded_xml" #"./data-raw/psychsci/grobid_0.8.2-full"

BADGE_REPOS <- c("tvyxz", "osf.io/tvyxz/", "osf.io/tvyxz")

STRUCTURE_PROMPT <- 'You are analysing a psychology research data repository.
You will receive a file tree. For each path return a JSON array (same order).
Each element: {"path": "<exact path>", "type": "<type>", "group": "<group>"}

type — pick one:
  data         : tabular data file intended for statistical analysis (rows = observations)
  codebook     : variable descriptions / data dictionary / key
  code         : analysis or processing script
  supplemental : supporting materials that are NOT raw data — includes survey
                 instruments (e.g. .qsf Qualtrics files), questionnaire PDFs,
                 scale items, consent forms, syntax/output files showing results,
                 preregistrations, supporting info appendices, saved plot objects
                 (e.g. .Rdata/.rda files containing ggplot/plot objects), and any
                 file labelled "supplemental/supporting"
  doc          : manuscript, report, general notes, project proposals, changelogs
  readme       : readme file
  asset        : image, audio, video, stimulus material used in the study
  other        : anything that does not fit above

group — pick one:
  "ex<N>"     : belongs to a main numbered experiment/study (e.g. "ex1", "ex2")
                Infer from folder name OR filename (e.g. "Study 2.csv" → "ex2")
                If the study has a letter suffix (e.g. "Study 4a", "Experiment 1b"),
                preserve it exactly: "ex4a", "ex1b". NEVER collapse "4a"/"4b" → "4".
  "pilot<N>"  : belongs to a pilot study (e.g. "pilot1", "pilot2")
                Same letter-suffix rule applies: "Pilot 1a" → "pilot1a".
                Use this whenever the folder or filename contains "pilot", "pre-pilot",
                "prepilot", or "preliminary study" — even if it also has a number.
                Number pilots independently from experiments (pilot1, pilot2, …).
  "other"     : not tied to a specific numbered experiment or pilot (shared files,
                meta-analyses, previous versions, project proposals, etc.)
  "na"        : type is readme, asset, supplemental, or other — group not applicable

Rules:
- "Supplemental Experiment N" or "Supplemental Study N" folders are NOT main
  experiments. Files inside them get group "other", NOT "ex<N>".
  The word "Supplemental" before "Experiment/Study" overrides the number.
- Pilots are NEVER "ex<N>" — if something is a pilot it is always "pilot<N>".
- Number pilots and experiments independently: a repo can have ex1, ex2, pilot1.
- Preserve letter suffixes exactly as written: "4a" stays "4a", never becomes "4".
- "Previous versions" and archive folders → type of their contents, group "other"
- Daily training verbatim scripts (e.g. "D23_verbatims.docx") → supplemental
- Syntax and output files (e.g. SPSS .sps, HTML output) → supplemental
- Sentinel paths like "[236_files.csv]" represent many identical files in that
  folder — classify the folder as a whole.
- You MUST echo back the exact path string provided. NEVER shorten, truncate,
  or abbreviate paths with "..." or any other placeholder. Every character of
  every path must appear verbatim in the output.
- Output ONLY the JSON array. Do not add any notes, comments, or explanatory
  text before or after the array.'

COLUMN_TYPE_PROMPT <- 'You are classifying columns in psychology research data.
For each column descriptor return a JSON array (same order).
Each element: {"descriptor": "<exact descriptor>", "col_type": "<type>"}

col_type — pick one:
  continuous  : numeric measurement — reaction time, age, VAS rating (0–10), Likert-scale
                mean, subscale score, count, percentage, any column with decimal values
  ordinal     : ordered integer scale with few levels — 1–5 Likert item, 1–10 attention
                rating, bounded compliance or distress score, ranked preference, grade
  categorical : unordered group or category code with few levels (condition, gender, language)
  binary      : exactly two possible values (yes/no, 0/1, treatment/control)
  id          : row or participant identifier — unique or nearly-unique integer per row
  unknown     : ONLY when name AND values together give no numeric signal — e.g. fully
                redacted data, meaningless all-constant codes. Do NOT use for any column
                whose samples look like numbers.

IMPORTANT: Prefer "continuous" or "ordinal" over "unknown". When in doubt between
"continuous" and "ordinal" for a numeric column, choose "continuous".

Output ONLY the JSON array. No notes, no text outside the array.'

# ── Pipeline function ─────────────────────────────────────────────────────────

run_index <- function(paper_id = NA, download = TRUE, output_dir = NULL) {

  t_start <- proc.time()[["elapsed"]]

  # ── 0. Resolve paper ────────────────────────────────────────────────────────

  if (is.na(paper_id)) {
    xml_files <- list.files(XML_DIR, pattern = "\\.xml$", full.names = FALSE)
    if (length(xml_files) == 0) stop("No XML files found in ", XML_DIR)
    paper_id  <- tools::file_path_sans_ext(sample(xml_files, 1))
    message("── Randomly selected paper: ", paper_id)
  }

  eff_dir <- if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_dir
  } else paper_output_dir(paper_id)

  xml_path <- file.path(XML_DIR, paste0(paper_id, ".xml"))
  paper    <- read(xml_path)
  stopifnot(!is.null(paper$id))

  target_dir <- file.path(DATA_DIR, paper_id)

  # ── 1. Download ─────────────────────────────────────────────────────────────

  t_download_start <- proc.time()[["elapsed"]]
  if (download) {
    links        <- osf_links(paper)
    unique_links <- setdiff(unique(links$text), BADGE_REPOS)

    if (length(unique_links) == 0) {
      stop("no_links: paper ", paper_id, " has no OSF data links")
    }

    if (!dir.exists(target_dir)) {
      osf_file_download(unique_links, download_to = target_dir,
                        max_download_size = 10e9, max_file_size = NULL)
    }
  }

  # ── 2. Sanitize directory names ─────────────────────────────────────────────

  sanitize_name <- function(name) {
    words <- strsplit(trimws(name), "\\s+")[[1]]
    words <- gsub("[^A-Za-z0-9_\\-]", "", words)  # strip ; : ? and other special chars
    words <- words[nchar(words) > 0]
    paste(head(words, MAX_DIR_WORDS), collapse = "_")
  }

  if (dir.exists(target_dir)) {
    all_dirs <- list.dirs(target_dir, full.names = TRUE, recursive = TRUE)
    all_dirs <- all_dirs[all_dirs != target_dir]

    for (i in seq_along(all_dirs)) {
      d <- all_dirs[i]
      if (!dir.exists(d)) next
      dname <- basename(d)
      if (!grepl("[^A-Za-z0-9_.\\-]", dname)) next  # skip if already clean
      new_name <- sanitize_name(dname)
      new_path <- file.path(dirname(d), new_name)
      if (new_path != d && !file.exists(new_path)) {
        file.rename(d, new_path)
        old_prefix <- paste0(d, "/")
        new_prefix <- paste0(new_path, "/")
        needs_update <- startsWith(all_dirs, old_prefix)
        all_dirs[needs_update] <- paste0(new_prefix,
          substr(all_dirs[needs_update], nchar(old_prefix) + 1L, nchar(all_dirs[needs_update])))
      }
    }
  }

  # ── 3. Unpack archives ──────────────────────────────────────────────────────

  # Helper: strip .git internals — repos sometimes include git-tracked task
  # scripts whose .git/ objects would otherwise flood the file list with
  # hundreds of meaningless SHA blobs.
  drop_git <- function(paths) paths[!grepl("(^|/)\\.git(/|$)", paths, perl = TRUE)]

  files <- if (dir.exists(target_dir)) {
    drop_git(list.files(target_dir, full.names = TRUE, recursive = TRUE))
  } else {
    character(0)
  }

  if (length(files) == 0) {
    if (!dir.exists(target_dir)) {
      stop("download_failed: OSF download produced no directory for paper ", paper_id)
    } else {
      stop("empty_repo: directory exists but contains no files for paper ", paper_id)
    }
  }

  archive_paths <- files[tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS]

  if (length(archive_paths) > 0) {
    message("── Unpacking ", length(archive_paths), " archive(s)")
    lapply(archive_paths, unpack_archive)
    files <- drop_git(list.files(target_dir, full.names = TRUE, recursive = TRUE))
    files <- files[!(tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS)]
  }

  # ── 3b. Explode multi-sheet Excel files into per-sheet CSVs ─────────────────
  # Each sheet becomes <stem>_<sheet_name>.csv alongside the original.
  # The original xlsx/xls is then deleted so downstream sees only flat CSVs.

  excel_paths <- files[tolower(tools::file_ext(files)) %in% c("xlsx", "xls")]
  if (length(excel_paths) > 0) {
    for (xl in excel_paths) {
      sheets <- tryCatch(readxl::excel_sheets(xl), error = function(e) {
        warning("Could not read sheets from ", basename(xl), ": ", conditionMessage(e))
        character(0)
      })
      if (length(sheets) == 0) next
      stem    <- tools::file_path_sans_ext(xl)
      n_written <- 0L
      for (sh in sheets) {
        df <- tryCatch(
          as.data.frame(readxl::read_excel(xl, sheet = sh), stringsAsFactors = FALSE),
          error = function(e) {
            warning("  skipping sheet '", sh, "' in ", basename(xl),
                    ": ", conditionMessage(e))
            NULL
          }
        )
        if (is.null(df) || nrow(df) == 0) next
        # Sanitize sheet name for use in a filename (replace path-unsafe chars)
        safe_sh  <- gsub("[/\\\\:*?\"<>|]", "_", sh)
        out_path <- paste0(stem, "_", safe_sh, ".csv")
        write.csv(df, out_path, row.names = FALSE)
        n_written <- n_written + 1L
      }
      if (n_written > 0) {
        message("  exploded ", basename(xl), " → ", n_written, " CSV(s)")
        file.remove(xl)
      }
    }
    # Refresh file list after explosion
    files <- drop_git(list.files(target_dir, full.names = TRUE, recursive = TRUE))
    files <- files[!(tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS)]
  }

  # ── 3c. Remove duplicate files ──────────────────────────────────────────────
  # Files with the same basename AND byte-size are candidate duplicates.
  # For text-based formats (csv/tsv/txt/dat), confirm by comparing the first
  # 3 lines; for other formats, name+size alone is treated as sufficient.
  # Duplicates are dropped from 'files' before LLM classification so they
  # don't consume LLM calls or column-extraction time. Only the first
  # occurrence of each duplicate group is kept.
  finfo       <- file.info(files)
  dedup_key   <- paste(basename(files), finfo$size, sep = "\01")
  is_dup_cand <- duplicated(dedup_key) | duplicated(dedup_key, fromLast = TRUE)

  if (any(is_dup_cand)) {
    text_exts <- c("csv", "tsv", "txt", "dat")
    dup_files <- character(0)

    for (k in unique(dedup_key[is_dup_cand])) {
      group <- files[dedup_key == k]
      ext   <- tolower(tools::file_ext(group[1]))
      if (ext %in% text_exts) {
        fingerprints <- vapply(group, function(p) {
          tryCatch(paste(readLines(p, n = 3L, warn = FALSE), collapse = "\n"),
                   error = function(e) "")
        }, character(1))
        dup_files <- c(dup_files, group[duplicated(fingerprints)])
      } else {
        dup_files <- c(dup_files, group[-1L])   # keep first, discard rest
      }
    }

    if (length(dup_files) > 0) {
      message("── Removed ", length(dup_files), " duplicate file(s) (same name, size",
              ", and content)")
      files <- setdiff(files, dup_files)
    }
  }

  # ── 4. Build relative-path tree ─────────────────────────────────────────────

  norm_base  <- normalizePath(target_dir, mustWork = FALSE)
  rel_paths  <- sub(paste0("^", norm_base, "/?"), "",
                    normalizePath(files, mustWork = FALSE))

  # ── 5. Detect aggregate folders ─────────────────────────────────────────────
  # Two patterns are treated as aggregate (collapsed to a single sentinel row):
  #
  #  A) FLAT AGGREGATE: a folder with > AGGREGATE_THRESHOLD direct file children.
  #
  #  B) PARTICIPANT AGGREGATE: a folder whose immediate subdirectories are mostly
  #     numeric-looking names (participant / subject IDs such as 17230, 17238 …)
  #     and there are > AGGREGATE_THRESHOLD such subfolders.
  #
  #     Detection uses list.dirs() rather than inferring ancestry from rel_paths.
  #     The path-decomposition approach (grandparents = strip filename from path)
  #     produces grandparent == top_dirs for every file, so it can never find
  #     numeric child *directories* — only files whose name is numeric.  More
  #     importantly it silently fails for participant folders nested more than one
  #     level below the paper root (e.g. paper/Fear/FCTM_Data/FCTM_Exp1/17230/).

  top_dirs   <- dirname(rel_paths)
  dir_counts <- table(top_dirs)

  # Pattern A
  flat_agg_dirs <- names(dir_counts[dir_counts > AGGREGATE_THRESHOLD])

  # Pattern B — scan actual directory tree so depth doesn't matter
  all_actual_dirs <- list.dirs(target_dir, full.names = FALSE, recursive = TRUE)
  all_actual_dirs <- all_actual_dirs[all_actual_dirs != ""]
  all_actual_dirs <- all_actual_dirs[
    !grepl("(^|/)\\.git(/|$)", all_actual_dirs, perl = TRUE)]

  participant_agg_dirs <- character(0)
  for (d in all_actual_dirs) {
    prefix      <- paste0(d, "/")
    children    <- all_actual_dirs[startsWith(all_actual_dirs, prefix)]
    child_names <- sub(prefix, "", children, fixed = TRUE)
    # Keep only *direct* children (no slash = no further nesting)
    child_names <- child_names[!grepl("/", child_names, fixed = TRUE)]
    if (length(child_names) == 0) next
    n_numeric <- sum(grepl("^\\d+$", child_names))
    if (n_numeric > AGGREGATE_THRESHOLD) {
      participant_agg_dirs <- c(participant_agg_dirs, d)
    }
  }

  agg_dirs <- unique(c(flat_agg_dirs, participant_agg_dirs))
  is_under_participant_agg <- vapply(rel_paths, function(p) {
    any(startsWith(p, paste0(participant_agg_dirs, "/")))
  }, logical(1))

  is_aggregate <- (top_dirs %in% flat_agg_dirs) | is_under_participant_agg
  aggregate_df <- NULL

  if (any(is_aggregate)) {
    agg_sentinels <- lapply(agg_dirs, function(d) {
      if (d %in% participant_agg_dirs) {
        members <- rel_paths[startsWith(rel_paths, paste0(d, "/"))]
      } else {
        members <- rel_paths[top_dirs == d]
      }
      exts         <- tolower(tools::file_ext(members))
      dominant_ext <- names(sort(table(exts), decreasing = TRUE))[1]
      data.frame(
        rel_path     = file.path(d, paste0("[", length(members),
                                           "_files.", dominant_ext, "]")),
        is_sentinel  = TRUE,
        member_count = length(members),
        stringsAsFactors = FALSE
      )
    })
    aggregate_df <- do.call(rbind, agg_sentinels)
    message("── Detected ", length(agg_dirs), " aggregate folder(s): ",
            paste(agg_dirs, collapse = ", "))
  }

  non_agg_relpaths <- rel_paths[!is_aggregate]
  # If every file fell into an aggregate folder there is nothing left to process
  # individually — sentinel offers no benefit here and would leave non_agg empty.
  # Cancel the sentinel and process all paths normally; the MAX_LLM_CALLS guard
  # still prevents runaway for genuinely oversized repos.
  if (length(non_agg_relpaths) == 0) {
    aggregate_df     <- NULL
    non_agg_relpaths <- rel_paths
  }
  llm_paths        <- c(non_agg_relpaths,
                        if (!is.null(aggregate_df)) aggregate_df$rel_path)

  # ── 6. LLM: understand repository structure ──────────────────────────────────

  t_download <- proc.time()[["elapsed"]] - t_download_start

  t_llm_start <- proc.time()[["elapsed"]]
  MAX_LLM_CALLS <- 10
  n_llm_calls   <- ceiling(length(llm_paths) / LLM_BATCH_SIZE)
  if (n_llm_calls > MAX_LLM_CALLS) {
    stop("too_large: ", length(llm_paths), " paths would require ",
         n_llm_calls, " LLM calls (max ", MAX_LLM_CALLS, ")")
  }

  structure_parsed <- llm_batch(
    paths         = llm_paths,
    system_prompt = STRUCTURE_PROMPT,
    user_prefix   = "Classify this repository tree:",
    key_col       = "path",
    extra_cols    = c("type", "group"),
    fallback_vals = list(type = "other", group = "na")
  )

  # ── 7. Expand sentinels back to individual files ─────────────────────────────

  if (!is.null(aggregate_df)) {
    sentinel_results <- merge(aggregate_df, structure_parsed,
                              by.x = "rel_path", by.y = "path", all.x = TRUE)

    agg_expanded <- lapply(seq_len(nrow(sentinel_results)), function(i) {
      row        <- sentinel_results[i, ]
      parent_dir <- sub("/\\[.*\\]$", "", row$rel_path)
      if (parent_dir %in% participant_agg_dirs) {
        members <- rel_paths[startsWith(rel_paths, paste0(parent_dir, "/"))]
      } else {
        members <- rel_paths[top_dirs == parent_dir]
      }
      if (length(members) == 0) {
        warning("Aggregate sentinel has no members: ", parent_dir)
        return(NULL)
      }
      data.frame(
        path        = file.path(norm_base, members),
        rel_path    = members,
        type        = row$type,
        group       = row$group,
        is_raw      = NA,
        is_sentinel = FALSE,
        stringsAsFactors = FALSE
      )
    })
    agg_expanded_df <- do.call(rbind, Filter(Negate(is.null), agg_expanded))
    # Apply extension-based type correction for files expanded from aggregates.
    # Files with unambiguous extensions (e.g. .R → code, .jpeg → asset) get the
    # correct type regardless of what the LLM assigned to the sentinel.
    if (!is.null(agg_expanded_df) && nrow(agg_expanded_df) > 0) {
      agg_ext  <- tolower(tools::file_ext(agg_expanded_df$rel_path))
      override <- AGGREGATE_EXT_OVERRIDE[agg_ext]
      to_override <- !is.na(override)
      agg_expanded_df$type[to_override] <- override[to_override]
    }
  } else {
    agg_expanded_df <- NULL
  }

  non_agg_df <- data.frame(
    path        = file.path(norm_base, non_agg_relpaths),
    rel_path    = non_agg_relpaths,
    type        = structure_parsed$type[match(non_agg_relpaths, structure_parsed$path)],
    group       = structure_parsed$group[match(non_agg_relpaths, structure_parsed$path)],
    is_raw      = NA,
    is_sentinel = FALSE,
    stringsAsFactors = FALSE
  )

  file_df          <- rbind(non_agg_df, agg_expanded_df)
  file_df$paper_id <- paper_id
  file_df$filename <- basename(file_df$path)
  file_df$ext      <- tolower(tools::file_ext(file_df$path))

  # ── 8. Save structure ────────────────────────────────────────────────────────

  structure_out <- file.path(eff_dir, "structure.csv")
  cat("\n── File inventory ──────────────────────────────\n")
  print(table(paste0(file_df$type, " / ", file_df$group)))

  t_llm <- proc.time()[["elapsed"]] - t_llm_start

  t_col_start <- proc.time()[["elapsed"]]
  # ── 9. Extract columns + sample values from data files ───────────────────────

  data_files <- file_df[file_df$type == "data" & !file_df$is_sentinel, ]
  message("── Extracting columns + statistics from ", nrow(data_files), " data file(s)")

  MAX_FILE_MB <- 500  # skip data files larger than this

  # Mutable accumulator — tracks cumulative MB read so far for this paper.
  # Using an environment so the closure inside extract_column_info can update it.
  .read_state <- new.env(parent = emptyenv())
  .read_state$mb_read   <- 0
  .read_state$limit_hit <- FALSE

  extract_column_info <- function(path, rel_path, group) {
    file_mb <- file.info(path)$size / 1048576
    if (!is.na(file_mb) && file_mb > MAX_FILE_MB) {
      message("  skipping (too large: ", round(file_mb), " MB): ", basename(path))
      return(NULL)
    }
    # Aggregate data cap: skip this file if adding it would exceed the per-paper limit.
    if (!is.na(file_mb) && (.read_state$mb_read + file_mb) > MAX_TOTAL_DATA_MB) {
      if (!.read_state$limit_hit) {
        message("  stopping column extraction: total data read would exceed ",
                round(MAX_TOTAL_DATA_MB / 1024, 0), " GB limit (",
                round(.read_state$mb_read / 1024, 1), " GB already read)")
        .read_state$limit_hit <- TRUE
      }
      return(NULL)
    }
    timed_out <- FALSE
    df <- tryCatch({
      setTimeLimit(elapsed = MAX_FILE_READ_SEC, transient = TRUE)
      result <- read_data_head(path, n_rows = Inf)
      setTimeLimit(elapsed = Inf, transient = FALSE)
      result
    }, error = function(e) {
      setTimeLimit(elapsed = Inf, transient = FALSE)
      timed_out <<- TRUE
      message("  skipping (timed out after ", round(MAX_FILE_READ_SEC / 60), " min): ",
              basename(path))
      NULL
    })
    if (timed_out) return(NULL)
    if (is.null(df) || ncol(df) == 0) {
      message("  skipping (unreadable or empty): ", basename(path))
      return(NULL)
    }
    if (!is.na(file_mb)) .read_state$mb_read <- .read_state$mb_read + file_mb

    auto_named <- grepl("^\\.\\.\\.\\d+$", names(df))

    # ── Multi-level header recovery ────────────────────────────────────────────
    # Initialise col_header_group as all-NA (default when no multi-level structure).
    col_header_group <- rep(NA_character_, ncol(df))

    if (mean(auto_named) > 0.5) {
      # Extract group labels from row-1 names and forward-fill across spans.
      # "SHAM...3" → prefix "SHAM"; "...4" → "" → NA → filled from last real prefix.
      row1_names   <- names(df)
      raw_prefixes <- sub("\\.\\.\\.\\d+$", "", row1_names)
      raw_prefixes[!nzchar(raw_prefixes)] <- NA_character_
      last_grp <- NA_character_
      col_header_group <- vapply(raw_prefixes, function(p) {
        if (!is.na(p)) last_grp <<- p
        last_grp
      }, character(1))

      # Branch 1: scan for a better sub-header row.
      sub_header_row    <- NULL
      current_auto_frac <- mean(auto_named)
      for (i in seq_len(min(MULTILEVEL_HEADER_LOOKAHEAD, nrow(df)))) {
        candidate      <- as.character(df[i, ])
        cand_auto_frac <- mean(grepl("^\\.\\.\\.\\d+$", candidate))
        # A real sub-header cell must be non-empty, non-NA, non-...N, and non-numeric.
        # Pure numeric rows are data rows, not label rows.
        has_real       <- any(!is.na(candidate) & nzchar(candidate) &
                              candidate != "NA" &
                              !grepl("^\\.\\.\\.\\d+$", candidate) &
                              is.na(suppressWarnings(as.numeric(candidate))))
        if (cand_auto_frac < current_auto_frac && has_real) {
          sub_header_row <- i
          break
        }
      }

      if (!is.null(sub_header_row)) {
        # Use sub-header values as column names.
        # NA or empty cells fall back to the original ...N name (preserves uniqueness).
        new_names           <- as.character(df[sub_header_row, ])
        fallback            <- is.na(new_names) | !nzchar(new_names)
        new_names[fallback] <- row1_names[fallback]
        new_names           <- make.unique(new_names)
        df                  <- df[(sub_header_row + 1):nrow(df), , drop = FALSE]
        names(df)           <- new_names
        # col_header_group is aligned column-wise; row slicing above does not affect it.
        message("  multi-level header resolved (used row ", sub_header_row + 1,
                " as header): ", basename(path))
      } else {
        # Branch 2: no sub-header found — group context not meaningful without a sub-header.
        col_header_group <- rep(NA_character_, ncol(df))
        has_any_real     <- any(!auto_named)
        if (has_any_real) {
          message("  multi-level header detected (partial labels retained): ",
                  basename(path))
          # proceed with df as-is
        } else {
          # skip: entirely placeholder header with no recoverable sub-header
          message("  skipping (multi-level header, no usable sub-header found): ",
                  basename(path))
          return(NULL)
        }
      }
    }

    sample_vals <- vapply(df, function(col) {
      vals <- as.character(col[!is.na(col)])
      if (length(vals) == 0) "" else paste(head(vals, N_DATA_READ), collapse = " | ")
    }, character(1))

    # ── Classify each column ───────────────────────────────────────────────────
    col_classifications <- lapply(names(df), function(col) {
      classify_col_type_rules(col, df[[col]])
    })

    col_types <- vapply(col_classifications, function(cls) {
      if (is.null(cls$col_type) || is.na(cls$col_type)) NA_character_ else cls$col_type
    }, character(1))

    ambiguous_idx <- vapply(col_classifications, `[[`, logical(1), "ambiguous")

    is_numeric_vec <- vapply(col_classifications, function(cls) {
      isTRUE(cls$is_numeric)
    }, logical(1))

    n_coerced_vec <- vapply(col_classifications, function(cls) {
      v <- cls$n_coerced
      if (is.null(v) || is.na(v)) NA_integer_ else as.integer(v)
    }, integer(1))

    # Unique sample values for LLM classification of ambiguous columns
    sample_vals_unique <- vapply(seq_along(names(df)), function(i) {
      if (!ambiguous_idx[i]) return(NA_character_)
      x_noNA <- df[[names(df)[i]]]
      x_noNA <- x_noNA[!is.na(x_noNA)]
      uniq_v <- unique(x_noNA)[seq_len(min(10, length(unique(x_noNA))))]
      paste(as.character(uniq_v), collapse = ", ")
    }, character(1))

    col_stats <- lapply(seq_along(names(df)), function(i) {
      col <- names(df)[i]
      cls <- col_classifications[[i]]

      # Determine which numeric vector to use for statistics
      x_for_stats <- cls$numeric_values
      if (is.null(x_for_stats) && isTRUE(cls$ambiguous)) {
        x_for_stats <- df[[col]]  # ambiguous numeric — compute tentative stats
      }

      if (is.null(x_for_stats)) {
        # Non-numeric, non-ambiguous: report n/n_missing only
        x_raw  <- df[[col]]
        n_miss <- sum(is.na(x_raw))
        n_val  <- length(x_raw) - n_miss
        return(list(n = n_val, n_missing = n_miss, mean = NA, sd = NA, se = NA,
                    median = NA, min = NA, max = NA, range = NA,
                    p25 = NA, p75 = NA, iqr = NA, skewness = NA, kurtosis = NA))
      }

      x_comp <- as.numeric(x_for_stats[!is.na(x_for_stats)])
      n      <- length(x_comp)
      n_miss <- sum(is.na(x_for_stats))
      if (n == 0) {
        return(list(n = 0L, n_missing = n_miss, mean = NA, sd = NA, se = NA,
                    median = NA, min = NA, max = NA, range = NA,
                    p25 = NA, p75 = NA, iqr = NA, skewness = NA, kurtosis = NA))
      }
      mn   <- mean(x_comp)
      s    <- if (n > 1) sd(x_comp) else NA_real_
      se   <- if (!is.na(s)) s / sqrt(n) else NA_real_
      med  <- median(x_comp)
      mn_v <- min(x_comp)
      mx_v <- max(x_comp)
      p25  <- quantile(x_comp, 0.25, names = FALSE)
      p75  <- quantile(x_comp, 0.75, names = FALSE)
      skew <- if (n > 2 && !is.na(s) && s > 0) mean((x_comp - mn)^3) / s^3 else NA_real_
      kurt <- if (n > 3 && !is.na(s) && s > 0) mean((x_comp - mn)^4) / s^4 - 3 else NA_real_
      list(n = n, n_missing = n_miss, mean = mn, sd = s, se = se,
           median = med, min = mn_v, max = mx_v, range = mx_v - mn_v,
           p25 = p25, p75 = p75, iqr = p75 - p25, skewness = skew, kurtosis = kurt)
    })

    raw_folder <- grepl("(^|/)(raw|raw_data)(/|$)", rel_path, ignore.case = TRUE, perl = TRUE)
    processed_folder <- grepl(
      "(^|/)(processed|processed_data|clean|cleaned|output|results|derived|interim)(/|$)",
      rel_path, ignore.case = TRUE, perl = TRUE)
    stem <- tools::file_path_sans_ext(basename(path))
    participant_filename <- grepl("(^|[_\\-])[0-9]{2,}([_\\-]|$)", stem, perl = TRUE)
    combined_filename    <- grepl("clean|combined|merged|full|all[_\\-]|aggregat", stem, ignore.case = TRUE)
    file_is_raw <- (raw_folder || participant_filename) && !processed_folder && !combined_filename

    stats_mat <- do.call(rbind, lapply(col_stats, as.data.frame, stringsAsFactors = FALSE))

    list(
      columns = data.frame(
        paper_id             = paper_id,
        source_file          = rel_path,
        filename             = basename(path),
        group                = group,
        col_header_group     = col_header_group,
        column_name          = names(df),
        sample_values        = sample_vals,
        col_type             = col_types,
        n_coerced            = n_coerced_vec,
        stats_mat,
        sample_values_unique = sample_vals_unique,
        is_numeric           = is_numeric_vec,
        stringsAsFactors = FALSE,
        row.names     = NULL
      ),
      is_raw = file_is_raw
    )
  }

  column_list  <- mapply(extract_column_info,
                         path = data_files$path, rel_path = data_files$rel_path,
                         group = data_files$group, SIMPLIFY = FALSE)
  column_list  <- Filter(Negate(is.null), column_list)
  columns_df   <- do.call(rbind, lapply(column_list, `[[`, "columns"))

  # ── LLM classification for ambiguous columns ──────────────────────────────
  if (!is.null(columns_df) && nrow(columns_df) > 0 && any(is.na(columns_df$col_type))) {
    ambig_rows <- which(is.na(columns_df$col_type))
    max_cols   <- MAX_COL_TYPE_LLM_CALLS * LLM_BATCH_SIZE
    if (length(ambig_rows) > max_cols) ambig_rows <- ambig_rows[seq_len(max_cols)]
    descriptors <- paste0('"', columns_df$column_name[ambig_rows], '"',
                          " (samples: ", columns_df$sample_values_unique[ambig_rows], ")")
    message("── LLM col_type: classifying ", length(ambig_rows), " ambiguous column(s)")
    llm_result <- tryCatch(
      llm_batch(
        paths         = descriptors,
        system_prompt = COLUMN_TYPE_PROMPT,
        user_prefix   = "Classify each column:",
        key_col       = "descriptor",
        extra_cols    = "col_type",
        fallback_vals = list(col_type = "unknown")
      ),
      error = function(e) {
        warning("LLM col_type batch failed: ", conditionMessage(e))
        data.frame(descriptor = descriptors,
                   col_type   = rep("unknown", length(descriptors)),
                   stringsAsFactors = FALSE)
      }
    )
    returned_types <- llm_result$col_type
    returned_types[!returned_types %in% VALID_COL_TYPES] <- "unknown"
    columns_df$col_type[ambig_rows] <- returned_types

    # Fallback: LLM "unknown" for a confirmed-numeric column → "continuous".
    # is_numeric is TRUE only for Rule 6 (integer numeric, 3–20 unique values);
    # NOT for Rule 3 ID columns, which must stay as "id" or "unknown".
    if ("is_numeric" %in% names(columns_df)) {
      num_unknown <- ambig_rows[
        columns_df$is_numeric[ambig_rows] & columns_df$col_type[ambig_rows] == "unknown"
      ]
      if (length(num_unknown) > 0) {
        columns_df$col_type[num_unknown] <- "continuous"
        message("── col_type fallback: ", length(num_unknown),
                " numeric column(s) reclassified from unknown \u2192 continuous")
      }
    }
  }

  # ── Final cleanup: fallback NAs, stat suppression, drop transient column ──
  if (!is.null(columns_df) && nrow(columns_df) > 0) {
    columns_df$col_type[is.na(columns_df$col_type)] <- "unknown"
    stat_cols     <- c("mean", "sd", "se", "median", "min", "max", "range",
                       "p25", "p75", "iqr", "skewness", "kurtosis")
    numeric_types <- c("continuous", "continuous_comma_decimal",
                       "continuous_outliers_excluded")
    suppress_rows <- !columns_df$col_type %in% numeric_types
    columns_df[suppress_rows, stat_cols] <- NA
    columns_df$sample_values_unique <- NULL
    columns_df$is_numeric           <- NULL
  }

  is_raw_flags <- setNames(
    vapply(column_list, `[[`, logical(1), "is_raw"),
    vapply(column_list, function(x) x$columns$source_file[1], character(1))
  )

  file_df$is_raw[match(names(is_raw_flags), file_df$rel_path)] <- is_raw_flags
  file_df$is_raw[is.na(file_df$is_raw)] <- FALSE

  n_raw    <- sum(file_df$type == "data" & file_df$is_raw  & !file_df$is_sentinel)
  n_nonraw <- sum(file_df$type == "data" & !file_df$is_raw & !file_df$is_sentinel)
  message("── is_raw detection: ", n_raw, " raw, ", n_nonraw, " non-raw data file(s)")

  write.csv(
    file_df[, c("paper_id", "path", "rel_path", "filename", "ext",
                "type", "group", "is_raw", "is_sentinel")],
    structure_out, row.names = FALSE
  )
  message("── Saved structure → ", structure_out)

  if (!is.null(columns_df) && nrow(columns_df) > 0) {
    columns_out   <- file.path(eff_dir, "columns.csv")
    invalid_types <- setdiff(unique(columns_df$col_type), VALID_COL_TYPES)
    if (length(invalid_types) > 0)
      warning("Unknown col_type values: ", paste(invalid_types, collapse = ", "))
    write.csv(columns_df, columns_out, row.names = FALSE)
    message("── Saved columns  → ", columns_out,
            "  (", nrow(columns_df), " rows across ",
            length(unique(columns_df$source_file)), " file(s))")
  } else {
    message("── No columns extracted")
    columns_df  <- NULL
    columns_out <- NULL
  }

  t_col <- proc.time()[["elapsed"]] - t_col_start
  elapsed <- proc.time()[["elapsed"]] - t_start

  # ── Return structured result ─────────────────────────────────────────────────

  list(
    paper_id       = paper_id,
    success        = TRUE,
    error          = NULL,
    elapsed_sec    = elapsed,
    download_sec   = t_download,
    llm_sec        = t_llm,
    column_sec     = t_col,
    n_files        = nrow(file_df),
    n_data_files   = nrow(data_files),
    n_agg_dirs     = length(agg_dirs),
    n_raw          = n_raw,
    n_nonraw       = n_nonraw,
    n_columns      = if (!is.null(columns_df)) nrow(columns_df) else 0L,
    n_source_files = if (!is.null(columns_df)) length(unique(columns_df$source_file)) else 0L,
    type_counts    = table(file_df$type),
    group_counts   = table(file_df$group),
    file_df        = file_df,
    columns_df     = columns_df,
    structure_path = structure_out,
    columns_path   = columns_out
  )
}
