# 0_index.R
# ─────────────────────────────────────────────────────────────────────────────
# Full pipeline: download → unpack → understand structure → classify files
#                → extract columns + sample values → save
#
# Input:  paper_id
# Output: data_check/structure/<paper_id>_structure.csv   (one row per file)
#         data_check/structure/<paper_id>_columns.csv     (one row per column)
# ─────────────────────────────────────────────────────────────────────────────

library(metacheck)
source("./data_check/helper.R")

llm_use(TRUE)
llm_model("ollama/gpt-oss:20b-cloud")

# ── Constants ─────────────────────────────────────────────────────────────────

DATA_DIR        <- "./data_check/data"
STRUCTURE_DIR   <- "./data_check/structure"
ARCHIVE_EXTS    <- c("zip", "gz", "tar", "tgz", "bz2", "xz")
LLM_BATCH_SIZE  <- 20
N_DATA_READ <- 5
# Folders with more than this many files are treated as aggregate datasets
# (e.g. a meta-analysis DOI tree) rather than individually indexed.
AGGREGATE_THRESHOLD <- 50
# Directory names longer than this many words are truncated; spaces → underscores
MAX_DIR_WORDS   <- 5

# ── Input ─────────────────────────────────────────────────────────────────────

xml_dir <- "./data-raw/psychsci/grobid_0.8.2"

# Set paper_id to a specific ID, or NA to pick one at random. best case: data_check/data/0956797615620784.xml
paper_id <- NA

if (is.na(paper_id)) {
  xml_files <- list.files(xml_dir, pattern = "\\.xml$", full.names = FALSE)
  if (length(xml_files) == 0) stop("No XML files found in ", xml_dir)
  paper_id  <- tools::file_path_sans_ext(sample(xml_files, 1))
  message("── Randomly selected paper: ", paper_id)
}

# ── 0. Resolve paper ──────────────────────────────────────────────────────────

xml_path <- file.path(xml_dir, paste0(paper_id, ".xml"))
paper    <- read(xml_path)
stopifnot(!is.null(paper$id))

target_dir <- file.path(DATA_DIR, paper_id)
if (!dir.exists(STRUCTURE_DIR)) dir.create(STRUCTURE_DIR, recursive = TRUE)

# ── 1. Download ───────────────────────────────────────────────────────────────

BADGE_REPOS <- c("tvyxz", "osf.io/tvyxz/", "osf.io/tvyxz")

links        <- osf_links(paper)
unique_links <- setdiff(unique(links$text), BADGE_REPOS)

if (!dir.exists(target_dir)) {
  for (link in unique_links) {
    osf_file_download(link, download_to = target_dir,
                      max_download_size = Inf, max_file_size = NULL)
  }
}

# ── 2. Sanitize directory names ───────────────────────────────────────────────
# BEcause OSF includes every single paper title in the pathing, LLM's can get confused (And this also
# wastes tokens on long paths. We truncate these.

sanitize_name <- function(name) {
  words <- strsplit(trimws(name), "\\s+")[[1]]
  words <- words[nchar(words) > 0]
  paste(head(words, MAX_DIR_WORDS), collapse = "_")
}

if (dir.exists(target_dir)) {
  all_dirs <- list.dirs(target_dir, full.names = TRUE, recursive = TRUE)
  all_dirs <- all_dirs[all_dirs != target_dir]   # skip root

  for (i in seq_along(all_dirs)) {
    d <- all_dirs[i]
    if (!dir.exists(d)) next
    dname <- basename(d)
    if (!grepl(" ", dname)) next
    new_name <- sanitize_name(dname)
    new_path <- file.path(dirname(d), new_name)
    if (new_path != d && !file.exists(new_path)) {
      file.rename(d, new_path)
      # Update all remaining entries that were under the old path so child
      # renames still point to the correct (now-moved) location
      all_dirs <- sub(paste0("^", d, "/"), paste0(new_path, "/"), all_dirs)
    }
  }
}

# ── 3. Unpack archives ────────────────────────────────────────────────────────

files <- if (dir.exists(target_dir)) {
  list.files(target_dir, full.names = TRUE, recursive = TRUE)
} else {
  character(0)
}

if (length(files) == 0) {
  stop("No files found for paper ", paper_id,
       " — check that the OSF download succeeded and the folder exists.")
}

archive_paths <- files[tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS]

if (length(archive_paths) > 0) {
  message("── Unpacking ", length(archive_paths), " archive(s)")
  lapply(archive_paths, unpack_archive)
  files <- list.files(target_dir, full.names = TRUE, recursive = TRUE)
  files <- files[!(tolower(tools::file_ext(files)) %in% ARCHIVE_EXTS)]
}

# ── 4. Build relative-path tree ───────────────────────────────────────────────

norm_base  <- normalizePath(target_dir, mustWork = FALSE)
rel_paths  <- sub(paste0("^", norm_base, "/?"), "",
                  normalizePath(files, mustWork = FALSE))

# ── 5. Detect aggregate folders ───────────────────────────────────────────────
# Two patterns are treated as aggregate (collapsed to a single sentinel row):
#
#  A) FLAT AGGREGATE: a folder with > AGGREGATE_THRESHOLD direct file children
#     (e.g. 236k results.csv files in one directory).
#
#  B) PARTICIPANT AGGREGATE: a folder whose immediate subdirectories are mostly
#     numeric-looking names (participant / subject IDs such as 17230, 17238 …)
#     and there are > AGGREGATE_THRESHOLD such subfolders.  The entire subtree
#     under that parent is collapsed to one sentinel so we don't send thousands
#     of near-identical per-participant files to the LLM.

top_dirs   <- dirname(rel_paths)
dir_counts <- table(top_dirs)

# Pattern A: too many direct files in one folder
flat_agg_dirs <- names(dir_counts[dir_counts > AGGREGATE_THRESHOLD])

# Pattern B: parent has many numeric immediate subdirectories
path_parts   <- strsplit(rel_paths, "/", fixed = TRUE)
# For each file, the "grandparent" is everything up to depth-2 from the root
get_nth_ancestor <- function(parts, n) {
  if (length(parts) <= n) NA_character_
  else paste(parts[seq_len(length(parts) - n)], collapse = "/")
}
grandparents <- vapply(path_parts, get_nth_ancestor, character(1), n = 1)
direct_subdirs_of <- function(parent) {
  # immediate child dir names of `parent` that appear in our paths
  children <- top_dirs[!is.na(grandparents) & grandparents == parent]
  unique(basename(children))
}
candidate_parents <- unique(grandparents[!is.na(grandparents)])
participant_agg_dirs <- character(0)
for (p in candidate_parents) {
  subdirs <- direct_subdirs_of(p)
  n_numeric <- sum(grepl("^\\d+$", subdirs))
  if (n_numeric > AGGREGATE_THRESHOLD) {
    participant_agg_dirs <- c(participant_agg_dirs, p)
  }
}

agg_dirs     <- unique(c(flat_agg_dirs, participant_agg_dirs))
# For participant aggregates, membership = any file whose path starts with the
# parent dir prefix (covers all nested subfolders).
is_under_participant_agg <- vapply(rel_paths, function(p) {
  any(startsWith(p, paste0(participant_agg_dirs, "/")))
}, logical(1))

is_aggregate <- (top_dirs %in% flat_agg_dirs) | is_under_participant_agg
aggregate_df <- NULL

if (any(is_aggregate)) {
  agg_sentinels <- lapply(agg_dirs, function(d) {
    # Flat agg: direct children only; participant agg: all descendants
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

# Build the paths that will be sent to the LLM: non-aggregate files + sentinels
non_agg_relpaths <- rel_paths[!is_aggregate]
llm_paths        <- c(non_agg_relpaths,
                      if (!is.null(aggregate_df)) aggregate_df$rel_path)

# ── 6. LLM: understand repository structure ───────────────────────────────────
# Single pass — LLM sees the full tree and returns type + group for every path.
# This is better than classifying in isolation because the LLM can use context
# (e.g. "all Study N folders follow the same pattern") across the whole repo.

# TODO: Supplemental experiments are sometimes real studies that deserve their
# own group (e.g. "supex1") rather than being lumped into "other". For now they
# are mapped to "other" to avoid false merges with the main experiment groups,
# but this should be revisited — ideally by introducing a "supex<N>" group and
# updating the downstream schema-mapping step to handle it accordingly.

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
                 preregistrations, supporting info appendices, and any file
                 labelled "supplemental/supporting"
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

structure_parsed <- llm_batch(
  paths         = llm_paths,
  system_prompt = STRUCTURE_PROMPT,
  user_prefix   = "Classify this repository tree:",
  key_col       = "path",
  extra_cols    = c("type", "group"),
  fallback_vals = list(type = "other", group = "na")
)

# ── 7. Expand sentinels back to individual files ──────────────────────────────

if (!is.null(aggregate_df)) {
  sentinel_results <- merge(aggregate_df, structure_parsed,
                            by.x = "rel_path", by.y = "path", all.x = TRUE)

  agg_expanded <- lapply(seq_len(nrow(sentinel_results)), function(i) {
    row        <- sentinel_results[i, ]
    parent_dir <- sub("/\\[.*\\]$", "", row$rel_path)
    # Participant aggregates own the whole subtree; flat aggregates own only
    # direct children (top_dirs == parent_dir).
    if (parent_dir %in% participant_agg_dirs) {
      members <- rel_paths[startsWith(rel_paths, paste0(parent_dir, "/"))]
    } else {
      members <- rel_paths[top_dirs == parent_dir]
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
  agg_expanded_df <- do.call(rbind, agg_expanded)
} else {
  agg_expanded_df <- NULL
}

# Build final file_df: non-aggregate rows + expanded aggregate rows
non_agg_df <- data.frame(
  path        = file.path(norm_base, non_agg_relpaths),
  rel_path    = non_agg_relpaths,
  type        = structure_parsed$type[match(non_agg_relpaths, structure_parsed$path)],
  group       = structure_parsed$group[match(non_agg_relpaths, structure_parsed$path)],
  is_raw      = NA,
  is_sentinel = FALSE,
  stringsAsFactors = FALSE
)

file_df <- rbind(non_agg_df, agg_expanded_df)
file_df$paper_id <- paper_id
file_df$filename <- basename(file_df$path)
file_df$ext      <- tolower(tools::file_ext(file_df$path))

# ── 8. Save structure (written again after step 8 populates is_raw) ───────────

structure_out <- file.path(STRUCTURE_DIR, paste0(paper_id, "_structure.csv"))
cat("\n── File inventory ──────────────────────────────\n")
print(table(paste0(file_df$type, " / ", file_df$group)))

# ── 9. Extract columns + sample values from data files ───────────────────────
# Skip sentinel rows (they represent folders, not readable files).
# is_raw is determined programmatically here after reading actual file content
# Read 3 rows: enough to see value types without loading full datasets.

data_files <- file_df[file_df$type == "data" & !file_df$is_sentinel, ]
message("── Extracting columns + statistics from ", nrow(data_files), " data file(s)")

# Reads full file once, returns:
#   $columns  — one row per column (sample values for all columns)
#   $is_raw   — logical scalar
# Stats columns (mean, sd, …) are NA for non-numeric columns.
extract_column_info <- function(path, rel_path, group) {
  df <- read_data_head(path, n_rows = Inf)
  if (is.null(df) || ncol(df) == 0) {
    message("  skipping (unreadable or empty): ", basename(path))
    return(NULL)
  }

  # Detect files where readxl found no header and auto-generated names like
  # "...1", "...2", ... — this means the file has no tabular column structure
  # (e.g. a questionnaire layout, a formatted report saved as xlsx).
  auto_named <- grepl("^\\.\\.\\.\\d+$", names(df))
  if (mean(auto_named) > 0.5) {
    message("  skipping (no proper header row, >50% auto-named columns): ",
            basename(path))
    return(NULL)
  }

  # TODO: Detect Qualtrics triple-header format — if row 1 of the data contains
  # {"ImportId": strings, the file uses the Qualtrics standard where:
  #   row 1 = machine-readable column names (already used as header)
  #   row 2 = human-readable labels
  #   row 3 = ImportId JSON (e.g. {"ImportId":"QID1_TEXT"})
  # In that case, skip rows 1-2 when extracting sample_vals so that actual
  # participant data (starting at row 3 of the file, row 1 of df here) is used.

  # Sample values: first N_DATA_READ non-NA values per column
  sample_vals <- vapply(df, function(col) {
    vals <- as.character(col[!is.na(col)])
    if (length(vals) == 0) "" else paste(head(vals, N_DATA_READ), collapse = " | ")
  }, character(1))

  # ── Descriptive statistics (numeric columns only) ──────────────────────────
  col_stats <- lapply(names(df), function(col) {
    x <- df[[col]]
    if (!is.numeric(x)) {
      return(list(n = NA, n_missing = NA, mean = NA, sd = NA, se = NA,
                  median = NA, min = NA, max = NA, range = NA,
                  p25 = NA, p75 = NA, iqr = NA, skewness = NA, kurtosis = NA))
    }
    x_comp <- x[!is.na(x)]
    n      <- length(x_comp)
    n_miss <- sum(is.na(x))
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

  # ── is_raw detection ───────────────────────────────────────────────────────
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
      paper_id      = paper_id,
      source_file   = rel_path,
      filename      = basename(path),
      group         = group,
      column_name   = names(df),
      sample_values = sample_vals,
      stats_mat,
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
is_raw_flags <- setNames(
  vapply(column_list, `[[`, logical(1), "is_raw"),
  vapply(column_list, function(x) x$columns$source_file[1], character(1))
)

# Write is_raw back into file_df
file_df$is_raw[match(names(is_raw_flags), file_df$rel_path)] <- is_raw_flags
file_df$is_raw[is.na(file_df$is_raw)] <- FALSE

n_raw    <- sum(file_df$type == "data" & file_df$is_raw  & !file_df$is_sentinel)
n_nonraw <- sum(file_df$type == "data" & !file_df$is_raw & !file_df$is_sentinel)
message("── is_raw detection: ", n_raw, " raw, ", n_nonraw, " non-raw data file(s)")

# Re-save structure CSV now that is_raw is populated
write.csv(
  file_df[, c("paper_id", "path", "rel_path", "filename", "ext",
              "type", "group", "is_raw", "is_sentinel")],
  structure_out, row.names = FALSE
)
message("── Saved structure → ", structure_out)

if (!is.null(columns_df) && nrow(columns_df) > 0) {
  columns_out <- file.path(STRUCTURE_DIR, paste0(paper_id, "_columns.csv"))
  write.csv(columns_df, columns_out, row.names = FALSE)
  message("── Saved columns  → ", columns_out,
          "  (", nrow(columns_df), " rows across ",
          length(unique(columns_df$source_file)), " file(s))")
} else {
  message("── No columns extracted")
}
