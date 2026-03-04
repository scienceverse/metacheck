# The goal of this script is to extract column names from every data file
# belonging to a paper and save them as a long-format index:
#   paper_id | source_file | column_name | experiment_group
#
# Input:  a paper ID (set below)
# Reads:  data_check/structure/<paper_id>_structure.csv  (from 0_data_index.R)
# Output: data_check/structure/<paper_id>_columns.csv

source("./data_check/helper.R")
library(metacheck)

# ── Constants ─────────────────────────────────────────────────────────────────

STRUCTURE_DIR <- "./data_check/structure"


# ── Input ─────────────────────────────────────────────────────────────────────

paper_id <- "09567976231220902"

# ── 1. Load structure index ───────────────────────────────────────────────────

structure_path <- file.path(STRUCTURE_DIR, paste0(paper_id, "_structure.csv"))

if (!file.exists(structure_path)) {
  stop("Structure file not found: ", structure_path,
       "\nRun 0_data_index.R for this paper first.")
}

structure_df <- read.csv(structure_path, stringsAsFactors = FALSE)

# Keep only data files (label starts with "data-")
data_files <- structure_df[grepl("^data", structure_df$label), ]

message("── Found ", nrow(data_files), " data file(s) for paper ", paper_id)

if (nrow(data_files) == 0) {
  stop("No data files found in structure CSV for paper ", paper_id, " — nothing to label.")
}

# ── 2. Extract columns from each data file ────────────────────────────────────

# Sniff the delimiter of a text file by counting candidate characters in the
# first non-empty line.  Returns the most frequent one, defaulting to ",".
sniff_delimiter <- function(path) {
  line <- ""
  con  <- file(path, "r")
  on.exit(close(con))
  for (i in seq_len(10)) {          # try up to 10 lines in case of BOM/blanks
    line <- readLines(con, n = 1, warn = FALSE)
    if (nchar(trimws(line)) > 0) break
  }
  candidates <- c(",", ";", "\t", "|")
  counts     <- vapply(candidates, function(d) nchar(line) - nchar(gsub(d, "", line, fixed = TRUE)), integer(1))
  winner     <- candidates[which.max(counts)]
  if (max(counts) == 0) "," else winner   # fall back to comma if none found
}

extract_columns <- function(path, label) {
  ext <- tolower(tools::file_ext(path))

  tryCatch({
    col_names <- switch(ext,
      csv  = {
        sep <- sniff_delimiter(path)
        names(read.delim(path, sep = sep, nrows = 0, check.names = FALSE))
      },
      tsv  = names(read.delim(path, nrows = 0, check.names = FALSE)),
      xlsx = ,
      xls  = names(readxl::read_excel(path, n_max = 0)),
      sav  = names(haven::read_sav(path, n_max = 0)),
      dta  = names(haven::read_dta(path, n_max = 0)),
      sas7bdat = names(haven::read_sas(path, n_max = 0)),
      rds  = {
        obj <- readRDS(path)
        if (is.data.frame(obj)) names(obj) else NULL
      },
      rda  = ,
      rdata = {
        env <- new.env()
        load(path, envir = env)
        dfs <- Filter(is.data.frame, as.list(env))
        if (length(dfs) > 0) names(dfs[[1]]) else NULL
      },
      {
        message("  skipping unsupported extension: ", ext, " (", basename(path), ")")
        NULL
      }
    )

    if (is.null(col_names) || length(col_names) == 0) {
      message("  no columns found in: ", basename(path))
      return(NULL)
    }

    data.frame(
      paper_id         = paper_id,
      source_file      = path,
      filename         = basename(path),
      experiment_group = sub("^data-", "", label),   # e.g. "ex1", "other"
      column_name      = col_names,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning("Failed to read ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

column_list <- mapply(
  extract_columns,
  path  = data_files$path,
  label = data_files$label,
  SIMPLIFY = FALSE
)

columns_df <- do.call(rbind, Filter(Negate(is.null), column_list))

if (is.null(columns_df) || nrow(columns_df) == 0) {
  stop("No columns could be extracted from any data file for paper ", paper_id,
       " — check warnings above for individual file failures.")
}

# ── 3. Save ───────────────────────────────────────────────────────────────────

out_path <- file.path(STRUCTURE_DIR, paste0(paper_id, "_columns.csv"))
write.csv(columns_df, out_path, row.names = FALSE)
message("── Saved column index (", nrow(columns_df), " rows) → ", out_path)