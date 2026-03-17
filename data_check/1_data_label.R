# 1_data_label.R
# ─────────────────────────────────────────────────────────────────────────────
# Exports: run_data_label(paper_id)
#
# Extracts column names from every data file belonging to a paper and saves
# them as a long-format index:
#   paper_id | source_file | filename | experiment_group | column_name
#
# Input:  paper_id (character)
#         outputs/<paper_id>/structure.csv  (from 0_index.R)
# Output: outputs/<paper_id>/columns.csv
#
# Sourcing this file defines run_data_label() only — no side effects.
# ─────────────────────────────────────────────────────────────────────────────

source("./data_check/helper.R")
library(metacheck)

# ── Pipeline function ─────────────────────────────────────────────────────────

run_data_label <- function(paper_id) {

  t_start <- proc.time()[["elapsed"]]

  # ── 1. Load structure index ──────────────────────────────────────────────────

  structure_path <- file.path(paper_output_dir(paper_id), "structure.csv")

  if (!file.exists(structure_path)) {
    stop("Structure file not found: ", structure_path,
         "\nRun 0_index.R for this paper first.")
  }

  structure_df <- read.csv(structure_path, stringsAsFactors = FALSE,
                            colClasses = c(paper_id = "character"))

  # Keep only data files
  data_files <- structure_df[structure_df$type == "data" & !structure_df$is_sentinel, ]

  message("── Found ", nrow(data_files), " data file(s) for paper ", paper_id)

  if (nrow(data_files) == 0) {
    stop("No data files found in structure CSV for paper ", paper_id, " — nothing to label.")
  }

  # ── 2. Helpers ────────────────────────────────────────────────────────────────

  # Sniff the delimiter of a text file by counting candidate characters in the
  # first non-empty line. Returns the most frequent one, defaulting to ",".
  sniff_delimiter <- function(path) {
    line <- ""
    con  <- file(path, "r")
    on.exit(close(con))
    for (i in seq_len(10)) {
      line <- readLines(con, n = 1, warn = FALSE)
      if (nchar(trimws(line)) > 0) break
    }
    candidates <- c(",", ";", "\t", "|")
    counts     <- vapply(candidates, function(d)
      nchar(line) - nchar(gsub(d, "", line, fixed = TRUE)), integer(1))
    if (max(counts) == 0) "," else candidates[which.max(counts)]
  }

  MAX_FILE_MB <- 500  # skip data files larger than this (matches 0_index.R limit)

  extract_columns <- function(path, group) {
    file_mb <- file.info(path)$size / 1048576
    if (!is.na(file_mb) && file_mb > MAX_FILE_MB) {
      message("  skipping (too large: ", round(file_mb), " MB): ", basename(path))
      return(NULL)
    }

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
        experiment_group = group,
        column_name      = col_names,
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      warning("Failed to read ", basename(path), ": ", conditionMessage(e))
      NULL
    })
  }

  # ── 3. Extract columns ────────────────────────────────────────────────────────

  column_list <- mapply(
    extract_columns,
    path  = data_files$path,
    group = data_files$group,
    SIMPLIFY = FALSE
  )

  columns_df <- do.call(rbind, Filter(Negate(is.null), column_list))

  if (is.null(columns_df) || nrow(columns_df) == 0) {
    stop("No columns could be extracted from any data file for paper ", paper_id,
         " — check warnings above for individual file failures.")
  }

  # ── 4. Save ───────────────────────────────────────────────────────────────────

  out_path <- file.path(paper_output_dir(paper_id), "columns.csv")
  write.csv(columns_df, out_path, row.names = FALSE)
  message("── Saved column index (", nrow(columns_df), " rows) → ", out_path)

  elapsed <- proc.time()[["elapsed"]] - t_start

  list(
    paper_id     = paper_id,
    success      = TRUE,
    error        = NA_character_,
    elapsed_sec  = elapsed,
    n_data_files = nrow(data_files),
    n_columns    = nrow(columns_df)
  )
}
