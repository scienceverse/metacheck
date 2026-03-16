# ── Text file helpers ─────────────────────────────────────────────────────────

# Sniff the delimiter of a text file by counting candidate characters in the
# first non-empty line.  Returns the most frequent one, defaulting to ",".
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

# Read the first n_rows of a data file regardless of format.
# Returns a data.frame, or NULL on failure / unsupported format.
read_data_head <- function(path, n_rows = 3) {
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    switch(ext,
      csv  = ,
      txt  = ,
      tsv  = ,
      dat  = {
        sep <- if (ext == "tsv") "\t" else sniff_delimiter(path)
        read.delim(path, sep = sep, nrows = n_rows, check.names = FALSE,
                   stringsAsFactors = FALSE)
      },
      xlsx = ,
      xls  = readxl::read_excel(path, n_max = n_rows),
      sav  = as.data.frame(haven::read_sav(path, n_max = n_rows)),
      dta  = as.data.frame(haven::read_dta(path, n_max = n_rows)),
      sas7bdat = as.data.frame(haven::read_sas(path, n_max = n_rows)),
      rds  = {
        obj <- readRDS(path)
        if (is.data.frame(obj)) head(obj, n_rows) else NULL
      },
      rda  = ,
      rdata = {
        env <- new.env()
        load(path, envir = env)
        dfs <- Filter(is.data.frame, as.list(env))
        if (length(dfs) > 0) head(dfs[[1]], n_rows) else NULL
      },
      NULL   # unsupported
    )
  }, error = function(e) {
    warning("Could not read ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

# ── Archive helpers ───────────────────────────────────────────────────────────

unpack_archive <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  stem <- tools::file_path_sans_ext(basename(path))
  dest <- file.path(dirname(path), stem)

  # Standalone compressed files (e.g. data.csv.gz, data.csv.bz2, data.csv.xz)
  # are NOT tar archives — decompress to a single file instead
  is_standalone <- (ext %in% c("gz", "bz2", "xz") &&
    !grepl("\\.(tar\\.(gz|bz2|xz)|tgz)$", tolower(basename(path))))

  if (is_standalone) {
    # dest is the decompressed file path (e.g. data.csv)
    if (file.exists(dest)) {
      message("  skipping (already unpacked): ", basename(path))
      return(dirname(path))
    }
    message("  decompressing: ", basename(path), " → ", dest)
    tryCatch({
      if (ext == "gz") {
        con_in <- gzfile(path, "rb")
      } else if (ext == "bz2") {
        con_in <- bzfile(path, "rb")
      } else {
        con_in <- xzfile(path, "rb")
      }
      con_out <- file(dest, "wb")
      on.exit({ close(con_in); close(con_out) })
      while (length(chunk <- readBin(con_in, "raw", n = 1048576L)) > 0)
        writeBin(chunk, con_out)
      dirname(path)
    }, error = function(e) {
      warning("Failed to decompress ", basename(path), ": ", conditionMessage(e))
      NULL
    })
  } else {
    if (dir.exists(dest)) {
      message("  skipping (already unpacked): ", basename(path))
      return(dest)
    }

    dir.create(dest, recursive = TRUE)
    message("  unpacking: ", basename(path), " → ", dest)

    tryCatch({
      if (ext == "zip") {
        utils::unzip(path, exdir = dest)
      } else {
        # tar, tgz, tar.gz, tar.bz2, tar.xz — untar auto-detects compression
        utils::untar(path, exdir = dest)
      }
      dest
    }, error = function(e) {
      warning("Failed to unpack ", basename(path), ": ", conditionMessage(e))
      NULL
    })
  }
}

# ── Rule-based classification ─────────────────────────────────────────────────

classify_by_rules <- function(path) {
  # tolower() normalises .R/.Rmd/.QMD etc. so ext_map keys can be plain lowercase
  fname <- tolower(basename(path))
  ext   <- tools::file_ext(fname)
  stem  <- tools::file_path_sans_ext(fname)

  # 1. Name pattern takes priority (catches codebooks saved as .xlsx, .csv, etc.)
  for (label in names(RULES$name_patterns)) {
    if (grepl(RULES$name_patterns[[label]], stem, perl = TRUE)) {
      return(list(label = label, certain = TRUE))
    }
  }

  # 2. Unambiguous extension
  if (ext %in% names(RULES$ext_map)) {
    # txt is a common catch-all — mark uncertain
    certain <- !(ext %in% c("txt"))
    return(list(label = RULES$ext_map[[ext]], certain = certain))
  }

  # 3. Ambiguous extensions (csv, xlsx, xls, html) and unknowns — send to LLM
  list(label = NA_character_, certain = FALSE)
}

# ── Column type classification ─────────────────────────────────────────────────

# Rule-based classification of a single data column.
# Returns list(col_type, ambiguous, numeric_values, n_coerced):
#   col_type      : character label from VALID_COL_TYPES, or NA when LLM is needed
#   ambiguous     : TRUE when the column should be sent to the LLM
#   numeric_values: numeric vector for stat computation (may be normalised),
#                   or NULL when the column is non-numeric
#   n_coerced     : integer count of values coerced to NA during normalisation,
#                   or NA_integer_ when no normalisation was applied
classify_col_type_rules <- function(col_name, values) {
  x_noNA  <- values[!is.na(values)]
  n_noNA  <- length(x_noNA)

  # Rule 1: all NA
  if (n_noNA == 0)
    return(list(col_type = "empty", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_))

  n_unique <- length(unique(x_noNA))

  # Rule 2: binary (≤ 2 unique non-NA values)
  if (n_unique <= 2)
    return(list(col_type = "binary", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_))

  # Rule 3: possible ID column — name matches common ID patterns AND all non-NA
  #         values are whole numbers.  Route to LLM rather than hard-classifying
  #         because ID columns appear in too many forms to rule-classify reliably.
  id_pat <- "(?i)(^|\\b)(id|subj|subject|participant|pp|ppt|pid|respondent)(\\b|$|[_\\-]?\\d)"
  if (grepl(id_pat, col_name, perl = TRUE)) {
    vals_num <- suppressWarnings(as.numeric(as.character(x_noNA)))
    if (!any(is.na(vals_num)) && all(vals_num == floor(vals_num)))
      return(list(col_type = NA_character_, ambiguous = TRUE,
                  numeric_values = suppressWarnings(as.numeric(as.character(values))),
                  n_coerced = NA_integer_))
  }

  # Rule 4: date — try as.Date on a sample of up to 20 unique string values
  char_sample <- as.character(unique(x_noNA))[seq_len(min(20, n_unique))]
  n_date_ok   <- sum(vapply(char_sample, function(v) {
    tryCatch(!is.na(as.Date(v)), warning = function(w) FALSE, error = function(e) FALSE)
  }, logical(1)))
  if (n_date_ok / length(char_sample) >= 0.70)
    return(list(col_type = "date", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_))

  # Rule 5: free text — long median string length
  if (median(nchar(as.character(x_noNA))) > 40)
    return(list(col_type = "text", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_))

  # Rule 6: numeric column
  if (is.numeric(values)) {
    if (n_unique > 20)
      return(list(col_type = "continuous", ambiguous = FALSE, numeric_values = values,
                  n_coerced = NA_integer_))
    # 3–20 unique values → ambiguous (LLM decides ordinal/categorical/continuous/binary)
    return(list(col_type = NA_character_, ambiguous = TRUE, numeric_values = values,
                n_coerced = NA_integer_))
  }

  # Rule 7: comma-decimal normalisation for character columns
  x_sub  <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA), fixed = TRUE)))
  pct_ok <- sum(!is.na(x_sub)) / n_noNA
  if (pct_ok >= 0.95) {
    num_vec    <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    n_coerced  <- sum(is.na(x_sub))  # non-NA values that failed conversion
    return(list(col_type = "continuous_comma_decimal", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = n_coerced))
  }
  if (pct_ok >= 0.80) {
    num_vec    <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    n_coerced  <- sum(is.na(x_sub))  # non-NA values that failed conversion
    return(list(col_type = "continuous_outliers_excluded", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = n_coerced))
  }

  # Rule 8: categorical (character, few short unique values)
  if (n_unique <= 10 && median(nchar(as.character(x_noNA))) <= 20)
    return(list(col_type = "categorical", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_))

  # Rule 9: text fallback for character columns
  list(col_type = "text", ambiguous = FALSE, numeric_values = NULL, n_coerced = NA_integer_)
}

# ── LLM helpers ───────────────────────────────────────────────────────────────

# Strip markdown fences and stray backticks the LLM may add around values
extract_json <- function(txt) {
  txt <- trimws(txt)
  # Remove ```json ... ``` or ``` ... ``` wrappers
  txt <- gsub("^```(?:json)?\\s*|\\s*```$", "", txt, perl = TRUE)
  txt <- trimws(txt)
  # Extract only the outermost JSON array [...] — discard any prose the LLM
  # appended after the closing bracket (e.g. "(Note: paths truncated ...)")
  m <- regexpr("(?s)\\[.*\\]", txt, perl = TRUE)
  if (m != -1) txt <- regmatches(txt, m)
  txt
}

clean_llm_values <- function(df) {
  # Strip backticks the LLM wraps around individual field values, e.g. `data`
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- gsub("`", "", df[[col]], fixed = TRUE)
    }
  }
  df
}

# Run an LLM prompt over a character vector in batches, joining results by a
# key column.  Returns a data.frame with columns c(key_col, extra_cols).
llm_batch <- function(paths, system_prompt, user_prefix, key_col, extra_cols,
                      fallback_vals) {
  chunks     <- split(paths, ceiling(seq_along(paths) / LLM_BATCH_SIZE))
  all_parsed <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    chunk_paths <- chunks[[i]]
    chunk_text  <- paste(seq_along(chunk_paths), chunk_paths, sep = ". ", collapse = "\n")
    chunk_input <- paste0(user_prefix, "\n\n", chunk_text,
                          "\n\nReturn ONLY a JSON array with exactly ", length(chunk_paths),
                          " objects — one per path above. Echo every path character-for-character.",
                          " No truncation. No notes. No text outside the array.")

    raw <- llm(system_prompt = system_prompt, text = chunk_input)

    needed_cols    <- c(key_col, extra_cols)
    chunk_fallback <- as.data.frame(
      c(list(paths = chunk_paths),
        setNames(lapply(fallback_vals, rep, length(chunk_paths)), extra_cols)),
      stringsAsFactors = FALSE
    )
    names(chunk_fallback)[1] <- key_col

    all_parsed[[i]] <- tryCatch({
      result <- clean_llm_values(jsonlite::fromJSON(extract_json(raw$answer)))
      if (!all(needed_cols %in% names(result))) {
        stop("Response missing fields: ",
             paste(setdiff(needed_cols, names(result)), collapse = ", "))
      }
      # Deduplicate LLM response on the key column before merging — duplicate
      # echoed keys (e.g. same basename returned twice) cause a many-to-many
      # join that drops rows.  Keep first occurrence of each key.
      result <- result[!duplicated(result[[key_col]]), needed_cols, drop = FALSE]
      merged <- merge(
        data.frame(x = chunk_paths, stringsAsFactors = FALSE) |> setNames(key_col),
        result,
        by = key_col, all.x = TRUE
      )
      for (col in extra_cols) merged[[col]][is.na(merged[[col]])] <- fallback_vals[[col]]
      merged[match(chunk_paths, merged[[key_col]]), ]
    }, error = function(e) {
      message("── LLM raw response (chunk ", i, ") ──\n", raw$answer,
              "\n────────────────────────────────")
      warning("Chunk ", i, " failed: ", conditionMessage(e), "; using fallback values")
      chunk_fallback
    })
  }

  do.call(rbind, all_parsed)
}
