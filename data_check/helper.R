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

  if (dir.exists(dest)) {
    message("  skipping (already unpacked): ", basename(path))
    return(dest)
  }

  dir.create(dest, recursive = TRUE)
  message("  unpacking: ", basename(path), " → ", dest)

  tryCatch({
    if (ext == "zip") {
      utils::unzip(path, exdir = dest)
    } else if (ext %in% c("tar", "tgz", "gz", "bz2", "xz")) {
      utils::untar(path, exdir = dest)
    }
    dest
  }, error = function(e) {
    warning("Failed to unpack ", basename(path), ": ", conditionMessage(e))
    NULL
  })
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
