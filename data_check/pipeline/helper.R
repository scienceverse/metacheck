# ── Output directory helper ───────────────────────────────────────────────────

# Return the per-paper output directory path, creating it if necessary.
# paper_id must be a character string (leading zeros are meaningful).
paper_output_dir <- function(paper_id) {
  dir_path <- file.path("./data_check/outputs", paper_id)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  dir_path
}

# ── Text file helpers ─────────────────────────────────────────────────────────

# Sniff the delimiter of a text file by counting candidate characters in the
# first non-empty line.  Returns the most frequent one, defaulting to ",".
sniff_delimiter <- function(path) {
  line <- character(0)
  con  <- file(path, "r")
  on.exit(close(con))
  for (i in seq_len(10)) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) break                         # EOF: empty file
    l <- trimws(line)
    if (nchar(l) > 0 && !startsWith(l, "#")) break      # found a non-blank, non-comment line
  }
  if (length(line) == 0) return(",")     # empty file — return safe default
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
        df  <- suppressWarnings(
          read.delim(path, sep = sep, nrows = n_rows, check.names = FALSE,
                     stringsAsFactors = FALSE)
        )
        # If any character column contains invalid UTF-8 bytes (e.g. Windows-1252
        # encoded files), retry with latin1 so downstream string ops don't crash.
        has_invalid <- any(vapply(df, function(col) {
          is.character(col) && any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
        }, logical(1)))
        if (has_invalid) {
          df <- suppressWarnings(
            read.delim(path, sep = sep, nrows = n_rows, check.names = FALSE,
                       stringsAsFactors = FALSE, fileEncoding = "latin1")
          )
        }
        df
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
    # Re-throw time-limit errors so callers can detect and report timeouts.
    if (grepl("time limit", conditionMessage(e), ignore.case = TRUE)) stop(e)
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
                n_coerced = NA_integer_, is_numeric = FALSE))

  n_unique <- length(unique(x_noNA))

  # Rule 2: binary (≤ 2 unique non-NA values)
  if (n_unique <= 2)
    return(list(col_type = "binary", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  # Rule 3: possible ID column — name matches common ID patterns AND all non-NA
  #         values are whole numbers.  Route to LLM rather than hard-classifying
  #         because ID columns appear in too many forms to rule-classify reliably.
  #         is_numeric = FALSE: LLM may correctly return "id"; do not fall back to "continuous".
  id_pat <- "(?i)(^|\\b)(id|subj|subject|participant|pp|ppt|pid|respondent)(\\b|$|[_\\-]?\\d)"
  if (grepl(id_pat, col_name, perl = TRUE)) {
    vals_num <- suppressWarnings(as.numeric(as.character(x_noNA)))
    if (!any(is.na(vals_num)) && all(vals_num == floor(vals_num)))
      return(list(col_type = NA_character_, ambiguous = TRUE,
                  numeric_values = suppressWarnings(as.numeric(as.character(values))),
                  n_coerced = NA_integer_, is_numeric = FALSE))
  }

  # Rule 4: date — try as.Date on a sample of up to 20 unique string values
  char_sample <- as.character(unique(x_noNA))[seq_len(min(20, n_unique))]
  n_date_ok   <- sum(vapply(char_sample, function(v) {
    tryCatch(!is.na(as.Date(v)), warning = function(w) FALSE, error = function(e) FALSE)
  }, logical(1)))
  if (n_date_ok / length(char_sample) >= 0.70)
    return(list(col_type = "date", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  # Rule 5: free text — long median string length
  if (median(nchar(as.character(x_noNA))) > 40)
    return(list(col_type = "text", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  # Rule 6a: decimal numeric — any fractional value is unambiguously continuous.
  # Fires before Rule 6 to avoid routing VAS / ratio scales to the LLM.
  if (is.numeric(values) && any(x_noNA != floor(x_noNA)))
    return(list(col_type = "continuous", ambiguous = FALSE, numeric_values = values,
                n_coerced = NA_integer_, is_numeric = FALSE))

  # Rule 6: integer numeric column.
  # > 20 unique values → continuous without LLM.
  # 3–20 unique values → route to LLM (ordinal vs continuous vs categorical).
  #   is_numeric = TRUE flags integer-numeric columns so a post-LLM fallback can
  #   replace "unknown" → "continuous" when the LLM cannot determine the type.
  if (is.numeric(values)) {
    if (n_unique > 20)
      return(list(col_type = "continuous", ambiguous = FALSE, numeric_values = values,
                  n_coerced = NA_integer_, is_numeric = FALSE))
    return(list(col_type = NA_character_, ambiguous = TRUE, numeric_values = values,
                n_coerced = NA_integer_, is_numeric = TRUE))
  }

  # Rule 7: comma-decimal normalisation for character columns
  x_sub  <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x_noNA), fixed = TRUE)))
  pct_ok <- sum(!is.na(x_sub)) / n_noNA
  if (pct_ok >= 0.95) {
    num_vec    <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    n_coerced  <- sum(is.na(x_sub))  # non-NA values that failed conversion
    return(list(col_type = "continuous_comma_decimal", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = n_coerced, is_numeric = FALSE))
  }
  if (pct_ok >= 0.80) {
    num_vec    <- suppressWarnings(as.numeric(gsub(",", ".", as.character(values), fixed = TRUE)))
    n_coerced  <- sum(is.na(x_sub))  # non-NA values that failed conversion
    return(list(col_type = "continuous_outliers_excluded", ambiguous = FALSE,
                numeric_values = num_vec, n_coerced = n_coerced, is_numeric = FALSE))
  }

  # Rule 8: categorical (character, few short unique values)
  if (n_unique <= 10 && median(nchar(as.character(x_noNA))) <= 20)
    return(list(col_type = "categorical", ambiguous = FALSE, numeric_values = NULL,
                n_coerced = NA_integer_, is_numeric = FALSE))

  # Rule 9: text fallback for character columns
  list(col_type = "text", ambiguous = FALSE, numeric_values = NULL,
       n_coerced = NA_integer_, is_numeric = FALSE)
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

    llm_params <- if (!is.null(getOption("llm_temperature"))) list(temperature = getOption("llm_temperature")) else list()
    raw <- llm(system_prompt = system_prompt, text = chunk_input, params = llm_params)

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

# ── Codebook parsing helpers ──────────────────────────────────────────────────

# Normalise a variable name for case-insensitive, whitespace-tolerant matching.
# Applies tolower, trims whitespace, collapses interior spaces, strips
# leading/trailing underscores and dots.
normalize_varname <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("[_]+", " ", x)   # treat underscores as word separators (e.g. SSS_total → sss total)
  x <- gsub("\\s+", " ", x)  # collapse any resulting multiple spaces
  x <- gsub("^[\\.]+|[\\.]+$", "", x)  # strip leading/trailing dots
  x <- trimws(x)
  x
}

# Normalise a label string for semantic-equivalence comparison.
# Strips possessives, punctuation, pluralising "s", and extra whitespace so
# that minor wording differences (e.g. "Participants' age" vs "Participant age")
# normalise to the same string.
normalize_label <- function(x) {
  x <- tolower(x)
  x <- gsub("'s|'s|\u2019s|\u2018s", "", x, perl = TRUE)  # strip possessives (straight + curly)
  x <- gsub("[^a-z0-9 ]", " ", x)                          # non-alphanumeric → space
  # Strip trailing "s" from words of ≥ 8 total chars (handles "participants" → "participant",
  # "feelings" → "feeling", "responses" → "response") while leaving short words intact
  x <- gsub("\\b([a-z]{7,})s\\b", "\\1", x, perl = TRUE)
  x <- gsub("\\s+", " ", trimws(x))                        # collapse whitespace
  x
}

# Scan a data.frame's column headers for a "variable name" column and a
# "label/description" column.  Returns list(var_col, lab_col) or NULL.
.find_codebook_cols <- function(col_names) {
  var_col <- grep(
    paste0("(?i)^(var(iable)?|name|column|field|variable_?name|varname|",
           "variable[_ ]?label|var[_ ]?label|item)$"),
    col_names, perl = TRUE, value = TRUE
  )[1]
  lab_col <- grep(
    paste0("(?i)^(label|description|desc|definition|meaning|explanation|text|",
           "label[_ ]?text|question|question[_ ]?text|variable[_ ]?description)$"),
    col_names, perl = TRUE, value = TRUE
  )[1]
  if (is.na(var_col) || is.na(lab_col)) return(NULL)
  list(var_col = var_col, lab_col = lab_col)
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
    group             = NA_character_,
    stringsAsFactors  = FALSE
  )
}

# Extract embedded variable labels from a Haven-labelled data.frame (SPSS/DTA).
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
    group             = NA_character_,
    stringsAsFactors  = FALSE
  )
}

# Map free-text experiment context strings to canonical group codes.
# e.g. "Experiment 1" -> "ex1", "Study 2a" -> "ex2a", "Pilot 1" -> "pilot1"
.infer_group <- function(context_str) {
  vapply(context_str, function(s) {
    if (is.null(s) || is.na(s) || !nzchar(trimws(as.character(s))))
      return(NA_character_)
    s <- trimws(as.character(s))
    # Pilot takes priority — never reclassify a pilot as "ex"
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

# Strip RTF control codes from a character string, returning plain text.
# Used internally by .extract_rich_text() for .rtf files.
.strip_rtf <- function(text) {
  text <- gsub("\\\\[a-z]+\\-?[0-9]*\\s?", " ", text)  # control words
  text <- gsub("\\\\[^a-z\n]",             " ", text)  # control symbols
  text <- gsub("[{}]",                      "",  text)  # braces
  text <- gsub("\\s+",                      " ", text)  # collapse whitespace
  trimws(text)
}

# Extract plain text from a rich-text or binary codebook file.
# Returns a single character string (possibly empty) on any failure.
# Supports: docx (officer), pdf (pdftools), rtf (regex strip),
#           doc (textutil on macOS), odt (unzip + XML strip).
# officer >= 0.7.0 and pdftools >= 3.0.0 must be installed (both are already present).
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
        pages <- pdftools::pdf_text(path)
        paste(pages, collapse = "\n")
      },
      rtf = {
        lines <- readLines(path, warn = FALSE)
        .strip_rtf(paste(lines, collapse = "\n"))
      },
      doc = {
        # Legacy binary Word (OLE2) — readLines() produces binary garbage.
        # Use macOS textutil to convert to plain text; fall back to empty string.
        if (nzchar(Sys.which("textutil"))) {
          lines <- system2("textutil", c("-convert", "txt", "-stdout",
                                         shQuote(path)),
                           stdout = TRUE, stderr = FALSE)
          paste(lines, collapse = "\n")
        } else ""
      },
      odt = {
        # OpenDocument is a ZIP containing content.xml — readLines() returns
        # binary ZIP noise.  Unzip content.xml and strip XML tags instead.
        tmp <- tempfile()
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        dir.create(tmp)
        result <- tryCatch({
          utils::unzip(path, files = "content.xml", exdir = tmp)
          xml_path <- file.path(tmp, "content.xml")
          if (!file.exists(xml_path)) return("")
          raw <- paste(readLines(xml_path, warn = FALSE), collapse = "\n")
          # Strip XML tags, decode common entities, collapse whitespace
          txt <- gsub("<[^>]+>", " ", raw)
          txt <- gsub("&amp;",  "&", txt, fixed = TRUE)
          txt <- gsub("&lt;",   "<", txt, fixed = TRUE)
          txt <- gsub("&gt;",   ">", txt, fixed = TRUE)
          txt <- gsub("&apos;", "'", txt, fixed = TRUE)
          txt <- gsub("&quot;", '"', txt, fixed = TRUE)
          txt <- gsub("\\s+",   " ", txt)
          trimws(txt)
        }, error = function(e) "")
        result
      },
      ""  # unknown extension
    )
  }, error = function(e) "")
}

# Read a codebook file and return a data.frame of variable definitions with
# columns: codebook_variable, label, codebook_source, group.
# Returns NULL (with warning) on failure, oversized file, or no definitions found.
# Relies on MAX_CODEBOOK_FILE_MB, MAX_CODEBOOK_LLM_CALLS, CODEBOOK_PARSE_PROMPT
# being defined in the calling script (same pattern as LLM_BATCH_SIZE in llm_batch).
parse_codebook <- function(path) {
  if (!file.exists(path)) {
    warning("Codebook file not found: ", basename(path))
    return(NULL)
  }
  file_mb <- file.info(path)$size / 1048576
  if (!is.na(file_mb) && file_mb > MAX_CODEBOOK_FILE_MB) {
    warning("Skipping codebook (", round(file_mb), " MB > ", MAX_CODEBOOK_FILE_MB,
            " MB limit): ", basename(path))
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path))
  src <- basename(path)

  # ── Structured extraction (rule-based) ──────────────────────────────────────
  result <- tryCatch({
    switch(ext,
      csv = , tsv = , dat = {
        sep <- if (ext == "tsv") "\t" else sniff_delimiter(path)
        # Read without treating any row as a header so we can scan for it.
        raw <- tryCatch(
          read.delim(path, sep = sep, header = FALSE, check.names = FALSE,
                     stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        if (is.null(raw) || nrow(raw) == 0) return(NULL)
        # Retry with latin1 if UTF-8 produces invalid bytes (mirrors read_data_head).
        has_invalid <- any(vapply(raw, function(col) {
          is.character(col) && any(is.na(iconv(col, from = "UTF-8", to = "UTF-8")))
        }, logical(1)))
        if (has_invalid) {
          raw <- tryCatch(
            read.delim(path, sep = sep, header = FALSE, check.names = FALSE,
                       stringsAsFactors = FALSE, fileEncoding = "latin1"),
            error = function(e) NULL
          )
          if (is.null(raw) || nrow(raw) == 0) return(NULL)
        }
        # Scan rows 1..CODEBOOK_HEADER_LOOKAHEAD for a row whose values match the
        # expected codebook column patterns.
        header_row <- NA_integer_
        lookahead  <- min(nrow(raw), CODEBOOK_HEADER_LOOKAHEAD)
        for (k in seq_len(lookahead)) {
          candidate <- trimws(as.character(raw[k, ]))
          if (!is.null(.find_codebook_cols(candidate))) {
            header_row <- k
            break
          }
        }
        if (is.na(header_row)) return(NULL)
        names(raw) <- trimws(as.character(raw[header_row, ]))
        df <- raw[seq(header_row + 1L, nrow(raw)), , drop = FALSE]
        rownames(df) <- NULL
        .extract_structured_codebook(df, src)
      },
      xlsx = , xls = {
        df <- tryCatch(
          as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE),
          error = function(e) NULL
        )
        .extract_structured_codebook(df, src)
      },
      sav = {
        df <- haven::read_sav(path)
        .extract_haven_labels(df, src)
      },
      dta = {
        df <- haven::read_dta(path)
        .extract_haven_labels(df, src)
      },
      docx = , doc = , pdf = , rtf = , odt = {
        text <- .extract_rich_text(path, ext)
        if (nchar(trimws(text)) < 10) {
          warning("No extractable text from ", src, " (", ext, ")")
          return(NULL)
        }
        strsplit(text, "\n")[[1]]  # return lines vector; handled below
      },
      NULL  # unsupported extension — fall through to LLM via readLines
    )
  }, error = function(e) {
    warning("Structured codebook parse failed for ", src, ": ", conditionMessage(e))
    NULL
  })

  # Rich-text formats return a character vector of lines (not a data.frame).
  # Route them directly to the shared LLM chunk loop below.
  rich_lines <- if (is.character(result) && !is.data.frame(result)) result else NULL

  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    result$group        <- .infer_group(result$group)
    result$parse_method <- "structured"
    return(result)
  }

  # ── LLM fallback for unstructured / unparseable files ────────────────────────
  lines <- if (!is.null(rich_lines)) {
    rich_lines
  } else {
    tryCatch(
      readLines(path, warn = FALSE),
      error = function(e) {
        warning("Cannot read ", src, " for LLM parsing: ", conditionMessage(e))
        character(0)
      }
    )
  }
  if (length(lines) == 0) return(NULL)

  .run_llm_chunk_loop(lines, src)
}

# Shared LLM chunking loop used by parse_codebook() for both plain-text and
# rich-text codebook sources.  Returns a data.frame or NULL.
.run_llm_chunk_loop <- function(lines, src) {
  chunks    <- split(lines, ceiling(seq_along(lines) / 100))
  max_calls <- min(length(chunks), MAX_CODEBOOK_LLM_CALLS)
  all_vars  <- vector("list", max_calls)

  for (i in seq_len(max_calls)) {
    chunk_text <- paste(chunks[[i]], collapse = "\n")
    llm_params <- if (!is.null(getOption("llm_temperature"))) list(temperature = getOption("llm_temperature")) else list()
    raw <- tryCatch(
      llm(system_prompt = CODEBOOK_PARSE_PROMPT,
          text = paste0("Extract all variable definitions from this codebook text:\n\n",
                        chunk_text),
          params = llm_params),
      error = function(e) {
        warning("LLM codebook parse failed (chunk ", i, " of ", src, "): ",
                conditionMessage(e))
        list(answer = "[]")
      }
    )
    all_vars[[i]] <- tryCatch({
      parsed <- jsonlite::fromJSON(extract_json(raw$answer))
      if (!is.data.frame(parsed) ||
          !all(c("variable_name", "label") %in% names(parsed))) return(NULL)
      parsed <- parsed[nzchar(trimws(as.character(parsed$variable_name))), , drop = FALSE]
      if (nrow(parsed) == 0) return(NULL)
      ec <- if ("experiment_context" %in% names(parsed))
        as.character(parsed$experiment_context) else NA_character_
      data.frame(
        codebook_variable = as.character(parsed$variable_name),
        label             = as.character(parsed$label),
        codebook_source   = src,
        group             = .infer_group(ec),
        stringsAsFactors  = FALSE
      )
    }, error = function(e) NULL)
  }

  result <- do.call(rbind, Filter(Negate(is.null), all_vars))
  if (is.null(result) || nrow(result) == 0) {
    message("  no variables extracted from: ", src)
    return(NULL)
  }
  result$parse_method <- "llm"
  result
}

# Match columns_df (from _columns.csv) against codebook_vars_df.
# Returns a _labels.csv-shaped data.frame covering every row of columns_df.
# Handles experiment-group scoping and conflict detection.
match_column_labels <- function(columns_df, codebook_vars_df,
                                column_match_prompt = NULL,
                                label_merge_prompt  = NULL) {
  # Support both "group" and "experiment_group" column names (historic schema variants)
  col_group <- if ("group" %in% names(columns_df)) columns_df$group else
               if ("experiment_group" %in% names(columns_df)) columns_df$experiment_group else
               rep(NA_character_, nrow(columns_df))

  make_empty <- function() {
    data.frame(
      paper_id          = columns_df$paper_id,
      source_file       = columns_df$source_file,
      column_name       = columns_df$column_name,
      group             = col_group,
      label             = NA_character_,
      codebook_variable = NA_character_,
      label_source      = NA_character_,
      label_status      = "unlabelled",
      label_method      = NA_character_,
      stringsAsFactors  = FALSE
    )
  }

  if (is.null(codebook_vars_df) || nrow(codebook_vars_df) == 0) return(make_empty())
  if (is.null(columns_df)       || nrow(columns_df) == 0)       return(make_empty())

  norm_col <- normalize_varname(columns_df$column_name)
  norm_var <- normalize_varname(codebook_vars_df$codebook_variable)

  n                <- nrow(columns_df)
  label_out        <- rep(NA_character_, n)
  cbk_var_out      <- rep(NA_character_, n)
  src_out          <- rep(NA_character_, n)
  status_out       <- rep("unlabelled",  n)
  label_method_out <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    nc <- norm_col[i]
    cg <- col_group[i]

    name_idx <- which(norm_var == nc)
    if (length(name_idx) == 0) next

    matches  <- codebook_vars_df[name_idx, , drop = FALSE]
    scoped   <- matches[!is.na(matches$group), , drop = FALSE]
    unscoped <- matches[ is.na(matches$group), , drop = FALSE]

    same_group_scoped <- scoped[!is.na(scoped$group) & scoped$group == cg, , drop = FALSE]
    applicable        <- rbind(unscoped, same_group_scoped)
    other_scoped      <- scoped[!is.na(scoped$group) & scoped$group != cg, , drop = FALSE]

    if (nrow(applicable) == 0) {
      if (nrow(other_scoped) > 0) {
        # Name exists only in a different experiment's codebook
        status_out[i]  <- "ambiguous_experiment"
        label_out[i]   <- paste(unique(other_scoped$label),             collapse = " | ")
        cbk_var_out[i] <- paste(unique(other_scoped$codebook_variable), collapse = " | ")
        src_out[i]     <- paste(unique(other_scoped$codebook_source),   collapse = " | ")
      }
      next
    }

    distinct_labels <- unique(applicable$label)
    if (length(distinct_labels) > 1) {
      # Rule-based equivalence check: normalise labels and re-check uniqueness
      norm_labels <- normalize_label(distinct_labels)
      if (length(unique(norm_labels)) == 1) {
        # All labels normalise to the same string — pick the longest original label
        canonical <- distinct_labels[which.max(nchar(distinct_labels))]
        status_out[i]        <- "labelled"
        label_out[i]         <- canonical
        cbk_var_out[i]       <- applicable$codebook_variable[1]
        src_out[i]           <- paste(unique(applicable$codebook_source), collapse = " | ")
        label_method_out[i]  <- "merged_rules"
      } else {
        # Labels differ semantically — flag for LLM resolution or leave as conflict
        status_out[i]  <- "conflicting_definition"
        label_out[i]   <- paste(distinct_labels,                           collapse = " | ")
        cbk_var_out[i] <- paste(unique(applicable$codebook_variable),     collapse = " | ")
        src_out[i]     <- paste(unique(applicable$codebook_source),       collapse = " | ")
      }
    } else {
      status_out[i]  <- "labelled"
      label_out[i]   <- distinct_labels[1]
      cbk_var_out[i] <- applicable$codebook_variable[1]
      src_out[i]     <- paste(unique(applicable$codebook_source), collapse = " | ")
    }
  }

  # Set label_method for rule-matched rows (merged_rules already set above)
  label_method_out[status_out == "labelled" & is.na(label_method_out)] <- "rules"

  # ── LLM merge tier: resolve remaining conflicting_definition rows ─────────────
  if (!is.null(label_merge_prompt)) {
    conflict_idx <- which(status_out == "conflicting_definition")
    if (length(conflict_idx) > 0) {
      # Build batch input: one entry per unique conflicting column name
      conflict_cols <- unique(columns_df$column_name[conflict_idx])
      batch_input <- lapply(conflict_cols, function(cn) {
        idx1     <- conflict_idx[columns_df$column_name[conflict_idx] == cn][1]
        raw_labs <- strsplit(label_out[idx1], " | ", fixed = TRUE)[[1]]
        list(column = cn, labels = raw_labs)
      })
      prompt_body <- paste0("Variables to check:\n",
                            jsonlite::toJSON(batch_input, auto_unbox = TRUE))
      llm_params <- if (!is.null(getOption("llm_temperature"))) list(temperature = getOption("llm_temperature")) else list()
      merge_resp <- tryCatch(
        llm(system_prompt = label_merge_prompt, text = prompt_body, params = llm_params),
        error = function(e) {
          warning("LLM label-merge call failed: ", conditionMessage(e))
          list(answer = "[]")
        }
      )
      merge_pairs <- tryCatch({
        parsed <- jsonlite::fromJSON(extract_json(merge_resp$answer),
                                     simplifyDataFrame = TRUE)
        if (is.data.frame(parsed) && nrow(parsed) > 0 &&
            all(c("column", "equivalent", "canonical") %in% names(parsed)))
          parsed else data.frame()
      }, error = function(e) data.frame())

      if (nrow(merge_pairs) > 0) {
        for (k in seq_len(nrow(merge_pairs))) {
          if (!isTRUE(merge_pairs$equivalent[k])) next
          canonical <- as.character(merge_pairs$canonical[k])
          if (is.na(canonical) || !nzchar(canonical)) next
          apply_idx <- conflict_idx[
            columns_df$column_name[conflict_idx] == merge_pairs$column[k]
          ]
          for (i in apply_idx) {
            label_out[i]        <- canonical
            status_out[i]       <- "labelled"
            label_method_out[i] <- "merged_llm"
          }
        }
      }
    }
  }

  # ── LLM secondary pass (T005–T009) ───────────────────────────────────────────
  if (!is.null(column_match_prompt)) {

    # T006: Collect candidate sets
    unlabelled_idx      <- which(status_out == "unlabelled")
    unlabelled_norm_cols <- unique(norm_col[unlabelled_idx])

    matched_norm_vars <- unique(normalize_varname(
      cbk_var_out[status_out == "labelled" & !is.na(cbk_var_out)]
    ))
    unmatched_vars_df <- codebook_vars_df[
      !normalize_varname(codebook_vars_df$codebook_variable) %in% matched_norm_vars,
      , drop = FALSE
    ]

    if (length(unlabelled_norm_cols) > 0 && nrow(unmatched_vars_df) > 0) {

      # T007: Build prompt body and call LLM
      col_list <- paste(seq_along(unlabelled_norm_cols),
                        columns_df$column_name[match(unlabelled_norm_cols, norm_col)],
                        sep = ". ", collapse = "\n")
      var_list <- paste(seq_len(nrow(unmatched_vars_df)),
                        unmatched_vars_df$codebook_variable,
                        sep = ". ", collapse = "\n")
      prompt_body <- paste0(
        "Data columns (unlabelled):\n", col_list,
        "\n\nCodebook variables (unmatched):\n", var_list
      )

      llm_params <- if (!is.null(getOption("llm_temperature"))) list(temperature = getOption("llm_temperature")) else list()
      llm_resp <- tryCatch(
        llm(system_prompt = column_match_prompt, text = prompt_body, params = llm_params),
        error = function(e) {
          warning("LLM column-matching call failed: ", conditionMessage(e))
          list(answer = "[]")
        }
      )

      # T008: Parse and validate response
      pairs_df <- tryCatch({
        json_txt <- extract_json(llm_resp$answer)
        parsed   <- jsonlite::fromJSON(json_txt, simplifyDataFrame = TRUE)
        if (is.data.frame(parsed) && nrow(parsed) > 0 &&
            all(c("column_name", "codebook_variable") %in% names(parsed))) {
          parsed
        } else {
          data.frame(column_name = character(0), codebook_variable = character(0),
                     stringsAsFactors = FALSE)
        }
      }, error = function(e) {
        data.frame(column_name = character(0), codebook_variable = character(0),
                   stringsAsFactors = FALSE)
      })

      norm_unmatched_vars <- normalize_varname(unmatched_vars_df$codebook_variable)

      # Validate: both sides must be in the submitted candidate sets
      valid_pairs <- pairs_df[
        normalize_varname(pairs_df$column_name)      %in% unlabelled_norm_cols &
        normalize_varname(pairs_df$codebook_variable) %in% norm_unmatched_vars,
        , drop = FALSE
      ]

      # T009: Apply valid pairs
      for (k in seq_len(nrow(valid_pairs))) {
        pair_norm_col <- normalize_varname(valid_pairs$column_name[k])
        pair_norm_var <- normalize_varname(valid_pairs$codebook_variable[k])

        row_idxs  <- which(norm_col == pair_norm_col & status_out == "unlabelled")
        var_row   <- which(norm_unmatched_vars == pair_norm_var)[1]

        if (length(row_idxs) == 0 || is.na(var_row)) next

        for (i in row_idxs) {
          label_out[i]        <- unmatched_vars_df$label[var_row]
          cbk_var_out[i]      <- unmatched_vars_df$codebook_variable[var_row]
          src_out[i]          <- unmatched_vars_df$codebook_source[var_row]
          status_out[i]       <- "llm"
          label_method_out[i] <- "llm"
        }
      }
    }
  }

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
    stringsAsFactors  = FALSE
  )
}
