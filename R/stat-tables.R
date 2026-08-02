# Extract rendered statistical RESULT TABLES from JASP (.jasp) and jamovi (.omv)
# files, and serialise them as STATO-typed statistical-output JSON.
#
# Both formats bundle a rendered `index.html` inside their (zip) archive holding
# every analysis result as standard HTML <table>s: an <h1>-<h4> heading names the
# analysis, a header row names the statistics (t, df, p, Cohen's d, ...), and
# each body row is a result. This reads those tables (Layer 1: lossless tidy
# extraction of ALL tables) and, for the ISA export, types each column via
# stato_type_column() (Layer 2 STATO where a class exists, Layer 3 fallback to
# the header text otherwise). See R/jasp.R / R/omv.R for the DATA readers; this
# is about the OUTPUT.

#' Read the statistical result tables from a JASP or jamovi file
#'
#' Opens a `.jasp` or `.omv` archive and returns every result table as a tidy
#' data frame together with the analysis it belongs to. No statistics knowledge
#' is applied here — this is the lossless extraction layer; semantic typing
#' happens downstream in [stat_output_json()] / [stat_results_long()].
#'
#' Three sources are tried, in order of fidelity:
#' 1. `.jasp` — the archive's own `analyses.json`, which holds the structured
#'    results;
#' 2. `.omv` — jamovi's protobuf-serialised `AnalysisResponse` blobs, decoded
#'    natively (see `inst/schema/jamovi/PROVENANCE.md`);
#' 3. either — the rendered `index.html`, as a fallback when the above are
#'    absent or unreadable.
#'
#' The structured sources are preferred because they carry each column's machine
#' `name` even when its displayed header is blank (the HTML then yields an
#' unnamed column), store values at full precision (a p of `0.000632` rather
#' than the rendered `"< .001"`), and mark missing cells explicitly instead of
#' as a rendered dash.
#'
#' @param path path to a `.jasp` or `.omv` file
#'
#' @returns a list with one element per result table, each a list of `analysis`
#'   (the nearest heading above the table), `title` (the table's own caption /
#'   first heading, when distinguishable), `data` (a data.frame of the table
#'   as rendered, header row as column names), and `table_index` (1-based
#'   ordinal position of this table among all tables in the rendered document —
#'   there is no source line to attach, since a JASP/jamovi analysis is
#'   GUI-produced). Empty list when the archive has no `index.html` or no
#'   tables.
#' @export
read_stat_tables <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (!requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("rvest", quietly = TRUE))
    stop("reading result tables needs the 'xml2' and 'rvest' packages.",
         call. = FALSE)

  tmp <- tempfile("stattbl_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- tryCatch(suppressWarnings(utils::unzip(path, exdir = tmp)),
                    error = function(e) character(0))

  # PREFERRED for .jasp: the archive's own analyses.json carries the STRUCTURED
  # results (machine column names, native numeric values), which beats scraping
  # the rendered HTML on every axis — see .jasp_structured_tables(). Falls
  # through to the HTML path below when absent or unparseable (older JASP
  # versions, partial archives), so no file that worked before stops working.
  aj <- files[basename(files) == "analyses.json"]
  if (length(aj)) {
    structured <- tryCatch(.jasp_structured_tables(aj[[1]]),
                           error = function(e) NULL)
    if (!is.null(structured) && length(structured)) return(structured)
  }

  # PREFERRED for .omv: jamovi stores each analysis as a protobuf-serialised
  # AnalysisResponse at "<index> <name>/analysis". Same advantages as the JASP
  # path (machine column names, native precision, explicit MISSING cells); see
  # .jmv_structured_tables() and inst/schema/jamovi/PROVENANCE.md. Falls through
  # to HTML when the blobs are absent or undecodable.
  af <- files[basename(files) == "analysis"]
  if (length(af)) {
    # Sort by the numeric prefix jamovi puts on the containing folder, so table
    # order (and therefore table_index) follows the document, not the zip order.
    ord <- suppressWarnings(as.integer(sub("^\\s*(\\d+).*$", "\\1",
                                           basename(dirname(af)))))
    af <- af[order(ifelse(is.na(ord), .Machine$integer.max, ord))]
    structured <- tryCatch(.jmv_structured_tables(af), error = function(e) NULL)
    if (!is.null(structured) && length(structured)) return(structured)
  }

  # PREFERRED for .spv: SPSS Viewer files store each table's own dimension/
  # cell structure in the archive itself (a binary blob, plus for older files
  # an accompanying XML structure document) rather than only a rendered
  # index.html — see R/spv.R for the full reader (also home to the public
  # import_spv()), ported from GNU PSPP's GPL-licensed .spv decoder (the only
  # known public implementation of this undocumented format). Same advantages
  # as the JASP/jamovi structured paths: native numeric precision, and the
  # exact SPSS syntax that produced each table is recovered too (carried as
  # $syntax on each returned table). Falls through to nothing further when
  # absent or unparseable, since .spv archives have no rendered index.html
  # fallback.
  sv <- files[grepl("^outputViewer[0-9]+(_heading)?\\.xml$", basename(files))]
  if (length(sv)) {
    structured <- tryCatch(.spv_read(tmp), error = function(e) NULL)
    if (!is.null(structured) && length(structured)) return(structured)
  }

  hp <- files[basename(files) == "index.html"]
  if (!length(hp)) return(list())

  doc <- tryCatch(xml2::read_html(hp[[1]]), error = function(e) NULL)
  if (is.null(doc)) return(list())

  tables <- xml2::xml_find_all(doc, "//table")
  if (length(tables) == 0) return(list())

  out <- lapply(seq_along(tables), function(i) {
    tb <- tables[[i]]
    # The analysis heading: the nearest <h1>-<h4> that precedes this table.
    heads <- xml2::xml_find_all(
      tb, "preceding::*[self::h1 or self::h2 or self::h3 or self::h4]")
    analysis <- if (length(heads))
      trimws(xml2::xml_text(heads[[length(heads)]])) else NA_character_

    parsed <- .stat_table_parse(tb)
    if (is.null(parsed)) return(NULL)
    # table_index: this table's 1-based ordinal position among ALL <table>s in
    # the rendered document. JASP/jamovi analyses have no source line (they are
    # GUI-produced, not code-produced), so this ordinal stands in for a line
    # number as the positional locator result_id needs.
    list(analysis = analysis, title = parsed$title, data = parsed$data,
         table_index = i)
  })
  Filter(Negate(is.null), out)
}

# A stored number -> the character form the rest of the pipeline works in.
# Deliberately lets R choose fixed vs scientific notation (`format()`'s default,
# via as.character()): forcing scientific = FALSE turns a p-value like 6.58e-72
# into seventy zeros and a digit, which is unreadable and defeats the numeric
# re-parse downstream. 15 significant digits keeps full double precision without
# printing float noise.
.stat_num_to_chr <- function(v) {
  if (!is.finite(v)) return(as.character(v))       # Inf / NaN keep their names
  if (v == round(v) && abs(v) < 1e15)
    return(format(v, scientific = FALSE, trim = TRUE))   # whole numbers: 113 not 1.13e2
  format(v, digits = 15, trim = TRUE)
}

# Strip JASP's internal column-name encoding. JASP mangles the user's variable
# names into result-column names as
# "JaspColumn_.21._Encoded_pearson_p.value", where only the trailing part after
# "_Encoded_" names the statistic; the prefix identifies which encoded source
# column it belongs to. Keeping the whole string produces unreadable keys, so the
# encoded prefix is dropped and the statistic suffix kept. Column uniqueness is
# preserved by make.unique() in the caller. Names without the marker pass
# through untouched.
.jasp_clean_colname <- function(x) {
  x <- as.character(x %||% "")
  out <- sub("^JaspColumn_.*?_Encoded_", "", x)
  if (!nzchar(trimws(out))) x else out
}

# ── Structured JASP reader (analyses.json) ───────────────────────────────────
# A .jasp archive stores, alongside the rendered index.html, the analyses' OWN
# structured results in analyses.json. Reading those beats scraping the HTML:
#   * every column carries a machine `name` even when its display `title` is
#     blank or purely cosmetic. The HTML renders only the title, so a blank one
#     produced an unnamed column and a junk generated key (v1, v2, ...);
#   * values are native JSON numbers at full stored precision — a p of
#     0.000632, where the HTML shows the rounded display string "< .001";
#   * each analysis has a real name (RegressionLinear, TTestPairedSamples),
#     which identifies the TEST, where the HTML only has a heading that is
#     often blank;
#   * row-label columns are explicit fields, not a positional guess.
# Result elements are nested arbitrarily (an analysis' `results` holds tables,
# `collection`s of tables, images, titles), so tables are gathered by walking
# the tree for any node carrying a `schema$fields` + `data`.
#
# Emits the SAME shape read_stat_tables() returns from HTML (analysis / title /
# data / table_index), so every downstream consumer is unchanged.
.jasp_structured_tables <- function(analyses_json) {
  j <- jsonlite::fromJSON(analyses_json, simplifyVector = FALSE)
  analyses <- j$analyses
  if (is.null(analyses) || !length(analyses)) return(list())

  out <- list()
  for (an in analyses) {
    # The analysis-level label: prefer its title when non-blank, else its
    # module name (RegressionLinear), which is the more informative of the two
    # in practice — JASP commonly leaves the per-analysis title empty.
    an_title <- trimws(as.character(an$title %||% ""))
    an_name  <- trimws(as.character(an$name %||% ""))
    label <- if (nzchar(an_title)) an_title else
      if (nzchar(an_name)) an_name else NA_character_
    # JASP's own id for THIS analysis instance. Five separate regressions all
    # carry name "RegressionLinear", so the name cannot tell them apart — but
    # each has its own id (4, 5, 6, ...), and its ANOVA / Descriptives / Model
    # Summary / Coefficients tables all nest under it. Carrying the id through
    # is what lets the four tables of one analysis be recognised as one test
    # while keeping five same-named analyses distinct.
    an_id <- an$id %||% NA
    an_id <- if (length(an_id) == 1 && !is.na(an_id))
      as.character(an_id) else NA_character_

    # Depth-first walk collecting every table node under this analysis.
    collect <- function(node) {
      if (!is.list(node)) return(invisible(NULL))
      fields <- node[["schema"]][["fields"]]
      if (!is.null(fields) && !is.null(node[["data"]])) {
        df <- .jasp_table_to_df(fields, node[["data"]])
        if (!is.null(df) && nrow(df) && ncol(df)) {
          ttl <- trimws(as.character(node$title %||% ""))
          out[[length(out) + 1L]] <<- list(
            analysis = label,
            analysis_id = an_id,
            title = if (nzchar(ttl)) ttl else NA_character_,
            data = df)
        }
        return(invisible(NULL))     # a table's own children are cells, not tables
      }
      for (nm in seq_along(node)) collect(node[[nm]])
      invisible(NULL)
    }
    collect(an$results)
  }
  if (!length(out)) return(list())
  # table_index is assigned across the whole document, as the HTML path does,
  # so result_id's positional locator means the same thing either way.
  for (i in seq_along(out)) out[[i]]$table_index <- i
  out
}

# Turn one JASP result table's schema fields + data rows into a data.frame whose
# column names are the fields' machine `name`s. Cells are kept as CHARACTER, to
# match what the HTML path produces and what the downstream typing expects
# (.stat_is_label_col / stato_type_column read text); the numeric precision
# advantage is preserved because the character comes from the full stored
# number, not a rounded display string. A missing cell becomes "" (the
# placeholder filter downstream drops it).
.jasp_table_to_df <- function(fields, data_rows) {
  raw_nms <- vapply(fields, function(f) as.character(f$name %||% ""), character(1))
  keep <- nzchar(raw_nms)
  raw_nms <- raw_nms[keep]; fields <- fields[keep]
  if (!length(raw_nms) || !length(data_rows)) return(NULL)
  # Data rows are keyed by the RAW field name; the cleaned name is only for the
  # data.frame header, so look cells up with raw_nms and label with nms.
  nms <- vapply(raw_nms, .jasp_clean_colname, character(1), USE.NAMES = FALSE)

  cell <- function(v) {
    if (is.null(v) || length(v) == 0) return("")
    v <- v[[1]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) return("")
    if (is.numeric(v)) return(.stat_num_to_chr(v))
    as.character(v)
  }
  cols <- lapply(raw_nms, function(n)
    vapply(data_rows, function(row) cell(row[n]), character(1)))
  df <- as.data.frame(stats::setNames(cols, make.unique(nms)),
                      stringsAsFactors = FALSE, check.names = FALSE)
  df
}

# ── Minimal protobuf wire-format reader (for jamovi .omv) ────────────────────
# A .omv archive stores each analysis as a protobuf-serialised AnalysisResponse
# at "<index> <name>/analysis". Decoding those beats scraping index.html for the
# same reasons analyses.json does for JASP (machine column names, native
# precision, explicit MISSING) — see inst/schema/jamovi/PROVENANCE.md, which
# vendors the upstream jamovi.proto and records the field numbers used below.
#
# This is NOT a general protobuf implementation: it reads the wire format only
# (varint / 64-bit / length-delimited / 32-bit), which is all that is needed to
# walk the handful of messages involved. It deliberately avoids RProtoBuf (what
# jamovi's own R code uses) because that needs the protobuf C++ system libraries
# — a heavy dependency, especially on Windows, for a format this simple.

# Read one base-128 varint starting at `pos`; returns value and the next pos.
.pb_varint <- function(raw, pos) {
  res <- 0; shift <- 0
  repeat {
    if (pos > length(raw)) return(list(value = res, pos = pos))
    b <- as.integer(raw[pos]); pos <- pos + 1L
    res <- res + bitwAnd(b, 127L) * (2^shift)
    if (bitwAnd(b, 128L) == 0L) break
    shift <- shift + 7L
    if (shift > 63) break                      # malformed; stop rather than loop
  }
  list(value = res, pos = pos)
}

# Split one protobuf message into a list of list(field, wire, value). A
# length-delimited value is returned as the raw payload (decode further with
# another .pb_fields() call, or .pb_str()). Returns NULL on malformed input, so
# every caller can fall back rather than error.
.pb_fields <- function(raw) {
  out <- list(); pos <- 1L; n <- length(raw)
  while (pos <= n) {
    v <- .pb_varint(raw, pos); key <- v$value; pos <- v$pos
    if (key == 0) return(NULL)
    fld <- bitwShiftR(as.integer(key), 3L); wt <- bitwAnd(as.integer(key), 7L)
    if (wt == 0L) {
      v <- .pb_varint(raw, pos); val <- v$value; pos <- v$pos
    } else if (wt == 2L) {
      v <- .pb_varint(raw, pos); len <- v$value; pos <- v$pos
      if (len < 0 || pos + len - 1L > n) return(NULL)
      val <- if (len == 0) raw(0) else raw[seq(pos, length.out = len)]
      pos <- pos + len
    } else if (wt == 1L) {                                   # fixed64 (double)
      if (pos + 7L > n) return(NULL)
      val <- readBin(raw[pos:(pos + 7L)], "double", size = 8L, endian = "little")
      pos <- pos + 8L
    } else if (wt == 5L) {                                   # fixed32 (float)
      if (pos + 3L > n) return(NULL)
      val <- readBin(raw[pos:(pos + 3L)], "double", size = 4L, endian = "little")
      pos <- pos + 4L
    } else return(NULL)                        # groups (3/4) are not used here
    out[[length(out) + 1L]] <- list(field = fld, wire = wt, value = val)
  }
  out
}

# All values for one field number; the first value; a payload as a string.
.pb_all <- function(fields, n)
  lapply(Filter(function(x) x$field == n, fields), `[[`, "value")
.pb_get <- function(fields, n) {
  for (x in fields) if (x$field == n) return(x$value)
  NULL
}
.pb_str <- function(x) {
  if (is.null(x) || !length(x)) return("")
  tryCatch(rawToChar(x), error = function(e) "")
}

# jamovi field numbers (see inst/schema/jamovi/PROVENANCE.md).
.JMV_F <- list(
  resp_name = 3L, resp_results = 7L,
  el_name = 1L, el_title = 2L, el_table = 6L, el_group = 8L, el_array = 9L,
  container_elements = 1L,
  tbl_columns = 1L,
  col_name = 1L, col_title = 2L, col_type = 3L, col_format = 4L, col_cells = 7L,
  cell_i = 1L, cell_d = 2L, cell_s = 3L, cell_o = 4L)

# One ResultsCell -> a character scalar. An explicit Other/MISSING (field 4)
# and an absent oneof both become "", which the downstream placeholder filter
# drops — so a jamovi empty cell is identified STRUCTURALLY, not by matching
# the em dash the HTML renders it as.
.jmv_cell <- function(cell_raw) {
  f <- .pb_fields(cell_raw)
  if (is.null(f)) return("")
  for (x in f) {
    if (x$field == .JMV_F$cell_i) return(.stat_num_to_chr(x$value))
    if (x$field == .JMV_F$cell_d) return(.stat_num_to_chr(x$value))
    if (x$field == .JMV_F$cell_s) return(.pb_str(x$value))
    if (x$field == .JMV_F$cell_o) return("")            # MISSING / NOT_A_NUMBER
  }
  ""
}

# jamovi lays its Descriptives table out WIDE: one column per
# (variable x statistic) pair, named "<variable>[<statistic>]"
# (Unethicality_study1_positive[n], ...[mean], ...[sd]) with a single data row,
# plus a "stat[...]" label column carrying the statistic's display name. Read
# literally, every VARIABLE name becomes a statistic key — which is how a corpus
# ends up with junk "statistics" called blame_study1_positive.
#
# Note this is the OPPOSITE convention to the ANOVA correction suffixes
# (f[gg], p[hf]) that .stato_strip_variant() discards: there the prefix is the
# statistic and the bracket a variant; here the bracket is the statistic and the
# prefix the variable. The two are told apart by shape, not by name: this layout
# is a single row whose columns nearly all carry a bracket suffix, with the
# suffixes repeating across many different prefixes.
#
# The SAME inversion appears in jamovi's Contingency Tables, with one row per
# row-level of the crosstab and columns "1[count]", "2[expected]",
# ".total[pcRow]" — there the prefix is a LEVEL of the column variable rather
# than a variable name, but structurally it is identical: prefix = a data
# value, bracket = the statistic. Both are handled by the one pivot below;
# `nrow` may be 1 (Descriptives) or many (a crosstab), and each existing row is
# crossed with each prefix to produce one output row.
#
# Detected here and pivoted to the natural long form — one ROW per
# variable/level, one COLUMN per statistic — which is what every downstream step
# already expects.
.jmv_is_wide_descriptives <- function(nms, nrow_df) {
  if (length(nms) < 6L) return(FALSE)
  has_br <- grepl("\\[[^]]+\\]$", nms)
  # Allow a few un-bracketed leading label columns (a crosstab carries its row
  # variable as a plain column), but the bracketed grid must dominate.
  if (mean(has_br) < 0.6) return(FALSE)
  prefix <- sub("\\[[^]]+\\]$", "", nms[has_br])
  suffix <- sub(".*\\[([^]]+)\\]$", "\\1", nms[has_br])
  np <- length(unique(prefix)); ns <- length(unique(suffix))
  if (np < 2L || ns < 2L) return(FALSE)
  # WHICH SIDE holds the statistic? Both layouts are crossed grids, so the
  # crossing test below cannot tell them apart:
  #   Descriptives / crosstab : <variable-or-level>[<statistic>]  -> pivot
  #   repeated-measures ANOVA : <statistic>[<correction>]         -> DO NOT pivot
  # The prefixes of the ANOVA form are recognised statistic names (ss, df, ms,
  # F, p, ges, eta) while its brackets are correction labels (none, GG, HF);
  # the Descriptives form is the reverse. Compare how many of each side we can
  # type, and only pivot when the BRACKET is the more statistic-like side —
  # otherwise .stato_strip_variant() already handles it correctly downstream.
  typed_frac <- function(x) {
    x <- unique(tolower(trimws(x)))
    if (!length(x)) return(0)
    mean(vapply(x, function(k) nzchar(stato_type_column(k)$termSource), logical(1)))
  }
  if (typed_frac(prefix) > typed_frac(suffix)) return(FALSE)
  # The decisive signal is that the grid is (mostly) CROSSED: the same suffixes
  # recur across different prefixes, so the column set is variables x statistics
  # rather than a flat list of distinct columns. Counting which side is larger
  # is NOT a valid test — a Descriptives table commonly reports more statistics
  # (n, mean, sd, median, skew, quartiles, ...) than it has variables.
  sum(has_br) >= 0.75 * np * ns
}

.jmv_pivot_wide_descriptives <- function(df) {
  nms <- names(df)
  has_br <- grepl("\\[[^]]+\\]$", nms)
  prefix <- sub("\\[[^]]+\\]$", "", nms)
  suffix <- sub(".*\\[([^]]+)\\]$", "\\1", nms)
  # jamovi's own label column is named stat[...]; it holds the statistic's
  # display name, not data, so it is not a variable and is dropped.
  keep <- has_br & prefix != "stat" & nzchar(prefix)
  if (!any(keep)) return(df)
  vars  <- unique(prefix[keep])
  stats <- unique(suffix[keep])
  # Un-bracketed columns are the table's own row labels (a crosstab's row
  # variable); they are carried through, repeated for each prefix, so the
  # row identity is not lost by the pivot.
  label_cols <- which(!has_br & nzchar(nms))
  nr <- nrow(df)
  grid <- expand.grid(row = seq_len(nr), var = vars,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  out <- data.frame(name = grid$var, stringsAsFactors = FALSE,
                    check.names = FALSE)
  for (lc in label_cols)
    out[[nms[lc]]] <- as.character(df[[lc]])[grid$row]
  for (s in stats) {
    out[[s]] <- vapply(seq_len(nrow(grid)), function(g) {
      hit <- which(keep & prefix == grid$var[g] & suffix == s)
      if (!length(hit)) "" else as.character(df[[hit[1]]][grid$row[g]])
    }, character(1), USE.NAMES = FALSE)
  }
  out
}

# NOTE on transposed tables: an earlier version tried to DETECT and un-transpose
# jamovi's t-test tables (a `name` column whose cells are statistic names, one
# further column per test variant: stud/welc/mann/bf) by checking whether the
# first column's cells were recognised statistic keys. That heuristic was
# removed: it also fired on repeated-measures "Within Subjects Effects" tables,
# whose columns are legitimately the sphericity corrections (none/GG/HF), and
# transposing those turned 945 correct rows into junk keys to fix 47 wrong ones.
# jamovi declares each column's role directly (ResultsColumn.type/format, read
# in .jmv_table_to_df() and honoured by .stat_is_label_col()), so the label
# columns of a transposed table are identified from the format's own metadata
# rather than guessed at from shape.

# One ResultsTable -> a data.frame with the columns' machine NAMES as headers.
.jmv_table_to_df <- function(tbl_raw) {
  tf <- .pb_fields(tbl_raw)
  if (is.null(tf)) return(NULL)
  cols <- .pb_all(tf, .JMV_F$tbl_columns)
  if (!length(cols)) return(NULL)
  parsed <- lapply(cols, function(cb) {
    cf <- .pb_fields(cb)
    if (is.null(cf)) return(NULL)
    nm <- .pb_str(.pb_get(cf, .JMV_F$col_name))
    ti <- .pb_str(.pb_get(cf, .JMV_F$col_title))
    cells <- .pb_all(cf, .JMV_F$col_cells)
    # jamovi DECLARES each column's role: `type` is "text" for a label column
    # and number/integer for a statistic, and `format` can name the quantity
    # outright ("pvalue"). Captured here and carried as attributes so the
    # downstream label-vs-statistic decision can use the format's own answer
    # instead of guessing from cell contents — the same advantage JASP's
    # schema$fields gives, which jamovi has all along in ResultsColumn.
    list(name = if (nzchar(nm)) nm else ti,
         type = .pb_str(.pb_get(cf, .JMV_F$col_type)),
         format = .pb_str(.pb_get(cf, .JMV_F$col_format)),
         values = vapply(cells, .jmv_cell, character(1)))
  })
  parsed <- Filter(function(p) !is.null(p) && nzchar(p$name), parsed)
  if (!length(parsed)) return(NULL)
  nrows <- max(vapply(parsed, function(p) length(p$values), integer(1)))
  if (nrows == 0) return(NULL)
  cols_out <- lapply(parsed, function(p) {
    v <- p$values
    if (length(v) < nrows) v <- c(v, rep("", nrows - length(v)))
    v
  })
  final_names <- make.unique(vapply(parsed, `[[`, character(1), "name"))
  df <- as.data.frame(stats::setNames(cols_out, final_names),
                      stringsAsFactors = FALSE, check.names = FALSE)
  # Carry jamovi's DECLARED per-column roles alongside the data, keyed by the
  # final (uniquified) column name. .stat_is_label_col() consults these before
  # falling back to guessing from cell contents.
  attr(df, "col_roles") <- stats::setNames(
    lapply(parsed, function(p) list(type = p$type, format = p$format)),
    final_names)
  # jamovi's wide Descriptives / crosstab layout -> natural long form. The pivot
  # reshapes the columns, so the declared roles no longer line up and are
  # dropped rather than left pointing at the wrong columns.
  if (.jmv_is_wide_descriptives(names(df), nrow(df))) {
    df <- .jmv_pivot_wide_descriptives(df)
    attr(df, "col_roles") <- NULL
  }
  df
}

# Walk one ResultsElement tree, collecting every table into `acc`. Groups and
# arrays nest further elements under field 1.
.jmv_collect <- function(el_raw, analysis_label, acc, analysis_id = NA_character_) {
  f <- .pb_fields(el_raw)
  if (is.null(f)) return(acc)
  title <- .pb_str(.pb_get(f, .JMV_F$el_title))
  tbl <- .pb_get(f, .JMV_F$el_table)
  if (!is.null(tbl)) {
    df <- .jmv_table_to_df(tbl)
    if (!is.null(df) && nrow(df) && ncol(df))
      acc[[length(acc) + 1L]] <- list(
        analysis = analysis_label,
        analysis_id = analysis_id,
        title = if (nzchar(title)) title else NA_character_,
        data = df)
  }
  for (fld in c(.JMV_F$el_group, .JMV_F$el_array)) {
    sub <- .pb_get(f, fld)
    if (is.null(sub)) next
    sf <- .pb_fields(sub)
    if (is.null(sf)) next
    for (child in .pb_all(sf, .JMV_F$container_elements))
      acc <- .jmv_collect(child, analysis_label, acc, analysis_id)
  }
  acc
}

# Structured reader for one .omv: decode every "<n> <name>/analysis" blob.
# Emits the SAME shape read_stat_tables() returns from HTML.
.jmv_structured_tables <- function(analysis_files) {
  out <- list()
  for (p in analysis_files) {
    raw <- tryCatch(readBin(p, "raw", file.size(p)), error = function(e) NULL)
    if (is.null(raw) || !length(raw)) next
    top <- .pb_fields(raw)
    if (is.null(top)) next
    label <- .pb_str(.pb_get(top, .JMV_F$resp_name))
    results <- .pb_get(top, .JMV_F$resp_results)
    if (is.null(results)) next
    # jamovi's own analysisId (field 2), the counterpart to JASP's analysis id:
    # every table nested under this AnalysisResponse belongs to one analysis
    # instance, and two analyses of the same TYPE (two ttestIS) have different
    # ids. Falls back to the containing folder name, which jamovi prefixes with
    # the same number ("104 ttestIS").
    aid <- .pb_get(top, 2L)
    aid <- if (!is.null(aid) && length(aid) == 1) as.character(aid) else
      basename(dirname(p))
    out <- .jmv_collect(results, if (nzchar(label)) label else NA_character_,
                        out, aid)
  }
  if (!length(out)) return(list())
  for (i in seq_along(out)) out[[i]]$table_index <- i
  out
}

# Parse one JASP/jamovi result <table>. These have a MULTI-ROW header:
#   row 1 = the table title (one <th colspan=N>),
#   optional super-header rows (e.g. "95% Confidence Interval" spanning a pair),
#   then the granular header row naming the statistics (t, df, p, ...),
#   then data rows — all padded with empty spacer <th>/<td> cells.
# rvest::html_table mis-handles this (it takes the title row as the header), so
# we pick the header row ourselves: the LAST all-<th> row with >1 distinct
# non-empty label. Data rows are the <td> rows; spacer (all-empty) columns are
# dropped, and remaining columns aligned to the chosen header positionally.
.stat_table_parse <- function(tb) {
  rows <- xml2::xml_find_all(tb, ".//tr")
  if (length(rows) == 0) return(NULL)

  row_cells <- lapply(rows, function(r) xml2::xml_find_all(r, "./th | ./td"))
  is_th_row <- vapply(rows, function(r)
    length(xml2::xml_find_all(r, "./td")) == 0 &&
    length(xml2::xml_find_all(r, "./th")) > 0, logical(1))

  # Expand one <tr>'s cells to a GRID vector, repeating each cell's text across
  # its colspan. JASP/jamovi headers use colspan=2 while data cells use colspan=1
  # plus empty spacers, so a cell-index match shifts values under the wrong
  # header; expanding both to the common grid aligns them by position.
  grid_of <- function(cells) {
    txt <- trimws(gsub("[[:space:]]+", " ",
      vapply(cells, xml2::xml_text, character(1))))
    span <- vapply(cells, function(c) {
      cs <- suppressWarnings(as.integer(xml2::xml_attr(c, "colspan")))
      if (is.na(cs) || cs < 1) 1L else cs
    }, integer(1))
    rep(txt, times = span)
  }

  # Title = a single-cell (colspan) first header row, if present.
  title <- NA_character_
  if (length(rows) && is_th_row[[1]] && length(row_cells[[1]]) == 1)
    title <- trimws(xml2::xml_text(row_cells[[1]][[1]]))

  # Header row: the last all-<th> row whose non-empty labels are not all equal
  # (a title/super-header row is a single repeated value or one cell).
  header_idx <- NA_integer_
  for (j in which(is_th_row)) {
    labs <- grid_of(row_cells[[j]]); labs <- labs[nzchar(labs)]
    if (length(unique(labs)) > 1) header_idx <- j
  }
  if (is.na(header_idx)) return(NULL)
  header_grid <- grid_of(row_cells[[header_idx]])
  gw <- length(header_grid)                 # grid width

  data_rows <- which(!is_th_row & seq_along(rows) > header_idx)
  if (!length(data_rows)) return(NULL)

  mat <- do.call(rbind, lapply(data_rows, function(k) {
    v <- grid_of(row_cells[[k]])
    length(v) <- gw                          # align to the header grid width
    v
  }))

  # Collapse the grid back to columns: consecutive grid positions sharing the
  # same header belong to one column (colspan=2 headers span two grid slots);
  # take the first non-empty data value in each header's span.
  hdr <- header_grid
  # column boundaries = runs of identical header label
  runs <- rle(hdr)
  col_end <- cumsum(runs$lengths); col_start <- col_end - runs$lengths + 1L
  cols <- lapply(seq_along(runs$values), function(ci) {
    idx <- col_start[ci]:col_end[ci]
    vals <- apply(mat[, idx, drop = FALSE], 1, function(r) {
      nz <- r[nzchar(r)]; if (length(nz)) nz[[1]] else ""
    })
    vals
  })
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- runs$values

  # Drop footnote rows. JASP/jamovi append notes ("Note. ...", "* p < .05")
  # inside the table as a row that spans the full width, so after grid collapse
  # the same note text lands in most columns. Detect a row whose non-empty cells
  # are (a) a single repeated value, and (b) look like prose (start with "Note",
  # contain a footnote marker, or are much longer than a statistic), and drop it.
  is_footnote <- vapply(seq_len(nrow(df)), function(ri) {
    vals <- trimws(as.character(df[ri, , drop = TRUE])); vals <- vals[nzchar(vals)]
    if (length(vals) == 0) return(TRUE)             # wholly empty row
    if (length(unique(vals)) == 1) {
      v <- vals[[1]]
      return(grepl("^Note", v, ignore.case = TRUE) ||
             grepl("^[*a-z]?\\s*p\\s*[<>=]", v) ||
             nchar(v) > 40)
    }
    FALSE
  }, logical(1))
  df <- df[!is_footnote, , drop = FALSE]

  # Drop spacer columns (empty header AND all cells empty).
  keep <- vapply(seq_along(df), function(c)
    nzchar(names(df)[[c]]) || any(nzchar(df[[c]] %||% "")), logical(1))
  df <- df[, keep, drop = FALSE]
  nm <- names(df); nm[!nzchar(nm)] <- paste0("V", which(!nzchar(nm)))
  names(df) <- make.unique(nm)
  if (!nrow(df)) return(NULL)

  list(title = title, data = df)
}
