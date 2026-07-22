# Extract rendered statistical RESULT TABLES from JASP (.jasp) and jamovi (.omv)
# files, and serialise them as STATO-typed ISA-JSON.
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
#' Opens a `.jasp` or `.omv` archive, reads its embedded `index.html`, and
#' returns every rendered result table as a tidy data frame together with the
#' analysis heading it sits under. No statistics knowledge is applied here — this
#' is the lossless extraction layer; semantic typing happens in the ISA export.
#'
#' @param path path to a `.jasp` or `.omv` file
#'
#' @returns a list with one element per result table, each a list of `analysis`
#'   (the nearest heading above the table), `title` (the table's own caption /
#'   first heading, when distinguishable), and `data` (a data.frame of the table
#'   as rendered, header row as column names). Empty list when the archive has no
#'   `index.html` or no tables.
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
    list(analysis = analysis, title = parsed$title, data = parsed$data)
  })
  Filter(Negate(is.null), out)
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
