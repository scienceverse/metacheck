# Read JASP (.jasp) data archives -------------------------------------------
#
# A .jasp file is a ZIP archive bundling a dataset, its variable metadata, the
# analyses that were run, and cached results. metacheck reads the DATA and the
# variable metadata (measurement level + value labels) so a .jasp can serve as
# both a data source and its own codebook — exactly as an SPSS .sav does.
#
# Two on-disk formats exist and both are handled:
#   * BINARY (JASP <= ~0.16): entries metadata.json + xdata.json + data.bin.
#     data.bin is COLUMN-MAJOR; each cell is a little-endian double (8 bytes) when
#     the column's measureType is "Continuous", else an int32 (4 bytes). Column
#     order/type come from metadata.json$dataSet$fields; value labels from
#     xdata.json; missing = NaN (double) / -2147483648 (int32).
#   * SQLITE (JASP >= ~0.17): a single entry internal.sqlite (a SQLite database).
#     Table `Columns` holds name/type/order; a `DataSet_<n>` table holds the
#     values as paired `Column_<id>_DBL`/`Column_<id>_INT` fields; `Labels` maps
#     integer codes to label strings.
#
# The reader returns columns as their raw values with haven-style attributes
# attached — attr(col, "label") = variable label, attr(col, "labels") = a named
# vector (names = label strings, values = numeric codes) — so the SAME codebook
# extractor used for haven/.sav data (`.extract_haven_labels`) consumes a .jasp
# with no special-casing.
#
# Layout verified against the JASP source: JASPImporter (tag v0.8.3.1) for the
# binary loop and CommonData/databaseinterface.cpp for the SQLite schema.

.JASP_INT_MIN <- -2147483648    # JASP's integer missing-value sentinel

#' Read a JASP (.jasp) file
#'
#' Extracts the dataset, its variable metadata (measurement level, value labels)
#' and the list of analyses stored in a `.jasp` archive. Handles both the legacy
#' binary format and the modern embedded-SQLite format.
#'
#' @param path path to a `.jasp` file
#'
#' @returns a list with `data` (a data.frame; labelled columns carry haven-style
#'   `label`/`labels` attributes), `columns` (a data.frame of `name` and `type`),
#'   `analyses` (the parsed `analyses.json`, or `NULL`), `format`
#'   (`"binary"` or `"sqlite"`), and `data_file_path` (the original source path
#'   recorded in the archive, or `NA`).
#' @export
#' @keywords internal
read_jasp <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  tmp <- tempfile("jasp_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- tryCatch(utils::unzip(path, exdir = tmp), error = function(e) character(0))
  if (!length(files)) stop("Could not open '", basename(path), "' as a .jasp (zip) archive.")
  base <- basename(files)

  analyses <- NULL
  aj <- files[base == "analyses.json"]
  if (length(aj))
    analyses <- tryCatch(jsonlite::fromJSON(aj[[1]], simplifyVector = FALSE),
                         error = function(e) NULL)

  out <- if (any(base == "internal.sqlite"))
    .read_jasp_sqlite(files[base == "internal.sqlite"][[1]])
  else if (any(base == "data.bin"))
    .read_jasp_binary(files)
  else
    stop("Unrecognised .jasp: no 'data.bin' or 'internal.sqlite' entry in ",
         basename(path), ".")

  out$analyses <- analyses
  out
}

# ── Legacy binary format ─────────────────────────────────────────────────────
.read_jasp_binary <- function(files) {
  base <- basename(files)
  meta <- jsonlite::fromJSON(files[base == "metadata.json"][[1]],
                             simplifyVector = FALSE)
  xdat <- if (any(base == "xdata.json"))
    jsonlite::fromJSON(files[base == "xdata.json"][[1]], simplifyVector = FALSE) else list()
  ds     <- meta$dataSet
  fields <- ds$fields
  nrow   <- ds$rowCount
  con <- file(files[base == "data.bin"][[1]], "rb")
  on.exit(close(con), add = TRUE)

  cols <- vector("list", length(fields))
  cmeta <- vector("list", length(fields))
  for (j in seq_along(fields)) {
    f  <- fields[[j]]
    mt <- f$measureType %||% "Nominal"
    is_scale <- identical(mt, "Continuous")
    if (is_scale) {
      v <- readBin(con, "double", n = nrow, size = 8, endian = "little")
      v[is.nan(v)] <- NA
    } else {
      v <- readBin(con, "integer", n = nrow, size = 4, endian = "little")
      v[v == .JASP_INT_MIN] <- NA
      labs <- .jasp_binary_labels(f, xdat)          # named vec: names=label, val=code
      if (length(labs)) attr(v, "labels") <- labs
    }
    if (!is.null(f$title) && nzchar(f$title) && !identical(f$title, f$name))
      attr(v, "label") <- as.character(f$title)
    cols[[j]]  <- v
    cmeta[[j]] <- data.frame(name = f$name, type = mt, stringsAsFactors = FALSE)
  }
  df <- stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE),
                        vapply(fields, function(f) f$name, character(1)))
  # as.data.frame drops per-column attributes; re-attach them.
  for (j in seq_along(cols)) {
    if (!is.null(attr(cols[[j]], "labels"))) attr(df[[j]], "labels") <- attr(cols[[j]], "labels")
    if (!is.null(attr(cols[[j]], "label")))  attr(df[[j]], "label")  <- attr(cols[[j]], "label")
  }
  list(data = df, columns = do.call(rbind, cmeta), format = "binary",
       data_file_path = meta$dataFilePath %||% NA_character_)
}

# Value labels for one binary-format field, as a haven-style named vector
# (names = label strings, values = integer codes). Reads the field's own
# `labels`, falling back to xdata.json[name]$labels. Each label entry is
# [code, "label", filterAllows].
.jasp_binary_labels <- function(field, xdat) {
  lst <- field$labels
  if (is.null(lst) || !length(lst)) {
    x <- xdat[[field$name]]
    lst <- if (!is.null(x)) x$labels else NULL
  }
  if (is.null(lst) || !length(lst)) return(numeric(0))
  codes <- suppressWarnings(as.numeric(vapply(lst, function(l) as.character(l[[1]]), character(1))))
  labs  <- vapply(lst, function(l) as.character(l[[2]]), character(1))
  keep  <- !is.na(codes) & nzchar(labs)
  if (!any(keep)) return(numeric(0))
  stats::setNames(codes[keep], labs[keep])
}

# ── Modern SQLite format ─────────────────────────────────────────────────────
.read_jasp_sqlite <- function(sqlite_path) {
  if (!requireNamespace("RSQLite", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE))
    stop("Reading a modern (SQLite) .jasp file needs the 'RSQLite' and 'DBI' packages.")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cmeta <- DBI::dbGetQuery(con,
    "SELECT id, name, columnType, colIdx FROM Columns ORDER BY colIdx")
  tabs <- DBI::dbListTables(con)
  dtab <- grep("^DataSet_", tabs, value = TRUE)
  if (!length(dtab)) stop("No DataSet_* table in internal.sqlite.")
  dtab <- dtab[[1]]
  phys <- DBI::dbListFields(con, dtab)

  labels_for <- function(cid) {
    l <- DBI::dbGetQuery(con, sprintf(
      "SELECT value, label FROM Labels WHERE columnId = %d ORDER BY ordering",
      as.integer(cid)))
    if (!nrow(l)) return(numeric(0))
    keep <- !is.na(l$value) & nzchar(l$label %||% "")
    if (!any(keep)) return(numeric(0))
    stats::setNames(as.numeric(l$value[keep]), l$label[keep])
  }

  cols <- vector("list", nrow(cmeta))
  for (i in seq_len(nrow(cmeta))) {
    cid <- cmeta$id[i]; ctype <- tolower(cmeta$columnType[i])
    dblc <- sprintf("Column_%d_DBL", cid)
    intc <- sprintf("Column_%d_INT", cid)
    is_scale <- ctype == "scale"
    phys_col <- if (is_scale && dblc %in% phys) dblc else if (intc %in% phys) intc else dblc
    v <- DBI::dbGetQuery(con, sprintf('SELECT "%s" AS v FROM "%s" ORDER BY rowNumber',
                                      phys_col, dtab))$v
    if (!is_scale) {
      v <- suppressWarnings(as.numeric(v))
      v[v == .JASP_INT_MIN] <- NA
      labs <- labels_for(cid)
      if (length(labs)) attr(v, "labels") <- labs
    }
    if (!is.na(cmeta$name[i]) && !is.null(cmeta$name[i])) {
      ttl <- tryCatch(DBI::dbGetQuery(con, sprintf(
        "SELECT title FROM Columns WHERE id = %d", as.integer(cid)))$title, error = function(e) NULL)
      if (length(ttl) && !is.na(ttl[[1]]) && nzchar(ttl[[1]]) && !identical(ttl[[1]], cmeta$name[i]))
        attr(v, "label") <- as.character(ttl[[1]])
    }
    cols[[i]] <- v
  }
  df <- stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE), cmeta$name)
  for (i in seq_along(cols)) {
    if (!is.null(attr(cols[[i]], "labels"))) attr(df[[i]], "labels") <- attr(cols[[i]], "labels")
    if (!is.null(attr(cols[[i]], "label")))  attr(df[[i]], "label")  <- attr(cols[[i]], "label")
  }

  dfp <- tryCatch(DBI::dbGetQuery(con, "SELECT dataFilePath FROM DataSets LIMIT 1")$dataFilePath,
                  error = function(e) NA_character_)
  list(data = df,
       columns = data.frame(name = cmeta$name, type = cmeta$columnType,
                            stringsAsFactors = FALSE),
       format = "sqlite",
       data_file_path = if (length(dfp)) dfp[[1]] else NA_character_)
}

# Human-readable one-line-per-analysis summary of a .jasp's analyses.json, for
# the recovered "code" artifact. Falls back to names/titles when options are
# opaque. `analyses` is the parsed analyses.json (list or list$analyses).
.jasp_analyses_summary <- function(analyses) {
  if (is.null(analyses)) return(character(0))
  al <- if (!is.null(analyses$analyses)) analyses$analyses else analyses
  if (!is.list(al) || !length(al)) return(character(0))
  vapply(seq_along(al), function(i) {
    a <- al[[i]]
    if (!is.list(a)) return(sprintf("%d. %s", i, as.character(a)))
    title  <- a$title %||% a$name %||% "analysis"
    module <- a$module %||% NA_character_
    sprintf("%d. %s%s", i, title,
            if (!is.na(module)) sprintf("  [module: %s]", module) else "")
  }, character(1))
}
