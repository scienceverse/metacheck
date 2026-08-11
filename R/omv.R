# Read jamovi (.omv) data archives ------------------------------------------
#
# An .omv file is a ZIP archive bundling a dataset, its variable metadata
# (measurement level + value labels), and the analyses that were run. It is the
# jamovi counterpart of a JASP .jasp file and shares almost all of its on-disk
# layout, so this reader is a deliberate parallel of R/jasp.R and returns the SAME
# contract, letting the SAME codebook extractor (`.extract_haven_labels`) consume
# an .omv with no special-casing — exactly as .jasp and .sav already do.
#
# On-disk layout (verified against real v8.0 and v11.0 archives in the corpus):
#   * metadata.json — dataSet$fields (name, dataType, measureType, columnType),
#     dataSet$rowCount. dataType is Integer / Decimal / Text.
#   * xdata.json    — value labels (same [code, "label", ...] shape as JASP).
#   * data.bin      — COLUMN-MAJOR binary. Per column, `rowCount` cells:
#       - dataType "Decimal" -> little-endian double (8 bytes); NaN = missing.
#       - dataType "Integer" -> int32 (4 bytes); -2147483648 = missing.
#       - dataType "Text"    -> int32 (4 bytes) INDEX into strings.bin; <0 = NA.
#   * strings.bin   — a NUL-separated pool of strings; a Text cell's int is the
#     0-based index into this pool.
#   * "NN <name>/analysis" — one protobuf blob per analysis. NOT parsed as a
#     structure; the embedded reproducible R-syntax call is extracted as text
#     (Level 1), paralleling JASP's `.jasp_analyses_summary`.
#
# Unlike JASP (which switched to an embedded SQLite database at v0.17), jamovi
# keeps the metadata.json + data.bin layout across archive versions, so there is a
# single reader here rather than two.
#
# This file also provides export_omv_html(), which is unrelated to reading
# data: it re-exports the archive's OWN rendered index.html (jamovi's output
# view, tables and plots) as one portable, image-inlined HTML file.

.OMV_INT_MIN <- -2147483648    # jamovi/JASP integer missing-value sentinel

#' Read a jamovi (.omv) file
#'
#' Extracts the dataset, its variable metadata (measurement level, value labels)
#' and a summary of the analyses stored in a `.omv` archive. The jamovi
#' counterpart of [import_jasp()], returning the same structure so downstream
#' code (codebook extraction, data checks) treats an `.omv` like a `.jasp` or
#' `.sav`.
#'
#' @param path path to a `.omv` file
#'
#' @returns a list with `data` (a data.frame; labelled columns carry haven-style
#'   `label`/`labels` attributes), `columns` (a data.frame of `name` and `type`),
#'   `analyses` (a character vector, one entry per analysis: its name and, when
#'   recoverable, the reproducible R syntax), `format` (`"jamovi"`), and
#'   `data_file_path` (`NA`; jamovi does not record the original import path).
#' @export
import_omv <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.omv$", path, ignore.case = TRUE))
    stop("Not a .omv file: ", path)
  tmp <- tempfile("omv_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # suppressWarnings: unzip() warns ("error 1 in extracting from zip file") on a
  # non-zip before we turn the empty result into a clean error below.
  files <- tryCatch(suppressWarnings(utils::unzip(path, exdir = tmp)),
                    error = function(e) character(0))
  if (!length(files))
    stop("Could not open '", basename(path), "' as a .omv (zip) archive.")
  base <- basename(files)

  if (!any(base == "metadata.json") || !any(base == "data.bin"))
    stop("Unrecognised .omv: no 'metadata.json'/'data.bin' entry in ",
         basename(path), ".")

  meta <- jsonlite::fromJSON(files[base == "metadata.json"][[1]],
                             simplifyVector = FALSE)
  xdat <- if (any(base == "xdata.json"))
    jsonlite::fromJSON(files[base == "xdata.json"][[1]], simplifyVector = FALSE) else list()
  ds     <- meta$dataSet
  fields <- ds$fields
  nrow   <- ds$rowCount %||% 0L

  # String pool (strings.bin): NUL-separated; a Text cell's int is its 0-based
  # index here. Absent when the dataset has no text columns.
  pool <- character(0)
  if (any(base == "strings.bin")) {
    sp   <- files[base == "strings.bin"][[1]]
    raw  <- readBin(sp, "raw", n = file.info(sp)$size)
    # Split on the NUL byte separator. The NUL cannot be written as a source-code
    # string literal (R rejects it), so split the raw vector on 0x00 and convert
    # each run to a character string.
    idx  <- which(raw == as.raw(0L))
    starts <- c(1L, idx + 1L)
    ends   <- c(idx - 1L, length(raw))
    pool <- vapply(seq_along(starts), function(k) {
      if (ends[k] < starts[k]) return("")
      rawToChar(raw[starts[k]:ends[k]])
    }, character(1))
  }

  con <- file(files[base == "data.bin"][[1]], "rb")
  on.exit(close(con), add = TRUE)

  cols  <- vector("list", length(fields))
  cmeta <- vector("list", length(fields))
  for (j in seq_along(fields)) {
    f  <- fields[[j]]
    dt <- f$dataType %||% "Integer"
    mt <- f$measureType %||% "Nominal"
    if (identical(dt, "Decimal")) {
      v <- readBin(con, "double", n = nrow, size = 8, endian = "little")
      v[is.nan(v)] <- NA
    } else {
      idx <- readBin(con, "integer", n = nrow, size = 4, endian = "little")
      if (identical(dt, "Text")) {
        # Text: the int is a 0-based index into the NUL-separated string pool.
        v <- rep(NA_character_, length(idx))
        ok <- !is.na(idx) & idx >= 0L & idx < length(pool)
        v[ok] <- pool[idx[ok] + 1L]
      } else {
        v <- idx
        v[v == .OMV_INT_MIN] <- NA
        labs <- .omv_labels(f, xdat)
        if (length(labs)) attr(v, "labels") <- labs
      }
    }
    # Variable label: jamovi's `description` (or `title`) when it differs from the
    # machine name. Same idea as JASP's `title`.
    ttl <- f$description %||% f$title %||% ""
    if (nzchar(ttl) && !identical(ttl, f$name))
      attr(v, "label") <- as.character(ttl)
    cols[[j]]  <- v
    cmeta[[j]] <- data.frame(name = f$name, type = mt, stringsAsFactors = FALSE)
  }

  df <- stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE),
                        vapply(fields, function(f) f$name, character(1)))
  # as.data.frame drops per-column attributes; re-attach label/labels.
  for (j in seq_along(cols)) {
    if (!is.null(attr(cols[[j]], "labels"))) attr(df[[j]], "labels") <- attr(cols[[j]], "labels")
    if (!is.null(attr(cols[[j]], "label")))  attr(df[[j]], "label")  <- attr(cols[[j]], "label")
  }

  list(data = df,
       columns = do.call(rbind, cmeta),
       analyses = .omv_analyses_summary(files),
       format = "jamovi",
       data_file_path = NA_character_)
}

# Value labels for one field as a haven-style named vector (names = label
# strings, values = integer codes). Reads the field's own `labels`, else
# xdata.json[name]$labels. Each entry is [code, "label", ...] — same shape as
# JASP, so this parallels `.jasp_binary_labels`.
.omv_labels <- function(field, xdat) {
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

# One-line-per-analysis summary of a .omv's analyses, for the recovered "code"
# artifact. Each analysis is a protobuf blob under "NN <name>/analysis"; we do
# NOT parse the protobuf (Level 2) but extract the embedded reproducible R-syntax
# call as text (Level 1) — jamovi stores it as a clean contiguous <pkg>::<fn>(...)
# string. Analyses with no recoverable syntax (e.g. an empty placeholder) are
# reported by name only. Parallels JASP's `.jasp_analyses_summary`.
.omv_analyses_summary <- function(files) {
  entries <- files[basename(files) == "analysis"]
  if (!length(entries)) return(character(0))
  # Order by the "NN <name>" folder that holds each analysis.
  entries <- entries[order(basename(dirname(entries)))]
  out <- vapply(seq_along(entries), function(i) {
    name <- basename(dirname(entries[i]))
    raw  <- tryCatch(readBin(entries[i], "raw",
                             n = file.info(entries[i])$size),
                     error = function(e) raw(0))
    # Replace every non-printable byte with a space rather than dropping it, so a
    # protobuf framing byte (e.g. the string-field tag 0x52 = "R") is not glued to
    # the package name that follows it across a dropped separator byte. The `::`
    # anchor in .omv_extract_syntax then stops its leftward walk at that space.
    keep <- raw >= as.raw(32) & raw <= as.raw(126)
    raw[!keep] <- as.raw(32)
    txt  <- rawToChar(raw)
    syntax <- .omv_extract_syntax(txt)
    if (nzchar(syntax)) sprintf("%d. %s  |  %s", i, name, syntax)
    else sprintf("%d. %s", i, name)
  }, character(1))
  out
}

# Extract the reproducible R call `<pkg>::<fn>( ... )` from an analysis blob's
# printable text. jamovi stores it verbatim as a protobuf string field, but the
# field's length-prefix framing byte (the tag 0x52 = ASCII "R") sits directly
# before the package name, and the call contains nested parens (e.g. `vars(...)`).
# So we anchor on `::`, walk LEFT to the true package-name start (stopping at the
# framing byte, not consuming it), then walk RIGHT matching PARENTHESES so a
# nested `vars(...)` does not end the call early. Returns "" when no call found.
.omv_extract_syntax <- function(txt) {
  m <- regexpr("::[A-Za-z0-9_.]+[ \t]*\\(", txt)
  if (m[[1]] < 0) return("")
  colons <- m[[1]]
  # Package name: the run of [A-Za-z0-9.] immediately left of "::". The framing
  # byte before it is not in that class, so it is naturally excluded.
  left <- colons - 1L
  while (left >= 1L && grepl("[A-Za-z0-9.]", substr(txt, left, left))) left <- left - 1L
  start <- left + 1L
  # Right: from the first "(" after "::", match to its balancing ")".
  open <- colons + attr(m, "match.length") - 1L    # position of "("
  depth <- 0L; end <- open
  for (k in open:nchar(txt)) {
    ch <- substr(txt, k, k)
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) { end <- k; break } }
  }
  if (depth != 0L) return("")                        # unbalanced -> give up
  gsub("[[:space:]]+", " ", trimws(substr(txt, start, end)))
}

#' Export a jamovi (.omv) file's own rendered output as standalone HTML
#'
#' A `.omv` archive already bundles a fully rendered `index.html` -- jamovi's
#' own output view, complete with result tables and any plots -- alongside the
#' data (see the file header). This extracts that `index.html` as-is and
#' inlines every plot it references (an `<img src="....png">` under one of
#' the per-analysis `resources/` folders) as a base64 `data:` URI, so the
#' result is a single, portable, self-contained file that looks exactly like
#' jamovi's own output window, with no external image files to keep alongside
#' it.
#'
#' @param path path to a `.omv` file
#' @param out path to write the HTML file to; defaults to `path` with its
#'   extension replaced by `.html`, written alongside the source file
#'
#' @returns the path written to, invisibly
#' @export
export_omv_html <- function(path, out = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.omv$", path, ignore.case = TRUE))
    stop("Not a .omv file: ", path)
  if (is.null(out)) out <- sub("\\.omv$", ".html", path, ignore.case = TRUE)

  tmp <- tempfile("omvexport_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- tryCatch(suppressWarnings(utils::unzip(path, exdir = tmp)),
                    error = function(e) character(0))
  if (!length(files)) stop("Could not open '", basename(path), "' as a .omv (zip) archive.")
  base <- basename(files)

  hp <- files[base == "index.html"]
  if (!length(hp)) stop("No 'index.html' in ", basename(path), "; nothing to export.")

  html <- readChar(hp[[1]], file.info(hp[[1]])$size, useBytes = TRUE)
  writeLines(.html_inline_images(html, tmp), out, useBytes = TRUE)
  invisible(out)
}

# Inline every <img src="..."> the HTML references as a resources-relative
# path -- a URL-encoded path (jamovi's own resources folders can contain
# spaces, e.g. "12 gamljGlmMixed/resources/...") is decoded before it is
# treated as a filesystem path. Non-image src values (there are none in
# practice, but a defensive check costs nothing) and missing files are left
# untouched. Shared verbatim between export_omv_html() and export_jasp_html()
# (R/jasp.R) rather than factored out, since each format's reader is meant to
# stay a self-contained file.
.html_inline_images <- function(html, root) {
  srcs <- regmatches(html, gregexpr('src="([^"]+\\.(png|jpe?g|gif))"', html,
                                    ignore.case = TRUE))[[1]]
  for (src in unique(srcs)) {
    rel <- sub('^src="(.*)"$', "\\1", src, ignore.case = TRUE)
    if (grepl("^(https?:)?//|^data:", rel, ignore.case = TRUE)) next
    img_path <- file.path(root, utils::URLdecode(rel))
    if (!file.exists(img_path)) next
    ext <- tolower(tools::file_ext(img_path))
    mime <- if (ext == "png") "image/png" else if (ext == "gif") "image/gif" else "image/jpeg"
    data_uri <- paste0("data:", mime, ";base64,", base64enc::base64encode(img_path))
    html <- sub(src, paste0('src="', data_uri, '"'), html, fixed = TRUE)
  }
  html
}
