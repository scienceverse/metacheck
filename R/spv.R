# Read SPSS Viewer (".spv") output files ------------------------------------
#
# A .spv file is SPSS Statistics' own rendered output archive: a ZIP bundling
# every result table's structure (which analysis produced it, its exact
# syntax) and cell data, but -- unlike a JASP .jasp or jamovi .omv -- no
# rendered index.html fallback and no embedded chart/graph images (a chart is
# stored as a declarative <graph>/<image> structure-XML item that SPSS/PSPP
# renders at view-time, not a shipped raster blob).
#
# .spv has no public specification. Everything below is ported from GNU
# PSPP (GPL-3.0-or-later, https://git.savannah.gnu.org/cgit/pspp.git), the
# only known public implementation of this format's decoder: its grammar
# files describe the byte/XML layout field-by-field, and its C source
# (spv.c, spv-light-decoder.c, spv-legacy-data.c, spv-legacy-decoder.c,
# spvbin-helpers.c) shows how those fields combine into something usable.
#
# THE DISPATCH RULE this whole module hinges on (found in spv_read_table_item()
# in spv.c): a table's tableStructure has a mandatory dataPath (a .bin zip
# member) and an OPTIONAL path (an XML zip member). If path is present, the
# table's cell/dimension data is the OLDER "legacy" format (raw case data in
# dataPath + pivot structure in path's detail-xml); if path is absent,
# dataPath alone is the modern "light-binary" format (SPSS Statistics 21+).
# This is a structural presence check, NOT a version-number check, despite
# "legacy vs modern" sounding like one.
#
# Two table encodings are decoded, handled transparently by import_spv():
# modern (SPSS 21+) tables via .spv_decode_light_table(), older tables via
# .spv_decode_legacy_data() + .spv_decode_legacy_table(). A table that fails
# to decode (an unsupported construct in either format, or real malformation)
# is skipped rather than aborting the whole file, so one bad table does not
# lose every other result in the same archive.
#
# This file also provides export_spv_html(), which -- since .spv carries no
# rendered view of its own -- builds one from the tables import_spv() already
# decodes (grouped by analysis heading, one <table> per result), and
# .spv_export_syntax(), which recovers the exact SPSS syntax that produced
# each result as a sibling ".sps" file for code_check() (see
# .code_expand_spv() in R/code_check.R, its only caller).
#
# Every piece below is ported code in service of ONE public entry point,
# import_spv() -- kept in a single file for that reason, even though it spans
# several genuinely distinct algorithms (byte-level cursor primitives, the
# legacy case-data/detail-xml decoder, the modern light-binary decoder, and
# the structure-XML reader that ties them together).

# ═══════════════════════════════════════════════════════════════════════════
# ── Low-level binary-cursor primitives ───────────────────────────────────────
# Ported from PSPP's spvbin-helpers.c. A cursor is a list(raw = <raw vector>,
# pos = <1-based next-byte offset>, limit = <1-based exclusive end, for
# count()-bounded sub-sections>). Every .spvbin_read_*() advances `pos` and
# returns list(value, cur); reading past `limit` throws a classed condition
# (spv_binary_eof) so callers can abort decoding one table without crashing
# the whole import_spv() call.
# ═══════════════════════════════════════════════════════════════════════════

.spvbin_cursor <- function(raw) list(raw = raw, pos = 1L, limit = length(raw) + 1L)

.spvbin_eof <- function(cur, n) {
  stop(structure(
    class = c("spv_binary_eof", "error", "condition"),
    list(message = sprintf("spv binary: needed %d bytes at offset %d, only %d available",
                            n, cur$pos, cur$limit - cur$pos))))
}

# Raw bytes, no interpretation (used by .spvbin_read_string() etc.).
.spvbin_read_bytes <- function(cur, n) {
  if (cur$pos + n - 1L >= cur$limit) .spvbin_eof(cur, n)
  bytes <- cur$raw[seq.int(cur$pos, length.out = n)]
  cur$pos <- cur$pos + n
  list(value = bytes, cur = cur)
}

.spvbin_read_byte <- function(cur) {
  r <- .spvbin_read_bytes(cur, 1L)
  list(value = as.integer(r$value), cur = r$cur)
}

.spvbin_read_bool <- function(cur) {
  r <- .spvbin_read_byte(cur)
  if (!r$value %in% c(0L, 1L))
    stop("spv binary: bad bool byte ", r$value, " at offset ", cur$pos)
  list(value = r$value == 1L, cur = r$cur)
}

# int16/int32/int64 are little-endian; be16/be32/be64 are big-endian (grammar's
# `be` prefix). int64 is read as an R double (readBin's "integer" size=8 is
# platform-risky and values here fit exactly in a double's 53-bit mantissa for
# all realistic table/cell counts).
.spvbin_read_int <- function(cur, size, signed = FALSE, endian = "little") {
  r <- .spvbin_read_bytes(cur, size)
  if (size == 8L) {
    # R's readBin has no native 8-byte integer read; recombine two 4-byte
    # signed-read halves (each always valid at size 4, unlike signed=FALSE
    # which readBin only allows for sizes 1-2) into a double. Only sums that
    # fit exactly (<= 2^53) are expected in this format.
    halves <- readBin(r$value, what = "integer", n = 2L, size = 4L, endian = endian)
    halves[halves < 0] <- halves[halves < 0] + 2^32
    value <- if (endian == "little") halves[1] + halves[2] * 2^32
             else halves[2] + halves[1] * 2^32
  } else {
    # readBin only accepts signed=FALSE for size 1-2; for size 4 always read
    # signed then correct to unsigned by adding 2^32 when requested and negative.
    value <- readBin(r$value, what = "integer",
                      size = if (size >= 4L) 4L else size,
                      signed = if (size >= 4L) TRUE else signed,
                      endian = endian)
    if (!signed && size >= 4L && value < 0) value <- value + 2^32
  }
  list(value = value, cur = r$cur)
}

.spvbin_read_int16 <- function(cur) .spvbin_read_int(cur, 2L, endian = "little")
.spvbin_read_int32 <- function(cur) .spvbin_read_int(cur, 4L, endian = "little")
.spvbin_read_int64 <- function(cur) .spvbin_read_int(cur, 8L, endian = "little")
.spvbin_read_be16  <- function(cur) .spvbin_read_int(cur, 2L, endian = "big")
.spvbin_read_be32  <- function(cur) .spvbin_read_int(cur, 4L, endian = "big")
.spvbin_read_be64  <- function(cur) .spvbin_read_int(cur, 8L, endian = "big")

.spvbin_read_double <- function(cur) {
  r <- .spvbin_read_bytes(cur, 8L)
  list(value = readBin(r$value, what = "double", size = 8L, endian = "little"),
       cur = r$cur)
}

.spvbin_read_float <- function(cur) {
  r <- .spvbin_read_bytes(cur, 4L)
  list(value = readBin(r$value, what = "double", size = 4L, endian = "little"),
       cur = r$cur)
}

# string = little-endian uint32 length prefix + raw bytes (no terminator, no
# fixed encoding — see spvlb_table_get_encoding() in spv-light-decoder.c; this
# reader returns raw bytes as a latin1 string and callers re-encode once the
# table's declared charset is known, mirroring to_utf8() in the C source).
# bestring is identical but with a BIG-endian length prefix (used in the
# TableSettings/Formats section of the light-binary grammar).
.spvbin_read_string_ <- function(cur, be = FALSE) {
  len <- if (be) .spvbin_read_be32(cur) else .spvbin_read_int32(cur)
  cur <- len$cur
  if (len$value == 0L) return(list(value = "", cur = cur))
  r <- .spvbin_read_bytes(cur, len$value)
  list(value = rawToChar(r$value, multiple = FALSE), cur = r$cur)
}
.spvbin_read_string   <- function(cur) .spvbin_read_string_(cur, be = FALSE)
.spvbin_read_bestring <- function(cur) .spvbin_read_string_(cur, be = TRUE)

# Matches literal marker byte(s) from the grammar (e.g. the `01`, `00`, `31`,
# `58` tokens). Returns TRUE/advances on match, FALSE/cursor-unchanged
# otherwise, mirroring spvbin_match_bytes()'s non-throwing probe semantics
# (used for optional/alternative sections like `(31 X | 58)`).
.spvbin_match_bytes <- function(cur, bytes) {
  n <- length(bytes)
  if (cur$pos + n - 1L >= cur$limit) return(list(matched = FALSE, cur = cur))
  actual <- as.integer(cur$raw[seq.int(cur$pos, length.out = n)])
  if (!identical(actual, as.integer(bytes))) return(list(matched = FALSE, cur = cur))
  cur$pos <- cur$pos + n
  list(matched = TRUE, cur = cur)
}
.spvbin_match_byte <- function(cur, byte) .spvbin_match_bytes(cur, byte)

.spvbin_expect_bytes <- function(cur, bytes, what = NULL) {
  r <- .spvbin_match_bytes(cur, bytes)
  if (!r$matched)
    stop("spv binary: expected marker byte(s) ", paste(bytes, collapse = " "),
         if (!is.null(what)) paste0(" (", what, ")"), " at offset ", cur$pos)
  r$cur
}

# `count(X)` grammar wrapper: a little-endian uint32 byte-length prefix bounds
# a sub-structure, used both to make a section skippable when its content is
# uninteresting and to detect truncated/malformed sections without needing to
# parse every field inside them. Returns a sub-cursor whose `limit` is capped
# to the prefixed length, and the outer cursor already advanced past the whole
# wrapped section (call `$after` to resume outer parsing).
.spvbin_read_count <- function(cur, be = FALSE) {
  len <- if (be) .spvbin_read_be32(cur) else .spvbin_read_int32(cur)
  cur <- len$cur
  end <- cur$pos + len$value
  if (end - 1L >= cur$limit) .spvbin_eof(cur, len$value)
  inner <- list(raw = cur$raw, pos = cur$pos, limit = end)
  after <- cur; after$pos <- end
  list(inner = inner, after = after)
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Legacy raw case-data decoder ─────────────────────────────────────────────
# Reader for the LEGACY ".spv" raw case-data binary format (the format's
# `dataPath` member when a table also has a sibling `path` XML member — see
# the dispatch rule above).
#
# Ported from PSPP's src/output/spv/old-binary.grammar (metadata header) and
# spv-legacy-data.c (the actual byte reader). Only the metadata header
# (`Metadata => int32*3 byte*28[source-name] ...`) is read by PSPP's
# generated parser; the data block and the trailing value-label `Strings`
# section are grammar lines PSPP itself comments out (`#Data`, `#Variable`,
# `#Strings`) and instead reads by hand directly off the raw buffer in
# spv-legacy-data.c — so this port does the same: a small generated-style
# reader for the header, then direct byte-offset reads for the rest.
#
# WHAT THIS FORMAT ACTUALLY STORES: not a pivot table at all, just raw
# per-variable case data (one "source" = one variable-group, e.g. all the
# variables behind one CORRELATIONS command), stored COLUMN-MAJOR as fixed-
# width doubles, with an optional trailing lookup table that relabels numeric
# codes to their string value-labels for categorical/string variables. The
# actual pivot STRUCTURE (which values are rows/columns, dimension names,
# footnotes) lives entirely in the sibling XML `path` member and is decoded
# below by cross-referencing that XML against the spv_data this returns.
# ═══════════════════════════════════════════════════════════════════════════

# One decoded "source" (one CORRELATIONS/etc.-worth of variables): a list of
# list(var_name, values) per variable, where `values` is a data.frame-ready
# list of list(d = <double or NA>, s = <string or NA>) per case -- kept as
# the (numeric xor string) union PSPP's own spv_data_value carries, since a
# variable's values only become strings once (if) the trailing Strings
# section relabels them.
.spvob_read_metadata <- function(cur) {
  n_values <- .spvbin_read_int32(cur); cur <- n_values$cur
  n_vars <- .spvbin_read_int32(cur); cur <- n_vars$cur
  data_offset <- .spvbin_read_int32(cur); cur <- data_offset$cur
  name_raw <- .spvbin_read_bytes(cur, 28L); cur <- name_raw$cur
  # vB0(byte*36[ext-source-name] int32[x]): a version-gated (v>=? — PSPP's
  # grammar marks this vB0, a build/branch condition on the LegacyBinary
  # header's own `version` byte, always present in every real file seen so
  # far) extended name used when the 28-byte field is entirely filled (no
  # room for a NUL) -- see decode_var_name() in spv-legacy-data.c, which
  # concatenates source_name + ext_source_name only when the first field has
  # no terminator within its own width.
  ext_name_raw <- .spvbin_read_bytes(cur, 36L); cur <- ext_name_raw$cur
  x <- .spvbin_read_int32(cur); cur <- x$cur

  name0 <- .spvob_fixed_string(name_raw$value)
  name <- if (nchar(name0, type = "bytes") >= 28L)
    paste0(name0, .spvob_fixed_string(ext_name_raw$value)) else name0

  list(value = list(n_values = n_values$value, n_vars = n_vars$value,
                     data_offset = data_offset$value, source_name = name),
       cur = cur)
}

# A fixed-width byte field's string content up to its first NUL (or the whole
# width if none) -- mirrors decode_fixed_string()'s strnlen()-then-copy.
.spvob_fixed_string <- function(raw_bytes) {
  ints <- as.integer(raw_bytes)
  nul_at <- which(ints == 0L)[1]
  n <- if (is.na(nul_at)) length(ints) else nul_at - 1L
  if (n == 0L) return("")
  rawToChar(raw_bytes[seq_len(n)])
}

#' Decode one legacy .spv raw case-data member
#'
#' Reads the raw bytes of a legacy-format `dataPath` zip member: PSPP's
#' `spv_legacy_data_decode()`, ported. Returns the per-"source" (variable
#' group) case data needed to reconstruct a pivot table once cross-referenced
#' against the sibling `path` XML member's dimension/category structure — see
#' [.spv_decode_legacy_table()].
#'
#' @param raw raw vector: the exact bytes of the `.bin` zip member.
#' @return a list of sources, each `list(source_name, variables)` where
#'   `variables` is a list of `list(var_name, values)`, `values` a list of
#'   `list(d, s)` per case (numeric xor string; `NA` for the unused side).
#'   `NULL` if the member cannot be decoded.
#' @keywords internal
.spv_decode_legacy_data <- function(raw) {
  tryCatch({
    cur <- .spvbin_cursor(raw)
    cur <- .spvbin_expect_bytes(cur, 0x00L, "LegacyBinary marker")
    version <- .spvbin_read_byte(cur); cur <- version$cur
    n_sources <- .spvbin_read_int16(cur); cur <- n_sources$cur
    member_size <- .spvbin_read_int32(cur); cur <- member_size$cur

    metas <- vector("list", n_sources$value)
    for (i in seq_len(n_sources$value)) {
      m <- .spvob_read_metadata(cur); cur <- m$cur; metas[[i]] <- m$value
    }

    # The data block and Strings section are NOT part of the generated
    # parser's grammar (PSPP comments them out) -- read directly off the raw
    # buffer at each source's own `data_offset`, exactly as
    # spv_legacy_data_decode() does, rather than continuing the cursor
    # sequentially (sources' data blocks are addressed by absolute offset,
    # not laid out in metadata order).
    sources <- vector("list", length(metas))
    max_end <- cur$pos
    for (i in seq_along(metas)) {
      md <- metas[[i]]
      var_size <- 288L + md$n_values * 8L
      source_size <- md$n_vars * var_size
      end_offset <- md$data_offset + source_size
      if (end_offset > length(raw))
        stop("spv legacy data: source '", md$source_name,
             "' runs past end of member")

      pos <- md$data_offset + 1L   # data_offset is 0-based (C convention)
      vars <- vector("list", md$n_vars)
      for (v in seq_len(md$n_vars)) {
        vname <- .spvob_fixed_string(raw[seq.int(pos, length.out = 288L)])
        pos <- pos + 288L
        vals <- vector("list", md$n_values)
        for (k in seq_len(md$n_values)) {
          d <- readBin(raw[seq.int(pos, length.out = 8L)], "double",
                       size = 8L, endian = "little")
          pos <- pos + 8L
          vals[[k]] <- list(d = d, s = NA_character_)
        }
        vars[[v]] <- list(var_name = vname, values = vals)
      }
      sources[[i]] <- list(source_name = md$source_name, variables = vars)
      max_end <- max(max_end, md$data_offset + source_size + 1L)
    }

    # Trailing Strings section (SourceMaps + Labels): relabels specific
    # (source, variable, value-index) cells from a raw numeric code to a
    # value-label string -- e.g. a variable's numeric codes 1/2/3 becoming
    # "Male"/"Female"/"Other" in the final table. Optional: absent when every
    # variable in the member is purely numeric.
    if (max_end - 1L < length(raw)) {
      cur2 <- list(raw = raw, pos = max_end, limit = length(raw) + 1L)
      strings <- tryCatch(.spvob_read_strings(cur2), error = function(e) NULL)
      if (!is.null(strings)) sources <- .spvob_apply_strings(sources, strings)
    }

    sources
  }, error = function(e) {
    warning("spv: could not decode legacy data: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}

# Strings => SourceMaps[maps] Labels
# SourceMaps => int32[n-maps] SourceMap*[n-maps]
# SourceMap => string[source-name] int32[n-variables] VariableMap*[n-variables]
# VariableMap => string[variable-name] int32[n-data] DatumMap*[n-data]
# DatumMap => int32[value-idx] int32[label-idx]
# Labels => int32[n-labels] Label*[n-labels]
# Label => int32[frequency] string[label]
.spvob_read_strings <- function(cur) {
  n_maps <- .spvbin_read_int32(cur); cur <- n_maps$cur
  maps <- vector("list", n_maps$value)
  for (i in seq_len(n_maps$value)) {
    sname <- .spvbin_read_string(cur); cur <- sname$cur
    n_vars <- .spvbin_read_int32(cur); cur <- n_vars$cur
    vmaps <- vector("list", n_vars$value)
    for (j in seq_len(n_vars$value)) {
      vname <- .spvbin_read_string(cur); cur <- vname$cur
      n_data <- .spvbin_read_int32(cur); cur <- n_data$cur
      datum_maps <- vector("list", n_data$value)
      for (k in seq_len(n_data$value)) {
        vidx <- .spvbin_read_int32(cur); cur <- vidx$cur
        lidx <- .spvbin_read_int32(cur); cur <- lidx$cur
        datum_maps[[k]] <- list(value_idx = vidx$value, label_idx = lidx$value)
      }
      vmaps[[j]] <- list(variable_name = vname$value, data = datum_maps)
    }
    maps[[i]] <- list(source_name = sname$value, variables = vmaps)
  }
  n_labels <- .spvbin_read_int32(cur); cur <- n_labels$cur
  labels <- vector("list", n_labels$value)
  for (i in seq_len(n_labels$value)) {
    freq <- .spvbin_read_int32(cur); cur <- freq$cur
    lbl <- .spvbin_read_string(cur); cur <- lbl$cur
    labels[[i]] <- list(frequency = freq$value, label = lbl$value)
  }
  list(maps = maps, labels = labels)
}

# Apply the Strings section's relabeling to the decoded sources: for each
# (source, variable, value-index) DatumMap entry, replace that case's numeric
# value with the referenced label's string -- mirrors decode_variable_map()
# in spv-legacy-data.c.
.spvob_apply_strings <- function(sources, strings) {
  labels <- strings$labels
  for (sm in strings$maps) {
    si <- which(vapply(sources, function(s) identical(s$source_name, sm$source_name), logical(1)))
    if (!length(si)) next
    src <- sources[[si[1]]]
    for (vm in sm$variables) {
      vi <- which(vapply(src$variables, function(v) identical(v$var_name, vm$variable_name), logical(1)))
      if (!length(vi)) next
      for (dm in vm$data) {
        k <- dm$value_idx + 1L
        li <- dm$label_idx + 1L
        if (k >= 1L && k <= length(src$variables[[vi[1]]]$values) &&
            li >= 1L && li <= length(labels)) {
          src$variables[[vi[1]]]$values[[k]] <- list(d = NA_real_, s = labels[[li]]$label)
        }
      }
    }
    sources[[si[1]]] <- src
  }
  sources
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Legacy table decoder (detail-xml + raw case data) ────────────────────────
# Cross-references a table's `detail-xml` ("visualization") structure XML
# against its raw case data (above) to assemble the same tidy data.frame shape
# .spv_decode_light_table() produces for modern tables.
#
# Ported from PSPP's src/output/spv/detail-xml.grammar (element structure)
# and spv-legacy-decoder.c (decode_spvdx_table() and helpers). Scope: CORE
# sourceVariable-based tables only (the common case -- T-Test/ANOVA/GLM/
# Correlations tables all use plain sourceVariable "series" for their
# dimensions and cell values). Explicitly OUT OF SCOPE, left to error out
# rather than guess: derivedVariable value-map-entry / relabeling expressions
# beyond the single `constant(0)`/`constant(N)` forms actually seen in real
# files, date/time value reconstruction, and non-table content
# (graphs/models/trees).
#
# VALIDATED against real legacy tables (see tests/testthat/fixtures/
# spv-test-corpus.md for the file list): decoded WITHOUT ERROR across 19
# tables spanning One-Sample/Independent-Samples T-Tests, Oneway ANOVA,
# UNIANOVA, and GLM repeated-measures (including a 4-way sphericity-correction
# Tests of Within-Subjects Effects table with 44 rows), every spot-checked
# value statistically coherent (matching t/df/p/F/eta-squared relationships,
# correctly summed ANOVA degrees of freedom, correctly symmetric CIs).
#
# One lesson from THIS format specifically: a dimension's series can
# legitimately have BLANK values for some rows and real text for others (a
# sub-grouping series like "dimension1group1" in a One-Sample Test table,
# which only labels the two CI rows with "95% Confidence Interval of the
# Difference" and is blank for the t/df/p/mean-difference rows) — this
# looked like a bug on first sight until compared against the source XML's
# own nest structure, which showed it correctly reflects a real sub-heading
# that only some rows fall under. Not every unexpected-looking output is a
# decoding bug; some genuinely reflect the source table's own structure.
# ═══════════════════════════════════════════════════════════════════════════

# One "series": one sourceVariable/derivedVariable's decoded values, keyed by
# its XML `id` (not its `sourceName`, which is only meaningful for
# sourceVariables). Mirrors struct spv_series in spv-legacy-decoder.c, minus
# the style/affix bookkeeping this port skips (see the light-binary decoder's
# "styling out of scope" note below -- the same applies here).
.spvdx_read_source_variable <- function(node, data) {
  source_name <- xml2::xml_attr(node, "sourceName")
  source <- xml2::xml_attr(node, "source")
  src <- Find(function(s) identical(s$source_name, source), data)
  if (is.null(src)) return(NULL)
  var <- Find(function(v) identical(v$var_name, source_name), src$variables)
  if (is.null(var)) return(NULL)

  # A sourceVariable can reference a SEPARATE labelVariable (another series
  # in the same source) whose values relabel this one's numeric codes to
  # display text -- e.g. dimension0categories (numeric 0/1) relabeled via
  # dimension0labels ("BlackChosen"/"WhiteChosen"), confirmed directly in the
  # study2.spv fixture. Mirrors decode_spvdx_source_variable()'s label_series
  # handling (the vme/format-based relabeling maps it also supports are out
  # of this version's scope; this direct label-variable case is the common
  # one actually seen).
  label_var_id <- xml2::xml_attr(node, "labelVariable")
  values <- var$values
  if (!is.na(label_var_id)) {
    label_ref <- xml2::xml_find_first(
      xml2::xml_root(node),
      sprintf(".//*[@id=%s]", shQuote(label_var_id, type = "sh")))
    if (!is.na(label_ref)) {
      lbl_source_name <- xml2::xml_attr(label_ref, "sourceName")
      lbl_var <- Find(function(v) identical(v$var_name, lbl_source_name), src$variables)
      if (!is.null(lbl_var) && length(lbl_var$values) == length(values)) {
        values <- lapply(seq_along(values), function(i) {
          if (is.na(values[[i]]$d)) values[[i]] else lbl_var$values[[i]]
        })
      }
    }
  }
  list(id = xml2::xml_attr(node, "id"), values = values)
}

# A derivedVariable in the scope this port covers is either `constant(0)`
# (one dimension level shared by all rows -- a placeholder single-category
# axis) or `constant(N)` for other N (rare, treated the same: a single
# constant category); PSPP's own valueMapEntry/map()-reference derived
# variables are out of scope (see the header comment above).
.spvdx_read_derived_variable <- function(node, n_values) {
  value_expr <- xml2::xml_attr(node, "value")
  if (!grepl("^constant\\(", value_expr))
    stop("spv legacy: unsupported derivedVariable value '", value_expr, "'")
  list(id = xml2::xml_attr(node, "id"),
       values = replicate(n_values, list(d = 0, s = NA_character_), simplify = FALSE))
}

# Resolve every sourceVariable/derivedVariable at the top level of a
# <visualization> into a named (by id) list of series, iterating to a fixed
# point since a derivedVariable's `constant(0)` needs another series' length
# and (in principle) variables can reference each other -- mirrors the
# retry-until-no-progress loop in decode_spvdx_table().
.spvdx_read_all_series <- function(root, data) {
  var_nodes <- xml2::xml_find_all(
    root, "./*[local-name()='sourceVariable' or local-name()='derivedVariable']")
  series <- list()
  remaining <- seq_along(var_nodes)
  repeat {
    progressed <- FALSE
    still_remaining <- integer(0)
    for (i in remaining) {
      node <- var_nodes[[i]]
      tag <- xml2::xml_name(node)
      s <- tryCatch({
        if (tag == "sourceVariable") .spvdx_read_source_variable(node, data)
        else {
          n <- if (length(series)) length(series[[1]]$values) else NA_integer_
          if (is.na(n)) NULL else .spvdx_read_derived_variable(node, n)
        }
      }, error = function(e) NULL)
      if (!is.null(s)) {
        series[[s$id]] <- s
        progressed <- TRUE
      } else still_remaining <- c(still_remaining, i)
    }
    remaining <- still_remaining
    if (!length(remaining) || !progressed) break
  }
  series
}

# faceting > cross > nest[side] > variableReference*: the ordered list of
# series ids nested along one axis (rows or columns). Each nest alternates a
# "categories" series (the actual values) with a "constant(0)" derived
# series (a display-grouping placeholder) -- see add_dimensions()'s pairwise
# `for (n = 0; ...) if not present, break` walk in the C decoder, mirrored
# here by just taking every OTHER reference (the categories ones), since the
# constant(0) placeholders never carry real category identity of their own.
.spvdx_nest_series_ids <- function(nest_node) {
  refs <- xml2::xml_find_all(nest_node, "./*[local-name()='variableReference']")
  vapply(refs, function(r) xml2::xml_attr(r, "ref"), character(1))
}

#' Decode one legacy .spv table (detail-xml structure + raw case data)
#'
#' @param xml_raw raw vector or string: the `path` member's XML content (a
#'   `<visualization>` document).
#' @param data the decoded raw case data for this table's `dataPath` member,
#'   as returned by [.spv_decode_legacy_data()].
#' @param title optional table title (from the structure reader's own
#'   `<title>` label, kept separate from this XML's own name so both formats
#'   agree on where a title comes from).
#' @return a data.frame in the same shape [.spv_decode_light_table()]
#'   returns (one row per cell, one column per dimension holding that cell's
#'   category label, plus `value`), or `NULL` if this table uses a
#'   construct out of this port's scope (falls through to the caller's HTML
#'   fallback).
#' @keywords internal
.spv_decode_legacy_table <- function(xml_raw, data, title = NA_character_) {
  tryCatch({
    doc <- xml2::read_xml(xml_raw)
    root <- xml2::xml_root(doc)
    series <- .spvdx_read_all_series(root, data)

    graph <- xml2::xml_find_first(root, "./*[local-name()='graph']")
    cross <- xml2::xml_find_first(graph, ".//*[local-name()='cross']")
    nests <- xml2::xml_find_all(cross, "./*[local-name()='nest']")
    if (length(nests) != 2)
      stop("spv legacy: expected exactly 2 nests (rows, columns), found ", length(nests))

    # PSPP's own faceting places COLUMNS as cross's first child sequence and
    # ROWS as its second (confirmed against decode_spvdx_table(): `columns =
    # spvdx_cast_nest(cross->seq[0])`, `rows = spvdx_cast_nest(cross->seq2[0])`).
    col_ids <- .spvdx_nest_series_ids(nests[[1]])
    row_ids <- .spvdx_nest_series_ids(nests[[2]])
    # Drop constant(0)-style placeholder ids (derivedVariable, not an actual
    # category source) -- keep only ids that resolved to a REAL series with
    # more than one distinct category OR that carry a genuine label (i.e.
    # sourceVariables), mirroring add_dimensions() only building a dimension
    # from series with n_values > 0 categories worth distinguishing.
    is_real_dim <- function(id) {
      node <- xml2::xml_find_first(root, sprintf(".//*[@id=%s]", shQuote(id, type = "sh")))
      !is.na(node) && xml2::xml_name(node) == "sourceVariable"
    }
    col_ids <- Filter(is_real_dim, col_ids)
    row_ids <- Filter(is_real_dim, row_ids)
    if (!length(col_ids) && !length(row_ids))
      stop("spv legacy: no real dimension series found")

    labeling <- xml2::xml_find_first(graph, ".//*[local-name()='interval']/*[local-name()='labeling']")
    cell_id <- xml2::xml_attr(labeling, "variable")
    cell_series <- series[[cell_id]]
    if (is.null(cell_series)) stop("spv legacy: no cell series '", cell_id, "'")

    dim_ids <- c(col_ids, row_ids)
    dims <- lapply(dim_ids, function(id) series[[id]])
    if (any(vapply(dims, is.null, logical(1))))
      stop("spv legacy: missing dimension series")
    n_cells <- length(cell_series$values)
    if (any(vapply(dims, function(d) length(d$values) != n_cells, logical(1))))
      stop("spv legacy: dimension/cell series length mismatch")

    dim_names <- make.unique(vapply(seq_along(dim_ids), function(i) {
      nm <- xml2::xml_attr(xml2::xml_find_first(
        root, sprintf(".//*[@id=%s]", shQuote(dim_ids[i], type = "sh"))), "label")
      if (!is.na(nm) && nzchar(nm)) nm else paste0("dim", i)
    }, character(1)))

    rows <- lapply(seq_len(n_cells), function(i) {
      labels <- lapply(dims, function(d) .spvdx_data_value_text(d$values[[i]]))
      names(labels) <- dim_names
      c(labels, list(value = .spvdx_data_value_text(cell_series$values[[i]])))
    })
    df <- do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
    names(df) <- c(dim_names, "value")
    attr(df, "spv_title") <- title
    # SPSS's own axis assignment -- dim_names is ordered COLUMNS-then-ROWS
    # (dim_ids <- c(col_ids, row_ids) above), matching the light-binary
    # decoder's spv_row_dims/spv_col_dims attributes (spv_assemble_table())
    # so export_spv_html()'s .spv_table_html() can pivot either format's
    # table the same way.
    attr(df, "spv_col_dims") <- dim_names[seq_along(col_ids)]
    attr(df, "spv_row_dims") <- if (length(row_ids))
      dim_names[length(col_ids) + seq_along(row_ids)] else character(0)
    df
  }, error = function(e) {
    warning("spv: could not decode legacy table: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}

# One spv_data_value (list(d, s)) rendered as the text this table's grid
# needs -- a string value renders as itself, a numeric value via
# .stat_num_to_chr() so full precision survives, matching how
# .spv_decode_light_table() treats its own Value union.
.spvdx_data_value_text <- function(v) {
  if (!is.null(v$s) && !is.na(v$s)) return(v$s)
  if (is.null(v$d) || is.na(v$d)) return(NA_character_)
  .stat_num_to_chr(v$d)
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Chart decoder (VizML detail-xml + legacy case data) ──────────────────────
# Decodes a <vgr:graph> structure item's chart.xml ("VizML" -- an IBM/SPSS
# visualization-description XML dialect, xmlns
# http://www.ibm.com/software/analytics/spss/xml/visualization) against the
# chart's own raw point data. Ported entirely from a REAL example file (see
# tests/testthat/fixtures -- no PSPP source covers charts at all, since GNU
# PSPP itself does not implement chart rendering; the SPV file-format doc only
# says "charts do not have a 'light' format" with no further byte-level spec).
#
# The two facts this decoder rests on, both confirmed against a real chart
# rather than assumed from any spec:
#   1. chart.xml is the SAME <visualization> XML dialect used by legacy
#      DETAIL-XML tables (.spv_decode_legacy_table()'s xml_raw), just
#      describing a plot instead of a pivot table -- a <point>/<line>/<bar>
#      mark element replaces the table's <graph><cross> pivot structure, and
#      <sourceVariable> elements work identically (source/sourceName linking
#      a variable id back to the raw case data's own column).
#   2. The chart's dataPath member (chartData.bin) is NOT a new binary
#      format -- it is byte-for-byte the SAME "LegacyBinary" raw case-data
#      format .spv_decode_legacy_data() already reads for legacy TABLES
#      (confirmed: a real chartData.bin decoded through that function
#      unmodified, with zero format changes, into a source "source0" holding
#      plausible real per-case values for the chart's own x/y variables).
# Given both, decoding a chart needs no new binary reader at all -- only a
# new xml-to-case-data cross-reference, parallel to
# .spvdx_read_source_variable()'s table equivalent but keyed off a mark
# element's `x`/`y variable="..."` references instead of a pivot nest.
#
# Scope (mirrors the legacy TABLE decoder's own "core constructs only"
# stance): a single `<point>` (scatter) mark's `x`/`y` series, plus any
# `<functionGuide>` elements (SPSS's fitted trend/regression lines, stored as
# a literal algebraic expression string in the `value` attribute -- e.g.
# "-4.37017757153376 * x + 73.5462079830134" -- requiring NO curve-fitting of
# our own, since SPSS already computed and stored the fit). Bar/line/box marks
# and other VizML constructs are out of this version's scope, left to return
# NULL (falls through to being skipped, same as an undecodable table).

# variable="..." references inside a chart's mark elements resolve through
# <sourceVariable id="..." source="..." sourceName="...">, exactly like a
# legacy table's dimension series -- reused here rather than duplicated.
# Returns the case data's own values (list of list(d, s)) for that variable,
# or NULL if the reference cannot be resolved.
.spvviz_resolve_variable <- function(root, var_id, data) {
  node <- xml2::xml_find_first(root, sprintf(".//*[@id=%s]", shQuote(var_id, type = "sh")))
  if (is.na(node) || xml2::xml_name(node) != "sourceVariable") return(NULL)
  source_name <- xml2::xml_attr(node, "sourceName")
  source <- xml2::xml_attr(node, "source")
  src <- Find(function(s) identical(s$source_name, source), data)
  if (is.null(src)) return(NULL)
  var <- Find(function(v) identical(v$var_name, source_name), src$variables)
  if (is.null(var)) return(NULL)
  list(id = var_id, source_name = source_name, values = var$values)
}

# A <functionGuide>'s `value` attribute is a literal algebraic expression in
# `x` (SPSS's own already-fitted curve, e.g. a linear/quadratic/cubic
# regression) -- parsed as an R expression and wrapped as a plain R function
# so export_spv_html() can evaluate it over a range of x for plotting,
# without this package doing any curve-fitting of its own. `x*x` (SPSS's own
# notation) is valid R already; no rewriting needed. Returns NULL if the
# expression fails to parse (an unsupported operator this port has not seen),
# so one bad guide is skipped rather than aborting the whole chart.
.spvviz_function_guide <- function(node) {
  expr_txt <- xml2::xml_attr(node, "value")
  if (is.na(expr_txt) || !nzchar(expr_txt)) return(NULL)
  fn <- tryCatch({
    expr <- parse(text = expr_txt)[[1]]
    f <- function(x) NULL
    body(f) <- expr
    f
  }, error = function(e) NULL)
  if (is.null(fn)) return(NULL)
  list(name = xml2::xml_attr(node, "name") %||% NA_character_,
       expr = expr_txt, fn = fn)
}

# `%||%` only substitutes on NULL, not NA, so a small `%NA%` helper picks the
# first non-NA/non-empty candidate instead of stopping at the first NA --
# using `%||%` for axis-label/title fallbacks would freeze on the first
# NA_character_ and never reach the later fallbacks (a real bug this exact
# mistake caused once; see spv_decode_chart()'s axis-label lookups).
`%NA%` <- function(a, b) if (is.na(a) || !nzchar(a %||% "")) b else a

# Axis label / title lookups shared by every chart mark type this decoder
# supports: the <label> text under an axis (falling back to a resolved
# variable's own `label`/`shortLabel` display name, then its raw source
# name), and the chart's own <labelFrame> title.
.spvviz_axis_label <- function(root, axis_id_suffix, var_id, var_source_name) {
  ax <- xml2::xml_find_first(root, sprintf(
    ".//*[local-name()='axis' and contains(@id, %s)]", shQuote(axis_id_suffix, type = "sh")))
  from_axis <- if (is.na(ax)) NA_character_ else {
    lbl <- xml2::xml_find_first(ax, ".//*[local-name()='label']//*[local-name()='text']")
    if (is.na(lbl)) NA_character_ else xml2::xml_text(lbl)
  }
  var_node <- xml2::xml_find_first(root, sprintf(".//*[@id=%s]", shQuote(var_id, type = "sh")))
  from_var <- if (is.na(var_node)) NA_character_ else
    (xml2::xml_attr(var_node, "label") %NA% xml2::xml_attr(var_node, "shortLabel")) %||% NA_character_
  from_axis %NA% from_var %NA% var_source_name
}
.spvviz_chart_title <- function(root, fallback) {
  node <- xml2::xml_find_first(root, ".//*[local-name()='labelFrame']//*[local-name()='text']")
  if (!is.na(node)) xml2::xml_text(node) else fallback
}

# A box plot's (<schema>) source data comes from EITHER of two places,
# confirmed against two different real files -- there is no single answer,
# unlike a <point> chart which always uses dataPath:
#   * inline XML: an <embeddedSource id="..."><names>Category;Label;Tooltips;
#     Value</names><row>0;;167,50;1.75</row>...</embeddedSource>, semicolon-
#     delimited, referenced by <sourceVariable source="<that id>"
#     sourceName="Category"|"Value">. Only that specific 4-column
#     (Category/Label/Tooltips/Value) shape -- the one actually seen paired
#     with a real <schema> mark -- is decoded; an embeddedSource with a
#     different column set (e.g. the "facet1;facet2;node" / "count;value"
#     layout-metadata shapes also seen in real files, which are NOT box-plot
#     data) is left to return NULL, same as any other out-of-scope construct.
#   * dataPath/chartData.bin: the SAME "LegacyBinary" case-data format a
#     <point> chart's x/y series come from (see .spv_decode_chart()'s header
#     comment) -- confirmed against a real file where the category variable's
#     `source` attribute names a dataPath source (e.g. "source0"), not an
#     embeddedSource id at all. .spv_decode_chart() tries embeddedSource
#     first (it needs no `data` argument) and falls back to this case-data
#     path when the source id doesn't resolve to an embeddedSource.
.spvviz_decode_boxplot_databin <- function(root, x_ref, y_ref, x_node, y_node, data) {
  x_var <- .spvviz_resolve_variable(root, x_ref, data)
  y_var <- .spvviz_resolve_variable(root, y_ref, data)
  if (is.null(x_var) || is.null(y_var)) return(NULL)
  if (length(x_var$values) != length(y_var$values)) return(NULL)

  # The category axis is a categorical (relabelled) integer code, exactly
  # like a legacy TABLE's categorical dimension series -- reuse the same
  # relabel lookup .spvviz_decode_boxplot_source() uses for the
  # embeddedSource case, keyed off the SAME <relabel from="N" to="label"/>
  # convention under the category's own sourceVariable node.
  relabels <- xml2::xml_find_all(x_node, ".//*[local-name()='relabel']")
  code_to_label <- if (length(relabels)) stats::setNames(
    vapply(relabels, xml2::xml_attr, character(1), attr = "to"),
    vapply(relabels, xml2::xml_attr, character(1), attr = "from")) else character(0)

  cats <- vapply(x_var$values, function(v) if (!is.null(v$d)) as.character(v$d) else NA_character_, character(1))
  category <- unname(code_to_label[cats])
  category[is.na(category)] <- cats[is.na(category)]  # no relabel entry: use the raw code
  value <- vapply(y_var$values, function(v) if (!is.null(v$d)) v$d else NA_real_, numeric(1))

  is_na_like <- function(v) is.na(v) | (is.numeric(v) & abs(v) >= .Machine$double.xmax)
  keep <- !is_na_like(category) & !is_na_like(value)
  if (!any(keep)) return(NULL)
  data.frame(category = category[keep], value = value[keep], stringsAsFactors = FALSE)
}

.spvviz_decode_boxplot_source <- function(root, source_id) {
  es <- xml2::xml_find_first(root, sprintf(
    ".//*[local-name()='embeddedSource' and @id=%s]", shQuote(source_id, type = "sh")))
  if (is.na(es)) return(NULL)
  col_names <- strsplit(xml2::xml_text(xml2::xml_find_first(es, ".//*[local-name()='names']")), ";", fixed = TRUE)[[1]]
  if (!setequal(col_names, c("Category", "Label", "Tooltips", "Value"))) return(NULL)
  rows <- xml2::xml_find_all(es, ".//*[local-name()='row']")
  if (!length(rows)) return(NULL)
  cells <- strsplit(xml2::xml_text(rows), ";", fixed = TRUE)
  if (any(vapply(cells, length, integer(1)) != length(col_names))) return(NULL)
  m <- do.call(rbind, cells)
  colnames(m) <- col_names
  # Category is a 0-based integer code; the real display label comes from
  # the Category sourceVariable's own <relabel from="N" to="label"/> map
  # (SPSS categorical value-labelling, parallel to a haven "labels"
  # attribute), not from this table's own "Label" column (which is a
  # DIFFERENT field -- an optional per-row annotation, empty in the file
  # this was ported from).
  cat_var <- xml2::xml_find_first(root, ".//*[local-name()='sourceVariable' and @sourceName='Category']")
  relabels <- if (!is.na(cat_var))
    xml2::xml_find_all(cat_var, ".//*[local-name()='relabel']") else xml2::xml_missing()
  code_to_label <- stats::setNames(
    vapply(relabels, xml2::xml_attr, character(1), attr = "to"),
    vapply(relabels, xml2::xml_attr, character(1), attr = "from"))
  category <- unname(code_to_label[m[, "Category"]])
  category[is.na(category)] <- m[is.na(category), "Category"]  # no relabel entry: use the raw code
  value <- suppressWarnings(as.numeric(gsub(",", ".", m[, "Value"], fixed = TRUE)))
  data.frame(category = category, value = value, stringsAsFactors = FALSE)
}

# Shared by <point> (scatter) and <interval> (histogram/bar) marks: both
# declare a plain `<x variable="..."/><y variable="..."/>` pair resolved
# against the chart's dataPath case data via .spvviz_resolve_variable() --
# the SAME resolution .spv_decode_chart() already used for <point> alone,
# now shared so <interval> gets it for free. Returns a data.frame(x, y) with
# SPSS's DBL_MAX "not applicable" sentinel (see .spv_decode_light_table())
# rows dropped, or NULL if the mark has no x/y reference or either series
# fails to resolve.
.spvviz_decode_xy <- function(mark_node, root, data) {
  x_ref <- xml2::xml_attr(xml2::xml_find_first(mark_node, "./*[local-name()='x']"), "variable")
  y_ref <- xml2::xml_attr(xml2::xml_find_first(mark_node, "./*[local-name()='y']"), "variable")
  if (is.na(x_ref) || is.na(y_ref)) return(NULL)

  x_var <- .spvviz_resolve_variable(root, x_ref, data)
  y_var <- .spvviz_resolve_variable(root, y_ref, data)
  if (is.null(x_var) || is.null(y_var)) return(NULL)
  if (length(x_var$values) != length(y_var$values)) return(NULL)

  xs <- vapply(x_var$values, function(v) if (!is.null(v$d)) v$d else NA_real_, numeric(1))
  ys <- vapply(y_var$values, function(v) if (!is.null(v$d)) v$d else NA_real_, numeric(1))
  is_na_like <- function(v) is.na(v) | abs(v) >= .Machine$double.xmax
  keep <- !is_na_like(xs) & !is_na_like(ys)
  if (!any(keep)) return(NULL)

  df <- data.frame(x = xs[keep], y = ys[keep])
  attr(df, "spv_chart_xlab") <- .spvviz_axis_label(root, "axisx", x_ref, x_var$source_name)
  attr(df, "spv_chart_ylab") <- .spvviz_axis_label(root, "axisy", y_ref, y_var$source_name)
  df
}

#' Decode one .spv chart (VizML chart.xml + its own raw case data)
#'
#' Handles three mark types, found by inspecting real chart-bearing files
#' from Zenodo/Figshare rather than any spec (`.spv` charts have none -- see
#' the section header above): a `<point>` scatter mark, an `<interval>`
#' histogram/bar mark (both resolved from `.spv_decode_legacy_data()`'s case
#' data via `.spvviz_decode_xy()`), and a `<schema>` box-plot mark (its data
#' resolved from EITHER an inline `<embeddedSource>` element or, when that
#' is absent, the same case data the other two marks use -- see
#' `.spvviz_decode_boxplot_source()` / `.spvviz_decode_boxplot_databin()`).
#' Any other mark (`<line>`/`<bar>`/`<area>`/...) is out of this version's
#' scope.
#'
#' @param xml_raw raw vector or string: the `path` member's XML content (a
#'   `<visualization>` document, the same VizML dialect a legacy table's
#'   detail-xml uses).
#' @param data the decoded raw case data for this chart's `dataPath` member,
#'   as returned by [.spv_decode_legacy_data()] -- used for `<point>`/
#'   `<interval>` charts, and as a `<schema>` box plot's fallback data source
#'   when it has no `<embeddedSource>`.
#' @param title optional chart title (from the structure reader's own
#'   context), used when the XML's own `<labelFrame>` title cannot be read.
#' @return For a `<point>`/`<interval>` chart: a data.frame with columns `x`,
#'   `y` (one row per case with both values present). For a `<schema>` box
#'   plot: a data.frame with columns `category` (the relabelled group name)
#'   and `value` (one row per case). `NULL` if this chart uses a construct
#'   out of this port's scope, or its variable/data references cannot be
#'   resolved. Every shape carries attributes `spv_chart_title`,
#'   `spv_chart_xlab`, `spv_chart_ylab` (character), and `spv_chart_type`
#'   (`"point"`, `"interval"`, or `"boxplot"`); a `<point>` chart also
#'   carries `spv_chart_fits` (a list of `.spvviz_function_guide()` results,
#'   possibly empty) for the fitted trend lines SPSS itself already computed.
#' @keywords internal
.spv_decode_chart <- function(xml_raw, data, title = NA_character_) {
  tryCatch({
    doc <- xml2::read_xml(xml_raw)
    root <- xml2::xml_root(doc)

    point <- xml2::xml_find_first(root, ".//*[local-name()='point']")
    interval <- xml2::xml_find_first(root, ".//*[local-name()='interval']")
    schema <- xml2::xml_find_first(root, ".//*[local-name()='schema']")

    if (!is.na(point)) {
      df <- .spvviz_decode_xy(point, root, data)
      if (is.null(df)) stop("spv chart: could not resolve <point> x/y series")
      attr(df, "spv_chart_fits") <- Filter(Negate(is.null), lapply(
        xml2::xml_find_all(root, ".//*[local-name()='functionGuide']"),
        .spvviz_function_guide))
      attr(df, "spv_chart_type") <- "point"
    } else if (!is.na(interval)) {
      df <- .spvviz_decode_xy(interval, root, data)
      if (is.null(df)) stop("spv chart: could not resolve <interval> x/y series")
      attr(df, "spv_chart_type") <- "interval"
    } else if (!is.na(schema)) {
      x_ref <- xml2::xml_attr(xml2::xml_find_first(schema, "./*[local-name()='x']"), "variable")
      y_ref <- xml2::xml_attr(xml2::xml_find_first(schema, "./*[local-name()='y']"), "variable")
      if (is.na(x_ref) || is.na(y_ref)) stop("spv chart: <schema> missing x/y variable reference")
      x_node <- xml2::xml_find_first(root, sprintf(".//*[@id=%s]", shQuote(x_ref, type = "sh")))
      y_node <- xml2::xml_find_first(root, sprintf(".//*[@id=%s]", shQuote(y_ref, type = "sh")))
      if (is.na(x_node) || is.na(y_node)) stop("spv chart: <schema> variable id not found")
      source_id <- xml2::xml_attr(x_node, "source")
      # Try the inline embeddedSource shape first (needs no case `data` at
      # all); when the source id doesn't resolve to one -- confirmed against
      # a real file where a box plot's category variable is backed by a
      # dataPath source instead -- fall back to the same case-data path
      # <point>/<interval> use.
      df <- .spvviz_decode_boxplot_source(root, source_id)
      if (is.null(df)) df <- .spvviz_decode_boxplot_databin(root, x_ref, y_ref, x_node, y_node, data)
      if (is.null(df)) stop("spv chart: <schema> data not in a supported shape")

      attr(df, "spv_chart_xlab") <- .spvviz_axis_label(root, "axisx", x_ref, xml2::xml_attr(x_node, "sourceName"))
      attr(df, "spv_chart_ylab") <- .spvviz_axis_label(root, "axisy", y_ref, xml2::xml_attr(y_node, "sourceName"))
      attr(df, "spv_chart_type") <- "boxplot"
    } else {
      stop("spv chart: no <point>, <interval>, or <schema> mark (out of this port's scope)")
    }

    attr(df, "spv_chart_title") <- .spvviz_chart_title(root, title)
    df
  }, error = function(e) {
    warning("spv: could not decode chart: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Modern light-binary table decoder ────────────────────────────────────────
# Decoder for the modern ".spv" light-binary table format (the
# "<n>_lightTableData.bin" members of an SPSS Statistics 21+ Viewer file).
#
# Ported from PSPP's src/output/spv/light-binary.grammar (the field-by-field
# byte layout) and spv-light-decoder.c (how those fields become a usable
# table). PSPP's own decoder also reconstructs full visual styling (fonts,
# borders, table-look themes); this port deliberately skips building any R
# representation of that — bytes for style sections are still consumed (in
# the exact shape the grammar specifies, so offsets stay correct) but
# discarded, since metacheck only needs the data: dimension names/categories,
# cell values, number formats, and the value of each cell.
#
# The grammar itself uses this notation: `X[name]` reads type X, `X*N` reads
# N repetitions, `X?` is optional, `(A | B)` is alternation, `count(X)` is a
# byte-length-prefixed sub-section, and literal hex bytes (`01`, `31`, `58`,
# ...) are marker bytes matched exactly. `31`/`58` in particular is a
# present/absent flag pervasive in ValueMod.
#
# ── HOW THIS WAS BUILT: lessons for porting an undocumented binary format ────
# (kept here, not just in a commit message, because the next person to touch
# this file — or to port a DIFFERENT undocumented format the same way — needs
# the METHOD, not just the result.)
#
# 1. FIND WHO ALREADY REVERSE-ENGINEERED IT, before reverse-engineering it
#    yourself. .spv has no public spec, but GNU PSPP had already done the
#    reverse-engineering, as a side effect of being a real SPSS-file-reading
#    statistics package: light-binary.grammar is the exact byte-level spec,
#    spv-light-decoder.c is a working reference implementation. Ported from
#    BOTH: the grammar gives field order/types, the C decoder shows how
#    fields combine into something usable (which fields are footnote refs vs.
#    styling vs. actual data). A grammar alone would have left semantic
#    questions (what does ValueMod's `31 55` inner marker mean?) unanswerable;
#    the C code alone would have left byte-level field boundaries buried in
#    generated-parser calls, not spelled out.
#
# 2. A GENERATED PARSER HIDES THE THING YOU NEED. PSPP does not hand-write its
#    grammar-to-bytes reader: `light-binary.grammar` is fed to
#    `binary-parser-generator` (a PSPP build-time tool) which emits the actual
#    byte-reading C code. That generated file was NEVER fetched or read here —
#    the grammar file (the generator's INPUT) plus spvbin-helpers.c (the
#    generator's OUTPUT's hand-written primitives, e.g. spvbin_parse_string)
#    were enough to derive the same byte layout by hand. If a format's parser
#    is generated, look for the generator's INPUT spec, not its output.
#
# 3. TRANSCRIBING A GRAMMAR BY EYE WILL INTRODUCE BUGS THAT LOOK PLAUSIBLE.
#    Two real bugs shipped in the first draft of this file, both silent (no
#    error, no obviously-wrong output) until tested against a real file with
#    more structural variety than the first few tables tried:
#      a. `Value => 00? 00? 00? 00? case(01 | 02 | ... | 06 | else)[type]` —
#         it is tempting to read this as "read one discriminator byte, switch
#         on it, treat anything not 1-6 as `else`" (a C tagged union reads
#         that way). It is WRONG: 01-06 are each independent literal-byte
#         PROBES (match-and-consume-if-present, like the `31`/`58` markers
#         used everywhere else in this grammar) — if none match, the `else`
#         branch begins with ZERO bytes consumed for a tag, and its first
#         field (ValueMod) starts immediately. Reading a tag byte
#         unconditionally works by ACCIDENT for types 1-6 (the byte you read
#         IS the tag) but corrupts every `else`/template value by stealing its
#         first byte — which, cruelly, is often still a valid-looking byte
#         (e.g. 0x31), so the corruption doesn't fail loudly; it just
#         silently misreads the next field as something plausible-but-wrong
#         until enough drift accumulates to hit an impossible byte.
#      b. Template placeholders use `^1`, `^2` (caret + 1-based index), not
#         `%1`/`%2` — there was no way to know this from the grammar file
#         (which just says `string[template]` with no placeholder syntax
#         documented) or from guessing at common templating conventions; it
#         was only found by looking at a REAL decoded template string
#         ("Unless otherwise noted, bootstrap results are based on ^1 ^2")
#         and noticing the substitution never fired.
#    Neither bug crashed on the first several tables tested (Descriptives,
#    System Settings, Bootstrap Specifications) because those tables' Values
#    happened to all be simple numeric/string/text types with no templates —
#    LESSON: a format decoder that works on 3 easy files is not validated;
#    test against the most STRUCTURALLY VARIED real file available (here, a
#    correlation matrix with bootstrap CIs and a templated footnote), not
#    just the first one that happens to be on disk.
#
# 4. WHEN A BYTE-LEVEL BUG RESISTS MANUAL HEX-TRACING, GET A REAL GROUND-TRUTH
#    DECODE FROM THE ORIGINAL TOOL rather than re-deriving intent from the
#    grammar text over and over. What DID resolve it: PSPP ships a
#    `pspp-output` CLI with an UNDOCUMENTED (`--help-developer`, not
#    `--help`) command `dump-light-table --raw` that prints the exact
#    field-by-field parsed structure of a real .spv file's tables — true
#    ground truth from the same code the grammar describes, down to every
#    footnote's `type: -1` / `template_string.id` / `n-args` /
#    `value_mod: none`. Rendered-output tools (`pspp-convert`, which only
#    emits final HTML/text/PDF) would NOT have been enough — the bug was in
#    intermediate field parsing, invisible in final rendered numbers.
#    PSPP had no official Windows binary (GNU ships source tarballs only);
#    the working installer came from a link ON GNU's OWN
#    gnu.org/software/pspp/get.html page pointing to a university-hosted
#    mirror of the automated Windows build — check the project's own
#    official pages for third-party-but-endorsed binaries before assuming
#    "no package manager entry" means "not installable."
#
# 5. ONCE YOU HAVE GROUND TRUTH, BISECT TOWARD THE DIVERGENCE RATHER THAN
#    RE-READING THE WHOLE FILE. The working method here: dump the ground-truth
#    structure for the exact table that fails, then step the R decoder one
#    field at a time (print the cursor offset after each read) until the
#    offset where MY decode disagrees with ground truth is found, then
#    inspect the raw bytes at exactly that offset. This turned "where is the
#    bug in ~250 lines of parsing code" into "what does byte 419 mean,"
#    which is answerable by re-reading ONE grammar clause carefully instead
#    of the whole grammar.
# ═══════════════════════════════════════════════════════════════════════════

# ── Value (grammar: Value / ValueMod / Argument, decoder: decode_spvlb_value) ─
# A pivot cell's displayed content: one of 6 variants (numeric, numeric-with-
# variable, text/template-string, formatted-string, variable-reference, or a
# generic templated value with sub-arguments). Returns
# list(type, x = <numeric or NA>, s = <string or NA>, format = <int32 or NA>).
# Styling (ValueMod's footnote refs, subscripts, font/cell StylePair) is parsed
# past for correct offsets but not retained, per the "data only" scope above —
# except footnote refs, which ARE retained since they are structural content
# (which footnote marker attaches to this value), not visual styling.
.spvlb_read_value_mod <- function(cur) {
  m31 <- .spvbin_match_bytes(cur, 0x31L)
  if (!m31$matched) {
    cur <- .spvbin_expect_bytes(m31$cur, 0x58L, "ValueMod absent marker")
    return(list(value = list(footnote_refs = integer(0)), cur = cur))
  }
  cur <- m31$cur
  n_refs <- .spvbin_read_int32(cur); cur <- n_refs$cur
  refs <- integer(n_refs$value)
  for (i in seq_len(n_refs$value)) { r <- .spvbin_read_int16(cur); cur <- r$cur; refs[i] <- r$value }
  n_sub <- .spvbin_read_int32(cur); cur <- n_sub$cur
  for (i in seq_len(n_sub$value)) { r <- .spvbin_read_string(cur); cur <- r$cur }  # subscripts, unused
  # v1(00 (i1 | i2) 00? 00? int32 00? 00?) | v3(count(TemplateString StylePair))
  # Version is carried on the cursor by the top-level reader (cur$version, set
  # from the Header's own version field). `i1`/`i2` are alternative literal
  # int32 marker VALUES (1 or 2), not different byte widths — read as one
  # int32 and validated, matching how Leaf's `i2` and Group's `i-1` markers
  # are handled elsewhere in this file.
  if (identical(cur$version, 1L)) {
    cur <- .spvbin_expect_bytes(cur, 0x00L, "ValueMod v1 marker")
    tag <- .spvbin_read_int32(cur); cur <- tag$cur
    if (!tag$value %in% c(1L, 2L))
      stop("spv binary: bad ValueMod v1 tag ", tag$value)
    cur <- .spvbin_match_bytes(cur, 0x00L)$cur
    cur <- .spvbin_match_bytes(cur, 0x00L)$cur
    cur <- .spvbin_read_int32(cur)$cur
    cur <- .spvbin_match_bytes(cur, 0x00L)$cur
    cur <- .spvbin_match_bytes(cur, 0x00L)$cur
  } else {
    cw <- .spvbin_read_count(cur)
    inner <- cw$inner
    inner <- .spvlb_skip_template_string(inner)
    inner <- .spvlb_skip_style_pair(inner)
    cur <- cw$after
  }
  list(value = list(footnote_refs = refs), cur = cur)
}

# TemplateString => count((count((i0 (58 | 31 55))?) (58 | 31 string[id]))?)
# Consumed for byte-alignment only; its content (a display-template override)
# is styling/presentation, not data.
.spvlb_skip_template_string <- function(cur) {
  cw <- .spvbin_read_count(cur)
  inner <- cw$inner
  if (inner$pos < inner$limit) {
    cw2 <- .spvbin_read_count(inner)
    in2 <- cw2$inner
    if (in2$pos < in2$limit) {
      i0 <- .spvbin_read_int32(in2); in2 <- i0$cur
      m <- .spvbin_match_bytes(in2, 0x31L)
      in2 <- if (m$matched) .spvbin_expect_bytes(m$cur, 0x55L, "template string 55 marker") else
        .spvbin_expect_bytes(in2, 0x58L, "template string absent marker")
    }
    inner <- cw2$after
    m <- .spvbin_match_bytes(inner, 0x31L)
    inner <- if (m$matched) .spvbin_read_string(m$cur)$cur else
      .spvbin_expect_bytes(inner, 0x58L, "template id absent marker")
  }
  cw$after
}

# StylePair => (31 FontStyle | 58) (31 CellStyle | 58); consumed, not retained.
.spvlb_skip_style_pair <- function(cur) {
  m <- .spvbin_match_bytes(cur, 0x31L)
  cur <- if (m$matched) .spvlb_skip_font_style(m$cur) else
    .spvbin_expect_bytes(cur, 0x58L, "font style absent marker")
  m <- .spvbin_match_bytes(cur, 0x31L)
  cur <- if (m$matched) .spvlb_skip_cell_style(m$cur) else
    .spvbin_expect_bytes(cur, 0x58L, "cell style absent marker")
  cur
}
# FontStyle => bool*4 string[fg] string[bg] string[typeface] byte[size]
.spvlb_skip_font_style <- function(cur) {
  for (i in 1:4) cur <- .spvbin_read_bool(cur)$cur
  for (i in 1:2) cur <- .spvbin_read_string(cur)$cur
  cur <- .spvbin_read_string(cur)$cur
  .spvbin_read_byte(cur)$cur
}
# CellStyle => int32 int32 double int16*4
.spvlb_skip_cell_style <- function(cur) {
  cur <- .spvbin_read_int32(cur)$cur
  cur <- .spvbin_read_int32(cur)$cur
  cur <- .spvbin_read_double(cur)$cur
  for (i in 1:4) cur <- .spvbin_read_int16(cur)$cur
  cur
}

# Argument => i0 Value[value] | int32[n-values] i0 Value*[n-values]
# The `i0` marker distinguishes single-value (int32 == 0) from multi-value
# arguments; read as a plain int32 and switched on, matching the C union.
.spvlb_read_argument <- function(cur) {
  tag <- .spvbin_read_int32(cur); cur <- tag$cur
  if (tag$value == 0L) {
    v <- .spvlb_read_value(cur)
    return(list(value = list(values = list(v$value)), cur = v$cur))
  }
  n <- tag$value
  zero <- .spvbin_read_int32(cur); cur <- zero$cur   # the i0 preceding the array
  vals <- vector("list", n)
  for (i in seq_len(n)) { v <- .spvlb_read_value(cur); cur <- v$cur; vals[[i]] <- v$value }
  list(value = list(values = vals), cur = cur)
}

# Value => 00? 00? 00? 00? case(01 | 02 | 03 | 04 | 05 | 06 | else)[type]
# The 4 leading optional 00 bytes are a padding artifact of the grammar (seen
# consumed unconditionally as literal bytes in the .grammar file); each is
# matched-if-present, never required, mirroring the `?` there.
#
# The type tags 01-06 are literal-match PROBES, like the 31/58 markers used
# throughout this grammar -- NOT an unconditional "read one byte, then
# switch" the way a C tagged union suggests. Ground-truthed against a real
# file's `else` (template) case via PSPP's own `pspp-output --help-developer
# dump-light-table --raw`: a template Value's very next bytes are ValueMod's
# OWN 31/58 presence flag, with NO discriminator byte consumed first. So each
# of 01..06 is tried as a match-and-consume-if-present probe (like Category's
# Leaf-vs-Group disambiguation elsewhere in this file); if none match, ZERO
# bytes are consumed and the `else` branch's ValueMod starts immediately.
.spvlb_read_value <- function(cur) {
  for (i in 1:4) cur <- .spvbin_match_bytes(cur, 0x00L)$cur
  type <- -1L
  for (t in 1:6) {
    m <- .spvbin_match_bytes(cur, as.integer(t))
    if (m$matched) { type <- t; cur <- m$cur; break }
  }

  out <- switch(as.character(type),
    "1" = { # ValueMod int32[format] double[x]
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      fmt <- .spvbin_read_int32(cur); cur <- fmt$cur
      x <- .spvbin_read_double(cur); cur <- x$cur
      list(type = "numeric", x = x$value, format = fmt$value,
           footnote_refs = vm$value$footnote_refs)
    },
    "2" = { # ValueMod int32[format] double[x] string[var] string[label] byte[show]
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      fmt <- .spvbin_read_int32(cur); cur <- fmt$cur
      x <- .spvbin_read_double(cur); cur <- x$cur
      var_name <- .spvbin_read_string(cur); cur <- var_name$cur
      value_label <- .spvbin_read_string(cur); cur <- value_label$cur
      show <- .spvbin_read_byte(cur); cur <- show$cur
      list(type = "numeric", x = x$value, format = fmt$value,
           var_name = var_name$value, value_label = value_label$value,
           footnote_refs = vm$value$footnote_refs)
    },
    "3" = { # string[local] ValueMod string[id] string[c] bool[fixed]
      local <- .spvbin_read_string(cur); cur <- local$cur
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      id <- .spvbin_read_string(cur); cur <- id$cur
      c_ <- .spvbin_read_string(cur); cur <- c_$cur
      fixed <- .spvbin_read_bool(cur); cur <- fixed$cur
      list(type = "text", s = c_$value, id = id$value,
           footnote_refs = vm$value$footnote_refs)
    },
    "4" = { # ValueMod int32[format] string[label] string[var] byte[show] string[s]
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      fmt <- .spvbin_read_int32(cur); cur <- fmt$cur
      value_label <- .spvbin_read_string(cur); cur <- value_label$cur
      var_name <- .spvbin_read_string(cur); cur <- var_name$cur
      show <- .spvbin_read_byte(cur); cur <- show$cur
      s <- .spvbin_read_string(cur); cur <- s$cur
      list(type = "string", s = s$value, format = fmt$value,
           var_name = var_name$value, value_label = value_label$value,
           footnote_refs = vm$value$footnote_refs)
    },
    "5" = { # ValueMod string[var] string[label] byte[show]
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      var_name <- .spvbin_read_string(cur); cur <- var_name$cur
      var_label <- .spvbin_read_string(cur); cur <- var_label$cur
      show <- .spvbin_read_byte(cur); cur <- show$cur
      list(type = "variable", s = var_name$value, var_label = var_label$value,
           footnote_refs = vm$value$footnote_refs)
    },
    "6" = { # string[local] ValueMod string[id] string[c]
      local <- .spvbin_read_string(cur); cur <- local$cur
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      id <- .spvbin_read_string(cur); cur <- id$cur
      c_ <- .spvbin_read_string(cur); cur <- c_$cur
      list(type = "text", s = c_$value, id = id$value,
           footnote_refs = vm$value$footnote_refs)
    },
    { # else: ValueMod string[template] int32[n-args] Argument*[n-args]
      vm <- .spvlb_read_value_mod(cur); cur <- vm$cur
      template <- .spvbin_read_string(cur); cur <- template$cur
      n_args <- .spvbin_read_int32(cur); cur <- n_args$cur
      args <- vector("list", n_args$value)
      for (i in seq_len(n_args$value)) {
        a <- .spvlb_read_argument(cur); cur <- a$cur; args[[i]] <- a$value
      }
      list(type = "template", s = template$value, args = args,
           footnote_refs = vm$value$footnote_refs)
    })
  list(value = out, cur = cur)
}

# ── Category / Dimension (grammar: Category/Leaf/Group/Dimension) ───────────
# Leaf => 00 00 00 i2 int32[leaf-index] i0
.spvlb_read_leaf <- function(cur) {
  for (b in c(0x00L, 0x00L, 0x00L)) cur <- .spvbin_expect_bytes(cur, b, "Leaf padding")
  i2 <- .spvbin_read_int32(cur); cur <- i2$cur          # the `i2` marker value
  idx <- .spvbin_read_int32(cur); cur <- idx$cur
  i0 <- .spvbin_read_int32(cur); cur <- i0$cur
  list(value = list(leaf_index = idx$value), cur = cur)
}

# Group => bool[merge] 00 01 int32[x23] i-1 int32[n-sub] Category*[n-sub]
.spvlb_read_group <- function(cur) {
  merge <- .spvbin_read_bool(cur); cur <- merge$cur
  cur <- .spvbin_expect_bytes(cur, 0x00L, "Group padding")
  cur <- .spvbin_expect_bytes(cur, 0x01L, "Group padding")
  x23 <- .spvbin_read_int32(cur); cur <- x23$cur
  im1 <- .spvbin_read_int32(cur); cur <- im1$cur        # the `i-1` marker value
  n_sub <- .spvbin_read_int32(cur); cur <- n_sub$cur
  subs <- vector("list", n_sub$value)
  for (i in seq_len(n_sub$value)) {
    c_ <- .spvlb_read_category(cur); cur <- c_$cur; subs[[i]] <- c_$value
  }
  list(value = list(merge = merge$value, subcategories = subs), cur = cur)
}

# Category => Value[name] (Leaf | Group)
# Disambiguated the same way spv-light-decoder.c does: a Group starts with a
# bool (0/1) followed by literal 00 01, while a Leaf starts with three literal
# 00 bytes — probe for the Leaf's fixed prefix first since it is unambiguous.
.spvlb_read_category <- function(cur) {
  name <- .spvlb_read_value(cur); cur <- name$cur
  probe <- .spvbin_match_bytes(cur, c(0x00L, 0x00L, 0x00L))
  if (probe$matched) {
    leaf <- .spvlb_read_leaf(cur)
    return(list(value = list(name = name$value, is_leaf = TRUE,
                              leaf_index = leaf$value$leaf_index), cur = leaf$cur))
  }
  grp <- .spvlb_read_group(cur)
  list(value = list(name = name$value, is_leaf = FALSE,
                     subcategories = grp$value$subcategories), cur = grp$cur)
}

# DimProperties => byte byte int32 bool[hide-dim-label] bool[hide-all-labels]
#                  01 int32[dim-index]
.spvlb_read_dim_properties <- function(cur) {
  cur <- .spvbin_read_byte(cur)$cur
  cur <- .spvbin_read_byte(cur)$cur
  cur <- .spvbin_read_int32(cur)$cur
  hide_dim_label <- .spvbin_read_bool(cur); cur <- hide_dim_label$cur
  hide_all_labels <- .spvbin_read_bool(cur); cur <- hide_all_labels$cur
  cur <- .spvbin_expect_bytes(cur, 0x01L, "DimProperties marker")
  dim_index <- .spvbin_read_int32(cur); cur <- dim_index$cur
  list(value = list(hide_dim_label = hide_dim_label$value,
                     hide_all_labels = hide_all_labels$value), cur = cur)
}

# Dimension => Value[name] DimProperties int32[n-categories] Category*[n-cat]
# Flattens the category tree into (path of names, is_leaf, leaf_index) rows via
# a depth-first walk, since the downstream table only needs, per axis
# position, which leaf_index it is and its label path — not the tree itself.
.spvlb_read_dimension <- function(cur) {
  name <- .spvlb_read_value(cur); cur <- name$cur
  props <- .spvlb_read_dim_properties(cur); cur <- props$cur
  n_cat <- .spvbin_read_int32(cur); cur <- n_cat$cur
  cats <- vector("list", n_cat$value)
  for (i in seq_len(n_cat$value)) {
    c_ <- .spvlb_read_category(cur); cur <- c_$cur; cats[[i]] <- c_$value
  }

  leaves <- list()
  walk <- function(cat, path) {
    label <- .spvlb_value_text(cat$name)
    here <- c(path, label)
    if (isTRUE(cat$is_leaf)) {
      leaves[[length(leaves) + 1L]] <<- list(leaf_index = cat$leaf_index,
                                              path = here)
    } else {
      for (sub in cat$subcategories) walk(sub, here)
    }
  }
  for (c_ in cats) walk(c_, character(0))
  # Leaves are addressed by leaf_index elsewhere (Cells, Axes); index this
  # dimension's leaves by that key for O(1) lookup during table assembly.
  by_index <- stats::setNames(lapply(leaves, `[[`, "path"),
                               vapply(leaves, function(l) as.character(l$leaf_index), character(1)))
  list(value = list(name = .spvlb_value_text(name$value), leaves = by_index,
                     n_leaves = length(leaves)),
       cur = cur)
}

# Best-effort plain-text rendering of a decoded Value, for use as a dimension/
# category label: numeric values render via their own format() (approximated
# with generic formatting, since full SPSS-format rendering is a display
# concern out of scope here — see spv_decode_fmt_spec()'s callers), text/
# string values use their string content, template values substitute each
# "^N" placeholder (confirmed against real output via `pspp-output
# dump-light-table --raw`, e.g. "Unless otherwise noted, bootstrap results are
# based on ^1 ^2") with its Nth argument's rendered value, and otherwise
# return the raw template with unresolved placeholders.
.spvlb_value_text <- function(v) {
  if (is.null(v)) return(NA_character_)
  switch(v$type,
    numeric = if (is.na(v$x) || abs(v$x) >= .Machine$double.xmax) "" else .stat_num_to_chr(v$x),
    string = v$s %||% "",
    variable = v$s %||% "",
    text = v$s %||% "",
    template = {
      txt <- v$s %||% ""
      args <- v$args %||% list()
      for (i in seq_along(args)) {
        vals <- args[[i]]$values %||% list()
        rendered <- paste(vapply(vals, .spvlb_value_text, character(1)), collapse = ", ")
        txt <- gsub(paste0("\\^", i, "\\b"), rendered, txt)
      }
      txt
    },
    "")
}

# ── Top-level Table (grammar: Table, decoder: decode_spvlb_table) ───────────

# Header => 01 00 int32[version] bool*5 int32[x3] int32*4 int64[table-id]
.spvlb_read_header <- function(cur) {
  cur <- .spvbin_expect_bytes(cur, 0x01L, "Header marker")
  cur <- .spvbin_expect_bytes(cur, 0x00L, "Header marker")
  version <- .spvbin_read_int32(cur); cur <- version$cur
  for (i in 1:5) cur <- .spvbin_read_bool(cur)$cur
  for (i in 1:5) cur <- .spvbin_read_int32(cur)$cur
  cur <- .spvbin_read_int64(cur)$cur
  list(value = list(version = version$value), cur = cur)
}

# Titles => Value[title] 01? Value[subtype] 01? 31 Value[user-title] 01?
#           (31 Value[corner-text] | 58) (31 Value[caption] | 58)
.spvlb_read_titles <- function(cur) {
  title <- .spvlb_read_value(cur); cur <- title$cur
  cur <- .spvbin_match_bytes(cur, 0x01L)$cur
  subtype <- .spvlb_read_value(cur); cur <- subtype$cur
  cur <- .spvbin_match_bytes(cur, 0x01L)$cur
  cur <- .spvbin_expect_bytes(cur, 0x31L, "Titles marker")
  user_title <- .spvlb_read_value(cur); cur <- user_title$cur
  cur <- .spvbin_match_bytes(cur, 0x01L)$cur
  m <- .spvbin_match_bytes(cur, 0x31L)
  corner_text <- NULL
  if (m$matched) { ct <- .spvlb_read_value(m$cur); cur <- ct$cur; corner_text <- ct$value
  } else cur <- .spvbin_expect_bytes(cur, 0x58L, "corner-text absent marker")
  m <- .spvbin_match_bytes(cur, 0x31L)
  caption <- NULL
  if (m$matched) { cp <- .spvlb_read_value(m$cur); cur <- cp$cur; caption <- cp$value
  } else cur <- .spvbin_expect_bytes(cur, 0x58L, "caption absent marker")
  list(value = list(title = title$value, subtype = subtype$value,
                     user_title = user_title$value, corner_text = corner_text,
                     caption = caption),
       cur = cur)
}

# Footnotes => int32[n] Footnote*[n]; Footnote => Value[text]
#              (58 | 31 Value[marker]) int32[show]
.spvlb_read_footnotes <- function(cur) {
  n <- .spvbin_read_int32(cur); cur <- n$cur
  out <- vector("list", n$value)
  for (i in seq_len(n$value)) {
    text <- .spvlb_read_value(cur); cur <- text$cur
    m <- .spvbin_match_bytes(cur, 0x31L)
    marker <- NULL
    if (m$matched) { mk <- .spvlb_read_value(m$cur); cur <- mk$cur; marker <- mk$value
    } else cur <- .spvbin_expect_bytes(cur, 0x58L, "footnote marker absent")
    show <- .spvbin_read_int32(cur); cur <- show$cur
    out[[i]] <- list(text = .spvlb_value_text(text$value),
                      marker = if (!is.null(marker)) .spvlb_value_text(marker) else NA_character_)
  }
  list(value = out, cur = cur)
}

# Areas => 00? Area*8; Area's fields are all styling — skipped for offset
# alignment only. v3(...) margins are version-gated on the SAME version the
# Header reported, carried on the cursor as `cur$version` by the top-level
# reader.
.spvlb_skip_area <- function(cur) {
  cur <- .spvbin_read_byte(cur)$cur                       # byte[index]
  cur <- .spvbin_expect_bytes(cur, 0x31L, "Area marker")
  cur <- .spvbin_read_string(cur)$cur                      # typeface
  cur <- .spvbin_read_float(cur)$cur                       # size
  cur <- .spvbin_read_int32(cur)$cur                       # style
  cur <- .spvbin_read_bool(cur)$cur                        # underline
  cur <- .spvbin_read_int32(cur)$cur                       # halign
  cur <- .spvbin_read_int32(cur)$cur                       # valign
  cur <- .spvbin_read_string(cur)$cur                      # fg-color
  cur <- .spvbin_read_string(cur)$cur                      # bg-color
  alt <- .spvbin_read_bool(cur); cur <- alt$cur             # alternate
  cur <- .spvbin_read_string(cur)$cur                      # alt-fg-color
  cur <- .spvbin_read_string(cur)$cur                      # alt-bg-color
  if (identical(cur$version, 3L)) for (i in 1:4) cur <- .spvbin_read_int32(cur)$cur
  cur
}
.spvlb_skip_areas <- function(cur) {
  cur <- .spvbin_match_bytes(cur, 0x00L)$cur
  for (i in 1:8) cur <- .spvlb_skip_area(cur)
  cur
}

# Borders => count(ib1 be32[n] Border*[n] bool[show-grid-lines] 00 00 00)
# Border => be32*3. All styling; only n and the count() length matter for
# skipping past correctly.
.spvlb_skip_borders <- function(cur) {
  cw <- .spvbin_read_count(cur)
  cw$after
}

.spvlb_skip_print_settings <- function(cur) {
  cw <- .spvbin_read_count(cur)
  cw$after
}

.spvlb_skip_table_settings <- function(cur) {
  cw <- .spvbin_read_count(cur)
  cw$after
}

# Formats => int32[n-widths] int32*[n] string[locale] int32[current-layer]
#            bool*3 Y0 CustomCurrency count(v1(X0?) v3(count(X1 count(X2)) count(X3)))
# Y0 => int32[epoch] byte[decimal] byte[grouping]
# CustomCurrency => int32[n-ccs] string*[n-ccs]
# Only the pieces actually used downstream (locale, for encoding fallback) are
# retained; X0/X1/X2/X3's own contents are display/session metadata (dataset
# name, custom currency symbols, row heights, style maps) irrelevant to
# extracted values, so their count()-wrapped bytes are skipped whole rather
# than field-by-field.
.spvlb_read_formats <- function(cur) {
  n_widths <- .spvbin_read_int32(cur); cur <- n_widths$cur
  for (i in seq_len(n_widths$value)) cur <- .spvbin_read_int32(cur)$cur
  locale <- .spvbin_read_string(cur); cur <- locale$cur
  cur <- .spvbin_read_int32(cur)$cur                       # current-layer
  for (i in 1:3) cur <- .spvbin_read_bool(cur)$cur
  cur <- .spvbin_read_int32(cur)$cur                       # Y0.epoch
  cur <- .spvbin_read_byte(cur)$cur                        # Y0.decimal
  cur <- .spvbin_read_byte(cur)$cur                        # Y0.grouping
  n_cc <- .spvbin_read_int32(cur); cur <- n_cc$cur          # CustomCurrency
  for (i in seq_len(n_cc$value)) cur <- .spvbin_read_string(cur)$cur
  cw <- .spvbin_read_count(cur)                             # the outer count(...)
  list(value = list(locale = locale$value), cur = cw$after)
}

# Axes => int32[n-layers] int32[n-rows] int32[n-cols]
#         int32*[n-layers] int32*[n-rows] int32*[n-cols]
.spvlb_read_axes <- function(cur) {
  n_layers <- .spvbin_read_int32(cur); cur <- n_layers$cur
  n_rows <- .spvbin_read_int32(cur); cur <- n_rows$cur
  n_cols <- .spvbin_read_int32(cur); cur <- n_cols$cur
  read_n <- function(cur, n) {
    out <- integer(n)
    for (i in seq_len(n)) { r <- .spvbin_read_int32(cur); cur <- r$cur; out[i] <- r$value }
    list(value = out, cur = cur)
  }
  layers <- read_n(cur, n_layers$value); cur <- layers$cur
  rows <- read_n(cur, n_rows$value); cur <- rows$cur
  cols <- read_n(cur, n_cols$value); cur <- cols$cur
  list(value = list(layers = layers$value, rows = rows$value, columns = cols$value),
       cur = cur)
}

# Cells => int32[n] Cell*[n]; Cell => int64[index] v1(00?) Value
.spvlb_read_cells <- function(cur) {
  n <- .spvbin_read_int32(cur); cur <- n$cur
  out <- vector("list", n$value)
  for (i in seq_len(n$value)) {
    idx <- .spvbin_read_int64(cur); cur <- idx$cur
    if (identical(cur$version, 1L)) cur <- .spvbin_match_bytes(cur, 0x00L)$cur
    v <- .spvlb_read_value(cur); cur <- v$cur
    out[[i]] <- list(index = idx$value, value = v$value)
  }
  list(value = out, cur = cur)
}

#' Decode one light-binary SPV table member into a tidy data frame
#'
#' Reads the raw bytes of a `"<n>_lightTableData.bin"` zip member (the modern,
#' SPSS Statistics 21+ table-data format) and returns a rectangular table:
#' one row per data cell, one column per row/column/layer dimension (holding
#' that cell's category label) plus a `value` column (character, via
#' `.stat_num_to_chr()` for numeric cells so full precision survives). Layer
#' dimensions are included as columns too — filtering to the "current" layer,
#' if desired, is left to the caller, since some multi-layer tables are
#' genuinely all wanted (e.g. every group in a split-file analysis).
#'
#' @param raw raw vector: the exact bytes of the `.bin` zip member.
#' @return a data.frame, or `NULL` if the member cannot be decoded (malformed
#'   or an unsupported version) — callers should treat this as "try the next
#'   fallback", not an error.
#' @keywords internal
.spv_decode_light_table <- function(raw) {
  tryCatch({
    cur <- .spvbin_cursor(raw)
    header <- .spvlb_read_header(cur); cur <- header$cur
    if (!header$value$version %in% c(1L, 3L))
      stop("spv binary: unsupported light-table version ", header$value$version)
    cur$version <- header$value$version

    titles <- .spvlb_read_titles(cur); cur <- titles$cur
    footnotes <- .spvlb_read_footnotes(cur); cur <- footnotes$cur
    cur <- .spvlb_skip_areas(cur)
    cur <- .spvlb_skip_borders(cur)
    cur <- .spvlb_skip_print_settings(cur)
    cur <- .spvlb_skip_table_settings(cur)
    formats <- .spvlb_read_formats(cur); cur <- formats$cur

    n_dims <- .spvbin_read_int32(cur); cur <- n_dims$cur
    dims <- vector("list", n_dims$value)
    for (i in seq_len(n_dims$value)) {
      d <- .spvlb_read_dimension(cur); cur <- d$cur; dims[[i]] <- d$value
    }

    axes <- .spvlb_read_axes(cur); cur <- axes$cur
    cells <- .spvlb_read_cells(cur); cur <- cells$cur

    spv_assemble_table(dims = dims, axes = axes$value, cells = cells$value,
                        title = .spvlb_value_text(titles$value$user_title),
                        footnotes = footnotes$value)
  }, error = function(e) {
    warning("spv: could not decode light-binary table: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}

# Shared by both the light-binary and legacy decoders: given per-dimension
# leaf tables and a flat cell list, unflatten each cell's mixed-radix `index`
# (light-binary) or its already-resolved dimension positions (legacy) into
# row/column/layer category labels, and build the tidy data.frame. `axes`
# gives, for each of layers/rows/columns, the ORDER of dimension positions
# (0-based, into `dims`) that make up that axis — mirroring decode_data_index()
# in spv-light-decoder.c, which decodes the flattened index least-significant
# dimension first.
spv_assemble_table <- function(dims, axes, cells, title = NA_character_,
                                footnotes = list()) {
  if (!length(dims) || !length(cells)) return(NULL)
  n_leaves <- vapply(dims, function(d) max(1L, d$n_leaves), integer(1))

  decode_index <- function(flat) {
    out <- integer(length(dims))
    remainder <- flat
    for (i in rev(seq_along(dims))) {
      if (n_leaves[i] > 0) {
        out[i] <- remainder %% n_leaves[i]
        remainder <- remainder %/% n_leaves[i]
      } else out[i] <- 0L
    }
    out
  }

  dim_names <- vapply(dims, function(d) if (nzchar(d$name %||% "")) d$name else "dim",
                       character(1))
  dim_names <- make.unique(dim_names)

  rows <- lapply(cells, function(cell) {
    idx <- decode_index(cell$index)
    labels <- vector("list", length(dims))
    for (j in seq_along(dims)) {
      leaf <- dims[[j]]$leaves[[as.character(idx[j])]]
      labels[[j]] <- if (!is.null(leaf)) paste(leaf, collapse = " / ") else NA_character_
    }
    names(labels) <- dim_names
    v <- cell$value
    val_chr <- if (identical(v$type, "numeric")) {
      # SPSS's DBL_MAX (~1.7976931348623157e+308) is its sentinel for "not
      # applicable" (e.g. no bootstrap CI for a variable's self-correlation,
      # which is always r=1) -- confirmed against real output via `pspp-output
      # dump-light-table --raw`, where such cells show as this exact value.
      # Rendering it as a literal number would read as a real (huge) result.
      if (is.na(v$x) || abs(v$x) >= .Machine$double.xmax) NA_character_
      else .stat_num_to_chr(v$x)
    } else v$s %||% NA_character_
    c(labels, list(value = val_chr))
  })

  df <- do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
  names(df) <- c(dim_names, "value")
  attr(df, "spv_title") <- title
  attr(df, "spv_footnotes") <- footnotes
  # SPSS's own axis assignment (which dimension is a ROW vs a COLUMN vs a
  # LAYER in the rendered pivot table) -- `axes$rows`/`axes$columns`/
  # `axes$layers` are 0-based dimension INDICES (position into `dims`), from
  # Axes => int32[n-layers] int32[n-rows] int32[n-cols] int32*[n-layers]
  # int32*[n-rows] int32*[n-cols] (see .spvlb_read_axes()). Recorded here as
  # dim_names so a renderer (export_spv_html()'s .spv_table_html()) can pivot
  # the long/tidy `df` back into a genuine cross-tab, the way SPSS itself
  # lays the table out, rather than showing one flat column per dimension.
  attr(df, "spv_row_dims") <- dim_names[axes$rows + 1L]
  attr(df, "spv_col_dims") <- dim_names[axes$columns + 1L]
  attr(df, "spv_layer_dims") <- dim_names[axes$layers + 1L]
  df
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Structure-XML reader (dispatch across tables) ────────────────────────────
# Reader for a ".spv" archive's STRUCTURE/heading XML members
# ("outputViewerNNNNNNNNNN.xml"), which name each output item's analysis
# (`commandName`), carry the exact SPSS syntax that produced it (in a
# container_text log item), and — critically — tell us which binary format
# each table's data is stored in.
#
# Ported from PSPP's src/output/spv/structure-xml.grammar (element structure)
# and the relevant parts of spv.c (spv_decode_container / spv_decode_children
# / spv_heading_read). Unlike the light-binary format, this layer is plain,
# already-well-formed XML (confirmed directly against a real .spv file's
# outputViewer*.xml member before any PSPP source was read at all — no
# port-then-guess risk here, since the tags are self-describing:
# commandName="Frequencies", <log> holding literal SPSS syntax text).
#
# ── LESSONS FROM BUILDING THIS FILE (two real bugs the grammar text alone
# would not have caught, both found by testing against a real archive rather
# than trusting the grammar's own document layout description) ────────────
#
# 1. THE GRAMMAR DESCRIBES ONE DOCUMENT'S ELEMENT NESTING; IT DOES NOT SAY HOW
#    MANY DOCUMENTS THERE ARE OR HOW THEY RELATE TO EACH OTHER. First attempt
#    assumed every "outputViewerNNNN.xml" was a self-contained root_heading
#    with any nested analyses INSIDE it, and explicitly excluded
#    "outputViewerNNNN_heading.xml" siblings as some kind of redundant
#    alternate view. Real files disprove this: a plain-numbered file
#    ("...0000.xml") holds ONLY a log/title container — the syntax text of
#    the command that was run — while the immediately following
#    "..._heading.xml" file holds that command's actual output (nested
#    <heading commandName="..."><container><table>...). These are SEPARATE
#    top-level documents in one flat, numerically-ordered sequence, not one
#    nested inside the other. Consequence for the code: command_name/syntax
#    context must thread ACROSS the whole ordered file sequence (each
#    document's ending state feeds the next document's starting state), not
#    just within one document's own element tree — see .spv_read_structure()
#    passing `command_name`/`syntax` between iterations of its file loop, and
#    .spvsx_walk_heading() returning them back out for that purpose. This was
#    caught by testing end-to-end against a real archive (0 rows found where
#    ~8 were expected) rather than trusting the "one file, fully nested"
#    assumption because it read plausibly from the grammar's flat
#    `(container | heading)*` production rule alone.
#
# 2. XML_TEXT() ON A PARSED HTML DOM INCLUDES <style> CONTENT VERBATIM. The
#    embedded HTML in a "log" text item includes its own inline
#    <style>p{...}</style> block (setting the monospace display font) as an
#    ordinary child element; a browser hides style-element content via CSS
#    semantics, but xml2::xml_text() has no notion of that and returns it as
#    plain text, prefixing the CSS ruleset verbatim onto every extracted
#    syntax string. Fixed by explicitly removing `.//style` nodes before
#    extracting text (see .spvsx_html_text()). This is a generic gotcha for
#    ANY xml2-based "extract visible text from embedded HTML" helper, not
#    specific to this format — worth remembering elsewhere in this codebase.
# ═══════════════════════════════════════════════════════════════════════════

# Read one .spv archive's structure XML and return, per output item found: a
# flat list of list(command_name, syntax, subtype, bin_member, xml_member,
# is_legacy). Only `table` content items are returned (container_text/object/
# image/graph/model/tree items are walked for command_name/syntax context but
# not returned as rows) — matching this port's "core tables only" scope.
#
# `dir_path` is the already-unzipped archive's temp directory (the caller,
# import_spv(), unzips once and passes the directory so this and the binary
# readers share one extraction).
.spv_read_structure <- function(dir_path) {
  members <- list.files(dir_path, recursive = TRUE, full.names = FALSE)
  # EVERY "outputViewerNNNN[_heading].xml" member is its own SEPARATE
  # root_heading document -- confirmed against a real file: plain-numbered
  # files ("...0000.xml") hold a lone log/title container (the SPSS syntax
  # that was run), while "_heading" files hold the actual analysis output,
  # nested as <heading commandName="..."> containing one or more <container>
  # <table> items. These are SIBLINGS in one flat numbered sequence, not one
  # nested inside the other -- so all of them are read, IN NUMERIC ORDER, and
  # the enclosing command_name/syntax context threads forward ACROSS files
  # (a log file's syntax applies to the very next heading file's tables),
  # not just within one file's own tree.
  docs <- members[grepl("^outputViewer[0-9]+(_heading)?\\.xml$", basename(members))]
  ord <- as.integer(gsub("\\D", "", basename(docs)))
  docs <- docs[order(ord)]

  out <- list()
  command_name <- NA_character_
  syntax <- NA_character_
  for (rel in docs) {
    doc <- tryCatch(xml2::read_xml(file.path(dir_path, rel)),
                     error = function(e) NULL)
    if (is.null(doc)) next
    walked <- .spvsx_walk_heading(xml2::xml_root(doc), command_name, syntax)
    out <- c(out, walked$rows)
    command_name <- walked$command_name
    syntax <- walked$syntax
  }
  out
}

# Depth-first walk of one root_heading/heading element's children
# (structure-xml.grammar: `(container | heading)*`), threading down the
# nearest enclosing commandName and the most recent SPSS syntax text seen (a
# `container` with `text[type=log]` -- see decode_container_text() in spv.c)
# so a table item picks up the syntax that was run immediately before it.
#
# Confirmed against a real file that a "log" container and the table(s) it
# describes are NOT nested in one document: SPSS writes them as SEPARATE
# sibling top-level "outputViewerNNNN.xml" documents (log) / "..._heading.xml"
# documents (tables), in one flat numbered sequence -- so command_name/syntax
# must thread ACROSS calls to this function (one per top-level document), not
# just within one call's own recursion. Returns list(rows, command_name,
# syntax): `rows` accumulated by ordinary list-building (each recursive call
# returns its own rows, appended by the caller), `command_name`/`syntax` the
# state to carry into the NEXT sibling document.
.spvsx_walk_heading <- function(node, command_name, syntax) {
  out <- list()
  children <- xml2::xml_find_all(node, "./*[local-name()='container' or local-name()='heading']")
  for (child in children) {
    tag <- xml2::xml_name(child)
    if (tag == "heading") {
      # A subheading: its OWN commandName (if any) becomes the enclosing one
      # for everything nested under it; syntax carries through unchanged
      # until a new log container is seen at this level or below.
      sub_cmd <- xml2::xml_attr(child, "commandName")
      sub <- .spvsx_walk_heading(child, command_name = sub_cmd %||% command_name,
                                 syntax = syntax)
      out <- c(out, sub$rows)
      command_name <- sub$command_name
      syntax <- sub$syntax
      next
    }
    # container: exactly one content child per structure-xml.grammar's
    # `container => label (table | container_text | graph | model | object |
    # image | tree)`.
    content <- xml2::xml_find_first(child, "./*[local-name()!='label']")
    if (is.na(content) || inherits(content, "xml_missing")) next
    ctag <- xml2::xml_name(content)

    if (ctag == "text") {
      # container_text: type=(title|log|text|page-title). A "log" item's own
      # text IS the literal SPSS syntax that was run, extracted from its
      # embedded HTML <log> block (see decode_embedded_html() in spv.c) --
      # here read as plain rendered text, since the syntax itself is what
      # matters, not its HTML markup.
      ttype <- xml2::xml_attr(content, "type")
      if (identical(ttype, "log")) {
        txt <- .spvsx_html_text(content)
        if (nzchar(trimws(txt %||% ""))) syntax <- txt
      }
      next
    }

    if (ctag == "table") {
      ts <- xml2::xml_find_first(content, "./*[local-name()='tableStructure']")
      bin_member <- if (!is.na(ts)) {
        dp <- xml2::xml_find_first(ts, "./*[local-name()='dataPath']")
        if (!is.na(dp)) xml2::xml_text(dp) else NA_character_
      } else NA_character_
      xml_member <- if (!is.na(ts)) {
        p <- xml2::xml_find_first(ts, "./*[local-name()='path']")
        if (!is.na(p)) xml2::xml_text(p) else NA_character_
      } else NA_character_

      out[[length(out) + 1L]] <- list(
        command_name = xml2::xml_attr(content, "commandName") %||% command_name,
        syntax = syntax,
        subtype = xml2::xml_attr(content, "subType"),
        bin_member = bin_member,
        xml_member = xml_member,
        is_legacy = !is.na(xml_member) && nzchar(xml_member %||% ""),
        is_graph = FALSE)
      next
    }

    if (ctag == "graph") {
      # A <vgr:graph> item (a chart): unlike <table>, its dataPath/path
      # children sit DIRECTLY under <graph> (no tableStructure wrapper) --
      # confirmed against a real file's structure XML. dataPath names the
      # chart's raw case data (in the SAME "LegacyBinary" format
      # .spv_decode_legacy_data() already reads for legacy tables -- verified
      # against a real chartData.bin, which decoded cleanly with no format
      # changes needed); path names the chart's own VizML <visualization>
      # description (axes, fitted-curve formulas, titles), read by
      # .spv_decode_chart() in the same way .spv_decode_legacy_table() reads
      # a legacy table's detail-xml.
      dp <- xml2::xml_find_first(content, "./*[local-name()='dataPath']")
      p  <- xml2::xml_find_first(content, "./*[local-name()='path']")
      out[[length(out) + 1L]] <- list(
        command_name = xml2::xml_attr(content, "commandName") %||% command_name,
        syntax = syntax,
        subtype = NA_character_,
        bin_member = if (!is.na(dp)) xml2::xml_text(dp) else NA_character_,
        xml_member = if (!is.na(p)) xml2::xml_text(p) else NA_character_,
        is_legacy = FALSE,
        is_graph = TRUE)
      next
    }
    # object/image/model/tree: not tables or charts, out of this port's
    # scope (see the "core tables only" decision) -- skipped, but still let
    # syntax/command_name context carry forward to later siblings.
  }
  list(rows = out, command_name = command_name, syntax = syntax)
}

# Plain-text content of a container_text's embedded HTML <log>/<text> block.
# The XML wraps real HTML as escaped text content (structure-xml.grammar:
# `html :lang=(en) => TEXT`), so it is parsed a second time as HTML and its
# text extracted -- matching decode_embedded_html()'s approach in spv.c,
# minus the font-style/markup reconstruction that port deliberately skips.
.spvsx_html_text <- function(text_node) {
  html_node <- xml2::xml_find_first(text_node, "./*[local-name()='html']")
  if (is.na(html_node)) return(NA_character_)
  raw <- xml2::xml_text(html_node)
  if (!nzchar(trimws(raw))) return(NA_character_)
  doc <- tryCatch(xml2::read_html(paste0("<div>", raw, "</div>")),
                   error = function(e) NULL)
  if (is.null(doc)) return(trimws(raw))
  # A real log block's own CSS <style> element (setting the monospace font
  # etc.) is present as a genuine child node, and xml_text() on the parsed
  # DOM includes it verbatim (a browser would hide it via CSS semantics
  # xml2 knows nothing about) -- confirmed against a real file, where the
  # raw stylesheet text otherwise prefixes every extracted syntax string.
  # Removed before extracting the visible text.
  div <- xml2::xml_find_first(doc, "//div")
  xml2::xml_remove(xml2::xml_find_all(div, ".//style"))
  txt <- xml2::xml_text(div)
  trimws(gsub("[ \t]+\n", "\n", txt))
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Top-level entry points ────────────────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

#' Read the statistical result tables from an SPSS Viewer (.spv) file
#'
#' Ties the structure reader (`.spv_read_structure()`, which table exists,
#' what analysis produced it, which format its data is in) and the two
#' decoders ([.spv_decode_light_table()] for modern tables,
#' [.spv_decode_legacy_data()] + [.spv_decode_legacy_table()] for pre-21
#' tables) into the SAME shape [read_stat_tables()] already returns from
#' JASP's `analyses.json` and jamovi's protobuf blobs.
#'
#' A table that fails to decode (an unsupported construct in either format,
#' or real malformation) is skipped rather than aborting the whole file, so
#' one bad table does not lose every other result in the same archive.
#'
#' @param path path to a `.spv` file
#'
#' @returns a list of result tables, each `list(analysis, title, data,
#'   syntax, table_index)` — the same shape [read_stat_tables()] returns for
#'   `.jasp`/`.omv` (`syntax` is `.spv`-specific: the exact SPSS syntax that
#'   produced the table, when recoverable). Empty list if the archive has no
#'   structure XML or no tables decode.
#' @export
import_spv <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.spv$", path, ignore.case = TRUE))
    stop("Not a .spv file: ", path)
  tmp <- tempfile("spv_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- tryCatch(suppressWarnings(utils::unzip(path, exdir = tmp)),
                    error = function(e) character(0))
  if (!length(files))
    stop("Could not open '", basename(path), "' as a .spv (zip) archive.")

  .spv_read(tmp)
}

# Shared worker behind import_spv() and read_stat_tables()'s .spv fallback
# path (R/stat-tables.R): both already have the archive unzipped to a temp
# directory by the time they need this, so the actual reading logic takes
# that directory rather than re-unzipping.
.spv_read <- function(dir_path) {
  rows <- tryCatch(.spv_read_structure(dir_path), error = function(e) NULL)
  if (is.null(rows) || !length(rows)) return(list())

  out <- list()
  for (r in rows) {
    if (is.na(r$bin_member) || !nzchar(r$bin_member)) next
    bin_path <- file.path(dir_path, r$bin_member)
    if (!file.exists(bin_path)) next

    if (isTRUE(r$is_graph)) {
      # A chart: dataPath is the SAME "LegacyBinary" case-data format a
      # legacy table's dataPath uses (see .spv_decode_chart()'s header
      # comment), and path is the chart's own VizML description.
      xml_path <- file.path(dir_path, r$xml_member)
      df <- if (!file.exists(xml_path)) NULL else {
        data <- tryCatch(
          .spv_decode_legacy_data(readBin(bin_path, "raw", file.size(bin_path))),
          error = function(e) NULL)
        if (is.null(data)) NULL else
          .spv_decode_chart(readBin(xml_path, "raw", file.size(xml_path)), data)
      }
      if (is.null(df) || !nrow(df)) next
      out[[length(out) + 1L]] <- list(
        analysis = r$command_name,
        title = attr(df, "spv_chart_title") %||% NA_character_,
        data = df,
        syntax = r$syntax,
        is_chart = TRUE)
      next
    }

    df <- if (isTRUE(r$is_legacy)) {
      xml_path <- file.path(dir_path, r$xml_member)
      if (!file.exists(xml_path)) NULL else {
        data <- tryCatch(
          .spv_decode_legacy_data(readBin(bin_path, "raw", file.size(bin_path))),
          error = function(e) NULL)
        if (is.null(data)) NULL else
          .spv_decode_legacy_table(
            readBin(xml_path, "raw", file.size(xml_path)), data,
            title = r$subtype)
      }
    } else {
      tryCatch(.spv_decode_light_table(
        readBin(bin_path, "raw", file.size(bin_path))), error = function(e) NULL)
    }
    if (is.null(df) || !nrow(df)) next

    out[[length(out) + 1L]] <- list(
      analysis = r$command_name,
      title = if (!is.na(r$subtype) && nzchar(r$subtype)) r$subtype else attr(df, "spv_title"),
      data = df,
      syntax = r$syntax,
      is_chart = FALSE)
  }
  if (!length(out)) return(list())
  for (i in seq_along(out)) out[[i]]$table_index <- i
  out
}

#' Recover an .spv file's SPSS syntax as a sibling .sps file
#'
#' A `.spv` file is SPSS's rendered OUTPUT, but its structure XML embeds the
#' exact syntax that produced each result (see `.spv_read_structure()`,
#' `.spv_read()`'s `syntax` field) — the same commands a researcher would
#' normally save separately as a `.sps` syntax file, but here recovered from
#' the output alone. Since `.spv` is classed `data_type = "output"` (see
#' `.data_check_types()` / `.data_check_type()` in R/data_check_helpers.R),
#' this materialises that recovered syntax as real CODE, in a `code`
#' subdirectory alongside the original `.spv` file, so it is discoverable
#' the same way an author's own saved `.sps` file would be — and, crucially,
#' so it then flows through `code_check()`'s ordinary SPSS-language analysis
#' (comments, absolute paths, library lines, ...) unmodified, since
#' `code_lang()` already recognises `.sps` (see `.code_expand_spv()` in
#' R/code_check.R, this function's only caller).
#'
#' @param spv_path path to the `.spv` file.
#' @param code_dir_name name of the sibling code subdirectory to write into,
#'   relative to `spv_path`'s own directory. Default `"code"`.
#' @return the path to the written `.sps` file, or `NA_character_` if the
#'   archive has no recoverable syntax (e.g. every command was skipped by
#'   this port's "core tables only" scope, or the file has no structure XML
#'   at all).
#' @keywords internal
.spv_export_syntax <- function(spv_path, code_dir_name = "code") {
  if (!file.exists(spv_path)) stop("File not found: ", spv_path, call. = FALSE)

  tmp <- tempfile("spvsyntax_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  tryCatch(suppressWarnings(utils::unzip(spv_path, exdir = tmp)),
           error = function(e) NULL)

  rows <- tryCatch(.spv_read_structure(tmp), error = function(e) NULL)
  if (is.null(rows) || !length(rows)) return(NA_character_)

  # Every table sharing one command repeats the SAME syntax string (see
  # .spv_read_structure()'s command_name/syntax threading) -- collapse
  # consecutive repeats so one BOOTSTRAP/CORRELATIONS/etc. command is written
  # once, in the order it was run, not once per result table it produced.
  syntaxes <- vapply(rows, function(r) r$syntax %||% NA_character_, character(1))
  keep <- c(TRUE, syntaxes[-1] != syntaxes[-length(syntaxes)] | is.na(syntaxes[-1]) != is.na(syntaxes[-length(syntaxes)]))
  syntaxes <- syntaxes[keep & !is.na(syntaxes) & nzchar(syntaxes)]
  if (!length(syntaxes)) return(NA_character_)

  code_dir <- file.path(dirname(spv_path), code_dir_name)
  dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(code_dir, paste0(tools::file_path_sans_ext(basename(spv_path)), ".sps"))
  writeLines(paste(syntaxes, collapse = "\n\n"), out_path, useBytes = TRUE)
  out_path
}

#' Export an SPSS Viewer (.spv) file's tables and charts as standalone HTML
#'
#' Unlike `.jasp`/`.omv`, a `.spv` archive carries no rendered view of its
#' own (see the file header) -- so, unlike [export_jasp_html()] and
#' [export_omv_html()], this does not re-export an existing document. It
#' builds a new HTML page from what [import_spv()] already decodes: one
#' heading per analysis (its recovered SPSS syntax shown underneath, when
#' recoverable), one `<table>` per result table, and -- for a chart entry
#' (`import_spv()`'s `is_chart = TRUE`) -- a scatter plot rendered from its
#' own decoded `(x, y)` points with any fitted trend lines SPSS itself
#' computed drawn over it (see [.spv_decode_chart()]), embedded as a base64
#' PNG the same way [export_jasp_html()] inlines its own plots.
#'
#' @param path path to a `.spv` file
#' @param out path to write the HTML file to; defaults to `path` with its
#'   extension replaced by `.html`, written alongside the source file
#'
#' @returns the path written to, invisibly
#' @export
export_spv_html <- function(path, out = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (is.null(out)) out <- sub("\\.spv$", ".html", path, ignore.case = TRUE)

  tables <- import_spv(path)

  body <- if (!length(tables)) {
    "<p>No result tables could be recovered from this .spv file.</p>"
  } else {
    sections <- vector("list", length(tables))
    last_analysis <- NA_character_
    for (i in seq_along(tables)) {
      tb <- tables[[i]]
      heading <- ""
      if (!is.na(tb$analysis %||% NA) && !identical(tb$analysis, last_analysis)) {
        heading <- sprintf("<h2>%s</h2>", .spv_html_escape(tb$analysis))
        last_analysis <- tb$analysis
      }
      title <- if (!is.na(tb$title %||% NA) && nzchar(tb$title))
        sprintf("<h3>%s</h3>", .spv_html_escape(tb$title)) else ""
      body_html <- if (isTRUE(tb$is_chart)) .spv_chart_html(tb$data) else .spv_table_html(tb$data)
      sections[[i]] <- paste0(heading, title, body_html)
    }
    paste(unlist(sections), collapse = "\n")
  }

  html <- sprintf(paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n",
    "<title>%s</title>\n",
    "<style>\n",
    "body { font-family: sans-serif; margin: 2em; }\n",
    "h2 { border-bottom: 1px solid #888; margin-top: 2em; }\n",
    "table { border-collapse: collapse; margin-bottom: 1.5em; }\n",
    "th, td { border: 1px solid #ccc; padding: 4px 10px; font-size: 90%%; text-align: right; }\n",
    "th { background: #f0f0f0; text-align: center; }\n",
    "td:first-child, th:first-child { text-align: left; }\n",
    "</style>\n</head>\n<body>\n<h1>%s</h1>\n%s\n</body>\n</html>\n"),
    .spv_html_escape(basename(path)), .spv_html_escape(basename(path)), body)

  writeLines(html, out, useBytes = TRUE)
  invisible(out)
}

.spv_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# One decoded .spv chart (import_spv()'s (x, y) point shape -- see
# .spv_decode_chart()) rendered as an image, embedded in export_spv_html()
# as a base64 PNG the same way export_jasp_html() inlines JASP's own plots.
# Dispatches on `spv_chart_type` (set by .spv_decode_chart()): "point" draws
# a scatter plot (with any fitted trend lines SPSS itself already computed
# overlaid), "interval" draws a bar chart (x = bin/category value, y = its
# count/summary height -- an interval mark's own axis roles, per the real
# <interval><x variable="value"/><y variable="count"/></interval> shape this
# was ported from), "boxplot" draws a box-and-whisker plot per category.
# Base R graphics only (no new plotting dependency): `grDevices::png()` is
# always available.
.spv_chart_html <- function(df) {
  if (is.null(df) || !nrow(df)) return("")
  chart_type <- attr(df, "spv_chart_type") %||% "point"
  xlab <- attr(df, "spv_chart_xlab") %||% "x"
  ylab <- attr(df, "spv_chart_ylab") %||% "y"

  png_path <- tempfile(fileext = ".png")
  grDevices::png(png_path, width = 640, height = 480, res = 96)
  on.exit(unlink(png_path), add = TRUE)
  tryCatch({
    if (identical(chart_type, "boxplot")) {
      graphics::boxplot(value ~ category, data = df, xlab = xlab, ylab = ylab,
                        col = "#5596E6")
    } else if (identical(chart_type, "interval")) {
      # An <interval> chart's rows are per-CASE (each row's y is that one
      # case's own contribution, e.g. 1) -- confirmed against a real
      # histogram where every row's y was literally 1, so plotting rows
      # as-is draws one sliver per case instead of one bar per bin.
      # summaryStatistic="sum" (seen on real <interval> nodes) is exactly
      # this: sum y by matching x before drawing.
      agg <- stats::aggregate(y ~ x, data = df, FUN = sum)
      agg <- agg[order(agg$x), ]
      heights <- stats::setNames(agg$y, vapply(agg$x, .spv_display_value, character(1)))
      graphics::barplot(heights, xlab = xlab, ylab = ylab, col = "#5596E6", border = NA)
    } else {
      fits <- attr(df, "spv_chart_fits") %||% list()
      graphics::plot(df$x, df$y, xlab = xlab, ylab = ylab, pch = 16,
                     col = grDevices::adjustcolor("black", alpha.f = 0.6))
      fit_colors <- c("#5596E6", "#D70033", "#298626", "#F3672A", "#E3D710")
      if (length(fits)) {
        xr <- seq(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE), length.out = 200)
        for (i in seq_along(fits)) {
          yr <- tryCatch(fits[[i]]$fn(xr), error = function(e) NULL)
          if (!is.null(yr))
            graphics::lines(xr, yr, col = fit_colors[((i - 1) %% length(fit_colors)) + 1], lwd = 2)
        }
        fit_labels <- vapply(fits, function(f)
          if (is.na(f$name %||% NA_character_) || !nzchar(f$name %||% "")) f$expr else f$name,
          character(1))
        graphics::legend("topright", legend = fit_labels,
                         col = fit_colors[seq_along(fits)], lwd = 2, bty = "n", cex = 0.8)
      }
    }
  }, finally = grDevices::dev.off())

  data_uri <- paste0("data:image/png;base64,", base64enc::base64encode(png_path))
  sprintf('<img src="%s" alt="chart" style="max-width: 100%%;">', data_uri)
}

# Round a decoded value cell to 3 decimal places for HTML DISPLAY only, to
# read like SPSS's own on-screen output (e.g. ".841" rather than
# "0.840583589880873"). This is deliberately display-only: the underlying
# data.frame value (used by stat_results_long() / stat_output_json() for
# exact statistical matching against reported results) is never touched,
# only the string written into a rendered <td> here. The .spv format's own
# per-cell display-format spec (which would give the EXACT decimal count SPSS
# used) is decoded from the archive but currently discarded (see the `format`
# field read in .spvlb_read_value(), unused past that point) -- a fixed
# 3-decimal round is a simpler, purely cosmetic stand-in, not a re-derivation
# of that spec.
#
# Two cases are deliberately left un-rounded rather than applying the rule
# blindly:
#   * WHOLE NUMBERS (a case count, N of Items, a df) round to themselves --
#     "397" not "397.000". SPSS never pads an integer statistic with zeros.
#   * VALUES THAT WOULD ROUND TO EXACTLY ZERO (a p-value like 4.7e-108) keep
#     full precision instead, since "0.000" reads as an impossible exact
#     zero rather than "very small" -- a materially misleading display, not
#     just a cosmetic loss of precision.
.spv_display_value <- function(x) {
  # A real NA (an unresolved dimension leaf, or a cell .spvlb_value_text()
  # never produced text for) must render as an EMPTY cell, matching SPSS's
  # own blank display for "not applicable" -- `x %||% ""` only substitutes on
  # NULL, so an actual NA value would otherwise become the literal string
  # "NA" via as.character(NA), which is wrong on two counts: it isn't blank,
  # and it looks like the two-letter category label "NA" some real tables
  # legitimately use (e.g. "North America").
  if (is.na(x %||% NA)) return("")
  x <- as.character(x)
  num <- suppressWarnings(as.numeric(x))
  if (is.na(num) || !is.finite(num) || !grepl("^[-+]?[0-9.]+([eE][-+]?[0-9]+)?$", x))
    return(x)
  if (num == round(num)) return(format(round(num), scientific = FALSE, trim = TRUE))
  rounded <- formatC(num, format = "f", digits = 3)
  if (num != 0 && as.numeric(rounded) == 0) return(x)
  rounded
}

# One decoded .spv table (import_spv()'s long/tidy shape: one row per cell,
# one column per row/column/layer dimension holding that cell's category
# label, plus a "value" column) rendered as an HTML <table> for
# export_spv_html(). Pivots into a genuine cross-tab using the `spv_row_dims`/
# `spv_col_dims` attributes both decoders attach (SPSS's own axis assignment —
# see spv_assemble_table() / .spv_decode_legacy_table()): row-dimension
# columns become a nested row stub (left-hand side, rowspan-merged on repeats,
# the way SPSS itself lays out e.g. "Group" over several statistic rows),
# column-dimension columns become nested column headers (colspan-merged the
# same way), and each cell goes in the resulting grid — instead of listing
# every dimension as its own flat column. Falls back to the old flat, one-
# column-per-dimension listing when the attributes are absent (a table with
# no recorded axis split) or name columns that no longer exist in `df`.
.spv_table_html <- function(df) {
  if (is.null(df) || !nrow(df) || !ncol(df)) return("")
  row_dims <- attr(df, "spv_row_dims") %||% character(0)
  col_dims <- attr(df, "spv_col_dims") %||% character(0)
  row_dims <- row_dims[row_dims %in% names(df)]
  col_dims <- col_dims[col_dims %in% names(df)]

  if ((length(row_dims) || length(col_dims)) &&
      length(row_dims) + length(col_dims) < ncol(df))
    return(.spv_table_html_pivot(df, row_dims, col_dims))

  # Fallback: flat listing (no usable axis split recorded).
  headers <- paste(sprintf("<th>%s</th>", vapply(names(df), .spv_html_escape, character(1))),
                   collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(df[i, , drop = TRUE],
                    function(v) .spv_html_escape(.spv_display_value(v)), character(1))
    paste0("<tr>", paste(sprintf("<td>%s</td>", cells), collapse = ""), "</tr>")
  }, character(1))
  sprintf("<table>\n<thead><tr>%s</tr></thead>\n<tbody>\n%s\n</tbody>\n</table>",
          headers, paste(rows, collapse = "\n"))
}

# Cross-tabulated rendering used by .spv_table_html() when a row/column axis
# split is available. `row_dims`/`col_dims` are the names of df's own
# dimension columns assigned to each axis (any dimension in neither, e.g. a
# LAYER, is dropped from the grid the same way a "current layer" filter would
# apply in SPSS's own Viewer -- out of scope here since only one layer is
# ever present in the tables this port covers).
.spv_table_html_pivot <- function(df, row_dims, col_dims) {
  row_key <- if (length(row_dims))
    do.call(paste, c(as.list(df[row_dims]), sep = "␟")) else rep("", nrow(df))
  col_key <- if (length(col_dims))
    do.call(paste, c(as.list(df[col_dims]), sep = "␟")) else rep("value", nrow(df))

  row_levels <- unique(row_key)
  col_levels <- unique(col_key)

  # One header row per column dimension (nested), each cell colspan-merged
  # across consecutive repeats -- e.g. a two-level column split ("Group" over
  # "Statistic") renders as two stacked <tr>s the way SPSS's own column
  # headers nest. Built top-down (outermost column dimension first); the
  # row-dimension names are spliced into the LAST (innermost) row's left
  # edge afterwards, aligned with the deepest header level the way SPSS's
  # own corner stub sits -- not as a separate row above everything.
  col_parts <- if (length(col_dims))
    strsplit(col_levels, "␟", fixed = TRUE) else list()
  header_row_cells <- if (length(col_dims)) {
    lapply(seq_along(col_dims), function(d) {
      labels <- vapply(col_parts, `[[`, character(1), d)
      rle_lens <- rle(labels)$lengths
      cells <- character(0)
      pos <- 1L
      for (n in rle_lens) {
        cells <- c(cells, if (n > 1L)
          sprintf('<th colspan="%d">%s</th>', n, .spv_html_escape(labels[pos]))
        else sprintf("<th>%s</th>", .spv_html_escape(labels[pos])))
        pos <- pos + n
      }
      cells
    })
  } else list(sprintf("<th>%s</th>", "value"))

  n_levels <- length(header_row_cells)
  header_rows <- vapply(seq_len(n_levels), function(d) {
    left <- if (length(row_dims)) {
      if (d < n_levels)
        if (length(row_dims) > 1L) sprintf('<th colspan="%d"></th>', length(row_dims)) else "<th></th>"
      else paste(sprintf("<th>%s</th>", vapply(row_dims, .spv_html_escape, character(1))),
                collapse = "")
    } else ""
    paste0("<tr>", left, paste(header_row_cells[[d]], collapse = ""), "</tr>")
  }, character(1))

  # Indexed by POSITION (match() into row_levels/col_levels), not by name --
  # a table with no row dimension gives every cell the same "" row_key, and
  # "" cannot be used as a matrix dimname subscript (R treats an empty-string
  # subscript as "no match" rather than "the row named empty string").
  grid <- matrix(NA_character_, nrow = length(row_levels), ncol = length(col_levels))
  ri <- match(row_key, row_levels); ci <- match(col_key, col_levels)
  for (i in seq_len(nrow(df))) grid[ri[i], ci[i]] <- df$value[i]

  row_parts <- if (length(row_dims))
    strsplit(row_levels, "␟", fixed = TRUE) else list()
  body_rows <- vapply(seq_along(row_levels), function(i) {
    stub <- if (length(row_dims))
      paste(sprintf("<td>%s</td>", vapply(row_parts[[i]], .spv_html_escape, character(1))),
            collapse = "")
    else ""
    cells <- paste(sprintf("<td>%s</td>",
                           vapply(grid[i, ], function(v) .spv_html_escape(.spv_display_value(v)), character(1))),
                   collapse = "")
    paste0("<tr>", stub, cells, "</tr>")
  }, character(1))

  sprintf("<table>\n<thead>\n%s\n</thead>\n<tbody>\n%s\n</tbody>\n</table>",
          paste(header_rows, collapse = "\n"), paste(body_rows, collapse = "\n"))
}
