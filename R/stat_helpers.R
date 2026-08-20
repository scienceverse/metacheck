# Shared statistics helpers.
#
# Small, format-agnostic utilities used by more than one part of the statistics
# pipeline (the .spv/.smcl/.out/.html syntax recovery, the JASP/jamovi/ipynb
# table readers, read_r_output(), the STATO typing layer, and the
# reported-vs-output matcher). They live here rather than in whichever file
# happened to need one first, so that:
#
#   * a caller does not have to source an unrelated format reader to reuse a
#     one-line formatter -- the reason code_check's syntax recovery previously
#     pulled in the whole of R/r-output.R and R/stat-tables.R;
#   * a helper shared by the paper-prose path and the output path (notably
#     .r_stat_pattern(), which extract_eq() and read_r_output() must agree on)
#     has a single, obvious home, so the two cannot drift apart.
#
# Nothing here knows about a specific file format. Format-specific parsing
# stays in its own file (R/spv.R, R/stata.R, R/mplus.R, R/stat-tables.R, ...).

# ---- numbers and text -------------------------------------------------

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
#
# Formerly .spv_display_value() in R/spv.R; renamed because nothing in it is
# SPSS-specific -- it is the generic "number as a reader expects to see it"
# formatter, and the rounding policy above applies to any result table.
.stat_display_value <- function(x) {
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

# Escape text for inclusion in generated HTML. Previously named
# .spv_html_escape() and defined in R/spv.R, which was misleading: nothing
# about it is SPSS-specific, and four of its five callers (the Stata and Mplus
# HTML exporters, plus the data_check and data_validate report tables) have
# nothing to do with .spv files at all.
#
# Escapes only &, < and > -- the three characters that can break out of HTML
# text content. Quotes are deliberately NOT escaped here: a caller placing text
# inside an HTML ATTRIBUTE needs &#39;/&quot; handling too, and does that
# itself at the point of use (see data_check.R and data_validate.R), because
# doing it here would double-escape the far more common text-content case.
.stat_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# ---- identifiers ------------------------------------------------------

# Reduce any text to a single safe token: lower case, with every run of
# non-alphanumeric characters collapsed to one underscore and leading/
# trailing underscores removed. Used to build result_ids that are usable as
# a filename or a column value without further escaping.
.stat_sanitize_id <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[^a-z0-9]+", "_", x)
  sub("^_|_$", "", x)
}

# ---- statistic names --------------------------------------------------

# Normalise a reported statistic name to a comparison key: lower-cased, Greek
# and superscripts folded to ASCII, punctuation dropped. "\u03b7p\u00b2" -> "etap2",
# "Cohen's d" -> "cohens d", "\u03c7\u00b2" -> "chi2".
.norm_stat_name <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")))
  s <- gsub("\u03b1", "alpha", s)    # alpha (Cronbach's \u03b1)
  s <- gsub("\u03b7", "eta", s)      # eta
  s <- gsub("\u03c7", "chi", s)      # chi
  s <- gsub("\u03b2", "beta", s)     # beta
  s <- gsub("\u03c1", "rho", s)      # rho
  s <- gsub("\u03c4", "tau", s)      # tau
  s <- gsub("\u0394", "delta", s)    # Delta
  s <- gsub("\u03b4", "delta", s)    # delta
  s <- gsub("\u00b2", "2", s)        # superscript 2
  s <- gsub("\u2019|\u2018|'", "", s)
  s <- gsub("[^a-z0-9 ]+", "", s)
  trimws(gsub("\\s+", " ", s))
}

# Sphericity-correction suffixes jamovi appends to repeated-measures ANOVA
# columns: f[gg] / p[hf] / df[none] are still an F, a p and a df -- the bracket
# names WHICH correction was applied (Greenhouse-Geisser, Huynh-Feldt, none),
# which is a property of how the value was computed, not a different quantity.
# Stripping the suffix before lookup types the whole family from the existing
# entries instead of needing one entry per statistic per correction.
# Also covers jamovi's test-variant suffixes (stat[stud] = Student's).
.stato_strip_variant <- function(key) sub("\\[[^]]*\\]$", "", key)

# ---- reported values --------------------------------------------------

# Normalise a value string to a number. APA leading-dot ".06" -> 0.06; strip
# thousands separators; "< .001"/"> .05" -> the bound (with a censored flag).
# Returns list(num, dec = #decimals as written, censored = "<"/">"/"").
.norm_value <- function(x) {
  s <- trimws(as.character(x %||% ""))
  cens <- ""
  if (grepl("^[<>]", s)) { cens <- substr(s, 1, 1); s <- trimws(sub("^[<>]\\s*", "", s)) }
  s <- gsub("[, ]", "", s)
  s <- sub("[^0-9.eE+-].*$", "", s)
  dm <- regmatches(s, regexpr("\\.[0-9]+", s))
  dec <- if (length(dm)) nchar(dm) - 1L else 0L
  s2 <- sub("^(-?)\\.", "\\10.", s)
  num <- suppressWarnings(as.numeric(s2))
  list(num = num, dec = dec, censored = cens)
}

# A bracketed interval, as extract_eq() captures a CI: "[.16, .29]" (the
# regex's own `\\[[^\\]]+\\]` alternative keeps the brackets and the separator
# as one rhs string, deliberately -- see R/text-extractors.R). .norm_value() has
# no notion of a two-number range, so untangling it happens here, upstream of
# .norm_value(): the two inner numbers are extracted and normalised
# separately. Returns NULL when `x` is not bracket-shaped or does not contain
# exactly two separated numbers.
.norm_interval <- function(x) {
  s <- trimws(as.character(x %||% ""))
  if (!grepl("^\\[.*\\]$", s)) return(NULL)
  inner <- sub("^\\[(.*)\\]$", "\\1", s)
  # APA separates CI bounds with a comma ("[.16, .29]"), a semicolon
  # ("[.16; .29]", common outside APA/in some European conventions), or an
  # en/em dash or plain hyphen with no comma/semicolon present ("[.16-.29]").
  # Comma/semicolon are tried first so a negative lower bound ("-0.16") is
  # never mistaken for a dash separator.
  parts <- if (grepl("[,;]", inner)) strsplit(inner, "[,;]")[[1]]
    else strsplit(inner, "(?<=[0-9])\\s*[-\u2013\u2014]\\s*(?=[.0-9])",
                  perl = TRUE)[[1]]
  parts <- trimws(parts)
  if (length(parts) != 2 || any(!nzchar(parts))) return(NULL)
  lo <- .norm_value(parts[1]); hi <- .norm_value(parts[2])
  if (is.na(lo$num) || is.na(hi$num)) return(NULL)
  list(lo = lo, hi = hi)
}

# ---- table cells and headers ------------------------------------------

# Is this cell a PLACEHOLDER rather than a value? JASP renders an empty cell in
# a result table as "." and jamovi as an em/en dash; a table for an analysis the
# user set up but never completed is placeholder in EVERY cell. Emitting those
# produces fully STATO-typed junk -- a "p-value" whose value is "." -- which is
# worse than omitting them, because a downstream matcher sees a p that exists
# but can never match anything. Treated exactly like the already-skipped empty
# cell: the key is omitted, and a result left with no values at all is dropped.
# Deliberately NARROW: only these exact markers (after trimming) count, so a
# real value is never discarded.
.STAT_PLACEHOLDERS <- c(".", "-", "\u2014", "\u2013", "\u2212",
                        "na", "nan", "null", "n/a")

.stat_is_placeholder <- function(x) {
  x <- trimws(as.character(x %||% ""))
  !nzchar(x) || tolower(x) %in% .STAT_PLACEHOLDERS
}

# Which columns of a result table are STATISTICS (vs row-label / structural
# columns). JASP/jamovi lay tables out differently PER TEST -- the label
# column(s) that key each row (test name, model, predictor, group, effect-size
# NAME) can be at the front, in the middle, or have an empty header, and there
# can be several. So classify by CONTENT, not header or position: a column is a
# row-LABEL when its non-empty cells are mostly non-numeric text; a statistic
# column holds (mostly) numbers OR has a header that maps to a known statistic.
# This adapts to every test automatically because it reads the actual column.
# `role` is the source format's OWN declaration of what this column is, when it
# makes one: jamovi's ResultsColumn carries type ("text" for a label column,
# number/integer for a statistic) and format (which can name the quantity
# outright, e.g. "pvalue"). A declaration beats any amount of guessing from cell
# contents -- a transposed t-test column mixes a variable name, a test name and
# then numbers, which no content heuristic classifies correctly -- so it is
# consulted first. NULL for sources that declare nothing (the HTML path), which
# falls through to the content test unchanged.
.stat_is_label_col <- function(header, values, role = NULL) {
  if (!is.null(role)) {
    ty <- tolower(trimws(as.character(role$type %||% "")))
    fm <- tolower(trimws(as.character(role$format %||% "")))
    # A declared quantity in `format` (pvalue, zto, ...) means a statistic.
    if (nzchar(fm)) return(FALSE)
    if (ty %in% c("number", "integer")) return(FALSE)
    if (identical(ty, "text")) return(TRUE)
  }
  h <- tolower(trimws(header %||% ""))
  vals <- trimws(as.character(values %||% character(0)))
  # Placeholders ("." in JASP, an em dash in jamovi) are not content: an
  # all-placeholder column carries no information either way. They must be
  # dropped BEFORE the numeric-content test below, because that test's regex
  # (`[0-9.]+`) matches a bare "." -- so a column of JASP placeholders would
  # otherwise look 100% numeric and be misclassified as a statistic column,
  # producing a junk statistic keyed off an empty header.
  vals <- vals[!vapply(vals, .stat_is_placeholder, logical(1))]
  # A header that types to a known statistic is a statistic column regardless of
  # content (e.g. a p column with "< .001" strings).
  if (nzchar(h) && !identical(stato_type_column(h)$termSource, ""))
    return(FALSE)
  # Content test: a value is "numeric-ish" if it parses as a number, is a
  # reported comparison (< .001), or an infinity -- the forms statistics take.
  if (length(vals) == 0) return(TRUE)   # empty column -> treat as label/spacer
  numlike <- grepl("^[<>=]?\\s*[-+]?[0-9.]+([eE][-+]?[0-9]+)?$", vals) |
             grepl("(?i)^[-+]?inf$", vals, perl = TRUE) |
             grepl("^[<>]\\s*[.0-9]", vals)
  # Label column when fewer than half its cells look numeric.
  mean(numlike) < 0.5
}

# A column header is "ambiguous" when Grobid's own extraction could not have
# given it real meaning: blank, a bare running number ("1.", "2." -- a
# correlation matrix's own column index, not a statistic name), or a dash/blank
# placeholder. .stat_family() returning NA already catches "unrecognised", but a
# blank/numeric header is unrecognised for a DIFFERENT reason (nothing was ever
# there to recognise) than a real-but-unmapped label -- kept as its own check so
# a future .stat_family() addition can't accidentally start treating "1." as a
# real family.
.table_header_ambiguous <- function(header) {
  h <- trimws(header %||% "")
  !nzchar(h) || grepl("^[0-9]+\\.?$|^-+$", h)
}

# ---- shared result pattern --------------------------------------------

# The "<name> [(df)] <op> <value>" statistic pattern, shared with extract_eq().
.r_stat_pattern <- function() {
  operators <- c("=", "<", ">", "~", "\u2248", "\u2260", "\u2264", "\u2265",
                 "\u226a", "\u226b")
  op <- paste(operators, collapse = "")
  gr <- "\u0370-\u03ff"
  list(op = op, pattern = paste0(
    "([", gr, "\u00b2a-zA-Z][", gr, "\u00b2a-zA-Z0-9._-]*)\\s*",  # statistic name
    "(\\([^)]*\\))?\\s*",                                          # optional (df)
    "([", op, "]{1,3})\\s*",                                      # comparator
    # value: a number (no trailing comma), scientific notation, or "< .001".
    "(<\\s*[.0-9]+|-?[0-9]+(?:\\.[0-9]+)?(?:e[-+]?[0-9]+)?)"
  ))
}
