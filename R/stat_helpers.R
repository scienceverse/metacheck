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
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""   # NA became "na" (U138)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)   # both ends: sub() trimmed only one ("_abc_" -> "abc_"; U138)
}

# The id prefix of a source file: "result" when there is none. `%||%` replaced
# only NULL, so an NA source file gave ids such as "na_na_result" (U138).
.stat_source_prefix <- function(source_file) {
  s <- if (length(source_file)) source_file[[1]] else NA_character_
  if (is.null(s) || is.na(s) || !nzchar(trimws(s))) s <- "result"
  .stat_sanitize_id(s)
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
  # the exponent counts: 1.5e-05 has 6 decimals, not 1, which gave a
  # tolerance of 0.05 that matched almost any small value (U143)
  ex <- regmatches(s, regexpr("[eE][+-]?[0-9]+$", s))
  if (length(ex)) dec <- max(0L, dec - as.integer(sub("^[eE]", "", ex)))
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

# ---- p-values that are not the result of a test --------------------------

# Which p-values in sentences are clearly not the result of a test: a
# threshold or a legend, not a finding (#604). A table note ("* p < .05,
# ** p < .01"), a figure legend ("Asterisks indicate significant differences
# (p < .05)") or a stated threshold ("p < .05 was considered significant",
# "thresholded at P < .001") gives the level that marks significance, and is
# not a p-value reported imprecisely. GROBID often puts such notes and
# legends into the running text, so the section cannot tell them apart.
#
# Each p-value is judged in its own clause: the text around it up to a ";"
# or a full stop followed by a space. That keeps the items of a note apart
# ("p < .05. ** p < .01.") and keeps a note apart from a result in the same
# sentence. A clause that holds a test statistic or an estimate (t(20) =,
# F =, r =, b =, M =, a confidence interval) is never taken as a note: the
# p-value then belongs to a result ("F(1, 7) = 8.03, # p < .05").
#
# A p-value is taken as not a test when its clause has, and no statistic:
#   marker     a symbol right before it: *, the asterisk operator, a dagger or double dagger, a section or paragraph sign, #, ^, +, ~, or a
#              repeat of them ("* * * p < .001", "*: p < .05", "** = p < .01");
#              "ns" or "n.s." only with ":" or "=" after it ("ns: p > .05"),
#              or in a clause with a symbol note too
#   threshold  words that state a threshold: a significance level, alpha,
#              "threshold", "cut-off", "criterion", "considered / regarded /
#              accepted as significant", "set at"; not "reached", which
#              reports a result ("did not reach significance levels, ps > .09")
#   legend     a marker that is the subject of a verb of meaning ("Asterisks
#              indicate", "Red dots denote", "Bold values represent"), the
#              passive ("indicated by an asterisk", "highlighted in bold"), or
#              "in bold" / "in grey"
#
# `p` is the table of extract_p_values() with `expanded` (the sentence):
# columns `paper_id`, `text_id`, `text` (the p-value as matched) and
# `expanded`. Returns a character vector, one per row: "marker",
# "threshold", "legend", or NA for a p-value that may be a result.
.p_not_test <- function(p) {
  out <- rep(NA_character_, nrow(p))
  if (!nrow(p)) return(out)
  sentence <- p$expanded %||% p$text
  sentence[is.na(sentence)] <- ""
  ptext <- trimws(p$text)

  # the k-th row of a p-value in a sentence is its k-th place there
  key <- paste(p$paper_id, p$text_id, sentence, ptext, sep = "\r")
  k <- stats::ave(seq_along(key), key, FUN = seq_along)

  # a statistic or an estimate: a short name, an optional (df), a comparison
  # and a number; not p (or q, an adjusted p), n, k, df or alpha, which
  # thresholds and notes use too. The fraction one quarter and A with a grave accent are how GROBID reads "=" and
  # the minus sign in some journals.
  stat_name <- paste0(
    "(?<![\\w.\u0370-\u03ff-])(?!(?:p|P|ps|Ps|q|n|N|k|K|df|alpha|value|values|pvalue|pvalues)(?![\\w\u0370-\u03ff]))",
    "(?!\u03b1(?![\\w\u0370-\u03ff]))",
    "(?:[A-Za-z\u0370-\u03ff][A-Za-z\u0370-\u03ff0-9\u00b2_]{0,5}|\u03c7\u00b2?)",
    "\\s*(?:\\([^()]{0,40}\\)|\\[[^][]{0,40}\\])?\\s*",
    "(?:=|<|>|\u2264|\u2265|\u00bc)\\s*[-\u2212\u00c0]?\\s*\\.?\\d")
  ci <- "\\bCI\\b\\s*[\\[(=:]?\\s*[-\u2212\u00c0]?\\.?\\d|\\d+\\s*%\\s*CI\\b"
  # a test statistic, which a p-value right after it belongs to: not a fit
  # index, R squared, a size or another number, which a table note can follow
  test_stat <- paste0(
    "(?<![\\w\u0370-\u03ff-])(?:t|F|z|Z|r|rs|R(?![\u00b22])|W|U|H|D|V|Q|T|B|b|d|g|OR|HR|RR|",
    "\u03c7\u00b2?|\u03c72|X2|chi2|chi\u00b2|\u03c1|rho|\u03c4|\u03b2|\u03b7\u00b2?p?|\u03c9\u00b2?)",
    "[0-9]{0,4}\\s*(?:\\([^()]{0,40}\\)|\\[[^][]{0,40}\\])?\\s*",
    "(?:=|<|>|\u2264|\u2265|\u00bc)\\s*[-\u2212\u00c0]?\\s*\\.?\\d")
  p_value <- "(?<![\\w])[pP]s?\\s*(?:-?values?)?\\s*(?:=|<|>|\u2264|\u2265|\u00bc)"
  symbol <- "[*\u2217\u2020\u2021\u00a7#\u00b6^~\ufffd]"
  marker <- paste0("(?<![\\w)\\]/])(?:(?:", symbol, "|(?<![\\w/+])\\++)\\s*)+[:=,]?\\s*$")
  ns_marker <- "(?<![\\w.])n\\.?s\\.?\\s*[:=]\\s*$"
  ns_plain <- "(?<![\\w.])n\\.?s\\.?\\s*,?\\s*$"
  # threshold words, each in a form that states a threshold: "threshold" or
  # "alpha" alone also name perception thresholds and alpha power
  thresh <- paste0(
    "thresholded|threshold(?:s|ing)?\\s+(?:of|at|was|were|is|set|for\\s+(?:statistical\\s+)?significance|level)\\b|",
    "(?:significance|statistical|voxel-?(?:wise|level)|cluster(?:-|\\s)?(?:level|wise|forming|extent)?|",
    "height|extent|uncorrected|corrected|FWE|FDR|peak|whole-?brain|lenient|liberal|conservative|stringent)",
    "\\s+thresholds?\\b|threshold-free|",
    "significance\\s+(?:level|criterion|cut-?off)|levels?\\s+of\\s+(?:statistical\\s+)?significance|",
    "\\balpha\\s*(?:level|value|criterion|of|was|is|=|set|\\()|\u03b1\\s*(?:level|value|=|<|\\()|",
    "(?:corrected|adjusted|Bonferroni|nominal)\\s+(?:alpha|\u03b1)\\b|",
    "(?:considered|regarded|accepted|deemed|defined|taken|treated|interpreted)\\s+(?:as\\s+)?(?:to\\s+be\\s+)?",
    "(?:statistically\\s+)?significan|(?:set|fixed)\\s+(?:at|to)\\s+(?:an?\\s+|the\\s+)?(?:alpha|\u03b1|p\\b|P\\b|two|one|\\.|0)|",
    "criteri(?:on|a)\\s+(?:for|of)\\s+(?:statistical\\s+)?significan|as\\s+(?:the|a|our)\\s+criterion|",
    "cut-?off\\s+(?:value\\s+)?(?:of|for|at)\\b|as\\s+(?:the|a)\\s+cut-?off|cut-?off\\s*:|",
    "significan(?:t|ce)\\s+(?:was|were|is|are)\\s+(?:set|defined|accepted|considered|determined|assumed)")
  # words that report a result against the level: "did not reach significance
  # levels", "was found above the level of statistical significance"
  reach <- "\\b(?:reach|exceed|surpass|surviv)\\w*|\\bpass(?:ed|es|ing)?\\b|\\b(?:above|below|beyond)\\s+(?:the\\s+)?(?:\\w+\\s+)?(?:level|threshold|significan)"
  vis <- paste0(
    "asterisks?|stars?|daggers?|symbols?|markers?|letters|superscripts?|",
    "(?:(?:filled|open|solid|dashed|dotted|horizontal|vertical|black|white|gr[ae]y|red|blue|green|",
    "orange|yellow|purple|pink|colou?red|bold|thick|shaded)\\s+)+",
    "(?:dots?|circles?|squares?|triangles?|diamonds?|lines?|bars?|arrows?|symbols?|markers?|",
    "boxes|cells?|areas?|regions?|channels?|clusters?|voxels?|points?|values?|entries|estimates|numbers|fonts?|text)",
    "|bold(?:ed|face)?\\s+(?:values?|entries|estimates|numbers|coefficients|correlations|cells?|text|font)")
  verb <- "(?:indicat|denot|represent|mark|show|highlight|depict|signif|correspond|identif)\\w*"
  legend <- paste0(
    "\\b(?:", vis, ")\\s+(?:\\S+\\s+){0,3}?", verb, "|",
    "(?:indicated|denoted|marked|highlighted|shown|represented|labell?ed|depicted|flagged|printed|",
    "presented|displayed|set|written|given)\\s+(?:by|with|in|as|using)\\s+(?:an?\\s+|the\\s+)?",
    "(?:asterisks?|stars?|daggers?|bold|boldface|italics?|colou?rs?|gr[ae]y|red|blue|green|black|white|",
    "symbols?|letters|superscripts?|lines?|bars?|dots|circles|arrows|shading|boxes|", symbol, ")",
    "|\\bin\\s+(?:bold|boldface|gr[ae]y|colou?r)\\b",
    # "* and ** indicate p < .05 and p < .01", "red dotted line: corrected p < .05"
    "|(?:", symbol, "\\s*)+(?:(?:and|or|,)\\s*(?:", symbol, "\\s*)+)*(?:indicat|denot|represent|mark|correspond)\\w*",
    "|\\b(?:", vis, ")\\s*:")
  # the end of a clause: ";", or a full stop with a space or a marker after it
  boundary <- paste0(";|\\.\\s+|\\.(?=", symbol, ")|:\\s+(?=[A-Z])")

  for (i in seq_along(out)) {
    s <- sentence[[i]]
    # not inside a longer value: "p < .05" is not found in "p < .051"
    at <- gregexpr(paste0(gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", ptext[[i]]), "(?![0-9])"),
                   s, perl = TRUE)[[1]]
    if (at[[1]] == -1 || k[[i]] > length(at)) next   # not found: may be a result
    start <- at[[k[[i]]]]
    end <- start + nchar(ptext[[i]]) - 1L
    before <- substr(s, max(1L, start - 120L), start - 1L)
    after <- substr(s, end + 1L, end + 80L)
    # the clause: back to the last boundary, on to the next
    cut_b <- gregexpr(boundary, before, perl = TRUE)[[1]]
    if (cut_b[[1]] > 0) {
      last <- length(cut_b)
      before <- substr(before, cut_b[[last]] + attr(cut_b, "match.length")[[last]], nchar(before))
    }
    cut_a <- regexpr(boundary, after, perl = TRUE)
    if (cut_a > 0) after <- substr(after, 1L, cut_a - 1L)
    clause <- paste(before, after)

    # a marker note: unless a test statistic comes right before the p-value,
    # with no other p-value between them ("t52 = 6.92, ***p < .001"), or
    # right after it ("*p < .05 [z = 2.38]")
    if (grepl(marker, before, perl = TRUE) || grepl(ns_marker, before, perl = TRUE, ignore.case = TRUE) ||
        (grepl(ns_plain, before, perl = TRUE, ignore.case = TRUE) &&
         grepl(paste0(symbol, "\\s*[pP]\\b"), s, perl = TRUE))) {
      st <- gregexpr(test_stat, before, perl = TRUE)[[1]]
      owned <- FALSE
      if (st[[1]] > 0) {
        last <- length(st)
        between <- substr(before, st[[last]] + attr(st, "match.length")[[last]], nchar(before))
        owned <- !grepl(p_value, between, perl = TRUE)
      }
      next_p <- regexpr(paste0(p_value, "|", symbol), after, perl = TRUE)
      soon <- substr(after, 1L, if (next_p > 0) next_p - 1L else 30L)
      if (!owned && !grepl(test_stat, substr(soon, 1L, 30L), perl = TRUE)) out[[i]] <- "marker"
      next
    }

    # the words of a threshold or a legend: never with a statistic or an
    # estimate anywhere in the clause, and never for "ps" or "all p", which
    # report results ("remained significant when thresholded at varying
    # levels, all Ps < .002")
    if (grepl(stat_name, clause, perl = TRUE) || grepl(ci, clause, perl = TRUE)) next
    if (grepl("^[pP]\\s*['\u2019]?s\\b", ptext[[i]], perl = TRUE) ||
        grepl("\\b(?:all|both|each)\\s*\\(?\\s*$", before, perl = TRUE, ignore.case = TRUE)) next
    # the p-value is the threshold itself: "a threshold of p < .05", "the
    # p < .05 uncorrected threshold", even where clusters "survived" it
    attached <- grepl(paste0("(?:threshold(?:ed|s)?|alpha|\u03b1|cut-?off|criterion|significance\\s+level)",
                             "\\s*(?:of|at|=|:|\\(|was\\s+set\\s+(?:at|to)|set\\s+(?:at|to))?\\s*",
                             "(?:an?\\s+|the\\s+)?(?:(?:two|one)-?\\s*tailed\\s+|(?:un)?corrected\\s+)?$"),
                      before, perl = TRUE, ignore.case = TRUE) ||
      grepl(paste0("^\\s*\\)?\\s*(?:\\(?(?:uncorrected|corrected|FWE|FDR|two-tailed|one-tailed)\\)?,?\\s*)*",
                   "threshold"), after, perl = TRUE, ignore.case = TRUE)
    # a threshold that is measured, not set: "participants showed a higher
    # exit threshold (P < 0.05)", "lower thresholds for CA than DYS"
    # A threshold that is set can be "lower" too ("using a lower statistical
    # threshold of P < .005"): a measured one is compared ("than") or someone
    # has it ("showed a higher threshold").
    comparative <- "(?:higher|lower|greater|smaller|larger|increased|decreased|elevated|reduced|raised)"
    measured <- grepl(paste0("\\b", comparative, "\\s+(?:\\w+\\s+){0,2}thresholds?\\b"),
                      clause, perl = TRUE, ignore.case = TRUE) &&
      (grepl("\\bthan\\b", clause, perl = TRUE, ignore.case = TRUE) ||
       grepl(paste0("\\b(?:showed|shows?|had|has|have|exhibited|displayed|revealed|demonstrated|yielded)\\s+",
                    "(?:an?\\s+|the\\s+)?(?:\\w+\\s+){0,1}", comparative), clause, perl = TRUE, ignore.case = TRUE))
    if (measured) next
    if (attached) {
      out[[i]] <- "threshold"
      next
    }
    if (grepl(thresh, clause, perl = TRUE, ignore.case = TRUE) &&
               !grepl(reach, clause, perl = TRUE, ignore.case = TRUE)) {
      out[[i]] <- "threshold"
    } else if (grepl(legend, clause, perl = TRUE, ignore.case = TRUE)) {
      out[[i]] <- "legend"
    }
  }
  out
}
