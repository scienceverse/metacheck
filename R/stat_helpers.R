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

# A number as a paper writes it, in the form R reads: spaces removed, the
# Unicode minus and A with a grave accent (GROBID's reading of the minus in
# some PDFs) as "-", and "x 10^", the multiplication sign or A with a
# circumflex or a tilde (GROBID's readings of it) before 10 and an exponent
# as "e": "1.8 x 10 -6" and "2.2\u00c210 \u00c016" are "1.8e-6" and "2.2e-16".
# With `ten_power`, 10 to a power with nothing before it ("10 -15", where
# reading the PDF lost the superscript) is "1e-15": only for a p-value, as
# elsewhere "10-15" can be a range. The one reader of such numbers for
# extract_p_values() and .norm_value(), so the two cannot disagree (#642).
.sci_text <- function(x, ten_power = FALSE) {
  s <- gsub("\\s", "", x)
  s <- gsub("[\u2212\u00c0]", "-", s)
  if (ten_power) s <- sub("^([<>]?)10\\^?(?=-\\d)", "\\11e", s, perl = TRUE)
  gsub("[x*\u00d7\u00c2\u00c3]10\\^?(?=[-+]?\\d)", "e", s, perl = TRUE)
}

# Normalise a value string to a number. APA leading-dot ".06" -> 0.06; strip
# thousands separators; "< .001"/"> .05" -> the bound (with a censored flag);
# scientific notation as papers write it ("1.8 x 10 -6"), through
# .sci_text(); with `ten_power` (a p-value), also "10 -15".
# Returns list(num, dec = #decimals as written, censored = "<"/">"/"").
.norm_value <- function(x, ten_power = FALSE) {
  s <- trimws(as.character(x %||% ""))
  cens <- ""
  if (grepl("^[<>]", s)) { cens <- substr(s, 1, 1); s <- trimws(sub("^[<>]\\s*", "", s)) }
  s <- gsub(",", "", s)
  # "1.8 x 10 -6" was read as 1.8, so a reported p never matched its own
  # output (#642)
  s <- .sci_text(s, ten_power = ten_power)
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

# ---- the decimal point of The Lancet ------------------------------------

# A paper's text with a raised dot between two digits read as a decimal point:
# The Lancet and medRxiv papers in its style write "p = 0\u00b70079", which
# GROBID gives with the middle dot or the bullet ("p = 0\u20220079"), so the
# p-values and tests of such papers were not read at all (#648; 87 values in 2
# of 141 preprints). Between two digits the dot is nothing else in a result.
# `paper` is a paper, a paper list, or a data frame with a `text` column; it
# is returned with that text changed.
.stat_decimal_point <- function(paper) {
  .stat_text_apply(paper, function(x) gsub("(?<=\\d)[\u00b7\u2022\u2219](?=\\d)", ".", x, perl = TRUE))
}

# A paper's text with the chi of a chi-squared test joined to its 2: reading
# a PDF often puts a space between them ("\u03c7 2 (10) = 9.61", "\u03c7 \u00b2 =
# 3.9", "X 2 (1) = 3.20"), and the statistic was then not read (#351). Only
# where a test follows: brackets with the degrees of freedom, or a
# comparison. Chi is never anything else than a chi-squared test in a
# result; a standing X with a 2 is read the same way as .test_stat_head reads
# it.
.stat_chi_join <- function(paper) {
  .stat_text_apply(paper, function(x) {
    x <- gsub("\u03c7\\s+([\u00b22\u2082])(?=\\s*[([=<>\u2264\u2265])", "\u03c7\\1", x, perl = TRUE)
    gsub("(?<![\\w\u0370-\u03ff])X\\s+([\u00b22])(?=\\s*[([=<>\u2264\u2265])", "X\\1", x, perl = TRUE)
  })
}

# Apply `fix` to the text of a paper, of each paper of a paper list, or of a
# data frame with a `text` column. Only a single paper's text was changed
# before, so a paper list kept "p = 0\u00b70079" and its p-values were not read.
.stat_text_apply <- function(paper, fix) {
  if (is.data.frame(paper)) {
    if ("text" %in% names(paper)) paper$text <- fix(paper$text)
  } else if (inherits(paper, "scivrs_paperlist")) {
    paper[] <- lapply(paper, .stat_text_apply, fix = fix)
  } else if (is.list(paper) && is.data.frame(paper$text) && "text" %in% names(paper$text)) {
    paper$text$text <- fix(paper$text$text)
  }
  paper
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

# A test statistic with a value: t(20) =, F =, chi2 =, z =, W =, OR =, b =.
# Not a fit index, R squared, a size or another number, which a table note
# can follow. The fraction one quarter and A with a grave accent are how
# GROBID reads "=" and the minus sign in some journals. Used by
# .p_not_test() (a p-value right after one belongs to it) and
# .reports_test() (#642). A chi-squared test in any of its written forms,
# also with a space before its 2, which reading a PDF often adds
# ("\u03c7 2 (10) =", "X 2 (1) =", "chi-square(2) =") (#351).
.test_stat_head <- paste0(
  "(?<![\\w\u0370-\u03ff-])(?:t|F|z|Z|r|rs|R(?![\u00b22])|W|U|H|D|V|Q|T|B|b|d|g|OR|HR|RR|",
  "\u03c7\\s?[\u00b22\u2082]?|X\\s?[2\u00b2]|chi[- ]?squared?|chi\\s?[2\u00b2]|\u03c1|rho|\u03c4|\u03b2|\u03b7\u00b2?p?|\u03c9\u00b2?)",
  "[0-9]{0,4}\\s*(?:\\([^()]{0,40}\\)|\\[[^][]{0,40}\\])?\\s*",
  "(?:=|<|>|\u2264|\u2265|\u00bc)\\s*[-\u2212\u00c0]?\\s*")
.test_stat_pattern <- paste0(.test_stat_head, "\\.?\\d")
# the same, with a value that is not 0 ("d = 0", "T = 0.00" are left out):
# a p-value of exactly 0 needs a large statistic (#642)
.test_stat_nonzero <- paste0(.test_stat_head, "(?!(?:0+\\.?0*|\\.0+)(?![\\d.]))\\.?\\d")

# Whether each sentence reports a statistical test: a test statistic with a
# value, another p-value written with a decimal point or an exponent, or a
# word that names a test or its outcome. extract_p_values() reads a whole
# "p = 0" or "p = 1" as a p-value only in such a sentence; otherwise it is
# often a probability or a model parameter ("with p = 1", "ARIMA values
# p = 0") (#642). With `nonzero` TRUE (for "p = 0"), a statistic counts only
# with a value that is not 0: "ARIMA values p = 0, d = 0" has no test.
.reports_test <- function(sentence, nonzero = FALSE) {
  sentence[is.na(sentence)] <- ""
  nonzero <- rep_len(nonzero, length(sentence))
  stat <- grepl(.test_stat_pattern, sentence, perl = TRUE)
  stat[nonzero] <- grepl(.test_stat_nonzero, sentence[nonzero], perl = TRUE)
  other_p <- paste0(
    "(?<![\\w])[pP]s?(?:-?values?)?\\s*[=<>\u2264\u2265\u2248~\u00bc]\\s*",
    "(?:\\d?\\.\\d|\\d+\\s*(?:[eE]|[x\u00d7*\u00c2\u00c3]\\s*10)\\s*[-\u2212\u00c0]|10\\s*\\^?\\s*[-\u2212\u00c0]\\s*\\d)")
  words <- paste0(
    "\\btest(?:s|ed|ing)?\\b|significan|\\bdiffer|Fisher|Wilcoxon|Mann.?Whitney|",
    "Kruskal|Kolmogorov|binomial|chi.?squ|AN(?:C|M)?OVA|permut|bootstrap|",
    "Bonferroni|\\bHolm\\b|Tukey|Dunn|Sidak|adjusted|corrected|post.?hoc|",
    "pairwise|comparisons?\\b|likelihood.ratio|\\bLRT\\b|regression|correlat|\\bexact\\b")
  stat |
    grepl(other_p, sentence, perl = TRUE) |
    grepl(words, sentence, perl = TRUE, ignore.case = TRUE)
}

# The symbols of a table note: *, the asterisk operator, a dagger or double
# dagger, a section or paragraph sign, #, ^, ~ and the replacement character;
# also the low asterisk, "@", and A with a tilde, which is how GROBID reads
# an asterisk in some journals ("A-tilde p < 0.05, A-tilde A-tilde p < 0.01") (#642)
.p_note_symbol <- "[*\u2217\u2020\u2021\u00a7#\u00b6^~\ufffd\u00c3\u204e@]"

# The clause of each p-value in its sentence: the text before it back to the
# last ";" or full stop (at most 120 characters), and the text after it on
# to the next (at most 80). The k-th row of a p-value in a sentence is its
# k-th place there, and "p < .05" is not found inside "p < .051". `p` has
# the columns `paper_id`, `text_id`, `text` and `expanded`. Returns a data
# frame with `before` and `after`, NA where the p-value is not found. Used
# by .p_not_test() and stat_p_exact (#604, #642).
.p_clause <- function(p) {
  n <- nrow(p)
  before <- after <- rep(NA_character_, n)
  if (!n) return(data.frame(before = before, after = after))
  sentence <- p$expanded %||% p$text
  sentence[is.na(sentence)] <- ""
  ptext <- trimws(p$text)
  key <- paste(p$paper_id, p$text_id, sentence, ptext, sep = "\r")
  k <- stats::ave(seq_along(key), key, FUN = seq_along)
  # the end of a clause: ";", or a full stop with a space or a marker after it
  boundary <- paste0(";|\\.\\s+|\\.(?=", .p_note_symbol, ")|:\\s+(?=[A-Z])")
  for (i in seq_len(n)) {
    s <- sentence[[i]]
    at <- gregexpr(paste0(gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", ptext[[i]]), "(?![0-9])"),
                   s, perl = TRUE)[[1]]
    if (at[[1]] == -1 || k[[i]] > length(at)) next
    start <- at[[k[[i]]]]
    end <- start + nchar(ptext[[i]]) - 1L
    b <- substr(s, max(1L, start - 120L), start - 1L)
    a <- substr(s, end + 1L, end + 80L)
    cut_b <- gregexpr(boundary, b, perl = TRUE)[[1]]
    if (cut_b[[1]] > 0) {
      last <- length(cut_b)
      b <- substr(b, cut_b[[last]] + attr(cut_b, "match.length")[[last]], nchar(b))
    }
    cut_a <- regexpr(boundary, a, perl = TRUE)
    if (cut_a > 0) a <- substr(a, 1L, cut_a - 1L)
    before[[i]] <- b
    after[[i]] <- a
  }
  data.frame(before = before, after = after)
}

# ---- p-values below 0 or above 1 (#351) ----------------------------------
#
# Rules from all 237,000 "p <comparison> number" in 18 corpora (16,847
# papers), each case read: 50 were negative and 180 were read above 1.

# The power of ten of a p-value in scientific notation, lost or garbled when
# the PDF was read, right after the number: a multiplication sign ("2.4 x",
# GROBID's "9 10 \u00c05" in some journals, "5.0.10 -12"), "10" with an exponent,
# a minus with digits ("1.3 -4"), a superscript or an "e". The number written
# is then not the p-value (99 of the 230).
.p_lost_power <- paste0(
  "^\\s*(?:[x\u00d7X\u00b7\u2022\u22c5\ufffd\u00c3\u00c2*]|9\\s+10\\b|\\.\\s*10\\b",
  "|10\\s*(?:\\^|[-\u2212\u00c0\u207b]|\\s+[-\u2212\u00c0]?\\d|E|e|$)",
  "|[-\u2212\u00c0\u207b]\\s*\\d|[\u2070\u00b9\u00b2\u00b3\u2074-\u2079]|[eE]\\s*[-\u2212+\u00c0]?\\s*\\d)")

# Why a "p" with a value below 0 or above 1 is not a p-value at all, from the
# text right before it (`before`) and right after its number (`after`); NA
# when there is no such reason:
# - "unit": a unit follows the number, so P is a physical quantity ("P = 2.9
#   \u00b5m", "P = 6.02 MPa", "V p = -60 mV");
# - "log": a logarithm of p ("-log10 P = 7.5");
# - "formula": the p is part of a name or a formula, written against "/" or
#   ":" or after a number and a hyphen ("Ca/P = 1.67", "N:P = -0.41", "1-p").
# 50 of the 230 were one of these, none a p-value.
.p_not_a_value <- function(before, after) {
  before[is.na(before)] <- ""
  after[is.na(after)] <- ""
  unit <- paste0(
    "^\\s*(?i:%|\u2030|[\u00b5\u03bcnmck]?m\\b|[kmg]?hz\\b|[mun]?s\\b|ps\\b|[km]?pa\\b|bar\\b|",
    "[mk]?w\\b|[mk]?v\\b|k\\b|\u00b0|[km]?ev\\b|m?g\\b|kg\\b|g/ml|m?l\\b|mmhg\\b|dbm?\\b|",
    "h\\b|hr\\b|min\\b|years?\\b|days?\\b|atm\\b)")
  logp <- "(?i:log|\\bln)\\s*(?:\\(?\\s*10\\s*\\)?|\u2081\u2080)?\\s*\\(?\\s*$"
  formula <- "(?:/|(?<!\\s):|\\d-)$"
  out <- rep(NA_character_, length(before))
  out[grepl(formula, before, perl = TRUE)] <- "formula"
  out[grepl(logp, before, perl = TRUE)] <- "log"
  out[grepl(unit, after, perl = TRUE)] <- "unit"
  out
}

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
#              GROBID's readings of an asterisk (A with a tilde, the low
#              asterisk) and "@"; symbols in brackets before or after it
#              ("(*) p < .05", "p < .01 (**)") (#642);
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

  # a statistic or an estimate: a short name, an optional (df), a comparison
  # and a number; not p (or q, an adjusted p), n, k, df or alpha, which
  # thresholds and notes use too. The fraction one quarter and A with a grave accent are how GROBID reads "=" and
  # the minus sign in some journals.
  stat_name <- paste0(
    "(?<![\\w.\u0370-\u03ff-])(?!(?:p|P|ps|Ps|q|n|N|k|K|df|alpha|value|values|pvalue|pvalues)(?![\\w\u0370-\u03ff]))",
    "(?![pP]_?", .p_subscripts, ")",   # pFWE, padj: a p-value (#642)
    "(?!\u03b1(?![\\w\u0370-\u03ff]))",
    "(?:[A-Za-z\u0370-\u03ff][A-Za-z\u0370-\u03ff0-9\u00b2_]{0,5}|\u03c7\\s?[\u00b22\u2082]?|X\\s[2\u00b2])",
    "\\s*(?:\\([^()]{0,40}\\)|\\[[^][]{0,40}\\])?\\s*",
    "(?:=|<|>|\u2264|\u2265|\u00bc)\\s*[-\u2212\u00c0]?\\s*\\.?\\d")
  ci <- "\\bCI\\b\\s*[\\[(=:]?\\s*[-\u2212\u00c0]?\\.?\\d|\\d+\\s*%\\s*CI\\b"
  test_stat <- .test_stat_pattern
  # a p with a correction name is a p-value too ("pFWE < .05", #642)
  p_value <- paste0("(?<![\\w])[pP]s?\\s*(?:-?values?)?(?:[\\s_-]?", .p_subscripts, ")?",
                    "\\s*(?:=|<|>|\u2264|\u2265|\u00bc)")
  symbol <- .p_note_symbol
  marker <- paste0("(?<![\\w)\\]/])(?:(?:", symbol, "|(?<![\\w/+])\\++)\\s*)+[:=,]?\\s*$")
  # a marker in brackets, before the p-value ("(*) p < .05", "(**) = p < .01")
  # or after it ("p < .01 (**)") (#642)
  marker_br_before <- paste0("\\(\\s*(?:", symbol, "\\s*)+\\)\\s*[:=,]?\\s*$")
  marker_br_after <- paste0("^\\s*\\(\\s*(?:", symbol, "\\s*)+\\)")
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

  ctx <- .p_clause(p)
  for (i in seq_along(out)) {
    s <- sentence[[i]]
    if (is.na(ctx$before[[i]])) next   # not found: may be a result
    before <- ctx$before[[i]]
    after <- ctx$after[[i]]
    clause <- paste(before, after)

    # a marker note: unless a test statistic comes right before the p-value,
    # with no other p-value between them ("t52 = 6.92, ***p < .001"), or
    # right after it ("*p < .05 [z = 2.38]")
    if (grepl(marker, before, perl = TRUE) || grepl(ns_marker, before, perl = TRUE, ignore.case = TRUE) ||
        grepl(marker_br_before, before, perl = TRUE) || grepl(marker_br_after, after, perl = TRUE) ||
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
