# Read Mplus (.out) output files ---------------------------------------------
#
# A .out file is Mplus's own default output: PLAIN TEXT, no markup language
# at all (unlike Stata's `.smcl`, which needs a markup-stripping pass, or
# SPSS's `.spv`, which needs binary decoding) -- confirmed against real files
# found in the local corpus (37 files across 15+ real deposits, the most
# common single genuine statistics-output format found there; see the
# session notes that led to this file). This makes Mplus the simplest of the
# three self-contained-output formats this package reads: the verbatim
# analysis syntax is right there at the top of the file under "INPUT
# INSTRUCTIONS", and every result section is introduced by an ALL-CAPS
# header line ("MODEL FIT INFORMATION", "MODEL RESULTS", ...) with no
# special markup to strip -- Mplus's own analogue of `.smcl`'s {hline}-drawn
# rules or `.spv`'s structure XML.
#
# Table detection reuses the SAME blank-column "river" splitting technique
# as R/r-output.R (.r_output_tables()) and R/stata.R (.stata_output_tables()),
# duplicated here rather than shared per this package's convention (each
# format's reader is a self-contained file) -- Mplus's own table-boundary
# convention (an ALL-CAPS header line, then either a fixed-width table or a
# run of "Label ... value" lines) is different again from both of those.
#
# What is NOT in scope: charts. Mplus can produce plots (via its companion
# Mplus Editor GUI, e.g. scatterplots, histograms of Monte Carlo draws), but
# these are never written into the `.out` text file itself -- there is no
# image data in this format to extract, exactly like `.smcl` and unlike
# `.jasp`/`.omv`.

# The exact sentinel Mplus prints for a FIXED (not estimated) parameter's
# standard error / test statistic / p-value -- e.g. a factor loading fixed to
# 1 for identification shows "1.000  0.000  999.000  999.000". Treated as a
# placeholder (like SPSS's DBL_MAX or the ".") rather than a real value: it is
# not a computed statistic, it is Mplus's way of marking "not applicable".
.MPLUS_FIXED_SENTINEL <- "999.000"

# A top-level section header, Mplus's own structural marker -- the closest
# thing this format has to `.smcl`'s {hline} rules or `.spv`'s structure XML.
# A permissive "is this line all-caps" test is NOT safe here: several bare
# row-group labels *inside* a section body (e.g. "MODEL RESULTS"'s " SW
# WITH", " IB         ON") are themselves all-uppercase and would otherwise
# be misread as the start of a brand new section, fragmenting one real table
# into dozens of bogus ones (confirmed the hard way against a real file).
# Mplus's own mature R parser, MplusAutomation, avoids this the same way: a
# fixed list of known section titles, matched whole-line (`^\s*<title>\s*$`),
# not a structural "looks like a header" heuristic. This vocabulary list is
# adapted from MplusAutomation's own (R/utilityFunctions.R's `headers`
# vector, ~90 entries covering mixture models, Bayesian CIs, IRT, indirect
# effects, plausible values, exploratory factor analysis, etc. -- output
# sections this package's own small 2-file corpus never exercises), since
# their list reflects years of real Mplus output across versions/ANALYSIS
# options that we have no local examples of. A few entries contain regex
# metacharacters (`\d+`, `.*`) for numbered/parameterised titles (e.g.
# "TECHNICAL 1 OUTPUT" vs "TECHNICAL 2 OUTPUT"); matched via one combined
# whole-line-anchored alternation, mirroring their approach.
.MPLUS_SECTION_TITLES <- c(
  "INPUT INSTRUCTIONS",
  "SUMMARY OF ANALYSIS", "SUMMARY OF DATA", "SUMMARY OF DATA FOR THE FIRST DATA SET",
  "SUMMARY OF DATA FOR THE FIRST REPLICATION",
  "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION",
  "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST DATA SET",
  "SUMMARY OF MISSING DATA PATTERNS", "SUMMARY OF CATEGORICAL DATA PROPORTIONS",
  "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION", "COVARIANCE COVERAGE OF DATA",
  "PROPORTION OF DATA PRESENT", "UNIVARIATE SAMPLE STATISTICS",
  "UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS",
  # Not in MplusAutomation's own list, but confirmed as a real sub-heading
  # in our local corpus (a genuine Mplus section title, just one their list
  # doesn't happen to enumerate).
  "THE MODEL ESTIMATION TERMINATED NORMALLY", "SAMPLE STATISTICS",
  "SAMPLE STATISTICS FOR THE FIRST REPLICATION", "RESULTS FOR BASIC ANALYSIS",
  "CROSSTABS FOR CATEGORICAL VARIABLES",
  "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES",
  "SUMMARY OF CENSORED LIMITS", "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES",
  "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST FIT FUNCTION VALUES",
  "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
  "TESTS OF MODEL FIT", "MODEL FIT INFORMATION", "CLASSIFICATION QUALITY",
  "SUMMARY OF MODEL FIT INFORMATION", "RESULTS FOR EXPLORATORY FACTOR ANALYSIS",
  "MODEL RESULTS USE THE LATENT CLASS VARIABLE ORDER",
  "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES",
  "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS",
  "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN",
  "C-SPECIFIC CLASSIFICATION RESULTS",
  "LATENT CLASS INDICATOR MEANS AND PROBABILITIES FOR EACH LATENT CLASS",
  "AVERAGE LATENT CLASS PROBABILITIES FOR MOST LIKELY LATENT CLASS PATTERN \\(ROW\\)",
  "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL",
  "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE",
  "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
  "AVERAGE LATENT CLASS PROBABILITIES FOR MOST LIKELY LATENT CLASS MEMBERSHIP \\(ROW\\)",
  "CLASSIFICATION PROBABILITIES FOR THE MOST LIKELY LATENT CLASS MEMBERSHIP \\(ROW\\)",
  "CLASSIFICATION PROBABILITIES FOR THE MOST LIKELY LATENT CLASS MEMBERSHIP \\(COLUMN\\)",
  "LOGITS FOR THE CLASSIFICATION PROBABILITIES FOR THE MOST LIKELY LATENT CLASS MEMBERSHIP \\(ROW\\)",
  "LOGITS FOR THE CLASSIFICATION PROBABILITIES FOR THE MOST LIKELY LATENT CLASS MEMBERSHIP \\(COLUMN\\)",
  "MODEL RESULTS", "MODEL RESULTS FOR .*", "LOGISTIC REGRESSION ODDS RATIO RESULTS.*",
  "RESULTS IN PROBABILITY SCALE", "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES",
  "IRT PARAMETERIZATION IN TWO-PARAMETER LOGISTIC METRIC",
  "IRT PARAMETERIZATION IN TWO-PARAMETER PROBIT METRIC", "IRT PARAMETERIZATION",
  "BRANT WALD TEST FOR PROPORTIONAL ODDS", "BETWEEN-LEVEL FACTOR SCORE COMPARISONS",
  "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
  "ODDS RATIOS? FOR THE ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
  "ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS",
  "LATENT CLASS ODDS RATIO RESULTS", "LOGRANK OUTPUT", "STANDARDIZED MODEL RESULTS",
  "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER \\d+", "R-SQUARE",
  "QUALITY OF NUMERICAL RESULTS", "QUALITY OF NUMERICAL RESULTS FOR .*",
  "TECHNICAL OUTPUT", "TECHNICAL \\d+ OUTPUT", "TECHNICAL \\d+ OUTPUT FOR THE .* MODEL",
  "TECHNICAL 5/6 OUTPUT",
  "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
  "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES",
  "TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS \\(CAUSALLY-DEFINED EFFECTS\\)",
  "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
  "CONFIDENCE INTERVALS OF MODEL RESULTS",
  "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS.*",
  "CREDIBILITY INTERVALS OF MODEL RESULTS", "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS",
  "CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS",
  "CONFIDENCE INTERVALS IN PROBABILITY SCALE",
  "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
  "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT,",
  "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
  "CONFIDENCE INTERVALS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS",
  "CONFIDENCE INTERVALS OF ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL",
  "EQUALITY TESTS OF MEANS ACROSS CLASSES USING POSTERIOR PROBABILITY-BASED",
  "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE",
  "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE 3-STEP PROCEDURE",
  "EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES",
  "TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS USING",
  "DIFFERENCE OUTPUT", "THE FOLLOWING DATA SET\\(S\\) DID NOT RESULT IN A COMPLETED REPLICATION:",
  "RESIDUAL OUTPUT", "RESIDUAL OUTPUT FOR THE.*", "MODEL MODIFICATION INDICES",
  "MODIFICATION INDICES", "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",
  "SUMMARIES OF PLAUSIBLE VALUES \\(N = NUMBER OF OBSERVATIONS \\* NUMBER OF IMPUTATIONS\\)",
  "SUMMARY OF PLAUSIBLE STANDARD DEVIATION \\(N = NUMBER OF OBSERVATIONS\\)",
  "FACTOR SCORE INFORMATION \\(COMPLETE DATA\\)", "SUMMARY OF FACTOR SCORES",
  "PLOT INFORMATION", "SAVEDATA INFORMATION",
  "CORRELATIONS AND MEAN SQUARE ERROR OF THE TRUE FACTOR VALUES AND THE FACTOR SCORES",
  "RESULTS SAVING INFORMATION", "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES",
  "DIAGRAM INFORMATION",
  "EXPLORATORY FACTOR ANALYSIS WITH [1-9]\\d* FACTOR\\(S\\):",
  "EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND \\d+ BETWEEN FACTOR\\(S\\):",
  "EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND UNRESTRICTED BETWEEN COVARIANCE:",
  "EXPLORATORY FACTOR ANALYSIS WITH UNRESTRICTED WITHIN COVARIANCE AND \\d+ BETWEEN FACTOR\\(S\\):")
.MPLUS_SECTION_HEADER_REGEXPR <- paste0(
  "^(", paste(.MPLUS_SECTION_TITLES, collapse = "|"), ")$")
.mplus_is_section_header <- function(line) {
  tl <- trimws(line)
  nzchar(tl) && grepl(.MPLUS_SECTION_HEADER_REGEXPR, tl, ignore.case = TRUE, perl = TRUE)
}

#' Split a .out file into named sections
#'
#' @param lines character vector: the raw file lines
#' @return a list of `list(title, lines)`, one per ALL-CAPS-headed section
#'   found; content before the first header (Mplus's version banner) is
#'   dropped, since it carries no results
#' @keywords internal
.mplus_sections <- function(lines) {
  header_idx <- which(vapply(lines, .mplus_is_section_header, logical(1)))
  if (!length(header_idx)) return(list())
  ends <- c(header_idx[-1] - 1L, length(lines))
  lapply(seq_along(header_idx), function(k) {
    list(title = trimws(lines[[header_idx[k]]]),
         lines = lines[(header_idx[k] + 1L):ends[k]])
  })
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Fixed-width text tables (duplicated blank-column technique -- see the
# file header for why this is not shared with R/r-output.R or R/stata.R's
# own versions: Mplus's table conventions are genuinely different again). ────
# ═══════════════════════════════════════════════════════════════════════════

.mplus_is_numlike <- function(x) {
  x <- trimws(x)
  grepl("^-?[0-9][0-9.]*(e[-+]?[0-9]+)?\\*?$", x, ignore.case = TRUE) |
  grepl("(?i)^(inf|-?inf|na|nan|\\*+)$", x)
}

.mplus_split_block <- function(block) {
  block <- gsub("\t", strrep(" ", 8), block, fixed = TRUE)
  w <- max(nchar(block))
  padded <- formatC(block, width = -w, flag = "-")
  chars <- do.call(rbind, strsplit(padded, "", fixed = TRUE))
  blank_col <- apply(chars, 2, function(cc) all(cc == " "))
  nb <- !blank_col
  if (!any(nb)) return(NULL)
  d <- diff(c(0L, as.integer(nb), 0L))
  starts <- which(d == 1L); ends <- which(d == -1L) - 1L
  lapply(seq_along(starts), function(k) trimws(substr(padded, starts[k], ends[k])))
}

# A candidate header line inside a section: >= 2 word-groups, not mostly
# numeric, not a lone dashed underline row ("________      ________").
.mplus_looks_header <- function(ln) {
  tl <- trimws(ln)
  if (!nzchar(tl) || grepl("^_+(\\s+_+)*$", tl)) return(FALSE)
  grps <- strsplit(tl, "\\s+")[[1]]; grps <- grps[nzchar(grps)]
  length(grps) >= 1 && mean(.mplus_is_numlike(grps)) < 0.5
}

# A bare group-label line inside a "MODEL RESULTS"-style table: no digits at
# all (a real data row always has numbers), not blank. Confirmed against real
# files: "Within Level", "Between Level", " IW       |", " SW       WITH",
# " Variances", " Residual Variances", " IB         ON", "STDYX
# Standardization" all have this shape -- Mplus's way of tagging which
# parameter block the rows underneath belong to.
.mplus_is_group_label <- function(ln) {
  tl <- trimws(ln)
  nzchar(tl) && !grepl("[0-9]", tl)
}

# A "MODEL RESULTS"-style header line: Mplus draws these result-table column
# headers from a small, fixed, documented vocabulary of statistic names --
# "Estimate", "S.E.", "Est./S.E.", "P-Value" (optionally split onto a leading
# "Two-Tailed" continuation line), or "Observed  Variable" for R-SQUARE.
# Recognising the actual words (rather than trying to structurally
# distinguish a header from a bare group-label line like "Within Level" or
# " SW       WITH", which is genuinely ambiguous by shape alone -- both are
# short, non-numeric text) is what makes header detection unambiguous here.
# NB "observed" alone is NOT in this list even though R-SQUARE's header uses
# it: a Monte Carlo "MODEL FIT INFORMATION" replication-percentile table has
# its own unrelated "Expected    Observed  ..." header that would otherwise
# collide with it (confirmed against a real file) -- "Observed" only counts
# together with "Variable", R-SQUARE's actual two-word header.
.MPLUS_HEADER_WORDS <- c("estimate", "s\\.e\\.", "est\\./s\\.e\\.", "p-value",
                        "two-tailed", "std\\.")
.mplus_is_stat_header_line <- function(ln) {
  tl <- trimws(ln)
  if (!nzchar(tl) || grepl("[0-9]", tl)) return(FALSE)
  grepl(paste(.MPLUS_HEADER_WORDS, collapse = "|"), tl, ignore.case = TRUE) ||
    grepl("observed", tl, ignore.case = TRUE) && grepl("variable", tl, ignore.case = TRUE)
}

.mplus_find_header <- function(lines, start) {
  n <- length(lines)
  if (start > n || !.mplus_is_stat_header_line(lines[[start]])) return(NULL)
  end <- start
  if (start + 1L <= n && .mplus_is_stat_header_line(lines[[start + 1L]]))
    end <- start + 1L
  list(lines = lines[start:end], next_line = end + 1L)
}

# A "Within Level" / "Between Level" (or "Latent Class N", "Group NAME")
# label is a coarser tier of grouping than a plain parameter-block label
# like "Variances" or "IW       |" -- tracked in its own `level` column
# (mirroring MplusAutomation's `BetweenWithin`/`LatentClass`/`Group`
# columns) rather than folded into the same `group` column, since the SAME
# block label (e.g. "Residual Variances") genuinely recurs once per level
# in a multilevel model -- without a separate column, filtering by `group`
# alone can't tell those rows apart.
.mplus_is_level_label <- function(ln) {
  tl <- trimws(ln)
  grepl("^(within|between)(\\s+level)?$", tl, ignore.case = TRUE) ||
    grepl("^(latent class|class|group)\\s+\\S+", tl, ignore.case = TRUE)
}

# Consume one "MODEL RESULTS"-style grouped table starting at a known
# stat-vocabulary header (see .mplus_is_stat_header_line()). Bare no-digit
# lines between data rows (" Within Level", " IW       |", " SW       WITH",
# " Variances", " Residual Variances", ...) update a running `group` label
# (or, for "Within Level"/"Between Level" and similar, a coarser `level`
# label -- see .mplus_is_level_label()) that gets attached to the rows
# below it, rather than ending the table -- real end-of-table is only
# end-of-section or a genuinely new stat-vocabulary header appearing again
# (R-SQUARE repeats the whole Within/Between grouping under a second
# "Observed Variable ..." header).
.mplus_read_grouped_table <- function(lines, start) {
  n <- length(lines)
  hdr <- .mplus_find_header(lines, start)
  if (is.null(hdr)) return(NULL)
  header_lines <- hdr$lines
  j <- hdr$next_line
  data_lines <- character(0)
  group <- NA_character_
  level <- NA_character_
  while (j <= n) {
    dl <- lines[[j]]; dtl <- trimws(dl)
    if (!nzchar(dtl)) {
      k <- j + 1L
      if (k <= n && !is.null(.mplus_find_header(lines, k))) break
      if (k <= n && nzchar(trimws(lines[[k]]))) { j <- j + 1L; next }
      break
    }
    if (!grepl("[0-9]", dtl)) {
      if (.mplus_is_level_label(dtl)) { level <- dtl; group <- NA_character_ } else group <- dtl
      j <- j + 1L; next
    }
    data_lines <- c(data_lines, paste0(level %NA% "", "␟", group %NA% "", "␟", dl))
    j <- j + 1L
  }
  if (!length(data_lines)) return(list(data = NULL, next_line = start + 1L))
  level_col <- sub("␟.*$", "", data_lines)
  has_level <- any(nzchar(level_col))
  rest_lines <- sub("^[^␟]*␟", "", data_lines)
  group_col <- sub("␟.*$", "", rest_lines)
  has_group <- any(nzchar(group_col))
  body_lines <- sub("^[^␟]*␟", "", rest_lines)
  cols <- .mplus_split_block(c(header_lines, body_lines))
  if (is.null(cols) || length(cols) < 2) return(list(data = NULL, next_line = start + 1L))
  n_header <- length(header_lines)
  header <- vapply(cols, function(cl) paste(trimws(cl[seq_len(n_header)]), collapse = " "), character(1))
  body <- lapply(cols, function(cl) cl[-seq_len(n_header)])
  df <- as.data.frame(body, stringsAsFactors = FALSE)
  nm <- trimws(header); nm[!nzchar(nm)] <- paste0("V", which(!nzchar(nm)))
  names(df) <- make.unique(nm)
  if (has_group) df <- cbind(data.frame(group = group_col, stringsAsFactors = FALSE), df)
  if (has_level) df <- cbind(data.frame(level = level_col, stringsAsFactors = FALSE), df)
  if (!any(vapply(df, function(c_) any(.mplus_is_numlike(c_)), logical(1))))
    return(list(data = NULL, next_line = start + 1L))
  list(data = df, next_line = j)
}

# Consume one correlation/covariance-matrix-style table: a variable-name
# header row, an underline row of `________` markers, then one data row per
# variable (its own row label plus one column per variable up to and
# including the diagonal -- Mplus prints these as a LOWER triangle). No
# grouping in this shape -- ends at the first blank line. The underline row
# is REQUIRED here (not made optional): loosening it to also catch
# "UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS" (a different,
# no-underline table shape -- see [.mplus_read_stats_table()]) let this
# path misfire inside "MODEL FIT INFORMATION"'s label/value lines, which
# happen to superficially resemble a short header too (confirmed the hard
# way: it re-fragmented an already-fixed section).
.mplus_read_matrix_table <- function(lines, start) {
  n <- length(lines)
  if (start > n || !.mplus_looks_header(lines[[start]])) return(NULL)
  header_lines <- lines[start]
  j <- start + 1L
  if (j <= n && .mplus_looks_header(lines[[j]]) &&
      (j + 1L > n || grepl("^_+", trimws(lines[[j + 1L]]))))
    { header_lines <- c(header_lines, lines[[j]]); j <- j + 1L }
  if (!(j <= n && grepl("^_+(\\s+_+)*$", trimws(lines[[j]])))) return(NULL)
  j <- j + 1L
  data_lines <- character(0)
  while (j <= n) {
    dl <- lines[[j]]; dtl <- trimws(dl)
    if (!nzchar(dtl) || !grepl("[0-9]", dtl)) break
    data_lines <- c(data_lines, dl)
    j <- j + 1L
  }
  if (!length(data_lines)) return(list(data = NULL, next_line = start + 1L))
  cols <- .mplus_split_block(c(header_lines, data_lines))
  if (is.null(cols) || length(cols) < 2) return(list(data = NULL, next_line = start + 1L))
  n_header <- length(header_lines)
  header <- vapply(cols, function(cl) paste(trimws(cl[seq_len(n_header)]), collapse = " "), character(1))
  body <- lapply(cols, function(cl) cl[-seq_len(n_header)])
  df <- as.data.frame(body, stringsAsFactors = FALSE)
  nm <- trimws(header); nm[!nzchar(nm)] <- paste0("V", which(!nzchar(nm)))
  names(df) <- make.unique(nm)
  if (!any(vapply(df, function(c_) any(.mplus_is_numlike(c_)), logical(1))))
    return(list(data = NULL, next_line = start + 1L))
  list(data = df, next_line = j)
}

# "UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS"'s own table shape:
# a two-line header naming its own small fixed vocabulary ("Variable",
# "Mean", "Skewness", "Kurtosis", "Minimum", "Maximum", "Percentiles",
# "Median"), NO underline row, and each variable's data spanning TWO
# physical lines (row 1: variable name + mean/skewness/minimum/etc.; row 2:
# sample size + variance/kurtosis/maximum/etc., right-aligned under the same
# columns). Recognised by its own header vocabulary rather than reusing the
# generic underline-optional matrix-table path, since that ambiguity once
# let this shape's detector misfire inside "MODEL FIT INFORMATION"'s
# label/value lines.
.MPLUS_STATS_HEADER_WORDS <- c("skewness", "kurtosis", "percentiles")
.mplus_is_stats_header_line <- function(ln) {
  tl <- trimws(ln)
  nzchar(tl) && !grepl("[0-9]", tl) &&
    grepl(paste(.MPLUS_STATS_HEADER_WORDS, collapse = "|"), tl, ignore.case = TRUE)
}
.mplus_read_stats_table <- function(lines, start) {
  n <- length(lines)
  if (start > n || !.mplus_is_stats_header_line(lines[[start]])) return(NULL)
  header_lines <- lines[start]
  j <- start + 1L
  if (j <= n && .mplus_looks_header(lines[[j]])) { header_lines <- c(header_lines, lines[[j]]); j <- j + 1L }
  # A single blank line commonly separates the two-line header from the
  # data rows (confirmed against the real file: header at lines 2-3, data
  # starting at line 5, with line 4 blank).
  if (j <= n && !nzchar(trimws(lines[[j]]))) j <- j + 1L
  data_lines <- character(0)
  while (j <= n) {
    dl <- lines[[j]]; dtl <- trimws(dl)
    if (!nzchar(dtl) || !grepl("[0-9]", dtl)) break
    data_lines <- c(data_lines, dl)
    j <- j + 1L
  }
  if (!length(data_lines)) return(list(data = NULL, next_line = start + 1L))
  cols <- .mplus_split_block(c(header_lines, data_lines))
  if (is.null(cols) || length(cols) < 2) return(list(data = NULL, next_line = start + 1L))
  n_header <- length(header_lines)
  header <- vapply(cols, function(cl) paste(trimws(cl[seq_len(n_header)]), collapse = " "), character(1))
  body <- lapply(cols, function(cl) cl[-seq_len(n_header)])
  df <- as.data.frame(body, stringsAsFactors = FALSE)
  nm <- trimws(header); nm[!nzchar(nm)] <- paste0("V", which(!nzchar(nm)))
  names(df) <- make.unique(nm)
  if (!any(vapply(df, function(c_) any(.mplus_is_numlike(c_)), logical(1))))
    return(list(data = NULL, next_line = start + 1L))
  list(data = df, next_line = j)
}

#' Extract fixed-width result tables from one Mplus section's lines
#'
#' Handles Mplus's two real table shapes, found across the local corpus: (1)
#' a "Model Results"-style table with a stat-vocabulary column header
#' ([.mplus_read_grouped_table()]) over rows split into named groups (e.g.
#' "Within Level" / "Between Level", then within each " IW       |" / "
#' Variances" / " Residual Variances"); and (2) a correlation/covariance
#' matrix table with a variable-name header and an underline row
#' ([.mplus_read_matrix_table()]). The two shapes are genuinely
#' distinguishable only by trying the stat-vocabulary header match first,
#' since a bare group-label line (" SW       WITH") and a real one-line
#' header are otherwise indistinguishable by shape alone -- see
#' [.mplus_is_stat_header_line()].
#'
#' @param lines character vector: one section's lines (from
#'   [.mplus_sections()])
#' @return a list of `list(title, data)`, one per detected table, plus a
#'   `consumed` logical vector (same length as `lines`) marking every line
#'   used by a successfully-extracted table -- passed to
#'   [.mplus_output_labelvalue()] so it never re-parses (and mangles) rows
#'   already captured here, e.g. a "MODEL RESULTS" data row like "IW
#'   0.151  0.022  6.887  0.000" otherwise also matches the generic
#'   label/one-trailing-value regex on its LAST column alone.
#' @keywords internal
.mplus_output_tables <- function(lines) {
  n <- length(lines); i <- 1L; tables <- list()
  consumed <- rep(FALSE, n)
  while (i <= n) {
    res <- .mplus_read_grouped_table(lines, i)
    if (is.null(res)) res <- .mplus_read_stats_table(lines, i)
    if (is.null(res)) res <- .mplus_read_matrix_table(lines, i)
    if (is.null(res)) { i <- i + 1L; next }
    if (!is.null(res$data)) {
      tables[[length(tables) + 1L]] <- list(title = NA_character_, data = res$data)
      consumed[i:(res$next_line - 1L)] <- TRUE
    }
    i <- res$next_line
  }
  attr(tables, "consumed") <- consumed
  tables
}

# ── One-line "Label ... value" results (e.g. "Number of Free Parameters 17",
# "Degrees of Freedom    15", "P-Value    0.0210") -- a section like "MODEL
# FIT INFORMATION" is mostly this shape, not a fixed-width table at all: one
# label, right-padded with spaces, then one trailing number. Collected as
# ONE table per section (all its label/value pairs as columns of a single
# row), matching how .stata_output_oneline() groups fragments from one
# command into one row.
.mplus_output_labelvalue <- function(lines, title) {
  # A label/value line: text, then 2+ spaces, then a single numeric token
  # (optionally trailed by "*" for a footnoted chi-square, or "D-01"-style
  # scientific notation Mplus itself uses, e.g. "0.100D-05").
  pat <- "^\\s*([A-Za-z][A-Za-z0-9 ./()%*-]*?)\\s{2,}(-?[0-9][0-9.]*(?:D[-+]?[0-9]+)?\\*?)\\s*$"
  stat <- character(0); val <- character(0)
  for (ln in lines) {
    m <- regmatches(ln, regexec(pat, ln))[[1]]
    if (length(m) == 3) { stat <- c(stat, trimws(m[[2]])); val <- c(val, m[[3]]) }
  }
  if (!length(stat)) return(list())
  df <- data.frame(as.list(stats::setNames(val, make.unique(stat))),
                   check.names = FALSE, stringsAsFactors = FALSE)
  list(list(title = title, data = df))
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Top-level entry points ────────────────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

#' Check whether a .out file is genuinely Mplus output
#'
#' `.out` is a very generic extension (also used for compiled Unix binaries,
#' generic tool logs, etc.), unlike `.spv`/`.smcl` which are unambiguously
#' SPSS/Stata-specific -- so, unlike those formats, a `.out` found in a
#' repository is not safely assumed to be Mplus by extension alone.
#' Classification (`data_classify_files()`/`.fixed_ext_type`) still keys on
#' the extension so a `.out` is downloaded in the first place (content can't
#' be checked before it's fetched), but this content check runs AFTER
#' download to catch and reclassify a false positive -- Mplus always prints
#' its own version banner as the file's first real line, so this is a cheap,
#' reliable real/not-real test.
#'
#' @param path path to a local file
#' @return `TRUE` if the file's content is genuine Mplus output, `FALSE`
#'   otherwise (including if it cannot be read at all)
#' @keywords internal
.mplus_is_genuine_output <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8", n = 5),
                    error = function(e) character(0))
  any(grepl("^Mplus VERSION", lines))
}

#' Read an Mplus (.out) output file
#'
#' Splits the file into its ALL-CAPS-headed sections (Mplus's own structural
#' marker -- see the file header) and extracts each section's fixed-width
#' result tables and label/value statistics. The `INPUT INSTRUCTIONS`
#' section (Mplus's own verbatim analysis syntax) is skipped here -- see
#' [.mplus_export_syntax()] for recovering it separately as a sibling `.inp`
#' file, the same way [.spv_export_syntax()] recovers `.sps` from `.spv`.
#'
#' @param path path to a `.out` file
#'
#' @returns a list of result tables, each `list(analysis, title, data,
#'   syntax, table_index)` -- the same shape [read_stat_tables()] returns
#'   for `.jasp`/`.omv`/`.spv`, so all formats can be processed identically
#'   downstream. `analysis` is the section title (e.g. `"MODEL RESULTS"`);
#'   `syntax` is the file's own recovered `INPUT INSTRUCTIONS` block (the
#'   same text on every table, since one `.out` file is one Mplus run).
#'   Empty list if the file has no recoverable section.
#' @export
import_mplus_output <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.out$", path, ignore.case = TRUE))
    stop("Not a .out file: ", path)

  if (!.mplus_is_genuine_output(path))
    stop("Not a Mplus .out file: ", path)

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  syntax <- .mplus_syntax_lines(lines)
  sections <- .mplus_sections(lines)
  if (!length(sections)) return(list())

  out <- list()
  for (sec in sections) {
    if (identical(sec$title, "INPUT INSTRUCTIONS")) next
    tabs <- .mplus_output_tables(sec$lines)
    consumed <- attr(tabs, "consumed")
    remaining_lines <- sec$lines[!consumed]
    blocks <- c(tabs, .mplus_output_labelvalue(remaining_lines, sec$title))
    for (b in blocks) {
      if (is.null(b$data) || !nrow(b$data) || !ncol(b$data)) next
      out[[length(out) + 1L]] <- list(
        analysis = sec$title, title = b$title %||% sec$title,
        data = b$data, syntax = syntax)
    }
  }
  if (!length(out)) return(list())
  for (i in seq_along(out)) out[[i]]$table_index <- i
  out
}

# The file's own "INPUT INSTRUCTIONS" block, as a single collapsed string --
# shared by import_mplus_output() (as the `syntax` field on every table) and
# .mplus_export_syntax() (written verbatim to a sibling .inp file). Returns
# NA_character_ when no such section exists (should not happen in a real
# Mplus file, but a truncated/corrupted one is handled gracefully).
.mplus_syntax_lines <- function(lines) {
  start <- which(grepl("^INPUT INSTRUCTIONS\\s*$", trimws(lines)))
  if (!length(start)) return(NA_character_)
  start <- start[[1]] + 1L
  # Ends at the next blank-then-nonblank ALL-CAPS header, OR at a "*** WARNING"/
  # "*** ERROR" block (Mplus prints those immediately after the echoed input,
  # still logically part of "what happened before the model ran" but not
  # part of the syntax itself).
  rest <- lines[start:length(lines)]
  end_rel <- which(vapply(rest, .mplus_is_section_header, logical(1)) |
                   grepl("^\\*\\*\\*\\s*(WARNING|ERROR)", trimws(rest)))
  body <- if (length(end_rel)) rest[seq_len(end_rel[[1]] - 1L)] else rest
  body <- body[nzchar(trimws(body))]
  if (!length(body)) return(NA_character_)
  paste(trimws(body), collapse = "\n")
}

#' Recover a .out file's Mplus syntax as a sibling .inp file
#'
#' A `.out` file's own `INPUT INSTRUCTIONS` section IS the exact Mplus
#' syntax that produced it -- unlike `.spv` (syntax recovered from a
#' SEPARATE structure element), and like `.smcl` (a command echo IS the
#' syntax). This materialises that block as a real `.inp` file (Mplus's own
#' input-syntax extension), in a `code` subdirectory alongside the original
#' `.out` file, so it is discoverable the same way an author's own saved
#' `.inp` file would be -- see `.code_expand_mplus()` in R/code_check.R,
#' this function's only caller.
#'
#' @param out_path path to the `.out` file
#' @param code_dir_name name of the sibling code subdirectory to write into,
#'   relative to `out_path`'s own directory. Default `"code"`.
#' @return the path to the written `.inp` file, or `NA_character_` if the
#'   file has no recoverable `INPUT INSTRUCTIONS` section.
#' @keywords internal
.mplus_export_syntax <- function(out_path, code_dir_name = "code") {
  if (!file.exists(out_path)) stop("File not found: ", out_path, call. = FALSE)
  if (!.mplus_is_genuine_output(out_path)) return(NA_character_)
  lines <- tryCatch(readLines(out_path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (!length(lines)) return(NA_character_)
  syntax <- .mplus_syntax_lines(lines)
  if (is.na(syntax) || !nzchar(syntax)) return(NA_character_)

  code_dir <- file.path(dirname(out_path), code_dir_name)
  dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(code_dir, paste0(tools::file_path_sans_ext(basename(out_path)), ".inp"))
  writeLines(strsplit(syntax, "\n", fixed = TRUE)[[1]], out_file, useBytes = TRUE)
  out_file
}

#' Export an Mplus (.out) output file as standalone HTML
#'
#' Builds an HTML page from what [import_mplus_output()] already decodes:
#' one heading per section (`SUMMARY OF ANALYSIS`, `MODEL FIT INFORMATION`,
#' `MODEL RESULTS`, ...) and one `<table>` per detected result. Like
#' [export_stata_smcl_html()], this has no figures to embed: Mplus never
#' writes chart data into the `.out` text file itself (see the file header).
#'
#' @param path path to a `.out` file
#' @param out path to write the HTML file to; defaults to `path` with its
#'   extension replaced by `.html`, written alongside the source file
#'
#' @returns the path written to, invisibly
#' @export
export_mplus_html <- function(path, out = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.out$", path, ignore.case = TRUE))
    stop("Not a .out file: ", path)
  if (is.null(out)) out <- sub("\\.out$", ".html", path, ignore.case = TRUE)

  tables <- import_mplus_output(path)

  body <- if (!length(tables)) {
    "<p>No result tables could be recovered from this .out file.</p>"
  } else {
    sections <- vector("list", length(tables))
    last_section <- NA_character_
    for (i in seq_along(tables)) {
      tb <- tables[[i]]
      heading <- ""
      if (!identical(tb$analysis, last_section)) {
        heading <- sprintf("<h3>%s</h3>", .spv_html_escape(tb$analysis))
        last_section <- tb$analysis
      }
      title <- if (!identical(tb$title, tb$analysis) && !is.na(tb$title %||% NA))
        sprintf("<h4>%s</h4>", .spv_html_escape(tb$title)) else ""
      sections[[i]] <- paste0(heading, title, .spv_table_html(tb$data))
    }
    paste(unlist(sections), collapse = "\n")
  }

  html <- sprintf(paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n",
    "<title>%s</title>\n",
    "<style>\n",
    "body { font-family: sans-serif; margin: 2em; }\n",
    "h3 { border-bottom: 1px solid #888; margin-top: 2em; }\n",
    "table { border-collapse: collapse; margin-bottom: 1.5em; }\n",
    "th, td { border: 1px solid #ccc; padding: 4px 10px; font-size: 90%%; text-align: right; }\n",
    "th { background: #f0f0f0; text-align: center; }\n",
    "td:first-child, th:first-child { text-align: left; }\n",
    "</style>\n</head>\n<body>\n<h1>%s</h1>\n%s\n</body>\n</html>\n"),
    .spv_html_escape(basename(path)), .spv_html_escape(basename(path)), body)

  writeLines(html, out, useBytes = TRUE)
  invisible(out)
}
