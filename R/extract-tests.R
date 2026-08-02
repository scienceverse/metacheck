# Extract the statistical TESTS a paper reports, as matchable units.
#
# This exists because `extract_eq()` (R/text-extractors.R) is a general
# equation scraper, not a test extractor, and three of its properties make it a
# poor matching source:
#
#   1. `grp_id` IS the sentence number. Its own comment says "set group equal to
#      sentence for now" — so a sentence reporting two t-tests yields ONE group
#      of ten components, which no single analysis can ever produce. On the
#      Psych Science corpus ~30% of groups carry more than six components.
#   2. The sentence text is dropped. Only lhs/comp/rhs survive, so a match
#      cannot be shown in context, and nothing ties a matched result back to the
#      claim it supports.
#   3. Anything of the form "<word> <op> <number>" is captured, so figure
#      dimensions (`height = 4`, `width = 3`), sample descriptions
#      (`range = 18-29`) and stray tokens arrive as "statistics".
#
# What matching needs instead is: one row per TEST, its components kept
# together, the sentence it was reported in (so a result can be traced to the
# claim), and enough typing to compare like with like. That is what this builds.
#
# The extraction is deliberately CONSERVATIVE about what counts as a test: a
# statistic name must be recognised (via the same STATO/metacheck vocabulary the
# output side uses), so a figure dimension is not silently promoted to evidence.

# Statistic names that ANCHOR a test — the ones a report is built around. A run
# of components containing none of these is descriptive text, not a test.
# NOTE these are compared AFTER .norm_stat_name(), so they must be written in
# its output form: no apostrophes, Greek folded to ASCII ("cohens d", not
# "Cohen's d" — a mismatch there silently drops every effect size).
.TEST_ANCHORS <- c("t", "f", "z", "chi2", "chisq", "x2", "r", "rho", "tau",
                   "u", "w", "h", "q", "beta", "b", "rr", "hr", "d",
                   "bf10", "bf01", "bf")

# Statistic names that ACCOMPANY an anchor within one reported test.
.TEST_SATELLITES <- c("p", "df", "se", "sd", "m", "mean", "md", "ci",
                      "lower", "upper", "n", "eta2", "etap2", "etap 2",
                      "omega2", "cohens d", "cohens ds", "cohens dz",
                      "hedges g", "g", "delta", "deltam", "bf10", "bf01",
                      "bf", "d", "r", "ci95", "95 ci")

# Names that ARE statistics in general but are not evidence for a reported
# test, so they must not keep a run alive on their own. `range` and `mode` are
# real STATO classes (so stato_type_column recognises them) yet in a results
# sentence they describe a sample, not a test; `or` is far more often the
# English word than an odds ratio, and admitting it turned "< 80 ms or > 550
# ms" into a reported test.
.TEST_NONEVIDENCE <- c("range", "mode", "or", "and", "min", "max", "sum",
                       "count", "total", "height", "width", "age")

# Normalise a reported statistic name to a comparison key: lower-cased, Greek
# and superscripts folded to ASCII, punctuation dropped. "ηp²" -> "etap2",
# "Cohen's d" -> "cohens d", "χ²" -> "chi2".
.norm_stat_name <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")))
  s <- gsub("η", "eta", s)      # eta
  s <- gsub("χ", "chi", s)      # chi
  s <- gsub("β", "beta", s)     # beta
  s <- gsub("ρ", "rho", s)      # rho
  s <- gsub("τ", "tau", s)      # tau
  s <- gsub("Δ", "delta", s)    # Delta
  s <- gsub("δ", "delta", s)    # delta
  s <- gsub("²", "2", s)        # superscript 2
  s <- gsub("’|‘|'", "", s)
  s <- gsub("[^a-z0-9 ]+", "", s)
  trimws(gsub("\\s+", " ", s))
}

# Is this name a statistic we recognise? Uses the SAME vocabulary as the output
# side (R/stato-map.R), so the two halves of a match agree on what a statistic
# is, plus the anchor/satellite lists for reporting conventions the output-side
# machine names do not cover ("cohen's d" as written in prose).
.is_stat_name <- function(nm) {
  key <- .norm_stat_name(nm)
  if (!nzchar(key)) return(FALSE)
  if (key %in% .TEST_NONEVIDENCE) return(FALSE)
  if (key %in% .TEST_ANCHORS || key %in% .TEST_SATELLITES) return(TRUE)
  nzchar(stato_type_column(key)$termSource)
}

.is_anchor <- function(nm) {
  key <- .norm_stat_name(nm)
  !(key %in% .TEST_NONEVIDENCE) && key %in% .TEST_ANCHORS
}

# Split one sentence's components into SEPARATE tests.
#
# The rule is positional and mirrors how results are written: a test is built
# around an anchor (t, F, r, ...), and a NEW anchor of the same kind starts a
# new test. "t = 3.77, p = .001, d = 0.77, t = 2.69, p = .013, d = 0.55" is two
# tests, and the second `t` is where the first ends. Satellites attach to the
# anchor they follow; leading satellites (a mean and SD reported before the
# test) attach to the first anchor.
.split_into_tests <- function(comps) {
  if (!length(comps)) return(list())
  is_anch <- vapply(comps, function(c) .is_anchor(c$name), logical(1))
  if (!any(is_anch)) return(list())          # no anchor -> not a test

  # A new test begins at an anchor whose normalised name has already been used
  # since the current test started.
  starts <- integer(0)
  seen <- character(0)
  for (i in seq_along(comps)) {
    if (!is_anch[i]) next
    nm <- .norm_stat_name(comps[[i]]$name)
    if (nm %in% seen) { starts <- c(starts, i); seen <- nm } else seen <- c(seen, nm)
  }
  starts <- sort(unique(c(1L, starts)))
  ends <- c(starts[-1] - 1L, length(comps))
  out <- lapply(seq_along(starts), function(k) comps[starts[k]:ends[k]])
  Filter(function(g) any(vapply(g, function(c) .is_anchor(c$name), logical(1))), out)
}

#' Extract the statistical tests a paper reports
#'
#' Builds the paper-side counterpart to the analysis output that
#' `reproducibility_check` extracts: one row per reported TEST, with its
#' components kept together and the sentence it was reported in retained, so a
#' matched result can be traced back to the claim it supports.
#'
#' This is a purpose-built alternative to [extract_eq()] for matching. It reuses
#' `extract_eq()`'s parsing of `name <op> value` fragments, then does three
#' things that parsing does not:
#'
#' * **isolates separate tests** within one sentence. `extract_eq()`'s `grp_id`
#'   is the sentence number, so a sentence reporting two t-tests yields a single
#'   ten-component "test" that no analysis can produce; here a repeated anchor
#'   (a second `t`, `F`, `r`, ...) starts a new test;
#' * **keeps the sentence**, its `text_id`, `paragraph_id` and `section_id`, so a
#'   match links a `result_id` to the sentence making the claim;
#' * **filters non-statistics**, so a figure's `height = 4` or a sample's
#'   `range = 18-29` is not offered as evidence. A run of components with no
#'   recognised test statistic in it is dropped entirely.
#'
#' @param paper a paper object (uses its `$eq` and `$text`)
#'
#' @returns a data.frame, one row per reported test, with `paper_id`,
#'   `test_no` (sequential within the paper), `text_id`, `paragraph_id`,
#'   `section_id`, `sentence` (the reporting sentence), `anchor` (the test
#'   statistic the report is built around, e.g. `"t"`), `n_components`,
#'   `components` (a list-column of `name`/`comp`/`value`/`df`), and `reported`
#'   (the test rendered back as text, e.g. `"t(23) = 3.77, p = .001, d = 0.77"`).
#' @export
extract_tests <- function(paper) {
  eq <- paper$eq
  if (is.null(eq) || !nrow(eq)) eq <- tryCatch(extract_eq(paper),
                                               error = function(e) NULL)
  if (is.null(eq) || !nrow(eq)) return(.empty_tests())

  txt <- paper$text
  pid <- tryCatch(paper_id(paper), error = function(e) NA_character_)

  rows <- list(); test_no <- 0L
  for (tid in unique(eq$text_id)) {
    sub <- eq[eq$text_id == tid, , drop = FALSE]
    comps <- lapply(seq_len(nrow(sub)), function(i) list(
      name  = sub$lhs[i],
      comp  = sub$comp[i],
      value = sub$rhs[i],
      df    = sub$df[i]))
    # Keep only components whose NAME is a statistic we recognise; a figure
    # dimension or a stray token is not evidence for a reported test.
    comps <- Filter(function(c) .is_stat_name(c$name), comps)
    for (g in .split_into_tests(comps)) {
      test_no <- test_no + 1L
      anchor <- Find(function(c) .is_anchor(c$name), g)
      sent <- if (!is.null(txt) && "text_id" %in% names(txt)) {
        s <- txt$text[txt$text_id == tid]
        if (length(s)) as.character(s[1]) else NA_character_
      } else NA_character_
      meta <- function(col) if (!is.null(txt) && col %in% names(txt)) {
        v <- txt[[col]][txt$text_id == tid]
        if (length(v)) v[1] else NA
      } else NA
      rows[[length(rows) + 1L]] <- data.frame(
        paper_id = pid, test_no = test_no, text_id = tid,
        paragraph_id = meta("paragraph_id"), section_id = meta("section_id"),
        sentence = sent,
        anchor = .norm_stat_name(anchor$name %||% ""),
        n_components = length(g),
        reported = .render_test(g),
        stringsAsFactors = FALSE)
      rows[[length(rows)]]$components <- I(list(g))
    }
  }
  if (!length(rows)) return(.empty_tests())
  do.call(rbind, rows)
}

.empty_tests <- function() {
  d <- data.frame(paper_id = character(0), test_no = integer(0),
                  text_id = integer(0), paragraph_id = integer(0),
                  section_id = integer(0), sentence = character(0),
                  anchor = character(0), n_components = integer(0),
                  reported = character(0), stringsAsFactors = FALSE)
  d$components <- I(list())
  d
}

# Render one test back as it would read in a paper: "t(23) = 3.77, p = .001".
.render_test <- function(g) {
  paste(vapply(g, function(c) {
    dfp <- if (!is.na(c$df %||% NA) && nzchar(as.character(c$df)))
      as.character(c$df) else ""
    sprintf("%s%s %s %s", c$name, dfp, c$comp %||% "=", c$value)
  }, character(1)), collapse = ", ")
}
