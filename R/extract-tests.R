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
# "cohens d"/"cohens ds"/"cohens dz" and "hedges g"/"hedges gs" (the
# spelled-out forms authors actually write in prose, e.g. "the effect was
# small, Cohen's d = 0.29") must be anchor-eligible too, not just bare "d"/
# "g": before this they sat ONLY in .TEST_SATELLITES, so a sentence reporting
# an effect size alone (no accompanying t/F) had zero anchors and was
# dropped entirely by .split_into_tests()'s "no anchor -> not a test" rule,
# even though the identical value written as bare "d = .29"/"g = .29" already
# survived. Confirmed against a real corpus paper (Cohen's d case).
# "alpha"/"cronbachs alpha": a reliability coefficient reported on its own
# ("Cronbach's α = .73") is a genuine standalone reportable statistic, not a
# satellite of some other anchor -- previously absent from BOTH lists (see
# .norm_stat_name()'s own missing α->alpha fold, fixed above), so every
# Cronbach's-alpha-only sentence in a real corpus paper produced no test at
# all, matched or not.
# "etap2"/"etap 2"/"eta2"/"omega2"/"delta"/"deltam": the same standalone-
# effect-size gap as Cohen's d/Hedges' g above (eta-squared, omega-squared,
# and Glass's delta are routinely reported as the sole statistic in a
# sentence, e.g. "the interaction was negligible, ηp² = .006") -- these are
# multi-character keys with no realistic collision with an unrelated token,
# unlike bare single-letter satellites (g, ...), which are deliberately LEFT
# satellite-only: promoting a bare letter to anchor status risks fabricating
# a "test" out of an unrelated variable name or figure label that happens to
# normalise to that same single character, the same ambiguity documented in
# stato-map.R for a bare "W" (Shapiro-Wilk vs. Wilcoxon).
# "m"/"mean": PREVIOUSLY reasoned to be satellite-only on the theory that "a
# bare M = 3.28 ... describes a sample, it does not report a test" -- that
# reasoning was WRONG, confirmed against a real corpus paper's own sentence:
# "M = 3.28, 95% CI = [3.18, 3.38], ..., Cronbach's α = .86" is not one test,
# it is TWO adjacent, unrelated claims (the scale's own mean+CI, and its
# separately-computed reliability) that .split_into_tests() fused into one
# because it can only detect a NEW unit starting via a REPEATED anchor/CI/
# estimate token, and M was never anchor-eligible, so nothing signalled that
# the mean+CI unit had already closed before alpha began. A mean with its own
# CI is exactly as complete and independently reportable a claim as
# "d = 0.29" alone; promoting "m"/"mean" to anchor status lets the EXISTING
# repeat-detection machinery draw this same boundary for free, the moment any
# OTHER anchor (alpha included) follows it, with no anchor-specific special
# case needed. Collision risk is the same low bar "m"'s multi-letter
# spelled-out form ("mean") already clears; the bare single-letter "m" is
# admitted here (unlike bare "g") because M is APA's own fixed, universal
# notation for a sample mean with no other common meaning in a results
# sentence, unlike "g" (Hedges' g, or a units label, or a subscript) or "w"
# (Shapiro-Wilk vs Wilcoxon, already documented as ambiguous above).
.TEST_ANCHORS <- c("t", "f", "z", "chi2", "chisq", "x2", "r", "rho", "tau",
                   "u", "w", "h", "q", "beta", "b", "rr", "hr", "d",
                   "cohens d", "cohens ds", "cohens dz",
                   "hedges g", "hedges gs",
                   "eta2", "etap2", "etap 2", "omega2", "delta", "deltam",
                   "bf10", "bf01", "bf", "alpha", "cronbachs alpha",
                   "m", "mean")

# Statistic names that ACCOMPANY an anchor within one reported test.
.TEST_SATELLITES <- c("p", "df", "se", "sd", "m", "mean", "md", "ci",
                      "lower", "upper", "n", "eta2", "etap2", "etap 2",
                      "omega2", "cohens d", "cohens ds", "cohens dz",
                      "hedges g", "hedges gs", "g", "delta", "deltam",
                      "bf10", "bf01", "bf", "d", "r", "ci95", "95 ci",
                      "alpha", "cronbachs alpha")

# Names that ARE statistics in general but are not evidence for a reported
# test, so they must not keep a run alive on their own. `range` and `mode` are
# real STATO classes (so stato_type_column recognises them) yet in a results
# sentence they describe a sample, not a test; `or` is far more often the
# English word than an odds ratio, and admitting it turned "< 80 ms or > 550
# ms" into a reported test.
.TEST_NONEVIDENCE <- c("range", "mode", "or", "and", "min", "max", "sum",
                       "count", "total", "height", "width", "age")

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

# A PRIMARY test statistic — anchor-eligible but NEVER also listed as a
# satellite (t, F, z, chi2, U, W, H, Q, a Bayes factor, ...): these always
# represent a hypothesis test in their own right, so a repeat of one of
# these is always a genuinely new test. Everything else in .TEST_ANCHORS
# (r, beta, d, eta2, alpha, m, ...) is DUAL-ROLE: also listed in
# .TEST_SATELLITES, because a sentence reporting one of them with no
# primary statistic at all ("Cohen's d = .29" alone) still needs to count
# as its own test — but that same name, arriving AFTER a primary anchor has
# already opened the current test, is that test's own effect size /
# descriptive, not a new claim (an F-test's own ηp², a t-test's own Cohen's
# d). .split_into_tests() uses this to decide which repeats really start a
# new test.
.is_primary_anchor <- function(nm) {
  key <- .norm_stat_name(nm)
  .is_anchor(nm) && !(key %in% .TEST_SATELLITES)
}

# Split one sentence's components into SEPARATE tests.
#
# The rule is positional and mirrors how results are written: a test is built
# around an anchor (t, F, r, ...), and a NEW anchor starts a new test —
# whether it REPEATS the previous one ("t = 3.77, p = .001, d = 0.77, t =
# 2.69, p = .013, d = 0.55" is two tests, the second `t` is where the first
# ends) or is a DIFFERENT anchor entirely ("M = 3.28, 95% CI = [3.18, 3.38],
# ..., Cronbach's α = .86" is ALSO two tests — the scale's own mean+CI, and
# its separately-computed reliability — not one test with a stray CI glued to
# an unrelated alpha; confirmed against a real corpus paper). Satellites
# attach to the anchor they follow.
#
# This means a satellite genuinely LEADING a later anchor of a DIFFERENT kind
# ("M = 1.93, SD = 0.76, W = 183.5, p = .791" -- a mean/SD reported ahead of
# the W-test they describe) is now also split apart the moment the satellite
# is itself anchor-eligible (M is -- see .TEST_ANCHORS's own comment on
# promoting "m"/"mean"): M becomes its own 1-component test, separated from
# the W-test. Accepted deliberately, not a bug: correctly keeping shared
# leading satellites glued to the RIGHT anchor while still splitting genuinely
# unrelated adjacent claims (M+CI vs. alpha) would need knowing WHICH later
# anchor a leading satellite belongs to, which nothing in a flat list of
# components can tell on its own. Splitting too far is the safe failure mode
# here: each piece still independently matches (match_reported_output()'s
# min_components = 1), so fewer components per test costs confidence, not
# correctness -- whereas the ORIGINAL bug (fusing two unrelated claims into
# one un-matchable blob) was a false claim of relatedness with no recovery.
#
# A repeated "95% CI" ALSO starts a new test, the same way a repeated anchor
# does: one test reports at most one confidence interval for its own primary
# effect, so a second CI appearing before the next anchor means the sentence
# is already describing a different quantity. Confirmed against a real corpus
# sentence — "F(1,494) = 323.77, p < .001, 95% CI = [.22,.29], Cohen's d =
# .10, estimate = .40, 95% CI = [.35,.44], F(1,573) = 375.81, p < .001" — where
# the FIRST F/CI/d describe the aesthetic-condition test and "estimate = .40,
# 95% CI = [.35,.44]" belongs with the SECOND F, not the first. The split point
# must land BEFORE "estimate", not at the second CI itself, or the estimate and
# its own CI end up split apart from each other (the estimate stranded in test
# 1, its CI moved alone into test 2) instead of staying together as one unit in
# test 2 — so "estimate" is ALSO tracked as a split-triggering name here (it
# behaves like a second anchor: it is itself the primary point-value for the
# new sub-claim, the CI immediately after it is what actually repeats).
#
# Scoped to CI + estimate specifically — not every satellite — because a
# repeated non-CI, non-estimate satellite (e.g. a second "p" with no repeated
# CI) is common and legitimate WITHIN one test elsewhere in this same corpus;
# generalising the repeat rule to every satellite would over-split those.
.split_into_tests <- function(comps) {
  if (!length(comps)) return(list())
  is_anch <- vapply(comps, function(c) .is_anchor(c$name), logical(1))
  if (!any(is_anch)) return(list())          # no anchor -> not a test
  is_primary <- vapply(comps, function(c) .is_primary_anchor(c$name), logical(1))

  nms <- vapply(comps, function(c) .norm_stat_name(c$name), character(1))
  is_ci  <- nms %in% c("ci", "ci95", "95 ci")
  is_est <- nms == "estimate"

  # A new test begins at:
  #  (a) a REPEAT of a name already seen in the current test — same anchor
  #      twice ("t = ..., t = ..."), or the same dual-role name twice ("d =
  #      ..., d = ..."); or
  #  (a2) a DUAL-ROLE anchor (r, beta, d, eta2, alpha, m, ... — anchor-
  #      eligible but ALSO a satellite; see .is_primary_anchor()'s own
  #      comment) arriving while the current test has NO primary anchor of
  #      its own yet ("M = ..., alpha = ..." — two cold, unrelated claims,
  #      neither one a supporting statistic for the other). Once a PRIMARY
  #      anchor (t, F, z, chi2, U, W, H, Q, a Bayes factor, ...) has already
  #      opened the test, a dual-role name is that test's own effect size /
  #      descriptive (an F-test's ηp², a t-test's Cohen's d) and stays
  #      attached rather than starting a new test — this is the fix: before
  #      it, "F(2,560) = 41.86, p < .001, ηp² = .13, 95% CI = [.08, .18]"
  #      wrongly split into an F-test and a stranded ηp²/CI pair the moment
  #      ηp² (itself anchor-eligible) followed the F, exactly the same way
  #      "t = 3.77, p = .001, Cohen's d = 0.77" split its OWN d away too —
  #      confirmed against a real corpus paper's F-test, whose ηp²/CI then
  #      matched (wrongly, on pure numeric coincidence) against an unrelated
  #      p-value from a different test in a different source file entirely.
  #      A PRIMARY anchor itself is NEVER exempted by this — a repeated t/F/
  #      etc. always starts a new test even with no earlier primary seen,
  #      since two primary statistics next to each other are never one
  #      test's own supporting statistic for the other; or
  #  (b) a repeated CI — one test reports at most one confidence interval for
  #      its own primary effect, so a second CI means a different quantity is
  #      already being described; or
  #  (c) an "estimate" that arrives AFTER the current test already has its own
  #      CI — normally a test's point value comes BEFORE its CI, so an
  #      "estimate" showing up post-CI is not this test's estimate, it is the
  #      lead-in for the NEXT test's own value (and that value's own,
  #      also-repeated CI must travel WITH it, not be split away from it,
  #      hence the split lands at (c) rather than at the repeated CI in (b)).
  # (b) and (c) are BOTH needed, in that order of precedence, on the same real
  # sentence — "F = 323.77, p < .001, 95% CI = [.22,.29], d = .10, estimate =
  # .40, 95% CI = [.35,.44], F = 375.81, p < .001": splitting only at the
  # second CI (b) strands "estimate" in test 1, apart from its own CI;
  # checking for (c) FIRST catches the split one component earlier, at
  # "estimate" itself, keeping it with its CI in test 2.
  starts <- integer(0)
  seen <- character(0); ci_seen_this_test <- FALSE
  anchor_seen_this_test <- FALSE; primary_seen_this_test <- FALSE
  group_start <- 1L   # index of the current group's own first component
  for (i in seq_along(comps)) {
    if (is_est[i] && ci_seen_this_test) {
      starts <- c(starts, i); seen <- character(0); ci_seen_this_test <- FALSE
      anchor_seen_this_test <- FALSE; primary_seen_this_test <- FALSE
      group_start <- i
      next
    }
    # (a) exact repeat, or (a2) a dual-role anchor with no primary anchor
    # open yet — a PRIMARY anchor ALWAYS splits once the current group
    # already holds at least one earlier component (i > group_start),
    # since it unambiguously starts its own new test regardless of what
    # preceded it — even a not-yet-anchored leading run like "estimate,
    # 95% CI" (test 2 of the F/estimate example above) is still a separate
    # claim from a THIRD F arriving after it, so this does not require
    # anchor_seen_this_test the way the dual-role branch does. A dual-role
    # anchor only splits when nothing primary has opened this test, i.e. it
    # would otherwise silently attach as if it were the open primary
    # anchor's own effect size.
    splits_here <- is_anch[i] && (
      (nms[i] %in% seen) ||
      (is_primary[i] && i > group_start) ||
      (!is_primary[i] && anchor_seen_this_test && !primary_seen_this_test)
    )
    if (splits_here) {
      starts <- c(starts, i); seen <- nms[i]
      ci_seen_this_test <- is_ci[i]; anchor_seen_this_test <- TRUE
      primary_seen_this_test <- is_primary[i]
      group_start <- i
      next
    }
    if (is_anch[i]) {
      anchor_seen_this_test <- TRUE
      if (is_primary[i]) primary_seen_this_test <- TRUE
    }
    if (!is_anch[i] && !is_ci[i]) next
    if (nms[i] %in% seen) {
      starts <- c(starts, i); seen <- nms[i]
      ci_seen_this_test <- is_ci[i]
      group_start <- i
    } else {
      seen <- c(seen, nms[i])
      if (is_ci[i]) ci_seen_this_test <- TRUE
    }
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
    # `sentence_pos` is this component's index in the WHOLE sentence's
    # original reporting order, tagged BEFORE .split_into_tests() cuts
    # `comps` apart — match-reported.R's .regroup_by_evidence() needs this
    # to survive the split (it re-pools components from the same sentence
    # and, for its text_proximity variant, compares how close two
    # components originally sat), so it must be set here, before any
    # split, not re-derived from a post-split index (which would reset to
    # 1, 2, 3... for every split piece and make cross-piece distance
    # meaningless).
    comps <- lapply(seq_len(nrow(sub)), function(i) list(
      name  = sub$lhs[i],
      comp  = sub$comp[i],
      value = sub$rhs[i],
      df    = sub$df[i],
      sentence_pos = i))
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
