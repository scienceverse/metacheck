# Match full statistical TESTS reported in a paper's text against the statistics
# EXTRACTED from the paper's analysis output (reproducibility_check's
# stat_results_long(), from JASP/jamovi files or run-R-code console output).
#
# A reported test is NOT a single number — it is a multi-component statement, e.g.
# "M = 1.93, SD = 0.76, W = 183.5, p = .791, rb = -0.16". extract_eq() shatters
# that into one row per number, but tags each with a `grp_id` that groups the
# numbers from the SAME reported test. We RECOMPOSE those groups back into whole
# tests, then check whether each test's component values CO-OCCUR in a single
# output analysis. Matching whole tests (not lone numbers) removes the coincidence
# problem: several components agreeing at once is essentially never accidental.
#
# We deliberately use only extract_eq (not statcheck): a recomposed eq group is a
# superset of what statcheck finds (statcheck catches t/F/chi2/Z/r/Q; eq groups
# also carry Wilcoxon W, descriptives, effect sizes, Bayes factors), so adding
# statcheck would only duplicate rows.
#
# Matching is PRECISION-AWARE: a reported value matches an output value when the
# output value, rounded to the reported number of decimals, equals it (reported
# "W = 370.5" matches an output 370.5; "d = .68" matches 0.6810).

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
# as one rhs string, deliberately — see R/text-extractors.R). .norm_value() has
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
    else strsplit(inner, "(?<=[0-9])\\s*[-–—]\\s*(?=[.0-9])",
                  perl = TRUE)[[1]]
  parts <- trimws(parts)
  if (length(parts) != 2 || any(!nzchar(parts))) return(NULL)
  lo <- .norm_value(parts[1]); hi <- .norm_value(parts[2])
  if (is.na(lo$num) || is.na(hi$num)) return(NULL)
  list(lo = lo, hi = hi)
}

# Coarse statistic family for a name, both sides. NA when unrecognised — used to
# (a) drop junk reported rows whose "name" is prose/a variable, and (b) prefer a
# type-consistent output cell when matching.
.stat_family <- function(name) {
  n <- tolower(trimws(name %||% ""))
  n <- gsub("[[:space:]]+", " ", n)
  dplyr::case_when(
    grepl("^t($| |-|value|stat)", n) | n == "student's t" |
      grepl("t-?test|student'?s t", n)                                ~ "t",
    grepl("^f($| |-|value| change)", n) | grepl("^pr\\(>f", n) |
      grepl("\\banova\\b|f-?test", n)                                 ~ "F",
    grepl("chi|χ|x-squared|x²|goodness of fit", n)                    ~ "chisq",
    grepl("^z($| |value)", n)                                         ~ "z",
    grepl("^w$|wilcoxon|mann-whitney", n)                             ~ "W",
    grepl("^u$", n)                                                   ~ "U",
    grepl("^q$", n)                                                   ~ "Q",
    grepl("^p$|p-value|p value|pr\\(>", n)                            ~ "p",
    grepl("cohen'?s d|^d$|^ds$|effect size", n)                       ~ "d",
    grepl("hedge", n)                                                 ~ "g",
    grepl("mean difference|^md$", n)                                  ~ "meandiff",
    grepl("^median|^med$", n)                                         ~ "median",
    # "(^|[^a-z])eta": an unanchored "eta" wrongly matched INSIDE "beta" (b-ETA)
    # and "meta", so every reported beta was mistyped as eta-squared and could
    # never match a real beta cell. Requiring the character before "eta" to be
    # either the string start or a non-letter excludes both without needing a
    # regex word-boundary (which "beta" would still satisfy, since b is itself
    # a word character adjacent to "eta" with no boundary between them).
    grepl("η|ηp²|(^|[^a-z])eta", n)                                   ~ "eta2",
    grepl("ω|omega", n)                                               ~ "omega2",
    grepl("odds", n)                                                  ~ "or",
    grepl("^rb$|rank.?biserial|biserial", n)                          ~ "rb",
    grepl("bf|bayes", n)                                              ~ "bf",
    grepl("^r$|correlation|pearson|^rs$", n)                          ~ "r",
    grepl("^rho|^ρ|spearman", n)                                      ~ "rho",
    # "stdcoef" is the common column name a hand-written standardisation helper
    # gives a standardised coefficient (e.g. the widely copied stdCoef.merMod()
    # snippet for lme4 models, which has no built-in standardised-beta output);
    # it names the same quantity as "beta"/"standardized" without either word
    # in it, so it needs its own case rather than relying on the substring
    # checks below to catch it.
    grepl("^β|beta|standardi|^std\\.?coef$", n)                       ~ "beta",
    grepl("^b$|estimate|unstandardi", n)                              ~ "b",
    grepl("^se$|std\\. error|standard error", n)                     ~ "se",
    grepl("^m$|^mean$", n)                                            ~ "mean",
    grepl("^sd$|std\\. dev", n)                                       ~ "sd",
    grepl("^n$|^ns$|sample size", n)                                  ~ "n",
    grepl("^α|alpha|cronbach", n)                                     ~ "alpha",
    # R-squared / marginal-conditional R2 (MuMIn::r.squaredGLMM() prints
    # "R2m"/"R2c"; psychometric::CI.Rsq() prints "Rsq" for the point estimate).
    grepl("^r2m?c?$|^rsq$|r-?squared|r sq(uared)?$", n)                ~ "rsq",
    # CI bounds: the reporting side writes "95% CI"/"lower"/"upper"/"interval";
    # the output side (psychometric::CI.Rsq() and similar) prints the bound
    # names "LCL"/"UCL" bare, with no "lower"/"upper" substring, so those are
    # matched explicitly. ci_lower/ci_upper are .norm_interval()'s own component
    # names for a reported bracketed CI ("[.16, .29]") split into two bounds.
    grepl("^lcl$|^ci_?lower$", n)                                     ~ "ci_lower",
    grepl("^ucl$|^ci_?upper$", n)                                     ~ "ci_upper",
    grepl("ci|lower|upper|interval", n)                               ~ "ci",
    TRUE ~ NA_character_
  )
}

# Recompose a paper's eq table into whole tests: one row per (text_id, grp_id),
# with its components as a list of (family, name, value_num, dec). Drops
# components whose name is not a recognised statistic (prose words, variable
# names) and whose value is not a clean number, so a group of pure junk vanishes.
.recompose_eq <- function(eq) {
  if (is.null(eq) || nrow(eq) == 0) return(list())
  eq$.tid <- eq$text_id %||% seq_len(nrow(eq))
  eq$.gid <- eq$grp_id %||% seq_len(nrow(eq))
  key <- paste(eq$.tid, eq$.gid, sep = "|")
  split(eq, key) |> lapply(function(g) {
    comps <- lapply(seq_len(nrow(g)), function(i) {
      fam <- .stat_family(g$lhs[i])
      if (is.na(fam)) return(NULL)   # drop junk components
      # The COMPARATOR lives in eq's own `comp` column, not in the value text:
      # extract_eq() splits "p < .001" into lhs="p", comp="<", rhs=".001", so
      # reading only rhs loses the censoring and turns every "p < .001" into
      # "p = .001". That silently fails to match a real p of 1.7e-05, which IS
      # < .001 - the commonest way a significant p is reported.
      cc <- if (!is.null(g$comp)) trimws(as.character(g$comp[i])) else NA_character_
      # A CI is reported as one bracketed value ("[.16, .29]") — extract_eq()
      # keeps it as one rhs string by design (see R/text-extractors.R) — but the
      # output side stores its two bounds as separate rows (LCL/UCL), so it is
      # split here into two independently matchable components rather than one
      # unparsed value that .norm_value() would drop as NA.
      ivl <- .norm_interval(g$rhs[i])
      if (!is.null(ivl)) {
        # family is the SPECIFIC bound family ("ci_lower"/"ci_upper"), not the
        # generic "ci" .stat_family() gave the raw label ("95% CI") — the
        # output side's LCL/UCL are typed to the specific families (see
        # .stat_family() above), and val_in()'s family-preference match would
        # never find them under the generic "ci".
        return(list(
          list(family = "ci_lower", name = "ci_lower", value = ivl$lo$num,
               dec = ivl$lo$dec, censored = ""),
          list(family = "ci_upper", name = "ci_upper", value = ivl$hi$num,
               dec = ivl$hi$dec, censored = "")))
      }
      nv <- .norm_value(g$rhs[i])
      if (is.na(nv$num)) return(NULL)
      cens <- nv$censored
      if (!nzchar(cens) && !is.na(cc) && cc %in% c("<", ">")) cens <- cc
      list(list(family = fam, name = g$lhs[i], value = nv$num,
                dec = nv$dec, censored = cens))
    })
    comps <- unlist(Filter(Negate(is.null), comps), recursive = FALSE)
    if (!length(comps)) return(NULL)
    list(text_id = g$.tid[1], grp_id = g$.gid[1], components = comps)
  }) |> (\(x) Filter(Negate(is.null), x))()
}

# Turn an extract_tests() table into the internal test shape. The components are
# already grouped per test there, so this only parses each value and keeps the
# provenance (text_id / sentence) that lets a match name the claim it supports.
.tests_from_extract <- function(tt) {
  if (is.null(tt) || !nrow(tt)) return(list())
  out <- lapply(seq_len(nrow(tt)), function(i) {
    g <- tt$components[[i]]
    comps <- lapply(g, function(c) {
      fam <- .stat_family(c$name)
      if (is.na(fam)) return(NULL)
      # A CI reported as one bracketed value ("[.16, .29]") is split into two
      # independently matchable components (its lower and upper bound) — see
      # .recompose_eq()'s identical handling above for why.
      ivl <- .norm_interval(c$value)
      if (!is.null(ivl)) {
        # family is the SPECIFIC bound family, not the generic "ci" — see
        # .recompose_eq()'s identical handling above for why.
        return(list(
          list(family = "ci_lower", name = "ci_lower", value = ivl$lo$num,
               dec = ivl$lo$dec, censored = ""),
          list(family = "ci_upper", name = "ci_upper", value = ivl$hi$num,
               dec = ivl$hi$dec, censored = "")))
      }
      nv <- .norm_value(c$value)
      if (is.na(nv$num)) return(NULL)
      cens <- nv$censored
      cc <- trimws(as.character(c$comp %||% ""))
      if (!nzchar(cens) && cc %in% c("<", ">")) cens <- cc
      list(list(family = fam, name = c$name, value = nv$num,
                dec = nv$dec, censored = cens))
    })
    comps <- unlist(Filter(Negate(is.null), comps), recursive = FALSE)
    if (!length(comps)) return(NULL)
    list(text_id = tt$text_id[i], grp_id = tt$test_no[i],
         sentence = tt$sentence[i], components = comps)
  })
  Filter(Negate(is.null), out)
}

#' Match reported statistical tests against the extracted analysis output
#'
#' Recomposes the statistics a paper REPORTS (from `paper$eq`, grouped by the
#' `grp_id` that ties one reported test's numbers together) into whole tests, then
#' checks whether each test's component values co-occur in a single analysis of
#' the paper's extracted OUTPUT (`reproducibility_check`'s statistical output). A
#' test is matched on its whole signature, not lone numbers, so a match means the
#' reported result actually appears in the reproducible output rather than a value
#' coinciding by chance. The result carries, per matched test, which output file
#' and analysis produced it (provenance).
#'
#' @param paper a paper object (its `$eq` is recomposed), or an eq data frame
#' @param output the extracted output: a [stat_results_long()] data frame, or the
#'   `stat_output` list a `reproducibility_check` result carries (its per-file
#'   `$long` tables are combined, keeping each file's provenance)
#' @param min_components a test must have at least this many recognised components
#'   to be assessed (default 2, so a lone extracted number — a bare `p = .05` or
#'   `N = 218` with no accompanying statistic — is not treated as a matchable
#'   "test"; a single value cannot be matched as a test and only reintroduces
#'   coincidental matches). Set 1 to include single-component rows.
#'
#' @returns a data.frame, one row per recomposed reported test, with:
#'   `text_id`, `grp_id`, `reported` (the recomposed test as text, e.g.
#'   "W=183.5 p=.791 rb=-0.16"), `n_components`, `n_matched` (components found in
#'   the best-matching output analysis), `found` (logical), `match_values` (the
#'   matched components as "name=value" pairs), `not_matched` (the UNMATCHED
#'   components of that same test, same "name=value" form — always populated
#'   alongside `match_values` for a partial match, so exactly which claim failed
#'   to reproduce is visible, not just a count), `source_file` / `analysis`
#'   (provenance of the match), and `confidence` ("full" all components matched
#'   / "partial" / "none"). Attribute `"summary"` holds the roll-up.
#' @export
match_reported_output <- function(paper, output, min_components = 2L) {
  # THREE accepted paper-side inputs, in order of preference:
  #   * an extract_tests() table  - already one row per TEST, with the sentence
  #     kept, so no recomposition is needed and the tests are correctly split;
  #   * a paper object            - extract_tests() is run on it, falling back
  #     to its $eq when that yields nothing;
  #   * a raw eq data frame       - recomposed by sentence, the legacy path.
  # extract_tests() is preferred because eq's grp_id is the SENTENCE number: a
  # sentence reporting two t-tests becomes one un-matchable ten-component
  # "test". See R/extract-tests.R.
  tests <- NULL
  if (is.data.frame(paper) && all(c("test_no", "components") %in% names(paper))) {
    tests <- .tests_from_extract(paper)
  } else if (inherits(paper, "scivrs_paper")) {
    tt <- tryCatch(extract_tests(paper), error = function(e) NULL)
    if (!is.null(tt) && nrow(tt)) tests <- .tests_from_extract(tt)
    else tests <- .recompose_eq(paper$eq)
  }
  if (is.null(tests)) tests <- .recompose_eq(if (inherits(paper, "scivrs_paper"))
    paper$eq else paper)

  # Output long table, keeping source_file + analysis provenance.
  out_long <- if (is.data.frame(output)) output else {
    parts <- lapply(output, function(s) s$long)
    parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
    if (length(parts)) dplyr::bind_rows(parts) else data.frame()
  }
  empty <- data.frame(text_id = integer(0), grp_id = integer(0),
                      reported = character(0), n_components = integer(0),
                      n_matched = integer(0), found = logical(0),
                      match_values = character(0), not_matched = character(0),
                      source_file = character(0),
                      analysis = character(0), confidence = character(0))
  if (!length(tests) || is.null(out_long) || !nrow(out_long)) {
    attr(empty, "summary") <- list(n_tests = length(tests), n_found = 0L,
                                   n_full = 0L, n_partial = 0L,
                                   n_missing = length(tests), pct_found = 0)
    return(empty)
  }

  # Index the output by (source_file, analysis): each is one candidate site where
  # a whole reported test could have been produced. Pre-parse values + families.
  sf <- out_long$source_file %||% rep(NA_character_, nrow(out_long))
  an <- out_long$analysis    %||% rep("(all)", nrow(out_long))
  ovals <- vapply(out_long$value, function(x) .norm_value(x)$num, numeric(1))
  ofam  <- .stat_family(out_long$statistic)
  keep  <- !is.na(ovals)
  # A candidate SITE is one place a whole reported test could have been
  # produced. `test_id` is that unit when the output carries it:
  # stat_results_long() groups on the source format's own analysis identity (a
  # JASP analysis id, a jamovi analysisId, an R statement), so one site is one
  # analysis with its several tables already united - a regression's F-test,
  # coefficients and R2 sit in three JASP tables but one test_id. Falls back to
  # (source_file, analysis) for output produced before test_id existed.
  tid   <- out_long$test_id
  site  <- if (!is.null(tid)) as.character(tid)[keep] else
    paste(sf[keep], an[keep], sep = "")
  by_site <- split(data.frame(val = ovals[keep], fam = ofam[keep],
                              sf = sf[keep], an = an[keep],
                              stringsAsFactors = FALSE), site)

  # ADDITIONAL, BROADER sites: several SEPARATE R statements often each carry
  # only PART of a reported result's signature -- a model is fit once, then
  # summary() prints its estimate/p, a hand-written helper prints its
  # standardised beta, and r.squaredGLMM()/CI.Rsq() print its R2 and CI, three+
  # calls that share no test_id but all describe the SAME fitted model. When
  # read_r_output() recognised the object each call operated on (resolved
  # through simple assignment chains, e.g. "r2_vid" back to "m_vid" -- see
  # .r_call_object_ref()/.r_root_ref_map() in R/r-output.R), stat_results_long()
  # carries it as `model_ref`. Union every row sharing a (model_ref, source_
  # file) into ONE extra candidate site, tried ALONGSIDE (not instead of) each
  # narrower test_id site in the best-site search below -- a real per-statement
  # site still wins on its own when a test's whole signature genuinely came
  # from one call, and the broader model site only helps when it does not.
  if ("model_ref" %in% names(out_long)) {
    mref <- out_long$model_ref[keep]
    has_ref <- !is.na(mref) & nzchar(mref)
    if (any(has_ref)) {
      model_site <- paste0("\x02model\x02", mref[has_ref], "\x02", sf[keep][has_ref])
      by_model <- split(data.frame(val = ovals[keep][has_ref],
                                   fam = ofam[keep][has_ref],
                                   sf = sf[keep][has_ref],
                                   an = an[keep][has_ref],
                                   stringsAsFactors = FALSE), model_site)
      by_site <- c(by_site, by_model)
    }
  }

  val_in <- function(cell, comp) {
    tol <- 0.5 / (10^comp$dec)
    if (nzchar(comp$censored)) {
      if (comp$censored == "<") any(cell$val < comp$value) else any(cell$val > comp$value)
    } else if (!is.na(comp$family)) {
      # A component with a KNOWN type must match an output cell of that SAME
      # type — no fallback to "any value in the site" for a typed statistic.
      # The fallback used to let a reported beta or CI bound coincidentally
      # match an unrelated, untyped cell (e.g. a standardised coefficient
      # `stdcoef` sitting near the same value as an unrelated model's beta) that
      # merely happened to round to the same number. That false hit did more
      # than mislabel one component: it could make a WRONG site outscore the
      # TRUE site in the best-site tie-break below, so a test's real beta/p
      # then failed to match at the site actually chosen — a false positive
      # elsewhere silently causing a false negative on the correct evidence.
      fam_cells <- cell$val[!is.na(cell$fam) & cell$fam == comp$family]
      any(abs(round(fam_cells, comp$dec) - comp$value) < tol)
    } else {
      # No recognised type for this component (name matched no family) — the
      # only case where matching by value alone is still attempted.
      any(abs(round(cell$val, comp$dec) - comp$value) < tol)
    }
  }

  rows <- lapply(tests, function(tst) {
    comps <- tst$components; nc <- length(comps)
    reported <- paste(vapply(comps, function(c)
      sprintf("%s=%s", c$name, format(c$value, trim = TRUE)), character(1)),
      collapse = " ")
    res <- data.frame(text_id = tst$text_id, grp_id = tst$grp_id,
                      reported = reported, n_components = nc, n_matched = 0L,
                      found = FALSE, match_values = NA_character_,
                      not_matched = NA_character_,
                      source_file = NA_character_, analysis = NA_character_,
                      confidence = "none", stringsAsFactors = FALSE)
    if (nc < min_components) return(res)

    # Best site = the analysis where the most components co-occur.
    best_n <- 0L; best_site <- NULL
    for (s in names(by_site)) {
      cell <- by_site[[s]]
      nm <- sum(vapply(comps, function(c) val_in(cell, c), logical(1)))
      if (nm > best_n) { best_n <- nm; best_site <- cell }
    }
    # A test is FOUND only when at least two of its components co-occur at one
    # site. Accepting a single component made a fabricated result "found": a
    # reported t(42) = 9.99, p = .001 whose t appears NOWHERE in the output
    # still matched, because some analysis somewhere contained a value rounding
    # to .001 (62 of them in a 5,000-statistic corpus). A lone number is
    # coincidence, not evidence - matching the whole signature is the entire
    # point, as this function's own documentation states. `min_components`
    # gates which tests are ASSESSED; this gates what counts as found.
    # Always record how many components were found, even below the threshold,
    # so a near-miss (one component of three) is visible rather than hidden as
    # a flat zero - that distinction matters when diagnosing WHY a reported
    # result could not be reproduced.
    res$n_matched <- best_n
    if (best_n >= min(2L, nc) && !is.null(best_site)) {
      res$found <- TRUE
      res$source_file <- best_site$sf[1]
      res$analysis <- best_site$an[1]
      res$confidence <- if (best_n == nc) "full" else "partial"
      # Record every component's outcome at the best site — matched AND
      # unmatched — as "name=value" pairs, so a partial match shows exactly
      # which of the test's own values were found and which were not, rather
      # than a bare count that hides which specific claim failed to reproduce.
      matched <- vapply(comps, function(c) val_in(best_site, c), logical(1))
      fmt <- function(cs) paste(vapply(cs, function(c)
        sprintf("%s=%s", c$name, format(c$value, trim = TRUE)), character(1)),
        collapse = ", ")
      res$match_values <- if (any(matched)) fmt(comps[matched]) else ""
      res$not_matched  <- if (any(!matched)) fmt(comps[!matched]) else ""
    }
    res
  })
  out <- dplyr::bind_rows(rows)

  n <- nrow(out)
  attr(out, "summary") <- list(
    n_tests = n, n_found = sum(out$found),
    n_full = sum(out$confidence == "full"),
    n_partial = sum(out$confidence == "partial"),
    n_missing = sum(!out$found),
    pct_found = if (n) round(100 * sum(out$found) / n, 1) else NA_real_)
  out
}
