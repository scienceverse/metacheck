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

# The anchor's own parenthetical degrees of freedom, as extract_eq() captures
# it in its `df` column: "(28)" for a one-df test (t, chi2, z, ...) or
# "(2, 57)" for a two-df F-test (numerator, denominator) — see
# text-extractors.R's own docstring for the exact format. This was
# PREVIOUSLY IGNORED ENTIRELY: .recompose_eq()/.tests_from_extract() read
# every OTHER field off a component (name, value, comp) but never `df`, so
# a reported t(309) = 8.31's own 309 could never be checked against the
# output's df cell, even though df is often the single most discriminating
# value for picking the right site when two candidate analyses report
# similar t/p/d values (confirmed against a real corpus paper's two
# same-shaped-looking t-tests at DIFFERENT sites, t(130)=5.56 vs
# t(114)=3.10 — the df alone tells them apart before even looking at p/d).
# Returns NULL when `x` is not a parenthetical df at all (bare NA, as most
# components carry). Returns list(df1 = <parsed value>) for a single df, or
# list(df1 = ..., df2 = ...) for an F-test's two.
.norm_df <- function(x) {
  # nzchar(NA_character_) is TRUE by default in R (NA is not "empty" unless
  # keepNA is set), so an explicit is.na() guard is required here -- relying
  # on nzchar() alone to catch a bare NA `df` field (the common case: only
  # the anchor component carries a real parenthetical, every other
  # component's df is NA) would silently skip this check.
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(NULL)
  s <- trimws(as.character(x))
  if (!nzchar(s)) return(NULL)
  inner <- sub("^\\((.*)\\)$", "\\1", s)
  if (identical(inner, s)) return(NULL)   # not parenthesised at all
  parts <- trimws(strsplit(inner, ",")[[1]])
  if (length(parts) == 1) {
    v <- .norm_value(parts[1])
    if (is.na(v$num)) return(NULL)
    return(list(df1 = v))
  }
  if (length(parts) == 2) {
    v1 <- .norm_value(parts[1]); v2 <- .norm_value(parts[2])
    if (is.na(v1$num) || is.na(v2$num)) return(NULL)
    return(list(df1 = v1, df2 = v2))
  }
  NULL   # more than two comma-separated parts: not a recognised df shape
}

# Coarse statistic family for a name, both sides. NA when unrecognised — used to
# (a) drop junk reported rows whose "name" is prose/a variable, and (b) prefer a
# type-consistent output cell when matching.
.stat_family <- function(name) {
  n <- tolower(trimws(name %||% ""))
  n <- gsub("[[:space:]]+", " ", n)
  # jamovi's t-test tables name their primary test-statistic column literally
  # "stat", with the bracket naming WHICH test produced it (not a correction
  # of the same quantity) — see stato-map.R's identical handling for the full
  # justification and the tag confirmation. This must be resolved BEFORE the
  # generic strip below, which would otherwise reduce "stat[stud]" to the
  # unmapped bare "stat" and lose the one thing that says what it even is.
  n_pre_strip <- n
  # Every OTHER jamovi bracket suffix (df[stud], p[stud], md[stud], es[stud],
  # cil[stud]/ciu[stud]/ciles[stud]/ciues[stud], f[gg]/p[hf] sphericity
  # corrections, ...) names a correction/variant of the SAME quantity, so
  # stripping it and classifying the bare prefix is correct — mirroring
  # .stato_strip_variant() (R/stato-map.R), which this reuses so the two
  # typing paths (display vs. matching) cannot silently diverge again. This
  # was previously ASSUMED to already work ("the generic cases below still
  # resolve correctly once ... bare post-strip names [are] passed through")
  # without the strip actually being implemented here — confirmed as a real
  # bug against a real corpus paper: p[stud] and es[stud] both returned NA
  # (unrecognised) with no strip, which meant a reported t/p/d test could
  # match its own t (once the stat[stud] fix above landed) but never its own
  # p or d, because the output side's p[stud]/es[stud] cells were invisible
  # to the type-gated match in val_in().
  n <- .stato_strip_variant(n)
  dplyr::case_when(
    grepl("^stat\\[(stud|welc)\\]$", n_pre_strip)                     ~ "t",
    n_pre_strip == "stat[mann]"                                       ~ "W",
    # Pr(>F) / Pr(>|t|) / Pr(>|z|) / Pr(>Chi) are ALWAYS a p-value column —
    # R's anova()/aov()/summary() print the test statistic the p-value belongs
    # to inside the parens (">F" for an F-test row, ">|t|" for a coefficient
    # row, ...), but that names which test the p-value IS FOR, not the
    # p-value's own type. This must be checked BEFORE the F/t/chisq anchor
    # clauses below, or "Pr(>F)" is claimed by the "^pr\\(>f" branch there and
    # typed family "F" — which silently blocks the reported p from ever
    # matching it (val_in()'s type-gate requires a "p"-family cell), even
    # though the exact right value sits at the exact right site. Confirmed
    # against a real corpus paper: an aov() table's own "F value"/"Pr(>F)"
    # pair at one test_id, where F matched (F stayed correctly typed below)
    # but p never could, until this line moved p-value recognition first.
    # "pval" is metafor::rma()'s own column name (its "Model Results" table
    # header), confirmed by reproducing rma()'s real print output — no space,
    # hyphen, or "Pr(>" wrapper, so it fell through every other pattern here to
    # NA (unrecognised) without this explicit case, and a meta-analysis
    # paper's reported "p < .001" could then only match at the wrong site (via
    # the censored branch's any-value fallback for an unrecognised name) or
    # not at all.
    grepl("^p$|^pval$|p-value|p value|pr\\(>", n)                     ~ "p",
    # No case existed for "df" at all before this — a standalone reported
    # "df = X" (extract-tests.R's own .TEST_SATELLITES lists "df" as valid
    # outside the t(df)/F(df1,df2) parenthetical form) could never match a
    # df cell on the output side, jamovi's own included (df[stud] strips to
    # bare "df" above and fell through to NA with no case to catch it).
    # "df1"/"df2" are the anchor's OWN parenthetical df, once split by
    # .norm_df() (an F-test's "F(2, 2159)" -> numerator/denominator) — kept
    # as a SEPARATE, narrower family from generic "df" because the output
    # side genuinely distinguishes them too (stato-map.R's own df1 ->
    # STATO:0000498 "numerator degrees of freedom", df2 -> STATO:0000527
    # "denominator", both distinct STATO classes from the generic df/
    # STATO:0000069): a numerator df of 2 must never be allowed to match a
    # denominator-df cell that also happens to equal 2.
    grepl("^df1$", n)                                                 ~ "df1",
    grepl("^df2$", n)                                                 ~ "df2",
    grepl("^df$", n)                                                  ~ "df",
    grepl("^t($| |-|value|stat)", n) | n == "student's t" |
      grepl("t-?test|student'?s t", n)                                ~ "t",
    grepl("^f($| |-|value| change)", n) |
      grepl("\\banova\\b|f-?test", n)                                 ~ "F",
    grepl("chi|χ|x-squared|x²|goodness of fit", n)                    ~ "chisq",
    grepl("^z($| |value)", n)                                         ~ "z",
    grepl("^w$|wilcoxon|mann-whitney", n)                             ~ "W",
    grepl("^u$", n)                                                   ~ "U",
    grepl("^q$", n)                                                   ~ "Q",
    # "es" bare is jamovi's own t-test "Effect Size" column name (its own
    # bracket-tag, e.g. es[stud], is stripped above like every other jamovi
    # variant suffix) — jamovi's effect-size column defaults to Cohen's d and
    # stato-map.R's stato_type_column() already treats bare "es" as an effect
    # size estimate unconditionally, so typing it "d" here for matching is
    # the same assumption already made on the display-typing side, not a new
    # one. Confirmed against a real corpus paper: a ttestOneS table's
    # es[stud] value rounded exactly to the manuscript's own reported
    # Cohen's d.
    grepl("cohen'?s d|^d$|^ds$|^es$|effect size", n)                  ~ "d",
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
    # "conf.low"/"conf.high" are broom::tidy()'s (and broom.mixed's) own bound
    # names — extremely common in modern tidyverse-style R analysis code — and
    # neither contains "lower"/"upper"/"ci"/"interval", so without this explicit
    # case every broom-derived CI bound returned NA here and could never match,
    # confirmed against a real corpus paper whose Experiment2.R output carried
    # conf.low/conf.high at the exact site its reported CI should have matched.
    # "ci.lb"/"ci.ub" are metafor::rma()'s own bound names (its "Model Results"
    # table header), confirmed by reproducing rma()'s real print output.
    # "cilow"/"cihig" are jamovi's OWN bound names (no separator at all between
    # "ci" and "low"/"hig") — confirmed against a real corpus paper's .omv
    # t-test output. None of these contain "lower"/"upper"/"ci_"/"conf" with a
    # separator, so without these explicit cases each fell through to the
    # GENERIC "ci" catch-all below (since "ci.lb"/"cilow" both still contain
    # the substring "ci") — and a generic "ci"-typed output cell can NEVER
    # match a reported CI bound, because .norm_interval() (the reporting side)
    # always splits a bracketed CI into the SPECIFIC ci_lower/ci_upper
    # families, never the generic "ci"; the strict type-gate in
    # match_reported_output()'s val_in() requires an EXACT family match, so a
    # bound typed only "ci" was invisible to every reported CI regardless of
    # its actual value.
    # "cil"/"ciu"/"ciles"/"ciues" are jamovi's OWN bound names after this
    # function's own .stato_strip_variant() strip above (cil[stud] -> "cil",
    # ciles[stud] -> "ciles") — jamovi's t-test table can report a CI on BOTH
    # the mean difference (cil/ciu) AND the effect size (ciles/ciues: "ci
    # [for the] es") as two DIFFERENT intervals in the same table, confirmed
    # against a real corpus paper's ttestOneS output (cil/ciu = [0.34, 0.56]
    # around md=0.45; ciles/ciues = [0.35, 0.59] around es=0.47 — visibly not
    # the same interval). A manuscript's single reported "95% CI = [x, y]"
    # gives no way to tell which of the two it names, so both map to the same
    # generic ci_lower/ci_upper family and val_in() tries either candidate —
    # matching whichever one the reported bound actually equals, rather than
    # silently picking one and missing the CI whenever it was the other.
    grepl("^lcl$|^ci_?lower$|^conf\\.?low$|^ci\\.lb$|^cilow$|^cil$|^ciles$", n) ~ "ci_lower",
    grepl("^ucl$|^ci_?upper$|^conf\\.?high$|^ci\\.ub$|^cihig?h?$|^ciu$|^ciues$", n) ~ "ci_upper",
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
      main <- list(family = fam, name = g$lhs[i], value = nv$num,
                  dec = nv$dec, censored = cens)
      # The anchor's OWN parenthetical df ("t(309) = 8.31" -> df "(309)",
      # "F(2, 2159) = 6.76" -> df "(2, 2159)") was previously dropped
      # entirely — extract_eq()'s own `df` column was never read here. Added
      # as a SEPARATE component alongside the anchor's value, so it must
      # independently match a df/df1/df2 cell at the site — see .norm_df()'s
      # own comment for why this is worth doing.
      dfv <- if (!is.null(g$df)) .norm_df(g$df[i]) else NULL
      if (!is.null(dfv)) {
        df_comps <- if (!is.null(dfv$df2))
          list(list(family = "df1", name = "df1", value = dfv$df1$num,
                    dec = dfv$df1$dec, censored = ""),
               list(family = "df2", name = "df2", value = dfv$df2$num,
                    dec = dfv$df2$dec, censored = ""))
        else list(list(family = "df", name = "df", value = dfv$df1$num,
                       dec = dfv$df1$dec, censored = ""))
        return(c(list(main), df_comps))
      }
      list(main)
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
    comps <- lapply(seq_along(g), function(gi) {
      c <- g[[gi]]
      # `is_anchor`/`pos` are carried through for .regroup_by_evidence()
      # ONLY (see its own header comment): they identify which components in
      # a SENTENCE's pooled candidates are anchor-eligible, and where each
      # sat in the original reported order, so evidence-driven regrouping can
      # re-derive test boundaries independently of extract_tests()'s own
      # text-only .split_into_tests() heuristic. Unused by the plain scoring
      # path below (val_in() never reads them). `pos` MUST be the SENTENCE-
      # wide position (extract-tests.R's own `sentence_pos`, tagged before
      # any split), not this loop's local `gi` — a post-split index would
      # reset to 1, 2, 3... for every piece .split_into_tests() cut the
      # sentence into, making a distance comparison between two components
      # from DIFFERENT split pieces meaningless. Falls back to `gi` only for
      # components built before sentence_pos existed (e.g. a stale cached
      # extract_tests() result predating this fix).
      pos <- c$sentence_pos %||% gi
      is_anch <- .is_anchor(c$name)
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
               dec = ivl$lo$dec, censored = "", is_anchor = FALSE, pos = pos),
          list(family = "ci_upper", name = "ci_upper", value = ivl$hi$num,
               dec = ivl$hi$dec, censored = "", is_anchor = FALSE, pos = pos)))
      }
      nv <- .norm_value(c$value)
      if (is.na(nv$num)) return(NULL)
      cens <- nv$censored
      cc <- trimws(as.character(c$comp %||% ""))
      if (!nzchar(cens) && cc %in% c("<", ">")) cens <- cc
      main <- list(family = fam, name = c$name, value = nv$num,
                  dec = nv$dec, censored = cens, is_anchor = is_anch, pos = pos)
      # The anchor's own parenthetical df — see .recompose_eq()'s identical
      # handling above for why this is added.
      dfv <- .norm_df(c$df)
      if (!is.null(dfv)) {
        df_comps <- if (!is.null(dfv$df2))
          list(list(family = "df1", name = "df1", value = dfv$df1$num,
                    dec = dfv$df1$dec, censored = "", is_anchor = FALSE, pos = pos),
               list(family = "df2", name = "df2", value = dfv$df2$num,
                    dec = dfv$df2$dec, censored = "", is_anchor = FALSE, pos = pos))
        else list(list(family = "df", name = "df", value = dfv$df1$num,
                       dec = dfv$df1$dec, censored = "", is_anchor = FALSE, pos = pos))
        return(c(list(main), df_comps))
      }
      list(main)
    })
    comps <- unlist(Filter(Negate(is.null), comps), recursive = FALSE)
    if (!length(comps)) return(NULL)
    list(text_id = tt$text_id[i], grp_id = tt$test_no[i],
         sentence = tt$sentence[i], components = comps)
  })
  Filter(Negate(is.null), out)
}

# Do two output SITES plausibly concern the SAME underlying variable? Used
# when a sentence's own components end up confirmed at two DIFFERENT sites
# (a mean+CI at one, an alpha at another; a t-test at one, its own df2 at a
# unioned residuals row at another) — row_label is the one human-readable
# identifier that survives across otherwise-unrelated analyses on the same
# variable, confirmed against a real corpus paper (a t-test's row_label
# "compassion_mindset" and its separately-computed reliability's row_label
# "compassion_mindset_1" both carry the token "compassion_mindset", despite
# different test_id, different analysis type, no shared table).
#
# Compares TOKENS (split on non-alphanumeric), not raw substrings or whole
# strings: row_labels are often themselves compound ("compassion_mindset_1",
# "emo_total_unlimited_limited"), so requiring an exact string match would
# miss the real link, while a raw substring match risks a false positive on
# an unrelated but textually similar label. A token must ALSO be more than 3
# characters to count — short tokens ("t", "d", "ci", "sd", numeric suffixes
# like "1"/"2") are exactly the kind of generic fragment that recurs across
# unrelated variables by chance, so they are excluded rather than treated as
# a genuine link between two sites.
.sites_share_variable <- function(rl_a, rl_b) {
  if (is.na(rl_a) || is.na(rl_b) || !nzchar(rl_a) || !nzchar(rl_b)) return(NA)
  tok <- function(s) {
    # tolower() FIRST, then split on non-alnum -- strsplit() has no
    # ignore.case argument at all (that belongs to grepl/grep/sub), so
    # case-insensitivity has to come from lower-casing the input string
    # before splitting, not from the split call itself.
    t <- strsplit(tolower(s), "[^a-z0-9]+", perl = TRUE)[[1]]
    t[nchar(t) > 3]
  }
  ta <- tok(rl_a); tb <- tok(rl_b)
  if (!length(ta) || !length(tb)) return(NA)
  any(ta %in% tb)
}

# Re-derive test boundaries from OUTPUT evidence instead of trusting
# extract_tests()'s text-only split — see the header comment at this
# function's own call site (inside match_reported_output()) for the full
# rationale and the two confirmed real-corpus failure modes it fixes.
#
# `text_proximity`: an anchor's claim on a pooled component can additionally
# be required to sit within `text_proximity` original sentence positions
# (`pos`) of the anchor itself, on top of matching the same output site.
# Defaults to NULL (output co-occurrence alone decides) after evaluating
# proximity = 3/5/10 against a real corpus paper's own 135-test regrouping
# and finding IDENTICAL results (n_found, n_full, and every implausible-
# split flag) at every value tested, once the same-family and cross-anchor
# site-exclusivity invariants below were in place: those two invariants
# resolve the actual coincidental-cross-test-claim failure mode text
# proximity was meant to guard against, leaving proximity with no measured
# effect. Kept as a parameter (not removed) in case a case not covered by
# this evaluation surfaces where it does help.
#
# Components lacking `is_anchor`/`pos` (the `.recompose_eq()` legacy path,
# used only when extract_tests() itself produced nothing) are left
# untouched: regrouping needs extract_tests()'s own anchor tagging, so a
# test built the legacy way keeps its original, ungrouped-by-evidence shape.
.regroup_by_evidence <- function(tests, by_site, val_in, text_proximity = NULL) {
  if (!length(tests)) return(tests)
  has_tags <- vapply(tests, function(t)
    length(t$components) > 0 &&
      all(vapply(t$components, function(c) !is.null(c$is_anchor), logical(1))),
    logical(1))
  if (!any(has_tags)) return(tests)

  taggable <- tests[has_tags]
  untouched <- tests[!has_tags]

  # Pool every test sharing one text_id (one SENTENCE) back together —
  # undoing extract_tests()'s own split — keyed on text_id since that is the
  # one identifier every test from the same sentence shares regardless of
  # how many pieces .split_into_tests() cut it into.
  tids <- vapply(taggable, function(t) as.character(t$text_id %||% NA), character(1))
  by_tid <- split(taggable, tids)

  # Best site + score for one component against every candidate site —
  # shared by the anchor search and the per-anchor claiming pass below, so
  # both use the IDENTICAL notion of "matches" that the main scoring loop
  # (val_in(), just above this function's call site) already uses.
  #
  # `used_sites` is a SHARED, MUTABLE set (an environment, since R's normal
  # copy-on-modify semantics would otherwise reset it every sentence): once
  # a site has been claimed as one anchor's OWN primary site (anywhere in
  # the paper, not just within one sentence), no OTHER anchor may ALSO claim
  # that identical site as ITS primary site -- the same output row cannot be
  # the true source of two DIFFERENT reported tests. A site already used is
  # skipped in best_site_for()'s search entirely, so a second anchor whose
  # true site is genuinely elsewhere still finds it (skipping the used one
  # does not stop the search, it just means that candidate can never win).
  # This is DELIBERATELY about the ANCHOR's own primary site only, not every
  # cell a satellite happens to match: a value legitimately re-cited across
  # two different sentences (the same p restated in the discussion section)
  # is not blocked by this, because it is the SATELLITE match (val_in() on
  # an already-claimed site's cell), not a competing anchor claim on that
  # site itself.
  used_sites <- new.env(parent = emptyenv())
  best_site_for <- function(comp, exclude_used = FALSE) {
    best_s <- NULL
    for (s in names(by_site)) {
      if (exclude_used && !is.null(used_sites[[s]])) next
      if (val_in(by_site[[s]], comp)) { best_s <- s; break }
    }
    best_s
  }

  regrouped <- lapply(by_tid, function(grp) {
    pool <- unlist(lapply(grp, `[[`, "components"), recursive = FALSE)
    if (!length(pool)) return(grp)
    is_anch <- vapply(pool, function(c) isTRUE(c$is_anchor), logical(1))
    if (!any(is_anch)) return(grp)   # nothing to regroup around

    # Which ORIGINAL pre-regroup test (index into `grp`) each pooled
    # component came from — needed after regrouping to ask "did this
    # component's original TEXT neighbours end up somewhere else?", which is
    # exactly the plausibility question .sites_share_variable() answers.
    orig_test <- rep(seq_along(grp), vapply(grp, function(t) length(t$components), integer(1)))

    anchor_idx <- which(is_anch)

    claimed <- rep(FALSE, length(pool))
    new_groups <- list(); new_sites <- character(0)
    for (k in seq_along(anchor_idx)) {
      ai <- anchor_idx[k]
      if (claimed[ai]) next   # an EARLIER anchor's site already swept this one up
      # Site search happens HERE, per anchor, INSIDE the claim loop -- not
      # precomputed for every anchor up front -- so it correctly sees sites
      # an EARLIER anchor in this same pass (same sentence, or an earlier
      # sentence entirely, since used_sites is shared across the whole
      # paper) has already claimed as ITS primary site. exclude_used = TRUE
      # is what enforces "one output row confirms at most one reported
      # test" -- see used_sites' own comment above for the full rationale.
      site_name <- best_site_for(pool[[ai]], exclude_used = TRUE)
      if (is.null(site_name)) next   # no (unused) evidence at all for this anchor; leave for fallback
      used_sites[[site_name]] <- TRUE
      cell <- by_site[[site_name]]
      candidate <- vapply(seq_along(pool), function(j) {
        if (claimed[j]) return(FALSE)
        if (!is.null(text_proximity) &&
            abs((pool[[j]]$pos %||% Inf) - (pool[[ai]]$pos %||% -Inf)) > text_proximity)
          return(FALSE)
        val_in(cell, pool[[j]])
      }, logical(1))
      candidate[ai] <- TRUE   # the anchor always claims itself
      # A group can never contain two components of the SAME family -- no
      # real test reports two p-values, two t-values, etc. A duplicate here
      # is proof the site is being asked to explain components from TWO
      # DIFFERENT original tests at once, not evidence of a real match.
      # Confirmed as a real bug against a real corpus paper: a sentence
      # reporting two SEPARATE t-tests, both "p < .001", where the site had
      # only ONE p cell -- val_in()'s censored branch (any cell_val < bound)
      # doesn't "consume" the cell it matches, so BOTH tests' own p
      # component independently satisfied the same censored check against
      # the same lone p cell, producing "t=14.46 p=0.001 p=0.001" with the
      # second test's p silently misattributed to the first test's group.
      # Resolved by keeping only the candidate CLOSEST to the anchor's own
      # `pos` per family (its most likely original pairing -- the anchor's
      # own text-adjacent components should always win a tie over one from
      # a different original test that only matched by output coincidence);
      # every other same-family candidate is released back to `claimed =
      # FALSE` so it can still be claimed correctly by whichever OTHER
      # anchor's pass it actually belongs to.
      cand_idx <- which(candidate)
      cand_fam <- vapply(cand_idx, function(j) pool[[j]]$family %||% NA_character_, character(1))
      dup_fams <- unique(cand_fam[duplicated(cand_fam) & !is.na(cand_fam)])
      for (fam in dup_fams) {
        tied <- cand_idx[cand_fam == fam]
        dist <- abs(vapply(tied, function(j) pool[[j]]$pos %||% Inf, numeric(1)) -
                    (pool[[ai]]$pos %||% -Inf))
        loser <- tied[-which.min(dist)]
        candidate[loser] <- FALSE
      }
      claim <- candidate
      claimed[claim] <- TRUE
      new_groups[[length(new_groups) + 1L]] <- pool[claim]
      new_sites <- c(new_sites, site_name)
    }
    # Anything no anchor's evidence claimed (an anchor with no site, or a
    # satellite no anchor's site happened to contain) falls back to
    # extract_tests()'s OWN original grouping for exactly those components —
    # never dropped, never left ungrouped. Reconstructed by intersecting the
    # original per-test membership with the leftover set, so a satellite
    # that WAS correctly grouped by the text heuristic (and no evidence
    # contradicts) keeps that grouping rather than becoming a stray single.
    leftover <- which(!claimed)
    if (length(leftover)) {
      offset <- 0L
      for (t in grp) {
        n <- length(t$components)
        keep <- (offset + seq_len(n)) %in% leftover   # local mask, same length as t$components
        if (any(keep)) {
          new_groups[[length(new_groups) + 1L]] <- t$components[keep]
          new_sites <- c(new_sites, NA_character_)   # no output evidence at all for this piece
        }
        offset <- offset + n
      }
    }
    # PLAUSIBILITY: a group's claim is "confirmed" (no flag) when either (a)
    # it kept every one of its ORIGINAL text-sentence test-mates (regrouping
    # changed nothing for it), or (b) an orphaned original test-mate ended
    # up in ANOTHER group whose own site shares a row_label token with this
    # one (see .sites_share_variable()) -- the SAME variable, reached via a
    # different analysis, which is exactly what you would expect a genuinely
    # split pipeline (descriptives here, a t-test there, its effect size
    # somewhere else) to look like. Otherwise flagged `plausible = FALSE`:
    # NOT dropped or downgraded automatically (per instruction: report every
    # match, let the reader judge) -- match_reported_output()'s own output
    # columns surface source_file/analysis for exactly this, and the report
    # text explains what the flag means once, generally, rather than
    # silently hiding or renaming the result.
    orig_of <- lapply(new_groups, function(g) unique(orig_test[match(g, pool)]))
    plausible <- vapply(seq_along(new_groups), function(gi) {
      mates <- setdiff(unlist(lapply(orig_of[[gi]], function(oi) which(orig_test == oi))),
                       match(new_groups[[gi]], pool))
      if (!length(mates)) return(TRUE)   # no orphaned original test-mates at all
      my_rl <- if (!is.na(new_sites[gi])) by_site[[new_sites[gi]]]$rl[1] else NA_character_
      other_gi <- vapply(mates, function(m)
        which(vapply(new_groups, function(g) m %in% match(g, pool), logical(1)))[1], integer(1))
      shares <- vapply(unique(other_gi), function(ogi) {
        other_rl <- if (!is.na(new_sites[ogi])) by_site[[new_sites[ogi]]]$rl[1] else NA_character_
        isTRUE(.sites_share_variable(my_rl, other_rl))
      }, logical(1))
      any(shares)
    }, logical(1))

    lapply(seq_along(new_groups), function(gi) {
      g <- new_groups[[gi]]
      list(text_id = grp[[1]]$text_id, grp_id = grp[[1]]$grp_id,
           sentence = grp[[1]]$sentence, components = g,
           plausible_split = plausible[gi])
    })
  })
  c(unlist(regrouped, recursive = FALSE), untouched)
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
#' @param include_tables if TRUE and `paper` is a paper object, also build
#'   tests from statistics reported only in a results table's cells (e.g. from
#'   Grobid's table parsing — see `.tei_table_contents()` in
#'   R/import-grobid.R and the 5-tier header/caption/value-shape/row/matrix
#'   approach in R/match-table.R's `.table_tests()`) and add them to the set
#'   checked against `output`. Default FALSE: table-derived tests are opt-in,
#'   currently used only by `reproducibility_check`, so
#'   every other caller's matching behaviour is unchanged.
#' @param min_components a test must have at least this many recognised components
#'   to be assessed (default 1, so a lone reported statistic — a bare
#'   correlation `r=-.27` or `d = 0.40` with no accompanying p/CI — is still checked against the
#'   output, TYPE-GATED the same as any other component: it only matches an
#'   output cell of the SAME family, at the site with the most co-occurring
#'   components, via `val_in()`. This carries a higher coincidence risk than a
#'   multi-component test (a single r rounding to the reported value at some
#'   site is more likely than a whole signature doing so by chance), so a
#'   single-component match is worth reading with that in mind — set to 2 to
#'   exclude single-component rows entirely, as this function used to by
#'   default.
#'
#' @returns a data.frame, one row per recomposed reported test, with:
#'   `text_id`, `grp_id`, `reported` (the recomposed test as text, e.g.
#'   "W=183.5 p=.791 rb=-0.16"), `n_components`, `n_matched` (components found in
#'   the best-matching output analysis), `found` (logical), `match_values` (the
#'   matched components as "name=value" pairs), `not_matched` (the UNMATCHED
#'   components of that same test, same "name=value" form — always populated
#'   alongside `match_values` for a partial match, so exactly which claim failed
#'   to reproduce is visible, not just a count), `source_file` / `analysis`
#'   (provenance of the match), `confidence` ("full" all components matched
#'   / "partial" / "none"), and `plausible_split` — NA unless this test's
#'   grouping came from `.regroup_by_evidence()` re-deriving test boundaries
#'   from OUTPUT co-occurrence rather than trusting `extract_tests()`'s
#'   text-only guess (see that function's own header comment): TRUE when a
#'   component split away from its original reported neighbours landed at a
#'   site sharing a `row_label` token with theirs (the same underlying
#'   variable, reached via a different analysis — plausible), FALSE when no
#'   such link was found (the split is NOT hidden or downgraded, only
#'   flagged, so a reader can judge the match's plausibility from the
#'   `source_file`/`analysis` of every piece involved). Attribute
#'   `"summary"` holds the roll-up.
#' @export
match_reported_output <- function(paper, output, include_tables = FALSE,
                                  min_components = 1L) {
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

  # Table-derived tests (opt-in): .table_tests() (R/match-table.R) scans
  # paper$table$contents (statistics reported only inside a results table's
  # cells, invisible to extract_eq()/extract_tests() because that content never
  # enters paper$text — see that file's own header comment for the full 5-tier
  # header/caption/value-shape/row/matrix approach). Already built in this
  # function's own internal "test" shape (list(text_id, grp_id, components)),
  # so it is simply appended to `tests` rather than recomposed here. Its
  # text_id is synthesised as a NEGATIVE number unique to its (table_id, row) —
  # never colliding with a real paper$text row id — so a table-derived row is
  # always visibly distinguishable in the RESULT rows this function returns
  # (see the final `rows` below, where text_id/grp_id are plain display
  # columns): no real text_id is ever <= 0.
  if (isTRUE(include_tables) && inherits(paper, "scivrs_paper")) {
    tt <- tryCatch(.table_tests(paper), error = function(e) NULL)
    if (!is.null(tt) && length(tt) > 0) tests <- c(tests, tt)
  }

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
                      analysis = character(0), confidence = character(0),
                      plausible_split = logical(0))
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
  # row_label is carried into every site cell for .regroup_by_evidence()'s
  # cross-site plausibility check: when a sentence's components end up
  # confirmed at TWO DIFFERENT sites, row_label is the one signal available
  # that ties them to the SAME underlying variable rather than an unrelated
  # coincidence -- confirmed against a real corpus paper, where a t-test's
  # own site (row_label "compassion_mindset") and its reliability alpha's
  # SEPARATE site (row_label "compassion_mindset_1") both carry the same
  # variable-name token despite sharing no test_id, analysis type, or table.
  rl <- out_long$row_label %||% rep(NA_character_, nrow(out_long))
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
                              sf = sf[keep], an = an[keep], rl = rl[keep],
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
                                   rl = rl[keep][has_ref],
                                   stringsAsFactors = FALSE), model_site)
      by_site <- c(by_site, by_model)
    }
  }

  # jamovi's own ANOVA table gives its "residuals" row (the denominator df,
  # SS, MS the whole table shares) a SEPARATE test_id from each effect row
  # (.stat_test_id() in stat-output.R keys on row_label, and "residuals" is
  # its own row) -- correct for a Coefficients table, where every row IS an
  # independently reportable test, but wrong here: "the residuals" is never
  # itself a reported claim, it exists only to supply the denominator df an
  # F-test's sibling effect rows need. Confirmed against a real corpus
  # paper's jamovi one-way-ANOVA output: F/p/etaSq sat at
  # "..._a92_gender", the SAME table's denominator df=2159 at
  # "..._a92_residuals" -- a reported "F(2, 2159)" could match its own F/p
  # but never its own df2, because 2159 sat at a DIFFERENT site. Same fix
  # shape as model_ref just above: union the residuals row into EACH sibling
  # effect test_id sharing its (source_file, analysis) prefix, as an
  # ADDITIONAL site tried alongside the narrow one -- a real per-row site
  # still wins on its own when a test's whole signature came from one row.
  # Matched by STRING PREFIX on test_id (the same opaque, sanitized id
  # .stat_test_id() built, "_residuals" being jamovi's own fixed row label,
  # confirmed identical across every ANOVA table in the same real paper) —
  # not by re-deriving analysis_id, which match_reported_output() has no
  # access to; this carries the same string-matching caveat model_ref's own
  # site key already accepts.
  if (!is.null(tid)) {
    tid_keep <- as.character(tid)[keep]
    is_resid <- grepl("_residuals$", tid_keep)
    if (any(is_resid)) {
      resid_prefix <- sub("_residuals$", "", tid_keep[is_resid])
      # A sibling's row_label can itself contain underscores (a pairwise
      # comparison row like "..._a92_female_male"), so the prefix can NOT be
      # recovered by stripping a fixed number of trailing segments from the
      # sibling's own id (that would cut into a multi-word label instead of
      # the row_label boundary). Matched by STARTS-WITH against
      # resid_prefix instead — correct regardless of how many underscores
      # the sibling's own row_label carries, since resid_prefix itself is
      # unambiguous (it is the residuals row's OWN id with only its fixed,
      # single-token "_residuals" suffix removed).
      for (pfx in unique(resid_prefix)) {
        resid_rows <- which(is_resid)[resid_prefix == pfx]
        sib_rows <- which(!is_resid)[startsWith(tid_keep[!is_resid], paste0(pfx, "_"))]
        if (!length(sib_rows)) next
        for (sib_tid in unique(tid_keep[sib_rows])) {
          rows <- c(which(tid_keep == sib_tid), resid_rows)
          by_site[[sib_tid]] <- data.frame(val = ovals[keep][rows],
                                           fam = ofam[keep][rows],
                                           sf = sf[keep][rows],
                                           an = an[keep][rows],
                                           rl = rl[keep][rows],
                                           stringsAsFactors = FALSE)
        }
      }
    }
  }

  val_in <- function(cell, comp) {
    tol <- 0.5 / (10^comp$dec)
    if (nzchar(comp$censored)) {
      # A censored bound ("p < .001") must ALSO respect a known family — the
      # same reason the exact-match branch below does. Without this, "p <
      # .001" matched ANY value below .001 at the site regardless of type,
      # including an unrelated SE/estimate that merely happened to be small —
      # confirmed as a real false positive: a reported test with no genuine
      # matching site at all (its real values appeared nowhere in the script's
      # output) still scored a spurious partial match, because the site's
      # OWN small-valued "pval" column (e.g. metafor::rma()'s own p, "<.0001",
      # itself well under .001) satisfied the censored check while an
      # unrelated "se" cell coincidentally rounded to the reported SE — two
      # false hits together outscored the correct (nonexistent) site. Falls
      # back to "any value" only when the component's name matched no
      # recognised family, same as the exact-match branch.
      cell_vals <- if (!is.na(comp$family))
        cell$val[!is.na(cell$fam) & cell$fam == comp$family] else cell$val
      if (comp$censored == "<") any(cell_vals < comp$value) else any(cell_vals > comp$value)
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
      if (any(abs(round(fam_cells, comp$dec) - comp$value) < tol)) return(TRUE)
      # SAME-SITE fallback to "b" (a generic model "Estimate"/coefficient
      # column) for certain reported families whose PROSE label is more
      # specific than what generic model-fitting software actually prints.
      # Two confirmed real cases:
      #   * "beta": authors very commonly write "β" for a model's raw/
      #     unstandardised coefficient (lme4/lmerTest's own printed "Estimate"
      #     column, family "b") without ever having computed a real
      #     standardised beta — six such components sat at the exact right
      #     test_id site as their matching t/p, unmatched only because
      #     "Estimate" (b) is not "β" (beta).
      #   * "d": a META-ANALYSIS pooled effect size (Cohen's d) is exactly
      #     what metafor::rma()'s generic "Model Results" table reports as
      #     "estimate" (typed "b", the same generic coefficient family a
      #     regression's slope gets) — rma() has no notion of "this run is
      #     estimating a d" to print a d-specific column name. Reproduced
      #     directly against a real metafor::rma() call to confirm its exact
      #     print header, then confirmed against a real corpus paper's own
      #     meta-analysis script.
      # SAFE only when the site has NO genuine cell of the reported family
      # itself to be confused with (that is the exact false-positive the
      # type-gate above exists to prevent, per the comment above) — so this
      # fires only as a last resort, never instead of a real same-family
      # match, and never across sites (a "b" at an UNRELATED site is never
      # reachable, since this still filters by `cell`, the one candidate site
      # already chosen).
      if (comp$family %in% c("beta", "d") &&
          !any(!is.na(cell$fam) & cell$fam == comp$family)) {
        b_cells <- cell$val[!is.na(cell$fam) & cell$fam == "b"]
        return(any(abs(round(b_cells, comp$dec) - comp$value) < tol))
      }
      # Same SAFE-fallback shape as beta/d -> b just above, for df1/df2 -> the
      # GENERIC "df" family: an F-test's own reported numerator/denominator
      # df ("F(2, 2159)", split into df1/df2 by .norm_df()) is typically
      # printed as two SEPARATE bare "df" cells by the underlying software,
      # not as distinct df1/df2 columns — jamovi's one-way-ANOVA table prints
      # the numerator df as a bare "df" on the effect row and the denominator
      # df as a bare "df" on the (unioned-in, see the residuals-row site
      # widening above) residuals row; R's oneway.test()/aov() do the
      # equivalent. Confirmed against a real corpus paper: an ANOVA's own F/p
      # already matched at the right site, but df1/df2 could not, until this
      # fallback, because the site's two df VALUES were both typed only the
      # generic "df", never anything df1/df2 could type-match directly.
      if (comp$family %in% c("df1", "df2") &&
          !any(!is.na(cell$fam) & cell$fam == comp$family)) {
        df_cells <- cell$val[!is.na(cell$fam) & cell$fam == "df"]
        return(any(abs(round(df_cells, comp$dec) - comp$value) < tol))
      }
      FALSE
    } else {
      # No recognised type for this component (name matched no family) — the
      # only case where matching by value alone is still attempted.
      any(abs(round(cell$val, comp$dec) - comp$value) < tol)
    }
  }

  # ── Evidence-driven regrouping ──────────────────────────────────────────
  #
  # extract_tests()'s own .split_into_tests() decides test boundaries from
  # TEXT ALONE (anchor vocabulary + repeat detection), before any output is
  # available -- and that heuristic is fundamentally ambiguous in both
  # directions, confirmed against a real corpus paper:
  #   * UNDER-splits: "M = 3.28, 95% CI = [3.18, 3.38], ..., Cronbach's α =
  #     .86" is textually ONE group (a single anchor, alpha, with no repeat
  #     to trigger a split), but is actually TWO unrelated claims -- a
  #     scale's own mean+CI, and its separately-computed reliability -- that
  #     the output confirms live in DIFFERENT analyses (a "descriptives"
  #     site and a "reliability" site).
  #   * OVER-splits: promoting every effect-size-shaped name (d, g, eta2, M,
  #     alpha, ...) to anchor status so the above case could split AT ALL
  #     also splits "t = 8.31, p < .001, d = 0.47" into two pieces (t+p,
  #     then d alone), even though t/p/d are the SAME test and the output
  #     confirms it -- they sit in ONE ttestOneS row together.
  # No fixed text-only rule distinguishes these: "anchor A followed by
  # anchor B" is sometimes the same claim (t then d) and sometimes not (M
  # then alpha), and the difference is not visible in the text at all -- it
  # is visible in the OUTPUT, which already tells us, per anchor, which
  # other reported values actually co-occur with it in one real analysis.
  # This re-derives test boundaries from that evidence: every ANCHOR
  # component from a shared sentence is pooled back together (undoing
  # .split_into_tests()'s guess), then each anchor's own best-matching site
  # (found the SAME way the scoring loop below finds one for a whole test)
  # greedily reclaims whichever OTHER pooled components also match there.
  # An anchor with no supporting site (no output at all, or nothing at that
  # site resembles it) falls back to extract_tests()'s original text-only
  # grouping for its own components -- so this only ever REFINES a boundary
  # using evidence, never removes the text-only fallback when there is none.
  if (length(by_site) > 0) tests <- .regroup_by_evidence(tests, by_site, val_in)

  rows <- lapply(tests, function(tst) {
    comps <- tst$components; nc <- length(comps)
    # A censored component ("p < .001") must show its comparator, not just its
    # bound: dropping `censored` here rendered EVERY such component as if it
    # were an exact "p=0.001", even though val_in()'s own matching (below)
    # correctly treats it as "< .001" throughout — the display simply never
    # read the field. Confirmed as a real bug against a real corpus paper's
    # regression table, whose several genuine "p < .001" cells all rendered
    # as a plain "p=0.001" in the report.
    reported <- paste(vapply(comps, function(c)
      sprintf("%s=%s%s", c$name, c$censored %||% "",
              format(c$value, trim = TRUE)), character(1)),
      collapse = " ")
    # NA when this test was never touched by .regroup_by_evidence() (no
    # output to regroup against, or a legacy .recompose_eq() test) -- TRUE/
    # FALSE only for a test regrouping actually produced, per its own
    # row_label plausibility check. Surfaced as its own column rather than
    # folded into `confidence`, so a reader can see BOTH how much of the
    # test matched AND whether a cross-site split (if any) looks plausible,
    # independently -- collapsing the two would hide which question a
    # "weak" result is actually answering.
    plausible <- tst$plausible_split %||% NA
    res <- data.frame(text_id = tst$text_id, grp_id = tst$grp_id,
                      reported = reported, n_components = nc, n_matched = 0L,
                      found = FALSE, match_values = NA_character_,
                      not_matched = NA_character_,
                      source_file = NA_character_, analysis = NA_character_,
                      confidence = "none", plausible_split = plausible,
                      stringsAsFactors = FALSE)
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
      # Same censored-comparator fix as `reported` above, for the per-component
      # match_values/not_matched breakdown.
      fmt <- function(cs) paste(vapply(cs, function(c)
        sprintf("%s=%s%s", c$name, c$censored %||% "",
                format(c$value, trim = TRUE)), character(1)),
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
