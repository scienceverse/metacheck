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
    grepl("η|eta|ηp²|partial eta", n)                                 ~ "eta2",
    grepl("ω|omega", n)                                               ~ "omega2",
    grepl("odds", n)                                                  ~ "or",
    grepl("^rb$|rank.?biserial|biserial", n)                          ~ "rb",
    grepl("bf|bayes", n)                                              ~ "bf",
    grepl("^r$|correlation|pearson|^rs$", n)                          ~ "r",
    grepl("^rho|^ρ|spearman", n)                                      ~ "rho",
    grepl("^β|beta|standardi", n)                                     ~ "beta",
    grepl("^b$|estimate|unstandardi", n)                              ~ "b",
    grepl("^se$|std\\. error|standard error", n)                     ~ "se",
    grepl("^m$|^mean$", n)                                            ~ "mean",
    grepl("^sd$|std\\. dev", n)                                       ~ "sd",
    grepl("^n$|^ns$|sample size", n)                                  ~ "n",
    grepl("^α|alpha|cronbach", n)                                     ~ "alpha",
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
      nv  <- .norm_value(g$rhs[i])
      if (is.na(fam) || is.na(nv$num)) return(NULL)   # drop junk components
      list(family = fam, name = g$lhs[i], value = nv$num,
           dec = nv$dec, censored = nv$censored)
    })
    comps <- Filter(Negate(is.null), comps)
    if (!length(comps)) return(NULL)
    list(text_id = g$.tid[1], grp_id = g$.gid[1], components = comps)
  }) |> (\(x) Filter(Negate(is.null), x))()
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
#'   matched output values), `source_file` / `analysis` (provenance of the match),
#'   and `confidence` ("full" all components matched / "partial" / "none").
#'   Attribute `"summary"` holds the roll-up.
#' @export
match_reported_output <- function(paper, output, min_components = 2L) {
  eq <- if (inherits(paper, "scivrs_paper")) paper$eq else paper
  tests <- .recompose_eq(eq)

  # Output long table, keeping source_file + analysis provenance.
  out_long <- if (is.data.frame(output)) output else {
    parts <- lapply(output, function(s) s$long)
    parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
    if (length(parts)) dplyr::bind_rows(parts) else data.frame()
  }
  empty <- data.frame(text_id = integer(0), grp_id = integer(0),
                      reported = character(0), n_components = integer(0),
                      n_matched = integer(0), found = logical(0),
                      match_values = character(0), source_file = character(0),
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
  site  <- paste(sf[keep], an[keep], sep = "")
  by_site <- split(data.frame(val = ovals[keep], fam = ofam[keep],
                              sf = sf[keep], an = an[keep],
                              stringsAsFactors = FALSE), site)

  val_in <- function(cell, comp) {
    tol <- 0.5 / (10^comp$dec)
    if (nzchar(comp$censored)) {
      if (comp$censored == "<") any(cell$val < comp$value) else any(cell$val > comp$value)
    } else {
      # prefer a family-consistent output cell; fall back to any value
      fam_cells <- cell$val[!is.na(cell$fam) & cell$fam == comp$family]
      any(abs(round(fam_cells, comp$dec) - comp$value) < tol) ||
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
    if (best_n >= 1L && !is.null(best_site)) {
      res$n_matched <- best_n
      res$found <- TRUE
      res$source_file <- best_site$sf[1]
      res$analysis <- best_site$an[1]
      res$confidence <- if (best_n == nc) "full" else "partial"
      # record which reported values were the matched ones
      matched <- vapply(comps, function(c) val_in(best_site, c), logical(1))
      res$match_values <- paste(vapply(comps[matched], function(c)
        format(c$value, trim = TRUE), character(1)), collapse = ", ")
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
