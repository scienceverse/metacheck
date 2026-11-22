#' Check Stats
#'
#' @param text the search table (or list of paper objects)
#' @param ... arguments to pass to statcheck()
#'
#' @return a table of statistics
#' @export
#'
#' @examples
#' paper <- demopaper()
#' stats(paper)
stats <- function(text, ...) {
  # An argument statcheck() does not have made every call fail, and the
  # error was swallowed, so stats() returned an empty table (U4)
  args <- list(...)
  allowed <- setdiff(names(formals(statcheck::statcheck)), c("texts", "messages"))
  bad <- setdiff(names(args) %||% rep("", length(args)), allowed)
  if (length(args) && (is.null(names(args)) || length(bad)))
    stop("stats() passes only named statcheck() arguments: ",
         paste(allowed, collapse = ", "), call. = FALSE)

  # a character vector is searched as a text table, one row per string; it
  # stopped with "argument is of length zero" (U4)
  if (is.character(text))
    text <- data.frame(text = text, text_id = seq_along(text))

  # lines with stats must have at least one number
  text <- text_search(text, "[0-9]")


  n <- nrow(text)
  if (n == 0) {
    return(data.frame())
  }

  # set up progress bar ----
  pb <- pb(n, "Checking stats [:bar] :current/:total :elapsedfull")

  subchecks <- lapply(seq_along(text$text), \(i) {
    subtext <- text$text[[i]]

    # statcheck uses cat() to output messages :(
    # A warning is muffled, not caught: catching it returned nothing for
    # the sentence, so one unparseable p-value ("p = .05-.10") dropped every
    # valid result next to it (U4)
    sink_output <- utils::capture.output(
      sc <- tryCatch(
        withCallingHandlers(statcheck::statcheck(subtext, messages = FALSE, ...),
                            warning = function(w) invokeRestart("muffleWarning")),
        error = function(e) {
          return(NULL)
        }
      )
    )
    if (verbose()) pb$tick()
    if (is.null(sc)) {
      return(data.frame())
    }
    sc$source <- i

    return(sc)
  })
  checks <- do.call(rbind, subchecks)

  if (nrow(checks) == 0) {
    return(checks)
  }

  text$source <- seq_along(text$text)

  stat_table <- dplyr::left_join(checks, text, by = "source")
  rownames(stat_table) <- NULL

  stat_table[, 2:ncol(stat_table)]
}

# For statcheck's rows (t- and F-tests): whether the reported p fits when
# statcheck's reading of the test is too strict, within the rounding of the
# statistic and the p (#695, point 6):
# - a statistic written with "<" or ">" is a bound, not a value: "F(1, 104) <
#   0.01, p = .939" fits, as every F below 0.01 gives a p above .92;
# - an F-test for equal variances (a variance ratio, as R's var.test() gives it)
#   reports a two-sided p, twice statcheck's: in a sentence about equal
#   variances or a variance ratio, twice the p fits. Levene's test is an ordinary
#   F with a one-tailed p, which statcheck reads right.
# NA where neither applies.
.stat_bound_or_variance_fits <- function(table) {
  n <- nrow(table)
  out <- rep(NA, n)
  if (!n) return(out)
  decimals <- function(s) {
    s <- sub("^[^0-9.]*", "", s)
    ifelse(grepl("\\.", s), nchar(sub("^[^.]*\\.", "", sub("[^0-9.].*$", "", s))), 0L)
  }
  # the p of a statistic: two-sided for t, upper tail for F
  p_of <- function(type, df1, df2, v) {
    if (type == "t") 2 * stats::pt(-abs(v), df2) else stats::pf(v, df1, df2, lower.tail = FALSE)
  }
  var_sentence <- "\\b(?:equal(?:ity)?|homogene\\w*|equality)\\s+(?:of\\s+)?variances?\\b|\\bvariances?\\s+(?:were|was|are|is)\\s+(?:not\\s+)?(?:equal|homogene)|\\bvariance\\s+ratio\\b|\\bvar\\.test\\b"
  for (i in seq_len(n)) {
    type <- table$test_type[i]
    df1 <- suppressWarnings(as.numeric(table$df1[i])); df2 <- suppressWarnings(as.numeric(table$df2[i]))
    tv <- abs(suppressWarnings(as.numeric(table$test_value[i])))
    rp <- suppressWarnings(as.numeric(table$reported_p[i]))
    if (is.na(tv) || is.na(rp) || is.na(df2) || !type %in% c("t", "F") || (type == "F" && is.na(df1))) next
    raw <- as.character(table$raw[i])
    tcomp <- as.character(table$test_comp[i] %||% "=")
    bound <- tcomp %in% c("<", ">")
    variance <- type == "F" && grepl(var_sentence, as.character(table$text[i] %||% ""), ignore.case = TRUE, perl = TRUE)
    if (!bound && !variance) next
    test_txt <- sub("^.*?[=<>]\\s*", "", sub(",\\s*[pP]\\s*[=<>].*$", "", raw), perl = TRUE)
    p_txt <- sub("^.*[pP]\\s*[=<>]\\s*", "", raw, perl = TRUE)
    d_t <- decimals(test_txt); d_p <- decimals(p_txt)
    half_t <- 0.5 * 10^-d_t
    # the statistic's possible values
    rng <- if (identical(tcomp, "<")) c(0, tv + half_t) else if (identical(tcomp, ">")) c(max(0, tv - half_t), Inf)
           else c(max(0, tv - half_t), tv + half_t)
    p_rng <- sort(c(p_of(type, df1, df2, rng[1]), p_of(type, df1, df2, rng[2])))
    if (variance) p_rng <- pmin(1, 2 * p_rng)
    comp <- as.character(table$p_comp[i])
    out[i] <- if (identical(comp, "<")) p_rng[1] < rp
              else if (identical(comp, ">")) p_rng[2] > rp
              else {
                half_p <- 0.5 * 10^-max(d_p, 1)
                p_rng[1] <= rp + half_p && p_rng[2] >= rp - half_p
              }
  }
  out
}

# Whether a paper says its tests are one-sided, anywhere in its text: statcheck
# reads one sentence at a time, and "All tests were one-sided" in the Method left
# the results later flagged (#695, point 5). A named logical by paper_id.
.paper_says_one_sided <- function(paper) {
  tx <- paper_table(paper, "text", cols = "text")
  if (!nrow(tx)) return(stats::setNames(logical(0), character(0)))
  says <- grepl(paste0("\\bone[- ]?(?:tailed|sided)\\b|\\bdirectional\\s+(?:tests?|hypothes[ie]s|predictions?)\\b|",
                       "\\b(?:tests?|hypothes[ie]s)\\s+(?:were|was)\\s+directional\\b"),
                tx$text, ignore.case = TRUE, perl = TRUE)
  out <- tapply(says, tx$paper_id, any)
  stats::setNames(as.logical(out), names(out))
}

# For statcheck's rows (t-tests, and F-tests with one degree of freedom in the
# numerator, which are a t squared): whether the reported p fits a one-sided test
# in either direction, within the rounding of the reported statistic and p:
# half the two-sided p, or 1 minus that half for an effect against the predicted
# direction (p = .973 for t = -1.93, mp20202474; #695, point 5). No two-sided p
# can reach the second value, so it hides no reporting error. NA where it does
# not apply.
.stat_one_sided_fits <- function(table) {
  n <- nrow(table)
  out <- rep(NA, n)
  if (!n) return(out)
  decimals <- function(s) {
    s <- sub("^[^0-9.]*", "", s)
    ifelse(grepl("\\.", s), nchar(sub("^[^.]*\\.", "", sub("[^0-9.].*$", "", s))), 0L)
  }
  for (i in seq_len(n)) {
    type <- table$test_type[i]
    df1 <- suppressWarnings(as.numeric(table$df1[i])); df2 <- suppressWarnings(as.numeric(table$df2[i]))
    tv <- abs(suppressWarnings(as.numeric(table$test_value[i])))
    rp <- suppressWarnings(as.numeric(table$reported_p[i]))
    if (is.na(tv) || is.na(rp) || is.na(df2) || !(type == "t" || (type == "F" && isTRUE(df1 == 1)))) next
    raw <- as.character(table$raw[i])
    test_txt <- sub("^.*?[=<>]\\s*", "", sub(",\\s*[pP]\\s*[=<>].*$", "", raw), perl = TRUE)
    p_txt <- sub("^.*[pP]\\s*[=<>]\\s*", "", raw, perl = TRUE)
    d_t <- decimals(test_txt); d_p <- decimals(p_txt)
    # the statistic's possible values, given its rounding, and the two-sided p
    lo <- max(0, tv - 0.5 * 10^-d_t); hi <- tv + 0.5 * 10^-d_t
    p2 <- if (type == "t") 2 * stats::pt(-c(hi, lo), df2) else
      stats::pf(c(hi, lo), 1, df2, lower.tail = FALSE)
    ranges <- list(p2 / 2, rev(1 - p2 / 2))
    comp <- as.character(table$p_comp[i])
    fits <- vapply(ranges, function(r) {
      if (identical(comp, "<")) return(r[1] < rp)
      if (identical(comp, ">")) return(r[2] > rp)
      half <- 0.5 * 10^-max(d_p, 1)
      r[1] <= rp + half && r[2] >= rp - half
    }, logical(1))
    out[i] <- any(fits)
  }
  out
}
