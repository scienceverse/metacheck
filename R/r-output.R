# Parse the CONSOLE OUTPUT of R analysis code into structured statistical result
# tables — the same tidy shape read_stat_tables() produces for JASP/jamovi, so it
# feeds the SAME STATO-typing + ISA-JSON pipeline (R/stato-map.R, R/stat-output.R).
#
# The reproducibility execute phase (repro_run_scripts) already RUNS each script
# and captures its stdout; this turns that captured text into results. R output
# comes in two shapes, handled separately then merged:
#   1. one-line tests  — "t = 2.34, df = 48, p-value = 0.02" (t.test, cor.test,
#      chisq.test, prop.test, shapiro.test). Parsed with the SAME statistic
#      pattern extract_eq() uses on manuscript prose (.r_stat_pattern).
#   2. text tables     — a fixed-width block like summary(lm)/aov/anova, with a
#      header row of statistic names and whitespace-aligned data rows.
#
# The regex in .r_stat_pattern mirrors extract_eq()'s (statistic name, optional
# (df), a comparator, a value); kept here as a shared helper so the paper-prose
# path and this output path stay in sync without duplication.

# The "<name> [(df)] <op> <value>" statistic pattern, shared with extract_eq().
.r_stat_pattern <- function() {
  operators <- c("=", "<", ">", "~", "≈", "≠", "≤", "≥",
                 "≪", "≫")
  op <- paste(operators, collapse = "")
  gr <- "Ͱ-Ͽ"
  list(op = op, pattern = paste0(
    "([", gr, "²a-zA-Z][", gr, "²a-zA-Z0-9._-]*)\\s*",  # statistic name
    "(\\([^)]*\\))?\\s*",                                          # optional (df)
    "([", op, "]{1,3})\\s*",                                      # comparator
    # value: a number (no trailing comma), scientific notation, or "< .001".
    "(<\\s*[.0-9]+|-?[0-9]+(?:\\.[0-9]+)?(?:e[-+]?[0-9]+)?)"
  ))
}

#' Extract statistical results from captured R console output
#'
#' Parses the text an R script prints (as captured from stdout) into tidy result
#' tables. Handles both one-line test output (`t.test`, `cor.test`, ...) and
#' fixed-width text tables (`summary(lm)`, `aov`, `anova`). The result matches the
#' shape of [read_stat_tables()], so it flows into the same STATO typing and
#' ISA-JSON export.
#'
#' @param text the captured console output: a character vector of lines, or a
#'   single string with embedded newlines
#' @param source_label optional label (e.g. the script name) recorded as the
#'   analysis for one-line results
#'
#' @returns a list with one element per detected result block, each a list of
#'   `analysis` (the test/section label), `title`, and `data` (a tidy
#'   data.frame) — the same structure [read_stat_tables()] returns.
#' @export
read_r_output <- function(text, source_label = NA_character_) {
  if (is.null(text)) return(list())
  lines <- if (length(text) == 1) strsplit(text, "\n", fixed = TRUE)[[1]] else text
  lines <- as.character(lines)
  if (!length(lines)) return(list())

  out <- c(
    .r_output_tables(lines),            # fixed-width text tables
    .r_output_oneline(lines, source_label)  # one-line "name = value" tests
  )
  Filter(function(t) !is.null(t) && nrow(t$data) > 0, out)
}

# ── One-line tests ────────────────────────────────────────────────────────────
# Find lines carrying "<stat> <op> <value>" fragments (often several per line,
# comma-separated: "t = 2.34, df = 48, p-value = 0.02"). A run of such fragments
# on nearby lines, under a test title line, becomes one result row.
.r_output_oneline <- function(lines, source_label) {
  pp <- .r_stat_pattern()
  # A "test title" line: R prints the test name centred/tabbed above the stats,
  # e.g. "\tWelch Two Sample t-test".
  title_re <- "(?i)(t-test|correlation|chi-squared|proportion|wilcoxon|shapiro|anova|fisher|kruskal|bartlett|mann-whitney|test)$"

  frags_on <- function(ln) {
    m <- regmatches(ln, gregexpr(pp$pattern, ln, perl = TRUE))[[1]]
    m
  }

  results <- list(); cur_title <- NA_character_; cur <- list()
  flush <- function() {
    if (!length(cur)) return(invisible())
    # one row: statistic -> value, from the collected fragments
    stat <- vapply(cur, `[[`, character(1), "stat")
    val  <- vapply(cur, `[[`, character(1), "val")
    df   <- data.frame(as.list(stats::setNames(val, make.unique(stat))),
                       check.names = FALSE, stringsAsFactors = FALSE)
    results[[length(results) + 1L]] <<- list(
      analysis = cur_title %||% (source_label %||% "R output"),
      title = cur_title %||% NA_character_, data = df)
  }

  for (ln in lines) {
    tl <- trimws(ln)
    if (grepl(title_re, tl) && !grepl(pp$pattern, tl, perl = TRUE)) {
      flush(); cur <- list(); cur_title <- tl
      next
    }
    fr <- frags_on(ln)
    if (length(fr)) {
      for (f in fr) {
        mm <- regmatches(f, regexec(pp$pattern, f, perl = TRUE))[[1]]
        if (length(mm) >= 5) {
          nm <- mm[[2]]; dfp <- mm[[3]]; val <- mm[[5]]
          # keep only recognisable statistic names (avoid matching prose)
          if (grepl("(?i)^(t|z|f|r|w|u|h|d|p|p-value|df|chi|x-squared|bf|rho|tau|s|v|estimate|mean)", nm)) {
            cur[[length(cur) + 1L]] <- list(stat = nm, val = val)
            if (nzchar(dfp)) cur[[length(cur) + 1L]] <-
              list(stat = "df", val = gsub("[()]", "", dfp))
          }
        }
      }
    }
  }
  flush()
  results
}

# ── Fixed-width text tables ───────────────────────────────────────────────────
# R prints tables FIXED-WIDTH, right-aligning each value under its header token:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.52206    0.18142   2.878  0.00596 **
# Gutters between columns can be a SINGLE space, and headers/values contain
# single spaces ("Std. Error", "Sum Sq", "< 2e-16"), so whitespace-splitting is
# unreliable. Instead we parse by CHARACTER POSITION: a table block is a header
# line plus contiguous data lines; the column boundaries are the character
# positions that are BLANK (space) in EVERY line of the block (header + data) —
# these vertical whitespace "rivers" separate the fixed-width columns. We split
# every line at those rivers, then trim. This is robust to 1-space gutters and
# multi-word cells because a genuine gutter is blank down the whole block while a
# within-cell space is not (some row has a character there).
.r_output_tables <- function(lines) {
  is_numlike <- function(x) {
    x <- trimws(x)
    grepl("^-?[0-9][0-9.]*(e[-+]?[0-9]+)?$", x, ignore.case = TRUE) |
    grepl("^[<>]\\s*-?[0-9.]+(e[-+]?[0-9]+)?$", x, ignore.case = TRUE) |
    grepl("(?i)^(inf|-?inf|na|nan|<\\s*2e-16)$", x)
  }
  is_quantile_hdr <- function(h) {
    h <- tolower(trimws(h)); h <- h[nzchar(h)]
    length(h) > 0 && all(h %in% c("min", "1q", "median", "3q", "max", "mean"))
  }
  # A candidate header line: has >=2 word-groups, mostly non-numeric, not prose
  # (no trailing ":" sentence, no "data:"/"alternative"/"Call"/"Signif").
  looks_header <- function(ln) {
    tl <- trimws(ln)
    if (!nzchar(tl)) return(FALSE)
    if (grepl("(?i)^(data:|alternative|signif|call|residual standard|multiple r-|--- *$|sample estimates|[0-9]+ (percent|observ))", tl))
      return(FALSE)
    grps <- strsplit(tl, "\\s+")[[1]]; grps <- grps[nzchar(grps)]
    length(grps) >= 2 && mean(is_numlike(grps)) < 0.4
  }
  # Split a set of block lines at columns blank in EVERY line.
  split_block <- function(block) {
    w <- max(nchar(block))
    padded <- formatC(block, width = -w, flag = "-")   # left-justify to width w
    chars <- do.call(rbind, strsplit(padded, "", fixed = TRUE))
    blank_col <- apply(chars, 2, function(cc) all(cc == " "))
    # column ranges = runs of NON-blank columns
    nb <- !blank_col
    if (!any(nb)) return(NULL)
    d <- diff(c(0L, as.integer(nb), 0L))
    starts <- which(d == 1L); ends <- which(d == -1L) - 1L
    lapply(seq_along(starts), function(k)
      trimws(substr(padded, starts[k], ends[k])))
  }

  n <- length(lines); i <- 1L; tables <- list(); section <- NA_character_
  while (i <= n) {
    tl <- trimws(lines[[i]])
    if (grepl("^[A-Za-z][A-Za-z0-9 .()|>-]*:$", tl)) section <- sub(":$", "", tl)

    if (looks_header(lines[[i]]) && !is_quantile_hdr(strsplit(tl, "\\s+")[[1]])) {
      # gather contiguous data lines (non-blank, containing a number, not prose)
      j <- i + 1L; data_lines <- character(0)
      while (j <= n) {
        dl <- lines[[j]]; dtl <- trimws(dl)
        if (!nzchar(dtl)) break
        if (grepl("(?i)^(---|signif|residual standard|multiple r-|f-statistic|call:|data:|alternative)", dtl)) break
        if (!grepl("[0-9]", dtl)) break
        # strip trailing significance codes so they don't form a column
        data_lines <- c(data_lines, sub("\\s+[*.]+\\s*$", "", dl))
        j <- j + 1L
      }
      if (length(data_lines) >= 1) {
        cols <- split_block(c(lines[[i]], data_lines))
        if (!is.null(cols) && length(cols) >= 2) {
          header <- vapply(cols, `[[`, character(1), 1L)         # first row = header
          body   <- lapply(cols, function(cl) cl[-1])            # rest = data
          # Repair a header word that a blank "river" wrongly split (e.g. a narrow
          # "F value" column: the value column is narrower than its 2-word header,
          # so a gutter appears mid-header). Symptom: a column whose DATA cells are
          # all empty and whose header is a bare word — merge it into the next
          # column's header. Only merges when the spurious column carries no data.
          k <- 1L
          while (k < length(body)) {
            if (all(!nzchar(trimws(body[[k]]))) && nzchar(trimws(header[[k]])) &&
                nzchar(trimws(header[[k + 1L]]))) {
              header[[k + 1L]] <- paste(trimws(header[[k]]), trimws(header[[k + 1L]]))
              header <- header[-k]; body <- body[-k]
            } else k <- k + 1L
          }
          df <- as.data.frame(body, stringsAsFactors = FALSE)
          nm <- trimws(header); nm[!nzchar(nm)] <- paste0("V", which(!nzchar(nm)))
          names(df) <- make.unique(nm)
          # keep only if the body actually has numeric cells (a real result table)
          if (any(vapply(df, function(c) any(is_numlike(c)), logical(1)))) {
            tables[[length(tables) + 1L]] <- list(
              analysis = section %||% NA_character_,
              title = section %||% NA_character_, data = df)
            i <- j; next
          }
        }
      }
    }
    i <- i + 1L
  }
  tables
}
