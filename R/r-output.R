# Parse the CONSOLE OUTPUT of R analysis code into structured statistical result
# tables — the same tidy shape read_stat_tables() produces for JASP/jamovi, so it
# feeds the SAME STATO-typing + statistical-output JSON pipeline (R/stato-map.R,
# R/stat-output.R).
#
# The reproducibility execute phase (repro_run_scripts) RUNS each script with
# source(script, echo = TRUE) and captures its stdout; the echo means the output
# is interleaved with "> <source statement>" / "+ <continuation>" prompt lines,
# one run of which precedes each top-level statement's output. .r_echo_chunks()
# splits that stream back into (source lines, output lines) per statement, and
# matches the source text against the script's own line-indexed text
# (code_lines) to recover the statement's starting line number — this is the
# only place a line number is ever attached to an extracted result. R output
# itself comes in two shapes, handled separately then merged:
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
#' Parses the text an R script prints (as captured from `source(script, echo =
#' TRUE)`) into tidy result tables, one per detected result block. Handles both
#' one-line test output (`t.test`, `cor.test`, ...) and fixed-width text tables
#' (`summary(lm)`, `aov`, `anova`). The result matches the shape of
#' [read_stat_tables()], so it flows into the same STATO typing and ISA-JSON
#' export.
#'
#' When `code_lines` is supplied (the script's own text, one element per source
#' line — what a run's `code_text_list[[file]]` already is), each result is
#' additionally tagged with the 1-based source `line` it came from, recovered by
#' matching the echoed `> `/`+ ` statement text against the script. A single
#' statement that prints several results (e.g. a `for` loop calling `t.test()`
#' each iteration) yields several results sharing one `line`; these are
#' distinguished by `line_seq` (1, 2, 3, ... within that line). Without
#' `code_lines` the echo is not parsed and `line`/`line_seq` are `NA`.
#'
#' @param text the captured console output: a character vector of lines, or a
#'   single string with embedded newlines
#' @param source_label optional label (e.g. the script name) recorded as the
#'   analysis for one-line results
#' @param code_lines optional character vector of the script's source lines (one
#'   element per line), used to recover the source `line` of each result via the
#'   echoed statement text. `NULL` (default) skips line attribution.
#'
#' @returns a list with one element per detected result block, each a list of
#'   `analysis` (the test/section label), `title`, `data` (a tidy data.frame),
#'   `line` (1-based source line, or `NA` when `code_lines` was not supplied),
#'   `line_seq` (1-based counter of results sharing that `line`, or `NA`), and
#'   `call_fn` (the recognised statistical function that produced the block,
#'   e.g. `"shapiro.test"`, or `""`) — the same structure [read_stat_tables()]
#'   returns, plus these three fields. `call_fn` lets [stato_type_column()]
#'   resolve statistic letters that are ambiguous in the printed output alone.
#' @export
read_r_output <- function(text, source_label = NA_character_, code_lines = NULL) {
  if (is.null(text)) return(list())
  lines <- if (length(text) == 1) strsplit(text, "\n", fixed = TRUE)[[1]] else text
  lines <- as.character(lines)
  if (!length(lines)) return(list())

  # Resolve "r2_vid" back to "m_vid" through an intermediate assignment
  # (`r2_vid <- r.squaredGLMM(m_vid)[1,2]`) — built once per script, from its
  # full source, since the chain can span lines far apart from either call
  # this links. NULL when code_lines is unavailable (no script text to trace).
  root_map <- if (!is.null(code_lines)) .r_root_ref_map(code_lines) else NULL
  resolve_ref <- function(ref) {
    if (is.na(ref)) return(ref)
    if (!is.null(root_map) && ref %in% names(root_map)) unname(root_map[[ref]])
    else ref
  }

  parse_chunk <- function(chunk_lines, line, call_text = "") {
    if (.r_is_preview_call(call_text)) return(list())
    out <- c(
      .r_output_tables(chunk_lines),
      .r_output_oneline(chunk_lines, source_label)
    )
    out <- Filter(function(t) !is.null(t) && nrow(t$data) > 0, out)
    fn <- .r_call_fn(call_text)
    ref <- resolve_ref(.r_call_object_ref(call_text))
    for (k in seq_along(out)) {
      out[[k]]$line <- line
      out[[k]]$line_seq <- k
      # The call that produced this block, when recognised — lets
      # stato_type_column() resolve statistic letters (W, S, V) that the
      # printed output alone leaves ambiguous.
      out[[k]]$call_fn <- fn
      # The fitted-model object this call operated on (e.g. "m_vid" from
      # `summary(m_vid)`), when recognisable — lets match_reported_output()
      # unite results from SEVERAL separate statements that all describe the
      # same model (summary() for p, a hand-written standardisation helper for
      # beta, r.squaredGLMM()/CI.Rsq() for R2) into one candidate site, since a
      # paper's single reported result routinely draws on all of them at once
      # while each individually only carries part of the signature.
      out[[k]]$model_ref <- ref
    }
    out
  }

  if (is.null(code_lines)) return(parse_chunk(lines, NA_integer_))

  chunks <- .r_echo_chunks(lines, code_lines)
  if (!length(chunks)) return(parse_chunk(lines, NA_integer_))
  unlist(lapply(chunks, function(ch) parse_chunk(ch$output, ch$line, ch$call)),
        recursive = FALSE, use.names = FALSE)
}

# Split echo = TRUE stdout into one chunk per top-level statement: a run of
# "> "/"+ " prompt lines (the echoed statement, possibly multi-line) followed by
# whatever it printed, up to the next "> " prompt or end of output. Comments and
# blank source lines are not echoed by source(), so this only ever sees
# statements that actually ran. Returns a list of list(line, output) — `line` is
# the 1-based position of the statement's FIRST line in `code_lines` (NA if no
# match), `output` the non-prompt lines that followed it.
.r_echo_chunks <- function(lines, code_lines) {
  is_prompt <- grepl("^(>|\\+) ?", lines)
  if (!any(is_prompt)) return(list())

  # A new CHUNK starts at every "> " line (a genuinely new top-level
  # statement); a "+ " line is a CONTINUATION of the statement already in
  # progress (a multi-line call/expression) and never starts one of its own.
  # The previous rule ("a prompt line preceded by a non-prompt line") could
  # not tell those apart: TWO SEPARATE one-line statements sitting back to
  # back with no output between them (e.g. an assignment that prints nothing,
  # immediately followed by the next statement's own "> " line) satisfied
  # "prompt line preceded by a prompt line" and so were wrongly fused into
  # ONE chunk — the first statement's (empty) output silently absorbed the
  # SECOND statement's entire printed result. That misattributed a table to
  # the wrong source line (and, worse, sometimes to an assignment target that
  # is not even a valid call, so no line at all): confirmed on real output
  # where "m_vid <- lmer(...)" (prints nothing) was immediately followed by
  # "summary(m_vid)" (prints the whole model summary) with no blank line
  # between their "> " prompts.
  is_new_stmt <- grepl("^> ?", lines)
  starts <- which(is_new_stmt)
  if (!length(starts)) return(list())
  ends <- c(starts[-1] - 1L, length(lines))

  norm <- function(x) trimws(x)
  code_norm <- norm(code_lines)
  # source(echo = TRUE) does not echo the statement's VERBATIM source text —
  # it deparses the parsed expression, which normalises whitespace (adds a
  # space around "<-", after every comma, ...). "r2_vid<-(f(x))[1,2]" in the
  # script is echoed as "r2_vid <- (f(x))[1, 2]". An EXACT match against
  # code_norm then fails for any statement whose author omitted those spaces,
  # so its `line` came back NA — and, worse, the corruption did not stop
  # there: on the file this was diagnosed against, that failure meant a
  # DIFFERENT statement's line number got attached to this chunk's output
  # instead of leaving it unattributed, misfiling a captured result under the
  # wrong source line entirely. Stripping ALL whitespace before comparing
  # (not just trimming ends) makes the match robust to deparse's added spaces
  # while still requiring every other character to agree.
  strip_ws <- function(x) gsub("[[:space:]]+", "", x)
  code_stripped <- strip_ws(code_norm)

  chunks <- lapply(seq_along(starts), function(k) {
    seg <- lines[starts[k]:ends[k]]
    prompt_n <- sum(grepl("^(>|\\+) ?", seg))
    stmt <- norm(sub("^(>|\\+) ?", "", seg[seq_len(prompt_n)]))
    output <- if (prompt_n < length(seg)) seg[(prompt_n + 1L):length(seg)] else character(0)
    first_stmt_line <- stmt[nzchar(stmt)][1]
    line <- if (!is.na(first_stmt_line) && length(first_stmt_line)) {
      m <- which(code_stripped == strip_ws(first_stmt_line))
      if (length(m)) m[[1]] else NA_integer_
    } else NA_integer_
    # Keep the echoed CALL, not just the line it was on. The printed output of
    # an htest has already discarded what produced it — a bare "W" is
    # Shapiro-Wilk's W after shapiro.test() but the rank sum after
    # wilcox.test(), and the printout cannot tell them apart. The call can, and
    # it costs nothing to carry since it was parsed here anyway to find `line`.
    list(line = line, call = paste(stmt, collapse = " "), output = output)
  })
  Filter(function(ch) length(ch$output) > 0, chunks)
}

# Which R function produced this chunk? Returns the first recognised
# statistical call in the echoed statement text, lower-cased, or "" when none
# is recognised. Deliberately a FIXED list of base/stats test functions whose
# printed statistic letter is ambiguous or whose identity aids typing — not a
# general R parser, and never a guess: an unrecognised call yields "", and the
# caller falls back to header-only typing.
.R_TEST_CALLS <- c("shapiro.test", "wilcox.test", "kruskal.test", "bartlett.test",
                   "fisher.test", "mcnemar.test", "chisq.test", "prop.test",
                   "t.test", "cor.test", "var.test", "binom.test",
                   "ks.test", "friedman.test", "mood.test", "fligner.test",
                   "ansari.test", "mantelhaen.test", "poisson.test")
# rstatix wraps several of the same base tests under its OWN underscore-named
# functions (t_test(), wilcox_test(), ...), printing a tidy tibble whose
# generic "statistic"/"p" columns are the SAME quantities the base test
# reports — but a literal "\\bt\\.test\\s*\\(" match never fires for
# "t_test(", so these were previously unrecognised calls, and the tibble's
# generic column names fell all the way through to untyped. Mapped to their
# base-R equivalent's key here (not duplicated in .STATO_BY_CALL) since the
# underlying statistic is identical either way.
.R_STATIX_ALIASES <- c(t_test = "t.test", wilcox_test = "wilcox.test",
                       kruskal_test = "kruskal.test", cor_test = "cor.test",
                       chisq_test = "chisq.test", prop_test = "prop.test",
                       var_test = "var.test", shapiro_test = "shapiro.test")
.r_call_fn <- function(call_text) {
  if (is.null(call_text) || !nzchar(call_text)) return("")
  ct <- tolower(call_text)
  hit <- .R_TEST_CALLS[vapply(.R_TEST_CALLS, function(f)
    grepl(paste0("\\b", gsub("\\.", "\\\\.", f), "\\s*\\("), ct, perl = TRUE),
    logical(1))]
  if (length(hit)) return(hit[[1]])
  rs <- names(.R_STATIX_ALIASES)[vapply(names(.R_STATIX_ALIASES), function(f)
    grepl(paste0("\\b", f, "\\s*\\("), ct, perl = TRUE), logical(1))]
  if (length(rs)) unname(.R_STATIX_ALIASES[[rs[[1]]]]) else ""
}

# Calls whose printed output is a DATA PREVIEW, never a statistical result —
# head()/tail()/str()/glimpse() of a data.frame, or just printing one, are
# tabular and often numeric, so the fixed-width table parser (.r_output_tables)
# would otherwise happily extract them as if they were a model's result table
# (seen on `head(df_sum_math)`: a row-number/factor-level preview parsed into
# spurious "V1"/"treat" statistic rows). A FIXED denylist, mirroring
# .R_TEST_CALLS's allowlist design: an unrecognised call is not skipped, so a
# real result is never dropped on a guess — only these specific, well-known
# inspection calls are excluded.
.R_PREVIEW_CALLS <- c("head", "tail", "str", "glimpse", "print", "view",
                     "dim", "names", "colnames", "rownames", "nrow", "ncol")
.r_is_preview_call <- function(call_text) {
  if (is.null(call_text) || !nzchar(call_text)) return(FALSE)
  ct <- tolower(call_text)
  any(vapply(.R_PREVIEW_CALLS, function(f)
    grepl(paste0("^(dplyr::|utils::|base::)?", f, "\\s*\\("), ct, perl = TRUE),
    logical(1)))
}

# The R object a statement's FIRST call operates on — e.g. "m_vid" from
# `summary(m_vid)`, `stdCoef.merMod(m_vid)`, or `r.squaredGLMM(m_vid)`; NA when
# assigning a NEW object (`m_vid <- lmer(...)`) or when no plain identifier is
# found. This is the shared thread across the several SEPARATE statements a
# paper's one reported result often draws on together: a model is fit once
# (`m_vid <- lmer(...)`), then summarised, standardised and CI'd in three
# further calls that each take `m_vid` (or a value derived from it, e.g.
# `r2_vid <- r.squaredGLMM(m_vid)[1,2]` then `CI.Rsq(r2_vid, ...)`) as their
# first argument — each such call's OWN result rows are tagged with the same
# ref, so match_reported_output() can unite them into one candidate site.
# Deliberately narrow: only a bare identifier (letters/digits/._), not an
# expression, a formula (contains "~"), a string, or a pure number — those are
# either not object references or too ambiguous to link two calls by.
.r_call_object_ref <- function(call_text) {
  if (is.null(call_text) || !nzchar(call_text)) return(NA_character_)
  ct <- trimws(call_text)
  # An assignment's target is a NEW object, not a reference to an existing
  # one — the two calls this is meant to link (e.g. `summary(m_vid)`) never
  # assign, so excluding an assignment statement here avoids wrongly linking
  # `m_vid <- lmer(...)` to a LATER, unrelated call that happens to also
  # assign a bare-identifier first argument to some other object.
  if (grepl("<-|=(?!=)", ct, perl = TRUE) &&
      grepl("^[A-Za-z._][A-Za-z0-9._]*\\s*(<-|=(?!=))", ct, perl = TRUE))
    return(NA_character_)
  # First (...) group after a function name, e.g. "m_vid" from
  # "stdCoef.merMod(m_vid)" or "m_vid" from "summary(m_vid)".
  m <- regmatches(ct, regexpr("(?<=\\()[^()]*(?=\\))", ct, perl = TRUE))
  if (!length(m) || !nzchar(m)) return(NA_character_)
  first_arg <- trimws(strsplit(m[[1]], ",")[[1]][1])
  if (grepl("^[A-Za-z._][A-Za-z0-9._]*$", first_arg)) first_arg else NA_character_
}

# Trace "r2_vid" back to "m_vid" through a script's own assignments, e.g.
# `r2_vid <- r.squaredGLMM(m_vid)[1,2]` records r2_vid -> m_vid; a later
# `CI.Rsq(r2_vid, ...)` then resolves to the SAME root as an earlier
# `summary(m_vid)`, letting match_reported_output() unite their results even
# though the calls that describe one fitted model never mention its name
# directly. Only a SIMPLE chain is traced (one identifier assigned from one
# call whose first argument is itself a bare identifier); an assignment whose
# right-hand side references no such identifier (`x <- 5`, `x <- read.csv(...)`)
# is simply absent from the map, so callers asking for it get the name back
# unresolved rather than a wrong guess.
#
# @param code_lines the script's own source lines (one element per line)
# @returns a named character vector, name = assigned variable, value = its
#   ROOT object (transitively resolved); empty (`character(0)`) when no chain
#   is found.
.r_root_ref_map <- function(code_lines) {
  lines <- trimws(as.character(code_lines %||% character(0)))
  assign_re <- "^([A-Za-z._][A-Za-z0-9._]*)\\s*(?:<-|=(?!=))\\s*(.*)$"
  hits <- regmatches(lines, regexec(assign_re, lines, perl = TRUE))
  direct <- stats::setNames(character(0), character(0))
  for (h in hits) {
    if (length(h) < 3) next
    lhs <- h[[2]]; rhs <- h[[3]]
    ref <- .r_call_object_ref(rhs)
    if (!is.na(ref) && !identical(ref, lhs)) direct[[lhs]] <- ref
  }
  if (!length(direct)) return(direct)
  # Resolve each entry to its transitive root (follow the chain until a name
  # is not itself a key in `direct`, or a cycle would loop forever).
  root_of <- function(x, seen = character(0)) {
    if (x %in% seen || !x %in% names(direct)) return(x)
    root_of(direct[[x]], c(seen, x))
  }
  vapply(names(direct), root_of, character(1)) |> stats::setNames(names(direct))
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
  # A tibble prints "# A tibble: 2 x 7" (or "2 × 7") above its real header —
  # a title line, not a header, but it has >=2 non-numeric word-groups so it
  # otherwise passes looks_header() and gets wrongly adopted as one (with zero
  # digit-bearing lines under it, since the REAL header follows next, so the
  # bogus "table" is abandoned — but that wastes the one attempt at the real
  # header entirely, see is_tibble_type_row() below for why that matters).
  is_tibble_title <- function(ln) grepl("^#\\s*A tibble", trimws(ln))
  # A tibble's own second header line: one type tag per column ("<chr>",
  # "<int>", "<dbl>", "<lgl>", "<fct>", "<date>", "<dttm>", "<list>", ...),
  # printed directly under the real column-name row. It has no digits, so the
  # data-gathering loop below (which requires a digit to accept a line as data)
  # would otherwise stop immediately after the real header — leaving zero data
  # lines, so the real header gets abandoned as a dead end, and iteration
  # resumes AT the type-tag row, which then gets wrongly adopted as the header
  # instead (its own next lines are real data rows, which do have digits).
  # Recognising it here lets the caller skip over it as part of the header
  # block, rather than letting it becomes a header of its own.
  is_tibble_type_row <- function(ln) {
    tl <- trimws(ln)
    if (!nzchar(tl)) return(FALSE)
    toks <- strsplit(tl, "\\s+")[[1]]; toks <- toks[nzchar(toks)]
    # rstatix (t_test(), anova_test(), ...) prints a leading "*" on the type
    # row of a grouped tibble, marking which row each output row belongs to
    # (its own row-identity column, printed as a bare number, "1", not a
    # header token) — drop it before the all-type-tags check, or the whole
    # row fails to register as a type row and gets read as real data instead
    # (surfacing as spurious "*"/"<chr>.1"/"<dbl>"/"<int>" "statistics").
    if (length(toks) > 0 && identical(toks[[1]], "*")) toks <- toks[-1]
    length(toks) > 0 &&
      all(grepl("^<(chr|int|dbl|lgl|fct|ord|date|dttm|list|cplx|raw)>$", toks))
  }
  # A candidate header line: has >=2 word-groups, mostly non-numeric, not prose
  # (no trailing ":" sentence, no "data:"/"alternative"/"Call"/"Signif").
  looks_header <- function(ln) {
    tl <- trimws(ln)
    if (!nzchar(tl)) return(FALSE)
    if (is_tibble_title(tl)) return(FALSE)
    if (grepl("(?i)^(data:|alternative|signif|call|residual standard|multiple r-|--- *$|sample estimates|[0-9]+ (percent|observ))", tl))
      return(FALSE)
    grps <- strsplit(tl, "\\s+")[[1]]; grps <- grps[nzchar(grps)]
    length(grps) >= 2 && mean(is_numlike(grps)) < 0.4
  }
  # Split a set of block lines at columns blank in EVERY line.
  split_block <- function(block) {
    # Expand literal tabs to spaces first: formatC()'s width padding is based
    # on nchar(), but a tab is ONE character to nchar() while the console that
    # originally printed it rendered it as several columns. Left un-expanded,
    # formatC() can pad two lines to the same nchar() while their strsplit()
    # results still differ in length (e.g. "a\tb" is 3 chars needing no pad
    # while "cd"/"ef" pad to 4), which throws off do.call(rbind, ...) below
    # with "number of columns of result is not a multiple of vector length".
    # 8-space tab stops match the common terminal default.
    block <- gsub("\t", strrep(" ", 8), block, fixed = TRUE)
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
      # A tibble's type-tag row sits directly under the real header; step past
      # it here so it is never mistaken for data (it has no digits) or, on a
      # later iteration, wrongly adopted as a header of its own — see
      # is_tibble_type_row()'s comment above.
      j <- i + 1L
      if (j <= n && is_tibble_type_row(lines[[j]])) j <- j + 1L
      # gather contiguous data lines (non-blank, containing a number, not prose)
      data_lines <- character(0)
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
