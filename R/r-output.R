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
  # A captured subprocess console can carry ANSI SGR colour codes (e.g.
  # effectsize/insight's cli-coloured footnotes: "\033[36m...\033[39m"), and
  # when the coloured text's own trailing newline is swallowed by the reset
  # escape, the NEXT statement's echoed "> " prompt lands glued onto the end of
  # that line instead of starting its own — confirmed against a real corpus
  # paper's cohens_d() output: "...pooled SD.\033[39m> FR_No <- t.test(...)".
  # .r_output_tables()'s data-gathering loop then reads that fused line as
  # part of the CURRENT result's data (it has no blank line to stop at),
  # pulling in the next statement's echoed source text as an extra "data" line
  # whose length has nothing to do with the table's real column width —
  # split_block()'s do.call(rbind, ...) then pads/rbinds lines of genuinely
  # different shape and warns "number of columns of result is not a multiple
  # of vector length". Splitting the prompt back onto its own line ONLY where
  # it was directly preceded by a colour-reset escape (not any "> "/"+ "
  # elsewhere in the text, which real output legitimately contains — e.g. a
  # printed "x > 5" or "3 + 2" — and must not be split) repairs exactly the
  # fusion this causes, before stripping the colour codes themselves.
  strip_ansi <- function(x) {
    x <- gsub("\033\\[[0-9;]*m(> |\\+ )", "\n\\1", x)
    gsub("\033\\[[0-9;]*m", "", x)
  }
  text <- if (length(text) == 1) strip_ansi(text) else
    vapply(text, strip_ansi, character(1), USE.NAMES = FALSE)
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
      .r_output_oneline(chunk_lines, source_label),
      .r_output_cohend(chunk_lines),
      .r_output_effectsize_d(chunk_lines)
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
                   "ansari.test", "mantelhaen.test", "poisson.test",
                   # effsize::cohen.d()'s printed "d" is ambiguous with
                   # ks.test()'s "D" without knowing the call — see
                   # .r_output_cohend() and .STATO_BY_CALL below.
                   "cohen.d",
                   # effectsize::cohens_d()/repeated_measures_d()'s printed "d"
                   # column is the same ambiguity — see .r_output_effectsize_d()
                   # and .STATO_BY_CALL below.
                   "cohens_d", "repeated_measures_d")
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

# The R object a statement is ABOUT — e.g. "m_vid" from `summary(m_vid)`,
# `stdCoef.merMod(m_vid)`, `r.squaredGLMM(m_vid)`, but also from shapes that
# used to fall through to NA (see each branch below): a bare print
# (`m_vid`), a pipe (`m_vid |> summary()`, `m_vid %>% eta_squared()`), a
# field/index access (`eta_squared(mc_model$anova_table$F[1])` ->
# "mc_model"), and an argument named or positioned anywhere other than
# first (`emmeans(specs = "team", object = mc_model)`). NA when assigning a
# NEW object (`m_vid <- lmer(...)`) or when no plain identifier is found
# anywhere recognised. This is the shared thread across the several
# SEPARATE statements a paper's one reported result often draws on together:
# a model is fit once (`m_vid <- lmer(...)`), then summarised, standardised
# and CI'd in further calls/prints that each name `m_vid` (or a value
# derived from it, e.g. `r2_vid <- r.squaredGLMM(m_vid)[1,2]` then
# `CI.Rsq(r2_vid, ...)`) — each such statement's OWN result rows are tagged
# with the same ref, so match_reported_output() can unite them into one
# candidate site.
#
# Confirmed as a real, live gap against a real corpus paper's script: an
# `aov_car()` model was fit once, then its OWN Anova table printed by
# bare `mc_model` (no call at all — case 1 below), its partial eta-squared
# printed via `eta_squared(mc_model)$Eta2` (already resolved before this
# fix), and its CI via `get.ci.partial.eta.squared(mc_model$anova_table$F[1],
# mc_model$anova_table$`num Df`[1], ..., conf.level = .95)` (a field/index
# expression, not a bare name — case 2 below). Of the three statements
# describing ONE model, only the middle one resolved before this fix, so
# the F/df/p table (bare print) and its own CI (field access) could never be
# united into one candidate site even though `eta_squared()`'s ref was
# right there — the F-table itself was the one link missing from the chain.
#
# Deliberately still narrow in what counts as "the same object": only a
# bare identifier (letters/digits/._), optionally followed by a `$`/`[`
# chain whose LEADING identifier is taken, never an arbitrary expression, a
# formula (contains "~"), a string, or a pure number, and never through a
# NESTED call (`summary(update(m_vid, ...))`, `eta_squared(car::Anova(
# mc_model))`) — the object reaching the outer function there is genuinely
# a NEW one (update()/Anova() return a different fitted object), so
# resolving through it would risk uniting two actually-different models
# under one ref, a false union that is worse than the status quo's false
# non-match (see this function's own callers: match_reported_output()'s
# whole design is built around never over-claiming a match). Left NA,
# same as an unresolvable case, rather than guessed either way.
.r_call_object_ref <- function(call_text) {
  if (is.null(call_text) || !nzchar(call_text)) return(NA_character_)
  ct <- trimws(call_text)
  # An assignment's target is a NEW object, not a reference to an existing
  # one — the calls this is meant to link (e.g. `summary(m_vid)`) never
  # assign, so excluding an assignment statement here avoids wrongly linking
  # `m_vid <- lmer(...)` to a LATER, unrelated statement that happens to also
  # name a bare-identifier object that is really a different assignment target.
  if (grepl("<-|=(?!=)", ct, perl = TRUE) &&
      grepl("^[A-Za-z._][A-Za-z0-9._]*\\s*(<-|=(?!=))", ct, perl = TRUE))
    return(NA_character_)

  # The leading identifier of a `$`/`[`-chain, e.g. "mc_model" from
  # "mc_model$anova_table$F[1]" or from a bare "mc_model" with no chain at
  # all. Used for every "is this text a reference to one object" check
  # below, so a field/index access is recognised the same way a bare name
  # already was, wherever it appears.
  leading_ident <- function(x) {
    x <- trimws(x)
    m <- regmatches(x, regexpr("^[A-Za-z._][A-Za-z0-9._]*", x, perl = TRUE))
    if (!length(m) || !nzchar(m)) return(NA_character_)
    # Only accept when the REST of the text (after the identifier) is
    # exclusively a `$name` / `[...]` / `[[...]]` chain — anything else
    # (an operator, a second identifier, a function call `ident(...)`)
    # means this was never a plain object reference to begin with.
    rest <- substr(x, nchar(m) + 1L, nchar(x))
    if (!nzchar(rest) || grepl("^(\\$[A-Za-z._][A-Za-z0-9._]*|\\[[^][]*\\]|\\[\\[[^][]*\\]\\])+$",
                              rest, perl = TRUE)) m else NA_character_
  }

  # Case 1: a bare print / pipe LHS — the whole statement (for a plain
  # print) or the text before the first `|>`/`%>%` (for a pipe) is itself
  # nothing but an object reference. Checked FIRST: `mc_model` alone has no
  # "(...)" at all for the case-2 scan below to find, and a pipe's true
  # subject sits OUTSIDE any parens entirely (`mc_model |> summary()` names
  # the object before the call, not inside it).
  pipe_split <- regmatches(ct, regexpr("\\s*(\\|>|%>%)\\s*", ct, perl = TRUE))
  lhs <- if (length(pipe_split) && nzchar(pipe_split))
    sub("\\s*(\\|>|%>%).*$", "", ct, perl = TRUE) else ct
  ref <- leading_ident(lhs)
  if (!is.na(ref)) return(ref)

  # Case 2: an argument inside the statement's FIRST top-level "(...)" —
  # covers a plain call (`summary(m_vid)`), a field/index access as that
  # argument (`get.ci.partial.eta.squared(mc_model$anova_table$F[1], ...)`),
  # and an argument at ANY position, named or positional
  # (`emmeans(specs = "team", object = mc_model)`), not just the first —
  # scanned with .repro_split_args() (reproducibility_check.R), which splits
  # on top-level commas only (respecting nested parens/brackets/quotes), so a
  # nested call's OWN commas do not fracture this call's argument list.
  # Deliberately does NOT look inside a NESTED call's own arguments
  # (`summary(update(m_vid, ...))`) — see this function's own header comment
  # for why resolving through one would be a guess, not a recovery.
  open_paren <- regexpr("(", ct, fixed = TRUE)
  if (open_paren == -1) return(NA_character_)
  # Balanced scan from the first "(" to its matching ")", so a multi-arg
  # call whose own arguments contain further parens is not truncated at the
  # first INNER ")" — the same "must remain balanced" reasoning
  # reproducibility_check.R's own call scanners already use.
  depth <- 0L; i <- open_paren; n <- nchar(ct); in_str <- NA_character_
  end <- NA_integer_
  while (i <= n) {
    chr <- substr(ct, i, i)
    if (!is.na(in_str)) {
      if (chr == "\\") i <- i + 1L
      else if (chr == in_str) in_str <- NA_character_
    } else if (chr %in% c("'", '"')) in_str <- chr
    else if (chr == "(") depth <- depth + 1L
    else if (chr == ")") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
    i <- i + 1L
  }
  if (is.na(end)) return(NA_character_)   # unbalanced — leave alone, do not guess
  args_text <- substr(ct, open_paren + 1L, end - 1L)
  args_list <- .repro_split_args(args_text)
  for (arg in args_list) {
    # Strip a leading `name = ` (named argument), same as
    # .repro_redirect_writes()'s own argument scan — but NOT `==`, a
    # comparison, which the negative lookahead excludes.
    val <- sub("^[.a-zA-Z][.a-zA-Z0-9_]*\\s*=\\s*(?!=)", "", trimws(arg$text), perl = TRUE)
    r <- leading_ident(val)
    if (!is.na(r)) return(r)
  }
  NA_character_
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

# ── effsize::cohen.d() ────────────────────────────────────────────────────────
# effsize::cohen.d()'s auto-printed result is neither a "<stat> <op> <value>"
# one-liner (.r_output_oneline) nor a header-plus-data-rows table
# (.r_output_tables): its point estimate line uses a COLON ("d estimate: 0.18
# (negligible)"), which neither parser's pattern matches at all, so a script
# that computes and PRINTS Cohen's d this way (a common idiom — an unassigned
# effsize::cohen.d(x, g) call, auto-printed by R's REPL) previously vanished
# from the extracted output entirely: confirmed against a real corpus paper
# where the value was verifiably computed and printed, yet absent from
# stat_output. Reproduced directly against a real effsize install; the print
# shape is:
#   Cohen's d
#
#   d estimate: 0.1811285 (negligible)
#   95 percent confidence interval:
#       lower     upper
#   -0.760357  1.122614
.r_output_cohend <- function(lines) {
  n <- length(lines)
  out <- list()
  for (i in seq_len(n)) {
    m <- regmatches(lines[[i]], regexec(
      "(?i)^\\s*d estimate:\\s*(-?[0-9.]+(?:e[-+]?[0-9]+)?)\\s*\\(([a-z]+)\\)\\s*$",
      lines[[i]], perl = TRUE))[[1]]
    if (length(m) < 3) next
    d_val <- m[[2]]; magnitude <- m[[3]]
    row <- stats::setNames(list(d_val), "d")
    # The CI, when present, is the next TWO lines: a "NN percent confidence
    # interval:" title, then a header/value pair ("    lower     upper" /
    # "-0.76  1.12") — R prints a NAMED NUMERIC VECTOR this way, header then
    # one data line, so a plain header/data column split (same fixed-width
    # logic .r_output_tables() uses) recovers both bounds.
    if (i + 2 <= n && grepl("(?i)confidence interval:\\s*$", lines[[i + 1]])) {
      hdr <- lines[[i + 2]]; dat <- if (i + 3 <= n) lines[[i + 3]] else ""
      hdr_toks <- strsplit(trimws(hdr), "\\s+")[[1]]
      dat_toks <- strsplit(trimws(dat), "\\s+")[[1]]
      if (length(hdr_toks) == 2 && length(dat_toks) == 2 &&
          all(grepl("(?i)^(lower|upper)$", hdr_toks)) &&
          all(grepl("^-?[0-9.]+(e[-+]?[0-9]+)?$", dat_toks, ignore.case = TRUE))) {
        row <- c(row, stats::setNames(as.list(dat_toks), tolower(hdr_toks)))
      }
    }
    df <- data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
    out[[length(out) + 1L]] <- list(
      analysis = "Cohen's d", title = sprintf("Cohen's d (%s)", magnitude),
      data = df)
  }
  out
}

# ── effectsize::cohens_d() / repeated_measures_d() / hedges_g() / glass_delta()
# These print a THREE-LINE pipe table that is neither a "<stat> <op> <value>"
# one-liner (.r_output_oneline) nor a normal header-plus-data-rows block
# (.r_output_tables can't see it as one: its data-gathering loop stops at the
# first line matching "^---", so the block's OWN dashed separator line — not a
# stray decoration, the table's actual header/body divider — is read as the
# END of the table before any data row is ever collected):
#   Cohen's d |        95% CI
#   -------------------------
#   -0.12     | [-0.73, 0.51]
#
#   - Estimated using pooled SD.
# repeated_measures_d()/hedges_g()/glass_delta() share this exact shape, only
# the header's first token differs ("d (rm)", "Hedges' g", "Glass' delta"),
# and the trailing footnote varies ("- Adjusted for small sample bias.",
# "- Deviation OMEGA...", none at all) — none of that changes the geometry
# being parsed, so one function covers all four. Reproduced directly against
# a real effectsize install; confirmed as the block that, before ANSI colour
# codes were stripped upstream (see read_r_output()'s header comment), could
# fuse with the NEXT echoed statement's prompt and corrupt split_block()'s
# column-boundary detection for a real corpus paper's cohens_d() calls.
.r_output_effectsize_d <- function(lines) {
  n <- length(lines)
  out <- list()
  i <- 1L
  while (i <= n) {
    hdr <- lines[[i]]
    m <- regmatches(hdr, regexec(
      "^\\s*(Cohen's d|d \\(rm\\)|Hedges'? g|Glass'? delta)\\s*\\|\\s*(?:[0-9]+% CI)?\\s*$",
      hdr, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(m) < 2 || i + 2 > n || !grepl("^-+$", lines[[i + 1]])) { i <- i + 1L; next }
    dat <- lines[[i + 2]]
    dm <- regmatches(dat, regexec(
      "^\\s*(-?[0-9.]+(?:e[-+]?[0-9]+)?)\\s*\\|\\s*\\[\\s*(-?[0-9.]+(?:e[-+]?[0-9]+)?)\\s*,\\s*(-?[0-9.]+(?:e[-+]?[0-9]+)?)\\s*\\]\\s*$",
      dat, perl = TRUE))[[1]]
    if (length(dm) < 4) { i <- i + 1L; next }
    label <- trimws(m[[2]])
    df <- data.frame(d = dm[[2]], ci_lower = dm[[3]], ci_upper = dm[[4]],
                     check.names = FALSE, stringsAsFactors = FALSE)
    out[[length(out) + 1L]] <- list(analysis = label, title = label, data = df)
    i <- i + 3L   # step past header, separator, data row
  }
  out
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
  # A cell like "2, 560" (a COMBINED df1/df2 pair, printed as one cell by
  # afex::aov_car()/aov_ez() and other ANOVA-table printers — confirmed
  # common, not a one-off) contains an internal space after the comma that
  # split_block()'s "blank in every line" gutter test cannot tell apart from
  # a real column boundary: the header ("df") is narrower than the data
  # ("2, 560"), so that internal space lines up with genuinely blank header
  # space above it and gets read as a gutter, fracturing the one cell into
  # two columns and shifting every later column over by one. Protected here
  # by swapping the comma-space for a non-breaking space (never blank under
  # `cc == " "`) before column splitting, then restored to a normal space in
  # the extracted cell text afterward — .split_combined_df() (below,
  # applied by the caller once the table is typed) parses the restored
  # "2, 560" into df1/df2.
  .NBSP <- " "
  protect_combined_df <- function(x)
    gsub("(?<=[0-9]),\\s+(?=[0-9])", paste0(",", .NBSP), x, perl = TRUE)
  restore_combined_df <- function(x) gsub(.NBSP, " ", x, fixed = TRUE)

  # A significance-code token (***/**/*/.) sitting as its own whitespace-
  # delimited run ANYWHERE in a data line — not just at the line's own end,
  # which is all the existing trailing-strip below ever covered. Sitting
  # mid-row (e.g. "41.86 *** .130", the stars between an F value and the
  # next real column) it occupies gutter space that a narrower header (a
  # bare "F") does not, so the true F/ges boundary is misjudged the same
  # way a combined df cell misjudges its own neighbour. R's own signif-code
  # alphabet, per stats:::printCoefmat's default: 0 '***' 0.001 '**' 0.01
  # '*' 0.05 '.' 0.1 ' ' 1. Blanked to EQUAL-WIDTH spaces (not deleted), so
  # column alignment is preserved and only that position's blank-ness
  # changes — deleting the token outright would shrink the line and merge
  # the columns on either side of it instead of separating them correctly.
  blank_sigcode <- function(x) {
    m <- gregexpr("(?<=\\s)(\\*{1,3}|\\.)(?=\\s)", x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(v) strrep(" ", nchar(v)))
    x
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
    block <- blank_sigcode(protect_combined_df(block))
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
      restore_combined_df(trimws(substr(padded, starts[k], ends[k]))))
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
      # A lone all-dash divider sitting DIRECTLY under the header (statsmodels'
      # OLS/Logit .summary(): "coef std err t P>|t| ..." header, then a "---"
      # rule, THEN the data rows) is a header/body divider, not R's own
      # footnote rule -- R's "---" only ever follows Signif. codes AFTER at
      # least one data row was already printed, never immediately under a
      # header with zero data collected yet. The stop condition below (`^---`)
      # exists specifically for that AFTER-data case, so it must not also fire
      # here or a statsmodels table is abandoned with zero data lines before
      # ever reaching its real content. Confirmed against real statsmodels
      # output from a Zenodo-sampled notebook (an OLS Regression Results
      # table whose coefficient block was silently dropped without this).
      if (j <= n && grepl("^-{3,}\\s*$", trimws(lines[[j]]))) j <- j + 1L
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
          # Mirror-image repair FIRST: a NARROW single-letter header ("F")
          # under WIDE data ("41.86", or a combined "2, 560" df cell wider
          # than its own "df" header) — the DATA spills past its narrow
          # header's own position into what looks like the PRECEDING column,
          # so the split lands the value in an ANONYMOUS column just before
          # the real header's own (now-empty-data) column. Symptom: a column
          # with an empty header but real data, immediately followed by a
          # column with a real header but all-empty data — splice them,
          # keeping the anonymous column's data under the following column's
          # header. Confirmed against a real corpus paper's aov_car() output
          # ("F" header over "41.86 ***" data — blank_sigcode() above already
          # removes the "***" so it cannot itself absorb the mismatch, but
          # the bare "F" header is still only one character wide against a
          # 5-character value). MUST run before the wide-header/narrow-data
          # repair below: that repair's own trigger (empty DATA, non-empty
          # header on both sides) also matches an UNFIXED narrow-header
          # column's real header sitting next to its own now-empty-until-
          # spliced data cell, and would wrongly glue two real headers
          # together (e.g. "F" + "ges" -> "F ges") before this repair ever
          # gets a chance to fill "F"'s data in from its neighbour.
          k <- 1L
          while (k < length(body)) {
            if (!nzchar(trimws(header[[k]])) && any(nzchar(trimws(body[[k]]))) &&
                all(!nzchar(trimws(body[[k + 1L]]))) && nzchar(trimws(header[[k + 1L]]))) {
              header[[k]] <- header[[k + 1L]]
              header <- header[-(k + 1L)]; body <- body[-(k + 1L)]
            } else k <- k + 1L
          }
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
