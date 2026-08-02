# Capture statistical RESULT OBJECTS from an executed R script, instead of
# parsing what those objects printed.
#
# read_r_output() (R/r-output.R) reads a script's captured stdout: fixed-width
# tables, htest printouts, regex over prose. That works, but the printed form
# has already discarded what the object knew — a bare "W" is Shapiro-Wilk's W
# after shapiro.test() and the rank sum after wilcox.test(), "< 2e-16" replaces
# the actual p, and a variable name in a column header is indistinguishable from
# a statistic name. The OBJECT carries all of it: an `htest` has `statistic`,
# `parameter`, `p.value`, `estimate` and `method` as named fields; a
# `summary.lm` has a real coefficients matrix with dimnames.
#
# So this runs inside the SUBPROCESS alongside the script (see
# repro_run_scripts()): a top-level task callback inspects the value of every
# top-level statement, keeps the ones that are recognised statistical results,
# reduces each to a plain list of named numbers, and appends it to a sidecar
# RDS. The parent reads that file back and converts it to the same tidy table
# shape read_stat_tables()/read_r_output() produce, so everything downstream —
# STATO typing, result_id, the native document — is unchanged.
#
# This is ADDITIVE: the stdout path still runs, and .r_merge_captures() prefers
# a captured object over a parsed block for the same source line. A script whose
# results are only printed (e.g. inside a loop with print()) still works via the
# text path, so nothing regresses when capture finds nothing.

# The classes worth capturing, and how to reduce each to named numbers. Kept as
# a FIXED list: an unrecognised class is ignored rather than guessed at.
.R_CAPTURE_CLASSES <- c("htest", "summary.lm", "anova", "summary.aov",
                        "summary.glm", "lm", "glm")

# One `htest` (t.test, cor.test, chisq.test, shapiro.test, wilcox.test, ...) ->
# list(analysis, call_fn, stats). `statistic` and `parameter` are NAMED in the
# object ("t", "df", "W", "X-squared"), which is exactly the header the printed
# form shows — but here we also know `method`, so the caller can disambiguate.
.r_cap_htest <- function(x) {
  st <- list()
  add <- function(nm, v) {
    if (is.null(v) || !length(v)) return()
    v <- v[!is.na(v)]
    if (!length(v)) return()
    nms <- names(v)
    for (i in seq_along(v)) {
      key <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else nm
      st[[key]] <<- unname(v[[i]])
    }
  }
  add("statistic", x$statistic)
  add("parameter", x$parameter)
  add("p", x$p.value)
  add("estimate", x$estimate)
  # A confidence interval is stored as a bare 2-vector with a conf.level attr.
  ci <- x$conf.int
  if (!is.null(ci) && length(ci) == 2) {
    st[["conf.low"]]  <- ci[[1]]
    st[["conf.high"]] <- ci[[2]]
  }
  if (!length(st)) return(NULL)
  list(analysis = as.character(x$method %||% "htest")[1],
       method   = as.character(x$method %||% "")[1],
       rows     = list(list(label = as.character(x$data.name %||% "")[1],
                            stats = st)))
}

# A coefficients matrix (summary.lm / summary.glm) -> one row per predictor,
# with the matrix's own column names as the statistic keys ("Estimate",
# "Std. Error", "t value", "Pr(>|t|)"), which the existing map already types.
.r_cap_coef_matrix <- function(cf, analysis) {
  if (is.null(cf) || !is.matrix(cf) || !nrow(cf)) return(NULL)
  cn <- colnames(cf); rn <- rownames(cf)
  if (is.null(cn)) return(NULL)
  rows <- lapply(seq_len(nrow(cf)), function(i) {
    st <- list()
    for (j in seq_along(cn)) {
      v <- cf[i, j]
      if (!is.na(v)) st[[cn[j]]] <- unname(v)
    }
    if (!length(st)) return(NULL)
    list(label = if (!is.null(rn)) rn[i] else as.character(i), stats = st)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  list(analysis = analysis, method = analysis, rows = rows)
}

# An anova / aov table is a data.frame whose columns are the statistics.
.r_cap_anova <- function(x, analysis) {
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!nrow(df) || !ncol(df)) return(NULL)
  cn <- names(df); rn <- rownames(df)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    st <- list()
    for (j in seq_along(cn)) {
      v <- suppressWarnings(as.numeric(df[i, j]))
      if (length(v) == 1 && !is.na(v)) st[[cn[j]]] <- v
    }
    if (!length(st)) return(NULL)
    list(label = if (!is.null(rn)) trimws(rn[i]) else as.character(i),
         stats = st)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  list(analysis = analysis, method = analysis, rows = rows)
}

# ── Identity from the CALL, for classless results ────────────────────────────
# Many R results carry no statistical identity in the OBJECT at all.
# aggregate(y ~ g, d, mean) and aggregate(y ~ g, d, sd) return byte-identical
# shapes — same class (data.frame), same dim, same names — so nothing in the
# value distinguishes a column of means from a column of SDs. The printed form
# is worse still: the column is headed with the VARIABLE's name ("wellbeing"),
# which is why such columns land in the export as junk "statistics".
#
# What does know is the CALL: the aggregating function is named right there, as
# `FUN`. So rather than special-casing aggregate(), this recovers `FUN` from any
# call that has one — the whole apply family (aggregate, tapply, sapply, vapply,
# lapply, by, mapply, Map, outer, ...) plus anything else whose formals name a
# FUN argument — via match.call(), which resolves positional, named and
# partially-matched arguments for free.
#
# The recovered function name is then looked up as a STATISTIC name through the
# ordinary vocabulary: `mean` types as sample mean, `sd` as standard deviation,
# `median` as the minted median term. A function that does not name a statistic
# (a user's own helper, an anonymous function) yields nothing and the value is
# left alone — the mechanism never invents an identity it cannot justify.
.r_call_fun_arg <- function(call_text) {
  if (is.null(call_text) || !nzchar(call_text)) return(NA_character_)
  cl <- tryCatch(str2lang(call_text), error = function(e) NULL)
  if (is.null(cl) || !is.call(cl)) return(NA_character_)
  fname <- tryCatch(as.character(cl[[1]])[1], error = function(e) NA_character_)
  if (is.na(fname) || !nzchar(fname)) return(NA_character_)
  f <- tryCatch(get(fname, mode = "function"), error = function(e) NULL)
  if (is.null(f)) return(NA_character_)

  pick <- function(m) {
    if (is.null(m)) return(NULL)
    fun <- m[["FUN"]]
    if (is.null(fun)) return(NULL)
    if (is.name(fun)) return(as.character(fun))
    if (is.character(fun) && length(fun)) return(fun[[1]])
    NULL          # anonymous function: no name to map, so no identity
  }
  hit <- pick(tryCatch(match.call(f, cl), error = function(e) NULL))
  if (!is.null(hit)) return(hit)

  # A GENERIC (aggregate, summary) has formals (x, ...), so a positionally
  # passed FUN stays inside `...` and match.call cannot see it. Retry against
  # the S3 methods, whose formals do name FUN.
  meths <- tryCatch(as.character(utils::methods(fname)),
                    error = function(e) character(0))
  for (mn in meths) {
    short <- sub(paste0("^", fname, "\\."), "", mn)
    mf <- tryCatch(utils::getS3method(fname, short), error = function(e) NULL)
    if (is.null(mf) || !("FUN" %in% names(formals(mf)))) next
    hit <- pick(tryCatch(match.call(mf, cl), error = function(e) NULL))
    if (!is.null(hit)) return(hit)
  }
  NA_character_
}

# A rectangular, classless result (aggregate/tapply/sapply output) whose
# statistic identity comes from the call's FUN. `stat_name` is that function's
# name; every non-grouping column is reported as that statistic, with the
# grouping column(s) becoming the row label.
.r_cap_by_fun <- function(v, stat_name, max_rows = 40L) {
  df <- tryCatch(as.data.frame(v, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || !nrow(df) || ncol(df) < 1) return(NULL)
  nms <- names(df)
  if (is.null(nms) || !length(nms)) return(NULL)
  # Guard against a call that did not actually AGGREGATE. sapply(d[, "y"], mean)
  # silently drops to a vector and maps mean over each of 40 individual numbers,
  # returning the raw data unchanged; capturing that would report 40 "sample
  # means" that are really observations. A genuine aggregate identifies its
  # groups — either a non-numeric grouping column, or names/rownames (tapply and
  # sapply name their result by group). A many-rowed result with NO group
  # identity is the degenerate case, and is refused.
  has_label_col <- any(!vapply(df, function(c) is.numeric(c) || is.integer(c),
                               logical(1)))
  rn <- rownames(df)
  # Default rownames are the positional "1","2","3"; a real aggregate's are the
  # group levels ("a","b") or the aggregated variables ("y","z").
  named_rows <- !is.null(rn) &&
    !identical(as.character(rn), as.character(seq_len(nrow(df))))
  if (!has_label_col && !named_rows) return(NULL)
  if (nrow(df) > max_rows) return(NULL)
  # Numeric columns hold the aggregated values; the rest identify the group.
  is_num <- vapply(df, function(c) is.numeric(c) || is.integer(c), logical(1))
  if (!any(is_num)) return(NULL)
  lab_cols <- which(!is_num); val_cols <- which(is_num)

  rows <- lapply(seq_len(nrow(df)), function(i) {
    lab <- if (length(lab_cols))
      paste(trimws(as.character(unlist(df[i, lab_cols, drop = TRUE]))),
            collapse = " ") else rownames(df)[i] %||% as.character(i)
    st <- list()
    for (j in val_cols) {
      x <- suppressWarnings(as.numeric(df[i, j]))
      if (length(x) == 1 && !is.na(x)) {
        # Key on the FUN name, not the column's own (variable) name: THAT is
        # the statistic. Several value columns keep the variable in the key so
        # they stay distinct.
        key <- if (length(val_cols) > 1)
          paste0(stat_name, " (", nms[j], ")") else stat_name
        st[[key]] <- x
      }
    }
    if (!length(st)) return(NULL)
    list(label = lab, stats = st)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  list(analysis = paste0(stat_name, " by group"), method = "", rows = rows)
}

# Reduce ONE top-level value to a capture record, or NULL when it is not a
# recognised statistical result. `call_text` is the statement that produced the
# value, used only when the value's own class carries no identity.
.r_capture_value <- function(v, call_text = NULL) {
  if (is.null(v)) return(NULL)

  if (inherits(v, "htest")) return(.r_cap_htest(v))

  if (inherits(v, "summary.lm") || inherits(v, "summary.glm")) {
    an <- if (inherits(v, "summary.glm")) "glm" else "lm"
    return(.r_cap_coef_matrix(v$coefficients, an))
  }
  # A bare lm/glm printed at top level carries no test statistics of its own;
  # summary() is what reports them, so the model object itself is skipped.

  if (inherits(v, "anova") || inherits(v, "summary.aov"))
    return(.r_cap_anova(if (is.list(v) && !is.data.frame(v) && length(v))
                          v[[1]] else v, "anova"))

  # LAST RESORT: the value's class said nothing, so ask the CALL. This is where
  # aggregate()/tapply()/sapply() results are typed — from their FUN — and it
  # generalises to any call naming a FUN whose name is a known statistic.
  # Deliberately last: a classed object always knows better than the call.
  fun <- .r_call_fun_arg(call_text)
  if (!is.na(fun) && nzchar(fun)) {
    # Only when the function NAMES a statistic the vocabulary recognises;
    # a user's own helper yields nothing rather than a fabricated identity.
    if (nzchar(stato_type_column(fun)$termSource))
      return(.r_cap_by_fun(v, fun))
  }

  NULL
}

# The reducer functions the child needs, as a plain named list. callr serialises
# these into the subprocess, where they are assigned into globalenv() — the
# child has no access to metacheck's namespace, so everything the callback
# calls must travel with it. `%||%` is included because the reducers use it.
.r_capture_helpers <- function() {
  # The call-based fallback asks stato_type_column() whether a FUN name is a
  # known statistic, so the vocabulary has to travel too. Sending the lookup
  # TABLES (plain vectors) plus the function keeps the child self-contained
  # without it needing metacheck installed.
  stato_tables <- list(map = .STATO_MAP, labels = .STATO_LABELS,
                       mc_map = .MC_STAT_MAP, mc_labels = .MC_STAT_LABELS,
                       mc_ns = .MC_STAT_NS, by_call = .STATO_BY_CALL)
  child_type <- function(header, call_fn = NULL) {
    key <- tolower(trimws(header %||% ""))
    if (!nzchar(key)) return(list(termSource = ""))
    keys <- unique(c(key, sub("\\[[^]]*\\]$", "", key)))
    for (k in keys) {
      if (k %in% names(stato_tables$map)) return(list(termSource = "STATO"))
      if (k %in% names(stato_tables$mc_map)) return(list(termSource = "metacheck"))
    }
    list(termSource = "")
  }
  list(
    `%||%`             = function(a, b) if (is.null(a)) b else a,
    .r_cap_htest       = .r_cap_htest,
    .r_cap_coef_matrix = .r_cap_coef_matrix,
    .r_cap_anova       = .r_cap_anova,
    .r_call_fun_arg    = .r_call_fun_arg,
    .r_cap_by_fun      = .r_cap_by_fun,
    stato_type_column  = child_type,
    .r_capture_value   = .r_capture_value)
}

# Build the code that runs INSIDE the subprocess. Returns a function suitable
# for callr::r().
#
# It does NOT use source(): addTaskCallback() sees only the value of the
# source() call itself, not of each statement inside it (verified — the callback
# fires, but every value it receives is source()'s own return, never the htest
# object a line produced), so a callback cannot capture per-statement results.
#
# Instead the script is parsed and its top-level expressions evaluated one at a
# time, which is what source(echo = TRUE) does anyway. That gives direct access
# to each statement's VALUE (the object to capture) and its `srcref` (the source
# line, matching what the echo-based text path recovers). The echo is reproduced
# faithfully — "> " before each statement, autoprinting of visible values — so
# stdout is byte-comparable with the previous behaviour and read_r_output() is
# unaffected.
.r_capture_runner <- function() {
  function(script, wd, capture_file, helpers) {
    setwd(wd)
    # Recreate the reducer helpers in the child (they cannot be referenced from
    # the parent's namespace inside callr).
    for (nm in names(helpers)) assign(nm, helpers[[nm]], envir = globalenv())
    captures <- list()
    # Always leave a file behind, even on error/timeout, so the parent can read
    # whatever was captured before the script died.
    on.exit(try(saveRDS(captures, capture_file), silent = TRUE), add = TRUE)

    exprs <- parse(script, keep.source = TRUE)
    srcrefs <- attr(exprs, "srcref")
    env <- globalenv()

    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      sr <- if (!is.null(srcrefs) && length(srcrefs) >= i) srcrefs[[i]] else NULL
      # Echo the statement exactly as source(echo = TRUE) would, so the text
      # parser still sees the "> " prompts it keys line attribution on.
      txt <- if (!is.null(sr)) as.character(sr) else deparse(e)
      cat(paste0("> ", txt, collapse = "\n"), "\n", sep = "")

      res <- withVisible(eval(e, envir = env))
      if (res$visible) {
        print(res$value)          # autoprint, as the REPL/source(echo=) does
      }
      call_txt <- paste(txt, collapse = " ")
      rec <- tryCatch(.r_capture_value(res$value, call_txt),
                      error = function(e) NULL)
      if (!is.null(rec)) {
        rec$line <- if (!is.null(sr)) as.integer(sr[1L]) else NA_integer_
        rec$call_text <- call_txt
        captures[[length(captures) + 1L]] <- rec
      }
    }
    invisible(NULL)
  }
}

# Turn the sidecar capture records into the tidy table shape the rest of the
# pipeline consumes (the same fields read_r_output() returns), so captured
# objects and parsed text are interchangeable downstream.
#
# @param code_lines the script's own source lines, for resolving `model_ref`
#   (which fitted-model object each capture's call operated on, through simple
#   assignment chains — see .r_call_object_ref()/.r_root_ref_map() in
#   R/r-output.R) the SAME way read_r_output() does for the console-text path.
#   Without it every model_ref here is NA, which is what silently happened
#   before this parameter existed: a captured result (e.g. CI.Rsq()'s object,
#   captured directly rather than parsed from printed text) never got tagged,
#   so match_reported_output() could not unite it with a text-parsed sibling
#   result (e.g. summary(m_vid)'s p-value) that WAS tagged — two halves of one
#   reported test, only one of which was ever findable as "the same site".
.r_captures_to_tables <- function(caps, source_label = NA_character_,
                                  code_lines = NULL) {
  if (is.null(caps) || !length(caps)) return(list())
  root_map <- if (!is.null(code_lines)) .r_root_ref_map(code_lines) else NULL
  resolve_ref <- function(ref) {
    if (is.na(ref)) return(ref)
    if (!is.null(root_map) && ref %in% names(root_map)) unname(root_map[[ref]])
    else ref
  }
  out <- list()
  for (rec in caps) {
    if (is.null(rec$rows) || !length(rec$rows)) next
    # One tidy table per capture: rows are the record's rows, columns the union
    # of their statistic keys (a coefficients matrix shares keys across rows).
    keys <- unique(unlist(lapply(rec$rows, function(r) names(r$stats)),
                          use.names = FALSE))
    if (!length(keys)) next
    df <- data.frame(label = vapply(rec$rows, function(r)
      as.character(r$label %||% ""), character(1)),
      stringsAsFactors = FALSE, check.names = FALSE)
    for (k in keys) {
      df[[k]] <- vapply(rec$rows, function(r) {
        v <- r$stats[[k]]
        if (is.null(v)) "" else .stat_num_to_chr(as.numeric(v))
      }, character(1))
    }
    out[[length(out) + 1L]] <- list(
      analysis = rec$analysis %||% NA_character_,
      title    = rec$analysis %||% NA_character_,
      data     = df,
      line     = rec$line %||% NA_integer_,
      line_seq = 1L,
      # The method string an htest carries ("Shapiro-Wilk normality test") is
      # more reliable than re-deriving the function from the call text.
      call_fn  = .r_method_to_fn(rec$method %||% ""),
      model_ref = resolve_ref(.r_call_object_ref(rec$call_text %||% "")),
      captured = TRUE)
  }
  # Number results sharing a source line, as read_r_output() does.
  if (length(out)) {
    lines <- vapply(out, function(x) as.integer(x$line %||% NA_integer_), integer(1))
    for (i in seq_along(out)) {
      same <- which(lines == lines[i] & !is.na(lines))
      out[[i]]$line_seq <- match(i, same)
    }
  }
  out
}

# Merge captured-object tables with text-parsed tables for ONE script.
#
# Both describe the same run, so the same result can appear twice: once as the
# object a statement returned and once as what that statement printed. A capture
# is strictly better where it exists (exact values, unambiguous statistic
# identity), so it WINS for any source line it covers, and the text tables for
# that line are dropped. Text tables on lines with no capture are kept — a
# result printed inside a loop, or by a function whose class the capture list
# does not recognise, is only ever visible in the console output.
#
# Tables with an unknown line (NA) cannot be matched either way, so text tables
# with no line are kept only when there are no captures at all; otherwise they
# would duplicate captured results without any way to tell.
.r_merge_captures <- function(cap_tabs, txt_tabs) {
  cap_tabs <- cap_tabs %||% list()
  txt_tabs <- txt_tabs %||% list()
  if (!length(cap_tabs)) return(txt_tabs)
  if (!length(txt_tabs)) return(cap_tabs)

  cap_lines <- unique(stats::na.omit(vapply(cap_tabs, function(x)
    as.integer(x$line %||% NA_integer_), integer(1))))

  keep_txt <- vapply(txt_tabs, function(x) {
    ln <- as.integer(x$line %||% NA_integer_)
    if (is.na(ln)) return(FALSE)   # unmatchable while captures exist
    !(ln %in% cap_lines)
  }, logical(1))

  out <- c(cap_tabs, txt_tabs[keep_txt])
  # Re-number results sharing a line, so result_id's `_<line_seq>` stays
  # correct after the merge.
  lines <- vapply(out, function(x) as.integer(x$line %||% NA_integer_), integer(1))
  for (i in seq_along(out)) {
    same <- which(lines == lines[i] & !is.na(lines))
    out[[i]]$line_seq <- if (length(same)) match(i, same) else 1L
  }
  out
}

# Map an htest `method` string onto the function that produced it, so
# stato_type_column()'s call-aware disambiguation works for captured objects
# exactly as it does for the text path.
.r_method_to_fn <- function(method) {
  m <- tolower(as.character(method %||% ""))
  if (!nzchar(m)) return("")
  if (grepl("shapiro", m)) return("shapiro.test")
  if (grepl("wilcoxon|mann-whitney", m)) return("wilcox.test")
  if (grepl("kruskal", m)) return("kruskal.test")
  if (grepl("bartlett", m)) return("bartlett.test")
  if (grepl("fisher", m)) return("fisher.test")
  if (grepl("mcnemar", m)) return("mcnemar.test")
  if (grepl("chi-squared|chi-square", m)) return("chisq.test")
  if (grepl("proportion", m)) return("prop.test")
  if (grepl("kolmogorov", m)) return("ks.test")
  if (grepl("friedman", m)) return("friedman.test")
  if (grepl("f test", m)) return("var.test")
  if (grepl("correlation", m)) return("cor.test")
  if (grepl("t-test", m)) return("t.test")
  ""
}
