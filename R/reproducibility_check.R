# Helpers for the reproducibility_check module. This module answers "can the
# paper's code be run on its data?" in two phases.
#
# STATIC helpers (never run downloaded code): collect the dependencies a run
# would need, rewrite each script's file paths to the Psych-DS layout the release
# uses, work out the order the scripts must run in, and diagnose why a referenced
# input is unavailable. Built on the static analysis already in code_check.R
# (code_library_names, code_file_refs, code_parse_r) and the file→location plan
# from psychds_check / data_check.
#
# EXECUTION helpers (run only when the module is called with execute = TRUE):
# repro_materialize_layout / repro_write_scripts build a throwaway copy of the
# Psych-DS layout, repro_install_deps installs the declared packages into a
# throwaway library, and repro_run_scripts runs each script in an isolated
# subprocess (callr) with a per-script timeout, capturing its outcome and output.
# Running downloaded code is a deliberate, opt-in action; see those functions.

#' Collect the package dependencies a set of code files declare
#'
#' Walks each code file with [code_library_names()] and returns the distinct
#' packages the paper's code loads, tagged with how each was referenced and
#' whether it is installable from CRAN, from a named GitHub/URL source, or is a
#' base/recommended package already shipped with R (so a run need not install
#' it). This is the input list a run would install; it is deliberately
#' name-only, because static analysis cannot know which *version* was used —
#' running against the latest packages and observing a break is itself a
#' finding (it argues the authors should have pinned versions).
#'
#' GitHub/URL sources are read from the code text, not the package name: a call
#' `remotes::install_github("user/repo")` names the source `user/repo`, which is
#' matched back to the package by the repo's basename. When a script pins a ref
#' (`user/repo@v1.2`), the ref is kept.
#'
#' @param code_text the code text for a single file (character vector), OR a list
#'   of such vectors (one per file) to pool across files
#' @param lang the language (only R declares installable packages here)
#'
#' @returns a data frame with columns `package`, `source` (`cran`, `github`,
#'   `url`, or `base`), `ref` (the source path/ref for github/url, else NA), and
#'   `base` (logical, TRUE for a base/recommended package). One row per distinct
#'   package. Empty frame (same columns) when none are found.
#' @export
#'
#' @examples
#' code_text <- c(
#'   "library(dplyr)",
#'   "remotes::install_github('tidyverse/ggplot2@v3.4.0')",
#'   "x <- stats::sd(1:10)"
#' )
#' repro_dependencies(code_text)
repro_dependencies <- function(code_text, lang = "R") {
  empty <- data.frame(package = character(0), source = character(0),
                      ref = character(0), base = logical(0))
  if (is.null(code_text)) return(empty)

  # Pool a list of per-file texts into one search, keeping the union of names.
  if (is.list(code_text)) {
    parts <- lapply(code_text, repro_dependencies, lang = lang)
    parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
    if (length(parts) == 0) return(empty)
    out <- dplyr::bind_rows(parts)
    # A package named both bare (library) and via a github source: keep the
    # github row (it carries the real install source), drop the duplicate.
    out <- out[order(out$package, match(out$source, c("github", "url", "cran", "base"))), ]
    out[!duplicated(out$package), , drop = FALSE]
  } else {
    if (!identical(lang, "R")) return(empty)

    # Package names loaded/installed, from the shared static extractor.
    names_df <- code_library_names(code_text, "R")
    pkgs <- unique(names_df$package)
    if (length(pkgs) == 0) return(empty)

    # GitHub / URL install sources named in the text, matched to a package by
    # the repo basename. install_github("user/repo@ref") -> package "repo".
    joined <- paste(code_text, collapse = "\n")
    gh_pat  <- "install_github\\s*\\(\\s*['\"]([^'\"]+)['\"]"
    url_pat <- "install\\.packages\\s*\\(\\s*['\"](https?://[^'\"]+)['\"]"
    gh  <- regmatches(joined, gregexpr(gh_pat,  joined, perl = TRUE))[[1]]
    url <- regmatches(joined, gregexpr(url_pat, joined, perl = TRUE))[[1]]
    gh_refs  <- sub(gh_pat,  "\\1", gh,  perl = TRUE)
    url_refs <- sub(url_pat, "\\1", url, perl = TRUE)
    # repo basename (drop owner and @ref) is the installed package name
    gh_pkg  <- sub("@.*$", "", basename(gh_refs))
    url_pkg <- sub("[_.].*$", "", basename(url_refs))   # pkg_1.0.tar.gz -> pkg

    base_pkgs <- .repro_base_packages()

    src <- rep("cran", length(pkgs))
    ref <- rep(NA_character_, length(pkgs))
    src[pkgs %in% base_pkgs] <- "base"
    for (k in seq_along(gh_pkg)) {
      hit <- pkgs == gh_pkg[k]
      if (any(hit)) { src[hit] <- "github"; ref[hit] <- gh_refs[k] }
    }
    for (k in seq_along(url_pkg)) {
      hit <- pkgs == url_pkg[k]
      if (any(hit)) { src[hit] <- "url"; ref[hit] <- url_refs[k] }
    }

    data.frame(package = pkgs, source = src, ref = ref,
               base = pkgs %in% base_pkgs)
  }
}

# The base + recommended packages shipped with R. A run never needs to install
# these, so they are tagged `base` and excluded from the install set. Resolved
# from the running R installation so it stays correct across R versions.
.repro_base_packages <- function() {
  ip <- tryCatch(utils::installed.packages(priority = c("base", "recommended")),
                 error = function(e) NULL)
  if (is.null(ip)) c("base", "methods", "utils", "stats", "graphics",
                     "grDevices", "datasets", "tools")
  else rownames(ip)
}

#' Find file paths built at runtime with sprintf()/paste()/paste0()/file.path()
#'
#' A path is sometimes not written as a literal string but assembled from
#' variables — `read_csv(sprintf("%s/%s/x.csv", wd, wd_data))`. This is fragile
#' authoring (it silently breaks whenever the code runs from a different
#' working directory than the author's own), and it also defeats
#' [code_file_refs()]'s literal-string extraction: the "reference" it returns
#' is really the RAW FORMAT STRING (`"%s/%s/x.csv"`, placeholders and all), so a
#' naive substring replacement of that text later corrupts the call — the
#' surrounding `sprintf(...)` keeps its now-meaningless trailing arguments,
#' `, wd, wd_data)`, uselessly attached after the plan's target path was
#' spliced in as if it were the whole first argument.
#'
#' This finds every such call in a script, and reports enough to fix it
#' properly: the call's own full text (so the whole expression, not just the
#' quoted substring, can be replaced), the raw format string, and — when every
#' placeholder's corresponding argument is a simple variable resolvable to a
#' literal string via [.repro_simple_string_vars()] — the format string with
#' those values substituted in, which is what basename-matching should key on
#' instead of the placeholder-laden original.
#'
#' Only `%s`/`%d` placeholders (the common case in practice; a `sprintf` with a
#' width/precision modifier like `%05d` is deliberately out of scope, since a
#' zero-padded numeric ID is unlikely to appear in a bare data-file reference)
#' are substituted; anything else in the format leaves that call unresolved
#' (returned with `resolved = NA`), never guessed. The arguments after the
#' format string are split on a plain comma, so an argument that is ITSELF a
#' call containing a comma (`sprintf("%s.csv", paste0(a, b))`) is not split
#' correctly either — this only means that argument then fails to resolve to
#' any known variable (falling back to `resolved = NA`, the same safe
#' unresolved outcome as any other unrecognised argument), never a wrong guess.
#'
#' @param code_text the code text for a single file (character vector)
#'
#' @returns a data frame with `call_text` (the full matched call, e.g.
#'   `sprintf("%s/%s/x.csv", wd, wd_data)`), `fmt` (the raw format string),
#'   `resolved` (the format string with resolvable placeholders substituted, or
#'   NA if any placeholder could not be resolved), and `line` (1-based line
#'   number the call starts on). Empty frame when the script has no such calls.
#' @keywords internal
.repro_format_call_refs <- function(code_text) {
  empty <- data.frame(call_text = character(0), fmt = character(0),
                      resolved = character(0), line = integer(0))
  if (is.null(code_text) || length(code_text) == 0) return(empty)
  nc <- code_remove_comments(code_text, "R")
  joined <- paste(nc, collapse = "\n")

  # A call to one of the format/join functions, containing a quoted string with
  # a file-like extension somewhere in its arguments. Matched with a manual
  # balanced-paren scan (not a single regex) because the argument list can
  # itself contain nested parens (e.g. another function call as an argument).
  fn_pat <- "\\b(sprintf|paste0|paste|file\\.path)\\s*\\("
  starts <- gregexpr(fn_pat, joined, perl = TRUE)[[1]]
  if (length(starts) == 1 && starts == -1) return(empty)
  lens <- attr(starts, "match.length")

  rows <- lapply(seq_along(starts), function(k) {
    open_paren <- starts[k] + lens[k] - 1L   # index of the "(" itself
    # Balanced scan from the "(" to its matching ")", respecting quotes so a
    # ")" inside a quoted string is not mistaken for the call's own close.
    depth <- 0L; i <- open_paren; n <- nchar(joined); in_str <- NA_character_
    end <- NA_integer_
    while (i <= n) {
      ch <- substr(joined, i, i)
      if (!is.na(in_str)) {
        if (ch == "\\") i <- i + 1L   # skip an escaped char inside the string
        else if (ch == in_str) in_str <- NA_character_
      } else if (ch %in% c("'", '"')) in_str <- ch
      else if (ch == "(") depth <- depth + 1L
      else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
      i <- i + 1L
    }
    if (is.na(end)) return(NULL)   # unbalanced — leave alone, do not guess
    call_text <- substr(joined, starts[k], end)
    args_text <- substr(joined, open_paren + 1L, end - 1L)

    # Only calls whose FIRST quoted string looks like a file reference (has an
    # extension) are candidates — this is what makes a plain paste0("a", "b")
    # elsewhere in the code invisible to this scan.
    m <- regexpr("(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1", args_text, perl = TRUE)
    if (m == -1) return(NULL)
    fmt <- regmatches(args_text, m)
    quoted_len <- nchar(fmt)   # includes the surrounding quotes
    fmt <- substr(fmt, 2, nchar(fmt) - 1)
    if (!grepl("\\.[A-Za-z0-9]{1,8}$", fmt)) return(NULL)
    # A bare sprintf()-style format specifier ("%.2e", "%.3f", "%05.2f") also
    # satisfies the "ends in a dotted extension-shape" check above by pure
    # coincidence — its own "%" + digits + "." + conversion letter looks
    # exactly like "<name>.<ext>". Checked on the BASENAME (not the whole
    # `fmt`), since a real path with a placeholder ("data/%s_data.csv") must
    # still be treated as a candidate — only the LAST path segment being a
    # bare format spec (no directory content, no literal filename text at
    # all) means this quoted string was never a file reference to begin with.
    # Confirmed as a real false positive against a real corpus paper's script
    # (sprintf("%.2e", est) reported "%.2e" as a file "not present in the
    # repository") — the SAME bug .repro_format_call_refs()'s sibling scan in
    # code_check.R's code_file_refs() was already fixed for; this is the
    # other of two independent places a format specifier can be
    # misidentified as a filename, missed the first time because this
    # function has its own, separate quoted-string extraction.
    if (grepl("^%[-+ 0#]*[0-9]*(\\.[0-9]+)?[diouxXeEfgGaAscp]$",
             basename(gsub("\\\\", "/", fmt)), perl = TRUE)) return(NULL)

    # Only calls with EXTRA arguments AFTER the quoted format string are what
    # this scan is for: a bare sprintf("Exp1A_Data.txt") with nothing else is a
    # plain literal in a no-op wrapper — code_file_refs()'s plain-literal path
    # already handles it correctly, and there is no trailing-argument-orphaning
    # risk to fix. `args_text` still carries the quotes around fmt (unlike
    # `fmt` itself, stripped above), so the remainder is found by skipping
    # `quoted_len` characters rather than re-matching the (already-consumed)
    # quoted text.
    after_fmt <- substr(args_text, quoted_len + 1, nchar(args_text))
    if (!grepl("^\\s*,", after_fmt, perl = TRUE)) return(NULL)

    line <- 1L + lengths(regmatches(substr(joined, 1, starts[k]),
                                    gregexpr("\n", substr(joined, 1, starts[k]))))[1]

    # Attempt substitution of %s/%d placeholders with the values of simple
    # top-level string-literal variables (wd <- "..."; wd_data <- "..."),
    # taken in argument order after the format string.
    extra_args <- trimws(strsplit(sub("^\\s*,\\s*", "", after_fmt, perl = TRUE),
                                  ",")[[1]])
    resolved <- NA_character_
    if (grepl("%[sd]", fmt) && length(extra_args) > 0) {
      vars <- .repro_simple_string_vars(nc)
      vals <- vars[extra_args]
      if (!any(is.na(vals)) && length(vals) == length(regmatches(
        fmt, gregexpr("%[sd]", fmt))[[1]])) {
        r <- fmt
        for (v in vals) r <- sub("%[sd]", v, r, perl = TRUE)
        resolved <- r
      }
    }
    data.frame(call_text = call_text, fmt = fmt, resolved = resolved, line = line)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

#' Simple top-level string-literal variable assignments in a script
#'
#' A minimal constant-folder: finds assignments of the exact shape
#' `name <- "literal"` (or `=`/`<<-`) at the START of a (comment-free) line —
#' the same "top level" approximation [repro_defined_vars()] uses — and returns
#' the literal each name was last assigned. Only a bare quoted string is
#' resolved (no concatenation, no function calls); anything else leaves that
#' name absent from the result, so a caller asking for an unresolvable name
#' gets `NA`, never a guess.
#'
#' @param code_text the code text for a single file, comments already stripped
#'
#' @returns a named character vector (name = literal value), possibly empty
#' @keywords internal
.repro_simple_string_vars <- function(code_text) {
  pat <- "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(?:<<-|<-|=)\\s*(['\"])((?:[^'\"\\\\]|\\\\.)*)\\2\\s*$"
  m <- regmatches(code_text, regexec(pat, code_text, perl = TRUE))
  hits <- Filter(function(x) length(x) == 4, m)
  if (length(hits) == 0) return(character(0))
  nm  <- vapply(hits, `[[`, character(1), 2)
  val <- vapply(hits, `[[`, character(1), 4)
  stats::setNames(val, nm)[!duplicated(nm, fromLast = TRUE)]
}

#' Find a script's WRITE calls and redirect their targets into the sandbox
#'
#' A script writes intermediate/output files by relative path
#' (`write.csv("../Data/Cleaned/x.csv", ...)`, `saveRDS(m, "out/model.rds")`).
#' [repro_rewrite_paths()] only ever rewrites READ calls (`code_file_refs()`'s
#' `include_writes` defaults to `FALSE` there) — a write's own path is left
#' exactly as the author wrote it, so when the materialised sandbox's directory
#' layout does not match the original repo's (it is Psych-DS's layout, not the
#' author's), the write's target directory frequently does not exist at all,
#' and the write fails with "cannot open the connection" — a sandboxing
#' artefact, not a real reproducibility finding, confirmed against a real
#' corpus paper whose script wrote to `"../Data/Cleaned/..."` (a path that only
#' resolves relative to the AUTHOR's own original working directory).
#'
#' Every detected write call is redirected to `file.path(output_dir,
#' basename(...))` — a single flat folder inside the sandbox that
#' [repro_materialize_layout()] always creates, so the write always succeeds
#' regardless of the original relative structure. Two call shapes are handled:
#'
#' * a LITERAL quoted target (`write.csv("cleaned.csv", ...)`) — redirected
#'   directly, keeping the literal's own basename;
#' * a BARE VARIABLE target (`write.csv(cleaned_file_name, ...)`, the variable
#'   assigned on an earlier line, e.g. `cleaned_file_name <- paste0("../Data/
#'   Cleaned/task_level_", Sys.Date(), ".csv")`) — the ENTIRE write call is
#'   replaced with a fixed literal path, using the VARIABLE's own name (not its
#'   unknowable runtime value, which may depend on `Sys.Date()` or other
#'   non-literal pieces this scan cannot evaluate) plus the extension its
#'   assignment's own last quoted segment declares. This does not need to
#'   resolve the variable's exact string value — only that its assignment
#'   LOOKS like a file path (ends `.<ext>` in its last quoted piece) — so it
#'   still succeeds even when a placeholder (`Sys.Date()`) makes the true
#'   runtime value unknowable ahead of time.
#'
#' A write whose target cannot be traced to a literal or a simple top-level
#' assignment (built from a loop variable, a function argument, a data value)
#' is left UNREWRITTEN, the same "do not guess" policy
#' [repro_rewrite_paths()] already follows for reads.
#'
#' @param code_text the code text for a single file (character vector)
#'
#' @returns a data frame with one row per detected write call: `call_text` (the
#'   full original call), `replacement` (the full replacement call text, or
#'   `NA` when unresolved), `redirected_name` (the basename the write now
#'   targets, for the read-side basename search to also check), and `line`
#'   (1-based line the call starts on). Empty frame when the script has no
#'   resolvable write calls.
#' @keywords internal
.repro_redirect_writes <- function(code_text) {
  empty <- data.frame(call_text = character(0), replacement = character(0),
                      redirected_name = character(0), line = integer(0))
  if (is.null(code_text) || length(code_text) == 0) return(empty)
  nc <- code_remove_comments(code_text, "R")
  joined <- paste(nc, collapse = "\n")

  write_fns <- c("write[\\._][A-Za-z\\._0-9]*", "saveRDS", "save\\.image",
                "save", "ggsave", "export", "fwrite")
  fn_pat <- paste0("\\b(", paste(write_fns, collapse = "|"), ")\\s*\\(")
  starts <- gregexpr(fn_pat, joined, perl = TRUE)[[1]]
  if (length(starts) == 1 && starts == -1) return(empty)
  lens <- attr(starts, "match.length")

  # Does an arbitrary expression's TEXT look like it builds a file path — its
  # LAST quoted segment ends in a dotted extension? Covers both a bare literal
  # ("a.csv") and a paste0()/sprintf()-built path ("dir/", Sys.Date(), ".csv"),
  # since only the FINAL quoted piece needs checking for a plain "prefix +
  # <computed> + extension" shape either way — shared by the assignment-RHS
  # scan below AND by a write call's own INLINE argument (`write.csv(df,
  # paste0("dir/", Sys.Date(), ".csv"))`, no intermediate variable at all),
  # since both are exactly the same "does this expression's text end with a
  # file-shaped quoted piece" question, just applied to different text spans.
  .repro_path_like_ext <- function(expr_text) {
    qs <- regmatches(expr_text, gregexpr("(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1",
                                         expr_text, perl = TRUE))[[1]]
    if (!length(qs)) return(NA_character_)
    last_q <- substr(qs[[length(qs)]], 2, nchar(qs[[length(qs)]]) - 1)
    if (grepl("\\.[A-Za-z0-9]{1,8}$", last_q))
      sub(".*(\\.[A-Za-z0-9]{1,8})$", "\\1", last_q) else NA_character_
  }

  # Top-level assignments whose RHS looks path-like (see above) — covers
  # `x <- "a.csv"` and `x <- paste0("dir/", Sys.Date(), ".csv")` alike.
  assign_pat <- "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(?:<<-|<-|=)\\s*(.+)$"
  am <- regmatches(nc, regexec(assign_pat, nc, perl = TRUE))
  assigns <- Filter(function(x) length(x) == 3, am)
  path_like_vars <- character(0)
  var_ext <- character(0)
  for (a in assigns) {
    ext <- .repro_path_like_ext(a[[3]])
    if (!is.na(ext)) { path_like_vars[[a[[2]]]] <- TRUE; var_ext[[a[[2]]]] <- ext }
  }

  rows <- lapply(seq_along(starts), function(k) {
    open_paren <- starts[k] + lens[k] - 1L
    depth <- 0L; i <- open_paren; n <- nchar(joined); in_str <- NA_character_
    end <- NA_integer_
    while (i <= n) {
      ch <- substr(joined, i, i)
      if (!is.na(in_str)) {
        if (ch == "\\") i <- i + 1L
        else if (ch == in_str) in_str <- NA_character_
      } else if (ch %in% c("'", '"')) in_str <- ch
      else if (ch == "(") depth <- depth + 1L
      else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
      i <- i + 1L
    }
    if (is.na(end)) return(NULL)   # unbalanced — leave alone, do not guess
    call_text <- substr(joined, starts[k], end)
    # The function name alone (just capture group 1) — sub(fn_pat, "\\1", ...)
    # would be wrong here: it replaces only the MATCHED SPAN (fn name + "(")
    # with the capture group, leaving the rest of call_text (the arguments)
    # appended unchanged, not isolating the name at all.
    fn_name <- regmatches(call_text, regexec(fn_pat, call_text, perl = TRUE))[[1]][2]
    args_text <- substr(joined, open_paren + 1L, end - 1L)
    line <- 1L + lengths(regmatches(substr(joined, 1, starts[k]),
                                    gregexpr("\n", substr(joined, 1, starts[k]))))[1]

    # Scan EVERY top-level (comma-separated) argument for the write target —
    # NOT just the first: the path is the first positional argument for some
    # write functions (write.csv(path, x)-style — actually rare), but the
    # SECOND for write.csv(x, path)/write.csv(x, file = path), and a NAMED
    # argument entirely for saveRDS(x, file = path). Scanning every argument
    # position (positional value or the part after `name =`) for whichever one
    # actually looks like a file target (a literal ending in an extension, or
    # a bare variable already known to be path-like) finds the real target
    # regardless of which argument position that particular function uses,
    # without hardcoding a per-function argument index.
    args_list <- .repro_split_args(args_text)
    hit <- NULL
    for (arg in args_list) {
      val <- sub("^[.a-zA-Z][.a-zA-Z0-9_]*\\s*=\\s*(?!=)", "", trimws(arg$text), perl = TRUE)
      val <- trimws(val)

      lit <- regexpr("^(['\"])((?:[^'\"\\\\]|\\\\.)*)\\1$", val, perl = TRUE)
      if (lit != -1) {
        target <- substr(val, 2, nchar(val) - 1)
        if (grepl("\\.[A-Za-z0-9]{1,8}$", target)) {
          hit <- list(start = arg$start, end = arg$end,
                      redirected = basename(gsub("\\\\", "/", target)))
          break
        }
        next
      }
      if (grepl("^[.a-zA-Z][.a-zA-Z0-9_]*$", val) && !is.na(path_like_vars[val])) {
        hit <- list(start = arg$start, end = arg$end,
                    redirected = paste0(val, var_ext[[val]]))
        break
      }
      # An INLINE path-building call as the argument itself — no intermediate
      # variable at all (`write.csv(df, paste0("dir/", Sys.Date(), ".csv"))`,
      # the common `%>% write.csv(paste0(...), ...)` pipe shape). Gated on the
      # value actually being a call to one of the known path-building
      # functions (not just "contains a quoted extension-shaped string
      # somewhere"), so an unrelated call whose LAST argument happens to be
      # such a string (rare, but not zero-risk) is not swept in by accident.
      if (grepl("^(paste0|paste|sprintf|file\\.path)\\s*\\(", val, perl = TRUE)) {
        ext <- .repro_path_like_ext(val)
        if (!is.na(ext)) {
          # No variable name to label the redirected file with (unlike the
          # bare-variable branch above) — the call's own function name plus
          # this write's position in the script is used instead, so two
          # different inline-built writes in the same file do not collide on
          # an identical redirected basename.
          hit <- list(start = arg$start, end = arg$end,
                      redirected = paste0("write_", k, ext))
          break
        }
      }
    }
    if (is.null(hit)) return(NULL)   # no argument resolves to a file target — leave alone

    replacement_arg <- sprintf('"{{REPRO_OUTPUT}}%s"', hit$redirected)
    new_args <- paste0(substr(args_text, 1, hit$start - 1L), replacement_arg,
                       substr(args_text, hit$end + 1L, nchar(args_text)))
    data.frame(call_text = call_text,
              replacement = paste0(fn_name, "(", new_args, ")"),
              redirected_name = hit$redirected, line = line)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

# Split an already-extracted argument-list string into its top-level (paren/
# bracket/quote-depth-0) comma-delimited arguments. Shared by
# .repro_redirect_writes()'s call parsing, which needs to scan every argument
# for the write target (not just the first — write.csv(x, path), saveRDS(x,
# file = path), and a first-argument path all occur in real code). Returns a
# list of list(text, start, end) — start/end are 1-based character positions
# WITHIN args_text (inclusive), so a caller can splice a replacement into the
# original string without re-matching. Empty list if args_text is empty.
.repro_split_args <- function(args_text) {
  if (!nzchar(args_text)) return(list())
  depth <- 0L; in_str <- NA_character_; n <- nchar(args_text)
  arg_start <- 1L
  args <- list()
  i <- 1L
  while (i <= n) {
    ch <- substr(args_text, i, i)
    if (!is.na(in_str)) {
      if (ch == "\\") { i <- i + 2L; next }   # skip an escaped char inside the string
      if (ch == in_str) in_str <- NA_character_
    } else if (ch %in% c("'", '"')) in_str <- ch
    else if (ch %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch %in% c(")", "]", "}")) depth <- depth - 1L
    else if (ch == "," && depth == 0L) {
      args[[length(args) + 1L]] <- list(text = substr(args_text, arg_start, i - 1L),
                                        start = arg_start, end = i - 1L)
      arg_start <- i + 1L
    }
    i <- i + 1L
  }
  args[[length(args) + 1L]] <- list(text = substr(args_text, arg_start, n),
                                    start = arg_start, end = n)
  args
}

#' Rewrite a code file's data-file paths to the Psych-DS layout
#'
#' A script reads and writes data by relative path (`read_csv("raw/x.csv")`,
#' `saveRDS(m, "../out/model.rds")`). When the release is re-laid-out to
#' Psych-DS (data under `data/`, per-study `study-<group>/data/`), those paths no
#' lives, using the file→target plan `psychds_check` produced.
#'
#' Matching is by **basename** (a script's `../data/x.csv` and the repo's
#' `raw/x.csv` are the same file seen from different working directories, so the
#' prefix is ignored — the same choice `code_file_refs()` makes). When several
#' plan files share a basename (a `demographics.csv` in more than one study), the
#' ambiguity is resolved by **study group**: the candidate whose target path is
#' in the same study as the script (derived from the paths via
#' `.data_group_from_path()`) is chosen. Only when that still leaves more than
#' one candidate is the reference left unrewritten and flagged.
#'
#' A plan row for a converted tabular file (`psychds_check`'s `convert`/
#' `original_target` columns — a `.dta`/`.sav`/`.xlsx`/... source, whose
#' `target_path` is the re-exported `_data.csv`, with the untouched original
#' kept alongside at `original_target`) is matched **by the reference's own
#' extension**: a reference ending in the original's extension (`read_dta(
#' "math.dta")`) is rewritten to `original_target`, since a format-specific
#' reader needs the real file, not the CSV re-export — using `target_path`
#' there would silently point the reader at bytes it cannot correctly parse.
#' A reference that already names the CSV form (`"math_data.csv"`, or a bare
#' `.csv` extension) still rewrites to `target_path` as before.
#'
#' A path assembled at runtime with `sprintf()`/`paste()`/`paste0()`/
#' `file.path()` (see [.repro_format_call_refs()]) is matched the same way,
#' using the format string's own literal basename (with any resolvable
#' placeholder substituted via [.repro_simple_string_vars()]) — but since the
#' true "reference" there is the WHOLE call, not the quoted substring inside
#' it, such rows carry `is_call = TRUE` and `ref` holds the full call text
#' (`sprintf("%s/%s/x.csv", wd, wd_data)`), so [repro_write_scripts()] replaces
#' the entire expression rather than corrupting it with a substring swap that
#' would orphan the call's trailing arguments. A call whose placeholders could
#' not all be resolved is reported unmatched (`matched = FALSE`) rather than
#' guessed, and is separately flagged in the report as fragile authoring
#' practice regardless of whether it resolved.
#'
#' A basename can also collide across plan rows for a reason that is NOT real
#' ambiguity at all: a paper linking several OSF components that mirror each
#' other (see [repro_materialize_layout()]'s own de-duplication for the
#' execution-time counterpart of this) commonly lists the identical physical
#' file more than once, and the study-grouping step can assign DIFFERENT
#' (and for all-but-one, WRONG) groups to different mirrors of the same file
#' — e.g. one mirror's folder structure carries a "Study 1" hint the others
#' lack, so only that one gets grouped correctly. When `structure_df` is
#' supplied, candidates are first collapsed by the CONTENT HASH of their
#' resolved source files: byte-identical candidates count as ONE, using
#' whichever one's own group agrees with the majority (ties broken by the
#' first occurrence) — confirmed against a real corpus paper where 4 mirrors
#' of the same `Anonymized_2024-08-23.csv` were grouped ex1/ex1/ex1/ex3, which
#' made the existing "do all candidates already agree on one target"
#' shortcut fail (3 way agree, 1 disagrees) and fell through to `ambiguous =
#' TRUE` even though there was only ever one real file. Without
#' `structure_df` (or when a candidate's source cannot be resolved/read),
#' this collapsing step is skipped and candidates are compared as before.
#'
#' @param code_text the code text for a single file (character vector)
#' @param file_name the script's own name/path (used to derive its study group)
#' @param plan the `psychds_check` table: one row per file with `file_name`,
#'   `target_path`, `current_path`, and (for converted tabular files)
#'   `original_target`
#' @param lang the language (only R is rewritten here)
#' @param structure_df optional `data_check` structure table (`file_name`,
#'   `file_location`) — when supplied, enables the content-hash mirror
#'   collapsing described above. `NULL` (default) skips it.
#'
#' @returns a data frame with one row per referenced path, columns `ref` (the
#'   path as written, or the full call text when `is_call`), `basename`,
#'   `matched` (logical, a plan file was found), `target` (its Psych-DS path,
#'   or NA), `ambiguous` (logical, several plan files matched and the study
#'   could not disambiguate), `n_candidates`, and `is_call` (logical, TRUE when
#'   `ref` is a whole `sprintf()`/`paste()`-family call rather than a literal
#'   path — see above). Empty frame when the script references no files.
#' @export
#'
#' @examples
#' plan <- data.frame(
#'   file_name = c("demographics.csv", "scores.csv"),
#'   target_path = c("study-ex1/data/study-demographics_data.csv",
#'                   "study-ex1/data/study-scores_data.csv"),
#'   current_path = c("ex1/demographics.csv", "ex1/scores.csv")
#' )
#' code_text <- c('d <- read.csv("data/demographics.csv")')
#' repro_rewrite_paths(code_text, "ex1/analysis.R", plan)
repro_rewrite_paths <- function(code_text, file_name, plan, lang = "R",
                                structure_df = NULL) {
  empty <- data.frame(ref = character(0), basename = character(0),
                      matched = logical(0), target = character(0),
                      ambiguous = logical(0), n_candidates = integer(0),
                      is_call = logical(0))
  if (!identical(lang, "R") || is.null(code_text)) return(empty)
  if (is.null(plan) || nrow(plan) == 0 ||
      !all(c("file_name", "target_path") %in% names(plan))) return(empty)

  refs <- code_file_refs(code_text, "R")
  # Format-string calls (sprintf()/paste()/paste0()/file.path()) are a SEPARATE
  # source of references: code_file_refs() already returns their raw format
  # string as a "reference" too (it matches the quoted-filename pattern), so
  # basename-matching still finds the right plan row from the placeholder-laden
  # text alone — but rewriting must replace the WHOLE call, not that substring,
  # or the call's trailing arguments are orphaned (see .repro_format_call_refs()
  # roxygen for the concrete case this fixes). Build the call rows here and
  # drop their raw format string from `refs`, so each such reference is
  # represented exactly once, as a call row.
  call_refs <- .repro_format_call_refs(code_text)
  if (nrow(call_refs) > 0) refs <- setdiff(refs, call_refs$fmt)
  if (length(refs) == 0 && nrow(call_refs) == 0) return(empty)
  ref_base <- tolower(basename(gsub("\\\\", "/", refs)))
  ref_ext  <- tolower(tools::file_ext(ref_base))

  # Candidate plan files by basename. Only rows with a real (non-NA) target are
  # reachable in the release; NA-target rows (consumed archives) are not.
  plan_base <- tolower(basename(gsub("\\\\", "/", plan$file_name)))
  has_target <- !is.na(plan$target_path) & nzchar(plan$target_path %||% "")

  # A converted row's ORIGINAL extension, so a reference naming that same
  # extension is known to want the original, not the CSV re-export.
  orig_target <- if ("original_target" %in% names(plan))
    plan$original_target else rep(NA_character_, nrow(plan))
  orig_ext <- tolower(tools::file_ext(orig_target %||% ""))

  # Pick the right column for a candidate row, given the reference's own
  # extension: the original when the reference names the pre-conversion
  # format, else the (possibly-CSV) target_path.
  pick_target <- function(cand_i, ref_e) {
    if (!is.na(orig_target[cand_i]) && nzchar(orig_target[cand_i]) &&
        nzchar(ref_e) && ref_e == orig_ext[cand_i]) orig_target[cand_i]
    else plan$target_path[cand_i]
  }

  # The script's own study group, for disambiguation.
  script_grp <- .data_group_from_path(file_name)

  # Resolve each PLAN ROW's own on-disk source (by file_name, falling back to
  # basename — same lookup shape repro_materialize_layout()'s find_source()
  # already uses), for the content-hash mirror collapsing below. Only built
  # when structure_df was supplied; NA everywhere otherwise (skips collapsing
  # entirely, same as before this was added).
  plan_source_path <- if (!is.null(structure_df) &&
                          all(c("file_name", "file_location") %in% names(structure_df))) {
    loc_lookup <- stats::setNames(structure_df$file_location, structure_df$file_name)
    loc_base   <- stats::setNames(loc_lookup, tolower(basename(names(loc_lookup))))
    vapply(plan$file_name, function(fn) {
      if (fn %in% names(loc_lookup)) {
        l <- loc_lookup[[fn]]
        if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
      }
      b <- tolower(basename(fn))
      if (b %in% names(loc_base)) {
        l <- loc_base[[b]]
        if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
      }
      NA_character_
    }, character(1))
  } else rep(NA_character_, nrow(plan))
  plan_hash <- ifelse(is.na(plan_source_path), NA_character_,
                      unname(tools::md5sum(plan_source_path)))

  # Collapse candidates that are BYTE-IDENTICAL mirrors of the same real file
  # into one representative before any target/group comparison runs — a
  # basename-mirror duplicate whose study-grouping happened to disagree with
  # its sibling mirrors is not genuine ambiguity (see this function's own
  # roxygen for the confirmed real case). A hash-less candidate (structure_df
  # not supplied, or its source unreadable) is never collapsed with anything
  # — it keeps comparing exactly as before.
  collapse_by_hash <- function(cand) {
    h <- plan_hash[cand]
    has_hash <- !is.na(h)
    if (sum(has_hash) < 2) return(cand)   # nothing to collapse
    keep <- cand[!has_hash]                # hash-less candidates: untouched
    for (grp_h in unique(h[has_hash])) {
      grp_cand <- cand[has_hash][h[has_hash] == grp_h]
      if (length(grp_cand) == 1) { keep <- c(keep, grp_cand); next }
      # Among byte-identical mirrors, prefer whichever group the MAJORITY
      # agree on (a lone dissenting mirror is the mis-grouped one); ties (incl.
      # all-NA groups) keep the first occurrence, same "do not guess further"
      # spirit as the rest of this function.
      grp_of <- .data_group_from_path(plan$target_path[grp_cand])
      tab <- table(grp_of[!is.na(grp_of)])
      rep_i <- if (length(tab) > 0) grp_cand[which(!is.na(grp_of) &
                grp_of == names(tab)[which.max(tab)])[1]] else grp_cand[1]
      keep <- c(keep, rep_i)
    }
    keep
  }

  rows <- lapply(seq_along(refs), function(i) {
    cand <- which(plan_base == ref_base[i] & has_target)
    cand <- collapse_by_hash(cand)
    n <- length(cand)
    if (n == 0) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = FALSE,
                        target = NA_character_, ambiguous = FALSE,
                        n_candidates = 0L, is_call = FALSE))
    }
    if (n == 1) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                        target = pick_target(cand, ref_ext[i]), ambiguous = FALSE,
                        n_candidates = 1L, is_call = FALSE))
    }
    # Several PLAN ROWS share the basename, but that is not necessarily several
    # possible TARGETS: the plan can list the same file more than once (e.g. it
    # is referenced from several places in the repository listing), and every
    # row still resolves to the identical Psych-DS path. That is not ambiguity —
    # disambiguating would be solving a problem that does not exist, and the
    # earlier code marked every such reference unresolved even though only one
    # answer was ever possible.
    cand_targets <- unique(vapply(cand, pick_target, character(1), ref_e = ref_ext[i]))
    if (length(cand_targets) == 1) {
      return(data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                        target = cand_targets, ambiguous = FALSE,
                        n_candidates = n, is_call = FALSE))
    }
    # Several candidates share the basename AND disagree on target: disambiguate
    # by study group. Prefer the candidate whose target path is in the script's
    # study; failing that, whose target is in the study the reference path
    # itself names.
    cand_grp <- .data_group_from_path(plan$target_path[cand])
    ref_grp <- .data_group_from_path(refs[i])
    pick <- integer(0)
    if (!is.na(script_grp)) pick <- cand[cand_grp == script_grp & !is.na(cand_grp)]
    if (length(pick) != 1 && !is.na(ref_grp))
      pick <- cand[cand_grp == ref_grp & !is.na(cand_grp)]
    if (length(pick) == 1) {
      data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                 target = pick_target(pick, ref_ext[i]), ambiguous = FALSE,
                 n_candidates = n, is_call = FALSE)
    } else {
      # Still ambiguous: do not guess. Flagged for the report; the script is run
      # unmodified for this reference.
      data.frame(ref = refs[i], basename = ref_base[i], matched = TRUE,
                 target = NA_character_, ambiguous = TRUE, n_candidates = n,
                 is_call = FALSE)
    }
  })

  # Format-string calls: match the SAME way, keyed on the resolved literal
  # basename when every placeholder resolved, else the raw format string's own
  # literal basename (works whenever the placeholders stand only for a
  # directory prefix, as observed in practice — the filename portion is fully
  # literal so basename-matching already finds the right plan row without
  # needing the placeholder resolved at all). `ref` is the CALL TEXT, not the
  # format string, so the caller replaces the whole expression.
  call_rows <- if (nrow(call_refs) > 0) lapply(seq_len(nrow(call_refs)), function(i) {
    key <- if (!is.na(call_refs$resolved[i])) call_refs$resolved[i] else call_refs$fmt[i]
    key_base <- tolower(basename(gsub("\\\\", "/", key)))
    key_ext  <- tolower(tools::file_ext(key_base))
    cand <- which(plan_base == key_base & has_target)
    n <- length(cand)
    if (n == 0)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                        matched = FALSE, target = NA_character_,
                        ambiguous = FALSE, n_candidates = 0L, is_call = TRUE))
    if (n == 1)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                        matched = TRUE, target = pick_target(cand, key_ext),
                        ambiguous = FALSE, n_candidates = 1L, is_call = TRUE))
    cand_targets <- unique(vapply(cand, pick_target, character(1), ref_e = key_ext))
    if (length(cand_targets) == 1)
      return(data.frame(ref = call_refs$call_text[i], basename = key_base,
                        matched = TRUE, target = cand_targets, ambiguous = FALSE,
                        n_candidates = n, is_call = TRUE))
    cand_grp <- .data_group_from_path(plan$target_path[cand])
    key_grp <- .data_group_from_path(key)
    pick <- integer(0)
    if (!is.na(script_grp)) pick <- cand[cand_grp == script_grp & !is.na(cand_grp)]
    if (length(pick) != 1 && !is.na(key_grp))
      pick <- cand[cand_grp == key_grp & !is.na(cand_grp)]
    if (length(pick) == 1)
      data.frame(ref = call_refs$call_text[i], basename = key_base, matched = TRUE,
                target = pick_target(pick, key_ext), ambiguous = FALSE,
                n_candidates = n, is_call = TRUE)
    else
      data.frame(ref = call_refs$call_text[i], basename = key_base, matched = TRUE,
                target = NA_character_, ambiguous = TRUE, n_candidates = n,
                is_call = TRUE)
  }) else list()

  dplyr::bind_rows(c(rows, call_rows))
}

#' Determine the order code files must run in
#'
#' Scripts in a repository usually depend on each other: a preparation script
#' writes a file a later analysis script reads, or one `source()`s another.
#' Running them in the wrong order fails for reasons that have nothing to do with
#' reproducibility, so a run must order them first. This derives an order from
#' three signals, in decreasing strength:
#'
#' 1. **read-after-write** — if file A's code writes `x.csv` and file B's code
#'    reads `x.csv`, A must precede B (hard data dependency);
#' 2. **`source()` edges** — if A `source()`s B, B must precede A;
#' 3. **numeric filename prefixes** — `0_prep.R` before `1_analysis.R` (a weak
#'    tie-breaker, not a dependency).
#'
#' The read/write and source references come from the shared static analysis
#' ([code_file_refs()] and a source scan); matching is by basename (as
#' elsewhere). The dependency graph is topologically sorted; a cycle (A reads a
#' file B writes and vice versa) cannot be ordered and is reported.
#'
#' @param files a data frame with one row per code file, columns `file_name`,
#'   and `reads` / `writes` / `sources` list-columns of basenames (as produced
#'   by [repro_file_io()]). When `reads`/`writes`/`sources` are absent they are
#'   derived from a `code_text` list-column if present.
#'
#' @returns a data frame with columns `file_name`, `order` (1-based run
#'   position, NA when unplaceable), `depends_on` (comma-joined file_names that
#'   must run earlier), and `order_basis` (`dependency`, `numeric`, or `none`).
#'   Attribute `"cycle"` is a character vector of file_names in a dependency
#'   cycle (empty when acyclic); attribute `"ambiguous"` is TRUE when no ordering
#'   signal placed a multi-file set (they share `order = NA`, basis `none`).
#' @export
#'
#' @examples
#' files <- data.frame(file_name = c("1_analysis.R", "0_prep.R"))
#' files$reads   <- list("clean.csv", character(0))
#' files$writes  <- list(character(0), "clean.csv")
#' files$sources <- list(character(0), character(0))
#' repro_run_order(files)
repro_run_order <- function(files, extra_edges = NULL) {
  if (is.null(files) || nrow(files) == 0)
    return(data.frame(file_name = character(0), order = integer(0),
                      depends_on = character(0), order_basis = character(0)))

  n <- nrow(files)
  fname <- files$file_name
  base_name <- tolower(basename(gsub("\\\\", "/", fname)))

  io_col <- function(col) {
    if (col %in% names(files)) files[[col]]
    else replicate(n, character(0), simplify = FALSE)
  }
  reads   <- io_col("reads")
  writes  <- io_col("writes")
  sources <- io_col("sources")

  # Edge j -> i means j must run BEFORE i. Built from read-after-write and
  # source(), keyed on basename.
  before <- vector("list", n)   # before[[i]] = indices that must precede i
  for (i in seq_len(n)) before[[i]] <- integer(0)

  # read-after-write: writer of a basename precedes its readers.
  writer_of <- list()
  for (j in seq_len(n)) for (w in tolower(writes[[j]]))
    writer_of[[w]] <- c(writer_of[[w]], j)
  for (i in seq_len(n)) for (r in tolower(reads[[i]])) {
    w <- setdiff(writer_of[[r]] %||% integer(0), i)   # not self
    if (length(w)) before[[i]] <- union(before[[i]], w)
  }
  # source(): if i sources B, the file whose basename is B precedes i.
  for (i in seq_len(n)) for (s in tolower(sources[[i]])) {
    j <- setdiff(which(base_name == s), i)
    if (length(j)) before[[i]] <- union(before[[i]], j)
  }

  # extra_edges: soft ordering edges inferred from execution (e.g. an
  # undefined-variable error implies the file defining that variable must run
  # first). A list of c(from_file_name, to_file_name) pairs; each adds a
  # "from precedes to" edge, matched to file positions by file_name.
  for (e in extra_edges %||% list()) {
    from <- which(fname == e[[1]]); to <- which(fname == e[[2]])
    from <- setdiff(from, to)   # never self
    if (length(from) && length(to))
      before[[to[[1]]]] <- union(before[[to[[1]]]], from)
  }

  # Kahn topological sort. `before[[i]]` are i's predecessors; build the forward
  # adjacency (`after[[j]]` = successors of j) and the in-degree, then repeatedly
  # emit a zero-in-degree node and decrement its successors. Any node never
  # reaching in-degree 0 sits in a cycle.
  after <- vector("list", n)
  for (i in seq_len(n)) after[[i]] <- integer(0)
  for (i in seq_len(n)) for (j in before[[i]]) after[[j]] <- c(after[[j]], i)
  indeg <- vapply(before, length, integer(1))

  # Deterministic tie-break among ready nodes. Real scripts carry their sequence
  # as digit runs anywhere in the name, not just a leading prefix: "0_prep.R",
  # but also "Exp1_02_preprocessing.R", "study2_1_import.R". So the numeric key
  # is the VECTOR of all digit runs in the name (Exp1_02 -> c(1, 2)), padded to a
  # common length and compared component-wise, which sorts Exp1_02 < Exp2_1
  # correctly. Files with no digits sort last, then alphabetically.
  digit_runs <- lapply(base_name, function(b)
    as.integer(regmatches(b, gregexpr("[0-9]+", b))[[1]]))
  has_num <- vapply(digit_runs, length, integer(1)) > 0
  maxk <- max(0L, vapply(digit_runs, length, integer(1)))
  # A matrix of padded numeric keys (NA-padded so no-digit names go last).
  num_key <- if (maxk > 0)
    do.call(rbind, lapply(digit_runs, function(v)
      c(v, rep(NA_integer_, maxk - length(v))))) else
    matrix(NA_integer_, nrow = n, ncol = 1)
  tie_key <- function(v) {
    args <- c(lapply(seq_len(ncol(num_key)), function(k) num_key[v, k]),
              list(base_name[v]))
    v[do.call(order, args)]
  }

  order_idx <- integer(0)
  placed <- rep(FALSE, n)
  ready <- tie_key(which(indeg == 0))
  while (length(ready) > 0) {
    node <- ready[[1]]; ready <- ready[-1]
    order_idx <- c(order_idx, node); placed[node] <- TRUE
    newly <- integer(0)
    for (i in after[[node]]) {
      indeg[i] <- indeg[i] - 1L
      if (indeg[i] == 0L) newly <- c(newly, i)
    }
    if (length(newly)) ready <- tie_key(c(ready, newly))
  }
  cycle_nodes <- which(!placed)

  # order_basis per file: `dependency` when it had an incoming edge (a real data
  # or source() dependency decided its position); else `numeric` when its name
  # carries digits that fed the ordering; else `none` (nothing distinguished it).
  had_edge <- vapply(before, function(x) length(x) > 0, logical(1))
  any_dependency <- any(had_edge)
  basis <- ifelse(had_edge, "dependency",
                  ifelse(has_num, "numeric", "none"))

  ord <- rep(NA_integer_, n)
  ord[order_idx] <- seq_along(order_idx)

  depends_on <- vapply(seq_len(n), function(i)
    paste(fname[before[[i]]], collapse = ", "), character(1))

  out <- data.frame(file_name = fname, order = ord,
                    depends_on = depends_on, order_basis = basis)
  attr(out, "cycle") <- fname[cycle_nodes]
  # Ambiguous when more than one file exists but nothing (no dependency edge and
  # no numbering in any filename) distinguishes their order.
  attr(out, "ambiguous") <- n > 1 && !any_dependency && !any(has_num)
  out
}

#' Extract the files a script reads, writes, and sources
#'
#' A thin wrapper that runs the static reference scans over each code file and
#' returns the per-file read / write / source basenames [repro_run_order()]
#' needs. Reads/writes come from [code_file_refs()] split by call type; sources
#' from a scan for `source()`. All returned as lowercased basenames.
#'
#' @param code_text_list a named list of code-text character vectors, one per
#'   file (names are the file_names)
#'
#' @returns a data frame with `file_name` and list-columns `reads`, `writes`,
#'   `sources` (each a character vector of basenames).
#' @export
repro_file_io <- function(code_text_list) {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))

  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))

  # A read call vs a write call, from the call name preceding the quoted path.
  write_fns <- "\\b(write[._][A-Za-z._0-9]*|saveRDS|save|save\\.image|ggsave|export|fwrite)\\s*\\("
  src_pat <- "source\\s*\\(\\s*['\"]([^'\"]+)['\"]"

  rows <- lapply(seq_along(code_text_list), function(k) {
    ct <- code_text_list[[k]]
    nc <- code_remove_comments(ct, "R")
    # include_writes = TRUE: outputs must be visible here, because a file one
    # script writes is the input the next script reads. Without them `writes` is
    # always empty and every intermediate looks like a missing input.
    all_refs <- code_file_refs(nc, "R", include_writes = TRUE)
    joined <- paste(nc, collapse = "\n")

    # Classify each ref per *occurrence*, not per file: a file can be written on
    # one line and read back on another (a cached intermediate), and it must then
    # count as both. Taking any-write-wins would drop such a file from `reads`.
    ref_base <- tolower(basename(gsub("\\\\", "/", all_refs)))
    is_write <- rep(FALSE, length(all_refs))
    is_read  <- rep(FALSE, length(all_refs))
    for (i in seq_along(all_refs)) {
      lines <- grep(all_refs[i], nc, fixed = TRUE, value = TRUE)
      w <- grepl(write_fns, lines, perl = TRUE, ignore.case = TRUE)
      is_write[i] <- any(w)
      is_read[i]  <- any(!w)
    }
    reads  <- ref_base[is_read]
    writes <- ref_base[is_write]

    srcs <- regmatches(joined, gregexpr(src_pat, joined, perl = TRUE))[[1]]
    srcs <- sub(src_pat, "\\1", srcs, perl = TRUE)
    srcs <- tolower(basename(gsub("\\\\", "/", srcs)))

    # code_file_refs() lists source() among its "read" calls, so a sourced
    # script leaks into `reads`. A source() is a CODE dependency (handled by the
    # source-edge ordering), not a data input, so drop sourced files from reads —
    # otherwise a present, sourced .R file is later mis-reported as a missing
    # data input.
    reads <- setdiff(reads, srcs)

    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$reads <- list(unique(reads)); d$writes <- list(unique(writes))
              d$sources <- list(unique(srcs)); d })()
  })
  dplyr::bind_rows(rows)
}

#' Find the variables each script defines at top level
#'
#' When a script errors with `object 'X' not found`, the fix is usually to run
#' the script that *defines* `X` first. This scans each file for **top-level**
#' variable assignments — `x <- ...`, `x = ...`, `x <<- ...`, and
#' `assign("x", ...)` — so an undefined-variable error can be matched to the file
#' that would supply it (see the module's corrective re-run). Only assignments at
#' the start of a (comment-free) line are taken, which approximates "top level":
#' an assignment indented inside a `function`/`for` body is not a global the next
#' sourced script would see, and taking it would create false ordering edges.
#'
#' @param code_text_list a named list of code-text character vectors, one per
#'   file (names are the file_names)
#'
#' @returns a data frame with `file_name` and a list-column `defines` (character
#'   vector of variable names each file assigns at top level).
#' @export
repro_defined_vars <- function(code_text_list) {
  if (is.null(code_text_list) || length(code_text_list) == 0)
    return(data.frame(file_name = character(0)))
  fname <- names(code_text_list) %||% as.character(seq_along(code_text_list))

  # A top-level assignment: line start (no indentation => not inside a block),
  # a valid R name, then <-, <<-, or = (not ==). Also assign("name", ...).
  assign_pat <- "^([.a-zA-Z][.a-zA-Z0-9_]*)\\s*(<<-|<-|=)(?!=)"
  assign_fn  <- "^\\s*assign\\s*\\(\\s*['\"]([.a-zA-Z][.a-zA-Z0-9_]*)['\"]"

  rows <- lapply(seq_along(code_text_list), function(k) {
    nc <- code_remove_comments(code_text_list[[k]], "R")
    m1 <- regmatches(nc, regexpr(assign_pat, nc, perl = TRUE))
    v1 <- sub(paste0(assign_pat, ".*"), "\\1", m1, perl = TRUE)
    m2 <- regmatches(nc, regexpr(assign_fn, nc, perl = TRUE))
    v2 <- sub(paste0(assign_fn, ".*"), "\\1", m2, perl = TRUE)
    defines <- unique(c(v1[nzchar(v1)], v2[nzchar(v2)]))
    data.frame(file_name = fname[k], stringsAsFactors = FALSE) |>
      (\(d) { d$defines <- list(defines); d })()
  })
  dplyr::bind_rows(rows)
}

#' Diagnose why a referenced input file is unavailable
#'
#' When a script reads a file that is not present, the reason matters: a file
#' deliberately withheld because it was too large to download is a different
#' finding from a file simply absent from the repository. This cross-references
#' the referenced basenames against the file plan and the download-skip records
#' `download_repo_files()` attaches, and classifies each unresolved reference.
#'
#' @param refs a character vector of referenced file basenames (unresolved reads)
#' @param plan the `psychds_check` table (file_name + target_path)
#' @param structure_df the `data_check` structure table (file_name +
#'   file_location + file_size)
#' @param skipped optional data frame of size-skipped files (the
#'   `oversize_skipped` / `gated` attributes of [download_repo_files()], with a
#'   `file_name` column)
#'
#' @returns a data frame with `basename`, `status` (one of `in_repo_not_downloaded`,
#'   `withheld_size`, `absent`), `detail` (a human-readable reason), and
#'   `similar_to` (a candidate near-match basename from the repository's file
#'   list, or `NA` — only ever set for `absent` rows; see below).
#' @export
repro_missing_inputs <- function(refs, plan, structure_df, skipped = NULL) {
  refs <- unique(tolower(basename(gsub("\\\\", "/", refs))))
  empty <- data.frame(basename = character(0), status = character(0),
                      detail = character(0), similar_to = character(0))
  if (length(refs) == 0) return(empty)

  struct_base <- if (!is.null(structure_df) && "file_name" %in% names(structure_df))
    tolower(basename(gsub("\\\\", "/", structure_df$file_name))) else character(0)
  skip_base <- if (!is.null(skipped) && "file_name" %in% names(skipped))
    tolower(basename(gsub("\\\\", "/", skipped$file_name))) else character(0)

  # A file genuinely absent from the repository is sometimes actually PRESENT
  # under a near-identical name — a typo in the code, or a capitalisation/
  # separator mismatch ("Data.csv" referenced, "data.csv" present) — which is a
  # different (and more actionable) finding than "this file was never shared at
  # all". Tried only for an otherwise-`absent` reference (an exact match already
  # resolved above needs no fallback): first the SAME STEM ignoring extension
  # (a script asking for `data.csv` when only `data.xlsx` was shared — common
  # when authors describe a conversion in prose but never update the code), then
  # a small EDIT DISTANCE on the full basename (a typo/case/punctuation slip).
  # Never guesses across a large distance — a near-miss is surfaced as a
  # candidate for a human to confirm, not silently treated as a match.
  all_candidates <- unique(struct_base)
  find_similar <- function(b) {
    if (length(all_candidates) == 0) return(NA_character_)
    stem <- tools::file_path_sans_ext(b)
    cand_stems <- tools::file_path_sans_ext(all_candidates)
    stem_hit <- all_candidates[cand_stems == stem & nzchar(stem)]
    if (length(stem_hit) > 0) return(stem_hit[[1]])
    # Small edit distance on the full basename (extension included, so a
    # same-stem-different-extension pair is already caught above and does not
    # also compete here). Threshold: at most 3 edits, and never more than a
    # quarter of the reference's own length, so a short name ("x.csv") cannot
    # match something unrelated purely because 3 edits is a large fraction of
    # it — same "do not guess" spirit as the rest of this file.
    max_dist <- min(3L, floor(nchar(b) / 4))
    if (max_dist < 1L) return(NA_character_)
    d <- utils::adist(b, all_candidates)[1, ]
    hit <- which(d <= max_dist)
    if (length(hit) == 0) return(NA_character_)
    all_candidates[hit[which.min(d[hit])]]
  }

  rows <- lapply(refs, function(b) {
    if (b %in% skip_base) {
      i <- which(skip_base == b)[1]
      sz <- suppressWarnings(as.numeric(skipped$file_size[i]))
      mb <- if (is.finite(sz)) sprintf(" (%.0f MB)", sz / (1024 * 1024)) else ""
      return(data.frame(basename = b, status = "withheld_size",
                        detail = sprintf(
                          "referenced file is in the repository but was not downloaded%s: over the size cap",
                          mb), similar_to = NA_character_))
    }
    if (b %in% struct_base) {
      i <- which(struct_base == b)[1]
      loc <- if ("file_location" %in% names(structure_df))
        structure_df$file_location[i] else NA_character_
      downloaded <- !is.na(loc) && nzchar(loc) && file.exists(loc %||% "")
      if (!downloaded)
        return(data.frame(basename = b, status = "in_repo_not_downloaded",
                          detail = "referenced file is listed in the repository but was not downloaded",
                          similar_to = NA_character_))
      # Present and downloaded — not actually missing; caller filters these out.
      return(data.frame(basename = b, status = "present",
                        detail = "referenced file is available",
                        similar_to = NA_character_))
    }
    similar <- find_similar(b)
    detail <- if (!is.na(similar))
      sprintf("referenced file is not present in the repository, but a similarly-named file exists: %s",
              similar)
      else "referenced file is not present in the repository"
    data.frame(basename = b, status = "absent", detail = detail,
              similar_to = similar)
  })
  out <- dplyr::bind_rows(rows)
  out[out$status != "present", , drop = FALSE]
}

# ─────────────────────────────────────────────────────────────────────────────
# EXECUTION helpers (the `execute = TRUE` phase). Everything below actually runs
# downloaded code, so it is reached only when the caller explicitly opts in via
# reproducibility_check(execute = TRUE). Each script runs in an isolated
# subprocess (callr) so a crash cannot take down the R session; note this
# isolates crashes, NOT the filesystem or network — running untrusted code is a
# deliberate, gated action. The layout is materialised into a throwaway temp dir
# so the real psychds output and the download cache are never touched.
# ─────────────────────────────────────────────────────────────────────────────

#' Materialise the Psych-DS layout for a paper into a throwaway directory
#'
#' The execute phase runs the paper's code against the Psych-DS layout, but the
#' module has only the *plan* (`psychds_check`'s file → `target_path`) and the
#' current on-disk location of each source file (`data_check`'s `structure`).
#' This builds the tree the plan describes inside a fresh temp directory: for
#' each planned file with a real target and an available source copy, the source
#' is copied to `<root>/<target_path>`. Nothing outside `root` is written, and
#' `root` is the caller's to delete.
#'
#' @param plan the `psychds_check` table (`file_name`, `target_path`)
#' @param structure_df the `data_check` structure table (`file_name`,
#'   `file_location`)
#' @param root the temp directory to build into (created if absent)
#'
#' @returns `root`, invisibly, with attribute `"materialised"`: a data frame of
#'   `target_path` / `source` / `ok` (whether the copy succeeded) rows.
#' @export
repro_materialize_layout <- function(plan, structure_df, root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  # A flat folder every write call is redirected into (see
  # .repro_redirect_writes(), applied in repro_write_scripts()) — created
  # UNCONDITIONALLY, before the no-plan early return below, since a script can
  # write output even for a paper with no psychds_check plan at all (no data
  # files to lay out, but still code that writes a results file).
  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  done <- data.frame(target_path = character(0), source = character(0),
                     ok = logical(0))
  if (is.null(plan) || nrow(plan) == 0 ||
      !all(c("file_name", "target_path") %in% names(plan)))
    return(invisible(structure(root, materialised = done)))

  loc_lookup <- if (!is.null(structure_df) &&
                    all(c("file_name", "file_location") %in% names(structure_df)))
    stats::setNames(structure_df$file_location, structure_df$file_name) else
    character(0)
  # basename index, for when plan file_name and structure file_name differ by dir
  loc_base <- if (length(loc_lookup))
    stats::setNames(loc_lookup, tolower(basename(names(loc_lookup)))) else
    character(0)

  find_source <- function(fn) {
    if (fn %in% names(loc_lookup)) {
      l <- loc_lookup[[fn]]
      if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
    }
    b <- tolower(basename(fn))
    if (b %in% names(loc_base)) {
      l <- loc_base[[b]]
      if (!is.na(l) && nzchar(l) && file.exists(l)) return(l)
    }
    NA_character_
  }

  has_target <- !is.na(plan$target_path) & nzchar(plan$target_path %||% "")
  rows <- lapply(which(has_target), function(i) {
    tgt <- plan$target_path[i]
    src <- find_source(plan$file_name[i])
    if (is.na(src))
      return(data.frame(target_path = tgt, source = NA_character_, ok = FALSE))
    dest <- file.path(root, tgt)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    ok <- isTRUE(file.copy(src, dest, overwrite = TRUE))
    data.frame(target_path = tgt, source = src, ok = ok)
  })

  # A converted tabular row (psychds_check's `convert`/`original_target`
  # columns — see R/data_check_helpers.R and repro_rewrite_paths()'s own
  # roxygen) ALSO needs its untouched ORIGINAL copied in, not just the
  # `_data.csv` re-export above: repro_rewrite_paths() rewrites a script's
  # read_dta()/read_sav()/read_excel() call to `original_target` specifically
  # BECAUSE that reader needs the real file, not the CSV. Without this, the
  # rewrite correctly names the right path, but nothing ever puts a file there
  # — confirmed directly against a real paper (a read_dta("math.dta") call,
  # correctly rewritten to "data/math.dta" (original_target keeps the file's
  # own real basename — see psychds_check.R's same_dir_real_name()), still
  # failed at execute time with "'data/math.dta' does not exist", because only
  # the target_path loop above had ever populated the sandbox).
  if ("original_target" %in% names(plan)) {
    has_orig <- !is.na(plan$original_target) & nzchar(plan$original_target %||% "")
    orig_rows <- lapply(which(has_orig), function(i) {
      tgt <- plan$original_target[i]
      src <- find_source(plan$file_name[i])
      if (is.na(src))
        return(data.frame(target_path = tgt, source = NA_character_, ok = FALSE))
      dest <- file.path(root, tgt)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      ok <- isTRUE(file.copy(src, dest, overwrite = TRUE))
      data.frame(target_path = tgt, source = src, ok = ok)
    })
    if (length(orig_rows)) rows <- c(rows, orig_rows)
  }

  done <- if (length(rows)) dplyr::bind_rows(rows) else done
  invisible(structure(root, materialised = done))
}

#' Write path-rewritten scripts into the materialised layout
#'
#' Each script's referenced data paths are rewritten to their Psych-DS target
#' (via the per-file `repro_rewrite_paths()` result) so that, run from `root`,
#' the reads/writes resolve against the materialised tree. The rewritten script
#' text is written to `root/<script target_path>` (or, when the plan has no
#' target for the script itself, to `root/<basename>` at the top level so it is
#' still runnable). A reference left ambiguous or unmatched is written unchanged
#' — the static report already flagged it.
#'
#' @param code_text_list named list of per-file code text (names are file_names)
#' @param rewrite_list named list of `repro_rewrite_paths()` results, same order
#' @param plan the `psychds_check` table (to find each script's own target_path)
#' @param root the materialised layout root
#'
#' @details
#' A `setwd()` call in analysis code is bad practice: it hardcodes an assumption
#' about where the code runs (often an absolute path on the author's own machine,
#' e.g. `setwd("D:/Dropbox/...")`), and it overrides the working directory the
#' runner sets to the materialised layout — so the script's relative reads then
#' resolve against a directory that does not exist here, and the reproduction
#' fails for a reason that has nothing to do with the analysis. To run the code
#' we therefore **comment out every `setwd()` call** (recorded per file in
#' `setwd_removed` and flagged in the report). The transform is in the same
#' spirit as the path rewriting: it makes the author's script runnable in its new
#' location without changing the analysis. A script that only reproduces after a
#' `setwd()` is removed is reported as "reproducible after ignoring a `setwd()`
#' that should not have been in the code and needs to be fixed".
#'
#' @returns a data frame with `file_name`, `script_path` (absolute path written),
#'   `run_dir` (the working directory a run should use — always `root`),
#'   `setwd_removed` (count of `setwd()` lines commented out),
#'   `setwd_paths` (comma-joined paths those calls named, for the warning), and
#'   `family_replaced` (count of `family =`/`base_family =` named-font
#'   arguments replaced with `"sans"`, so a plot call does not fail with
#'   "invalid font type" for a font not registered on the machine running the
#'   sandbox).
#' @export
repro_write_scripts <- function(code_text_list, rewrite_list, plan, root) {
  fnames <- names(code_text_list)
  plan_base <- if (!is.null(plan) && "file_name" %in% names(plan))
    tolower(basename(plan$file_name)) else character(0)

  script_target <- function(fn) {
    # A script's own place in the tree: match its basename in the plan; else put
    # it at the tree root under its basename so it is still runnable.
    if (length(plan_base)) {
      m <- which(plan_base == tolower(basename(fn)) &
                   !is.na(plan$target_path) & nzchar(plan$target_path %||% ""))
      if (length(m) >= 1) return(plan$target_path[m[[1]]])
    }
    basename(fn)
  }

  # A line that calls setwd(). Matches setwd( at a statement start (allowing
  # leading whitespace), so setwd inside a longer expression is still caught by
  # the call token. The quoted argument (if any) is captured for the warning.
  setwd_line <- "^(\\s*)(setwd\\s*\\(.*)$"
  setwd_arg  <- "setwd\\s*\\(\\s*['\"]([^'\"]+)['\"]"

  rows <- lapply(seq_along(code_text_list), function(i) {
    fn  <- fnames[[i]]
    txt <- code_text_list[[i]]
    rw  <- rewrite_list[[fn]]
    # Apply each resolved (matched, non-ambiguous, real target) rewrite as a
    # literal replacement. Fixed (non-regex) to avoid metacharacter surprises
    # in file paths. A plain reference replaces just the quoted path; an
    # `is_call` row (sprintf()/paste()-family — see repro_rewrite_paths())
    # replaces the ENTIRE call with a bare literal string, since `ref` there is
    # the whole call text: substituting only a substring inside it would leave
    # the call's other arguments (e.g. `wd, wd_data` after the format string)
    # orphaned in a now-broken expression.
    if (!is.null(rw) && nrow(rw)) {
      good <- rw$matched & !rw$ambiguous & !is.na(rw$target) & nzchar(rw$target)
      is_call_col <- if ("is_call" %in% names(rw)) rw$is_call else rep(FALSE, nrow(rw))
      for (k in which(good)) {
        repl <- if (isTRUE(is_call_col[k]))
          paste0('"', rw$target[k], '"') else rw$target[k]
        txt <- gsub(rw$ref[k], repl, txt, fixed = TRUE)
      }
    }

    # Redirect WRITE calls into the sandbox's flat output/ folder (see
    # .repro_redirect_writes()'s own roxygen for why: an unrewritten write
    # commonly targets a relative directory that does not exist in the
    # materialised layout — a sandboxing artefact, not a real reproducibility
    # finding — confirmed against a real corpus paper's script writing to
    # "../Data/Cleaned/..."). Applied on the WHOLE joined text (a write call
    # can wrap across lines, e.g. piped into write.csv() on its own line), then
    # re-split back into per-line form so the rest of this function's
    # line-vector operations (setwd detection below) still work unchanged.
    wr <- .repro_redirect_writes(txt)
    if (nrow(wr) > 0) {
      joined_txt <- paste(txt, collapse = "\n")
      for (k in seq_len(nrow(wr))) {
        repl <- gsub("{{REPRO_OUTPUT}}",
                    paste0(gsub("\\\\", "/", file.path(root, "output")), "/"),
                    wr$replacement[k], fixed = TRUE)
        joined_txt <- sub(wr$call_text[k], repl, joined_txt, fixed = TRUE)
      }
      txt <- strsplit(joined_txt, "\n", fixed = TRUE)[[1]]
    }

    # Comment out setwd() lines so they cannot override the sandbox wd. Record
    # how many, and the path each named, for the report's warning.
    is_setwd <- grepl(setwd_line, txt, perl = TRUE)
    setwd_n <- sum(is_setwd)
    setwd_paths <- character(0)
    if (setwd_n > 0) {
      hit_lines <- txt[is_setwd]
      m <- regmatches(hit_lines, regexpr(setwd_arg, hit_lines, perl = TRUE))
      setwd_paths <- sub(paste0(".*", setwd_arg, ".*"), "\\1",
                         m[nzchar(m)], perl = TRUE)
      # Comment the offending lines (keep them visible, but inert), tagging why.
      txt[is_setwd] <- sub(setwd_line,
        "\\1# [reproducibility_check removed setwd] \\2", txt[is_setwd],
        perl = TRUE)
    }

    # Replace a named font family (family = "Times New Roman", base_family =
    # "Arial", ...) with grid's own universal fallback "sans" — a specific
    # named font is very commonly NOT registered as usable on the machine
    # actually running the sandboxed subprocess (no font-registration setup
    # of its own, unlike the author's own interactive session where the OS
    # font is simply present), so the plot call fails with "invalid font
    # type" for a reason that has nothing to do with the analysis itself —
    # confirmed against a real corpus paper's theme_few(base_family = "Times
    # New Roman") failing exactly this way in the callr subprocess. "sans" is
    # grid's own documented generic family name, resolved by every graphics
    # device without needing the named font installed at all. Deliberately
    # narrow: only a `family =` / `base_family =` NAMED argument is touched
    # (not any quoted string that merely looks like a font name), the same
    # "do not guess beyond what is clearly the thing in question" restraint
    # the rest of this function's rewrites already follow.
    family_pat <- "\\b(base_family|family)\\s*=\\s*(['\"])[^'\"]*\\2"
    joined_for_family <- paste(txt, collapse = "\n")
    family_n <- lengths(regmatches(joined_for_family,
                                   gregexpr(family_pat, joined_for_family, perl = TRUE)))[1]
    if (family_n > 0) {
      joined_for_family <- gsub(family_pat, "\\1 = \"sans\"", joined_for_family, perl = TRUE)
      txt <- strsplit(joined_for_family, "\n", fixed = TRUE)[[1]]
    }

    tgt  <- script_target(fn)
    dest <- file.path(root, tgt)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeLines(txt, dest)
    data.frame(file_name = fn, script_path = dest, run_dir = root,
               setwd_removed = setwd_n,
               setwd_paths = paste(setwd_paths, collapse = ", "),
               family_replaced = family_n)
  })
  dplyr::bind_rows(rows)
}

#' Install a paper's declared dependencies into a throwaway library
#'
#' Installs each non-base dependency into `lib_dir` (a temp library, never the
#' user's), so a run does not mutate the user's installed packages. CRAN packages
#' come from `install.packages()`; GitHub/URL sources from the ref the code named
#' (`remotes::install_github()` / `install.packages(url)`), honouring a pinned
#' `@ref` when present. Every install's outcome is recorded; installing runs
#' package build/configure scripts, so this is part of the gated execute phase.
#'
#' @param install_deps the module's `install_deps` frame (`package`, `source`,
#'   `ref`), base packages already excluded
#' @param lib_dir the throwaway library path (created if absent); ignored for
#'   CRAN-source packages when `cran_to_main_lib = TRUE` (see below)
#' @param cran_to_main_lib if `TRUE`, CRAN-source packages are installed into
#'   your DEFAULT library (no `lib` argument — same as calling
#'   `install.packages(pkg)` yourself) instead of the throwaway `lib_dir`, and so
#'   PERSIST after the run rather than being deleted with the sandbox. This
#'   trades a one-time, permanent change to your R library for install work that
#'   is shared across every later call needing the same package (useful when
#'   `reproducibility_check()` runs across many papers in one session/script: a
#'   CRAN package installed for an early paper is already present, and skipped
#'   as already-installed, for every later one). GitHub/URL sources are
#'   deliberately excluded from this and always go into the throwaway `lib_dir`
#'   regardless of this argument — those are arbitrary, unreviewed code you
#'   likely do not want permanently in your main library. Default `FALSE`
#'   (everything throwaway, the original behaviour).
#'
#' A CRAN-source package that `install.packages()` cannot find is not
#' necessarily a broken dependency: CRAN removes (archives) packages that stop
#' passing its checks, but every version ever published stays available as a
#' source tarball under the CRAN Archive
#' (`https://cran.r-project.org/src/contrib/Archive/<pkg>/`). When the live-CRAN
#' install fails, this retries once from the Archive's most recent version
#' before giving up — a package the paper's authors could install at the time
#' (and that later dropped off live CRAN) still installs, instead of the run
#' recording an "errored" script for a cause that has nothing to do with the
#' paper's code. The retry is recorded in `via_archive` so the report can say
#' the run used a stale-but-real version rather than the one the authors had.
#'
#' @returns a data frame with `package`, `source`, `installed` (logical),
#'   `message` (error text on failure, else ""), `via_archive` (logical,
#'   TRUE when installed from the CRAN Archive after a live-CRAN failure), and
#'   `category` (see [.repro_classify_install_message()]; `NA` for a
#'   successful install).
#' @export
repro_install_deps <- function(install_deps, lib_dir, cran_to_main_lib = FALSE) {
  empty <- data.frame(package = character(0), source = character(0),
                      installed = logical(0), message = character(0),
                      via_archive = logical(0), category = character(0))
  if (is.null(install_deps) || nrow(install_deps) == 0) return(empty)
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  old_lib <- .libPaths(); on.exit(.libPaths(old_lib), add = TRUE)
  .libPaths(c(lib_dir, old_lib))

  # A non-interactive session (Rscript, a Quarto render, a CI job) has repos set
  # to the unresolved placeholder "@CRAN@". Interactively R would prompt for a
  # mirror; here install.packages() just fails with "trying to use CRAN without
  # setting a mirror", so EVERY not-yet-installed dependency fails and the
  # paper's scripts then error on library() — which reads as the paper being
  # irreproducible when the real cause is this session's configuration. Set a
  # mirror for the duration of the installs when none is configured.
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) ||
      any(!nzchar(repos)) || any(repos == "@CRAN@")) {
    old_repos <- getOption("repos")
    on.exit(options(repos = old_repos), add = TRUE)
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    message("[repro]     no CRAN mirror configured; using cloud.r-project.org ",
            "for this install.")
  }

  gh_avail <- requireNamespace("remotes", quietly = TRUE)

  rows <- lapply(seq_len(nrow(install_deps)), function(i) {
    pkg <- install_deps$package[i]
    src <- install_deps$source[i]
    ref <- install_deps$ref[i]
    # A CRAN package already installed in one of the REAL (pre-throwaway)
    # library paths needs no work — this is what makes cran_to_main_lib's reuse
    # across papers actually pay off, instead of every call re-installing it.
    # Checked against `old_lib` specifically (not requireNamespace()'s default
    # search), because requireNamespace() also returns TRUE for a namespace
    # that is merely already LOADED in this R session — which happens after
    # this function once installed a package into the throwaway `lib_dir` and
    # loaded it from there. That throwaway directory is deleted at the end of
    # the run, so a later call in the same session would report "already
    # installed" for a package that no longer exists anywhere on disk, and a
    # freshly spawned callr subprocess (which starts with none of this
    # session's loaded namespaces) would then fail to find it at all.
    cran_main <- identical(src, "cran") && isTRUE(cran_to_main_lib)
    if (cran_main && length(find.package(pkg, lib.loc = old_lib, quiet = TRUE)) > 0) {
      message("[repro]     '", pkg, "' already installed (main library); skipping.")
      return(data.frame(package = pkg, source = src, installed = TRUE, message = "",
                        via_archive = FALSE))
    }
    # DEBUG (TEMPORARY — remove before release): quiet = FALSE so compilation
    # progress is visible; a slow install then reads as work, not a hang.
    message("[repro]     installing '", pkg, "' (source: ", src,
            if (cran_main) ", into main library" else ", into throwaway library",
            ") ...")
    install_lib <- if (cran_main) old_lib[1] else lib_dir
    res <- tryCatch({
      if (identical(src, "github")) {
        if (!gh_avail) stop("the 'remotes' package is needed to install GitHub sources")
        remotes::install_github(ref, lib = lib_dir, upgrade = "never",
                                quiet = FALSE)
      } else if (identical(src, "url")) {
        utils::install.packages(ref, lib = lib_dir, repos = NULL, quiet = FALSE)
      } else {
        # Explicit lib=: .libPaths()[1] is `lib_dir` (the throwaway) at this
        # point, not R's real user library, so an unqualified install.packages()
        # call here would silently install into the throwaway and vanish at
        # the end of the run — the bug this whole block works around above.
        # old_lib[1] is the real library this session would install to
        # normally (captured before the throwaway push).
        utils::install.packages(pkg, lib = install_lib, quiet = FALSE)
      }
      # Confirm it can actually be loaded (main library needs no lib.loc; the
      # throwaway paths were already prepended to .libPaths() above).
      if (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib_dir) &&
          !requireNamespace(pkg, quietly = TRUE))
        stop("installed but package '", pkg, "' is not loadable")
      list(ok = TRUE, msg = "", via_archive = FALSE)
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e), via_archive = FALSE))

    # A CRAN-source package that failed is not necessarily broken code: CRAN
    # may simply no longer be SERVING it (removed for a check failure, a
    # dependency going away, ...) while every version it ever published still
    # sits in the Archive. Retry once from there before accepting the failure —
    # this is the one case worth a second attempt, since GitHub/URL failures
    # already point at a specific, user-named source with nothing else to try.
    if (!res$ok && identical(src, "cran")) {
      message("[repro]     '", pkg, "' not available on live CRAN; trying the ",
              "CRAN Archive (last published version) ...")
      res2 <- .repro_cran_archive_install(pkg, install_lib, lib_dir)
      if (res2$ok) {
        message("[repro]     '", pkg, "' installed from the CRAN Archive (",
                res2$version, ").")
        res <- list(ok = TRUE, msg = "", via_archive = TRUE)
      } else {
        message("[repro]     '", pkg, "' could not be installed from the ",
                "CRAN Archive either: ", res2$msg)
        res$msg <- paste0(res$msg, " (CRAN Archive retry also failed: ", res2$msg, ")")
      }
    }
    data.frame(package = pkg, source = src, installed = res$ok, message = res$msg,
              via_archive = isTRUE(res$via_archive),
              category = if (isTRUE(res$ok)) NA_character_
                        else .repro_classify_install_message(res$msg))
  })
  dplyr::bind_rows(rows)
}

#' Classify why a package installation failed
#'
#' `repro_install_deps()`/`repro_install_deps_docker()` already capture the raw
#' error text for a failed package, but the raw text alone does not say what
#' KIND of failure it was — and the distinction matters for what a reader
#' should do about it: a package genuinely gone from CRAN with no successor is
#' a real, permanent problem (worth reporting as-is), a transient network
#' hiccup is worth an automatic retry rather than treating as conclusive, and a
#' missing compiler toolchain is an environment-setup gap, not a defect in the
#' paper's own code. None of these change whether the install COUNTS as a
#' failure (they still do, and still stay out of the traffic light — see the
#' module's own dependency-unavailable handling), only how informative the
#' report is about the subtype.
#'
#' Matched against the install error message's own text with a fixed set of
#' regexes, most-specific first, so a message that could plausibly match more
#' than one category (rare, but not impossible) is not misassigned to a looser
#' pattern. A message matching none of them is `"other"` — the raw `message`
#' column is always still the authoritative text; this is advisory framing on
#' top of it, not a replacement for reading the message.
#'
#' `install.packages()`'s own standard CRAN-unavailability message —
#' `"package 'x' is not available for this version of R"` — is CRAN's ONE
#' generic wording for two different real causes (the package was removed/
#' archived entirely, or a current version simply never supported this R
#' release) and does not itself say which one applies; confirmed directly
#' against real R output rather than assumed. Splitting those into two
#' categories from the message text alone would be a guess this classifier
#' does not make — both are reported as `"cran_unavailable"`, and the CRAN
#' Archive retry's own `via_archive` result elsewhere already provides the
#' real, non-guessed signal for the archived case ([repro_install_deps()]),
#' since a successful Archive install proves that specific cause rather than
#' inferring it from wording.
#'
#' @param msg the error message text (`repro_install_deps()`'s own `message`
#'   column for one failed row)
#'
#' @returns a single category string: `"cran_unavailable"`,
#'   `"compile_failure"`, `"network"`, `"transitive_dependency_missing"`, or
#'   `"other"`
#' @keywords internal
.repro_classify_install_message <- function(msg) {
  if (is.null(msg) || is.na(msg) || !nzchar(msg)) return("other")

  # A source build that failed partway through compiling/configuring — most
  # often a missing system library or compiler toolchain, not an R-level
  # problem at all. Checked before the broader patterns below: a compile
  # failure's own text ("non-zero exit status") never overlaps with them, but
  # checking it first keeps this category's meaning narrow and unambiguous.
  # NOTE: a bare "error: " substring was deliberately tried and dropped here —
  # R wraps almost any condition's own text in "Error: ..." regardless of
  # cause (confirmed: it false-matched a GitHub 404 "not found" message that
  # has nothing to do with compiling), so it is not a usable compile signal on
  # its own; every pattern below is specific to an actual build-tool message.
  if (grepl("configure(:| failed| error)|non-zero exit status|compilation failed|make(:| error)\\b|\\bgcc\\b|g\\+\\+|gfortran",
           msg, ignore.case = TRUE, perl = TRUE))
    return("compile_failure")

  # Could not even reach the repository/host — a transient condition, worth
  # retrying, not evidence the package is unavailable.
  if (grepl("could not resolve host|couldn.t connect to server|timed? ?out|connection (refused|reset|timed out)|network is unreachable|ssl (connect|certificate) error|could not reach",
           msg, ignore.case = TRUE, perl = TRUE))
    return("network")

  # The package ITSELF is not being installed here — a NAMED dependency of it
  # is what's missing/unavailable ("there is no package called 'x'", "dependency
  # 'x' is not available for package 'y'"). Checked before the generic
  # cran_unavailable pattern below, since "dependency 'x' is not available..."
  # would otherwise also match that broader "is not available" wording and be
  # misclassified as the package's own CRAN availability, not its dependency's.
  if (grepl("there is no package called|depend(s|ency|encies) .*(is|are) not available|unable to (find|locate) (required )?package",
           msg, ignore.case = TRUE, perl = TRUE))
    return("transitive_dependency_missing")

  # CRAN's generic "not available" wording — covers both a fully archived
  # package and one simply incompatible with this R version; see roxygen above
  # for why those are not split further.
  if (grepl("is not available (for|as a package)|package .* is not available|was built (for|under) R version",
           msg, ignore.case = TRUE, perl = TRUE))
    return("cran_unavailable")

  "other"
}

#' Install a package's most recent CRAN Archive version
#'
#' Every package ever published to CRAN keeps its full version history under
#' the CRAN Archive (`https://cran.r-project.org/src/contrib/Archive/<pkg>/`),
#' even after the live repository stops serving it. This lists that directory,
#' takes the most recent version by file modification time (the directory
#' listing carries no other reliable ordering), and installs the source
#' tarball directly — the one fallback worth trying automatically when a
#' CRAN-source dependency is not on live CRAN, since it needs no extra
#' service or paper-specific date, just the package name.
#'
#' @param pkg the package name
#' @param install_lib the library to install into (the real library for
#'   `cran_to_main_lib`, else the throwaway `lib_dir`)
#' @param lib_dir the throwaway library (checked for loadability, same as the
#'   caller's main install path)
#'
#' @returns a list `ok` (logical), `msg` (character, error text on failure),
#'   `version` (the Archive version installed, or NA)
#' @keywords internal
.repro_cran_archive_install <- function(pkg, install_lib, lib_dir) {
  fail <- function(msg) list(ok = FALSE, msg = msg, version = NA_character_)
  archive_url <- paste0("https://cran.r-project.org/src/contrib/Archive/", pkg, "/")
  listing <- tryCatch(readLines(archive_url, warn = FALSE),
                      error = function(e) NULL)
  if (is.null(listing)) return(fail("could not reach the CRAN Archive listing"))

  # The Archive's directory listing is an HTML index: each row names a source
  # tarball "<pkg>_<version>.tar.gz" and its own modification date, which is
  # the only ordering signal available (versions do not always sort
  # lexically, e.g. "1.9" vs "1.10"). Take the row with the LATEST date.
  tarball_pat <- paste0(pkg, "_[0-9][^\"']*\\.tar\\.gz")
  hit_lines <- grep(tarball_pat, listing, value = TRUE)
  if (length(hit_lines) == 0)
    return(fail("package not found in the CRAN Archive"))

  date_pat <- "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})"
  dates <- suppressWarnings(as.POSIXct(
    sub(paste0(".*", date_pat, ".*"), "\\1", hit_lines), tz = "UTC"))
  files <- regmatches(hit_lines, regexpr(tarball_pat, hit_lines))
  ord <- if (all(!is.na(dates))) order(dates, decreasing = TRUE) else
    order(files, decreasing = TRUE)
  latest_file <- files[ord][1]
  version <- sub(paste0("^", pkg, "_(.*)\\.tar\\.gz$"), "\\1", latest_file)
  tarball_url <- paste0(archive_url, latest_file)

  ok <- tryCatch({
    if (requireNamespace("remotes", quietly = TRUE)) {
      # remotes::install_url() resolves the tarball's OWN Imports/Depends from
      # live CRAN before installing it; a plain install.packages(repos = NULL,
      # type = "source") does not (repos = NULL disables all dependency
      # resolution, since it means "this is a direct file, not a repository
      # lookup") — confirmed as a real, general bug, not specific to one
      # package: an archived package with ANY unmet CRAN dependency fails
      # R CMD INSTALL with a non-zero exit status here, which read as "this
      # package cannot be installed on this R version" even though the
      # package itself installs fine once its dependencies are present
      # (confirmed against dynamic 1.1.0's own missing `simstandard` import).
      remotes::install_url(tarball_url, dependencies = NA, lib = install_lib,
                           upgrade = "never", quiet = FALSE)
    } else {
      # No remotes: fall back to the old dependency-blind path rather than
      # failing outright — still works for an archived package with no unmet
      # dependencies of its own.
      utils::install.packages(tarball_url, lib = install_lib,
                              repos = NULL, type = "source", quiet = FALSE)
    }
    if (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib_dir) &&
        !requireNamespace(pkg, quietly = TRUE))
      stop("installed but package '", pkg, "' is not loadable")
    TRUE
  }, error = function(e) conditionMessage(e))
  if (isTRUE(ok)) list(ok = TRUE, msg = "", version = version)
  else fail(ok)
}

#' Run the paper's scripts, in order, in isolated subprocesses
#'
#' Runs each script with [callr::r()] from `run_dir`, with `lib_dir` prepended to
#' the subprocess `.libPaths()` so the throwaway library is seen. Each run is
#' bounded by `timeout` seconds; its exit, error, stdout and stderr are captured.
#' A script whose inputs are known-missing (in `skip`) is not run — it is
#' recorded `skipped_missing_inputs`, since it cannot succeed and would only
#' produce a confusing error. callr isolates a crash to the subprocess; it does
#' not sandbox the filesystem or network, so this only runs on explicit opt-in.
#'
#' @param run_tbl `repro_write_scripts()` output (`file_name`, `script_path`,
#'   `run_dir`)
#' @param order a vector of `file_name`s in the order to run (unplaceable files
#'   are appended at the end in input order)
#' @param lib_dir throwaway library to expose to each subprocess (or NULL)
#' @param timeout per-script timeout in seconds
#' @param skip character vector of `file_name`s to record as
#'   `skipped_missing_inputs` instead of running
#' @param parses named logical (by file_name); a file that will not parse is
#'   recorded `not_parsed` and not run
#' @param failed_deps character vector of package names that
#'   [repro_install_deps()] could not install (CRAN Archive retry included) —
#'   used only to tell apart a script that failed because ITS OWN dependency is
#'   genuinely unavailable (`dependency_unavailable`) from a script that failed
#'   for any other reason (`errored`), so an unavailable package does not read
#'   as a bug in the paper's code
#'
#' @returns a data frame with `file_name`, `outcome` (one of `ran_ok`,
#'   `errored`, `timed_out`, `not_parsed`, `skipped_missing_inputs`,
#'   `dependency_unavailable`), `error` (message or ""), `error_type`
#'   (`undefined_variable`, `timeout`, `dependency_unavailable`, `runtime`, or
#'   NA), `undefined_var` (the missing variable name when
#'   `error_type == "undefined_variable"`, else NA), `stdout`, `stderr`,
#'   `elapsed` (seconds), and `script_lines` (list-column: the EXECUTED script's
#'   own text, one element per source line — the exact text `stdout`'s echoed
#'   statements were run from, so [read_r_output()] can match them back to a
#'   line number; empty character(0) for a script that did not run).
#' @export
repro_run_scripts <- function(run_tbl, order, lib_dir = NULL, timeout = 600,
                              skip = character(0), parses = NULL,
                              failed_deps = character(0)) {
  if (is.null(run_tbl) || nrow(run_tbl) == 0)
    return(data.frame(file_name = character(0), outcome = character(0),
                      error = character(0), error_type = character(0),
                      undefined_var = character(0), stdout = character(0),
                      stderr = character(0), elapsed = numeric(0)) |>
             dplyr::mutate(script_lines = list(), captures = list()))
  if (!requireNamespace("callr", quietly = TRUE))
    stop("the 'callr' package is required to execute code (execute = TRUE).",
         call. = FALSE)

  # Order the files: placeable ones by the given order, the rest appended.
  ordered_names <- c(order[order %in% run_tbl$file_name],
                     setdiff(run_tbl$file_name, order))
  libs <- if (!is.null(lib_dir)) c(lib_dir, .libPaths()) else .libPaths()

  pb_run <- pb(length(ordered_names), ":what [:bar] :current/:total")
  pb_run$tick(0, list(what = ""))
  on.exit(pb_run$terminate())

  rows <- lapply(ordered_names, function(fn) {
    pb_run$tick(1, list(what = fn))
    row <- run_tbl[run_tbl$file_name == fn, ][1, ]

    no_lines <- function(df) dplyr::mutate(df, script_lines = list(character(0)),
                                           captures = list(NULL))
    if (!is.null(parses) && fn %in% names(parses) && !isTRUE(parses[[fn]]))
      return(no_lines(data.frame(file_name = fn, outcome = "not_parsed", error = "",
                        error_type = NA_character_, undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))
    if (fn %in% skip)
      return(no_lines(data.frame(file_name = fn, outcome = "skipped_missing_inputs",
                        error = "", error_type = NA_character_,
                        undefined_var = NA_character_,
                        stdout = "", stderr = "", elapsed = 0)))

    # The EXECUTED script's own text (path-rewritten, setwd()-commented — what
    # actually ran), one element per source line: this is what read_r_output()
    # matches echoed statements against to recover a line number, so it must be
    # read from script_path (not the pre-rewrite code_text_list) to stay aligned
    # with what the echo in stdout actually shows.
    exec_lines <- tryCatch(readLines(row$script_path, warn = FALSE),
                           error = function(e) character(0))

    # DEBUG (TEMPORARY — remove before release): announce each script as it starts
    # and finishes, so a hang is pinned to the exact script and its wall time.
    message("[repro]   -> running '", fn, "' (timeout ", timeout, "s) ...")

    # Capture the child's stdout/stderr to FILES, not OS pipes. With stdout = "|"
    # callr routes output through a pipe the parent only drains after the call
    # returns; a chatty script fills the pipe buffer and the child blocks on
    # write while the parent blocks on wait — a deadlock (this is the hang).
    # Redirecting to files removes the buffer entirely, so no script can wedge
    # the run, and the output is still captured (read back below).
    out_file <- tempfile(fileext = ".out")
    err_file <- tempfile(fileext = ".err")
    # Sidecar for CAPTURED RESULT OBJECTS (R/r-capture.R). A task callback in
    # the child records each top-level statistical object — the object itself,
    # not what it printed — so p-values, statistic identities and coefficient
    # matrices come back exact instead of re-parsed from console text. Written
    # by the child on exit; absent if the script died before writing, in which
    # case the stdout path alone is used.
    cap_file <- tempfile(fileext = ".rds")
    t0 <- Sys.time()
    out <- tryCatch(
      callr::r(
        # callr::r() has no working-directory argument, so set it INSIDE the
        # subprocess before sourcing, so the script's relative paths resolve
        # against the materialised layout root.
        .r_capture_runner(),
        args = list(script = row$script_path, wd = row$run_dir,
                    capture_file = cap_file,
                    helpers = .r_capture_helpers()),
        libpath = libs,
        timeout = timeout, error = "error",
        stdout = out_file, stderr = err_file),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    captures <- if (file.exists(cap_file))
      tryCatch(readRDS(cap_file), error = function(e) NULL) else NULL
    unlink(cap_file)

    # Read whatever the child wrote before it finished / errored / timed out.
    read_cap <- function(f) if (file.exists(f))
      paste(readLines(f, warn = FALSE), collapse = "\n") else ""
    so <- read_cap(out_file); se <- read_cap(err_file)
    unlink(c(out_file, err_file))

    message("[repro]   <- '", fn, "' done in ", round(elapsed, 1), "s (",
            if (inherits(out, "condition")) "condition/error" else "ok",
            "; stdout ", nchar(so), " chars, stderr ", nchar(se), " chars)")

    # callr signals a timeout with a classed condition; a script error arrives as
    # a callr "error in processx"/"callr_error" carrying the child's message.
    if (inherits(out, "condition")) {
      is_timeout <- inherits(out, "callr_timeout_error") ||
        grepl("timed? ?out", conditionMessage(out), ignore.case = TRUE)
      msg <- conditionMessage(out)
      # Classify the error. An "object 'X' not found" is special: it usually
      # means the script expected a variable an earlier (unrun / separately-run)
      # script defined — the classic symptom of a snippet meant to be source()d
      # into a larger session, not run standalone. We capture the variable name;
      # a later phase uses it as a run-order signal (the file defining X should
      # precede this one). Check both the callr message and the captured stderr,
      # since the child's own error text lands in stderr.
      #
      # R phrases a missing FUNCTION differently from a missing variable
      # ("could not find function \"foo\"" vs "object 'x' not found") — both are
      # the same underlying problem (an unresolved top-level symbol, usually
      # because the script that defines it was never run/sourced first), so
      # both patterns are checked and feed the same undefined_var extraction.
      undef_pat <- "object ['\"]([^'\"]+)['\"] not found"
      fn_pat    <- "could not find function ['\"]([^'\"]+)['\"]"
      undef_src <- if (grepl(undef_pat, msg)) msg else
        if (grepl(undef_pat, se)) se else
        if (grepl(fn_pat, msg)) msg else
        if (grepl(fn_pat, se)) se else NA_character_
      undef_var <- if (!is.na(undef_src) && grepl(undef_pat, undef_src))
        sub(paste0(".*", undef_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(undef_pat, undef_src)))
        else if (!is.na(undef_src) && grepl(fn_pat, undef_src))
        sub(paste0(".*", fn_pat, ".*"), "\\1",
            regmatches(undef_src, regexpr(fn_pat, undef_src)))
        else NA_character_

      # A script that library()/require()s a package we already know FAILED to
      # install (even after the CRAN Archive retry — see repro_install_deps())
      # is not a bug in the paper's code: it never got the chance to run its
      # real analysis. R's own error for this is "there is no package called
      # 'x'" (from library()) or the same text via require()'s stop-on-failure
      # form; match it against failed_deps rather than trusting the message's
      # wording alone, so a script that merely REFERENCES an unavailable
      # package's name elsewhere is not misclassified.
      nopkg_pat <- "there is no package called ['\"]([^'\"]+)['\"]"
      nopkg_src <- if (grepl(nopkg_pat, msg)) msg else
        if (grepl(nopkg_pat, se)) se else NA_character_
      nopkg_var <- if (!is.na(nopkg_src))
        sub(paste0(".*", nopkg_pat, ".*"), "\\1",
            regmatches(nopkg_src, regexpr(nopkg_pat, nopkg_src))) else NA_character_
      dep_unavailable <- !is.na(nopkg_var) && nopkg_var %in% failed_deps

      etype <- if (is_timeout) "timeout"
               else if (dep_unavailable) "dependency_unavailable"
               else if (!is.na(undef_var)) "undefined_variable"
               else "runtime"
      outc <- if (is_timeout) "timed_out"
              else if (dep_unavailable) "dependency_unavailable"
              else "errored"
      data.frame(file_name = fn, outcome = outc,
                 error = msg, error_type = etype, undefined_var = undef_var,
                 stdout = so, stderr = se, elapsed = elapsed) |>
        dplyr::mutate(script_lines = list(exec_lines),
                      captures = list(captures))
    } else {
      data.frame(file_name = fn, outcome = "ran_ok", error = "",
                 error_type = NA_character_, undefined_var = NA_character_,
                 stdout = so, stderr = se, elapsed = elapsed) |>
        dplyr::mutate(script_lines = list(exec_lines),
                      captures = list(captures))
    }
  })
  dplyr::bind_rows(rows)
}
