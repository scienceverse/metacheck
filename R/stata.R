# Read Stata Markup and Control Language (.smcl) output logs ----------------
#
# A .smcl file is Stata's own default output-log format: PLAIN TEXT with
# brace-delimited markup directives (e.g. "{txt}", "{col 40}", "{hline 20}")
# telling the Results window / Viewer how to render each line -- documented
# in Stata's own `smcl` manual entry (help smcl / the "smcl" PDF in the Stata
# manuals), unlike SPSS's `.spv` (undocumented binary) or JASP/jamovi's own
# formats. This file's decoder is therefore a markup-STRIPPER (turn one
# marked-up line into the plain text Stata's Results window would show), not
# a byte-format reverse-engineering exercise.
#
# Once stripped to plain text, a .smcl log looks exactly like R's own
# `source(script, echo = TRUE)` console output: a "{com}. <command>" echo
# line (Stata's analogue of R's "> ") followed by whatever that command
# printed, including fixed-width result tables. The table-detection
# machinery is therefore modeled directly on R/r-output.R's
# .r_output_tables() (blank-column "river" splitting), duplicated here
# rather than shared, since Stata's own table-boundary conventions
# ({hline}-drawn rules, not R's prose stop-words like "residual standard
# error") are genuinely different from R's.
#
# The complete real-world directive inventory this stripper handles was
# taken from 9 real .smcl files found on Zenodo (a mix of survey-analysis,
# SEM, and thesis-replication logs), not just the documented set -- notably
# Stata itself also emits `{text}` as an undocumented (in the manual excerpt
# checked) synonym for `{txt}`, confirmed appearing tens of thousands of
# times in one real SEM-iteration log.
#
# What is NOT in scope: charts. A `graph export "....png"` command line is
# recovered as ordinary command-echo text (the exact command that was run),
# but the PNG file itself is never embedded in the .smcl -- Stata writes it
# to disk separately, so there is no image data in this format to extract at
# all (unlike .jasp/.omv, and unlike even .spv's rare embedded charts).

# Style/mode directives that are purely zero-width markers (they change how
# SUBSEQUENT text renders -- color/font in the real Results window -- but
# carry no text of their own and are simply removed). `text` is Stata's own
# undocumented-in-the-manual-excerpt synonym for `txt`, confirmed in real
# files. `err`/`res`/`inp`/`txt`/`cmd` are the four content-type styles (syntax
# 1: bare, no argument); `sf`/`bf`/`it` are font-face switches; `smcl`/`ul off`
# are the log-file bookend / underline-off markers.
.SMCL_ZERO_WIDTH <- c("txt", "text", "res", "inp", "cmd", "com", "err",
                     "sf", "bf", "it", "smcl", "ul off", "ul on", "hilite",
                     "hi", "reset")

# `{c NAME}` box-drawing / literal-brace character codes, the complete set
# observed across the real corpus (see the file header) plus the small
# number of others documented in the smcl manual that share the same
# 2-3-letter naming convention (top/bottom/left/right tee, four corners).
# `{c -(}`/`{c )-}` are how SMCL escapes a LITERAL brace (needed inside a
# `foreach ... {` Stata block, which would otherwise be read as a directive
# opener) -- rendered back to the literal character here, not stripped away,
# since it is real command-echo text.
.SMCL_C_CODES <- c(
  "|" = "|", "+" = "+", "-" = "-",
  "TT" = "┬", "BT" = "┴", "LT" = "├", "RT" = "┤",
  "TLC" = "┌", "TRC" = "┐", "BLC" = "└", "BRC" = "┘",
  "-(" = "{", ")-" = "}")

#' Render one SMCL-marked-up line as plain text
#'
#' Interprets a single line of `.smcl` markup the way Stata's own Results
#' window / Viewer would display it: `{col N}` jumps (padding with spaces)
#' to absolute column `N`, `{hline N}` draws `N` (default 1 screen width's
#' worth, capped at 78 as Stata's own default window width) `-` characters,
#' `{space N}` inserts `N` literal spaces, `{c NAME}` substitutes a
#' box-drawing / literal-brace character (see `.SMCL_C_CODES`), a syntax-2/4
#' directive (`{res:text}`, `{ralign 12:text}`) keeps just `text` (rendered
#' according to its own rule when one applies, e.g. right-alignment), and
#' every other directive (a style/mode marker, a link directive such as
#' `{help ...}`, a comment `{* ...}`) is a zero-width no-op that is simply
#' removed. Unrecognised directives are also removed rather than left in the
#' output verbatim, since a stray `{unknown}` reads far worse in extracted
#' text than silently dropping a directive this port has not seen.
#'
#' @param line one raw line of `.smcl` text (as read from the file, one
#'   element of `readLines()`'s result)
#' @return the plain-text rendering of that line
#' @keywords internal
.smcl_render_line <- function(line) {
  out <- line
  pos <- 0L  # current rendered-column position, for {col N}

  # {c NAME} -- resolved FIRST, since its payload can itself contain
  # characters ("|", "+", "-", "(", ")") that would otherwise confuse the
  # generic directive regex below.
  for (code in names(.SMCL_C_CODES))
    out <- gsub(paste0("{c ", code, "}"), .SMCL_C_CODES[[code]], out, fixed = TRUE)

  # Walk the line left to right, expanding directives in order (so {col N}'s
  # column count reflects the ALREADY-rendered text before it, matching how
  # Stata itself lays a line out).
  result <- character(0)
  i <- 1L; n <- nchar(out)
  while (i <= n) {
    ch <- substr(out, i, i)
    if (ch != "{") { result <- c(result, ch); pos <- pos + 1L; i <- i + 1L; next }

    close <- regexpr("\\}", substr(out, i, n))
    if (close < 0) { result <- c(result, ch); pos <- pos + 1L; i <- i + 1L; next }  # unmatched brace: literal
    directive <- substr(out, i + 1L, i + close - 2L)
    i <- i + close  # advance past the closing "}"

    if (grepl("^col ", directive)) {
      target <- suppressWarnings(as.integer(sub("^col ", "", directive)))
      if (!is.na(target) && target > pos) { result <- c(result, strrep(" ", target - pos)); pos <- target }
      next
    }
    if (grepl("^space ", directive)) {
      k <- suppressWarnings(as.integer(sub("^space ", "", directive)))
      if (!is.na(k) && k > 0) { result <- c(result, strrep(" ", k)); pos <- pos + k }
      next
    }
    if (identical(directive, "hline") || grepl("^hline ", directive)) {
      k <- suppressWarnings(as.integer(sub("^hline ?", "", directive)))
      if (is.na(k)) k <- 78L - pos  # syntax 1 (no count): rest of the (default 78-col) line
      if (k > 0) { result <- c(result, strrep("-", k)); pos <- pos + k }
      next
    }
    if (identical(directive, ".-")) {
      result <- c(result, strrep("-", max(1L, 78L - pos))); pos <- 78L
      next
    }
    if (grepl("^dup ", directive)) {
      m <- regmatches(directive, regexec("^dup ([0-9]+):(.*)$", directive))[[1]]
      if (length(m) == 3) {
        k <- as.integer(m[[2]])
        txt <- strrep(m[[3]], k)
        result <- c(result, txt); pos <- pos + nchar(txt)
      }
      next
    }
    if (grepl("^char |^c 0x", directive)) {
      code <- suppressWarnings(as.integer(sub("^char |^c 0x", "", directive)))
      if (!is.na(code) && code >= 0 && code <= 255) {
        ch2 <- intToUtf8(code)
        result <- c(result, ch2); pos <- pos + 1L
      }
      next
    }
    # Alignment directives {lalign N:text} / {ralign N:text} / {center N:text}
    # / {center:text} / {rcenter[ #]:text}: keep the text, padded/aligned
    # within a field of width N when one is given. This is the only case
    # where a directive's OWN text argument needs layout applied, rather
    # than being passed straight through.
    al <- regmatches(directive, regexec("^(lalign|ralign|center|rcenter) ?([0-9]*):(.*)$", directive))[[1]]
    if (length(al) == 4 && nzchar(al[[1]])) {
      kind <- al[[2]]; width <- suppressWarnings(as.integer(al[[3]])); txt <- al[[4]]
      if (is.na(width) || width <= nchar(txt)) {
        result <- c(result, txt); pos <- pos + nchar(txt)
      } else {
        pad <- width - nchar(txt)
        padded <- switch(kind,
          lalign = paste0(txt, strrep(" ", pad)),
          ralign = paste0(strrep(" ", pad), txt),
          paste0(strrep(" ", pad %/% 2L), txt, strrep(" ", pad - pad %/% 2L)))  # center/rcenter
        result <- c(result, padded); pos <- pos + width
      }
      next
    }
    # Any other syntax-2/4 directive with a ":text" payload (e.g.
    # "{res:-640.68206}", "{help summarize:clicking}", "{it:italics}"):
    # keep just the text, dropping the directive name/args.
    colon <- regexpr(":", directive, fixed = TRUE)
    if (colon > 0) {
      txt <- substr(directive, colon + 1L, nchar(directive))
      result <- c(result, txt); pos <- pos + nchar(txt)
      next
    }
    # A bare directive with no payload (a style/mode marker, {smcl}, a
    # comment "{* ...}", an unrecognised directive): zero-width, dropped.
  }
  paste(result, collapse = "")
}

#' Render a full .smcl file's lines as plain text
#'
#' Applies [.smcl_render_line()] to every line, mirroring how the Stata
#' Results window / Viewer would display the whole log.
#'
#' @param lines character vector, one element per raw `.smcl` line
#' @return character vector of the same length, plain-text rendered
#' @keywords internal
.smcl_render <- function(lines) vapply(lines, .smcl_render_line, character(1), USE.NAMES = FALSE)

# ═══════════════════════════════════════════════════════════════════════════
# ── Command/output splitting ─────────────────────────────────────────────────
# A rendered .smcl line that WAS a "{com}. <command>" / "{com}> <continuation>"
# echo starts with a literal ". " or "> " once rendered to plain text
# (the {com} style marker itself renders to nothing, per .SMCL_ZERO_WIDTH) --
# the exact analogue of R's "> "/"+ " echo prompts, which .r_echo_chunks()
# (R/r-output.R) already splits on. A multi-line command continuation is
# additionally sometimes prefixed with a literal line-number ("  2. ", "  3.
# ") when Stata is inside a block (foreach/if/while) -- confirmed in a real
# file's `foreach var of varlist ... { ... }` block.
# ═══════════════════════════════════════════════════════════════════════════

#' Split a rendered .smcl transcript into (command, output) chunks
#'
#' @param rendered character vector: the plain-text rendering from
#'   [.smcl_render()]
#' @return a list of `list(command, output)`, one per top-level Stata
#'   command found (a run of `. `/`> `/`  N. ` echo lines followed by
#'   whatever it printed, up to the next echo or end of file)
#' @keywords internal
.smcl_command_chunks <- function(rendered) {
  is_echo <- grepl("^(\\. |> |\\s*[0-9]+\\. )", rendered)
  if (!any(is_echo)) return(list())
  is_new <- grepl("^\\. ", rendered)
  starts <- which(is_new)
  if (!length(starts)) return(list())
  ends <- c(starts[-1] - 1L, length(rendered))

  lapply(seq_along(starts), function(k) {
    seg <- rendered[starts[k]:ends[k]]
    echo_n <- sum(grepl("^(\\. |> |\\s*[0-9]+\\. )", seg))
    cmd <- trimws(sub("^(\\. |> |\\s*[0-9]+\\. )", "", seg[seq_len(echo_n)]))
    output <- if (echo_n < length(seg)) seg[(echo_n + 1L):length(seg)] else character(0)
    list(command = paste(cmd, collapse = " "), output = output)
  })
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Fixed-width text tables (duplicated from R/r-output.R's
# .r_output_tables() blank-column-splitting technique -- see that file's own
# comment for the full rationale. NOT called directly: R's own stop-word
# heuristics ("residual standard error", "Call:", ...) do not apply to
# Stata's tables, which are bounded by {hline}-drawn dash rules instead, so
# this is a Stata-specific re-application of the SAME splitting primitive,
# not a shared function. ─────────────────────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

.stata_is_numlike <- function(x) {
  x <- trimws(x)
  grepl("^-?[0-9][0-9.,]*(e[-+]?[0-9]+)?$", x, ignore.case = TRUE) |
  grepl("^[<>]\\s*-?[0-9.]+(e[-+]?[0-9]+)?$", x, ignore.case = TRUE) |
  grepl("(?i)^(inf|-?inf|na|nan|\\.)$", x)
}

# Split a set of block lines at columns blank in EVERY line -- identical
# technique to R/r-output.R's split_block(), duplicated per this file's
# self-containment convention (see the file header).
.stata_split_block <- function(block) {
  block <- gsub("\t", strrep(" ", 8), block, fixed = TRUE)
  w <- max(nchar(block))
  padded <- formatC(block, width = -w, flag = "-")
  chars <- do.call(rbind, strsplit(padded, "", fixed = TRUE))
  blank_col <- apply(chars, 2, function(cc) all(cc == " "))
  nb <- !blank_col
  if (!any(nb)) return(NULL)
  d <- diff(c(0L, as.integer(nb), 0L))
  starts <- which(d == 1L); ends <- which(d == -1L) - 1L
  lapply(seq_along(starts), function(k) trimws(substr(padded, starts[k], ends[k])))
}

# A Stata result table is bounded by {hline}-drawn dash rules (rendered to a
# literal run of "-", optionally interspersed with box-drawing tee/cross
# characters at column-boundary points -- e.g. "{hline 13}{c +}{hline 57}").
# This is a MUCH more reliable table-boundary signal than R's own prose
# stop-words, since Stata always draws one before a table's header/body and
# often after its last data row too.
.stata_is_rule_line <- function(line) {
  tl <- trimws(line)
  nzchar(tl) && grepl("^[-┬┴├┤┌┐└┘+]+$", tl)
}

#' Extract fixed-width result tables from rendered .smcl output
#'
#' @param lines character vector: one command's rendered output lines (from
#'   [.smcl_command_chunks()])
#' @return a list of `list(title, data)`, one per detected table
#' @keywords internal
.stata_output_tables <- function(lines) {
  n <- length(lines); i <- 1L; tables <- list()
  while (i <= n) {
    if (.stata_is_rule_line(lines[[i]])) {
      # Header candidate: the line(s) immediately BEFORE this rule (a table's
      # header sits above its first {hline}), OR immediately after it (a
      # `merge`-style report table has no header row above its first rule,
      # only column labels via {col N} with no header text at all -- handled
      # by falling through to treat the very next non-rule line as a lone
      # label/value row instead).
      header_start <- i - 1L
      while (header_start >= 1L && nzchar(trimws(lines[[header_start]])) &&
             !.stata_is_rule_line(lines[[header_start]])) header_start <- header_start - 1L
      header_start <- header_start + 1L
      if (header_start >= i) { i <- i + 1L; next }  # no header text above the rule
      header_lines <- lines[header_start:(i - 1L)]

      j <- i + 1L
      data_lines <- character(0)
      while (j <= n && !.stata_is_rule_line(lines[[j]]) && nzchar(trimws(lines[[j]]))) {
        data_lines <- c(data_lines, lines[[j]])
        j <- j + 1L
      }
      # A table may end with a second {hline} rule (common: header rule +
      # body + closing rule, e.g. a `tabulate`'s "Total" row is followed by
      # nothing further, no closing rule needed) -- consume one if present.
      if (j <= n && .stata_is_rule_line(lines[[j]])) j <- j + 1L

      if (length(data_lines) >= 1) {
        cols <- .stata_split_block(c(header_lines, data_lines))
        if (!is.null(cols) && length(cols) >= 2) {
          n_header <- length(header_lines)
          header <- vapply(cols, function(cl) paste(trimws(cl[seq_len(n_header)]), collapse = " "), character(1))
          body <- lapply(cols, function(cl) cl[-seq_len(n_header)])
          df <- as.data.frame(body, stringsAsFactors = FALSE)
          nm <- trimws(header); nm[!nzchar(nm)] <- paste0("V", which(!nzchar(nm)))
          names(df) <- make.unique(nm)
          if (any(vapply(df, function(c_) any(.stata_is_numlike(c_)), logical(1)))) {
            tables[[length(tables) + 1L]] <- list(title = NA_character_, data = df)
          }
        }
      }
      i <- j; next
    }
    i <- i + 1L
  }
  tables
}

# ── One-line results (e.g. "estat gof"'s "Deviance goodness-of-fit = 367.79",
# or "Iteration 2: log pseudolikelihood = -640.68206") -- shares
# .r_stat_pattern() (R/r-output.R) since the underlying "<name> <op> <value>"
# shape is identical to R's one-line test output; not duplicated since it is
# a pure regex helper with no R-specific assumptions baked in.
.stata_output_oneline <- function(lines, source_label) {
  pp <- .r_stat_pattern()
  results <- list()
  for (ln in lines) {
    fr <- regmatches(ln, gregexpr(pp$pattern, ln, perl = TRUE))[[1]]
    if (!length(fr)) next
    stat <- character(0); val <- character(0)
    for (f in fr) {
      mm <- regmatches(f, regexec(pp$pattern, f, perl = TRUE))[[1]]
      if (length(mm) >= 5 && nzchar(trimws(mm[[2]]))) {
        stat <- c(stat, trimws(mm[[2]])); val <- c(val, mm[[5]])
      }
    }
    if (length(stat)) {
      df <- data.frame(as.list(stats::setNames(val, make.unique(stat))),
                       check.names = FALSE, stringsAsFactors = FALSE)
      results[[length(results) + 1L]] <- list(title = NA_character_, data = df)
    }
  }
  results
}

# ═══════════════════════════════════════════════════════════════════════════
# ── Top-level entry points ────────────────────────────────────────────────
# ═══════════════════════════════════════════════════════════════════════════

#' Read a Stata Markup and Control Language (.smcl) output log
#'
#' Renders the file's markup to plain text (see [.smcl_render()]), splits it
#' into one chunk per Stata command (the `{com}. <command>` echo, mirroring
#' [read_r_output()]'s R-console-echo splitting), and extracts each chunk's
#' result tables and one-line statistics. `graph export "....png"` commands
#' are recovered as ordinary command text; the PNG itself is never embedded
#' in a `.smcl` file (Stata writes it to disk separately), so no chart data
#' exists in this format to extract (see the file header).
#'
#' @param path path to a `.smcl` file
#'
#' @returns a list of result tables, each `list(analysis, title, data,
#'   syntax, table_index)` -- the same shape [read_stat_tables()] returns
#'   for `.jasp`/`.omv`/`.spv`, so all four formats can be processed
#'   identically downstream. `analysis` is the Stata command that produced
#'   the table (e.g. `"summarize communication_index barrier_index"`);
#'   `syntax` duplicates `analysis` (Stata's own echoed command IS its
#'   syntax, unlike `.spv` where syntax is recovered separately from a
#'   different structure). Empty list if the file has no recoverable
#'   command output.
#' @export
import_stata_smcl <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.smcl$", path, ignore.case = TRUE))
    stop("Not a .smcl file: ", path)

  raw_lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  rendered <- .smcl_render(raw_lines)
  chunks <- .smcl_command_chunks(rendered)
  if (!length(chunks)) return(list())

  out <- list()
  for (ch in chunks) {
    if (!length(ch$output)) next
    blocks <- c(.stata_output_tables(ch$output), .stata_output_oneline(ch$output, ch$command))
    for (b in blocks) {
      if (is.null(b$data) || !nrow(b$data) || !ncol(b$data)) next
      out[[length(out) + 1L]] <- list(
        analysis = ch$command, title = b$title %||% NA_character_,
        data = b$data, syntax = ch$command)
    }
  }
  if (!length(out)) return(list())
  for (i in seq_along(out)) out[[i]]$table_index <- i
  out
}

#' Export a Stata (.smcl) output log as standalone HTML
#'
#' Builds an HTML page from what [import_stata_smcl()] already decodes: one
#' heading per Stata command (its exact syntax, since a `.smcl` command echo
#' IS the syntax) and one `<table>` per detected result. Unlike
#' [export_jasp_html()]/[export_omv_html()] (which re-export an
#' already-rendered view) or [export_spv_html()] (which renders decoded
#' charts as images), this has no figures to embed at all: a `.smcl` file
#' never contains chart image data (see the file header) -- a `graph
#' export` command is shown as recovered command text only.
#'
#' @param path path to a `.smcl` file
#' @param out path to write the HTML file to; defaults to `path` with its
#'   extension replaced by `.html`, written alongside the source file
#'
#' @returns the path written to, invisibly
#' @export
export_stata_smcl_html <- function(path, out = NULL) {
  if (!file.exists(path)) stop("File not found: ", path)
  if (!grepl("\\.smcl$", path, ignore.case = TRUE))
    stop("Not a .smcl file: ", path)
  if (is.null(out)) out <- sub("\\.smcl$", ".html", path, ignore.case = TRUE)

  tables <- import_stata_smcl(path)

  body <- if (!length(tables)) {
    "<p>No result tables could be recovered from this .smcl file.</p>"
  } else {
    sections <- vector("list", length(tables))
    last_cmd <- NA_character_
    for (i in seq_along(tables)) {
      tb <- tables[[i]]
      heading <- ""
      if (!identical(tb$analysis, last_cmd)) {
        heading <- sprintf("<h3><code>%s</code></h3>", .stat_html_escape(tb$analysis))
        last_cmd <- tb$analysis
      }
      sections[[i]] <- paste0(heading, .spv_table_html(tb$data))
    }
    paste(unlist(sections), collapse = "\n")
  }

  html <- sprintf(paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n",
    "<title>%s</title>\n",
    "<style>\n",
    "body { font-family: sans-serif; margin: 2em; }\n",
    "h3 { border-bottom: 1px solid #888; margin-top: 2em; }\n",
    "table { border-collapse: collapse; margin-bottom: 1.5em; }\n",
    "th, td { border: 1px solid #ccc; padding: 4px 10px; font-size: 90%%; text-align: right; }\n",
    "th { background: #f0f0f0; text-align: center; }\n",
    "td:first-child, th:first-child { text-align: left; }\n",
    "</style>\n</head>\n<body>\n<h1>%s</h1>\n%s\n</body>\n</html>\n"),
    .stat_html_escape(basename(path)), .stat_html_escape(basename(path)), body)

  writeLines(html, out, useBytes = TRUE)
  invisible(out)
}

#' Recover a .smcl file's Stata syntax as a sibling .do file
#'
#' A `.smcl` file is Stata's rendered OUTPUT log, but every command it ran is
#' echoed verbatim (a `.smcl` command echo IS the exact Stata syntax, unlike
#' `.spv`'s syntax which is recovered from a SEPARATE structure element --
#' see [.spv_export_syntax()] in R/spv.R, whose role this mirrors for
#' `code_check()`). This materialises that syntax as a real `.do` file, in a
#' `code` subdirectory alongside the original `.smcl` file, so it is
#' discoverable the same way an author's own saved `.do` file would be --
#' and, crucially, so it then flows through `code_check()`'s ordinary
#' Stata-language analysis (comments, absolute paths, library lines)
#' unmodified, since `code_lang()` already recognises `.do` (see
#' `.code_expand_smcl()` in R/code_check.R, this function's only caller).
#'
#' @param smcl_path path to the `.smcl` file
#' @param code_dir_name name of the sibling code subdirectory to write into,
#'   relative to `smcl_path`'s own directory. Default `"code"`.
#' @return the path to the written `.do` file, or `NA_character_` if the
#'   file has no recoverable command echoes at all.
#' @keywords internal
.smcl_export_syntax <- function(smcl_path, code_dir_name = "code") {
  if (!file.exists(smcl_path)) stop("File not found: ", smcl_path, call. = FALSE)

  raw_lines <- tryCatch(readLines(smcl_path, warn = FALSE, encoding = "UTF-8"),
                        error = function(e) character(0))
  if (!length(raw_lines)) return(NA_character_)
  rendered <- .smcl_render(raw_lines)
  chunks <- .smcl_command_chunks(rendered)
  commands <- vapply(chunks, `[[`, character(1), "command")
  commands <- commands[nzchar(trimws(commands))]
  if (!length(commands)) return(NA_character_)

  code_dir <- file.path(dirname(smcl_path), code_dir_name)
  dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(code_dir, paste0(tools::file_path_sans_ext(basename(smcl_path)), ".do"))
  writeLines(commands, out_path, useBytes = TRUE)
  out_path
}
