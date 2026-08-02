# Recognise HTML files that are rendered STATISTICAL OUTPUT ------------------
#
# A ".html" file in a research repository is, by extension alone, hopelessly
# ambiguous: it could be the researcher's rendered R Markdown / Quarto analysis
# (code + printed results), a translated Stata log, an experiment's own runner
# page (jsPsych/psiTurk task templates), or a project documentation site
# (GitHub Pages). Unlike .spv/.smcl/.jasp/.omv/.out (each unambiguous, or at
# least narrow, by extension), ".html" needs CONTENT sniffing to tell these
# apart at all -- there is no format-locked answer the way .fixed_ext_type
# gives .spv/.smcl.
#
# Two independent signals feed data_classify_files()/.fixed_ext_type's
# override (see R/data_check_helpers.R): the file's OWN NAME containing
# "output" (the same kind of naming convention codebook/readme rules already
# use), and — after download, since content can't be checked before that —
# .html_sniff_kind() below. Confirmed from real repository content (not from
# metacheck's OWN generated report/export HTML, which must never be mistaken
# for a deposited file):
#
#   * R Markdown / Quarto (pandoc/knitr) knit output: <meta name="generator"
#     content="pandoc" /> in <head>, and each input chunk as either
#     <pre class="r"><code>...</code></pre> (pandoc's classic pre-2.x
#     highlighting, e.g. RStudio's default Rmd theme) or
#     <div class="sourceCode r">... (pandoc's newer syntax-highlighting
#     extension, Quarto's default). Both were seen in real corpus files.
#     Since the input code sits verbatim (HTML-entity-escaped) inside these
#     blocks, it is fully recoverable — see .html_export_r_source() below,
#     the same "recover a sibling code file" idea as .spv_export_syntax() /
#     .smcl_export_syntax() / .mplus_export_syntax().
#
#   * A Stata log translated to HTML (`translate x.smcl x.html`) or exported
#     from Stata's Viewer: NOT YET CONFIRMED against a real corpus file (none
#     was found in the sample searched). .html_sniff_kind() looks for Stata's
#     own "." command-echo prompt at the start of a rendered line inside a
#     <pre>/<body> block, as a best-effort heuristic — flagged "stata" only
#     for classification (data_type = "output"), with NO code-recovery
#     function, since the real markup shape has not been verified. Treat this
#     branch as provisional until confirmed against a genuine example.
#
# What is deliberately EXCLUDED (seen in the same corpus sample, and must not
# be mistaken for output): jsPsych/psiTurk experiment-runner templates
# (consent/debriefing/instructions pages — task MATERIALS, not analysis
# output) and project documentation sites (a GitHub Pages docs/ tree). Neither
# carries a pandoc generator tag or an "r" code-highlighting class, so the
# signatures above do not fire on them — no separate exclusion list is needed,
# only a real trailing else-branch.

#' Sniff whether an HTML file is rendered statistical/analysis output
#'
#' Reads a LOCAL html file's head and body for the tool-specific fingerprints
#' documented in this file's header, and reports which (if any) rendering tool
#' produced it. Deliberately conservative: an unrecognised `.html` (a task
#' template, a docs site, a plain webpage) returns `NA`, never a guess.
#'
#' @param path path to a local `.html` file
#'
#' @returns a length-1 character: `"rmd"` (R Markdown/Quarto via pandoc),
#'   `"stata"` (a Stata log rendered/translated to HTML — provisional, see
#'   file header), or `NA_character_` when neither fingerprint is found (or
#'   the file cannot be read).
#' @keywords internal
.html_sniff_kind <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(NA_character_)

  # A generous but bounded read: the generator meta tag is always in the first
  # few hundred lines (the <head>), and even a large knit document's HEAD is
  # small relative to its body, so this avoids reading a multi-MB report in
  # full just to sniff it. If nothing matches in the head sample, we still
  # check for the pre/div code-block classes, which can appear later, but cap
  # that scan too (a report with no chunk in its first many thousand lines is
  # not meaningfully "code + output" regardless).
  head_lines <- tryCatch(readLines(path, n = 500, warn = FALSE, encoding = "UTF-8"),
                        error = function(e) character(0))
  if (length(head_lines) == 0) return(NA_character_)
  head_txt <- paste(head_lines, collapse = "\n")

  # "pandoc" (classic R Markdown / knitr) or "quarto" (Quarto self-identifies
  # this way in its generator tag, even though it runs pandoc underneath).
  is_pandoc_head <- grepl('generator["\']?\\s*content\\s*=\\s*["\'](pandoc|quarto)',
                          head_txt, ignore.case = TRUE, perl = TRUE)

  # The generator tag can be pushed past the 500-line sample by a very long
  # custom <head> (embedded CSS/JS); the chunk classes are the corroborating
  # signal pandoc/knitr always emits somewhere in the body, so scan further
  # for them before deciding. Two shapes (verified against real output and a
  # synthetic Quarto-style fixture — see .html_export_r_source() roxygen):
  # classic <pre class="r"><code>, and the newer syntax-highlighted form
  # where the "r" class sits on the INNER <pre> (<pre class="sourceCode r">),
  # not the outer <div class="sourceCode"> that wraps it.
  #
  # metacheck's OWN report (inst/templates/_report.qmd) is ALSO rendered with
  # Quarto, so the generator/chunk checks alone would misfire on it too — a
  # false positive confirmed directly against a real generated report during
  # testing. "metacheck@scienceverse.org" is the report template's own
  # contact-email text (in its fixed intro boilerplate, so not something a
  # researcher's own Rmd/Qmd would ever contain), checked over the SAME body
  # sample rather than a separate read.
  body_lines <- tryCatch(readLines(path, n = 20000, warn = FALSE, encoding = "UTF-8"),
                        error = function(e) character(0))
  body_txt <- paste(body_lines, collapse = "\n")
  # fixed = TRUE: the search string itself has no regex metacharacters to
  # escape (unlike the patterns below), so a LITERAL "." is used here, not
  # "\\." — with fixed = TRUE a backslash is matched as a literal backslash,
  # not treated as an escape, so "\\." would never match the real "." in
  # "metacheck@scienceverse.org" (caught only by testing against the real
  # report file, not by reasoning about the regex).
  if (grepl("metacheck@scienceverse.org", body_txt, fixed = TRUE))
    return(NA_character_)

  if (is_pandoc_head ||
      grepl('<pre class="r"><code>|<pre class="sourceCode r">',
            body_txt, perl = TRUE))
    return("rmd")

  # Stata command-echo prompt: a rendered/translated log line starting with
  # ". " (Stata's own interactive prompt) followed by a real command word,
  # inside what is otherwise a <pre>/<body>-based plain-text rendering (no
  # pandoc/knitr signature already matched above). Kept deliberately narrow
  # (anchored at a rendered line start, a command word immediately after) to
  # avoid matching an ordinary sentence that happens to contain ". word".
  if (grepl("^\\.\\s+[a-z][a-z0-9_]*\\b", body_lines, perl = TRUE) |> any())
    return("stata")

  NA_character_
}

#' Recover R source from a knitted R Markdown / Quarto HTML file
#'
#' Extracts every input-code chunk pandoc/knitr rendered into the document
#' (`<pre class="r"><code>...</code></pre>` or `<div class="sourceCode r">`),
#' in document order, and writes them — separated by a blank line, matching
#' how [code_read()] already treats whitespace between statements — as a
#' sibling `.R` file. This is the HTML analogue of [.smcl_export_syntax()] /
#' [.spv_export_syntax()]: the ORIGINAL analysis code sits verbatim inside the
#' rendered document (HTML-entity-escaped only, never altered), so it is
#' exactly recoverable, not reconstructed or guessed.
#'
#' Printed console OUTPUT (`## `-prefixed lines pandoc/knitr interleave after
#' each chunk) is deliberately NOT included: this produces a re-runnable `.R`
#' script for [code_check()]/[reproducibility_check()], and a `##`-prefixed
#' line is a comment in R syntax anyway, so leaving it out changes nothing a
#' parse would see — it is dropped here only to keep the recovered file to
#' actual code.
#'
#' @param html_path path to a local `.html` file already confirmed (by
#'   [.html_sniff_kind()]) to be `"rmd"`
#' @param code_dir_name the sibling folder to write into, same convention as
#'   [.spv_export_syntax()] / [.smcl_export_syntax()] (default `"code"`)
#'
#' @returns the path to the written `.R` file, or `NA_character_` if the
#'   document could not be read or contained no recoverable R chunk
#' @keywords internal
.html_export_r_source <- function(html_path, code_dir_name = "code") {
  if (!requireNamespace("xml2", quietly = TRUE))
    stop("recovering R source from an html file needs the 'xml2' package.",
         call. = FALSE)
  if (!file.exists(html_path)) stop("File not found: ", html_path, call. = FALSE)

  doc <- tryCatch(xml2::read_html(html_path, encoding = "UTF-8"),
                 error = function(e) NULL)
  if (is.null(doc)) return(NA_character_)

  # Both chunk markups pandoc/knitr can emit (see file header): classic
  # <pre class="r"><code> (chunk text is the <code> node's own text, already
  # HTML-entity-decoded by xml2), and the newer syntax-highlighted form, an
  # outer <div class="sourceCode"> wrapping <pre class="sourceCode r"><code>
  # whose TEXT is split across per-token <span>s (each token — keyword,
  # string, comment — its own element for syntax colouring). The "r" class
  # sits on the INNER <pre>, not the outer div (verified directly against a
  # synthetic fixture built from real Quarto/pandoc output — the div carries
  # only class="sourceCode", with no language on it at all). xml_text() on the
  # <code> node concatenates all descendant text regardless of how many spans
  # it is split across, so both shapes yield the same result: the chunk's
  # original plain-text source.
  classic <- xml2::xml_find_all(doc, "//pre[@class='r']/code")
  source_div_code <- xml2::xml_find_all(
    doc, "//pre[contains(concat(' ', @class, ' '), ' sourceCode ')" |>
      paste0(" and contains(concat(' ', @class, ' '), ' r ')]/code"))
  chunks <- c(classic, source_div_code)
  if (length(chunks) == 0) return(NA_character_)

  chunk_text <- vapply(chunks, function(nd) xml2::xml_text(nd), character(1))
  chunk_text <- chunk_text[nzchar(trimws(chunk_text))]
  if (length(chunk_text) == 0) return(NA_character_)

  code_dir <- file.path(dirname(html_path), code_dir_name)
  dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(code_dir,
                        paste0(tools::file_path_sans_ext(basename(html_path)), ".R"))
  writeLines(paste(chunk_text, collapse = "\n\n"), out_path, useBytes = TRUE)
  out_path
}
