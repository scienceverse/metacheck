## researchbox_report.R — a clean, condensed report for the ResearchBox app ##
##
## Not part of the metacheck package: local to this app only. Runs repo_check,
## code_check, and data_check, then builds a short summary of only what needs
## fixing -- no module foldouts, no full file/repository listings, no table of
## contents -- instead of the full metacheck report() output.
##
## Rendered with shiny::markdown() (CommonMark via the commonmark package)
## directly in the app, so no .qmd file and no Quarto CLI is needed. That
## means the one fold-out section below is plain HTML <details>, not Quarto's
## `::: {.callout-...}` div syntax, which CommonMark does not understand.

.rbox_report_intro <- paste(
  "[Metacheck](https://www.scienceverse.org/metacheck/) is a tool that",
  "screens scientific manuscripts and aims to identify potential issues for",
  "improvement. Our goal is to guide researchers towards best practices,",
  "especially with respect to practices that researchers easily forget, or",
  "might not have learned about yet. Metacheck is developed to help",
  "researchers correctly and completely report statistical results, will",
  "point to possibly relevant information about citations, and provides",
  "feedback about data and code sharing."
)

# Friendly phrasing for each data_check "check" name -- kept in sync by hand
# with the (unexported) check_phrase table in inst/modules/data_check.R, so
# the breakdown here reads the same way the full report would.
.rbox_check_phrase <- c(
  "Out-of-range values" = "with values outside the column's apparent range (likely data-entry errors)",
  "Miscoded missing"  = "with miscoded missing values",
  Constant            = "with a single constant value",
  "Empty column"      = "with no observed values (entirely empty)",
  "SPSS filter variable" = "holding an SPSS \"Select Cases\" filter (analyses may have used only a subset of rows)",
  "Case issues"       = "with inconsistent category casing",
  Whitespace          = "with leading/trailing whitespace",
  "Numeric as text"   = "with numeric values stored as text",
  "Problematic column name" = "whose name contains file-illegal characters or is excessively long",
  "Colliding column names" = "whose names become identical when special characters are removed",
  "Mixed encoding"    = "with values in a legacy (non-UTF-8) encoding",
  "Personal info (values)"      = "whose values look like personal information",
  "Personal info (column name)" = "whose name suggests personal information",
  "Geographic coordinates"      = "that look like geographic coordinates",
  "Free-text (may hold PII)"    = "of free text that may contain personal detail"
)

# Friendly phrasing for each check_file_naming() "bad"-severity rule -- kept in
# sync by hand with .file_naming_severity in R/file-naming.R, the same way
# .rbox_check_phrase mirrors data_check's check_phrase table above.
.rbox_naming_phrase <- c(
  spaces               = "with a space in the name",
  "special-characters" = "with a character other than letters, digits, underscore, or dash",
  diacritics           = "with non-ASCII characters (e.g. accented letters)",
  "date-format"        = "with an invalid YYYYMMDD date",
  unclassifiable       = "that could not be classified by name or extension",
  "path-length-255"    = "with a path over the 255-character limit"
)

# code_check summary bullets that only ever report good news / nothing found,
# never a problem to fix -- dropped outright (see .rbox_filter_code_bullets).
.rbox_code_drop_exact <- c(
  "All libraries/imports were loaded in one block.",
  "No absolute file paths were found.",
  "No setwd() calls were found.",
  "No code files could be checked for comments.",
  "All your code files had comments.",
  "All files loaded in the code were present in the repository.",
  "No parsing issues of R-type files were found.",
  "No packages/libraries were detected in the code."
)

## helpers ----

# Split a module's "\n- " bullet-joined summary_text (see repo_check.R /
# code_check.R in inst/modules) into individual trimmed bullet lines, dropping
# empties. Returns character(0) when there is nothing to report.
.rbox_bullets <- function(summary_text) {
  summary_text <- summary_text %||% ""
  if (!nzchar(trimws(summary_text))) return(character(0))
  parts <- strsplit(summary_text, "\n-\\s*")[[1]] |> trimws()
  parts[nzchar(parts)]
}

# Plain markdown bullet list from a character vector, or NULL when empty --
# NULL means "print nothing", matching the "say nothing if no problems" rule.
.rbox_bullet_list <- function(bullets) {
  if (length(bullets) == 0) return(NULL)
  paste0("- ", bullets, collapse = "\n")
}

# repo_check's summary_text always includes two purely factual bullets
# (file/repo counts, README counts) alongside the genuinely conditional ones
# (empty repos, archive formats, naming, ...). Drop the file-count bullet
# outright, keep the README bullet only when it actually reports a repo
# missing one, and drop the naming bullet -- it is replaced by the per-rule
# breakdown .rbox_naming_section() builds straight from naming_issues.
.rbox_filter_repo_bullets <- function(bullets) {
  bullets <- bullets[!grepl("^We found \\d+ files? in \\d+ repositor", bullets)]

  is_readme <- grepl("without READMEs\\.$", bullets)
  if (any(is_readme)) {
    n <- suppressWarnings(as.integer(
      sub(".* and (\\d+) .+ without READMEs\\.$", "\\1", bullets[is_readme])))
    drop_readme <- is_readme
    drop_readme[is_readme] <- is.na(n) | n == 0
    bullets <- bullets[!drop_readme]
  }

  bullets[!grepl("naming problem|naming suggestion", bullets, ignore.case = TRUE)]
}

# code_check's summary_text always starts with an unconditional file-count
# bullet ("We found N R, N Python, ... code files"), sometimes followed (in
# the same bullet) by a real size-cap refusal message -- strip just the count
# clause, keeping any refusal text. Every other bullet is a fixed pair of
# good-news/bad-news strings per check; drop the good-news ones by exact match
# or (for the two count-based ones) by pattern.
.rbox_filter_code_bullets <- function(bullets) {
  if (length(bullets) == 0) return(bullets)
  bullets[1] <- sub("^We found [^.]+ code files?\\.\\s*", "", bullets[1])
  bullets <- bullets[nzchar(trimws(bullets))]
  bullets <- bullets[!bullets %in% .rbox_code_drop_exact]
  bullets <- bullets[!grepl("^The code loaded \\d+ distinct package", bullets)]
  bullets[!grepl("^A pinned R/package environment was found", bullets)]
}

# Pull one "#### Heading" ... up to (not including) the next "#### " heading
# out of a module's flat `report` character vector, instead of recomputing the
# section from scratch. Returns NULL when the heading is not present (e.g. no
# files were found to build a tree from).
.rbox_extract_section <- function(report_vec, heading) {
  if (is.null(report_vec)) return(NULL)
  start <- which(report_vec == heading)
  if (length(start) == 0) return(NULL)
  start <- start[[1]]
  is_heading <- grepl("^#### ", report_vec)
  after <- which(is_heading & seq_along(report_vec) > start)
  end <- if (length(after) > 0) after[[1]] - 1 else length(report_vec)
  report_vec[start:end]
}

# Build the "## File Naming" bullet list -- one line per broken rule, naming
# every file that breaks it -- from repo_check's naming_issues data.frame
# (file_name, rule, severity, detail; see check_file_naming() in
# R/file-naming.R). Only "bad" severity is shown (the rules that "should be
# fixed"); "suggestion" rows are left out, matching how the rest of this
# report only surfaces what needs fixing. Returns NULL when there is nothing
# to fix, so the caller can print nothing (per the "say nothing if no
# problems" rule).
.rbox_naming_bullets <- function(naming_issues) {
  if (is.null(naming_issues) || nrow(naming_issues) == 0) return(NULL)
  bad <- naming_issues[naming_issues$severity == "bad", , drop = FALSE]
  if (nrow(bad) == 0) return(NULL)

  by_rule <- split(bad$file_name, bad$rule)
  parts <- vapply(names(by_rule), function(rule) {
    files <- unique(by_rule[[rule]])
    phrase <- if (rule %in% names(.rbox_naming_phrase)) .rbox_naming_phrase[[rule]] else
      sprintf("breaking the '%s' naming rule", rule)
    sprintf("%d file name%s %s: %s", length(files), plural(length(files)), phrase,
            paste(files, collapse = ", "))
  }, character(1))

  .rbox_bullet_list(unname(parts))
}

# Generic "detail fold-out" builder used for every bullet below that has a
# count-only summary line but a module return value with row-level detail
# behind it. `df` is pre-filtered to the offending rows; `fmt` is a sprintf()
# template whose %s/%d slots are filled, in order, from the columns named in
# `cols` (so each row becomes one line). Returns NULL (print nothing) when
# `df` has 0 rows, so callers can unconditionally wrap every bullet without
# each needing its own empty-check.
.rbox_detail_lines <- function(df, fmt, cols) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  args <- lapply(cols, function(col) df[[col]])
  do.call(sprintf, c(list(fmt), args))
}

# CommonMark (shiny::markdown()) does not allow a raw HTML block like
# <details> to sit as an item inside a markdown "- " list -- it breaks the
# list rendering around it (closes the <ul> early, and everything after is
# left as unparsed text; verified against shiny::markdown() output). So a
# section's bullets must stay together as ONE markdown list, and every
# fold-out that belongs to one of those bullets has to be pulled out and
# appended AFTER the whole list closes, as its own top-level block -- exactly
# how the personal-information fold-out already worked before this change.
#
# This little "accumulator" collects bullets and fold-outs separately as a
# section is built up bullet-by-bullet, so callers do not have to juggle two
# vectors by hand at every call site.
.rbox_section_new <- function() list(bullets = character(0), foldouts = character(0))

.rbox_section_add <- function(section, bullet, lines = NULL, title = NULL) {
  if (is.null(bullet)) return(section)
  section$bullets <- c(section$bullets, bullet)
  if (!is.null(lines)) {
    section$foldouts <- c(section$foldouts, .rbox_details(.rbox_bullet_list(lines), title))
  }
  section
}

# Render an accumulated section as markdown blocks: the bullet list first,
# then every fold-out as its own block. Returns NULL (nothing to add to the
# report) when there are no bullets at all.
.rbox_section_render <- function(section) {
  bullet_list <- .rbox_bullet_list(section$bullets)
  if (is.null(bullet_list)) return(NULL)
  c(bullet_list, section$foldouts)
}

# Plain HTML <details> fold-out -- CommonMark passes raw HTML blocks through
# untouched, so this renders as a native collapsible section without Quarto.
.rbox_details <- function(text, title) {
  sprintf("<details>\n<summary>%s</summary>\n\n%s\n\n</details>",
         title, paste(text, collapse = "\n\n"))
}

# Wrap a rendered report fragment as a standalone HTML page, for the "Download
# HTML" button -- the in-app view instead embeds the fragment directly.
.rbox_html_page <- function(body_html, subtitle = "") {
  sprintf('<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Metacheck ResearchBox Report</title>
<style>
body { font-family: -apple-system, "Segoe UI", Roboto, Arial, sans-serif; max-width: 900px; margin: 2em auto; padding: 0 1em; line-height: 1.5; color: #222; }
table { border-collapse: collapse; margin: 1em 0; }
th, td { border: 1px solid #ccc; padding: 0.4em 0.7em; text-align: left; }
th { background-color: #eee; }
details { margin: 1em 0; border: 1px solid #ccc; border-radius: 6px; padding: 0.5em 1em; }
summary { cursor: pointer; font-weight: bold; list-style: none; }
summary::-webkit-details-marker { display: none; }
summary::before { content: "\\25B8 "; display: inline-block; }
details[open] > summary::before { content: "\\25BE "; }
pre { background: #f5f5f5; padding: 1em; overflow-x: auto; }
h1, h2 { border-bottom: 1px solid #ddd; padding-bottom: 0.2em; }
</style>
</head>
<body>
<h1>Metacheck ResearchBox Report</h1>
<p><em>%s</em></p>
%s
</body>
</html>', htmltools::htmlEscape(subtitle), body_html)
}

## main function ----

#' Create a condensed ResearchBox report
#'
#' Runs repo_check, code_check, and data_check on a paper (normally one built
#' with `test_paper(url = <researchbox url>)`) and builds a short summary of
#' only what needs fixing: the data_check file tree (always shown), a plain
#' bullet list of repository- and code-level issues, a breakdown of file-naming
#' problems by rule (each naming every affected file) in a fold-out section, a
#' breakdown of flagged data columns by problem type, and -- only when a
#' personal-information flag occurred -- a fixed explanation table in its own
#' fold-out section. Checks that found nothing to flag contribute nothing to
#' the report.
#'
#' @param paper a paper object (e.g. from `test_paper()`)
#' @param args a list of arguments to pass to repo_check/code_check/data_check,
#'   named by module (see `report()`)
#'
#' @returns a list with `markdown` (the report source), `html` (rendered via
#'   `shiny::markdown()`), and `subtitle` (the repository URL or paper title)
researchbox_report <- function(paper, args = list()) {
  # codebook_check's scale/task naming (the LLM stages) only runs under
  # llm_use(TRUE), which this app never sets -- so including it here costs
  # nothing extra and stays rules-only. Only its coverage fields
  # (documentation matching) and scale_violations (documented-vs-actual value
  # checks) are read below; its scale/task fields are deliberately never
  # touched, so no scale extraction reaches this report either way.
  modules <- c("repo_check", "code_check", "data_check", "codebook_check")
  mo <- report_module_run(paper, modules, args)

  body <- character(0)

  ## surface module failures plainly; "na" (nothing to check) is silent ----
  for (m in modules) {
    if (identical(mo[[m]]$traffic_light, "fail")) {
      body <- c(body, sprintf(
        "> **`%s` could not be completed:** %s", m,
        mo[[m]]$report %||% mo[[m]]$summary_text %||% "Unknown error."))
    }
  }

  ## Data Tree -- always visible, replaces the file-naming detail entirely ----
  tree <- .rbox_extract_section(mo$data_check$report, "#### Data Tree")
  if (!is.null(tree)) {
    body <- c(body, "## Files", tree)
  } else if (is.null(mo$repo_check$table) || nrow(mo$repo_check$table) == 0) {
    body <- c(body, "## Files", "No files could be found in this ResearchBox.")
  }

  ## Repository issues, minus the file-naming bullet (broken out below instead) ----
  repo_bullets <- .rbox_filter_repo_bullets(.rbox_bullets(mo$repo_check$summary_text))
  repo_section <- .rbox_section_new()

  repo_tbl    <- mo$repo_check$table
  gated_repos <- mo$repo_check$gated_repos

  for (bullet in repo_bullets) {
    detail <- NULL
    title  <- NULL

    if (grepl("^We found \\d+ archive file", bullet)) {
      detail <- .rbox_detail_lines(
        repo_tbl[!is.na(repo_tbl$file_type) & repo_tbl$file_type == "archive", , drop = FALSE],
        "`%s` (in %s)", c("file_name", "repo_url"))
      title <- "Archive files -- expand to see the full list"

    } else if (grepl("without READMEs\\.$", bullet)) {
      has_readme <- repo_tbl$repo_url[!is.na(repo_tbl$file_type) & repo_tbl$file_type == "readme"]
      missing <- unique(repo_tbl$repo_url[!repo_tbl$repo_url %in% has_readme])
      detail <- if (length(missing)) sprintf("`%s`", missing) else NULL
      title <- "Repositories without a README -- expand to see the full list"

    } else if (grepl("^We found \\d+ .+ with restricted-access files", bullet)) {
      detail <- .rbox_detail_lines(
        gated_repos[!is.na(gated_repos$repo_error) & gated_repos$repo_error == "restricted access", , drop = FALSE],
        "`%s`", "repo_url")
      title <- "Repositories with restricted-access files -- expand to see the full list"

    } else if (grepl("closed with no files reachable anywhere", bullet)) {
      detail <- .rbox_detail_lines(
        gated_repos[!is.na(gated_repos$repo_error) & gated_repos$repo_error == "closed registration source", , drop = FALSE],
        "`%s`", "repo_url")
      title <- "Closed, unreachable registrations -- expand to see the full list"

    } else if (grepl("retrieved from the OSF registration instead", bullet)) {
      mirrored <- gated_repos[!is.na(gated_repos$repo_error) &
        grepl("^closed registration source \\(files retrieved from registration: ", gated_repos$repo_error), , drop = FALSE]
      if (nrow(mirrored)) {
        mirrored$registration_url <- sub(
          "^closed registration source \\(files retrieved from registration: (.+)\\)$",
          "\\1", mirrored$repo_error)
        detail <- .rbox_detail_lines(mirrored, "`%s` (files retrieved from `%s`)",
                                     c("repo_url", "registration_url"))
      }
      title <- "Closed registrations mirrored elsewhere -- expand to see the full list"

    } else if (grepl("^We could not classify \\d+ file", bullet)) {
      detail <- .rbox_detail_lines(
        repo_tbl[!is.na(repo_tbl$data_type) & repo_tbl$data_type == "unknown", , drop = FALSE],
        "`%s` (in %s)", c("file_name", "repo_url"))
      title <- "Unclassified files -- expand to see the full list"
    }

    repo_section <- .rbox_section_add(repo_section, bullet, detail, title)
  }

  repo_out <- .rbox_section_render(repo_section)
  if (!is.null(repo_out)) body <- c(body, "## Repository", repo_out)

  ## File naming, broken down by rule and fully listing the affected files,
  ## in a fold-out (a full per-file table is not printed inline) ----
  naming_list <- .rbox_naming_bullets(mo$repo_check$naming_issues)
  if (!is.null(naming_list)) {
    body <- c(body, .rbox_details(naming_list,
      "File naming problems to fix -- expand to see the full list"))
  }

  ## Code issues, with the good-news bullets filtered out, and a detail
  ## fold-out attached to each bullet that has row-level backing data ----
  code_bullets <- .rbox_filter_code_bullets(.rbox_bullets(mo$code_check$summary_text))
  code_section <- .rbox_section_new()
  code_tbl <- mo$code_check$table

  for (bullet in code_bullets) {
    detail <- NULL
    title  <- NULL

    if (grepl("^\\d+ code files? had no comments", bullet)) {
      detail <- .rbox_detail_lines(
        code_tbl[!is.na(code_tbl$percentage_comment) & code_tbl$percentage_comment == 0, , drop = FALSE],
        "`%s`", "file_name")
      title <- "Code files with no comments -- expand to see the full list"

    } else if (grepl("^\\d+ files? loaded in the code .+ missing in the repository", bullet)) {
      missing_rows <- code_tbl[!is.na(code_tbl$loaded_files_missing) &
                                  code_tbl$loaded_files_missing > 0, , drop = FALSE]
      missing_rows$n_missing_phrase <- sprintf(
        "%d file%s", missing_rows$loaded_files_missing, plural(missing_rows$loaded_files_missing))
      detail <- .rbox_detail_lines(
        missing_rows, "`%s` loads %s that could not be found in the repository: %s",
        c("file_name", "n_missing_phrase", "loaded_files_missing_names"))
      title <- "Missing files -- expand to see the full list"

    } else if (grepl("^Absolute file paths were found", bullet)) {
      detail <- .rbox_detail_lines(
        code_tbl[!is.na(code_tbl$code_abs_path) & code_tbl$code_abs_path > 0, , drop = FALSE],
        "`%s`: %s", c("file_name", "absolute_paths"))
      title <- "Absolute file paths -- expand to see the full list"

    } else if (grepl("^setwd\\(\\) calls were found", bullet)) {
      detail <- .rbox_detail_lines(
        code_tbl[!is.na(code_tbl$code_setwd) & code_tbl$code_setwd > 0, , drop = FALSE],
        "`%s`: %s", c("file_name", "setwd_calls"))
      title <- "setwd() calls -- expand to see the full list"

    } else if (grepl("^Libraries/imports were loaded in multiple places", bullet)) {
      detail <- .rbox_detail_lines(
        code_tbl[vapply(code_tbl$library_max_between > 3, isTRUE, logical(1)), , drop = FALSE],
        "`%s`", "file_name")
      title <- "Files with libraries loaded in multiple places -- expand to see the full list"

    } else if (grepl("^Parsing issues of R-type files were found", bullet)) {
      detail <- .rbox_detail_lines(
        code_tbl[!is.na(code_tbl$parse_error) & code_tbl$parse_error, , drop = FALSE],
        "`%s`: %s", c("file_name", "parse_error_msg"))
      title <- "Files with parsing issues -- expand to see the full list"
    }

    code_section <- .rbox_section_add(code_section, bullet, detail, title)
  }

  code_out <- .rbox_section_render(code_section)
  if (!is.null(code_out)) body <- c(body, "## Code", code_out)

  ## Data-quality issues, broken down by problem type ----
  findings <- mo$data_check$findings
  col_findings <- if (!is.null(findings))
    findings[!is.na(findings$column), , drop = FALSE] else NULL
  has_col_findings <- !is.null(col_findings) && nrow(col_findings) > 0

  # Values a column holds that the paper's OWN codebook/value-label scheme
  # does not allow -- see scale_violations in inst/modules/codebook_check.R.
  # Fired only against a DOCUMENTED range (never one metacheck inferred), so
  # this is a genuine documented-vs-actual discrepancy, not a guess. Kept
  # entirely separate from codebook_check's scale/task naming -- only this
  # one field is read from that module's return value here.
  scale_violations <- mo$codebook_check$scale_violations
  has_violations <- !is.null(scale_violations) && nrow(scale_violations) > 0

  if (has_col_findings || has_violations) {
    dq_section <- .rbox_section_new()

    if (has_col_findings) {
      check_counts <- col_findings |>
        dplyr::distinct(.data$source_file, .data$column, .data$check) |>
        dplyr::count(.data$check, name = "columns", sort = TRUE)

      for (i in seq_len(nrow(check_counts))) {
        chk <- check_counts$check[i]
        phrase <- if (chk %in% names(.rbox_check_phrase)) .rbox_check_phrase[[chk]] else
          sprintf("flagged by %s", tolower(chk))
        n <- check_counts$columns[i]
        bullet <- sprintf("%d column%s %s", n, plural(n), phrase)

        # detail carries the SPECIFIC reason this column was flagged (e.g. for
        # "Personal info (column name)": "matched: name" -- which keyword in
        # the name triggered it), not just the bare column name -- a name
        # like `Rand1` or `Ignore` is meaningless on its own without it.
        chk_rows <- col_findings[col_findings$check == chk, , drop = FALSE] |>
          dplyr::distinct(.data$source_file, .data$column, .data$detail)
        detail <- .rbox_detail_lines(chk_rows, "`%s` in `%s` -- %s",
                                     c("column", "source_file", "detail"))

        dq_section <- .rbox_section_add(dq_section, bullet, detail,
          sprintf("%s -- expand to see the full list", chk))
      }
    }

    if (has_violations) {
      n <- nrow(scale_violations)
      bullet <- sprintf(
        "%d column%s %s values outside the range documented in its own codebook/value labels",
        n, plural(n), if (n == 1) "has" else "have")
      detail <- .rbox_detail_lines(
        scale_violations,
        "`%s` in `%s`: documented range %s, but found %s (%s)",
        c("column", "source_file", "documented", "values", "kinds"))
      dq_section <- .rbox_section_add(dq_section, bullet, detail,
        "Values outside the documented range -- expand to see the full list")
    }

    dq_out <- .rbox_section_render(dq_section)
    body <- c(body, "## Data Quality", dq_out)
  }

  ## Documentation coverage -- how many extracted data columns are matched to
  ## a codebook/README variable definition, and which are not. Only
  ## coverage/label fields are read from codebook_check's return value here;
  ## its scale/task naming is never touched, so no scale extraction happens
  ## regardless of this module running ----
  cb_bullets <- .rbox_bullets(mo$codebook_check$summary_text)
  cb_tbl <- mo$codebook_check$table
  cb_vars <- mo$codebook_check$codebook_vars

  # codebook_check's own misalign_msg bullet restates the same "N variables,
  # M% matched" numbers the first bullet already gave, in a longer paragraph,
  # then guesses ONE generic reason (computed scores/subscales) -- a guess
  # that does not fit every case (e.g. a codebook naming columns by POSITION,
  # `var1`/`var2`/..., matches no real column name for a completely different
  # reason). Dropped here in favour of showing the actual codebook variable
  # names and data column names side by side below, so the reader sees the
  # real mismatch instead of a generic guess at its cause.
  is_misalign_bullet <- grepl("^A codebook was found and parsed", cb_bullets)
  cb_bullets <- cb_bullets[!is_misalign_bullet]

  if (length(cb_bullets) > 0 && !is.null(cb_tbl) && nrow(cb_tbl) > 0) {
    cb_section <- .rbox_section_new()

    for (bullet in cb_bullets) {
      detail <- NULL
      title  <- NULL

      if (grepl("^\\d+ of \\d+ data columns? \\(\\d+%\\) (?:is|are) documented", bullet)) {
        detail <- .rbox_detail_lines(
          cb_tbl[!is.na(cb_tbl$label_status) & cb_tbl$label_status == "unlabelled", , drop = FALSE],
          "`%s` in `%s`", c("column_name", "source_file"))
        title <- "Undocumented columns -- expand to see the full list"

      } else if (grepl("conflicting or ambiguous label", bullet)) {
        detail <- .rbox_detail_lines(
          cb_tbl[!is.na(cb_tbl$label_status) &
                   cb_tbl$label_status %in% c("conflicting_definition", "ambiguous_experiment"), , drop = FALSE],
          "`%s` in `%s`", c("column_name", "source_file"))
        title <- "Columns with a conflicting label -- expand to see the full list"
      }

      cb_section <- .rbox_section_add(cb_section, bullet, detail, title)
    }

    # When any codebook variable never matched a real column, show the two
    # name lists side by side, one data file at a time, so the reader can see
    # for themselves why they don't line up -- e.g. a codebook naming columns
    # `var1`, `var2`, ... by position instead of by the data's own column
    # names -- instead of a single guessed explanation.
    #
    # Pairing a data file to ITS OWN codebook file, not by study `group`:
    # several data files can share one `group` (e.g. a main study and its
    # supplementary-materials file both scoped to the same study code), and
    # every codebook sharing that group would otherwise get pulled into
    # EVERY file's comparison -- confirmed against a real box where three
    # unrelated data files and their three separate codebooks all shared a
    # single group. codebook_source names the parsed codebook file directly
    # (e.g. "Study 1a Data.csv___CODEBOOK.csv" for "Study 1a Data.csv"), so
    # matching on that stem is exact where the naming convention holds;
    # `group` is used only as a fallback for a file with no such pairing,
    # since it is still the best available link (imprecise, but nothing is
    # lost that group-based matching would have shown anyway).
    if (any(is_misalign_bullet) && !is.null(cb_vars) && nrow(cb_vars) > 0) {
      files <- unique(cb_tbl$source_file) |> stats::na.omit() |> as.character()
      cb_stem <- sub("___CODEBOOK\\.[A-Za-z0-9]+$", "", cb_vars$codebook_source %||% NA_character_)

      compare_lines <- unlist(lapply(files, function(f) {
        file_rows <- cb_tbl[!is.na(cb_tbl$source_file) & cb_tbl$source_file == f, , drop = FALSE]
        cols <- unique(file_rows$column_name)

        vars <- cb_vars[!is.na(cb_stem) & cb_stem == f, , drop = FALSE]
        if (nrow(vars) == 0) {
          g <- unique(stats::na.omit(file_rows$group))
          if (length(g) > 0)
            vars <- cb_vars[!is.na(cb_vars$group) & cb_vars$group %in% g, , drop = FALSE]
        }

        if (length(cols) == 0 && nrow(vars) == 0) return(NULL)
        var_lines <- if (nrow(vars) > 0) sprintf(
          "`%s` (%s)", vars$codebook_variable,
          ifelse(is.na(vars$label) | !nzchar(vars$label), "no label",
                 substr(vars$label, 1, 60))) else "(none)"
        sprintf(
          "**`%s`**\n    - Data columns: %s\n    - Codebook variables: %s",
          f,
          if (length(cols)) paste(sprintf("`%s`", cols), collapse = ", ") else "(none)",
          paste(var_lines, collapse = "; "))
      }))
      if (length(compare_lines) > 0) {
        cb_section$foldouts <- c(cb_section$foldouts, .rbox_details(
          compare_lines,
          "Codebook variables vs. data columns, by file -- expand to compare"))
      }
    }

    cb_out <- .rbox_section_render(cb_section)
    if (!is.null(cb_out)) body <- c(body, "## Documentation", cb_out)
  }

  ## assemble ----
  subtitle <- paper$url$href[[1]] %||% paper$info$title %||% ""
  md <- paste(c(.rbox_report_intro, body), collapse = "\n\n")

  list(
    markdown = md,
    html     = shiny::markdown(md),
    subtitle = subtitle
  )
}

