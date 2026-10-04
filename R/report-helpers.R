#' Set or get the report type
#'
#' Controls two independent things about how [report()]/[report_repository()]
#' build a report:
#' * **rendering mechanics** -- a full, interactive HTML report (light/dark
#'   theme toggle, sortable/paginated JavaScript tables, tabbed sections) vs.
#'   a "simple" static report with none of that -- plain tables, stacked
#'   headings instead of tabs, a single theme, no inline `<script>` content.
#'   Email clients and mail scanners routinely flag the embedded JavaScript a
#'   full report carries (even though it is inert without a browser), so a
#'   "simple" report type is the setting to use before emailing a report.
#' * **content amount** -- every module's full detail vs. only the modules
#'   flagged red/yellow/fail (i.e. what needs attention), with just their
#'   one-line summary and no expandable detail, "How It Works" callout, or
#'   validation note. This is the "brief" setting, for getting to "what to
#'   improve" as fast as possible.
#'
#' These two axes combine into four values: `"full"` (default), `"brief"`,
#' `"simple"`, and `"simple_brief"`. [report_table()] and any module building
#' tabbed output (e.g. `data_check`'s per-file table tabset) check this
#' setting themselves, since they build their markdown/HTML while a module
#' runs -- before [report()] ever selects a template.
#'
#' @param report_type if one of `"full"`, `"brief"`, `"simple"`, or
#'   `"simple_brief"`, sets the report type; `NULL` (the default) returns the
#'   current setting without changing it
#'
#' @returns the current option value
#' @export
#'
#' @examples
#' report_type()
#' report_type("simple")
#' report_type("brief")
#' report_type("simple_brief")
#' report_type("full")
report_type <- function(report_type = NULL) {
  if (is.null(report_type)) {
    return(getOption("metacheck.report_type", "full"))
  }

  report_type <- tolower(report_type[[1]])
  if (!report_type %in% c("full", "brief", "simple", "simple_brief")) {
    stop("Set report_type with 'full', 'brief', 'simple', or 'simple_brief'",
      call. = FALSE)
  }

  options(metacheck.report_type = report_type)
  invisible(getOption("metacheck.report_type"))
}

# Whether a report_type value uses static, JS-free rendering (plain Pandoc via
# rmarkdown::render(), not Quarto) -- true for "simple" and "simple_brief".
.report_is_static <- function(report_type) {
  report_type %in% c("simple", "simple_brief")
}

# Whether a report_type value shows only flagged modules (red/yellow/fail)
# with summary text only, skipping full detail -- true for "brief" and
# "simple_brief".
.report_is_brief <- function(report_type) {
  report_type %in% c("brief", "simple_brief")
}

# Brief mode's to-do list needs the SPECIFIC thing to fix (e.g. the exact
# sentence an effect was called "marginally significant" in, or the exact
# imprecise p-value found) -- summary_text alone only says a module found
# something wrong, never what/where. No module returns that detail as a
# separate, standardised field (confirmed: each module's "flagged rows"
# table -- report_table, zero_table, report_table_absolute, etc. -- is a
# local variable inside the module's own code, filtered and labelled
# ad hoc, never part of its returned list), so modules are not changed.
# Instead, this reaches into module_output$report, which scroll_table()
# already builds as one or more `​```{r}` chunks per flagged table, each
# embedding that exact data frame as literal, deparsed R source (`table <-
# structure(list(...))`) for later evaluation by rmarkdown/Quarto. Since
# that source was produced by deparse() of a plain data frame inside this
# same package (never user-authored code), parsing effectively reduces to
# an inert, read-only expression -- there is no path for it to run anything
# other than reconstruct that data frame -- so evaluating it here to pull
# the data back out, instead of waiting for a later Pandoc render, is safe.
# Every module's flagged table lists one row per issue with at least one
# descriptive column (named differently per module -- "Text", "Sentence",
# "Reference", "File name", ...), so rather than guess which column is "the"
# text, every column of a row becomes one "Header: value" clause, joined
# into a single bullet -- a generic rendering that needs no per-module
# column-name convention.
# A module's flagged tables are often split across more than one
# scroll_table() call that describe the SAME rows from different angles --
# e.g. power.R emits info_table (one row per power analysis, with a
# power_type status column) and text_table (the same power_id, with the
# actual matched sentence) as two separate tables rather than one. Grouping
# each in isolation (as .report_group_table() does) would show the status
# counts with no sentence to back them up. This merges tables pairwise
# before grouping, two different ways depending on their shape:
#  - same columns (e.g. codebook_check's per-file tabset repeating an
#    identical-shaped table once per file) -> stacked into one table, so
#    the same issue repeated across files is counted once instead of once
#    per file;
#  - different columns sharing exactly one column name with mostly-matching
#    values (e.g. power.R's shared `power_id`) -> joined on that column, so
#    a status column from one table and the evidence text from another end
#    up on the same row.
# Tables that match neither case stay separate and are grouped on their own.
.report_merge_tables <- function(tbls) {
  if (length(tbls) <= 1) return(tbls)

  merged <- list(tbls[[1]])
  for (tbl in tbls[-1]) {
    last <- merged[[length(merged)]]
    if (identical(names(tbl), names(last))) {
      merged[[length(merged)]] <- rbind(last, tbl)
      next
    }

    shared <- intersect(names(tbl), names(last))
    if (length(shared) == 1) {
      key <- shared[[1]]
      # power.R's own power_id is int in one table, chr in the other --
      # coerce both sides to character so the join can match on value
      # rather than failing silently on a type mismatch.
      last[[key]] <- as.character(last[[key]])
      tbl[[key]] <- as.character(tbl[[key]])
      joined <- tryCatch(merge(last, tbl, by = key, all = TRUE),
                         error = \(e) NULL)
      if (!is.null(joined)) {
        merged[[length(merged)]] <- joined
        next
      }
    }

    merged[[length(merged) + 1]] <- tbl
  }
  merged
}

# Turns one flagged table into one bullet per distinct issue, instead of one
# bullet per row -- e.g. power.R's info_table lists one row per detected
# power analysis with a `power_type` column repeating "unknown" 3 times;
# the old row-per-bullet rendering showed that 3 times verbatim rather than
# once, as "2 items: power_type: unknown". No module marks which of its
# columns is "the issue" vs. "which row this is", so this guesses from each
# column's own shape: a column whose non-NA values repeat (fewer distinct
# values than rows) is treated as a status/issue column worth grouping and
# counting; an all-NA column is dropped outright (no information);
# everything else (every value distinct -- e.g. the matched sentence, a
# file name, a DOI) is treated as supporting evidence and quoted back for
# the group, per the user's own request to show the actual flagged
# sentence(s) a count refers to, not just the count.
.report_group_table <- function(tbl, max_quotes = 3L) {
  n <- nrow(tbl)
  if (n == 0) return(character(0))

  is_status_col <- vapply(tbl, \(col) {
    non_na <- col[!is.na(col) & nzchar(as.character(col))]
    length(non_na) > 0 && length(unique(non_na)) < n
  }, logical(1))
  all_na_col <- vapply(tbl, \(col) all(is.na(col) | !nzchar(as.character(col))),
                       logical(1))
  status_cols <- names(tbl)[is_status_col & !all_na_col]
  context_cols <- names(tbl)[!is_status_col & !all_na_col]
  # A bookkeeping id column (power_id, bib_id, row_id, ...) is neither a
  # status worth counting (it's different for every row, by definition)
  # nor evidence worth quoting (it names a row, not a fact about it) -- the
  # column that joined the two tables together in the first place (see
  # .report_merge_tables()) still ends up here otherwise, read right back
  # out as a stray "1 -- " glued onto the quoted sentence.
  is_id_col <- grepl("(^|_)id$", context_cols, ignore.case = TRUE)
  context_cols <- context_cols[!is_id_col]

  if (length(status_cols) == 0) {
    # No column repeats -- every row is its own distinct issue (e.g. each
    # row names a different missing file, or a different incoherent
    # reference) -- fall back to one bullet per row, same as before.
    return(apply(tbl, 1, \(row) {
      clauses <- ifelse(nzchar(names(row)),
                        sprintf("**%s:** %s", names(row), row),
                        row)
      sprintf("- %s", paste(clauses, collapse = " -- "))
    }))
  }

  group_key <- do.call(paste, c(tbl[status_cols], sep = "\u001f"))
  groups <- split(seq_len(n), group_key)

  vapply(groups, \(rows) {
    g_n <- length(rows)
    status_vals <- tbl[rows[1], status_cols, drop = FALSE]
    status_text <- sprintf("**%s:** %s", status_cols, status_vals) |>
      paste(collapse = ", ")
    bullet <- sprintf("- %d item%s: %s", g_n, if (g_n == 1) "" else "s",
                      status_text)

    if (length(context_cols) == 0) return(bullet)

    quote_rows <- utils::head(rows, max_quotes)
    quotes <- apply(tbl[quote_rows, context_cols, drop = FALSE], 1, \(row) {
      paste(row, collapse = " -- ")
    })
    quotes <- sprintf("  > %s", quotes)
    if (g_n > max_quotes) {
      quotes <- c(quotes, sprintf("  > ...and %d more.", g_n - max_quotes))
    }

    verb <- if (length(quote_rows) == 1) "sentence" else "sentences"
    paste(c(bullet,
           sprintf("  These issues were observed in the following %s:", verb),
           quotes),
         collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

# Named, per-module exceptions for a column whose values are not uniformly
# "a status worth reporting" -- most status columns are (every distinct
# value is something to flag, e.g. codebook_check's `Status: unlabelled`),
# but power.R's `power_type` is a classification, not a verdict:
#  - without an LLM (regex mode), a power analysis is classified as
#    "apriori"/"sensitivity"/"posthoc"/"compromise" just as often as the
#    classification fails ("unknown") -- only "unknown" is an actual
#    problem;
#  - with an LLM, power.R additionally tries to extract every one of
#    power.R's own `llm_cols` (statistical_test, sample_size, alpha_level,
#    power, effect_size, effect_size_metric, software, as well as
#    power_type again) from the text -- here a row's issue is whichever of
#    those came back NA, not its power_type value specifically.
# There is no data in the table itself that marks either case (regex
# mode's values are all equally plain strings; LLM mode's NAs look like any
# other missing value), so this is named here instead. Checked against
# `module` (the module's own file/function name, e.g. "power"), not the
# table's shape, since nothing about the table itself distinguishes this
# case from an ordinary status column.
.report_table_filters <- list(
  power = function(tbl) {
    llm_cols <- c("power_type", "statistical_test", "sample_size",
                  "alpha_level", "power", "effect_size",
                  "effect_size_metric", "software")
    present <- intersect(llm_cols, names(tbl))
    if (length(present) == 0) return(tbl)

    if (setequal(present, "power_type")) {
      # Regex mode: power_type is the only llm_cols member present, and its
      # value is a classification label, not a missingness signal -- only
      # "unknown" (classification failed) is an issue.
      return(tbl[is.na(tbl$power_type) | tbl$power_type == "unknown", ])
    }

    # LLM mode: any of these columns being NA for a row is the issue,
    # regardless of which one(s). Rows with no NA among them had every
    # essential detail and are not a to-do item.
    has_na <- Reduce(`|`, lapply(tbl[present], is.na))
    tbl[has_na, ]
  }
)

# Extracts every scroll_table()-embedded data frame from a module's report
# text, in order, with no merging or filtering -- the raw material both
# the generic pipeline (.report_merge_tables()/.report_group_table()) and
# a per-module renderer (.report_module_bullets) start from.
.report_extract_tables <- function(report) {
  chunk_pattern <- "(?s)```\\{r\\}.*?# table data -+\\s*\\n(table <- .*?)\\n\\n# display table.*?```"
  tbls <- unlist(lapply(report, \(chunk) {
    m <- gregexpr(chunk_pattern, chunk, perl = TRUE)
    codes <- regmatches(chunk, m)[[1]]
    if (length(codes) == 0) return(NULL)

    lapply(codes, \(code) {
      table_code <- sub(chunk_pattern, "\\1", code, perl = TRUE)
      tryCatch(eval(parse(text = table_code)), error = \(e) NULL)
    })
  }), recursive = FALSE)
  Filter(\(t) is.data.frame(t) && nrow(t) > 0, tbls)
}

# Finds a collapse_section() callout in a module's report text by its
# title (e.g. reproducibility_check's own "Output — <file> (<outcome>)"
# per-script dropdown, holding the real stdout/stderr transcript -- the
# only place that detail lives, never part of any table) and returns its
# body as a plain <details> block instead of the fenced-div callout
# collapse_section() itself always emits. A Quarto/Pandoc "::: {.callout}"
# fenced div is not safe to nest inside an indented markdown list item
# (the same brittleness already noted on module_report()'s own <details>
# choice over a second nested div) -- brief mode's to-do bullets are
# exactly such a list, so the callout's plain-text content is pulled back
# out and re-wrapped in a real HTML container, which nests safely because
# unlike a fenced div, a <details> element does not need a surrounding
# blank line to parse.
.report_find_callout <- function(report, title_pattern) {
  chunk <- Filter(\(x) grepl(title_pattern, x, perl = TRUE), report)
  if (length(chunk) == 0) return(NULL)

  pattern <- '(?s)::: \\{\\.callout-\\w+[^}]*title="([^"]*)"[^}]*\\}\\n\\n(.*?)\\n\\n:::'
  m <- regmatches(chunk[[1]], regexpr(pattern, chunk[[1]], perl = TRUE))
  if (!nzchar(m)) return(NULL)

  body <- sub(pattern, "\\2", m, perl = TRUE)
  # Once this body sits inside a raw <details> block, it is in CommonMark's
  # "raw HTML block" territory -- markdown syntax inside is not guaranteed
  # to be processed (confirmed live: with two ```` fenced blocks present,
  # Pandoc rendered the first as inline code and partly swallowed the
  # second into a plain paragraph instead of two <pre> blocks). Converted
  # to real HTML here instead of leaving markdown Pandoc may or may not
  # process consistently: a ```` fence becomes <pre>, **bold** becomes
  # <strong>, with the original text HTML-escaped first so a literal "<"/
  # "&" in captured stdout/stderr (e.g. "x < y") cannot be mistaken for a
  # tag.
  body <- .stat_html_escape(body)
  body <- gsub("(?s)````\\n(.*?)\\n````", "<pre>\\1</pre>", body, perl = TRUE)
  body <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", body)

  list(
    title = sub(pattern, "\\1", m, perl = TRUE),
    body  = body
  )
}

# Renders "<value> in <file>" per the user's own request, one line per
# distinct finding -- including splitting a single cell that already packs
# several values together (code_check's own convention: multiple matches
# in one file joined with ", " or " | " into one string, e.g. "file.csv,
# file.csv, file.csv" or "/lisa/file.csv | C:/lisa/file.csv"). Takes a
# two-column table directly: one column is assumed to be the file name,
# the other the finding -- the caller (.report_module_bullets$code_check)
# already knows which table and which columns those are, since it looked
# them up by name rather than guessing from shape.
.report_file_finding_bullets <- function(tbl, file_col, finding_col) {
  rows <- lapply(seq_len(nrow(tbl)), \(i) {
    file <- tbl[[file_col]][i]
    findings <- tbl[[finding_col]][i] |>
      strsplit("\\s*(,|\\|)\\s*") |>
      _[[1]]
    sprintf("%s in %s", findings, file)
  }) |> unlist()

  sprintf("- %s", rows)
}

# Whole-module renderers, tried before the generic merge/group pipeline
# (.report_merge_tables()/.report_group_table()) rather than alongside it.
# code_check.R emits ~10 scroll_table() calls across the categories in its
# report (Missing Files, Absolute Paths, setwd, install.packages, parse
# errors, plus a wide per-file overview table) -- generically merging
# these (as .report_merge_tables() does for every other module, by
# matching shared column names) joins them all into one wide table keyed
# on "File name", burying the exact "<value> in <file>" rendering the user
# asked for under a dozen unrelated columns most rows don't have. Picking
# out specifically-named tables by their own column names, instead of
# feeding everything through the generic shape-based merge, is what
# actually produces that rendering -- hence a renderer named to the module
# rather than another generic heuristic.
.report_module_bullets <- list(
  code_check = function(module_output) {
    tbls <- .report_extract_tables(module_output$report)
    bullets <- lapply(tbls, \(tbl) {
      cols <- names(tbl)
      if (identical(cols, c("File name", "Missing Files")) ||
          identical(cols, c("File name", "Absolute paths found"))) {
        return(.report_file_finding_bullets(tbl, cols[[1]], cols[[2]]))
      }
      if (identical(cols, c("File name", "Error Message"))) {
        return(.report_file_finding_bullets(tbl, "File name", "Error Message"))
      }
      # Every other code_check table (the wide per-file overview; setwd()/
      # install.packages() tables, which are rare enough in practice that
      # the generic renderer's row-per-bullet fallback is already fine) is
      # left to the generic pipeline below.
      NULL
    })
    bullets <- unlist(Filter(Negate(is.null), bullets))

    other_tbls <- Filter(\(tbl) {
      cols <- names(tbl)
      !identical(cols, c("File name", "Missing Files")) &&
        !identical(cols, c("File name", "Absolute paths found")) &&
        !identical(cols, c("File name", "Error Message")) &&
        !identical(cols, c("File Name", "% Comments", "Missing Files",
                          "Absolute Paths", "Code Between Libraries"))
    }, tbls)
    generic <- lapply(.report_merge_tables(other_tbls), .report_group_table) |>
      unlist()

    c(bullets, generic) %||% character(0)
  },
  # data_check.R's report always includes a raw-data preview (one table per
  # file, columns named after the data's own column names, e.g. "id" |
  # "dv" | "binary") and a descriptives overview (Column | Representation |
  # Level | ... | Max) -- both unconditional context about the data
  # itself, shown whether or not anything is wrong with it. The ONLY table
  # that means "here is a problem" is "Issues Identified" (File | Column |
  # Issues, built from all_issue_findings), which only exists in the report
  # text at all when something was actually flagged. Generic
  # merging/grouping has no way to tell a data preview apart from an
  # issues table -- both are "a table with repeated-looking column names"
  # -- so this looks for that one specific column signature instead, and
  # returns nothing (not even a one-row-per-preview dump) when no table
  # matches it, since "no issues" means exactly that.
  data_check = function(module_output) {
    tbls <- .report_extract_tables(module_output$report)
    issues_tbl <- Filter(\(tbl) identical(names(tbl), c("File", "Column", "Issues")),
                         tbls)
    if (length(issues_tbl) == 0) return(character(0))

    issues_tbl <- issues_tbl[[1]]
    # Issues cells are HTML built by data_check's own .dv_issue_cell() --
    # `<span title='...'>icon label</span>`, one such span per line when a
    # column has more than one distinct issue, newline-joined -- except
    # scroll_table() itself (not data_check) turns every "\n" in a
    # character column into a literal "<br>" before the table is deparsed
    # into the report text, so by the time this runs the separator between
    # two issues on the same column is "<br>", not "\n". Stripped back to
    # plain text (tag and tooltip removed) since a brief to-do line is
    # plain markdown, not raw HTML, then flattened to one (issue, column,
    # file) row per issue -- a column with two distinct issues becomes two
    # rows here, one per issue, so each groups with its own kind below
    # rather than staying bundled with an unrelated second issue on the
    # same column.
    plain_issues <- gsub("<span[^>]*>\\s*|\\s*</span>", "",
                         issues_tbl$Issues) |>
      strsplit("<br>")
    n_issues <- lengths(plain_issues)
    flat <- data.frame(
      issue  = unlist(plain_issues),
      column = rep(issues_tbl$Column, n_issues),
      file   = rep(issues_tbl$File, n_issues)
    )

    groups <- split(seq_len(nrow(flat)), flat$issue)
    vapply(groups, \(rows) {
      g_n <- length(rows)
      where <- sprintf('"%s" in "%s"', flat$column[rows], flat$file[rows])
      sprintf("- %d item%s: %s, namely %s", g_n, if (g_n == 1) "" else "s",
             flat$issue[rows[1]], paste(where, collapse = "; "))
    }, character(1), USE.NAMES = FALSE)
  },
  # repo_check.R's report is mostly unconditional inventory, same pattern as
  # data_check's raw previews: a per-repository stats table (Repository |
  # Platform | Error | All Files | ...), a full file manifest (Repository |
  # File | Size | Type), and up to one "File | Group | Path" table PER DATA
  # TYPE from its "see how every file was classified" audit section -- all
  # shown whether or not anything is wrong. The ONE table that is actual
  # findings is "File | Rule | Severity | Detail" (naming_tbl, built from
  # check_file_naming() -- confirmed every row it emits is a real rule
  # violation, never a clean/passing file, so no further filtering is
  # needed before grouping by Rule). The per-repository stats table's Error
  # column, when present and non-NA for a row (repo_check drops the column
  # entirely when every repository resolved cleanly), is also surfaced --
  # the one piece of that otherwise-inventory table that is itself a
  # problem (a private/inaccessible/failed repository).
  repo_check = function(module_output) {
    tbls <- .report_extract_tables(module_output$report)

    naming_tbl <- Filter(\(tbl) identical(names(tbl), c("File", "Rule", "Severity", "Detail")),
                         tbls)
    naming_bullets <- if (length(naming_tbl) > 0) {
      naming_tbl <- naming_tbl[[1]]
      groups <- split(seq_len(nrow(naming_tbl)), naming_tbl$Rule)
      vapply(groups, \(rows) {
        g_n <- length(rows)
        sev <- unique(naming_tbl$Severity[rows])
        sev_text <- if (length(sev) == 1) sprintf(" (%s)", sev) else ""
        sprintf('- %d item%s: %s%s, namely "%s"', g_n, if (g_n == 1) "" else "s",
               naming_tbl$Rule[rows[1]], sev_text,
               paste(naming_tbl$File[rows], collapse = '", "'))
      }, character(1), USE.NAMES = FALSE)
    } else {
      character(0)
    }

    repo_tbl <- Filter(\(tbl) "Error" %in% names(tbl) && "Repository" %in% names(tbl) &&
                         !"File" %in% names(tbl),
                       tbls)
    error_bullets <- if (length(repo_tbl) > 0) {
      repo_tbl <- repo_tbl[[1]]
      has_error <- !is.na(repo_tbl$Error) & nzchar(repo_tbl$Error)
      if (any(has_error)) {
        sprintf('- %s: "%s"', repo_tbl$Error[has_error], repo_tbl$Repository[has_error])
      } else {
        character(0)
      }
    } else {
      character(0)
    }

    c(error_bullets, naming_bullets)
  },
  # reproducibility_check.R's report has FOUR tables, two of which are
  # cleanly issues-only (missing_table: "File | Status | Reason", only ever
  # built when n_missing_inputs > 0; order_table: "Order | File | Runs
  # after | Basis", pure inventory of the run plan, always shown, never an
  # issue) -- same split as other modules. The other two are a shape
  # neither "pure inventory" nor "pure issues" fits: ONE ROW PER ITEM
  # regardless of outcome (exec_table has a row for every script, deliberately
  # including every "ran_ok" one, per the module's own comment: "for EVERY
  # script (not just failures)"; match_table has a row for every reported
  # statistic the module tried to reproduce, matched or not). For these
  # two, only the rows whose own status column marks them as NOT fine are
  # kept before grouping -- exec_table's Outcome != "ran_ok", match_table's
  # Confidence != "full" (confirmed against match-reported.R: confidence is
  # exactly "full"/"partial"/"none", "full" being every component matched).
  reproducibility_check = function(module_output) {
    tbls <- .report_extract_tables(module_output$report)

    missing_tbl <- Filter(\(tbl) identical(names(tbl), c("File", "Status", "Reason")), tbls)
    missing_bullets <- if (length(missing_tbl) > 0) {
      .report_group_table(missing_tbl[[1]])
    } else {
      character(0)
    }

    exec_tbl <- Filter(\(tbl) identical(names(tbl), c("File", "Outcome", "Detail", "Time (s)")),
                       tbls)
    exec_bullets <- if (length(exec_tbl) > 0) {
      exec_tbl <- exec_tbl[[1]]
      flagged <- exec_tbl[exec_tbl$Outcome != "ran_ok", c("File", "Outcome", "Detail")]

      # errored/timed_out are the two outcomes with a real, distinct
      # stdout/stderr transcript per file worth reading (per the user's
      # own request) -- each gets its own line plus that transcript, as
      # plain indented text (not a nested bullet list -- see
      # .report_find_callout()'s own comment on why a Quarto callout div
      # cannot safely nest inside the brief to-do list this feeds into).
      # The remaining outcomes (skipped_missing_inputs, not_parsed,
      # dependency_unavailable) never ran at all, so there is no
      # meaningfully different transcript per file -- those stay grouped
      # exactly as the generic renderer already does for every other
      # module.
      has_transcript <- flagged$Outcome %in% c("errored", "timed_out")
      transcript_rows <- flagged[has_transcript, ]
      grouped_rows <- flagged[!has_transcript, ]

      transcript_bullets <- vapply(seq_len(nrow(transcript_rows)), \(i) {
        file <- transcript_rows$File[i]
        outcome <- transcript_rows$Outcome[i]
        callout <- .report_find_callout(module_output$report,
                                        sprintf("Output — %s \\(%s\\)",
                                               gsub("([.])", "\\\\\\1", file), outcome))
        bullet <- sprintf("- %s: \"%s\"", outcome, file)
        if (is.null(callout)) return(bullet)

        details <- sprintf(
          "    <details><summary>Error details</summary>\n\n    %s\n\n    </details>",
          gsub("\n", "\n    ", callout$body)
        )
        paste(bullet, details, sep = "\n")
      }, character(1))

      c(if (nrow(grouped_rows) > 0) .report_group_table(grouped_rows) else character(0),
        transcript_bullets)
    } else {
      character(0)
    }

    match_tbl <- Filter(\(tbl) all(c("Reported", "Found", "Confidence") %in% names(tbl)), tbls)
    match_bullets <- if (length(match_tbl) > 0) {
      match_tbl <- match_tbl[[1]]
      match_tbl <- match_tbl[match_tbl$Confidence != "full", ]
      if (nrow(match_tbl) > 0) {
        # Not left to .report_group_table()'s own cardinality guess: with
        # as few rows as this table usually has, Plausible (mostly a blank
        # string) can look lower-cardinality than Confidence by sheer
        # coincidence and get grouped on instead -- Confidence (this
        # table's actual severity signal, per match-reported.R) is named
        # explicitly here rather than guessed at.
        groups <- split(seq_len(nrow(match_tbl)), match_tbl$Confidence)
        vapply(groups, \(rows) {
          g_n <- length(rows)
          where <- sprintf('"%s" (found: %s)', match_tbl$Reported[rows],
                           ifelse(nzchar(match_tbl$Found[rows]),
                                  match_tbl$Found[rows], "no"))
          sprintf("- %d item%s: confidence %s, namely %s", g_n,
                 if (g_n == 1) "" else "s", match_tbl$Confidence[rows[1]],
                 paste(where, collapse = "; "))
        }, character(1), USE.NAMES = FALSE)
      } else {
        character(0)
      }
    } else {
      character(0)
    }

    c(missing_bullets, exec_bullets, match_bullets)
  },
  # stat_effect_size.R's report has two tables that genuinely overlap: a
  # bare one-column vector of sentences missing an effect size entirely
  # (scroll_table(table_missing$text) -- unnamed, since it is a plain
  # character vector, not a data frame), and "detail_table" (explicitly
  # titled "All detected and assessed stats" in the module's own text --
  # EVERY detected test, matched and unmatched alike, inventory like
  # data_check's/repo_check's full-audit tables), which repeats those same
  # missing-effect-size sentences as rows with Effect Size == NA alongside
  # every clean one. Brief mode reports the missing sentences once (from
  # the first table, since it is already issues-only) and, from
  # detail_table, only rows coherence-checked as "no_match" -- a confirmed
  # inconsistency between the effect size and its test statistic.
  # "indeterminate" (the checker genuinely could not tell, e.g. a
  # non-integer df) is deliberately excluded per the user's own call: it
  # is not a confirmed problem, and showing it as a to-do item would read
  # as a false accusation.
  stat_effect_size = function(module_output) {
    tbls <- .report_extract_tables(module_output$report)

    missing_tbl <- Filter(\(tbl) identical(names(tbl), ""), tbls)
    missing_bullets <- if (length(missing_tbl) > 0) {
      sprintf('- missing effect size, namely "%s"', missing_tbl[[1]][[1]])
    } else {
      character(0)
    }

    detail_tbl <- Filter(\(tbl) all(c("d Coherence", "eta Coherence") %in% names(tbl)), tbls)
    coherence_bullets <- if (length(detail_tbl) > 0) {
      detail_tbl <- detail_tbl[[1]]
      no_match <- detail_tbl$`d Coherence` == "no_match" |
        detail_tbl$`eta Coherence` == "no_match"
      no_match[is.na(no_match)] <- FALSE
      if (any(no_match)) .report_group_table(detail_tbl[no_match, ]) else character(0)
    } else {
      character(0)
    }

    c(missing_bullets, coherence_bullets)
  }
)

.report_flagged_bullets <- function(module_output) {
  report <- module_output$report
  if (is.null(report) || all(report == "")) return(character(0))

  module_renderer <- .report_module_bullets[[module_output$module %||% ""]]
  if (!is.null(module_renderer)) {
    return(module_renderer(module_output) %||% character(0))
  }

  tbls <- .report_extract_tables(report)
  if (length(tbls) == 0) return(character(0))

  # Filtered AFTER merging, not before: power.R's own two tables share rows
  # via power_id (see .report_merge_tables()), and filtering info_table's
  # power_type down to "unknown" rows before that join would leave the
  # dropped rows' power_id values unmatched on the other side, surfacing as
  # a spurious "power_type: NA" group once merge(..., all = TRUE) pads them.
  merged <- .report_merge_tables(tbls)

  table_filter <- .report_table_filters[[module_output$module %||% ""]]
  if (!is.null(table_filter)) {
    merged <- lapply(merged, table_filter)
    merged <- Filter(\(t) nrow(t) > 0, merged)
    if (length(merged) == 0) return(character(0))
  }

  bullets <- lapply(merged, .report_group_table) |>
    unlist()

  bullets %||% character(0)
}

#' Make Scroll Table
#'
#' A helper function for making module reports.
#'
#' See [quarto article layout](https://quarto.org/docs/authoring/article-layout.html) for column options. The most common are "body" (centre column), "page" (span all columns"), and "margin" (only in right margin).
#'
#' To set colwidths, use a numeric or character vector. For a numeric vector, numbers greater than 1 will be interpreted as pixels, less than 1 as percents. Character vectors will be passed as is (e.g., "3em"). If you only want to specify some columns, set the others to NA, like c(200, NA, 200, NA).
#'
#' @param table the data frame to show in a table, or a vector for a list
#' @param colwidths set column widths as a vector of px (number > 1) or percent (numbers <= 1)
#' @param maxrows if the table has more rows than this, paginate
#' @param escape whether or not to escape the DT (necessary if using raw html)
#' @param column which quarto column to show tables in
#'
#' @returns the markdown R chunk to create this table
#' @export
#'
#' @examples
#' scroll_table(LETTERS)
scroll_table <- function(table,
                         colwidths = "auto",
                         maxrows = 2,
                         escape = FALSE,
                         column = "body") {
  # convert vectors to a table
  if (is.atomic(table)) {
    table <- data.frame(table)
    colnames(table) <- ""
  }

  # return nothing if no table contents
  if (is.null(table) || nrow(table) == 0 || ncol(table) == 0) {
    return("")
  }

  # replace line breaks with <br>
  for (col in names(table)) {
    if (is.character(table[[col]])) {
      table[[col]] <- gsub("\n", "<br>", table[[col]])
    }
  }

  tbl_code <- paste(deparse(table), collapse = "\n")

  column_loc <- ""
  if (column != "body") {
    column_loc <- paste0("#| column: ", column)
  }

  colwidths_code <- paste(deparse(colwidths), collapse = "\n")

  # generate markdown to create the table
  md <- sprintf(
    "
```{r}
#| echo: false
%s

# table data --------------------------------------
table <- %s

# display table -----------------------------------
metacheck::report_table(table, %s, %s, %s)
```
", column_loc,
    tbl_code,
    colwidths_code,
    maxrows,
    ifelse(isTRUE(escape), "TRUE", "FALSE")
  )

  return(md)
}

#' Display a Table in a Report
#'
#' A function to display tables in reports.
#'
#' When [report_type()] is `"simple"` or `"simple_brief"`, this renders a
#' plain static HTML table (via `knitr::kable()`) in a CSS-only scrollable
#' box instead of a
#' `DT::datatable()` widget -- no JavaScript at all, so the report stays safe
#' to email. Every row is kept and reachable by scrolling (just like the full
#' report's paginated widget, just without the JS), except for a pathologically
#' large table, which is truncated to [.report_table_simple_max_rows] rows with
#' a note, so one huge table cannot make the report unboundedly large.
#'
#' @param table the data frame to show in a table, or a vector for a list
#' @param colwidths set column widths as a vector of px (number > 1) or percent (numbers <= 1)
#' @param maxrows if the table has more rows than this, paginate (full report)
#'   or show a scrollbar sized to approximately this many rows (simple report)
#' @param escape whether or not to escape the table (necessary if using raw html)
#'
#' @returns the datatable (full report) or a knitr::kable object (simple report)
#' @export
#'
#' @examples
#' report_table(iris)
report_table <- function(table, colwidths = "auto", maxrows = 2, escape = FALSE) {
  # replace line breaks with <br>
  for (col in names(table)) {
    if (is.character(table[[col]])) {
      table[[col]] <- gsub("\n", "<br>", table[[col]])
    }
  }

  # let col names break at _
  names(table) <- gsub("_", "_<wbr>", names(table))

  if (.report_is_static(report_type())) {
    return(.report_table_static(table, colwidths, maxrows, escape))
  }

  # set up columnDef
  if (length(colwidths) == 1 && colwidths == "auto") {
    cd_code <- list()
  } else {
    # set up column width definitions
    # colwidths <- rep_len(colwidths, ncol(table))
    cd <- lapply(seq_along(colwidths), \(i) {
      x <- colwidths[[i]]
      if (is.na(x)) {
        return(NULL)
      }

      if (is.numeric(x)) {
        if (x > 1) {
          x <- paste0(x, "px")
        } else {
          x <- paste0(x * 100, "%")
        }
      }
      # targets are 0-based
      list(targets = i - 1, width = x)
    })
    cd_code <- cd[!sapply(cd, is.null)]
  }

  # set up options
  dom <- ifelse(nrow(table) > maxrows, "<'top' p>", "t")
  options <- list(
    dom = dom,
    autoWidth = TRUE,
    ordering = FALSE,
    pageLength = maxrows,
    columnDefs = cd_code
  )

  DT::datatable(table,
    options,
    selection = "none",
    rownames = FALSE,
    escape = escape
  )
}

# Largest number of rows a single table keeps in a simple-mode report before
# being truncated with a note -- an upfront safety cap, not the normal
# behaviour (a report-sized table is expected to stay well under this; this
# only guards against a pathologically large one making the file unboundedly
# large, since simple mode has no JS pagination to hide rows behind).
.report_table_simple_max_rows <- 500L

# Plain, static HTML table for report_type("simple"): knitr::kable() (no
# JavaScript, no htmlwidgets dependency) wrapped in a CSS-only scrollable box
# (plain `overflow` -- works in every browser and the vast majority of email
# clients, with no scripting involved) sized to roughly `maxrows` visible rows,
# so a long table is still fully reachable by scrolling rather than hidden
# behind JS-driven pagination. Column width is applied via inline <col> tags,
# since kable() itself has no per-column width option.
.report_table_static <- function(table, colwidths = "auto", maxrows = 2,
                                 escape = FALSE) {
  n_total <- nrow(table)
  truncated <- n_total > .report_table_simple_max_rows
  if (truncated) {
    table <- utils::head(table, .report_table_simple_max_rows)
  }

  kbl <- knitr::kable(table, format = "html", escape = isTRUE(escape),
                      row.names = FALSE, table.attr = 'class="dt-static"')
  kbl <- as.character(kbl)

  # Inject a <colgroup> right after the opening <table ...> tag when explicit
  # widths were requested -- kable() has no column-width argument, and this is
  # simpler than hand-building the whole table from scratch.
  if (!(length(colwidths) == 1 && identical(colwidths, "auto"))) {
    widths <- vapply(seq_len(ncol(table)), function(i) {
      x <- if (i <= length(colwidths)) colwidths[[i]] else NA
      if (is.na(x)) return("")
      if (is.numeric(x)) x <- if (x > 1) paste0(x, "px") else paste0(x * 100, "%")
      sprintf(' style="width:%s"', x)
    }, character(1))
    colgroup <- paste0("<colgroup>",
                       paste0("<col", widths, ">", collapse = ""),
                       "</colgroup>")
    kbl <- sub("(<table[^>]*>)", paste0("\\1", colgroup), kbl)
  }

  # A fixed-height scroll box roughly sized to `maxrows` visible rows (~2.5em
  # each, plus the header) when the table has more rows than that -- small
  # tables render at their natural height with no scrollbar at all.
  box <- if (n_total > maxrows) {
    height <- sprintf("%.0fem", (maxrows + 1) * 2.5)
    sprintf('<div style="max-height:%s;overflow:auto;">%s</div>', height, kbl)
  } else {
    kbl
  }

  note <- if (truncated) sprintf(
    "\n\n*Showing the first %d of %d rows.*\n",
    .report_table_simple_max_rows, n_total) else ""

  # asis_output(), not a bare string: this is the last value of an R chunk
  # with no `results: 'asis'` chunk option, and knitr auto-print()s a plain
  # character vector as quoted/escaped text rather than passing it through to
  # Pandoc. asis_output() is what DT::datatable()'s own print.htmlwidget and
  # knitr::kable()'s own print method rely on under the hood for the SAME
  # purpose in the full-report path -- this makes report_table()'s simple-mode
  # branch behave identically from the chunk's point of view.
  knitr::asis_output(paste0(box, note))
}

#' Make Collapsible Section
#'
#' A helper function for making module reports.
#'
#' When [report_type()] is `"simple"` or `"simple_brief"`, this renders as a
#' plain fenced div with the title as a real bold line of text, not a Quarto
#' callout: Quarto's
#' `title`/`collapse` fenced-div attributes are Quarto-specific -- plain
#' Pandoc (what [report()]'s simple-mode render uses, see its own docs)
#' passes an unrecognised attribute straight through as a literal (invisible)
#' HTML attribute rather than rendering it as content, and
#' expanding/collapsing needs Bootstrap's collapse JavaScript, which a simple
#' report never includes. The content is always shown inline, uncollapsed.
#'
#' @param text The text to put in the collapsible section; vectors will be collapse with line breaks between (e.g., into paragraphs)
#' @param title The title of the collapse header
#' @param callout the type of quarto callout block
#' @param collapse whether to collapse the block at the start
#'
#' @returns text
#' @export
#'
#' @examples
#' text <- c("Paragraph 1...", "Paragraph 2...")
#' collapse_section(text) |> cat()
collapse_section <- function(text, title = "Learn More",
                             callout = c("tip", "note", "warning", "important", "caution"),
                             collapse = TRUE) {
  callout <- match.arg(callout)
  body <- paste0(text, collapse = "\n\n")

  if (.report_is_static(report_type())) {
    fmt <- '::: {.callout-%s}\n\n**%s**\n\n%s\n\n:::\n'
    return(sprintf(fmt, callout, title, body))
  }

  fmt <- '::: {.callout-%s title="%s" collapse="%s"}\n\n%s\n\n:::\n'
  sprintf(
    fmt,
    callout,
    title,
    ifelse(collapse, "true", "false"),
    body
  )
}

#' Pluralise
#'
#' Helper function for conditional plurals. For example, if you want to return "1 error" or "2 errors", you can use this in a sprintf().
#'
#' @param n the number
#' @param singular the word or ending when n = 1
#' @param plural the word or ending n != 1
#'
#' @returns a string
#' @export
#'
#' @examples
#' n <- 0:3
#' sprintf("I have %d friend%s", n, plural(n))
#' sprintf("I have %d %s", n, plural(n, "octopus", "octopi"))
plural <- function(n, singular = "", plural = "s") {
  ifelse(n == 1, singular, plural)
}


# Format a number for a cap message: whole numbers plainly (no scientific
# notation, even for large MB values like 31908), fractional numbers to one
# decimal (34.4). Never uses exponent form, which reads badly for file sizes.
.cap_num <- function(x) {
  if (is.na(x)) return("unknown")
  if (!is.finite(x)) return("Inf")
  if (isTRUE(all.equal(x, round(x))))
    format(round(x), trim = TRUE, scientific = FALSE, big.mark = "")
  else formatC(x, format = "f", digits = 1)
}


#' Build a "cannot process this many items" count-cap message
#'
#' When a unit of work (a repository, a codebook file, a survey) has more items
#' than a count cap allows, the whole unit is skipped rather than truncated.
#' Returns `NULL` when within the cap.
#'
#' Note this is a COUNT cap, and skipping is the right response because the
#' items are not independently useful — half a codebook's LLM chunks describe
#' half its variables. Download SIZE limits behave differently: they fill to
#' the budget smallest-file-first and report what was omitted (see
#' `download_repo_files()`).
#'
#' @param n_needed the number of items the unit actually has
#' @param param the name of the parameter that caps this (e.g. `"codebook_max_calls"`)
#' @param current the current value of that parameter
#' @param unit a noun for one item (e.g. `"tabular data file"`)
#' @param context a label for the unit of work skipped (e.g. the repo URL)
#' @param action the verb for what was skipped (e.g. `"extract"`, `"analyse"`)
#'
#' @returns a single string, or `NULL` when `n_needed <= current`
#' @export
#' @keywords internal
cap_gate_count <- function(n_needed, param, current, unit = "item",
                           context = NULL, action = "process") {
  if (is.null(n_needed) || is.na(n_needed) || n_needed <= current) return(NULL)
  where <- if (!is.null(context) && nzchar(context)) sprintf(" for %s", context) else ""
  sprintf(
    paste0("%d %s%s%s exceed%s the `%s` cap of %s. ",
           "Set `%s >= %d` to %s them; %s was skipped."),
    n_needed, unit, plural(n_needed), where,
    if (n_needed == 1) "s" else "",
    param, .cap_num(current),
    param, n_needed, action,
    if (!is.null(context) && nzchar(context)) context else "this unit")
}



#' Make an html link
#'
#' @param url the URL to link to
#' @param text the text to link
#' @param new_window whether to open in a new window
#' @param type handle common links, like "doi" ()
#'
#' @returns string
#' @export
#'
#' @examples
#' link("https://scienceverse.org")
link <- function(url, text = url, new_window = TRUE, type = "") {
  if (type == "doi") {
    url <- gsub("https?://doi.org/", "", url) |>
      sprintf("https://doi.org/%s", x = _)
  }

  nw <- ""
  text <- gsub("^https?://", "", text)
  if (new_window) nw <- " target='_blank'"
  links <- sprintf(
    "<a href='%s'%s>%s</a>",
    url, nw, text
  )
  links[is.na(url)] <- NA

  return(links)
}


#' Format Reference
#'
#' Format a reference for display in a report.
#'
#' The argument `bib` should be a bibentry object (e.g., like those made by `citation()`, but it can also handle a bibtex object or a bibtex formatted character vector. If these do not read in as valid bibtex, the original text of bib will be returned unformatted.
#'
#' @param bib a bibentry object or list of bibentry objects
#'
#' @returns formatted text
#' @export
#'
#' @examples
#' mc <- citation("metacheck")
#' format_ref(mc)
#'
#' # handles bibtext
#' bib_mc <- utils::toBibtex(mc)
#' format_ref(bib_mc)
#'
#' paper <- demopaper()
#' format_ref(paper$bib$ref[1:2])
format_ref <- function(bib) {
  if (!all(sapply(bib, inherits, "bibentry"))) {
    # try parsing as bibtex
    tmpfile <- tempfile(fileext = ".bib")
    writeLines(bib, tmpfile)
    bib <- tryCatch(bibtex::read.bib(tmpfile),
      error = \(e) {
        return(bib)
      }
    )
  }

  # handle list of bibentries
  if (!inherits(bib, "bibentry") && is.list(bib)) {
    bib <- Reduce(c, bib)
  }

  formatted <- tryCatch({
      format(bib, style = "html")
    },
    error = \(e) {
      md <- format(bib, style = "md")
      return(md)
    })

  # tidy up
  gsub("\\n|<p>|</p>", " ", formatted) |> trimws()
}
