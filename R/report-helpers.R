#' Set or get the report type
#'
#' Controls whether [report()]/[report_repository()] build a full, interactive
#' HTML report (light/dark theme toggle, sortable/paginated JavaScript tables,
#' tabbed sections) or a "simple" static report with none of that -- plain
#' tables, stacked headings instead of tabs, a single theme, no inline
#' `<script>` content. Email clients and mail scanners routinely flag the
#' embedded JavaScript a full report carries (even though it is inert without
#' a browser), so `report_type("simple")` is the setting to use before
#' emailing a report. [report_table()] and any module building tabbed output
#' (e.g. `data_check`'s per-file table tabset) check this setting themselves,
#' since they build their markdown/HTML while a module runs -- before
#' [report()] ever selects a template.
#'
#' @param report_type if `"full"` or `"simple"`, sets the report type;
#'   `NULL` (the default) returns the current setting without changing it
#'
#' @returns the current option value (`"full"` or `"simple"`)
#' @export
#'
#' @examples
#' report_type()
#' report_type("simple")
#' report_type("full")
report_type <- function(report_type = NULL) {
  if (is.null(report_type)) {
    return(getOption("metacheck.report_type", "full"))
  }

  report_type <- tolower(report_type[[1]])
  if (!report_type %in% c("full", "simple")) {
    stop("Set report_type with 'full' or 'simple'", call. = FALSE)
  }

  options(metacheck.report_type = report_type)
  invisible(getOption("metacheck.report_type"))
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
#' When [report_type()] is `"simple"`, this renders a plain static HTML table
#' (via `knitr::kable()`) in a CSS-only scrollable box instead of a
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

  if (identical(report_type(), "simple")) {
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
#' When [report_type()] is `"simple"`, this renders as a plain fenced div
#' with the title as a real bold line of text, not a Quarto callout: Quarto's
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

  if (identical(report_type(), "simple")) {
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
