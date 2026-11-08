# The checklist report: report_type("checklist"). Every check of every module
# is sorted by its result -- to fix, to improve, decide by hand, for
# information, fine, not applicable -- instead of being shown module by
# module, and each check that needs work says what was found and what to do
# on separate lines. Rendered like the simple report (plain Pandoc through
# rmarkdown::render(), no JavaScript), in the colours and font of metacheck's
# website (inst/templates/_report_checklist.Rmd).
#
# A module gives its checks in a `checks` data frame in its output, one row
# per check (see report_type()'s documentation for the columns). A module
# without one is shown as a single check: its traffic light gives the result,
# its summary text what was found, and its report the folded details.

# The results, in the order the report shows them: the section heading, the
# word on a check's tag, and the label of its count.
.checklist_results <- data.frame(
  status = c("fix", "improve", "decide", "info", "fine", "na"),
  heading = c("To fix", "To improve", "Decide by hand", "For information", "Fine", "Not applicable"),
  tag = c("To fix", "To improve", "Decide by hand", "Info", "Fine", "N/A"),
  count = c("to fix", "to improve", "to decide by hand", "for information", "fine", "not applicable")
)

# The result a module without a `checks` table gets, from its traffic light.
.checklist_light_status <- c(red = "fix", yellow = "improve", green = "fine",
                             info = "info", na = "na", fail = "fix")

# Every check of every module, as one data frame with the columns
# module, status, title, section, found, todo, why, note, details and
# details_title (all character; "" when a module left one out).
.checklist_checks <- function(module_output) {
  cols <- c("status", "title", "section", "found", "todo", "why", "note",
            "details", "details_title")
  rows <- lapply(module_output, \(mo) {
    checks <- mo$checks
    if (is.data.frame(checks) && nrow(checks) > 0) {
      checks <- as.data.frame(checks)
    } else {
      tl <- mo$traffic_light %||% "info"
      report <- mo$report %||% ""
      # data_check's file tree has a section of its own (.checklist_file_tree())
      if (identical(mo$module, "data_check")) report <- .file_tree_drop_from_report(report)
      report <- paste(report, collapse = "\n\n")
      summary <- paste(mo$summary_text %||% "", collapse = "\n\n")
      details <- if (tl == "fail" || report == summary) "" else report
      # an error message is plain text: as markdown, "<haven_labelled>" was
      # taken as an HTML tag and left out of the page
      found <- if (tl == "fail") {
        sprintf("This module failed to run: %s", .stat_html_escape(report))
      } else summary
      status <- unname(.checklist_light_status[tl])
      # a module with its own section shows its details there, not folded here
      note <- ""
      if (.checklist_has_section(mo)) {
        details <- ""
        note <- sprintf("See the section [%s](#%s) below for the details.",
                        mo$title %||% mo$module, .checklist_section_id(mo))
      }
      checks <- data.frame(
        status = if (is.na(status)) "info" else status,
        title = mo$title %||% mo$module,
        found = found,
        note = note,
        details = details
      )
    }
    for (col in setdiff(cols, names(checks))) checks[[col]] <- ""
    checks <- checks[cols]
    checks[] <- lapply(checks, \(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x
    })
    unknown <- !checks$status %in% .checklist_results$status
    if (any(unknown)) {
      warning(mo$module, " gave checks with an unknown status (",
              paste(unique(checks$status[unknown]), collapse = ", "),
              "); they are shown for information", call. = FALSE)
      checks$status[unknown] <- "info"
    }
    checks$module <- mo$module %||% ""
    checks
  })
  if (length(rows) == 0) {
    checks <- as.data.frame(stats::setNames(rep(list(character(0)), length(cols) + 1),
                                            c(cols, "module")))
  } else {
    checks <- do.call(rbind, unname(rows))
  }
  rownames(checks) <- NULL
  checks
}

# Whether a module gives its own section for the checklist report
# (`checklist_section`, R Markdown), and the id of that section's heading.
.checklist_has_section <- function(mo) {
  sec <- mo$checklist_section
  !is.null(sec) && length(sec) > 0 && any(nzchar(sec))
}
.checklist_section_id <- function(mo) {
  paste0("section-", gsub("[^A-Za-z0-9_-]", "-", mo$module %||% "module"))
}

# The sections modules give, each under its own heading, with the module's
# full report folded at its end.
.checklist_module_sections <- function(module_output) {
  secs <- vapply(module_output, \(mo) {
    if (!.checklist_has_section(mo)) return("")
    report <- paste(mo$report %||% "", collapse = "\n\n")
    paste(c(sprintf("## %s {#%s}", mo$title %||% mo$module, .checklist_section_id(mo)),
            paste(mo$checklist_section, collapse = "\n\n"),
            .checklist_fold(if (nzchar(report)) report else "",
                            sprintf("All details of the %s", mo$title %||% mo$module))),
          collapse = "\n\n")
  }, character(1))
  secs[nzchar(secs)]
}

# The repository's file tree, open in a section of its own: data_check's tree,
# else one drawn from repo_check's files (see .file_tree()), as in the
# repository apps' brief report. NULL when no repository was checked.
.checklist_file_tree <- function(module_output) {
  tree <- .file_tree(module_output)
  if (is.null(tree)) return(NULL)
  paste(c("## Files in the repository {#files}", tree), collapse = "\n\n")
}

# A check's folded details: <details> with its summary line. `title` falls
# back to "Show details".
.checklist_fold <- function(details, title = "") {
  if (!nzchar(details)) return("")
  if (!nzchar(title)) title <- "Show details"
  sprintf("<details><summary>%s</summary>\n\n::: {.fold}\n\n%s\n\n:::\n\n</details>",
          .spv_html_escape(title), details)
}

# One block for a check that needs work: its tag, title and section, then
# what was found, what to do, why and a note, each on its own line, then its
# folded details.
.checklist_block <- function(check) {
  res <- .checklist_results[.checklist_results$status == check$status, ]
  where <- if (nzchar(check$section)) sprintf(" [%s]{.where}", check$section) else ""
  head <- sprintf("::: {.check-head}\n[%s]{.tag .%s} [%s]{.check-title}%s\n:::",
                  res$tag, check$status, check$title, where)
  labels <- c(found = "What we found", todo = "What to do", why = "Why", note = "Note")
  rows <- vapply(names(labels), \(col) {
    if (!nzchar(check[[col]])) return("")
    sprintf("::: {.label}\n%s\n:::\n\n::: {.text%s}\n%s\n:::",
            labels[[col]], if (col == "todo") " .todo" else "", check[[col]])
  }, character(1))
  rows <- rows[nzchar(rows)]
  rows <- if (length(rows)) {
    sprintf("::: {.rows}\n%s\n:::", paste(rows, collapse = "\n\n"))
  } else ""
  sprintf("::: {.check .%s}\n%s\n\n%s\n\n%s\n:::",
          check$status, head, rows,
          .checklist_fold(check$details, check$details_title))
}

# One line for a check that is fine, not applicable or for information: its
# tag, title, what was found, what to do and a note (if any).
.checklist_row <- function(check) {
  res <- .checklist_results[.checklist_results$status == check$status, ]
  text <- c(check$found, check$todo, check$note)
  text <- paste(text[nzchar(text)], collapse = "\n\n")
  sprintf("::: {.done-row}\n::: {.tagcell}\n[%s]{.tag .%s}\n:::\n\n::: {.t}\n%s\n:::\n\n::: {.d}\n%s\n:::\n:::",
          res$tag, check$status, check$title, text)
}

# The counts at the top: one box per result except "for information", a link
# to its section when there is something in it.
.checklist_counts <- function(checks) {
  res <- .checklist_results[.checklist_results$status != "info", ]
  boxes <- vapply(seq_len(nrow(res)), \(i) {
    n <- sum(checks$status == res$status[[i]])
    inner <- sprintf("<b>%d</b><span>%s</span>", n, res$count[[i]])
    if (n > 0) {
      sprintf('<a class="count %s" href="#%s">%s</a>', res$status[[i]], res$status[[i]], inner)
    } else {
      sprintf('<span class="count %s zero">%s</span>', res$status[[i]], inner)
    }
  }, character(1))
  sprintf('<nav class="counts" aria-label="Results">%s</nav>', paste(boxes, collapse = ""))
}

# An image as a data: URI, so the report stays one file; "" when the file
# is not there.
.checklist_data_uri <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return("")
  type <- switch(tolower(tools::file_ext(path)),
                 png = "image/png", jpg = , jpeg = "image/jpeg",
                 svg = "image/svg+xml", gif = "image/gif", ttf = "font/ttf",
                 "application/octet-stream")
  paste0("data:", type, ";base64,", base64enc::base64encode(path))
}

# The text of the checklist report (R Markdown), from module output.
.report_checklist_qmd <- function(module_output, paper = list()) {
  template <- system.file("templates/_report_checklist.Rmd", package = "metacheck")
  head <- paste(readLines(template, encoding = "UTF-8"), collapse = "\n")

  # title: the module's own title when the report has one module
  title <- if (length(module_output) == 1) {
    module_output[[1]]$title %||% "MetaCheck Report"
  } else "MetaCheck Report"
  subtitle <- paper$info$title
  if (length(subtitle) == 0 || is.na(subtitle[[1]])) subtitle <- ""
  subtitle <- subtitle[[1]]

  # date, metacheck version, DOI and the commits of installed modules
  sources <- lapply(module_output, \(mo) {
    tryCatch(module_info(mo$module)$source, error = \(e) NULL)
  })
  sources <- Filter(Negate(is.null), sources)
  meta <- c(
    as.character(Sys.Date()),
    paste("Metacheck", utils::packageVersion("metacheck")),
    vapply(names(sources), \(m) {
      sprintf("module %s (commit %s)", m, substr(sources[[m]]$sha, 1, 7))
    }, character(1)),
    if (!is.na(paper$info$doi %||% NA) && nzchar(paper$info$doi)) {
      paste("DOI", paper$info$doi)
    }
  )

  # metacheck's logo, and any logo a module gives (`logo`, a file path)
  logo_paths <- c(system.file("app/www/images/logo.png", package = "metacheck"),
                  unlist(lapply(module_output, `[[`, "logo")))
  logos <- vapply(unique(logo_paths), .checklist_data_uri, character(1))
  logos <- sprintf('<img src="%s" alt="logo">', logos[nzchar(logos)])

  font <- .checklist_data_uri(system.file("app/www/fonts/Cabin.ttf", package = "metacheck"))
  fill <- c(
    PAGETITLE = gsub('(["\\\\])', "\\\\\\1", paste(c(title, subtitle[nzchar(subtitle)]), collapse = ": ")),
    FONT_DATA_URI = font,
    LOGOS = paste(logos, collapse = ""),
    TITLE = .spv_html_escape(title),
    SUBTITLE = .spv_html_escape(subtitle),
    META = .spv_html_escape(paste(meta, collapse = " \u00B7 "))
  )
  for (token in names(fill)) {
    head <- paste(strsplit(head, paste0("{{", token, "}}"), fixed = TRUE)[[1]],
                  collapse = fill[[token]])
  }

  # the checks, by result
  checks <- .checklist_checks(module_output)
  intro <- unlist(lapply(module_output, `[[`, "intro"))
  sections <- lapply(seq_len(nrow(.checklist_results)), \(i) {
    status <- .checklist_results$status[[i]]
    these <- checks[checks$status == status, , drop = FALSE]
    if (nrow(these) == 0) return(NULL)
    heading <- sprintf("## %s [(%d)]{.n} {#%s}",
                       .checklist_results$heading[[i]], nrow(these), status)
    items <- lapply(seq_len(nrow(these)), \(j) {
      if (status %in% c("fix", "improve", "decide")) {
        .checklist_block(these[j, ])
      } else .checklist_row(these[j, ])
    })
    if (!status %in% c("fix", "improve", "decide")) {
      items <- sprintf("::: {.done}\n%s\n:::", paste(items, collapse = "\n\n"))
    }
    paste(c(heading, items), collapse = "\n\n")
  })

  # more detail: the folded details of the checks shown as one line, and
  # how each module works
  done <- checks[checks$status %in% c("info", "fine", "na") & nzchar(checks$details), , drop = FALSE]
  more <- vapply(seq_len(nrow(done)), \(j) {
    .checklist_fold(done$details[[j]],
                    if (nzchar(done$details_title[[j]])) done$details_title[[j]] else done$title[[j]])
  }, character(1))
  hiw <- vapply(module_output, \(mo) {
    hiw <- .report_how_it_works(mo$module)
    if (is.null(hiw)) return("")
    .checklist_fold(paste(c(hiw$text, hiw$validation), collapse = "\n\n"),
                    sprintf("How %s works", mo$title %||% mo$module))
  }, character(1))
  more <- c(more, hiw[nzchar(hiw)])
  more <- if (length(more)) {
    sprintf("## More detail\n\n::: {.more}\n%s\n:::", paste(more, collapse = "\n\n"))
  }

  # the R chunks of scroll_table() run in a new R process, which needs the
  # report type too (see report_qmd())
  setup_chunk <- "```{r, include=FALSE}\nmetacheck::report_type(\"checklist\")\n```"
  footer <- paste0(
    '<footer><p>If there are things we can improve, please email us at ',
    '<a href="mailto:metacheck@scienceverse.org">metacheck@scienceverse.org</a> ',
    'or leave an issue at <a href="https://github.com/scienceverse/metacheck/issues">',
    'github.com/scienceverse/metacheck/issues</a>.</p></footer>')

  body <- c(
    if (length(intro)) sprintf("::: {.mc-intro}\n%s\n:::", paste(intro, collapse = "\n\n")),
    .checklist_counts(checks),
    unlist(sections),
    .checklist_module_sections(module_output),
    .checklist_file_tree(module_output),
    more
  )
  paste(head,
        setup_chunk,
        sprintf("::: {.mc-main}\n\n%s\n\n:::", paste(body, collapse = "\n\n")),
        footer,
        "\n",
        sep = "\n\n")
}
