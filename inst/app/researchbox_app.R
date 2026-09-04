## researchbox_app.R — Check a ResearchBox only: paste a URL, get a report ##
suppressPackageStartupMessages({
  # Tab files (tabs/researchbox_check.R, tabs/researchbox_options.R) call
  # metacheck's own exported functions unqualified (e.g. test_paper()).
  # shiny::runApp() sources them into the global env, which only has
  # metacheck on the search path if the caller already ran library(metacheck)
  # themselves -- see report_app.R and
  # https://github.com/scienceverse/metacheck/issues/320.
  library(metacheck)
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
})

options(shiny.maxRequestSize = 100 * 1024^2,
        scipen = 10,
        digits = 4)

source("tabs/researchbox_check.R")
source("tabs/researchbox_options.R")
source("researchbox_report.R")


## usage statistics ----
#
# ANONYMOUS USAGE STATISTICS. See report_app.R and osf_app.R for the full
# rationale; this app keeps the same kind of record, in its own file, so all
# three can be reported together. Nothing about the ResearchBox itself (its
# ID, title, or contents) is recorded, only that a report was generated and
# how many check modules ran.
usage_file <- function(filename = "researchbox_app_usage.csv") {
  dir <- Sys.getenv("METACHECK_USAGE_DIR")
  if (!nzchar(dir)) {
    dir <- rappdirs::user_data_dir("metacheck", "scienceverse")
  }
  ok <- tryCatch({
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dir.exists(dir) && file.access(dir, mode = 2) == 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) return("")
  file.path(dir, filename)
}

usage_record <- function(event, reports = 0L, modules = 0L) {
  if (Sys.getenv("SHINY_PORT") == "") return(invisible(NULL))
  tryCatch({
    path <- usage_file()
    if (!nzchar(path)) return(invisible(NULL))   # nowhere writable
    row <- data.frame(
      date    = format(Sys.Date()),          # date only, never a timestamp
      event   = event,                       # "session" or "report"
      reports = as.integer(reports),
      modules = as.integer(modules)
    )
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }, error = function(e) NULL, warning = function(w) NULL)
  invisible(NULL)
}

#' Read the anonymous usage statistics this app has recorded
#'
#' Returns one row per day with the number of sessions, the number of reports
#' produced, and the number of check modules run. See the note above for
#' exactly what is and is not recorded.
usage_summary <- function() {
  path <- usage_file()
  if (!nzchar(path) || !file.exists(path)) {
    return(data.frame(date = character(0), sessions = integer(0),
                      reports = integer(0), modules = integer(0)))
  }
  x <- utils::read.csv(path)
  dates <- sort(unique(x$date))
  data.frame(
    date     = dates,
    sessions = vapply(dates, function(d) sum(x$date == d & x$event == "session"), 0L),
    reports  = vapply(dates, function(d) sum(x$reports[x$date == d]), 0L),
    modules  = vapply(dates, function(d) sum(x$modules[x$date == d]), 0L),
    row.names = NULL
  )
}


## normalise a pasted URL or bare ID to a full ResearchBox URL ----
# Accepts "6018", "researchbox.org/6018", or a full "https://researchbox.org/
#6018" (with or without a trailing slash). Returns NA_character_ when the
# input matches none of these.
normalize_rbox_url <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(NA_character_)
  if (grepl("^[0-9]+$", x)) {
    return(paste0("https://researchbox.org/", x))
  }
  m <- regmatches(x, regexpr("(?:https?://)?researchbox\\.org/[0-9]+",
                             x, perl = TRUE, ignore.case = TRUE))
  if (length(m) == 0 || !nzchar(m)) return(NA_character_)
  if (!grepl("^https?://", m, ignore.case = TRUE)) m <- paste0("https://", m)
  m
}


## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck ResearchBox"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Check ResearchBox", tabName = "researchbox_tab",
               icon = icon("box-archive"), selected = TRUE),
      menuItem("Options", tabName = "researchbox_options_tab",
               icon = icon("sliders"))
    ),
    HTML("<img src='images/logo.png' alt='Logo' style='width: 85%; margin: 1em;' />")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      researchbox_tab,
      researchbox_options_tab
    )
  )
)


## server ----
server <- function(input, output, session) {
  # One row per visit. See the note above: this counts visits, not people.
  usage_record("session")

  ## reactiveVals ----
  report_result  <- reactiveVal(NULL)   # list(markdown, html, subtitle)
  report_running <- reactiveVal(FALSE)
  report_error   <- reactiveVal("")

  # The modules that read the files inside a repository, all taking the same
  # local_path argument, run here on the box found from the pasted URL alone.
  repo_modules <- c("repo_check", "code_check", "data_check")

  ### options_done — return to the Check tab ----
  observeEvent(input$options_done, {
    updateTabItems(session, "tabs", "researchbox_tab")
  })

  ### options_update — go to the Options tab ----
  observeEvent(input$options_update, {
    updateTabItems(session, "tabs", "researchbox_options_tab")
  })

  ### run_check ----
  observeEvent(input$run_check, {
    report_result(NULL)
    report_error("")
    report_running(TRUE)

    url <- normalize_rbox_url(input$rbox_url)

    if (is.na(url)) {
      report_error(paste0("'", input$rbox_url, "' is not a valid ResearchBox ",
                          "URL or ID. Try something like ",
                          "https://researchbox.org/6018."))
      report_running(FALSE)
      return(NULL)
    }

    tryCatch({
      withProgress(message = "Generating report", value = 0, {
        incProgress(0.1, detail = "Preparing...")
        # A minimal paper whose only content is the ResearchBox link, so
        # repo_check finds it exactly as it would in a real manuscript.
        paper <- test_paper(url = url)

        incProgress(0.1, detail = "Running modules...")
        local_path <- trimws(input$local_path %||% "")
        has_local  <- nzchar(local_path) &&
                        (file.exists(local_path) || dir.exists(local_path))
        max_fs   <- input$max_file_size %||% 100
        max_ds   <- input$max_download_size %||% 500
        cache_dl <- isTRUE(input$cache_downloads)

        size_args <- list(max_file_size = max_fs, max_download_size = max_ds,
                          cache = cache_dl)
        if (has_local) size_args$local_path <- local_path

        module_args <- list(
          repo_check = if (has_local) list(local_path = local_path) else list(),
          code_check = size_args,
          data_check = size_args
        )

        incProgress(0.6, detail = "Summarising results...")
        result <- researchbox_report(paper, args = module_args)

        incProgress(0.2, detail = "Done!")

        # Anonymous usage statistics: one finished report, and how many check
        # modules produced it. Recorded here, after the render succeeded, so
        # a failed attempt is not counted as a report.
        usage_record("report", 1L, length(repo_modules))
      })

      report_result(result)

    }, error = function(e) {
      report_error(e$message)
    }, finally = {
      report_running(FALSE)
    })
  }, ignoreNULL = TRUE)

  ### r_code ----
  output$r_code <- renderUI({
    url <- normalize_rbox_url(input$rbox_url)
    if (is.na(url)) url <- "https://researchbox.org/6018"

    local_path_snip <- trimws(input$local_path %||% "")
    has_local_snip <- nzchar(local_path_snip) &&
      (file.exists(local_path_snip) || dir.exists(local_path_snip))
    # A Windows path pasted in as C:\Users\... would put \U into the string
    # literal below, which R cannot parse. R accepts forward slashes on
    # Windows, so show the path that way in the copyable code.
    local_path_snip <- gsub("\\", "/", local_path_snip, fixed = TRUE)

    max_fs   <- input$max_file_size %||% 100
    max_ds   <- input$max_download_size %||% 500
    cache_dl <- isTRUE(input$cache_downloads)
    local_arg <- if (has_local_snip) sprintf(', local_path = "%s"', local_path_snip) else ""

    size_args_code <- sprintf(
      'list(max_file_size = %s, max_download_size = %s, cache = %s%s)',
      max_fs, max_ds, cache_dl, local_arg
    )

    code <- paste0(
      'library(metacheck)\n',
      'source("researchbox_report.R") # the condensed report this app uses\n\n',
      '# a minimal paper that just links to the ResearchBox\n',
      'paper <- test_paper(url = "', url, '")\n\n',
      '# run repo_check, code_check, and data_check, and write a condensed report\n',
      'args <- list(\n',
      '  code_check = ', size_args_code, ',\n',
      '  data_check = ', size_args_code, '\n',
      ')\n',
      'researchbox_report(paper, args = args)'
    )

    tags$pre(tags$code(code, .noWS = "outside"), .noWS = "outside")
  })

  ### report_status_ui ----
  output$report_status_ui <- renderUI({
    running <- report_running()
    result  <- report_result()
    err     <- report_error()

    if (running) {
      shiny::p(icon("spinner", class = "fa-spin"), " Generating report, please wait...")
    } else if (nzchar(err)) {
      shiny::p(style = "color: #c0392b;", icon("circle-xmark"), " Error: ", err)
    } else if (!is.null(result)) {
      tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
                 " Report generated successfully -- see below."),
        downloadButton("report_dl", "Download HTML")
      )
    } else {
      NULL
    }
  })

  ### report_content_ui — the report itself, rendered in-page ----
  output$report_content_ui <- renderUI({
    result <- report_result()
    if (is.null(result)) return(NULL)
    div(class = "rbox-report", result$html)
  })

  ### report_dl ----
  output$report_dl <- downloadHandler(
    filename = function() "metacheck_researchbox_report.html",
    content  = function(file) {
      result <- report_result()
      if (is.null(result)) return(NULL)
      writeLines(.rbox_html_page(as.character(result$html), result$subtitle), file)
    }
  )

} # end server()

shinyApp(ui, server)

