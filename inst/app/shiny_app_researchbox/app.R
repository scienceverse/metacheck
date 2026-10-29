## app.R — Check a ResearchBox only: paste a URL, get a report ##
##
## Shiny Server edition. Deploy by copying this folder to the Shiny Server app
## directory (a sibling of the PDF report and repository-check apps). The
## report is metacheck's brief report plus a folded file tree (see
## brief_report.R), rendered with Quarto (see render_report.R); progress is
## shown step by step (see shiny_progress.R). Each check runs in its own R
## process (see background_job.R), so a long check does not freeze the app
## for other visitors.
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

# before the tabs, which use api_keys_box()
source("api_keys.R")
source("tabs/researchbox_check.R")
source("tabs/researchbox_options.R")
source("render_report.R")
source("background_job.R")

ensure_quarto()

# The modules run on the box, and what the progress bar says while each runs.
# codebook_check stays rules-only: its LLM steps only run under llm_use(TRUE),
# which this app never sets.
RBOX_MODULES <- c(
  repo_check     = "Downloading and listing the box's files",
  code_check     = "Checking code files",
  data_check     = "Checking data files",
  codebook_check = "Checking codebooks"
)

# One check, run in its own R process (see background_job.R): it starts with
# nothing loaded, so it loads metacheck and the helper files itself. Returns
# the path of the rendered report.
researchbox_job <- function(paper, modules, args, htmlpath) {
  suppressPackageStartupMessages(library(metacheck))
  source("render_report.R")
  source("brief_report.R")
  source("shiny_progress.R")
  use_shiny_progress()
  progress_step(0.05, "Starting the check...")
  # The modules fill the bar from 10% to 90%, with a line under it for every
  # step metacheck reports (see shiny_progress.R).
  shiny_progress_modules(modules, from = 0.1, to = 0.9)
  render_brief_report(paper, names(modules), htmlpath, args = args)
  progress_step(0.95, "Preparing the report for display...")
  htmlpath
}

## concurrency control ----
# Each check runs in its own R process, so without a limit every visitor could
# start one at the same time. Same on-disk counter as the repository-check
# app, in its own directory so the two apps have separate limits.
MAX_CONCURRENT_CHECKS <- 2L
SLOT_STALE_SECONDS    <- 1800   # running checks refresh their slot (see bg_watch)
SLOT_DIR <- local({
  cand <- file.path("/tmp", "metacheck_researchbox_slots")
  ok <- tryCatch(dir.create(cand, showWarnings = FALSE, recursive = TRUE) ||
                   dir.exists(cand), error = function(e) FALSE)
  if (!ok || file.access(cand, 2) != 0) {
    cand <- file.path(tempdir(), "researchbox_slots")
    dir.create(cand, showWarnings = FALSE, recursive = TRUE)
  }
  cand
})

slots_in_use <- function() {
  slots <- list.files(SLOT_DIR, full.names = TRUE)
  if (length(slots)) {
    ages <- as.numeric(Sys.time()) -
      vapply(slots, function(f) as.numeric(file.info(f)$mtime), numeric(1))
    stale <- slots[is.na(ages) | ages > SLOT_STALE_SECONDS]
    if (length(stale)) unlink(stale, recursive = TRUE)
    slots <- setdiff(slots, stale)
  }
  length(slots)
}

slot_acquire <- function() {
  if (slots_in_use() >= MAX_CONCURRENT_CHECKS) return("")
  slot <- file.path(SLOT_DIR, paste0(Sys.getpid(), "-", as.integer(Sys.time()),
                                     "-", sample.int(1e6, 1)))
  if (!dir.create(slot, showWarnings = FALSE)) return("")
  if (slots_in_use() > MAX_CONCURRENT_CHECKS) {
    unlink(slot, recursive = TRUE); return("")
  }
  slot
}

slot_release <- function(slot) {
  if (nzchar(slot)) unlink(slot, recursive = TRUE)
}


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
#6018" (with or without a trailing slash). A peer-review link's passcode
# ("researchbox.org/6018&PEER_REVIEW_passcode=ABCDEF") is kept: without it a
# box that is not public cannot be read. Returns NA_character_ when the
# input matches none of these.
normalize_rbox_url <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(NA_character_)
  if (grepl("^[0-9]+$", x)) {
    return(paste0("https://researchbox.org/", x))
  }
  m <- regmatches(x, regexpr("(?:https?://)?researchbox\\.org/[0-9]+(?:&PEER_REVIEW_passcode=[A-Za-z0-9]+)?",
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

  output$api_key_notice <- renderUI(api_key_notice("researchbox", session))

  ## reactiveVals ----
  report_html    <- reactiveVal(NULL)   # the rendered report, as raw bytes
  report_url     <- reactiveVal("")     # where the in-page frame loads it from
  report_running <- reactiveVal(FALSE)
  report_error   <- reactiveVal("")

  ### options_done — return to the Check tab ----
  observeEvent(input$options_done, {
    updateTabItems(session, "tabs", "researchbox_tab")
  })

  ### options_update — go to the Options tab ----
  observeEvent(input$options_update, {
    updateTabItems(session, "tabs", "researchbox_options_tab")
  })

  # Stops this session's running check, if any (set below). A visitor who
  # leaves mid-check frees the server: the check's process is stopped and its
  # slot and files are released.
  stop_job <- NULL
  session$onSessionEnded(function() {
    if (!is.null(stop_job)) stop_job()
  })

  ### run_check ----
  # The check runs in its own R process (researchbox_job(), via
  # background_job.R), so this app stays responsive for other visitors.
  observeEvent(input$run_check, {
    if (isTRUE(report_running())) {
      showNotification("A check is already running in this session. Please wait for it to finish.",
                       type = "warning")
      return(invisible())
    }

    url <- normalize_rbox_url(input$rbox_url)

    if (is.na(url)) {
      report_error(paste0("'", input$rbox_url, "' is not a valid ResearchBox ",
                          "URL or ID. Try something like ",
                          "https://researchbox.org/6018."))
      return(NULL)
    }

    slot <- slot_acquire()
    if (!nzchar(slot)) {
      report_error(sprintf("The server is busy checking other ResearchBoxes (limit %d at a time). Please try again in a few minutes.",
                           MAX_CONCURRENT_CHECKS))
      return(NULL)
    }

    report_html(NULL)
    report_url("")
    report_error("")
    report_running(TRUE)

    # A minimal paper whose only content is the ResearchBox link, so
    # repo_check finds it exactly as it would in a real manuscript.
    paper <- test_paper(url = url)
    paper$info$title <- url

    local_path <- trimws(input$local_path %||% "")
    has_local  <- nzchar(local_path) &&
                    (file.exists(local_path) || dir.exists(local_path))
    max_fs   <- input$max_file_size %||% 100
    max_ds   <- input$max_download_size %||% 500
    keys     <- api_keys_entered(input, "researchbox")
    # Files fetched with a visitor's own key are not kept in the cache the
    # next visitors' checks read from.
    cache_dl <- isTRUE(input$cache_downloads) && length(keys) == 0

    size_args <- list(max_file_size = max_fs, max_download_size = max_ds,
                      cache = cache_dl)
    if (has_local) size_args$local_path <- local_path

    module_args <- list(
      repo_check = if (has_local) list(local_path = local_path) else list(),
      code_check = size_args,
      data_check = size_args
    )

    htmlpath <- tempfile(fileext = ".html")

    # Runs once, when the check ends in any way (done, failed, or the visitor
    # left).
    cleanup <- function() {
      slot_release(slot)
      report_running(FALSE)
      if (file.exists(htmlpath)) unlink(htmlpath)
      stop_job <<- NULL
    }

    job <- bg_start(researchbox_job, list(
      paper = paper, modules = RBOX_MODULES, args = module_args,
      htmlpath = htmlpath
    ), keys = keys)
    stop_watch <- bg_watch(job, session, "Generating report",
      heartbeat = slot,
      on_done = function(path) {
        report_html(readBin(path, "raw", n = file.info(path)$size))
        # Anonymous usage statistics: one finished report, and how many check
        # modules produced it. Recorded here, after the render succeeded, so
        # a failed attempt is not counted as a report.
        usage_record("report", 1L, length(RBOX_MODULES))
        # Served to the in-page frame from this session's memory only.
        report_url(session$registerDataObj(
          name = "metacheck_researchbox_report", data = report_html(),
          filter = function(data, req) shiny::httpResponse(200, "text/html; charset=utf-8", data)
        ))
        cleanup()
      },
      on_error = function(msg) {
        report_error(msg)
        cleanup()
      }
    )
    stop_job <<- function() {
      stop_watch()
      cleanup()
    }
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

    modules_code <- paste0('c("', paste(names(RBOX_MODULES), collapse = '", "'), '")')

    code <- paste0(
      'library(metacheck)\n\n',
      '# a minimal paper that just links to the ResearchBox\n',
      'paper <- test_paper(url = "', url, '")\n\n',
      '# run the checks and write the brief report (this app also adds\n',
      '# the folded list of files, which report() does not)\n',
      'args <- list(\n',
      '  code_check = ', size_args_code, ',\n',
      '  data_check = ', size_args_code, '\n',
      ')\n',
      'report(paper, modules = ', modules_code, ',\n',
      '       report_type = "brief", args = args,\n',
      '       output_file = "researchbox_report.html")'
    )

    tags$pre(tags$code(code, .noWS = "outside"), .noWS = "outside")
  })

  ### report_status_ui ----
  output$report_status_ui <- renderUI({
    running <- report_running()
    have    <- !is.null(report_html()) && nzchar(report_url())
    err     <- report_error()

    if (running) {
      shiny::p(icon("spinner", class = "fa-spin"), " Generating report, please wait...")
    } else if (nzchar(err)) {
      shiny::p(style = "color: #c0392b;", icon("circle-xmark"), " Error: ", err)
    } else if (have) {
      tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
                 " Report generated successfully -- see below."),
        tags$div(class = "report-view-actions",
          tags$a(href = report_url(), target = "_blank", class = "btn btn-default",
                 icon("up-right-from-square"), " Open in new tab"),
          downloadButton("report_dl", "Download HTML")
        ),
        tags$div(class = "report-save-note", icon("triangle-exclamation"),
          tags$b(" Save your report to keep it. "),
          "The report is not stored on the server; it is gone when you close or ",
          "reload this tab. Use ", tags$b("Download HTML"), " to keep a copy.")
      )
    } else {
      NULL
    }
  })

  ### report_content_ui — the report itself, shown in-page ----
  output$report_content_ui <- renderUI({
    url <- report_url()
    if (is.null(report_html()) || !nzchar(url)) return(NULL)
    tags$iframe(src = url, class = "report-frame")
  })

  ### report_dl ----
  output$report_dl <- downloadHandler(
    filename = function() "metacheck_researchbox_report.html",
    content  = function(file) {
      bytes <- report_html()
      if (!is.null(bytes)) writeBin(bytes, file)
    }
  )

} # end server()

shinyApp(ui, server)

