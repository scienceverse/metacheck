## app.R --------------------------------------------------------------------
## metacheck repository check app -- Shiny Server edition.
##
## Runs only repo_check, code_check and data_check on a code/data repository,
## with no manuscript. Two inputs:
##   * a link to an online repository (GitHub, OSF, Zenodo, Dryad, ...): the
##     server fetches the files itself, nothing is uploaded by the user;
##   * a .zip of a local folder, uploaded through the browser.
##
## Deploy by copying this folder to the Shiny Server app directory (a sibling
## of the PDF report app). Quarto is shared with that app (see render_report.R).
## The report is metacheck's brief report plus a folded file tree (see
## brief_report.R); progress is shown step by step (see shiny_progress.R).
## Each check runs in its own R process (see background_job.R), so a long
## check does not freeze the app for other visitors.

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(metacheck)
})

source("render_report.R")
source("background_job.R")
source("api_keys.R")

## --- limits -----------------------------------------------------------------
# Sized for the shared server (~196 GB disk, 4 cores). The worst case on disk
# is MAX_CONCURRENT_CHECKS jobs each holding an uploaded zip plus its extracted
# contents (or a downloaded repository), i.e. about 2 x (500 + 1000) MB.
MAX_UPLOAD_MB          <- 500    # compressed .zip upload
MAX_EXTRACTED_MB       <- 1000   # extracted zip contents (zip-bomb guard, 2x upload)
MAX_ZIP_FILES          <- 20000  # entries in an uploaded zip
MAX_FILE_MB            <- 100    # per file fetched from an online repository
MAX_DOWNLOAD_MB        <- 500    # per online repository (metacheck's own budget)
MIN_FREE_DISK_MB       <- 10000  # refuse new jobs when less than this is free
MAX_CONCURRENT_CHECKS  <- 2L
CHECK_COOLDOWN_SECONDS <- 30
SLOT_STALE_SECONDS     <- 1800   # repository checks can run much longer than PDFs

MODULES <- c("repo_check", "code_check", "data_check")

# The services whose keys a visitor can enter (see api_keys.R)
KEY_SERVICES <- c("github", "osf", "dryad", "researchbox", "gitlab")

# What the progress bar says while each module runs.
MODULE_LABELS <- c(
  repo_check = "Listing the repository's files",
  code_check = "Downloading and checking code files",
  data_check = "Downloading and checking data files"
)

# One check, run in its own R process (see background_job.R): it starts with
# nothing loaded, so it loads metacheck and the helper files itself. Returns
# the path of the rendered report.
repocheck_job <- function(paper, modules, labels, args, htmlpath, cache_dir,
                          work_dir = NULL) {
  suppressPackageStartupMessages(library(metacheck))
  source("render_report.R")
  source("brief_report.R")
  source("shiny_progress.R")
  # An uploaded .zip is checked from the folder that holds it, so the report
  # names it by its own folder name, not by a server path (#500). Changed only
  # after the app's own files above are sourced, which are found relative to
  # the app's folder.
  if (!is.null(work_dir)) setwd(work_dir)
  use_shiny_progress()
  progress_step(0.05, "Starting the check...")
  # Keep every file metacheck downloads inside this check's own folder.
  options(metacheck.repo_cache.dir = cache_dir)
  # The modules fill the bar from 10% to 90%, with a line under it for every
  # step metacheck reports (see shiny_progress.R).
  shiny_progress_modules(labels, from = 0.1, to = 0.9)
  render_brief_report(paper, modules, htmlpath, args = args)
  progress_step(0.95, "Preparing the report for display...")
  htmlpath
}

options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2,
        scipen = 10,
        digits = 4)

ensure_quarto()

## --- usage statistics -------------------------------------------------------
# Anonymous counts only (date, event, number of reports/modules), kept to
# justify continued development in grant applications. No identifier, no
# repository URL, no file names. Only the hosted app records anything. Same
# scheme as the PDF report app, in its own file in the same folder.
usage_file <- function(filename = "shiny_app_repocheck_usage.csv") {
  dir <- Sys.getenv("METACHECK_USAGE_DIR")
  if (!nzchar(dir)) dir <- rappdirs::user_data_dir("metacheck", "scienceverse")
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
    if (!nzchar(path)) return(invisible(NULL))
    row <- data.frame(date = format(Sys.Date()), event = event,
                      reports = as.integer(reports), modules = as.integer(modules))
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }, error = function(e) NULL, warning = function(w) NULL)
  invisible(NULL)
}

## --- disk space -------------------------------------------------------------
# Free space (MB) on the filesystem holding `path`, via POSIX df. NA if df is
# unavailable (e.g. running locally on Windows), in which case the check is
# skipped rather than blocking every job.
disk_free_mb <- function(path = tempdir()) {
  out <- tryCatch(system2("df", c("-Pk", shQuote(path)), stdout = TRUE,
                          stderr = FALSE),
                  error = function(e) character(0), warning = function(w) character(0))
  if (length(out) < 2) return(NA_real_)
  fields <- strsplit(trimws(out[length(out)]), "\\s+")[[1]]
  avail_kb <- suppressWarnings(as.numeric(fields[4]))
  if (is.na(avail_kb)) NA_real_ else avail_kb / 1024
}

# Free space after reserving room for the jobs already running.
disk_headroom_ok <- function(running_jobs) {
  free <- disk_free_mb()
  if (is.na(free)) return(TRUE)
  reserved <- running_jobs * (MAX_UPLOAD_MB + MAX_EXTRACTED_MB)
  free - reserved >= MIN_FREE_DISK_MB
}

## --- error message translation ---------------------------------------------
explain_error <- function(msg) {
  msg <- paste(msg, collapse = " ")
  if (!nzchar(msg) || is.na(msg)) {
    return("The check could not be completed (the process was interrupted or timed out). Please try again in a few minutes.")
  }
  m <- tolower(msg)
  if (grepl("entity too large|payload too large|413|maximum upload", m)) {
    return(sprintf("Your .zip file is too large (the limit is %d MB). Remove large data files or outputs you do not need checked, and try again.", MAX_UPLOAD_MB))
  }
  if (grepl("holds no files", m)) {
    return("The .zip holds no files. Please zip the folder that holds your files, and upload that file.")
  }
  if (grepl("zip file is corrupt|cannot open zip|error -1|not a zip|zip file cannot be opened", m)) {
    return("The uploaded file could not be opened as a .zip archive. Please zip your folder again (e.g. right-click > Compress / Send to > Compressed folder) and upload that file.")
  }
  if (grepl("could not resolve|connection refused|connection (timed out|reset)|failed to connect", m)) {
    return("The repository host could not be reached right now. Please check the link and try again in a few minutes.")
  }
  if (grepl("rate limit|429", m)) {
    return("The repository host is rate-limiting requests right now. Please try again later.")
  }
  if (grepl("404|not found", m)) {
    return("The repository could not be found. Please check that the link is correct and that the repository is public.")
  }
  paste0("Something went wrong while checking the repository: ", msg)
}

## --- concurrency control ----------------------------------------------------
# Same on-disk semaphore as the PDF report app, in its own directory so the
# two apps have separate caps.
SLOT_DIR <- local({
  cand <- file.path("/tmp", "metacheck_repocheck_slots")
  ok <- tryCatch(dir.create(cand, showWarnings = FALSE, recursive = TRUE) ||
                   dir.exists(cand), error = function(e) FALSE)
  if (!ok || file.access(cand, 2) != 0) {
    cand <- file.path(getwd(), ".repocheck_slots")
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

## --- input preparation ------------------------------------------------------
# Check an uploaded zip without extracting it: size after extraction, number
# of entries, and no path that would escape the extraction folder.
check_zip <- function(zip_path) {
  listing <- tryCatch(utils::unzip(zip_path, list = TRUE),
                      error = function(e) NULL, warning = function(w) NULL)
  if (is.null(listing)) {
    # unzip() cannot open a .zip without entries, which is only the
    # end-of-archive record, starting "PK\5\6" (Lakens/tue_datamanager_check#4, item 8)
    sig <- tryCatch(readBin(zip_path, "raw", 4), error = function(e) raw(0))
    if (identical(sig, as.raw(c(0x50, 0x4b, 0x05, 0x06)))) stop("The .zip holds no files.", call. = FALSE)
    stop("zip file cannot be opened", call. = FALSE)
  }
  # a .zip of an empty folder holds only folder entries
  if (all(grepl("[/\\\\]$", listing$Name))) stop("The .zip holds no files.", call. = FALSE)
  if (nrow(listing) > MAX_ZIP_FILES) {
    stop(sprintf("The .zip contains %d files; the limit is %d. Please remove files you do not need checked (e.g. renv/, node_modules/, .git/).",
                 nrow(listing), MAX_ZIP_FILES), call. = FALSE)
  }
  extracted_mb <- sum(listing$Length, na.rm = TRUE) / 1024^2
  if (extracted_mb > MAX_EXTRACTED_MB) {
    stop(sprintf("The .zip unpacks to %.0f MB; the limit is %d MB. Please remove large data or output files you do not need checked, and try again.",
                 extracted_mb, MAX_EXTRACTED_MB), call. = FALSE)
  }
  bad <- grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", listing$Name) |
    grepl("^([/\\\\]|[A-Za-z]:)", listing$Name)
  if (any(bad)) stop("The .zip contains unsafe file paths and cannot be processed.", call. = FALSE)
  invisible(extracted_mb)
}

# Unzip into `dest` and return the repository root: the single top-level
# folder when the zip wraps everything in one, otherwise `dest` itself.
extract_zip <- function(zip_path, dest) {
  utils::unzip(zip_path, exdir = dest)
  unlink(file.path(dest, "__MACOSX"), recursive = TRUE)
  top <- list.files(dest, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  if (length(top) == 1 && dir.exists(top)) top else dest
}

normalise_repo_url <- function(url) {
  url <- trimws(url %||% "")
  if (!nzchar(url)) stop("Please enter the link to an online repository.", call. = FALSE)
  if (nchar(url) > 500) stop("That link is too long.", call. = FALSE)
  if (!grepl("^https?://", url, ignore.case = TRUE)) url <- paste0("https://", url)
  if (!grepl("^https?://[A-Za-z0-9.-]+\\.[A-Za-z]{2,}(/|$)", url)) {
    stop("That does not look like a web link. Please paste the full repository URL, e.g. https://github.com/user/repo or https://osf.io/abcde.", call. = FALSE)
  }
  url
}

download_args <- list(max_file_size = MAX_FILE_MB, max_download_size = MAX_DOWNLOAD_MB)


## UI -------------------------------------------------------------------------
check_tab <- tabItem(
  tabName = "check_tab",
  h2("Check a Code and Data Repository"),
  tags$p(
    "This app runs the repository checks of ",
    tags$a("Metacheck", href = "https://www.scienceverse.org/metacheck/", target = "_blank"),
    " on a code and data repository: ", tags$b("repo_check"),
    " (what is in the repository and how it is organised), ", tags$b("code_check"),
    " (R, Rmd, Qmd, SAS, SPSS and Stata scripts) and ", tags$b("data_check"),
    " (data files and codebooks). No manuscript is needed."
  ),
  tags$p(
    "The report is short: it lists only the checks that found something to ",
    "improve, and what to change. The repository's files are listed in a ",
    "folded section you can open."
  ),
  tags$p(
    "See the ",
    tags$a("Metacheck Manual", href = "https://www.scienceverse.org/metacheck_book", target = "_blank"),
    " for what each check does. You can report ",
    tags$a("bugs", href = "https://github.com/scienceverse/metacheck/issues", target = "_blank"), "."
  ),
  tags$div(class = "capacity-note capacity-warn",
    icon("triangle-exclamation"),
    tags$b(" Large repositories can take a long time. "),
    "A small repository is checked in a minute or two; one with many files or ",
    "large data files can take ", tags$b("ten minutes or more"),
    ". Please keep this tab open while it runs. This is a shared research ",
    "server, so only a few checks run at the same time; if it is busy, please ",
    "try again in a few minutes."
  ),
  uiOutput("busy_banner"),
  uiOutput("api_key_notice"),
  box(title = "Repository", collapsible = FALSE, width = 12,
    radioButtons("input_mode", NULL,
      choiceNames = list(
        "Link to an online repository (GitHub, OSF, Zenodo, Dryad, ...)",
        "Upload a folder from my computer as a .zip file"
      ),
      choiceValues = list("url", "zip"),
      selected = "url"
    ),
    conditionalPanel(
      condition = "input.input_mode == 'url'",
      textInput("repo_url", NULL, width = "100%",
                placeholder = "https://github.com/user/repo  or  https://osf.io/abcde"),
      tags$p(class = "upload-hint",
        "Nothing is uploaded from your computer: the server fetches the public ",
        "files from the repository itself. ",
        sprintf("Files over %d MB are skipped, and at most %d MB is fetched per repository (the smallest files first).",
                MAX_FILE_MB, MAX_DOWNLOAD_MB)),
      actionButton("run_url", "Check Repository", icon = icon("magnifying-glass"),
                   class = "btn-options-done")
    ),
    conditionalPanel(
      condition = "input.input_mode == 'zip'",
      tags$div(class = "pdf-upload",
        fileInput("repo_zip", NULL, multiple = FALSE, accept = ".zip", width = "100%",
                  buttonLabel = tagList(icon("upload"), "Upload .zip"),
                  placeholder = "No file selected")
      ),
      tags$p(class = "upload-hint",
        sprintf("Zip the repository folder and upload the .zip (max %d MB, at most %d MB once unpacked). ",
                MAX_UPLOAD_MB, MAX_EXTRACTED_MB),
        "Leave out folders you do not need checked, such as .git/, renv/library/ ",
        "or large raw data. The check starts as soon as the upload finishes.")
    )
  ),
  api_keys_box(KEY_SERVICES),
  box(title = "Privacy", collapsible = TRUE, collapsed = TRUE, width = 12,
    tags$ul(
      tags$li("\U0001F310 For an online repository, the server contacts that repository's host (e.g. GitHub, OSF, Zenodo) to list and download its public files."),
      tags$li("\U0001F512 An uploaded .zip and its unpacked files are kept only while the check runs and are deleted immediately afterwards."),
      tags$li("\U0001F512 The report is held in this browser session's memory only and is never stored on the server."),
      tags$li("\U0001F512 No LLM is used.")
    )
  ),
  uiOutput("check_status_ui")
)

report_view_tab <- tabItem(
  tabName = "report_view_tab",
  h2("Your Repository Report"),
  uiOutput("report_view_ui")
)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck repo"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Check Repository", tabName = "check_tab",
               icon = icon("folder-open"), selected = TRUE),
      menuItem("Report", tabName = "report_view_tab", icon = icon("eye"))
    ),
    HTML("<img src='images/logo.png' alt='Logo' style='width: 85%; margin: 1em;' />")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(check_tab, report_view_tab)
  )
)


## server ---------------------------------------------------------------------
server <- function(input, output, session) {
  usage_record("session")

  report_html   <- reactiveVal(NULL)
  report_url    <- reactiveVal("")
  running       <- reactiveVal(FALSE)
  check_error   <- reactiveVal("")
  last_finished <- reactiveVal(0)

  output$busy_banner <- renderUI({
    invalidateLater(5000, session)
    inuse <- slots_in_use()
    if (!disk_headroom_ok(inuse)) {
      tags$div(class = "busy-banner busy-full", icon("hard-drive"),
        " The server is low on disk space right now. Please try again later.")
    } else if (inuse >= MAX_CONCURRENT_CHECKS) {
      tags$div(class = "busy-banner busy-full", icon("hourglass-half"),
        sprintf(" The server is at capacity (%d of %d checks running). Please try again in a few minutes.",
                inuse, MAX_CONCURRENT_CHECKS))
    } else if (inuse > 0) {
      tags$div(class = "busy-banner busy-some", icon("spinner", class = "fa-spin"),
        sprintf(" %d of %d check slots in use. Capacity available.",
                inuse, MAX_CONCURRENT_CHECKS))
    } else NULL
  })

  output$api_key_notice <- renderUI(api_key_notice(KEY_SERVICES, session))

  # Stops this session's running check, if any (set by run_check()).
  stop_job <- NULL
  # A visitor who leaves mid-check frees the server: the check's process is
  # stopped and its slot and files are released.
  session$onSessionEnded(function() {
    if (!is.null(stop_job)) stop_job()
  })

  # Shared runner for both inputs. `prepare(workdir)` returns
  # list(paper = , args = ) and may put files in `workdir`, which is always
  # deleted afterwards. The check itself runs in its own R process
  # (repocheck_job(), via background_job.R), so this app stays responsive.
  run_check <- function(prepare, cleanup_paths = character(0)) {
    if (isTRUE(running())) {
      showNotification("A check is already running in this session. Please wait for it to finish.",
                       type = "warning")
      unlink(cleanup_paths)
      return(invisible())
    }
    wait <- CHECK_COOLDOWN_SECONDS - (as.numeric(Sys.time()) - last_finished())
    if (wait > 0) {
      showNotification(sprintf("Please wait %d more seconds before starting another check.",
                               ceiling(wait)), type = "warning")
      unlink(cleanup_paths)
      return(invisible())
    }
    slot <- slot_acquire()
    if (!nzchar(slot)) {
      check_error(sprintf("The server is busy checking other repositories (limit %d at a time). Please try again in a few minutes.",
                          MAX_CONCURRENT_CHECKS))
      unlink(cleanup_paths)
      return(invisible())
    }
    if (!disk_headroom_ok(slots_in_use() - 1L)) {
      slot_release(slot)
      check_error("The server is low on disk space right now. Please try again later.")
      unlink(cleanup_paths)
      return(invisible())
    }

    report_html(NULL); report_url(""); check_error(""); running(TRUE)
    workdir  <- tempfile("repocheck_"); dir.create(workdir)
    htmlpath <- tempfile(fileext = ".html")

    # Runs once, when the check ends in any way (done, failed, or the
    # visitor left).
    cleanup <- function() {
      slot_release(slot)
      last_finished(as.numeric(Sys.time()))
      running(FALSE)
      unlink(workdir, recursive = TRUE, force = TRUE)
      unlink(cleanup_paths)
      if (file.exists(htmlpath)) unlink(htmlpath)
      stop_job <<- NULL
    }

    # Preparing is quick (building the paper object; for a .zip, checking and
    # unpacking it), so it stays in this process, where its errors are shown.
    prepared <- tryCatch(prepare(workdir), error = function(e) e)
    if (inherits(prepared, "error")) {
      check_error(explain_error(conditionMessage(prepared)))
      cleanup()
      return(invisible())
    }

    job <- bg_start(repocheck_job, list(
      paper = prepared$paper, modules = MODULES,
      labels = MODULE_LABELS[MODULES], args = prepared$args,
      htmlpath = htmlpath, cache_dir = file.path(workdir, "cache"),
      work_dir = prepared$work_dir
    ), keys = api_keys_entered(input, KEY_SERVICES))
    stop_watch <- bg_watch(job, session, "Checking repository",
      heartbeat = slot, max_minutes = 60,
      on_done = function(path) {
        report_html(readBin(path, "raw", n = file.info(path)$size))
        usage_record("report", 1L, length(MODULES))
        report_url(session$registerDataObj(
          name = "metacheck_repo_report", data = report_html(),
          filter = function(data, req) shiny::httpResponse(200, "text/html; charset=utf-8", data)
        ))
        updateTabItems(session, "tabs", "report_view_tab")
        cleanup()
      },
      on_error = function(msg) {
        check_error(explain_error(msg))
        cleanup()
      }
    )
    stop_job <<- function() {
      stop_watch()
      cleanup()
    }
  }

  observeEvent(input$run_url, {
    url <- tryCatch(normalise_repo_url(input$repo_url), error = function(e) {
      check_error(conditionMessage(e)); NULL
    })
    if (is.null(url)) return(invisible())
    run_check(function(workdir) {
      paper <- metacheck::test_paper(text = url, url = url)
      paper$info$title <- url
      list(paper = paper,
           args = list(code_check = download_args, data_check = download_args))
    })
  })

  observeEvent(input$repo_zip, {
    req(input$repo_zip)
    zip_path <- input$repo_zip$datapath
    zip_name <- tools::file_path_sans_ext(basename(input$repo_zip$name))
    run_check(function(workdir) {
      check_zip(zip_path)
      # repo_check names a local repository by the path it is given, so the
      # report showed the server's temporary folder ("Repository:
      # /tmp/RtmpAbC123/repocheck_1a2b3c/repo/my_study", #500). The zip is
      # unpacked into a folder named after it, and the check runs from the
      # folder above, with only the repository's own folder name as
      # local_path ("Repository: my_study").
      folder <- gsub("[^A-Za-z0-9._ -]", "_", zip_name)
      if (!nzchar(gsub("[ ._-]", "", folder))) folder <- "repository"
      repo_dir <- extract_zip(zip_path, file.path(workdir, "repo", folder))
      unlink(zip_path)
      paper <- metacheck::test_paper()
      paper$info$title <- zip_name
      list(paper = paper, work_dir = dirname(repo_dir),
           args = list(repo_check = list(local_path = basename(repo_dir), local_only = TRUE)))
    }, cleanup_paths = zip_path)
  }, ignoreNULL = TRUE)

  output$check_status_ui <- renderUI({
    err  <- check_error()
    have <- !is.null(report_html()) && nzchar(report_url())
    if (running()) {
      shiny::p(icon("spinner", class = "fa-spin"),
               " Checking the repository, please wait. Large repositories can take ten minutes or more.")
    } else if (nzchar(err)) {
      shiny::p(style = "color: #c0392b;", icon("circle-xmark"), " Error: ", err)
    } else if (have) {
      tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
                 " Report generated. It has opened in the ", tags$b("Report"), " tab."),
        actionButton("report_view", "View Report", icon = icon("eye")),
        downloadButton("report_dl", "Download HTML"),
        tags$div(class = "report-save-note", icon("triangle-exclamation"),
          tags$b(" Save your report to keep it. "),
          "The report is not stored on the server; it is gone when you close or ",
          "reload this tab. Use ", tags$b("Download HTML"), " to keep a copy.")
      )
    } else NULL
  })

  observeEvent(input$report_view, updateTabItems(session, "tabs", "report_view_tab"))

  output$report_view_ui <- renderUI({
    url <- report_url()
    if (is.null(report_html()) || !nzchar(url)) {
      return(shiny::p(class = "report-empty", icon("circle-info"),
        " No report yet. Check a repository on the ", tags$b("Check Repository"),
        " tab; the report will appear here when it is ready."))
    }
    tagList(
      tags$div(class = "report-view-actions",
        tags$a(href = url, target = "_blank", class = "btn btn-default",
               icon("up-right-from-square"), " Open in new tab"),
        downloadButton("report_dl2", "Download HTML")
      ),
      tags$iframe(src = url, class = "report-frame")
    )
  })

  report_download <- downloadHandler(
    filename = function() "metacheck_repository_report.html",
    content  = function(file) {
      bytes <- report_html()
      if (!is.null(bytes)) writeBin(bytes, file)
    }
  )
  output$report_dl  <- report_download
  output$report_dl2 <- report_download
}

shinyApp(ui, server)
