## app.R --------------------------------------------------------------------
## metacheck report app -- Shiny Server edition.
##
## A self-contained version of metacheck::report_app() designed to run on a
## Shiny Server that has R (and the metacheck + rmarkdown packages) but NOT:
##   * the Quarto CLI   -> reports are rendered with rmarkdown (see render_report.R)
##   * a local GROBID    -> PDFs are converted by a remote GROBID server
##   * Ollama            -> local LLMs are unavailable; optional LLM is via Groq
##
## Deploy by copying this folder to the Shiny Server app directory. The only
## file that has to be named app.R is this one; everything else is sourced or
## served from www/.
##
## See README.md for what this edition can and cannot do, and for the privacy
## implications of the optional Groq API-key field.

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(metacheck)
})

source("render_report.R")

# Load server-provided API keys (MISTRAL_API_KEY, GROQ_API_KEY) from a
# .Renviron in this app directory. Shiny Server does not reliably read a
# working-directory .Renviron at R startup, so we load it explicitly here.
# This file is kept on the server only and out of git (see .gitignore).
local({
  renv <- file.path(getwd(), ".Renviron")
  if (file.exists(renv)) readRenviron(renv)
})

MAX_UPLOAD_MB <- 50
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2,
        scipen = 10,
        digits = 4)

## --- usage statistics -------------------------------------------------------
#
# ANONYMOUS USAGE STATISTICS. These numbers are kept so that continued
# development of metacheck can be justified in future grant applications,
# which is what pays for the work. Please keep this in place.
#
# What is recorded, and nothing else: the date, whether a session was started
# or a report finished, how many reports were produced, and how many check
# modules were run to produce them.
#
# There is NO identifier of any kind: no name, no file name, no DOI, no
# content from the paper, no token, no address of the person using the app,
# and no way to link two rows to the same visitor. A row records that
# something happened, never who did it or what they checked. "Sessions"
# therefore counts visits rather than people, and overcounts anyone who comes
# back; it is reported that way rather than being dressed up as a user count.
# The file stays on the machine running the app and is never sent anywhere.
#
# Nothing is recorded when the app runs locally: only the hosted app counts,
# so development and testing do not inflate the figures.
#
# This is the same record report_app.R and osf_app.R keep, written to its own
# file in the same folder, so the three can be reported together.
# Where the file goes. On a Shiny Server the app runs as the `shiny` user,
# whose home directory may not be writable (or set at all), and
# rappdirs::user_data_dir() resolves to ~/.local/share/metacheck there. So the
# location can be set explicitly with the METACHECK_USAGE_DIR environment
# variable in the app's Renviron, and the default is only used when it can
# actually be written to. Returns "" when there is nowhere to write, and
# usage_record() then does nothing.
usage_file <- function(filename = "shiny_app_usage.csv") {
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
  # Only count real use of the hosted app. Running it locally is development
  # and testing, which would inflate the figures the grant application rests
  # on. SHINY_PORT is set by Shiny Server and empty everywhere else.
  if (Sys.getenv("SHINY_PORT") == "") return(invisible(NULL))
  # Recording usage must never interfere with using the app, so any failure
  # here (an unwritable folder, a locked file) is swallowed deliberately.
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

## --- Error message translation --------------------------------------------
## metacheck's low-level errors (GROBID HTTP status, connection failures, etc.)
## are cryptic for end users. Translate the common ones into plain, actionable
## guidance. (Note: the network steps have no enforceable timeout -- a blocking
## socket can't be interrupted from R -- so a hung remote service is bounded
## only by the 600s stale-slot reclaim, not a per-request timeout.)
explain_error <- function(msg) {
  msg <- paste(msg, collapse = " ")
  if (!nzchar(msg) || is.na(msg)) {
    return(paste0("The report could not be generated (the process was ",
      "interrupted or timed out). Please try again in a few minutes."))
  }
  m <- tolower(msg)
  if (grepl("reached.*time limit|timeout|timed out", m)) {
    return(paste0("The server took too long and stopped this report. This ",
      "usually means a very large PDF or a slow external service. Please try ",
      "again in a few minutes, or with a smaller PDF."))
  }
  if (grepl("entity too large|payload too large|413|maximum upload", m)) {
    return(sprintf(paste0("Your PDF is too large to process (the limit is %d MB). ",
      "Please upload a smaller PDF -- for example, remove large embedded images, ",
      "or upload only the manuscript text."), MAX_UPLOAD_MB))
  }
  if (grepl("connection to the grobid server failed|could not resolve|connection refused|connection (timed out|reset)", m)) {
    return(paste0("The PDF-conversion server could not be reached right now. ",
      "Please try again in a few minutes, or switch the conversion server in ",
      "the Options tab."))
  }
  if (grepl("internal server error|500|bad request|400|unprocessable", m)) {
    return(paste0("Your PDF could not be converted. It may be scanned (image-only, ",
      "with no selectable text), password-protected, corrupted, or not a real ",
      "PDF. Please upload a text-based PDF of the manuscript."))
  }
  if (grepl("no pdf.*detected|does not exist|no json or xml", m)) {
    return(paste0("No readable PDF was found in your upload. Please make sure ",
      "you selected a valid .pdf file and try again."))
  }
  if (grepl("api[_ ]?key|unauthor|401|403", m)) {
    return(paste0("The optional LLM power check could not authenticate with the ",
      "provider. Please disable the LLM power check in the Options tab, or try ",
      "the other provider."))
  }
  # default: show the original, but framed
  paste0("Something went wrong while generating your report: ", msg)
}

## --- Concurrency control ---------------------------------------------------
## Report generation is CPU-bound and blocks a single-threaded R process for its
## whole duration (GROBID + modules + Quarto). To avoid overload and the
## silent-disconnect experience under load, we cap how many reports run AT ONCE
## across the whole server, using an on-disk slot directory as a cross-process
## semaphore. (Open-source Shiny Server may run the app as several R processes,
## so an in-memory counter alone would not be global -- the filesystem is.)
MAX_CONCURRENT_REPORTS <- 4L                 # global cap across all sessions (= core count)
REPORT_COOLDOWN_SECONDS <- 20                # per-session wait between reports
SLOT_STALE_SECONDS      <- 600               # reclaim slots from crashed runs
# A FIXED shared path (not tempdir(), which is per-process) so the semaphore is
# global even if Shiny Server runs the app as several R processes. Use a stable
# system temp path; fall back to the app dir if that is not writable.
SLOT_DIR <- local({
  cand <- file.path("/tmp", "metacheck_report_slots")
  ok <- tryCatch(dir.create(cand, showWarnings = FALSE, recursive = TRUE) ||
                   dir.exists(cand), error = function(e) FALSE)
  if (!ok || file.access(cand, 2) != 0) {
    cand <- file.path(getwd(), ".report_slots")
    dir.create(cand, showWarnings = FALSE, recursive = TRUE)
  }
  cand
})

# Count currently-held slots, reclaiming any stale ones (from a process that
# died mid-render without releasing). Returns the live slot count.
report_slots_in_use <- function() {
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

# Try to atomically claim a slot. dir.create() is atomic, so this is race-safe
# even across processes. Returns the slot path on success, or "" if full.
report_slot_acquire <- function() {
  if (report_slots_in_use() >= MAX_CONCURRENT_REPORTS) return("")
  slot <- file.path(SLOT_DIR,
                    paste0(Sys.getpid(), "-", as.integer(Sys.time()), "-",
                           sample.int(1e6, 1)))
  ok <- dir.create(slot, showWarnings = FALSE)
  # Re-check after creating: if we just tipped over the cap due to a race,
  # release and report full. (dir.create can't overshoot by more than the
  # number of racing processes, which is small.)
  if (!ok) return("")
  if (report_slots_in_use() > MAX_CONCURRENT_REPORTS) {
    unlink(slot, recursive = TRUE); return("")
  }
  slot
}

report_slot_release <- function(slot) {
  if (nzchar(slot)) unlink(slot, recursive = TRUE)
}

# Modules that are validated AND can run on a server (no local-file modules by
# default; repo_check/code_check/data_check/codebook_check still run against
# online repositories, since local_path is never set here).
VALIDATED_MODULES <- c(
  "power", "marginal", "prereg_check", "ethics_check",
  "ref_pubpeer", "ref_retraction", "ref_replication", "ref_accuracy",
  "stat_check", "stat_p_exact", "stat_p_nonsig", "stat_effect_size",
  "repo_check", "code_check", "data_check", "codebook_check"
)

# Remote GROBID servers offered (no "local" option on a server).
GROBID_SERVERS <- list(
  metacheck   = "https://grobid.hti.ieis.tue.nl",
  huggingface = "https://grobidOrg-grobid.hf.space"
)

# LLM providers offered for the optional power check. Each entry defines the
# ellmer "provider/model" string, the env var holding the API key, the place to
# get a key, and a short region/privacy note. Mistral (France, EU) is the
# default so the LLM step does not route to the USA; Groq (USA) is offered too.
# Both are cheap (a fraction of a US cent per paper) and have free tiers.
LLM_PROVIDERS <- list(
  mistral = list(
    # mistral-medium: notably stronger extraction/reasoning than small, still
    # well under a US cent per paper. Returns clean fenced JSON for this task.
    label    = "Mistral (France, EU)",
    model    = "mistral/mistral-medium-latest",
    key_env  = "MISTRAL_API_KEY",
    keys_url = "https://console.mistral.ai/api-keys",
    region   = "EU (France)",
    privacy  = paste0(
      "Mistral AI is a French company and acts as a GDPR data processor for ",
      "API requests, governed by French law. Data is processed within the EU ",
      "or transferred only under EU-recognised safeguards (adequacy decisions ",
      "or Standard Contractual Clauses). This deployment has opted out of ",
      "model training, so your text is not used to train Mistral's models, ",
      "and is retained only transiently to process the request. This is the ",
      "most GDPR-friendly option."
    )
  ),
  groq = list(
    label    = "Groq (USA)",
    model    = "groq/openai/gpt-oss-20b",
    key_env  = "GROQ_API_KEY",
    keys_url = "https://console.groq.com/keys",
    region   = "USA",
    privacy  = paste0(
      "Groq is a US company; your text is processed in the United States ",
      "under Groq's Data Processing Addendum, with contractual safeguards ",
      "(Standard Contractual Clauses) for the international transfer. This ",
      "deployment has opted out of model training, so your text is not used ",
      "to train models. Choose this only if you are comfortable with US ",
      "processing of your manuscript text."
    )
  )
)
LLM_DEFAULT_PROVIDER <- "mistral"


## UI -----------------------------------------------------------------------
report_tab <- tabItem(
  tabName = "report_tab",
  h2("Generate a Metacheck Report"),
  tags$p(
    "Select a PDF file below. The report will be generated automatically ",
    "using all validated modules. When it is ready, a ",
    tags$b("View Report"), " button will appear — click it to open the report ",
    "in a new browser tab."
  ),
  tags$div(class = "capacity-note capacity-warn",
    icon("triangle-exclamation"),
    tags$b(" Busy server? Please try again in a few minutes. "),
    "This is a shared research server with limited capacity. Each report takes ",
    "up to a minute or two, and only a few can run at the same time. If the ",
    "server is at capacity when you upload, you'll see a message asking you to ",
    "try again shortly — just wait a few minutes and upload again. Your request ",
    "is never lost silently. Thank you for your patience."
  ),
  uiOutput("busy_banner"),
  tags$div(class = "pdf-upload",
    fileInput("upload_pdf", NULL, multiple = FALSE, accept = ".pdf",
              width = "100%",
              buttonLabel = tagList(icon("upload"), "Upload PDF"),
              placeholder = "No file selected")
  ),
  tags$p(class = "upload-hint",
    sprintf("Please upload a text-based PDF of a single manuscript (max %d MB). ",
            MAX_UPLOAD_MB),
    "Scanned (image-only) or password-protected PDFs cannot be processed."),
  box(title = "Privacy", collapsible = TRUE, width = 12,
    tags$p("Change the settings in the 'Options' tab to enable or disable ",
           "checks that use external servers."),
    uiOutput("gdpr_privacy_ui"),
    actionButton("options_update", "Update Options",
                 icon = icon("arrow-right"), class = "btn-options-done")
  ),
  uiOutput("report_status_ui")
)

report_view_tab <- tabItem(
  tabName = "report_view_tab",
  h2("Your Metacheck Report"),
  uiOutput("report_view_ui")
)

options_tab <- tabItem(
  tabName = "options_tab",
  h2("Options"),
  tags$p(
    "These settings control what metacheck sends to, or retrieves from, ",
    "external servers when generating a report. You can leave them at their ",
    "defaults, or adjust them here before uploading a PDF."
  ),
  box(title = NULL, collapsible = FALSE, width = 12,

    radioButtons(
      "grobid_server_choice", "PDF Conversion Server:",
      choiceNames = list(
        "Use GDPR compliant GROBID server at Eindhoven University of Technology",
        "Use Full GROBID via HuggingFace (USA)"
      ),
      choiceValues = list("metacheck", "huggingface"),
      selected = "metacheck"
    ),

    tags$div(class = "report-checks",
      checkboxInput("query_crossref", "Query CrossRef", value = TRUE),
      tags$span("Send full references to CrossRef API"),
      checkboxInput("query_pubpeer", "Query PubPeer", value = TRUE),
      tags$span("Send reference DOIs to PubPeer API"),
      checkboxInput("query_repos", "Query Data Repositories", value = TRUE),
      tags$span("Use API to query repositories such as GitHub, Zenodo, and the OSF")
    ),

    tags$hr(),

    ## --- Optional LLM power check ----------------------------------------
    tags$h4("Optional: LLM power check"),
    tags$p(
      "The power-analysis check can optionally use a large language model to ",
      "check whether power analyses report all the required information. The ",
      "LLM is provided for you — no API key needed; just pick a provider."
    ),
    checkboxInput("use_llm", tags$b("Use LLM power check"), value = FALSE),
    conditionalPanel(
      condition = "input.use_llm == true",
      tags$div(class = "llm-key-panel",
        radioButtons(
          "llm_provider", "LLM provider:",
          choiceNames  = unname(lapply(LLM_PROVIDERS, `[[`, "label")),
          choiceValues = unname(names(LLM_PROVIDERS)),
          selected     = LLM_DEFAULT_PROVIDER
        ),
        uiOutput("llm_cost_note")
      )
    ),

    tags$br(),
    actionButton("options_done", "Done — back to Create Report",
                 icon = icon("arrow-left"), class = "btn-options-done")
  )
)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Create Report", tabName = "report_tab",
               icon = icon("file-pdf"), selected = TRUE),
      menuItem("Report", tabName = "report_view_tab", icon = icon("eye")),
      menuItem("Options", tabName = "options_tab", icon = icon("sliders"))
    ),
    HTML("<img src='images/logo.png' alt='Logo' style='width: 85%; margin: 1em;' />")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(report_tab, report_view_tab, options_tab)
  )
)


## server -------------------------------------------------------------------
server <- function(input, output, session) {
  # One row per visit. See the note above: this counts visits, not people.
  usage_record("session")

  # The rendered report is held in memory (raw bytes), never left on disk and
  # never exposed as a static file. report_url is a session-scoped, unguessable
  # endpoint that streams those bytes; it is only valid for this session.
  report_html     <- reactiveVal(NULL)       # raw vector of the HTML, or NULL
  report_url      <- reactiveVal("")         # in-memory serving URL for this run
  report_running  <- reactiveVal(FALSE)
  report_error    <- reactiveVal("")
  report_filename <- reactiveVal("")
  last_finished   <- reactiveVal(0)          # time the last report finished (cooldown)

  observeEvent(input$options_done,   updateTabItems(session, "tabs", "report_tab"))
  observeEvent(input$options_update, updateTabItems(session, "tabs", "options_tab"))

  ### Live server-capacity banner --------------------------------------------
  # Polls the shared slot count so users see whether the server is at capacity
  # BEFORE they upload, rather than being surprised by a busy message.
  output$busy_banner <- renderUI({
    invalidateLater(5000, session)           # refresh every 5s
    inuse <- report_slots_in_use()
    if (inuse >= MAX_CONCURRENT_REPORTS) {
      tags$div(class = "busy-banner busy-full",
        icon("hourglass-half"),
        sprintf(" The server is at capacity (%d of %d reports running). ",
                inuse, MAX_CONCURRENT_REPORTS),
        "You can still upload, but you may be asked to wait a moment.")
    } else if (inuse > 0) {
      tags$div(class = "busy-banner busy-some",
        icon("spinner", class = "fa-spin"),
        sprintf(" %d of %d report slots in use. Capacity available.",
                inuse, MAX_CONCURRENT_REPORTS))
    } else NULL
  })

  # The currently selected LLM provider definition (defaults to the default).
  llm_provider <- reactive({
    p <- input$llm_provider %||% LLM_DEFAULT_PROVIDER
    LLM_PROVIDERS[[p]] %||% LLM_PROVIDERS[[LLM_DEFAULT_PROVIDER]]
  })

  # Is the LLM power check requested for this run? (just the toggle now -- the
  # API key is provided by the server, not the user.)
  llm_requested <- reactive(isTRUE(input$use_llm))

  ### LLM provider: cost + privacy/GDPR note --------------------------------
  output$llm_cost_note <- renderUI({
    prov <- llm_provider()
    tags$div(class = "llm-cost-note",
      tags$p(
        icon("circle-info"),
        " This uses the ", tags$code(sub("^[^/]+/", "", prov$model)),
        " model on ", tags$b(prov$label), ". The API key is provided by this ",
        "server, so there is no cost to you."
      ),
      tags$p(
        tags$b("Privacy & GDPR. "),
        "When enabled, only the paragraphs of your manuscript that mention ",
        "statistical power are sent to the provider's API to be analysed — the ",
        "rest of your paper is not sent. This app never stores your manuscript ",
        "text or the report; both exist only in memory for your session. ",
        prov$privacy
      )
    )
  })

  ### Privacy panel ----------------------------------------------------------
  output$gdpr_privacy_ui <- renderUI({
    use_crossref <- isTRUE(input$query_crossref)
    use_pubpeer  <- isTRUE(input$query_pubpeer)
    use_repos    <- isTRUE(input$query_repos)
    use_llm      <- llm_requested()
    grobid       <- input$grobid_server_choice %||% "metacheck"

    grobid_use_text <- switch(grobid,
      "metacheck"   = "\U0001F6E1️ The PDF file is converted using a GDPR compliant server at Eindhoven University of Technology.",
      "huggingface" = "\U0001F310 The PDF file is converted using an external server (HuggingFace, USA).",
      "The PDF file is converted using an external server."
    )

    if (use_crossref || use_pubpeer) {
      apis <- "CrossRef and PubPeer"
      if (!use_crossref) apis <- "PubPeer, but not CrossRef,"
      if (!use_pubpeer)  apis <- "CrossRef, but not PubPeer,"
      doi_use_text <- sprintf("\U0001F310 DOIs are sent to %s to retrieve information about references.", apis)
    } else {
      doi_use_text <- "\U0001F512 DOIs are not sent to CrossRef or PubPeer to retrieve information about references."
    }

    repo_use_text <- if (use_repos) {
      "\U0001F310 APIs are used to retrieve information from online data repositories about linked URLs."
    } else {
      "\U0001F512 We will not retrieve information from online data repositories about linked URLs."
    }

    llm_use_text <- if (use_llm) {
      prov <- llm_provider()
      lock <- if (identical(prov$region, "EU (France)")) "\U0001F6E1️" else "\U0001F310"
      detail <- if (identical(prov$region, "EU (France)")) {
        "Mistral acts as a GDPR data processor (French law; EU adequacy/SCC safeguards)."
      } else {
        "Processed in the USA under Groq's Data Processing Addendum (SCC safeguards)."
      }
      sprintf("%s An LLM is enabled via %s: model %s. Only the power-related paragraphs are sent. %s This deployment has opted out of model training, so your text is not used to train models.",
              lock, prov$label, sub("^[^/]+/", "", prov$model), detail)
    } else {
      "\U0001F512 No LLM is used (LLM power check is off)."
    }

    tags$ul(
      tags$li(grobid_use_text),
      tags$li(doi_use_text),
      tags$li(repo_use_text),
      tags$li(llm_use_text)
    )
  })

  ### Generate report on upload ---------------------------------------------
  observeEvent(input$upload_pdf, {
    req(input$upload_pdf)

    # Gate 1: per-session guards. One report at a time per browser, and a short
    # cooldown after the previous one so a single user can't hammer the server.
    if (isTRUE(report_running())) {
      showNotification("A report is already being generated in this session. ",
                       "Please wait for it to finish.", type = "warning")
      return(invisible())
    }
    wait <- REPORT_COOLDOWN_SECONDS - (as.numeric(Sys.time()) - last_finished())
    if (wait > 0) {
      showNotification(sprintf("Please wait %d more second%s before generating another report.",
                               ceiling(wait), if (ceiling(wait) == 1) "" else "s"),
                       type = "warning")
      return(invisible())
    }

    # Gate 2: global capacity. Atomically claim one of the limited slots shared
    # across all sessions/processes. If the server is at capacity, tell the user
    # clearly instead of silently queuing (which leads to timeouts/disconnects).
    slot <- report_slot_acquire()
    if (!nzchar(slot)) {
      report_error(sprintf(
        "The server is busy generating other reports right now (limit %d at a time). Please try again in a minute.",
        MAX_CONCURRENT_REPORTS))
      report_running(FALSE)
      return(invisible())
    }

    report_html(NULL); report_url("")
    report_error(""); report_running(TRUE); report_filename("")

    files <- input$upload_pdf
    report_filename(files$name)
    htmlpath <- tempfile(fileext = ".html")
    tmp <- tempfile(); dir.create(tmp, showWarnings = FALSE)

    # Decide LLM use for this run. The provider's API key is supplied by the
    # server (loaded from .Renviron into the process env), not by the user.
    prov     <- llm_provider()
    use_llm  <- isTRUE(input$use_llm)

    tryCatch({
      use_crossref <- isTRUE(input$query_crossref)

      if (use_llm) {
        if (!nzchar(Sys.getenv(prov$key_env))) {
          stop(sprintf("The LLM power check is enabled for %s, but this server ",
                       prov$label),
               sprintf("has no %s configured. Choose another provider or ",
                       prov$key_env),
               "disable the LLM power check.", call. = FALSE)
        }
        metacheck::llm_use(TRUE)
        metacheck::llm_model(prov$model)
      } else {
        metacheck::llm_use(FALSE)
      }

      withProgress(message = "Generating report", value = 0, {
        incProgress(0.15, detail = "Converting PDF...")
        grobid_args <- list(method = "grobid",
                            api_url = GROBID_SERVERS[[input$grobid_server_choice]])
        json <- do.call(metacheck::convert,
                        c(list(files$datapath, tmp, crossref_lookup = FALSE),
                          grobid_args))

        # convert() may "succeed" but return NA / no usable file for a corrupt,
        # scanned, or empty PDF. Catch that before we try to read it.
        if (length(json) == 0 || all(is.na(json)) ||
            !any(file.exists(stats::na.omit(as.character(json))))) {
          stop("Your PDF could not be converted. It may be scanned (image-only), ",
               "password-protected, corrupted, or not a real PDF. Please upload a ",
               "text-based PDF of the manuscript.", call. = FALSE)
        }

        incProgress(0.15, detail = "Reading paper...")
        paper <- metacheck::read(json)

        # An empty/near-empty conversion reads OK but yields no usable text, so
        # the report would be blank. The extracted body lives in paper$text
        # (a data frame of text segments); warn rather than show an empty report.
        txt <- tryCatch(paper$text, error = function(e) NULL)
        n_text <- if (is.data.frame(txt)) nrow(txt) else length(txt)
        if (is.null(n_text) || n_text < 1) {
          stop("No readable text could be extracted from your PDF. It is likely ",
               "scanned (image-only) or has no selectable text layer. Please ",
               "upload a text-based PDF.", call. = FALSE)
        }

        if (use_crossref) {
          incProgress(0.2, detail = "Querying CrossRef (this can take a while)...")
          # Reference matching is non-essential: a CrossRef hiccup must NOT kill
          # an otherwise-good report. Degrade gracefully.
          paper <- tryCatch(
            metacheck::add_bib_match(paper),
            error = function(e) {
              showNotification(
                "Reference lookup (CrossRef) was skipped (slow or unavailable); the rest of the report was still generated.",
                type = "warning", duration = 8)
              paper
            })
        }

        incProgress(if (use_crossref) 0.2 else 0.4, detail = "Running modules...")
        modules <- VALIDATED_MODULES
        if (!isTRUE(input$query_pubpeer)) modules <- setdiff(modules, "ref_pubpeer")
        # ref_accuracy needs the CrossRef bib_match table to compare against
        if (!use_crossref) modules <- setdiff(modules, "ref_accuracy")
        if (!isTRUE(input$query_repos))
          modules <- setdiff(modules, c("repo_check", "code_check",
                                        "data_check", "codebook_check"))

        incProgress(0.15, detail = "Rendering HTML (Quarto)...")
        render_report_no_quarto(paper, modules, htmlpath)

        incProgress(0.1, detail = "Securing report...")
        # Read the self-contained HTML into memory, then delete it from disk
        # immediately so no report file lingers and nothing is served as a
        # static file. (Quarto must write a file to render; we don't keep it.)
        bytes <- readBin(htmlpath, "raw", n = file.info(htmlpath)$size)
        report_html(bytes)
        unlink(htmlpath)

        incProgress(0.05, detail = "Done!")

        # Anonymous usage statistics: one finished report, and how many check
        # modules produced it. Recorded here, after the render succeeded, so a
        # failed attempt is not counted as a report. Nothing about the paper
        # itself is recorded.
        usage_record("report", 1L, length(modules))
      })

      # Register a session-scoped, in-memory endpoint that streams the bytes.
      # The URL is unguessable and only valid for this Shiny session; nothing
      # touches disk. We re-register on each run to get a fresh URL.
      url <- session$registerDataObj(
        name = "metacheck_report",
        data = report_html(),
        filter = function(data, req) {
          shiny::httpResponse(
            200, "text/html; charset=utf-8", data
          )
        }
      )
      report_url(url)
      # Show the report automatically, the way the local desktop app does -- but
      # WITHOUT window.open(). A new browser tab opened after an async render
      # (not a direct click) is killed by popup blockers. Instead we switch to
      # an in-app "Report" tab that holds an iframe pointing at the in-memory
      # endpoint; that needs no popup and no user gesture, so it "just appears".
      updateTabItems(session, "tabs", "report_view_tab")
    }, error = function(e) {
      report_error(explain_error(conditionMessage(e)))
    }, finally = {
      # Release the global capacity slot and start this session's cooldown.
      report_slot_release(slot)
      last_finished(as.numeric(Sys.time()))
      # Keys are server-provided via .Renviron (persist for the process); we
      # only reset the LLM toggle state for metacheck here.
      metacheck::llm_use(FALSE)
      report_running(FALSE)
      # Delete the uploaded PDF and ALL render intermediates (qmd, figures,
      # any stray html) right now -- not just at session end.
      unlink(tmp, recursive = TRUE)
      if (!is.null(files$datapath)) unlink(files$datapath)
      if (file.exists(htmlpath)) unlink(htmlpath)
    })
  }, ignoreNULL = TRUE)

  ### Report status ----------------------------------------------------------
  output$report_status_ui <- renderUI({
    running <- report_running(); err <- report_error()
    have    <- !is.null(report_html()) && nzchar(report_url())
    if (running) {
      shiny::p(icon("spinner", class = "fa-spin"),
               " Generating report, please wait...")
    } else if (nzchar(err)) {
      shiny::p(style = "color: #c0392b;", icon("circle-xmark"), " Error: ", err)
    } else if (have) {
      tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
                 " Report generated successfully. It has opened in the ",
                 tags$b("Report"), " tab. Use ", tags$b("View Report"),
                 " to return to it, or ", tags$b("Open in new tab"),
                 " for a full-screen view."),
        actionButton("report_view", "View Report", icon = icon("eye")),
        uiOutput("report_open_tab_ui", inline = TRUE),
        downloadButton("report_dl", "Download HTML"),
        tags$div(class = "report-save-note",
          icon("triangle-exclamation"),
          tags$b(" Save your report to keep it. "),
          "To protect your privacy, this report is ",
          tags$b("not stored on the server"), " — it only lives in this browser ",
          "session's memory (RAM) and is gone for good when you close or reload ",
          "this tab, or after a period of inactivity. This is deliberate: it ",
          "means your paper and its report are never written to disk where ",
          "others could access them. Use ", tags$b("Download HTML"),
          " to save a permanent copy now. The viewed tab is a temporary, ",
          "session-only view; opening it later will not work."
        )
      )
    } else NULL
  })

  # "View Report" simply returns the user to the in-app Report tab (no popup).
  observeEvent(input$report_view, {
    if (!nzchar(report_url())) return(NULL)
    updateTabItems(session, "tabs", "report_view_tab")
  })

  ### In-app Report tab ------------------------------------------------------
  # The report is shown automatically here in a full-width/tall iframe pointing
  # at the session-scoped in-memory endpoint. No window.open(), so nothing is
  # blocked by popup blockers; nothing extra touches disk.
  output$report_view_ui <- renderUI({
    url <- report_url()
    if (is.null(report_html()) || !nzchar(url)) {
      return(shiny::p(class = "report-empty",
        icon("circle-info"),
        " No report yet. Upload a PDF on the ", tags$b("Create Report"),
        " tab; your report will appear here automatically when it's ready."))
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

  # Real <a> link (a direct user-gesture navigation), offered as a full-screen
  # fallback. Unlike a server-round-trip window.open(), this is not popup-blocked.
  output$report_open_tab_ui <- renderUI({
    url <- report_url()
    if (!nzchar(url)) return(NULL)
    tags$a(href = url, target = "_blank", class = "btn btn-default",
           icon("up-right-from-square"), " Open in new tab")
  })

  report_download <- downloadHandler(
    filename = function() "metacheck_report.html",
    content  = function(file) {
      bytes <- report_html()
      if (!is.null(bytes)) writeBin(bytes, file)
    }
  )
  output$report_dl  <- report_download   # on Create Report tab
  output$report_dl2 <- report_download   # on Report tab

  # Reset the LLM flag if the session ends mid-run. (Keys are server-provided
  # via .Renviron and intentionally persist for the process.)
  session$onSessionEnded(function() {
    metacheck::llm_use(FALSE)
  })
}

shinyApp(ui, server)
