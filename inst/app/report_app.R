## report_app.R — Create Report only: upload a PDF, get a report ##
suppressPackageStartupMessages({
  # Tab files (tabs/report.R, tabs/options.R) call metacheck's own exported
  # functions unqualified (e.g. llm_model_list()). shiny::runApp() sources
  # them into the global env, which only has metacheck on the search path if
  # the caller already ran library(metacheck) themselves -- report_app()
  # itself does not, so metacheck::report_app() failed with "could not find
  # function" for anyone who hadn't separately attached the package first.
  # See https://github.com/scienceverse/metacheck/issues/320.
  library(metacheck)
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
})

options(shiny.maxRequestSize = 100 * 1024^2,
        scipen = 10,
        digits = 4)

debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) {
    message(...)
    #} else {
    list(...) |>
      toString() |>
      shinyjs::logjs()
  }
}

source("tabs/report.R")
source("tabs/options.R")


## usage statistics ----
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
# This is the same record the OSF download app keeps, written to its own file
# in the same folder, so the two can be reported together.
# Where the file goes. On a Shiny Server the app runs as the `shiny` user,
# whose home directory may not be writable (or set at all), and
# rappdirs::user_data_dir() resolves to ~/.local/share/metacheck there. So the
# location can be set explicitly with the METACHECK_USAGE_DIR environment
# variable in the app's Renviron, and the default is only used when it can
# actually be written to. Returns "" when there is nowhere to write, and
# usage_record() then does nothing.
usage_file <- function(filename = "report_app_usage.csv") {
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


## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Create Report", tabName = "report_tab",
               icon = icon("file-pdf"), selected = TRUE),
      menuItem("Options", tabName = "options_tab",
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
      report_tab,
      options_tab
    )
  )
)


## server ----
server <- function(input, output, session) {
  # One row per visit. See the note above: this counts visits, not people.
  usage_record("session")

  ## reactiveVals ----
  report_path     <- reactiveVal("")
  report_running  <- reactiveVal(FALSE)
  report_error    <- reactiveVal("")
  report_filename <- reactiveVal("")

  validated_modules <- c(
    "power", "marginal", "prereg_check", "ethics_check",
    "ref_pubpeer", "ref_retraction", "ref_replication", "ref_accuracy",
    "stat_check", "stat_p_exact", "stat_p_nonsig", "stat_effect_size",
    "repo_check", "code_check", "data_check", "codebook_check"
  )

  # Modules that read the files inside a repository. They all take the same
  # local_path / local_only arguments, and none of them can run when neither
  # an online repository nor a local folder is available.
  repo_modules <- c("repo_check", "code_check", "data_check", "codebook_check")

  ### options_done — return to the Report tab ----
  observeEvent(input$options_done, {
    updateTabItems(session, "tabs", "report_tab")
  })

  ### options_update — go to the Options tab ----
  observeEvent(input$options_update, {
    updateTabItems(session, "tabs", "options_tab")
  })

  ### gdpr_privacy_ui ----
  # Landing-page privacy message, driven by the current Options settings.
  output$gdpr_privacy_ui <- renderUI({
    use_crossref <- isTRUE(input$query_crossref)
    use_pubpeer  <- isTRUE(input$query_pubpeer)
    use_repos    <- isTRUE(input$query_repos)
    use_llm      <- isTRUE(input$llm_model_choice != "none")
    use_grobid   <- input$grobid_server_choice %||% "metacheck"


    ## Use transparency text ----
    ### Grobid server use text ----
    # PDF conversion (GROBID) line — depends on the chosen conversion server
    grobid_use_text <- switch(use_grobid,
      "local"       = "\U0001F512 The PDF file is converted locally. No data is sent to external servers.",
      "metacheck"   = "\U0001F6E1\uFE0F The PDF file is converted using a GDPR compliant server at Eindhoven University of Technology.",
      "huggingface" = "\U0001F310 The PDF file is converted using an external server.",
      "The PDF file is converted using an external server."
    )

    ### DOI use text ----
    if (use_crossref || use_pubpeer) {
      apis <- "CrossRef and PubPeer"
      if (!use_crossref) apis <- "PubPeer, but not CrossRef,"
      if (!use_pubpeer) apis <- "CrossRef, but not PubPeer,"
      doi_use_text <- sprintf("\U0001F310 DOIs are sent to %s to retrieve information about references.", apis)
    } else {
      doi_use_text <- "\U0001F512 DOIs are not sent to CrossRef or PubPeer to retrieve information about references."
    }

    ### Repo use text ----
    repo_use_text <- ifelse(
      use_repos,
      "\U0001F310 APIs are used to retrieve information from online data repositories about linked URLs.",
      "\U0001F512 We will not retrieve information from online data repositories about linked URLs (the repo_check, code_check, data_check, and codebook_check modules will only run on local files)."
    )

    ### LLM use text ----
    llm_use_text <- ifelse(
      !use_llm, "\U0001F512 The use of an LLM is not enabled in settings.",
      sprintf("%s The %s LLM model %s is enabled.",
              ifelse(grepl("^ollama", input$llm_model_choice), "\U0001F6E1\uFE0F", "\U0001F310"),
              ifelse(grepl("^ollama", input$llm_model_choice), "local", "external"),
              input$llm_model_choice)
    )

    # add lines
    tags$ul(
      tags$li(grobid_use_text),
      tags$li(doi_use_text),
      tags$li(repo_use_text),
      tags$li(llm_use_text)
    )
  })

  ### upload_pdf ----
  observeEvent(input$upload_pdf, {
    req(input$upload_pdf)

    report_path("")
    report_error("")
    report_running(TRUE)
    report_filename("")

    files <- input$upload_pdf
    report_filename(files$name)
    qmdpath  <- tempfile(fileext = ".qmd")
    htmlpath <- sub("qmd$", "html", qmdpath)
    tmp <- tempfile()
    dir.create(tmp, showWarnings = FALSE)

    tryCatch({
      use_crossref <- isTRUE(input$query_crossref)

      withProgress(message = "Generating report", value = 0, {

        incProgress(0.15, detail = "Converting PDF...")
        grobid_args <- switch(input$grobid_server_choice,
          "huggingface" = list(method = "grobid",
                             api_url = "https://grobidOrg-grobid.hf.space"),
          "metacheck" = list(method = "grobid",
                             api_url = "https://grobid.hti.ieis.tue.nl"),
          "local"     = list(method = "grobid",
                             api_url = "http://localhost:8070"),
          list()
        )
        # Convert without CrossRef here so the (often slow) CrossRef lookup
        # gets its own visible progress step below, rather than hiding inside
        # the "Converting PDF" step.
        json <- do.call(convert,
                        c(list(files$datapath, tmp,
                               crossref_lookup = FALSE),
                          grobid_args))

        incProgress(0.15, detail = "Reading paper...")
        paper <- read(json)

        if (use_crossref) {
          incProgress(0.2, detail = "Querying CrossRef (this can take a while)...")
          paper <- add_bib_match(paper)
        }

        incProgress(if (use_crossref) 0.2 else 0.4, detail = "Running modules...")
        local_path <- trimws(input$local_path)
        has_local  <- nzchar(local_path) &&
                        (file.exists(local_path) || dir.exists(local_path))
        modules <- validated_modules
        if (!isTRUE(input$query_pubpeer)) modules <- setdiff(modules, "ref_pubpeer")
        # ref_accuracy needs the CrossRef bib_match table to compare against
        if (!use_crossref) modules <- setdiff(modules, "ref_accuracy")
        if (!isTRUE(input$query_repos) && !has_local)
          modules <- setdiff(modules, repo_modules)
        skip_online <- has_local && !isTRUE(input$query_repos)
        module_args <- if (has_local) {
          stats::setNames(
            lapply(repo_modules, function(m) {
              list(local_path = local_path, local_only = skip_online)
            }),
            repo_modules)
        } else list()
        report(paper,
               modules       = modules,
               output_file   = qmdpath,
               output_format = "qmd",
               args          = module_args)

        incProgress(0.2, detail = "Rendering HTML...")
        quarto::quarto_render(input = qmdpath,
                              quiet = TRUE,
                              output_format = "html",
                              metadata = list(html = list(theme = NULL)))

        incProgress(0.2, detail = "Done!")

        # Anonymous usage statistics: one finished report, and how many check
        # modules produced it. Recorded here, after the render succeeded, so a
        # failed attempt is not counted as a report. Nothing about the paper
        # itself is recorded.
        usage_record("report", 1L, length(modules))
      })

      report_path(htmlpath)
      addResourcePath("tmp_cr", dirname(htmlpath))
      session$sendCustomMessage(type = "openTab",
                                message = paste0("tmp_cr/", basename(htmlpath)))

    }, error = function(e) {
      report_error(e$message)
    }, finally = {
      report_running(FALSE)
      unlink(tmp, recursive = TRUE)
    })
  }, ignoreNULL = TRUE)

  ### r_code
  output$r_code <- renderUI({
    # triggers
    use_crossref <- isTRUE(input$query_crossref)
    use_pubpeer  <- isTRUE(input$query_pubpeer)
    use_repos    <- isTRUE(input$query_repos)
    use_llm      <- !input$llm_model_choice %in% "none"
    local_path_snip <- trimws(input$local_path)
    grobid_server <- input$grobid_server_choice

    # set up code
    fname        <- report_filename()
    if (!nzchar(fname)) fname <- "myfile.pdf"
    json_name    <- sub("\\.[Pp][Dd][Ff]$", ".json", fname)
    has_local_snip  <- nzchar(local_path_snip) &&
      (file.exists(local_path_snip) || dir.exists(local_path_snip))
    # A Windows path pasted in as C:\Users\... would put \U into the string
    # literal below, which R cannot parse. R accepts forward slashes on
    # Windows, so show the path that way in the copyable code.
    local_path_snip <- gsub("\\", "/", local_path_snip, fixed = TRUE)

    # make module list
    mods_used    <- validated_modules
    if (!use_pubpeer) mods_used <- setdiff(mods_used, "ref_pubpeer")
    if (!use_crossref) mods_used <- setdiff(mods_used, "ref_accuracy")
    if (!use_repos && !has_local_snip)
      mods_used <- setdiff(mods_used, repo_modules)
    mods_str     <- paste0('"', mods_used, '"', collapse = ', ') |> sprintf("c(%s)", x= _)

    # convert args
    crossref_arg <- if (use_crossref) ", crossref_lookup = TRUE" else ""
    grobid_server_arg <- switch(grobid_server,
                                "metacheck" = ',\n        api_url = "https://grobid.hti.ieis.tue.nl"',
                                "local"     = ',\n        api_url = "http://localhost:8070"',
                                "")
    args_code    <- if (has_local_snip) {
      lo_arg <- if (!use_repos) ", local_only = TRUE" else ""
      arg_lines <- paste0('    ', repo_modules,
                          ' = list(local_path = "', local_path_snip, '"', lo_arg, ')',
                          collapse = ',\n')
      paste0(',\n  args = list(\n', arg_lines, ')')
    } else { "" }

    llm_code     <- ifelse(use_llm,
                           sprintf('llm_use(TRUE)\nllm_model(%s)\n\n', llm_model()),
                           "llm_use(FALSE)\n\n")

    code <- paste0(
      'library(metacheck)\n\n',
      llm_code,
      '# convert PDF to JSON and read in\n',
      sprintf('convert("%s", method = "grobid"%s%s)\n',
              fname, crossref_arg, grobid_server_arg),
      'paper <- read("', json_name, '")\n\n',
      '# choose modules and run the report\n',
      'modules <- ', mods_str, '\n',
      'report(paper, modules', args_code, ')'
    )

    tags$pre(tags$code(code, .noWS="outside"), .noWS="outside")
  })

  ### report_status_ui ----
  output$report_status_ui <- renderUI({
    running <- report_running()
    path <- report_path()
    err     <- report_error()

    if (running) {
      shiny::p(icon("spinner", class = "fa-spin"), " Generating report, please wait...")
    } else if (nzchar(err)) {
      shiny::p(style = "color: #c0392b;", icon("circle-xmark"), " Error: ", err)
    } else if (nzchar(path) && file.exists(path)) {
      tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
                 " Report generated successfully and opened in a new tab."),
        actionButton("report_view", "View Report Again",
                     icon = icon("eye")),
        downloadButton("report_dl", "Download HTML")
      )
    } else {
      NULL
    }
  })

  ### report_view ----
  observeEvent(input$report_view, {
    path <- report_path()
    if (!file.exists(path)) return(NULL)
    session$sendCustomMessage(type = "openTab",
                              message = paste0("tmp_cr/", basename(path)))
  })

  ### report_dl ----
  output$report_dl <- downloadHandler(
    filename = function() "metacheck_report.html",
    content  = function(file) {
      path <- report_path()
      if (file.exists(path)) file.copy(path, file)
    }
  )

  ### llm_model_chooser ----
  observeEvent(input$llm_model_choice, {
    model_id <- input$llm_model_choice
    if (model_id == "none") {
      llm_use(FALSE)
    } else {
      llm_use(TRUE)
      llm_model(model_id)
    }
  })

  observeEvent(input$grobid_server_choice, {
    if (input$grobid_server_choice == "local") {
      up <- .grobid_isalive("http://localhost:8070", error = FALSE)
      if (!up)  {
        showNotification(
          "The local Grobid server does not seem to be running. Check http://localhost:8070",
          type = "warning", duration = NULL
        )
      }
    }
  })


} # end server()

shinyApp(ui, server)
