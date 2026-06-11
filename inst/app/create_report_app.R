## create_report_app.R — Create Report only: upload a PDF, get a report ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(DT)
  library(metacheck)
  library(dplyr)
})

source("R/constants.R")
source("R/funcs.R")
source("tabs/create_report.R")
source("tabs/options.R")


## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Create Report", tabName = "create_report_tab",
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
      create_report_tab,
      options_tab
    )
  )
)


## server ----
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)

  ## reactiveVals ----
  create_report_path     <- reactiveVal("")
  create_report_running  <- reactiveVal(FALSE)
  create_report_error    <- reactiveVal("")
  create_report_filename <- reactiveVal("")

  validated_modules <- c(
    "power", "marginal", "prereg_check",
    "ref_pubpeer", "ref_retraction", "ref_replication",
    "stat_check", "stat_p_exact", "stat_p_nonsig", "repo_check", "code_check"
  )

  ### options_done — return to the Create Report tab ----
  observeEvent(input$options_done, {
    updateTabItems(session, "tabs", "create_report_tab")
  })

  ### gdpr_privacy_ui ----
  # Landing-page privacy message, driven by the current Options settings.
  output$gdpr_privacy_ui <- renderUI({
    use_crossref <- isTRUE(input$create_crossref)
    use_pubpeer  <- isTRUE(input$create_pubpeer)
    use_repos    <- isTRUE(input$create_repos)
    use_llm      <- isTRUE(input$create_use_llm)
    grobid       <- input$grobid_server_choice %||% "metacheck"

    any_external <- use_crossref || use_pubpeer || use_repos

    lines <- list()

    # Lead with the call to action whenever anything is sent externally
    # (either a non-local PDF conversion server, or any of the query options).
    if (any_external || grobid != "local") {
      lines <- c(lines, list(
        tags$p(tags$em(
          "Change the settings in the 'Options' tab to disable modules that send information to external servers."
        ))
      ))
    }

    # PDF conversion (GROBID) line — depends on the chosen conversion server
    grobid_line <- switch(grobid,
      "local"       = "The PDF file is converted locally. No data is sent to external servers.",
      "metacheck"   = "The PDF file is converted using a GDPR compliant server at Eindhoven University of Technology.",
      "huggingface" = "The PDF file is converted using an external server.",
      "The PDF file is converted using an external server."
    )
    lines <- c(lines, list(tags$p(grobid_line)))

    if (!any_external) {
      lines <- c(lines, list(
        tags$p("No data is sent to or retrieved from external servers.")
      ))
    } else {
      if (use_crossref || use_pubpeer) {
        lines <- c(lines, list(
          tags$p("DOIs are sent to external servers to retrieve information about references.")
        ))
      }
      if (use_repos) {
        lines <- c(lines, list(
          tags$p("API's are used to retrieve information from online data repositories.")
        ))
      }
    }

    if (!use_llm) {
      lines <- c(lines, list(
        tags$p("The use of an Ollama large model is not enabled in settings.")
      ))
    }

    do.call(tagList, lines)
  })

  ### create_pdf ----
  observeEvent(input$create_pdf, {
    req(input$create_pdf)

    create_report_path("")
    create_report_error("")
    create_report_running(TRUE)
    create_report_filename("")

    files <- input$create_pdf
    create_report_filename(files$name)
    qmdpath  <- tempfile(fileext = ".qmd")
    htmlpath <- sub("qmd$", "html", qmdpath)
    tmp <- tempfile()
    dir.create(tmp, showWarnings = FALSE)

    tryCatch({
      use_crossref <- isTRUE(input$create_crossref)

      withProgress(message = "Generating report", value = 0, {

        incProgress(0.15, detail = "Converting PDF...")
        grobid_args <- switch(input$grobid_server_choice,
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
        local_path <- trimws(input$create_local_path)
        has_local  <- nzchar(local_path) &&
                        (file.exists(local_path) || dir.exists(local_path))
        modules <- validated_modules
        if (!isTRUE(input$create_pubpeer)) modules <- setdiff(modules, "ref_pubpeer")
        if (!isTRUE(input$create_repos) && !has_local)
          modules <- setdiff(modules, c("repo_check", "code_check"))
        skip_online <- has_local && !isTRUE(input$create_repos)
        module_args <- if (has_local) {
          list(repo_check = list(local_path = local_path, local_only = skip_online),
               code_check = list(local_path = local_path, local_only = skip_online))
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
      })

      create_report_path(htmlpath)
      addResourcePath("tmp_cr", dirname(htmlpath))
      session$sendCustomMessage(type = "openTab",
                                message = paste0("tmp_cr/", basename(htmlpath)))

    }, error = function(e) {
      create_report_error(e$message)
    }, finally = {
      create_report_running(FALSE)
      unlink(tmp, recursive = TRUE)
    })
  }, ignoreNULL = TRUE)

  ### create_report_status_ui ----
  output$create_report_status_ui <- renderUI({
    running <- create_report_running()
    path    <- create_report_path()
    err     <- create_report_error()

    if (running) {
      shiny::p(icon("spinner", class = "fa-spin"), " Generating report, please wait...")
    } else if (nzchar(err)) {
      shiny::p(style = "color: #c0392b;", icon("circle-xmark"), " Error: ", err)
    } else if (nzchar(path) && file.exists(path)) {
      fname        <- create_report_filename()
      json_name    <- sub("\\.[Pp][Dd][Ff]$", ".json", fname)
      use_crossref <- isTRUE(input$create_crossref)
      use_pubpeer  <- isTRUE(input$create_pubpeer)
      use_repos    <- isTRUE(input$create_repos)
      use_llm      <- isTRUE(input$create_use_llm)
      local_path_snip <- trimws(input$create_local_path)
      has_local_snip  <- nzchar(local_path_snip) &&
                           (file.exists(local_path_snip) || dir.exists(local_path_snip))
      mods_used    <- validated_modules
      if (!use_pubpeer) mods_used <- setdiff(mods_used, "ref_pubpeer")
      if (!use_repos && !has_local_snip)
        mods_used <- setdiff(mods_used, c("repo_check", "code_check"))
      crossref_arg <- if (use_crossref) ", crossref_lookup = TRUE" else ""
      mods_str     <- paste0('c(', paste0('"', mods_used, '"', collapse = ', '), ')')
      llm_code     <- if (use_llm) 'llm_use(TRUE)\n\n' else ""
      args_code    <- if (has_local_snip) {
        lo_arg <- if (!use_repos) ", local_only = TRUE" else ""
        paste0(',\n  args = list(\n',
               '    repo_check = list(local_path = "', local_path_snip, '"', lo_arg, '),\n',
               '    code_check = list(local_path = "', local_path_snip, '"', lo_arg, '))')
      } else ""
      code <- paste0(
        'library(metacheck)\n\n',
        llm_code,
        'convert("', fname, '"', crossref_arg, ')\n',
        'paper <- read("', json_name, '")\n\n',
        'report(paper,\n',
        '  modules = ', mods_str, args_code, ')'
      )
      tagList(
        shiny::p(style = "color: #27ae60;", icon("circle-check"),
          " Report generated successfully and opened in a new tab."),
        actionButton("create_report_view", "View Report Again",
                     icon = icon("eye")),
        downloadButton("create_report_dl", "Download HTML"),
        tags$br(), tags$br(),
        shiny::p(tags$strong("The following R code would create this report directly from R:")),
        tags$pre(tags$code(code))
      )
    } else {
      NULL
    }
  })

  ### create_report_view ----
  observeEvent(input$create_report_view, {
    path <- create_report_path()
    if (!file.exists(path)) return(NULL)
    session$sendCustomMessage(type = "openTab",
                              message = paste0("tmp_cr/", basename(path)))
  })

  ### create_report_dl ----
  output$create_report_dl <- downloadHandler(
    filename = function() "metacheck_report.html",
    content  = function(file) {
      path <- create_report_path()
      if (file.exists(path)) file.copy(path, file)
    }
  )

  ### create_use_llm ----
  observeEvent(input$create_use_llm, {
    use <- isTRUE(input$create_use_llm)
    llm_use(use)
    if (use) {
      available <- tryCatch(llm_model_list("ollama")$id, error = function(e) character(0))
      if (length(available) == 0) {
        shinyjs::alert(
          "No Ollama models found. Please install Ollama and pull at least one model before enabling LLM use.\nSee: https://www.scienceverse.org/metacheck/articles/ollama.html"
        )
        llm_use(FALSE)
        updateCheckboxInput(session, "create_use_llm", value = FALSE)
      } else {
        preferred <- c("qwen2.5:3b", "llama3.2:3b", "mistral:7b")
        model_id  <- preferred[preferred %in% available][1]
        if (is.na(model_id)) model_id <- available[[1]]
        llm_model(paste0("ollama/", model_id))
      }
    }
  })

} # end server()

shinyApp(ui, server)
