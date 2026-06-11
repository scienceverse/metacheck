## report_app.R — Create Report only: upload a PDF, get a report ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  # library(DT)
  # library(metacheck)
  # library(dplyr)
})

source("R/constants.R")
source("R/funcs.R")
source("tabs/report.R")
source("tabs/options.R")


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
  options(shiny.maxRequestSize = 100 * 1024^2)

  ## reactiveVals ----
  report_path     <- reactiveVal("")
  report_running  <- reactiveVal(FALSE)
  report_error    <- reactiveVal("")
  report_filename <- reactiveVal("")

  validated_modules <- c(
    "power", "marginal", "prereg_check",
    "ref_pubpeer", "ref_retraction", "ref_replication",
    "stat_check", "stat_p_exact", "stat_p_nonsig", "repo_check", "code_check"
  )

  ### options_done — return to the Report tab ----
  observeEvent(input$options_done, {
    updateTabItems(session, "tabs", "report_tab")
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
      "\U0001F512 We will not retrieve information from online data repositories about linked URLs (code_check and repo_check modules will only run on local files)."
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
        local_path <- trimws(input$local_path)
        has_local  <- nzchar(local_path) &&
                        (file.exists(local_path) || dir.exists(local_path))
        modules <- validated_modules
        if (!isTRUE(input$query_pubpeer)) modules <- setdiff(modules, "ref_pubpeer")
        if (!isTRUE(input$query_repos) && !has_local)
          modules <- setdiff(modules, c("repo_check", "code_check"))
        skip_online <- has_local && !isTRUE(input$query_repos)
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

    # make module list
    mods_used    <- validated_modules
    if (!use_pubpeer) mods_used <- setdiff(mods_used, "ref_pubpeer")
    if (!use_repos && !has_local_snip)
      mods_used <- setdiff(mods_used, c("repo_check", "code_check"))
    mods_str     <- paste0('"', mods_used, '"', collapse = ', ') |> sprintf("c(%s)", x= _)

    # convert args
    crossref_arg <- if (use_crossref) ", crossref_lookup = TRUE" else ""
    grobid_server_arg <- switch(grobid_server,
                                "metacheck" = ',\n        api_url = "https://grobid.hti.ieis.tue.nl"',
                                "local"     = ',\n        api_url = "http://localhost:8070"',
                                "")
    args_code    <- if (has_local_snip) {
      lo_arg <- if (!use_repos) ", local_only = TRUE" else ""
      paste0(',\n  args = list(\n',
             '    repo_check = list(local_path = "', local_path_snip, '"', lo_arg, '),\n',
             '    code_check = list(local_path = "', local_path_snip, '"', lo_arg, '))')
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


} # end server()

shinyApp(ui, server)
