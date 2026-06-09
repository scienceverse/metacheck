## app.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(metacheck)
  library(dplyr)
})

source("R/constants.R")
source("R/funcs.R")


## Interface Tab Items ----
source("tabs/create_report.R")
source("tabs/run_demo.R")
source("tabs/advanced.R")
source("tabs/llm.R")   # kept for LLM server references


## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Create Report", tabName = "create_report_tab",
               icon = icon("file-pdf"), selected = TRUE),
      menuItem("Run Demo",      tabName = "run_demo_tab",
               icon = icon("play")),
      menuItem("Advanced Mode", tabName = "advanced_tab",
               icon = icon("sliders"))
    ),
    actionButton("return_paper", "Quit & Return"),
    uiOutput("files_loaded"),
    tags$br(),
    # selectInput("lang", "Change language",
    #             choices = c(English = "en",
    #                         Dutch   = "nl",
    #                         Spanish = "es",
    #                         Chinese = "zh"),
    #             selected = "en"),
    # p("Most of the phrases have not been translated; this is just a proof of concept.",
    #   style = "margin: 0 1em;"),
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
      run_demo_tab,
      advanced_tab,
      llm_tab
    )
  )
)


## server ----
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

  updateNumericInput(session, "llm_max_calls", value = metacheck::llm_max_calls())

  if (Sys.getenv("GROQ_API_KEY") != "") hide("llm_api")

  ## reactiveVals ----
  debug_msg("----reactiveVals----")

  my_paper            <- reactiveVal( list() )
  text_table          <- reactiveVal( data.frame() )
  llm_table           <- reactiveVal( data.frame() )
  mod_table           <- reactiveVal( data.frame() )
  mod_summary         <- reactiveVal( "" )
  mod_title           <- reactiveVal( "" )
  mod_desc            <- reactiveVal( "" )
  mod_details         <- reactiveVal( "" )
  report_path         <- reactiveVal( "" )
  total_cost          <- reactiveVal(0)
  create_report_path     <- reactiveVal( "" )
  create_report_running  <- reactiveVal( FALSE )
  create_report_error    <- reactiveVal( "" )
  create_report_filename <- reactiveVal( "" )

  ### return_paper ----
  observeEvent(input$return_paper, {
    debug_msg("return_paper")
    s <- my_paper()
    if (length(s) == 1) s <- s[[1]]
    stopApp(s)
  })

  observe({
    paper <- my_paper()
    if (length(paper) > 0) {
      text_table(text_search(paper))
      choices <- names(paper)
    } else {
      choices <- c()
    }
    updateSelectInput(session, "paper_name", choices = choices)
  })

  ### on text_table() change ----
  observe({
    needs_text_table <- c("download_table", "llm_submit", "text_search",
                          "run_statcheck", "check_p_values")
    if (nrow(text_table()) == 0) {
      lapply(needs_text_table, shinyjs::disable)
    } else {
      lapply(needs_text_table, shinyjs::enable)
    }
  })

  ### on llm_table() change ----
  observe({
    needs_llm_table <- "download_llm"
    if (nrow(llm_table()) == 0) {
      lapply(needs_llm_table, shinyjs::disable)
    } else {
      lapply(needs_llm_table, shinyjs::enable)
    }
  })

  ## create report ----
  debug_msg("---- create report ----")

  validated_modules <- c(
    "power", "marginal", "prereg_check",
    "ref_pubpeer", "ref_retraction", "ref_replication",
    "stat_check", "stat_p_exact", "stat_p_nonsig", "repo_check", "code_check"
  )

  ### create_pdf ----
  observeEvent(input$create_pdf, {
    req(input$create_pdf)
    debug_msg("create_pdf")

    create_report_path("")
    create_report_error("")
    create_report_running(TRUE)
    create_report_filename("")

    files <- input$create_pdf
    create_report_filename(files$name)
    qmdpath <- tempfile(fileext = ".qmd")
    htmlpath <- sub("qmd$", "html", qmdpath)
    tmp <- tempfile()
    dir.create(tmp, showWarnings = FALSE)

    tryCatch({
      withProgress(message = "Generating report", value = 0, {

        incProgress(0.2, detail = "Converting PDF...")
        grobid_args <- switch(input$grobid_server_choice,
          "metacheck" = list(method = "grobid",
                             api_url = "https://grobid.hti.ieis.tue.nl"),
          "local"     = list(method = "grobid",
                             api_url = "http://localhost:8070"),
          list()  # "huggingface" — auto-discovery via priority list
        )
        json <- do.call(convert,
                        c(list(files$datapath, tmp,
                               crossref_lookup = isTRUE(input$create_crossref)),
                          grobid_args))

        incProgress(0.2, detail = "Reading paper...")
        paper <- read(json)

        incProgress(0.2, detail = "Running modules...")
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
               modules     = modules,
               output_file = qmdpath,
               output_format = "qmd",
               args        = module_args)

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
      llm_code     <- if (use_llm) {
        'llm_use(TRUE)\nllm_model("ollama/qwen2.5:3b")\n\n'
      } else ""
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
        tags$hr(),
        shiny::p(tags$strong("The following R code would create this report directly from R:")),
        tags$pre(tags$code(code))
      )
    } else {
      NULL
    }
  })

  ### create_report_view ----
  observeEvent(input$create_report_view, {
    debug_msg("create_report_view")
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
    if (use) llm_model("ollama/qwen2.5:3b")
  })

  ## load ----
  debug_msg("---- load ----")

  ### demo ----
  observeEvent(input$demo, {
    debug_msg("demo")
    p <- paperlist(demopaper())
    update_from_paper(p)
  })

  ### batch_demo ----
  observeEvent(input$batch_demo, {
    debug_msg("batch_demo")
    update_from_paper(psychsci)
  })

  ### convert_paper ----
  observeEvent(input$convert_paper, {
    debug_msg("convert_paper")
    update_from_paper(list())
    files <- input$convert_paper

    tryCatch({
      n   <- length(files$datapath)
      s   <- vector("character", n)
      tmp <- tempfile()
      dir.create(tmp, showWarnings = FALSE)
      on.exit(unlink(tmp, recursive = TRUE))

      withProgress(message = 'Converting files', value = 0, {
        detail <- paste0("1/", n, " (", files$name[[1]], ")")
        incProgress(0, detail = detail)

        for (i in seq_along(files$datapath)) {
          path <- files$datapath[[i]]
          s[[i]] <- convert(path, tmp, crossref_lookup = FALSE)
          if (i < n) {
            detail <- paste0(i + 1, "/", n, " (", files$name[[i + 1]], ")")
          }
          incProgress(1 / n, detail = detail)
        }
      })

      papers <- read(tmp) |> paperlist()
      names(papers) <- basename(files$name) |> gsub("\\.(pdf|PDF)$", "", x = _)
      for (i in seq_along(papers)) {
        name <- names(papers)[[i]]
        papers[[i]]$paper_id      <- name
        papers[[i]]$info$filename <- basename(files$name[[i]])
      }

      update_from_paper(papers)
    }, error = function(e) {
      shinyjs::alert(e$message)
    })
  }, ignoreNULL = TRUE)

  ### load_paper ----
  observeEvent(input$load_paper, {
    debug_msg("load_paper")
    update_from_paper(list())
    files <- input$load_paper

    tryCatch({
      s <- read(files$datapath) |> paperlist()
      names(s) <- basename(files$name) |> gsub("\\.json$", "", x = _)
      for (i in seq_along(s)) {
        name              <- names(s)[[i]]
        s[[i]]$paper_id   <- name
        s[[i]]$info$filename <- basename(files$name[[i]])
      }
      update_from_paper(s)
    }, error = function(e) {
      shinyjs::alert(e$message)
    })
  }, ignoreNULL = TRUE)

  ### update_from_paper ----
  update_from_paper <- function(paper) {
    debug_msg("update_from_paper")

    text_table(data.frame())
    llm_table(data.frame())
    mod_table(data.frame())
    mod_summary("")
    mod_title("")
    report_path("")

    removeCssClass("mod_title", "red")
    removeCssClass("mod_title", "yellow")
    removeCssClass("mod_title", "green")
    removeCssClass("mod_title", "na")
    removeCssClass("mod_title", "fail")
    removeCssClass("mod_title", "info")

    my_paper(paper)
  }

  ### n_papers_loaded ----
  output$n_papers_loaded <- renderText({
    n <- length(my_paper())
    paste0(n, " Paper", plural(n), " Loaded")
  })

  ### files_loaded ----
  output$files_loaded <- renderText({
    n <- length(my_paper())
    paste0("<p style='margin: 0 1em;'>", n, " Paper", plural(n), " Loaded</p>")
  })

  ### demo_paper_status ----
  output$demo_paper_status <- renderText({
    paper <- my_paper()
    if (length(paper) == 0) return("No paper loaded yet.")
    title <- paper[[1]]$info$title
    if (is.null(title) || !nzchar(as.character(title))) {
      paste(length(paper), "paper(s) loaded.")
    } else {
      paste("Loaded:", title)
    }
  })

  ### paper_name ----
  observeEvent(input$paper_name, {
    debug_msg("paper_name")
    info <- my_paper()[[input$paper_name]]$info
  })

  output$paper_title <- renderUI({
    req(input$paper_name, my_paper())
    h4(my_paper()[[input$paper_name]]$info$title)
  })
  output$paper_desc <- renderUI({
    req(input$paper_name, my_paper())
    abstract <- text_search(my_paper()[[input$paper_name]], return = "section") |>
      dplyr::filter(section_type == "abstract") |>
      _$text
    p(abstract)
  })
  output$paper_keywords <- renderText({
    req(input$paper_name, my_paper())
    my_paper()[[input$paper_name]]$info$keywords |>
      unlist() |>
      paste(collapse = "; ")
  })

  ## text ----

  ### text_table ----
  output$text_table <- renderDT({
    debug_msg("text_table")
    cols <- c("text", "text_id", "paper_id", "header", "section_type")
    tt   <- text_table()
    if (length(unique(tt$paper_id)) == 1) cols <- setdiff(cols, "paper_id")
    tt[, cols]
  },
  selection = 'none',
  rownames  = FALSE,
  options   = dt_options
  )

  ### text_search ----
  observeEvent(input$text_search, {
    debug_msg("text_search")
    text <- text_table()
    if (!"table" %in% input$search_options | nrow(text) == 0) text <- my_paper()
    text_table(data.frame())

    tryCatch({
      sec <- input$search_section
      if (sec == "all") sec <- NULL

      tt <- text_search(text,
                        pattern     = input$search_pattern,
                        return      = input$search_return,
                        ignore.case = "ignore.case" %in% input$search_options,
                        fixed       = "fixed"        %in% input$search_options,
                        perl        = "perl"         %in% input$search_options)
      text_table(tt)

      sec <- unique(tt$section_type)
      sec <- sec[!is.na(sec)] |> sort()
      sec <- c("all", sec)
      updateSelectInput(session, "search_section", choices = sec, selected = "all")
    }, error = function(e) {
      shinyjs::alert(e$message)
    })
  }, ignoreNULL = TRUE)

  ### input$search_options ----
  observeEvent(input$search_options, {
    debug_msg("search_options")
    selected <- input$search_options
    choices  <- c("Search this table" = "table",
                  "Fixed"             = "fixed",
                  "Ignore Case"       = "ignore.case",
                  "PERL regex"        = "perl")
    if ("fixed" %in% selected) {
      selected <- base::setdiff(selected, c("ignore.case", "perl"))
      choices  <- choices[1:2]
    }
    updateCheckboxGroupInput(session, "search_options",
                             choices = choices, selected = selected)
  }, ignoreNULL = FALSE)

  ### search_reset ----
  observeEvent(input$search_reset, {
    debug_msg("search_reset")
    updateTextAreaInput(session, "search_pattern", value = "*")
    s <- my_paper()
    if (length(s) > 0) text_search(s) |> text_table()
  })

  ### search presets ----
  observeEvent(input$search_preset_p, {
    updateTextAreaInput(session, "search_pattern",
                        value = "p\\s*(=|<|>)+\\s*[0-9\\.,-]*\\d")
  })
  observeEvent(input$search_preset_n, {
    updateTextAreaInput(session, "search_pattern",
                        value = "N\\s*=\\s*[0-9,\\.]*\\d")
  })

  ### download_table ----
  output$download_table <- downloadHandler(
    filename = function() { debug_msg("download_table"); "table.csv" },
    content  = function(file) write.csv(text_table(), file, row.names = FALSE)
  )

  ## report ----

  ### report_run ----
  observeEvent(input$report_run, {
    debug_msg("report_run")
    report_path("")
    qmdpath <- tempfile(fileext = ".qmd")
    path    <- sub("qmd$", "html", qmdpath)

    withProgress(message = "Run Report:", value = 0, {
      modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)
      modules  <- sapply(modlists, \(x) input[[x]]) |> unlist() |> unname()

      if (length(my_paper()) == 0) return(NULL)

      incProgress(1/3, detail = "Modules")
      sink <- tryCatch({
        report(my_paper()[[1]],
               modules = modules,
               output_file = qmdpath,
               output_format = "qmd")
      }, error = function(e) {
        showModal(modalDialog(title = "Report Error", e$message,
                              easyClose = TRUE,
                              footer = tagList(modalButton("Dismiss"))))
        return("")
      })

      incProgress(1/3, detail = "Rendering HTML")
      sink <- tryCatch({
        quarto::quarto_render(input = qmdpath, quiet = TRUE,
                              output_format = "html",
                              metadata = list(html = list(theme = NULL)))
      }, error = function(e) {
        showModal(modalDialog(title = "HTML Render Error", e$message,
                              easyClose = TRUE,
                              footer = tagList(modalButton("Dismiss"))))
        return("")
      })

      incProgress(1/3, detail = "Complete!")
    })

    report_path(path)
    shinyjs::click("report_view")
  })

  ### report_view ----
  observeEvent(input$report_view, {
    debug_msg("report_view")
    report_file <- report_path()
    if (!file.exists(report_file)) return(NULL)
    addResourcePath("tmp", dirname(report_file))
    url <- paste0("tmp/", basename(report_file))
    session$sendCustomMessage(type = "openTab", message = url)
  })

  ### report_defaults ----
  update_report_modules <- function(modules) {
    modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)
    for (ml in modlists) updateCheckboxGroupInput(session, ml, selected = modules)
  }

  observeEvent(input$report_defaults, {
    debug_msg("report_defaults")
    update_report_modules(c(
      "prereg_check", "funding_check", "coi_check", "power",
      "repo_check", "code_check", "stat_check", "stat_p_exact",
      "stat_p_nonsig", "stat_effect_size", "marginal",
      "ref_accuracy", "ref_replication", "ref_retraction",
      "ref_pubpeer", "ref_summary"
    ))
  })

  observeEvent(input$report_info, {
    debug_msg("report_info")
    update_report_modules(c("all_p_values", "all_urls"))
  })

  ### report_dl_quarto ----
  output$report_dl_quarto <- downloadHandler(
    filename = function() { debug_msg("report_dl_quarto"); "metacheck_report.qmd" },
    content  = function(file) {
      output_file <- sub("html$", "qmd", report_path())
      if (!file.exists(output_file)) return(NULL)
      file.copy(output_file, file)
    }
  )

  ### report_dl_html ----
  output$report_dl_html <- downloadHandler(
    filename = function() { debug_msg("report_dl_html"); "metacheck_report.html" },
    content  = function(file) {
      output_file <- report_path()
      if (!file.exists(output_file)) return(NULL)
      file.copy(output_file, file)
    }
  )

  ## modules ----

  ### module_list ----
  observeEvent(input$module_list, {
    debug_msg("module_list")
    info <- module_info(input$module_list)
    mod_desc(info$description)
    mod_details(info$details)
    mod_title("")
    mod_table(data.frame())
    mod_summary("")
    removeCssClass("mod_title", "red")
    removeCssClass("mod_title", "yellow")
    removeCssClass("mod_title", "green")
    removeCssClass("mod_title", "na")
    removeCssClass("mod_title", "fail")
    removeCssClass("mod_title", "info")
  }, ignoreNULL = FALSE)

  ### run_module ----
  observeEvent(input$run_module, {
    shinyjs::disable("run_module")
    on.exit(shinyjs::enable("run_module"))
    if (length(my_paper()) == 0) return(NULL)

    output <- tryCatch({
      module_run(my_paper(), input$module_list)
    }, error = function(e) {
      list(module = input$module_list,
           title  = paste("Module Failure:", input$module_list),
           table  = data.frame(),
           report = e$message,
           traffic_light = "fail")
    })

    mod_title(output$title)
    removeCssClass("mod_title", "red")
    removeCssClass("mod_title", "yellow")
    removeCssClass("mod_title", "green")
    removeCssClass("mod_title", "na")
    removeCssClass("mod_title", "fail")
    removeCssClass("mod_title", "info")
    addCssClass("mod_title", output$traffic_light)
    mod_table(output$table %||% data.frame())
    mod_summary(output$summary_text %||% "")
  })

  ### mod_table ----
  output$mod_table <- renderDT({
    debug_msg("mod_table")
    mod_table()
  },
  selection = 'none',
  rownames  = FALSE,
  options   = dt_options
  )

  ### mod_title ----
  output$mod_title <- renderText({
    debug_msg("mod_title")
    mod_title()
  })

  ### mod_desc ----
  output$mod_desc <- renderText({
    debug_msg("mod_desc")
    mod_desc()
  })

  ### mod_details ----
  output$mod_details <- renderText({
    debug_msg("mod_details")
    mod_details() |> md2html()
  })

  ### mod_summary ----
  output$mod_summary <- renderText({
    debug_msg("mod_summary")
    mod_summary() |> md2html()
  })

  ## llm ----

  output$total_cost <- renderValueBox({
    valueBox(round(total_cost(), 5), "total cost",
             icon = icon("dollar-sign"), color = "green")
  })

  ### llm_max_calls ----
  observeEvent(input$llm_max_calls, {
    debug_msg("llm_max_calls")
    if (is.numeric(input$llm_max_calls)) {
      llm_max_calls(input$llm_max_calls)
      updateNumericInput(session, "llm_max_calls", value = llm_max_calls())
    }
  })

  ### llm_submit ----
  observeEvent(input$llm_submit, {
    debug_msg("llm_submit")
    text   <- text_table()
    groups <- unique(text[, input$llm_group_by, drop = FALSE])

    if (nrow(groups) > input$llm_max_calls) {
      showModal(modalDialog(
        title = "Too many calls",
        paste("This will create", nrow(groups),
              "calls to the LLM. Set the maximum number allowed higher if this is OK."),
        easyClose = TRUE,
        footer = tagList(modalButton("Dismiss"))
      ))
    } else {
      subtext <- text_search(text, return = input$llm_group_by)
      res     <- llm(text = subtext, system_prompt = input$llm_query)
      llm_table(res)
    }
  })

  ### llm_table ----
  output$llm_table <- renderDT({
    debug_msg("llm_table")
    gt <- llm_table()
    if (!is.null(gt$cost)) {
      total_cost(sum(gt$cost))
      gt$cost <- round(gt$cost, 5)
    }
    gt
  },
  selection = 'none',
  rownames  = FALSE,
  options   = dt_options
  )

  ### download_llm ----
  output$download_llm <- downloadHandler(
    filename = function() { debug_msg("download_llm"); "llm.csv" },
    content  = function(file) write.csv(llm_table(), file, row.names = FALSE)
  )

  debug_msg("server functions created")

  # .app.paper ----
  if (exists(".app.paper.") && !is.null(.app.paper.)) {
    if ("scivrs_paper" %in% class(.app.paper.)) {
      .app.paper.        <- list(.app.paper.)
      names(.app.paper.) <- .app.paper.[[1]]$name
    }
    update_from_paper(.app.paper.)
  }

} # end server()

shinyApp(ui, server)
