## demo_app.R — Upload/Demo, Advanced modules, and Text Search ##
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
source("tabs/advanced.R")
source("tabs/text.R")
source("tabs/options_demo.R")


## Main tab — everything in one logical flow:
## 1. load files / demo papers, 2. search text, 3. run modules & create report
main_tab <- tabItem(
  tabName = "main_tab",

  advanced_info_box,

  # ---- 1. Load files & demo papers ----
  box(
    width = 12, title = "Load Files, a Demo Paper, or Psychological Science articles",
    tags$small(
      "Use ", tags$em("Convert PDF"), " to convert a single PDF and load it. ",
      "Use ", tags$em("Load JSON"), " for files already converted with ",
      tags$code("convert()"), "."
    ),
    fluidRow(
      column(width = 6,
             fileInput("load_paper", "Load JSON",
                       multiple = TRUE, width = "100%", accept = ".json")),
      column(width = 6,
             fileInput("convert_paper", "Convert PDF",
                       multiple = FALSE, width = "100%", accept = ".pdf"))
    ),
    uiOutput("adv_download_json_ui"),
    fluidRow(
      column(width = 6,
             actionButton("demo", "Load Demo Paper", icon = icon("file"))),
      column(width = 6,
             actionButton("batch_demo", "Load Psychological Science Papers", icon = icon("layer-group")))
    ),
    tags$br(),
    box(
      width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Paper Info",
      selectInput("paper_name", "Paper", c()),
      uiOutput("paper_title"),
      uiOutput("paper_desc"),
      textOutput("paper_keywords")
    )
  ),

  # ---- 2. Search text ----
  text_search_block,

  # ---- 3. Select modules & create report ----
  advanced_modules_box,
  advanced_explorer_box
)


## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "metacheck — Demo"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Metacheck", tabName = "main_tab",
               icon = icon("list-check"), selected = TRUE),
      menuItem("Options",   tabName = "options_tab",
               icon = icon("sliders"))
    ),
    uiOutput("files_loaded"),
    tags$br(),
    HTML("<img src='images/logo.png' alt='Logo' style='width: 85%; margin: 1em;' />")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      main_tab,
      options_demo_tab
    )
  )
)


## server ----
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)

  ## reactiveVals ----
  my_paper    <- reactiveVal(list())
  text_table  <- reactiveVal(data.frame())
  mod_table   <- reactiveVal(data.frame())
  mod_summary <- reactiveVal("")
  mod_title   <- reactiveVal("")
  mod_desc    <- reactiveVal("")
  mod_details <- reactiveVal("")
  report_path <- reactiveVal("")

  adv_converted_json <- reactiveVal(NULL)

  ### options_done — return to the main tab ----
  observeEvent(input$options_done, {
    updateTabItems(session, "tabs", "main_tab")
  })

  ### create_llm_backend — configure the LLM (Power module) ----
  observeEvent(input$create_llm_backend, {
    backend <- input$create_llm_backend %||% "none"

    if (backend == "none") {
      llm_use(FALSE)
      return(invisible())
    }

    if (backend == "ollama") {
      available <- tryCatch(llm_model_list("ollama")$id,
                            error = function(e) character(0))
      if (length(available) == 0) {
        shinyjs::alert(
          paste0("No Ollama models found. Please install Ollama and pull at ",
                 "least one model before using the LLM backend.\nSee: ",
                 "https://www.scienceverse.org/metacheck/articles/ollama.html")
        )
        llm_use(FALSE)
        updateRadioButtons(session, "create_llm_backend", selected = "none")
        return(invisible())
      }
      preferred <- c("qwen2.5:3b", "llama3.2:3b", "mistral:7b")
      model_id  <- preferred[preferred %in% available][1]
      if (is.na(model_id)) model_id <- available[[1]]
      llm_use(TRUE)
      llm_model(paste0("ollama/", model_id))

    } else if (backend == "groq") {
      if (!nzchar(Sys.getenv("GROQ_API_KEY"))) {
        shinyjs::alert(
          paste0("No GROQ_API_KEY found. The Groq backend requires an API key.\n",
                 "Get a key at https://console.groq.com/keys and add it to your ",
                 ".Renviron as GROQ_API_KEY.\nSee: ",
                 "https://www.scienceverse.org/metacheck/reference/llm.html")
        )
        llm_use(FALSE)
        updateRadioButtons(session, "create_llm_backend", selected = "none")
        return(invisible())
      }
      available <- tryCatch(llm_model_list("groq")$id,
                            error = function(e) character(0))
      llm_use(TRUE)
      if (length(available) > 0) llm_model(paste0("groq/", available[[1]]))
    }
  }, ignoreNULL = TRUE)

  ### module gating from Options toggles ----
  # Disable the module checkboxes whose Options toggle is off, so users can't
  # run modules that send/retrieve external information they've opted out of.
  observe({
    for (toggle in names(demo_gating)) {
      enabled <- demo_toggle_on(input[[toggle]])
      mods    <- demo_gating[[toggle]]
      modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)
      for (ml in modlists) {
        for (m in mods) {
          sel <- paste0("#", ml, " input[value='", m, "']")
          if (enabled) {
            shinyjs::enable(selector = sel)
          } else {
            shinyjs::disable(selector = sel)
            # also clear any current selection of a now-disabled module
            cur <- input[[ml]]
            if (m %in% cur) {
              updateCheckboxGroupInput(session, ml,
                                       selected = setdiff(cur, m))
            }
          }
        }
      }
    }
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
  # Only the download button depends on having results; the Search button must
  # always be clickable (it is what produces the results in the first place).
  observe({
    if (nrow(text_table()) == 0) {
      shinyjs::disable("download_table")
    } else {
      shinyjs::enable("download_table")
    }
  })

  ## load ----

  ### update_from_paper ----
  update_from_paper <- function(paper) {
    text_table(data.frame())
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

  ### demo ----
  observeEvent(input$demo, {
    p <- paperlist(demopaper())
    update_from_paper(p)
  })

  ### batch_demo ----
  observeEvent(input$batch_demo, {
    update_from_paper(psychsci)
  })

  ### adv_download_json_ui ----
  output$adv_download_json_ui <- renderUI({
    path <- adv_converted_json()
    if (is.null(path) || !file.exists(path)) return(NULL)
    downloadButton("adv_download_json", "Download JSON")
  })

  output$adv_download_json <- downloadHandler(
    filename = function() basename(adv_converted_json()),
    content  = function(file) file.copy(adv_converted_json(), file)
  )

  ### convert_paper ----
  observeEvent(input$convert_paper, {
    update_from_paper(list())
    adv_converted_json(NULL)
    files <- input$convert_paper

    tryCatch({
      n   <- length(files$datapath)
      s   <- vector("character", n)
      tmp <- tempfile()
      dir.create(tmp, showWarnings = FALSE)

      withProgress(message = "Converting files", value = 0, {
        detail <- paste0("1/", n, " (", files$name[[1]], ")")
        incProgress(0, detail = detail)
        for (i in seq_along(files$datapath)) {
          s[[i]] <- convert(files$datapath[[i]], tmp, crossref_lookup = FALSE)
          if (i < n) {
            detail <- paste0(i + 1, "/", n, " (", files$name[[i + 1]], ")")
          }
          incProgress(1 / n, detail = detail)
        }
      })

      if (n == 1 && file.exists(s[[1]])) adv_converted_json(s[[1]])

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

  ### files_loaded ----
  output$files_loaded <- renderText({
    n <- length(my_paper())
    paste0("<p style='margin: 0 1em;'>", n, " Paper", plural(n), " Loaded</p>")
  })

  ### paper_name ----
  observeEvent(input$paper_name, {
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

  ## text search ----

  ### text_table ----
  output$text_table <- DT::renderDT({
    tt <- text_table()
    if (nrow(tt) == 0) return(data.frame())
    cols <- c("text", "text_id", "paper_id", "header", "section_type")
    cols <- intersect(cols, names(tt))
    if (length(unique(tt$paper_id)) == 1) cols <- setdiff(cols, "paper_id")
    tt[, cols, drop = FALSE]
  },
  selection = "none",
  rownames  = FALSE,
  options   = dt_options
  )

  ### text_search ----
  observeEvent(input$text_search, {
    if (length(my_paper()) == 0) {
      shinyjs::alert("Load a paper or demo first, then search its text.")
      return(NULL)
    }
    text <- text_table()
    if (!"table" %in% input$search_options || nrow(text) == 0) text <- my_paper()
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
      updateSelectInput(session, "search_section", choices = c("all", sec), selected = "all")
    }, error = function(e) {
      shinyjs::alert(e$message)
    })
  }, ignoreNULL = TRUE)

  ### search_options ----
  observeEvent(input$search_options, {
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
    updateTextAreaInput(session, "search_pattern", value = "*")
    s <- my_paper()
    if (length(s) > 0) text_search(s) |> text_table()
  })

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
    filename = function() "table.csv",
    content  = function(file) write.csv(text_table(), file, row.names = FALSE)
  )

  ## report ----

  ### report_run ----
  observeEvent(input$report_run, {
    report_path("")
    qmdpath <- tempfile(fileext = ".qmd")
    path    <- sub("qmd$", "html", qmdpath)

    modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)
    modules  <- sapply(modlists, \(x) input[[x]]) |> unlist() |> unname()

    # honour the Options gating server-side: drop any module whose toggle is off
    for (toggle in names(demo_gating)) {
      if (!demo_toggle_on(input[[toggle]])) {
        modules <- setdiff(modules, demo_gating[[toggle]])
      }
    }

    # validate before doing any work (return exits the observer, not a block)
    if (length(my_paper()) == 0) {
      showModal(modalDialog(title = "No paper loaded",
                            "Load a paper or demo before creating a report.",
                            easyClose = TRUE,
                            footer = tagList(modalButton("Dismiss"))))
      return(NULL)
    }

    if (length(modules) == 0) {
      showModal(modalDialog(title = "No modules selected",
                            paste("Select at least one module to include in",
                                  "the report. (Modules disabled in Options",
                                  "cannot be selected.)"),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Dismiss"))))
      return(NULL)
    }

    withProgress(message = "Run Report:", value = 0, {
      incProgress(1/3, detail = "Modules")
      tryCatch({
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
      tryCatch({
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
    report_file <- report_path()
    if (!file.exists(report_file)) return(NULL)
    addResourcePath("tmp", dirname(report_file))
    session$sendCustomMessage(type = "openTab",
                              message = paste0("tmp/", basename(report_file)))
  })

  ### report_defaults ----
  update_report_modules <- function(modules) {
    # only select modules whose Options toggle is enabled, so the selection
    # always matches what can actually run
    for (toggle in names(demo_gating)) {
      if (!demo_toggle_on(input[[toggle]])) {
        modules <- setdiff(modules, demo_gating[[toggle]])
      }
    }
    modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)
    for (ml in modlists) updateCheckboxGroupInput(session, ml, selected = modules)
  }

  observeEvent(input$report_defaults, {
    update_report_modules(c(
      "prereg_check", "funding_check", "coi_check", "power",
      "repo_check", "code_check", "stat_check", "stat_p_exact",
      "stat_p_nonsig", "stat_effect_size", "marginal",
      "ref_accuracy", "ref_replication", "ref_retraction",
      "ref_pubpeer", "ref_summary"
    ))
  })

  observeEvent(input$report_info, {
    update_report_modules(c("all_p_values", "all_urls"))
  })

  ### report_dl_quarto ----
  output$report_dl_quarto <- downloadHandler(
    filename = function() "metacheck_report.qmd",
    content  = function(file) {
      output_file <- sub("html$", "qmd", report_path())
      if (file.exists(output_file)) file.copy(output_file, file)
    }
  )

  ### report_dl_html ----
  output$report_dl_html <- downloadHandler(
    filename = function() "metacheck_report.html",
    content  = function(file) {
      output_file <- report_path()
      if (file.exists(output_file)) file.copy(output_file, file)
    }
  )

  ## module explorer ----

  ### module_list ----
  observeEvent(input$module_list, {
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

    result <- tryCatch({
      module_run(my_paper(), input$module_list)
    }, error = function(e) {
      list(module = input$module_list,
           title  = paste("Module Failure:", input$module_list),
           table  = data.frame(),
           report = e$message,
           traffic_light = "fail")
    })

    mod_title(result$title)
    removeCssClass("mod_title", "red")
    removeCssClass("mod_title", "yellow")
    removeCssClass("mod_title", "green")
    removeCssClass("mod_title", "na")
    removeCssClass("mod_title", "fail")
    removeCssClass("mod_title", "info")
    addCssClass("mod_title", result$traffic_light)
    mod_table(result$table %||% data.frame())
    mod_summary(result$summary_text %||% "")
  })

  ### mod_table ----
  output$mod_table <- DT::renderDT({
    mod_table()
  },
  selection = "none",
  rownames  = FALSE,
  options   = dt_options
  )

  output$mod_title <- renderText({ mod_title() })
  output$mod_desc  <- renderText({ mod_desc() })
  output$mod_details <- renderText({ mod_details() |> md2html() })
  output$mod_summary <- renderText({ mod_summary() |> md2html() })

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
