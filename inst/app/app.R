## app.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(DT)
  library(papercheck)
  library(dplyr)
  library(shiny.i18n)
})

source("R/constants.R")
source("R/funcs.R")
source("i18n/trans.R")


## Interface Tab Items ----
source("tabs/load.R")
source("tabs/text.R")
source("tabs/llm.R")
source("tabs/mod.R")
source("tabs/report.R")

## UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "PaperCheck"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Load Files", tabName = "load_tab",
               icon = icon("file")),
      menuItem("Report", tabName = "report_tab",
               icon = icon("list-check")),
      menuItem("Modules", tabName = "mod_tab",
               icon = icon("database")),
      menuItem("Search Text", tabName = "text_tab",
               icon = icon("magnifying-glass")),
      menuItem("LLM", tabName = "llm_tab",
               icon = icon("robot"))
    ),
    actionButton("demo", "Load Demo File"),
    actionButton("batch_demo", "Load Batch Demo"),
    #actionButton("reset_paper", "Reset"),
    actionButton("return_paper", "Quit & Return"),
    tags$br(),

    selectInput("lang", "Change language",
                choices = c(English = "en",
                            Dutch = "nl",
                            Spanish = "es",
                            Chinese = "zh"),
                selected = "en"),
    p("Most of the phrases have not been translated; this is just a proof of concept.", style="margin: 0 1em;")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      load_tab,
      mod_tab,
      report_tab,
      text_tab,
      llm_tab
    )
  )
)


## server ----
server <- function(input, output, session) {
  updateNumericInput(session, "llm_max_calls", value = papercheck::llm_max_calls())

  if (Sys.getenv("GROQ_API_KEY") != "") hide("llm_api")

  ## reactiveVals ----
  debug_msg("----reactiveVals----")

  my_paper <- reactiveVal( list() )
  text_table <- reactiveVal( data.frame() )
  llm_table <- reactiveVal( data.frame() )
  mod_table <- reactiveVal( data.frame() )
  mod_report <- reactiveVal( "" )
  mod_title <- reactiveVal( "" )
  report_path <- reactiveVal( "" )
  total_cost <- reactiveVal(0)

  ### return_paper ----
  observeEvent(input$return_paper, {
    debug_msg("return_paper")

    # just return sv object if only one paper
    s <- my_paper()
    if (length(s) == 1) s <- s[[1]]

    stopApp(s)
  })

  observe({
    paper <- my_paper()

    if (length(paper) > 0) {
      text_table(search_text(paper))

      # reset search interface
      # c("search_pattern",
      #   "search_section",
      #   "search_return",
      #   "search_ignore_case",
      #   "search_fixed") |> sapply(shinyjs::reset)
      choices <- names(paper)
    } else {
      choices <- c()
    }
    updateSelectInput(session, "paper_name", choices = choices)
  })

  ### on text_table() change ----
  observe({
    needs_text_table <- c("download_table", "llm_submit", "search_text",
                          "run_statcheck", "check_p_values")
    if (nrow(text_table()) == 0) {
      lapply(needs_text_table, shinyjs::disable)
    } else {
      lapply(needs_text_table, shinyjs::enable)
      #shinyjs::click("search_text") # trigger search
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

  ## load ----
  debug_msg("---- load ----")

  ### demo ----
  observeEvent(input$demo, {
    debug_msg("demo")

    p <- list(read_grobid(demoxml()))
    id <- "to_err_is_human.xml"
    names(p) <- id
    p[[1]]$name <- id
    p[[1]]$info$filename <- id
    p[[1]]$full_text$id = id
    update_from_paper(p)
  })

  ### batch_demo ----
  observeEvent(input$batch_demo, {
    debug_msg("batch_demo")

    s <- read_grobid(demodir())
    update_from_paper(s)
  })

  ### load_xml ----
  observeEvent(input$load_xml, {
    debug_msg("load_xml")

    tryCatch({
      n <- length(input$load_xml$datapath)
      s <- vector("list", n)
      withProgress(message = 'Processing files', value = 0, {
        detail <- paste("1/", n, " (", input$load_xml$name[[1]], ")")
        incProgress(0, detail = detail)

        for (i in seq_along(input$load_xml$datapath)) {
          path <- input$load_xml$datapath[[i]]
          s[[i]] <- read_grobid(path)
          if (i < n) {
            detail <- paste(i+1, "/", n, " (",
                            input$load_xml$name[[i+1]], ")")
          }
          incProgress(1/n, detail = detail)
        }
      })

      # fix filename because of shiny upload
      names(s) <- basename(input$load_xml$name)
      for (i in seq_along(s)) {
        name <- names(s)[[i]]
        s[[i]]$name <- name
        s[[i]]$info$filename <- name
        s[[i]]$full_text$id <- name
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
    mod_report("")
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
    p <- ifelse(n==1, "paper", "papers")
    paste(n, p, "loaded")
  })

  ### paper_name ----
  observeEvent(input$paper_name, {
    debug_msg("paper_name")

    info <- my_paper()[[input$paper_name]]$info

    #updateTextInput(session, "paper_title", value = info$title)
    # updateTextAreaInput(session, "paper_desc",
    #                     value = info$description)
    # updateTextInput(session, "paper_keywords",
    #                 value = paste(info$keywords, collapse = "; "))
  })

  output$paper_title <- renderUI({
    h4(my_paper()[[input$paper_name]]$info$title)
  })
  output$paper_desc <- renderUI({
    p(my_paper()[[input$paper_name]]$info$description)
  })
  output$paper_keywords <- renderText({
    my_paper()[[input$paper_name]]$info$keywords |>
      paste(collapse = "; ")
  })

  ## text ----

  ### text_table ----
  output$text_table <- renderDT({
    debug_msg("text_table")

    text_table()
  },
  selection = 'none',
  rownames = FALSE,
  options = dt_options
  )

  ### search_text ----
  observeEvent(input$search_text, {
    debug_msg("search_text")

    text <- text_table()
    if (!"table" %in% input$search_options | nrow(text) == 0) {
      text <- my_paper()
    }
    text_table(data.frame()) # clear table

    tryCatch({
      sec <- input$search_section
      if (sec == "all") sec <- NULL

      tt <- search_text(text,
                        pattern = input$search_pattern,
                        section = sec,
                        return = input$search_return,
                        ignore.case = "ignore.case" %in% input$search_options,
                        fixed = "fixed" %in% input$search_options,
                        perl = "perl" %in% input$search_options
      )
      text_table(tt)

      # update interface elements
      updateCheckboxGroupInput(session, "llm_group_by",
                               choices = names(tt),
                               selected = "file",
                               inline = TRUE)

      sec <- unique(tt$section)
      sec <- sec[!is.na(sec)] |> sort()
      sec <- c("all", sec)
      updateSelectInput(session, "search_section",
                        choices = sec,
                        selected = "all")
    }, error = function(e) {
      shinyjs::alert(e$message)
    })
  }, ignoreNULL = TRUE)

  ### input$search_options ----
  observeEvent(input$search_options, {
    debug_msg("search_options")
    debug_msg(input$search_options)

    selected <- input$search_options
    choices <- c("Search this table" = "table",
                 "Fixed" = "fixed",
                 "Ignore Case" = "ignore.case",
                 "PERL regex" = "perl")
    if ("fixed" %in% selected) {
      selected <- base::setdiff(selected, c("ignore.case", "perl"))
      choices <- choices[1:2]
    }
    updateCheckboxGroupInput(session, "search_options",
                             choices = choices,
                             selected = selected)
  }, ignoreNULL = FALSE)

  ### search_reset ----
  observeEvent(input$search_reset, {
    debug_msg("search_reset")

    updateTextAreaInput(session, "search_pattern", value = "*")
    s <- my_paper()

    if (length(s) > 0) {
      search_text(s) |> text_table()
    }
  })

  ### search presets ----

  observeEvent(input$search_preset_p, {
    updateTextAreaInput(session, "search_pattern", value = "p\\s*(=|<|>)+\\s*[0-9\\.,-]*\\d")
  })

  observeEvent(input$search_preset_n, {
    updateTextAreaInput(session, "search_pattern", value = "N\\s*=\\s*[0-9,\\.]*\\d")
  })


  ### download_table ----
  output$download_table <- downloadHandler(
    filename = function() {
      debug_msg("download_table")
      paste0("table.csv")
    },
    content = function(file) {
      write.csv(text_table(), file, row.names = FALSE)
    }
  )

  ## report ----

  ### report_run ----
  observeEvent(input$report_run, {
    debug_msg("report_run")

    report_path("")

    waiter <- waiter::Waiter$new(id = "report_text")
    waiter$show()
    on.exit(waiter$hide())

    # modules <- input$report_module_list
    modules <- c(input$module_text,
                 input$module_code,
                 input$module_ml,
                 input$module_ai)

    if (length(my_paper()) == 0) return(NULL)

    path <- tryCatch({
       report(my_paper()[[1]],
              modules = modules,
              output_file = tempfile(fileext = ".qmd"),
              output_format = "qmd")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Report Error",
        e$message,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Dismiss")
        )
      ))
      return("")
    })

    report_path(path)
  })

  ### report_defaults ----

  update_report_modules <- function(modules) {
    updateCheckboxGroupInput(session, "module_text", selected = modules)
    updateCheckboxGroupInput(session, "module_code", selected = modules)
    updateCheckboxGroupInput(session, "module_ml", selected = modules)
    updateCheckboxGroupInput(session, "module_ai", selected = modules)
  }

  observeEvent(input$report_defaults, {
    debug_msg("report_defaults")

    modules <- c("imprecise-p",
                 "marginal",
                 "osf-check",
                 "retractionwatch",
                 "ref-consistency")

    update_report_modules(modules)
  })

  observeEvent(input$report_info, {
    debug_msg("report_info")

    modules <- c("all-p-values", "all-urls")

    update_report_modules(modules)
  })

  ### report_text ----
  output$report_text <- renderUI({
    debug_msg("report_text")

    if (!file.exists(report_path())) {
      return("")
    }

    # waiter <- waiter::Waiter$new(id = "report_text")
    # waiter$show()
    # on.exit(waiter$hide())
    #
    # tryCatch({
    #   quarto::quarto_render(input = report_path(),
    #                         quiet = TRUE,
    #                         output_format = "html",
    #                         metadata = list(html = list(theme = NULL))
    #                         )
    # })

    report_text <- report_path() |>
      #sub("qmd$", "html", x = _) |>
      readLines() |>
      paste(collapse = "\n") |>
      #HTML()
      tags$textarea(rows = 20, readonly = "readonly")

    return(report_text)
  })

  ### report_dl_quarto ----
  output$report_dl_quarto <- downloadHandler(
    filename = function() {
      debug_msg("report_dl_quarto")
      paste0("papercheck_report.qmd")
    },
    content = function(file) {
      file.copy(report_path(), file)
    }
  )

  ### report_dl_html ----
  output$report_dl_html <- downloadHandler(
    filename = function() {
      debug_msg("report_dl_html")
      paste0("papercheck_report.html")
    },
    content = function(file) {
      waiter <- waiter::Waiter$new(id = "report_text")
      waiter$show()
      on.exit(waiter$hide())

      tryCatch({
        quarto::quarto_render(input = report_path(),
                              quiet = TRUE,
                              output_format = "html")
      })

      output_file <- sub("qmd$", "html", report_path())
      if (!file.exists(output_file)) return(NULL)
      file.copy(output_file, file)
    }
  )

  ## modules ----

  ### run_module ----
  observeEvent(input$run_module, {
    output <- tryCatch({
      module_run(my_paper(), input$module_list)
    }, error = function(e) {
      err <- list(
        module = input$module_list,
        title = paste("Module Failure:", input$module_list),
        table = data.frame(),
        report = e$message,
        traffic_light = "fail"
      )
      return(err)
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
    mod_report(output$report %||% "")

  })

  ### mod_table ----
  output$mod_table <- renderDT({
    debug_msg("mod_table")

    mod_table()
  },
  selection = 'none',
  rownames = FALSE,
  options = dt_options
  )

  ### mod_title ----
  output$mod_title <- renderText({
    debug_msg("mod_title")

    mod_title()
  })

  ### mod_report ----
  output$mod_report <- renderText({
    debug_msg("mod_report")

    mod_report()
  })

  ## llm ----

  output$total_cost <- renderValueBox({
    valueBox(
      round(total_cost(), 5),
      "total cost",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })

  ### llm_max_calls----
  observeEvent(input$llm_max_calls, {
    debug_msg("llm_max_calls")
    if (is.numeric(input$llm_max_calls)) {
      llm_max_calls(input$llm_max_calls)
      newmax <- llm_max_calls()
      updateNumericInput(session, "llm_max_calls", value = newmax)
    }
  })

  ### llm_submit----
  observeEvent(input$llm_submit, {
    debug_msg("llm_submit")

    text <- text_table()
    groups <- unique(text[, input$llm_group_by, drop = FALSE])

    if (nrow(groups) > input$llm_max_calls) {
      showModal(modalDialog(
        title = "Too many calls",
        paste("This will create", nrow(groups), "calls to the LLM. Set the maximum number allowed higher if this is OK."),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Dismiss")
        )
      ))
    } else {
      subtext <- search_text(text, return = input$llm_group_by)
      res <- llm(text = subtext,
                      query = input$llm_query,
                      API_KEY = input$llm_api)
      #
      # n <- nrow(groups)
      # res <- vector("list", n)
      # withProgress(message = 'Querying LLM', value = 0, {
      #   detail <- paste(groups[1, ], collapse = ":") |>
      #     paste("1/", n, " (", x = _, ")")
      #   incProgress(0, detail = detail)
      #   for (i in 1:n) {
      #     subtext <- dplyr::semi_join(text, groups[i, ,drop = FALSE],
      #                                 by = input$llm_group_by)
      #     res[[i]] <- llm(text = subtext,
      #                     query = input$llm_query,
      #                     API_KEY = input$llm_api)
      #     if (i < n) {
      #       detail <- paste(groups[i+1, ], collapse = ":") |>
      #         paste(i+1, "/", n, " (", x = _, ")")
      #     }
      #     incProgress(1/n, detail = detail)
      #   }
      # })
      #
      # res <- do.call(rbind, res)
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
  rownames = FALSE,
  options = dt_options
  )

  ### download_llm ----
  output$download_llm <- downloadHandler(
    filename = function() {
      debug_msg("download_llm")
      paste0("llm.csv")
    },
    content = function(file) {
      write.csv(llm_table(), file, row.names = FALSE)
    }
  )

  ## translation ----
  debug_msg("----translation ----")

  ### i18n ----
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  ### lang ----
  observeEvent(input$lang, {
    debug_msg("lang")

    # text changes (h3, h4, p)
    for (h in trans_text) {
      suppressWarnings(tt <- i18n()$t(h))

      js <- sprintf("$('*[en=\"%s\"]').text(\"%s\");",
                    gsub("'", "\\\\'", h), tt)
      shinyjs::runjs(js)
    }

    # input label changes
    for (func in names(trans_labels)) {
      for (nm in names(trans_labels[[func]])) {
        l <- trans_labels[[func]][[nm]]
        tl <- suppressWarnings(
          i18n()$t(l)
        )
        if (tl == "") tl <- NULL

        args <- list(
          session = session,
          inputId = nm,
          label = tl
        )

        # set up choices for relevant inputs
        ch <- trans_choices[[func]][[nm]]
        if (!is.null(ch)) {
          tch <- suppressWarnings(
            i18n()$t(names(ch))
          )
          new_choices <- stats::setNames(ch, tch)
          #debug_msg(dput(new_choices))
          args$choices <- new_choices
          args$selected <- input[[nm]]
        }

        do.call(func, args)
      }
    }
  }, ignoreInit = TRUE)

  # save_trans ----
  save_trans(trans_text, trans_labels)

  debug_msg("server functions created")

  # .app.paper ----
  if (exists(".app.paper.") && !is.null(.app.paper.)) {
    if ("scivrs_paper" %in% class(.app.paper.)) {
      .app.paper. <- list(.app.paper.)
      names(.app.paper.) <- .app.paper.[[1]]$name
    }
    update_from_paper( .app.paper. )
  }

} # end server()

shinyApp(ui, server)
