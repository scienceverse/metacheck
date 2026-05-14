## app.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(DT)
  library(metacheck)
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
  dashboardHeader(title = "metacheck"),
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
               icon = icon("magnifying-glass")) #,
      # menuItem("LLM", tabName = "llm_tab",
      #          icon = icon("robot"))
    ),
    actionButton("demo", "Load Demo Paper"),
    actionButton("batch_demo", "Load PsychSci"),
    #actionButton("reset_paper", "Reset"),
    actionButton("return_paper", "Quit & Return"),
    uiOutput("files_loaded"),
    tags$br(),

    selectInput("lang", "Change language",
                choices = c(English = "en",
                            Dutch = "nl",
                            Spanish = "es",
                            Chinese = "zh"),
                selected = "en"),
    p("Most of the phrases have not been translated; this is just a proof of concept.",
      style="margin: 0 1em;"),
    HTML("<img src='images/logo.png' alt='Logo' style='width: 85%; margin: 1em;' />")
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
  updateNumericInput(session, "llm_max_calls", value = metacheck::llm_max_calls())

  if (Sys.getenv("GROQ_API_KEY") != "") hide("llm_api")

  ## reactiveVals ----
  debug_msg("----reactiveVals----")

  my_paper <- reactiveVal( list() )
  text_table <- reactiveVal( data.frame() )
  llm_table <- reactiveVal( data.frame() )
  mod_table <- reactiveVal( data.frame() )
  mod_summary <- reactiveVal( "" )
  mod_title <- reactiveVal( "" )
  mod_desc <- reactiveVal( "" )
  mod_details <- reactiveVal( "" )
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

    p <- paperlist(demopaper())
    update_from_paper(p)
  })

  ### batch_demo ----
  observeEvent(input$batch_demo, {
    debug_msg("batch_demo")

    #s <- psychsci[c(48, 91)]
    update_from_paper(psychsci)
  })

  ### convert_paper ----
  observeEvent(input$convert_paper, {
    debug_msg("convert_paper")
    update_from_paper(list()) # clear interface
    files <- input$convert_paper

    tryCatch({
      n <- length(files$datapath)
      s <- vector("character", n)
      tmp <- tempfile()
      dir.create(tmp, showWarnings = FALSE)
      on.exit(unlink(tmp, recursive = TRUE))

      withProgress(message = 'Converting files', value = 0, {
        detail <- paste0("1/", n, " (", files$name[[1]], ")")
        incProgress(0, detail = detail)

        for (i in seq_along(files$datapath)) {
          path <- files$datapath[[i]]
          s[[i]] <- convert(path, tmp, crossref_lookup=FALSE)
          if (i < n) {
            detail <- paste0(i+1, "/", n, " (",
                             files$name[[i+1]], ")")
          }
          incProgress(1/n, detail = detail)
        }
      })

      papers <- read(tmp) |> paperlist()

      # fix filename because of shiny upload
      names(papers) <- basename(files$name) |>
        gsub("\\.(pdf|PDF)$", "", x = _)
      for (i in seq_along(papers)) {
        name <- names(papers)[[i]]
        papers[[i]]$paper_id <- name
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
    update_from_paper(list()) # clear interface
    files <- input$load_paper

    tryCatch({
      s <- read(files$datapath) |> paperlist()

      # fix filename because of shiny upload
      names(s) <- basename(files$name) |>
        gsub("\\.json$", "", x = _)
      for (i in seq_along(s)) {
        name <- names(s)[[i]]
        s[[i]]$paper_id <- name
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
    paste0("<p style = 'margin: 0 1em;'>", n, " Paper", plural(n), " Loaded</p>")
  })

  ### paper_name ----
  observeEvent(input$paper_name, {
    debug_msg("paper_name")

    info <- my_paper()[[input$paper_name]]$info

    #updateTextInput(session, "paper_title", value = info$title)
    # updateTextAreaInput(session, "paper_desc",
    #                     value = info$abstract)
    # updateTextInput(session, "paper_keywords",
    #                 value = paste(info$keywords, collapse = "; "))
  })

  output$paper_title <- renderUI({
    req(input$paper_name, my_paper())
    h4(my_paper()[[input$paper_name]]$info$title)
  })
  output$paper_desc <- renderUI({
    req(input$paper_name, my_paper())
    abstract <- search_text(my_paper()[[input$paper_name]], return = "section") |>
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
    tt <- text_table()
    if (length(unique(tt$paper_id)) == 1) {
      cols <- setdiff(cols, "paper_id")
    }

    tt[, cols]
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
                        return = input$search_return,
                        ignore.case = "ignore.case" %in% input$search_options,
                        fixed = "fixed" %in% input$search_options,
                        perl = "perl" %in% input$search_options
      )
      text_table(tt)

      # update interface elements
      # updateCheckboxGroupInput(session, "llm_group_by",
      #                          choices = names(tt),
      #                          selected = "file",
      #                          inline = TRUE)

      sec <- unique(tt$section_type)
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

    # get checked values from module lists
    modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)
    modules <- sapply(modlists, \(x) input[[x]]) |> unlist() |> unname()

    if (length(my_paper()) == 0) return(NULL)

    path <- tempfile(fileext = ".qmd")
    sink <- tryCatch({
       report(my_paper()[[1]],
              modules = modules,
              output_file = path,
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
    modlists <- names(input) |> grep("^report_module_list_.+", x = _, value = TRUE)

    for (ml in modlists) {
      updateCheckboxGroupInput(session, ml, selected = modules)
    }
  }

  observeEvent(input$report_defaults, {
    debug_msg("report_defaults")

    modules <- c("prereg_check",
                 "funding_check",
                 "coi_check",
                 "power",
                 "repo_check",
                 "code_check",
                 "stat_check",
                 "stat_p_exact",
                 "stat_p_nonsig",
                 "stat_effect_size",
                 "marginal",
                 "ref_accuracy",
                 "ref_replication",
                 "ref_retraction",
                 "ref_pubpeer",
                 "ref_summary")

    update_report_modules(modules)
  })

  observeEvent(input$report_info, {
    debug_msg("report_info")

    modules <- c("all_p_values", "all_urls")

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
      paste0("metacheck_report.qmd")
    },
    content = function(file) {
      file.copy(report_path(), file)
    }
  )

  ### report_dl_html ----
  output$report_dl_html <- downloadHandler(
    filename = function() {
      debug_msg("report_dl_html")
      paste0("metacheck_report.html")
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

  ### module_list ---
  observeEvent(input$module_list, {
    debug_msg("module_list")

    info <- module_info(input$module_list)

    mod_desc(info$description)
    mod_details(info$details)

    # reset module output
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
    mod_summary(output$summary_text %||% "")
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

  ### mod_desc----
  output$mod_desc <- renderText({
    debug_msg("mod_desc")

    mod_desc()
  })

  ### mod_details----
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
                 system_prompt = input$llm_query)
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
