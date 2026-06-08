### run_demo_tab ----
run_demo_tab <- tabItem(
  tabName = "run_demo_tab",
  fluidRow(
    tabBox(
      width = 12,
      id = "run_demo_box",

      tabPanel(
        "Demo Paper",
        shiny::p("Load a single demo paper to explore metacheck."),
        actionButton("demo", "Load Demo Paper", icon = icon("file")),
        tags$br(), tags$br(),
        textOutput("demo_paper_status")
      ),

      tabPanel(
        "PsychSci Batch",
        shiny::p("Load all bundled Psychological Science papers for batch exploration."),
        actionButton("batch_demo", "Load PsychSci Papers", icon = icon("layer-group")),
        tags$br(), tags$br(),
        textOutput("n_papers_loaded")
      ),

      tabPanel(
        "Search Text",
        box(
          width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Search",
          fluidRow(
            column(width = 12, textAreaInput("search_pattern", "Pattern", "*", "100%"))
          ),
          fluidRow(
            column(width = 4, selectInput("search_section", "Section", c("all"))),
            column(width = 4, selectInput(
              "search_return", "Return",
              c("sentence", "paragraph", "section", "header", "match", "paper_id")
            )),
            column(width = 4, div(
              checkboxGroupInput(
                "search_options", NULL,
                c("Search this table" = "table",
                  "Fixed"            = "fixed",
                  "Ignore Case"      = "ignore.case",
                  "PERL regex"       = "perl"),
                selected = "ignore.case"
              )
            ))
          ),
          actionButton("text_search",      "Search"),
          actionButton("search_reset",     "Reset"),
          actionButton("search_preset_p",  "p-values"),
          actionButton("search_preset_n",  "sample size")
        ),
        downloadButton("download_table", "Download Table"),
        dataTableOutput("text_table")
      )
    )
  )
)
