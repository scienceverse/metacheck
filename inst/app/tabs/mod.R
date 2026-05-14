### mod_tab ----
mod_tab <- tabItem(
  tabName = "mod_tab",
  p("Check the details below for information on how modules work and whether they access external resources."),
  fluidRow(
    column(
      width = 6,
      selectInput("module_list", NULL,
                  stats::setNames(module_list()$name, module_list()$title))
    ),
    column(
      width = 6,
      actionButton("run_module", "Run Module"),
      downloadButton("download_mod_table", "Download Table")
    )
  ),
  textOutput("mod_desc"),
  textOutput("mod_title", container = tags$h2),
  uiOutput("mod_summary"),
  dataTableOutput("mod_table"),
  box(width = 12, collapsible = TRUE, collapsed = TRUE,
      title = "Module Details",
      uiOutput("mod_details")
  )
)
