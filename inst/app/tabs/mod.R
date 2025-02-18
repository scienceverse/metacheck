### mod_tab ----
mod_tab <- tabItem(
  tabName = "mod_tab",
  selectInput("module_list", NULL,
              stats::setNames(module_list()$name, module_list()$title)),
  actionButton("run_module", "Run Module"),
  downloadButton("download_mod_table", "Download Table"),
  textOutput("mod_title", container = tags$h2),
  textOutput("mod_report"),
  dataTableOutput("mod_table")
)
