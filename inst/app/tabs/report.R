# set up module list

modules <- metacheck::module_list()

### report_tab ----
report_tab <- tabItem(
  tabName = "report_tab",
  actionButton("report_info", "Info"),
  actionButton("report_defaults", "Defaults"),
  fluidRow(
    column(width = 12, checkboxGroupInput("report_module_list", "", modules$name))
  ),
  actionButton("report_run", "Run Report"),
  downloadButton("report_dl_quarto", "Download Quarto"),
  downloadButton("report_dl_html", "Download HTML"),

  uiOutput("report_text")
)
