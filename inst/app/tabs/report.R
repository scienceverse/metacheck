# set up module list

modules <- papercheck::module_list()
mod <- list()
for (type in c("text", "code", "ml", "ai")) {
  m <- modules[modules$type == type, ]
  mod[[type]] <- stats::setNames(m$name, m$title)
}

### report_tab ----
report_tab <- tabItem(
  tabName = "report_tab",
  actionButton("report_info", "Info"),
  actionButton("report_defaults", "Defaults"),
  fluidRow(
    column(width = 3, checkboxGroupInput("module_text", "Text", mod$text)),
    column(width = 3, checkboxGroupInput("module_code", "Code", mod$code)),
    column(width = 3, checkboxGroupInput("module_ml", "ML", mod$ml)),
    column(width = 3, checkboxGroupInput("module_ai", "ChatGPT", mod$ai))
  ),

  # checkboxGroupInput("report_module_list", NULL,
  #             stats::setNames(module_list()$name, module_list()$title),
  #             c("imprecise-p",
  #               "osf-check",
  #               "retractionwatch",
  #               "marginal")),
  actionButton("report_run", "Run Report"),
  downloadButton("report_dl_quarto", "Download Quarto"),
  downloadButton("report_dl_html", "Download HTML"),

  uiOutput("report_text")
)
