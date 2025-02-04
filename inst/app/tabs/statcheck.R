### statcheck_tab ----
statcheck_tab <- tabItem(
  tabName = "statcheck_tab",
  actionButton("run_statcheck", "Run Statcheck"),
  actionButton("check_p_values", "P-values"),
  downloadButton("download_statcheck", "Download Table"),
  checkboxInput("statcheck_errors", "Only Show Errors"),
  dataTableOutput("statcheck_table")
)
