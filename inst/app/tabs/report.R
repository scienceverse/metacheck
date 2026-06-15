### report_tab ----
report_tab <- tabItem(
  tabName = "report_tab",

  h2("Generate a Metacheck Report"),
  tags$p(
    "Select a PDF file below. The report will be generated automatically ",
    "using all validated modules and will open in a new browser tab when ready."
  ),

  tags$div(class = "pdf-upload",
    fileInput(
      "upload_pdf", NULL,
      multiple = FALSE,
      accept = ".pdf",
      width = "100%",
      buttonLabel = tagList(icon("upload"), "Upload PDF"),
      placeholder = "No file selected"
    )
  ),

  # GDPR and privacy message, driven by the current Options settings
  box(title = "GDPR and Privacy",
      collapsible = TRUE,
      width = 12,
      tags$p("Change the settings in the 'Options' tab to enable or disable modules that use external servers."),
      uiOutput("gdpr_privacy_ui"),
      actionButton(
        "options_update", "Update Options",
        icon = icon("arrow-right"),
        class = "btn-options-done"
      )
  ),

  box(title = "R Code",
      collapsible = TRUE,
      width = 12,
      tags$p("The following R code would create this report directly from R:"),
      uiOutput("r_code")
  ),
  uiOutput("report_status_ui")
)
