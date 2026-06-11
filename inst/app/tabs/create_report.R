### create_report_tab ----
create_report_tab <- tabItem(
  tabName = "create_report_tab",

  fluidRow(
    column(
      width = 10, offset = 1,
      box(
        width = 12,
        h3("Generate a Metacheck Report"),
        tags$p(
          "Select a PDF file below. The report will be generated automatically ",
          "using all validated modules and will open in a new browser tab when ready."
        ),

        tags$div(
          class = "create-pdf-upload",
          fileInput(
            "create_pdf", NULL,
            multiple = FALSE,
            accept = ".pdf",
            width = "100%",
            buttonLabel = tagList(icon("upload"), "Upload PDF"),
            placeholder = "No file selected"
          )
        ),

        # GDPR and privacy message, driven by the current Options settings
        tags$div(
          class = "gdpr-privacy-box",
          h4("GDPR and privacy"),
          uiOutput("gdpr_privacy_ui")
        ),

        uiOutput("create_report_status_ui")
      )
    )
  )
)
