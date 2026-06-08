### create_report_tab ----
create_report_tab <- tabItem(
  tabName = "create_report_tab",

  # Inject styles directly so they are never stale from browser cache
  tags$style(HTML("
    .create-pdf-upload .input-group {
      border: 2px solid #59935B;
      border-radius: 4px;
      overflow: hidden;
    }
    .create-pdf-upload .btn-file {
      background-color: #59935B !important;
      color: #fff !important;
      font-weight: 600 !important;
      border: none !important;
    }
    .create-pdf-upload .btn-file:hover,
    .create-pdf-upload .btn-file:focus {
      background-color: #4a7d4c !important;
      color: #fff !important;
    }
    .create-pdf-upload .form-control {
      background-color: rgba(89,147,91,0.08) !important;
      border: none !important;
      box-shadow: none !important;
    }
    .create-report-switches .shiny-input-container {
      white-space: nowrap !important;
    }
    .create-report-switches .control-label {
      white-space: nowrap !important;
    }
  ")),

  fluidRow(
    column(
      width = 10, offset = 1,
      box(
        width = 12,
        h3("Generate a Metacheck Report"),
        tags$p(style = "font-size: 0.9em;",
          "Select a PDF file below. The report will be generated automatically ",
          "using all validated modules and will open in a new browser tab when ready."
        ),
        tags$p(style = "font-size: 0.9em;",
          "To run Metacheck without sending data to an external server, use a local GROBID server, and leave the Query Crossref, Query PubPeer, and Query Code Repositories options to FALSE. You can manually download repositories and link to the local folder. The GDPR compliant GROBID server in The Netherlands will not store files."
        ),
        tags$div(
          style = "font-size: 0.9em;",
          radioButtons(
            "grobid_server_choice",
            "PDF Conversion Server:",
            choiceNames = list(
              "Use GDPR compliant GROBID server in The Netherlands",
              "Use HuggingFace server in the USA",
              tagList(
                "Use local GROBID server through Docker — see ",
                tags$a("metacheck docs",
                  href = paste0("https://www.scienceverse.org/metacheck/",
                                "articles/metacheck.html#load-from-pdf"),
                  target = "_blank"),
                " and ",
                tags$a("GROBID docs",
                  href = "https://grobid.readthedocs.io/en/latest/",
                  target = "_blank")
              )
            ),
            choiceValues = list("metacheck", "huggingface", "local"),
            selected = "metacheck"
          ),
          tags$div(
            class = "create-report-switches",
            style = "display: grid; grid-template-columns: max-content 1fr; align-items: center; row-gap: 0.2em; column-gap: 1em; margin-bottom: 0.5em;",
            switchInput("create_crossref", "Query Crossref", FALSE,
              onLabel = "TRUE", offLabel = "FALSE",
              onStatus = "success", offStatus = "default",
              size = "small", labelWidth = "70px", inline = TRUE),
            tags$span("Send full references to Crossref API"),
            switchInput("create_pubpeer", "Query PubPeer", FALSE,
              onLabel = "TRUE", offLabel = "FALSE",
              onStatus = "success", offStatus = "default",
              size = "small", labelWidth = "70px", inline = TRUE),
            tags$span("Send reference DOIs to PubPeer API"),
            switchInput("create_repos", "Query Repos", FALSE,
              onLabel = "TRUE", offLabel = "FALSE",
              onStatus = "success", offStatus = "default",
              size = "small", labelWidth = "70px", inline = TRUE),
            tags$span("Use API to query repositories such as GitHub, Zenodo, and the OSF"),
            switchInput("create_use_llm", "LLM use", FALSE,
              onLabel = "TRUE", offLabel = "FALSE",
              onStatus = "success", offStatus = "default",
              size = "small", labelWidth = "70px", inline = TRUE),
            tags$span(
              "Enable GDPR compliant LLM use through local ",
              tags$a("Ollama",
                href   = "https://www.scienceverse.org/metacheck/articles/ollama.html",
                target = "_blank")
            )
          ),
          textInput(
            "create_local_path",
            "Local archive or folder (optional)",
            value = "",
            placeholder = "Full path to a local folder or archive, e.g. C:/Users/me/study_code.zip",
            width = "100%"
          )
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
        uiOutput("create_report_status_ui")
      )
    )
  )
)
