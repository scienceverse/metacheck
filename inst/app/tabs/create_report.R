### create_report_tab ----
create_report_tab <- tabItem(
  tabName = "create_report_tab",
  fluidRow(
    column(
      width = 8, offset = 2,
      box(
        width = 12,
        h3("Generate a Metacheck Report"),
        p(
          "Select a PDF file below. The report will be generated automatically ",
          "using all validated modules and will open in a new browser tab when ready."
        ),
        p(
          "To run this software without sending any data to an external server, use your own local GROBID server, and leave the Query Crossref, Query PubPeer, and Query Code Repositories options to FALSE. The GDPR compliant GROBID server in The Netherlands will not store any files sent to the server." 
        ),
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
          style = "display: grid; grid-template-columns: max-content 1fr; align-items: center; row-gap: 0.5em; column-gap: 2em; margin-bottom: 0.75em;",
          switchInput("create_crossref", "Query Crossref", FALSE,
            onLabel = "TRUE", offLabel = "FALSE",
            onStatus = "success", offStatus = "default",
            labelWidth = "120px", inline = TRUE),
          tags$span("Send full references to Crossref API"),
          switchInput("create_pubpeer", "Query PubPeer", FALSE,
            onLabel = "TRUE", offLabel = "FALSE",
            onStatus = "success", offStatus = "default",
            labelWidth = "120px", inline = TRUE),
          tags$span("Send reference DOIs to PubPeer API"),
          switchInput("create_repos", "Query Code Repositories", FALSE,
            onLabel = "TRUE", offLabel = "FALSE",
            onStatus = "success", offStatus = "default",
            labelWidth = "120px", inline = TRUE),
          tags$span("Use API to query repositories such as GitHub, Zenodo, and the OSF"),
          switchInput("create_use_llm", "LLM use", FALSE,
            onLabel = "TRUE", offLabel = "FALSE",
            onStatus = "success", offStatus = "default",
            labelWidth = "120px", inline = TRUE),
          tags$span(
            "Enable GDPR compliant LLM use through local ",
            tags$a("Ollama",
              href   = "https://www.scienceverse.org/metacheck/articles/ollama.html",
              target = "_blank")
          )
        ),
        fileInput(
          "create_pdf", NULL,
          multiple = FALSE,
          accept = ".pdf",
          width = "100%",
          buttonLabel = "Browse...",
          placeholder = "No PDF selected"
        ),
        uiOutput("create_report_status_ui")
      )
    )
  )
)
