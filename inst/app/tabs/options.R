### options_tab ----
# Settings for the Create Report tab. Users can open this tab to change what
# is sent to / retrieved from external servers, or skip it and use the
# defaults (CrossRef / PubPeer / Repos on; LLM off).
options_tab <- tabItem(
  tabName = "options_tab",
  h2("Options"),
  tags$p(
    "These settings control what metacheck sends to, or retrieves from, ",
    "external servers when generating a report. You can leave them at ",
    "their defaults, or adjust them here before uploading a PDF."
  ),

  box(title = NULL,
      collapsible = FALSE,
      width = 12,
    radioButtons(
      "grobid_server_choice",
      "PDF Conversion Server:",
      choiceNames = list(
        "Use GDPR compliant GROBID 0.8.2 server at Eindhoven University of Technology",
        "Use Full GROBID 0.9.0 via HuggingFace (USA)",
        tagList(
          "Use local GROBID server via Docker — see ",
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
      class = "report-checks",
      checkboxInput("query_crossref", "Query CrossRef", value = TRUE),
      tags$span("Send full references to Crossref API"),
      checkboxInput("query_pubpeer", "Query PubPeer", value = TRUE),
      tags$span("Send reference DOIs to PubPeer API"),
      checkboxInput("query_repos", "Query Data Repositories", value = TRUE),
      tags$span("Use API to query repositories such as GitHub, Zenodo, and the OSF")
    ),

    textInput(
      "local_path",
      "If you want to check a local folder that contains all files you plan to upload to a data repository, or if you unchecked the option to retrieve information from online data repositories, you can still run the repo_check and code_check modules by manually downloading the repositories and speciying the local folder.",
      value = "",
      placeholder = "Full path to a local folder or archive, e.g. C:/Users/me/study_code.zip",
      width = "100%"
    ),

    tags$div(
      class = "llm-model-chooser",
      selectInput("llm_model_choice", "LLM Model",
                  choices =  {
                    x <- llm_model_list()
                    model_ids <- c()
                    if (nrow(x) > 0) { # order by ollama, other platforms, and size (ollama only)
                      x$size <- x$size %||% NA
                      x <- dplyr::arrange(x, platform != "ollama", platform, dplyr::desc(size))
                      model_ids <- paste0(x$platform, "/", x$id)
                    }
                    c("none", model_ids)
                  }),
      tags$span(
        "Enable GDPR compliant LLM use through local ",
        tags$a("Ollama",
               href   = "https://www.scienceverse.org/metacheck/articles/ollama.html",
               target = "_blank")
      )
    ),

    tags$br(),
    actionButton(
      "options_done", "Done — back to Create Report",
      icon = icon("arrow-left"),
      class = "btn-options-done"
    )
  )

)
