### advanced_tab ----

# Build module checkbox sections (same logic as old report.R)
adv_modules_info <- metacheck::module_list()
adv_sections     <- unique(adv_modules_info$section)
adv_mod_sections <- lapply(adv_sections, function(sec) {
  m   <- dplyr::filter(adv_modules_info, section == sec)
  mod <- stats::setNames(m$name, m$title)
  cb  <- paste0("report_module_list_", sec)
  column(width = floor(12 / length(adv_sections)),
         h3(sec),
         checkboxGroupInput(cb, "", mod))
})

advanced_tab <- tabItem(
  tabName = "advanced_tab",

  tags$div(
    class = "callout callout-warning",
    style = "margin-bottom: 1em;",
    tags$p(
      tags$strong("Warning: "),
      "This tab provides access to all Metacheck modules, including those not validated. ",
      "Furthermore, modules may be placeholders, or have stopped working. ",
      "Modules might send data to non-GDPR compliant servers. ",
      "Check what these modules do in the help files before running them."
    )
  ),

  # ---- Load Files ----
  box(
    width = 12, title = "Load Files",
    HTML(paste0(
      "<small>Use <em>Convert PDF</em> to read in a single PDF. ",
      "Use <em>Load JSON</em> for files already converted with ",
      "<code>convert()</code>.</small>"
    )),
    tags$br(),
    fluidRow(
      column(width = 6,
             fileInput("load_paper", "Load JSON",
                       multiple = TRUE, width = "100%", accept = ".json")),
      column(width = 6,
             fileInput("convert_paper", "Convert PDF",
                       multiple = FALSE, width = "100%", accept = ".pdf"))
    ),
    box(
      width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Paper Info",
      selectInput("paper_name", "Paper", c()),
      uiOutput("paper_title"),
      uiOutput("paper_desc"),
      textOutput("paper_keywords")
    )
  ),

  # ---- Modules & Report ----
  box(
    width = 12, title = "Select Modules & Create Report",
    fluidRow(
      column(width = 12,
             actionButton("report_run", "Create Report",
                          icon = icon("play"), class = "btn-primary btn-lg"),
             actionButton("report_view", "View Report", icon = icon("eye")),
             downloadButton("report_dl_quarto", "Download Quarto"),
             downloadButton("report_dl_html",   "Download HTML"),
             tags$span(style = "margin-left: 1.5em;"),
             actionButton("report_info",     "Info"),
             actionButton("report_defaults", "Defaults"))
    ),
    tags$br(),
    do.call(fluidRow, adv_mod_sections),
    tags$br(),
    tags$small(
      "Most modules only process information internally. ",
      "RetractionWatch and Replication Check query an internal version of those databases. ",
      "PubPeer Comments, Repository Check, and Code Check retrieve information about ",
      "references or linked repositories. Modules that use LLMs (currently only Power) ",
      "do so optionally, only if you set llm_use(TRUE) and provide an API key."
    )
  ),

  # ---- Module Explorer ----
  box(
    width = 12, title = "Module Explorer",
    collapsible = TRUE, collapsed = TRUE,
    fluidRow(
      column(width = 6,
             selectInput("module_list", NULL,
                         stats::setNames(adv_modules_info$name,
                                         adv_modules_info$title))),
      column(width = 6,
             actionButton("run_module", "Run Module"),
             downloadButton("download_mod_table", "Download Table"))
    ),
    textOutput("mod_desc"),
    textOutput("mod_title", container = tags$h2),
    uiOutput("mod_summary"),
    dataTableOutput("mod_table"),
    box(width = 12, collapsible = TRUE, collapsed = TRUE,
        title = "Module Details",
        uiOutput("mod_details"))
  )
)
