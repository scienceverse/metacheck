### advanced module/report UI blocks ----

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

# ---- Modules & Report blocks (composed into the single main tab) ----
advanced_modules_box <-
  # ---- Modules & Report ----
  box(
    width = 12, title = "Select Modules & Create Report",
    fluidRow(
      column(width = 12,
             actionButton("report_run", "Create Report",
                          icon = icon("play"), class = "btn-primary"),
             actionButton("report_view", "View Report", icon = icon("eye")),
             downloadButton("report_dl_quarto", "Download Quarto"),
             downloadButton("report_dl_html",   "Download HTML"),
             tags$span(style = "margin-left: 1.5em;"),
             actionButton("report_info",     "Select info modules"),
             actionButton("report_defaults", "Select recommended modules"))
    ),
    tags$br(),
    do.call(fluidRow, adv_mod_sections)
  )

# ---- Module Explorer ----
advanced_explorer_box <-
  box(
    width = 12, title = "Module Explorer",
    collapsible = TRUE, collapsed = FALSE,
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
    DT::DTOutput("mod_table"),
    box(width = 12, collapsible = TRUE, collapsed = FALSE,
        title = "Module Details",
        uiOutput("mod_details"))
  )

# ---- info box (shown at top of the main tab) ----
advanced_info_box <-
  tags$div(
    class = "advanced-info-box",
    tags$p(
      "This page provides access to all Metacheck modules, including those not ",
      "yet validated and placeholders that may not work. See the ",
      tags$strong("Options"), " tab to enable or disable modules that send ",
      "information to external servers."
    )
  )
