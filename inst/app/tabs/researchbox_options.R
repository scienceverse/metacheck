### researchbox_options_tab ----
researchbox_options_tab <- tabItem(
  tabName = "researchbox_options_tab",
  h2("Options"),

  tags$p(
    "These settings control how much is downloaded from the ResearchBox, ",
    "and let you add files from a local folder alongside it."
  ),

  box(title = NULL,
      collapsible = FALSE,
      width = 12,

    numericInput(
      "max_file_size", "Largest single file to download (MB)",
      value = 100, min = 1
    ),
    numericInput(
      "max_download_size", "Largest total download per box (MB)",
      value = 500, min = 1
    ),
    checkboxInput(
      "cache_downloads",
      "Keep downloaded files in a persistent cache for later re-runs",
      value = FALSE
    ),

    textInput(
      "local_path",
      "Optionally add files from a local folder alongside the ResearchBox (e.g. files not stored on ResearchBox)",
      value = "",
      placeholder = "Full path to a local folder or archive, e.g. C:/Users/me/study_code.zip",
      width = "100%"
    ),

    tags$br(),
    actionButton(
      "options_done", "Done — back to Check ResearchBox",
      icon = icon("arrow-left"),
      class = "btn-options-done"
    )
  )
)
