### researchbox_tab ----
researchbox_tab <- tabItem(
  tabName = "researchbox_tab",

  # The report itself, once generated, is shown first -- above the input and
  # options -- so re-running a check does not bury the previous result. Both
  # outputs render NULL until a report exists, so this adds no visible space
  # before then.
  uiOutput("report_status_ui"),
  uiOutput("report_content_ui"),

  h2("Check a ResearchBox"),
  tags$p(
    "Paste the URL (or just the numeric ID) of a public ResearchBox below. ",
    "Metacheck will download its files and run the repo_check, code_check, ",
    "and data_check modules, then show a condensed summary below."
  ),

  tags$div(class = "pdf-upload",
    textInput(
      "rbox_url", NULL,
      value = "",
      placeholder = "https://researchbox.org/6018",
      width = "100%"
    ),
    actionButton("run_check", "Run Check", icon = icon("play"))
  ),

  box(title = "Privacy",
      collapsible = TRUE,
      width = 12,
      tags$p("Change the settings in the 'Options' tab to adjust download limits or add local files."),
      tags$ul(
        tags$li("\U0001F310 The ResearchBox archive is downloaded from researchbox.org to list and check its files."),
        tags$li("\U0001F512 No data from the ResearchBox is sent anywhere else.")
      ),
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
  )
)
