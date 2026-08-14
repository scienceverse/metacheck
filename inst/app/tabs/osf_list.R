### list_tab ----
# Step 1: turn an OSF user or project ID into a list of projects, which the
# user searches and ticks before moving to step 2.
list_tab <- tabItem(
  tabName = "list_tab",

  h2("Find OSF Projects"),
  tags$p(
    "Enter an OSF user ID to list everything they have, or a project ID to ",
    "download just that project. A full OSF web address works too."
  ),

  box(title = "OSF Token",
      collapsible = FALSE,
      width = 12,
      tags$p(
        "A token is only needed for private projects, and it raises the limit ",
        "from 100 requests an hour to 10,000 a day. Create one at ",
        tags$a("osf.io/settings/tokens",
               href = "https://osf.io/settings/tokens",
               target = "_blank"),
        " with the osf.full_read scope. It is used for this session only and ",
        "is not written to disk."
      ),
      passwordInput("osf_pat", "OSF personal access token", width = "100%"),
      actionButton("pat_check", "Check token", icon = icon("key"))
  ),

  box(title = NULL,
      collapsible = FALSE,
      width = 12,
      textInput("osf_id", "OSF user or project ID",
                placeholder = "e.g. 4i578 or https://osf.io/6nt4v",
                width = "100%"),
      actionButton("find_projects", "List projects",
                   icon = icon("magnifying-glass"),
                   class = "btn-options-done"),
      uiOutput("list_status_ui")
  ),

  box(title = "Projects",
      collapsible = TRUE,
      width = 12,
      tags$div(
        class = "report-checks",
        textInput("search_term", "Search titles",
                  placeholder = "type a word to narrow the list",
                  width = "100%"),
        checkboxInput("private_only", "Only show private projects",
                      value = FALSE)
      ),
      tags$div(
        actionButton("select_all", "Select all shown",
                     icon = icon("check-double")),
        actionButton("clear_all", "Clear selection",
                     icon = icon("xmark"))
      ),
      uiOutput("selection_count_ui"),
      DT::DTOutput("project_table"),
      tags$br(),
      actionButton("to_download", "Continue to download",
                   icon = icon("arrow-right"),
                   class = "btn-options-done")
  )
)
