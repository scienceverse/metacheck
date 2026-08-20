### download_tab ----
# Step 2: the projects ticked in step 1, the settings osf_file_download()
# takes, and the folder to write into.
download_tab <- tabItem(
  tabName = "download_tab",

  h2("Download"),

  box(title = "Selected projects",
      collapsible = TRUE,
      width = 12,
      uiOutput("selection_ui"),
      actionButton("back_to_list", "Back to the list",
                   icon = icon("arrow-left"))
  ),

  box(title = "Where to save",
      collapsible = FALSE,
      width = 12,
      # Running on your own machine, the server IS your computer, so you can
      # pick a folder on it. Hosted, the server is somewhere else: browsing it
      # would show the server's folders and save the files there, so the app
      # zips them and sends them to your browser instead.
      uiOutput("where_to_save_ui")
  ),

  box(title = "Settings",
      collapsible = TRUE,
      width = 12,

      radioButtons(
        "dl_mode", "What to take",
        choiceNames = list(
          "Everything, as fast as the OSF allows (one archive per component)",
          "List the files first, then take what fits the size limits",
          "List the files first, then take whole components as archives"
        ),
        choiceValues = list("all", "select", "zip"),
        selected = "all"
      ),
      uiOutput("mode_note_ui"),

      tags$div(
        class = "report-checks",
        checkboxInput("metadata", "Also save wiki, logs, and project details",
                      value = TRUE),
        tags$span("Adds an _osf_metadata folder, at about four extra requests per project"),
        checkboxInput("unzip", "Unzip archives after downloading", value = TRUE),
        tags$span("Only applies when taking whole components as archives"),
        checkboxInput("ignore_folder_structure",
                      "Put every file in one folder", value = FALSE),
        tags$span("Ignores the folder structure the project uses")
      ),

      numericInput("max_file_size",
                   "Largest single file to take (MB, blank for no limit)",
                   value = NA, min = 0, step = 1),
      numericInput("max_download_size",
                   "Most to download in total (MB, blank for no limit)",
                   value = NA, min = 0, step = 1),

      tags$br(),
      actionButton("start_download", "Download",
                   icon = icon("download"),
                   class = "btn-options-done"),
      uiOutput("download_status_ui")
  ),

  box(title = "What was downloaded",
      collapsible = TRUE,
      width = 12,
      DT::DTOutput("result_table")
  ),

  box(title = "Doing this in R",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      tags$p(
        "Running metacheck yourself gets you everything, including files kept ",
        "on a linked GitHub or Dropbox account, which are not part of the OSF ",
        "archive. It also lets you archive what you download on Zenodo. See ",
        tags$a("Archiving an OSF Account on Zenodo",
               href = paste0("https://www.scienceverse.org/metacheck_book/",
                             "chapters/archiving-osf-to-zenodo.html"),
               target = "_blank"),
        " in the Metacheck Manual."
      )
  )
)
