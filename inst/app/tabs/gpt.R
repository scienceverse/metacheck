### gpt_tab ----
gpt_tab <- tabItem(
  tabName = "gpt_tab",

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "ChatGPT",
      textInput("gpt_query", "Query", "Summarise this text", "100%"),
      textInput("gpt_context", "Context", "Answer briefly, for a scientific audience", "100%"),
      textInput("gpt_api", "ChatGPT API Key", Sys.getenv("CHATGPT_KEY"), "100%"),
      fluidRow(
        column(4, checkboxGroupInput("gpt_group_by", "Group By", c(), inline = TRUE)),
        valueBoxOutput("total_cost"),
        column(4, div(actionButton("gpt_submit", "Search"),
                      numericInput("gpt_max_calls", "Maximum allowed calls", getOption("papercheck.gpt_max_calls"), 1, NA, 1)))
      )
  ),
  downloadButton("download_gpt", "Download Answers"),
  dataTableOutput("gpt_table")
)
