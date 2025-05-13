### llm_tab ----
llm_tab <- tabItem(
  tabName = "llm_tab",

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "LLM",
      textInput("llm_query", "Query", "Summarise this text", "100%"),
      textInput("llm_api", "GROQ API Key", Sys.getenv("GROQ_API_KEY"), "100%"),
      fluidRow(
        column(4, checkboxGroupInput("llm_group_by", "Group By", c(), inline = TRUE)),
        valueBoxOutput("total_cost"),
        column(4, div(actionButton("llm_submit", "Search"),
                      numericInput("llm_max_calls", "Maximum allowed calls", getOption("papercheck.llm_max_calls"), 1, NA, 1)))
      )
  ),
  downloadButton("download_llm", "Download Answers"),
  dataTableOutput("llm_table")
)
