### text search block (composed into the single main tab) ----
text_search_block <- tagList(
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Search Text",
      fluidRow(
        column(width = 12, textAreaInput("search_pattern", "Pattern", "*", "100%"))
      ),
      fluidRow(
        column(width = 4, selectInput("search_section", "Section", c("all"))),
        column(width = 4, selectInput("search_return", "Return", c("sentence", "paragraph", "section", "header", "match", "paper_id"))),
        column(width = 4, div(
          checkboxGroupInput("search_options", NULL,
                             c("Search this table" = "table",
                               "Fixed" = "fixed",
                               "Ignore Case" = "ignore.case",
                               "PERL regex" = "perl"),
                             selected = "ignore.case")
        ))
      ),
      actionButton("text_search", "Search"),
      actionButton("search_reset", "Reset"),
      actionButton("search_preset_p", "p-values"),
      actionButton("search_preset_n", "sample size")
  ),
  downloadButton("download_table", "Download Table"),
  DT::DTOutput("text_table")
)

