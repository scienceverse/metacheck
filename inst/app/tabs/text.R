### text_tab ----
text_tab <- tabItem(
  tabName = "text_tab",

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Search",
      fluidRow(
        column(width = 12, textAreaInput("search_pattern", "Pattern", "*", "100%"))
      ),
      fluidRow(
        column(width = 4, selectInput("search_section", "Section", c("all"))),
        column(width = 4, selectInput("search_return", "Return", c("sentence", "paragraph", "div", "section", "match", "id"))),
        column(width = 4, div(
          checkboxGroupInput("search_options", NULL,
                             c("Search this table" = "table",
                               "Fixed" = "fixed",
                               "Ignore Case" = "ignore.case",
                               "PERL regex" = "perl"),
                             selected = "ignore.case")
        ))
      ),
      actionButton("search_text", "Search"),
      actionButton("search_reset", "Reset"),
      actionButton("search_preset_p", "p-values"),
      actionButton("search_preset_n", "sample size")
  ),
  downloadButton("download_table", "Download Table"),
  dataTableOutput("text_table")
)

