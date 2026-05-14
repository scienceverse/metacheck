### load_tab ----
load_tab <- tabItem(
  tabName = "load_tab",
  HTML("<small>Use Convert to read in a single PDF. This uses a local grobid server if you have one set up on <a href='(http://localhost:8070'>localhost:8070</a>, or an <a href='https://www.scienceverse.org/metacheck/convert.json'>external server</a> if you don't, and will save files to a temporary directory. If you want to process more than one PDF at a time, or want more control over the conversion, use the `convert()` function in the R package to create and save JSON files.</small>"),
  fluidRow(
    column(width = 6,
           fileInput("load_paper", "Load JSON",
                     multiple = TRUE,
                     width = "100%",
                     accept = ".json")),
    column(width = 6,
           fileInput("convert_paper", "Convert PDF",
              multiple = FALSE,
              width = "100%",
              accept = ".pdf"))
  ),
  textOutput("n_papers_loaded"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Paper Info",
      selectInput("paper_name", "Paper Name", c()),
      uiOutput("paper_title"),
      uiOutput("paper_desc"),
      textOutput("paper_keywords")
  )
)

