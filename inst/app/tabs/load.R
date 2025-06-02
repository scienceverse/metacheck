### load_tab ----
load_tab <- tabItem(
  tabName = "load_tab",
  p("This app is under development; all materials created should be carefully checked.", class = "warning"),
  p("Use `pdf2grobid()` to create XML files from PDFs. This relies on an external or local grobid server, and will save files to your machine, so needs to run outside this app."),
  fileInput("load_xml", "Load XML",
            multiple = TRUE,
            width = "100%",
            accept = ".xml"),
  textOutput("n_papers_loaded"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Paper Info",
      selectInput("paper_name", "Paper Name", c()),
      uiOutput("paper_title"),
      uiOutput("paper_desc"),
      textOutput("paper_keywords")
  )
)

