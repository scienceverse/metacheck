### load_tab ----
load_tab <- tabItem(
  tabName = "load_tab",
  p("This app is under development; all materials created should be carefully checked."),
  fileInput("load_xml", "Load from XML", multiple = TRUE, width = "100%", accept = ".xml"),
  textOutput("n_papers_loaded"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Paper Info",
      selectInput("paper_name", "Paper Name", c()),
      uiOutput("paper_title"),
      uiOutput("paper_desc"),
      textOutput("paper_keywords")
      # textInput("paper_title", "paper Title", "", "100%"),
      # textAreaInput("paper_desc", "paper Description", "", "100%"),
      # textInput("paper_keywords", "Keywords (separate with semicolons)", "", "100%", )
  )
)

