# set up module list

modules <- metacheck::module_list()
sections <- unique(modules$section)
mod_list_sections <- lapply(sections, \(sec) {
  m <- dplyr::filter(modules, section == sec)
  mod <- stats::setNames(m$name, m$title)
  cb_name <- paste0("report_module_list_", sec)
  column(width = floor(12/length(sections)),
         h3(sec),
         checkboxGroupInput(cb_name, "", mod))
})

### report_tab ----
report_tab <- tabItem(
  tabName = "report_tab",
  p("The report will only run on the first paper in a set. You can download the quarto file and render it yourself, or download an html version. We currently can't show the HTML version embedded in the shiny app, but are working on it."),
  actionButton("report_run", "Run Report"),
  downloadButton("report_dl_quarto", "Download Quarto"),
  downloadButton("report_dl_html", "Download HTML"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title="Modules",
      actionButton("report_info", "Info"),
      actionButton("report_defaults", "Defaults"),
      do.call(fluidRow, mod_list_sections),
      h4("Modules and External Resources"),
      tags$small("Most modules only process information internally. RetractionWatch and Replication Check query an internal version of those databases. PubPeer Comments, Repository Check, and Code Check retrieve information about references or linked repositories. The Causal Claims module connects to a machine learning classifier run on HuggingFace. Modules that use LLMs (currently only Power) do so optionally, only if you set llm_use(TRUE) and provide an API key. They only transmit relevant selections of text for processing. The Reference modules use the DOIs from the enhanced bibliography if you converted the PDF with `crossref_lookup = TRUE` or used `add_bib_match()`, otherwise they only check references with DOIs. See further module details on the Modules tab.")
  ),
  uiOutput("report_text")
)
