#' Summarise Sections
#'
#' @description
#' Generate a 1-sentence summary for each section
#'
#' @author Lisa DeBruine
#'
#' @param paper a paper object or paperlist object
#' @param ... optional arguments to `llm()`, like model or params
#'
#' @returns a list with table, traffic light, and report text
#'
#' @examples
#' module_run(psychsci[[1]], "llm_summarise")
llm_summarise <- function(paper, ...) {
  sections <- search_text(paper, return = "section")
  query <- "Summarise this section briefly, in one sentence. Do not include any preamble explaining what you are going to do, just give the sentence."

  table <- llm(sections, query, ...)

  list(
    table = table,
    traffic_light = "info",
    report = "A one-sentence summary for each section"
  )
}
