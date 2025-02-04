#' Create a paper object
#'
#' Create a new paper object or load a paper from PDF or XML
#'
#' @param name The name of the study or a file path to a PDF or grobid XML
#' @param ... further arguments to add
#'
#' @return A study object with class scivrs_paper
#' @export
#' @examples
#'
#' p <- paper("Demo Paper")
paper <- function(name = "Demo Paper", ...) {
  is_xml <- isTRUE(grepl("\\.xml$", name, ignore.case = TRUE))
  is_pdf <- isTRUE(grepl("\\.pdf$", name, ignore.case = TRUE))

  if (is_xml & file.exists(name)) {
    paper <- read_grobid(name)
  } else if (is_pdf & file.exists(name)) {
    xml <- pdf2grobid(name, ...)
    paper <- read_grobid(xml)
  } else {
    # make empty paper object
    paper <- c(
      list(name = name),
      list(
        info = list(),
        authors = list(),
        full_text = data.frame(),
        references = data.frame(),
        citations = data.frame()
      )
    )

    class(paper) <- c("scivrs_paper", "list")
    class(paper$authors) <- c("scivrs_authors", "list")
  }

  invisible(paper)
}

