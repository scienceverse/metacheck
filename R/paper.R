#' Create a paper object
#'
#' Create a new paper object or load a paper from PDF or XML
#'
#' @param name The name of the study or a file path to a PDF or grobid XML
#' @param ... further arguments to add
#'
#' @return An object with class scivrs_paper
#' @export
#' @keywords internal
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
    paper <- list(
      id = gsub("\\.(pdf|xml)$", "", name, ignore.case = TRUE),
      info = list(),
      authors = list(),
      full_text = data.frame(),
      references = data.frame(),
      citations = data.frame()
    )

    class(paper) <- c("scivrs_paper", "list")
    class(paper$authors) <- c("scivrs_authors", "list")
  }

  invisible(paper)
}

#' Create a paperlist object
#'
#' Create a new paperlist object from individual paper objects or lists of paper objects
#'
#' @param ... scivrs_paper objects or lists of paper objects
#' @param merge_duplicates if duplicates exist, merge them
#'
#' @return An object with class scivrs_paperlist
#' @export
#' @keywords internal
#' @examples
#'
#' p1 <- psychsci[[1]]
#' p2 <- psychsci[[2]]
#' plist <- paperlist(p1, p2)
#'
#' merged <- paperlist(psychsci[1:2], psychsci[2:3])
paperlist <- function(..., merge_duplicates = TRUE) {
  dots <- list(...)

  if (is_paper_list(dots)) {
    paperlist <- dots
  } else {
    is_paper <- sapply(dots, inherits, "scivrs_paper")
    dots[is_paper] <- lapply(dots[is_paper], list)
    is_paperlist <- sapply(dots, is_paper_list)
    if (all(is_paperlist)) {
      paperlist <- do.call(c, dots)
    } else {
      stop("The arguments must be paper objects or lists of paper objects")
    }
  }

  # update names from id
  names(paperlist) <- sapply(paperlist, \(x) x$id)

  if (merge_duplicates) {
    # check for duplicate IDs
    dupes <- names(paperlist) |> duplicated() |> which()
    for (d in rev(dupes)) {
      dupe <- paperlist[names(paperlist) == names(paperlist)[d]]
      if (identical(unname(dupe[-length(dupe)]), unname(dupe[-1]))) {
        paperlist[[d]] <- NULL
      }
    }
  }

  class(paperlist) <- c("scivrs_paperlist", "list")

  invisible(paperlist)
}
