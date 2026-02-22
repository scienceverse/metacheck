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
paper <- function(id = NULL, ...) {
  if (is.null(id)) {
    # make a random hash from the time
    id <- Sys.time() |>
      format("%s%OS6") |>
      charToRaw() |>
      tools::md5sum(bytes = _) |>
      substr(1, 14)
  }

  paper <- list(
    paper_id = id,
    info = list(),
    authors = list(),
    text = data.frame(),
    links = data.frame(),
    tables = data.frame(),
    sections = data.frame(),
    bib = data.frame(),
    xrefs = data.frame(),
    figures = data.frame(),
    equations = data.frame()
  )

  class(paper) <- c("scivrs_paper", "list")

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
  names(paperlist) <- sapply(paperlist, \(x) x$paper_id)

  if (merge_duplicates) {
    # check for duplicate IDs
    dupes <- names(paperlist) |>
      duplicated() |>
      which()
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

#' Test paper
#'
#' Create a paper object with the specified text (mainly for testing/demos).
#'
#' @param text a vector of text to add
#'
#' @returns a paper object
#' @export
#'
#' @examples
#' # to test a paper with a specific URL
#' p <- test_paper("https://osf.io/abcde")
test_paper <- function(text = LETTERS) {
  p <- paper()

  p$text <- data.frame(
    text_id = seq_along(text),
    section_id = 0,
    paragraph_id = 0,
    text = as.character(text)
  )

  p$sections <- data.frame(
    section_id = 0,
    header = "Test",
    parent_section_id = NA,
    section_type = "unknown",
    classification_score = 0
  )

  p$info <- data.frame(
    title = "Test Paper",
    file_hash = p$paper_id,
    input_format = "test"
  )

  return(p)
}
