#' Get paper from text
#'
#' @param txt The text to read into a paper object, or a path to a text file
#' @param id The ID to use for the paper (e.g., a DOI) defaults to the filename if txt is a path
#'
#' @returns A paper object with class scivrs_paper
#' @export
#'
#' @examples
#' paper <- read_text("This is my paper. That's it!")
read_text <- function(txt, id = "") {
  ## check if txt is a text file ----
  if (file.exists(txt)) {
    filename <- txt
    if (id == "") id <- basename(filename)

    if (grepl("\\.docx$", filename, ignore.case = TRUE)) {
      # is a word file
      # TODO: use style_name to parse better
      docx <- officer::read_docx(filename)
      doc_summary <- officer::docx_summary(docx, preserve = TRUE)
      parsed_text <- doc_summary$text
    } else if (grepl("\\.txt$", filename, ignore.case = TRUE)) {
      parsed_text <- readLines(filename)
    } else {
      # TODO: better version of this?
      # doc <- readtext::readtext(filename)
      # parsed_text <- strsplit(doc$text, "\\n+")[[1]]
    }
  } else {
    parsed_text <- strsplit(txt, "\\n+")[[1]]
  }

  # get rid of blank lines
  parsed_text <- parsed_text[trimws(parsed_text) != ""]

  ## split text into paragraphs ----
  alltext <- data.frame(
    header = NA,
    text = parsed_text,
    div = NA,
    p = NA
  )

  ## identify probably headers ----
  headers <- grep("^.{1,20}$", alltext$text)
  alltext$header[headers] <- alltext$text[headers]
  alltext$div[headers] <- seq_along(headers)
  header <- div <- NULL # fix devtools check warning
  alltext <- tidyr::fill(alltext, header, div) |>
    dplyr::group_by(div) |>
    dplyr::mutate(p = dplyr::row_number() - 1) |>
    dplyr::ungroup()

  ## tokenize sentences ----
  # TODO: get tidytext to stop breaking sentences at "S.E. ="
  text <- NULL # hack to stop cmdcheck warning :(
  if (nrow(alltext) > 0) {
    ft <- alltext |>
      tidytext::unnest_sentences(text, text, to_lower = FALSE) |>
      dplyr::mutate(s = dplyr::row_number(), .by = c("div", "p"))
    ft$id <- id
  } else {
    ft <- data.frame(
      header = character(0),
      text = character(0),
      div = double(0),
      p = double(0),
      s = double(0),
      id = character(0)
    )
  }

  # classify headers ----
  abstract <- grepl("abstract", ft$header, ignore.case = TRUE)
  intro <- grepl("intro", ft$header, ignore.case = TRUE)
  method <- grepl("method", ft$header, ignore.case = TRUE)
  results <- grepl("result", ft$header, ignore.case = TRUE)
  discussion <- grepl("discuss", ft$header, ignore.case = TRUE)
  ft$section <- rep(NA_character_, nrow(ft))
  ft$section[abstract] <- "abstract"
  ft$section[intro] <- "intro"
  ft$section[method] <- "method"
  ft$section[discussion] <- "discussion"
  ft$section[results] <- "results"

  colorder <- c("text", "section", "header", "div", "p", "s", "id")

  blank_divs <- grepl("^\\s*$", ft$text)

  body_table <- ft[!blank_divs, colorder]
  rownames(body_table) <- NULL

  # set up paper object ----
  p <- paper()

  # make an empty xml object so functions below return empty data frames
  xml<- xml2::xml_new_document() |>
    xml2::xml_add_child("root")

  p$name <- id
  p$full_text <- body_table

  #p$info$filename <- filename
  p$info$title <- ""
  p$info$description <- ""

  # keywords ----
  p$info$keywords <- xml2::xml_find_all(xml, "//keywords //term") |>
    xml2::xml_text()

  # get authors ----
  p$authors <- get_authors(xml)

  # references ----
  refs <- get_refs(xml)
  p$references <- refs$references
  p$citations <- refs$citations

  # DOI ----
  doi <- get_doi(xml)
  p$info$doi <- doi

  # submission ----
  submission <- get_submission(xml)
  p$info$submission <- submission

  return(p)
}
