#' Read in grobid XML or bibr JSON
#'
#' @param file_path path to a single directory containing XML and/or JSON files, or a vector of XML/JSON paths
#' @param include_images whether to include images in the figures table of the paper object (they make object size larger, only relevant to bibr imports)
#' @param recursive whether to read files in subfolders (files should have unique paper_ids, or errors can occur)
#'
#' @returns a paper or paperlist
#' @export
read <- function(file_path, include_images = FALSE, recursive = FALSE) {
  # handle directory or multiple files ----
  if (length(file_path) == 1 && dir.exists(file_path)) {
    dir_path <- file_path
    file_path <- list.files(dir_path,
                            pattern = "\\.(json|xml)$",
                            recursive = recursive,
                            full.names = TRUE)
  }
  if (length(file_path) == 0) {
    message("No JSON or XML files found.")
    return(paperlist())
  }

  # remove XML where same JSON and XML found
  json <- grep("\\.json$", file_path, value = TRUE)
  xml_dupes <- gsub("\\.json$", "\\.xml", json)
  file_path <- setdiff(file_path, xml_dupes)

  pb <- pb(length(file_path), "Loading :current/:total [:bar] (:what)")
  papers <- lapply(file_path, \(fp) {
    pb$tick(1, list(what = basename(fp)))
    tryCatch({
      if (grepl("\\.json$", fp, ignore.case = TRUE)) {
        .read_bibr(fp, include_images)
      } else if (grepl("\\.xml$", fp, ignore.case = TRUE)) {
        .grobid_to_bibr(fp)
      }
    }, error = \(e) {
      logger("read", e$message)
      return(NULL)
    })
  })
  papers <- paperlist(papers)
  if (length(papers) == 1) papers <- papers[[1]]

  return(papers)
}


#' Read bibr JSON file
#'
#' @param file_path path to the JSON file
#' @param include_images whether to include images in the figures table of the paper object (they make object size larger)
#'
#' @returns a paper object
#' @export
#'
#' @keywords internal
.read_bibr <- function(file_path, include_images = FALSE) {
  # read JSON ----
  data <- jsonlite::read_json(file_path,
                              simplifyVector = TRUE,
                              simplifyDataFrame = TRUE)

  paper <- paper()
  paper$paper_id <- data$paper_id

  # info ----
  info <- data$info
  keywords <- info$keywords
  info$keywords <- NA
  zeros <- sapply(info, length) == 0
  info[zeros] <- NA
  paper$info <- as.data.frame(info)
  paper$info$keywords <- I(list(keywords))
  paper$info$abstract <- NULL # TODO: remove after bibr fixed

  # author ----
  if (!is.null(data$author) && length(data$author) > 0) {
    paper$author <- as.data.frame(data$author)
  }

  # bib ----
  if (!is.null(data$bib) && length(data$bib) > 0) {
    paper$bib <- as.data.frame(data$bib)
    # paper$bib$authors <- .coerce_bib_authors(paper$bib$authors)
    # if ("editors" %in% names(paper$bib)) {
    #   paper$bib$editors <- .coerce_bib_authors(paper$bib$editors)
    # }
  }

  # eq ----
  if (!is.null(data$eq) && length(data$eq) > 0) {
    paper$eq <- as.data.frame(data$eq)
  }


  # figure ----
  if (!is.null(data$figure) && length(data$figure) > 0) {
    paper$figure <- as.data.frame(data$figure)
    if (!include_images) {
      paper$figure$image <- NA_character_
    }
    paper$figure$caption <- NULL #tempfix
  }

  # url ----
  if (!is.null(data$url) && length(data$url) > 0) {
    paper$url <- as.data.frame(data$url)
    paper$url$href <- gsub("\\s", "", paper$url$href) # tempfix
    paper$url$href <- gsub("\\.$", "", paper$url$href) # tempfix
  }

  # section ----
  if (!is.null(data$section) && length(data$section) > 0) {
    paper$section <- as.data.frame(data$section)
  }

  # table ----
  if (!is.null(data$table) && length(data$table) > 0) {
    paper$table <- as.data.frame(data$table)
    paper$table$caption <- NULL #tempfix
  }

  # text ----
  if (!is.null(data$text) && length(data$text) > 0) {
    paper$text <- as.data.frame(data$text)
  }

  # xref ----
  if (!is.null(data$xref) && length(data$xref) > 0) {
    paper$xref <- as.data.frame(data$xref)
  }

  # bib_match ----
  if (!is.null(data$bib_match) && length(data$bib_match) > 0) {
    paper$bib_match <- as.data.frame(data$bib_match)
    # TODO: check if this is needed
    # if ("authors" %in% names(paper$bib_match)) {
    #   paper$bib_match$authors <- .coerce_bib_authors(paper$bib_match$authors)
    # }
    # if ("editors" %in% names(paper$bib_match)) {
    #   paper$bib_match$editors <- .coerce_bib_authors(paper$bib_match$editors)
    # }
  }

  # ensure all expected columns exist and have correct types
  # (JSON may drop all-NA columns or read them back as logical)
  paper <- .paper_coerce(paper)

  paper
}
