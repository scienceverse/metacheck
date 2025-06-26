#' Convert a PDF to Grobid XML
#'
#' This function uses a public grobid server maintained by Patrice Lopez. You can set up your own local grobid server following instructions from <https://grobid.readthedocs.io/> and set the argument `grobid_url` to its path (probably <http://localhost:8070>)
#'
#' Consolidation of citations, headers, and funders looks up these items in CrossRef or another database to fix or enhance information (see <https://grobid.readthedocs.io/en/latest/Consolidation/>). This can slow down conversion. Consolidating headers is only useful for published papers, and can be set to 0 for work in prep.
#'
#' @param filename path to the PDF
#' @param save_path directory or file path to save to; set to NULL to save to a temp file
#' @param grobid_url the URL to the grobid server
#' @param start the first page of the PDF to read (defaults to -1 to read all pages)
#' @param end the last page of the PDF to read (defaults to -1 to read all pages)
#' @param consolidateCitations whether to fix/enhance citations
#' @param consolidateHeader whether to fix/enhance paper info
#' @param consolidateFunders whether to fix/enhance funder info
#'
#' @return XML object
#' @export
#'
pdf2grobid <- function(filename, save_path = ".",
                       grobid_url = "https://kermitt2-grobid.hf.space",
                       start = -1,
                       end = -1,
                       consolidateCitations = 0,
                       consolidateHeader = 0,
                       consolidateFunders = 0) {
  # "http://localhost:8070"
  # "https://grobid.work.abed.cloud"
  site_down(grobid_url, "The grobid server %s is not available")

  # handle list of files or a directory----
  if (length(filename) > 1) {
    if (is.null(save_path) || !dir.exists(save_path)) {
      warning(save_path, " is not a directory, so the PDFs will be saved in the working directory: ", getwd())
      save_path = "."
    }

    # set up progress bar ----
    if (verbose()) {
      pb <- progress::progress_bar$new(
        total = length(filename), clear = FALSE,
        format = "Processing PDFs [:bar] :current/:total :elapsedfull"
      )
      pb$tick(0)
      Sys.sleep(0.2)
      pb$tick(0)
    }

    xmls <- lapply(filename, \(pdf) {
      args <- list(
        filename = pdf,
        save_path = save_path,
        grobid_url = grobid_url,
        start = start,
        end = end,
        consolidateCitations = consolidateCitations,
        consolidateHeader = consolidateHeader,
        consolidateFunders = consolidateFunders
      )
      xml <- tryCatch(do.call(pdf2grobid, args),
                      error = function(e) { return(FALSE) })
      if (verbose()) pb$tick()
      xml
    })

    errors <- sapply(xmls, isFALSE)
    if (any(errors)) {
      warning(sum(errors), " of ", length(xmls), " files did not convert: ",
              paste(filename[errors], collapse = ", "))
    }

    return(xmls)
  } else if (dir.exists(filename)) {
    pdfs <- list.files(filename, "\\.pdf",
                       full.names = TRUE,
                       recursive = TRUE)
    if (length(pdfs) == 0) {
      warning("There are no PDF files in the directory ", filename)
    }
    xmls <- pdf2grobid(pdfs, save_path, grobid_url)
    return(xmls)
  }

  if (!file.exists(filename)) {
    stop("The file ", filename, " does not exist.")
  }

  file <- httr::upload_file(filename)
  post_url <- paste0(grobid_url, "/api/processFulltextDocument")
  args <- list(input = file,
               start = start,
               end = end,
               consolidateCitations = consolidateCitations,
               consolidateHeader = consolidateHeader,
               consolidateFunders = consolidateFunders,
               includeRawCitations = 1)
  resp <- httr::POST(post_url, body = args, encode = "multipart")

  # Check if the request was successful
  status <- httr::http_status(resp)
  if (status$category != "Success") {
    stop(status$reason)
  }

  # save to save_path
  if (is.null(save_path)) {
    save_file <- tempfile(fileext = ".xml")
  } else if (dir.exists(save_path)) {
    base <- basename(filename) |>
      sub("\\.pdf", "", x = _, TRUE) |>
      paste0(".xml")
    save_file <- file.path(save_path, base)
  } else {
    save_file <- save_path |>
      sub("\\.xml", "", x = _, TRUE) |>
      paste0(".xml")
  }

  # Save the response content
  content <- httr::content(resp, as = "raw")
  writeBin(content, save_file)

  # read in as xml
  if (is.null(save_path)) {
    xml <- read_grobid_xml(save_file)
    return(xml)
  } else {
    save_file
  }
}

