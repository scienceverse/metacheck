#' Convert documents
#'
#' Uses grobid or bibr to convert a PDF file to paper format
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_path Path to a directory in which to save the JSON file
#' @param method whether to use bibr or grobid to convert a file
#' @param ... further arguments to pass to convert_bibr or convert_grobid
#'
#' @returns the path to the JSON file
#' @export
convert <- function(file_path,
                    save_path = ".",
                    method = c("auto", "bibr", "grobid"),
                    ...) {
  # TODO: replace with logic for checking local versions of grobid or bibr
  method <- match.arg(method)
  if (method == "auto") method <- "grobid"

  # check file types (xml/pdf/doc/docx)
  if (length(file_path) == 1 && dir.exists(file_path)) {
    files <- list.files(file_path)
  } else {
    files <- file_path
  }
  xmls <- grepl("\\.xml$", files, ignore.case = TRUE) |> sum()
  pdfs <- grepl("\\.pdf$", files, ignore.case = TRUE) |> sum()
  docs <- grepl("\\.docx?$", files, ignore.case = TRUE) |> sum()

  if (xmls) {
    method <- "xml" # convert xmls
  } else if (pdfs) {
    # use default above
  } else if (docs) {
    method <- "bibr" # only bibr does docs
  } else {
    stop("No PDF, XML, DOC or DOCX files detected.")
  }

  # set up args
  args <- list(...)
  args$file_path <- file_path
  args$save_path <- save_path
  crossref_lookup <- args$crossref_lookup %||%
    as.logical(args$consolidate_citations) %||% FALSE

  if (method == "xml") {
    # convert XML to bibr
    bib_path <- grobid_to_bibr(file_path, save_path, crossref_lookup)
  } else if (method == "grobid") {
    tmp_xml <- tempfile(fileext = ".xml")
    args$save_path <- tmp_xml

    # convert PDF to XML
    grobid_args <- c("file_path", "save_path", "api_url", "start_page", "end_page")
    valid_args <- intersect(names(args), grobid_args)
    tmp_xml <- do.call(convert_grobid, args[valid_args])

    # convert to bibr
    bib_path <- grobid_to_bibr(tmp_xml, save_path, crossref_lookup)
    unlink(tmp_xml)
  } else if (method == "bibr") {
    # convert PDF or DOC to bibr
    bibr_args <- formals(convert_bibr) |> names()
    valid_args <- intersect(names(args), bibr_args)
    bib_path <- do.call(convert_bibr, args)
  }

  return(bib_path)
}
