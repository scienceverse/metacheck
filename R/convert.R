#' Convert documents
#'
#' Uses grobid or bibr to convert a file to paper format.
#'
#' Both bibr and grobid can handle PDF files. Only bibr can convert doc or docx files. Already-converted grobid XML files can be converted to bibr format (set crossref_lookup=TRUE to add a bib_match table). If the file_path is a directory, the method will be xml if any XML files are present, and bibr if only doc or docx files are present.
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_path Path to a directory in which to save the JSON file
#' @param method whether to use bibr, grobid, or grobid_to_bibr to convert a file (see Details)
#' @param ... further arguments to pass to convert_bibr, convert_grobid, or grobid_to_bibr
#'
#' @returns the path to the JSON file
#' @export
convert <- function(file_path,
                    save_path = ".",
                    method = c("auto", "bibr", "grobid", "xml"),
                    ...) {
  args <- list(...)

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
    # use default method
  } else if (docs) {
    method <- "bibr" # only bibr does docs
  } else {
    stop("No PDF, XML, DOC or DOCX files detected.")
  }

  # auto-detect method (local grobid > local bibr > online priority list)
  method <- match.arg(method)
  if (method == "auto") {
    grobid_local_url <- "http://localhost:8070"
    bibr_local_url <- "http://localhost:8000"

    grobid_up <- .grobid_isalive(grobid_local_url, error = FALSE)
    bibr_up <- .bibr_isalive(bibr_local_url, NULL, error = FALSE)

    if (grobid_up) { # check for local grobid
      message("Using local grobid")
      method <- "grobid"
      args$api_url <- grobid_local_url
    } else if (bibr_up) {# check for local bibr
      message("Using local bibr")
      method <- "bibr"
      args$api_url <- bibr_local_url
    }
  }

  # check server priority list
  if (method == "auto" || is.null(args$api_url)) {
    servers_url <- "https://www.scienceverse.org/metacheck/convert.json"

    if (!online(servers_url)) {
      stop("No local grobid or bibr detected, online versions not available")
    }

    servers <- jsonlite::read_json(servers_url)

    for (s in servers) {
      # skip if server doesn't match a set method
      if (!method %in% c(s$service, "auto")) next

      message("Checking ", s$id)
      if (s$service == "grobid") {
        up <- .grobid_isalive(s$url, error = FALSE)
      } else if (s$service == "bibr") {
        api_key <- args$api_key %||% Sys.getenv("SCIVRS_API_KEY")
        up <- .bibr_isalive(s$url, api_key, error = FALSE)
      }

      if (up) {
        message("Using ", s$id)
        method <- s$service
        args$api_url <- s$url
        break
      }
    }
  }



  # set up args
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
