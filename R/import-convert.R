#' Convert documents
#'
#' Uses grobid or bibr to convert a file to paper format.
#'
#' Both bibr and grobid can handle PDF files. Only bibr can convert doc or docx files. Already-converted grobid XML files can be converted to bibr format (set crossref_lookup=TRUE to add a bib_match table). If the file_path is a directory, the method will be xml if any XML files are present, and bibr if only doc or docx files are present.
#'
#' Unless \code{api_url}, \code{api_key} or \code{backend} is given, the servers are tried in this order: metacheck's API on SURF Research Cloud (see \code{\link{convert_bibr}}; it uses its bibr, or its own grobid when its bibr is not ready, and converts PDF files, not doc or docx files), the grobid server of TU/e, a grobid on this computer (\code{http://localhost:8070}), then the other servers in the online list at \url{https://www.scienceverse.org/metacheck/convert.json} (public grobid servers on Hugging Face). This is the same for the methods "auto", "bibr" and "grobid"; a server of the wrong kind for the method is skipped. While bibr does not work, no bibr server on this computer or in the list is looked for. A server, key or backend that is given is used as given.
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_path Path to a directory in which to save the JSON file; there is no default, because CRAN does not allow writing into the
#'   working directory unless you ask for it
#' @param method whether to use bibr, grobid, or xml (grobid_to_bibr) to convert a file (see Details)
#' @param crossref_lookup whether to add the bib_match table from crossref
#' @param keep_xml if the method is grobid, whether to keep intermediate XML files
#' @param ... further arguments to pass to convert_bibr, convert_grobid, or grobid_to_bibr
#'
#' @returns the path to the JSON file
#' @export
#' @examples
#' \donttest{
#' if (online("grobid.hti.ieis.tue.nl")) {
#'   json <- convert(demofile("pdf"), tempdir(), method = "grobid")
#'   paper <- read(json)
#' }
#' }
convert <- function(file_path,
                    save_path,
                    method = c("auto", "bibr", "grobid", "xml"),
                    crossref_lookup = FALSE,
                    keep_xml = TRUE,
                    ...) {
  args <- list(...)

  # check file types (xml/pdf/doc/docx) ----
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
  # before any file is sent to a server: there is no default folder
  force(save_path)

  # choose a server ----
  # Always in this order: metacheck's API on SURF Research Cloud (its bibr, or
  # its own GROBID when its bibr is not ready), the TU/e GROBID server, a GROBID
  # (then a bibr) on this computer, then the other servers of the online list
  # (see .convert_candidates()). A server, key or backend given in `...` is
  # used as given.
  method <- match.arg(method)
  given <- !is.null(args$api_url) || !is.null(args$api_key) || !is.null(args$backend)

  # SURF converts PDFs, not DOC or DOCX files; it returns the paper JSON
  # whichever method was asked for
  surf <- method != "xml" && !docs && !given && .surf_isalive(error = FALSE)
  if (surf) {
    message("Using the SURF API")
    method <- "bibr"
    args$backend <- "surf"
  }

  # a server given with method "auto": GROBID when it answers as one, else bibr
  if (given && method == "auto") {
    method <- if (!is.null(args$api_url) &&
                  .grobid_isalive(args$api_url, error = FALSE)) "grobid" else "bibr"
  }

  # XML is converted locally, so no server is looked up for it (offline, the
  # lookup stopped an XML conversion, U18)
  if (!surf && !given && method != "xml") {
    for (s in .convert_candidates()) {
      # skip if server doesn't match a set method
      if (!method %in% c(s$service, "auto")) next
      # a bibr server is not asked while bibr does not work (see .bibr_working)
      if (identical(s$service, "bibr") && !.bibr_working) next

      message("Checking ", s$id)
      up <- if (s$service == "grobid") {
        .grobid_isalive(s$url, error = FALSE)
      } else {
        .bibr_isalive(s$url, args$api_key %||% Sys.getenv("SCIVRS_API_KEY"),
                      error = FALSE)
      }

      if (up) {
        message("Using ", s$id)
        method <- s$service
        args$api_url <- s$url
        break
      }
    }
    if (method == "auto" || (method == "grobid" && is.null(args$api_url))) {
      stop("No conversion server answers: not metacheck's server on SURF, ",
           "the TU/e GROBID server, a GROBID on this computer ",
           "(http://localhost:8070), or the servers of the online list.",
           call. = FALSE)
    }
  }

  # set up args ----
  args$file_path <- file_path
  args$save_path <- save_path

  # convert ----
  if (method == "xml") {
    ## xml: convert XML to bibr ----
    bibr_path <- grobid_to_bibr(file_path, save_path, crossref_lookup)
  } else if (method == "grobid") {
    ## grobid: convert PDF to XML ----
    if (keep_xml) {
      args$save_path <- sub("\\.json$", "\\.xml", save_path)
    } else {
      # a temporary folder, in which convert_grobid() names each XML file
      # after its PDF: the paper ID and JSON file name come from the XML file
      # name, and a random tempfile() name gave the paper a random ID
      tmp_dir <- tempfile("grobid")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE))
      args$save_path <- tmp_dir
    }

    grobid_args <- c("file_path", "save_path", "api_url", "start_page", "end_page")
    valid_args <- intersect(names(args), grobid_args)
    tmp_xml <- do.call(convert_grobid, args[valid_args])
    bibr_path <- grobid_to_bibr(tmp_xml, save_path, crossref_lookup)
  } else if (method == "bibr") {
    ## bibr: convert PDF or DOC to bibr ----
    bibr_args <- formals(convert_bibr) |> names()
    valid_args <- intersect(names(args), bibr_args)
    bibr_path <- do.call(convert_bibr, args[valid_args])
  }

  return(bibr_path)
}

# Whether convert() looks for a bibr server on its own (a local bibr at
# localhost:8000, and the bibr servers of the online list). FALSE while bibr
# does not work (November 2026): asking for it only cost time. The SURF API is
# still used, as it converts with its own GROBID when its bibr is not ready
# (see .surf_isalive()), and convert_bibr() still works when called directly.
# Set to TRUE again once bibr works.
.bibr_working <- FALSE

# The conversion servers metacheck tries after the SURF API and a local GROBID
# or bibr, in their order: the TU/e GROBID first, then other public ones
# (pkgdown/assets/convert.json, published with the website). An empty list
# when the list cannot be read. `service` keeps only "grobid" or "bibr" ones.
.convert_servers <- function(service = NULL) {
  servers_url <- "https://www.scienceverse.org/metacheck/convert.json"
  if (!online(servers_url)) return(list())
  servers <- tryCatch(jsonlite::read_json(servers_url), error = function(e) list())
  if (!is.null(service)) {
    servers <- Filter(function(s) identical(s$service, service), servers)
  }
  servers
}

# The TU/e GROBID server, tried right after metacheck's server on SURF
.tue_grobid_url <- "https://grobid.hti.ieis.tue.nl"

# The servers convert() tries after metacheck's server on SURF, in order: the
# TU/e GROBID server, a GROBID and a bibr on this computer, then the other
# servers of the online list (the Hugging Face GROBID servers). The TU/e server
# is tried even when the online list cannot be read.
.convert_candidates <- function() {
  same <- function(a, b) identical(sub("/+$", "", a), sub("/+$", "", b))
  online_list <- .convert_servers()
  tue <- Filter(function(s) same(s$url, .tue_grobid_url), online_list)
  if (!length(tue)) {
    tue <- list(list(id = "TUE grobid", service = "grobid", url = .tue_grobid_url))
  }
  local <- list(
    list(id = "local grobid", service = "grobid", url = "http://localhost:8070"),
    list(id = "local bibr", service = "bibr", url = "http://localhost:8000"))
  rest <- Filter(function(s) !same(s$url, .tue_grobid_url), online_list)
  c(tue[1], local, rest)
}

# The GROBID server convert_grobid() uses when none is given: the first GROBID
# server of .convert_candidates() that answers (TU/e, this computer, Hugging
# Face; metacheck's server on SURF returns paper JSON, not GROBID's XML), else
# `default`, so the caller's own check reports why.
.grobid_first_alive <- function(default) {
  for (s in .convert_candidates()) {
    if (!identical(s$service, "grobid")) next
    message("Checking ", s$id)
    if (.grobid_isalive(s$url, error = FALSE)) {
      message("Using ", s$id)
      return(s$url)
    }
  }
  default
}
