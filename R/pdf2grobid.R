#' Convert a PDF to Grobid XML
#'
#' This function uses a public grobid server maintained by Patrice Lopez. You can set up your own local grobid server following instructions from <https://grobid.readthedocs.io/> and set the argument `grobid_url` to its path (probably <http://localhost:8070>)
#'
#' Consolidation of citations, headers, and funders looks up these items in CrossRef or another database to fix or enhance information (see <https://grobid.readthedocs.io/en/latest/Consolidation/>). This can slow down conversion. Consolidating headers is only useful for published papers, and can be set to 0 for work in prep.
#'
#' @param filename path to the PDF, a vector of paths, or a directory name that contains PDFs
#' @param save_path directory or file path to save to; set to NULL to save to a temp file
#' @param grobid_url the URL to the grobid server
#' @param start the first page of the PDF to read (defaults to -1 to read all pages)
#' @param end the last page of the PDF to read (defaults to -1 to read all pages)
#' @param consolidate_citations whether to fix/enhance citations
#' @param consolidate_header whether to fix/enhance paper info
#' @param consolidate_funders whether to fix/enhance funder info
#'
#' @return XML object
#' @export
#'
pdf2grobid <- function(filename, save_path = ".",
                       grobid_url = "https://kermitt2-grobid.hf.space",
                       # grobid_url = "http://api.metacheck.app",
                       start = -1,
                       end = -1,
                       consolidate_citations = 0,
                       consolidate_header = 0,
                       consolidate_funders = 0) {
  # check if grobid_url is a valid url, before connecting to it
  if (!grepl("^https?://", grobid_url)) {
    stop("grobid_url must be a valid URL, starting with http or https!")
  }

  if (grobid_url != "http://api.metacheck.app") {
    # test if the server is up using the isalive endpoint, instead of sitedown
    service_status_url <- httr::modify_url(grobid_url, path = "/api/isalive")

    resp <- tryCatch(
      {
        httr::GET(service_status_url)
      },
      error = function(e) {
        stop(
          "Connection to the GROBID server failed!",
          "Please check your connection or the URL: ", grobid_url
        )
      }
    )

    status <- httr::status_code(resp)
    if (status != 200) {
      stop("GROBID server does not appear up and running on the provided URL. Status: ", status)
    }
  }

  # handle list of files or a directory----
  if (length(filename) > 1) {
    if (is.null(save_path)) save_path <- "."
    if (length(save_path) == 1) {
      dir.create(save_path, FALSE)
      save_path <- rep_len(save_path, length(filename))
    }
    if (length(save_path) != length(filename)) {
      stop("The argument save_path must be a single directory name or a vector of file names with the same length as the number of files to convert.")
    }

    # set up progress bar ----
    pb <- pb(
      length(filename),
      "Processing PDFs [:bar] :current/:total :elapsedfull"
    )

    xmls <- mapply(\(pdf, sp) {
      args <- list(
        filename = pdf,
        save_path = sp,
        grobid_url = grobid_url,
        start = start,
        end = end,
        consolidate_citations = consolidate_citations,
        consolidate_header = consolidate_header,
        consolidate_funders = consolidate_funders
      )
      xml <- tryCatch(do.call(pdf2grobid, args),
        error = function(e) {
          return(e$message)
        }
      )
      pb$tick()
      xml
    }, pdf = filename, sp = save_path)

    errors <- !file.exists(xmls)
    if (any(errors)) {
      warning(
        sum(errors), " of ", length(xmls), " files did not convert: \n",
        paste0(" * ", filename[errors], ": ", xmls[errors], collapse = "\n")
      )
      xmls[errors] <- NA_character_
    }

    # summary message
    n_success <- sum(!errors)
    n_total <- length(xmls)
    message(sprintf(
      "%d out of %d PDF file%s successfully converted to Grobid TEI XML.",
      n_success, n_total, ifelse(n_total == 1, "", "s")
    ))

    return(invisible(xmls)) # invisible to prioritize formatted print at the end
  } else if (dir.exists(filename)) {
    pdfs <- list.files(filename, "\\.pdf",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    if (length(pdfs) == 0) {
      warning("There are no PDF files in the directory ", filename)
    }
    xmls <- pdf2grobid(pdfs, save_path, grobid_url)
    return(invisible(xmls))
  }

  if (!file.exists(filename)) {
    stop("The file ", filename, " does not exist.")
  }

  if (grobid_url == "http://api.metacheck.app") {
    pyta <- pytacheck(filename)
    content <- pyta$grobid_xml
    # TODO: integrate other info into this
  } else {
    # grobid server
    file <- httr::upload_file(filename)
    post_url <- httr::modify_url(grobid_url, path = "/api/processFulltextDocument")
    args <- list(
      input = file,
      start = start,
      end = end,
      consolidateCitations = consolidate_citations,
      consolidateHeader = consolidate_header,
      consolidateFunders = consolidate_funders,
      includeRawCitations = 1
    )
    resp <- httr::POST(post_url, body = args, encode = "multipart")

    # Check if the request was successful
    status <- httr::http_status(resp)
    if (status$category != "Success") {
      stop(status$reason)
    }

    content <- httr::content(resp, as = "raw")
  }

  # save to save_path
  if (is.null(save_path)) {
    save_file <- tempfile(fileext = ".xml")
  } else if (dir.exists(save_path)) { # save_path is an existing dir
    base <- basename(filename) |>
      sub("\\.pdf", "", x = _, TRUE) |>
      paste0(".xml")
    save_file <- file.path(save_path, base)
  } else { # save_path is a file name
    # make subdirs if necessary
    dir.create(dirname(save_path),
      showWarnings = FALSE,
      recursive = TRUE
    )

    save_file <- save_path |>
      sub("\\.xml", "", x = _, TRUE) |>
      paste0(".xml")
  }

  # Save the response content
  writeBin(content, save_file)

  # read in as xml
  if (is.null(save_path)) {
    xml <- read_xml(save_file)
    return(xml)
  } else {
    save_file
  }
}

#' Process a paper using the Pytacheck API
#'
#' @param file_path Path to the PDF file
#' @param api_url Base URL of the API
#' @param api_key Key to access pytacheck
#'
#' @return A list of parsed information
#' @export
#' @keywords internal
pytacheck <- function(file_path,
                      api_url = "https://api.metacheck.app/get-paper-metadata/",
                      api_key = Sys.getenv("PYTACHECK_API")) {
  message("Requesting ...\n")

  # Make the POST request
  response <- httr::POST(
    api_url,
    httr::add_headers(`X-API-Key` = api_key),
    body = list(
      file = httr::upload_file(file_path),
      replace_metadata = "true"
      # Optional: extract only specific pages (0-indexed)
      # start_page = 0,
      # end_page = 5
    ),
    encode = "multipart"
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # The response content is an Arrow stream
    arrow_stream <- httr::content(response, as = "raw")

    # Write to a temporary file to ensure stable reading
    tmp_file <- tempfile(fileext = ".arrow")
    writeBin(arrow_stream, tmp_file)

    data <- list()

    # Read the Arrow stream (Version 3.0 with 8 tables)
    message("Reading main tables...\n")
    tryCatch(
      {
        # Use Arrow's ReadableFile for more robust sequential reading
        stream <- arrow::ReadableFile$create(tmp_file)
        on.exit(stream$close())

        tables <- c(
          "sentences", "links", "tables",
          "sections", "authors", "references",
          "citations", "info"
        )

        for (table in tables) {
          data[[table]] <- as.data.frame(arrow::read_ipc_stream(stream))
        }

        # Read any remaining tables (dynamic tables)
        data$extra <- list()
        message("Reading dynamic tables...\n")

        while (TRUE) {
          # Try to read next table
          # We need a way to check for EOF cleanly or catch error
          # arrow::read_ipc_stream throws error on EOF usually or returns empty?
          # A safer way with ReadableFile is to try/catch
          skip <- FALSE
          tryCatch(
            {
              tbl <- arrow::read_ipc_stream(stream)
              if (is.null(tbl)) {
                break
              }
              data$extra[[length(data$extra) + 1]] <- as.data.frame(tbl)
              message(sprintf(
                "  Read dynamic table %d (Rows: %d)\n",
                length(data$extra), nrow(tbl)
              ))
            },
            error = function(e) {
              # Assume EOF or end of stream if error matches specific message, otherwise print
              # But for now, we just break on error assuming EOS
              skip <<- TRUE
            }
          )
          if (skip) break
        }

        message("Successfully retrieved and parsed metadata.\n")
      },
      error = function(e) {
        message("Error parsing Arrow stream:", e$message, "\n")
      },
      finally = {
        if (file.exists(tmp_file)) unlink(tmp_file)
      }
    )
  } else {
    stop(
      "Request failed with status code: ",
      httr::status_code(response), "\n",
      httr::content(response, as = "text")
    )
  }

  data
}
