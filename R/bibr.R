#' Convert documents using bibr
#'
#' Converts document files (PDF, DOC, DOCX) to structured JSON using the bibr
#' extraction service. Supports two backends: the Scienceverse platform
#' (\code{"scivrs"}) which uses a job queue with load balancing, and a
#' self-hosted bibr instance (\code{"selfhosted"}) for direct API access.
#'
#' When \code{backend = "auto"} (the default), the \code{"scivrs"} backend is
#' used if \code{api_key} is provided or the \code{SCIVRS_API_KEY} environment
#' variable is set. Otherwise, \code{"selfhosted"} is used (no authentication
#' required).
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_path Path to a directory in which to save the JSON file
#' @param backend Which backend to use: \code{"auto"} (default) detects from
#'   the available API key, \code{"scivrs"} uses the Scienceverse platform,
#'   \code{"selfhosted"} uses a direct bibr API instance.
#' @param api_key API key (scivrs backend only). A Bearer token starting with
#'   \code{sv_}, defaults to the \code{SCIVRS_API_KEY} env var. Ignored for
#'   the \code{"selfhosted"} backend, which requires no authentication.
#' @param api_url Base URL of the API. Defaults to the appropriate URL for
#'   the selected backend.
#' @param include_figures Whether to include base64-encoded figure images
#'   in the output (default FALSE)
#' @param start_page First page of the file to extract (default 1)
#' @param end_page Last page of the file to extract (default Inf for all pages)
#' @param poll_interval Seconds between status polls, scivrs backend only
#'   (default 2)
#' @param timeout Maximum seconds to wait for processing, scivrs backend only
#'   (default 600)
#'
#' @return Path(s) to the saved JSON file(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-detect backend from environment variables
#' pdf <- demofile("pdf")
#' convert_bibr(pdf)
#'
#' # Explicitly use Scienceverse platform
#' convert_bibr(pdf, backend = "scivrs")
#'
#' # Use self-hosted bibr instance
#' convert_bibr(pdf, backend = "selfhosted")
#'
#' # Extract specific pages
#' convert_bibr(pdf, start_page = 1, end_page = 10)
#'
#' # Directory of papers
#' dir <- system.file("demo", package = "metacheck")
#' convert_bibr(dir, save_path = "results/")
#' }
convert_bibr <- function(file_path,
                         save_path = ".",
                         backend = c("auto", "scivrs", "selfhosted"),
                         api_key = NULL,
                         api_url = NULL,
                         include_figures = FALSE,
                         start_page = 1,
                         end_page = Inf,
                         poll_interval = 2,
                         timeout = 600) {
  backend <- match.arg(backend)

  # auto-detect backend ----
  if (backend == "auto") {
    if (!is.null(api_key) || nchar(Sys.getenv("SCIVRS_API_KEY")) > 0) {
      backend <- "scivrs"
    } else {
      backend <- "selfhosted"
    }
  }

  # resolve API key (scivrs only) ----
  if (backend == "scivrs" && is.null(api_key)) {
    api_key <- Sys.getenv("SCIVRS_API_KEY")
    if (nchar(api_key) == 0) {
      stop("API key not set. ",
           "Set the SCIVRS_API_KEY environment variable or pass api_key directly.",
           call. = FALSE)
    }
  }

  # resolve API URL ----
  if (is.null(api_url)) {
    api_url <- switch(backend,
      scivrs = "https://platform.metacheck.app",
      selfhosted = "http://localhost:8000"
    )
  }

  # handle directory or multiple files ----
  if (length(file_path) == 1 && dir.exists(file_path)) {
    dir_path <- file_path
    file_path <- list.files(dir_path,
                            pattern = "\\.(docx?|pdf)$",
                            full.names = TRUE)
  }

  if (length(file_path) > 1) {
    pb <- pb(length(file_path), "Converting :current/:total [:bar] (:what)")
    json_paths <- sapply(file_path, \(fp) {
      pb$tick(1, list(what = basename(fp)))
      tryCatch(
        convert_bibr(file_path = fp,
                     save_path = save_path,
                     backend = backend,
                     api_key = api_key,
                     api_url = api_url,
                     include_figures = include_figures,
                     start_page = start_page,
                     end_page = end_page,
                     poll_interval = poll_interval,
                     timeout = timeout),
        error = \(e) {
          logger("convert_bibr", e$message)
          return(NULL)
      })
    })
    return(json_paths)
  }


  # convert page values to zero-based (NULL = omit from request) ----
  zb_start_page <- if (start_page > 1) start_page - 1 else NULL
  zb_end_page <- if (is.finite(end_page)) end_page - 1 else NULL

  # dispatch to backend ----
  contents <- switch(backend,
    scivrs = .bibr_request_scivrs(
      file_path, api_url, api_key, include_figures,
      zb_start_page, zb_end_page, poll_interval, timeout
    ),
    selfhosted = .bibr_request_selfhosted(
      file_path, api_url, include_figures,
      zb_start_page, zb_end_page
    )
  )

  # save result ----
  .bibr_save_result(contents, file_path, save_path)
}

#' Check bibr server status
#'
#' @param api_url the URL to the bibr server
#' @param api_key the API key to use (NULL if local)
#' @param error whether to generate and error on failure
#'
#' @returns boolean
#' @keywords internal
.bibr_isalive <- function(api_url,
                          api_key = Sys.getenv("SCIVRS_API_KEY"),
                          error = TRUE) {
  resp <- tryCatch(
    {
      req <- httr2::request(api_url) |>
        httr2::req_url_path_append("ready")

      if (!is.null(api_key)) {
        req <- httr2::req_auth_bearer_token(req, api_key)
      }
      req |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    },
    error = function(e) {
      if (error) {
        stop(
          "Connection to the BIBR server failed. ",
          "Please check your connection or the URL: ", api_url,
          call. = FALSE
        )
      }
    }
  )

  if (is.null(resp)) return(FALSE)

  # check status
  status <- httr2::resp_status(resp)
  if (status != 200) {
    if (error) {
      stop("The BIBR server does not appear up and running on the URL ",
           api_url, ". Status: ", status,
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }

  # check for bad API key
  ct <- resp$headers$`content-type`
  if (!grepl("json", resp$headers$`content-type`)) {
    if (error) {
      stop("The server is running, but the API key is not valid",
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }

  # check readiness
  body <- httr2::resp_body_json(resp)
  if (body$status != "ready" || body$checks$bibr != "ok") {
    if (error) {
      stop("The server is running, but BIBR is not ready", status,
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }

  return(TRUE)
}


#' Submit and poll a job on the Scienceverse platform
#' @noRd
.bibr_request_scivrs <- function(file_path, api_url, api_key,
                                  include_figures, start_page, end_page,
                                  poll_interval, timeout) {
  # submit job ----
  submit_req <- httr2::request(api_url) |>
    httr2::req_url_path_append("jobs") |>
    httr2::req_auth_bearer_token(api_key)

  body <- list(
    .req = submit_req,
    file = curl::form_file(file_path),
    include_figures = tolower(as.character(include_figures))
  )
  if (!is.null(start_page)) body$start_page <- as.character(start_page)
  if (!is.null(end_page)) body$end_page <- as.character(end_page)

  submit_req <- do.call(httr2::req_body_multipart, body) |>
    httr2::req_timeout(60)

  submit_resp <- httr2::req_perform(submit_req)
  if (httr2::resp_status(submit_resp) != 200) {
    logger("convert_bibr", "submission failed")
    stop("Job submission failed (HTTP ", httr2::resp_status(submit_resp), "): ",
         httr2::resp_body_string(submit_resp),
         call. = FALSE)
  }

  job <- httr2::resp_body_json(submit_resp)
  job_id <- job$job_id

  # poll for completion ----
  status_url <- paste0(api_url, "/jobs/", job_id)
  elapsed <- 0

  pb <- pb(NA, ":job (:spin) :elapsed :what")
  on.exit(pb$terminate())
  #job <- sprintf("Job: %s [%s]", job_id, basename(file_path))
  job <- basename(file_path)
  pb$tick(0, list(what = "submitted", job = job))

  repeat {
    Sys.sleep(poll_interval)
    elapsed <- elapsed + poll_interval

    status_resp <- httr2::request(status_url) |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    status <- httr2::resp_body_json(status_resp)

    msg <- status$status
    if (!is.null(status$stage)) {
      msg <- sprintf("%s (%s)", status$status, status$stage)
    }
    pb$tick(0, list(what = msg, job = job))

    if (identical(status$status, "complete")) break

    if (identical(status$status, "failed")) {
      err_msg <- status$stage %||% "unknown error"
      logger("convert_bibr", list(job_id = job_id, error = err_msg))
      stop("Job ", job_id, " failed: ", err_msg, call. = FALSE)
    }

    if (elapsed >= timeout) {
      logger("convert_bibr", list(job_id = job_id, error = "timeout"))
      stop("Job ", job_id, " timed out after ", timeout, "s ",
           "(last status: ", status$status, ")",
           call. = FALSE)
    }
  }

  # download result ----
  result_req <- httr2::request(api_url) |>
    httr2::req_url_path_append("jobs", job_id, "result") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_timeout(120)

  result_resp <- httr2::req_perform(result_req)
  if (httr2::resp_status(result_resp) != 200) {
    logger("convert_bibr", "download failed")
    stop("Result download failed (HTTP ", httr2::resp_status(result_resp), ")",
         call. = FALSE)
  }

  httr2::resp_body_raw(result_resp)
}


#' Send a direct request to a self-hosted bibr instance
#' @noRd
.bibr_request_selfhosted <- function(file_path, api_url,
                                      include_figures, start_page, end_page) {
  req <- httr2::request(api_url) |>
    httr2::req_url_path_append("papers", "extract")

  body <- list(
    .req = req,
    file = curl::form_file(file_path),
    include_figures = tolower(as.character(include_figures))
  )
  if (!is.null(start_page)) body$start_page <- as.character(start_page)
  if (!is.null(end_page)) body$end_page <- as.character(end_page)

  req <- do.call(httr2::req_body_multipart, body) |>
    httr2::req_timeout(300)

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    code <- httr2::resp_status(resp)
    msg <- httr2::resp_status_desc(resp)
    stop("Bibr request failed with status code: ", code, "\n", msg,
         call. = FALSE)
  }

  httr2::resp_body_raw(resp)
}


#' Save raw bibr result to a JSON file
#' @noRd
.bibr_save_result <- function(contents, file_path, save_path) {
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  json_path <- basename(file_path) |>
    gsub("\\..{1,4}$", "\\.json", x = _) |>
    file.path(save_path, x = _)
  writeBin(contents, json_path)
  json_path
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
read_bibr <- function(file_path, include_images = FALSE) {
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
  paper <- paper_coerce(paper)

  # fix urls with . at end
  if (nrow(paper$url) > 0) {
    paper$url$href <- gsub("\\.$", "", paper$url$href)
  }

  paper
}


#' Read in grobid XML or bibr JSON
#'
#' @param file_path path to a directory containing XML and/or JSON files, or a vector of paths
#' @param include_images whether to include images in the figures table of the paper object (they make object size larger, only relevant to bibr imports)
#'
#' @returns a paper or paperlist
#' @export
read <- function(file_path, include_images = FALSE) {
  # handle directory or multiple files ----
  if (length(file_path) == 1 && dir.exists(file_path)) {
    dir_path <- file_path
    file_path <- list.files(dir_path,
                            pattern = "\\.(json|xml)$",
                            full.names = TRUE)
  }

  pb <- pb(length(file_path), "Loading :current/:total [:bar] (:what)")
  papers <- lapply(file_path, \(fp) {
    pb$tick(1, list(what = basename(fp)))
    tryCatch({
      if (grepl("\\.json$", fp, ignore.case = TRUE)) {
        read_bibr(fp, include_images)
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


#' Format Bib Authors
#'
#' Formats a structured author list (data frame with given/family columns)
#' as a display string.
#'
#' @param authors a data frame with \code{given} and \code{family} columns,
#'   or a list of such data frames
#'
#' @returns a character string (or vector) of formatted author names
#' @export
#'
#' @examples
#' authors <- data.frame(given = c("Alice H.", "Wendy"),
#'                       family = c("Eagly", "Wood"))
#' format_bib_authors(authors)
format_bib_authors <- function(authors) {
  if (is.list(authors) && !is.data.frame(authors)) {
    return(sapply(authors, format_bib_authors))
  }
  if (is.null(authors) || (is.data.frame(authors) && nrow(authors) == 0)) {
    return(NA_character_)
  }
  if (is.character(authors)) {
    a <- paste(authors, collapse = "; ")
    return(a)
  }
  paste(authors$family, authors$given, sep = ", ", collapse = "; ")
}


#' Coerce bib authors column to list of data frames
#'
#' Handles mixed input: structured \code{[{given, family}]} arrays (read as
#' data frames by jsonlite) and legacy pipe-separated strings. Returns a list
#' column suitable for storing in a data frame.
#'
#' @param col a list column (from jsonlite) or character vector of authors
#'
#' @returns a list where each element is a data frame with \code{given}/\code{family}
#' @keywords internal
.coerce_bib_authors <- function(col) {
  if (is.null(col)) return(col)

  # jsonlite may simplify a column of empty arrays into a 0-column data frame;

  # convert to a list of NULLs so lapply iterates over rows
  if (is.data.frame(col) && ncol(col) == 0) {
    col <- replicate(nrow(col), NULL, simplify = FALSE)
  }

  I(lapply(col, \(x) {
    if (is.null(x) || (is.atomic(x) && length(x) == 1 && is.na(x))) {
      return(data.frame(given = character(0), family = character(0)))
    }
    if (is.data.frame(x)) {
      # keep only given/family columns
      x[, intersect(c("given", "family"), names(x)), drop = FALSE]
    } else if (is.matrix(x)) {
      # legacy format: matrix with columns [given, family]
      data.frame(given = x[, 1], family = x[, 2], stringsAsFactors = FALSE)
    } else if (is.character(x) && length(x) == 1) {
      .parse_author_string(x)
    } else {
      data.frame(given = character(0), family = character(0))
    }
  }))
}


#' Parse a legacy author string into a data frame
#'
#' Handles formats like "Family, Given; Family2, Given2" or
#' "Family, Given, and Given2 Family2".
#'
#' @param s a character string of authors
#'
#' @returns a data frame with \code{given} and \code{family} columns
#' @keywords internal
.parse_author_string <- function(s) {
  if (is.na(s) || nchar(trimws(s)) == 0) {
    return(data.frame(given = character(0), family = character(0)))
  }

  # try semicolon-separated first: "Family, Given; Family2, Given2"
  if (grepl(";", s)) {
    parts <- trimws(strsplit(s, ";")[[1]])
    parsed <- lapply(parts, \(p) {
      fg <- trimws(strsplit(p, ",")[[1]])
      if (length(fg) >= 2) {
        data.frame(given = fg[[2]], family = fg[[1]])
      } else {
        data.frame(given = "", family = fg[[1]])
      }
    })
    return(do.call(rbind, parsed))
  }

  # try "and"-separated: "Family, Given, and Given2 Family2"
  # or simple "Family, Given"
  parts <- trimws(strsplit(s, "\\band\\b|&")[[1]])
  parsed <- lapply(parts, \(p) {
    # "Family, Given" pattern
    fg <- trimws(strsplit(p, ",")[[1]])
    if (length(fg) >= 2) {
      data.frame(given = trimws(fg[[2]]), family = trimws(fg[[1]]))
    } else {
      # "Given Family" pattern (space-separated, last word is family)
      words <- trimws(strsplit(trimws(p), "\\s+")[[1]])
      if (length(words) >= 2) {
        data.frame(given = paste(words[-length(words)], collapse = " "),
                   family = words[[length(words)]])
      } else if (length(words) == 1) {
        data.frame(given = "", family = words[[1]])
      } else {
        data.frame(given = character(0), family = character(0))
      }
    }
  })
  do.call(rbind, parsed)
}
