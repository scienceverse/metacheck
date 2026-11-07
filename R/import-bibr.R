#' Convert documents using bibr
#'
#' Converts document files (PDF, DOC, DOCX) to structured JSON using the bibr
#' extraction service. Supports three backends: metacheck's API on SURF
#' Research Cloud (\code{"surf"}), which converts PDF files and needs no API
#' key; the Scienceverse platform (\code{"scivrs"}) which uses a job queue
#' with load balancing; and a self-hosted bibr instance (\code{"selfhosted"})
#' for direct API access.
#'
#' When \code{backend = "auto"} (the default), the \code{"scivrs"} backend is
#' used if \code{api_key} is provided or the \code{SCIVRS_API_KEY} environment
#' variable is set, and \code{"selfhosted"} if \code{api_url} is provided or
#' the \code{BIBR_API_URL} environment variable is set. Otherwise,
#' \code{"surf"} is used.
#'
#' The \code{"surf"} backend sends the PDF to the \code{/convert} endpoint of
#' metacheck's API on SURF Research Cloud, whose address can be set with the
#' \code{SURF_API_URL} environment variable. It converts only PDF files (at
#' most 50 MB), always whole and without figures, so \code{include_figures},
#' \code{start_page} and \code{end_page} do not apply. The server deletes the
#' upload once it is converted, and the result an hour later.
#'
#' A self-hosted bibr server that requires authentication (\code{bibr serve}
#' with \code{AUTH_API_KEY} set, as on a SURF Research Cloud workspace) gets
#' its bearer token from \code{api_key} or the \code{BIBR_API_KEY}
#' environment variable; its address can be set with \code{BIBR_API_URL}.
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_path Path to a directory in which to save the JSON file; there is no default, because CRAN does not allow writing into the
#'   working directory unless you ask for it
#' @param backend Which backend to use: \code{"auto"} (default) detects from
#'   the available API key and URL (see Details), \code{"surf"} uses
#'   metacheck's API on SURF Research Cloud, \code{"scivrs"} uses the
#'   Scienceverse platform, \code{"selfhosted"} uses a direct bibr API
#'   instance.
#' @param api_key API key. For the scivrs backend, a Bearer token starting
#'   with \code{sv_}, defaulting to the \code{SCIVRS_API_KEY} env var. For the
#'   selfhosted backend, the server's bearer token if it requires one,
#'   defaulting to the \code{BIBR_API_KEY} env var (none if unset).
#' @param api_url Base URL of the API. Defaults to the appropriate URL for
#'   the selected backend (for surf, the \code{SURF_API_URL} env var or
#'   \code{https://metacheckserve.metacheck.src.surf-hosted.nl/api};
#'   for selfhosted, the \code{BIBR_API_URL} env var or
#'   \code{http://localhost:8000}).
#' @param include_figures Whether to include base64-encoded figure images
#'   in the output (default FALSE; not for the surf backend)
#' @param start_page First page of the file to extract (default 1; not for
#'   the surf backend)
#' @param end_page Last page of the file to extract (default Inf for all
#'   pages; not for the surf backend)
#' @param poll_interval Seconds between status polls, scivrs backend only
#'   (default 2)
#' @param timeout Maximum seconds to wait for processing (default 600)
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
#' # Explicitly use metacheck's API on SURF Research Cloud
#' convert_bibr(pdf, backend = "surf")
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
                         save_path,
                         backend = c("auto", "surf", "scivrs", "selfhosted"),
                         api_key = NULL,
                         api_url = NULL,
                         include_figures = FALSE,
                         start_page = 1,
                         end_page = Inf,
                         poll_interval = 2,
                         timeout = 600) {
  backend <- match.arg(backend)
  # before any file is sent to a server: there is no default folder
  force(save_path)

  # auto-detect backend ----
  # (a key means the Scienceverse platform, a bibr address a self-hosted
  # bibr, as convert() passes for a local bibr; otherwise SURF)
  if (backend == "auto") {
    if (!is.null(api_key) || nchar(Sys.getenv("SCIVRS_API_KEY")) > 0) {
      backend <- "scivrs"
    } else if (!is.null(api_url) || nchar(Sys.getenv("BIBR_API_URL")) > 0) {
      backend <- "selfhosted"
    } else {
      backend <- "surf"
    }
  }

  if (backend == "surf" &&
      (isTRUE(include_figures) || start_page > 1 || is.finite(end_page))) {
    warning("The SURF API converts whole PDFs without figures; ",
            "include_figures, start_page and end_page are not used.",
            call. = FALSE)
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

  # resolve API key (selfhosted: optional bearer token) ----
  if (backend == "selfhosted" && is.null(api_key) &&
      nchar(Sys.getenv("BIBR_API_KEY")) > 0) {
    api_key <- Sys.getenv("BIBR_API_KEY")
  }

  # resolve API URL ----
  if (is.null(api_url)) {
    api_url <- switch(backend,
      surf = .surf_api_url(),
      scivrs = "https://platform.metacheck.app",
      selfhosted = rep_if(Sys.getenv("BIBR_API_URL"), "http://localhost:8000", replace = "")
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
    surf = .bibr_request_surf(file_path, api_url, timeout),
    scivrs = .bibr_request_scivrs(
      file_path, api_url, api_key, include_figures,
      zb_start_page, zb_end_page, poll_interval, timeout
    ),
    selfhosted = .bibr_request_selfhosted(
      file_path, api_url, include_figures,
      zb_start_page, zb_end_page,
      api_key = api_key, timeout = timeout
    )
  )

  # save result ----
  .bibr_save_result(contents, file_path, save_path)
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
        # retry_on_failure covers a connection-level failure (timeout, DNS,
        # refused), not just an HTTP error response -- a live server can
        # still throw here on one bad connection attempt (see issue #363,
        # same failure mode confirmed for the sibling .grobid_isalive()
        # check), so retrying absorbs a one-off blip instead of treating it
        # the same as a genuinely dead server.
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
        httr2::req_perform()
    },
    error = function(e) {
      if (error) {
        stop(
          "Connection to the BIBR server failed. ",
          "Please check your connection or the URL: ", api_url,
          " (", conditionMessage(e), ")",
          call. = FALSE
        )
      }
    }
  )

  if (is.null(resp)) return(FALSE)

  # check status
  status <- httr2::resp_status(resp)
  # A missing or wrong API key is refused with 401/403 (platform.metacheck.app
  # did so on 2026-10-14; it used to answer 200 with a non-JSON page, which
  # the content-type check below catches). Either way the server is up and the
  # key is the problem, so say that rather than "not up and running".
  if (status %in% c(401L, 403L)) {
    if (error) {
      stop("The server is running, but the API key is not valid",
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }
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
  # The Scienceverse platform reports checks$bibr; a self-hosted bibr serve
  # reports its own checks (ocr, classifiers, redis) and no bibr entry.
  bibr_check <- body$checks$bibr
  if (!identical(body$status, "ready") ||
      (!is.null(bibr_check) && !identical(bibr_check, "ok"))) {
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


#' The address of metacheck's API on SURF Research Cloud
#' @noRd
.surf_api_url <- function() {
  rep_if(Sys.getenv("SURF_API_URL"), "https://metacheckserve.metacheck.src.surf-hosted.nl/api", replace = "")
}

#' Check that metacheck's API on SURF Research Cloud can convert PDFs
#'
#' Its /health endpoint answers with `status` "ok", `bibr` TRUE when the
#' server's bibr is ready, and `grobid` "local" when the server has a GROBID of
#' its own. Its /convert uses that GROBID when bibr is not ready or fails
#' (`.convert_paper()` in the surf-research-cloud repository), so the server can
#' convert a PDF when either one is there.
#'
#' @param api_url the URL of the API
#' @param error whether to generate an error on failure
#'
#' @returns boolean
#' @keywords internal
.surf_isalive <- function(api_url = .surf_api_url(), error = TRUE) {
  resp <- tryCatch(
    {
      httr2::request(api_url) |>
        httr2::req_url_path_append("health") |>
        httr2::req_timeout(15) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        # absorb a one-off connection blip (see .bibr_isalive())
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
        httr2::req_perform()
    },
    error = function(e) {
      if (error) {
        stop("Connection to the SURF API failed. ",
             "Please check your connection or the URL: ", api_url,
             " (", conditionMessage(e), ")",
             call. = FALSE)
      }
    }
  )

  if (is.null(resp)) return(FALSE)

  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
  ready <- status == 200 && identical(body$status, "ok") &&
    (isTRUE(body$bibr) || identical(body$grobid, "local"))
  if (!ready && error) {
    stop("The SURF API on ", api_url, " cannot convert PDFs at the moment",
         if (status != 200) paste0(" (HTTP ", status, ")")
         else " (its bibr is not ready and it has no GROBID of its own)",
         call. = FALSE)
  }

  ready
}

#' Convert a PDF with metacheck's API on SURF Research Cloud
#'
#' One call to /convert, which answers once bibr is done (the server's proxy
#' waits up to 30 minutes). A refused or failed conversion answers with JSON
#' holding the reason in `error` (400 bad upload, 413 too large, 422 bibr
#' failed, 429 too many requests, 503 queue full).
#' @noRd
.bibr_request_surf <- function(file_path, api_url, timeout = 600) {
  if (!grepl("\\.pdf$", file_path, ignore.case = TRUE)) {
    stop("The SURF API converts only PDF files, not ", basename(file_path),
         "; use backend = \"scivrs\" or \"selfhosted\" for DOC and DOCX files.",
         call. = FALSE)
  }

  resp <- httr2::request(api_url) |>
    httr2::req_url_path_append("convert") |>
    httr2::req_body_multipart(file = curl::form_file(file_path)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status != 200) {
    msg <- tryCatch(httr2::resp_body_json(resp)$error, error = \(e) NULL) %||%
      httr2::resp_status_desc(resp)
    logger("convert_bibr", list(file = basename(file_path), error = msg))
    stop("The SURF API could not convert ", basename(file_path),
         " (HTTP ", status, "): ", msg, call. = FALSE)
  }

  # the server saves every upload as input.pdf, so bibr reports that name;
  # put the real one back
  json <- rawToChar(httr2::resp_body_raw(resp))
  real_name <- jsonlite::toJSON(enc2utf8(basename(file_path)), auto_unbox = TRUE)
  json <- sub("\"file_name\":\"input.pdf\"", paste0("\"file_name\":", real_name),
              json, fixed = TRUE, useBytes = TRUE)
  charToRaw(json)
}


#' Send a direct request to a self-hosted bibr instance
#' @noRd
.bibr_request_selfhosted <- function(file_path, api_url,
                                      include_figures, start_page, end_page,
                                      api_key = NULL, timeout = 300) {
  req <- httr2::request(api_url) |>
    httr2::req_url_path_append("papers", "extract")

  if (!is.null(api_key) && nchar(api_key) > 0) {
    req <- httr2::req_auth_bearer_token(req, api_key)
  }

  body <- list(
    .req = req,
    file = curl::form_file(file_path),
    include_figures = tolower(as.character(include_figures))
  )
  if (!is.null(start_page)) body$start_page <- as.character(start_page)
  if (!is.null(end_page)) body$end_page <- as.character(end_page)

  req <- do.call(httr2::req_body_multipart, body) |>
    httr2::req_timeout(timeout)

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
      data.frame(given = x[, 1], family = x[, 2])
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


