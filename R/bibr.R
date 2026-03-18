#' Process a paper using the Scienceverse platform API
#'
#' Submits a document to the Scienceverse platform queue for extraction.
#' The platform runs bibr behind Arq workers with load balancing, and is
#' the recommended way to process papers. Use \code{\link{bibr_convert}} for
#' direct bibr API access without the queue.
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_dir Path to a directory in which to save the JSON file
#' @param api_url Base URL of the Scienceverse platform API
#' @param api_key Platform API key (Bearer token, starts with \code{sv_}).
#'   Defaults to the \code{PLATFORM_API_KEY} environment variable.
#' @param poll_interval Seconds between status polls (default 2)
#' @param timeout Maximum seconds to wait for processing (default 600)
#'
#' @return Path(s) to the saved JSON file(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # Single file
#' pdf <- system.file("demo/to_err_is_human.pdf", package = "metacheck")
#' platform_bibr_convert(pdf)
#'
#' # Directory of papers
#' dir <- system.file("demo", package = "metacheck")
#' platform_bibr_convert(dir, save_dir = "results/")
#' }
platform_bibr_convert <- function(file_path,
                       save_dir = ".",
                       api_url = "https://platform.metacheck.app",
                       api_key = Sys.getenv("PLATFORM_API_KEY"),
                       poll_interval = 2,
                       timeout = 600) {
  if (nchar(api_key) == 0) {
    stop("Platform API key not set. ",
         "Set the PLATFORM_API_KEY environment variable or pass api_key directly.",
         call. = FALSE)
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
        platform_bibr_convert(file_path = fp,
                              save_dir = save_dir,
                              api_url = api_url,
                              api_key = api_key,
                              poll_interval = poll_interval,
                              timeout = timeout),
        error = \(e) {
          logger("platform_bibr_convert", e$message)
          return(NULL)
      })
    })
    return(json_paths)
  }

  # submit job ----
  submit_req <- httr2::request(api_url) |>
    httr2::req_url_path_append("jobs") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_multipart(
      file = curl::form_file(file_path)
    ) |>
    httr2::req_timeout(60)

  submit_resp <- httr2::req_perform(submit_req)
  if (httr2::resp_status(submit_resp) != 200) {
    stop("Job submission failed (HTTP ", httr2::resp_status(submit_resp), "): ",
         httr2::resp_body_string(submit_resp),
         call. = FALSE)
  }

  job <- httr2::resp_body_json(submit_resp)
  job_id <- job$job_id

  # poll for completion ----
  status_url <- paste0(api_url, "/jobs/", job_id)
  elapsed <- 0

  # set up progress bar ----
  pb <- pb(NA, "(:spin) :elapsed :what")
  on.exit(pb$terminate())
  pb$tick(0, list(what = "submitted"))
  pb$message(paste0("Job: ", job_id, " [", basename(file_path), "]"))

  repeat {
    Sys.sleep(poll_interval)
    elapsed <- elapsed + poll_interval

    status_resp <- httr2::request(status_url) |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    status <- httr2::resp_body_json(status_resp)

    msg <- paste0(status$status,
               if (!is.null(status$stage)) paste0(" (", status$stage, ")"))
    pb$tick(0, list(what = msg))

    if (identical(status$status, "complete")) break

    if (identical(status$status, "failed")) {
      err_msg <- status$stage %||% "unknown error"
      stop("Job ", job_id, " failed: ", err_msg, call. = FALSE)
    }

    if (elapsed >= timeout) {
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
    stop("Result download failed (HTTP ", httr2::resp_status(result_resp), ")",
         call. = FALSE)
  }

  contents <- httr2::resp_body_raw(result_resp)
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  json_path <- basename(file_path) |>
    gsub("\\..{1,4}$", "\\.json", x = _) |>
    file.path(save_dir, x = _)
  writeBin(contents, json_path)

  pb$tick(0, list(what = json_path))
  json_path
}


#' Process a paper using the bibr API
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_dir Path to a directory in which to save the JSON file
#' @param api_url Base URL of the API
#' @param api_key Key to access bibr
#' @param start_page First page of the file to extract
#' @param end_page Last page of the file to extract
#'
#' @return A list of parsed information
#' @export
#' @keywords internal
bibr_convert <- function(file_path,
                         save_dir = ".",
                         api_url = "https://api.bibr.metacheck.app",
                         api_key = Sys.getenv("BIBR_API"),
                         start_page = 1,
                         end_page = Inf) {
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
        bibr_convert(file_path = fp,
                     save_dir = save_dir,
                     api_url = api_url,
                     api_key = api_key,
                     start_page = start_page,
                     end_page = end_page),
        error = \(e) {
          logger("bibr_convert", e$message)
          return(NULL)
      })
    })
    return(json_paths)
  }

  # change to zero-based values
  zb_start_page <- start_page - 1
  zb_end_page <- ifelse(end_page == Inf, -1, end_page - 1)

  # Make the POST request ----
  req <- httr2::request(api_url) |>
    httr2::req_auth_basic("thesanogoeffect", api_key) |>
    httr2::req_url_path_append("papers", "extract") |>
    httr2::req_body_multipart(
      file = curl::form_file(file_path)
    ) |>
    httr2::req_timeout(300)

  resp <- httr2::req_perform(req)

  # Check if the request was successful
  if (httr2::resp_status(resp) == 200) {
    contents <- httr2::resp_body_raw(resp)

    # Write to file
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    json_path <- basename(file_path) |>
      gsub("\\..{1,4}$", "\\.json", x = _) |>
      file.path(save_dir, x = _)
    writeBin(contents, json_path)

  } else {
    code <- httr2::resp_status(resp)
    msg <- httr2::resp_status_desc(resp)
    stop(
      "Bibr request failed with status code: ", code, "\n", msg
    )
  }

  json_path
}


#' Read bibr JSON file
#'
#' @param file_path path to the JSON file (or legacy ZIP file)
#'
#' @returns a paper object
#' @export
#'
#' @keywords internal
read_bibr <- function(file_path) {
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
  paper$info$keywords <- I(list(as.vector(keywords)))

  # author ----
  if (!is.null(data$author) && length(data$author) > 0) {
    paper$author <- as.data.frame(data$author)
  }

  # bib ----
  if (!is.null(data$bib) && length(data$bib) > 0) {
    paper$bib <- as.data.frame(data$bib)
    paper$bib$authors <- .coerce_bib_authors(paper$bib$authors)
    if ("editors" %in% names(paper$bib)) {
      paper$bib$editors <- .coerce_bib_authors(paper$bib$editors)
    }
  }

  # eq ----
  if (!is.null(data$eq) && length(data$eq) > 0) {
    paper$eq <- as.data.frame(data$eq)
  }


  # figure ----
  if (!is.null(data$figure) && length(data$figure) > 0) {
    paper$figure <- as.data.frame(data$figure)
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
  }

  # text ----
  if (!is.null(data$text) && length(data$text) > 0) {
    paper$text <- as.data.frame(data$text)
  }

  # xref ----
  if (!is.null(data$xref) && length(data$xref) > 0) {
    paper$xref <- as.data.frame(data$xref)
  }

  # bib_matches ----
  # if (!is.null(data$bib_matches) && length(data$bib_matches) > 0) {
  #   paper$bib_matches <- as.data.frame(data$bib_matches)
  #   if ("authors" %in% names(paper$bib_matches)) {
  #     paper$bib_matches$authors <- .coerce_bib_authors(paper$bib_matches$authors)
  #   }
  #   if ("editors" %in% names(paper$bib_matches)) {
  #     paper$bib_matches$editors <- .coerce_bib_authors(paper$bib_matches$editors)
  #   }
  # }

  # ensure all expected columns exist and have correct types
  # (JSON may drop all-NA columns or read them back as logical)
  paper <- paper_coerce(paper)
  # template <- paper()
  # for (slot_name in names(template)) {
  #   tmpl <- template[[slot_name]]
  #   slot <- paper[[slot_name]]
  #   if (is.data.frame(tmpl) && is.data.frame(slot) && nrow(slot) > 0) {
  #     for (col in names(tmpl)) {
  #       if (!col %in% names(slot)) {
  #         # add missing column with appropriate NA type
  #         if (is.list(tmpl[[col]])) {
  #           slot[[col]] <- I(replicate(nrow(slot), NULL, simplify = FALSE))
  #         } else {
  #           slot[[col]] <- NA
  #         }
  #       }
  #       # coerce all-NA logical columns to the template type
  #       if (is.logical(slot[[col]]) && all(is.na(slot[[col]]))) {
  #         tmpl_type <- typeof(tmpl[[col]])
  #         if (tmpl_type == "integer") slot[[col]] <- NA_integer_
  #         else if (tmpl_type == "double") slot[[col]] <- NA_real_
  #         else if (tmpl_type == "character") slot[[col]] <- NA_character_
  #         else if (tmpl_type == "list") {
  #           slot[[col]] <- I(replicate(nrow(slot), NULL, simplify = FALSE))
  #         }
  #       }
  #     }
  #     paper[[slot_name]] <- slot
  #   }
  # }

  # fix urls with . at end
  if (nrow(paper$url) > 0) {
    paper$url$href <- gsub("\\.$", "", paper$url$href)
  }

  paper
}


#' Read in grobid XML or bibr JSON
#'
#' @param file_path path to a directory containing XML and/or JSON files, or a vector of paths
#'
#' @returns a paper or paperlist
#' @export
read <- function(file_path) {
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
        read_bibr(file_path = fp)
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
  if (is.character(authors)) return(authors)
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

