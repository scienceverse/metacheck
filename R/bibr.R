#' Process a paper using the Scienceverse platform API
#'
#' Submits a document to the Scienceverse platform queue for extraction.
#' The platform runs bibr behind Arq workers with load balancing, and is
#' the recommended way to process papers. Use \code{\link{bibr_convert}} for
#' direct bibr API access without the queue.
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_dir Path to a directory in which to save the zip file
#' @param api_url Base URL of the Scienceverse platform API
#' @param api_key Platform API key (Bearer token, starts with \code{sv_}).
#'   Defaults to the \code{PLATFORM_API_KEY} environment variable.
#' @param poll_interval Seconds between status polls (default 2)
#' @param timeout Maximum seconds to wait for processing (default 600)
#'
#' @return Path(s) to the saved zip file(s)
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
    zip_paths <- sapply(file_path, \(fp) {
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
    return(zip_paths)
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
    httr2::req_url_query(format = "arrow") |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_timeout(120)

  result_resp <- httr2::req_perform(result_req)
  if (httr2::resp_status(result_resp) != 200) {
    stop("Result download failed (HTTP ", httr2::resp_status(result_resp), ")",
         call. = FALSE)
  }

  contents <- httr2::resp_body_raw(result_resp)
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  zip_path <- basename(file_path) |>
    gsub("\\..{1,4}$", "\\.zip", x = _) |>
    file.path(save_dir, x = _)
  writeBin(contents, zip_path)

  pb$tick(0, list(what = zip_path))
  zip_path
}


#' Process a paper using the bibr API
#'
#' @param file_path Path to the document file, or a directory of documents
#' @param save_dir Path to a directory in which to save the zip file
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
    zip_paths <- sapply(file_path, \(fp) {
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
    return(zip_paths)
  }

  # change to zero-based values
  zb_start_page <- start_page - 1
  zb_end_page <- ifelse(end_page == Inf, -1, end_page - 1)

  # Make the POST request ----
  req <- httr2::request(api_url) |>
    httr2::req_auth_basic("thesanogoeffect", api_key) |>
    httr2::req_url_path_append("papers", "extract", "arrow") |>
    httr2::req_body_multipart(
      file = curl::form_file(file_path)
      #start_page = zb_start_page
      # end_page = zb_end_page
    ) |>
    httr2::req_timeout(300)

  resp <- httr2::req_perform(req)

  # Check if the request was successful
  if (httr2::resp_status(resp) == 200 &&
      httr2::resp_content_type(resp) == "application/zip") {
    contents <- httr2::resp_body_raw(resp)

    # Write to file
    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
    zip_path <- basename(file_path) |>
      gsub("\\..{1,4}$", "\\.zip", x = _) |>
      file.path(save_dir, x = _)
    writeBin(contents, zip_path)

  } else {
    code <- httr2::resp_status(resp)
    msg <- httr2::resp_status_desc(resp)
    stop(
      "Bibr request failed with status code: ", code, "\n", msg
    )
  }

  zip_path
}


#' Read Bibr zip file
#'
#' @param file_path path to the zip file
#'
#' @returns a paper object
#' @export
#'
#' @keywords internal
read_bibr <- function(file_path) {
  # temp dir for unzip and cleanup ----
  exdir <- file.path(
    tempdir(),
    basename(file_path) |> gsub("\\.zip$", "", x = _)
  )
  on.exit(unlink(exdir, recursive = TRUE))

  # unzip and check manifest ----
  unzipped_files <- utils::unzip(file_path, exdir = exdir)
  # manifest <- grep("manifest\\.json$", unzipped_files, value = TRUE) |>
  #   jsonlite::read_json()

  # read in arrow tables -----
  paper <- paper()
  # all_tables <- c(manifest$tables, manifest$dynamic_tables)
  all_tables <- grep("\\.arrow$", unzipped_files, value = TRUE)
  for (tbl_path in all_tables) {
    table_name <- basename(tbl_path) |> gsub("\\.arrow$", "", x = _)
    paper[[table_name]] <- arrow::read_ipc_file(tbl_path)
  }

  # temporary processing for format changes to be added to bibr ----
  paper$paper_id <- paper$info$file_hash
  if ("tbl_id" %in% names(paper$tables)) {
    names(paper$tables)[[1]] <- "table_id"
    names(paper$tables)[[3]] <- "html"
  }
  names(paper$figures)[[1]] <- "figure_id"
  paper$figures$caption <- NULL
  paper$equations$eq_type <- NULL

  # append references to sections and text and replace with text_id
  if ("bib_text" %in% paper$bib) {
    section_id <- max(c(0, paper$sections$section_id)) + 1
    sec_add <- list(section_id = section_id,
                    header = "References",
                    section_type = "references")
    paper$sections <- dplyr::bind_rows(paper$sections, sec_add)
    text_ids <- max(c(0, paper$text$text_id)) + seq_along(paper$bib$bib_text)
    p_ids <- max(c(0, paper$text$paragraph_id)) + seq_along(paper$bib$bib_text)
    text_add <- data.frame(
      text_id = text_ids,
      paragraph_id = p_ids,
      section_id = section_id,
      text = paper$bib$bib_text
    )
    paper$text <- dplyr::bind_rows(paper$text, text_add)
    paper$bib$text_id <- text_ids
    paper$bib$bib_text <- NULL

    suppressWarnings({
    paper$bib <- data.frame(
      bib_id = paper$bib$bib_id,
      bib_type = paper$bib$type,
      doi = paper$bib$doi,
      title = paper$bib$title,
      authors = paper$bib$authors %||% paper$bib$author,
      editors = paper$bib$editors %||% paper$bib$editor,
      publisher = paper$bib$publisher,
      publication_year = paper$bib$publication_year %||% paper$bib$year,
      publication_date = NA_character_,
      container = paper$bib$container %||% paper$bib$journal %||% paper$bib$booktitle,
      volume = paper$bib$volume,
      issue = paper$bib$number %||% paper$bib$issue,
      first_page = paper$bib$first_page,
      last_page = paper$bib$last_page,
      edition = NA_character_,
      version = NA_character_,
      url = paper$bib$url %||% paper$bib$link,
      text_id = paper$bib$text_id
    )
    })
  }


  # fix urls with . at end
  paper$links$url <- gsub("\\.$", "", paper$links$url)

  paper
}

#' Read in grobis XML or bibr ZIP
#'
#' @param file_path path to a directory containing XML and/or zip files, or a vector of paths to XML or zip files
#'
#' @returns a paper or paperlist
#' @export
read <- function(file_path) {
  # handle directory or multiple files ----
  if (length(file_path) == 1 && dir.exists(file_path)) {
    dir_path <- file_path
    file_path <- list.files(dir_path,
                            pattern = "\\.(zip|xml)$",
                            full.names = TRUE)
  }

  pb <- pb(length(file_path), "Loading :current/:total [:bar] (:what)")
  papers <- lapply(file_path, \(fp) {
    pb$tick(1, list(what = basename(fp)))
    tryCatch({
      if (grepl("\\.zip$", fp, ignore.case = TRUE)) {
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


