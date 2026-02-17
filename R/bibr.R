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
                            pattern = "\\.(docx?|html|md|pdf|txt)$",
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
  if (httr2::resp_status(resp) == 200) {
    # httr2::resp_content_type(resp)
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
#' @param file_path path to the zip file or a directory of zip files
#'
#' @returns a paper object
#' @export
read_bibr <- function(file_path) {
  # handle directory or multiple files ----
  if (length(file_path) == 1 && dir.exists(file_path)) {
    dir_path <- file_path
    file_path <- list.files(dir_path,
                            pattern = "\\.zip$",
                            full.names = TRUE)
  }

  if (length(file_path) > 1) {
    pb <- pb(length(file_path), "Loading :current/:total [:bar] (:what)")
    papers <- lapply(file_path, \(fp) {
      pb$tick(1, list(what = basename(fp)))
      tryCatch(
        read_bibr(file_path = fp),
        error = \(e) {
          logger("read_bibr", e$message)
          return(NULL)
        })
    })
    papers <- paperlist(papers)

    return(papers)
  }

  # temp dir for unzip and cleanup ----
  exdir <- file.path(
    tempdir(),
    basename(file_path) |> gsub("\\.zip$", "", x = _)
  )
  on.exit(unlink(exdir, recursive = TRUE))

  # unzip and check manifest ----
  unzipped_files <- utils::unzip(file_path, exdir = exdir)
  manifest <- file.path(exdir, "manifest.json") |>
    jsonlite::read_json()

  # read in arrow tables -----
  paper <- paper(manifest$file_hash)
  all_tables <- c(manifest$tables, manifest$dynamic_tables)
  for (table_name in all_tables) {
    tbl_path <- file.path(exdir, paste0(table_name, ".arrow"))
    paper[[table_name]] <- arrow::read_ipc_file(tbl_path)
  }

  paper
}
