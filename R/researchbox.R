#' Find ResearchBox Links in Papers
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the ResearchBox url in the first (text) column
#' @export
#'
#' @examples
#' rbox_links(psychsci)
rbox_links <- function(paper) {
  found <- search_text(paper, "researchbox")

  # match up to ">"
  match_ap <- search_text(found, "/researchbox\\.org[^\\>]+", return = "match")

  # clean up the text
  match_ap$text <- match_ap$text |>
    paste0("https:/", x = _)

  unique_matches <- match_ap |>
    unique()

  return(unique_matches)
}

#' Retrieve info from ResearchBox by URL
#'
#' @param rb_url an ResearchBox URL, or a table containing them (e.g., as created by `rbox_links()`)
#' @param id_col the index or name of the column that contains ResearchBox URLs, if id is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#' # get info on one OSF node
#' rbox_retrieve("https://researchbox.org/801")
#' }
rbox_retrieve <- function(rb_url, id_col = 1, pb = NULL) {
  if (!online("researchbox.org")) {
    stop("ResearchBox.org seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "ResearchBox Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(rb_url)) {
    table <- rb_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "rb_url"
    raw_urls <- unique(rb_url) |> stats::na.omit()
    table <- data.frame(rb_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    rb_url = raw_urls
  )
  ids <- ids[!is.na(ids$rb_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$rb_url)

  if (length(valid_ids) == 0) {
    ("No valid ResearchBox links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(table)
  }

  # iterate over valid IDs
  paste0(
    "Starting ResearchBox retrieval for ",
    length(valid_ids), " file",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  i <- 0
  error <- FALSE
  while (!error & i < length(valid_ids)) {
    i <- i + 1
    info <- rbox_info(valid_ids[[i]])
    if ("error" %in% names(info)) error <- TRUE
    id_info[[i]] <- info
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "rb_url")

  # reduplicate and add original table info
  by <- stats::setNames("rb_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".rb")
  )

  paste0("...ResearchBox retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from ResearchBox by URL
#'
#' @param rb_url a ResearchBox URL
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
rbox_info <- function(rb_url, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ", rb_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # set up return table
  obj <- data.frame(
    rb_url = rb_url
  )
  # get website
  resp <- httr2::request(rb_url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # check if redirect (suppress encodings warning)
  suppressMessages({
    body_text <- httr2::resp_body_string(resp)
    pattern <- "(?<=window\\.location\\.replace\\(')https://researchbox.org/\\d+(?='\\))"
    if (grepl(pattern, body_text, perl = TRUE)) {
      matches <- gregexpr(pattern, body_text, perl = TRUE)
      redirect_url <- regmatches(body_text, matches)

      resp <- httr2::request(redirect_url[[1]]) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    }
  })

  # handle missing file
  if (httr2::resp_status(resp) != 200) {
    warning(rb_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # Read the content with specified encoding
  html <- httr2::resp_body_string(resp) |>
    xml2::read_html(encoding = "UTF-8")

  # get file list
  file_names <- xml2::xml_find_all(html, "//p [@class='file_name']") |>
    xml2::xml_text()
  # filedesc <- xml2::xml_find_all(html, "//p [@class='preview_link']") |> # blocked out, seems gone after website redesign?
  #   xml2::xml_text()
  # filedesc <- filedesc[filedesc!=""]
  file_list <- data.frame(
    name = file_names
    # description = filedesc
  )
  obj$files <- list(file_list)

  # get info from bottom table
  body <- xml2::xml_find_all(html, "//body") |>
    rvest::html_text2() # xml2::xml_text()

  # section borders
  sections <- c(
    RB_target = "SUPPLEMENTARY FILES FOR",
    RB_license = "LICENSE FOR USE",
    RB_public = "BOX PUBLIC SINCE",
    RB_authors = "BOX CREATORS",
    RB_abstract = "ABSTRACT",
    done = "$('.file_number')"
  )

  for (i in 1:5) {
    obj[[names(sections)[i]]] <- tryCatch(
      {
        after <- strsplit(body, sections[[i]], fixed = TRUE)[[1]][[2]]
        answer <- strsplit(after, sections[[i + 1]], fixed = TRUE)[[1]][[1]]
        trimws(answer)
      },
      error = \(e) return(NA_character_)
    )
  }

  return(obj)
}


#' Retrieve files from ResearchBox by URL
#'
#' @param rb_url a vector of ResearchBox URLs
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
rbox_file_download <- function(rb_url, pb = NULL) {
  listed <- NULL

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  # vectorise
  if (length(rb_url) > 1) {
    unique_rb <- unique(rb_url) |> setdiff(NA)

    file_lists <- lapply(unique_rb, rbox_file_download, pb = pb)
    info <- do.call(dplyr::bind_rows, args = file_lists)
    orig <- data.frame(rb_url = rb_url)
    df <- dplyr::left_join(orig, info, by = "rb_url")

    return(df)
  }

  paste0("* Retrieving files from ", rb_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # Download a ZIP to a temp dir
  tmp_dir <- file.path(tempdir(), paste0("rbx_", as.integer(Sys.time())))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # Create a path for the ZIP file
  zip_path <- file.path(tmp_dir, basename(rb_url))
  if (!grepl("\\.zip$", zip_path, ignore.case = TRUE)) {
    zip_path <- file.path(tmp_dir, "archive.zip")
  }

  # download (use binary mode on Windows)
  paste0("Downloading to: ", zip_path) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  url_researchbox <- paste0(
    "https://s3.wasabisys.com/zipballs.researchbox.org/ResearchBox_",
    sub("^https://researchbox.org/", "", rb_url),
    ".zip"
  )
  tryCatch({
    utils::download.file(url_researchbox,
      destfile = zip_path,
      mode = "wb",
      quiet = TRUE
    )
  }, error = \(e) {})

  if (!file.exists(zip_path) || file.size(zip_path) == 0) {
    warning("Download failed or resulted in an empty file: ", zip_path)
    return(NULL)
  }

  # unzip into a subfolder
  out_dir <- file.path(tmp_dir, "unzipped")
  dir.create(out_dir, showWarnings = FALSE)

  paste0("Unzipping into: ", out_dir) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  unzipped_files <- utils::unzip(zip_path, exdir = out_dir)

  if (length(unzipped_files) == 0) {
    warning("Unzip produced no files. The archive might be corrupt.")
    return(NULL)
  }

  # list files (recursively) and return
  files <- list.files(out_dir, recursive = TRUE, full.names = FALSE)
  file_locations <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  file_info <- file.info(file_locations)

  # Create dataframe
  rb_file_info <- data.frame(
    rb_url = rep(rb_url, length(files)),
    name = files,
    file_location = file_locations,
    size = file_info$size,
    isdir = file_info$isdir
  )

  rb_file_info$ext <- strsplit(rb_file_info$name, "\\.") |>
    sapply(\(x) {
      if (length(x) < 2) {
        return("")
      }
      x[[length(x)]]
    }) |>
    tolower()
  rb_file_info <- dplyr::left_join(
    rb_file_info,
    metacheck::file_types,
    by = "ext"
  )

  return(rb_file_info)
}
