#' Find Zenodo Links in Papers
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Zenodo url in the first (text) column
#' @export
#'
#' @examples
#' zenodo_links(psychsci)
zenodo_links <- function(paper) {
  href <- NULL

  links <- paper_table(paper, "url") |>
    dplyr::filter(grepl("zenodo\\.org|10\\.5281/zenodo", href, ignore.case = TRUE))

  links$zenodo_url <- links$href
  links$zenodo_id <- .zenodo_id(links$zenodo_url)
  links$zenodo_link <- ifelse(
    is.na(links$zenodo_id),
    NA_character_,
    paste0("https://doi.org/10.5281/zenodo.", links$zenodo_id)
  )

  return(links)
}

#' Get zenodo ID from URL
#'
#' @param zenodo_url a vector of URLs to zenodo repos
#'
#' @returns a character vector of IDs
#' @keywords internal
.zenodo_id <- function(zenodo_url) {
  if (length(zenodo_url) == 0) {
    return(character(0))
  }

  if (length(zenodo_url) > 1) {
    return(vapply(zenodo_url, .zenodo_id, character(1)))
  }

  # handle single zenodo_url ----
  zenodo_url <- trimws(as.character(zenodo_url))

  if (is.na(zenodo_url) || !nzchar(zenodo_url)) {
    return(NA_character_)
  }

  if (grepl("^[0-9]+$", zenodo_url)) {
    return(zenodo_url)
  }

  patterns <- c(
    "10\\.5281/zenodo\\.([0-9]+)",
    "zenodo\\.org/(?:records?|uploads)/([0-9]+)",
    "zenodo\\.([0-9]+)"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, zenodo_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(zenodo_url, match)[[1]]
    if (length(groups) >= 2) {
      return(groups[[2]])
    }
  }

  return(NA_character_)
}

#' Retrieve info from Zenodo by URL
#'
#' @param zenodo_url an Zenodo URL, or a table containing them (e.g., as created by `zenodo_links()`)
#' @param id_col the index or name of the column that contains Zenodo URLs, if id is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   # get info on one zenodo link
#'   zenodo_info("https://doi.org/10.5281/zenodo.18648142")
#' }
zenodo_info <- function(zenodo_url, id_col = 1, pb = NULL) {
  if (!online("zenodo.org")) {
    stop("Zenodo.org seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Zenodo Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(zenodo_url)) {
    table <- zenodo_url
    table$zenodo_url <- table[[id_col]]
  } else {
    raw_urls <- unique(zenodo_url) |> stats::na.omit()
    table <- data.frame(zenodo_url = raw_urls)
  }

  ids <- data.frame(
    zenodo_url = table$zenodo_url,
    zenodo_id = .zenodo_id(table$zenodo_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$zenodo_url), , drop = FALSE]
  valid_ids <- unique(stats::na.omit(ids$zenodo_id))

  if (length(valid_ids) == 0) {
    ("No valid Zenodo links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "zenodo_url"))
  }

  # iterate over valid IDs
  paste0(
    "Starting Zenodo retrieval for ",
    length(valid_ids), " file",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  i <- 0
  while (i < length(valid_ids)) {
    i <- i + 1
    id_info[[i]] <- .zenodo_info(valid_ids[[i]], pb = pb)
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _)

  data <- table |>
    dplyr::left_join(ids, by = "zenodo_url") |>
    dplyr::left_join(info, by = "zenodo_id", suffix = c("", ".zenodo"))

  paste0("...Zenodo retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from Zenodo by ID
#'
#' @param zenodo_id a Zenodo ID or URL
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.zenodo_info <- function(zenodo_id, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  zenodo_id <- .zenodo_id(zenodo_id)
  paste0("* Retrieving info from Zenodo ID ", zenodo_id, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # set up return table
  obj <- data.frame(
    zenodo_id = zenodo_id
  )

  # Build the URL
  zenodo_api_url <- paste0("https://zenodo.org/api/records/", zenodo_id)

  resp <- httr2::request(zenodo_api_url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    warning(zenodo_id, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  rec <- tryCatch(
    httr2::resp_body_json(resp),
    error = \(e) NULL
  )
  if (is.null(rec)) {
    obj$error <- "parse_error"
    return(obj)
  }

  metadata <- rec[["metadata"]]
  stats <- rec[["stats"]]
  resource_type <- metadata[["resource_type"]]
  license <- metadata[["license"]]
  if (is.list(license)) {
    if (!is.null(license$id)) {
      license <- license$id
    } else if (!is.null(license$title)) {
      license <- license$title
    } else {
      license <- NA_character_
    }
  }

  if (is.null(license)) {
    license <- NA_character_
  }

  # Basic metadata
  obj$title <-            metadata[["title"]] %||% NA_character_
  obj$doi <-              rec[["doi"]] %||% NA_character_
  obj$description <-      metadata[["description"]] %||% NA_character_
  obj$publication_date <- metadata[["publication_date"]] %||% NA_character_
  obj$updated_date <-     rec[["updated"]] %||% NA_character_
  obj$creators <-         list(if (is.null(metadata[["creators"]])) list() else metadata[["creators"]])
  obj$keywords <- list(if (is.null(metadata[["keywords"]])) list() else metadata[["keywords"]])
  obj$resource_type <- if (is.null(resource_type[["type"]])) NA_character_ else resource_type[["type"]]
  obj$journal <- list(if (is.null(metadata[["journal"]])) list() else metadata[["journal"]])
  obj$owners <- list(if (is.null(rec[["owners"]])) list() else rec[["owners"]])
  obj$license <- license
  obj$downloads <- stats[["downloads"]] %||% NA_real_
  obj$unique_downloads <- stats[["unique_downloads"]] %||% NA_real_
  obj$views <- stats[["views"]] %||% NA_real_
  obj$files <- list(if (is.null(rec[["files"]])) list() else rec[["files"]])

  return(obj)
}


#' Download all Zenodo Project Files
#'
#' Creates a directory for the Zenodo ID and downloads all of the files using a folder structure from the Zenodo project nodes and file storage structure. Returns (invisibly) a data frame with file info.
#'
#' You can limit downloads to only files under a specific size (defaults to 10MB) and only a maximum download size (largest files will be omitted until total size is under the limit). Omitted files will be listed as messages in verbose mode, and included in the returned data frame with the downloaded column value set to FALSE.
#'
#' @param zenodo_id an Zenodo ID or URL
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL or Inf for no restrictions
#' @param max_download_size maximum total size to download - set to NULL of Inf for no restrictions
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info
#' @export
#'
#' @examples
#' \dontrun{
#'   zenodo_file_download("2591593")
#' }
zenodo_file_download <- function(zenodo_id,
                                 download_to = ".",
                                 max_file_size = 10,
                                 max_download_size = 100,
                                 pb = NULL) {
  zenodo_id <- .zenodo_id(zenodo_id) |>
    stats::na.omit() |>
    unique()
  if (length(zenodo_id) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Zenodo File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple IDs ----
  if (length(zenodo_id) > 1) {
    paste0(
      "Starting downloads for ", length(zenodo_id),
      " Zenodo records...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(zenodo_id, function(x) {
      tryCatch(
        zenodo_file_download(
          x,
          download_to = download_to,
          max_file_size = max_file_size,
          max_download_size = max_download_size,
          pb = pb
        ),
        error = function(e) {
          warning(x, " resulted in an error:\n  ", conditionMessage(e), "\n")
          return(NULL)
        }
      )
    })

    # bind all rows; drop NULLs safely
    dl_list <- dl_list[!vapply(dl_list, is.null, logical(1))]
    if (length(dl_list) == 0) return(NULL)

    dl <- dplyr::bind_rows(dl_list)
    paste0(
      "...Completed downloads for ", length(zenodo_id),
      " Zenodo records"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  # --- retrieve record contents ----
  paste0("Starting retrieval for ", zenodo_id) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(zenodo_info(zenodo_id, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", zenodo_id, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from nested entries
  rows <- lapply(
    files_list,
    function(x) {
      dplyr::tibble(
        id       = x$id %||% NA_character_,
        key      = x$key %||% NA_character_,
        size     = as.numeric( x$size %||% NA_real_),
        checksum = x$checksum %||% NA_character_,
        self     = x$links$self %||% NA_character_
      )
    }
  )

  files <- dplyr::bind_rows(rows)

  if (nrow(files) == 0) {
    message("- ", zenodo_id, " contained no files")
    return(NULL)
  }

  # --- size filters (MB) ----
  if (!is.null(max_file_size) && is.finite(max_file_size) && max_file_size > 0) {
    too_big_files <- which(files$size > max_file_size * 1024 * 1024)
    if (length(too_big_files) > 0) {
      for (i in too_big_files) {
        paste0(
          "- omitting ", files$key[[i]],
          " (", round(files$size[[i]] / 1024 / 1024, 1), "MB)"
        ) |>
          list(what = _) |>
          pb$tick(0, tokens = _)
      }
      files <- files[-too_big_files, , drop = FALSE]
    }
  }

  # remove largest files until total <= limit
  if (!is.null(max_download_size) && is.finite(max_download_size) && max_download_size > 0) {
    while (nrow(files) > 0 && sum(files$size, na.rm = TRUE) > max_download_size * 1024 * 1024) {
      max_file <- which(files$size == max(files$size, na.rm = TRUE))[1L]
      paste0(
        "- omitting ", files$key[[max_file]],
        " (", round(files$size[[max_file]] / 1024 / 1024, 1), "MB)"
      ) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      files <- files[-max_file, , drop = FALSE]
    }
  }

  if (nrow(files) == 0) {
    paste0("- All files omitted due to size constraints") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # --- target directory (avoid overwrite) ----
  download_to <- fs::path_abs(download_to)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, as.character(zenodo_id))
  }
  i <- 0L
  while (dir.exists(download_to)) {
    i <- i + 1L
    base <- sub("_\\d+$", "", download_to)
    download_to <- paste0(base, "_", i)
  }
  dir.create(download_to, showWarnings = FALSE, recursive = FALSE)
  paste0("- Created directory ", download_to) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # --- download into temp, then copy to target ----
  temppath <- fs::file_temp()
  dir.create(temppath)

  n <- nrow(files)
  files$downloaded <- FALSE

  for (i in seq_len(n)) {
    ok <- FALSE
    if (!is.na(files$self[[i]]) && nzchar(files$self[[i]])) {
      # write to a stable temp filename (use Zenodo file `id`)
      target_path <- file.path(temppath, files$id[[i]])
      resp <- tryCatch(
        {
          httr2::request(files$self[[i]]) |>
            httr2::req_timeout(600) |>
            httr2::req_error(is_error = \(resp) FALSE) |>
            httr2::req_perform()
        },
        error = \(e) NULL
      )
      if (!is.null(resp) && httr2::resp_status(resp) == 200) {
        writeBin(httr2::resp_body_raw(resp), target_path)
        ok <- TRUE
      }
    }
    files$downloaded[i] <- isTRUE(ok)
    paste0("Downloading file ", i, " of ", n) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
  }

  # copy to flat target directory using original filename if available
  for (i in seq_len(nrow(files))) {
    if (isTRUE(files$downloaded[i])) {
      from <- file.path(temppath, files$id[[i]])
      fname <- if (!is.na(files$key[[i]]) && nzchar(files$key[[i]])) files$key[[i]] else files$id[[i]]
      to <- file.path(download_to, fname)
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      file.copy(from, to, overwrite = TRUE)
    }
  }

  # clean up
  unlink(temppath, recursive = TRUE)

  # --- return table ----
  files$folder    <- basename(download_to)
  files$zenodo_id <- as.character(zenodo_id)
  files <- files[, c("folder", "zenodo_id", "id", "key", "size", "checksum", "self", "downloaded")]

  invisible(files)
}
