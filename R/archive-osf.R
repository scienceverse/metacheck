
#' Check OSF API Server Status
#'
#' Check the status of the OSF API server.
#'
#' The OSF API server is down a lot, so it's often good to check it before you run a bunch of OSF functions. When the server is down, it can take several seconds to return an error, so scripts where you are checking many URLs can take a long time before you realise they aren't working.
#'
#' You can only make 100 API requests per hour, unless you authorise your requests, when you can make 10K requests per day. The osf functions in metacheck often make several requests per URL to get all of the info. You can authorise them by creating an OSF token at https://osf.io/settings/tokens and including the following line in your .Renviron file:
#'
#' OSF_PAT="replace-with-your-token-string"
#'
#' @param osf_api the OSF API to use (e.g., "https://api.osf.io/v2")
#' @param on_error whether to stop, warn, or ignore errors
#'
#' @returns the OSF status
#' @export
#'
#' @examples
#' osf_api_check()
osf_api_check <- function(osf_api = getOption("metacheck.osf.api"),
                          on_error = c("stop", "warn", "ignore")) {
  on_error <- match.arg(on_error)
  status_code <- 0
  status <- tryCatch({
    if (!curl::has_internet()) return("no internet")
    resp <- httr2::request(osf_api) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      # httr2::req_retry(
      #   max_tries = 3,
      #   is_transient = \(resp) httr2::resp_status(resp) == 429
      # ) |>
      httr2::req_perform()
    status_code <- httr2::resp_status(resp)
    httr2::resp_status_desc(resp)
  }, error = \(e) {
    return(e$message)
  })

  if (status_code != 200) {
    logger("osf_api_check", list(error = status, code = status_code))
    msg <- sprintf(
      "The OSF API seems to be having a problem:\nError %d: %s\nCheck %s",
      status_code, status, osf_api
    )
    if (on_error == "warn") {
      warning(msg, call. = FALSE)
    } else if (on_error == "stop") {
      stop(msg, call. = FALSE)
    }
  }

  return(status)
}

#' Find OSF Links in Papers
#'
#' Get all OSF links.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the OSF url in the first (href) column
#' @export
#'
#' @examples
#' osf_links(psychsci)
osf_links <- function(paper) {
  urls <- paper_table(paper, "url")
  urls$href <- gsub("\\s", "", urls$href) # temp fix for urls with spaces
  osf <- grepl("osf\\.io", urls$href, ignore.case = TRUE)
  urls[osf, ]
}


#' Retrieve info from the OSF by ID
#'
#' @param osf_url an OSF ID or URL, or a table containing them
#' @param id_col the index or name of the column that contains OSF IDs or URLs, if id is a table
#' @param recursive whether to retrieve all children
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#' # get info on one OSF node
#' osf_info("pngda")
#'
#' # also get child nodes and files
#' osf_info("https://osf.io/6nt4v", recursive = TRUE)
#' }
osf_info <- function(osf_url, id_col = 1,
                     recursive = FALSE,
                     pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "OSF Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(osf_url)) {
    table <- osf_url
    id_col_name <- colnames(table[id_col])
    raw_osf_urls <- table[[id_col]]
  } else {
    id_col_name <- "osf_url"
    raw_osf_urls <- unique(osf_url) |> stats::na.omit() |> as.character()
    table <- data.frame(osf_url = raw_osf_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    osf_url = raw_osf_urls
  )
  ids$osf_id <- osf_check_id(ids$osf_url)
  ids <- ids[!is.na(ids$osf_id), , drop = FALSE] |> unique()

  valid_ids <- unique(ids$osf_id)

  if (length(valid_ids) == 0) {
    paste0("No valid OSF links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(table)
  }

  # retrieve info for all valid IDs in parallel
  info <- .osf_info(valid_ids, pb = pb) |>
    dplyr::left_join(ids, by = "osf_id")
  if (!"project" %in% colnames(info)) {
    info$project <- rep(NA_character_, nrow(info))
  }

  # reduplicate and add original table info
  by <- stats::setNames("osf_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".osf")
  )

  if (isTRUE(recursive)) {
    paste0("...Main retrieval complete")|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    paste0("Starting retrieval of children...")|>
      list(what = _) |>
      pb$tick(0, tokens = _)

    children <- info
    child_collector <- data.frame()
    urls <- children$children[!is.na(children$children)]
    while (length(urls) > 0) {
      resp <- lapply(urls, osf_get_all_pages) |> dplyr::bind_rows()
      children <- .osf_parse_response(resp)
      # resps <- .batch_query(urls, msg = "OSF Child Info", req_func = .osf_headers)
      # children <- lapply(resps, .osf_parse_response) |> dplyr::bind_rows()
      child_collector <- dplyr::bind_rows(child_collector, children)
      urls <- children$children[!is.na(children$children)]
    }

    # get all new node IDs to search for files
    all_nodes <- dplyr::bind_rows(info, child_collector)
    files <- all_nodes
    urls <- files$files[!is.na(files$files)]
    file_collector <- data.frame()
    while (length(urls) > 0) {
      resp <- lapply(urls, osf_get_all_pages) |> dplyr::bind_rows()
      files <- .osf_parse_response(resp)
      # resps <- .batch_query(urls, msg = "OSF File Info", req_func = .osf_headers)
      #files <- lapply(resps, .osf_parse_response) |> dplyr::bind_rows()
      file_collector <- dplyr::bind_rows(file_collector, files)
      urls <- files$files[!is.na(files$files)]
    }

    data <- list(data, child_collector, file_collector) |>
     dplyr::bind_rows()
  }

  paste0("...OSF retrieval complete!")|>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}



#' Check OSF IDs
#'
#' Check if strings are valid OSF IDs, URLs, or waterbutler IDs. Basically an improved wrapper for `osfr::as_id()` that returns NA for invalid IDs in a vector.
#'
#' @param osf_id a vector of OSF IDs or URLs
#'
#' @returns a vector of valid IDs, with NA in place of invalid IDs
#' @export
#'
#' @examples
#' osf_check_id("pngda")
#' osf_check_id("osf.io/pngda")
#' osf_check_id("https://osf.io/pngda")
#' osf_check_id("https://osf .io/png da") # rogue whitespace
#' osf_check_id("pnda") # invalid
osf_check_id <- function(osf_id) {
  clean_id <- osf_id |>
    gsub("\\s", "", x = _) |>
    tolower()

  sapply(clean_id, \(id) {
    tryCatch(
      {
        # for plain IDs (not URLs), check directly
        if (grepl("^[a-z0-9]{5}(_v\\d+)?$", id)) {
          return(id)
        }
        if (grepl("^[a-z0-9]{5}(_v\\d+)?\\?view_only=.+$", id)) {
          return(id)
        }
        if (nchar(id) == 24 && grepl("^[a-z0-9]+$", id)) {
          return(id)
        }

        # for URLs, parse and extract the path
        parsed <- tryCatch(httr2::url_parse(id), error = \(e) NULL)
        if (is.null(parsed)) stop()

        path <- parsed$path |>
          strsplit("/", fixed = TRUE) |> # fs::path_split() |>
          sapply(utils::tail, 1)

        # All OSF IDs are 5 or 24 characters
        if (grepl("^[a-z0-9]{5}(_v\\d+)?$", path)) {
          if (!is.null(parsed$query$view_only)) {
            path <- paste0(path, "?view_only=", parsed$query$view_only)
          }
          return(path)
        }
        if (nchar(path) == 24) {
          return(path)
        }

        stop()
      },
      error = \(e) {
        # try to extract 5-char ID
        m <- gregexpr("(?<=osf\\.io/)[a-z0-9]{5}(_v\\d+)?[?/]?",
          id,
          perl = TRUE
        )
        id5 <- regmatches(id, m) |> sub("[?/]$", "", x = _)
        if (nchar(id5) %in% c(5, 8, 9)) {
          return(id5)
        }

        # else...
        warning(id, " is not a valid OSF ID",
          call. = FALSE, immediate. = FALSE
        )
        return(NA_character_)
      }
    )
  }, USE.NAMES = FALSE)
}

#' Get OSF GUID Type
#'
#' @param guid the 5-letter GUID
#'
#' @returns the type
#' @export
#'
#' @examples
#' # osf_type("pngda")
osf_type <- function(guid) {
  if (length(guid) > 1) {
    pb <- pb(
      total = length(guid),
      format = "Checking OSF Types [:bar] :current/:total :elapsedfull"
    )
    types <- sapply(guid, \(g) {
      pb$tick()
      osf_type(g)
    })
    return(types)
  }

  osf_api <- getOption("metacheck.osf.api")
  id <- osf_check_id(guid)

  if (is.na(id)) return(NA_character_)

  url <- sprintf(
    "%s/guids/%s/?resolve=false",
    osf_api, id
  )
  info <- osf_get_all_pages(url)

  otype <- info$relationships$referent$links$related$meta$type

  otype %||% NA_character_
}


#' Get All OSF API Query Pages
#'
#' OSF API queries only return up to 10 items per page, so this helper functions checks for extra pages and returns all of them
#'
#' @param url the OSF API URL
#' @param page_end The last page to get
#'
#' @returns a table of the returned data
#' @export
#' @examples
#' # get the 20 newest preprints
#' \dontrun{
#' osf_api <- getOption("metacheck.osf.api")
#' url <- sprintf("%s/preprints/?search=date_created-desc", osf_api)
#' preprints <- osf_get_all_pages(url, 2)
#' }
osf_get_all_pages <- function(url, page_end = Inf) {
  Sys.sleep(osf_delay())

  content <- tryCatch({
    resp <- httr2::request(url) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = \(resp) httr2::resp_status(resp) == 429
      ) |>
      httr2::req_perform()
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  },
  error = function(e) {
    return(NULL)
  })

  next_url <- content$links$`next`
  last_url <- content$links$last

  subdata <- NULL
  if (!is.null(next_url)) {
    m <- gregexpr("(?<=page=)\\d+", next_url, perl = TRUE)
    page <- regmatches(next_url, m)[[1]] |> as.numeric()
    if (length(page) && page <= page_end) {
      subdata <- osf_get_all_pages(next_url, page_end)
    }
  }

  data <- tryCatch({
    dplyr::bind_rows(content$data, subdata)
  }, error = \(e) {
    logger("osf_get_all_pages", list(url = url))
    return(content$data)
  })

  return(data)
}



#' Set the OSF delay
#'
#' Sometimes the OSF gets fussy if you make too many calls, so you can set a delay of a few seconds before each call. Use `osf_delay()` to get or set the OSF delay.
#'
#' @param delay the number of seconds to wait between OSF calls
#'
#' @return NULL
#' @export
#'
#' @examples
#' osf_delay()
osf_delay <- function(delay = NULL) {
  if (is.null(delay)) {
    return(getOption("metacheck.osf.delay"))
  } else if (is.numeric(delay)) {
    options(metacheck.osf.delay = delay)
    invisible(getOption("metacheck.osf.delay"))
  } else {
    stop("set osf_delay with a numeric value for the number of seconds to wait between OSF calls")
  }
}


#' Download all OSF Project Files
#'
#' Creates a directory for the OSF ID and downloads all of the files using a folder structure from the OSF project nodes and file storage structure. Returns (invisibly) a data frame with file info.
#'
#' Some differences may exist because the OSF allows longer file names with characters that may not be allowed on a file system, so these are cleaned up when downloading.
#'
#' You can limit downloads to only files under a specific size (defaults to 10MB) and only a maximum download size (largest files will be omitted until total size is under the limit). Omitted files will be listed as messages in verbose mode, and included in the returned data frame with the downloaded column value set to FALSE.
#'
#' @param osf_id an OSF ID or URL
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL for no restrictions
#' @param max_download_size maximum total size to download
#' @param max_folder_length maximum folder name length (set to make sure paths are <260 character on some Windows OS)
#' @param ignore_folder_structure if TRUE, download all files into a single folder
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info
#' @export
#'
#' @examples
#' \dontrun{
#' osf_file_download("6nt4v")
#' }
osf_file_download <- function(osf_id,
                              download_to = ".",
                              max_file_size = 10,
                              max_download_size = 100,
                              max_folder_length = Inf,
                              ignore_folder_structure = FALSE,
                              pb = NULL) {
  ## error checking ----
  osf_id <- osf_check_id(osf_id) |>
    stats::na.omit() |>
    unique()
  if (length(osf_id) == 0) {
    return(NULL)
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "OSF File Download"))
    on.exit(pb$terminate())
  }

  ## iterate ----
  if (length(osf_id) > 1) {
    paste0(
      "Starting downloads for ", length(osf_id),
      " OSF projects...\n"
    )|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    dl <- lapply(osf_id, function(x) {
      tryCatch(
        {
          osf_file_download(
            x,
            download_to,
            max_file_size,
            max_download_size,
            max_folder_length,
            ignore_folder_structure
          )
        },
        error = function(e) {
          warning(
            x, " resulted in an error:\n  ",
            e$message, "\n"
          )
        }
      )
    }) |>
      do.call(dplyr::bind_rows, args = _)
    paste0(
      "...Completed downloads for ", length(osf_id),
      " OSF projects"
    )|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    # names(dl) <- osf_id
    return(dl)
  }

  ## get files and folders ----
  paste0("Starting retrieval for ", osf_id)|>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(
    osf_info(osf_id, recursive = TRUE, pb = pb)
  )
  cols <- c("osf_id", "name", "provider", "path", "kind", "size", "download_url", "parent", "project") |>
    intersect(names(contents))
  files <- contents[contents$osf_type == "files", cols, drop = FALSE]

  if (nrow(files) == 0) {
    paste0("- ", osf_id, " contained no files")|>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  ## restrict file size ----
  if (!is.null(max_file_size) && max_file_size > 0) {
    too_big_files <- which(files$size > max_file_size * 1024 * 1024)
    if (length(too_big_files) > 0) {
      for (i in too_big_files) {
        paste0(
          "- omitting ", files$name[[i]],
          " (", round(files$size[[i]] / 1024 / 1024, 1), "MB)"
        )|>
          list(what = _) |>
          pb$tick(0, tokens = _)
      }

      files <- files[-too_big_files, ]
    }
  }

  ## restrict total download size ----
  while (sum(files$size, na.rm = TRUE) > max_download_size * 1024 * 1024) {
    max_file <- which(files$size == max(files$size, na.rm = TRUE))

    paste0(
      "- omitting ", files$name[[max_file]],
      " (", round(files$size[[max_file]] / 1024 / 1024, 1), "MB)"
    )|>
      list(what = _) |>
      pb$tick(0, tokens = _)

    files <- files[-max_file, ]
  }

  ## set up download directory (make sure it doesn't overwrite anything)
  # On the OSF you can nest folders and give long folder names, but windows has a 260 character folder name limit.
  # download_to <- fs::path_abs(download_to)
  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, osf_id)
  }
  i <- 0
  while (dir.exists(download_to)) {
    i <- i + 1
    download_to <- download_to |>
      sub("_\\d+$", "", x = _) |>
      paste0("_", i)
  }
  dir.create(download_to, showWarnings = FALSE, recursive = FALSE)
  paste0("- Created directory ", download_to)|>
    list(what = _) |>
    pb$tick(0, tokens = _)

  if (sum(files$kind == "file") > 0) {
    ## download all to temp folder ----
    # temppath <- fs::file_temp()
    temppath <- tempfile()
    on.exit(unlink(temppath, recursive = TRUE))
    dir.create(temppath)

    files_to_download <- which(files$kind == "file")

    # urls <- files$download_url[files_to_download]
    # resps <- .batch_query(urls, msg = "Downloading Files", req_func = .osf_headers)

    # save downloaded content to temp files
    for (j in seq_along(files_to_download)) {
      i <- files_to_download[[j]]
      sprintf("Downloading file %d/%d: %s",
               j, length(files_to_download), files$name[[i]]) |>
        list(what = _) |>
        pb$tick(0, tokens = _)

      url <- files$download_url[i]
      tryCatch({
        resp <- .batch_query(url, msg = NULL, req_func = .osf_headers)[[1]]
        #resp <- resps[[j]]
        if (!inherits(resp, "error") && httr2::resp_status(resp) == 200) {
          writeBin(httr2::resp_body_raw(resp),
                   file.path(temppath, files$osf_id[[i]]))
        }
      },
      error = \(e) {
        logger("osf_file_download", list(error = e$message, url = url))
      })
    }

    "Setting up file structure" |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    ## determine parent folders ----
    parent_folders <- sapply(seq_along(files$osf_id), \(i) {
      item <- files[i, ]
      parents <- data.frame()
      last_parent <- item$project
      while (last_parent != osf_id) {
        next_parent <- contents[contents$osf_id == last_parent, ]
        if (nrow(next_parent) == 0) {
          last_parent <- osf_id
        } else {
          parents <- dplyr::bind_rows(parents, next_parent)
          last_parent <- parents[nrow(parents), "project"]
        }
      }
      base_parent <- contents[contents$osf_id == osf_id, ]
      parents <- dplyr::bind_rows(parents, base_parent)
      #parents <- parents[!parents$path %in% "/", ]

      pf <- rev(parents$name) |>
        # gsub("[^A-za-z0-9_\\-\\.]+", "_", x = _, perl = TRUE) |>
        # gsub("_+", "_", x = _, perl = TRUE) |>
        path_sanitize() |>
        paste(collapse = "/")

      pf
    })

    # workaround for when requesting a folder directly
    folder_in_path <- mapply(\(folder, file) {
      pattern <- sprintf("/%s/", folder)
      regexpr(pattern, file, fixed = TRUE)[[1]]
    }, parent_folders, files$path)
    if (all(folder_in_path == 1)) {
      parent_folders <- ""
    }

    files$save_path <- sprintf("%s%s%s%s", files$provider,
                               ifelse(nzchar(parent_folders), "/", ""),
                               parent_folders,
                               files$path)

    if (max_folder_length < Inf) {
      # deal with dirname and basename not recognising that "/code/" is a directory
      hacky_replace <- "--replace-this--"
      hacky_fp <- ifelse(substring(files$save_path, nchar(files$save_path)) == "/",
                         paste0(files$save_path, hacky_replace),
                         files$save_path)
      fp <- dirname(hacky_fp) |>
        strsplit("/") |>
        lapply(substr, start = 0, stop = max_folder_length) |>
        sapply(paste0, collapse = "/") |>
        paste0("/", basename(hacky_fp)) |>
        gsub(hacky_replace, "", x = _, fixed = TRUE)
      if (any(fp != files$save_path)) {
        warning("Some folder names were truncated to max_folder_length = ", max_folder_length, " characters")
      }
      files$save_path <- fp
    }

    files_to_copy <- which(files$kind == "file")
    if (isTRUE(ignore_folder_structure)) {
      files$save_path[files_to_copy] <- path_sanitize(files$name[files_to_copy], keep_sep = FALSE)
      dupes <- duplicated(files$save_path[files_to_copy])
      files$save_path[files_to_copy][dupes] <-
        paste0(files$osf_id[files_to_copy][dupes], "-",
               files$name[files_to_copy][dupes])
    }

    for (i in files_to_copy) {
      from <- file.path(temppath, files$osf_id[[i]])
      to <- file.path(download_to, files$save_path[[i]])
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      file.copy(from, to)
    }
  } else {
    files_to_copy <- c()
  }

  ## set up return table ----
  contents$folder <- basename(download_to)
  ret <- contents[
    contents$kind %in% "file",
    c("folder", "osf_id", "name", "filetype", "size", "downloads", "provider")
  ]

  if (length(files_to_copy) > 0) {
    copied <- files[files_to_copy, c("osf_id", "save_path")]
    names(copied)[[2]] <- "path"
    copied$downloaded <- TRUE
    ret <- dplyr::left_join(ret, copied, by = "osf_id")
    ret$downloaded <- ifelse(ret$downloaded %in% TRUE, TRUE, FALSE)
  } else {
    ret$downloaded <- FALSE
  }

  sprintf("%d files downloaded!", sum(ret$downloaded)) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  invisible(ret)
}


#' Get A list of preprints from the OSF
#'
#' @param provider a vector of the preprint providers, e.g. psyarxiv, socarxiv, edarxiv (see <https://osf.io/preprints/discover>)
#' @param date_created a single date or a vector of two date (min and max)
#' @param date_modified a single date or a vector of two date (min and max)
#' @param page_start the first page of 10 entries
#' @param page_end the last page of 10 entires to read
#'
#' @returns a table of preprint info
#' @export
#' @examples
#' \dontrun{
#' dc <- c("2025-09-01", "2025-10-01")
#' pp <- osf_preprint_list("psyarxiv", date_created = dc)
#' files <- pp$primary_file
#' }
osf_preprint_list <- function(provider = NULL,
                              date_created = NULL,
                              date_modified = NULL,
                              # is_published = NULL, # can only access own unpublished works
                              page_start = 1,
                              page_end = page_start) {
  filters <- paste0("page=", page_start)

  if (!is.null(provider)) {
    f <- paste0(provider, collapse = ",") |>
      paste0("filter[provider]=", x = _)
    filters <- c(filters, f)
  }

  if (!is.null(date_created)) {
    if (length(date_created) == 1) {
      f <- paste0("filter[date_created]=", date_created)
      filters <- c(filters, f)
    } else if (length(date_created) == 2) {
      gte <- paste0("filter[date_created][gte]=", min(date_created))
      lte <- paste0("filter[date_created][lte]=", max(date_created))
      filters <- c(filters, gte, lte)
    }
  }

  if (!is.null(date_modified)) {
    if (length(date_modified) == 1) {
      f <- paste0("filter[date_modified]=", date_modified)
      filters <- c(filters, f)
    } else if (length(date_modified) == 2) {
      gte <- paste0("filter[date_modified][gte]=", min(date_modified))
      lte <- paste0("filter[date_modified][lte]=", max(date_modified))
      filters <- c(filters, gte, lte)
    }
  }

  # if (!is.null(is_published)) {
  #   val <- ifelse(is_published == TRUE || is_published == "true",
  #                 "true", "false")
  #   f <- paste0("filter[is_published]=", val)
  #   filters <- c(filters, f)
  # }

  url <- paste(filters, collapse = "&") |>
    paste0(getOption("metacheck.osf.api"), "/preprints/", "?", x = _)

  pp <- osf_get_all_pages(url, page_end = page_end)

  .osf_preprint_data(pp)
}
