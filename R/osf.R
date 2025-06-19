#' OSF Headers
#'
#' @returns a header list
#' @export
#' @keywords internal
osf_headers <- function() {
  headers <- list(`User-Agent` = "Papercheck")
  osf_pat <- Sys.getenv("OSF_PAT")
  if (!is.null(osf_pat)) {
    headers$Authorization <- sprintf("Bearer %s", osf_pat)
  }
  headers$`Accept-Header` <- "application/vnd.api+json"

  return(headers)
}

#' Find OSF Links in Papers
#'
#' OSF links can be tricky to find in PDFs, since they can insert spaces in odd places, and view-only links that contain a ? are often interpreted as being split across sentences. This function is our best attempt at catching and fixing them all.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the OSF url in the first (text) column
#' @export
#'
#' @examples
#' osf_links(psychsci)
osf_links <- function(paper) {
  # get OSF links
  OSF_RGX <- "\\bosf\\s*\\.\\s*io\\s*/\\s*[a-z0-9]{5}\\s*/?\\s*\\??\\b"
  found_osf <- search_text(paper, OSF_RGX, return = "match")

  #get ? links (often in next sentence)
  VO_RGX <- paste0(
    "\\bosf\\s*\\.\\s*io\\s*/", # osf.io
    "\\s*[a-z0-9]{5}", # 5-letter code
    "\\s*/\\s*\\?\\s*view_only\\s*=\\s*[0-9a-f]+" #vo-link
  )

  has_quest <- grepl("\\?", found_osf$text)
  expanded_vo <- found_osf[has_quest, , drop = FALSE] |>
    expand_text(paper, plus = 1)

  expanded_vo$old_text <- expanded_vo$text
  expanded_vo$text <- expanded_vo$expanded

  found_vo <- expanded_vo |>
    search_text(VO_RGX, return = "match")

  found_vo$expanded <- found_vo$text
  found_vo$text <- found_vo$old_text
  found_vo$old_text <- NULL

  # combine
  all_osf <- dplyr::left_join(found_osf, found_vo,
                              by = names(found_osf))
  all_osf$text = ifelse(is.na(all_osf$expanded),
                        all_osf$text, all_osf$expanded)
  all_osf$expanded <- NULL

  return(all_osf)
}


#' Check OSF API Server Status
#'
#' Check the status of the OSF API server.
#'
#' The OSF API server is down a lot, so it's often good to check it before you run a bunch of OSF functions. When the server is down, it can take several seconds to return an error, so scripts where you are checking many URLs can take a long time before you realise they aren't working.
#'
#' You can only make 100 API requests per hour, unless you authorise your requests, when you can make 10K requests per day. The osf functions in papercheck often make several requests per URL to get all of the info. You can authorise them by creating an OSF token at https://osf.io/settings/tokens and including the following line in your .Renviron file:
#'
#' OSF_PAT="replace-with-your-token-string"
#'
#' @param osf_api the OSF API to use (e.g., "https://api.osf.io/v2")
#'
#' @returns the OSF status
#' @export
#'
osf_api_check <- function(osf_api = getOption("papercheck.osf.api")) {
  h <- httr::GET(osf_api, osf_headers())
  osf_api_calls_inc()
  status <- dplyr::case_match(
    h$status_code,
    200 ~ "ok",
    204 ~ "no content",
    400 ~ "bad request",
    403 ~ "forbidden",
    404 ~ "not found",
    405 ~ "method not allowed",
    409 ~ "conflict",
    410 ~ "gone",
    429 ~ "too many requests",
    500:599 ~ "server error",
    .default = "unknown"
  )

  return(status)
}


#' Retrieve info from the OSF by ID
#'
#' @param osf_url an OSF ID or URL, or a table containing them
#' @param id_col the index or name of the column that contains OSF IDs or URLs, if id is a table
#' @param recursive whether to retrieve all children
#' @param find_project find the top-level project associated with a file (adds 1+ API calls)
#'
#' @returns a data frame of information
#' @export
osf_retrieve <- function(osf_url, id_col = 1,
                         recursive = FALSE,
                         find_project = FALSE) {
  api_check <- osf_api_check()
  if (api_check != "ok") {
    stop("The OSF API seems to be having a problem: ", api_check,
        "\nCheck ", getOption("papercheck.osf.api"))
  }

  # handle list of links
  if (is.data.frame(osf_url)) {
    table <- osf_url
    id_col_name <- colnames(table[id_col])
    raw_osf_urls <- table[[id_col]]
  } else {
    id_col_name <- "osf_url"
    raw_osf_urls <- unique(osf_url) |> stats::na.omit()
    table <- data.frame(osf_url = raw_osf_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    osf_url = raw_osf_urls
  )
  ids$osf_id <- osf_check_id(raw_osf_urls)
  ids <- ids[!is.na(ids$osf_id), , drop = FALSE] |> unique()

  valid_ids <- unique(ids$osf_id)

  # iterate over valid IDs
  message("Starting OSF retrieval for ", length(valid_ids), " files...")

  id_info <- vector("list", length(valid_ids))
  too_many <- FALSE
  i = 0
  while (!too_many & i < length(valid_ids)) {
    i = i + 1
    oi <- osf_info(valid_ids[[i]])
    if (oi$osf_type == "too many requests") break
    id_info[[i]] <- oi
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "osf_id")

  # reduplicate and add original table info
  by <- stats::setNames("osf_url", id_col_name)
  data <- dplyr::left_join(table, info, by = by,
                           suffix = c("", ".osf"))

  if (isTRUE(recursive)) {
    message("...Main retrieval complete")
    message("Starting retrieval of children...")

    children <- info
    child_collector <- data.frame()

    while(nrow(children) > 0) {
      node_ids <- children[children$osf_type == "nodes", "osf_id"]

      children <- lapply(node_ids, osf_children) |>
        do.call(dplyr::bind_rows, args = _)

      child_collector <- dplyr::bind_rows(child_collector, children)
    }

    # get all new node IDs to search for files
    all_nodes <- dplyr::bind_rows(info, child_collector)
    node_type <- all_nodes$osf_type == "nodes"
    node_ids <- all_nodes[node_type, ]$osf_id |> unique()

    files <- lapply(node_ids, osf_files) |>
      do.call(dplyr::bind_rows, args = _)

    # TODO: handle nested folders
    folders <- files$osf_id[files$kind == "folder"]
    subfiles <- lapply(folders, osf_files) |>
      do.call(dplyr::bind_rows, args = _)

    data <- list(data, child_collector, files, subfiles) |>
      do.call(dplyr::bind_rows, args = _)
  }

  # get top-level project ----
  if (find_project) {
    parents <- data.frame(
      parent = unique(data$parent) |> stats::na.omit()
    )
    parents$project <- sapply(parents$parent, osf_parent_project)

    if (nrow(parents) == 0) {
      data$project <- NA_character_
    } else {
      data <- dplyr::left_join(data, parents, by = "parent")
    }

    is_project <- sapply(data$osf_type, identical, "nodes") & is.na(data$parent)
    data$project[is_project] <- data$osf_id[is_project]
  }

  message("...OSF retrieval complete!")

  return(data)
}


#' Retrieve info from the OSF by ID
#'
#' @param osf_id an OSF ID or URL
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
osf_info <- function(osf_id) {
  message("* Retrieving info from ", osf_id, "...")
  osf_api <- getOption("papercheck.osf.api")

  # double-check ID
  valid_id <- osf_check_id(osf_id)

  # handle invalid ID gracefully ----
  if (is.na(valid_id)) {
    obj <- data.frame(
      osf_id = osf_id,
      osf_type = "invalid"
    )
    return(obj)
  }

  Sys.sleep(osf_delay())

  # check for all osf_types ----
  osf_types <- c("nodes",
                 "files",
                 "preprints",
                 "registrations")
                 #"users")
  for (osf_type in osf_types) {
    warning <- NULL
    content <- tryCatch({
      url <- sprintf("%s/%s/%s", osf_api, osf_type, valid_id)
      node_get <- httr::GET(url, osf_headers())
      osf_api_calls_inc()
      if (node_get$status_code == 200) {
        jsonlite::fromJSON(rawToChar(node_get$content))
      } else if (node_get$status_code == 404) {
        NULL
      } else {
        warning <- dplyr::case_match(
          node_get$status_code,
          200 ~ "ok",
          204 ~ "no content",
          400 ~ "bad request",
          401 ~ "unauthorized",
          403 ~ "forbidden",
          404 ~ "not found",
          405 ~ "method not allowed",
          409 ~ "conflict",
          410 ~ "gone",
          429 ~ "too many requests",
          500:599 ~ "server error",
          .default = paste("error", node_get$status_code)
        )

        NULL
      }
    }, error = function(e) return(NULL) )

    # deal with warnings ----
    if (identical(warning, "unauthorized")) {
      return(data.frame(
        osf_id = valid_id,
        osf_type = "private",
        public = FALSE
      ))
    } else if (!is.null(warning)) {
      osf_type <- warning
      break
    }

    if (!is.null(content) & is.null(content$errors)) {
      data <- content$data
      break
    } else {
      osf_type <- "unknown"
    }
  }

  # handle data ----
  if (osf_type == "nodes") return(osf_node_data(data))
  if (osf_type == "files") return (osf_file_data(data))
  if (osf_type == "preprints") return(osf_preprint_data(data))
  if (osf_type == "registrations") return(osf_reg_data(data))
  if (osf_type == "users") return(osf_user_data(data))

  if (osf_type == "too many requests") {
    warning("Too many requests", call. = FALSE)
    obj <- data.frame(
      osf_id = osf_id,
      osf_type = osf_type
    )
    return(obj)
  }

  # unfound but valid ID
  warning(osf_id, " could not be found", call. = FALSE)
  obj <- data.frame(
    osf_id = osf_id,
    osf_type = "unfound"
  )
  return(obj)
}

#' Structure OSF Node Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
osf_node_data <- function(data) {
  if (is.null(data) | length(data) == 0) return(data.frame())

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$title %||% NA_character_,
    description = att$description %||% NA_character_,
    osf_type = data$type,
    public = att$public %||% NA,
    category = att$category %||% NA_character_,
    registration = att$registration %||% NA,
    preprint = att$preprint %||% NA,
    parent = data$relationships$parent$data$id %||% NA_character_
  )

  return(obj)
}

#' Structure OSF File Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
osf_file_data <- function(data) {
  if (is.null(data) | length(data) == 0) return(data.frame())

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$name,
    description = att$description %||% NA_character_,
    osf_type = data$type,
    kind = att$kind %||% NA_character_,
    public = att$public %||% NA,
    category = att$category %||% NA_character_,
    size = att$size %||% NA_integer_,
    downloads = att$extra$downloads %||% NA_integer_,
    parent = data$relationships$target$data$id %||% NA_character_
  )

  filetype <- data.frame(
    id = seq_along(obj$name),
    ext = strsplit(obj$name, "\\.") |>
      sapply(\(x) x[[length(x)]]) |>
      tolower()
  ) |>
    dplyr::left_join(file_types, by = "ext") |>
    dplyr::summarise(type = paste(.data$type, collapse = ";"),
                     .by = c("id", "ext"))
  obj$filetype <- filetype$type

  return(obj)
}

#' Structure OSF Preprint Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
osf_preprint_data <- function(data) {
  if (is.null(data) | length(data) == 0) return(data.frame())

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$title,
    description = att$description %||% NA_character_,
    osf_type = data$type,
    public = att$public %||% NA,
    doi = att$doi %||% NA_character_,
    version = att$version %||% NA_integer_,
    parent = data$relationships$node$data$id %||% NA_character_
  )

  return(obj)
}

#' Structure OSF Registration Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
osf_reg_data <- function(data) {
  if (is.null(data) | length(data) == 0) return(data.frame())

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$title %||% NA_character_,
    osf_type = data$type,
    category = "registration",
    registration = att$registration %||% NA,
    preprint = data$attributes$preprint %||% NA,
    parent = data$relationships$registered_from$data$id %||% NA_character_
  )

  return(obj)
}

#' Structure OSF User Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
osf_user_data <- function(data) {
  if (is.null(data) | length(data) == 0) return(data.frame())

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$full_name %||% NA_character_,
    osf_type = data$type,
    public = TRUE,
    orcid = att$social$orcid %||% NA_character_
  )

  return(obj)
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
#' osf_check_id("https://osf.io/pngda/View")
#' osf_check_id("https://osf .io/png da") # rogue whitespace
#' osf_check_id("pnda") # invalid
osf_check_id <- function(osf_id) {
  # osfr::as_id fails completely if one id is invalid
  # so use sapply
  sapply(osf_id, \(id) {
    tryCatch({
      id |>
        gsub("\\s", "", x = _) |>
        osfr::as_id() |>
        as.character()
    },
    error = \(e) {
      warning(id, " is not a valid OSF ID",
             call. = FALSE, immediate. = FALSE)
      return(NA_character_)
    })
  }, USE.NAMES = FALSE)
}

#' Get All OSF API Query Pages
#'
#' OSF API queries only return up to 10 items per page, so this helper functions checks for extra pages and returns all of them
#'
#' @param url the OSF API URL
#'
#' @returns a table of the returned data
#' @export
#' @keywords internal
osf_get_all_pages <- function(url) {
  Sys.sleep(osf_delay())

  content <- tryCatch({
    node_get <- httr::GET(url, osf_headers())
    osf_api_calls_inc()
    jsonlite::fromJSON(rawToChar(node_get$content))
  }, error = function(e) return(NULL))

  next_url <- content$links$`next`

  if (!is.null(next_url)) {
    subdata <- osf_get_all_pages(next_url)
  } else {
    subdata <- NULL
  }

  if (!is.null(subdata)) {
    data <- dplyr::bind_rows(content$data, subdata)
  } else {
    data <- content$data
  }

  return(data)
}

#' List Files in an OSF Component
#'
#' @param osf_id an OSF ID
#'
#' @returns a data frame with file info
#' @export
#' @keywords internal
osf_files <- function(osf_id) {
  osf_api <- getOption("papercheck.osf.api")
  node_id <- osf_check_id(osf_id)

  message("* Retrieving files for ", node_id, "...")

  if (nchar(node_id) == 5) {
    url <- sprintf("%s/nodes/%s/files/osfstorage/", osf_api, node_id)
  } else {
    url <- sprintf("%s/files/%s/", osf_api, node_id)
    filedata <- osf_get_all_pages(url)
    url <- filedata$relationships$files$links$related$href
  }

  data <- osf_get_all_pages(url)
  obj <- osf_file_data(data)

  return(obj)
}

#' List Children of an OSF Component
#'
#' @param osf_id an OSF ID
#'
#' @returns a data frame with child info
#' @export
#' @keywords internal
osf_children <- function(osf_id) {
  osf_api <- getOption("papercheck.osf.api")
  node_id <- osf_check_id(osf_id)

  message("* Retrieving children for ", node_id, "...")

  url <- sprintf("%s/nodes/%s/children/",
                 osf_api, node_id)
  data <- osf_get_all_pages(url)
  obj <- osf_node_data(data)

  return(obj)
}


#' Summarize Directory Contents
#'
#' @param contents a table with columns name, path such as from `osf_contents()`
#'
#' @returns the table with new columns is_readme, is_data, is_code, is_codebook, and best_guess
#' @export
#'
summarize_contents <- function(contents) {
  contents$is_readme <- grepl("read[ _-]?me", contents$name, ignore.case = TRUE)

  contents$is_data <- dplyr::case_when(
    contents$category == "data" ~ TRUE,
    contents$filetype == "data" ~ TRUE,
    grepl("data", contents$name, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )
  contents$is_code <- dplyr::case_when(
    contents$category == "code" ~ TRUE,
    contents$filetype == "code" ~ TRUE,
    grepl("code|script", contents$name, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  contents$is_codebook <- dplyr::case_when(
    contents$category == "codebook" ~ TRUE,
    grepl("code[ _]?book", contents$name, ignore.case = TRUE) ~ TRUE,
    grepl("data[ _]?dict", contents$name, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  contents$best_guess <- dplyr::case_when(
    contents$is_readme ~ "readme",
    contents$is_code ~ "code",
    contents$is_data ~ "data",
    contents$is_codebook ~ "codebook",
    .default = ""
  )

  return(contents)
}


#' Get OSF Parent Project
#'
#' @param osf_id an OSF ID
#'
#' @returns the ID of the parent project
#' @export
#' @keywords internal
osf_parent_project <- function(osf_id) {
  valid_id <- osf_check_id(osf_id)
  if (is.na(valid_id)) return(NA_character_)

  # TODO: make this more efficient my just getting the parent
  obj <- suppressMessages( osf_info(valid_id) )

  if (is.null(obj$parent) || is.na(obj$parent)) return(osf_id)

  parent <- osf_parent_project(obj$parent)

  return(parent)
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
    return(getOption("papercheck.osf.delay"))
  } else if (is.numeric(delay)) {
    options(papercheck.osf.delay = delay)
    invisible(getOption("papercheck.osf.delay"))
  } else {
    stop("set osf_delay with a numeric value for the number of seconds to wait between OSF calls")
  }
}

#' Increment OSF API Call Count
#'
#' @returns NULL
#' @export
#' @keywords internal
osf_api_calls_inc <- function() {
  n <- getOption("papercheck.osf.api.calls") |> as.integer()
  options(papercheck.osf.api.calls = n + 1)
}

#' Get/set the OSF API Call Count
#'
#' @param calls the number of OSF calls made since the last reset
#'
#' @return NULL
#' @export
#'
#' @examples
#' osf_api_calls()
osf_api_calls <- function(calls = NULL) {
  if (is.null(calls)) {
    return(getOption("papercheck.osf.api.calls"))
  } else if (is.numeric(calls)) {
    options(papercheck.osf.api.calls = calls)
    invisible(getOption("papercheck.osf.api.calls"))
  } else {
    stop("set osf_api_calls with a numeric value (usually reset to 0)")
  }
}
