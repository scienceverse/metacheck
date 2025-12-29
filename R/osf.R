#' OSF Headers
#'
#' @returns a header list
#' @export
#' @keywords internal
osf_headers <- function() {
  headers <- list(`User-Agent` = "metacheck")
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
#' You can only make 100 API requests per hour, unless you authorise your requests, when you can make 10K requests per day. The osf functions in metacheck often make several requests per URL to get all of the info. You can authorise them by creating an OSF token at https://osf.io/settings/tokens and including the following line in your .Renviron file:
#'
#' OSF_PAT="replace-with-your-token-string"
#'
#' @param osf_api the OSF API to use (e.g., "https://api.osf.io/v2")
#'
#' @returns the OSF status
#' @export
#'
#' @examples
#' osf_api_check()
osf_api_check <- function(osf_api = getOption("metacheck.osf.api")) {
  if (!curl::has_internet()) {
    return("no internet")
  }
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
#' @param find_project DEPRECATED always TRUE now - find the top-level project associated with a file (adds 1+ API calls)
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \donttest{
#'   # get info on one OSF node
#'   osf_retrieve("pngda")
#'
#'   # also get child nodes and files, and parent project
#'   osf_retrieve("https://osf.io/6nt4v", TRUE, TRUE)
#' }
osf_retrieve <- function(osf_url, id_col = 1,
                         recursive = FALSE,
                         find_project = FALSE) {
  api_check <- osf_api_check()
  if (api_check != "ok") {
    stop("The OSF API seems to be having a problem: ", api_check,
        "\nCheck ", getOption("metacheck.osf.api"))
  }

  # handle list of links
  if (is.data.frame(osf_url)) {
    table <- osf_url
    id_col_name <- colnames(table[id_col])
    raw_osf_urls <- table[[id_col]]
  } else {
    id_col_name <- "osf_url"
    raw_osf_urls <- unique(osf_url) |>
      stats::na.omit() |> as.character()
    table <- data.frame(osf_url = raw_osf_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    osf_url = raw_osf_urls
  )
  ids$osf_id <- osf_check_id(raw_osf_urls)
  ids <- ids[!is.na(ids$osf_id), , drop = FALSE] |> unique()

  valid_ids <- unique(ids$osf_id)

  if (length(valid_ids) == 0) {
    message("No valid OSF links")
    return(table)
  }

  # iterate over valid IDs
  message("Starting OSF retrieval for ", length(valid_ids),
          " URL", ifelse(length(valid_ids) == 1, "", "s"), "...")

  id_info <- vector("list", length(valid_ids))
  too_many <- FALSE
  i = 0
  while (!too_many & i < length(valid_ids)) {
    i = i + 1
    oi <- osf_info(valid_ids[[i]])
    if ("too many requests" %in% oi$osf_type) too_many <- TRUE
    id_info[[i]] <- oi
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "osf_id")
  if (!"project" %in% colnames(info)) {
    info$project <- rep(NA_character_, nrow(info))
  }

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

      children <- osf_children(node_ids)

      child_collector <- dplyr::bind_rows(child_collector, children)
    }

    # get all new node IDs to search for files
    all_nodes <- dplyr::bind_rows(info, child_collector)
    node_type <- all_nodes$osf_type == "nodes"
    if ("kind" %in% names(all_nodes)) {
      node_type <-  node_type | all_nodes$kind == "folder"
    }
    node_type <- sapply(node_type, isTRUE)

    folders <- all_nodes[node_type, c("osf_id", "project")] |> unique()

    file_collector <- data.frame()
    while(nrow(folders) > 0) {
      subfiles <- mapply(\(f, proj_id) {
          dat <- osf_files(f)
          dat$project <- rep(proj_id, nrow(dat))
          dat
        }, folders$osf_id, folders$project, SIMPLIFY = FALSE) |>
        do.call(dplyr::bind_rows, args = _)
      if (nrow(subfiles) > 0 && "kind" %in% names(subfiles)) {
        folders <- subfiles[subfiles$kind == "folder", c("osf_id", "project")]
        #folders <- folders[!is.na(folders$osf_id), ]
      } else {
        folders <- data.frame()
      }
      file_collector <- dplyr::bind_rows(file_collector, subfiles)
    }

    data <- list(data, child_collector, file_collector) |>
      do.call(dplyr::bind_rows, args = _)
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
  osf_api <- getOption("metacheck.osf.api")

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
  if (nchar(valid_id) == 5) {
    osf_types <- "guids"
  } else {
    # waterbutler IDs are usually files
    osf_types <- c("files",
                   "nodes",
                   "preprints",
                   "registrations",
                   "users")
  }

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
      osf_type <- content$data$type
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
    parent = data$relationships$parent$data$id %||% NA_character_,
    project = data$relationships$root$data$id %||% NA_character_
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

  if (is.null(att$guid)) {
    osf_id <- data$id
  } else {
    osf_id <- ifelse(is.na(att$guid), data$id, att$guid)
  }

  obj <- data.frame(
    osf_id = osf_id,
    #osf_id = data$id,
    name = att$name,
    description = att$description %||% NA_character_,
    osf_type = data$type,
    kind = att$kind %||% NA_character_,
    filetype = NA_character_,
    public = att$public %||% NA,
    category = att$category %||% NA_character_,
    size = att$size %||% NA_integer_,
    downloads = att$extra$downloads %||% NA_integer_,
    download_url = data$links$download %||% NA_character_,
    parent = data$relationships$target$data$id %||%
             data$relationships$parent_folder$data$id %||%
             NA_character_,
    project = data$relationships$root$data$id %||% NA_character_
  )

  # guess file type
  is_file <- obj$kind == "file"
  obj$filetype[is_file] <- filetype(obj$name[is_file])

  folders <- which(obj$kind == "folder")
  if (length(folders) &&
      !is.null(data$relationships$root_folder$data$id)) {
    obj$osf_id[folders] <-
      data$relationships$root_folder$data$id[folders]
  }

  return(obj)
}

#' Get file Type from Extension
#'
#' @param filename the file name
#'
#' @returns a named vector of file types
#' @export
#'
#' @examples
#' filetype("script.R")
filetype <- function(filename) {
  .data <- NULL

  ext <- data.frame(
    id = seq_along(filename),
    ext = strsplit(filename, "\\.") |>
      sapply(\(x) x[[length(x)]]) |>
      tolower()
  )

  add_types <- ext |>
    dplyr::left_join(metacheck::file_types, by = "ext") |>
    dplyr::summarise(type = paste(.data$type, collapse = ";"),
                     .by = c("id", "ext"))

  filetype <- add_types$type
  names(filetype) <- filename

  return(filetype)
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
    #tags = sapply(att$tags, paste, collapse = ";"),
    osf_type = data$type,
    provider = data$relationships$provider$data$id,
    public = att$public %||% NA,
    doi = att$doi %||% NA_character_,
    version = att$version %||% NA_integer_,
    is_published = att$is_published %||% NA,
    date_created = att$date_created %||% NA_character_,
    date_modified = att$date_modified %||% NA_character_,
    parent = data$relationships$node$data$id %||% NA_character_,
    project = data$relationships$root$data$id %||% NA_character_,
    primary_file = data$relationships$primary_file$links$related$href %||% NA_character_
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
    parent = data$relationships$registered_from$data$id %||% NA_character_,
    project = data$relationships$root$data$id %||% NA_character_
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
#' osf_check_id("https://osf .io/png da") # rogue whitespace
#' osf_check_id("pnda") # invalid
osf_check_id <- function(osf_id) {
  clean_id <- osf_id |>
    gsub("\\s", "", x = _) |>
    tolower()

  sapply(clean_id, \(id) {
    tryCatch({
      path <- httr::parse_url(id)$path |>
        fs::path_split() |>
        sapply(utils::tail, 1)

      # All OSF IDs are 5 or 24 characters
      if (grepl("^[a-z0-9]{5}(_v\\d+)?$", path)) return(path)
      if (nchar(path) == 24) return(path)

      stop()
    },
    error = \(e) {
      # try to extract 5-char ID
      m <- gregexpr("(?<=osf\\.io/)[a-z0-9]{5}(_v\\d+)?[?/]?",
                    id, perl = TRUE)
      id5 <- regmatches(id, m) |> sub("[?/]$", "", x = _)
      if (nchar(id5) %in% c(5, 8, 9)) return(id5)

      # else...
      warning(id, " is not a valid OSF ID",
              call. = FALSE, immediate. = FALSE)
      return(NA_character_)
    })
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
    pb <- pb(total = length(guid),
             format = "Checking OSF Types [:bar] :current/:total :elapsedfull")
    types <- sapply(guid, \(g) {
      pb$tick()
      osf_type(g)
    })
    return(types)
  }

  osf_api <- getOption("metacheck.osf.api")
  id <- osf_check_id(guid)
  url <- sprintf("%s/guids/%s/?resolve=false",
                 osf_api, id)
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
#' \donttest{
#' osf_api <- getOption("metacheck.osf.api")
#' url <- sprintf("%s/preprints/?search=date_created-desc", osf_api)
#' preprints <- osf_get_all_pages(url, 2)
#' }
osf_get_all_pages <- function(url, page_end = Inf) {
  Sys.sleep(osf_delay())

  content <- tryCatch({
    node_get <- httr::GET(url, osf_headers())
    osf_api_calls_inc()
    jsonlite::fromJSON(rawToChar(node_get$content))
  }, error = function(e) return(NULL))

  next_url <- content$links$`next`
  last_url <- content$links$last

  # m <- gregexpr("(?<=page=)\\d+", url, perl = TRUE)
  # this_page <- regmatches(url, m)[[1]] |> as.numeric()
  # if (length(this_page) == 0) this_page <- 1

  # if (!is.null(last_url)) {
  #   m <- gregexpr("(?<=page=)\\d+", last_url, perl = TRUE)
  #   end <- regmatches(last_url, m)[[1]] |> as.numeric()
  #
  #   if (end < page_end) { page_end <- end }
  #   message("Retrieving ", this_page, "/", page_end,
  #           " of ", end, " pages")
  # }

  subdata <- NULL
  if (!is.null(next_url)) {
    m <- gregexpr("(?<=page=)\\d+", next_url, perl = TRUE)
    page <- regmatches(next_url, m)[[1]] |> as.numeric()
    if (length(page) && page <= page_end) {
      subdata <- osf_get_all_pages(next_url, page_end)
    }
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
  osf_api <- getOption("metacheck.osf.api")
  node_id <- osf_check_id(osf_id)

  message("* Retrieving files for ", node_id, "...")

  if (nchar(node_id) == 5) {
    url <- sprintf("%s/nodes/%s/files/", osf_api, node_id)
  } else {
    url <- sprintf("%s/files/%s/", osf_api, node_id)
  }

  storage <- osf_get_all_pages(url)
  file_links <- storage$relationships$files$links$related$href

  obj <- lapply(file_links, \(url) {
    data <- osf_get_all_pages(url)
    obj <- osf_file_data(data)
    obj$parent <- rep(osf_id, nrow(obj))

    return(obj)
  }) |>
    do.call(dplyr::bind_rows, args = _)

  return(obj)
}


#' List Children of an OSF Component
#'
#' @param osf_id a vector of OSF IDs
#'
#' @returns a data frame with child info
#' @export
#' @keywords internal
osf_children <- function(osf_id) {
  osf_api <- getOption("metacheck.osf.api")
  node_id <- osf_check_id(osf_id)

  if (length(node_id) == 0) return(data.frame())

  message("* Retrieving children for ",
          paste(node_id, collapse = ", "),
          "...")

  # url <- sprintf("%s/nodes/%s/children/",
  #                osf_api, node_id)
  # data <- osf_get_all_pages(url)
  # obj <- osf_node_data(data)

  url <- sprintf("%s/nodes/?filter[id]=%s&embed=children",
                 osf_api, paste(node_id, collapse = ","))
  data <- osf_get_all_pages(url)
  children <- do.call(dplyr::bind_rows, data$embeds$children$data)
  obj <- osf_node_data(children)

  return(obj)
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

  if (!is.null(obj$project) && !is.na(obj$project)) return(obj$project)
  if (is.null(obj$parent) || is.na(obj$parent)) return(osf_id)

  parent <- osf_parent_project(obj$parent)

  return(parent)
}


#' Summarize Directory Contents
#'
#' @param contents a table with columns name, path such as from `osf_contents()`
#'
#' @returns the table with new column file_category
#' @export
#'
summarize_contents <- function(contents) {
  nm <- contents$name
  cat <- contents$category
  ft <- contents$filetype

  # category is from OSF, so can be: analysis, communication, data, hypothesis, instrumentation, methods and measures, procedure, project, software, other, but mostly uncategorized (NA)

  # hard rules
  sure_class <- dplyr::case_when(
    ft == "stats" ~ "code",
    ft == "data" ~ "data",
    ft == "code" ~ "code",
    grepl("code[ _]?book", nm, ignore.case = TRUE) ~ "codebook",
    grepl("data[ _]?dict", nm, ignore.case = TRUE) ~ "codebook",
  )

  is_readme <- grepl("read[ _-]?me", contents$name, ignore.case = TRUE)

  # data
  is_data <- dplyr::case_when(
    cat == "data" ~ TRUE,
    ft == "data" ~ TRUE,
    grepl("data", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  # code
  is_code <- dplyr::case_when(
    cat == "code" ~ TRUE,
    ft == "code" ~ TRUE,
    grepl("code|script", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  # codebook
  is_codebook <- dplyr::case_when(
    cat == "codebook" ~ TRUE,
    grepl("code[ _]?book", nm, ignore.case = TRUE) ~ TRUE,
    grepl("data[ _]?dict", nm, ignore.case = TRUE) ~ TRUE,
    .default = FALSE
  )

  contents$file_category <- dplyr::case_when(
    !is.na(sure_class) ~ sure_class,
    is_readme ~ "readme",
    is_codebook ~ "codebook",
    # is_code ~ "code",
    # is_data ~ "data",
    .default = NA_character_
  )

  return(contents)
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

#' Increment OSF API Call Count
#'
#' @returns NULL
#' @export
#' @keywords internal
osf_api_calls_inc <- function() {
  n <- getOption("metacheck.osf.api.calls") |> as.integer()
  options(metacheck.osf.api.calls = n + 1)
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
    return(getOption("metacheck.osf.api.calls"))
  } else if (is.numeric(calls)) {
    options(metacheck.osf.api.calls = calls)
    invisible(getOption("metacheck.osf.api.calls"))
  } else {
    stop("set osf_api_calls with a numeric value (usually reset to 0)")
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
#'
#' @returns data frame of file info
#' @export
#'
#' @examples
#' \donttest{
#'   osf_file_download("6nt4v")
#' }
osf_file_download <- function(osf_id,
                              download_to = ".",
                              max_file_size = 10,
                              max_download_size = 100,
                              max_folder_length = Inf,
                              ignore_folder_structure = FALSE) {
  ## error checking ----
  osf_id <- osf_check_id(osf_id) |> stats::na.omit() |> unique()
  if (length(osf_id) == 0) return(NULL)

  ## iterate ----
  if (length(osf_id) > 1) {
    message("Starting downloads for ", length(osf_id),
            " OSF projects...\n")
    dl <- lapply(osf_id, function(x) {
      tryCatch({
        osf_file_download(x,
                          download_to,
                          max_file_size,
                          max_download_size,
                          max_folder_length,
                          ignore_folder_structure)
      }, error = function(e) {
        warning(x, " resulted in an error:\n  ",
                e$message, "\n")
      })
    }) |>
      do.call(dplyr::bind_rows, args = _)
    message("...Completed downloads for ", length(osf_id),
            " OSF projects")
    #names(dl) <- osf_id
    return(dl)
  }

  ## get files and folders ----
  message("Starting retrieval for ", osf_id)
  contents <- suppressMessages(
    osf_retrieve(osf_id, recursive = TRUE)
  )
  cols <- c("osf_id", "name", "parent", "kind", "size", "download_url") |>
    intersect(names(contents))
  files <- contents[contents$osf_type == "files", cols, drop = FALSE]

  if (nrow(files) == 0) {
    message("- ", osf_id, " contained no files")
    return(NULL)
  }

  ## restrict file size ----
  if (!is.null(max_file_size) && max_file_size > 0) {
    too_big_files <- which(files$size > max_file_size*1024*1024)
    if (length(too_big_files) > 0) {
      for (i in too_big_files) {
        message("- omitting ", files$name[[i]],
                " (", round(files$size[[i]]/1024/1024, 1), "MB)")
      }

      files <- files[-too_big_files, ]
    }
  }

  ## restrict total download size ----
  while (sum(files$size, na.rm = TRUE) > max_download_size*1024*1024) {
    max_file <- which(files$size == max(files$size, na.rm = TRUE))

    message("- omitting ", files$name[[max_file]],
            " (", round(files$size[[max_file]]/1024/1024, 1), "MB)")

    files <- files[-max_file, ]
  }

  ## set up download directory (make sure it doesn't overwrite anything)
  # On the OSF you can nest folders and give long folder names, but windows has a 260 character folder name limit.
  download_to <- fs::path_abs(download_to)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, osf_id)
  }
  i = 0
  while (dir.exists(download_to)) {
    i = i + 1
    download_to <- download_to |>
      sub("_\\d+$", "", x = _) |>
      paste0("_", i)
  }
  dir.create(download_to, showWarnings = FALSE, recursive = FALSE)
  message("- Created directory ", download_to)

  if (sum(files$kind == "file") > 0) {
    ## download all to temp folder ----
    temppath <- fs::file_temp()
    dir.create(temppath)

    files_to_download <- which(files$kind == "file")
    pb <- pb(total = length(files_to_download),
             format = "Downloading files [:bar] :current/:total :elapsedfull")

    for (i in files_to_download) {
      tryCatch({
        write_loc <- file.path(temppath, files$osf_id[[i]]) |>
          httr::write_disk(overwrite = TRUE)
        response <- httr::GET(files$download_url[[i]], write_loc)

        # TODO: deal with errors
      }, error = \(e) {})
      pb$tick()
    }

    trunc_warning <- FALSE

    if (isTRUE(ignore_folder_structure)) {
      files$path <- fs::path_sanitize(files$name)

      while(duplicated(files$path) |> any()) {
        # get all duplicated paths
        d <- which(files$path %in% files$path[duplicated(files$path)])

        dupes <- files$path[d]
        parents <- files$parent[d]
        ext <-  fs::path_ext(dupes)
        ext[ext != ""] <- paste0(".", ext[ext != ""])
        base <- fs::path_ext_remove(dupes)
        files$path[d] <- paste0(base, "_", parents, ext)
      }
    } else {
      ## structure files into download_to ----
      files$path <- ""

      for (i in seq_along(files$osf_id)) {
        item <- files[i, ]
        if (is.na(item$parent) || item$parent == osf_id) {
          files$path[[i]] <- item$name
        } else {
          parents <- data.frame()
          last_parent <- item$parent
          while (last_parent != osf_id) {
            next_parent <- contents[contents$osf_id == last_parent, ]
            if (nrow(next_parent) == 0) {
              last_parent <- osf_id
            } else {
              parents <- dplyr::bind_rows(parents, next_parent)
              last_parent <- parents[nrow(parents), "parent"]
            }
          }

          # make sure file components <= max_folder_length
          # TODO: handle windows limitations more gracefully
          maxlen <- sapply(nchar(parents$name), min, max_folder_length)
          newnames <- substr(parents$name, 1, maxlen)
          if (!all(newnames == parents$name)) {
            trunc_warning <- TRUE
            parents$name <- newnames
          }

          files$path[[i]] <- rev(parents$name) |>
            fs::path_sanitize() |>
            paste(collapse = "/") |>
            paste0("/", item$name)
        }
      }

      # add base folder name
      basename <- contents$name[[1]] |> fs::path_sanitize()
      files$path <- file.path(basename, files$path)
    }

    if (trunc_warning) {
      warning("Some folder names were truncated to max_folder_length = ", max_folder_length, " characters")
    }

    files_to_copy <- which(files$kind == "file")
    for (i in files_to_copy) {
      from <- file.path(temppath, files$osf_id[[i]])
      to <- file.path(download_to, files$path[[i]])
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      file.copy(from, to)
    }

    ## clean up temp dir
    unlink(temppath, recursive = TRUE)
  } else {
    files_to_copy <- c()
  }

  ## set up return table ----
  contents$folder <- basename(download_to)
  ret <- contents[contents$kind %in% "file",
                  c("folder", "osf_id", "name", "filetype", "size", "downloads")]

  if (length(files_to_copy) > 0) {
    copied <- files[files_to_copy, c("osf_id", "path")]
    copied$downloaded <- TRUE
    ret <- dplyr::left_join(ret, copied, by = "osf_id")
    ret$downloaded <- ifelse(ret$downloaded %in% TRUE, TRUE, FALSE)
  } else {
    ret$downloaded <- FALSE
  }

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
#' \donttest{
#'   dc <- c("2025-09-01", "2025-10-01")
#'   pp <- osf_preprint_list("psyarxiv", date_created = dc)
#'   files <- pp$primary_file
#' }
osf_preprint_list <- function(provider = NULL,
                  date_created = NULL,
                  date_modified = NULL,
                  #is_published = NULL, # can only access own unpublished works
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

  osf_preprint_data(pp)
}
