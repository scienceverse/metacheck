#' OSF Headers
#'
#' Adds OSF auth and accept headers to an httr2 request.
#'
#' @param req an httr2 request object
#'
#' @returns the modified request
#' @export
#' @keywords internal
.osf_headers <- function(req) {
  req <- req |>
    httr2::req_headers(
      `User-Agent` = "metacheck",
      Accept = "application/vnd.api+json"
    )
  osf_pat <- Sys.getenv("OSF_PAT")
  if (!nzchar(osf_pat)) {
    return(req)
  }

  # PAT exists, check validation
  req_pat <- req |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", osf_pat))

  return(req_pat)
}

#' OSF PAT Validation
#'
#' Checks for validity of the OSF PAT and unsets it if needed.
#'
#' @param osf_pat the OSF PAT (read from renviron by default)
#'
#' @returns logical (TRUE if OSF_PAT is set and valid)
#' @export
#' @keywords internal
.osf_pat_validate <- function(osf_pat = Sys.getenv("OSF_PAT")) {
  if (osf_pat == "") return(FALSE)
  if (!online("api.osf.io")) return(FALSE)

  # check a publicly available preprint (Nosek badges)
  probe <- "https://api.osf.io/v2/preprints/khbvy/"

  req <- httr2::request(probe) |>
    httr2::req_error(is_error = \(r) FALSE)  |>
    # httr2::req_timeout(5) |>
    httr2::req_headers(
      `User-Agent` = "metacheck",
      Accept = "application/vnd.api+json"
    )

  # try anonymously
  sc_anon <- tryCatch(
    req |> httr2::req_perform() |> httr2::resp_status(),
    error = \(e) NA
  )

  # public file not available - something wrong
  if (!sc_anon %in% 200L) {
    warning(
      "The OSF_PAT could not be validated because the test file is not avilable; the OSF may be down.",
      call. = FALSE
    )
    return(FALSE)
  }

  # try with PAT authorisation
  sc_auth <- tryCatch(
    req |>
      httr2::req_headers(Authorization = sprintf("Bearer %s", osf_pat)) |>
      httr2::req_perform() |>
      httr2::resp_status(),
    error = \(e) NA
  )

  # authorised access success
  if (sc_auth %in% 200L) {
    return(TRUE)
  }

  if (sc_auth %in% c(401L, 403L)) {
    # unset PAT if invalid
    warning(
      "The current OSF_PAT blocks access to public files. ",
      "Clearing OSF_PAT for this session. ",
      "Update or remove it in .Renviron.",
      call. = FALSE
    )
    Sys.setenv(OSF_PAT = "")
  }

  return(FALSE)
}

#' Retrieve info from the OSF by ID
#'
#' @param osf_id an vector of OSF IDs or URLs
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.osf_info <- function(osf_id, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  valid_ids <- osf_check_id(osf_id)

  if (all(is.na(valid_ids))) {
    return(data.frame(
      osf_id = osf_id,
      osf_type = "invalid"
    ))
  }

  osf_api <- getOption("metacheck.osf.api")

  # Separate 5-char GUIDs from 24-char waterbutler IDs
  is_guid <- nchar(valid_ids) %in% 5
  is_vo <- grepl("/?\\?\\s*view_only=", valid_ids)
  # id_vo <- strsplit(valid_ids[is_vo], "/?\\?\\s*view_only=")
  # vo_ids <- sapply(id_vo, `[[`, 1)
  # vo_tokens <- sapply(id_vo, `[[`, 2)
  guid_ids <- valid_ids[is_guid | is_vo]
  wb_ids <- valid_ids[!is_guid & !is_vo & !is.na(valid_ids)]

  urls <- c(
    sprintf("%s/guids/%s", osf_api, guid_ids),
    #sprintf("%s/nodes/%s/?view_only=%s", osf_api, vo_ids, vo_tokens),
    sprintf("%s/files/%s", osf_api, wb_ids)
  )

  resps <- .batch_query(urls, msg = "OSF Info", req_func = .osf_headers)
  all_ids <- c(guid_ids, wb_ids)

  # Process responses
  results <- vector("list", length(resps))

  for (i in seq_along(resps)) {
    resp <- resps[[i]]
    id <- all_ids[[i]]

    results[[i]] <- tryCatch({
      if (inherits(resp, "error")) {
        warning(id, " resulted in an error", call. = FALSE)
        data.frame(osf_id = id, osf_type = "error")
      } else {
        .osf_parse_response(resp, pb = pb)
      }
    }, error = \(e) {
      data.frame(osf_id = id, osf_type = "error")
    })
  }

  info_table <- do.call(dplyr::bind_rows, results)
  info_table$osf_id <- all_ids

  if (any(is.na(valid_ids))) {
    invalid <- data.frame(
      osf_id = osf_id[is.na(valid_ids)],
      osf_type = "invalid"
    )
    info_table <- dplyr::bind_rows(info_table, invalid)
  }

  return(info_table)
}

#' Parse an OSF API response into a data frame
#'
#' @param resp an httr2 response
#' @param pb a progress bar
#'
#' @returns a single-row data frame
#' @keywords internal
.osf_parse_response <- function(resp, pb = NULL) {
  id <- NA_character_
  if (is.data.frame(resp)) {
    all_data <- resp
  } else {
    sc <- httr2::resp_status(resp)
    if (sc == 200) {
      content <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      all_data <- content$data
    } else if (sc %in% c(401, 403)) {
      return(data.frame(osf_id = id,
                        osf_type = "private",
                        public = FALSE))
    } else if (sc == 429) {
      warning("Too many requests", call. = FALSE)
      return(data.frame(osf_id = id, osf_type = "too many requests"))
    } else {
      warning(id, " could not be found", call. = FALSE)
      return(data.frame(osf_id = id, osf_type = "unfound"))
    }
  }

  if (is.data.frame(all_data) && nrow(all_data) == 0) {
    return(NULL)
  }

  pds <- lapply(seq_along(all_data$id), \(i) {
    if (length(all_data$id) == 1) {
      data <- all_data
    } else {
      data <- all_data[i, ]
    }
    osf_type <- data$type

    pd <- NULL
    if (osf_type == "nodes") pd <- .osf_node_data(data)
    if (osf_type == "files") pd <- .osf_file_data(data)
    if (osf_type == "preprints") pd <- .osf_preprint_data(data)
    if (osf_type == "registrations") pd <- .osf_reg_data(data)
    if (osf_type == "users") pd <- .osf_user_data(data)
    if (is.null(pd)) {
      warning(id, " has unknown type: ", osf_type, call. = FALSE)
      pd <- data.frame(osf_id = id, osf_type = "unknown")
    }

    return(pd)
  }) |> dplyr::bind_rows()

  return(pds)
}


#' Structure OSF Node Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
.osf_node_data <- function(data) {
  if (is.null(data) | length(data) == 0) {
    return(data.frame())
  }

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
    self = data$links$self %||% NA_character_,
    children = data$relationships$children$links$related$href %||% NA_character_,
    files = data$relationships$files$links$related$href %||% NA_character_,
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
.osf_file_data <- function(data) {
  if (is.null(data) | length(data) == 0) {
    return(data.frame())
  }

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$name  %||% NA_character_,
    description = att$description %||% NA_character_,
    provider = att$provider %||% NA_character_,
    osf_type = data$type,
    kind = att$kind %||% NA_character_,
    filetype = NA_character_,
    public = att$public %||% NA,
    category = att$category %||% NA_character_,
    size = att$size %||% NA_integer_,
    downloads = att$extra$downloads %||% NA_integer_,
    path = att$materialized_path %||% att$path %||% NA_character_,
    self = data$links$self %||% NA_character_,
    files = data$relationships$files$links$related$href %||% NA_character_,
    download_url = data$links$download %||% NA_character_,
    parent = data$relationships$parent_folder$data$id %||%
      data$relationships$target$data$id %||% NA_character_,
    project = data$relationships$target$data$id %||%
      data$relationships$root$data$id %||% NA_character_
  )

  # guess file type
  is_file <- obj$kind == "file"
  obj$filetype[is_file] <- filetype(obj$name[is_file])

  folders <- which(obj$kind == "folder")
  noname <- folders & is.na(obj$name)
  obj$name[noname] <- obj$provider[noname] # name unnamed folders after the provider
  if (length(folders) &&
      !is.null(data$relationships$root_folder$data$id)) {
    ids <- data$relationships$root_folder$data$id[folders]
    obj$osf_id[folders][!is.na(ids)] <- ids[!is.na(ids)]
  }

  return(obj)
}

#' Structure OSF Preprint Data
#'
#' @param data the data object from an OSF API call
#'
#' @returns a data frame with a subset of data
#' @export
#' @keywords internal
.osf_preprint_data <- function(data) {
  if (is.null(data) | length(data) == 0) {
    return(data.frame())
  }

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$title,
    description = att$description %||% NA_character_,
    # tags = sapply(att$tags, paste, collapse = ";"),
    osf_type = data$type,
    provider = data$relationships$provider$data$id,
    public = att$public %||% NA,
    doi = att$doi %||% NA_character_,
    version = att$version %||% NA_integer_,
    is_published = att$is_published %||% NA,
    date_created = att$date_created %||% NA_character_,
    date_modified = att$date_modified %||% NA_character_,
    self = data$links$self %||% NA_character_,
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
.osf_reg_data <- function(data) {
  if (is.null(data) | length(data) == 0) {
    return(data.frame())
  }

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$title %||% NA_character_,
    osf_type = data$type,
    category = "registration",
    registration = att$registration %||% NA,
    preprint = data$attributes$preprint %||% NA,
    self = data$links$self %||% NA_character_,
    children = data$relationships$children$links$related$href %||% NA_character_,
    files = data$relationships$files$links$related$href %||% NA_character_,
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
.osf_user_data <- function(data) {
  if (is.null(data) | length(data) == 0) {
    return(data.frame())
  }

  att <- data$attributes

  obj <- data.frame(
    osf_id = data$id,
    name = att$full_name %||% NA_character_,
    osf_type = data$type,
    public = TRUE,
    orcid = att$social$orcid %||% NA_character_,
    self = data$links$self %||% NA_character_
  )

  return(obj)
}

#' Get OSF Parent Project
#'
#' @param osf_id an OSF ID
#'
#' @returns the ID of the parent project
#' @export
#' @keywords internal
.osf_parent_project <- function(osf_id) {
  valid_id <- osf_check_id(osf_id)
  if (is.na(valid_id)) {
    return(NA_character_)
  }

  # TODO: make this more efficient by just getting the parent
  obj <- suppressMessages(.osf_info(valid_id))

  if (obj$osf_type == "error") {
    logger(".osf_parent_project", list(error = "osf error"))
    return(NA_character_)
  }

  if (!is.null(obj$project) && !is.na(obj$project)) {
    return(obj$project)
  }
  if (is.null(obj$parent) || is.na(obj$parent)) {
    return(osf_id)
  }

  parent <- .osf_parent_project(obj$parent)

  return(parent)
}
