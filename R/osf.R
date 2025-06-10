#' Retrieve info from the OSF by ID
#'
#' @param id an OSF ID or URL
#'
#' @returns a data frame of information
#' @export
osf_retrieve <- function(id) {
  # handle list of links
  if (is.data.frame(id)) id <- id[[1]]
  if (length(id) > 1) {
    data <- lapply(unique(id), \(subid) {
      message("Retrieving info from ", subid, "...")
      osf_retrieve(subid)
    }) |>
      do.call(dplyr::bind_rows, args = _) |>
      # reduplicate
      dplyr::right_join(data.frame(id = id), by = "id")

    return(data)
  }

  osf_id <- tryCatch(osfr::as_id(id),
                 error = \(e) {
                   warning(id, " is not a valid OSF ID", call. = FALSE)
                   return(NULL)
                 })

  # invalid ID ----
  if (is.null(osf_id)) {
    obj <- data.frame(
      id = id,
      osf_type = "invalid"
    )
    return(obj)
  }

  Sys.sleep(osf_delay())

  # check for node ----
  node <- tryCatch(osfr::osf_retrieve_node(osf_id),
                   error = \(e) return(NULL))
  if (!is.null(node)) {
    obj <- data.frame(
      id = id,
      osf_id = node$id,
      name = node$name,
      osf_type = "node",
      category = node$meta[[1]]$attributes$category %||% NA,
      registration = node$meta[[1]]$attributes$registration %||% NA,
      preprint = node$meta[[1]]$attributes$preprint %||% NA,
      parent = node$meta[[1]]$relationships$parent$data$id %||% NA
    )

    if (obj$category == "project") {
      obj$project <- obj$osf_id
    } else {
      obj$project <- osf_parent_project(obj$parent)
    }

    return(obj)
  }

  # check for file ----
  file <- tryCatch(osfr::osf_retrieve_file(osf_id),
                     error = \(e) return(NULL))
  if (!is.null(file)) {
    obj <- data.frame(
      id = id,
      osf_id = file$id,
      name = file$name,
      osf_type = "file",
      category = "file",
      downloads = file$meta[[1]]$attributes$extra$downloads %||% NA,
      parent = file$meta[[1]]$relationships$node$data$id %||% NA
    )
    obj$project <- osf_parent_project(obj$parent)

    return (obj)
  }

  # check for user ----
  user <- tryCatch(osfr::osf_retrieve_user(osf_id),
                     error = \(e) return(NULL))
  if (!is.null(user)) {
    obj <- data.frame(
      id = id,
      osf_id = user$id,
      name = user$name,
      osf_type = "user",
      category = "user",
      orcid = user$meta[[1]]$attributes$social$orcid %||% NA
    )

    return(obj)
  }

  # check for registration ----
  reg <- tryCatch(
    httr::GET(paste0("https://api.osf.io/v2/registrations/?filter[id]=", osf_id)),
    error = function(e) return(NULL)
  )
  if (!is.null(reg)) {
    reg_data <- jsonlite::fromJSON(rawToChar(reg$content))

  if (reg_data$links$meta$total > 0) {
      obj <- data.frame(
        id = id,
        osf_id = reg_data$data$id,
        name = reg_data$data$attributes$title %||% NA,
        osf_type = "registration",
        category = "registration",
        registration = reg_data$data$attributes$registration %||% NA,
        preprint = reg_data$data$attributes$preprint %||% NA,
        parent = reg_data$data$relationships$registered_from$data$id %||% NA
      )

      obj$project <- osf_parent_project(obj$parent)

      return(obj)
    }
  }

  # unfound but valid ID
  warning(id, " could not be found")
  obj <- data.frame(
    id = id,
    osf_type = "unfound"
  )
  return(obj)
}


#' Get contents of OSF node
#'
#' Retrieve the contents recursiveley from an OSF node
#'
#' @param id an OSF ID or URL
#'
#' @returns a data frame of the contents and info about them
#' @export
#'
osf_contents <- function(id) {
  # handle list of links
  if (is.data.frame(id)) id <- id[[1]]
  if (length(id) > 1) {
    data <- lapply(unique(id), \(subid) {
      osf_contents(subid)
    }) |>
    do.call(dplyr::bind_rows, args = _)

    return(data)
  }

  osf_id <- tryCatch(osfr::as_id(id),
                     error = \(e) {
                       warning(id, " is not a valid OSF ID", call. = FALSE)
                       return(NULL)
                     })

  # set up info
  info <- data.frame(
    osf_id = as.character(osf_id),
    type = "node"
  )

  message("Retrieving contents from ", osf_id, "...")

  Sys.sleep(osf_delay())

  node <- tryCatch(osfr::osf_retrieve_node(osf_id),
                   error = \(e) return(NULL))
  if (is.null(node)) {
    info$type <- "folder"
    node <- tryCatch(osfr::osf_retrieve_file(osf_id),
                     error = \(e) return(NULL))
  }

  if (is.null(node)) {
    info$type <- "missing"
    return(info)
  }

  info$name <- node$name
  info$parent <- node$meta[[1]]$relationships$parent$data$id %||% ""
  info$category <- node$meta[[1]]$attributes$category %||% node$meta[[1]]$attributes$kind
  info$description <- node$meta[[1]]$attributes$description %||% ""

  # add in any files
  files <- tryCatch(osfr::osf_ls_files(node), error = \(e) return(data.frame()))
  ftable <- data.frame(osf_id = character(0))
  if (nrow(files) > 0) {
    # check file extensions
    fileext <- data.frame(
      osf_id = files$id,
      ext = sub("^.*\\.", "", files$name) |> tolower()
    ) |>
      dplyr::left_join(file_types, by = "ext")

    ftable_all <- data.frame(
      osf_id = files$id,
      name = files$name,
      type = sapply(files$meta, \(x) x$attributes$kind %||% ""),
      category = node$meta[[1]]$attributes$category %||% NA,
      parent = node$id,
      description = sapply(files$meta, \(x) x$attributes$description %||% ""),
      filetype = fileext$type,
      filesize = sapply(files$meta, \(x) x$attributes$size %||% NA),
      downloads = sapply(files$meta, \(x) x$attributes$extra$downloads %||% NA)
    )

    ftable_files <- dplyr::filter(ftable_all, type != "folder")
    ftable_folders <- dplyr::filter(ftable_all, type == "folder") |>
      dplyr::pull(osf_id) |>
      lapply(osf_contents)
    ftable <- dplyr::bind_rows(ftable_files, ftable_folders)
  }

  # get child nodes
  children <- tryCatch(osfr::osf_ls_nodes(node), error = \(e) return(data.frame()))
  ctable <- data.frame(osf_id = character(0))
  if (nrow(children) > 0) {
      ctable <- lapply(children$id, osf_contents)
  }

  table <- info |>
    dplyr::bind_rows(ftable) |>
    dplyr::bind_rows(ctable)

  return(table)
}

#' Summarize Directory Contents
#'
#' @param contents a table with columns name, path such as from `osf_contents()`
#'
#' @returns the table with new columns is_readme, is_data, is_code, and best_guess
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

  contents$best_guess <- dplyr::case_when(
    contents$is_readme ~ "readme",
    contents$is_code ~ "code",
    contents$is_data ~ "data",
    .default = ""
  )

  return(contents)
}


#' Get OSF Parent Project
#'
#' @param id an OSF ID
#'
#' @returns the ID of the parent project
#' @export
osf_parent_project <- function(id) {
  Sys.sleep(osf_delay())
  obj <- osf_retrieve(id)

  if (obj$osf_type == "project" || is.na(obj$parent)) return(id)

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
