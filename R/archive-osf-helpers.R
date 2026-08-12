#' OSF Headers
#'
#' Adds OSF auth and accept headers to an httr2 request.
#'
#' @param req an httr2 request object
#' @param pat an OSF personal access token to authorise the request with.
#'   Defaults to the token set by [osf_pat()], which falls back to the `OSF_PAT`
#'   environment variable (normally set in `.Renviron`). `.osf_headers()` is
#'   passed bare as a request configurator in several places (e.g. as
#'   `.batch_query(req_func = )`), so a token supplied for a whole run is set
#'   with [osf_pat()] rather than threaded through every call site. Named `pat`
#'   rather than `osf_pat` because an argument of that name would shadow the
#'   [osf_pat()] function supplying its own default.
#'
#' @returns the modified request
#' @export
#' @keywords internal
.osf_headers <- function(req, pat = osf_pat()) {
  req <- req |>
    httr2::req_headers(
      `User-Agent` = "metacheck",
      Accept = "application/vnd.api+json"
    )
  pat <- pat %||% ""
  if (!nzchar(pat)) {
    return(req)
  }

  # PAT exists, check validation
  req_pat <- req |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", pat))

  return(req_pat)
}

#' Set or get the OSF personal access token
#'
#' Use `osf_pat()` to get the token used to authorise OSF API requests, or
#' `osf_pat("your-token")` to set it for the rest of the session.
#'
#' Without a token you can make 100 API requests per hour; with one you can make
#' 10,000 per day, so a token is worth setting before downloading whole projects
#' or listing everything a user has.
#'
#' To create one, sign in at <https://osf.io/settings/tokens>, click "Create
#' token", give it a name, and tick the `osf.full_read` scope (add
#' `osf.full_write` only if you also intend to write to the OSF). Copy the token
#' immediately, as the OSF does not show it again.
#'
#' The usual way to store it is a line in your `.Renviron` file (open it with
#' `usethis::edit_r_environ()`), which is read every time R starts:
#'
#' `OSF_PAT="replace-with-your-token-string"`
#'
#' Setting the token with `osf_pat()` overrides that for the current session
#' only, which is useful when you cannot edit `.Renviron` or want to use a
#' different account for one run.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' osf_pat() # returns "" unless a token is set
osf_pat <- function(pat = NULL) {
  if (is.null(pat)) {
    return(getOption("metacheck.osf.pat") %||% Sys.getenv("OSF_PAT"))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set osf_pat with a single string containing your OSF token",
         call. = FALSE)
  }
  options(metacheck.osf.pat = pat)
  invisible(pat)
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

#' List the Projects Belonging to an OSF User
#'
#' Every project an OSF user contributes to, as a table you can read and filter
#' before downloading anything. Pass the whole table, or any subset of it, to
#' [osf_file_download()].
#'
#' `/users/{id}/nodes/` returns each node the user can see, which includes
#' components as separate rows alongside the projects that contain them.
#' Downloading a project pulls in its components' files automatically (the
#' folder structure in [osf_file_download()] nests them, at any depth), so
#' listing a component separately would only download the same files twice.
#' Each node is therefore reduced to its root (`relationships$root`, which
#' [.osf_node_data()] exposes as the `project` column); a node with no root
#' recorded stands as its own project.
#'
#' No file counts or sizes are reported. They would cost an extra request per
#' project and could only ever count files at the project's own root, missing
#' everything inside folders and components -- while the download itself
#' already lists each project in full, so the true size is discovered there
#' anyway.
#'
#' @param user_id an OSF user ID (the 5-character GUID in a profile URL, e.g.
#'   `"4i578"` for <https://osf.io/4i578>)
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame with one row per project: `osf_id`, `name`,
#'   `category`, `public`, and `osf_url`. Zero rows when the user has no
#'   projects that could be listed.
#' @export
#'
#' @examples
#' \dontrun{
#' # see what there is
#' projects <- osf_user_projects("4i578")
#'
#' # download only the public ones
#' projects |>
#'   subset(public) |>
#'   osf_file_download(download_to = "my_osf")
#'
#' # or pick by name
#' projects |>
#'   subset(grepl("ManyLabs", name)) |>
#'   osf_file_download(download_to = "manylabs")
#' }
osf_user_projects <- function(user_id, pb = NULL) {
  user_id <- osf_check_id(user_id) |> stats::na.omit() |> unique()

  empty <- data.frame(
    osf_id = character(0), name = character(0), category = character(0),
    public = logical(0), osf_url = character(0)
  )
  if (length(user_id) == 0) return(empty)

  osf_api <- getOption("metacheck.osf.api")
  url <- sprintf("%s/users/%s/nodes/", osf_api, user_id[[1]])
  nodes <- osf_get_all_pages(url)

  if (!is.null(attr(nodes, "osf_error")) || length(nodes) == 0) {
    return(empty)
  }

  info <- .osf_parse_response(nodes, pb = pb)
  if (is.null(info) || nrow(info) == 0) return(empty)

  # A top-level project reports no root of its own, so it stands as its own
  # project; a component reports the project that contains it.
  root <- info$project %||% rep(NA_character_, nrow(info))
  root[is.na(root)] <- info$osf_id[is.na(root)]
  info$root_id <- root

  # Keep each root project's OWN row where the listing includes it, so the
  # title shown is the project's rather than a component's. A component whose
  # parent project the user does not contribute to has no such row, so its root
  # is carried through with whatever details are available.
  own_row <- info[!is.na(info$osf_id) & info$osf_id == info$root_id, , drop = FALSE]
  missing_root <- setdiff(stats::na.omit(unique(info$root_id)), own_row$osf_id)

  out <- data.frame(
    osf_id = c(own_row$osf_id, missing_root),
    name = c(own_row$name %||% rep(NA_character_, nrow(own_row)),
             rep(NA_character_, length(missing_root))),
    category = c(own_row$category %||% rep(NA_character_, nrow(own_row)),
                 rep(NA_character_, length(missing_root))),
    public = c(own_row$public %||% rep(NA, nrow(own_row)),
               rep(NA, length(missing_root)))
  )
  out <- out[!is.na(out$osf_id), , drop = FALSE]
  out <- out[!duplicated(out$osf_id), , drop = FALSE]

  # A project the user contributes to only through one of its components has no
  # row of its own in the listing, so it would appear with no title at all --
  # unselectable, which defeats the point of listing. One batched request fills
  # those in. A project that stays unnamed after this is one the token cannot
  # see (the API answers 401), which `public = FALSE` then records.
  unnamed <- which(is.na(out$name))
  if (length(unnamed) > 0) {
    urls <- sprintf("%s/nodes/%s/", osf_api, out$osf_id[unnamed])
    resps <- .batch_query(urls, msg = NULL, req_func = .osf_headers)

    for (i in seq_along(unnamed)) {
      resp <- resps[[i]]
      if (is.null(resp) || inherits(resp, "error")) next
      if (httr2::resp_status(resp) != 200) {
        # 401/403 means the project exists but this token cannot read it
        if (httr2::resp_status(resp) %in% c(401, 403)) {
          out$public[unnamed[i]] <- FALSE
        }
        next
      }
      body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE),
                       error = \(e) NULL)
      if (is.null(body)) next
      att <- body$data$attributes
      out$name[unnamed[i]] <- att$title %||% NA_character_
      out$category[unnamed[i]] <- att$category %||% NA_character_
      out$public[unnamed[i]] <- att$public %||% NA
    }
  }

  out$osf_url <- paste0("https://osf.io/", out$osf_id)
  rownames(out) <- NULL

  out
}

#' List the project IDs belonging to an OSF user
#'
#' The IDs from [osf_user_projects()], for callers that only need the vector.
#'
#' @param user_id an OSF user ID
#' @param pb a progress bar passed from another function
#'
#' @returns a character vector of unique project IDs, or `character(0)` when the
#'   user has no visible projects
#' @export
#' @keywords internal
.osf_user_nodes <- function(user_id, pb = NULL) {
  osf_user_projects(user_id, pb = pb)$osf_id
}

#' Expand OSF user IDs into their project IDs
#'
#' Any ID in `osf_id` that belongs to an OSF *user* rather than a project is
#' replaced by that user's projects; every other ID is passed through unchanged.
#' This is what lets [osf_file_download()] accept a profile ID and download
#' everything that user has shared.
#'
#' @param osf_id a vector of OSF IDs (already validated by [osf_check_id()])
#' @param pb a progress bar passed from another function
#'
#' @returns a character vector of IDs with any user IDs replaced by their
#'   projects
#' @export
#' @keywords internal
.osf_expand_user_ids <- function(osf_id, pb = NULL) {
  if (length(osf_id) == 0) return(osf_id)

  # osf_type() is one API call per ID, so only ask about IDs that could be a
  # user: a user GUID is always 5 characters, never a 24-character waterbutler
  # file ID and never a view-only link.
  could_be_user <- nchar(osf_id) == 5 & !grepl("view_only=", osf_id)
  if (!any(could_be_user)) return(osf_id)

  types <- suppressWarnings(osf_type(osf_id[could_be_user]))
  is_user <- !is.na(types) & types == "users"
  if (!any(is_user)) return(osf_id)

  user_ids <- osf_id[could_be_user][is_user]
  expanded <- lapply(user_ids, \(uid) {
    projects <- .osf_user_nodes(uid, pb = pb)
    if (length(projects) == 0) {
      warning("OSF user ", uid, " has no projects that could be listed. ",
              "Private projects need an OSF token; see ?osf_pat",
              call. = FALSE)
      return(character(0))
    }
    message(sprintf("OSF user %s has %d project%s to download",
                    uid, length(projects), plural(length(projects))))
    projects
  })

  # Keep non-user IDs in their original order, then append what each user
  # expanded to; unique() drops a project named directly as well as via its
  # owner.
  c(osf_id[!(osf_id %in% user_ids)], unlist(expanded, use.names = FALSE)) |>
    unique()
}

#' Check downloaded files against the file system
#'
#' Confirms that every file the download planned to save is actually present,
#' and is the size the OSF reported. Called at the end of [osf_file_download()].
#'
#' A row can be marked as downloaded because the copy step ran for it, while
#' nothing usable ended up on disk: the transfer may have failed after retries,
#' `file.copy()` may have returned FALSE, the file system may have rejected the
#' name, or the write may have been truncated. Only the file system settles it.
#'
#' Size is compared where the OSF reported one. A file present but a different
#' size is counted as not downloaded, since a truncated file is worse than an
#' absent one -- it looks complete to everything downstream.
#'
#' @param ret the return table being built, with `path` and `downloaded` columns
#' @param download_to the absolute folder the project was saved in
#' @param check_size whether to compare each file's size on disk against the
#'   size the OSF reported. May be a single value, or one value per row: in zip
#'   mode with `unzip = FALSE`, rows covered by an archive share one file on
#'   disk (so a per-file size comparison is meaningless for them), while rows
#'   fetched individually because no archive could hold them are real files
#'   whose size can and should be checked.
#'
#' @returns `ret` with `downloaded` corrected against the file system, and a
#'   `size_on_disk` column added
#' @export
#' @keywords internal
.osf_verify_downloads <- function(ret, download_to, check_size = TRUE) {
  if (is.null(ret) || nrow(ret) == 0) return(ret)

  if (!"path" %in% names(ret)) {
    ret$size_on_disk <- NA_real_
    ret$downloaded <- FALSE
    return(ret)
  }

  # A row whose path was never set (no copy was planned for it) is missing by
  # definition; file.path() with NA would otherwise build the literal path
  # "dir/NA" and test that.
  has_path <- !is.na(ret$path)
  full <- rep(NA_character_, nrow(ret))
  full[has_path] <- file.path(download_to, ret$path[has_path])

  on_disk <- rep(FALSE, nrow(ret))
  on_disk[has_path] <- file.exists(full[has_path]) & !dir.exists(full[has_path])

  size_on_disk <- rep(NA_real_, nrow(ret))
  size_on_disk[on_disk] <- file.size(full[on_disk])
  ret$size_on_disk <- size_on_disk

  ok <- on_disk & !is.na(size_on_disk)

  # A file present but the wrong size is worse than an absent one: it looks
  # complete to everything downstream. Only compared where the OSF reported a
  # size, and only for rows the caller says are real files (see `check_size`).
  check_size <- rep_len(as.logical(check_size), nrow(ret))
  if (any(check_size %in% TRUE)) {
    expected <- suppressWarnings(
      as.numeric(ret$size %||% rep(NA_real_, nrow(ret))))
    size_matches <- is.na(expected) | (size_on_disk == expected)
    ok <- ok & (!(check_size %in% TRUE) | size_matches %in% TRUE)
  }

  ret$downloaded <- ok & ret$downloaded %in% TRUE
  ret
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
