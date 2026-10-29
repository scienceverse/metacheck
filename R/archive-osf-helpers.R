#' OSF Headers
#'
#' Adds OSF auth and accept headers to an httr2 request.
#'
#' @param req an httr2 request object
#' @param pat an OSF personal access token to authorise the request with.
#'   Defaults to the token set by [osf_pat()], which falls back to the `OSF_PAT`
#'   environment variable (normally set in `.Renviron`), left out when the OSF
#'   refuses it (see [api_key_status()]). `.osf_headers()` is
#'   passed bare as a request configurator in several places (e.g. as
#'   `.batch_query(req_func = )`), so a token supplied for a whole run is set
#'   with [osf_pat()] rather than threaded through every call site. Named `pat`
#'   rather than `osf_pat` because an argument of that name would shadow the
#'   [osf_pat()] function supplying its own default.
#'
#' @returns the modified request
#' @export
#' @keywords internal
.osf_headers <- function(req, pat = .key_use("osf", check = .osf_key_check)) {
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

  # 15 s with one retry, not a single 5 s try: on 2026-10-13 the OSF API was
  # slow for hours (single requests of 6-15 s), and a 5 s limit then reported
  # a valid token as unverifiable ("the OSF may be down") -- which is how the
  # test of this function failed while the OSF was answering correctly.
  req <- httr2::request(probe) |>
    httr2::req_error(is_error = \(r) FALSE)  |>
    httr2::req_timeout(15) |>
    httr2::req_retry(max_tries = 2, retry_on_failure = TRUE) |>
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
      "The OSF_PAT could not be validated because the test file is not available; the OSF may be down.",
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

  # authorised access success; remembered, so .osf_headers() does not check
  # the key a second time (see .key_use())
  if (sc_auth %in% 200L) {
    assign(paste("osf", osf_pat), TRUE, envir = .key_checked)
    return(TRUE)
  }

  if (sc_auth %in% c(401L, 403L)) {
    # remembered, so .osf_headers() leaves this token out (see .key_use())
    assign(paste("osf", osf_pat), FALSE, envir = .key_checked)
    if (identical(osf_pat, Sys.getenv("OSF_PAT"))) {
      # unset PAT if invalid
      .key_problem_record("osf", "rejected", source = "server")
      warning(
        "The current OSF_PAT blocks access to public files. ",
        "Clearing OSF_PAT for this session. ",
        "Update or remove it in .Renviron.",
        call. = FALSE
      )
      Sys.setenv(OSF_PAT = "")
    } else {
      # A token given directly (such as one typed in osf_app()) is not the
      # one in .Renviron: clearing OSF_PAT here removed a working token from
      # .Renviron for the rest of the session.
      warning("This OSF token blocks access to public files, so it is not used.",
              call. = FALSE)
    }
  }

  return(FALSE)
}

# A key the OSF refuses answers 401 even for a public preprint (the one
# .osf_pat_validate() also uses).
.osf_key_check <- function(key) {
  .key_check_request("https://api.osf.io/v2/preprints/khbvy/",
                     list(Authorization = sprintf("Bearer %s", key),
                          Accept = "application/vnd.api+json"))
}

#' Retrieve info from the OSF by ID
#'
#' @param osf_id an vector of OSF IDs or URLs
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a previously cached listing for an id
#'   already looked up (see [repo_info_cache_clear()]) instead of re-querying the
#'   OSF API. Off by default. Separate from (and in addition to) `osf_info()`'s
#'   own in-memory, session-only cache (`options(metacheck.osf.cache = )`) --
#'   this one is persistent on disk and survives a restart of the script.
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.osf_info <- function(osf_id, pb = NULL, cache = FALSE) {
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
  # a versioned GUID (abcde_v1) is a GUID too: it was sent to /files/ (U52)
  is_guid <- grepl("^[a-z0-9]{5}(_v[0-9]+)?$", valid_ids, ignore.case = TRUE)
  is_vo <- grepl("/?\\?\\s*view_only=", valid_ids)
  # id_vo <- strsplit(valid_ids[is_vo], "/?\\?\\s*view_only=")
  # vo_ids <- sapply(id_vo, `[[`, 1)
  # vo_tokens <- sapply(id_vo, `[[`, 2)
  guid_ids <- valid_ids[is_guid | is_vo]
  wb_ids <- valid_ids[!is_guid & !is_vo & !is.na(valid_ids)]

  all_urls <- c(
    sprintf("%s/guids/%s", osf_api, guid_ids),
    #sprintf("%s/nodes/%s/?view_only=%s", osf_api, vo_ids, vo_tokens),
    sprintf("%s/files/%s", osf_api, wb_ids)
  )
  all_ids <- c(guid_ids, wb_ids)

  # Reuse a persistent cached entry per id when requested (see
  # R/repo-info-cache.R), so restarting a corpus build does not re-query OSF
  # for a node already looked up in an earlier run. This is separate from
  # (and checked before falling through to) osf_info()'s own in-memory
  # session cache above, which does not survive a restart. A cache miss/off
  # falls through to the request exactly as before.
  results <- vector("list", length(all_ids))
  need_fetch <- rep(TRUE, length(all_ids))
  if (isTRUE(cache)) {
    for (i in seq_along(all_ids)) {
      hit <- .repo_info_cache_get("osf", all_ids[i])
      if (!is.null(hit)) {
        results[[i]] <- hit
        need_fetch[i] <- FALSE
      }
    }
  }

  fetch_idx <- which(need_fetch)
  if (length(fetch_idx) > 0) {
    fetch_urls <- all_urls[fetch_idx]
    fetch_ids  <- all_ids[fetch_idx]
    resps <- .batch_query(fetch_urls, msg = "OSF Info", req_func = .osf_headers)

    for (k in seq_along(fetch_idx)) {
      i <- fetch_idx[k]
      resp <- resps[[k]]
      id <- fetch_ids[[k]]

      results[[i]] <- tryCatch({
        if (inherits(resp, "error") || is.null(resp)) {
          warning(id, " resulted in an error", call. = FALSE)
          data.frame(osf_id = id, osf_type = "error",
                     error = paste("the request to the OSF failed:",
                                   if (inherits(resp, "error")) conditionMessage(resp) else "no response"))
        } else {
          # `id` is the ID this response was requested for, so a failure can name
          # it rather than reporting "NA could not be found".
          .osf_parse_response(resp, pb = pb, osf_id = id)
        }
      }, error = \(e) {
        data.frame(osf_id = id, osf_type = "error",
                   error = paste("the OSF answer could not be read:", conditionMessage(e)))
      })

      if (isTRUE(cache) && .repo_info_ok(results[[i]]))
        .repo_info_cache_put("osf", id, results[[i]])
    }
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

# The component tree below one OSF node, and every node's storage providers,
# in as few requests as the OSF allows.
#
# Walking the tree one node at a time costs two requests per component: its
# /children/ (to find the next level) and its /files/ (to find which storage
# providers it uses -- osfstorage, and any add-on such as GitHub). One listing
# of /nodes/?filter[root]=<project>&embed=files returns every component of the
# project, at every depth, 100 per page, each with its providers embedded --
# for pngda, 8 nodes in 1 request instead of 17 (measured 2026-10-13). The
# embedded provider objects are the same objects /files/ returns, so they
# parse to identical rows (checked with identical() on pngda's mc45x).
#
# `filter[root]` takes the PROJECT's id, so for a linked component the whole
# project is listed and only the part below `osf_id` is kept, by following
# each node's parent. Registrations work the same way on /registrations/.
#
# One deliberate difference from the node-by-node walk: the filtered listing
# leaves out a private component that the token's owner can see without being
# a contributor to it (verified on pngda's ybm3c, which /children/ returns
# when the token's owner can see it). Such a component is not part of what a
# paper shares with its readers, and on a hosted app it would show the token
# owner's private files in someone else's report, so leaving it out is right.
#
# An embedded provider list carries only its first page (10 items); a node
# with more than that is returned in `missing`, for the caller to list the
# old way. Returns NULL when the tree could not be listed like this at all
# (request failed, listing incomplete, or `osf_id` not in it), so the caller
# walks it node by node as before.
#
# Returns list(nodes = parsed rows of the components below `osf_id`,
# providers = parsed provider rows of `osf_id` and those components,
# missing = files links of nodes whose providers still need listing).
.osf_tree <- function(osf_id, osf_type, root) {
  if (length(osf_type) != 1 || !osf_type %in% c("nodes", "registrations") ||
      length(root) != 1 || is.na(root) || !nzchar(root)) return(NULL)
  osf_api <- getOption("metacheck.osf.api")
  url <- sprintf("%s/%s/?filter[root]=%s&embed=files", osf_api, osf_type, root)
  all <- tryCatch(osf_get_all_pages(url), error = \(e) NULL)
  if (is.null(all) || !is.null(attr(all, "osf_error")) ||
      !is.null(attr(all, "osf_incomplete")) || !is.data.frame(all) ||
      nrow(all) == 0 || !osf_id %in% all$id) return(NULL)

  # Keep `osf_id` and everything below it.
  parent <- all$relationships$parent$data$id %||% rep(NA_character_, nrow(all))
  keep <- osf_id
  frontier <- osf_id
  while (length(frontier) > 0) {
    kids <- setdiff(all$id[!is.na(parent) & parent %in% frontier], keep)
    keep <- c(keep, kids)
    frontier <- kids
  }
  sub <- all[all$id %in% keep, , drop = FALSE]
  below <- sub[sub$id != osf_id, , drop = FALSE]
  nodes <- if (nrow(below) > 0) .osf_parse_response(below) else NULL

  # Each node's embedded provider list.
  emb <- sub$embeds$files
  providers <- data.frame()
  missing <- character(0)
  files_links <- sub$relationships$files$links$related$href %||%
    rep(NA_character_, nrow(sub))
  for (k in seq_len(nrow(sub))) {
    p <- if (is.list(emb$data)) emb$data[[k]] else NULL
    total <- emb$links$meta$total[k] %||% NA
    complete <- is.data.frame(p) && nrow(p) > 0 &&
      (is.na(total) || nrow(p) >= total)
    if (complete) {
      providers <- dplyr::bind_rows(providers, .osf_parse_response(p))
    } else if (!is.na(files_links[k])) {
      # No embed, an embed error, or more providers than its first page held.
      missing <- c(missing, files_links[k])
    }
  }

  list(nodes = nodes, providers = providers, missing = missing)
}

# A GitHub repository linked to an OSF node, listed through GitHub itself.
#
# Through the OSF, a linked GitHub repository is listed one folder per request,
# each proxied to GitHub by the OSF's file service: 3 requests and 72 seconds
# for scienceverse/demo, linked to mc45x (measured 2026-10-13), and the rows
# come back without file sizes. GitHub's own recursive tree lists the whole
# repository in 2 requests (github_tree_files(): 3 seconds there), with sizes.
# The node's /addons/github/ settings name the repository ("owner/repo:"),
# readable without a token for a public node (checked the same day).
#
# The rows are shaped like .osf_file_data()'s, with download links on
# raw.githubusercontent.com instead of the OSF's proxy. Each `osf_id` is a
# made-up "<node>-github-<n>", since only the OSF's proxy assigns real ones:
# it is unique and safe as a file name, which is what osf_file_download()
# uses it for.
#
# Returns NULL whenever this route cannot be used -- no GitHub settings, a
# branch other than the default named in them, or GitHub not listing the
# repository (e.g. a private repository, which the OSF can still show through
# its owner's GitHub account) -- and the caller then lists it through the OSF
# as before, so nothing is skipped.
.osf_github_files <- function(node) {
  osf_api <- getOption("metacheck.osf.api")
  addon <- tryCatch(
    osf_get_all_pages(sprintf("%s/nodes/%s/addons/github/", osf_api, node)),
    error = \(e) NULL)
  folder_id <- tryCatch(addon$attributes$folder_id, error = \(e) NULL)
  if (!is.character(folder_id) || length(folder_id) != 1 ||
      is.na(folder_id) || !nzchar(folder_id)) return(NULL)
  repo <- sub(":.*$", "", folder_id)
  branch <- sub("^[^:]*:?", "", folder_id)
  if (nzchar(branch) || !grepl("^[^/[:space:]]+/[^/[:space:]]+$", repo)) return(NULL)

  gh <- tryCatch(github_tree_files(repo), error = \(e) NULL)
  f <- gh$files
  if (is.null(gh) || isTRUE(gh$gated) || !is.data.frame(f) || nrow(f) == 0 ||
      !all(c("name", "path", "download_url", "size") %in% names(f))) return(NULL)

  file_rows <- data.frame(
    osf_id = sprintf("%s-github-%d", node, seq_len(nrow(f))),
    name = f$name,
    description = NA_character_,
    provider = "github",
    osf_type = "files",
    kind = "file",
    filetype = filetype(f$name),
    public = NA,
    category = NA_character_,
    size = f$size,
    downloads = NA_integer_,
    path = paste0("/", f$path),
    self = NA_character_,
    files = NA_character_,
    download_url = f$download_url,
    parent = node,
    project = node
  )

  # Folder rows, as the OSF listing has them (a folder is never downloaded,
  # but callers build save paths and counts from the full listing).
  parts <- strsplit(f$path, "/", fixed = TRUE)
  dirs <- unique(unlist(lapply(parts, \(p) {
    if (length(p) < 2) return(character(0))
    vapply(seq_len(length(p) - 1), \(k) paste(p[seq_len(k)], collapse = "/"), "")
  })))
  folder_rows <- if (length(dirs) > 0) data.frame(
    osf_id = sprintf("%s-github-dir-%d", node, seq_along(dirs)),
    name = basename(dirs),
    description = NA_character_,
    provider = "github",
    osf_type = "files",
    kind = "folder",
    filetype = NA_character_,
    public = NA,
    category = NA_character_,
    size = NA_integer_,
    downloads = NA_integer_,
    path = paste0("/", dirs, "/"),
    self = NA_character_,
    files = NA_character_,
    download_url = NA_character_,
    parent = node,
    project = node
  ) else NULL

  dplyr::bind_rows(folder_rows, file_rows)
}

#' Parse an OSF API response into a data frame
#'
#' @param resp an httr2 response
#' @param pb a progress bar
#'
#' @param osf_id the ID this response is for, used in the messages. Without it
#'   a failure could only report "NA could not be found", which tells the
#'   reader nothing about which of their IDs was wrong.
#'
#' @returns a single-row data frame
#' @keywords internal
.osf_parse_response <- function(resp, pb = NULL, osf_id = NA_character_) {
  id <- osf_id
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
      # The OSF sends no time at which its limit ends (100 requests per hour
      # without a key, 10,000 per day with one), so an hour is assumed.
      limit <- .key_limit_reached("osf")
      warning(limit, call. = FALSE)
      # `error` marks the row as a failed lookup, so repo_check reports it and
      # the listing cache does not keep it (.repo_info_ok(), #501)
      return(data.frame(osf_id = id, osf_type = "too many requests",
                        error = limit))
    } else if (sc >= 500) {
      # The OSF API is down or overloaded (an outage answered every request
      # with HTTP 503 on 2026-10-17): that says nothing about whether the
      # project exists, so it is not reported as "could not be found", and is
      # kept with its reason instead of being dropped (#501).
      warning(if (is.na(id)) "An OSF resource" else id,
              ": the OSF API did not answer (HTTP ", sc,
              "); the OSF may be down. Try again later.", call. = FALSE)
      return(data.frame(osf_id = id, osf_type = "error",
                        error = sprintf("the OSF API did not answer (HTTP %d)", as.integer(sc))))
    } else {
      # 410 (Gone) is the OSF saying the project existed and was deleted or
      # withdrawn, which is worth distinguishing from 404 (never existed, or
      # mistyped) -- the reader's next step is different in each case.
      warning(
        if (is.na(id)) "An OSF resource" else id,
        if (identical(sc, 410L) || identical(sc, 410)) {
          " has been deleted or withdrawn from the OSF (HTTP 410), so its files cannot be downloaded."
        } else {
          paste0(
            " could not be found on the OSF (HTTP ", sc,
            "). Check the ID is spelled correctly",
            if (!is.na(id)) sprintf(" and that you can see it at https://osf.io/%s", id) else "",
            ". A private project needs an OSF token; see ?osf_pat")
        },
        call. = FALSE)
      return(data.frame(osf_id = id, osf_type = "unfound",
                        error = if (identical(as.integer(sc), 410L))
                          "deleted or withdrawn from the OSF (HTTP 410)"
                        else sprintf("not found on the OSF (HTTP %d)", as.integer(sc))))
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
      # [[ ]]: $root matched root_folder when root was missing (U52)
      data$relationships[["root"]]$data$id %||% NA_character_
  )

  # guess file type
  is_file <- obj$kind == "file"
  obj$filetype[is_file] <- filetype(obj$name[is_file])

  folders <- which(obj$kind == "folder")
  # a logical test per row: `folders & is.na(...)` mixed row numbers with
  # TRUE/FALSE, so unnamed rows that are not folders were renamed too (U52)
  noname <- obj$kind %in% "folder" & is.na(obj$name)
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
