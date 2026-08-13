# Downloading a whole OSF repository the fast way: walk the component tree,
# take one archive per node, and never list the individual files.
#
# osf_file_download(mode = "select") lists every file first, because that is
# what `max_file_size` filters on and what the per-file size check verifies
# against. Neither applies when the answer is "give me all of it", and the
# listing is where nearly all the time goes: for ManyLabs2 (8cd4r) it ran over
# 15 minutes before fetching anything, while this route retrieved the whole
# 1.9 GB project in 396 seconds (measured 2026-08-13).
#
# What is given up, and why it is acceptable here:
#   * size limits cannot filter inside an archive, so this takes whole nodes
#   * there is no per-file expected size to verify against, so the check is
#     that the archive itself unzipped rather than that each file matches
#   * files on a linked add-on (GitHub, Dropbox) are not in any OSF archive,
#     so those are still fetched individually

#' Walk an OSF project's component tree
#'
#' Every node under `osf_id`, including the project itself, breadth-first.
#' Only the tree is walked -- no file listings -- because the archive endpoint
#' needs a node ID and nothing else.
#'
#' @param osf_id the root OSF node ID
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame with `osf_id` and `title` for each node
#' @keywords internal
.osf_walk_nodes <- function(osf_id, pb = NULL) {
  osf_api <- getOption("metacheck.osf.api")

  out <- data.frame(osf_id = character(0), title = character(0))
  todo <- osf_id
  level <- 0

  while (length(todo) > 0) {
    level <- level + 1
    if (!is.null(pb)) {
      sprintf("Finding components: level %d, %d to check (%d found)",
              level, length(todo), nrow(out)) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
    }

    # One request per node for its children. The title comes back in the same
    # response for every node but the root, so folders can be named after the
    # component rather than its five-character ID at no extra cost.
    found <- data.frame(osf_id = character(0), title = character(0))
    nxt <- character(0)
    for (id in todo) {
      url <- sprintf("%s/nodes/%s/children/", osf_api, id)
      kids <- tryCatch(osf_get_all_pages(url), error = \(e) NULL)
      if (is.null(kids) || !is.data.frame(kids) || nrow(kids) == 0) next
      found <- dplyr::bind_rows(found, data.frame(
        osf_id = kids$id,
        title = kids$attributes$title %||% rep(NA_character_, nrow(kids))
      ))
      nxt <- c(nxt, kids$id)
    }

    out <- dplyr::bind_rows(out, found)
    todo <- nxt
  }

  # The root's own title needs its own request, since nothing listed it as a
  # child.
  root_title <- tryCatch({
    resp <- httr2::request(sprintf("%s/nodes/%s/", osf_api, osf_id)) |>
      .osf_headers() |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) NA_character_ else
      httr2::resp_body_json(resp, simplifyVector = TRUE)$data$attributes$title
  }, error = \(e) NA_character_)

  dplyr::bind_rows(
    data.frame(osf_id = osf_id, title = root_title %||% NA_character_),
    out
  )
}

#' Download a node's files that live on a linked add-on
#'
#' The OSF's `?zip=` endpoint archives `osfstorage` and nothing else: asking it
#' for a `github` provider returns 404 (checked 2026-08-13). A project that
#' links an external repository therefore has files no archive can deliver --
#' 29 of pngda's 57, for example -- so those are listed and fetched one at a
#' time. This is the only route to them, and it is why `mode = "all"` is not
#' purely archive-based.
#'
#' @param node the OSF node ID
#' @param node_dir the folder this node's files are written to
#' @param pb a progress bar passed from another function
#'
#' @returns a list with `files` and `bytes` retrieved
#' @keywords internal
.osf_download_addons <- function(node, node_dir, pb = NULL) {
  osf_api <- getOption("metacheck.osf.api")
  none <- list(files = 0L, bytes = 0)

  provs <- tryCatch(osf_get_all_pages(sprintf("%s/nodes/%s/files/",
                                              osf_api, node)),
                    error = \(e) NULL)
  if (is.null(provs) || !is.data.frame(provs) || nrow(provs) == 0) return(none)

  names_p <- provs$attributes$provider %||% character(0)
  extra <- setdiff(tolower(names_p), "osfstorage")
  if (length(extra) == 0) return(none)

  got <- 0L
  bytes <- 0
  for (p in extra) {
    if (!is.null(pb)) {
      sprintf("%s: listing %s files (not in the OSF archive)", node, p) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
    }
    listing <- tryCatch(
      osf_get_all_pages(sprintf("%s/nodes/%s/files/%s/", osf_api, node, p)),
      error = \(e) NULL)
    if (is.null(listing) || !is.data.frame(listing) || nrow(listing) == 0) next

    info <- .osf_parse_response(listing)
    if (is.null(info) || nrow(info) == 0) next

    # A provider listing is one level deep; anything below is a folder that
    # has to be walked the same way.
    while (any(!is.na(info$files))) {
      more <- lapply(stats::na.omit(info$files), \(u) {
        tryCatch(osf_get_all_pages(u), error = \(e) NULL)
      })
      more <- more[!vapply(more, is.null, logical(1))]
      if (length(more) == 0) break
      parsed <- lapply(more, .osf_parse_response) |> dplyr::bind_rows()
      if (nrow(parsed) == 0) break
      info$files <- NA_character_
      info <- dplyr::bind_rows(info, parsed)
    }

    fl <- info[info$kind %in% "file" & !is.na(info$download_url), ]
    if (nrow(fl) == 0) next

    dir.create(node_dir, showWarnings = FALSE, recursive = TRUE)
    dests <- file.path(node_dir, p,
                       path_sanitize(sub("^/+", "", fl$path %||% fl$name)))
    for (d in unique(dirname(dests))) {
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
    }

    # No expected size is passed: for an add-on the OSF reports the size it
    # recorded when it last indexed the external repository, which goes stale
    # as soon as the file changes there (see .download_many_parallel()).
    errs <- .download_many_parallel(fl$download_url, dests,
                                    rep(NA_real_, nrow(fl)))
    ok <- which(is.na(errs))
    got <- got + length(ok)
    if (length(ok) > 0) {
      bytes <- bytes + sum(file.size(dests[ok]), na.rm = TRUE)
    }
  }

  list(files = got, bytes = bytes)
}

#' Download every file in an OSF project, as one archive per node
#'
#' The implementation of `osf_file_download(mode = "all")`.
#'
#' @param osf_id the OSF node ID
#' @param download_to the folder to download into
#' @param metadata whether to also retrieve wikis, logs and node metadata
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame with one row per node, reporting what arrived
#' @keywords internal
.osf_download_all <- function(osf_id, download_to, metadata = TRUE,
                              pb = NULL) {
  mb <- 1024 * 1024

  nodes <- .osf_walk_nodes(osf_id, pb = pb)
  message(sprintf("%s: %d node%s to download", osf_id, nrow(nodes),
                  plural(nrow(nodes))))

  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  if (dir.exists(download_to)) download_to <- file.path(download_to, osf_id)
  dir.create(download_to, showWarnings = FALSE, recursive = TRUE)

  results <- vector("list", nrow(nodes))

  for (i in seq_len(nrow(nodes))) {
    node <- nodes$osf_id[[i]]
    # Folders are named after the component, falling back to its ID when the
    # title could not be read. The ID is appended so two components sharing a
    # title do not collide, and so a folder can always be traced back.
    title <- nodes$title[[i]]
    folder <- if (is.na(title) || !nzchar(title)) node else
      paste0(path_sanitize(title, keep_sep = FALSE), "_", node)
    node_dir <- file.path(download_to, folder)

    # No resume here, deliberately.
    #
    # Resuming needs a way to tell what is already correct on disk, and this
    # mode has none: it never lists the files, so it does not know what the
    # component should contain. Reading the archive's index without downloading
    # it would answer that -- zip_peek() does exactly this for other hosts by
    # asking for the tail of the file with an HTTP Range request -- but the OSF
    # does not support it on this endpoint. Verified 2026-08-13: a HEAD returns
    # 501, and a Range request returns 200 with the whole archive rather than
    # 206 with the requested bytes, so zip_peek() returns NULL for it.
    #
    # Skipping a component merely because its folder is non-empty would be
    # wrong: a run interrupted midway through unpacking leaves a partial folder
    # that looks finished. So each run fetches every archive again. Use
    # `mode = "select"` when a resumable download matters more than speed.
    sprintf("Downloading %s (%d of %d)", folder, i, nrow(nodes)) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    zip_url <- sprintf(
      "https://files.osf.io/v1/resources/%s/providers/osfstorage/?zip=", node)
    zip_path <- file.path(download_to, paste0(node, ".zip"))

    ok <- tryCatch({
      resp <- httr2::request(zip_url) |>
        .osf_headers() |>
        httr2::req_timeout(1800) |>
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                         is_transient = .storage_is_transient,
                         backoff = .storage_backoff) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform(path = zip_path)
      httr2::resp_status(resp) == 200
    }, error = \(e) FALSE)

    n_files <- 0L
    bytes <- 0
    if (isTRUE(ok) && file.exists(zip_path) && file.size(zip_path) > 0) {
      # An empty archive is 22 bytes (a zip with no entries), which a node with
      # no osfstorage files legitimately returns.
      entries <- tryCatch(utils::unzip(zip_path, list = TRUE), error = \(e) NULL)
      if (!is.null(entries) && nrow(entries) > 0) {
        dir.create(node_dir, showWarnings = FALSE, recursive = TRUE)
        tryCatch(utils::unzip(zip_path, exdir = node_dir), error = \(e) NULL)
        on_disk <- list.files(node_dir, recursive = TRUE, all.files = FALSE)
        on_disk <- on_disk[!dir.exists(file.path(node_dir, on_disk))]
        n_files <- length(on_disk)
        bytes <- sum(file.size(file.path(node_dir, on_disk)), na.rm = TRUE)
      }
      unlink(zip_path)
    } else {
      unlink(zip_path)
    }

    # Files on a linked add-on (GitHub, Dropbox, and so on) are not in the OSF
    # archive -- ?zip= covers osfstorage only, and returns 404 for any other
    # provider. Without this, "all" would quietly return part of the project:
    # pngda has 57 files, of which 29 sit on a GitHub add-on, so the archive
    # alone yields 28. Those files are listed and fetched individually, which
    # costs one listing request per add-on and is the only way to get them.
    addon <- .osf_download_addons(node, node_dir, pb = pb)

    # Count what is actually in the folder, once, after everything has been
    # written. Adding the archive's count to the add-on's double-counts
    # anything both routes touched and misses anything neither reported, which
    # is how a node with 29 files on disk came to be reported as 28.
    if (dir.exists(node_dir)) {
      present <- list.files(node_dir, recursive = TRUE, all.files = FALSE)
      present <- present[!dir.exists(file.path(node_dir, present))]
      n_files <- length(present)
      bytes <- sum(file.size(file.path(node_dir, present)), na.rm = TRUE)
    }

    results[[i]] <- data.frame(
      folder = folder,
      osf_project = node,
      osf_url = paste0("https://osf.io/", node),
      title = title,
      files = n_files,
      bytes = bytes,
      download_path = if (n_files > 0) node_dir else NA_character_,
      downloaded = isTRUE(ok)
    )
  }

  out <- dplyr::bind_rows(results)

  failed <- which(!out$downloaded)
  if (length(failed) > 0) {
    warning(sprintf(
      "%d of %d node%s could not be downloaded (e.g. %s). Rerun to try again.",
      length(failed), nrow(out), plural(nrow(out)),
      paste(utils::head(out$osf_project[failed], 3), collapse = ", ")),
      call. = FALSE)
  }

  if (isTRUE(metadata)) {
    for (i in seq_len(nrow(out))) {
      target <- out$download_path[[i]]
      if (is.na(target)) next
      tryCatch(.osf_metadata_download(out$osf_project[[i]], target, pb = pb),
               error = \(e) NULL)
    }
  }

  message(sprintf("%s: %d file%s in %d node%s, %s",
                  osf_id, sum(out$files), plural(sum(out$files)),
                  sum(out$files > 0), plural(sum(out$files > 0)),
                  .cap_size_str(sum(out$bytes, na.rm = TRUE))))

  invisible(out)
}
