# Figshare (https://figshare.com) is a single hosted service (unlike
# Dataverse's many independent installations), so detection here keys off one
# host the same way zenodo_links() does -- an institutional "white-label"
# instance (e.g. tandf.figshare.com) is also matched, since Figshare's API is
# reached at the same api.figshare.com regardless of which branded subdomain a
# reader followed.
#
# Every article is reachable through the same Figshare REST API
# (https://docs.figshare.com/), verified live 2026-08-16 against article
# 18093368 (api.figshare.com/v2/articles/18093368): field names below
# (title/doi/authors/license/files) were read from that real response, not
# guessed from documentation.
#
# Every function below takes a `host` argument (default "api.figshare.com")
# because 4TU.ResearchData's Djehuty platform (data.4tu.nl, DOI prefix
# 10.4121) is a documented backward-compatible implementation of this same
# Figshare v2 API -- verified live 2026-08-16: GET data.4tu.nl/v2/articles/
# <id-or-uuid> returns the identical field set (title/doi/authors/license/
# files, files[].id/name/size/download_url/computed_md5) as api.figshare.com,
# just at a different host and accepting either a numeric id or a uuid for the
# same article. See archive-4tu.R for the thin researchdata4tu_*() wrappers
# that call these functions with host = "data.4tu.nl".

#' Find Figshare Links in Papers
#'
#' Get all Figshare links: real hyperlinks from the paper's own `url` table,
#' plus a body-text fallback for a BARE mention (a DOI like
#' "10.6084/m9.figshare.1234567" is routinely cited without any URL scheme at
#' all) that the source PDF/HTML never encoded as an actual hyperlink -- the
#' `url` table only ever contains links the source document itself made
#' clickable. Same two-tier approach `zenodo_links()` uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Figshare url in the first (text) column
#' @export
#'
#' @examples
#' figshare_links(psychsci)
figshare_links <- function(paper) {
  href <- text <- NULL

  # A branded instance (tandf.figshare.com, rsc.figshare.com, ...) is still
  # Figshare -- every one of them is served by the same api.figshare.com, so
  # the host match is deliberately "any subdomain of figshare.com" rather than
  # a fixed list the way Dataverse needs one for its independent installations.
  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("figshare\\.com|10\\.6084/m9\\.figshare", href, ignore.case = TRUE))

  fs_bare_regex <- paste0(
    "(?:https?://)?(?:[a-z0-9.-]+\\.)?figshare\\.com/(?:articles|ndownloader)/[A-Za-z0-9/_.-]*",
    "|(?:https?://)?(?:doi\\.org/)?10\\.6084/m9\\.figshare\\.[0-9]+(?:\\.v[0-9]+)?"
  )
  other_fs <- text_search(paper, fs_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_fs) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$figshare_url <- links$href
  links$figshare_id <- .figshare_id(links$figshare_url)

  return(links)
}

#' Get Figshare article ID from URL or DOI
#'
#' @param figshare_url a vector of URLs or DOIs to figshare articles
#'
#' @returns a character vector of numeric article IDs
#' @keywords internal
.figshare_id <- function(figshare_url) {
  if (length(figshare_url) == 0) {
    return(character(0))
  }

  if (length(figshare_url) > 1) {
    return(vapply(figshare_url, .figshare_id, character(1)))
  }

  # handle single figshare_url ----
  figshare_url <- trimws(as.character(figshare_url))

  if (is.na(figshare_url) || !nzchar(figshare_url)) {
    return(NA_character_)
  }

  if (grepl("^[0-9]+$", figshare_url)) {
    return(figshare_url)
  }

  # A DOI carries the article id as its final segment, optionally followed by
  # a version (10.6084/m9.figshare.18093368.v1) -- the version is dropped, so
  # .figshare_id() always resolves to the latest version via the plain
  # /articles/{id} endpoint, matching how .zenodo_id() drops nothing (Zenodo
  # has no separate version suffix) but is otherwise the same pattern family.
  patterns <- c(
    "10\\.6084/m9\\.figshare\\.([0-9]+)",
    "figshare\\.com/articles/(?:dataset|[a-z]+)/[^/]+/([0-9]+)",
    "figshare\\.com/articles/([0-9]+)",
    "ndownloader\\.figshare\\.com/files/([0-9]+)"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, figshare_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(figshare_url, match)[[1]]
    if (length(groups) >= 2) {
      return(groups[[2]])
    }
  }

  return(NA_character_)
}

#' Retrieve info from Figshare by URL
#'
#' @param figshare_url a Figshare URL or DOI, or a table containing them
#'   (e.g., as created by [figshare_links()])
#' @param id_col the index or name of the column that contains Figshare URLs,
#'   if `figshare_url` is a table
#' @param host the Figshare-compatible API host to query. Default
#'   `"api.figshare.com"`; [researchdata4tu_info()] calls this with
#'   `"data.4tu.nl"` instead (see the note at the top of this file).
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   figshare_info("https://doi.org/10.6084/m9.figshare.18093368.v1")
#' }
figshare_info <- function(figshare_url, id_col = 1, host = "api.figshare.com", pb = NULL) {
  if (!online(host)) {
    stop(host, " seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Figshare Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(figshare_url)) {
    table <- figshare_url
    table$figshare_url <- table[[id_col]]
  } else {
    raw_urls <- unique(figshare_url) |> stats::na.omit()
    table <- data.frame(figshare_url = raw_urls)
  }

  ids <- data.frame(
    figshare_url = table$figshare_url,
    figshare_id = .figshare_id(table$figshare_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$figshare_url), , drop = FALSE]
  valid_ids <- unique(stats::na.omit(ids$figshare_id))

  if (length(valid_ids) == 0) {
    ("No valid Figshare links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "figshare_url"))
  }

  paste0(
    "Starting Figshare retrieval for ",
    length(valid_ids), " article",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  for (i in seq_along(valid_ids)) {
    id_info[[i]] <- .figshare_info(valid_ids[[i]], host = host, pb = pb)
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "figshare_url") |>
    dplyr::left_join(info, by = "figshare_id", suffix = c("", ".figshare"))

  paste0("...Figshare retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one Figshare article
#'
#' @param figshare_id a Figshare article ID
#' @param host the Figshare-compatible API host to query
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @keywords internal
.figshare_info <- function(figshare_id, host = "api.figshare.com", pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from Figshare article ", figshare_id, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(figshare_id = as.character(figshare_id))

  api_url <- sprintf("https://%s/v2/articles/%s", host, figshare_id)

  resp <- .batch_query(api_url, msg = NULL,
                       req_func = \(req) .figshare_headers(req, host = host))[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(figshare_id, " could not be found on ", host, call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  rec <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
  if (is.null(rec)) {
    obj$error <- "parse_error"
    return(obj)
  }

  authors_field <- rec$authors
  authors <- if (is.list(authors_field)) {
    vapply(authors_field, function(a) a$full_name %||% NA_character_, character(1))
  } else {
    character(0)
  }

  obj$title <-            rec$title %||% NA_character_
  obj$doi <-              rec$doi %||% NA_character_
  obj$publication_date <- rec$published_date %||% NA_character_
  obj$updated_date <-     rec$modified_date %||% NA_character_
  obj$authors <-          list(authors)
  obj$license <-          rec$license$name %||% NA_character_
  obj$files <-            list(rec$files %||% list())

  return(obj)
}

# Add the right auth header to a Figshare-compatible API/download request.
# Public articles are readable without a token (verified live against
# article 18093368: "is_public": true, retrieved with no Authorization
# header); a token only raises rate limits and unlocks private articles, so
# it is added whenever one is set rather than being required.
#
# `host` selects WHICH token to look up: real Figshare uses figshare_pat(),
# while 4TU.ResearchData (host = "data.4tu.nl") uses its own separate
# researchdata4tu_pat() -- a token from one is meaningless to the other (see
# archive-4tu.R). This function does not itself dispatch on `host`: it is
# only ever called with `host = "data.4tu.nl"` via researchdata4tu_*()'s
# withr::local_options() override of figshare_pat()'s own option (or, for a
# plain file URL outside that call chain, .auth_for_url() in
# repo-download.R looks up .researchdata4tu_pat() directly instead of
# calling this function at all) -- `host` is accepted here only so callers
# in archive-figshare.R and .figshare_info() have one consistent signature
# to call regardless of which host they are querying.
.figshare_headers <- function(req, host = "api.figshare.com") {
  req <- req |> httr2::req_headers(`User-Agent` = "metacheck")
  pat <- tryCatch(figshare_pat(), error = \(e) "")
  if (nzchar(pat %||% "")) {
    req <- req |> httr2::req_headers(Authorization = sprintf("token %s", pat))
  }
  req
}

#' Set or get a Figshare API token
#'
#' Figshare issues personal access tokens from account settings
#' (https://figshare.com/account/applications). A token is optional for
#' reading public articles (used here only to raise rate limits or read
#' private ones), unlike Dataverse where a token is per-installation.
#'
#' Store it as the `FIGSHARE_PAT` environment variable so it is read every
#' time R starts.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' figshare_pat() # returns "" unless a token is set
figshare_pat <- function(pat = NULL) {
  .figshare_pat(pat)
}

.figshare_pat <- function(pat = NULL) {
  opt <- "metacheck.figshare.pat"
  env <- "FIGSHARE_PAT"

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set figshare_pat with a single string containing your Figshare token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Download all files from a Figshare article
#'
#' Creates a directory for the article and downloads all of its files.
#' Returns (invisibly) a data frame with file info.
#'
#' Unlike Dataverse, OSF, or Zenodo, Figshare's v2 API documents no
#' whole-article bulk-download endpoint (verified 2026-08-16: no
#' `ndownloader.figshare.com/articles/...` zip route appears anywhere in
#' https://docs.figshare.com/) -- every file's `download_url` is its own
#' request, so this always downloads file by file. A `.zip` already present in
#' the article is still handled the same way `dataverse_file_download()`
#' handles one: fetched whole and left as a zip unless `unzip_types` asks for
#' only specific members out of it.
#'
#' You can limit downloads to only files under a specific size (defaults to
#' 10MB) and only a maximum download size (largest files will be omitted until
#' total size is under the limit). Omitted files will be listed as messages in
#' verbose mode, and included in the returned data frame with the `downloaded`
#' column value set to FALSE.
#'
#' @param figshare_id a Figshare article ID, URL, or DOI
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL or
#'   Inf for no restrictions
#' @param max_download_size maximum total size to download - set to NULL or
#'   Inf for no restrictions
#' @param unzip_types file categories to extract from `.zip` files in the
#'   article rather than downloading the zip whole, named as
#'   [data_classify_files()] names them: `"data"`, `"code"`, `"materials"`,
#'   `"documentation"`, `"output"`, `"unknown"`. More than one may be given.
#'   `NULL` (the default) downloads zips whole and leaves them zipped, as
#'   before. `max_file_size` applies to each extracted member rather than to
#'   the archive.
#' @param host the Figshare-compatible API host to query. Default
#'   `"api.figshare.com"`; [researchdata4tu_file_download()] calls this with
#'   `"data.4tu.nl"` instead (see the note at the top of this file).
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info. When members are extracted from a zip, its
#'   row reports the zip with `extracted` giving the number of members written;
#'   `size_on_disk` and `checksum_ok` are then `NA`, because what is on disk is
#'   the members rather than the archive Figshare published a size and MD5 for.
#' @export
#'
#' @examples
#' \dontrun{
#'   figshare_file_download("18093368")
#'
#'   # take only the data files out of any zip in the article
#'   figshare_file_download("18093368", unzip_types = "data")
#' }
figshare_file_download <- function(figshare_id,
                                   download_to = ".",
                                   max_file_size = 10,
                                   max_download_size = 100,
                                   unzip_types = NULL,
                                   host = "api.figshare.com",
                                   pb = NULL) {
  figshare_id <- .figshare_id(figshare_id) |>
    stats::na.omit() |>
    unique()
  if (length(figshare_id) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Figshare File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple articles ----
  if (length(figshare_id) > 1) {
    paste0(
      "Starting downloads for ", length(figshare_id), " Figshare article",
      ifelse(length(figshare_id) == 1, "", "s"), "...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(figshare_id, function(x) {
      tryCatch(
        figshare_file_download(
          x,
          download_to = download_to,
          max_file_size = max_file_size,
          max_download_size = max_download_size,
          unzip_types = unzip_types,
          host = host,
          pb = pb
        ),
        error = function(e) {
          warning(x, " resulted in an error:\n  ", conditionMessage(e), "\n")
          return(NULL)
        }
      )
    })

    dl_list <- dl_list[!vapply(dl_list, is.null, logical(1))]
    if (length(dl_list) == 0) return(NULL)

    dl <- dplyr::bind_rows(dl_list)
    paste0(
      "...Completed downloads for ", length(figshare_id), " Figshare article",
      ifelse(length(figshare_id) == 1, "", "s")
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  # --- retrieve article contents ----
  paste0("Starting retrieval for Figshare article ", figshare_id) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(.figshare_info(figshare_id, host = host, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", figshare_id, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from entries (verified live field names: id, name,
  # size, computed_md5, download_url -- see .figshare_info()).
  rows <- lapply(files_list, function(x) {
    dplyr::tibble(
      id       = as.character(x$id %||% NA_character_),
      key      = x$name %||% NA_character_,
      size     = as.numeric(x$size %||% NA_real_),
      checksum = x$computed_md5 %||% NA_character_,
      self     = x$download_url %||% NA_character_
    )
  })

  files <- dplyr::bind_rows(rows)
  if (nrow(files) == 0) {
    message("- ", figshare_id, " contained no files")
    return(NULL)
  }

  # A zip we were asked to look inside is exempt from the size caps below, for
  # the same reason zenodo_file_download() exempts it: the caps measure the
  # file Figshare would send, and for these we have decided not to transfer
  # that -- only selected members are fetched, each capped individually by
  # `max_file_size` at extraction time.
  unzippable <- rep(FALSE, nrow(files))
  if (!is.null(unzip_types) && length(unzip_types) > 0)
    unzippable <- .is_zip(files$key) & !is.na(files$self) & nzchar(files$self %||% "")

  # --- size filters (MB) ----
  if (!is.null(max_file_size) && is.finite(max_file_size) && max_file_size > 0) {
    too_big_files <- which(files$size > max_file_size * 1024 * 1024 & !unzippable)
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
      unzippable <- unzippable[-too_big_files]
    }
  }

  if (!is.null(max_download_size) && is.finite(max_download_size) && max_download_size > 0) {
    while (any(!unzippable) &&
           sum(files$size[!unzippable], na.rm = TRUE) > max_download_size * 1024 * 1024) {
      capped <- which(!unzippable)
      max_file <- capped[which.max(files$size[capped])]
      paste0(
        "- omitting ", files$key[[max_file]],
        " (", round(files$size[[max_file]] / 1024 / 1024, 1), "MB)"
      ) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      files <- files[-max_file, , drop = FALSE]
      unzippable <- unzippable[-max_file]
    }
  }

  if (nrow(files) == 0) {
    paste0("- All files omitted due to size constraints") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # --- target directory (avoid overwrite) ----
  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  folder_name <- paste0("figshare_", figshare_id)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, folder_name)
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
  temppath <- tempfile()
  on.exit(unlink(temppath, recursive = TRUE))
  dir.create(temppath)

  n <- nrow(files)
  files$downloaded <- FALSE
  files$extracted <- NA_integer_

  for (i in seq_len(n)) {
    # --- selected members out of a zip, instead of the whole zip ----
    if (unzippable[i]) {
      paste0("Reading zip contents of ", files$key[[i]]) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      got <- tryCatch(
        .figshare_zip_members(files$self[[i]], dest = download_to,
                              keep_types = unzip_types,
                              max_file_size = max_file_size),
        error = \(e) NULL)
      if (!is.null(got)) {
        files$extracted[i] <- sum(got$ok %in% TRUE)
        files$downloaded[i] <- TRUE
        paste0("- extracted ", files$extracted[i], " file",
               plural(files$extracted[i]), " from ", files$key[[i]]) |>
          list(what = _) |>
          pb$tick(0, tokens = _)
        next
      }
      paste0("- could not read ", files$key[[i]],
             " without downloading it; fetching the whole archive") |>
        list(what = _) |>
        pb$tick(0, tokens = _)
    }

    ok <- FALSE
    if (!is.na(files$self[[i]]) && nzchar(files$self[[i]])) {
      target_path <- file.path(temppath, files$id[[i]])
      resp <- tryCatch(
        {
          httr2::request(files$self[[i]]) |>
            .figshare_headers() |>
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
  files$path <- NA_character_
  for (i in seq_len(nrow(files))) {
    if (!is.na(files$extracted[i])) next
    if (isTRUE(files$downloaded[i])) {
      from <- file.path(temppath, files$id[[i]])
      fname <- if (!is.na(files$key[[i]]) && nzchar(files$key[[i]])) files$key[[i]] else files$id[[i]]
      to <- file.path(download_to, fname)
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      file.copy(from, to, overwrite = TRUE)
      files$path[[i]] <- fname
    }
  }

  # --- verify what actually reached the disk ----
  files <- .figshare_verify_downloads(files, download_to)

  n_missing <- sum(!files$downloaded)
  if (n_missing > 0) {
    worst <- files[!files$downloaded, ]
    warning(sprintf(
      "%d of %d file%s from Figshare article %s did not arrive intact (e.g. %s). The returned table marks %s downloaded = FALSE. Run again to retry.",
      n_missing, nrow(files), plural(nrow(files)), figshare_id,
      paste(utils::head(worst$key, 3), collapse = ", "),
      if (n_missing == 1) "it" else "them"), call. = FALSE)
  }

  # --- return table ----
  files$folder <- basename(download_to)
  files$figshare_id <- as.character(figshare_id)
  files <- files[, c("folder", "figshare_id", "id", "key", "path", "size",
                     "size_on_disk", "checksum", "checksum_ok", "self",
                     "downloaded", "extracted")]

  invisible(files)
}

#' Fetch selected files out of a .zip in a Figshare article without downloading it
#'
#' A zip published on Figshare is often mostly material a reader does not
#' need: stimuli, images, or videos alongside the few data files. Figshare
#' serves its per-file download URLs with byte-range support and every member
#' of a zip is compressed on its own, so individual members can be pulled out
#' of the archive while the rest is never transferred (see
#' `.zip_member_fetch()`, shared with the identical Zenodo/Dataverse path).
#'
#' @param url the file's `download_url`
#' @param dest directory to write the extracted members into
#' @param keep_types file categories worth extracting, as
#'   [data_classify_files()] names them; the default keeps data and
#'   documentation and skips materials, matching [zip_decision()]
#' @param max_file_size largest member to extract (in MB), applied per member
#'   exactly as the article-level cap is applied per file
#'
#' @returns a data frame with one row per extracted member (`name`, `path`,
#'   `size`, `ok`), or `NULL` when the archive could not be listed and the
#'   caller should download it whole instead
#' @keywords internal
.figshare_zip_members <- function(url, dest,
                                  keep_types = c("data", "documentation"),
                                  max_file_size = 10) {
  listing <- tryCatch(zip_peek(url), error = \(e) NULL)
  if (is.null(listing) || nrow(listing) == 0) return(NULL)

  types <- data_classify_files(basename(listing$name))
  keep <- types %in% keep_types

  if (!is.null(max_file_size) && is.finite(max_file_size) && max_file_size > 0)
    keep <- keep & !is.na(listing$size) &
      listing$size <= max_file_size * 1024 * 1024

  if (!any(keep)) return(listing[0, c("name", "size"), drop = FALSE])

  .zip_fetch_members(url, names = listing$name[keep], dest = dest)
}

#' Check downloaded Figshare files against the file system
#'
#' Mirrors [`.zenodo_verify_downloads()`]/[`.dataverse_verify_downloads()`]:
#' confirms every file the download planned to save is present, is the size
#' Figshare reported, and (when Figshare published a computed_md5) matches
#' the checksum.
#'
#' A row whose `extracted` count is set is exempt from both checks, for the
#' same reason as the Zenodo/Dataverse case: Figshare's size and checksum
#' describe the zip as published, and that zip was deliberately never
#' downloaded -- only chosen members were, each verified against the CRC32 in
#' the archive's own central directory as it is extracted.
#'
#' @param files the file table being built, with `path`, `size`, `checksum`,
#'   and `downloaded` columns, and optionally `extracted`
#' @param download_to the folder the article was saved in
#'
#' @returns `files` with `downloaded` corrected against the file system, plus
#'   `size_on_disk` and `checksum_ok` columns
#' @keywords internal
.figshare_verify_downloads <- function(files, download_to) {
  if (is.null(files) || nrow(files) == 0) return(files)

  unzipped <- if ("extracted" %in% names(files)) !is.na(files$extracted)
              else rep(FALSE, nrow(files))

  files$size_on_disk <- NA_real_
  files$checksum_ok <- NA

  if (!"path" %in% names(files)) {
    files$downloaded <- FALSE
    return(files)
  }

  has_path <- !is.na(files$path)
  full <- rep(NA_character_, nrow(files))
  full[has_path] <- file.path(download_to, files$path[has_path])

  on_disk <- rep(FALSE, nrow(files))
  on_disk[has_path] <- file.exists(full[has_path]) & !dir.exists(full[has_path])
  files$size_on_disk[on_disk] <- file.size(full[on_disk])

  ok <- on_disk & !is.na(files$size_on_disk)

  expected <- suppressWarnings(as.numeric(files$size %||% rep(NA_real_, nrow(files))))
  ok <- ok & (is.na(expected) | files$size_on_disk == expected) %in% TRUE

  # Figshare's computed_md5 is always MD5 (unlike Dataverse, which may publish
  # SHA-1 instead), so no type check is needed before hashing.
  to_hash <- which(ok & !is.na(files$checksum) &
                     grepl("^[0-9a-f]{32}$", files$checksum, ignore.case = TRUE))
  for (i in to_hash) {
    got <- tryCatch(unname(tools::md5sum(full[[i]])), error = \(e) NA_character_)
    files$checksum_ok[[i]] <- identical(tolower(got), tolower(files$checksum[[i]]))
  }
  ok <- ok & !(files$checksum_ok %in% FALSE)

  files$downloaded <- ok & files$downloaded %in% TRUE

  if (any(unzipped)) files$downloaded[unzipped] <- TRUE
  files
}
