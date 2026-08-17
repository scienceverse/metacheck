# Dryad (https://datadryad.org) is a single hosted service (unlike
# Dataverse's many independent installations), so detection here keys off one
# host the same way zenodo_links() does.
#
# Verified live 2026-08-16 against a real dataset (doi:10.5061/dryad.j1fd7,
# api.figshare.com's counterpart at datadryad.org/api/v2/datasets/...): field
# names below (identifier/title/authors/publicationDate/lastModificationDate/
# license, and per-file path/size/mimeType/digest/digestType) were read from
# that real JSON response, not guessed from documentation. The dataset JSON
# also carries `_links.stash:download.href`, Dryad's documented whole-dataset
# bulk-download endpoint (the same link a dataset page's own "Download
# Dataset" button uses) -- unlike Figshare, which has no such endpoint.

#' Find Dryad Links in Papers
#'
#' Get all Dryad links: real hyperlinks from the paper's own `url` table,
#' plus a body-text fallback for a BARE mention (a DOI like
#' "10.5061/dryad.j1fd7" is routinely cited without any URL scheme at all)
#' that the source PDF/HTML never encoded as an actual hyperlink -- the `url`
#' table only ever contains links the source document itself made clickable.
#' Same two-tier approach `zenodo_links()` uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Dryad url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' dryad_links(psychsci)
#' }
dryad_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("datadryad\\.org|10\\.5061/dryad", href, ignore.case = TRUE))

  dryad_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?datadryad\\.org/(?:stash/)?dataset[s]?/doi[:%]",
    "[A-Za-z0-9%._/-]*",
    "|(?:https?://)?(?:doi\\.org/)?10\\.5061/dryad\\.[A-Za-z0-9]+"
  )
  other_dryad <- text_search(paper, dryad_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_dryad) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$dryad_url <- links$href
  links$dryad_doi <- .dryad_doi(links$dryad_url)

  return(links)
}

#' Get Dryad DOI from URL
#'
#' @param dryad_url a vector of URLs or DOIs to dryad datasets
#'
#' @returns a character vector of DOIs in "10.5061/dryad.xxxxx" form
#' @keywords internal
.dryad_doi <- function(dryad_url) {
  if (length(dryad_url) == 0) {
    return(character(0))
  }

  if (length(dryad_url) > 1) {
    return(vapply(dryad_url, .dryad_doi, character(1)))
  }

  # handle single dryad_url ----
  dryad_url <- trimws(as.character(dryad_url))

  if (is.na(dryad_url) || !nzchar(dryad_url)) {
    return(NA_character_)
  }

  dryad_url <- utils::URLdecode(dryad_url)

  match <- regexec("(10\\.5061/dryad\\.[A-Za-z0-9]+)", dryad_url, perl = TRUE,
                   ignore.case = TRUE)
  groups <- regmatches(dryad_url, match)[[1]]
  if (length(groups) >= 2) {
    return(tolower(groups[[2]]))
  }

  return(NA_character_)
}

#' Retrieve info from Dryad by URL
#'
#' @param dryad_url a Dryad dataset URL or DOI, or a table containing them
#'   (e.g., as created by [dryad_links()])
#' @param id_col the index or name of the column that contains Dryad URLs, if
#'   `dryad_url` is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   dryad_info("https://doi.org/10.5061/dryad.j1fd7")
#' }
dryad_info <- function(dryad_url, id_col = 1, pb = NULL) {
  if (!online("datadryad.org")) {
    stop("Dryad seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Dryad Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(dryad_url)) {
    table <- dryad_url
    table$dryad_url <- table[[id_col]]
  } else {
    raw_urls <- unique(dryad_url) |> stats::na.omit()
    table <- data.frame(dryad_url = raw_urls)
  }

  ids <- data.frame(
    dryad_url = table$dryad_url,
    dryad_doi = .dryad_doi(table$dryad_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$dryad_url), , drop = FALSE]
  valid_dois <- unique(stats::na.omit(ids$dryad_doi))

  if (length(valid_dois) == 0) {
    ("No valid Dryad links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "dryad_url"))
  }

  paste0(
    "Starting Dryad retrieval for ",
    length(valid_dois), " dataset",
    ifelse(length(valid_dois) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_dois))
  for (i in seq_along(valid_dois)) {
    id_info[[i]] <- .dryad_info(valid_dois[[i]], pb = pb)
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "dryad_url") |>
    dplyr::left_join(info, by = "dryad_doi", suffix = c("", ".dryad"))

  paste0("...Dryad retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one Dryad dataset
#'
#' @param dryad_doi the dataset's DOI (e.g. "10.5061/dryad.j1fd7")
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @keywords internal
.dryad_info <- function(dryad_doi, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from Dryad DOI ", dryad_doi, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(dryad_doi = dryad_doi)

  # Both the "doi:" colon and the DOI's own slash must be percent-encoded in
  # the path (verified live against doi:10.5061/dryad.j1fd7, reachable only as
  # .../datasets/doi%3A10.5061%2Fdryad.j1fd7).
  encoded <- utils::URLencode(paste0("doi:", dryad_doi), reserved = TRUE)
  api_url <- sprintf("https://datadryad.org/api/v2/datasets/%s", encoded)

  resp <- .batch_query(api_url, msg = NULL, req_func = .dryad_headers)[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(dryad_doi, " could not be found on Dryad", call. = FALSE)
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
    vapply(authors_field, function(a) {
      full <- trimws(paste(a$firstName %||% "", a$lastName %||% ""))
      if (nzchar(full)) full else NA_character_
    }, character(1))
  } else {
    character(0)
  }

  # The files listing sits under the dataset's VERSION, not the dataset
  # itself (verified live: dataset JSON links to "stash:version" ->
  # /api/v2/versions/<id>, and only THAT resource links on to "stash:files").
  version_href <- rec$`_links`$`stash:version`$href
  files_list <- list()
  if (!is.null(version_href)) {
    files_resp <- .batch_query(paste0("https://datadryad.org", version_href, "/files"),
                               msg = NULL, req_func = .dryad_headers)[[1]]
    if (!is.null(files_resp) && httr2::resp_status(files_resp) == 200) {
      files_rec <- tryCatch(httr2::resp_body_json(files_resp), error = \(e) NULL)
      files_list <- files_rec$`_embedded`$`stash:files` %||% list()
    }
  }

  obj$title <-            rec$title %||% NA_character_
  obj$doi <-              rec$identifier %||% NA_character_
  obj$publication_date <- rec$publicationDate %||% NA_character_
  obj$updated_date <-     rec$lastModificationDate %||% NA_character_
  obj$authors <-          list(authors)
  obj$license <-          rec$license %||% NA_character_
  obj$files <-            list(files_list)

  return(obj)
}

# Add the right auth header to a Dryad API/download request. Dryad uses a
# single OAuth2 bearer token (unlike Dataverse's per-installation key).
#
# Dataset METADATA and file LISTINGS are readable without a token, even for a
# public dataset -- verified live against doi:10.5061/dryad.j1fd7. Downloading
# FILE BYTES is not: both /api/v2/files/<id>/download and the dataset-level
# /api/v2/datasets/<doi>/download bulk endpoint answered 401 with no token,
# even for that same fully public dataset (verified live 2026-08-16). This
# matches Dryad's own documentation ("Some API calls, such as ... downloading
# files, require a token for any call" -- api_accounts.md), so it is expected
# behaviour, not a bug: a token is REQUIRED here, unlike Figshare/Dataverse/
# Zenodo/OSF where a token is optional for public content.
.dryad_headers <- function(req) {
  req <- req |> httr2::req_headers(`User-Agent` = "metacheck")
  pat <- tryCatch(dryad_pat(), error = \(e) "")
  if (nzchar(pat %||% "")) {
    req <- req |> httr2::req_headers(Authorization = sprintf("Bearer %s", pat))
  }
  req
}

#' Set or get a Dryad API token
#'
#' Dryad issues OAuth2 bearer tokens from account settings. Unlike Figshare,
#' Dataverse, Zenodo, and OSF, a token is REQUIRED to download file bytes even
#' from a fully public dataset (verified live 2026-08-16: both the per-file
#' and whole-dataset download endpoints answer 401 with no token, though
#' dataset metadata and file listings are readable without one). Without a
#' token, [dryad_file_download()] can list what a dataset contains but cannot
#' fetch any of it.
#'
#' Store it as the `DRYAD_PAT` environment variable so it is read every time
#' R starts.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' dryad_pat() # returns "" unless a token is set
dryad_pat <- function(pat = NULL) {
  .dryad_pat(pat)
}

.dryad_pat <- function(pat = NULL) {
  opt <- "metacheck.dryad.pat"
  env <- "DRYAD_PAT"

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set dryad_pat with a single string containing your Dryad token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Download all files from a Dryad dataset
#'
#' Creates a directory for the dataset and downloads all of its files.
#' Returns (invisibly) a data frame with file info.
#'
#' A Dryad API token (see [dryad_pat()]) is REQUIRED to download file bytes,
#' even from a fully public dataset -- unlike [figshare_file_download()],
#' [dataverse_file_download()], or [zenodo_file_download()]. Without one set,
#' every file in the returned table has `downloaded = FALSE`: the dataset can
#' still be listed (see [dryad_info()]), but nothing can be fetched.
#'
#' You can limit downloads to only files under a specific size (defaults to
#' 10MB) and only a maximum download size (largest files will be omitted until
#' total size is under the limit). Omitted files will be listed as messages in
#' verbose mode, and included in the returned data frame with the `downloaded`
#' column value set to FALSE.
#'
#' A `.zip` in the dataset is normally fetched whole and left as a zip. Set
#' `unzip_types` to pull only the files you want out of it instead, the same
#' as [dataverse_file_download()] and [zenodo_file_download()] do.
#'
#' @param dryad_doi the dataset's DOI, or a URL containing it
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL or
#'   Inf for no restrictions
#' @param max_download_size maximum total size to download - set to NULL or
#'   Inf for no restrictions
#' @param unzip_types file categories to extract from `.zip` files in the
#'   dataset rather than downloading the zip whole, named as
#'   [data_classify_files()] names them: `"data"`, `"code"`, `"materials"`,
#'   `"documentation"`, `"output"`, `"unknown"`. More than one may be given.
#'   `NULL` (the default) downloads zips whole and leaves them zipped, as
#'   before. `max_file_size` applies to each extracted member rather than to
#'   the archive.
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info. When members are extracted from a zip, its
#'   row reports the zip with `extracted` giving the number of members written;
#'   `size_on_disk` and `checksum_ok` are then `NA`, because what is on disk is
#'   the members rather than the archive Dryad published a size and digest for.
#' @export
#'
#' @examples
#' \dontrun{
#'   dryad_file_download("10.5061/dryad.j1fd7")
#'
#'   # take only the data files out of any zip in the dataset
#'   dryad_file_download("10.5061/dryad.j1fd7", unzip_types = "data")
#' }
dryad_file_download <- function(dryad_doi,
                                download_to = ".",
                                max_file_size = 10,
                                max_download_size = 100,
                                unzip_types = NULL,
                                pb = NULL) {
  dryad_doi <- .dryad_doi(dryad_doi) |>
    stats::na.omit() |>
    unique()
  if (length(dryad_doi) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Dryad File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple datasets ----
  if (length(dryad_doi) > 1) {
    paste0(
      "Starting downloads for ", length(dryad_doi), " Dryad dataset",
      ifelse(length(dryad_doi) == 1, "", "s"), "...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(dryad_doi, function(x) {
      tryCatch(
        dryad_file_download(
          x,
          download_to = download_to,
          max_file_size = max_file_size,
          max_download_size = max_download_size,
          unzip_types = unzip_types,
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
      "...Completed downloads for ", length(dryad_doi), " Dryad dataset",
      ifelse(length(dryad_doi) == 1, "", "s")
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  # --- retrieve dataset contents ----
  paste0("Starting retrieval for ", dryad_doi) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(.dryad_info(dryad_doi, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", dryad_doi, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from entries (verified live field names: path, size,
  # digest, digestType, and _links.self.href / _links["stash:download"].href
  # -- see .dryad_info()). The file id is the numeric segment of
  # _links.self.href (".../api/v2/files/<id>"), there being no separate id
  # field in the response.
  rows <- lapply(files_list, function(x) {
    self_href <- x$`_links`$self$href %||% NA_character_
    dl_href <- x$`_links`$`stash:download`$href %||% NA_character_
    file_id <- if (!is.na(self_href))
      sub("^.*/([0-9]+)$", "\\1", self_href) else NA_character_
    digest_type <- tolower(x$digestType %||% NA_character_)
    dplyr::tibble(
      id       = file_id,
      key      = x$path %||% NA_character_,
      size     = as.numeric(x$size %||% NA_real_),
      checksum = x$digest %||% NA_character_,
      checksum_type = digest_type,
      self     = if (!is.na(dl_href)) paste0("https://datadryad.org", dl_href)
                 else NA_character_
    )
  })

  files <- dplyr::bind_rows(rows)
  if (nrow(files) == 0) {
    message("- ", dryad_doi, " contained no files")
    return(NULL)
  }

  # A zip we were asked to look inside is exempt from the size caps below, for
  # the same reason zenodo_file_download() exempts it: the caps measure the
  # file Dryad would send, and for these we have decided not to transfer
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
  folder_name <- gsub("[^A-Za-z0-9._-]+", "_", dryad_doi)
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
        .dryad_zip_members(files$self[[i]], dest = download_to,
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
            .dryad_headers() |>
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
  files <- .dryad_verify_downloads(files, download_to)

  n_missing <- sum(!files$downloaded)
  if (n_missing > 0) {
    worst <- files[!files$downloaded, ]
    warning(sprintf(
      "%d of %d file%s from Dryad dataset %s did not arrive intact (e.g. %s). The returned table marks %s downloaded = FALSE. Run again to retry.",
      n_missing, nrow(files), plural(nrow(files)), dryad_doi,
      paste(utils::head(worst$key, 3), collapse = ", "),
      if (n_missing == 1) "it" else "them"), call. = FALSE)
  }

  # --- return table ----
  files$folder <- basename(download_to)
  files$dryad_doi <- dryad_doi
  files <- files[, c("folder", "dryad_doi", "id", "key", "path", "size",
                     "size_on_disk", "checksum", "checksum_ok", "self",
                     "downloaded", "extracted")]

  invisible(files)
}

#' Fetch selected files out of a .zip in a Dryad dataset without downloading it
#'
#' A zip published on Dryad is often mostly material a reader does not need:
#' stimuli, images, or videos alongside the few data files. Dryad's download
#' links support byte-range requests and every member of a zip is compressed
#' on its own, so individual members can be pulled out of the archive while
#' the rest is never transferred (see `.zip_member_fetch()`, shared with the
#' identical Zenodo/Dataverse/Figshare path).
#'
#' @param url the file's download URL
#' @param dest directory to write the extracted members into
#' @param keep_types file categories worth extracting, as
#'   [data_classify_files()] names them; the default keeps data and
#'   documentation and skips materials, matching [zip_decision()]
#' @param max_file_size largest member to extract (in MB), applied per member
#'   exactly as the dataset-level cap is applied per file
#'
#' @returns a data frame with one row per extracted member (`name`, `path`,
#'   `size`, `ok`), or `NULL` when the archive could not be listed and the
#'   caller should download it whole instead
#' @keywords internal
.dryad_zip_members <- function(url, dest,
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

#' Check downloaded Dryad files against the file system
#'
#' Mirrors [`.dataverse_verify_downloads()`]: confirms every file the
#' download planned to save is present, is the size Dryad reported, and (when
#' Dryad published an MD5 digest) matches the checksum.
#'
#' A row whose `extracted` count is set is exempt from both checks, for the
#' same reason as the Dataverse case: Dryad's size and digest describe the
#' zip as published, and that zip was deliberately never downloaded -- only
#' chosen members were, each verified against the CRC32 in the archive's own
#' central directory as it is extracted.
#'
#' @param files the file table being built, with `path`, `size`, `checksum`,
#'   `checksum_type`, and `downloaded` columns, and optionally `extracted`
#' @param download_to the folder the dataset was saved in
#'
#' @returns `files` with `downloaded` corrected against the file system, plus
#'   `size_on_disk` and `checksum_ok` columns
#' @keywords internal
.dryad_verify_downloads <- function(files, download_to) {
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

  # Dryad's digestType has only ever been observed as "md5" (verified live on
  # 3 files of a real dataset), but the field is published per-file rather
  # than fixed, so the type is checked exactly as Dataverse's checksum_type
  # is, rather than assumed.
  is_md5 <- !is.na(files$checksum_type) & files$checksum_type == "md5"
  to_hash <- which(ok & is_md5 & !is.na(files$checksum) &
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
