# ReShare (https://reshare.ukdataservice.ac.uk) is the UK Data Service's
# self-deposit repository for social science research data. Unlike Dataverse/
# Zenodo/Figshare/Dryad, it runs on EPrints (a customised installation, not
# DSpace or the Figshare-family software) -- verified live 2026-08-16:
# GET reshare.ukdataservice.ac.uk/api/info/version (the Dataverse-specific
# endpoint) returns 404, ruling out the Dataverse listing some third-party
# registries wrongly carry for this host.
#
# EPrints exposes a generic REST/export API at /id/eprint/<id>, content-
# negotiated via the Accept header (https://wiki.eprints.org/w/API:EPrints/
# Apache/REST). Verified live against two real deposits (eprint 854001, DOI
# 10.5255/UKDA-SN-854001, and eprint 854243): GET .../id/eprint/854001 with
# Accept: application/json returns full metadata with no authentication
# required, and a DOI resolves (via dx.doi.org) straight to
# reshare.ukdataservice.ac.uk/<id>/, so the numeric id is the id both the
# metadata endpoint and the DOI redirect agree on. Field names below
# (title/doi/creators/lastmod/documents[].files[]) were read from that real
# response, not guessed from EPrints' generic documentation, which does not
# itself specify what fields an install exposes.
#
# File bytes are served from .../id/file/<fileid> (verified live: HTTP 200,
# Content-Length matching the metadata's filesize, MD5 matching the metadata's
# hash, and Accept-Ranges: bytes / 206 Partial Content on a Range request --
# so zip_peek()/unzip_types work here exactly as for the other hosts).

#' Find ReShare Links in Papers
#'
#' Get all ReShare links: real hyperlinks from the paper's own `url` table,
#' plus a body-text fallback for a BARE mention (a DOI like
#' "10.5255/UKDA-SN-854001" is routinely cited without any URL scheme at all)
#' that the source PDF/HTML never encoded as an actual hyperlink -- the `url`
#' table only ever contains links the source document itself made clickable.
#' Same two-tier approach `zenodo_links()` uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the ReShare url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' reshare_links(psychsci)
#' }
reshare_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("reshare\\.ukdataservice\\.ac\\.uk|10\\.5255/ukda-sn",
                        href, ignore.case = TRUE))

  reshare_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?reshare\\.ukdataservice\\.ac\\.uk/[0-9]+/?",
    "|(?:https?://)?(?:dx\\.)?(?:doi\\.org/)?10\\.5255/UKDA-SN-[0-9]+"
  )
  other_reshare <- text_search(paper, reshare_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_reshare) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$reshare_url <- links$href
  links$reshare_id <- .reshare_id(links$reshare_url)

  return(links)
}

#' Get ReShare eprint ID from URL or DOI
#'
#' @param reshare_url a vector of URLs or DOIs to reshare deposits
#'
#' @returns a character vector of numeric eprint IDs
#' @keywords internal
.reshare_id <- function(reshare_url) {
  if (length(reshare_url) == 0) {
    return(character(0))
  }

  if (length(reshare_url) > 1) {
    return(vapply(reshare_url, .reshare_id, character(1)))
  }

  # handle single reshare_url ----
  reshare_url <- trimws(as.character(reshare_url))

  if (is.na(reshare_url) || !nzchar(reshare_url)) {
    return(NA_character_)
  }

  if (grepl("^[0-9]+$", reshare_url)) {
    return(reshare_url)
  }

  # A DOI (10.5255/UKDA-SN-854001) carries the eprint id as its final numeric
  # segment -- verified live to be the same id the URL path and the metadata
  # endpoint both use, so no separate DOI-to-id lookup call is needed the way
  # Dataverse's persistentId requires.
  patterns <- c(
    "10\\.5255/UKDA-SN-([0-9]+)",
    "reshare\\.ukdataservice\\.ac\\.uk/([0-9]+)"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, reshare_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(reshare_url, match)[[1]]
    if (length(groups) >= 2) {
      return(groups[[2]])
    }
  }

  return(NA_character_)
}

#' Retrieve info from ReShare by URL
#'
#' @param reshare_url a ReShare URL or DOI, or a table containing them (e.g.,
#'   as created by [reshare_links()])
#' @param id_col the index or name of the column that contains ReShare URLs,
#'   if `reshare_url` is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   reshare_info("https://doi.org/10.5255/UKDA-SN-854001")
#' }
reshare_info <- function(reshare_url, id_col = 1, pb = NULL) {
  if (!online("reshare.ukdataservice.ac.uk")) {
    stop("ReShare seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "ReShare Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(reshare_url)) {
    table <- reshare_url
    table$reshare_url <- table[[id_col]]
  } else {
    raw_urls <- unique(reshare_url) |> stats::na.omit()
    table <- data.frame(reshare_url = raw_urls)
  }

  ids <- data.frame(
    reshare_url = table$reshare_url,
    reshare_id = .reshare_id(table$reshare_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$reshare_url), , drop = FALSE]
  valid_ids <- unique(stats::na.omit(ids$reshare_id))

  if (length(valid_ids) == 0) {
    ("No valid ReShare links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "reshare_url"))
  }

  paste0(
    "Starting ReShare retrieval for ",
    length(valid_ids), " deposit",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  for (i in seq_along(valid_ids)) {
    id_info[[i]] <- .reshare_info(valid_ids[[i]], pb = pb)
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "reshare_url") |>
    dplyr::left_join(info, by = "reshare_id", suffix = c("", ".reshare"))

  paste0("...ReShare retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one ReShare deposit
#'
#' @param reshare_id a ReShare eprint ID
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @keywords internal
.reshare_info <- function(reshare_id, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ReShare eprint ", reshare_id, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(reshare_id = as.character(reshare_id))

  api_url <- sprintf("https://reshare.ukdataservice.ac.uk/id/eprint/%s", reshare_id)

  resp <- .batch_query(api_url, msg = NULL, req_func = .reshare_headers)[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(reshare_id, " could not be found on ReShare", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  rec <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
  if (is.null(rec)) {
    obj$error <- "parse_error"
    return(obj)
  }

  creators_field <- rec$creators
  authors <- if (is.list(creators_field)) {
    vapply(creators_field, function(a) {
      nm <- a$name %||% list()
      full <- trimws(paste(nm$given %||% "", nm$family %||% ""))
      if (nzchar(full)) full else NA_character_
    }, character(1))
  } else {
    character(0)
  }

  # documents[] holds one entry per uploaded item (often an archive), each
  # with its own files[] (usually length 1: the item's single underlying
  # file) -- verified live: eprint 854001 has 10 documents, each wrapping one
  # file. Flattened here so downstream code sees one row per actual file, the
  # same shape .figshare_info()/.dryad_info() return.
  docs <- rec$documents %||% list()
  files_flat <- list()
  for (d in docs) {
    for (f in (d$files %||% list())) {
      f$content <- d$content %||% NA_character_   # data / documentation / readme / ...
      files_flat[[length(files_flat) + 1L]] <- f
    }
  }

  obj$title <-            rec$title %||% NA_character_
  obj$doi <-              rec$doi %||% NA_character_
  obj$publication_date <- rec$datestamp %||% NA_character_
  obj$updated_date <-     rec$lastmod %||% NA_character_
  obj$authors <-          list(authors)
  obj$license <-          NA_character_
  obj$files <-            list(files_flat)

  return(obj)
}

# Add the right auth header to a ReShare API/download request. Public
# deposits are readable without a token (verified live against eprint 854001:
# metadata and file bytes both retrieved with no Authorization header at
# all). ReShare has no documented personal-access-token scheme the way
# Figshare/Dataverse/Zenodo/OSF do, so no token support is offered here --
# every function in this file works only against public deposits.
.reshare_headers <- function(req) {
  req |> httr2::req_headers(`User-Agent` = "metacheck")
}

#' Download all files from a ReShare deposit
#'
#' Creates a directory for the deposit and downloads all of its files.
#' Returns (invisibly) a data frame with file info.
#'
#' Like Figshare, ReShare's EPrints REST API documents no whole-deposit
#' bulk-download endpoint -- each file is served from its own
#' `.../id/file/<fileid>` URL, so this always downloads file by file. A
#' `.zip` already present in the deposit (ReShare's own documents are
#' frequently zips of related files, e.g. all interviews for one country) is
#' still handled the same way [dataverse_file_download()] handles one:
#' fetched whole and left as a zip unless `unzip_types` asks for only
#' specific members out of it.
#'
#' You can limit downloads to only files under a specific size (defaults to
#' 10MB) and only a maximum download size (largest files will be omitted until
#' total size is under the limit). Omitted files will be listed as messages in
#' verbose mode, and included in the returned data frame with the `downloaded`
#' column value set to FALSE.
#'
#' @param reshare_id a ReShare eprint ID, URL, or DOI
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL or
#'   Inf for no restrictions
#' @param max_download_size maximum total size to download - set to NULL or
#'   Inf for no restrictions
#' @param unzip_types file categories to extract from `.zip` files in the
#'   deposit rather than downloading the zip whole, named as
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
#'   the members rather than the archive ReShare published a size and MD5 for.
#' @export
#'
#' @examples
#' \dontrun{
#'   reshare_file_download("854001")
#'
#'   # take only the data files out of any zip in the deposit
#'   reshare_file_download("854001", unzip_types = "data")
#' }
reshare_file_download <- function(reshare_id,
                                  download_to = ".",
                                  max_file_size = 10,
                                  max_download_size = 100,
                                  unzip_types = NULL,
                                  pb = NULL) {
  reshare_id <- .reshare_id(reshare_id) |>
    stats::na.omit() |>
    unique()
  if (length(reshare_id) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "ReShare File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple deposits ----
  if (length(reshare_id) > 1) {
    paste0(
      "Starting downloads for ", length(reshare_id), " ReShare deposit",
      ifelse(length(reshare_id) == 1, "", "s"), "...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(reshare_id, function(x) {
      tryCatch(
        reshare_file_download(
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
      "...Completed downloads for ", length(reshare_id), " ReShare deposit",
      ifelse(length(reshare_id) == 1, "", "s")
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  # --- retrieve deposit contents ----
  paste0("Starting retrieval for ReShare deposit ", reshare_id) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(.reshare_info(reshare_id, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", reshare_id, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from entries (verified live field names: fileid,
  # filename, filesize, hash, hash_type, uri -- see .reshare_info()). `uri`
  # is already the full download URL (http://reshare.ukdataservice.ac.uk/id/
  # file/<fileid>) -- upgraded to https here since the API serves both.
  rows <- lapply(files_list, function(x) {
    self_url <- x$uri %||% NA_character_
    if (!is.na(self_url)) self_url <- sub("^http://", "https://", self_url)
    dplyr::tibble(
      id       = as.character(x$fileid %||% NA_character_),
      key      = x$filename %||% NA_character_,
      size     = as.numeric(x$filesize %||% NA_real_),
      checksum = x$hash %||% NA_character_,
      checksum_type = tolower(x$hash_type %||% NA_character_),
      self     = self_url
    )
  })

  files <- dplyr::bind_rows(rows)
  if (nrow(files) == 0) {
    message("- ", reshare_id, " contained no files")
    return(NULL)
  }

  # A zip we were asked to look inside is exempt from the size caps below, for
  # the same reason zenodo_file_download() exempts it: the caps measure the
  # file ReShare would send, and for these we have decided not to transfer
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
  folder_name <- paste0("reshare_", reshare_id)
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
        .reshare_zip_members(files$self[[i]], dest = download_to,
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
            .reshare_headers() |>
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
  files <- .reshare_verify_downloads(files, download_to)

  n_missing <- sum(!files$downloaded)
  if (n_missing > 0) {
    worst <- files[!files$downloaded, ]
    warning(sprintf(
      "%d of %d file%s from ReShare deposit %s did not arrive intact (e.g. %s). The returned table marks %s downloaded = FALSE. Run again to retry.",
      n_missing, nrow(files), plural(nrow(files)), reshare_id,
      paste(utils::head(worst$key, 3), collapse = ", "),
      if (n_missing == 1) "it" else "them"), call. = FALSE)
  }

  # --- return table ----
  files$folder <- basename(download_to)
  files$reshare_id <- as.character(reshare_id)
  files <- files[, c("folder", "reshare_id", "id", "key", "path", "size",
                     "size_on_disk", "checksum", "checksum_ok", "self",
                     "downloaded", "extracted")]

  invisible(files)
}

#' Fetch selected files out of a .zip in a ReShare deposit without downloading it
#'
#' A zip published on ReShare is often mostly material a reader does not
#' need: stimuli, images, or videos alongside the few data files. ReShare's
#' per-file download URLs support byte-range requests and every member of a
#' zip is compressed on its own, so individual members can be pulled out of
#' the archive while the rest is never transferred (see `.zip_member_fetch()`,
#' shared with the identical Zenodo/Dataverse/Figshare/Dryad path).
#'
#' @param url the file's download URL
#' @param dest directory to write the extracted members into
#' @param keep_types file categories worth extracting, as
#'   [data_classify_files()] names them; the default keeps data and
#'   documentation and skips materials, matching [zip_decision()]
#' @param max_file_size largest member to extract (in MB), applied per member
#'   exactly as the deposit-level cap is applied per file
#'
#' @returns a data frame with one row per extracted member (`name`, `path`,
#'   `size`, `ok`), or `NULL` when the archive could not be listed and the
#'   caller should download it whole instead
#' @keywords internal
.reshare_zip_members <- function(url, dest,
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

#' Check downloaded ReShare files against the file system
#'
#' Mirrors [`.dataverse_verify_downloads()`]: confirms every file the
#' download planned to save is present, is the size ReShare reported, and
#' (when ReShare published an MD5 hash) matches the checksum.
#'
#' A row whose `extracted` count is set is exempt from both checks, for the
#' same reason as the Dataverse case: ReShare's size and hash describe the
#' zip as published, and that zip was deliberately never downloaded -- only
#' chosen members were, each verified against the CRC32 in the archive's own
#' central directory as it is extracted.
#'
#' @param files the file table being built, with `path`, `size`, `checksum`,
#'   `checksum_type`, and `downloaded` columns, and optionally `extracted`
#' @param download_to the folder the deposit was saved in
#'
#' @returns `files` with `downloaded` corrected against the file system, plus
#'   `size_on_disk` and `checksum_ok` columns
#' @keywords internal
.reshare_verify_downloads <- function(files, download_to) {
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

  # ReShare's hash_type has only ever been observed as "MD5" (verified live on
  # a real file), but the field is published per-file rather than fixed, so
  # the type is checked exactly as Dataverse's checksum_type is, rather than
  # assumed.
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
