#' Find Zenodo Links in Papers
#'
#' Get all Zenodo links: real hyperlinks from the paper's own `url` table,
#' plus a body-text fallback for a BARE mention (a DOI like
#' "10.5281/zenodo.1234567" is routinely cited without any URL scheme at all)
#' that the source PDF/HTML never encoded as an actual hyperlink — the `url`
#' table only ever contains links the source document itself made clickable.
#' Same two-tier approach `github_links()` uses for GitHub.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Zenodo url in the first (text) column
#' @export
#'
#' @examples
#' zenodo_links(psychsci)
zenodo_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("zenodo\\.org|10\\.5281/zenodo", href, ignore.case = TRUE))

  zen_bare_regex <- paste0(
    "(?:https?://)?zenodo\\.org/(?:record|records)/[0-9]+",
    "|(?:https?://)?(?:doi\\.org/)?10\\.5281/zenodo\\.[0-9]+"
  )
  other_zen <- text_search(paper, zen_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_zen) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$zenodo_url <- links$href
  links$zenodo_id <- .zenodo_id(links$zenodo_url)
  links$zenodo_link <- ifelse(
    is.na(links$zenodo_id),
    NA_character_,
    paste0("https://doi.org/10.5281/zenodo.", links$zenodo_id)
  )

  return(links)
}

#' Get zenodo ID from URL
#'
#' @param zenodo_url a vector of URLs to zenodo repos
#'
#' @returns a character vector of IDs
#' @keywords internal
.zenodo_id <- function(zenodo_url) {
  if (length(zenodo_url) == 0) {
    return(character(0))
  }

  if (length(zenodo_url) > 1) {
    return(vapply(zenodo_url, .zenodo_id, character(1)))
  }

  # handle single zenodo_url ----
  zenodo_url <- trimws(as.character(zenodo_url))

  if (is.na(zenodo_url) || !nzchar(zenodo_url)) {
    return(NA_character_)
  }

  if (grepl("^[0-9]+$", zenodo_url)) {
    return(zenodo_url)
  }

  patterns <- c(
    "10\\.5281/zenodo\\.([0-9]+)",
    "zenodo\\.org/(?:records?|uploads)/([0-9]+)",
    "zenodo\\.([0-9]+)"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, zenodo_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(zenodo_url, match)[[1]]
    if (length(groups) >= 2) {
      return(groups[[2]])
    }
  }

  return(NA_character_)
}

#' Retrieve info from Zenodo by URL
#'
#' @param zenodo_url an Zenodo URL, or a table containing them (e.g., as created by `zenodo_links()`)
#' @param id_col the index or name of the column that contains Zenodo URLs, if id is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   # get info on one zenodo link
#'   zenodo_info("https://doi.org/10.5281/zenodo.18648142")
#' }
zenodo_info <- function(zenodo_url, id_col = 1, pb = NULL) {
  if (!online("zenodo.org")) {
    stop("Zenodo.org seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Zenodo Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(zenodo_url)) {
    table <- zenodo_url
    table$zenodo_url <- table[[id_col]]
  } else {
    raw_urls <- unique(zenodo_url) |> stats::na.omit()
    table <- data.frame(zenodo_url = raw_urls)
  }

  ids <- data.frame(
    zenodo_url = table$zenodo_url,
    zenodo_id = .zenodo_id(table$zenodo_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$zenodo_url), , drop = FALSE]
  valid_ids <- unique(stats::na.omit(ids$zenodo_id))

  if (length(valid_ids) == 0) {
    ("No valid Zenodo links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "zenodo_url"))
  }

  # iterate over valid IDs
  paste0(
    "Starting Zenodo retrieval for ",
    length(valid_ids), " file",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  i <- 0
  while (i < length(valid_ids)) {
    i <- i + 1
    id_info[[i]] <- .zenodo_info(valid_ids[[i]], pb = pb)
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _)

  data <- table |>
    dplyr::left_join(ids, by = "zenodo_url") |>
    dplyr::left_join(info, by = "zenodo_id", suffix = c("", ".zenodo"))

  paste0("...Zenodo retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from Zenodo by ID
#'
#' @param zenodo_id a Zenodo ID or URL
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.zenodo_info <- function(zenodo_id, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  zenodo_id <- .zenodo_id(zenodo_id)
  paste0("* Retrieving info from Zenodo ID ", zenodo_id, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # set up return table
  obj <- data.frame(
    zenodo_id = zenodo_id
  )

  # Build the URL
  zenodo_api_url <- paste0("https://zenodo.org/api/records/", zenodo_id)

  resp <- .batch_query(zenodo_api_url, msg = NULL)[[1]]

  if (httr2::resp_status(resp) != 200) {
    warning(zenodo_id, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  rec <- tryCatch(
    httr2::resp_body_json(resp),
    error = \(e) NULL
  )
  if (is.null(rec)) {
    obj$error <- "parse_error"
    return(obj)
  }

  metadata <- rec$metadata

  # Basic metadata
  obj$title <-            metadata$title %||% NA_character_
  obj$doi <-              rec$doi %||% NA_character_
  obj$description <-      metadata$description %||% NA_character_
  obj$publication_date <- metadata$publication_date %||% NA_character_
  obj$updated_date <-     rec$updated %||% NA_character_
  obj$creators <-         list(metadata$creators) %||% list(c())
  obj$keywords <-         list(metadata$keywords) %||% list(c())
  obj$resource_type <-    metadata$resource_type$type %||% NA_character_
  obj$journal <-          list(metadata$journal) %||% list(c())
  obj$owners <-           list(rec$owners) %||% list(c())
  obj$license <-          metadata$license$id %||%
                          metadata$license$title %||%
                          metadata$license %||%
                          NA_character_
  obj$downloads <-        rec$stats$downloads %||% NA_real_
  obj$unique_downloads <- rec$stats$unique_downloads %||% NA_real_
  obj$views <-            rec$stats$views %||% NA_real_
  obj$files <-            list(rec$files) %||% list(c())

  return(obj)
}


#' Download all Zenodo Project Files
#'
#' Creates a directory for the Zenodo ID and downloads all of the files using a folder structure from the Zenodo project nodes and file storage structure. Returns (invisibly) a data frame with file info.
#'
#' You can limit downloads to only files under a specific size (defaults to 10MB) and only a maximum download size (largest files will be omitted until total size is under the limit). Omitted files will be listed as messages in verbose mode, and included in the returned data frame with the downloaded column value set to FALSE.
#'
#' A `.zip` in the record is normally fetched whole and left as a zip. Set
#' `unzip_types` to pull only the files you want out of it instead: Zenodo
#' serves per-file URLs with byte-range support and each member of a zip is
#' compressed separately, so the wanted members can be read out of the archive
#' while the rest is never transferred. A zip of stimuli and three data files
#' then costs the three data files. When the archive cannot be read that way
#' (the listing fails, or the host refuses ranges) it is downloaded whole as
#' before, so this never loses a file -- it only avoids transferring one.
#'
#' @param zenodo_id an Zenodo ID or URL
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL or Inf for no restrictions
#' @param max_download_size maximum total size to download - set to NULL of Inf for no restrictions
#' @param unzip_types file categories to extract from `.zip` files in the record
#'   rather than downloading the zip whole, named as [data_classify_files()]
#'   names them: `"data"`, `"code"`, `"materials"`, `"documentation"`,
#'   `"output"`, `"unknown"`. More than one may be given. `NULL` (the default)
#'   downloads zips whole and leaves them zipped, as before. `max_file_size`
#'   applies to each extracted member rather than to the archive.
#' @param pb a progress bar passed from another function
#'
#' @returns data frame of file info. When members are extracted from a zip, its
#'   row reports the zip with `extracted` giving the number of members written;
#'   `size_on_disk` and `checksum_ok` are then `NA`, because what is on disk is
#'   the members rather than the archive Zenodo published a size and MD5 for.
#' @export
#'
#' @examples
#' \dontrun{
#'   zenodo_file_download("2591593")
#'
#'   # take only the data files out of any zip in the record
#'   zenodo_file_download("2591593", unzip_types = "data")
#' }
zenodo_file_download <- function(zenodo_id,
                                 download_to = ".",
                                 max_file_size = 10,
                                 max_download_size = 100,
                                 unzip_types = NULL,
                                 pb = NULL) {
  zenodo_id <- .zenodo_id(zenodo_id) |>
    stats::na.omit() |>
    unique()
  if (length(zenodo_id) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Zenodo File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple IDs ----
  if (length(zenodo_id) > 1) {
    paste0(
      "Starting downloads for ", length(zenodo_id),
      " Zenodo records...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(zenodo_id, function(x) {
      tryCatch(
        zenodo_file_download(
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

    # bind all rows; drop NULLs safely
    dl_list <- dl_list[!vapply(dl_list, is.null, logical(1))]
    if (length(dl_list) == 0) return(NULL)

    dl <- dplyr::bind_rows(dl_list)
    paste0(
      "...Completed downloads for ", length(zenodo_id),
      " Zenodo records"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  # --- retrieve record contents ----
  paste0("Starting retrieval for ", zenodo_id) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(zenodo_info(zenodo_id, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", zenodo_id, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from nested entries
  rows <- lapply(
    files_list,
    function(x) {
      dplyr::tibble(
        id       = x$id %||% NA_character_,
        key      = x$key %||% NA_character_,
        size     = as.numeric( x$size %||% NA_real_),
        checksum = x$checksum %||% NA_character_,
        self     = x$links$self %||% NA_character_
      )
    }
  )

  files <- dplyr::bind_rows(rows)

  if (nrow(files) == 0) {
    message("- ", zenodo_id, " contained no files")
    return(NULL)
  }

  # A zip we were asked to look inside is exempt from the size caps below.
  # Those caps measure the file Zenodo would send, and for these that number
  # describes bytes we have decided not to transfer: only selected members are
  # fetched, each capped individually by `max_file_size` at extraction time.
  # Judging a 130 MB archive by its full size would discard it before the
  # extraction that makes it cheap -- which is exactly the archive `unzip_types`
  # exists for, so without this exemption the option could never do anything.
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

  # remove largest files until total <= limit
  if (!is.null(max_download_size) && is.finite(max_download_size) && max_download_size > 0) {
    # Only the files that will be transferred whole count against the total.
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
      unzippable <- unzippable[-max_file]   # keep the mask aligned with `files`
    }
  }

  if (nrow(files) == 0) {
    paste0("- All files omitted due to size constraints") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # --- target directory (avoid overwrite) ----
  # download_to <- fs::path_abs(download_to)
  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, as.character(zenodo_id))
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
  # temppath <- fs::file_temp()
  temppath <- tempfile()
  on.exit(unlink(temppath, recursive = TRUE))
  dir.create(temppath)

  n <- nrow(files)
  files$downloaded <- FALSE

  # --- bulk archive, when every file in the record survived the size filters ---
  # files-archive is all-or-nothing for the record (undocumented in Zenodo's API
  # reference, but a stable first-party endpoint -- the same URL Zenodo's own
  # record pages link to as "Download all"). It is only used here when nothing
  # was filtered out above, so the archive's contents are exactly the files we
  # want -- no wasted transport, unlike the general download_repo_files() case
  # where a caller may want only a subset of a record.
  used_bulk <- FALSE
  n_in_record <- length(files_list)

  # `unzippable` (computed with the size filters above, and kept aligned with
  # `files` as rows were dropped) marks the zips to be read member by member.
  # Their presence also rules out the bulk archive: it would transfer the whole
  # record, including the zip contents being avoided, before anything could be
  # selected from it.
  unzip_me <- unzippable

  if (n == n_in_record && !any(unzip_me)) {
    zip_url <- sprintf("https://zenodo.org/api/records/%s/files-archive", zenodo_id)
    zip_path <- file.path(temppath, "archive.zip")
    dl_ok <- tryCatch({
      resp <- httr2::request(zip_url) |>
        httr2::req_timeout(600) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
        httr2::req_perform(path = zip_path)
      httr2::resp_status(resp) == 200 && file.exists(zip_path) && file.size(zip_path) > 0
    }, error = \(e) FALSE)

    if (isTRUE(dl_ok)) {
      entries <- tryCatch(utils::unzip(zip_path, list = TRUE), error = \(e) NULL)
      if (!is.null(entries) && nrow(entries) > 0) {
        extract_dir <- file.path(temppath, "extracted")
        dir.create(extract_dir)
        tryCatch(utils::unzip(zip_path, exdir = extract_dir), error = \(e) NULL)
        for (i in seq_len(n)) {
          src <- file.path(extract_dir, files$key[[i]])
          if (file.exists(src) && file.size(src) > 0) {
            file.copy(src, file.path(temppath, files$id[[i]]))
            files$downloaded[i] <- TRUE
          }
        }
        used_bulk <- all(files$downloaded)
      }
    }
    unlink(zip_path)
    "Downloaded as one archive" |>
      list(what = _) |>
      pb$tick(0, tokens = _)
  }

  # --- file-by-file fallback (used when the bulk archive was skipped, or
  # extraction did not account for every wanted file) ----
  files$extracted <- NA_integer_
  if (!used_bulk) {
    for (i in seq_len(n)) {
      if (isTRUE(files$downloaded[i])) next   # already extracted from the archive

      # --- selected members out of a zip, instead of the whole zip ----
      # Members are written straight into the target directory, not into
      # temppath: they keep their own names from inside the archive, whereas the
      # file-by-file path below stages each download under the Zenodo file `id`
      # and renames it when copying.
      if (unzip_me[i]) {
        paste0("Reading zip contents of ", files$key[[i]]) |>
          list(what = _) |>
          pb$tick(0, tokens = _)
        got <- tryCatch(
          .zenodo_zip_members(files$self[[i]], dest = download_to,
                              keep_types = unzip_types,
                              max_file_size = max_file_size),
          error = \(e) NULL)
        if (!is.null(got)) {
          # A readable archive with nothing wanted inside is a success with zero
          # members, not a failure: there was nothing to fetch.
          files$extracted[i] <- sum(got$ok %in% TRUE)
          files$downloaded[i] <- TRUE
          paste0("- extracted ", files$extracted[i], " file",
                 plural(files$extracted[i]), " from ", files$key[[i]]) |>
            list(what = _) |>
            pb$tick(0, tokens = _)
          next
        }
        # Listing failed (host refused ranges, or the directory was unreadable):
        # fall through and download the archive whole, as before.
        paste0("- could not read ", files$key[[i]],
               " without downloading it; fetching the whole archive") |>
          list(what = _) |>
          pb$tick(0, tokens = _)
      }

      ok <- FALSE
      if (!is.na(files$self[[i]]) && nzchar(files$self[[i]])) {
        # write to a stable temp filename (use Zenodo file `id`)
        target_path <- file.path(temppath, files$id[[i]])
        resp <- tryCatch(
          {
            httr2::request(files$self[[i]]) |>
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
  }

  # copy to flat target directory using original filename if available
  files$path <- NA_character_
  for (i in seq_len(nrow(files))) {
    # An unzipped row has no staged file to copy: its members were written to
    # download_to directly, under their own names inside the archive.
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
  files <- .zenodo_verify_downloads(files, download_to)

  n_missing <- sum(!files$downloaded)
  if (n_missing > 0) {
    worst <- files[!files$downloaded, ]
    warning(sprintf(
      "%d of %d file%s from Zenodo record %s did not arrive intact (e.g. %s). The returned table marks %s downloaded = FALSE. Run again to retry.",
      n_missing, nrow(files), plural(nrow(files)), zenodo_id,
      paste(utils::head(worst$key, 3), collapse = ", "),
      if (n_missing == 1) "it" else "them"), call. = FALSE)
  }

  # --- return table ----
  files$folder    <- basename(download_to)
  files$zenodo_id <- as.character(zenodo_id)
  files <- files[, c("folder", "zenodo_id", "id", "key", "path", "size",
                     "size_on_disk", "checksum", "checksum_ok", "self",
                     "downloaded", "extracted")]

  invisible(files)
}

#' Fetch selected files out of a .zip in a Zenodo record without downloading it
#'
#' A zip published on Zenodo is often mostly material a reader does not need:
#' stimuli, images, or videos alongside the few data files. Zenodo serves its
#' per-file URLs with byte-range support, and every member of a zip is
#' compressed on its own, so individual members can be pulled out of the archive
#' while the rest is never transferred (see `.zip_member_fetch()`). Verified
#' 2026-08-13 on record 13384475: one 2.06 MB member out of a 129.8 MB archive
#' in about half a second.
#'
#' This is not possible for every archive. `.7z`, `.rar` and `.tar.gz` compress
#' all their files as one stream, so nothing inside can be read without
#' decompressing everything before it, and Zenodo's own whole-record
#' `files-archive` endpoint ignores range requests entirely. Both cases fall
#' back to downloading the archive whole.
#'
#' @param url the zip's `self` download URL in the record
#' @param dest directory to write the extracted members into
#' @param keep_types file categories worth extracting, as
#'   [data_classify_files()] names them; the default keeps data and
#'   documentation and skips materials, matching [zip_decision()]
#' @param max_file_size largest member to extract (in MB), applied per member
#'   exactly as the record-level cap is applied per file
#'
#' @returns a data frame with one row per extracted member (`name`, `path`,
#'   `size`, `ok`), or `NULL` when the archive could not be listed and the
#'   caller should download it whole instead
#' @keywords internal
.zenodo_zip_members <- function(url, dest,
                                keep_types = c("data", "documentation"),
                                max_file_size = 10) {
  listing <- tryCatch(zip_peek(url), error = \(e) NULL)
  if (is.null(listing) || nrow(listing) == 0) return(NULL)

  types <- data_classify_files(basename(listing$name))
  keep <- types %in% keep_types

  # A member whose size is unknown (NA, as Zip64 entries are) is not extracted:
  # the size cap cannot be applied to it and its bytes cannot be located.
  if (!is.null(max_file_size) && is.finite(max_file_size) && max_file_size > 0)
    keep <- keep & !is.na(listing$size) &
      listing$size <= max_file_size * 1024 * 1024

  if (!any(keep)) return(listing[0, c("name", "size"), drop = FALSE])

  .zip_fetch_members(url, names = listing$name[keep], dest = dest)
}

#' Check downloaded Zenodo files against the file system
#'
#' Confirms that every file the download planned to save is present, is the
#' size Zenodo reported, and matches the checksum Zenodo published for it.
#'
#' Up to this point `downloaded` records only that the transfer step ran. A
#' request that failed, a copy that did not happen, or a truncated write all
#' leave a row marked TRUE with nothing usable on disk. That matters most when
#' archiving a whole record unattended, where nobody is watching each file.
#'
#' Zenodo publishes an MD5 checksum per file, which catches corruption that a
#' size comparison cannot -- a file can be the right length and still be wrong.
#' Hashing uses `tools::md5sum()` from base R, as `import-grobid.R` and
#' `llm-cache.R` already do, so this adds no dependency. Only files Zenodo gave
#' an `md5:` value for are hashed; for anything else the size check stands on
#' its own.
#'
#' A row whose `extracted` count is set is exempt from both checks. Zenodo's
#' size and MD5 describe the zip as published, and that zip was deliberately
#' never downloaded -- only chosen members were. Measured against those values
#' every such row would look truncated. The members carry their own integrity
#' check instead: each is verified against the CRC32 stored in the archive's
#' central directory as it is extracted, in `.zip_member_fetch()`.
#'
#' @param files the file table being built, with `path`, `size`, `checksum`,
#'   and `downloaded` columns, and optionally `extracted`
#' @param download_to the folder the record was saved in
#'
#' @returns `files` with `downloaded` corrected against the file system, plus
#'   `size_on_disk` and `checksum_ok` columns
#' @keywords internal
.zenodo_verify_downloads <- function(files, download_to) {
  if (is.null(files) || nrow(files) == 0) return(files)

  # Rows whose members were extracted rather than whose file was downloaded.
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

  # Size: a file present at the wrong length is worse than an absent one,
  # because it looks complete to everything downstream.
  expected <- suppressWarnings(as.numeric(files$size %||% rep(NA_real_, nrow(files))))
  ok <- ok & (is.na(expected) | files$size_on_disk == expected) %in% TRUE

  # Checksum: only for files that survived the checks above, since hashing is
  # the expensive part and there is no point hashing a file that is missing.
  md5 <- sub("^md5:", "", files$checksum %||% rep(NA_character_, nrow(files)))
  to_hash <- which(ok & !is.na(md5) &
                     grepl("^[0-9a-f]{32}$", md5, ignore.case = TRUE))
  for (i in to_hash) {
    got <- tryCatch(unname(tools::md5sum(full[[i]])), error = \(e) NA_character_)
    files$checksum_ok[[i]] <- identical(tolower(got), tolower(md5[[i]]))
  }
  ok <- ok & !(files$checksum_ok %in% FALSE)

  files$downloaded <- ok & files$downloaded %in% TRUE

  # Restore the unzipped rows, which every check above necessarily failed: they
  # have no `path`, because the archive itself was never written to disk. Their
  # members were CRC-checked individually during extraction, so the row stands
  # as the extraction left it.
  if (any(unzipped)) files$downloaded[unzipped] <- TRUE
  files
}
