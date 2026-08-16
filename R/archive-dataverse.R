# Dataverse (https://dataverse.org) is open-source repository software, not a
# single host: hundreds of independent installations run it, each on its own
# domain (Harvard Dataverse at dataverse.harvard.edu, the demo/test instance at
# demo.dataverse.org, DataverseNL, institutional installs at universities...).
# Unlike Zenodo or the OSF, there is no single hostname or DOI prefix that
# reliably marks a link as Dataverse -- a bare "10.7910/DVN/..." DOI, for
# example, is Harvard's prefix but is not guaranteed to belong to Dataverse
# specifically. So detection here matches a maintained list of known Dataverse
# hosts (see .dataverse_hosts()) rather than a DOI pattern, mirroring how
# researchbox_links() and psycharchives_links() key off their own host(s).
#
# Checked against the psychsci corpus (2026-08-14, 13 real hyperlinks across 3
# hosts: dataverse.harvard.edu x10, dataverse.nl x1, dataverse.unc.edu x1, no
# bare-DOI mentions at all) -- every real citation carried the hostname, which
# is what makes a host list viable rather than a DOI-prefix guess.
#
# Every installation exposes the same Dataverse REST API
# (https://guides.dataverse.org/en/latest/api/), so once a host is recognised,
# every function below (info, file listing, download) works identically
# regardless of which installation the link points at.

# Known Dataverse installations. Extend this list as more are found in real
# papers; an installation not on this list is simply never recognised as
# Dataverse (its links fall through unclassified, the same as any other
# unrecognised URL -- there is no false-positive risk from under-listing, only
# a missed repository).
.dataverse_hosts <- function() {
  c(
    "dataverse.harvard.edu",   # Harvard Dataverse, the flagship instance
    "demo.dataverse.org",      # Dataverse demo/test instance
    "dataverse.nl",            # DataverseNL
    "dataverse.unc.edu",       # UNC Dataverse
    "abacus.library.ubc.ca",   # Abacus (UBC)
    "borealisdata.ca",         # Borealis (Canadian national repository)
    "dataverse.icrisat.org",
    "data.aussda.at",          # AUSSDA (Austria)
    "darus.uni-stuttgart.de",  # DaRUS (U. Stuttgart)
    "dataverse.lib.virginia.edu"
  )
}

# Regex fragment matching any known host, for use inside a larger pattern.
.dataverse_host_regex <- function() {
  paste(gsub("\\.", "\\\\.", .dataverse_hosts()), collapse = "|")
}

#' Find Dataverse Links in Papers
#'
#' Get all Dataverse links: real hyperlinks from the paper's own `url` table,
#' matched against a list of known Dataverse installations (see
#' [dataverse_links()]'s source for the list; Dataverse is open-source software
#' run by many independent hosts, unlike Zenodo, so there is no single hostname
#' or DOI prefix to match against everything). Same two-tier hyperlink +
#' bare-mention approach `zenodo_links()` uses, applied per known host.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Dataverse url in the first (text) column
#' @export
#'
#' @examples
#' dataverse_links(psychsci)
dataverse_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(.dataverse_host_regex(), href, ignore.case = TRUE))

  # A bare mention (no scheme, e.g. "dataverse.harvard.edu/dataset.xhtml?...")
  # the source PDF/HTML never encoded as an actual hyperlink -- the `url` table
  # only ever contains links the source document itself made clickable.
  dv_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?(?:", .dataverse_host_regex(), ")",
    "/[A-Za-z0-9/danddoi:._?=&%-]*"
  )
  other_dv <- text_search(paper, dv_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_dv) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$dataverse_url <- links$href
  parsed <- .dataverse_parse(links$dataverse_url)
  links$dataverse_host <- parsed$host
  links$dataverse_doi <- parsed$doi

  return(links)
}

# Parse a Dataverse URL into its host and persistentId (DOI), vectorised.
#
# Dataverse dataset pages carry the DOI as a query parameter in one of two
# forms: "?persistentId=doi:10.xxxx/YYYY" (dataset.xhtml, citation, and the API
# itself all use this) or a bare "/dataset.xhtml?id=<internal id>" (an
# installation's own numeric id, not a DOI -- returned as NA here, since the
# API calls below need the persistentId, not this internal id).
#
# @param url a character vector of Dataverse URLs
# @returns a data.frame(host, doi), one row per url, both NA when unparseable
# @keywords internal
.dataverse_parse <- function(url) {
  url <- as.character(url)
  host <- rep(NA_character_, length(url))
  doi  <- rep(NA_character_, length(url))

  has_url <- !is.na(url) & nzchar(url)
  if (!any(has_url)) return(data.frame(host = host, doi = doi))

  host_m <- regmatches(url, regexpr(.dataverse_host_regex(), url,
                                    ignore.case = TRUE, perl = TRUE))
  host[has_url] <- tolower(host_m)[seq_len(sum(has_url))]
  # regexpr/regmatches on a non-match returns character(0) for that element,
  # which the assignment above silently drops -- realign by re-matching per
  # element instead when any host is missing after a bulk match.
  missing <- has_url & (is.na(host) | !nzchar(host %||% ""))
  if (any(missing)) {
    host[missing] <- vapply(url[missing], function(u) {
      m <- regmatches(u, regexpr(.dataverse_host_regex(), u,
                                 ignore.case = TRUE, perl = TRUE))
      if (length(m) == 0) NA_character_ else tolower(m)
    }, character(1))
  }

  doi_pattern <- "persistentId=doi:([^&\\s\"']+)"
  doi_m <- regmatches(url, regexec(doi_pattern, url, ignore.case = TRUE, perl = TRUE))
  doi <- vapply(doi_m, function(m) {
    if (length(m) < 2) NA_character_ else utils::URLdecode(m[[2]])
  }, character(1))
  # A DOI never legitimately ends in a bare ".": when the persistentId sits at
  # the very end of a URL, sentence-final punctuation from the source PDF/HTML
  # is sometimes captured as part of the href itself (observed live in the
  # psychsci corpus, paper 0956797617748679: the same dataset cited twice, once
  # as "...OE9BNP" and once as "...OE9BNP." -- the latter is not a different
  # dataset, just a period the source document's own hyperlink swallowed).
  # Left unstripped this looks like two datasets and doubles the API calls,
  # each failing on the "." variant.
  doi <- sub("\\.$", "", doi)

  data.frame(host = host, doi = doi, stringsAsFactors = FALSE)
}

#' Retrieve info from Dataverse by URL
#'
#' @param dataverse_url a Dataverse dataset URL (or DOI, if `host` is also
#'   given), or a table containing them (e.g., as created by
#'   [dataverse_links()])
#' @param id_col the index or name of the column that contains Dataverse URLs,
#'   if `dataverse_url` is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   dataverse_info("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GGDUND")
#' }
dataverse_info <- function(dataverse_url, id_col = 1, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Dataverse Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(dataverse_url)) {
    table <- dataverse_url
    table$dataverse_url <- table[[id_col]]
  } else {
    raw_urls <- unique(dataverse_url) |> stats::na.omit()
    table <- data.frame(dataverse_url = raw_urls)
  }

  parsed <- .dataverse_parse(table$dataverse_url)
  ids <- data.frame(
    dataverse_url = table$dataverse_url,
    dataverse_host = parsed$host,
    dataverse_doi = parsed$doi
  ) |> unique()
  ids <- ids[!is.na(ids$dataverse_url), , drop = FALSE]
  valid <- ids[!is.na(ids$dataverse_host) & !is.na(ids$dataverse_doi), , drop = FALSE]

  if (nrow(valid) == 0) {
    ("No valid Dataverse links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "dataverse_url"))
  }

  if (!online(valid$dataverse_host[[1]])) {
    stop(valid$dataverse_host[[1]], " seems to be offline")
  }

  paste0(
    "Starting Dataverse retrieval for ",
    nrow(valid), " dataset",
    ifelse(nrow(valid) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", nrow(valid))
  for (i in seq_len(nrow(valid))) {
    id_info[[i]] <- .dataverse_info(valid$dataverse_host[[i]], valid$dataverse_doi[[i]], pb = pb)
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "dataverse_url") |>
    dplyr::left_join(info, by = c("dataverse_host", "dataverse_doi"), suffix = c("", ".dataverse"))

  paste0("...Dataverse retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one Dataverse dataset
#'
#' @param host the Dataverse installation's hostname (e.g. "dataverse.harvard.edu")
#' @param doi the dataset's persistentId (a DOI, e.g. "10.7910/DVN/GGDUND")
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @keywords internal
.dataverse_info <- function(host, doi, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ", host, " (", doi, ")...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(dataverse_host = host, dataverse_doi = doi)

  api_url <- sprintf(
    "https://%s/api/datasets/:persistentId/?persistentId=doi:%s",
    host, doi)

  resp <- .batch_query(api_url, msg = NULL, req_func = .dataverse_headers)[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(doi, " could not be found on ", host, call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  rec <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
  if (is.null(rec) || is.null(rec$data)) {
    obj$error <- "parse_error"
    return(obj)
  }

  data <- rec$data
  version <- data$latestVersion %||% list()
  fields <- version$metadataBlocks$citation$fields %||% list()

  field_val <- function(type_name) {
    for (f in fields) {
      if (identical(f$typeName, type_name)) return(f$value)
    }
    NULL
  }

  title <- field_val("title")
  authors_field <- field_val("author")
  authors <- if (is.list(authors_field)) {
    vapply(authors_field, function(a) {
      a$authorName$value %||% NA_character_
    }, character(1))
  } else {
    character(0)
  }

  obj$title <-            title %||% NA_character_
  obj$doi <-              data$persistentUrl %||% NA_character_
  obj$publication_date <- version$releaseTime %||% data$publicationDate %||% NA_character_
  obj$updated_date <-     version$lastUpdateTime %||% NA_character_
  obj$authors <-          list(authors)
  obj$license <-          version$license$name %||% NA_character_
  obj$files <-            list(version$files %||% list())

  return(obj)
}

# Add the right auth header to a Dataverse API/download request. Dataverse
# uses a single per-installation API token (X-Dataverse-key), unlike OSF/
# Zenodo's Bearer tokens -- named per-host with .dataverse_pat() below, since a
# token issued by one installation is meaningless to any other.
.dataverse_headers <- function(req) {
  host <- tryCatch(httr2::url_parse(req$url)$hostname, error = \(e) NULL)
  req <- req |> httr2::req_headers(`User-Agent` = "metacheck")
  pat <- tryCatch(.dataverse_pat(host), error = \(e) "")
  if (nzchar(pat %||% "")) {
    req <- req |> httr2::req_headers(`X-Dataverse-key` = pat)
  }
  req
}

#' Set or get a Dataverse API token
#'
#' Dataverse installations each issue their own API token (unlike Zenodo/OSF,
#' which are single services) -- a token from `dataverse.harvard.edu` is
#' meaningless to `dataverse.nl`. Tokens are therefore stored per host.
#'
#' Create a token by signing in to the installation, opening your account page,
#' and selecting "API Token".
#'
#' Store it as an environment variable named `DATAVERSE_PAT_<HOST>`, with the
#' host uppercased and every `.` replaced by `_` (e.g.
#' `DATAVERSE_PAT_DATAVERSE_HARVARD_EDU`), so it is read every time R starts.
#'
#' @param host the Dataverse installation's hostname (e.g. "dataverse.harvard.edu")
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' dataverse_pat("dataverse.harvard.edu") # returns "" unless a token is set
dataverse_pat <- function(host, pat = NULL) {
  .dataverse_pat(host, pat)
}

.dataverse_pat <- function(host, pat = NULL) {
  key <- toupper(gsub("[^A-Za-z0-9]+", "_", host %||% ""))
  opt <- paste0("metacheck.dataverse.pat.", key)
  env <- paste0("DATAVERSE_PAT_", key)

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set dataverse_pat with a single string containing your Dataverse token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Download all files from a Dataverse dataset
#'
#' Creates a directory for the dataset and downloads all of its files. Returns
#' (invisibly) a data frame with file info.
#'
#' You can limit downloads to only files under a specific size (defaults to
#' 10MB) and only a maximum download size (largest files will be omitted until
#' total size is under the limit). Omitted files will be listed as messages in
#' verbose mode, and included in the returned data frame with the `downloaded`
#' column value set to FALSE.
#'
#' A `.zip` in the dataset is normally fetched whole and left as a zip. Set
#' `unzip_types` to pull only the files you want out of it instead: Dataverse's
#' file-download endpoint honours byte-range requests (verified 2026-08-14
#' against demo.dataverse.org: `HEAD`/`Range` both return `Accept-Ranges: bytes`
#' and a `206 Partial Content` response) and each member of a zip is compressed
#' separately, so the wanted members can be read out of the archive while the
#' rest is never transferred -- the same mechanism [zenodo_file_download()]
#' uses. When the archive cannot be read that way, it is downloaded whole as
#' before, so this never loses a file -- it only avoids transferring one.
#'
#' @param host the Dataverse installation's hostname (e.g. "dataverse.harvard.edu")
#' @param doi the dataset's persistentId (a DOI, e.g. "10.7910/DVN/GGDUND")
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
#'   the members rather than the archive Dataverse published a size and MD5 for.
#' @export
#'
#' @examples
#' \dontrun{
#'   dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/GGDUND")
#'
#'   # take only the data files out of any zip in the dataset
#'   dataverse_file_download("dataverse.harvard.edu", "10.7910/DVN/GGDUND",
#'                           unzip_types = "data")
#' }
dataverse_file_download <- function(host, doi,
                                    download_to = ".",
                                    max_file_size = 10,
                                    max_download_size = 100,
                                    unzip_types = NULL,
                                    pb = NULL) {
  if (length(host) == 0 || length(doi) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Dataverse File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple datasets (vectorised host/doi) ----
  if (length(host) > 1 || length(doi) > 1) {
    host <- rep_len(host, max(length(host), length(doi)))
    doi  <- rep_len(doi,  max(length(host), length(doi)))
    pairs <- unique(data.frame(host = host, doi = doi, stringsAsFactors = FALSE))

    paste0(
      "Starting downloads for ", nrow(pairs), " Dataverse dataset",
      ifelse(nrow(pairs) == 1, "", "s"), "...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(seq_len(nrow(pairs)), function(i) {
      tryCatch(
        dataverse_file_download(
          pairs$host[[i]], pairs$doi[[i]],
          download_to = download_to,
          max_file_size = max_file_size,
          max_download_size = max_download_size,
          unzip_types = unzip_types,
          pb = pb
        ),
        error = function(e) {
          warning(pairs$doi[[i]], " resulted in an error:\n  ", conditionMessage(e), "\n")
          return(NULL)
        }
      )
    })

    dl_list <- dl_list[!vapply(dl_list, is.null, logical(1))]
    if (length(dl_list) == 0) return(NULL)

    dl <- dplyr::bind_rows(dl_list)
    paste0(
      "...Completed downloads for ", nrow(pairs), " Dataverse dataset",
      ifelse(nrow(pairs) == 1, "", "s")
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  if (is.null(host) || is.na(host) || !nzchar(host) ||
      is.null(doi) || is.na(doi) || !nzchar(doi)) return(NULL)

  # --- retrieve dataset contents ----
  paste0("Starting retrieval for ", doi, " on ", host) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(.dataverse_info(host, doi, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", doi, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from nested entries. Dataverse's file listing nests the
  # actual file metadata under "dataFile"; `id` is the numeric datafile id the
  # download endpoint (/api/access/datafile/<id>) takes.
  rows <- lapply(files_list, function(x) {
    df <- x$dataFile %||% list()
    dplyr::tibble(
      id       = as.character(df$id %||% NA_character_),
      key      = x$label %||% df$filename %||% NA_character_,
      size     = as.numeric(df$filesize %||% NA_real_),
      checksum = (df$checksum$value %||% NA_character_),
      checksum_type = tolower(df$checksum$type %||% NA_character_),
      self     = if (!is.null(df$id))
        sprintf("https://%s/api/access/datafile/%s", host, df$id)
      else NA_character_
    )
  })

  files <- dplyr::bind_rows(rows)
  if (nrow(files) == 0) {
    message("- ", doi, " contained no files")
    return(NULL)
  }

  # A zip we were asked to look inside is exempt from the size caps below, for
  # the same reason zenodo_file_download() exempts it: the caps measure the
  # file Dataverse would send, and for these we have decided not to transfer
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
  folder_name <- gsub("[^A-Za-z0-9._-]+", "_", doi)
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

  # No bulk whole-dataset archive path here (unlike Zenodo's files-archive):
  # Dataverse's /api/access/dataset/:persistentId endpoint zips the WHOLE
  # dataset regardless of the file selection above, so it would defeat the
  # size filters and the per-file API-token auth just applied. download_repo_files()
  # in repo-download.R makes that all-or-nothing tradeoff explicitly, the same
  # way it does for OSF/Zenodo; this per-record download always goes file by
  # file (or member by member, for a zip named in unzip_types).
  for (i in seq_len(n)) {
    # --- selected members out of a zip, instead of the whole zip ----
    if (unzippable[i]) {
      paste0("Reading zip contents of ", files$key[[i]]) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      got <- tryCatch(
        .dataverse_zip_members(files$self[[i]], dest = download_to,
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
            .dataverse_headers() |>
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
  files <- .dataverse_verify_downloads(files, download_to)

  n_missing <- sum(!files$downloaded)
  if (n_missing > 0) {
    worst <- files[!files$downloaded, ]
    warning(sprintf(
      "%d of %d file%s from Dataverse dataset %s did not arrive intact (e.g. %s). The returned table marks %s downloaded = FALSE. Run again to retry.",
      n_missing, nrow(files), plural(nrow(files)), doi,
      paste(utils::head(worst$key, 3), collapse = ", "),
      if (n_missing == 1) "it" else "them"), call. = FALSE)
  }

  # --- return table ----
  files$folder <- basename(download_to)
  files$dataverse_host <- host
  files$dataverse_doi <- doi
  files <- files[, c("folder", "dataverse_host", "dataverse_doi", "id", "key",
                     "path", "size", "size_on_disk", "checksum", "checksum_ok",
                     "self", "downloaded", "extracted")]

  invisible(files)
}

#' Fetch selected files out of a .zip in a Dataverse dataset without downloading it
#'
#' A zip published on Dataverse is often mostly material a reader does not
#' need: stimuli, images, or videos alongside the few data files. Dataverse
#' serves its per-file download URLs with byte-range support (verified
#' 2026-08-14 against demo.dataverse.org), and every member of a zip is
#' compressed on its own, so individual members can be pulled out of the
#' archive while the rest is never transferred (see `.zip_member_fetch()`,
#' shared with the identical Zenodo path).
#'
#' As with Zenodo, this is not possible for every archive: `.7z`, `.rar` and
#' `.tar.gz` compress all their files as one stream, so nothing inside can be
#' read without decompressing everything before it. Both cases fall back to
#' downloading the archive whole.
#'
#' @param url the file's download URL (`/api/access/datafile/<id>`)
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
.dataverse_zip_members <- function(url, dest,
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

#' Check downloaded Dataverse files against the file system
#'
#' Mirrors [`.zenodo_verify_downloads()`]: confirms every file the download
#' planned to save is present, is the size Dataverse reported, and (when
#' Dataverse published an MD5 or SHA-1) matches the checksum.
#'
#' A row whose `extracted` count is set is exempt from both checks, for the
#' same reason as the Zenodo case: Dataverse's size and checksum describe the
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
.dataverse_verify_downloads <- function(files, download_to) {
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

  # Dataverse publishes MD5 (default) or SHA-1, named by `checksum_type`.
  # tools::md5sum() only computes MD5, so only that type is verified here; a
  # SHA-1-only dataset falls back to the size check alone, same as Zenodo does
  # for a file with no checksum at all.
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
