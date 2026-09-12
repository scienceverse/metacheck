# PsychArchives (ZPID) is a DSpace repository. Unlike ResearchBox (which has no
# API, forcing archive-researchbox.R to scrape HTML and download a whole zip just
# to list its contents), PsychArchives exposes the legacy DSpace 6 REST API at
# https://www.psycharchives.org/rest/. That lets us LIST public bitstreams
# (name, size, retrieve URL) without downloading anything, so this file mirrors
# archive-researchbox.R structurally but defers the actual byte-fetch to
# download_repo_files() (the Zenodo/OSF pattern) rather than downloading eagerly.
#
# Access control is handled for us by the API: a restricted item's protected
# bitstreams simply do not appear in /bitstreams, so listing only ever yields
# publicly retrievable files.
#
# PsychArchives is not the only installation of this API: any DSpace <=6
# installation exposes the identical /rest/ shape. The functions below
# (.psycharchives_rest, .psycharchives_info, and therefore the exported
# psycharchives_info()/psycharchives_file_download()) take/derive a `host`
# rather than assuming www.psycharchives.org, so they work unmodified for any
# host in .dspace_legacy_hosts() -- see dspace_links() below, which is the
# general (all-hosts) counterpart to psycharchives_links() (PsychArchives
# only, kept for anyone calling it directly and expecting only PsychArchives
# results). Other installations found and verified live 2026-09-11 via
# DataCite's public client registry (api.datacite.org/clients?software=dspace,
# 169 results) filtered to hosts answering GET /rest/test with "REST api is
# running." (the same legacy-API health check psycharchives.org answers) --
# hosts answering the newer DSpace 7+ /server/api/ shape instead are handled
# separately, in archive-dspace7.R.
.dspace_legacy_hosts <- function() {
  c(
    "www.psycharchives.org",         # PsychArchives, ZPID (Germany)
    "bonndoc.ulb.uni-bonn.de",       # bonndoc, University of Bonn (Germany)
    "dspace.ut.ee",                  # University of Tartu Library (Estonia)
    "qsardb.org",                    # QsarDB
    "repositorio-digital.cide.edu",  # CIDE (Mexico)
    "ses.library.usyd.edu.au"        # University of Sydney eScholarship (Australia)
  )
}

.dspace_legacy_host_regex <- function() {
  paste(gsub("\\.", "\\\\.", .dspace_legacy_hosts()), collapse = "|")
}

# Extract (host, handle) from any reference to a known legacy-DSpace
# installation: a real hyperlink already carries its own host, so the handle
# is read out of the URL regardless of that installation's own handle prefix
# (e.g. "20.500.xxxx"); a BARE mention with no host attached (no domain in the
# text at all) can only be attributed to PsychArchives, since its prefix
# (20.500.12034) is the only one this package has on record -- another
# installation's own prefix would need to be looked up and added here before
# its bare mentions could be recognised the same way.
#
# Vectorised; returns a data.frame(host, handle) the same length as `url`,
# both NA for an element that matches neither case.
.dspace_legacy_parse <- function(url) {
  url <- as.character(url)
  n <- length(url)
  host <- rep(NA_character_, n)
  handle <- rep(NA_character_, n)
  has_url <- !is.na(url) & nzchar(url)
  if (!any(has_url)) return(data.frame(host = host, handle = handle, stringsAsFactors = FALSE))

  host_regex <- .dspace_legacy_host_regex()
  for (i in which(has_url)) {
    u <- url[i]
    hm <- regmatches(u, regexpr(host_regex, u, ignore.case = TRUE, perl = TRUE))
    if (length(hm) > 0) {
      host[i] <- tolower(hm)
    } else if (grepl("20\\.500\\.12034/[0-9]+", u)) {
      host[i] <- "www.psycharchives.org"
    }
    hd <- regmatches(u, regexpr("[0-9]{1,5}(?:\\.[0-9]+){0,3}/[0-9A-Za-z.]+", u, perl = TRUE))
    if (length(hd) > 0) handle[i] <- sub("[.,;]+$", "", hd)
  }
  data.frame(host = host, handle = handle, stringsAsFactors = FALSE)
}

# Extract the handle suffix (e.g. "20.500.12034/17526") from any PsychArchives
# reference: a hdl.handle.net URL, a psycharchives.org item page, or a bare
# handle. Returns NA_character_ when no handle is present. Kept as a thin
# wrapper over the general .dspace_legacy_parse() so existing callers are
# unaffected.
.psycharchives_handle <- function(url) {
  .dspace_legacy_parse(url)$handle[[1]]
}

#' Find DSpace (legacy REST API) Links in Papers
#'
#' Get all links to any of the legacy-DSpace-REST installations in
#' \code{.dspace_legacy_hosts()} (PsychArchives plus the others verified
#' 2026-09-11 -- see the note at the top of this file): real hyperlinks from
#' the paper's own \code{url} table, plus a body-text fallback. The fallback
#' recognises a bare mention (no hostname attached) only for PsychArchives'
#' own handle prefix (20.500.12034) -- see \code{.dspace_legacy_parse()} for
#' why the other installations' bare mentions cannot be resolved the same way.
#' This is the general (all-hosts) counterpart to \code{psycharchives_links()},
#' which stays PsychArchives-only for anyone calling it directly.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the DSpace url in the first (href) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' dspace_links(psychsci)
#' }
dspace_links <- function(paper) {
  href <- text <- NULL
  host_regex <- .dspace_legacy_host_regex()

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(host_regex, href, ignore.case = TRUE))

  ds_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?(?:", host_regex, ")/[A-Za-z0-9/._-]+",
    "|(?:https?://)?(?:hdl\\.handle\\.net/)?20\\.500\\.12034/[0-9]+"
  )
  other_ds <- text_search(paper, ds_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  dplyr::bind_rows(found_href, other_ds) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()
}

#' Find PsychArchives Links in Papers
#'
#' Get all PsychArchives links: real hyperlinks from the paper's own `url`
#' table, plus a body-text fallback for a BARE mention (a handle like
#' "20.500.12034/17526" is routinely cited without any URL scheme at all)
#' that the source PDF/HTML never encoded as an actual hyperlink — the `url`
#' table only ever contains links the source document itself made clickable.
#' Same two-tier approach `github_links()` uses for GitHub.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the PsychArchives url in the first (href) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' psycharchives_links(psychsci)
#' }
psycharchives_links <- function(paper) {
  href <- text <- NULL

  # Match both the psycharchives.org item pages and the hdl.handle.net handle
  # form (20.500.12034/...) that resolves to the same item.
  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("psycharchives\\.org|20\\.500\\.12034/", href,
                        ignore.case = TRUE))

  pa_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?psycharchives\\.org/[A-Za-z0-9/._-]+",
    "|(?:https?://)?(?:hdl\\.handle\\.net/)?20\\.500\\.12034/[0-9]+"
  )
  other_pa <- text_search(paper, pa_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  dplyr::bind_rows(found_href, other_pa) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()
}

#' Retrieve info from PsychArchives by URL
#'
#' @param pa_url a PsychArchives URL (item page, handle, or hdl.handle.net link),
#'   or a table containing them (e.g., as created by `psycharchives_links()`)
#' @param id_col the index or name of the column that contains PsychArchives URLs,
#'   if `pa_url` is a table
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a cached listing from a prior call for the
#'   same item instead of re-querying PsychArchives (see
#'   [repo_info_cache()]); default `FALSE`
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#' psycharchives_info("https://hdl.handle.net/20.500.12034/17526")
#' }
psycharchives_info <- function(pa_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (!online("psycharchives.org")) {
    stop("PsychArchives.org seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "PsychArchives Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(pa_url)) {
    table <- pa_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "pa_url"
    raw_urls <- unique(pa_url) |> stats::na.omit()
    table <- data.frame(pa_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    pa_url = raw_urls
  )
  ids <- ids[!is.na(ids$pa_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$pa_url)

  if (length(valid_ids) == 0) {
    ("No valid PsychArchives links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(table)
  }

  # iterate over valid IDs
  paste0(
    "Starting PsychArchives retrieval for ",
    length(valid_ids), " item",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  i <- 0
  error <- FALSE
  while (!error & i < length(valid_ids)) {
    i <- i + 1
    id <- valid_ids[[i]]
    cached <- if (isTRUE(cache)) .repo_info_cache_get("psycharchives", id) else NULL
    if (!is.null(cached)) {
      info <- cached
    } else {
      info <- .psycharchives_info(id)
      if (isTRUE(cache) && .repo_info_ok(info))
        .repo_info_cache_put("psycharchives", id, info)
    }
    if ("error" %in% names(info)) error <- TRUE
    id_info[[i]] <- info
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "pa_url")

  # reduplicate and add original table info
  by <- stats::setNames("pa_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".pa")
  )

  paste0("...PsychArchives retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from PsychArchives by URL
#'
#' @param pa_url a PsychArchives URL
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.psycharchives_info <- function(pa_url, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ", pa_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # set up return table
  obj <- data.frame(
    pa_url = pa_url
  )

  # Host is derived from pa_url itself (any known legacy-DSpace installation,
  # falling back to PsychArchives for a bare handle with no host attached --
  # see .dspace_legacy_parse()), so this function needs no host parameter of
  # its own and every existing caller (psycharchives_info(),
  # psycharchives_file_download()) automatically supports every host in
  # .dspace_legacy_hosts() without any change on their part.
  parsed <- .dspace_legacy_parse(pa_url)
  host <- parsed$host[[1]] %||% "www.psycharchives.org"
  handle <- parsed$handle[[1]]
  if (is.na(handle)) {
    warning(pa_url, " is not a valid PsychArchives handle", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # Resolve the handle to a DSpace item (UUID) via the REST API.
  item <- .psycharchives_rest(paste0("/handle/", handle), host = host)
  if (is.null(item) || is.null(item$uuid)) {
    warning(pa_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }
  uuid <- item$uuid

  # Item-level metadata (title, authors, DOI, rights, date).
  meta <- .psycharchives_rest(paste0("/items/", uuid, "?expand=metadata"), host = host)
  md <- meta$metadata %||% list()
  md_val <- function(key) {
    # %empty_or% (not %||%) because m$value can come back as a length-zero
    # value rather than NULL; vapply(..., character(1)) requires exactly
    # length 1 from every call.
    vals <- vapply(md, \(m) if (identical(m$key, key)) (m$value %empty_or% NA_character_) else NA_character_,
                   character(1))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
  }

  # %empty_or% (not %||%) because item$name can come back as a length-zero
  # value rather than NULL; %||% letting that through would break the $<-
  # assignment below with "replacement has 0 rows".
  obj$PA_title    <- item$name %empty_or% md_val("dc.title")
  obj$PA_authors  <- md_val("dc.contributor.author")
  obj$PA_doi      <- md_val("dc.identifier.doi")
  obj$PA_license  <- md_val("dc.rights")
  obj$PA_date     <- md_val("dc.date.available")
  obj$PA_abstract <- md_val("dc.description.abstract")

  # Public bitstream list. Restricted bitstreams are omitted by the API, so this
  # only ever contains publicly retrievable files.
  bitstreams <- .psycharchives_rest(paste0("/items/", uuid, "/bitstreams?limit=1000"), host = host)
  file_list <- if (length(bitstreams) == 0) {
    data.frame(
      name = character(0),
      size = numeric(0),
      retrieve = character(0)
    )
  } else {
    # %empty_or% (not %||%) because a field can come back as a length-zero
    # value rather than NULL; vapply(..., character(1)/numeric(1)) requires
    # exactly length 1 from every call.
    data.frame(
      name = vapply(bitstreams, \(b) b$name %empty_or% NA_character_, character(1)),
      size = vapply(bitstreams, \(b) as.numeric(b$sizeBytes %empty_or% NA_real_), numeric(1)),
      retrieve = vapply(bitstreams,
        \(b) if (is.null(b$retrieveLink)) NA_character_
             else paste0("https://", host, b$retrieveLink),
        character(1))
    )
  }
  obj$files <- list(file_list)

  return(obj)
}

# One DSpace legacy-REST request returning parsed JSON, or NULL on any failure
# / non-200. `host` defaults to PsychArchives so any pre-existing call site
# that never passed one keeps its exact prior behaviour. Kept internal and
# unexported; mirrors the httr2 error handling used elsewhere.
.psycharchives_rest <- function(path, host = "www.psycharchives.org") {
  url <- paste0("https://", host, "/rest", path)
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) return(NULL)
    httr2::resp_body_json(resp)
  }, error = \(e) NULL)
}

#' Retrieve public file list from PsychArchives by URL
#'
#' Lists the publicly retrievable bitstreams of one or more PsychArchives items
#' via the DSpace REST API, without downloading them. Each row carries an
#' absolute `file_url` (the bitstream retrieve endpoint) so the actual bytes are
#' fetched later by [download_repo_files()], the same deferred path used for
#' Zenodo and OSF. Restricted files are omitted by the API and never appear here.
#'
#' @param pa_url a vector of PsychArchives URLs
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a cached listing from a prior call for the
#'   same item instead of re-querying PsychArchives (see
#'   [repo_info_cache()]); default `FALSE`
#'
#' @returns a data frame of file information (one row per public bitstream)
#' @export
psycharchives_file_download <- function(pa_url, pb = NULL, cache = FALSE) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  # vectorise
  if (length(pa_url) > 1) {
    unique_pa <- unique(pa_url) |> setdiff(NA)

    file_lists <- lapply(unique_pa, psycharchives_file_download, pb = pb, cache = cache)
    info <- do.call(dplyr::bind_rows, args = file_lists)
    orig <- data.frame(pa_url = pa_url)
    df <- dplyr::left_join(orig, info, by = "pa_url")

    # Collect each item's rights flag (a named url -> dc.rights vector) so the
    # caller can warn about restricted items without a second API round-trip.
    rights <- unlist(lapply(file_lists, \(x) attr(x, "rights")))
    attr(df, "rights") <- rights
    doi <- unlist(lapply(file_lists, \(x) attr(x, "doi")))
    attr(df, "doi") <- doi

    return(df)
  }

  paste0("* Listing files from ", pa_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  cached <- if (isTRUE(cache)) .repo_info_cache_get("psycharchives", pa_url) else NULL
  if (!is.null(cached)) {
    info <- cached
  } else {
    info <- .psycharchives_info(pa_url, pb = pb)
    if (isTRUE(cache) && .repo_info_ok(info))
      .repo_info_cache_put("psycharchives", pa_url, info)
  }
  if ("error" %in% names(info)) return(NULL)

  # Rights flag (e.g. "restrictedAccess") and doi carried as attributes rather
  # than columns, so the file frame stays file-only while the caller can still
  # read them. Reuses the metadata .psycharchives_info() already fetched.
  rights <- stats::setNames(info$PA_license %||% NA_character_, pa_url)
  doi <- stats::setNames(info$PA_doi %||% NA_character_, pa_url)

  file_list <- info$files[[1]]
  if (is.null(file_list) || nrow(file_list) == 0) {
    # Item resolved but exposes no public bitstreams (typically a fully
    # restricted item). Return a zero-row frame that still carries the
    # rights/doi attributes, so the restricted-access warning (and license/
    # doi surfacing in repo_check) can still fire.
    empty <- data.frame(
      pa_url = character(0), name = character(0), file_url = character(0),
      file_location = character(0), size = numeric(0), isdir = logical(0),
      ext = character(0), type = character(0)
    )
    attr(empty, "rights") <- rights
    attr(empty, "doi") <- doi
    return(empty)
  }

  pa_file_info <- data.frame(
    pa_url = rep(pa_url, nrow(file_list)),
    name = file_list$name,
    file_url = file_list$retrieve,
    file_location = NA_character_,
    size = file_list$size,
    isdir = FALSE
  )

  pa_file_info$ext <- strsplit(pa_file_info$name, "\\.") |>
    sapply(\(x) {
      if (length(x) < 2) {
        return("")
      }
      x[[length(x)]]
    }) |>
    tolower()
  pa_file_info <- dplyr::left_join(
    pa_file_info,
    metacheck::file_types,
    by = "ext"
  )

  attr(pa_file_info, "rights") <- rights
  attr(pa_file_info, "doi") <- doi

  return(pa_file_info)
}
