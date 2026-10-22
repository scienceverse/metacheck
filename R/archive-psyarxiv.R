# PsyArXiv's own API (https://next.psyarxiv.org/api)
#
# PsyArXiv preprints used to be reachable only through the OSF API, as the
# OSF preprint provider "psyarxiv" (see osf_preprint_list()). PsyArXiv now has
# its own read-only API: no key, open CORS, and the same ids (a preprint keeps
# its OSF guid; an author from the OSF is "osf:<guid>"). These functions read
# it. The OSF functions are unchanged.
#
# The base address is the option `metacheck.psyarxiv.api` (default
# "https://next.psyarxiv.org/api/v1"; the API is still a prototype, so the
# address may change when it leaves next.psyarxiv.org).

# -- internal helpers -------------------------------------------------------------

# The site that relative links in API answers (fileUrl, url, links.next) are
# relative to: the API address without its /api/... path.
.psyarxiv_site <- function() {
  sub("/api(/.*)?$", "", getOption("metacheck.psyarxiv.api"))
}

# A relative link from the API made absolute; NA and absolute links unchanged.
.psyarxiv_abs <- function(url) {
  url <- as.character(url)
  rel <- !is.na(url) & startsWith(url, "/")
  url[rel] <- paste0(.psyarxiv_site(), url[rel])
  url
}

# One GET to the API. `path` starts with "/" and is relative to the API
# address, or is a full URL (a `links.next` from an earlier answer). Returns
# the parsed JSON as nested lists, or stops with the API's own error message
# (it answers errors as {"error": "...", "statusCode": n}).
.psyarxiv_get <- function(path, query = list()) {
  url <- if (grepl("^https?://", path)) path else
    paste0(getOption("metacheck.psyarxiv.api"), path)
  query <- query[!vapply(query, is.null, logical(1))]
  resp <- .mc_request(url) |>
    httr2::req_url_query(!!!query, .multi = "comma") |>
    httr2::req_headers(`User-Agent` = "scienceverse/metacheck") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 502, 503)) |>
    httr2::req_perform()
  sc <- httr2::resp_status(resp)
  if (sc >= 400) {
    msg <- tryCatch(httr2::resp_body_json(resp)$error, error = \(e) NULL) %||%
      httr2::resp_status_desc(resp)
    logger("psyarxiv", list(url = url, status = sc, error = msg))
    stop(sprintf("PsyArXiv API: %s (HTTP %d) for %s", msg, sc, url), call. = FALSE)
  }
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

# NULL (JSON null) as NA of the right type, for building data frames.
.psyarxiv_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
.psyarxiv_lgl <- function(x) if (is.null(x)) NA else as.logical(x)
.psyarxiv_int <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)

# Several values (a list of strings, or of objects with `field`) as one
# string, "; "-separated; NA when there are none.
.psyarxiv_join <- function(x, field = NULL) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  vals <- if (is.null(field)) unlist(x) else
    vapply(x, \(i) .psyarxiv_chr(i[[field]]), character(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
}

# One preprint (a list from the API: from a listing, a single record or a
# bulk file) as a one-row data frame. The listing fields are always present;
# the fields only a single record carries are NA otherwise (bulk files have
# the listing fields only, checked 2026-10-22).
.psyarxiv_row <- function(p) {
  a <- p$authors %||% list()
  data.frame(
    psyarxiv_id = .psyarxiv_chr(p$id),
    title = .psyarxiv_chr(p$title),
    abstract = .psyarxiv_chr(p$abstract),
    state = .psyarxiv_chr(p$state),
    version = .psyarxiv_int(p$version),
    doi = .psyarxiv_chr(p$doi),
    publication_doi = .psyarxiv_chr(p$publicationDoi),
    first_posted = .psyarxiv_chr(p$firstPostedAt),
    version_posted = .psyarxiv_chr(p$versionPostedAt),
    license = .psyarxiv_chr(p$license$name),
    authors = .psyarxiv_join(a, "name"),
    author_ids = .psyarxiv_join(a, "id"),
    author_orcids = .psyarxiv_join(a, "orcid"),
    author_count = .psyarxiv_int(p$authorCount %||% length(a)),
    subjects = .psyarxiv_join(p$subjects, "name"),
    tags = .psyarxiv_join(p$tags),
    has_data = .psyarxiv_lgl(p$hasData),
    has_prereg = .psyarxiv_lgl(p$hasPrereg),
    language = .psyarxiv_chr(p$language),
    url = .psyarxiv_abs(.psyarxiv_chr(p$url)),
    # only in a single record (psyarxiv_info())
    data_status = .psyarxiv_chr(p$dataStatus),
    data_links = .psyarxiv_join(p$dataLinks),
    prereg_status = .psyarxiv_chr(p$preregStatus),
    prereg_links = .psyarxiv_join(p$preregLinks),
    materials_status = .psyarxiv_chr(p$materialsStatus),
    materials_links = .psyarxiv_join(p$materialsLinks),
    has_coi = .psyarxiv_lgl(p$hasCoi),
    coi_statement = .psyarxiv_chr(p$coiStatement),
    supplements_url = .psyarxiv_chr(p$supplements$url),
    supplements_status = .psyarxiv_chr(p$supplements$status),
    file_url = .psyarxiv_abs(.psyarxiv_chr(p$fileUrl)),
    osf_url = .psyarxiv_chr(p$osfUrl),
    n_versions = if (is.null(p$versions)) NA_integer_ else length(p$versions),
    withdrawal_reason = .psyarxiv_chr(p$withdrawalReason),
    stringsAsFactors = FALSE
  )
}

.psyarxiv_rows <- function(results) {
  if (length(results) == 0) return(.psyarxiv_row(list())[0, ])
  do.call(rbind, lapply(results, .psyarxiv_row))
}

# The versions of one record as a data frame.
.psyarxiv_version_rows <- function(id, versions) {
  if (length(versions) == 0) {
    return(data.frame(psyarxiv_id = character(0), version = integer(0),
                      doi = character(0), posted = character(0),
                      state = character(0), file_url = character(0),
                      file_name = character(0), osf_file_url = character(0),
                      file_revisions = integer(0),
                      withdrawal_reason = character(0)))
  }
  do.call(rbind, lapply(versions, \(v) data.frame(
    psyarxiv_id = id,
    version = .psyarxiv_int(v$version),
    doi = .psyarxiv_chr(v$doi),
    posted = .psyarxiv_chr(v$postedAt),
    state = .psyarxiv_chr(v$state),
    file_url = .psyarxiv_abs(.psyarxiv_chr(v$fileUrl)),
    file_name = .psyarxiv_chr(v$fileName),
    osf_file_url = .psyarxiv_chr(v$osfFileUrl),
    file_revisions = .psyarxiv_int(v$fileRevisions),
    withdrawal_reason = .psyarxiv_chr(v$withdrawalReason),
    stringsAsFactors = FALSE
  )))
}

# A preprint id from an id, a PsyArXiv, OSF or psyarxiv.com link, or a
# PsyArXiv DOI (10.31234/osf.io/<id>, optionally with _v<n>).
.psyarxiv_id <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^10\\.31234/osf\\.io/", "", x)
  x <- sub("^https?://[^/]+/(preprints?/(psyarxiv/)?)?", "", x)
  x <- sub("[/?#].*$", "", x)
  sub("_v\\d+$", "", x)
}

# -- search -----------------------------------------------------------------------

#' Search PsyArXiv Preprints
#'
#' Search and list PsyArXiv preprints with PsyArXiv's own API, which replaces
#' listing them through the OSF ([osf_preprint_list()] with
#' `provider = "psyarxiv"`). No key is needed.
#'
#' `q` uses PsyArXiv's search syntax: words are combined with AND;
#' `"working memory"` for a phrase; `memor*` for truncation (there is no
#' stemming); `OR`, `NOT` or `-`, and brackets; and the fields `title:`,
#' `abstract:`, `author:`, `tag:` and `subject:` (or `ti:`, `ab:`, `au:`,
#' `kw:`, `su:`). See <https://next.psyarxiv.org/help/search>.
#'
#' Year and date filters use the date version 1 was posted, which never
#' changes. For the whole catalog, [psyarxiv_dump()] is faster than paging.
#'
#' @param q a search query (see Details), or NULL to list all preprints
#' @param subject subject id(s); includes the subjects beneath them (see [psyarxiv_subjects()])
#' @param license license id(s) (see [psyarxiv_licenses()])
#' @param year_from,year_to first-posted year range, inclusive
#' @param from,to first-posted date range ("YYYY-MM-DD"), inclusive
#' @param has_data,has_prereg TRUE to keep only preprints with open data, or with a preregistration
#' @param author an author id, e.g. "osf:kp6rt"
#' @param language an ISO 639-1 language code, e.g. "de"
#' @param state "published" (default), "withdrawn" or "all"
#' @param sort "relevance" (default with `q`), "newest" (default without), "oldest" or "title"
#' @param per_page results per page (at most 100)
#' @param page_start the first page to get
#' @param page_end the last page to get (`Inf` for all pages)
#'
#' @returns a data frame with one row per preprint (see [psyarxiv_info()] for
#'   the columns; the ones only a single record has are NA here), with the
#'   attributes `total` (the number of matching preprints) and `interpreted`
#'   (the query as the search engine read it)
#' @export
#' @seealso [psyarxiv_info()], [psyarxiv_dump()], [osf_preprint_list()]
#'
#' @examples
#' \donttest{
#' # preprints with "working memory" in the title and open data, newest first
#' pp <- psyarxiv_search('title:"working memory"', has_data = TRUE,
#'                       sort = "newest", per_page = 5)
#' attr(pp, "total")
#' }
psyarxiv_search <- function(q = NULL,
                            subject = NULL,
                            license = NULL,
                            year_from = NULL,
                            year_to = NULL,
                            from = NULL,
                            to = NULL,
                            has_data = NULL,
                            has_prereg = NULL,
                            author = NULL,
                            language = NULL,
                            state = NULL,
                            sort = NULL,
                            per_page = 100,
                            page_start = 1,
                            page_end = page_start) {
  if (!is.null(state)) state <- match.arg(state, c("published", "withdrawn", "all"))
  if (!is.null(sort)) sort <- match.arg(sort, c("relevance", "newest", "oldest", "title"))
  stopifnot(per_page >= 1, page_start >= 1, page_end >= page_start)
  lgl <- \(x) if (is.null(x)) NULL else tolower(as.character(isTRUE(x)))

  query <- list(q = q, subject = subject, license = license,
                year_from = year_from, year_to = year_to,
                from = if (is.null(from)) NULL else as.character(from),
                to = if (is.null(to)) NULL else as.character(to),
                has_data = lgl(has_data), has_prereg = lgl(has_prereg),
                author = author, language = language, state = state,
                sort = sort, per_page = min(per_page, 100), page = page_start)

  content <- .psyarxiv_get("/preprints", query)
  results <- content$results
  total <- content$total
  page <- page_start
  next_url <- content$links$`next`
  while (!is.null(next_url) && page < page_end) {
    page <- page + 1
    content <- .psyarxiv_get(.psyarxiv_abs(next_url))
    results <- c(results, content$results)
    next_url <- content$links$`next`
  }

  out <- .psyarxiv_rows(results)
  attr(out, "total") <- total
  attr(out, "interpreted") <- content$interpreted
  out
}

# -- single records ---------------------------------------------------------------

#' Get PsyArXiv Preprint Records
#'
#' The full record of one or more PsyArXiv preprints: what [psyarxiv_search()]
#' returns plus the open data, preregistration and materials statements and
#' links, the conflict of interest statement, the linked OSF project with the
#' supplements, and the link to the current file.
#'
#' @param id preprint ids, PsyArXiv or OSF preprint links, or PsyArXiv DOIs
#'   (e.g. "28j7s", "https://osf.io/preprints/psyarxiv/28j7s",
#'   "10.31234/osf.io/28j7s_v2")
#'
#' @returns a data frame with one row per id. A preprint the API does not
#'   have gives a row with only `psyarxiv_id` and `error` filled in.
#' @export
#' @seealso [psyarxiv_versions()], [psyarxiv_download()]
#'
#' @examples
#' \donttest{
#' pp <- psyarxiv_info("pr4u6")
#' pp$data_status
#' pp$supplements_url
#' }
psyarxiv_info <- function(id) {
  ids <- .psyarxiv_id(id)
  rows <- lapply(ids, \(i) {
    rec <- tryCatch(.psyarxiv_get(paste0("/preprints/", i)),
                    error = \(e) conditionMessage(e))
    if (is.character(rec)) {
      row <- .psyarxiv_row(list(id = i))
      row$error <- rec
    } else {
      row <- .psyarxiv_row(rec)
      row$error <- NA_character_
    }
    row
  })
  do.call(rbind, rows)
}

#' Get the Versions of a PsyArXiv Preprint
#'
#' Each version of a preprint has its own DOI, date and file.
#'
#' @param id a preprint id, link or DOI (see [psyarxiv_info()])
#'
#' @returns a data frame with one row per version, newest first: its DOI,
#'   date, state, the file on PsyArXiv (`file_url`) and on the OSF
#'   (`osf_file_url`), and how many earlier revisions of the file the OSF kept
#' @export
#'
#' @examples
#' \donttest{
#' psyarxiv_versions("pr4u6")
#' }
psyarxiv_versions <- function(id) {
  i <- .psyarxiv_id(id)[1]
  content <- .psyarxiv_get(paste0("/preprints/", i, "/versions"))
  .psyarxiv_version_rows(i, content$versions)
}

#' Get a PsyArXiv Author
#'
#' @param id an author id, e.g. "osf:kp6rt" (an OSF user guid alone is
#'   prefixed with "osf:")
#'
#' @returns a one-row data frame: name, ORCID (and whether the OSF verified
#'   it), affiliation, number of preprints, OSF profile, and the author's
#'   subjects and frequent coauthors. List their preprints with
#'   `psyarxiv_search(author = id)`.
#' @export
#'
#' @examples
#' \donttest{
#' psyarxiv_author("osf:kp6rt")
#' }
psyarxiv_author <- function(id) {
  id <- as.character(id)[1]
  if (!grepl(":", id)) id <- paste0("osf:", id)
  a <- .psyarxiv_get(paste0("/authors/", utils::URLencode(id, reserved = TRUE)))
  data.frame(
    author_id = .psyarxiv_chr(a$id),
    name = .psyarxiv_chr(a$name),
    orcid = .psyarxiv_chr(a$orcid),
    orcid_verified = .psyarxiv_lgl(a$orcidVerified),
    affiliation = .psyarxiv_chr(a$affiliation),
    preprint_count = .psyarxiv_int(a$preprintCount),
    osf_profile_url = .psyarxiv_chr(a$osfProfileUrl),
    subjects = .psyarxiv_join(a$subjects, "name"),
    coauthors = .psyarxiv_join(a$coauthors, "name"),
    coauthor_ids = .psyarxiv_join(a$coauthors, "id"),
    stringsAsFactors = FALSE
  )
}

# -- files ------------------------------------------------------------------------

#' Download PsyArXiv Preprint Files
#'
#' Downloads the manuscript of each preprint (the current version, or a
#' given version) from PsyArXiv's copy, or from the OSF when PsyArXiv has
#' only the OSF link.
#'
#' @param id preprint ids, links or DOIs (see [psyarxiv_info()]); a DOI with
#'   "_v<n>" downloads that version
#' @param version a version number for all ids, or NULL for the version in
#'   the DOI, else the current one
#' @param save_path the folder to save the files in
#' @param overwrite whether to replace a file that is already there
#'
#' @returns the paths of the saved files (NA where nothing could be
#'   downloaded), invisibly. Files are named `<id>_v<version>.<ext>`.
#' @export
#'
#' @examples
#' \dontrun{
#' psyarxiv_download("28j7s", save_path = tempdir())
#' }
psyarxiv_download <- function(id, version = NULL, save_path = ".",
                              overwrite = FALSE) {
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  doi_version <- suppressWarnings(as.integer(
    sub(".*_v(\\d+)$", "\\1", ifelse(grepl("_v\\d+$", id), id, NA))))
  ids <- .psyarxiv_id(id)
  paths <- vapply(seq_along(ids), \(k) {
    i <- ids[k]
    v <- tryCatch(psyarxiv_versions(i), error = \(e) {
      warning(i, ": ", conditionMessage(e), call. = FALSE); NULL
    })
    if (is.null(v) || nrow(v) == 0) return(NA_character_)
    want <- version %||% doi_version[k]
    row <- if (is.na(want %||% NA)) v[which.max(v$version), ] else v[v$version == want, ]
    if (nrow(row) == 0) {
      warning(i, ": no version ", want, call. = FALSE); return(NA_character_)
    }
    src <- if (!is.na(row$file_url)) row$file_url else row$osf_file_url
    if (is.na(src)) {
      warning(i, " version ", row$version, " has no file (withdrawn?)", call. = FALSE)
      return(NA_character_)
    }
    ext <- tools::file_ext(row$file_name %||% "")
    if (is.na(ext) || ext == "") ext <- "pdf"
    dest <- file.path(save_path, sprintf("%s_v%d.%s", i, row$version, ext))
    if (file.exists(dest) && !overwrite) return(dest)
    ok <- tryCatch({
      .mc_request(src) |>
        httr2::req_headers(`User-Agent` = "scienceverse/metacheck") |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(path = dest)
      TRUE
    }, error = \(e) {
      warning(i, ": download failed: ", conditionMessage(e), call. = FALSE)
      FALSE
    })
    if (ok) dest else NA_character_
  }, character(1))
  invisible(paths)
}

# -- bulk files -------------------------------------------------------------------

#' List the PsyArXiv Bulk Files
#'
#' PsyArXiv publishes its whole catalog as gzipped JSON Lines, one file per
#' year a preprint was first posted (withdrawn preprints included).
#'
#' @returns a data frame: file name, year, size in bytes, number of
#'   preprints, when it was built, and its download link
#' @export
#'
#' @examples
#' \donttest{
#' psyarxiv_dumps()
#' }
psyarxiv_dumps <- function() {
  d <- .psyarxiv_get("/dumps")$dumps
  data.frame(
    name = vapply(d, \(x) .psyarxiv_chr(x$name), character(1)),
    year = vapply(d, \(x) .psyarxiv_int(x$year), integer(1)),
    size = vapply(d, \(x) as.numeric(x$size %||% NA), numeric(1)),
    records = vapply(d, \(x) .psyarxiv_int(x$records), integer(1)),
    built = vapply(d, \(x) .psyarxiv_chr(x$builtAt), character(1)),
    url = .psyarxiv_abs(vapply(d, \(x) .psyarxiv_chr(x$url), character(1))),
    stringsAsFactors = FALSE
  )
}

#' Download PsyArXiv Bulk Files
#'
#' Downloads the yearly bulk files and, by default, reads them into one data
#' frame with the same columns as [psyarxiv_search()]. This is the way to get
#' the whole catalog; paging through [psyarxiv_search()] is much slower.
#'
#' The bulk files hold what a search result holds, so the columns only a
#' single record has (the data, preregistration and materials statements,
#' the supplements and the file links) are NA; get them with
#' [psyarxiv_info()] for the preprints you need.
#'
#' @param year the years to get (default: all)
#' @param save_path the folder to save the files in
#' @param read whether to read the files and return a data frame
#' @param overwrite whether to download a file again when it is already there
#'
#' @returns a data frame of preprints (when `read = TRUE`), else the paths
#'   of the downloaded files
#' @export
#'
#' @examples
#' \dontrun{
#' pp2016 <- psyarxiv_dump(2016)
#' }
psyarxiv_dump <- function(year = NULL, save_path = tempdir(), read = TRUE,
                          overwrite = FALSE) {
  d <- psyarxiv_dumps()
  if (!is.null(year)) {
    missing <- setdiff(year, d$year)
    if (length(missing)) warning("No bulk file for ", paste(missing, collapse = ", "), call. = FALSE)
    d <- d[d$year %in% year, , drop = FALSE]
  }
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(save_path, d$name)
  for (k in seq_along(paths)) {
    if (file.exists(paths[k]) && !overwrite) next
    .mc_request(d$url[k], low_speed_time = 120) |>
      httr2::req_headers(`User-Agent` = "scienceverse/metacheck") |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = paths[k])
  }
  if (!read) return(invisible(paths))
  rows <- lapply(paths, \(p) {
    con <- gzfile(p, open = "r", encoding = "UTF-8")
    on.exit(close(con))
    lines <- readLines(con, warn = FALSE)
    lines <- lines[nzchar(lines)]
    .psyarxiv_rows(lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE))
  })
  do.call(rbind, rows)
}

# -- reference lists --------------------------------------------------------------

#' PsyArXiv Subjects
#'
#' @returns a data frame of PsyArXiv's subject taxonomy (three levels): id,
#'   name, parent id, depth (0 = top) and the number of preprints
#' @export
#'
#' @examples
#' \donttest{
#' psyarxiv_subjects()
#' }
psyarxiv_subjects <- function() {
  s <- .psyarxiv_get("/subjects")$subjects
  data.frame(
    id = vapply(s, \(x) .psyarxiv_chr(x$id), character(1)),
    name = vapply(s, \(x) .psyarxiv_chr(x$name), character(1)),
    parent_id = vapply(s, \(x) .psyarxiv_chr(x$parentId), character(1)),
    depth = vapply(s, \(x) .psyarxiv_int(x$depth), integer(1)),
    count = vapply(s, \(x) .psyarxiv_int(x$count), integer(1)),
    stringsAsFactors = FALSE
  )
}

#' PsyArXiv Licenses
#'
#' @returns a data frame of the licenses PsyArXiv preprints use: id, name,
#'   short name, link, and the number of preprints
#' @export
#'
#' @examples
#' \donttest{
#' psyarxiv_licenses()
#' }
psyarxiv_licenses <- function() {
  l <- .psyarxiv_get("/licenses")$licenses
  data.frame(
    id = vapply(l, \(x) .psyarxiv_chr(x$id), character(1)),
    name = vapply(l, \(x) .psyarxiv_chr(x$name), character(1)),
    short_name = vapply(l, \(x) .psyarxiv_chr(x$shortName), character(1)),
    url = vapply(l, \(x) .psyarxiv_chr(x$url), character(1)),
    count = vapply(l, \(x) .psyarxiv_int(x$count), integer(1)),
    stringsAsFactors = FALSE
  )
}
