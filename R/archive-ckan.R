# CKAN (https://ckan.org) is the catalogue software behind many government
# and research data portals (EnviDat, the Natural History Museum's Data
# Portal, Canada's and California's open data, ...). Every installation
# answers the same API (#637; verified 2026-11-13 on data.cnra.ca.gov and
# www.envidat.ch):
#   * GET /api/3/action/status_show names the software (ckan_version); the
#     repository survey used it to find the installations
#     (data-raw/repo_survey/repo_hosts.R, R/repo-tables.R);
#   * GET /api/3/action/package_show?id=<name or id> gives a dataset: its
#     title, notes, author, license_id, doi (where the portal registers one)
#     and its `resources`, each with a name, format, size, url and url_type.
# A resource is a file the catalogue holds ("upload", or an address on the
# portal itself), or a link to another website (an ArcGIS map page, a
# government page); only the first are the dataset's files, as for LDbase's
# "external link" items. A dataset's address is /dataset/<name or id> (on
# open.canada.ca /data/en/dataset/<id>), or on EnviDat /#/metadata/<name>,
# where its DOIs lead (10.16904/envidat.75).

# CKAN installations: Canada's open data portal, whose CKAN is mounted under
# /data (its API at /data/api/3/..., checked 2026-11-13; the survey's check
# asked the host's root, as for every other installation, where only this
# one of 49 answered under /data instead), and the installations the
# repository survey found (R/repo-tables.R)
.ckan_hosts <- function() {
  unique(c("open.canada.ca", .ckan_hosts_survey()))
}

# Where an installation's API is, below its host: "" for all but those
# mounted under a path
.ckan_api_base <- function(host) {
  bases <- c("open.canada.ca" = "/data")
  b <- unname(bases[sub("^www\\.", "", tolower(host))])
  ifelse(is.na(b), "", b)
}

# DOI prefixes of the hand-listed installations (DataCite's repositories
# whose DOIs land on open.canada.ca: Agriculture and Agri-Food Canada, the
# Canada Energy Regulator, GEOSCAN, PSC), with the survey's
.ckan_doi_prefixes_all <- function() {
  unique(c("10.23684", "10.35002", "10.23687", "10.82126", .ckan_doi_prefixes()))
}

#' Find CKAN data portal links in a paper
#'
#' Finds links to datasets on the CKAN data portals in metacheck's list
#' (EnviDat, the Natural History Museum's Data Portal, government open data
#' portals, ...), as hyperlinks or in the text, and DOIs under one of these
#' portals' DOI prefixes, which are looked up once at DataCite to find the
#' dataset's address (a DOI under any other prefix is not looked up).
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with a row for each link (`href`, plus `text_id` /
#'   `paper_id`), with `ckan_url` (the link) and `ckan_id` (the portal's
#'   host and the dataset's name or id, `host/name`)
#' @export
#'
#' @examples
#' paper <- test_paper(url = "https://data.cnra.ca.gov/dataset/statewide-crop-mapping")
#' ckan_links(paper)
ckan_links <- function(paper) {
  href <- text <- NULL
  hosts <- .ckan_hosts()
  empty <- dplyr::tibble(paper_id = character(0), href = character(0), ckan_url = character(0), ckan_id = character(0))
  if (!length(hosts)) return(empty)
  rx <- paste(gsub("\\.", "\\\\.", hosts), collapse = "|")
  path <- "(?:/data/[a-z]{2})?/(?:dataset|#/metadata)/[-A-Za-z0-9_]+"
  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(paste0("(?:", rx, ")", path), href, ignore.case = TRUE, perl = TRUE))
  bare <- text_search(paper, paste0("(?:https?://)?(?:www\\.)?(?:", rx, ")", path),
                      return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))
  # a DOI under a portal's prefix, replaced by the address it is registered
  # with when that is on one of these hosts
  pre <- .ckan_doi_prefixes_all()
  by_doi <- NULL
  if (length(pre)) {
    doi_rx <- paste0("(?:https?://)?(?:dx\\.)?(?:doi\\.org/)?((?:", paste(gsub("\\.", "\\\\.", pre), collapse = "|"),
                     ")/[-._;()/:A-Za-z0-9]+[A-Za-z0-9])")
    by_doi <- text_search(paper, doi_rx, return = "match", perl = TRUE) |>
      dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))
    if (nrow(by_doi)) {
      doi <- sub(doi_rx, "\\1", by_doi$href, perl = TRUE, ignore.case = TRUE)
      by_doi$href <- vapply(doi, \(d) {
        u <- .repo_doi_url(d)$url
        if (!is.na(u) && .url_host_is(u, hosts)) u else NA_character_
      }, character(1), USE.NAMES = FALSE)
      by_doi <- by_doi[!is.na(by_doi$href), , drop = FALSE]
    }
  }
  links <- dplyr::bind_rows(found_href, bare, by_doi)
  if (!nrow(links)) return(empty)
  links$href <- sub("[/.]+$", "", links$href)
  links <- unique(links)
  links$ckan_url <- links$href
  links$ckan_id <- .ckan_id(links$ckan_url)
  dplyr::as_tibble(links[!is.na(links$ckan_id), , drop = FALSE])
}

# "<host>/<dataset name or id>" from a CKAN dataset address, NA otherwise
.ckan_id <- function(url) {
  if (!length(url)) return(character(0))
  rx <- paste0("(?i)^(?:https?://)?((?:www\\.)?(?:", paste(gsub("\\.", "\\\\.", .ckan_hosts()), collapse = "|"),
               "))(?:/data/[a-z]{2})?/(?:dataset|#/metadata)/([-A-Za-z0-9_]+)")
  m <- regmatches(url, regexec(rx, url, perl = TRUE))
  vapply(m, \(x) if (length(x) >= 3) paste0(tolower(x[[2]]), "/", x[[3]]) else NA_character_, character(1))
}

#' Get information about CKAN datasets
#'
#' Reads each dataset's record from its portal's API
#' (`/api/3/action/package_show`): title, DOI, licence, and its files. A
#' resource that is only a link to another website (a map page, another
#' agency's page) is not one of the dataset's files and is not listed.
#'
#' @param ckan_url a vector of CKAN dataset URLs, or a table containing them
#'   (e.g. from [ckan_links()])
#' @param id_col the index or name of the column that contains the URLs, if
#'   `ckan_url` is a table
#' @param pb a progress bar from [pb()], or NULL to create one
#' @param cache if `TRUE`, a listing is read from / written to the persistent
#'   repository-listing cache (see `repo_info_cache_clear()`)
#'
#' @returns a data frame with one row per link: `ckan_url`, `ckan_id`,
#'   `title`, `doi`, `license`, `authors`, a list-column `files` (a data frame
#'   per link with `file_name`, `file_path`, `file_url`, `file_size`,
#'   `format`) and `error` (NA, or why the link could not be listed)
#' @export
#' @examples
#' \donttest{
#' if (online("www.envidat.ch")) {
#'   ckan_info("https://www.envidat.ch/#/metadata/chelsa-climatologies")
#' }
#' }
ckan_info <- function(ckan_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "CKAN Retrieve"))
    on.exit(pb$terminate())
  }
  urls <- if (is.data.frame(ckan_url)) ckan_url[[id_col]] else ckan_url
  urls <- unique(stats::na.omit(as.character(urls)))
  urls <- urls[nzchar(urls)]
  if (!length(urls)) {
    return(data.frame(ckan_url = character(0), ckan_id = character(0), title = character(0),
                      doi = character(0), license = character(0), error = character(0)))
  }
  rows <- lapply(urls, function(u) {
    id <- .ckan_id(u)
    cached <- if (isTRUE(cache) && !is.na(id)) .repo_info_cache_get("ckan", id) else NULL
    if (!is.null(cached)) { cached$ckan_url <- u; return(cached) }
    res <- .ckan_info(u, id, pb = pb)
    if (isTRUE(cache) && .repo_info_ok(res)) .repo_info_cache_put("ckan", id, res)
    res
  })
  pb$tick(0, list(what = "...CKAN retrieval complete!"))
  dplyr::bind_rows(rows)
}

# One CKAN dataset -> one row (see ckan_info())
.ckan_info <- function(url, id, pb = NULL) {
  obj <- data.frame(ckan_url = url, ckan_id = id, title = NA_character_, doi = NA_character_,
                    license = NA_character_, error = NA_character_)
  obj$authors <- list(character(0))
  obj$files <- list(NULL)
  if (is.na(id)) { obj$error <- "not a CKAN dataset link"; return(obj) }
  host <- sub("/.*$", "", id)
  if (!is.null(pb)) pb$tick(0, list(what = paste0("* Retrieving info from CKAN dataset ", id, "...")))
  api <- sprintf("https://%s%s/api/3/action/package_show?id=%s", host, .ckan_api_base(host),
                 utils::URLencode(sub("^[^/]+/", "", id), reserved = TRUE))
  resp <- .batch_query(api, msg = NULL)[[1]]
  if (is.null(resp) || inherits(resp, "error") || httr2::resp_status(resp) != 200) {
    obj$error <- "unfound"
    return(obj)
  }
  rec <- tryCatch(httr2::resp_body_json(resp)$result, error = \(e) NULL)
  if (is.null(rec)) { obj$error <- "parse_error"; return(obj) }
  obj$title <- rec$title %empty_or% NA_character_
  obj$doi <- rec$doi %empty_or% NA_character_
  obj$license <- rec$license_title %empty_or% rec$license_id %empty_or% NA_character_
  obj$authors <- list(stats::na.omit(c(rec$author %empty_or% NA_character_, rec$maintainer %empty_or% NA_character_)))
  res <- rec$resources %||% list()
  # the dataset's own files: uploaded, or at an address on the portal itself
  own <- vapply(res, \(r) identical(r$url_type, "upload") ||
                  (!is.null(r$url) && nzchar(r$url) && .url_host_is(r$url, host)), NA)
  res <- res[own]
  if (length(res)) {
    file_url <- vapply(res, \(r) as.character(r$url %empty_or% NA_character_), character(1))
    from_url <- utils::URLdecode(sub("[?#].*$", "", basename(file_url)))
    fmt <- vapply(res, \(r) tolower(as.character(r$format %empty_or% "")), character(1))
    nm <- vapply(res, \(r) as.character(r$name %empty_or% NA_character_), character(1))
    # the file's own name where its address ends in one, else the resource's
    # name with its format as the extension
    file_name <- ifelse(grepl("\\.[A-Za-z0-9]{1,8}$", from_url), from_url,
                        ifelse(nzchar(fmt) & !is.na(nm), paste0(nm, ".", fmt), nm))
    obj$files <- list(data.frame(
      file_name = file_name, file_path = file_name, file_url = file_url,
      file_size = vapply(res, \(r) suppressWarnings(as.numeric(r$size %empty_or% NA_real_)), numeric(1)),
      format = fmt))
  }
  obj
}
