# DataONE (https://www.dataone.org) is a FEDERATION of independently-run
# repositories ("member nodes"), not one hosted service -- unlike Zenodo or
# Mendeley Data, and closer in spirit to Dataverse's many installations
# (archive-dataverse.R) or DSpace's (archive-psycharchives.R,
# archive-dspace7.R). Each member node runs the same underlying software
# (Metacat) and exposes the same DataONE REST API shape, but at a DIFFERENT
# base path per installation -- confirmed live 2026-09-12 by comparing two
# real member nodes: Arctic Data Center answers at
# https://arcticdata.io/metacat/d1/mn/v2/, while KNB answers the identical
# request shape at https://knb.ecoinformatics.org/knb/d1/mn/v2/ (its own
# "/metacat/..." path returns the site's ordinary web app instead of API XML).
# So each host below is only added once verified live, the same discipline
# archive-dataverse.R's and archive-figshare.R's host lists use.
#
# Hosts found via re3data.org (https://www.re3data.org, searched for
# "DataONE" 2026-09-12) and each confirmed live the same way: a plain GET to
# "<api_base>node" returns DataONE's own XML node-capabilities document
# rather than an HTML page.
.dataone_hosts <- function() {
  list(
    list(host = "arcticdata.io",           api_base = "/metacat/d1/mn/v2/", doi_prefix = "10.18739"),
    list(host = "knb.ecoinformatics.org",  api_base = "/knb/d1/mn/v2/",     doi_prefix = "10.5063"),
    list(host = "metacat.tfri.gov.tw",     api_base = "/metacat/d1/mn/v2/", doi_prefix = NA_character_),
    list(host = "smithsonian.dataone.org", api_base = "/metacat/d1/mn/v2/", doi_prefix = NA_character_)
  )
}

# Regex fragment matching any known member node host (for URL detection) or
# the site-wide DataONE search portal, which links out to individual member
# node landing pages using the same "view/doi:..." path shape.
.dataone_host_regex <- function() {
  hosts <- vapply(.dataone_hosts(), function(h) h$host, character(1))
  paste(gsub("\\.", "\\\\.", hosts), collapse = "|")
}

#' Find DataONE Links in Papers
#'
#' Get all DataONE links: real hyperlinks from the paper's own `url` table,
#' plus a body-text fallback for a BARE DOI mention (routinely cited without
#' any URL scheme at all) using the DOI prefixes of known hosts (see
#' .dataone_hosts()) -- the `url` table only ever contains links the source
#' document itself made clickable. Same two-tier approach `zenodo_links()`
#' uses.
#'
#' Unlike the single-host archive modules, a DataONE citation only resolves
#' to a specific member node's own API (see the file-level note in
#' archive-dataone.R) -- a host neither listed by domain nor identifiable by
#' its DOI prefix is not one this package can currently query, even if it is
#' a real DataONE member node.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the DataONE url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' dataone_links(psychsci)
#' }
dataone_links <- function(paper) {
  href <- text <- NULL

  host_regex <- .dataone_host_regex()
  doi_prefixes <- vapply(.dataone_hosts(), function(h) h$doi_prefix, character(1))
  doi_prefixes <- stats::na.omit(doi_prefixes)
  doi_regex <- paste(gsub("\\.", "\\\\.", doi_prefixes), collapse = "|")

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(paste0(host_regex, "|", doi_regex), href, ignore.case = TRUE))

  dataone_bare_regex <- paste0(
    "(?:https?://)?(?:", host_regex, ")/(?:view|catalog/view)/doi:[^\\s\"'<>)]+",
    "|(?:https?://)?(?:doi\\.org/)?(?:", doi_regex, ")/[A-Za-z0-9._/-]+"
  )
  other_dataone <- text_search(paper, dataone_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_dataone) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$dataone_url <- links$href
  links$dataone_host <- .dataone_host(links$dataone_url)
  links$dataone_pid <- .dataone_pid(links$dataone_url)

  return(links)
}

#' Identify which known DataONE member node a URL or DOI belongs to
#'
#' @param dataone_url a vector of URLs or DOIs
#'
#' @returns a character vector of hosts (see .dataone_hosts()), or `NA` for
#'   one that matches no known host
#' @keywords internal
.dataone_host <- function(dataone_url) {
  if (length(dataone_url) == 0) return(character(0))
  if (length(dataone_url) > 1) return(vapply(dataone_url, .dataone_host, character(1)))

  dataone_url <- trimws(as.character(dataone_url))
  if (is.na(dataone_url) || !nzchar(dataone_url)) return(NA_character_)

  for (h in .dataone_hosts()) {
    if (grepl(h$host, dataone_url, fixed = TRUE)) return(h$host)
    if (!is.na(h$doi_prefix) &&
        grepl(paste0(gsub("\\.", "\\\\.", h$doi_prefix), "/"), dataone_url, perl = TRUE)) {
      return(h$host)
    }
  }
  NA_character_
}

#' Get a DataONE persistent identifier (PID) from a URL or DOI
#'
#' @param dataone_url a vector of URLs or DOIs to DataONE-hosted objects
#'
#' @returns a character vector of PIDs, normalised to DataONE's own
#'   `doi:<prefix>/<suffix>` form
#' @keywords internal
.dataone_pid <- function(dataone_url) {
  if (length(dataone_url) == 0) return(character(0))
  if (length(dataone_url) > 1) return(vapply(dataone_url, .dataone_pid, character(1)))

  dataone_url <- trimws(as.character(dataone_url))
  if (is.na(dataone_url) || !nzchar(dataone_url)) return(NA_character_)

  # A member node landing page URL already carries the PID verbatim in its
  # own "doi:..." form (e.g. ".../view/doi:10.18739/A2GT5FG86") -- confirmed
  # live 2026-09-12 by resolving a real citation's DOI and following the
  # redirect.
  match <- regexec("doi:(10\\.[0-9]+/[A-Za-z0-9._-]+)", dataone_url, perl = TRUE, ignore.case = TRUE)
  groups <- regmatches(dataone_url, match)[[1]]
  if (length(groups) >= 2) return(paste0("doi:", groups[[2]]))

  # A bare DOI (with or without a doi.org/https:// prefix) has no "doi:"
  # marker of its own -- add one, since that is the literal PID string
  # DataONE's own API expects.
  match <- regexec("(?:doi\\.org/)?(10\\.[0-9]+/[A-Za-z0-9._-]+)$", dataone_url, perl = TRUE, ignore.case = TRUE)
  groups <- regmatches(dataone_url, match)[[1]]
  if (length(groups) >= 2) return(paste0("doi:", groups[[2]]))

  NA_character_
}

#' Retrieve info from DataONE by URL
#'
#' @param dataone_url a DataONE URL or DOI, or a table containing them (e.g.,
#'   as created by [dataone_links()])
#' @param id_col the index or name of the column that contains DataONE URLs,
#'   if `dataone_url` is a table
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a previously cached listing for a dataset
#'   already looked up (see [repo_info_cache()]) instead of re-querying the
#'   host's API. Off by default.
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   dataone_info("https://doi.org/10.18739/A2GT5FG86")
#' }
dataone_info <- function(dataone_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "DataONE Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(dataone_url)) {
    table <- dataone_url
    table$dataone_url <- table[[id_col]]
  } else {
    raw_urls <- unique(dataone_url) |> stats::na.omit()
    table <- data.frame(dataone_url = raw_urls)
  }

  ids <- data.frame(
    dataone_url = table$dataone_url,
    dataone_host = .dataone_host(table$dataone_url),
    dataone_pid = .dataone_pid(table$dataone_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$dataone_url), , drop = FALSE]
  valid <- ids[!is.na(ids$dataone_host) & !is.na(ids$dataone_pid), , drop = FALSE] |> unique()

  if (nrow(valid) == 0) {
    ("No valid DataONE links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "dataone_url"))
  }

  # A host that could not be resolved (a real DataONE citation this package
  # does not have a verified host entry for -- see the file-level note in
  # archive-dataone.R) gets no online() pre-flight of its own since there is
  # no host to check; it simply never produces a row here.
  if (!online("dataone.org")) {
    stop("dataone.org seems to be offline")
  }

  paste0(
    "Starting DataONE retrieval for ",
    nrow(valid), " dataset",
    ifelse(nrow(valid) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", nrow(valid))
  for (i in seq_len(nrow(valid))) {
    host <- valid$dataone_host[[i]]
    pid <- valid$dataone_pid[[i]]
    ckey <- paste(host, pid)
    cached <- if (isTRUE(cache)) .repo_info_cache_get("dataone", ckey) else NULL
    if (!is.null(cached)) {
      id_info[[i]] <- cached
    } else {
      id_info[[i]] <- .dataone_info(pid, host = host, pb = pb)
      if (isTRUE(cache) && .repo_info_ok(id_info[[i]]))
        .repo_info_cache_put("dataone", ckey, id_info[[i]])
    }
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "dataone_url") |>
    dplyr::left_join(info, by = c("dataone_host", "dataone_pid"), suffix = c("", ".dataone"))

  paste0("...DataONE retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one DataONE object
#'
#' @param pid a DataONE persistent identifier (e.g. `"doi:10.18739/..."`)
#' @param host the member node host to query (see .dataone_hosts())
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @keywords internal
.dataone_info <- function(pid, host, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from DataONE ", host, " (", pid, ")...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(dataone_host = host, dataone_pid = pid)

  api_base <- NULL
  for (h in .dataone_hosts()) if (identical(h$host, host)) api_base <- h$api_base
  if (is.null(api_base)) {
    obj$error <- "unknown_host"
    return(obj)
  }

  epid <- utils::URLencode(pid, reserved = TRUE)
  obj_url <- paste0("https://", host, api_base, "object/", epid)

  resp <- .batch_query(obj_url, msg = NULL)[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(pid, " could not be found on ", host, call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # The object at a dataset's own PID is its science metadata document --
  # almost always EML (Ecological Metadata Language), the format every
  # verified host in .dataone_hosts() uses -- describing the dataset and
  # listing each of its data files as a separate <physical> element. A
  # non-EML metadata format (rare; DataONE also allows e.g. ISO 19115) is
  # reported as an error rather than guessed at, since this package has no
  # parser for it.
  body <- tryCatch(httr2::resp_body_string(resp), error = \(e) NULL)
  doc <- tryCatch(xml2::read_xml(body %||% ""), error = \(e) NULL)
  if (is.null(doc) || xml2::xml_name(xml2::xml_root(doc)) != "eml") {
    obj$error <- "unsupported_metadata_format"
    return(obj)
  }

  title <- xml2::xml_text(xml2::xml_find_first(doc, ".//*[local-name()='title']"))
  creators <- xml2::xml_find_all(doc, ".//*[local-name()='creator']")
  # %empty_or% (not %||%) because a name part can come back as a length-zero
  # value rather than NULL; vapply(..., character(1)) requires exactly
  # length 1 from every call.
  authors <- vapply(creators, function(cr) {
    given <- xml2::xml_text(xml2::xml_find_first(cr, ".//*[local-name()='givenName']")) %empty_or% ""
    surn  <- xml2::xml_text(xml2::xml_find_first(cr, ".//*[local-name()='surName']")) %empty_or% ""
    org   <- xml2::xml_text(xml2::xml_find_first(cr, ".//*[local-name()='organizationName']")) %empty_or% ""
    full <- trimws(paste(given, surn))
    if (nzchar(full)) full else if (nzchar(org)) org else NA_character_
  }, character(1))

  pub_date <- xml2::xml_text(xml2::xml_find_first(doc, ".//*[local-name()='pubDate']"))
  license  <- xml2::xml_text(xml2::xml_find_first(doc, ".//*[local-name()='intellectualRights']"))

  obj$title <-            title %empty_or% NA_character_
  obj$doi <-              pid
  obj$publication_date <- pub_date %empty_or% NA_character_
  obj$authors <-          list(authors)
  # intellectualRights is often a longer free-text statement rather than a
  # short licence name (e.g. "This work is dedicated to the public domain
  # under CC0 1.0...") -- kept as-is rather than trimmed, since there is no
  # reliable way to extract a short name from arbitrary free text the way
  # metadata$license$id does for Zenodo's structured field.
  obj$license <-          license %empty_or% NA_character_

  # Each <physical> element describes one data file (objectName, size, and a
  # url naming its own PID, extracted below rather than used directly --
  # verified live 2026-09-12: it points at DataONE's central coordinating
  # node, cn.dataone.org/cn/v2/resolve/<pid>, which is not this file's own
  # member node and need not be reachable for the file to be -- see the
  # file-level note in archive-dataone.R). Every verified host serves its own
  # objects at the same "<api_base>object/<pid>" path the metadata document
  # itself just came from, so that is used for file_url instead.
  physicals <- xml2::xml_find_all(doc, ".//*[local-name()='physical']")
  files <- lapply(physicals, function(p) {
    name <- xml2::xml_text(xml2::xml_find_first(p, ".//*[local-name()='objectName']"))
    size <- xml2::xml_text(xml2::xml_find_first(p, ".//*[local-name()='size']"))
    url  <- xml2::xml_text(xml2::xml_find_first(p, ".//*[local-name()='url']"))
    file_pid <- sub("^.*/", "", url %empty_or% "")
    list(
      key  = name %empty_or% NA_character_,
      size = suppressWarnings(as.numeric(size %empty_or% NA_real_)),
      pid  = if (nzchar(file_pid)) file_pid else NA_character_
    )
  })
  obj$files <- list(files)

  return(obj)
}
