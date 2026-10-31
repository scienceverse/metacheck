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
#
# data.piscoweb.org (PISCO, Partnership for Interdisciplinary Studies of
# Coastal Oceans, UC Santa Barbara) added 2026-09-19: found via a real
# paper citing a bare 10.6085/AA/... DOI with no host domain in the URL at
# all ("uploaded to DataONE and are accessible here: https://doi.org/
# 10.6085/AA/marine_ltm.20.1") -- resolving that DOI live redirects to
# data.piscoweb.org/metacatui/..., confirmed to run the same Metacat API
# at /metacat/d1/mn/v2/node (real DataONE XML node-capabilities response),
# and GET .../meta/doi:10.6085%2FAA%2Fmarine_ltm.20.1 returns the real
# systemMetadata record for that exact dataset. 10.6085 registered to this
# host specifically (not DataONE generally) via DataCite's own record for
# that DOI.
.dataone_hosts <- function() {
  list(
    list(host = "arcticdata.io",           api_base = "/metacat/d1/mn/v2/", doi_prefix = "10.18739"),
    list(host = "knb.ecoinformatics.org",  api_base = "/knb/d1/mn/v2/",     doi_prefix = "10.5063"),
    list(host = "metacat.tfri.gov.tw",     api_base = "/metacat/d1/mn/v2/", doi_prefix = NA_character_),
    list(host = "smithsonian.dataone.org", api_base = "/metacat/d1/mn/v2/", doi_prefix = NA_character_),
    list(host = "data.piscoweb.org",       api_base = "/metacat/d1/mn/v2/", doi_prefix = "10.6085")

    # NOT added here (issue #461): the Environmental Data Initiative (EDI,
    # portal.edirepository.org, API host pasta.lternet.edu, DOI prefix
    # 10.6073) cites 5 real papers but was left out rather than listed with
    # a guessed api_base. EDI runs PASTA, not Metacat -- confirmed live
    # 2026-10-05 that it does NOT expose the Metacat "<api_base>object/<pid>"
    # shape every host above relies on (pasta.lternet.edu/d1/mn/v2/node and
    # portal.edirepository.org/nis/d1/mn/v2/node both 404). PASTA's own API
    # is real and live (pasta.lternet.edu/package/docs/api -> 200) but uses a
    # different path shape ("/package/<type>/<scope>/<identifier>/<revision>",
    # confirmed via a real 403 permissions-error body naming that service
    # method, not a 404) that .dataone_object_size()/.dataone_info() do not
    # know how to build a URL for -- adding the host without also adding that
    # URL-construction logic would register the host as "known" while every
    # lookup against it silently fails. This is a code change, not a table
    # entry; re-check before adding.
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

  # KNB's own real landing pages use a "#view/doi:..." fragment, not a
  # "/view/doi:..." path segment -- confirmed live 2026-09-29 (issue #435):
  # "https://knb.ecoinformatics.org/#view/doi:10.5063/..." is a real citation
  # shape this pattern's "/(?:view|catalog/view)/doi:" branch never actually
  # matched (the "#" breaks it). A host without its own registered doi_prefix
  # (metacat.tfri.gov.tw, smithsonian.dataone.org -- see .dataone_hosts())
  # has no fallback via the doi_regex branch either, so this host-branch
  # match is the only path to detecting a citation on those hosts at all.
  #
  # KNB also predates DataONE's DOI minting: many older datasets are cited
  # only by their legacy Metacat docid ("knb.1404.1" -- scope.identifier.
  # revision), with no DOI or URL at all in the citing text (confirmed live
  # 2026-09-29, issue #435: a paper citing only "http://knb.ecoinformatics.org"
  # and, separately, "identifier knb.1404.1"). Confirmed live that this
  # docid, used AS the PID with no "doi:" wrapper, resolves directly against
  # KNB's own API (knb/d1/mn/v2/object/knb.1404.1 -> HTTP 200, real EML).
  # Matched narrowly on the literal "knb." prefix (not a generic
  # word.digits.digit pattern) to avoid matching unrelated numbers in prose.
  knb_docid_regex <- "\\bknb\\.[0-9]+\\.[0-9]+\\b"

  dataone_bare_regex <- paste0(
    "(?:https?://)?(?:", host_regex, ")/(?:#?view|catalog/view)/doi:[^\\s\"'<>)]+",
    "|(?:https?://)?(?:doi\\.org/)?(?:", doi_regex, ")/[A-Za-z0-9._/-]+",
    "|", knb_docid_regex
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
  if (length(dataone_url) > 1) return(vapply(dataone_url, .dataone_host, character(1), USE.NAMES = FALSE))

  dataone_url <- trimws(as.character(dataone_url))
  if (is.na(dataone_url) || !nzchar(dataone_url)) return(NA_character_)

  if (grepl("^knb\\.[0-9]+\\.[0-9]+$", dataone_url, perl = TRUE)) {
    return("knb.ecoinformatics.org")
  }

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
  if (length(dataone_url) > 1) return(vapply(dataone_url, .dataone_pid, character(1), USE.NAMES = FALSE))

  dataone_url <- trimws(as.character(dataone_url))
  if (is.na(dataone_url) || !nzchar(dataone_url)) return(NA_character_)

  if (grepl("^knb\\.[0-9]+\\.[0-9]+$", dataone_url, perl = TRUE)) {
    return(dataone_url)
  }

  # A member node landing page URL already carries the PID verbatim in its
  # own "doi:..." form (e.g. ".../view/doi:10.18739/A2GT5FG86") -- confirmed
  # live 2026-09-12 by resolving a real citation's DOI and following the
  # redirect.
  # A marked PID may contain "/", as a bare one may (below); a final "." is
  # the sentence's full stop, not part of the PID (U40).
  match <- regexec("doi:(10\\.[0-9]+/[A-Za-z0-9._/-]+)", dataone_url, perl = TRUE, ignore.case = TRUE)
  groups <- regmatches(dataone_url, match)[[1]]
  if (length(groups) >= 2) return(paste0("doi:", sub("[./]+$", "", groups[[2]])))

  # A bare DOI (with or without a doi.org/https:// prefix) has no "doi:"
  # marker of its own -- add one, since that is the literal PID string
  # DataONE's own API expects. The suffix itself is allowed to contain a
  # further slash (e.g. PISCO's 10.6085/AA/marine_ltm.20.1, confirmed live
  # to be a real, resolving DOI -- DataONE PIDs are not required to be
  # slash-free after the prefix the way most other repositories' DOIs are)
  # -- [A-Za-z0-9._-]+ alone cannot match across a "/", which silently
  # dropped this whole class of PID before this fix.
  match <- regexec("(?:doi\\.org/)?(10\\.[0-9]+/[A-Za-z0-9._/-]+)$", dataone_url, perl = TRUE, ignore.case = TRUE)
  groups <- regmatches(dataone_url, match)[[1]]
  if (length(groups) >= 2) return(paste0("doi:", sub("[./]+$", "", groups[[2]])))

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
#'   already looked up (see [repo_info_cache_clear()]) instead of re-querying the
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
    # A caller passing dataone_links()'s own output back in (the documented,
    # normal usage) already has dataone_host/dataone_pid columns of its
    # own; ids below recomputes both independently, and left joining ids
    # onto a table that already has those names produces .x/.y-suffixed
    # duplicates instead of plain columns -- silently breaking the SECOND
    # join further down (by = c("dataone_host", "dataone_pid")), which
    # then errors with "must be present in the data" only when a dataset
    # isn't found (confirmed live 2026-09-19). Dropped here so ids's own
    # recomputed values are always what flows through, never stale/
    # duplicated ones. Same fix applied identically across every
    # archive-*.R file sharing this table/ids/left_join shape (dataverse,
    # dryad, figshare, mendeley, reshare, zenodo).
    table$dataone_host <- NULL
    table$dataone_pid <- NULL
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
  # one fetch per dataset: unique on the URL too, two URLs for one dataset
  # fetched it twice, and the join below then gave each URL two rows (U35)
  valid <- valid[!duplicated(valid[, c("dataone_host", "dataone_pid")]), , drop = FALSE]

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

# One file's byte size via a HEAD request against its own object/<pid>
# endpoint, read from the Content-Length header -- used only for the
# no-<physical> case in .dataone_info() below, where size is not present
# anywhere in the metadata document itself, and since 2026-10-13 only for a
# file that .dataone_package_sizes()'s single Solr query did not cover (that
# query normally answers for every file at once). Confirmed live 2026-09-29 (issue
# #435 follow-up): a HEAD to object/<pid> returns Content-Length without
# downloading the file. Returns NA_real_ on any failure (offline host,
# non-200, missing header) rather than erroring, matching this file's other
# defensive-read helpers.
.dataone_object_size <- function(host, api_base, pid) {
  url <- paste0("https://", host, api_base, "object/", utils::URLencode(pid, reserved = TRUE))
  tryCatch({
    resp <- .mc_request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) return(NA_real_)
    len <- httr2::resp_header(resp, "Content-Length")
    suppressWarnings(as.numeric(len %empty_or% NA_real_))
  }, error = \(e) NA_real_)
}

# Byte sizes of every object a metadata document documents, from ONE query to
# the member node's Solr index:
#   <api_base>query/solr/?q=isDocumentedBy:"<pid>"&fl=id,fileName,size&rows=1000&wt=json
# Replaces one .dataone_object_size() HEAD request per file for EML entities
# that carry no <physical> element. Verified live 2026-10-13 on KNB
# doi:10.5063/F11V5CFN (3 objects returned in one reply -- the EML document
# itself plus its two urn:uuid data files, Seed_tracking_data.csv 213713 and
# Trapping_data.csv 136236 bytes; the first matches the Content-Length the
# old HEAD request reports) and on Arctic Data Center doi:10.18739/A2GT5FG86,
# where Roots_4_15_21.csv is 3656 bytes in both the Solr reply and the EML's
# own <size>. The metadata document's own row is returned too and
# simply goes unused. rows = 1000 is far above any package seen so far; an
# object beyond it is simply absent and gets the HEAD fallback.
#
# Returns a numeric vector named by object id, with each object's file name in
# the attribute "file_names", or numeric(0) when the node
# has no /query/solr/ endpoint or the query fails for any reason (on
# 2026-10-13 smithsonian.dataone.org and data.piscoweb.org refused the
# connection outright, and metacat.tfri.gov.tw took ~7 s to answer) -- the
# caller then falls back to the per-file HEAD request. No retries and a
# bounded timeout, so an unreachable node costs one failed attempt, not a
# backoff sequence.
.dataone_package_sizes <- function(host, api_base, pid) {
  q <- paste0('isDocumentedBy:"', gsub('"', '\\\\"', pid), '"')
  url <- paste0("https://", host, api_base, "query/solr/?q=",
                utils::URLencode(q, reserved = TRUE),
                "&fl=id,fileName,size&rows=1000&wt=json")
  rec <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(30) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    # KNB labels this reply text/json, which resp_body_json() refuses by default
    # (checked live 2026-10-31); without check_type every query counted as failed.
    if (httr2::resp_status(resp) != 200) NULL else httr2::resp_body_json(resp, check_type = FALSE)
  }, error = \(e) NULL)

  docs <- rec$response$docs
  if (!is.list(docs) || length(docs) == 0) return(numeric(0))

  ids <- vapply(docs, \(d) as.character(d$id %empty_or% NA_character_), character(1))
  sizes <- vapply(docs, \(d) suppressWarnings(as.numeric(d$size %empty_or% NA_real_)),
                  numeric(1))
  file_names <- vapply(docs, \(d) as.character(d$fileName %empty_or% NA_character_), character(1))
  keep <- !is.na(ids)
  out <- stats::setNames(sizes[keep], ids[keep])
  attr(out, "file_names") <- file_names[keep]
  out
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

  # DataONE matches identifiers as exact strings, but a DOI is the same DOI in
  # any case, and DataCite registers DOIs in upper case, which is how KNB
  # stores them (verified 2026-10-17: doi:10.5063/f11z42jx and
  # doi:10.5063/F11z42jx answer 404, doi:10.5063/F11Z42JX answers 200; #476).
  # So a DOI written in lower or mixed case is asked for once more in upper
  # case. Only after a 404, so an identifier found as written is never
  # changed; only for a "doi:" identifier, since a urn:uuid or knb.* docid is
  # not case-insensitive; and the DOI prefix is digits, so only the suffix
  # changes. The upper-case form cannot be a different dataset: DOI names
  # differ only by more than case. `obj$dataone_pid` keeps the identifier as
  # given, which dataone_info() joins on; `pid` becomes the one found, for the
  # requests below.
  if (!is.null(resp) && httr2::resp_status(resp) == 404 &&
      startsWith(pid, "doi:")) {
    upper <- paste0("doi:", toupper(substring(pid, 5)))
    if (!identical(upper, pid)) {
      resp_upper <- .batch_query(
        paste0("https://", host, api_base, "object/",
               utils::URLencode(upper, reserved = TRUE)), msg = NULL)[[1]]
      if (!is.null(resp_upper) && httr2::resp_status(resp_upper) == 200) {
        resp <- resp_upper
        pid <- upper
        epid <- utils::URLencode(pid, reserved = TRUE)
        obj_url <- paste0("https://", host, api_base, "object/", epid)
      }
    }
  }

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
  # a missing name part is NA, which %empty_or% does not replace, so a
  # surname-only creator was "NA Smith" (U40)
  name_part <- function(cr, part) {
    v <- xml2::xml_text(xml2::xml_find_first(cr, sprintf(".//*[local-name()='%s']", part)))
    if (length(v) == 0 || is.na(v)) "" else trimws(v)
  }
  authors <- vapply(creators, function(cr) {
    given <- name_part(cr, "givenName")
    surn  <- name_part(cr, "surName")
    org   <- name_part(cr, "organizationName")
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
  entities <- xml2::xml_find_all(
    doc,
    ".//*[local-name()='dataTable' or local-name()='otherEntity' or
          local-name()='spatialVector' or local-name()='spatialRaster']"
  )
  files <- lapply(entities, function(e) {
    p <- xml2::xml_find_first(e, ".//*[local-name()='physical']")
    if (!is.na(p)) {
      name <- xml2::xml_text(xml2::xml_find_first(p, ".//*[local-name()='objectName']"))
      size <- xml2::xml_text(xml2::xml_find_first(p, ".//*[local-name()='size']"))
      url  <- xml2::xml_text(xml2::xml_find_first(p, ".//*[local-name()='url']"))
      # A PID can hold slashes, so in a .../resolve/<pid> URL all of what follows
      # counts: verified live 2026-10-31 on data.piscoweb.org, where
      # doi:10.6085/AA/marine_ltm.21.1 is found and marine_ltm.21.1 gives 404.
      url <- url %empty_or% ""
      file_pid <- if (grepl("/resolve/", url, fixed = TRUE)) sub("^.*/resolve/", "", url) else sub("^.*/", "", url)
      return(list(
        key  = name %empty_or% NA_character_,
        size = suppressWarnings(as.numeric(size %empty_or% NA_real_)),
        pid  = if (nzchar(file_pid)) file_pid else NA_character_
      ))
    }

    # No <physical> child at all -- confirmed live 2026-09-29 (issue #435
    # follow-up) against knb.ecoinformatics.org: some KNB records (e.g.
    # doi:10.5063/F11V5CFN) list an <otherEntity> as a bare name/type pair
    # with no <physical>, so no size/url/checksum is available from the
    # metadata document itself. Its own "id" attribute is usable as a PID
    # ONLY when shaped like "urn-uuid-<uuid>" (-> "urn:uuid:<uuid>", confirmed
    # live to resolve directly against the SAME host's own object/<pid>
    # endpoint used above). A document-scoped id in other shapes (e.g. a
    # SHA1-looking id on records that DO have <physical>, seen on the same
    # host) is NOT a resolvable object PID -- confirmed live to 404 -- so it
    # is left as no file_url rather than guessed at.
    name <- xml2::xml_text(xml2::xml_find_first(e, ".//*[local-name()='entityName']"))
    raw_id <- xml2::xml_attr(e, "id") %empty_or% NA_character_
    pid <- if (!is.na(raw_id) && grepl("^urn-uuid-", raw_id, perl = TRUE)) {
      sub("^urn-uuid-", "urn:uuid:", raw_id)
    } else {
      NA_character_
    }
    # Size is not in the metadata document at all for this shape, unlike the
    # <physical> branch above -- filled in below, after every entity has been
    # read, so all missing sizes can be asked for in one request.
    list(
      key  = name %empty_or% NA_character_,
      size = NA_real_,
      pid  = pid
    )
  })

  # Sizes for the no-<physical> entities: one Solr query on the member node
  # for every object this metadata document documents (see
  # .dataone_package_sizes()), instead of one HEAD request per file. A file
  # the query did not answer for -- node without /query/solr/, failed query,
  # or a pid missing from the result -- still gets the per-file HEAD
  # (.dataone_object_size()), so no size that was found before is lost.
  # Only the no-<physical> entities: a <physical> entity whose own <size> is
  # blank was never looked up before either, and its pid comes from a URL
  # (still percent-encoded) rather than from a node-local id.
  no_physical <- vapply(entities, \(e) {
    is.na(xml2::xml_find_first(e, ".//*[local-name()='physical']"))
  }, logical(1))
  no_id <- vapply(files, \(f) is.na(f$pid), logical(1))
  need <- which(no_physical | no_id)
  if (length(need) > 0) {
    sizes <- .dataone_package_sizes(host, api_base, pid)
    # An entity without an object PID can still be found in this reply by its
    # file name, without any folder its name gives. Verified live 2026-10-31 on
    # KNB: doi:10.5063/F1VD6WMF has a <physical> with no url, named "Current
    # Results - 0.7.7/trait_data.csv", which is the object trait_data.csv, and
    # doi:10.5063/F1MC8X92 a bare <otherEntity> with no id. Only when exactly one
    # object has that name.
    file_names <- attr(sizes, "file_names")
    for (j in which(no_id)) {
      hit <- which(file_names == basename(files[[j]]$key))
      if (length(hit) == 1) files[[j]]$pid <- names(sizes)[hit]
    }
    for (j in need) {
      if (is.na(files[[j]]$pid) || !is.na(files[[j]]$size)) next
      s <- unname(sizes[files[[j]]$pid])
      if (length(s) != 1 || is.na(s)) {
        s <- .dataone_object_size(host, api_base, files[[j]]$pid)
      }
      files[[j]]$size <- s
    }
  }
  obj$files <- list(files)

  return(obj)
}
