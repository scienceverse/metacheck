# DSpace 7 replaced the legacy /rest/ REST API used by
# archive-psycharchives.R's .dspace_legacy_*() functions with a differently
# shaped HAL-JSON API under /server/api/ -- an installation running DSpace 7
# or later does not answer /rest/ at all, so it needs its own implementation
# rather than a `host` parameter on the legacy one.
#
# Verified live 2026-09-11 against repository.gatech.edu (Georgia Tech Digital
# Repository, item uuid bac086e5-c606-474b-af1e-4a6122694af5, handle
# 1853/67239):
#   - GET /server/api/pid/find?id=<handle>            resolves a handle to an
#     item (id/uuid/name/handle/metadata) the same way /rest/handle/<handle>
#     did for legacy DSpace.
#   - GET /server/api/core/items/<uuid>                returns the same item
#     directly when the uuid is already known (e.g. from a citation of the
#     item's own page rather than its handle).
#   - GET /server/api/core/items/<uuid>/bundles         lists the item's
#     bundles (ORIGINAL, LICENSE, THUMBNAIL, SWORD, ...) -- only the ORIGINAL
#     bundle holds the actual deposited files; the others are licence/derived
#     artefacts and are not listed as files here, mirroring how
#     .dspace_legacy_info() only ever sees public bitstreams.
#   - GET /server/api/core/bundles/<bundle-uuid>/bitstreams  lists that
#     bundle's files: name, sizeBytes, checkSum.value (MD5), and
#     _links.content.href as the direct download URL.
# Metadata field names (dc.title, dc.contributor.author, dc.identifier.doi,
# dc.rights, dc.date.issued/available) are read defensively (NA when absent)
# since DSpace metadata schemas are configured per installation and not every
# field is guaranteed to exist for every item.
#
# Access control: as with legacy DSpace, an anonymous request only ever lists
# publicly retrievable bitstreams -- this has not been separately verified
# live against a genuinely restricted item on DSpace 7 (no such item was
# available to test against), so it is carried over as the same assumption
# legacy DSpace's own documentation confirms, not an independently verified
# fact for this API version.

# Known DSpace 7+ installations, found via DataCite's public client registry
# (api.datacite.org/clients?software=dspace, 169 results) and individually
# confirmed live 2026-09-11 by requesting GET /server/api and checking for a
# HAL "_links" root document. Hosts that answered the legacy /rest/ API
# instead are in .dspace_legacy_hosts() (archive-psycharchives.R); hosts that
# answered neither were left out (may be a different repository platform, an
# installation mounted under a non-root path, or simply unreachable when
# checked -- re-check before adding).
.dspace7_hosts <- function() {
  c(
    "archive.uax.com",                          # UAX Archive
    "aura.abdn.ac.uk",                          # University of Aberdeen (UK)
    "bedl.asabe.org",                           # Biosystems Engineering Digital Library
    "comum.rcaap.pt",                           # IPS - Instituto Politecnico de Setubal (Portugal)
    "conservancy.umn.edu",                      # Data Repository for the University of Minnesota (USA)
    "cris.usm.cl",                              # CRIS, Universidad de Santa Maria (Chile)
    "daks.uni-kassel.de",                       # DaKS, University of Kassel (Germany)
    "datakatalogi.helsinki.fi",                 # University of Helsinki Data Catalogue (Finland)
    "deposita.ibict.br",                        # Repositório Comum do Brasil (Deposita)
    "digitalcollection.zhaw.ch",                # ZHAW Digital Collection (Switzerland)
    "diglib.eg.org",                            # Eurographics Digital Library
    "dl.gi.de",                                 # Gesellschaft für Informatik (Germany)
    "dspace.lib.cranfield.ac.uk",               # Cranfield Online Research Data (UK)
    "dspace.ut.ee",                             # University of Tartu Library (Estonia)
    "ecosistema.buap.mx",                       # EcoBUAP (Mexico)
    "hephaestus.nup.ac.cy",                     # HEPHAESTUS, Neapolis University Paphos (Cyprus)
    "irep.mbzuai.ac.ae",                        # MBZUAI iRep (UAE)
    "irf.fhnw.ch",                              # Institutional Repository FHNW (Switzerland)
    "ktisis.cut.ac.cy",                         # KTISIS, Cyprus University of Technology
    "mro.massey.ac.nz",                         # Massey Research Online (New Zealand)
    "nalt-dspace.progress.plus",                # NALT
    "opara.zih.tu-dresden.de",                  # OPARA (Germany)
    "open.fau.de",                              # FAU Erlangen-Nürnberg open access repository (Germany)
    "open.ifz-muenchen.de",                     # IfZ-Repositorium (Germany)
    "open.uni-marburg.de",                      # open_UMR, University of Marburg (Germany)
    "openresearch.ceu.edu",                     # Central European University Open Research Repository
    "openscience.ub.uni-mainz.de",              # Johannes Gutenberg University Mainz (Germany)
    "proforis.phsg.ch",                         # Proforis, University of Teacher Education St.Gallen (Switzerland)
    "publish.fid-media.de",                     # FID Media Publish (Germany)
    "recil.ensinolusofona.pt",                  # Repositório Científico Lusófona (Portugal)
    "repositorio.ipen.br",                      # Repositório Digital IPEN (Brazil)
    "repositorio.ipsantarem.pt",                # Instituto Politécnico de Santarém (Portugal)
    "repositorio.tec.mx",                       # RITEC, Tecnológico de Monterrey (Mexico)
    "repositorio.uac.pt",                       # Universidade dos Açores (Portugal)
    "repositorio.uautonoma.cl",                 # Universidad Autónoma de Chile
    "repositorio.ucentral.cl",                  # Universidad Central de Chile
    "repositorio.ucn.cl",                       # Universidad Católica del Norte (Chile)
    "repositorio.ucsm.edu.pe",                  # Universidad Católica de Santa María (Peru)
    "repositorio.umecit.edu.pa",                # UMECIT (Panama)
    "repository.aus.edu",                       # American University of Sharjah (UAE)
    "repository.difu.de",                       # Difu-Repository (Germany)
    "repository.escholarship.umassmed.edu",     # UMass Chan Medical School (USA)
    "repository.gatech.edu",                    # Georgia Tech Digital Repository (USA)
    "repository.gchumanrights.org",             # Global Campus of Human Rights
    "repository.mines.edu",                     # Colorado School of Mines (USA)
    "repository.mu.edu.et",                     # Mekelle University (Ethiopia)
    "repository.universidadean.edu.co",         # Biblioteca Digital Minerva, Universidad EAN (Colombia)
    "repository.upol.cz",                       # Palacký University Open Portal (Czechia)
    "repozitar.techlib.cz",                     # National Library of Technology (Czechia)
    "researchrepository.universityofgalway.ie", # University of Galway (Ireland)
    "ridda2.utp.ac.pa",                         # Universidad Tecnológica de Panamá
    "scholarbank.nus.edu.sg",                   # National University of Singapore
    "scholarshare.temple.edu",                  # TUScholarShare, Temple University (USA)
    "scholarworks.umass.edu",                   # University of Massachusetts Amherst (USA)
    "share.swps.edu.pl",                        # SWPS University (Poland)
    "tam-datahub.online.uni-marburg.de",        # TAM DataHub, University of Marburg (Germany)
    "toubkal.imist.ma",                         # Toubkal (Morocco)
    "tudatalib.ulb.tu-darmstadt.de",            # TUdatalib, TU Darmstadt (Germany)
    "ulir.ul.ie",                               # University of Limerick (Ireland)
    "umontreal.scholaris.ca",                   # Papyrus, Université de Montréal (Canada)
    "unsworks.unsw.edu.au",                     # University of New South Wales (Australia)
    "utoronto.scholaris.ca",                    # TSpace, University of Toronto (Canada)
    "uwo.scholaris.ca",                         # Western Open Repository (Canada)
    "www.research-collection.ethz.ch",          # ETH Zürich Research Collection (Switzerland)

    # -- added 2026-10-05, issue #461 --
    "ecommons.cornell.edu",                     # Cornell eCommons (USA) -- confirmed live via /server/api (DSpace 8.2); NOT legacy DSpace despite issue #461 filing it there (/rest/test 404s with a DSpace-CRIS branded 404 page)
    "www.repository.cam.ac.uk"                  # Apollo, University of Cambridge (UK) -- citable UI host; its own /server/api 404s, see .dspace7_api_host_aliases() below for the real API host
  ) |>
    # and the hosts the repository survey found (#636; R/repo-tables.R)
    c(.dspace7_hosts_survey())
}

.dspace7_host_regex <- function() {
  paste(gsub("\\.", "\\\\.", .dspace7_hosts()), collapse = "|")
}

# UI host -> real API host, for an installation where the two are split
# across different subdomains (so far seen only at Cambridge's Apollo
# repository -- every other host above serves both the citable landing page
# and /server/api on the same domain). Confirmed live 2026-10-05, issue
# #461: www.repository.cam.ac.uk/server/api 404s, while
# api.repository.cam.ac.uk/server/api returns a real DSpace 8 root document
# naming www.repository.cam.ac.uk as its own "dspaceUI". Consulted by
# .dspace7_parse() so a citation landing on the UI host still queries the
# right API host; add further entries here, not as a separate unrelated
# host in .dspace7_hosts(), if another split installation turns up.
.dspace7_api_host_aliases <- function() {
  c("www.repository.cam.ac.uk" = "api.repository.cam.ac.uk")
}

# Extract (host, uuid, handle) from a URL referencing a known DSpace 7 host.
# A citation may link either the item's uuid-based page (.../items/<uuid>) or
# its persistent handle (.../handle/<prefix>/<suffix>, still served as a
# permalink by DSpace 7) -- both are captured so .dspace7_info() can resolve
# the item directly by uuid when present (one request) or fall back to
# /pid/find for a handle (two requests). No bare (host-less) mention fallback
# here: unlike PsychArchives, no single installation among these 64 is common
# enough to justify guessing which one a bare handle prefix belongs to.
#
# Vectorised; returns a data.frame(host, uuid, handle) the same length as
# `url`, all NA for an element matching none of these.
.dspace7_parse <- function(url) {
  url <- as.character(url)
  n <- length(url)
  host <- rep(NA_character_, n)
  uuid <- rep(NA_character_, n)
  handle <- rep(NA_character_, n)
  has_url <- !is.na(url) & nzchar(url)
  if (!any(has_url))
    return(data.frame(host = host, uuid = uuid, handle = handle, stringsAsFactors = FALSE))

  host_regex <- .dspace7_host_regex()
  uuid_pat <- "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  handle_pat <- "(?<=/handle/)[0-9]{1,5}(?:\\.[0-9]+){0,2}/[0-9A-Za-z.]+"

  aliases <- .dspace7_api_host_aliases()
  for (i in which(has_url)) {
    u <- url[i]
    hm <- regmatches(u, regexpr(host_regex, u, ignore.case = TRUE, perl = TRUE))
    if (length(hm) > 0) {
      h <- tolower(hm)
      # A citation landing on a UI-only host (see .dspace7_api_host_aliases())
      # needs the real API host here, since this value is used downstream
      # (dspace7_file_download()/.dspace7_info()) to build the actual
      # /server/api request -- the UI host itself would 404 on every call.
      host[i] <- if (h %in% names(aliases)) aliases[[h]] else h
    }

    um <- regmatches(u, regexpr(uuid_pat, u, ignore.case = TRUE, perl = TRUE))
    if (length(um) > 0) uuid[i] <- tolower(um)

    hd <- regmatches(u, regexpr(handle_pat, u, perl = TRUE))
    if (length(hd) > 0) handle[i] <- sub("[.,;]+$", "", hd)
  }
  data.frame(host = host, uuid = uuid, handle = handle, stringsAsFactors = FALSE)
}

#' Find DSpace 7+ Links in Papers
#'
#' Get all links to any of the DSpace 7 (or later) installations in
#' \code{.dspace7_hosts()} -- see the note at the top of this file for how
#' they were found and why they need a separate implementation from
#' \code{dspace_links()} (legacy DSpace, archive-psycharchives.R). Real
#' hyperlinks from the paper's own \code{url} table, plus a body-text
#' fallback for a bare mention that still carries one of these hosts (a bare
#' handle prefix with no host at all cannot be attributed to a specific
#' installation -- see \code{.dspace7_parse()}).
#'
#' A DOI mentioned in the text is also looked up, and kept when it leads to
#' one of these installations. The paper's own DOI, and DOIs under a prefix
#' that another detector handles and no DSpace 7 installation uses (see
#' \code{.dspace7_skip_doi_prefixes()}), are not looked up. The lookup reads the
#' URL registered for the DOI from doi.org's handle API rather than visiting
#' the landing page, and each DOI is looked up once per R session.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the DSpace 7 url in the first (href) column
#' @export
#'
#' @examples
#' paper <- demopaper()
#' dspace7_links(paper)
dspace7_links <- function(paper) {
  href <- text <- NULL
  host_regex <- .dspace7_host_regex()

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(host_regex, href, ignore.case = TRUE))

  ds7_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?(?:", host_regex, ")/[A-Za-z0-9/._-]+"
  )
  other_ds7 <- text_search(paper, ds7_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # A paper citing ONLY a DOI, with the host domain never appearing anywhere
  # in its text, was once invisible to dspace7_links() entirely. Confirmed live 2026-09-29 (issue
  # #435): 10.13020/q3y6-h459 resolves to conservancy.umn.edu, a known host,
  # but neither that host string nor a recognisable path shape ever appears
  # in the citing text.
  #
  # Resolved instead by live-resolving each distinct DOI-shaped string found
  # in the paper under a DSpace 7 prefix (.dspace7_doi_prefixes()) and keeping
  # only the ones that redirect to a known DSpace 7 host -- bounded to the
  # DOIs actually present in this one paper, not an unbounded scan. The bare "10.xxx/yyy" is captured directly (the same
  # regexec()-capture-group idiom every sibling archive-*.R DOI extractor
  # uses -- .dryad_doi(), .figshare_id(), .dataone_pid() -- rather than
  # matching a whole doi.org URL and normalising it afterwards).
  doi_bare_regex <- "(?:https?://)?(?:dx\\.)?(?:doi\\.org/)?(10\\.\\d{3,9}/[-._;()/:<>A-Za-z0-9]+[A-Za-z0-9])"
  doi_mentions <- text_search(paper, doi_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  .extract_doi <- function(x) {
    match <- regexec(doi_bare_regex, x, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(x, match)[[1]]
    if (length(groups) >= 2) groups[[2]] else NA_character_
  }

  #
  # Resolving every DOI in every paper cost 0.4 to 6.5 s per DOI (measured
  # 2026-10-13; about 0.85 DOIs per paper on a psychology corpus), and most
  # of those DOIs could never lead to a DSpace 7 item: the paper's own DOI,
  # and DOIs under a prefix another detector already handles and that no
  # DSpace 7 installation uses (see .dspace7_skip_doi_prefixes()). Both are
  # left out before any request is made, and the rest are looked up through
  # doi.org's handle API instead of by following the redirect to the
  # landing page -- see .dspace7_resolve_doi_url().
  other_ds7_doi <- if (nrow(doi_mentions) > 0) {
    doi_mentions$doi <- vapply(doi_mentions$href, .extract_doi, character(1))
    doi_mentions <- doi_mentions[!is.na(doi_mentions$doi), , drop = FALSE]

    # The paper's own DOI, matched per paper so that one paper of a
    # paperlist citing another paper of the same list is still checked.
    own <- tryCatch(paper_table(paper, "info", "doi"), error = \(e) NULL)
    if (!is.null(own) && nrow(own) > 0 && all(c("paper_id", "doi") %in% names(own)) &&
        "paper_id" %in% names(doi_mentions)) {
      own_keys <- paste(own$paper_id, .dspace7_doi_key(own$doi))
      is_own <- paste(doi_mentions$paper_id, .dspace7_doi_key(doi_mentions$doi)) %in% own_keys
      doi_mentions <- doi_mentions[!is_own, , drop = FALSE]
    }

    # Only a DOI under a prefix with DOIs registered on a DSpace 7+ host is
    # looked up (.dspace7_doi_prefixes(), R/repo-tables.R; #636): looking up
    # every other DOI cost 25,172 lookups (about 2.4 hours) in the main texts
    # of 17 corpora (13,706 papers), and 445,034 in their reference lists,
    # where the prefix list leaves 20 and 78. The list leaves out the
    # prefixes of the other detectors' platforms (.dspace7_skip_doi_prefixes(),
    # minus Dataverse's), so a prefix a Dataverse and a DSpace 7 host share
    # (10.23673, Tartu; 10.7939, Alberta) is looked up by both.
    prefix <- tolower(sub("/.*$", "", doi_mentions$doi))
    dois <- unique(doi_mentions$doi[prefix %in% .dspace7_doi_prefixes()])
    # .dspace7_parse() needs the resolved landing-page URL itself (to read
    # off a uuid or handle from its path), not the bare DOI string, so the
    # resolved URL replaces href here rather than just gating on whether one
    # was found.
    resolved_urls <- vapply(dois, .dspace7_resolve_doi_url, character(1))
    names(resolved_urls) <- dois
    rows <- doi_mentions[doi_mentions$doi %in% dois[!is.na(resolved_urls)], , drop = FALSE]
    if (nrow(rows) > 0) rows$href <- unname(resolved_urls[rows$doi])
    rows$doi <- NULL
    rows
  } else {
    doi_mentions[0, , drop = FALSE]
  }

  dplyr::bind_rows(found_href, other_ds7, other_ds7_doi) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()
}

# A DOI in a comparable form: lower case, without a doi.org/dx.doi.org
# address or "doi:" label in front. Vectorised; NA stays NA.
.dspace7_doi_key <- function(doi) {
  doi <- tolower(trimws(as.character(doi)))
  sub("^(?:https?://)?(?:dx\\.)?(?:doi\\.org/)?(?:doi:\\s*)?", "", doi, perl = TRUE)
}

# DOI prefixes dspace7_links() does not resolve: each belongs to a host
# another detector already handles (OSF, Zenodo, Dryad, Figshare and its
# institutional instances, 4TU.ResearchData, Mendeley Data, DataONE, UK Data
# Service ReShare, PsychArchives, Dataverse, LDbase), so a DOI under it cannot lead
# to a DSpace 7 item. Read from the other detectors' own prefix tables
# rather than copied, so a prefix added there is skipped here too.
#
# Checked against DataCite 2026-10-13, two ways: (1) for every host in
# .dspace7_hosts(), DataCite's facet of the DOI prefixes registered with a
# URL on that host (query url:*<host>*), and (2) 25 random DOIs from every
# prefix below, following an hdl.handle.net registration once, checked for
# a DSpace 7 host. One prefix turned up in both a Dataverse table and a
# DSpace 7 installation: 10.23673 is listed for datadoi.ee (the University
# of Tartu's Dataverse) but the same university also registers DOIs under
# it for dspace.ut.ee, so it is still resolved.
.dspace7_skip_doi_prefixes <- function() {
  dataone <- vapply(.dataone_hosts(), \(h) h$doi_prefix %||% NA_character_, character(1))
  skip <- c(
    "10.17605",                                # OSF
    "10.5281",                                 # Zenodo
    .dryad_doi_prefixes(),                     # Dryad
    "10.6084", names(.figshare_doi_prefix_hosts()),  # Figshare
    "10.4121",                                 # 4TU.ResearchData
    "10.17632",                                # Mendeley Data
    "10.33009",                                # LDbase
    stats::na.omit(dataone),                   # DataONE member nodes
    "10.5255",                                 # UK Data Service ReShare
    "10.23668",                                # PsychArchives
    "10.7910", unlist(.dataverse_doi_prefix_hosts(), use.names = FALSE)  # Dataverse
  )
  setdiff(unique(tolower(skip)), "10.23673")
}

# Session cache of resolved DOIs (DOI key -> DSpace 7 URL, or NA when the
# DOI leads elsewhere), so a DOI cited by many papers in a corpus is looked
# up once. Only definite answers are kept: a request that failed (network
# error, server error) is tried again the next time the DOI comes up.
.dspace7_doi_cache <- new.env(parent = emptyenv())

# TRUE when `url` lies on a known DSpace 7 host, or a subdomain of one. The
# URL's host name is compared (a leading "www." aside): a host found anywhere
# in the URL, as in its path or query string, counted (U211).
.dspace7_on_known_host <- function(url) {
  if (length(url) != 1 || is.na(url)) return(FALSE)
  host <- tryCatch(httr2::url_parse(url)$hostname, error = \(e) NULL)
  if (is.null(host) || !length(host) || is.na(host)) return(FALSE)
  host <- sub("^www\\.", "", tolower(host))
  known <- sub("^www\\.", "", tolower(.dspace7_hosts()))
  any(host == known | endsWith(host, paste0(".", known)))
}

# The URL registered for one handle -- a DOI is a handle too -- read from a
# Handle System proxy's JSON API, without visiting that URL. Used here and
# by .researchdata4tu_resolve_uuid() (archive-4tu.R); doi_resolves()
# (doi.R) asks the same API but only whether a URL exists.
#
# @param handle e.g. "10.13020/q3y6-h459" or "11299/208294"
# @param api the proxy's handle API, doi.org's or hdl.handle.net's
# @returns list(url = the registered URL or NA, ok = TRUE when the proxy
#   gave a definite answer -- a URL, "handle not found" (responseCode 100,
#   HTTP 404) or "no URL value" (responseCode 200) -- and FALSE when the
#   request itself failed)
.handle_registered_url <- function(handle, api = "https://doi.org/api/handles/") {
  url <- paste0(api, utils::URLencode(handle, reserved = TRUE), "?type=URL")
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(30) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  if (is.null(resp)) return(list(url = NA_character_, ok = FALSE))
  body <- tryCatch(httr2::resp_body_json(resp, check_type = FALSE), error = \(e) NULL)
  code <- body$responseCode %||% NA
  if (identical(as.integer(code), 1L)) {
    vals <- Filter(\(v) identical(v$type, "URL"), body$values %||% list())
    value <- if (length(vals) > 0) vals[[1]]$data$value %empty_or% NA_character_ else NA_character_
    return(list(url = as.character(value), ok = TRUE))
  }
  list(url = NA_character_, ok = isTRUE(as.integer(code) %in% c(100L, 200L)))
}

# Resolve a single DOI and return the URL it leads to if that is on a known
# DSpace 7 host, or NA if it does not resolve or leads elsewhere -- see
# dspace7_links()'s header comment (issue #435).
#
# Uses the Handle System's JSON API rather than following the DOI's
# redirects: GET https://doi.org/api/handles/<doi>?type=URL returns the
# registered URL in about 0.35 s without downloading any landing page
# (following the redirects took 0.4 to 6.5 s per DOI, measured 2026-10-13).
# Two kinds of registered URL need one more step, each also a handle-API
# lookup at hdl.handle.net:
#   - an hdl.handle.net address. Many DSpace installations register their
#     DOIs this way, e.g. 10.13020/q3y6-h459 -> hdl.handle.net/11299/208294
#     -> conservancy.umn.edu (verified 2026-10-13).
#   - a DSpace /handle/<handle> page on a host that is not in
#     .dspace7_hosts(): the installation's former address, kept in the DOI
#     record after the repository moved, e.g. 10.35090 DOIs registered at
#     smartech.gatech.edu, which now redirects to repository.gatech.edu.
#     The handle record itself is updated when a repository moves, so it
#     leads to the current address.
# Results are cached for the session (see .dspace7_doi_cache).
.dspace7_resolve_doi_url <- function(doi) {
  key <- .dspace7_doi_key(doi)
  if (is.na(key) || !nzchar(key)) return(NA_character_)
  if (exists(key, envir = .dspace7_doi_cache, inherits = FALSE))
    return(get(key, envir = .dspace7_doi_cache, inherits = FALSE))

  hdl_api <- "https://hdl.handle.net/api/handles/"
  # DataCite's record first: it gives the address and says whether the DOI
  # is a publication (a paper or thesis in an institutional repository),
  # which is not looked up further (#637; .repo_doi_url(), R/repo-kind.R);
  # a DOI DataCite does not have is asked of the handle service
  reg <- .repo_doi_url(doi)
  url <- reg$url
  ok <- reg$ok

  hdl_rx <- "^https?://(?:hdl\\.)?handle\\.net/([^?#]+)"
  if (ok && !is.na(url) && grepl(hdl_rx, url, ignore.case = TRUE, perl = TRUE)) {
    handle <- .url_decode(sub(hdl_rx, "\\1", url, ignore.case = TRUE, perl = TRUE))
    reg <- .handle_registered_url(handle, api = hdl_api)
    url <- reg$url
    ok <- reg$ok
  }

  if (ok && !is.na(url) && !.dspace7_on_known_host(url)) {
    handle <- regmatches(url, regexpr("(?<=/handle/)[0-9]{1,5}(?:\\.[0-9]+){0,2}/[0-9A-Za-z.]+",
                                      url, perl = TRUE))
    if (length(handle) > 0) {
      reg <- .handle_registered_url(sub("[.,;]+$", "", handle), api = hdl_api)
      if (!reg$ok) ok <- FALSE
      if (.dspace7_on_known_host(reg$url)) url <- reg$url
    }
  }

  resolved <- if (ok && .dspace7_on_known_host(url)) url else NA_character_
  if (ok) assign(key, resolved, envir = .dspace7_doi_cache)
  resolved
}

# One DSpace 7 REST request returning parsed JSON, or NULL on any failure /
# non-200. Mirrors .psycharchives_rest()'s error handling. `path` is
# relative to the installation's /server/api; `url` instead takes a full
# address, for following a `_links.next` page link as given.
.dspace7_rest <- function(path, host, url = paste0("https://", host, "/server/api", path)) {
  tryCatch({
    resp <- .mc_request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) return(NULL)
    httr2::resp_body_json(resp)
  }, error = \(e) NULL)
}

# The uuid of the item a handle names, from /pid/find without following its
# redirect. /pid/find answers 302 with the item's own address
# (".../server/api/core/items/<uuid>", verified 2026-10-13 on
# conservancy.umn.edu 11299/208294 and repository.gatech.edu 1853/67239);
# following it downloads the whole item, which then had to be requested a
# second time anyway, because query parameters such as `embed` do not
# survive the redirect. Reading the uuid from the Location header instead
# saves that download. A 200 answer (an installation that does not
# redirect) is read the same way the followed redirect used to be.
.dspace7_find_uuid <- function(host, handle) {
  url <- paste0("https://", host, "/server/api/pid/find?id=",
                utils::URLencode(handle, reserved = TRUE))
  resp <- tryCatch(
    .mc_request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  if (is.null(resp)) return(NA_character_)

  status <- httr2::resp_status(resp)
  uuid_pat <- "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  if (status >= 300 && status < 400) {
    loc <- httr2::resp_header(resp, "location") %||% ""
    m <- regmatches(loc, regexpr(uuid_pat, loc, ignore.case = TRUE))
    return(if (length(m) > 0) tolower(m) else NA_character_)
  }
  if (status == 200) {
    found <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    return(found$uuid %empty_or% NA_character_)
  }
  NA_character_
}

# One item, with its bundles and their bitstreams embedded, in a single
# request. Verified live 2026-10-13: /core/items/<uuid>?embed=bundles/
# bitstreams returns the item, its bundles and each bundle's bitstreams
# together (previously three requests: the item, /bundles, and the ORIGINAL
# bundle's /bitstreams), and embed.size=bundles/bitstreams=200 (accepted,
# though not documented) raises the embedded bitstream page from DSpace's
# default 20 to 200.
.dspace7_item <- function(host, uuid) {
  .dspace7_rest(paste0("/core/items/", uuid,
                       "?embed=bundles/bitstreams&embed.size=bundles/bitstreams=200"),
                host = host)
}

# Every bitstream of one bundle. A bitstream listing is a page (20 by
# default; 200 when embedded by .dspace7_item()), and the code used to read
# only the first one, so an item with more files than that was silently
# cut short. Each further page is reached through `_links.next`, an
# absolute address such as ".../core/bundles/<uuid>/bitstreams?page=1&size=1"
# (verified 2026-10-13 on conservancy.umn.edu with embed.size 1). When the
# installation did not embed the bitstreams, they are requested directly.
# Warns when fewer bitstreams arrive than the page reports in total.
.dspace7_bitstreams <- function(bundle, host) {
  page <- bundle[["_embedded"]][["bitstreams"]]
  if (is.null(page)) {
    page <- .dspace7_rest(paste0("/core/bundles/", bundle$uuid, "/bitstreams?size=200"),
                          host = host)
  }
  out <- page[["_embedded"]][["bitstreams"]] %||% list()
  total <- page$page$totalElements
  next_href <- page[["_links"]][["next"]][["href"]]
  seen <- character(0)
  while (!is.null(next_href) && !next_href %in% seen) {
    seen <- c(seen, next_href)
    nxt <- .dspace7_rest(url = next_href)
    if (is.null(nxt)) break
    out <- c(out, nxt[["_embedded"]][["bitstreams"]] %||% list())
    next_href <- nxt[["_links"]][["next"]][["href"]]
  }
  if (!is.null(total) && length(out) < total) {
    warning(sprintf(
      "%s listed only %d of the %d files of bundle %s. Files not listed are not retrieved, so this listing is incomplete. Run again later.",
      host, length(out), as.integer(total), bundle$uuid %||% ""), call. = FALSE)
  }
  out
}

# Retrieve one item's info (title/authors/doi/license/date + its ORIGINAL
# bundle's public files) from a DSpace 7 installation. Resolves by uuid
# directly when known (one request, with bundles and bitstreams embedded --
# see .dspace7_item()); otherwise by handle, reading the uuid from
# /pid/find's redirect first (two requests -- see .dspace7_find_uuid()).
# This used to take four requests for a handle (pid/find followed to the
# item, the same item again, /bundles, /bitstreams) and three for a uuid.
.dspace7_info <- function(host, uuid = NA_character_, handle = NA_character_, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  # NA is not NULL, so %||% never fell back and the message showed "(NA)" (U42)
  item_label <- if (!is.null(uuid) && !is.na(uuid)) uuid else
    if (!is.null(handle) && !is.na(handle)) handle else "unknown"
  paste0("* Retrieving info from ", host, " (", item_label, ")...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(dspace7_host = host)

  item <- NULL
  if (!is.na(uuid) && nzchar(uuid)) {
    item <- .dspace7_item(host, uuid)
  }
  if (is.null(item) && !is.na(handle) && nzchar(handle)) {
    found_uuid <- .dspace7_find_uuid(host, handle)
    if (!is.na(found_uuid)) {
      item <- .dspace7_item(host, found_uuid)
    }
  }
  if (is.null(item) || is.null(item$uuid)) {
    warning(host, " (", item_label, ") could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }
  obj$dspace7_uuid <- item$uuid

  md <- item$metadata %||% list()
  md_val <- function(keys) {
    for (key in keys) {
      entries <- md[[key]]
      if (!is.null(entries) && length(entries) > 0) {
        # %empty_or% (not %||%) because m$value can come back as a
        # length-zero value rather than NULL; vapply(..., character(1))
        # requires exactly length 1 from every call.
        vals <- vapply(entries, \(m) m$value %empty_or% NA_character_, character(1))
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0) return(paste(vals, collapse = "; "))
      }
    }
    NA_character_
  }

  # %empty_or% (not %||%) because a field can come back as a length-zero
  # value rather than NULL; %||% letting that through would break the $<-
  # assignment below with "replacement has 0 rows".
  obj$title           <- item$name %empty_or% md_val("dc.title")
  obj$authors          <- md_val("dc.contributor.author")
  obj$doi              <- md_val(c("dc.identifier.doi"))
  obj$license           <- md_val(c("dc.rights", "dc.rights.uri", "dc.rights.license"))
  obj$publication_date <- md_val(c("dc.date.issued", "dc.date.available"))
  obj$updated_date      <- item$lastModified %empty_or% NA_character_
  # what the item is (dc.type: "Dataset", "Article", "Doctoral thesis", ...):
  # repo_check ignores a publication (#637; .repo_publication_type())
  obj$record_type       <- md_val("dc.type")

  # Only the ORIGINAL bundle holds deposited files -- LICENSE/THUMBNAIL/SWORD
  # (and others some installations add) are derived or administrative, not
  # data the manuscript's own citation is pointing at. Verified live
  # 2026-09-11 (see the note at the top of this file).
  file_list <- data.frame(name = character(0), size = numeric(0),
                          checksum = character(0), retrieve = character(0))
  # The bundles normally arrive embedded in the item (see .dspace7_item());
  # an installation that ignores `embed` is asked for them separately, as
  # before.
  bundles <- item[["_embedded"]][["bundles"]]
  if (is.null(bundles)) {
    bundles <- .dspace7_rest(paste0("/core/items/", item$uuid, "/bundles"), host = host)
  }
  bundle_list <- bundles[["_embedded"]][["bundles"]] %||% list()
  original <- NULL
  for (b in bundle_list) if (identical(b$name, "ORIGINAL")) original <- b
  if (!is.null(original)) {
    bitstreams <- .dspace7_bitstreams(original, host = host)
    if (length(bitstreams) > 0) {
      # %empty_or% (not %||%) because a field can come back as a length-zero
      # value rather than NULL; vapply(..., character(1)/numeric(1)) requires
      # exactly length 1 from every call.
      file_list <- data.frame(
        name = vapply(bitstreams, \(b) b$name %empty_or% NA_character_, character(1)),
        size = vapply(bitstreams, \(b) as.numeric(b$sizeBytes %empty_or% NA_real_), numeric(1)),
        checksum = vapply(bitstreams,
          \(b) b$checkSum$value %empty_or% NA_character_, character(1)),
        retrieve = vapply(bitstreams,
          \(b) b[["_links"]][["content"]][["href"]] %empty_or% NA_character_, character(1))
      )
    }
  }
  obj$files <- list(file_list)

  obj
}

#' Retrieve public file list from a DSpace 7+ installation by URL
#'
#' Lists the publicly retrievable files (the item's ORIGINAL bundle) of one or
#' more DSpace 7 items via the REST API, without downloading them. Each row
#' carries an absolute \code{file_url} so the actual bytes are fetched later
#' by \code{\link{download_repo_files}}, the same deferred path used for
#' Zenodo, OSF, and legacy DSpace (\code{psycharchives_file_download()}).
#'
#' @param dspace7_url a vector of DSpace 7 item URLs
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of file information (one row per public file)
#' @export
#' @examples
#' \donttest{
#' if (online("conservancy.umn.edu")) {
#'   # lists the public files, which download_repo_files() can then fetch
#'   dspace7_file_download("https://conservancy.umn.edu/handle/11299/208294")
#' }
#' }
dspace7_file_download <- function(dspace7_url, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  if (length(dspace7_url) > 1) {
    unique_urls <- unique(dspace7_url) |> setdiff(NA)

    file_lists <- lapply(unique_urls, dspace7_file_download, pb = pb)
    info <- do.call(dplyr::bind_rows, file_lists)
    # every link failed: NULL, as for one failed link; the join below stopped
    # on a table without columns ("Join columns in `y` must be present", U43)
    if (nrow(info) == 0) return(NULL)
    orig <- data.frame(dspace7_url = dspace7_url)
    df <- dplyr::left_join(orig, info, by = "dspace7_url")

    # doi/license carried as attributes rather than columns, mirroring
    # psycharchives_file_download()'s identical pattern for legacy DSpace --
    # see the comment there for why (keeps the file frame file-only while the
    # caller, repo_check.R, can still surface them in repo_metadata).
    license <- unlist(lapply(file_lists, \(x) attr(x, "license")))
    attr(df, "license") <- license
    doi <- unlist(lapply(file_lists, \(x) attr(x, "doi")))
    attr(df, "doi") <- doi
    attr(df, "record_type") <- unlist(lapply(file_lists, \(x) attr(x, "record_type")))

    return(df)
  }

  paste0("* Listing files from ", dspace7_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  parsed <- .dspace7_parse(dspace7_url)
  host <- parsed$host[[1]]
  if (is.na(host)) return(NULL)

  info <- .dspace7_info(host, uuid = parsed$uuid[[1]], handle = parsed$handle[[1]], pb = pb)
  if ("error" %in% names(info)) return(NULL)

  # license/doi carried as attributes rather than columns -- reuses the
  # metadata .dspace7_info() already fetched, same mechanism
  # psycharchives_file_download() uses for legacy DSpace's rights/doi.
  license <- stats::setNames(info$license %||% NA_character_, dspace7_url)
  doi <- stats::setNames(info$doi %||% NA_character_, dspace7_url)
  # dc.type, so repo_check can tell a publication from a deposit (#637)
  record_type <- stats::setNames(info$record_type %||% NA_character_, dspace7_url)

  file_list <- info$files[[1]]
  if (is.null(file_list) || nrow(file_list) == 0) {
    paste0("- ", dspace7_url, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    empty <- data.frame(
      dspace7_url = character(0), name = character(0), file_url = character(0),
      file_location = character(0), size = numeric(0), isdir = logical(0),
      ext = character(0), type = character(0)
    )
    attr(empty, "license") <- license
    attr(empty, "doi") <- doi
    attr(empty, "record_type") <- record_type
    return(empty)
  }

  df <- data.frame(
    dspace7_url = rep(dspace7_url, nrow(file_list)),
    name = file_list$name,
    file_url = file_list$retrieve,
    file_location = NA_character_,
    size = file_list$size,
    isdir = FALSE
  )

  df$ext <- strsplit(df$name, "\\.") |>
    sapply(\(x) if (length(x) < 2) "" else x[[length(x)]]) |>
    tolower()
  df <- dplyr::left_join(df, metacheck::file_types, by = "ext")

  attr(df, "license") <- license
  attr(df, "doi") <- doi
  attr(df, "record_type") <- record_type

  df
}
