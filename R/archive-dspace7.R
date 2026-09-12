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
    "www.research-collection.ethz.ch"           # ETH Zürich Research Collection (Switzerland)
  )
}

.dspace7_host_regex <- function() {
  paste(gsub("\\.", "\\\\.", .dspace7_hosts()), collapse = "|")
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

  for (i in which(has_url)) {
    u <- url[i]
    hm <- regmatches(u, regexpr(host_regex, u, ignore.case = TRUE, perl = TRUE))
    if (length(hm) > 0) host[i] <- tolower(hm)

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
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the DSpace 7 url in the first (href) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' dspace7_links(psychsci)
#' }
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

  dplyr::bind_rows(found_href, other_ds7) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()
}

# One DSpace 7 REST request returning parsed JSON, or NULL on any failure /
# non-200. Mirrors .psycharchives_rest()'s error handling.
.dspace7_rest <- function(path, host) {
  url <- paste0("https://", host, "/server/api", path)
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) return(NULL)
    httr2::resp_body_json(resp)
  }, error = \(e) NULL)
}

# Retrieve one item's info (title/authors/doi/license/date + its ORIGINAL
# bundle's public files) from a DSpace 7 installation. Resolves by uuid
# directly when known (one request); otherwise by handle via /pid/find (two
# requests: resolve, then the same item lookup a direct uuid would have used).
.dspace7_info <- function(host, uuid = NA_character_, handle = NA_character_, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ", host, " (", (uuid %||% handle), ")...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(dspace7_host = host)

  item <- NULL
  if (!is.na(uuid) && nzchar(uuid)) {
    item <- .dspace7_rest(paste0("/core/items/", uuid), host = host)
  }
  if (is.null(item) && !is.na(handle) && nzchar(handle)) {
    found <- .dspace7_rest(paste0("/pid/find?id=", utils::URLencode(handle, reserved = TRUE)), host = host)
    if (!is.null(found) && !is.null(found$uuid)) {
      item <- .dspace7_rest(paste0("/core/items/", found$uuid), host = host)
    }
  }
  if (is.null(item) || is.null(item$uuid)) {
    warning(host, " (", (uuid %||% handle %||% "unknown"), ") could not be found", call. = FALSE)
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

  # Only the ORIGINAL bundle holds deposited files -- LICENSE/THUMBNAIL/SWORD
  # (and others some installations add) are derived or administrative, not
  # data the manuscript's own citation is pointing at. Verified live
  # 2026-09-11 (see the note at the top of this file).
  file_list <- data.frame(name = character(0), size = numeric(0),
                          checksum = character(0), retrieve = character(0))
  bundles <- .dspace7_rest(paste0("/core/items/", item$uuid, "/bundles"), host = host)
  bundle_list <- bundles[["_embedded"]][["bundles"]] %||% list()
  original <- NULL
  for (b in bundle_list) if (identical(b$name, "ORIGINAL")) original <- b
  if (!is.null(original)) {
    bs <- .dspace7_rest(paste0("/core/bundles/", original$uuid, "/bitstreams"), host = host)
    bitstreams <- bs[["_embedded"]][["bitstreams"]] %||% list()
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
dspace7_file_download <- function(dspace7_url, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  if (length(dspace7_url) > 1) {
    unique_urls <- unique(dspace7_url) |> setdiff(NA)

    file_lists <- lapply(unique_urls, dspace7_file_download, pb = pb)
    info <- do.call(dplyr::bind_rows, file_lists)
    orig <- data.frame(dspace7_url = dspace7_url)
    df <- dplyr::left_join(orig, info, by = "dspace7_url")
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

  file_list <- info$files[[1]]
  if (is.null(file_list) || nrow(file_list) == 0) {
    paste0("- ", dspace7_url, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
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

  df
}
