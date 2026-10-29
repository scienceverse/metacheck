# Dryad (https://datadryad.org) is a single hosted service (unlike
# Dataverse's many independent installations), so detection here keys off one
# host the same way zenodo_links() does.
#
# Verified live 2026-08-16 against a real dataset (doi:10.5061/dryad.j1fd7,
# api.figshare.com's counterpart at datadryad.org/api/v2/datasets/...): field
# names below (identifier/title/authors/publicationDate/lastModificationDate/
# license, and per-file path/size/mimeType/digest/digestType) were read from
# that real JSON response, not guessed from documentation. The dataset JSON
# also carries `_links.stash:download.href`, Dryad's documented whole-dataset
# bulk-download endpoint (the same link a dataset page's own "Download
# Dataset" button uses) -- unlike Figshare, which has no such endpoint.
#
# Dryad DOIs are not all issued under the 10.5061 prefix -- Dryad's own
# DataCite client record (api.datacite.org/clients/dryad.dryad) only lists
# its two CURRENTLY active DataCite prefixes (10.5061, 10.15146), but real
# minted DOIs also exist under several older EZID-era prefixes that predate
# or sit outside that registry (older datasets keep whatever prefix they
# were minted under, unlike a URL that can be updated after the fact).
# The list below is the empirically COMPLETE set found by repeatedly
# random-sampling DataCite's own DOI index for Dryad's client id
# (data/code_data_preparation/scratch_find_all_dryad_prefixes.R,
# 2026-09-17) until 40 consecutive batches of 100 random DOIs each
# produced no new prefix. Every prefix was individually confirmed live via
# doi.org's own handle API to resolve to a real datadryad.org dataset page
# (e.g. 10.25338/B8N33J -> https://datadryad.org/dataset/doi:10.25338/B8N33J).
# Two prefixes seen elsewhere in the SAME investigation (10.7294, 10.5063)
# were checked the same way and confirmed NOT Dryad (VTechData and KNB
# respectively) -- kept as an explicit, individually-verified allowlist
# rather than a bare numeric-prefix wildcard for exactly that reason:
# nothing guarantees a not-yet-seen prefix is Dryad's rather than an
# unrelated publisher's, and DataCite prefixes are reused/reassigned
# across organizations over time.
.dryad_doi_prefixes <- function() {
  c("10.5061", "10.15146", "10.25338", "10.25349", "10.5068",
    "10.6071", "10.6075", "10.6076", "10.6078", "10.6086",
    "10.7272", "10.7280", "10.7291", "10.7941")
}

#' Find Dryad Links in Papers
#'
#' Get all Dryad links: real hyperlinks from the paper's own `url` table,
#' plus a body-text fallback for a BARE mention (a DOI like
#' "10.5061/dryad.j1fd7" is routinely cited without any URL scheme at all)
#' that the source PDF/HTML never encoded as an actual hyperlink -- the `url`
#' table only ever contains links the source document itself made clickable.
#' Same two-tier approach `zenodo_links()` uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Dryad url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' dryad_links(psychsci)
#' }
dryad_links <- function(paper) {
  href <- text <- NULL

  # 10.5061 DOIs always carry a literal "dryad." word after the slash
  # (10.5061/dryad.j1fd7); the alternate prefixes below do not
  # (10.25338/B8N33J) -- matched separately since a prefix-only pattern
  # for 10.25338/10.5068 would also swallow unrelated DOIs under those
  # same registrant prefixes if they exist (unverified; see the
  # .dryad_doi_prefixes() note above).
  alt_prefixes <- setdiff(.dryad_doi_prefixes(), "10.5061")
  alt_prefix_regex <- paste(gsub("\\.", "\\\\.", alt_prefixes), collapse = "|")

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(paste0("datadryad\\.org|10\\.5061/dryad|", alt_prefix_regex, "/"),
                        href, ignore.case = TRUE))

  dryad_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?datadryad\\.org/(?:stash/)?dataset[s]?/doi[:%]",
    "[A-Za-z0-9%._/-]*",
    # a sharing link, resolved to its DOI by dryad_info() (#484)
    "|(?:https?://)?(?:www\\.)?datadryad\\.org/(?:stash/)?share/[A-Za-z0-9_-]+",
    "|(?:https?://)?(?:doi\\.org/)?10\\.5061/dryad\\.[A-Za-z0-9]+",
    "|(?:https?://)?(?:doi\\.org/)?(?:", alt_prefix_regex, ")/[A-Za-z0-9]+"
  )
  other_dryad <- text_search(paper, dryad_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_dryad) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$dryad_url <- links$href
  links$dryad_doi <- .dryad_doi(links$dryad_url)

  return(links)
}

#' Get Dryad DOI from URL
#'
#' @param dryad_url a vector of URLs or DOIs to dryad datasets
#'
#' @returns a character vector of DOIs in "10.5061/dryad.xxxxx" form
#' @keywords internal
.dryad_doi <- function(dryad_url) {
  if (length(dryad_url) == 0) {
    return(character(0))
  }

  # USE.NAMES = FALSE (also in the other repositories' id helpers): named by
  # the URLs, an NA name became an NA row name in *_info()'s data.frame(),
  # which stopped with "row names contain missing values" (U33)
  if (length(dryad_url) > 1) {
    return(vapply(dryad_url, .dryad_doi, character(1), USE.NAMES = FALSE))
  }

  # handle single dryad_url ----
  dryad_url <- trimws(as.character(dryad_url))

  if (is.na(dryad_url) || !nzchar(dryad_url)) {
    return(NA_character_)
  }

  dryad_url <- utils::URLdecode(dryad_url)

  alt_prefixes <- setdiff(.dryad_doi_prefixes(), "10.5061")
  alt_prefix_regex <- paste(gsub("\\.", "\\\\.", alt_prefixes), collapse = "|")
  patterns <- c(
    "(10\\.5061/dryad\\.[A-Za-z0-9]+)",
    paste0("((?:", alt_prefix_regex, ")/[A-Za-z0-9]+)")
  )
  for (pattern in patterns) {
    match <- regexec(pattern, dryad_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(dryad_url, match)[[1]]
    if (length(groups) >= 2) {
      return(tolower(groups[[2]]))
    }
  }

  return(NA_character_)
}

# The token of a Dryad sharing link ("datadryad.org/stash/share/<token>" or
# "datadryad.org/share/<token>"), or NA. Vectorised.
.dryad_share_token <- function(url) {
  m <- regmatches(url, regexec("datadryad\\.org/(?:stash/)?share/([A-Za-z0-9_-]+)",
                               url, perl = TRUE, ignore.case = TRUE))
  vapply(m, \(x) if (length(x) >= 2) x[[2]] else NA_character_, character(1))
}

# The DOI behind a Dryad sharing link (#484). Authors link a sharing link,
# made for reviewers before publication, instead of the dataset DOI; it keeps
# working after publication and redirects to the dataset's landing page,
# whose address carries the DOI (verified 2026-10-16: datadryad.org/share/
# <token> answers 302 to /landing/show?id=doi%3A10.5061%2Fdryad.t76hdr81n,
# and an unknown token answers 404). One request, without following the
# redirect. Returns list(doi, error): the DOI, or NA with the reason.
.dryad_share_doi <- function(url) {
  token <- .dryad_share_token(url)
  if (is.na(token)) return(list(doi = NA_character_, error = NA_character_))
  resp <- tryCatch(
    .mc_request(sprintf("https://datadryad.org/share/%s", token)) |>
      httr2::req_headers(`User-Agent` = "metacheck") |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL)
  status <- if (is.null(resp)) NA_integer_ else httr2::resp_status(resp)
  loc <- if (is.null(resp)) NULL else httr2::resp_header(resp, "location")
  doi <- if (is.null(loc)) NA_character_ else .dryad_doi(loc)
  error <- if (!is.na(doi)) NA_character_ else if (isTRUE(status == 404))
    "private or expired Dryad sharing link" else
    "the Dryad sharing link could not be resolved"
  list(doi = doi, error = error)
}

#' Retrieve info from Dryad by URL
#'
#' @param dryad_url a Dryad dataset URL or DOI, or a table containing them
#'   (e.g., as created by [dryad_links()])
#' @param id_col the index or name of the column that contains Dryad URLs, if
#'   `dryad_url` is a table
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a previously cached listing for a dataset
#'   already looked up (see [repo_info_cache_clear()]) instead of re-querying
#'   Dryad's API. Off by default. Worth enabling for a corpus build likely to
#'   be interrupted and restarted, since Dryad's per-IP request quota is
#'   otherwise re-spent on every restart re-listing datasets nothing new
#'   needs fetching for.
#'
#' @returns a data frame of information, one row per link, with the dataset's
#'   metadata, its file records in the list column `files`, and
#'   `storage_size` (the dataset's total size in bytes as Dryad reports it),
#'   `files_total` (the number of files Dryad reports), and
#'   `files_incomplete` (`TRUE` when fewer files could be listed than Dryad
#'   reports; a warning is given too). A sharing link
#'   (`datadryad.org/stash/share/...`) is followed to the dataset it leads to;
#'   one that leads nowhere has its reason in `error`, as a dataset that
#'   could not be found does.
#' @export
#' @examples
#' \dontrun{
#'   dryad_info("https://doi.org/10.5061/dryad.j1fd7")
#' }
dryad_info <- function(dryad_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (!online("datadryad.org")) {
    stop("Dryad seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Dryad Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(dryad_url)) {
    table <- dryad_url
    table$dryad_url <- table[[id_col]]
    # A caller passing dryad_links()'s own output back in (the documented,
    # normal usage) already has a dryad_doi column of its own; ids below
    # recomputes it independently, and left joining ids onto a table that
    # already has that name produces dryad_doi.x/.y suffixes instead of a
    # plain dryad_doi column -- silently breaking the SECOND join further
    # down (by = "dryad_doi"), which then errors with "must be present in
    # the data" only when a dataset isn't found (confirmed live 2026-09-19).
    # Dropped here so ids's own recomputed values are always what flows
    # through, never stale/duplicated ones. Same fix applied identically
    # across every archive-*.R file sharing this table/ids/left_join shape
    # (dataone, dataverse, figshare, mendeley, reshare, zenodo).
    table$dryad_doi <- NULL
  } else {
    raw_urls <- unique(dryad_url) |> stats::na.omit()
    table <- data.frame(dryad_url = raw_urls)
  }

  ids <- data.frame(
    dryad_url = table$dryad_url,
    dryad_doi = .dryad_doi(table$dryad_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$dryad_url), , drop = FALSE]

  # A sharing link carries no DOI; ask Dryad where it leads (#484). One that
  # leads nowhere keeps its reason, which repo_check reports.
  share_error <- character(0)
  is_share <- is.na(ids$dryad_doi) & !is.na(.dryad_share_token(ids$dryad_url))
  for (k in which(is_share)) {
    res <- .dryad_share_doi(ids$dryad_url[[k]])
    ids$dryad_doi[[k]] <- res$doi
    if (!is.na(res$error)) share_error[[ids$dryad_url[[k]]]] <- res$error
  }
  add_share_error <- function(d) {
    if (length(share_error) == 0) return(d)
    if (!"error" %in% names(d)) d$error <- NA_character_
    hit <- d$dryad_url %in% names(share_error)
    d$error[hit] <- unname(share_error[d$dryad_url[hit]])
    d
  }

  valid_dois <- unique(stats::na.omit(ids$dryad_doi))

  if (length(valid_dois) == 0) {
    ("No valid Dryad links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(add_share_error(dplyr::left_join(table, ids, by = "dryad_url")))
  }

  paste0(
    "Starting Dryad retrieval for ",
    length(valid_dois), " dataset",
    ifelse(length(valid_dois) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_dois))
  for (i in seq_along(valid_dois)) {
    doi <- valid_dois[[i]]
    cached <- if (isTRUE(cache)) .repo_info_cache_get("dryad", doi) else NULL
    # A listing stored before the paging fix (0.3.3.1) has no `files_total`:
    # it holds only Dryad's first page of 20 files, so it is not reused, and
    # Dryad is asked again (#506). Neither is one marked incomplete.
    if (!is.null(cached) && (!"files_total" %in% names(cached) ||
                             isTRUE(cached$files_incomplete)))
      cached <- NULL
    if (!is.null(cached)) {
      id_info[[i]] <- cached
    } else {
      id_info[[i]] <- .dryad_info(doi, pb = pb)
      # An incomplete listing (see .dryad_list_files()) is not cached: a
      # cached copy would be replayed on every later run, turning a page
      # that failed once into files that are never seen again.
      if (isTRUE(cache) && .repo_info_ok(id_info[[i]]) &&
          !isTRUE(id_info[[i]]$files_incomplete))
        .repo_info_cache_put("dryad", doi, id_info[[i]])
    }
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "dryad_url") |>
    dplyr::left_join(info, by = "dryad_doi", suffix = c("", ".dryad"))
  data <- add_share_error(data)

  paste0("...Dryad retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one Dryad dataset
#'
#' @param dryad_doi the dataset's DOI (e.g. "10.5061/dryad.j1fd7")
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information, including `storage_size`,
#'   `files_total` and `files_incomplete` (see [dryad_info()])
#' @keywords internal
.dryad_info <- function(dryad_doi, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  .dryad_warn_if_no_key()

  paste0("* Retrieving info from Dryad DOI ", dryad_doi, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(dryad_doi = dryad_doi)

  # Both the "doi:" colon and the DOI's own slash must be percent-encoded in
  # the path (verified live against doi:10.5061/dryad.j1fd7, reachable only as
  # .../datasets/doi%3A10.5061%2Fdryad.j1fd7).
  encoded <- utils::URLencode(paste0("doi:", dryad_doi), reserved = TRUE)
  api_url <- sprintf("https://datadryad.org/api/v2/datasets/%s", encoded)

  resp <- .batch_query(api_url, msg = NULL, req_func = .dryad_headers)[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(dryad_doi, " could not be found on Dryad", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  rec <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
  if (is.null(rec)) {
    obj$error <- "parse_error"
    return(obj)
  }

  authors_field <- rec$authors
  authors <- if (is.list(authors_field)) {
    # %empty_or% (not %||%) because a$firstName/a$lastName can come back as
    # length-zero values rather than NULL; vapply(..., character(1))
    # requires exactly length 1 from every call.
    vapply(authors_field, function(a) {
      full <- trimws(paste(a$firstName %empty_or% "", a$lastName %empty_or% ""))
      if (nzchar(full)) full else NA_character_
    }, character(1))
  } else {
    character(0)
  }

  # The files listing sits under the dataset's VERSION, not the dataset
  # itself (verified live: dataset JSON links to "stash:version" ->
  # /api/v2/versions/<id>, and only THAT resource links on to "stash:files").
  # The listing is paged -- see .dryad_list_files() for why every page has
  # to be followed.
  version_href <- rec$`_links`$`stash:version`$href
  files_list <- list()
  incomplete <- NULL
  if (!is.null(version_href)) {
    files_list <- .dryad_list_files(paste0(version_href, "/files"), dryad_doi = dryad_doi)
    incomplete <- attr(files_list, "dryad_incomplete")
  }

  # Scalar fields use %empty_or% (not %||%) because a JSON field can come
  # back as a length-zero value (e.g. an empty array) rather than NULL; %||%
  # would let that through unchanged and break the $<- assignment above with
  # "replacement has 0 rows". List-wrapped fields (authors, files) don't
  # need it: wrapping in list() always yields length 1.
  obj$title <-            rec$title %empty_or% NA_character_
  obj$doi <-              rec$identifier %empty_or% NA_character_
  obj$publication_date <- rec$publicationDate %empty_or% NA_character_
  obj$updated_date <-     rec$lastModificationDate %empty_or% NA_character_
  obj$authors <-          list(authors)
  obj$license <-          rec$license %empty_or% NA_character_
  obj$files <-            list(files_list)

  # storageSize is the dataset's total size in bytes as Dryad itself reports
  # it, independent of the file listing (verified 2026-10-13 on
  # doi:10.5061/dryad.9ghx3ffgc: storageSize 756438 equals the sum of its 22
  # listed file sizes exactly). Kept so the whole-dataset zip can be sized
  # without summing a listing that may be incomplete. files_total is the
  # number of files Dryad says the version holds (the listing's own
  # `total`), and files_incomplete is TRUE when fewer than that arrived --
  # the same check osf_get_all_pages() makes with its osf_incomplete
  # attribute.
  obj$storage_size <-     as.numeric(rec$storageSize %empty_or% NA_real_)
  obj$files_total <-      as.numeric(attr(files_list, "dryad_total") %empty_or% NA_real_)
  obj$files_incomplete <- !is.null(incomplete)

  return(obj)
}

# List every file of one Dryad dataset version, following the listing's
# pages to the end.
#
# /api/v2/versions/<id>/files is paged: without a per_page parameter Dryad
# returns only the first 20 files (its DEFAULT_PAGE_SIZE), with the rest
# behind `_links.next`. Reading only that first page silently dropped every
# later file -- verified 2026-10-13 on a real corpus, where 18 datasets
# listed at exactly 20 files really held 21 to 185 files each (645 files
# missed in all; doi:10.5061/dryad.wm37pvmkm has 185). Each page is
# requested at per_page = 100 (the largest Dryad accepts), so a dataset of
# up to 100 files costs one request, and `_links.next` is followed until it
# is absent. Every page carries `total` (the version's file count) and the
# `_links.next` href is relative ("/api/v2/versions/104509/files?page=2&
# per_page=10", verified 2026-10-13 on doi:10.5061/dryad.9ghx3ffgc).
#
# When fewer files arrive than `total` reports (a later page failed, e.g. on
# Dryad's rate limit), the listing is returned as far as it got, with a
# warning and a `dryad_incomplete` attribute c(expected =, got =), mirroring
# osf_get_all_pages()'s `osf_incomplete`. A first page that fails
# altogether is reported the same way (expected = NA), since an empty list
# would otherwise read as a dataset with no files.
#
# @param files_href the listing's path, "/api/v2/versions/<id>/files"
# @param dryad_doi the dataset's DOI, used only in the warning
# @param per_page files per page; 100 is Dryad's maximum
# @returns a list of file records (as in `_embedded$stash:files`), with a
#   `dryad_total` attribute (NULL when Dryad reported none) and, when the
#   listing is short, a `dryad_incomplete` attribute
.dryad_list_files <- function(files_href, dryad_doi = NA_character_, per_page = 100) {
  .dryad_abs <- function(href) {
    if (grepl("^https?://", href)) href else paste0("https://datadryad.org", href)
  }
  url <- paste0(.dryad_abs(files_href),
                if (grepl("\\?", files_href)) "&" else "?",
                "per_page=", per_page)

  files <- list()
  total <- NULL
  failed <- FALSE
  seen <- character(0)
  while (!is.null(url)) {
    # A `next` link pointing back at a page already read would loop forever.
    if (url %in% seen) break
    seen <- c(seen, url)

    resp <- .batch_query(url, msg = NULL, req_func = .dryad_headers)[[1]]
    if (is.null(resp) || httr2::resp_status(resp) != 200) {
      failed <- TRUE
      break
    }
    rec <- tryCatch(httr2::resp_body_json(resp), error = \(e) NULL)
    if (is.null(rec)) {
      failed <- TRUE
      break
    }
    total <- total %||% rec$total
    files <- c(files, rec$`_embedded`$`stash:files` %||% list())
    next_href <- rec$`_links`$`next`$href
    url <- if (is.null(next_href) || !nzchar(next_href)) NULL else .dryad_abs(next_href)
  }

  total <- suppressWarnings(as.numeric(total %empty_or% NA_real_))
  short <- if (is.na(total)) failed else length(files) < total
  attr(files, "dryad_total") <- if (is.na(total)) NULL else total
  if (short) {
    warning(sprintf(
      "Dryad listed only %d of %s files for %s. Files not listed are not retrieved, so this listing is incomplete. Run again later (Dryad limits requests; see ?dryad_auth, which raises the limit).",
      length(files), if (is.na(total)) "its" else format(total), dryad_doi),
      call. = FALSE)
    attr(files, "dryad_incomplete") <- c(expected = total, got = length(files))
  }
  files
}

# Add the right auth header to a Dryad API/download request. Dryad uses a
# single OAuth2 bearer token (unlike Dataverse's per-installation key).
#
# Dataset METADATA and file LISTINGS are readable without a token, even for a
# public dataset -- verified live against doi:10.5061/dryad.j1fd7. Downloading
# FILE BYTES is not: both /api/v2/files/<id>/download and the dataset-level
# /api/v2/datasets/<doi>/download bulk endpoint answered 401 with no token,
# even for that same fully public dataset (verified live 2026-08-16). This
# matches Dryad's own documentation ("Some API calls, such as ... downloading
# files, require a token for any call" -- api_accounts.md), so it is expected
# behaviour, not a bug: a token is REQUIRED here, unlike Figshare/Dataverse/
# Zenodo/OSF where a token is optional for public content.
#
# When client-credentials (dryad_auth()) are set, req_oauth_client_credentials()
# handles the whole token fetch/cache/automatic-refresh cycle: it exchanges the
# client id/secret for an access token on first use, caches it in memory, and
# transparently fetches a new one once the cached token is within
# expiry_margin seconds of expiring -- so a run spanning many hours never
# hits the dryad_pat() 10-hour manual-token expiry (see dryad_auth()). Falls
# back to the static dryad_pat() bearer header when no client is set, for
# callers who only ever generated a plain token.
#
# A token set in this session with dryad_pat() (a visitor's own key in the
# metacheck apps, see api_keys_use()) comes before client credentials from
# the DRYAD_CLIENT_ID/DRYAD_CLIENT_SECRET environment variables (the server's
# key); client credentials set in this session with dryad_auth() still come
# first.
.dryad_headers <- function(req) {
  req <- req |> httr2::req_headers(`User-Agent` = "metacheck")
  session_client <- nzchar(getOption("metacheck.dryad.client_id") %||% "")
  session_pat <- getOption("metacheck.dryad.pat") %||% ""
  if (!session_client && nzchar(session_pat)) {
    return(req |> httr2::req_headers(Authorization = sprintf("Bearer %s", session_pat)))
  }
  client <- tryCatch(.dryad_oauth_client(), error = \(e) NULL)
  if (!is.null(client)) {
    return(req |> httr2::req_oauth_client_credentials(client))
  }
  pat <- tryCatch(dryad_pat(), error = \(e) "")
  if (nzchar(pat %||% "")) {
    req <- req |> httr2::req_headers(Authorization = sprintf("Bearer %s", pat))
  }
  req
}

#' Set or get a Dryad API token
#'
#' Dryad issues OAuth2 bearer tokens from account settings. Unlike Figshare,
#' Dataverse, Zenodo, and OSF, a token is REQUIRED to download file bytes even
#' from a fully public dataset (verified live 2026-08-16: both the per-file
#' and whole-dataset download endpoints answer 401 with no token, though
#' dataset metadata and file listings are readable without one). Without a
#' token, [dryad_file_download()] can list what a dataset contains but cannot
#' fetch any of it.
#'
#' This token expires after 10 hours and must be regenerated by hand from
#' Dryad's account settings -- for a long corpus run, [dryad_auth()] avoids
#' this by exchanging a client id/secret for a token automatically, with no
#' manual re-entry. When client credentials are set via [dryad_auth()], they
#' take priority over this token.
#'
#' Store it as the `DRYAD_PAT` environment variable so it is read every time
#' R starts.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' dryad_pat() # returns "" unless a token is set
dryad_pat <- function(pat = NULL) {
  .dryad_pat(pat)
}

.dryad_pat <- function(pat = NULL) {
  opt <- "metacheck.dryad.pat"
  env <- "DRYAD_PAT"

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set dryad_pat with a single string containing your Dryad token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Set or get Dryad OAuth2 client credentials
#'
#' Dryad also issues a client id/secret pair (from the same account settings
#' page as the plain token in [dryad_pat()]), meant for the OAuth2
#' client-credentials grant. Unlike a plain token, which expires after 10
#' hours and must be regenerated by hand, a client id/secret is exchanged for
#' a short-lived access token automatically -- and refreshed automatically,
#' for as long as the credentials themselves stay valid -- every time a
#' request needs one, via `httr2::req_oauth_client_credentials()`. This is the
#' right choice for a long-running corpus build; [dryad_pat()] remains
#' available for a single quick session where generating a client id/secret
#' is not worth the extra step.
#'
#' Store `client_id`/`client_secret` as the `DRYAD_CLIENT_ID`/
#' `DRYAD_CLIENT_SECRET` environment variables so they are read every time R
#' starts, the same way [dryad_pat()] reads `DRYAD_PAT`.
#'
#' @param client_id the OAuth2 client id, or NULL to get the current client
#' @param client_secret the OAuth2 client secret, required when `client_id`
#'   is supplied
#' @param token_url the token endpoint; Dryad's documented default should
#'   not normally need overriding
#'
#' @returns invisibly, the `httr2::oauth_client()` built from the current
#'   credentials, or `NULL` when none are set
#' @export
#'
#' @examples
#' \dontrun{
#' dryad_auth("my-client-id", "my-client-secret")
#' }
dryad_auth <- function(client_id = NULL, client_secret = NULL,
                       token_url = "https://datadryad.org/oauth/token") {
  .dryad_auth(client_id, client_secret, token_url)
}

.dryad_auth <- function(client_id = NULL, client_secret = NULL,
                        token_url = "https://datadryad.org/oauth/token") {
  opt_id     <- "metacheck.dryad.client_id"
  opt_secret <- "metacheck.dryad.client_secret"
  opt_url    <- "metacheck.dryad.token_url"
  env_id     <- "DRYAD_CLIENT_ID"
  env_secret <- "DRYAD_CLIENT_SECRET"

  if (is.null(client_id)) {
    id     <- getOption(opt_id) %||% Sys.getenv(env_id)
    secret <- getOption(opt_secret) %||% Sys.getenv(env_secret)
    url    <- getOption(opt_url) %||% token_url
    if (!nzchar(id %||% "") || !nzchar(secret %||% "")) return(invisible(NULL))
    return(invisible(httr2::oauth_client(id = id, token_url = url, secret = secret)))
  }

  if (!is.character(client_id) || length(client_id) != 1 || is.na(client_id) ||
      is.null(client_secret) || !is.character(client_secret) ||
      length(client_secret) != 1 || is.na(client_secret)) {
    stop("Set dryad_auth with a single client_id string and a single ",
         "client_secret string", call. = FALSE)
  }
  options(setNames(list(client_id, client_secret, token_url),
                   c(opt_id, opt_secret, opt_url)))
  invisible(httr2::oauth_client(id = client_id, token_url = token_url,
                                secret = client_secret))
}

# The oauth_client built from whatever credentials dryad_auth() currently
# holds, or NULL when none are set -- checked fresh on every request (see
# .dryad_headers()) rather than cached once, so credentials set or changed
# mid-session take effect on the very next Dryad request.
.dryad_oauth_client <- function() {
  .dryad_auth()
}

# Warn, once per session, the first time a Dryad dataset is resolved with no
# key set (dryad_pat() nor dryad_auth()). Dataset metadata/listing works
# without one (see below), so this is shown here rather than failing outright
# -- but every later file-byte download for that dataset will 401 (see
# .dryad_headers()'s own comment), and surfacing that only after the fact, one
# failed download at a time, means a whole corpus run can finish before the
# user learns why every Dryad file came back empty. Shown at most once per
# session, same pattern as the repo-cache notice in download_repo_files();
# reset with options(metacheck.dryad_pat.notified = NULL) to see it again.
.dryad_warn_if_no_key <- function() {
  if (isTRUE(getOption("metacheck.dryad_pat.notified"))) return(invisible())
  has_key <- nzchar(tryCatch(dryad_pat(), error = \(e) "") %||% "") ||
    !is.null(tryCatch(.dryad_oauth_client(), error = \(e) NULL))
  if (!has_key) {
    message(
      "No Dryad API key is set. Dryad dataset listings and metadata can be ",
      "read without one, but downloading file CONTENTS from Dryad requires ",
      "a key even for a fully public dataset -- every such download will ",
      "fail with 'HTTP 401' until one is set. See ?dryad_pat or ?dryad_auth."
    )
  }
  options(metacheck.dryad_pat.notified = TRUE)
}

# Fetch one Dryad file's bytes to `target_path`. Extracted out of
# dryad_file_download()'s per-file loop (issue #456.6) so the 401-vs-other-
# failure distinction can be tested in isolation, with a single plain
# req_perform() call and no retry/sequential machinery to mock.
#
# @param self_url the file's own download URL (`files$self`)
# @param target_path where to write the bytes on success
# @returns `list(ok = TRUE)`, or `list(ok = FALSE, error = <string or NULL>)`
#   -- `error` is NULL when the request itself failed (network/timeout, no
#   response at all), since there is nothing more specific to report there.
.dryad_download_one_file <- function(self_url, target_path) {
  resp <- tryCatch(
    {
      httr2::request(self_url) |>
        .dryad_headers() |>
        httr2::req_timeout(600) |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    },
    error = \(e) NULL
  )
  if (is.null(resp)) return(list(ok = FALSE, error = NULL))
  if (httr2::resp_status(resp) == 200) {
    writeBin(.resp_bytes(resp), target_path)
    return(list(ok = TRUE))
  }
  # A 401 here is otherwise indistinguishable from any other download failure
  # once this file ends up staying "unknown" downstream -- see
  # .dryad_401_hint()'s own comment and issue #456.6, which flagged exactly
  # this confusion for the download_repo_files() path that repo_check/
  # data_check actually use. Recorded here too so this standalone export
  # reports the same clear cause.
  status <- httr2::resp_status(resp)
  list(ok = FALSE, error = paste0("HTTP ", status, .dryad_401_hint(self_url, status)))
}

#' Download all files from a Dryad dataset
#'
#' Creates a directory for the dataset and downloads all of its files.
#' Returns (invisibly) a data frame with file info.
#'
#' A Dryad API token (see [dryad_pat()]) is REQUIRED to download file bytes,
#' even from a fully public dataset -- unlike [figshare_file_download()],
#' [dataverse_file_download()], or [zenodo_file_download()]. Without one set,
#' every file in the returned table has `downloaded = FALSE`: the dataset can
#' still be listed (see [dryad_info()]), but nothing can be fetched.
#'
#' You can limit downloads to only files under a specific size (defaults to
#' 10MB) and only a maximum download size (largest files will be omitted until
#' total size is under the limit). Omitted files will be listed as messages in
#' verbose mode, and included in the returned data frame with the `downloaded`
#' column value set to FALSE.
#'
#' A `.zip` in the dataset is normally fetched whole and left as a zip. Set
#' `unzip_types` to pull only the files you want out of it instead, the same
#' as [dataverse_file_download()] and [zenodo_file_download()] do.
#'
#' @param dryad_doi the dataset's DOI, or a URL containing it
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
#'   the members rather than the archive Dryad published a size and digest for.
#' @export
#'
#' @examples
#' \dontrun{
#'   dryad_file_download("10.5061/dryad.j1fd7")
#'
#'   # take only the data files out of any zip in the dataset
#'   dryad_file_download("10.5061/dryad.j1fd7", unzip_types = "data")
#' }
dryad_file_download <- function(dryad_doi,
                                download_to = ".",
                                max_file_size = 10,
                                max_download_size = 100,
                                unzip_types = NULL,
                                pb = NULL) {
  dryad_doi <- .dryad_doi(dryad_doi) |>
    stats::na.omit() |>
    unique()
  if (length(dryad_doi) == 0) return(NULL)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Dryad File Download"))
    on.exit(pb$terminate())
  }

  # --- iterate over multiple datasets ----
  if (length(dryad_doi) > 1) {
    paste0(
      "Starting downloads for ", length(dryad_doi), " Dryad dataset",
      ifelse(length(dryad_doi) == 1, "", "s"), "...\n"
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)

    dl_list <- lapply(dryad_doi, function(x) {
      tryCatch(
        dryad_file_download(
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
      "...Completed downloads for ", length(dryad_doi), " Dryad dataset",
      ifelse(length(dryad_doi) == 1, "", "s")
    ) |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dl)
  }

  # --- retrieve dataset contents ----
  paste0("Starting retrieval for ", dryad_doi) |>
    list(what = _) |>
    pb$tick(0, tokens = _)
  contents <- suppressMessages(.dryad_info(dryad_doi, pb = pb))

  files_list <- list()
  if ("files" %in% names(contents) && nrow(contents) > 0) {
    files_list <- contents$files[[1]]
  }

  if (is.null(files_list) || length(files_list) == 0) {
    paste0("- ", dryad_doi, " contained no files") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(NULL)
  }

  # Build a flat table from entries (verified live field names: path, size,
  # digest, digestType, and _links.self.href / _links["stash:download"].href
  # -- see .dryad_info()). The file id is the numeric segment of
  # _links.self.href (".../api/v2/files/<id>"), there being no separate id
  # field in the response.
  # %empty_or% (not %||%) because a field can come back as a length-zero
  # value rather than NULL; %||% letting that through would break
  # dplyr::tibble()'s recycling (or the !is.na() checks below, which error
  # with "argument is of length zero" on a length-zero input).
  rows <- lapply(files_list, function(x) {
    self_href <- x$`_links`$self$href %empty_or% NA_character_
    dl_href <- x$`_links`$`stash:download`$href %empty_or% NA_character_
    file_id <- if (!is.na(self_href))
      sub("^.*/([0-9]+)$", "\\1", self_href) else NA_character_
    digest_type <- tolower(x$digestType %empty_or% NA_character_)
    dplyr::tibble(
      id       = file_id,
      key      = x$path %empty_or% NA_character_,
      size     = as.numeric(x$size %empty_or% NA_real_),
      checksum = x$digest %empty_or% NA_character_,
      checksum_type = digest_type,
      self     = if (!is.na(dl_href)) paste0("https://datadryad.org", dl_href)
                 else NA_character_
    )
  })

  files <- dplyr::bind_rows(rows)
  if (nrow(files) == 0) {
    message("- ", dryad_doi, " contained no files")
    return(NULL)
  }

  # A zip we were asked to look inside is exempt from the size caps below, for
  # the same reason zenodo_file_download() exempts it: the caps measure the
  # file Dryad would send, and for these we have decided not to transfer
  # that -- only selected members are fetched, each capped individually by
  # `max_file_size` at extraction time.
  unzippable <- rep(FALSE, nrow(files))
  if (!is.null(unzip_types) && length(unzip_types) > 0)
    unzippable <- .is_zip(files$key) & !is.na(files$self) & nzchar(files$self %||% "")

  # --- size filters (MB) ----
  # Omitted files stay in the table with downloaded = FALSE, as documented;
  # they were dropped from it (U36).
  omit <- .omit_for_size(files$size, files$key, unzippable,
                         max_file_size, max_download_size, pb)

  if (all(omit)) {
    paste0("- All files omitted due to size constraints") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(invisible(.all_omitted_table(files, list(dryad_doi = dryad_doi))))
  }

  # --- target directory (avoid overwrite) ----
  download_to <- normalizePath(download_to, winslash = "/", mustWork = FALSE)
  folder_name <- gsub("[^A-Za-z0-9._-]+", "_", dryad_doi)
  if (dir.exists(download_to)) {
    download_to <- file.path(download_to, folder_name)
  }
  # the base name is kept as it is: stripping "_<digits>" on each pass also
  # stripped the deposit id of "figshare_<id>", so the next download went to
  # "figshare_1" instead of "figshare_<id>_1" (U37)
  i <- 0L
  base <- download_to
  while (dir.exists(download_to)) {
    i <- i + 1L
    download_to <- paste0(base, "_", i)
  }
  dir.create(download_to, showWarnings = FALSE, recursive = FALSE)
  paste0("- Created directory ", download_to) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # --- download into temp, then copy to target ----
  temppath <- tempfile()
  on.exit(unlink(temppath, recursive = TRUE), add = TRUE)
  dir.create(temppath)

  n <- nrow(files)
  files$downloaded <- FALSE
  files$extracted <- NA_integer_
  failed <- data.frame(key = character(0), member = character(0),
                       error = character(0), stringsAsFactors = FALSE)

  for (i in seq_len(n)) {
    if (omit[i]) next   # left out for its size (U36)
    # --- selected members out of a zip, instead of the whole zip ----
    if (unzippable[i]) {
      paste0("Reading zip contents of ", files$key[[i]]) |>
        list(what = _) |>
        pb$tick(0, tokens = _)
      got <- tryCatch(
        .dryad_zip_members(files$self[[i]], dest = download_to,
                          keep_types = unzip_types,
                          max_file_size = max_file_size,
                          size = if ("size" %in% names(files)) files$size[[i]] else NULL),
        error = \(e) NULL)
      if (!is.null(got)) {
        files$extracted[i] <- sum(got$ok %in% TRUE)
        files$downloaded[i] <- TRUE
        paste0("- extracted ", files$extracted[i], " file",
               plural(files$extracted[i]), " from ", files$key[[i]]) |>
          list(what = _) |>
          pb$tick(0, tokens = _)
        # A member that failed to extract left a row with ok == FALSE; report
        # what actually went wrong instead of only counting successes, so a
        # transient failure worth retrying can be told apart from one that
        # will not resolve on its own (#429).
        bad <- got[!(got$ok %in% TRUE), , drop = FALSE]
        if (nrow(bad) > 0) {
          failed <- rbind(failed, data.frame(
            key = files$key[[i]], member = bad$name, error = bad$error,
            stringsAsFactors = FALSE))
          for (j in seq_len(nrow(bad)))
            paste0("  - failed to extract ", bad$name[j], ": ", bad$error[j]) |>
              list(what = _) |>
              pb$tick(0, tokens = _)
        }
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
      got <- .dryad_download_one_file(files$self[[i]], target_path)
      ok <- got$ok
      if (!ok && !is.null(got$error)) {
        failed <- rbind(failed, data.frame(
          key = files$key[[i]], member = NA_character_, error = got$error,
          stringsAsFactors = FALSE))
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
  files <- .dryad_verify_downloads(files, download_to)

  n_missing <- sum(!files$downloaded & !omit)   # omitted files were never meant to arrive
  if (n_missing > 0) {
    worst <- files[!files$downloaded & !omit, ]
    warning(sprintf(
      "%d of %d file%s from Dryad dataset %s did not arrive intact (e.g. %s). The returned table marks %s downloaded = FALSE. Run again to retry.",
      n_missing, nrow(files), plural(nrow(files)), dryad_doi,
      paste(utils::head(worst$key, 3), collapse = ", "),
      if (n_missing == 1) "it" else "them"), call. = FALSE)
  }

  # --- return table ----
  files$folder <- basename(download_to)
  files$dryad_doi <- dryad_doi
  files <- files[, c("folder", "dryad_doi", "id", "key", "path", "size",
                     "size_on_disk", "checksum", "checksum_ok", "self",
                     "downloaded", "extracted")]
  attr(files, "failed") <- failed

  invisible(files)
}

#' Fetch selected files out of a .zip in a Dryad dataset without downloading it
#'
#' A zip published on Dryad is often mostly material a reader does not need:
#' stimuli, images, or videos alongside the few data files. Dryad's download
#' links support byte-range requests and every member of a zip is compressed
#' on its own, so individual members can be pulled out of the archive while
#' the rest is never transferred (see `.zip_member_fetch()`, shared with the
#' identical Zenodo/Dataverse/Figshare path).
#'
#' @param url the file's download URL
#' @param dest directory to write the extracted members into
#' @param keep_types file categories worth extracting, as
#'   [data_classify_files()] names them; the default keeps data and
#'   documentation and skips materials, matching [zip_decision()]
#' @param max_file_size largest member to extract (in MB), applied per member
#'   exactly as the dataset-level cap is applied per file
#'
#' @param size the zip's size in bytes as the listing gives it, or `NULL`.
#'   Passed on to [zip_peek()] so it does not have to send a `HEAD` request to
#'   learn a size the listing already gave.
#'
#' @returns a data frame with one row per extracted member (`name`, `path`,
#'   `size`, `ok`), or `NULL` when the archive could not be listed and the
#'   caller should download it whole instead
#' @keywords internal
.dryad_zip_members <- function(url, dest,
                               keep_types = c("data", "documentation"),
                               max_file_size = 10, size = NULL) {
  listing <- tryCatch(zip_peek(url, size = size), error = \(e) NULL)
  if (is.null(listing) || nrow(listing) == 0) return(NULL)

  types <- data_classify_files(basename(listing$name))
  keep <- types %in% keep_types

  if (!is.null(max_file_size) && is.finite(max_file_size) && max_file_size > 0)
    keep <- keep & !is.na(listing$size) &
      listing$size <= max_file_size * 1024 * 1024

  if (!any(keep)) return(listing[0, c("name", "size"), drop = FALSE])

  .zip_fetch_members(url, names = listing$name[keep], dest = dest)
}

#' Check downloaded Dryad files against the file system
#'
#' Mirrors [`.dataverse_verify_downloads()`]: confirms every file the
#' download planned to save is present, is the size Dryad reported, and (when
#' Dryad published an MD5 or SHA-256 digest) matches the checksum.
#'
#' A row whose `extracted` count is set is exempt from both checks, for the
#' same reason as the Dataverse case: Dryad's size and digest describe the
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
.dryad_verify_downloads <- function(files, download_to) {
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

  # Dryad's digestType is published per file, and both "md5" (seen on 3 files
  # of an older dataset) and "sha-256" occur: every one of the 22 files of
  # doi:10.5061/dryad.9ghx3ffgc reports "sha-256" with a 64-character hex
  # digest (verified 2026-10-13). Only MD5 used to be checked, so SHA-256
  # files were never verified at all. The SHA-256 is computed with
  # .bibr12_sha256() (tools::sha256sum() on R >= 4.5, else the digest
  # package when installed); where neither is available, checksum_ok stays
  # NA for those files -- not verified, rather than marked as failed.
  type <- tolower(gsub("[^A-Za-z0-9]", "", files$checksum_type %||% rep(NA_character_, nrow(files))))
  has_sum <- !is.na(files$checksum)
  is_md5 <- !is.na(type) & type == "md5" & has_sum &
    grepl("^[0-9a-f]{32}$", files$checksum, ignore.case = TRUE)
  is_sha256 <- !is.na(type) & type == "sha256" & has_sum &
    grepl("^[0-9a-f]{64}$", files$checksum, ignore.case = TRUE)
  for (i in which(ok & (is_md5 | is_sha256))) {
    got <- tryCatch(
      if (is_md5[[i]]) unname(tools::md5sum(full[[i]])) else .bibr12_sha256(full[[i]]),
      error = \(e) NA_character_)
    if (is.na(got)) next
    files$checksum_ok[[i]] <- identical(tolower(got), tolower(files$checksum[[i]]))
  }
  ok <- ok & !(files$checksum_ok %in% FALSE)

  files$downloaded <- ok & files$downloaded %in% TRUE

  if (any(unzipped)) files$downloaded[unzipped] <- TRUE
  files
}
