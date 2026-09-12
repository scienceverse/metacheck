# Mendeley Data (https://data.mendeley.com) is a single hosted service run by
# Elsevier, the same relationship Figshare has to its own institutional
# "branded" pages (see archive-figshare.R) -- an institution can run a
# white-labelled front end on its own domain (e.g. Washington University's
# "Digital Commons Data", digitalcommonsdata.wustl.edu) while the data itself
# lives on the same backend: verified live 2026-09-12, that front end's own
# /public-api/datasets/<id> answered with a real Mendeley Data dataset (not
# one belonging to that institution), proving the API is host-independent.
# Detection therefore keys off the DOI prefix (10.17632, always resolves via
# data.mendeley.com regardless of which domain a paper's citation used)
# rather than a host list, unlike Dataverse/DSpace's many independent
# installations.
#
# Every field name below (name/doi/contributors/data_licence/files/
# content_details) was read from a real response, not guessed from
# documentation -- verified live 2026-09-12 against dataset vjtxybrc28
# (GET https://data.mendeley.com/public-api/datasets/vjtxybrc28).
#
# Known API quirk: passing a `version` query parameter (even the dataset's
# only/current version) makes the `files` array come back empty -- confirmed
# live 2026-09-12 across three different real datasets. This looks like a bug
# in Mendeley's own API rather than anything specific to a particular
# dataset, but it means this integration can only reliably list files for a
# dataset's CURRENT version, not a specific older version a paper's DOI might
# cite (data.mendeley.com/datasets/<id>/<version>) -- .mendeley_id() below
# therefore drops any version suffix, the same way .figshare_id() does for
# Figshare's own version suffix.

#' Find Mendeley Data Links in Papers
#'
#' Get all Mendeley Data links: real hyperlinks from the paper's own `url`
#' table, plus a body-text fallback for a BARE mention (a DOI like
#' "10.17632/vjtxybrc28.1" is routinely cited without any URL scheme at all)
#' that the source PDF/HTML never encoded as an actual hyperlink -- the `url`
#' table only ever contains links the source document itself made clickable.
#' Same two-tier approach `zenodo_links()` uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the Mendeley Data url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' mendeley_links(psychsci)
#' }
mendeley_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("data\\.mendeley\\.com|10\\.17632/", href, ignore.case = TRUE))

  mendeley_bare_regex <- paste0(
    "(?:https?://)?data\\.mendeley\\.com/datasets/[A-Za-z0-9]+(?:/[0-9]+)?",
    "|(?:https?://)?(?:doi\\.org/)?10\\.17632/[A-Za-z0-9]+(?:\\.[0-9]+)?"
  )
  other_mendeley <- text_search(paper, mendeley_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_mendeley) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$mendeley_url <- links$href
  links$mendeley_id <- .mendeley_id(links$mendeley_url)
  links$mendeley_link <- ifelse(
    is.na(links$mendeley_id),
    NA_character_,
    paste0("https://doi.org/10.17632/", links$mendeley_id)
  )

  return(links)
}

#' Get Mendeley Data dataset ID from URL or DOI
#'
#' @param mendeley_url a vector of URLs or DOIs to Mendeley Data datasets
#'
#' @returns a character vector of dataset IDs
#' @keywords internal
.mendeley_id <- function(mendeley_url) {
  if (length(mendeley_url) == 0) {
    return(character(0))
  }

  if (length(mendeley_url) > 1) {
    return(vapply(mendeley_url, .mendeley_id, character(1)))
  }

  # handle single mendeley_url ----
  mendeley_url <- trimws(as.character(mendeley_url))

  if (is.na(mendeley_url) || !nzchar(mendeley_url)) {
    return(NA_character_)
  }

  if (grepl("^[A-Za-z0-9]+$", mendeley_url)) {
    return(mendeley_url)
  }

  # The version suffix (DOI's trailing ".<n>", or the URL's trailing
  # "/<n>") is dropped -- see the file-level note above on why only the
  # current version's files can be listed at all.
  patterns <- c(
    "10\\.17632/([A-Za-z0-9]+)(?:\\.[0-9]+)?",
    "data\\.mendeley\\.com/datasets/([A-Za-z0-9]+)(?:/[0-9]+)?"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, mendeley_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(mendeley_url, match)[[1]]
    if (length(groups) >= 2) {
      return(groups[[2]])
    }
  }

  return(NA_character_)
}

#' Retrieve info from Mendeley Data by URL
#'
#' @param mendeley_url a Mendeley Data URL or DOI, or a table containing them
#'   (e.g., as created by [mendeley_links()])
#' @param id_col the index or name of the column that contains Mendeley Data
#'   URLs, if `mendeley_url` is a table
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a previously cached listing for a dataset
#'   already looked up (see [repo_info_cache()]) instead of re-querying the
#'   API. Off by default.
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   mendeley_info("https://doi.org/10.17632/vjtxybrc28.1")
#' }
mendeley_info <- function(mendeley_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (!online("data.mendeley.com")) {
    stop("data.mendeley.com seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "Mendeley Data Retrieve"))
    on.exit(pb$terminate())
  }

  if (is.data.frame(mendeley_url)) {
    table <- mendeley_url
    table$mendeley_url <- table[[id_col]]
  } else {
    raw_urls <- unique(mendeley_url) |> stats::na.omit()
    table <- data.frame(mendeley_url = raw_urls)
  }

  ids <- data.frame(
    mendeley_url = table$mendeley_url,
    mendeley_id = .mendeley_id(table$mendeley_url)
  ) |>
    unique()
  ids <- ids[!is.na(ids$mendeley_url), , drop = FALSE]
  valid_ids <- unique(stats::na.omit(ids$mendeley_id))

  if (length(valid_ids) == 0) {
    ("No valid Mendeley Data links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "mendeley_url"))
  }

  paste0(
    "Starting Mendeley Data retrieval for ",
    length(valid_ids), " dataset",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- vector("list", length(valid_ids))
  for (i in seq_along(valid_ids)) {
    id <- valid_ids[[i]]
    cached <- if (isTRUE(cache)) .repo_info_cache_get("mendeley", id) else NULL
    if (!is.null(cached)) {
      id_info[[i]] <- cached
    } else {
      id_info[[i]] <- .mendeley_info(id, pb = pb)
      if (isTRUE(cache) && .repo_info_ok(id_info[[i]]))
        .repo_info_cache_put("mendeley", id, id_info[[i]])
    }
  }

  info <- do.call(dplyr::bind_rows, id_info)

  data <- table |>
    dplyr::left_join(ids, by = "mendeley_url") |>
    dplyr::left_join(info, by = "mendeley_id", suffix = c("", ".mendeley"))

  paste0("...Mendeley Data retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from one Mendeley Data dataset
#'
#' @param mendeley_id a Mendeley Data dataset ID
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @keywords internal
.mendeley_info <- function(mendeley_id, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from Mendeley Data ", mendeley_id, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  obj <- data.frame(mendeley_id = mendeley_id)

  # No `version` query param -- see the file-level note above: adding one
  # (even the dataset's only version) makes the API's own `files` array come
  # back empty, so this always reads the CURRENT version's files.
  api_url <- paste0("https://data.mendeley.com/public-api/datasets/", mendeley_id)

  resp <- .batch_query(api_url, msg = NULL)[[1]]

  if (is.null(resp) || httr2::resp_status(resp) != 200) {
    warning(mendeley_id, " could not be found", call. = FALSE)
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

  authors_field <- rec$contributors
  authors <- if (is.list(authors_field)) {
    # %empty_or% (not %||%) because first_name/last_name can come back as
    # length-zero values rather than NULL; vapply(..., character(1))
    # requires exactly length 1 from every call.
    vapply(authors_field, function(a) {
      full <- trimws(paste(a$first_name %empty_or% "", a$last_name %empty_or% ""))
      if (nzchar(full)) full else NA_character_
    }, character(1))
  } else {
    character(0)
  }

  # Scalar fields use %empty_or% (not %||%) because a JSON field can come
  # back as a length-zero value (e.g. an empty array) rather than NULL; %||%
  # would let that through unchanged and break the $<- assignment below with
  # "replacement has 0 rows". List-wrapped fields (authors, files) don't
  # need it: wrapping in list() always yields length 1.
  obj$title <-            rec$name %empty_or% NA_character_
  obj$doi <-              rec$doi$id %empty_or% NA_character_
  obj$description <-      rec$description %empty_or% NA_character_
  obj$publication_date <- rec$publish_date %empty_or% NA_character_
  obj$updated_date <-     rec$modified_on %empty_or% NA_character_
  obj$authors <-          list(authors)
  obj$license <-          rec$data_licence$short_name %empty_or%
                          rec$data_licence$full_name %empty_or%
                          NA_character_
  obj$files <-            list(rec$files %||% list())

  return(obj)
}
