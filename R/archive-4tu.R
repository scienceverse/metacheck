# 4TU.ResearchData (https://data.4tu.nl) is the Dutch national repository for
# science/engineering/design research data, run jointly by TU Delft,
# TU Eindhoven, University of Twente, and Wageningen University. Its
# repository software, Djehuty (github.com/4TUResearchData/djehuty), is a
# documented backward-compatible implementation of the Figshare v2 API --
# verified live 2026-08-16: GET data.4tu.nl/v2/articles/<id> returns the
# identical field set (title/doi/authors/license/files, files[].id/name/size/
# download_url/computed_md5) as api.figshare.com/v2/articles/<id>, just
# hosted at data.4tu.nl.
#
# Two differences from real Figshare, both confirmed live:
#   - the endpoint accepts EITHER the numeric `id` or the `uuid` for the same
#     article (both returned identical HTTP 200 responses for article
#     16766929 / uuid 7f866e02-eb39-4a2a-8f7d-2d053ee6cde9); and
#   - every article's DOI (10.4121 prefix) always carries the numeric id
#     (e.g. "10.4121/16766929.v1"), even for articles whose primary listed
#     key elsewhere on the site is a uuid, so extracting an id from a cited
#     DOI is reliable the same way it is for real Figshare.
#
# Because the API is otherwise identical, this file holds only link detection
# and thin wrappers around archive-figshare.R's functions called with
# host = "data.4tu.nl" -- see that file for the shared implementation
# (info retrieval, file download, zip-member extraction, checksum
# verification).

#' Find 4TU.ResearchData Links in Papers
#'
#' Get all 4TU.ResearchData links: real hyperlinks from the paper's own `url`
#' table, plus a body-text fallback for a BARE mention (a DOI like
#' "10.4121/16766929" is routinely cited without any URL scheme at all) that
#' the source PDF/HTML never encoded as an actual hyperlink -- the `url` table
#' only ever contains links the source document itself made clickable. Same
#' two-tier approach `zenodo_links()` uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the 4TU.ResearchData url in the first (text) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' researchdata4tu_links(psychsci)
#' }
researchdata4tu_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("data\\.4tu\\.nl|10\\.4121/", href, ignore.case = TRUE))

  fourtu_bare_regex <- paste0(
    "(?:https?://)?(?:www\\.)?data\\.4tu\\.nl/(?:articles|datasets)/",
    "[A-Za-z0-9/_.-]*",
    "|(?:https?://)?(?:doi\\.org/)?10\\.4121/(?:uuid:)?[A-Za-z0-9-]+(?:\\.v[0-9]+)?"
  )
  other_fourtu <- text_search(paper, fourtu_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same repo commonly differ only by a
  # trailing slash, and left un-normalized that turns one repo into two
  # throughout repo_check.
  links <- dplyr::bind_rows(found_href, other_fourtu) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()

  links$researchdata4tu_url <- links$href
  links$researchdata4tu_id <- .researchdata4tu_id(links$researchdata4tu_url)

  return(links)
}

#' Get 4TU.ResearchData article ID from URL or DOI
#'
#' @param researchdata4tu_url a vector of URLs or DOIs to 4TU.ResearchData
#'   articles
#'
#' @returns a character vector of numeric article IDs, or a uuid when no
#'   numeric id can be extracted (data.4tu.nl's API accepts either -- see the
#'   note at the top of this file)
#' @keywords internal
.researchdata4tu_id <- function(researchdata4tu_url) {
  if (length(researchdata4tu_url) == 0) {
    return(character(0))
  }

  if (length(researchdata4tu_url) > 1) {
    return(vapply(researchdata4tu_url, .researchdata4tu_id, character(1)))
  }

  # handle single researchdata4tu_url ----
  researchdata4tu_url <- trimws(as.character(researchdata4tu_url))

  if (is.na(researchdata4tu_url) || !nzchar(researchdata4tu_url)) {
    return(NA_character_)
  }

  if (grepl("^[0-9]+$", researchdata4tu_url)) {
    return(researchdata4tu_url)
  }

  # A DOI's numeric segment is the article id (see note at top of file); the
  # version suffix is dropped, matching .figshare_id()'s treatment of
  # Figshare's identical ".v<n>" suffix. A "uuid:" DOI has no numeric id
  # anywhere in it, so the uuid itself is extracted and passed straight
  # through -- the API accepts it just as well.
  patterns <- c(
    "10\\.4121/uuid:([0-9a-f-]{36})",
    "10\\.4121/([0-9]+)",
    "data\\.4tu\\.nl/datasets/([0-9a-f-]{36})",
    "data\\.4tu\\.nl/articles/(?:dataset/[^/]+/)?([0-9]+)"
  )

  for (pattern in patterns) {
    match <- regexec(pattern, researchdata4tu_url, perl = TRUE, ignore.case = TRUE)
    groups <- regmatches(researchdata4tu_url, match)[[1]]
    if (length(groups) >= 2) {
      return(groups[[2]])
    }
  }

  return(NA_character_)
}

#' Retrieve info from 4TU.ResearchData by URL
#'
#' Thin wrapper around [figshare_info()] with `host = "data.4tu.nl"` -- see
#' the note at the top of this file for why 4TU.ResearchData's Djehuty
#' platform can reuse the Figshare-family implementation directly.
#'
#' @param researchdata4tu_url a 4TU.ResearchData URL or DOI, or a table
#'   containing them (e.g., as created by [researchdata4tu_links()])
#' @param id_col the index or name of the column that contains
#'   4TU.ResearchData URLs, if `researchdata4tu_url` is a table
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#'   researchdata4tu_info("https://doi.org/10.4121/16766929.v1")
#' }
researchdata4tu_info <- function(researchdata4tu_url, id_col = 1, pb = NULL) {
  raw_url <- if (is.data.frame(researchdata4tu_url)) {
    researchdata4tu_url[[id_col]]
  } else {
    researchdata4tu_url
  }
  table <- data.frame(researchdata4tu_url = raw_url)

  # figshare_info() resolves ids via .figshare_id(), which only recognises
  # figshare.com/10.6084 patterns -- a 10.4121 DOI (4TU's own prefix) would
  # never match it, so calling that function directly here would silently
  # return no metadata for every article (an earlier version of this
  # function did exactly that, caught by testing live: .figshare_id() on a
  # 10.4121 DOI returns NA, so figshare_info() took its "No valid Figshare
  # links" branch). The id is resolved with .researchdata4tu_id() instead,
  # which understands both 10.4121 DOIs and data.4tu.nl URLs, and each
  # resolved id is looked up with .figshare_info() directly -- the internal
  # single-record worker figshare_info() itself calls once per id, and which
  # takes an already-resolved id rather than re-deriving one -- bypassing
  # figshare_info()'s own (Figshare-only) id-resolution and its join logic
  # entirely, rather than working around them.
  if (!online("data.4tu.nl")) {
    stop("data.4tu.nl seems to be offline")
  }
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "4TU.ResearchData Retrieve"))
    on.exit(pb$terminate())
  }

  ids <- data.frame(
    researchdata4tu_url = table$researchdata4tu_url,
    researchdata4tu_id  = .researchdata4tu_id(table$researchdata4tu_url)
  ) |> unique()
  ids <- ids[!is.na(ids$researchdata4tu_url), , drop = FALSE]
  valid_ids <- unique(stats::na.omit(ids$researchdata4tu_id))

  if (length(valid_ids) == 0) {
    ("No valid 4TU.ResearchData links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(dplyr::left_join(table, ids, by = "researchdata4tu_url"))
  }

  paste0(
    "Starting 4TU.ResearchData retrieval for ",
    length(valid_ids), " article",
    ifelse(length(valid_ids) == 1, "", "s"), "..."
  ) |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  id_info <- lapply(valid_ids, \(id) .figshare_info(id, host = "data.4tu.nl", pb = pb))
  info <- do.call(dplyr::bind_rows, id_info)
  names(info)[names(info) == "figshare_id"] <- "researchdata4tu_id"

  data <- table |>
    dplyr::left_join(ids, by = "researchdata4tu_url") |>
    dplyr::left_join(info, by = "researchdata4tu_id", suffix = c("", ".researchdata4tu"))

  paste0("...4TU.ResearchData retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  data
}

#' Set or get a 4TU.ResearchData API token
#'
#' 4TU.ResearchData issues its own personal access tokens (from account
#' settings on data.4tu.nl) -- a Figshare.com token is meaningless here, so
#' this is stored separately from [figshare_pat()]. A token is optional for
#' reading public articles (used here only to raise rate limits or read
#' private ones).
#'
#' Store it as the `RESEARCHDATA4TU_PAT` environment variable so it is read
#' every time R starts.
#'
#' @param pat the token to set, or NULL to get the current token
#'
#' @returns the current token (character; `""` when none is set)
#' @export
#'
#' @examples
#' researchdata4tu_pat() # returns "" unless a token is set
researchdata4tu_pat <- function(pat = NULL) {
  .researchdata4tu_pat(pat)
}

.researchdata4tu_pat <- function(pat = NULL) {
  opt <- "metacheck.researchdata4tu.pat"
  env <- "RESEARCHDATA4TU_PAT"

  if (is.null(pat)) {
    return(getOption(opt) %||% Sys.getenv(env))
  }
  if (!is.character(pat) || length(pat) != 1 || is.na(pat)) {
    stop("Set researchdata4tu_pat with a single string containing your 4TU.ResearchData token",
         call. = FALSE)
  }
  args <- list(pat)
  names(args) <- opt
  do.call(options, args)
  invisible(pat)
}

#' Download all files from a 4TU.ResearchData article
#'
#' Thin wrapper around [figshare_file_download()] with `host = "data.4tu.nl"`
#' -- see the note at the top of this file. Creates a directory for the
#' article and downloads all of its files. Returns (invisibly) a data frame
#' with file info.
#'
#' Uses [researchdata4tu_pat()], not [figshare_pat()], for authentication
#' (see that function's documentation for why the two are separate).
#'
#' @param researchdata4tu_id a 4TU.ResearchData article ID, uuid, URL, or DOI
#' @param download_to path to download to
#' @param max_file_size maximum file size to download (in MB) - set to NULL or
#'   Inf for no restrictions
#' @param max_download_size maximum total size to download - set to NULL or
#'   Inf for no restrictions
#' @param unzip_types file categories to extract from `.zip` files in the
#'   article rather than downloading the zip whole, named as
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
#'   the members rather than the archive 4TU.ResearchData published a size and
#'   MD5 for.
#' @export
#'
#' @examples
#' \dontrun{
#'   researchdata4tu_file_download("16766929")
#'
#'   # take only the data files out of any zip in the article
#'   researchdata4tu_file_download("16766929", unzip_types = "data")
#' }
researchdata4tu_file_download <- function(researchdata4tu_id,
                                          download_to = ".",
                                          max_file_size = 10,
                                          max_download_size = 100,
                                          unzip_types = NULL,
                                          pb = NULL) {
  researchdata4tu_id <- .researchdata4tu_id(researchdata4tu_id) |>
    stats::na.omit() |>
    unique()
  if (length(researchdata4tu_id) == 0) return(NULL)

  withr::local_options(list(metacheck.figshare.pat = .researchdata4tu_pat()))

  dl <- figshare_file_download(
    researchdata4tu_id,
    download_to = download_to,
    max_file_size = max_file_size,
    max_download_size = max_download_size,
    unzip_types = unzip_types,
    pb = pb,
    host = "data.4tu.nl"
  )
  if (!is.null(dl) && "figshare_id" %in% names(dl)) {
    names(dl)[names(dl) == "figshare_id"] <- "researchdata4tu_id"
  }
  dl
}
