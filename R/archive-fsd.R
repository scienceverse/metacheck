# FSD (Finnish Social Science Data Archive, run as the "Aila Data Service" by
# Tampere University) exposes an open DDI-C 2.5 XML metadata record per study
# -- NO authentication needed -- at
# https://services.fsd.tuni.fi/catalogue/<ID>/DDI/<ID>_eng.xml. Confirmed live
# 2026-09-09 against a real study (FSD2653, "Finnish National Election Study
# 2011"): the record includes the study title, a DOI (agency="DOI" IDNo,
# e.g. 10.60686/t-fsd2653), an access-restriction statement
# (dataAccs/useStmt/restrctn), and one fileDscr block per data file (name,
# format, case/variable counts) -- but, unlike Dryad/Zenodo/OSF/PsychArchives,
# NO file-level download URI anywhere in the record.
#
# Downloading the actual data requires either Haka (the Finnish
# higher-education/research federated login), an Aila username/password
# (issued to foreign researchers or non-Haka institutions), or, for most
# studies, a per-dataset access-application form (stated purpose, funder,
# etc.) reviewed by FSD -- none of which a script can automate, and a
# minority of studies are open without registration but with no documented
# anonymous download endpoint either. This file therefore only LISTS what a
# study contains (for reporting: is there data, what format, how big) --
# there is no fsd_file_download(); every listed file has file_url = NA, the
# same "found but inaccessible" treatment repo_check already gives a private
# OSF node or a restricted PsychArchives item.

.FSD_DDI_BASE <- "https://services.fsd.tuni.fi/catalogue"

# Extract a canonical "FSD<digits>" study id from any real citation form seen
# in the wild: the catalogue URL (services.fsd.tuni.fi/catalogue/FSD2653),
# the DOI (doi.org/10.60686/t-fsd2653), the URN (urn.fi/urn:nbn:fi:fsd:
# t-fsd2653), or a bare in-text mention ("FSD2653"). All four share "fsd"
# immediately followed (after at most one separator character) by the
# study's digits somewhere in the string -- confirmed against all three real
# URL forms above; the digits are always what api/catalogue calls need,
# regardless of which form cited the study. Returns NA_character_ when no
# study id is present.
.fsd_study_id <- function(url) {
  url <- as.character(url)
  m <- regmatches(url, regexpr("(?i)fsd[:_-]?([0-9]{3,6})", url, perl = TRUE))
  if (length(m) == 0 || !nzchar(m)) return(NA_character_)
  digits <- sub("(?i).*?([0-9]{3,6})$", "\\1", m, perl = TRUE)
  paste0("FSD", digits)
}

#' Find FSD Links in Papers
#'
#' Get all Finnish Social Science Data Archive (FSD / Aila Data Service)
#' links: real hyperlinks from the paper's own `url` table (the catalogue
#' page, a 10.60686 DOI, or a urn.fi URN), plus a body-text fallback for a
#' bare "FSD####" mention that the source PDF/HTML never encoded as an actual
#' hyperlink. Same two-tier approach `github_links()` uses for GitHub.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the FSD url in the first (href) column
#' @export
#'
#' @examples
#' \dontrun{
#' psychsci <- papers_load("psychsci", cache = TRUE)
#' fsd_links(psychsci)
#' }
fsd_links <- function(paper) {
  href <- text <- NULL

  fsd_regex <- paste0(
    "(?:https?://)?(?:www\\.|services\\.)?fsd\\.tuni\\.fi/[A-Za-z0-9/._?=&%-]+",
    "|(?:https?://)?doi\\.org/10\\.60686/[A-Za-z0-9.-]+",
    "|(?:https?://)?urn\\.fi/urn:nbn:fi:fsd:[A-Za-z0-9.-]+"
  )
  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl(fsd_regex, href, ignore.case = TRUE, perl = TRUE))

  fsd_bare_regex <- "FSD[0-9]{3,6}"
  other_fsd <- text_search(paper, fsd_bare_regex, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why this normalization is needed: a real hyperlink and
  # a bare body-text mention of the same study commonly differ only by a
  # trailing slash or query string, and left un-normalized that turns one
  # study into several throughout repo_check.
  dplyr::bind_rows(found_href, other_fsd) |>
    dplyr::mutate(href = sub("/+$", "", href)) |>
    unique()
}

#' Retrieve info from FSD by URL
#'
#' Lists a Finnish Social Science Data Archive study's title, DOI, access
#' restriction, and data-file inventory (name, format, case/variable counts)
#' via its open DDI-C 2.5 XML metadata record. There is no file-level
#' download here -- the DDI record carries no download URI, and actual data
#' access requires Haka/Aila login or a per-dataset access application (see
#' the file header comment) -- so `files` is for REPORTING what a study
#' contains, not for `download_repo_files()` to fetch.
#'
#' @param fsd_url an FSD URL (catalogue page, DOI, or URN), or a table
#'   containing them (e.g., as created by `fsd_links()`)
#' @param id_col the index or name of the column that contains FSD URLs, if
#'   `fsd_url` is a table
#' @param pb a progress bar passed from another function
#' @param cache if `TRUE`, reuse a cached listing from a prior call for the
#'   same study instead of re-querying FSD (see [repo_info_cache()]);
#'   default `FALSE`
#'
#' @returns a data frame of information
#' @export
#' @examples
#' \dontrun{
#' fsd_info("https://services.fsd.tuni.fi/catalogue/FSD2653")
#' }
fsd_info <- function(fsd_url, id_col = 1, pb = NULL, cache = FALSE) {
  # The bare domain does not resolve at all (confirmed live: fsd.tuni.fi
  # times out with no response, unlike www.fsd.tuni.fi or the
  # services.fsd.tuni.fi subdomain the DDI records themselves live on).
  if (!online("www.fsd.tuni.fi")) {
    stop("FSD (fsd.tuni.fi) seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "FSD Retrieve"))
    on.exit(pb$terminate())
  }

  # handle list of links
  if (is.data.frame(fsd_url)) {
    table <- fsd_url
    id_col_name <- colnames(table[id_col])
    raw_urls <- table[[id_col]]
  } else {
    id_col_name <- "fsd_url"
    raw_urls <- unique(fsd_url) |> stats::na.omit()
    table <- data.frame(fsd_url = raw_urls)
  }

  # remove blank, missing, duplicate, or invalid IDs
  ids <- data.frame(
    fsd_url = raw_urls
  )
  ids <- ids[!is.na(ids$fsd_url), , drop = FALSE] |> unique()
  valid_ids <- unique(ids$fsd_url)

  if (length(valid_ids) == 0) {
    ("No valid FSD links") |>
      list(what = _) |>
      pb$tick(0, tokens = _)
    return(table)
  }

  # iterate over valid IDs
  paste0(
    "Starting FSD retrieval for ",
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
    cached <- if (isTRUE(cache)) .repo_info_cache_get("fsd", id) else NULL
    if (!is.null(cached)) {
      info <- cached
    } else {
      info <- .fsd_info(id)
      if (isTRUE(cache) && .repo_info_ok(info))
        .repo_info_cache_put("fsd", id, info)
    }
    if ("error" %in% names(info)) error <- TRUE
    id_info[[i]] <- info
  }

  info <- id_info |>
    do.call(dplyr::bind_rows, args = _) |>
    dplyr::left_join(ids, by = "fsd_url")

  # reduplicate and add original table info
  by <- stats::setNames("fsd_url", id_col_name)
  data <- dplyr::left_join(table, info,
    by = by,
    suffix = c("", ".fsd")
  )

  paste0("...FSD retrieval complete!") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  return(data)
}

#' Retrieve info from FSD by URL
#'
#' @param fsd_url an FSD URL
#' @param pb a progress bar passed from another function
#'
#' @returns a data frame of information
#' @export
#' @keywords internal
.fsd_info <- function(fsd_url, pb = NULL) {
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }

  paste0("* Retrieving info from ", fsd_url, "...") |>
    list(what = _) |>
    pb$tick(0, tokens = _)

  # set up return table
  obj <- data.frame(
    fsd_url = fsd_url
  )

  study_id <- .fsd_study_id(fsd_url)
  if (is.na(study_id)) {
    warning(fsd_url, " is not a valid FSD study reference", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  doc <- .fsd_ddi_xml(study_id)
  if (is.null(doc)) {
    warning(fsd_url, " could not be found", call. = FALSE)
    obj$error <- "unfound"
    return(obj)
  }

  # Study-level metadata lives under stdyDscr/citation/titlStmt -- the DDI
  # RECORD itself also has its own titlStmt under docDscr (its title is
  # "DDI description: <study title>", not the study's own title), so this
  # must be anchored at stdyDscr specifically, not a bare //titl.
  stdy_titl <- xml2::xml_find_first(doc, "//stdyDscr/citation/titlStmt/titl")
  obj$FSD_title <- if (!is.na(stdy_titl)) xml2::xml_text(stdy_titl) else NA_character_

  doi_node <- xml2::xml_find_first(
    doc, "//stdyDscr/citation/titlStmt/IDNo[@agency='DOI']")
  obj$FSD_doi <- if (!is.na(doi_node)) xml2::xml_text(doi_node) else NA_character_

  # FSD's own access-restriction category (e.g. "The dataset is (B)
  # available for research, teaching and study.") -- not a Creative Commons
  # style license string; reported as-is rather than reinterpreted, since
  # the lettered categories are FSD's own internal scheme, not a standard
  # this package should try to re-derive.
  restrctn <- xml2::xml_find_first(doc, "//stdyDscr/dataAccs/useStmt/restrctn")
  obj$FSD_access <- if (!is.na(restrctn)) xml2::xml_text(restrctn) else NA_character_

  # File inventory: one fileDscr per data file. name/format/case/variable
  # counts only -- no download URI exists in this record at all (see file
  # header comment), so there is no file_url/size-in-bytes to carry here.
  file_nodes <- xml2::xml_find_all(doc, "//fileDscr")
  .node_text <- function(n, path) {
    v <- xml2::xml_find_first(n, path)
    if (is.na(v)) NA_character_ else xml2::xml_text(v)
  }
  file_list <- if (length(file_nodes) == 0) {
    data.frame(name = character(0), format = character(0),
               cases = integer(0), variables = integer(0),
               stringsAsFactors = FALSE)
  } else {
    data.frame(
      name = vapply(file_nodes, .node_text, character(1), path = ".//fileName"),
      format = vapply(file_nodes, .node_text, character(1), path = ".//fileType"),
      cases = suppressWarnings(as.integer(
        vapply(file_nodes, .node_text, character(1), path = ".//caseQnty"))),
      variables = suppressWarnings(as.integer(
        vapply(file_nodes, .node_text, character(1), path = ".//varQnty"))),
      stringsAsFactors = FALSE
    )
  }
  obj$files <- list(file_list)

  return(obj)
}

# One DDI-C 2.5 XML record for a study, parsed and namespace-stripped (the
# document declares a default namespace, xmlns="ddi:codebook:2_5", which
# would otherwise make every plain-tag-name XPath below match nothing --
# xml_ns_strip() is xml2's documented way to make a namespaced document
# queryable with plain XPath). Returns NULL on any failure / non-200.
.fsd_ddi_xml <- function(study_id) {
  url <- sprintf("%s/%s/DDI/%s_eng.xml", .FSD_DDI_BASE, study_id, study_id)
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) return(NULL)
    doc <- xml2::read_xml(httr2::resp_body_string(resp))
    xml2::xml_ns_strip(doc)
    doc
  }, error = \(e) NULL)
}
