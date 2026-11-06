# LDbase (https://ldbase.org) is the data repository for educational and
# developmental sciences run by Florida State University (FSU Psychology, the
# Florida Center for Reading Research and FSU Libraries), funded by NIH. It
# runs on Drupal 10 but has no public API: Drupal's JSON:API is switched off
# (/jsonapi answers 404, checked 2026-10-15), so everything here is read from
# the public web pages, the same way rbox_info() reads ResearchBox.
#
# Content is organised in PROJECTS, each holding datasets, code and documents
# (and child projects). Every item has its own page, at
# ldbase.org/<projects|datasets|code|documents>/<uuid>, and DOIs under the
# prefix 10.33009 (e.g. 10.33009/ldbase.1627053448.d151) lead to those pages.
# Verified on several projects (2026-10-15):
#   * a project page lists ALL of its items, child projects included, in its
#     "Project Hierarchy" tree (the "Most Recent ..." blocks above it are
#     truncated, so they are not used);
#   * an item page links its current file under /system/files/..., inside the
#     field-file-upload (datasets), field-code-file (code) or
#     field-document-file (documents) block; older dataset versions are not
#     linked there;
#   * files download without an account, and a one-byte ranged request
#     reports the size, which download_repo_files() uses when the listing has
#     none (pages show no sizes);
#   * the project's "download all project files" zip is NOT used: LDbase built
#     it for one project but answered 500/503 for four others.

#' Find LDbase links in a paper
#'
#' Finds links to LDbase (<https://ldbase.org>) projects, datasets, code and
#' documents, as hyperlinks or in the text, and LDbase DOIs (prefix 10.33009,
#' e.g. `10.33009/ldbase.1627053448.d151`). LDbase's own DOI
#' (`10.33009/ldbase`, cited for the repository itself) and links to its home
#' or search pages are not included: they name no deposit.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with a row for each link (`href`, plus `text_id` /
#'   `paper_id`), with `ldbase_url` (the link) and `ldbase_type` (`projects`,
#'   `datasets`, `code`, `documents`, or `doi` for a DOI not yet resolved)
#' @export
#'
#' @examples
#' paper <- test_paper(url = "https://ldbase.org/projects/09609d82-c45f-4442-8c0c-d136191bdc50")
#' ldbase_links(paper)
ldbase_links <- function(paper) {
  href <- text <- NULL

  found_href <- paper_table(paper, "url") |>
    dplyr::filter(grepl("ldbase\\.org|10\\.33009/", href, ignore.case = TRUE))

  other_ldbase <- text_search(paper, .ldbase_regex(), return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))

  # See osf_links() for why trailing slashes are dropped; a trailing full stop
  # is dropped too, since a DOI often ends a sentence.
  links <- dplyr::bind_rows(found_href, other_ldbase) |>
    dplyr::mutate(href = sub("[/.]+$", "", trimws(href))) |>
    unique()
  links <- links[grepl(.ldbase_regex(), links$href, perl = TRUE, ignore.case = TRUE), ,
                 drop = FALSE]

  links$ldbase_url <- links$href
  links$ldbase_type <- .ldbase_type(links$ldbase_url)
  links
}

# An LDbase item page or an LDbase deposit DOI. A DOI must carry something
# after "ldbase." -- the bare 10.33009/ldbase is the repository's own DOI.
.ldbase_regex <- function() {
  paste0(
    "(?:https?://)?(?:www\\.)?ldbase\\.org(?::443)?/",
    "(?:projects|datasets|code|documents)/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",
    "|(?:https?://)?(?:dx\\.)?(?:doi\\.org/)?10\\.33009/ldbase\\.[A-Za-z0-9.]*[A-Za-z0-9]"
  )
}

# "projects"/"datasets"/"code"/"documents" for an item page URL, "doi" for an
# LDbase DOI, NA otherwise.
.ldbase_type <- function(url) {
  if (length(url) == 0) return(character(0))
  type <- rep(NA_character_, length(url))
  m <- regmatches(url, regexec("(?i)ldbase\\.org(?::443)?/(projects|datasets|code|documents)/",
                               url, perl = TRUE))
  hit <- lengths(m) == 2
  type[hit] <- tolower(vapply(m[hit], `[[`, character(1), 2))
  type[!hit & grepl("10\\.33009/ldbase\\.", url, ignore.case = TRUE)] <- "doi"
  type
}

# The canonical page URL for an item link: https, no port, no trailing
# slash or query.
.ldbase_page_url <- function(url) {
  m <- regmatches(url, regexpr(
    "(?i)ldbase\\.org(?::443)?/(projects|datasets|code|documents)/[0-9a-f-]{36}",
    url, perl = TRUE))
  if (!length(m)) return(NA_character_)
  paste0("https://", sub(":443", "", tolower(m), fixed = TRUE))
}

#' Get the files of LDbase projects, datasets, code and documents
#'
#' Lists the files of each LDbase link: for a project, every dataset, code
#' and document in its hierarchy (child projects included); for a single
#' dataset, code or document, its own file. A DOI is first resolved to the
#' page it leads to. LDbase has no API, so this reads the public pages (one
#' request per project or item). Items stored elsewhere (LDbase's "external
#' link" option) have no file here and are not listed.
#'
#' @param ldbase_url a vector of LDbase URLs or DOIs, or a table containing
#'   them (e.g. from [ldbase_links()])
#' @param id_col the index or name of the column that contains the URLs, if
#'   `ldbase_url` is a table
#' @param pb a progress bar from [pb()], or NULL to create one
#' @param cache if `TRUE`, a listing is read from / written to the persistent
#'   repository-listing cache (see `repo_info_cache_clear()`)
#'
#' @returns a data frame with one row per link: `ldbase_url`, `ldbase_page`
#'   (the resolved page), `ldbase_type`, `title`, `doi`, `license` (the
#'   licence of the item, or every distinct licence of a project's items
#'   joined by "; "), a list-column `files` (a data frame per link with
#'   `file_name`, `file_path`, `file_url`, `item_type`, `item_url`) and
#'   `error` (NA, or why the link could not be listed)
#' @export
#' @examples
#' \donttest{
#' if (online("ldbase.org")) {
#'   ldbase_info("https://ldbase.org/datasets/ef328591-e249-4803-b673-76f237717d1d")
#' }
#' }
ldbase_info <- function(ldbase_url, id_col = 1, pb = NULL, cache = FALSE) {
  if (!online("ldbase.org")) {
    stop("ldbase.org seems to be offline")
  }

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    pb$tick(0, list(what = "LDbase Retrieve"))
    on.exit(pb$terminate())
  }

  urls <- if (is.data.frame(ldbase_url)) ldbase_url[[id_col]] else ldbase_url
  urls <- unique(stats::na.omit(as.character(urls)))
  urls <- urls[nzchar(urls)]
  if (length(urls) == 0) {
    return(data.frame(ldbase_url = character(0), ldbase_page = character(0),
                      ldbase_type = character(0), title = character(0),
                      doi = character(0), license = character(0),
                      error = character(0)))
  }

  rows <- lapply(urls, function(u) {
    cached <- if (isTRUE(cache)) .repo_info_cache_get("ldbase", u) else NULL
    if (!is.null(cached)) return(cached)
    # A failure (e.g. a firewall browser check) is recorded as this link's
    # error, so the other links are still listed.
    res <- tryCatch(.ldbase_info(u, pb = pb), error = \(e) {
      d <- data.frame(ldbase_url = u, ldbase_page = NA_character_,
                      ldbase_type = .ldbase_type(u), title = NA_character_,
                      doi = NA_character_, license = NA_character_,
                      error = conditionMessage(e))
      d$files <- list(NULL)
      d
    })
    if (isTRUE(cache) && .repo_info_ok(res)) .repo_info_cache_put("ldbase", u, res)
    res
  })

  pb$tick(0, list(what = "...LDbase retrieval complete!"))
  dplyr::bind_rows(rows)
}

# One LDbase link -> one row (see ldbase_info()).
.ldbase_info <- function(url, pb = NULL) {
  obj <- data.frame(ldbase_url = url, ldbase_page = NA_character_,
                    ldbase_type = NA_character_, title = NA_character_,
                    doi = NA_character_, license = NA_character_,
                    error = NA_character_)
  obj$files <- list(NULL)

  if (!is.null(pb)) pb$tick(0, list(what = paste0("* Retrieving info from LDbase ", url, "...")))

  page <- .ldbase_page_url(url)
  if (is.na(page) && identical(.ldbase_type(url), "doi")) {
    doi <- regmatches(url, regexpr("10\\.33009/ldbase\\.[A-Za-z0-9.]*[A-Za-z0-9]", url,
                                   ignore.case = TRUE))
    resolved <- .handle_registered_url(doi)
    page <- .ldbase_page_url(resolved$url %||% NA_character_)
    if (is.na(page)) {
      obj$error <- if (isTRUE(resolved$ok)) "unfound" else "doi_unresolved"
      return(obj)
    }
  }
  if (is.na(page)) {
    obj$error <- "unfound"
    return(obj)
  }
  obj$ldbase_page <- page
  obj$ldbase_type <- .ldbase_type(page)

  html <- .ldbase_get(page)
  if (is.null(html)) {
    obj$error <- "unfound"
    return(obj)
  }
  meta <- .ldbase_page_meta(html)
  obj$title <- meta$title
  obj$doi <- meta$doi

  # A project lists every item in its hierarchy, each read from its own page;
  # a single item is its own (already fetched) page.
  items <- if (identical(obj$ldbase_type, "projects")) .ldbase_project_items(html) else page
  files <- lapply(items, function(item) {
    item_html <- if (identical(item, page)) html else .ldbase_get(item)
    if (is.null(item_html)) return(NULL)
    f <- .ldbase_item_files(item_html, item)
    if (!nrow(f)) return(NULL)
    f$license <- .ldbase_page_meta(item_html)$license
    if (identical(obj$ldbase_type, "projects")) {
      # Each item's files in a folder named after the item, as in LDbase's
      # own project zip, so files of different items do not collide.
      folder <- gsub("[/\\\\:*?\"<>|]+", "_", .ldbase_page_meta(item_html)$title %||% basename(item))
      f$file_path <- paste0(trimws(folder), "/", f$file_name)
    }
    f
  })
  files <- dplyr::bind_rows(files)
  if (nrow(files)) {
    lic <- unique(stats::na.omit(files$license))
    obj$license <- if (length(lic)) paste(lic, collapse = "; ") else NA_character_
    files$license <- NULL
    obj$files <- list(files)
  }
  obj
}

# GET one LDbase page as an xml2 document, or NULL on failure. .batch_query()
# retries LDbase's occasional 500/503 answers. Paced to one request per
# second (httr2's throttle is shared by every request to the host in the
# session): LDbase's firewall answers with a browser check (see
# .waf_challenge_check(), R/zip-peek.R) after a burst of requests, which it
# did for about an hour during a validation run of eight projects.
.ldbase_get <- function(url) {
  resp <- .batch_query(url, msg = NULL, accept = "text/html",
                       throttle_capacity = 1, throttle_fill_time_s = 1)[[1]]
  if (is.null(resp)) return(NULL)
  if (.waf_challenge_check(resp, url))
    stop("firewall browser check (AWS WAF challenge); try again later", call. = FALSE)
  if (httr2::resp_status(resp) != 200) return(NULL)
  tryCatch(xml2::read_html(httr2::resp_body_string(resp), encoding = "UTF-8"),
           error = \(e) NULL)
}

# Title, DOI and licence of an LDbase page, from its schema.org JSON-LD
# (title, licence) and its DOI field.
.ldbase_page_meta <- function(html) {
  out <- list(title = NA_character_, doi = NA_character_, license = NA_character_)
  ld <- xml2::xml_find_all(html, "//script[@type='application/ld+json']") |>
    xml2::xml_text()
  for (txt in ld) {
    j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = \(e) NULL)
    if (is.null(j)) next
    for (x in if (is.null(names(j))) j else list(j)) {
      if (identical(x[["@type"]], "BreadcrumbList")) next
      if (is.character(x$name) && length(x$name) == 1) out$title <- x$name
      if (is.character(x$license) && length(x$license) == 1) out$license <- x$license
    }
  }
  if (is.na(out$title)) {
    h1 <- xml2::xml_find_first(html, "//h1") |> xml2::xml_text()
    if (!is.na(h1)) out$title <- trimws(sub("^[^:]+:\\s*", "", h1))
  }
  doi_txt <- xml2::xml_find_all(html, "//div[contains(@class,'field--name-field-doi')]//a/@href") |>
    xml2::xml_text()
  doi <- regmatches(doi_txt, regexpr("10\\.33009/[^\\s\"'<>]+", doi_txt, perl = TRUE))
  if (length(doi)) out$doi <- doi[[1]]
  out
}

# Every dataset, code and document page in a project's "Project Hierarchy"
# tree, below the project itself (the tree item marked current), child
# projects included.
.ldbase_project_items <- function(html) {
  hrefs <- xml2::xml_find_all(
    html, "//li[contains(@class,'project-tree-view-item-current')]//a/@href") |>
    xml2::xml_text()
  # Without the tree (a page layout change), fall back to every item link on
  # the page, which may be only the most recent ones.
  if (!length(hrefs))
    hrefs <- xml2::xml_find_all(html, "//a/@href") |> xml2::xml_text()
  hrefs <- hrefs[grepl("^/(datasets|code|documents)/", hrefs)]
  if (!length(hrefs)) return(character(0))
  urls <- vapply(paste0("https://ldbase.org", hrefs), .ldbase_page_url, character(1))
  urls <- unique(stats::na.omit(unname(urls)))
  urls[.ldbase_type(urls) %in% c("datasets", "code", "documents")]
}

# The current file(s) of a dataset, code or document page: the links in its
# file field (see the file header). Returns file_name, file_url, file_path,
# item_type, item_url; 0 rows for an item stored elsewhere.
.ldbase_item_files <- function(html, item_url) {
  hrefs <- xml2::xml_find_all(html, paste0(
    "//div[contains(@class,'field--name-field-file-upload') or ",
    "contains(@class,'field--name-field-code-file') or ",
    "contains(@class,'field--name-field-document-file')]//a/@href")) |>
    xml2::xml_text()
  hrefs <- unique(hrefs[grepl("/system/files/", hrefs, fixed = TRUE)])
  if (!length(hrefs)) {
    return(data.frame(file_name = character(0), file_path = character(0),
                      file_url = character(0), item_type = character(0),
                      item_url = character(0)))
  }
  file_url <- ifelse(grepl("^https?://", hrefs), hrefs, paste0("https://ldbase.org", hrefs))
  file_url <- sub("^http://", "https://", file_url)
  file_name <- .url_decode(basename(hrefs))
  data.frame(file_name = file_name, file_path = file_name, file_url = file_url,
             item_type = .ldbase_type(item_url), item_url = item_url)
}
