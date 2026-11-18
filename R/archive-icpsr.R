# ICPSR and openICPSR, with the AEA Data and Code Repository (economics
# journals): replication packages with DOIs 10.3886/E<project>V<version>, and
# ICPSR's curated studies, 10.3886/ICPSR<study>. Their records are at
# DataCite (title, creators, date, and the article a package belongs to),
# which is enough to tell whose deposit a link is and to list it. Their files
# cannot be read automatically: openICPSR has moved to ICPSR's site, and
# every project page and download answers an automatic request with a
# browser check (HTTP 403, tested 2026-11-18, also after logging in with a
# registered account). The AEA Data Editor's tools pass it with a token from
# ICPSR (header x-openicpsr-cloudflare-token), which metacheck does not have.

# Links papers write: the DOI (with or without "V<n>"), the openICPSR project
# page, ICPSR's study pages (old and new site), and the AEA landing page of a
# project
.icpsr_rx <- paste0(
  "(?i)10\\.3886/(?:E[0-9]{4,7}(?:V[0-9]+)?|ICPSR[0-9]{4,6}(?:\\.v[0-9]+)?)",
  "|openicpsr\\.org/openicpsr/project/[0-9]{4,7}(?:/version/V[0-9]+)?",
  "|icpsr\\.umich\\.edu/(?:icpsrweb/[A-Za-z]+|web/[A-Za-z]+|sites/[A-Za-z]+/view)/studies/[0-9]{4,7}(?:/versions/V[0-9]+)?")

# The DOI a link names: "10.3886/E195681V1", "10.3886/ICPSR36138.v1"
.icpsr_doi_of <- function(link) {
  l <- as.character(link)
  out <- rep(NA_character_, length(l))
  d <- regmatches(l, regexpr("(?i)10\\.3886/(?:E[0-9]{4,7}(?:V[0-9]+)?|ICPSR[0-9]{4,6}(?:\\.v[0-9]+)?)", l, perl = TRUE))
  has_d <- grepl("(?i)10\\.3886/", l, perl = TRUE)
  out[has_d] <- d
  proj <- !has_d & grepl("openicpsr\\.org/openicpsr/project/", l, ignore.case = TRUE)
  out[proj] <- paste0("10.3886/E", sub("^.*project/([0-9]+).*$", "\\1", l[proj]),
                      ifelse(grepl("/version/V[0-9]+", l[proj], ignore.case = TRUE),
                             toupper(sub("^.*/version/(V[0-9]+).*$", "\\1", l[proj], ignore.case = TRUE)), ""))
  st <- !has_d & !proj & grepl("icpsr\\.umich\\.edu/.*/studies/[0-9]+", l, ignore.case = TRUE)
  # a study page names a curated study, or (on the new site) an openICPSR
  # project; the number is the same, and DataCite has the project's E-DOI
  # for openICPSR numbers (6 digits, from 100000) and the ICPSR-DOI for the
  # curated studies
  num <- sub("^.*/studies/([0-9]+).*$", "\\1", l[st])
  ver <- ifelse(grepl("/versions/V[0-9]+", l[st], ignore.case = TRUE),
                toupper(sub("^.*/versions/(V[0-9]+).*$", "\\1", l[st], ignore.case = TRUE)), "")
  # (ICPSR's study numbers have five digits in their DOIs: ICPSR04652)
  out[st] <- ifelse(nchar(num) >= 6, paste0("10.3886/E", num, ver), paste0("10.3886/ICPSR", sprintf("%05d", as.integer(num))))
  out <- sub("(?i)^10\\.3886/e", "10.3886/E", out, perl = TRUE)
  out <- sub("(?i)^(10\\.3886/E[0-9]+)v", "\\1V", out, perl = TRUE)
  out
}

#' Find ICPSR and openICPSR deposits in a paper
#'
#' Finds replication packages in openICPSR, including the AEA Data and Code
#' Repository of the American Economic Association's journals, and ICPSR's
#' curated studies: their DOIs (`10.3886/E195681V1`, `10.3886/ICPSR36138.v1`),
#' openICPSR project pages and ICPSR study pages.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with a row for each deposit: `href` (its DOI as a
#'   link), `doi`, `repo_type` ("icpsr"), `link` (as the paper writes it), and
#'   `text_id` / `paper_id`
#' @export
#'
#' @examples
#' icpsr_links(test_paper(text = "Data and code: https://doi.org/10.3886/E195681V1"))
icpsr_links <- function(paper) {
  none <- data.frame(paper_id = character(0), href = character(0), doi = character(0),
                     repo_type = character(0), link = character(0), text_id = integer(0))
  href <- text <- NULL
  found_href <- paper_table(paper, "url")
  found_href <- found_href[grepl(.icpsr_rx, found_href$href %||% character(0), perl = TRUE), , drop = FALSE]
  in_text <- text_search(paper, .icpsr_rx, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))
  links <- dplyr::bind_rows(found_href, in_text)
  if (!nrow(links)) return(none)
  if (!"paper_id" %in% names(links)) links$paper_id <- NA_character_
  if (!"text_id" %in% names(links)) links$text_id <- NA_integer_
  links$doi <- .icpsr_doi_of(links$href)
  links <- links[!is.na(links$doi), , drop = FALSE]
  links <- links[order(is.na(links$text_id)), , drop = FALSE]
  # one row per deposit: a versioned and an unversioned DOI of one project
  # are one deposit (the versioned one is kept)
  key <- tolower(sub("(?i)(V[0-9]+|\\.v[0-9]+)$", "", links$doi, perl = TRUE))
  links <- links[order(key, !grepl("(?i)(V[0-9]+|\\.v[0-9]+)$", links$doi, perl = TRUE)), , drop = FALSE]
  key <- tolower(sub("(?i)(V[0-9]+|\\.v[0-9]+)$", "", links$doi, perl = TRUE))
  links <- links[!duplicated(paste(links$paper_id, key)), , drop = FALSE]
  if (!nrow(links)) return(none)
  out <- data.frame(paper_id = links$paper_id, href = paste0("https://doi.org/", links$doi), doi = links$doi,
                    repo_type = "icpsr", link = links$href, text_id = links$text_id)
  for (j in seq_len(nrow(out))) assign(.own_core(out$href[[j]]), out$link[[j]], envir = .repo_link_alias)
  rownames(out) <- NULL
  out
}

#' Get the records of ICPSR and openICPSR deposits
#'
#' Reads each deposit's record from DataCite: title, creators, publication
#' date, description, resource type, and the works it relates to (for an AEA
#' replication package, the article it belongs to). The files are not read:
#' ICPSR's site does not answer automatic requests.
#'
#' @param icpsr_url the deposits' addresses, as [icpsr_links()] gives them in
#'   `href`, or a data frame with them in column `id_col`
#' @param id_col the column of `icpsr_url` holding the addresses
#' @param pb a progress bar, or NULL
#' @param cache whether to keep each answer for the session
#'
#' @returns a data frame with a row per address: `icpsr_url`, `doi`, `title`,
#'   `creators` (names separated by "; "), `publication_date`, `description`,
#'   `resource_type`, `related` (`<relation>:<DOI>` pairs), and `error`
#'   ("unfound" when DataCite has no such DOI)
#' @export
#'
#' @examples
#' \donttest{
#' if (online("api.datacite.org")) icpsr_info("https://doi.org/10.3886/E195681V1")
#' }
icpsr_info <- function(icpsr_url, id_col = 1, pb = NULL, cache = FALSE) {
  urls <- if (is.data.frame(icpsr_url)) icpsr_url[[id_col]] else icpsr_url
  urls <- unique(stats::na.omit(as.character(urls)))
  urls <- urls[nzchar(urls)]
  empty <- data.frame(icpsr_url = character(0), doi = character(0), title = character(0),
                      creators = character(0), publication_date = character(0), description = character(0),
                      resource_type = character(0), related = character(0), error = character(0))
  if (!length(urls)) return(empty)
  one <- function(u, resp) {
    doi <- .icpsr_doi_of(u)
    obj <- data.frame(icpsr_url = u, doi = doi, title = NA_character_, creators = NA_character_,
                      publication_date = NA_character_, description = NA_character_,
                      resource_type = NA_character_, related = NA_character_, error = NA_character_)
    st <- if (inherits(resp, "httr2_response")) httr2::resp_status(resp) else NA_integer_
    a <- if (st %in% 200) tryCatch(httr2::resp_body_json(resp)$data$attributes, error = \(e) NULL)
    if (is.null(a)) { obj$error <- if (st %in% 404) "unfound" else "no_answer"; return(obj) }
    people <- vapply(a$creators %||% list(), \(p) as.character(p$name %||% paste(p$givenName %||% "", p$familyName %||% "")), "")
    # DataCite writes "Family, Given"; the other readers give "Given Family"
    people <- ifelse(grepl(",", people), paste(trimws(sub("^[^,]*,", "", people)), sub(",.*$", "", people)), people)
    tt <- unlist(lapply(a$titles %||% list(), \(t) t$title))
    obj$title <- if (length(tt)) as.character(tt[[1]]) else NA_character_
    obj$creators <- if (length(people)) paste(trimws(people), collapse = "; ") else NA_character_
    dates <- a$dates %||% list()
    issued <- unlist(lapply(dates, \(d) if (identical(d$dateType, "Issued")) d$date))
    obj$publication_date <- substr(issued[1] %||% as.character(a$publicationYear %||% NA), 1, 10)
    obj$description <- substr(paste(unlist(lapply(a$descriptions %||% list(), `[[`, "description")), collapse = " "), 1, 2000)
    obj$resource_type <- a$types$resourceTypeGeneral %||% NA_character_
    obj$related <- .meta_related(a$relatedIdentifiers, "relationType", "relatedIdentifier")
    obj
  }
  rows <- vector("list", length(urls))
  todo <- integer(0)
  for (i in seq_along(urls)) {
    cached <- if (isTRUE(cache)) .repo_info_cache_get("icpsr", urls[[i]]) else NULL
    if (is.null(cached)) todo <- c(todo, i) else rows[[i]] <- cached
  }
  if (length(todo)) {
    resps <- .batch_query(paste0("https://api.datacite.org/dois/", utils::URLencode(.icpsr_doi_of(urls[todo]))), msg = NULL)
    for (k in seq_along(todo)) {
      i <- todo[[k]]
      rows[[i]] <- one(urls[[i]], resps[[k]])
      if (isTRUE(cache) && .repo_info_ok(rows[[i]])) .repo_info_cache_put("icpsr", urls[[i]], rows[[i]])
    }
  }
  dplyr::bind_rows(rows)
}
