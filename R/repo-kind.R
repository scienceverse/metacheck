# Is a repository record a data or code deposit, or a publication (#637)?
#
# Institutional repositories (DSpace, EPrints) hold papers, books and theses
# beside data, and a paper often cites one of its own earlier papers or a
# thesis there. Such a record is not a deposit, and repo_check ignores it.
# - A DOI's kind is known before its record is read: DataCite's record of a
#   DOI gives its registered address and its resource type in one request
#   (verified 2026-11-13: 10.17037/data.174, 10.5525/gla.researchdata.2235
#   and 10.7801/330 are "Dataset"). A DOI DataCite does not have (a journal
#   article registered with Crossref) is looked up through the handle
#   service, as before.
# - A plain link carries no type, so the record read anyway decides: EPrints
#   records give `type` (data_collection or dataset on the data repositories;
#   article on IIASA's), DSpace items dc.type. When the record gives none,
#   the deposit is kept only when it holds data or code files.

# DataCite resource types (resourceTypeGeneral) of publications
.publication_resource_types <- c(
  "Text", "JournalArticle", "Journal", "Book", "BookChapter", "ConferencePaper",
  "ConferenceProceeding", "Dissertation", "Preprint", "Report", "PeerReview", "Standard")

# Does a repository record's own type (EPrints `type`, DSpace dc.type, or a
# DataCite resource type) say it is a publication? NA when there is no type.
# EPrints' own publication types: article, book, book_section, monograph,
# conference_item, thesis, patent; DSpace's dc.type is free text ("Article",
# "Journal article", "Doctoral thesis", "Working paper", "Text", ...).
.repo_publication_type <- function(type) {
  t <- tolower(trimws(as.character(type)))
  t <- gsub("[_-]+", " ", t)
  # Zenodo's "publication" (InvenioRDM: "publication-article", ...), poster
  # and presentation are publications too
  out <- grepl(paste0("\\b(publication|article|articles|book|book section|chapter|monograph|conference( item| paper| proceedings?)?",
                      "|proceedings?|thesis|theses|dissertation|patent|report|working paper|preprint|review|journal|text",
                      "|journalarticle|bookchapter|conferencepaper|conferenceproceeding|peerreview|standard",
                      "|letter|editorial|poster|presentation|lecture)\\b"), t, perl = TRUE) |
    t %in% tolower(.publication_resource_types)
  # a record that also names data or software is not a publication
  out <- out & !grepl("\\b(dataset|data set|data collection|data|software|code)\\b", t, perl = TRUE)
  out[is.na(type) | !nzchar(t)] <- NA
  out
}

# Same-session memo of .repo_doi_records(), keyed by the lower-cased DOI
.repo_doi_cache <- new.env(parent = emptyenv())

# DataCite's record of each DOI: its registered address and resource type,
# several requests at once. `in_datacite` is FALSE for a DOI DataCite does not
# have, and NA when the request failed (not remembered, so asked again).
#
# @param doi a character vector of DOIs
# @returns a data frame with doi (lower case), url, resource_type, in_datacite
.repo_doi_records <- function(doi) {
  doi <- unique(tolower(as.character(doi)))
  doi <- doi[!is.na(doi) & nzchar(doi)]
  todo <- doi[!vapply(doi, exists, NA, envir = .repo_doi_cache, inherits = FALSE)]
  if (length(todo)) {
    resps <- .batch_query(paste0("https://api.datacite.org/dois/", utils::URLencode(todo)), msg = NULL)
    for (k in seq_along(todo)) {
      r <- resps[[k]]
      st <- if (inherits(r, "httr2_response")) httr2::resp_status(r) else NA_integer_
      a <- if (st %in% 200) tryCatch(httr2::resp_body_json(r)$data$attributes, error = \(e) NULL)
      val <- if (!is.null(a)) {
        list(url = a$url %||% NA_character_, resource_type = a$types$resourceTypeGeneral %||% NA_character_, in_datacite = TRUE)
      } else if (st %in% 404) {
        list(url = NA_character_, resource_type = NA_character_, in_datacite = FALSE)
      }
      if (!is.null(val)) assign(todo[k], val, envir = .repo_doi_cache)
    }
  }
  rec <- lapply(doi, \(d) if (exists(d, envir = .repo_doi_cache, inherits = FALSE)) get(d, envir = .repo_doi_cache) else
    list(url = NA_character_, resource_type = NA_character_, in_datacite = NA))
  data.frame(doi = doi,
             url = vapply(rec, \(x) as.character(x$url %||% NA), character(1)),
             resource_type = vapply(rec, \(x) as.character(x$resource_type %||% NA), character(1)),
             in_datacite = vapply(rec, \(x) as.logical(x$in_datacite), logical(1)))
}

# The address a DOI is registered with, unless DataCite calls it a
# publication: DataCite's registered address, else (a DOI DataCite does not
# have, or DataCite did not answer) the handle service's.
#
# @returns list(url, ok, publication): `ok` is FALSE when no lookup answered,
#   as .handle_registered_url() gives it
.repo_doi_url <- function(doi) {
  none <- list(url = NA_character_, ok = FALSE, publication = FALSE)
  if (length(doi) != 1 || is.na(doi) || !nzchar(doi)) return(none)
  r <- .repo_doi_records(doi)
  if (isTRUE(.repo_publication_type(r$resource_type)))
    return(list(url = NA_character_, ok = TRUE, publication = TRUE))
  if (!is.na(r$url)) return(list(url = r$url, ok = TRUE, publication = FALSE))
  reg <- tryCatch(.handle_registered_url(doi), error = \(e) none)
  list(url = reg$url %||% NA_character_, ok = isTRUE(reg$ok), publication = FALSE)
}
