# The peer review of an article ----
#
# Where an article's peer review is public, it can be found in three places:
#   - the publisher's XML prints the reviews with the article (eLife, F1000,
#     part of PLOS): a paper read from such XML has them in its `review` and
#     `review_text` tables (bibr export schema 12.6);
#   - Crossref registers a review as a record of its own, with a DOI, linked
#     to the article it reviews (over a million records: Wiley, Copernicus,
#     the Royal Society of Chemistry, eLife, F1000, PeerJ, the Royal Society
#     and others). The record gives the kind of document, the round, the
#     date, the reviewer when named, and an address; not the text. For Wiley,
#     the Royal Society, the Royal Society of Chemistry, SAGE, IOP and others
#     the address is on Web of Science (earlier on Publons);
#   - Europe PMC's XML of an article in PubMed Central holds the reviews the
#     publisher printed, also for an article metacheck read from a PDF.
# The peer_review module (inst/modules/peer_review.R) puts these together.

#' Peer Reviews of an Article in Crossref
#'
#' Asks Crossref for the peer review documents registered for an article:
#' referee reports, decision letters and editors' reports, author responses.
#' Crossref gives what each document is and where it can be read, not its
#' text.
#'
#' @param doi the DOI of an article (one DOI)
#'
#' @returns a data frame with a row per document: `article_doi`, `doi` (the
#'   document's own DOI), `review_type` ("referee_report", "editor_report",
#'   "author_response", "other"), `stage` ("pre-publication" or
#'   "post-publication"), `round`, `date`, `reviewer` (the names given, NA
#'   for an anonymous review), `title` and `url`; no rows when Crossref has
#'   none, NULL when Crossref could not be asked
#' @export
#' @examples
#' \donttest{
#' if (online("api.crossref.org")) {
#'   crossref_peer_reviews("10.7554/eLife.80984")
#' }
#' }
crossref_peer_reviews <- function(doi) {
  empty <- data.frame(article_doi = character(0), doi = character(0), review_type = character(0),
                      stage = character(0), round = integer(0), date = character(0),
                      reviewer = character(0), title = character(0), url = character(0))
  doi <- doi_clean(doi[[1]])
  if (is.na(doi) || !nzchar(doi)) return(empty)
  # one request to Crossref: the response, or NULL when it could not be asked
  ask <- function(url, ...) {
    tryCatch({
      .mc_request(url) |>
        httr2::req_url_query(..., mailto = email()) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
    }, error = function(e) NULL)
  }
  found <- function(resp) httr2::resp_body_json(resp)$message$items

  # Crossref's search for the reviews of an article tells capital letters
  # apart, and publishers write the article's DOI in their own way (as
  # "10.1098/RSOB.210098" for reviews on Web of Science, "10.7554/eLife.80984",
  # "10.7717/peerj.1558"). So the reviews are asked for in two ways:
  # 1. the reviews of the DOI as given, in small letters and in capitals
  forms <- unique(c(doi, tolower(doi), toupper(doi)))
  resp <- ask("https://api.crossref.org/works",
              filter = paste0("type:peer-review,relation.type:is-review-of,",
                              paste0("relation.object:", forms, collapse = ",")),
              rows = 100)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  items <- found(resp)
  # 2. the reviews the article's own record lists (capitals do not matter
  #    there), for a DOI written in yet another way; a record made in the
  #    last weeks can be missing from this list, so it does not replace 1.
  art <- ask(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE)))
  art_ok <- !is.null(art) && httr2::resp_status(art) %in% c(200, 404)
  if (!art_ok && !length(items)) return(NULL)
  if (art_ok && httr2::resp_status(art) == 200) {
    listed <- httr2::resp_body_json(art)$message$relation$`has-review`
    listed <- unique(tolower(unlist(lapply(listed, function(x) x$id))))
    have <- tolower(vapply(items, function(it) it$DOI %||% "", character(1)))
    listed <- setdiff(listed, have)
    for (part in split(listed, ceiling(seq_along(listed) / 40))) {
      more <- ask("https://api.crossref.org/works",
                  filter = paste0("type:peer-review,", paste0("doi:", part, collapse = ",")), rows = 100)
      if (is.null(more) || httr2::resp_status(more) != 200) {
        if (!length(items)) return(NULL)
        break
      }
      items <- c(items, found(more))
    }
  }
  if (!length(items)) return(empty)
  kind <- c("referee-report" = "referee_report", "editor-report" = "editor_report",
            "author-comment" = "author_response", "community-comment" = "other",
            "manuscript" = "other", "aggregate" = "other", "recommendation" = "editor_report")
  rows <- lapply(items, function(it) {
    people <- c(it$author, it$editor)
    names <- vapply(people, function(a) {
      n <- trimws(paste(a$given %||% "", a$family %||% a$name %||% ""))
      if (nzchar(n)) n else NA_character_
    }, character(1))
    names <- names[!is.na(names) & !grepl("^anonymous", names, ignore.case = TRUE)]
    d <- unlist(it$issued$`date-parts`[[1]] %||% it$created$`date-parts`[[1]])
    data.frame(
      article_doi = doi, doi = tolower(it$DOI %||% NA_character_),
      review_type = unname(kind[it$review$type %||% ""]) %NA% "other",
      stage = it$review$stage %||% NA_character_,
      round = suppressWarnings(as.integer(sub("^[^0-9]*", "", it$review$`revision-round` %||% NA_character_))),
      date = if (length(d)) paste(sprintf("%02d", as.integer(d)), collapse = "-") else NA_character_,
      reviewer = if (length(names)) paste(names, collapse = "; ") else NA_character_,
      title = (it$title %||% list(NA_character_))[[1]] %||% NA_character_,
      url = it$resource$primary$URL %||% NA_character_)
  })
  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$doi), , drop = FALSE]
  # Crossref has one kind for what an editor writes: by its title, a decision
  # letter is told from an editor's assessment
  out$review_type[out$review_type %in% "editor_report" &
                    grepl("^decision letter", out$title, ignore.case = TRUE)] <- "decision_letter"
  out <- out[order(out$round, out$date, out$doi, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The peer reviews an article's XML at Europe PMC prints
#'
#' For an article in PubMed Central whose publisher printed the peer reviews
#' with it: the reviews and their paragraphs, read from Europe PMC's XML as
#' from a publisher's XML (see `.jats_extras()`).
#'
#' @param doi the article's DOI
#' @returns a list: `review` and `review_text` (data frames as a paper's
#'   tables of those names), or NULL when the article is not in PubMed
#'   Central, prints no reviews, or Europe PMC could not be asked
#' @noRd
.europepmc_peer_reviews <- function(doi) {
  tryCatch({
    base <- "https://www.ebi.ac.uk/europepmc/webservices/rest/"
    hit <- .mc_request(paste0(base, "search")) |>
      httr2::req_url_query(query = paste0("DOI:", dQuote(doi, FALSE)), format = "json", resultType = "lite") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    r <- Filter(function(x) identical(tolower(x$doi %||% ""), tolower(doi)) && !is.null(x$pmcid), hit$resultList$result)
    if (!length(r)) return(NULL)
    txt <- .mc_request(paste0(base, r[[1]]$pmcid, "/fullTextXML")) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform() |>
      httr2::resp_body_string()
    # only an XML that prints reviews is read
    if (!grepl("<sub-article", txt, fixed = TRUE)) return(NULL)
    xml <- xml2::read_xml(txt, options = "HUGE")
    xml2::xml_ns_strip(xml)
    art <- xml2::xml_find_first(xml, "//article")
    ex <- .jats_extras(art, list(bib = character(0), fig = character(0), tab = character(0), foot = character(0)))
    if (!length(ex$review)) return(NULL)
    one <- function(x) if (is.null(x) || !length(x)) NA else x[[1]]
    review <- do.call(rbind, lapply(seq_along(ex$review), function(i) {
      v <- ex$review[[i]]
      data.frame(review_id = i, review_type = as.character(one(v$review_type)), title = as.character(one(v$title)),
                 round = as.integer(one(v$round)), date = as.character(one(v$date)),
                 doi = as.character(one(v$doi)), url = as.character(one(v$url)))
    }))
    review_text <- do.call(rbind, lapply(ex$review_text, function(t) {
      data.frame(review_id = t$review_id, paragraph_id = t$paragraph_id, text = t$text)
    }))
    if (is.null(review_text)) review_text <- data.frame(review_id = integer(0), paragraph_id = integer(0), text = character(0))
    list(review = review, review_text = review_text, pmcid = r[[1]]$pmcid)
  }, error = function(e) NULL)
}
