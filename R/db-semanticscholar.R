#' Search Semantic Scholar by title
#'
#' Asks Semantic Scholar for the work whose title best matches each title.
#' Semantic Scholar holds conference papers without DOIs (ICML, NeurIPS,
#' ICLR), which Crossref and DataCite do not. Without a key, its requests
#' share a pool with all other users that often refuses them, so they are
#' sent about one a second, retried after a wait when refused, and asked
#' once more, more slowly, when still refused; when every title of a group
#' of five is refused, the rest are not asked. A free key
#' (https://www.semanticscholar.org/product/api#api-key-form) in the
#' environment variable `SEMANTIC_SCHOLAR_API_KEY` is used when set.
#'
#' @param title the title(s) to search for
#'
#' @returns a data frame with the index of the title searched for (`query`),
#'   and the `doi` (NA when the work has none), `url`, `title`, `year` (of
#'   the earliest version, often the preprint), `authors` ("Family, Given"
#'   names separated by "; "), `venue` and `type` ("Preprint" for a work
#'   found only on arXiv, otherwise "Conference", "JournalArticle" or NA) of
#'   the work found. A title whose search failed has a row with `title` NA
#'   and `error` TRUE; a title with no match has no rows.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' semanticscholar_query("Deep residual learning for image recognition")
#' }
semanticscholar_query <- function(title) {
  empty <- data.frame(query = integer(0), doi = character(0), url = character(0),
                      title = character(0), year = integer(0),
                      authors = character(0), venue = character(0),
                      type = character(0), error = logical(0))
  failed <- \(j) data.frame(query = j, doi = NA_character_, url = NA_character_,
                            title = NA_character_, year = NA_integer_,
                            authors = NA_character_, venue = NA_character_,
                            type = NA_character_, error = TRUE)
  todo <- which(!is.na(title) & nzchar(trimws(title)))
  if (!length(todo)) return(empty)
  if (!online("api.semanticscholar.org")) return(failed(todo))
  urls <- paste0(
    "https://api.semanticscholar.org/graph/v1/paper/search/match?fields=",
    "title,year,authors,venue,externalIds,publicationTypes,url&query=",
    utils::URLencode(title[todo], reserved = TRUE))
  key <- Sys.getenv("SEMANTIC_SCHOLAR_API_KEY")
  # Titles are asked in groups of five. When every title of a group is
  # refused, the service is taken as unavailable and the rest are not asked
  # (they count as failed searches), so that a refusing service does not
  # hold up the module.
  ask <- \(idx, every) {
    out <- vector("list", length(idx))
    for (g in split(seq_along(idx), ceiling(seq_along(idx) / 5))) {
      out[g] <- .batch_query(
        urls[idx[g]], msg = NULL, batch_size = length(g), delay = 0,
        req_func = \(req) if (nzchar(key)) httr2::req_headers(req, `x-api-key` = key, .redact = "x-api-key") else req,
        throttle_capacity = 1, throttle_fill_time_s = every,
        timeout_s = 15, max_tries = 3)
      if (all(vapply(out[g], refused, logical(1)))) break
    }
    out
  }
  refused <- \(r) !inherits(r, "httr2_response") || httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)
  resps <- ask(seq_along(urls), 1.1)
  # titles still refused after the retries are asked once more, more slowly
  again <- which(vapply(resps, refused, logical(1)))
  if (length(again)) {
    Sys.sleep(10)
    resps[again] <- ask(again, 3)
  }
  # a particle belongs to the family name ("van Dijk")
  particles <- "^(van|von|de|der|den|del|della|da|di|du|dos|le|la|bin|ibn)$"
  name_parts <- \(name) {
    w <- strsplit(trimws(name), "\\s+")[[1]]
    if (length(w) < 2) return(name)
    k <- length(w)
    while (k > 2 && grepl(particles, w[k - 1], ignore.case = TRUE)) k <- k - 1
    paste(paste(w[k:length(w)], collapse = " "),
          paste(w[seq_len(k - 1)], collapse = " "), sep = ", ")
  }
  lapply(seq_along(todo), \(j) {
    resp <- resps[[j]]
    if (inherits(resp, "httr2_response") && httr2::resp_status(resp) == 404)
      return(empty)  # no match
    if (is.null(resp) || inherits(resp, "error") || httr2::resp_status(resp) >= 400)
      return(failed(todo[j]))
    d <- tryCatch(httr2::resp_body_json(resp)$data[[1]], error = \(e) NULL)
    if (is.null(d)) return(failed(todo[j]))
    ids <- d$externalIds
    types <- unlist(d$publicationTypes)
    venue <- d$venue %||% NA_character_
    # an arXiv record without a venue, or with arXiv as its venue; missing
    # publication types say nothing
    arxiv_only <- !is.null(ids$ArXiv) && (is.null(ids$DOI) || grepl("^10\\.48550/", ids$DOI, ignore.case = TRUE)) &&
      (is.na(venue) || !nzchar(venue) || grepl("(?i)^(arxiv|corr)\\b", venue, perl = TRUE))
    data.frame(
      query = todo[j],
      doi = ids$DOI %||% NA_character_,
      url = d$url %||% NA_character_,
      title = d$title %||% NA_character_,
      year = as.integer(d$year %||% NA),
      authors = paste(vapply(d$authors, \(a) name_parts(a$name %||% ""), ""),
                      collapse = "; "),
      venue = venue,
      type = if (arxiv_only) "Preprint"
             else if ("Conference" %in% types) "Conference"
             else if ("JournalArticle" %in% types) "JournalArticle" else NA_character_,
      error = FALSE)
  }) |> dplyr::bind_rows(empty)
}
