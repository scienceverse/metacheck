# Handle links (#660): "hdl.handle.net/10411/XYZ" names a deposit by its
# Handle, which leads to the deposit's address on its host (DataverseNL, a
# DSpace installation, an institutional Figshare). The Handle system's public
# interface gives that address in one request; when one of repo_check's
# finders recognises it, the deposit is checked like a link to that address.
# In 19 corpora (18,708 papers) there were 119 Handle links in 96 papers; of
# 40 sampled, 36 resolved and 12 led to a host metacheck reads (most of the
# others to theses and articles in institutional repositories).

# repo_check's link finders, by repository type, for reading a list of
# addresses (the reference list, resolved Handles) with the same rules
.repo_link_finders <- function() {
  list(osf = osf_links, github = github_links, gitlab = gitlab_links, researchbox = rbox_links,
       dspace = dspace_links, dspace7 = dspace7_links, zenodo = invenio_links,
       dataverse = dataverse_links, figshare = figshare_links, dryad = dryad_links,
       reshare = eprints_links, researchdata4tu = researchdata4tu_links,
       mendeley = mendeley_links, dataone = dataone_links, ldbase = ldbase_links,
       ckan = ckan_links)
}

# The link a paper gave for an address found by resolving it (a Handle), so
# the ownership step finds the sentence that cites it (see .own_needle()).
# Kept for the session.
.repo_link_alias <- new.env(parent = emptyenv())
.repo_link_alias_of <- function(h) {
  vapply(h, function(x) {
    k <- .own_core(x)
    if (!is.na(k) && exists(k, envir = .repo_link_alias, inherits = FALSE))
      get(k, envir = .repo_link_alias, inherits = FALSE) else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# Resolved Handles, for the session
.handle_cache <- new.env(parent = emptyenv())
.handle_resolve <- function(handle) {
  if (exists(handle, envir = .handle_cache, inherits = FALSE)) return(get(handle, envir = .handle_cache))
  r <- .handle_registered_url(handle, api = "https://hdl.handle.net/api/handles/")
  if (isTRUE(r$ok)) assign(handle, r$url, envir = .handle_cache)
  r$url %||% NA_character_
}

.handle_rx <- "(?i)(?:https?://)?(?:hdl\\.)?handle\\.net/([0-9]{2,5}(?:\\.[0-9]+)*/[^\\s<>\"')\\],;]+)"

#' Find deposits cited by their Handle
#'
#' Finds Handle links (`hdl.handle.net/<prefix>/<suffix>`) in a paper, asks
#' the Handle system where each leads, and keeps those that lead to a
#' repository the repo_check module reads (a Dataverse installation such as
#' DataverseNL or Borealis, a DSpace installation, an institutional Figshare),
#' as a link to that address. A Handle that leads elsewhere (a thesis in an
#' institutional repository, a web page) is left out.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with a row for each deposit: `href` (the address the
#'   Handle leads to), `repo_type` (the repository type, as repo_check names
#'   it), `handle` (the link the paper gave), and `text_id` / `paper_id`
#' @export
#'
#' @examples
#' \donttest{
#' if (online("hdl.handle.net")) {
#'   handle_links(test_paper(text = "Data: http://hdl.handle.net/11667/195"))
#' }
#' }
handle_links <- function(paper) {
  none <- data.frame(paper_id = character(0), href = character(0), repo_type = character(0),
                     handle = character(0), text_id = integer(0))
  href <- text <- NULL
  found_href <- paper_table(paper, "url")
  found_href <- found_href[grepl(.handle_rx, found_href$href %||% character(0), perl = TRUE), , drop = FALSE]
  in_text <- text_search(paper, .handle_rx, return = "match", perl = TRUE, include_refs = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))
  links <- dplyr::bind_rows(found_href, in_text)
  if (!nrow(links)) return(none)
  if (!"paper_id" %in% names(links)) links$paper_id <- NA_character_
  if (!"text_id" %in% names(links)) links$text_id <- NA_integer_
  links$href <- sub("[.]+$", "", trimws(links$href))
  links$handle <- sub(.handle_rx, "\\1", links$href, perl = TRUE)
  links <- links[!duplicated(links[c("paper_id", "handle")]), c("paper_id", "href", "handle", "text_id"), drop = FALSE]
  links$resolved <- vapply(links$handle, function(h) tryCatch(.handle_resolve(h), error = \(e) NA_character_),
                           character(1), USE.NAMES = FALSE)
  links <- links[!is.na(links$resolved) & nzchar(links$resolved), , drop = FALSE]
  if (!nrow(links)) return(none)
  # which finder knows the address: each address read as one sentence
  q <- .own_reference_paper("handles", links$resolved)
  finders <- .repo_link_finders()
  hits <- dplyr::bind_rows(lapply(names(finders), function(k) {
    x <- tryCatch(suppressMessages(suppressWarnings(finders[[k]](q))), error = \(e) NULL)
    if (!is.data.frame(x) || !nrow(x) || !"href" %in% names(x)) return(NULL)
    data.frame(href = sub("[.,;:]+$", "", x$href), i = x$text_id %||% NA_integer_, repo_type = k)
  }))
  if (is.null(hits) || !nrow(hits)) return(none)
  hits <- hits[!is.na(hits$i), , drop = FALSE]
  hits <- hits[!duplicated(hits$i), , drop = FALSE]
  out <- data.frame(paper_id = links$paper_id[hits$i], href = hits$href, repo_type = hits$repo_type,
                    handle = links$href[hits$i], text_id = links$text_id[hits$i])
  # the ownership step looks for the Handle in the paper's sentences
  for (j in seq_len(nrow(out))) assign(.own_core(out$href[[j]]), out$handle[[j]], envir = .repo_link_alias)
  rownames(out) <- NULL
  out
}
