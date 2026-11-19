# File-sharing services (#660): Google Drive, Dropbox, OneDrive and the like
# are not repositories. Files there can be changed or deleted at any time,
# they get no DOI, and a link stops working when the account is closed, so
# repo_check warns for every such link. The files are not read: these
# services give no access without a login or a key.

# The services, each with the pattern of its sharing links (host and path).
# Google Forms is a survey tool, not a place to share files, and is left out.
.file_share_services <- c(
  "Google Drive" = paste0("(?:drive\\.google\\.com|drive\\.usercontent\\.google\\.com)/[^\\s<>\"')\\]]*",
                          "|docs\\.google\\.com/(?:document|spreadsheets|presentation)/[^\\s<>\"')\\]]*"),
  "Dropbox" = "(?:www\\.)?dropbox\\.com/(?:s|sh|scl|l)/[^\\s<>\"')\\]]*|dl\\.dropboxusercontent\\.com/[^\\s<>\"')\\]]*",
  "OneDrive" = "1drv\\.ms/[^\\s<>\"')\\]]*|onedrive\\.live\\.com/[^\\s<>\"')\\]]*|[a-z0-9-]+\\.sharepoint\\.com/[^\\s<>\"')\\]]*",
  "Box" = "(?:[a-z0-9-]+\\.)*box\\.com/(?:s|shared|v)/[^\\s<>\"')\\]]*",
  "WeTransfer" = "(?:wetransfer\\.com/downloads|we\\.tl)/[^\\s<>\"')\\]]*",
  "MEGA" = "mega\\.(?:nz|co\\.nz|io)/(?:file|folder|#)[^\\s<>\"')\\]]*",
  "iCloud Drive" = "(?:www\\.)?icloud\\.com/iclouddrive/[^\\s<>\"')\\]]*",
  "Baidu Netdisk" = "pan\\.baidu\\.com/[^\\s<>\"')\\]]*",
  "SURFdrive" = "surfdrive\\.surf\\.nl/[^\\s<>\"')\\]]*"
)

.file_share_regex <- function() {
  paste0("(?i)(?:https?://)?(?:www\\.)?(?:", paste(.file_share_services, collapse = "|"), ")")
}

# The same, also matching the Internet Archive's address in front of a link
# ("web.archive.org/web/20220203164905/https://docs.google.com/..."): such a
# link is an archived copy, which lasts, so it is found in order to be left out
# (3 Collabra papers cited lists on Google Sheets this way).
.file_share_regex_archived <- function() {
  paste0("(?i)(?:(?:https?://)?web\\.archive\\.org/web/[0-9a-z_]+/)?(?:https?://)?(?:www\\.)?(?:",
         paste(.file_share_services, collapse = "|"), ")")
}

# The service a link belongs to (NA for none).
.file_share_service <- function(url) {
  out <- rep(NA_character_, length(url))
  for (s in rev(names(.file_share_services)))
    out[grepl(paste0("(?i)(?:^|//|\\.|\\b)(?:", .file_share_services[[s]], ")"), url, perl = TRUE)] <- s
  out
}

#' Find links to file-sharing services in a paper
#'
#' Finds links to file-sharing services that are not repositories: Google
#' Drive (and Google Docs, Sheets and Slides), Dropbox, OneDrive and
#' SharePoint, Box, WeTransfer, MEGA, iCloud Drive, Baidu Netdisk and
#' SURFdrive, as hyperlinks or in the text, the reference list included.
#' Files on these services can be changed or deleted at any time, get no DOI,
#' and the link stops working when the account is closed, so data and code
#' belong in a repository instead. Google Forms, a survey tool, is not
#' included.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with a row for each link (`href`, plus `text_id` /
#'   `paper_id`) and `service`, the name of the file-sharing service
#' @export
#'
#' @examples
#' paper <- test_paper(url = "https://www.dropbox.com/s/abc123/data.csv?dl=0")
#' file_share_links(paper)
file_share_links <- function(paper) {
  href <- text <- NULL
  none <- data.frame(href = character(0), service = character(0))
  rx <- .file_share_regex()
  found_href <- paper_table(paper, "url")
  found_href <- found_href[grepl(rx, found_href$href %||% character(0), perl = TRUE), , drop = FALSE]
  in_text <- text_search(paper, .file_share_regex_archived(), return = "match", perl = TRUE,
                         include_refs = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))
  links <- dplyr::bind_rows(found_href, in_text)
  if (!nrow(links)) return(none)
  # the punctuation that ends a sentence is not part of the link
  links$href <- sub("[.,;:]+$", "", trimws(links$href))
  links <- links[grepl(rx, links$href, perl = TRUE), , drop = FALSE]
  # an archived copy lasts (see .file_share_regex_archived()): no warning
  links <- links[!grepl("web\\.archive\\.org/web/", links$href, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(links)) return(none)
  if (!"paper_id" %in% names(links)) links$paper_id <- NA_character_
  keep <- intersect(c("paper_id", "href", "text_id"), names(links))
  links <- unique(links[, keep, drop = FALSE])
  links <- links[!duplicated(links[c("paper_id", "href")]), , drop = FALSE]
  # one link written twice, in full and shortened ("https://nihcc.app.box.com/v/x"
  # as a hyperlink, "app.box.com/v/x" read from the text): the full form stays
  core <- tolower(sub("^https?://(www\\.)?", "", links$href))
  part <- vapply(seq_len(nrow(links)), function(i) {
    same <- which(links$paper_id %in% links$paper_id[[i]] & seq_len(nrow(links)) != i)
    any(nchar(core[same]) > nchar(core[[i]]) & endsWith(core[same], core[[i]]))
  }, logical(1))
  links <- links[!part, , drop = FALSE]
  links$service <- .file_share_service(links$href)
  rownames(links) <- NULL
  links
}
