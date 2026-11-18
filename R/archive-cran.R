# CRAN (#478, #660): a paper that presents an R package often shares its code
# on CRAN only. A CRAN package lists its authors (CRAN requires it), so whose
# package a link names is reliable: in 40 sampled links from 19 corpora, a
# paper author was a package author in all 18 packages the paper presented
# and in none of the 22 it only used (lme4, vegan, ggplot2). The paper's own
# package is listed, not downloaded: CRAN has checked it, and its code is a
# package, not the paper's analysis.

# A CRAN package link, as papers write it (#478): the package page in its
# forms (".../web/packages/<pkg>", ".../package=<pkg>", once "package-<pkg>"),
# a file of the package (its manual, a vignette), and the package DOI.
.cran_rx <- paste0("(?i)cran\\.r-project\\.org/(?:web/packages/|package=|package-)([A-Za-z][A-Za-z0-9.]*[A-Za-z0-9])",
                   "|10\\.32614/CRAN\\.package\\.([A-Za-z][A-Za-z0-9.]*[A-Za-z0-9])")

.cran_url <- function(pkg) paste0("https://CRAN.R-project.org/package=", pkg)
.cran_pkg_of <- function(url) {
  out <- rep(NA_character_, length(url))
  m <- regmatches(url, regexec(.cran_rx, url, perl = TRUE))
  for (i in seq_along(m)) if (length(m[[i]]) == 3) out[[i]] <- if (nzchar(m[[i]][2])) m[[i]][2] else m[[i]][3]
  out
}

#' Find CRAN package links in a paper
#'
#' Finds links to R packages on CRAN: the package page in its usual forms
#' (`https://CRAN.R-project.org/package=<name>`,
#' `cran.r-project.org/web/packages/<name>/index.html`), a file of the package
#' (its manual or a vignette), and the package DOI
#' (`10.32614/CRAN.package.<name>`). The repo_check module reads each package's record
#' to tell the paper's own package from one it uses.
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a data frame with a row for each package: `href` (the package's
#'   CRAN address), `package`, `repo_type` ("cran"), `link` (the link as the
#'   paper writes it), and `text_id` / `paper_id`
#' @export
#'
#' @examples
#' p <- test_paper(text = "Our package is on CRAN: https://CRAN.R-project.org/package=metacheck.")
#' cran_links(p)
cran_links <- function(paper) {
  none <- data.frame(paper_id = character(0), href = character(0), package = character(0),
                     repo_type = character(0), link = character(0), text_id = integer(0))
  href <- text <- NULL
  found_href <- paper_table(paper, "url")
  found_href <- found_href[grepl(.cran_rx, found_href$href %||% character(0), perl = TRUE), , drop = FALSE]
  in_text <- text_search(paper, .cran_rx, return = "match", perl = TRUE) |>
    dplyr::select(href = text, dplyr::any_of(c("text_id", "paper_id")))
  links <- dplyr::bind_rows(found_href, in_text)
  if (!nrow(links)) return(none)
  if (!"paper_id" %in% names(links)) links$paper_id <- NA_character_
  if (!"text_id" %in% names(links)) links$text_id <- NA_integer_
  links$package <- .cran_pkg_of(links$href)
  links <- links[!is.na(links$package), , drop = FALSE]
  # the text's own form first, as the sentence holds it
  links <- links[order(is.na(links$text_id)), , drop = FALSE]
  links <- links[!duplicated(links[c("paper_id", "package")]), , drop = FALSE]
  if (!nrow(links)) return(none)
  out <- data.frame(paper_id = links$paper_id, href = .cran_url(links$package), package = links$package,
                    repo_type = "cran", link = links$href, text_id = links$text_id)
  # the ownership step looks for the link as the paper writes it
  for (j in seq_len(nrow(out))) assign(.own_core(out$href[[j]]), out$link[[j]], envir = .repo_link_alias)
  rownames(out) <- NULL
  out
}

# "Roger Bivand [cre, aut] (<https://orcid.org/...>), Nicholas Lewin-Koh
# [aut]" -> "Roger Bivand; Nicholas Lewin-Koh"
.cran_people <- function(author) {
  a <- gsub("\\[[^]]*\\]|\\([^)]*\\)|<[^>]*>", " ", author %||% "")
  p <- trimws(unlist(strsplit(a, ",|\\band\\b|\n")))
  p <- gsub("\\s+", " ", p[nchar(p) > 1])
  if (length(p)) paste(unique(p), collapse = "; ") else NA_character_
}

#' Get the records of CRAN packages
#'
#' Reads each package's record from crandb (the CRAN package database), also
#' for an archived package: its title, authors, version, publication date
#' and description.
#'
#' @param cran_url the packages' CRAN addresses, as [cran_links()] gives them
#'   in `href`, or a data frame with them in column `id_col`
#' @param id_col the column of `cran_url` holding the addresses, if it is a
#'   data frame
#' @param pb a progress bar, or NULL
#' @param cache whether to keep each answer for the session
#'
#' @returns a data frame with a row per address: `cran_url`, `package`,
#'   `title`, `creators` (names separated by "; "), `version`,
#'   `publication_date`, `description`, `resource_type` ("Software"), and
#'   `error` ("unfound" when CRAN has no such package)
#' @export
#'
#' @examples
#' \donttest{
#' if (online("crandb.r-pkg.org")) cran_info("https://CRAN.R-project.org/package=Reol")
#' }
cran_info <- function(cran_url, id_col = 1, pb = NULL, cache = FALSE) {
  urls <- if (is.data.frame(cran_url)) cran_url[[id_col]] else cran_url
  urls <- unique(stats::na.omit(as.character(urls)))
  urls <- urls[nzchar(urls)]
  if (!length(urls)) {
    return(data.frame(cran_url = character(0), package = character(0), title = character(0),
                      creators = character(0), version = character(0), publication_date = character(0),
                      description = character(0), resource_type = character(0), error = character(0)))
  }
  if (is.null(pb)) { pb <- pb(NA, "(:spin) :what"); on.exit(pb$terminate()) }
  rows <- lapply(urls, function(u) {
    cached <- if (isTRUE(cache)) .repo_info_cache_get("cran", u) else NULL
    if (!is.null(cached)) return(cached)
    pkg <- .cran_pkg_of(u)
    pb$tick(0, list(what = paste0("* Retrieving the CRAN record of ", pkg, "...")))
    obj <- data.frame(cran_url = u, package = pkg, title = NA_character_, creators = NA_character_,
                      version = NA_character_, publication_date = NA_character_, description = NA_character_,
                      resource_type = "Software", error = NA_character_)
    txt <- .seq_get(paste0("https://crandb.r-pkg.org/", pkg))
    j <- tryCatch(jsonlite::fromJSON(txt %||% ""), error = \(e) NULL)
    if (is.null(j) || is.null(j$Package)) {
      obj$error <- "unfound"
    } else {
      obj$title <- gsub("\\s+", " ", j$Title %||% NA_character_)
      obj$creators <- .cran_people(j$Author %||% j$Maintainer %||% NA_character_)
      obj$version <- j$Version %||% NA_character_
      obj$publication_date <- substr(j$`Date/Publication` %||% j$Date %||% NA_character_, 1, 10)
      obj$description <- substr(gsub("\\s+", " ", j$Description %||% NA_character_), 1, 2000)
    }
    if (isTRUE(cache) && .repo_info_ok(obj)) .repo_info_cache_put("cran", u, obj)
    obj
  })
  dplyr::bind_rows(rows)
}
