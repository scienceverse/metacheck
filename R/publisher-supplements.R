# Supplementary files a publisher hosts with the article ----
#
# A publisher's XML lists the article's supplementary files (the paper's
# `supplement` table, bibr export schema 12.6; see R/import-jats-extras.R):
# appendices, extra tables and figures, and sometimes the data and code.
# They are not in a repository, but they are files the authors shared with
# the paper, so repo_check lists them as one more place with files
# (`repo_type` "supplement", its address the article's DOI link), and
# data_check and code_check read the ones that can be downloaded.
#
# The XML gives a file's name, seldom its web address. The address is
#   - the one the XML gives (PLOS and the journals on Ubiquity's platform
#     give each file a DOI, which leads to the file);
#   - PubMed Central's copy of the file, when the article is there: the XML is
#     PubMed Central's own, or Europe PMC knows the article by its DOI;
#   - unknown otherwise. Wiley and SAGE host the files at a known address,
#     but refuse requests that do not come from a browser (as for their XML):
#     these files are listed, with the page a reader can get them from, and
#     not downloaded.

#' Find a Paper's Supplementary Files
#'
#' The papers whose publisher XML lists supplementary files. Each such paper
#' gets one link, the article's DOI link, which stands for "the supplementary
#' files the publisher hosts with this article".
#'
#' @param paper a paper object or paperlist object
#'
#' @returns a table with the columns `paper_id` and `href`
#' @export
#' @examples
#' paper <- read(demofile("xml"))
#' supplement_links(paper)
supplement_links <- function(paper) {
  empty <- dplyr::tibble(paper_id = character(0), href = character(0))
  supp <- tryCatch(paper_table(paper, "supplement"), error = \(e) NULL)
  if (!is.data.frame(supp) || !nrow(supp) || !"paper_id" %in% names(supp)) return(empty)
  info <- tryCatch(paper_table(paper, "info"), error = \(e) NULL)
  if (!is.data.frame(info) || !"doi" %in% names(info)) return(empty)
  ids <- unique(supp$paper_id)
  doi <- info$doi[match(ids, info$paper_id)]
  ok <- !is.na(doi) & nzchar(doi)
  dplyr::tibble(paper_id = ids[ok], href = paste0("https://doi.org/", doi[ok]))
}

#' List a Paper's Supplementary Files
#'
#' The supplementary files a paper's publisher XML lists, each with the web
#' address it can be downloaded from, when there is one: the address the XML
#' gives (a file's own DOI), or PubMed Central's copy when the article is in
#' PubMed Central. Files of publishers that refuse automatic downloads (Wiley,
#' SAGE) are listed without a download address, with the page they are on.
#'
#' @param paper a paper object or paperlist object
#' @param lookup whether to ask the web: Europe PMC for the article's PubMed
#'   Central copy, and a file's DOI for the file's name and size
#' @param cache whether to keep Europe PMC's answers in the lasting cache
#'
#' @returns a data frame with a row per file: `paper_id`, `repo_url` (the
#'   article's DOI link, as [supplement_links()] gives it), `file_name`,
#'   `file_path`, `file_url` (NA when the file cannot be downloaded),
#'   `file_size`, `label`, `caption`, `source` (where the address is from),
#'   `page_url` (the page a reader can get the file from) and `holds` (for
#'   Europe PMC's zip of the files: the names of the files the XML lists)
#' @export
#' @examples
#' paper <- read(demofile("xml"))
#' supplement_files(paper, lookup = FALSE)
supplement_files <- function(paper, lookup = TRUE, cache = FALSE) {
  empty <- data.frame(paper_id = character(0), repo_url = character(0), file_name = character(0),
                      file_path = character(0), file_url = character(0), file_size = numeric(0),
                      label = character(0), caption = character(0), source = character(0),
                      page_url = character(0), holds = character(0))
  links <- supplement_links(paper)
  if (!nrow(links)) return(empty)
  supp <- paper_table(paper, "supplement")
  info <- paper_table(paper, "info")
  known_ext <- metacheck::file_types$ext
  has_ext <- function(name) !is.na(name) & tolower(sub("^.*\\.", "", name)) %in% known_ext & grepl("\\.", name)

  rows <- lapply(seq_len(nrow(links)), function(k) {
    id <- links$paper_id[k]
    s <- supp[supp$paper_id %in% id, , drop = FALSE]
    i <- info[match(id, info$paper_id), , drop = FALSE]
    doi <- sub("^https://doi.org/", "", links$href[k])
    n <- nrow(s)
    out <- data.frame(
      paper_id = id, repo_url = links$href[k], file_name = as.character(s$file_name),
      file_path = as.character(s$file_name), file_url = NA_character_, file_size = NA_real_,
      label = as.character(s$label), caption = as.character(s$caption),
      source = NA_character_, page_url = .supplement_page(doi, s$file_name), holds = NA_character_)

    # 1. the address the XML gives: a web address, or the file's own DOI
    own <- ifelse(!is.na(s$url) & grepl("^https?://", s$url), s$url,
                  ifelse(!is.na(s$doi), paste0("https://doi.org/", s$doi), NA_character_))
    for (j in which(!is.na(own))) {
      out$file_url[j] <- own[j]
      out$source[j] <- "address in the XML"
      # a file's DOI does not tell its name ("journal.pone.0230416.s001")
      if (!has_ext(out$file_name[j]) && isTRUE(lookup)) {
        r <- .supplement_resolve(own[j])
        if (!is.na(r$file_name)) out$file_name[j] <- out$file_path[j] <- r$file_name
        out$file_size[j] <- r$file_size
      }
    }

    # 2. PubMed Central's copy, for the files without an address: Europe PMC
    # gives an article's supplementary files as one zip file (PubMed Central's
    # own address of a file shows a browser check). The zip replaces the
    # files it holds; repo_check and data_check look inside it.
    todo <- which(is.na(out$file_url))
    if (length(todo)) {
      pmcid <- i$pmcid %||% NA_character_
      if ((is.na(pmcid) || !nzchar(pmcid)) && isTRUE(lookup)) {
        pmcid <- .europepmc_supplements(doi, cache = cache)
      }
      if (!is.na(pmcid) && nzchar(pmcid)) {
        zip_name <- paste0(pmcid, "_supplementary_files.zip")
        zip <- data.frame(
          paper_id = id, repo_url = links$href[k], file_name = zip_name, file_path = zip_name,
          file_url = paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/", pmcid,
                            "/supplementaryFiles?includeInlineImage=false"),
          file_size = NA_real_, label = NA_character_,
          caption = NA_character_,
          source = "Europe PMC", page_url = paste0("https://europepmc.org/article/PMC/", pmcid),
          holds = paste(stats::na.omit(out$file_name[todo]), collapse = "; "))
        out <- rbind(out[setdiff(seq_len(n), todo), , drop = FALSE], zip)
      }
    }
    out$source[is.na(out$file_url)] <- "no address for automatic download"
    out
  })
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$file_name) & nzchar(out$file_name), , drop = FALSE]
  # a file the XML lists twice (in the text and in the back matter) is one file
  out <- out[!duplicated(out[c("paper_id", "file_name")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The page a reader can get a supplementary file from
#'
#' Wiley and SAGE host supplementary files at an address made of the
#' article's DOI and the file's name; for other publishers it is the article's
#' own page.
#' @noRd
.supplement_page <- function(doi, file_name) {
  file <- basename(as.character(file_name))
  enc <- vapply(file, \(f) utils::URLencode(f %NA% "", reserved = TRUE), character(1), USE.NAMES = FALSE)
  if (grepl("^10\\.(1111|1002)/", doi)) {
    return(paste0("https://onlinelibrary.wiley.com/action/downloadSupplement?doi=",
                  utils::URLencode(doi, reserved = TRUE), "&file=", enc))
  }
  if (grepl("^10\\.1177/", doi)) {
    return(paste0("https://journals.sagepub.com/doi/suppl/", doi, "/suppl_file/", enc))
  }
  rep(paste0("https://doi.org/", doi), length(file))
}

#' A file's name and size, from the address its DOI or link leads to
#'
#' Follows the redirects of a HEAD request: the name is the one the server
#' gives (Content-Disposition), else the last part of the final address.
#' @returns a list: `file_name` and `file_size` (NA when not found)
#' @noRd
.supplement_resolve <- function(url) {
  none <- list(file_name = NA_character_, file_size = NA_real_)
  tryCatch({
    resp <- .mc_request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200) return(none)
    cd <- httr2::resp_header(resp, "Content-Disposition") %empty_or% ""
    name <- regmatches(cd, regexpr("filename\\*?=[^;]+", cd))
    name <- if (length(name)) gsub("^filename\\*?=(UTF-8'')?|[\"']", "", name) else
      basename(sub("[?#].*$", "", resp$url))
    name <- utils::URLdecode(name)
    if (!grepl("\\.[A-Za-z0-9]{1,6}$", name)) name <- NA_character_
    len <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Content-Length") %empty_or% NA_real_))
    list(file_name = name, file_size = len)
  }, error = \(e) none)
}


#' The PubMed Central id of an article with supplementary files, from Europe PMC
#'
#' Europe PMC is asked for the article by its DOI.
#'
#' @param doi the article's DOI
#' @param cache whether to keep the answer in the lasting cache
#' @returns the PMCID, or NA when the article is not in PubMed Central, has no
#'   supplementary files there, or Europe PMC does not answer
#' @noRd
.europepmc_supplements <- function(doi, cache = FALSE) {
  cached <- if (isTRUE(cache)) .repo_info_cache_get("europepmc_supplements", doi) else NULL
  if (!is.null(cached)) return(cached$pmcid)
  pmcid <- tryCatch({
    hit <- .mc_request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
      httr2::req_url_query(query = paste0("DOI:", dQuote(doi, FALSE)), format = "json", resultType = "core") |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    r <- Filter(function(x) identical(tolower(x$doi %||% ""), tolower(doi)) && !is.null(x$pmcid) &&
                  identical(x$hasSuppl, "Y"), hit$resultList$result)
    if (length(r)) r[[1]]$pmcid else NA_character_
  }, error = function(e) NULL)
  if (is.null(pmcid)) return(NA_character_) # no answer: not kept
  if (isTRUE(cache)) .repo_info_cache_put("europepmc_supplements", doi, list(pmcid = pmcid))
  pmcid
}

#' Mark the supplementary files that are also in one of the paper's repositories
#'
#' Authors often give the publisher a copy of what they also deposited. Such
#' a copy need not be downloaded and checked a second time. A supplementary
#' file is a copy when a file of a repository of the same paper has the same
#' name (upper and lower case alike) and, where both sizes are known, the
#' same size. Europe PMC's zip of an article's supplementary files is a copy
#' when every file the XML lists for it is.
#'
#' @param supp the supplementary files (rows of repo_check's file table)
#' @param repo_files the files of the paper's repositories (the same columns)
#' @param listed the table [supplement_files()] returned, for the names of
#'   the files a zip holds (`holds`)
#' @returns `supp` with the column `copy_of`: the address of the repository
#'   that holds the same file, NA when none does
#' @noRd
.supplement_copies <- function(supp, repo_files, listed = NULL) {
  supp$copy_of <- rep(NA_character_, nrow(supp))
  if (!nrow(supp) || !is.data.frame(repo_files) || !nrow(repo_files)) return(supp)
  key <- function(x) tolower(basename(as.character(x)))
  same_file <- function(paper, name, size) {
    r <- repo_files[repo_files$paper_id %in% paper & key(repo_files$file_name) %in% key(name), , drop = FALSE]
    if (!is.na(size) && nrow(r)) r <- r[is.na(r$file_size) | r$file_size == size, , drop = FALSE]
    if (nrow(r)) r$repo_url[1] else NA_character_
  }
  for (j in seq_len(nrow(supp))) {
    supp$copy_of[j] <- same_file(supp$paper_id[j], supp$file_name[j], supp$file_size[j])
    # a zip that was not opened: the files the XML lists for it
    held <- if (is.data.frame(listed) && "holds" %in% names(listed)) {
      listed$holds[listed$paper_id %in% supp$paper_id[j] & listed$file_name %in% supp$file_name[j]]
    } else character(0)
    held <- unlist(strsplit(held[!is.na(held)], "; ", fixed = TRUE))
    if (is.na(supp$copy_of[j]) && length(held)) {
      found <- vapply(held, \(h) same_file(supp$paper_id[j], h, NA_real_), character(1))
      if (!anyNA(found)) supp$copy_of[j] <- found[[1]]
    }
  }
  supp
}

#' repo_check's report on the supplementary files
#'
#' @param supp the supplementary files, with `copy_of` (see
#'   `.supplement_copies()`), `file_type`, `file_url` and `page_url`
#' @returns a list: `summary` (one sentence, or NULL without files) and
#'   `report` (the report section, or NULL)
#' @noRd
.supplement_report <- function(supp) {
  if (!is.data.frame(supp) || !nrow(supp)) return(list(summary = NULL, report = NULL))
  n <- nrow(supp)
  copy <- !is.na(supp$copy_of)
  no_address <- is.na(supp$file_url) & !copy & !grepl("[.]zip/", supp$file_path %||% "")
  types <- table(ifelse(is.na(supp$file_type), "other", supp$file_type))
  types <- paste(sprintf("%d %s", as.integer(types), names(types)), collapse = ", ")
  parts <- c(
    sprintf("The publisher hosts %d supplementary file%s with the article (%s).", n, plural(n), types),
    if (any(copy)) sprintf("%d of them %s also in a repository the paper links to, and %s not checked a second time.",
                           sum(copy), plural(sum(copy), "is", "are"), plural(sum(copy), "is", "are")),
    if (any(no_address)) sprintf("%d could not be downloaded automatically: the publisher's website refuses automatic downloads.",
                                 sum(no_address)))
  shown <- data.frame(
    File = ifelse(is.na(supp$page_url), supp$file_path,
                  sprintf("<a href='%s' target='_blank'>%s</a>", supp$page_url, supp$file_path)),
    Type = ifelse(is.na(supp$file_type), "", supp$file_type),
    Status = ifelse(copy, paste("also in", supp$copy_of),
                    ifelse(no_address, "not downloaded: the publisher refuses automatic downloads", "checked")))
  list(summary = paste(parts, collapse = " "),
       report = c("#### Supplementary files hosted by the publisher",
                  paste(parts, collapse = " "),
                  paste("Files a publisher hosts get no DOI of their own and cannot be found or cited apart from the article.",
                        "Data and code are easier to find and reuse in a repository such as Zenodo or the OSF."),
                  scroll_table(shown, maxrows = 10)))
}
