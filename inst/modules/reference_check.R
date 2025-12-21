#' Reference Check
#'
#' @description
#' This module checks references. It warns for missing DOI's and other mismatches with CrossRef.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param crossref_min_score The minimum score to return a DOI match from `get_doi()`
#'
#' @returns report text
#'
#' @examples
#' module_run(psychsci[[129]], "reference_check")
reference_check <- function(paper, crossref_min_score = 50) {
  # for testing: paper <- psychsci[[109]]

  # table ----
  bib <- concat_tables(paper, "bib")

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      report = "We found no references",
      summary_text = "We found no references"
    )
    return(norefs)
  }

  # get DOIs and missing separately
  missing_dois <- is.na(bib$doi)
  sprintf("\nChecking %d reference%s with DOIs and %d reference%s without.",
          sum(!missing_dois),
          sum(!missing_dois) |> plural(),
          sum(missing_dois),
          sum(missing_dois) |> plural()
  ) |> message()
  with_doi <- crossref_doi(bib$doi[!missing_dois])
  without_doi <- crossref_query(bib$ref[missing_dois])
  order <- c(which(!missing_dois), which(missing_dois))
  table <- dplyr::bind_rows(with_doi, without_doi) |>
    sort_by(order)

  #table <- crossref_query(bib$ref)

  table$doi <- NULL
  table$ref <- format_ref(bib$ref)
  table$original_doi <- bib$doi
  table$id <- bib$id
  table$xref_id <- bib$xref_id

  # missing/mismatched DOIs
  table$doi_found <- is.na(bib$doi) & !is.na(table$DOI)
  table$doi_mismatch <- !is.na(bib$doi) & bib$doi != table$DOI

  ## other mismatches ----

  # clean up text to prevent irrelevant mismatches
  clean <- \(x) {
    tolower(x) |>
      gsub("\\s+", " ", x = _) |>
      gsub("[\u2018\u2019\u201A\u201B]", "'", x = _) |>
      gsub("[\u201C\u201D\u201E\u201F]", '"', x = _) |>
      gsub("\\.$", "", x = _)
  }

  table$title_mismatch <- {
    bib_title <- clean(bib$title)
    cr_title <- clean(table$title)
    pre_bib_title <- strsplit(bib_title, ":") |> sapply(`[[`, 1)
    pre_cr_title <- strsplit(cr_title, ":") |> sapply(`[[`, 1)

    !is.na(bib_title) & !is.na(cr_title) &
      bib_title != cr_title &
      pre_bib_title != pre_cr_title
  }

  table$author_mismatch <- {
    sapply(seq_along(table$author), \(i) {
      if (is.null(table$author[[i]]) || nrow(table$author[[i]]) == 0) return(FALSE)
      cr_auth <- clean(table$author[[i]]$family)
      bib_auth <- clean(bib$authors[[i]])
      in_auth <- sapply(cr_auth, grepl, x = bib_auth)

      !all(in_auth)
    })
  }


  # traffic_light ----
  tl <- "green"
  if (any(table$doi_found) || any(table$doi_mismatch)) {
    tl <- "yellow"
  }

  # summary_table ----
  summary_table <- dplyr::summarise(table, .by = id,
                   refs_checked = sum(!is.na(ref)),
                   missing_doi = sum(is.na(original_doi)),
                   doi_found = sum(doi_found),
                   doi_mismatch = sum(doi_mismatch))

  # summary_text
  summary_text <- sprintf(
    "We checked %d reference%s in CrossRef, %d of which had a DOI. We found %d missing DOI%s and %d DOI%s that did not match the original. We could not find matches for %d reference%s.",
    nrow(table),
    nrow(table) |> plural(),
    sum(!is.na(table$original_doi)),
    sum(table$doi_found, na.rm = TRUE),
    sum(table$doi_found, na.rm = TRUE) |> plural(),
    sum(table$doi_mismatch, na.rm = TRUE),
    sum(table$doi_mismatch, na.rm = TRUE) |> plural(),
    sum(is.na(table$DOI)),
    sum(is.na(table$DOI)) |> plural()
  )

  guidance <- "Double check any references listed in the tables below. Many books do not have a DOI or are not listed in CrossRef. Garbled references are usually a result of poor parsing of the paper by grobid; we are working on more accurate alternatives."

  if (tl == "green") guidance <- ""

  # report ----

  ## found table ----
  found_table <- table[table$doi_found, c("DOI", "ref"), drop = FALSE]
  if (nrow(found_table)) {
    found_table$DOI <- link(found_table$DOI, type = "doi")
    names(found_table) <- c("Found DOI", "Original Reference")
  }

  ## missing table ----
  missing_table <- table[is.na(table$DOI), c("ref"), drop = FALSE]
  if (nrow(missing_table)) {
    missing_table$type <- bib$bibtype[is.na(table$DOI)]
    names(missing_table) <- c("Reference not found in CrossRef", "Type")
  }

  ## doi mismatch table ----
  mismatch_table <- table[table$doi_mismatch, c("ref","original_doi", "DOI")]
  if (nrow(mismatch_table)) {
    mismatch_table$doi <- link(mismatch_table$original_doi, type = "doi")
    mismatch_table$DOI <- link(mismatch_table$DOI, type = "doi")
    names(mismatch_table) <- c("Reference", "Original DOI", "CrossRef DOI")
  }

  ## experimental checks ----
  title_table <- table[table$title_mismatch, c("ref", "title")]
  if (nrow(title_table)) {
    title_table$orig <- bib$title[table$title_mismatch]
    title_table <- title_table[, c("orig", "title", "ref")]
    names(title_table) <- c("Original Title", "CrossRef Title", "Reference")
  }

  author_table <- table[table$author_mismatch, c("ref", "author")]
  if (nrow(author_table)) {
    author_table$orig <- bib$authors[table$author_mismatch]
    author_table$cr <- sapply(author_table$author, \(a) {
      paste(substr(a$given, 1, 1), a$family, collapse = ", ")
    })
    author_table <- author_table[, c("orig", "cr", "ref")]
    names(author_table) <- c("Original Authors", "CrossRef Authors", "Reference")
  }

  exp_check <- ""
  if (nrow(title_table) || nrow(author_table)) {
    exp <- c(
      "Title and author mismatches are commonly false positives or minor differences in punctuation.",
      scroll_table(title_table)
    )
    exp_checks <- collapse_section(exp, "Experimental Checks")
  }

  report <- c(
    summary_text,
    guidance,
    scroll_table(found_table),
    scroll_table(mismatch_table),
    scroll_table(missing_table),
    exp_checks
  )


  # return a list ----
  list(
    table = table,
    summary_table = summary_table,
    na_replace = 0,
    traffic_light = tl,
    report = report,
    summary_text = summary_text
  )
}


