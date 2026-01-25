#' Reference Accuracy
#'
#' @description
#' This module checks references for mismatches with CrossRef.
#'
#' @details
#' It looks up the DOIs originally present in your paper (not those found by ref_doi_check) and returns the bibliographic information.
#'
#' We then check that the title from your reference section is the same as the retrieved title (ignoring differences in capitalisation) and that all author last names in your reference section are also in the retrieved author list (we do not check first names or order yet). This check is done for all references with crossref entries, including those found by ref_doi_check, if it was previously run.
#'
#' Mismatches may be because of problems with our parsing of references from your PDF (we're working on improving this), incorrect formatting in CrossRef, or minor differences in punctuation.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#'
#' @returns report list
ref_accuracy <- function(paper) {
  # for testing: paper <- psychsci[[109]]

  # table ----
  all_bib <- concat_tables(paper, "bib")
  bib <- all_bib[!is.na(all_bib$doi), ]

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      summary_text = "We found no references with DOIs"
    )
    return(norefs)
  }

  ## get papers with DOIs ----
  table <- crossref_doi(bib$doi)
  table$ref <- format_ref(bib$ref)
  table$id <- bib$id
  table$xref_id <- bib$xref_id

  # deal with crossref errors
  if ("error" %in% names(table) & all(!is.na(table$error))) {
    error <- list(
      traffic_light = "fail",
      summary_text = "We could not check reference accuracy because of a problem with CrossRef.",
      table = table
    )
    return(error)
  }

  # add in missing DOI is there
  missing_doi <- get_prev_outputs("ref_doi_check", "table")
  # missing_doi <- module_run(paper, "ref_doi_check")$table
  table <- dplyr::bind_rows(table, missing_doi)

  # missing references
  table$ref_not_found <- !is.na(table$DOI) & is.na(table$type)

  ## other mismatches ----
  aut_title <- dplyr::select(all_bib,
                             id, xref_id,
                             orig_authors = authors,
                             orig_title = title)
  table <- dplyr::left_join(table, aut_title, by = c("id", "xref_id"))

  # clean up text to prevent irrelevant mismatches
  clean <- \(x) {
    tolower(x) |>
      gsub("\\s+", " ", x = _) |>
      gsub("[\u2018\u2019\u201A\u201B]", "'", x = _) |>
      gsub("[\u201C\u201D\u201E\u201F]", '"', x = _) |>
      gsub("\\.$", "", x = _)
  }

  table$title_mismatch <- {
    bib_title <- clean(table$orig_title)
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
      bib_auth <- clean(table$orig_authors[[i]])
      in_auth <- sapply(cr_auth, grepl, x = bib_auth)

      !all(in_auth)
    })
  }

  # remove unchecked, don't do earlier for matching with bib
  table <- table[!is.na(table$DOI), ]

  # traffic_light ----
  tl <- "green"
  if (any(table$ref_not_found) ||
      any(table$title_mismatch) ||
      any(table$author_mismatch)) {
    tl <- "yellow"
  }

  # summary_table ----
  summary_table <- dplyr::summarise(table, .by = id,
                                    refs_checked = sum(!is.na(DOI)),
                                    refs_not_found = sum(ref_not_found),
                                    title_mismatch = sum(title_mismatch),
                                    author_mismatch = sum(author_mismatch)
                                    )

  # summary_text
  summary_text <- sprintf(
    "We checked %d reference%s with DOIs in CrossRef and found  matches for %d.",
    nrow(table),
    nrow(table) |> plural(),
    sum(!table$ref_not_found)
  )

  guidance <- "Double check any references listed in the tables below. This tool has a high false positive rate."

  if (tl == "green") guidance <- ""

  # report ----

  ## unfound table ----
  unfound_table <- table[table$ref_not_found, c("ref"), drop = FALSE]
  names(unfound_table) <- c("Unfound Reference")

  ## title mismatches ----
  title_table <- table[table$title_mismatch, c("orig_title", "title", "ref")]
  names(title_table) <- c("Original Title", "CrossRef Title", "Reference")

  ## author mismatches ----
  author_table <- table[table$author_mismatch, c("orig_authors", "author", "ref")]
  if (nrow(author_table)) {
    author_table$cr <- sapply(author_table$author, \(a) {
      paste(substr(a$given, 1, 1), a$family, collapse = ", ")
    })
    author_table <- author_table[, c("orig_authors", "cr", "ref")]
    names(author_table) <- c("Original Authors", "CrossRef Authors", "Reference")
  }

  report <- c(
    guidance,
    scroll_table(unfound_table, 5),
    scroll_table(title_table, 5),
    scroll_table(author_table, 5)
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


