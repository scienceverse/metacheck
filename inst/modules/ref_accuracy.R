#' Reference Accuracy
#'
#' @description
#' This module checks references for mismatches with CrossRef.
#'
#' @details
#' This module uses the bib_match table from each paper (this can be added or refreshed using `add_bib_match()`) to detect possible problems in the reference section.
#'
#' We check that the title from your reference section is the same as the retrieved title (ignoring differences in capitalisation) and that all author last names in your reference section are also in the retrieved author list (we do not check first names or order yet). This check is done for all references with crossref entries in the bib_match table.
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
  # table ----
  bib <- paper_table(paper, "bib")
  bib_match <- paper_table(paper, "bib_match")

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      summary_text = "We found no references"
    )
    return(norefs)
  }

  if (nrow(bib_match) == 0) {
    norefs <- list(
      traffic_light = "error",
      summary_text = "We found no bib_match entries. You may need to add them with `add_bib_match()`."
    )
    return(norefs)
  }

  cols <- c("paper_id", "bib_id", "doi", "title", "year", "authors")
  ref_table <- ref_table(paper)
  ref_table$doi <- NULL
  table <- dplyr::inner_join(
    bib[, cols], bib_match[, cols],
    by = c("paper_id", "bib_id"),
    suffix = c(".orig", ".match")
  ) |>
    dplyr::left_join(ref_table, by = c("paper_id", "bib_id"))


  table$doi_mismatch <- tolower(table$doi.orig) != tolower(table$doi.match)
  table$year_mismatch <- table$year.orig != table$year.match

  # clean up text to prevent irrelevant mismatches
  clean <- \(x) {
    tolower(x) |>
      gsub("</?[a-z]+>", "", x = _) |>
      gsub("\\p{Pd}", "", x = _, perl = TRUE) |> # remove dashes
      gsub("\\s+", " ", x = _) |>
      gsub("[\u2018\u2019\u201A\u201B\u0060]", "'", x = _) |>
      # make all single quotes, even doubles
      gsub("[\"\u201C\u201D\u201E\u201F]", "'", x = _) |>
      gsub("\\.\\s*$", "", x = _) # remove . at end
  }

  table$title_mismatch <- {
    bib_title <- clean(table$title.orig)
    match_title <- clean(table$title.match)
    pre_bib_title <- gsub(":.*", "", bib_title)
    pre_match_title <- gsub(":.*", "", match_title)

    # find mismatches
    mm <- !is.na(bib_title) & !is.na(match_title) &
      bib_title != match_title &
      pre_bib_title != pre_match_title

    # find titles in text that aren't parsed into title correctly
    clean_text <- clean(table$text)
    in_text <- mapply(\(pattern, x) grepl(pattern, x, fixed = TRUE),
           pattern = match_title, x = clean_text, USE.NAMES = FALSE)

    # mismatched excluding unparsed matches in text
    mm & !in_text
  }

  # TODO: make this better
  table$author_mismatch <- {
     last_names <- lapply(table$authors.match, \(a) {
       tryCatch({
         if (is.data.frame(a)) {
           a$family
         } else {
           names <- strsplit(a, "; ")[[1]]
           sapply(names, \(x) strsplit(x, ", ")[[1]][[1]])
         }
       }, error = \(e) return(NA))
     })

     mapply(\(l, o) {
       found <- sapply(l, \(x) grepl(clean(x), clean(o), fixed = TRUE))
       !all(found)
     }, last_names, table$authors.orig)
  }

  # add unmatched papers
  unmatched <- dplyr::anti_join(
    ref_table, table,
    by = c("paper_id", "bib_id")
  )
  table$no_match <- FALSE
  if (nrow(unmatched)) {
    unmatched$no_match <- TRUE
    table <- dplyr::bind_rows(table, unmatched)
  }


  # traffic_light ----
  tl <- "green"
  if (any(table$no_match %in% T) ||
      any(table$doi_mismatch %in% T) ||
      any(table$year_mismatch %in% T) ||
      any(table$title_mismatch %in% T) ||
      any(table$author_mismatch %in% T)) {
    tl <- "yellow"
  }

  # summary_table ----
  summary_table <- dplyr::summarise(table,
    .by = paper_id,
    refs_checked = length(bib_id),
    no_match = sum(no_match, na.rm = TRUE),
    doi_mismatch    = sum(doi_mismatch,    na.rm = TRUE),
    year_mismatch   = sum(year_mismatch,   na.rm = TRUE),
    title_mismatch  = sum(title_mismatch,  na.rm = TRUE),
    author_mismatch = sum(author_mismatch, na.rm = TRUE)
  )

  # summary_text
  summary_text <- sprintf(
    "We checked %d reference%s in CrossRef and found entries for %d.",
    nrow(bib),
    nrow(bib) |> plural(),
    nrow(bib_match)
  )

  guidance <- c("Double check any references listed in the tables below. This module has a high false positive rate.",
  "Title mismatches often happen because of errors reading text from PDFs. Author mismatches often happen because of errors in parsing author lists. Year mismatches often happen because of differences between date of first publication and date of print publication.")

  if (tl == "green") guidance <- ""

  # report ----

  ## no matches ----
  nomatch_table <- table[table$no_match %in% TRUE,
                         c("text"), drop = FALSE]
  names(nomatch_table) <- c("References without Matches")

  ## doi mismatches ----
  doi_table <- table[table$doi_mismatch %in% TRUE,
                     c("text", "doi.orig", "doi.match")]
  if (nrow(doi_table)) {
    not_na <- !is.na(doi_table$doi.orig)
    doi_table$doi.orig[not_na] <- paste0(
      "https://doi.org/", doi_table$doi.orig[not_na] ) |>
      link(doi_table$doi.orig[not_na] )
    doi_table$doi.match <- paste0("https://doi.org/", doi_table$doi.match) |>
      link(doi_table$doi.match)
    names(doi_table) <- c("References with Mismatched DOIs", "Original DOI", "CrossRef DOI")
  }

  ## year mismatches ----
  year_mm <- table$year_mismatch %in% TRUE
  year_table <- table[year_mm, c("text", "year.orig", "year.match")]
  if (nrow(year_table)) {
    year_table$year.match <- paste0("https://doi.org/", table$doi.match[year_mm]) |>
      link(year_table$year.match)
    names(year_table) <- c("References with Mismatched Years", "Original Year", "CrossRef Year")
  }

  ## title mismatches ----
  title_mm <- table$title_mismatch %in% TRUE
  title_table <- table[title_mm, c("text", "title.orig", "title.match")]
  if (nrow(title_table)) {
    title_table$title.match <- paste0("https://doi.org/", table$doi.match[title_mm]) |>
      link(title_table$title.match)
    names(title_table) <- c("References with Mismatched Titles", "Original Title", "CrossRef Title")
  }

  ## author mismatches ----
  author_table <- table[table$author_mismatch %in% TRUE,
                        c("text", "authors.orig", "authors.match")]
  if (nrow(author_table)) {
    author_table$authors.match <- sapply(author_table$authors.match, \(a) {
      if (nrow(a) > 6) {
        a$family[[4]] <- sprintf("(%d authors omitted)", nrow(a) - 4)
        a$given[[4]] <- "..."
        a <- a[c(1:4, nrow(a)), ]
      }
      paste(a$family, a$given, sep = ", ", collapse = "; <br>")
    })
    names(author_table) <- c("References with Mismatched Authors", "Original Authors", "CrossRef Authors")
  }

  report <- c(
    guidance,
    scroll_table(doi_table, 5, colwidths = c(.5, .25, .25)),
    scroll_table(year_table, 5, colwidths = c(.5, .25, .25)),
    scroll_table(title_table, 5, colwidths = c(.5, .25, .25)),
    scroll_table(author_table, 5, colwidths = c(.5, .25, .25)),
    scroll_table(nomatch_table, 5)
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
