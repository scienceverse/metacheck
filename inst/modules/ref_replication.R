#' Replication Check
#'
#' @description
#' This module checks references and warns for citations of original studies for which replication or reproduction studies exist in the FLoRA database.
#'
#' @details
#' The Replication Check module compares the reference list against original studies in the FLoRA (FORRT Library of Reproduction and Replication Attempts) database based on the DOI. If a study in the database is found, a reminder is provided that a replication or reproduction of the original study exists, and should be cited.
#'
#' No reminder is given when the paper already cites that replication or reproduction, or is that replication. A reference matches when it has the replication's DOI or another DOI FLoRA gives for it (a preprint, the individual report of a study in a project paper, or that project paper), or when its text contains both the replication's title (of at least 20 characters, compared as words in lower case) and the surname of its first author. The title and surname also find a replication cited without a DOI or as another version, and keep apart replications with the same title by different authors. The paper itself counts as the replication when it has the replication's DOI or its title.
#'
#' Original studies are found only by DOI: the DOI in the paper's reference list, or one looked up with `add_bib_match()` before the module runs (see `ref_table()`). A cited original without a DOI is not checked.
#'
#' It is possible the original study was cited for other reasons than the empirical claim tested, or that the replication/reproduction in the FLoRA database is for only one of the studies in the paper, and not the study the authors discuss.
#'
#' The database can be manually updated with the `FLoRA_update()` function. For more information, see <https://forrt.org/replication-hub/flora/>.
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#' @author Lukas Wallrich (\email{lukas.wallrich@gmail.com})
#'
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param show_outcomes logical. If TRUE, include the replication or
#'   reproduction outcome in the report table. Default is FALSE.
#'
#' @returns a list
ref_replication <- function(paper, show_outcomes = FALSE) {
  # create table ----
  refs <- ref_table(paper)
  bib <- dplyr::filter(refs, !is.na(doi), doi != "")

  # If there are no rows, return immediately
  if (nrow(bib) == 0) {
    norefs <- list(
      traffic_light = "na",
      summary_text = "We found no references with DOIs, so none could be checked against FLoRA."
    )
    return(norefs)
  }

  ## join to flora table
  flora <- FLoRA() |>
    dplyr::select(
      doi = doi_o,
      replication_ref = apa_ref_r,
      replication_doi = doi_r,
      replication_url = url_r,
      replication_title = title_r,
      replication_alt = alt_identifier_r,
      replication_outcome = outcome,
      replication_type = type
    )

  # DOIs compared without a resolver prefix, with percent-encoding undone
  # ("%3c" for "<"), and in lower case, as DOIs are; the reference keeps its
  # own DOI (U157)
  no_prefix <- \(x) sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", trimws(x),
                        ignore.case = TRUE)
  doi_key <- function(x) {
    x <- no_prefix(x)
    x[!is.na(x)] <- curl::curl_unescape(x[!is.na(x)])
    tolower(x)
  }
  # words in lower case, without punctuation, with a space at each end so
  # that a title or a name is found only as whole words
  words <- function(x) {
    x[is.na(x)] <- ""
    paste0(" ", trimws(tolower(gsub("[^\\p{L}\\p{N}]+", " ", x, perl = TRUE))), " ")
  }
  # the surname of a reference's first author: the last word before the first
  # comma or "(" ("Nauts, S., ...", "Max Korbmacher, Ching Kwan, ...",
  # "Open Science Collaboration (2015)")
  first_author <- \(ref) sub("^.* ", "", trimws(words(sub("^\\s*([^,(]+).*$", "\\1", ref))))

  bib$.doi_key <- doi_key(bib$doi)
  flora$.doi_key <- doi_key(flora$doi)
  flora$doi <- NULL
  # a paper can cite an original twice, and FLoRA lists several replications
  table <- dplyr::inner_join(bib, flora, by = ".doi_key",
                             relationship = "many-to-many")
  table$.doi_key <- NULL
  bib$.doi_key <- NULL
  n_matched <- nrow(table)

  ## each replication's DOIs: its DOI, the DOIs among its alternative
  ## identifiers (a preprint, a project paper's individual report, or the
  ## project paper itself), and its URL when that is a DOI link. FLoRA
  ## separates identifiers by commas
  split_ids <- \(x) if (is.na(x)) character(0) else strsplit(x, ",\\s*")[[1]]
  rep_ids <- lapply(seq_len(nrow(table)), \(i) {
    x <- doi_key(c(table$replication_doi[i], split_ids(table$replication_alt[i]),
                   table$replication_url[i]))
    unique(x[!is.na(x) & grepl("^10\\.", x)])
  })

  ## remove rows that are already cited by the same paper: checked against
  ## every paper's references, paper B citing a replication hid paper A's
  ## citation of the original (U116). A reference matches by any of the
  ## replication's DOIs, or when its text holds both the replication's title
  ## and its first author's surname: this finds the replication cited
  ## without a DOI (#716), as another version with its own DOI (a preprint),
  ## or with a title the PDF reader ran into the journal name. The surname
  ## keeps apart replications with the same title, such as two
  ## reproductions of one original, or a replication titled like its original
  cited <- split(doi_key(bib$doi), bib$paper_id)
  ref_words <- split(words(refs$text), refs$paper_id)
  rep_title <- words(table$replication_title)
  rep_author <- paste0(" ", first_author(table$replication_ref), " ")
  by_title <- nchar(rep_title) >= 22 & nchar(rep_author) >= 4  # title of 20+ characters
  already_cited <- vapply(seq_len(nrow(table)), \(i) {
    pid <- as.character(table$paper_id[i])
    txt <- ref_words[[pid]]
    any(rep_ids[[i]] %in% cited[[pid]]) ||
      (by_title[i] && any(grepl(rep_title[i], txt, fixed = TRUE) &
                            grepl(rep_author[i], txt, fixed = TRUE)))
  }, logical(1))
  table <- table[!already_cited, ]
  rep_ids <- rep_ids[!already_cited]
  rep_title <- rep_title[!already_cited]
  by_title <- by_title[!already_cited]

  ## remove rows whose replication is the citing paper itself: a replication
  ## report cites the original it replicates, and was told to cite itself
  ## (#695: 6 rows in 5 of 149 Meta-Psychology papers). FLoRA can list only
  ## the preprint or thesis of a published paper, so the paper's own title
  ## counts as well
  own <- paper_table(paper, "info", c("doi", "title"))
  if (!"title" %in% names(own)) own$title <- rep(NA_character_, nrow(own))
  own_doi <- doi_key(own$doi[match(table$paper_id, own$paper_id)])
  own_title <- words(own$title[match(table$paper_id, own$paper_id)])
  is_self <- vapply(seq_len(nrow(table)), \(i)
    (!is.na(own_doi[i]) && nzchar(own_doi[i]) && own_doi[i] %in% rep_ids[[i]]) ||
      (by_title[i] && own_title[i] == rep_title[i]),
    logical(1))
  table <- table[!is_self, ]

  # Remove trailing URLs from reference text to avoid duplication with link,
  # except in a project paper's reference that names the individual report
  # ("... Individual report available at https://osf.io/yt5q8/"): that URL is
  # the report itself, while the link added below is the project paper's DOI
  report_at <- "available at:?\\s+https?://"
  names_report <- grepl(report_at, table$replication_ref, ignore.case = TRUE)
  table$replication_ref[!names_report] <- trimws(gsub(
    "https?://[^[:space:]]+$", "",
    table$replication_ref[!names_report], ignore.case = TRUE
  ))

  # traffic_light ----
  tl <- if (nrow(table)) "info" else "na"

  # each pair of an original and a replication counts once per paper, also
  # when the paper cites the original twice (e.g. its DOI in two spellings)
  pair <- paste(table$paper_id, doi_key(table$doi), table$replication_doi,
                table$replication_url, table$replication_ref, table$replication_type)
  first <- !duplicated(pair)

  # summary_table ----
  summary_table <- dplyr::summarise(
    table[first, ],
    .by = "paper_id",
    replications = dplyr::n(),
  )

  # summary_text & report ----
  if (nrow(table) == 0) {
    summary_text <- if (n_matched > 0) {
      "We found no replications or reproductions in the FLoRA database to add for the originals you cited."
    } else {
      "No citations to articles in the FLoRA database were found."
    }
    report <- sprintf(
      "We checked %d reference%s with DOIs. %s",   # "1 references" (U82)
      sum(!is.na(bib$doi)), plural(sum(!is.na(bib$doi))), summary_text
    )
  } else {
    ## summary_text ----
    n_replications <- sum(table$replication_type[first] == "replication", na.rm = TRUE)
    n_reproductions <- sum(table$replication_type[first] == "reproduction", na.rm = TRUE)
    n_originals <- dplyr::n_distinct(doi_key(table$doi))

    if (n_reproductions == 0) {
      summary_text <- sprintf(
        "We found %d replication%s for %d original%s you cited.",
        n_replications, plural(n_replications), n_originals, plural(n_originals)
      )
    } else if (n_replications == 0) {
      summary_text <- sprintf(
        "We found %d reproduction%s for %d original%s you cited.",
        n_reproductions, plural(n_reproductions), n_originals, plural(n_originals)
      )
    } else {
      summary_text <- sprintf(
        "We found %d replication%s and %d reproduction%s for %d original%s you cited.",
        n_replications, plural(n_replications),
        n_reproductions, plural(n_reproductions),
        n_originals, plural(n_originals)
      )
    }

    ## report_text ----
    n_doi <- sum(!is.na(bib$doi))
    has_both_types <- n_replications > 0 && n_reproductions > 0

    if (n_reproductions == 0) {
      study_type_text <- "replication studies"
      col_header <- "Replication"
    } else if (n_replications == 0) {
      study_type_text <- "reproduction studies"
      col_header <- "Reproduction"
    } else {
      study_type_text <- "replication/reproduction studies"
      col_header <- "Replication/Reproduction"
    }

    report_text <- sprintf(
      "We checked %d reference%s with DOIs. %s\n\nCheck if you are aware of the %s, and cite them where appropriate.",
      n_doi, plural(n_doi), summary_text, study_type_text
    )

    ## report_table ----
    report_table <- table[, c("text", "replication_ref")]
    # the individual report's URL (after "available at") becomes a link
    ref_html <- table$replication_ref
    at <- regexpr(report_at, ref_html, ignore.case = TRUE)
    report_part <- substring(ref_html, at)
    ref_html[names_report] <- paste0(
      substring(ref_html, 1, at - 1),
      gsub("(https?://)([^[:space:]]+)",
           "<a href='\\1\\2' target='_blank'>\\2</a>", report_part)
    )[names_report]

    # Create links using DOI if available, otherwise use URL
    has_doi <- !is.na(table$replication_doi) & table$replication_doi != ""
    # Format DOI links with doi: prefix to match format_ref() output
    rep_doi <- no_prefix(table$replication_doi)
    doi_links <- link(
      url = paste0("https://doi.org/", rep_doi),
      text = paste0("doi:", rep_doi)
    )
    replication_links <- ifelse(has_doi, doi_links, link(table$replication_url))

    # Only label entries with type if both replications and reproductions are present
    if (has_both_types) {
      type_label <- tools::toTitleCase(table$replication_type)
      report_table$replication_ref <- sprintf(
        "<b>[%s]</b> %s %s",
        type_label,
        ref_html,
        replication_links
      )
    } else {
      report_table$replication_ref <- sprintf(
        "%s %s",
        ref_html,
        replication_links
      )
    }
    names(report_table) <- c("Reference", col_header)

    # the type is in the column header, or in each row's label
    if (show_outcomes) {
      report_table$Outcome <- table$replication_outcome
    }

    ## report ----
    colwidths <- if (show_outcomes) c(.35, .45, .2) else c(.5, .5)
    report <- c(report_text, scroll_table(report_table, colwidths = colwidths))
  }

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
