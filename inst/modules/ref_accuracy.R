#' Reference Accuracy
#'
#' @description
#' This module compares the details each reference cites with the record of the cited work and flags references whose details differ. For a reference without a DOI, it searches for the work by its title and suggests its DOI when the record agrees with the citation.
#'
#' @details
#' A flagged reference is a *possible citation error* (`incoherent` in the tables): its cited details differ from the record. The difference may come from a citation mistake, an error in reading the reference list from the PDF, an error in the record, or an AI-generated reference, so check the original source. Only possible citation errors make the traffic light yellow.
#'
#' **Sources and time.** The module uses the Crossref records in the `bib_match` table (see `add_bib_match()`). It also asks doi.org for the record of a cited DOI that Crossref did not return (from DataCite, mEDRA or another agency), Crossref for the dates of records whose year differs, and Crossref, DataCite and Semantic Scholar for the titles of references without a DOI. Semantic Scholar holds many books and conference papers that have no DOI but has strict rate limits. Therefore, it is only queried when the other two did not find the cited work, at about one title a second. A free key in the environment variable `SEMANTIC_SCHOLAR_API_KEY` makes refused requests rarer. With `semantic_scholar = FALSE` it is not queried, and the references it would have been asked about are counted as not checked, not as not found.
#'
#' **References with a DOI** are compared field by field with the record of their DOI, after `doi_clean()` has removed the URLs, spaces and punctuation that reading the PDF adds. A cited DOI without a Crossref record is looked up at doi.org, which returns the record from the agency that registered it, and the reference is compared with that record. If the DOI is not registered, the reference is flagged and searched by its title: the work found is shown, and a reference found in no source is listed as not found instead. Any other cited DOI without a record is listed as not checked.
#'
#' **References without a DOI** are compared with Crossref's match when its title is almost the cited title (a character similarity of at least 0.9), or is similar and has a Crossref score of at least `suggest_score`; the others are searched by their title in Crossref, DataCite and Semantic Scholar. A record whose title, authors, journal or year differ flags the reference. A record that agrees has its DOI suggested, if it is the only work with a DOI, that title and that year among the records found. A reference found in none of the sources is listed as not found, not flagged: most are books, reports and web pages that these sources do not hold, but a journal article that cannot be found may have a wrong title or may not exist. Each reference without a suggested DOI is listed with the reason. Titles of fewer than three words, and works the reference calls unpublished, are not searched.
#'
#' **Books** and other works not cited as articles (as the PDF reader classifies the reference) are never flagged when they have no DOI, because the record found by their title is often not the cited work. They can still get a suggested DOI, when the record lists the cited authors or lists no authors (as publishers' records of books often do). A journal article is not suggested for a cited book, as a journal article with a book's title is usually a review of it; the exception is a reference that names the journal outside the title, which is probably an article that the PDF reader wrongly classified as a book.
#'
#' **Fields.** Differences that are usually harmless are tolerated:
#'
#' - **DOI**: URL wrappers, spaces, punctuation, the old double slash (`10.1037//`), and DOIs truncated or split by a PDF line-break hyphen when the intact DOI is recoverable. A cited DOI that the registry lists as an alias of the record's DOI is a note.
#' - **Year**: up to `year_tolerance` years, or the year of the record's online, print or issue date. A year off by one more year is a note when it is near the year the DOI was created or the work accepted.
#' - **Authors**: the record's first `max_authors` surnames must all be cited, its first author first. Surnames are compared on their letters (accents, spacing and case are ignored), allowing shortened compound surnames, particles, ASCII umlaut/Nordic transliterations (`ue`, `oe`, `ae`, `aa`), and corrupted replacement characters in the registry record. A list truncated with "et al." needs only the authors it cites, and given and family names swapped on one side are a note (as they are often metadata errors).
#' - **Journal**: ISO 4 abbreviations and common contractions (`Jt.`, `Natl.`), acronyms, "&" for "and", British and American spellings, European stopwords, bilingual `=` titles, publisher journal codes, a missing subtitle, a 1-character tail or transposition typo in a long word of a multi-word name, and a conference's proceedings cited without "Proceedings of" or by its acronym ("AAAI"). No word may be skipped. A journal cited by another of its names is accepted when Wikidata gives that name for the journal or for its predecessor or successor (including aliases with a city or subtitle); other names may be flagged. Only journal articles are compared.
#' - **Title**: case, punctuation, Unicode forms, a missing subtitle, a translation in square brackets, corrupted replacement characters in the registry record, and British, Canadian and Australian spellings (folded to the American from VarCon's list of spelling variants). Any other difference, even of one word, is flagged.
#' - **Preprints**: a cited preprint is not compared on its year, journal or title with its published version, which is a note. A reference cited in a journal whose work is found only as a preprint is flagged, unless its venue is a conference.
#'
#' Each limit errs towards not flagging, to keep false alarms few; they are judgement calls set on the papers described under validation. Raising `title_similarity` or `suggest_score` compares fewer records, so it gives fewer flags but also fewer suggested DOIs. Entries that were not read reliably from the PDF (two references read as one, a DOI read without its reference) are listed and not compared. The report shows each difference as a diff of the printed reference and the record.
#'
#' <validation>In the earlier 30-paper Psychological Science development benchmark (1,269 references), the module flagged 29 references (24 citation errors by hand), suggested 561 DOIs (63/64 checked correct), and caught 140/141 synthetic errors; on CiteTracer, it flagged or listed as not found 68% of fabricated citations (including 197/197 fabricated venues on the current code) and flagged 0.7% of real ones. In a held-out multi-discipline sample of 160 papers across 8 journals (480 sampled references, `semantic_scholar = FALSE`), it flagged 5.8% of references (28/480): 36% were genuine citation errors (10/28, 95% CI 21-54%), with 25% PDF-reading artifacts, 18% registry or preprint-revision differences, and 21% overly strict rules or title-search mismatches, while 30/30 sampled suggested DOIs were correct. In 24 separate full-bibliography papers (1,622 references), median flags per paper was 2.0 (range 0-15; 25/50 audited flags were citation errors, 50%, 95% CI 37-63%). Precision is highest on cleanly typeset APA papers with full English journal names and lower (~35-50%) on heterogeneous biomedical or preprint-heavy PDFs; check every flag against the original source.</validation>
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#' @author Lukas Wallrich (\email{l.wallrich@bbk.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param max_authors how many of the leading authors to compare against the
#'   retrieved record. Author lists are routinely truncated with "et al.",
#'   and GROBID often drops the tail of long lists, so only
#'   the first `max_authors` surnames must match.
#' @param title_similarity the minimum character-level similarity (0-1, after
#'   stripping case, punctuation, footnote markers and diacritics) for a record
#'   to be taken as the cited work. Its title must then be the cited title, by
#'   the rules under Details.
#' @param min_mismatches deprecated, and will be removed. How many of the
#'   fields most affected by reading the PDF (title and authors) must disagree
#'   with the record before a reference is flagged. The default (1) flags a
#'   single title or author mismatch. Set to 2 to require both; in the
#'   validation this lost all seven author errors and all ten title errors. A mismatched
#'   DOI, journal or year flags on its own, whatever this setting.
#' @param year_tolerance how many years the cited year may differ from the
#'   retrieved record before the year is flagged. The default (1) allows a
#'   one-year difference, because the online-first and print publication years
#'   of an article routinely differ by a year. Set to 0 to require the same
#'   year (apart from the date and version rules under Details), or higher
#'   to be more lenient.
#' @param suggest_score for references with no DOI, the minimum Crossref
#'   relevance score for Crossref's match to be compared with the
#'   reference, and its DOI suggested when nothing differs. A match with a
#'   lower score is compared only when it has almost the cited title, as
#'   low-scoring matches are often the wrong paper. Raise to be more conservative, lower to
#'   suggest more.
#'   Scores of 1 or less, as bibr 12.x exports give, are on a 0-1 scale and
#'   are compared with `suggest_score / 100`.
#' @param semantic_scholar whether to search Semantic Scholar for works that
#'   Crossref and DataCite did not find (default TRUE). Its requests are paced
#'   at about one a second, so FALSE is faster; the references it would have
#'   been asked about are then counted as not checked (`search_failed`), not
#'   as not found.
#'
#' @returns a module output list. Its `table` has one row per reference, with
#'   the cited (`.orig`) and record (`.match`) values and these columns:
#'   - `tier`: how the reference was checked. "provided": compared with the
#'     record of its own DOI (from Crossref, from doi.org, or after stripping a
#'     PDF line-break hyphen recovered by title search). "unresolved": its DOI has no record, and doi.org
#'     reports that it does not exist (flagged with the work its title search
#'     found, or listed as not found). "registered": its DOI exists
#'     but doi.org returned no record; "unconfirmed": doi.org could not be asked; "not_checked":
#'     its paper was not looked up (these three are not checked). "crossref": no
#'     DOI, and `add_bib_match()` found a record. "search": no DOI, and a title
#'     search found a record that disagrees (flagged). "none": no DOI and no
#'     record. "unreadable": not read reliably from the PDF (see
#'     `read_problem`). "fragment": a DOI read as an entry of its own.
#'   - `incoherent`: the reference is flagged.
#'   - `doi_mismatch`, `title_mismatch`, `author_mismatch`, `year_mismatch`,
#'     `container_mismatch`: which fields disagree, for flagged references only.
#'     FALSE does not mean the field was compared.
#'   - `author_note`: how the cited authors differ from the record's (an
#'     author missing from the reference, or another first author).
#'   - `year_note`: the cited year is that of another date in the record (a
#'     note).
#'   - `name_note`: an author's given and family names are swapped in the
#'     citation or the record (a note).
#'   - `preprint_only`: the reference is cited in a journal, but only a
#'     preprint with its title was found (flagged).
#'   - `published_doi`: the cited preprint has a published version with this
#'     DOI (a note).
#'   - `doi_alias`: the cited DOI is an alias of the record's DOI (a note).
#'   - `no_doi`, `doi_suggested`: no DOI was cited; a DOI is suggested.
#'   - `not_found`: the title search found the work in none of Crossref,
#'     DataCite and Semantic Scholar.
#'   - `search_failed`: the title search failed, or Semantic Scholar was not
#'     searched, so it is not known whether the work exists.
#'   - `no_match`: no record was retrieved, for whatever reason (see `tier`).
#'   - `doi_exists`, `doi_ra`: doi.org's answer for a DOI without a Crossref record.
#'   - `record_source`: where the compared record comes from (for a DOI, the agency that registered it).
#'
#'   `summary_table` counts per paper the references with a DOI checked (`refs_checked`,
#'   tiers "provided" and "unresolved"), flagged, without a DOI, with a
#'   suggested DOI, not found and unreadable; a reference without a DOI may be
#'   counted as flagged, as suggested or as not found, but in only one of them.
ref_accuracy <- function(paper, max_authors = 6, title_similarity = 0.7,
                         min_mismatches = 1, year_tolerance = 1,
                         suggest_score = 70, semantic_scholar = TRUE) {
  if (!missing(min_mismatches))
    warning("`min_mismatches` is deprecated and will be removed: set to 2, it misses ",
            "citation errors in a title or in the authors.", call. = FALSE)
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

  cols <- c("paper_id", "bib_id", "doi", "title", "year", "authors",
            "container", "bib_type")
  if (!"bib_type" %in% names(bib)) bib$bib_type <- NA_character_
  # Papers that were explicitly looked up by add_bib_match() (even when all
  # lookups missed and bib_match has 0 rows), in addition to papers that have
  # at least one bib_match row. Legacy papers with an empty bib_match frame
  # without the "looked_up" attribute stay unlooked-up.
  paper_list <- if (.is_paper_list(paper)) paper else list(paper)
  explicit_looked_up <- vapply(paper_list, \(p) isTRUE(attr(p$bib_match, "looked_up")), logical(1))
  explicit_ids <- vapply(paper_list[explicit_looked_up],
                         \(p) as.character(p$paper_id %||% NA_character_), character(1))
  explicit_ids <- explicit_ids[!is.na(explicit_ids)]
  # per-paper DOI-lookup provenance recorded by add_bib_match()
  has_doi_prov_p <- vapply(paper_list, \(p) !is.null(attr(p$bib_match, "doi_bib_ids")), logical(1))
  prov_paper_ids <- vapply(paper_list[has_doi_prov_p],
                           \(p) as.character(p$paper_id %||% NA_character_), character(1))
  prov_paper_ids <- prov_paper_ids[!is.na(prov_paper_ids)]
  doi_bib_keys <- unlist(lapply(paper_list[has_doi_prov_p], \(p) {
    ids <- attr(p$bib_match, "doi_bib_ids")
    if (!length(ids) || is.null(p$paper_id)) return(character(0))
    paste(p$paper_id, ids, sep = "\001")
  }), use.names = FALSE)
  # no Crossref lookup (convert() without crossref, no add_bib_match()): the
  # references are not checked, which is not a fault of the paper, but those
  # without a DOI are still counted
  if (nrow(bib_match) == 0) bib_match <- bib[0, cols]
  if (!"bib_type" %in% names(bib_match)) bib_match$bib_type <- NA_character_
  looked_up_ids <- unique(c(as.character(bib_match$paper_id), explicit_ids))
  ref_table <- ref_table(paper)
  ref_table$doi <- NULL
  # one row per reference: with several bib_match rows for a bib_id the three
  # joins below gave k^3 rows, and ref_table() repeats the reference too
  # (U114). When a reference printed its own DOI, prefer an unscored DOI
  # lookup over a scored title search; otherwise keep the highest score.
  # matches read from elsewhere may have no score column
  if (!"score" %in% names(bib_match)) bib_match$score <- NA_real_
  if (!"service" %in% names(bib_match)) bib_match$service <- NA_character_
  if (nrow(bib_match) > 0) {
    bib_doi_map <- stats::setNames(bib$doi, paste(bib$paper_id, bib$bib_id, sep = "\001"))
    bm_key <- paste(bib_match$paper_id, bib_match$bib_id, sep = "\001")
    bm_has_cited_doi <- !is.na(bib_doi_map[bm_key]) & nzchar(trimws(bib_doi_map[bm_key]))
    prefer_doi <- is.na(bib_match$score) & bm_has_cited_doi
    bib_match <- bib_match[order(!prefer_doi, -bib_match$score), , drop = FALSE]
    bib_match <- bib_match[!duplicated(bib_match[, c("paper_id", "bib_id")]), , drop = FALSE]
  }
  ref_table <- ref_table[!duplicated(ref_table[, c("paper_id", "bib_id")]), , drop = FALSE]
  # left join so every reference is kept, including those with no Crossref
  # record. A reference that printed a DOI which does not resolve produces no
  # bib_match row; an inner join would silently drop it (and lose its cited
  # DOI), mislabelling it as a reference without a DOI.
  # The Crossref relevance score comes along: only a title search gives one.
  table <- dplyr::left_join(
    bib[, cols], bib_match[, c(cols, "score", "service")],
    by = c("paper_id", "bib_id"),
    suffix = c(".orig", ".match")
  ) |>
    dplyr::left_join(ref_table, by = c("paper_id", "bib_id"))
  blank <- \(x) is.na(x) | !nzchar(trimws(x))
  alnum <- \(x) gsub("[^a-z0-9]", "", tolower(x))
  norm_doi <- \(x) tolower(sub("^(10\\.\\d+)//", "\\1/", doi_clean(x)))

  # Extract any DOI printed in the raw reference text (and count how many appear)
  # BEFORE doi.org lookup so we can recover a GROBID-truncated or hyphen-inserted
  # doi.orig when the printed reference text itself holds the single intact DOI.
  doi_in_text_pat <- "(?i)(doi\\.org/|\\bdoi:\\s*)\\S+|10\\.\\d{4,9}(/|%2F)\\S+"
  text_doi_matches <- lapply(table$text, \(x) {
    if (is.na(x)) return(character(0))
    m <- regmatches(x, gregexpr(doi_in_text_pat, x, perl = TRUE))[[1]]
    if (!length(m)) return(character(0))
    unique(doi_clean(sub("(?i)^.*?(doi\\.org/|doi:\\s*)", "", m, perl = TRUE)))
  })
  text_doi_n <- lengths(text_doi_matches)
  text_doi <- vapply(text_doi_matches, \(m) if (length(m)) m[[1]] else NA_character_,
                     character(1), USE.NAMES = FALSE)
  # Recover GROBID-mangled doi.orig when a single valid DOI is printed in text
  # and doi.orig is either a strict prefix (truncation, e.g. "10.1016/j.neuroimage."
  # or a journal-level stub "10.5007/1980-0037" when text prints the full article
  # DOI "10.5007/1980-0037.2014v16n2p233") or differs only by an extra hyphen
  # inserted by GROBID that is absent from the printed text. Never clear an
  # existing article record whose title already matches the cited title (guards
  # against glued trailing text like ".../a0012345.Epub" in the raw PDF text).
  can_recover_doi <- !blank(table$doi.orig) &
    text_doi_n == 1L & !is.na(text_doi) & (doi_valid_format(text_doi) %in% TRUE)
  if (any(can_recover_doi)) {
    for (ri in which(can_recover_doi)) {
      d_orig <- norm_doi(table$doi.orig[ri])
      d_text <- norm_doi(text_doi[ri])
      if (d_orig == d_text) next
      is_trunc <- nchar(d_orig) >= 8L && startsWith(d_text, d_orig) && nchar(d_text) > nchar(d_orig)
      is_extra_hyphen <- blank(table$title.match[ri]) && nchar(d_orig) > nchar(d_text) &&
        gsub("-", "", d_orig, fixed = TRUE) == gsub("-", "", d_text, fixed = TRUE)
      if (is_trunc && !blank(table$title.match[ri])) {
        # Only replace an existing record if it is plainly not the cited article
        rec_is_article <- table$bib_type.match[ri] %in% c("article", "journal_article", "inproceedings")
        if (rec_is_article) {
          t1 <- alnum(table$title.orig[ri])
          t2 <- alnum(table$title.match[ri])
          if (!nzchar(t1) || !nzchar(t2) ||
              (1 - utils::adist(t1, t2)[1, 1] / max(nchar(t1), nchar(t2))) >= 0.7) {
            next
          }
        }
      }
      if (is_trunc || is_extra_hyphen) {
        table$doi.orig[ri] <- text_doi[ri]
        if (is_trunc && !blank(table$title.match[ri])) {
          table$doi.match[ri] <- NA_character_
          table$title.match[ri] <- NA_character_
          table$year.match[ri] <- NA_integer_
          table$authors.match[ri] <- list(NA)
          table$container.match[ri] <- NA_character_
          table$bib_type.match[ri] <- NA_character_
          table$score[ri] <- NA_real_
          table$service[ri] <- NA_character_
        }
      }
    }
  }

  # A cited DOI without a Crossref record is checked at doi.org: whether it
  # exists, with which agency, and, if it exists, its record (DataCite, mEDRA
  # and other agencies are not looked up by add_bib_match(), and a Crossref
  # lookup may have failed). The record is then compared like any other.
  table$doi_exists <- NA
  table$doi_ra <- NA_character_
  unres <- which(!blank(table$doi.orig) & blank(table$title.match) &
                   as.character(table$paper_id) %in% looked_up_ids)
  if (length(unres)) {
    ra <- doi_ra(table$doi.orig[unres])
    table$doi_exists[unres] <- ra$exists
    table$doi_ra[unres] <- ra$ra
    reg <- unres[ra$exists %in% TRUE]
    if (length(reg)) {
      csl <- doi_csl(table$doi.orig[reg])
      got <- !blank(csl$title)
      reg <- reg[got]
      csl <- csl[got, , drop = FALSE]
      table$doi.match[reg] <- csl$doi
      table$title.match[reg] <- csl$title
      table$year.match[reg] <- csl$year
      table$authors.match[reg] <- csl$authors
      table$container.match[reg] <- csl$container
      table$bib_type.match[reg] <- csl$bib_type
      table$service[reg] <- ifelse(table$doi_ra[reg] %in% c("Crossref", "DataCite"),
                                   tolower(table$doi_ra[reg]), table$doi_ra[reg])
    }
  }

  # Reading problems: entries that cannot be compared field by field.
  table$read_problem <- NA_character_
  # A DOI printed in the reference that the PDF reader did not take as its DOI:
  # a URL with "%2F" for "/", or a malformed DOI ("doi.org/1073191116659134"
  # for 10.1177/1073191116659134). It counts as the cited DOI when it is the
  # DOI the title search found, apart from lost punctuation or prefix; the
  # record is then the DOI's record, and a malformed DOI is a DOI mismatch.
  # Another DOI was not looked up, so the entry is a reading problem.
  in_text_doi <- blank(table$doi.orig) & !is.na(text_doi) &
    nchar(alnum(text_doi)) >= 8
  same_doi <- !is.na(table$doi.match) & (
    alnum(text_doi) == alnum(table$doi.match) |
      alnum(text_doi) == alnum(sub("^[^/]*/", "", table$doi.match)))
  table$doi.orig[in_text_doi & same_doi] <- text_doi[in_text_doi & same_doi]
  table$read_problem[in_text_doi & !same_doi] <- sprintf(
    "A DOI is printed in this reference (%s), but it was not read from the PDF, so it was not looked up.",
    text_doi[in_text_doi & !same_doi])

  # Only a DOI was read (possibly with a stray number read as year from the
  # DOI/URL): the rest of the reference stayed with the entry before it. When
  # that entry has no DOI and its title search found this exact DOI, the DOI
  # is given to it and the fragment is dropped.
  doi_only <- !blank(table$doi.orig) & blank(table$title.orig) &
    blank(table$authors.orig) & blank(table$container.orig)
  prev <- c(NA, seq_len(nrow(table) - 1))
  prev[c(TRUE, table$paper_id[-1] != table$paper_id[-nrow(table)])] <- NA
  joins <- doi_only & !is.na(prev)
  joins[joins] <- blank(table$doi.orig[prev[joins]]) &
    !is.na(table$doi.match[prev[joins]]) &
    norm_doi(table$doi.orig[joins]) == norm_doi(table$doi.match[prev[joins]])
  table$doi.orig[prev[joins]] <- table$doi.orig[joins]
  table$read_problem[joins] <- "fragment"
  table$read_problem[doi_only & !joins] <-
    "Only the DOI was read for this entry; the rest of the reference was not separated from the entry before it."

  # Fragment entries without a title or authors whose non-URL text is too
  # short to be a checkable reference (e.g. a split-off DOI/page fragment
  # where GROBID filled a stray year or container). Require non-NA text so
  # synthetic or metadata-only entries without text are not misclassified.
  text_no_url <- ifelse(
    is.na(table$text), "",
    gsub("(?i)(https?://\\S+|\\bdoi:\\s*\\S+|10\\.\\d{3,9}/\\S+)", " ",
         table$text, perl = TRUE))
  bare_frag <- is.na(table$read_problem) & !is.na(table$text) & !blank(table$doi.orig) &
    blank(table$title.orig) & blank(table$authors.orig) &
    nchar(gsub("[^\\p{L}]", "", text_no_url, perl = TRUE)) < 15L
  table$read_problem[bare_frag] <-
    "This entry is a fragment split from another reference when reading the PDF, so it was not checked."

  # Publisher download footers (Wiley Online Library, JSTOR, etc.) merged into
  # a reference across a page break. Strip trailing boilerplate so unaffected
  # fields are still checked; mark the entry unreadable when the footer
  # contaminated parsed fields or split the citation text in the middle.
  footer_pat <- paste0(
    "(?i)downloaded\\s+from\\s+https?://[^\\s]+.*?(?:",
    "wiley\\s+online\\s+library(?:\\s+on\\s*\\[?[0-9/.-]+\\]?)?\\.?",
    "(?:\\s*see\\s+the\\s+terms\\s+and\\s+conditions(?:\\s*\\([^)]*\\))?(?:\\s+on\\s+wiley\\s+online\\s+library)?(?:\\s+for\\s+rules\\s+of\\s+use[^.]*\\.?)?)?|",
    "see\\s+the\\s+terms\\s+and\\s+conditions(?:\\s*\\([^)]*\\))?(?:\\s+on\\s+wiley\\s+online\\s+library)?(?:\\s+for\\s+rules\\s+of\\s+use[^.]*\\.?)?|",
    "all\\s+use\\s+subject\\s+to\\s+\\S+\\.?|",
    "on\\s*\\[?\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}\\]?\\.?)|",
    "(?i)this\\s+content\\s+downloaded\\s+from\\s+\\S+.*?all\\s+use\\s+subject\\s+to\\s+\\S+\\.?|",
    "(?i)see\\s+the\\s+terms\\s+and\\s+conditions\\s*\\(https?://onlinelibrary\\.wiley\\.com[^)]*\\)\\s+on\\s+wiley\\s+online\\s+library(?:\\s+for\\s+rules\\s+of\\s+use[^.]*\\.?)?"
  )
  footer_words <- "(?i)wiley\\s+online\\s+library|downloaded\\s+from|terms\\s+and\\s+conditions|onlinelibrary\\.wiley|about\\.jstor\\.org"
  has_footer <- is.na(table$read_problem) & !is.na(table$text) &
    grepl(footer_pat, table$text, perl = TRUE)
  if (any(has_footer)) {
    for (fi in which(has_footer)) {
      tx <- table$text[fi]
      m <- regexpr(footer_pat, tx, perl = TRUE)
      f_start <- as.integer(m)
      f_len <- attr(m, "match.length")
      ftxt <- substr(tx, f_start, f_start + f_len - 1L)
      before_f <- trimws(substr(tx, 1L, f_start - 1L))
      after_f <- trimws(substring(tx, f_start + f_len))
      cleaned_tx <- trimws(paste(before_f, after_f))
      fields_blob <- paste(table$authors.orig[fi], table$title.orig[fi],
                           table$container.orig[fi])
      yr_str <- if (is.na(table$year.orig[fi])) "" else as.character(table$year.orig[fi])
      yr_in_footer <- nzchar(yr_str) &&
        grepl(paste0("\\b", yr_str, "\\b"), ftxt, perl = TRUE) &&
        !grepl(paste0("\\b", yr_str, "\\b"), cleaned_tx, perl = TRUE)
      # footer in the middle of the reference text (non-trivial text on both sides)
      in_middle <- nchar(gsub("[^\\p{L}]", "", before_f, perl = TRUE)) >= 6L &&
        nchar(gsub("[^\\p{L}]", "", after_f, perl = TRUE)) >= 6L
      if (grepl(footer_words, fields_blob, perl = TRUE) || yr_in_footer || in_middle) {
        table$read_problem[fi] <-
          "A publisher download footer was merged into this reference when reading the PDF, so it was not checked."
      } else {
        # trailing footer only: strip from comparison text so real errors stay flagged
        table$text[fi] <- cleaned_tx
      }
    }
  }

  # Two references read as one entry: a second APA author-year start
  # ("... Bar, M. (2004). ..."). The details and the DOI may come from
  # different references, so the entry is not compared.
  apa_start <- "\\.\\s\\(((19|20)\\d{2}[a-z]?|n\\.d\\.|in press)\\)\\."
  merged <- lengths(gregexpr(apa_start, table$text, perl = TRUE)) >= 2 &
    grepl(apa_start, table$text, perl = TRUE)
  table$read_problem[merged & is.na(table$read_problem)] <-
    "This entry contains two references, which were read from the PDF as one, so it was not checked."

  # DOI: only flag when the reference itself printed a DOI that differs from
  # the matched record. A blank reference DOI is not a mismatch (the citer just
  # did not include one, and Crossref supplied it). Cited DOIs are cleaned
  # first, so a doi.org or journal URL around the same DOI is not a mismatch,
  # and the old double slash after the prefix ("10.1037//0022-3514...", common
  # in APA references) is the same DOI as the single slash.
  # doi_clean() also drops what reading the PDF adds ("10.1037/0003-066x.54.6.4 08",
  # "...408.", "<https://doi.org/...>").
  doi_differs <- !is.na(table$doi.orig) & nzchar(table$doi.orig) &
    norm_doi(table$doi.orig) != norm_doi(table$doi.match)
  # A record that add_bib_match() found by looking up the cited DOI has no
  # search score. If it carries another DOI, the registry treats the cited DOI
  # as an alias of it, e.g. after a journal moved publisher: the citation still
  # finds the right work, so this is a note, not an incoherence. A record from
  # a title search (it has a score) can show that the cited DOI is wrong; that
  # is the route for a cited DOI that is not valid as printed, such as a DOI
  # written as a URL before add_bib_match() cleaned DOIs. bibr 12.x exports do
  # not record how a record was found (writing one drops the score), so there
  # a different DOI stays a mismatch.
  tbl_keys <- paste(table$paper_id, table$bib_id, sep = "\001")
  prov_known <- as.character(table$paper_id) %in% prov_paper_ids
  by_doi_evid <- ifelse(prov_known,
                        tbl_keys %in% doi_bib_keys | (table$doi_exists %in% TRUE),
                        is.na(table$score))
  looked_up_by_doi <- by_doi_evid &
    doi_valid_format(doi_clean(table$doi.orig)) %in% TRUE &
    !table$paper_id %in% .bibr12_paper_ids(paper)
  table$doi_alias <- (doi_differs %in% TRUE) & looked_up_by_doi
  table$doi_mismatch <- doi_differs & !table$doi_alias

  # year: only flag when more than `year_tolerance` years apart (online-first
  # vs print publication routinely differ by a year and is not an incoherence).
  # Ignore a parsed year that was actually extracted from a DOI/URL (e.g.
  # "1007" from "10.1007/..." in older GROBID exports): a year below 1500, or
  # a 4-digit number appearing only inside a DOI/URL and nowhere else in text.
  year_num <- suppressWarnings(as.numeric(table$year.orig))
  year_from_doi <- !is.na(year_num) & (
    year_num < 1500 |
      mapply(\(y, doi, tx_clean, tx_raw) {
        if (is.na(y)) return(FALSE)
        ys <- as.character(as.integer(y))
        in_doi <- (!is.na(doi) && grepl(paste0("\\b", ys, "\\b"), doi, perl = TRUE)) ||
          (!is.na(tx_raw) && grepl(paste0("10\\.", ys, "/"), tx_raw, perl = TRUE))
        if (!in_doi) return(FALSE)
        # only treat as DOI-derived when it does not also appear outside URLs/DOIs
        !is.na(tx_clean) && nzchar(trimws(tx_clean)) &&
          !grepl(paste0("\\b", ys, "\\b"), tx_clean, perl = TRUE)
      }, year_num, table$doi.orig, text_no_url, table$text, USE.NAMES = FALSE)
  )
  year_diff <- abs(year_num - as.numeric(table$year.match))
  table$year_mismatch <- !is.na(year_diff) & !year_from_doi & year_diff > year_tolerance

  # fold accented Latin letters to plain ASCII ("Gredebäck" and "Gredeback",
  # "Nguyễn" and "Nguyen", "Þórsson" and "Thorsson"), so that diacritics never
  # cause a mismatch on their own
  deaccent <- \(x) stringi::stri_trans_general(x, "Latin-ASCII")

  # join words broken by a line-break hyphen (ASCII hyphen, Unicode dashes,
  # soft hyphen) followed by whitespace
  dehyphen <- \(x) gsub("(?<=\\p{L})[\\p{Pd}\u00ad]\\s+(?=\\p{L})", "", x, perl = TRUE)

  # decode common HTML/XML entities (named, Greek, and numeric) on a
  # comparison copy so escaped markup (&lt;em&gt;) and entities (&alpha;,
  # &ndash;) are normalised before tag stripping. Only touches strings with
  # "&", and keeps literal comparisons ("p < .05") intact.
  html_entities <- c(
    "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&apos;" = "'",
    "&nbsp;" = " ", "&ndash;" = "-", "&mdash;" = "\u2014", "&minus;" = "-",
    "&lsquo;" = "'", "&rsquo;" = "'", "&ldquo;" = "\"", "&rdquo;" = "\"",
    "&times;" = "x", "&plusmn;" = "\u00b1", "&hellip;" = "\u2026",
    "&alpha;" = "\u03b1", "&beta;" = "\u03b2", "&gamma;" = "\u03b3",
    "&delta;" = "\u03b4", "&epsilon;" = "\u03b5", "&zeta;" = "\u03b6",
    "&eta;" = "\u03b7", "&theta;" = "\u03b8", "&iota;" = "\u03b9",
    "&kappa;" = "\u03ba", "&lambda;" = "\u03bb", "&mu;" = "\u03bc",
    "&micro;" = "\u03bc", "&nu;" = "\u03bd", "&xi;" = "\u03be",
    "&omicron;" = "\u03bf", "&pi;" = "\u03c0", "&rho;" = "\u03c1",
    "&sigma;" = "\u03c3", "&tau;" = "\u03c4", "&upsilon;" = "\u03c5",
    "&phi;" = "\u03c6", "&chi;" = "\u03c7", "&psi;" = "\u03c8",
    "&omega;" = "\u03c9",
    "&Alpha;" = "\u0391", "&Beta;" = "\u0392", "&Gamma;" = "\u0393",
    "&Delta;" = "\u0394", "&Epsilon;" = "\u0395", "&Zeta;" = "\u0396",
    "&Eta;" = "\u0397", "&Theta;" = "\u0398", "&Iota;" = "\u0399",
    "&Kappa;" = "\u039a", "&Lambda;" = "\u039b", "&Mu;" = "\u039c",
    "&Nu;" = "\u039d", "&Xi;" = "\u039e", "&Omicron;" = "\u039f",
    "&Pi;" = "\u03a0", "&Rho;" = "\u03a1", "&Sigma;" = "\u03a3",
    "&Tau;" = "\u03a4", "&Upsilon;" = "\u03a5", "&Phi;" = "\u03a6",
    "&Chi;" = "\u03a7", "&Psi;" = "\u03a8", "&Omega;" = "\u03a9"
  )
  decode_entities <- \(x) {
    x <- as.character(x)
    idx <- which(!is.na(x) & grepl("&", x, fixed = TRUE))
    if (!length(idx)) return(x)
    s <- x[idx]
    # unwrap double-escaped markup tags (&amp;lt;em&amp;gt;) once
    s <- gsub("&amp;(lt|gt);", "&\\1;", s, ignore.case = TRUE)
    for (nm in names(html_entities)) {
      hit <- grepl(nm, s, fixed = TRUE)
      if (any(hit)) s[hit] <- gsub(nm, html_entities[[nm]], s[hit], fixed = TRUE)
    }
    # numeric character references (&#8211;, &#x03B1;), guarding control/invalid codepoints
    num <- which(grepl("&#(x[0-9A-Fa-f]+|[0-9]+);", s, perl = TRUE))
    for (k in num) {
      cur <- s[k]
      m <- gregexpr("&#(?:x([0-9A-Fa-f]+)|([0-9]+));", cur, perl = TRUE)
      ms <- regmatches(cur, m)[[1]]
      if (length(ms)) {
        rep_chars <- vapply(ms, \(e) {
          cp <- if (grepl("^&#x", e, ignore.case = TRUE)) {
            strtoi(sub("^&#[xX]([0-9A-Fa-f]+);$", "\\1", e), 16L)
          } else {
            suppressWarnings(as.integer(sub("^&#([0-9]+);$", "\\1", e)))
          }
          if (is.na(cp) || cp < 32L || cp > 1114111L || (cp >= 55296L && cp <= 57343L)) return(" ")
          tryCatch(intToUtf8(cp), error = \(err) " ")
        }, character(1))
        regmatches(cur, m) <- list(rep_chars)
        s[k] <- cur
      }
    }
    # decode &amp; last
    s <- gsub("&amp;", "&", s, fixed = TRUE)
    x[idx] <- s
    x
  }

  # strip recognised HTML/JATS/MathML formatting tags (preserving literal
  # comparisons like "p < .05" and SICI DOI angle brackets)
  markup_tag <- "</?(?:i|b|em|strong|u|sup|sub|scp|small|span|italic|bold|monospace|inf|sc|mml:[a-z0-9-]+|jats:[a-z0-9-]+|tex-math|inline-formula)\\b[^>]*>"
  strip_markup <- \(x) {
    # MathML <mml:annotation> holds a duplicate TeX/plain representation
    x <- gsub("<mml:annotation\\b[^>]*>.*?</mml:annotation>", "", x, ignore.case = TRUE, perl = TRUE)
    gsub(markup_tag, "", x, ignore.case = TRUE, perl = TRUE)
  }

  # strip trailing footnote superscript markers ("Title<sup>1</sup>",
  # "Title<sup>*</sup>") without removing scientific/math superscripts such as
  # "R<sup>2</sup>" or "CO<sup>2</sup>" (even at the end of a title)
  strip_footnote_sup <- \(x) {
    # non-digit footnote symbols (*, dagger, double dagger, section sign)
    x <- gsub("<sup>\\s*[*\u2020\u2021\u00a7,\\- ]+\\s*</sup>\\s*$", "",
              x, ignore.case = TRUE, perl = TRUE)
    # numeric footnote markers only after a >=4-letter word or closing punctuation/quote
    gsub("(?<=\\p{L}{4}|[.\"'?!\u2019\u201d)])<sup>\\s*[0-9,\\- ]+\\s*</sup>\\s*$", "",
         x, ignore.case = TRUE, perl = TRUE)
  }

  # fold isolated Greek letters (and micro sign) to their English names for
  # title comparison ("α-diversity" == "alpha-diversity", "β" == "beta",
  # "δ15N" == "delta15N"), leaving multi-letter Greek words untouched
  greek_map <- c(
    "\u03b1" = "alpha", "\u03b2" = "beta", "\u03b3" = "gamma",
    "\u03b4" = "delta", "\u03b5" = "epsilon", "\u03b6" = "zeta",
    "\u03b7" = "eta", "\u03b8" = "theta", "\u03d1" = "theta",
    "\u03b9" = "iota", "\u03ba" = "kappa", "\u03f0" = "kappa",
    "\u03bb" = "lambda", "\u03bc" = "mu", "\u00b5" = "mu",
    "\u03bd" = "nu", "\u03be" = "xi", "\u03bf" = "omicron",
    "\u03c0" = "pi", "\u03d6" = "pi", "\u03c1" = "rho", "\u03f1" = "rho",
    "\u03c3" = "sigma", "\u03c2" = "sigma", "\u03c4" = "tau",
    "\u03c5" = "upsilon", "\u03c6" = "phi", "\u03d5" = "phi",
    "\u03c7" = "chi", "\u03c8" = "psi", "\u03c9" = "omega"
  )
  fold_greek <- \(x) {
    idx <- which(!is.na(x) & grepl("[\\p{Greek}\u00b5]", x, perl = TRUE))
    if (!length(idx)) return(x)
    s <- x[idx]
    for (g in names(greek_map)) {
      hit <- grepl(g, s, fixed = TRUE)
      if (any(hit)) {
        pat <- paste0("(?<![\\p{Greek}\u00b5])", g, "(?![\\p{Greek}\u00b5])")
        s[hit] <- gsub(pat, paste0(" ", greek_map[[g]], " "), s[hit], perl = TRUE)
      }
    }
    x[idx] <- s
    x
  }

  # rejoin spacing/combining diacritics detached inside words by PDF extraction
  # ("I. Jime \u00b4nez" -> "I. Jimenez", "Gonza \u00b4lez-Gaita \u00b4n" ->
  # "Gonzalez-Gaitan", "Mendonc \u00b8a" -> "Mendonca")
  rejoin_diacritics <- \(x) {
    gsub("(?<=\\p{L})\\s*[\u00b4\u00a8\u00b8\u02c6\u02dc\u00af\u02da\u02c7\u0300-\u036f]+\\s*(?=\\p{L})",
         "", x, perl = TRUE)
  }

  # clean up text to prevent irrelevant mismatches
  clean <- \(x) {
    decode_entities(x) |>
      strip_markup() |>
      # strip invisible Unicode format/bidi control marks (e.g. U+200E LRM, soft hyphen)
      gsub("\\p{Cf}", "", x = _, perl = TRUE) |>
      # strip trademark/copyright symbols before Latin-ASCII turns "\u00ae" into "(R)"
      gsub("[\u00ae\u2122\u00a9]", "", x = _, perl = TRUE) |>
      rejoin_diacritics() |>
      tolower() |>
      deaccent() |>                              # fold diacritics
      gsub("\\p{Pd}", " ", x = _, perl = TRUE) |> # dashes fold to spaces (they were removed: "retractedevil"; U114)
      gsub("\\s+", " ", x = _) |>
      gsub("[\u2018\u2019\u201A\u201B\u0060]", "'", x = _) |>
      # make all single quotes, even doubles
      gsub("[\"\u201C\u201D\u201E\u201F]", "'", x = _) |>
      gsub("\\.\\s*$", "", x = _) # remove . at end
  }

  # journal/container: flag when the cited journal disagrees with Crossref.
  # Skip when either side is missing (NA container is unknown, not a mismatch).
  # Journals are routinely abbreviated (ISO-4: "J. Pers. Soc. Psychol." for
  # "Journal of Personality and Social Psychology"), so a plain string compare
  # over-flags. We drop stop-words, then accept the shorter name when its
  # significant tokens appear in the longer one in order, starting with its
  # first token, allowing each abbreviated token to be a prefix of the full
  # token; the longer name may continue with a subtitle or section ("Journal
  # of Experimental Psychology: General"). A name that starts inside another
  # ("Psychological Science" in "Current Directions in Psychological Science")
  # is another journal. Words lost in
  # reading the PDF ("Perception, & Psychophysics" for "Attention, Perception,
  # & Psychophysics") are allowed when the record's name is in the reference
  # text. "WIREs" is Wiley's short form of "Wiley Interdisciplinary Reviews".
  # Spellings of the same word ("Behaviour" and "Behavior", "Paediatrics"
  # and "Pediatrics", "Analyse" and "Analyze", "Grey" and "Gray") are folded
  # to the American spelling on both sides, from VarCon's list of British,
  # Canadian and Australian spellings (data-raw/spelling_variants.R), which
  # leaves out misspellings. A word that is not listed is folded by its
  # longest listed ending of six or more letters, for compounds
  # ("neurobehavioural").
  spelling <- readRDS(system.file("databases/spelling_variants.Rds", package = "metacheck"))
  # VarCon contains short stem entries (ret=ert, jour=jor, prev=perv, ren=ern)
  # that break ISO-4 journal abbreviations ("J. Coll. Stud. Ret.", "Prev. Med.");
  # keep only genuine short UK/US spelling variants for words <= 4 letters.
  short_uk_us <- c("grey", "haem", "aeon", "tyre", "gaol", "axe", "kerb", "cosy",
                   "tsar", "tzar", "thru", "ikon", "mum", "mums", "fuze", "gage",
                   "nett", "okay", "saki", "tiro", "wiz", "egis", "aery", "eyry",
                   "cagy", "joky", "nite", "pouf")
  spelling <- spelling[nchar(names(spelling)) >= 5L | names(spelling) %in% short_uk_us]
  spelling_env <- list2env(as.list(spelling), hash = TRUE, parent = emptyenv())
  uk_us <- \(w) {
    if (!length(w)) return(w)
    u <- unique(w)
    nz <- !is.na(u) & nzchar(u)
    out <- rep(NA_character_, length(u))
    if (any(nz)) {
      out[nz] <- unlist(mget(u[nz], envir = spelling_env, ifnotfound = NA_character_),
                        use.names = FALSE)
    }
    rest <- which(is.na(out) & nz & nchar(u) > 6L)
    if (length(rest)) {
      nc <- nchar(u[rest])
      max_k <- max(nc) - 1L
      for (k in 6L:max_k) {
        active <- which(nc > k)
        if (!length(active)) break
        r_idx <- rest[active]
        u_act <- u[r_idx]
        nc_act <- nc[active]
        ending <- substring(u_act, nc_act - k + 1L)
        hit <- unlist(mget(ending, envir = spelling_env, ifnotfound = NA_character_),
                      use.names = FALSE)
        ok <- !is.na(hit)
        if (any(ok)) {
          out[r_idx[ok]] <- paste0(substr(u_act[ok], 1L, nc_act[ok] - k), hit[ok])
        }
      }
    }
    res <- ifelse(is.na(out), u, out)
    res[match(w, u)]
  }
  # uk_us for each element of a list of words, in one call
  uk_us_list <- \(l) {
    flat <- uk_us(unlist(l))
    unname(split(flat, factor(rep(seq_along(l), lengths(l)), levels = seq_along(l))))
  }
  # the words of each name, lower case, without "of", "the", ...
  # Attaches attr(toks, "raw") with the pre-uk_us tokens so abbreviated UK stems
  # ("Paediatr" vs uk_us("Paediatric") == "pediatric") still match on their raw form.
  journal_token_list <- \(x) {
    x <- decode_entities(x)
    # preserve ISO-4 "For." (Forest/Forensic/Forschung) BEFORE clean() strips trailing "."
    x <- gsub("(?i)\\bfor\\.", "\001for ", x, perl = TRUE)
    x <- clean(x)
    x <- gsub("&", " and ", x, fixed = TRUE)
    x <- gsub("\\bwires\\b", "wiley interdisciplinary reviews", x)
    # keep letters and digits of all scripts (and the \001 sentinel) so
    # numbered journals ("Periodontol. 2000" == "Periodontology 2000") and
    # non-Latin names stay intact
    x <- gsub("[^\\p{L}\\p{N}\001 ]", " ", x, perl = TRUE)
    # "a"/"e"/"i" after "series"/"section"/"part" or as the final token of a
    # multi-word name ("Phys. Rev. E", "Acta Crystallogr. A") is a section
    # letter, not an article/conjunction; leading "i" ("i-Perception" /
    # "I-Perception") is kept case-insensitively; mid-title "e"/"i"/"y" are
    # dropped as European stopwords regardless of capitalisation.
    sec_words <- c("series", "ser", "section", "sect", "part", "pt")
    stops <- c("of", "and", "the", "for", "in", "on", "a", "an",
               "de", "et", "i", "y", "e", "und", "der", "die", "das",
               "des", "du", "la", "le", "les", "di", "del", "della",
               "delle", "en", "fur", "fuer", "zur", "")
    raw_list <- strsplit(trimws(gsub("\\s+", " ", x)), " ")
    filt_raw <- lapply(raw_list, \(toks) {
      if (!length(toks) || all(is.na(toks))) return(character(0))
      toks <- toks[!is.na(toks)]
      n <- length(toks)
      nxt <- c(toks[-1], "")
      prv <- c("", utils::head(toks, -1))
      nxt_clean <- gsub("\001", "", nxt, fixed = TRUE)
      is_sec_word <- toks %in% sec_words & grepl("^[a-z]$", nxt_clean)
      is_sec_letter <- toks %in% c("a", "e", "i") &
        (prv %in% sec_words | (n >= 2L & seq_len(n) == n))
      is_lead_i <- n >= 2L & seq_len(n) == 1L & toks == "i"
      keep <- !is_sec_word & (is_sec_letter | is_lead_i | !toks %in% stops)
      out <- gsub("\001", "", toks[keep], fixed = TRUE)
      out[!is.na(out) & nzchar(out)]
    })
    folded_list <- uk_us_list(filt_raw)
    Map(\(f, r) structure(f, raw = r), folded_list, filt_raw)
  }
  journal_tokens <- \(x) journal_token_list(x)[[1]]
  # Explicit whitelist of genuine non-prefix ISO-4/Medline contractions
  # ("jt" -> "joint", "natl" -> "national", "intl" -> "international") so
  # ordinary abbreviations ("med", "res") never match unrelated words
  # ("methods", "reports").
  iso4_contractions <- list(
    jt = "joint", natl = "national", intl = "international",
    wkly = "weekly", qtrly = "quarterly", qtly = "quarterly",
    mthly = "monthly", dept = "department", dpt = "department",
    mgmt = "management", mfg = "manufacturing", assn = "association",
    govt = "government", jrnl = "journal", hlth = "health",
    bldg = "building", cntrl = "control", ctrl = "control",
    envt = "environment", cnty = "county", hsp = "hospital"
  )
  is_iso4_contraction <- \(s, l) {
    if (is.na(s) || is.na(l)) return(FALSE)
    target <- iso4_contractions[[s]]
    !is.null(target) && startsWith(l, target)
  }
  tok_match <- \(a, b) {
    if (is.na(a) || is.na(b)) return(FALSE)
    startsWith(b, a) || startsWith(a, b) ||
      is_iso4_contraction(a, b) || is_iso4_contraction(b, a)
  }
  # do tokens ta match tb from position `from` on, a token as a prefix or
  # ISO-4 contraction (on either uk_us-folded or raw tokens)?
  tokens_at <- \(ta, tb, from) {
    idx <- from + seq_along(ta) - 1L
    if (!all(idx <= length(tb))) return(FALSE)
    ra <- attr(ta, "raw") %||% ta
    rb <- attr(tb, "raw") %||% tb
    all(vapply(seq_along(ta), \(k) {
      tok_match(ta[k], tb[idx[k]]) || tok_match(ra[k], rb[idx[k]])
    }, logical(1)))
  }
  # do the tokens of ta begin tb, each token or its abbreviation (a prefix or
  # ISO-4 contraction) in turn? tb may continue (a subtitle or section), but no
  # word of tb may be skipped: "Journal of Experimental Psychology" is not
  # "Journal of Experimental Social Psychology". Two words that make one word
  # (or its abbreviated prefix, e.g. "Cardio-Thoracic" vs "Cardiothorac") are
  # accepted, as are uppercase 2-letter initials ("NY" -> "New York").
  in_sequence <- \(ta, tb, caps_a = character(0), caps_b = character(0)) {
    ra <- attr(ta, "raw") %||% ta
    rb <- attr(tb, "raw") %||% tb
    hit <- \(i, j) tok_match(ta[i], tb[j]) || tok_match(ra[i], rb[j])
    join_hit <- \(single, s_raw, p1, p2, r1, r2) {
      f12 <- paste0(p1, p2); r12 <- paste0(r1, r2)
      f12 == single || r12 == s_raw ||
        (nchar(single) > nchar(p1) && startsWith(f12, single)) ||
        (nchar(s_raw) > nchar(r1) && startsWith(r12, s_raw))
    }
    i <- 1L
    j <- 1L
    while (i <= length(ta)) {
      if (j > length(tb)) return(FALSE)
      if (i < length(ta) && join_hit(tb[j], rb[j], ta[i], ta[i + 1], ra[i], ra[i + 1])) {
        i <- i + 2L; j <- j + 1L
      } else if (j < length(tb) && join_hit(ta[i], ra[i], tb[j], tb[j + 1], rb[j], rb[j + 1])) {
        i <- i + 1L; j <- j + 2L
      } else if (j < length(tb) && nchar(ta[i]) == 2L && ta[i] %in% caps_a &&
                 paste0(substr(tb[j], 1L, 1L), substr(tb[j + 1L], 1L, 1L)) == ta[i]) {
        i <- i + 1L; j <- j + 2L
      } else if (i < length(ta) && nchar(tb[j]) == 2L && tb[j] %in% caps_b &&
                 paste0(substr(ta[i], 1L, 1L), substr(ta[i + 1L], 1L, 1L)) == tb[j]) {
        i <- i + 2L; j <- j + 1L
      } else if (hit(i, j)) {
        i <- i + 1L; j <- j + 1L
      } else return(FALSE)
    }
    TRUE
  }
  # Tight edit tolerance for multi-word journal names of equal token count:
  # allows at most one long word (>= 7 letters) to differ by either (a) a single
  # adjacent-letter transposition ("Neuroscinece" vs "Neuroscience") or (b) a
  # 1-character tail/ending difference after a shared stem of >= max(6, n-3)
  # characters ("J. Neuroscince" vs "Journal of Neuroscience", "J periodontal"
  # vs "Journal of Periodontology"). Keeps distinct specialties ("Hematology"
  # vs "Hepatology", "Ethology" vs "Ethnology") strictly flagged.
  near_journal_tokens <- \(ta, tb) {
    if (length(ta) < 2L || length(ta) != length(tb)) return(FALSE)
    ra <- attr(ta, "raw") %||% ta
    rb <- attr(tb, "raw") %||% tb
    is_adj_swap <- \(a, b) {
      if (nchar(a) != nchar(b) || nchar(a) < 7L) return(FALSE)
      ca <- strsplit(a, "", fixed = TRUE)[[1]]
      cb <- strsplit(b, "", fixed = TRUE)[[1]]
      diff_i <- which(ca != cb)
      length(diff_i) == 2L && diff_i[2L] == diff_i[1L] + 1L &&
        ca[diff_i[1L]] == cb[diff_i[2L]] && ca[diff_i[2L]] == cb[diff_i[1L]]
    }
    lcp_len <- \(a, b) {
      ca <- strsplit(a, "", fixed = TRUE)[[1]]
      cb <- strsplit(b, "", fixed = TRUE)[[1]]
      k <- min(length(ca), length(cb))
      mm <- which(ca[seq_len(k)] != cb[seq_len(k)])
      if (length(mm)) mm[1L] - 1L else k
    }
    fuzzy <- 0L
    for (k in seq_along(ta)) {
      if (tok_match(ta[k], tb[k]) || tok_match(ra[k], rb[k])) next
      a <- ta[k]; b <- tb[k]
      if (nchar(a) < 7L || nchar(b) < 7L) return(FALSE)
      ok <- is_adj_swap(a, b)
      if (!ok) {
        short_t <- if (nchar(a) <= nchar(b)) a else b
        long_t <- if (nchar(a) <= nchar(b)) b else a
        lcp <- lcp_len(short_t, long_t)
        if (lcp >= max(6L, nchar(short_t) - 3L)) {
          d_full <- utils::adist(a, b)[1, 1]
          d_pref <- if (nchar(short_t) >= 8L)
            utils::adist(short_t, substr(long_t, 1L, nchar(short_t)))[1, 1]
          else Inf
          ok <- min(d_full, d_pref) <= 1L
        }
      }
      if (!ok) return(FALSE)
      fuzzy <- fuzzy + 1L
      if (fuzzy > 1L) return(FALSE)
    }
    fuzzy == 1L
  }
  # extract 2-letter ALL-CAPS tokens (e.g. "NY", "UK") from a raw journal string
  caps2_tokens <- \(s) {
    if (is.na(s) || !nzchar(s)) return(character(0))
    m <- regmatches(s, gregexpr("\\b[A-Z]{2}\\b", s, perl = TRUE))[[1]]
    tolower(unique(m))
  }
  # De Gruyter and similar publishers occasionally deposit a 3-6 letter lowercase
  # journal code ("cogl", "bchm") as Crossref's container-title that also appears
  # in the article's DOI ("10.1515/cogl.2005...", "10.1515/bchm...")
  publisher_code_match <- \(cited, record, doi) {
    if (is.na(cited) || is.na(record) || is.na(doi)) return(FALSE)
    rc <- trimws(record)
    if (!grepl("^[a-z]{3,6}$", rc)) return(FALSE)
    if (!grepl(paste0("(?i)(/|\\.)", rc, "(\\.|/|-|$)"), doi, perl = TRUE)) return(FALSE)
    cl <- gsub("[^a-z]", "", clean(cited))
    if (nchar(cl) < nchar(rc)) return(FALSE)
    sc <- strsplit(rc, "", fixed = TRUE)[[1]]
    lc <- strsplit(cl, "", fixed = TRUE)[[1]]
    pos <- 1L; nl <- length(lc)
    for (ch in sc) {
      if (pos > nl) return(FALSE)
      m <- match(ch, lc[pos:nl])
      if (is.na(m)) return(FALSE)
      pos <- pos + m
    }
    TRUE
  }
  journal_coherent <- \(cited, record, text) {
    # bilingual parallel journal titles in PubMed/Medline ("Local Name = English Name")
    if (grepl("\\s*=\\s*", cited)) {
      parts <- trimws(strsplit(cited, "\\s*=\\s*", perl = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) >= 2L &&
          any(vapply(parts, \(p) journal_coherent(p, record, text), logical(1))))
        return(TRUE)
    }
    if (grepl("\\s*=\\s*", record)) {
      parts <- trimws(strsplit(record, "\\s*=\\s*", perl = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts) >= 2L &&
          any(vapply(parts, \(p) journal_coherent(cited, p, text), logical(1))))
        return(TRUE)
    }
    # a journal read as the subtitle of its name ("An International Journal
    # of Research and Practice" in "Infant and Child Development: An
    # International Journal of Research and Practice", or "Biological Sciences"
    # when the reference text prints "Proceedings of the Royal Society ...: Biological Sciences")
    if (!is.na(text)) {
      ct <- clean(text); cc <- clean(cited); cr <- clean(record)
      if (grepl(paste0(cr, ": ", cc), ct, fixed = TRUE)) return(TRUE)
      if (nzchar(cc) && endsWith(cr, paste0(": ", cc)) && grepl(cr, ct, fixed = TRUE))
        return(TRUE)
    }
    # an acronym before a full stop, colon, or hyphen at the start of a name
    # ("MMWR. Morbidity and Mortality Weekly Report", "JARO-J Assoc Res Otolaryngol")
    # may be left out, as may a space-separated leading acronym whose letters match
    # the initials of the remaining title ("PNAS Proceedings of the National Academy...")
    acronym <- "^\\s*[A-Z]{2,6}\\s*[.:\u2013\u2014-]\\s*(?=\\S)"
    strip_leading_acro <- \(s) {
      if (grepl(acronym, s, perl = TRUE)) return(sub(acronym, "", s, perl = TRUE))
      if (grepl("^\\s*[A-Z]{2,6}\\s+\\S", s, perl = TRUE)) {
        lead <- sub("^\\s*([A-Z]{2,6})\\s+.*$", "\\1", s, perl = TRUE)
        rest <- sub("^\\s*[A-Z]{2,6}\\s+", "", s, perl = TRUE)
        rt <- journal_tokens(rest)
        if (length(rt) >= nchar(lead) &&
            startsWith(toupper(paste(substr(rt, 1L, 1L), collapse = "")), lead))
          return(rest)
      }
      s
    }
    # proceedings are cited by the conference name ("AAAI Conference on
    # Artificial Intelligence" for "Proceedings of the AAAI Conference ..."),
    # or by its acronym alone ("AAAI")
    proc <- "(?i)^\\s*proceedings\\s+of\\s+(the\\s+)?"
    if (grepl(proc, record, perl = TRUE)) {
      conf <- sub(proc, "", record, perl = TRUE)
      if (grepl("^\\s*[A-Z]{3,}\\s*$", cited) &&
          trimws(cited) %in% strsplit(conf, "[^[:alnum:]]+")[[1]]) return(TRUE)
      if (names_coherent(cited, conf, text)) return(TRUE)
    }
    # a venue cited by the initials of its name's main words ("ICML" for
    # "International Conference on Machine Learning", "NeurIPS" for "Neural
    # Information Processing Systems")
    short <- gsub("[^[:alpha:]]", "", cited)
    caps <- gsub("[^A-Z]", "", short)
    if (!grepl("\\s", trimws(gsub("\\b\\d{4}\\b", "", cited))) && nchar(caps) >= 3 &&
        identical(caps, toupper(paste(substr(journal_tokens(record), 1, 1), collapse = ""))))
      return(TRUE)
    # PubMed appends a parenthetical city or edition qualifier to one-word
    # journal names ("Cancers (Basel)", "Obesity (Silver Spring)",
    # "BMJ (Clinical Research ed.)"): accept when only one side has a trailing
    # parenthetical qualifier (even if GROBID clipped the closing ")") and the
    # base name is a matching one-word journal.
    city_pat <- "\\s*\\([A-Za-z][A-Za-z .,-]{1,35}\\)?\\s*$"
    if (xor(grepl(city_pat, cited), grepl(city_pat, record))) {
      t1 <- as.character(journal_tokens(sub(city_pat, "", cited)))
      t2 <- as.character(journal_tokens(sub(city_pat, "", record)))
      if (length(t1) == 1L && identical(t1, t2)) return(TRUE)
    }
    c_stripped <- strip_leading_acro(cited)
    r_stripped <- strip_leading_acro(record)
    names_coherent(cited, record, text) ||
      ((c_stripped != cited || r_stripped != record) &&
         names_coherent(c_stripped, r_stripped, text))
  }
  names_coherent <- \(cited, record, text) {
    ta <- journal_tokens(cited); tb <- journal_tokens(record)
    if (!length(ta) || !length(tb)) return(TRUE)        # unknown, don't flag
    # drop a standalone 4-digit conference year (e.g. "EMNLP 2024" vs "EMNLP")
    # when it appears on only one side, while keeping shared numbers such as
    # "Periodontol. 2000" vs "Periodontology 2000"
    drop_one_sided_year <- \(u, v) {
      is_yr <- grepl("^(19|20)\\d{2}$", u)
      if (!any(is_yr)) return(u)
      keep <- !is_yr | (u %in% v)
      if (!any(keep)) return(u)
      ru <- attr(u, "raw") %||% u
      structure(u[keep], raw = ru[keep])
    }
    ta <- drop_one_sided_year(ta, tb)
    tb <- drop_one_sided_year(tb, ta)
    # a one-word name is not abbreviated (ISO 4), so it must match exactly
    # ("CVPR" is not "CVP", "Science" is not "Sciences")
    if (length(ta) == 1 && length(tb) == 1) return(identical(as.character(ta), as.character(tb)))
    ca <- caps2_tokens(cited); cb <- caps2_tokens(record)
    short <- if (length(ta) > length(tb)) tb else ta
    long <- if (length(ta) > length(tb)) ta else tb
    caps_s <- if (length(ta) > length(tb)) cb else ca
    caps_l <- if (length(ta) > length(tb)) ca else cb
    # a one-word compound journal split by a hyphen or space ("Neuro-Image" vs "NeuroImage")
    if (length(short) == 1L && length(long) == 2L) {
      rs <- attr(short, "raw") %||% short
      rl <- attr(long, "raw") %||% long
      if (paste0(long[1L], long[2L]) == short[1L] || paste0(rl[1L], rl[2L]) == rs[1L])
        return(TRUE)
    }
    if (!(length(short) == 1 && length(long) > 1) && in_sequence(short, long, caps_s, caps_l))
      return(TRUE)
    if (near_journal_tokens(ta, tb)) return(TRUE)
    # a one-word journal whose full record name carries a delimited subtitle
    # ("Flora" for "Flora - Morphology, Distribution, Functional Ecology of Plants")
    if (length(short) == 1L && length(long) > 1L) {
      long_raw <- if (length(ta) > length(tb)) cited else record
      sub_sep <- "\\s*(?::|\\s[\u2013\u2014-]\\s)\\s*"
      if (grepl(sub_sep, long_raw, perl = TRUE)) {
        main_toks <- journal_tokens(sub(paste0(sub_sep, ".*$"), "", long_raw, perl = TRUE))
        if (identical(as.character(main_toks), as.character(short))) return(TRUE)
      }
    }
    # the record's name in the reference text, when the parsed journal lost
    # words of it
    if (length(ta) >= length(tb)) return(FALSE)
    # (a word hyphenated at a line break, "Ad- vances", is joined)
    tt <- if (is.na(text)) character() else
      journal_tokens(gsub("(?<=[a-z])- (?=[a-z])", "", text, perl = TRUE))
    length(tt) >= length(tb) &&
      any(vapply(seq_len(length(tt) - length(tb) + 1L),
                 \(k) tokens_at(tb, tt, k), logical(1)))
  }
  # Only compared when the record is a journal article: for book chapters,
  # proceedings and reports the registry's container is often a series or
  # platform name ("Oxford Handbooks Online" for a handbook's title). An
  # article number read as the journal ("Article e17018") is not compared
  # either.
  article_number <- grepl("^(article\\s*)?e?\\d+$", trimws(tolower(table$container.orig)))
  # bibr 12.x calls a journal article "journal_article"
  is_article <- table$bib_type.match %in% c("article", "journal_article") |
    is.na(table$bib_type.match)
  table$container_mismatch <- mapply(\(a, b, text, doi, check) {
    if (!check || is.na(a) || is.na(b) || !nzchar(trimws(a)) || !nzchar(trimws(b)))
      return(FALSE)
    !journal_coherent(a, b, text) && !publisher_code_match(a, b, doi)
  }, table$container.orig, table$container.match, table$text, table$doi.match,
  is_article & !article_number, USE.NAMES = FALSE)
  # A journal cited by another of its names in Wikidata is accepted: an alias
  # of the record's journal, or a name of the journal Wikidata links to it as
  # predecessor or successor (data-raw/journal_successions.R). Crossref's
  # records of back issues may give the current name ("ILR Review" for a 2006
  # article in Industrial and Labor Relations Review), or append a subtitle
  # ("Proceedings of the Royal Society B: Biological Sciences" for Wikidata's
  # "Proceedings of the Royal Society B"). Indexed on the first normalised
  # token so leading articles ("The ...") and punctuation align with the
  # comparator. Used for both DOI-matched and title-searched references.
  as_listed <- \(cited, name) {
    ta <- journal_tokens(cited); tb <- journal_tokens(name)
    if (!length(ta) || !length(tb)) return(FALSE)
    if (length(ta) == 1) {
      if (identical(as.character(ta), as.character(tb))) return(TRUE)
      # Wikidata aliases often append a city ("Europace (London, England)") or
      # colon/dash subtitle ("Europace : Journal of the Working Groups...")
      base <- sub("\\s*(?::|\\s[\u2013\u2014-]\\s).*$", "", name, perl = TRUE)
      base <- sub("\\s*\\([A-Za-z][A-Za-z .,-]{1,35}\\)?\\s*$", "", base)
      return(base != name && identical(as.character(ta), as.character(journal_tokens(base))))
    }
    length(ta) <= length(tb) && in_sequence(ta, tb, caps2_tokens(cited), caps2_tokens(name))
  }
  journal_succeeds <- local({
    js <- NULL; js_journal <- NULL; js_quick <- NULL
    # Fast first-word key matching journal_tokens() (strips leading articles
    # "the/a/an", punctuation, and folds diacritics) so only the tiny subset of
    # Wikidata names starting with the same word is tokenised with uk_us().
    quick_key <- \(x) {
      x <- tolower(trimws(deaccent(x)))
      x <- sub("^(the|a|an)\\s+", "", x, perl = TRUE)
      substr(gsub("[^\\p{L}]", "", x, perl = TRUE), 1L, 5L)
    }
    \(cited, record) {
      if (is.na(cited) || is.na(record) || !nzchar(trimws(cited)) || !nzchar(trimws(record)))
        return(FALSE)
      if (is.null(js)) {
        js <<- readRDS(system.file("databases/journal_successions.Rds", package = "metacheck"))
        js_journal <<- rep(seq_along(js$size), js$size)
        js_quick <<- quick_key(js$names)
      }
      rec <- as.character(journal_tokens(record))
      if (!length(rec)) return(FALSE)
      cand <- which(js_quick == quick_key(record))
      same <- if (length(cand)) vapply(journal_token_list(js$names[cand]), \(w) {
        wc <- as.character(w)
        if (length(rec) == 1) identical(wc, rec) else
          length(rec) <= length(wc) && identical(wc[seq_along(rec)], rec)
      }, logical(1)) else logical(0)
      journals <- unique(js_journal[cand[same]])
      # if Crossref appends a subtitle (": Biological Sciences") to a specific
      # multi-word name that Wikidata stores without the subtitle, match the
      # main title before the colon exactly
      if (!length(journals) && grepl(":", record, fixed = TRUE)) {
        main_str <- sub("\\s*:.*$", "", record)
        rec_main <- as.character(journal_tokens(main_str))
        if (length(rec_main) >= 2L) {
          cand2 <- which(js_quick == quick_key(main_str))
          same2 <- if (length(cand2)) vapply(journal_token_list(js$names[cand2]),
                                             \(w) identical(as.character(w), rec_main), logical(1)) else logical(0)
          journals <- unique(js_journal[cand2[same2]])
        }
      }
      if (!length(journals)) return(FALSE)
      journals <- unique(c(journals, js$links[js$links[, 1] %in% journals, 2]))
      alt <- setdiff(js$names[js_journal %in% journals], record)
      if (any(vapply(alt, \(a) as_listed(cited, a), logical(1)))) return(TRUE)
      # reverse direction: cited carries a subtitle ("Proceedings of the Royal
      # Society B: Biological Sciences") while Wikidata stores the alias without
      # the subtitle ("Proceedings of the Royal Society B"). When record also
      # carries a subtitle, require the subtitles to agree.
      if (grepl(":", cited, fixed = TRUE)) {
        cited_main <- sub("\\s*:.*$", "", cited)
        sub_ok <- !grepl(":", record, fixed = TRUE) ||
          journal_coherent(sub("^[^:]*:\\s*", "", cited),
                           sub("^[^:]*:\\s*", "", record), NA_character_)
        if (sub_ok && length(journal_tokens(cited_main)) >= 2L &&
            any(vapply(alt, \(a) as_listed(cited_main, a), logical(1))))
          return(TRUE)
      }
      FALSE
    }
  })
  jmm <- which(table$container_mismatch)
  for (k in jmm) {
    if (journal_succeeds(table$container.orig[k], table$container.match[k])) {
      table$container_mismatch[k] <- FALSE
    } else if (!is.na(table$text[k]) && grepl(":", table$container.match[k], fixed = TRUE)) {
      # subtitle-only container.orig ("Biological Sciences") when the full
      # predecessor/successor name appears in the reference text before it
      cc <- clean(table$container.orig[k])
      cr <- clean(table$container.match[k])
      if (nzchar(cc) && endsWith(cr, paste0(": ", cc))) {
        ct <- clean(table$text[k])
        at <- regexpr(cc, ct, fixed = TRUE)
        if (at > 1L) {
          # extract the clause containing the journal before the subtitle
          pre_sub <- trimws(substr(ct, 1L, at - 1L))
          # strip everything up to the end of the title (last sentence before journal)
          cand_j <- trimws(sub("^.*\\.\\s+(?=[a-z][^.]*$)", "", pre_sub, perl = TRUE))
          # also check the longer segment after the title if the journal itself has a dot ("London. Series B")
          if (nzchar(pre_sub)) {
            tt <- if (!is.na(table$title.orig[k]) && nzchar(trimws(table$title.orig[k])))
              clean(table$title.orig[k]) else ""
            after_t <- if (nzchar(tt) && grepl(tt, pre_sub, fixed = TRUE)) {
              trimws(sub(paste0("^.*?", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", tt), "\\W*"),
                         "", pre_sub, perl = TRUE))
            } else cand_j
            full_in_text <- trimws(paste(after_t, cc))
            if (nzchar(after_t) &&
                (journal_coherent(full_in_text, table$container.match[k], NA_character_) ||
                   journal_succeeds(full_in_text, table$container.match[k])))
              table$container_mismatch[k] <- FALSE
          }
        }
      }
    }
  }

  # title check: compare on character similarity rather than exact equality, so
  # that formatting noise (footnote markers, em-dash spacing, colons/question
  # marks, line-break word splits like "Proba bilistic") does not flag a real
  # title, while a genuinely different title still does. norm_title strips
  # trailing footnote superscripts, HTML entities/tags and punctuation, folds
  # dashes, diacritics and isolated Greek letters.
  norm_title <- \(x) {
    x <- strip_footnote_sup(decode_entities(x))
    x <- fold_greek(clean(x))
    # letters and digits of every script: keeping only a-z emptied a title
    # in Cyrillic or Chinese, which then agreed with any record
    gsub("[^\\p{L}\\p{N}]", "", x, perl = TRUE)
  }
  # normalised word list used by same_title(), anchored in_text, and
  # title_spill_in_container()
  title_words <- \(x) {
    x <- strip_footnote_sup(decode_entities(x))
    x <- fold_greek(clean(x))
    x <- gsub("&", " and ", x, fixed = TRUE)
    uk_us_list(strsplit(trimws(gsub("[^\\p{L}\\p{N}]+", " ", x, perl = TRUE)), " "))
  }
  title_squash <- \(x) vapply(title_words(x), paste, "", collapse = "")
  # helper: does word vector `needle` appear contiguously in `haystack`?
  subseq_pos <- \(needle, haystack) {
    nl <- length(needle); hl <- length(haystack)
    if (!nl || hl < nl) return(integer(0))
    starts <- which(haystack[seq_len(hl - nl + 1L)] == needle[1L])
    if (!length(starts)) return(integer(0))
    starts[vapply(starts, \(s) identical(haystack[s:(s + nl - 1L)], needle), logical(1))]
  }

  # End of title spilled into container.orig ("Ceará. Rodriguésia",
  # "Alaska. Journal of Experimental Zoology"): when container.orig has a
  # sentence boundary, check that the part after it matches the record's
  # journal independently AND the part before it is the end of the record's
  # title (reconstructing the record's title with title.orig when present).
  title_spill_in_container <- \(cited_c, rec_c, cited_t, rec_t, text) {
    if (is.na(cited_c) || is.na(rec_c) || is.na(rec_t) ||
        !grepl("[.?!]\\s+", cited_c, perl = TRUE)) return(FALSE)
    m <- gregexpr("[.?!]\\s+", cited_c, perl = TRUE)[[1]]
    ml <- attr(m, "match.length")
    if (length(m) == 1L && m == -1L) return(FALSE)
    rec_sq <- title_squash(rec_t)
    if (!nzchar(rec_sq)) return(FALSE)
    cit_sq <- if (is.na(cited_t) || !nzchar(trimws(cited_t))) "" else title_squash(cited_t)
    for (idx in seq_along(m)) {
      prefix <- trimws(substr(cited_c, 1L, m[idx] - 1L))
      rest <- trimws(substring(cited_c, m[idx] + ml[idx]))
      if (!nzchar(prefix) || !nzchar(rest)) next
      pref_sq <- title_squash(prefix)
      if (!nzchar(pref_sq) || !endsWith(rec_sq, pref_sq)) next
      # when a cited title was read, cited_title + spill must reconstruct the
      # record title (allowing a digits-only year-range gap when GROBID split a
      # title on an embedded year like ", 2000-2010, Rwanda")
      if (nzchar(cit_sq) && paste0(cit_sq, pref_sq) != rec_sq) {
        if (!startsWith(rec_sq, cit_sq)) next
        gap_end <- nchar(rec_sq) - nchar(pref_sq)
        gap <- if (gap_end >= nchar(cit_sq)) substr(rec_sq, nchar(cit_sq) + 1L, gap_end) else ""
        if (nchar(cit_sq) + nchar(pref_sq) < nchar(rec_sq) - 2L &&
            !grepl("^[0-9]+$", gap)) next
      }
      if (journal_coherent(rest, rec_c, text) || journal_succeeds(rest, rec_c))
        return(TRUE)
    }
    FALSE
  }
  jmm_spill <- which(table$container_mismatch)
  for (k in jmm_spill) {
    if (title_spill_in_container(table$container.orig[k], table$container.match[k],
                                 table$title.orig[k], table$title.match[k], table$text[k]))
      table$container_mismatch[k] <- FALSE
  }

  # does a cited title agree with a record's title? Vectorised over the three
  # arguments; also used for the DataCite records found for a reference
  # without a DOI. Optional `container` guards anchored in_text against
  # matching a journal name printed in the reference text.
  title_check <- \(cited, record, text, container = rep(NA_character_, length(cited))) {
    if (!length(cited)) return(list(mismatch = logical(0), agrees = logical(0),
                                    starts_same = logical(0), charsim = numeric(0),
                                    exact = logical(0), same = logical(0)))
    a <- norm_title(cited)
    b <- norm_title(record)
    charsim <- mapply(\(x, y) {
      if (is.na(x) || is.na(y) || !nzchar(x) || !nzchar(y)) return(NA_real_)
      1 - utils::adist(x, y)[1, 1] / max(nchar(x), nchar(y))
    }, a, b, USE.NAMES = FALSE)

    # also accept when the record title appears intact in the reference text
    # at the title position (handles cases where GROBID cut the title short at
    # a parenthesis or line break). Compares normalised word sequences (same
    # HTML/Greek/UK-US/de-hyphenation folding as same_title), anchored to the
    # parsed title so a journal name printed in the reference text (e.g.
    # "Annual Review of Ecology and Evolution" for a paper titled "Ecology")
    # cannot rescue a wrong title. Only computed for rows with both record and
    # text present.
    in_text <- rep(FALSE, length(cited))
    need_it <- which(!is.na(record) & nzchar(trimws(record)) & !is.na(text) & nzchar(trimws(text)))
    if (length(need_it)) {
      clean_text <- clean(dehyphen(text[need_it]))
      match_clean <- clean(record[need_it])
      escape <- \(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
      rec_wl <- title_words(record[need_it])
      cit_wl <- title_words(cited[need_it])
      txt_wl <- title_words(dehyphen(text[need_it]))
      cont_wl <- title_words(container[need_it])
      in_text[need_it] <- vapply(seq_along(need_it), \(j) {
        rw <- rec_wl[[j]]; rw <- rw[nzchar(rw)]
        tw <- txt_wl[[j]]; tw <- tw[nzchar(tw)]
        if (!length(rw) || length(tw) < length(rw)) return(FALSE)
        cw <- cit_wl[[j]]; cw <- cw[nzchar(cw)]
        # do not let a record title match when it is actually the cited journal
        kw <- cont_wl[[j]]; kw <- kw[nzchar(kw)]
        if (length(kw) && identical(rw, kw)) return(FALSE)
        # anchor to the parsed title when one was read: either the record title
        # starts/ends with the parsed title (GROBID cut title.orig short), or
        # the parsed title starts with the record title (GROBID appended an APA
        # "(Version 1.0) [Computer code]" suffix to title.orig), or GROBID
        # mistakenly put the "In: <Proceedings/Book>" line into title.orig
        if (length(cw)) {
          in_book_title <- grepl("^in\\s*:", trimws(cited[need_it][j]), ignore.case = TRUE) &&
            length(rw) >= 4L
          anchored <- in_book_title || identical(rw, cw) ||
            (length(cw) >= 2L && length(rw) >= length(cw) && identical(rw[seq_along(cw)], cw)) ||
            (length(rw) >= 2L && length(cw) > length(rw) && identical(cw[seq_along(rw)], rw)) ||
            (length(cw) >= 3L && length(rw) >= length(cw) && identical(utils::tail(rw, length(cw)), cw)) ||
            (length(cw) >= 3L && length(rw) >= length(cw) && length(cw) / length(rw) >= 0.4 && length(subseq_pos(cw, rw)) > 0L)
          if (!anchored) return(FALSE)
        } else if (length(rw) < 3L) {
          return(FALSE)
        }
        pos <- subseq_pos(rw, tw)
        if (!length(pos)) return(FALSE)
        # check that the matched title ends at a title delimiter, bracket or end
        # of text (not mid-phrase like "Origins" inside "Origins of species")
        pat <- match_clean[j]
        if (!is.na(pat) && nzchar(pat) && grepl(pat, clean_text[j], fixed = TRUE)) {
          if (grepl(paste0("(^|[^\\p{L}\\p{N}])", escape(pat),
                           if (grepl("[:?.!]$", pat)) "" else "\\s*($|[:?.!(\\[])"),
                    clean_text[j], perl = TRUE)) return(TRUE)
        }
        # word-level boundary check when spelling/punctuation/Greek differed
        last_w <- rw[length(rw)]
        end_i <- pos + length(rw) - 1L
        if (any(end_i == length(tw))) return(TRUE)
        # verify in the raw/cleaned text that the last word of rw is followed by
        # a title delimiter or bracket (., ?, !, :, (, [) rather than a plain word
        grepl(paste0("\\b", escape(last_w), "\\s*($|[:?.!(\\[])"),
              fold_greek(clean_text[j]), perl = TRUE)
      }, logical(1))
    }

    # Crossref often holds only the main title, without the subtitle the
    # reference cites ("Marginally significant effects as evidence for
    # hypotheses" for "...: Changing attitudes over four decades"). A cited
    # title that starts with the record's title is accepted when the rest
    # starts at a title delimiter (":", "?", "." or "!"); the subtitle itself
    # cannot then be checked.
    delimited <- \(x) {
      x <- strip_footnote_sup(decode_entities(x))
      trimws(gsub("[^\\p{L}\\p{N}:?.!]+", " ", fold_greek(clean(x)), perl = TRUE))
    }
    # The citation may also omit a subtitle that the record has.
    starts_at_delimiter <- \(long, short) {
      if (is.na(long) || is.na(short) || nchar(short) < 20 ||
          !startsWith(long, short)) return(FALSE)
      grepl("[:?.!]$", short) ||
        grepl("^ ?[:?.!]", substring(long, nchar(short) + 1))
    }
    cited_d <- delimited(cited)
    record_d <- delimited(record)
    main_title <- mapply(\(c, r) starts_at_delimiter(c, r) || starts_at_delimiter(r, c),
                         cited_d, record_d, USE.NAMES = FALSE)

    # APA gives the English translation of a title in another language in
    # square brackets ("Características vocais ... [Vocal acoustic
    # characteristics ...]"), which may be close to the record's title
    translated <- mapply(\(cited, y) {
      tr <- regmatches(cited, regexpr("\\[[^]]+\\]", cited))
      if (is.na(y) || !nzchar(y) || !length(tr)) return(FALSE)
      x <- norm_title(tr)
      nzchar(x) && 1 - utils::adist(x, y)[1, 1] / max(nchar(x), nchar(y)) >= title_similarity
    }, ifelse(is.na(cited), "", cited), b, USE.NAMES = FALSE)

    # For a suggested DOI, a title that only adds or drops words at the end
    # (a subtitle without its colon, a footnote Crossref appended to the
    # title) is close enough: the suggestion is checked by the user.
    starts_same <- mapply(\(x, y) {
      if (is.na(x) || is.na(y)) return(FALSE)
      short <- if (nchar(x) < nchar(y)) x else y
      nchar(short) >= 20 && startsWith(if (nchar(x) < nchar(y)) y else x, short)
    }, a, b, USE.NAMES = FALSE)

    # A title that was not read (or a record without one) neither agrees nor
    # disagrees: it is not a mismatch, but no DOI is suggested for it.
    known <- !is.na(charsim)
    agrees <- (known & charsim >= title_similarity) | in_text | main_title | translated

    # A title that agrees identifies the work; it is the same title only when,
    # after folding case, diacritics, Unicode forms ("SDO\u2087" and "SDO7"),
    # "&", punctuation, spacing, isolated Greek letters and British/American
    # spellings, the two are equal, or one is the other's part before or after
    # a title delimiter (":", "?", "." or "!"): a citation or record without
    # the subtitle, or without a main title or acronym before it ("PRBoost: ...").
    # A title that stops or continues without a delimiter is another title
    # ("Effects of training on memory" is not "Effects of training on memory
    # in older adults"). Any other difference, even of one word, is a title
    # mismatch.
    same_title <- \(x, y) {
      if (is.na(x) || is.na(y)) return(FALSE)
      # an APA description after the cited title ("[Conference session]")
      x <- sub("\\s*\\[[^]]*\\]?\\s*$", "", stringi::stri_trans_nfkc(x))
      y <- stringi::stri_trans_nfkc(y)
      sx <- title_squash(x)
      sy <- title_squash(y)
      if (!nzchar(sx) || !nzchar(sy)) return(FALSE)
      if (sx == sy) return(TRUE)
      # registry UTF-8 encoding corruption in record title ("\uFFFD" replacement
      # character or mid-word "??"): match each corrupted run as 1-3 letters
      # when the rest of the title matches identically
      if (grepl("\uFFFD|(?<=\\p{L})\\?{2,}", y, perl = TRUE)) {
        y_mark <- gsub("\uFFFD+|(?<=\\p{L})\\?{2,}", "\002", y, perl = TRUE)
        parts <- strsplit(paste0(y_mark, " "), "\002", fixed = TRUE)[[1]]
        psq <- vapply(parts, title_squash, "")
        if (sum(nchar(psq)) >= 10L) {
          pat <- paste0("^", paste(psq, collapse = "[a-z0-9]{1,3}"), "$")
          if (grepl(pat, sx, perl = TRUE)) return(TRUE)
        }
      }
      # a footnote that the record appends to its title ("...adolescence11The
      # full text of this article ..."): a marker followed by a sentence, not a
      # number that continues the title ("Memory across adult life 2")
      if (startsWith(sy, sx) && grepl("^[0-9]{1,3}[a-z]{20,}", substring(sy, nchar(sx) + 1))) return(TRUE)
      # a bare trailing 1-2 digit footnote number glued directly (no space) onto
      # a >=4-letter word or closing quote at the end of one side only
      # ("...markers12" vs "...markers", '..."citizenship"1' vs '..."citizenship"',
      # "...gender1" vs "...Gender<sup>1</sup>"), preserving spaced numbers
      # ("Memory across adult life 2") and short codes ("R2", "CO2").
      glued_fn <- "(?<=\\p{L}{4})(['\"\u2019\u201d]?)[1-9][0-9]?(?=\\.?\\s*$)"
      if (grepl(glued_fn, y, perl = TRUE) &&
          title_squash(sub(glued_fn, "\\1", y, perl = TRUE)) == sx) return(TRUE)
      if (grepl(glued_fn, x, perl = TRUE) &&
          title_squash(sub(glued_fn, "\\1", x, perl = TRUE)) == sy) return(TRUE)
      long <- if (nchar(sx) > nchar(sy)) x else y
      long_is_rec <- nchar(sy) > nchar(sx)
      short <- if (nchar(sx) > nchar(sy)) sy else sx
      cut <- gregexpr("[:?.!\u2014]", long)[[1]]
      cut <- cut[cut > 0]
      # do not match a commentary/erratum record ("Re: <Title>", "Comment on: <Title>",
      # "Erratum: <Title>") to the original cited paper
      commentary_prefix <- paste0(
        "^(re|comment(\\s+on)?|comments(\\s+on)?|reply(\\s+to)?|response(\\s+to)?|",
        "erratum(\\s+to)?|corrigendum(\\s+to)?|correction(\\s+to)?|retraction(\\s+of)?|",
        "letter(\\s+to|\\s+regarding|\\s+on)?|book\\s+review|review\\s+of|",
        "editorial\\s+on|discussion(\\s+of)?)$"
      )
      any(vapply(cut, \(k) {
        if (title_squash(substr(long, 1, k - 1)) == short) return(TRUE)
        if (title_squash(substring(long, k + 1)) == short) {
          if (long_is_rec && grepl(commentary_prefix, trimws(tolower(substr(long, 1, k - 1))), perl = TRUE))
            return(FALSE)
          return(TRUE)
        }
        FALSE
      }, logical(1)))
    }
    same <- known & as.logical(unlist(mapply(same_title, cited, record, USE.NAMES = FALSE)))
    # the record's whole title in the printed reference, when it is not
    # shorter than the title read, which reading the PDF cut short ("... A
    # correction to Cousineau" for "... A correction to Cousineau (2005)");
    # a shorter record title in the reference may be part of another title
    # ("Beck Depression Inventory-II" in "Manual for the Beck Depression
    # Inventory-II")
    same <- same | (known & in_text & nchar(b) >= nchar(a))
    # a title found in the reference text, or a translated title, is not a
    # mismatch, but a DOI found by its title is suggested only for the same
    # title ("Beck Depression Inventory-II" is in "Manual for the Beck
    # Depression Inventory-II")
    exact <- same | (known & (in_text | translated))
    list(mismatch = known & !agrees, agrees = agrees, starts_same = starts_same,
         charsim = charsim, exact = exact, same = same)
  }
  tc <- title_check(table$title.orig, table$title.match, table$text, table$container.orig)
  # a record with another title is compared only when it is the record of the
  # cited DOI; then any difference in the title is a mismatch
  table$title_mismatch <- !is.na(tc$charsim) & !tc$exact
  table$title_starts_same <- tc$starts_same
  table$title_agrees <- tc$agrees
  title_charsim <- tc$charsim

  # Before a year is flagged, Crossref's dates for the record are read.
  # bib_match keeps the print year, and some publishers (IEEE among them)
  # print a paper years after it appears online without depositing the
  # online date. A cited year within `year_tolerance` of the online, print,
  # issue or posting date is not a mismatch. A cited year that is only near
  # the year the DOI was created, or the work accepted, is a note
  # (`year_note`), not a flag. Only records whose title agrees are read.
  record_dates <- c(published = "published-online", published = "published-print",
                    published = "issued", published = "posted",
                    created = "created", accepted = "accepted", approved = "approved")
  # for each DOI, "published" when the cited year is near a publication date,
  # the name of another date it is near, or NA; with the year of that date
  # (attribute "year")
  # A date that is not a publication date excuses a cited year only when the
  # work was published at most a year more than `year_tolerance` from it
  # (`record`, the record's year), so that a much later publication is
  # still flagged.
  year_source <- \(doi, cited, record) {
    out <- rep(NA_character_, length(doi))
    attr(out, "year") <- rep(NA_integer_, length(doi))
    ok <- which(doi_valid_format(doi_clean(doi)) %in% TRUE & !is.na(cited))
    if (!length(ok)) return(out)
    cr <- tryCatch(suppressMessages(crossref_doi(doi[ok], select = c("DOI", record_dates))),
                   error = \(e) NULL)
    if (is.null(cr) || nrow(cr) != length(ok)) return(out)
    yrs <- vapply(record_dates, \(col)
      suppressWarnings(as.integer(substr(cr[[col]] %||% rep(NA, nrow(cr)), 1, 4))),
      integer(length(ok)))
    yrs <- matrix(yrs, nrow = length(ok))
    # a DOI created after the work was published (deposited for an old
    # paper) says nothing about the year it appeared
    pub <- suppressWarnings(apply(yrs[, names(record_dates) == "published", drop = FALSE],
                                  1, min, na.rm = TRUE))
    after <- !is.na(yrs) & col(yrs) > sum(names(record_dates) == "published") &
      is.finite(pub) & yrs > pub
    yrs[after] <- NA
    near <- !is.na(yrs) & abs(yrs - cited[ok]) <= year_tolerance
    near <- matrix(near, nrow = length(ok))
    k <- apply(near, 1, \(r) which(r)[1])
    y <- attr(out, "year")
    out[ok] <- names(record_dates)[k]
    y[ok] <- yrs[cbind(seq_along(ok), k)]
    far <- !is.na(out) & out != "published" &
      !(abs(cited - as.integer(record)) <= year_tolerance + 1) %in% TRUE
    out[far] <- NA
    y[far] <- NA
    attr(out, "year") <- y
    out
  }
  year_note_text <- \(src, year, record) sprintf(
    "the cited year is near the year of the %s (%s); the work was published in %s",
    c(created = "DOI's creation", accepted = "work's acceptance",
      approved = "work's approval")[src], year, record)
  table$year_note <- NA_character_
  # the record each note refers to
  note_record <- rep(NA_character_, nrow(table))
  ychk <- which(table$year_mismatch & (table$title_agrees | table$title_starts_same))
  if (length(ychk)) {
    src <- year_source(table$doi.match[ychk], as.integer(table$year.orig[ychk]),
                       table$year.match[ychk])
    table$year_mismatch[ychk[!is.na(src)]] <- FALSE
    is_soft <- !is.na(src) & src != "published"
    soft <- ychk[is_soft]
    table$year_note[soft] <- year_note_text(src[is_soft], attr(src, "year")[is_soft],
                                            table$year.match[soft])
    note_record[soft] <- table$doi.match[soft]
  }

  # A reference that cites a preprint (its journal, or its text when no
  # journal was read, names a preprint server) is not compared on its year or
  # journal with a published version of the work, and a reference that cites
  # a published version may be a year later than its preprint (see the title
  # search below). A preprint with a published version
  # is noted (`published_doi`), so that a reviewer can see that it has been
  # superseded.
  preprint_words <- "(?i)\\b(arxiv|corr|biorxiv|medrxiv|psyarxiv|socarxiv|edarxiv|chemrxiv|ssrn|research square|preprints?)\\b"
  cited_preprint <- table$bib_type.orig %in% "preprint" |
    (!blank(table$container.orig) & grepl(preprint_words, table$container.orig, perl = TRUE)) |
    (blank(table$container.orig) & grepl(preprint_words, table$text, perl = TRUE))
  table$published_doi <- NA_character_
  # A reference cited in a journal whose work is found only as a preprint is
  # flagged (`preprint_only`): the journal article may not exist. Conference
  # papers are left out, because many proceedings (ICML, NeurIPS, ICLR) have
  # no DOIs, so their papers are found only as preprints. A venue is taken as
  # a conference when it names one ("Proceedings", "Workshop", ...), has an
  # acronym ("ICML", "Tiny Papers @ ICLR"), or the reference is typed as a
  # conference paper.
  conference_words <- "(?i)\\b(proc|proceedings|conference|conf|workshops?|symposium|meeting|congress|colloquium|summit|track)\\b|advances in neural information processing"
  acronym_word <- "(^|[^[:alnum:]])[[:alnum:]]*[A-Z][[:alnum:]]*[A-Z][[:alnum:]]*($|[^[:alnum:]])"
  # Journals whose names look like conferences ("IEEE Transactions on ...",
  # "Proceedings of the National Academy of Sciences", "PLoS ONE") are taken
  # as journals: names of items with an ISSN in Wikidata (data-raw/journal_names.R),
  # matched as journal names are, and a few journals cited by their acronym
  # alone ("PNAS", "JMLR").
  journal_names <- readRDS(system.file("databases/journal_names.Rds", package = "metacheck"))
  listed_journal <- local({
    first <- NULL
    first_word <- \(x) {
      x <- tolower(trimws(deaccent(x)))
      x <- gsub("\\bwires\\b", "wiley", x)
      x <- sub("^(?:(?:of|and|the|for|in|on|a|an|de|et)\\b[^\\p{L}]*)+", "", x, perl = TRUE)
      sub("^[^\\p{L}]*([\\p{L}]+).*$", "\\1", x, perl = TRUE)
    }
    \(x) {
      if (trimws(x) %in% journal_names$acronyms) return(TRUE)
      tok <- journal_tokens(x)
      if (!length(tok)) return(FALSE)
      if (is.null(first)) first <<- uk_us(first_word(journal_names$titles))
      cand <- journal_names$titles[nzchar(first) &
                                     (startsWith(first, tok[1]) | startsWith(tok[1], first))]
      # narrow large buckets (e.g. "Proceedings of ...") by the second token
      # prefix (both folded and raw UK/US forms) before running full comparison
      if (length(cand) > 50L && length(tok) >= 2L && nchar(tok[2]) >= 2L) {
        tok_raw <- attr(tok, "raw") %||% tok
        p2 <- substr(tok[2], 1L, min(3L, nchar(tok[2])))
        p2_raw <- substr(tok_raw[2], 1L, min(3L, nchar(tok_raw[2])))
        cand_low <- tolower(deaccent(cand))
        keep2 <- grepl(p2, cand_low, fixed = TRUE) |
          (p2_raw != p2 & grepl(p2_raw, cand_low, fixed = TRUE))
        cand <- cand[keep2]
      }
      # a conference is not the journal its name begins with ("Proceedings
      # of the IEEE Computer Society Conference ..." is not "Proceedings of
      # the IEEE")
      if (grepl(event_words, x, perl = TRUE))
        cand <- cand[grepl(event_words, cand, perl = TRUE)]
      if (!length(cand)) return(FALSE)
      any(vapply(cand, \(t) journal_coherent(x, t, NA_character_), logical(1)))
    }
  })
  # a name with a journal word and no event word is a journal ("Journal of
  # the International Neuropsychological Society: JINS", "ACM Transactions
  # on Graphics")
  event_words <- "(?i)\\b(conferences?|symposium|workshops?|meetings?|congress|colloquium|summit)\\b"
  journal_words <- "(?i)\\b(journal|reviews?|wires|letters|transactions|bulletin|annals|quarterly|magazine)\\b"
  looks_like_conference <- !blank(table$container.orig) &
    (grepl(conference_words, table$container.orig, perl = TRUE) |
       grepl(acronym_word, table$container.orig)) &
    !(grepl(journal_words, table$container.orig, perl = TRUE) &
        !grepl(event_words, table$container.orig, perl = TRUE))
  # journal_cited is only needed for references without a DOI (or unresolved
  # DOIs) in papers that were looked up
  need_jc <- looks_like_conference & !cited_preprint &
    as.character(table$paper_id) %in% looked_up_ids &
    (blank(table$doi.orig) | blank(table$title.match))
  check <- unique(table$container.orig[need_jc])
  listed <- if (length(check)) check[vapply(check, listed_journal, logical(1))] else character(0)
  journal_cited <- !cited_preprint & !blank(table$container.orig) &
    (!looks_like_conference | table$container.orig %in% listed) &
    !table$bib_type.orig %in% c("inproceedings", "proceedings", "conference",
                                "book", "inbook", "incollection", "techreport",
                                "phdthesis", "mastersthesis", "misc", "preprint")
  table$preprint_only <- FALSE


  # author check: do the leading author surnames from the retrieved record
  # appear in the cited author list, and is the record's first author cited
  # first? Only the first `max_authors` are required, because author lists are
  # commonly truncated with "et al." (and GROBID often drops the tail of long
  # lists), so a missing trailing author is not evidence of an incoherent
  # citation. Diacritics are ignored (handled by clean()).
  last_names <- lapply(table$authors.match, \(a) {
    tryCatch({
      # the given names go along, for names with their parts swapped
      if (is.data.frame(a)) {
        structure(a$family, given = if ("given" %in% names(a)) a$given else rep(NA_character_, nrow(a)))
      } else {
        names <- strsplit(a, "; ")[[1]]
        structure(unname(sapply(names, \(x) strsplit(x, ", ")[[1]][[1]])),
                  given = unname(sapply(names, \(x) strsplit(x, ", ")[[1]][2])))
      }
    }, error = \(e) return(NA))
  })

  # The cited authors are read one person at a time ("Family, Given; ..."),
  # and a record surname must equal one cited surname. Surnames are compared
  # on their letters only, so spacing in particles and compound names ("Le
  # Doux" and "LeDoux", or Crossref's non-breaking space in "de Lange") does
  # not matter. When one side is a single word, it also matches the other
  # side's last word: Crossref sometimes has the given name in the family
  # field ("Mike Burton"), citations shorten compound surnames ("Vafaei
  # Shooshtari" cited as "Shooshtari"), and Crossref sometimes drops the
  # particle that the citation keeps ("Gelder" cited as "De Gelder"). A
  # cited name without a comma is taken as "Given Family": the record
  # surname must equal its last word or words ("Wood" is not found in
  # "John Blackwood"). bibr 12.x exports keep the author list as printed
  # ("Adam, H., Shirako, A., & Maddux, W. W"), which cannot be split into
  # people reliably, so there a record surname must equal one or more
  # consecutive whole words of the list ("Adam" is not found in "McAdam").
  letters_only <- \(x) gsub("[^\\p{L}]", "", clean(x), perl = TRUE)
  # ASCII umlaut/Nordic transliteration expansion (a-umlaut -> ae, o-umlaut/o-stroke -> oe,
  # u-umlaut -> ue, a-ring -> aa): applied only to strings containing the actual
  # diacritic so English names ("Bauer", "Goel") are never over-merged.
  umlaut_expand <- \(x) {
    s <- tolower(decode_entities(x))
    s <- gsub("\u00e4", "ae", s, fixed = TRUE)
    s <- gsub("\u00f6", "oe", s, fixed = TRUE)
    s <- gsub("\u00fc", "ue", s, fixed = TRUE)
    s <- gsub("\u00f8", "oe", s, fixed = TRUE)
    gsub("\u00e5", "aa", s, fixed = TRUE)
  }
  letters_umlaut <- \(x) letters_only(umlaut_expand(x))
  # registry encoding corruption in record surnames ("Cola\uFFFDo", "INGULF MEDB??"):
  # turn each corrupted run into a bounded 1-2 letter wildcard anchored to the
  # intact letters (requiring >= 4 intact letters so short names never over-match).
  corrupt_surname_pat <- \(x) {
    if (!grepl("\uFFFD|\\?{2,}", x, perl = TRUE)) return(NA_character_)
    x_mark <- gsub("\uFFFD+|\\?{2,}", "\002", x, perl = TRUE)
    parts <- strsplit(paste0(x_mark, " "), "\002", fixed = TRUE)[[1]]
    pl <- letters_only(parts)
    if (sum(nchar(pl)) < 4L) return(NA_character_)
    paste0("^", paste(pl, collapse = "[a-z]{1,2}"), "$")
  }
  # strip trailing generational suffixes ("Galetti Jr." -> "Galetti")
  strip_gen_suffix <- \(x) sub("\\s+(?:jr|sr|ii|iii|iv)\\.?$", "", trimws(x),
                               ignore.case = TRUE, perl = TRUE)
  # the last part of a surname, after a space, a hyphen or an elided
  # particle: "O'Connell" read from the PDF as "Connell", "Purdie-Vaughns"
  # cited as "Vaughns"
  last_part_raw <- \(x) sub(".*([[:space:]\u00a0\u2010-]|\\b[DdLlOo]['\u2019])", "",
                            strip_gen_suffix(x), perl = TRUE)
  last_part <- \(x) letters_only(last_part_raw(x))
  last_part_u <- \(x) letters_umlaut(last_part_raw(x))
  cited_surnames <- \(o) {
    people <- trimws(strsplit(paste(rejoin_diacritics(o), collapse = ";"), ";", fixed = TRUE)[[1]])
    people <- people[nzchar(people)]
    family <- strip_gen_suffix(trimws(sub(",.*", "", people)))
    has_comma <- grepl(",", people, fixed = TRUE)
    list(name = letters_only(family),
         name_u = letters_umlaut(family),
         last = last_part(family),
         last_u = last_part_u(family),
         one_word = has_comma & !grepl("[[:space:]\u00a0]", family),
         given_first = !has_comma,
         given = ifelse(has_comma, letters_only(sub("^[^,]*,", "", people)), ""),
         # "Alice Brown Jones" ends in "jones", "brownjones", ...
         ends = lapply(family, \(f) {
           w <- letters_only(strsplit(f, "[[:space:]\u00a0]+")[[1]])
           w <- w[nzchar(w)]
           wu <- letters_umlaut(strsplit(f, "[[:space:]\u00a0]+")[[1]])
           wu <- wu[nzchar(wu)]
           unique(c(
             vapply(seq_along(w), \(k) paste(utils::tail(w, k), collapse = ""), ""),
             vapply(seq_along(wu), \(k) paste(utils::tail(wu, k), collapse = ""), "")
           ))
         }))
  }
  # which cited people does record surname x (given name g) match? With
  # `swapped`, a name whose parts are swapped on one side matches too: the
  # record's given name is the cited surname, and the record's surname starts
  # with the cited initial ("Wu, K." for given "Wu", family "Kui"). Such a
  # name is not a mismatch, but it is noted.
  person_matches <- \(x, cs, g = NA, swapped = FALSE) {
    x_clean <- strip_gen_suffix(x)
    full <- letters_only(x_clean)
    full_u <- letters_umlaut(x_clean)
    fulls <- unique(c(full, full_u))
    last <- last_part(x_clean)
    last_u <- last_part_u(x_clean)
    lasts <- unique(c(last, last_u))
    record_one_word <- !grepl("[[:space:]\u00a0]", x_clean)
    # trailing multi-word suffixes of a particle surname ("Van der Veeken" ->
    # "derveeken"), allowing a citation that drops a leading particle
    rec_ends <- if (!record_one_word) {
      rw <- letters_only(strsplit(x_clean, "[[:space:]\u00a0]+")[[1]])
      rw <- rw[nzchar(rw)]
      rwu <- letters_umlaut(strsplit(x_clean, "[[:space:]\u00a0]+")[[1]])
      rwu <- rwu[nzchar(rwu)]
      unique(c(
        if (length(rw) >= 2L) vapply(seq_along(rw), \(k) paste(utils::tail(rw, k), collapse = ""), "") else character(0),
        if (length(rwu) >= 2L) vapply(seq_along(rwu), \(k) paste(utils::tail(rwu, k), collapse = ""), "") else character(0)
      ))
    } else character(0)
    g <- if (is.na(g)) "" else letters_only(g)
    corrupt_pat <- corrupt_surname_pat(x_clean)
    corrupt_hit <- if (!is.na(corrupt_pat)) {
      grepl(corrupt_pat, cs$name, perl = TRUE) | grepl(corrupt_pat, cs$last, perl = TRUE)
    } else rep(FALSE, length(cs$name))
    (cs$name %in% fulls) | (cs$name_u %in% fulls) |
      (cs$one_word & ((cs$name %in% lasts) | (cs$name_u %in% lasts))) |
      (!cs$given_first & nchar(cs$name) >= 4L & ((cs$name %in% rec_ends) | (cs$name_u %in% rec_ends))) |
      (record_one_word & !cs$given_first & ((cs$last %in% fulls) | (cs$last_u %in% fulls))) |
      (cs$given_first & vapply(cs$ends, \(e) any(fulls %in% e), logical(1))) |
      corrupt_hit |
      (swapped & nzchar(g) & nzchar(full) & cs$name == g & nzchar(cs$given) &
         substr(cs$given, 1, 1) == substr(full, 1, 1))
  }
  # is record surname x one or more consecutive words of a printed list?
  # Never join single-letter initials (e.g. "L." + "I.") into a short surname ("Li").
  words_match <- \(x, w) {
    x_clean <- strip_gen_suffix(x)
    fulls <- unique(c(letters_only(x_clean), letters_umlaut(x_clean)))
    lasts <- unique(c(last_part(x_clean), last_part_u(x_clean)))
    wl <- letters_only(w)
    wlu <- letters_umlaut(w)
    join_vec <- \(v) {
      out <- unlist(lapply(1:3, \(k) {
        if (length(v) < k) return(character())
        if (k == 1L) return(v)
        vapply(seq_len(length(v) - k + 1L), \(s) {
          seg <- v[s:(s + k - 1L)]
          if (any(nchar(seg) < 2L)) "" else paste(seg, collapse = "")
        }, "")
      }))
      out[nzchar(out)]
    }
    joined <- unique(c(join_vec(wl), join_vec(wlu)))
    corrupt_pat <- corrupt_surname_pat(x_clean)
    if (!is.na(corrupt_pat) && any(grepl(corrupt_pat, joined, perl = TRUE)))
      return(TRUE)
    any(fulls %in% joined) || any(lasts %in% wl) || any(lasts %in% wlu) ||
      any(fulls %in% last_part(w)) || any(fulls %in% last_part_u(w))
  }
  printed_words <- \(o) {
    w <- strsplit(o, "[[:space:]\u00a0,;&]+")[[1]]
    w[nzchar(letters_only(w))]
  }
  bibr <- table$paper_id %in% .bibr12_paper_ids(paper)
  # A list truncated with "et al." (or "and others", as BibTeX gives it) or
  # shortened with an ellipsis ("\u2026", "...", ". . .", as APA/Wiley do for
  # long lists) is a citation style. The PDF reader may drop "et al." or the
  # ellipsis from the authors, so the author block of the reference text
  # before the title (de-hyphenating line breaks on both sides, with a
  # bounded author-year fallback) is read too.
  etal <- "(?i)\\bet\\.?\\s*al\\b|(^|[;,&]\\s*|\\band\\s+)others\\b"
  ellipsis_pat <- "\u2026|\\.\\s*\\.\\s*\\."
  author_block <- \(text, title) {
    if (is.na(text) || !nzchar(trimws(text))) return("")
    tx <- clean(dehyphen(text))
    if (!is.na(title) && nzchar(trimws(title))) {
      tt <- clean(dehyphen(title))
      at <- regexpr(substr(tt, 1, 20), tx, fixed = TRUE)
      if (at > 0L) return(substr(tx, 1L, at - 1L))
      # fallback when punctuation/spelling differed in the first 20 chars:
      # locate the first 3 words of the title
      tw <- strsplit(trimws(gsub("[^\\p{L}\\p{N}]+", " ", tt, perl = TRUE)), " ")[[1]]
      tw <- tw[nzchar(tw)]
      if (length(tw) >= 3L) {
        pat <- paste(tw[1:3], collapse = "\\W+")
        at2 <- regexpr(pat, tx, perl = TRUE)
        if (at2 > 0L) return(substr(tx, 1L, at2 - 1L))
      }
    }
    # bounded author-date fallback: text before the first "(YYYY)" or ". YYYY."
    # within the leading 350 characters (avoids years inside titles or DOIs)
    head_tx <- substr(tx, 1L, 350L)
    ym <- regexpr("(\\(\\s*(1[89]\\d{2}|20\\d{2})[a-z]?\\s*\\)|\\.\\s+(1[89]\\d{2}|20\\d{2})[a-z]?\\.)",
                  head_tx, perl = TRUE)
    if (ym > 1L) substr(head_tx, 1L, ym - 1L) else ""
  }
  truncated <- \(o, text, title) {
    o_str <- paste(o, collapse = "; ")
    before <- author_block(text, title)
    list(
      etal = grepl(etal, o_str, perl = TRUE) || grepl(etal, before, perl = TRUE),
      ellipsis = grepl(ellipsis_pat, o_str, perl = TRUE) || grepl(ellipsis_pat, before, perl = TRUE),
      before = before
    )
  }
  # for each reference, a note on what disagrees, or NA
  author_check <- \(record, cited, as_printed, text, title) mapply(\(l, o, as_printed, text, title) {
    if (length(l) == 0 || all(is.na(l))) return(NA_character_)  # nothing to check
    # no cited authors were read from the reference: unknown, not a mismatch
    if (length(o) == 0 || all(is.na(o)) || !nzchar(trimws(paste(o, collapse = ""))))
      return(NA_character_)
    g <- attr(l, "given") %||% rep(NA_character_, length(l))
    first <- if (!is.na(l[1]) && nzchar(letters_only(l[1]))) l[1] else NA
    first_g <- g[1]
    keep <- !is.na(l) & nzchar(letters_only(l))
    l_full <- l[keep]
    g_full <- g[keep]
    l <- utils::head(l_full, max_authors)
    g <- utils::head(g_full, max_authors)
    cut <- truncated(o, text, title)
    o_raw <- paste(o, collapse = "; ")
    o <- gsub(etal, "", o_raw, perl = TRUE)
    o_no_ell <- gsub(ellipsis_pat, "", o, perl = TRUE)
    tail_mismatch <- character(0)
    if (as_printed) {
      w <- printed_words(o_no_ell)
      if (cut$ellipsis) {
        if (grepl(ellipsis_pat, o, perl = TRUE)) {
          parts_e <- strsplit(o, ellipsis_pat, perl = TRUE)[[1]]
          pre_o <- parts_e[1]
          post_o <- if (length(parts_e) >= 2L) parts_e[length(parts_e)] else ""
          n_pre <- length(Filter(\(s) nzchar(letters_only(s)),
                                 strsplit(pre_o, "(?<=\\.),\\s*|\\s*&\\s*|\\s+and\\s+|;", perl = TRUE)[[1]]))
          l <- utils::head(l, max(1L, n_pre))
          if (nzchar(letters_only(post_o)) && length(l_full) > length(l)) {
            last_rec <- l_full[length(l_full)]
            if (!words_match(last_rec, printed_words(post_o)))
              tail_mismatch <- sprintf("%s is in the record but not in the reference", last_rec)
          }
        } else {
          l <- utils::head(l, 1L)
        }
      } else if (cut$etal) {
        # check all visibly printed authors before "et al.", not only author 1
        n_vis <- length(Filter(\(s) nzchar(letters_only(s)),
                               strsplit(o_no_ell, "(?<=\\.),\\s*|\\s*&\\s*|\\s+and\\s+|;", perl = TRUE)[[1]]))
        l <- utils::head(l, max(1L, n_vis))
      }
      found <- vapply(l, \(x) words_match(x, w), logical(1))
      first_ok <- is.na(first) || words_match(first, printed_words(sub(",.*", "", o_no_ell)))
    } else {
      cs <- cited_surnames(o_no_ell)
      n_cited <- sum(nzchar(cs$name))
      if (cut$ellipsis) {
        if (grepl(ellipsis_pat, o, perl = TRUE)) {
          parts_e <- strsplit(o, ellipsis_pat, perl = TRUE)[[1]]
          cs_pre <- cited_surnames(parts_e[1])
          n_pre <- max(1L, sum(nzchar(cs_pre$name)))
          l <- utils::head(l, n_pre)
          g <- utils::head(g, n_pre)
          if (length(parts_e) >= 2L && nzchar(letters_only(parts_e[length(parts_e)])) &&
              length(l_full) > n_pre) {
            cs_post <- cited_surnames(parts_e[length(parts_e)])
            last_rec <- l_full[length(l_full)]
            last_g <- g_full[length(g_full)]
            if (!any(person_matches(last_rec, cs_post, last_g, swapped = TRUE)))
              tail_mismatch <- sprintf("%s is in the record but not in the reference", last_rec)
          }
        } else if (n_cited >= 2L && length(l_full) >= n_cited) {
          # GROBID dropped the ellipsis from authors.orig, leaving leading
          # authors + final author: require leading n_cited - 1 authors, and
          # verify the last cited surname matches the record's final author
          # (unless all n_cited already match the leading record authors)
          last_rec <- l_full[length(l_full)]
          last_g <- g_full[length(g_full)]
          lead_ok <- all(vapply(seq_len(min(length(l), n_cited)),
                                \(k) any(person_matches(l[k], cs, g[k], swapped = TRUE)), logical(1)))
          if (!lead_ok) {
            n_pre <- max(1L, n_cited - 1L)
            l <- utils::head(l, n_pre)
            g <- utils::head(g, n_pre)
            if (length(l_full) > n_pre &&
                !any(person_matches(last_rec, cs, last_g, swapped = TRUE)))
              tail_mismatch <- sprintf("%s is in the record but not in the reference", last_rec)
          } else {
            l <- utils::head(l, n_cited)
            g <- utils::head(g, n_cited)
          }
        } else {
          l <- utils::head(l, max(1L, n_cited))
          g <- utils::head(g, max(1L, n_cited))
        }
      } else if (cut$etal) {
        l <- utils::head(l, max(1L, n_cited))
        g <- utils::head(g, max(1L, n_cited))
      }
      found <- vapply(l, \(x) any(person_matches(x, cs)), logical(1))
      swap <- !found & vapply(seq_along(l), \(k)
        any(person_matches(l[k], cs, g[k], swapped = TRUE)), logical(1))
      found <- found | swap
      first_ok <- is.na(first) || !length(cs$name) ||
        person_matches(first, cs, first_g, swapped = TRUE)[1]
      # Rescue GROBID Vancouver/eLife/PLOS author-column shifts (e.g. "Sg, Sumner-Jones",
      # "Kim, Wang W", ", Devin A") and detached PDF spacing diacritics ("I. Jime \u00b4nez"):
      # if a record surname appears intact as whole word(s) in the printed author
      # block before the title AND either appeared inside an entry's given-name
      # slot (without joining across a comma/semicolon boundary) or the raw text
      # had detached spacing diacritics, accept it without relaxing typos,
      # comma-split names ("Eag, Ly"), or swapped author order.
      if ((!all(found) || !first_ok) && nzchar(cut$before)) {
        bw <- printed_words(cut$before)
        people_raw <- trimws(strsplit(rejoin_diacritics(o_no_ell), ";", fixed = TRUE)[[1]])
        given_parts <- ifelse(grepl(",", people_raw, fixed = TRUE),
                              trimws(sub("^[^,]*,", "", people_raw)), "")
        in_given_slot <- \(nm) any(vapply(given_parts[nzchar(given_parts)],
                                          \(gp) words_match(nm, printed_words(gp)), logical(1)))
        has_detached <- !is.na(text) &&
          grepl("[\u00b4\u00a8\u00b8\u02c6\u02dc\u00af\u02da\u02c7\u0300-\u036f]", text, perl = TRUE)
        if (!all(found)) {
          for (k in which(!found)) {
            if (nchar(letters_only(strip_gen_suffix(l[k]))) >= 2L &&
                words_match(l[k], bw) &&
                (in_given_slot(l[k]) || has_detached)) {
              found[k] <- TRUE
            }
          }
        }
        if (!first_ok && !is.na(first)) {
          lead_clause <- sub("^\\s*\\[?\\d+\\]?\\.?\\s+", "", trimws(cut$before))
          lead_clause <- sub("[,;&].*|\\s+and\\s+.*", "", lead_clause, perl = TRUE)
          first_given <- if (length(given_parts)) given_parts[1L] else ""
          first_shifted <- nzchar(first_given) && words_match(first, printed_words(first_given))
          if (words_match(first, printed_words(lead_clause)) &&
              (first_shifted || has_detached)) {
            first_ok <- TRUE
          }
        }
      }
    }
    # each record surname that is missing, with the cited name closest to it
    # (a misspelling: "Sonuga-Barke is cited as Sonuga-Burke")
    cited_words <- unique(strsplit(trimws(o_no_ell), "[;,&[:space:]\u00a0]+")[[1]])
    cited_words <- cited_words[nchar(letters_only(cited_words)) >= 2]
    missing <- vapply(l[!found], \(x) {
      d <- if (length(cited_words)) utils::adist(letters_only(x), letters_only(cited_words))[1, ] /
        pmax(nchar(letters_only(x)), nchar(letters_only(cited_words))) else numeric(0)
      if (length(d) && min(d) <= 1 / 3) sprintf("%s is cited as %s", x, cited_words[which.min(d)])
      else sprintf("%s is in the record but not in the reference", x)
    }, "")
    all_missing <- c(missing, tail_mismatch)
    note <- c(
      if (length(all_missing)) paste(all_missing, collapse = "; "),
      if (!length(all_missing) && !first_ok) sprintf("the record's first author, %s, is not cited first", first))
    if (length(note)) note
    else if (!as_printed && any(swap)) paste0("SWAP:", paste(l[swap], collapse = ", "))
    else NA_character_
  }, record, cited, as_printed, text, title, USE.NAMES = FALSE)
  # a note of swapped names is split from the mismatch notes
  split_swap <- \(note) {
    swap <- startsWith(note, "SWAP:") %in% TRUE
    list(note = ifelse(swap, NA_character_, note),
         swapped = ifelse(swap, sprintf(
           "given and family names are swapped in the record or the citation (family name in the record: %s)",
           sub("SWAP:", "", note, fixed = TRUE)), NA_character_))
  }
  an <- split_swap(author_check(last_names, table$authors.orig, bibr,
                                table$text, table$title.orig))
  table$author_note <- an$note
  table$name_note <- an$swapped
  table$author_mismatch <- !is.na(table$author_note)

  # tier: how did we get the record we are checking against?
  #  "provided"   - the reference printed its own DOI and Crossref returned a
  #                 record for it; we compare the cited details against that
  #                 exact record. Firm.
  #  "unresolved" - the reference printed a DOI, but there is no record for
  #                 it. Flagged as a DOI that could not be found. bib_match
  #                 does not record why a lookup found nothing, so this also
  #                 catches a lookup that failed for network reasons (which
  #                 add_bib_match() only reports as a message at the time) and
  #                 a DOI that is not valid as printed and whose title search
  #                 found nothing. doi.org is then asked whether the DOI
  #                 exists; only a DOI it reports as not existing stays here.
  #  "registered" - an unresolved DOI that doi.org knows but whose record it
  #                 did not return, so it is not checked. (A record doi.org
  #                 returns is compared, as tier "provided".)
  #  "unconfirmed" - an unresolved DOI that doi.org could not be asked about
  #                 (offline, a timeout, a server error): not checked.
  #  "not_checked" - the reference printed a DOI, but its paper has no
  #                 bib_match rows at all: it was not looked up.
  #  "crossref"   - the reference had no DOI and Crossref supplied one via fuzzy
  #                 title/author search; the match may be wrong, so it is only
  #                 compared when its title agrees and the search was confident.
  #  "none"       - no DOI at all.
  #  "search"     - no DOI, and a title search (below) found a record with
  #                 the cited title whose authors or year differ: flagged.
  #  "unreadable" - a reading problem (see read_problem): listed, not compared.
  #  "fragment"   - a DOI read as an entry of its own and given to the
  #                 reference before it; not counted.
  has_own_doi <- !blank(table$doi.orig)
  has_record  <- !blank(table$title.match)
  looked_up   <- as.character(table$paper_id) %in% looked_up_ids
  table$tier <- dplyr::case_when(
    table$read_problem %in% "fragment" ~ "fragment",
    !is.na(table$read_problem) ~ "unreadable",
    has_own_doi & has_record   ~ "provided",
    has_own_doi & looked_up    ~ "unresolved",
    has_own_doi                ~ "not_checked",
    has_record                 ~ "crossref",
    .default                   = "none"
  )
  # an unresolved DOI was checked at doi.org (above): one that does not exist
  # stays flagged; one that is registered but whose record doi.org did not
  # return is listed as not checked, as is one doi.org could not be asked about
  unres <- table$tier == "unresolved"
  table$tier[unres & table$doi_exists %in% TRUE] <- "registered"
  table$tier[unres & is.na(table$doi_exists)] <- "unconfirmed"
  # no_match: the reference could not be matched to a retrieved record (kept for
  # downstream modules such as ref_summary that group on it)
  table$no_match <- !has_record & table$tier != "fragment"

  # incoherence: cited details disagree with the record the reference's OWN DOI
  # points to (tier "provided"), the exact record the author cited. A reference
  # with no DOI is compared with the record a Crossref title search found only
  # when that record would be suggested: a confident match with the same title.
  # A less certain match is often another paper, so a "mismatch" would be
  # Crossref's error, not the citation's. The title is not compared then, as it
  # agrees by construction, and a record with the same title but other authors,
  # journal or year is flagged instead of suggested: a citation error, another
  # version of the work (an abstract, a reprint), or an AI-generated reference
  # that borrows a real title.
  # A DOI, journal or year mismatch flags on its own; title and author
  # mismatches only count when at least `min_mismatches` of them disagree. In
  # the validation, however, the genuine errors were author mismatches, and the
  # journal and year flags came from reading or registry differences.
  # A cited preprint whose DOI leads to the published article is not compared
  # on its year or journal either.
  article_set <- c("article", "journal_article", "inproceedings")
  provided_pub <- table$tier %in% "provided" & cited_preprint &
    table$bib_type.match %in% article_set &
    (table$title_agrees | table$title_starts_same) %in% TRUE
  table$year_mismatch[provided_pub] <- FALSE
  table$container_mismatch[provided_pub] <- FALSE
  table$title_mismatch[provided_pub] <- FALSE
  table$year_note[provided_pub] <- NA_character_
  strong <- (table$doi_mismatch %in% TRUE) |
            (table$year_mismatch %in% TRUE) |
            (table$container_mismatch %in% TRUE)
  n_parsing_mismatch <- (table$title_mismatch %in% TRUE) +
                        (table$author_mismatch %in% TRUE)
  # a reference is incoherent when it printed its own DOI and either that DOI
  # could not be found (tier "unresolved") or the retrieved record disagrees
  # with the cited details (tier "provided"), or when it printed none and the
  # record that would be suggested disagrees (see below)
  table$incoherent <- (table$tier == "unresolved") |
    ((table$tier == "provided") &
       (strong | n_parsing_mismatch >= min_mismatches))

  # summary_table ----
  # references that printed a DOI (tiers "provided" and "unresolved") are
  # checked; references that printed no DOI (tiers "crossref" and "none") are
  # listed so the user can add one. A DOI is suggested when the Crossref
  # title search found a match that scored high enough to be trustworthy (a
  # low score is usually the wrong paper) and whose title agrees with the
  # cited title: a match with another title is another work. bibr 12.x
  # exports score their matches 0-1; a Crossref score that low never passes
  # add_bib_match()'s minimum, so the scale can be read from the score itself.
  table$no_doi <- table$tier %in% c("crossref", "none")
  min_score <- ifelse(table$score %in% NA | table$score > 1,
                      suggest_score, suggest_score / 100)
  table$doi_suggested <- table$tier == "crossref" & !is.na(table$score) &
    table$score >= min_score & !blank(table$doi.match) &
    (table$title_agrees | table$title_starts_same)
  # does Crossref's record have the cited title, whatever its score?
  cr_same_title <- table$tier == "crossref" & !blank(table$doi.match) &
    (table$title_agrees | table$title_starts_same)
  title_differs <- table$title_mismatch
  table$title_mismatch[table$tier == "crossref"] <- FALSE
  # A record with the cited title is compared with the citation when the
  # match is confident, or, below `suggest_score`, when the titles are almost
  # the same (a wrong year or author lowers Crossref's score, so these matches
  # were neither compared nor suggested). One that disagrees is flagged, with
  # the record shown, and not suggested. Unless both are articles, the match
  # is compared only on its authors: books and reports have editions and
  # reprints with the same title in other years and with other series
  # names, and a DOI is not suggested for one whose record has another year.
  # A book whose authors disagree is not flagged on this match: the title
  # search below looks for a work with the title by the cited authors.
  article_pair <- (is.na(table$bib_type.match) | table$bib_type.match %in% article_set) &
    (is.na(table$bib_type.orig) | table$bib_type.orig %in% article_set)
  near_same <- (!is.na(title_charsim) & title_charsim >= 0.9) | tc$same
  # A cited journal article whose same-title match is a preprint, a dataset
  # or another record bib_match calls "misc" is not judged on that match: the
  # title search below looks for a published version among several records.
  misc_match <- cr_same_title & journal_cited &
    table$bib_type.match %in% c("misc", "preprint")
  table$doi_suggested[misc_match] <- FALSE
  compared <- cr_same_title & !misc_match & article_pair & (table$doi_suggested | near_same)
  # a book or report is suggested when it has the cited title and both list
  # authors, which agree; any other is searched
  listed <- vapply(last_names, \(a) any(!is.na(a) & nzchar(a)), logical(1)) &
    !blank(table$authors.orig)
  authors_compared <- cr_same_title & !article_pair & tc$same & listed &
    !table$author_mismatch %in% TRUE
  book_search <- cr_same_title & !article_pair & !authors_compared
  # a cited preprint whose match is a published version: authors only
  preprint_pub <- ((compared | authors_compared) & cited_preprint &
    table$bib_type.match %in% article_set) | provided_pub
  table$year_mismatch[preprint_pub] <- FALSE
  table$container_mismatch[preprint_pub] <- FALSE
  # the record was found by a similar title, so a title that is not the same
  # is a mismatch (a preprint and its published version may differ in title)
  table$title_mismatch[(compared | authors_compared) & !preprint_pub] <-
    title_differs[(compared | authors_compared) & !preprint_pub]
  nodoi_strong <- (table$year_mismatch %in% TRUE) | (table$container_mismatch %in% TRUE)
  nodoi_parsing <- (table$author_mismatch %in% TRUE) + (table$title_mismatch %in% TRUE) >=
    min_mismatches
  table$incoherent <- table$incoherent | (compared & (nodoi_strong | nodoi_parsing))
  other_year <- table$year_mismatch %in% TRUE
  table$year_mismatch[authors_compared] <- FALSE
  table$container_mismatch[authors_compared] <- FALSE
  # a match compared on all its fields is suggested whatever its score when
  # none of them differs: Crossref scores the short references of classic
  # articles low
  table$doi_suggested <- (compared | authors_compared) &
    !table$incoherent & !(!compared & other_year) & !preprint_pub
  pub <- preprint_pub & !table$incoherent
  table$published_doi[pub] <- table$doi.match[pub]
  # a year note stands only for a record that was compared
  note_record[!(table$tier %in% "provided" | compared) | preprint_pub] <- NA

  # A reference without a DOI whose title is not that of Crossref's match is
  # searched by its title alone, in Crossref (whose title search for
  # add_bib_match() also uses the authors and journal, and keeps a match only
  # above a minimum score, which short titles do not reach) and then in
  # DataCite (arXiv, Zenodo, OSF, data repositories), and then in Semantic
  # Scholar (conference papers without DOIs). The titles of the first five
  # records (Semantic Scholar's best match) are compared with the cited
  # title. A reference found in none is listed as not found: a work these
  # sources do not hold (a
  # book, a report, a web page), a title misread from the PDF, or a reference
  # that does not exist. Titles of fewer than three words, and references in
  # papers that were not looked up, are not searched. A reference whose cited
  # DOI does not exist is searched too: found, it stays flagged with the
  # record found; not found, it is listed as not found instead.
  # words as ICU segments them, so that a title in a script written without
  # spaces (Chinese, Japanese) is counted in words, not as one
  n_words <- stringi::stri_count_words(ifelse(is.na(table$title.orig), "", table$title.orig))
  # works the reference itself calls unpublished are not expected in these
  # sources, so they are not searched or listed as not found
  unpublished <- grepl(paste0(
    "(?i)\\b(unpublished|in preparation|manuscript (submitted|under review)|",
    "submitted for publication|under review|personal communication)\\b"),
    paste(table$text, table$container.orig, table$bib_type.orig), perl = TRUE)
  searched <- which(!unpublished & (table$tier %in% c("crossref", "none") & (!cr_same_title | misc_match | book_search) |
                       table$tier %in% "unresolved") &
                      !blank(table$title.orig) & n_words >= 3 & looked_up)
  table$not_found <- FALSE
  table$search_failed <- FALSE
  skipped <- rep(FALSE, nrow(table))
  search_named <- rep(FALSE, nrow(table))
  # For each title q[i], the first record with the same title (a row of
  # hits), NA when there is none, and NA_integer_ - 1 (-1) when the search
  # failed.
  # the types of records that are articles, preprints or conference papers
  record_article_types <- c("journal-article", "proceedings-article", "posted-content",
                            "JournalArticle", "Conference", "Preprint")
  # a preprint record: Crossref's posted content, Semantic Scholar's and
  # DataCite's preprints, and arXiv's DOIs (prefix 10.48550)
  is_preprint_rec <- \(type, doi) type %in% c("posted-content", "Preprint") |
    grepl("^10\\.48550/", doi, ignore.case = TRUE)
  table$record_source <- ifelse(table$tier %in% c("provided", "crossref"),
                                dplyr::coalesce(table$service, "crossref"),
                                NA_character_)
  # the Semantic Scholar page of a record found there
  table$record_url <- NA_character_
  # what the title search found for a reference it does not flag, for the
  # report of references without a DOI
  found <- data.frame(source = rep(NA_character_, nrow(table)), type = NA_character_,
                      doi = NA_character_, url = NA_character_, title = NA_character_)
  source_name <- \(x) dplyr::case_when(
    x %in% "datacite" ~ "DataCite", x %in% "semanticscholar" ~ "Semantic Scholar",
    is.na(x) | x %in% "crossref" ~ "Crossref", .default = x)
  # why a work found by its title has no DOI suggested
  found_why <- rep(NA_character_, nrow(table))
  if (length(searched)) {
    # Deduplicate HTTP queries by unique title `q`, then evaluate and rank the
    # returned candidates independently for each reference `m` in `searched`
    # using that reference's own text, year, authors, venue class and format
    # (so results do not depend on paper order or batch size).
    q_ref <- table$title.orig[searched]
    q <- unique(q_ref)
    qi <- match(q_ref, q)
    n_s <- length(searched)
    s_text <- table$text[searched]
    s_cont <- table$container.orig[searched]
    s_year <- table$year.orig[searched]
    s_auth <- table$authors.orig[searched]
    s_bibr <- bibr[searched]
    s_type <- table$bib_type.orig[searched]
    s_journal <- journal_cited[searched]
    s_cited_book <- !is.na(s_type) & !s_type %in% c(article_set, "preprint")

    cr <- crossref_query(as.list(q), min_score = 0, rows = 5,
                         select = c("DOI", "title", "score", "author", "year", "type",
                                    "container-title"))
    cr <- if (is.null(cr$ref)) {
      # Crossref could not be reached
      data.frame(query = seq_along(q), doi = NA_character_,
                 title = NA_character_, year = NA_integer_,
                 type = NA_character_, container = NA_character_, error = TRUE)
    } else {
      x <- data.frame(query = match(cr$ref, q), doi = cr$DOI %||% NA_character_,
                      title = cr$title %||% NA_character_,
                      year = suppressWarnings(as.integer(cr$year %||% NA)),
                      type = cr$type %||% NA_character_,
                      container = cr$`container-title` %||% NA_character_,
                      error = !is.na(cr$error %||% NA_character_))
      x$authors <- lapply(cr$author %||% vector("list", nrow(x)), \(a)
        if (is.data.frame(a)) structure(a$family, given = if ("given" %in% names(a)) a$given else rep(NA, nrow(a)))
        else NA_character_)
      x
    }
    cr_err <- seq_along(q) %in% unique(cr$query[cr$error])
    dc_err <- rep(FALSE, length(q))
    s2_err <- rep(FALSE, length(q))
    s2_skipped <- rep(FALSE, length(q))

    # every record each search returned, to select per reference and to tell
    # whether a suggested DOI is unique for the cited title and year
    cand <- cr[!cr$error & !blank(cr$title), c("query", "doi", "title", "year", "type", "container")]
    if (nrow(cand)) cand$authors <- cr$authors[!cr$error & !blank(cr$title)] else cand$authors <- list()
    cand$source <- rep("crossref", nrow(cand))
    cand$url <- rep(NA_character_, nrow(cand))

    # "Family, Given; ..." names as surnames with their given names
    split_names <- \(a) {
      p <- trimws(strsplit(a, ";", fixed = TRUE)[[1]])
      structure(trimws(sub(",.*", "", p)),
                given = ifelse(grepl(",", p), trimws(sub("^[^,]*,", "", p)), NA))
    }

    # For every reference m in `searched`, choose its best matching row in
    # `cand` using reference m's own metadata and ONLY from sources that
    # reference m itself was eligible to query (preventing batch-dependent
    # fallback leakage when two papers share a title). For a journal citation,
    # a confirmed published article/conference record beats a preprint even when
    # the preprint has the cited year.
    pub_types <- c("journal-article", "proceedings-article", "JournalArticle", "Conference")
    allow_dc <- rep(FALSE, n_s)
    allow_s2 <- rep(FALSE, n_s)
    ref_s2_skipped <- rep(FALSE, n_s)
    select_per_ref <- \() {
      best <- rep(NA_integer_, n_s)
      book_matched <- rep(FALSE, n_s)
      if (!nrow(cand)) return(list(best = best, book_matched = book_matched))
      by_q <- split(seq_len(nrow(cand)), cand$query)
      q_rows <- by_q[as.character(qi)]
      pair_m <- rep(seq_len(n_s), lengths(q_rows))
      pair_c <- unlist(q_rows, use.names = FALSE)
      if (!length(pair_m)) return(list(best = best, book_matched = book_matched))
      # enforce per-reference source eligibility so fallback queries triggered
      # by another paper never leak into references that did not need fallback
      src_c <- cand$source[pair_c]
      src_ok <- src_c == "crossref" |
        (src_c == "datacite" & allow_dc[pair_m]) |
        (src_c == "semanticscholar" & allow_s2[pair_m])
      pair_m <- pair_m[src_ok]
      pair_c <- pair_c[src_ok]
      if (!length(pair_m)) return(list(best = best, book_matched = book_matched))
      ok <- title_check(q_ref[pair_m], cand$title[pair_c], s_text[pair_m], s_cont[pair_m])
      keep <- (ok$agrees | ok$starts_same) %in% TRUE
      if (!any(keep)) return(list(best = best, book_matched = book_matched))
      km <- pair_m[keep]
      kc <- pair_c[keep]
      ksame <- ok$same[keep] %in% TRUE
      src_rank <- match(cand$source[kc], c("crossref", "datacite", "semanticscholar"))
      pre <- is_preprint_rec(cand$type[kc], cand$doi[kc])
      is_pub <- cand$type[kc] %in% pub_types & !pre
      other_year <- !(cand$year[kc] == s_year[km]) %in% TRUE
      for (m in unique(km)) {
        idx <- which(km == m)
        if (length(idx) == 1L) {
          best[m] <- kc[idx[1L]]
        } else {
          rows_m <- kc[idx]
          # prefer candidates whose authors, journal, and exact title agree with
          # the cited reference before falling back to year/source rank (avoids
          # picking a Yearbook summary or different paper with the same title)
          au_listed <- vapply(cand$authors[rows_m], \(a) any(!is.na(a) & nzchar(a)), logical(1))
          au_ok <- if (!blank(s_auth[m]) && any(au_listed)) {
            notes_m <- split_swap(author_check(
              cand$authors[rows_m], rep(s_auth[m], length(rows_m)),
              rep(s_bibr[m], length(rows_m)), rep(s_text[m], length(rows_m)),
              rep(q_ref[m], length(rows_m))))$note
            au_listed & is.na(notes_m)
          } else rep(FALSE, length(rows_m))
          cont_ok <- if (!blank(s_cont[m])) {
            vapply(cand$container[rows_m], \(rc) {
              !is.na(rc) && nzchar(trimws(rc)) &&
                (journal_coherent(s_cont[m], rc, s_text[m]) || journal_succeeds(s_cont[m], rc))
            }, logical(1))
          } else rep(FALSE, length(rows_m))
          full_t <- title_squash(cand$title[rows_m]) == title_squash(q_ref[m])
          ord <- if (s_journal[m]) {
            order(!au_ok, !is_pub[idx], !cont_ok, !full_t, other_year[idx], src_rank[idx], pre[idx])
          } else {
            order(!au_ok, !cont_ok, !full_t, other_year[idx], src_rank[idx], pre[idx])
          }
          best[m] <- rows_m[ord[1L]]
        }
      }
      # check which cited books already have a same-title candidate by the cited authors
      bk_idx <- which(s_cited_book[km] & ksame)
      if (length(bk_idx)) {
        b_m <- km[bk_idx]
        b_c <- kc[bk_idx]
        b_note <- split_swap(author_check(cand$authors[b_c], s_auth[b_m], s_bibr[b_m],
                                          s_text[b_m], q_ref[b_m]))$note
        b_ok <- vapply(cand$authors[b_c], \(a) any(!is.na(a) & nzchar(a)), logical(1)) &
          !blank(s_auth[b_m]) & is.na(b_note)
        book_matched[unique(b_m[b_ok])] <- TRUE
      }
      list(best = best, book_matched = book_matched)
    }

    need_fallback <- \(sel) {
      has_c <- !is.na(sel$best)
      is_pre <- rep(FALSE, n_s)
      if (any(has_c)) {
        is_pre[has_c] <- is_preprint_rec(cand$type[sel$best[has_c]], cand$doi[sel$best[has_c]])
      }
      !has_c | (s_journal & is_pre) | (s_cited_book & has_c & !sel$book_matched)
    }

    sel <- select_per_ref()
    need_dc <- need_fallback(sel)
    allow_dc <- need_dc
    rest <- sort(unique(qi[need_dc]))
    if (length(rest)) {
      dc <- datacite_query(q[rest])
      for (col in c("doi", "title", "year", "authors", "type"))
        if (is.null(dc[[col]])) dc[[col]] <- rep(NA, nrow(dc))
      dc_err[rest[unique(dc$query[dc$error])]] <- TRUE
      ok <- which(!dc$error & !blank(dc$title))
      if (length(ok)) {
        add <- data.frame(query = rest[dc$query[ok]], doi = dc$doi[ok], title = dc$title[ok],
                          year = suppressWarnings(as.integer(dc$year[ok])), type = dc$type[ok],
                          container = rep(NA_character_, length(ok)))
        add$authors <- lapply(dc$authors[ok], split_names)
        add$source <- rep("datacite", length(ok))
        add$url <- rep(NA_character_, length(ok))
        cand <- rbind(cand, add)
      }
      sel <- select_per_ref()
    }
    # a title found in neither is searched in Semantic Scholar, which holds
    # conference papers without DOIs; its requests are paced. Without it, a
    # title not found so far is not checked (not "not found"), and a preprint
    # found for a journal citation is not flagged
    need_s2 <- need_fallback(sel)
    rest <- sort(unique(qi[need_s2]))
    if (length(rest) && !semantic_scholar) {
      s2_skipped[rest] <- TRUE
      ref_s2_skipped <- need_s2
      rest <- integer(0)
    } else {
      allow_s2 <- need_s2
    }
    if (length(rest)) {
      s2 <- semanticscholar_query(q[rest])
      s2_err[rest[unique(s2$query[s2$error])]] <- TRUE
      ok <- which(!s2$error & !blank(s2$title))
      if (length(ok)) {
        add <- data.frame(query = rest[s2$query[ok]], doi = s2$doi[ok], title = s2$title[ok],
                          year = suppressWarnings(as.integer(s2$year[ok])), type = s2$type[ok],
                          container = s2$venue[ok])
        add$authors <- lapply(s2$authors[ok], split_names)
        add$source <- rep("semanticscholar", length(ok))
        add$url <- s2$url[ok]
        cand <- rbind(cand, add)
      }
      sel <- select_per_ref()
    }

    # build per-reference winning record table
    rec <- data.frame(source = rep(NA_character_, n_s),
                      doi = NA_character_, title = NA_character_, year = NA_integer_,
                      type = NA_character_, container = NA_character_,
                      url = NA_character_)
    rec$authors <- vector("list", n_s)
    got <- which(!is.na(sel$best))
    if (length(got)) {
      cols_c <- c("source", "doi", "title", "year", "type", "container", "url")
      rec[got, cols_c] <- cand[sel$best[got], cols_c]
      rec$authors[got] <- cand$authors[sel$best[got]]
    }

    # a book found with other authors is shown with the record of its title by
    # the cited authors, when one was found, or else with one of its title
    # that lists no authors (of the cited year first)
    bq <- which(s_cited_book & !is.na(rec$source))
    if (length(bq) && nrow(cand)) {
      listed_rec <- \(a) any(!is.na(a) & nzchar(a))
      for (m in bq) {
        cur_note <- split_swap(author_check(rec$authors[m], s_auth[m], s_bibr[m],
                                            s_text[m], q_ref[m]))$note
        if (listed_rec(rec$authors[[m]]) && !blank(s_auth[m]) && is.na(cur_note)) next
        cm <- which(cand$query == qi[m] & (
          cand$source == "crossref" |
            (cand$source == "datacite" & allow_dc[m]) |
            (cand$source == "semanticscholar" & allow_s2[m])
        ))
        if (!length(cm)) next
        is_same <- title_check(rep(q_ref[m], length(cm)), cand$title[cm],
                               rep(s_text[m], length(cm)), rep(s_cont[m], length(cm)))$same %in% TRUE
        cm <- cm[is_same]
        if (!length(cm)) next
        notes_cm <- split_swap(author_check(cand$authors[cm], rep(s_auth[m], length(cm)),
                                            rep(s_bibr[m], length(cm)), rep(s_text[m], length(cm)),
                                            rep(q_ref[m], length(cm))))$note
        has_a <- vapply(cand$authors[cm], listed_rec, logical(1))
        pick_m <- cm[has_a & !blank(s_auth[m]) & is.na(notes_cm)]
        if (!length(pick_m) && listed_rec(rec$authors[[m]])) pick_m <- cm[!has_a]
        if (!length(pick_m)) next
        pick_m <- pick_m[order(!cand$year[pick_m] %in% s_year[m])][1L]
        cols_c <- c("source", "doi", "title", "year", "type", "container", "url")
        rec[m, cols_c] <- cand[pick_m, cols_c]
        rec$authors[m] <- cand$authors[pick_m]
      }
    }

    has_win <- !is.na(rec$source)
    any_err <- cr_err[qi] | (allow_dc & dc_err[qi]) | (allow_s2 & s2_err[qi]) | ref_s2_skipped
    table$not_found[searched[!has_win & !any_err]] <- TRUE
    table$search_failed[searched[!has_win & any_err]] <- TRUE
    skipped[searched[!has_win & ref_s2_skipped]] <- TRUE
    rec$i <- searched
    rec$pre_unsure <- s_journal & has_win & is_preprint_rec(rec$type, rec$doi) &
      ((allow_dc & dc_err[qi]) | (allow_s2 & s2_err[qi]) | ref_s2_skipped)
    found[searched, ] <- rec[, c("source", "type", "doi", "url", "title")]

    # a DOI that does not exist: the work found by its title is shown with it;
    # one not found is not a citation error that can be shown, so it is listed
    # as not found; when the search failed, the DOI stays flagged.
    # If the unresolved DOI equals the found work's real DOI once spurious PDF
    # line-break hyphen(s) are removed (e.g. "10.1152/japplphys-iol.00630.2016"
    # vs "10.1152/japplphysiol.00630.2016"), recover it and compare all other
    # fields (authors, title, year, journal) against that record instead of
    # flagging the PDF hyphenation as a broken DOI.
    unres <- table$tier[searched] %in% "unresolved"
    table$search_failed[searched[unres]] <- FALSE
    table$incoherent[searched[unres & table$not_found[searched]]] <- FALSE
    fu <- which(unres & !is.na(rec$source))
    hyphen_rec <- integer(0)
    if (length(fu)) {
      f <- searched[fu]
      d_orig <- norm_doi(table$doi.orig[f])
      d_rec <- norm_doi(rec$doi[fu])
      is_lb_hyphen <- !is.na(d_orig) & !is.na(d_rec) & nzchar(d_rec) &
        nchar(d_orig) > nchar(d_rec) &
        gsub("-", "", d_orig, fixed = TRUE) == gsub("-", "", d_rec, fixed = TRUE)
      if (any(is_lb_hyphen)) {
        f_h <- f[is_lb_hyphen]
        fu_h <- fu[is_lb_hyphen]
        table$doi.orig[f_h] <- rec$doi[fu_h]
        table$tier[f_h] <- "provided"
        table$no_match[f_h] <- FALSE
        table$doi_mismatch[f_h] <- FALSE
        table$incoherent[f_h] <- FALSE
        table$record_source[f_h] <- rec$source[fu_h]
        table$doi.match[f_h] <- rec$doi[fu_h]
        table$record_url[f_h] <- rec$url[fu_h]
        table$title.match[f_h] <- rec$title[fu_h]
        table$year.match[f_h] <- rec$year[fu_h]
        table$container.match[f_h] <- rec$container[fu_h]
        hyphen_rec <- fu_h
      }
      fu_bad <- fu[!is_lb_hyphen]
      if (length(fu_bad)) {
        f_b <- searched[fu_bad]
        table$doi_mismatch[f_b] <- TRUE
        table$record_source[f_b] <- rec$source[fu_bad]
        table$doi.match[f_b] <- rec$doi[fu_bad]
        table$record_url[f_b] <- rec$url[fu_bad]
        table$title.match[f_b] <- rec$title[fu_bad]
        table$year.match[f_b] <- rec$year[fu_bad]
        table$container.match[f_b] <- rec$container[fu_bad]
      }
    }
    rec_hyphen <- if (length(hyphen_rec)) rec[hyphen_rec, , drop = FALSE] else rec[0, , drop = FALSE]
    rec <- rec[!unres, , drop = FALSE]
    if (nrow(rec_hyphen)) {
      # Compare recovered line-break-hyphen DOIs on authors, title, year and
      # journal so any real discrepancy on those references is still flagged.
      ih <- rec_hyphen$i
      an_h <- split_swap(author_check(rec_hyphen$authors, table$authors.orig[ih], bibr[ih],
                                      table$text[ih], table$title.orig[ih]))
      amis_h <- !is.na(an_h$note)
      tmis_h <- !title_check(table$title.orig[ih], rec_hyphen$title, table$text[ih],
                             table$container.orig[ih])$exact
      ydiff_h <- abs(as.numeric(table$year.orig[ih]) - rec_hyphen$year)
      ymis_h <- !is.na(ydiff_h) & ydiff_h > year_tolerance
      if (any(ymis_h & rec_hyphen$source == "crossref")) {
        ychk_h <- which(ymis_h & rec_hyphen$source == "crossref")
        src_h <- year_source(rec_hyphen$doi[ychk_h], as.integer(table$year.orig[ih[ychk_h]]),
                             rec_hyphen$year[ychk_h])
        ymis_h[ychk_h[!is.na(src_h)]] <- FALSE
      }
      cmis_h <- mapply(\(a, b, text, doi, ct, rt, check) {
        check && !is.na(a) && !is.na(b) && nzchar(trimws(a)) && nzchar(trimws(b)) &&
          !journal_coherent(a, b, text) && !publisher_code_match(a, b, doi) &&
          !journal_succeeds(a, b) && !title_spill_in_container(a, b, ct, rt, text)
      }, table$container.orig[ih], rec_hyphen$container, table$text[ih], rec_hyphen$doi,
      table$title.orig[ih], rec_hyphen$title,
      (rec_hyphen$type %in% "journal-article" |
         (rec_hyphen$source == "semanticscholar" & !rec_hyphen$type %in% "Preprint")) &
        !article_number[ih] & !cited_preprint[ih],
      USE.NAMES = FALSE)
      flag_h <- amis_h | tmis_h | ymis_h | cmis_h
      if (any(flag_h)) {
        fh <- ih[flag_h]
        table$author_note[fh] <- an_h$note[flag_h]
        table$author_mismatch[fh] <- amis_h[flag_h]
        table$title_mismatch[fh] <- tmis_h[flag_h]
        table$year_mismatch[fh] <- ymis_h[flag_h]
        table$container_mismatch[fh] <- cmis_h[flag_h]
        table$incoherent[fh] <- TRUE
      }
    }

    # A record found with the cited title is compared like Crossref's
    # confident match: when its authors, year or (for a Crossref journal
    # article) journal disagree, the reference is flagged with that record. Only an article is compared with an article
    # or a preprint of almost the same title: books, software, reports and
    # standard titles ("Dyadic data analysis") have editions, versions,
    # reprints, chapters and reviews with the same title by other authors
    # or in other years.
    rec <- rec[!is.na(rec$source), ]
    any_rec <- rec
    article_types <- record_article_types
    cited_type <- table$bib_type.orig[rec$i]
    a <- norm_title(table$title.orig[rec$i])
    b <- norm_title(rec$title)
    near_same <- vapply(seq_along(a), \(k)
      1 - utils::adist(a[k], b[k])[1, 1] / max(nchar(a[k]), nchar(b[k]), 1) >= 0.9,
      logical(1)) | title_check(table$title.orig[rec$i], rec$title, table$text[rec$i],
                                table$container.orig[rec$i])$same
    rec <- rec[rec$type %in% article_types & near_same &
                 (is.na(cited_type) | cited_type %in% c("article", "journal_article", "inproceedings")), ]
    if (nrow(rec)) {
      i <- rec$i
      an <- split_swap(author_check(rec$authors, table$authors.orig[i], bibr[i],
                                    table$text[i], table$title.orig[i]))
      note <- an$note
      ydiff <- abs(table$year.orig[i] - rec$year)
      # a preprint and a published version of the work appear in different
      # years, and only the published version has a journal
      rec_preprint <- is_preprint_rec(rec$type, rec$doi)
      # the title, unless one is a preprint and the other its published version
      tmis <- !title_check(table$title.orig[i], rec$title, table$text[i],
                           table$container.orig[i])$exact &
        cited_preprint[i] == rec_preprint
      parsing <- (!is.na(note)) + tmis >= min_mismatches
      amis <- !is.na(note) & parsing
      tmis <- tmis & parsing
      # a cited preprint is not compared on its year with a published
      # version; a cited published version may appear up to a year more
      # than `year_tolerance` after its preprint
      pub_of_preprint <- !cited_preprint[i] & rec_preprint
      later <- table$year.orig[i] - rec$year
      # Semantic Scholar gives the year of a work's earliest version
      early <- pub_of_preprint | rec$source == "semanticscholar"
      ymis <- !is.na(ydiff) & ydiff > year_tolerance &
        !(cited_preprint[i] & !rec_preprint) &
        !(early & later > 0 & later <= year_tolerance + 1)
      ychk <- which(ymis & rec$source == "crossref")
      if (length(ychk)) {
        src <- year_source(rec$doi[ychk], as.integer(table$year.orig[i[ychk]]), rec$year[ychk])
        ymis[ychk[!is.na(src)]] <- FALSE
        soft <- !is.na(src) & src != "published"
        table$year_note[i[ychk[soft]]] <- year_note_text(
          src[soft], attr(src, "year")[soft], rec$year[ychk[soft]])
        note_record[i[ychk[soft]]] <- rec$doi[ychk[soft]]
      }
      # DataCite records have no journal; Semantic Scholar's venue is its
      # full name ("International Conference on Machine Learning"). Apply the
      # same succession and title-spill checks used for DOI-matched records.
      cmis <- mapply(\(a, b, text, doi, ct, rt, check) {
        check && !is.na(a) && !is.na(b) && nzchar(trimws(a)) && nzchar(trimws(b)) &&
          !journal_coherent(a, b, text) && !publisher_code_match(a, b, doi) &&
          !journal_succeeds(a, b) && !title_spill_in_container(a, b, ct, rt, text)
      }, table$container.orig[i], rec$container, table$text[i], rec$doi,
      table$title.orig[i], rec$title,
      (rec$type %in% "journal-article" |
         (rec$source == "semanticscholar" & !rec$type %in% "Preprint")) &
        !article_number[i] & !cited_preprint[i],
      USE.NAMES = FALSE)
      pmis <- pub_of_preprint & journal_cited[i] & !rec$pre_unsure
      # For a no-DOI bare title search, do not flag a candidate when zero
      # corroborating metadata fields agree (authors, year, AND journal all
      # disagree or journal is absent): that is usually a different paper that
      # happens to share a short or generic title (e.g. "Memory and consciousness").
      has_au_info <- !blank(table$authors.orig[i]) &
        vapply(rec$authors, \(a) any(!is.na(a) & nzchar(a)), logical(1))
      has_yr_info <- !is.na(table$year.orig[i]) & !is.na(rec$year)
      has_ct_info <- !blank(table$container.orig[i]) & !blank(rec$container)
      zero_corrob <- !pmis & has_au_info & amis & has_yr_info & ymis & (!has_ct_info | cmis)
      flag <- (amis | tmis | ymis | cmis | pmis) & !zero_corrob
      named <- !is.na(an$swapped)
      table$name_note[i[named]] <- an$swapped[named]
      search_named[i[named]] <- TRUE
      note_record[i[named & is.na(note_record[i])]] <- rec$doi[named & is.na(note_record[i])]
      table$record_url[i[named]] <- rec$url[named]
      # a published version without a DOI (a conference paper) is not noted
      pub <- !flag & cited_preprint[i] & !rec_preprint & !is.na(rec$doi)
      table$published_doi[i[pub]] <- rec$doi[pub]
      if (any(flag)) {
        f <- i[flag]
        table$tier[f] <- "search"
        table$no_match[f] <- FALSE
        table$record_source[f] <- rec$source[flag]
        table$doi.match[f] <- rec$doi[flag]
        table$record_url[f] <- rec$url[flag]
        table$title.match[f] <- rec$title[flag]
        table$year.match[f] <- rec$year[flag]
        table$container.match[f] <- rec$container[flag]
        table$author_note[f] <- note[flag]
        table$author_mismatch[f] <- amis[flag]
        table$title_mismatch[f] <- tmis[flag]
        table$year_mismatch[f] <- ymis[flag]
        table$container_mismatch[f] <- cmis[flag] | pmis[flag]
        table$preprint_only[f] <- pmis[flag]
        table$doi_mismatch[f] <- FALSE
        table$incoherent[f] <- TRUE
      }
    }

    # A DOI found by the title search is suggested for the work with the
    # cited title and year, when the searches returned only one such work
    # with a DOI (a book and a review of it, or two printings, often share a
    # title and year, and then neither is suggested) and its authors are the
    # cited ones. For the others, the report gives the reason.
    s <- any_rec[!table$incoherent[any_rec$i] & is.na(table$published_doi[any_rec$i]), ]
    if (nrow(s)) {
      i <- s$i
      s_pos <- match(i, searched)
      k <- qi[s_pos]
      s_is_book <- s_cited_book[s_pos]
      cc <- cand[!blank(cand$doi) & !is.na(cand$year), ]
      # per reference, the one candidate with the cited title and year, or
      # minus the number of candidates
      pick <- vapply(seq_along(i), \(j) {
        m <- which(cc$query == k[j] & cc$year %in% table$year.orig[i[j]] & (
          cc$source == "crossref" |
            (cc$source == "datacite" & allow_dc[s_pos[j]]) |
            (cc$source == "semanticscholar" & allow_s2[s_pos[j]])
        ))
        if (!length(m)) return(NA_integer_)
        same_m <- title_check(rep(table$title.orig[i[j]], length(m)), cc$title[m],
                              rep(table$text[i[j]], length(m)),
                              rep(table$container.orig[i[j]], length(m)))$same %in% TRUE
        m <- m[same_m]
        m <- m[!duplicated(tolower(doi_clean(cc$doi[m])))]
        if (length(m) == 1) m else if (length(m)) -length(m) else NA_integer_
      }, integer(1))
      c1 <- cc[ifelse(pick %in% seq_len(nrow(cc)), pick, NA_integer_), ]
      # a publisher's record of a book often lists no authors, so for a
      # reference cited as a book the cited authors suffice when the record
      # names none; a record that names other authors is not suggested
      has_authors <- (vapply(c1$authors, \(a) any(!is.na(a)), logical(1)) | s_is_book) &
        !blank(table$authors.orig[i])
      note <- split_swap(author_check(c1$authors, table$authors.orig[i], bibr[i],
                                      table$text[i], table$title.orig[i]))$note
      # only the record that was compared with the citation (above) is suggested
      one <- pick %in% seq_len(nrow(cc)) &
        tolower(doi_clean(c1$doi)) %in% tolower(doi_clean(s$doi)) &
        tolower(doi_clean(c1$doi)) == tolower(doi_clean(dplyr::coalesce(s$doi, "")))
      # a preprint is not suggested for a reference cited in a journal
      pre <- one & is_preprint_rec(c1$type, c1$doi) & journal_cited[i]
      # A reference cited as a book whose record is a journal article may
      # have found a review of the book, which carries its title and, in
      # Semantic Scholar, often its authors: the article is suggested only
      # when the printed reference names its journal. Crossref is asked for
      # the type and journal of a DOI found elsewhere.
      review <- rep(FALSE, length(i))
      chk <- which(one & has_authors & is.na(note) & !pre & s_is_book & !blank(c1$doi))
      if (length(chk)) {
        typ <- c1$type[chk]
        jn <- c1$container[chk]
        ask <- which(c1$source[chk] != "crossref")
        if (length(ask)) {
          cr_rec <- tryCatch(suppressMessages(crossref_doi(c1$doi[chk[ask]],
                               select = c("DOI", "type", "container-title"))),
                             error = \(e) NULL)
          if (!is.null(cr_rec) && nrow(cr_rec) == length(ask)) {
            typ[ask] <- dplyr::coalesce(cr_rec$type %||% NA_character_, typ[ask])
            jn[ask] <- dplyr::coalesce(cr_rec$`container-title` %||% NA_character_, jn[ask])
          }
        }
        # the journal's words in the printed reference outside the title
        # ("Language" in "... early language comprehension" names no
        # journal), abbreviated or split at a line break as in the journal
        # comparison. The record's title is taken out when it is in the
        # printed reference, else the cited title, which may hold the journal
        # when the reference was misread as a book.
        join <- \(x) gsub("(?<=[a-z])- (?=[a-z])", "", x, perl = TRUE)
        named <- !blank(jn) & mapply(\(j, text, titles) {
          if (is.na(text)) return(FALSE)
          tb <- journal_tokens(j)
          tt <- journal_tokens(join(text))
          for (title in titles[!is.na(titles)]) {
            ti <- journal_tokens(join(title))
            if (!length(ti) || length(tt) < length(ti)) next
            pos <- which(vapply(seq_len(length(tt) - length(ti) + 1L),
                                \(k) identical(as.character(tt[k + seq_along(ti) - 1L]), as.character(ti)), logical(1)))
            if (length(pos)) {
              mask <- pos[1] + seq_along(ti) - 1L
              tt[mask] <- "\001"
              if (!is.null(attr(tt, "raw"))) attr(tt, "raw")[mask] <- "\001"
              break
            }
          }
          length(tb) > 0 && length(tt) >= length(tb) &&
            any(vapply(seq_len(length(tt) - length(tb) + 1L), \(k) tokens_at(tb, tt, k), logical(1)))
        }, jn, table$text[i[chk]], Map(c, c1$title[chk], table$title.orig[i[chk]]))
        # a preprint server registered as a journal ("SSRN Electronic Journal")
        server <- grepl(preprint_words, jn, perl = TRUE)
        review[chk] <- typ %in% c("journal-article", "JournalArticle") & !named & !server
        c1$container[chk] <- ifelse(blank(c1$container[chk]), jn, c1$container[chk])
      }
      ok <- one & has_authors & is.na(note) & !pre & !review
      where <- source_name(ifelse(one, c1$source, s$source))
      found_why[i] <- dplyr::case_when(
        ok ~ NA_character_,
        (pick < 0) %in% TRUE ~ sprintf(
          "The searches found %d records with this title and year (such as a book and a review of it), so it is not clear which is the cited work.", -pick),
        pre ~ sprintf("Cited in a journal; the closest record in %s is a preprint.", where),
        review ~ sprintf("Cited as a book; the closest record in %s is an article in %s, which the reference does not name: it may be a review of the book.",
                         where, ifelse(blank(c1$container), "a journal", c1$container)),
        one & !has_authors ~ sprintf(
          "The closest record in %s may be the cited work, but it or the reference lists no authors to confirm it.", where),
        one ~ sprintf("The closest record in %s has other authors.", where),
        blank(s$doi) ~ sprintf("The closest record in %s has no DOI.", where),
        !title_check(table$title.orig[i], s$title, table$text[i], table$container.orig[i])$same %in% TRUE ~
          sprintf("The closest record in %s has a slightly different title.", where),
        !is.na(s$year) & !is.na(table$year.orig[i]) ~ sprintf(
          "The closest record in %s has another year (%s): it may be another edition or version.", where, s$year),
        .default = sprintf(
          "The closest record in %s may be the cited work, but it or the reference gives no year to confirm it.", where))
      if (any(ok)) {
        f <- i[ok]
        c1 <- c1[ok, ]
        table$doi_suggested[f] <- TRUE
        table$record_source[f] <- c1$source
        table$doi.match[f] <- c1$doi
        table$record_url[f] <- c1$url
        table$title.match[f] <- c1$title
        table$year.match[f] <- c1$year
        table$container.match[f] <- c1$container
        found[f, c("source", "type", "doi", "url", "title")] <-
          c1[, c("source", "type", "doi", "url", "title")]
      }
    }
  }
  # the field mismatches are those of flagged references, so that ref_summary
  # reports the same references as this module
  for (col in grep("_mismatch$", names(table), value = TRUE)) {
    table[[col]] <- table[[col]] %in% TRUE & table$incoherent &
      table$tier %in% c("provided", "crossref", "search")
  }
  table$author_note[!table$author_mismatch] <- NA_character_
  table$year_note[is.na(note_record)] <- NA_character_
  table$name_note[!(table$tier %in% "provided" | compared | authors_compared |
                      search_named)] <- NA_character_
  summary_table <- dplyr::summarise(table,
    .by = paper_id,
    refs_checked  = sum(tier %in% c("provided", "unresolved"), na.rm = TRUE),
    incoherent    = sum(incoherent, na.rm = TRUE),
    no_doi        = sum(no_doi, na.rm = TRUE),
    doi_suggested = sum(doi_suggested, na.rm = TRUE),
    not_found     = sum(not_found, na.rm = TRUE),
    unreadable    = sum(tier == "unreadable", na.rm = TRUE)
  )

  # summary_text
  n_checked  <- sum(summary_table$refs_checked)
  n_inc      <- sum(table$incoherent & !table$tier %in% c("crossref", "search"))
  n_inc_nodoi <- sum(table$incoherent & table$tier %in% c("crossref", "search"))
  n_nodoi    <- sum(summary_table$no_doi)
  n_suggest  <- sum(summary_table$doi_suggested)
  n_unread   <- sum(summary_table$unreadable)
  n_unlooked <- sum(table$tier == "not_checked")
  n_reg      <- sum(table$tier == "registered")
  n_unconf   <- sum(table$tier == "unconfirmed")
  n_nf       <- sum(table$not_found)
  n_skip     <- sum(skipped & table$search_failed)
  n_sfail    <- sum(table$search_failed) - n_skip
  n_pub      <- sum(!is.na(table$published_doi))
  n_ynote    <- sum(!is.na(table$year_note))
  n_nnote    <- sum(!is.na(table$name_note))
  n_refs     <- nrow(table)
  n_diff     <- n_inc + n_inc_nodoi
  n_doi      <- n_checked + n_reg + n_unconf + n_unlooked
  summary_text <- paste0(
    if (!any(looked_up))
      "The references were not looked up at Crossref, so their accuracy was not checked."
    else paste0(
      sprintf("Of %d reference%s, ", n_refs, plural(n_refs)),
      if (n_diff > 0)
        sprintf("%d differ%s from %s record (possible citation error%s)",
                n_diff, if (n_diff == 1) "s" else "", if (n_diff == 1) "its" else "their",
                if (n_diff == 1) "" else "s")
      else "none differs from its record",
      if (n_nf > 0)
        sprintf(" and %d %s not found in Crossref, DataCite or Semantic Scholar (check that %s)",
                n_nf, if (n_nf == 1) "was" else "were",
                if (n_nf == 1) "it exists" else "they exist")
      else "",
      "."),
    sprintf(" %d include%s a DOI%s.", n_doi, if (n_doi == 1) "s" else "",
      if (n_nodoi > 0) sprintf(" and %d %s not%s", n_nodoi, if (n_nodoi == 1) "does" else "do",
        if (n_suggest > 0) sprintf("; the report suggests a DOI for %d of these", n_suggest) else "")
      else ""),
    if (n_sfail > 0) sprintf(" The title search failed for %d reference%s, so %s not checked; run the check again later.",
                             n_sfail, plural(n_sfail), if (n_sfail == 1) "it was" else "they were") else "",
    if (n_skip > 0) sprintf(" %d reference%s not found in Crossref or DataCite %s not checked, because Semantic Scholar was not searched.",
                            n_skip, plural(n_skip), if (n_skip == 1) "was" else "were") else "",
    if (n_pub > 0) sprintf(" %d cited preprint%s %s a published version.",
                           n_pub, plural(n_pub), if (n_pub == 1) "has" else "have") else "",
    if (n_ynote > 0) sprintf(" %d cited year%s %s near the year of the DOI's creation or the work's acceptance, not its publication.",
                             n_ynote, plural(n_ynote), if (n_ynote == 1) "is" else "are") else "",
    if (n_nnote > 0) sprintf(" In %d reference%s, an author's given and family names are swapped, in the reference or in the record.",
                             n_nnote, plural(n_nnote)) else "",
    if (n_unread > 0) sprintf(" %d entr%s could not be read reliably from the PDF and %s listed in the report.",
                              n_unread, if (n_unread == 1) "y" else "ies",
                              if (n_unread == 1) "is" else "are") else "",
    if (n_reg > 0)
      sprintf(" %d cited DOI%s %s registered, but no record was retrieved for %s, so %s not checked.",
              n_reg, plural(n_reg), if (n_reg == 1) "is" else "are",
              if (n_reg == 1) "it" else "them",
              if (n_reg == 1) "the reference was" else "the references were") else "",
    if (n_unconf > 0)
      sprintf(" %d cited DOI%s had no record, and doi.org did not say whether %s, so %s not checked.",
              n_unconf, plural(n_unconf),
              if (n_unconf == 1) "it exists" else "they exist",
              if (n_unconf == 1) "the reference was" else "the references were") else "",
    if (n_unlooked > 0 && any(looked_up))
      sprintf(" %d reference%s with a DOI %s not checked, because %s paper was not looked up at Crossref.",
              n_unlooked, plural(n_unlooked),
              if (n_unlooked == 1) "was" else "were",
              if (n_unlooked == 1) "its" else "their") else ""
  )

  # traffic_light ----
  # Only differences from a record make the light yellow. It is green when a
  # reference was compared with a record, and "na" when none was. References
  # not found and reading problems are listed but do not change the light.
  any_compared <- n_checked > 0 || any(compared | authors_compared) ||
    any(table$doi_suggested)
  tl <- if (any(table$incoherent %in% TRUE)) "yellow" else if (!any_compared) "na" else "green"

  guidance <- "Check each possible citation error against the original source before changing the reference. A difference may come from a citation mistake, an error in reading the PDF, an error in the registry's record, a search that found another work, or an AI-generated reference."

  if (n_inc + n_inc_nodoi == 0) guidance <- ""

  # report ----

  # text for the report: HTML special characters escaped (a SICI DOI such as
  # "...<183::AID-BDM318>..." would be read as a tag), white space collapsed
  # (Crossref titles can hold line breaks)
  esc <- \(x) {
    # formatting tags (raw or entity-escaped, e.g. "&lt;em&gt;") in registry
    # titles are dropped, so titles look alike whatever their source
    x <- strip_markup(decode_entities(as.character(x)))
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", gsub(">", "&gt;", x, fixed = TRUE), fixed = TRUE)
    trimws(gsub("\\s+", " ", x))
  }
  # the reference as printed; a reference without text (a bibr 12.x export
  # can have none) is shown by its parsed details
  ref_text <- \(r) ifelse(is.na(r$text),
    paste0(r$authors.orig, " (", r$year.orig, "). ", r$title.orig, ". ", r$container.orig),
    r$text)

  # for one reference, build the "what is wrong" cell: each mismatching field is
  # shown as a difference between the printed reference and the record: text
  # only in the printed reference struck through in red, text only in the
  # record underlined in green, and the words they share plain. A word
  # changed by a few letters is shown letter by letter ("Neuropsychopha[r]macology").
  strike <- \(x) if (nzchar(x)) paste0(
    "<del style=\"color:#c00\">", esc(x), "</del>") else ""
  added <- \(x) if (nzchar(x)) paste0(
    "<ins style=\"color:#1a7f37\">", esc(x), "</ins>") else ""
  letter_diff <- \(a, b) {
    # dashes and hyphens of every kind are one character
    a <- gsub("\\p{Pd}", "-", a, perl = TRUE)
    b <- gsub("\\p{Pd}", "-", b, perl = TRUE)
    tr <- strsplit(attr(utils::adist(a, b, counts = TRUE), "trafos")[1, 1], "")[[1]]
    a <- strsplit(a, "")[[1]]
    b <- strsplit(b, "")[[1]]
    out <- character()
    same <- d <- ins <- ""
    flush <- \() {
      out <<- c(out, esc(same), strike(d), added(ins))
      same <<- d <<- ins <<- ""
    }
    i <- j <- 1
    for (t in tr) {
      if (t == "M") {
        if (nzchar(d) || nzchar(ins)) flush()
        same <- paste0(same, a[i]); i <- i + 1; j <- j + 1
      } else {
        if (nzchar(same)) { out <- c(out, esc(same)); same <- "" }
        if (t %in% c("S", "D")) { d <- paste0(d, a[i]); i <- i + 1 }
        if (t %in% c("S", "I")) { ins <- paste0(ins, b[j]); j <- j + 1 }
      }
    }
    flush()
    paste(out, collapse = "")
  }
  word_diff <- \(a, b) {
    ta <- strsplit(trimws(a), "\\s+")[[1]]
    tb <- strsplit(trimws(b), "\\s+")[[1]]
    key <- \(w) uk_us(gsub("[^[:alnum:]]", "", deaccent(tolower(w))))
    ka <- key(ta)
    kb <- key(tb)
    # longest common subsequence of the words
    L <- matrix(0L, length(ta) + 1, length(tb) + 1)
    for (i in rev(seq_along(ta))) for (j in rev(seq_along(tb)))
      L[i, j] <- if (ka[i] == kb[j]) L[i + 1, j + 1] + 1L else max(L[i + 1, j], L[i, j + 1])
    out <- character()
    da <- db <- character()
    flush <- \() {
      if (length(da) == 1 && length(db) == 1 &&
          utils::adist(da, db) / max(nchar(da), nchar(db)) <= 0.4) {
        out <<- c(out, letter_diff(da, db))
      } else {
        out <<- c(out, paste(c(if (length(da)) strike(paste(da, collapse = " ")),
                               if (length(db)) added(paste(db, collapse = " "))),
                             collapse = " "))
      }
      da <<- db <<- character()
    }
    i <- j <- 1
    while (i <= length(ta) || j <= length(tb)) {
      if (i <= length(ta) && j <= length(tb) && ka[i] == kb[j]) {
        if (length(da) || length(db)) flush()
        out <- c(out, esc(ta[i])); i <- i + 1; j <- j + 1
      } else if (j > length(tb) || (i <= length(ta) && L[i + 1, j] >= L[i, j + 1])) {
        da <- c(da, ta[i]); i <- i + 1
      } else {
        db <- c(db, tb[j]); j <- j + 1
      }
    }
    if (length(da) || length(db)) flush()
    paste(out[nzchar(out)], collapse = " ")
  }
  correction <- function(label, cited, record, words = TRUE) {
    cited <- if (is.na(cited)) "" else gsub("\\s+", " ", trimws(as.character(cited)))
    record <- gsub("\\s+", " ", trimws(strip_markup(decode_entities(as.character(record)))))
    if (!nzchar(cited)) return(sprintf("<b>%s:</b> (not printed) %s", label, added(record)))
    sprintf("<b>%s:</b> %s", label,
            if (words) word_diff(cited, record) else letter_diff(cited, record))
  }
  # "Sonuga-Barke is cited as Sonuga-Burke" as a difference; other notes as text
  author_diff <- \(note) paste(vapply(strsplit(note, "; ", fixed = TRUE)[[1]], \(n) {
    m <- regmatches(n, regexec("^(.*) is cited as (.*)$", n))[[1]]
    if (length(m) == 3) letter_diff(m[3], m[2]) else esc(n)
  }, ""), collapse = "; ")
  discrepancies <- function(r) {
    # an unresolved cited DOI has no record to compare against
    if (r$tier == "unresolved") {
      gone <- if (isFALSE(r$doi_exists)) "is not registered at doi.org" else "could not be found"
      if (!is.na(r$doi.match) && nzchar(r$doi.match))
        return(sprintf("%s<br>The cited DOI %s; the DOI shown is that of the work found by the cited title in %s.",
                       correction("DOI", r$doi.orig, r$doi.match, words = FALSE), gone,
                       source_name(r$record_source)))
      return(sprintf(
        "<b>DOI:</b> the cited DOI %s %s%s", strike(r$doi.orig), gone,
        if (!is.na(r$record_source))
          sprintf("; the work found by the cited title in %s is shown", source_name(r$record_source))
        else ""))
    }
    parts <- c(
      if (r$tier %in% c("crossref", "search"))
        sprintf("No DOI cited. The work found by its title in %s differs:",
                source_name(r$record_source)),
      if (isTRUE(r$title_mismatch))
        correction("title", r$title.orig, r$title.match),
      # the record surnames that are missing, rather than the whole list
      if (isTRUE(r$author_mismatch))
        sprintf("<b>authors:</b> %s", author_diff(r$author_note)),
      if (isTRUE(r$preprint_only))
        sprintf("<b>journal:</b> cited in %s, but only a preprint was found",
                esc(r$container.orig))
      else if (isTRUE(r$container_mismatch))
        correction("journal", r$container.orig, r$container.match),
      if (isTRUE(r$year_mismatch))
        sprintf("<b>year:</b> %s %s", strike(as.character(r$year.orig)),
                added(as.character(r$year.match))),
      if (isTRUE(r$doi_mismatch))
        correction("DOI", r$doi.orig, r$doi.match, words = FALSE),
      if (identical(r$tier, "provided") &&
          (is_preprint_rec(r$bib_type.match, r$doi.match) ||
             grepl("^10\\.(1101/\\d|31234/|31235/|31219/|48550/|21203/|20944/)",
                   doi_clean(r$doi.match %||% ""), ignore.case = TRUE)))
        "<i>Note: this is a preprint; its registry record may have been revised since it was cited.</i>"
    )
    paste(parts, collapse = "<br>")
  }

  # a record's DOI, or its Semantic Scholar page when it has none; encode each
  # DOI path segment with reserved = TRUE (encoding #, ?, <, >, [, ], quotes)
  # while keeping literal "/" separators cleanly intact
  doi_href <- \(doi) {
    cl <- doi_clean(doi)
    vapply(cl, \(x) {
      if (is.na(x) || !nzchar(x)) return(NA_character_)
      parts <- strsplit(x, "/", fixed = TRUE)[[1]]
      enc <- paste(vapply(parts, \(p) utils::URLencode(p, reserved = TRUE), character(1)),
                   collapse = "/")
      gsub("\"", "%22", gsub("'", "%27", paste0("https://doi.org/", enc), fixed = TRUE), fixed = TRUE)
    }, character(1), USE.NAMES = FALSE)
  }
  safe_url <- \(u) vapply(as.character(u), \(x) {
    if (is.na(x) || !grepl("^https?://", x, ignore.case = TRUE)) return(NA_character_)
    x <- gsub("'", "%27", gsub("\"", "%22", x, fixed = TRUE), fixed = TRUE)
    gsub("<", "%3C", gsub(">", "%3E", x, fixed = TRUE), fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
  record_link <- \(doi, url) {
    su <- safe_url(url)
    ifelse(
      !is.na(doi) & nzchar(doi),
      link(doi_href(doi), esc(doi)),
      ifelse(is.na(su), "", link(su, "Semantic Scholar")))
  }

  ## incoherent references ----
  inc_rows <- table[table$incoherent %in% TRUE, ]
  incoherent_report <- NULL
  if (nrow(inc_rows) > 0) {
    # link to the retrieved record; for an unresolved DOI there is no record, so
    # show the cited (non-resolving) DOI instead
    rec_doi <- ifelse(is.na(inc_rows$doi.match) | !nzchar(inc_rows$doi.match),
                      inc_rows$doi.orig, inc_rows$doi.match)
    out <- data.frame(
      `Reference as printed` = esc(ref_text(inc_rows)),
      Difference = vapply(seq_len(nrow(inc_rows)), \(i) discrepancies(inc_rows[i, ]),
                          character(1)),
      # a cited DOI written as a URL is cleaned for the link target only
      Record = record_link(rec_doi, inc_rows$record_url),
      check.names = FALSE
    )
    names(out)[2] <- "Printed \u2192 record"
    incoherent_report <- scroll_table(out, 5, colwidths = c(.45, .4, .15))
  }

  ## cited DOIs that are aliases of the record's DOI ----
  alias_rows <- table[table$doi_alias %in% TRUE, ]
  alias_report <- NULL
  if (nrow(alias_rows) > 0) {
    out <- data.frame(
      Reference = esc(ref_text(alias_rows)),
      `Cited DOI` = esc(alias_rows$doi.orig),
      `Current DOI` = link(doi_href(alias_rows$doi.match),
                           esc(alias_rows$doi.match)),
      check.names = FALSE
    )
    alias_report <- scroll_table(out, 5, colwidths = c(.6, .2, .2))
  }

  ## registered DOIs without a record ----
  reg_rows <- table[table$tier %in% c("registered", "unconfirmed"), ]
  reg_report <- NULL
  if (nrow(reg_rows) > 0) {
    out <- data.frame(
      Reference = esc(ref_text(reg_rows)),
      DOI = link(doi_href(reg_rows$doi.orig),
                 esc(reg_rows$doi.orig)),
      `Registered with` = ifelse(reg_rows$tier == "unconfirmed",
                                 "unknown (no answer from doi.org)",
                                 reg_rows$doi_ra),
      check.names = FALSE
    )
    reg_report <- scroll_table(out, 5, colwidths = c(.6, .25, .15))
  }

  ## entries that could not be read reliably ----
  unread_rows <- table[table$tier == "unreadable", ]
  unread_report <- NULL
  if (nrow(unread_rows) > 0) {
    out <- data.frame(
      Entry = esc(ref_text(unread_rows)),
      Problem = esc(unread_rows$read_problem),
      check.names = FALSE
    )
    unread_report <- scroll_table(out, 5, colwidths = c(.6, .4))
  }

  ## references found in no registry ----
  nf_rows <- table[table$not_found, ]
  nf_report <- NULL
  if (nrow(nf_rows) > 0) {
    out <- data.frame(Reference = paste0(esc(ref_text(nf_rows)), ifelse(
      nf_rows$tier %in% "unresolved", "<br><i>The cited DOI is not registered at doi.org.</i>", "")),
      check.names = FALSE)
    nf_report <- scroll_table(out, 5)
  }

  ## preprints with a published version, years of another date, swapped names ----
  note_rows <- which(!is.na(table$published_doi) | !is.na(table$year_note) |
                       !is.na(table$name_note))
  notes_report <- NULL
  if (length(note_rows)) {
    r <- table[note_rows, ]
    rec_doi <- dplyr::coalesce(r$published_doi, note_record[note_rows], r$doi.match)
    notes <- vapply(seq_len(nrow(r)), \(j) paste(c(
      if (!is.na(r$published_doi[j])) "Cites a preprint; a published version was found.",
      if (!is.na(r$year_note[j])) paste0("Year: ", esc(r$year_note[j]), "."),
      if (!is.na(r$name_note[j])) paste0("Authors: ", esc(r$name_note[j]), ".")),
      collapse = "<br>"), "")
    out <- data.frame(
      Reference = esc(ref_text(r)),
      Note = notes,
      Record = record_link(rec_doi, r$record_url),
      check.names = FALSE
    )
    notes_report <- scroll_table(out, 5, colwidths = c(.5, .3, .2))
  }

  ## references without a DOI: suggested DOIs, and the others with the reason ----
  # (those flagged above are listed there, those not found and preprints with
  # a published version in their own sections)
  nodoi <- which(table$no_doi %in% TRUE & !table$incoherent & !table$not_found &
                   is.na(table$published_doi))
  sugg <- nodoi[table$doi_suggested[nodoi]]
  rest <- setdiff(nodoi, sugg)
  suggested_report <- other_report <- NULL
  if (length(sugg)) {
    r <- table[sugg, ]
    # the record a suggested DOI points to, so that it can be compared with
    # the citation before it is added
    record <- sprintf("%s (%s). <i>%s</i>", esc(r$title.match), r$year.match,
                      esc(r$container.match))
    record <- gsub(" (NA)", "", gsub(" <i>NA</i>", "", record, fixed = TRUE), fixed = TRUE)
    by_search <- !is.na(found$source[sugg])
    record <- paste0(record, ifelse(by_search,
      sprintf("<br>Found by its title in %s.", source_name(found$source[sugg])), ""))
    out <- data.frame(
      `Reference as printed` = esc(ref_text(r)),
      `Suggested DOI` = paste0(link(doi_href(r$doi.match), esc(r$doi.match)),
                               "<br>", record),
      check.names = FALSE)
    suggested_report <- scroll_table(out, 5, colwidths = c(.6, .4))
  }
  if (length(rest)) {
    r <- table[rest, ]
    f <- found[rest, ]
    n_w <- n_words[rest]
    # Crossref's closest match has the cited title, but did not match the
    # rest of the reference well enough to be suggested
    weak <- is.na(f$source) & r$tier %in% "crossref" & !blank(r$doi.match) &
      (r$title_agrees | r$title_starts_same) %in% TRUE
    why <- dplyr::case_when(
      !looked_up[rest] ~ "Not searched: this paper's references were not looked up in Crossref.",
      unpublished[rest] ~ "Cited as unpublished: not searched.",
      blank(r$title.orig) | n_w < 3 ~ "Not searched: no title was read from the PDF, or it has fewer than three words.",
      skipped[rest] ~ "Not found in Crossref or DataCite; Semantic Scholar was not searched (`semantic_scholar = FALSE`).",
      r$search_failed ~ "The title search failed: run the module again.",
      !is.na(found_why[rest]) ~ found_why[rest],
      weak & (r$year.match != r$year.orig) %in% TRUE ~ sprintf(
        "Crossref's closest record has the cited title but another year (%s): it may be another edition or version.",
        r$year.match),
      weak ~ "Crossref's closest record has the cited title but is a book, chapter or report, whose editions share titles, and it is not clear that it is the cited one.",
      .default = "No record found that could be checked against the reference.")
    rec_cell <- ifelse(!is.na(f$source), record_link(f$doi, f$url),
                       ifelse(weak, paste(record_link(r$doi.match, NA), "(?)"), ""))
    out <- data.frame(
      `Reference as printed` = esc(ref_text(r)),
      `Why not suggested` = why,
      `Closest record` = rec_cell,
      check.names = FALSE)
    other_report <- scroll_table(out, 5, colwidths = c(.55, .3, .15))
  }

  report <- c(
    guidance,
    if (!is.null(incoherent_report))
      c("### Possible citation errors\n",
        "The details of these references differ from the record of the work. For a reference with a DOI, the record is that of its DOI (from Crossref or doi.org). For a reference without a DOI, or with a DOI that is not registered, it is the work found by the cited title in Crossref, DataCite or Semantic Scholar, which may be another work or version: its DOI is linked but not suggested. The differences show text that is only in the printed reference <del style=\"color:#c00\">struck through in red</del>, and text that is only in the record <ins style=\"color:#1a7f37\">underlined in green</ins>.\n",
        incoherent_report),
    if (!is.null(notes_report))
      c("### Preprints, years and names to check\n",
        "These notes are not citation errors, and a reference with a note may also be listed above. Each note is one of three kinds: a cited preprint that has a published version, which may be the better one to cite; a cited year that is near the year the DOI was created or the work accepted, not the year it was published (check which year to cite); or an author whose given and family names are swapped in the reference or in the record (check which order is right).\n",
        notes_report),
    if (!is.null(alias_report))
      c("### DOIs with another registered form\n",
        "The registry lists the cited DOI as an alias of another DOI for the same work, for example after the journal moved to a new publisher. The cited DOI still leads to the work, so this is not an error; the DOI the registry gives is shown, and may be cited instead.\n",
        alias_report),
    if (!is.null(reg_report))
      c("### DOIs that could not be checked\n",
        "No record was retrieved for these cited DOIs, so the references were not checked: follow each DOI and compare the reference with the work. Where the table names a registration agency, doi.org knows the DOI but did not return its record, and running the check again may fix it. Where it says unknown, doi.org gave no answer, so it is not known whether the DOI exists: run the check again later.\n",
        reg_report),
    if (!is.null(unread_report))
      c("### Entries that could not be read reliably\n",
        "These entries were not read correctly from the PDF, so they were not checked: check them by hand against the original source. A reading problem is usually a limitation of the PDF reading, not an error in the reference list.\n",
        unread_report),
    if (!is.null(nf_report))
      c("### References not found\n",
        "These references have no DOI, or one that is not registered, and no work with the cited title was found in Crossref, DataCite (which includes arXiv, Zenodo and OSF) or Semantic Scholar (which includes conference papers without DOIs). Books, reports, theses, software and web pages are often missing from these sources, and a title misread from the PDF is not found either. For a journal article or a preprint, check the title and that the work exists.\n",
        nf_report),
    if (!is.null(suggested_report))
      c("### Suggested DOIs\n",
        "These references have no DOI, but a work was found whose details agree with the reference: its title, year and authors (a book's record may list no authors), and for an article its journal. The source is named when the work was found by searching for its title rather than by Crossref's match for the reference. Check that the linked record is the cited work and version before adding its DOI. The retraction, PubPeer and replication checks do not use these suggestions: for every reference without a DOI, they use the DOI of Crossref's match, whether or not it is suggested.\n",
        suggested_report),
    if (!is.null(other_report))
      c("### Other references without a DOI\n",
        "These references have no DOI, and no record found for them matches the reference closely enough for its DOI to be suggested; the table gives the reason. Where the closest record is linked, check whether it is the cited work before using its DOI; one marked (?) is Crossref's best guess and is more likely to be another work.\n",
        other_report)
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

