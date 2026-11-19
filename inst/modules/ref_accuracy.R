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
#' - **DOI**: URL wrappers, spaces, punctuation and the old double slash (`10.1037//`). A cited DOI that the registry lists as an alias of the record's DOI is a note.
#' - **Year**: up to `year_tolerance` years, or the year of the record's online, print or issue date. A year off by one more year is a note when it is near the year the DOI was created or the work accepted.
#' - **Authors**: the record's first `max_authors` surnames must all be cited, its first author first. Surnames are compared on their letters (accents, spacing and case are ignored), allowing shortened compound surnames and particles. A list truncated with "et al." needs only the authors it cites, and given and family names swapped on one side are a note (as they are often metadata errors).
#' - **Journal**: ISO 4 abbreviations, acronyms, "&" for "and", British and American spellings, a missing subtitle, and a conference's proceedings cited without "Proceedings of", or by the short name in the conference's name ("AAAI" for "Proceedings of the AAAI Conference on Artificial Intelligence"). No word may be skipped. A journal cited by another of its names is accepted when Wikidata gives that name for the journal or for its predecessor or successor, as Crossref's records of back issues may give a journal's current name (a 1972 article in the Journal of the Royal Statistical Society, Series B (Methodological), is in its successor, Series B: Statistical Methodology); other names may be flagged. Only journal articles are compared.
#' - **Title**: case, punctuation, Unicode forms, a missing subtitle, a translation in square brackets, and British, Canadian and Australian spellings (folded to the American from VarCon's list of spelling variants). Any other difference, even of one word, is flagged.
#' - **Preprints**: a cited preprint is not compared on its year, journal or title with its published version, which is a note. A reference cited in a journal whose work is found only as a preprint is flagged, unless its venue is a conference.
#'
#' Each limit errs towards not flagging, to keep false alarms few; they are judgement calls set on the papers described under validation. Raising `title_similarity` or `suggest_score` compares fewer records, so it gives fewer flags but also fewer suggested DOIs. Entries that were not read reliably from the PDF (two references read as one, a DOI read without its reference) are listed and not compared. The report shows each difference as a diff of the printed reference and the record.
#'
#' <validation>In 30 randomly sampled Psychological Science papers (1,269 references, November 2026), the module flagged 29 references, of which 24 were citation errors when checked by hand. Of the 706 references without a DOI, 561 were given a suggested DOI; 63 of the 64 checked by hand were the cited work. It flagged 140 of 141 errors added to references with a DOI. On CiteTracer's synthetic benchmark (`data-raw/ref_accuracy_citetracer.R`), it flagged or listed as not found 68% of citations with a fabricated field, and flagged 0.7% of real citations. The rules were developed on these data, so they may perform less well on others.</validation>
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
#'     record of its own DOI (from Crossref, or else from doi.org). "unresolved": its DOI has no record, and doi.org
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
  # no Crossref lookup (convert() without crossref, no add_bib_match()): the
  # references are not checked, which is not a fault of the paper, but those
  # without a DOI are still counted
  if (nrow(bib_match) == 0) bib_match <- bib[0, cols]
  if (!"bib_type" %in% names(bib_match)) bib_match$bib_type <- NA_character_
  ref_table <- ref_table(paper)
  ref_table$doi <- NULL
  # one row per reference: with several bib_match rows for a bib_id the three
  # joins below gave k^3 rows, and ref_table() repeats the reference too
  # (U114); the match with the highest score is kept
  # matches read from elsewhere may have no score column
  if (!"score" %in% names(bib_match)) bib_match$score <- NA_real_
  if (!"service" %in% names(bib_match)) bib_match$service <- NA_character_
  bib_match <- bib_match[order(-bib_match$score), , drop = FALSE]
  bib_match <- bib_match[!duplicated(bib_match[, c("paper_id", "bib_id")]), , drop = FALSE]
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

  # A cited DOI without a Crossref record is checked at doi.org: whether it
  # exists, with which agency, and, if it exists, its record (DataCite, mEDRA
  # and other agencies are not looked up by add_bib_match(), and a Crossref
  # lookup may have failed). The record is then compared like any other.
  table$doi_exists <- NA
  table$doi_ra <- NA_character_
  unres <- which(!blank(table$doi.orig) & blank(table$title.match) &
                   table$paper_id %in% bib_match$paper_id)
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
  text_doi <- vapply(table$text, \(x) {
    if (is.na(x)) return(NA_character_)
    m <- regmatches(x, regexpr(
      "(?i)(doi\\.org/|\\bdoi:\\s*)\\S+|10\\.\\d{4,9}(/|%2F)\\S+", x, perl = TRUE))
    if (!length(m)) return(NA_character_)
    doi_clean(sub("(?i)^.*?(doi\\.org/|doi:\\s*)", "", m, perl = TRUE))
  }, character(1), USE.NAMES = FALSE)
  in_text_doi <- blank(table$doi.orig) & !is.na(text_doi) &
    nchar(alnum(text_doi)) >= 8
  same_doi <- !is.na(table$doi.match) & (
    alnum(text_doi) == alnum(table$doi.match) |
      alnum(text_doi) == alnum(sub("^[^/]*/", "", table$doi.match)))
  table$doi.orig[in_text_doi & same_doi] <- text_doi[in_text_doi & same_doi]
  table$read_problem[in_text_doi & !same_doi] <- sprintf(
    "A DOI is printed in this reference (%s), but it was not read from the PDF, so it was not looked up.",
    text_doi[in_text_doi & !same_doi])

  # Only a DOI was read: the rest of the reference stayed with the entry
  # before it. When that entry has no DOI and its title search found this one,
  # the DOI is given to it and the fragment is dropped.
  doi_only <- !blank(table$doi.orig) & blank(table$title.orig) &
    blank(table$authors.orig) & blank(table$container.orig) & is.na(table$year.orig)
  prev <- c(NA, seq_len(nrow(table) - 1))
  prev[c(TRUE, table$paper_id[-1] != table$paper_id[-nrow(table)])] <- NA
  joins <- doi_only & !is.na(prev)
  joins[joins] <- blank(table$doi.orig[prev[joins]]) &
    alnum(doi_clean(table$doi.orig[joins])) == alnum(table$doi.match[prev[joins]]) &
    !is.na(table$doi.match[prev[joins]])
  table$doi.orig[prev[joins]] <- table$doi.orig[joins]
  table$read_problem[joins] <- "fragment"
  table$read_problem[doi_only & !joins] <-
    "Only the DOI was read for this entry; the rest of the reference was not separated from the entry before it."

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
  norm_doi <- \(x) tolower(sub("^(10\\.\\d+)//", "\\1/", doi_clean(x)))
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
  looked_up_by_doi <- is.na(table$score) &
    doi_valid_format(doi_clean(table$doi.orig)) %in% TRUE &
    !table$paper_id %in% .bibr12_paper_ids(paper)
  table$doi_alias <- (doi_differs %in% TRUE) & looked_up_by_doi
  table$doi_mismatch <- doi_differs & !table$doi_alias

  # year: only flag when more than `year_tolerance` years apart (online-first
  # vs print publication routinely differ by a year and is not an incoherence)
  year_diff <- abs(as.numeric(table$year.orig) - as.numeric(table$year.match))
  table$year_mismatch <- !is.na(year_diff) & year_diff > year_tolerance

  # fold accented Latin letters to plain ASCII ("Gredebäck" and "Gredeback",
  # "Nguyễn" and "Nguyen", "Þórsson" and "Thorsson"), so that diacritics never
  # cause a mismatch on their own
  deaccent <- \(x) stringi::stri_trans_general(x, "Latin-ASCII")

  # clean up text to prevent irrelevant mismatches
  clean <- \(x) {
    tolower(x) |>
      gsub("</?[a-z]+>", "", x = _) |>
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
  spelling_keys <- names(spelling)
  uk_us <- \(w) {
    out <- unname(spelling[w])
    rest <- which(is.na(out) & nchar(w) > 6)
    for (k in 6:max(6, nchar(w[rest]) - 1)) {
      if (!length(rest)) break
      ending <- substring(w[rest], nchar(w[rest]) - k + 1)
      hit <- match(ending, spelling_keys)
      ok <- !is.na(hit) & nchar(w[rest]) > k
      out[rest[ok]] <- paste0(substr(w[rest[ok]], 1, nchar(w[rest[ok]]) - k), spelling[hit[ok]])
    }
    ifelse(is.na(out), w, out)
  }
  # uk_us for each element of a list of words, in one call
  uk_us_list <- \(l) {
    flat <- uk_us(unlist(l))
    unname(split(flat, factor(rep(seq_along(l), lengths(l)), levels = seq_along(l))))
  }
  # the words of each name, lower case, without "of", "the", ...
  journal_token_list <- \(x) {
    x <- clean(x)
    x <- gsub("&amp;|&", " and ", x)
    x <- gsub("\\bwires\\b", "wiley interdisciplinary reviews", x)
    x <- gsub("[^a-z ]", " ", x)
    # "a" after "series", "section" or "part" is a letter, not a word to drop
    # ("Series A" is not "Series B")
    lapply(uk_us_list(strsplit(trimws(gsub("\\s+", " ", x)), " ")), \(toks) {
      letter <- toks == "a" & c("", head(toks, -1)) %in% c("series", "ser", "section", "sect", "part", "pt")
      toks[(letter | !toks %in% c("of","and","the","for","in","on","a","an","de","")) & nchar(toks) > 0]
    })
  }
  journal_tokens <- \(x) journal_token_list(x)[[1]]
  # do tokens ta match tb from position `from` on, a token as a prefix?
  tokens_at <- \(ta, tb, from) {
    idx <- from + seq_along(ta) - 1L
    all(idx <= length(tb)) &&
      all(startsWith(tb[idx], ta) | startsWith(ta, tb[idx]))
  }
  # do the tokens of ta begin tb, each token or its abbreviation (a prefix)
  # in turn? tb may continue (a subtitle or section), but no word of tb may
  # be skipped: "Journal of Experimental Psychology" is not "Journal of
  # Experimental Social Psychology". Two words that make one word of the
  # other name are a word split at a line break ("Psycho logical Review").
  in_sequence <- \(ta, tb) {
    hit <- \(a, b) startsWith(b, a) || startsWith(a, b)
    i <- 1L
    j <- 1L
    while (i <= length(ta)) {
      if (j > length(tb)) return(FALSE)
      if (i < length(ta) && paste0(ta[i], ta[i + 1]) == tb[j]) {
        i <- i + 2L; j <- j + 1L
      } else if (j < length(tb) && paste0(tb[j], tb[j + 1]) == ta[i]) {
        i <- i + 1L; j <- j + 2L
      } else if (hit(ta[i], tb[j])) {
        i <- i + 1L; j <- j + 1L
      } else return(FALSE)
    }
    TRUE
  }
  journal_coherent <- \(cited, record, text) {
    # a journal read as the subtitle of its name ("An International Journal
    # of Research and Practice" in "Infant and Child Development: An
    # International Journal of Research and Practice")
    if (!is.na(text) && grepl(paste0(clean(record), ": ", clean(cited)), clean(text), fixed = TRUE))
      return(TRUE)
    # an acronym before a full stop at the start of a name ("MMWR. Morbidity
    # and Mortality Weekly Report") may be left out
    acronym <- "^\\s*[A-Z]{2,}\\.\\s+(?=\\S)"
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
    names_coherent(cited, record, text) ||
      (grepl(acronym, cited, perl = TRUE) || grepl(acronym, record, perl = TRUE)) &&
      names_coherent(sub(acronym, "", cited, perl = TRUE),
                     sub(acronym, "", record, perl = TRUE), text)
  }
  names_coherent <- \(cited, record, text) {
    ta <- journal_tokens(cited); tb <- journal_tokens(record)
    if (!length(ta) || !length(tb)) return(TRUE)        # unknown, don't flag
    # a one-word name is not abbreviated (ISO 4), so it must match exactly
    # ("CVPR" is not "CVP", "Science" is not "Sciences")
    if (length(ta) == 1 && length(tb) == 1) return(identical(ta, tb))
    short <- if (length(ta) > length(tb)) tb else ta
    long <- if (length(ta) > length(tb)) ta else tb
    if (!(length(short) == 1 && length(long) > 1) && in_sequence(short, long))
      return(TRUE)
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
  table$container_mismatch <- mapply(\(a, b, text, check) {
    if (!check || is.na(a) || is.na(b) || !nzchar(trimws(a)) || !nzchar(trimws(b)))
      return(FALSE)
    !journal_coherent(a, b, text)
  }, table$container.orig, table$container.match, table$text,
  is_article & !article_number, USE.NAMES = FALSE)
  # A journal cited by another of its names in Wikidata is accepted: an alias
  # of the record's journal, or a name of the journal Wikidata links to it as
  # predecessor or successor (data-raw/journal_successions.R). Crossref's
  # records of back issues may give the current name ("ILR Review" for a 2006
  # article in Industrial and Labor Relations Review). The record's name must
  # be one of the journal's names, word for word, or begin one. The cited name
  # may abbreviate the other name or stop early, but not add words to it: the
  # name of a predecessor ("Philosophical Transactions") does not accept a
  # journal that begins with it (its series A, for series B).
  as_listed <- \(cited, name) {
    ta <- journal_tokens(cited); tb <- journal_tokens(name)
    if (!length(ta) || !length(tb)) return(FALSE)
    if (length(ta) == 1) return(identical(ta, tb))
    length(ta) <= length(tb) && in_sequence(ta, tb)
  }
  jmm <- which(table$container_mismatch)
  if (length(jmm)) {
    js <- readRDS(system.file("databases/journal_successions.Rds", package = "metacheck"))
    # the journal of each name, and the journals linked to each journal
    js_journal <- rep(seq_along(js$size), js$size)
    # the names that begin with the same letters as the record's are compared
    # with it word for word (the file keeps one spelling of a name, which may
    # be British or American)
    quick <- \(x) substr(gsub("[^a-z]", "", tolower(x)), 1, 5)
    js_quick <- quick(js$names)
    for (k in jmm) {
      rec <- journal_tokens(table$container.match[k])
      cand <- which(js_quick == quick(table$container.match[k]))
      same <- vapply(journal_token_list(js$names[cand]), \(w)
        if (length(rec) == 1) identical(w, rec) else
          length(rec) <= length(w) && identical(w[seq_along(rec)], rec), logical(1))
      journals <- unique(js_journal[cand[same]])
      if (!length(journals)) next
      journals <- unique(c(journals, js$links[js$links[, 1] %in% journals, 2]))
      alt <- setdiff(js$names[js_journal %in% journals], table$container.match[k])
      if (any(vapply(alt, \(a) as_listed(table$container.orig[k], a), logical(1))))
        table$container_mismatch[k] <- FALSE
    }
  }

  # title check: compare on character similarity rather than exact equality, so
  # that formatting noise (footnote markers, em-dash spacing, colons/question
  # marks, line-break word splits like "Proba bilistic") does not flag a real
  # title, while a genuinely different title still does. norm_title strips tags,
  # footnote markers and punctuation, folds dashes to spaces and diacritics.
  norm_title <- \(x) {
    # footnote superscripts first: clean() removes the tags but keeps their
    # text, so this never matched after it and the markers stayed (U114)
    x <- gsub("<sup>.*?</sup>", "", x, ignore.case = TRUE, perl = TRUE)
    x <- clean(x)                 # lowercases, strips tags, folds diacritics
    # letters and digits of every script: keeping only a-z emptied a title
    # in Cyrillic or Chinese, which then agreed with any record
    gsub("[^\\p{L}\\p{N}]", "", x, perl = TRUE)
  }
  # does a cited title agree with a record's title? Vectorised over the three
  # arguments; also used for the DataCite records found for a reference
  # without a DOI
  title_check <- \(cited, record, text) {
    if (!length(cited)) return(list(mismatch = logical(0), agrees = logical(0),
                                    starts_same = logical(0), charsim = numeric(0),
                                    exact = logical(0), same = logical(0)))
    a <- norm_title(cited)
    b <- norm_title(record)
    charsim <- mapply(\(x, y) {
      if (is.na(x) || is.na(y) || !nzchar(x) || !nzchar(y)) return(NA_real_)
      1 - utils::adist(x, y)[1, 1] / max(nchar(x), nchar(y))
    }, a, b, USE.NAMES = FALSE)

    # also accept when the record title appears verbatim in the reference text
    # (handles cases where GROBID failed to split the title out of the
    # citation), as whole words ("behavior" is not found in "behaviorism" or
    # "misbehavior") and ending at a title delimiter or at the bracket of an
    # APA description ("(Version 1.0.0) [Computer code]"), so that a cited title
    # that continues ("Origins" in "The origins of ...") or a journal name
    # followed by its volume is not taken for the title.
    # (a word split at a line break, "normal- ized", is joined)
    clean_text <- clean(gsub("(?<=[a-z])- (?=[a-z])", "", text, perl = TRUE))
    match_clean <- clean(record)
    escape <- \(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
    in_text <- mapply(\(pattern, x) !is.na(pattern) && nzchar(pattern) &&
                        grepl(pattern, x, fixed = TRUE) &&
                        grepl(paste0("(^|[^a-z0-9])", escape(pattern),
                                     if (grepl("[:?.!]$", pattern)) "" else "\\s*($|[:?.!(\\[])"),
                              x, perl = TRUE),
                      match_clean, clean_text, USE.NAMES = FALSE)

    # Crossref often holds only the main title, without the subtitle the
    # reference cites ("Marginally significant effects as evidence for
    # hypotheses" for "...: Changing attitudes over four decades"). A cited
    # title that starts with the record's title is accepted when the rest
    # starts at a title delimiter (":", "?", "." or "!"); the subtitle itself
    # cannot then be checked.
    delimited <- \(x) {
      x <- gsub("<sup>.*?</sup>", "", x, ignore.case = TRUE, perl = TRUE)
      trimws(gsub("[^a-z0-9:?.!]+", " ", clean(x)))
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
    # "&", punctuation, spacing and British and American spellings, the two
    # are equal, or one is the other's part before or after a title delimiter
    # (":", "?", "." or "!"): a citation or record without the subtitle, or
    # without a main title or acronym before it ("PRBoost: ..."). A title
    # that stops or continues without a delimiter is another title ("Effects
    # of training on memory" is not "Effects of training on memory in older
    # adults"). Any other difference, even of one word, is a title mismatch.
    words <- \(x) {
      x <- gsub("<sup>.*?</sup>", "", x, ignore.case = TRUE, perl = TRUE)
      x <- gsub("&amp;|&", " and ", clean(x))
      uk_us_list(strsplit(trimws(gsub("[^\\p{L}\\p{N}]+", " ", x, perl = TRUE)), " "))
    }
    squash <- \(x) vapply(words(x), paste, "", collapse = "")
    same_title <- \(x, y) {
      if (is.na(x) || is.na(y)) return(FALSE)
      # an APA description after the cited title ("[Conference session]")
      x <- sub("\\s*\\[[^]]*\\]?\\s*$", "", stringi::stri_trans_nfkc(x))
      y <- stringi::stri_trans_nfkc(y)
      sx <- squash(x)
      sy <- squash(y)
      if (!nzchar(sx) || !nzchar(sy)) return(FALSE)
      if (sx == sy) return(TRUE)
      # a footnote that the record appends to its title ("...adolescence11The
      # full text of this article ..."): a marker followed by a sentence, not a
      # number that continues the title ("Memory across adult life 2")
      if (startsWith(sy, sx) && grepl("^[0-9]{1,3}[a-z]{20,}", substring(sy, nchar(sx) + 1))) return(TRUE)
      long <- if (nchar(sx) > nchar(sy)) x else y
      short <- if (nchar(sx) > nchar(sy)) sy else sx
      cut <- gregexpr("[:?.!\u2014]", long)[[1]]
      cut <- cut[cut > 0]
      any(vapply(cut, \(k) squash(substr(long, 1, k - 1)) == short ||
                   squash(substring(long, k + 1)) == short, logical(1)))
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
  tc <- title_check(table$title.orig, table$title.match, table$text)
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
    \(x) {
      if (trimws(x) %in% journal_names$acronyms) return(TRUE)
      tok <- journal_tokens(x)
      if (!length(tok)) return(FALSE)
      if (is.null(first)) first <<- vapply(journal_token_list(journal_names$titles),
        \(t) t[1] %||% "", "")
      cand <- journal_names$titles[nzchar(first) &
                                     (startsWith(first, tok[1]) | startsWith(tok[1], first))]
      # a conference is not the journal its name begins with ("Proceedings
      # of the IEEE Computer Society Conference ..." is not "Proceedings of
      # the IEEE")
      if (grepl(event_words, x, perl = TRUE))
        cand <- cand[grepl(event_words, cand, perl = TRUE)]
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
  check <- unique(table$container.orig[looks_like_conference & !cited_preprint])
  listed <- check[vapply(check, listed_journal, logical(1))]
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
  letters_only <- \(x) gsub("[^a-z]", "", clean(x))
  # the last part of a surname, after a space, a hyphen or an elided
  # particle: "O'Connell" read from the PDF as "Connell", "Purdie-Vaughns"
  # cited as "Vaughns"
  last_part <- \(x) letters_only(
    sub(".*([[:space:]\u00a0\u2010-]|\\b[DdLlOo]['\u2019])", "", x, perl = TRUE))
  cited_surnames <- \(o) {
    people <- trimws(strsplit(paste(o, collapse = ";"), ";", fixed = TRUE)[[1]])
    people <- people[nzchar(people)]
    family <- trimws(sub(",.*", "", people))
    has_comma <- grepl(",", people, fixed = TRUE)
    list(name = letters_only(family),
         last = last_part(family),
         one_word = has_comma & !grepl("[[:space:]\u00a0]", family),
         given_first = !has_comma,
         given = ifelse(has_comma, letters_only(sub("^[^,]*,", "", people)), ""),
         # "Alice Brown Jones" ends in "jones", "brownjones", ...
         ends = lapply(family, \(f) {
           w <- letters_only(strsplit(f, "[[:space:]\u00a0]+")[[1]])
           w <- w[nzchar(w)]
           vapply(seq_along(w), \(k) paste(utils::tail(w, k), collapse = ""), "")
         }))
  }
  # which cited people does record surname x (given name g) match? With
  # `swapped`, a name whose parts are swapped on one side matches too: the
  # record's given name is the cited surname, and the record's surname starts
  # with the cited initial ("Wu, K." for given "Wu", family "Kui"). Such a
  # name is not a mismatch, but it is noted.
  person_matches <- \(x, cs, g = NA, swapped = FALSE) {
    full <- letters_only(x)
    last <- last_part(x)
    record_one_word <- !grepl("[[:space:]\u00a0]", trimws(x))
    g <- if (is.na(g)) "" else letters_only(g)
    cs$name == full |
      (cs$one_word & cs$name == last) |
      (record_one_word & !cs$given_first & cs$last == full) |
      (cs$given_first & vapply(cs$ends, \(e) full %in% e, logical(1))) |
      (swapped & nzchar(g) & nzchar(full) & cs$name == g & nzchar(cs$given) &
         substr(cs$given, 1, 1) == substr(full, 1, 1))
  }
  # is record surname x one or more consecutive words of a printed list?
  words_match <- \(x, w) {
    full <- letters_only(x)
    wl <- letters_only(w)
    joined <- unlist(lapply(1:3, \(k) {
      if (length(wl) < k) return(character())
      vapply(seq_len(length(wl) - k + 1), \(s) paste(wl[s:(s + k - 1)], collapse = ""), "")
    }))
    full %in% joined || last_part(x) %in% wl || full %in% last_part(w)
  }
  printed_words <- \(o) {
    w <- strsplit(o, "[[:space:]\u00a0,;&]+")[[1]]
    w[nzchar(letters_only(w))]
  }
  bibr <- table$paper_id %in% .bibr12_paper_ids(paper)
  # A list truncated with "et al." (or "and others", as BibTeX gives it) is
  # a citation style: only as many record authors as were cited are required.
  # The PDF reader may drop "et al." from the authors, so the reference text
  # before the cited title is read too.
  etal <- "(?i)\\bet\\.?\\s*al\\b|(^|[;,&]\\s*|\\band\\s+)others\\b"
  truncated <- \(o, text, title) {
    # compared after clean(), so a title read with other capitals than the
    # reference text ("The Origins of" in "The origins of") is still found
    before <- if (is.na(text)) "" else {
      text <- clean(text)
      at <- if (is.na(title) || !nzchar(title)) -1L else
        regexpr(substr(clean(title), 1, 20), text, fixed = TRUE)
      if (at > 0) substr(text, 1, at - 1) else ""
    }
    grepl(etal, paste(o, collapse = "; "), perl = TRUE) ||
      grepl(etal, before, perl = TRUE)
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
    l <- utils::head(l[keep], max_authors)
    g <- utils::head(g[keep], max_authors)
    cut <- truncated(o, text, title)
    o <- gsub(etal, "", paste(o, collapse = "; "), perl = TRUE)
    if (as_printed) {
      w <- printed_words(o)
      if (cut) l <- utils::head(l, 1)
      found <- vapply(l, \(x) words_match(x, w), logical(1))
      first_ok <- is.na(first) || words_match(first, printed_words(sub(",.*", "", o)))
    } else {
      cs <- cited_surnames(o)
      if (cut) l <- utils::head(l, max(1, sum(nzchar(cs$name))))
      found <- vapply(l, \(x) any(person_matches(x, cs)), logical(1))
      swap <- !found & vapply(seq_along(l), \(k)
        any(person_matches(l[k], cs, g[k], swapped = TRUE)), logical(1))
      found <- found | swap
      first_ok <- is.na(first) || !length(cs$name) ||
        person_matches(first, cs, first_g, swapped = TRUE)[1]
    }
    # each record surname that is missing, with the cited name closest to it
    # (a misspelling: "Sonuga-Barke is cited as Sonuga-Burke")
    cited_words <- unique(strsplit(trimws(o), "[;,&[:space:]\u00a0]+")[[1]])
    cited_words <- cited_words[nchar(letters_only(cited_words)) >= 2]
    missing <- vapply(l[!found], \(x) {
      d <- if (length(cited_words)) utils::adist(letters_only(x), letters_only(cited_words))[1, ] /
        pmax(nchar(letters_only(x)), nchar(letters_only(cited_words))) else numeric(0)
      if (length(d) && min(d) <= 1 / 3) sprintf("%s is cited as %s", x, cited_words[which.min(d)])
      else sprintf("%s is in the record but not in the reference", x)
    }, "")
    note <- c(
      if (!all(found)) paste(missing, collapse = "; "),
      if (all(found) && !first_ok) sprintf("the record's first author, %s, is not cited first", first))
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
  looked_up   <- table$paper_id %in% bib_match$paper_id
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
  title_found <- \(hits, q, text, year) {
    first <- rep(NA_integer_, length(q))
    first[unique(hits$query[hits$error])] <- -1L
    ok_rows <- which(!hits$error & !blank(hits$title))
    if (length(ok_rows)) {
      h <- hits[ok_rows, ]
      ok <- title_check(q[h$query], h$title, text[h$query])
      same <- ok_rows[ok$agrees | ok$starts_same]
      # a record of the cited year first (not a reprint or another
      # edition), and a published version before a preprint with the same title
      pre <- is_preprint_rec(hits$type[same], hits$doi[same])
      other_year <- !(hits$year[same] == year[hits$query[same]]) %in% TRUE
      same <- same[order(hits$query[same], other_year, pre)]
      first[unique(hits$query[same])] <- same[!duplicated(hits$query[same])]
    }
    first
  }
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
    # each title is searched once, and its result given to every reference
    # with that title (the same work cited twice, or in several papers)
    q_ref <- table$title.orig[searched]
    q <- unique(q_ref)
    qi <- match(q_ref, q)
    q_text <- table$text[searched][match(q, q_ref)]
    q_year <- table$year.orig[searched][match(q, q_ref)]
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
    # every record each search returned, to tell whether the found work is
    # the only one with its title and year (below)
    cand <- cr[!cr$error & !blank(cr$title), c("query", "doi", "title", "year", "type", "container")]
    if (nrow(cand)) cand$authors <- cr$authors[!cr$error & !blank(cr$title)] else cand$authors <- list()
    cand$source <- rep("crossref", nrow(cand))
    cand$url <- rep(NA_character_, nrow(cand))
    first <- title_found(cr, q, q_text, q_year)
    rec <- data.frame(source = ifelse(first > 0, "crossref", NA),
                      doi = NA_character_, title = NA_character_, year = NA_integer_,
                      type = NA_character_, container = NA_character_,
                      url = NA_character_)
    rec$authors <- vector("list", nrow(rec))
    got <- which(first > 0)
    rec[got, c("doi", "title", "year", "type", "container")] <-
      cr[first[got], c("doi", "title", "year", "type", "container")]
    rec$authors[got] <- cr$authors[first[got]]
    # a title Crossref could not be searched for is still searched in DataCite
    # "Family, Given; ..." names as surnames with their given names
    split_names <- \(a) {
      p <- trimws(strsplit(a, ";", fixed = TRUE)[[1]])
      structure(trimws(sub(",.*", "", p)),
                given = ifelse(grepl(",", p), trimws(sub("^[^,]*,", "", p)), NA))
    }
    # A title found only as a preprint, for a reference cited in a journal,
    # is searched in the other sources too, for a published version; when a
    # search for it fails, the reference is not flagged as found only as a
    # preprint.
    q_first <- match(q, q_ref)
    q_journal <- journal_cited[searched][q_first]
    pre_unsure <- rep(FALSE, length(q))
    s2_skipped <- rep(FALSE, length(q))
    # A reference cited as a book, report or other work that is not an
    # article is searched in all three sources for a record with its title by
    # the cited authors: a title such as "Social psychology" belongs to
    # several books, and a book's record is often a review of it, a chapter or
    # a record that lists its editors or contributors.
    q_auth <- table$authors.orig[searched][q_first]
    q_bibr <- bibr[searched][q_first]
    q_type <- table$bib_type.orig[searched][q_first]
    q_cited_book <- !is.na(q_type) & !q_type %in% c(article_set, "preprint")
    # do the authors of these records agree with those cited for queries qk?
    authors_agree <- \(authors, qk) {
      if (!length(qk)) return(logical(0))
      note <- split_swap(author_check(authors, q_auth[qk], q_bibr[qk], q_text[qk], q[qk]))$note
      vapply(authors, \(a) any(!is.na(a) & nzchar(a)), logical(1)) & !blank(q_auth[qk]) & is.na(note)
    }
    # the candidates with the cited title by the cited authors
    cand_by_authors <- \() title_check(q[cand$query], cand$title, q_text[cand$query])$same %in% TRUE &
      authors_agree(cand$authors, cand$query)
    pending <- \() which(is.na(first) | first < 0 |
                          (q_journal & is_preprint_rec(rec$type, rec$doi) & !is.na(rec$source)) |
                          (q_cited_book & !is.na(rec$source) &
                             !seq_along(q) %in% cand$query[cand_by_authors()]))
    # the record each search found for the pending titles: kept when the title
    # had none, or had a preprint for a journal citation and this is a
    # published article (a record of unknown type may be the preprint again)
    take <- \(rest, f, hits) {
      had <- !is.na(rec$source[rest])
      pre <- had & q_journal[rest] & is_preprint_rec(rec$type[rest], rec$doi[rest])
      k <- ifelse(f %in% seq_len(nrow(hits)), f, NA_integer_)
      pre_unsure[rest[pre & f %in% -1L]] <<- TRUE
      !is.na(k) & (!had | (pre & hits$type[k] %in% c("journal-article", "proceedings-article",
                                                      "JournalArticle", "Conference") &
                             !is_preprint_rec(hits$type[k], hits$doi[k])))
    }
    rest <- pending()
    if (length(rest)) {
      dc <- datacite_query(q[rest])
      for (col in c("doi", "title", "year", "authors", "type"))
        if (is.null(dc[[col]])) dc[[col]] <- rep(NA, nrow(dc))
      dfirst <- title_found(dc, q[rest], q_text[rest], q_year[rest])
      ok <- which(!dc$error & !blank(dc$title))
      add <- data.frame(query = rest[dc$query[ok]], doi = dc$doi[ok], title = dc$title[ok],
                        year = suppressWarnings(as.integer(dc$year[ok])), type = dc$type[ok],
                        container = rep(NA_character_, length(ok)))
      add$authors <- lapply(dc$authors[ok], split_names)
      add$source <- rep("datacite", length(ok))
      add$url <- rep(NA_character_, length(ok))
      cand <- rbind(cand, add)
      # not found when neither search found it, failed when either failed
      new <- take(rest, dfirst, dc)
      first[rest] <- ifelse(!is.na(rec$source[rest]) & !new, first[rest],
                            ifelse(is.na(dfirst) & first[rest] %in% -1L, -1L, dfirst))
      got <- which(new)
      rec$source[rest[got]] <- "datacite"
      rec[rest[got], c("doi", "title", "year", "type")] <- dc[dfirst[got], c("doi", "title", "year", "type")]
      rec$container[rest[got]] <- NA_character_
      rec$authors[rest[got]] <- lapply(dc$authors[dfirst[got]], split_names)
    }
    # a title found in neither is searched in Semantic Scholar, which holds
    # conference papers without DOIs; its requests are paced. Without it, a
    # title not found so far is not checked (not "not found"), and a preprint
    # found for a journal citation is not flagged
    rest <- pending()
    if (length(rest) && !semantic_scholar) {
      unfound <- rest[is.na(first[rest]) | first[rest] < 0]
      first[unfound] <- -1L
      s2_skipped[unfound] <- TRUE
      pre_unsure[rest[q_journal[rest] & !is.na(rec$source[rest]) &
                        is_preprint_rec(rec$type[rest], rec$doi[rest])]] <- TRUE
      rest <- integer(0)
    }
    if (length(rest)) {
      s2 <- semanticscholar_query(q[rest])
      sfirst <- title_found(s2, q[rest], q_text[rest], q_year[rest])
      ok <- which(!s2$error & !blank(s2$title))
      add <- data.frame(query = rest[s2$query[ok]], doi = s2$doi[ok], title = s2$title[ok],
                        year = suppressWarnings(as.integer(s2$year[ok])), type = s2$type[ok],
                        container = s2$venue[ok])
      add$authors <- lapply(s2$authors[ok], split_names)
      add$source <- rep("semanticscholar", length(ok))
      add$url <- s2$url[ok]
      cand <- rbind(cand, add)
      new <- take(rest, sfirst, s2)
      first[rest] <- ifelse(!is.na(rec$source[rest]) & !new, first[rest],
                            ifelse(is.na(sfirst) & first[rest] %in% -1L, -1L, sfirst))
      got <- which(new)
      rec$source[rest[got]] <- "semanticscholar"
      rec[rest[got], c("doi", "title", "year", "type", "container")] <-
        s2[sfirst[got], c("doi", "title", "year", "type", "venue")]
      rec$url[rest[got]] <- s2$url[sfirst[got]]
      rec$authors[rest[got]] <- lapply(s2$authors[sfirst[got]], split_names)
    }
    # a book found with other authors is shown with the record of its title by
    # the cited authors, when one was found, or else with one of its title
    # that lists no authors (of the cited year first)
    bq <- which(q_cited_book & !is.na(rec$source))
    if (length(bq)) {
      by_authors <- cand_by_authors()
      no_authors <- title_check(q[cand$query], cand$title, q_text[cand$query])$same %in% TRUE &
        !vapply(cand$authors, \(a) any(!is.na(a) & nzchar(a)), logical(1))
      listed_rec <- \(a) any(!is.na(a) & nzchar(a))
      for (k in bq) {
        if (authors_agree(rec$authors[k], k)) next
        m <- which(by_authors & cand$query == k)
        if (!length(m) && listed_rec(rec$authors[[k]])) m <- which(no_authors & cand$query == k)
        if (!length(m)) next
        m <- m[order(!cand$year[m] %in% q_year[k])][1]
        rec[k, c("source", "doi", "title", "year", "type", "container", "url")] <-
          cand[m, c("source", "doi", "title", "year", "type", "container", "url")]
        rec$authors[k] <- cand$authors[m]
      }
    }
    table$not_found[searched[is.na(first[qi])]] <- TRUE
    table$search_failed[searched[first[qi] %in% -1L]] <- TRUE
    skipped[searched[s2_skipped[qi]]] <- TRUE
    rec <- rec[qi, , drop = FALSE]
    rec$i <- searched
    rec$pre_unsure <- pre_unsure[qi]
    found[searched, ] <- rec[, c("source", "type", "doi", "url", "title")]

    # a DOI that does not exist: the work found by its title is shown with it;
    # one not found is not a citation error that can be shown, so it is listed
    # as not found; when the search failed, the DOI stays flagged
    unres <- table$tier[searched] %in% "unresolved"
    table$search_failed[searched[unres]] <- FALSE
    table$incoherent[searched[unres & table$not_found[searched]]] <- FALSE
    fu <- which(unres & !is.na(rec$source))
    if (length(fu)) {
      f <- searched[fu]
      table$doi_mismatch[f] <- TRUE
      table$record_source[f] <- rec$source[fu]
      table$doi.match[f] <- rec$doi[fu]
      table$record_url[f] <- rec$url[fu]
      table$title.match[f] <- rec$title[fu]
      table$year.match[f] <- rec$year[fu]
      table$container.match[f] <- rec$container[fu]
    }
    rec <- rec[!unres, , drop = FALSE]

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
      logical(1)) | title_check(table$title.orig[rec$i], rec$title, table$text[rec$i])$same
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
      tmis <- !title_check(table$title.orig[i], rec$title, table$text[i])$exact &
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
      # full name ("International Conference on Machine Learning")
      cmis <- mapply(\(a, b, text, check) {
        check && !is.na(a) && !is.na(b) && nzchar(trimws(a)) && nzchar(trimws(b)) &&
          !journal_coherent(a, b, text)
      }, table$container.orig[i], rec$container, table$text[i],
      (rec$type %in% "journal-article" |
         (rec$source == "semanticscholar" & !rec$type %in% "Preprint")) &
        !article_number[i] & !cited_preprint[i],
      USE.NAMES = FALSE)
      pmis <- pub_of_preprint & journal_cited[i] & !rec$pre_unsure
      flag <- amis | tmis | ymis | cmis | pmis
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
      k <- qi[match(i, searched)]
      cc <- cand[!blank(cand$doi) & !is.na(cand$year), ]
      cc <- cc[title_check(q[cc$query], cc$title, q_text[cc$query])$same %in% TRUE, ]
      # per reference, the one candidate with the cited year, or minus the
      # number of candidates
      pick <- vapply(seq_along(i), \(j) {
        m <- which(cc$query == k[j] & cc$year %in% table$year.orig[i[j]])
        m <- m[!duplicated(tolower(doi_clean(cc$doi[m])))]
        if (length(m) == 1) m else if (length(m)) -length(m) else NA_integer_
      }, integer(1))
      c1 <- cc[ifelse(pick %in% seq_len(nrow(cc)), pick, NA_integer_), ]
      # a publisher's record of a book often lists no authors, so for a
      # reference cited as a book the cited authors suffice when the record
      # names none; a record that names other authors is not suggested
      has_authors <- (vapply(c1$authors, \(a) any(!is.na(a)), logical(1)) | q_cited_book[k]) &
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
      chk <- which(one & has_authors & is.na(note) & !pre & q_cited_book[k] & !blank(c1$doi))
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
                                \(k) identical(tt[k + seq_along(ti) - 1L], ti), logical(1)))
            if (length(pos)) {
              tt[pos[1] + seq_along(ti) - 1L] <- "\001"
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
        !title_check(table$title.orig[i], s$title, table$text[i])$same %in% TRUE ~
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
    # formatting tags in registry titles ("<i>Pan troglodytes</i>") are
    # dropped, so that titles look alike whatever their source
    x <- gsub("</?(i|b|em|strong|u|sup|sub|scp|span|mml:[a-z]+|jats:[a-z-]+)\\b[^>]*>", "",
              as.character(x), ignore.case = TRUE, perl = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
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
    record <- gsub("\\s+", " ", trimws(as.character(record)))
    record <- gsub("</?(i|b|em|strong|u|sup|sub|scp|span|mml:[a-z]+|jats:[a-z-]+)\\b[^>]*>", "",
                   record, ignore.case = TRUE, perl = TRUE)
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
        correction("DOI", r$doi.orig, r$doi.match, words = FALSE)
    )
    paste(parts, collapse = "<br>")
  }

  # a record's DOI, or its Semantic Scholar page when it has none
  record_link <- \(doi, url) ifelse(
    !is.na(doi) & nzchar(doi),
    link(paste0("https://doi.org/", doi_clean(doi)), esc(doi)),
    ifelse(is.na(url), "", link(url, "Semantic Scholar")))

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
      `Current DOI` = link(paste0("https://doi.org/", alias_rows$doi.match),
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
      DOI = link(paste0("https://doi.org/", doi_clean(reg_rows$doi.orig)),
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
      Problem = unread_rows$read_problem,
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
      `Suggested DOI` = paste0(link(paste0("https://doi.org/", r$doi.match), esc(r$doi.match)),
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

