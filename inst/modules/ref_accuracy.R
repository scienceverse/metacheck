#' Reference Accuracy
#'
#' @description
#' This module checks whether each reference with a DOI is internally coherent: it compares the details the paper cites for a reference against the authoritative record that the reference's DOI points to, and flags any reference whose cited details do not match.
#'
#' @details
#' This module looks for *incoherent* references: a reference is incoherent when the title, authors, journal, or year that the paper cites disagree with the record that the reference's own DOI points to. Currently, it will most often be a parsing error where the reference list from the PDF is not extracted perfectly accurately, but the module can point out AI generated references, of mistakes in citations. You will need check the original source.
#'
#' The module uses the `bib_match` table, which holds the metadata retrieved from CrossRef for each reference (added or refreshed with `add_bib_match()`, which makes live network calls).
#'
#' **Only references that supply their own DOI are checked.** For these, we look up the record that exact DOI points to and compare it field by field against what the paper cites. A reference with no DOI is *not* checked: the only record we could find for it comes from a CrossRef title search, which often returns the wrong paper, so any "mismatch" would be CrossRef's error rather than the citation's. References without a DOI are instead listed separately, with a recommendation to add a DOI where one exists. For those, a suggested DOI is offered only when CrossRef found a high-confidence match (controlled by `suggest_score`).
#'
#' For each checked reference we compare five fields against the DOI's record:
#'
#' - **DOI** — flagged only when the reference printed a DOI that differs from the record (a blank cited DOI is never a mismatch).
#' - **Year** — flagged when the cited year is more than `year_tolerance` years from the record (online-first and print years routinely differ by one year, so this is allowed by default).
#' - **Journal** — flagged when the cited journal disagrees with the record. Journal names are matched tolerantly: standard ISO-4 abbreviations (e.g. "J. Pers. Soc. Psychol." for "Journal of Personality and Social Psychology"), "&"/"and", and "the" are treated as equivalent.
#' - **Title** — flagged when the cited title is too different from the record's title, judged by character-level similarity (`title_similarity`) after stripping case, punctuation, footnote markers and diacritics. This ignores cosmetic differences (colons, question marks, em-dash spacing, line-break word splits) while still catching a genuinely different title.
#' - **Authors** — flagged when the leading author surnames (the first `max_authors`) from the record are not all present in the cited author list. Only the leading authors are required because author lists are routinely truncated with "et al.". Diacritics are ignored, so "Gredebäck" and "Gredeback" match.
#'
#' DOI, journal and year are reliable signals and flag a reference on their own. Title and author mismatches are more often caused by imperfect PDF parsing, so these are governed by `min_mismatches`: the default (1) flags a reference when either the title or the authors disagree. Set `min_mismatches = 2` to be more conservative and require both to disagree (fewer false positives, lower recall).
#'
#' The report lists each incoherent reference with the specific cited value struck through and the record's value beside it, so the discrepancy is visible at a glance.
#'
#' <validation>In validation on psychology references with a DOI, the module flagged genuine, coherent references at a low rate: around 8% of checked references at the default setting (min_mismatches = 1), and about 3% with the more conservative setting (min_mismatches = 2). On deliberately corrupted references it caught journal and year fabrications reliably (about 96-100%) at either setting, and essentially all single-field fabrications (including single title or author changes) at min_mismatches = 1. Importantly, in a hand-review of references flagged across 10 papers from 5 fields, the large majority of flags were imperfect extraction of the reference from the PDF (for example a journal name mis-parsed, or two references merged) rather than genuine citation errors. Real citation mistakes are rare, so even though the false positive rate is low, a flag should be read as "check this reference against the original source", not as a confirmed error.</validation>
#'
#' @keywords reference
#'
#' @author Daniel Lakens (\email{D.Lakens@tue.nl})
#' @author Lisa DeBruine (\email{lisa.debruine@glasgow.ac.uk})
#'
#' @import dplyr
#'
#' @param paper a paper object or paperlist object
#' @param max_authors how many of the leading authors to compare against the
#'   retrieved record. Author lists are routinely truncated with "et al." (APA
#'   abbreviates after 7), and GROBID often drops the tail of long lists, so we
#'   only require the first `max_authors` surnames to match.
#' @param title_similarity the minimum character-level similarity (0-1, after
#'   stripping case, punctuation, footnote markers and diacritics) for a cited
#'   title to be considered a match for the retrieved title. Lower values
#'   tolerate more formatting noise; a title below this is flagged.
#' @param min_mismatches how many of the parsing-sensitive fields (title and
#'   author) must disagree with the retrieved record before a reference is
#'   flagged as incoherent. The default (1) flags a single title or author
#'   mismatch, which catches more genuine errors; the extra false positives this
#'   adds are minor compared with those that already come from imperfect PDF
#'   parsing. Set to 2 to be more conservative and require two of these fields
#'   to disagree. A mismatched DOI, journal or year always flags on its own,
#'   regardless of this setting.
#' @param year_tolerance how many years the cited year may differ from the
#'   retrieved record before the year is flagged. The default (1) allows a
#'   one-year difference, because the online-first and print publication years
#'   of an article routinely differ by a year. Set to 0 to require an exact
#'   match, or higher to be more lenient.
#' @param suggest_score for references with no DOI, the minimum CrossRef
#'   relevance score for a DOI found by title search to be offered as a
#'   suggested DOI. Low-scoring matches are usually the wrong paper, so they are
#'   not suggested. Raise to be more conservative, lower to suggest more.
#'
#' @returns report list
ref_accuracy <- function(paper, max_authors = 6, title_similarity = 0.7,
                         min_mismatches = 1, year_tolerance = 1,
                         suggest_score = 70) {
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

  cols <- c("paper_id", "bib_id", "doi", "title", "year", "authors",
            "container")
  ref_table <- ref_table(paper)
  ref_table$doi <- NULL
  # left join so every reference is kept, including those with no CrossRef
  # record. A reference that printed a DOI which does not resolve produces no
  # bib_match row; an inner join would silently drop it (and lose its cited
  # DOI), mislabelling it as a reference without a DOI.
  table <- dplyr::left_join(
    bib[, cols], bib_match[, cols],
    by = c("paper_id", "bib_id"),
    suffix = c(".orig", ".match")
  ) |>
    dplyr::left_join(ref_table, by = c("paper_id", "bib_id")) |>
    # carry the CrossRef relevance score, used to decide whether a DOI found by
    # title search is trustworthy enough to suggest for a reference with no DOI
    dplyr::left_join(bib_match[, c("paper_id", "bib_id", "score")],
                     by = c("paper_id", "bib_id"))

  # DOI: only flag when the reference itself printed a DOI that differs from
  # the matched record. A blank reference DOI is not a mismatch (the citer just
  # did not include one, and CrossRef supplied it).
  table$doi_mismatch <- !is.na(table$doi.orig) & nzchar(table$doi.orig) &
    tolower(table$doi.orig) != tolower(table$doi.match)

  # year: only flag when more than `year_tolerance` years apart (online-first
  # vs print publication routinely differ by a year and is not an incoherence)
  year_diff <- abs(as.numeric(table$year.orig) - as.numeric(table$year.match))
  table$year_mismatch <- !is.na(year_diff) & year_diff > year_tolerance

  # fold accented Latin letters to plain ASCII so that e.g. "Gredebäck" and
  # "Gredeback" are treated as the same name and diacritics never cause a
  # mismatch on their own. Done in base R (no locale-dependent iconv): multi-
  # character cases first, then a 1:1 chartr table built so the from/to strings
  # cannot drift out of sync.
  accent_map <- c(
    a = "àáâãäåāăą",
    c = "çćčĉċ", d = "ďð",
    e = "èéêëēėęě",
    g = "ğĝġģ", i = "ìíîïįĩ",
    l = "ĺļľ", n = "ñńňņ",
    o = "òóôõöōőŏ",
    r = "ŕřŗ", s = "śšşŝș",
    t = "ťţ", u = "ùúûüūůűŭ",
    y = "ýÿŷ", z = "źżž"
  )
  accent_from <- paste(accent_map, collapse = "")
  accent_to <- paste(mapply(\(base, ch) strrep(base, nchar(ch)),
                            names(accent_map), accent_map), collapse = "")
  deaccent <- \(x) {
    x <- gsub("ß", "ss", x)   # eszett
    x <- gsub("æ", "ae", x)   # ae ligature
    x <- gsub("œ", "oe", x)   # oe ligature
    x <- gsub("đ", "d", x)    # d with stroke
    x <- gsub("ł", "l", x)    # l with stroke
    x <- gsub("ø", "o", x)    # o with stroke
    x <- gsub("ı", "i", x)    # dotless i
    chartr(accent_from, accent_to, x)
  }

  # clean up text to prevent irrelevant mismatches
  clean <- \(x) {
    tolower(x) |>
      gsub("</?[a-z]+>", "", x = _) |>
      deaccent() |>                              # fold diacritics
      gsub("\\p{Pd}", "", x = _, perl = TRUE) |> # remove dashes
      gsub("\\s+", " ", x = _) |>
      gsub("[\u2018\u2019\u201A\u201B\u0060]", "'", x = _) |>
      # make all single quotes, even doubles
      gsub("[\"\u201C\u201D\u201E\u201F]", "'", x = _) |>
      gsub("\\.\\s*$", "", x = _) # remove . at end
  }

  # journal/container: flag when the cited journal disagrees with CrossRef.
  # Skip when either side is missing (NA container is unknown, not a mismatch).
  # Journals are routinely abbreviated (ISO-4: "J. Pers. Soc. Psychol." for
  # "Journal of Personality and Social Psychology"), so a plain string compare
  # over-flags. We drop stop-words, then accept a name as coherent if its
  # significant tokens match the other name's tokens in sequence, allowing each
  # abbreviated token to be a prefix of the full token.
  journal_tokens <- \(x) {
    x <- clean(x)
    x <- gsub("&amp;|&", " and ", x)
    x <- gsub("[^a-z ]", " ", x)
    toks <- strsplit(trimws(gsub("\\s+", " ", x)), " ")[[1]]
    toks[!toks %in% c("of","and","the","for","in","on","a","an","de","") &
           nchar(toks) > 0]
  }
  # do the (possibly abbreviated) tokens of `a` match, in order, a subsequence-
  # by-prefix of `b`'s tokens?
  journal_coherent <- \(a, b) {
    ta <- journal_tokens(a); tb <- journal_tokens(b)
    if (!length(ta) || !length(tb)) return(TRUE)        # unknown, don't flag
    if (length(ta) > length(tb)) { tmp <- ta; ta <- tb; tb <- tmp }
    j <- 1L
    for (tok in ta) {
      hit <- FALSE
      while (j <= length(tb)) {
        if (startsWith(tb[j], tok) || startsWith(tok, tb[j])) {
          hit <- TRUE; j <- j + 1L; break
        }
        j <- j + 1L
      }
      if (!hit) return(FALSE)
    }
    TRUE
  }
  table$container_mismatch <- mapply(\(a, b) {
    if (is.na(a) || is.na(b) || !nzchar(trimws(a)) || !nzchar(trimws(b)))
      return(FALSE)
    !journal_coherent(a, b)
  }, table$container.orig, table$container.match, USE.NAMES = FALSE)

  # title check: compare on character similarity rather than exact equality, so
  # that formatting noise (footnote markers, em-dash spacing, colons/question
  # marks, line-break word splits like "Proba bilistic") does not flag a real
  # title, while a genuinely different title still does. norm_title strips tags,
  # footnote markers and punctuation, folds dashes to spaces and diacritics.
  norm_title <- \(x) {
    x <- clean(x)                                 # lowercases, folds diacritics
    x <- gsub("<sup>.*?</sup>", "", x)            # footnote superscripts
    x <- gsub("</?[a-z]+>", "", x)                # any other tags
    x <- gsub("[^a-z0-9]", "", x)                 # keep only alphanumerics
    x
  }
  table$title_mismatch <- {
    a <- norm_title(table$title.orig)
    b <- norm_title(table$title.match)
    charsim <- mapply(\(x, y) {
      if (is.na(x) || is.na(y) || !nzchar(x) || !nzchar(y)) return(NA_real_)
      1 - utils::adist(x, y)[1, 1] / max(nchar(x), nchar(y))
    }, a, b, USE.NAMES = FALSE)

    # also accept when the record title appears verbatim in the reference text
    # (handles cases where GROBID failed to split the title out of the citation)
    clean_text <- clean(table$text)
    match_clean <- clean(table$title.match)
    in_text <- mapply(\(pattern, x) !is.na(pattern) && nzchar(pattern) &&
                        grepl(pattern, x, fixed = TRUE),
                      match_clean, clean_text, USE.NAMES = FALSE)

    !is.na(charsim) & charsim < title_similarity & !in_text
  }

  # author check: do the leading author surnames from the retrieved record
  # appear in the cited author list? Only the first `max_authors` are required,
  # because author lists are commonly truncated with "et al." (and GROBID often
  # drops the tail of long lists), so a missing trailing author is not evidence
  # of an incoherent citation. Diacritics are ignored (handled by clean()).
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
       if (length(l) == 0 || all(is.na(l))) return(FALSE)  # nothing to check
       l <- utils::head(l[!is.na(l)], max_authors)
       found <- sapply(l, \(x) grepl(clean(x), clean(o), fixed = TRUE))
       !all(found)
     }, last_names, table$authors.orig)
  }

  # tier: how did we get the record we are checking against?
  #  "provided"   - the reference printed its own DOI and CrossRef returned a
  #                 record for it; we compare the cited details against that
  #                 exact record. Firm.
  #  "unresolved" - the reference printed a DOI, but CrossRef returned no record
  #                 for it, so the cited DOI could not be found. Flagged.
  #                 add_bib_match() warns separately if any lookup failed for
  #                 network reasons, so a missing record here is taken to mean
  #                 the DOI genuinely does not resolve, not a connection problem.
  #  "crossref"   - the reference had no DOI and CrossRef supplied one via fuzzy
  #                 title/author search; the match may be wrong, so we do not
  #                 flag it.
  #  "none"       - no DOI at all.
  has_own_doi <- !is.na(table$doi.orig) & nzchar(table$doi.orig)
  has_record  <- !is.na(table$title.match) & nzchar(table$title.match)
  table$tier <- dplyr::case_when(
    has_own_doi & has_record   ~ "provided",
    has_own_doi                ~ "unresolved",
    has_record                 ~ "crossref",
    .default                   = "none"
  )
  # no_match: the reference could not be matched to a retrieved record (kept for
  # downstream modules such as ref_summary that group on it)
  table$no_match <- !has_record

  # incoherence: cited details disagree with the record the reference's OWN DOI
  # points to. We only flag references in the "provided" tier (the reference
  # printed its own DOI), because then we are comparing against the exact record
  # the author cited. References with no DOI are not flagged: the only record we
  # have for them was found by a CrossRef title search, which is often the wrong
  # paper, so a "mismatch" would be CrossRef's error, not the citation's. Those
  # are reported separately as references that could not be checked.
  # Journal, year and DOI are reliable signals and flag on their own; title and
  # author mismatches also come from PDF parsing noise, so they only count when
  # at least `min_mismatches` of them disagree.
  strong <- (table$doi_mismatch %in% TRUE) |
            (table$year_mismatch %in% TRUE) |
            (table$container_mismatch %in% TRUE)
  n_parsing_mismatch <- (table$title_mismatch %in% TRUE) +
                        (table$author_mismatch %in% TRUE)
  # a reference is incoherent when it printed its own DOI and either that DOI
  # could not be found (tier "unresolved") or the retrieved record disagrees
  # with the cited details (tier "provided").
  table$incoherent <- (table$tier == "unresolved") |
    ((table$tier == "provided") &
       (strong | n_parsing_mismatch >= min_mismatches))

  # traffic_light ----
  tl <- if (any(table$incoherent %in% TRUE)) "yellow" else "green"

  # summary_table ----
  # references that printed a DOI (tiers "provided" and "unresolved") are
  # checked; references with no DOI at all are reported separately so the user
  # can add one.
  table$no_doi <- table$tier == "none"
  summary_table <- dplyr::summarise(table,
    .by = paper_id,
    refs_checked = sum(tier %in% c("provided", "unresolved"), na.rm = TRUE),
    incoherent   = sum(incoherent, na.rm = TRUE),
    no_doi       = sum(no_doi, na.rm = TRUE)
  )

  # summary_text
  n_checked  <- sum(table$tier %in% c("provided", "unresolved"), na.rm = TRUE)
  n_inc      <- sum(table$incoherent, na.rm = TRUE)
  n_nodoi    <- sum(table$no_doi, na.rm = TRUE)
  summary_text <- sprintf(
    "We checked the %d reference%s that supplied a DOI against the record that DOI points to, and found %d incoherent reference%s to check for parsing errors or mistakes. %d reference%s had no DOI and could not be checked. Incoherent references are mostly PDF parsing errors (we are working on improving reference parsing).%s",
    n_checked, plural(n_checked), n_inc, plural(n_inc),
    n_nodoi, plural(n_nodoi),
    if (n_nodoi > 0) " Adding a DOI to every reference that has one lets the other reference checks (retraction, PubPeer, and replication) run on them too." else ""
  )

  guidance <- "The references below supplied a DOI, but one or more of the cited details (title, authors, journal, or year) does not match the record that DOI points to. Such an incoherent is most often an error in reading the reference from the PDF, but it could be a mistake, ar an AI generated reference. Check each against the original source. Incoherent references are mostly PDF parsing errors (we are working on improving reference parsing)."

  if (n_inc == 0) guidance <- ""

  # report ----

  # render the surname list of a CrossRef author record (a data.frame of
  # given/family) as a plain "Family1, Family2, ..." string
  author_string <- function(a) {
    if (is.null(a) || !is.data.frame(a) || nrow(a) == 0) return(NA_character_)
    fam <- a$family[!is.na(a$family)]
    if (!length(fam)) return(NA_character_)
    paste(utils::head(fam, max_authors), collapse = ", ")
  }

  # for one reference, build the "what is wrong" cell: each mismatching field is
  # shown as the cited value struck through in red followed by the value from
  # the record, so the discrepancy is visible at a glance.
  strike <- function(x) paste0(
    "<span style=\"color:#c00;text-decoration:line-through\">", x, "</span>")
  correction <- function(label, cited, record) {
    if (is.na(cited) || !nzchar(as.character(cited))) cited <- "(missing)"
    sprintf("<b>%s:</b> %s &rarr; %s", label, strike(cited), record)
  }
  discrepancies <- function(r) {
    # an unresolved cited DOI has no record to compare against
    if (r$tier == "unresolved") {
      return(sprintf(
        "<b>DOI:</b> the cited DOI %s could not be found in CrossRef",
        strike(r$doi.orig)))
    }
    parts <- c(
      if (isTRUE(r$title_mismatch))
        correction("title", r$title.orig, r$title.match),
      if (isTRUE(r$author_mismatch))
        correction("authors", r$authors.orig, author_string(r$authors.match[[1]])),
      if (isTRUE(r$container_mismatch))
        correction("journal", r$container.orig, r$container.match),
      if (isTRUE(r$year_mismatch))
        correction("year", r$year.orig, r$year.match),
      if (isTRUE(r$doi_mismatch))
        correction("DOI", r$doi.orig, r$doi.match)
    )
    paste(parts, collapse = "<br>")
  }

  ## incoherent references (each printed its own DOI) ----
  inc_rows <- table[table$incoherent %in% TRUE, ]
  incoherent_report <- NULL
  if (nrow(inc_rows) > 0) {
    # link to the retrieved record; for an unresolved DOI there is no record, so
    # show the cited (non-resolving) DOI instead
    rec_doi <- ifelse(is.na(inc_rows$doi.match) | !nzchar(inc_rows$doi.match),
                      inc_rows$doi.orig, inc_rows$doi.match)
    out <- data.frame(
      Reference = inc_rows$text,
      `What is wrong (cited → record)` =
        vapply(seq_len(nrow(inc_rows)), \(i) discrepancies(inc_rows[i, ]),
               character(1)),
      Record = link(paste0("https://doi.org/", rec_doi), rec_doi),
      check.names = FALSE
    )
    incoherent_report <- scroll_table(out, 5, colwidths = c(.45, .4, .15))
  }

  ## references without a DOI (could not be checked) ----
  nodoi_rows <- table[table$no_doi %in% TRUE & !is.na(table$text), ]
  nodoi_report <- NULL
  if (nrow(nodoi_rows) > 0) {
    # only offer a suggested DOI when the CrossRef title-search match scored
    # high enough to be trustworthy; a low score is usually the wrong paper.
    suggested <- ifelse(
      !is.na(nodoi_rows$score) & nodoi_rows$score >= suggest_score &
        !is.na(nodoi_rows$doi.match) & nzchar(nodoi_rows$doi.match),
      link(paste0("https://doi.org/", nodoi_rows$doi.match), nodoi_rows$doi.match),
      ""
    )
    out <- data.frame(
      Reference = nodoi_rows$text,
      `Suggested DOI` = suggested,
      check.names = FALSE
    )
    nodoi_report <- scroll_table(out, 5, colwidths = c(.7, .3))
  }

  report <- c(
    guidance,
    if (!is.null(incoherent_report))
      c("### Incoherent references\n",
        "The cited details of these references do not match the record their DOI points to.\n",
        incoherent_report),
    if (!is.null(nodoi_report))
      c("### References without a DOI\n",
        "These references have no DOI, so they could not be checked here, and they also cannot be checked by the other reference modules (retraction, PubPeer, and replication checks all rely on the DOI). We searched CrossRef for a matching DOI; where a confident match was found it is suggested below. We recommend adding a DOI to every reference that has one: it lets these checks run and makes each cited work easier to find and verify.\n",
        nodoi_report)
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
