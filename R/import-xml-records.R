# Publisher XML: metadata records ----
#
# Some full-text links give a record of the article's metadata instead of its
# text: the XML a publisher sends to PubMed (<ArticleSet>), to Crossref
# (<doi_batch>), to DOAJ (<records>), an EndNote export (<xml><records>), and
# the Islamic World Science Citation Center's format (<XML><ISCJOURNAL>).
# They give the title, authors with their affiliations, journal, volume,
# pages, date, DOI, keywords, the abstract (the paper's only text) and often
# the references. A file can hold several articles; the first is read, with a
# warning.

#' Text of the first node an XPath finds, "" when none
#' @noRd
.rec1 <- function(node, xpath) {
  if (is.null(node) || inherits(node, "xml_missing")) return("")
  n <- xml2::xml_find_first(node, xpath)
  if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
}

#' The first record of a file, with a warning when there are more
#' @noRd
.rec_first <- function(records, what) {
  if (!length(records)) stop("the file holds no ", what, call. = FALSE)
  if (length(records) > 1) {
    warning("the file holds ", length(records), " ", what, "s; the first is read", call. = FALSE)
  }
  records[[1]]
}

#' A record's abstract paragraphs as markup
#' @noRd
.rec_abstract <- function(node) {
  if (is.null(node) || inherits(node, "xml_missing")) return(character(0))
  ps <- xml2::xml_find_all(node, ".//p | .//AbstractText")
  txt <- if (length(ps)) vapply(ps, \(p) {
    lab <- xml2::xml_attr(p, "Label")
    paste0(if (!is.na(lab)) paste0(lab, ": ") else "", .tei_squish(xml2::xml_text(p)))
  }, "") else .tei_squish(xml2::xml_text(node))
  .tei_esc(txt[nzchar(txt)])
}

#' Convert the XML a publisher sends to PubMed (<ArticleSet>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.pubmed_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  a <- .rec_first(xml2::xml_find_all(xml, "//Article"), "article")
  j <- xml2::xml_find_first(a, "./Journal")
  pd <- xml2::xml_find_first(j, "./PubDate")
  date <- paste(c(.rec1(pd, "./Year"), .rec1(pd, "./Month"), .rec1(pd, "./Day"))[nzchar(c(.rec1(pd, "./Year"), .rec1(pd, "./Month"), .rec1(pd, "./Day")))], collapse = "-")
  authors <- lapply(xml2::xml_find_all(a, "./AuthorList/Author"), \(au) {
    list(given = trimws(paste(.rec1(au, "./FirstName"), .rec1(au, "./MiddleName"))),
         family = .rec1(au, "./LastName"), literal = .rec1(au, "./CollectiveName"),
         orcid = .rec1(au, "./Identifier[@Source='ORCID']"),
         affs = vapply(xml2::xml_find_all(au, "./Affiliation | ./AffiliationInfo/Affiliation"), \(f) .tei_squish(xml2::xml_text(f)), ""))
  })
  doi <- .rec1(a, "./ELocationID[@EIdType='doi'] | ./ArticleIdList/ArticleId[@IdType='doi']")
  kw <- vapply(xml2::xml_find_all(a, ".//Object[@Type='keyword']/Param[@Name='value'] | .//Keyword"), \(k) .tei_squish(xml2::xml_text(k)), "")
  refs <- vapply(xml2::xml_find_all(a, ".//ReferenceList/Reference/Citation"), \(r) .tei_esc(.tei_squish(xml2::xml_text(r))), "")
  meta <- list(title = .rec1(a, "./ArticleTitle"), authors = authors,
               abstract = .rec_abstract(xml2::xml_find_first(a, "./Abstract")),
               keywords = kw, doi = doi, journal = .rec1(j, "./JournalTitle"), issn = .rec1(j, "./Issn"),
               publisher = .rec1(j, "./PublisherName"), volume = .rec1(j, "./Volume"), issue = .rec1(j, "./Issue"),
               fpage = .rec1(a, "./FirstPage"), lpage = .rec1(a, "./LastPage"),
               date = date, language = .rec1(a, "./Language"), refs = refs, version = "pubmed")
  .blocks_to_bibr12(xml_path, list(), meta, "pubmed")
}

#' Convert a Crossref deposit (<doi_batch>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.crossref_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  # the deposited works, whatever their type (journal_article,
  # posted_content, report-paper_metadata, content_item, peer_review...): an
  # element with a title and a DOI
  items <- xml2::xml_find_all(xml, paste0(
    "//body//*[titles and doi_data][not(ancestor::citation_list)]",
    "[not(self::journal_issue) and not(self::journal_metadata) and not(self::proceedings_metadata)]"))
  a <- .rec_first(items, "article")
  journal <- xml2::xml_find_first(a, "ancestor::journal/journal_metadata")
  issue <- xml2::xml_find_first(a, "ancestor::journal/journal_issue")
  date_of <- function(node) {
    pd <- xml2::xml_find_first(node, "./publication_date")
    p <- c(.rec1(pd, "./year"), .rec1(pd, "./month"), .rec1(pd, "./day"))
    p[2:3] <- ifelse(nzchar(p[2:3]), sprintf("%02d", suppressWarnings(as.integer(p[2:3]))), "")
    paste(p[nzchar(p) & p != "NA"], collapse = "-")
  }
  date <- date_of(a)
  if (!nzchar(date)) date <- date_of(issue)
  authors <- lapply(xml2::xml_find_all(a, "./contributors/person_name[@contributor_role='author' or not(@contributor_role)] | ./contributors/organization[@contributor_role='author']"), \(p) {
    list(given = .rec1(p, "./given_name"), family = .rec1(p, "./surname"),
         literal = if (xml2::xml_name(p) == "organization") .tei_squish(xml2::xml_text(p)) else "",
         orcid = .rec1(p, "./ORCID"),
         affs = vapply(xml2::xml_find_all(p, "./affiliation | ./affiliations//institution_name"), \(f) .tei_squish(xml2::xml_text(f)), ""))
  })
  refs <- vapply(xml2::xml_find_all(a, "./citation_list/citation"), \(c) {
    u <- .rec1(c, "./unstructured_citation")
    if (nzchar(u)) return(.tei_esc(u))
    parts <- c(.rec1(c, "./author"), .rec1(c, "./cYear"), .rec1(c, "./article_title | ./series_title | ./volume_title"),
               .rec1(c, "./journal_title"), .rec1(c, "./volume"), .rec1(c, "./first_page"))
    doi <- .rec1(c, "./doi")
    .tei_esc(paste(c(parts[nzchar(parts)], if (nzchar(doi)) paste0("https://doi.org/", doi)), collapse = ". "))
  }, "")
  lic <- .rec1(a, ".//program/license_ref | .//license_ref")
  meta <- list(title = .rec1(a, "./titles/title"), subtitle = .rec1(a, "./titles/subtitle"),
               authors = authors, abstract = .rec_abstract(xml2::xml_find_first(a, "./abstract")),
               doi = .rec1(a, "./doi_data/doi"), journal = .rec1(journal, "./full_title"),
               issn = .rec1(journal, "./issn[@media_type='electronic'] | ./issn"),
               volume = .rec1(issue, "./journal_volume/volume"), issue = .rec1(issue, "./issue"),
               fpage = .rec1(a, "./pages/first_page"), lpage = .rec1(a, "./pages/last_page"),
               date = date, license_url = lic, refs = refs[nzchar(refs)],
               language = xml2::xml_attr(a, "language") %NA% "",
               version = paste0("crossref-", xml2::xml_attr(xml2::xml_root(xml), "version") %NA% "unknown"))
  .blocks_to_bibr12(xml_path, list(), meta, "crossref")
}

#' Convert a DOAJ article record (<records>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.doaj_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  r <- .rec_first(xml2::xml_find_all(xml, "//record"), "article")
  aff_nodes <- xml2::xml_find_all(r, "./affiliationsList/affiliationName")
  aff <- stats::setNames(vapply(aff_nodes, \(f) .tei_squish(xml2::xml_text(f)), ""), xml2::xml_attr(aff_nodes, "affiliationId"))
  authors <- lapply(xml2::xml_find_all(r, "./authors/author"), \(au) {
    id <- .rec1(au, "./affiliationId")
    list(literal = .rec1(au, "./name"), email = .rec1(au, "./email"), orcid = .rec1(au, "./orcid_id"),
         affs = if (nzchar(id) && id %in% names(aff)) unname(aff[id]) else character(0))
  })
  meta <- list(title = .rec1(r, "./title"), authors = authors,
               abstract = .rec_abstract(xml2::xml_find_first(r, "./abstract")),
               keywords = vapply(xml2::xml_find_all(r, "./keywords/keyword"), \(k) .tei_squish(xml2::xml_text(k)), ""),
               doi = .rec1(r, "./doi"), journal = .rec1(r, "./journalTitle"),
               issn = if (nzchar(.rec1(r, "./eissn"))) .rec1(r, "./eissn") else .rec1(r, "./issn"),
               publisher = .rec1(r, "./publisher"), volume = .rec1(r, "./volume"), issue = .rec1(r, "./issue"),
               fpage = .rec1(r, "./startPage"), lpage = .rec1(r, "./endPage"),
               date = .rec1(r, "./publicationDate"), language = .rec1(r, "./language"), version = "doaj")
  .blocks_to_bibr12(xml_path, list(), meta, "doaj")
}

#' Convert an EndNote XML export (<xml><records>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.endnote_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  r <- .rec_first(xml2::xml_find_all(xml, "//records/record"), "record")
  authors <- lapply(xml2::xml_find_all(r, "./contributors/authors/author"), \(au) {
    a <- .tei_squish(xml2::xml_text(au))
    if (grepl(",", a)) list(family = trimws(sub(",.*$", "", a)), given = trimws(sub("^[^,]*,", "", a))) else list(literal = a)
  })
  doi <- regmatches(.rec1(r, "./electronic-resource-num"), regexpr("10\\.[0-9]{4,9}/\\S+", .rec1(r, "./electronic-resource-num")))
  pages <- strsplit(.rec1(r, "./pages"), "\\s*[-\u2013]\\s*")[[1]]
  meta <- list(title = .rec1(r, "./titles/title"), authors = authors,
               abstract = .rec_abstract(xml2::xml_find_first(r, "./abstract")),
               keywords = vapply(xml2::xml_find_all(r, "./keywords/keyword"), \(k) .tei_squish(xml2::xml_text(k)), ""),
               doi = if (length(doi)) doi else "",
               journal = if (nzchar(.rec1(r, "./periodical/full-title"))) .rec1(r, "./periodical/full-title") else .rec1(r, "./titles/secondary-title"),
               issn = .rec1(r, "./isbn"), volume = .rec1(r, "./volume"), issue = .rec1(r, "./number"),
               fpage = if (length(pages)) pages[1] else "", lpage = if (length(pages) > 1) pages[2] else "",
               date = .rec1(r, "./dates/year"), language = .rec1(r, "./language"), version = "endnote")
  .blocks_to_bibr12(xml_path, list(), meta, "endnote")
}

#' Convert the Islamic World Science Citation Center's format (<XML>
#' <ISCJOURNAL>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.isc_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  j <- xml2::xml_find_first(xml, "//ISCJOURNAL")
  a <- .rec_first(xml2::xml_find_all(xml, "//ARTICLES/ARTICLE"), "article")
  # the English parts (TitleE, NameE) when there are, else the others
  pick <- function(node, en, other) { v <- .rec1(node, en); if (nzchar(v)) v else .rec1(node, other) }
  authors <- lapply(xml2::xml_find_all(a, "./AUTHORS/AUTHOR"), \(au) {
    list(given = trimws(paste(pick(au, "./NameE", "./Name"), pick(au, "./MidNameE", "./MidName"))),
         family = pick(au, "./FamilyE", "./Family"), email = .rec1(au, "./EMAILS/Email"),
         affs = vapply(xml2::xml_find_all(au, "./Organizations/Organization"), \(f) .tei_squish(xml2::xml_text(f)), ""))
  })
  abs <- xml2::xml_find_all(a, "./ABSTRACTS/ABSTRACT")
  abs_txt <- vapply(abs, \(x) .rec1(x, "./CONTENT"), "")
  abs_txt <- abs_txt[nzchar(abs_txt)]
  # the DOI next to the article, or inside it
  doi <- .rec1(a, "./DOI")
  if (!nzchar(doi)) doi <- .rec1(a, "preceding-sibling::DOI[1]")
  meta <- list(title = pick(a, "./TitleE", "./TitleF"), authors = authors,
               abstract = if (length(abs_txt)) .tei_esc(gsub("<[^>]+>", " ", abs_txt[1])) else character(0),
               keywords = vapply(xml2::xml_find_all(a, "./KEYWORDS/KEYWORD/KeyText"), \(k) .tei_squish(xml2::xml_text(k)), ""),
               doi = doi, volume = .rec1(j, "./VOL"), issue = .rec1(j, "./NO"), date = .rec1(j, "./YEAR"),
               fpage = .rec1(a, "./PAGES/PAGE/FPAGE"), lpage = .rec1(a, "./PAGES/PAGE/TPAGE"),
               refs = vapply(xml2::xml_find_all(a, "./REFRENCES/REFRENCE/REF | ./REFERENCES/REFERENCE/REF"), \(r) .tei_esc(.tei_squish(xml2::xml_text(r))), ""),
               version = "isc")
  .blocks_to_bibr12(xml_path, list(), meta, "isc")
}
