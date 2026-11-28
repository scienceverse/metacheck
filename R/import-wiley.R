# Publisher XML: Wiley ----
#
# Wiley delivers full text (the text-mining links it registers with CrossRef,
# onlinelibrary.wiley.com/doi/full-xml/<DOI>) in its own XML, WileyML: a
# <component> in the namespace http://www.wiley.com/namespaces/wiley. The
# metadata is in <header>: three <publicationMeta> elements (the journal, the
# issue and the article itself) and <contentMeta> (title, authors,
# affiliations, abstracts, keywords, funding, supporting information). The
# text is in <body>: sections, the bibliography, notes and appendices. The
# journals of the societies whose journals Wiley publishes on Wiley Online
# Library are delivered in the same format. As for Elsevier, the article is
# translated into JATS, element by element (section is a JATS <sec>, a link
# to an id an <xref>, tabular a <table-wrap>, bib a <ref>), and read as JATS
# from there (see R/import-jats.R).

#' A Wiley element's id (its xml:id, read as xml-id by `.elsevier_read()`)
#' @noRd
.wly_id <- function(node) {
  id <- xml2::xml_attr(node, "xml-id")
  id[is.na(id)] <- xml2::xml_attr(node, "id")[is.na(id)]
  id
}

#' The content of an element as markup, without the element's own tags
#' @noRd
.wly_inner <- function(node) {
  if (inherits(node, "xml_missing")) return("")
  paste(vapply(xml2::xml_contents(node), as.character, character(1)), collapse = "")
}

#' A Wiley date (2020-04-16, 2021-10) as a JATS date element
#' @noRd
.wly_date <- function(tag, type_attr, type, iso) {
  if (is.na(iso) || !grepl("^[0-9]{4}", iso)) return("")
  p <- strsplit(iso, "-", fixed = TRUE)[[1]]
  sprintf("<%s %s=\"%s\">%s%s<year>%s</year></%s>", tag, type_attr, type,
          if (length(p) >= 3) sprintf("<day>%s</day>", p[3]) else "",
          if (length(p) >= 2) sprintf("<month>%s</month>", p[2]) else "",
          p[1], tag)
}

#' Rename Wiley's text elements to their JATS names, in place
#'
#' @param node the component (prefixes removed)
#' @param kind named character vector: element id -> JATS ref-type
#' @noRd
.wly_rename <- function(node, kind) {
  ren <- function(xpath, name) {
    for (n in xml2::xml_find_all(node, xpath)) xml2::xml_set_name(n, name)
  }
  # links to an element of the article are cross-references, the others
  # web links
  for (x in xml2::xml_find_all(node, ".//link")) {
    href <- xml2::xml_attr(x, "href")
    if (is.na(href)) href <- ""
    if (startsWith(href, "#")) {
      rid <- substring(href, 2)
      type <- unname(kind[rid])
      xml2::xml_set_name(x, "xref")
      xml2::xml_set_attr(x, "rid", rid)
      xml2::xml_set_attr(x, "href", NULL)
      if (!is.na(type)) xml2::xml_set_attr(x, "ref-type", type)
    } else {
      xml2::xml_set_name(x, "ext-link")
    }
  }
  ren(".//url", "ext-link")
  # Wiley links only a citation's year ("(Smith et al., <link>2020</link>;
  # Jones, <link>2019</link>)"): the citation's text is its authors and year,
  # as Grobid reads it from a PDF. The names written since the opening
  # bracket or the last semicolon move into the citation; a year without
  # names before it ("2020a, <link>2020b</link>") stays as it is.
  for (x in xml2::xml_find_all(node, ".//xref[@ref-type='bibr'][not(*)]")) {
    year <- xml2::xml_text(x)
    if (!grepl("^\\s*(1[5-9]|20)[0-9]{2}[a-z]?\\s*$", year)) next
    prev <- xml2::xml_find_first(x, "preceding-sibling::node()[1]")
    if (inherits(prev, "xml_missing") || xml2::xml_type(prev) != "text") next
    t <- xml2::xml_text(prev)
    if (!grepl("[(;[]", t)) next
    names <- sub("^.*[(;[]", "", t)
    if (!grepl("[[:upper:]]", names) || nchar(names) > 300) next
    # the space after the semicolon stays in the sentence
    trimmed <- sub("^\\s+", "", names)
    xml2::xml_text(prev) <- paste0(substr(t, 1, nchar(t) - nchar(names)),
                                   substr(names, 1, nchar(names) - nchar(trimmed)))
    xml2::xml_text(x) <- paste0(trimmed, year)
  }
  # formulas: MathML in the text is an inline formula; a displayed one keeps
  # its MathML when it has it (many are only an image)
  for (m in xml2::xml_find_all(node, ".//math[not(ancestor::displayedItem)]")) {
    xml2::xml_add_parent(m, "inline-formula")
  }
  ren(".//displayedItem", "disp-formula")
  xml2::xml_remove(xml2::xml_find_all(node, paste0(
    ".//blockFixed[@type='graphic'] | .//mediaResourceGroup | ",
    ".//mediaResource[not(parent::supportingInfoItem)]")))
  ren(".//blockFixed", "disp-quote")
  ren(".//inlineGraphic", "inline-graphic")
  # formatting: italic, foreign-language italic, bold, small and full capitals
  ren(".//i | .//fi", "italic")
  ren(".//b", "bold")
  ren(".//fc | .//sc", "sc")
  # a line of lineated text (program code, an address) ends with a space
  for (l in xml2::xml_find_all(node, ".//lineatedText/line")) {
    sp <- xml2::xml_add_child(l, "x")
    xml2::xml_text(sp) <- " "
  }
  ren(".//lineatedText | .//line", "x")
  ren(".//computerCode", "code")
  for (l in xml2::xml_find_all(node, ".//list")) {
    style <- xml2::xml_attr(l, "style")
    xml2::xml_set_attr(l, "list-type", if (identical(style, "bulleted")) "bullet" else "order")
  }
  ren(".//listItem", "list-item")
  # a hypothesis or proposition: its name ("H1.1") starts its first paragraph
  for (ms in xml2::xml_find_all(node, ".//mathStatement")) {
    t <- xml2::xml_find_first(ms, "./title")
    p <- xml2::xml_find_first(ms, "./p")
    if (!inherits(t, "xml_missing") && !inherits(p, "xml_missing")) {
      xml2::xml_replace(p, xml2::read_xml(sprintf("<p><bold>%s</bold> %s</p>",
                                                  .tei_esc(.els_text(t)), .wly_inner(p))))
      xml2::xml_remove(t)
    }
    xml2::xml_set_name(ms, "statement")
  }
  # a box: its title is the caption; its sections are parts of the box
  for (f in xml2::xml_find_all(node, ".//feature")) {
    title <- xml2::xml_find_first(f, "./titleGroup/title[@type='main'] | ./title[@type='main']")
    xml2::xml_remove(xml2::xml_find_all(f, "./titleGroup | ./title"))
    if (!inherits(title, "xml_missing")) {
      cap <- xml2::xml_add_child(f, "caption", .where = 0)
      tt <- xml2::xml_add_child(cap, "title")
      xml2::xml_text(tt) <- .els_text(title)
    }
    ren(".//feature//section", "x")
    xml2::xml_set_name(f, "boxed-text")
  }
  # a paragraph that only holds a list, quotation or formula is that block
  blocks <- c("list", "disp-quote", "disp-formula", "statement", "code", "boxed-text",
              "tabular", "tabularFixed", "figure")
  for (p in xml2::xml_find_all(node, ".//p")) {
    kids <- xml2::xml_children(p)
    txt <- xml2::xml_text(xml2::xml_find_all(p, "./text()"))
    if (length(kids) && all(xml2::xml_name(kids) %in% blocks) && !any(nzchar(trimws(txt)))) {
      xml2::xml_set_name(p, "x")
    }
  }
  xml2::xml_remove(xml2::xml_find_all(node, ".//p[@onlyChannels='print']"))
  invisible(node)
}

#' A Wiley figure as a JATS `<fig>` string
#' @noRd
.wly_figure <- function(f) {
  cap <- xml2::xml_find_first(f, "./caption")
  cap_xml <- if (inherits(cap, "xml_missing")) "" else if (length(xml2::xml_find_all(cap, "./p"))) {
    as.character(cap)
  } else {
    paste0("<caption><p>", .wly_inner(cap), "</p></caption>")
  }
  sprintf("<fig id=\"%s\">%s%s</fig>", .tei_attr(.wly_id(f) %NA% ""),
          if (nzchar(.els_text(f, "./label"))) sprintf("<label>%s</label>", .tei_esc(.els_text(f, "./label"))) else "",
          cap_xml)
}

#' A Wiley table (tabular, or tabularFixed without a number) as a JATS
#' `<table-wrap>` string
#' @noRd
.wly_table <- function(tb) {
  title <- xml2::xml_find_first(tb, "./titleGroup/title[@type='main'] | ./title[@type='main']")
  cap <- xml2::xml_find_first(tb, "./caption")
  cap_xml <- paste0(
    if (!inherits(title, "xml_missing")) paste0("<p>", .wly_inner(title), "</p>") else "",
    if (!inherits(cap, "xml_missing")) {
      if (length(xml2::xml_find_all(cap, "./p"))) .wly_inner(cap) else paste0("<p>", .wly_inner(cap), "</p>")
    } else "")
  tables <- vapply(xml2::xml_find_all(tb, ".//tgroup"), \(tg) {
    .els_cals_to_xhtml(tg)
    as.character(tg)
  }, character(1))
  notes <- vapply(xml2::xml_find_all(tb, ".//noteGroup/note"), \(n) {
    xml2::xml_set_name(n, "fn")
    as.character(n)
  }, character(1))
  sprintf("<table-wrap id=\"%s\">%s%s%s%s</table-wrap>",
          .tei_attr(.wly_id(tb) %NA% ""),
          if (nzchar(.els_text(tb, "./label"))) sprintf("<label>%s</label>", .tei_esc(.els_text(tb, "./label"))) else "",
          if (nzchar(cap_xml)) paste0("<caption>", cap_xml, "</caption>") else "",
          paste(tables, collapse = ""),
          if (length(notes)) paste0("<table-wrap-foot>", paste(notes, collapse = ""), "</table-wrap-foot>") else "")
}

#' A Wiley reference (bib) as a JATS `<ref>` string
#' @noRd
.wly_ref <- function(b) {
  esc <- .tei_esc
  wrap <- function(tag, x) if (nzchar(x)) sprintf("<%s>%s</%s>", tag, esc(x), tag) else ""
  id <- .tei_attr(.wly_id(b) %NA% "")
  cit <- xml2::xml_find_first(b, "./citation")
  if (inherits(cit, "xml_missing")) cit <- b
  printed <- .tei_squish(xml2::xml_text(cit))
  one <- function(xpath) .els_text(cit, xpath)
  person <- function(p) {
    if (xml2::xml_name(p) == "groupName") return(wrap("collab", .tei_squish(xml2::xml_text(p))))
    family <- paste(c(.els_text(p, "./familyNamePrefix"), .els_text(p, "./familyName")), collapse = " ")
    paste0("<name>", wrap("surname", trimws(family)), wrap("given-names", .els_text(p, "./givenNames")),
           wrap("suffix", .els_text(p, "./nameSuffix")), "</name>")
  }
  group <- function(xpath, type) {
    p <- xml2::xml_find_all(cit, xpath)
    if (!length(p)) return("")
    sprintf("<person-group person-group-type=\"%s\">%s</person-group>", type,
            paste(vapply(p, person, character(1)), collapse = ""))
  }
  type <- xml2::xml_attr(cit, "type")
  ptype <- switch(type %NA% "", journal = "journal", book = "book", dataRelates = "data", "other")
  title <- one("./articleTitle")
  chapter <- one("./chapterTitle")
  data_title <- one("./dataTitle")
  source <- one("./journalTitle")
  if (!nzchar(source)) source <- one("./bookTitle")
  if (!nzchar(source)) source <- one("./otherTitle")
  if (!nzchar(source)) source <- one("./dataRepository")
  if (!nzchar(source)) source <- one("./source")
  year <- xml2::xml_attr(xml2::xml_find_first(cit, "./pubYear"), "year")
  if (is.na(year)) year <- one("./pubYear")
  # the DOI: from a doi.org link, else written in the reference
  links <- xml2::xml_attr(xml2::xml_find_all(cit, "./url"), "href")
  links <- links[!is.na(links)]
  is_doi <- grepl("^https?://(dx\\.)?doi\\.org/10\\.", links, ignore.case = TRUE)
  doi <- if (any(is_doi)) sub("^https?://(dx\\.)?doi\\.org/", "", links[is_doi][1], ignore.case = TRUE) else ""
  if (!nzchar(doi)) {
    hit <- regmatches(printed, regexpr("10\\.[0-9]{4,}/[^[:space:]]+", printed))
    if (length(hit)) doi <- sub("[.,;]+$", "", hit)
  }
  # Wiley prints hyphens as U+2010, also inside DOIs
  doi <- gsub(paste0("[", intToUtf8(c(0x2010, 0x2011)), "]"), "-", doi)
  urls <- links[!is_doi]
  cit_xml <- paste0(
    sprintf("<element-citation publication-type=\"%s\">", ptype),
    group("./author | ./groupName", "author"),
    group("./editor", "editor"),
    wrap("article-title", title), wrap("chapter-title", chapter), wrap("data-title", data_title),
    wrap("source", source), wrap("edition", one("./edition")),
    wrap("series", one("./bookSeriesTitle")), wrap("year", year),
    wrap("volume", one("./vol")), wrap("issue", one("./issue")),
    wrap("fpage", one("./pageFirst")), wrap("lpage", one("./pageLast")),
    wrap("elocation-id", one("./eLocator")),
    wrap("publisher-loc", one("./publisherLoc")), wrap("publisher-name", one("./publisherName")),
    if (nzchar(doi)) sprintf("<pub-id pub-id-type=\"doi\">%s</pub-id>", esc(doi)) else "",
    paste0(vapply(xml2::xml_find_all(cit, "./accessionId"), \(a) {
      sprintf("<pub-id pub-id-type=\"accession\">%s</pub-id>", esc(.tei_squish(xml2::xml_text(a))))
    }, character(1)), collapse = ""),
    paste0(sprintf("<ext-link href=\"%s\">%s</ext-link>", .tei_attr(urls), esc(urls)), collapse = ""),
    "</element-citation>",
    if (nzchar(printed)) sprintf("<mixed-citation specific-use=\"source-text\">%s</mixed-citation>", esc(printed)) else ""
  )
  sprintf("<ref id=\"%s\">%s</ref>", id, cit_xml)
}

#' Translate a Wiley XML article into JATS
#'
#' @param xml the article, read with `.elsevier_read()` (prefixes removed)
#' @returns the JATS article as a string
#' @noRd
.wiley_to_jats <- function(xml) {
  comp <- xml2::xml_root(xml)
  header <- xml2::xml_find_first(comp, "./header")
  body <- xml2::xml_find_first(comp, "./body")
  product <- xml2::xml_find_first(header, "./publicationMeta[@level='product']")
  part <- xml2::xml_find_first(header, "./publicationMeta[@level='part']")
  unit <- xml2::xml_find_first(header, "./publicationMeta[@level='unit']")
  cm <- xml2::xml_find_first(header, "./contentMeta")
  esc <- .tei_esc
  wrap <- function(tag, x) if (nzchar(x)) sprintf("<%s>%s</%s>", tag, esc(x), tag) else ""
  txt <- function(node, xpath) .els_text(node, xpath)
  attr1 <- function(node, xpath, a) {
    n <- if (inherits(node, "xml_missing")) node else xml2::xml_find_first(node, xpath)
    if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_attr(n, a)
  }

  # the kind of each id a link can point to ----
  kind_of <- function(xpath, type) {
    ids <- .wly_id(xml2::xml_find_all(comp, xpath))
    stats::setNames(rep(type, length(ids)), ids)[!is.na(ids)]
  }
  kind <- c(kind_of("//bib", "bibr"), kind_of("//figure", "fig"),
            kind_of("//tabular | //tabularFixed", "table"),
            kind_of("//tabular//note | //tabularFixed//note", "table-fn"),
            kind_of("//body//note[not(ancestor::tabular) and not(ancestor::tabularFixed)]", "fn"),
            kind_of("//displayedItem", "disp-formula"), kind_of("//section", "sec"),
            kind_of("//supportingInfoItem", "supplementary-material"),
            kind_of("//feature", "boxed-text"), kind_of("//appendix", "app"))

  # people and affiliations ----
  contribs <- vapply(xml2::xml_find_all(cm, "./creators/creator"), \(cr) {
    role <- xml2::xml_attr(cr, "creatorRole") %NA% "author"
    rid <- strsplit(gsub("#", "", xml2::xml_attr(cr, "affiliationRef") %NA% ""), "\\s+")[[1]]
    rid <- rid[nzchar(rid)]
    orcid <- attr1(cr, "./idGroup/id[@type='orcid']", "value")
    group <- txt(cr, "./groupName")
    name <- if (nzchar(group)) wrap("collab", group) else {
      family <- trimws(paste(txt(cr, "./personName/familyNamePrefix"), txt(cr, "./personName/familyName")))
      paste0("<name>", wrap("surname", family), wrap("given-names", txt(cr, "./personName/givenNames")),
             wrap("suffix", txt(cr, "./personName/nameSuffix")), "</name>")
    }
    # editor -> "Editor", guestEditor -> "Guest editor"
    role_name <- if (role == "author") "" else {
      r <- tolower(gsub("([a-z])([A-Z])", "\\1 \\2", role))
      paste0(toupper(substr(r, 1, 1)), substring(r, 2))
    }
    paste0(
      sprintf("<contrib contrib-type=\"%s\"%s>", if (role == "author") "author" else "editor",
              if (identical(xml2::xml_attr(cr, "corresponding"), "yes")) " corresp=\"yes\"" else ""),
      if (!is.na(orcid)) sprintf("<contrib-id contrib-id-type=\"orcid\">%s</contrib-id>", esc(orcid)) else "",
      name, wrap("email", txt(cr, "./contactDetails/email")),
      if (nzchar(role_name)) wrap("role", role_name) else "",
      paste0(sprintf("<xref ref-type=\"aff\" rid=\"%s\"/>", .tei_attr(rid)), collapse = ""),
      "</contrib>")
  }, character(1))
  has_editor <- length(xml2::xml_find_all(cm, "./creators/creator[@creatorRole and not(@creatorRole='author')]")) > 0
  affs <- vapply(xml2::xml_find_all(cm, "./affiliationGroup/affiliation"), \(a) {
    parts <- vapply(xml2::xml_find_all(a, ".//*[not(*)]"), \(p) .tei_squish(xml2::xml_text(p)), character(1))
    sprintf("<aff id=\"%s\">%s</aff>", .tei_attr(.wly_id(a) %NA% ""),
            esc(paste(parts[nzchar(parts)], collapse = ", ")))
  }, character(1))

  # notes about the article in the header: the handling editor, equal
  # contributions, funding ----
  author_notes <- character(0)
  funding_stmt <- character(0)
  corr <- xml2::xml_find_first(unit, "./correspondenceTo")
  if (!inherits(corr, "xml_missing")) {
    lines <- vapply(xml2::xml_find_all(corr, ".//line"), \(l) .tei_squish(xml2::xml_text(l)), character(1))
    lines <- lines[nzchar(lines) & !grepl("^correspondence$", lines, ignore.case = TRUE)]
    emails <- vapply(xml2::xml_find_all(corr, ".//email"), \(e) .tei_squish(xml2::xml_text(e)), character(1))
    author_notes <- c(author_notes, paste0("<corresp>", esc(paste(lines, collapse = "; ")),
                                           paste0(" <email>", esc(emails), "</email>", collapse = ""),
                                           "</corresp>"))
  }
  for (n in xml2::xml_find_all(header, "./noteGroup/note")) {
    type <- xml2::xml_attr(n, "type") %NA% "other"
    ps <- vapply(xml2::xml_find_all(n, ".//p"), \(p) .tei_squish(xml2::xml_text(p)), character(1))
    ps <- ps[nzchar(ps)]
    if (type == "funding") {
      # the first paragraph is the heading "Funding information"
      if (length(ps) > 1 && grepl("^funding( information)?$", ps[1], ignore.case = TRUE)) ps <- ps[-1]
      funding_stmt <- c(funding_stmt, paste(ps, collapse = " "))
    } else if (type == "handlingEditor") {
      # the editor is also a creator when the file names one
      if (!has_editor) {
        t <- sub("\\s*handling editor\\s*$", "", paste(ps, collapse = " "), ignore.case = TRUE)
        author_notes <- c(author_notes, sprintf("<fn fn-type=\"edited-by\"><p>Handling Editor: %s</p></fn>", esc(t)))
      }
    } else {
      fn_type <- switch(type, equal = "equal", "other")
      author_notes <- c(author_notes, sprintf("<fn fn-type=\"%s\"><p>%s</p></fn>", fn_type,
                                              esc(paste(ps, collapse = " "))))
    }
  }

  # keywords, subjects, funding, supporting information ----
  kwd_groups <- vapply(xml2::xml_find_all(cm, "./keywordGroup"), \(g) {
    type <- xml2::xml_attr(g, "type")
    terms <- vapply(xml2::xml_find_all(g, "./keyword"), \(k) wrap("kwd", .tei_squish(xml2::xml_text(k))), "")
    sprintf("<kwd-group%s>%s</kwd-group>",
            if (!is.na(type) && type != "author") sprintf(" kwd-group-type=\"%s\"", .tei_attr(type)) else "",
            paste(terms, collapse = ""))
  }, character(1))
  subj <- function(type, xpath) {
    s <- vapply(xml2::xml_find_all(unit, xpath), \(x) .tei_squish(xml2::xml_text(x)), "")
    s <- s[nzchar(s)]
    if (!length(s)) return("")
    sprintf("<subj-group subj-group-type=\"%s\">%s</subj-group>", type,
            paste0("<subject>", esc(s), "</subject>", collapse = ""))
  }
  categories <- paste0(subj("heading", "./titleGroup/title[@type='articleCategory']"),
                       subj("toc-heading", "./titleGroup/title[starts-with(@type, 'tocHeading')]"),
                       subj("subject", "./subjectInfo/subject"))
  awards <- vapply(xml2::xml_find_all(cm, "./fundingInfo"), \(f) {
    name <- txt(f, "./fundingAgency")
    if (!nzchar(name)) return("")
    fdoi <- attr1(f, "./fundingAgency", "funderDoi")
    ids <- vapply(xml2::xml_find_all(f, "./fundingNumber"), \(a) wrap("award-id", .tei_squish(xml2::xml_text(a))), "")
    sprintf("<award-group><funding-source><institution-wrap><institution>%s</institution>%s</institution-wrap></funding-source>%s</award-group>",
            esc(name),
            if (!is.na(fdoi)) sprintf("<institution-id institution-id-type=\"doi\">%s</institution-id>", esc(fdoi)) else "",
            paste(ids, collapse = ""))
  }, character(1))
  awards <- awards[nzchar(awards)]
  funding_xml <- if (length(awards) || length(funding_stmt)) {
    paste0("<funding-group>", paste(awards, collapse = ""),
           paste0("<funding-statement>", esc(funding_stmt), "</funding-statement>", collapse = ""),
           "</funding-group>")
  } else ""
  supp <- vapply(xml2::xml_find_all(cm, "./supportingInformation/supportingInfoItem"), \(s) {
    href <- attr1(s, "./mediaResource", "href")
    mt <- strsplit(attr1(s, "./mediaResource", "mimeType") %NA% "", "/", fixed = TRUE)[[1]]
    sprintf("<supplementary-material id=\"%s\"%s%s>%s</supplementary-material>",
            .tei_attr(.wly_id(s) %NA% ""),
            if (!is.na(href)) sprintf(" href=\"%s\"", .tei_attr(href)) else "",
            if (length(mt) == 2) sprintf(" mimetype=\"%s\" mime-subtype=\"%s\"", .tei_attr(mt[1]), .tei_attr(mt[2])) else "",
            if (nzchar(txt(s, "./caption"))) sprintf("<caption><p>%s</p></caption>", esc(txt(s, "./caption"))) else "")
  }, character(1))

  # dates, copyright, licence, counts ----
  events <- xml2::xml_find_all(unit, "./eventGroup/event")
  ev_type <- xml2::xml_attr(events, "type")
  ev <- function(types) {
    d <- xml2::xml_attr(events[ev_type %in% types], "date")
    d <- d[!is.na(d)]
    if (length(d)) d[[length(d)]] else NA_character_
  }
  history <- paste0(.wly_date("date", "date-type", "received", ev("manuscriptReceived")),
                    .wly_date("date", "date-type", "rev-recd", ev("manuscriptRevised")),
                    .wly_date("date", "date-type", "accepted", ev("manuscriptAccepted")))
  if (nzchar(history)) history <- paste0("<history>", history, "</history>")
  first_online <- ev("firstOnline")
  if (is.na(first_online)) first_online <- ev(c("publishedOnlineEarlyUnpaginated", "publishedOnlineFinalForm"))
  pub_dates <- paste0(.wly_date("pub-date", "pub-type", "epub", first_online),
                      .wly_date("pub-date", "pub-type", "ppub", attr1(part, "./coverDate", "startDate")))
  copyright <- txt(unit, "./copyright")
  if (!nzchar(copyright)) copyright <- txt(part, "./copyright")
  cy <- regmatches(copyright, regexpr("(1[89]|20)[0-9]{2}", copyright))
  holder <- sub("^.*?(1[89]|20)[0-9]{2}\\s*", "", copyright)
  holder <- trimws(sub("\\..*$", "", holder))
  legal <- xml2::xml_find_first(unit, "./legalStatement")
  license <- if (inherits(legal, "xml_missing")) "" else {
    href <- attr1(legal, ".//link | .//url", "href")
    sprintf("<license%s><license-p>%s</license-p></license>",
            if (!is.na(href)) sprintf(" href=\"%s\"", .tei_attr(href)) else "",
            esc(.tei_squish(xml2::xml_text(legal))))
  }
  count_xml <- function(wtype, jtype) {
    n <- attr1(cm, sprintf("./countGroup/count[@type='%s']", wtype), "number")
    if (is.na(n)) n <- attr1(unit, sprintf("./countGroup/count[@type='%s']", wtype), "number")
    if (is.na(n)) "" else sprintf("<%s count=\"%s\"/>", jtype, .tei_attr(n))
  }
  counts <- paste0(count_xml("figureTotal", "fig-count"), count_xml("tableTotal", "table-count"),
                   count_xml("referenceTotal", "ref-count"), count_xml("pageTotal", "page-count"),
                   count_xml("wordTotal", "word-count"))

  # the peer review history, when the article links to it ----
  review_links <- xml2::xml_attr(xml2::xml_find_all(
    comp, "//section[@type='transparentPeerReview']//url | //section[@type='transparentPeerReview']//link[not(starts-with(@href, '#'))]"),
    "href")
  related <- paste0(sprintf("<related-article related-article-type=\"peer-review-report\" href=\"%s\"/>",
                            .tei_attr(unique(review_links[!is.na(review_links)]))), collapse = "")

  # the text, renamed to JATS ----
  .wly_rename(comp, kind)
  # abstracts: the main one, and the graphical and short ones by type
  lang <- xml2::xml_attr(comp, "xml-lang") %NA% (xml2::xml_attr(comp, "lang") %NA% "en")
  abstracts <- vapply(xml2::xml_find_all(cm, "./abstractGroup/abstract"), \(a) {
    type <- xml2::xml_attr(a, "type") %NA% "main"
    alang <- xml2::xml_attr(a, "xml-lang") %NA% (xml2::xml_attr(a, "lang") %NA% lang)
    xml2::xml_remove(xml2::xml_find_all(a, "./title"))
    inner <- .wly_inner(a)
    if (!nzchar(.tei_squish(xml2::xml_text(a)))) return("")
    if (type == "main" && alang != lang) {
      return(sprintf("<trans-abstract xml:lang=\"%s\">%s</trans-abstract>", .tei_attr(alang), inner))
    }
    sprintf("<abstract%s>%s</abstract>",
            if (type != "main") sprintf(" abstract-type=\"%s\"", .tei_attr(type)) else "", inner)
  }, character(1))
  # the main abstract first: the JATS reader takes the one without a type
  abstracts <- abstracts[order(!startsWith(abstracts, "<abstract>"))]

  title_node <- xml2::xml_find_first(cm, "./titleGroup/title[@type='main']")
  if (inherits(title_node, "xml_missing")) title_node <- xml2::xml_find_first(cm, "./titleGroup/title")
  subtitle_node <- xml2::xml_find_first(cm, "./titleGroup/title[@type='subtitle' or @type='sub']")

  back <- character(0)
  notes <- character(0)
  refs <- character(0)
  sections <- ""
  if (!inherits(body, "xml_missing")) {
    # floats as JATS elements, in their place
    for (f in xml2::xml_find_all(body, ".//figure")) xml2::xml_replace(f, xml2::read_xml(.wly_figure(f)))
    for (tb in xml2::xml_find_all(body, ".//tabular | .//tabularFixed")) {
      xml2::xml_replace(tb, xml2::read_xml(.wly_table(tb)))
    }
    # notes: the text's footnotes and endnotes, read from the back matter
    for (n in xml2::xml_find_all(body, ".//note[not(ancestor::table-wrap)]")) {
      xml2::xml_set_name(n, "fn")
      notes <- c(notes, as.character(n))
    }
    xml2::xml_remove(xml2::xml_find_all(body, ".//fn[not(ancestor::table-wrap)] | .//noteGroup[not(ancestor::table-wrap)]"))
    # references
    refs <- vapply(xml2::xml_find_all(body, ".//bibliography//bib"), .wly_ref, character(1))
    xml2::xml_remove(xml2::xml_find_all(body, ".//bibliography"))
    # appendices
    for (ap in xml2::xml_find_all(body, ".//appendix")) {
      title <- xml2::xml_find_first(ap, "./titleGroup/title[@type='main'] | ./title[@type='main']")
      head <- if (inherits(title, "xml_missing")) "Appendix" else .tei_squish(xml2::xml_text(title))
      xml2::xml_remove(xml2::xml_find_all(ap, "./titleGroup | ./title"))
      for (s in xml2::xml_find_all(ap, ".//section")) xml2::xml_set_name(s, "sec")
      xml2::xml_set_name(ap, "app")
      t <- xml2::xml_add_child(ap, "title", .where = 0)
      xml2::xml_text(t) <- head
      back <- c(back, paste0("<app-group>", as.character(ap), "</app-group>"))
      xml2::xml_remove(ap)
    }
    # sections, with their declared type in JATS's words
    sec_types <- c(introduction = "intro", materialsAndMethods = "materials|methods",
                   methods = "methods", results = "results", discussion = "discussion",
                   conclusions = "conclusions", conclusion = "conclusions",
                   acknowledgments = "acknowledgments", conflictOfInterest = "COI-statement",
                   dataAvailability = "data-availability", dataAccess = "data-availability",
                   authorContributions = "author-contributions", funding = "funding",
                   ethics = "ethics-statement", transparentPeerReview = "peer-review")
    for (s in xml2::xml_find_all(body, ".//section")) {
      type <- xml2::xml_attr(s, "type")
      xml2::xml_set_name(s, "sec")
      if (!is.na(type)) xml2::xml_set_attr(s, "sec-type", sec_types[type] %NA% type)
      xml2::xml_set_attr(s, "type", NULL)
    }
    # statements (the acknowledgements, conflicts of interest, data
    # availability, peer review history) are back matter; "Open Research"
    # only groups them
    for (o in xml2::xml_find_all(body, ".//sec[@sec-type='openResearch']")) {
      own_ps <- xml2::xml_find_all(o, "./p")
      if (length(own_ps)) {
        h <- txt(o, "./title")
        back <- c(back, sprintf("<sec sec-type=\"open-research\"><title>%s</title>%s</sec>",
                                esc(if (nzchar(h)) h else "Open Research"),
                                paste(vapply(own_ps, as.character, ""), collapse = "")))
      }
      for (s in xml2::xml_find_all(o, "./sec")) xml2::xml_add_sibling(o, s, .where = "before")
      xml2::xml_remove(o)
    }
    stmt_types <- c("acknowledgments", "COI-statement", "data-availability", "peer-review",
                    "author-contributions", "funding", "ethics-statement")
    for (s in xml2::xml_find_all(body, ".//sec[@sec-type]")) {
      st <- xml2::xml_attr(s, "sec-type")
      if (!st %in% stmt_types) next
      if (st == "acknowledgments") xml2::xml_set_name(s, "ack")
      back <- c(back, as.character(s))
      xml2::xml_remove(s)
    }
    sections <- paste(vapply(xml2::xml_children(body), as.character, character(1)), collapse = "\n")
  }

  # metadata ----
  doi <- txt(unit, "./doi")
  issn_e <- txt(product, "./issn[@type='electronic']")
  issn_p <- txt(product, "./issn[@type='print']")
  num <- function(node, type) txt(node, sprintf("./numberingGroup/numbering[@type='%s']", type))
  elocator <- attr1(unit, "./idGroup/id[@type='eLocator']", "value") %NA% ""
  unit_type <- xml2::xml_attr(unit, "type") %NA% ""
  category <- txt(unit, "./titleGroup/title[@type='articleCategory']")
  art_type <- if (unit_type %in% c("", "article") && nzchar(category)) {
    gsub("[^a-z]+", "-", tolower(category))
  } else {
    tolower(gsub("([a-z])([A-Z])", "\\1-\\2", unit_type))
  }
  title <- .wly_inner(title_node)
  front <- paste0(
    "<front><journal-meta><journal-title-group>",
    wrap("journal-title", txt(product, "./titleGroup/title[@type='main']")),
    wrap("abbrev-journal-title", txt(product, "./titleGroup/title[@type='short']")),
    "</journal-title-group>",
    if (nzchar(issn_e)) sprintf("<issn pub-type=\"epub\">%s</issn>", esc(issn_e)) else "",
    if (nzchar(issn_p)) sprintf("<issn pub-type=\"ppub\">%s</issn>", esc(issn_p)) else "",
    "</journal-meta><article-meta>",
    if (nzchar(doi)) sprintf("<article-id pub-id-type=\"doi\">%s</article-id>", esc(doi)) else "",
    if (nzchar(categories)) paste0("<article-categories>", categories, "</article-categories>") else "",
    "<title-group><article-title>", title, "</article-title>",
    if (!inherits(subtitle_node, "xml_missing")) paste0("<subtitle>", .wly_inner(subtitle_node), "</subtitle>") else "",
    "</title-group>",
    "<contrib-group>", paste(contribs, collapse = ""), "</contrib-group>", paste(affs, collapse = ""),
    if (length(author_notes)) paste0("<author-notes>", paste(author_notes, collapse = ""), "</author-notes>") else "",
    pub_dates, wrap("volume", num(part, "journalVolume")), wrap("issue", num(part, "journalIssue")),
    wrap("fpage", num(unit, "pageFirst")), wrap("lpage", num(unit, "pageLast")),
    wrap("elocation-id", elocator),
    history,
    "<permissions>", wrap("copyright-statement", copyright),
    if (length(cy)) wrap("copyright-year", cy) else "",
    if (nzchar(holder) && nzchar(copyright)) wrap("copyright-holder", holder) else "",
    license, "</permissions>",
    related,
    paste(abstracts[nzchar(abstracts)], collapse = ""),
    paste(kwd_groups, collapse = ""), funding_xml,
    if (nzchar(counts)) paste0("<counts>", counts, "</counts>") else "",
    "</article-meta></front>"
  )

  paste0(
    sprintf("<article dtd-version=\"%s\" xml:lang=\"%s\"%s>",
            .tei_attr(paste0("wileyml-", xml2::xml_attr(comp, "version") %NA% "unknown")),
            .tei_attr(lang),
            if (nzchar(art_type)) sprintf(" article-type=\"%s\"", .tei_attr(art_type)) else ""),
    front,
    "<body>", sections, "</body>",
    "<back>", paste(back, collapse = "\n"),
    if (length(notes)) paste0("<fn-group>", paste(notes, collapse = "\n"), "</fn-group>") else "",
    "<ref-list>", paste(refs, collapse = "\n"), "</ref-list></back>",
    "<floats-group>", paste(supp, collapse = "\n"), "</floats-group>",
    "</article>"
  )
}

#' Convert a Wiley full-text XML file to a paper
#'
#' @param xml_path path to Wiley's XML (onlinelibrary.wiley.com/doi/full-xml/)
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.wiley_to_bibr12 <- function(xml_path) {
  jats <- .wiley_to_jats(.elsevier_read(xml_path))
  xml <- xml2::read_xml(jats, options = "HUGE")
  .jats_to_bibr12(xml_path, xml = xml, input_format = "xml", producer = "wiley")
}
