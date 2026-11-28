# Publisher XML: Elsevier ----
#
# Elsevier delivers full text (its Article Retrieval API, and the text-mining
# links it registers with CrossRef) in its own XML: a
# <full-text-retrieval-response> with the article in Elsevier's journal
# article DTD (the ce:, ja: and sb: elements). The article is translated into
# JATS, element by element (ce:section is a JATS <sec>, ce:cross-ref an
# <xref>, sb:reference an <element-citation>), and read as JATS from there
# (see R/import-jats.R).

#' Elsevier XML with its namespace prefixes removed
#'
#' Elsevier's elements keep their prefixes (ce:para, sb:title) when the
#' namespaces are stripped, so the prefixes are removed from the text before
#' it is read: from element names (ce:para -> para) and, so that two
#' attributes never get the same name, written into attribute names
#' (xlink:href -> xlink-href). Namespace declarations are removed.
#'
#' @param path path to the Elsevier XML file
#' @returns an xml2 document
#' @noRd
.elsevier_read <- function(path) {
  txt <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(txt) <- "UTF-8"
  txt <- gsub("\\sxmlns(:[A-Za-z0-9_.-]+)?=\"[^\"]*\"", "", txt, perl = TRUE)
  # element names, in start and end tags
  txt <- gsub("<(/?)[A-Za-z0-9_.-]+:([A-Za-z0-9_.-]+)", "<\\1\\2", txt, perl = TRUE)
  # attribute names inside tags: (?<=\s) name with a prefix, followed by =
  txt <- gsub("(<[^<>]*?)\\s([A-Za-z0-9_.-]+):([A-Za-z0-9_.-]+)=", "\\1 \\2-\\3=",
              txt, perl = TRUE)
  # a tag can hold several prefixed attributes: repeat until none is left
  for (i in 1:5) {
    new <- gsub("(<[^<>]*?)\\s([A-Za-z0-9_.-]+):([A-Za-z0-9_.-]+)=", "\\1 \\2-\\3=",
                txt, perl = TRUE)
    if (identical(new, txt)) break
    txt <- new
  }
  xml2::read_xml(txt, options = "HUGE")
}

#' The text of an element, squished; "" when absent
#' @noRd
.els_text <- function(node, xpath = ".") {
  if (inherits(node, "xml_missing")) return("")
  n <- xml2::xml_find_first(node, xpath)
  if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
}

#' A CALS table (Elsevier's tgroup/row/entry) as an XHTML table (tr/td)
#'
#' Column spans come from an entry's first and last column names (namest,
#' nameend) and the order of the colspec elements, row spans from morerows.
#' @noRd
.els_cals_to_xhtml <- function(tgroup) {
  cols <- xml2::xml_attr(xml2::xml_find_all(tgroup, "./colspec"), "colname")
  for (entry in xml2::xml_find_all(tgroup, ".//entry")) {
    st <- match(xml2::xml_attr(entry, "namest"), cols)
    en <- match(xml2::xml_attr(entry, "nameend"), cols)
    if (!is.na(st) && !is.na(en) && en > st) xml2::xml_set_attr(entry, "colspan", en - st + 1)
    mr <- suppressWarnings(as.integer(xml2::xml_attr(entry, "morerows")))
    if (!is.na(mr) && mr > 0) xml2::xml_set_attr(entry, "rowspan", mr + 1)
    xml2::xml_set_name(entry, if (length(xml2::xml_find_all(entry, "ancestor::thead"))) "th" else "td")
  }
  for (row in xml2::xml_find_all(tgroup, ".//row")) xml2::xml_set_name(row, "tr")
  xml2::xml_remove(xml2::xml_find_all(tgroup, "./colspec | ./spanspec"))
  xml2::xml_set_name(tgroup, "table")
}

#' Rename Elsevier's body elements to their JATS names, in place
#'
#' @param art the Elsevier article element (prefixes removed)
#' @param kind named character vector: element id -> JATS ref-type
#' @noRd
.els_rename <- function(art, kind) {
  ren <- function(xpath, name) {
    for (n in xml2::xml_find_all(art, xpath)) xml2::xml_set_name(n, name)
  }
  # formulas: displayed ones in <display>, the rest inline
  for (f in xml2::xml_find_all(art, ".//formula")) {
    disp <- identical(xml2::xml_name(xml2::xml_parent(f)), "display")
    xml2::xml_set_name(f, if (disp) "disp-formula" else "inline-formula")
  }
  # cross-references: JATS names the kind of target
  for (x in xml2::xml_find_all(art, ".//cross-ref | .//cross-refs")) {
    rid <- strsplit(xml2::xml_attr(x, "refid") %||% "", "\\s+")[[1]]
    type <- unname(kind[rid[rid %in% names(kind)]])[1]
    xml2::xml_set_name(x, "xref")
    xml2::xml_set_attr(x, "rid", paste(rid, collapse = " "))
    if (!is.na(type %||% NA)) xml2::xml_set_attr(x, "ref-type", type)
  }
  for (x in xml2::xml_find_all(art, ".//inter-ref")) {
    xml2::xml_set_name(x, "ext-link")
    xml2::xml_set_attr(x, "href", xml2::xml_attr(x, "xlink-href"))
  }
  # a non-breaking or thin space element is a space
  for (x in xml2::xml_find_all(art, ".//hsp | .//vsp")) {
    xml2::xml_set_name(x, "x")
    xml2::xml_text(x) <- " "
  }
  xml2::xml_remove(xml2::xml_find_all(art, ".//float-anchor | .//e-component | .//alt-text"))
  for (tg in xml2::xml_find_all(art, ".//tgroup")) .els_cals_to_xhtml(tg)
  ren(".//section", "sec")
  ren(".//section-title", "title")
  ren(".//para | .//simple-para | .//note-para", "p")
  ren(".//inf", "sub")
  ren(".//small-caps", "sc")
  ren(".//footnote | .//table-footnote", "fn")
  ren(".//display | .//enunciation | .//textbox-body", "x")
  ren(".//textbox", "boxed-text")
  invisible(art)
}

#' Translate an Elsevier full-text response into JATS
#'
#' @param xml the response, read with `.elsevier_read()`
#' @returns the JATS article as a string
#' @noRd
.elsevier_to_jats <- function(xml) {
  core <- xml2::xml_find_first(xml, "//coredata")
  art <- xml2::xml_find_first(xml, "//serial-item/*[1] | //nonserial-item/*[1]")
  if (inherits(art, "xml_missing")) {
    stop("the Elsevier XML has no article text (originalText), only its metadata",
         call. = FALSE)
  }
  head <- xml2::xml_find_first(art, "./head")
  body <- xml2::xml_find_first(art, "./body")
  tail <- xml2::xml_find_first(art, "./tail")
  floats <- xml2::xml_find_first(art, "./floats")
  esc <- .tei_esc
  wrap <- function(tag, x) if (nzchar(x)) sprintf("<%s>%s</%s>", tag, esc(x), tag) else ""
  str <- function(nodes) paste(vapply(nodes, as.character, character(1)), collapse = "\n")

  # the kind of each id a cross-reference can point to
  kind_of <- function(xpath, type) {
    ids <- xml2::xml_attr(xml2::xml_find_all(art, xpath), "id")
    stats::setNames(rep(type, length(ids)), ids)[!is.na(ids)]
  }
  kind <- c(kind_of(".//bib-reference", "bibr"), kind_of(".//figure", "fig"),
            kind_of(".//table", "table"), kind_of(".//footnote", "fn"),
            kind_of(".//formula", "disp-formula"))

  # authors and affiliations (before renaming) ----
  contribs <- character(0)
  affs <- character(0)
  for (g in xml2::xml_find_all(head, "./author-group")) {
    group_affs <- xml2::xml_attr(xml2::xml_find_all(g, "./affiliation"), "id")
    for (a in xml2::xml_find_all(g, "./affiliation")) {
      txt <- .els_text(a, "./textfn")
      if (!nzchar(txt)) {
        tmp <- .xml_copy(a)
        xml2::xml_remove(xml2::xml_find_all(tmp, "./label"))
        txt <- .tei_squish(xml2::xml_text(tmp))
      }
      affs <- c(affs, sprintf("<aff id=\"%s\">%s</aff>", .tei_attr(xml2::xml_attr(a, "id") %||% ""), esc(txt)))
    }
    for (au in xml2::xml_find_all(g, "./author")) {
      rid <- xml2::xml_attr(xml2::xml_find_all(au, "./cross-ref"), "refid")
      rid <- unlist(strsplit(rid[!is.na(rid)], "\\s+"))
      # an author who points to none of the group's affiliations has them all
      if (!any(rid %in% group_affs)) rid <- c(rid, group_affs[!is.na(group_affs)])
      orcid <- xml2::xml_attr(au, "orcid") %||% NA_character_
      email <- .els_text(au, "./e-address[@type='email' or not(@type)]")
      # CRediT roles, with the vocabulary's link
      roles <- vapply(xml2::xml_find_all(au, "./contributor-role"), \(r) {
        uri <- xml2::xml_attr(r, "role")
        sprintf("<role%s>%s</role>",
                if (!is.na(uri)) sprintf(" vocab-term-identifier=\"%s\"", .tei_attr(uri)) else "",
                esc(.tei_squish(xml2::xml_text(r))))
      }, character(1))
      contribs <- c(contribs, paste0(
        "<contrib contrib-type=\"author\"><name>", wrap("surname", .els_text(au, "./surname")),
        wrap("given-names", .els_text(au, "./given-name")), "</name>",
        if (!is.na(orcid) && nzchar(orcid)) sprintf("<contrib-id contrib-id-type=\"orcid\">%s</contrib-id>", esc(orcid)) else "",
        wrap("email", email), paste(roles, collapse = ""),
        paste0(sprintf("<xref ref-type=\"aff\" rid=\"%s\"/>", .tei_attr(rid)), collapse = ""),
        "</contrib>"))
    }
  }

  # references (before renaming) ----
  refs <- vapply(xml2::xml_find_all(art, ".//bib-reference"), \(b) {
    id <- xml2::xml_attr(b, "id") %||% ""
    printed <- .els_text(b, "./source-text")
    r <- xml2::xml_find_first(b, ".//reference")
    if (inherits(r, "xml_missing")) {
      other <- .els_text(b, ".//other-ref")
      if (!nzchar(other)) other <- printed
      return(sprintf("<ref id=\"%s\"><mixed-citation>%s</mixed-citation></ref>", .tei_attr(id), esc(other)))
    }
    persons <- function(xpath, type) {
      p <- xml2::xml_find_all(r, xpath)
      if (!length(p)) return("")
      names <- vapply(p, \(x) {
        if (xml2::xml_name(x) == "collaboration") return(wrap("collab", .tei_squish(xml2::xml_text(x))))
        paste0("<name>", wrap("surname", .els_text(x, "./surname")),
               wrap("given-names", .els_text(x, "./given-name")), "</name>")
      }, character(1))
      sprintf("<person-group person-group-type=\"%s\">%s</person-group>", type, paste(names, collapse = ""))
    }
    host <- xml2::xml_find_first(r, "./host")
    issue <- xml2::xml_find_first(host, "./issue")
    book <- xml2::xml_find_first(host, "./edited-book | ./book")
    is_journal <- !inherits(issue, "xml_missing")
    title <- .els_text(r, "./contribution/title/maintitle")
    source <- if (is_journal) {
      .els_text(issue, "./series/title/maintitle")
    } else {
      .els_text(book, "./title/maintitle")
    }
    if (!is_journal && !nzchar(source) && !nzchar(title)) source <- .els_text(host, ".//maintitle")
    year <- .els_text(host, ".//date")
    doi <- .els_text(host, "./doi")
    if (!nzchar(doi)) doi <- .els_text(r, ".//doi")
    url <- xml2::xml_attr(xml2::xml_find_first(r, ".//inter-ref"), "xlink-href")
    cit <- paste0(
      sprintf("<element-citation publication-type=\"%s\">", if (is_journal) "journal" else if (!inherits(book, "xml_missing")) "book" else "other"),
      persons("./contribution/authors/author | ./contribution/authors/collaboration", "author"),
      persons(".//editors/editor", "editor"),
      if (!is_journal && !inherits(book, "xml_missing") && nzchar(title)) wrap("chapter-title", title) else wrap("article-title", title),
      wrap("source", source), wrap("year", year),
      wrap("volume", .els_text(issue, "./series/volume-nr")),
      wrap("issue", .els_text(issue, "./issue-nr")),
      wrap("fpage", .els_text(host, "./pages/first-page")),
      wrap("lpage", .els_text(host, "./pages/last-page")),
      wrap("elocation-id", .els_text(host, "./article-number")),
      wrap("publisher-name", .els_text(host, ".//publisher/name")),
      if (nzchar(doi)) sprintf("<pub-id pub-id-type=\"doi\">%s</pub-id>", esc(doi)) else "",
      if (!is.na(url) && nzchar(url %||% "")) sprintf("<ext-link href=\"%s\">%s</ext-link>", .tei_attr(url), esc(url)) else "",
      "</element-citation>",
      # the reference as printed, for the reference text
      if (nzchar(printed)) sprintf("<mixed-citation specific-use=\"source-text\">%s</mixed-citation>", esc(printed)) else ""
    )
    sprintf("<ref id=\"%s\">%s</ref>", .tei_attr(id), cit)
  }, character(1))

  # abstract and keywords (before renaming) ----
  abstracts <- xml2::xml_find_all(head, "./abstract")
  cls <- xml2::xml_attr(abstracts, "class")
  main <- abstracts[cls %in% c("author", NA)]
  if (!length(main)) main <- abstracts[!cls %in% c("author-highlights", "graphical", "teaser")]
  keywords <- xml2::xml_find_all(head, "./keywords[not(@class) or @class='keyword']/keyword")
  kwd <- vapply(keywords, \(k) wrap("kwd", .tei_squish(xml2::xml_text(k))), character(1))
  # other keyword groups (JEL codes) are subject terms
  other_kwd <- vapply(xml2::xml_find_all(head, "./keywords[@class and not(@class='keyword')]"), \(g) {
    terms <- vapply(xml2::xml_find_all(g, "./keyword"), \(k) wrap("kwd", .tei_squish(xml2::xml_text(k))), "")
    sprintf("<kwd-group kwd-group-type=\"%s\">%s</kwd-group>",
            .tei_attr(tolower(xml2::xml_attr(g, "class"))), paste(terms, collapse = ""))
  }, character(1))

  # dates, copyright, handling editor, funders (before renaming) ----
  date_xml <- function(name, type) {
    d <- xml2::xml_find_all(head, paste0("./", name))
    if (!length(d)) return("")
    d <- d[[length(d)]] # the last revision
    y <- xml2::xml_attr(d, "year")
    if (is.na(y)) return("")
    sprintf("<date date-type=\"%s\">%s%s<year>%s</year></date>", type,
            if (!is.na(xml2::xml_attr(d, "day"))) wrap("day", xml2::xml_attr(d, "day")) else "",
            if (!is.na(xml2::xml_attr(d, "month"))) wrap("month", xml2::xml_attr(d, "month")) else "",
            esc(y))
  }
  history <- paste0(date_xml("date-received", "received"), date_xml("date-revised", "rev-recd"),
                    date_xml("date-accepted", "accepted"))
  if (nzchar(history)) history <- paste0("<history>", history, "</history>")
  cr <- xml2::xml_find_first(art, "./item-info/copyright")
  copyright <- paste0(
    wrap("copyright-statement", .els_text(core, "./copyright")),
    if (!inherits(cr, "xml_missing") && !is.na(xml2::xml_attr(cr, "year"))) wrap("copyright-year", xml2::xml_attr(cr, "year")) else "",
    if (!inherits(cr, "xml_missing")) wrap("copyright-holder", .tei_squish(xml2::xml_text(cr))) else "")
  editors <- vapply(xml2::xml_find_all(head, "./miscellaneous"), \(m) {
    t <- .tei_squish(xml2::xml_text(m))
    if (!grepl("^(handling|associate|action|guest)? ?editor", t, ignore.case = TRUE)) return("")
    sprintf("<fn fn-type=\"edited-by\"><p>%s</p></fn>", esc(t))
  }, character(1))
  editors <- editors[nzchar(editors)]
  funders <- vapply(xml2::xml_find_all(xml, "//funding-list/funding"), \(f) {
    name <- .els_text(f, "./funding-agency")
    if (!nzchar(name)) name <- .els_text(f, "./funding-agency-matched-string")
    if (!nzchar(name)) return("")
    # SciVal's funder ids are the Crossref Funder Registry's
    fid <- regmatches(.els_text(f, "./funding-agency-id"), regexpr("[0-9]{9,}$", .els_text(f, "./funding-agency-id")))
    awards <- vapply(xml2::xml_find_all(f, "./funding-id"), \(a) wrap("award-id", .tei_squish(xml2::xml_text(a))), "")
    sprintf("<award-group><funding-source><institution-wrap><institution>%s</institution>%s</institution-wrap></funding-source>%s</award-group>",
            esc(name),
            if (length(fid)) sprintf("<institution-id institution-id-type=\"doi\">10.13039/%s</institution-id>", fid) else "",
            paste(awards, collapse = ""))
  }, character(1))
  funders <- funders[nzchar(funders)]
  funding_xml <- if (length(funders)) paste0("<funding-group>", paste(funders, collapse = ""), "</funding-group>") else ""

  # declared section roles become JATS section types
  for (s in xml2::xml_find_all(art, ".//section[@role]")) {
    xml2::xml_set_attr(s, "sec-type", xml2::xml_attr(s, "role"))
  }
  # supplementary components: their label and description, before the
  # renaming drops them from the text
  supp <- vapply(xml2::xml_find_all(art, ".//e-component"), \(e) {
    sprintf("<supplementary-material>%s%s</supplementary-material>",
            wrap("label", .els_text(e, "./label")),
            if (nzchar(.els_text(e, "./alt-text"))) sprintf("<caption><p>%s</p></caption>", esc(.els_text(e, "./alt-text"))) else "")
  }, character(1))

  # the article's own elements, renamed to JATS ----
  .els_rename(art, kind)
  for (a in main) {
    xml2::xml_remove(xml2::xml_find_all(a, "./title")) # "Abstract"
    for (s in xml2::xml_find_all(a, "./abstract-sec")) xml2::xml_set_name(s, "sec")
  }
  abstract <- if (length(main)) {
    paste0("<abstract>", paste(vapply(xml2::xml_children(main[[1]]), as.character, character(1)), collapse = ""), "</abstract>")
  } else ""

  # statements and back matter
  back <- character(0)
  stmt <- function(nodes, type) {
    vapply(nodes, \(n) {
      xml2::xml_set_name(n, "sec")
      xml2::xml_set_attr(n, "sec-type", type)
      as.character(n)
    }, character(1))
  }
  back <- c(back, stmt(xml2::xml_find_all(art, ".//data-availability"), "data-availability"))
  back <- c(back, stmt(xml2::xml_find_all(art, ".//conflict-of-interest"), "COI-statement"))
  for (a in xml2::xml_find_all(art, ".//acknowledgment")) {
    xml2::xml_set_name(a, "ack")
    back <- c(back, as.character(a))
  }
  for (ap in xml2::xml_find_all(art, ".//appendices")) {
    for (s in xml2::xml_find_all(ap, "./sec")) xml2::xml_set_name(s, "app")
    xml2::xml_set_name(ap, "app-group")
    back <- c(back, as.character(ap))
  }
  sections <- if (inherits(body, "xml_missing")) "" else str(xml2::xml_find_all(body, "./sections/*"))

  # floats ----
  fig_tab <- if (inherits(floats, "xml_missing")) list() else xml2::xml_children(floats)
  float_jats <- vapply(fig_tab, \(f) {
    name <- xml2::xml_name(f)
    id <- .tei_attr(xml2::xml_attr(f, "id") %||% "")
    label <- .els_text(f, "./label")
    cap <- xml2::xml_find_first(f, "./caption")
    cap <- if (inherits(cap, "xml_missing")) "" else paste0("<caption>", paste(vapply(xml2::xml_children(cap), as.character, character(1)), collapse = ""), "</caption>")
    if (name == "figure") {
      return(sprintf("<fig id=\"%s\">%s%s</fig>", id, wrap("label", label), cap))
    }
    if (name == "table") {
      tab <- xml2::xml_find_first(f, "./table")
      foot <- xml2::xml_find_all(f, "./legend | ./fn")
      foot <- if (length(foot)) {
        paste0("<table-wrap-foot>", paste(vapply(foot, \(x) {
          xml2::xml_set_name(x, "fn")
          as.character(x)
        }, character(1)), collapse = ""), "</table-wrap-foot>")
      } else ""
      return(sprintf("<table-wrap id=\"%s\">%s%s%s%s</table-wrap>", id, wrap("label", label), cap,
                     if (inherits(tab, "xml_missing")) "" else as.character(tab), foot))
    }
    ""
  }, character(1))

  # metadata ----
  ct <- function(name) .els_text(core, paste0("./", name))
  doi <- ct("doi")
  date <- ct("coverDate")
  lic <- .els_text(core, "./openaccessUserLicense")
  pmid <- .els_text(xml, "//pubmed-id")
  issn <- ct("issn")
  if (grepl("^[0-9]{7}[0-9Xx]$", issn)) issn <- paste0(substr(issn, 1, 4), "-", substr(issn, 5, 8))
  pub_date_xml <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date)) {
    sprintf("<pub-date pub-type=\"ppub\"><day>%s</day><month>%s</month><year>%s</year></pub-date>",
            substr(date, 9, 10), substr(date, 6, 7), substr(date, 1, 4))
  } else ""
  title_node <- xml2::xml_find_first(head, "./title")
  title <- if (inherits(title_node, "xml_missing")) esc(ct("title")) else
    paste(vapply(xml2::xml_contents(title_node), as.character, character(1)), collapse = "")
  front <- paste0(
    "<front><journal-meta><journal-title-group>", wrap("journal-title", ct("publicationName")),
    "</journal-title-group>", if (nzchar(issn)) sprintf("<issn pub-type=\"epub\">%s</issn>", esc(issn)) else "",
    "<publisher>", wrap("publisher-name", ct("publisher")), "</publisher></journal-meta>",
    "<article-meta>",
    if (nzchar(doi)) sprintf("<article-id pub-id-type=\"doi\">%s</article-id>", esc(doi)) else "",
    if (nzchar(pmid)) sprintf("<article-id pub-id-type=\"pmid\">%s</article-id>", esc(pmid)) else "",
    "<title-group><article-title>", title, "</article-title></title-group>",
    "<contrib-group>", paste(contribs, collapse = ""), "</contrib-group>", paste(affs, collapse = ""),
    if (length(editors)) paste0("<author-notes>", paste(editors, collapse = ""), "</author-notes>") else "",
    pub_date_xml, wrap("volume", ct("volume")), wrap("issue", ct("issueIdentifier")),
    wrap("fpage", ct("startingPage")), wrap("lpage", ct("endingPage")),
    history,
    "<permissions>", copyright,
    if (nzchar(lic)) sprintf("<license href=\"%s\"/>", .tei_attr(lic)) else "",
    "</permissions>",
    abstract,
    if (length(kwd)) paste0("<kwd-group>", paste(kwd, collapse = ""), "</kwd-group>") else "",
    paste(other_kwd, collapse = ""), funding_xml,
    "</article-meta></front>"
  )

  version <- xml2::xml_attr(art, "version") %||% NA_character_
  # Elsevier's document subtype, in JATS's words where the meaning is plain
  # (fla: full-length article); any other code is kept as given
  sub_type <- xml2::xml_attr(art, "docsubtype")
  art_type <- c(fla = "research-article", rev = "review-article", sco = "brief-report",
                edi = "editorial", err = "correction", dis = "discussion")[sub_type]
  if (is.na(art_type)) art_type <- sub_type
  paste0(
    sprintf("<article dtd-version=\"%s\"%s>", .tei_attr(if (is.na(version)) "unknown" else version),
            if (!is.na(art_type)) sprintf(" article-type=\"%s\"", .tei_attr(art_type)) else ""),
    front,
    "<body>", sections, "</body>",
    "<back>", paste(back, collapse = "\n"),
    "<ref-list>", paste(refs, collapse = "\n"), "</ref-list></back>",
    "<floats-group>", paste(float_jats, collapse = "\n"), paste(supp, collapse = "\n"),
    "</floats-group>",
    "</article>"
  )
}

#' Convert an Elsevier full-text XML file to a paper
#'
#' @param xml_path path to the XML the Elsevier Article Retrieval API returns
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.elsevier_to_bibr12 <- function(xml_path) {
  jats <- .elsevier_to_jats(.elsevier_read(xml_path))
  xml <- xml2::read_xml(jats, options = "HUGE")
  .jats_to_bibr12(xml_path, xml = xml, input_format = "xml", producer = "elsevier")
}
