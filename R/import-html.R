# Publisher XML: web pages ----
#
# A full-text link sometimes gives a web page: the HTML version of an article
# (OJS's "XHTML galley" plugin), or a page saying the article is not there
# ("404 Not Found"). The page is read with the HTML parser, which accepts
# what an XML parser refuses. Headings (h1-h6), paragraphs, lists, tables and
# figure captions are read as blocks (see R/import-xml-blocks.R); the
# metadata from the page's citation_* and Dublin Core meta tags (title,
# authors, DOI, journal, date, abstract, keywords, references). A short page
# that says it is an error stops with what it says.

#' Convert a web page to a paper
#' @param xml_path path to the HTML (or XHTML) file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.html_to_bibr12 <- function(xml_path) {
  txt <- .xml_text_utf8(xml_path)
  html <- xml2::read_html(txt, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE"))
  body <- xml2::xml_find_first(html, "//body")
  if (inherits(body, "xml_missing")) body <- xml2::xml_root(html)
  page_text <- .tei_squish(xml2::xml_text(body))
  if (nchar(page_text) < 2000 && grepl("\\b(404|403|500|not found|forbidden|access denied|error|no encontrad|n\u00E3o encontrad|unauthori[sz]ed)\\b",
                                       page_text, ignore.case = TRUE)) {
    stop("an error page from a web server, not an article: ", substr(page_text, 1, 150), call. = FALSE)
  }
  meta_tag <- function(names) {
    v <- character(0)
    for (n in names) {
      v <- c(v, xml2::xml_attr(xml2::xml_find_all(html, sprintf(
        "//meta[translate(@name, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')='%s']", tolower(n))), "content"))
    }
    .tei_squish(v[!is.na(v) & nzchar(trimws(v))])
  }
  one <- function(names) { v <- meta_tag(names); if (length(v)) v[[1]] else "" }

  rules <- .generic_inline_rules
  inl <- function(n) .tei_squish(.blk_inline(n, rules))
  blocks <- list()
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  skip <- c("head", "script", "style", "nav", "footer", "aside", "form", "noscript", "iframe",
            "button", "select", "svg", "template", "header", "menu")
  block_names <- c("p", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "table", "figure", "div",
                   "section", "article", "blockquote", "pre", "dl", "main")
  walk <- function(node) {
    for (ch in xml2::xml_children(node)) {
      nm <- tolower(xml2::xml_name(ch))
      if (nm %in% skip) next
      cls <- tolower(xml2::xml_attr(ch, "class") %NA% "")
      if (grepl("\\b(navbar|menu|breadcrumb|sidebar|footer|cookie|share|toolbar)\\b", cls)) next
      if (grepl("^h[1-6]$", nm)) {
        t <- inl(ch)
        if (nzchar(.blk_plain(t))) add(.blk_h(t, as.integer(substring(nm, 2))))
      } else if (nm %in% c("p", "blockquote", "pre", "address")) {
        t <- inl(ch)
        if (nzchar(.blk_plain(t))) add(.blk_p(t))
      } else if (nm %in% c("ul", "ol")) {
        items <- vapply(xml2::xml_find_all(ch, "./li"), inl, "")
        items <- items[nzchar(items)]
        if (length(items)) add(.blk_list(as.list(items), ordered = nm == "ol"))
      } else if (nm == "dl") {
        dts <- xml2::xml_find_all(ch, "./dt")
        items <- vapply(dts, \(d) paste0(inl(d), ": ", inl(xml2::xml_find_first(d, "following-sibling::dd[1]"))), "")
        if (length(items)) add(.blk_list(as.list(items)))
      } else if (nm == "table") {
        rows <- lapply(xml2::xml_find_all(ch, ".//tr"), \(r) vapply(xml2::xml_find_all(r, "./td | ./th"), inl, ""))
        cap <- xml2::xml_find_first(ch, "./caption")
        add(.blk_table(rows, if (inherits(cap, "xml_missing")) "" else inl(cap),
                       length(xml2::xml_find_all(ch, ".//thead/tr"))))
      } else if (nm == "figure") {
        cap <- xml2::xml_find_first(ch, ".//figcaption")
        if (!inherits(cap, "xml_missing")) add(.blk_fig(inl(cap)))
      } else {
        # a container: its own text (written without <p>) is a paragraph
        kids <- tolower(xml2::xml_name(xml2::xml_children(ch)))
        own <- xml2::xml_text(xml2::xml_find_all(ch, "./text()"))
        if (any(nzchar(trimws(own))) && !any(kids %in% block_names)) {
          t <- inl(ch)
          if (nzchar(.blk_plain(t))) add(.blk_p(t))
        } else {
          walk(ch)
        }
      }
    }
  }
  walk(body)
  authors <- meta_tag(c("citation_author", "DC.Creator.PersonalName", "dc.creator"))
  meta <- list(
    title = one(c("citation_title", "DC.Title", "dc.title")),
    authors = lapply(unique(authors), \(a) {
      # "Family, Given" as citation_author writes it
      if (grepl(",", a)) list(family = trimws(sub(",.*$", "", a)), given = trimws(sub("^[^,]*,", "", a)))
      else list(literal = a)
    }),
    doi = one(c("citation_doi", "DC.Identifier.DOI", "dc.identifier.doi")),
    journal = one(c("citation_journal_title", "DC.Source", "dc.source")),
    issn = one(c("citation_issn", "DC.Source.ISSN")),
    volume = one(c("citation_volume", "DC.Source.Volume")),
    issue = one(c("citation_issue", "DC.Source.Issue")),
    fpage = one("citation_firstpage"), lpage = one("citation_lastpage"),
    date = gsub("/", "-", one(c("citation_publication_date", "citation_date", "DC.Date.issued", "dc.date"))),
    language = one(c("citation_language", "DC.Language")),
    publisher = one(c("citation_publisher", "DC.Publisher")),
    abstract = { a <- one(c("citation_abstract", "DC.Description", "dc.description")); if (nchar(a) > 100) .tei_esc(a) else character(0) },
    keywords = unique(trimws(unlist(strsplit(meta_tag(c("citation_keywords", "DC.Subject")), ";")))),
    refs = .tei_esc(meta_tag("citation_reference")),
    version = "html"
  )
  if (!nzchar(meta$title)) {
    t <- xml2::xml_text(xml2::xml_find_first(html, "//head/title"))
    if (!is.na(t)) meta$title <- .tei_squish(t)
  }
  # the references the page lists as meta tags replace those in its text
  if (length(meta$refs)) {
    in_refs <- FALSE
    keep <- vapply(blocks, \(b) {
      if (b$type == "h") in_refs <<- identical(.blk_heading_kind(b$text), "references")
      !in_refs
    }, TRUE)
    blocks <- blocks[keep]
  }
  if (!length(blocks) && !length(meta$abstract)) stop("the web page holds no article text", call. = FALSE)
  .blocks_to_bibr12(xml_path, blocks, meta, "html")
}
