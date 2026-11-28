# Publisher XML: JATS ----
#
# JATS (the Journal Article Tag Suite, ANSI/NISO Z39.96) is the XML format
# most publishers deliver full text in, and the one PubMed Central stores
# every article in. Its versions (JATS 1.0-1.4, the older NLM Journal
# Publishing and Archiving DTDs 2.x-3.0) and the publishers' own variants of
# it (APA's Journal Archive DTD, Taylor & Francis's TF JATS, Pensoft's TaxPub)
# use the same element names, and CERMINE writes its output in it too.
#
# The file is translated into the TEI that Grobid writes, and that TEI is read
# by .grobid_to_bibr12(), the converter for Grobid's TEI. Publisher XML then
# gets exactly the sentence splitting, cross-references, statistics and text
# repairs a PDF read by Grobid gets, and a fix there applies to both. What TEI
# has no place for (journal, volume, pages, licence, statements) is added to
# the paper's info table afterwards.

#' Recognise the format of an XML full-text file
#'
#' Reads the start of the file: its first element, its namespace and DOCTYPE
#' line, and the elements near the start that tell formats with the same first
#' element apart (an `<article>` is JATS, DocBook or an OJS export).
#'
#' @param path path to an XML file
#' @returns a list: `format` (see `.read_xml_fulltext()` for the formats, or
#'   "unknown"), `root` (the first element's name) and `doctype` (the DOCTYPE
#'   line, or NA)
#' @noRd
.xml_format <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  raw <- readBin(con, "raw", 50000L)
  is_pdf <- length(raw) >= 5 && identical(rawToChar(raw[1:5]), "%PDF-")
  # a file that is not text at all (an image, a ZIP archive saved as .xml)
  hex <- paste(as.character(utils::head(raw, 4)), collapse = "")
  binary <- c(png = "^89504e47", jpeg = "^ffd8ff", gif = "^47494638", "zip archive" = "^504b0304",
              gzip = "^1f8b", "Word 97-2003 or Excel file" = "^d0cf11e0")
  hit <- names(binary)[vapply(binary, grepl, logical(1), x = hex)]
  if (length(hit)) return(list(format = "not_xml", root = hit[1], doctype = NA_character_))
  raw <- raw[raw != as.raw(0)] # UTF-16: keep the ASCII characters
  txt <- iconv(rawToChar(raw), "UTF-8", "UTF-8", sub = "")
  if (is.na(txt)) txt <- ""
  # the byte order mark, and comments, which can hold "<" and ">"
  txt <- sub(paste0("^", intToUtf8(0xFEFF)), "", txt)
  txt <- gsub("<!--.*?-->", "", txt, perl = TRUE)
  doctype <- regmatches(txt, regexpr("<!DOCTYPE[^>]*>", txt))
  doctype <- if (length(doctype)) doctype else NA_character_
  tags <- regmatches(txt, gregexpr("<[^>]+>", txt))[[1]]
  tags <- tags[!grepl("^<[?!]", tags)]
  root <- if (length(tags)) sub("^<([^[:space:]>/]+).*$", "\\1", tags[[1]]) else NA_character_
  local <- sub("^.*:", "", root %NA% "")
  first <- if (length(tags)) tags[[1]] else ""
  has <- function(pattern) grepl(pattern, txt, perl = TRUE)
  dt <- doctype %NA% ""
  # text before the XML declaration (a PHP notice an OJS server printed)
  junk <- grepl("\\S", sub("<\\?xml.*$", "", txt)) && has("<\\?xml")
  html_roots <- c("html", "head", "body", "h1", "h2", "h3", "h4", "h5", "h6", "div",
                  "p", "br", "title", "meta", "link", "script", "span", "center", "pre")
  format <- if (is_pdf) {
    "pdf"
  } else if (junk) {
    "junk_prefix"
  } else if (is.na(root)) {
    "unknown"
  } else if (local %in% c("TEI", "teiCorpus")) {
    "tei"
  } else if (local == "full-text-retrieval-response") {
    "elsevier"
  } else if (local == "component" && grepl("http://www.wiley.com/namespaces/wiley", first, fixed = TRUE)) {
    "wiley"
  } else if (grepl("DocBook", dt, fixed = TRUE) || grepl("docbook.org/ns/docbook", first, fixed = TRUE)) {
    "docbook"
  } else if (grepl("PKP|OJS", dt) || grepl("pkp.sfu.ca", first, fixed = TRUE)) {
    "ojs"
  } else if (local %in% c("book", "book-part-wrapper", "book-part")) {
    "bits"
  } else if (local == "article") {
    "jats"
  } else if (local == "doc" && grepl("\\ssps=", first)) {
    "scielo_sps"
  } else if (local == "TaggedPDF-doc") {
    "taggedpdf"
  } else if (local %in% c("package", "wordDocument") || (local == "document" && grepl("^<w:", root))) {
    "word"
  } else if (local %in% c("document", "document-content") && grepl("^<office:", first)) {
    "odf"
  } else if (local == "ArticleSet") {
    "pubmed"
  } else if (local == "doi_batch") {
    "crossref"
  } else if (local == "records" && has("doaj")) {
    "doaj"
  } else if (local == "xml" && has("<records>\\s*<record>") && has("<ref-type")) {
    "endnote"
  } else if (local == "XML" && has("<ISCJOURNAL")) {
    "isc"
  } else if (local == "pdf2xml") {
    "pdf2xml"
  } else if (local == "document" && has("<page[\\s>]") && has("<line[\\s>]")) {
    "ocr_lines"
  } else if (local == "root" && has("<segment[\\s>]")) {
    "segments"
  } else if (local == "Aps") {
    "aps"
  } else if (local == "Workbook") {
    "spreadsheet"
  } else if (tolower(local) %in% c("error", "service-error", "errors", "fault")) {
    "error_response"
  } else if (tolower(local) %in% html_roots || grepl("<!DOCTYPE html", dt, ignore.case = TRUE)) {
    "html"
  } else {
    "unknown"
  }
  list(format = format, root = root, doctype = doctype)
}

#' Read an XML file that may use named character entities
#'
#' A JATS file can use named entities such as `&mdash;` that its DTD defines.
#' The DTD is not loaded (that would mean downloading it), so such a file does
#' not read. The entities HTML knows are replaced by their characters first;
#' an unknown entity is removed.
#'
#' @param path path to the XML file
#' @returns an xml2 document with namespaces stripped
#' @noRd
.xml_read_publisher <- function(path) {
  # a link whose xlink namespace the file never declares still reads; the
  # warning about it is not the reader's concern. An undefined entity is
  # dropped by libxml2 with a warning ("Entity 'mdash' not defined"): the
  # file is then read again with its entities replaced.
  undefined <- FALSE
  quiet_read <- function(x) {
    withCallingHandlers(
      xml2::read_xml(x, options = "HUGE"),
      warning = \(w) {
        msg <- conditionMessage(w)
        if (grepl("Entity '.*' not defined", msg)) undefined <<- TRUE
        # (De Gruyter writes an id twice, as id and as a prefixed id)
        if (grepl("Namespace prefix|Entity '.*' not defined|Namespaced Attribute .* redefined", msg)) {
          invokeRestart("muffleWarning")
        }
      })
  }
  xml <- tryCatch(quiet_read(path), error = \(e) e)
  if (inherits(xml, "error") || undefined) {
    # read again from the text, repaired: in its own encoding, without the
    # control characters XML does not allow, with a bare "&" written as
    # "&amp;", and with HTML's named entities as characters
    txt <- .xml_text_utf8(path)
    txt <- sub("encoding=[\"'][^\"']+[\"']", "encoding=\"UTF-8\"", txt)
    txt <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", "", txt, perl = TRUE)
    txt <- gsub("&(?!#?[A-Za-z0-9]+;)", "&amp;", txt, perl = TRUE)
    ents <- unique(regmatches(txt, gregexpr("&[A-Za-z][A-Za-z0-9]*;", txt))[[1]])
    ents <- setdiff(ents, c("&amp;", "&lt;", "&gt;", "&quot;", "&apos;"))
    for (e in ents) {
      ch <- tryCatch(
        xml2::xml_text(xml2::read_html(paste0("<p>", e, "</p>"))),
        error = \(err) "")
      if (identical(ch, e)) ch <- "" # not an HTML entity
      # as a character reference, so a "<" or "&" stays text
      ref <- if (nzchar(ch)) paste0("&#", utf8ToInt(ch), ";", collapse = "") else ""
      txt <- gsub(e, ref, txt, fixed = TRUE)
    }
    # still broken (an end tag that does not match): read by the HTML
    # parser, which reads on (see .xml_parse_lenient())
    xml <- tryCatch(quiet_read(txt), error = \(e) {
      # without namespaces, as the XML parser reads them: xlink:href is
      # href, mml:math is math
      t <- sub("^\\s*<\\?xml[^>]*\\?>", "", txt, perl = TRUE)
      t <- gsub("\\sxlink:", " ", t, perl = TRUE)
      t <- gsub("<(/?)mml:", "<\\1", t, perl = TRUE)
      .xml_parse_lenient(t)
    })
  }
  xml2::xml_ns_strip(xml)
  xml
}

#' A copy of an element, to change without changing the document
#'
#' The copy keeps the `xlink:` prefix of its links but not the declaration
#' of that prefix, which libxml2 warns about; the warning is dropped, as is
#' the one about an id written twice (as id and as a prefixed id, De Gruyter).
#' @noRd
.xml_copy <- function(node) {
  withCallingHandlers(
    xml2::xml_root(xml2::read_xml(as.character(node))),
    warning = \(w) {
      if (grepl("Namespace prefix|Namespaced Attribute .* redefined", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
}

# TEI markup ----

.tei_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.tei_attr <- function(x) gsub("\"", "&quot;", .tei_esc(x), fixed = TRUE)

# runs of white space as one space; the byte order mark some files keep
# inside their text (Pensoft) removed
.tei_squish <- function(x) {
  x <- gsub(intToUtf8(0xFEFF), "", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

#' The text of TEI-style markup, without its tags
#' @noRd
.tei_markup_text <- function(markup) {
  vapply(markup, \(m) {
    if (is.na(m) || !nzchar(m)) return("")
    x <- tryCatch(xml2::read_xml(paste0("<x>", m, "</x>")), error = \(e) NULL)
    if (is.null(x)) return(.tei_squish(gsub("<[^>]+>", "", m)))
    .tei_squish(xml2::xml_text(x))
  }, character(1), USE.NAMES = FALSE)
}

# JATS to TEI ----

#' The text of a MathML formula, spaced as it is typeset
#'
#' The text of the elements alone runs together ("P(b>0)=.97",
#' "[0.09,0.17]"), which the statistics code does not read. Comparison signs
#' get a space on each side, a comma a space after it, a MathML space a
#' space; a fraction is a/b, a root sqrt(...), an exponent of a number ^, and
#' a fenced group keeps its brackets and separators.
#'
#' @param math a MathML `math` element
#' @returns character string
#' @noRd
.mathml_text <- function(math) .tei_squish(.mathml_walk(math))

# = < > and their relatives: le, ge, ne, approx, tilde operator, plus-minus
.mathml_rel <- function() {
  c("=", "<", ">", "~", intToUtf8(c(0x2264, 0x2265, 0x2260, 0x2248, 0x223C, 0xB1), multiple = TRUE))
}

#' A MathML node's text, spaced as it is typeset (see `.mathml_text()`)
#' @noRd
.mathml_walk <- function(node) {
  rel <- .mathml_rel()
  minus <- intToUtf8(0x2212)
  walk <- function(n) {
    if (xml2::xml_type(n) != "element") return(xml2::xml_text(n))
    kids <- xml2::xml_children(n)
    k <- function(i) if (length(kids) >= i) walk(kids[[i]]) else ""
    switch(
      xml2::xml_name(n),
      mspace = " ",
      mo = {
        t <- trimws(xml2::xml_text(n))
        if (t %in% rel) paste0(" ", t, " ") else if (t == ",") ", " else t
      },
      mi = , mn = , mtext = , ms = xml2::xml_text(n),
      msup = {
        b <- k(1)
        e <- k(2)
        if (grepl("[0-9]$", b) && grepl(paste0("^[-+0-9", minus, "]"), e)) paste0(b, "^", e) else paste0(b, e)
      },
      mfrac = paste0(k(1), "/", k(2)),
      msqrt = paste0("sqrt(", paste(vapply(kids, walk, ""), collapse = ""), ")"),
      mfenced = {
        open <- xml2::xml_attr(n, "open") %||% NA
        close <- xml2::xml_attr(n, "close") %||% NA
        sep <- xml2::xml_attr(n, "separators") %||% NA
        if (is.na(open)) open <- "("
        if (is.na(close)) close <- ")"
        if (is.na(sep)) sep <- ","
        sep <- substr(gsub("\\s", "", sep), 1, 1)
        if (sep == ",") sep <- ", "
        paste0(open, paste(vapply(kids, walk, ""), collapse = sep), close)
      },
      semantics = k(1), # the presentation, not its annotations
      annotation = , "annotation-xml" = "",
      paste(vapply(kids, walk, ""), collapse = "")
    )
  }
  walk(node)
}

#' The main comparison of a MathML formula: its sides and its sign
#'
#' The sign that compares the formula's top-level parts, not one inside a
#' bracket or a subscript: in P(b > 0) = .97 it is the "=", with "P(b > 0)" on
#' its left, where reading the typeset text finds "b > 0" first. A formula
#' with no comparison at its top level has none.
#'
#' @param math a MathML `math` element
#' @returns a list of list(text, lhs, comp, rhs), one per comparison; empty
#'   without one
#' @noRd
.mathml_relation <- function(math) {
  rel <- .mathml_rel()
  top <- math
  # down through the wrappers that hold a single part
  repeat {
    kids <- xml2::xml_children(top)
    if (length(kids) == 1 && xml2::xml_name(kids[[1]]) %in% c("mrow", "semantics", "mstyle")) {
      top <- kids[[1]]
    } else if (xml2::xml_name(top) == "semantics" && length(kids)) {
      top <- kids[[1]]
    } else {
      break
    }
  }
  kids <- xml2::xml_children(top)
  # brackets are often written as operators of their own ("<mo>(</mo>"), not
  # as a fenced group: a sign between them is inside the bracket
  mo <- vapply(kids, \(k) if (xml2::xml_name(k) == "mo") trimws(xml2::xml_text(k)) else "", "")
  depth <- cumsum(mo %in% c("(", "[", "{")) - cumsum(mo %in% c(")", "]", "}"))
  depth_before <- depth - (mo %in% c("(", "[", "{"))
  is_rel <- mo %in% rel & depth_before == 0
  side <- function(idx) .tei_squish(paste(vapply(kids[idx], .mathml_walk, ""), collapse = ""))
  # a top-level comma or semicolon separates comparisons ("P(b < 0) = .66,
  # P(b > 0) = .34"): each part gives its own
  sep <- mo %in% c(",", ";") & depth_before == 0
  part <- cumsum(sep)
  out <- list()
  for (p in unique(part)) {
    idx <- which(part == p & !sep)
    at <- idx[is_rel[idx]]
    if (!length(at) || at[1] == idx[1] || at[1] == idx[length(idx)]) next
    stop_at <- if (length(at) > 1) at[2] - 1L else idx[length(idx)]
    out[[length(out) + 1L]] <- list(
      text = .mathml_text(math), lhs = side(idx[idx < at[1]]),
      comp = mo[at[1]], rhs = side(seq(at[1] + 1, stop_at)))
  }
  out
}

#' The text of a formula: its MathML, else its TeX, else its text
#' @noRd
.jats_math_text <- function(node) {
  # MathML elements keep their mml: prefix when the namespaces are stripped,
  # so they are found by their local name
  math <- if (xml2::xml_name(node) == "math") node else
    xml2::xml_find_first(node, ".//*[local-name()='math']")
  if (!inherits(math, "xml_missing")) return(.mathml_text(math))
  tex <- xml2::xml_find_first(node, ".//*[local-name()='tex-math']")
  if (!inherits(tex, "xml_missing")) return(.tei_squish(xml2::xml_text(tex)))
  # a formula printed as an image: its number ("1") is not its text
  if (length(xml2::xml_find_all(node, "./label"))) {
    node <- .xml_copy(node)
    xml2::xml_remove(xml2::xml_find_all(node, "./label"))
  }
  .tei_squish(xml2::xml_text(node))
}

#' A JATS element's content as TEI-style inline markup
#'
#' Citations, figure, table and footnote references become Grobid's
#' `<ref type= target=>`, links `<ref type="url">`; formatting (italic,
#' bold, small caps) is dropped and its text kept. Floats and footnotes
#' printed inside a paragraph are left out here: they are read on their own.
#'
#' @param node an xml2 node
#' @param ids named lists mapping JATS ids to TEI ids (`bib`, `fig`, `tab`,
#'   `foot`)
#' @returns a character string of markup
#' @noRd
.jats_inline <- function(node, ids) {
  out <- character(0)
  # a citation range ("[12-16]", linked only at 12 and 16) cites the
  # references between its ends too: the last citation's reference number
  # and the text written since it
  last_bib <- NA_integer_
  since <- ""
  dashes <- c("-", intToUtf8(c(0x2010, 0x2011, 0x2012, 0x2013, 0x2014, 0x2212), multiple = TRUE))
  for (ch in xml2::xml_contents(node)) {
    type <- xml2::xml_type(ch)
    if (type == "text" || type == "cdata") {
      out <- c(out, .tei_esc(xml2::xml_text(ch)))
      since <- paste0(since, xml2::xml_text(ch))
      next
    }
    if (type != "element") next
    name <- xml2::xml_name(ch)
    if (name != "xref") since <- paste0(since, "x")
    piece <- switch(
      name,
      xref = {
        extra <- character(0)
        if (identical(xml2::xml_attr(ch, "ref-type"), "bibr")) {
          rid <- strsplit(xml2::xml_attr(ch, "rid") %||% "", "\\s+")[[1]]
          k <- suppressWarnings(as.integer(sub("^b", "", unname(ids$bib[rid]))))
          k <- k[!is.na(k)]
          gap <- gsub("[][()[:space:]]", "", since)
          if (length(k) && !is.na(last_bib) && gap %in% dashes && min(k) > last_bib + 1) {
            extra <- paste0("b", seq(last_bib + 1L, min(k) - 1L))
          }
          last_bib <- if (length(k)) max(k) else NA_integer_
        } else {
          last_bib <- NA_integer_
        }
        since <- ""
        .jats_xref(ch, ids, extra)
      },
      "ext-link" = ,
      uri = .jats_link(ch, ids),
      sup = {
        s <- .jats_inline(ch, ids)
        before <- paste(out, collapse = "")
        # an exponent: 10<sup>-5</sup> is 10^-5, not 10-5
        # (the minus sign is often the real minus, U+2212)
        if (grepl("[0-9]$", before) &&
            grepl(paste0("^[-+0-9", intToUtf8(0x2212), "]"), .tei_markup_text(s)) &&
            !grepl("<ref", s, fixed = TRUE)) paste0("^", s) else s
      },
      "inline-formula" = ,
      "disp-formula" = .tei_esc(.jats_math_text(ch)),
      "break" = " ",
      "list-item" = ,
      p = paste0(" ", .jats_inline(ch, ids), " "),
      # floats, footnotes and labels inside a paragraph are read on their own
      fn = ,
      "table-wrap" = ,
      "table-wrap-group" = ,
      fig = ,
      "fig-group" = ,
      "alt-text" = ,
      "long-desc" = ,
      "inline-graphic" = ,
      graphic = ,
      "object-id" = "",
      .jats_inline(ch, ids)
    )
    out <- c(out, piece)
  }
  paste(out, collapse = "")
}

#' A JATS cross-reference as Grobid's `<ref>`
#' @param extra further TEI targets (the references inside a citation range)
#' @noRd
.jats_xref <- function(node, ids, extra = character(0)) {
  text <- .jats_inline(node, ids)
  type <- xml2::xml_attr(node, "ref-type")
  tei_type <- switch(type %||% "", bibr = "bibr", fig = "figure", table = "table",
                     fn = "foot", "disp-formula" = "formula", NA_character_)
  if (is.na(tei_type)) return(text) # a reference to an affiliation, a table note
  key <- switch(tei_type, bibr = "bib", figure = "fig", table = "tab", foot = "foot", NA)
  rid <- strsplit(xml2::xml_attr(node, "rid") %||% "", "\\s+")[[1]]
  target <- if (!is.na(key)) unname(ids[[key]][rid]) else character(0)
  target <- c(extra, target[!is.na(target)])
  attr <- if (length(target)) {
    sprintf(" target=\"%s\"", paste0("#", target, collapse = " "))
  } else {
    ""
  }
  sprintf("<ref type=\"%s\"%s>%s</ref>", tei_type, attr, text)
}

#' A JATS link as Grobid's `<ref type="url">`
#' @noRd
.jats_link <- function(node, ids) {
  text <- .jats_inline(node, ids)
  href <- xml2::xml_attr(node, "href")
  if (is.na(href) || !nzchar(trimws(href))) href <- xml2::xml_text(node)
  href <- trimws(href)
  if (grepl("^10\\.[0-9]{4,}/", href)) href <- paste0("https://doi.org/", href)
  # a PubMed or GenBank number is not a link
  if (!grepl("^(https?|ftp)://|^www\\.", href, ignore.case = TRUE)) return(text)
  sprintf("<ref type=\"url\" target=\"%s\">%s</ref>", .tei_attr(href), text)
}

#' The paragraphs of a block element, as TEI `<p>` markup
#'
#' @param node a JATS block element (p, list, disp-quote, boxed-text, ...)
#' @returns character vector of `<p>...</p>` strings
#' @noRd
.jats_paragraphs <- function(node, ids) {
  name <- xml2::xml_name(node)
  if (name == "p") {
    m <- .tei_squish(.jats_inline(node, ids))
    return(if (nzchar(m)) paste0("<p>", m, "</p>") else character(0))
  }
  if (name %in% c("title", "label", "sec", "fig", "fig-group", "table-wrap",
                  "table-wrap-group", "fn-group", "fn", "ref-list", "graphic",
                  "media", "alternatives", "object-id", "permissions",
                  "sec-meta", "kwd-group")) {
    return(character(0))
  }
  if (name %in% c("disp-formula", "preformat", "code", "verse-line")) {
    m <- .tei_esc(.jats_math_text(node))
    return(if (nzchar(m)) paste0("<p>", m, "</p>") else character(0))
  }
  # containers (list, list-item, disp-quote, boxed-text, statement, def-list,
  # supplementary-material, caption, ...): their own paragraphs, in order.
  # An element with running text, or with only inline elements (a funding
  # statement with links in it), is one paragraph.
  kids <- xml2::xml_children(node)
  text_kids <- xml2::xml_find_all(node, "./text()")
  has_text <- any(nzchar(trimws(xml2::xml_text(text_kids))))
  if (!length(kids) || has_text || !any(xml2::xml_name(kids) %in% .jats_blocks)) {
    m <- .tei_squish(.jats_inline(node, ids))
    return(if (nzchar(m)) paste0("<p>", m, "</p>") else character(0))
  }
  unlist(lapply(kids, .jats_paragraphs, ids = ids))
}

# JATS elements that hold paragraphs rather than running text
.jats_blocks <- c(
  "p", "list", "list-item", "def-list", "def-item", "def", "disp-quote",
  "boxed-text", "statement", "speech", "verse-group", "sec", "app", "fig",
  "fig-group", "table-wrap", "table-wrap-group", "caption", "title", "label",
  "disp-formula", "disp-formula-group", "preformat", "code",
  "supplementary-material", "fn", "fn-group", "glossary", "ref-list",
  "media", "graphic", "alternatives", "permissions", "sec-meta", "kwd-group",
  "notes", "ack", "array", "chem-struct-wrap"
)

#' A section title as plain text, with its label as Grobid's `n`
#' @noRd
.jats_head <- function(node, ids) {
  title <- xml2::xml_find_first(node, "./title")
  label <- xml2::xml_find_first(node, "./label")
  head <- if (inherits(title, "xml_missing")) "" else .tei_markup_text(.jats_inline(title, ids))
  n <- if (inherits(label, "xml_missing")) "" else .tei_squish(xml2::xml_text(label))
  # "1." is written n="1", as Grobid writes it: the full stop inside the
  # heading's markup made the sentence splitter cut a heading-only section
  # into two text rows
  n <- sub("[.:]+$", "", n)
  list(head = head, n = n)
}

#' Flatten nested JATS sections into Grobid's flat divs
#'
#' Grobid writes one `<div>` per heading, never one inside another, and the
#' TEI reader expects that. Each section becomes a div with its own title and
#' its own paragraphs, followed by the divs of its subsections, in document
#' order. Paragraphs outside any section form a div without a title.
#'
#' @param node a body, sec, app or ack element
#' @returns a list of divs: `list(head, n, ps)`
#' @noRd
.jats_flatten <- function(node, ids, head = NULL) {
  divs <- list()
  cur <- list(head = head$head %||% "", n = head$n %||% "", ps = character(0))
  pushed <- FALSE
  # a section with only a title ("Study 1") is kept as a div without
  # paragraphs, as Grobid keeps it; a section's paragraphs after one of its
  # subsections form a second div under the same title
  flush <- function() {
    if (length(cur$ps) || (!pushed && nzchar(cur$head))) {
      divs[[length(divs) + 1L]] <<- cur
      pushed <<- TRUE
    }
    cur$ps <<- character(0)
  }
  for (ch in xml2::xml_children(node)) {
    name <- xml2::xml_name(ch)
    if (name %in% c("sec", "app")) {
      flush()
      divs <- c(divs, .jats_flatten(ch, ids, .jats_head(ch, ids)))
      # paragraphs after a subsection stay under this section's title
    } else {
      cur$ps <- c(cur$ps, .jats_paragraphs(ch, ids))
    }
  }
  flush()
  divs
}

#' Grobid TEI for a list of divs
#' @noRd
.tei_divs <- function(divs, type = NULL) {
  vapply(divs, \(d) {
    n <- if (nzchar(d$n)) sprintf(" n=\"%s\"", .tei_attr(d$n)) else ""
    head <- if (nzchar(d$head)) sprintf("<head%s>%s</head>", n, .tei_esc(d$head)) else ""
    sprintf("<div%s>%s%s</div>",
            if (is.null(type)) "" else sprintf(" type=\"%s\"", type),
            head, paste(d$ps, collapse = ""))
  }, character(1)) |> paste(collapse = "\n")
}

#' The type of a statement in the back matter, as Grobid names it
#'
#' From the publisher's declared type (`sec-type`, `fn-type`, `notes-type`)
#' and the statement's title: conflicts of interest, funding, data
#' availability, author contributions, ethics, acknowledgements; anything
#' else is an appendix ("annex").
#' @noRd
.jats_back_type <- function(declared, title) {
  declared <- if (is.null(declared) || is.na(declared)) "" else declared
  title <- if (is.null(title) || is.na(title)) "" else title
  key <- tolower(paste(declared, title))
  if (grepl("conflict|competing|\\bcoi\\b|declaration of interest", key)) return("conflict")
  if (grepl("\\bfund|financ|supported-by|\\bgrant", key)) return("funding")
  if (grepl("availab|open practice|transparen|open data|data.?sharing|data.?access", key)) {
    return("availability")
  }
  if (grepl("contribut|^con\\b|credit", key)) return("contribution")
  if (grepl("ethic|consent|institutional review", key)) return("ethics")
  if (grepl("acknowledg", key)) return("acknowledgement")
  "annex"
}

#' The statement a section heading names, when the whole heading is one of
#' the standard statement headings
#'
#' Publishers also print statements as ordinary top-level sections of the
#' body (Elsevier: "Funding", "Declaration of competing interest", "CRediT
#' authorship contribution statement"). Only a heading that is wholly a
#' standard statement heading counts, so a section such as "Funding of public
#' goods" stays a section of the paper.
#'
#' @param title a section heading
#' @returns the statement type as `.jats_back_type()` names it, or NA
#' @noRd
.statement_heading <- function(title) {
  t <- tolower(.tei_squish(gsub("[[:punct:]]+$", "", title %||% "")))
  # "Authors' contributions" written with the typographic apostrophe (U+2019)
  t <- gsub(intToUtf8(0x2019), "'", t, fixed = TRUE)
  patterns <- c(
    conflict = "^(declarations? of (competing|conflicting) interests?( statement)?|(statement of )?competing interests?( statement)?|conflicts? of interests?( statement)?|disclosure statement|disclosures?)$",
    funding = "^(funding( sources?| information| statement| details)?|financial (support|disclosure)|sources? of funding|role of the funding source)$",
    availability = "^((research )?data( and (code|materials?|software))? availability( statement)?|availability of (data|code|materials?)( and (code|materials?|data))?|data (sharing|access)( statement)?|open (practices|data|science)( statement)?|transparency and openness)$",
    contribution = "^((credit )?author(s'?|ship)? contributions?( statement)?|credit authorship contribution statement|credit author statement|contributions)$",
    ethics = "^(ethic(s|al)( approval| statement| considerations| declarations?)?|ethics approval and consent to participate|statement of ethics|informed consent)$",
    acknowledgement = "^acknowledge?ments?$"
  )
  for (n in names(patterns)) if (grepl(patterns[[n]], t)) return(n)
  NA_character_
}

#' Table cells, with row and column spans filled in
#'
#' @param table a JATS (XHTML) table element
#' @returns TEI `<table>` markup, Grobid's `<row>`/`<cell>`, or "" without rows
#' @noRd
.jats_table_tei <- function(table, ids) {
  if (inherits(table, "xml_missing")) return("")
  rows <- xml2::xml_find_all(table, ".//tr")
  if (!length(rows)) return("")
  grid <- list()
  pending <- list() # column -> list(text, rows left) for row spans
  for (r in seq_along(rows)) {
    cells <- xml2::xml_find_all(rows[[r]], "./th | ./td")
    out <- character(0)
    col <- 1L
    place <- function() {
      while (!is.null(pending[[as.character(col)]])) {
        p <- pending[[as.character(col)]]
        out[col] <<- p$text
        p$left <- p$left - 1L
        pending[[as.character(col)]] <<- if (p$left > 0) p else NULL
        col <<- col + 1L
      }
    }
    for (cell in cells) {
      place()
      text <- .tei_markup_text(.jats_inline(cell, ids))
      cs <- suppressWarnings(as.integer(xml2::xml_attr(cell, "colspan")))
      rs <- suppressWarnings(as.integer(xml2::xml_attr(cell, "rowspan")))
      if (is.na(cs) || cs < 1) cs <- 1L
      if (is.na(rs) || rs < 1) rs <- 1L
      for (k in seq_len(cs)) {
        out[col] <- text
        if (rs > 1) pending[[as.character(col)]] <- list(text = text, left = rs - 1L)
        col <- col + 1L
      }
    }
    place()
    out[is.na(out)] <- ""
    grid[[r]] <- out
  }
  tei_rows <- vapply(grid, \(g) {
    paste0("<row>", paste0("<cell>", .tei_esc(g), "</cell>", collapse = ""), "</row>")
  }, character(1))
  paste0("<table>", paste(tei_rows, collapse = ""), "</table>")
}

#' A JATS reference as Grobid's `<biblStruct>`
#'
#' @param ref a JATS `<ref>` element
#' @param id the TEI id ("b0", ...)
#' @returns TEI markup
#' @noRd
.jats_bibl <- function(ref, id) {
  cit <- xml2::xml_find_first(ref, ".//element-citation | .//mixed-citation | .//citation | .//nlm-citation")
  if (inherits(cit, "xml_missing")) cit <- ref
  ptype <- tolower(xml2::xml_attr(cit, "publication-type") %||%
                     xml2::xml_attr(cit, "citation-type") %||% "")
  if (is.na(ptype)) ptype <- ""
  one <- function(xpath) {
    n <- xml2::xml_find_first(cit, xpath)
    if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
  }
  persons <- function(xpath) {
    nodes <- xml2::xml_find_all(cit, xpath)
    vapply(nodes, \(p) {
      if (xml2::xml_name(p) == "collab") {
        return(sprintf("<persName><surname>%s</surname></persName>",
                       .tei_esc(.tei_squish(xml2::xml_text(p)))))
      }
      sur <- .tei_squish(xml2::xml_text(xml2::xml_find_first(p, "./surname")))
      giv <- .tei_squish(xml2::xml_text(xml2::xml_find_first(p, "./given-names")))
      if (is.na(sur) || !nzchar(sur)) sur <- .tei_squish(xml2::xml_text(p))
      if (is.na(giv)) giv <- ""
      sprintf("<persName><forename>%s</forename><surname>%s</surname></persName>",
              .tei_esc(giv), .tei_esc(sur))
    }, character(1))
  }
  people <- "./person-group[@person-group-type='author' or not(@person-group-type)]/*[self::name or self::string-name or self::collab]"
  authors <- persons(people)
  if (!length(authors)) authors <- persons("./name | ./string-name | ./collab")
  editors <- persons("./person-group[@person-group-type='editor']/*[self::name or self::string-name or self::collab]")

  title <- one("./article-title")
  chapter <- one("./chapter-title")
  if (!nzchar(title)) title <- chapter
  if (!nzchar(title)) title <- one("./data-title")
  source <- one("./source")
  year <- one("./year")
  if (!nzchar(year)) year <- one("./date/year")
  if (!nzchar(year)) {
    # a reference printed as plain text: the year in its text, in
    # parentheses as APA prints it, else the first year-shaped number
    txt <- .tei_squish(xml2::xml_text(cit))
    y <- regmatches(txt, regexpr("\\((1[89]|20)[0-9]{2}[a-z]?\\)", txt))
    if (!length(y)) y <- regmatches(txt, regexpr("\\b(1[89]|20)[0-9]{2}[a-z]?\\b", txt))
    if (length(y)) year <- gsub("[()]", "", y)
  }
  volume <- one("./volume")
  issue <- one("./issue")
  fpage <- one("./fpage")
  lpage <- one("./lpage")
  if (!nzchar(fpage)) fpage <- one("./elocation-id")
  publisher <- one("./publisher-name")
  doi <- one(".//pub-id[@pub-id-type='doi'] | .//object-id[@pub-id-type='doi']")
  if (!nzchar(doi)) {
    links <- xml2::xml_find_all(cit, ".//ext-link | .//uri")
    hrefs <- c(xml2::xml_attr(links, "href"), xml2::xml_text(links))
    hit <- regmatches(hrefs, regexpr("10\\.[0-9]{4,}/[^[:space:]]+", hrefs))
    if (length(hit)) doi <- hit[[1]]
  }
  is_journal <- ptype %in% c("journal", "article", "periodical") ||
    (!nzchar(ptype) && nzchar(volume) && nzchar(source) && !nzchar(chapter))
  level <- if (is_journal) "j" else "m"

  # the reference as printed: a mixed citation's text; an element citation's
  # parts, joined in the order of an APA reference; the printed text itself
  # when the file gives it as a second citation (Elsevier)
  raw <- .tei_squish(xml2::xml_text(cit))
  printed <- xml2::xml_find_first(ref, "./mixed-citation[@specific-use='source-text']")
  if (!inherits(printed, "xml_missing")) {
    raw <- .tei_squish(xml2::xml_text(printed))
  } else if (xml2::xml_name(cit) == "element-citation") {
    names_txt <- .tei_markup_text(authors)
    au <- if (length(names_txt)) paste(names_txt, collapse = ", ") else ""
    pages <- paste(c(fpage, lpage)[nzchar(c(fpage, lpage))], collapse = "-")
    vol <- paste0(volume, if (nzchar(issue)) paste0("(", issue, ")") else "")
    parts <- c(
      au, if (nzchar(year)) paste0("(", year, ")") else "",
      title, paste(c(source, vol, pages)[nzchar(c(source, vol, pages))], collapse = ", "),
      publisher, if (nzchar(doi)) paste0("https://doi.org/", doi) else ""
    )
    parts <- parts[nzchar(parts)]
    if (length(parts)) raw <- paste(sub("\\.$", "", parts), collapse = ". ")
  }

  imprint <- paste0(
    if (nzchar(publisher)) sprintf("<publisher>%s</publisher>", .tei_esc(publisher)) else "",
    if (nzchar(year)) sprintf("<date type=\"published\">%s</date>", .tei_esc(year)) else "",
    if (nzchar(volume)) sprintf("<biblScope unit=\"volume\">%s</biblScope>", .tei_esc(volume)) else "",
    if (nzchar(issue)) sprintf("<biblScope unit=\"issue\">%s</biblScope>", .tei_esc(issue)) else "",
    if (nzchar(fpage) || nzchar(lpage)) {
      sprintf("<biblScope unit=\"page\"%s%s/>",
              if (nzchar(fpage)) sprintf(" from=\"%s\"", .tei_attr(fpage)) else "",
              if (nzchar(lpage)) sprintf(" to=\"%s\"", .tei_attr(lpage)) else "")
    } else ""
  )
  analytic <- if (nzchar(chapter) || (is_journal && nzchar(title)) ||
                  (nzchar(title) && nzchar(source))) {
    sprintf("<analytic><title level=\"a\">%s</title>%s</analytic>",
            .tei_esc(title), paste0("<author>", authors, "</author>", collapse = ""))
  } else ""
  monogr_title <- if (nzchar(analytic)) source else if (nzchar(title)) title else source
  monogr_authors <- if (nzchar(analytic)) "" else paste0("<author>", authors, "</author>", collapse = "")
  if (!length(authors)) monogr_authors <- ""
  monogr <- sprintf(
    "<monogr>%s%s%s<imprint>%s</imprint></monogr>",
    if (nzchar(monogr_title)) sprintf("<title level=\"%s\">%s</title>", level, .tei_esc(monogr_title)) else "",
    monogr_authors,
    paste0("<editor>", editors, "</editor>", collapse = ""),
    imprint
  )
  if (!length(editors)) monogr <- sub("<editor></editor>", "", monogr, fixed = TRUE)
  sprintf("<biblStruct xml:id=\"%s\">%s%s%s<note type=\"raw_reference\">%s</note></biblStruct>",
          id, analytic, monogr,
          if (nzchar(doi)) sprintf("<idno type=\"DOI\">%s</idno>", .tei_esc(doi)) else "",
          .tei_esc(raw))
}

#' The text of an affiliation, without its label
#' @noRd
.jats_aff_text <- function(aff) {
  aff <- .xml_copy(aff)
  # the institution's identifiers (ROR, Ringgold) are not part of its name
  xml2::xml_remove(xml2::xml_find_all(aff, ".//label | .//sup[not(ancestor::institution)] | .//institution-id"))
  parts <- xml2::xml_children(aff)
  named <- parts[xml2::xml_name(parts) %in% c("institution", "institution-wrap",
                                              "addr-line", "city", "state",
                                              "country", "postal-code")]
  if (length(named) && length(named) == length(parts)) {
    txt <- vapply(named, \(n) .tei_squish(xml2::xml_text(n)), character(1))
    return(paste(txt[nzchar(txt)], collapse = ", "))
  }
  .tei_squish(xml2::xml_text(aff))
}

#' Translate a JATS article into the TEI Grobid writes
#'
#' @param xml a JATS document (namespaces stripped)
#' @returns a list: `tei` (the TEI document as a string) and `meta` (the
#'   metadata TEI has no place for, for the paper's info table)
#' @noRd
.jats_to_tei <- function(xml) {
  art <- xml2::xml_find_first(xml, "/article")
  if (inherits(art, "xml_missing")) art <- xml2::xml_root(xml)
  front <- xml2::xml_find_first(art, "./front")
  meta_node <- xml2::xml_find_first(front, "./article-meta")
  journal_node <- xml2::xml_find_first(front, "./journal-meta")
  body <- xml2::xml_find_first(art, "./body")
  back <- xml2::xml_find_first(art, "./back")
  m1 <- function(node, xpath) {
    if (inherits(node, "xml_missing")) return("")
    n <- xml2::xml_find_first(node, xpath)
    if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
  }

  # ids: JATS id -> TEI id, for cross-references ----
  # the article itself: not the peer reviews, decision letters and author
  # responses PLOS and eLife add as sub-articles
  own <- "[not(ancestor::sub-article) and not(ancestor::response)]"
  in_supp <- "[not(ancestor::supplementary-material)]"
  figs <- xml2::xml_find_all(art, paste0("//fig", in_supp, own))
  tabs <- xml2::xml_find_all(art, paste0("//table-wrap", in_supp, own))
  refs <- xml2::xml_find_all(art, paste0("//ref-list//ref", own))
  fns <- xml2::xml_find_all(art, paste0("//fn[not(ancestor::table-wrap-foot) and not(ancestor::table-wrap)]", own))
  fn_type <- tolower(xml2::xml_attr(fns, "fn-type"))
  fn_label <- vapply(fns, \(f) m1(f, "./label | ./title"), character(1))
  fn_first <- vapply(fns, \(f) substr(.tei_squish(xml2::xml_text(f)), 1, 80), character(1))
  in_notes <- vapply(fns, \(f) {
    length(xml2::xml_find_all(f, "ancestor::author-notes | ancestor::fn-group[parent::back]")) > 0
  }, logical(1))
  # a statement (conflicts, funding, contributions...): back matter; any
  # other footnote is a footnote
  fn_stmt_type <- mapply(\(t, l, f) {
    b <- .jats_back_type(t, if (nzchar(l)) l else f)
    if (b == "annex") NA_character_ else b
  }, fn_type, fn_label, fn_first, USE.NAMES = FALSE)
  # a note about one author ("contributed equally", a present address) is no
  # statement, whatever its words
  is_stmt <- !is.na(fn_stmt_type) & (in_notes | !is.na(fn_type)) &
    !fn_type %in% c("corresp", "equal", "present-address", "current-aff", "deceased", "edited-by")
  # notes about the authors and editors are not footnotes of the text; the
  # editors and reviewers they name are contributors (see .jats_extras())
  about_people <- in_notes & (fn_type %in% c("corresp", "equal", "present-address", "current-aff",
                                             "deceased", "other", "edited-by") |
                                !is.na(xml2::xml_attr(fns, "custom-type")))
  is_note <- !is_stmt & !about_people
  is_note[is.na(is_note)] <- TRUE

  id_map <- function(nodes, prefix) {
    jid <- xml2::xml_attr(nodes, "id")
    # APA puts a reference's id on its citation, not on <ref>
    inner <- vapply(nodes, \(n) {
      xml2::xml_attr(xml2::xml_find_first(n, "./*[@id]"), "id") %||% NA_character_
    }, character(1))
    jid[is.na(jid)] <- inner[is.na(jid)]
    tid <- paste0(prefix, seq_along(nodes) - 1L)
    stats::setNames(tid, jid)[!is.na(jid)]
  }
  ids <- list(bib = id_map(refs, "b"), fig = id_map(figs, "fig_"),
              tab = id_map(tabs, "tab_"), foot = id_map(fns[is_note], "foot_"))

  # header: title, authors, DOI, keywords, abstract ----
  title <- m1(meta_node, "./title-group/article-title")
  subtitle <- m1(meta_node, "./title-group/subtitle")
  if (nzchar(subtitle)) title <- paste0(sub("[:.]$", "", title), ": ", subtitle)
  doi <- m1(meta_node, "./article-id[@pub-id-type='doi']")

  affs <- xml2::xml_find_all(art, paste0("//aff[not(ancestor::ref-list)]", own))
  aff_text <- stats::setNames(vapply(affs, .jats_aff_text, character(1)),
                              xml2::xml_attr(affs, "id"))
  corresp_email <- m1(meta_node, ".//author-notes//email | .//corresp//email")
  contribs <- xml2::xml_find_all(meta_node, ".//contrib-group/contrib[not(@contrib-type) or @contrib-type='author']")
  authors <- vapply(contribs, \(cb) {
    nm <- xml2::xml_find_first(cb, "./name | ./string-name | ./name-alternatives/name")
    if (inherits(nm, "xml_missing")) return("") # a group author
    given <- m1(nm, "./given-names")
    family <- m1(nm, "./surname")
    if (!nzchar(family)) family <- .tei_squish(xml2::xml_text(nm))
    email <- m1(cb, ".//email")
    if (!nzchar(email) && identical(xml2::xml_attr(cb, "corresp"), "yes")) email <- corresp_email
    orcid <- m1(cb, "./contrib-id[@contrib-id-type='orcid']")
    # a link to the affiliation, or (APA) the author's own rid attribute
    rid <- c(xml2::xml_attr(xml2::xml_find_all(cb, "./xref[@ref-type='aff']"), "rid"),
             xml2::xml_attr(cb, "rid"))
    rid <- unlist(strsplit(rid[!is.na(rid)], "\\s+"))
    aff <- unname(aff_text[rid[rid %in% names(aff_text)]])
    own <- xml2::xml_find_first(cb, "./aff")
    if (!length(aff) && !inherits(own, "xml_missing")) aff <- .jats_aff_text(own)
    if (!length(aff) && length(aff_text) == 1) aff <- unname(aff_text)
    paste0(
      "<author><persName>",
      sprintf("<forename type=\"first\">%s</forename>", .tei_esc(given)),
      sprintf("<surname>%s</surname>", .tei_esc(family)),
      "</persName>",
      if (nzchar(email)) sprintf("<email>%s</email>", .tei_esc(email)) else "",
      if (nzchar(orcid)) sprintf("<idno type=\"ORCID\">%s</idno>", .tei_esc(orcid)) else "",
      paste0("<affiliation>", .tei_esc(aff), "</affiliation>", collapse = ""),
      "</author>")
  }, character(1))
  authors <- authors[nzchar(authors)]

  # the authors' keywords: a typed group (research organism, JEL codes, an
  # assessment's terms) holds subject terms (see .jats_extras())
  kwd <- xml2::xml_find_all(meta_node, paste0(
    ".//kwd-group[not(@kwd-group-type) or @kwd-group-type='author' or ",
    "@kwd-group-type='author-keywords' or @kwd-group-type='author-generated']/kwd"))
  keywords <- vapply(kwd, \(k) .tei_squish(xml2::xml_text(k)), character(1))
  abstracts <- xml2::xml_find_all(meta_node, "./abstract")
  if (length(abstracts) > 1) {
    plain <- abstracts[is.na(xml2::xml_attr(abstracts, "abstract-type"))]
    abstracts <- if (length(plain)) plain else abstracts[1]
  }
  abstract_ps <- unlist(lapply(abstracts, \(a) {
    divs <- .jats_flatten(a, ids)
    unlist(lapply(divs, `[[`, "ps"))
  }))

  # body ----
  # top-level sections that are statements by their heading go to the back
  # matter (see .statement_heading())
  body_stmts <- list()
  if (!inherits(body, "xml_missing")) {
    for (s in xml2::xml_find_all(body, "./sec")) {
      type <- .statement_heading(.jats_head(s, ids)$head)
      if (is.na(type)) next
      body_stmts[[length(body_stmts) + 1L]] <- list(
        type = type, divs = .jats_flatten(s, ids, .jats_head(s, ids)))
      xml2::xml_remove(s)
    }
  }
  body_divs <- if (inherits(body, "xml_missing")) list() else .jats_flatten(body, ids)

  # floats ----
  caption_markup <- function(node) {
    cap <- xml2::xml_find_first(node, "./caption")
    if (inherits(cap, "xml_missing")) return("")
    parts <- vapply(xml2::xml_children(cap), \(k) .tei_squish(.jats_inline(k, ids)), character(1))
    paste(parts[nzchar(parts)], collapse = " ")
  }
  float_tei <- function(node, i, is_table) {
    label <- m1(node, "./label")
    number <- .tei_squish(sub("^(fig(ure)?|tab(le)?|scheme|chart|plate|box)\\.?", "", label, ignore.case = TRUE))
    number <- sub("[.:]$", "", number)
    desc <- caption_markup(node)
    if (is_table) {
      foot <- xml2::xml_find_all(node, "./table-wrap-foot//p | ./table-wrap-foot/fn[not(p)]")
      foot <- vapply(foot, \(f) .tei_squish(.jats_inline(f, ids)), character(1))
      if (length(foot)) desc <- paste(c(desc, foot[nzchar(foot)]), collapse = " ")
    }
    head <- sub("[.:]$", "", label)
    # APA labels a float with its number only ("1")
    if (grepl("^[A-Z]?[0-9]", head)) head <- paste(if (is_table) "Table" else "Figure", head)
    sprintf("<figure%s xml:id=\"%s%d\"><head>%s</head><label>%s</label><figDesc>%s</figDesc>%s</figure>",
            if (is_table) " type=\"table\"" else "",
            if (is_table) "tab_" else "fig_", i - 1L,
            .tei_esc(head), .tei_esc(number), desc,
            if (is_table) .jats_table_tei(xml2::xml_find_first(node, ".//table"), ids) else "")
  }
  fig_tei <- vapply(seq_along(figs), \(i) float_tei(figs[[i]], i, FALSE), character(1))
  tab_tei <- vapply(seq_along(tabs), \(i) float_tei(tabs[[i]], i, TRUE), character(1))

  notes <- fns[is_note]
  # a copy of an element without its label, to read its text
  without_label <- function(node) {
    tmp <- .xml_copy(node)
    xml2::xml_remove(xml2::xml_find_all(tmp, "./label | ./title"))
    tmp
  }
  note_tei <- vapply(seq_along(notes), \(i) {
    lab <- m1(notes[[i]], "./label")
    tmp <- without_label(notes[[i]])
    sprintf("<note place=\"foot\"%s xml:id=\"foot_%d\">%s</note>",
            if (nzchar(lab)) sprintf(" n=\"%s\"", .tei_attr(lab)) else "",
            i - 1L, .tei_squish(.jats_inline(tmp, ids)))
  }, character(1))

  # back matter: statements, acknowledgements, appendices ----
  back_divs <- list() # type -> list of divs
  add_back <- function(type, divs) {
    if (!length(divs)) return()
    back_divs[[type]] <<- c(back_divs[[type]], divs)
  }
  if (!inherits(back, "xml_missing")) {
    for (ch in xml2::xml_children(back)) {
      name <- xml2::xml_name(ch)
      if (name == "ack") {
        h <- .jats_head(ch, ids)
        if (!nzchar(h$head)) h$head <- "Acknowledgments"
        add_back("acknowledgement", .jats_flatten(ch, ids, h))
      } else if (name == "app-group") {
        add_back("annex", .jats_flatten(ch, ids))
      } else if (name == "app") {
        add_back("annex", .jats_flatten(ch, ids, .jats_head(ch, ids)))
      } else if (name %in% c("sec", "notes")) {
        h <- .jats_head(ch, ids)
        declared <- xml2::xml_attr(ch, if (name == "sec") "sec-type" else "notes-type")
        add_back(.jats_back_type(declared, h$head), .jats_flatten(ch, ids, h))
      }
    }
  }
  for (s in body_stmts) add_back(s$type, s$divs)
  # paragraphs written straight into the author notes (APA puts its
  # conflict-of-interest, data, preregistration and funding statements in one
  # such paragraph): an "Author Note", as Grobid reads the printed author note
  if (!inherits(meta_node, "xml_missing")) {
    an <- xml2::xml_find_all(meta_node, "./author-notes/p")
    ps <- unlist(lapply(an, .jats_paragraphs, ids = ids))
    if (length(ps)) add_back("acknowledgement", list(list(head = "Author Note", n = "", ps = ps)))
  }
  # statements in the front matter: the funding statement, and named fields
  # (PLOS: "Data Availability") ----
  if (!inherits(meta_node, "xml_missing")) {
    for (fs in xml2::xml_find_all(meta_node, ".//funding-group/funding-statement")) {
      ps <- .jats_paragraphs(fs, ids)
      if (!length(ps)) ps <- paste0("<p>", .tei_squish(.jats_inline(fs, ids)), "</p>")
      add_back("funding", list(list(head = "Funding", n = "", ps = ps)))
    }
    for (cm in xml2::xml_find_all(meta_node, ".//custom-meta")) {
      name <- m1(cm, "./meta-name")
      type <- .jats_back_type(xml2::xml_attr(cm, "id"), name)
      value <- xml2::xml_find_first(cm, "./meta-value")
      if (type == "annex" || inherits(value, "xml_missing")) next
      m <- .tei_squish(.jats_inline(value, ids))
      if (nzchar(m)) add_back(type, list(list(head = name, n = "", ps = paste0("<p>", m, "</p>"))))
    }
  }
  for (i in which(is_stmt)) {
    f <- fns[[i]]
    lab <- m1(f, "./label | ./title")
    tmp <- without_label(f)
    ps <- unlist(lapply(xml2::xml_children(tmp), .jats_paragraphs, ids = ids))
    if (!length(ps) && nzchar(.tei_squish(xml2::xml_text(tmp)))) {
      ps <- paste0("<p>", .tei_squish(.jats_inline(tmp, ids)), "</p>")
    }
    add_back(fn_stmt_type[[i]], list(list(head = lab, n = "", ps = ps)))
  }
  back_tei <- vapply(names(back_divs), \(t) {
    sprintf("<div type=\"%s\">%s</div>", t, .tei_divs(back_divs[[t]]))
  }, character(1))

  bibl <- vapply(seq_along(refs), \(i) .jats_bibl(refs[[i]], paste0("b", i - 1L)), character(1))

  # the metadata TEI has no place for ----
  pub_date <- function() {
    dates <- xml2::xml_find_all(meta_node, "./pub-date")
    if (!length(dates)) return(c(NA_character_, NA_character_))
    type <- paste(xml2::xml_attr(dates, "pub-type"), xml2::xml_attr(dates, "date-type"))
    pick <- order(!grepl("epub", type), !grepl("ppub|pub", type))[1]
    d <- dates[[pick]]
    y <- m1(d, "./year")
    mo <- suppressWarnings(as.integer(m1(d, "./month")))
    da <- suppressWarnings(as.integer(m1(d, "./day")))
    iso <- if (!nzchar(y)) NA_character_ else if (is.na(mo)) y else if (is.na(da)) {
      sprintf("%s-%02d", y, mo)
    } else sprintf("%s-%02d-%02d", y, mo, da)
    c(iso, if (nzchar(y)) y else NA_character_)
  }
  pd <- pub_date()
  license <- xml2::xml_find_first(meta_node, ".//permissions/license")
  license_url <- if (inherits(license, "xml_missing")) "" else xml2::xml_attr(license, "href") %||% ""
  if (is.na(license_url) || !nzchar(license_url)) license_url <- m1(meta_node, ".//permissions//license_ref")
  stmt_text <- function(type) {
    d <- back_divs[[type]]
    if (!length(d)) return(NA_character_)
    paste(.tei_markup_text(unlist(lapply(d, `[[`, "ps"))), collapse = " ")
  }
  issn <- m1(journal_node, "./issn[@pub-type='epub'] | ./issn[@publication-format='electronic']")
  if (!nzchar(issn)) issn <- m1(journal_node, "./issn")
  na <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) NA_character_ else x
  meta <- list(
    journal = na(m1(journal_node, ".//journal-title")),
    volume = na(m1(meta_node, "./volume")),
    issue = na(m1(meta_node, "./issue")),
    first_page = na(if (nzchar(m1(meta_node, "./fpage"))) m1(meta_node, "./fpage") else m1(meta_node, "./elocation-id")),
    last_page = na(m1(meta_node, "./lpage")),
    issn = na(issn),
    publisher = na(m1(journal_node, ".//publisher-name")),
    published = na(pd[[2]]),
    published_date = na(pd[[1]]),
    license = na(m1(meta_node, ".//permissions/license")),
    license_url = na(license_url),
    pmid = na(m1(meta_node, "./article-id[@pub-id-type='pmid']")),
    pmcid = na(m1(meta_node, "./article-id[@pub-id-type='pmc' or @pub-id-type='pmcid']")),
    language = na(xml2::xml_attr(art, "lang")),
    funding_statement = na(stmt_text("funding")),
    coi_statement = na(stmt_text("conflict")),
    ethics_statement = na(stmt_text("ethics")),
    data_availability = na(stmt_text("availability")),
    dtd_version = na(xml2::xml_attr(art, "dtd-version"))
  )
  if (!is.na(meta$pmcid) && !grepl("^PMC", meta$pmcid)) meta$pmcid <- paste0("PMC", meta$pmcid)

  # the TEI document ----
  abstract_tei <- if (length(abstract_ps)) {
    paste0("<abstract><div>", paste(abstract_ps, collapse = ""), "</div></abstract>")
  } else ""
  keyword_tei <- if (length(keywords)) {
    paste0("<textClass><keywords>", paste0("<term>", .tei_esc(keywords), "</term>", collapse = ""),
           "</keywords></textClass>")
  } else ""
  tei <- paste0(
    "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\"><teiHeader><fileDesc>",
    "<titleStmt><title level=\"a\" type=\"main\">", .tei_esc(title), "</title></titleStmt>",
    "<sourceDesc><biblStruct><analytic>", paste(authors, collapse = ""), "</analytic>",
    "<monogr><imprint/></monogr>",
    if (nzchar(doi)) sprintf("<idno type=\"DOI\">%s</idno>", .tei_esc(doi)) else "",
    "</biblStruct></sourceDesc></fileDesc>",
    "<encodingDesc><appInfo><application ident=\"jats\" version=\"",
    .tei_attr(if (is.na(meta$dtd_version)) "unknown" else meta$dtd_version),
    "\"/></appInfo></encodingDesc>",
    "<profileDesc>", keyword_tei, abstract_tei, "</profileDesc></teiHeader>",
    "<text><body>\n", .tei_divs(body_divs), "\n",
    paste(fig_tei, collapse = "\n"), "\n", paste(tab_tei, collapse = "\n"), "\n",
    paste(note_tei, collapse = "\n"), "\n</body><back>\n",
    paste(back_tei, collapse = "\n"),
    "<div type=\"references\"><listBibl>", paste(bibl, collapse = "\n"),
    "</listBibl></div></back></text></TEI>"
  )
  list(tei = tei, meta = meta)
}

#' Convert a JATS XML file to a paper
#'
#' Translates the file into the TEI Grobid writes (see `.jats_to_tei()`),
#' reads that with `.grobid_to_bibr12()`, and adds what TEI has no place for
#' to the info table: journal, volume, issue, pages, ISSN, publisher,
#' publication date, licence, PubMed ids, language and the funding,
#' conflict-of-interest, ethics and data-availability statements.
#'
#' @param xml_path path to a JATS XML file (or to the file `xml` was made from)
#' @param xml the JATS document, when it is not read from `xml_path` (the
#'   Elsevier converter passes its translation)
#' @param input_format the paper's `info$input_format`
#' @param producer who wrote the XML, for `extraction$producer`
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.jats_to_bibr12 <- function(xml_path, xml = NULL, input_format = "jats",
                            producer = "jats") {
  if (is.null(xml)) xml <- .xml_read_publisher(xml_path)
  # what the file declares beyond the text (12.6), read before the TEI
  # translation moves statement sections out of the body; the id maps only
  # matter for citations, which this plain text does not keep
  art <- xml2::xml_find_first(xml, "/article")
  if (inherits(art, "xml_missing")) art <- xml2::xml_root(xml)
  no_ids <- list(bib = character(0), fig = character(0), tab = character(0),
                 foot = character(0))
  extras <- .jats_extras(art, no_ids)
  conv <- .jats_to_tei(xml)

  # the TEI under the same file name, so the paper keeps its id
  dir <- tempfile("jats")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  tei_path <- file.path(dir, basename(xml_path))
  writeLines(enc2utf8(conv$tei), tei_path, useBytes = TRUE)

  # without the repairs of numbers a PDF needs (see .xml_read_grobid())
  paper <- .grobid_to_bibr12(tei_path, pdf_repairs = FALSE)

  # the source is the JATS file, not the temporary TEI
  paper$info$file_name <- basename(xml_path)
  paper$info$sha256 <- .bibr12_sha256(xml_path)
  paper$info$file_hash <- substr(paper$info$sha256, 1, 16)
  paper$info$input_format <- input_format
  for (col in names(conv$meta)) {
    if (col %in% names(paper$info) && !is.na(conv$meta[[col]])) {
      paper$info[[col]] <- conv$meta[[col]]
    }
  }
  w <- paper$extraction$warnings %||% list()
  w <- Filter(\(x) !identical(x$code, "METACHECK_SOURCE_IS_TEI"), w)
  paper$extraction$warnings <- w
  paper$extraction$producer <- list(
    name = producer,
    version = if (is.na(conv$meta$dtd_version)) "unknown" else conv$meta$dtd_version,
    build_sha = NULL)
  .jats_apply_extras(paper, extras)
}
