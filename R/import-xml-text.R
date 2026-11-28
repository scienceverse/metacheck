# Publisher XML: formats made by PDF, OCR and typesetting tools ----
#
# Journals also deliver the XML their tools write: Acrobat's "Save as XML"
# (<TaggedPDF-doc>, the PDF's structure tags), SciELO's markup (<doc sps=>,
# SciELO's format before JATS), poppler's pdftohtml -xml (<pdf2xml>, text
# lines with their position), a printer's page description (<Aps>, runs of
# glyphs with their position), OCR output (<document><page><par><line>), a
# list of words (<root><segment>) and spreadsheets a typesetting system wrote
# the article into (<Workbook>). Each is read as blocks (see
# R/import-xml-blocks.R); where the format only gives positions, lines are
# joined into paragraphs by the space between them, and a line in a larger
# font than the text is a heading.

# --- Acrobat's tagged PDF ----------------------------------------------------

#' Convert Acrobat's "Save as XML" to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.taggedpdf_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  rules <- list(italic = c("i", "em"), bold = c("b", "strong"), skip = c("figure", "artifact"),
                link = "link", href = function(n) .tei_squish(xml2::xml_text(n)), space = "br")
  blocks <- list()
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  walk <- function(node) {
    for (ch in xml2::xml_children(node)) {
      nm <- xml2::xml_name(ch)
      if (nm %in% c("xmpmeta", "xpacket", "Artifact", "Figure", "TOC", "TOCI", "Index")) next
      if (grepl("^H[1-6]?$", nm) || nm == "Title") {
        level <- if (nm %in% c("H", "Title")) 1L else as.integer(substring(nm, 2))
        txt <- .tei_squish(.blk_inline(ch, rules))
        if (nzchar(txt)) add(.blk_h(txt, level))
      } else if (nm %in% c("P", "BlockQuote", "Quote", "Note", "Caption", "Reference", "Code", "Formula")) {
        txt <- .tei_squish(.blk_inline(ch, rules))
        if (nzchar(.blk_plain(txt))) add(.blk_p(txt))
      } else if (nm == "L") {
        items <- vapply(xml2::xml_find_all(ch, "./LI"), \(li) {
          b <- xml2::xml_find_first(li, "./LBody")
          .tei_squish(.blk_inline(if (inherits(b, "xml_missing")) li else b, rules))
        }, "")
        if (length(items)) add(.blk_list(as.list(items[nzchar(items)])))
      } else if (nm == "Table") {
        rows <- lapply(xml2::xml_find_all(ch, ".//TR"), \(r) {
          vapply(xml2::xml_find_all(r, "./TH | ./TD"), \(c) .tei_squish(.blk_inline(c, rules)), "")
        })
        if (length(rows)) add(.blk_table(rows, head_rows = sum(vapply(xml2::xml_find_all(ch, ".//TR"),
          \(r) length(xml2::xml_find_all(r, "./TH")) > 0 && !length(xml2::xml_find_all(r, "./TD")), TRUE))))
      } else {
        # Part, Sect, Art, Div, Span and other containers
        walk(ch)
      }
    }
  }
  walk(xml2::xml_root(xml))
  m1 <- function(xpath) {
    n <- xml2::xml_find_first(xml, xpath)
    if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
  }
  creators <- vapply(xml2::xml_find_all(xml, "//xmpmeta//creator//li"), \(n) .tei_squish(xml2::xml_text(n)), "")
  meta <- list(title = m1("//xmpmeta//title//li"),
               authors = lapply(creators[nzchar(creators)], \(n) list(literal = n)),
               version = "acrobat-tagged-pdf")
  # a file name as the title (".indd", ".docx") is no title
  if (grepl("\\.(indd|docx?|pdf|rtf|odt)\\s*$", meta$title, ignore.case = TRUE)) meta$title <- ""
  .blocks_to_bibr12(xml_path, blocks, meta, "acrobat")
}

# --- SciELO's markup ---------------------------------------------------------

#' Convert a file in SciELO's markup (<doc sps=...>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.scielo_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  root <- xml2::xml_root(xml)
  rules <- list(italic = c("italic", "i", "em"), bold = c("bold", "b"), sup = "sup", sub = "sub",
                link = c("uri", "a", "ext-link"), href = function(n) xml2::xml_attr(n, "href") %NA% .tei_squish(xml2::xml_text(n)),
                skip = c("label", "graphic"), space = "br")
  inl <- function(n) .tei_squish(.blk_inline(n, rules))
  m1 <- function(node, xpath) {
    n <- xml2::xml_find_first(node, xpath)
    if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
  }
  # body
  blocks <- list()
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  walk <- function(node, depth) {
    for (ch in xml2::xml_children(node)) {
      nm <- tolower(xml2::xml_name(ch))
      if (nm %in% c("sec", "subsec")) {
        t <- xml2::xml_find_first(ch, "./sectitle")
        if (!inherits(t, "xml_missing")) add(.blk_h(inl(t), depth + 1L))
        walk(ch, depth + 1L)
      } else if (nm == "sectitle") {
        next
      } else if (nm == "p") {
        txt <- inl(ch)
        if (nzchar(.blk_plain(txt))) add(.blk_p(txt))
      } else if (nm == "list") {
        items <- vapply(xml2::xml_find_all(ch, "./li"), inl, "")
        if (length(items)) add(.blk_list(as.list(items)))
      } else if (nm %in% c("tabwrap", "table")) {
        rows <- lapply(xml2::xml_find_all(ch, ".//tr"), \(r) vapply(xml2::xml_find_all(r, "./td | ./th"), inl, ""))
        cap <- xml2::xml_find_first(ch, ".//caption")
        add(.blk_table(rows, if (inherits(cap, "xml_missing")) "" else inl(cap),
                       length(xml2::xml_find_all(ch, ".//thead/tr"))))
      } else if (nm %in% c("figgrp", "fig")) {
        cap <- xml2::xml_find_first(ch, ".//caption")
        if (!inherits(cap, "xml_missing")) add(.blk_fig(inl(cap)))
      } else {
        walk(ch, depth)
      }
    }
  }
  body <- xml2::xml_find_first(root, ".//xmlbody")
  walk(if (inherits(body, "xml_missing")) root else body, 0L)
  refs <- vapply(xml2::xml_find_all(root, ".//refs/ref | .//ref[not(ancestor::refs)]"), \(r) {
    tmp <- .xml_copy(r)
    xml2::xml_remove(xml2::xml_find_all(tmp, "./label"))
    inl(tmp)
  }, "")
  lang <- xml2::xml_attr(root, "language") %NA% ""
  abs_nodes <- xml2::xml_find_all(root, ".//xmlabstr | .//abstract")
  abs_lang <- xml2::xml_attr(abs_nodes, "language")
  main <- abs_nodes[abs_lang %in% c(lang, NA)]
  if (!length(main)) main <- abs_nodes
  abstract <- if (length(main)) {
    ps <- xml2::xml_find_all(main[[1]], ".//p")
    if (length(ps)) vapply(ps, inl, "") else inl(main[[1]])
  } else character(0)
  kw_groups <- xml2::xml_find_all(root, ".//kwdgrp")
  kw_lang <- xml2::xml_attr(kw_groups, "language")
  kg <- kw_groups[kw_lang %in% c(lang, NA)]
  if (!length(kg)) kg <- kw_groups
  aff <- stats::setNames(vapply(xml2::xml_find_all(root, ".//normaff | .//aff"), \(a) {
    t <- m1(a, "./orgname")
    if (nzchar(t)) paste(c(t, m1(a, "./city"), m1(a, "./country")), collapse = ", ") |> sub(pattern = "(, )+$", replacement = "") else .tei_squish(xml2::xml_text(a))
  }, ""), xml2::xml_attr(xml2::xml_find_all(root, ".//normaff | .//aff"), "id"))
  authors <- lapply(xml2::xml_find_all(root, ".//authors/pauthor | .//author[fname]"), \(a) {
    rid <- strsplit(xml2::xml_attr(a, "rid") %NA% "", "\\s+")[[1]]
    list(given = m1(a, "./fname"), family = m1(a, "./surname"),
         orcid = xml2::xml_attr(a, "authorid") %NA% m1(a, "./authorid"),
         affs = unname(aff[rid[rid %in% names(aff)]]))
  })
  at <- function(a) xml2::xml_attr(root, a) %NA% ""
  date <- at("dateiso")
  if (grepl("^[0-9]{8}$", date)) date <- sub("^([0-9]{4})([0-9]{2})([0-9]{2})$", "\\1-\\2-\\3", date)
  date <- sub("-00(-00)?$", "", date)
  meta <- list(
    title_markup = { t <- xml2::xml_find_first(root, ".//doctitle"); if (inherits(t, "xml_missing")) NULL else inl(t) },
    authors = authors, abstract = abstract[nzchar(.blk_plain(abstract))],
    keywords = if (length(kg)) vapply(xml2::xml_find_all(kg[[1]], ".//kwd"), \(k) .tei_squish(xml2::xml_text(k)), "") else character(0),
    doi = m1(root, ".//doi"), journal = at("jtitle"), issn = if (nzchar(at("eissn"))) at("eissn") else at("issn"),
    publisher = at("pubname"), volume = at("volid"), issue = at("issueno"),
    fpage = at("fpage"), lpage = at("lpage"), date = date, language = lang,
    license_url = at("license"), refs = refs[nzchar(refs)],
    version = paste0("scielo-sps-", at("sps"))
  )
  .blocks_to_bibr12(xml_path, blocks, meta, "scielo")
}

# --- text with positions: pdf2xml and printer page descriptions -------------

#' Text pieces with their position joined into lines and paragraphs
#'
#' @param d a data frame of text pieces: `page`, `x`, `y` (from the top),
#'   `w` (width), `size` (font size) and `text`
#' @returns a list of blocks: paragraphs, and headings for short lines in a
#'   font larger than the text's
#' @noRd
.positions_to_blocks <- function(d) {
  d <- d[nzchar(trimws(d$text)) & !is.na(d$y), , drop = FALSE]
  if (!nrow(d)) return(list())
  # pieces on one line: the same page and nearly the same height
  d$line <- NA_integer_
  d <- d[order(d$page, d$y, d$x), ]
  tol <- pmax(1, stats::median(d$size, na.rm = TRUE) * 0.4)
  line <- 0L
  last_page <- -1
  last_y <- -Inf
  for (i in seq_len(nrow(d))) {
    if (d$page[i] != last_page || abs(d$y[i] - last_y) > tol) line <- line + 1L
    d$line[i] <- line
    last_page <- d$page[i]
    last_y <- d$y[i]
  }
  lines <- lapply(split(d, d$line), \(l) {
    l <- l[order(l$x), ]
    txt <- l$text[1]
    for (k in seq_len(nrow(l))[-1]) {
      gap <- l$x[k] - (l$x[k - 1] + l$w[k - 1])
      sep <- if (is.na(gap) || gap > 0.2 * l$size[k] || grepl("\\s$", txt) || grepl("^\\s", l$text[k])) " " else ""
      txt <- paste0(txt, sep, l$text[k])
    }
    data.frame(page = l$page[1], y = mean(l$y), size = max(l$size), text = .tei_squish(txt))
  })
  lines <- do.call(rbind, lines)
  lines <- lines[order(lines$page, lines$y), ]
  body_size <- as.numeric(names(which.max(table(round(lines$size)))))
  blocks <- list()
  para <- character(0)
  flush <- function() {
    if (length(para)) {
      txt <- para[1]
      for (p in para[-1]) {
        # a word broken at the end of a line
        txt <- if (grepl("[[:alpha:]]-$", txt) && grepl("^[[:lower:]]", p)) paste0(sub("-$", "", txt), p) else paste(txt, p)
      }
      blocks[[length(blocks) + 1L]] <<- .blk_p(.tei_esc(.tei_squish(txt)))
    }
    para <<- character(0)
  }
  prev <- NULL
  for (i in seq_len(nrow(lines))) {
    l <- lines[i, ]
    heading <- round(l$size) >= body_size + 2 && nchar(l$text) < 150
    gap <- if (is.null(prev) || prev$page != l$page) 0 else l$y - prev$y
    if (heading) {
      flush()
      blocks[[length(blocks) + 1L]] <- .blk_h(.tei_esc(l$text), 1L)
    } else {
      if (!is.null(prev) && (gap > 1.8 * l$size || abs(round(l$size) - round(prev$size)) >= 2)) flush()
      para <- c(para, l$text)
    }
    prev <- l
  }
  flush()
  blocks
}

#' Convert poppler's pdftohtml -xml output to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.pdf2xml_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  fonts <- xml2::xml_find_all(xml, "//fontspec")
  font_size <- stats::setNames(as.numeric(xml2::xml_attr(fonts, "size")), xml2::xml_attr(fonts, "id"))
  pages <- xml2::xml_find_all(xml, "//page")
  d <- do.call(rbind, lapply(seq_along(pages), \(k) {
    t <- xml2::xml_find_all(pages[[k]], ".//text")
    if (!length(t)) return(NULL)
    num <- function(a) suppressWarnings(as.numeric(xml2::xml_attr(t, a)))
    data.frame(page = k, x = num("left"), y = num("top"), w = num("width"),
               size = unname(font_size[xml2::xml_attr(t, "font")]) %||% num("height"),
               text = xml2::xml_text(t))
  }))
  if (is.null(d)) stop("the pdf2xml file holds no text", call. = FALSE)
  d$size[is.na(d$size)] <- stats::median(d$size, na.rm = TRUE)
  .blocks_to_bibr12(xml_path, .positions_to_blocks(d), list(version = "pdf2xml"), "pdf2xml")
}

#' Convert a printer's page description (<Aps>: glyph runs with positions)
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.aps_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  pages <- xml2::xml_find_all(xml, "//Page")
  d <- do.call(rbind, lapply(seq_along(pages), \(k) {
    g <- xml2::xml_find_all(pages[[k]], ".//Glyphs")
    if (!length(g)) return(NULL)
    tr <- strsplit(xml2::xml_attr(g, "RenderTransform") %||% rep("1 0 0 1 0 0", length(g)), "\\s+")
    num <- function(i) vapply(tr, \(v) suppressWarnings(as.numeric(v[i])), numeric(1))
    size <- suppressWarnings(as.numeric(xml2::xml_attr(xml2::xml_find_first(g, "./Font"), "SizePoints")))
    w <- suppressWarnings(as.numeric(xml2::xml_attr(xml2::xml_find_first(g, "./Size"), "Width")))
    data.frame(page = k, x = num(5), y = num(6), w = w, size = size,
               text = xml2::xml_text(xml2::xml_find_first(g, "./Text")))
  }))
  if (is.null(d)) stop("the page description holds no text", call. = FALSE)
  d$size[is.na(d$size)] <- stats::median(d$size, na.rm = TRUE)
  d$w[is.na(d$w)] <- 0
  .blocks_to_bibr12(xml_path, .positions_to_blocks(d), list(version = "aps"), "aps")
}

# --- OCR lines, word lists, spreadsheets ------------------------------------

#' Convert OCR output (<document><page><par><line>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.ocr_lines_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  pars <- xml2::xml_find_all(xml, "//par")
  blocks <- lapply(pars, \(p) {
    lines <- .tei_squish(xml2::xml_text(xml2::xml_find_all(p, ".//line | .//row")))
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return(NULL)
    txt <- lines[1]
    for (l in lines[-1]) {
      txt <- if (grepl("[[:alpha:]]-$", txt) && grepl("^[[:lower:]]", l)) paste0(sub("-$", "", txt), l) else paste(txt, l)
    }
    .blk_p(.tei_esc(txt))
  })
  blocks <- Filter(Negate(is.null), blocks)
  .blocks_to_bibr12(xml_path, blocks, list(version = "ocr"), "ocr")
}

#' Convert a list of words (<root><segment>) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.segments_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  words <- xml2::xml_text(xml2::xml_find_all(xml, "//segment"))
  txt <- .tei_squish(paste(words, collapse = " "))
  if (!nzchar(txt)) stop("the file holds no words", call. = FALSE)
  # without lines or paragraphs, the text is split at the part names a
  # paper prints ("Abstract", "References")
  parts <- strsplit(txt, "(?<=[.!?])\\s+(?=(Abstract|ABSTRACT|Introduction|INTRODUCTION|Methods?|METHODS?|Results|RESULTS|Discussion|DISCUSSION|Conclusions?|CONCLUSIONS?|References|REFERENCES)\\b)", perl = TRUE)[[1]]
  blocks <- lapply(parts, \(p) .blk_p(.tei_esc(p)))
  .blocks_to_bibr12(xml_path, blocks, list(version = "segments"), "segments")
}

#' Convert a spreadsheet a typesetting system wrote an article into
#' (<Workbook>, Excel's XML): each cell's text, split at its line breaks, is
#' paragraphs
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.spreadsheet_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  rules <- list(italic = "i", bold = "b", sup = "sup", sub = "sub", skip = character(0), space = character(0))
  blocks <- list()
  for (cell in xml2::xml_find_all(xml, "//Cell/Data")) {
    for (b in xml2::xml_find_all(cell, ".//br")) xml2::xml_replace(b, xml2::read_xml("<x>@@PARA@@</x>"))
    pieces <- strsplit(.blk_inline(cell, rules), "@@PARA@@", fixed = TRUE)[[1]]
    for (p in pieces) {
      p <- .tei_squish(p)
      if (nzchar(.blk_plain(p))) blocks[[length(blocks) + 1L]] <- .blk_p(p)
    }
  }
  title <- xml2::xml_text(xml2::xml_find_first(xml, "//DocumentProperties/Title"))
  if (is.na(title) || grepl("\\.(indd|docx?|pdf)\\s*$", title, ignore.case = TRUE)) title <- ""
  .blocks_to_bibr12(xml_path, blocks, list(title = title, version = "spreadsheet"), "spreadsheet")
}
