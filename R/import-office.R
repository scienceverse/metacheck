# Publisher XML: Word and OpenDocument files ----
#
# Journals deliver the word-processor file of an article as XML: Word's
# single-file XML (<pkg:package>, a .docx in one XML file; Word 2007 and
# later), Word 2003's XML (<w:wordDocument>), and OpenDocument's (<office:
# document>, a flat .fodt, or its content.xml). A paragraph's style tells a
# heading from text: Word's "Heading 1", "Titre 2", "Uberschrift 3", "Titulo
# 1", "Kop 2" (in any language, or by the outline level the style sets),
# OpenDocument's <text:h>. Lists, tables, links, footnotes, italics, bold,
# superscripts and subscripts are kept. Each is read as blocks (see
# R/import-xml-blocks.R).

# --- Word ---------------------------------------------------------------------

#' A Word paragraph's or run container's content as JATS inline markup
#'
#' @param node a w:p, w:hyperlink or other run container (prefixes removed)
#' @param links named vector: relationship id -> address
#' @param notes environment collecting footnote numbers
#' @noRd
.word_inline <- function(node, links = character(0)) {
  out <- character(0)
  on <- function(rpr, name) {
    n <- xml2::xml_find_first(rpr, paste0("./", name))
    !inherits(n, "xml_missing") && !(xml2::xml_attr(n, "w-val") %NA% "true") %in% c("0", "false", "none")
  }
  for (ch in xml2::xml_children(node)) {
    nm <- xml2::xml_name(ch)
    if (nm == "r") {
      txt <- paste(vapply(xml2::xml_children(ch), \(k) {
        switch(xml2::xml_name(k), t = .tei_esc(xml2::xml_text(k)), tab = " ", br = " ", cr = " ",
               noBreakHyphen = "-", sym = "", "")
      }, character(1)), collapse = "")
      if (!nzchar(txt)) next
      rpr <- xml2::xml_find_first(ch, "./rPr")
      if (!inherits(rpr, "xml_missing")) {
        va <- xml2::xml_attr(xml2::xml_find_first(rpr, "./vertAlign"), "w-val") %NA% ""
        if (va == "superscript") txt <- paste0("<sup>", txt, "</sup>")
        if (va == "subscript") txt <- paste0("<sub>", txt, "</sub>")
        if (on(rpr, "i")) txt <- paste0("<italic>", txt, "</italic>")
        if (on(rpr, "b")) txt <- paste0("<bold>", txt, "</bold>")
      }
      out <- c(out, txt)
    } else if (nm %in% c("hyperlink", "hlink")) {
      inner <- .word_inline(ch, links)
      href <- xml2::xml_attr(ch, "r-id")
      href <- if (!is.na(href)) unname(links[href]) else xml2::xml_attr(ch, "w-dest") %NA% (xml2::xml_attr(ch, "dest") %NA% NA_character_)
      out <- c(out, if (length(href) == 1 && !is.na(href) && grepl("^(https?|ftp)://|^www\\.", href)) {
        sprintf("<ext-link href=\"%s\">%s</ext-link>", .tei_attr(href), inner)
      } else inner)
    } else if (nm %in% c("ins", "smartTag", "fldSimple", "sdt", "sdtContent", "customXml", "moveTo")) {
      out <- c(out, .word_inline(ch, links))
    }
  }
  paste(out, collapse = "")
}

#' The blocks of a Word document body
#'
#' @param body the w:body element (prefixes removed)
#' @param styles named vector: style id -> style name (lower case)
#' @param outline named vector: style id -> outline level (0 = heading 1)
#' @param links named vector: relationship id -> address
#' @noRd
.word_blocks <- function(body, styles, outline, links) {
  blocks <- list()
  pending_list <- character(0)
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  end_list <- function() {
    if (length(pending_list)) add(.blk_list(as.list(pending_list)))
    pending_list <<- character(0)
  }
  heading_level <- function(p) {
    ppr <- xml2::xml_find_first(p, "./pPr")
    lvl <- suppressWarnings(as.integer(xml2::xml_attr(xml2::xml_find_first(ppr, "./outlineLvl"), "w-val")))
    if (!is.na(lvl) && lvl < 9) return(lvl + 1L)
    sid <- xml2::xml_attr(xml2::xml_find_first(ppr, "./pStyle"), "w-val") %NA% ""
    if (!nzchar(sid)) return(NA_integer_)
    if (!is.na(outline[sid])) return(as.integer(outline[sid]) + 1L)
    name <- styles[sid] %NA% tolower(sid)
    m <- regmatches(name, regexec("^(heading|titre|.berschrift|t.tulo|kop|titolo|nadpis|otsikko|rubrik)\\s*([1-9])$", name))[[1]]
    if (length(m)) return(as.integer(m[3]))
    # a journal template's title style ("MDPI_1.2 title", "Article Title")
    if (grepl("(^|[ _.])(article )?(title|titre|titel|titulo|titolo)$", iconv(name, "UTF-8", "ASCII//TRANSLIT", sub = ""))) {
      return(0L)
    }
    if (name %in% c("title", "titre", "titel", "titulo", "t\u00EDtulo", "titolo")) return(0L)
    NA_integer_
  }
  for (ch in xml2::xml_children(body)) {
    nm <- xml2::xml_name(ch)
    if (nm == "p") {
      txt <- .tei_squish(.word_inline(ch, links))
      if (!nzchar(.blk_plain(txt))) next
      lvl <- heading_level(ch)
      is_list <- !inherits(xml2::xml_find_first(ch, "./pPr/numPr | ./pPr/listPr"), "xml_missing")
      if (!is.na(lvl) && nchar(.blk_plain(txt)) < 300) {
        end_list()
        add(if (lvl == 0L) list(type = "title", text = txt) else .blk_h(txt, lvl))
      } else if (is_list) {
        pending_list <- c(pending_list, txt)
      } else {
        end_list()
        add(.blk_p(txt))
      }
    } else if (nm == "tbl") {
      end_list()
      rows <- lapply(xml2::xml_find_all(ch, "./tr"), \(r) {
        vapply(xml2::xml_find_all(r, "./tc"), \(c) {
          .tei_squish(paste(vapply(xml2::xml_find_all(c, ".//p"), \(p) .word_inline(p, links), ""), collapse = " "))
        }, "")
      })
      if (length(rows)) add(.blk_table(rows))
    } else if (nm %in% c("sdt", "sdtContent", "customXml", "sect", "subSection")) {
      end_list()
      for (b in .word_blocks(if (nm == "sdt") xml2::xml_find_first(ch, "./sdtContent") else ch, styles, outline, links)) add(b)
    }
  }
  end_list()
  blocks
}

#' Convert a Word XML file (single-file package, or Word 2003) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.word_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  part <- function(name) {
    p <- xml2::xml_find_first(xml, sprintf("//part[@pkg-name='%s' or @name='%s']/xmlData", name, name))
    if (inherits(p, "xml_missing")) NULL else p
  }
  doc <- part("/word/document.xml")
  styles_node <- part("/word/styles.xml")
  if (is.null(doc)) {
    # Word 2003: one document with its styles
    doc <- xml2::xml_root(xml)
    styles_node <- xml2::xml_find_first(doc, ".//styles")
  }
  body <- xml2::xml_find_first(doc, ".//body")
  if (inherits(body, "xml_missing")) stop("the Word file has no body", call. = FALSE)
  st <- if (is.null(styles_node) || inherits(styles_node, "xml_missing")) list() else xml2::xml_find_all(styles_node, ".//style")
  ids <- vapply(st, \(s) xml2::xml_attr(s, "w-styleId") %NA% "", "")
  styles <- stats::setNames(vapply(st, \(s) tolower(xml2::xml_attr(xml2::xml_find_first(s, "./name"), "w-val") %NA% ""), ""), ids)
  outline <- stats::setNames(vapply(st, \(s) {
    suppressWarnings(as.integer(xml2::xml_attr(xml2::xml_find_first(s, "./pPr/outlineLvl"), "w-val")))
  }, integer(1)), ids)
  outline <- outline[!is.na(outline) & outline < 9]
  rels <- xml2::xml_find_all(xml, "//part[contains(@pkg-name, 'document.xml.rels') or contains(@name, 'document.xml.rels')]//Relationship")
  links <- stats::setNames(xml2::xml_attr(rels, "Target"), xml2::xml_attr(rels, "Id"))

  blocks <- .word_blocks(body, styles, outline, links)
  # footnotes and endnotes
  fn_part <- part("/word/footnotes.xml")
  notes <- if (is.null(fn_part)) xml2::xml_find_all(body, ".//footnote") else
    xml2::xml_find_all(fn_part, ".//footnote[not(@w-type)]")
  for (n in notes) {
    txt <- .tei_squish(paste(vapply(xml2::xml_find_all(n, ".//p"), \(p) .word_inline(p, links), ""), collapse = " "))
    if (nzchar(.blk_plain(txt))) blocks[[length(blocks) + 1L]] <- .blk_note(txt, xml2::xml_attr(n, "w-id") %NA% "")
  }
  # the title: a paragraph in the "Title" style, else the document's title
  is_title <- vapply(blocks, \(b) identical(b$type, "title"), TRUE)
  title <- if (any(is_title)) blocks[[which(is_title)[1]]]$text else ""
  blocks <- blocks[!is_title]
  core <- part("/docProps/core.xml")
  if (!nzchar(title) && !is.null(core)) {
    t <- xml2::xml_text(xml2::xml_find_first(core, ".//title"))
    # a template's own title ("Type of the Paper (Article, Review...)") is none
    if (!is.na(t) && !grepl("\\.(docx?|indd)$", t) && !.blk_not_title(t)) title <- .tei_esc(.tei_squish(t))
  }
  meta <- list(title_markup = if (nzchar(title)) title else NULL, version = "word")
  .blocks_to_bibr12(xml_path, blocks, meta, "word")
}

# --- OpenDocument --------------------------------------------------------------

#' Convert an OpenDocument XML file (flat .fodt, or content.xml) to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.odf_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  rules <- list(link = "a", href = function(n) xml2::xml_attr(n, "xlink-href"),
                skip = c("note", "bookmark", "bookmark-start", "bookmark-end", "soft-page-break",
                         "sequence-decls", "tracked-changes"),
                space = c("s", "tab", "line-break"))
  inl <- function(n) .tei_squish(.blk_inline(n, rules))
  blocks <- list()
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  walk <- function(node) {
    for (ch in xml2::xml_children(node)) {
      nm <- xml2::xml_name(ch)
      if (nm == "h") {
        lvl <- suppressWarnings(as.integer(xml2::xml_attr(ch, "text-outline-level"))) %||% 1L
        if (is.na(lvl)) lvl <- 1L
        txt <- inl(ch)
        if (nzchar(.blk_plain(txt))) add(.blk_h(txt, lvl))
      } else if (nm == "p") {
        for (n in xml2::xml_find_all(ch, ".//note")) {
          add(.blk_note(inl(xml2::xml_find_first(n, "./note-body")), .tei_squish(xml2::xml_text(xml2::xml_find_first(n, "./note-citation")))))
        }
        txt <- inl(ch)
        if (nzchar(.blk_plain(txt))) add(.blk_p(txt))
      } else if (nm == "list") {
        items <- vapply(xml2::xml_find_all(ch, "./list-item"), \(li) {
          paste(vapply(xml2::xml_find_all(li, ".//p | .//h"), inl, ""), collapse = " ")
        }, "")
        items <- items[nzchar(items)]
        if (length(items)) add(.blk_list(as.list(items)))
      } else if (nm == "table") {
        rows <- lapply(xml2::xml_find_all(ch, ".//table-row"), \(r) {
          vapply(xml2::xml_find_all(r, "./table-cell"), \(c) paste(vapply(xml2::xml_find_all(c, ".//p"), inl, ""), collapse = " "), "")
        })
        if (length(rows)) add(.blk_table(rows, head_rows = length(xml2::xml_find_all(ch, ".//table-header-rows/table-row"))))
      } else if (!nm %in% c("meta", "styles", "automatic-styles", "master-styles", "font-face-decls",
                            "settings", "scripts", "sequence-decls", "tracked-changes", "forms")) {
        walk(ch)
      }
    }
  }
  body <- xml2::xml_find_first(xml, "//body")
  walk(if (inherits(body, "xml_missing")) xml2::xml_root(xml) else body)
  title <- xml2::xml_text(xml2::xml_find_first(xml, "//meta/title"))
  meta <- list(title = if (is.na(title) || grepl("\\.(docx?|odt)$", title)) "" else title,
               version = paste0("odf-", xml2::xml_attr(xml2::xml_root(xml), "office-version") %NA% "unknown"))
  .blocks_to_bibr12(xml_path, blocks, meta, "opendocument")
}
