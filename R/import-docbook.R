# Publisher XML: DocBook ----
#
# DocBook (OASIS; versions 4.x with a DOCTYPE, 5.x in the namespace
# http://docbook.org/ns/docbook) is a format for technical documents; journals
# deliver it when their system exports a Word file as DocBook. Its sections
# (sect1-sect5, section), paragraphs, lists, tables, figures, footnotes and
# bibliography are read as blocks (see R/import-xml-blocks.R); its metadata
# from <info> or <articleinfo>.

# DocBook's inline elements, as JATS
.docbook_inline_rules <- list(
  italic = c("emphasis", "citetitle", "foreignphrase", "firstterm", "wordasword"),
  bold = c("strong"), sup = "superscript", sub = "subscript",
  link = c("ulink", "link"),
  href = function(n) {
    h <- xml2::xml_attr(n, "url")
    if (is.na(h)) h <- xml2::xml_attr(n, "xlink-href")
    h
  },
  skip = c("footnote", "inlinegraphic", "inlinemediaobject", "anchor", "indexterm", "remark"),
  space = c("sbr", "lineannotation")
)

#' DocBook's inline markup, with emphasis role="bold" as bold
#' @noRd
.docbook_inline <- function(node) {
  for (e in xml2::xml_find_all(node, ".//emphasis[@role='bold' or @role='strong']")) {
    xml2::xml_set_name(e, "strong")
  }
  .tei_squish(.blk_inline(node, .docbook_inline_rules))
}

#' The blocks of a DocBook element
#' @noRd
.docbook_blocks <- function(node, depth = 0L) {
  blocks <- list()
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  sections <- c("sect1", "sect2", "sect3", "sect4", "sect5", "section", "simplesect",
                "appendix", "chapter", "preface", "partintro", "refsect1", "refsect2")
  block_kids <- c("itemizedlist", "orderedlist", "variablelist", "table", "informaltable",
                  "figure", "informalfigure", "blockquote", "programlisting", "literallayout",
                  "screen", "para", "simpara", "formalpara", "note", "tip", "warning",
                  "important", "caution", "equation", "informalequation", "example")
  for (ch in xml2::xml_children(node)) {
    nm <- xml2::xml_name(ch)
    if (nm %in% c("title", "subtitle", "titleabbrev", "info", "articleinfo", "sectioninfo",
                  "sect1info", "sect2info", "indexterm", "anchor", "remark")) next
    if (nm %in% sections) {
      t <- xml2::xml_find_first(ch, "./title | ./info/title")
      if (!inherits(t, "xml_missing")) add(.blk_h(.docbook_inline(t), depth + 1L))
      for (b in .docbook_blocks(ch, depth + 1L)) add(b)
    } else if (nm %in% c("bibliography", "bibliodiv", "bibliolist")) {
      t <- xml2::xml_find_first(ch, "./title")
      add(.blk_h(if (inherits(t, "xml_missing")) "References" else .docbook_inline(t), depth + 1L))
      for (e in xml2::xml_find_all(ch, ".//bibliomixed | .//biblioentry | .//bibliomisc[not(ancestor::biblioentry) and not(ancestor::bibliomixed)]")) {
        add(.blk_ref(.docbook_inline(e)))
      }
    } else if (nm %in% c("para", "simpara")) {
      # footnotes are notes; lists or tables inside a paragraph are blocks
      for (fn in xml2::xml_find_all(ch, ".//footnote")) {
        add(.blk_note(paste(vapply(xml2::xml_find_all(fn, ".//para | .//simpara"), .docbook_inline, ""), collapse = " "),
                      xml2::xml_attr(fn, "label") %NA% ""))
      }
      inner <- xml2::xml_children(ch)
      if (any(xml2::xml_name(inner) %in% block_kids)) {
        tmp <- .xml_copy(ch)
        xml2::xml_remove(xml2::xml_find_all(tmp, paste0("./", block_kids, collapse = " | ")))
        txt <- .docbook_inline(tmp)
        if (nzchar(.blk_plain(txt))) add(.blk_p(txt))
        for (b in .docbook_blocks(ch, depth)) add(b)
      } else {
        txt <- .docbook_inline(ch)
        if (nzchar(.blk_plain(txt))) add(.blk_p(txt))
      }
    } else if (nm == "formalpara") {
      t <- xml2::xml_find_first(ch, "./title")
      p <- xml2::xml_find_first(ch, "./para")
      add(.blk_p(paste0(if (!inherits(t, "xml_missing")) paste0("<bold>", .docbook_inline(t), "</bold> ") else "",
                        if (!inherits(p, "xml_missing")) .docbook_inline(p) else "")))
    } else if (nm %in% c("itemizedlist", "orderedlist")) {
      items <- vapply(xml2::xml_find_all(ch, "./listitem"), \(li) {
        ps <- xml2::xml_find_all(li, ".//para | .//simpara")
        if (length(ps)) paste(vapply(ps, .docbook_inline, ""), collapse = " ") else .docbook_inline(li)
      }, "")
      if (length(items)) add(.blk_list(as.list(items), ordered = nm == "orderedlist"))
    } else if (nm == "variablelist") {
      items <- vapply(xml2::xml_find_all(ch, "./varlistentry"), \(v) {
        paste0(.docbook_inline(xml2::xml_find_first(v, "./term")), ": ",
               paste(vapply(xml2::xml_find_all(v, "./listitem//para"), .docbook_inline, ""), collapse = " "))
      }, "")
      if (length(items)) add(.blk_list(as.list(items)))
    } else if (nm %in% c("table", "informaltable")) {
      rows <- lapply(xml2::xml_find_all(ch, ".//row | .//tr"), \(r) {
        vapply(xml2::xml_find_all(r, "./entry | ./td | ./th"), .docbook_inline, "")
      })
      n_head <- length(xml2::xml_find_all(ch, ".//thead/row | .//thead/tr"))
      t <- xml2::xml_find_first(ch, "./title | ./caption")
      add(.blk_table(rows, if (inherits(t, "xml_missing")) "" else .docbook_inline(t), n_head))
    } else if (nm %in% c("figure", "informalfigure")) {
      t <- xml2::xml_find_first(ch, "./title | .//caption")
      if (!inherits(t, "xml_missing")) add(.blk_fig(.docbook_inline(t)))
    } else if (nm %in% c("programlisting", "literallayout", "screen")) {
      add(.blk_p(.tei_esc(.tei_squish(xml2::xml_text(ch)))))
    } else if (nm == "footnote") {
      add(.blk_note(.docbook_inline(ch)))
    } else {
      # other containers (blockquote, note, example, an unnamed wrapper)
      for (b in .docbook_blocks(ch, depth)) add(b)
    }
  }
  blocks
}

#' Convert a DocBook file to a paper
#'
#' @param xml_path path to the DocBook XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.docbook_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  root <- xml2::xml_root(xml)
  info <- xml2::xml_find_first(root, "./info | ./articleinfo | ./bookinfo")
  m1 <- function(node, xpath) {
    if (inherits(node, "xml_missing")) return("")
    n <- xml2::xml_find_first(node, xpath)
    if (inherits(n, "xml_missing")) "" else .tei_squish(xml2::xml_text(n))
  }
  title <- m1(root, "./title")
  if (!nzchar(title)) title <- m1(info, "./title")
  authors <- if (inherits(info, "xml_missing")) list() else lapply(xml2::xml_find_all(info, ".//author"), \(a) {
    list(given = m1(a, ".//firstname | .//givenname"), family = m1(a, ".//surname"),
         literal = if (!nzchar(m1(a, ".//surname"))) m1(a, "./personname | .") else "",
         email = m1(a, ".//email"),
         affs = vapply(xml2::xml_find_all(a, ".//affiliation"), \(f) .tei_squish(xml2::xml_text(f)), ""))
  })
  abstract <- if (inherits(info, "xml_missing")) character(0) else
    vapply(xml2::xml_find_all(info, "./abstract//para | ./abstract//simpara"), .docbook_inline, "")
  meta <- list(
    title = title, subtitle = m1(info, "./subtitle"), authors = authors,
    abstract = abstract[nzchar(abstract)],
    keywords = if (inherits(info, "xml_missing")) character(0) else
      vapply(xml2::xml_find_all(info, ".//keyword"), \(k) .tei_squish(xml2::xml_text(k)), ""),
    doi = m1(info, ".//biblioid[@class='doi'] | .//biblioid[contains(., '10.')]"),
    date = m1(info, "./pubdate | ./date"),
    language = xml2::xml_attr(root, "lang") %NA% (xml2::xml_attr(root, "xml-lang") %NA% ""),
    version = paste0("docbook-", xml2::xml_attr(root, "version") %NA% "4")
  )
  .blocks_to_bibr12(xml_path, .docbook_blocks(root), meta, "docbook")
}
