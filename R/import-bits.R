# Publisher XML: BITS ----
#
# BITS (the Book Interchange Tag Suite, NISO JATS's book variant) holds a book
# (<book>) or one of its chapters (<book-part-wrapper>). Inside a chapter it
# uses JATS's own elements (sec, p, xref, ref-list), so the book is rearranged
# into one JATS article and read as JATS (see R/import-jats.R): the chapter's
# metadata (or the book's) is the front matter, each chapter of a whole book
# is a section with the chapter's title, and the chapters' reference lists,
# appendices and notes are the back matter. Many BITS files from publishers
# hold only the metadata of the book or chapter.

#' Translate a BITS book or chapter into JATS
#'
#' @param xml the BITS document (namespaces stripped)
#' @returns the JATS article as a string
#' @noRd
.bits_to_jats <- function(xml) {
  root <- xml2::xml_root(xml)
  book_meta <- xml2::xml_find_first(root, ".//book-meta")
  parts <- xml2::xml_find_all(root, ".//book-part[not(ancestor::book-part)]")
  # a chapter's own metadata, when the file is one chapter
  part_meta <- if (length(parts) == 1) xml2::xml_find_first(parts[[1]], "./book-part-meta") else
    xml2::xml_find_first(root, "./book-part-meta")
  meta <- if (!inherits(part_meta, "xml_missing")) part_meta else book_meta
  str <- function(nodes) paste(vapply(nodes, as.character, character(1)), collapse = "")
  first <- function(node, xpath) {
    if (inherits(node, "xml_missing")) return(node)
    xml2::xml_find_first(node, xpath)
  }
  inner <- function(node) {
    if (inherits(node, "xml_missing")) return("")
    paste(vapply(xml2::xml_contents(node), as.character, character(1)), collapse = "")
  }

  # the chapters' ids are kept apart: the same reference id in two chapters
  if (length(parts) > 1) {
    for (k in seq_along(parts)) {
      for (n in xml2::xml_find_all(parts[[k]], ".//*[@id]")) {
        xml2::xml_set_attr(n, "id", paste0("p", k, "-", xml2::xml_attr(n, "id")))
      }
      for (n in xml2::xml_find_all(parts[[k]], ".//*[@rid]")) {
        rid <- strsplit(xml2::xml_attr(n, "rid"), "\\s+")[[1]]
        xml2::xml_set_attr(n, "rid", paste(paste0("p", k, "-", rid), collapse = " "))
      }
    }
  }

  # body: one chapter's sections, or each chapter as a section ----
  part_body <- function(p) {
    b <- xml2::xml_find_first(p, "./body | ./named-book-part-body")
    sub_parts <- xml2::xml_find_all(p, "./body/book-part | ./book-part")
    xml2::xml_remove(xml2::xml_find_all(b, "./book-part"))
    paste0(if (inherits(b, "xml_missing")) "" else inner(b),
           paste(vapply(sub_parts, part_sec, character(1)), collapse = ""))
  }
  part_sec <- function(p) {
    t <- first(p, "./book-part-meta/title-group/title")
    lab <- first(p, "./book-part-meta/title-group/label")
    sprintf("<sec>%s<title>%s</title>%s</sec>",
            if (!inherits(lab, "xml_missing")) as.character(lab) else "",
            inner(t), part_body(p))
  }
  body <- if (length(parts) == 1) part_body(parts[[1]]) else
    paste(vapply(parts, part_sec, character(1)), collapse = "")
  book_body <- xml2::xml_find_first(root, "./book-body")
  if (!length(parts) && !inherits(book_body, "xml_missing")) body <- inner(book_body)

  # back: references, appendices, acknowledgements, notes ----
  backs <- xml2::xml_find_all(root, ".//back | .//book-back")
  refs <- xml2::xml_find_all(root, ".//ref-list//ref")
  others <- unlist(lapply(backs, \(b) {
    keep <- xml2::xml_find_all(b, "./*[not(self::ref-list)]")
    vapply(keep, as.character, character(1))
  }))

  # front: the chapter's (or the book's) metadata ----
  title_node <- first(meta, "./title-group/title")
  if (inherits(title_node, "xml_missing")) title_node <- first(book_meta, "./book-title-group/book-title")
  subtitle <- first(meta, "./title-group/subtitle")
  if (inherits(subtitle, "xml_missing") && identical(meta, book_meta)) subtitle <- first(book_meta, "./book-title-group/subtitle")
  doi <- xml2::xml_text(first(meta, "./book-part-id[@book-part-id-type='doi'] | ./book-id[@book-id-type='doi'] | ./book-id[@pub-id-type='doi']"))
  if (is.na(doi)) doi <- xml2::xml_text(first(book_meta, "./book-id[@book-id-type='doi'] | ./book-id[@pub-id-type='doi']"))
  contribs <- first(meta, "./contrib-group")
  if (inherits(contribs, "xml_missing")) contribs <- first(book_meta, "./contrib-group")
  pick <- function(xpath) {
    n <- first(meta, xpath)
    if (inherits(n, "xml_missing")) n <- first(book_meta, xpath)
    if (inherits(n, "xml_missing")) "" else as.character(n)
  }
  book_title <- xml2::xml_text(first(book_meta, "./book-title-group/book-title"))
  front <- paste0(
    "<front><journal-meta><journal-title-group>",
    # the book a chapter is in, in the place of the journal
    if (!identical(meta, book_meta) && !is.na(book_title)) sprintf("<journal-title>%s</journal-title>", .tei_esc(book_title)) else "",
    "</journal-title-group>", pick("./publisher"), "</journal-meta>",
    "<article-meta>",
    if (!is.na(doi)) sprintf("<article-id pub-id-type=\"doi\">%s</article-id>", .tei_esc(.tei_squish(doi))) else "",
    "<title-group><article-title>", inner(title_node), "</article-title>",
    if (!inherits(subtitle, "xml_missing")) paste0("<subtitle>", inner(subtitle), "</subtitle>") else "",
    "</title-group>",
    if (!inherits(contribs, "xml_missing")) as.character(contribs) else "",
    str(xml2::xml_find_all(meta, "./aff")),
    pick("./pub-date"), pick("./volume"), pick("./fpage"), pick("./lpage"),
    pick("./permissions"), str(xml2::xml_find_all(meta, "./abstract")),
    str(xml2::xml_find_all(meta, "./kwd-group")), pick("./funding-group"),
    "</article-meta></front>"
  )
  version <- xml2::xml_attr(root, "dtd-version") %NA% "unknown"
  paste0(
    sprintf("<article dtd-version=\"bits-%s\"%s>", .tei_attr(version),
            if (!is.na(xml2::xml_attr(root, "lang"))) sprintf(" xml:lang=\"%s\"", .tei_attr(xml2::xml_attr(root, "lang"))) else ""),
    front, "<body>", body, "</body>",
    "<back>", paste(others, collapse = ""), "<ref-list>", str(refs), "</ref-list></back></article>"
  )
}

#' Convert a BITS file to a paper
#'
#' @param xml_path path to the BITS XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.bits_to_bibr12 <- function(xml_path) {
  jats <- .bits_to_jats(.xml_read_publisher(xml_path))
  # the copied elements keep their xlink: and mml: prefixes
  jats <- sub("^<article ", paste0("<article xmlns:xlink=\"http://www.w3.org/1999/xlink\" ",
                                   "xmlns:mml=\"http://www.w3.org/1998/Math/MathML\" "), jats)
  xml <- xml2::read_xml(jats, options = "HUGE")
  xml2::xml_ns_strip(xml)
  .jats_to_bibr12(xml_path, xml = xml, input_format = "xml", producer = "bits")
}
