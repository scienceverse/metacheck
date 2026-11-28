# Publisher XML: formats without JATS's structure ----
#
# Many XML formats hold an article without marking its parts the way JATS
# does: word processors (Word, OpenDocument), DocBook, Acrobat's "Save as
# XML", web pages, typesetting exports, the output of PDF and OCR tools, and
# journals' own formats. Each reader for such a format turns the file into a
# list of blocks (headings with their level, paragraphs, lists, tables,
# figures, notes, references) and the article's metadata; `.jats_from_blocks()`
# writes these as JATS, and the paper is read from that JATS as any JATS file
# is (see R/import-jats.R). Headings that name the abstract, the keywords or
# the references are recognised here, in every language the readers met
# (English, Spanish, Portuguese, French, German, Dutch, Italian).

#' An XML file as UTF-8 text, with namespace prefixes removed
#'
#' Reads a file in any encoding (UTF-8, UTF-16 with its byte order mark, or
#' the encoding the XML declaration names), removes namespace declarations and
#' the prefixes of element and attribute names (w:p -> p, text:h -> h,
#' xlink:href -> xlink-href), and parses it, recovering from the errors
#' hand-made formats contain.
#'
#' @param path path to the XML file
#' @param txt the file's text instead of the path
#' @returns an xml2 document
#' @noRd
.xml_read_plain <- function(path, txt = NULL) {
  if (is.null(txt)) txt <- .xml_text_utf8(path)
  # undeclared HTML entities (&nbsp;) as characters
  ents <- c(nbsp = 160, ndash = 8211, mdash = 8212, hellip = 8230, lsquo = 8216,
            rsquo = 8217, ldquo = 8220, rdquo = 8221, middot = 183, copy = 169,
            reg = 174, deg = 176, plusmn = 177, times = 215, laquo = 171, raquo = 187,
            shy = 173, eacute = 233, aacute = 225, iacute = 237, oacute = 243,
            uacute = 250, ntilde = 241, ccedil = 231, atilde = 227, otilde = 245,
            agrave = 224, egrave = 232, uuml = 252, ouml = 246, auml = 228)
  for (e in names(ents)) {
    txt <- gsub(paste0("&", e, ";"), paste0("&#", ents[[e]], ";"), txt, fixed = TRUE)
  }
  # control characters XML does not allow (typesetting exports keep them)
  txt <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", "", txt, perl = TRUE)
  txt <- gsub("&#(x0*[0-8bcef]|x0*1[0-9a-f]|0*[0-8]|0*1[1-24-9]|0*2[0-9]|0*3[01]);", "", txt,
              perl = TRUE, ignore.case = TRUE)
  txt <- sub("^\\s*<\\?xml[^>]*\\?>", "", txt, perl = TRUE)
  txt <- gsub("\\sxmlns(:[A-Za-z0-9_.-]+)?\\s*=\\s*(\"[^\"]*\"|'[^']*')", "", txt, perl = TRUE)
  txt <- gsub("<(/?)[A-Za-z0-9_.-]+:([A-Za-z0-9_.-]+)", "<\\1\\2", txt, perl = TRUE)
  # one prefixed attribute per tag at a time: repeat until none is left
  for (i in 1:100) {
    # (a tag, not a comment or declaration; a "?" in a link before the
    # attribute does not end the tag)
    new <- gsub("(<[A-Za-z][^<>]*?)\\s([A-Za-z0-9_.-]+):([A-Za-z0-9_.-]+)\\s*=", "\\1 \\2-\\3=",
                txt, perl = TRUE)
    if (identical(new, txt)) break
    txt <- new
  }
  xml <- tryCatch(xml2::read_xml(txt, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE", "NOBLANKS")),
                  error = function(e) NULL)
  if (!is.null(xml)) return(xml)
  .xml_parse_lenient(txt)
}

#' Parse broken XML with the HTML parser
#'
#' xml2 stops at the first error of a file the XML parser gives up on (an
#' end tag that does not match, an attribute without quotes); the HTML parser
#' reads on. It treats some names as a web page's (it moves `<title>` and
#' `<body>`, closes a `<p>` before a list): those are renamed before parsing
#' and back after. Element names come out in lower case.
#'
#' @param txt the document's text
#' @returns an xml2 document (its root is the file's first element; several
#'   top elements are put under `<document>`)
#' @noRd
.xml_parse_lenient <- function(txt) {
  special <- c("html", "head", "body", "title", "p", "meta", "link", "base", "style", "script",
               "caption", "label", "object", "source", "map", "form", "select", "option",
               "button", "input", "frame", "iframe", "noscript", "template", "col", "colgroup",
               "address", "list", "li", "dd", "dt", "a", "font", "center")
  pat <- paste0("<(/?)(", paste(special, collapse = "|"), ")(?=[\\s/>])")
  txt <- gsub(pat, "<\\1xprot-\\2", txt, perl = TRUE, ignore.case = TRUE)
  html <- xml2::read_html(txt, options = c("RECOVER", "NOERROR", "NOWARNING", "HUGE", "NOBLANKS"))
  for (n in xml2::xml_find_all(html, "//*[starts-with(local-name(), 'xprot-')]")) {
    xml2::xml_set_name(n, sub("^xprot-", "", xml2::xml_name(n)))
  }
  top <- xml2::xml_find_all(html, "/html/body/*")
  if (!length(top)) stop("the file could not be parsed as XML", call. = FALSE)
  if (length(top) == 1) return(xml2::xml_new_root(top[[1]]))
  doc <- xml2::xml_new_root("document")
  for (t in top) xml2::xml_add_child(doc, t)
  doc
}

#' A text file's content as UTF-8, whatever its encoding
#' @noRd
.xml_text_utf8 <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  if (length(raw) >= 2 && ((raw[1] == as.raw(0xff) && raw[2] == as.raw(0xfe)) ||
                           (raw[1] == as.raw(0xfe) && raw[2] == as.raw(0xff)))) {
    enc <- if (raw[1] == as.raw(0xff)) "UTF-16LE" else "UTF-16BE"
    txt <- iconv(list(raw[-(1:2)]), enc, "UTF-8")
  } else {
    if (length(raw) >= 3 && all(raw[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) raw <- raw[-(1:3)]
    raw <- raw[raw != as.raw(0)]
    txt <- rawToChar(raw)
    decl <- regmatches(txt, regexpr("^\\s*<\\?xml[^>]*encoding\\s*=\\s*[\"']([^\"']+)", txt, useBytes = TRUE))
    enc <- if (length(decl)) toupper(sub("^.*[\"']", "", decl)) else "UTF-8"
    if (enc %in% c("UTF-8", "UTF8", "US-ASCII", "ASCII")) {
      Encoding(txt) <- "UTF-8"
      # a file that says UTF-8 and is not: read it as Windows-1252
      if (!validUTF8(txt)) txt <- iconv(txt, "WINDOWS-1252", "UTF-8", sub = "?")
    } else {
      txt <- iconv(txt, enc, "UTF-8", sub = "?")
    }
  }
  if (is.na(txt)) stop("the file's text could not be read in any encoding", call. = FALSE)
  txt
}

#' An element's content as JATS inline markup
#'
#' Text is kept; elements named in `rules` become JATS's italic, bold,
#' superscript, subscript and links; elements in `rules$skip` are left out,
#' those in `rules$space` stand for a space (a line break); any other element
#' gives its own content.
#'
#' @param node an xml2 node
#' @param rules a list of lower-case local element names: `italic`, `bold`,
#'   `sup`, `sub`, `link` (with `href`, a function of the node giving the
#'   address), `skip`, `space`
#' @returns a character string of JATS inline markup
#' @noRd
.blk_inline <- function(node, rules) {
  out <- character(0)
  for (ch in xml2::xml_contents(node)) {
    type <- xml2::xml_type(ch)
    if (type %in% c("text", "cdata")) {
      out <- c(out, .tei_esc(xml2::xml_text(ch)))
      next
    }
    if (type != "element") next
    nm <- tolower(sub("^.*:", "", xml2::xml_name(ch)))
    if (nm %in% rules$skip) next
    if (nm %in% rules$space) {
      out <- c(out, " ", .blk_inline(ch, rules), " ")
      next
    }
    inner <- .blk_inline(ch, rules)
    piece <- if (nm %in% rules$italic) {
      paste0("<italic>", inner, "</italic>")
    } else if (nm %in% rules$bold) {
      paste0("<bold>", inner, "</bold>")
    } else if (nm %in% rules$sup) {
      paste0("<sup>", inner, "</sup>")
    } else if (nm %in% rules$sub) {
      paste0("<sub>", inner, "</sub>")
    } else if (nm %in% rules$link && !is.null(rules$href)) {
      href <- rules$href(ch)
      if (length(href) == 1 && !is.na(href) && grepl("^(https?|ftp)://|^www\\.|^10\\.[0-9]{4,}/", href)) {
        sprintf("<ext-link href=\"%s\">%s</ext-link>", .tei_attr(href), inner)
      } else inner
    } else inner
    out <- c(out, piece)
  }
  paste(out, collapse = "")
}

#' Plain text of a piece of JATS inline markup
#' @noRd
.blk_plain <- function(markup) {
  x <- gsub("<[^>]+>", "", markup)
  x <- gsub("&lt;", "<", gsub("&gt;", ">", x, fixed = TRUE), fixed = TRUE)
  .tei_squish(gsub("&amp;", "&", x, fixed = TRUE))
}

# Block constructors: a heading, a paragraph, a list, a table, a figure, a
# footnote, a reference. `text` is JATS inline markup.
.blk_h <- function(text, level = 1L) list(type = "h", text = text, level = as.integer(level))
.blk_p <- function(text) list(type = "p", text = text)
.blk_list <- function(items, ordered = FALSE) list(type = "list", items = items, ordered = ordered)
.blk_table <- function(rows, caption = "", head_rows = 0L) {
  list(type = "table", rows = rows, caption = caption, head_rows = as.integer(head_rows))
}
.blk_fig <- function(caption) list(type = "fig", caption = caption)
.blk_note <- function(text, label = "") list(type = "note", text = text, label = label)
.blk_ref <- function(text) list(type = "ref", text = text)

# Headings that name a part of an article, in the languages met
.blk_heading_words <- list(
  abstract = "^(abstract|summary|resumen|resumo|r[e\u00e9]sum[e\u00e9]|zusammenfassung|samenvatting|riassunto|sommario|sintesi)$",
  keywords = "^(key ?words|keywords|palabras clave|palavras[- ]chave|mots[- ]cl[e\u00e9]s|schl[u\u00fc]sselw[o\u00f6]rter|trefwoorden|parole chiave|descriptores|descritores)$",
  references = "^(references?|reference list|bibliography|literature cited|works cited|cited literature|literature|referencias( bibliogr[a\u00e1]ficas)?|refer[e\u00ea]ncias( bibliogr[a\u00e1]ficas)?|bibliograf[i\u00ed]a|bibliografia|r[e\u00e9]f[e\u00e9]rences( bibliographiques)?|literatur(verzeichnis)?|literatuur|riferimenti bibliografici)$"
)

#' What a heading names: "abstract", "keywords", "references", or NA
#' @noRd
.blk_heading_kind <- function(text) {
  t <- tolower(.blk_plain(text))
  t <- .tei_squish(gsub("^[0-9ivx.]+\\s+|[[:punct:]]+$|^[[:punct:]]+", "", t))
  for (k in names(.blk_heading_words)) if (grepl(.blk_heading_words[[k]], t)) return(k)
  NA_character_
}

#' Whether a line cannot be an article's title: a section's name, an article
#' type, a table or figure caption, or a line of the journal's details
#' @noRd
.blk_not_title <- function(text) {
  t <- tolower(.tei_squish(gsub("[[:punct:]]+$", "", text)))
  if (!is.na(.blk_heading_kind(t))) return(TRUE)
  sections <- paste0(
    "^([0-9ivx.]+\\s+)?(introduc(tion|ci\u00F3n|cion|\u00E3o|ao|tie)|einleitung|background|antecedentes|",
    "methods?|methodology|materials? and methods|metodolog\u00EDa|metodologia|m\u00E9todos?|metodos?|",
    "results?|resultados|discussion|discusi\u00F3n|discusion|discuss\u00E3o|conclusions?|conclusiones|",
    "conclus\u00F5es|conclusoes|acknowledge?ments?|agradecimientos|agradecimentos)$")
  types <- paste0(
    "^(original (research )?(article|paper)|research (article|paper)|review( article)?|",
    "art[i\u00ED]culo (original|de (investigaci[o\u00F3]n|revisi[o\u00F3]n))|artigo (original|de revis[a\u00E3]o)|",
    "editorial|short communication|case report|letter|commentary|ensayo|ensaio)$")
  caption <- "^(table|tabla|tabel|tabela|tableau|figure|figura|fig\\.?|gr[a\u00E1]fico|chart)\\s*[0-9]"
  journal <- "\\b(issn|e-issn|doi|vol\\.?|volume|n\u00BA|issue)\\b|^(https?://|www\\.)|^[0-9]"
  # a short line that starts with an article type ("Research Article Open
  # access", a template's "Type of the Paper (Article, Review...)")
  # (only labels no title starts with: not "review" or "letter")
  type_start <- nchar(t) < 60 && grepl(paste0(
    "^(type of (the )?paper|original (research )?(article|paper)|research (article|paper)|",
    "short communication|case report|art[i\u00ED]culo original|artigo original)\\b"), t)
  grepl(sections, t) || grepl(types, t) || type_start || grepl(caption, t) || grepl(journal, t)
}

# A paragraph that starts with a part's name: "Abstract: ...", "Keywords: ..."
.blk_lead <- "^\\s*(<[^>]+>)*\\s*(%s)\\s*(</[^>]+>)*\\s*[:.\u2014\u2013-]\\s*"

#' Write blocks and metadata as a JATS article
#'
#' @param blocks a list of blocks (see `.blk_h()` and the others)
#' @param meta a list: `title`, `subtitle`, `authors` (a list of lists with
#'   `given`, `family`, `literal`, `email`, `orcid`, `affs`), `abstract`
#'   (paragraphs as markup), `keywords`, `doi`, `journal`, `issn`,
#'   `publisher`, `volume`, `issue`, `fpage`, `lpage`, `date` (ISO 8601),
#'   `language`, `license_url`, `article_type`, `refs` (references as markup),
#'   `version` (the format, for the producer's version)
#' @returns the JATS article as a string
#' @noRd
.jats_from_blocks <- function(blocks, meta = list()) {
  esc <- .tei_esc
  get <- function(x) { v <- meta[[x]]; if (is.null(v) || !length(v) || is.na(v[1])) "" else as.character(v[1]) }
  wrap <- function(tag, x) if (nzchar(x)) sprintf("<%s>%s</%s>", tag, esc(x), tag) else ""
  abstract <- meta$abstract %||% character(0)
  keywords <- meta$keywords %||% character(0)
  refs <- meta$refs %||% character(0)
  notes <- character(0)
  title <- meta$title_markup %||% esc(get("title"))

  # a paragraph that is only a part's name is that part's heading (text from
  # PDF and OCR tools has no headings)
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    if (b$type == "p" && nchar(.blk_plain(b$text)) < 60 && !is.na(.blk_heading_kind(b$text))) {
      blocks[[i]] <- .blk_h(b$text, 1L)
    }
  }

  # the abstract, keywords and references, by their headings ----
  keep <- rep(TRUE, length(blocks))
  i <- 1L
  while (i <= length(blocks)) {
    b <- blocks[[i]]
    kind <- if (b$type == "h") .blk_heading_kind(b$text) else NA_character_
    if (!is.na(kind)) {
      j <- i + 1L
      got <- character(0)
      while (j <= length(blocks) && !(blocks[[j]]$type == "h" && blocks[[j]]$level <= b$level)) {
        bj <- blocks[[j]]
        if (bj$type %in% c("p", "ref")) got <- c(got, bj$text)
        if (bj$type == "list") got <- c(got, unlist(bj$items))
        j <- j + 1L
      }
      if (kind == "abstract" && !length(abstract) && length(got)) {
        abstract <- got
        keep[i:(j - 1L)] <- FALSE
      } else if (kind == "abstract" && length(abstract)) {
        # a second abstract (another language) stays in the text
      } else if (kind == "keywords" && length(got)) {
        if (!length(keywords)) keywords <- trimws(unlist(strsplit(.blk_plain(paste(got, collapse = "; ")), "[;,\u00b7]")))
        keep[i:(j - 1L)] <- FALSE
      } else if (kind == "references") {
        refs <- c(refs, got)
        keep[i:(j - 1L)] <- FALSE
      }
      i <- j
      next
    }
    if (b$type == "p") {
      plain <- b$text
      ab <- sprintf(.blk_lead, "abstract|summary|resumen|resumo|r\u00e9sum\u00e9|resume")
      kw <- sprintf(.blk_lead, "key ?words|keywords|palabras clave|palavras[- ]chave|mots[- ]cl\u00e9s|descriptores|descritores")
      if (!length(abstract) && grepl(ab, plain, ignore.case = TRUE, perl = TRUE)) {
        # as plain text: the part's name can open formatting that closes
        # after the text ("<bold>Abstract: We ...</bold>")
        abstract <- .tei_esc(.blk_plain(sub(ab, "", plain, ignore.case = TRUE, perl = TRUE)))
        keep[i] <- FALSE
      } else if (grepl(kw, plain, ignore.case = TRUE, perl = TRUE)) {
        if (!length(keywords)) {
          kws <- .blk_plain(sub(kw, "", plain, ignore.case = TRUE, perl = TRUE))
          keywords <- trimws(unlist(strsplit(kws, "[;,\u00b7]")))
        }
        keep[i] <- FALSE
      }
    }
    if (b$type == "ref") {
      refs <- c(refs, b$text)
      keep[i] <- FALSE
    }
    if (b$type == "note") {
      notes <- c(notes, sprintf("<fn>%s<p>%s</p></fn>", wrap("label", b$label %||% ""), b$text))
      keep[i] <- FALSE
    }
    i <- i + 1L
  }
  blocks <- blocks[keep]
  keywords <- keywords[nzchar(keywords)]
  # the title, when the format names none: the first heading or short
  # paragraph among the first blocks that is not a section's name, an article
  # type, a caption or a line of the journal's details
  if (sum(grepl("[[:alpha:]]", strsplit(.blk_plain(title), "")[[1]])) < 3) title <- ""
  if (!nzchar(title)) {
    for (k in seq_len(min(5L, length(blocks)))) {
      b <- blocks[[k]]
      if (!b$type %in% c("h", "p")) next
      t <- .blk_plain(b$text)
      if (nchar(t) >= 15 && nchar(t) <= 300 && !.blk_not_title(t)) {
        title <- b$text
        blocks <- blocks[-k]
        break
      }
    }
  }

  # the body: headings open sections, nested by level ----
  out <- character(0)
  open <- integer(0)
  close_to <- function(level) {
    while (length(open) && open[length(open)] >= level) {
      out <<- c(out, "</sec>")
      open <<- open[-length(open)]
    }
  }
  for (b in blocks) {
    piece <- switch(
      b$type,
      h = {
        close_to(b$level)
        open <- c(open, b$level)
        sprintf("<sec><title>%s</title>", b$text)
      },
      p = if (nzchar(.blk_plain(b$text))) sprintf("<p>%s</p>", b$text) else "",
      list = sprintf("<list list-type=\"%s\">%s</list>", if (isTRUE(b$ordered)) "order" else "bullet",
                     paste0("<list-item><p>", unlist(b$items), "</p></list-item>", collapse = "")),
      table = {
        rows <- vapply(seq_along(b$rows), \(k) {
          cell <- if (k <= b$head_rows) "th" else "td"
          paste0("<tr>", paste0("<", cell, ">", b$rows[[k]], "</", cell, ">", collapse = ""), "</tr>")
        }, character(1))
        lab <- regmatches(.blk_plain(b$caption), regexpr("^(table|tabla|tabela|tableau|tabelle|tabel)\\s*[0-9]+", .blk_plain(b$caption), ignore.case = TRUE))
        sprintf("<table-wrap>%s%s<table>%s</table></table-wrap>",
                if (length(lab)) wrap("label", lab) else "",
                if (nzchar(b$caption)) sprintf("<caption><p>%s</p></caption>", b$caption) else "",
                paste(rows, collapse = ""))
      },
      fig = {
        lab <- regmatches(.blk_plain(b$caption), regexpr("^(fig(ure)?|figura|abbildung|figuur)\\.?\\s*[0-9]+", .blk_plain(b$caption), ignore.case = TRUE))
        sprintf("<fig>%s<caption><p>%s</p></caption></fig>", if (length(lab)) wrap("label", lab) else "", b$caption)
      },
      ""
    )
    out <- c(out, piece)
  }
  close_to(-Inf)

  # references: a DOI or web address in a reference is its link ----
  ref_xml <- vapply(seq_along(refs), \(k) {
    r <- refs[[k]]
    if (!grepl("<ext-link", r, fixed = TRUE)) {
      r <- gsub("((https?://(dx\\.)?doi\\.org/)?\\b10\\.[0-9]{4,9}/[^[:space:]<>\"]+[^[:space:]<>\".,;)])",
                "<ext-link href=\"\\1\">\\1</ext-link>", r, perl = TRUE)
    }
    sprintf("<ref id=\"r%d\"><mixed-citation>%s</mixed-citation></ref>", k, r)
  }, character(1))

  # front matter ----
  authors <- meta$authors %||% list()
  aff_all <- unique(unlist(lapply(authors, `[[`, "affs")))
  aff_all <- aff_all[!is.na(aff_all) & nzchar(aff_all)]
  contribs <- vapply(authors, \(a) {
    g <- function(x) { v <- a[[x]]; if (is.null(v) || !length(v) || is.na(v[1])) "" else as.character(v[1]) }
    name <- if (nzchar(g("family"))) {
      paste0("<name>", wrap("surname", g("family")), wrap("given-names", g("given")), "</name>")
    } else if (nzchar(g("literal"))) {
      # a name written as one string: its last word is the family name
      parts <- strsplit(.tei_squish(g("literal")), " ")[[1]]
      paste0("<name>", wrap("surname", parts[length(parts)]),
             wrap("given-names", paste(parts[-length(parts)], collapse = " ")), "</name>")
    } else return("")
    rid <- match(a$affs %||% character(0), aff_all)
    paste0("<contrib contrib-type=\"author\">",
           if (nzchar(g("orcid"))) wrap("contrib-id", g("orcid")) |> sub(pattern = "<contrib-id>", replacement = "<contrib-id contrib-id-type=\"orcid\">", fixed = TRUE) else "",
           name, wrap("email", g("email")),
           paste0(sprintf("<xref ref-type=\"aff\" rid=\"aff%d\"/>", rid[!is.na(rid)]), collapse = ""),
           "</contrib>")
  }, character(1))
  affs <- if (length(aff_all)) paste0(sprintf("<aff id=\"aff%d\">", seq_along(aff_all)), esc(aff_all), "</aff>", collapse = "") else ""
  date <- get("date")
  pub_date <- if (grepl("^[0-9]{4}", date)) {
    p <- strsplit(substr(date, 1, 10), "-", fixed = TRUE)[[1]]
    sprintf("<pub-date pub-type=\"epub\">%s%s<year>%s</year></pub-date>",
            if (length(p) >= 3) wrap("day", p[3]) else "", if (length(p) >= 2) wrap("month", p[2]) else "", p[1])
  } else ""
  front <- paste0(
    "<front><journal-meta><journal-title-group>", wrap("journal-title", get("journal")),
    "</journal-title-group>",
    if (nzchar(get("issn"))) sprintf("<issn pub-type=\"epub\">%s</issn>", esc(get("issn"))) else "",
    "<publisher>", wrap("publisher-name", get("publisher")), "</publisher></journal-meta>",
    "<article-meta>",
    if (nzchar(get("doi"))) sprintf("<article-id pub-id-type=\"doi\">%s</article-id>", esc(sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", get("doi"), ignore.case = TRUE))) else "",
    "<title-group><article-title>", title, "</article-title>", wrap("subtitle", get("subtitle")), "</title-group>",
    "<contrib-group>", paste(contribs[nzchar(contribs)], collapse = ""), "</contrib-group>", affs,
    pub_date, wrap("volume", get("volume")), wrap("issue", get("issue")),
    wrap("fpage", get("fpage")), wrap("lpage", get("lpage")),
    if (nzchar(get("license_url"))) sprintf("<permissions><license href=\"%s\"/></permissions>", .tei_attr(get("license_url"))) else "",
    if (length(abstract)) paste0("<abstract>", paste0("<p>", abstract, "</p>", collapse = ""), "</abstract>") else "",
    if (length(keywords)) paste0("<kwd-group>", paste0("<kwd>", esc(keywords), "</kwd>", collapse = ""), "</kwd-group>") else "",
    meta$extra_meta %||% "",
    "</article-meta></front>"
  )
  paste0(
    sprintf("<article dtd-version=\"%s\"%s%s>", .tei_attr(get("version") %NA% "unknown"),
            if (nzchar(get("language"))) sprintf(" xml:lang=\"%s\"", .tei_attr(get("language"))) else "",
            if (nzchar(get("article_type"))) sprintf(" article-type=\"%s\"", .tei_attr(get("article_type"))) else ""),
    front, "<body>", paste(out, collapse = ""), "</body>",
    "<back>", if (length(notes)) paste0("<fn-group>", paste(notes, collapse = ""), "</fn-group>") else "",
    "<ref-list>", paste(ref_xml, collapse = ""), "</ref-list></back></article>"
  )
}

#' Read blocks and metadata as a paper
#'
#' @param xml_path the file the blocks were read from
#' @param blocks,meta see `.jats_from_blocks()`
#' @param producer the reader's name, for `extraction$producer`
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.blocks_to_bibr12 <- function(xml_path, blocks, meta, producer) {
  jats <- .jats_from_blocks(blocks, meta)
  xml <- xml2::read_xml(jats, options = "HUGE")
  .jats_to_bibr12(xml_path, xml = xml, input_format = "xml", producer = producer)
}
