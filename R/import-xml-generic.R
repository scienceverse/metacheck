# Publisher XML: journals' own formats, PDFs, server notices ----
#
# What `.read_xml_fulltext()` does with files no standard reader takes: a PDF
# saved with an .xml name is read by Grobid; text a server printed before the
# XML declaration is left out; and any other XML is read as a journal's own
# format when it holds an article's text (`.generic_xml_to_bibr12()`).

#' Read a PDF (saved with an .xml name, or inside an OJS export) with Grobid
#'
#' @param pdf_path path to the PDF (any extension)
#' @param name the file name the paper is named after
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.pdf_to_bibr12 <- function(pdf_path, name = basename(pdf_path)) {
  dir <- tempfile("pdfxml")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  pdf <- file.path(dir, paste0(sub("\\.[^.]*$", "", name), ".pdf"))
  file.copy(pdf_path, pdf)
  out <- file.path(dir, "grobid")
  dir.create(out)
  tei <- convert_grobid(pdf, out)
  paper <- .grobid_to_bibr12(tei)
  paper$info$file_name <- name
  paper
}

#' Read a file without the text a server printed before its XML declaration
#'
#' @param fp path to the file
#' @param reader the function that reads the cleaned file
#' @returns what `reader` returns
#' @noRd
.xml_without_prefix <- function(fp, reader) {
  txt <- .xml_text_utf8(fp)
  at <- regexpr("<?xml", txt, fixed = TRUE)
  if (at < 1) stop("no XML in the file", call. = FALSE)
  dir <- tempfile("xmlclean")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  clean <- file.path(dir, basename(fp))
  writeLines(enc2utf8(substring(txt, at)), clean, useBytes = TRUE)
  paper <- reader(clean)
  paper$info$file_name <- basename(fp)
  paper
}

# Element names (lower case, without accents and other characters) that name
# a part of an article, in journals' own formats
.generic_names <- list(
  title = "^(title|titulo|tituno|titlee|titleen|articletitle|doctitle|titre|titel|titoloarticolo|titulodoartigo|titleart)$",
  other_title = "^(subtitle|subtitulo|titdos|tittres|titlef|titulof|titletranslated|transtitle|alttitle)$",
  author = "^(author|autor|authors|autores|authorname|nombre|nombreautor|creator|auteur|autore)$",
  affiliation = "^(affiliation|afiliacion|filiacion|univer|universidad|institution|institucion|aff|affil|instituicao)$",
  orcid = "^(orcid|orcidid)$", email = "^(email|correo|mail|correoelectronico)$",
  doi = "^(doi)$",
  abstract = "^(abstract|abstracts|resumen|resumo|summary|resume|abstracte|abstracten|zusammenfassung)$",
  keywords = "^(keywords|keyword|palabrasclave|palavraschave|kwd|kwdgroup|motscles|descriptores|keywordse)$",
  heading = "^(heading|head|h[1-6]|sectiontitle|sectitle|titulo[0-9]|tit[0-9]|apartado|seccion|epigrafe|subtit[0-9]?|intertitulo|titulosecao)$",
  references = "^(references|reference|referencias|referencia|bibliografia|bibliography|refs|ref|citation|citations|biblio|referencesitem|referenciasbibliograficas)$",
  journal = "^(journal|revista|journaltitle|nombrerevista)$",
  issn = "^(issn|eissn|pissn)$", year = "^(year|anio|ano|annee|jahr)$",
  volume = "^(vol|volume|volumen)$", issue = "^(issue|no|numero|number)$",
  skip = "^(script|style|head|meta|link|cabecera|header|footer|copyrightinfo|url|http|pais|organismo)$"
)

#' An element name as the patterns of `.generic_names` read it
#' @noRd
.generic_key <- function(name) {
  n <- tolower(iconv(sub("^.*:", "", name), "UTF-8", "ASCII//TRANSLIT", sub = ""))
  gsub("[^a-z0-9]", "", n)
}

#' What a journal's own element names: a key of `.generic_names`, or "text"
#' @noRd
.generic_kind <- function(name) {
  k <- .generic_key(name)
  for (n in names(.generic_names)) if (grepl(.generic_names[[n]], k)) return(n)
  "text"
}

# inline elements in journals' own formats, and in web pages
.generic_inline_rules <- list(
  italic = c("i", "em", "italic", "emphasis", "cursiva"),
  bold = c("b", "strong", "bold", "negrita"), sup = c("sup", "superscript"),
  sub = c("sub", "subscript"), link = c("a", "ext-link", "ulink", "link", "uri"),
  href = function(n) {
    h <- xml2::xml_attr(n, "href")
    if (is.na(h)) h <- xml2::xml_attr(n, "xlink-href")
    if (is.na(h)) h <- xml2::xml_attr(n, "url")
    if (is.na(h)) h <- .tei_squish(xml2::xml_text(n))
    h
  },
  skip = c("script", "style"), space = c("br", "line")
)

#' Read an XML file in a journal's own format
#'
#' An element with text of its own is one unit: a paragraph, or the title,
#' an author, an affiliation, the abstract, the keywords, a heading or a
#' reference when its name says so (in English, Spanish, Portuguese and
#' French). A file whose text is too short to be an article stops with its
#' first element.
#'
#' @param fp path to the XML file
#' @param fmt what `.xml_format()` found
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.generic_xml_to_bibr12 <- function(fp, fmt = .xml_format(fp)) {
  xml <- tryCatch(.xml_read_plain(fp), error = function(e) NULL)
  not_read <- function(why) {
    stop("not an XML format metacheck reads: its first element is <", fmt$root, ">",
         if (!is.na(fmt$doctype)) paste0(" and its DOCTYPE ", fmt$doctype), "; ", why,
         call. = FALSE)
  }
  if (is.null(xml)) not_read("it could not be parsed")
  meta <- list(title = "", authors = list(), abstract = character(0), keywords = character(0),
               refs = character(0), version = paste0("own-", fmt$root %NA% "unknown"))
  blocks <- list()
  add <- function(b) blocks[[length(blocks) + 1L]] <<- b
  # the units: elements with text of their own, outside another unit
  units <- xml2::xml_find_all(xml, "//*[text()[normalize-space()]][not(ancestor::*[text()[normalize-space()]])]")
  for (u in units) {
    kind <- .generic_kind(xml2::xml_name(u))
    # the unit's own name, else its parent's ("<abstract><content>")
    if (kind == "text") {
      for (a in rev(xml2::xml_name(xml2::xml_parents(u)))) {
        k <- .generic_kind(a)
        if (k %in% c("abstract", "references", "keywords", "author", "affiliation", "skip")) kind <- k
      }
    }
    txt <- .tei_squish(.blk_inline(u, .generic_inline_rules))
    plain <- .blk_plain(txt)
    if (!nzchar(plain) || kind == "skip") next
    switch(
      kind,
      title = if (!nzchar(meta$title)) meta$title <- plain else add(.blk_h(txt, 1L)),
      other_title = NULL,
      author = {
        # several names in one element, separated by commas or semicolons
        nms <- trimws(unlist(strsplit(plain, "\\s*[;,]\\s*|\\s+(and|y|e|et)\\s+")))
        nms <- nms[grepl("\\s", nms) & nchar(nms) < 80]
        for (n in nms) meta$authors[[length(meta$authors) + 1L]] <- list(literal = n, affs = character(0))
      },
      affiliation = {
        k <- length(meta$authors)
        if (k) meta$authors[[k]]$affs <- c(meta$authors[[k]]$affs, plain)
      },
      orcid = {
        k <- length(meta$authors)
        o <- regmatches(plain, regexpr("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9Xx]", plain))
        if (k && length(o)) meta$authors[[k]]$orcid <- paste0("https://orcid.org/", toupper(o))
      },
      email = {
        k <- length(meta$authors)
        if (k) meta$authors[[k]]$email <- plain
      },
      doi = {
        d <- regmatches(plain, regexpr("10\\.[0-9]{4,9}/[^[:space:]]+", plain))
        if (length(d) && is.null(meta$doi)) meta$doi <- sub("[.,;]+$", "", d)
      },
      abstract = meta$abstract <- c(meta$abstract, sub("^(abstract|resumen|resumo|summary)\\s*[:.]\\s*", "", txt, ignore.case = TRUE)),
      keywords = meta$keywords <- c(meta$keywords, trimws(unlist(strsplit(
        sub("^[^:]{0,30}:\\s*", "", plain), "\\s*[;,]\\s*")))),
      heading = add(.blk_h(txt, 1L)),
      references = add(.blk_ref(txt)),
      journal = if (is.null(meta$journal) && nchar(plain) < 200) meta$journal <- plain else add(.blk_p(txt)),
      issn = if (is.null(meta$issn)) meta$issn <- regmatches(plain, regexpr("[0-9]{4}-[0-9]{3}[0-9Xx]", plain))[1],
      year = if (is.null(meta$date) && grepl("^(1[89]|20)[0-9]{2}$", plain)) meta$date <- plain,
      volume = if (is.null(meta$volume) && nchar(plain) < 20) meta$volume <- plain,
      issue = if (is.null(meta$issue) && nchar(plain) < 20) meta$issue <- plain,
      add(.blk_p(txt))
    )
  }
  # a second language's abstract after the first stays one abstract each
  words <- sum(lengths(strsplit(c(vapply(blocks, \(b) .blk_plain(b$text %||% ""), ""),
                                  .blk_plain(paste(meta$abstract, collapse = " "))), "\\s+")))
  if (words < 100) not_read("it holds no article text")
  .blocks_to_bibr12(fp, blocks, meta, "xml")
}
