# Publisher XML: OJS exports ----
#
# Open Journal Systems (PKP), the software many small journals run, exports
# an article in its own XML: OJS 2.x as <article> with the DOCTYPE "-//PKP//
# OJS Articles and Issues XML//EN", OJS 3.x in the namespace
# http://pkp.sfu.ca. The export holds the article's metadata (title,
# abstract, authors, keywords, DOI, pages, issue) and the article's files,
# written into the XML in base64 (<embed>). The PDF among them is read by
# Grobid (as a PDF is), and the paper gets the export's metadata: its title,
# DOI, keywords, journal details and, when Grobid found none, its abstract.
# An HTML file is read as a web page (see R/import-html.R). An export
# without a file gives a paper of the metadata, the abstract its only text.

#' The metadata of an OJS export
#' @param xml the export (prefixes removed)
#' @returns a list for `.jats_from_blocks()`
#' @noRd
.ojs_meta <- function(xml) {
  root <- xml2::xml_root(xml)
  pub <- xml2::xml_find_first(root, "./publication[last()]")
  src <- if (inherits(pub, "xml_missing")) root else pub
  lang <- xml2::xml_attr(root, "locale") %NA% (xml2::xml_attr(src, "locale") %NA% "")
  # the text in the article's own language when the export has several
  local1 <- function(xpath) {
    nodes <- xml2::xml_find_all(src, xpath)
    if (!length(nodes)) return(NULL)
    loc <- xml2::xml_attr(nodes, "locale")
    nodes[[if (any(loc %in% lang)) which(loc %in% lang)[1] else 1]]
  }
  txt <- function(xpath) { n <- local1(xpath); if (is.null(n)) "" else .tei_squish(xml2::xml_text(n)) }
  # abstracts are HTML written as text ("&lt;p&gt;")
  html_paras <- function(x) {
    if (!nzchar(x)) return(character(0))
    h <- xml2::read_html(paste0("<div>", x, "</div>"))
    ps <- xml2::xml_find_all(h, "//p")
    v <- if (length(ps)) .tei_squish(xml2::xml_text(ps)) else .tei_squish(xml2::xml_text(h))
    .tei_esc(v[nzchar(v)])
  }
  abs_node <- local1("./abstract")
  authors <- lapply(xml2::xml_find_all(src, "./author | ./authors/author"), \(a) {
    m <- function(xp) .rec1(a, xp)
    list(given = trimws(paste(m("./givenname | ./firstname"), m("./middlename"))),
         family = m("./familyname | ./lastname"), email = m("./email"), orcid = m("./orcid"),
         affs = vapply(xml2::xml_find_all(a, "./affiliation"), \(f) .tei_squish(xml2::xml_text(f)), "")[1])
  })
  kw <- c(vapply(xml2::xml_find_all(src, "./keywords/keyword | ./subjects/subject"), \(k) .tei_squish(xml2::xml_text(k)), ""),
          trimws(unlist(strsplit(txt("./indexing/subject"), "[;,]"))))
  doi <- .rec1(src, "./id[@type='doi'] | ./id[@advice='update'][contains(., '10.')]")
  if (!nzchar(doi)) doi <- .rec1(root, "./id[@type='doi']")
  pages <- strsplit(.rec1(src, "./pages"), "\\s*[-\u2013]\\s*")[[1]]
  issue <- xml2::xml_find_first(src, "./issue_identification")
  if (inherits(issue, "xml_missing")) issue <- xml2::xml_find_first(root, "./issue_identification")
  list(
    title = txt("./title"), subtitle = txt("./subtitle"), authors = authors,
    abstract = if (is.null(abs_node)) character(0) else html_paras(xml2::xml_text(abs_node)),
    keywords = unique(kw[nzchar(kw)]), doi = doi,
    volume = .rec1(issue, "./volume"), issue = .rec1(issue, "./number"),
    date = if (nzchar(.rec1(src, "./date_published"))) .rec1(src, "./date_published") else xml2::xml_attr(src, "date_published") %NA% .rec1(issue, "./year"),
    fpage = if (length(pages)) pages[1] else "", lpage = if (length(pages) > 1) pages[2] else "",
    language = sub("_.*$", "", lang),
    refs = vapply(xml2::xml_find_all(src, "./citations/citation"), \(c) .tei_esc(.tei_squish(xml2::xml_text(c))), ""),
    version = if (grepl("pkp.sfu.ca", xml2::xml_attr(root, "schemaLocation") %NA% "") || !inherits(pub, "xml_missing")) "ojs-3" else "ojs-2"
  )
}

#' Convert an OJS export to a paper
#' @param xml_path path to the XML file
#' @returns a paper object in bibr export schema 12.x form
#' @noRd
.ojs_to_bibr12 <- function(xml_path) {
  xml <- .xml_read_plain(xml_path)
  meta <- .ojs_meta(xml)
  embeds <- xml2::xml_find_all(xml, "//embed")
  type_of <- function(e) {
    p <- xml2::xml_parent(e)
    tolower(paste(xml2::xml_attr(e, "mime_type"), xml2::xml_attr(e, "filename"),
                  xml2::xml_attr(p, "filetype"), xml2::xml_attr(p, "filename"),
                  xml2::xml_attr(xml2::xml_parent(p), "filetype")))
  }
  types <- vapply(embeds, type_of, "")
  pdf <- which(grepl("pdf", types))
  html <- which(grepl("html", types))
  pick <- if (length(pdf)) pdf[1] else if (length(html)) html[1] else NA_integer_
  if (is.na(pick)) return(.blocks_to_bibr12(xml_path, list(), meta, "ojs"))

  dir <- tempfile("ojs")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ext <- if (length(pdf)) ".pdf" else ".html"
  file <- file.path(dir, paste0(sub("\\.[^.]*$", "", basename(xml_path)), ext))
  writeBin(jsonlite::base64_dec(gsub("\\s", "", xml2::xml_text(embeds[[pick]]))), file)
  paper <- if (ext == ".pdf") .pdf_to_bibr12(file, basename(xml_path)) else .html_to_bibr12(file)
  # the export's metadata, which is the journal's own
  info <- paper$info
  set <- function(col, value) if (col %in% names(info) && length(value) && nzchar(value[1] %NA% "")) info[[col]] <<- value[1]
  set("title", meta$title)
  set("doi", sub("^(https?://(dx\\.)?doi\\.org/)", "", meta$doi))
  set("volume", meta$volume)
  set("issue", meta$issue)
  set("first_page", meta$fpage)
  set("last_page", meta$lpage)
  set("language", meta$language)
  if (length(meta$keywords) && "keywords" %in% names(info)) info$keywords <- list(meta$keywords)
  if ((is.na(info$abstract %||% NA) || !nzchar(info$abstract %||% "")) && length(meta$abstract) && "abstract" %in% names(info)) {
    info$abstract <- .blk_plain(paste(meta$abstract, collapse = " "))
  }
  info$file_name <- basename(xml_path)
  info$input_format <- "xml"
  paper$info <- info
  paper$extraction$producer <- list(name = paste0("ojs+", if (ext == ".pdf") "grobid" else "html"),
                                    version = meta$version, build_sha = NULL)
  paper
}
