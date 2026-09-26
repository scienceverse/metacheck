#' Convert Grobid TEI to a paper in bibr export schema 12.x form
#'
#' Called by `.grobid_to_bibr(schema_version = "12.0")`. The body text is
#' split into sentences as `.grobid_to_bibr()` does; captions and footnotes
#' are one text row each, with no section, after the body and the reference
#' list. Ids are 1-based positions in document order, and cross-reference
#' targets are looked up from the xml:id each `<ref>` points at.
#'
#' The source is the PDF Grobid read when it sits next to the TEI (published.pdf
#' for published.pdf.tei.xml or published.xml), else the TEI file. The
#' producer is Grobid (the version in the TEI header), which also says when it
#' extracted the content, and the converter metacheck. Metacheck's own warning
#' codes start with METACHECK_.
#'
#' @param xml_path path to a single XML file
#' @param schema_version the export schema version ("12.0")
#' @returns a paper object
#' @noRd
.grobid_to_bibr12 <- function(xml_path, schema_version = "12.0") {
  if (!identical(schema_version, "12.0")) {
    stop("schema_version must be NULL or \"12.0\"", call. = FALSE)
  }
  section_id <- header <- section_type <- NULL # cmdcheck

  xml <- .xml_read_grobid(xml_path)

  warnings <- list()
  warn <- function(code, message) {
    warnings[[length(warnings) + 1L]] <<- list(code = code, message = message)
  }
  squish <- function(x) trimws(gsub("\\s+", " ", x))
  na_if_empty <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
    x
  }

  # source: the PDF Grobid read, when it is next to the TEI ----
  stem <- sub("\\.tei\\.xml$", "", xml_path, ignore.case = TRUE)
  pdfs <- c(stem, sub("\\.xml$", ".pdf", xml_path, ignore.case = TRUE))
  pdfs <- pdfs[grepl("\\.pdf$", pdfs, ignore.case = TRUE) & file.exists(pdfs)]
  src_path <- if (length(pdfs)) pdfs[[1]] else xml_path
  if (!length(pdfs)) {
    warn("METACHECK_SOURCE_IS_TEI", paste(
      "The PDF Grobid read was not found next to the TEI file,",
      "so source is the TEI file."))
  }
  source <- list(
    file_name = basename(src_path),
    sha256 = .bibr12_sha256(src_path),
    input_format = if (length(pdfs)) "pdf" else "tei"
  )
  if (is.na(source$sha256)) {
    warn("METACHECK_SOURCE_SHA256_UNAVAILABLE",
         "SHA-256 needs R >= 4.5 or the digest package.")
  }

  # floats, notes and references, in document (TEI) order ----
  floats <- xml2::xml_find_all(xml, "//figure")
  is_tab <- xml2::xml_attr(floats, "type") %in% "table"
  notes <- xml2::xml_find_all(xml, "//note[@place='foot']")
  bibs <- xml2::xml_find_all(xml, "//listBibl //biblStruct")

  # body: sentences as .grobid_to_bibr() makes them, without floats and notes
  body_xml <- xml2::read_xml(as.character(xml))
  xml2::xml_find_all(body_xml, "//figure | //note[@place='foot']") |>
    xml2::xml_remove()
  body <- .tei_text(body_xml)
  sec <- dplyr::count(body, section_id, header, section_type)
  empty_headers <- (body$formatted == body$header) |> sapply(isTRUE)
  body <- body[!empty_headers, ]

  # the same header text .grobid_to_bibr() makes
  sec_header <- sapply(sec$header, \(h) {
    if (is.na(h)) return(NA_character_)
    x <- paste0("<p>", h, "</p>") |> xml2::read_xml()
    head <- xml2::xml_find_first(x, "//head")
    if (length(head) == 0) return(xml2::xml_text(x))
    header <- xml2::xml_text(head)
    n <- xml2::xml_attr(head, "n")
    if (!is.na(n)) header <- paste(n, header)
    return(header)
  }, USE.NAMES = FALSE)
  sec_header[grepl("^\\[div-\\d+\\]$", sec_header)] <- NA_character_

  section_types <- c(
    acknowledgement = "acknowledgment", annex = "appendix",
    availability = "data_availability", conflict = "coi",
    contribution = "author_contributions", foot = "footnote"
  )
  sec_type <- sec$section_type
  mapped <- !is.na(sec_type) & sec_type %in% names(section_types)
  sec_type[mapped] <- section_types[sec_type[mapped]]
  sec_type[!is.na(sec_type) & !sec_type %in% c(
    "title", "abstract", "intro", "method", "results", "discussion",
    "references", "acknowledgment", "funding", "keywords", "endnote",
    "appendix", "data_availability", "author_contributions", "coi",
    "ethics", "footnote", "table", "figure", "unknown"
  )] <- "unknown"

  sections <- list(
    section_id = seq_along(sec$section_id),
    header = na_if_empty(sec_header),
    level = rep(1L, nrow(sec)),
    section_type = sec_type
  )
  body_section <- match(body$section_id, sec$section_id)

  # references: one text row each, in a References section ----
  bib <- .tei_bib(xml)
  n_bib <- nrow(bib)
  ref_section <- length(sections$section_id) + 1L
  if (n_bib > 0) {
    sections$section_id <- c(sections$section_id, ref_section)
    sections$header <- c(sections$header, "References")
    sections$level <- c(sections$level, 1L)
    sections$section_type <- c(sections$section_type, "references")
  }

  # captions and footnotes: one text row each, with no section ----
  desc <- lapply(floats, xml2::xml_find_first, ".//figDesc")
  caption <- vapply(desc, \(d) {
    if (inherits(d, "xml_missing")) NA_character_ else squish(xml2::xml_text(d))
  }, character(1)) |> na_if_empty()
  # a caption starts with its printed label, which Grobid can leave out of
  # figDesc but keeps in <head> ("Figure 1 :")
  float_head <- vapply(floats, \(f) {
    squish(.xml_find1_text(f, "./head"))
  }, character(1)) |> sub("\\s+([:.])$", "\\1", x = _)
  labelled <- grepl("^(fig(ure)?|tab(le)?)\\.?\\s*[A-Z]{0,2}[0-9]", caption,
                    ignore.case = TRUE) |
    startsWith(tolower(caption), tolower(float_head))
  add_head <- !is.na(caption) & nzchar(float_head) & !labelled
  caption[add_head] <- paste(float_head[add_head], caption[add_head])
  has_caption <- !is.na(caption)
  note_text <- squish(xml2::xml_text(notes))

  # the text table, and the TEI markup of each row to find <ref>s in
  n_body <- nrow(body)
  n_cap <- sum(has_caption)
  text <- c(body$text, bib$bib_text %||% character(0), caption[has_caption],
            note_text)
  markup <- c(body$formatted, rep(NA_character_, n_bib),
              vapply(desc[has_caption], as.character, character(1)),
              vapply(notes, as.character, character(1)))
  text_id <- seq_along(text)
  para <- max(c(0L, body$paragraph_id))
  paragraph_id <- c(body$paragraph_id, para + seq_len(n_bib + n_cap + length(notes)))
  text_section <- c(body_section, rep(ref_section, n_bib),
                    rep(NA_integer_, n_cap + length(notes)))
  ref_text_id <- n_body + seq_len(n_bib)
  cap_text_id <- rep(NA_integer_, length(floats))
  cap_text_id[has_caption] <- n_body + n_bib + seq_len(n_cap)
  note_text_id <- n_body + n_bib + n_cap + seq_along(notes)

  # figures, tables and footnotes: 1-based positions in document order ----
  float_label <- vapply(floats, \(f) {
    gsub("\\s", "", .xml_find1_text(f, "./label"))
  }, character(1)) |> na_if_empty()
  float_page <- vapply(floats, \(f) {
    coords <- xml2::xml_attr(f, "coords")
    if (is.na(coords)) {
      coords <- xml2::xml_attr(xml2::xml_find_first(f, ".//graphic"), "coords")
    }
    page <- suppressWarnings(as.integer(sub(",.*", "", coords)))
    if (!is.na(page) && page >= 1) page else NA_integer_
  }, integer(1))
  if (length(floats)) {
    warn("METACHECK_FLOAT_SECTION_UNKNOWN", paste(
      "Grobid lists figures and tables after the body, so the section each is",
      "printed in is unknown: their section_id is null."))
  }

  figure <- list(
    figure_id = seq_len(sum(!is_tab)),
    label = float_label[!is_tab],
    text_id = cap_text_id[!is_tab],
    caption = caption[!is_tab],
    page_number = float_page[!is_tab]
  )
  table_nodes <- lapply(floats[is_tab], xml2::xml_find_first, ".//table")
  table <- list(
    table_id = seq_len(sum(is_tab)),
    label = float_label[is_tab],
    text_id = cap_text_id[is_tab],
    contents = lapply(table_nodes, \(t) .tei_table_contents(t) %||% list()),
    caption = caption[is_tab],
    page_number = float_page[is_tab]
  )
  footnote <- list(
    footnote_id = seq_along(notes),
    label = na_if_empty(xml2::xml_attr(notes, "n")),
    text_id = note_text_id
  )

  # references ----
  bib_col <- function(col) bib[[col]] %||% rep(NA, n_bib)
  bib_doi <- na_if_empty(bib_col("doi"))
  year <- as.integer(bib_col("year"))
  bib_tbl <- list(
    bib_id = seq_len(n_bib),
    text_id = ref_text_id,
    bib_type = .bibr12_bib_type(bib_col("bib_type")),
    doi = .bibr12_doi(bib_doi),
    title = na_if_empty(bib_col("title")),
    authors = na_if_empty(bib_col("authors")),
    editors = na_if_empty(bib_col("editors")),
    publisher = na_if_empty(bib_col("publisher")),
    year = year,
    year_suffix = na_if_empty(bib_col("year_suffix")),
    published_date = ifelse(!is.na(year) & year >= 1000 & year <= 2999,
                            sprintf("%04d", year), NA_character_),
    container = na_if_empty(bib_col("container")),
    volume = na_if_empty(bib_col("volume")),
    issue = na_if_empty(bib_col("issue")),
    first_page = na_if_empty(bib_col("first_page")),
    last_page = na_if_empty(bib_col("last_page")),
    is_in_press = rep(FALSE, n_bib)
  )

  # cross-references: the xml:id each <ref> targets, by type ----
  ids <- function(nodes, n = seq_along(nodes)) {
    stats::setNames(n, xml2::xml_attr(nodes, "id"))
  }
  targets <- list(
    bib = ids(bibs),
    figure = ids(floats[!is_tab]),
    table = ids(floats[is_tab]),
    foot = ids(notes)
  )
  xref_types <- c(bibr = "bib", figure = "figure", table = "table",
                  foot = "foot", formula = "equation")

  refs <- mapply(\(f, tid) {
    if (is.na(f)) return(NULL)
    r <- paste0("<p>", f, "</p>") |>
      xml2::read_html() |>
      xml2::xml_find_all("//ref")
    if (length(r) == 0) return(NULL)
    dplyr::tibble(
      type = xml2::xml_attr(r, "type"),
      target = xml2::xml_attr(r, "target"),
      contents = xml2::xml_text(r),
      text_id = tid
    )
  }, markup, text_id, SIMPLIFY = FALSE) |>
    dplyr::bind_rows()
  if (!nrow(refs)) {
    refs <- dplyr::tibble(type = character(0), target = character(0),
                          contents = character(0), text_id = integer(0))
  }
  urls <- refs[refs$type %in% "url", ]
  refs <- refs[!refs$type %in% "url", ]

  unmapped <- !refs$type %in% names(xref_types)
  if (any(unmapped)) {
    warn("METACHECK_XREF_TYPE_UNMAPPED", sprintf(
      "%d reference(s) of Grobid type %s have no xref_type and were left out.",
      sum(unmapped), paste(unique(refs$type[unmapped]), collapse = ", ")))
    refs <- refs[!unmapped, ]
  }
  refs$xref_type <- unname(xref_types[refs$type])
  # a <ref> naming several targets is one row per target
  target <- strsplit(refs$target, "\\s+")
  refs <- refs[rep(seq_len(nrow(refs)), lengths(target)), ]
  refs$target <- as.character(unlist(target))
  refs$target_id <- mapply(\(type, target) {
    if (is.na(target) || is.null(targets[[type]])) return(NA_integer_)
    unname(targets[[type]][sub("^#", "", target)])
  }, refs$xref_type, refs$target, USE.NAMES = FALSE) |>
    as.integer()

  # URLs, cleaned up as .grobid_to_bibr() does ----
  href <- gsub("\\s", "", urls$target) |> gsub("\\.$", "", x = _)
  link_text <- urls$contents
  same <- gsub("^https?://", "", href) ==
    gsub("^https?://", "", gsub("\\s", "", link_text))
  for (i in seq_along(same)) {
    text <- gsub(link_text[[i]], href[[i]], text, fixed = TRUE)
  }
  link_text[same] <- NA_character_

  # where each item is printed in its sentence, when that is unambiguous
  span <- function(tid, x) {
    s <- mapply(\(t, x) {
      if (is.na(t) || is.na(x) || !nzchar(x)) return(c(NA_integer_, NA_integer_))
      at <- gregexpr(x, t, fixed = TRUE)[[1]]
      if (length(at) != 1 || at[[1]] < 1) return(c(NA_integer_, NA_integer_))
      c(at[[1]] - 1L, at[[1]] - 1L + nchar(x))
    }, text[tid], x, USE.NAMES = FALSE)
    matrix(as.integer(s), nrow = 2)
  }
  xref_span <- span(refs$text_id, refs$contents)
  url_span <- span(urls$text_id, href) # the text now prints each href

  # authors and their affiliations ----
  au <- .tei_authors(xml)
  n_au <- nrow(au)
  au_col <- function(col) au[[col]] %||% rep(NA, n_au)
  aff <- na_if_empty(au_col("affiliation"))
  aff_text <- unique(aff[!is.na(aff)])
  orcid <- as.character(au_col("orcid"))
  orcid_pattern <- "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]"
  orcid <- ifelse(grepl(orcid_pattern, orcid),
                  sub(paste0(".*(", orcid_pattern, ").*"),
                      "https://orcid.org/\\1", orcid),
                  NA_character_)

  author <- list(
    author_id = seq_len(n_au),
    given = na_if_empty(au_col("given")),
    family = na_if_empty(au_col("family")),
    email = na_if_empty(au_col("email")),
    corresponding = rep(FALSE, n_au),
    orcid = orcid,
    role = rep(list(character(0)), n_au),
    credit_roles = rep(list(character(0)), n_au)
  )
  affiliation <- list(
    affiliation_id = seq_along(aff_text),
    text = aff_text,
    author_ids = lapply(aff_text, \(a) which(aff %in% a))
  )

  # metadata ----
  keywords <- .xml_find_text(xml, ".//textClass/keywords/term")
  if (identical(keywords, "")) keywords <- character(0)
  abstract <- xml2::xml_find_all(xml, ".//abstract //p") |>
    xml2::xml_text() |>
    squish() |>
    paste(collapse = " ")
  doi <- .xml_find1_text(xml, "//teiHeader//idno[@type='DOI']") |>
    na_if_empty()
  dois <- c(doi, bib_doi)
  bad_doi <- !is.na(dois) & is.na(.bibr12_doi(dois))
  if (any(bad_doi)) {
    warn("METACHECK_DOI_NOT_VALID", sprintf(
      "%d DOI(s) are not bare DOIs (10.xxxx/...) and were left out: %s",
      sum(bad_doi), paste(utils::head(dois[bad_doi], 3), collapse = "; ")))
  }
  metadata <- list(
    title = na_if_empty(.xml_find1_text(xml, ".//titleStmt/title")),
    abstract = na_if_empty(abstract),
    keywords = list(keywords),
    doi = .bibr12_doi(doi)
  )

  # extraction: Grobid produced it, metacheck converted it ----
  app <- xml2::xml_find_first(xml, "//teiHeader//application[@ident='GROBID']")
  if (inherits(app, "xml_missing")) {
    app <- xml2::xml_find_first(xml, "//teiHeader//application")
  }
  app_name <- na_if_empty(tolower(xml2::xml_attr(app, "ident")))
  app_version <- na_if_empty(xml2::xml_attr(app, "version"))
  producer <- list(
    name = if (is.na(app_name)) "unknown" else app_name,
    version = if (is.na(app_version)) "unknown" else app_version,
    build_sha = NULL
  )
  # when Grobid extracted the content, else when metacheck converted it
  completed_at <- .bibr12_utc(xml2::xml_attr(app, "when"))
  if (is.na(completed_at)) {
    completed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  # statistics, as .grobid_to_bibr() finds them ----
  paper_id <- tools::file_path_sans_ext(basename(src_path))
  tmp <- paper(paper_id)
  tmp$text <- data.frame(text = text, text_id = text_id,
                         paragraph_id = paragraph_id, section_id = text_section)
  tmp$section <- data.frame(section_id = sections$section_id,
                            header = sections$header,
                            section_type = sections$section_type)
  eq <- extract_eq(tmp)
  comps <- c("=", "<", ">", "~", "\u2248", "\u2260", "\u2264", "\u2265",
             "\u226A", "\u226B")
  comp_map <- c("<=" = "\u2264", "=<" = "\u2264", ">=" = "\u2265",
                "=>" = "\u2265", "<<" = "\u226A", ">>" = "\u226B",
                "~=" = "\u2248", "~~" = "\u2248", "==" = "=")
  eq$comp <- ifelse(eq$comp %in% names(comp_map), comp_map[eq$comp], eq$comp)
  off_vocab <- !eq$comp %in% comps
  if (any(off_vocab)) {
    warn("METACHECK_EQ_COMP_UNMAPPED", sprintf(
      "%d statistic(s) with comparator %s were left out.",
      sum(off_vocab), paste(unique(eq$comp[off_vocab]), collapse = ", ")))
    eq <- eq[!off_vocab, ]
  }

  tables <- list(
    author = author,
    affiliation = affiliation,
    text = list(text = text, text_id = text_id, paragraph_id = paragraph_id,
                section_id = text_section),
    section = sections,
    url = list(url_id = seq_along(href), href = href, link_text = link_text,
               text_id = urls$text_id, start = url_span[1, ],
               end = url_span[2, ]),
    bib = bib_tbl,
    xref = list(xref_id = seq_len(nrow(refs)), target_id = refs$target_id,
                xref_type = refs$xref_type, contents = refs$contents,
                text_id = refs$text_id, start = xref_span[1, ],
                end = xref_span[2, ]),
    figure = figure,
    table = table,
    footnote = footnote,
    eq = list(eq_id = seq_len(nrow(eq)), text_id = eq$text_id,
              grp_id = eq$grp_id, lhs = eq$lhs, df = eq$df, comp = eq$comp,
              rhs = eq$rhs)
  )

  extraction <- list(
    producer = producer,
    converter = list(
      name = "metacheck",
      version = as.character(utils::packageVersion("metacheck")),
      build_sha = NULL
    ),
    completed_at = completed_at,
    ocr = NULL,
    llm = NULL,
    warnings = warnings
  )

  info <- .bibr12_info(metadata, source, schema_version, producer)
  .bibr12_paper(paper_id, info, tables, extraction)
}

#' An ISO 8601 date and time as the UTC "YYYY-MM-DDTHH:MM:SSZ" of bibr 12.x
#'
#' @param x a date and time with a UTC offset, e.g. Grobid's
#'   "2025-07-10T14:15+0000"
#' @returns character, NA when `x` has no time or offset
#' @noRd
.bibr12_utc <- function(x) {
  x <- sub("Z$", "+0000", trimws(as.character(x)))
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  for (fmt in c("%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%dT%H:%M%z")) {
    time <- as.POSIXct(x, format = fmt, tz = "UTC")
    if (!is.na(time)) return(format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  NA_character_
}
