#' Convert Grobid TEI XML file to bibr format
#'
#' @param xml_file the XML file
#'
#' @returns a paper object
#' @export
grobid_to_bibr <- function(xml_file) {
  bibr_version = "5.6"

  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stopped grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    )
  xml <- xml2::read_xml(xml_text)

  file_hash <- substr(tools::md5sum(xml_file), 1, 14)
  paper <- paper(file_hash)

  # info ----
  title <- xml_find1(xml, ".//titleStmt/title")
  abstract <- xml_find1(xml, ".//abstract")

  keywords <- xml_find(xml, ".//textClass/keywords/term")
  if (is.null(keywords)) keywords <- character()

  doi <- xml_find1(xml, ".//idno[@type='DOI']")

  # authors ----
  author_nodes <- xml2::xml_find_all(xml, "//sourceDesc //author[persName]")
  authors <- lapply(seq_along(author_nodes), function(i) {
    a <- author_nodes[[i]]

    given <- xml_find1(a, ".//forename")
    family <- xml_find1(a, ".//surname")
    email <- xml_find1(a, ".//email")
    affiliation <- xml_find1(a, ".//affiliation")
    orcid <- xml_find1(a, ".//idno[@type='ORCID']")

    list(
      author_id = i,
      given = given,
      family = family,
      affiliation = affiliation,
      email = email,
      corresponding = FALSE,
      orcid = orcid,
      role = list(NULL)
    )
  })

  paper$authors <- dplyr::bind_rows(authors)

  # info ----

  paper$info <- list(
    title = title %||% "",
    description = abstract,
    keywords = keywords,
    doi = doi,
    file_hash = file_hash,
    input_format = "TEI XML",
    file_name = xml_file,
    bibr_version = bibr_version,
    paper_type = "unknown",
    paper_type_confidence = 0,
    oecd_l1 = NULL,
    oecd_l2 = NULL,
    oecd_confidence = 0
  )

  # text ----
  ft <- tei_full_text(xml)
  ft$p <- seq_along(ft$p)
  ft <- process_full_text(ft)

  paper$tbl <- ft[ft$section == "tab", ]
  paper$fig <- ft[ft$section == "fig", ]
  ft <- ft[!ft$section %in% c("fig", "tab"), ]
  ft <- ft[ft$text != ft$header, ]

  paper$text <- data.frame(
    text_id = seq_along(ft$text),
    paragraph_id = ft$p,
    section_id = ft$div,
    text = ft$text
  )

  sec <- dplyr::count(ft, div, header, section)
  paper$sections <- data.frame(
    section_id = sec$div,
    header = sec$header,
    section_type = sec$section
  )

  # bib ----
  bib <- tei_bib(xml)

  paper$bib <- data.frame(
    bib_id = gsub("b", "", bib$xref_id) |> as.integer(),
    bibtype = bib$bibtype %||% NA_character_,
    bib_text = format_ref(bib$ref) %||% NA_character_,
    doi = bib$doi %||% NA_character_,
    title = bib$title %||% NA_character_,
    author = bib$authors %||% NA_character_,
    journal_title = bib$journal %||% NA_character_,
    year = bib$year %||% NA_real_,
    volume = bib$volume %||% NA_character_,
    issue = bib$number %||% NA_character_,
    first_page = bib$first_page %||% NA_character_,
    last_page = bib$last_page %||% NA_character_,
    booktitle = bib$booktitle %||% NA_character_,
    editor = bib$editor %||% NA_character_,
    publisher = bib$publisher %||% NA_character_,
    isbn = NA_character_,
    issn = NA_character_,
    link = NA_character_,
    crossref_verified = FALSE,
    crossref_match = NA_character_,
    crossref_score = NA_real_
  )

  # xrefs ----
  paper$xrefs <- tei_xrefs(xml) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      text = paper$text$text[which.min(stringdist::stringdist(text, paper$text$text))]
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(paper$text, by = "text") |>
    dplyr::select(xref_id, xref_type = type, contents, text_id) |>
    dplyr::mutate(xref_type = dplyr::case_match(xref_type,
                                                "bibr" ~ "bib",
                                                "figure" ~ "fig",
                                                "table" ~ "tbl"),
                  xref_id = suppressWarnings(gsub("[a-z]", "", xref_id) |> as.integer()))

  # links ----
  links <- extract_urls(paper)
  paper$links <- data.frame(
    url = links$text,
    link_text = rep(NA_character_, nrow(links)),
    text_id = links$text_id
  )

  # equations ----
  paper$equations <- extract_equations(paper)

  return(paper)
}


#' Process full text table
#'
#' @param full_text a table of the full text
#'
#' @returns a data frame
#' @keywords internal
process_full_text <- function(full_text) {
  ## tokenize sentences ----
  # TODO: get tidytext to stop breaking sentences at "S.E. ="
  text <- NULL # hack to stop cmdcheck warning :(

  if (!is.null(full_text) && nrow(full_text) > 0) {
    ft <- full_text |>
      # stop initials getting parsed as sentences
      dplyr::mutate(text = gsub("\\b([A-Z])\\.", "\\1", text)) |>
      tidytext::unnest_sentences(text, text, to_lower = FALSE) |>
      dplyr::mutate(s = dplyr::row_number(), .by = c("div", "p"))
  } else {
    ft <- data.frame(
      header = character(0),
      text = character(0),
      div = double(0),
      p = double(0),
      s = double(0)
    )
  }

  # convert link notation to <url>
  ft$text <- ft$text |>
    gsub("\\{\\{", "<", x = _) |>
    gsub("\\}\\}", ">", x = _)

  # classify headers ----
  back <- !is.na(ft$section)
  back_sections <- ft$section[back]
  nospace_headers <- gsub("\\s", "", ft$header)
  abstract <- grepl("abstract", nospace_headers, ignore.case = TRUE)
  intro <- grepl("intro", nospace_headers, ignore.case = TRUE)
  method <- grepl("method|material", nospace_headers, ignore.case = TRUE)
  results <- grepl("result", nospace_headers, ignore.case = TRUE)
  discussion <- grepl("discuss", nospace_headers, ignore.case = TRUE)
  references <- grepl("bibliography|reference", nospace_headers, ignore.case = TRUE)
  ft$section <- rep(NA_character_, nrow(ft))
  ft$section[abstract] <- "abstract"
  ft$section[intro] <- "intro"
  ft$section[method] <- "method"
  ft$section[discussion] <- "discussion"
  ft$section[results] <- "results"
  ft$section[references] <- "references"
  ft$section[back] <- back_sections

  # beginning sections after abstract with no header labelled intro
  non_blanks <- which(!is.na(ft$section) & ft$section != "abstract")
  if (length(non_blanks) > 0) {
    blank_start <- non_blanks[[1]] - 1
    blanks <- rep(c(TRUE, FALSE), c(blank_start, length(ft$section) - blank_start))
    blanks[abstract] <- FALSE
    ft$section[blanks] <- "intro"
  }

  # check if sections with no label are Figure or Table
  first_s <- ft$p == 1 & ft$s == 1
  no_header <- substr(ft$header, 0, 4) == "[div"

  fig_n <- grepl("^Figure\\s*\\d+", ft$text)
  fig_divs <- ft[first_s & no_header & fig_n, "div"]
  ft[ft$div %in% fig_divs, "section"] <- "figure"

  tab_n <- grepl("^Table\\s*\\d+", ft$text)
  tab_divs <- ft[first_s & no_header & tab_n, "div"]
  ft[ft$div %in% tab_divs, "section"] <- "table"

  # assume sections are the same class as previous if unclassified (after abstract)
  for (i in seq_along(ft$section)) {
    if (i > 1 &
        !abstract[i] &
        isFALSE(abstract[i - 1]) &
        is.na(ft$section[i])) {
      ft$section[i] <- ft$section[i - 1]
    }
  }

  colorder <- c("text", "section", "header", "div", "p", "s")

  blank_divs <- grepl("\\[div-\\d+\\]", ft$text)
  # blank_divs <- ft$p == 0

  body_table <- ft[!blank_divs, colorder]
  rownames(body_table) <- NULL

  return(body_table)
}

#' Get full text from TEI type XML
#'
#' @param xml The XML
#'
#' @return a data frame with all text
#' @keywords internal
tei_full_text <- function(xml) {
  div <- NULL # ugh cmdcheck

  ## abstract ----
  abst_table <- data.frame(
    header = "Abstract",
    text = xml_find(xml, ".//abstract //p"),
    div = 0
  )
  abst_table$p <- seq_along(abst_table$text)

  ## body ----
  divs <- xml2::xml_find_all(xml, "//text //body //div")
  div_text <- lapply(seq_along(divs), \(i){
    div <- divs[[i]]
    header <- xml2::xml_find_first(div, ".//head") |> xml2::xml_text()
    if (is.na(header)) header <- sprintf("[div-%02d]", i)
    paragraphs <- xml_find(div, ".//p")
    df <- data.frame(
      header = header,
      text = c(header, paragraphs),
      div = i,
      p = c(0, seq_along(paragraphs))
    )
  })

  # back matter ----
  back <- xml2::xml_find_all(xml, "//back //div")
  types <- xml2::xml_attr(back, "type") |>
    setdiff(c(NA, "references"))
  back_text <- lapply(types, function(t) {
    str <- paste0("//back //div[@type='", t, "'] //div")
    divs <- xml2::xml_find_all(xml, str)
    b_text <- lapply(seq_along(divs), \(i){
      div <- divs[[i]]
      header <- xml_find1(div, ".//head")
      paragraphs <- xml_find(div, ".//p")
      df <- data.frame(
        header = header,
        text = c(header, paragraphs),
        div = NA,
        p = c(0, seq_along(paragraphs)),
        section = t
      )
    })

    do.call(rbind, b_text)
  }) |> do.call(rbind, args = _)

  # make divs increment (this is gross code)
  if (!is.null(back_text)) {
    start <- length(div_text) + 1
    end <- sum(back_text$p == 0) + start - 1
    back_text$div[back_text$p == 0] <- start:end
    back_text <- tidyr::fill(back_text, div)
  }

  ## add figures and tables ----
  # TODO: get sentences with internal refs to figs
  figs <- xml2::xml_find_all(xml, "//figure")
  figtbl <- lapply(figs, \(fig) {
    figid <- xml2::xml_attr(fig, "id")

    data.frame(
      header = xml_find1(fig, ".//head"),
      text = xml_find1(fig, ".//figDesc"),
      section = sub("_\\d+$", "", x = figid),
      div = sub("^(fig|tab)_", "", x = figid) |> as.numeric(),
      p = 1
    )
  }) |> do.call(rbind, args = _)
  figtbl <- figtbl %||% data.frame()

  ## add footnotes ----
  notes <- xml2::xml_find_all(xml, "//note[@place='foot']")
  notetbl <- lapply(notes, \(note) {
    noteid <- xml2::xml_attr(note, "id")
    data.frame(
      header = "",
      text = xml2::xml_text(note),
      section = sub("_\\d+$", "", x = noteid),
      div = sub("^foot_", "", x = noteid) |> as.numeric()
    )
  }) |> do.call(rbind, args = _)
  notetbl <- notetbl %||% data.frame()

  all_tables <- c(
    list(abst_table),
    div_text,
    list(
      back_text,
      figtbl,
      notetbl
    )
  )
  full_text <- do.call(dplyr::bind_rows, all_tables)

  return(full_text)
}

#' Get cross references from TEI type XML
#'
#' @param xml The XML
#'
#' @return xrefs table
#' @keywords internal
tei_xrefs <- function(xml) {
  text <- xref_id <- type <- NULL
  xrefs <- xml2::xml_find_all(xml, "//ref")
  if (length(xrefs) == 0) {
    return(data.frame(
      xref_id = character(0),
      type = character(0),
      contents = character(0),
      text = character(0)
    ))
  }

  types <- sapply(xrefs, xml2::xml_attr, "type")
  targets <- sapply(xrefs, xml2::xml_attr, "target")
  contents <- sapply(xrefs, xml2::xml_text)
  p <- lapply(xrefs, xml2::xml_parent) |>
    sapply(as.character) |>
    gsub("</?p>", "", x = _)

  # get in-text citation
  xref_data <- data.frame(
    i = seq_along(xrefs),
    xref_id = sub("#", "", targets),
    type = types,
    contents = contents,
    p = p
  ) |>
    # stop initials getting parsed as sentences
    dplyr::mutate(p = gsub("\\b([A-Z])\\.", "\\1", p)) |>
    tidytext::unnest_sentences(output = "text", input = "p", to_lower = FALSE) |>
    dplyr::filter(grepl("<ref", text, fixed = TRUE)) |>
    dplyr::rowwise() |>
    dplyr::filter(
      (is.na(xref_id) & grepl(contents, xml2::read_html(text) |> xml2::xml_text(), fixed = TRUE)) |
        grepl(paste0("#", xref_id), text, fixed = TRUE)
    )

  if (nrow(xref_data) > 0) {
    xref_data <- xref_data |>
      dplyr::mutate(text = xml2::read_html(text) |> xml2::xml_text()) |>
      dplyr::ungroup() |>
      dplyr::arrange(type, gsub("\\D", "", x = xref_id) |> as.integer())
  }

  xrefs <- xref_data[c("xref_id", "type", "contents", "text")] |> unique()
  return(xrefs)
}

#' Get bibliography from TEI type XML
#'
#' @param xml The XML
#'
#' @return bib table
#' @keywords internal
tei_bib <- function(xml) {
  refs <- xml2::xml_find_all(xml, "//listBibl //biblStruct")

  if (length(refs) > 0) {
    bib_table <- data.frame(
      xref_id = xml2::xml_attr(refs, "id")
    )
    # ref_table$doi <- xml2::xml_find_first(refs, ".//analytic //idno[@type='DOI']") |>
    #   xml2::xml_text()

    bibs <- lapply(refs, xml2bib)
    bib_table$ref <- bibs

    # pull visible text on error
    # deal with rare print format errors
    # e.g., doi = "10.1177/\\penalty-\\@M002383099704000203"
    formatted <- bibs |>
      sapply(\(bib) {
        suppressWarnings({ # TODO: log this
          tryCatch(format(bib), error = \(e) {
            tryCatch(format(bib, "md"), error = \(e) {
              return("")
            })
          })
        })
      }) |>
      gsub("\\n", " ", x = _)

    bib_errors <- which(formatted == "")

    if (length(bib_errors) > 0) {
      bib_table$ref[[bib_errors]] <- refs[[bib_errors]] |>
        xml2::xml_text() |>
        gsub("\\s+", " ", x = _) |>
        trimws()
    }

    bib_table$doi <- sapply(bibs, \(x) x$doi %||% NA_character_)
    bib_table$bibtype <- sapply(bibs, \(x) x$bibtype %||% NA_character_)
    bib_table$title <- sapply(bibs, \(x) x$title %||% NA_character_)
    bib_table$journal <- sapply(bibs, \(x) x$journal %||% NA_character_)
    bib_table$year <- sapply(bibs, \(x) x$year %||% NA_integer_)
    bib_table$authors <- lapply(bibs, \(x) x$author %||% NA_character_) |>
      sapply(paste, collapse = ", ")
  } else {
    bib_table <- data.frame(
      xref_id = character(0),
      doi = character(0),
      ref = character(0)
    )
  }

  return(bib_table)
}


#' Find and return info from XML by xpath
#'
#' @param xml the xml xmlument, node, or nodeset
#' @param xpath a string containing an xpath expression
#' @param join optional string to join vectors
#'
#' @returns text
#' @keywords internal
xml_find <- function(xml, xpath, join = NULL) {
  text <- xml2::xml_find_all(xml, xpath) |>
    xml2::xml_text(trim = TRUE) |>
    gsub("\\s+", " ", x = _)

  if (!is.null(join)) text <- paste(text, collapse = join)

  if (length(text) == 0) text <- ""

  return(text)
}

#' Find and return first info from XML by xpath
#'
#' @param xml the xml xmlument, node, or nodeset
#' @param xpath a string containing an xpath expression
#' @param join optional string to join vectors
#'
#' @returns text
#' @keywords internal
xml_find1 <- function(xml, xpath, join = NULL) {
  xml_find(xml, xpath, join)[[1]]
}

#' Find and return date info from XML
#'
#' @param xml the xml node
#' @param xpath a string containing an xpath expression
#'
#' @returns text
#' @keywords internal
xml_date <- function(xml, xpath = ".//string-date") {
  date <- xml2::xml_find_first(xml, xpath)

  m <- date |>
    xml2::xml_find_first(".//month") |>
    xml2::xml_attr("number") |>
    as.numeric()
  d <- xml_find1(date, ".//day") |> as.numeric()
  y <- xml_find1(date, ".//year") |> as.numeric()

  if (is.na(m) & is.na(d) & is.na(y)) {
    return(NULL)
  }
  sprintf("%d-%02d-%02d", y, m, d)
}

#' Parse XML bib format to bibtex
#'
#' @param ref the biblStruct xml object
#'
#' @returns a bibentry
#' @export
#' @keywords internal
xml2bib <- function(ref) {
  b <- list(bibtype = "misc")

  b$doi <- xml_find1(ref, ".//idno[@type='DOI']")

  b$title <- xml_find1(ref, ".//title[@level='a']")

  b$author <- xml2::xml_find_all(ref, ".//author //persName") |>
    lapply(\(a) {
      forename <- xml_find(a, ".//forename", join = " ")
      surname <- xml_find(a, ".//surname", join = " ")

      utils::person(
        given = forename,
        family = surname
      )
    }) |>
    do.call(base::c, args = _)

  b$editor <- xml2::xml_find_all(ref, ".//editor //persName") |>
    lapply(\(a) {
      forename <- xml_find(a, ".//forename", join = " ")
      surname <- xml_find(a, ".//surname", join = " ")

      utils::person(
        given = forename,
        family = surname
      )
    }) |>
    do.call(base::c, args = _)

  b$journal <- xml_find1(ref, ".//title[@level='j']")

  b$booktitle <- xml_find1(ref, ".//title[@level='m']")

  # imprint
  imprint <- xml2::xml_find_first(ref, ".//imprint")
  b$publisher <- xml_find1(imprint, ".//publisher")
  b$year <- xml_find1(imprint, ".//date[@type='published']")
  b$volume <- xml_find1(imprint, ".//biblScope[@unit='volume']")
  b$number <- xml_find1(imprint, ".//biblScope[@unit='issue']")
  page_unit <- xml2::xml_find_first(imprint, ".//biblScope[@unit='page']")
  if (!is.na(page_unit)) {
    pages <- xml2::xml_text(page_unit)
    if (pages == "") {
      pages <- xml2::xml_attrs(page_unit)
      if (!is.na(pages[[1]])) {
        b$pages <- paste(pages[["from"]], pages[["to"]], sep = "-")
        b$first_page <- pages[["from"]]
        b$last_page <- pages[["to"]]
      }
    } else {
      b$pages <- pages
      b$first_page <- pages
    }
  }

  b[is.na(b)] <- NULL
  if (!is.null(b$journal)) {
    b$bibtype <- "article"
    if (is.null(b$year)) {
      # b$bibtype <- "unpublished"
      note <- xml2::xml_find_first(ref, ".//note") |> xml2::xml_text()
      b$year <- note %||% "no year"
    }
  } else if (!is.null(b$booktitle)) {
    b$bibtype <- "incollection"
    if (is.null(b$title)) {
      b$bibtype <- "book"
      b$title <- b$booktitle
      b$booktitle <- NULL
    }
  }

  bib <- tryCatch(do.call(utils::bibentry, b),
                  error = function(e) {
                    b$bibtype <- "misc"
                    bib <- do.call(utils::bibentry, b)
                    return(bib)

                    # TODO: fix more types
                    # warning(e$message, "\\n")
                    # return(txt)
                  }
  )

  bib
}
