#' Get paper from XML or text file
#'
#' This should work with XML files in TEI (grobid), JATS APA-DTD, NLM-DTD and cermine formats, plus basic parsing of plain text files.
#'
#' @param filename the path to the file, a vector of file paths, or the path to a directory
#'
#' @return A paper object with class scivrs_paper, or a list of paper objects
#' @export
#'
#' @examples
#' # paper <- read(filename)
read <- function(filename) {
  text <- NULL

  if (length(filename) > 1) {
    # handle list of files or a directory----

    ## set up progress bar ----
    pb <- pb(length(filename),
             "Processing file [:bar] :current/:total :elapsedfull")

    ## get unique names ----
    dirs <- filename |>
      sapply(strsplit, split = "/")
    maxlen <- sapply(dirs, length) |> max()
    dir_df <- lapply(dirs, \(x) {
      x[1:maxlen]
    }) |>
      as.data.frame() |>
      t()
    distinct_vals <- apply(dir_df, 2, unique) |> lapply(length) > 1
    unique_names <- dir_df[ , distinct_vals, drop = FALSE] |>
      apply(1, \(row) {
        row[!is.na(row)] |> paste0(collapse = "/")
      }) |>
      gsub("\\.(xml|txt|docx)$", "", x = _, ignore.case = TRUE)

    p <- lapply(filename, \(x) {
      p1 <- tryCatch(read(x),
                     error = \(e) {
                       warning(x, " did not read correctly",
                               call. = FALSE)
                       return(NULL)
                     })
      pb$tick()
      p1
    })

    names(p) <- unique_names
    for (un in unique_names) {
      if (!is.null(p[[un]])) {
        p[[un]]$id <- un
        if (nrow(p[[un]]$full_text) > 0) {
          p[[un]]$full_text$id <- un
        }
      }
    }

    # remove NULLs
    valid <- !sapply(p, is.null)
    return(paperlist(p[valid]))
  } else if (dir.exists(filename)) {
    ## iterate over a directory ----
    files <- list.files(filename, "\\.(xml|txt|docx)",
                       full.names = TRUE,
                       recursive = TRUE)
    if (length(files) == 0) {
      stop("There are no xml, docx or txt files in the directory ", filename)
    }
    p <- read(files)
    return(p)
  }

  ## single file filename ----
  if (!file.exists(filename)) {
    warning("The file ", filename, " does not exist.")
    return(NULL)
  }

  ## read xml ----
  xml <- tryCatch(read_xml(filename),
                  error = function(e) {
                    return(NULL)
                  })

  # return nothing if the file can't be read, so iteration doesn't fail
  if (is.null(xml) || isFALSE(xml)) {
    warning("The file ", filename, " was not valid XML, docx or plain text", call. = FALSE)
    return(NULL)
  }

  ## XML type ----
  dtd <- xml2::xml_attr(xml, "dtd")
  # APA uses APA JATS
  apa <- grepl("//APA//DTD", dtd)
  # PLOS1 and cermine use NLM JATS
  nlm <- grepl("//NLM//DTD", dtd)
  # https://jats.nlm.nih.gov/archiving/tag-library/1.1/
  xml_name <- xml2::xml_name(xml)
  word <- grepl("office.word", xml2::xml_attrs(xml)) |> any()

  xml_type <- dplyr::case_when(
    isTRUE(apa) ~ "apa",
    isTRUE(nlm) ~ "nlm",
    xml_name == "TEI" ~ "tei",
    xml_name == "article" ~ "nlm",
    word ~ "word",
    .default = "text"
  )

  ## set up paper object ----
  p <- paper()
  id <- basename(filename) |>
    gsub("\\.(xml|txt)$", "", x = _, ignore.case = TRUE)

  if (xml_type == "apa") {
    p$info <- apa_info(xml)
    p$authors <- apa_authors(xml)
    full_text <- apa_full_text(xml)
    p$bib <- jats_bib(xml)
    p$xrefs <- jats_xrefs(xml)
  } else if (xml_type == "nlm") {
    p$info <- nlm_info(xml)
    p$authors <- nlm_authors(xml)
    full_text <- nlm_full_text(xml)
    p$bib <- jats_bib(xml)
    p$xrefs <- jats_xrefs(xml)
  } else if (xml_type == "tei") {
    p$info <- tei_info(xml)
    p$authors <- tei_authors(xml)
    full_text <- tei_full_text(xml)
    p$bib <- tei_bib(xml)
    p$xrefs <- tei_xrefs(xml)
  } else if (xml_type == "text") {
    p$info <- text_info(xml)
    p$authors <- text_authors(xml)
    full_text <- text_full_text(xml)
    p$bib <- text_bib(xml)
    p$xrefs <- text_xrefs(xml)
  } else if (xml_type == "word") {
    p$info <- word_info(xml)
    p$authors <- word_authors(xml)
    full_text <- word_full_text(filename)
    p$bib <- word_bib(xml)
    p$xrefs <- word_xrefs(xml)
  }

  p$info$filename <- filename
  p$full_text <- process_full_text(full_text)

  # add paper ID to tables
  p$id <- id
  p$full_text$id <- rep(id, nrow(p$full_text))
  p$xrefs$id <- rep(id, nrow(p$xrefs))
  p$bib$id <- rep(id, nrow(p$bib))

  # join sentence location to xrefs
  pos_cols <- c("text", "section", "div", "p", "s")
  first_text <- p$full_text[pos_cols] |>
    dplyr::slice(1, .by = text)
  p$xrefs <- dplyr::left_join(p$xrefs, first_text, by = "text")

  return(p)
}

#' @rdname read
#' @export
read_grobid <- read

#' @rdname read
#' @export
read_cermine<- read

#' @rdname read
#' @export
read_text <- read

#' Read XML
#'
#' @param filename The path to the XML file to be read
#'
#' @returns An XML object
#' @keywords internal
read_xml <- function(filename) {
  ext <- gsub(".*\\.", "", filename) |> tolower()
  xml <- NULL

  if (ext == "xml") {
     raw_text <- filename |>
      readLines(warn = FALSE)

     # check doctype
     doctype <- grep("^\\s*<!DOCTYPE", x = raw_text,
                     value = TRUE, ignore.case = TRUE) |>
       trimws()
     doc_elements <- strsplit(doctype, "\"")
     dtd <- ""
     if (length(doc_elements) > 0 &&
         length(doc_elements[[1]]) >= 2) {
       dtd <- doc_elements[[1]][[2]]
     }

     xml_text <- raw_text |>
      paste(collapse = "\n") |>
      # fixes a glitch that stopped grobid xml from being read
      gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
           x = _, fixed = TRUE) |>
      # get rid of sentence tags
      gsub("</s><s>", " ", x = _) |>
      gsub("</?s>", "", x = _) |>
      # TODO: handle this better in full_text functions
      # replace URL links with markdown style
      gsub("<ref type=\"url\" target=\"([^\"]+)\">([^<]+)</ref>",
           "{{\\1}}", x = _) |>
      gsub("<ext-link[^>]+xlink:href=\"([^\"]+)\"[^>]*>([^<]+)</ext-link>",
           "{{\\1}}", x = _)

    xml <- tryCatch({
      myxml <- xml2::read_xml(xml_text)
      xml2::xml_attr(myxml, "dtd") <- dtd
      myxml
    }, error = function(e) { return(NULL) })
  } else if (ext == "docx") {
    docx <- officer::read_docx(filename)
    summary <- officer::docx_summary(docx)
    xml <- officer::docx_body_xml(docx)
  } else {
    # read everything else as plain text
    raw_text <- readLines(filename, warn = FALSE)

    xml_text <- raw_text[nzchar(raw_text)] |>
      htmltools::htmlEscape() |>
      paste0("<p>", x = _, "</p>") |>
      paste(collapse = "\n") |>
      paste('<?xml version="1.0" encoding="UTF-8"?>',
            "<text>", x = _, "</text>\n",
            sep = "\n")

    xml <- tryCatch(xml2::read_xml(xml_text),
                    error = function(e) { return(NULL) })
  }

  return(xml)
}

#' Find and return info from XML by xpath
#'
#' @param xml the xml document, node, or nodeset
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

  return (text)
}

#' Find and return first info from XML by xpath
#'
#' @param xml the xml document, node, or nodeset
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

  m <- date |> xml2::xml_find_first(".//month") |>
    xml2::xml_attr("number") |> as.numeric()
  d <- xml_find1(date, ".//day") |> as.numeric()
  y <- xml_find1(date, ".//year") |> as.numeric()

  if (is.na(m) & is.na(d) & is.na(y)) return(NULL)
  sprintf("%d-%02d-%02d", y, m, d)
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
        isFALSE(abstract[i-1]) &
        is.na(ft$section[i]) ) {
      ft$section[i] <- ft$section[i-1]
    }
  }

  colorder <- c("text", "section", "header", "div", "p", "s")

  blank_divs <- grepl("\\[div-\\d+\\]", ft$text)
  #blank_divs <- ft$p == 0

  body_table <- ft[!blank_divs, colorder]
  rownames(body_table) <- NULL

  return(body_table)
}

# APA JATS ----

#' Get article info from JATS APA-DTD type XML
#'
#' @param xml The XML
#'
#' @return an info list
#' @keywords internal
apa_info <- function(xml) {
  info <- list()

  info$title <- xml_find1(xml, "//article-title")
  info$description <-  xml_find(xml, "//abstract //p", join = "\n\n")
  info$keywords <- xml_find(xml, "//kwd-group //kwd")
  info$doi <- xml_find(xml, "//article-id[@pub-id-type='doi']")

  # print_pub_date
  info$pub_print <- xml_date(xml, "//pub-date[@pub-type='print']")
  info$pub_online <- xml_date(xml, "//pub-date[@pub-type='online']")
  info$received <- xml_date(xml, "//history //string-date[@content-type='received']")
  info$revised <- xml_date(xml, "//history //string-date[@content-type='revised']")
  info$accepted <- xml_date(xml, "//history //string-date[@content-type='accepted']")


  return(info)
}

#' Get author info from JATS APA-DTD type XML
#'
#' @param xml The XML
#'
#' @return an author list
#' @keywords internal
apa_authors <- function(xml) {
    s <- study()
    authors <- xml2::xml_find_all(xml, "//contrib-group[@content-type='primary-authors'] //contrib[@contrib-type='author']")

    affiliations <- xml2::xml_find_all(xml, "//contrib-group[@content-type='primary-authors'] //aff")
    affs <- xml2::xml_text(affiliations)
    names(affs) <- xml2::xml_attr(affiliations, "id")

    for (a in authors) {
      family <- xml2::xml_find_first(a, ".//surname") |>
        xml2::xml_text() |> trimws()
      given <- xml2::xml_find_all(a, ".//given-names") |>
        xml2::xml_text() |> trimws()
      email <- xml2::xml_find_all(a, ".//email") |> xml2::xml_text()
      orcid <- xml2::xml_find_all(a, ".//contrib-id[@contrib-id-type='orcid']") |> xml2::xml_text()

      if (length(orcid) == 0) orcid = NULL

      # get affiliations
      rids <- xml2::xml_attr(a, "rid") |> strsplit("\\s")
      my_affs <- intersect(rids[[1]], names(affs))
      affiliation <- affs[my_affs] |> unname()

      s <- add_author(s, family, given, orcid, email = email,
                      affiliation = affiliation)
    }

    return(s$authors)
}

#' Get full text from JATS APA-DTD type XML
#'
#' @param xml The XML
#'
#' @return a data frame with all text
#' @keywords internal
apa_full_text <- function(xml) {
  ## abstract ----
  abst_table <- data.frame(
    header = "Abstract",
    text = xml_find(xml, ".//abstract //p"),
    div = 0
  )
  abst_table$p <- seq_along(abst_table$text)

  ## body ----
  divs <- xml2::xml_find_all(xml, "//body //sec")
  div_text <- lapply(seq_along(divs), \(i){
    div <- divs[[i]]
    header <- xml_find1(div, ".//title")
    if (is.na(header) | header == "-") header <- sprintf("[div-%02d]", i)
    paragraphs <- xml2::xml_find_all(div, ".//p") |>
      xml2::xml_text(trim = TRUE) |>
      gsub("\\s+", " ", x = _)
    df <- data.frame(
      header = header,
      text = c(header, paragraphs),
      div = i,
      p = c(0, seq_along(paragraphs))
    )
  })

  ## add figures and tables ----
  # TODO: get sentences with internal refs to figs
  figs <- xml2::xml_find_all(xml, "//fig")
  figtbl <- lapply(figs, \(fig) {
    id <- xml_find1(fig, ".//label")|> as.numeric()
    title <- xml_find(fig, ".//caption //title")
    p <- xml_find(fig, ".//caption //p")
    if(length(p) == 0) p <- title
    data.frame(
      header = title,
      text = c(title, p),
      section = "fig",
      div = id,
      p = c(0, seq_along(p))
    )
  }) |> do.call(dplyr::bind_rows, args = _)

  ## author notes
  authornotes <- xml2::xml_find_all(xml, "//author-notes")
  authnotetbl <- data.frame()
  if (length(authornotes) > 0) {
    authnotetbl <- data.frame(
      header = "Author Notes",
      text = xml_find(authornotes, ".//p"),
      section = "annex",
      div = 0
    )
  }

  ## add footnotes ----
  notes <- xml2::xml_find_all(xml, "//fn-group //fn")
  notetbl <- lapply(notes, \(note) {
    id <- xml_find1(note, ".//label") |> as.numeric()
    p <- xml_find(note, ".//p")
    if(length(p) == 0) p <- ""
    data.frame(
      header = xml2::xml_attr(note, "id"),
      text = p,
      section = "foot",
      div = id,
      p = seq_along(p)
    )
  }) |> do.call(dplyr::bind_rows, args = _)

  all_tables <- c(list(abst_table),
                  div_text,
                  list(notetbl,
                       authnotetbl,
                       figtbl))
  full_text <- do.call(dplyr::bind_rows, all_tables)

  return(full_text)
}

#' Get cross references from JATS APA-DTD or NLM DTD type XML
#'
#' @param xml The XML
#'
#' @return xrefs table
#' @keywords internal
jats_xrefs <- function(xml) {
  text <- i <- xref_id <- type <- NULL

  xrefs <- xml2::xml_find_all(xml, "//xref")
  if (length(xrefs) == 0) {
    return(data.frame(
      xref_id = character(0),
      type = character(0),
      contents = character(0),
      text = character(0)
    ))
  }

  types <- sapply(xrefs, xml2::xml_attr, "ref-type")
  targets <- sapply(xrefs, xml2::xml_attr, "rid")
  id <- sapply(xrefs, xml2::xml_attr, "id")
  if (all(is.na(id))) id <- targets
  contents <- sapply(xrefs, xml2::xml_text)
  p <- lapply(xrefs, xml2::xml_parent) |>
    sapply(as.character) |>
    gsub("</?p>", "", x = _)

  xref_data <- data.frame(
    i = id,
    xref_id = sub("#", "", targets),
    type = types,
    contents = contents,
    p = p
  ) |>
    # stop initials getting parsed as sentences
    dplyr::mutate(p = gsub("\\b([A-Z])\\.", "\\1", p)) |>
    tidytext::unnest_sentences(output = "text", input = "p", to_lower = FALSE) |>
    dplyr::filter(grepl("<xref", text, fixed = TRUE)) |>
    dplyr::rowwise() |>
    dplyr::filter(grepl(i, text, fixed = TRUE)) |>
    dplyr::mutate(text = xml2::read_html(text) |> xml2::xml_text()) |>
    dplyr::ungroup() |>
    # Cermine lumps refs together, so split :(
    # should split contents too,
    # but splitting on ; doesn't always match n of refs split
    tidyr::separate_longer_delim(xref_id, delim = " ") |>
    dplyr::arrange(type,
                   gsub("\\D", "", x = xref_id) |> as.integer())

  xrefs <- xref_data[c("xref_id", "type", "contents", "text")] |> unique()

  return(xrefs)
}

#' Get bibliography from JATS APA-DTD or NLM DTD type XML
#'
#' @param xml The XML
#'
#' @return an list of two tables: references and citations
#' @keywords internal
jats_bib <- function(xml) {
  refs <- xml2::xml_find_all(xml, "//back //ref-list //ref")

  bib_table <- lapply(refs, \(ref) {
    cite <- xml2::xml_find_first(ref, ".//mixed-citation")

    # authors
    names <- xml2::xml_find_all(cite, ".//person-group[@person-group-type='author']")
    if (length(names) == 0) {
      names <- xml2::xml_find_all(cite, ".//string-name")
    }
    surnames <- sapply(names, xml_find, ".//surname")
    givennames <- sapply(names, xml_find, ".//given-names")
    authors <- paste(givennames, surnames) |> paste(collapse = "; ")

    # editors
    names <- xml2::xml_find_all(cite, ".//person-group[@person-group-type='editor']")
    surnames <- sapply(names, xml_find, ".//surname")
    givennames <- sapply(names, xml_find, ".//given-names")
    editors <- paste(givennames, surnames) |> paste(collapse = "; ")

    pubtype <- xml2::xml_attr(cite, "publication-type")
    if (is.na(pubtype)) {
      # try to guess
      titles <- xml_find(cite, ".//article-title")
      pubtype <- dplyr::case_when(
        length(titles) > 1 ~ "book-chapter",
        xml_find(cite, ".//volume") != "" ~ "journal",
        .default = "other"
      )
    }
    bibtype <- dplyr::case_match(pubtype,
                                 "journal" ~ "Article",
                                 "book-chapter" ~ "InCollection",
                                 "book" ~ "Book",
                                 "other" ~ "Misc",
                                 .default = "Misc")

    id <- xml2::xml_attr(cite, "id")
    if (is.na(id)) id <- xml2::xml_attr(ref, "id")

    data.frame(
      xref_id = id,
      ref = xml2::xml_text(ref) |> gsub("\\s+", " ", x = _),
      doi = xml_find1(ref, ".//pub-id[@pub-id-type='doi']"),
      bibtype = bibtype,
      title = xml_find1(cite, ".//article-title"),
      journal = xml_find1(cite, ".//source"),
      authors = authors,
      year =   xml_find1(cite, ".//year"),
      volume = xml_find1(cite, ".//volume"),
      issue =  xml_find1(cite, ".//issue"),
      fpage =  xml_find1(cite, ".//fpage"),
      lpage =  xml_find1(cite, ".//lpage")
    )
  }) |> do.call(dplyr::bind_rows, args = _)

  return(bib_table)
}

# NLM JATS ----
# Used by PLOS1 and Cermine

#' Get article info from JATS NLM-DTD type XML
#'
#' @param xml The XML
#'
#' @return an info list
#' @keywords internal
nlm_info <- function(xml, filename = "") {
  info <- list()

  info$title <- xml_find(xml, "//front //article-title")
  info$description <-  xml_find(xml, "//abstract //p", join = "\n\n")
  info$keywords <- xml_find(xml, "//kwd-group //kwd") |>
    gsub("^eol>", "", x = _)
  info$doi <- xml_find(xml, "//front //pub-id[@pub-id-type='doi']")

  return(info)
}

#' Get author info from JATS NLM-DTD type XML
#'
#' @param xml The XML
#'
#' @return an author list
#' @keywords internal
nlm_authors <- function(xml) {
  s <- study()
  authors <- xml2::xml_find_all(xml, "//contrib-group //contrib[@contrib-type='author']")

  for (a in authors) {
    name <- xml_find(a, ".//string-name")
    m <- gregexpr(" (van |de |van der |van de |della )?[\\S]+$", name,
                  perl = TRUE, ignore.case = TRUE)
    family <- regmatches(name, m) |> trimws()
    given <- sub(paste0(family, "$"), "", name) |> trimws()
    email <- xml_find(a, ".//email")
    orcid <- xml_find(a, ".//idno[@type='ORCID']")
    if (orcid == "") orcid = NULL

    affs <- xml2::xml_find_all(a, ".//xref[@ref-type='aff']")
    affiliation <- lapply(affs, function(aff) {
      rid <- xml2::xml_attr(aff, "rid") |>
        sprintf(".//aff[@id='%s']", x = _)
      aff_info <- xml2::xml_find_all(xml, rid)
      org <- xml_find(aff_info, ".//institution")
      org
    })

    s <- add_author(s, family, given, orcid, email = email,
                    affiliation = affiliation)
  }

  return(s$authors)
}

#' Get full text from JATS NLM-DTD type XML
#'
#' @param xml The XML
#'
#' @return a data frame with all text
#' @keywords internal
nlm_full_text <- function(xml) {
  ## abstract ----
  abst_table <- data.frame(
    header = "Abstract",
    text = xml_find(xml, ".//abstract //p"),
    div = 0
  )
  abst_table$p <- seq_along(abst_table$text)

  ## body ----
  divs <- xml2::xml_find_all(xml, "//body //sec")
  div_text <- lapply(seq_along(divs), \(i){
    div <- divs[[i]]
    header <- xml2::xml_find_first(div, ".//title") |> xml2::xml_text()
    if (is.na(header) | header == "-") header <- sprintf("[div-%02d]", i)
    paragraphs <- xml2::xml_find_all(div, ".//p") |>
      xml2::xml_text(trim = TRUE) |>
      gsub("\\s+", " ", x = _)
    df <- data.frame(
      header = header,
      text = c(header, paragraphs),
      div = i,
      p = c(0, seq_along(paragraphs))
    )
  })



  full_text <- do.call(dplyr::bind_rows, c(list(abst_table),
                                         div_text
  ))

  return(full_text)
}



# TEI ----
# used by grobid

#' Get article info from TEI type XML
#'
#' @param xml The XML
#'
#' @return an info list
#' @keywords internal
tei_info <- function(xml, filename = "") {
  info <- list()

  info$title <- xml_find1(xml, "//titleStmt //title")
  info$description <- xml_find(xml, "//abstract //p", join = "\n\n")
  info$keywords <- xml_find(xml, "//keywords //term")
  info$doi <- xml_find(xml, "//sourceDesc //idno[@type='DOI']")

  # parse submission dates, which suck
  tryCatch({
    info$submission <- xml_find(xml, "//sourceDesc //note[@type='submission']")

    # set up date regex
    # TODO: move this to utilities
    months <- "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|January|February|March|April|June|July|August|September|October|November|December"
    pattern <- paste0(
      "(", months, "|\\d{1,4})",
      "[- /\\.](", months, "|\\d{1,2}),?",
      "[- /\\.]\\d{2,4}"
    )

    m <- gregexpr(pattern, info$submission, ignore.case = TRUE)
    dates <- regmatches(info$submission, m)
    if (length(dates[[1]] > 0)) {
      parsed_dates <- suppressWarnings(anytime::anydate(dates[[1]]))
      parsed_dates_na <- is.na(parsed_dates) |> which()

      for (i in parsed_dates_na) {
        d <- dates[[1]][i]
        if (grepl("\\d{1,2}/\\d{1,2}/\\d{2}", d)) {
          # stupid psychsci format :(
          parts <- strsplit(d, "/")[[1]] |> as.integer()
          thisyear <- Sys.Date() |> format("%Y") |> as.integer()
          parts[3] <- ifelse(parts[3] < thisyear + 1,
                             parts[3] + 2000, parts[3] + 1900)
          d <- sprintf("%d-%02d-%02d", parts[3], parts[1], parts[2])
        }

        parsed_dates[i] <- d
      }

      received <- gregexpr("received", info$submission, ignore.case = TRUE)[[1]][[1]]
      accepted <- gregexpr("accepted", info$submission, ignore.case = TRUE)[[1]][[1]]
    }

    for (i in seq_along(parsed_dates)) {
      if (is.null(info$received) &&
          received != -1 && received < m[[1]][[i]]) {
        info$received <- parsed_dates[[i]]
      }
      if (is.null(info$accepted) &&
          accepted != -1 && accepted < m[[1]][[i]]) {
        info$accepted <- parsed_dates[[i]]
      }
    }

  }, error = \(e) {})

  return(info)
}

#' Get author info from TEI type XML
#'
#' @param xml The XML
#'
#' @return an author list
#' @keywords internal
tei_authors <- function(xml) {
  s <- study()
  authors <- xml2::xml_find_all(xml, "//sourceDesc //author[persName]")

  for (a in authors) {
    family <- xml_find(a, ".//surname", join = " ")
    given <- xml_find(a, ".//forename", join = " ")
    email <- xml_find(a, ".//email", join = ";")
    orcid <- xml_find(a, ".//idno[@type='ORCID']")

    if (orcid == "") orcid = NULL

    affs <- xml2::xml_find_all(a, ".//affiliation")
    affiliation <- lapply(affs, function(aff) {
      org <- xml2::xml_find_all(aff, ".//orgName")
      names <- xml2::xml_attr(org, "type")
      vals <- xml2::xml_text(org)
      stats::setNames(as.list(vals), names)
    })

    s <- add_author(s, family, given, orcid, email = email,
                    affiliation = affiliation)
  }

  return(s$authors)
}

#' Get full text from TEI type XML
#'
#' @param xml The XML
#'
#' @return a data frame with all text
#' @keywords internal
tei_full_text <- function(xml) {
  div <- NULL  # ugh cmdcheck

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

  all_tables <- c(list(abst_table),
                   div_text,
                   list(back_text,
                        figtbl,
                        notetbl))
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
    formatted <- bibs |>
      sapply(\(bib) {
        tryCatch(format(bib), error = \(e) { return("")})
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



# Text ----

#' Get article info from plain text
#'
#' @param xml The XML
#'
#' @return an info list
#' @keywords internal
text_info <- function(xml, filename = "") {

}

#' Get author info from plain text
#'
#' @param xml The XML
#'
#' @return an author list
#' @keywords internal
text_authors <- function(xml) {

}

#' Get full text from plain text
#'
#' @param xml The XML
#'
#' @return a data frame with all text
#' @keywords internal
text_full_text <- function(xml) {
  full_text <- data.frame(
    header = NA,
    text = xml_find(xml, "p"),
    div = NA,
    p = NA,
    section = NA
  )

  ## identify probably headers ----
  headers <- grep("^.{1,20}$", full_text$text)
  full_text$header[headers] <- full_text$text[headers]
  full_text$div[headers] <- seq_along(headers)
  header <- div <- NULL # fix devtools check warning
  full_text <- tidyr::fill(full_text, header, div) |>
    dplyr::group_by(div) |>
    dplyr::mutate(p = dplyr::row_number() - 1) |>
    dplyr::ungroup()

  return(full_text)
}

#' Get cross references from text file
#'
#' @param xml The XML
#'
#' @return refs table
#' @keywords internal
text_xrefs <- function(xml) {
  xrefs <- data.frame(
    xref_id = character(0),
    type = character(0),
    contents = character(0),
    text = character(0)
  )

  return(xrefs)
}

#' Get bibliography from text file
#'
#' @param xml The XML
#'
#' @return bib table
#' @keywords internal
text_bib <- function(xml) {
  bib <- data.frame()

  return(bib)
}

# Word ----

#' Get article info from Word
#'
#' @param xml The XML
#'
#' @return an info list
#' @keywords internal
word_info <- function(xml, filename = "") {

}

#' Get author info from  Word
#'
#' @param xml The XML
#'
#' @return an author list
#' @keywords internal
word_authors <- function(xml) {

}

#' Get full text from  Word
#'
#' @param filename The filename
#'
#' @return a data frame with all text
#' @keywords internal
word_full_text <- function(filename) {
  docx <- officer::read_docx(filename)
  summary <- officer::docx_summary(docx)

  full_text <- data.frame(
    header = NA,
    text = summary$text,
    div = NA,
    p = NA,
    section = NA
  )

  ## identify probably headers ----
  headers <- grepl("heading", summary$style_name) |> which()
  full_text$header[headers] <- full_text$text[headers]
  full_text$div[headers] <- seq_along(headers)
  header <- div <- NULL # fix devtools check warning
  full_text <- tidyr::fill(full_text, header, div) |>
    dplyr::group_by(div) |>
    dplyr::mutate(p = dplyr::row_number() - 1) |>
    dplyr::ungroup()

  return(full_text)
}

#' Get cross references from Word .docx
#'
#' @param xml The XML
#'
#' @return xrefs table
#' @keywords internal
word_xrefs <- function(xml) {
  xrefs <- data.frame(
    xref_id = character(0),
    type = character(0),
    contents = character(0),
    text = character(0)
  )

  return(xrefs)
}

#' Get bibliography from Word .docx
#'
#' @param xml The XML
#'
#' @return bib table
#' @keywords internal
word_bib <- function(xml) {
  bib <- data.frame()

  return(bib)
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

  b$doi <- xml2::xml_find_first(ref, ".//idno[@type='DOI']") |>
    xml2::xml_text()

  b$title <- xml2::xml_find_first(ref, ".//title[@level='a']") |>
    xml2::xml_text()

  b$author <- xml2::xml_find_all(ref, ".//author //persName") |>
    lapply(\(a) {
      forename <- xml2::xml_find_all(a, ".//forename") |> xml2::xml_text()
      surname <- xml2::xml_find_all(a, ".//surname") |> xml2::xml_text()

      utils::person(given = forename,
                    family = surname)
    }) |> do.call(base::c, args = _)

  b$editor <- xml2::xml_find_all(ref, ".//editor //persName") |>
    lapply(\(a) {
      forename <- xml2::xml_find_all(a, ".//forename") |> xml2::xml_text()
      surname <- xml2::xml_find_all(a, ".//surname") |> xml2::xml_text()

      utils::person(given = forename,
                    family = surname)
    }) |> do.call(base::c, args = _)

  b$journal <- xml2::xml_find_first(ref, ".//title[@level='j']") |>
    xml2::xml_text() |>
    gsub("\\s+", " ", x = _) |> trimws()

  b$booktitle <- xml2::xml_find_first(ref, ".//title[@level='m']") |>
    xml2::xml_text()

  # imprint
  imprint <- xml2::xml_find_first(ref, ".//imprint")
  b$publisher <-  xml2::xml_find_first(imprint, ".//publisher") |>
    xml2::xml_text()
  b$year <-  xml2::xml_find_first(imprint, ".//date[@type='published']") |>
    xml2::xml_text()
  b$volume <- xml2::xml_find_first(imprint, ".//biblScope[@unit='volume']") |>
    xml2::xml_text()
  b$number <- xml2::xml_find_first(imprint, ".//biblScope[@unit='issue']") |>
    xml2::xml_text()
  page_unit <- xml2::xml_find_first(imprint, ".//biblScope[@unit='page']")
  if (!is.na(page_unit)) {
    pages <- xml2::xml_text(page_unit)
    if (pages == "") {
      pages <- xml2::xml_attrs(page_unit)
      if (!is.na(pages[[1]])) {
        b$pages <- paste(pages[["from"]], pages[["to"]], sep = "-")
      }
    } else {
      b$pages <- pages
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
                    #warning(e$message, "\\n")
                    #return(txt)
                  })

  bib
}

get_app_info <- function(xml) {
  app <- xml2::xml_find_first(xml, "//appInfo //application")
  list(
    version = xml2::xml_attr(app, "version"),
    when = xml2::xml_attr(app, "when"),
    url = xml2::xml_find_first(app, "//ref") |> xml2::xml_attr("target")
  )
}

#' Validate Papers
#'
#' A quick function to help diagnose problems with imported papers. It checks if there is a title, doi, abstract, and a bibliography
#'
#' @param paper a paper object or a list of paper objects
#'
#' @returns a list or data frame of checks
#' @export
#'
#' @examples
#' paper_validate(psychsci[[1]])
#' paper_validate(psychsci)
paper_validate <- function(paper) {
  if (is_paper_list(paper)) {
    checks <- lapply(paper, paper_validate) |>
      lapply(dplyr::as_tibble) |>
      do.call(rbind, args = _)
    return(checks)
  }

  if (!is_paper(paper)) {
    stop("The object must be a paper or paperlist to check")
  }

  # check if doi is valid
  pattern <- "^10\\.\\d{3,9}\\/[-._;()/:A-Za-z0-9]*[A-Za-z0-9]$"

  doi <- dplyr::case_when(
    paper$info$doi == "" ~ "missing",
    !grepl(pattern, paper$info$doi, perl = TRUE) ~ "invalid",
    .default = ""
  )

  # check if title is missing
  title <- dplyr::case_when(
    paper$info$title == "" ~ "missing",
    grepl("commentary on", paper$info$title, ignore.case = TRUE) ~ "commentary",
    grepl("corrigendum", paper$info$title, ignore.case = TRUE) ~ "corrigendum",
    grepl("erratum", paper$info$title, ignore.case = TRUE) ~ "erratum",
    grepl("reply to", paper$info$title, ignore.case = TRUE) ~ "reply",
    .default = ""
  )

  # check abstract
  abstract <- dplyr::case_when(
    paper$info$description == "" ~ "missing",
    .default = ""
  )

  valid <- doi == "" &
    title != "missing" &
    abstract == "" &
    nrow(paper$bib) > 0

  list(
    id = paper$id,
    valid = valid,
    doi = doi,
    title = title,
    abstract = abstract,
    bib = nrow(paper$bib)
  )
}
