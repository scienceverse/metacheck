#' Convert a PDF to Grobid XML
#'
#' This function uses a public grobid server maintained by Patrice Lopez. You can set up your own local grobid server following instructions from <https://grobid.readthedocs.io/> and set the argument `api_url` to its path (probably <http://localhost:8070>)
#'
#' Consolidation of citations, headers, and funders looks up these items in CrossRef or another database to fix or enhance information (see <https://grobid.readthedocs.io/en/latest/Consolidation/>). This can slow down conversion. Consolidating headers is only useful for published papers, and can be set to 0 for work in prep.
#'
#' @param file_path path to the PDF, a vector of paths, or a directory name that contains PDFs
#' @param save_path directory or file path to save to; set to NULL to save to a temp file
#' @param api_url the URL to the grobid server
#' @param start the first page of the PDF to read (defaults to -1 to read all pages)
#' @param end the last page of the PDF to read (defaults to -1 to read all pages)
#' @param consolidate_citations whether to fix/enhance citations
#' @param consolidate_header whether to fix/enhance paper info
#' @param consolidate_funders whether to fix/enhance funder info
#'
#' @return XML object
#' @export
#'
grobid_convert <- function(file_path, save_path = ".",
                           api_url = "https://kermitt2-grobid.hf.space",
                           start = -1,
                           end = -1,
                           consolidate_citations = 0,
                           consolidate_header = 0,
                           consolidate_funders = 0) {
  if (!all(file.exists(file_path))) {
    stop("Files do not exist")
  }

  # check if api_url is a valid url, before connecting to it
  if (!grepl("^https?://", api_url)) {
    stop("api_url must be a valid URL, starting with http or https!")
  }

  # test if the server is up using the isalive endpoint, instead of sitedown
  resp <- tryCatch(
    {
      httr2::request(api_url) |>
        httr2::req_url_path("/api/isalive") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    },
    error = function(e) {
      stop(
        "Connection to the GROBID server failed!",
        "Please check your connection or the URL: ", api_url
      )
    }
  )

  status <- httr2::resp_status(resp)
  if (status != 200) {
    stop("GROBID server does not appear up and running on the provided URL. Status: ", status)
  }

  # handle list of files or a directory----
  if (length(file_path) > 1) {
    if (is.null(save_path)) save_path <- "."
    if (length(save_path) == 1) {
      dir.create(save_path, FALSE)
      save_path <- rep_len(save_path, length(file_path))
    }
    if (length(save_path) != length(file_path)) {
      stop("The argument save_path must be a single directory name or a vector of file names with the same length as the number of files to convert.")
    }

    # set up progress bar
    pb <- pb(
      length(file_path),
      "Processing PDFs [:bar] :current/:total :elapsedfull"
    )

    xmls <- mapply(\(pdf, sp) {
      args <- list(
        file_path = pdf,
        save_path = sp,
        api_url = api_url,
        start = start,
        end = end,
        consolidate_citations = consolidate_citations,
        consolidate_header = consolidate_header,
        consolidate_funders = consolidate_funders
      )
      xml <- tryCatch(
        do.call(grobid_convert, args),
        error = function(e) {
          logger("grobid_convert", list(error = e$message))
          return(e$message)
        }
      )
      pb$tick()
      xml
    }, pdf = file_path, sp = save_path)

    errors <- !file.exists(xmls)
    if (any(errors)) {
      warning(
        sum(errors), " of ", length(xmls), " files did not convert: \n",
        paste0(" * ", file_path[errors], ": ", xmls[errors], collapse = "\n")
      )
      xmls[errors] <- NA_character_
    }

    # summary message
    n_success <- sum(!errors)
    n_total <- length(xmls)
    message(sprintf(
      "%d out of %d PDF file%s successfully converted to Grobid TEI XML.",
      n_success, n_total, plural(n_total)
    ))

    return(invisible(xmls)) # invisible to prioritize formatted print at the end
  } else if (dir.exists(file_path)) {
    pdfs <- list.files(file_path, "\\.pdf",
                       full.names = TRUE,
                       recursive = TRUE,
                       ignore.case = TRUE
    )
    if (length(pdfs) == 0) {
      warning("There are no PDF files in the directory ", file_path)
    }
    xmls <- grobid_convert(pdfs, save_path, api_url)
    return(invisible(xmls))
  }

  if (!file.exists(file_path)) {
    stop("The file ", file_path, " does not exist.")
  }

  # grobid server
  resp <- httr2::request(api_url) |>
    httr2::req_url_path("/api/processFulltextDocument") |>
    httr2::req_body_multipart(
      input = curl::form_file(file_path),
      start = as.character(start),
      end = as.character(end),
      consolidateCitations = as.character(consolidate_citations),
      consolidateHeader = as.character(consolidate_header),
      consolidateFunders = as.character(consolidate_funders),
      includeRawCitations = "1"
    ) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  # Check if the request was successful
  if (httr2::resp_status(resp) >= 400) {
    stop(httr2::resp_status_desc(resp))
  }

  content <- httr2::resp_body_raw(resp)

  # save to save_path
  if (is.null(save_path)) {
    save_file <- tempfile(fileext = ".xml")
  } else if (dir.exists(save_path)) { # save_path is an existing dir
    base <- basename(file_path) |>
      sub("\\.pdf", "", x = _, TRUE) |>
      paste0(".xml")
    save_file <- file.path(save_path, base)
  } else { # save_path is a file name
    # make subdirs if necessary
    dir.create(dirname(save_path),
               showWarnings = FALSE,
               recursive = TRUE
    )

    save_file <- save_path |>
      sub("\\.xml", "", x = _, TRUE) |>
      paste0(".xml")
  }

  # Save the response content
  writeBin(content, save_file)

  # read in as xml
  if (is.null(save_path)) {
    xml <- read(save_file)
    return(xml)
  } else {
    save_file
  }
}


#' Convert Grobid TEI XML file to bibr format
#'
#' @param xml_file the XML file
#' @param save_path directory or file path to save to; set to NULL to return a paper object
#' @param crossref_lookup whether to look up references in crossref
#'
#' @returns a paper object
#' @export
grobid_to_bibr <- function(xml_file,
                           save_path = ".",
                           crossref_lookup = FALSE) {
  # handle directory or multiple files ----
  if (length(xml_file) == 1 && dir.exists(xml_file)) {
    dir_path <- xml_file
    xml_file <- list.files(dir_path,
                           pattern = "\\.xml$",
                           ignore.case = TRUE,
                           full.names = TRUE)
  }

  if (length(xml_file) > 1) {
    pb <- pb(length(xml_file), "Converting :step [:bar] (:what) :current/:total")
    errors <- 0
    paper <- lapply(xml_file, \(xml_file1) {
      what <- basename(xml_file1)
      pb$tick(0, list(step = "", what = what))
      p <- tryCatch(
        .grobid_to_bibr(xml_file = xml_file1, pb),
        error = \(e) {
          errors <<- errors + 1
          logger("grobid_to_bibr", e$message)
          return(NULL)
        })
      pb$tick(1, list(step = "complete", what = what))
      p
    })

    if (errors > 0) {
      warning("There ", plural(errors, "was", "were"), " ",
              errors, " error", plural(errors),
              "; use lastlog(", errors, ") to see them.")

      # remove NULLS
      paper <- Filter(Negate(is.null), paper)
    }

    paper <- paperlist(paper)
  } else {
    paper <- .grobid_to_bibr(xml_file)
  }

  if (isTRUE(crossref_lookup)) {
    paper <- add_bib_match(paper)
  }
  if (is.null(save_path)) {
    return(paper)
  } else {
    file_name <- basename(xml_file) |> gsub("\\.xml$", "", x = _)
    json_paths <- paper_write(paper, file_name, save_path)
    return(json_paths)
  }
}


#' Convert grobid to Bibr format
#'
#' @param xml_file a singhle XML file
#' @param pb a progress bar passed from `grobid_to_bibr()`
#'
#' @returns a paper object
#' @export
#' @keywords internal
.grobid_to_bibr <- function(xml_file, pb = NULL) {
  bibr_version = "9.0"
  what <- basename(xml_file)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) Converting :step (:what)")
    on.exit(pb$terminate())
    pb$tick(0, list(step = "", what = what))
  }

  pb$tick(0, list(step = "XML", what = what))
  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stops grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    )
  xml <- xml2::read_xml(xml_text)

  file_hash <- substr(tools::md5sum(xml_file), 1, 16)[[1]]
  paper <- paper(file_hash)

  # info ----
  pb$tick(0, list(step = "info", what = what))
  title <- xml_find1(xml, ".//titleStmt/title")
  abstract <- xml_find1(xml, ".//abstract")
  keywords <- xml_find(xml, ".//textClass/keywords/term")
  if (keywords[[1]] == "") keywords <- c()
  doi <- xml_find1(xml, ".//idno[@type='DOI']")

  paper$info <- data.frame(
    title = title %||% "",
    keywords = I(list(keywords)),
    doi = doi,
    file_hash = file_hash,
    input_format = "TEI XML",
    file_name = xml_file,
    bibr_version = bibr_version,
    paper_type = "unknown",
    paper_type_confidence = 0,
    oecd_l1 = NA_character_,
    oecd_l2 = NA_character_,
    oecd_confidence = NA_real_
  )

  # author ----
  pb$tick(0, list(step = "author", what = what))
  paper$author <- tei_authors(xml)

  # text ----
  pb$tick(0, list(step = "text"))
  ft <- tei_text(xml)
  ft$p <- seq_along(ft$p)
  ft <- process_full_text(ft)

  # TODO: figures and tables
  # paper$table <- ft[ft$section == "tab", ]
  # paper$fig <- ft[ft$section == "fig", ]
  ft <- ft[!ft$section %in% c("fig", "tab"), ]
  ft <- ft[ft$text != ft$header, ]

  paper$text <- data.frame(
    text_id = seq_along(ft$text),
    paragraph_id = ft$p,
    section_id = ft$div,
    text = ft$text
  )

  # section ----
  sec <- dplyr::count(ft, div, header, section)
  paper$section <- data.frame(
    section_id = sec$div,
    header = sec$header,
    parent_section_id = rep(NA_integer_, nrow(sec)),
    section_type = sec$section,
    classification_score = rep(NA_real_, nrow(sec))
  )

  # bib ----
  pb$tick(0, list(step = "bib", what = what))
  paper$bib <- tei_bib(xml)

  # append references to section and text and replace with text_id
  if (nrow(paper$bib) > 0) {
    section_id <- max(c(0, paper$section$section_id)) + 1
    sec_add <- list(section_id = section_id,
                   header = "References",
                   section_type = "references")
    paper$section <- dplyr::bind_rows(paper$section, sec_add)
    text_ids <- max(c(0, paper$text$text_id)) + seq_along(paper$bib$bib_text)
    p_ids <- max(c(0, paper$text$paragraph_id)) + seq_along(paper$bib$bib_text)
    text_add <- data.frame(
      text_id = text_ids,
      paragraph_id = p_ids,
      section_id = section_id,
      text = paper$bib$bib_text
    )
    paper$text <- dplyr::bind_rows(paper$text, text_add)
    paper$bib$text_id <- text_ids
  }
  paper$bib$bib_text <- NULL

  # xref ----
  pb$tick(0, list(step = "xref", what = what))
  paper$xref <- tei_xrefs(xml, text_table = paper$text)

  # url ----
  pb$tick(0, list(step = "url", what = what))
  links <- extract_urls(paper)
  paper$url <- data.frame(
    href = links$text,
    link_text = rep(NA_character_, nrow(links)),
    text_id = links$text_id
  )

  # eq ----
  pb$tick(0, list(step = "eq", what = what))
  paper$eq <- extract_equations(paper)
  paper$eq$paper_id <- NULL

  paper <- paper_coerce(paper)

  pb$tick(0, list(step = "complete", what = what))
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
tei_text <- function(xml) {
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

#' Get authors from TEI type XML
#'
#' @param xml The XML
#'
#' @return authors table
#' @keywords internal
tei_authors <- function(xml) {
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
      role = list(character(0))
    )
  })

  dplyr::bind_rows(authors)
}

#' Get cross references from TEI type XML
#'
#' @param xml The XML
#' @param text_table The text table for the paper
#'
#' @return xrefs table
#' @keywords internal
tei_xrefs <- function(xml, text_table) {
  text <- xref_id <- xref_type <- NULL
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
    xref_type = types,
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
      dplyr::arrange(xref_type, gsub("\\D", "", x = xref_id) |> as.integer())
  }

  # get text_id
  cols <- c("xref_id", "xref_type", "contents", "text")
  xrefs <- xref_data[, cols] |>
    unique() |>
    dplyr::left_join(text_table, by = "text") |>
    dplyr::select(xref_id, xref_type, contents, text_id, text) |>
    dplyr::mutate(xref_type = dplyr::case_match(xref_type,
                                                "bibr" ~ "bib",
                                                "figure" ~ "figure",
                                                "table" ~ "tbl"),
                  xref_id = suppressWarnings(gsub("[a-z]", "", xref_id) |>
                                               as.integer()))

  # fuzzy_match if no text id
  # no_id <- xrefs[is.na(xrefs$text_id), cols] |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(
  #     strdist = which.min(stringdist::stringdist(text, text_table$text)),
  #     text = text_table$text[strdist],
  #     text_id = text_table$text_id[strdist]
  #   ) |>
  #   dplyr::ungroup()
  xrefs$text <- NULL

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
      bib_id = xml2::xml_attr(refs, "id") |>
        gsub("b", "", x = _) |>
        as.integer(),
      bib_text = xml2::xml_text(refs) |>
        gsub("\\s+", " ", x = _) |>
        trimws()
    )

    bibs <- lapply(refs, xml2bib)

    bib_table$doi <- sapply(bibs, \(x) x$doi %||% NA_character_)
    bib_table$bib_type <- sapply(bibs, \(x) x$bibtype %||% NA_character_)
    bib_table$title <- sapply(bibs, \(x) x$title %||% NA_character_)
    bib_table$container <- sapply(bibs, \(x) x$journal %||% x$booktitle %||% NA_character_)
    bib_table$publication_year <- sapply(bibs, \(x) x$year %||% NA_integer_)
    bib_table$authors <- bibs |>
      lapply(\(x) x$author) |>
      lapply(\(x) {
        rows <- lapply(x, \(a) {
          a <- unlist(a) |> as.list()
          data.frame(
            given = a$given %||% NA_character_,
            family = a$family %||% NA_character_
          )
        })
        if (length(rows)) do.call(rbind, rows)
        else data.frame(given = character(0), family = character(0))
      })
    bib_table$issue <- sapply(bibs, \(x) x$number %||% NA_character_)
    bib_table$first_page <- sapply(bibs, \(x) x$first_page %||% NA_character_)
    bib_table$last_page <- sapply(bibs, \(x) x$last_page %||% NA_character_)
    bib_table$editors <- bibs |>
      lapply(\(x) x$editor) |>
      lapply(\(x) {
        rows <- lapply(x, \(a) {
          a <- unlist(a)
          data.frame(
            given = a[["given"]] %||% NA_character_,
            family = a[["family"]] %||% NA_character_
          )
        })
        if (length(rows)) do.call(rbind, rows)
        else data.frame(given = character(0), family = character(0))
      })
    bib_table$publisher <- sapply(bibs, \(x) x$publisher %||% NA_character_)
  } else {
    bib_table <- data.frame(
      bib_id = character(0),
      bib_text = character(0),
      bib_type = character(0),
      authors = character(0),
      publication_year = integer(0),
      doi = character(0)
    )
  }

  return(bib_table)
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

  return(text)
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
