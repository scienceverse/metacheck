#' Convert a PDF to Grobid XML
#'
#' This function uses a public grobid server maintained by Patrice Lopez. You can set up your own local grobid server following instructions from <https://grobid.readthedocs.io/> and set the argument `api_url` to its path (probably <http://localhost:8070>)
#'
#' Consolidation of citations, headers, and funders looks up these items in CrossRef or another database to fix or enhance information (see <https://grobid.readthedocs.io/en/latest/Consolidation/>). This can slow down conversion. Consolidating headers is only useful for published papers, and can be set to 0 for work in prep.
#'
#' @param file_path path to the PDF, a vector of paths, or a directory name that contains PDFs
#' @param save_path directory or file path to save to; set to NULL to return the XML directly
#' @param api_url the URL to the grobid server
#' @param start_page the first page of the PDF to read (defaults to -1 to read all pages)
#' @param end_page the last page of the PDF to read (defaults to -1 to read all pages)
#' @param consolidate_citations whether to fix/enhance citations
#' @param consolidate_header whether to fix/enhance paper info
#' @param consolidate_funders whether to fix/enhance funder info
#'
#' @return XML object
#' @export
#'
convert_grobid <- function(file_path, save_path = ".",
                           api_url = "http://localhost:8070",
                           start_page = -1,
                           end_page = -1,
                           consolidate_citations = 0,
                           consolidate_header = 0,
                           consolidate_funders = 0) {
  if (!all(file.exists(file_path))) {
    stop("Files do not exist")
  }

  # test if the server is up
  .grobid_isalive(api_url)

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
        start = start_page,
        end = end_page,
        consolidate_citations = consolidate_citations,
        consolidate_header = consolidate_header,
        consolidate_funders = consolidate_funders
      )
      xml <- tryCatch(
        do.call(convert_grobid, args),
        error = function(e) {
          logger("convert_grobid", list(error = e$message))
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
    xmls <- convert_grobid(pdfs, save_path, api_url)
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
      start = as.character(start_page),
      end = as.character(end_page),
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
    unlink(save_file)
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

  pb <- pb(length(xml_file), "Converting :step [:bar] (:what) :current/:total")
  errors <- 0
  paper <- lapply(xml_file, \(xml_file1) {
    what <- basename(xml_file1)
    pb$tick(0, list(step = "", what = what))
    p <- tryCatch(
      .grobid_to_bibr(xml_file = xml_file1, pb),
      error = \(e) {
        errors <<- errors + 1
        logger("grobid_to_bibr", list(xml_path = xml_file1, error = e$message))
        return(NULL)
      })

    pb$tick(1, list(step = "", what = what))

    if (!.is_paper(p)) { # error
      if (is.null(save_path)) return(NULL)
      return(NA_character_)
    }

    if (isTRUE(crossref_lookup)) {
      p <- add_bib_match(p)
    }

    # return paper object
    if (is.null(save_path)) return(p)

    # or save paper and return file name
    # save here instead of after iteration so batches can be cancelled with partial return
    file_name <- basename(xml_file1) |> gsub("\\.xml$", "", x = _)
    json_path <- paper_write(p, file_name, save_path)
    return(json_path)
  })

  if (errors > 0) {
    e <- ifelse(errors == 1, "", paste0("1:", errors))
    warning("There ", plural(errors, "was", "were"), " ",
            errors, " error", plural(errors),
            "; use lastlog(", e, ")")
  }

  if (!is.null(save_path)) return(unlist(paper))

  if (length(paper) > 1) {
    # remove NULLS and return paper list
    paper <- Filter(Negate(is.null), paper)
    paper <- paperlist(paper)
  } else {
    paper <- paper[[1]]
  }
  return(paper)
}


#' Check grobid server status
#'
#' @param api_url the URL to the grobid server
#' @param error whether to generate and error on failure
#'
#' @returns boolean
#' @keywords internal
.grobid_isalive <- function(api_url, error = TRUE) {
  # test if the server is up using the isalive endpoint, instead of sitedown
  resp <- tryCatch(
    {
      httr2::request(api_url) |>
        httr2::req_url_path("/api/isalive") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()
    },
    error = function(e) {
      if (error) {
        stop(
          "Connection to the GROBID server failed! ",
          "Please check your connection or the URL: ", api_url,
          call. = FALSE
        )
      }
    }
  )

  if (is.null(resp)) return(FALSE)

  status <- httr2::resp_status(resp)
  if (status != 200 && error) {
    stop("GROBID server does not appear up and running on the provided URL. Status: ", status,
         call. = FALSE)
  }

  return(status == 200)
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
  header <- section_type <- NULL

  schema <- .paper_schema()
  m <- regexec("(?<=\\(v)[\\d\\.]+", schema$description, perl = TRUE)
  bibr_version = regmatches(schema$description, m)[[1]]
  what <- basename(xml_file)

  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) Converting (:what)")
    on.exit(pb$terminate())
    pb$tick(0, list(step = "", what = what))
  }

  xml_text <- readLines(xml_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # fixes a glitch that stops grobid xml from being read
    gsub(' xmlns="http://www.tei-c.org/ns/1.0"', "",
         x = _, fixed = TRUE
    ) |>
    gsub("Fig\\. (\\d{1,2})(\\s*\\.)?", "Fig \\1", x = _) |>
    gsub("Figure\\. (\\d{1,2})(\\s*\\.)?", "Figure \\1", x = _) |>
    gsub("Tab\\. (\\d{1,2})(\\s*\\.)?", "Tab \\1", x = _) |>
    gsub("Table\\. (\\d{1,2})(\\s*\\.)?", "Table \\1", x = _)

  xml <- xml2::read_xml(xml_text)

  file_hash <- substr(tools::md5sum(xml_file), 1, 16)[[1]]
  paper_id <- basename(xml_file) |> sub("\\.xml", "", x = _)
  paper <- paper(paper_id)

  # info ----
  title <- xml_find1(xml, ".//titleStmt/title")
  #abstract <- xml_find1(xml, ".//abstract")
  keywords <- xml_find(xml, ".//textClass/keywords/term")
  if (keywords[[1]] == "") keywords <- c()
  doi <- xml_find1(xml, ".//idno[@type='DOI']")
  grobid_version <- xml |>
    xml2::xml_find_first(".//application[@ident='GROBID']") |>
    xml2::xml_attr("version")
  if (is.na(grobid_version)) {
    input_format <- "Unknown TEI XML"
  } else {
    input_format <- paste("grobid", grobid_version)
  }

  paper$info <- dplyr::tibble(
    title = title %||% "",
    keywords = I(list(keywords)),
    doi = doi,
    file_hash = file_hash,
    input_format = input_format,
    file_name = xml_file,
    bibr_version = bibr_version,
    paper_type = "unknown",
    paper_type_confidence = 0,
    oecd_l1 = NA_character_,
    oecd_l2 = NA_character_,
    oecd_confidence = NA_real_
  )

  # author ----
  paper$author <- .tei_authors(xml)

  # text ----
  paper$text  <- .tei_text(xml)

  # section ----
  sec <- dplyr::count(paper$text, section_id, header, section_type)
  paper$text$header <- NULL
  paper$text$section_type <- NULL

  header <- sapply(sec$header, \(h) {
    if (is.na(h)) return(NA_character_)

    x <- paste0("<p>", h, "</p>") |> xml2::read_xml()
    head <- xml2::xml_find_first(x, "//head")

    if (length(head) == 0) return(xml2::xml_text(x))

    header <- xml2::xml_text(head)
    n <- xml2::xml_attr(head, "n")
    if (!is.na(n)) header <- paste(n, header)

    return(header)
  })

  paper$section <- dplyr::tibble(
    section_id = sec$section_id,
    header = header,
    parent_section_id = rep(NA_integer_, nrow(sec)),
    section_type = sec$section_type,
    classification_score = rep(NA_real_, nrow(sec))
  )

  # figure ----
  fig_sec <- paper$section[paper$section$section_type == "figure", ]$section_id
  paper$figure <- dplyr::tibble(
    figure_id = seq_along(fig_sec),
    section_id = fig_sec,
    image = rep(NA_character_, length(fig_sec)),
    page_number = rep(NA_integer_, length(fig_sec))
  )

  # table ----
  tab_sec <- paper$section[paper$section$section_type == "table", ]$section_id
  paper$table <- dplyr::tibble(
    table_id = seq_along(tab_sec),
    section_id = tab_sec,
    html = rep(NA_character_, length(tab_sec)),
    contents = rep(NA_character_, length(tab_sec)),
    page_number = rep(NA_integer_, length(tab_sec))
  )

  # add html
  tabs <- xml2::xml_find_all(xml, "//figure[@type='table']")
  if (length(tabs) == nrow(paper$table)) {
    for (i in seq_along(tabs)) {
      html <- xml2::xml_find_first(tabs[[i]], ".//table") |>
        paste()
      paper$table$html[[i]] <- html
    }
  }

  # bib ----
  paper$bib <- .tei_bib(xml)

  # append references to section and text and replace with text_id
  if (nrow(paper$bib) > 0) {
    section_id <- max(c(0, paper$section$section_id)) + 1
    sec_add <- dplyr::tibble(section_id = section_id,
                   header = "References",
                   section_type = "references")
    paper$section <- dplyr::bind_rows(paper$section, sec_add)
    text_ids <- max(c(0, paper$text$text_id)) + seq_along(paper$bib$bib_text)
    p_ids <- max(c(0, paper$text$paragraph_id)) + seq_along(paper$bib$bib_text)
    text_add <- dplyr::tibble(
      text_id = text_ids,
      paragraph_id = p_ids,
      section_id = section_id,
      text = paper$bib$bib_text,
      page_number = NA_integer_
    )
    paper$text <- dplyr::bind_rows(paper$text, text_add)
    paper$bib$text_id <- text_ids
  }
  paper$bib$bib_text <- NULL

  # xref ----
  paper$xref <- .tei_xrefs(text_table = paper$text)

  # url ----
  paper$url <- .tei_url(text_table = paper$text)

  # fix URLs in text that have rogue spaces
  same <- gsub("^https?://", "", paper$url$href) ==
    gsub("^https?://", "", gsub("\\s", "", paper$url$link_text))
  for (i in seq_along(same)) {
    paper$text$text <- gsub(paper$url$link_text[[i]],
         paper$url$href[[i]],
         paper$text$text, fixed = TRUE)
  }
  paper$url$link_text[same] <- NA_character_

  # eq ----
  paper$eq <- extract_eq(paper)
  paper$eq$paper_id <- NULL

  paper <- .paper_coerce(paper)

  return(paper)
}


#' Process full text table
#'
#' @param full_text a table of the full text
#'
#' @returns a data frame
#' @keywords internal
.process_full_text <- function(full_text) {
  ## tokenize sentences ----
  # TODO: get tidytext to stop breaking sentences at "S.E. ="
  text <- NULL # hack to stop cmdcheck warning :(

  if (!is.null(full_text) && nrow(full_text) > 0) {
    # stop initials and abbreviations getting parsed as sentences
    # full_text$text <- full_text$text |>
    operators <- c(
      "=", "<", ">", "~",
      "\u2248", # ~~
      "\u2260", # !=
      "\u2264", # <=
      "\u2265", # >=
      "\u226A", # <<
      "\u226B" # >>
    ) |> paste(collapse = "")
    num_pre_op <- sprintf("\\s+([\u00B20-9.]+\\s*[%s])", operators)


    full_text$formatted <- full_text$formatted |>
      # fix common mangled stats
      #gsub("\u00B2",                            "2", x = _) |>
      gsub(num_pre_op, "\\1", x = _) |>
      gsub("r\\s*p\\s*2",                        "rp\u00B2", x = _) |>
      gsub("ω\\s*p\\s*2",                        "ωp\u00B2", x = _) |>
      gsub("η\\s*p\\s*[2\u00B2]",                "ηp\u00B2", x = _) |>
      gsub("η\\s*G\\s*[2\u00B2]",                "ηG\u00B2", x = _) |>
      gsub("η\\s*[2\u00B2]",                     "η\u00B2", x = _) |>
      gsub("τ\\s*[2\u00B2]",                     "τ\u00B2", x = _) |>
      gsub("\\br\\s*[2\u00B2](\\s*[=><])",       "r\u00B2\\1", x = _) |>
      gsub("\\bR\\s*[2\u00B2]\\s+M\\b",          "R\u00B2M", x = _) |>
      gsub("\\bR\\s*[2\u00B2]\\b",               "R\u00B2", x = _) |>
      gsub("\\bI\\s*[2\u00B2]\\b",               "I\u00B2", x = _) |>
      gsub("χ\\s*[2\u00B2]",                     "χ\u00B2", x = _) |>
      gsub("\\bf\\s*[2\u00B2](\\s*[=><])",       "f\u00B2\\1", x = _) |>
      gsub("χ[2\u00B2]\\s*\\((\\s*\\d+)\\s*\\)", "χ\u00B2(\\1)", x = _) |>
      gsub("\\br\\s*\\((\\s*\\d+)\\s*\\)",       "r(\\1)", x = _) |>
      gsub("\\bd\\s+z\\b",                       "dz", x = _) |>
      gsub("\\bBF\\s+([10]{2})\\b",              "BF\\1", x = _) |> # BF 10; BF 01

      gsub("(https?://)\\s+", "\\1", x = _) |> # whitespace in url
      gsub("(\\d\\.)\\s+(\\d)", "\\1\\2", x = _) |> # #. #
      gsub("\\b[Ff]ig\\. (\\D?\\d)", "Fig \\1", x = _) |>
      gsub("\\b[Ff]igure\\. (\\d)", "Figure \\1", x = _) |>
      gsub("\\b[Tt]ab\\. (\\d)", "Tab \\1", x = _) |>
      gsub("\\b[Tt]able\\. (\\d)", "Table \\1", x = _) |>
      gsub("\\b([A-Z])\\.", "\\1$%", x = _) |> #initials (put back later)
      gsub("^<p>", "", x = _) |>
      gsub("</p>$", "", x = _) |>
      gsub("^<figDesc>", "", x = _) |>
      gsub("</figDesc>$", "", x = _) |>
      gsub("</ref><ref", "</ref> <ref", x = _, fixed = TRUE) |>
      trimws()

    ft <- full_text |>
      tidytext::unnest_sentences(formatted, formatted, to_lower = FALSE)

    ft$text <- sapply(ft$formatted, \(x) {
      tryCatch({
        x |> paste("<p>", x = _, "<p>") |>
          xml2::read_html() |>
          xml2::xml_text() |>
          trimws()
      }, error = \(e) {
        return(x)
      })
    }, USE.NAMES = FALSE)

    # return initials
    ft$formatted <- gsub("\\b([A-Z])\\$%", "\\1\\.", x = ft$formatted)
    ft$text <- gsub("\\b([A-Z])\\$%", "\\1\\.", x = ft$text)

    # merge sentence frags
    no_end <- which(!grepl("[\\.\\?\\!\\\"]$", x = ft$text))
    no_cap <- which(!grepl("^[A-Z]", ft$text))
    merge <- (no_end + 1) %in% no_cap
    # iterate backward in case of >2 in a row
    for (x in rev(no_end[merge])) {
      ft$text[[x]] <- paste(ft$text[[x]], ft$text[[x + 1]])
      ft$formatted[[x]] <- paste(ft$formatted[[x]], ft$formatted[[x + 1]])
      keep <- setdiff(seq_along(ft$text), x + 1)
      ft <- ft[keep, ]
    }

    # remove redundant formatted
    ft$formatted[ft$formatted == ft$text] <- NA_character_
  } else {
    ft <- dplyr::tibble(
      header = character(0),
      text = character(0),
      formatted = character(0),
      section = character(0),
      div = integer(0),
      p = integer(0)
    )
  }

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
  first_s <- ft$p == 1
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

  colorder <- c("text", "section", "header", "div", "p", "formatted")

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
.tei_text <- function(xml) {
  div <- text <- NULL # ugh cmdcheck

  ## abstract ----
  p <- xml2::xml_find_all(xml, ".//abstract //p")
  abst_table <- dplyr::tibble(
    header = "Abstract",
    #text = xml2::xml_text(p),
    formatted = as.character(p),
    div = 0,
    section = "abstract"
  )

  ## body ----
  divs <- xml2::xml_find_all(xml, "//text //body //div")
  div_text <- lapply(seq_along(divs), \(i){
    div <- divs[[i]]
    h <- xml2::xml_find_first(div, ".//head")
    header <- as.character(h)
    if (is.na(header)) header <- sprintf("[div-%02d]", i)
    p <- xml2::xml_find_all(div, ".//p")
    if (length(p) == 0) p <- header

    df <- dplyr::tibble(
      header = header,
      #text = xml2::xml_text(p),
      formatted = as.character(p),
      div = i,
      section = NA_character_
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
      h <- xml2::xml_find_first(div, ".//head")
      header <- as.character(h)
      paragraphs <- xml_find(div, ".//p")
      x <- xml2::xml_find_all(div, ".//p")
      df <- dplyr::tibble(
        header = header,
        #text = xml2::xml_text(x),
        p = seq_along(x),
        formatted = as.character(x),
        div = NA,
        section = t
      )
    })

    do.call(rbind, b_text)
  }) |> do.call(rbind, args = _)

  # make divs increment (this is gross code)
  if (!is.null(back_text)) {
    start <- length(div_text) + 1
    end <- sum(back_text$p == 1) + start - 1
    back_text$div[back_text$p == 1] <- start:end
    back_text <- tidyr::fill(back_text, div)
    back_text$p <- NULL
  }

  ## add figures and tables ----
  # TODO: get sentences with internal refs to figs
  figs <- xml2::xml_find_all(xml, "//figure")
  figtbl <- lapply(figs, \(fig) {
    figid <- xml2::xml_attr(fig, "id")
    formatted <- xml2::xml_find_first(fig, ".//figDesc")
    h <- xml2::xml_find_first(fig, ".//head")
    header <- as.character(h)

    dplyr::tibble(
      header = header,
      #text = xml_find1(fig, ".//figDesc"),
      formatted = as.character(formatted),
      section = sub("_\\d+$", "", x = figid),
      div = sub("^(fig|tab)_", "", x = figid) |> as.numeric()
    )
  }) |> do.call(rbind, args = _)
  figtbl <- figtbl %||% dplyr::tibble()

  ## add footnotes ----
  notes <- xml2::xml_find_all(xml, "//note[@place='foot']")
  notetbl <- lapply(notes, \(note) {
    noteid <- xml2::xml_attr(note, "id")
    dplyr::tibble(
      header = "",
      #text = xml2::xml_text(note),
      formatted = as.character(note),
      section = sub("_\\d+$", "", x = noteid),
      div = sub("^foot_", "", x = noteid) |> as.numeric()
    )
  }) |> do.call(rbind, args = _)
  notetbl <- notetbl %||% dplyr::tibble()

  all_tables <- c(
    list(abst_table),
    div_text,
    list(
      back_text,
      figtbl,
      notetbl
    )
  )
  ft <- do.call(dplyr::bind_rows, all_tables)
  ft <- ft[!is.na(ft$formatted), ]

  # re-number p and div
  ft$p <- 0
  ft$p <- seq_along(ft$p)
  figtab <- ft[ft$section %in% c("fig", "tab", "foot"), ]
  nofigtab <- ft[!ft$section %in% c("fig", "tab", "foot"), ]
  divmax <- ifelse(nrow(nofigtab), max(nofigtab$div, na.rm = TRUE), 0)
  figtab$div <- divmax + seq_along(figtab$div)
  ft <- dplyr::bind_rows(nofigtab, figtab)

  ft$section[ft$section == "tab"] <- "table"
  ft$section[ft$section == "fig"] <- "figure"

  # split sentences and get rid of headers in text column
  ft <- .process_full_text(ft)
  #ft <- ft[ft$text != ft$header, ]

  full_text <- dplyr::tibble(
    text = ft$text,
    text_id = seq_along(ft$text),
    paragraph_id = ft$p,
    section_id = ft$div,
    page_number = rep(NA_integer_, nrow(ft)),
    header = ft$header,
    section_type = ft$section,
    formatted = ft$formatted
  )

  return(full_text)
}

#' Get authors from TEI type XML
#'
#' @param xml The XML
#'
#' @return authors table
#' @keywords internal
.tei_authors <- function(xml) {
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

#' Get cross references from text table
#'
#' @param text_table The text table for the paper
#'
#' @return xrefs table
#' @keywords internal
.tei_xrefs <- function(text_table) {
  null_table <- dplyr::tibble(
    xref_id =  character(0),
    xref_type = character(0),
    contents =  character(0),
    text_id = integer(0)
  )

  if (nrow(text_table) == 0) return(null_table)

  xrefs <- mapply(\(f, text_id) {
    if (is.na(f)) return(null_table)
    ref <- paste0("<p>", f, "</p>") |>
      xml2::read_html() |>
      xml2::xml_find_all("//ref")

    if (length(ref) == 0) return(null_table)

    dplyr::tibble(
      xref_id =  sapply(ref, xml2::xml_attr, "target"),
      xref_type = sapply(ref, xml2::xml_attr, "type"),
      contents =  sapply(ref, xml2::xml_text),
      text_id = text_id
    )
  }, f = text_table$formatted, text_id = text_table$text_id, SIMPLIFY = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::filter(xref_type != "url")

  xrefs$xref_id <- gsub("\\D", "", xrefs$xref_id) |> as.integer()

  return(xrefs)
}

#' Get URLs from text table
#'
#' @param text_table The text table for the paper
#'
#' @return xrefs table
#' @keywords internal
.tei_url <- function(text_table) {
  null_table <- dplyr::tibble(
    href =  character(0),
    link_text =  character(0),
    text_id = integer(0)
  )

  if (nrow(text_table) == 0) return(null_table)

  urls <- mapply(\(f, text_id) {
    if (is.na(f)) return(null_table)
    url <- paste0("<p>", f, "</p>") |>
      xml2::read_html() |>
      xml2::xml_find_all("//ref[@type='url']")

    if (length(url) == 0) return(null_table)

    dplyr::tibble(
      href =  sapply(url, xml2::xml_attr, "target"),
      link_text =  sapply(url, xml2::xml_text),
      text_id = text_id
    )
  }, f = text_table$formatted, text_id = text_table$text_id, SIMPLIFY = FALSE) |>
    dplyr::bind_rows()

  return(urls)
}

#' Get bibliography from TEI type XML
#'
#' @param xml The XML
#'
#' @return bib table
#' @keywords internal
.tei_bib <- function(xml) {
  refs <- xml2::xml_find_all(xml, "//listBibl //biblStruct")

  if (length(refs) > 0) {
    bib_table <- lapply(refs, .xml2bib) |>
      dplyr::bind_rows()

    bib_table$bib_id <- xml2::xml_attr(refs, "id") |>
      gsub("b", "", x = _) |>
      as.integer()

    bib_table$bib_text <- xml_find(refs, ".//note[@type='raw_reference']") |>
      gsub("\\s+", " ", x = _) |>
      trimws()

    # extract first occurrence of year from string
    year <- sapply(bib_table$year, \(x) {
      m <- regexpr("\\b[12]\\d{3}[a-z]?\\b", x)
      y <- regmatches(x, m)
      ifelse(length(y), y[[1]], NA_character_)
    })

    bib_table$year <- gsub("\\D", "", year) |> as.integer()
    bib_table$year_suffix <- gsub("\\d", "", year)
  } else {
    bib_table <- dplyr::tibble(
      bib_id = character(0),
      bib_text = character(0)
    )
  }

  return(bib_table)
}




#' Parse XML bib format to bibtex
#'
#' @param ref the biblStruct xml object
#'
#' @returns a bibentry
#' @export
#' @keywords internal
.xml2bib <- function(ref) {
  b <- list(bib_type = "misc")

  b$doi <- xml_find1(ref, ".//idno[@type='DOI']")

  b$title <- xml_find1(ref, ".//title[@level='a']")

  b$authors <- xml2::xml_find_all(ref, ".//author //persName") |>
    lapply(\(a) {
      forename <- xml_find(a, ".//forename", join = " ")
      surname <- xml_find(a, ".//surname", join = " ")

      paste0(surname, ", ", forename)
    }) |> paste(collapse = "; ")

  b$editors <- xml2::xml_find_all(ref, ".//editor //persName") |>
    lapply(\(a) {
      forename <- xml_find(a, ".//forename", join = " ")
      surname <- xml_find(a, ".//surname", join = " ")

      paste0(surname, ", ", forename)
    }) |> paste(collapse = "; ")

  b$journal <- xml_find1(ref, ".//title[@level='j']")

  b$booktitle <- xml_find1(ref, ".//title[@level='m']")

  # imprint
  imprint <- xml2::xml_find_first(ref, ".//imprint")
  b$publisher <- xml_find1(imprint, ".//publisher")
  b$year <- xml_find1(imprint, ".//date[@type='published']")
  b$volume <- xml_find1(imprint, ".//biblScope[@unit='volume']")
  b$issue <- xml_find1(imprint, ".//biblScope[@unit='issue']")
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
    b$bib_type <- "article"
    b$container <- b$journal
    if (is.null(b$year)) {
      # b$bibtype <- "unpublished"
      note <- xml2::xml_find_first(ref, ".//note") |> xml2::xml_text()
      b$year <- note %||% "no year"
    }
  } else if (!is.null(b$booktitle)) {
    if (is.null(b$title)) {
      b$bib_type <- "book"
      b$title <- b$booktitle
    } else {
      b$bib_type <- "incollection"
      b$container <- b$booktitle
    }
  }

  b$booktitle <- NULL
  b$journal <- NULL
  b$pages <- NULL

  return(b)
}
