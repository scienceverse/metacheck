#' Get paper from cermine XML file
#'
#' Convert PDFs to cermine XML at <http://cermine.ceon.pl>
#'
#' @param filename the path to the XML file, a vector of file paths, or the path to a directory containing XML files
#'
#' @return A paper object with class scivrs_paper, or a list of paper objects
#' @export
#'
#' @examples
#' # paper <- read_cermine(filename)
read_cermine <- function(filename) {
  # handle list of files or a directory----
  if (length(filename) > 1) {
    # set up progress bar ----
    if (verbose()) {
      pb <- progress::progress_bar$new(
        total = length(filename), clear = FALSE,
        format = "Processing XMLs [:bar] :current/:total :elapsedfull"
      )
      pb$tick(0)
      Sys.sleep(0.2)
      pb$tick(0)
    }

    # get unique names
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
      apply(1, paste0, collapse = "/") |>
      gsub("\\.xml$", "", x = _)

    p <- lapply(filename, \(x) {
      p1 <- read_cermine(x)
      if (verbose()) pb$tick()
      p1
    })

    names(p) <- unique_names
    for (un in unique_names) {
      if (!is.null(p[[un]]) && nrow(p[[un]]$full_text) > 0) {
        p[[un]]$full_text$id <- un
      }
    }

    # remove NULLs
    valid <- !sapply(p, is.null)
    return(p[valid])
  } else if (dir.exists(filename)) {
    xmls <- list.files(filename, "\\.xml",
                       full.names = TRUE,
                       recursive = TRUE)
    if (length(xmls) == 0) {
      stop("There are no xml files in the directory ", filename)
    }
    p <- read_cermine(xmls)
    return(p)
  }

  # add .xml if not there
  filename <- gsub("(\\.xml)?$", "\\.xml", filename)

  if (!file.exists(filename)) {
    stop("The file ", filename, " does not exist.")
  }

  # read xml ----
  xml <- tryCatch(read_cermine_xml(filename),
                  error = function(e) {
                    warning("The file ", filename, " was not valid cermine XML", call. = FALSE)
                    return(FALSE)
                  })

  # return nothing if the file can't be read, so iteration doesn't fail
  if (isFALSE(xml)) return(NULL)

  # set up paper object ----
  p <- paper()

  p$id <- basename(filename) |>
    gsub("\\.(xml|pdf)$", "", x = _, ignore.case = TRUE)
  p$info$filename <- filename
  p$info$title <- xml2::xml_find_first(xml, "//front //article-title") |>
    xml2::xml_text() |>
    trimws()
  p$info$description <-  xml2::xml_find_all(xml, "//abstract //p") |>
    xml2::xml_text() |>
    paste(collapse = "\n\n")

  # keywords ----
  p$info$keywords <- xml2::xml_find_all(xml, "//kwd-group //kwd") |>
    xml2::xml_text() |>
    gsub("^eol>", "", x = _)

  # get authors ----
  p$authors <- get_cermine_authors(xml)

  # full text----
  p$full_text <- get_cermine_full_text(xml, id = basename(filename))

  # references ----
  refs <- get_cermine_refs(xml)
  p$references <- refs$references
  p$citations <- refs$citations
  #
  # # DOI ----
  # doi <- get_doi(xml)
  # p$info$doi <- doi
  #
  # # submission ----
  # submission <- get_submission(xml)
  # p$info$submission <- submission

  return(p)
}



#' Read in cermine XML
#'
#' @param filename The path to the XML file to be read
#'
#' @return An XML object
#' @keywords internal
read_cermine_xml <- function(filename) {
  xml_text <- filename |>
    readLines(warn = FALSE) |>
    paste(collapse = "\n") |>
    # remove non-breaking spaces
    gsub(" \u00A0", " ", x = _) |>
    gsub("\u00A0", " ", x = _) |>
    gsub("<ref type=\"url\" target=\"([^\"]+)\">([^<]+)</ref>",
         "{{\\1}}", x = _)

  xml <- tryCatch(xml2::read_xml(xml_text), error = function(e) {
    stop("The file ", filename, " could not be read as XML")
  })

  if (xml2::xml_name(xml) != "article") {
    stop("This XML file does not parse as a valid Cermine XML")
  }

  return(xml)
}


#' Get author info from Cermine XML
#'
#' @param xml The cermine XML
#'
#' @return an author list
#' @keywords internal
get_cermine_authors <- function(xml) {
  s <- study()
  authors <- xml2::xml_find_all(xml, "//contrib-group //contrib[@contrib-type='author']")

  for (a in authors) {
    name <- xml2::xml_find_all(a, ".//string-name") |> xml2::xml_text()
    family <- sub("^.* ", "", name) |> trimws()
    given <- sub(paste0(family, "$"), "", name) |> trimws()
    email <- xml2::xml_find_all(a, ".//email") |> xml2::xml_text()
    orcid <- xml2::xml_find_all(a, ".//idno[@type='ORCID']") |> xml2::xml_text()
    # if (is.null(orcid) & !is.null(family)) {
    #   orcid_lookup <- get_orcid(family, given)
    #   if (length(orcid_lookup) == 1) orcid <- orcid_lookup
    # }
    if (length(orcid) == 0) orcid = NULL

    affs <- xml2::xml_find_all(a, ".//xref[@ref-type='aff']")
    affiliation <- lapply(affs, function(aff) {
      rid <- xml2::xml_attr(aff, "rid") |>
        sprintf(".//aff[@id='%s']", x = _)
      aff_info <- xml2::xml_find_all(xml, rid)
      org <- xml2::xml_find_first(aff_info, ".//institution") |>
        xml2::xml_text()
      org
    })

    s <- add_author(s, family, given, orcid, email = email,
                    affiliation = affiliation)
  }

  return(s$authors)
}

#' Add section info to full text table
#'
#' @param xml The cermine XML
#' @param id An ID for the paper
#'
#' @return a data frame of the classified full text
#' @keywords internal
#'
get_cermine_full_text<- function(xml, id = "") {
  div <- NULL  # ugh cmdcheck

  ## abstract ----
  abstract <- xml2::xml_find_all(xml, "//abstract //p") |>
    xml2::xml_text()

  if (length(abstract) > 0) {
    abst_table <- data.frame(
      header = "Abstract",
      text = abstract,
      div = 0,
      p = seq_along(abstract)
    )
  } else {
    abst_table <- data.frame()
  }

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

  ## add figures and tables ----
  # TODO: get sentences with internal refs to figs
  # figs <- xml2::xml_find_all(xml, "//figure")
  # figtbl <- data.frame()
  # if (length(figs) > 0) {
  #   figids <- xml2::xml_attr(figs, "id")
  #   figtbl <- data.frame(
  #     header = xml2::xml_find_all(figs, ".//head") |>
  #       xml2::xml_text(),
  #     text = xml2::xml_find_all(figs, ".//figDesc") |>
  #       xml2::xml_text(),
  #     section = sub("_\\d+$", "", x = figids),
  #     div = sub("^(fig|tab)_", "", x = figids) |> as.numeric()
  #   )
  # }

  ## add footnotes ----
  # TODO: find and example to finish and test this
  # notes <- xml2::xml_find_all(xml, "//note[@place='foot']")
  # notetbl <- data.frame()
  # if (length(notes) > 0) {
  #   noteids <- xml2::xml_attr(notes, "id")
  #   notetbl <- data.frame(
  #     header = "",
  #     text = xml2::xml_find_all(notes, ".//p") |> xml2::xml_text(),
  #     section = sub("_\\d+$", "", x = noteids),
  #     div = sub("^foot_", "", x = noteids) |> as.numeric()
  #   )
  # }

  ## tokenize sentences ----
  # TODO: get tidytext to stop breaking sentences at "S.E. ="
  text <- NULL # hack to stop cmdcheck warning :(
  alltext <- do.call(dplyr::bind_rows, c(list(abst_table),
                                         div_text#,
                                         # list(back_text,
                                         #      figtbl,
                                         #      notetbl)
                                         ))
  if (nrow(alltext) > 0) {
    ft <- alltext |>
      # stop initials getting parsed as sentences
      dplyr::mutate(text = gsub("\\b([A-Z])\\.", "\\1", text)) |>
      tidytext::unnest_sentences(text, text, to_lower = FALSE) |>
      dplyr::mutate(s = dplyr::row_number(), .by = c("div", "p"))
    ft$id <- id
  } else {
    ft <- data.frame(
      header = character(0),
      text = character(0),
      div = double(0),
      p = double(0),
      s = double(0),
      id = character(0)
    )
  }

  # convert link notation to <url>
  ft$text <- ft$text |>
    gsub("\\{\\{", "<", x = _) |>
    gsub("\\}\\}", ">", x = _)

  # classify headers ----
  back <- !is.na(ft$section)
  back_sections <- ft$section[back]
  abstract <- grepl("abstract", ft$header, ignore.case = TRUE)
  intro <- grepl("intro", ft$header, ignore.case = TRUE)
  method <- grepl("method", ft$header, ignore.case = TRUE)
  results <- grepl("result", ft$header, ignore.case = TRUE)
  discussion <- grepl("discuss", ft$header, ignore.case = TRUE)
  ft$section <- rep(NA_character_, nrow(ft))
  ft$section[abstract] <- "abstract"
  ft$section[intro] <- "intro"
  ft$section[method] <- "method"
  ft$section[discussion] <- "discussion"
  ft$section[results] <- "results"
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

  colorder <- c("text", "section", "header", "div", "p", "s", "id")

  blank_divs <- grepl("\\[div-\\d+\\]", ft$text)
  #blank_divs <- ft$p == 0

  body_table <- ft[!blank_divs, colorder]
  rownames(body_table) <- NULL

  return(body_table)
}


#' Get references from cermine XML
#'
#' @param xml The cermine XML
#'
#' @return a list with a data frame of references and a data frame of citation sentences
#' @keywords internal
get_cermine_refs <- function(xml) {
  refs <- xml2::xml_find_all(xml, "//back //ref-list //ref")

  if (length(refs) > 0) {
    ref_table <- data.frame(
      bib_id = xml2::xml_attr(refs, "id")
    )

    # TODO: DOIs are very badly parsed in cermine
    reftext <- xml2::xml_text(refs)
    m <- gregexec("(?<=doi.org/)[^\\n]+", reftext, perl = TRUE)
    ref_table$doi <- regmatches(reftext, m) |> sapply(\(x) {
      y = as.vector(x) |> gsub("\\s", "", x = _)
      if (length(y)) y else NA_character_
    })


    ref_table$ref <- lapply(refs, cermine2bib) |>
      sapply(format) |>
      gsub("\\n", " ", x = _)

  } else {
    ref_table <- data.frame(
      bib_id = character(0),
      doi = character(0),
      ref = character(0)
    )
  }

  # get in-text citation ----
  #textrefs <- xml2::xml_find_all(xml, "//body //ref[@type='bibr']")
  textrefs <- xml2::xml_find_all(xml, "//xref[@ref-type='bibr']")

  if (length(textrefs) > 0) {
    # get parent paragraphs of all in-text references and parse into sentences
    textrefp <- data.frame(
      p = xml2::xml_parent(textrefs) |> as.character() |>
        gsub("</?p>", "", x = _)
    ) |>
      tidytext::unnest_sentences(output = "text", input = "p", to_lower = FALSE)

    # find refs
    matches <- gregexpr("(?<=xref ref-type=\"bibr\" rid=\")[ref0-9]+(?=\")",
                        textrefp$text, perl = TRUE) |>
      regmatches(textrefp$text, m = _)
    textrefp$bib_id <- sapply(matches, paste, collapse = ";")

    citation_table <- textrefp[textrefp$bib_id != "", ]
    citation_table$text <- lapply(citation_table$text, xml2::read_html) |>
      sapply(xml2::xml_text) |>
      gsub("\\s+", " ", x = _) |>
      gsub("\\) (?=[,.;])", "\\)", x = _, perl = TRUE)

    citation_table <- citation_table |>
      tidyr::separate_longer_delim("bib_id", delim = ";")
  } else {
    citation_table = data.frame(bib_id = character(0),
                                text = character(0))
  }

  return(list(
    references = ref_table,
    citations = citation_table[, c("bib_id", "text")]
  ))
}


#' Parse cermine XML bib format to bibtex
#'
#' @param ref the biblStruct xml object
#'
#' @returns a bibentry
#' @export
#' @keywords internal
cermine2bib <- function(ref) {
  b <- list(bibtype = "misc")

  # b$doi <- xml2::xml_find_first(ref, ".//idno[@type='DOI']") |>
  #   xml2::xml_text()

  b$title <- xml2::xml_find_first(ref, ".//article-title") |>
    xml2::xml_text()

  b$author <- xml2::xml_find_all(ref, ".//string-name") |>
    lapply(\(a) {
      forename <- xml2::xml_find_all(a, ".//given-names") |> xml2::xml_text()
      surname <- xml2::xml_find_all(a, ".//surname") |> xml2::xml_text()

      utils::person(given = forename,
                    family = surname)
    }) |> do.call(c, args = _)

  b$editor <- xml2::xml_find_all(ref, ".//editor //persName") |>
    lapply(\(a) {
      forename <- xml2::xml_find_all(a, ".//forename") |> xml2::xml_text()
      surname <- xml2::xml_find_all(a, ".//surname") |> xml2::xml_text()

      utils::person(given = forename,
                    family = surname)
    }) |> do.call(c, args = _)

  b$journal <- xml2::xml_find_first(ref, ".//source") |>
    xml2::xml_text() |>
    gsub("\\s+", " ", x = _) |> trimws()

  # b$booktitle <- xml2::xml_find_first(ref, ".//title[@level='m']") |>
  #   xml2::xml_text()

  # imprint
  # b$publisher <-  xml2::xml_find_first(".//publisher") |>
  #   xml2::xml_text()
  b$year <-  xml2::xml_find_first(ref, ".//year") |>
    xml2::xml_text()
  b$volume <- xml2::xml_find_first(ref, ".//volume") |>
    xml2::xml_text()
  b$number <- xml2::xml_find_first(ref, ".//issue") |>
    xml2::xml_text()
  pages <- c(
    xml2::xml_find_first(ref, ".//fpage") |> xml2::xml_text(),
    xml2::xml_find_first(ref, ".//lpage") |> xml2::xml_text()
  )
  b$pages <- pages[!is.na(pages)] |> paste(collapse = "-")

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

                    # pull visible text on error
                    # txt <- xml2::xml_text(ref) |>
                    #   gsub("\\s+", " ", x = _) |>
                    #   trimws()

                    # TODO: fix more types
                    #warning(e$message, "\\n")
                    #return(txt)
                  })

  bib
}
