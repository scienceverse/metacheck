#' Create a paper object
#'
#' Create a new paper object or load a paper from PDF or XML
#'
#' @param name The name of the study or a file path to a PDF or grobid XML
#' @param ... further arguments to add
#'
#' @return An object with class scivrs_paper
#' @export
#' @keywords internal
paper <- function(id = NULL, ...) {
  if (is.null(id)) {
    # make a random hash from the time
    id <- Sys.time() |>
      format("%s%OS6") |>
      charToRaw() |>
      tools::md5sum(bytes = _) |>
      substr(1, 14)
  }

  paper <- list(
    paper_id = id,
    info = data.frame(
      title = character(0),
      abstract = character(0),
      keywords = I(list()),
      doi = character(0),
      file_hash = character(0),
      input_format = character(0),
      file_name = character(0),
      bibr_version = character(0),
      paper_type = character(0),
      paper_type_confidence = numeric(0),
      oecd_l1 = character(0),
      oecd_l2 = character(0),
      oecd_confidence = numeric(0)
    ),
    author = data.frame(
      author_id = integer(0),
      given = character(0),
      family = character(0),
      affiliation = character(0),
      email = character(0),
      corresponding = character(0),
      orcid = character(0),
      role = character(0)
    ),
    bib = data.frame(
      bib_id = integer(0),
      bib_type = character(0),
      doi = character(0),
      title = character(0),
      authors = I(list()),
      editors = I(list()),
      publisher = character(0),
      publication_year = integer(0),
      publication_date = character(0),
      container = character(0),
      volume = character(0),
      issue = character(0),
      first_page = character(0),
      last_page = character(0),
      edition = character(0),
      version = character(0),
      url = character(0),
      text_id = integer(0)
    ),
    eq = data.frame(
      text_id = integer(0),
      grp_id = integer(0),
      lhs = character(0),
      comp = character(0),
      rhs = character(0),
      study_id = integer(0)
    ),
    fig = data.frame(
      fig_id = integer(0),
      section_id = integer(0),
      image = character(0),
      page_number = integer(0),
      study_id = integer(0)
    ),
    url = data.frame(
      href = character(0),
      link_text = character(0),
      text_id = integer(0)
    ),
    section = data.frame(
      section_id = integer(0),
      header = character(0),
      parent_section_id = integer(0),
      section_type = character(0),
      classification_score = double(0),
      study_id = integer(0)
    ),
    study = data.frame(
      study_id = integer(0),
      label = character(0),
      root_section_id = integer(0)
    ),
    table = data.frame(
      table_id = integer(0),
      section_id = integer(0),
      html = character(0),
      contents = I(list()),
      page_number = integer(0),
      study_id = integer(0)
    ),
    text = data.frame(
      text_id = integer(0),
      paragraph_id = integer(0),
      section_id = integer(0),
      text = character(0),
      page_number = integer(0),
      study_id = integer(0)
    ),
    xref = data.frame(
      xref_id = integer(0),
      xref_type = character(0),
      contents = character(0),
      text_id = integer(0),
      study_id = integer(0)
    ),
    bib_matches = data.frame(
      bib_id = integer(0),
      source = character(0),
      source_id = character(0),
      match_score = numeric(0),
      bib_type = character(0),
      doi = character(0),
      title = character(0),
      authors = I(list()),
      editors = I(list()),
      publisher = character(0),
      publication_year = integer(0),
      container = character(0),
      volume = character(0),
      issue = character(0),
      first_page = character(0),
      last_page = character(0),
      url = character(0)
    )
  )

  class(paper) <- c("scivrs_paper", "list")

  invisible(paper)
}

#' Create a paperlist object
#'
#' Create a new paperlist object from individual paper objects or lists of paper objects
#'
#' @param ... scivrs_paper objects or lists of paper objects
#' @param merge_duplicates if duplicates exist, merge them
#'
#' @return An object with class scivrs_paperlist
#' @export
#' @keywords internal
#' @examples
#'
#' p1 <- psychsci[[1]]
#' p2 <- psychsci[[2]]
#' plist <- paperlist(p1, p2)
#'
#' merged <- paperlist(psychsci[1:2], psychsci[2:3],
#'.                    merge_duplicates = TRUE)
paperlist <- function(..., merge_duplicates = FALSE) {
  dots <- list(...)

  if (is_paper_list(dots)) {
    paperlist <- dots
  } else {
    is_paper <- sapply(dots, inherits, "scivrs_paper")
    dots[is_paper] <- lapply(dots[is_paper], list)
    is_paperlist <- sapply(dots, is_paper_list)
    if (all(is_paperlist)) {
      paperlist <- do.call(c, dots)
    } else {
      stop("The arguments must be paper objects or lists of paper objects")
    }
  }

  # update names from id
  names(paperlist) <- sapply(paperlist, \(x) x$paper_id)

  if (merge_duplicates) {
    # check for duplicate IDs
    dupes <- names(paperlist) |>
      duplicated() |>
      which()
    for (d in rev(dupes)) {
      dupe <- paperlist[names(paperlist) == names(paperlist)[d]]
      if (identical(unname(dupe[-length(dupe)]), unname(dupe[-1]))) {
        paperlist[[d]] <- NULL
      }
    }
  }

  class(paperlist) <- c("scivrs_paperlist", "list")

  invisible(paperlist)
}

#' Test paper
#'
#' Create a paper object with the specified text (mainly for testing/demos).
#'
#' @param text a vector of text to add
#'
#' @returns a paper object
#' @export
#'
#' @examples
#' # to test a paper with a specific URL
#' p <- test_paper("https://osf.io/abcde")
test_paper <- function(text = LETTERS) {
  p <- paper()

  p$text <- data.frame(
    text_id = seq_along(text),
    section_id = 0,
    paragraph_id = 0,
    text = as.character(text)
  )

  p$section <- data.frame(
    section_id = 0,
    header = "Test",
    parent_section_id = NA,
    section_type = "unknown",
    classification_score = 0
  )

  p$info <- data.frame(
    title = "Test Paper",
    file_hash = p$paper_id,
    input_format = "test"
  )

  return(p)
}

#' Validate a Paper Object
#'
#' Checks if a paper object conforms to the JSON schema.
#'
#' @param paper a paper object
#'
#' @returns TRUE or error
#' @export
#'
#' @examples
#' paper <- list(paper_id = "Not a paper object")
#' validate_paper(paper)
#'
#' paper <- demopaper()
#' validate_paper(paper)
validate_paper <- function(paper) {
  json <- system.file("schema/paper.json", package = "metacheck")
  schema <- jsonlite::read_json(json)
  error <- FALSE
  warning <- FALSE
  error_msg <- c()
  warning_msg <- c()

  # check for required tables
  paper_tables <- unlist(schema$required)
  if (!all(paper_tables %in% names(paper))) {
    missing <- setdiff(paper_tables, names(paper)) |>
      paste(collapse = ", ") |>
      paste("The following tables are missing:\n", x = _)
    error_msg <- c(error_msg, missing)
    error <- TRUE
  }

  # check required and optional columns for non-bib tables
  tbls <- c("info", "author", "text", "url", "section",
            "xref", "table", "fig", "eq")
  defs <- c("info", "author", "sentence", "link", "section",
            "xref", "table", "figure", "equation")

  sink <- mapply(\(tbl, def) {
    cols <- names(paper[[tbl]])
    ok <- schema$`$defs`[[def]]$properties |> names()
    req <- schema$`$defs`[[def]]$required |> unlist()

    if (!all(req %in% cols)) {
      missing <- setdiff(req, cols) |>
        paste(collapse = ", ") |>
        sprintf("The %s table is missing required columns:\n %s", tbl, x =_)
      error_msg <<- c(error_msg, missing)
      error <<- TRUE
    }

    if (!all(cols %in% ok)) {
      extra <- setdiff(cols, ok) |>
        paste(collapse = ", ") |>
        sprintf("The %s table has extra columns:\n %s", tbl, x =_)
      warning_msg <<- c(warning_msg, extra)
      warning <<- TRUE
    }
  }, tbl = tbls, def = defs)

  # bib table is a little more complex
  cols <- names(paper$bib)
  ok <- c(
    schema$`$defs`$biblio$properties |> names(),
    schema$`$defs`$biblio_ref$allOf[[2]]$properties |> names()
  )
  req <- c(
    schema$`$defs`$biblio$required |> unlist(),
    schema$`$defs`$biblio_ref$allOf[[2]]$required |> unlist()
  )

  if (!all(req %in% cols)) {
    missing <- setdiff(req, cols) |>
      paste(collapse = ", ") |>
      sprintf("The bib table is missing required columns:\n %s", x =_)
    error_msg <- c(error_msg, missing)
    error <- TRUE
  }

  if (!all(cols %in% ok)) {
    extra <- setdiff(cols, ok) |>
      paste(collapse = ", ") |>
      sprintf("The bib table has extra columns:\n %s", x =_)
    warning_msg <- c(warning_msg, extra)
    warning <- TRUE
  }

  if (warning) {
    warning(paste(warning_msg, collapse = "\n"))
  }

  if (error) {
    stop(paste(error_msg, collapse = "\n"))
  }

  return(TRUE)
}

#' Detect a paper object
#'
#' @param paper the object to test
#'
#' @returns logical
#' @export
#' @keywords internal
is_paper <- function(paper) {
  if (!is.list(paper)) {
    return(FALSE)
  }
  is_paper <- inherits(paper, "scivrs_paper")

  return(is_paper)
}

#' Detect a list of paper objects
#'
#' @param paper the object to test
#'
#' @returns logical
#' @export
#' @keywords internal
is_paper_list <- function(paper) {
  if (!is.list(paper)) {
    return(FALSE)
  }

  is_paper <- sapply(paper, inherits, what = "scivrs_paper")
  if (all(is_paper)) {
    return(TRUE)
  }

  return(FALSE)
}


#' Print Paper Object
#'
#' @param x The scivrs_paper list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.scivrs_paper <- function(x, ...) {
  underline <- rep("-", nchar(x$paper_id)) |> paste(collapse = "")
  txt <- sprintf(
    "%s\n%s\n%s\n\n%s\n\n* Sections: %d\n* Sentences: %d\n* Bibliography: %d\n* X-Refs: %d\n\n",
    underline, x$paper_id, underline,
    x$info$title %||% "{No title}",
    nrow(x$section),
    nrow(x$text),
    nrow(x$bib),
    nrow(x$xref)
  )

  cat(txt)
}

#' Print PaperList Object
#'
#' @param x The scivrs_paperlist object
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
print.scivrs_paperlist <- function(x, ...) {
  txt <- paper_table(x, "info", c("title", "doi"))

  print(txt)
}

#' Subset PaperList Object
#'
#' @param x The scivrs_paperlist object
#' @param ... Additional parameters for print
#' @param drop relevant for matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples).
#'
#' @export
#' @keywords internal
`[.scivrs_paperlist` <- function(x, ..., drop = TRUE) {
  paperlist(NextMethod())
}


#' Get demo paper
#'
#' @return paper object
#' @export
#'
#' @examples
#' paper <- demopaper()
demopaper <- function() {
  file_path <- system.file("demo/to_err_is_human.json",
                           package = "metacheck")

  read_bibr(file_path)
}

#' Paper tables
#'
#' Return a table from a paper object or concatenate tables across a list of paper objects.
#'
#' @param paper a paper or paperlist
#' @param table a table name
#' @param cols the columns to return from the table (default all columns)
#'
#' @return a merged table
#' @export
#'
#' @examples
#' biblio <- paper_table(psychsci[1:10], "bib")
#' xrefs <- paper_table(psychsci[1:10], "xref")
paper_table <- function(paper, table, cols = NULL) {
  if (!is_paper_list(paper)) paper <- list(paper)

  # add paper_id to tables
  table_list <- lapply(paper, `[[`, table)
  for (i in seq_along(paper)) {
    x <- table_list[[i]]
    if (is.data.frame(x)) {
      table_list[[i]]$paper_id <- rep(paper[[i]]$paper_id, nrow(x))
    }
  }

  merged_table <- dplyr::bind_rows(table_list)
  if (!is.null(cols)) {
    cols <- c(cols, "paper_id")
    keep <- intersect(cols, names(merged_table))
    merged_table <- merged_table[, keep, drop = FALSE]
  }

  merged_table
}


#' Write paper
#'
#' Save a paper as a JSON file.
#'
#' @param paper a paper object
#' @param file_name the name of the file (if NULL, defaults to the paper_id)
#' @param save_path the directory to save the JSON file in
#'
#' @returns the path to the JSON file
#' @export
#'
#' @examples
#' dontrun{
#' paper <- demopaper()
#' paper$info$title <- "New title"
#' paper_write(paper, "new_paper")
#' }
paper_write <- function(paper, file_name = NULL, save_path = ".") {
  save_path <- normalizePath(save_path)
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

  if (is_paper_list(paper)) {
    if (is.null(file_name)) file_name <- names(paper)
    pb <- pb(length(paper), ":what [:bar] :current/:total")
    pb$tick(0, list(what = "Saving..."))
    json_paths <- mapply(\(p, f, s) {
      jp <- paper_write(p, f, s)
      pb$tick(1, list(what = f))
      jp
    }, paper, file_name, save_path)

    return(json_paths)
  }

  if (is.null(file_name)) file_name <- paper$paper_id
  file_name <- gsub("\\.(json|zip)$", "", x = file_name)
  json_path <- file.path(save_path, paste0(file_name, ".json"))

  jsonlite::write_json(paper, json_path, auto_unbox = TRUE, pretty = TRUE)

  return(json_path)
}
