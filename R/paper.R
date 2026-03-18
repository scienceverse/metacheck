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
      corresponding = logical(0),
      orcid = character(0),
      role = I(list())
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
      text_id = integer(0),
      match = I(list())
    ),
    eq = data.frame(
      text_id = integer(0),
      grp_id = integer(0),
      lhs = character(0),
      comp = character(0),
      rhs = character(0)
    ),
    figure = data.frame(
      figure_id = integer(0),
      section_id = integer(0),
      image = character(0),
      page_number = integer(0)
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
      classification_score = double(0)
    ),
    table = data.frame(
      table_id = integer(0),
      section_id = integer(0),
      html = character(0),
      contents = I(list()),
      page_number = integer(0)
    ),
    text = data.frame(
      text_id = integer(0),
      paragraph_id = integer(0),
      section_id = integer(0),
      text = character(0),
      page_number = integer(0)
    ),
    xref = data.frame(
      xref_id = integer(0),
      xref_type = character(0),
      contents = character(0),
      text_id = integer(0)
    )#,
    # bib_matches = data.frame(
    #   bib_id = integer(0),
    #   source = character(0),
    #   source_id = character(0),
    #   match_score = numeric(0),
    #   bib_type = character(0),
    #   doi = character(0),
    #   title = character(0),
    #   authors = I(list()),
    #   editors = I(list()),
    #   publisher = character(0),
    #   publication_year = integer(0),
    #   container = character(0),
    #   volume = character(0),
    #   issue = character(0),
    #   first_page = character(0),
    #   last_page = character(0),
    #   url = character(0)
    # )
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
      print(dots)
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
#' paper_validate(paper)
#'
#' paper <- demopaper()
#' paper_validate(paper)
paper_validate <- function(paper) {
  json <- system.file("schema/paper.json", package = "metacheck")
  schema <- jsonlite::read_json(json, simplifyVector = TRUE)
  error_msg <- c()
  warning_msg <- c()

  # check for required tables
  req_tables <- schema$required
  ok_tables <- names(schema$properties)
  paper_tables <- names(paper)

  if (!all(req_tables %in% paper_tables)) {
    missing <- setdiff(req_tables, paper_tables) |>
      paste(collapse = ", ") |>
      sprintf("The following tables are missing:\n %s", x = _)
    error_msg <- c(error_msg, missing)
  }

  if (!all(paper_tables %in% ok_tables)) {
    extra <- setdiff(paper_tables, ok_tables) |>
      paste(collapse = ", ") |>
      sprintf("The paper has extra tables:\n %s", x =_)
    warning_msg <- c(warning_msg, extra)
  }

  # check required and optional columns
  tbls <- setdiff(ok_tables, c("paper_id")) |>
    intersect(names(paper))

  type_map <- list(
    "string" = "character",
    "integer" = "integer",
    "number" = "double",
    "boolean" = "logical",
    "array" = "list",
    "object" = "list"
  )

  sink <- lapply(tbls, \(tbl) {
    ref <- schema$properties[[tbl]]$`$ref` %||%
      schema$properties[[tbl]]$items$`$ref`
    def <- strsplit(ref, "/")[[1]][[3]]

    cols <- names(paper[[tbl]])
    req <- schema$`$defs`[[def]]$required
    ok <- schema$`$defs`[[def]]$properties |> names()

    # error on required cols
    if (!all(req %in% cols)) {
      missing <- setdiff(req, cols) |>
        paste(collapse = ", ") |>
        sprintf("The %s table is missing required columns:\n %s", tbl, x =_)
      error_msg <<- c(error_msg, missing)
    }

    # warn on cols not in schema
    if (!all(cols %in% ok)) {
      extra <- setdiff(cols, ok) |>
        paste(collapse = ", ") |>
        sprintf("The %s table has extra columns:\n %s", tbl, x =_)
      warning_msg <<- c(warning_msg, extra)
    }

    # check column types
    types <- schema$`$defs`[[def]]$properties |>
      sapply(\(x) x$type[[1]])

    for (col in intersect(cols, ok)) {
      schema_type <- types[[col]]
      col_type <- typeof(paper[[tbl]][[col]])

      if (!type_map[[schema_type]] %in% col_type) {
        type_mismatch <- sprintf(
          "The %s column of the %s table is a %s type, but should be a %s type",
          col, tbl, col_type, type_map[[schema_type]])
        warning_msg <<- c(warning_msg, type_mismatch)
      }
    }
  })

  if (length(warning_msg)) {
    warning(paste(warning_msg, collapse = "\n"))
  }

  if (length(error_msg)) {
    stop(paste(error_msg, collapse = "\n"))
  }

  # paper_check <- paper_coerce(paper)
  # identical(paper, paper_check)

  return(TRUE)
}


#' Coerce paper object types
#'
#' Convert columns to the correct type
#'
#' @param paper  a paper object
#'
#' @returns a paper object
#' @export
paper_coerce <- function(paper) {
  json <- system.file("schema/paper.json", package = "metacheck")
  schema <- jsonlite::read_json(json, simplifyVector = TRUE)

  type_func <- list(
    "string" = as.character,
    "integer" = as.integer,
    "number" = as.double,
    "boolean" =  as.logical
  )

  schema_tables <- names(schema$properties)
  paper_tables <- names(paper)

  schema_type <- schema$properties$paper_id$type |> setdiff("null")
  paper$paper_id <- type_func[[schema_type]](paper$paper_id)

  tbls <- intersect(paper_tables, schema_tables) |>
    setdiff(c("paper_id"))

  for (tbl in tbls) {
    ref <- schema$properties[[tbl]]$`$ref` %||%
      schema$properties[[tbl]]$items$`$ref`
    def <- strsplit(ref, "/")[[1]][[3]]
    prop <- schema$`$defs`[[def]]$properties
    cols <- intersect(names(paper[[tbl]]), names(prop))
    for (col in cols) {
      schema_type <- prop[[col]]$type[[1]]
      if (schema_type %in% names(type_func)) {
        paper[[tbl]][[col]] <- type_func[[schema_type]](paper[[tbl]][[col]])
      }
    }
  }

  return(paper)
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
