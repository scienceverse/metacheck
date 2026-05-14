#' Get paper schema
#'
#' Read in the JSON schema for bibr formatted paper objects
#'
#' @returns The schema as a list
#' @export
#'
#' @examples
#' schema <- paper_schema()
#' schema$`$defs`$info$required
paper_schema <- function() {
  json <- system.file("schema/paper.json", package = "metacheck")
  schema <- jsonlite::read_json(json, simplifyVector = TRUE)

  return(schema)
}

#' Create a paper object
#'
#' Create a new paper object or load a paper from PDF or XML
#'
#' @param id The ID of the study
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
    paper_id = id
  )
  class(paper) <- c("scivrs_paper", "list")

  schema <- paper_schema()

  for (tbl in setdiff(schema$required, "paper_id")) {
    ref <- schema$properties[[tbl]]$`$ref` %||%
      schema$properties[[tbl]]$items$`$ref`
    def <- strsplit(ref, "/")[[1]][[3]]
    cols <- schema$`$defs`[[def]]$properties |> names()

    paper[[tbl]] <- data.frame()
    for (col in cols) {
      paper[[tbl]][[col]] <- character(0)
    }
  }

  paper <- paper_coerce(paper)

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
#'                     merge_duplicates = TRUE)
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
#' @param url a vector of URLs to add
#'
#' @returns a paper object
#' @export
#'
#' @examples
#' # to test a paper with a specific URL
#' p <- test_paper("https://osf.io/abcde")
test_paper <- function(text = LETTERS, url = character(0)) {
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
    parent_section_id = NA_integer_,
    section_type = "unknown",
    classification_score = 0
  )

  p$info <- data.frame(
    title = "Test Paper",
    file_hash = p$paper_id,
    input_format = "test"
  )

  p$url <- data.frame(
    href = url,
    link_text = rep(NA_character_, length(url)),
    text_id = seq_along(url)
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
#' tryCatch(
#'   paper_validate(paper),
#'   error = \(e) print(e$message)
#' )
#'
#' paper <- demopaper()
#' paper_validate(paper)
paper_validate <- function(paper) {
  schema <- paper_schema()
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
    type_map <- list(
      "string" = "character",
      "integer" = "integer",
      "number" = "double",
      "boolean" = "logical",
      "array" = "list",
      "object" = "list"
    )

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
  if (is_paper_list(paper)) {
    papers <- lapply(paper, paper_coerce) |>
      paperlist()
    return(papers)
  }

  schema <- paper_schema()

  type_func <- list(
    "string" = as.character,
    "integer" = as.integer,
    "number" = as.double,
    "boolean" =  as.logical,
    "object" = as.data.frame,
    "array" = as.list
  )

  schema_tables <- names(schema$properties)
  paper_tables <- names(paper)

  schema_type <- schema$properties$paper_id$type |> setdiff("null")
  paper$paper_id <- type_func[[schema_type]](paper$paper_id)

  tbls <- intersect(paper_tables, schema_tables) |>
    setdiff(c("paper_id"))

  # count logged errors/warnings
  logs <- 0

  for (tbl in tbls) {
    ref <- schema$properties[[tbl]]$`$ref` %||%
      schema$properties[[tbl]]$items$`$ref`
    def <- strsplit(ref, "/")[[1]][[3]]
    prop <- schema$`$defs`[[def]]$properties
    cols <- intersect(names(paper[[tbl]]), names(prop))
    for (col in cols) {
      schema_type <- prop[[col]]$type[[1]]
      if (schema_type %in% names(type_func)) {
        paper[[tbl]][[col]] <- tryCatch(
          type_func[[schema_type]](paper[[tbl]][[col]]),
          error = \(e) {
            logger(label = "paper_coerce",
                   list(table = tbl,
                        column = col,
                        error = e$message)
            )
            stop(e)
          },
          warning = \(w) {
            orig <- paper[[tbl]][[col]]
            x <- suppressWarnings(
              type_func[[schema_type]](orig)
            )
            convert_problems <- suppressWarnings(
              which(is.na(orig == x) & !is.na(orig))
            )

            logs <<- logs + 1
            example <- orig[[convert_problems[1]]]
            if (length(example) == 0) example <- ""

            logger(label = "paper_coerce",
                   list(paper_id = paper$paper_id,
                        table = tbl,
                        column = col,
                        rows = paste(convert_problems, collapse = ", "),
                        example = example,
                        warning = w$message)
            )

            # convert and return anyways
            return(x)
          }
        )
      }
    }
  }

  if (logs > 0) {
    warning("There ", plural(logs, "was", "were"),
            " ", logs, " warning", plural(logs),
            "; check lastlog(1:", logs, ")")
  }

  return(paper)
}

#' Detect a paper object
#'
#' Lightweight check if an object is a paper vs paperlist. Use `paper_validate()` for a thorough check.
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
  file_path <- system.file("demos/to_err_is_human.json",
                           package = "metacheck")

  read_bibr(file_path)
}

#' Get a demo file
#'
#' Return the file path for various versions of the demo paper. Use `demopaper()` to directly read it as a paper object from the json file.
#'
#' @param ext the extension of the file
#'
#' @return file path
#' @export
#'
#' @examples
#' json <- demofile()
#' pdf <- demofile("pdf")
demofile <- function(ext = c("json", "pdf", "docx", "doc", "xml", "qmd")) {
  ext <- match.arg(ext)

  file_path <- paste0("demos/to_err_is_human.", ext) |>
    system.file(package = "metacheck")

  return(file_path)
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
  if (!is_paper_list(paper)) {
    if (!is_paper(paper)) stop("paper must be a paper or paperlist object.")
    paper <- list(paper)
  }

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

  dplyr::as_tibble(merged_table)
}


#' Get Paper IDs
#'
#' @param paper a paper or paperlist
#'
#' @returns a table with paper_id column
#' @export
#'
#' @examples
#' paper_id(psychsci)
paper_id <- function(paper) {
  paper_table(paper, "info", "paper_id")
}

#' Reference and DOI table
#'
#' Return a table with fixed DOIs and reference text from a paper object or concatenate tables across a list of paper objects.
#'
#' @param paper a paper or paperlist
#'
#' @return a merged table
#' @export
#'
#' @examples
#' biblio <- ref_table(psychsci[[1]])
ref_table <- function(paper) {
  bib_id <- text <- NULL
  cols <- c("paper_id", "bib_id", "doi")
  bib_orig <- paper_table(paper, "bib", cols)
  bib_match <- paper_table(paper, "bib_match", cols)
  if (nrow(bib_match) == 0) {
    bib <- bib_orig
  } else {
    bib <- dplyr::anti_join(bib_orig, bib_match,
                            by = c("paper_id", "bib_id")) |>
      dplyr::bind_rows(bib_match) |>
      dplyr::arrange(paper_id, bib_id)
  }

  ref_text <- dplyr::inner_join(
    paper_table(paper, "bib"),
    paper_table(paper, "text"),
    by = c("paper_id", "text_id")
  ) |>
    dplyr::select(paper_id, bib_id, text)

  dplyr::inner_join(bib, ref_text, by = c("paper_id", "bib_id"))
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
#' \dontrun{
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

  jsonlite::write_json(paper, json_path,
                       na = "null",
                       null = "null",
                       auto_unbox = TRUE,
                       pretty = TRUE)

  return(json_path)
}


#' View a figure image
#'
#' @param paper a paper object
#' @param figure_id the id for the figure to show
#'
#' @returns plots the figure
#' @export
#'
#' @examples
#' paper <- demopaper()
#' fig_image_view(paper, 1)
#' fig_image_view(paper, 2)
fig_image_view <- function(paper, figure_id = 1) {
  figs <- paper$figure
  b64 <- figs[figs$figure_id == figure_id[[1]], "image"]

  if (length(b64) == 0 || is.na(b64)) return(NULL)

  img_binary <- sub("^data:image/[^;]+;base64,", "", b64) |>
    base64enc::base64decode()

  img <- jpeg::readJPEG(img_binary)
  plot(grDevices::as.raster(img))
}
