#' Get paper schema
#'
#' Read in the JSON schema for bibr formatted paper objects
#'
#' @returns The schema as a list
#' @keywords internal
#'
.paper_schema <- function() {
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
#' @examples
#' p <- paper("my_paper")
#' p
paper <- function(id = NULL, ...) {
  if (is.null(id)) {
    # make a random hash from the time: the md5 of its text, written to a
    # temporary file, because tools::md5sum(bytes = _) needs R >= 4.5.0 while
    # DESCRIPTION allows R >= 4.3.0 (digest is only suggested). The same hash
    # digest::digest(algo = "md5", serialize = FALSE) gave.
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    Sys.time() |> format("%s%OS6") |> charToRaw() |> writeBin(tmp)
    id <- unname(tools::md5sum(tmp)) |> substr(1, 14)
  }

  paper <- list(
    paper_id = id
  )
  class(paper) <- c("scivrs_paper", "list")

  schema <- .paper_schema()

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

  paper <- .paper_coerce(paper)

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
#' p1 <- demopaper()
#' p2 <- read(demofile("xml"))
#' plist <- paperlist(p1, p2)
#'
#' # the two are the same paper, read from two files
#' merged <- paperlist(p1, p2, merge_duplicates = TRUE)
paperlist <- function(..., merge_duplicates = FALSE) {
  dots <- list(...)

  if (.is_paper_list(dots)) {
    paperlist <- dots
  } else {
    .is_paper <- sapply(dots, inherits, "scivrs_paper")
    dots[.is_paper] <- lapply(dots[.is_paper], list)
    .is_paperlist <- sapply(dots, .is_paper_list)
    if (all(.is_paperlist)) {
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
  # papers left sharing an id are renamed "~2", ... (U208)
  paperlist <- .unique_paper_ids(paperlist)

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

  p$text <- dplyr::tibble(
    text_id = seq_along(text),
    section_id = 0,
    paragraph_id = 0,
    text = as.character(text),
    formatted = as.character(text)
  )

  p$section <- dplyr::tibble(
    section_id = 0,
    header = "Test",
    parent_section_id = NA_integer_,
    section_type = "unknown",
    classification_score = 0
  )

  # Fill a row of the info table paper() already built from the schema, rather
  # than replacing it with a narrower one. Overwriting dropped every column not
  # set here -- including `doi`, which report_qmd() reads, so building a report
  # from a test paper warned "Unknown or uninitialised column" on every run.
  p$info[1, ] <- NA
  p$info$title <- "Test Paper"
  p$info$file_hash <- p$paper_id
  p$info$input_format <- "test"

  p$url <- dplyr::tibble(
    href = url,
    link_text = NA_character_,
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
  schema <- .paper_schema_bibr12()
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
    if (is.null(ref)) return(NULL) # not a table, e.g. extraction
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

  # paper_check <- .paper_coerce(paper)
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
#' @keywords internal
.paper_coerce <- function(paper) {
  if (.is_paper_list(paper)) {
    papers <- lapply(paper, .paper_coerce) |>
      paperlist()
    return(papers)
  }

  schema <- .paper_schema_bibr12()

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
    if (is.null(ref)) next # not a table, e.g. extraction
    def <- strsplit(ref, "/")[[1]][[3]]
    prop <- schema$`$defs`[[def]]$properties
    cols <- intersect(names(paper[[tbl]]), names(prop))
    for (col in cols) {
      schema_type <- prop[[col]]$type[[1]]
      if (schema_type %in% names(type_func)) {
        paper[[tbl]][[col]] <- tryCatch(
          type_func[[schema_type]](paper[[tbl]][[col]]),
          error = \(e) {
            logger(label = ".paper_coerce",
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

            logger(label = ".paper_coerce",
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
#' @keywords internal
.is_paper <- function(paper) {
  if (!is.list(paper)) {
    return(FALSE)
  }
  .is_paper <- inherits(paper, "scivrs_paper")

  return(.is_paper)
}

#' Detect a list of paper objects
#'
#' @param paper the object to test
#'
#' @returns logical
#' @keywords internal
.is_paper_list <- function(paper) {
  if (!is.list(paper)) {
    return(FALSE)
  }

  .is_paper <- sapply(paper, inherits, what = "scivrs_paper")
  if (all(.is_paper)) {
    return(TRUE)
  }

  return(FALSE)
}


#' Print Paper Object
#'
#' @param x The scivrs_paper list
#' @param ... Additional parameters for print
#' @returns `x`, invisibly
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
  invisible(x)
}

#' Print PaperList Object
#'
#' @param x The scivrs_paperlist object
#' @param ... Additional parameters for print
#' @returns `x`, invisibly
#'
#' @export
#' @keywords internal
print.scivrs_paperlist <- function(x, ...) {
  txt <- paper_table(x, "info", c("title", "doi"))

  print(txt)
  invisible(x)
}

#' Subset PaperList Object
#'
#' @param x The scivrs_paperlist object
#' @param ... Additional parameters for print
#' @param drop relevant for matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples).
#' @returns a `scivrs_paperlist` with the selected papers
#'
#' @export
#' @keywords internal
`[.scivrs_paperlist` <- function(x, ..., drop = TRUE) {
  paperlist(NextMethod())
}


#' Get demo paper
#'
#' `"to_err_is_human"` (the default) is a short paper written to show
#' metacheck's checks. `"stroop_effect"` is a small reproducible analysis
#' whose data and code are bundled as a demo repository (see
#' `demofile("repo")`); the paper is its manuscript, converted with Grobid.
#'
#' @param paper which demo paper: `"to_err_is_human"` or `"stroop_effect"`
#'
#' @return paper object
#' @export
#'
#' @examples
#' paper <- demopaper()
#' stroop <- demopaper("stroop_effect")
demopaper <- function(paper = c("to_err_is_human", "stroop_effect")) {
  paper <- match.arg(paper)
  file_path <- system.file(paste0("demos/", paper, ".json"),
                           package = "metacheck")

  .read_bibr(file_path)
}

#' Get a demo file
#'
#' Return the file path for various versions of the demo paper. Use `demopaper()` to directly read it as a paper object from the json file.
#'
#' `demofile("repo")` returns the folder of a demo repository instead: a
#' small copy of <https://github.com/Lakens/stroop_effect_paper> (data, code,
#' codebook, README, licence and `renv.lock`), for functions that check a
#' repository on your own computer, such as [local_files()].
#'
#' @param ext the extension of the file, or `"repo"` for the demo repository
#'
#' @return file path (a folder for `"repo"`)
#' @export
#'
#' @examples
#' json <- demofile()
#' pdf <- demofile("pdf")
#' repo <- demofile("repo")
demofile <- function(ext = c("json", "pdf", "docx", "doc", "xml", "qmd", "repo")) {
  ext <- match.arg(ext)

  if (ext == "repo") {
    return(system.file("extdata/stroop_effect_paper", package = "metacheck"))
  }

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
#' paper <- demopaper()
#' biblio <- paper_table(paper, "bib")
#' xrefs <- paper_table(paper, "xref")
paper_table <- function(paper, table, cols = NULL) {
  if (!.is_paper_list(paper)) {
    if (!.is_paper(paper)) stop("paper must be a paper or paperlist object.")
    paper <- list(paper)
  }
  # An empty paper list has no tables to bind, so the result had no columns,
  # or logical ones once text_search() added them: joins on paper_id and
  # string functions on text stopped nearly every module (U79). An empty
  # paper's tables are typed and have no rows.
  if (length(paper) == 0) paper <- list(paper())

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
#' @returns a vector of paper_ids
#' @export
#'
#' @examples
#' paper <- demopaper()
#' paper_id(paper)
paper_id <- function(paper) {
  # each paper's own id: read from the info tables, a paper whose info table
  # has no rows had no id, so a paper list lost it (U102)
  valid_id <- \(id) length(id) == 1 && !is.na(id) && nzchar(id)
  if (.is_paper(paper) && valid_id(paper$paper_id)) return(as.character(paper$paper_id))
  if (.is_paper_list(paper) && length(paper) > 0 &&
      all(vapply(paper, \(p) valid_id(p$paper_id), logical(1)))) {
    return(unname(vapply(paper, \(p) as.character(p$paper_id), character(1))))
  }
  paper_table(paper, "info", "paper_id")$paper_id
}

# A module's summary table with one row per paper and the same value in every
# row of each column: .per_paper_table(paper, repo_n = 0, files_n = NA). For
# an empty paper or paper list it has no rows; data.frame(paper_id =
# character(0), repo_n = 0) stopped with "arguments imply differing number of
# rows: 0, 1" (U79).
.per_paper_table <- function(paper, ...) {
  out <- data.frame(paper_id = paper_id(paper) %||% character(0),
                    stringsAsFactors = FALSE)
  cols <- list(...)
  for (nm in names(cols)) out[[nm]] <- rep(cols[[nm]], length.out = nrow(out))
  out
}

# Papers in a list that share a paper_id: the second and later get "~2",
# "~3", ... (with a warning naming them). Tables are joined and grouped on
# paper_id, so a shared id mixed the papers: sentences joined to the other
# paper's sections (every row twice), summaries with more rows than papers,
# references pooled, and ethics_check and open_practices stopped ("factor
# level [2] is duplicated") (U208, U101). Applied where a list comes in:
# paperlist(), module_run() and report().
.unique_paper_ids <- function(paper) {
  if (!.is_paper_list(paper) || length(paper) < 2) return(paper)
  ids <- vapply(paper, \(x) as.character(x$paper_id %||% NA_character_)[1], character(1))
  dup <- duplicated(ids) & !is.na(ids)
  if (!any(dup)) return(paper)
  occurrence <- vapply(seq_along(ids), \(i) sum(ids[seq_len(i)] %in% ids[i]), integer(1))
  new_ids <- ifelse(dup, paste0(ids, "~", occurrence), ids)
  for (i in which(dup)) paper[[i]]$paper_id <- new_ids[i]
  names(paper) <- new_ids
  warning(sprintf(
    "%d paper%s shared a paper_id with an earlier paper and %s renamed: %s",
    sum(dup), plural(sum(dup)), plural(sum(dup), "was", "were"),
    paste(new_ids[dup], collapse = ", ")), call. = FALSE)
  paper
}

# TRUE when a paper or paper list has no text and no references, so a module
# that reads them has nothing to check. Such a module reports "na" for it
# (.empty_input_result()): an empty paper got red ("no statement found") or
# green ("nothing flagged") from nine modules. Inputs that are not papers are
# never empty here.
.paper_is_empty <- function(paper) {
  if (!.is_paper(paper) && !.is_paper_list(paper)) return(FALSE)
  nrow(paper_table(paper, "text")) == 0 && nrow(paper_table(paper, "bib")) == 0
}

# The result of a text or reference module for an empty input (see
# .paper_is_empty()).
.empty_input_result <- function(paper) {
  list(traffic_light = "na",
       summary_text = "There was no text to check.",
       summary_table = .per_paper_table(paper))
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
#' paper <- demopaper()
#' biblio <- ref_table(paper)
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
#' @param save_path the directory to save the JSON file in; there is no default, because CRAN does not allow writing into the
#'   working directory unless you ask for it
#' @param schema_version NULL (the default) saves the paper object as is;
#'   "12.0" saves a bibr export schema 12.0 file, for a paper read from a bibr
#'   12.0 export or converted with `grobid_to_bibr(schema_version = "12.0")`.
#'   The file keeps the paper's extraction block (a bibr export keeps bibr as
#'   its producer and the time bibr extracted it) and names metacheck as the
#'   converter.
#'
#' @returns the path to the JSON file
#' @export
#'
#' @examples
#' paper <- demopaper()
#' paper$info$title <- "New title"
#' json <- paper_write(paper, "new_paper", tempdir())
#' read(json)$info$title
paper_write <- function(paper, file_name = NULL, save_path,
                        schema_version = NULL) {
  if (!is.null(schema_version) && !identical(schema_version, "12.0")) {
    stop("schema_version must be NULL or \"12.0\"", call. = FALSE)
  }

  # made before its full path is asked for, which warned for a new folder
  dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
  save_path <- normalizePath(save_path)

  if (.is_paper_list(paper)) {
    if (is.null(file_name)) file_name <- names(paper)
    pb <- pb(length(paper), ":what [:bar] :current/:total")
    pb$tick(0, list(what = "Saving..."))
    json_paths <- mapply(\(p, f, s) {
      jp <- paper_write(p, f, s, schema_version)
      pb$tick(1, list(what = f))
      jp
    }, paper, file_name, save_path)

    return(invisible(json_paths))
  }

  if (is.null(file_name)) file_name <- paper$paper_id
  file_name <- gsub("\\.(json|zip)$", "", x = file_name)
  json_path <- file.path(save_path, paste0(file_name, ".json"))

  if (!is.null(schema_version)) {
    # digits = NA writes 15 significant digits, so rewriting a bibr export
    # changed values such as 0.9411764705882353 (U22); those are written with
    # the digits they need
    jsonlite::write_json(.json_exact_doubles(.paper_to_bibr12(paper)), json_path,
                         na = "null",
                         null = "null",
                         auto_unbox = TRUE,
                         pretty = TRUE,
                         digits = NA,
                         json_verbatim = TRUE)
    return(invisible(json_path))
  }

  jsonlite::write_json(paper, json_path,
                       na = "null",
                       null = "null",
                       auto_unbox = TRUE,
                       pretty = TRUE)

  invisible(json_path)
}


# A double that 15 significant digits would change, as the shortest JSON text
# that reads back as exactly that double (class "json", for write_json()'s
# json_verbatim); NULL for any other value.
.exact_double_json <- function(v) {
  if (!is.double(v) || length(v) != 1 || !is.finite(v)) return(NULL)
  if (as.numeric(sprintf("%.15g", v)) == v) return(NULL)
  s <- sprintf("%.16g", v)
  if (as.numeric(s) != v) s <- sprintf("%.17g", v)
  structure(s, class = "json")
}

# Every double in `x` (lists, data frame columns, vectors) that 15
# significant digits would change is replaced by .exact_double_json(); the
# rest is left as it is. A vector with such a value becomes a list, which
# write_json() writes as the same array.
.json_exact_doubles <- function(x) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, \(col) {
      out <- .json_exact_doubles(col)
      if (is.double(col) && length(col) == 1 && !is.double(out)) list(out) else out
    })
    return(x)
  }
  if (is.list(x)) {
    x[] <- lapply(x, .json_exact_doubles)
    return(x)
  }
  if (!is.double(x)) return(x)
  exact <- lapply(x, .exact_double_json)
  if (all(vapply(exact, is.null, logical(1)))) return(x)
  out <- Map(\(e, v) e %||% v, exact, x)
  names(out) <- names(x)
  if (length(x) == 1 && !inherits(x, "AsIs")) out[[1]] else out
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
