# bibr export schema 12.x ----
#
# Reads and writes the JSON of bibr's export schema 12.x
# (https://bibr.org/schema/bibr-export-v12.schema.json). A paper read from a
# 12.x file keeps metacheck's own names where it has them -- the 12.x
# `metadata` and `source` objects make the `info` table and `metadata_match` is
# `info_match` -- and the 12.x names and meanings everywhere else: `xref_id` is
# the row's own key and `target_id` the row it cites, and a caption or footnote
# is a text row with no section that `figure`, `table` or `footnote` points at
# with `text_id`. The export's `extraction` block is kept as `paper$extraction`.
# Metacheck keeps a degrees-of-freedom value in its parentheses ("(28)"), as
# its own statistics code expects; 12.x writes it bare ("28").

# The columns of each 12.0 table, in the order bibr writes them, and their
# types: chr, int, num and lgl are scalars; chr[] and int[] are arrays;
# chr[][] is an array of arrays of strings (table contents); json is kept as
# parsed (the person and funder objects of the match tables).
.bibr12_match_cols <- c(
  service = "chr", service_id = "chr", score = "num", bib_type = "chr",
  doi = "chr", title = "chr", author = "json", editor = "json",
  publisher = "chr", year = "int", published_date = "chr", container = "chr",
  volume = "chr", issue = "chr", first_page = "chr", last_page = "chr",
  edition = "chr", version = "chr", url = "chr", license_url = "chr",
  license_spdx = "chr", funder = "json"
)

.bibr12_cols <- list(
  metadata = c(
    title = "chr", abstract = "chr", keywords = "chr[]", doi = "chr",
    pmid = "chr", pmcid = "chr", arxiv = "chr", language = "chr",
    paper_type = "chr", oecd_l1 = "chr", oecd_l2 = "chr", journal = "chr",
    volume = "chr", issue = "chr", first_page = "chr", last_page = "chr",
    issn = "chr", publisher = "chr", published = "chr",
    published_date = "chr", license = "chr", license_url = "chr",
    license_spdx = "chr", funding_statement = "chr", coi_statement = "chr",
    ethics_statement = "chr", data_availability = "chr"
  ),
  author = c(
    author_id = "int", given = "chr", family = "chr", suffix = "chr",
    literal = "chr", email = "chr", corresponding = "lgl", orcid = "chr",
    role = "chr[]", credit_roles = "chr[]"
  ),
  affiliation = c(
    affiliation_id = "int", text = "chr", institution = "chr",
    department = "chr", city = "chr", country = "chr", author_ids = "int[]"
  ),
  funding = c(funding_id = "int", funder = "chr", award_ids = "chr[]"),
  text = c(
    text = "chr", text_id = "int", paragraph_id = "int", section_id = "int",
    page_number = "int", formatted = "chr"
  ),
  section = c(
    section_id = "int", header = "chr", level = "int",
    parent_section_id = "int", section_type = "chr"
  ),
  url = c(
    url_id = "int", href = "chr", link_text = "chr", text_id = "int",
    start = "int", end = "int"
  ),
  bib = c(
    bib_id = "int", text_id = "int", bib_type = "chr", doi = "chr",
    title = "chr", authors = "chr", editors = "chr", publisher = "chr",
    year = "int", year_suffix = "chr", date = "chr", published_date = "chr",
    container = "chr", volume = "chr", issue = "chr", first_page = "chr",
    last_page = "chr", edition = "chr", version = "chr", url = "chr",
    is_in_press = "lgl", arxiv = "chr", pmid = "chr", series = "chr",
    access_date = "chr", note = "chr"
  ),
  xref = c(
    xref_id = "int", target_id = "int", xref_type = "chr", contents = "chr",
    text_id = "int", start = "int", end = "int"
  ),
  figure = c(
    figure_id = "int", label = "chr", section_id = "int", text_id = "int",
    image = "chr", caption = "chr", page_number = "int"
  ),
  table = c(
    table_id = "int", label = "chr", section_id = "int", text_id = "int",
    html = "chr", contents = "chr[][]", caption = "chr", page_number = "int"
  ),
  footnote = c(footnote_id = "int", label = "chr", text_id = "int"),
  eq = c(
    eq_id = "int", text_id = "int", start = "int", end = "int",
    grp_id = "int", verbatim = "chr", lhs = "chr", df = "chr", comp = "chr",
    rhs = "chr"
  ),
  metadata_match = .bibr12_match_cols,
  affiliation_match = c(
    affiliation_id = "int", service = "chr", service_id = "chr",
    score = "num", name = "chr", country_code = "chr"
  ),
  funding_match = c(
    funding_id = "int", service = "chr", service_id = "chr", score = "num",
    name = "chr", country_code = "chr", funder_doi = "chr"
  ),
  bib_match = c(bib_id = "int", .bibr12_match_cols)
)

# 12.0 record tables (names) and the paper table that holds each (values)
.bibr12_tables <- c(
  author = "author", affiliation = "affiliation", funding = "funding",
  text = "text", section = "section", url = "url", bib = "bib",
  xref = "xref", figure = "figure", table = "table", footnote = "footnote",
  eq = "eq", metadata_match = "info_match",
  affiliation_match = "affiliation_match", funding_match = "funding_match",
  bib_match = "bib_match"
)

# the extraction keys 12.0 defines; other keys are not written
.bibr12_extraction_keys <- c(
  "producer", "converter", "completed_at", "ocr", "llm", "settings",
  "timings", "usage", "identity", "enrichment", "diagnostics", "validation",
  "qualification", "warnings", "pages", "float_parts", "text_regions",
  "regions", "trace"
)

#' Paper schema with the bibr 12.x additions
#'
#' paper.json (see `.paper_schema()`) merged with inst/schema/paper-bibr12.json:
#' the tables and columns a paper read from a bibr 12.x export adds. Every
#' addition is optional, so papers in the older format validate as before.
#'
#' @returns the schema as a list
#' @noRd
.paper_schema_bibr12 <- function() {
  merge <- function(base, add) {
    for (nm in names(add)) {
      b <- base[[nm]]
      a <- add[[nm]]
      base[[nm]] <- if (is.list(b) && is.list(a) && !is.null(names(a))) {
        merge(b, a)
      } else if (identical(nm, "enum")) {
        union(b, a)
      } else {
        a
      }
    }
    base
  }

  add <- system.file("schema/paper-bibr12.json", package = "metacheck") |>
    jsonlite::read_json(simplifyVector = TRUE)
  add$description <- NULL
  merge(.paper_schema(), add)
}

#' Is this a paper in bibr export schema 12.x form?
#'
#' @param paper a paper object
#' @returns logical
#' @noRd
.is_bibr12 <- function(paper) {
  isTRUE(grepl("^12\\.", paper$info$schema_version[1]))
}

#' The paper_ids of the papers in bibr export schema 12.x form
#'
#' @param paper a paper or paperlist
#' @returns character vector
#' @noRd
.bibr12_paper_ids <- function(paper) {
  if (.is_paper(paper)) paper <- list(paper)
  ids <- vapply(paper, \(p) {
    if (.is_bibr12(p)) as.character(p$paper_id) else NA_character_
  }, character(1))
  ids[!is.na(ids)]
}

#' Build a typed data frame for one 12.0 table
#'
#' @param columns a named list of columns: vectors, or lists with one element
#'   per row (NULL for a missing value); a missing column is all NA
#' @param cols the table's column types (see `.bibr12_cols`)
#' @param n the number of rows (default: the length of the longest column)
#' @returns a data frame with the columns of `cols`, in order
#' @noRd
.bibr12_df <- function(columns, cols, n = max(c(0L, lengths(columns)))) {
  scalar <- list(chr = as.character, int = as.integer, num = as.double,
                 lgl = as.logical)

  out <- lapply(stats::setNames(nm = names(cols)), \(col) {
    v <- columns[[col]]
    type <- cols[[col]]

    if (type %in% names(scalar)) {
      if (is.null(v)) v <- rep(NA, n)
      if (is.list(v)) {
        v <- unlist(lapply(v, \(e) if (length(e) == 0) NA else e[[1]]))
      }
      return(scalar[[type]](v %||% logical(0)))
    }

    if (is.null(v)) v <- vector("list", n)
    switch(
      type,
      "chr[]" = lapply(v, \(e) as.character(unlist(e))),
      "int[]" = lapply(v, \(e) as.integer(unlist(e))),
      "chr[][]" = lapply(v, \(e) lapply(e, \(r) as.character(unlist(r)))),
      json = lapply(v, \(e) e)
    )
  })

  structure(out, class = "data.frame", row.names = .set_row_names(n))
}

#' The columns of a JSON array of objects
#'
#' @param rows a list of parsed JSON objects
#' @param cols the table's column types (see `.bibr12_cols`)
#' @returns a named list with one list per column, NULL for a missing value
#' @noRd
.bibr12_rows <- function(rows, cols) {
  lapply(stats::setNames(nm = names(cols)), \(col) {
    lapply(rows, \(r) r[[col]])
  })
}

#' Build the info table of a 12.x paper
#'
#' @param metadata the 12.x metadata object (a named list)
#' @param source the 12.x source object (a named list)
#' @param schema_version the file's schema version
#' @param producer the extraction producer object
#' @returns a one-row data frame
#' @noRd
.bibr12_info <- function(metadata, source, schema_version, producer) {
  info <- .bibr12_df(lapply(metadata, list), .bibr12_cols$metadata, 1L)
  info$file_name <- as.character(source$file_name %||% NA)
  info$sha256 <- as.character(source$sha256 %||% NA)
  info$input_format <- as.character(source$input_format %||% NA)
  info$schema_version <- as.character(schema_version)
  # the older info columns paper.json requires
  info$file_hash <- substr(info$sha256, 1, 16)
  info$bibr_version <- if (identical(producer$name, "bibr")) {
    as.character(producer$version)
  } else {
    NA_character_
  }

  info
}

#' Assemble a paper object from 12.x parts
#'
#' @param paper_id the paper id
#' @param info the info table (see `.bibr12_info()`)
#' @param tables a named list (12.0 table names) of column lists
#' @param extraction the extraction block (a list)
#' @returns a paper object
#' @noRd
.bibr12_paper <- function(paper_id, info, tables, extraction) {
  paper <- paper(paper_id)
  paper$info <- info
  for (tbl in names(.bibr12_tables)) {
    paper[[.bibr12_tables[[tbl]]]] <- .bibr12_df(tables[[tbl]] %||% list(),
                                                 .bibr12_cols[[tbl]])
  }

  # the older given/family columns metacheck's modules read (ref_accuracy)
  names_df <- function(persons) {
    data.frame(
      given = vapply(persons, \(p) as.character(p$given %||% NA), ""),
      family = vapply(persons, \(p) as.character(p$family %||% NA), "")
    )
  }
  for (tbl in c("bib_match", "info_match")) {
    paper[[tbl]]$authors <- lapply(paper[[tbl]]$author, names_df)
    paper[[tbl]]$editors <- lapply(paper[[tbl]]$editor, names_df)
  }

  paper$extraction <- extraction

  .paper_coerce(paper)
}

#' Read a bibr export schema 12.x file
#'
#' Called by `.read_bibr()` for a file with a root `schema_version`.
#'
#' @param file_path path to the JSON file
#' @param include_images whether to keep figure images
#' @returns a paper object
#' @noRd
.read_bibr12 <- function(file_path, include_images = FALSE) {
  x <- jsonlite::read_json(file_path, simplifyVector = FALSE)

  version <- as.character(x$schema_version[[1]])
  if (!grepl("^12\\.", version)) {
    stop("bibr export schema ", version, " is not supported: metacheck ",
         "reads schema 12.x and the older files without a root ",
         "schema_version (", basename(file_path), ")", call. = FALSE)
  }

  tables <- lapply(stats::setNames(nm = names(.bibr12_tables)), \(tbl) {
    .bibr12_rows(x[[tbl]], .bibr12_cols[[tbl]])
  })

  # metacheck keeps degrees of freedom in parentheses, as printed: "(28)"
  tables$eq$df <- lapply(tables$eq$df, \(df) {
    if (is.null(df) || grepl("^\\(.*\\)$", df)) df else paste0("(", df, ")")
  })

  if (!include_images) tables$figure$image <- NULL

  info <- .bibr12_info(x$metadata, x$source, version, x$extraction$producer)
  .bibr12_paper(x$paper_id, info, tables, x$extraction)
}

#' Convert a paper to bibr export schema 12.0
#'
#' Called by `paper_write(schema_version = "12.0")`. Keeps the extraction
#' block of the paper (a bibr export keeps bibr as its producer and the time
#' bibr extracted it) and names metacheck as the converter. A paper read from
#' a later 12.x file is not rewritten, since the rewrite would have to keep
#' keys metacheck does not know.
#'
#' @param paper a paper object in 12.0 form
#' @returns a list that `jsonlite::write_json(auto_unbox = TRUE)` writes as
#'   a 12.0 export
#' @noRd
.paper_to_bibr12 <- function(paper) {
  if (!.is_bibr12(paper)) {
    stop("paper_write(schema_version = \"12.0\") writes papers read from a ",
         "bibr 12.x export, or converted from Grobid TEI with ",
         "grobid_to_bibr(schema_version = \"12.0\"); '", paper$paper_id,
         "' is in metacheck's older format", call. = FALSE)
  }
  version <- paper$info$schema_version[[1]]
  if (!identical(version, "12.0")) {
    stop("paper_write(schema_version = \"12.0\") cannot rewrite '",
         paper$paper_id, "': it was read from bibr export schema ", version,
         ", and a rewrite keeps every key, including those metacheck does ",
         "not know", call. = FALSE)
  }

  warnings <- paper$extraction$warnings %||% list()
  warn <- function(code, message) {
    warnings[[length(warnings) + 1L]] <<- list(code = code, message = message)
  }

  tables <- lapply(stats::setNames(nm = names(.bibr12_tables)), \(tbl) {
    df <- paper[[.bibr12_tables[[tbl]]]]
    columns <- if (is.data.frame(df)) as.list(df) else list()
    .bibr12_df(columns, .bibr12_cols[[tbl]], n = NROW(df))
  })

  # 12.x writes degrees of freedom bare: "28"
  tables$eq$df <- sub("^\\((.*)\\)$", "\\1", tables$eq$df)

  # match rows made by metacheck's add_bib_match() have the older columns
  for (tbl in c("bib_match", "metadata_match")) {
    df <- paper[[.bibr12_tables[[tbl]]]]
    rows <- tables[[tbl]]
    if (!NROW(df)) next

    for (who in c("author", "editor")) {
      older <- df[[paste0(who, "s")]]
      if (is.null(df[[who]]) && is.list(older)) {
        rows[[who]] <- lapply(older, \(p) {
          if (!is.data.frame(p) || !nrow(p)) return(NULL)
          lapply(seq_len(nrow(p)), \(i) {
            person <- list(given = p$given[[i]], family = p$family[[i]])
            person[!is.na(person)]
          })
        })
      }
    }

    if (is.null(df$published_date) && !is.null(df$date)) {
      date <- as.character(df$date)
      iso <- grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?$", date)
      rows$published_date <- ifelse(iso, date, NA_character_)
    }

    off_scale <- !is.na(rows$score) & (rows$score < 0 | rows$score > 1)
    if (any(off_scale)) {
      rows$score[off_scale] <- NA_real_
      warn("METACHECK_MATCH_SCORE_NOT_0_1", sprintf(
        "%d %s score(s) were not on the 0-1 scale (e.g. CrossRef relevance scores) and were written as null",
        sum(off_scale), tbl))
    }

    services <- c("crossref", "openalex", "datacite", "doi.org", "openlibrary",
                  "ror", "manual", "other")
    rows$service[!rows$service %in% services] <- "other"
    rows$bib_type <- .bibr12_bib_type(rows$bib_type)
    rows$doi <- .bibr12_doi(rows$doi)

    tables[[tbl]] <- rows
  }

  # arrays: I() keeps a one-element array an array under auto_unbox
  tables <- lapply(stats::setNames(nm = names(tables)), \(tbl) {
    rows <- tables[[tbl]]
    cols <- .bibr12_cols[[tbl]]
    for (col in names(cols)[cols %in% c("chr[]", "int[]")]) {
      rows[[col]] <- lapply(rows[[col]], I)
    }
    for (col in names(cols)[cols == "chr[][]"]) {
      rows[[col]] <- lapply(rows[[col]], \(x) lapply(x, I))
    }
    rows
  })

  info <- paper$info
  metadata <- .bibr12_df(as.list(info), .bibr12_cols$metadata, n = 1L) |>
    as.list() |>
    lapply(\(v) if (is.list(v)) I(v[[1]]) else v[[1]])

  source <- list(
    file_name = as.character(info$file_name[[1]]),
    sha256 = as.character(info$sha256[[1]] %||% NA),
    input_format = as.character(info$input_format[[1]])
  )

  extraction <- paper$extraction
  extraction <- extraction[intersect(names(extraction), .bibr12_extraction_keys)]
  extraction["converter"] <- list(list(
    name = "metacheck",
    version = as.character(utils::packageVersion("metacheck")),
    build_sha = NULL
  ))
  # when the producer extracted the content, which a converter keeps
  extraction["completed_at"] <- list(
    extraction$completed_at %||%
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  for (key in c("ocr", "llm")) {
    if (!key %in% names(extraction)) extraction[key] <- list(NULL)
  }
  extraction["warnings"] <- list(warnings)
  first <- intersect(c("producer", "converter", "completed_at", "ocr", "llm"),
                     names(extraction))
  extraction <- extraction[c(first, setdiff(names(extraction), first))]

  c(
    list(
      paper_id = as.character(paper$paper_id),
      schema_version = "12.0",
      source = source,
      metadata = metadata
    ),
    tables[c("author", "affiliation", "funding", "text", "section", "url",
             "bib", "xref", "figure", "table", "footnote", "eq",
             "metadata_match", "affiliation_match", "funding_match",
             "bib_match")],
    list(extraction = extraction)
  )
}

#' Normalize DOIs for bibr 12.x: bare and lowercase, NA when not a DOI
#'
#' @param x character vector
#' @returns character vector
#' @noRd
.bibr12_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", x)
  x[!grepl("^10\\.[0-9]{4,9}/\\S+$", x)] <- NA_character_
  x
}

#' Map reference types to the bibr 12.x vocabulary
#'
#' @param x character vector of bibtex, CrossRef or 12.x types
#' @returns character vector
#' @noRd
.bibr12_bib_type <- function(x) {
  types <- c("journal_article", "book", "book_chapter", "dataset", "software",
             "preprint", "conference_paper", "report", "thesis", "other")
  map <- c(
    article = "journal_article", "journal-article" = "journal_article",
    incollection = "book_chapter", inbook = "book_chapter",
    "book-chapter" = "book_chapter", inproceedings = "conference_paper",
    conference = "conference_paper",
    "proceedings-article" = "conference_paper", techreport = "report",
    phdthesis = "thesis", mastersthesis = "thesis",
    dissertation = "thesis", "posted-content" = "preprint"
  )
  x <- as.character(x)
  mapped <- !is.na(x) & x %in% names(map)
  x[mapped] <- map[x[mapped]]
  x[!is.na(x) & !x %in% types] <- "other"
  x
}

#' SHA-256 digest of a file
#'
#' @param path file path
#' @returns 64 lowercase hex characters, or NA when neither R >= 4.5 nor the
#'   digest package is available
#' @noRd
.bibr12_sha256 <- function(path) {
  tools_ns <- asNamespace("tools")
  if (exists("sha256sum", envir = tools_ns, inherits = FALSE)) {
    return(unname(get("sha256sum", envir = tools_ns)(path)))
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(path, algo = "sha256", file = TRUE))
  }
  NA_character_
}
