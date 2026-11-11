#' Read in grobid XML or bibr JSON
#'
#' @param file_path path to a single directory containing XML and/or JSON files, or a vector of XML/JSON paths
#' @param include_images whether to include images in the figures table of the paper object (they make object size larger, only relevant to bibr imports)
#' @param recursive whether to read files in subfolders (files should have unique paper_ids, or errors can occur)
#'
#' @returns a paper or paperlist
#' @export
#' @examples
#' paper <- read(demofile("xml"))
#' paper$info$title
read <- function(file_path, include_images = FALSE, recursive = FALSE) {
  # handle directory or multiple files ----
  if (length(file_path) == 1 && dir.exists(file_path)) {
    dir_path <- file_path
    file_path <- list.files(dir_path,
                            pattern = "\\.(json|xml)$",
                            recursive = recursive,
                            full.names = TRUE)
  }
  if (length(file_path) == 0) {
    message("No JSON or XML files found.")
    return(paperlist())
  }

  # remove XML where same JSON and XML found
  json <- grep("\\.json$", file_path, value = TRUE)
  xml_dupes <- gsub("\\.json$", "\\.xml", json)
  file_path <- setdiff(file_path, xml_dupes)

  pb <- pb(length(file_path), "Loading :current/:total [:bar] (:what)")
  papers <- lapply(file_path, \(fp) {
    pb$tick(1, list(what = basename(fp)))
    tryCatch({
      if (grepl("\\.json$", fp, ignore.case = TRUE)) {
        .read_bibr(fp, include_images)
      } else if (grepl("\\.xml$", fp, ignore.case = TRUE)) {
        .grobid_to_bibr(fp)
      }
    }, error = \(e) {
      logger("read", e$message)
      warning("Could not read ", basename(fp), ": ", e$message, call. = FALSE)
      return(NULL)
    })
  })
  papers <- paperlist(papers[!vapply(papers, is.null, logical(1))])
  if (length(papers) == 1) papers <- papers[[1]]

  return(papers)
}


#' Read bibr JSON file
#'
#' @param file_path path to the JSON file
#' @param include_images whether to include images in the figures table of the paper object (they make object size larger)
#'
#' @returns a paper object
#' @export
#'
#' @keywords internal
#' @examples
#' paper <- .read_bibr(demofile("json"))
#' paper$info$title
.read_bibr <- function(file_path, include_images = FALSE) {
  # read JSON ----
  # keys read exactly ([[ ]], not $): with no bib key, data$bib read the
  # bib_match rows as references (U25)
  data <- jsonlite::read_json(file_path,
                              simplifyVector = TRUE,
                              simplifyDataFrame = TRUE)

  # A JSON file that is not a paper ("{}", or the output of another tool) was
  # read as a paper with no paper_id, and every module then failed with
  # "replacement has 0 rows, data has 1" (#499). Every bibr export, old and
  # current schema, has a paper_id at its root.
  id <- if (is.list(data)) data[["paper_id"]] else NULL
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    stop(basename(file_path), " is not bibr output (it has no paper_id), ",
         "so it cannot be read as a paper", call. = FALSE)
  }

  # bibr export schema 11 and later put schema_version at the root ----
  if (!is.null(data[["schema_version"]])) {
    return(.read_bibr12(file_path, include_images))
  }

  paper <- paper()
  paper$paper_id <- data[["paper_id"]]

  # info ----
  info <- data[["info"]]
  keywords <- info$keywords
  info$keywords <- NA
  zeros <- sapply(info, length) == 0
  info[zeros] <- NA
  paper$info <- as.data.frame(info)
  paper$info$keywords <- I(list(keywords))
  paper$info$abstract <- NULL # TODO: remove after bibr fixed

  # author ----
  if (!is.null(data[["author"]]) && length(data[["author"]]) > 0) {
    paper$author <- as.data.frame(data[["author"]])
  }

  # bib ----
  if (!is.null(data[["bib"]]) && length(data[["bib"]]) > 0) {
    paper$bib <- as.data.frame(data[["bib"]])
    # paper$bib$authors <- .coerce_bib_authors(paper$bib$authors)
    # if ("editors" %in% names(paper$bib)) {
    #   paper$bib$editors <- .coerce_bib_authors(paper$bib$editors)
    # }
  }

  # eq ----
  if (!is.null(data[["eq"]]) && length(data[["eq"]]) > 0) {
    paper$eq <- as.data.frame(data[["eq"]])
  }


  # figure ----
  if (!is.null(data[["figure"]]) && length(data[["figure"]]) > 0) {
    paper$figure <- as.data.frame(data[["figure"]])
    if (!include_images) {
      paper$figure$image <- NA_character_
    }
    paper$figure$caption <- NULL #tempfix
  }

  # url ----
  if (!is.null(data[["url"]]) && length(data[["url"]]) > 0) {
    paper$url <- as.data.frame(data[["url"]])
    paper$url$href <- gsub("\\s", "", paper$url$href) # tempfix
    paper$url$href <- gsub("\\.$", "", paper$url$href) # tempfix
  }

  # section ----
  if (!is.null(data[["section"]]) && length(data[["section"]]) > 0) {
    paper$section <- as.data.frame(data[["section"]])
  }

  # table ----
  if (!is.null(data[["table"]]) && length(data[["table"]]) > 0) {
    paper$table <- as.data.frame(data[["table"]])
    paper$table$caption <- NULL #tempfix

    # `contents` is a RAGGED array of arrays (one row per table row, itself an
    # array of cell strings — see .tei_table_contents() in R/import-grobid.R).
    # The main read above uses simplifyVector/simplifyDataFrame = TRUE (needed
    # for every OTHER field to come back as plain data frames), but jsonlite's
    # simplifier reshapes THIS field into a 2D character matrix per table
    # instead of preserving list(header_row, data_row, ...) — confirmed
    # directly against a real corpus paper's table: a 16-row x 10-cell table
    # round-tripped through simplifyVector = TRUE came back as a `chr[16,10]`
    # matrix, not a 16-element list of 10-cell vectors, which broke
    # extract_eq_table()'s row/header indexing silently (no error, just wrong
    # results). Re-read ONLY the table section unsimplified and splice its
    # `contents` back in, which keeps every other field's existing (correct)
    # simplified shape untouched.
    raw_tables <- tryCatch(
      jsonlite::read_json(file_path, simplifyVector = FALSE)$table,
      error = function(e) NULL)
    if (!is.null(raw_tables) && length(raw_tables) == nrow(paper$table)) {
      paper$table$contents <- lapply(raw_tables, function(t) {
        rows <- t$contents
        if (is.null(rows)) return(NULL)
        # jsonlite::read_json(simplifyVector = FALSE) keeps EVERY JSON array
        # as a list, never simplified — so each row comes back as a list of
        # length-1 character strings, not a plain character vector. Every
        # downstream table reader (.table_typed_cells(), .table_matrix_cols(),
        # both in R/match-table.R) does VECTORISED string ops on a row
        # (strsplit(header, ...), grepl() across a whole row at once), which
        # error on a list rather than a character vector — confirmed as a
        # real crash against a real corpus paper's table ("non-character
        # argument" from strsplit()). unlist() each row back to a character
        # vector, one level only (so a genuinely ragged/nested cell, if Grobid
        # ever produced one, is not silently flattened further than this).
        lapply(rows, function(r) unlist(r, use.names = FALSE))
      })
    }
  }

  # text ----
  if (!is.null(data[["text"]]) && length(data[["text"]]) > 0) {
    paper$text <- as.data.frame(data[["text"]])
  }

  # xref ----
  if (!is.null(data[["xref"]]) && length(data[["xref"]]) > 0) {
    paper$xref <- as.data.frame(data[["xref"]])
  }

  # bib_match ----
  if (!is.null(data[["bib_match"]]) && length(data[["bib_match"]]) > 0) {
    paper$bib_match <- as.data.frame(data[["bib_match"]])
    # TODO: check if this is needed
    # if ("authors" %in% names(paper$bib_match)) {
    #   paper$bib_match$authors <- .coerce_bib_authors(paper$bib_match$authors)
    # }
    # if ("editors" %in% names(paper$bib_match)) {
    #   paper$bib_match$editors <- .coerce_bib_authors(paper$bib_match$editors)
    # }
  }

  # ensure all expected columns exist and have correct types
  # (JSON may drop all-NA columns or read them back as logical)
  paper <- .paper_repair_links(paper)
  paper <- .paper_coerce(paper)

  paper
}


# Links broken by a space ----
#
# Reading a PDF (bibr and Grobid alike) sometimes turns the end of a line
# inside a link into a space: "https://github.com/ owner/repo",
# "10.1016/ j.cognition.2020.104", "doi. org/". The link is then not found
# whole, so a repository is not checked. The repair removes such a space
# only where a fixed part of a link comes before it (#411, #458). Removing
# the space after every link is not safe: most links are followed by an
# ordinary word, and of 909 main-text links followed by a space and more
# text (17 corpora), joining all of them changed 368 links that were right
# as written into links that do not exist. Repositories found by many
# website names (Dataverse, DataONE, DSpace) are left out: no one fixed
# name comes before the space.

# The text after a space looks like part of a link: it starts with a letter
# or digit, and before the next space it has a "/", a "_", a digit, or a
# capital letter after a small one ("seantuck12/MODISTools", "RoboDoig"); an
# ordinary word ("with", "Harvard", "review?") does not.
.link_part <- "(?=[A-Za-z0-9])(?=\\S*?(?:[/_0-9]|[a-z][A-Z]))"

# Repositories whose links begin with one fixed website name
.link_hosts <- paste(
  "github\\.com", "gitlab\\.com", "figshare\\.com", "bitbucket\\.org",
  "datadryad\\.org", "aspredicted\\.org", "zenodo\\.org", "researchbox\\.org",
  sep = "|")

#' Join links that reading the PDF split with a space
#'
#' @param x a character vector (the sentences of a paper)
#' @returns `x` with those spaces removed; nothing else is changed
#' @noRd
.repair_links <- function(x) {
  if (!length(x)) return(x)
  # every rule needs a "/", "_ ", "http" or a website's ending after a full
  # stop and a space; most sentences have none of these, so only the others
  # go through the rules
  may <- which(grepl("/|_\\s|https?\\s*:|\\.\\s+(?:org|com|io)\\b", x,
                     perl = TRUE, ignore.case = TRUE))
  if (!length(may)) return(x)
  x[may] <- .repair_link_rules(x[may])
  x
}

.repair_link_rules <- function(x) {
  # every rule with perl = TRUE: R's other engine took 2 to 12 seconds per
  # rule for 46,000 sentences with ignore.case = TRUE, perl 0.1
  x <- x |>
    # "https ://", "https: //", and "https:// doi.org": only a website's
    # name follows "https://"
    gsub("\\b(https?)\\s*:\\s*//", "\\1://", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    gsub("\\b(https?://)\\s+(?=[A-Za-z0-9])", "\\1", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    # a website's name split from its ending: "doi. org", "github. com"
    gsub("\\b(doi|zenodo|researchbox|bitbucket|datadryad)\\.\\s+org\\b",
         "\\1.org", x = _, ignore.case = TRUE, perl = TRUE) |>
    gsub("\\b(github|gitlab|figshare)\\.\\s+com\\b", "\\1.com", x = _,
         ignore.case = TRUE, perl = TRUE) |>
    gsub("\\bosf\\.\\s+io\\b", "osf.io", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    # a space after the "/" of a DOI link or an OSF link
    gsub("\\bdoi\\.org/\\s+", "doi.org/", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    gsub("\\bosf\\.io/\\s+", "osf.io/", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    # and of another repository, when the text after it looks like part of
    # a link: "zenodo.org/ with doi:" stays as it is
    gsub(paste0("(?i:\\b(", .link_hosts, ")/)\\s+", .link_part), "\\1/",
         x = _, perl = TRUE) |>
    # a space after a DOI's prefix, which is never followed by one
    # ("10.1016/ j.cognition"), or inside a repository's DOI or record
    gsub("(\\b10\\.\\d{4,9}/)\\s+", "\\1", x = _, perl = TRUE) |>
    gsub("\\b(records?/)\\s+(?=[0-9])", "\\1", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    gsub("(/dryad\\.|/zenodo\\.)\\s+([a-z0-9])", "\\1\\2", x = _,
         ignore.case = TRUE, perl = TRUE) |>
    gsub("(/m9\\.figshare\\.)\\s+([0-9])", "\\1\\2", x = _,
         ignore.case = TRUE, perl = TRUE)

  # a link that ends in "_" goes on after the space: a link never ends in
  # "_" ("github.com/nardus/zoonotic_ rank/tree/main"); a link can be split
  # more than once
  cut <- paste0("((?i:https?://|www\\.|\\b(?:osf\\.io|", .link_hosts,
                ")/)\\S*_)\\s+(?=[A-Za-z0-9])")
  for (i in 1:5) {
    joined <- gsub(cut, "\\1", x, perl = TRUE)
    if (identical(joined, x)) break
    x <- joined
  }
  x
}

# The positions (1 = first character) in `old` of the characters that are
# not in `new`, where `new` is `old` with some spaces removed
.removed_positions <- function(old, new) {
  o <- strsplit(old, "")[[1]]
  n <- strsplit(new, "")[[1]]
  gone <- integer(0)
  j <- 1L
  for (i in seq_along(o)) {
    if (j <= length(n) && o[[i]] == n[[j]]) j <- j + 1L else gone <- c(gone, i)
  }
  gone
}

#' Join links in a paper's sentences that reading the PDF split with a space
#'
#' Repairs the text table with `.repair_links()`. The url, xref and eq tables
#' give where each item is in its sentence (`start`, counted from 0, and
#' `end`, just after it); these move back by the number of spaces removed
#' before them, so a paper written with `paper_write()` stays right.
#'
#' @param paper a paper or paperlist
#' @returns the paper or paperlist
#' @noRd
.paper_repair_links <- function(paper) {
  if (.is_paper_list(paper)) {
    return(paperlist(lapply(paper, .paper_repair_links)))
  }
  tx <- paper$text
  if (!is.data.frame(tx) || !nrow(tx) || !is.character(tx$text)) return(paper)

  old <- tx$text
  new <- .repair_links(old)
  changed <- which(!is.na(old) & old != new)
  if (!length(changed)) return(paper)
  paper$text$text <- new

  if (!"text_id" %in% names(tx)) return(paper)
  for (tbl in c("url", "xref", "eq")) {
    df <- paper[[tbl]]
    if (!is.data.frame(df) || !nrow(df) ||
        !all(c("text_id", "start", "end") %in% names(df))) next
    for (r in changed) {
      rows <- which(df$text_id %in% tx$text_id[[r]])
      if (!length(rows)) next
      gone <- .removed_positions(old[[r]], new[[r]])
      for (col in c("start", "end")) {
        p <- df[[col]][rows]
        df[[col]][rows] <- p - vapply(p, \(q) sum(gone <= q), integer(1))
      }
    }
    paper[[tbl]] <- df
  }
  paper
}
