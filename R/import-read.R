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
  paper <- .paper_repair_text(paper)
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

# The full website names of the repositories repo_check reads, and of the
# link services before them (doi.org, hdl.handle.net)
.link_names <- c(
  "doi.org", "dx.doi.org", "hdl.handle.net", "osf.io", "github.com",
  "gitlab.com", "bitbucket.org", "zenodo.org", "figshare.com",
  "datadryad.org", "researchbox.org", "aspredicted.org", "data.mendeley.com",
  "data.4tu.nl", "psycharchives.org", "reshare.ukdataservice.ac.uk",
  "ldbase.org", "archive.softwareheritage.org")

#' Join a website's name that reading the PDF split at a dot
#'
#' A space after a dot ("doi. org/", "hdl. handle. net/") or before one
#' ("doi .org/", "osf .io/") in one of `.link_names` is removed when a "/"
#' follows the name, as in every link to a deposit; no sentence has a
#' repository's name, a split at its dot, and a "/" after it other than a
#' link. In 17 corpora (13,706 papers, 4.5 million sentences) this joined
#' 2,474 names in 705 papers that the earlier rules (a space after the dot
#' of nine names) left split, all of them links ("doi .org/" 2,161,
#' "dx. doi.org/" 207, "osf .io/" 25, "hdl. handle. net/" 21); it no longer
#' joins a name without a "/" after it, which happened in 3 sentences, none a
#' link ("datadryad. org for production"). Letters are kept as written
#' ("OSF. IO/" -> "OSF.IO/").
#'
#' @param x a character vector without NA
#' @returns `x` with those spaces removed
#' @noRd
.repair_link_names <- function(x) {
  nm <- .link_names[order(-nchar(.link_names))]
  parts <- vapply(strsplit(nm, ".", fixed = TRUE),
                  \(p) paste(p, collapse = "\\s*\\.\\s*"), character(1))
  rx <- paste0("(?i)\\b(?:", paste(parts, collapse = "|"), ")(?=\\s*/)")
  m <- gregexpr(rx, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), \(s) gsub("\\s+", "", s))
  x
}

#' Join links that reading the PDF split with a space
#'
#' @param x a character vector (the sentences of a paper)
#' @returns `x` with those spaces removed, and GROBID's temporary folder and
#'   Check Point security wrappers taken out of links; characters are only
#'   removed, never added or changed
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
    # GROBID's own temporary folder, written before a link it could not
    # resolve ("file://localhost/opt/grobid/grobid-home/tmp/10.5281/zenodo.
    # 15527338"): 22 sentences in 12 papers of 17 corpora (#634)
    gsub("file://localhost/\\S*?grobid-home/tmp/", "", x = _, perl = TRUE) |>
    # a Check Point e-mail security wrapper around a link
    # ("https://protect.checkpoint.com/v2/___https://osf.io/qby5j___.YzJ1...");
    # wrappers that encode the link (Outlook, Proofpoint) are undone when
    # repo_check gathers links, as decoding changes characters
    # (.repo_link_unwrapped())
    gsub("https?://protect\\.checkpoint\\.com/v2/___(\\S+?)___\\.[A-Za-z0-9+/=_-]+",
         "\\1", x = _, ignore.case = TRUE, perl = TRUE) |>
    # "https ://", "https: //", and "https:// doi.org": only a website's
    # name follows "https://"
    gsub("\\b(https?)\\s*:\\s*//", "\\1://", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    gsub("\\b(https?://)\\s+(?=[A-Za-z0-9])", "\\1", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    # a website's name split at a dot ("doi. org/", "osf .io/", "hdl. handle.
    # net/"), see .repair_link_names()
    .repair_link_names() |>
    # a space after the "/" of a DOI link or an OSF link
    gsub("\\b(doi\\.org/)\\s+", "\\1", x = _, ignore.case = TRUE,
         perl = TRUE) |>
    gsub("\\b(osf\\.io/)\\s+", "\\1", x = _, ignore.case = TRUE,
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

# References whose DOI a space split ----
#
# The rules above need a fixed part of a link before the space. In the
# reference list a DOI is often split anywhere, also inside its number
# ("10.5281/zenodo.76187 49", "10. 5281/ ZENODO. 13986711"), and no rule can
# tell those spaces from the ones after a link. But the reader already found
# each reference's DOI whole (GROBID's <idno type="DOI">, bibr's own), with
# the sentence it is in. Where that DOI is in its sentence only with spaces
# inside, the spaces are removed, so the text holds the DOI the reader found;
# no character is added or changed (#459). In 17 corpora (10,453 papers),
# 51,362 reference DOIs were in their sentence only with spaces; of 300 of
# them, 296 exist as joined, at doi.org (checked 2026-11-12).
#
# Two exceptions. The reader's DOI sometimes took in the word after it, the
# next reference's author or a word of the text ("...90173-ICao",
# "...010.Targeting"), while the sentence has the space: 61 of the 71 DOIs
# with a capitalised word after a space do not exist. A space before a
# capitalised word is therefore kept. And a DOI that holds a second DOI
# ("10.1007/BF0251070510.1007/BF02510705") is left out, as joining it would
# break the copy written right.

#' Join each reference's DOI in its sentence
#'
#' @param text the sentences of a paper
#' @param text_id their ids
#' @param bib the paper's bib table (`doi`, `text_id`)
#' @returns `text` with spaces inside the references' DOIs removed
#' @noRd
.repair_bib_dois <- function(text, text_id, bib) {
  if (!is.data.frame(bib) || !nrow(bib) ||
      !all(c("doi", "text_id") %in% names(bib))) return(text)
  doi <- as.character(bib$doi)
  ok <- !is.na(doi) & nzchar(doi) & !is.na(bib$text_id) &
    !grepl(".10\\.\\d{4,9}/", doi, perl = TRUE)
  rows <- match(bib$text_id[ok], text_id)
  doi <- doi[ok]
  for (k in which(!is.na(rows))) {
    r <- rows[[k]]
    s <- text[[r]]
    if (is.na(s) || grepl(tolower(doi[[k]]), tolower(s), fixed = TRUE)) next
    pat <- paste(.zenodo_regex_escape(strsplit(doi[[k]], "")[[1]]),
                 collapse = "\\s*")
    m <- regexpr(pat, s, perl = TRUE, ignore.case = TRUE)
    if (m < 0) next
    end <- m + attr(m, "match.length")
    joined <- gsub("\\s+(?![A-Z][a-z])", "", substr(s, m, end - 1), perl = TRUE)
    text[[r]] <- paste0(substr(s, 1, m - 1), joined, substr(s, end, nchar(s)))
  }
  text
}

# Line numbers ----
#
# A manuscript with numbered lines keeps some of its line numbers when GROBID
# or bibr reads it, as stand-alone numbers inside sentences: "t(118) = 7.35,
# p < 538 .001", "chi2(1) = 542 26.69", "no main effect of 918 919 920 ... 936
# cue type". A statistic is then read with the wrong value, or not at all
# (#648). In the 1,586 preprints of the two report sites, about 1.5 to 2% hold
# such numbers.
#
# Line numbers are told from the other stand-alone numbers in a text (numbers
# in sentences, numbered citations printed as plain numbers, tables) by how
# they advance: one per printed line, so the text between two of them is
# about 50 to 150 characters per step. A paper is taken to hold line numbers
# when the longest chain of stand-alone numbers that increases in steps of 1
# to 10 through the text has at least 10 such steps, and they are at least
# half of its steps that are not within one run of numbers, and its middle
# value is 50 or more. Numbered citations advance about 270 to 300 characters
# per step, with a wide spread; tables advance a few characters per step; and
# ordinary small numbers ("Diary 1", "fixed to 1.") can chain at a line's rate
# but stay small. A run of 5 or more numbers that each add one, from 30
# upwards ("894 895 896 ..."), is the numbers of empty lines; "Tables 1 2 3
# 4" and the ticks of an axis start lower, and a run of years (1950 to 2050)
# stays. In the 22 papers where only such runs were found, all 28 runs were
# line numbers.
#
# Either sign is taken as sure. Then the runs, and the chain's numbers that
# are a line's distance from their neighbours in it, are removed, and from
# them one pass through the whole text, across sentences, removes each other
# stand-alone number that fits the line numbers before and after it. In a
# paper without either sign nothing is removed. In the 1,586 preprints, 75
# papers changed and 1,561 numbers were removed. Of 40 removed numbers drawn
# at random (from an earlier version), 36 were line numbers and 3 more
# probably so; of the 105 the pass first added, 101 were line numbers before
# the amount and table rules below; and all 96 that allowing a short line
# and checking the chain's ends on both sides added were line numbers, while
# it kept "acquiring 160 contiguous slices" and "68 of 250 vaccinated" (and
# 17 line numbers, mostly next to a table the reader moved).
#
# A stand-alone number here is 1 to 4 digits, not in brackets or a list, not
# part of a range or a decimal, and not the number of a label ("Table 2",
# "Study 3", "et al. 12", "n 40").
.line_no_label <- paste0(
  "(?i)(?:^|\\W)(?:tables?|figures?|figs?\\.?|stud(?:y|ies)|experiments?|sections?|chapters?|",
  "appendix|items?|waves?|sessions?|steps?|models?|hypothes[ie]s|pages?|pp?\\.|vol\\.?|no\\.?|",
  "versions?|grades?|days?|weeks?|years?|months?|et al\\.?|refs?\\.?|n|k|h|s|t)\\s*$")

# Is going `dv` line numbers up over `chars` characters a line's distance?
# 40 to 170 characters per line on average; one line may be shorter, down to
# nothing, as the last line of a paragraph, a heading or an empty line can be
# ("ongoing at the time of assessment. 158", "894 895 896")
.line_step_ok <- function(dv, chars) {
  (chars / dv >= 40 & chars / dv <= 170) | (dv == 1L & chars <= 170)
}

# `text` the sentences of one paper, in order; returns them with the line
# numbers removed (and the space beside each)
.repair_line_numbers <- function(text) {
  if (!length(text)) return(text)
  t <- ifelse(is.na(text), "", as.character(text))
  m <- gregexpr("(?<![\\w.,\\-\u2212\u2013])\\d{1,4}(?![\\w,]|\\.\\d)", t, perl = TRUE)
  found <- vapply(m, \(st) st[[1]] > 0L, logical(1))
  if (!any(found)) return(text)
  toks <- data.frame(
    s = rep(which(found), lengths(m[found])),
    start = as.integer(unlist(m[found])),
    len = unlist(lapply(m[found], attr, "match.length")))
  toks$value <- as.integer(substring(t[toks$s], toks$start, toks$start + toks$len - 1L))
  offset <- cumsum(c(0, nchar(t) + 1L))[seq_along(t)]
  toks$pos <- offset[toks$s] + toks$start
  before <- trimws(substr(t[toks$s], 1L, toks$start - 1L), "right")
  after <- trimws(substring(t[toks$s], toks$start + toks$len), "left")
  prev <- substring(before, nchar(before)); nxt <- substr(after, 1L, 1L)
  stand <- !prev %in% c("[", "(", ",", ";", "\u2013", "-", "/", ":") &
    !nxt %in% c("]", ")", ",", ";", "\u2013", "-", "/", "%", ":") &
    !grepl(.line_no_label, substring(before, pmax(1L, nchar(before) - 14L)), perl = TRUE)
  remove <- rep(FALSE, nrow(toks))

  # runs of 5 or more numbers adding one each, from 30 up, in one sentence
  for (i in unique(toks$s)) {
    k <- which(toks$s == i)
    if (length(k) < 5) next
    v <- toks$value[k]
    gap <- toks$start[k][-1] - (toks$start[k] + toks$len[k])[-length(k)]
    step <- c(FALSE, diff(v) == 1L & gap <= 2L)
    r <- rle(step)
    ends <- cumsum(r$lengths)
    for (q in which(r$values & r$lengths >= 4L)) {
      span <- (ends[q] - r$lengths[q]):ends[q]
      # not a list of years ("2011 2012 2013 2014 2015" in a table)
      years <- all(v[span] >= 1950L & v[span] <= 2050L)
      if (v[span[1]] >= 30L && !years) remove[k[span]] <- TRUE
    }
  }

  # the chain of stand-alone numbers, and how fast it advances
  cand <- which(stand & !remove)
  if (length(cand) >= 15L) {
    v <- toks$value[cand]
    n <- length(v); best <- rep(1L, n); prev_i <- rep(0L, n)
    for (i in seq_len(n)[-1]) {
      w <- max(1L, i - 400L):(i - 1L)
      ok <- w[(v[i] - v[w]) >= 1L & (v[i] - v[w]) <= 10L]
      if (length(ok)) {
        j <- ok[which.max(best[ok])]
        if (best[j] + 1L > best[i]) { best[i] <- best[j] + 1L; prev_i[i] <- j }
      }
    }
    i <- which.max(best); ch <- integer(0)
    while (i > 0L) { ch <- c(i, ch); i <- prev_i[i] }
    if (length(ch) >= 15L) {
      rate <- diff(toks$pos[cand[ch]]) / diff(v[ch])
      line_steps <- sum(rate >= 50 & rate <= 150)
      other <- sum(rate >= 8)
      # Ordinary small numbers ("fixed to 1.", "Diary 2", the rows of a table
      # numbered 1 to 30) can form such a chain too; the line numbers left in
      # a manuscript were above 50 in every case seen
      if (line_steps >= 10L && line_steps >= 0.5 * other && stats::median(v[ch]) >= 50) {
        # Only a number that is a line's distance from its neighbours in the
        # chain, on both sides, also at the chain's two ends: a number of the
        # text that fits the sequence ("between 300 and 420 ms", "for 24
        # weeks", "68 of 250 vaccinated" in an abstract without line numbers,
        # followed by "254 million") was removed with the line numbers around
        # it. A true line number at an end is found by the pass below.
        ok_step <- .line_step_ok(diff(v[ch]), diff(toks$pos[cand[ch]]))
        keep_side <- c(FALSE, ok_step) & c(ok_step, FALSE)
        remove[cand[ch][keep_side]] <- TRUE
      }
    }
  }
  if (!any(remove)) return(text)

  # The numbers found so far are a sure sign of line numbers. From them, one
  # pass through the whole text, across sentences, adds each stand-alone
  # number that fits the nearest line number before or after it: 1 to 10
  # higher (or lower), at a line's distance (.line_step_ok()). A number is not
  # added when a line number on either side is within 10 steps of it but at
  # the wrong distance, when it is an amount ("254 million"), or when a
  # number is right before and after it, as in the row of a table ("1169 159
  # 21.4", "95 (23%) 242 (21%)").
  prev_word <- sub("^.*\\s", "", before)
  next_word <- sub("\\s.*$", "", after)
  amount <- grepl("^(?:thousand|million|billion|trillion)", next_word, perl = TRUE, ignore.case = TRUE)
  in_row <- grepl("\\d%?\\)?$", prev_word, perl = TRUE) & grepl("^\\(?\\.?\\d", next_word, perl = TRUE)
  fits <- function(dv, dpos) {
    near <- dv >= 1L & dv <= 10L
    c(near = near, ok = near && .line_step_ok(dv, dpos))
  }
  for (round in 1:20) {
    added <- FALSE
    for (r in which(stand & !remove & !amount & !in_row)) {
      before <- which(remove & toks$pos < toks$pos[r])
      after <- which(remove & toks$pos > toks$pos[r])
      l <- if (length(before)) fits(toks$value[r] - toks$value[before[length(before)]],
                                    toks$pos[r] - toks$pos[before[length(before)]]) else c(near = FALSE, ok = FALSE)
      a <- if (length(after)) fits(toks$value[after[1]] - toks$value[r],
                                   toks$pos[after[1]] - toks$pos[r]) else c(near = FALSE, ok = FALSE)
      if ((l[["ok"]] || a[["ok"]]) && (!l[["near"]] || l[["ok"]]) && (!a[["near"]] || a[["ok"]])) {
        remove[r] <- TRUE; added <- TRUE
      }
    }
    if (!added) break
  }

  # remove each number, with the space after it (or before it, at the end)
  out <- t
  for (i in unique(toks$s[remove])) {
    k <- rev(which(toks$s == i & remove))
    x <- out[[i]]
    for (r in k) {
      a <- toks$start[r]; b <- a + toks$len[r] - 1L
      if (b < nchar(x) && substr(x, b + 1L, b + 1L) == " ") b <- b + 1L
      else if (a > 1L && substr(x, a - 1L, a - 1L) == " ") a <- a - 1L
      x <- paste0(substr(x, 1L, a - 1L), substring(x, b + 1L))
    }
    out[[i]] <- x
  }
  out[is.na(text)] <- NA_character_
  out
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

#' Clean a paper's sentences of what reading the PDF left wrong
#'
#' Repairs the text table with `.repair_line_numbers()` (a manuscript's line
#' numbers), `.repair_links()` (links split with a space) and
#' `.repair_bib_dois()` (references' DOIs). The url, xref and eq tables
#' give where each item is in its sentence (`start`, counted from 0, and
#' `end`, just after it); these move back by the number of characters removed
#' before them, so a paper written with `paper_write()` stays right.
#'
#' @param paper a paper or paperlist
#' @returns the paper or paperlist
#' @noRd
.paper_repair_text <- function(paper) {
  if (.is_paper_list(paper)) {
    return(paperlist(lapply(paper, .paper_repair_text)))
  }
  tx <- paper$text
  if (!is.data.frame(tx) || !nrow(tx) || !is.character(tx$text)) return(paper)

  old <- tx$text
  # line numbers first: the link rules would join a line number after
  # "osf.io/ " to the link
  new <- .repair_line_numbers(old)
  new <- .repair_links(new)
  # after the rules, so a reference's DOI is joined only where the rules left
  # no whole copy of it. Both only remove spaces; in 17 corpora (4.5 million
  # sentences) the order changed one sentence, where joining first also
  # joined a second copy of the DOI inside another link (checked 2026-11-12)
  if ("text_id" %in% names(tx)) {
    new <- .repair_bib_dois(new, tx$text_id, paper$bib)
  }
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
