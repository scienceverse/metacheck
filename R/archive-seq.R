# Sequence and expression archives (#660): NCBI's GEO, the INSDC archives
# (NCBI's BioProject and SRA, EBI's ENA, DDBJ), and EBI's ArrayExpress.
#
# Papers name these deposits by accession number ("GSE149523", "PRJNA770129",
# "SRR7416431", "E-MTAB-6487") more often than by link. The INSDC archives
# share their records, so ENA's portal answers for all three, also for a run,
# experiment, sample or submission number, which it resolves to its study.
# Tested 2026-11-18 on 60 mentions from 19 corpora (57 records found).
#
# What each record gives the ownership step (R/repo-ownership.R):
#   * GEO: title, contributors by name, submission date, and the PubMed ids
#     of the papers it belongs to (turned into DOIs, so a record that names
#     this paper is the paper's own);
#   * ENA (BioProject, SRA, ENA, DDBJ): title, the submitting institute,
#     first public date; no people;
#   * ArrayExpress (BioStudies): title, authors, release date, and the paper
#     it belongs to (DOI or PubMed id).

# Accession numbers: GEO series; INSDC projects and studies; INSDC runs,
# experiments, samples and submissions (resolved to their study);
# ArrayExpress experiments.
.seq_acc_rx <- c(
  geo = "\\bGSE[0-9]{3,8}\\b",
  ena_study = "\\bPRJ(?:NA|EB|DB)[0-9]{3,9}\\b|\\b[SED]RP[0-9]{6,9}\\b",
  ena_other = "\\b[SED]R[RXSA][0-9]{6,9}\\b|\\bSAM(?:N|EA|D)[0-9]{6,11}\\b",
  arrayexpress = "\\bE-(?!ISSN)[A-Z]{4}-[0-9]{1,7}\\b"
)

# Words of a sentence that deposits data rather than reuses it, to look these
# sentences up first when a paper names many accessions (see seq_archive_links()).
.seq_deposit_words <- "(?i)deposit|submitted|generated (in|for|during) (this|the present|the current)|have been (made )?available|our (data|sequenc)|we (deposited|submitted|uploaded)"

# The address of an accession's record, by archive
.seq_url <- function(acc, type) {
  ifelse(type == "geo", paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", acc),
  ifelse(type == "arrayexpress", paste0("https://www.ebi.ac.uk/biostudies/arrayexpress/studies/", acc),
         paste0("https://www.ebi.ac.uk/ena/browser/view/", acc)))
}

# The accession in a link made by .seq_url() (or written in a paper)
.seq_acc_of <- function(url) {
  rx <- paste0("(?:", paste(.seq_acc_rx, collapse = "|"), ")")
  m <- regmatches(url, regexpr(rx, url, perl = TRUE))
  out <- rep(NA_character_, length(url))
  out[grepl(rx, url, perl = TRUE)] <- m
  out
}

#' Find sequence and expression archive deposits in a paper
#'
#' Finds deposits in NCBI's Gene Expression Omnibus (GEO series, `GSE...`),
#' the INSDC archives (BioProject `PRJNA...`, `PRJEB...`, `PRJDB...`; SRA, ENA
#' and DDBJ studies `SRP...`, `ERP...`, `DRP...`; and runs, experiments,
#' samples and submissions, which are looked up as their study), and EBI's
#' ArrayExpress (`E-MTAB-...` and the like), by accession number or link.
#' A sentence that names a project or study is represented by those; a run or
#' sample number counts only in a sentence that names no study, and then only
#' the first one, as a sentence's runs belong to one study. A paper that names
#' more than `max_n` accessions keeps the ones in sentences that deposit data
#' first.
#'
#' @param paper a paper object or paperlist object
#' @param max_n the largest number of accessions kept per paper
#'
#' @returns a data frame with a row for each deposit: `href` (the record's
#'   address), `accession`, `repo_type` (`geo`, `ena` or `arrayexpress`),
#'   and `text_id` / `paper_id`
#' @export
#'
#' @examples
#' paper <- test_paper(text = "RNA-seq data are deposited in GEO (GSE149523).")
#' seq_archive_links(paper)
seq_archive_links <- function(paper, max_n = 25) {
  none <- data.frame(paper_id = character(0), href = character(0), accession = character(0),
                     repo_type = character(0), text_id = integer(0))
  rx <- paste0("(?:", paste(.seq_acc_rx, collapse = "|"), ")")
  hits <- text_search(paper, rx, return = "sentence", perl = TRUE, ignore.case = FALSE)
  if (!is.data.frame(hits) || !nrow(hits)) return(none)
  if (!"paper_id" %in% names(hits)) hits$paper_id <- NA_character_
  rows <- lapply(seq_len(nrow(hits)), function(i) {
    s <- hits$text[[i]]
    get <- function(k) unique(regmatches(s, gregexpr(.seq_acc_rx[[k]], s, perl = TRUE))[[1]])
    acc <- c(get("geo"), get("ena_study"), get("arrayexpress"))
    type <- c(rep("geo", length(get("geo"))), rep("ena", length(get("ena_study"))),
              rep("arrayexpress", length(get("arrayexpress"))))
    if (!length(get("ena_study"))) {
      o <- get("ena_other")
      if (length(o)) { acc <- c(acc, o[[1]]); type <- c(type, "ena") }
    }
    if (!length(acc)) return(NULL)
    data.frame(paper_id = hits$paper_id[[i]], text_id = hits$text_id[[i]] %||% NA_integer_,
               accession = acc, repo_type = type,
               deposit = grepl(.seq_deposit_words, s, perl = TRUE))
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(none)
  out <- out[order(out$paper_id, !out$deposit), , drop = FALSE]
  out <- out[!duplicated(out[c("paper_id", "accession")]), , drop = FALSE]
  out <- dplyr::bind_rows(lapply(split(out, out$paper_id), utils::head, max_n))
  out$href <- .seq_url(out$accession, out$repo_type)
  out$deposit <- NULL
  out <- out[, c("paper_id", "href", "accession", "repo_type", "text_id")]
  rownames(out) <- NULL
  out
}

# A text answer from an archive (NULL when it fails)
.seq_get <- function(url) {
  r <- tryCatch(.mc_request(url) |> httr2::req_error(is_error = \(e) FALSE) |>
                  httr2::req_timeout(60) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform(),
                error = \(e) NULL)
  if (is.null(r) || httr2::resp_status(r) != 200) return(NULL)
  httr2::resp_body_string(r)
}

# PubMed ids -> DOIs (one request for all)
.seq_pubmed_dois <- function(pmids) {
  pmids <- unique(pmids[!is.na(pmids) & grepl("^[0-9]+$", pmids)])
  if (!length(pmids)) return(character(0))
  txt <- .seq_get(paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&retmode=json&id=",
                         paste(pmids, collapse = ",")))
  j <- tryCatch(jsonlite::fromJSON(txt %||% "", simplifyVector = FALSE)$result, error = \(e) NULL)
  if (is.null(j)) return(character(0))
  dois <- vapply(pmids, function(id) {
    ids <- j[[id]]$articleids %||% list()
    d <- unlist(lapply(ids, \(a) if (identical(a$idtype, "doi")) a$value else NULL))
    if (length(d)) tolower(d[[1]]) else NA_character_
  }, character(1))
  dois[!is.na(dois)]
}

# "Sep 24 2024" or "2024-09-24..." -> "2024-09-24"
.seq_date <- function(x) {
  x <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(x, format = "%b %d %Y"))
  iso <- suppressWarnings(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  out <- ifelse(!is.na(d), format(d), ifelse(!is.na(iso), format(iso), NA_character_))
  as.character(out)
}

# One GEO series -> one row
.seq_geo_info <- function(url) {
  acc <- .seq_acc_of(url)
  obj <- data.frame(seq_url = url, accession = acc, repo_type = "geo", title = NA_character_,
                    creators = NA_character_, organisation = NA_character_, publication_date = NA_character_,
                    description = NA_character_, resource_type = "Dataset", related = NA_character_,
                    error = NA_character_)
  txt <- .seq_get(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", acc,
                         "&targ=self&form=text&view=brief"))
  if (is.null(txt) || !grepl("!Series_title", txt, fixed = TRUE)) { obj$error <- "unfound"; return(obj) }
  tag <- function(t) {
    m <- regmatches(txt, gregexpr(paste0("(?m)^!", t, " = (.*)$"), txt, perl = TRUE))[[1]]
    unique(trimws(sub(paste0("^!", t, " = "), "", m)))
  }
  # "Manish,V,Bais" -> "Manish Bais"
  people <- unique(c(tag("Series_contributor"), tag("Series_contact_name")))
  people <- vapply(strsplit(people, ","), \(p) paste(p[nzchar(p)][c(1, length(p[nzchar(p)]))], collapse = " "), "")
  obj$title <- tag("Series_title")[1] %||% NA_character_
  obj$creators <- if (length(people)) paste(unique(people), collapse = "; ") else NA_character_
  obj$organisation <- paste(tag("Series_contact_institute"), collapse = "; ")
  obj$publication_date <- .seq_date(tag("Series_submission_date")[1] %||% NA)
  obj$description <- substr(paste(tag("Series_summary"), collapse = " "), 1, 2000)
  dois <- .seq_pubmed_dois(tag("Series_pubmed_id"))
  if (length(dois)) obj$related <- paste(paste0("IsSupplementTo:", dois), collapse = " | ")
  obj
}

# One INSDC study (or a run, experiment, sample or submission of it) -> one row
.seq_ena_info <- function(url) {
  acc <- .seq_acc_of(url)
  obj <- data.frame(seq_url = url, accession = acc, repo_type = "ena", title = NA_character_,
                    creators = NA_character_, organisation = NA_character_, publication_date = NA_character_,
                    description = NA_character_, resource_type = "Dataset", related = NA_character_,
                    study = NA_character_, error = NA_character_)
  portal <- "https://www.ebi.ac.uk/ena/portal/api/"
  study <- acc
  if (!grepl(.seq_acc_rx[["ena_study"]], acc, perl = TRUE)) {
    # a run, experiment, sample or submission: its study
    q <- if (grepl("^[SED]RA", acc)) paste0("search?result=read_run&query=submission_accession=%22", acc, "%22")
         else paste0("filereport?accession=", acc, "&result=read_run")
    t <- .seq_get(paste0(portal, q, "&fields=study_accession&limit=1&format=tsv"))
    tab <- tryCatch(utils::read.delim(text = t %||% "", colClasses = "character"), error = \(e) NULL)
    study <- if (is.data.frame(tab) && nrow(tab)) tab$study_accession[[1]] else NA_character_
    if (is.na(study)) { obj$error <- "unfound"; return(obj) }
  }
  t <- .seq_get(paste0(portal, "search?result=study&query=study_accession=%22", study,
                       "%22%20OR%20secondary_study_accession=%22", study,
                       "%22&fields=study_accession,secondary_study_accession,study_title,study_description,center_name,first_public&format=tsv"))
  tab <- tryCatch(utils::read.delim(text = t %||% "", colClasses = "character", quote = ""), error = \(e) NULL)
  if (!is.data.frame(tab) || !nrow(tab)) { obj$error <- "unfound"; return(obj) }
  obj$study <- tab$study_accession[[1]]
  obj$title <- tab$study_title[[1]]
  obj$description <- substr(tab$study_description[[1]] %||% NA_character_, 1, 2000)
  obj$organisation <- tab$center_name[[1]]
  obj$publication_date <- .seq_date(tab$first_public[[1]])
  obj
}

# One ArrayExpress experiment (a BioStudies study) -> one row
.seq_arrayexpress_info <- function(url) {
  acc <- .seq_acc_of(url)
  obj <- data.frame(seq_url = url, accession = acc, repo_type = "arrayexpress", title = NA_character_,
                    creators = NA_character_, organisation = NA_character_, publication_date = NA_character_,
                    description = NA_character_, resource_type = "Dataset", related = NA_character_,
                    error = NA_character_)
  txt <- .seq_get(paste0("https://www.ebi.ac.uk/biostudies/api/v1/studies/", acc))
  j <- tryCatch(jsonlite::fromJSON(txt %||% "", simplifyVector = FALSE), error = \(e) NULL)
  if (is.null(j)) { obj$error <- "unfound"; return(obj) }
  # attributes are name/value pairs, on the study and in its subsections
  attrs <- function(a) vapply(a %||% list(), \(x) paste0(x$name %||% "", "\r", x$value %||% ""), "")
  val <- function(a, nm) { a <- attrs(a); sub("^[^\r]*\r", "", a[startsWith(a, paste0(nm, "\r"))]) }
  sec <- j$section %||% list()
  subs <- sec$subsections %||% list()
  flat <- list()
  walk <- function(s) {
    if (is.null(s)) return()
    if (!is.null(s$type)) flat[[length(flat) + 1]] <<- s
    else if (is.list(s)) for (e in s) walk(e)
  }
  walk(subs)
  type_of <- vapply(flat, \(s) tolower(s$type %||% ""), "")
  authors <- unlist(lapply(flat[type_of == "author"], \(s) val(s$attributes, "Name")))
  orgs <- unlist(lapply(flat[type_of %in% c("organization", "organisation")], \(s) val(s$attributes, "Name")))
  pubs <- flat[type_of == "publication"]
  obj$title <- val(sec$attributes, "Title")[1] %||% val(j$attributes, "Title")[1] %||% NA_character_
  obj$description <- substr(val(sec$attributes, "Description")[1] %||% NA_character_, 1, 2000)
  obj$creators <- if (length(authors)) paste(unique(authors), collapse = "; ") else NA_character_
  obj$organisation <- if (length(orgs)) paste(unique(orgs), collapse = "; ") else NA_character_
  obj$publication_date <- .seq_date(val(j$attributes, "ReleaseDate")[1] %||% NA)
  dois <- unlist(lapply(pubs, \(s) tolower(val(s$attributes, "DOI"))))
  dois <- c(dois, .seq_pubmed_dois(unlist(lapply(pubs, \(s) val(s$attributes, "PubMed ID")))))
  dois <- unique(dois[!is.na(dois) & nzchar(dois)])
  if (length(dois)) obj$related <- paste(paste0("IsSupplementTo:", dois), collapse = " | ")
  obj
}

#' Get the records of sequence and expression archive deposits
#'
#' Reads the record of each deposit [seq_archive_links()] finds: a GEO
#' series from GEO, an INSDC study (BioProject, SRA, ENA, DDBJ) from ENA's
#' portal, an ArrayExpress experiment from BioStudies. A run, experiment,
#' sample or submission number is read as its study.
#'
#' @param seq_url the records' addresses, as [seq_archive_links()] gives them
#'   in `href`, or a data frame with them in column `id_col`
#' @param id_col the column of `seq_url` holding the addresses, if it is a
#'   data frame
#' @param pb a progress bar, or NULL
#' @param cache whether to keep each answer for the session (see
#'   `.repo_info_cache_get()`)
#'
#' @returns a data frame with a row per address: `seq_url`, `accession`,
#'   `repo_type`, `title`, `creators` (names, separated by "; "),
#'   `organisation` (the submitting institute), `publication_date`,
#'   `description`, `resource_type`, `related` (`IsSupplementTo:<DOI>` for
#'   each paper the record belongs to), and `error` ("unfound" when the
#'   archive has no such record)
#' @export
#'
#' @examples
#' \donttest{
#' if (online("www.ebi.ac.uk")) seq_archive_info("https://www.ebi.ac.uk/ena/browser/view/PRJNA770129")
#' }
seq_archive_info <- function(seq_url, id_col = 1, pb = NULL, cache = FALSE) {
  urls <- if (is.data.frame(seq_url)) seq_url[[id_col]] else seq_url
  urls <- unique(stats::na.omit(as.character(urls)))
  urls <- urls[nzchar(urls)]
  if (!length(urls)) {
    return(data.frame(seq_url = character(0), accession = character(0), repo_type = character(0),
                      title = character(0), creators = character(0), organisation = character(0),
                      publication_date = character(0), description = character(0),
                      resource_type = character(0), related = character(0), error = character(0)))
  }
  if (is.null(pb)) {
    pb <- pb(NA, "(:spin) :what")
    on.exit(pb$terminate())
  }
  rows <- lapply(urls, function(u) {
    cached <- if (isTRUE(cache)) .repo_info_cache_get("seq", u) else NULL
    if (!is.null(cached)) return(cached)
    pb$tick(0, list(what = paste0("* Retrieving the archive record ", .seq_acc_of(u), "...")))
    res <- tryCatch(
      if (grepl("ncbi\\.nlm\\.nih\\.gov/geo/", u)) .seq_geo_info(u)
      else if (grepl("biostudies|arrayexpress", u)) .seq_arrayexpress_info(u)
      else .seq_ena_info(u),
      error = \(e) data.frame(seq_url = u, accession = .seq_acc_of(u), repo_type = NA_character_,
                              error = conditionMessage(e)))
    if (isTRUE(cache) && .repo_info_ok(res)) .repo_info_cache_put("seq", u, res)
    res
  })
  dplyr::bind_rows(rows)
}

# The size of a GEO series' supplementary files, from its folder listing
# ("550K", "1.2G"), in bytes; NA when the listing cannot be read
.seq_geo_suppl_bytes <- function(acc) {
  num <- sub("^GSE", "", acc)
  stub <- if (nchar(num) > 3) paste0("GSE", substr(num, 1, nchar(num) - 3), "nnn") else "GSEnnn"
  html <- .seq_get(paste0("https://ftp.ncbi.nlm.nih.gov/geo/series/", stub, "/", acc, "/suppl/"))
  if (is.null(html)) return(NA_real_)
  m <- regmatches(html, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2} +[0-9.]+[KMGT]?", html))[[1]]
  if (!length(m)) return(0)
  s <- sub("^.* +", "", m)
  mult <- c(K = 1e3, M = 1e6, G = 1e9, T = 1e12)
  u <- sub("^[0-9.]+", "", s)
  sum(as.numeric(sub("[KMGT]$", "", s)) * ifelse(nzchar(u), mult[u], 1), na.rm = TRUE)
}

#' The size of sequence and expression archive deposits
#'
#' How much each deposit holds, without downloading it: an INSDC study's
#' runs and their total size (ENA's file report), a GEO series' samples and
#' the size of its supplementary files, an ArrayExpress experiment's files.
#' repo_check lists the authors' own deposits with this, and does not
#' download them (#660).
#'
#' @param info a table from [seq_archive_info()] (`seq_url`, `accession`,
#'   `repo_type`, and `study` for INSDC records)
#'
#' @returns `info`'s `seq_url` with `units` (the number of runs, samples or
#'   files), `unit` ("runs", "samples" or "files") and `bytes` (NA when the
#'   archive does not say)
#' @export
#'
#' @examples
#' \donttest{
#' if (online("www.ebi.ac.uk")) {
#'   i <- seq_archive_info("https://www.ebi.ac.uk/ena/browser/view/PRJNA770129")
#'   seq_archive_size(i)
#' }
#' }
seq_archive_size <- function(info) {
  none <- data.frame(seq_url = character(0), units = integer(0), unit = character(0), bytes = numeric(0))
  if (!is.data.frame(info) || !nrow(info)) return(none)
  rows <- lapply(seq_len(nrow(info)), function(i) {
    u <- info$seq_url[[i]]; acc <- info$accession[[i]]; type <- info$repo_type[[i]] %||% NA_character_
    out <- data.frame(seq_url = u, units = NA_integer_, unit = NA_character_, bytes = NA_real_)
    if (type %in% "ena") {
      study <- if ("study" %in% names(info) && !is.na(info$study[[i]])) info$study[[i]] else acc
      t <- .seq_get(paste0("https://www.ebi.ac.uk/ena/portal/api/filereport?accession=", study,
                           "&result=read_run&fields=run_accession,fastq_bytes,submitted_bytes&format=tsv"))
      tab <- tryCatch(utils::read.delim(text = t %||% "", colClasses = "character"), error = \(e) NULL)
      if (is.data.frame(tab)) {
        b <- ifelse(nzchar(tab$fastq_bytes %||% ""), tab$fastq_bytes, tab$submitted_bytes %||% "")
        out$units <- nrow(tab); out$unit <- "runs"
        out$bytes <- sum(vapply(strsplit(b, ";"), \(x) sum(suppressWarnings(as.numeric(x)), na.rm = TRUE), 1))
      }
    } else if (type %in% "geo") {
      t <- .seq_get(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", acc, "&targ=self&form=text&view=brief"))
      if (!is.null(t)) {
        out$units <- length(gregexpr("(?m)^!Series_sample_id", t, perl = TRUE)[[1]])
        out$unit <- "samples"
        out$bytes <- .seq_geo_suppl_bytes(acc)
      }
    } else if (type %in% "arrayexpress") {
      j <- tryCatch(jsonlite::fromJSON(.seq_get(paste0("https://www.ebi.ac.uk/biostudies/api/v1/studies/", acc,
                                                       "/info")) %||% ""), error = \(e) NULL)
      if (!is.null(j$files)) { out$units <- as.integer(j$files); out$unit <- "files" }
    }
    out
  })
  dplyr::bind_rows(rows)
}
