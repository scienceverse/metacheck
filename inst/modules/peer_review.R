#' Peer Review
#'
#' @description
#' Find the peer review of an article: whether it is public, where it can be read, and the reviews themselves when their text is available.
#'
#' @details
#' The Peer Review module looks for the documents of an article's peer review (reviewer reports, decision letters and editors' reports, author responses) in four places:
#'
#' 1. the publisher's XML, when the paper was read from it and the publisher prints the reviews with the article (eLife, F1000, part of PLOS): these come with their text;
#' 2. Crossref, where publishers register a review as a record of its own, linked to the article (Wiley, Copernicus, the Royal Society of Chemistry, eLife, F1000, PeerJ, the Royal Society and others): what each document is, its round, date and reviewer when named, and an address; not its text (see [crossref_peer_reviews()]);
#' 3. Europe PMC's XML of the article, for an article in PubMed Central whose publisher printed the reviews, also when the paper was read from a PDF: with their text;
#' 4. the paper's own text: a sentence that says where the peer review history, reports or file can be found, with a link.
#'
#' It also asks PubPeer for comments on the article itself, as review after publication (Statcheck's automatic comments are left out).
#'
#' A single paper's report says whether the peer review is public, lists the addresses where it can be read with the documents at each, and shows each review whose text is available in a box that folds out. For reviews on Web of Science (earlier on Publons: Wiley, the Royal Society, the Royal Society of Chemistry, SAGE, IOP and others) the address is given and the text is not shown.
#'
#' The module does not judge the paper: the traffic light is "info" when a review or a comment was found, and "na" when none was.
#'
#' @keywords general
#'
#' @author Daniel Lakens (\email{d.lakens@tue.nl})
#'
#' @param paper a paper object or paperlist object
#' @param lookup whether to ask Crossref, Europe PMC and PubPeer; FALSE uses only what the paper itself holds
#'
#' @returns a list
#'
peer_review <- function(paper, lookup = TRUE) {
  # nothing to check in an empty paper or list: "na"
  if (.paper_is_empty(paper)) return(.empty_input_result(paper))

  ids <- paper_id(paper)
  papers <- if (.is_paper_list(paper)) paper else list(paper)
  found <- lapply(seq_along(papers), \(i) .peer_review_of(papers[[i]], ids[[i]], lookup))

  # table: one row per document ----
  table <- dplyr::bind_rows(lapply(found, `[[`, "docs"))
  texts <- dplyr::bind_rows(lapply(found, `[[`, "texts"))
  not_asked <- unlist(lapply(found, `[[`, "not_asked"))

  # summary_table ----
  summary_table <- dplyr::bind_rows(lapply(seq_along(found), \(i) {
    d <- found[[i]]$docs
    where <- unique(stats::na.omit(d$where))
    data.frame(
      paper_id = ids[[i]],
      peer_review_open = nrow(d) > 0,
      review_n = nrow(d),
      review_text_n = sum(d$has_text),
      peer_review_where = if (length(where)) paste(where, collapse = "; ") else NA_character_,
      pubpeer_comments = found[[i]]$pubpeer$n,
      pubpeer_url = found[[i]]$pubpeer$url)
  }))

  # traffic light ----
  any_found <- nrow(table) > 0 || any(summary_table$pubpeer_comments > 0, na.rm = TRUE)
  tl <- if (any_found) "info" else if (length(not_asked)) "fail" else "na"

  # summary_text ----
  n <- nrow(summary_table)
  n_open <- sum(summary_table$peer_review_open)
  summary_text <- if (n == 1) {
    if (n_open) {
      sprintf("The peer review of this article is public: we found %d document%s%s.", nrow(table), plural(nrow(table)),
              if (sum(table$has_text)) sprintf(", %d with %s text", sum(table$has_text),
                                              plural(sum(table$has_text), "its", "their")) else "")
    } else if (length(not_asked)) {
      sprintf("We could not ask %s, and the paper itself shows no public peer review.", paste(not_asked, collapse = " and "))
    } else {
      "We found no public peer review for this article."
    }
  } else {
    sprintf("%d of %d papers have public peer review.", n_open, n)
  }
  pp_n <- sum(summary_table$pubpeer_comments, na.rm = TRUE)
  if (pp_n > 0) {
    summary_text <- paste(summary_text, sprintf("PubPeer has %d comment%s on %s.", pp_n, plural(pp_n),
                                                if (n == 1) "it" else "these papers"))
  }

  # report ----
  report <- NULL
  if (n == 1) {
    report <- summary_text
    if (nrow(table)) {
      # a row per address: the documents that can be read there (`table`
      # has a row per document)
      at <- ifelse(is.na(table$where), "", table$where)
      span <- function(x) {
        x <- unique(x[!is.na(x)])
        if (!length(x)) "" else if (length(x) == 1) as.character(x) else paste(min(x), "to", max(x))
      }
      shown <- do.call(rbind, lapply(unique(at), \(w) {
        d <- table[at == w, , drop = FALSE]
        data.frame(
          Documents = .peer_review_count(d$review_type),
          Round = span(d$round),
          Date = span(d$date),
          Reviewer = paste(unique(d$reviewer[!is.na(d$reviewer)]), collapse = "; "),
          Where = if (grepl("^https?://", w)) {
            sprintf("<a href='%s' target='_blank'>%s</a>", w, sub("^https?://", "", w))
          } else w)
      }))
      report <- c(report, scroll_table(shown, maxrows = 12))
      if (any(grepl("publons\\.com|webofscience\\.com", table$where))) {
        report <- c(report, paste(
          "These reviews are on Web of Science (earlier on Publons):",
          "we give their address and do not show their text."))
      }
      # each review with its text, in a box that folds out
      for (k in which(table$has_text)) {
        body <- texts$text[texts$paper_id %in% table$paper_id[k] & texts$review_id %in% table$review_id[k]]
        title <- paste0(.peer_review_label(table$review_type[k]),
                        if (!is.na(table$round[k])) sprintf(", round %d", table$round[k]) else "",
                        if (!is.na(table$reviewer[k])) sprintf(", by %s", table$reviewer[k]) else "",
                        if (!is.na(table$date[k])) sprintf(" (%s)", table$date[k]) else "")
        report <- c(report, collapse_section(body, title = title, callout = "note"))
      }
    }
    if (pp_n > 0) {
      report <- c(report, sprintf("Comments after publication: <a href='%s' target='_blank'>PubPeer</a>.",
                                  summary_table$pubpeer_url))
    }
    if (length(not_asked) && n_open) {
      report <- c(report, sprintf("We could not ask %s, so documents may be missing.", paste(not_asked, collapse = " and ")))
    }
  }

  # return a list ----
  list(
    table = table,
    # the paragraphs of the reviews whose text is available: paper_id,
    # review_id (as in `table`), paragraph_id and text
    review_text = texts,
    summary_table = summary_table,
    na_replace = list(peer_review_open = FALSE, review_n = 0L, review_text_n = 0L),
    traffic_light = tl,
    summary_text = summary_text,
    report = report
  )
}

#' The words for a kind of peer review document
#' @noRd
.peer_review_label <- function(type) {
  labels <- c(referee_report = "Reviewer report", decision_letter = "Decision letter",
              editor_report = "Editor's report", author_response = "Author response",
              assessment = "Assessment", acceptance_letter = "Acceptance letter",
              history = "Peer review history")
  out <- unname(labels[type])
  out[is.na(out)] <- "Peer review document"
  out
}

#' The documents at one address, in words
#'
#' One document by its kind ("Reviewer report"); several counted by kind
#' ("4 reviewer reports, 4 decision letters"), in the order given.
#' @noRd
.peer_review_count <- function(type) {
  lab <- .peer_review_label(type)
  if (length(lab) == 1) return(lab)
  kinds <- unique(lab)
  n <- vapply(kinds, \(k) sum(lab == k), integer(1))
  many <- paste0(sub("y$", "ie", tolower(kinds)), "s")
  paste(sprintf("%d %s", n, ifelse(n == 1, tolower(kinds), many)), collapse = ", ")
}

#' The peer review documents of one paper
#'
#' @param p a paper object
#' @param id its paper id
#' @param lookup whether to ask Crossref, Europe PMC and PubPeer
#' @returns a list: `docs` (a row per document: paper_id, review_id,
#'   review_type, stage, round, date, reviewer, title, doi, where, source,
#'   has_text), `texts` (paper_id, review_id, paragraph_id, text), `pubpeer`
#'   (n and url) and `not_asked` (the services that did not answer)
#' @noRd
.peer_review_of <- function(p, id, lookup = TRUE) {
  cols <- c("review_type", "stage", "round", "date", "reviewer", "title", "doi", "where", "source")
  docs <- data.frame(review_type = character(0), stage = character(0), round = integer(0), date = character(0),
                     reviewer = character(0), title = character(0), doi = character(0), where = character(0),
                     source = character(0), key = integer(0))
  texts <- data.frame(key = integer(0), paragraph_id = integer(0), text = character(0))
  not_asked <- character(0)
  chr <- function(x, n) if (is.null(x)) rep(NA_character_, n) else as.character(x)

  # 1. the reviews the publisher's XML prints or links to
  rv <- p$review
  if (is.data.frame(rv) && nrow(rv)) {
    n <- nrow(rv)
    ct <- p$contributor
    who <- vapply(seq_len(n), \(k) {
      cid <- unlist(rv$contributor_ids[[k]] %||% integer(0))
      if (!length(cid) || !is.data.frame(ct)) return(NA_character_)
      r <- ct[ct$contributor_id %in% cid, , drop = FALSE]
      nm <- trimws(paste(ifelse(is.na(r$given), "", r$given), ifelse(is.na(r$family), r$literal %||% "", r$family)))
      nm <- nm[nzchar(nm) & !is.na(nm)]
      if (length(nm)) paste(nm, collapse = "; ") else NA_character_
    }, character(1))
    doi <- chr(rv[["doi"]], n)
    docs <- data.frame(
      review_type = chr(rv$review_type, n), stage = "pre-publication",
      round = suppressWarnings(as.integer(rv[["round"]] %||% rep(NA, n))), date = chr(rv[["date"]], n),
      reviewer = who, title = chr(rv[["title"]], n), doi = tolower(doi),
      where = ifelse(!is.na(doi), paste0("https://doi.org/", doi), chr(rv[["url"]], n)),
      source = "xml", key = rv$review_id)
    # a link to the whole history is one document of its own kind
    docs$review_type[docs$review_type %in% "other" & is.na(docs$doi) & !is.na(docs$where)] <- "history"
    rt <- p$review_text
    if (is.data.frame(rt) && nrow(rt)) {
      texts <- data.frame(key = rt$review_id, paragraph_id = rt$paragraph_id, text = rt$text)
      docs$where[is.na(docs$where) & docs$key %in% texts$key] <- "in the article's XML"
    }
  }

  # 2. a sentence of the paper that says where the reviews are
  said <- .peer_review_sentence_links(p)
  said <- said[!tolower(sub("/+$", "", said)) %in% tolower(sub("/+$", "", stats::na.omit(docs$where)))]
  if (length(said)) {
    docs <- dplyr::bind_rows(docs, data.frame(
      review_type = "history", stage = "pre-publication", where = said, source = "text",
      key = max(c(0L, docs$key)) + seq_along(said)))
  }

  article_doi <- as.character(p$info[["doi"]] %||% NA_character_)[1]
  pubpeer <- list(n = NA_integer_, url = NA_character_)
  if (isTRUE(lookup) && !is.na(article_doi) && nzchar(article_doi)) {
    # 3. the documents Crossref registers for the article
    cr <- tryCatch(crossref_peer_reviews(article_doi), error = \(e) NULL)
    if (is.null(cr)) {
      not_asked <- c(not_asked, "Crossref")
    } else if (nrow(cr)) {
      m <- match(cr$doi, docs$doi)
      both <- which(!is.na(m))
      if (length(both)) {
        docs$source[m[both]] <- "xml, crossref"
        docs$reviewer[m[both]] <- ifelse(is.na(docs$reviewer[m[both]]), cr$reviewer[both], docs$reviewer[m[both]])
        docs$date[m[both]] <- ifelse(is.na(docs$date[m[both]]), cr$date[both], docs$date[m[both]])
        docs$round[m[both]] <- ifelse(is.na(docs$round[m[both]]), cr$round[both], docs$round[m[both]])
      }
      new <- cr[is.na(m), , drop = FALSE]
      if (nrow(new)) {
        docs <- dplyr::bind_rows(docs, data.frame(
          review_type = new$review_type, stage = new$stage, round = new$round, date = new$date,
          reviewer = new$reviewer, title = new$title, doi = new$doi,
          where = ifelse(is.na(new$url), paste0("https://doi.org/", new$doi), new$url),
          source = "crossref", key = max(c(0L, docs$key)) + seq_len(nrow(new))))
        # the link to the whole history is not a document next to the
        # documents themselves, which are on the same website
        host <- function(u) sub("^https?://(www\\.)?([^/]+).*$", "\\2", u)
        docs <- docs[!(docs$review_type %in% "history" & host(docs$where) %in% host(new$url)), , drop = FALSE]
      }
    }

    # 4. the reviews Europe PMC's XML prints, when no text is at hand
    if (!nrow(texts)) {
      ep <- .europepmc_peer_reviews(article_doi)
      if (!is.null(ep) && nrow(ep$review)) {
        for (k in seq_len(nrow(ep$review))) {
          e <- ep$review[k, ]
          at <- if (!is.na(e$doi)) match(tolower(e$doi), docs$doi) else NA_integer_
          # without a DOI in the XML: the document of the same kind that
          # Crossref registered and that has no text yet
          if (is.na(at)) {
            same <- which(docs$review_type %in% e$review_type & !docs$key %in% texts$key)
            if (length(same)) at <- same[[1]]
          }
          if (is.na(at)) {
            docs <- dplyr::bind_rows(docs, data.frame(
              review_type = e$review_type, stage = "pre-publication", round = e$round, date = e$date,
              title = e$title, doi = tolower(e$doi),
              where = if (!is.na(e$doi)) paste0("https://doi.org/", e$doi) else
                paste0("https://europepmc.org/article/PMC/", ep$pmcid),
              source = "europepmc", key = max(c(0L, docs$key)) + 1L))
            at <- nrow(docs)
          } else {
            docs$source[at] <- paste0(docs$source[at], ", europepmc")
          }
          tk <- ep$review_text[ep$review_text$review_id %in% e$review_id, , drop = FALSE]
          if (nrow(tk)) texts <- rbind(texts, data.frame(key = docs$key[at], paragraph_id = tk$paragraph_id, text = tk$text))
        }
      }
    }

    # comments after publication, on the article itself
    pp <- tryCatch(pubpeer_comments(article_doi), error = \(e) NULL)
    if (is.null(pp)) {
      not_asked <- c(not_asked, "PubPeer")
    } else if (nrow(pp) && "total_comments" %in% names(pp)) {
      users <- trimws(unlist(strsplit(as.character(pp$users[[1]] %||% ""), ",")))
      n_pp <- as.integer(pp$total_comments[[1]]) - ("statcheck" %in% tolower(users))
      pubpeer <- list(n = n_pp, url = if (n_pp > 0) as.character(pp[["url"]][[1]] %||% NA_character_) else NA_character_)
    } else {
      pubpeer$n <- 0L
    }
  }

  # in the order of the review: round, then date
  docs <- docs[order(docs$round, docs$date, na.last = TRUE), , drop = FALSE]
  docs$review_id <- seq_len(nrow(docs))
  docs$has_text <- docs$key %in% texts$key
  texts$review_id <- docs$review_id[match(texts$key, docs$key)]
  for (cc in setdiff(cols, names(docs))) docs[[cc]] <- rep(NA_character_, nrow(docs))
  docs <- cbind(paper_id = rep(id, nrow(docs)),
                docs[, c("review_id", cols, "has_text"), drop = FALSE])
  rownames(docs) <- NULL
  texts <- data.frame(paper_id = rep(id, nrow(texts)), review_id = texts$review_id,
                      paragraph_id = texts$paragraph_id, text = texts$text)
  list(docs = docs, texts = texts, pubpeer = pubpeer, not_asked = not_asked)
}

#' The links in sentences that say where an article's peer review can be read
#'
#' A sentence about the record of the review (its history, reports, file)
#' that holds a link; not in the reference list.
#'
#' @param p a paper object
#' @returns the links, a character vector
#' @noRd
.peer_review_sentence_links <- function(p) {
  tx <- p$text
  if (!is.data.frame(tx) || !nrow(tx) || !"text" %in% names(tx)) return(character(0))
  se <- p$section
  type <- if (is.data.frame(se) && "section_id" %in% names(tx)) {
    se$section_type[match(tx$section_id, se$section_id)]
  } else rep(NA_character_, nrow(tx))
  record <- paste0(
    "peer[- ]review(ed|ers?)?[^.]{0,40}\\b(histor(y|ies)|reports?|files?|records?|comments|information)\\b|",
    "\\b(referee|reviewer|review)s?'? reports?\\b")
  hit <- which(grepl(record, tx$text, ignore.case = TRUE, perl = TRUE) & !type %in% "references")
  if (!length(hit)) return(character(0))
  u <- p$url
  linked <- if (is.data.frame(u) && nrow(u) && all(c("href", "text_id") %in% names(u))) {
    u$href[u$text_id %in% tx$text_id[hit]]
  } else character(0)
  printed <- unlist(regmatches(tx$text[hit], gregexpr("https?://[^[:space:]<>\"]+", tx$text[hit])))
  links <- unique(sub("[.,;:)]+$", "", c(linked, printed)))
  links[!is.na(links) & nzchar(links)]
}
