# Publisher XML: what a JATS file declares beyond the text ----
#
# bibr export schema 12.6 has room for what a publisher's XML declares and a
# PDF does not print in a form a parser can rely on: dates, copyright, counts,
# statements, subject terms (open-science badges, assessment terms), related
# articles, editors and reviewers, supplementary files, peer reviews,
# funders with their registry DOIs, CRediT roles, ROR IDs, figure text
# alternatives, table header rows, data citations and declared section
# types. `.jats_extras()` reads them from the JATS document;
# `.jats_apply_extras()` adds them to the paper `.grobid_to_bibr12()` made
# from the TEI translation (see R/import-jats.R).

#' A JATS date element as ISO 8601 (YYYY, YYYY-MM or YYYY-MM-DD)
#' @noRd
.jats_iso_date <- function(node) {
  if (inherits(node, "xml_missing")) return(NA_character_)
  get <- function(x) {
    n <- xml2::xml_find_first(node, paste0("./", x))
    if (inherits(n, "xml_missing")) NA_character_ else trimws(xml2::xml_text(n))
  }
  y <- get("year")
  if (is.na(y) || !grepl("^[0-9]{4}$", y)) {
    iso <- xml2::xml_attr(node, "iso-8601-date")
    if (!is.na(iso) && grepl("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?", iso)) {
      return(regmatches(iso, regexpr("^[0-9]{4}(-[0-9]{2}(-[0-9]{2})?)?", iso)))
    }
    return(NA_character_)
  }
  mo <- suppressWarnings(as.integer(get("month")))
  da <- suppressWarnings(as.integer(get("day")))
  if (is.na(mo) || mo < 1 || mo > 12) return(y)
  if (is.na(da) || da < 1 || da > 31) return(sprintf("%s-%02d", y, mo))
  sprintf("%s-%02d-%02d", y, mo, da)
}

#' A DOI anywhere in a string, bare and lowercase
#' @noRd
.jats_find_doi <- function(x) {
  x <- x[!is.na(x)]
  hit <- regmatches(x, regexpr("10\\.[0-9]{4,9}/[^[:space:]\"<>]+", x))
  if (!length(hit)) return(NA_character_)
  .bibr12_doi(sub("[.,;)]+$", "", hit[[1]]))
}

#' The statement a heading names, for the statements 12.6 adds
#'
#' Whole-heading patterns, as `.statement_heading()`: "Statement of
#' Relevance", "Public Significance Statement", "Generative AI statement",
#' "Use of AI", "Open Practices".
#' @returns "significance", "ai", "open_practices" or NA
#' @noRd
.jats_extra_statement <- function(title) {
  t <- tolower(.tei_squish(gsub("[[:punct:]]+$", "", title %||% "")))
  if (is.na(t) || !nzchar(t)) return(NA_character_)
  if (grepl("^(statement of (relevance|significance)|(public )?significance statement|(author )?impact statement|translational (relevance|significance))$", t)) {
    return("significance")
  }
  if (grepl("^((use of )?(generative )?(ai|artificial intelligence)( statement| disclosure| use)?|declaration of (generative )?ai.*|generative ai statement|statement on (the use of )?(generative )?ai)$", t)) {
    return("ai")
  }
  if (grepl("^(open (science )?practices( statement)?|transparency and openness( promotion)?|research transparency statement)$", t)) {
    return("open_practices")
  }
  NA_character_
}

#' Map a JATS section type to metacheck's section types
#' @noRd
.jats_sec_type <- function(sec_type) {
  t <- tolower(unlist(strsplit(sec_type %||% "", "[|[:space:]]+")))
  t <- t[nzchar(t)]
  if (!length(t)) return(NA_character_)
  map <- c(intro = "intro", introduction = "intro", background = "intro",
           methods = "method", method = "method", materials = "method",
           "materials-methods" = "method", subjects = "method", cases = "method",
           results = "results", discussion = "discussion", conclusions = "discussion",
           conclusion = "discussion", "data-availability" = "data_availability",
           "coi-statement" = "coi", "author-contributions" = "author_contributions",
           "financial-disclosure" = "funding", funding = "funding",
           "ethics-statement" = "ethics", acknowledgments = "acknowledgment",
           "supplementary-material" = "appendix", appendix = "appendix")
  hit <- unname(map[t])
  hit <- hit[!is.na(hit)]
  # "results|discussion" is a results section
  if (length(hit)) hit[[1]] else NA_character_
}

#' Read what a JATS article declares beyond its text
#'
#' @param art the JATS `<article>` element (namespaces stripped)
#' @param ids the id maps of `.jats_to_tei()` (for the inline text of reviews)
#' @returns a named list (see `.jats_apply_extras()`)
#' @noRd
.jats_extras <- function(art, ids) {
  own <- "[not(ancestor::sub-article) and not(ancestor::response)]"
  front <- xml2::xml_find_first(art, "./front")
  meta <- xml2::xml_find_first(front, "./article-meta")
  m1 <- function(node, xpath) {
    if (inherits(node, "xml_missing")) return(NA_character_)
    n <- xml2::xml_find_first(node, xpath)
    if (inherits(n, "xml_missing")) NA_character_ else {
      x <- .tei_squish(xml2::xml_text(n))
      if (nzchar(x)) x else NA_character_
    }
  }
  int1 <- function(node, xpath) {
    n <- xml2::xml_find_first(node, xpath)
    if (inherits(n, "xml_missing")) return(NA_integer_)
    v <- xml2::xml_attr(n, "count")
    if (is.na(v)) v <- xml2::xml_text(n)
    suppressWarnings(as.integer(gsub("[^0-9]", "", v)))
  }
  text_of <- function(node, drop = "./title | ./label | ./caption/title") {
    tmp <- .xml_copy(node)
    xml2::xml_remove(xml2::xml_find_all(tmp, drop))
    ps <- unlist(lapply(xml2::xml_children(tmp), .jats_paragraphs, ids = ids))
    if (!length(ps)) ps <- paste0("<p>", .tei_squish(.jats_inline(tmp, ids)), "</p>")
    paste(.tei_markup_text(ps)[nzchar(.tei_markup_text(ps))], collapse = " ")
  }
  na_empty <- function(x) if (length(x) && !is.na(x) && nzchar(x)) x else NA_character_

  # metadata ----
  dates <- xml2::xml_find_all(meta, "./history/date")
  dtype <- tolower(xml2::xml_attr(dates, "date-type"))
  date_of <- function(types) {
    d <- dates[dtype %in% types]
    if (length(d)) .jats_iso_date(d[[length(d)]]) else NA_character_
  }
  counts <- xml2::xml_find_first(meta, "./counts")
  md <- list(
    article_type = na_empty(tolower(xml2::xml_attr(art, "article-type") %||% NA)),
    received_date = date_of("received"),
    revised_date = date_of(c("rev-recd", "revised")),
    accepted_date = date_of("accepted"),
    copyright_statement = m1(meta, "./permissions/copyright-statement"),
    copyright_holder = m1(meta, "./permissions/copyright-holder"),
    copyright_year = suppressWarnings(as.integer(m1(meta, "./permissions/copyright-year"))),
    word_count = if (inherits(counts, "xml_missing")) NA_integer_ else int1(counts, "./word-count"),
    figure_count = if (inherits(counts, "xml_missing")) NA_integer_ else int1(counts, "./fig-count"),
    table_count = if (inherits(counts, "xml_missing")) NA_integer_ else int1(counts, "./table-count"),
    ref_count = if (inherits(counts, "xml_missing")) NA_integer_ else int1(counts, "./ref-count"),
    page_count = if (inherits(counts, "xml_missing")) NA_integer_ else int1(counts, "./page-count"),
    significance_statement = NA_character_, ai_statement = NA_character_,
    open_practices_statement = NA_character_
  )
  # statements by their heading: sections, notes, footnotes, boxed text,
  # named fields
  holders <- xml2::xml_find_all(art, paste0(
    "(//sec | //notes | //fn | //boxed-text)", own))
  for (h in holders) {
    title <- m1(h, "./title | ./label | ./caption/title | ./caption/p[1]")
    type <- .jats_extra_statement(title)
    if (is.na(type)) next
    key <- paste0(if (type == "open_practices") "open_practices" else type, "_statement")
    if (!is.na(md[[key]])) next
    txt <- text_of(h, "./title | ./label | ./caption")
    if (nzchar(txt)) md[[key]] <- txt
  }
  for (cm in xml2::xml_find_all(meta, ".//custom-meta")) {
    type <- .jats_extra_statement(m1(cm, "./meta-name"))
    key <- paste0(type, "_statement")
    if (!is.na(type) && is.na(md[[key]])) md[[key]] <- m1(cm, "./meta-value")
  }

  # subject terms ----
  subj <- list()
  add_subj <- function(scheme, term, code = NA_character_) {
    if (is.na(term) || !nzchar(term)) return()
    subj[[length(subj) + 1L]] <<- list(scheme = scheme, term = term, code = code)
  }
  for (g in xml2::xml_find_all(meta, ".//article-categories//subj-group")) {
    type <- tolower(xml2::xml_attr(g, "subj-group-type") %||% NA)
    if (is.na(type)) type <- "heading"
    for (s in xml2::xml_find_all(g, "./subject")) add_subj(type, .tei_squish(xml2::xml_text(s)))
    for (cs in xml2::xml_find_all(g, "./compound-subject")) {
      code <- m1(cs, "./compound-subject-part[@content-type='code']")
      label <- m1(cs, "./compound-subject-part[@content-type='label']")
      badge <- type == "article-features" &&
        grepl("^(open-data|open-materials|prereg|preregist)", code %||% "")
      add_subj(if (badge) "open-science-badge" else type, if (is.na(label)) code else label, code)
    }
  }
  # typed keyword groups of the article, and the assessment terms eLife puts
  # in its assessment's front matter
  groups <- c(xml2::xml_find_all(meta, ".//kwd-group[@kwd-group-type]"),
              xml2::xml_find_all(art, "//sub-article//kwd-group[@kwd-group-type='claim-importance' or @kwd-group-type='evidence-strength']"))
  for (g in groups) {
    type <- tolower(xml2::xml_attr(g, "kwd-group-type"))
    if (type %in% c("author", "author-keywords", "author-generated", "abbreviations")) next
    scheme <- switch(type, "claim-importance" = "assessment-importance",
                     "evidence-strength" = "assessment-strength", type)
    for (k in xml2::xml_find_all(g, "./kwd")) add_subj(scheme, .tei_squish(xml2::xml_text(k)))
  }

  # related works ----
  rel <- list()
  relation_of <- function(t) {
    t <- if (is.null(t) || is.na(t)) "" else tolower(t)
    switch(t,
           "corrected-article" = "corrects", "retracted-article" = "retracts",
           "preprint" = "preprint", "companion" = "companion",
           "commentary-article" = , "commentary" = , "article-commented-on" = "comments_on",
           "reply-to" = , "in-reply-to" = "replies_to",
           "peer-review-report" = , "peer-reviewed-article" = "peer_review",
           "object-of-concern" = "expression_of_concern_for",
           "other")
  }
  for (r in xml2::xml_find_all(meta, "./related-article | ./related-object")) {
    href <- xml2::xml_attr(r, "href")
    type <- xml2::xml_attr(r, "related-article-type") %||% NA
    if (is.na(type)) type <- xml2::xml_attr(r, "source-type") %||% NA
    if (is.na(type)) type <- xml2::xml_attr(r, "link-type") %||% NA
    relation <- relation_of(type)
    # the web address of a peer review history (Publons, PubPeer) holds the
    # DOI of the article reviewed, not of the review: only a DOI link gives
    # the review's DOI
    href_doi <- if (relation == "peer_review" && !is.na(href) &&
                    !grepl("^(doi:|10\\.|https?://(dx\\.)?doi\\.org/)", href, ignore.case = TRUE)) {
      NA_character_
    } else href
    doi <- .jats_find_doi(c(href_doi, xml2::xml_attr(r, "object-id"), xml2::xml_text(r)))
    url <- if (!is.na(href) && grepl("^https?://", href)) href else NA_character_
    rel[[length(rel) + 1L]] <- list(relation = relation, doi = doi, url = url,
                                    title = m1(r, ".//article-title"))
  }
  for (ev in xml2::xml_find_all(meta, "./pub-history/event")) {
    su <- xml2::xml_find_all(ev, "./self-uri")
    ct <- xml2::xml_attr(su, "content-type")
    for (k in which(ct %in% c("preprint", "reviewed-preprint"))) {
      href <- xml2::xml_attr(su[[k]], "href")
      rel[[length(rel) + 1L]] <- list(
        relation = "preprint", doi = .jats_find_doi(href),
        url = if (!is.na(href) && grepl("^https?://", href)) href else NA_character_,
        title = NA_character_)
    }
  }
  if (length(rel)) {
    keys <- vapply(rel, \(r) paste(r$relation, r$doi, r$url), "")
    rel <- rel[!duplicated(keys)]
  }

  # contributors who are not authors, and peer reviews ----
  contrib <- list()
  add_contrib <- function(cb) {
    nm <- xml2::xml_find_first(cb, "./name | ./string-name")
    collab <- m1(cb, "./collab")
    roles <- vapply(xml2::xml_find_all(cb, "./role"), \(r) .tei_squish(xml2::xml_text(r)), "")
    type <- xml2::xml_attr(cb, "contrib-type")
    if (!length(roles) && !is.na(type)) roles <- gsub("[_-]", " ", type)
    orcid <- m1(cb, "./contrib-id[@contrib-id-type='orcid']")
    if (!is.na(orcid)) orcid <- paste0("https://orcid.org/", sub("^.*?([0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]).*$", "\\1", orcid))
    aff <- m1(cb, ".//aff | ./xref[@ref-type='aff']")
    affn <- xml2::xml_find_first(cb, ".//aff")
    if (!inherits(affn, "xml_missing")) aff <- .jats_aff_text(affn)
    given <- if (inherits(nm, "xml_missing")) NA_character_ else m1(nm, "./given-names")
    family <- if (inherits(nm, "xml_missing")) NA_character_ else m1(nm, "./surname")
    # a person named twice (an editor of the article and of its assessment)
    # is one contributor with both roles
    if (!is.na(family)) {
      same <- which(vapply(contrib, \(c) identical(c$family, family) && identical(c$given, given), TRUE))
      if (length(same)) {
        contrib[[same[1]]]$role <<- list(unique(c(contrib[[same[1]]]$role[[1]], roles)))
        return(same[1])
      }
    }
    contrib[[length(contrib) + 1L]] <<- list(
      given = if (inherits(nm, "xml_missing")) NA_character_ else m1(nm, "./given-names"),
      family = if (inherits(nm, "xml_missing")) NA_character_ else m1(nm, "./surname"),
      literal = collab, orcid = orcid, role = list(unique(roles)), affiliation = aff)
    length(contrib)
  }
  for (cb in xml2::xml_find_all(meta, ".//contrib-group/contrib[@contrib-type and not(@contrib-type='author')]")) {
    add_contrib(cb)
  }
  # Frontiers and Pensoft name their editors and reviewers in a note
  for (fn in xml2::xml_find_all(meta, ".//author-notes/fn[@custom-type or @fn-type='edited-by']")) {
    t <- .tei_squish(xml2::xml_text(fn))
    role <- if (grepl("^reviewed by", t, ignore.case = TRUE)) "Reviewer" else "Editor"
    t <- sub("^[^:]*:\\s*", "", t)
    contrib[[length(contrib) + 1L]] <- list(given = NA_character_, family = NA_character_,
      literal = t, orcid = NA_character_, role = list(role), affiliation = NA_character_)
  }

  review <- list()
  review_text <- list()
  # an absent attribute or element is NA, not NULL
  or_empty <- function(x) if (is.null(x) || is.na(x)) "" else x
  type_of_review <- function(sa) {
    at <- tolower(or_empty(xml2::xml_attr(sa, "article-type")))
    su <- tolower(or_empty(xml2::xml_attr(sa, "specific-use")))
    title <- tolower(or_empty(m1(sa, "./front-stub//article-title | ./front//article-title")))
    if (su == "acceptance-letter" || grepl("acceptance letter", title)) return("acceptance_letter")
    # before decision letters: "Author response to Decision Letter 0"
    if (at %in% c("author-comment", "reply", "author-response") || grepl("author response", title)) {
      return("author_response")
    }
    if (grepl("decision", su) || at %in% c("decision-letter", "aggregated-review-documents") ||
        grepl("decision letter", title)) return("decision_letter")
    if (at %in% c("referee-report", "reviewer-report")) return("referee_report")
    if (at == "editor-report" && grepl("assessment", title)) return("assessment")
    if (at == "editor-report") return("editor_report")
    "other"
  }
  for (sa in xml2::xml_find_all(art, "//sub-article | //response")) {
    stub <- xml2::xml_find_first(sa, "./front-stub | ./front")
    title <- m1(stub, ".//article-title")
    rnd <- suppressWarnings(as.integer(sub("^.*?decision letter\\s*([0-9]+).*$", "\\1", tolower(title %||% ""))))
    date <- .jats_iso_date(xml2::xml_find_first(stub, ".//pub-date"))
    if (is.na(date)) {
      d <- m1(sa, ".//named-content[@content-type='letter-date' or @content-type='author-response-date']")
      pd <- suppressWarnings(as.Date(d, format = "%d %b %Y"))
      if (!is.na(pd)) date <- format(pd, "%Y-%m-%d")
    }
    # the review's own people (eLife marks a review's reviewers as its
    # authors); an author response is by the paper's authors, who are in
    # author[] already
    rtype <- type_of_review(sa)
    cids <- if (rtype == "author_response") integer(0) else
      vapply(xml2::xml_find_all(stub, ".//contrib-group/contrib"), add_contrib, integer(1))
    body <- xml2::xml_find_first(sa, "./body")
    ps <- if (inherits(body, "xml_missing")) character(0) else .tei_markup_text(.jats_paragraphs(body, ids))
    ps <- ps[nzchar(ps)]
    review[[length(review) + 1L]] <- list(
      review_type = rtype, title = title,
      round = if (is.na(rnd)) NA_integer_ else rnd + 1L, date = date,
      doi = .bibr12_doi(m1(stub, "./article-id[@pub-id-type='doi']")),
      url = NA_character_, contributor_ids = list(as.integer(cids)))
    for (k in seq_along(ps)) {
      review_text[[length(review_text) + 1L]] <- list(
        review_id = length(review), paragraph_id = k, text = ps[[k]])
    }
  }
  # reviews the article only links to
  for (r in rel[vapply(rel, \(x) identical(x$relation, "peer_review"), TRUE)]) {
    review[[length(review) + 1L]] <- list(review_type = "other", title = r$title, round = NA_integer_,
      date = NA_character_, doi = r$doi, url = r$url, contributor_ids = list(integer(0)))
  }

  # supplementary files ----
  supp <- lapply(xml2::xml_find_all(art, paste0("//supplementary-material", own)), \(s) {
    media <- xml2::xml_find_first(s, ".//media | .//inline-supplementary-material")
    href <- xml2::xml_attr(s, "href")
    if (is.na(href) && !inherits(media, "xml_missing")) href <- xml2::xml_attr(media, "href")
    mt <- xml2::xml_attr(s, "mimetype")
    if (is.na(mt) && !inherits(media, "xml_missing")) mt <- xml2::xml_attr(media, "mimetype")
    st <- xml2::xml_attr(s, "mime-subtype")
    if (is.na(st) && !inherits(media, "xml_missing")) st <- xml2::xml_attr(media, "mime-subtype")
    cap <- xml2::xml_find_first(s, "./caption")
    caption <- if (inherits(cap, "xml_missing")) NA_character_ else na_empty(.tei_squish(xml2::xml_text(cap)))
    list(
      label = m1(s, "./label"), caption = caption,
      doi = .jats_find_doi(c(m1(s, ".//object-id[@pub-id-type='doi']"), href)),
      url = if (!is.na(href) && grepl("^https?://", href)) href else NA_character_,
      file_name = if (!is.na(href) && !grepl("^https?://", href)) basename(href) else
        if (!is.na(href)) basename(sub("[?#].*$", "", href)) else NA_character_,
      media_type = if (!is.na(mt) && !is.na(st)) paste0(mt, "/", st) else NA_character_)
  })

  # funding ----
  funding <- lapply(xml2::xml_find_all(meta, "./funding-group/award-group"), \(g) {
    src <- xml2::xml_find_first(g, "./funding-source")
    name_node <- xml2::xml_find_first(src, ".//institution")
    # an award group with only an award number has no funder (filtered below)
    funder <- if (inherits(src, "xml_missing")) "" else if (!inherits(name_node, "xml_missing")) .tei_squish(xml2::xml_text(name_node)) else {
      tmp <- .xml_copy(src)
      xml2::xml_remove(xml2::xml_find_all(tmp, ".//institution-id"))
      .tei_squish(xml2::xml_text(tmp))
    }
    ids_txt <- c(xml2::xml_text(xml2::xml_find_all(src, ".//institution-id")),
                 xml2::xml_attr(src, "href"))
    fdoi <- .jats_find_doi(ids_txt)
    if (!is.na(fdoi) && !startsWith(fdoi, "10.13039/")) fdoi <- NA_character_
    awards <- vapply(xml2::xml_find_all(g, "./award-id"), \(a) .tei_squish(xml2::xml_text(a)), "")
    recips <- vapply(xml2::xml_find_all(g, "./principal-award-recipient"), \(p) {
      nm <- xml2::xml_find_first(p, "./name | ./string-name")
      if (inherits(nm, "xml_missing")) return(.tei_squish(xml2::xml_text(p)))
      paste(stats::na.omit(c(m1(nm, "./given-names"), m1(nm, "./surname"))), collapse = " ")
    }, "")
    list(funder = funder, award_ids = list(awards[nzchar(awards)]), funder_doi = fdoi,
         recipients = list(recips[nzchar(recips)]))
  })
  funding <- Filter(\(f) nzchar(f$funder %||% ""), funding)

  # per author, per affiliation, per float, per reference ----
  contribs <- xml2::xml_find_all(meta, ".//contrib-group/contrib[not(@contrib-type) or @contrib-type='author']")
  has_name <- vapply(contribs, \(cb) !inherits(xml2::xml_find_first(cb, "./name | ./string-name | ./name-alternatives/name"), "xml_missing"), TRUE)
  author_roles <- lapply(contribs[has_name], \(cb) {
    r <- xml2::xml_find_all(cb, "./role")
    role <- vapply(r, \(x) .tei_squish(xml2::xml_text(x)), "")
    uri <- c(xml2::xml_attr(r, "vocab-term-identifier"), xml2::xml_attr(r, "content-type"))
    term <- sub("^.*/contributor-roles/([a-z-]+)/?$", "\\1", uri[!is.na(uri) & grepl("credit.*contributor-roles", uri)])
    list(role = role[nzchar(role)],
         credit = unique(paste0("https://credit.niso.org/contributor-roles/", term, "/")))
  })
  affs <- xml2::xml_find_all(art, paste0("//aff[not(ancestor::ref-list)]", own))
  aff_ror <- stats::setNames(vapply(affs, \(a) {
    r <- m1(a, ".//institution-id[@institution-id-type='ror' or @institution-id-type='ROR']")
    if (is.na(r)) return(NA_character_)
    r <- sub("^(https?://)?(www\\.)?ror\\.org/", "", r)
    if (grepl("^0[a-z0-9]{6}[0-9]{2}$", r)) paste0("https://ror.org/", r) else NA_character_
  }, ""), vapply(affs, .jats_aff_text, ""))

  in_supp <- "[not(ancestor::supplementary-material)]"
  figs <- xml2::xml_find_all(art, paste0("//fig", in_supp, own))
  fig_alt <- vapply(figs, \(f) m1(f, ".//alt-text"), "")
  tabs <- xml2::xml_find_all(art, paste0("//table-wrap", in_supp, own))
  table_grid <- lapply(tabs, \(tw) {
    t <- xml2::xml_find_first(tw, ".//table")
    if (inherits(t, "xml_missing")) return(NULL)
    n_head <- length(xml2::xml_find_all(t, ".//thead/tr"))
    if (!n_head) return(NULL)
    tei <- xml2::read_xml(.jats_table_tei(t, ids))
    rows <- lapply(xml2::xml_find_all(tei, "//row"), \(r) xml2::xml_text(xml2::xml_find_all(r, "./cell")))
    width <- max(lengths(rows))
    rows <- lapply(rows, \(r) { length(r) <- width; r[is.na(r)] <- ""; r })
    head <- vapply(seq_len(width), \(ci) {
      parts <- vapply(rows[seq_len(n_head)], `[[`, "", ci)
      parts <- unique(parts[nzchar(parts)])
      paste(parts, collapse = " / ")
    }, "")
    c(list(head), rows[-seq_len(n_head)])
  })
  refs <- xml2::xml_find_all(art, paste0("//ref-list//ref", own))
  bib_relation <- vapply(refs, \(r) {
    su <- tolower(paste(xml2::xml_attr(xml2::xml_find_all(r, ".//*[@specific-use]"), "specific-use"), collapse = " "))
    if (grepl("issupplementedby|is-supplemented-by", su)) "supplements" else NA_character_
  }, "")

  # the kind of work a reference declares (a dataset, software, a thesis):
  # Grobid's TEI has no place for it, and a dataset read as TEI was a book
  # chapter
  bib_kind <- vapply(refs, \(r) {
    cit <- xml2::xml_find_first(r, ".//element-citation | .//mixed-citation | .//citation | .//nlm-citation")
    t <- tolower(xml2::xml_attr(cit, "publication-type") %NA% (xml2::xml_attr(cit, "citation-type") %NA% ""))
    switch(t, data = , dataset = , database = "dataset", software = "software",
           preprint = "preprint", thesis = "thesis", report = "report",
           confproc = , conference = "conference_paper", NA_character_)
  }, "")

  # a reference's web address: its link, else an address printed in it; not a
  # DOI link (the DOI has its own column)
  bib_url <- vapply(refs, \(r) {
    links <- xml2::xml_find_all(r, ".//ext-link | .//uri")
    hrefs <- c(xml2::xml_attr(links, "href"), xml2::xml_text(links))
    printed <- regmatches(xml2::xml_text(r), gregexpr("(https?|ftp)://[^[:space:]<>\"]+", xml2::xml_text(r)))[[1]]
    hrefs <- trimws(c(hrefs[!is.na(hrefs)], printed))
    hrefs <- sub("[.,;:)\\]]+$", "", hrefs, perl = TRUE)
    hrefs <- hrefs[grepl("^(https?|ftp)://", hrefs, ignore.case = TRUE) &
                     !grepl("^https?://(dx\\.)?doi\\.org/", hrefs, ignore.case = TRUE)]
    if (length(hrefs)) hrefs[[1]] else NA_character_
  }, "")

  # statistics written as MathML formulas: their main comparison ----
  maths <- xml2::xml_find_all(art, paste0(
    "//*[local-name()='math'][not(ancestor::sub-article) and not(ancestor::response) ",
    "and not(ancestor::ref-list)]"))
  formulas <- unlist(lapply(maths, .mathml_relation), recursive = FALSE)

  # declared section types, by heading ----
  secs <- xml2::xml_find_all(art, paste0("//body//sec[@sec-type]", own))
  # keyed by the heading as the paper prints it: "1 Method" for a numbered one
  sec_types <- stats::setNames(
    vapply(secs, \(s) .jats_sec_type(xml2::xml_attr(s, "sec-type")), ""),
    vapply(secs, \(s) {
      h <- .jats_head(s, ids)
      if (nzchar(h$n)) paste(h$n, h$head) else h$head
    }, ""))
  sec_types <- sec_types[!is.na(sec_types) & nzchar(names(sec_types))]

  list(metadata = md, subject = subj, related = rel, contributor = contrib,
       supplement = supp, review = review, review_text = review_text,
       funding = funding, author_roles = author_roles, aff_ror = aff_ror,
       fig_alt = fig_alt, table_grid = table_grid, bib_relation = bib_relation,
       bib_url = bib_url, bib_kind = bib_kind, formulas = formulas, sec_types = sec_types)
}

#' Add what `.jats_extras()` read to a paper, which becomes 12.6
#'
#' @param paper the paper `.grobid_to_bibr12()` made from the TEI translation
#' @param ex the list `.jats_extras()` returns
#' @returns the paper, with `info$schema_version` "12.6"
#' @noRd
.jats_apply_extras <- function(paper, ex) {
  for (col in names(ex$metadata)) {
    v <- ex$metadata[[col]]
    if (length(v) == 1 && !is.na(v)) paper$info[[col]] <- v
    else if (!col %in% names(paper$info)) paper$info[[col]] <- v
  }
  rows <- function(lst, tbl) {
    cols <- .bibr12_cols[[tbl]]
    if (!length(lst)) return(.bibr12_df(list(), cols))
    columns <- lapply(stats::setNames(nm = names(cols)), \(c) lapply(lst, \(r) r[[c]]))
    columns[[names(cols)[1]]] <- seq_along(lst)
    .bibr12_df(columns, cols, n = length(lst))
  }
  for (tbl in c("subject", "related", "contributor", "supplement", "review", "review_text")) {
    paper[[tbl]] <- rows(ex[[tbl]], tbl)
  }
  if (length(ex$funding)) paper$funding <- rows(ex$funding, "funding")

  # authors: roles and CRediT roles, in byline order
  n <- min(nrow(paper$author), length(ex$author_roles))
  for (i in seq_len(n)) {
    paper$author$role[[i]] <- ex$author_roles[[i]]$role
    cr <- ex$author_roles[[i]]$credit
    paper$author$credit_roles[[i]] <- cr[grepl("^https://credit\\.niso\\.org/contributor-roles/[a-z-]+/$", cr)]
  }
  # affiliations: the ROR ID declared for the same text
  if (nrow(paper$affiliation)) {
    paper$affiliation$ror <- unname(ex$aff_ror[paper$affiliation$text])
  }
  # figures and tables are in the same order as in the file
  if (nrow(paper$figure) == length(ex$fig_alt)) {
    paper$figure$alt_text <- ifelse(nzchar(ex$fig_alt), ex$fig_alt, NA_character_)
  }
  if (nrow(paper$table) == length(ex$table_grid)) {
    for (i in seq_along(ex$table_grid)) {
      g <- ex$table_grid[[i]]
      if (is.null(g)) next
      # the header rows the file marks, joined per column, instead of the
      # guess from the cells' shape
      paper$table$contents[[i]] <- g
      paper$table$header_rows[[i]] <- 1L
    }
  }
  if (nrow(paper$bib) == length(ex$bib_relation)) paper$bib$relation <- ex$bib_relation
  if (nrow(paper$bib) == length(ex$bib_url)) paper$bib$url <- ex$bib_url
  if (nrow(paper$bib) == length(ex$bib_kind)) {
    k <- !is.na(ex$bib_kind)
    paper$bib$bib_type[k] <- ex$bib_kind[k]
  }
  paper <- .text_links(paper)
  paper <- .mathml_eq(paper, ex$formulas)
  # section types the publisher declares, for a heading that names one
  # section; statements keep the type their heading gave them
  if (length(ex$sec_types) && nrow(paper$section)) {
    hdr <- paper$section$header
    keep <- c("abstract", "references", "acknowledgment", "funding", "coi",
              "data_availability", "author_contributions", "ethics")
    for (h in unique(names(ex$sec_types))) {
      at <- which(hdr %in% h)
      if (length(at) != 1 || paper$section$section_type[at] %in% keep) next
      types <- unique(ex$sec_types[names(ex$sec_types) == h])
      if (length(types) == 1) paper$section$section_type[at] <- types
    }
  }
  paper$info$schema_version <- "12.6"
  .paper_coerce(paper)
}

#' Web addresses printed as plain text, added to the url table
#'
#' A publisher marks most links (`<ext-link>`, which becomes a url row), but
#' not all: "osf.io/b4cgq", "www.virped.org" are printed as plain text. Such
#' an address is added when it starts with a scheme ("https://"), with
#' "www.", or is a domain followed by a path ("osf.io/b4cgq"); a word with a
#' full stop in it ("e.g.", "Fig.1") is none of these. A full stop or bracket
#' that ends the sentence is not part of the address. The reference list is
#' left out: its addresses are in `bib$url` and `bib$doi`.
#'
#' @param paper a paper in 12.x form
#' @returns the paper, with url rows added
#' @noRd
.text_links <- function(paper) {
  tx <- paper$text
  if (!NROW(tx)) return(paper)
  refs <- paper$section$section_id[paper$section$section_type %in% "references"]
  rows <- which(!tx$section_id %in% refs | is.na(tx$section_id))
  pattern <- paste0(
    "(?<![\\w@/.:-])(",
    "(?:https?|ftp)://[^\\s<>\"]+",
    "|www\\.[a-z0-9-]+(?:\\.[a-z0-9-]+)*\\.[a-z]{2,}(?:/[^\\s<>\"]*)?",
    "|(?:[a-z0-9-]+\\.)+[a-z]{2,}/[^\\s<>\"]+",
    ")")
  key <- function(x) tolower(sub("/+$", "", sub("^(https?|ftp)://", "", x)))
  known <- paste(paper$url$text_id, key(paper$url$href))
  add <- list()
  for (r in rows) {
    txt <- tx$text[[r]]
    m <- gregexpr(pattern, txt, perl = TRUE, ignore.case = TRUE)[[1]]
    if (m[1] < 0) next
    hits <- regmatches(txt, list(m))[[1]]
    for (k in seq_along(hits)) {
      h <- hits[[k]]
      # the sentence's punctuation, and a closing bracket the address did
      # not open
      repeat {
        new <- sub("[.,;:!?'\"\u2019\u201d]+$", "", h, perl = TRUE)
        if (grepl("[)\\]]$", new, perl = TRUE)) {
          open <- lengths(regmatches(new, gregexpr("[(\\[]", new, perl = TRUE)))
          close <- lengths(regmatches(new, gregexpr("[)\\]]", new, perl = TRUE)))
          if (close > open) new <- sub(".$", "", new)
        }
        if (identical(new, h)) break
        h <- new
      }
      if (!grepl("[a-z0-9]", h, ignore.case = TRUE)) next
      id <- paste(tx$text_id[[r]], key(h))
      if (id %in% known) next
      known <- c(known, id)
      start <- m[[k]] - 1L
      add[[length(add) + 1L]] <- list(
        href = if (grepl("^(https?|ftp)://", h, ignore.case = TRUE)) h else paste0("https://", h),
        link_text = NA_character_, text_id = tx$text_id[[r]],
        start = start, end = start + nchar(h))
    }
  }
  if (!length(add)) return(paper)
  new <- .bibr12_df(lapply(stats::setNames(nm = names(.bibr12_cols$url)), \(c) lapply(add, \(a) a[[c]])),
                    .bibr12_cols$url, n = length(add))
  url <- rbind(paper$url, new)
  url <- url[order(url$text_id, url$start), ]
  url$url_id <- seq_len(nrow(url))
  rownames(url) <- NULL
  paper$url <- url
  paper
}

#' Statistics written as MathML formulas, from the formula's own parts
#'
#' The statistics found in a sentence's text take the first comparison sign
#' they meet: in "P(b > 0) = .97" that is the ">" inside the bracket. A MathML
#' formula marks its main comparison (see `.mathml_relation()`), so the rows
#' the text gave from inside a formula are replaced by one row from its parts,
#' and a formula's comparison the text did not find is added. A left side
#' such as "t(38)" keeps its degrees of freedom apart, in brackets, as the
#' rest of the table does. Only the comparison signs the table allows are
#' kept ("2.1 \u00b1 0.3" is a mean and its spread, not a comparison).
#'
#' @param paper a paper in 12.x form
#' @param formulas the formulas' relations (`.jats_extras()`)
#' @returns the paper
#' @noRd
.mathml_eq <- function(paper, formulas) {
  comps <- c("=", "<", ">", "~", intToUtf8(c(0x2248, 0x2260, 0x2264, 0x2265, 0x226A, 0x226B), multiple = TRUE))
  formulas <- Filter(\(f) f$comp %in% comps && nzchar(f$lhs) && nzchar(f$rhs), formulas)
  if (!length(formulas) || !NROW(paper$text)) return(paper)
  eq <- paper$eq
  # the text's statistics write the minus sign "-" and may drop spaces: compare
  # in that form
  plain <- function(x) gsub("\\s+", "", gsub(intToUtf8(0x2212), "-", x, fixed = TRUE))
  # all comparisons of one formula together: the text's rows inside it are
  # removed once, then each comparison adds its row (a second comparison must
  # not take the first's row for one of the text's)
  by_text <- split(formulas, vapply(formulas, `[[`, "", "text"))
  for (fs in by_text) {
    text <- fs[[1]]$text
    formula <- plain(text)
    # the same formula printed several times gives one entry per print: its
    # distinct comparisons, once per time the sentence prints it
    fs <- fs[!duplicated(vapply(fs, \(f) paste(f$lhs, f$comp, f$rhs, sep = "\r"), ""))]
    tids <- paper$text$text_id[grepl(text, paper$text$text, fixed = TRUE)]
    for (tid in tids) {
      times <- lengths(regmatches(paper$text$text[paper$text$text_id == tid][1],
                                  gregexpr(text, paper$text$text[paper$text$text_id == tid][1], fixed = TRUE)))
      here <- which(eq$text_id == tid)
      inside <- here[vapply(here, \(i) {
        all(vapply(c(eq$lhs[i], eq$rhs[i]), \(x) is.na(x) || grepl(plain(x), formula, fixed = TRUE), TRUE))
      }, logical(1))]
      grp <- if (length(inside)) eq$grp_id[inside[1]] else max(c(0L, eq$grp_id[here])) + 1L
      rows <- lapply(fs, \(f) {
        lhs <- f$lhs
        df <- NA_character_
        parts <- regmatches(lhs, regexec("^(.{1,8}?)\\s*(\\([0-9.,\\s]+\\))$", lhs, perl = TRUE))[[1]]
        if (length(parts) == 3) {
          lhs <- parts[2]
          df <- gsub("\\s+", "", parts[3])
        }
        row <- eq[0, ]
        row[1, ] <- NA
        row$text_id <- tid
        row$grp_id <- grp
        row$lhs <- lhs
        row$df <- df
        row$comp <- f$comp
        row$rhs <- f$rhs
        row
      })
      if (length(inside)) eq <- eq[-inside, ]
      eq <- rbind(eq, do.call(rbind, rep(rows, max(1L, times))))
    }
  }
  eq <- eq[order(eq$text_id, eq$grp_id), ]
  if ("eq_id" %in% names(eq)) eq$eq_id <- seq_len(nrow(eq))
  rownames(eq) <- NULL
  paper$eq <- eq
  paper
}
