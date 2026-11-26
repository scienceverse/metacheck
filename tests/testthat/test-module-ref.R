# title searches that find nothing, for tests whose references cite a DOI
# that does not exist (these are searched by title)
mock_no_search <- function(env = parent.frame()) {
  local_mocked_bindings(
    crossref_query = \(ref, ...) data.frame(ref = unlist(ref), DOI = NA_character_),
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), error = logical(0)),
    semanticscholar_query = \(title) data.frame(query = integer(0),
      doi = character(0), url = character(0), title = character(0),
      year = integer(0), authors = character(0), venue = character(0),
      type = character(0), error = logical(0)),
    .package = "metacheck", .env = env)
}

test_that("ref_consistency", {
  module <- "ref_consistency"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- demopaper()

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")   # yellow, not red (#695, point 2)
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(mod_output$module, module)

  # iteration
  skip_no_psychsci()
  paper <- psychsci[c(23, 25)]
  mod_output1 <- module_run(paper[[1]], module)
  mod_output2 <- module_run(paper[[2]], module)
  mod_output3 <- module_run(paper, module)
  t12 <- rbind(mod_output1$table, mod_output2$table)
  diffs <- dplyr::setdiff(t12, mod_output3$table)
  expect_equal(nrow(diffs), 0)
})

test_that("ref_miscitation", {
  module <- "ref_miscitation"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- demopaper()

  ## custom db
  test_doi <- "10.1037/0003-066x.54.6.408"
  db <- data.frame(
    doi = test_doi,
    reftext = "The full reference (this is a test)",
    warning = "Lorem ipsum this is a test..."
  )

  mod_output <- module_run(paper, module, db = db)
  expect_equal(mod_output$table$doi[[1]], test_doi)
  expect_equal(mod_output$summary_table$`miscite_10.1037/0003-066x.54.6.408`,
               1)

  ## all matched papers are reported
  db <- data.frame(
    doi = unique(na.omit(paper$bib$doi))[1:2],
    reftext = c("Reference A", "Reference B"),
    warning = c("Warning A", "Warning B")
  )
  mod_output <- module_run(paper, module, db = db)
  expect_length(mod_output$report, 2)
})


test_that("ref_accuracy", {
  # doi.org reports the demo paper's two unresolved DOIs as not existing; a
  # title search finds the first (faux, in DataCite) and not the second
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE),
                        crossref_doi = \(doi, ...) data.frame(DOI = doi),
                        crossref_query = \(ref, ...) data.frame(ref = unlist(ref), DOI = NA_character_),
                        datacite_query = \(title, ...) {
                          k <- grep("^Faux", title)
                          data.frame(query = k, doi = rep("10.5281/zenodo.2669586", length(k)),
                                     title = title[k], year = 2019L, authors = "DeBruine, Lisa",
                                     type = "Software", error = FALSE)
                        },
                        semanticscholar_query = \(title) data.frame(query = integer(0),
                          doi = character(0), url = character(0), title = character(0),
                          year = integer(0), authors = character(0), venue = character(0),
                          type = character(0), error = logical(0)))
  module <- "ref_accuracy"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no bib_match: nothing is checked, but references without a DOI are
  # counted
  paper <- demopaper()
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_match(mod_output$summary_text, "not looked up at Crossref")
  expect_equal(mod_output$summary_table$refs_checked, 0)
  expect_equal(mod_output$summary_table$no_doi, 1)
  expect_match(mod_output$summary_text, "and 1 does not")

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(nrow(mod_output$table), nrow(paper$bib))

  # references that supplied a DOI are checked; the demo paper includes two
  # references with a DOI that does not exist, and one without a DOI, which
  # Crossref matched by title (#528). The work of the first DOI is found by
  # its title, so it is flagged with that record; the second is not found.
  # Crossref's match for the reference without a DOI has the cited title,
  # shortened, and two authors the reference omits, so it is flagged too.
  st <- mod_output$summary_table
  expect_equal(st$refs_checked, 4)
  expect_equal(st$no_doi, 1)
  expect_equal(st$incoherent, 2)
  expect_equal(mod_output$traffic_light, "yellow")
  unresolved <- mod_output$table[mod_output$table$tier == "unresolved", ]
  expect_equal(unresolved$incoherent, c(TRUE, FALSE))
  expect_equal(unresolved$not_found, c(FALSE, TRUE))
  # the package's DOI at CRAN (Crossref) or Zenodo (DataCite)
  expect_match(unresolved$doi.match[1], "faux|zenodo\\.2669586", ignore.case = TRUE)
  report <- paste(unlist(mod_output$report), collapse = "\n")
  expect_match(report, "the DOI shown is that of the work found by the cited title in", fixed = TRUE)
  expect_match(report, "The cited DOI is not registered at doi.org.", fixed = TRUE)

  # a checked reference (one whose DOI resolves) whose cited journal differs
  # from the record its DOI points to is also incoherent. Corrupt the journal
  # of a "provided" reference and confirm one more reference is flagged.
  checked_id <- mod_output$table$bib_id[mod_output$table$tier == "provided"][[1]]
  bad <- demopaper()
  bad$bib$container[bad$bib$bib_id == checked_id] <-
    "A Completely Different Journal Name"
  bad_out <- module_run(bad, module)
  expect_equal(bad_out$traffic_light, "yellow")
  expect_equal(bad_out$summary_table$incoherent, 3)

  # min_mismatches controls whether a single title/author mismatch flags. A lone
  # author mismatch flags at the default (1) but not when set to 2.
  auth <- demopaper()
  auth$bib$authors[auth$bib$bib_id == checked_id] <- "Nonexistent, A; Madeup, B"
  loose  <- module_run(auth, module)                       # default = 1
  expect_warning(strict <- module_run(auth, module, min_mismatches = 2), "deprecated")
  # the lone author mismatch adds one only at the sensitive setting
  expect_equal(loose$summary_table$incoherent, 3)
  expect_equal(strict$summary_table$incoherent, 1)
})

test_that("ref_replication", {
  module <- "ref_replication"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no DOIs
  paper <- demopaper()
  paper$bib$doi[[1]] <- NA
  paper$bib <- paper$bib[is.na(paper$bib$doi), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)

  mod_output <- module_run(paper, module, show_outcomes = TRUE)
  expect_equal(mod_output$table$replication_outcome, "failed")
  expect_equal(mod_output$table$replication_type, "replication")
})

test_that("ref_retraction", {
  module <- "ref_retraction"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no DOIs
  paper <- demopaper()
  paper$bib <- paper$bib[is.na(paper$bib$doi), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$retractionwatch, "Retraction")
})

test_that("ref_pubpeer", {
  skip_no_psychsci()
  module <- "ref_pubpeer"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- demopaper()
  paper$bib <- paper$bib[c(), ]
  paper$bib_match <- paper$bib_match[c(), ]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # no bib_match
  paper <- demopaper()
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  # 2, not 3: one of the 3 comments on 10.1177/0956797614520714 is Statcheck's,
  # which is left out of the count (#695, point 14); PubPeer's own data, so
  # this changes when someone comments on the cited papers
  expect_equal(sum(mod_output$table$total_comments), 2)

  # no DOIs
  paper <- demopaper()
  paper$bib <- paper$bib[is.na(paper$bib$doi), ]
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(sum(mod_output$table$total_comments), 2)   # see above

  # multiple papers
  paper <- psychsci[c(4, 9)]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_gte(nrow(mod_output$table), 2)
  expect_equal(mod_output$summary_table$paper_id, names(psychsci)[c(4, 9)])
}, "mock")


test_that("ref_summary", {
  module <- "ref_summary"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 5)
  expect_in("doi", names(mod_output$table))
  expect_disjoint("crossref_doi_mismatch", names(mod_output$table))
  expect_disjoint("retractionwatch", names(mod_output$table))

  # chaining 1
  paper <- demopaper()
  mod_output <- paper |>
    module_run("ref_accuracy") |>
    module_run(module)
  expect_in("accuracy_mismatch", names(mod_output$table))
  expect_disjoint("retractionwatch", names(mod_output$table))
})

test_that("ref_pubpeer reports a failed PubPeer request instead of stopping (U23)", {
  # pubpeer_comments() gives NULL for an error answer; the join stopped on it
  local_mocked_bindings(pubpeer_comments = function(doi) NULL)
  mo <- module_run(demopaper(), "ref_pubpeer")
  expect_equal(mo$traffic_light, "fail")
  expect_match(mo$summary_text, "PubPeer could not be reached", fixed = TRUE)
})

test_that("ref_consistency reports a citation to a missing reference, and reads 'bib' xrefs (U115, U207)", {
  # a citation with a bib_id that is not in the bibliography was never
  # reported, and an older bibr export ("bib" xrefs) had no citations at all
  p <- demopaper()
  x <- p$xref[p$xref$xref_type == "bibr", ][1, ]
  x$xref_id <- 99L; x$contents <- "(Nobody, 1900)"
  p$xref <- rbind(p$xref, x)
  mo <- module_run(p, "ref_consistency")
  expect_equal(mo$summary_table$n_missing, 1)
  expect_true(99L %in% mo$table$bib_id)

  p2 <- demopaper()
  n_cited <- module_run(p2, "ref_consistency")$summary_table$n_xrefs
  p2$xref$xref_type[p2$xref$xref_type == "bibr"] <- "bib"
  expect_equal(module_run(p2, "ref_consistency")$summary_table$n_xrefs, n_cited)
})

test_that("ref_accuracy counts a reference with several bib_match rows once, and ignores footnote markers in titles (U114)", {
  mock_no_search()
  # doi.org reports the demo paper's two unresolved DOIs as not existing
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE),
                        crossref_doi = \(doi, ...) data.frame(DOI = doi))
  # three joins multiplied duplicate bib_match rows (3 copies: 31 rows for 5
  # references); <sup> markers were compared as part of the title
  p <- demopaper()
  n <- nrow(module_run(p, "ref_accuracy")$table)
  p$bib_match <- rbind(p$bib_match, p$bib_match[1, ], p$bib_match[1, ])
  expect_equal(nrow(module_run(p, "ref_accuracy")$table), n)

  p <- demopaper()
  i <- match(p$bib_match$bib_id[1], p$bib$bib_id)
  before <- module_run(p, "ref_accuracy")$table
  p$bib$title[i] <- paste0(p$bib$title[i], "<sup>1</sup>")
  after <- module_run(p, "ref_accuracy")$table
  expect_equal(after$title_mismatch, before$title_mismatch)
})

test_that("ref_accuracy accepts a cited DOI written as a URL", {
  # a PLOS article URL around the matched DOI was flagged as a DOI mismatch
  p <- demopaper()
  i <- match(p$bib_match$bib_id[1], p$bib$bib_id)
  doi <- p$bib_match$doi[1]
  p$bib$doi[i] <- paste0("http://journals.plos.org/plosone/article?id=", doi)
  tbl <- module_run(p, "ref_accuracy")$table
  expect_false(tbl$doi_mismatch[tbl$bib_id == p$bib_match$bib_id[1]])
  p$bib$doi[i] <- paste0("https://doi.org/", toupper(doi))
  tbl <- module_run(p, "ref_accuracy")$table
  row <- tbl$bib_id == p$bib_match$bib_id[1]
  expect_false(tbl$doi_mismatch[row])
  expect_false(tbl$incoherent[row])

  # a genuinely different DOI is still flagged (the cited DOI could not be
  # looked up, so the record was found by a title search; a record found by
  # the cited DOI itself is an alias)
  p$bib$doi[i] <- "https://doi.org/10.1037/0003-066x.54.6.4 09"
  p$bib_match$score[1] <- 80
  tbl <- module_run(p, "ref_accuracy")$table
  expect_true(tbl$doi_mismatch[row])
  expect_true(tbl$incoherent[row])

  # an unresolved DOI written as a URL links to doi.org once
  p$bib_match <- p$bib_match[-1, ]
  p$bib$doi[i] <- paste0("https://doi.org/", doi)
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE))
  mock_no_search()
  mo <- module_run(p, "ref_accuracy")
  expect_equal(mo$table$tier[mo$table$bib_id == p$bib$bib_id[i]], "unresolved")
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_false(grepl("doi.org/https", report, fixed = TRUE))
  expect_match(report, paste0("https://doi.org/", doi), fixed = TRUE)
})

test_that("ref_accuracy asks doi.org about a cited DOI without a record", {
  p <- demopaper()
  i <- match(1, p$bib$bib_id)
  own_rec <- p$bib_match[p$bib_match$bib_id == 1, ]
  p$bib_match <- p$bib_match[p$bib_match$bib_id != 1, ]
  ra <- NULL
  # the record doi.org returns: none, or that of the cited work
  csl <- NULL
  no_csl <- data.frame(doi = NA_character_, title = NA_character_, year = NA_integer_,
                       container = NA_character_, bib_type = NA_character_)
  no_csl$authors <- list(data.frame(given = character(0), family = character(0)))
  local_mocked_bindings(doi_csl = \(doi) csl %||% no_csl[rep(1, length(doi)), ])
  # the answer for bib 1; the demo's other unresolved DOIs get none
  local_mocked_bindings(doi_ra = \(doi) {
    own <- doi == p$bib$doi[i]
    data.frame(doi = doi, ra = ifelse(own, ra$ra, NA_character_),
               exists = ifelse(own, ra$exists, NA))
  })
  row <- \(mo) mo$table[mo$table$bib_id == 1, ]

  # doi.org: the DOI does not exist
  # (and its title is not found: listed as not found)
  mock_no_search()
  ra <- list(ra = NA_character_, exists = FALSE)
  mo <- module_run(p, "ref_accuracy")
  expect_equal(row(mo)$tier, "unresolved")
  expect_false(row(mo)$incoherent)
  expect_true(row(mo)$not_found)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "is not registered at doi.org", fixed = TRUE)

  # doi.org could not be asked: not checked, not flagged
  ra <- list(ra = NA_character_, exists = NA)
  mo <- module_run(p, "ref_accuracy")
  expect_equal(row(mo)$tier, "unconfirmed")
  expect_false(row(mo)$incoherent)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "no answer from doi.org", fixed = TRUE)

  # the DOI is registered with another agency: listed, not flagged
  ra <- list(ra = "mEDRA", exists = TRUE)
  mo <- module_run(p, "ref_accuracy")
  expect_equal(row(mo)$tier, "registered")
  expect_false(row(mo)$incoherent)
  expect_equal(row(mo)$doi_ra, "mEDRA")
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "DOIs that could not be checked", fixed = TRUE)
  expect_match(report, "mEDRA", fixed = TRUE)
  expect_match(mo$summary_text, "1 cited DOI is registered, but no record was retrieved for it", fixed = TRUE)

  # doi.org returns the agency's record: compared like a Crossref record
  csl <- data.frame(doi = own_rec$doi, title = own_rec$title, year = own_rec$year,
                    container = own_rec$container, bib_type = own_rec$bib_type)
  csl$authors <- own_rec$authors
  mo <- module_run(p, "ref_accuracy")
  expect_equal(row(mo)$tier, "provided")
  expect_false(row(mo)$incoherent)
  expect_equal(row(mo)$record_source, "mEDRA")

  # ... and a record that differs from the reference is flagged
  csl$title <- "A wholly different title about something else entirely"
  mo <- module_run(p, "ref_accuracy")
  expect_equal(row(mo)$tier, "provided")
  expect_true(row(mo)$incoherent)
  expect_true(row(mo)$title_mismatch)
})

test_that("ref_accuracy lists references without a DOI that no registry holds", {
  # bib 3 (Lakens 2018) has no DOI; without its CrossRef match it is searched
  # by title in CrossRef, then in DataCite, then in Semantic Scholar
  p <- demopaper()
  p$bib_match <- p$bib_match[p$bib_match$bib_id != 3, ]
  title <- p$bib$title[p$bib$bib_id == 3]
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA))
  cr_title <- dc_title <- s2 <- NULL
  dc_called <- FALSE
  local_mocked_bindings(
    crossref_query = \(ref, ...) {
      q <- unlist(ref)
      if (is.na(cr_title)) return(data.frame(ref = q, DOI = NA_character_, error = "request failed"))
      data.frame(ref = q, DOI = "10.1/x", title = cr_title, score = 20)
    },
    datacite_query = \(title, ...) {
      dc_called <<- TRUE
      if (is.null(dc_title)) return(data.frame(query = integer(0), doi = character(0),
        title = character(0), year = integer(0), authors = character(0), error = logical(0)))
      data.frame(query = 1L, doi = paste0("10.2/y", c("", "z"))[seq_along(dc_title)],
                 title = dc_title, year = 2018L, authors = "Lakens", error = FALSE)
    },
    semanticscholar_query = \(title) s2 %||% data.frame(query = integer(0),
      doi = character(0), url = character(0), title = character(0),
      year = integer(0), authors = character(0), venue = character(0),
      type = character(0), error = logical(0)))
  nf <- \(mo) mo$table$not_found[mo$table$bib_id == 3]

  # found nowhere: listed, with the summary
  cr_title <- "Equivalence of something else entirely"
  mo <- module_run(p, "ref_accuracy")
  expect_true(nf(mo))
  expect_true(dc_called)
  expect_equal(mo$summary_table$not_found, 1)
  expect_match(mo$summary_text, "1 was not found in Crossref, DataCite or Semantic Scholar", fixed = TRUE)
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "### References not found", fixed = TRUE)
  # without Semantic Scholar, a title the other two did not find is not
  # checked, not "not found"
  local({
    local_mocked_bindings(semanticscholar_query = \(title) stop("not searched"))
    mo <- module_run(p, "ref_accuracy", semantic_scholar = FALSE)
    expect_false(nf(mo))
    expect_true(mo$table$search_failed[mo$table$bib_id == 3])
    expect_match(mo$summary_text, "1 reference not found in Crossref or DataCite was not checked, because Semantic Scholar was not searched", fixed = TRUE)
    expect_match(paste(unlist(mo$report), collapse = "\n"), "Semantic Scholar was not searched", fixed = TRUE)
  })
  # a reference not found does not change the traffic light
  p2 <- p; p2$bib_match <- p2$bib_match[0, ]
  expect_equal(module_run(p2, "ref_accuracy")$traffic_light, "na")

  # CrossRef holds the title, with a score too low for add_bib_match()
  cr_title <- paste0(title, ": A Tutorial")
  dc_called <- FALSE
  mo <- module_run(p, "ref_accuracy")
  expect_false(nf(mo))
  expect_false(dc_called)

  # DataCite holds it, with the cited year and authors: its DOI is suggested,
  # unless another work has that title and year (a review of a book)
  cr_title <- "Equivalence of something else entirely"
  dc_title <- title
  mo <- module_run(p, "ref_accuracy")
  expect_false(nf(mo))
  expect_equal(mo$table$doi.match[mo$table$bib_id == 3], "10.2/y")
  expect_true(mo$table$doi_suggested[mo$table$bib_id == 3])
  dc_title <- c(title, title)
  mo <- module_run(p, "ref_accuracy")
  expect_false(mo$table$doi_suggested[mo$table$bib_id == 3])
  expect_match(paste(unlist(mo$report), collapse = "\n"), "2 records with this title and year", fixed = TRUE)
  # a title cut short in reading the PDF, whose full title is in the
  # reference text: the work with that full title has its DOI suggested
  p4 <- p
  p4$bib$title[p4$bib$bib_id == 3] <- sub(" Research$", "", title)
  dc_title <- title
  mo <- module_run(p4, "ref_accuracy")
  expect_true(mo$table$doi_suggested[mo$table$bib_id == 3])
  # a number that continues the title is another work, not a footnote
  dc_title <- paste(title, "2")
  mo <- module_run(p, "ref_accuracy")
  expect_false(mo$table$doi_suggested[mo$table$bib_id == 3])

  # Semantic Scholar holds it, with other authors: flagged, linking its page
  dc_title <- NULL
  s2 <- data.frame(query = 1L, doi = NA_character_, url = "https://www.semanticscholar.org/paper/abc",
                   title = title, year = 2018L, authors = "Smith, John; Jones, Ann",
                   venue = "Some Conference", type = "Conference", error = FALSE)
  mo <- module_run(p, "ref_accuracy")
  expect_false(nf(mo))
  expect_true(mo$table$incoherent[mo$table$bib_id == 3])
  expect_match(paste(unlist(mo$report), collapse = "\n"),
               "https://www.semanticscholar.org/paper/abc", fixed = TRUE)
  s2 <- NULL

  # the search failed: not listed, but counted as not checked
  cr_title <- NA
  dc_title <- NULL
  mo <- module_run(p, "ref_accuracy")
  expect_false(nf(mo))
  expect_true(mo$table$search_failed[mo$table$bib_id == 3])
  expect_match(mo$summary_text, "The title search failed for 1 reference", fixed = TRUE)

  # a title in a script written without spaces is counted in words, and
  # searched
  p3 <- p
  p3$bib$title[p3$bib$bib_id == 3] <- "\u57fa\u4e8e\u6df1\u5ea6\u5b66\u4e60\u7684\u56fe\u50cf\u8bc6\u522b\u65b9\u6cd5\u7814\u7a76"
  cr_title <- "Equivalence of something else entirely"
  expect_true(nf(module_run(p3, "ref_accuracy")))

  # a title cited twice is found for both references
  p2 <- p
  b <- p2$bib[p2$bib$bib_id == 3, ]
  b$bib_id <- 30L
  p2$bib <- rbind(p2$bib, b)
  cr_title <- title
  tbl <- module_run(p2, "ref_accuracy")$table
  expect_false(any(tbl$not_found[tbl$bib_id %in% c(3, 30)]))
})

test_that("ref_accuracy accepts author lists truncated with et al.", {
  # bib 3 (Lakens, Scheel & Isager 2018) cites only Lakens; its record has
  # all three. A confident match, so the record is compared.
  p <- demopaper()
  i <- p$bib$bib_id == 3
  m <- p$bib_match$bib_id == 3
  p$bib_match$score[m] <- 90
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA))
  row <- \(p) { t <- module_run(p, "ref_accuracy")$table; t[t$bib_id == 3, ] }
  expect_true(row(p)$author_mismatch)

  # "et al." in the author list, or "others" as BibTeX gives it
  p1 <- p; p1$bib$authors[i] <- "Lakens, Daniël; et al."
  expect_false(row(p1)$incoherent)
  p2 <- p; p2$bib$authors[i] <- "Lakens, Daniël; others"
  expect_false(row(p2)$incoherent)
  # "et al." only in the reference text, before the title
  p3 <- p
  ti <- p3$text$text_id == p3$bib$text_id[i]
  p3$text$text[ti] <- sub("Lakens, Daniël.", "Lakens, Daniël, et al.", p3$text$text[ti], fixed = TRUE)
  expect_false(row(p3)$incoherent)
  # also when the title was read with other capitals than the text has
  p5 <- p3; p5$bib$title[i] <- toupper(p5$bib$title[i])
  expect_false(row(p5)$incoherent)
  # the cited authors must still be the record's first authors
  p4 <- p; p4$bib$authors[i] <- "Scheel, Anne M; et al."
  expect_true(row(p4)$author_mismatch)
})

test_that("ref_accuracy compares a record that a title search finds", {
  # bib 3 without its CrossRef match is searched by title; a record with the
  # same title whose authors or year differ flags it
  p <- demopaper()
  p$bib_match <- p$bib_match[p$bib_match$bib_id != 3, ]
  title <- p$bib$title[p$bib$bib_id == 3]
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA),
                        crossref_doi = \(doi, ...) data.frame(DOI = doi))
  cr_authors <- "Lakens"
  cr_year <- 2018L
  cr_container <- NA_character_
  local_mocked_bindings(
    crossref_query = \(ref, ...) {
      x <- data.frame(ref = unlist(ref), DOI = "10.1177/2515245918770963",
                      title = title, score = 20, year = cr_year,
                      type = "journal-article", `container-title` = cr_container,
                      check.names = FALSE)
      x$author <- list(data.frame(given = "A", family = cr_authors))
      x
    },
    datacite_query = \(title, ...) stop("not searched"),
    semanticscholar_query = \(title) stop("not searched"))
  row <- \(mo) mo$table[mo$table$bib_id == 3, ]

  mo <- module_run(p, "ref_accuracy")
  expect_false(row(mo)$incoherent)
  expect_false(row(mo)$not_found)

  cr_authors <- c("Smith", "Jones")
  mo <- module_run(p, "ref_accuracy")
  expect_equal(row(mo)$tier, "search")
  expect_true(row(mo)$incoherent)
  # ref_summary reports the mismatch, not "no match"
  expect_false(row(mo)$no_match)
  expect_true(row(mo)$author_mismatch)
  expect_equal(row(mo)$doi.match, "10.1177/2515245918770963")
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "The work found by its title in Crossref differs", fixed = TRUE)
  expect_equal(mo$traffic_light, "yellow")

  cr_authors <- "Lakens"
  cr_year <- 2012L
  mo <- module_run(p, "ref_accuracy")
  expect_true(row(mo)$year_mismatch)
  expect_false(row(mo)$author_mismatch)

  # and the journal of a CrossRef journal article
  cr_year <- 2018L
  cr_container <- "Psychological Methods"
  mo <- module_run(p, "ref_accuracy")
  expect_true(row(mo)$container_mismatch)
  expect_equal(row(mo)$container.match, "Psychological Methods")

  # a book is not flagged, but its DOI is suggested only when a record has
  # its title by the cited authors
  p2 <- p
  p2$bib$bib_type[p2$bib$bib_id == 3] <- "book"
  mo <- module_run(p2, "ref_accuracy")
  expect_false(row(mo)$incoherent)
  expect_false(row(mo)$not_found)
  cr_container <- NA_character_
  cr_authors <- c("Smith", "Jones")
  s2_authors <- "Smith, John"
  s2_doi <- NA_character_
  local_mocked_bindings(
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), error = logical(0)),
    semanticscholar_query = \(titles) {
      x <- data.frame(query = match(title, titles), doi = s2_doi,
                      url = "https://www.semanticscholar.org/paper/b", title = title,
                      year = 2018L, authors = s2_authors, venue = NA_character_,
                      type = NA_character_, error = FALSE)
      x[!is.na(x$query), ]
    })
  mo <- module_run(p2, "ref_accuracy")
  expect_false(row(mo)$incoherent)
  expect_false(row(mo)$doi_suggested)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "has other authors", fixed = TRUE)
  # Semantic Scholar holds the book by the cited author, without a DOI
  s2_authors <- "Lakens, Daniel"
  mo <- module_run(p2, "ref_accuracy")
  expect_false(row(mo)$incoherent)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "Semantic Scholar has no DOI", fixed = TRUE)
  # with a DOI that Crossref registers as an article in a journal the
  # reference does not name: perhaps a review of the book
  s2_doi <- "10.9/review"
  cr_year <- 2012L  # Crossref's book by other authors is another edition
  local_mocked_bindings(crossref_doi = \(doi, ...) data.frame(DOI = doi, type = "journal-article",
    `container-title` = "Journal of Book Reviews", check.names = FALSE))
  mo <- module_run(p2, "ref_accuracy")
  expect_false(row(mo)$doi_suggested)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "may be a review of the book", fixed = TRUE)
  # a book, or an article in a journal the reference names (here split at a
  # line break, "Ad- vances in Methods ..."), is suggested
  local_mocked_bindings(crossref_doi = \(doi, ...) data.frame(DOI = doi, type = "book",
    `container-title` = NA_character_, check.names = FALSE))
  mo <- module_run(p2, "ref_accuracy")
  expect_true(row(mo)$doi_suggested)
  local_mocked_bindings(crossref_doi = \(doi, ...) data.frame(DOI = doi, type = "journal-article",
    `container-title` = "Advances in Methods and Practices in Psychological Science", check.names = FALSE))
  mo <- module_run(p2, "ref_accuracy")
  expect_true(row(mo)$doi_suggested)
  # a journal whose name is in the cited title only is not named by the
  # reference ("Equivalence Testing for Psychological Research")
  local_mocked_bindings(crossref_doi = \(doi, ...) data.frame(DOI = doi, type = "journal-article",
    `container-title` = "Psychological Research", check.names = FALSE))
  mo <- module_run(p2, "ref_accuracy")
  expect_false(row(mo)$doi_suggested)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "may be a review of the book", fixed = TRUE)
  # a record of the book that lists no authors (as publishers' records of
  # books often do) is suggested; one with other authors is not (above)
  s2_authors <- NA_character_
  local_mocked_bindings(crossref_doi = \(doi, ...) data.frame(DOI = doi, type = "book",
    `container-title` = NA_character_, check.names = FALSE))
  mo <- module_run(p2, "ref_accuracy")
  expect_true(row(mo)$doi_suggested)
})

test_that("ref_accuracy discovers synthetic errors and ignores cosmetic differences", {
  # bib 1 (Eagly & Wood 1999) cites its own DOI and matches its record; each
  # case changes the citation or the record and runs the module on it
  base <- demopaper()
  i <- match(1, base$bib$bib_id)
  m <- match(1, base$bib_match$bib_id)
  # a cited DOI that does not exist is searched by its title, which finds
  # the record
  local_mocked_bindings(
    crossref_doi = \(doi, ...) data.frame(DOI = doi),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE),
    crossref_query = \(ref, ...) {
      q <- unlist(ref)
      x <- data.frame(ref = q, DOI = ifelse(q == base$bib$title[i], base$bib_match$doi[m], NA),
                      title = q, score = 90, year = 1999L, type = "journal-article",
                      `container-title` = "American Psychologist", check.names = FALSE)
      x$author <- rep(list(base$bib_match$authors[[m]]), nrow(x))
      x
    })
  run <- function(edit) {
    p <- edit(base)
    tbl <- module_run(p, "ref_accuracy")$table
    as.list(tbl[tbl$bib_id == 1, ])
  }
  expect_false(run(identity)$incoherent)
  # given and family names swapped in the record: a note, not a flag
  r <- run(\(p) { p$bib_match$authors[[m]][2, c("given", "family")] <- c("Wood", "Wendy"); p })
  expect_false(r$incoherent)
  expect_match(r$name_note, "swapped")

  # errors that should be discovered, with the field that shows them
  errors <- list(
    year = \(p) { p$bib$year[i] <- 2004L; p },
    container = \(p) { p$bib$container[i] <- "Journal of Experimental Social Psychology"; p },
    # the title as parsed and in the reference text, which the module also
    # searches for the record's title
    title = \(p) {
      new <- "Gender differences in moral judgement: A meta-analysis"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    author = \(p) { p$bib$authors[i] <- "Smith, John; Wood, Wendy"; p },
    # a misspelled author, as in "Bernsten" for "Berntsen"
    author = \(p) { p$bib$authors[i] <- "Eagley, Alice H; Wood, Wendy"; p },
    # the DOI of another paper: its record disagrees with the citation
    title = \(p) {
      p$bib$doi[i] <- base$bib_match$doi[2]
      p$bib_match[m, setdiff(names(p$bib_match), "bib_id")] <-
        base$bib_match[2, setdiff(names(p$bib_match), "bib_id")]
      p
    },
    # a DOI that could not be looked up, so the record came from a title
    # search, and that points elsewhere
    doi = \(p) { p$bib$doi[i] <- "10.1037/0003-066x.54.6.4 09"; p$bib_match$score[m] <- 80; p },
    # a fabricated DOI that does not resolve
    unresolved = \(p) { p$bib$doi[i] <- "10.1037/0003-066x.99.9.999"; p$bib_match <- p$bib_match[-m, ]; p },
    # a changed compound surname shares only its last word with the record
    author = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "Smith Jones"
      p$bib$authors[i] <- "Brown Jones, Alice H; Wood, Wendy"; p
    },
    # a surname is not found across the boundary to the given name
    author = \(p) { p$bib$authors[i] <- "Eag, Ly; Wood, Wendy"; p },
    author = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "St. John"
      p$bib$authors[i] <- "Smith, John; Wood, Wendy"; p
    },
    # the record's title continues without a title delimiter
    title = \(p) {
      p$bib_match$title[m] <- "The Origins of Sex Differences in Human Behavior"
      new <- "The Origins of Sex Differences in Human Behaviorism and the Long-Term Effects of Hormone Therapy"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    title = \(p) {
      p$bib_match$title[m] <- "The Origins of Sex Differences in Human Behavior"
      new <- "The Origins of Sex Differences in Human Behavior and the Long-Term Effects of Hormone Therapy"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    # the record's title is found in the reference text only inside a word
    title = \(p) {
      p$bib_match$title[m] <- "Learning changes human behaviour"
      new <- "Unlearning changes human behaviour: A different account of animal cognition"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    # a short record title that is found in the reference text as the journal
    title = \(p) { p$bib_match$title[m] <- "American Psychologist"; p },
    # a misspelling that a spelling-variant rule must not fold ("Rolles")
    title = \(p) {
      new <- sub("Roles", "Rolles", p$bib$title[i], fixed = TRUE)
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    # one word changed
    title = \(p) {
      new <- sub("Human", "Animal", p$bib$title[i], fixed = TRUE)
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    # a DOI written as a URL that was matched by a title search, before
    # add_bib_match() cleaned DOIs, to a work with another DOI
    doi = \(p) {
      p$bib$doi[i] <- "https://doi.org/10.1037/0003-066x.99.9.999"
      p$bib_match$score[m] <- 80; p
    },
    # a one-word journal name that a longer one starts with
    container = \(p) {
      p$bib_match$container[m] <- "Cognition and Emotion"
      p$bib$container[i] <- "Cognition"; p
    },
    # a journal whose name has the cited one's words with another between them
    container = \(p) {
      p$bib_match$container[m] <- "Journal of Experimental Social Psychology"
      p$bib$container[i] <- "Journal of Experimental Psychology"; p
    },
    # another journal that begins with the name of the record's predecessor
    # in Wikidata ("Philosophical Transactions of the Royal Society of London")
    container = \(p) {
      p$bib_match$container[m] <- "Philosophical Transactions of the Royal Society B: Biological Sciences"
      p$bib$container[i] <- "Philosophical Transactions of the Royal Society of London. Series A"; p
    },
    # a word missing from a name Wikidata gives the journal
    container = \(p) {
      p$bib_match$container[m] <- "Journal of the Royal Statistical Society Series B: Statistical Methodology"
      p$bib$container[i] <- "Journal of the Royal Society B: Methodological"; p
    },
    # a journal whose name contains the cited one, and the reverse
    container = \(p) {
      p$bib_match$container[m] <- "Current Directions in Psychological Science"
      p$bib$container[i] <- "Psychological Science"; p
    },
    container = \(p) {
      p$bib_match$container[m] <- "Psychological Science"
      p$bib$container[i] <- "Current Directions in Psychological Science"; p
    },
    container = \(p) { p$bib$container[i] <- "Journal of the American Psychologist"; p },
    # an abbreviated short form is not an acronym that may be left out
    container = \(p) {
      p$bib_match$container[m] <- "Psychological Science"
      p$bib$container[i] <- "Curr. Dir. Psychol. Sci."; p
    },
    # another conference's acronym
    container = \(p) {
      p$bib_match$container[m] <- "Proceedings of the AAAI Conference on Artificial Intelligence"
      p$bib$container[i] <- "IJCAI"; p
    },
    # the first author is not cited first
    author = \(p) { p$bib$authors[i] <- "Wood, Wendy; Eagly, Alice H"; p },
    author = \(p) { p$bib$authors[i] <- "Smith, John; Eagly, Alice H; Wood, Wendy"; p },
    # names without a comma ("Given Family") match on whole words only
    author = \(p) { p$bib$authors[i] <- "Alice H Eagly; John Blackwood"; p },
    author = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "Smith Jones"
      p$bib$authors[i] <- "Alice Brown Jones; Wendy Wood"; p
    }
  )
  for (k in seq_along(errors)) {
    field <- names(errors)[k]
    r <- run(errors[[k]])
    expect_true(r$incoherent, label = paste("error", k, field))
    if (field == "unresolved") {
      expect_equal(r$tier, "unresolved")
    } else {
      expect_true(r[[paste0(field, "_mismatch")]], label = paste("error", k, field))
    }
  }

  # cosmetic differences that should not be flagged
  fine <- list(
    doi_url = \(p) { p$bib$doi[i] <- "https://doi.org/10.1037/0003-066X.54.6.408"; p },
    doi_double_slash = \(p) { p$bib$doi[i] <- "10.1037//0003-066x.54.6.408"; p },
    year_online_first = \(p) { p$bib$year[i] <- 2000L; p },
    journal_abbreviated = \(p) { p$bib$container[i] <- "Am. Psychol."; p },
    # another name Wikidata gives the journal, and its predecessor's name
    # (data-raw/journal_successions.R)
    journal_other_name = \(p) {
      p$bib_match$container[m] <- "Philosophical Transactions of the Royal Society B: Biological Sciences"
      p$bib$container[i] <- "Philosophical Transactions of the Royal Society of London. Series B: Biological Sciences"; p
    },
    journal_predecessor = \(p) {
      p$bib_match$container[m] <- "Journal of the Royal Statistical Society Series B: Statistical Methodology"
      p$bib$container[i] <- "Journal of the Royal Statistical Society. Series B (Methodological)"; p
    },
    # the record has only the main title, and a line-break hyphen in the
    # reference text ("Dif- ferences") hides it there
    subtitle_not_in_record = \(p) {
      p$bib_match$title[m] <- "The Origins of Sex Differences in Human Behavior"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub("Differences", "Dif- ferences", p$text$text[j], fixed = TRUE)
      p
    },
    particle_spacing = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "Le Eagly"
      p$bib$authors[i] <- "LeEagly, Alice H; Wood, Wendy"; p
    },
    nonbreaking_space = \(p) {
      p$bib_match$authors[[m]]$family[2] <- "de\u00a0Wood"
      p$bib$authors[i] <- "Eagly, Alice H; De Wood, Wendy"; p
    },
    given_name_in_family = \(p) { p$bib_match$authors[[m]]$family[1] <- "Alice Eagly"; p },
    comma_below = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "\u021aurcanu"
      p$bib$authors[i] <- "Turcanu, Alice H; Wood, Wendy"; p
    },
    given_first = \(p) { p$bib$authors[i] <- "Alice H Eagly; Wendy Wood"; p },
    vietnamese = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "Nguy\u1ec5n"
      p$bib$authors[i] <- "Nguyen, Alice H; Wood, Wendy"; p
    },
    icelandic = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "\u00de\u00f3rsson"
      p$bib$authors[i] <- "Thorsson, Alice H; Wood, Wendy"; p
    },
    # "O'Connell" read from the PDF as "Connell"
    elided_particle = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "O\u2019Eagly"
      p$bib$authors[i] <- "Eagly, Alice H; Wood, Wendy"; p
    },
    hyphen_part = \(p) {
      p$bib_match$authors[[m]]$family[1] <- "Purdie-Eagly"
      p$bib$authors[i] <- "Eagly, Alice H; Wood, Wendy"; p
    },
    # the record's title in the reference text, followed by an APA description
    software_version = \(p) {
      j <- p$text$text_id == p$bib$text_id[i]
      p$bib_match$title[m] <- "The origins of sex differences in human behavior"
      p$text$text[j] <- sub(p$bib$title[i], "The origins of sex differences in human behavior (Version 1.0.0) [Computer code]", p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- "The origins of sex differences in human behavior (Version 1.0.0) [Computer code]. Zenodo"
      p
    },
    # spellings from VarCon's list, and a compound word ending in one
    british_title = \(p) {
      new <- sub("Behavior", "Behaviour", p$bib$title[i], fixed = TRUE)
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    compound_british_title = \(p) {
      p$bib_match$title[m] <- sub("human behavior", "human neurobehavioral analysis", p$bib_match$title[m], ignore.case = TRUE)
      new <- sub("Human Behavior", "Human Neurobehavioural Analysis", p$bib$title[i], fixed = TRUE)
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    british_spelling = \(p) {
      p$bib_match$container[m] <- "Nature Human Behaviour"
      p$bib$container[i] <- "Nat. Hum. Behav."; p
    },
    american_spelling = \(p) {
      p$bib_match$container[m] <- "Journal of Paediatric Behaviour and Organisation"
      p$bib$container[i] <- "Journal of Pediatric Behavior and Organization"; p
    },
    journal_subtitle = \(p) { p$bib_match$container[m] <- "American Psychologist: Official Journal"; p },
    # words of the journal lost in reading the PDF, which the reference text
    # has ("Perception, & Psychophysics" for "Attention, Perception, &
    # Psychophysics")
    journal_words_lost = \(p) {
      p$bib_match$container[m] <- "Journal of the American Psychologist"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub("American Psychologist", "Journal of the American Psychologist", p$text$text[j], fixed = TRUE)
      p
    },
    # a word split at a line break, and a longer form of the name
    journal_word_split = \(p) { p$bib$container[i] <- "Ameri can Psychologist"; p },
    journal_word_split_abbreviated = \(p) {
      p$bib_match$container[m] <- "Psychological Review"
      p$bib$container[i] <- "Psycho logical Rev."; p
    },
    # an acronym before the full name
    journal_acronym = \(p) {
      p$bib_match$container[m] <- "MMWR. Morbidity and Mortality Weekly Report"
      p$bib$container[i] <- "Morbidity and Mortality Weekly Report"; p
    },
    journal_proceedings = \(p) {
      p$bib_match$container[m] <- "Proceedings of the AAAI Conference on Artificial Intelligence"
      p$bib$container[i] <- "AAAI Conference on Artificial Intelligence"; p
    },
    journal_conference_acronym = \(p) {
      p$bib_match$container[m] <- "Proceedings of the AAAI Conference on Artificial Intelligence"
      p$bib$container[i] <- "AAAI"; p
    },
    journal_wires = \(p) {
      p$bib_match$container[m] <- "Wiley Interdisciplinary Reviews: Cognitive Science"
      p$bib$container[i] <- "WIREs Cognitive Science"; p
    },
    # the citation omits a subtitle that the record has
    record_subtitle = \(p) {
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(": Evolved Dispositions Versus Social Roles", "", p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- "The Origins of Sex Differences in Human Behavior"
      p
    },
    # a title in another language with its English translation in brackets
    translated_title = \(p) {
      new <- "Les origines des différences sexuelles [The origins of sex differences in human behavior: Evolved dispositions versus social roles]"
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub(p$bib$title[i], new, p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- new
      p
    },
    # a title cut short in reading the PDF, whose full title is in the
    # reference text, split at a line break
    title_cut_in_reading = \(p) {
      j <- p$text$text_id == p$bib$text_id[i]
      p$text$text[j] <- sub("Dispositions", "Dis- positions", p$text$text[j], fixed = TRUE)
      p$bib$title[i] <- sub(" Social Roles$", " Social", p$bib$title[i])
      p
    },
    # the record drops the particle the citation keeps
    particle_not_in_record = \(p) { p$bib$authors[i] <- "Eagly, Alice H; Van Wood, Wendy"; p },
    no_authors_read = \(p) { p$bib$authors[i] <- ""; p },
    # a line-break space in a DOI that was then matched by a title search
    doi_space_title_search = \(p) {
      p$bib$doi[i] <- "10.1037/0003-066x.54.6.4 08"; p$bib_match$score[m] <- 80; p
    },
    # a bib_match table without a score column
    no_score_column = \(p) { p$bib_match$score <- NULL; p },
    # an article number read as the journal
    article_number_as_journal = \(p) { p$bib$container[i] <- "Article e17018"; p },
    # a book chapter, whose registry container is a series or platform name
    chapter_container = \(p) {
      p$bib_match$bib_type[m] <- "incollection"
      p$bib_match$container[m] <- "Oxford Handbooks Online"; p
    }
  )
  for (k in names(fine)) {
    expect_false(run(fine[[k]])$incoherent, label = k)
  }

  # known misses: errors the module does not find, recorded so that a change
  # in them is deliberate
  misses <- list(
    extra_author = \(p) { p$bib$authors[i] <- "Eagly, Alice H; Smith, John; Wood, Wendy"; p },
    given_name = \(p) { p$bib$authors[i] <- "Eagly, John; Wood, Wendy"; p }
  )
  for (k in names(misses)) {
    expect_false(run(misses[[k]])$incoherent, label = k)
  }

  # a cited DOI that the registry lists as an alias of the record's DOI is a
  # note, not an incoherence
  r <- run(\(p) { p$bib_match$doi[m] <- "10.1037/alias-of-the-cited"; p })
  expect_true(r$doi_alias)
  expect_false(r$incoherent)
})

test_that("ref_accuracy counts references without a DOI and suggests a DOI for confident matches (#528)", {
  # a reference with no printed DOI that CrossRef matched by title search was
  # counted neither as checked nor as without a DOI, and no DOI was suggested
  # bib 3 printed no DOI; CrossRef found one. The demo cites only its first
  # author, which would be flagged
  demo3 <- \() {
    p <- demopaper()
    p$bib$authors[p$bib$bib_id == 3] <- "Lakens, Daniël; Scheel, Anne M; Isager, Peder M"
    p
  }
  p <- demo3()
  k <- match(3, p$bib_match$bib_id)
  p$bib_match$score[k] <- 85
  mo <- module_run(p, "ref_accuracy")
  tbl <- mo$table
  expect_equal(tbl$tier[tbl$bib_id == 3], "crossref")
  expect_true(tbl$no_doi[tbl$bib_id == 3])
  expect_true(tbl$doi_suggested[tbl$bib_id == 3])
  expect_equal(mo$summary_table$no_doi, sum(tbl$tier %in% c("crossref", "none")))
  expect_equal(mo$summary_table$doi_suggested, 1)
  expect_equal(mo$summary_table$refs_checked + mo$summary_table$no_doi, nrow(tbl))
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, paste0("https://doi.org/", p$bib_match$doi[k]), fixed = TRUE)

  # a low-scoring match with the cited title and details is suggested too;
  # one with a similar title is counted but not suggested
  p$bib_match$score[k] <- 40
  mo <- module_run(p, "ref_accuracy")
  expect_equal(mo$summary_table$doi_suggested, 1)
  p$bib_match$title[k] <- "Equivalence Testing in Psychological Research Settings"
  mo <- module_run(p, "ref_accuracy")
  expect_true(mo$table$no_doi[mo$table$bib_id == 3])
  expect_equal(mo$summary_table$doi_suggested, 0)

  # with no reference that supplied a DOI, in its DOI field or its text, the
  # light is green when a reference without one was compared
  p$bib$doi <- ""
  p$text$text <- gsub("(https?://doi\\.org/|doi:)\\S+", "", p$text$text)
  expect_equal(module_run(p, "ref_accuracy")$traffic_light, "green")

  # a match with another title is another work: no DOI is suggested. A title
  # that continues the cited title after a delimiter (a subtitle) is the same
  # title; one that continues without a delimiter is another title.
  p <- demo3()
  p$bib_match$score[k] <- 85
  p$bib_match$title[k] <- "A different paper by the same authors"
  mo <- module_run(p, "ref_accuracy")
  expect_false(mo$table$doi_suggested[mo$table$bib_id == 3])
  p$bib_match$title[k] <- "Equivalence testing for psychological research in practice"
  mo <- module_run(p, "ref_accuracy")
  expect_false(mo$table$doi_suggested[mo$table$bib_id == 3])
  expect_true(mo$table$title_mismatch[mo$table$bib_id == 3])
  p$bib_match$title[k] <- "Equivalence testing for psychological research: A tutorial"
  mo <- module_run(p, "ref_accuracy")
  expect_true(mo$table$doi_suggested[mo$table$bib_id == 3])
  # the suggestion shows the record it points to
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "Equivalence testing for psychological research: A tutorial (2018)", fixed = TRUE)
})

test_that("ref_accuracy reads DOIs from the reference text and lists entries it could not read", {
  run <- function(p, id = 3) {
    tbl <- module_run(p, "ref_accuracy")$table
    as.list(tbl[tbl$bib_id == id, ])
  }
  set_text <- function(p, id, text) {
    p$text$text[p$text$text_id == p$bib$text_id[p$bib$bib_id == id]] <- text
    p
  }
  base <- demopaper()
  i <- match(3, base$bib$bib_id)
  k <- match(3, base$bib_match$bib_id)
  base$bib_match$score[k] <- 85
  ref <- "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: A tutorial. Advances in Methods and Practices in Psychological Science, 1(2), 259-269."
  base$bib$container[i] <- "Advances in Methods and Practices in Psychological Science"
  base$bib$authors[i] <- "Lakens, Daniel; Scheel, Anne M; Isager, Peder M"

  # a DOI printed as a URL with "%2F" that the PDF reader did not take as the
  # DOI is the cited DOI, and the reference is checked
  p <- set_text(base, 3, paste(ref, "https://doi.org/10.1177%2F2515245918770963"))
  r <- run(p)
  expect_equal(r$tier, "provided")
  expect_false(r$incoherent)

  # a malformed printed DOI that is the DOI found by the title search, without
  # its prefix or punctuation, is a DOI mismatch
  for (bad in c("https://doi.org/2515245918770963", "https://doi.org/101177/2515245918770963")) {
    r <- run(set_text(base, 3, paste(ref, bad)))
    expect_true(r$doi_mismatch, label = bad)
    expect_true(r$incoherent, label = bad)
  }

  # another printed DOI was not looked up: a reading problem, not a flag
  mo <- module_run(set_text(base, 3, paste(ref, "https://doi.org/10.1177/0000000000000001")), "ref_accuracy")
  r <- as.list(mo$table[mo$table$bib_id == 3, ])
  expect_equal(r$tier, "unreadable")
  expect_false(r$incoherent)
  expect_equal(mo$summary_table$unreadable, 1)
  expect_match(paste(unlist(mo$report), collapse = "\n"), "Entries that could not be read reliably")

  # two references read as one entry are listed, not compared, and do not
  # change the traffic light
  p <- demopaper()
  j <- match(1, p$bib$bib_id)
  p$bib$container[j] <- "Nature Reviews Neuroscience"
  p <- set_text(p, 1, "Eagly, A. H., & Wood, W. (1999). The origins of sex differences in human behavior: Evolved dispositions versus social roles. American Psychologist, 54(6), 408-423. https://doi.org/10.1037/ 0003-066x.54.6.408 Bar, M. (2004). Visual objects in context. Nature Reviews Neuroscience, 5(8), 617-629.")
  r <- run(p, 1)
  expect_equal(r$tier, "unreadable")
  expect_false(r$incoherent)
  expect_false(r$container_mismatch)

  # only a DOI was read: when the entry before has no DOI and its title search
  # found this DOI, the DOI is given to it
  p <- base
  frag <- p$bib[i, ]
  frag$bib_id <- 99L
  frag$text_id <- max(p$text$text_id) + 1L
  frag[c("title", "authors", "container")] <- ""
  frag$year <- NA
  frag$doi <- base$bib_match$doi[k]
  p$bib <- rbind(p$bib[1:i, ], frag, p$bib[-(1:i), ])
  tx <- p$text[p$text$text_id == p$bib$text_id[i], ]
  tx$text_id <- frag$text_id
  tx$text <- paste0("https://doi.org/", frag$doi)
  p$text <- rbind(p$text, tx)
  mo <- module_run(p, "ref_accuracy")
  expect_equal(mo$table$tier[mo$table$bib_id == 3], "provided")
  expect_equal(mo$table$tier[mo$table$bib_id == 99], "fragment")
  expect_equal(mo$summary_table$unreadable, 0)
  # without such an entry before it, it is a reading problem
  p$bib$doi[i] <- "10.1177/0956797614520714"
  mo <- module_run(p, "ref_accuracy")
  expect_equal(mo$table$tier[mo$table$bib_id == 99], "unreadable")
})

test_that("ref_accuracy names the missing author, and ref_summary reports the same references", {
  p <- demopaper()
  i <- match(1, p$bib$bib_id)
  p$bib$authors[i] <- "Eagley, Alice H; Wood, Wendy"
  mo <- module_run(p, "ref_accuracy")
  expect_equal(mo$table$author_note[mo$table$bib_id == 1], "Eagly is cited as Eagley")
  # shown letter by letter: the "e" only in the printed name struck through
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "Eagl<del", fixed = TRUE)
  p$bib$authors[i] <- "Wood, Wendy; Eagly, Alice H"
  report <- paste(unlist(module_run(p, "ref_accuracy")$report), collapse = "\n")
  expect_match(report, "the record's first author, Eagly, is not cited first", fixed = TRUE)

  # at min_mismatches = 2 a lone author mismatch is not flagged, and its
  # mismatch column is FALSE, so that ref_summary does not report it
  expect_warning(mo <- module_run(p, "ref_accuracy", min_mismatches = 2), "deprecated")
  r <- mo$table[mo$table$bib_id == 1, ]
  expect_false(r$incoherent)
  expect_false(r$author_mismatch)
})

test_that("ref_pubpeer counts a DOI cited three times once, and runs without url fields (U118)", {
  # 3 citations x 3 lookups gave 9 rows ("You cited 9 references", 18
  # comments); without a url the module stopped
  p <- test_paper("x")
  local_mocked_bindings(
    ref_table = function(paper) data.frame(paper_id = paper$paper_id, bib_id = 1:3,
                                           doi = "10.1/x", text = c("A", "A2", "A3")),
    pubpeer_comments = function(doi) data.frame(doi = doi, total_comments = 2, users = "someone"))
  mo <- module_run(p, "ref_pubpeer")
  expect_match(mo$summary_text, "You cited 1 reference", fixed = TRUE)
  expect_equal(mo$summary_table$pubpeer_comments, 2)
})

test_that("ref_retraction compares DOIs without case (U157)", {
  # a lower-case citation of a RetractionWatch DOI with capitals was missed
  p <- test_paper("x")
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = paper$paper_id, bib_id = 1, doi = "10.1016/s0140-6736(97)11096-0",
               text = "Wakefield"))
  mo <- module_run(p, "ref_retraction")
  expect_equal(mo$table$doi, "10.1016/s0140-6736(97)11096-0")
  expect_match(mo$table$retractionwatch, "Retraction", fixed = TRUE)
})

test_that("ref_miscitation matches DOIs without case, and a missing DOI matches nothing (U117, U157)", {
  paper <- demopaper()
  test_doi <- "10.1037/0003-066x.54.6.408"
  db <- data.frame(doi = c(toupper(test_doi), NA), reftext = c("Ref", "None"),
                   warning = c("Warn", "No DOI"))
  mo <- module_run(paper, "ref_miscitation", db = db)
  expect_equal(unique(mo$table$doi), test_doi)
  expect_false("No DOI" %in% mo$table$warning)
})

test_that("ref_replication drops a citation of the original only when the same paper cites the replication (U116)", {
  # paper B citing a replication hid paper A's citation of the original
  fl <- FLoRA()
  row <- fl[!is.na(fl$doi_r) & nzchar(fl$doi_r) & !is.na(fl$doi_o) & nzchar(fl$doi_o), ][1, ]
  pa <- test_paper("x"); pa$paper_id <- "A"
  pb <- test_paper("y"); pb$paper_id <- "B"
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = c("A", "B"), bib_id = 1L, doi = c(row$doi_o, row$doi_r),
               text = c("original", "replication")))
  mo <- module_run(paperlist(pa, pb), "ref_replication")
  # the replication paper B cites is still listed for paper A
  expect_true(tolower(row$doi_r) %in% tolower(mo$table$replication_doi[mo$table$paper_id == "A"]))
})

test_that("ref_replication does not suggest the citing paper itself (#695)", {
  # a replication report cites its original, and was told to cite itself
  fl <- FLoRA()
  row <- fl[!is.na(fl$doi_r) & nzchar(fl$doi_r) & !is.na(fl$doi_o) & nzchar(fl$doi_o), ][1, ]
  n_rep <- sum(tolower(fl$doi_o) == tolower(row$doi_o))
  pa <- test_paper("x"); pa$paper_id <- "A"
  pa$info$doi <- paste0("https://doi.org/", toupper(row$doi_r))
  pb <- test_paper("y"); pb$paper_id <- "B"
  local_mocked_bindings(ref_table = function(paper)
    data.frame(paper_id = c("A", "B"), bib_id = 1L, doi = row$doi_o, text = "original"))
  mo <- module_run(paperlist(pa, pb), "ref_replication")
  expect_false(tolower(row$doi_r) %in% tolower(mo$table$replication_doi[mo$table$paper_id == "A"]))
  expect_equal(sum(mo$table$paper_id == "A"), n_rep - 1)
  # another paper citing the same original is still told about it
  expect_true(tolower(row$doi_r) %in% tolower(mo$table$replication_doi[mo$table$paper_id == "B"]))
})

test_that("ref_summary labels unmatched references and keeps one row per reference (U119)", {
  # no_match with every *_mismatch FALSE gave NA; two replications repeated
  # the reference
  p <- demopaper()
  pid <- p$paper_id; b <- unique(ref_table(p)$bib_id)
  acc <- data.frame(paper_id = pid, bib_id = b[1:3], no_match = c(TRUE, FALSE, TRUE),
                    title_mismatch = c(FALSE, TRUE, FALSE), doi_mismatch = FALSE,
                    tier = c("none", "provided", "unresolved"))
  rep_tbl <- data.frame(paper_id = pid, bib_id = b[3], replication_type = c("direct", "conceptual"))
  fake <- structure(list(module = "ref_replication", table = rep_tbl, paper = p,
                         summary_table = data.frame(paper_id = pid),
                         prev_outputs = list(ref_accuracy = list(table = acc))),
                    class = "metacheck_module_output")
  mo <- module_run(fake, "ref_summary")
  expect_equal(nrow(mo$table), length(b))
  expect_equal(mo$table$accuracy_mismatch[mo$table$bib_id == b[1]], "no match")
  # a cited DOI that does not exist is not "no match"
  expect_equal(mo$table$accuracy_mismatch[mo$table$bib_id == b[3]], "DOI not found")
})

test_that("ref_accuracy reads bibr 12.x author lists and article types", {
  # bibr 12.x keeps the author list as printed ("Adam, H., Shirako, A., &
  # Maddux, W. W") and calls a journal article "journal_article"
  p <- read(test_path("fixtures", "bibr12", "preprint.json"))
  # no searches for the references without a DOI
  local_mocked_bindings(
    crossref_query = \(ref, ...) data.frame(ref = unlist(ref), DOI = NA_character_, error = "offline"),
    datacite_query = \(title, ...) data.frame(query = seq_along(title), doi = NA_character_,
      title = NA_character_, year = NA_integer_, authors = NA_character_, error = TRUE),
    semanticscholar_query = \(title) data.frame(query = seq_along(title), doi = NA_character_,
      url = NA_character_, title = NA_character_, year = NA_integer_,
      authors = NA_character_, venue = NA_character_, type = NA_character_, error = TRUE))
  mo <- module_run(p, "ref_accuracy")
  tbl <- mo$table[mo$table$tier == "provided", ]
  expect_true(any(tbl$bib_type.match == "journal_article"))
  expect_lt(sum(tbl$author_mismatch), 3)

  k <- which(p$bib$bib_id == tbl$bib_id[!tbl$incoherent][1])
  q <- p
  q$bib$container[k] <- "A Completely Different Journal Name"
  r <- module_run(q, "ref_accuracy")$table
  expect_true(r$container_mismatch[r$bib_id == p$bib$bib_id[k]])
  q <- p
  q$bib$authors[k] <- "Nobody, A., & Else, B"
  r <- module_run(q, "ref_accuracy")$table
  expect_true(r$author_mismatch[r$bib_id == p$bib$bib_id[k]])

  # record surnames are whole words of the printed list, the first one first,
  # and a compound surname may be cited by its last part
  fam <- tbl$authors.match[!tbl$incoherent][[1]]$family
  m <- match(p$bib$bib_id[k], p$bib_match$bib_id)
  check <- function(authors, family = fam) {
    q <- p
    q$bib$authors[k] <- authors
    q$bib_match$authors[[m]]$family <- family
    r <- module_run(q, "ref_accuracy")$table
    r$incoherent[r$bib_id == p$bib$bib_id[k]]
  }
  printed <- \(f) paste0(f, ", A.", collapse = ", ")
  expect_false(check(printed(fam)))
  expect_true(check(printed(paste0("Mc", fam))))
  expect_true(check(printed(rev(fam))))
  expect_false(check(printed(c("Vaughns", fam[-1])), c("Purdie-Vaughns", fam[-1])))
  expect_false(check(printed(c("Le Doux", fam[-1])), c("LeDoux", fam[-1])))
})

test_that("ref_accuracy compares a reference without a DOI only with a record it would suggest", {
  mock_no_search()
  # an uncertain title-search match is not compared, so ref_summary shows no
  # mismatch for it
  # no CrossRef dates for the record, unless a test gives them
  dates <- data.frame()
  local_mocked_bindings(crossref_doi = \(doi, ...) if (nrow(dates)) dates else data.frame(DOI = doi),
                        doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE))
  p <- demopaper()
  k <- match(3, p$bib_match$bib_id)
  p$bib_match$year[k] <- 1900L
  p$bib_match$score[k] <- 40
  q <- p
  q$bib_match$title[k] <- "Equivalence Testing in Psychological Research Settings"
  tbl <- module_run(q, "ref_accuracy")$table
  expect_equal(tbl$tier[tbl$bib_id == 3], "crossref")
  expect_false(tbl$year_mismatch[tbl$bib_id == 3])
  expect_false(tbl$incoherent[tbl$bib_id == 3])
  # whatever its score, a match with the cited title is compared
  tbl <- module_run(p, "ref_accuracy")$table
  expect_true(tbl$year_mismatch[tbl$bib_id == 3])

  # a confident match with the same title but other authors, journal or year
  # is flagged, with the record, and its DOI is not suggested
  p$bib_match$score[k] <- 85
  mo <- module_run(p, "ref_accuracy")
  tbl <- mo$table
  expect_true(tbl$incoherent[tbl$bib_id == 3])
  expect_true(tbl$year_mismatch[tbl$bib_id == 3])
  expect_false(tbl$doi_suggested[tbl$bib_id == 3])
  expect_true(tbl$no_doi[tbl$bib_id == 3])
  expect_equal(mo$traffic_light, "yellow")
  expect_match(mo$summary_text, "differs from its record (possible citation error)", fixed = TRUE)
  report <- paste(unlist(mo$report), collapse = "\n")
  expect_match(report, "No DOI cited. The work found by its title in Crossref differs", fixed = TRUE)
  # a title that differs in one word from the record it was matched to
  q <- p
  q$bib_match$year[k] <- q$bib$year[q$bib$bib_id == 3]
  q$bib$title[q$bib$bib_id == 3] <- "Equivalence Testing for Psychiatric Research"
  r <- module_run(q, "ref_accuracy")$table
  expect_true(r$incoherent[r$bib_id == 3])
  expect_true(r$title_mismatch[r$bib_id == 3])
  expect_match(report, paste0("https://doi.org/", p$bib_match$doi[k]), fixed = TRUE)
  p$bib_match$year[k] <- tbl$year.orig[tbl$bib_id == 3]
  p$bib_match$authors[[k]] <- data.frame(given = "Ann", family = "Other")
  tbl <- module_run(p, "ref_accuracy")$table
  expect_true(tbl$author_mismatch[tbl$bib_id == 3])
  expect_false(tbl$doi_suggested[tbl$bib_id == 3])
  # differing authors alone need min_mismatches = 1, as the title is not
  # compared; the demo cites only the first of the record's three authors
  p <- demopaper()
  p$bib_match$score[k] <- 85
  tbl <- module_run(p, "ref_accuracy")$table
  expect_equal(tbl$author_note[tbl$bib_id == 3], "Scheel is in the record but not in the reference; Isager is in the record but not in the reference")
  expect_warning(tbl <- module_run(p, "ref_accuracy", min_mismatches = 2)$table, "deprecated")
  expect_false(tbl$incoherent[tbl$bib_id == 3])
  expect_true(tbl$doi_suggested[tbl$bib_id == 3])

  # a match without a DOI is not suggested, nor counted as suggested
  p$bib_match$score[k] <- 90
  p$bib_match$doi[k] <- NA_character_
  mo <- module_run(p, "ref_accuracy")
  expect_equal(mo$summary_table$doi_suggested, 0)

  # in a paper list, a paper that was not looked up has no DOIs that "could
  # not be found"
  a <- demopaper()
  b <- demopaper()
  b$paper_id <- "nolookup"
  b$bib_match <- b$bib_match[0, ]
  mo <- module_run(paperlist(a, b), "ref_accuracy")
  st <- mo$summary_table[mo$summary_table$paper_id == "nolookup", ]
  expect_equal(st$refs_checked, 0)
  expect_equal(st$incoherent, 0)
  expect_true(all(mo$table$tier[mo$table$paper_id == "nolookup"] %in%
                    c("not_checked", "none")))
  expect_match(mo$summary_text, "not looked up at Crossref")

  # a reference whose title was not read is not compared with, and gets no
  # DOI from, a confident match with another title
  p <- demopaper()
  i <- p$bib$bib_id == 3
  p$bib$title[i] <- ""
  p$bib_match$score[k] <- 85
  p$bib_match$title[k] <- "Completely Unrelated Paper Title"
  tbl <- module_run(p, "ref_accuracy")$table
  expect_false(tbl$incoherent[tbl$bib_id == 3])
  expect_false(tbl$doi_suggested[tbl$bib_id == 3])

  # below suggest_score, an article with almost the same title is compared
  p <- demopaper()
  p$bib_match$title[k] <- p$bib$title[i]
  p$bib_match$score[k] <- 60
  p$bib_match$year[k] <- 2010L
  tbl <- module_run(p, "ref_accuracy")$table
  expect_true(tbl$year_mismatch[tbl$bib_id == 3])
  # a book with the same title in another year is another edition: not
  # flagged, and its DOI not suggested (its authors are compared)
  p$bib_match$authors[[k]] <- data.frame(given = "Daniël", family = "Lakens")
  p$bib$bib_type[i] <- "book"
  p$bib_match$bib_type[k] <- "book"
  p$bib_match$score[k] <- 100
  tbl <- module_run(p, "ref_accuracy")$table
  expect_false(tbl$incoherent[tbl$bib_id == 3])
  expect_false(tbl$doi_suggested[tbl$bib_id == 3])

  # a cited preprint is not compared on its year or journal with a published
  # version, which is noted instead of suggested
  p <- demopaper()
  p$bib_match$score[k] <- 85
  p$bib_match$year[k] <- 2010L
  p$bib_match$authors[[k]] <- data.frame(given = "Daniël", family = "Lakens")
  p$bib$container[i] <- "PsyArXiv"
  mo <- module_run(p, "ref_accuracy")
  r <- mo$table[mo$table$bib_id == 3, ]
  expect_false(r$incoherent)
  expect_false(r$doi_suggested)
  expect_equal(r$published_doi, p$bib_match$doi[k])
  expect_match(paste(unlist(mo$report), collapse = "\n"), "Cites a preprint", fixed = TRUE)

  # a cited journal article whose same-title match is a preprint is searched
  # by title, and flagged when the search finds only the preprint
  hits <- \(types) {
    x <- data.frame(ref = p$bib$title[i], DOI = paste0("10.31234/", seq_along(types)),
                    title = p$bib$title[i], score = 90, year = p$bib$year[i],
                    type = types, `container-title` = NA_character_, check.names = FALSE)
    x$author <- rep(list(data.frame(given = "Daniël", family = "Lakens")), length(types))
    x
  }
  found <- "posted-content"
  local_mocked_bindings(
    crossref_query = \(ref, ...) hits(found),
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), error = logical(0)),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE))
  q <- demopaper()
  q$bib_match$score[k] <- 85
  q$bib_match$bib_type[k] <- "misc"
  q$bib$container[i] <- "Journal of Imaginary Results"
  r <- module_run(q, "ref_accuracy")
  expect_true(r$table$preprint_only[r$table$bib_id == 3])
  expect_true(r$table$incoherent[r$table$bib_id == 3])
  expect_match(paste(unlist(r$report), collapse = "\n"), "only a preprint was found", fixed = TRUE)
  # not when it is cited from a conference
  q$bib$container[i] <- "ICML"
  r <- module_run(q, "ref_accuracy")$table
  expect_false(r$preprint_only[r$bib_id == 3])
  # a journal named like a conference is a journal
  q$bib$container[i] <- "Proceedings of the National Academy of Sciences"
  r <- module_run(q, "ref_accuracy")$table
  expect_true(r$preprint_only[r$bib_id == 3])
  # not when the search also finds the published article, after the preprint
  found <- c("posted-content", "journal-article")
  r <- module_run(q, "ref_accuracy")$table
  expect_false(r$preprint_only[r$bib_id == 3])
  # nor when Crossref finds only the preprint but Semantic Scholar finds the
  # journal article, or the Semantic Scholar search fails
  found <- "posted-content"
  s2 <- data.frame(query = 1L, doi = NA_character_, url = "https://www.semanticscholar.org/paper/abc",
                   title = p$bib$title[i], year = p$bib$year[i], authors = "Lakens, Daniël",
                   venue = "Journal of Imaginary Results", type = "JournalArticle", error = FALSE)
  local_mocked_bindings(semanticscholar_query = \(title) {
    s2$query <- match(p$bib$title[i], title)
    s2[!is.na(s2$query), ]
  })
  r <- module_run(q, "ref_accuracy")$table
  expect_false(r$preprint_only[r$bib_id == 3])
  s2 <- data.frame(query = 1L, doi = NA_character_, url = NA_character_, title = NA_character_,
                   year = NA_integer_, authors = NA_character_, venue = NA_character_,
                   type = NA_character_, error = TRUE)
  r <- module_run(q, "ref_accuracy")$table
  expect_false(r$preprint_only[r$bib_id == 3])
  # but a Semantic Scholar record of unknown type does not clear it
  s2 <- data.frame(query = 1L, doi = NA_character_, url = NA_character_, title = p$bib$title[i],
                   year = p$bib$year[i], authors = "Lakens, Daniël", venue = NA_character_,
                   type = NA_character_, error = FALSE)
  r <- module_run(q, "ref_accuracy")$table
  expect_true(r$preprint_only[r$bib_id == 3])

  # a cited year that is the year the DOI was created, before the work was
  # published, is a note; one near the
  # online date is neither a note nor a flag
  p$bib$container[i] <- NA_character_
  y <- p$bib$year[i]
  p$bib_match$year[k] <- y + 2L
  dates <- data.frame(DOI = p$bib_match$doi[k], issued = paste0(y + 2, "; 1"),
                      created = paste0(y, "; 5; 2"))
  r <- module_run(p, "ref_accuracy")$table
  r <- r[r$bib_id == 3, ]
  expect_false(r$incoherent)
  expect_match(r$year_note, "near the year of the DOI.s creation")
  # but not when the work was published more than a year after that
  p$bib_match$year[k] <- y + 3L
  dates$issued <- paste0(y + 3, "; 1")
  r <- module_run(p, "ref_accuracy")$table
  expect_true(r$year_mismatch[r$bib_id == 3])
  p$bib_match$year[k] <- y + 2L
  dates$issued <- paste0(y + 2, "; 1")
  dates$`published-online` <- paste0(y, "; 6")
  r <- module_run(p, "ref_accuracy")$table
  expect_false(r$incoherent[r$bib_id == 3])
  expect_true(is.na(r$year_note[r$bib_id == 3]))
})

test_that("ref_pubpeer leaves Statcheck's comment out of the count (#695, point 14)", {
  p <- demopaper()
  dois <- unique(stats::na.omit(p$bib$doi))[1:3]
  local_mocked_bindings(pubpeer_comments = function(doi) data.frame(
    doi = dois, total_comments = c(1, 3, 2), url = "https://pubpeer.com/x",
    users = c("Statcheck", "Statcheck, A. Reader", "A. Reader, B. Writer")))
  mo <- module_run(p, "ref_pubpeer")
  # Statcheck alone: left out; with others: one comment less, and not a commenter
  expect_setequal(mo$table$doi, dois[2:3])
  expect_equal(mo$table$total_comments[mo$table$doi == dois[2]], 2)
  expect_equal(mo$table$users[mo$table$doi == dois[2]], "A. Reader")
  expect_match(paste(mo$report, collapse = " "), "Commenters", fixed = TRUE)
})

test_that("ref_accuracy handles markup, footers, fragments, Greek, and title spills", {
  mock_no_search()
  local_mocked_bindings(
    crossref_doi = \(doi, ...) data.frame(DOI = doi),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA)
  )
  base <- demopaper()
  i <- match(1, base$bib$bib_id)
  m <- match(1, base$bib_match$bib_id)
  ti <- match(base$bib$text_id[i], base$text$text_id)
  run1 <- \(p) {
    tbl <- module_run(p, "ref_accuracy", semantic_scholar = FALSE)$table
    as.list(tbl[tbl$bib_id == 1, ])
  }

  # 1. Escaped HTML tags in record title (&lt;em&gt;)
  p <- base
  p$bib$title[i] <- "Growth of Quercus robur in temperate forests"
  p$bib_match$title[m] <- "Growth of &lt;em&gt;Quercus robur&lt;/em&gt; in temperate forests"
  expect_false(run1(p)$title_mismatch)

  # 2. Inline scientific superscripts preserved (R^2 != R^3), trailing footnote still stripped
  p <- base
  p$bib$title[i] <- "Interpreting R<sup>2</sup> in ecology"
  p$bib_match$title[m] <- "Interpreting R<sup>3</sup> in ecology"
  expect_true(run1(p)$title_mismatch)
  p$bib_match$title[m] <- "Interpreting R2 in ecology"
  expect_false(run1(p)$title_mismatch)

  # 3. Spelled-out Greek letters (alpha/beta/delta15N) and negative control (alpha != beta)
  p <- base
  p$bib$title[i] <- "Patterns of alpha-diversity and beta diversity with delta15N"
  p$bib_match$title[m] <- "Patterns of \u03b1-diversity and \u03b2-diversity with \u03b4<sup>15</sup>N"
  expect_false(run1(p)$title_mismatch)
  p$bib_match$title[m] <- "Patterns of \u03b2-diversity and \u03b2-diversity with \u03b4<sup>15</sup>N"
  expect_true(run1(p)$title_mismatch)

  # 4. Ellipsis-shortened author lists (both in authors.orig and dropped into text),
  #    with negative controls for wrong first or wrong final author
  p <- base
  p$bib_match$authors[[m]] <- data.frame(
    given = LETTERS[1:10],
    family = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon",
               "Zeta", "Eta", "Theta", "Iota", "Omega")
  )
  p$bib$authors[i] <- "Alpha, A; Beta, B; \u2026; Omega, J"
  expect_false(run1(p)$author_mismatch)
  # GROBID dropped the ellipsis from authors.orig, keeping it only in text
  p2 <- p
  p2$bib$authors[i] <- "Alpha, A; Beta, B; Omega, J"
  p2$text$text[ti] <- paste0("Alpha, A., Beta, B., \u2026 Omega, J. (1999). ", p2$bib$title[i], ".")
  expect_false(run1(p2)$author_mismatch)
  # negative control: wrong final author after ellipsis is still flagged
  p3 <- p
  p3$bib$authors[i] <- "Alpha, A; Beta, B; \u2026; Wronglast, J"
  expect_true(run1(p3)$author_mismatch)
  # negative control: wrong first author before ellipsis is still flagged
  p4 <- p
  p4$bib$authors[i] <- "Beta, B; Alpha, A; \u2026; Omega, J"
  expect_true(run1(p4)$author_mismatch)

  # 5. "et al." in text when title is hyphenated at a line break
  p <- base
  p$bib_match$authors[[m]] <- data.frame(given = c("A", "B", "C"), family = c("Alpha", "Beta", "Gamma"))
  p$bib$authors[i] <- "Alpha, A"
  p$bib$title[i] <- "Ecological dynamics of temperate forests"
  p$bib_match$title[m] <- p$bib$title[i]
  p$text$text[ti] <- "Alpha, A., et al. (1999). Eco- logical dynamics of temperate forests. American Psychologist."
  expect_false(run1(p)$author_mismatch)

  # 6. Former journal name with Crossref subtitle ("Proceedings of the Royal Society B: Biological Sciences")
  p <- base
  p$bib$container[i] <- "Proceedings of the Royal Society of London B"
  p$bib_match$container[m] <- "Proceedings of the Royal Society B: Biological Sciences"
  expect_false(run1(p)$container_mismatch)

  # 7. End of title spilled into container.orig ("Alaska. Journal of Experimental Zoology")
  #    and negative control (unrelated prefix before period)
  p <- base
  p$bib$title[i] <- "Population dynamics of salmon in"
  p$bib_match$title[m] <- "Population dynamics of salmon in Alaska"
  p$bib$container[i] <- "Alaska. Journal of Experimental Zoology"
  p$bib_match$container[m] <- "Journal of Experimental Zoology"
  p$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Population dynamics of salmon in Alaska. Journal of Experimental Zoology, 12, 1-10."
  r <- run1(p)
  expect_false(r$container_mismatch)
  expect_false(r$title_mismatch)
  # negative control: prefix is NOT the end of the title -> still flagged
  p_bad <- p
  p_bad$bib$container[i] <- "Biogeochemistry. Journal of Experimental Zoology"
  expect_true(run1(p_bad)$container_mismatch)

  # 8. Anchored in_text rescues cut title at parenthesis, but does NOT let a
  #    journal name in text match a wrong single-word title ("Ecology")
  p <- base
  p$bib$title[i] <- "Effects of fire on Pinus sylvestris"
  p$bib_match$title[m] <- "Effects of fire on Pinus sylvestris (Pinaceae) regeneration"
  p$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Effects of fire on Pinus sylvestris (Pinaceae) regeneration. American Psychologist, 54, 1-10."
  expect_false(run1(p)$title_mismatch)
  # negative control: real title error still flagged even with trailing Wiley footer
  p_eco <- base
  p_eco$bib$title[i] <- "Ecology"
  p_eco$bib$container[i] <- "Annual Review of Ecology and Evolution"
  p_eco$bib_match$title[m] <- "Annual Review of Ecology and Evolution"
  p_eco$bib_match$container[m] <- "Annual Review of Ecology and Evolution"
  p_eco$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Ecology. Annual Review of Ecology and Evolution, 30, 1-10."
  expect_true(run1(p_eco)$title_mismatch)

  # 9. Fragment entry with stray DOI-derived year ("1007") and DOI-derived year ignored
  p <- base
  p$bib$year[i] <- 1007L
  p$bib$doi[i] <- "10.1007/s00442-012-2345-6"
  p$bib_match$doi[m] <- "10.1007/s00442-012-2345-6"
  p$bib_match$year[m] <- 2012L
  expect_false(run1(p)$year_mismatch)
  # DOI-only fragment that also carries stray year 1007 is still caught as unreadable/fragment
  p_frag <- base
  p_frag$bib$title[i] <- ""
  p_frag$bib$authors[i] <- ""
  p_frag$bib$container[i] <- ""
  p_frag$bib$year[i] <- 1007L
  expect_equal(run1(p_frag)$tier, "unreadable")
  expect_false(run1(p_frag)$incoherent)

  # 10. Wiley download footer: mid-entry/contaminating -> unreadable;
  #     trailing non-contaminating footer keeps a real citation error flagged
  p <- base
  footer <- "Downloaded from https://besjournals.onlinelibrary.wiley.com/doi/10.1111/123 by Test Univ, Wiley Online Library on [12/05/2023]. See the Terms and Conditions (https://onlinelibrary.wiley.com/terms-and-conditions) on Wiley Online Library."
  p$text$text[ti] <- paste("Eagly, A. H., & Wood, W. (1999).", footer, p$bib$title[i], "American Psychologist.")
  expect_equal(run1(p)$tier, "unreadable")
  expect_false(run1(p)$incoherent)
  # trailing footer + real surname typo still flags the surname error
  p_trail <- base
  p_trail$bib$authors[i] <- "Eagley, Alice H; Wood, Wendy"
  p_trail$text$text[ti] <- paste(p_trail$text$text[ti], footer)
  r_trail <- run1(p_trail)
  expect_equal(r_trail$tier, "provided")
  expect_true(r_trail$author_mismatch)

  # 11. Deduplication prefers unscored DOI hit over scored title search
  p <- base
  dup <- p$bib_match[m, ]
  dup$score <- 95
  dup$title <- "Wrong Scored Title Match"
  p$bib_match <- rbind(dup, p$bib_match)
  expect_false(run1(p)$title_mismatch)
})

test_that("ref_accuracy title search is invariant to paper order for shared titles", {
  # Two papers cite the same title with different years and venue classes;
  # candidate selection must evaluate each reference independently.
  p1 <- demopaper(); p1$paper_id <- "P1"
  p2 <- demopaper(); p2$paper_id <- "P2"
  k1 <- match(3, p1$bib$bib_id)
  p1$bib_match <- p1$bib_match[p1$bib_match$bib_id != 3, ]
  p2$bib_match <- p2$bib_match[p2$bib_match$bib_id != 3, ]
  title <- p1$bib$title[k1]
  p1$bib$year[k1] <- 2018L
  p1$bib$container[k1] <- "PsyArXiv"
  p1$bib$authors[k1] <- "Lakens, Daniël"
  p2$bib$year[k1] <- 2020L
  p2$bib$container[k1] <- "Advances in Methods and Practices in Psychological Science"
  p2$bib$authors[k1] <- "Lakens, Daniël"

  cr_hits <- data.frame(
    ref = rep(title, 2),
    DOI = c("10.31234/pre", "10.1177/pub"),
    title = rep(title, 2),
    score = c(90, 88),
    year = c(2018L, 2020L),
    type = c("posted-content", "journal-article"),
    `container-title` = c(NA_character_, "Advances in Methods and Practices in Psychological Science"),
    check.names = FALSE
  )
  cr_hits$author <- rep(list(data.frame(given = "Daniël", family = "Lakens")), 2)

  local_mocked_bindings(
    crossref_query = \(ref, ...) cr_hits,
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), type = character(0), error = logical(0)),
    semanticscholar_query = \(title) data.frame(query = integer(0), doi = character(0), url = character(0),
      title = character(0), year = integer(0), authors = character(0), venue = character(0), type = character(0), error = logical(0)),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA),
    crossref_doi = \(doi, ...) data.frame(DOI = doi)
  )
  m12 <- module_run(paperlist(p1, p2), "ref_accuracy", semantic_scholar = FALSE)$table
  m21 <- module_run(paperlist(p2, p1), "ref_accuracy", semantic_scholar = FALSE)$table
  r12_p2 <- m12[m12$paper_id == "P2" & m12$bib_id == 3, ]
  r21_p2 <- m21[m21$paper_id == "P2" & m21$bib_id == 3, ]
  expect_equal(r12_p2$doi.match, "10.1177/pub")
  expect_equal(r21_p2$doi.match, "10.1177/pub")
  expect_equal(r12_p2$incoherent, r21_p2$incoherent)
  expect_equal(r12_p2$doi_suggested, r21_p2$doi_suggested)
})

test_that("ref_accuracy handles PDF diacritics, journal variants, author shifts, and candidate ranking", {
  mock_no_search()
  local_mocked_bindings(
    crossref_doi = \(doi, ...) data.frame(DOI = doi),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA)
  )
  base <- demopaper()
  i <- match(1, base$bib$bib_id)
  m <- match(1, base$bib_match$bib_id)
  ti <- match(base$bib$text_id[i], base$text$text_id)
  run1 <- \(p) {
    tbl <- module_run(p, "ref_accuracy", semantic_scholar = FALSE)$table
    as.list(tbl[tbl$bib_id == 1, ])
  }

  # 1. Trademark symbol in journal name ("Thyroid\u00ae" vs "Thyroid")
  p <- base
  p$bib$container[i] <- "Thyroid"
  p$bib_match$container[m] <- "Thyroid\u00ae"
  expect_false(run1(p)$container_mismatch)

  # 2. ISO-4 short abbreviations not broken by VarCon ("J. Coll. Stud. Ret.", "Eur. J. For. Res.")
  p <- base
  p$bib$container[i] <- "J. Coll. Stud. Ret."
  p$bib_match$container[m] <- "Journal of College Student Retention: Research, Theory & Practice"
  expect_false(run1(p)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Eur. J. For. Res."
  p$bib_match$container[m] <- "European Journal of Forest Research"
  expect_false(run1(p)$container_mismatch)

  # 3. Latin "et" stopword and optional "Section"/"Series" before single section letter
  p <- base
  p$bib$container[i] <- "Biochim Biophys Acta"
  p$bib_match$container[m] <- "Biochimica et Biophysica Acta (BBA) - Bioenergetics"
  expect_false(run1(p)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Acta Crystallogr. D. Biol. Crystallogr"
  p$bib_match$container[m] <- "Acta Crystallographica Section D Biological Crystallography"
  expect_false(run1(p)$container_mismatch)
  # negative control: Section A must NOT match Section D
  p_sec <- p
  p_sec$bib$container[i] <- "Acta Crystallogr. A. Biol. Crystallogr"
  expect_true(run1(p_sec)$container_mismatch)

  # 4. One-word journal with parenthetical city ("Cancers (Basel)") or delimited subtitle ("Flora - ...")
  p <- base
  p$bib$container[i] <- "Cancers (Basel)"
  p$bib_match$container[m] <- "Cancers"
  expect_false(run1(p)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Flora"
  p$bib_match$container[m] <- "Flora - Morphology, Distribution, Functional Ecology of Plants"
  expect_false(run1(p)$container_mismatch)
  # negative control: "Science" must NOT match "Science Advances" (no subtitle separator)
  p_sci <- base
  p_sci$bib$container[i] <- "Science"
  p_sci$bib_match$container[m] <- "Science Advances"
  expect_true(run1(p_sci)$container_mismatch)

  # 5. Reverse Royal Society B succession with subtitle & subtitle-only container.orig
  p <- base
  p$bib$container[i] <- "Proceedings of the Royal Society B: Biological Sciences"
  p$bib_match$container[m] <- "Proceedings of the Royal Society of London. Series B: Biological Sciences"
  expect_false(run1(p)$container_mismatch)
  # negative control: wrong subtitle still flags
  p_sub <- p
  p_sub$bib$container[i] <- "Proceedings of the Royal Society B: Mathematical Sciences"
  expect_true(run1(p_sub)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Biological Sciences"
  p$bib_match$container[m] <- "Proceedings of the Royal Society B: Biological Sciences"
  p$text$text[ti] <- paste0(
    "Eagly, A. H., & Wood, W. (1999). ", p$bib$title[i],
    ". Proceedings of the Royal Society of London. Series B, Biological Sciences, 276, 3819-3825.")
  expect_false(run1(p)$container_mismatch)

  # 6. Glued bare footnote digit at end of title (vs spaced number / COVID19 vs COVID20)
  p <- base
  p$bib$title[i] <- "Analysis of genetic diversity via microsatellite markers"
  p$bib_match$title[m] <- "Analysis of genetic diversity via microsatellite markers12"
  expect_false(run1(p)$title_mismatch)
  p_q <- base
  p_q$bib$title[i] <- "Job satisfaction and employee \"citizenship\"1"
  p_q$bib_match$title[m] <- "Job satisfaction and employee \"citizenship\""
  expect_false(run1(p_q)$title_mismatch)
  # negative controls: spaced number and differing suffix digits must stay flagged
  p_sp <- base
  p_sp$bib$title[i] <- "Memory across adult life"
  p_sp$bib_match$title[m] <- "Memory across adult life 2"
  p_sp$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Memory across adult life. American Psychologist."
  expect_true(run1(p_sp)$title_mismatch)
  p_cov <- base
  p_cov$bib$title[i] <- "Epidemiology of COVID19"
  p_cov$bib_match$title[m] <- "Epidemiology of COVID20"
  p_cov$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Epidemiology of COVID19. American Psychologist."
  expect_true(run1(p_cov)$title_mismatch)

  # 7. Generational suffix, particle-shortened surname, detached PDF diacritics, Vancouver shift
  p <- base
  p$bib$authors[i] <- "Hatanaka, T; Galetti, P M"
  p$bib_match$authors[[m]] <- data.frame(given = c("Terumi", "Pedro"), family = c("Hatanaka", "Galetti Jr."))
  expect_false(run1(p)$author_mismatch)

  p <- base
  p$bib$authors[i] <- "Hubert, M; Der Veeken, S V"
  p$bib_match$authors[[m]] <- data.frame(given = c("Mia", "Stephan"), family = c("Hubert", "Van der Veeken"))
  expect_false(run1(p)$author_mismatch)

  p <- base
  p$bib$authors[i] <- "Cadena, C D; Jime, I"
  p$bib_match$authors[[m]] <- data.frame(given = c("Carlos", "Ivan"), family = c("Cadena", "Jim\u00e9nez"))
  p$text$text[ti] <- paste0("Cadena, C. D., I. Jime \u00b4nez. 1999. ", p$bib$title[i], ". American Psychologist.")
  expect_false(run1(p)$author_mismatch)

  p <- base
  p$bib$authors[i] <- "Lu, H C; Kim, Wang W"
  p$bib_match$authors[[m]] <- data.frame(given = c("Hsiang", "Wei", "Ji-Yoen"), family = c("Lu", "Wang", "Kim"))
  p$text$text[ti] <- paste0("Lu HC, Wang W, Kim JY. 1999. ", p$bib$title[i], ". American Psychologist.")
  expect_false(run1(p)$author_mismatch)
  # negative control: real typo in Vancouver text stays flagged
  p_typo <- p
  p_typo$bib$authors[i] <- "Lu, H C; Kim, Wong W"
  p_typo$text$text[ti] <- paste0("Lu HC, Wong W, Kim JY. 1999. ", p$bib$title[i], ". American Psychologist.")
  expect_true(run1(p_typo)$author_mismatch)

  # 8. Title search ranks by author/container & rejects "Re: <Title>" and zero-corroboration hits
  p_s <- demopaper()
  k3 <- match(3, p_s$bib$bib_id)
  p_s$bib_match <- p_s$bib_match[p_s$bib_match$bib_id != 3, ]
  t3 <- p_s$bib$title[k3]
  p_s$bib$year[k3] <- 2018L
  p_s$bib$container[k3] <- "Advances in Methods and Practices in Psychological Science"
  p_s$bib$authors[k3] <- "Lakens, Dani\u00ebl"

  # 8a: multi-candidate search prefers matching author/journal over Yearbook summary
  cr_multi <- data.frame(
    ref = rep(t3, 2),
    DOI = c("10.1016/yearbook", "10.1177/2515245918770963"),
    title = rep(t3, 2),
    score = c(95, 85),
    year = c(2018L, 2018L),
    type = c("journal-article", "journal-article"),
    `container-title` = c("Yearbook of Psychology", "Advances in Methods and Practices in Psychological Science"),
    check.names = FALSE
  )
  cr_multi$author <- list(
    data.frame(given = "Other", family = "Dungan"),
    data.frame(given = "Dani\u00ebl", family = "Lakens")
  )
  local_mocked_bindings(
    crossref_query = \(ref, ...) cr_multi,
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), type = character(0), error = logical(0)),
    semanticscholar_query = \(title) data.frame(query = integer(0), doi = character(0), url = character(0),
      title = character(0), year = integer(0), authors = character(0), venue = character(0), type = character(0), error = logical(0)),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA),
    crossref_doi = \(doi, ...) data.frame(DOI = doi)
  )
  r_multi <- module_run(p_s, "ref_accuracy", semantic_scholar = FALSE)$table
  r_m3 <- r_multi[r_multi$bib_id == 3, ]
  expect_false(r_m3$incoherent)
  expect_false(r_m3$doi_suggested) # two records with same title+year -> conservative no-suggest
  # when the Yearbook summary has another year, the author/journal-matching record is suggested
  cr_multi2 <- cr_multi
  cr_multi2$year <- c(2019L, 2018L)
  local_mocked_bindings(crossref_query = \(ref, ...) cr_multi2)
  r_multi2 <- module_run(p_s, "ref_accuracy", semantic_scholar = FALSE)$table
  r_m3b <- r_multi2[r_multi2$bib_id == 3, ]
  expect_false(r_m3b$incoherent)
  expect_true(r_m3b$doi_suggested)
  expect_equal(r_m3b$doi.match, "10.1177/2515245918770963")

  # 8b: "Re: <Title>" commentary and zero-corroboration different work do not false-flag
  cr_re <- data.frame(
    ref = t3,
    DOI = "10.1016/comment",
    title = paste0("Re: ", t3),
    score = 90,
    year = 2021L,
    type = "journal-article",
    `container-title` = "Other Journal",
    check.names = FALSE
  )
  cr_re$author <- list(data.frame(given = "Someone", family = "Else"))
  local_mocked_bindings(
    crossref_query = \(ref, ...) cr_re,
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), type = character(0), error = logical(0)),
    semanticscholar_query = \(title) data.frame(query = integer(0), doi = character(0), url = character(0),
      title = character(0), year = integer(0), authors = character(0), venue = character(0), type = character(0), error = logical(0)),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA),
    crossref_doi = \(doi, ...) data.frame(DOI = doi)
  )
  r_re <- module_run(p_s, "ref_accuracy", semantic_scholar = FALSE)$table
  expect_false(r_re$incoherent[r_re$bib_id == 3])

  # 8c: single-field error (wrong year, right author/journal) in no-DOI search STILL flags!
  cr_yr <- data.frame(
    ref = t3,
    DOI = "10.1177/2515245918770963",
    title = t3,
    score = 90,
    year = 2022L,
    type = "journal-article",
    `container-title` = "Advances in Methods and Practices in Psychological Science",
    check.names = FALSE
  )
  cr_yr$author <- list(data.frame(given = "Dani\u00ebl", family = "Lakens"))
  local_mocked_bindings(
    crossref_query = \(ref, ...) cr_yr,
    datacite_query = \(title, ...) data.frame(query = integer(0), doi = character(0),
      title = character(0), year = integer(0), authors = character(0), type = character(0), error = logical(0)),
    semanticscholar_query = \(title) data.frame(query = integer(0), doi = character(0), url = character(0),
      title = character(0), year = integer(0), authors = character(0), venue = character(0), type = character(0), error = logical(0)),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA),
    crossref_doi = \(doi, ...) data.frame(DOI = doi)
  )
  r_yr <- module_run(p_s, "ref_accuracy", semantic_scholar = FALSE)$table
  expect_true(r_yr$incoherent[r_yr$bib_id == 3])
  expect_true(r_yr$year_mismatch[r_yr$bib_id == 3])
})

test_that("ref_accuracy preserves batch invariance, scientific superscripts, DOI recovery, and HTML escaping", {
  mock_no_search()
  local_mocked_bindings(
    crossref_doi = \(doi, ...) data.frame(DOI = doi),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA)
  )
  base <- demopaper()
  i <- match(1, base$bib$bib_id)
  m <- match(1, base$bib_match$bib_id)
  ti <- match(base$bib$text_id[i], base$text$text_id)
  run1 <- \(p) {
    tbl <- module_run(p, "ref_accuracy", semantic_scholar = FALSE)$table
    as.list(tbl[tbl$bib_id == 1, ])
  }

  # 1. Trailing scientific superscript R^2 vs R^3 at END of title stays flagged
  p <- base
  p$bib$title[i] <- "Measuring variance with R<sup>2</sup>"
  p$bib_match$title[m] <- "Measuring variance with R<sup>3</sup>"
  p$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Measuring variance with R2. American Psychologist."
  expect_true(run1(p)$title_mismatch)
  # trailing footnote after >=4-letter word still stripped
  p_fn <- base
  p_fn$bib$title[i] <- "Measuring variance in psychology"
  p_fn$bib_match$title[m] <- "Measuring variance in psychology<sup>1</sup>"
  expect_false(run1(p_fn)$title_mismatch)

  # 2. Invisible Unicode format/bidi marks (U+200E LRM) stripped in clean()
  p <- base
  p$bib$title[i] <- "Replication and extension of Svenson"
  p$bib_match$title[m] <- "Replication and extension of Svenson (1981)\u200e"
  p$text$text[ti] <- "Eagly, A. H., & Wood, W. (1999). Replication and extension of Svenson (1981). American Psychologist."
  expect_false(run1(p)$title_mismatch)

  # 3. Journal normalization: section letters, ISO-4 "For.", and UK/US stems
  p <- base
  p$bib$container[i] <- "Acta Crystallogr. A"
  p$bib_match$container[m] <- "Acta Crystallographica Section B"
  expect_true(run1(p)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Ann. For."
  p$bib_match$container[m] <- "Annals of Forestry"
  expect_false(run1(p)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Paediatr Perinat Epidemiol"
  p$bib_match$container[m] <- "Paediatric and Perinatal Epidemiology"
  expect_false(run1(p)$container_mismatch)

  p <- base
  p$bib$container[i] <- "Int J Paediatr Dent"
  p$bib_match$container[m] <- "International Journal of Paediatric Dentistry"
  expect_false(run1(p)$container_mismatch)

  # 4. bare_frag requires non-NA text
  p <- base
  p$bib$title[i] <- ""
  p$bib$authors[i] <- ""
  p$text$text[ti] <- NA_character_
  expect_false(identical(run1(p)$tier, "unreadable"))

  # 5. GROBID-mangled doi.orig recovered from single intact printed DOI in text
  #    with negative controls when text itself contains the truncated/hyphenated DOI
  p_trunc <- base
  p_trunc$bib$doi[i] <- "10.1037/0003-066x."
  p_trunc$bib_match <- p_trunc$bib_match[-m, ]
  attr(p_trunc$bib_match, "looked_up") <- TRUE
  p_trunc$text$text[ti] <- paste(p_trunc$text$text[ti], "doi:10.1037/0003-066x.54.6.408")
  local_mocked_bindings(
    doi_ra = \(doi) data.frame(doi = doi, ra = ifelse(doi == "10.1037/0003-066x.54.6.408", "Crossref", NA_character_),
                               exists = doi == "10.1037/0003-066x.54.6.408"),
    doi_csl = \(doi) {
      r <- data.frame(doi = doi, title = base$bib$title[i], year = base$bib$year[i],
                      container = base$bib$container[i], bib_type = "article", stringsAsFactors = FALSE)
      r$authors <- list(base$bib_match$authors[[m]])
      r
    }
  )
  r_trunc <- run1(p_trunc)
  expect_equal(r_trunc$tier, "provided")
  expect_false(r_trunc$incoherent)
  # extra hyphen inserted by GROBID absent in printed text
  p_hyp <- p_trunc
  p_hyp$bib$doi[i] <- "10.1037/0003--066x.54.6.408"
  r_hyp <- run1(p_hyp)
  expect_equal(r_hyp$tier, "provided")
  expect_false(r_hyp$incoherent)
  # Negative control: genuinely truncated DOI printed in text stays unresolved
  p_bad_t <- p_trunc
  p_bad_t$text$text[ti] <- sub("10\\.1037/0003-066x\\.54\\.6\\.408", "10.1037/0003-066x.", p_bad_t$text$text[ti])
  r_bad_t <- run1(p_bad_t)
  expect_equal(r_bad_t$tier, "unresolved")

  # 6. looked_up attribute with 0 bib_match rows still checks unresolved DOIs
  p_zero <- base
  p_zero$bib <- p_zero$bib[i, , drop = FALSE]
  p_zero$bib_match <- p_zero$bib_match[0, , drop = FALSE]
  attr(p_zero$bib_match, "looked_up") <- TRUE
  local_mocked_bindings(doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE))
  r_zero <- module_run(p_zero, "ref_accuracy", semantic_scholar = FALSE)$table
  expect_equal(r_zero$tier, "unresolved")

  # 7. Batch-independent DataCite fallback: adding another paper that triggers
  #    DataCite for a shared title must NOT change P2's result
  p_a <- demopaper(); p_a$paper_id <- "PA"
  p_b <- demopaper(); p_b$paper_id <- "PB"
  k3 <- match(3, p_a$bib$bib_id)
  p_a$bib_match <- p_a$bib_match[p_a$bib_match$bib_id != 3, ]
  p_b$bib_match <- p_b$bib_match[p_b$bib_match$bib_id != 3, ]
  sh_t <- p_a$bib$title[k3]
  # PA cites a journal -> Crossref preprint triggers DataCite fallback for PA
  p_a$bib$container[k3] <- "Psychological Science"
  p_a$bib$authors[k3] <- "Lakens, Dani\u00ebl"
  # PB cites a preprint (not a journal!) -> Crossref preprint already suffices for PB,
  # so PB alone does NOT query DataCite; PB has wrong author -> flagged alone AND in batch
  p_b$bib$container[k3] <- "PsyArXiv"
  p_b$bib$authors[k3] <- "Wrongauthor, Z"
  cr_pre <- data.frame(
    ref = sh_t, DOI = "10.31234/osf.io/abc", title = sh_t, score = 90,
    year = 2018L, type = "posted-content", `container-title` = NA_character_,
    check.names = FALSE
  )
  cr_pre$author <- list(data.frame(given = "Dani\u00ebl", family = "Lakens"))
  dc_alt <- data.frame(
    query = 1L, doi = "10.5281/zenodo.999", title = sh_t, year = 2018L,
    authors = "Wrongauthor, Z", type = "Preprint", error = FALSE, stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    crossref_query = \(ref, ...) cr_pre,
    datacite_query = \(title, ...) dc_alt,
    semanticscholar_query = \(title) data.frame(query = integer(0), doi = character(0), url = character(0),
      title = character(0), year = integer(0), authors = character(0), venue = character(0), type = character(0), error = logical(0)),
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = NA),
    crossref_doi = \(doi, ...) data.frame(DOI = doi)
  )
  alone_b <- module_run(p_b, "ref_accuracy", semantic_scholar = FALSE)$table
  batch_ab <- module_run(paperlist(p_a, p_b), "ref_accuracy", semantic_scholar = FALSE)$table
  rb_alone <- alone_b[alone_b$bib_id == 3, ]
  rb_batch <- batch_ab[batch_ab$paper_id == "PB" & batch_ab$bib_id == 3, ]
  expect_identical(rb_batch$incoherent, rb_alone$incoherent)
  expect_identical(rb_batch$doi.match, rb_alone$doi.match)

  # 8. Report HTML/attribute escaping (read_problem, doi_href #, ?, [], and preprint context note)
  p_esc <- base
  p_esc$bib$doi[i] <- NA_character_
  p_esc$bib_match <- p_esc$bib_match[-m, ]
  p_esc$text$text[ti] <- "Smith (2020). Title. doi:10.1000/<img/onerror=1>"
  rep_esc <- paste(unlist(module_run(p_esc, "ref_accuracy", semantic_scholar = FALSE)$report), collapse = "\n")
  expect_false(grepl("<img", rep_esc, fixed = TRUE))
  expect_match(rep_esc, "&lt;img", fixed = TRUE)

  p_pre <- base
  p_pre$bib$doi[i] <- "10.31234/osf.io/5yexm"
  p_pre$bib_match$doi[m] <- "10.31234/osf.io/5yexm"
  p_pre$bib_match$bib_type[m] <- "preprint"
  p_pre$bib$authors[i] <- "Eagly, A H"
  mo_pre <- module_run(p_pre, "ref_accuracy", semantic_scholar = FALSE)
  expect_true(mo_pre$table$incoherent[mo_pre$table$bib_id == 1])
  expect_match(paste(unlist(mo_pre$report), collapse = "\n"),
               "preprint; its registry record may have been revised", fixed = TRUE)
})

test_that("ref_accuracy handles journal abbreviations, encoding corruption, umlauts, and DOI recovery", {
  mock_no_search()
  base <- demopaper()
  i <- match(1, base$bib$bib_id)
  m <- match(1, base$bib_match$bib_id)
  ti <- match(base$bib$text_id[i], base$text$text_id)
  run1 <- \(p) {
    tbl <- module_run(p, "ref_accuracy", semantic_scholar = FALSE)$table
    tbl[tbl$bib_id == 1, ]
  }

  # 1. Journal matching: Wikidata 1-word alias with city/subtitle, bilingual '=',
  #    Polish/European stopwords, numbered journals, ISO-4 contractions, compound
  #    hyphens, 2-letter uppercase initials, multi-word parenthetical qualifiers,
  #    leading acronym with hyphen/initials, De Gruyter publisher codes, tight
  #    abbreviation-aware edit distance
  j_ok <- list(
    c("Europace", "EP Europace"),
    c("Eklem hastaliklari ve cerrahisi = Jt Dis Relat Surg", "Joint Diseases and Related Surgery"),
    c("Neurol. Neurochir. Pol", "Neurologia i Neurochirurgia Polska"),
    c("Periodontol.2000", "Periodontology 2000"),
    c("J. Bone Jt. Surg", "The Journal of Bone & Joint Surgery"),
    c("Eur. J. Cardiothorac. Surg", "European Journal of Cardio-Thoracic Surgery"),
    c("Neuro-Image", "NeuroImage"),
    c("Ann NY Acad Sci", "Annals of the New York Academy of Sciences"),
    c("Obesity (Silver Spring)", "Obesity"),
    c("BMJ (Clinical Research", "BMJ"),
    c("JARO-J Assoc Res Otolaryngol", "Journal of the Association for Research in Otolaryngology"),
    c("PNAS Proceedings of the National Academy of Sciences of the United States of America",
      "Proceedings of the National Academy of Sciences"),
    c("J. Neuroscince", "Journal of Neuroscience"),
    c("I-Perception", "i-Perception"),
    c("Revista Brasileira de Cineantropometria E Desempenho Humano",
      "Revista Brasileira de Cineantropometria e Desempenho Humano")
  )
  for (pair in j_ok) {
    p <- base
    p$bib$container[i] <- pair[1]
    p$bib_match$container[m] <- pair[2]
    expect_false(run1(p)$container_mismatch, info = paste(pair, collapse = " vs "))
  }

  # De Gruyter lowercase journal code in Crossref when DOI carries the code
  p_dg <- base
  p_dg$bib$doi[i] <- "10.1515/cogl.2005.16.3.437"
  p_dg$bib_match$doi[m] <- "10.1515/cogl.2005.16.3.437"
  p_dg$bib$container[i] <- "Cognitive Linguistics"
  p_dg$bib_match$container[m] <- "cogl"
  expect_false(run1(p_dg)$container_mismatch)

  # Negative controls: genuinely wrong journals MUST stay flagged
  j_bad <- list(
    c("Clin Epidemiol", "Journal of Clinical Epidemiology"),
    c("The American Journal of Sports Medicine", "Medicine & Science in Sports & Exercise"),
    c("Journal of Sport", "Journal of Support"),
    c("Psychol Med", "Psychological Methods"),
    c("Stat Med", "Statistical Methods in Medical Research"),
    c("Psychol Res", "Psychological Reports"),
    c("Annals of Hematology", "Annals of Hepatology"),
    c("J Hepatol", "Journal of Hematology"),
    c("Journal of Ethology", "Journal of Ethnology"),
    c("Physical Review E", "Physical Review A"),
    c("Acta Crystallogr. E", "Acta Crystallographica Section A"),
    c("Nature", "cogl")
  )
  for (pair in j_bad) {
    p <- base
    p$bib$container[i] <- pair[1]
    p$bib_match$container[m] <- pair[2]
    if (pair[2] == "cogl") {
      p$bib$doi[i] <- "10.1515/cogl.2005.16.3.437"
      p$bib_match$doi[m] <- "10.1515/cogl.2005.16.3.437"
    }
    expect_true(run1(p)$container_mismatch, info = paste(pair, collapse = " vs "))
  }

  # 2. Registry UTF-8 encoding corruption (\uFFFD and ??) in record surnames & titles
  p_enc <- base
  p_enc$bib$authors[i] <- "Cola\u00e7o, P; Ingulf Medbo, J"
  p_enc$bib_match$authors[[m]] <- data.frame(
    given = c("P.", "J."), family = c("Cola\uFFFDo", "INGULF MEDB??"),
    stringsAsFactors = FALSE
  )
  expect_false(run1(p_enc)$author_mismatch)

  # Negative controls: corrupted record surname must NOT match unrelated or short names
  p_enc_bad <- p_enc
  p_enc_bad$bib$authors[i] <- "Smith, P; Ingulf Medbo, J"
  expect_true(run1(p_enc_bad)$author_mismatch)
  p_enc_short <- base
  p_enc_short$bib$authors[i] <- "Kimura, J"
  p_enc_short$bib_match$authors[[m]] <- data.frame(given = "J.", family = "Kim??", stringsAsFactors = FALSE)
  expect_true(run1(p_enc_short)$author_mismatch)

  p_tenc <- base
  p_tenc$bib$title[i] <- "Effects of \u00e7-peptide on metabolism in adults"
  p_tenc$bib_match$title[m] <- "Effects of \uFFFD-peptide on metabolism in adults"
  expect_false(run1(p_tenc)$title_mismatch)

  # 3. ASCII umlaut / Nordic transliteration in surnames (both directions)
  p_um <- base
  p_um$bib$authors[i] <- "Mueller, L; Luethen, F; Soerensen, K"
  p_um$bib_match$authors[[m]] <- data.frame(
    given = c("L.", "F.", "K."), family = c("M\u00fcller", "L\u00fcthen", "S\u00f8rensen"),
    stringsAsFactors = FALSE
  )
  expect_false(run1(p_um)$author_mismatch)

  p_um_rev <- base
  p_um_rev$bib$authors[i] <- "M\u00fcller, L; L\u00fcthen, F"
  p_um_rev$bib_match$authors[[m]] <- data.frame(
    given = c("L.", "F."), family = c("Mueller", "Luethen"),
    stringsAsFactors = FALSE
  )
  expect_false(run1(p_um_rev)$author_mismatch)

  # Negative control: non-umlaut names ("Bauer" vs "Baur") must stay flagged
  p_um_bad <- base
  p_um_bad$bib$authors[i] <- "Baur, L"
  p_um_bad$bib_match$authors[[m]] <- data.frame(given = "L.", family = "Bauer", stringsAsFactors = FALSE)
  expect_true(run1(p_um_bad)$author_mismatch)

  # 4. Truncated journal-level DOI recovery even when truncated stub had a record
  p_tr <- base
  p_tr$bib$doi[i] <- "10.5007/1980-0037"
  p_tr$bib_match$doi[m] <- "10.5007/1980-0037"
  p_tr$bib_match$title[m] <- "Journal Name Stub"
  p_tr$bib_match$bib_type[m] <- "journal"
  p_tr$text$text[ti] <- paste(base$bib$authors[i], base$bib$title[i],
                              "http://dx.doi.org/10.5007/1980-0037.2014v16n2p233")
  local_mocked_bindings(
    doi_ra = \(doi) data.frame(doi = doi, ra = "Crossref", exists = TRUE),
    doi_csl = \(doi) {
      d <- data.frame(
        doi = "10.5007/1980-0037.2014v16n2p233",
        title = base$bib$title[i], year = base$bib$year[i],
        container = base$bib$container[i], bib_type = "article",
        stringsAsFactors = FALSE
      )
      d$authors <- list(base$bib_match$authors[[m]])
      d
    }
  )
  r_tr <- run1(p_tr)
  expect_equal(r_tr$doi.orig, "10.5007/1980-0037.2014v16n2p233")
  expect_false(r_tr$incoherent)

  # Negative control: glued text after a valid DOI (".../a0012345.Epub") must NOT
  # wipe an already-matching article record
  p_glued <- base
  p_glued$bib$doi[i] <- "10.1037/a0012345"
  p_glued$bib_match$doi[m] <- "10.1037/a0012345"
  p_glued$text$text[ti] <- paste(base$bib$authors[i], base$bib$title[i],
                                 "https://doi.org/10.1037/a0012345.Epub")
  r_glued <- run1(p_glued)
  expect_equal(r_glued$doi.orig, "10.1037/a0012345")
  expect_false(r_glued$incoherent)

  # Cold Spring Harbor journal DOI (10.1101/gr.*) must NOT get the preprint note
  p_csh <- base
  p_csh$bib$doi[i] <- "10.1101/gr.123456.111"
  p_csh$bib_match$doi[m] <- "10.1101/gr.123456.111"
  p_csh$bib$authors[i] <- "Eagly, A H"
  mo_csh <- module_run(p_csh, "ref_accuracy", semantic_scholar = FALSE)
  expect_true(mo_csh$table$incoherent[mo_csh$table$bib_id == 1])
  expect_false(grepl("this is a preprint", paste(unlist(mo_csh$report), collapse = "\n"), fixed = TRUE))

  # 5. Unresolved DOI with PDF line-break hyphen recovered via title search
  p_hy <- base
  p_hy$bib$doi[i] <- "10.1152/japplphys-iol.00630.2016"
  p_hy$bib_match <- p_hy$bib_match[-m, ]
  attr(p_hy$bib_match, "looked_up") <- TRUE
  p_hy$text$text[ti] <- paste(base$bib$authors[i], base$bib$title[i], "10.1152/japplphys-iol.00630.2016")
  cr_hit <- data.frame(
    ref = base$bib$title[i], DOI = "10.1152/japplphysiol.00630.2016",
    title = base$bib$title[i], score = 95, year = base$bib$year[i],
    type = "journal-article", `container-title` = base$bib$container[i],
    check.names = FALSE
  )
  cr_hit$author <- list(base$bib_match$authors[[m]])
  local_mocked_bindings(
    doi_ra = \(doi) data.frame(doi = doi, ra = NA_character_, exists = FALSE),
    crossref_query = \(ref, ...) cr_hit,
    crossref_doi = \(doi, ...) data.frame(DOI = doi)
  )
  r_hy <- run1(p_hy)
  expect_equal(r_hy$tier, "provided")
  expect_false(r_hy$incoherent)

  # Negative control: line-break hyphen recovery STILL flags a genuine wrong year
  p_hy_bad <- p_hy
  p_hy_bad$bib$year[i] <- base$bib$year[i] - 5L
  r_hy_bad <- run1(p_hy_bad)
  expect_true(r_hy_bad$incoherent)
  expect_true(r_hy_bad$year_mismatch)
})
