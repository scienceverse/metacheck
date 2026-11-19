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
  expect_equal(mod_output$traffic_light, "red")
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
  expect_equal(sum(mod_output$table$total_comments), 3)

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
  expect_equal(sum(mod_output$table$total_comments), 3)

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
