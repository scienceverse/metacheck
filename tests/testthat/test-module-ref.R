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
})


test_that("ref_accuracy", {
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

  # no bib_match
  paper <- demopaper()
  paper$bib_match <- NULL
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "error")
  expect_null(mod_output$table)
  expect_match(mod_output$summary_text, "add_bib_match")

  # relevant references
  paper <- demopaper()
  mod_output <- module_run(paper, module)
  expect_equal(nrow(mod_output$table), nrow(paper$bib))

  # references that supplied a DOI are checked; the demo paper includes two
  # references with a DOI that does not resolve, which are flagged as incoherent
  st <- mod_output$summary_table
  expect_equal(st$refs_checked, 4)
  expect_equal(st$no_doi, 0)
  expect_equal(st$incoherent, 2)
  expect_equal(mod_output$traffic_light, "yellow")

  # those two are the "unresolved" tier: a cited DOI CrossRef could not find
  unresolved <- mod_output$table[mod_output$table$tier == "unresolved", ]
  expect_equal(nrow(unresolved), 2)
  expect_true(all(unresolved$incoherent))

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
  strict <- module_run(auth, module, min_mismatches = 2)   # conservative
  # the two unresolved references are always incoherent; the lone author
  # mismatch adds a third only at the sensitive setting
  expect_equal(loose$summary_table$incoherent, 3)
  expect_equal(strict$summary_table$incoherent, 2)
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
