

test_that("datacite_doi", {
  expect_true(is.function(metacheck::datacite_doi))
  expect_no_error(helplist <- help(datacite_doi, metacheck))

  expect_error(datacite_doi(badarg))

  # NULL VALUE
  doi <- NULL
  exp <- data.frame(doi = character(0))
  info <- datacite_doi(doi)
  expect_equal(info, exp)

  # empty vector
  doi <- c()
  exp <- data.frame(doi = character(0))
  info <- datacite_doi(doi)
  expect_equal(info, exp)

  skip_api("api.datacite.org")

  # one item
  doi <- "10.5281/zenodo.2669586"
  info <- datacite_doi(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$title, "faux: Simulation for Factorial Designs")

  # one NA
  doi <- NA
  info <- datacite_doi(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$title, NA_character_)

  # multiple items
  doi <- c("10.5281/zenodo.2669586", "10.5281/zenodo.3564348")
  info <- datacite_doi(doi)
  expect_equal(info$title, c("faux: Simulation for Factorial Designs",
                             "Data Skills for Reproducible Science"))
})








test_that("openalex_doi", {
  expect_true(is.function(metacheck::openalex_doi))
  expect_no_error(helplist <- help(openalex_doi, metacheck))

  doi <- "10.1177/fake"
  suppressWarnings(oa <- openalex_doi(doi))
  expect_equal(oa$DOI, doi)
  expect_equal(oa$error, "not found")

  doi <- "bad.form"
  oa <- openalex_doi(doi)
  expect_equal(oa$DOI, doi)
  expect_equal(oa$error, "malformed")

  # short DOI
  doi <- "10.1177/0956797614520714"
  oa <- openalex_doi(doi)
  expect_equal(oa$is_retracted, TRUE)
  expect_equal(oa$abstract, "We propose that dishonest and creative behavior have something in common: They both involve breaking rules. Because of this shared feature, creativity may lead to dishonesty (as shown in prior work), and dishonesty may lead to creativity (the hypothesis we tested in this research). In five experiments, participants had the opportunity to behave dishonestly by overreporting their performance on various tasks. They then completed one or more tasks designed to measure creativity. Those who cheated were subsequently more creative than noncheaters, even when we accounted for individual differences in their creative ability (Experiment 1). Using random assignment, we confirmed that acting dishonestly leads to greater creativity in subsequent tasks (Experiments 2 and 3). The link between dishonesty and creativity is explained by a heightened feeling of being unconstrained by rules, as indicated by both mediation (Experiment 4) and moderation (Experiment 5).")

  # long DOI
  doi <- c("https://doi.org/10.1177/0956797613520608")
  oa <- openalex_doi(doi)
  expect_equal(oa$id, "https://openalex.org/W2134722098")

  # multiple DOIs
  dois <- c("10.1177/0956797613520608", "10.1177/0956797614522816")
  oa <- openalex_doi(dois)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")
  expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746")

  # DOI from paper
  paper <- psychsci[[1]]
  oa <- openalex_doi(paper)
  expect_equal(oa$id, "https://openalex.org/W2134722098")

  # DOIs from paperlist
  paper <- psychsci[1:2]
  oa <- openalex_doi(paper)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")
  expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746")
  # one malformatted DOI
  paper <- psychsci[10:11]
  paper[[2]]$info$doi <- paste0(paper[[2]]$info$doi, "x")
  # expect_warning(oa <- openalex(paper))  DOES NOT FAIL ON LAKENS
  # expect_equal(oa[[1]]$id, "https://openalex.org/W1824074316")
  # expect_equal(oa[[2]], list(error = paper[[2]]$info$doi))

  # select
  doi <- "10.1177/0956797614520714"
  oa <- openalex_doi(doi, select = "is_retracted")
  expect_equal(oa$is_retracted, TRUE)
})

test_that("openalex_query", {
  expect_true(is.function(metacheck::openalex_query))
  expect_no_error(helplist <- help(openalex_query, metacheck))

  expect_error(openalex_query(bad_arg))

  doi <- "https://doi.org/10.1525/collabra.33267"
  title <- "Sample Size Justification"
  source <- "Collabra Psychology"
  authors <- "Lakens, D"
  b <- openalex_query(title, source, authors)
  expect_equal(nrow(b), 1)
  expect_equal(b$display_name, title)
  expect_equal(b$doi, doi)
})

test_that("bibtex_add_dois", {
  expect_true(is.function(metacheck::bibtex_add_dois))
  expect_no_error(helplist <- help(bibtex_add_dois, metacheck))

  bibfile <- test_path("fixtures", "missing_dois.bib")
  save_to <- withr::local_tempfile(fileext = ".bib")

  strict <- bibtex_add_dois(bibfile, save_to)
  expect_true(file.exists(save_to))
  expect_equal(nrow(strict), 17)
  expect_equal(is.na(strict$DOI) |> sum(), 6)
  strict_msgs <- attr(strict, "msgs")

  # check for a missing doi
  doi <- "10.1371/journal.pone.0281086"
  expect_equal(strict$DOI[[1]], doi)
})

test_that("bibtex_add_dois nostrict", {
  bibfile <- test_path("fixtures", "missing_dois.bib")
  save_to <- withr::local_tempfile(fileext = ".bib")
  nostrict <- bibtex_add_dois(bibfile, save_to, strict = FALSE)
  expect_true(file.exists(save_to))
  expect_equal(nrow(nostrict), 17)
  expect_equal(is.na(nostrict$DOI) |> sum(), 3)

  # check for a missing doi
  doi <- "10.1371/journal.pone.0281086"
  expect_equal(nostrict$DOI[[1]], doi)
})

test_that("bib_add_dois", {
  expect_true(is.function(metacheck::bib_add_dois))
  expect_no_error(helplist <- help(bib_add_dois, metacheck))

  bib <- psychsci[[2]]$bib[1:10, ]
  bib_strict <- bib_add_dois(bib)
  bib_nostrict <- bib_add_dois(bib, strict = FALSE)

  expect_equal(nrow(bib_strict), 10)
  expect_equal(nrow(bib_nostrict), 10)
  strict_na <- is.na(bib_strict$doi) |> sum()
  nostrict_na <- is.na(bib_nostrict$doi) |> sum()
  expect_true(strict_na >= nostrict_na)

  doi1 <- "10.1037/0033-295x.94.2.115"
  expect_equal(bib_strict$doi[[1]], NA_character_)
  expect_equal(bib_nostrict$doi[[1]], doi1)

  doi8 <- "10.1016/j.visres.2007.09.013"
  expect_equal(bib_strict$doi[[8]], doi8)
  expect_equal(bib_nostrict$doi[[8]], doi8)
})


