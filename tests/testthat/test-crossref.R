test_that("exists", {
  expect_true(is.function(papercheck::crossref))
  expect_no_error(helplist <- help(crossref, papercheck))

  expect_true(is.function(papercheck::openalex))
  expect_no_error(helplist <- help(openalex, papercheck))
})

test_that("crossref", {
  skip()
  skip_if_offline("api.labs.crossref.org")

  doi <- "10.1177/fake"
  expect_message(cr <- crossref(doi))
  expect_equal(cr, list())

  # single doi
  doi <- "10.1177/0956797614520714"
  cr <- crossref(doi)
  expect_equal(cr$`cr-labs-updates`[[1]]$`update-nature`, "Retraction")

  # list of DOIs
  dois <- info_table(psychsci, "doi")
  dois$doi <- gsub("pss\\.", "", dois$doi) |> gsub("sagepub\\.", "", x = _)
  doi <- dois$doi[1:2]
  cr2 <- crossref(doi)
})

# httptest::start_capturing()
httptest::with_mock_api({
test_that("openalex", {
  email("debruine@gmail.com")

  doi <- "10.1177/fake"
  expect_warning(oa <- openalex(doi))
  expect_equal(oa, list(error = doi))

  # short DOI
  doi <- "10.1177/0956797614520714"
  oa <- openalex(doi)
  expect_equal(oa$is_retracted, TRUE)
  expect_equal(oa$abstract, "We propose that dishonest and creative behavior have something in common: They both involve breaking rules. Because of this shared feature, creativity may lead to dishonesty (as shown in prior work), and dishonesty may lead to creativity (the hypothesis we tested in this research). In five experiments, participants had the opportunity to behave dishonestly by overreporting their performance on various tasks. They then completed one or more tasks designed to measure creativity. Those who cheated were subsequently more creative than noncheaters, even when we accounted for individual differences in their creative ability (Experiment 1). Using random assignment, we confirmed that acting dishonestly leads to greater creativity in subsequent tasks (Experiments 2 and 3). The link between dishonesty and creativity is explained by a heightened feeling of being unconstrained by rules, as indicated by both mediation (Experiment 4) and moderation (Experiment 5).")

  # long DOI
  doi <- c("https://doi.org/10.1177/0956797613520608")
  oa <- openalex(doi)
  expect_equal(oa$id, "https://openalex.org/W2134722098")

  # multiple DOIs
  dois <- c("10.1177/0956797613520608", "10.1177/0956797614522816")
  oa <- openalex(dois)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")
  expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746")
  # DOI from paper
  paper <- psychsci[[1]]
  oa <- openalex(paper)
  expect_equal(oa$id, "https://openalex.org/W2134722098")

  # DOIs from paperlist
  paper <- psychsci[1:2]
  oa <- openalex(paper)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")
  expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746")
  # one malformatted DOI
  paper <- psychsci[10:11]
  paper[[2]]$info$doi <- paste0(paper[[2]]$info$doi, "x")
  expect_warning(oa <- openalex(paper))
  expect_equal(oa[[1]]$id, "https://openalex.org/W1824074316")
  expect_equal(oa[[2]], list(error = paper[[2]]$info$doi))

  # select
  doi <- "10.1177/0956797614520714"
  oa <- openalex(doi, select = "is_retracted")
  expect_equal(oa$is_retracted, TRUE)
})
}) # end mock api
# httptest::stop_capturing()

