test_that("exists", {
  expect_true(is.function(metacheck::crossref_doi))
  expect_no_error(helplist <- help(crossref_doi, metacheck))

  expect_true(is.function(metacheck::crossref_query))
  expect_no_error(helplist <- help(crossref_query, metacheck))

  expect_true(is.function(metacheck::openalex))
  expect_no_error(helplist <- help(openalex, metacheck))
})

# httptest::start_capturing()
httptest::with_mock_api({

test_that("crossref_doi", {
  #skip_if_offline("api.labs.crossref.org")

  doi <- "10.1177/fake"
  cr <- crossref_doi(doi)
  expect_equal(cr$DOI, doi)
  expect_equal(cr$error, "Not Found")
  expect_null(cr$author)

  # single doi
  doi <- "10.1177/0956797614520714"
  cr <- crossref_doi(doi)
  exp <- "Retracted: Evil Genius? How Dishonesty Can Lead to Greater Creativity"
  expect_equal(cr$title, exp)

  # list of DOIs
  dois <- info_table(psychsci, "doi")
  dois$doi <- gsub("pss\\.", "", dois$doi) |> gsub("sagepub\\.", "", x = _)
  doi <- dois$doi[1:2]
  cr2 <- crossref_doi(doi)
  expect_equal(nrow(cr2), 2)
  expect_equal(cr2$DOI, doi)
})


test_that("openalex", {
  doi <- "10.1177/fake"
  # expect_warning(oa <- openalex(doi))  DOES NOT FAIL ON LAKENS
  # expect_equal(oa, list(error = doi))

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
  # expect_warning(oa <- openalex(paper))  DOES NOT FAIL ON LAKENS
  # expect_equal(oa[[1]]$id, "https://openalex.org/W1824074316")
  # expect_equal(oa[[2]], list(error = paper[[2]]$info$doi))

  # select
  doi <- "10.1177/0956797614520714"
  oa <- openalex(doi, select = "is_retracted")
  expect_equal(oa$is_retracted, TRUE)
})

test_that("crossref_query", {
  #skip_if_offline("api.labs.crossref.org")

  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."

  obs <- crossref_query(ref, min_score = 50)
  exp <- "10.1080/2833373x.2024.2376046"
  expect_equal(obs$DOI, exp)
  exp <- c("ref", "DOI", "score", "type", "title", "author",
           "container-title", "volume", "issue", "URL", "year")
  expect_equal(names(obs), exp)

  ref <- "DeBruine, L. (2027) I haven't written this paper. Journal of Journals."
  obs <- crossref_query(ref, min_score = 50)
  expect_equal(obs$DOI, NA_character_)
  expect_equal(obs$ref, ref)

  # from bibentry ref
  ref <- psychsci[[1]]$bib$ref[[1]]
  obs <- crossref_query(ref)
  exp <- "10.1093/brain/110.3.747"
  expect_equal(obs$DOI, exp)

  # vectorised
  ref <- c("Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1).",
           "DeBruine, L. (2027) I haven't written this paper. Journal of Journals.")
  obs <- crossref_query(ref)
  exp <- c("10.1080/2833373x.2024.2376046", NA)
  expect_equal(obs$DOI, exp)

  # TODO: lots of terrible bib
  paper <- psychsci$`0956797620967261`
})

}) # end mock api
# httptest::stop_capturing()
