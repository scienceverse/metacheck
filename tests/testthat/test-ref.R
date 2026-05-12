#httptest2::start_capturing()
httptest2::use_mock_api()

# skip online checking in aspredicted_* functions
testthat::local_mocked_bindings(
  online = \(...) TRUE
)

test_that("add_bib_match", {
  expect_true(is.function(metacheck::add_bib_match))
  expect_no_error(helplist <- help(add_bib_match, metacheck))

  expect_error(add_bib_match(bad_arg))

  # no refs
  paper <- test_paper("No refs")
  paper_bm <- add_bib_match(paper)
  expect_equal(nrow(paper_bm$bib), 0)
  expect_equal(paper_bm$bib_match, NULL)

  # skip_api("api.labs.crossref.org")

  # only unmatching refs
  paper <- test_paper("No matching refs")
  paper$bib <- data.frame(
    bib_id = 1,
    doi = NA,
    title = "Not a real paper",
    container = "Journal of Journals",
    authors = "Not A. Realname"
  )
  paper_bm <- add_bib_match(paper)
  expect_equal(paper_bm$bib$bib_id, 1)
  expect_equal(nrow(paper_bm$bib_match), 0)

  # invalid DOI
  paper <- test_paper("Invalid DOI")
  paper$bib <- data.frame(
    bib_id = 1,
    doi = "not.a.doi",
    title = "Facial resemblance enhances trust",
    container = "Proceedings of the Royal Society of London B",
    authors = "Lisa DeBruine"
  )
  paper_bm <- add_bib_match(paper)
  expect_equal(paper_bm$bib$bib_id, 1)
  expect_equal(nrow(paper_bm$bib_match), 1)
  expect_equal(paper_bm$bib_match$doi, "10.1098/rspb.2002.2034")
  expect_gte(paper_bm$bib_match$score, 50)

  # valid DOI
  paper <- test_paper("Valid DOI")
  paper$bib <- data.frame(
    bib_id = 1,
    doi = "10.1098/rspb.2002.2034",
    title = "Facial resemblance enhances trust",
    container = "Proceedings of the Royal Society of London B",
    authors = "Lisa DeBruine"
  )
  paper_bm <- add_bib_match(paper)
  expect_equal(paper_bm$bib$bib_id, 1)
  expect_equal(nrow(paper_bm$bib_match), 1)
  expect_equal(paper_bm$bib_match$doi, "10.1098/rspb.2002.2034")
  expect_equal(paper_bm$bib_match$score, NA_real_)

  # make paper with 2 refs
  paper <- test_paper("LDB test papers")
  paper$bib <- data.frame(
    bib_id = 1:2,
    doi = NA,
    title = c("Facial resemblance enhances trust",
              "Trustworthy but not Lustworthy"),
    container = c("Proceedings of the Royal Society of London B"),
    authors = I(list("Lisa DeBruine", "Lisa DeBruine"))
  )
  paper_bm <- add_bib_match(paper, 0)

  expect_equal(paper_bm$bib_match$bib_id, 1:2)
  expect_equal(paper_bm$bib_match$doi, c("10.1098/rspb.2002.2034",
                                         "10.1098/rspb.2004.3003"))
  expect_equal(paper_bm$bib_match$year, c(2002, 2005))

  # set threshold between two papers
  min_score <- mean(paper_bm$bib_match$score)
  paper_bm2 <- add_bib_match(paper, min_score)
  expect_equal(paper_bm2$bib_match$doi, "10.1098/rspb.2002.2034")

  # paperlist
  paper1 <- test_paper()
  paper1$bib <- data.frame(
    bib_id = 1:2,
    doi = NA,
    title = c("Facial resemblance enhances trust",
              "Trustworthy but not Lustworthy"),
    container = c("Proceedings of the Royal Society of London B"),
    authors = c("Lisa DeBruine", "Lisa DeBruine")
  )
  paper2 <- test_paper()
  paper2$bib <- data.frame(
    bib_id = 1:2,
    doi = NA,
    title = c("Trustworthy but not Lustworthy",
              "Equivalence Tests: A Practical Primer for t Tests, Correlations, and Meta-Analyses"),
    container = c("Proceedings of the Royal Society of London B",
                  "Social Psychological and Personality Science"),
    authors = c("Lisa DeBruine", "Daniel Lakens")
  )
  paper <- paperlist(list(paper1, paper2))
  paper_bm <- add_bib_match(paper, 0)

  expect_equal(paper_bm[[1]]$bib_match$doi, c("10.1098/rspb.2002.2034",
                                              "10.1098/rspb.2004.3003"))
  expect_equal(paper_bm[[2]]$bib_match$doi, c("10.1098/rspb.2004.3003",
                                              "10.1177/1948550617697177"))
})

test_that("longer bib", {
  skip_if_quick()
  # skip_api("api.labs.crossref.org")

  xml_file <- test_path("fixtures", "formats", "published.pdf.tei.xml")
  paper <- grobid_to_bibr(xml_file, save_path = NULL, FALSE)
  paper_bm <- add_bib_match(paper, min_score = 50)

  bm <- dplyr::inner_join(paper_bm$bib, paper_bm$bib_match,
                          by = "bib_id", suffix = c(".bib", ".bm"))

  # DOIs same if present (some osf.io don't match)
  bib_dois <- bm$doi.bib[!is.na(bm$doi.bib)]
  bm_dois <- bm$doi.bm[!is.na(bm$doi.bib)]
  match_pcnt <- mean(bib_dois %in% bm_dois)
  expect_true(match_pcnt > .85)
})





test_that("crossref_query", {
  expect_true(is.function(metacheck::crossref_query))
  expect_no_error(helplist <- help(crossref_query, metacheck))

  # skip_api("api.labs.crossref.org")

  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."

  obs <- crossref_query(ref, min_score = 50)
  exp <- "10.1080/2833373x.2024.2376046"
  expect_equal(obs$DOI, exp)
  exp <- c("ref", "DOI", "score", "type", "title", "author", "publisher",
           "container-title", "volume", "issue", "URL", "year")
  expect_setequal(names(obs), exp)

  ref <- "DeBruine, L. (2027) I haven't written this paper. Journal of Journals."
  obs <- crossref_query(ref, min_score = 50)
  expect_equal(obs$DOI, NA_character_)
  expect_equal(obs$ref, ref)

  # select
  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."
  obs <- crossref_query(ref, select = c("DOI"))
  expect_setequal(names(obs), c("ref", "DOI"))

  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."
  obs <- crossref_query(ref, select = c("score", "title"))
  expect_setequal(names(obs), c("ref", "score", "title"))

  ref <- data.frame(
    authors = "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M.",
    title = "The benefits of preregistration and Registered Reports",
    container = "Evidence-Based Toxicology",
    year = 2024
  )
  obs <- crossref_query(ref, select = c("score", "title"))
  expect_equal(obs$title, "The benefits of preregistration and Registered Reports")

  # from bibentry
  # ref <- bibentry(
  #   "article",
  #   journal = psychsci[[1]]$bib$container[[1]],
  #   title = psychsci[[1]]$bib$title[[1]],
  #   author = psychsci[[1]]$bib$authors[[1]],
  #   year = psychsci[[1]]$bib$year[[1]]
  # )
  # obs <- crossref_query(ref)
  # exp <- "10.1093/brain/110.3.747"
  # expect_equal(obs$DOI, exp)

  # vectorised
  ref <- c("Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1).",
           "DeBruine, L. (2027) I haven't written this paper. Journal of Journals.")
  obs <- crossref_query(ref)
  exp <- c("10.1080/2833373x.2024.2376046", NA)
  expect_equal(obs$DOI, exp)

  # problem with encoded ( and )
  ref <- "Levi DM, Klein SA, Aitsebaomo AP, Mon-Williams M, Tresilian JR, Strang NC, Kochhar P, Wann JP (1985. 1998).\n“Improving vision: Neural compensation for optical defocus.” _Proceedings of the Royal Society B:\nBiological Sciences_, *25*, 71-77. doi:10.1016/0042-6989(85)90207\n<https://doi.org/10.1016/0042-6989(85)90207>."
  obs <- crossref_query(ref)
  exp <- "10.1098/rspb.1998.0266"
  expect_equal(obs$DOI, exp)
})

test_that("crossref_query batch", {
  # skip_api("api.labs.crossref.org")

  # paper object as ref
  ref <- demopaper()
  obs <- crossref_query(ref)
  exp <- c("10.32614/cran.package.faux",
           "10.1037/0003-066x.54.6.408",
           "10.1177/0956797614520714",
           "10.1177/2515245918770963",
           NA)
  expect_setequal(obs$DOI, exp)

  skip_if_quick()

  # longer paper list
  ref <- psychsci[[1]]$bib[1:20, ]
  obs <- crossref_query(ref)

  obs2 <- lapply(1:20, # seq_along(ref$bib$bib_id),
                 \(i) crossref_query(ref[i, , drop = FALSE])) |>
    dplyr::bind_rows()

  # scores aren't deterministic, but should be close
  score_diff <- abs(obs$score - obs2$score) |> na.omit()
  expect_true(all(score_diff < 2))

  obs$score <- NULL
  obs2$score <- NULL

  expect_equal(obs, obs2)
})

test_that("crossref_doi", {
  expect_true(is.function(metacheck::crossref_doi))
  expect_no_error(helplist <- help(crossref_doi, metacheck))

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
  dois <- paper_table(psychsci, "info")$doi
  doi <- dois[1:2]
  cr2 <- crossref_doi(doi, c("DOI", "title"))
  expect_equal(cr2$DOI, doi)
  expect_setequal(names(cr2), c("DOI", "title"))

  # paper
  doi <- psychsci[[1]]
  cr3 <- crossref_doi(doi, c("DOI", "title"))
  expect_equal(cr3$DOI, cr2$DOI[[1]])

  # paperlist
  doi <- psychsci[1:2]
  cr4 <- crossref_doi(doi, c("DOI", "title"))
  expect_equal(cr4$DOI, cr2$DOI)
})

test_that("datacite_doi", {
  expect_true(is.function(metacheck::datacite_doi))
  expect_no_error(helplist <- help(datacite_doi, metacheck))

  expect_error(datacite_doi(badarg))

  # NULL VALUE
  doi <- NULL
  info <- datacite_doi(doi)
  expect_equal(nrow(info), 0)

  # empty vector
  doi <- c()
  info <- datacite_doi(doi)
  expect_equal(nrow(info), 0)

  # skip_api("api.datacite.org")

  # one item
  doi <- "10.5281/zenodo.2669586"
  info <- datacite_doi(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$service, "datacite")
  expect_equal(info$title, "faux: Simulation for Factorial Designs")

  # one NA
  doi <- NA
  info <- datacite_doi(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$service, "datacite")
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
  expect_equal(oa[[1]]$DOI, doi)
  expect_equal(oa[[1]]$error, "malformed")

  # skip_api("api.openalex.org")

  # short DOI
  doi <- "10.1177/0956797614520714"
  oa <- openalex_doi(doi)
  expect_equal(oa[[1]]$is_retracted, TRUE)
  expect_equal(oa[[1]]$abstract, "We propose that dishonest and creative behavior have something in common: They both involve breaking rules. Because of this shared feature, creativity may lead to dishonesty (as shown in prior work), and dishonesty may lead to creativity (the hypothesis we tested in this research). In five experiments, participants had the opportunity to behave dishonestly by overreporting their performance on various tasks. They then completed one or more tasks designed to measure creativity. Those who cheated were subsequently more creative than noncheaters, even when we accounted for individual differences in their creative ability (Experiment 1). Using random assignment, we confirmed that acting dishonestly leads to greater creativity in subsequent tasks (Experiments 2 and 3). The link between dishonesty and creativity is explained by a heightened feeling of being unconstrained by rules, as indicated by both mediation (Experiment 4) and moderation (Experiment 5).")

  # long DOI
  doi <- c("https://doi.org/10.1177/0956797613520608")
  oa <- openalex_doi(doi)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")

  # multiple DOIs
  dois <- c("10.1177/0956797613520608", "10.1177/0956797614522816")
  oa <- openalex_doi(dois)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")
  expect_equal(oa[[2]]$id, "https://openalex.org/W2103593746")

  # DOI from paper
  paper <- psychsci[[1]]
  oa <- openalex_doi(paper)
  expect_equal(oa[[1]]$id, "https://openalex.org/W2134722098")

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
  expect_equal(oa[[1]]$is_retracted, TRUE)
})

test_that("openalex_query", {
  expect_true(is.function(metacheck::openalex_query))
  expect_no_error(helplist <- help(openalex_query, metacheck))

  expect_error(openalex_query(bad_arg))

  # skip_api("api.openalex.org")

  doi <- "https://doi.org/10.1525/collabra.33267"
  title <- "Sample Size Justification"
  source <- "Collabra Psychology"
  authors <- "Lakens, D"
  b <- openalex_query(title, source, authors)
  expect_equal(nrow(b), 1)
  expect_equal(b$display_name, title)
  expect_equal(b$doi, doi)
})


httptest2::stop_mocking()
#httptest2::stop_capturing()
