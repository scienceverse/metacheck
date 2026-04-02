test_that("add_bib_match", {
  expect_true(is.function(metacheck::add_bib_match))
  expect_no_error(helplist <- help(add_bib_match, metacheck))

  expect_error(add_bib_match(bad_arg))

  # no refs
  paper <- test_paper("No refs")
  paper_bm <- add_bib_match(paper)
  expect_equal(nrow(paper_bm$bib), 0)
  expect_equal(paper_bm$bib_match, NULL)

  skip_api("api.labs.crossref.org")

  # only unmatching refs
  paper <- test_paper("No matching refs")
  paper$bib <- data.frame(
    bib_id = 1,
    title = c("Not a real paper"),
    container = c("Journal of Journals"),
    authors = I(list(c("Not A. Realname")))
  )
  paper_bm <- add_bib_match(paper)
  expect_equal(paper_bm$bib$bib_id, 1)
  expect_equal(nrow(paper_bm$bib_match), 0)

  # make paper with 2 refs
  paper <- test_paper("LDB test papers")
  paper$bib <- data.frame(
    bib_id = 1:2,
    title = c("Facial resemblance enhances trust",
              "Trustworthy but not Lustworthy"),
    container = c("Proceedings of the Royal Society of London B"),
    authors = I(list("Lisa DeBruine", "Lisa DeBruine"))
  )
  paper_bm <- add_bib_match(paper, 0)

  expect_equal(paper_bm$bib_match$bib_id, 1:2)
  expect_equal(paper_bm$bib_match$doi, c("10.1098/rspb.2002.2034",
                                         "10.1098/rspb.2004.3003"))

  # set threshold between two papers
  min_score <- mean(paper_bm$bib_match$score)
  paper_bm2 <- add_bib_match(paper, min_score)
  expect_equal(paper_bm2$bib_match$doi, "10.1098/rspb.2002.2034")

  # paperlist
  paper1 <- test_paper()
  paper1$bib <- data.frame(
    bib_id = 1:2,
    title = c("Facial resemblance enhances trust",
              "Trustworthy but not Lustworthy"),
    container = c("Proceedings of the Royal Society of London B"),
    authors = I(list("Lisa DeBruine", "Lisa DeBruine"))
  )
  paper2 <- test_paper()
  paper2$bib <- data.frame(
    bib_id = 1:2,
    title = c("Trustworthy but not Lustworthy",
              "Equivalence Tests: A Practical Primer for t Tests, Correlations, and Meta-Analyses"),
    container = c("Proceedings of the Royal Society of London B",
                  "Social Psychological and Personality Science"),
    authors = I(list("Lisa DeBruine", "Daniel Lakens"))
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
  skip_api("api.labs.crossref.org")

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



test_that("doi_lookup", {
  expect_true(is.function(metacheck::doi_lookup))
  expect_no_error(helplist <- help(doi_lookup, metacheck))

  expect_error(doi_lookup(badarg))

  # NULL VALUE
  doi <- NULL
  exp <- data.frame(doi = character(0))
  info <- doi_lookup(doi)
  expect_equal(info, exp)

  # empty vector
  doi <- c()
  exp <- data.frame(doi = character(0))
  info <- doi_lookup(doi)
  expect_equal(info, exp)

  skip_api("doi.org")

  # one item
  doi <- "10.7717/peerj.4375"
  info <- doi_lookup(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$title, "The state of OA: a large-scale analysis of the prevalence and impact of Open Access articles")

  # one NA
  doi <- NA
  info <- doi_lookup(doi)
  expect_equal(nrow(info), 1)
  expect_equal(info$title, NA_character_)

  # multiple items
  doi <- c("10.1177/2515245920970949", "10.1037/0003-066x.54.6.408")
  info <- doi_lookup(doi)
  exp <- c("Improving Transparency, Falsifiability, and Rigor by Making Hypothesis Tests Machine-Readable",
           "The origins of sex differences in human behavior: Evolved dispositions versus social roles.")
  expect_equal(info$title, exp)
})

test_that("doi_clean", {
  expect_true(is.function(metacheck::doi_clean))
  expect_no_error(helplist <- help(doi_clean, metacheck))

  expect_error(doi_clean(bad_arg))

  exp <- "10.1038/nphys1170"
  doi <- "https://doi.org/10.1038/nphys1170"
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  doi <- "doi:10.1038/nphys1170"
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  doi <- "  DOI : 10.1038/nphys1170 "
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  doi <- "bad.doi"
  clean <- doi_clean(doi)
  expect_equal(clean, doi)

  # multiple
  doi <- c(
    "https://doi.org/10.1038/nphys1170",
    "doi:10.1038/nphys1170",
    "  DOI : 10.1038/nphys1170 ",
    "10.1038/nphys1170",
    "",
    NA
  )
  clean <- doi_clean(doi)
  exp <- rep("10.1038/nphys1170", 4) |> c("", NA)
  expect_equal(clean, exp)

  doi <- list(
    "https://doi.org/10.1038/nphys1170",
    "doi:10.1038/nphys1170",
    "  DOI : 10.1038/nphys1170 ",
    "10.1038/nphys1170",
    "",
    NA
  )
  clean <- doi_clean(doi)
  expect_equal(clean, exp)

  # journal-specific format
  doi <- "http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0004153"
  clean <- doi_clean(doi)
  exp <- "10.1371/journal.pone.0004153"
  expect_equal(clean, exp)

  # make sure the start match at the first 10.###
  doi <- "10.1234/more.10.2345"
  clean <- doi_clean(doi)
  expect_equal(clean, doi)

  # remove post # sections
  doi <- "10.1234/mypaper#section"
  clean <- doi_clean(doi)
  expect_equal(clean, "10.1234/mypaper")

  # remove post /full
  doi <- "10.1234/mypaper/full"
  clean <- doi_clean(doi)
  expect_equal(clean, "10.1234/mypaper")
})

test_that("doi_valid_format", {
  expect_true(is.function(metacheck::doi_valid_format))
  expect_no_error(helplist <- help(doi_valid_format, metacheck))

  expect_error(doi_valid_format(bad_arg))

  doi <- "10.1038/nphys1170"
  v <- doi_valid_format(doi)
  expect_equal(v, T)

  doi <- "no10.1038/nphys1170"
  v <- doi_valid_format(doi)
  expect_equal(v, F)

  doi <- NA
  v <- doi_valid_format(doi)
  expect_equal(v, F)

  doi <- c("10.1038/nphys1170", "bad", NA)
  v <- doi_valid_format(doi)
  expect_equal(v, c(T, F, F))

  # odd format
  doi <- "10.1002/(SICI)1099-1611(200001/02)9:1<11::AID-PON424>3.0.CO;2-Z"
  v <- doi_valid_format(doi)
  expect_equal(v, T)
})

# httptest::start_capturing()
httptest::use_mock_api()

test_that("doi_resolves", {
  expect_true(is.function(metacheck::doi_resolves))
  expect_no_error(helplist <- help(doi_resolves, metacheck))

  expect_error(doi_resolves(bad_arg))

  doi <- "10.1038/nphys1170"
  check <- doi_resolves(doi)
  expect_true(check)

  doi <- "10.1234/invalid.doi"
  check <- doi_resolves(doi)
  expect_false(check)

  check <- doi_resolves(NA)
  expect_equal(check, NA)

  check <- doi_resolves("")
  expect_equal(check, NA)

  # invalid format
  check <- doi_resolves("bad.doi")
  expect_false(check)

  # multiple
  doi <- c("10.1038/nphys1170", "10.1234/invalid.doi", "bad.doi", NA)
  check <- doi_resolves(doi)
  expect_equal(check, c(T, F, F, NA))

  # clean DOIs
  doi <- "https://doi.org/10.1038/nphys1170"
  check <- doi_resolves(doi)
  expect_true(check)

  doi <- "doi:10.1038/nphys1170"
  check <- doi_resolves(doi)
  expect_true(check)

  doi <- "  DOI : 10.1038/nphys1170 "
  check <- doi_resolves(doi)
  expect_true(check)

  # try to break it
  doi <- list("10.1038/nphys1170", "10.1234/invalid.doi", NA)
  check <- doi_resolves(doi)
  expect_equal(check, c(T, F, NA))

  # should it do this or break?
  doi <- list(c("10.1038/nphys1170", "10.1234/invalid.doi"),
              c("10.1038/nphys1170", NA))
  check <- doi_resolves(doi)
  expect_equal(check, c(T, F, T, NA))

  # server not available - without_internet not working!
  doi <- c("10.1038/nphys1170", "10.1234/invalid.doi", "bad.doi", NA)
  # httptest::without_internet({
  #   check <- doi_resolves(doi)
  #   expect_equal(check, c(NA, NA, NA, F))
  # })
})

test_that("crossref_query", {
  expect_true(is.function(metacheck::crossref_query))
  expect_no_error(helplist <- help(crossref_query, metacheck))

  skip_api("api.labs.crossref.org")

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
  expect_equal(names(obs), c("ref", "DOI"))

  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."
  obs <- crossref_query(ref, select = c("score", "title"))
  expect_equal(names(obs), c("ref", "score", "title"))

  # from bibentry
  ref <- bibentry(
    "article",
    journal = psychsci[[1]]$bib$container[[1]],
    title = psychsci[[1]]$bib$title[[1]],
    author = psychsci[[1]]$bib$authors[[1]],
    year = psychsci[[1]]$bib$year[[1]]
  )
  obs <- crossref_query(ref)
  exp <- "10.1093/brain/110.3.747"
  expect_equal(obs$DOI, exp)

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
  skip_api("api.labs.crossref.org")

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

test_that(".crossref_batch_query", {
  expect_true(is.function(metacheck:::.crossref_batch_query))

  expect_error(.crossref_batch_query())


})


httptest::stop_mocking()
# httptest::stop_capturing()
