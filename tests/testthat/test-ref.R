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
  dois <- info_table(psychsci, "doi")
  dois$doi <- gsub("pss\\.", "", dois$doi) |> gsub("sagepub\\.", "", x = _)
  doi <- dois$doi[1:2]
  cr2 <- crossref_doi(doi)
  expect_equal(nrow(cr2), 2)
  expect_equal(cr2$DOI, doi)
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

test_that("crossref_query", {
  expect_true(is.function(metacheck::crossref_query))
  expect_no_error(helplist <- help(crossref_query, metacheck))

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

  # select
  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."
  obs <- crossref_query(ref, select = c("DOI"))
  expect_equal(names(obs), c("ref", "DOI"))

  ref <- "Lakens, D., Mesquida, C., Rasti, S., & Ditroilo, M. (2024). The benefits of preregistration and Registered Reports. Evidence-Based Toxicology, 2(1)."
  obs <- crossref_query(ref, select = c("score", "title"))
  expect_equal(names(obs), c("ref", "score", "title"))

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

  # paper object as ref
  ref <- read(demoxml())
  obs <- crossref_query(ref)
  exp <- c("10.1098/rspb.1998.0380",
           "10.1177/0956797614520714",
           NA,
           "10.1177/2515245918770963"
  )
  expect_equal(obs$DOI, exp)

  # problem with encoded ( and )
  ref <- "Levi DM, Klein SA, Aitsebaomo AP, Mon-Williams M, Tresilian JR, Strang NC, Kochhar P, Wann JP (1985. 1998).\n“Improving vision: Neural compensation for optical defocus.” _Proceedings of the Royal Society B:\nBiological Sciences_, *25*, 71-77. doi:10.1016/0042-6989(85)90207\n<https://doi.org/10.1016/0042-6989(85)90207>."
  obs <- crossref_query(ref)
  exp <- "10.1098/rspb.1998.0266"
  expect_equal(obs$DOI, exp)

  # TODO: lots of terrible bib
  skip_if_quick()
  paper <- psychsci$`0956797620967261`
  obs <- crossref_query(paper$bib$ref)
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

httptest::stop_mocking()
# httptest::stop_capturing()
