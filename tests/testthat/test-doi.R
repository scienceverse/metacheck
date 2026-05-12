#httptest2::start_capturing()
httptest2::use_mock_api()

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

  #skip_api("doi.org")

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
  # httptest2::without_internet({
  #   check <- doi_resolves(doi)
  #   expect_equal(check, c(NA, NA, NA, F))
  # })
})

httptest2::stop_mocking()
#httptest2::stop_capturing()
