# httptest::start_capturing()
httptest::use_mock_api()

test_that("bibsearch", {
  expect_true(is.function(metacheck::bibsearch))
  expect_no_error(helplist <- help(bibsearch, metacheck))

  expect_error(bibsearch(bad_arg))

  doi <- "https://doi.org/10.1525/collabra.33267"
  title <- "Sample Size Justification"
  source <- "Collabra Psychology"
  authors <- "Lakens, D"
  b <- bibsearch(title, source, authors)
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
