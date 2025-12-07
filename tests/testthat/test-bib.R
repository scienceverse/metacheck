verbose(TRUE)
email("debruine@gmail.com") # for openalex searches

test_that("exists", {
  expect_true(is.function(metacheck::bibsearch))
  expect_no_error(helplist <- help(bibsearch, metacheck))
})

test_that("errors", {
  expect_error(bibsearch(bad_arg))
})

# TODO: figure out why with_mock_api isn't working
#httptest::start_capturing()

httptest::with_mock_api({

skip_bib <- function() {
  skip_if_offline("api.openalex.org")
  skip_on_ci()
  skip("Open Alex functions")
}

test_that("bibsearch", {
  skip_bib()

  doi <- "https://doi.org/10.1525/collabra.33267"
  title <- "Sample Size Justification"
  source <- "Collabra Psychology"
  authors <- "Lakens, D"
  expect_message(b <- bibsearch(title, source, authors))
  expect_equal(nrow(b), 1)
  expect_equal(b$display_name, title)
  expect_equal(b$doi, doi)
})

test_that("bibtex_add_dois", {
  skip_bib()

  bibfile <- "missing_dois.bib"
  save_to <- "missing_dois_strict.bib"
  unlink(save_to)

  expect_message(strict <- bibtex_add_dois(bibfile, save_to))
  expect_true(file.exists(save_to))
  expect_equal(nrow(strict), 17)
  expect_equal(is.na(strict$DOI) |> sum(), 6)
  strict_msgs <- attr(strict, "msgs")

  save_to <- "missing_dois_nostrict.bib"
  unlink(save_to)

  expect_message(nostrict <- bibtex_add_dois(bibfile, save_to, strict = FALSE))
  expect_true(file.exists(save_to))
  expect_equal(nrow(nostrict), 17)
  expect_equal(is.na(nostrict$DOI) |> sum(), 3)

  # check for a missing doi
  doi <- "10.1371/journal.pone.0281086"
  expect_equal(strict$DOI[[1]], doi)
  expect_equal(nostrict$DOI[[1]], doi)
})

test_that("bib_add_dois", {
  skip_bib()

  bib <- psychsci[[2]]$bib[1:10, ]
  expect_message(bib_strict <- bib_add_dois(bib))
  expect_message(bib_nostrict <- bib_add_dois(bib, strict = FALSE))

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

}) # end with_mock_api

#httptest::stop_capturing()
