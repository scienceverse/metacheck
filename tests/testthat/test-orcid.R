test_that("exists", {
  expect_true(is.function(papercheck::get_orcid))
  expect_no_error(helplist <- help(get_orcid, papercheck))

  expect_true(is.function(papercheck::orcid_person))
  expect_no_error(helplist <- help(orcid_person, papercheck))
})

test_that("errors", {
  expect_error(get_orcid(bad_arg))
  expect_error(orcid_person(bad_arg))
})

httptest::with_mock_api({
  # httptest::start_capturing()

test_that("get_orcid", {
  obs <- get_orcid("DeBruine", "Lisa")
  exp <- "0000-0002-7523-5539"
  expect_equal(obs, exp)

  obs <- get_orcid("DeBruine", "L")
  expect_equal(obs, exp)

  obs <- get_orcid("DeBruine", "L.")
  expect_equal(obs, exp)

  obs <- get_orcid("DeBruine", "L. M.")
  expect_equal(obs, exp)

  verbose(FALSE)
  obs <- get_orcid("DeBruine")
  expect_true(length(obs) > 1)
  expect_true(exp %in% obs)
  verbose(TRUE)
})

test_that("orcid_person", {
  orcid <- "0000-0002-7523-5539"
  person <- orcid_person(orcid)
  expect_equal(person$orcid, orcid)
  expect_equal(person$given, "Lisa")
  expect_equal(person$family, "DeBruine")
  expect_equal(person$country, "GB")
  expect_equal(person$email, "lisa.debruine@glasgow.ac.uk;debruine@gmail.com")
})


# httptest::stop_capturing()

}) # end httptest::with_mock_api
