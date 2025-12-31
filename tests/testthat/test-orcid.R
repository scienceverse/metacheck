test_that("exists", {
  expect_true(is.function(metacheck::get_orcid))
  expect_no_error(helplist <- help(get_orcid, metacheck))

  expect_true(is.function(metacheck::orcid_person))
  expect_no_error(helplist <- help(orcid_person, metacheck))
})

test_that("errors", {
  expect_error(get_orcid(bad_arg))
  expect_error(orcid_person(bad_arg))
})

# httptest::start_capturing()
httptest::use_mock_api()

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

  suppressMessages( obs <- get_orcid("DeBruine") )
  expect_true(length(obs) > 1)
  expect_true(exp %in% obs)
})

test_that("orcid_person", {
  orcid <- "0000-0002-7523-5539"
  person <- orcid_person(orcid)
  expect_equal(person$orcid, orcid)
  expect_equal(person$given, "Lisa")
  expect_equal(person$family, "DeBruine")
  expect_equal(person$country, "GB")
  expect_equal(person$email[[1]], c("lisa.debruine@glasgow.ac.uk", "debruine@gmail.com"))
})

httptest::stop_mocking()
# httptest::stop_capturing()
