test_that("osf_preprint_list", {
  expect_true(is.function(metacheck::osf_preprint_list))
  expect_no_error(helplist <- help(osf_preprint_list, metacheck))

  expect_error(osf_preprint_list(bad_arg))
})

# httptest::start_capturing()
httptest::use_mock_api()

test_that("defaults", {
  pp <- osf_preprint_list()
  expect_equal(nrow(pp), 10)

  # test filters
  provider <- "psyarxiv"
  psyarxiv <- osf_preprint_list(provider)
  expect_equal(nrow(psyarxiv), 10)

  psyarxiv20 <- osf_preprint_list(provider, page_end = 2)
  expect_equal(nrow(psyarxiv20), 20)
  expect_equal(psyarxiv$osf_id, psyarxiv20$osf_id[1:10])

  psyarxiv10 <- osf_preprint_list(provider, page_start = 2)
  expect_equal(nrow(psyarxiv10), 10)
  expect_equal(psyarxiv20$osf_id[11:20], psyarxiv10$osf_id)

  # date_created
  date_created <- psyarxiv$date_created[[1]]
  psyarxiv1 <- osf_preprint_list(provider = provider,
                                 date_created = date_created)
  expect_equal(nrow(psyarxiv1), 1)
  expect_equal(psyarxiv$name[[1]], psyarxiv1$name[[1]])

  date_created <- c("2022-09-01", "2022-09-02")
  psyarxiv2 <- osf_preprint_list(provider = provider,
                                 date_created = date_created)
  dates <- substr(psyarxiv2$date_created, 1, 10)
  expect_true(all(dates %in% date_created))

  # date_modified
  date_modified <- psyarxiv$date_modified[[1]]
  psyarxiv1m <- osf_preprint_list(provider = provider,
                                  date_modified = date_modified)
  expect_equal(nrow(psyarxiv1m), 1)
  expect_equal(psyarxiv$name[[1]], psyarxiv1m$name[[1]])

  date_modified <- c("2022-09-01", "2022-09-02")
  psyarxiv2m <- osf_preprint_list(provider = provider,
                                 date_modified = date_modified)
  dates <- substr(psyarxiv2m$date_modified, 1, 10)
  expect_true(all(dates %in% date_modified))
})

httptest::stop_mocking()
# httptest::stop_capturing()
