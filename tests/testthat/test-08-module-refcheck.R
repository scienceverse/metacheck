verbose(FALSE)

#httptest::start_capturing()
##httptest::with_mock_api({

test_that("reference_check", {
  module <- "reference_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references - info
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(mod_output$summary_table$refs_checked, 4)
  expect_equal(mod_output$summary_table$missing_doi, 1)
  expect_equal(mod_output$summary_table$doi_found, 1)
  expect_equal(mod_output$summary_table$doi_mismatch, 0)
})


test_that("replications", {
  module <- "replications"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)
})

test_that("retractionwatch", {
  module <- "retractionwatch"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  # relevant references
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$table$retractionwatch, "Retraction")
})

test_that("pubpeer", {
  module <- "pubpeer"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no references
  paper <- psychsci[[210]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_null(mod_output$table)

  skip_if_offline("pubpeer.com")
  # relevant references
  paper <- read(demoxml())
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "info")
  expect_equal(sum(mod_output$table$total_comments), 3)
})

test_that("chaining", {
  paper <- read(demoxml())
  # remove DOIs to make sure rw/reps/pp are getting DOIs from rc
  paper$bib$doi <- NA

  mo1 <- module_run(paper, "reference_check")
  expect_equal(mo1$summary_table$missing_doi, 4)

  mo2 <- module_run(mo1, "retractionwatch")
  expect_equal(mo2$table$doi, "10.1177/0956797614520714")
  expect_equal(mo2$summary_table$refs_checked, 4)

  mo3 <- module_run(mo2, "replications")
  expect_equal(mo3$table$doi, "10.1098/rspb.1998.0380")
  expect_equal(mo3$summary_table$retractionwatch, 1)

  mo4 <- module_run(mo3, "pubpeer")
  expect_equal(mo4$table$total_comments, c(0, 3, 0, 0))
  expect_equal(mo4$summary_table$replications, 1)
})

#}) # end mock api
#httptest::stop_capturing()
