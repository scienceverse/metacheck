#httptest::start_capturing()
httptest::use_mock_api()

test_that("ref_doi_check", {
  module <- "ref_doi_check"
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
  expect_equal(nrow(mod_output$table), 1)
  expect_equal(mod_output$summary_table$refs_checked, 1)

  # if offline, none will be found
  exp <- ifelse(online(), 1, 0)
  expect_equal(mod_output$summary_table$doi_found, exp)
})

test_that("ref_accuracy", {
  module <- "ref_accuracy"
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
  exp <- ifelse(online("api.labs.crossref.org"), "yellow", "fail")
  expect_equal(mod_output$traffic_light, exp)
  expect_equal(nrow(mod_output$table), 3)

  skip_if_offline("api.labs.crossref.org")
  expect_equal(mod_output$summary_table$refs_checked, 3)
  expect_equal(mod_output$summary_table$refs_not_found, 1)
  expect_equal(mod_output$summary_table$title_mismatch, 1)
  expect_equal(mod_output$summary_table$author_mismatch, 0)
})

test_that("ref_doi_check + ref_accuracy", {
  skip_if_offline("api.labs.crossref.org")

  paper <- read(demoxml())
  mod_output <- paper |>
    module_run("ref_doi_check") |>
    module_run("ref_accuracy")

  expect_equal(nrow(mod_output$table), nrow(paper$bib))
})


test_that("ref_replication", {
  module <- "ref_replication"
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

test_that("ref_retraction", {
  module <- "ref_retraction"
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

test_that("ref_pubpeer", {
  module <- "ref_pubpeer"
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
  expect_equal(sum(mod_output$table$total_comments), 3)
})

test_that("chaining", {
  paper <- read(demoxml())
  # remove DOIs to make sure rw/reps/pp are getting DOIs from rc
  paper$bib$doi <- NA

  mo1 <- module_run(paper, "ref_doi_check")
  expect_equal(mo1$summary_table$doi_found, 3)

  mo2 <- module_run(mo1, "ref_retraction")
  expect_equal(mo2$table$doi, "10.1177/0956797614520714")
  expect_equal(mo2$summary_table$doi_found, 3)

  mo3 <- module_run(mo2, "ref_replication")
  expect_equal(mo3$table$doi, "10.1098/rspb.1998.0380")
  expect_equal(mo3$summary_table$retractionwatch, 1)

  mo4 <- module_run(mo3, "ref_pubpeer")
  expect_equal(mo4$table$total_comments, 3)
  expect_equal(mo4$summary_table$replications, 1)
})

test_that("ref_consistency", {
  module <- "ref_consistency"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- demoxml() |> read()

  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "red")
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(mod_output$module, module)

  # iteration
  paper <- psychsci[c(23, 25)]
  mod_output1 <- module_run(paper[[1]], module)
  mod_output2 <- module_run(paper[[2]], module)
  mod_output3 <- module_run(paper, module)
  expect_equal(rbind(mod_output1$table, mod_output2$table),
               mod_output3$table)

  mod_output1$traffic_light
  mod_output2$traffic_light
  mod_output3$traffic_light
})

test_that("ref_miscitation", {
  module <- "ref_miscitation"
  mods <- module_list()
  expect_true(module %in% mods$name)

  filename <- test_path("fixtures", "problem_xml")
  paper <- read(filename)

  mod_output <- module_run(paper, module)
  expect_true("10.1525/collabra.33267" %in% mod_output$table$doi)
  expect_true("miscite_10.1525/collabra.33267" %in%
                names(mod_output$summary_table))
  expect_equal(nrow(mod_output$summary_table),
               list.files(filename) |> length())
  expect_equal(mod_output$traffic_light, "yellow")

  ## custom db
  test_doi <- "10.1038/nrn3475"
  db <- data.frame(
    doi = test_doi,
    reftext = "The full reference (this is a test)",
    warning = "Lorem ipsum this is a test..."
  )

  mod_output <- module_run(paper, module, db = db)
  expect_equal(mod_output$table$doi[[1]], test_doi)
  expect_equal(mod_output$summary_table$`miscite_10.1038/nrn3475`,
               c("b11", NA, NA, NA, "b6"))

  ## custom db - doi is in bib but not xref text!
  test_doi <- "10.1016/j.anbehav.2022.09.006"
  db <- data.frame(
    doi = test_doi,
    reftext = "The full reference (this is a test)",
    warning = "Lorem ipsum this is a test..."
  )

  mod_output <- module_run(paper, module, db = db)
  expect_equal(mod_output$table$doi, test_doi)
})

httptest::stop_mocking()
#httptest::stop_capturing()
