test_that("coi_check", {
  module <- "coi_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no COI
  paper <- test_paper("There is a conflict between X and Y.")
  expect_no_error( mo <- module_run(paper, module))
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 0)

  # has a COI
  paper <- test_paper("The researchers state no conflict of interest.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "green")
  expect_equal(nrow(mo$table), 1)

  # paperlist
  paper <- paperlist(
    test_paper("The researchers state no conflict of interest."),
    test_paper("There is a conflict between X and Y.")
  )
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$coi_found, c(TRUE, FALSE))
})

test_that("funding_check", {
  module <- "funding_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- demopaper()
  expect_no_error( mo <- module_run(paper, module))

  # no funding
  paper <- test_paper("The funding for arts is not great.")
  expect_no_error( mo <- module_run(paper, module))
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 0)

  # has funding
  paper <- test_paper("This research was funded by UKRI grant #202020.")
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "green")
  expect_equal(nrow(mo$table), 1)

  # paperlist
  paper <- paperlist(
    test_paper("This research was funded by UKRI grant #202020."),
    test_paper("The funding for arts is not great.")
  )
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$summary_table), 2)
  expect_equal(mo$summary_table$funding_found, c(T,F))
})
