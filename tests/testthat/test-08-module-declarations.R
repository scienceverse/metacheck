test_that("coi_check", {
  module <- "coi_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # no COI
  paper <- read(demoxml())
  expect_no_error( mo <- module_run(paper, module))
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 0)

  # has a COI
  paper <- psychsci[[100]]
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "green")
  expect_equal(nrow(mo$table), 1)

  # paperlist
  paper <- psychsci[1:10]
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$summary_table), 10)
  expect_equal(mo$summary_table$coi_found, rep(TRUE, 10))
})

test_that("funding_check", {
  module <- "funding_check"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- read(demoxml())
  expect_no_error( mo <- module_run(paper, module))

  # no funding
  paper <- read(demoxml())
  expect_no_error( mo <- module_run(paper, module))
  expect_equal(mo$traffic_light, "red")
  expect_equal(nrow(mo$table), 0)

  # has funding
  paper <- psychsci[[1]]
  mo <- module_run(paper, module)
  expect_equal(mo$traffic_light, "green")
  expect_equal(nrow(mo$table), 1)

  # paperlist
  paper <- psychsci[1:5]
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$summary_table), 5)
  expect_equal(mo$summary_table$funding_found, rep(TRUE, 5))
})
