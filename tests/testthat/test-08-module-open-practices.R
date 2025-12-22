test_that("open_practices", {
  module <- "open_practices"
  mods <- module_list()
  expect_true(module %in% mods$name)

  paper <- read(demoxml())
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "green")
  expect_equal(mo$table$is_open_data, TRUE)
  expect_equal(mo$table$is_open_code, TRUE)

  paper <- psychsci[100:101]
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$table), length(paper))
  expect_equal(mo$table$id, mo$summary_table$id)
  expect_equal(mo$table$is_open_data, c(T, T))
  expect_equal(mo$table$is_open_code, c(T, F))
})
