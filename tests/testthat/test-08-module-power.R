test_that("power, no LLM", {
  skip("UNDER DEVELOPMENT")

  module <- "power"
  mods <- module_list()
  expect_true(module %in% mods$name)

  llm_use(FALSE)

  # no relevant text
  paper <- psychsci[[1]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(nrow(mod_output$summary), 1)
  expect_equal(mod_output$summary$power.n, 0)
  expect_equal(mod_output$summary$power.complete, NA_integer_)

  # several power sentences in one paragraph
  paper <- psychsci[[10]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 2)
  expect_equal(nrow(mod_output$summary), 1)
  expect_equal(mod_output$summary$power.n, 2)
  expect_equal(mod_output$summary$power.complete, NA_integer_)

  # multiple papers
  paper <- psychsci[10:15]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "yellow")
  expect_equal(nrow(mod_output$table), 4)
  expect_equal(nrow(mod_output$summary), 6)
  expect_equal(mod_output$summary$power.n, c(2, 0, 0, 0, 1, 1))
  expect_equal(mod_output$summary$power.complete, rep(NA_integer_, 6))
})

test_that("power, with LLM", {
  skip("UNDER DEVELOPMENT")

  module <- "power"

  llm_use(TRUE)

  # no relevant text
  paper <- psychsci[[1]]
  mod_output <- module_run(paper, module)
  expect_equal(mod_output$traffic_light, "na")
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(nrow(mod_output$summary), 1)
  expect_equal(mod_output$summary$power.n, 0)
  expect_equal(mod_output$summary$power.complete, NA_integer_)

  #httptest::start_capturing()

  httptest::with_mock_api({
    wd <- getwd()
    httptest::.mockPaths(wd)

    # several power sentences in one paragraph
    paper <- psychsci[[10]]
    mod_output <- module_run(paper, module)
    expect_equal(mod_output$traffic_light, "red")
    expect_equal(nrow(mod_output$table), 3)
    expect_equal(mod_output$table$sample, c(13500, 24, 72) |> as.character())
    expect_equal(nrow(mod_output$summary), 1)
    expect_equal(mod_output$summary$power.n, 3)
    expect_equal(mod_output$summary$power.complete, 2)

    # multiple papers
    paper <- psychsci[10:15]
    mod_output <- module_run(paper, module)
    expect_equal(mod_output$traffic_light, "red")
    expect_equal(nrow(mod_output$table), 5)
    expect_equal(nrow(mod_output$summary), 6)
    expect_equal(mod_output$table$complete, c(F, T, T, F, F))
    expect_equal(mod_output$summary$power.n, c(3, 0, 0, 0, 1, 1))
    expect_equal(mod_output$summary$power.complete, c(2, NA, NA, NA, 0, 0))
  })

  #httptest::stop_capturing()
})
