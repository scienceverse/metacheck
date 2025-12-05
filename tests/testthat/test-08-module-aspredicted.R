test_that("aspredicted", {
  module <- "aspredicted"
  mods <- module_list()
  expect_true(module %in% mods$name)

  # contains no AP link
  paper <- psychsci$`0956797613520608`
  mod_output <- module_run(paper, module)
  expect_equal(nrow(mod_output$table), 0)
  expect_equal(mod_output$summary_table$id, '0956797613520608')
  expect_equal(mod_output$summary_table$AP_links, 0)
  expect_equal(mod_output$traffic_light, "na")

  # check reporting
  report <- module_report(mod_output)
  exp <- "## AsPredicted {.na}\n\nNo AsPredicted links were found."
  expect_equal(exp, report)

  # httptest::start_capturing()

  httptest::with_mock_api({
    wd <- getwd()
    httptest::.mockPaths(wd)

    verbose(FALSE)

    # contains a link
    paper <- psychsci$`09567976221082938`
    mod_output <- module_run(paper, module)
    expect_equal(nrow(mod_output$table), 1)
    expect_true("AP_title" %in% names(mod_output$table))
    expect_true("AP_sample_size" %in% names(mod_output$table))
    expect_equal(mod_output$summary_table$id, '09567976221082938')
    expect_equal(mod_output$summary_table$AP_links, 1)
    expect_equal(mod_output$traffic_light, "info")
    expect_true(grepl(mod_output$table$AP_sample_size[[1]],
                      mod_output$report, fixed = TRUE))

    # check reporting
    report <- module_report(mod_output)
    exp <- "1 AsPredicted link was found and retrieved"
    expect_true(grepl(exp, report))
  })

  # httptest::stop_capturing()
})
