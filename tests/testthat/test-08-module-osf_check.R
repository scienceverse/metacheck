httptest::with_mock_api({
  wd <- getwd()
  httptest::.mockPaths(wd)

  test_that("osf_check", {
    module <- "osf_check"

    verbose(FALSE)

    text <- data.frame(
      text = c("https://osf.io/5tbm9/",
               "https://osf.io/629bx/",
               "osf.io/ abcd5"),
      id = c("private", "public", "unfound")
    )
    expect_warning( mo <- module_run(text, module), "abcd5" )

    if (all(mo$table$status == "unknown")) {
      skip("OSF is down")
    }
    exp <- c("closed", "open", "unfound")
    expect_equal(mo$table$status, exp)

    # iteration
    paper <- psychsci[5:10]
    mod_output <- module_run(paper, module)
    ids <- mod_output$table$id |> unique()
    expect_equal(ids, c("0956797615569001",
                        "0956797615569889",
                        "0956797615583071"))

    verbose(TRUE)
  })

  test_that("osf_check no OSF", {
    module <- "osf_check"
    paper <- read("problems/Takagishi.xml")
    mod_output <- module_run(paper, module)
    expect_equal(mod_output$table, data.frame(id = character(0)))
    expect_equal(mod_output$summary, data.frame(id = paper$id))
    expect_equal(mod_output$traffic_light, "na")
  })

})
