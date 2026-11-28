test_that("open_practices", {
  module <- "open_practices"
  mods <- module_list()
  expect_true(module %in% mods$name)
})

test_that("single paper", {
  module <- "open_practices"
  paper <- demopaper()
  mo <- module_run(paper, module)

  expect_equal(mo$traffic_light, "green")
  expect_true(any(mo$table$data))
  expect_true(any(mo$table$code))
  expect_false(any(mo$table$materials))
  expect_true(any(mo$table$prereg))
})

test_that("open_practices paperlist", {
  module <- "open_practices"
  paper <- paperlist(
    test_paper("Open code is available at https://github.com/repo/mine."),
    test_paper("Data available upon request.")
  )
  mo <- module_run(paper, module)
  expect_equal(nrow(mo$table), length(paper))
  expect_equal(mo$table$data, c(F, T))
  expect_equal(mo$table$code, c(T, F))
  expect_equal(mo$table$on_request, c(F, T))

  # on request is flagged, but is not open sharing
  expect_equal(mo$summary_table$data_open, c(F, F))
  expect_equal(mo$summary_table$on_request, c(F, T))

  # unless the same sentence also names a repository
  paper <- test_paper("Data are available at https://osf.io/hk4yq/; raw data on request.")
  mo <- module_run(paper, module)
  expect_equal(mo$summary_table$data_open, TRUE)
  expect_equal(mo$traffic_light, "yellow")   # yellow, not red (#695, point 2)
})


test_that("open_practices only open data", {
  # examples from https://authorservices.taylorandfrancis.com/data-sharing-policies/open-data/
  module <- "open_practices"
  paper <- test_paper("Data for all experiments have been made publicly available on OSF at https://osf.io/hk4yq/.")
  mo <- module_run(paper, module)
  expect_equal(mo$table$data, TRUE)
})

test_that("open_practices only open code", {
  statements <- c("The computer code for the analyses reported here can be accessed at the Open Science Framework (https://osf.io/geq9x/).",
                  "All analysis code for this study has been made publicly available via the Open Science Framework and can be accessed at https://osf.io/geq9x/.")

  # just statements
  module <- "open_practices"
  paper <- test_paper(statements)
  mo <- module_run(paper, module)
  expect_equal(mo$table$data, c(F, F))
  expect_equal(mo$table$code, c(T, T))
  expect_equal(mo$table$text, statements)
  expect_equal(mo$summary_table$data_open, F)
  expect_equal(mo$summary_table$code_open, T)
})

test_that("open_practices both open data and code", {
  # examples from https://authorservices.taylorandfrancis.com/data-sharing-policies/open-data/
  module <- "open_practices"
  paper <- test_paper("The data and code to reproduce the findings of this study are available at the Open Science Framework at https://osf.io/abcde.")
  mo <- module_run(paper, module)
  expect_equal(mo$table$data, TRUE)
  expect_equal(mo$table$code, TRUE)
  expect_equal(mo$summary_table$data_open, T)
  expect_equal(mo$summary_table$code_open, T)
})


test_that("error: argument is of length zero", {
  skip_no_psychsci()
  module <- "open_practices"
  paper <- psychsci$`0956797617714811`
  mo <- module_run(paper, module)

  expect_equal(nrow(mo$table), 0)
  expect_equal(mo$summary_table$data_open, FALSE)
})

test_that("open_practices: a list counts papers without a statement, on-request ignores case (U106, U107)", {
  # a list with one flagged paper got the single-paper light and summary;
  # "ON REQUEST" was not flagged
  p1 <- test_paper("All data are available at https://osf.io/abcde."); p1$paper_id <- "p1"
  p2 <- test_paper("We report r(68) = .58."); p2$paper_id <- "p2"
  mo <- module_run(paperlist(p1, p2), "open_practices")
  expect_equal(mo$traffic_light, "info")
  expect_match(mo$summary_text, "1 only data, 0 only code, and 1 neither", fixed = TRUE)
  mo3 <- module_run(test_paper("Data are available ON REQUEST from the authors."), "open_practices")
  expect_match(mo3$summary_text, "only on request", fixed = TRUE)
})

test_that("open_practices: the badges a publisher's XML declares", {
  # jats_cases.xml declares the Open Data badge, and no other
  mo <- module_run(read(test_path("fixtures", "formats", "jats_cases.xml")), "open_practices")
  st <- mo$summary_table
  expect_true(st$badge_data)
  expect_false(st$badge_materials)
  expect_false(st$badge_prereg)
  expect_true(any(grepl("open-science badge Open Data", mo$report, fixed = TRUE)))

  # a paper read from a PDF does not say: NA, and no line in the report
  mo <- module_run(read(demofile("xml")), "open_practices")
  expect_true(is.na(mo$summary_table$badge_data))
  expect_false(any(grepl("badge", mo$report)))
})

test_that("open_practices: a badge wins over a search that finds nothing", {
  # jats_cases.xml has the Open Data badge, and no sentence the search counts
  mo <- module_run(read(test_path("fixtures", "formats", "jats_cases.xml")), "open_practices")
  st <- mo$summary_table
  expect_true(st$data_open)
  expect_equal(st$data_source, "metadata")
  expect_true(is.na(st$materials_source))
  expect_true(any(grepl("according to the Open Data badge", mo$report, fixed = TRUE)))

  # a sentence without a badge: the text's answer alone
  p <- test_paper("All data are available at https://osf.io/abcde.")
  st <- module_run(p, "open_practices")$summary_table
  expect_true(st$data_open)
  expect_equal(st$data_source, "text")
})
