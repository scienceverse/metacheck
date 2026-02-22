test_that("exists", {
  expect_true(is.function(metacheck::info_table))
  expect_no_error(helplist <- help(info_table, metacheck))
})

test_that("errors", {
  expect_error(info_table(),
               "argument \"paper\" is missing, with no default",
               fixed = TRUE)
})

test_that("defaults", {
  # list of papers
  paper <- psychsci[1:3]
  info <- c("paper_id", "title", "doi")
  infotable <- info_table(paper)

  expect_equal(infotable$paper_id, names(paper))
  expect_equal(names(infotable), info)

  # one paper
  paper <- demopaper()
  infotable <- info_table(paper)

  expect_equal(infotable$paper_id, paper$paper_id)
  expect_equal(names(infotable), info)
})

test_that("missing items", {
  # list of papers
  paper <- demopaper()
  info <- c("doi", "title", "not a column")
  infotable <- info_table(paper, info)

  expected <- c("paper_id", info)
  expect_equal(names(infotable), expected)

  expect_equal(infotable$`not a column`, NA)

  info <- c("doi", "title", "paper_id")
  infotable <- info_table(paper, info)
  expect_equal(names(infotable), info)
})
