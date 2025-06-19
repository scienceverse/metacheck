test_that("exists", {
  expect_true(is.function(papercheck::info_table))
  expect_no_error(helplist <- help(info_table, papercheck))

  expect_true(is.function(papercheck::author_table))
  expect_no_error(helplist <- help(author_table, papercheck))
})

test_that("errors", {
  expect_error(info_table(),
               "argument \"paper\" is missing, with no default",
               fixed = TRUE)

  expect_error(author_table(),
               "argument \"paper\" is missing, with no default",
               fixed = TRUE)
})

test_that("defaults", {
  # list of papers
  paper <- demodir() |> read_grobid()
  info <- c("id", "filename", "title", "keywords", "doi")
  infotable <- info_table(paper)

  expect_equal(infotable$id, names(paper))
  expect_equal(names(infotable), info)

  # one paper
  paper <- demoxml() |> read_grobid()
  infotable <- info_table(paper)

  expect_equal(infotable$id, paper$id)
  expect_equal(names(infotable), info)
})

test_that("missing items", {
  # list of papers
  paper <- demodir() |> read_grobid()
  info <- c("doi", "title", "not a column")
  infotable <- info_table(paper, info)

  expected <- c("id", info)
  expect_equal(names(infotable), expected)

  expect_equal(infotable$`not a column`, rep(NA, length(paper)))

  info <- c("doi", "title", "id")
  infotable <- info_table(paper, info)
  expect_equal(names(infotable), info)
})

test_that("author_table", {
  paper <- psychsci[[1]]
  a1 <- author_table(paper)
  expect_equal(nrow(a1), 7)
  expect_equal(a1$n, 1:7)

  paper <- psychsci[1:2]
  a2 <- author_table(paper)
  expect_equal(nrow(a2), 10)
  expect_equal(a2$n, c(1:7, 1:3))
})
