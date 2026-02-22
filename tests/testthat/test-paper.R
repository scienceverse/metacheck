test_that("exists", {
  expect_true(is.function(metacheck::paper))
  expect_true(is.function(metacheck::paperlist))
})

test_that("paper", {
  p <- paper()
  expect_s3_class(p, "scivrs_paper")
  exp_names <- c("paper_id", "info", "authors", "text", "links", "tables", "sections",
                 "bib", "xrefs", "figures", "equations")
  expect_contains(names(p), exp_names)
  expect_match(p$paper_id, "^[a-f0-9]{14}$")
  expect_equal(p$info, list())
  expect_equal(length(p$authors), 0)
  expect_equal(p$text, data.frame())
  expect_equal(p$sections, data.frame())
  expect_equal(p$bib, data.frame())
  expect_equal(p$xrefs, data.frame())
  expect_equal(p$links, data.frame())
  expect_equal(p$tables, data.frame())
  expect_equal(p$figures, data.frame())
})

test_that("paperlist", {
  # individual papers
  p1 <- psychsci[[1]]
  p2 <- psychsci[[2]]
  pl <- paperlist(p1, p2)

  expect_s3_class(pl, "scivrs_paperlist")
  expect_equal(names(pl), c(p1$paper_id, p2$paper_id))

  # single list of papers
  pl <- paperlist(psychsci[1:2])

  expect_s3_class(pl, "scivrs_paperlist")
  expect_equal(names(pl), c(p1$paper_id, p2$paper_id))

  # multiple lists of papers
  pl <- paperlist(psychsci[1:2], psychsci[3:4])

  expect_s3_class(pl, "scivrs_paperlist")
  expect_equal(names(pl), names(psychsci[1:4]))

  # single plus lists of papers
  pl <- paperlist(p1, psychsci[2:4])

  expect_s3_class(pl, "scivrs_paperlist")
  expect_equal(names(pl), names(psychsci[1:4]))

  # merge duplicate papers
  merged <- paperlist(psychsci[1:2], psychsci[2:3], psychsci[1:3])
  expect_equal(names(merged), names(psychsci[1:3]))

  # don't merge duplicate papers
  merged <- paperlist(psychsci[1:2], psychsci[2:3], merge_duplicates = FALSE)
  expect_equal(names(merged), names(psychsci)[c(1:2,2:3)])
})


test_that("test_paper", {
  expect_true(is.function(metacheck::test_paper))
  expect_no_error(helplist <- help(test_paper, metacheck))

  p <- test_paper("A")
  expect_equal(p$text$text, "A")

  p <- test_paper(LETTERS)
  expect_equal(p$text$text, LETTERS)
})
