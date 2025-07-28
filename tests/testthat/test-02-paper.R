test_that("exists", {
  expect_true(is.function(papercheck::paper))
  expect_true(is.function(papercheck::paperlist))
})

test_that("paper", {
  p <- paper()
  expect_s3_class(p, "scivrs_paper")
  exp_names <- c("id", "info", "authors", "full_text", "bib", "xrefs")
  expect_equal(names(p), exp_names)
  expect_equal(p$id, "Demo Paper")
  expect_equal(p$info, list())
  expect_equal(length(p$authors), 0)
  expect_s3_class(p$authors, "scivrs_authors")
  expect_equal(p$full_text, data.frame())
  expect_equal(p$bib, data.frame())
  expect_equal(p$xrefs, data.frame())

  xml <- demoxml()
  p <- paper(xml)
  expect_equal(nrow(p$full_text), 24)
  expect_equal(nrow(p$bib), 2)
  expect_equal(nrow(p$xrefs), 2)
})

test_that("paperlist", {
  # individual papers
  p1 <- psychsci[[1]]
  p2 <- psychsci[[2]]
  pl <- paperlist(p1, p2)

  expect_s3_class(pl, "scivrs_paperlist")
  expect_equal(names(pl), c(p1$id, p2$id))

  # single list of papers
  pl <- paperlist(psychsci[1:2])

  expect_s3_class(pl, "scivrs_paperlist")
  expect_equal(names(pl), c(p1$id, p2$id))

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
