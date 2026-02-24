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

test_that("demo paper", {
  paper <- demopaper()
  expect_s3_class(paper, "scivrs_paper")
  expect_match(paper$info$title, "^To Err is Human")
})

test_that("paper_table", {
  # concat 2 papers
  paper <- psychsci[1:2]
  bibs <- paper_table(paper, "bib")
  n <- nrow(paper[[1]]$bib) + nrow(paper[[2]]$bib)
  expect_equal(nrow(bibs), n)

  ids <- unique(bibs$paper_id)
  expect_equal(ids, names(paper))

  # concat 1 paper
  paper <- demopaper()
  info <- paper_table(paper, "info")
  expect_equal(info$file_hash, paper$paper_id)

  # select columns
  paper <- demopaper()
  cols <- c("given", "family")
  authors <- paper_table(paper, "authors", cols)
  expect_equal(names(authors), c(cols, "paper_id"))

  # concat 1 paper empty table
  paper <- demopaper()
  paper$bib <- data.frame(text = character(0))
  bib <- paper_table(paper, c("bib"))
  expect_equal(nrow(bib), 0)
  expect_equal(names(bib), c("text", "paper_id"))
})


test_that("is_paper_list", {
  expect_equal(is_paper_list(psychsci), TRUE)
  expect_equal(is_paper_list(psychsci[1]), TRUE)
  expect_equal(is_paper_list(psychsci[[1]]), FALSE)
  expect_equal(is_paper_list(list(1,3,5)), FALSE)
  expect_equal(is_paper_list(NULL), FALSE)

  # empty lists return TRUE
  expect_equal(is_paper_list(psychsci[c()]), TRUE)
  expect_equal(is_paper_list(list()), TRUE)
})

test_that("print.scivrs_paper", {
  paper <- demopaper()
  op <- capture_output(print(paper))
  op.sv <- capture_output(print.scivrs_paper(paper))

  expect_equal(op, op.sv)
  expect_match(op, paper$paper_id)
  expect_match(op, paper$info$title, fixed = TRUE)
})

test_that("print.scivrs_paperlist", {
  x <- psychsci[1:3]
  op <- capture_output(print(x))
  op.sv <- capture_output(print.scivrs_paperlist(x))

  expect_true(grepl("# A tibble: 3", op, fixed = TRUE))
  expect_equal(op, op.sv)

  # test papers
  x <- paperlist(
    test_paper(LETTERS),
    test_paper(letters)
  )

  op <- capture_output(print(x))
  expect_match(op, x[[1]]$paper_id)
  expect_match(op, x[[2]]$paper_id)
})

test_that("[.scivrs_paperlist", {
  # subsetting maintains class
  x <- psychsci[1:3]
  expect_s3_class(psychsci, "scivrs_paperlist")
  expect_s3_class(x, "scivrs_paperlist")
})
