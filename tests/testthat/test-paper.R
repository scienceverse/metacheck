test_that("exists", {
  expect_true(is.function(metacheck::paper))
  expect_true(is.function(metacheck::paperlist))
})

test_that("paper", {
  paper <- paper()
  expect_s3_class(paper, "scivrs_paper")
  expect_match(paper$paper_id, "^[a-f0-9]{14}$")
  expect_true(paper_validate(paper))
})

test_that("paper_coerce", {
  paper <- paper()
  paper$info <- data.frame(
    title = 1,
    oecd_confidence = "20",
    keywords = I(list(c("A", "B")))
  )
  paper$text$text_id <- double(0)
  cpaper <- paper_coerce(paper)

  expect_equal("character", typeof(cpaper$info$title))
  expect_equal("double", typeof(cpaper$info$oecd_confidence))
  expect_equal("integer", typeof(cpaper$text$text_id))
  expect_equal("list", typeof(cpaper$info$keywords))

  # NAs introduced by coercion
  paper <- paper()
  paper$info <- data.frame(
    oecd_confidence = "NO",
    keywords = "kw"
  )
  expect_warning(cpaper <- paper_coerce(paper))
  expect_equal(cpaper$info$oecd_confidence, NA_real_)
  expect_equal(cpaper$info$keywords, list("kw"))

  # check log
  ll <- lastlog(1)
  expect_equal(ll$label, "paper_coerce")
  expect_equal(ll$paper_id, paper$paper_id)
  expect_equal(ll$table, "info")
  expect_equal(ll$column, "oecd_confidence")
  expect_equal(ll$rows, "1")
  expect_equal(ll$example, "NO")
})

test_that("paper_validate", {
  expect_true(is.function(metacheck::paper_validate))
  expect_no_error(helplist <- help(paper_validate, metacheck))

  expect_error(paper_validate(bad_arg))

  paper <- paper()
  expect_true(paper_validate(paper))

  paper <- list(paper_id = "not a paper")
  expect_error(paper_validate(paper))
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
  merged <- paperlist(psychsci[1:2], psychsci[2:3], psychsci[1:3],
                      merge_duplicates = TRUE)
  expect_equal(names(merged), names(psychsci[1:3]))

  # don't merge duplicate papers
  merged <- paperlist(psychsci[1:2], psychsci[2:3])
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
  expect_true(paper_validate(paper))
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
  # expect_equal(info$file_hash, paper$paper_id)

  # select columns
  paper <- demopaper()
  cols <- c("given", "family")
  authors <- paper_table(paper, "author", cols)
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


test_that("paper_write", {
  expect_true(is.function(metacheck::paper_write))
  expect_no_error(helplist <- help(paper_write, metacheck))

  expect_error(paper_write(bad_arg))

  # save an exact copy
  paper <- demopaper()
  save_path <- withr::local_tempdir()
  json_path <- paper_write(paper, NULL, save_path)
  expect_true(file.exists(json_path))
  paper2 <- read(json_path)
  expect_setequal(names(paper), names(paper2))

  # save changes
  paper <- demopaper()
  paper$info$title <- "New paper"
  paper$bib <- paper$bib[1:2, ]
  save_path <- withr::local_tempdir()
  json_path <- paper_write(paper, NULL, save_path)
  expect_true(file.exists(json_path))
  paper2 <- read(json_path)
  expect_setequal(names(paper), names(paper2))
})
