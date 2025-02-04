test_that("exists", {
  expect_true(is.function(read_text))
})

test_that("sections", {
  txt <- "Abstract\nThis is a paragraph of text that is in the abstract. This is sentence two of the abstract.\n\nIntroduction\nThe first sentence of the introduction. The second sentence of the introduction."
  p <- read_text(txt)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(nrow(ft), 6)
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_equal(ft$section, rep(c("abstract", "intro"), each = 3))
})

test_that("from text file", {
  txt <- "Abstract\nThis is a paragraph of text that is in the abstract. This is sentence two of the abstract.\n\nIntroduction\nThe first sentence of the introduction. The second sentence of the introduction."
  filename <- tempfile(fileext = ".txt")
  write(txt, filename)

  p <- read_text(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(nrow(ft), 6)
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_equal(ft$section, rep(c("abstract", "intro"), each = 3))

  file.remove(filename)
})

test_that("from txt", {
  filename <- system.file("extdata/to_err_is_human.txt", package = "papercheck")
  p <- read_text(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_true(all(c("intro", "results", "discussion")
                  %in% ft$section))
})

test_that("from word docx", {
  filename <- system.file("extdata/to_err_is_human.docx", package = "papercheck")
  p <- read_text(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_true(all(c("intro", "results", "discussion")
                  %in% ft$section))
})

test_that("from word doc", {
  filename <- system.file("extdata/to_err_is_human.doc", package = "papercheck")
  p <- read_text(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_true(all(c("intro", "results", "discussion")
                  %in% ft$section))
})
