test_that("from text file", {
  txt <- "Abstract\n\nThis is a paragraph of text that is in the abstract. This is sentence two of the abstract.\n\nIntroduction\n\nThe first sentence of the introduction. The second sentence of the introduction."
  filename <- tempfile(fileext = ".txt")
  write(txt, filename)

  xml <- read_xml(filename)
  expect_equal(xml_find1(xml, "p"), "Abstract")
  expect_equal(xml_find(xml, "p")[[3]], "Introduction")

  p <- read(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(nrow(ft), 6)
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_equal(ft$section, rep(c("abstract", "intro"), each = 3))

  file.remove(filename)
})

test_that("from txt", {
  filename <- system.file("extdata/to_err_is_human.txt", package = "metacheck")
  p <- read(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_true(all(c("intro", "results", "discussion")
                  %in% ft$section))
})

test_that("from word docx", {
  filename <- system.file("extdata/to_err_is_human.docx", package = "metacheck")
  p <- read(filename)
  ft <- p$full_text

  expect_equal(class(p), c("scivrs_paper", "list"))
  expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
  expect_true(all(c("intro", "results", "discussion", "references")
                  %in% ft$section))
})

# test_that("from word doc", {
#   filename <- system.file("extdata/to_err_is_human.doc", package = "metacheck")
#   p <- read(filename)
#   ft <- p$full_text
#
#   expect_equal(class(p), c("scivrs_paper", "list"))
#   expect_equal(colnames(ft), c("text", "section", "header", "div", "p", "s", "id"))
#   expect_true(all(c("intro", "results", "discussion")
#                   %in% ft$section))
# })
