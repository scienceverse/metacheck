test_that("convert", {
  expect_true(is.function(metacheck::convert))
  expect_no_error(helplist <- help(convert, metacheck))

  expect_error(convert(bad_arg))

  # no files
  file_path <- file.path(withr::local_tempdir(), "emptydir")
  dir.create(file_path)
  expect_error(convert(file_path), "No PDF, XML, DOC or DOCX files detected")

  # no relevant files
  txt_path <- file.path(file_path, "hi.txt")
  write("hi", txt_path)
  expect_error(convert(file_path), "No PDF, XML, DOC or DOCX files detected")
})

test_that("XML-auto", {
  file_path <- demofile("xml")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("XML-grobid", {
  file_path <- demofile("xml")
  save_path <- withr::local_tempdir()
  method <- "grobid"

  obs <- convert(file_path, save_path, method)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))

  # no bib_match unless crossref_lookup = TRUE
  expect_disjoint("bib_match", names(paper))
})

test_that("XML-crossref", {
  skip_api("api.labs.crossref.org")

  file_path <- demofile("xml")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path, crossref_lookup = TRUE)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
  expect_in("bib_match", names(paper))
})

test_that("PDF-auto", {
  skip_api(grobid_url)
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path, api_url = grobid_url)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("PDF-grobid", {
  skip_api(grobid_url)
  skip_if_not(.grobid_isalive(grobid_url, error = FALSE),
              message = "grobid not available")

  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  method <- "grobid"

  obs <- convert(file_path, save_path, method, api_url = grobid_url)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("PDF-bibr", {
  skip_api(bibr_url)
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  file_path <- demofile("pdf")
  save_path <- withr::local_tempdir()
  method <- "bibr"

  obs <- convert(file_path, save_path, method)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("DOC-auto", {
  skip_api(bibr_url)
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  file_path <- demofile("doc")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})

test_that("DOCX-auto", {
  skip_api(bibr_url)
  skip_if_not(.bibr_isalive(bibr_url, error = FALSE),
              message = "bibr not available")

  file_path <- demofile("doc")
  save_path <- withr::local_tempdir()

  obs <- convert(file_path, save_path)
  expect_match(obs, "\\.json$")
  expect_true(file.exists(obs))

  paper <- read(obs)
  expect_true(paper_validate(paper))
})
