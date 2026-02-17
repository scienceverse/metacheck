test_that("bibr_convert", {
  expect_true(is.function(metacheck::bibr_convert))
  expect_no_error(helplist <- help(bibr_convert, metacheck))

  expect_error(bibr_convert(bad_arg))

  # pdf
  file_name <- "to_err_is_human.pdf"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_grepl("to_err_is_human\\.zip", zip_path)
  pdf <- read_bibr(zip_path)
  expect_equal(pdf$info$file_name, file_name)
  expect_grepl("^[a-f0-9]{14}$", pdf$id)

  # docx
  file_name <- "to_err_is_human.docx"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_grepl("to_err_is_human\\.zip", zip_path)
  docx <- read_bibr(zip_path)
  expect_equal(docx$info$file_name, file_name)
  expect_grepl("^[a-f0-9]{14}$", docx$id)

  # html
  file_name <- "to_err_is_human.html"
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_true(file.exists(zip_path) |> all())
  expect_grepl("to_err_is_human\\.zip", zip_path)
  html <- read_bibr(zip_path)
  expect_equal(html$info$file_name, file_name)
  expect_grepl("^[a-f0-9]{14}$", html$id)

  # multiple files
  file_name <- c("to_err_is_human.pdf", "published.pdf")
  file_path <- test_path("fixtures", "formats", file_name)
  save_path <- withr::local_tempdir()
  zip_path <- bibr_convert(file_path, save_path)
  expect_grepl("to_err_is_human\\.zip", zip_path[[1]])
  expect_grepl("published\\.zip", zip_path[[2]])
  expect_true(file.exists(zip_path) |> all())
})


test_that("read_bibr", {
  expect_true(is.function(metacheck::read_bibr))
  expect_no_error(helplist <- help(read_bibr, metacheck))

  expect_error(read_bibr(bad_arg))

  # single paper
  file_path <- test_path("fixtures", "bibr", "to_err_is_human.zip")
  paper <- read_bibr(file_path)

  expect_s3_class(paper, "scivrs_paper")
  obs <- names(paper)
  exp <- c("id", "info", "authors", "text", "links", "tables",
           "sections", "bib", "xrefs", "figures")
  expect_contains(obs, exp)
  expect_grepl("^[a-f0-9]{14}$", paper$id)

  # vector of paths
  zips <- c("to_err_is_human.zip", "published.zip")
  file_path <- test_path("fixtures", "bibr", zips)
  papers <- read_bibr(file_path)
  expect_s3_class(papers, "scivrs_paperlist")
  expect_s3_class(papers[[1]], "scivrs_paper")
  expect_s3_class(papers[[2]], "scivrs_paper")

  expect_equal(paper, papers[[1]])
  expect_equal(names(papers), c(papers[[1]]$id, papers[[2]]$id))

  # directory
  file_path <- test_path("fixtures", "bibr", "psychsci")
  ps <- read_bibr(file_path)
  expect_s3_class(ps, "scivrs_paperlist")
  expect_s3_class(ps[[1]], "scivrs_paper")
})

